package me.micseydel.actor.notifications

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.notifications.NotificationCenterManager._
import me.micseydel.actor.perimeter.{HueControl, NtfyerActor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault._
import me.micseydel.vault.persistence.TypedJsonRef
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

object NotificationCenterManager {

  private val JsonName = "notification_center"

  // mailbox

  sealed trait Message

  case class StartTinkering(tinker: Tinker) extends Message

  sealed trait NotificationMessage extends Message

  case class NewNotification(notification: Notification) extends NotificationMessage

  case class CompleteNotification(notificationId: String) extends NotificationMessage

  case class JustSideEffect(sideEffect: SideEffect) extends NotificationMessage

  // FIXME: move this to the Operator?
  case class RegisterReplyTo(replyTo: SpiritRef[NotificationId], id: SpiritId) extends NotificationMessage

  // model

  sealed trait SideEffect

  case class PushNotification(key: String, message: String) extends SideEffect

  case class HueCommand(command: HueControl.Command) extends SideEffect

  case class Chime(message: ChimeActor.Command) extends SideEffect

  //

  case class Notification(
                           time: ZonedDateTime,
                           string: String,
                           maybeRef: Option[NoteId],
                           notificationId: NotificationId,
                           sideEffects: List[SideEffect],
                           requestNotificationOnCompletion: Option[SpiritId] = None
                         )

  object Notification {
    def toMarkdownListLine(notification: Notification, checkboxes: Boolean = true): String = {
      notification match {
        case Notification(time, string, maybeRef, notificationId, _, _) =>
          val beforeTimestamp = if (checkboxes) Some("[ ] ") else None
          maybeRef match {
            case None =>
              MarkdownUtil.listLineWithTimestamp(time, string, beforeTimestamp, blockId = Some(notificationId.id))
            case Some(ref) =>
              MarkdownUtil.listLineWithTimestampAndRef(time, string, ref, beforeTimestamp, blockId = Some(notificationId.id))
          }
      }
    }
  }

  def apply(ntfyAbility: Tinker => Ability[NtfyerActor.Message]): Behavior[Message] =
    Behaviors.withStash(10) { stash =>
      Behaviors.receiveMessage {
        case message@(_: NotificationMessage) =>
          stash.stash(message)
          Behaviors.same

        case StartTinkering(tinker) =>
          stash.unstashAll(finishInitializing(ntfyAbility(tinker))(tinker))
      }
    }

  private def finishInitializing(ntfyAbility: Ability[NtfyerActor.Message])(implicit Tinker: Tinker): Ability[Message] =
    Tinkerer(rgb(205, 205, 0), "❗️").initializedWithTypedJson(JsonName, NotificationCenterManagerJsonFormat.notificationCenterStateJsonFormat) {
      case (context, jsonRef) =>
        val notificationCenterActor: SpiritRef[NotificationCenterActor.Message] = context.cast(NotificationCenterActor(context.self), "NotificationCenterActor")
        val upcomingNotificationsManager: SpiritRef[UpcomingNotificationsManager.Message] = context.cast(UpcomingNotificationsManager(context.self), "UpcomingNotificationsManager")

        val ntfyer = context.cast(ntfyAbility, "Ntfyer")

        val chime: SpiritRef[ChimeActor.Command] = context.cast(ChimeActor(), "Chime")

        ability(Map.empty)(Tinker, upcomingNotificationsManager, notificationCenterActor, jsonRef, ntfyer, chime)
    }

  private def ability(replyTos: Map[SpiritId, SpiritRef[NotificationId]])(implicit Tinker: Tinker, upcomingNotificationsManager: SpiritRef[UpcomingNotificationsManager.Message], notificationCenterActor: SpiritRef[NotificationCenterActor.Message], jsonRef: TypedJsonRef[NotificationCenterState], ntfyer: SpiritRef[NtfyerActor.Message], chime: SpiritRef[ChimeActor.Command]): Ability[Message] =
    Tinker.receive { (context, message) =>
      implicit val c: TinkerContext[_] = context
      message match {
        case RegisterReplyTo(replyTo, id) =>
          val updated = replyTos.updated(id, replyTo)
          context.actorContext.log.info(s"Registering id $id to ${replyTo.path}; map size currently ${updated.size}")
          ability(updated)

        case JustSideEffect(sideEffect) =>
          sideEffect match {
            case PushNotification(key, message) =>
              ntfyer !! NtfyerActor.DoNotify(key, message)
            case HueCommand(command) =>
              context.system.hueControl !! command
            case Chime(message) =>
              chime !! message
          }
          Tinker.steadily

        case NewNotification(notification) if notification.time.minusSeconds(1).isAfter(context.system.clock.now()) =>
          // (we subtract 1 as a fudge factor to prevent race conditions around different clocks varying slightly)
          jsonRef.updateOrSetDefault(NotificationCenterState(Map.empty))(_.withIncluded(notification)) match {
            case Failure(exception) => throw exception
            case Success(_) =>
              context.actorContext.log.info(s"Notification with id ${notification.notificationId} being forwarded to upcoming notifications center")
              notificationCenterActor !! NotificationCenterActor.ClearNotification(notification.notificationId.id)
              upcomingNotificationsManager !! UpcomingNotificationsManager.UpcomingNotification(notification)
              Tinker.steadily
          }

        case NewNotification(notification) =>
          jsonRef.updateOrSetDefault(NotificationCenterState(Map.empty))(_.withIncluded(notification)) match {
            case Failure(exception) => throw exception
            case Success(_) =>
              context.actorContext.log.info(s"Adding notification ${notification.notificationId}")
              notificationCenterActor !! NotificationCenterActor.AddNotification(notification)

              notification.sideEffects.foreach {
                case PushNotification(key, message) =>
                  ntfyer !! NtfyerActor.DoNotify(key, message)
                case HueCommand(command) =>
                  context.system.hueControl !! command
                case Chime(message) =>
                  chime !! message
              }

              Tinker.steadily
          }

        case CompleteNotification(id) =>
          // in order to notify the original sender of the completion state, we had to serialize the ref
          jsonRef.updateOrSetDefault(NotificationCenterState(Map.empty)) { state =>
            state.get(id) match {
              case Some(Notification(_, _, _, _, _, Some(spiritId))) =>
                replyTos.get(spiritId) match {
                  case None =>
                    context.actorContext.log.warn(s"Cannot mark $id as done with $spiritId because no replyTo registered")
                  case Some(replyTo) =>
                    context.actorContext.log.info(s"Telling ${replyTo.path} id $id is completed")
                    replyTo !! NotificationId(id)
                }

              case None =>
                context.actorContext.log.warn(s"id $id was already completed or not recorded")
              case Some(Notification(_, _, _, _, _, None)) =>
                context.actorContext.log.debug(s"No replyTo request for id $id")
            }

            state.removed(id)
          }

          notificationCenterActor !! NotificationCenterActor.ClearNotification(id)
          upcomingNotificationsManager !! UpcomingNotificationsManager.MarkNotificationCompleted(id)
          Tinker.steadily

        case StartTinkering(_) =>
          context.actorContext.log.warn(s"Already received StartTinkering, ignoring")
          Tinker.steadily
      }
    }

  //

  final case class NotificationId(id: String) extends AnyVal
}

case class NotificationCenterState(map: Map[String, Notification]) {
  def withIncluded(notification: Notification): NotificationCenterState = {
    NotificationCenterState(map.updated(notification.notificationId.id, notification))
  }

  def get(k: String): Option[Notification] = map.get(k)

  def removed(k: String): NotificationCenterState = NotificationCenterState(map.removed(k))
}

object NotificationCenterManagerJsonFormat extends DefaultJsonProtocol {

  import LinkIdJsonProtocol.noteIdFormat
  import me.micseydel.Common.{OptionalJsonFormat, ZonedDateTimeJsonFormat}
  import me.micseydel.actor.perimeter.HueControlJsonFormat.HueControlCommandJsonFormat

  implicit val pushNotificationJsonFormat: RootJsonFormat[PushNotification] = jsonFormat2(PushNotification)
  implicit val hueCommandJsonFormat: RootJsonFormat[HueCommand] = jsonFormat1(HueCommand)
  implicit val maybeNoteIdFormat: JsonFormat[Option[NoteId]] = OptionalJsonFormat(noteIdFormat)
  implicit val notificationIdJsonFormat: JsonFormat[NotificationId] = jsonFormat1(NotificationId)

  import ChimeJsonFormat.ChimeMessageJsonFormat

  implicit val chimeJsonFormat: JsonFormat[Chime] = jsonFormat1(Chime)

  implicit object SideEffectJsonFormat extends RootJsonFormat[SideEffect] {
    def write(m: SideEffect): JsValue = {
      val (jsObj, typ) = m match {
        case l: PushNotification => (l.toJson.asJsObject, "PushNotification")
        case l: HueCommand => (l.toJson.asJsObject, "HueCommand")
        case l: Chime => (l.toJson.asJsObject, "Chime")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): SideEffect = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("PushNotification")) => value.convertTo[PushNotification]
        case Seq(JsString("HueCommand")) => value.convertTo[HueCommand]
        case Seq(JsString("Chime")) => value.convertTo[Chime]
        case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(_)) for one of {PushNotification, HueCommand} but got $other")
      }
    }
  }

  import me.micseydel.vault.SpiritId.SpiritIdJsonFormatter.spiritIDJsonFormat

  implicit val notificationJsonFormat: RootJsonFormat[Notification] = jsonFormat6(Notification.apply)

  val notificationCenterStateJsonFormat: RootJsonFormat[NotificationCenterState] = jsonFormat1(NotificationCenterState)
}
