package me.micseydel.actor.notifications

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.{PathModifiedEvent, PathUpdatedEvent}
import me.micseydel.actor._
import me.micseydel.actor.notifications.ChimeActor.Message
import me.micseydel.actor.notifications.ChimeJsonFormat.ChimeMessageJsonFormat
import me.micseydel.actor.notifications.NotificationCenterManager._
import me.micseydel.actor.perimeter.{HueControl, NtfyerActor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault._
import me.micseydel.vault.persistence.{NoteRef, TypedJsonRef}
import org.slf4j.Logger
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.annotation.unused
import scala.util.{Failure, Success, Try}

object NotificationCenterManager {

  private val NoteName = "Notification Center"
  private val JsonName = "notification_center"
  private val WatchedFilename = s"$NoteName.md"

  // mailbox

  sealed trait Message

  //  private
  case class PathUpdated(pathUpdatedEvent: PathUpdatedEvent) extends Message

  case class StartTinkering(tinker: Tinker) extends Message

  sealed trait NotificationMessage extends Message

  case class NewNotification(notification: Notification) extends NotificationMessage

  case class CompleteNotification(notificationId: String) extends NotificationMessage

  case class UpcomingNotification(notification: Notification) extends NotificationMessage

  case class JustSideEffect(sideEffect: SideEffect) extends NotificationMessage

  // FIXME: move this to the Operator?
  case class RegisterReplyTo(replyTo: SpiritRef[NotificationId], id: SpiritId) extends NotificationMessage

  sealed trait SideEffect

  case class PushNotification(key: String, message: String) extends SideEffect

  case class HueCommand(command: HueControl.Command) extends SideEffect

  case class Chime(message: ChimeActor.Message) extends SideEffect

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

  def apply(vaultRoot: VaultPath,
            ntfyAbility: Tinker => Ability[NtfyerActor.Message],
            chimeHost: Option[String]): Behavior[Message] = Behaviors.setup { context =>
    val subdirectory = s"${ActorNotesFolderWatcherActor.ActorNotesSubdirectory}/notification_center"

    @unused // receives messages from a thread it creates, we don't send it messages but the adapter lets it reply to us
    val folderWatcher = context.spawn(
      FolderWatcherActor(
        vaultRoot.resolve(subdirectory),
        context.messageAdapter(PathUpdated)
      ),
      "NotificationCenterFolderWatcherActor"
    )

    initializing(ntfyAbility, chimeHost)
  }

  private def initializing(ntfyAbility: Tinker => Ability[NtfyerActor.Message], chimeHost: Option[String]): Behavior[Message] = Behaviors.withStash(10) { stash =>
    Behaviors.receiveMessage {
      case message@(PathUpdated(_) | _: NotificationMessage) =>
        stash.stash(message)
        Behaviors.same

      case StartTinkering(tinker) =>
        stash.unstashAll(finishInitializing(ntfyAbility(tinker), chimeHost)(tinker))
    }
  }

  private def finishInitializing(ntfyAbility: Ability[NtfyerActor.Message], maybeChimeHost: Option[String])(implicit Tinker: Tinker): Ability[Message] =
    Tinker.initializedWithNote(NoteName, "_actor_notes/notification_center") { case (_, noteRef) =>
      Tinker.initializedWithTypedJson(JsonName, NotificationCenterManagerJsonFormat.notificationCenterStateJsonFormat) { case (context, jsonRef) =>
        val upcomingNotificationsManager: SpiritRef[UpcomingNotificationsManager.Message] = context.cast(UpcomingNotificationsManager(context.self), "UpcomingNotificationsManager")
        val ntfyer = context.cast(ntfyAbility, "Ntfyer")

        val chime: SpiritRef[ChimeActor.Message] = maybeChimeHost match {
          case Some(chimeHost) =>
            context.cast(ChimeActor(chimeHost), "Chime")

          case None =>
            context.cast(Tinker.ignore, "InertChime")
        }

        ability(Map.empty)(Tinker, upcomingNotificationsManager, noteRef, jsonRef, ntfyer, chime)
      }
    }

  private def ability(replyTos: Map[SpiritId, SpiritRef[NotificationId]])(implicit Tinker: Tinker, upcomingNotificationsManager: SpiritRef[UpcomingNotificationsManager.Message], noteRef: NoteRef, jsonRef: TypedJsonRef[NotificationCenterState], ntfyer: SpiritRef[NtfyerActor.Message], chime: SpiritRef[ChimeActor.Message]): Ability[Message] =
    Tinkerer(rgb(205, 205, 0), "❗️").receive { (context, message) =>
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
          Behaviors.same

        case UpcomingNotification(notification) =>
          jsonRef.updateOrSetDefault(NotificationCenterState(Map.empty))(_.withIncluded(notification))
          upcomingNotificationsManager !! UpcomingNotificationsManager.UpcomingNotification(notification)
          Behaviors.same

        case NewNotification(notification) =>
          jsonRef.updateOrSetDefault(NotificationCenterState(Map.empty))(_.withIncluded(notification))
          context.actorContext.log.info(s"Adding notification ${notification.notificationId}")
          logErrorIfNeeded(
            context.actorContext.log,
            NotificationCenterManagerMarkdown.addNotification(noteRef, notification),
            _ => s"Something went wrong adding notification for time ${notification.time} to notification center"
          )

          notification.sideEffects.foreach {
            case PushNotification(key, message) =>
              ntfyer !! NtfyerActor.DoNotify(key, message)
            case HueCommand(command) =>
              context.system.hueControl !! command
            case Chime(message) =>
              chime !! message
          }

          Behaviors.same

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

          NotificationCenterManagerMarkdown.clearNotification(noteRef, id)
          upcomingNotificationsManager !! UpcomingNotificationsManager.MarkNotificationCompleted(id)
          Behaviors.same

        case PathUpdated(PathModifiedEvent(modifiedPath)) if modifiedPath.toString.endsWith(WatchedFilename) =>
          context.actorContext.log.info(s"Clearing completed notifications for $modifiedPath...")

          NotificationCenterManagerMarkdown.clearDone(noteRef) match {
            case Success(cleared) =>
              context.actorContext.log.info(s"Cleared: $cleared")
              if (cleared.nonEmpty) {
                jsonRef.read() match {
                  case Failure(exception) => context.actorContext.log.error(s"Reading state from JSON failed", exception)
                  case Success(state) =>
                    context.actorContext.log.info(s"")
                    for (done <- cleared) {
                      state.get(done.id).flatMap(_.requestNotificationOnCompletion) match {
                        case None =>
                          context.actorContext.log.info(s"${done.id} (from $done) was not in ${state.map}")
                        case Some(spiritId) =>
                          replyTos.get(spiritId) match {
                            case None =>
                              context.actorContext.log.warn(s"Unable to update registered listener for missing spirit $spiritId")
                            case Some(replyTo) =>
                              context.actorContext.log.info(s"Sending $done to ${replyTo.path} to mark as done")
                              replyTo !! done
                          }
                      }
                    }
                }
              }

            case Failure(exception) =>
              context.actorContext.log.error(s"Failed to clear done ${noteRef.noteId.asString} elements", exception)
          }

          Behaviors.same

        case PathUpdated(PathModifiedEvent(modifiedPath)) if modifiedPath.toString.endsWith("Upcoming Notifications.md") =>
          context.actorContext.log.debug(s"Ignoring [[Upcoming Notifications]] update")
          Behaviors.same

        case PathUpdated(pathUpdatedEvent) =>
          context.actorContext.log.warn(s"Expected $WatchedFilename but found $pathUpdatedEvent")
          Behaviors.same

        case StartTinkering(_) =>
          context.actorContext.log.warn(s"Already received StartTinkering, ignoring")
          Behaviors.same
      }
    }

  private def logErrorIfNeeded(log: Logger, f: => Try[NoOp.type], message: Throwable => String): Unit = {
    f match {
      case Failure(exception) =>
        log.error(message(exception), exception)
      case Success(NoOp) =>
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

  import me.micseydel.actor.perimeter.HueControlJsonFormat.HueControlCommandJsonFormat
  import me.micseydel.Common.{OptionalJsonFormat, ZonedDateTimeJsonFormat}
  import LinkIdJsonProtocol.noteIdFormat

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
