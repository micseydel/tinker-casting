package me.micseydel.actor.notifications

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor._
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.Purple
import me.micseydel.dsl._
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.vault.VaultKeeper.NoteRefResponse
import me.micseydel.vault._
import me.micseydel.vault.persistence.NoteRef
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success}

object UpcomingNotificationsManager {

  private val NoteName = "Upcoming Notifications"

  // _actor_notes/notification_center/Upcoming Notifications.md
  private val Subdirectory = s"${ActorNotesFolderWatcherActor.ActorNotesSubdirectory}/notification_center"

  // mailbox

  sealed trait Message

  private case object ItsMidnight extends Message

  private case class ReceiveNoteRef(noteRefResponse: NoteRefResponse) extends Message

  case class UpcomingNotification(notification: Notification) extends Message
  case class MarkNotificationCompleted(noteId: String) extends Message

  private case class TimeForNotification(notification: Notification) extends Message

  //

  def apply(notificationCenterManager: SpiritRef[NotificationCenterManager.Message])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(Purple, "⏳").setup { context =>
    implicit val c: TinkerContext[_] = context

    val timeKeeper = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeDailyAt(0, 0, context.self, ItsMidnight, None)

    import NotificationCenterManagerJsonFormat.notificationJsonFormat

    val rememberingTimeKeeper: SpiritRef[RememberingTimeKeeper.PostInitMessage[TimeForNotification]] = context.cast(RememberingTimeKeeper(
      context.self.narrow[TimeForNotification],
      TimeForNotificationJsonFormat.apply,
      "upcoming_notifications_queued"
    ), "RememberingTimeKeeper")

    // FIXME: use Tinker.initializedWithNote() instead
    context.system.vaultKeeper !!
      VaultKeeper.RequestExclusiveNoteRef(NoteName, context.messageAdapter(ReceiveNoteRef).underlying, Some(Subdirectory))

    initializing(notificationCenterManager, rememberingTimeKeeper)
  }

  private def initializing(notificationCenterManager: SpiritRef[NotificationCenterManager.Message], rememberingTimeKeeper: SpiritRef[RememberingTimeKeeper.PostInitMessage[TimeForNotification]])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { tinkerContext =>
    tinkerContext.actorContext.log.info("Initializing UpcomingNotificationsManager")
    Behaviors.withStash(10) { stash =>
      Behaviors.receive { (context, message) =>
        message match {
          case ReceiveNoteRef(NoteRefResponse(receivedNoteName, noteRefOrWhyNot)) =>
            if (receivedNoteName == NoteName) {
              noteRefOrWhyNot match {
                case Right(noteRef) =>
                  context.log.info(s"Switching to initialized state now...")
                  stash.unstashAll(behavior(notificationCenterManager, rememberingTimeKeeper, noteRef).behavior)

                case Left(msg) =>
                  context.log.error(s"Notification Center unable to get NoteRef for [[$NoteName]]: $msg")
                  Behaviors.stopped
              }
            } else {
              context.log.warn(s"Expected NoteRef for [[$NoteName]] but got $receivedNoteName")
              Behaviors.same
            }

          case other =>
            stash.stash(other)
            Behaviors.same
        }
      }
    }
  }

  private def behavior(
                        notificationCenterManager: SpiritRef[NotificationCenterManager.Message],
                        rememberingTimeKeeper: SpiritRef[RememberingTimeKeeper.PostInitMessage[TimeForNotification]],
                        noteRef: NoteRef
                      )(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case UpcomingNotification(notification) =>
        context.actorContext.log.info(s"Notification ${notification.notificationId} adding to upcoming Markdown, creating a REMEMBERED time keeper reminder")

        UpcomingNotificationMarkdown.addUpcomingNotification(noteRef, notification)(context.system.clock, context.actorContext.log) match {
          case Failure(exception) =>
            context.actorContext.log.error(s"Failure while trying to add upcoming notification ${notification.notificationId}, ${notification.time}", exception)
          case Success(_) =>
        }

        implicit val clock: TinkerClock = context.system.clock
        rememberingTimeKeeper !! RememberingTimeKeeper.RemindMeAt(
          notification.time,
          TimeForNotification(notification),
          notification.notificationId.toString
        )

        Tinker.steadily

      case MarkNotificationCompleted(noteId) =>
        context.actorContext.log.info(s"Notification id $noteId is complete, removing")
        UpcomingNotificationMarkdown.removeUpcomingNotification(noteRef, noteId)(context.actorContext.log)
        Tinker.steadily

      case TimeForNotification(notification) =>
        context.actorContext.log.info(s"Timer up for notification ${notification.notificationId}")
        notificationCenterManager !! NotificationCenterManager.NewNotification(notification)
        val notificationId = notification.notificationId.toString
        UpcomingNotificationMarkdown.removeUpcomingNotification(noteRef, notificationId)(context.actorContext.log)
        Tinker.steadily

      case ItsMidnight =>
        context.actorContext.log.info(s"It's midnight, adding today's plans to the notification center ")
        val midnight = nearestMidnightToNow(context.system.clock)
        val formatter = DateTimeFormatter.ofPattern("yyyyMMdd")
        val id = formatter.format(midnight)
        val notification = Notification(midnight, s"- ![[Plans#^$id]]", None, NotificationId(id), Nil)

        notificationCenterManager !! NotificationCenterManager.NewNotification(notification)

        Tinker.steadily


      // unexpected

      case ReceiveNoteRef(noteRefResponse) =>
        context.actorContext.log.warn(s"Already received NoteRef, unexpectedly got $noteRefResponse")
        Tinker.steadily
    }
  }

  private def nearestMidnightToNow(tinkerClock: TinkerClock): ZonedDateTime = {
    nearestMidnightTo(tinkerClock.now())
  }

  private def nearestMidnightTo(time: ZonedDateTime): ZonedDateTime = {
    MillisFromMidnight(time) match {
      case MillisUntil(_) =>
        MillisFromMidnight.midnightFor(time.plusDays(1))

      case MillisSince(_) =>
        MillisFromMidnight.midnightFor(time)
    }
  }


  private object TimeForNotificationJsonFormat extends DefaultJsonProtocol {
    def apply(implicit notificationJsonFormat: RootJsonFormat[Notification]): RootJsonFormat[TimeForNotification] = {
      jsonFormat1(TimeForNotification)
    }
  }
}
