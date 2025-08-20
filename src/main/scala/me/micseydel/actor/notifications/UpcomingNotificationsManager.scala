package me.micseydel.actor.notifications

import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor._
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.Purple
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import scala.util.{Failure, Success}

object UpcomingNotificationsManager {

  private val NoteName = "Upcoming Notifications"

  // mailbox

  sealed trait Message

  private case class ItsMidnight(forDay: LocalDate) extends Message

  private case class ReceiveNotePing(ping: Ping) extends Message

  case class UpcomingNotification(notification: Notification) extends Message

  case class MarkNotificationCompleted(noteId: String) extends Message

  private case class TimeForNotification(notification: Notification) extends Message

  //

  def apply(notificationCenterManager: SpiritRef[NotificationCenterManager.Message])(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](NoteName, Purple, "⏳", ReceiveNotePing) { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context

      context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))

      import NotificationCenterManagerJsonFormat.notificationJsonFormat

      val rememberingTimeKeeper: SpiritRef[RememberingTimeKeeper.Message[TimeForNotification]] = context.cast(RememberingTimeKeeper(
        context.self.narrow[TimeForNotification],
        TimeForNotificationJsonFormat.apply,
        "upcoming_notifications_queued"
      ), "RememberingTimeKeeper")

      behavior(notificationCenterManager, rememberingTimeKeeper, noteRef)
    }

  private def behavior(
                        notificationCenterManager: SpiritRef[NotificationCenterManager.Message],
                        rememberingTimeKeeper: SpiritRef[RememberingTimeKeeper.Message[TimeForNotification]],
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
        UpcomingNotificationMarkdown.removeUpcomingNotification(noteRef, noteId)
        Tinker.steadily

      case TimeForNotification(notification) =>
        context.actorContext.log.info(s"Timer up for notification ${notification.notificationId}")
        notificationCenterManager !! NotificationCenterManager.NewNotification(notification)
        val notificationId = notification.notificationId.id
        UpcomingNotificationMarkdown.removeUpcomingNotification(noteRef, notificationId) match {
          case Failure(exception) => throw exception
          case Success(_) =>
        }
        Tinker.steadily

      case ItsMidnight(_) =>
        context.actorContext.log.info(s"It's midnight, adding today's plans to the notification center ")
        val midnight = nearestMidnightToNow(context.system.clock)
        val formatter = DateTimeFormatter.ofPattern("yyyyMMdd")
        val id = formatter.format(midnight)

        // FIXME: this actor should be in a lib, this behavior needs to be moved to userspace
        val notifications = List(
          Notification(midnight, s"- ![[Plans#^$id]]", None, NotificationId(id), Nil),
          Notification(midnight, s"- [[Transcribed mobile notes (${midnight.minusDays(1).toLocalDate})#Notes without acknowledgements]]", None, NotificationId(s"withoutack-$id"), Nil)
        )

        for (notification <- notifications) {
          notificationCenterManager !! NotificationCenterManager.NewNotification(notification)
        }


        Tinker.steadily

      case ReceiveNotePing(_) =>
        context.actorContext.log.debug("Note update detected but not doing anything with it yet")
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
