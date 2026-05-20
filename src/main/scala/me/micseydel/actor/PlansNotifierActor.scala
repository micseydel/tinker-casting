package me.micseydel.actor

import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.*
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object PlansNotifierActor {
  sealed trait Message

  private case class ReceiveNotePing(ping: Ping) extends Message

  private case class ReceiveMidnight(midnight: ZonedDateTime) extends Message

  val NoteName = "PlansNotifierActor"
  val Emoji = "🛫"

  def apply()(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](NoteName, TinkerColor.random(), Emoji, ReceiveNotePing) {
      (context, noteRef) =>
        implicit val tc: TinkerContext[?] = context
        implicit val nr: NoteRef = noteRef

        context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ReceiveMidnight))

        behavior()
    }

  def behavior()(implicit Tinker: Tinker, context: TinkerContext[?], noteRef: NoteRef): Ability[Message] =
    Tinker.receiveMessage {
      case ReceiveNotePing(_) =>
        // FIXME: what fun stuff can I add? calendar integration tinkering?
        Tinker.steadily

      case ReceiveMidnight(givenMidnight) =>
        // FIXME copy-paste from UNM, cleanup @ 3
        val midnight = nearestMidnightToNow(context.system.clock)
        if (midnight != givenMidnight) {
          // FIXME: if this hasn't been an issue within 3 days of comitting, it can be deleted (just use the given, maybe even modify the upcomingthing)
          context.actorContext.log.warn(s"UpcomingNotificationsManager.nearestMidnightToNow(context.system.clock) -> $midnight BUUUT givenMidnight=$givenMidnight")
        }

        val formatter = DateTimeFormatter.ofPattern("yyyyMMdd")
        val id = formatter.format(midnight)
        NotificationCenterManager.NewNotification(Notification(midnight, s"- ![[Plans#^$id]]", None, NotificationId(id), Nil))

        Tinker.steadily
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
}
