package me.micseydel.actor.kitties

import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{HueCommand, Notification, NotificationId}
import me.micseydel.actor.perimeter.HueControl
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.{Tinker, TinkerContext, Tinkerer}
import me.micseydel.model.{Butter, CatOfMine, LitterBoxChoice, Peanut}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId

import java.time.ZonedDateTime
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object CatNotificationsManager {
  sealed trait Message

  final case class LitterUsed(when: ZonedDateTime, litterBoxChoice: LitterBoxChoice, maybeCat: Option[CatOfMine], ref: NoteId) extends Message
  final case class LitterClean(litterBoxChoice: LitterBoxChoice, ref: NoteId) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(CatBrown, "🐱").setup { context =>
    context.actorContext.log.info("Starting CatNotificationsManager")
    behavior()
  }

  private def behavior()(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context

    message match {
      case LitterUsed(when, choice, maybeCat, noteId) =>
        val now = context.system.clock.now()
        val timeSinceEvent = TimeUtil.between(when, now)

        val timeToWait = maybeCat match {
          case Some(Peanut) | None => 20.minutes
          case Some(Butter) => 15.minutes
        }

        val delay: FiniteDuration = timeToWait - timeSinceEvent

        context.actorContext.log.info(s"Using delay ${delay.toMinutes} minutes for cat $maybeCat ($timeToWait - $timeSinceEvent)")

        val notification = LitterNotification(when.plusMinutes(delay.toMinutes), s"$choice needs sifting", noteId, NotificationId(choice.toString))
        context.system.notifier !! NotificationCenterManager.NewNotification(notification)

        Tinker.steadily

      case LitterClean(litterBoxChoice, ref) =>
        context.actorContext.log.info(s"Marking $litterBoxChoice as done and canceling the timer ($ref)")
        context.system.notifier !! NotificationCenterManager.CompleteNotification(litterBoxChoice.toString)
        Tinker.steadily
    }
  }

  def LitterNotification(time: ZonedDateTime, string: String, noteId: NoteId, notificationId: NotificationId): Notification = {
    Notification(time, string, Some(noteId), notificationId, List(HueCommand(HueControl.DoALightShow())))
  }
}
