package me.micseydel.testsupport

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{Chime, ExpireCooldown, Notification}
import me.micseydel.actor.perimeter.NtfyerActor

object NotifierForTesting {
  def apply(maybeNtfyer: Option[ActorRef[NtfyerActor.Message]]): Behavior[NotificationCenterManager.Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
//      case NotificationCenterManager.PathUpdated(pathUpdatedEvent) => ??? // shouldn't happen
      case NotificationCenterManager.StartTinkering(tinker) => Behaviors.same
      case message: NotificationCenterManager.NotificationMessage =>

        val sideEffects: List[NotificationCenterManager.SideEffect] = message match {
          case NotificationCenterManager.NewNotification(Notification(time, string, maybeRef, notificationId, sideEffects, requestNotificationOnCompletion)) => sideEffects
          case NotificationCenterManager.CompleteNotification(notificationId) => Nil
//          case NotificationCenterManager.UpcomingNotification(Notification(time, string, maybeRef, notificationId, sideEffects, requestNotificationOnCompletion)) => sideEffects
          case NotificationCenterManager.JustSideEffect(sideEffect, _) => List(sideEffect)
          case NotificationCenterManager.RegisterReplyTo(replyTo, id) => Nil
        }

        sideEffects.foreach {
          case NotificationCenterManager.PushNotification(key, message) =>
            maybeNtfyer match {
              case Some(ntfyer) => ntfyer ! NtfyerActor.DoNotify(key, message)
              case None => context.log.warn(s"Received `$message` for key $key but no NtfyerActor was provided for testing")
            }
          case NotificationCenterManager.HueCommand(command) => ???

          case Chime(message) => ???
        }

        Behaviors.same

      case ExpireCooldown(_) => ???
    }
  }
}
