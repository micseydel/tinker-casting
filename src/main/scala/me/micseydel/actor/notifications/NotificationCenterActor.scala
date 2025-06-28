package me.micseydel.actor.notifications

import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.Notification
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}

import scala.util.{Failure, Success}

object NotificationCenterActor {
  sealed trait Message
  private case class ReceiveNotePing(ping: Ping) extends Message
  private[notifications] case class AddNotification(notification: Notification) extends Message
  private[notifications] case class ClearNotification(id: String) extends Message

  private val NoteName = "Notification Center"
  def apply(completeNotification: SpiritRef[NotificationCenterManager.CompleteNotification])(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](NoteName, rgb(205, 205, 0), "📢", ReceiveNotePing) { (context, noteRef) =>
    implicit val c: TinkerContext[_] = context
    Tinker.receiveMessage {
      case ReceiveNotePing(_) =>
        NotificationCenterManagerMarkdown.clearDone(noteRef) match {
          case Success(cleared) =>
            if (cleared.nonEmpty) {
              // FIXME: create a batch-receiving message
              for (clearToPropagate <- cleared) {
                completeNotification !! NotificationCenterManager.CompleteNotification(clearToPropagate.id)
              }
              context.actorContext.log.info(s"Cleared: $cleared")
            }

          case Failure(exception) =>
            context.actorContext.log.error(s"Failed to clear done ${noteRef.noteId.asString} elements", exception)
        }

        Tinker.steadily

      case AddNotification(notification) =>
        NotificationCenterManagerMarkdown.addNotification(noteRef, notification)
        Tinker.steadily

      case ClearNotification(id) =>
        NotificationCenterManagerMarkdown.clearNotification(noteRef, id)
        Tinker.steadily
    }
  }
}
