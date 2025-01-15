package me.micseydel.testsupport

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.TinkerProbeActor
import me.micseydel.TinkerProbeActor.RecordSideEffect
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{HueCommand, Notification}
import me.micseydel.actor.perimeter.{HueControl, NtfyerActor}

object HueControlForTesting {
  def apply(probe: ActorRef[TinkerProbeActor.Message]): Behavior[HueControl.Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case command: HueControl.Command =>
        probe !  RecordSideEffect(HueCommand(command))
        Behaviors.same

      case HueControl.StartTinkering(_) =>
        ???

      case update: HueControl.StateUpdate =>
        ???
    }
  }
}
