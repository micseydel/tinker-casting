package me.micseydel.testsupport

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.TinkerProbeActor
import me.micseydel.actor.notifications.NotificationCenterManager.PushNotification
import me.micseydel.actor.perimeter.NtfyerActor

object NtfyForTesting {
  def apply(probe: ActorRef[TinkerProbeActor.RecordSideEffect]): Behavior[NtfyerActor.Message] = Behaviors.receive { (context, message) =>
    message match {
      case dn@NtfyerActor.DoNotify(key, message) =>
        probe ! TinkerProbeActor.RecordSideEffect(PushNotification(key, message))
        Behaviors.same
      case NtfyerActor.NtfyHttpCallResult(response) => ???
    }
  }
}
