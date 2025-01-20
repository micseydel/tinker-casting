package me.micseydel.actor

import me.micseydel.actor.EventReceiver.HeartRate
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability

object HeartRateMonitorActor {
  sealed trait Message
  final case class Receive(payload: String) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    context.system.eventReceiver ! EventReceiver.ClaimEventType(HeartRate, context.messageAdapter(Receive).underlying)

    Tinker.withMessages {
      case Receive(payload) =>
        context.actorContext.log.info(s"Received $payload")
        Tinker.steadily
    }
  }
}
