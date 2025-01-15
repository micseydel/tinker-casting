package me.micseydel.testsupport

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.Operator

object OperatorForTesting {
  def apply(): Behavior[Operator.Message] = Behaviors.receive { (context, message) =>
    message match {
      case anything =>
        context.log.warn(s"Operator received $anything")
        Behaviors.same
      //      case Gossiper.StartTinkering(tinker) => ???
      //      case Gossiper.Receive(notedTranscription) => ???
      //      case Gossiper.SubscribeAccurate(subscriber) => ???
      //      case Gossiper.SubscribeHybrid(subscriber) => ???
    }
  }
}
