package me.micseydel.testsupport

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.testsupport.TimeOrchestratorForTesting.Deferral

object DeferringTimeKeeperForTesting {
  def apply(deferTo: ActorRef[TimeOrchestratorForTesting.Message]): Behavior[TimeKeeper.Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage { message =>
      deferTo ! Deferral(message)
      Behaviors.same
      //      case TimeKeeper.RemindMeIn(delay, replyTo, message, timerKey) => ???
      //      case TimeKeeper.RemindMeEvery(delay, replyTo, message, timerKey) => ???
      //      case TimeKeeper.Cancel(key) => ???
      //      case TimeKeeper.RemindMeDailyAt(hour, minute, replyTo, message, timerKey) => ???
      //      case _ => ???
    }
  }
}
