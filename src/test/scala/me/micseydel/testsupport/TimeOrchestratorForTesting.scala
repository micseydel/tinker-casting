package me.micseydel.testsupport

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.dsl.cast.TimeKeeper

object TimeOrchestratorForTesting {
  sealed trait Message

  final case class Fetch(replyTo: ActorRef[ActorRef[TimeKeeper.Message]]) extends Message

  final case class Deferral(message: TimeKeeper.Message) extends Message

  // FIXME
  final case class Go() extends Message

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    implicit val timeKeeper: ActorRef[TimeKeeper.Message] = context.spawn(DeferringTimeKeeperForTesting(context.self), "DeferringTimeKeeperForTesting")
    waitUntilToldToGo()
  }

  def waitUntilToldToGo()(implicit timeKeeper: ActorRef[TimeKeeper.Message]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case Fetch(replyTo) =>
        replyTo ! timeKeeper
        Behaviors.same
      case Deferral(message) =>
//        TestHelpers.log(s"Deferring $message")
        deferring(List(deferOrJustCrash(message)))

      case Go() =>
        TestHelpers.log("Go-ing but nothing buffered")
        Behaviors.same
    }
  }

  import TimeKeeper._

  private def deferring(punting: List[Punting[_]])(implicit timeKeeper: ActorRef[TimeKeeper.Message]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case Fetch(replyTo) =>
        replyTo ! timeKeeper
        Behaviors.same
      case Deferral(message) =>
//        TestHelpers.log(s"Deferring $message, total buffer size is now ${punting.size + 1}")
        deferring(deferOrJustCrash(message) :: punting)

      case Go() =>
        TestHelpers.log(s"Go-ing for ${punting.size} buffered elements")
        punting.foreach {
          case Punting(replyTo, message) =>
            replyTo ! message
        }
        waitUntilToldToGo()
    }
  }

  case class Punting[M](replyTo: ActorRef[M], message: M)

  object Punting {
    def apply[M](replyTo: ActorRef[M], message: M) = new Punting[M](replyTo, message)
  }

  private def deferOrJustCrash[M](message: TimeKeeper.Message): Punting[_] = {
    message match {
      case RemindMeIn(delay, replyTo, message, timerKey) => Punting(replyTo.underlying, message)
      case RemindMeEvery(delay, replyTo, message, timerKey) => ???
      case Cancel(key) => ???
      case RemindMeDailyAt(hour, minute, replyTo, message, timerKey) => Punting(replyTo.underlying, message)
      case TimerFired(replyTo, message) => ???
    }
  }
}
