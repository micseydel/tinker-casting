package me.micseydel.dsl.cast

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.util.TimeUtil

import java.util.UUID
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object UntrackedTimeKeeper {
  sealed trait Message

  /**
   * Used for one-off reminders.
   */
  case class RemindMeIn[M](delay: FiniteDuration, replyTo: ActorRef[M], message: M, timerKey: Option[Any]) extends Message

  object RemindMeIn {
    def apply[M](delay: FiniteDuration, replyTo: ActorRef[M], message: M, timerKey: Option[Any]): RemindMeIn[M] = {
      new RemindMeIn(delay, replyTo, message, timerKey)
    }
  }

  case class RemindMeEvery[M](delay: FiniteDuration, replyTo: ActorRef[M], message: M, timerKey: Option[Any]) extends Message

  case class Cancel(key: Any) extends Message

  /**
   * e.g. "remind me daily at midnight" would be hour=0
   *
   * @param hour   - [0, 23]
   * @param minute - [0, 59]
   */
  case class RemindMeDailyAt[M](hour: Int, minute: Int, replyTo: ActorRef[M], message: M, timerKey: Option[Any]) extends Message

  object RemindMeDailyAt {
    /**
     * Convenience for RemindMeDailyAt case class generation with minute=0
     */
    def apply[M](hour: Int, replyTo: ActorRef[M], message: M, timerKey: Option[Any]): RemindMeDailyAt[M] = {
      new RemindMeDailyAt[M](hour, 0, replyTo, message, timerKey)
    }
  }


  private case class TimerFired[M](replyTo: ActorRef[M], message: M) extends Message

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.withTimers { timers =>
      // startTimerAtFixedRate - send at fixed rate, e.g. every 5 seconds

      Behaviors.receiveMessage {
        case RemindMeIn(delay, replyTo, message, key) =>
          timers.startSingleTimer(
            key.getOrElse(UUID.randomUUID()),
            TimerFired(replyTo, message),
            delay
          )

          Behaviors.same

        case RemindMeDailyAt(hour, minute, replyTo, message, key) =>
          val initialDelay: FiniteDuration = TimeUtil.timeUntil(hour, minute)
          val delay: FiniteDuration = 24.hours

          if (context.log.isDebugEnabled) {
            context.log.debug(s"For RemindMeDailyAt(hour=$hour, minute=$minute) using initial delay $initialDelay (key=$key, message=$message)")
          } else {
            context.log.info(s"For RemindMeDailyAt(hour=$hour, minute=$minute) using initial delay $initialDelay (key=$key)")
          }

          timers.startTimerWithFixedDelay(
            key,
            TimerFired(replyTo, message),
            initialDelay,
            delay
          )

          Behaviors.same

        case RemindMeEvery(delay, replyTo, message, timerKey) =>
          context.log.debug("Starting timer for every {}", delay)
          timers.startTimerAtFixedRate(timerKey.getOrElse(UUID.randomUUID()), TimerFired(replyTo, message), delay)
          Behaviors.same

        case TimerFired(replyTo, message) =>
          context.log.debug("Sending {} message to replyTo {}", message, replyTo)
          replyTo ! message
          Behaviors.same

        case Cancel(key) =>
          context.log.debug("Canceling key {}", key)
          timers.cancel(key)
          Behaviors.same
      }
    }
  }
}
