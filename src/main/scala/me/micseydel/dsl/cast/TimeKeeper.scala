package me.micseydel.dsl.cast

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
import me.micseydel.util.TimeUtil

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import java.util.UUID
import scala.concurrent.duration.*

object TimeKeeper {
  sealed trait Message

  /**
   * Used for one-off reminders.
   */
  case class RemindMeIn[M](delay: FiniteDuration, replyTo: SpiritRef[M], message: M, timerKey: Option[Any]) extends Message

  object RemindMeIn {
    def apply[M](delay: FiniteDuration, replyTo: SpiritRef[M], message: M, timerKey: Option[Any]): RemindMeIn[M] = {
      new RemindMeIn(delay, replyTo, message, timerKey)
    }
  }

  def RemindMeAt[M](at: ZonedDateTime, recipient: SpiritRef[M], message: M, timerKey: Option[Any])(implicit clock: TinkerClock): RemindMeIn[M] = {
    val delay = TimeUtil.between(clock.now(), at)
    RemindMeIn(delay, recipient, message, timerKey)
  }

  def RemindMeAt[M](at: LocalDate, recipient: SpiritRef[M], message: M, timerKey: Option[Any])(implicit clock: TinkerClock): RemindMeIn[M] = {
    RemindMeAt(at.atStartOfDay(ZoneId.systemDefault()), recipient, message, timerKey)
  }

  case class RemindMeEvery[M](interval: FiniteDuration, initialDelay: Option[FiniteDuration], replyTo: SpiritRef[M], message: M, timerKey: Option[Any]) extends Message

  object RemindMeEvery {
    def apply[M](interval: FiniteDuration, replyTo: SpiritRef[M], message: M, timerKey: Option[Any]): RemindMeEvery[M] =
      RemindMeEvery(interval, None, replyTo, message, timerKey)

    def apply[M](interval: FiniteDuration, initialDelay: FiniteDuration, replyTo: SpiritRef[M], message: M, timerKey: Option[Any]): RemindMeEvery[M] =
      RemindMeEvery(interval, Some(initialDelay), replyTo, message, timerKey)
  }

  case class Cancel(key: Any) extends Message

  /**
   * e.g. "remind me daily at midnight" would be hour=0
   *
   * @param hour   - [0, 23]
   * @param minute - [0, 59]
   */
  case class RemindMeDailyAt[M](hour: Int, minute: Int, replyTo: SpiritRef[M], message: M, timerKey: Option[Any]) extends Message

  object RemindMeDailyAt {
    /**
     * Convenience for RemindMeDailyAt case class generation with minute=0
     */
    def apply[M](hour: Int, replyTo: SpiritRef[M], message: M, timerKey: Option[Any]): RemindMeDailyAt[M] = {
      new RemindMeDailyAt[M](hour, 0, replyTo, message, timerKey)
    }
  }


//  private
  case class TimerFired[M](replyTo: SpiritRef[M], message: M) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(0, 255, 0), "⏰").setup { context =>
    implicit val c: TinkerContext[_] = context

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

          if (context.actorContext.log.isDebugEnabled) {
            context.actorContext.log.debug(s"For RemindMeDailyAt(hour=$hour, minute=$minute) using initial delay $initialDelay (key=$key, message=$message)")
          } else {
            context.actorContext.log.info(s"For RemindMeDailyAt(hour=$hour, minute=$minute) using initial delay $initialDelay (key=$key)")
          }

          timers.startTimerAtFixedRate(
            key.getOrElse(UUID.randomUUID().toString),
            TimerFired(replyTo, message),
            initialDelay,
            delay
          )

          Behaviors.same

        case RemindMeEvery(interval, maybeInitialDelay, replyTo, message, timerKey) =>
          context.actorContext.log.debug("Starting timer for every {}", interval)
          maybeInitialDelay match {
            case Some(initialDelay) =>
              timers.startTimerAtFixedRate(timerKey.getOrElse(UUID.randomUUID()), TimerFired(replyTo, message), initialDelay, interval)
            case None =>
              timers.startTimerAtFixedRate(timerKey.getOrElse(UUID.randomUUID()), TimerFired(replyTo, message), interval)
          }
          Behaviors.same

        case TimerFired(replyTo, message) =>
          context.actorContext.log.debug("Sending {} message to replyTo {}", message, replyTo)
          replyTo !! message
          Behaviors.same

        case Cancel(key) =>
          context.actorContext.log.debug("Canceling key {}", key)
          timers.cancel(key)
          Behaviors.same
      }
    }
  }
}
