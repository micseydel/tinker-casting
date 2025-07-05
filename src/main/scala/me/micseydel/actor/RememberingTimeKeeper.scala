package me.micseydel.actor

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl._
import me.micseydel.vault.persistence.TypedJsonRef
import spray.json.{DefaultJsonProtocol, JsonFormat}

import java.time.Duration._
import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

object RememberingTimeKeeper {
  sealed trait Message[M]

  case class RemindMeIn[M](delay: FiniteDuration, message: M, timerKey: String) extends Message[M]

  case class Cancel[M](timerKey: String) extends Message[M]

  def RemindMeAt[M](zonedDateTime: ZonedDateTime, message: M, timerKey: String)(implicit tinkerClock: TinkerClock): RemindMeIn[M] = {
    val delay = FiniteDuration(between(tinkerClock.now(), zonedDateTime).toSeconds, TimeUnit.SECONDS)
    RemindMeIn(
      delay,
      message,
      timerKey
    )
  }

  def apply[M](replyTo: SpiritRef[M], jsonFormat: JsonFormat[M], filename: String)(implicit Tinker: Tinker): Ability[Message[M]] =
    Tinkerer(rgb(100, 200, 10), "🔮").initializedWithTypedJson(filename, StateJsonFormat(jsonFormat)) { case (context, typedJsonRef) =>
      val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      context.actorContext.log.info(s"Reading $filename json from disk, re-creating any non-expired timers and then writing non-expired back to disk...")

      implicit val sender: Sender = context.sender
      val latestState: State[M] = typedJsonRef.updateOrSetDefault(State.empty) {
        case State(entries) =>
          State[M](entries.values.collect {
            case pending@Pending(when, key, message) if context.system.clock.now().isBefore(when) =>
              val delay = FiniteDuration(between(context.system.clock.now(), when).toMinutes, TimeUnit.MINUTES)
              context.actorContext.log.info(s"Setting $key for $delay")
              timeKeeper !!! TimeKeeper.RemindMeIn(delay, replyTo, message, Some(key))
              key -> pending
          }.toMap)
      } match {
        case Failure(exception) => throw exception
        case Success(state) => state
      }

      context.actorContext.log.info(s"Latest state $latestState")

      behavior(typedJsonRef, replyTo, timeKeeper)
    }

  //

  private def behavior[M](jsonRef: TypedJsonRef[State[M]], replyTo: SpiritRef[M], timeKeeper: SpiritRef[TimeKeeper.Message])(implicit sender: Sender, Tinker: Tinker): Ability[Message[M]] = Tinker.receive { (context, message) =>
    context.actorContext.log.info("Updating JSON persistent state")

    message match {
      case message: Message[_] =>
        implicit val clock: TinkerClock = context.system.clock
        val latestState: State[M] = jsonRef.updateOrSetDefault(State.empty)(_.integrate(message)) match {
          case Failure(exception) => throw exception
          case Success(state) => state
        }

        context.actorContext.log.info(s"Integrated $message, latest state $latestState")

        message match {
          case RemindMeIn(delay, messageToForward, timerKey) =>
            context.actorContext.log.info(s"Using delay ${delay.toSeconds} seconds for $timerKey")
            timeKeeper !!! TimeKeeper.RemindMeIn(delay, replyTo, messageToForward, Some(timerKey))
            Tinker.steadily

          case Cancel(timerKey) =>
            context.actorContext.log.info(s"Canceling for $timerKey")
            timeKeeper !!! TimeKeeper.Cancel(timerKey)
            Tinker.steadily
        }
    }
  }

  // model

  case class Pending[M](when: ZonedDateTime, key: String, message: M)

  case class State[M](entries: Map[String, Pending[M]]) {
    def integrate(command: Message[M])(implicit tinkerClock: TinkerClock): State[M] = {
      State[M](command match {
        case RemindMeIn(delay, message, timerKey) =>
          entries.updated(timerKey, Pending(
            tinkerClock.now().plusMinutes(delay.toMinutes),
            timerKey,
            message
          ))

        case Cancel(timerKey) =>
          entries.removed(timerKey)
      })
    }
  }

  object State {
    def empty[M]: State[M] = State[M](Map.empty)
  }

  // serialization

  private object StateJsonFormat extends DefaultJsonProtocol {
    def apply[M](implicit jsonFormat: JsonFormat[M]): JsonFormat[State[M]] = {
      import me.micseydel.Common.ZonedDateTimeJsonFormat
      implicit val entryJsonFormat: JsonFormat[Pending[M]] = jsonFormat3(Pending[M])
      jsonFormat1(State[M])
    }
  }
}
