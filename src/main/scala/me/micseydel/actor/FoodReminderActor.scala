package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.HungerTracker.HungerState
import me.micseydel.actor.LastAteNoteRef.LastAteNoteRefResult
import me.micseydel.actor.NutritionListener.LastAte
import me.micseydel.actor.notifications.NotificationCenterManager.{JustSideEffect, PushNotification}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.time.format.DateTimeParseException
import java.time.{Duration, ZonedDateTime}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS, MILLISECONDS}
import scala.util.{Failure, Success, Try}

object FoodReminderActor {
  sealed trait Message

  case class CaloriesConsumed(when: ZonedDateTime) extends Message

//  private
  case object TimeHasPassed extends Message

  def apply(hungerTracker: SpiritRef[HungerState], foodTimeNtfyKey: Option[String])(implicit Tinker: Tinker): Ability[Message] =
    setup(hungerTracker, foodTimeNtfyKey)

  val NoteName = "last_ate"
  val MaxTimeWithoutEating: FiniteDuration = 4.hours

  private def setup(hungerTracker: SpiritRef[HungerState], foodTimeNtfyKey: Option[String])(implicit Tinker: Tinker): Ability[Message] = {
    Tinkerer(rgb(135, 206, 235), "🍲").withNote(NoteName) { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context
      val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      // FIXME: consider restoring this instead of the lower TimeHasPassed message, so that off-hours can detect hunger
      // (detect WITHOUT bothersome notifications)
      //    context.self !! TimeHasPassed // get things started - this will result in timer creation

      if (foodTimeNtfyKey.isEmpty) {
        context.actorContext.log.warn("Food time Ntfy key missing, will not be sending push notifications")
      }

      val now = context.system.clock.now()
      val hour = now.getHour

      val wrappedNoteRef = new LastAteNoteRef(noteRef)

      wrappedNoteRef.useLastAteOrLog(context.actorContext.log) { lastAte =>
        if (now.isAfter(lastAte.plusHours(MaxTimeWithoutEating.toHours))) {
          context.actorContext.log.info(s"Last ate at $lastAte, isHungry=true")
          hungerTracker !! HungerState(LastAte(lastAte), isHungry = true)
        } else {
          context.actorContext.log.info(s"Last ate at $lastAte, isHungry=false")
          hungerTracker !! HungerState(LastAte(lastAte), isHungry = false)
        }
      }

      if (now.isDaylight) {
        context.actorContext.log.info(s"$NoteName reminder initializing; hour $hour, starting in fastedNOTok state")
        fastedNOTok()(Tinker, wrappedNoteRef, timeKeeper, hungerTracker, foodTimeNtfyKey)
      } else {
        context.actorContext.log.info(s"$NoteName reminder initializing; hour $hour, starting in fastedok state")
        fastedok()(Tinker, wrappedNoteRef, timeKeeper, hungerTracker, foodTimeNtfyKey)
      }
    }
  }

  private def fastedNOTok()(implicit Tinker: Tinker, noteRef: LastAteNoteRef, timeKeeper: SpiritRef[TimeKeeper.Message], hungerTracker: SpiritRef[HungerState], foodTimeNtfyKey: Option[String]): Ability[Message] = Tinker.setup { setupContext =>
    // get the ball rolling on startup
    (setupContext.self !! TimeHasPassed)(setupContext)

    Tinker.receive { (context, message) =>
      implicit val c: TinkerContext[_] = context
      implicit val tc: TinkerClock = context.system.clock

      message match {
        case TimeHasPassed =>
          if (context.system.clock.now().isDaylight) {
            noteRef.useLastAteOrLog(context.actorContext.log) { lastAte =>
              val duration = TimeUtil.timeSince(lastAte)
              if (duration.toHours >= MaxTimeWithoutEating.toHours) {
                val overdueMinutes = duration.toMinutes - MaxTimeWithoutEating.toMinutes
                val notification = s"You are $overdueMinutes minutes overdue to eat!"
                context.actorContext.log.info(s"Sending notification: $notification")
                foodTimeNtfyKey.foreach { key =>
                  context.system.notifier !! JustSideEffect(PushNotification(key, notification))
                }
                hungerTracker !! HungerState(LastAte(lastAte), isHungry = true)
              } else {
                val finiteDuration = FiniteDuration(duration.toMillis, MILLISECONDS)
                val waitTime: FiniteDuration = MaxTimeWithoutEating - finiteDuration
                context.actorContext.log.info(s"Last ate $lastAte, delaying notification by $waitTime")
                timeKeeper !! TimeKeeper.RemindMeIn(waitTime, context.self, TimeHasPassed, Some(TimeHasPassed))
                hungerTracker !! HungerState(LastAte(lastAte), isHungry = false)
              }
            }

            Tinker.steadily
          } else {
            fastedok()
          }

        case CaloriesConsumed(when) =>
          val now = ZonedDateTime.now(when.getZone)
          val duration = Duration.between(when, now)
          val finiteDuration = FiniteDuration(duration.toMillis, MILLISECONDS)


          val waitTime: FiniteDuration = MaxTimeWithoutEating - finiteDuration

          context.actorContext.log.info(s"Writing $when to $NoteName and setting a new timer for $waitTime")

          noteRef.setLastAte(when)

          val momentarilySaited = waitTime.toMillis > 0
          if (momentarilySaited) {
            hungerTracker !! HungerState(LastAte(when), isHungry = false)
            timeKeeper !! TimeKeeper.RemindMeIn(waitTime, context.self, TimeHasPassed, Some(TimeHasPassed))
          } else {
            hungerTracker !! HungerState(LastAte(when), isHungry = true)
            context.self !! TimeHasPassed
          }

          Tinker.steadily
      }
    }
  }

  private def fastedok()(implicit Tinker: Tinker, noteRef: LastAteNoteRef, timeKeeper: SpiritRef[TimeKeeper.Message], hungerTracker: SpiritRef[HungerState], foodTimeNtfyKey: Option[String]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    implicit val tc: TinkerClock = context.system.clock

    message match {
      case TimeHasPassed =>
        val now = tc.now()
        if (now.isDaylight) {
          context.self !! TimeHasPassed
          fastedNOTok()
        } else if (now.getHour < 9) {
          val waitTime = TimeUtil.timeUntil(9, 0, now)
          timeKeeper !! TimeKeeper.RemindMeIn(waitTime, context.self, TimeHasPassed, Some(TimeHasPassed))
          Tinker.steadily
        } else { // > 22
          // just punt to past midnight for now
          val waitTime = TimeUtil.timeUntil(23, 59, now) + FiniteDuration(2.minutes.toMinutes, TimeUnit.MINUTES)
          timeKeeper !! TimeKeeper.RemindMeIn(waitTime, context.self, TimeHasPassed, Some(TimeHasPassed))
          Tinker.steadily
        }

      case CaloriesConsumed(when) =>
        val now = tc.now()
        val duration = Duration.between(when, now)
        val waitTime: FiniteDuration = MaxTimeWithoutEating - FiniteDuration(duration.toMillis, MILLISECONDS)

        context.actorContext.log.info(s"Writing $when to $NoteName and setting a new timer for $waitTime")

        noteRef.setLastAte(when)

        // FIXME: should check the timing here
        hungerTracker !! HungerState(LastAte(when), isHungry = false)

        if (now.isDaylight) {
          context.actorContext.log.info(s"Hour ${now.getHour}, switching from fastedok to fastedNOTok")
          fastedNOTok()
        } else {
          timeKeeper !! TimeKeeper.RemindMeIn(waitTime, context.self, TimeHasPassed, Some(TimeHasPassed))
          Tinker.steadily
        }

    }
  }

  implicit class RichTime(val t: ZonedDateTime) extends AnyVal {
    def isDaylight: Boolean = {
      val hour = t.getHour
      hour >= 9 && hour < 22
    }
  }
}

private class LastAteNoteRef(noteRef: NoteRef) {
  def useLastAteOrLog(log: Logger)(f: ZonedDateTime => Unit): NoOp.type = {
    getLastAte() match {
      case LastAteNoteRef.Success(lastAte) => f(lastAte)
      case failure: LastAteNoteRef.Failure =>
        val message = failure match {
          case LastAteNoteRef.FileNotFound(_) => "file not found"
          case LastAteNoteRef.MalformedContent(_) => "malformed content"
          case LastAteNoteRef.UnknownError(_) => "unknown error"
        }

        if (log.isDebugEnabled) {
          log.warn(message, failure.exception)
        } else {
          log.warn(message)
        }
    }
    NoOp
  }

  private def getLastAte(): LastAteNoteRefResult = noteRef.readMarkdown().map(_.trim).flatMap { raw =>
    Try(ZonedDateTime.parse(raw))
  } match {
    case Success(value) => LastAteNoteRef.Success(value)
    case Failure(exception: FileNotFoundException) => LastAteNoteRef.FileNotFound(exception)
    case Failure(exception: DateTimeParseException) => LastAteNoteRef.MalformedContent(exception)
    case Failure(exception) => LastAteNoteRef.UnknownError(exception)
  }

  def setLastAte(lastAte: ZonedDateTime): NoOp.type = {
    noteRef.setMarkdown(lastAte.toString) match {
      case Failure(exception) => throw exception
      case Success(value) => value
    }
  }
}

private object LastAteNoteRef {
  sealed trait LastAteNoteRefResult

  case class Success(lastAte: ZonedDateTime) extends LastAteNoteRefResult

  sealed trait Failure extends LastAteNoteRefResult {
    val exception: Throwable
  }

  case class FileNotFound(exception: FileNotFoundException) extends Failure
  case class MalformedContent(exception: DateTimeParseException) extends Failure
  case class UnknownError(exception: Throwable) extends Failure
}
