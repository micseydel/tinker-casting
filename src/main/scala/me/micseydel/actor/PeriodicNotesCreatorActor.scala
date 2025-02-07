package me.micseydel.actor

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.{SystemWideTimeKeeper, TimeKeeper}
import me.micseydel.dsl.{Operator, SpiritRef, Tinker, TinkerContext, TinkerContextImpl, Tinkerer}
import me.micseydel.util.FileSystemUtil
import me.micseydel.vault.VaultPath

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object PeriodicNotesCreatorActor {
  sealed trait Message
  private case class ItsMidnight(itsMidnight: SystemWideTimeKeeper.ItsMidnight.type) extends Message

  def apply(vaultPath: VaultPath)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info("Starting up")
    behavior(vaultPath)
  }

  private def behavior(vaultPath: VaultPath)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(255, 222, 71), "🌞").setup { context =>
    implicit val c: TinkerContext[_] = context
    context.actorContext.log.info("Subscribing ItsMidnight message through Operator")
    context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))

    Tinker.receiveMessage {
      case ItsMidnight(_) =>
        val dayForNote = {
          val now = context.system.clock.now()
          MillisFromMidnight(now) match {
            case MillisUntil(millis) =>
              context.actorContext.log.info(s"Received ItsMidnight $millis ms before midnight, using TOMORROW")
              now.plusDays(1)
            case MillisSince(millis) =>
              context.actorContext.log.info(s"Received ItsMidnight $millis ms after midnight, using TODAY")
              now
          }
        }

        context.actorContext.log.info(s"Using $dayForNote for the day")

        val notePath = vaultPath.resolve("periodic_notes")
          .resolve("daily")
          .resolve(s"${isoDateFormatter.format(dayForNote)}.md")

        // FIXME: abstract this into the noteref, remove FileSystemUtil uses in this package
        val shouldNotCreate = FileSystemUtil.pathIsANonEmptyFile(notePath.toString)
        if (shouldNotCreate) {
          context.actorContext.log.warn(s"Was about to write to $notePath but it wasn't empty so aborted")
        } else {
          context.actorContext.log.info(s"Writing daily note to $notePath")
          val contents = template(dayForNote)
          FileSystemUtil.writeToPath(notePath, contents)
        }

        Tinker.steadily
    }
  }

  // util

  private val verboseDayFormatter = DateTimeFormatter.ofPattern("EEEE, d MMMM yyyy (DDD)")
  private val mobileTextCaptureFormatter = DateTimeFormatter.ofPattern("M-d-yy")
  private val isoDateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  private def template(today: ZonedDateTime): String = {
    val yesterday = today.minus(1, ChronoUnit.DAYS)
    val tomorrow = today.plus(1, ChronoUnit.DAYS)

    s"""|---
       |aliases:
       |- ${verboseDayFormatter.format(today)}
       |---
       |
       |-
       |
       |# See also
       |
       |- Prior day: [[${isoDateFormatter.format(yesterday)}]]
       |- Next day: [[${isoDateFormatter.format(tomorrow)}]]
       |- Captures
       |    - [[Transcribed mobile notes (${isoDateFormatter.format(today)})]]
       |        - Cats
       |            - [[CatsTranscriptions notes (${isoDateFormatter.format(today)})]]
       |            - [[Structured cats notes (${isoDateFormatter.format(today)})]]
       |                - [[Litter boxes (${isoDateFormatter.format(today)})]]
       |                - [[Litter boxes sifting (${isoDateFormatter.format(today)})]]
       |
       |""".stripMargin
  }
}

sealed trait MillisFromMidnight
final case class MillisUntil(millis: Long) extends MillisFromMidnight
final case class MillisSince(millis: Long) extends MillisFromMidnight

object MillisFromMidnight {
  def apply(triggerTime: ZonedDateTime): MillisFromMidnight = {
    // triggerTime is expected to be shortly before, or shortly after midnight (observed ~200ms)
    val startOfToday = triggerTime.toLocalDate.atStartOfDay(triggerTime.getZone)
    val startOfTomorrow = startOfToday.plusDays(1)

    // these are both absolute value; the sum is 24 hours
    val millisBetweenTodayStartAndTriggerTime: Long = ChronoUnit.MILLIS.between(startOfToday, triggerTime)
    val millisBetweenTriggerTimeAndTomorrowStart: Long = ChronoUnit.MILLIS.between(triggerTime, startOfTomorrow)

    if (millisBetweenTodayStartAndTriggerTime > millisBetweenTriggerTimeAndTomorrowStart) {
      // this means it's BEFORE midnight
      MillisUntil(millisBetweenTriggerTimeAndTomorrowStart)
    } else {
      // this means AFTER or EXACTLY midnight
      MillisSince(millisBetweenTodayStartAndTriggerTime)
    }
  }

  def midnightFor(zonedDateTime: ZonedDateTime): ZonedDateTime = {
    zonedDateTime.withHour(0).withMinute(0).withSecond(0).withNano(0)
  }

  // manual testing

//  def main(args: Array[String]): Unit = {
//    val twoHundredMillisInNanos = 200.millis.toNanos // 200000000
//
//    val midnight = ZonedDateTime.of(2023, 12, 24, 0, 0, 0, 0, ZonedDateTime.now().getZone)
//
//    val justBeforeMidnight = MillisFromMidnight(midnight.minusNanos(twoHundredMillisInNanos))
//    val justAfterMidnight = MillisFromMidnight(midnight.plusNanos(twoHundredMillisInNanos))
//
//    println(s"justBeforeMidnight: $justBeforeMidnight")
//    println(s"Midnight: ${MillisFromMidnight(midnight)}")
//    println(s"justAfterMidnight: $justAfterMidnight")
//
//    println(MillisFromMidnight(ZonedDateTime.parse("2023-12-24T23:59:59.965142-08:00[America/Los_Angeles]")))
//  }
}
