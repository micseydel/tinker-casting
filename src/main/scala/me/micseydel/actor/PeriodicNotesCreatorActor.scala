package me.micseydel.actor

import me.micseydel.actor.DailyNoteActor.TheDayHasPassed
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Operator, Tinker, TinkerContext}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef.{Contents, FileDoesNotExit, FileReadException}

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, ZonedDateTime}

object PeriodicNotesCreatorActor {
  sealed trait Message
  private case class ItsMidnight(day: LocalDate) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info("Starting up PeriodicNotesCreatorActor")
    setup()
  }

  private def setup()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Periodic Notes Templates", rgb(255, 222, 71), "🌞") { (context, noteRef) =>
    // FIXME: use the noteRef to wire in non-hard coded template
    implicit val c: TinkerContext[_] = context
    context.actorContext.log.info("Subscribing ItsMidnight message through Operator")
    context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))

    val dailyNoteLookup: LookUpSpiritByDay[DailyNoteActor.Message] = LookUpSpiritByDay[DailyNoteActor.Message] { (context, captureDate) =>
      context.cast(DailyNoteActor(captureDate), TimeUtil.localDateTimeToISO8601Date(captureDate))
    }

    behavior(dailyNoteLookup)
  }

  private def behavior(dailyNoteLookup: LookUpSpiritByDay[DailyNoteActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case ItsMidnight(dayForNote) =>
        dailyNoteLookup :?> dayForNote match {
          case (potentiallyUpdatedLookup, _) =>
            potentiallyUpdatedLookup :?> dayForNote.minusDays(1) match {
              case (potentiallyEvenMoreUpdatedLookup, yesterdaysDailyNoteActor) =>
                yesterdaysDailyNoteActor !! TheDayHasPassed
                behavior(potentiallyEvenMoreUpdatedLookup)
            }
        }
    }
  }

  def main(args: Array[String]): Unit = {
    // these replacements are hacks for Obsidian Templater compatibility
    println(s"""|<% "---" %>
                |aliases:
                |- <% moment().format("dddd, D MMMM YYYY (DDD)") %>
                |---
                |
                |-
                |
                |# I/O
                |
                |-
                |
                |# See also
                |
                |- Prior day: [[<% tp.date.yesterday("YYYY-MM-DD") %>]]
                |- Next day: [[<% tp.date.tomorrow("YYYY-MM-DD") %>]]
                |- [[Things I wish I had time for (<% tp.date.now("YYYY-MM-DD") %>)]]
                |- Captures
                |    - [[Transcribed mobile notes (<% tp.date.now("YYYY-MM-DD") %>)]]
                |        - Cats
                |            - [[CatsTranscriptions notes (<% tp.date.now("YYYY-MM-DD") %>)]]
                |            - [[Structured cats notes (<% tp.date.now("YYYY-MM-DD") %>)]]
                |                - [[Litter boxes (<% tp.date.now("YYYY-MM-DD") %>)]]
                |                - [[Litter boxes sifting (<% tp.date.now("YYYY-MM-DD") %>)]]
                |
                |""".stripMargin)
  }
}

object DailyNoteActor {
  sealed trait Message

  final case object TheDayHasPassed extends Message

  def apply(forDay: LocalDate)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(forDay.toString, rgb(255, 222, 71), "•", Some("periodic_notes/daily")) { (context, noteRef) =>
    noteRef.readMarkdownSafer() match {
      case Contents(_) =>
        context.actorContext.log.info(s"NOT creating daily note for $forDay, note already exists")
        behavior(forDay)
      case FileDoesNotExit =>
        context.actorContext.log.info(s"Creating daily note for $forDay")
        noteRef.setMarkdown(PeriodicNotesCreatorActorUtil.template(forDay))
        behavior(forDay)
      case FileReadException(exception) =>
        context.actorContext.log.error(s"Daily note creation for $forDay failed", exception)
        Tinker.ignore
    }
  }

  private def behavior(forDay: LocalDate)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case TheDayHasPassed =>
        context.actorContext.log.info(s"The day $forDay has passed")
        Tinker.steadily
    }
  }
}

private object PeriodicNotesCreatorActorUtil {
  val VerboseDayFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("EEEE, d MMMM yyyy (DDD)")
  val IsoDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val mobileTextCaptureFormatter = DateTimeFormatter.ofPattern("M-d-yy")

  def template(today: LocalDate): String = {
    val yesterday = today.minus(1, ChronoUnit.DAYS)
    val tomorrow = today.plus(1, ChronoUnit.DAYS)

    s"""|---
        |aliases:
        |- ${VerboseDayFormatter.format(today)}
        |---
        |
        |-
        |
        |# I/O
        |
        |-
        |
        |# See also
        |
        |- Prior day: [[${IsoDateFormatter.format(yesterday)}]]
        |- Next day: [[${IsoDateFormatter.format(tomorrow)}]]
        |- [[Things I wish I had time for (${IsoDateFormatter.format(today)})]]
        |- Captures
        |    - [[Transcribed mobile notes (${IsoDateFormatter.format(today)})]]
        |        - Cats
        |            - [[CatsTranscriptions notes (${IsoDateFormatter.format(today)})]]
        |            - [[Structured cats notes (${IsoDateFormatter.format(today)})]]
        |                - [[Litter boxes (${IsoDateFormatter.format(today)})]]
        |                - [[Litter boxes sifting (${IsoDateFormatter.format(today)})]]
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
