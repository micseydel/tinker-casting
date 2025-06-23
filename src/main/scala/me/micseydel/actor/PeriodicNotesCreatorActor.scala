package me.micseydel.actor

import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import me.micseydel.actor.DailyNoteActor.TheDayHasPassed
import me.micseydel.actor.PeriodicNotesCreatorActor.{Message, behavior}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Operator, SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.VaultKeeper
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.vault.persistence.NoteRef.{Contents, FileDoesNotExist, FileReadException}

import java.io.FileNotFoundException
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, ZonedDateTime}
import scala.util.{Failure, Success, Try}

object PeriodicNotesCreatorActor {
  sealed trait Message
  private case class ItsMidnight(day: LocalDate) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info("Starting up PeriodicNotesCreatorActor")
    setup()
  }

  private def setup()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Periodic Notes Templates", rgb(255, 222, 71), "🌞") { (context, noteRef) =>
    noteRef.getDailyNoteTemplate() match {
      case Validated.Valid(noteName: String) =>
        implicit val c: TinkerContext[_] = context
        context.actorContext.log.info("Subscribing ItsMidnight message through Operator")
        context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))

        val dailyNotesManager = context.cast(DailyNotesManager(noteName), "DailyNotesManager")

        behavior(dailyNotesManager)

      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"PeriodicNotesCreatorActor failed, invalid dailiy template: $e")
        Tinker.ignore
    }
  }

  private def behavior(dailyNotesManager: SpiritRef[DailyNotesManager.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case ItsMidnight(dayForNote) =>
        dailyNotesManager !! DailyNotesManager.ItsMidnight(dayForNote)
        Tinker.steadily
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    //noinspection AccessorLikeMethodIsEmptyParen
    def getDailyNoteTemplate(): ValidatedNel[String, String] = {
      noteRef.read().flatMap(_.yamlFrontMatter).map(_.get("daily")) match {
        case Success(Some(noteName: String)) =>
          noteName.validNel
        case Success(other) =>
          s"Expected ${noteRef.noteId} to contain a `daily` string property but found: $other".invalidNel
        case Failure(exception) =>
          Common.getStackTraceString(exception).invalidNel
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

object DailyNotesManager {
  sealed trait Message

  case class ItsMidnight(day: LocalDate) extends Message

  def apply(templateName: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(templateName, TinkerColor.random(), "🏛️") { (context, noteRef) =>
    val dailyNoteLookup: LookUpSpiritByDay[DailyNoteActor.Message] = LookUpSpiritByDay[DailyNoteActor.Message] { (context, captureDate) =>
      context.cast(DailyNoteActor(captureDate, noteRef.getTemplate()), TimeUtil.localDateTimeToISO8601Date(captureDate))
    }

    behavior(dailyNoteLookup)
  }

  private def behavior(dailyNoteLookup: LookUpSpiritByDay[DailyNoteActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    Tinker.receiveMessage {
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

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    //noinspection AccessorLikeMethodIsEmptyParen
    def getTemplate(): String = {
      // FIXME: return the validated object instead
      val validated: ValidatedNel[String, String] = noteRef.readMarkdownSafer() match {
        case Contents(s) => s.validNel
        case NoteRef.FileDoesNotExist => s"Note ${noteRef.noteId} did not exist".invalidNel
        case FileReadException(exception) => Common.getStackTraceString(exception).invalidNel
      }
      validated match {
        case Validated.Valid(a: String) => a
        case Validated.Invalid(e) => throw new RuntimeException(e.toString())
      }
    }
  }
}


object DailyNoteActor {
  sealed trait Message

  final case object TheDayHasPassed extends Message

  def apply(forDay: LocalDate, template: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(forDay.toString, rgb(255, 222, 71), "•", Some("periodic_notes/daily")) { (context, noteRef) =>
    val substitutedTemplate = template // FIXME HACKS so the Templater plugin can be used in a pinch
      .replace("<% \"---\" %>", "---")
      .replace("<% tp.date.now(\"YYYY-MM-DD\") %>", IsoDateFormatter.format(forDay))
      .replace("<% moment().format(\"dddd, D MMMM YYYY (DDD)\") %>", VerboseDayFormatter.format(forDay))
      .replace("<% tp.date.yesterday(\"YYYY-MM-DD\") %>", IsoDateFormatter.format(forDay.minusDays(1)))
      .replace("<% tp.date.tomorrow(\"YYYY-MM-DD\") %>", IsoDateFormatter.format(forDay.plusDays(1)))

    noteRef.readMarkdownSafer() match {
      case Contents(_) =>
        context.actorContext.log.info(s"NOT creating daily note for $forDay, note already exists")
        behavior(forDay)

      case FileDoesNotExist =>
        context.actorContext.log.info(s"Creating daily note for $forDay")
        noteRef.setMarkdown(substitutedTemplate)
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

  private val VerboseDayFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("EEEE, d MMMM yyyy (DDD)")
  private val IsoDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
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
