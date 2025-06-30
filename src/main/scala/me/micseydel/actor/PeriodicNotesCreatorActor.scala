package me.micseydel.actor

import scala.jdk.CollectionConverters._
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.util.TimeUtil
import me.micseydel.vault.{Note, NoteId, VaultPath}
import me.micseydel.vault.persistence.{BasicNoteRef, NoteRef}
import me.micseydel.vault.persistence.NoteRef.{Contents, FileDoesNotExist}

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{Duration, LocalDate, ZonedDateTime}
import java.util
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
}

object DailyNotesManager {
  sealed trait Message

  case class ItsMidnight(day: LocalDate) extends Message

  def apply(templateName: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(templateName, TinkerColor.random(), "🏛️") { (context, noteRef) =>
    implicit val c: TinkerContext[_] = context
    context.actorContext.log.info(s"Starting DailyNotesManager for day $templateName")
    val dailyNoteLookup: LookUpSpiritByDay[DailyNoteActor.Message] = LookUpSpiritByDay[DailyNoteActor.Message] { (context, captureDate) =>
      val actor: SpiritRef[DailyNoteActor.Message] = context.cast(DailyNoteActor(captureDate), TimeUtil.localDateTimeToISO8601Date(captureDate))
      actor !! DailyNoteActor.Create(noteRef.getTemplate())
      actor
    }

    // without this, it doesn't exist at midnight yet to receive its own midnight trigger for Today->Yesterday aliasing
    dailyNoteLookup :?> context.system.clock.now() match {
      case (warmedUpLookup, _) =>
        behavior(warmedUpLookup)
    }
  }

  private def behavior(dailyNoteLookup: LookUpSpiritByDay[DailyNoteActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    Tinker.receiveMessage {
      case ItsMidnight(dayForNote) =>
        dailyNoteLookup :?> dayForNote match {
          case (updated, _) =>
            behavior(updated)
        }
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    //noinspection AccessorLikeMethodIsEmptyParen
    def getTemplate(): ValidatedNel[String, String] = {
      noteRef.readMarkdownSafer() match {
        case Contents(Success(s)) => s.validNel
        case NoteRef.FileDoesNotExist => s"Note ${noteRef.noteId} did not exist".invalidNel
        case Contents(Failure(exception)) => Common.getStackTraceString(exception).invalidNel
      }
    }
  }
}


object DailyNoteActor {
  sealed trait Message

  final case class Create(template: ValidatedNel[String, String]) extends Message
  final case class ItsMidnight(day: LocalDate) extends Message

  def apply(forDay: LocalDate)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(forDay.toString, rgb(255, 222, 71), "•", Some("periodic_notes/daily")) { (context, noteRef) =>
    implicit val c: TinkerContext[_] = context
    context.actorContext.log.info(s"Starting DailyNoteActor for day $forDay")
    context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))
    behavior(forDay, noteRef)
  }

  private def behavior(forDay: LocalDate, noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case Create(Valid(template)) =>
        noteRef.readMarkdownSafer() match {
          case FileDoesNotExist =>
            context.actorContext.log.info(s"Creating daily note for $forDay")
            noteRef.setMarkdown(substitutedTemplate(template, forDay)) match {
              case Failure(exception) => context.actorContext.log.error("Failed to set Markdown", exception)
              case Success(_) =>
            }

          case Contents(Success(_)) =>
            context.actorContext.log.info(s"NOT creating daily note for $forDay, note already exists")

          case Contents(Failure(exception)) =>
            context.actorContext.log.error(s"Daily note creation for $forDay failed", exception)
        }

        Tinker.steadily

      case Create(Invalid(msg)) =>
        context.actorContext.log.warn(s"Failed to create daily template for day $forDay: $msg")
        Tinker.steadily

      case ItsMidnight(newDay) =>
        val daysBetween = TimeUtil.daysBetween(forDay, newDay)
        context.actorContext.log.info(s"Actor for $forDay can tell it's now $newDay (with daysBetween=$daysBetween)")

        noteRef.getNoteAndAliases().flatMap { case (note, aliases) =>
          // FIXME: this code should not discard non-alias frontmatter!
          daysBetween match {
            case 0 =>
              context.actorContext.log.warn(s"Did not expect to receive an ItsMidnight message the same day as creation ($forDay)")
              Success(note)
            case 1 =>
              val removed = aliases.remove(Today)
              if (removed) {
                aliases.add(Yesterday)
                context.actorContext.log.info(s"Replaced alias Today with Yesterday")
                noteRef.setFrontMatter(Map("aliases" -> aliases).asJava)
              } else {
                // FIXME: probably change to info
                context.actorContext.log.warn(s"Tried to remove Today from $forDay aliases, but found: $aliases")
                Success(note)
              }
            case 2 =>
              val removed = aliases.remove(Yesterday)
              if (!removed) context.actorContext.log.warn(s"Tried to remove Yesterday from $forDay aliases, but found: $aliases")
              noteRef.setFrontMatter(Map("aliases" -> aliases).asJava)
            case other =>
              context.actorContext.log.debug(s"Day $forDay ignoring midnight ping after $other days")
              Success(note)
          }
        } match {
          case Failure(exception) => context.actorContext.log.error(s"Something went wrong trying to fetch aliases from $forDay", exception)
          case Success(_) =>
        }

        Tinker.steadily
    }
  }

  private val Yesterday = "Yesterday"
  private val Today = "Today"

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getNoteAndAliases(): Try[(Note, util.List[String])] = {
      noteRef.readNote().flatMap { note =>
        note.yamlFrontMatter.flatMap { yaml =>
          yaml.get("aliases") match {
            case Some(aliases: java.util.List[String] @unchecked) => Success(note -> aliases)
            case other => Failure(new RuntimeException(s"Expected a java.util.List[String] containing aliases in an option, found $other"))
          }
        }
      }
    }
  }

  private def substitutedTemplate(template: String, forDay: LocalDate): String = {
    template // FIXME HACKS so the Templater plugin can be used in a pinch
      .replace("<% \"---\" %>", "---")
      .replace("<% tp.date.now(\"YYYY-MM-DD\") %>", IsoDateFormatter.format(forDay))
      .replace("<% moment().format(\"dddd, D MMMM YYYY (DDD)\") %>", VerboseDayFormatter.format(forDay))
      .replace("<% tp.date.yesterday(\"YYYY-MM-DD\") %>", IsoDateFormatter.format(forDay.minusDays(1)))
      .replace("<% tp.date.tomorrow(\"YYYY-MM-DD\") %>", IsoDateFormatter.format(forDay.plusDays(1)))
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
