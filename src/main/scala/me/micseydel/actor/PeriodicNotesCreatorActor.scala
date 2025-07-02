package me.micseydel.actor

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.vault.persistence.NoteRef.{Contents, FileDoesNotExist}
import me.micseydel.{Common, NoOp}

import java.io.FileNotFoundException
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, ZonedDateTime}
import scala.jdk.CollectionConverters._
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
        context.actorContext.log.warn(s"PeriodicNotesCreatorActor failed, invalid daily template: $e")
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

      noteRef.getTemplate() match {
        case Valid(template) =>
          actor !! DailyNoteActor.ReceiveTemplate(template)
        case Invalid(e) =>
          context.actorContext.log.warn(s"Something went wrong getting the template from disk for creating $captureDate: $e")
      }

      actor
    }

    val today = context.system.clock.now()
    dailyNoteLookup :?> today match {
      case (dailyNoteLookupWithToday, _) =>
        dailyNoteLookupWithToday :?> today.minusDays(1) match {
          case (dailyNoteLookupWithYesterday, _) =>
            dailyNoteLookupWithYesterday :?> today.minusDays(2) match {
              case (warmedUpLookup, _) =>
                context.actorContext.log.info(s"Cache warmed up with today and prior two days: $warmedUpLookup")
                behavior(warmedUpLookup)
            }
        }
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

  final case class ReceiveTemplate(template: String) extends Message
  private case class NewDay(day: LocalDate) extends Message

  def apply(forDay: LocalDate)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(forDay.toString, rgb(255, 222, 71), "•", Some("periodic_notes/daily")) { (context, noteRef) =>
    implicit val c: TinkerContext[_] = context
    context.actorContext.log.info(s"Starting DailyNoteActor for day $forDay")
    context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(NewDay))
    noteRef.tidyYamlAliases(forDay)(context.system.clock) match {
      case Failure(exception) => context.actorContext.log.warn(s"Something went wrong tidying yaml for a prior day on actor (app?) startup", exception)
      case Success(_) =>
    }

    behavior(forDay, noteRef)
  }

  private def behavior(forDay: LocalDate, noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveTemplate(template) =>
        noteRef.readMarkdownSafer() match {
          case FileDoesNotExist =>
            context.actorContext.log.info(s"Creating daily note for $forDay")
            noteRef.createNote(template, forDay) match {
              case Failure(exception) => context.actorContext.log.error("Failed to set Markdown", exception)
              case Success(_) =>
            }

            if (context.system.clock.today().isAfter(forDay)) {
              context.actorContext.log.warn(s"Tidying aliases for $forDay, it seems this note was created late?")
              noteRef.tidyYamlAliases(forDay)(context.system.clock)
            }

          case Contents(Success(_)) =>
            context.actorContext.log.info(s"NOT creating daily note for $forDay, note already exists")

          case Contents(Failure(exception)) =>
            context.actorContext.log.error(s"Daily note creation for $forDay failed", exception)
        }

        Tinker.steadily

      case NewDay(currentDay) =>
        context.actorContext.log.info(s"Tidying yaml for $forDay (on $currentDay)")
        noteRef.tidyYamlAliases(currentDay)(context.system.clock) match {
          case Failure(exception) => context.actorContext.log.warn(s"Tidying aliases on $currentDay for $forDay failed", exception)
          case Success(_) =>
        }

        Tinker.steadily
    }
  }

  private val Yesterday = "Yesterday"
  private val Today = "Today"

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def createNote(template: String, forDay: LocalDate): Try[NoOp.type] =
      noteRef.setMarkdown(substitutedTemplate(template, forDay))

    def tidyYamlAliases(forDay: LocalDate)(implicit clock: TinkerClock): Try[NoOp.type] = {
      noteRef.readNote().flatMap {
        case note@Note(markdown, _) =>
          note.yamlFrontMatter.flatMap { frontMatter =>
            val updated: java.util.Map[String, Object] = new java.util.HashMap[String, Object]()

            val doUpdate = frontMatter.map {
              case ("aliases", aliases: java.util.List[String] @unchecked) =>
                if (tidyAliasesList(aliases, forDay)) {
                  updated.put("aliases", aliases.asInstanceOf[Object])
                  true
                } else false
              case (key, value) =>
                updated.put(key, value.asInstanceOf[Object])
                false
            }.exists(identity)

            if (doUpdate) {
              noteRef
                .setTo(Note(markdown, updated.asScala.toMap))
                .map(_ => NoOp)
            } else {
              Success(NoOp)
            }
          }
      }.recoverWith {
        case _: FileNotFoundException => Success(NoOp)
      }
    }

    private def tidyAliasesList(aliasesToTidy: java.util.List[String], forDay: LocalDate)(implicit clock: TinkerClock): Boolean = {
      val daysBetween = TimeUtil.daysBetween(forDay, clock.today())
      daysBetween match {
        case 1 => // [[Yesterday]]
          val removed = aliasesToTidy.remove(Today)
          if (removed) {
            // if we removed "Today" as an alias, create "Yesterday" alias
            aliasesToTidy.add(Yesterday)
          }
          removed
        case 2 => // day before yesterday
          // just make sure the alias is gone
          aliasesToTidy.remove(Yesterday)
        case _ =>
          false
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
}
