package me.micseydel.actor.kitties

import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.DailyNotesRouter
import me.micseydel.actor.kitties.LitterBoxReportActor.{AddToInbox, EventCapture, LitterSiftedObservation}
import me.micseydel.actor.kitties.LitterBoxesHelper.LitterSifted
import me.micseydel.actor.kitties.LitterCharts.{AuditCompleted, AuditNotCompleted, HasInbox, LitterSummaryForDay}
import me.micseydel.actor.kitties.MarkdownWithoutJsonExperiment.{DataPoint, Report}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.model._
import me.micseydel.util.ParseUtil.{batchConsecutiveComments, getLinesAfterHeader, getNoteId, getZonedDateTimeFromListLineFront}
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import org.slf4j.Logger

import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.{Failure, Success}


object LitterBoxReportActor {
  sealed trait Message

  sealed trait EventCapture extends Message {
    def when: ZonedDateTime
  }

  case class LitterSiftedObservation(capture: LitterBoxesHelper.LitterSifted) extends EventCapture {
    def when: ZonedDateTime = capture.event.when
  }

  case class AddToInbox(string: String, when: ZonedDateTime) extends EventCapture

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] =
    setup()

  private def setup()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor.CatBrown, "🗑️").setup { context =>
    implicit val c: TinkerContext[_] = context

    val monthlyLitterGraphActor: SpiritRef[LitterSummaryForDay] = context.cast(MonthlyLitterGraphActor(), "MonthlyLitterGraphActor")
    val last30DaysLitterGraphActor: SpiritRef[LitterSummaryForDay] = context.cast(Last30DaysLitterGraphActor(), "Last30DaysLitterGraphActor")

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[EventCapture]] = context.cast(DailyNotesRouter(DailyAbility(_, _, _, monthlyLitterGraphActor, last30DaysLitterGraphActor), 30), "DailyNotesRouter")

    Tinker.receiveMessage {
      case ec@LitterSiftedObservation(_) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(ec, ec.when)
        Tinker.steadily

      case ati@AddToInbox(_, when) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(ati, when)
        Tinker.steadily
    }
  }
}

private[kitties] object DailyAbility {
  def apply(forDay: LocalDate, color: TinkerColor, emoji: String, monthlyLitterGraphActor: SpiritRef[LitterSummaryForDay], last30DaysLitterGraphActor: SpiritRef[LitterSummaryForDay])(implicit Tinker: Tinker): (String, Ability[EventCapture]) = {
    val isoDate = TimeUtil.localDateTimeToISO8601Date(forDay)
    val noteName = s"Litter boxes sifting ($isoDate)"

    noteName -> NoteMakingTinkerer[EventCapture](noteName, color, emoji) { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context

      noteRef.getDocument(forDay) match {
        case Invalid(e) =>
          e.toList match {
            case List(justOne) if justOne.contains("No such file or directory") =>
              // refactor, but this is normal and expected
            case other =>
              context.actorContext.log.warn(s"Something unexpected happened: $other")
          }
        case Validated.Valid(document: Document) =>
          val summary = documentToSummary(document, forDay)
          context.actorContext.log.info(s"$forDay updating the note and listeners with summary $summary")
          monthlyLitterGraphActor !! summary
          last30DaysLitterGraphActor !! summary
          noteRef.setMarkdown(document.toMarkdown) match {
            case Failure(exception) => context.actorContext.log.warn(s"Something went wrong $forDay", exception)
            case Success(NoOp) =>
          }
      }

      Tinker.receiveMessage { observation =>
        val validatedUpdatedDocument: ValidatedNel[String, Document] = noteRef.addEventCapture(observation)(context.actorContext.log)

        validatedUpdatedDocument match {
          case Validated.Valid(document: Document) =>
            val summaryForDay = documentToSummary(document, forDay)
            monthlyLitterGraphActor !! summaryForDay
            last30DaysLitterGraphActor !! summaryForDay

          case Invalid(e) =>
            context.actorContext.log.warn(s"Something(s) went wrong: ${e}")
        }

        Tinker.steadily
      }
    }
  }

  private def documentToSummary(document: Document, forDay: LocalDate): LitterSummaryForDay = {
    val (pee, poop) = document.toSummary

    val auditStatus: LitterCharts.AuditStatus = if (document.inbox.nonEmpty) {
      HasInbox
    } else {
      if (document.report.audited) {
        AuditCompleted
      } else {
        AuditNotCompleted
      }
    }

    LitterSummaryForDay(forDay, pee, poop, auditStatus)
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def addEventCapture(eventCapture: EventCapture)(implicit log: Logger): ValidatedNel[String, Document] = {
      eventCapture match {
        case obs@LitterSiftedObservation(_) => addObservation(obs)
        case ati@AddToInbox(_, _) => addToInbox(ati)
      }
    }

    private def addObservation(observation: LitterSiftedObservation)(implicit log: Logger): ValidatedNel[String, Document] = {
      val datapoint = observation.capture match {
        case LitterSifted(LitterSiftedEvent(when, _, contents), ref, maybeRaw) =>
          DataPoint(when, contents, ref, maybeRaw.toList.map(c => s"    - $c"))
      }
      getDocument(observation.capture.when.toLocalDate) match {
        case v@Validated.Valid((document: Document)) =>
          val updatedDocument = document.append(datapoint)
          val updatedMarkdown = updatedDocument.toMarkdown
          noteRef.setMarkdown(updatedMarkdown) match {
            case Failure(exception) => Common.getStackTraceString(exception).invalidNel
            case Success(NoOp) =>
              document.validNel
          }
        case iv@Validated.Invalid(e) =>
          e.toList match {
            case List(justone) if justone.contains("FileNotFoundException") =>
              val document = Document(Report.fresh(List(datapoint)), Nil)
              noteRef.setMarkdown(document.toMarkdown) match {
                case Failure(exception) => Common.getStackTraceString(exception).invalidNel
                case Success(NoOp) =>
                  log.debug("HACK seems like the first file of the day, creating")
                  Validated.Valid(document)
              }

            case _ =>
              log.warn(s"Failed to generate the markdown report because: $e")
              iv
          }
      }
    }

    private def addToInbox(toAdd: AddToInbox)(implicit log: Logger): ValidatedNel[String, Document] = {
      getDocument(toAdd.when.toLocalDate) match {
        case Validated.Valid((document: Document)) =>
          val updatedDocument = document.appendToInbox(toAdd.string)
          noteRef.setMarkdown(updatedDocument.toMarkdown) match {
            case Failure(exception) => Common.getStackTraceString(exception).invalidNel
            case Success(NoOp) =>
              Validated.Valid(updatedDocument)
          }

        case iv@Validated.Invalid(e) =>
          if (e.exists(_.contains("FileNotFoundException"))) {
            val document = Document(Report.fresh(), List(toAdd.string))
            noteRef.setMarkdown(document.toMarkdown) match {
              case Failure(exception) => Common.getStackTraceString(exception).invalidNel
              case Success(NoOp) =>
                Validated.Valid(document)
            }
          } else {
            log.warn(s"Failed to generate the markdown report because: $e")
            iv
          }
      }
    }

    def getDocument(forDay: LocalDate): ValidatedNel[String, Document] = {
      noteRef.readMarkdown() match {
        case Success(markdown) =>
          MarkdownWithoutJsonExperiment(markdown, forDay)

        case Failure(exception) =>
          Invalid(Common.getStackTraceString(exception)).toValidatedNel
      }
    }
  }
}

case class Document(report: Report, inbox: List[String]) {
  def toMarkdown: String = this match {
    case Document(Report(Nil, _), Nil) =>
      ""
    case Document(report: Report, Nil) =>
      report.toMarkdown
    case Document(Report(Nil, _), _) =>
      inboxMd
    case Document(report: Report, _) =>
      s"""${report.markdownSummary}
         |
         |$inboxMd
         |
         |${report.events}
         |""".stripMargin
  }

  def append(dataPoint: DataPoint): Document = this.copy(report = report.append(dataPoint))

  def appendToInbox(string: String): Document = this.copy(inbox = string :: inbox)

  private def inboxMd: String = ("# Inbox" :: "" :: inbox.map("- " + _).reverse).mkString("\n")

  def toSummary: (Int, Int) = {
    report.datapoints.map(_.siftedContents.multiset).foldRight((0, 0)) { case (toAdd, (peeSoFar, pooSoFar)) =>
      (
        peeSoFar + toAdd.getOrElse(Urination, 0),
        pooSoFar + toAdd.getOrElse(Defecation, 0)
      )
    }
  }
}

object MarkdownWithoutJsonExperiment {
  def apply(markdown: String, day: LocalDate): ValidatedNel[String, Document] = {
    val inboxLines = markdown.split("\n").toList.dropWhile(_ != "# Inbox").drop(1).takeWhile(!_.startsWith("#")).filter(_.nonEmpty).map(_.drop(2))

    val audited = markdown.contains("- [x] Audited")

    val linesToParse: List[String] = getLinesAfterHeader(markdown, "Events")

    @tailrec
    def parseLines(linesToParse: List[String], accumulator: List[LineParseResult]): List[LineParseResult] = {
      linesToParse match {
        case Nil =>
          accumulator.reverse

        case line :: theRest =>
          val (comments, remaining) = batchConsecutiveComments(theRest)
          LineParser.apply(line, day) match {
            case ParseSuccessDatapoint(datapoint) =>
              parseLines(remaining, ParseSuccessDatapoint(datapoint.copy(comments = comments)) :: accumulator)
            case pf@ParseFailure(_, _, existingComments) =>
              if (existingComments.nonEmpty) {
                throw new RuntimeException("Comment should have been empty but found: comments")
              }

              parseLines(remaining, pf.copy(comments = comments) :: accumulator)
          }
      }
    }

    val lineParseResults: List[LineParseResult] = parseLines(linesToParse, Nil)

    val (failures, successes) = lineParseResults.partitionMap {
      case ParseFailure(rawLine, reasons, comments) =>
        val joinedReasons = reasons.toList.mkString(",")
        val msg = s"""Found ${reasons.size} failures: $joinedReasons; for line with comments:\n- $rawLine\n${comments.mkString("\n")}"""
        Left(msg)
      case ParseSuccessDatapoint(datapoint) =>
        Right(datapoint)
    }

    if (failures.nonEmpty) {
      Validated.Invalid(NonEmptyList("Not all lines were successfully parsed", failures))
    } else {
      val report = Report(successes.sortBy(_.zonedDateTime), audited)
      Validated.Valid(Document(report, inboxLines.reverse))
    }
  }


  private sealed trait LineParseResult

  private case class ParseSuccessDatapoint(datapoint: DataPoint) extends LineParseResult

  private case class ParseFailure(rawLine: String, reason: NonEmptyList[String], comments: List[String]) extends LineParseResult

  // yaml, summary bullets (poo/pee count), event bullets (time, type, ref)
  // - \[02:58:14AM\] 💦 ([[Transcription for mobile_audio_capture_20240217-025814.wav|ref]])
  case class DataPoint(zonedDateTime: ZonedDateTime, siftedContents: SiftedContents, noteId: NoteId, comments: List[String] = Nil)

  case class Report(datapoints: List[DataPoint], audited: Boolean) {
    def nonEmpty: Boolean = datapoints.nonEmpty

    def toMarkdown: String = {
      s"""$markdownSummary
         |
         |$events
         |""".stripMargin
    }

    private[kitties] def markdownSummary: String = {
      def total(litterUseType: LitterUseType): Int = distinctDatapoints.map(_.siftedContents.multiset.getOrElse(litterUseType, 0)).sum

      val (totalPee, totalPoo) = summary
      val auditedChar = if (audited) 'x' else ' '
      s"""# Summary
         |
         |- Total pee: $totalPee
         |- Total poo: $totalPoo
         |- [$auditedChar] Audited""".stripMargin
    }

    private[kitties] def events: String = {
      val eventsList: String = distinctDatapoints.distinct.map {
        case DataPoint(zonedDateTime, siftedContents, noteId, maybeComments) =>
          MarkdownUtil.listLineWithTimestampAndRef(zonedDateTime, siftedContents.toEmojis, noteId) +
            Some(maybeComments).filter(_.nonEmpty).map(_.mkString("\n", "\n", "")).getOrElse("")
      }.mkString("\n")
      s"""# Events
         |
         |$eventsList""".stripMargin
    }

    private def distinctDatapoints: List[DataPoint] = datapoints.distinct

    def append(datapoint: DataPoint): Report = {
      this.copy(datapoints = (datapoint :: datapoints).distinct.sortBy(_.zonedDateTime))
    }

    def summary: (Int, Int) = {
      def total(litterUseType: LitterUseType): Int = distinctDatapoints.map(_.siftedContents.multiset.getOrElse(litterUseType, 0)).sum

      val totalPee = total(Urination)
      val totalPoo = total(Defecation)

      (totalPee, totalPoo)
    }
  }

  object Report {
    def fresh(datapoints: List[DataPoint] = Nil): Report = Report(datapoints, audited = false)
  }

  private object LineParser {
    // FIXME: if I break the formatting, then received messages can't be encoded in markdown...
    // - without risk of duplication
    // - or loss
    // FIXME it's ok to have compromises for this report because, unlike the general case,
    // I know that updates only happen from voice events, and I'm not worried about updating the report
    // while a voice event is pending (though something like a power outage could make this an issue)
    def apply(line: String, day: LocalDate): LineParseResult = {
      // e.g.
      // List(-, \[07:29:22PM\], , ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]))
      val validated = getZonedDateTimeFromListLineFront(line.split(' ').toList, day).andThen {
        case (entryTime, theRest) =>
          getSiftedContents(theRest).andThen {
            // partsByWhitespace.splitAt(partsByWhitespace.length - 3) -> (sC, tR)
            case (siftedContents, theRemainder) =>
              getNoteId(theRemainder).map { noteId =>
                DataPoint(entryTime, siftedContents, noteId)
              }
          }
      }

      validated match {
        case Validated.Valid(datapoint: DataPoint) =>
          ParseSuccessDatapoint(datapoint)
        case Validated.Invalid(reasons: NonEmptyList[String]) =>
          ParseFailure(line, reasons, Nil)
      }
    }


    private val LitterUseTypeMap: Map[String, LitterUseType] = Map[String, LitterUseType](
      "💩" -> Defecation,
      "💦" -> Urination
    )

    private def getSiftedContents(partsByWhitespace: List[String]): ValidatedNel[String, (SiftedContents, List[String])] = {
      val (shouldBeEmojis, remainderForNoteId) = partsByWhitespace.splitAt(partsByWhitespace.length - 3)

      def extractRelevantEmoji(high: Char, low: Char): Option[LitterUseType] = {
        LitterUseTypeMap.get(new String(Array(high, low)))
      }

      @tailrec
      def helper(remaining: List[Char])(accumulatedUses: List[LitterUseType], accumulatedRejections: List[String]): (List[LitterUseType], List[String]) = {
        remaining match {
          case firstCodePoint :: secondCodePoint :: theRest =>
            extractRelevantEmoji(firstCodePoint, secondCodePoint) match {
              case Some(emoji) =>
                helper(theRest)(emoji :: accumulatedUses, accumulatedRejections)
              case None =>
                helper(theRest)(accumulatedUses, firstCodePoint.toString :: accumulatedRejections)
            }

          case Nil =>
            (accumulatedUses, accumulatedRejections)

          case justOne :: Nil =>
            (accumulatedUses, justOne.toString :: accumulatedRejections)
        }
      }

      val (emojis, rejected) = helper(shouldBeEmojis.flatten)(Nil, Nil)

      if (emojis.nonEmpty) {
        Validated.Valid((SiftedContents(emojis), remainderForNoteId))
      } else {
        val msg = s"Expected characters (emojis!) in ${LitterUseTypeMap.keys.toSet} but got ${rejected.toSet} ($shouldBeEmojis)"
        Validated.Invalid(NonEmptyList.of(msg))
      }
    }
  }
}
