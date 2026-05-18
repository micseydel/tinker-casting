package me.micseydel.actor.kitties

import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import com.softwaremill.quicklens.ModifyPimp
import me.micseydel.actor.kitties.Helper.RichNoteRef
import me.micseydel.actor.kitties.LitterBoxReportsActor.*
import me.micseydel.actor.kitties.LitterBoxesHelper.LitterSifted
import me.micseydel.actor.kitties.LitterCharts.{AuditCompleted, AuditNotCompleted, AuditStatus, HasInbox, LitterReportForDay, LitterSummaryForDay}
import me.micseydel.actor.kitties.MarkdownDailyLitterSummaryReportDocumentParser.{DataPoint, LineParseResult, LineParser, LitterReport, ParseFailure, ParseSuccessDatapoint}
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.model.{Defecation, LitterSiftedEvent, LitterUseType, SiftedContents, Urination}
import me.micseydel.util.ParseUtil.{batchConsecutiveComments, getLinesAfterHeader, getNoteId, getZonedDateTimeFromListLineFront}
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import org.slf4j.Logger

import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.{Failure, Success}

private[kitties] object DailyLitterSummaryActor {
  private val EmptyDocument = DailyLitterDocument(LitterReport.fresh(), Nil)

  def apply(forDay: LocalDate, color: TinkerColor, emoji: String, monthlyLitterGraphActor: SpiritRef[LitterSummaryForDay], last30DaysLitterGraphActor: SpiritRef[LitterReportForDay])(implicit Tinker: EnhancedTinker[MyCentralCast]): (String, Ability[Message]) = {
    val isoDate = TimeUtil.localDateTimeToISO8601Date(forDay)
    val noteName = s"Litter boxes sifting ($isoDate)"

    implicit val monthly: SpiritRef[LitterSummaryForDay] = monthlyLitterGraphActor
    implicit val recent: SpiritRef[LitterReportForDay] = last30DaysLitterGraphActor
    implicit val parser: MarkdownDailyLitterSummaryReportDocumentParser = new MarkdownDailyLitterSummaryReportDocumentParser(forDay)

    noteName -> AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](noteName, color, emoji, ReceiveNotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[?] = context
      implicit val nr: NoteRef = noteRef

      val audited = noteRef.getDocument() match {
        case Validated.Valid((markdownFromDisk, document: DailyLitterDocument)) =>
          val summary: LitterSummaryForDay = document.toSummary(forDay)
          context.actorContext.log.info(s"[$forDay] freshing monthly and last 30 days charts with summary $summary")
          monthlyLitterGraphActor !! summary
          last30DaysLitterGraphActor !! LitterReportForDay(forDay, document.report)
          summary.auditStatus == AuditCompleted
        case Invalid(Left(msg)) =>
          context.actorContext.log.warn(s"Something went wrong fetching the structured document from disk: $msg")
          false
        case Invalid(Right(NoteRef.FileDoesNotExist)) =>
          monthlyLitterGraphActor !! EmptyDocument.toSummary(forDay)
          last30DaysLitterGraphActor !! LitterReportForDay(forDay, EmptyDocument.report)
          false
      }

      behavior(audited)
    }
  }

  def behavior(cachedAuditStatus: Boolean)(implicit Tinker: EnhancedTinker[MyCentralCast], monthlyLitterGraphActor: SpiritRef[LitterSummaryForDay], last30DaysLitterGraphActor: SpiritRef[LitterReportForDay], noteRef: NoteRef, parser: MarkdownDailyLitterSummaryReportDocumentParser): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    implicit val log: Logger = context.actorContext.log

    Tinker.receiveMessage { message: Message =>
      val maybeUpdatedDocument: Option[DailyLitterDocument] = message match {
        case ReceiveNotePing(NoOp) =>
          val validatedDocument: Validated[Either[String, NoteRef.FileDoesNotExist.type], (String, DailyLitterDocument)] =
            noteRef.getDocument()

          validatedDocument
            .map(_._2)
            .map(Some(_))
            .getOrElse(None) match {
            case Some(document) =>
              val latestDocIsAudited = document.report.auditStatus == AuditCompleted
              if (latestDocIsAudited && cachedAuditStatus != latestDocIsAudited) {
                // (if this is newly audited, normalize the inbox)
                Some(document.normalizeInbox(parser.day))
              } else {
                None
              }

            case _ => None
          }

        case observation: EventCapture =>
          noteRef.addEventCapture(observation) match {
            case Validated.Valid(document: DailyLitterDocument) => Some(document)
            case Invalid(e) =>
              context.actorContext.log.warn(s"Something(s) went wrong: ${e}")
              None
          }
      }

      maybeUpdatedDocument match {
        case None =>
          context.actorContext.log.warn(s"(CANARY) no need to update ${noteRef.noteId}")
          Tinker.steadily
        case Some(document) =>
          noteRef.setMarkdown(document.toMarkdown) match {
            case Failure(exception) => context.actorContext.log.warn(s"Failed to refresh markdown!", exception)
            case Success(NoOp) => context.actorContext.log.warn(s"(CANARY) refreshed markdown for ${noteRef.noteId}")
          }
          val summaryForDay = document.toSummary(parser.day)
          monthlyLitterGraphActor !! summaryForDay
          last30DaysLitterGraphActor !! LitterReportForDay(parser.day, document.report)
          behavior(document.report.auditStatus == AuditCompleted)
      }
    }
  }
}

//

private object Helper {
  implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def addEventCapture(eventCapture: EventCapture)(implicit log: Logger, parser: MarkdownDailyLitterSummaryReportDocumentParser): ValidatedNel[String, DailyLitterDocument] = {
      eventCapture match {
        case obs@LitterSiftedObservation(_) => addObservation(obs)
        case ati@AddToInbox(_, _) => addToInbox(ati)
      }
    }

    private def addObservation(observation: LitterSiftedObservation)(implicit log: Logger, parser: MarkdownDailyLitterSummaryReportDocumentParser): ValidatedNel[String, DailyLitterDocument] = {
      val datapoint = observation.capture match {
        case LitterSifted(LitterSiftedEvent(when, _, contents), ref, maybeRaw) =>
          DataPoint(when, contents, ref, maybeRaw.toList.map(c => s"    - $c"))
      }
      getDocument() match {
        case v@Validated.Valid((existingMarkdown, document: DailyLitterDocument)) =>
          val updatedDocument = document.append(datapoint)
          val updatedMarkdown = updatedDocument.toMarkdown
          if (updatedMarkdown != existingMarkdown) {
            noteRef.setMarkdown(updatedMarkdown) match {
              case Failure(exception) => Common.getStackTraceString(exception).invalidNel
              case Success(NoOp) =>
                updatedDocument.validNel
            }
          } else {
            document.validNel // should be same as updatedDocument
          }
        case iv@Validated.Invalid(e) =>
          e match {
            case Left(msg) =>
              log.warn(s"Failed to generate the markdown report because: $msg")
              msg.invalidNel

            case Right(NoteRef.FileDoesNotExist) =>
              val document = DailyLitterDocument(LitterReport.fresh(List(datapoint)), Nil)
              noteRef.setMarkdown(document.toMarkdown) match {
                case Failure(exception) => Common.getStackTraceString(exception).invalidNel
                case Success(NoOp) =>
                  log.debug("HACK seems like the first file of the day, creating")
                  Validated.Valid(document)
              }
          }
      }
    }

    private def addToInbox(toAdd: AddToInbox)(implicit log: Logger, parser: MarkdownDailyLitterSummaryReportDocumentParser): ValidatedNel[String, DailyLitterDocument] = {
      getDocument() match {
        case Validated.Valid((existingMarkdown, document: DailyLitterDocument)) =>
          val updatedDocument = document.appendToInbox(toAdd.string)

          val updatedMarkdown = updatedDocument.toMarkdown
          if (updatedMarkdown != existingMarkdown) {
            noteRef.setMarkdown(updatedDocument.toMarkdown) match {
              case Failure(exception) => Common.getStackTraceString(exception).invalidNel
              case Success(NoOp) =>
                Validated.Valid(updatedDocument)
            }
          } else {
            updatedDocument.valid
          }


        case iv@Validated.Invalid(e) =>
          e match {
            case Left(msg) =>
              log.warn(s"Failed to generate the markdown report because: $msg")
              msg.invalidNel

            case Right(NoteRef.FileDoesNotExist) =>
              val document = DailyLitterDocument(LitterReport.fresh(), List(toAdd.string))
              noteRef.setMarkdown(document.toMarkdown) match {
                case Failure(exception) => Common.getStackTraceString(exception).invalidNel
                case Success(NoOp) =>
                  Validated.Valid(document)
              }
          }
      }
    }

    def getDocument()(implicit parser: MarkdownDailyLitterSummaryReportDocumentParser): Validated[Either[String, NoteRef.FileDoesNotExist.type], (String, DailyLitterDocument)] = {
      noteRef.readMarkdownSafer() match {
        case NoteRef.Contents(Success(markdown)) =>
          parser(markdown)
            .map(d => (markdown, d))
            .leftMap(Left(_))
        case NoteRef.Contents(Failure(exception)) => Left(Common.getStackTraceString(exception)).invalid
        case NoteRef.FileDoesNotExist => Right(NoteRef.FileDoesNotExist).invalid
      }
    }
  }
}


// data model


case class DailyLitterDocument(report: LitterReport, inbox: List[String]) {
  def toMarkdown: String = this match {
    case DailyLitterDocument(LitterReport(Nil, _), Nil) =>
      ""
    case DailyLitterDocument(report: LitterReport, Nil) =>
      report.toMarkdown
    case DailyLitterDocument(LitterReport(Nil, _), _) =>
      "# Summary\n\n- Total pee: 0\n- Total poo: 0\n- [ ] Audited\n" + inboxMd
    case DailyLitterDocument(report: LitterReport, _) =>
      s"""${report.markdownSummary}
         |
         |$inboxMd
         |
         |${report.events}
         |""".stripMargin
  }

  def append(dataPoint: DataPoint): DailyLitterDocument =
    this.copy(report = report.append(dataPoint)) // updates the audit status internally

  def appendToInbox(string: String): DailyLitterDocument = {
    if (inbox.contains(string)) {
      this
    } else {
      this.copy(
        report = report.copy(auditStatus = HasInbox),
        inbox = string :: inbox
      )
    }
  }

  def normalizeInbox(forDay: LocalDate)(implicit log: Logger): DailyLitterDocument = {
    val (leftoverInbox, inboxCorrections) = inbox.map(LineParser.apply(_, forDay)).partitionMap {
      case MarkdownDailyLitterSummaryReportDocumentParser.ParseSuccessDatapoint(datapoint) =>
        Right(datapoint)

      case MarkdownDailyLitterSummaryReportDocumentParser.ParseFailure(rawLine, reason, comments) =>
        log.warn(s"!! parse failure: $reason ($comments)")
        Left(rawLine)
    }

    if (inboxCorrections.nonEmpty) {
      val partlyCorrected = inboxCorrections.foldRight(this) { (correction, documentSoFar) =>
        documentSoFar.append(correction)
      }
      partlyCorrected
        .modify(_.inbox).setTo(leftoverInbox)
        .modify(_.report.auditStatus).setTo(AuditCompleted)
    } else {
      this
    }
  }

  private def inboxMd: String = (
    "# Inbox" :: "" ::
      inbox.map("- " + _).reverse // FIXME: remove the reverse?
    ).mkString("\n")

  def toSummary(forDay: LocalDate): LitterSummaryForDay = {
    report.toSummary(forDay)
  }
}


// markdown (pure)


class MarkdownDailyLitterSummaryReportDocumentParser(val day: LocalDate) {
  def apply(markdown: String): Validated[String, DailyLitterDocument] = {
    val inboxLines = markdown.split("\n").toList.dropWhile(_ != "# Inbox").drop(1).takeWhile(!_.startsWith("#")).filter(_.nonEmpty).map(_.drop(2))

    val auditedButton = markdown.contains("- [x] Audited")

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
      s"Not all lines were successfully parsed $failures".invalid
    } else {
      val auditStatus = if (auditedButton) {
        AuditCompleted
      } else if (inboxLines.nonEmpty) {
        HasInbox
      } else {
        AuditNotCompleted
      }

      val report = LitterReport(successes.sortBy(_.zonedDateTime), auditStatus)
      DailyLitterDocument(report, inboxLines.reverse).valid
    }
  }





}

object MarkdownDailyLitterSummaryReportDocumentParser {
  object LitterReport {
    def fresh(datapoints: List[DataPoint] = Nil): LitterReport = LitterReport(datapoints, AuditNotCompleted)
  }

  object LineParser {
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
          case '1' :: theRest =>
            helper(theRest)(Urination :: accumulatedUses, accumulatedRejections)

          case '2' :: theRest =>
            helper(theRest)(Defecation :: accumulatedUses, accumulatedRejections)

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

  sealed trait LineParseResult

  case class ParseSuccessDatapoint(datapoint: DataPoint) extends LineParseResult

  case class ParseFailure(rawLine: String, reason: NonEmptyList[String], comments: List[String]) extends LineParseResult

  // yaml, summary bullets (poo/pee count), event bullets (time, type, ref)
  // - \[02:58:14AM\] 💦 ([[Transcription for mobile_audio_capture_20240217-025814.wav|ref]])
  case class DataPoint(zonedDateTime: ZonedDateTime, siftedContents: SiftedContents, noteId: NoteId, comments: List[String] = Nil)

  case class LitterReport(datapoints: List[DataPoint], auditStatus: AuditStatus) {
    def nonEmpty: Boolean = datapoints.nonEmpty

    def toSummary(forDay: LocalDate): LitterSummaryForDay = {
      val (pee, poop) = aggregates
      LitterSummaryForDay(forDay, pee, poop, auditStatus)
    }

    def toMarkdown: String = {
      s"""$markdownSummary
         |
         |$events
         |""".stripMargin
    }

    private[kitties] def markdownSummary: String = {
      def total(litterUseType: LitterUseType): Int = distinctDatapoints.map(_.siftedContents.multiset.getOrElse(litterUseType, 0)).sum

      val (totalPee, totalPoo) = aggregates
      val auditedChar = if (auditStatus == AuditCompleted) 'x' else ' '
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

    def append(datapoint: DataPoint): LitterReport = {
      this.copy(
        datapoints = (datapoint :: datapoints).distinct.sortBy(_.zonedDateTime),
        auditStatus = if (auditStatus == AuditCompleted) {
          AuditNotCompleted
        } else {
          auditStatus
        }
      )
    }

    def aggregates: (Int, Int) = {
      def total(litterUseType: LitterUseType): Int = distinctDatapoints.map(_.siftedContents.multiset.getOrElse(litterUseType, 0)).sum

      val totalPee = total(Urination)
      val totalPoo = total(Defecation)

      (totalPee, totalPoo)
    }
  }
}