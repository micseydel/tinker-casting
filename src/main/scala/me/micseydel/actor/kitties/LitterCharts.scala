package me.micseydel.actor.kitties

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.NoOp
import me.micseydel.actor.MonthlyNotesRouter
import me.micseydel.actor.MonthlyNotesRouter.Month
import me.micseydel.actor.kitties.LitterCharts.{AuditCompleted, LitterReportForDay, LitterSummaryForDay}
import me.micseydel.actor.kitties.MarkdownDailyLitterSummaryReportDocumentParser.{DataPoint, LitterReport}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.*
import me.micseydel.model.LitterSiftedEventJsonProtocol.SiftedContentsFormat
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.{DoubleSeries, IntSeries, Series, averageOfLastN}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger
import spray.json.*

import java.io.FileNotFoundException
import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object LitterCharts {
  sealed trait AuditStatus

  case object AuditCompleted extends AuditStatus

  case object AuditNotCompleted extends AuditStatus

  case object HasInbox extends AuditStatus

  case class LitterSummaryForDay(forDay: LocalDate, peeClumps: Int, poops: Int, auditStatus: AuditStatus)

  case class LitterReportForDay(forDay: LocalDate, report: LitterReport) {
    def toSummary: LitterSummaryForDay = report.toSummary(forDay)
  }

  implicit object AuditStatusFormat extends JsonFormat[AuditStatus] {
    def write(obj: AuditStatus): JsValue = JsString(obj.toString)

    def read(json: JsValue): AuditStatus = json match {
      case JsString("AuditCompleted") => AuditCompleted
      case JsString("AuditNotCompleted") => AuditNotCompleted
      case JsString("HasInbox") => HasInbox
      case other => deserializationError(s"Expected a AuditStatus: {AuditCompleted, AuditNotCompleted, HasInbox} but got $other")
    }
  }
}

private object LitterGraphHelper {
  case class Document(summaries: Map[LocalDate, LitterSummaryForDay]) {
    /**
     * flag for whether the document was updated, and latest document
     */
    def integrate(incomingSummary: LitterSummaryForDay): (Boolean, Document) = {
      summaries.get(incomingSummary.forDay) match {
        case Some(existing) if existing == incomingSummary =>
          false -> this
        case _ =>
          true -> Document(
            summaries.updated(incomingSummary.forDay, incomingSummary)
          )
      }
    }

    def toMarkdown(limitDays: Int = 30)(implicit l: Logger): String = {
      val sorted = summaries.toList
        .sortBy(_._1)
      val xaxis: List[LocalDate] = sorted.map(_._1)
      val yaxis: List[LitterSummaryForDay] = sorted.map(_._2)

      val needsAudit = yaxis
        .filter(_.auditStatus != AuditCompleted)
        .map(s => s.forDay -> s.auditStatus)

      val labels: List[String] = xaxis.takeRight(limitDays).zipWithIndex.map { case (day, i) => if (i % 2 == 0) day.toString else "" }

      val number1s: List[Int] = yaxis.map(_.peeClumps)
      val number2s: List[Int] = yaxis.map(_.poops)

      val series: List[Series[?]] = averageOfLastN(number1s).andThen(ones => averageOfLastN(number2s).map(ones -> _)) match {
        case Validated.Valid((averageNumber1s: List[Double], averageNumber2s: List[Double])) =>
          val truncated1s = number1s.takeRight(limitDays)
          List(
            IntSeries("#1", truncated1s),
            IntSeries("#2", number2s.takeRight(limitDays)),
            DoubleSeries("avg #1", averageNumber1s),
            DoubleSeries("avg #2", averageNumber2s)
          )
        case Validated.Invalid(e) =>
          l.warn(s"Failed to generate running average, ignoring ($e)")
          List(
            IntSeries("#1", number1s.takeRight(limitDays)),
            IntSeries("#2", number2s.takeRight(limitDays)),
          )
      }

      val chart = ObsidianCharts.chart(labels, series)

      val auditSection = {
        if (needsAudit.nonEmpty) {
          Some(
            "Needs auditing" -> needsAudit.map { case (forDay, auditType) =>
              s"- [[Litter boxes sifting ($forDay)]]: $auditType"
            }.reverse.mkString("", "\n", "\n")
          )
        } else {
          None
        }
      }

      val textLines = sorted.reverse.map {
        case (date, LitterSummaryForDay(_, peeClumps, poops, _)) =>
          s"- [[Litter boxes sifting ($date)|$date]] $peeClumps💦 $poops💩"
      }.mkString("\n")

      import DocumentJsonProtocol.documentJsonFormat

      val raw =
        s"""```json
           |${Document(summaries).toJson.toString}
           |```""".stripMargin

      val sections = List(
        Some("Chart" -> chart),
        auditSection,
        Some("Days" -> textLines),
        Some("Raw" -> raw)
      ).flatten
      
      sections.map { case (header, body) =>
        s"# $header\n\n$body\n"
      }.mkString("\n")
    }
  }

  object DocumentJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.util.JsonUtil.CommonJsonProtocol.LocalDateTypeJsonFormat

    implicit val litterSummaryForDayJsonFormat: RootJsonFormat[LitterCharts.LitterSummaryForDay] = jsonFormat4(LitterSummaryForDay)
    implicit val documentMapFormat: RootJsonFormat[Map[LocalDate, LitterCharts.LitterSummaryForDay]] = mapFormat[LocalDate, LitterSummaryForDay]

    implicit val documentJsonFormat: RootJsonFormat[Document] = jsonFormat1(Document)

    import me.micseydel.util.JsonUtil.ZonedDateTimeJsonFormat
    import me.micseydel.vault.LinkIdJsonProtocol.noteIdFormat
    implicit val DataPointJsonFormat: JsonFormat[DataPoint] = jsonFormat4(DataPoint)
    implicit val reportJsonFormat: JsonFormat[MarkdownDailyLitterSummaryReportDocumentParser.LitterReport] = jsonFormat2(MarkdownDailyLitterSummaryReportDocumentParser.LitterReport(_, _))
    implicit val litterReportForDayJsonFormat: JsonFormat[LitterReportForDay] = jsonFormat2(LitterReportForDay)
  }

  implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {

    import DocumentJsonProtocol.documentJsonFormat

    def readDocument()(implicit log: Logger): Try[Option[Document]] = {
      noteRef.readNote().flatMap {
        case Note(markdown, maybeFrontmatter) =>
          markdown.split('\n')
            .dropWhile(!_.startsWith("# Raw"))
            .drop(1)
            .dropWhile(_ == "")
            .toList match {
            case "```json" :: raw :: "```" :: ignoring =>
              log.debug(s"ignoring ${ignoring.size} lines after json, parsing $raw")
              Try(Some(raw.parseJson.convertTo[Document]))
                .recoverWith {
                  case _: DeserializationException =>
                    Try(maybeFrontmatter.map(_.parseJson.convertTo[Document]))
                }
            case other =>
              log.warn(s"falling back on frontmatter, no raw json section found in ${noteRef.noteId} markdown: $markdown")
              // FIXME: logging
              Try(maybeFrontmatter.map(_.parseJson.convertTo[Document]))
          }
      }.recoverWith {
        case _: FileNotFoundException => Success(None)
      }
    }

    def setDocument(document: Document)(implicit l: Logger): Try[NoOp.type] = {
      noteRef.setTo(Note(document.toMarkdown(), None)).map(_ => NoOp)
    }
  }
}

object MonthlyLitterGraphActor {
  private val NoteName = "Monthly Litter Siftings"

  def apply()(implicit Tinker: Tinker): Ability[LitterSummaryForDay] = {
    NoteMakingTinkerer(NoteName, TinkerColor.random(), "📈") { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context

      val monthlyNotesRouter: SpiritRef[MonthlyNotesRouter.Envelope[LitterSummaryForDay]] =
        context.cast(MonthlyNotesRouter(MonthlyAbility(_, _, _)), "MonthlyNotesRouter")

      Tinker.receiveMessage { case (message: LitterSummaryForDay) =>
        monthlyNotesRouter !! MonthlyNotesRouter.Envelope(message, message.forDay)
        Tinker.steadily
      }
    }
  }
}

object MonthlyAbility {

  import LitterGraphHelper.RichNoteRef

  def apply(forMonth: Month, color: TinkerColor, emoji: String)(implicit Tinker: Tinker): (String, Ability[LitterCharts.LitterSummaryForDay]) = {
    val noteName = s"Litter Sifting Chart (${forMonth.isoMonth})"
    noteName -> NoteMakingTinkerer(noteName, color, emoji) { (context, noteRef) =>
      implicit val l: Logger = context.actorContext.log
      l.info(s"Started ${noteRef.noteId}")
      Tinker.receiveMessage { summary: LitterSummaryForDay =>
        noteRef.readDocument().flatMap {
          case Some(document) =>
            document.integrate(summary) match {
              case (false, _) =>
                Success(NoOp)
              case (true, updatedDocument) =>
                noteRef.setDocument(updatedDocument)
              // FIXME: dynamic days in month?
            }
          case None =>
            val document = LitterGraphHelper.Document(Map(summary.forDay -> summary))
            noteRef.setDocument(document)
        } match {
          case Failure(exception) => context.actorContext.log.error(s"Failed to process summary for ${summary.forDay}", exception)
          case Success(NoOp) =>
        }

        Tinker.steadily
      }
    }
  }
}


object Last30DaysLitterGraphActor {

  import LitterGraphHelper.RichNoteRef

  def apply()(implicit Tinker: Tinker): Ability[LitterReportForDay] = {
    val noteName = "Litter Sifting Chart (last 30 days)"
    NoteMakingTinkerer(noteName, TinkerColor.random(), "~") { case (context, noteRef) =>
      implicit val l: Logger = context.actorContext.log
      l.info(s"Started ${noteRef.noteId}, refreshing markdown")

      noteRef.readDocument() match {
        case Failure(exception) => l.warn("failed to read/refresh markdown", exception)
        case Success(None) => l.warn("didn't find any json")
        case Success(Some(document)) =>
          setDocumentTruncated(noteRef, document) match {
            case Failure(exception) => l.warn("failed to write/refresh markdown", exception)
            case Success(NoOp) =>
          }
      }

      Tinker.receiveMessage { report: LitterReportForDay =>
        import me.micseydel.actor.kitties.LitterGraphHelper.DocumentJsonProtocol.litterReportForDayJsonFormat
        val OutTopic = s"${noteRef.noteId}/publish/LitterReportForDay"
        context.actorContext.log.debug(s"publishing to $OutTopic")
        context.system.mqtt ! TypedMqtt.Publish(OutTopic, report.toJson.compactPrint.getBytes)

        val summary: LitterSummaryForDay = report.toSummary
        noteRef.readDocument().flatMap {
          case Some(document) =>
            document.integrate(summary) match {
              case (false, _) =>
                Success(NoOp)
              case (true, updatedDocument) =>
                setDocumentTruncated(noteRef, updatedDocument)
            }
          case None =>
            val document = LitterGraphHelper.Document(Map(summary.forDay -> summary))
            setDocumentTruncated(noteRef, document)
        } match {
          case Failure(exception) => context.actorContext.log.error(s"Failed to process summary for ${summary.forDay}", exception)
          case Success(NoOp) =>
        }

        Tinker.steadily
      }
    }
  }

  private def setDocumentTruncated(noteRef: NoteRef, document: LitterGraphHelper.Document)(implicit l: Logger): Try[NoOp.type] = {
    val latestDay = document.summaries.keys.max
    noteRef.setDocument(LitterGraphHelper.Document(
      // keep the stored state small, with a little extra
      document.summaries.filter(_._1.isAfter(latestDay.minusDays(40))))
    )
  }
}
