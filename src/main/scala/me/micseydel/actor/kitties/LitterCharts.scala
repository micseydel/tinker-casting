package me.micseydel.actor.kitties

import me.micseydel.NoOp
import me.micseydel.actor.MonthlyNotesRouter
import me.micseydel.actor.MonthlyNotesRouter.Month
import me.micseydel.actor.kitties.LitterCharts.LitterSummaryForDay
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.IntSeries
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import spray.json._

import java.io.FileNotFoundException
import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object LitterCharts {
  case class LitterSummaryForDay(forDay: LocalDate, peeClumps: Int, poops: Int)
}

private object LitterGraphHelper {
  case class Document(summaries: Map[LocalDate, LitterSummaryForDay]) {
    /**
     * flag for whether the document was updated, and latest document
     */
    def integrate(incomingSummary: LitterSummaryForDay): (Boolean, Document) = {
      summaries.get(incomingSummary.forDay) match {
        case Some(existing) if (existing == incomingSummary) =>
          false -> this
        case _ =>
          true -> Document(
            summaries.updated(incomingSummary.forDay, incomingSummary)
          )
      }
    }

    def toMarkdown: String = {
      val sorted = summaries.toList.sortBy(_._1)
      val xaxis = sorted.map(_._1)
      val yaxis = sorted.map(_._2)

      val labels: List[String] = xaxis.zipWithIndex.map { case (day, i) => if (i % 2 == 0) day.toString else "" }

      val series = List(
        IntSeries("#1", yaxis.map(_.peeClumps)),
        IntSeries("#2", yaxis.map(_.poops))
      )

      ObsidianCharts.chart(labels, series)
    }
  }

  object DocumentJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.Common.CommonJsonProtocol.LocalDateTypeJsonFormat

    implicit val litterSummaryForDayJsonFormat: RootJsonFormat[LitterCharts.LitterSummaryForDay] = jsonFormat3(LitterSummaryForDay)
    implicit val documentMapFormat: RootJsonFormat[Map[LocalDate, LitterCharts.LitterSummaryForDay]] = mapFormat[LocalDate, LitterSummaryForDay]

    implicit val documentJsonFormat: RootJsonFormat[Document] = jsonFormat1(Document)
  }

  implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {

    import DocumentJsonProtocol.documentJsonFormat

    def readDocument(): Try[Option[Document]] = {
      noteRef.readNote().map(_.maybeFrontmatter).recoverWith {
        case _: FileNotFoundException => Success(None)
      }.flatMap(maybeFrontmatter =>
        Try(maybeFrontmatter.map(_.parseJson.convertTo[Document]))
      )
    }

    def setDocument(document: Document): Try[NoOp.type] = {
      noteRef.setTo(Note(document.toMarkdown, Some(document.toJson.toString))).map(_ => NoOp)
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
      Tinker.receiveMessage { summary: LitterSummaryForDay =>
          noteRef.readDocument().flatMap {
            case Some(document) =>
              document.integrate(summary) match {
                case (false, _) =>
                  Success(NoOp)
                case (true, updatedDocument) =>
                  noteRef.setDocument(updatedDocument)
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


object Last21DaysLitterGraphActor {
  import LitterGraphHelper.RichNoteRef

  def apply()(implicit Tinker: Tinker): Ability[LitterSummaryForDay] = {
    val noteName = s"Litter Sifting Chart (last 21 days or so)"
    NoteMakingTinkerer(noteName, TinkerColor.random(), "~") { case (context, noteRef) =>
      Tinker.receiveMessage { summary: LitterSummaryForDay =>
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

  private def setDocumentTruncated(noteRef: NoteRef, document: LitterGraphHelper.Document): Try[NoOp.type] = {
    val latestDay = document.summaries.keys.max
    noteRef.setDocument(LitterGraphHelper.Document(document.summaries.filter(_._1.isAfter(latestDay.minusDays(21)))))
  }
}
