package me.micseydel.actor.kitties

import me.micseydel.NoOp
import me.micseydel.actor.kitties.MarkdownDailyLitterSummaryReportDocumentParser.DataPoint
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.util.MarkdownUtil
import me.micseydel.util.TimeUtil.YearMonthDayTimeSecondFormatter
import me.micseydel.vault.persistence.NoteRef

import java.io.FileNotFoundException
import scala.util.{Failure, Success}

object LitterMatchCorrectionActor {
  sealed trait Message

  final case class Correction(dataPoint: DataPoint) extends Message

  val NoteName = "LitterMatchCorrectionActor"
  val Emoji = "✏️"

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(NoteName, TinkerColor.random(), Emoji) { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef

    Tinker.receiveMessage {
      case Correction(DataPoint(zonedDateTime, siftedContents, noteId, _)) =>
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(zonedDateTime, siftedContents.toEmojis, noteId, dateTimeFormatter = YearMonthDayTimeSecondFormatter)
        // FIXME: need appendIdempotent, and to maybe add a "create if doesn't exist" flag
        nr.append(lineToAdd) match {
          case Failure(_: FileNotFoundException) =>
            noteRef.setMarkdown(lineToAdd) match {
              case Failure(exception) => context.actorContext.log.warn(s"Failed to create ${noteRef.noteId}", exception)
              case Success(NoOp) =>
            }
          case Failure(exception) => context.actorContext.log.warn(s"Failed to add: $lineToAdd", exception)
          case Success(NoOp) =>
        }
        Tinker.steadily
    }
  }
}
