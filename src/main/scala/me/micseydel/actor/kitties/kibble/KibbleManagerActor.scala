package me.micseydel.actor.kitties.kibble

import me.micseydel.NoOp
import me.micseydel.actor.kitties.kibble.KibbleModel.KibbleContainer
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.model.NotedTranscription
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import scala.annotation.{tailrec, unused}
import scala.util.{Failure, Success}

object KibbleManagerActor {
  sealed trait Message

  private[kitties] final case class MaybeHeardKibbleMention(notedTranscription: NotedTranscription) extends Message

  sealed trait Event extends Message

  private[kitties] sealed trait KibbleContainerMeasurement extends Event {
    def container: KibbleContainer

    def massGrams: Int
  }

  private[kitties] case class KibbleRefill(container: KibbleContainer, massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends KibbleContainerMeasurement

  private[kitties] case class RemainingKibbleMeasure(container: KibbleContainer, massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends KibbleContainerMeasurement

  private[kitties] case class KibbleDiscarded(massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends Event

  private val NoteName = "Kibble Tinkering 2.0"

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message](NoteName, CatBrown, "🍚") { (context, noteRef) =>
    @unused
    val listener = context.cast(KibbleManagerListenerActor(context.self), "KibbleManagerListenerActor")

    Tinker.receiveMessage {
      case MaybeHeardKibbleMention(notedTranscription) =>
        context.actorContext.log.debug(s"Received ${notedTranscription.noteId}")
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(notedTranscription.capture.captureTime, notedTranscription.capture.whisperResult.whisperResultContent.text, notedTranscription.noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)
        Tinker.steadily

      case KibbleRefill(container, mass, time, noteId) =>
        val text = s"Refilled $container to $mass"
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)
        Tinker.steadily

      case RemainingKibbleMeasure(container, mass, time, noteId) =>
        val text = s"Measured $container at $mass"
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)
        Tinker.steadily

      case KibbleDiscarded(mass, time, noteId) =>
        val text = s"Discarded $mass kibble"
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)
        Tinker.steadily
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def addOrThrow(line: String)(log: Logger): Unit = {
      noteRef
        .readMarkdown()
        .recoverWith {
          case _: FileNotFoundException => Success("")
        }
        .map(addToMarkdown(_, line)(log))
        .flatMap(noteRef.setMarkdown) match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }
    }

    private[kitties] def addToMarkdown(original: String, formattedLineToAdd: String)(implicit log: Logger): String = {
      if (original.isEmpty) {
        s"""# Summary
           |
           |- for the human to play with, uninterrupted
           |
           |# Inbox
           |
           |$formattedLineToAdd
           |
           |# History
           |
           |$formattedLineToAdd
           |""".stripMargin
      } else {
        val allLines = original.split("\n").toList

        @tailrec
        def findInbox(lines: List[String], accumulator: List[String]): (List[String], List[String]) = {
          lines match {
            case "# Inbox" :: tail => (accumulator, tail)
            case notYetInbox :: tail => findInbox(tail, notYetInbox :: accumulator)
            case Nil => (accumulator, Nil)
          }
        }

        val (notTouchingReversed, whenTailStarts) = findInbox(allLines, Nil)

        @tailrec
        def findMarkdownListEnded(lines: List[String], accumulator: List[String], seenListStart: Boolean = false): (List[String], List[String]) = {
          lines match {
            case head :: tail if head.startsWith("- ") => findMarkdownListEnded(tail, head :: accumulator, seenListStart = true)
            case "" :: tail if seenListStart => (accumulator, tail) // an empty line indicates the list has ended
            case head :: tail => findMarkdownListEnded(tail, head :: accumulator, seenListStart) // tolerates nested lists
            case Nil => (accumulator, Nil)
          }
        }

        val (completedSofar, theRest) = findMarkdownListEnded(whenTailStarts, "# Inbox" :: notTouchingReversed)

        val (shouldBeFinishedReverse, shouldBeEmpty) = findMarkdownListEnded(theRest, "" :: formattedLineToAdd :: completedSofar)

        val result = ("" :: formattedLineToAdd :: shouldBeFinishedReverse).reverse.mkString("\n")

        if (shouldBeEmpty.nonEmpty) {
          log.warn(s"Should have been empty: $shouldBeEmpty; result = $result")
        }

        result
      }
    }
  }
}
