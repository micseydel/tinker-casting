package me.micseydel.dsl.cast.chronicler

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.NoOp
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import scala.util.{Failure, Success, Try}

object ChroniclerMOCDailyNote {
  sealed trait Message

  sealed trait PostInitMessage extends Message {
    def time: ZonedDateTime
  }

  case class AddNote(noteEntry: TranscribedMobileNoteEntry) extends PostInitMessage {
    override def time: ZonedDateTime = noteEntry.time
  }

  case class ListenerAcknowledgement(noteRef: NoteId, forDay: LocalDate, timeOfAck: ZonedDateTime, details: String, setState: Option[NoteState]) extends PostInitMessage {
    override def time: ZonedDateTime = timeOfAck
  }

  // behavior

  def apply(forDate: LocalDate)(implicit Tinker: Tinker): Ability[Message] = {
    val noteName = s"$BaseNoteName (${forDate.format(DateTimeFormatter.ISO_LOCAL_DATE)})"
    NoteMakingTinkerer(noteName, TinkerColor.random(), "👨‍💻") { (context, noteRef) =>
      context.actorContext.log.info(s"Starting daily transcriptions note for $forDate")
      behavior(forDate, noteRef)
    }
  }

  private def behavior(forDate: LocalDate, noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = {
    // FIXME: remove forDate?
    Tinker.receive { (context, message) =>
      implicit val l: Logger = context.actorContext.log
      l.info(s"Received PostInitMessage of type ${message.getClass.getCanonicalName}")

      message match {
        case message: PostInitMessage =>

          (noteRef.readMarkdownSafer() match {
            case NoteRef.Contents(Success(markdown)) =>
              val latestMarkdown = ChroniclerMOCDailyMarkdown.updatedMarkdown(markdown, message)
//              if (latestMarkdown != markdown) {
//                noteRef.setMarkdown(latestMarkdown)
//              } else {
//                Success(NoOp)
//              }
              Success(latestMarkdown)
            case NoteRef.Contents(f@Failure(exception)) => f
            case NoteRef.FileDoesNotExist =>
              Success(ChroniclerMOCDailyMarkdown.updatedMarkdown("", message))
          }).flatMap(noteRef.setMarkdown)


//          noteRef.updateWith(message) match {
//            case Failure(exception) => context.actorContext.log.warn("Failed to set markdown", exception)
//            case Success(NoOp) =>
//          }

          Tinker.steadily
      }
    }
  }

//  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
//    def updateWith(message: PostInitMessage)(implicit log: Logger): Try[NoOp.type] = {
//      noteRef.readMarkdown().map(ChroniclerMOCDailyMarkdown.parse(_, message.time.toLocalDate)) match {
//        case f@Failure(exception) =>
////          log.warn("Failed to read/parse document", exception)
//          f.map(_ => NoOp)
//        case Success(document) =>
//          val latest = message match {
//            case AddNote(noteEntry) =>
//              document.addEntry(noteEntry)
//            case ack@ListenerAcknowledgement(_, _, _, _) =>
//              document.addAcknowledgement(ack)
//          }
//          if (document != latest) {
//            noteRef.setMarkdown(latest.toMarkdown)
//          } else {
//            Success(NoOp)
//          }
//      }
//    }
//  }

  // model

  sealed trait DataPointState {
    def prefix: String
  }

  case object Todo extends DataPointState {
    override def prefix: String = "[ ] "
  }

  case object Completed extends DataPointState {
    override def prefix: String = "[x] "
  }

  case object StruckThrough extends DataPointState {
    override def prefix: String = "~~"
  }

  // constants

  private val BaseNoteName = "Transcribed mobile notes"
}
