package me.micseydel.dsl.cast.chronicler

import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Operator, Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import scala.util.{Failure, Success}

object ChroniclerMOCDailyNote {
  sealed trait Message {
    def time: ZonedDateTime
  }

  case class ReceiveMidnight(time: ZonedDateTime) extends Message

  sealed trait MarkdownMutatingMessage extends Message

  case class AddNote(noteEntry: TranscribedMobileNoteEntry) extends MarkdownMutatingMessage {
    override def time: ZonedDateTime = noteEntry.time
  }

  case class ListenerAcknowledgement(noteRef: NoteId, forDay: LocalDate, timeOfAck: ZonedDateTime, details: String, setState: Option[NoteState]) extends MarkdownMutatingMessage {
    override def time: ZonedDateTime = timeOfAck
  }

  // behavior

  def apply(forDate: LocalDate)(implicit Tinker: Tinker): Ability[Message] = {
    val noteName = s"$BaseNoteName (${forDate.format(DateTimeFormatter.ISO_LOCAL_DATE)})"
    NoteMakingTinkerer(noteName, TinkerColor.random(), "👨‍💻") { (context, noteRef) =>
      implicit val tc: TinkerContext[?] = context

      context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ReceiveMidnight))

      context.actorContext.log.info(s"Starting daily transcriptions note for $forDate")
      behavior(forDate, noteRef)
    }
  }

  private def behavior(forDate: LocalDate, noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] =
    Tinker.receive { (context, message) =>
      implicit val tc: TinkerContext[?] = context
      implicit val l: Logger = context.actorContext.log
      l.info(s"Received PostInitMessage of type ${message.getClass.getCanonicalName}")

      message match {
        case message: MarkdownMutatingMessage =>
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

        case ReceiveMidnight(midnight) =>
          val notesWithoutAck = noteRef.readMarkdownSafer() match {
            case NoteRef.FileDoesNotExist =>
              context.actorContext.log.warn(s"[CANARY] No transcriptions for $forDate?")
              false
            case NoteRef.Contents(s) =>
              s match {
                case Failure(exception) =>
                  context.actorContext.log.warn(s"[CANARY] Unexpected failure for $forDate", exception)
                  false
                case Success(markdown) =>
                  val hasNotesWithoutAck = markdown.contains("# Notes without acknowledgements")
                  context.actorContext.log.info(s"[CANARY] markdown of length ${markdown.length} hasNotesWithoutAck=$hasNotesWithoutAck for midnight $midnight")
                  hasNotesWithoutAck
              }
          }

          if (notesWithoutAck) {
            val formatter = DateTimeFormatter.ofPattern("yyyyMMdd")
            val id = formatter.format(midnight)
            val notificationId = s"withoutack-$id"
            context.actorContext.log.info(s"[CANARY] creating notification $notificationId for midnight $midnight")
            val notification = Notification(midnight, s"- [[Transcribed mobile notes (${midnight.minusDays(1).toLocalDate})#Notes without acknowledgements]]", None, NotificationId(notificationId), Nil)
            context.system.notifier !! NotificationCenterManager.NewNotification(notification)
          } else {
            context.actorContext.log.info(s"[CANARY] no notes without ack, so not sending any notification")
          }

          Tinker.steadily
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
