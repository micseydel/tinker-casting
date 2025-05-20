package me.micseydel.dsl.cast.chronicler

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import cats.data.{Validated, ValidatedNel}
import ChroniclerMOC.{NoteState, TranscribedMobileNoteEntry}
import ChroniclerMOCDailyMarkdown.ParseSuccessGenericTimedListItem
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.cast.chronicler.ChroniclerMOCDailyMarkdown.ParseSuccessGenericTimedListItem
import me.micseydel.vault.persistence.{BasicNoteRef, JsonlRef, JsonlRefT, NoteRef}
import me.micseydel.vault.{Note, NoteId}
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import scala.util.{Failure, Success}

object ChroniclerMOCDailyNote {
  sealed trait Message

  sealed trait PostInitMessage extends Message {
    def time: ZonedDateTime
  }

  case class AddNote(noteEntry: TranscribedMobileNoteEntry) extends PostInitMessage {
    override def time: ZonedDateTime = noteEntry.time
  }

  case class ListenerAcknowledgement(noteRef: NoteId, timeOfAck: ZonedDateTime, details: String, setState: Option[NoteState]) extends PostInitMessage {
    override def time: ZonedDateTime = timeOfAck
  }

  // behavior

  def apply(forDate: LocalDate)(implicit Tinker: Tinker): Behavior[Message] = Behaviors.setup { context =>
    context.log.info(s"Starting daily transcriptions note for $forDate")

    val isoDate = forDate.format(DateTimeFormatter.ISO_LOCAL_DATE)
    val noteName = s"$BaseNoteName ($isoDate)"
    val jsonFilename = s"""${BaseNoteName.replace(" ", "_").toLowerCase}_$isoDate"""

    Tinker.initializedWithNoteAndPersistedMessages(noteName, jsonFilename, TranscribedMobileNotesJsonProtocol.PostInitializationMessageJsonFormat) {
      case (context, noteRef, jsonlRef) =>
        context.actorContext.log.info(s"Starting")
        behavior(forDate, noteRef, jsonlRef)
    }
  }

  private def behavior(forDate: LocalDate, noteRef: NoteRef, jsonlRef: JsonlRefT[PostInitMessage])(implicit Tinker: Tinker): Behavior[Message] =
    Behaviors.receive { (context, message) =>
      context.log.info(s"Received PostInitMessage of type ${message.getClass.getCanonicalName}")

      message match {
        case message: PostInitMessage =>

          // !!! side effect!
          val todaysCaptures: List[PostInitMessage] = jsonlRef.appendAndGet(message) match {
            case Failure(exception) =>
              throw new RuntimeException(s"Failure for jsonlRef ${jsonlRef}", exception)
            case Success(latest) => latest
          }

          val (_: Seq[AddNote], acks: Seq[ListenerAcknowledgement]) = todaysCaptures.partitionMap {
            case an: AddNote => Left(an)
            case la: ListenerAcknowledgement => Right(la)
          }

          val acksMap: Map[NoteId, Seq[ListenerAcknowledgement]] = acks.groupMap(_.noteRef)(identity)

          val timeForNoteId: Map[NoteId, ZonedDateTime] = todaysCaptures.collect {
            case AddNote(TranscribedMobileNoteEntry(time, ref, _)) =>
              ref -> time
          }.toMap

          populateMarkdownFromCapturesOrHandleSuccessfulParse(forDate, noteRef, context.log, todaysCaptures) { (frontmatter, listItemParseResult) =>
            context.log.info(s"Extracted ${listItemParseResult.size} valid datapoints")
            val updatedDatapoints = message match {
              case an@AddNote(TranscribedMobileNoteEntry(time, ref, wordCount)) =>
                val listItem = ChroniclerMOCDailyMarkdown.addNote2String(an, acksMap)

                //                context.log.warn(s"canary $listItem")
                ChroniclerMOCDailyMarkdown.combineConsecutiveTimestampedDatapoints(
                  // inefficient, but these should always be small (literally "atomic") files
                  listItemParseResult :+ ParseSuccessGenericTimedListItem(
                    time,
                    // FIXME: stupid awful hack to prevent duplication; this whole code needs a refactor
                    listItem.slice(15, listItem.length),
                    None, Nil
                  ), Nil).sortBy(_.time)

              case ListenerAcknowledgement(noteId, _, _, _) =>
                timeForNoteId.get(noteId) match {
                  case Some(time) =>
                    listItemParseResult.filter(_.time != time)
                  case None =>
                    listItemParseResult
                }
            }

            val newMarkdown = ChroniclerMOCDailyMarkdown(todaysCaptures, updatedDatapoints)
            context.log.info(s"Generated new markdown for $noteRef, writing to disk...")
            noteRef.setTo(Note(newMarkdown, frontmatter))
          }

          Behaviors.same
      }
    }

  private def populateMarkdownFromCapturesOrHandleSuccessfulParse(forDate: LocalDate, noteRef: NoteRef, log: Logger, todaysCaptures: List[PostInitMessage])(handler: (Option[String], List[ParseSuccessGenericTimedListItem]) => Unit): Unit = {
    noteRef.read() match {
      case Success(Note(markdown, frontmatter)) =>
        log.info("Successfully read Note from disk")
        val unacknowledged: ValidatedNel[String, List[ChroniclerMOCDailyMarkdown.ParseSuccessGenericTimedListItem]] = ChroniclerMOCDailyMarkdown.extractUnacknowledged(markdown, forDate)

        unacknowledged match {
          case Validated.Valid(listItemParseResult) =>
            log.info(s"Extracted ${listItemParseResult.size} valid datapoints")
            handler(frontmatter, listItemParseResult)

          case Validated.Invalid(errors) =>
            log.warn(s"Failed to extract unacknowledged: $errors")
        }

      case Failure(_: FileNotFoundException) =>
        log.debug(s"File $noteRef did not exist, creating for the first time")
        noteRef.setRaw(ChroniclerMOCDailyMarkdown(todaysCaptures, parseResults = Nil))

      case Failure(exception) =>
        log.warn(s"Unexpected failure fetching the note", exception)
    }
  }

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
