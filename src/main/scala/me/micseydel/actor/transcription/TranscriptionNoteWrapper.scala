package me.micseydel.actor.transcription

import me.micseydel.NoOp
import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.model._
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.{JsonlRefT, NoteRef}

import java.time.temporal.ChronoUnit
import scala.util.{Failure, Success, Try}

object TranscriptionNoteWrapper {
  sealed trait Message

  case class TranscriptionCompletedEvent(event: WhisperResult) extends Message

  // behaviors

  def apply(capture: NoticedAudioNote, listener: SpiritRef[Chronicler.ReceiveNotedTranscription])(implicit Tinker: Tinker): Ability[Message] =
    setup(capture, listener)

  private def setup(capture: NoticedAudioNote, listener: SpiritRef[Chronicler.ReceiveNotedTranscription])(implicit Tinker: Tinker): Ability[Message] = {
    val jsonFilenameWithoutExtension = capture.transcriptionNoteName.replace(" ", "_").toLowerCase
    Tinker.withPersistedMessages(jsonFilenameWithoutExtension, TranscriptionMessageListJsonProtocol.TranscriptionNoteWrapperMessageJsonFormat) { jsonlRef =>
      NoteMakingTinkerer[Message](capture.transcriptionNoteName, TinkerColor.Purple, "🎵") { (context, noteRef) =>
        context.actorContext.log.info(s"Setting stub for ${capture.transcriptionNoteName}")

        noteRef.setStub(capture) match {
          case Failure(exception) => throw exception
          case Success(value) => context.actorContext.log.info(s"wrote $value")
        }

        behavior(capture, noteRef, jsonlRef)(Tinker, listener)
      }
    }
  }

  private def behavior(capture: NoticedAudioNote, noteRef: NoteRef, jsonlRef: JsonlRefT[Message])(implicit Tinker: Tinker, listener: SpiritRef[Chronicler.ReceiveNotedTranscription]): Ability[Message] = Tinker.withPriorMessages(jsonlRef) { (context, message, priorMessages) =>
    implicit val c: TinkerContext[_] = context

    message match {
      case TranscriptionCompletedEvent(whisperResult@WhisperResult(WhisperResultContent(_, _), WhisperResultMetadata(_, _, _, _))) =>
        val notedTranscription: NotedTranscription = {
          val transcriptionCapture = TranscriptionCapture(whisperResult, capture.captureTime)
          NotedTranscription(transcriptionCapture, noteRef.noteId)
        }

        listener !! Chronicler.ReceiveNotedTranscription(notedTranscription)

        // FIXME
        // FIXME instead of storing all prior messages, just update a state json, because we can't/don't want to store any exceptions, which are just for logs; maybe store a string to associate with the stack trace? a uuid?
        // FIXME
        noteRef.regenerateMarkdown(priorMessages, capture)  match {
          case Success(_) =>
          case Failure(exception) => throw exception
        }

        Tinker.steadily
    }
  }

  // ---

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def setStub(capture: NoticedAudioNote): Try[Note] = {
      noteRef.setTo(capture match {
        case NoticedAudioNote(wavPath, captureTime, lengthSeconds, transcriptionStartedTime) =>
          val frontMatter =
            s"""transcription_of: ${wavPath.getFileName}
               |captured_by: $captureTime
               |duration: ${TimeUtil.getFormattedDuration(lengthSeconds)}
               |transcriptionStartedTime: ${transcriptionStartedTime.truncatedTo(ChronoUnit.SECONDS)}"""

          val body = s"![[${wavPath.getFileName}]]\n"

          Note(body, Some(frontMatter))
      })
    }

    def regenerateMarkdown(priorMessages: List[Message], capture: NoticedAudioNote): Try[NoOp.type] = {
      noteRef.setMarkdown(TranscriptionModel.toMarkdown(priorMessages)(capture))
    }
  }
}
