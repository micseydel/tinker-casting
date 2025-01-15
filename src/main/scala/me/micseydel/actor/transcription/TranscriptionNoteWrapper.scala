package me.micseydel.actor.transcription

import akka.actor.typed.ActorRef
import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor.RasaActor
import me.micseydel.actor.ollama.FetchChatResponseActor
import me.micseydel.actor.ollama.OllamaModel.{ChatResponse, ChatResponseFailure, ChatResponseResult}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.model._
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.{JsonlRefT, NoteRef}

import java.time.temporal.ChronoUnit
import scala.util.{Failure, Success, Try}

object TranscriptionNoteWrapper {
  sealed trait Message

  case class TranscriptionCompletedEvent(event: WhisperResult) extends Message

  case class ReceiveRasaResult(result: RasaResult, notedTranscription: NotedTranscription) extends Message

  private[transcription] case class ReceiveResponseOllama(response: ChatResponse) extends Message

  // behaviors

  def apply(
             capture: NoticedAudioNote,
             rasaActor: ActorRef[RasaActor.Message],
             listener: SpiritRef[Chronicler.ActOnNoteRef]
           )(implicit Tinker: Tinker): Ability[Message] = setup(capture, rasaActor, listener)

  private def setup(
                     capture: NoticedAudioNote,
                     rasaActor: ActorRef[RasaActor.Message],
                     listener: SpiritRef[Chronicler.ActOnNoteRef]
                   )(implicit Tinker: Tinker): Ability[Message] = {
    val jsonFilenameWithoutExtension = capture.transcriptionNoteName.replace(" ", "_").toLowerCase
    Tinker.initializedWithNoteAndPersistedMessages(capture.transcriptionNoteName, jsonFilenameWithoutExtension, TranscriptionMessageListJsonProtocol.TranscriptionNoteWrapperMessageJsonFormat) { (context, noteRef, jsonlRef) =>
      context.actorContext.log.info(s"Setting stub for ${capture.transcriptionNoteName}")

      val stub = noteStub(capture)
      noteRef.setTo(stub) match {
        case Failure(exception) => throw exception
        case Success(value) => context.actorContext.log.info(s"wrote $value")
      }

      behavior(capture, noteRef, jsonlRef)(Tinker, rasaActor, listener)
    }
  }

  private def behavior(capture: NoticedAudioNote, noteRef: NoteRef, jsonlRef: JsonlRefT[Message])(implicit Tinker: Tinker, rasaActor: ActorRef[RasaActor.Message], listener: SpiritRef[Chronicler.ActOnNoteRef]): Ability[Message] = Tinker.withPriorMessages(jsonlRef) { (context, message, priorMessages) =>
     implicit val c: TinkerContext[_] = context

     message match {
      case TranscriptionCompletedEvent(whisperResult@WhisperResult(WhisperResultContent(rawText, _), WhisperResultMetadata(model, _, _, _))) =>
        val notedTranscription: NotedTranscription = {
          val transcriptionCapture = TranscriptionCapture(whisperResult, capture.captureTime)
          NotedTranscription(transcriptionCapture, noteRef.noteId, None)
        }

        if (rawText.wordCount < 30) {
          // FIXME: this is a bad pattern - noteTranscription is "captured" by Akka for subsequent ReceiveRasaResult adapters
          val adapter = context.messageAdapter(ReceiveRasaResult(_, notedTranscription))

          // FIXME: this should be done by listeners, not here
          rasaActor ! RasaActor.GetRasaResult(rawText, adapter.underlying)

        } else {
          context.actorContext.log.info("Word count too high, not passing to Rasa; sending noteRef and notedTranscription withOUT Rasa back to Chronicler")
          listener !! Chronicler.ActOnNoteRef(noteRef.noteId, notedTranscription)
        }

        regenerateMarkdown(priorMessages, capture, noteRef)

        if (model == LargeModel && rawText.wordCount > 100) {
          context.castAnonymous(FetchChatResponseActor(
            s"Please politely summarize the following transcribed voice note for a speaker with they/them pronouns, using 3-5 top level Markdown bullet points with sub bullets for elaboration:\n\n$rawText",
            "llama3",
            context.messageAdapter(ReceiveResponseOllama))
          )
        }

        Tinker.steadily

      case ReceiveResponseOllama(response) =>
        response match {
          case ChatResponseFailure(msg, exception) =>
            exception match {
              case Some(throwable) =>
                context.actorContext.log.error(s"Ollama failure: $msg", throwable)
              case None =>
                context.actorContext.log.error(s"Ollama failure: $msg")
            }

          case ChatResponseResult(response, model) =>
            // FIXME: hacky!
            noteRef.append(
              s"""# Ollama summary ($model)
                 |
                 |$response
                 |""".stripMargin)
        }

        Tinker.steadily

        // FIXME: bug - if the Rasa result comes in after multiple transcriptions complete for some reason
        //  (e.g. due to EasyRecorder flushing to dish and Syncthing syncing it) then the later, more recent
        //  and longer transcription is overwritten
      case ReceiveRasaResult(rasaResult, notedTranscriptionWithoutRasa) =>
        val notedTranscription = notedTranscriptionWithoutRasa.copy(rasaResult = Some(rasaResult))
        context.actorContext.log.debug("Sending noteRef and notedTranscription WITH Rasa back to Chronicler")
        listener !! Chronicler.ActOnNoteRef(noteRef.noteId, notedTranscription)

        Tinker.steadily
    }
  }

  // ---

  private def regenerateMarkdown(priorMessages: List[Message], capture: NoticedAudioNote, noteRef: NoteRef): Unit = {
    priorMessages match {
      case (msgs) =>
        noteRef.setMarkdown(toMarkdown(msgs)(capture)) match {
          case Success(_) =>
          case Failure(exception) => throw exception
        }
    }
  }

  private def noteStub(capture: NoticedAudioNote): Note = {
    capture match {
      case NoticedAudioNote(wavPath, captureTime, lengthSeconds, transcriptionStartedTime) =>
        val frontMatter = s"""transcription_of: ${wavPath.getFileName}
           |captured_by: $captureTime
           |duration: ${TimeUtil.getFormattedDuration(lengthSeconds)}
           |transcriptionStartedTime: ${transcriptionStartedTime.truncatedTo(ChronoUnit.SECONDS)}"""

        val body = s"![[${wavPath.getFileName}]]\n"

        Note(body, Some(frontMatter))
    }
  }

  // implementation details

  private def toMarkdown(messages: List[Message])(capture: NoticedAudioNote): String = {
    val model = TranscriptionModel.MarkdownModel(capture.wavPath, capture.captureTime, capture.lengthSeconds, capture.transcriptionStartedTime)

    messages.foldRight(model) { (message, accumulatingResult) =>
      message match {
        case TranscriptionCompletedEvent(wr) =>
          accumulatingResult.addTranscriptionCompletedEvent(TimedWhisperResult(wr, capture.transcriptionStartedTime))
        case ReceiveRasaResult(RasaResult(_, Intent(_, intent), _, _, _), _) =>
          accumulatingResult.withFyi(s"Ignored Rasa result with intent $intent")
        case ReceiveResponseOllama(_) =>
          accumulatingResult
      }
    }.toMarkdown
  }
}
