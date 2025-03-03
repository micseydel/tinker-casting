package me.micseydel.actor.transcription

import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor.ollama.OllamaModel
import me.micseydel.actor.ollama.OllamaModel.ChatResponseResult
import me.micseydel.actor.transcription.TranscriptionNoteWrapper.{Message, ReceiveResponseOllama, TranscriptionCompletedEvent}
import me.micseydel.model._
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.util.{StringUtil, TimeUtil}

import java.nio.file.Path
import java.time.ZonedDateTime

private case class TimedWhisperResult(whisperResult: WhisperResult, startTime: ZonedDateTime)

private[transcription] object TranscriptionModel {
  def toMarkdown(messages: List[Message])(capture: NoticedAudioNote): String = {
    val model = TranscriptionModel.MarkdownModel(capture.wavPath, capture.captureTime, capture.lengthSeconds, capture.transcriptionStartedTime)

    messages.foldRight(model) { (message, accumulatingResult) =>
      message match {
        case TranscriptionCompletedEvent(wr) =>
          accumulatingResult.addTranscriptionCompletedEvent(TimedWhisperResult(wr, capture.transcriptionStartedTime))
//        case ReceiveRasaResult(RasaResult(_, Intent(_, intent), _, _, _), _) =>
//          accumulatingResult.copy(maybeRasaIntent = Some(intent))
        case ReceiveResponseOllama(chatResponse) =>
          chatResponse match {
            case result@OllamaModel.ChatResponseResult(_, _) => accumulatingResult.copy(maybeChatResponseResult = Some(result))
            case OllamaModel.ChatResponseFailure(_, _) => accumulatingResult
          }
      }
    }.toMarkdown
  }

  case class MarkdownModel(
                            // header
                            wavPath: Path,
                            capturedBy: ZonedDateTime,
                            duration: Double,
                            queuedForTranscription: ZonedDateTime,
                            // async additions
                            maybeWhisperLargeResult: Option[TimedWhisperResult] = None,
                            maybeWhisperBaseResult: Option[TimedWhisperResult] = None,
                            maybeChatResponseResult: Option[ChatResponseResult] = None,
                            maybeRasaIntent: Option[String] = None
                          ) {

    def toMarkdown: String = {
      val beginning = maybeWhisperLargeResult.map { whisperLarge =>
          val text = whisperLarge.whisperResult.whisperResultContent.text
          if (text.wordCount > 30) {
            val rawTextStart = StringUtil.truncateText(text.strip(), 100)
            rawTextStart
          } else {
            text
          }
      }

      val parts: List[String] = List(
        beginning.map(_  + "\n"),
        Some(s"![[${wavPath.getFileName}]]"),
        Some("# Details"),
        maybeWhisperLargeResult.map(_.whisperResult).map(noteContentsForModel),
        maybeWhisperBaseResult.map(_.whisperResult).map(noteContentsForModel),
        maybeRasaIntent.filterNot(_ == "no_intent").map(intent => s"# Rasa\n\n- $intent\n"),
        maybeChatResponseResult.map {
          case ChatResponseResult(response, model) => s"# $model\n\n$response\n"
        }
      ).flatten

      parts.mkString("\n")
    }

    private def getResultForModel(model: WhisperModel): Option[TimedWhisperResult] = {
      model match {
        case BaseModel =>
          maybeWhisperBaseResult
        case LargeModel =>
          maybeWhisperLargeResult
      }
    }

    def addTranscriptionCompletedEvent(result: TimedWhisperResult): MarkdownModel = {
      def helper(model: WhisperModel, resultUnderConsideration: TimedWhisperResult): (MarkdownModel, Option[TimedWhisperResult]) = {
        (this, getResultForModel(model) match {
          case Some(TimedWhisperResult(_, existingTranscriptionStartTime)) =>
            Some(resultUnderConsideration).filter(_.startTime.isAfter(existingTranscriptionStartTime))
          case None =>
            Some(resultUnderConsideration)
        })
      }

      result match {
        case r@TimedWhisperResult(WhisperResult(_, WhisperResultMetadata(whisperModel, _, _, _)), _) =>
          helper(whisperModel, r) match {
            case (markdownModel, None) =>
              markdownModel
            case (model, resultToAdd@Some(_)) =>
              whisperModel match {
                case BaseModel =>
                  model.copy(maybeWhisperBaseResult = resultToAdd)
                case LargeModel =>
                  model.copy(maybeWhisperLargeResult = resultToAdd)
              }
          }
      }
    }

    private val MaxLength = 200

    private def noteContentsForModel(event: WhisperResult): String = {
      event match {
        case WhisperResult(WhisperResultContent(text, segments), WhisperResultMetadata(model, performedOn, _, perfCounterElapsed)) =>
          val wordCount = text.wordCount
          val rawTextStart = StringUtil.truncateText(text.strip(), MaxLength)

          val contents =
            s"""## Model $model
               |
               |- *Host \\[$performedOn\\] used \\[$model\\] taking ${f"$perfCounterElapsed%.1f"}s for word count $wordCount*
               |
               |### $model Start
               |
               |$rawTextStart
               |""".stripMargin

          if (segments.size > 1) {
            val segList = segments.zipWithIndex.map { case (seg, index) =>
              s"- \\[${formatTime(seg.start)}\\] ${seg.text.strip} ^seg-$model-$index"
            }.mkString("\n")

            contents + s"\n### $model Segments\n\n" + segList
          } else {
            contents
          }
      }
    }

    private def formatTime(secondsFloat: Float): String = {
      val wholeSeconds = secondsFloat.toInt
      val subSeconds = (secondsFloat - wholeSeconds) * 1000

      val minutes = wholeSeconds / 60
      val seconds = wholeSeconds % 60

      f"$minutes%02d:$seconds%02d.$subSeconds%03.0f"
    }
  }
}
