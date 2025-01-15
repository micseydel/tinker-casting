package me.micseydel.actor.transcription

import me.micseydel.model.{BaseModel, LargeModel, WhisperModel, WhisperResult, WhisperResultContent, WhisperResultMetadata}
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.util.{StringUtil, TimeUtil}

import java.nio.file.Path
import java.time.ZonedDateTime

private case class TimedWhisperResult(whisperResult: WhisperResult, startTime: ZonedDateTime)

private[transcription] object TranscriptionModel {
  case class MarkdownModel(
                            // header
                            wavPath: Path,
                            capturedBy: ZonedDateTime,
                            duration: Double,
                            queuedForTranscription: ZonedDateTime,
                            // async additions
                            maybeWhisperLargeResult: Option[TimedWhisperResult] = None,
                            maybeWhisperBaseResult: Option[TimedWhisperResult] = None,
                            //                                  maybeChatResponse: Option[Either[Throwable, ChatResponse]] = None,
                            fyis: List[String] = Nil
                          ) {

    def toMarkdown: String = {
      s"![[${wavPath.getFileName}]]\n" + ((maybeWhisperBaseResult, maybeWhisperLargeResult) match {
        case (None, None) =>
          s"- queued for transcription at $queuedForTranscription with duration ${TimeUtil.getFormattedDuration(duration)}, will update on receipt of transcription"
        case (Some(base), None) =>
            noteContentsForModel(base.whisperResult)
        case (None, Some(large)) =>
          noteContentsForModel(large.whisperResult)
        case (Some(base), Some(large)) =>
          noteContentsForModel(large.whisperResult) +
            "\n" +
            noteContentsForModel(base.whisperResult)
      })
    }

    private def getResultForModel(model: WhisperModel): Option[TimedWhisperResult] = {
      model match {
        case BaseModel =>
          maybeWhisperBaseResult
        case LargeModel =>
          maybeWhisperLargeResult
      }
    }

    def withFyi(fyi: String): MarkdownModel = this.copy(fyis = fyi :: fyis)

    def addTranscriptionCompletedEvent(result: TimedWhisperResult): MarkdownModel = {
      def helper(model: WhisperModel, resultUnderConsideration: TimedWhisperResult): (MarkdownModel, Option[TimedWhisperResult]) = {
        getResultForModel(model) match {
          case Some(TimedWhisperResult(_, existingTranscriptionStartTime)) =>
            if (resultUnderConsideration.startTime.isAfter(existingTranscriptionStartTime)) {
              val withFyi = this.withFyi(s"Ignoring $model transcription started at $existingTranscriptionStartTime")
              (withFyi, Some(resultUnderConsideration))
            } else {
              val withFyi = this.withFyi(s"Ignored $model transcription started at ${resultUnderConsideration.startTime}")
              (withFyi, None)
            }
          case None =>
            (this, Some(resultUnderConsideration))
        }
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
            s"""# Model $model
               |
               |- *Host \\[$performedOn\\] used \\[$model\\] taking ${f"$perfCounterElapsed%.1f"}s for word count $wordCount*
               |
               |## $model Start
               |
               |$rawTextStart
               |""".stripMargin

          if (segments.size > 1) {
            val segList = segments.zipWithIndex.map { case (seg, index) =>
              s"- \\[${formatTime(seg.start)}\\] ${seg.text.strip} ^seg-$model-$index"
            }.mkString("\n")

            contents + s"\n## $model Segments\n\n" + segList
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
