package me.micseydel.model

import me.micseydel.vault.{LinkIdJsonProtocol, NoteId}
import spray.json.{DefaultJsonProtocol, JsonFormat}

import java.time.ZonedDateTime

object Transcription {
  // see: [[Scala regular expression for wikilinks (with alias) extraction]]
//  private[model] def extractContent(input: String): Set[String] = {
//    val pattern = "\\[\\[(.*?)(\\|.*?)?\\]\\]".r
//    pattern.findAllIn(input).matchData
//      .map(m => m.group(1))
//      .toSet
//  }
}

case class NotedTranscription(
    capture: TranscriptionCapture,
    noteId: NoteId
)

object NotedTranscription {
  object NotedTranscriptionJsonProtocol extends DefaultJsonProtocol {
    import LinkIdJsonProtocol.noteIdFormat
    import RasaResultProtocol.rasaResultFormat
    import me.micseydel.model.TranscriptionCapture.TranscriptionCaptureJsonProtocol.transcriptionCaptureFormat
    implicit val notedTranscriptionFormat: JsonFormat[NotedTranscription] = jsonFormat2(NotedTranscription.apply)
  }
}



case class TranscriptionCapture(whisperResult: WhisperResult, captureTime: ZonedDateTime)

object TranscriptionCapture {
  object TranscriptionCaptureJsonProtocol extends DefaultJsonProtocol {
    import WhisperResultJsonProtocol.whisperResultFormat
    import me.micseydel.Common.ZonedDateTimeJsonFormat

    implicit val transcriptionCaptureFormat: JsonFormat[TranscriptionCapture] = jsonFormat2(TranscriptionCapture.apply)
  }
}
