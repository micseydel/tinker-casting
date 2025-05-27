package me.micseydel.actor.transcription

import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor.transcription.TranscriptionNoteWrapper.{Message, TranscriptionCompletedEvent}
import me.micseydel.model.WhisperResultJsonProtocol
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

case object TranscriptionMessageListJsonProtocol extends DefaultJsonProtocol {

  implicit object TranscriptionNoteWrapperMessageJsonFormat extends RootJsonFormat[Message] {

    import WhisperResultJsonProtocol.whisperResultFormat
    import me.micseydel.Common.{PathJsonFormat, ZonedDateTimeJsonFormat}

    implicit val noticedAudioNoteFormat: RootJsonFormat[NoticedAudioNote] = jsonFormat4(NoticedAudioNote)
    implicit val transcriptionCompletedEventFormat: RootJsonFormat[TranscriptionCompletedEvent] = jsonFormat1(TranscriptionCompletedEvent)

    def write(m: Message): JsValue = {
      val (jsObj, typ) = m match {
        case tce: TranscriptionCompletedEvent => (tce.toJson.asJsObject, "TranscriptionCompletedEvent")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): Message = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("TranscriptionCompletedEvent")) => value.convertTo[TranscriptionCompletedEvent]
        case other => throw DeserializationException(s"""Unknown type $other, expected Seq(JsString("TranscriptionEvent", "ReceiveRasaResult", "ReceiveResponseOllama"))""")
      }
    }
  }
}
