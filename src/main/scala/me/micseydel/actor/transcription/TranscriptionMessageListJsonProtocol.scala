package me.micseydel.actor.transcription

import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor.transcription.TranscriptionNoteWrapper.{Message, ReceiveRasaResult, ReceiveResponseOllama, TranscriptionCompletedEvent}
import me.micseydel.model.{RasaResultProtocol, WhisperResultJsonProtocol}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

case object TranscriptionMessageListJsonProtocol extends DefaultJsonProtocol {

  import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat

  implicit object TranscriptionNoteWrapperMessageJsonFormat extends RootJsonFormat[Message] {

    import RasaResultProtocol.rasaResultFormat
    import WhisperResultJsonProtocol.whisperResultFormat
    import me.micseydel.Common.{PathJsonFormat, ZonedDateTimeJsonFormat}

    implicit val noticedAudioNoteFormat: RootJsonFormat[NoticedAudioNote] = jsonFormat4(NoticedAudioNote)
    implicit val transcriptionCompletedEventFormat: RootJsonFormat[TranscriptionCompletedEvent] = jsonFormat1(TranscriptionCompletedEvent)
    implicit val receiveRasaResultFormat: RootJsonFormat[ReceiveRasaResult] = jsonFormat2(ReceiveRasaResult)

    import me.micseydel.actor.ollama.OllamaJsonFormat.ChatResponseJsonFormat
    implicit val receiveResponseOllamaFormat: RootJsonFormat[ReceiveResponseOllama] = jsonFormat1(ReceiveResponseOllama)

    def write(m: Message): JsValue = {
      val (jsObj, typ) = m match {
        case tce: TranscriptionCompletedEvent => (tce.toJson.asJsObject, "TranscriptionCompletedEvent")
        case rrr: ReceiveRasaResult => (rrr.toJson.asJsObject, "ReceiveRasaResult")
        case rro: ReceiveResponseOllama =>
                      (rro.toJson.asJsObject, "ReceiveResponseOllama")
//          throw new RuntimeException("This should never be serialized, I'm doing hacky things instead")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): Message = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("TranscriptionCompletedEvent")) => value.convertTo[TranscriptionCompletedEvent]
        case Seq(JsString("ReceiveRasaResult")) => value.convertTo[ReceiveRasaResult]
        case Seq(JsString("ReceiveResponseOllama")) => value.convertTo[ReceiveResponseOllama]
        case other => throw DeserializationException(s"""Unknown type $other, expected Seq(JsString("TranscriptionEvent", "ReceiveRasaResult", "ReceiveResponseOllama"))""")
      }
    }
  }
}
