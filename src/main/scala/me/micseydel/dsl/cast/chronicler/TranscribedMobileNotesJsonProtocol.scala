package me.micseydel.dsl.cast.chronicler

import ChroniclerMOC.{AutomaticallyIntegrated, NeedsAttention, NoteState, TranscribedMobileNoteEntry}
import ChroniclerMOCDailyNote.{AddNote, ListenerAcknowledgement, PostInitMessage}
import me.micseydel.util.JsonUtil.ZonedDateTimeJsonFormat
import spray.json._
import me.micseydel.vault.LinkIdJsonProtocol.noteIdFormat

object TranscribedMobileNotesJsonProtocol extends DefaultJsonProtocol {

  implicit val transcribedMobileNoteEntryFormat: RootJsonFormat[TranscribedMobileNoteEntry] = jsonFormat3(TranscribedMobileNoteEntry)
  implicit val addNoteFormat: RootJsonFormat[AddNote] = jsonFormat1(AddNote)

  implicit object NoteStateJsonFormat extends RootJsonFormat[NoteState] {
    def write(m: NoteState): JsValue = m match {
      case NeedsAttention => JsString("NeedsAttention")
      case AutomaticallyIntegrated => JsString("AutomaticallyIntegrated")
    }

    def read(value: JsValue): NoteState = {
      value match {
        case JsString("NeedsAttention") => NeedsAttention
        case JsString("AutomaticallyIntegrated") => AutomaticallyIntegrated
        case other => throw DeserializationException(s"""Unknown type $other; expected Seq(JsString("AddNote")) or Seq(JsString("ListenerAcknowledgement"))""")
      }
    }
  }

  implicit val listenerAcknowledgementFormat: RootJsonFormat[ListenerAcknowledgement] = jsonFormat4(ListenerAcknowledgement)

  implicit object PostInitializationMessageJsonFormat extends RootJsonFormat[PostInitMessage] {
    def write(m: PostInitMessage): JsValue = {
      val (jsObj, typ) = m match {
        case an: AddNote => (an.toJson.asJsObject, "AddNote")
        case la: ListenerAcknowledgement => (la.toJson.asJsObject, "ListenerAcknowledgement")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): PostInitMessage = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("AddNote")) => value.convertTo[AddNote]
        case Seq(JsString("ListenerAcknowledgement")) => value.convertTo[ListenerAcknowledgement]
        case other => throw DeserializationException(s"""Unknown type $other; expected Seq(JsString("AddNote")) or Seq(JsString("ListenerAcknowledgement"))""")
      }
    }
  }

  val listMessageJsonFormat: JsonFormat[List[PostInitMessage]] = listFormat(PostInitializationMessageJsonFormat)
}
