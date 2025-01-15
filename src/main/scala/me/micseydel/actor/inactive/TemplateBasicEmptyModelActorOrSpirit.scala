package me.micseydel.actor.inactive

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.inactive.InitializedWithNoteAndPersistedMessagesTemplate.Receive
import me.micseydel.actor.inactive.InitializedWithNoteAndPersistedMessagesTemplate.Message
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

object TemplateBasicEmptyModelActorOrSpirit {
  sealed trait Message

  def spirit()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.unhandled
  }

  def actor(): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.unhandled
  }
}


private
//


object InitializedWithNoteAndPersistedMessagesTemplate {
  sealed trait Message

  case class Receive() extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.initializedWithNoteAndPersistedMessages("NoteName", "json_name", TEMPLATEJsonFormat.messageJsonFormat) { case (context, noteRef, jsonRef) =>
    Tinker.unhandled
  }
}

private object TEMPLATEJsonFormat extends DefaultJsonProtocol {
  implicit val receiveMessageFormat: JsonFormat[Receive] = jsonFormat0(Receive)

  implicit object messageJsonFormat extends RootJsonFormat[Message] {
    def write(m: Message): JsValue = {
      val (jsObj, typ) = m match {
        case o: Receive => (o.toJson.asJsObject, "Receive")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): Message = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("Receive")) => value.convertTo[Receive]
        case other => throw DeserializationException(s"""Unknown type $other, expected {ReceiveLastAte, ReceiveFrustrationDetected, ReceiveSleepReport, ReceiveAranetResult}""")
      }
    }
  }
}
