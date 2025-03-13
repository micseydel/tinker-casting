package me.micseydel.actor.kitties.kibble.json

//import me.micseydel.actor.kitties.kibble.KibbleManagerActor.{Message, KibbleDiscarded, KibbleRefill, RemainingKibbleMeasure}
import me.micseydel.actor.kitties.kibble.KibbleModel._
import me.micseydel.vault.LinkIdJsonProtocol
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat}

case object KibbleEventListJsonProtocol extends DefaultJsonProtocol {
  implicit object KibbleContainerJsonFormat extends RootJsonFormat[KibbleContainer] {
    def write(m: KibbleContainer): JsValue = {
      JsString(m match {
        case Circular1 => "Circular1"
        case Circular2 => "Circular2"
        case RectangularL => "RectangularL"
        case RectangularS => "RectangularS"
      })
    }

    def read(value: JsValue): KibbleContainer = {
      value match {
        case JsString("Circular1") => Circular1
        case JsString("Circular2") => Circular2
//        case JsString("RectangularL") => RectangularL
        case JsString("RectangularS") => RectangularS
        case other => throw DeserializationException(s"Unknown type, expected something in {Circular1, Circular2, RectangularS, RectangularL} but got $other")
      }
    }
  }

  import LinkIdJsonProtocol.noteIdFormat
  import me.micseydel.Common.ZonedDateTimeJsonFormat

//  implicit val kibbleRefilledJsonFormat: RootJsonFormat[KibbleRefill] = jsonFormat4(KibbleRefill)
//  implicit val kibbleMeasuredJsonFormat: RootJsonFormat[RemainingKibbleMeasure] = jsonFormat4(RemainingKibbleMeasure)
//  implicit val kibbleDiscardedJsonFormat: RootJsonFormat[KibbleDiscarded] = jsonFormat3(KibbleDiscarded)
//
//  implicit object EventJsonFormat extends RootJsonFormat[Message] {
//    def write(m: Message): JsValue = {
//      val (jsObj, typ) = m match {
//        case p: KibbleRefill => (p.toJson.asJsObject, "KibbleRefill")
//        case p: RemainingKibbleMeasure => (p.toJson.asJsObject, "RemainingKibbleMeasure")
//        case o: KibbleDiscarded => (o.toJson.asJsObject, "KibbleDiscarded")
//      }
//      JsObject(jsObj.fields + ("type" -> JsString(typ)))
//    }
//
//    def read(value: JsValue): Message = {
//      value.asJsObject.getFields("type") match {
//        case Seq(JsString("KibbleRefill")) => value.convertTo[KibbleRefill]
//        case Seq(JsString("RemainingKibbleMeasure")) => value.convertTo[RemainingKibbleMeasure]
//        case Seq(JsString("KibbleDiscarded")) => value.convertTo[KibbleDiscarded]
//        case other => throw DeserializationException(s"Unknown type, expected something in {KibbleRefill, KibbleDiscarded, RemainingKibbleMeasure} but got $other")
//      }
//    }
//  }
}
