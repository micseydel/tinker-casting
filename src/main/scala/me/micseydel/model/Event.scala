package me.micseydel.model

// types of events
sealed trait EventType
object TranscriptionCompleted extends EventType
object Tinkering extends EventType
//object RasaResultCompleted extends EventType

// simple wrapper
//case class IncomingEvent(eventType: EventType, payload: String)

//object IncomingEvent {
//  def apply(eventType: EventType, payload: String) = new IncomingEvent(eventType, payload)
//  def unapply(string: String): Option[IncomingEvent] = EventProtocol.extractEvent(string)
//}

// assuming we have the following case class and protocol
//object EventJsonProtocol extends DefaultJsonProtocol {
//  implicit object EventTypeJsonFormat extends RootJsonFormat[EventType] {
//    def write(e: EventType): JsString = JsString(e.toString)
//
//    def read(value: JsValue): EventType = value match {
//      case JsString(s) => s match {
//        case "TranscriptionCompleted" | "transcription_completed" => TranscriptionCompleted
//        case _ => throw DeserializationException(s"Expected transcription_completed")
//      }
//      case _ => throw DeserializationException(s"Expected transcription_completed")
//    }
//  }
//
//  implicit val eventFormat: RootJsonFormat[IncomingEvent] = jsonFormat2(IncomingEvent)
//
//  def extractEvent(jsonString: String): Option[IncomingEvent] = {
//    val parsed = Try(jsonString.parseJson.convertTo[IncomingEvent])
//    parsed match {
//      case Success(event) => Some(event)
//      case Failure(ex) => None
//    }
//  }
// }
