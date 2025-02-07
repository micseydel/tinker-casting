package me.micseydel.actor.kitties.kibble

import me.micseydel.actor.kitties.kibble.KibbleManagerActor.{Event, KibbleDiscarded, KibbleRefill, RemainingKibbleMeasure}
import me.micseydel.actor.kitties.kibble.KibbleModel._
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.{LinkIdJsonProtocol, NoteId}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.annotation.unused
import scala.util.{Failure, Success}

// FIXME: emit Rasa training data? (or almost-Rasa?)
object KibbleManagerActor {
  sealed trait Event {
    def time: ZonedDateTime

    def noteId: NoteId
  }

  private[kitties] sealed trait KibbleContainerMeasurement extends Event {
    def container: KibbleContainer

    def massGrams: Int
  }

  private[kitties] case class KibbleRefill(container: KibbleContainer, massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends KibbleContainerMeasurement

  private[kitties] case class RemainingKibbleMeasure(container: KibbleContainer, massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends KibbleContainerMeasurement

  private[kitties] case class KibbleDiscarded(massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends Event

  private val NoteName = "Kibble Tinkering"

  def apply()(implicit Tinker: Tinker): Ability[Event] = NoteMakingTinkerer[Event](NoteName, CatBrown, "🍚") { (context, noteRef) =>
    Tinker.withPersistedMessages("kibble_tinkering", KibbleEventListJsonProtocol.EventJsonFormat) { jsonlRef =>

      @unused
      val listener = context.cast(KibbleManagerListenerActor(context.self), "KibbleManagerListenerActor")

      context.actorContext.log.info("Refreshing Markdown")
      jsonlRef.get() match {
        case Failure(exception) => throw exception
        case Success(events) =>
          noteRef.setMarkdown(KibbleMarkdownGenerator(events)(context.actorContext.log)) match {
            case Failure(exception) => throw exception
            case Success(_) =>
          }
      }

      Tinker.receiveMessage { m =>
        val allEvents: List[Event] = jsonlRef.appendAndGet(m) match {
          case Failure(exception) => throw exception
          case Success(events) => events
        }

        context.actorContext.log.debug(s"Now have ${allEvents.size}, added $m")

        noteRef.setMarkdown(KibbleMarkdownGenerator(allEvents)(context.actorContext.log))

        Tinker.steadily
      }
    }
  }
}

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
        case JsString("RectangularL") => RectangularL
        case JsString("RectangularS") => RectangularS
        case other => throw DeserializationException(s"Unknown type, expected something in {Circular1, Circular2, RectangularS, RectangularL} but got $other")
      }
    }
  }

  import LinkIdJsonProtocol.noteIdFormat
  import me.micseydel.Common.ZonedDateTimeJsonFormat

  implicit val kibbleRefilledJsonFormat: RootJsonFormat[KibbleRefill] = jsonFormat4(KibbleRefill)
  implicit val kibbleMeasuredJsonFormat: RootJsonFormat[RemainingKibbleMeasure] = jsonFormat4(RemainingKibbleMeasure)
  implicit val kibbleDiscardedJsonFormat: RootJsonFormat[KibbleDiscarded] = jsonFormat3(KibbleDiscarded)

  implicit object EventJsonFormat extends RootJsonFormat[Event] {
    def write(m: Event): JsValue = {
      val (jsObj, typ) = m match {
        case p: KibbleRefill => (p.toJson.asJsObject, "KibbleRefill")
        case p: RemainingKibbleMeasure => (p.toJson.asJsObject, "RemainingKibbleMeasure")
        case o: KibbleDiscarded => (o.toJson.asJsObject, "KibbleDiscarded")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): Event = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("KibbleRefill")) => value.convertTo[KibbleRefill]
        case Seq(JsString("RemainingKibbleMeasure")) => value.convertTo[RemainingKibbleMeasure]
        case Seq(JsString("KibbleDiscarded")) => value.convertTo[KibbleDiscarded]
        case other => throw DeserializationException(s"Unknown type, expected something in {KibbleRefill, KibbleDiscarded, RemainingKibbleMeasure} but got $other")
      }
    }
  }
}
