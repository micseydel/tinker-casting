package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.EventReceiver.{HeartRate, Pupil}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.IntSeries
import me.micseydel.vault.persistence.NoteRef
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import spray.json._
import scala.util.{Failure, Success}

// copied from HeartRateMonitorActor
object PupilMonitorActor {
  sealed trait Message
  private case class ReceivePayload(payload: String) extends Message
  private case class ReceivePing(ping: Ping) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("Pupil Tinkering", TinkerColor.random(), "👁️", ReceivePing) { case (context, noteRef) =>
    context.system.eventReceiver ! EventReceiver.ClaimEventType(Pupil, context.messageAdapter(ReceivePayload).underlying)

    noteRef.setMarkdown("- [ ] Click to generate chart\n") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    implicit val nr: NoteRef = noteRef
    behavior(Nil)
  }

  private def behavior(measurements: List[Int])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceivePayload(payload) =>

        import PayloadJsonFormat.payloadJsonFormat
        val received = payload.parseJson.convertTo[Payload]
        context.actorContext.log.warn(s"Received $received")

        // FIXME: determine payload
//        context.actorContext.log.info(s"Received $payload")
        behavior(Nil)

      case ReceivePing(_) =>
        context.actorContext.log.info(s"noteRef.checkBoxIsChecked()? ${noteRef.checkBoxIsChecked()}")
//        if (noteRef.checkBoxIsChecked()) {
//          noteRef.setMarkdown(
//            s"""- [ ] Click to re-generate chart
//               |    - Max ${measurements.max}
//               |    - Total measurements ${measurements.size}
//               |
//               |# Chart
//               |${ObsidianCharts.chart(IntSeries("Heart Rate", measurements.reverse))}
//               |""".stripMargin)
//        } else {
//          context.actorContext.log.debug("Received note ping but checkbox was not checked")
//        }

        Tinker.steadily
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }

  private case class Payload(
                              captureTime: Double, // time.time() from Python
                              confidence: Double,
                              diameter: Int
                            )

  private object PayloadJsonFormat extends DefaultJsonProtocol {
    implicit val payloadJsonFormat: RootJsonFormat[Payload] = jsonFormat3(Payload)
  }
}
