package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.EventReceiver.Pupil
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.{DoubleSeries, IntSeries}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef
import spray.json.*

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

// copied from HeartRateMonitorActor
object PupilMonitorActor {
  sealed trait Message
  private case class ReceivePayload(payload: String) extends Message
  private case class ReceivePing(ping: Ping) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("Pupil Tinkering", TinkerColor.random(), "👁️", ReceivePing, Some("_actor_notes")) { case (context, noteRef) =>
    context.system.eventReceiver ! EventReceiver.ClaimEventType(Pupil, context.messageAdapter(ReceivePayload).underlying)

    noteRef.setMarkdown("- [ ] Click to generate chart\n") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    implicit val nr: NoteRef = noteRef
    behavior(Nil)
  }

  private def behavior(measurements: List[Payload])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    import PayloadJsonFormat.payloadJsonFormat

    message match {
      case ReceivePayload(payload) =>
        val received = payload.parseJson.convertTo[Payload]
//        context.actorContext.log.warn(s"Received $received")
        behavior(received :: measurements)

      case ReceivePing(_) =>
        context.actorContext.log.info(s"noteRef.checkBoxIsChecked()? ${noteRef.checkBoxIsChecked()}")
        if (noteRef.checkBoxIsChecked()) {
          if (measurements.nonEmpty) {
            val sorted = measurements.sortBy(_.time)
            context.actorContext.log.warn(s"Generating chart from ${measurements.size}")
            noteRef.updateMarkdown(sorted)
            behavior(sorted)
          } else {
            context.actorContext.log.warn("Received note ping but checkbox was not checked")
            Tinker.steadily
          }
        } else {
          context.actorContext.log.debug("Ignoring change, wasn't a checkbox mark")
          Tinker.steadily
        }
    }
  }

  //

  private def measurementsToChart(measurements: List[Payload]): String = ObsidianCharts.chart(
    List(
      IntSeries("Diameter", measurements.map(_.diameter).reverse),
      DoubleSeries("Confidence", measurements.map(_.confidence*100).reverse)
    )
  )

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }

    def updateMarkdown(measurements: List[Payload]): Try[NoOp.type] = {
      val chart = measurementsToChart(measurements)

      val minConfidence = 0.85
      val chart2 = measurementsToChart(measurements.filter(_.confidence > minConfidence))

      noteRef.setMarkdown(
        s"""- [ ] Click to re-generate chart
           |    - Min ${measurements.minBy(_.captureTime).time}
           |    - Max ${measurements.maxBy(_.captureTime).time}
           |    - Total measurements ${measurements.size}
           |
           |# Charts
           |
           |$chart
           |
           |## Just confidence >$minConfidence
           |
           |$chart2
           |""".stripMargin)
    }
  }

  private case class Payload(
                              captureTime: Double, // time.time() from Python
                              confidence: Double,
                              diameter: Int
                            ) {
    def time: ZonedDateTime = TimeUtil.pythonEpocheToZonedDateTime(captureTime.toInt)
  }

  private object PayloadJsonFormat extends DefaultJsonProtocol {
    implicit val payloadJsonFormat: RootJsonFormat[Payload] = jsonFormat3(Payload)
  }
}
