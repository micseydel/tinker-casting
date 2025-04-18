package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.EventReceiver.HeartRate
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.Series
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object HeartRateMonitorActor {
  sealed trait Message
  private case class ReceivePayload(payload: String) extends Message
  private case class ReceivePing(ping: Ping) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("Heart Rate Tinkering", TinkerColor.random(), "❤️", ReceivePing) { case (context, noteRef) =>
    context.system.eventReceiver ! EventReceiver.ClaimEventType(HeartRate, context.messageAdapter(ReceivePayload).underlying)

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
        context.actorContext.log.info(s"Received $payload")
        behavior(payload.toInt :: measurements)

      case ReceivePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          noteRef.setMarkdown(
            s"""- [ ] Click to re-generate chart
               |    - Max ${measurements.max}
               |    - Total measurements ${measurements.size}
               |
               |# Chart
               |${ObsidianCharts.chart(Series("Heart Rate", measurements.reverse))}
               |""".stripMargin)
        } else {
          context.actorContext.log.debug("Received note ping but checkbox was not checked")
        }

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
}
