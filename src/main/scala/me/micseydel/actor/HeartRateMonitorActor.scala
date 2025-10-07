package me.micseydel.actor

import me.micseydel.NoOp
import FolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TypedMqtt.MqttMessage
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TypedMqtt}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.IntSeries
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object HeartRateMonitorActor {
  sealed trait Message
  private case class ReceiveMqttMessage(message: MqttMessage) extends Message
  private case class ReceivePing(ping: Ping) extends Message

  private val Topic = "simple_heart_rate"
  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("Heart Rate Tinkering", TinkerColor.random(), "❤️", ReceivePing, Some("_actor_notes")) { case (context, noteRef) =>
    context.system.mqtt ! TypedMqtt.Subscribe("simple_heart_rate", context.messageAdapter(ReceiveMqttMessage).underlying)

    noteRef.setMarkdown("- [ ] Click to generate chart\n") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    implicit val nr: NoteRef = noteRef
    behavior(Nil)
  }

  private def behavior(measurements: List[Int])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveMqttMessage(MqttMessage(Topic, payload)) =>
        val int = new String(payload).toInt
        context.actorContext.log.info(s"""Received ${payload.mkString("Array(", ", ", ")")} -> int""")
        behavior(int :: measurements)

      case ReceivePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          noteRef.setMarkdown(
            s"""- [ ] Click to re-generate chart
               |    - Max ${measurements.max}
               |    - Total measurements ${measurements.size}
               |
               |# Chart
               |${ObsidianCharts.chart(IntSeries("Heart Rate", measurements.reverse))}
               |""".stripMargin)
        } else {
          context.actorContext.log.debug("Received note ping but checkbox was not checked")
        }

        Tinker.steadily

      case ReceiveMqttMessage(MqttMessage(unexpectedTopic, payload)) =>
        val startOfPayload = payload.take(1000).mkString("Array(", ", ", ")")
        context.actorContext.log.warn(s"Unexpected topic $unexpectedTopic, only expected $Topic; payload start =${startOfPayload}")
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
