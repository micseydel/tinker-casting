package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.perimeter.AranetActor
import me.micseydel.actor.perimeter.AranetActor.{Aranet, AranetJsonProtocol, AranetResults}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.*
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.DoubleSeries

import scala.util.{Failure, Success}

object HumidityWatcherActor {
  sealed trait Message

  private case class ReceiveAranet(result: AranetActor.Result) extends Message

  private val NoteName = "Humidity Watcher"
  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(NoteName, TinkerColor.random(), "🌵") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    context.system.operator !! Operator.SubscribeAranet4(context.messageAdapter(ReceiveAranet))

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[AranetResults]]] = context.cast(DailyNotesRouter(
      NoteName,
      "aranet",
      AranetJsonProtocol.payloadFormat,
      toMarkdown
    ), "DailyNotesRouter")

    Tinker.receiveMessage {
      case ReceiveAranet(result) =>
        result match {
          case AranetActor.AranetFailure(throwable) => throw throwable
          case result@AranetActor.AranetResults(aras, meta) =>
            dailyNotesAssistant !! DailyNotesRouter.Envelope(
              DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(result),
              meta.captureTime
            )

            val lines = s"- generated at ${meta.captureTime}" :: aras.map {
              case Aranet(address, co2, humidity, name, pressure, rssi, temperature) =>
                s"- $name = $humidity"
            }

            val markdown = lines.mkString("\n")
            noteRef.setMarkdown(markdown) match {
              case Failure(exception) => throw exception
              case Success(NoOp) =>
            }
        }

        Tinker.steadily
    }
  }

  private def toMarkdown(items: List[AranetResults], clock: TinkerClock): String = {
    items match {
      case Nil =>
        "No measurements\n"
      case _ =>
        val measurements: List[Double] = items.sortBy(_.meta.captureTime).map(_.averageHumidity)
        val chart = ObsidianCharts.chart(List.fill(measurements.size)(""), List(DoubleSeries("humdity", measurements)))

        val latest: AranetResults = items.last

        s"""# Chart
           |
           |$chart
           |- Latest average humdity: **${latest.averageHumidity}** at ${latest.meta.captureTime.toString.take(19)}
           |- Markdown generated ${clock.now().toString.take(19)}
           |""".stripMargin
    }
  }
}
