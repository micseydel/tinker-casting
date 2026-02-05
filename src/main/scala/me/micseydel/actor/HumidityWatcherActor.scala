package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.perimeter.AranetActor
import me.micseydel.actor.perimeter.AranetActor.{Aranet, AranetJsonProtocol, AranetResults}
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.DoubleSeries
import me.micseydel.util.TimeUtil

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
      "humidity",
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

            val lines = s"- generated at ${meta.captureTime} via [[Aranet Devices]]" :: aras.map {
              case Aranet(address, co2, humidity, name, pressure, rssi, temperature) =>
                s"- $name = $humidity"
            }

            val today = s"""\n# Today
               |![[$NoteName (${context.system.clock.today()})]]
               |""".stripMargin

            val markdown = lines.mkString("\n") + today
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
        val averageChart = ObsidianCharts.chart(sparseLabelsExperiment(items), List(DoubleSeries("average", measurements)))

        // FIXME: hack - how can I configure this from Markdown/Frontmatter instead?
        val justBedroomMeasurements: List[AranetResults] = items.filter(_.aras.exists(_.name == "Aranet4 29686"))
        val justBedroomHumidity = justBedroomMeasurements.flatMap(_.aras.filter(_.name == "Aranet4 29686")).map(_.humidity)
        val justBedroomChart = ObsidianCharts.chart(
          sparseLabelsExperiment(justBedroomMeasurements),
          List(DoubleSeries("bedroom", justBedroomHumidity))
        )

        val latest: AranetResults = items.last

        f"""- Latest average humidity: **${latest.averageHumidity}%.1f** at ${latest.meta.captureTime.toString.take(19)}
           |- Markdown generated ${clock.now().toString.take(19)}
           |# Charts
           |
           |## Bedroom
           |
           |$justBedroomChart
           |
           |## Average
           |
           |$averageChart
           |""".stripMargin
    }
  }

  private def sparseLabelsExperiment(measurements: List[AranetResults]): List[String] = {
    // FIXME it turns out the Charts plugin is a little smart about this, so punting the effort
    measurements.map(_.meta.captureTime).map(_.format(TimeUtil.WithinDay24HourDateTimeFormatter))
  }
}
