package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.RawSensorData.Formatter
import me.micseydel.actor.perimeter.AranetActor
import me.micseydel.actor.perimeter.AranetActor.{AranetResults, Meta}
import me.micseydel.actor.wyze.WyzeActor
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

object AirQualityManagerActor {
  sealed trait Message

  case class ReceivePurpleAir(data: RawSensorData) extends Message

  case class ReceiveAranetResults(results: AranetActor.Result) extends Message

  private case class ReceiveNoteUpdated(ping: Ping) extends Message


  private val Filename = "Air Quality Management"

  def apply(
             purpleAirActor: SpiritRef[PurpleAirActor.Message],
             wyzeActor: SpiritRef[WyzeActor.Message],
             aranetActor: SpiritRef[AranetActor.Message]
           )(implicit Tinker: Tinker): Ability[Message] =
    Tinkerer[Message](TinkerColor.rgb(245, 245, 250), "🫧").withWatchedActorNote(Filename, ReceiveNoteUpdated) { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context

      purpleAirActor !! PurpleAirActor.Subscribe(context.messageAdapter(ReceivePurpleAir))
      aranetActor !! AranetActor.Fetch(context.messageAdapter(ReceiveAranetResults))

      context.actorContext.log.info("Subscribed to Purple Air and did a fetch for Aranet4")

      initializing(None, None)(Tinker, new AirQualityNoteRef(noteRef), purpleAirActor, wyzeActor, aranetActor)
    }

  private def initializing(latestAqi: Option[Measurement], latestCO2: Option[Measurement])(implicit Tinker: Tinker, noteRef: AirQualityNoteRef, purpleAirActor: SpiritRef[PurpleAirActor.Message], wyzeActor: SpiritRef[WyzeActor.Message], aranetActor: SpiritRef[AranetActor.Message]): Ability[Message] = Tinker.setup { context =>
    noteRef.initializing(context.system.clock.now(), latestAqi, latestCO2) match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }

    context.actorContext.log.info("Just set initial markdown")

    Tinker.withMessages {
      case ReceivePurpleAir(raw@RawSensorData(_, _)) =>
        latestCO2 match {
          case None =>
            context.actorContext.log.info("Failed to fetch CO2")
            initializing(Some(toMeasurement(raw)), None)
          case Some(co2) =>
            behavior(toMeasurement(raw), co2)
        }

      case ReceiveAranetResults(result) =>
        val results = result.getOrFail
        toMeasurement(results) match {
          case None =>
            context.actorContext.log.warn(s"""Aranet results were empty at ${results.meta.captureTime} for elapsed ${results.meta.elapsed}s""")
            Tinker.steadily

          case Some(co2) =>
            latestAqi match {
              case Some(aqi) => behavior(aqi, co2)
              case None =>
                context.actorContext.log.info(s"CO2 is $co2 but no AQI received yet")
                initializing(None, Some(co2))
            }
        }

      case ReceiveNoteUpdated(_) =>
        context.actorContext.log.warn("Ignoring updated note")
        Tinker.steadily
    }
  }

  private def behavior(latestAqi: Measurement, latestCO2: Measurement)(implicit Tinker: Tinker, noteRef: AirQualityNoteRef, purpleAirActor: SpiritRef[PurpleAirActor.Message], wyzeActor: SpiritRef[WyzeActor.Message], aranetActor: SpiritRef[AranetActor.Message]): Ability[Message] = Tinker.setup { context =>
    noteRef.withLatest(context.system.clock.now(), latestAqi, latestCO2)

    Tinker.withMessages {
      case ReceivePurpleAir(data) =>
        behavior(toMeasurement(data), latestCO2)

      case ReceiveAranetResults(results) =>
        toMeasurement(results.getOrFail) match {
          case Some(measurement) =>
            behavior(latestAqi, measurement)
          case None =>
            Tinker.steadily
        }

      case ReceiveNoteUpdated(_) =>
        context.actorContext.log.warn("Ignoring updated note")
        Tinker.steadily
    }
  }

  private case class Measurement(value: Int, receivedAt: ZonedDateTime)

  private def toMeasurement(results: AranetResults): Option[Measurement] = {
    results match {
      case results@AranetResults(_, Meta(_, captureTime)) =>
        results.preferred.map { ara =>
          Measurement(ara.co2, captureTime)
        }
    }
  }

  private def toMeasurement(rawSensorData: RawSensorData): Measurement = {
    rawSensorData match {
      case RawSensorData(dateTime, pm2_5_aqi) =>
        val time = ZonedDateTime.parse(dateTime, Formatter)
        Measurement(pm2_5_aqi, time)
    }
  }

  private class AirQualityNoteRef(noteRef: NoteRef) {
    def initializing(now: ZonedDateTime, latestAqi: Option[Measurement], latestCO2: Option[Measurement]): Try[NoOp.type] = {
      noteRef.setMarkdown(
        s"""- [initializing] Generated at $now
           |- Latest AQI $latestAqi
           |- Latest CO2 $latestCO2
           |""".stripMargin)
    }

    def withLatest(now: ZonedDateTime, latestAqi: Measurement, latestCo2: Measurement): Try[NoOp.type] = {
      noteRef.setMarkdown(
        s"""- Generated at $now
           |- Latest AQI $latestAqi
           |- Latest CO2 $latestCo2
           |""".stripMargin)
    }
  }
}

