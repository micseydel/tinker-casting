package me.micseydel.actor

import me.micseydel.actor.RawSensorData.Formatter
import me.micseydel.actor.perimeter.AranetActor
import me.micseydel.actor.perimeter.AranetActor.{AranetResults, Meta}
import me.micseydel.actor.wyze.WyzeActor
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime

object AirQualityManagerActor {
  sealed trait Message

  case class ReceivePurpleAir(data: RawSensorData) extends Message

  case class ReceiveAranetResults(results: AranetResults) extends Message


  private val Filename = "Air Quality Management"
  private val Subdirectory = Some("_actor_notes/airquality")

  def apply(
             purpleAirActor: SpiritRef[PurpleAirActor.Message],
             wyzeActor: SpiritRef[WyzeActor.Message],
             aranetActor: SpiritRef[AranetActor.Message]
           )(implicit Tinker: Tinker): Ability[Message] =
    Tinkerer(TinkerColor.rgb(255, 255, 255), "🫧").initializedWithNote(Filename, Subdirectory) { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context

      purpleAirActor !! PurpleAirActor.Subscribe(context.messageAdapter(ReceivePurpleAir))
      aranetActor !! AranetActor.Fetch(context.messageAdapter(ReceiveAranetResults))

      context.actorContext.log.info("Subscribed to Purple Air and did a fetch for Aranet4")

      initializing(None, None)(Tinker, noteRef, purpleAirActor, wyzeActor, aranetActor)
    }

  private def initializing(latestAqi: Option[Measurement], latestCO2: Option[Measurement])(implicit Tinker: Tinker, noteRef: NoteRef, purpleAirActor: SpiritRef[PurpleAirActor.Message], wyzeActor: SpiritRef[WyzeActor.Message], aranetActor: SpiritRef[AranetActor.Message]): Ability[Message] = Tinker.setup { context =>
    noteRef.setMarkdown(s"[${context.system.clock.now()}] initializing")
    context.actorContext.log.info("Just set initial markdown")

    Tinker.withMessages {
      case ReceivePurpleAir(raw@RawSensorData(_, _)) =>
        latestCO2 match {
          case None =>
            context.actorContext.log.info("Failed to fetch CO2")
            initializing(Some(toMeasurement(raw)), None)
          case Some(co2) =>
            initialized(toMeasurement(raw), co2)
        }

      case ReceiveAranetResults(results) =>
        toMeasurement(results) match {
          case None =>
            context.actorContext.log.warn(s"Aranet results were empty at ${results.meta.captureTime} for elapsed ${results.meta.elapsed}s")
            Tinker.steadily

          case Some(co2) =>
            latestAqi match {
              case Some(aqi) => initialized(aqi, co2)
              case None =>
                context.actorContext.log.info(s"CO2 is $co2 but no AQI received yet")
                initializing(None, Some(co2))
            }
        }
    }
  }

  private def initialized(latestAqi: Measurement, latestCO2: Measurement)(implicit Tinker: Tinker, noteRef: NoteRef, purpleAirActor: SpiritRef[PurpleAirActor.Message], wyzeActor: SpiritRef[WyzeActor.Message], aranetActor: SpiritRef[AranetActor.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val cl: TinkerClock = context.system.clock
    Tinker.withMessages { message =>
      noteRef.setMarkdown(s"- [${context.system.clock.now()}] initialized(latestAqi=$latestAqi, latestCO2=$latestCO2)\n- ignored messaged $message")
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
}
