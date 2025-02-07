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

import java.io.FileNotFoundException
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

      val airPurifier: SpiritRef[AirPurifierActor.Message] = context.cast(AirPurifierActor(wyzeActor), "AirPurifierActor")

      initializing(None, None)(Tinker, new AirQualityNoteRef(noteRef), purpleAirActor, airPurifier, aranetActor)
    }

  private def initializing(latestAqi: Option[Measurement], latestCO2: Option[Measurement])(implicit Tinker: Tinker, noteRef: AirQualityNoteRef, purpleAirActor: SpiritRef[PurpleAirActor.Message], airPurifier: SpiritRef[AirPurifierActor.Message], aranetActor: SpiritRef[AranetActor.Message]): Ability[Message] = Tinker.setup { context =>
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
            context.actorContext.log.info("Finishing initializiation...")
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
              case Some(aqi) =>
                context.actorContext.log.info("Finishing initializiation...")
                behavior(aqi, co2)
              case None =>
                context.actorContext.log.info(s"CO2 is $co2 but no AQI received yet")
                initializing(None, Some(co2))
            }
        }

      case ReceiveNoteUpdated(_) =>
        // FIXME copy-paste
        implicit val c: TinkerContext[_] = context
        val (updateAqi, updateCo2) = noteRef.getCheckBoxes()

        if (updateAqi) {
          context.actorContext.log.info("Requesting an AQI fetch")
          purpleAirActor !! PurpleAirActor.DoFetchNow()
        }

        if (updateCo2) {
          context.actorContext.log.info("Requesting a CO2 fetch")
          aranetActor !! AranetActor.Fetch(context.messageAdapter(ReceiveAranetResults))
        }
        Tinker.steadily
    }
  }

  private def behavior(latestAqi: Measurement, latestCO2: Measurement)(implicit Tinker: Tinker, noteRef: AirQualityNoteRef, purpleAirActor: SpiritRef[PurpleAirActor.Message], airPurifier: SpiritRef[AirPurifierActor.Message], aranetActor: SpiritRef[AranetActor.Message]): Ability[Message] = Tinker.setup { context =>
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
        implicit val c: TinkerContext[_] = context
        val (updateAqi, updateCo2) = noteRef.getCheckBoxes()

        if (updateAqi) {
          context.actorContext.log.info("Requesting an AQI fetch")
          purpleAirActor !! PurpleAirActor.DoFetchNow()
        }

        if (updateCo2) {
          context.actorContext.log.info("Requesting a CO2 fetch")
          aranetActor !! AranetActor.Fetch(context.messageAdapter(ReceiveAranetResults))
        }

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
           |    - [ ] refresh now
           |- Latest CO2 $latestCO2
           |    - [ ] refresh now
           |""".stripMargin)
    }

    def withLatest(now: ZonedDateTime, latestAqi: Measurement, latestCo2: Measurement): Try[NoOp.type] = {
      noteRef.setMarkdown(
        s"""- Generated at $now
           |- Latest AQI $latestAqi
           |    - [ ] refresh now
           |- Latest CO2 $latestCo2
           |    - [ ] refresh now
           |""".stripMargin)
    }

    /**
     * @return (doUpdateAQI, doUpdateCO2)
     */
    def getCheckBoxes(): (Boolean, Boolean) = {
      noteRef.readMarkdown() match {
        case Failure(exception) => throw exception
        case Success(markdown) =>
          markdown.split("\\n")
            .filter(_.startsWith("    - ["))
            .filter(_.length >= 8)
            .map(_.charAt(7))
            .toList match {
            case List('x', _) => (true, false)
            case List(_, 'x') => (false, true)
            case List('x', 'x') => (true, true)
            case other => (false, false)
          }
      }
    }
  }
}

object AirPurifierActor {
  sealed trait Message
  final case class SetTo(onOff: Boolean) extends Message
  private case class ReceiveNotePing(ping: Ping) extends Message

  def apply(wyzeActor: SpiritRef[WyzeActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinkerer[Message](TinkerColor(200, 250, 250), "🗼").withWatchedActorNote("Air Purifier", ReceiveNotePing) { (context, noteRef) =>
    implicit val wa: SpiritRef[WyzeActor.Message] = wyzeActor
    implicit val nr: NoteRef = noteRef

    noteRef.getWyzeMac() match {
      case None =>
        noteRef.setMarkdown(
          s"""- set `wyze_mac` property to enable air quality to use the air purifier
             |
             |![[Wyze Plugs]]""".stripMargin
        )
        initializing()

      case Some(wyzeMac) =>
        behavior(wyzeMac)
    }
  }

  private def initializing()(implicit Tinker: Tinker, wyzeActor: SpiritRef[WyzeActor.Message], noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case command@SetTo(_) =>
        context.actorContext.log.warn(s"[initializing] Ignoring command $command, set wyze_mac property to enable")
        Tinker.steadily

      case ReceiveNotePing(_) =>
        noteRef.getWyzeMac() match {
          case Some(wyzeMac) =>
            context.actorContext.log.info(s"Initializing with mac $wyzeMac")
            behavior(wyzeMac)
          case None =>
            context.actorContext.log.debug("[initializing] Note updated but no wyze_mac property found")
            Tinker.steadily
        }
    }
  }

  private def behavior(wyzeMac: String)(implicit Tinker: Tinker, wyzeActor: SpiritRef[WyzeActor.Message], noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    // FIXME: fetch state from Wyze to initialize, then read from this note instead of forcing users to use the embed
    noteRef.setMarkdown(
      s"""![[Wyze Plugs]]""".stripMargin
    ) match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }
    Tinker.withMessages {
      case SetTo(onOff) =>
        implicit val c: TinkerContext[_] = context
        wyzeActor !! WyzeActor.SetPlug(wyzeMac, onOff)
        Tinker.steadily

      case ReceiveNotePing(_) =>
        context.actorContext.log.warn("Ignoring note update")
        Tinker.steadily
    }
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getWyzeMac(): Option[String] = noteRef.readNote() match {
      case Success(note) =>
        note.yamlFrontMatter.toOption.flatMap(_.get("wyze_mac").map(_.toString))
      case Failure(exception: FileNotFoundException) => None
      case Failure(exception) => throw exception
    }
  }
}
