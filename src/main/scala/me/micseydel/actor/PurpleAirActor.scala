package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.PurpleAirPollingActor.AlterPolling
import me.micseydel.actor.RawSensorData.Formatter
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl._
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.Series
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import spray.json._

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}
import scala.util.{Failure, Success}

object PurpleAirActor {
  sealed trait Message

  case class Subscribe(subscriber: SpiritRef[RawSensorData]) extends Message

  private case class ReceivePing(ping: Ping) extends Message

  private case class ReceiveRawSensorData(data: RawSensorData) extends Message

  def apply(uri: String)(implicit Tinker: Tinker): Ability[Message] = Tinkerer[Message](TinkerColor(185, 96, 203), "💨").withWatchedActorNote("PurpleAir AQI measurements", ReceivePing) { (context, noteRef) =>
    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[RawSensorData]]] = context.cast(DailyNotesRouter(
      "PurpleAir AQI measurements",
      "purpleair",
      PurpleAirJsonFormat.rawSensorDataJsonFormat,
      PurpleAirMarkdown.apply
    ), "DailyNotesRouter")

    val DefaultPollingInterval = 10.minutes

    val apiPoller: SpiritRef[PurpleAirPollingActor.Message] = context.cast(PurpleAirPollingActor(uri, DefaultPollingInterval, context.messageAdapter(ReceiveRawSensorData)), "PurpleAirPollingActor")

    behavior(DefaultPollingInterval.toMinutes, Nil)(Tinker, noteRef, dailyNotesAssistant, apiPoller)
  }

  private def behavior(
                        pollingIntervalMinutes: Long,
                        subscribers: List[SpiritRef[RawSensorData]])(implicit Tinker: Tinker, noteRef: NoteRef, dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[RawSensorData]]], apiPoller: SpiritRef[PurpleAirPollingActor.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case Subscribe(subscriber) =>
        behavior(pollingIntervalMinutes, subscriber :: subscribers)

      case ReceivePing(_) =>
        noteRef.readNote() match {
          case Failure(exception) =>
            context.actorContext.log.error("Failure reading from disk", exception)
            Tinker.steadily

          case Success(note@Note(markdown, _)) =>
            val yaml: Option[Map[String, Any]] = note.yamlFrontMatter
            context.actorContext.log.warn(s"Frontmatter: $yaml")

            yaml match {
              case None =>
                context.actorContext.log.warn("No Frontmatter")
                Tinker.steadily

              case Some(frontmatter) =>
                val refreshNow = markdown(3) match {
                  case 'x' => true
                  case ' ' => false
                  case other =>
                    context.actorContext.log.warn(s"Unexpected char $other for $markdown")
                    false
                }

                val updateIntervalTo: Option[Long] =
                  frontmatter.get("polling_interval_minutes") match {
                    case Some(value: Int) if value > 0 && value != pollingIntervalMinutes => Some(value.toLong)
                    case other =>
                      context.actorContext.log.warn(s"Ignoring $other")
                      None
                  }

                if (refreshNow || updateIntervalTo.nonEmpty) {
                  val alteringPolling = AlterPolling(forceCheckNow = refreshNow, interval = updateIntervalTo.map(_.minutes))
                  context.actorContext.log.warn(s"Altering polling! $alteringPolling")
                  apiPoller !! alteringPolling
                } else {
                  context.actorContext.log.info("Ignoring Frontmatter update - refreshNow was false and no valid update interval")
                }

                updateIntervalTo match {
                  case Some(updatedInterval) =>
                    context.actorContext.log.warn(s"Caching updated interval $updatedInterval")
                    behavior(updatedInterval, subscribers)
                  case None => Tinker.steadily
                }
            }
        }

      case ReceiveRawSensorData(data) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(
          DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(data),
          data.zonedDatetime
        )

        for (subscriber <- subscribers) {
          subscriber !! data
        }

        val subscriberList = subscribers.map { s =>
          s"  - `${s.path.toString.drop(24).takeWhile(_ != '$')}`"
        }.mkString("\n")

        val frontmatter: Map[String, Object] = Map(
          "polling_interval_minutes" -> pollingIntervalMinutes.asInstanceOf[Object]
        )

        implicit val clock: TinkerClock = context.system.clock
        val note = Note(
          s"""- [ ] Click to refresh now
             |- Generated: ${context.system.clock.now()}
             |- Latest: **${data.pm2_5_aqi}** (${TimeUtil.timeSince(data.zonedDatetime).toMinutes} minutes before report generation)
             |- Subscribers:
             |$subscriberList
             |
             |# Today
             |![[PurpleAir AQI measurements (${context.system.clock.today()})]]
             |""".stripMargin,
          frontmatter
        )

        noteRef.setTo(note) match {
          case Failure(exception) =>
            context.actorContext.log.error("Unexpected failure", exception)
          case Success(_) =>
        }

        Tinker.steadily
    }
  }

  private object PurpleAirMarkdown {
    def apply(items: List[RawSensorData], clock: TinkerClock): String = {
      items match {
        case Nil =>
          "No measurements\n"
        case _ =>
          val measurements = items.sortBy(_.zonedDatetime).map(_.pm2_5_aqi)
          val chart = ObsidianCharts.chart(List.fill(measurements.size)(""), List(Series("aqi", measurements)))

          val latest = items.last

          s"""# Chart
             |
             |$chart
             |- Latest value: **${latest.pm2_5_aqi}** at ${latest.zonedDatetime.toString.take(19)}
             |- Markdown generated ${clock.now().toString.take(19)}
             |""".stripMargin
      }
    }
  }
}


private object PurpleAirPollingActor {
  sealed trait Message

  case object HeartBeat extends Message

  case class AlterPolling(forceCheckNow: Boolean, interval: Option[FiniteDuration]) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable) extends Message

  private case class ReceiveUnmarshalling(rawSensorData: RawSensorData) extends Message

  def apply(uri: String, initialInterval: FiniteDuration, replyTo: SpiritRef[RawSensorData])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    context.actorContext.log.info(s"Making initial request then polling every $initialInterval")
    makeRequest(uri)
    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(initialInterval, context.self, PurpleAirPollingActor.HeartBeat, Some(PurpleAirPollingActor.HeartBeat))

    behavior(uri, replyTo, timeKeeper)
  }

  private def behavior(uri: String, replyTo: SpiritRef[RawSensorData], timeKeeper: SpiritRef[TimeKeeper.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    message match {
      case HeartBeat =>
        context.actorContext.log.info("Received heartbeat, making request...")
        makeRequest(uri)
        Tinker.steadily

      case inert@AlterPolling(false, None) =>
        context.actorContext.log.warn(s"Received inert AlterPolling request $inert")
        Tinker.steadily

      case a@AlterPolling(forceCheckNow, maybeDuration) =>
        context.actorContext.log.warn(s"AlterPolling $a")
        if (forceCheckNow) {
          context.actorContext.log.info("A check outside of the usual heartbeat has been requested, making API request now...")
          makeRequest(uri)
        }

        maybeDuration match {
          case Some(updatedInterval) =>
            context.actorContext.log.info(s"Updating polling interval to $updatedInterval")
            timeKeeper !! TimeKeeper.RemindMeEvery(updatedInterval, context.self, PurpleAirPollingActor.HeartBeat, Some(PurpleAirPollingActor.HeartBeat))
          case None =>
        }

        Tinker.steadily

      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received request response...")

        implicit val rawSensorJsonFormat: RootJsonFormat[RawSensorData] = PurpleAirJsonFormat.rawSensorDataJsonFormat
        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]
        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[RawSensorData])
            } catch {
              case e: DeserializationException =>
                ReceiveFailedHttpResponse(new RuntimeException(s"failed to parse: $models", e))
              case e: Throwable =>
                ReceiveFailedHttpResponse(new RuntimeException(s"something unexpected went wrong", e))
            }
        }

        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.warn(s"Failed to connect to local PurpleAir", exception)
        Tinker.steadily

      case ReceiveUnmarshalling(models) =>
        context.actorContext.log.info("Unmarshalling successful, replying and wrapping")
        replyTo !! models
        Tinker.steadily
    }
  }

  // util

  private def makeRequest(uri: String)(implicit context: TinkerContext[Message], actorSystem: ActorSystem[_]): Unit = {
    context.pipeToSelf(request(uri)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }
  }

  private def request(uri: String)(implicit s: ActorSystem[_]): Future[HttpResponse] = {
    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = uri,
      headers = List(
        headers.Accept(MediaTypes.`application/json`)
      )
    ))
  }
}

object PurpleAirJsonFormat extends DefaultJsonProtocol {
  implicit val rawSensorDataJsonFormat: RootJsonFormat[RawSensorData] =
    jsonFormat(
      RawSensorData.apply,
      "DateTime",
      "pm2.5_aqi"
    )
}

case class RawSensorData(
                          //                          SensorId: String,
                          DateTime: String,
                          //  Geo: String,
                          //                          Mem: Int,
                          //  memfrag: Int,
                          //  memfb: Int,
                          //                          memcs: Int,
                          //                          Id: Int,
                          //  lat: Double,
                          //  lon: Double,
                          //                          Adc: Double,
                          //                          loggingrate: Int,
                          //                          place: String,
                          //  version: String,
                          //  uptime: Long,
                          //  rssi: Int,
                          //  period: Int,
                          //  httpsuccess: Int,
                          //  httpsends: Int,
                          //  hardwareversion: String,
                          //  hardwarediscovered: String,
                          //                          current_temp_f: Int,
                          //                          current_humidity: Int,
                          //                          current_dewpoint_f: Int,
                          //                          pressure: Double,
                          //  p25aqic: String,
                          pm2_5_aqi: Int,
                          //  pm1_0_cf_1: Double,
                          //  p_0_3_um: Double,
                          //  pm2_5_cf_1: Double,
                          //  p_0_5_um: Double,
                          //  pm10_0_cf_1: Double,
                          //  p_1_0_um: Double,
                          //  pm1_0_atm: Double,
                          //  p_2_5_um: Double,
                          //  pm2_5_atm: Double,
                          //  p_5_0_um: Double,
                          //  pm10_0_atm: Double,
                          //  p_10_0_um: Double,
                          //  pa_latency: Int,
                          //  wlstate: String,
                          //  status_0: Int,
                          //  status_1: Int,
                          //  status_2: Int,
                          //  status_3: Int,
                          //  status_4: Int,
                          //  status_5: Int,
                          //  status_7: Int,
                          //  status_8: Int,
                          //  status_9: Int,
                          //  ssid: String
                        ) {

  def zonedDatetime: ZonedDateTime = ZonedDateTime.parse(DateTime, Formatter)
}

object RawSensorData {
  val Formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy/MM/dd'T'HH:mm:ss'z'")
    .withZone(java.time.ZoneOffset.UTC)
}
