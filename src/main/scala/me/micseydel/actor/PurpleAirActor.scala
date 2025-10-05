package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.unmarshalling.Unmarshal
import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import FolderWatcherActor.Ping
import me.micseydel.actor.PurpleAirPollingActor.AlterPolling
import me.micseydel.actor.PurpleAirSensorData.Formatter
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.IntSeries
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import spray.json.*

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success}

object PurpleAirActor {
  sealed trait Message

  final case class Subscribe(subscriber: SpiritRef[PurpleAirSensorData]) extends Message

  final case class DoFetchNow() extends Message

  private case class ReceivePing(ping: Ping) extends Message

  private case class ReceiveRawSensorData(data: PurpleAirSensorData) extends Message

  private val NoteName = "PurpleAir AQI measurements"
  private val DefaultPollingInterval = 5.minutes

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("PurpleAir AQI measurements", TinkerColor(185, 96, 203), "💨", ReceivePing, Some("_actor_notes")) { (context, noteRef) =>
    noteRef.getValidatedConfig(DefaultPollingInterval) match {
      case Validated.Invalid(problems) =>
        context.actorContext.log.warn(s"Failed to read config for $NoteName: $problems")
        Tinker.ignore

      case Validated.Valid(config@Config(uri, pollingIntervalMinutes)) =>
        val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[PurpleAirSensorData]]] = context.cast(DailyNotesRouter(
          NoteName,
          "purpleair",
          PurpleAirJsonFormat.rawSensorDataJsonFormat,
          PurpleAirMarkdown.apply
        ), "DailyNotesRouter")

        val apiPoller: SpiritRef[PurpleAirPollingActor.Message] = context.cast(PurpleAirPollingActor(uri, pollingIntervalMinutes, context.messageAdapter(ReceiveRawSensorData)), "PurpleAirPollingActor")

        behavior(config, Nil)(Tinker, noteRef, dailyNotesAssistant, apiPoller)
    }
  }

  private def behavior(
                        config: Config,
                        subscribers: List[SpiritRef[PurpleAirSensorData]])(implicit Tinker: Tinker, noteRef: NoteRef, dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[PurpleAirSensorData]]], apiPoller: SpiritRef[PurpleAirPollingActor.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context

    def checkboxChecked(markdown: String): Boolean = {
      markdown(3) match {
        case 'x' => true
        case ' ' => false
        case other =>
          context.actorContext.log.warn(s"Unexpected char $other for $markdown")
          false
      }
    }

    message match {
      case Subscribe(subscriber) =>
        behavior(config, subscriber :: subscribers)

      case DoFetchNow() =>
        context.actorContext.log.info("Doing a fetch now")
        apiPoller !! AlterPolling(forceCheckNow = true, interval = config.pollingInterval)
        Tinker.steadily

      case ReceivePing(_) =>
        noteRef.readNote() match {
          case Failure(exception) =>
            context.actorContext.log.error("Failure reading from disk", exception)
            Tinker.steadily

          case Success(note@Note(markdown, _)) =>
            val yaml: Option[Map[String, Any]] = note.yamlFrontMatter.toOption

            val refreshNow = checkboxChecked(markdown)
            if (refreshNow) {
              yaml match {
                case None =>
                  context.actorContext.log.warn("No Frontmatter")
                  Tinker.steadily

                case Some(frontmatter) =>
                  val validatedConfig: ValidatedNel[String, Config] = mapToValidatedConfig(frontmatter, config.pollingInterval)
                  val maybeUpdatedConfig: Option[Config] = validatedConfig match {
                    case Validated.Invalid(problems) =>
                      context.actorContext.log.warn(s"Something(s) went wrong parsing the properties: $problems")
                      None
                    case Validated.Valid(latestConfig) =>
                      if (latestConfig == config) {
                        context.actorContext.log.debug(s"Config $config is unchanged")
                        None
                      } else {
                        context.actorContext.log.info(s"Updating config from $config to $latestConfig")
                        Some(latestConfig)
                      }
                  }

                  if (refreshNow) {
                    val alteringPolling = AlterPolling(
                      forceCheckNow = refreshNow,
                      interval = maybeUpdatedConfig.map(_.pollingInterval).getOrElse(config.pollingInterval)
                    )

                    context.actorContext.log.debug(s"Altering polling! $alteringPolling")
                    apiPoller !! alteringPolling
                  } else {
                    context.actorContext.log.info("Ignoring Frontmatter update - refreshNow was false and no valid update interval")
                  }

                  maybeUpdatedConfig match {
                    case None => Tinker.steadily
                    case Some(updatedConfig) =>
                      behavior(updatedConfig, subscribers)
                  }
              }
            } else {
              context.actorContext.log.info("Checkbox not checked")
              Tinker.steadily
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

        implicit val clock: TinkerClock = context.system.clock

        val markdown = s"""- [ ] Click to refresh now
                          |- Generated: ${context.system.clock.now()}
                          |- Latest: **${data.pm2_5_aqi}** (${TimeUtil.timeSince(data.zonedDatetime).toMinutes} minutes before report generation)
                          |- Subscribers:
                          |$subscriberList
                          |
                          |# Today
                          |![[PurpleAir AQI measurements (${context.system.clock.today()})]]
                          |""".stripMargin

        noteRef.setMarkdown(markdown) match {
          case Failure(exception) =>
            context.actorContext.log.error("Unexpected failure", exception)
          case Success(_) =>
        }

        Tinker.steadily
    }
  }

  private object PurpleAirMarkdown {
    def apply(items: List[PurpleAirSensorData], clock: TinkerClock): String = {
      items match {
        case Nil =>
          "No measurements\n"
        case _ =>
          val measurements = items.sortBy(_.zonedDatetime).map(_.pm2_5_aqi)
          val chart = ObsidianCharts.chart(List.fill(measurements.size)(""), List(IntSeries("aqi", measurements)))

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

  //

  private case class Config(uri: String, pollingInterval: FiniteDuration)

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getValidatedConfig(defaultInterval: FiniteDuration): ValidatedNel[String, Config] = {
      noteRef.readNote() match {
        case Failure(exception) => Common.getStackTraceString(exception).invalidNel
        case Success(note) =>
          note.yamlFrontMatter match {
            case Failure(exception) => Common.getStackTraceString(exception).invalidNel
            case Success(properties) =>
              mapToValidatedConfig(properties, defaultInterval)
          }
      }
    }
  }

  private def mapToValidatedConfig(properties: Map[String, Any], defaultPollingInterval: FiniteDuration): ValidatedNel[String, Config] = {
    val pollingInterval: Option[FiniteDuration] = properties.get("polling_interval_minutes") match {
      case Some(value: Int) =>
        Some(value.minutes)
      case _ =>
        None
    }

    properties.get("uri") match {
      case Some(uri: String) =>
        Config(uri, pollingInterval.getOrElse(defaultPollingInterval)).validNel
      case _ =>
        s"Property `uri` is required in [[$NoteName]]".invalidNel
    }
  }
}


private object PurpleAirPollingActor {
  sealed trait Message

  case object HeartBeat extends Message

  case class AlterPolling(forceCheckNow: Boolean, interval: FiniteDuration) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable) extends Message

  private case class ReceiveUnmarshalling(rawSensorData: PurpleAirSensorData) extends Message

  def apply(uri: String, initialInterval: FiniteDuration, replyTo: SpiritRef[PurpleAirSensorData])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    context.actorContext.log.info(s"Making initial request then polling every $initialInterval")
    makeRequest(uri)
    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(initialInterval, context.self, PurpleAirPollingActor.HeartBeat, Some(PurpleAirPollingActor.HeartBeat))

    behavior(uri, replyTo, timeKeeper)
  }

  private def behavior(uri: String, replyTo: SpiritRef[PurpleAirSensorData], timeKeeper: SpiritRef[TimeKeeper.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    message match {
      case HeartBeat =>
        context.actorContext.log.info("Received heartbeat, making request...")
        makeRequest(uri)
        Tinker.steadily

      case a@AlterPolling(forceCheckNow, interval) =>
        context.actorContext.log.debug(s"AlterPolling $a")
        if (forceCheckNow) {
          context.actorContext.log.info("A check outside of the usual heartbeat has been requested, making API request now...")
          makeRequest(uri)
        }

        context.actorContext.log.info(s"Updating polling interval to $interval (should reset current timer)")
        timeKeeper !! TimeKeeper.RemindMeEvery(interval, context.self, PurpleAirPollingActor.HeartBeat, Some(PurpleAirPollingActor.HeartBeat))

        Tinker.steadily

      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received request response...")

        implicit val rawSensorJsonFormat: RootJsonFormat[PurpleAirSensorData] = PurpleAirJsonFormat.rawSensorDataJsonFormat
        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]
        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[PurpleAirSensorData])
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
  implicit val rawSensorDataJsonFormat: RootJsonFormat[PurpleAirSensorData] =
    jsonFormat(
      PurpleAirSensorData.apply,
      "DateTime",
      "pm2.5_aqi"
    )
}

case class PurpleAirSensorData(
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



  def zonedDatetime: ZonedDateTime =
    ZonedDateTime.parse(DateTime, Formatter)
      .withZoneSameInstant(ZoneId.systemDefault())
}

object PurpleAirSensorData {
  val Formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy/MM/dd'T'HH:mm:ss'z'")
    .withZone(java.time.ZoneOffset.UTC)
}
