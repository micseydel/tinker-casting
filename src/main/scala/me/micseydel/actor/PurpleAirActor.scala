package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.RawSensorData.Formatter
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.Series
import spray.json._

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.unused
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object PurpleAirActor {
  sealed trait Message
//  case class Subscribe(data: RawSensorData) extends Message
  private case class ReceiveRawSensorData(data: RawSensorData) extends Message

  def apply(uri: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[RawSensorData]]] = context.cast(DailyNotesRouter(
      "PurpleAir AQI measurements",
      "purpleair",
      PurpleAirJsonFormat.rawSensorDataJsonFormat,
      PurpleAirMarkdown.apply
    ), "DailyNotesRouter")

    @unused // internally driven
    val readingActor = context.cast(ReadingPollingActor(uri, context.messageAdapter(ReceiveRawSensorData)), "ReadingPollingActor")

    Tinker.withMessages {
      case ReceiveRawSensorData(data) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(
          DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(data),
          data.zonedDatetime
        )
        Tinker.steadily
    }
  }

  private object PurpleAirMarkdown {
    def apply(items: List[RawSensorData], clock: TinkerClock): String = {
      val measurements = items.sortBy(_.zonedDatetime).map(_.`pm2_5_aqi`)
      ObsidianCharts.chart(List.fill(measurements.size)(""), List(Series("aqi", measurements))) + "\n"
    }
  }
}


private object ReadingPollingActor {
  sealed trait Message

  case object HeartBeat extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable) extends Message

  private case class ReceiveUnmarshalling(rawSensorData: RawSensorData) extends Message

  def apply(uri: String, replyTo: SpiritRef[RawSensorData])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[_] = context

    context.actorContext.log.info(s"Making request... to $uri")

    context.actorContext.log.info("Making initial request then polling every 10 minutes")
    context.pipeToSelf(request(uri)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }
    val timeKeeper = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(10.minutes, context.self, ReadingPollingActor.HeartBeat, None)

    Tinker.withMessages {
      case HeartBeat =>
        context.actorContext.log.info("Received heartbeat, making request...")
        context.pipeToSelf(request(uri)) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
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
