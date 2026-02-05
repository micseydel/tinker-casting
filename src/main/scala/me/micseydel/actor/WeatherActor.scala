package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, ResponseEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml.{PimpedString, *}
import spray.json.*

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object WeatherActor {
  sealed trait Message

  private case class ReceiveNotePing(ping: Ping) extends Message

  private case class Receive(tried: Try[HttpResponse]) extends Message

  private case class ReceiveUnmarshalling(result: Try[String]) extends Message

  def apply(apiKey: String)(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("WeatherActor", TinkerColor.random(), "🌡️", ReceiveNotePing) { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.getConfig() match {
      case Validated.Valid(config: Config) =>
        noteRef.setMarkdown(s"- [ ] Click to refresh ${context.system.clock.now()}") match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }
        behavior(apiKey, config)
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"No Pirate weather config: $e")
        Tinker.ignore
    }
  }

  private def behavior(apiKey: String, config: Config)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[?] = context.system.actorSystem
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case ReceiveNotePing(NoOp) =>
        if (noteRef.checkBoxIsChecked()) {
          context.actorContext.log.warn("Starting weather request!")
          context.pipeToSelf(startHttp(apiKey, config))(Receive)
        } else context.actorContext.log.warn("Box not checked")
        Tinker.steadily
      case Receive(tried) =>
        tried match {
          case Failure(exception) => context.actorContext.log.warn(s"Problem fetching weather", exception)
          case Success(httpResponse) =>
            context.actorContext.log.warn("Piping weather request response to self")
            val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
            val fut: Future[String] = umarshal.to[String]
            context.pipeToSelf(fut)(ReceiveUnmarshalling)
        }
        Tinker.steadily

      case ReceiveUnmarshalling(maybeUnsharsalled) =>
        maybeUnsharsalled match {
          case Failure(exception) => throw exception
          case Success(rawJson) =>
            import JsonFormat.weatherResultJsonFormat
            Try(rawJson.parseJson.convertTo[WeatherResult]) match {
              case Failure(exception) => throw new RuntimeException(s"Raw JSON was not as expected: $rawJson", exception)
              case Success(weatherResult) =>
                noteRef.setWeather(weatherResult)
            }
        }

        Tinker.steadily
    }
  }

  ///

  case class Config(
                     latitude: Double,
                     longitude: Double
                   )

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {

    import ConfigYamlProtocol.configYamlFormat

    def getConfig(): ValidatedNel[String, Config] =
      noteRef.readNote().flatMap(note => Try(note.maybeFrontmatter.map(_.parseYaml.convertTo[Config])).recoverWith {
        case e: net.jcazevedo.moultingyaml.DeserializationException =>
          Failure(new RuntimeException(s"${note.maybeFrontmatter}", e))
      }) match {
        case Failure(exception) => Common.getStackTraceString(exception).invalidNel
        case Success(maybeConfig) =>
          maybeConfig match {
            case None => "Missing config".invalidNel
            case Some(config) => config.validNel
          }
      }

    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => markdown.split("\n").headOption.exists(_.startsWith("- [x] "))) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }

    def setWeather(weatherResult: WeatherResult)(implicit context: TinkerContext[?]): Unit = {
      import JsonFormat.*
      noteRef.setMarkdown(
        s"""- [ ] Click to refresh ${context.system.clock.now()}
           |
           |- Current (${weatherResult.alerts.size} alerts)
           |    - Humidity ${weatherResult.currently.humidity}
           |    - UV index ${weatherResult.currently.uvIndex}
           |    - Apparent temperature ${weatherResult.currently.apparentTemperature}
           |
           |# Alerts
           |
           |```json
           |${weatherResult.alerts.toJson.prettyPrint}
           |```
           |
           |# Minutely
           |
           |```json
           |${weatherResult.minutely.toJson.prettyPrint}
           |```
           |
           |# Hourly
           |
           |```json
           |${weatherResult.hourly.toJson.prettyPrint}
           |```
           |
           |# Daily
           |
           |```json
           |${weatherResult.daily.toJson.prettyPrint}
           |```
           |
           |""".stripMargin) match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }
    }
  }

  private object ConfigYamlProtocol extends DefaultYamlProtocol {
    implicit val configYamlFormat: YamlFormat[Config] = yamlFormat2(Config)
  }

  private def startHttp(apiKey: String, config: Config)(implicit s: ActorSystem[?]): Future[HttpResponse] = {
    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = s"https://api.pirateweather.net/forecast/$apiKey/${config.latitude},${config.longitude}"
    ))
  }

  // externally-defined model

  private object JsonFormat extends DefaultJsonProtocol {
    implicit val flagsJsonFormat: RootJsonFormat[Flags] = jsonFormat4(Flags)
    implicit val alertsJsonFormat: RootJsonFormat[Alert] = jsonFormat7(Alert)
//    implicit val dailyDataFormat = jsonFormat39(DailyData)
    implicit val dailyDataJsonFormat: RootJsonFormat[DailyData] = jsonFormat22(DailyData)
    implicit val dailyJsonFormat: RootJsonFormat[Daily] = jsonFormat3(Daily)
    implicit val hourlyDataFormat: RootJsonFormat[HourlyData] = jsonFormat22(HourlyData)
    implicit val precipDataFormat: RootJsonFormat[PrecipData] = jsonFormat5(PrecipData)
    implicit val hourlyJsonFormat: RootJsonFormat[Hourly] = jsonFormat3(Hourly)
    implicit val minutelyJsonFormat: RootJsonFormat[Minutely] = jsonFormat3(Minutely)
    implicit val currentlyJsonFormat: RootJsonFormat[Currently] = jsonFormat21(Currently)
    implicit val weatherResultJsonFormat: RootJsonFormat[WeatherResult] = jsonFormat11(WeatherResult)
  }

  case class WeatherResult(
                            latitude: Double,
                            longitude: Double,
                            timezone: String,
                            offset: Double,
                            elevation: Int,
                            currently: Currently,
                            minutely: Minutely,
                            hourly: Hourly,
                            daily: Daily,
                            alerts: List[Alert],
                            flags: Flags
                          )

  case class Currently(
                        time: Long,
                        summary: String,
                        icon: String,
                        nearestStormDistance: Double,
                        nearestStormBearing: Int,
                        precipIntensity: Double,
                        precipProbability: Double,
                        precipIntensityError: Double,
                        precipType: String,
                        temperature: Double,
                        apparentTemperature: Double,
                        dewPoint: Double,
                        humidity: Double,
                        pressure: Double,
                        windSpeed: Double,
                        windGust: Double,
                        windBearing: Int,
                        cloudCover: Double,
                        uvIndex: Double,
                        visibility: Double,
                        ozone: Double
                      )

  case class Minutely(
                       summary: String,
                       icon: String,
                       data: List[PrecipData]
                     )

  // FIXME: MinutelyData?
  case class PrecipData(
                         time: Long,
                         precipIntensity: Double,
                         precipProbability: Double,
                         precipIntensityError: Double,
                         precipType: String
                       )

  case class Hourly(
                     summary: String,
                     icon: String,
                     data: List[HourlyData]
                   )

  case class HourlyData(
                         time: Long,
                         summary: String,
                         icon: String,
                         precipIntensity: Double,
                         precipProbability: Double,
                         precipIntensityError: Double,
                         precipAccumulation: Double,
                         precipType: String,
                         temperature: Double,
                         apparentTemperature: Double,
                         dewPoint: Double,
                         humidity: Double,
                         pressure: Double,
                         windSpeed: Double,
                         windGust: Double,
                         windBearing: Int,
                         cloudCover: Double,
                         uvIndex: Double,
                         visibility: Double,
                         ozone: Double,
                         nearestStormDistance: Double,
                         nearestStormBearing: Int
                       )

  case class Daily(
                    summary: String,
                    icon: String,
                    data: List[DailyData]
                  )

  case class DailyData(
                        time: Long,
                        summary: String,
                        icon: String,
//                        sunriseTime: Long,
//                        sunsetTime: Long,
//                        moonPhase: Double,
                        precipIntensity: Double,
                        precipIntensityMax: Double,
                        precipIntensityMaxTime: Long,
                        precipProbability: Double,
                        precipAccumulation: Double,
                        precipType: String,
//                        temperatureHigh: Double,
//                        temperatureHighTime: Long,
//                        temperatureLow: Double,
//                        temperatureLowTime: Long,
//                        apparentTemperatureHigh: Double,
//                        apparentTemperatureHighTime: Long,
//                        apparentTemperatureLow: Double,
//                        apparentTemperatureLowTime: Long,
//                        dewPoint: Double,
                        humidity: Double,
                        pressure: Double,
                        windSpeed: Double,
//                        windGust: Double,
//                        windGustTime: Long,
//                        windBearing: Long,
//                        cloudCover: Double,
                        uvIndex: Double,
                        uvIndexTime: Long,
//                        visibility: Double,
                        temperatureMin: Double,
                        temperatureMinTime: Long,
                        temperatureMax: Double,
                        temperatureMaxTime: Long,
                        apparentTemperatureMin: Double,
                        apparentTemperatureMinTime: Long,
                        apparentTemperatureMax: Double,
                        apparentTemperatureMaxTime: Long,
                      )

  case class Alert(
                     title: String,
                     regions: List[String],
                     severity: String,
                     time: Long,
                     expires: Long,
                     description: String,
                     uri: String
                   )

  case class Flags(
                    sources: List[String],
                    sourceTimes: Map[String, String],
//                    nearestStation: Double,
                    units: String,
                    version: String
                  )

}
