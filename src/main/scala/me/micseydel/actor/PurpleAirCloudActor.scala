package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.unmarshalling.Unmarshal
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.PurpleAirCloudAPI.{PurpleAirBatchResult, Request, SimplePurpleAirResult}
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.DoubleSeries
import me.micseydel.util.{JsonUtil, TimeUtil, YamUtil}
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml.{PimpedString, *}
import spray.json.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneId, ZonedDateTime}
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object PurpleAirCloudActor {
  sealed trait Message

  private case class ReceivePing(ping: Ping) extends Message

  final case class ReceiveApiResult(batchResults: PurpleAirBatchResult) extends Message

  //

  def apply(apiKey: String)(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("PurpleAirCloudActor", rgb(102, 15, 213), "☁️", ReceivePing) { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.getConfig() match {
      case Validated.Valid(config: Config) =>
        implicit val api: SpiritRef[PurpleAirCloudAPI.Message] = context.cast(PurpleAirCloudAPI(apiKey, context.messageAdapter(ReceiveApiResult)), "PurpleAirCloudAPI")
        context.actorContext.log.info(s"Initialized with ${config.sensors}")

        implicit val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[ReceiveApiResult]]] = context.cast(DailyNotesRouter(
          noteRef.noteId.id,
          "purpleaircloudapi",
          ReceiveApiResultJsonProtocol.receiveApiResultJsonFormat,
          dailyMarkdown(config.sensors)
        ), "DailyNotesRouter")

        // FIXME: this should be more dynamic than relying on startup
        behavior(config.sensors)
      case Validated.Invalid(errors) =>
        context.actorContext.log.warn(s"Startup failed: $errors")
        Tinker.ignore
    }
  }

  private def behavior(sensors: NonEmptyList[ConfigEntry])(implicit Tinker: Tinker, noteRef: NoteRef, api: SpiritRef[PurpleAirCloudAPI.Message], dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[ReceiveApiResult]]]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val tc: TinkerContext[?] = context
    message match {
      case ReceivePing(NoOp) =>
        if (noteRef.checkBoxIsChecked()) {
          context.actorContext.log.info("Checkbox was checked, making a sensors request")
          api !! Request(sensors.map(_.index))
        }

        Tinker.steadily

      case r@ReceiveApiResult(batchResult: PurpleAirBatchResult) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(
          DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(r),
          batchResult.time
        )

        noteRef.setMarkdown(s"- [ ] Do fetch\n    - Last generated ${context.system.clock.now()} with batch result server time (${batchResult.time}\n\n![[${noteRef.noteId.id} (${context.system.clock.today()})]]\n\n---\n\n" + batchResult.results.map {
          case SimplePurpleAirResult(index, pm25, pm25_10minute, pm25_30minute, pm25_60minute, pm25_6hour, pm25_24hour) =>
            val sensorIndexToLabel = sensors.map {
              case ConfigEntry(index, label) => index -> label
            }.toList.toMap
            val name = sensorIndexToLabel.get(index).flatten.map(label => s"$label ($index)").getOrElse(index)
            s"""- index: $name
               |    - pm25: $pm25
               |    - pm25_10minute: $pm25_10minute
               |    - pm25_30minute: $pm25_30minute
               |    - pm25_60minute: $pm25_60minute
               |    - pm25_6hour: $pm25_6hour
               |    - pm25_24hour: $pm25_24hour""".stripMargin
        }.toList.mkString("\n")) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }
        Tinker.steadily
    }
  }

  //

  private object ReceiveApiResultJsonProtocol extends DefaultJsonProtocol {
    import me.micseydel.util.JsonUtil.ZonedDateTimeJsonFormat
    implicit val simplePurpleAirResultJsonFormat: JsonFormat[SimplePurpleAirResult] = jsonFormat7(SimplePurpleAirResult)
    implicit val nonEmptyNewSimplePurpleAirResultJsonFormat: JsonFormat[NonEmptyList[SimplePurpleAirResult]] = JsonUtil.nonEmptyListJsonFormat()
    implicit val purpleAirBatchResultJsonFormat: RootJsonFormat[PurpleAirBatchResult] = jsonFormat2(PurpleAirBatchResult)
    implicit val receiveApiResultJsonFormat: RootJsonFormat[ReceiveApiResult] = jsonFormat1(ReceiveApiResult)
  }

  private def dailyMarkdown(sensors: NonEmptyList[ConfigEntry])(messages: List[ReceiveApiResult], clock: TinkerClock): String = {
    val sensorIndexToLabel = sensors.map {
      case ConfigEntry(index, label) => index -> label
    }.toList.toMap

    val unwrapped: List[NonEmptyList[SimplePurpleAirResult]] = messages.map(_.batchResults).sortBy(_.time).map(_.results)

    val batchedByIndex = unwrapped.foldRight(Map[Int, NonEmptyList[SimplePurpleAirResult]]()) { (singleBatchMeasurement, soFar) =>
      val batchedByIndexAccumulator: Map[Int, NonEmptyList[SimplePurpleAirResult]] = singleBatchMeasurement.toList.foldRight(soFar) { (sensorReading, accumulator) =>
        accumulator.updatedWith(sensorReading.index) {
          case None => Some(NonEmptyList.of(sensorReading))
          case Some(value) => Some(value.prepend(sensorReading))
        }
      }
      batchedByIndexAccumulator
    }

    batchedByIndex.map(kv => kv._1 -> kv._2.toList).map { case (index, readings) =>
      val series = List(
        DoubleSeries("pm25", readings.map(_.pm25)),
        DoubleSeries("pm25_10minute", readings.map(_.pm25_10minute)),
        DoubleSeries("pm25_30minute", readings.map(_.pm25_30minute)),
        DoubleSeries("pm25_60minute", readings.map(_.pm25_60minute)),
        DoubleSeries("pm25_6hour", readings.map(_.pm25_6hour)),
        DoubleSeries("pm25_24hour", readings.map(_.pm25_24hour))
      )

      val chart = ObsidianCharts.chart(series)

      s"""# ${sensorIndexToLabel.get(index).flatten.getOrElse(index)}
         |
         |$chart
         |""".stripMargin
    }.mkString("\n")
  }

  //

  case class ConfigEntry(index: Int, label: Option[String])

  case class Config(sensors: NonEmptyList[ConfigEntry])

  private object ConfigYamlProtocol extends DefaultYamlProtocol {
    implicit val configEntryYamlFormat: YamlFormat[ConfigEntry] = yamlFormat2(ConfigEntry)
    implicit val configEntryListYamlFormat: YamlFormat[List[ConfigEntry]] = listFormat[ConfigEntry]
    implicit val configEntryNonEmptyListYamlFormat: YamlFormat[NonEmptyList[ConfigEntry]] = YamUtil.nonEmptyListYamlFormat()
    implicit val configYamlFormat: YamlFormat[Config] = yamlFormat1(Config)
  }

  import ConfigYamlProtocol.configYamlFormat

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {

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
        .map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }
}


object PurpleAirCloudAPI {
  sealed trait Message

  final case class Request(indexes: NonEmptyList[Int]) extends Message

  private case class Receive(tried: Try[HttpResponse]) extends Message

  private case class ReceiveUnmarshalling(result: Try[String]) extends Message

  //

  case class SimplePurpleAirResult(index: Int, pm25: Float, pm25_10minute: Float, pm25_30minute: Float, pm25_60minute: Float, pm25_6hour: Float, pm25_24hour: Float)

  case class PurpleAirBatchResult(time: ZonedDateTime, results: NonEmptyList[SimplePurpleAirResult])

  def apply(apiKey: String, replyTo: SpiritRef[PurpleAirBatchResult]): Behavior[Message] = Behaviors.setup { context =>
    implicit val s: ActorSystem[?] = context.system

    Behaviors.receiveMessage {
      case Request(indexes) =>
        context.log.info("Making an HTTP request...")
        context.pipeToSelf(startHttp(apiKey, indexes))(Receive)
        Behaviors.same

      // -- implementation details --

      case Receive(tried) =>
        context.log.info("Received HTTP result")
        tried match {
          case Failure(exception) =>
            context.log.error("HTTP request failed", exception)
          case Success(httpResponse) =>
            // FIXME: does this not contain the cost?
            context.log.info(s"HTTP request succeeded, piping to self now to unmarshal")
            val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
            val fut: Future[String] = umarshal.to[String]
            context.pipeToSelf(fut)(ReceiveUnmarshalling)
        }
        Behaviors.same

      case ReceiveUnmarshalling(result) =>
        result match {
          case Failure(exception) => throw exception
          case Success(value) =>
            context.log.info("Unmarshalling")
            value.parseJson match {
              case JsObject(fields) =>
                val time = fields.get("data_time_stamp") match {
                  case Some(JsNumber(value)) =>
                    // FIXME: systemDefault should instead probably be in the context/clock
                    TimeUtil.pythonEpocheToZonedDateTime(value.toLong)
                  case other => throw net.jcazevedo.moultingyaml.DeserializationException(s"Needed a `data_time_stamp` number, time since epoche but got $other")
                }

                val maybeResult: Option[PurpleAirBatchResult] = {
                  val sensors = fields.get("data") match {
                    case Some(JsArray(elements)) =>
                      elements.toList.flatMap {
                        case JsArray(Vector(JsNumber(index), JsNumber(pm25), JsNumber(pm25_10minute), JsNumber(pm25_30minute), JsNumber(pm25_60minute), JsNumber(pm25_6hour), JsNumber(pm25_24hour))) =>
                          Some(SimplePurpleAirResult(index.toInt, pm25.toFloat, pm25_10minute.toFloat, pm25_30minute.toFloat, pm25_60minute.toFloat, pm25_6hour.toFloat, pm25_24hour.toFloat))
                        case other =>
                          // FIXME: provide fields.get("fields") which should be clarifying
                          context.log.warn(s"Expected an array with fields {index, pm25, pm25_10minute, pm25_30minute, pm25_60minute, pm25_6hour, pm25_24hour} but got $other")
                          Nil
                      }
                    case None =>
                      context.log.warn(s"Missing field `data` in fields $fields")
                      Nil

                    case other =>
                      context.log.warn(s"Field `data` was not an array: $other")
                      Nil
                  }

                  sensors match {
                    case head :: tail =>
                      Some(PurpleAirBatchResult(time, NonEmptyList(head, tail)))
                    case Nil =>
                      context.log.warn("data contained an empty list")
                      None
                  }
                }

                maybeResult match {
                  case Some(result) =>
                    context.log.info(maybeResult.mkString("\n"))
                    implicit val sender: Sender = Sender(context.self.path)
                    replyTo !!! result
                  case None =>
                }

              case other =>
                context.log.warn(s"Expected a JsObject with `data` field, but got $other")
            }
        }
        Behaviors.same
    }
  }

  private def startHttp(apiKey: String, showOnly: NonEmptyList[Int])(implicit s: ActorSystem[?]): Future[HttpResponse] = {
    val fields = List("pm2.5", "pm2.5_10minute", "pm2.5_30minute", "pm2.5_60minute", "pm2.5_6hour", "pm2.5_24hour")
    val uriToUse = "https://api.purpleair.com/v1/sensors/?" + Map(
      "fields" -> fields,
      "show_only" -> showOnly.toList
    ).map {
      case (k, v) =>
        val encoded = URLEncoder.encode(v.mkString(","), StandardCharsets.UTF_8.toString)
        s"$k=$encoded"
    }.mkString("&")

    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = uriToUse,
      headers = List(
        headers.Accept(MediaTypes.`application/json`),
        headers.RawHeader("X-API-Key", apiKey)
      )
    ))
  }
}
