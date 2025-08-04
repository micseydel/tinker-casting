package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.unmarshalling.Unmarshal
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.PurpleAirCloudAPI.{Request, SimplePurpleAirResult}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Sender, SpiritRef, Tinker, TinkerContext}
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml
import net.jcazevedo.moultingyaml.{DefaultYamlProtocol, PimpedAny, YamlArray, YamlFormat, YamlValue}
import spray.json.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object PurpleAirCloudActor {
  sealed trait Message

  private case class ReceivePing(ping: Ping) extends Message
  private case class ReceiveApiResult(simplePurpleAirResults: NonEmptyList[SimplePurpleAirResult]) extends Message

  //

  def apply(apiKey: String)(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("PurpleAirCloudActor", rgb(102, 15, 213), "☁️", ReceivePing) { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.getConfig() match {
      case Validated.Valid(config: Config) =>
        implicit val api: SpiritRef[PurpleAirCloudAPI.Message] = context.cast(PurpleAirCloudAPI(apiKey, context.messageAdapter(ReceiveApiResult)), "PurpleAirCloudAPI")
        context.actorContext.log.info(s"Initialized with ${config.sensors}")
        // FIXME: this should be more dynamic than relying on startup
        behavior(config.sensors)
      case Validated.Invalid(errors) =>
        context.actorContext.log.warn(s"Startup failed: $errors")
        Tinker.ignore
    }
  }

  private def behavior(sensors: NonEmptyList[ConfigEntry])(implicit Tinker: Tinker, noteRef: NoteRef, api: SpiritRef[PurpleAirCloudAPI.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val tc: TinkerContext[?] = context
    message match {
      case ReceivePing(NoOp) =>
        if (noteRef.checkBoxIsChecked()) {
          context.actorContext.log.info("Checkbox was checked, making a sensors request")
          api !! Request(sensors.map(_.index))
        }

        Tinker.steadily

      case ReceiveApiResult(simplePurpleAirResults) =>
        noteRef.setMarkdown("- [ ] Do fetch\n---\n" + simplePurpleAirResults.map {
          case SimplePurpleAirResult(index, pm25, pm25_10minute, pm25_30minute, pm25_60minute, pm25_6hour, pm25_24hour) =>
            s"""- index: $index
               |    - pm25: $pm25
               |    - pm25_10minute: $pm25_10minute
               |    - pm25_30minute: $pm25_30minute
               |    - pm25_60minute: $pm25_60minute
               |    - pm25_6hour: $pm25_6hour
               |    - pm25_24hour: $pm25_24hour
               |""".stripMargin
        }.toList.mkString("\n")) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }
        Tinker.steadily
    }
  }

  //

  case class ConfigEntry(index: Int, label: Option[String])

  case class Config(sensors: NonEmptyList[ConfigEntry])

  // FIXME: centralize
  private def nonEmptyListJsonFormat[T]()(implicit listYamlFormat: YamlFormat[List[T]], yamlFormat: YamlFormat[T]): YamlFormat[NonEmptyList[T]] = new YamlFormat[NonEmptyList[T]] {
    def write(m: NonEmptyList[T]): YamlValue = {
      YamlArray(m.toList.toVector.map(_.toYaml))
    }

    def read(value: YamlValue): NonEmptyList[T] = {
      value.convertTo[List[T]] match {
        case head :: tail => NonEmptyList(head, tail)
        case Nil => throw moultingyaml.DeserializationException("list was empty")
      }
    }
  }

  private object ConfigYamlProtocol extends DefaultYamlProtocol {
    implicit val configEntryYamlFormat: YamlFormat[ConfigEntry] = yamlFormat2(ConfigEntry)
    implicit val configEntryListYamlFormat: YamlFormat[List[ConfigEntry]] = listFormat[ConfigEntry]
    implicit val configEntryNonEmptyListYamlFormat: YamlFormat[NonEmptyList[ConfigEntry]] = nonEmptyListJsonFormat()
    implicit val configYamlFormat: YamlFormat[Config] = yamlFormat1(Config)
  }

  import ConfigYamlProtocol.configYamlFormat

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    import net.jcazevedo.moultingyaml.*

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

  def apply(apiKey: String, replyTo: SpiritRef[NonEmptyList[SimplePurpleAirResult]]): Behavior[Message] = Behaviors.setup { context =>
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
            context.log.info(s"HTTP request succeeded, piping to self now to unmarshal (headers: ${httpResponse.headers})")
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
                val maybeResult: List[SimplePurpleAirResult] = fields.get("data") match {
                  case Some(JsArray(elements)) =>
                    elements.toList.flatMap {
                      case JsArray(Vector(JsNumber(index), JsNumber(pm25), JsNumber(pm25_10minute), JsNumber(pm25_30minute), JsNumber(pm25_60minute), JsNumber(pm25_6hour), JsNumber(pm25_24hour))) =>
                        List(SimplePurpleAirResult(index.toInt, pm25.toFloat, pm25_10minute.toFloat, pm25_30minute.toFloat, pm25_60minute.toFloat, pm25_6hour.toFloat, pm25_24hour.toFloat))
                      case other =>
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

                maybeResult match {
                  case head :: tail =>
                    val result = NonEmptyList(head, tail)
                    context.log.info(maybeResult.mkString("\n"))
                    implicit val sender: Sender = Sender(context.self.path)
                    replyTo !!! result
                  case Nil =>
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
