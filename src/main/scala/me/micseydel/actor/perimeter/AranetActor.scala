package me.micseydel.actor.perimeter

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, ResponseEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.Common.ZonedDateTimeJsonFormat
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.vault.Note
import spray.json._

import java.time.ZonedDateTime
import scala.concurrent.Future
import scala.util.{Failure, Success}

object AranetActor {

  case class Config(aranetHost: String, aranetPort: Int, highCO2Key: Option[String])

  // mailbox
  sealed trait Message

  case class Fetch(replyTo: SpiritRef[Result]) extends Message

  case class ReceiveResult(result: Result) extends Message

  case class ReceiveNoteUpdated(ping: Ping) extends Message

  def apply(config: Config)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    behavior(config, lastSeenElevated = false)
  }

  private val NoteName = "Aranet Devices"

  private def behavior(config: Config, lastSeenElevated: Boolean)(implicit Tinker: Tinker): Ability[Message] = {
    Tinker.withWatchedActorNote[Message](NoteName, ReceiveNoteUpdated.apply) { (context, noteRef) =>
      import AranetJsonProtocol.payloadFormat

      val httpRequest = HttpRequest(
        method = HttpMethods.GET,
        uri = s"http://${config.aranetHost}:${config.aranetPort}/ara4s"
      )

      noteRef.setMarkdown(s"[${ZonedDateTime.now()}] initializing...")

      Tinker.withMessages {
        case ReceiveResult(result) =>
          result match {
            case AranetFailure(throwable) => context.actorContext.log.error("Aranet fetching failed", throwable)
            case AranetResults(aras, Meta(elapsed, captureTime)) =>
              context.actorContext.log.info(s"Setting Markdown...")
              noteRef.setMarkdown {
                "- [ ] Click to refresh\n" +
                s"- Captured at ${captureTime.toString.take(19)} taking ${elapsed.toInt}s\n" +
                  aras.map {
                    case Aranet(_, co2, humidity, name, _, _, temperature) =>
                      s"- $name -> **CO2 $co2**  temp $temperature°C  humidity $humidity"
                  }.mkString("\n") + "\n"
              }
          }

          Tinker.steadily

        case Fetch(replyTo) =>
          context.actorContext.log.info("Making HTTP request for Aranet")
          context.castAnonymous(HttpFetchAndUnmarshall(httpRequest, replyTo, AranetFailure, context.messageAdapter(ReceiveResult)))

          Tinker.steadily

        case ReceiveNoteUpdated(_) =>
          noteRef.readNote() match {
            case Failure(exception) => throw exception
            case Success(Note(markdown, _)) =>
              markdown.split("\n", 2).toList match {
                case "- [x] Click to refresh" :: _ =>
                  context.actorContext.log.info("clicktorefresh triggered!")
                  context.castAnonymous(HttpFetchAndUnmarshall(httpRequest, context.messageAdapter(ReceiveResult), AranetFailure))

                case "- [ ] Click to refresh" :: _ =>
                case firstLine :: _ if firstLine.contains("initializing") =>
                case _ =>
                  context.actorContext.log.warn(s"Weird Markdown: $markdown")
              }

            case Success(ignoring) =>
              context.actorContext.log.info(s"ignoring $ignoring")
          }

          Tinker.steadily

      }
    }
  }

  // model

  case class Aranet(
                     address: String,
                     co2: Int,
                     humidity: Double,
                     name: String,
                     pressure: Double,
                     rssi: Int,
                     temperature: Double
                   )

  case class Meta(elapsed: Double, captureTime: ZonedDateTime)

  sealed trait Result {
    def getOrFail: AranetResults
  }

  case class AranetFailure(throwable: Throwable) extends Result {
    override def getOrFail: AranetResults = throw throwable
  }

  case class AranetResults(aras: List[Aranet], meta: Meta) extends Result {
    def preferred: Option[Aranet] = {
      // FIXME: pull this out into configuration, or note Yaml
      aras.find(_.name.contains("1A300"))
        .orElse(aras.find(_.name.contains("24DBE")))
    }

    def anyElevated: Option[Aranet] = aras.find(_.co2 > 1000)

    override def getOrFail: AranetResults = this
  }

  object AranetJsonProtocol extends DefaultJsonProtocol {
    implicit val aranetFormat: RootJsonFormat[Aranet] = jsonFormat7(Aranet)
    implicit val metaFormat: RootJsonFormat[Meta] = jsonFormat2(Meta)
    implicit val payloadFormat: RootJsonFormat[AranetResults] = jsonFormat2(AranetResults.apply)
  }
}

private object HttpFetchAndUnmarshall {
  sealed trait Message[T]

  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]

  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]

  private case class ReceiveUnmarshalling[T](reply: T) extends Message[T]

  // behavior
  def apply[T, E <: T, R <: T](httpRequest: HttpRequest, replyTo: SpiritRef[T], failureWrapper: Throwable => E, cc: SpiritRef[T])(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[R]): Ability[Message[T]] = Tinker.setup { context =>
    apply(httpRequest, replyTo, failureWrapper, Some(cc))
  }

  def apply[T, E <: T, R <: T](httpRequest: HttpRequest, replyTo: SpiritRef[T], failureWrapper: Throwable => E, cc: Option[SpiritRef[T]] = None)(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[R]): Ability[Message[T]] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    implicit val s: ActorSystem[_] = context.system.actorSystem

    context.pipeToSelf(Http().singleRequest(httpRequest)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.withMessages {
      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received HttpResponse, beginning unmarshalling process")

        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]

        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[R])
            } catch {
              case e: DeserializationException =>
                // FIXME: this capture should be opt-in via config due to risk of leaking sensitive data
                ReceiveFailedHttpResponse(new RuntimeException(s"failed to parse: $models", e))
            }
        }

        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.error("Something went wrong with HttpResponse", exception)
        val wrapped = failureWrapper(exception)
        replyTo !! wrapped
        cc.foreach(_ !! wrapped)
        Tinker.done

      case ReceiveUnmarshalling(response) =>
        context.actorContext.log.info("Unmarshalling succeeding, replying now")
        replyTo !! response
        cc.foreach(_ !! response)
        Tinker.done
    }
  }
}
