package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse, ResponseEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext, Tinkerer}
import me.micseydel.dsl.Tinker.Ability
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

import scala.annotation.unused
import scala.concurrent.Future
import scala.util.{Failure, Success}

object WyzeActor {
  sealed trait Message

  case class ReceiveWyzeAPIResponse(response: WyzeAPIResponse) extends Message

  private val NoteName = "Wyze tinkering"

  def apply(wyzeUri: String)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor.rgb(0, 255, 255), "🔌").setup { _ =>
    Tinker.initializedWithNote(NoteName) { (context, noteRef) =>
      val request = HttpRequest(
        method = HttpMethods.GET,
        uri = s"http://$wyzeUri/wyze/device"
      )

      import WyzeAPIJsonProtocol.wyzeAPIResponseResultJsonFormat

      @unused // fetches and then dies
      val devicesFetcher: SpiritRef[HttpFetchAndUnmarshall.Message[WyzeAPIResponse]] =
        context.castAnonymous(HttpFetchAndUnmarshall(request, context.messageAdapter(ReceiveWyzeAPIResponse), FailedWyzeAPIResponse))

      Tinker.withMessages {
        case ReceiveWyzeAPIResponse(response) =>
          response match {
            case FailedWyzeAPIResponse(exception) =>
              context.actorContext.log.error("Failed to get device list", exception)
              Tinker.steadily
            case WyzeAPIResponseResult(wyzeDeviceList) =>
              context.actorContext.log.info(s"Received device list, writing to $NoteName a total of ${wyzeDeviceList.size} devices")

              noteRef.setMarkdown {
                val devices = wyzeDeviceList.map {
                  case OutdoorPlug(ip, mac, nickname, photosensitive_switch, push_switch, rssi, ssid) =>
                    s"- $nickname: $mac"
                }.mkString("\n") + "\n"

                s"""- generated ${context.system.clock.now()}
                   |# Devices
                   |
                   |$devices""".stripMargin
              }

              Tinker.steadily
          }
      }
    }
  }

  // model

  sealed trait WyzeAPIResponse

  case class FailedWyzeAPIResponse(exception: Throwable) extends WyzeAPIResponse

  case class WyzeAPIResponseResult(wyze_device_list: List[WyzeDevice]) extends WyzeAPIResponse

  // api

  sealed trait WyzeDevice

  case class OutdoorPlug(ip: String, mac: String, nickname: String, photosensitive_switch: Boolean, push_switch: Int, rssi: String, ssid: String) extends WyzeDevice

  object WyzeAPIJsonProtocol extends DefaultJsonProtocol {
    implicit val outdoorPlugJsonFormat: RootJsonFormat[OutdoorPlug] = jsonFormat7(OutdoorPlug)

    implicit object WyzeDeviceJsonFormat extends RootJsonFormat[WyzeDevice] {
      def write(m: WyzeDevice): JsValue = {
        m match {
          case op: OutdoorPlug =>
            JsObject(op.toJson.asJsObject.fields + ("type" -> JsString("OutdoorPlug")))
        }
      }

      def read(value: JsValue): WyzeDevice = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("OutdoorPlug")) => value.convertTo[OutdoorPlug]
          case other => throw DeserializationException(s"""Unknown type $other; expected Seq(JsString("OutdoorPlug"))""")
        }
      }
    }

    implicit val wyzeAPIResponseResultJsonFormat: RootJsonFormat[WyzeAPIResponseResult] = jsonFormat1(WyzeAPIResponseResult)
  }
}


object HttpFetchAndUnmarshall {
  sealed trait Message[T]

  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]

  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]

  private case class ReceiveUnmarshalling[T](reply: T) extends Message[T]

  // behavior

  def apply[T, E <: T, R <: T](httpRequest: HttpRequest, replyTo: SpiritRef[T], failureWrapper: Throwable => E)(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[R]): Ability[Message[T]] = Tinker.setup { context =>
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

        context.pipeToSelf(Unmarshal(httpResponse.entity).to[R]) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(chatResponse) => ReceiveUnmarshalling(chatResponse)
        }
        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.error("Something went wrong with HttpResponse", exception)
        replyTo !! failureWrapper(exception)
        Tinker.steadily

      case ReceiveUnmarshalling(response) =>
        context.actorContext.log.info("Unmarshalling succeeding, replying now")
        replyTo !! response
        Tinker.steadily // FIXME
    }
  }
}
