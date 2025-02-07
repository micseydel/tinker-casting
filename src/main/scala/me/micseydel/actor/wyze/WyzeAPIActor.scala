package me.micseydel.actor.wyze

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.wyze.WyzePlugModel.WyzeAPIJsonProtocol.wyzePlugAPIResultJsonFormat
import me.micseydel.actor.wyze.WyzePlugModel.{WyzePlugAPIResponse, WyzePlugAPIResponseFailed, WyzePlugAPIResult}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext, Tinkerer}
import spray.json._

import scala.annotation.unused
import scala.concurrent.Future
import scala.util.{Failure, Success}

private[wyze] object WyzeAPIActor {
  sealed trait Message

  final case class GetDevices(replyTo: SpiritRef[WyzePlugAPIResponse]) extends Message

  final case class SetPlugIsOn(deviceMac: String, isOn: Boolean) extends Message


  def apply(wyzeUri: String)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor.rgb(0, 255, 255), "☁️").setup { context =>
    Tinker.receiveMessage {
      case GetDevices(replyTo) =>
        val request = HttpRequest(
          method = HttpMethods.GET,
          uri = s"http://$wyzeUri/wyze/plug"
        )

        @unused // fetches and then dies
        val devicesFetcher = {
          val behavior = HttpFetchAndUnmarshall[WyzePlugAPIResponse, WyzePlugAPIResponseFailed, WyzePlugAPIResult](
            request, replyTo, WyzePlugAPIResponseFailed
          )
          context.castAnonymous(behavior)
        }

        Tinker.steadily

      case SetPlugIsOn(deviceMac, isOn) =>
        context.actorContext.log.info(s"Setting MAC $deviceMac on state to $isOn")
        val jsPayload = JsObject(
          "device_mac" -> JsString(deviceMac),
          "is_on" -> JsBoolean(isOn)
        )

        val request = HttpRequest(
          method = HttpMethods.POST,
          uri = s"http://$wyzeUri/wyze/plug",
          entity = HttpEntity(ContentTypes.`application/json`, jsPayload.toString)
        )

        context.actorContext.log.info(s"About to make request $request")

        // call is for the side effect
        context.castAnonymous(HttpFetchUnmarshallAndLog(request))

        Tinker.steadily
    }
  }
}


object WyzePlugModel {
  sealed trait WyzePlugAPIResponse

  final case class WyzePlugAPIResponseFailed(throwable: Throwable) extends WyzePlugAPIResponse

  final case class WyzePlug(ip: String, mac: String, nickname: String, is_on: Option[Boolean])

  final case class WyzePlugAPIResult(wyze_plug_list: List[WyzePlug]) extends WyzePlugAPIResponse

  object WyzeAPIJsonProtocol extends DefaultJsonProtocol {
    implicit val wyzePlugJsonFormat: RootJsonFormat[WyzePlug] = jsonFormat4(WyzePlug)
    implicit val wyzePlugAPIResultJsonFormat: RootJsonFormat[WyzePlugAPIResult] = jsonFormat1(WyzePlugAPIResult)
  }
}


private object HttpFetchAndUnmarshall {
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

    Tinker.receiveMessage {
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
        replyTo !! failureWrapper(exception)
        Tinker.done

      case ReceiveUnmarshalling(response) =>
        context.actorContext.log.info("Unmarshalling succeeding, replying now")
        replyTo !! response
        Tinker.done
    }
  }
}

private object HttpFetchUnmarshallAndLog {
  sealed trait Message[T]

  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]

  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]

  def apply[T, E <: T, R <: T](httpRequest: HttpRequest)(implicit Tinker: Tinker): Ability[Message[T]] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem

    context.actorContext.log.info("Making request...")
    context.pipeToSelf(Http().singleRequest(httpRequest)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.receiveMessage {
      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received HttpResponse, discarding rather than unmarshalling")
        httpResponse.discardEntityBytes()
        Tinker.done

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.error("Something went wrong with HttpResponse", exception)
        Tinker.done
    }
  }
}
