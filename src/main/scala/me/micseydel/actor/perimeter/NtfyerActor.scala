package me.micseydel.actor.perimeter

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse, StatusCode}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.StreamTcpException
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{Tinker, TinkerContext, Tinkerer}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object NtfyerActor {
  // mailbox

  sealed trait Message

  case class DoNotify(key: String, message: String) extends Message

  // key -> response
  private case class ReceiveNetworkReply(response: (String, Try[HttpResponse])) extends Message
  private case class ReceiveUnmarshaling(payload: (String, StatusCode, Try[String])) extends Message

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(0, 255, 255), "📧").setup { context =>
    implicit val s: ActorSystem[?] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    Tinker.receiveMessage {
      case DoNotify(key, message) =>
        makeRequest(key, message)
        context.actorContext.log.info(s"Sending HTTP POST request to ntfy")
        Behaviors.same

      case ReceiveNetworkReply((key, response)) =>
        response match {
          case Failure(exception:   StreamTcpException) =>
            context.actorContext.log.error(s"NtfyHttpCallResult failed for key $key; is there an internet connection?", exception)
          case Failure(exception) =>
            context.actorContext.log.warn("ntfy call failed", exception)
          case Success(httpResponse: HttpResponse) =>
            if (httpResponse.status == StatusCode.int2StatusCode(200)) {
              httpResponse.discardEntityBytes()
              context.actorContext.log.debug(s"Success 200, discarding entity bytes")
            } else {
              context.actorContext.log.warn(s"Unexpected status code ${httpResponse.status.intValue()}, ${httpResponse.status.reason()}; unmarshaling the payload for details now...")
              context.pipeToSelf(Unmarshal(httpResponse.entity).to[String])(unmarshaling => (ReceiveUnmarshaling((key, httpResponse.status, unmarshaling))))
            }
        }

        Behaviors.same

      case ReceiveUnmarshaling((key, httpStatusCode, responseString)) =>
        responseString match {
          case Failure(exception) => context.actorContext.log.error(s"Got ${httpStatusCode.intValue()} on $key but failed to unmarshal payload (as a string!)", exception)
          case Success(reason) => context.actorContext.log.warn(s"Got ${httpStatusCode.intValue()} on $key: $reason")
        }

        Behaviors.same
    }
  }

  //

  private def makeRequest(key: String, message: String)(implicit context: TinkerContext[Message], actorSystem: ActorSystem[?]): Unit = {
    val uri = s"https://ntfy.sh/$key" // FIXME: allow for custom ntfy hosts
    context.pipeToSelf(request(uri, message))(networkReply => ReceiveNetworkReply(key -> networkReply))
  }

  private def request(uri: String, message: String)(implicit s: ActorSystem[?]): Future[HttpResponse] = {
    val payload = HttpEntity(ContentTypes.`application/x-www-form-urlencoded`, message)
    Http().singleRequest(HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = payload
    ))
  }
}
