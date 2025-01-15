package me.micseydel.dsl.cast

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.{Http, model}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, StatusCode}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.dsl.cast.NetworkPerimeterActor.HttpResponse

import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object NetworkPerimeterActor {
  // mailbox

  sealed trait Message

  case class DoHttpPost(url: String, message: String, replyTo: ActorRef[HttpResponse], key: String) extends Message

  // model (for replies)

  // FIXME: remove Akka StatusCode reference
  case class HttpResponse(key: String, result: Try[(StatusCode, String)])

  // behavior

  def apply()(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case DoHttpPost(url, message, replyTo, key) =>
        context.log.info(s"Spawning a HttpPostChaperone for key $key")
        // stops itself once its work is done
        context.spawnAnonymous(HttpPostChaperone(url, message, replyTo, key))
        Behaviors.same
    }
  }
}

private object HttpPostChaperone {
  sealed trait Message

  private case class FollowupOnHttpPost(response: Try[model.HttpResponse], replyTo: ActorRef[HttpResponse], key: String) extends Message

  private case class FollowupOnUnmarshaling(response: HttpResponse, replyTo: ActorRef[HttpResponse]) extends Message

  def apply(url: String, message: String, replyTo: ActorRef[HttpResponse], key: String)(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.setup { context =>
    implicit val actorSystem: ActorSystem[_] = context.system

    val payload = HttpEntity(ContentTypes.`application/x-www-form-urlencoded`, message)

    context.log.debug(s"Sending post request for key $key")
    context.pipeToSelf(Http().singleRequest(HttpRequest(
      method = HttpMethods.POST,
      uri = url,
      entity = payload
    )))(FollowupOnHttpPost(_, replyTo, key))

    Behaviors.receiveMessage {
      case FollowupOnHttpPost(response, replyTo, key) =>
        context.log.debug(s"Unmarshaling for key $key")
        context.pipeToSelf(response match {
          case Success(httpResponse) =>
            Unmarshal(httpResponse.entity).to[String].map { resultString =>
              (httpResponse.status, resultString)
            }
          case Failure(exception) => Future.failed(exception)
        }) { result =>
          FollowupOnUnmarshaling(HttpResponse(key, result), replyTo)
        }

        Behaviors.same

      case FollowupOnUnmarshaling(httpResponse, replyTo) =>
        replyTo ! httpResponse
        context.log.debug(s"Replied for key ${httpResponse.key}, stopping")
        Behaviors.stopped
    }
  }
}
