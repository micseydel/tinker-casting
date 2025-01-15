package me.micseydel.actor.ollama

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.ollama.OllamaModel.Models
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import spray.json.RootJsonFormat

import scala.util.{Failure, Success}

object FetchModelsActor {
  sealed trait Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable) extends Message

  private case class ReceiveUnmarshalling(models: Models) extends Message

  def apply(replyTo: SpiritRef[Models])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[_] = context

    val uri = "http://localhost:11434/api/tags"
    context.actorContext.log.info(s"Making request... to $uri")

    context.pipeToSelf(Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = uri
    ))) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.withMessages {
      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received request response...")

        implicit val format: RootJsonFormat[Models] = OllamaJsonFormat.receiveModelsFormat
        context.pipeToSelf(Unmarshal(httpResponse.entity).to[Models]) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) => ReceiveUnmarshalling(models)
        }

        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.debug(s"Failed to connect to Ollama", exception)
        context.actorContext.log.warn(s"Failed to connect to Ollama; if Ollama isn't install and setup, this is normal and expected")
        Tinker.steadily

      case ReceiveUnmarshalling(models) =>
        context.actorContext.log.info("Unmarshalling successful, replying and wrapping")
        replyTo !! models
        Tinker.done
    }
  }
}
