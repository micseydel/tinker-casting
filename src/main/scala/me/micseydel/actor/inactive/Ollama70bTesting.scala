package me.micseydel.actor.inactive

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.settings.{ClientConnectionSettings, ConnectionPoolSettings}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.ollama.OllamaModel.{ChatResponse, ChatResponseFailure}
import me.micseydel.dsl.Tinker.Ability
import spray.json._

import scala.annotation.unused
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object Ollama70bTesting {

  sealed trait Message

  private final case class ReceiveResponse(chatResponse: ChatResponse) extends Message

  def behavior(): Behavior[Message] = Behaviors.setup { context =>
    val hostAndPort = "localhost:11434"

    context.spawnAnonymous(FetchChatResponseActorOllama70b(hostAndPort, "Why are there moons?", "llama3:70b", context.messageAdapter(ReceiveResponse)))

    Behaviors.receiveMessage {
      case ReceiveResponse(chatResponse) =>
        val msg = s"Received $chatResponse"
        println(msg)
        context.log.warn(msg)
        Behaviors.same
    }
  }

  def main(args: Array[String]): Unit = {
    @unused
    val system: ActorSystem[Message] =
      ActorSystem(behavior(), "hello")
  }
}

private object FetchChatResponseActorOllama70b {
  sealed trait Message

  def apply(hostAndPort: String, prompt: String, model: String, replyTo: ActorRef[ChatResponse]): Ability[Message] = Behaviors.setup { context =>
    val uri = s"http://$hostAndPort/api/generate"

    context.log.info("Starting HTTP request...")

    val payload = JsObject(
      "model" -> JsString(model),
      "prompt" -> JsString(prompt),
      "stream" -> JsBoolean(false)
    )
    context.log.info(s"Making request... to $uri with model $model and ${prompt.length} length prompt")

    import me.micseydel.actor.ollama.OllamaJsonFormat.chatResponseResultFormat
    context.spawnAnonymous(GenericFetchAndUnmarshall(uri, payload, replyTo, ChatResponseFailure.apply))

    Behaviors.receiveMessage { _ =>
      Behaviors.same
    }
  }
}

private object GenericFetchAndUnmarshall {
  sealed trait Message[T]

  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]

  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]

  private case class ReceiveUnmarshalling[T](reply: T) extends Message[T]

  // behavior

  def apply[T, E <: T, R <: T](uri: String, payload: JsObject, replyTo: ActorRef[T], failureWrapper: Throwable => E)(implicit jsonFormat: RootJsonFormat[R]): Behavior[Message[T]] = Behaviors.setup { context =>
    implicit val s: ActorSystem[_] = context.system

    /////
    val connectionSettings = ClientConnectionSettings(context.system)
      .withIdleTimeout(10.minutes)
    val poolSettings = ConnectionPoolSettings(context.system)
      .withConnectionSettings(connectionSettings)
      .withResponseEntitySubscriptionTimeout(10.minutes)
    /////

    context.pipeToSelf(Http().singleRequest(
      HttpRequest(
        method = HttpMethods.POST,
        uri = uri,
        entity = HttpEntity(ContentTypes.`application/json`, payload.toString)
      ),
      settings = poolSettings // this was the fix
    )) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Behaviors.receiveMessage {
      case ReceiveHttpResponse(httpResponse) =>
        context.log.info("Received HttpResponse, beginning unmarshalling process")
        context.pipeToSelf(Unmarshal(httpResponse.entity).to[String]) {
          case Failure(exception) =>
            ReceiveFailedHttpResponse(new RuntimeException("Trying new code...", exception))
          case Success(chatResponse) =>
            Try(chatResponse.parseJson.convertTo[R]) match {
              case Failure(exception) => ReceiveFailedHttpResponse(new RuntimeException(s"Unexpected chat response: $chatResponse", exception))
              case Success(value) => ReceiveUnmarshalling(value)
            }
        }
        Behaviors.same

      case ReceiveFailedHttpResponse(exception) =>
        context.log.error("Something went wrong with HttpResponse", exception)
        replyTo ! failureWrapper(exception)
        Behaviors.same

      case ReceiveUnmarshalling(response) =>
        replyTo ! response
        Behaviors.same
    }
  }
}
