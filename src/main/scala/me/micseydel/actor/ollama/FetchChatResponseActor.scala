package me.micseydel.actor.ollama

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.settings.{ClientConnectionSettings, ConnectionPoolSettings}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.ollama.OllamaModel.{ChatResponse, ChatResponseFailure}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.vault.VaultKeeper
import spray.json._

import java.util.Base64
import scala.concurrent.duration.DurationInt
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object FetchChatResponseActor {

  // mailbox

  sealed trait Message

  // behavior and regex

  private val WikiLinkPattern: Regex = """\[\[(.+\.(png|jpg|jpeg|gif|bmp))\|?.*?\]\]""".r

  def apply(hostAndPort: String, prompt: String, model: String, replyTo: SpiritRef[ChatResponse])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    val uri = s"http://$hostAndPort/api/generate"

    // check if we need to collect and b64-encode attachments
    // FIXME: should support more than llava
    val maybeSpecialization: Option[Ability[ImageFetchChatResponseActor.Message]] = if (model.contains("llava")) {
      val imageAttachments = WikiLinkPattern.findAllMatchIn(prompt).map(_.group(1)).toList
      if (imageAttachments.nonEmpty) {
        context.actorContext.log.info(s"Detected multimodel (image) model with ${imageAttachments.size} attachments")
        Some(ImageFetchChatResponseActor(uri, prompt, model, imageAttachments, replyTo))
      } else {
        context.actorContext.log.info("Detected multimodel (image) model but no images detected")
        None
      }
    } else {
      context.actorContext.log.info("Using a regular text-based model")
      None
    }

    maybeSpecialization match {
      case Some(specialization) =>
        context.actorContext.log.info("Found llava with attachments, handing off to specialized image handling actor")
        context.castAnonymous(specialization)
        Tinker.steadily // FIXME
        Tinker.ignore // FIXME

      case None =>
        context.actorContext.log.info("Starting HTTP request...")

        val payload = JsObject(
          "model" -> JsString(model),
          "prompt" -> JsString(prompt),
          "stream" -> JsBoolean(false)
        )
        context.actorContext.log.info(s"Making request... to $uri with model $model and ${prompt.length} length prompt")

        import me.micseydel.actor.ollama.OllamaJsonFormat.chatResponseResultFormat
        context.castAnonymous(EXPERIMENTHttpFetchAndUnmarshall(uri, payload, replyTo, ChatResponseFailure.apply))

        Tinker.steadily // FIXME
        Tinker.ignore // FIXME
    }
  }
}

private object ImageFetchChatResponseActor {
  sealed trait Message

  private case class Receive(response: Either[String, List[Array[Byte]]]) extends Message

  def apply(uri: String, prompt: String, model: String, attachmentNames: List[String], replyTo: SpiritRef[ChatResponse])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>

    implicit val c: TinkerContext[_] = context

    context.actorContext.log.info(s"Requesting ${attachmentNames.size} attachments...")
    context.system.vaultKeeper !! VaultKeeper.RequestAttachmentsContents(attachmentNames, context.messageAdapter(Receive).underlying)

    Tinker.receiveMessage {
      case Receive(Right(contents)) =>
        context.actorContext.log.info(s"Received ${contents.size} attachments, making Ollama request...")
        val payload = JsObject(
          "model" -> JsString(model),
          "prompt" -> JsString(prompt),
          "stream" -> JsBoolean(false),
          "images" -> JsArray(contents.map(attachment => JsString(b64encode(attachment))).toVector)
        )

        import me.micseydel.actor.ollama.OllamaJsonFormat.chatResponseResultFormat
        context.castAnonymous(EXPERIMENTHttpFetchAndUnmarshall(
          uri,
          payload,
          replyTo,
          ChatResponseFailure.apply
        ))

        Tinker.steadily

      case Receive(Left(msg)) =>
        val message = s"Fetch from disk failed: $msg"
        context.actorContext.log.error(message)
        replyTo !! ChatResponseFailure(message)
        Tinker.steadily // FIXME
    }
  }

  private def b64encode(bytes: Array[Byte]): String = {
    Base64.getEncoder.encodeToString(bytes)
  }
}

private object EXPERIMENTHttpFetchAndUnmarshall {
  sealed trait Message[T]

  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]

  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]

  private case class ReceiveUnmarshalling[T](reply: T) extends Message[T]

  // behavior

  def apply[T, E <: T, R <: T](uri: String, payload: JsObject, replyTo: SpiritRef[T], failureWrapper: Throwable => E)(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[R]): Ability[Message[T]] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    implicit val s: ActorSystem[_] = context.system.actorSystem

    // this is essential for 70b models on interesting prompts, but may be a poor default
    val connectionSettings = ClientConnectionSettings(context.system.actorSystem)
      .withIdleTimeout(10.minutes)

    val poolSettings = ConnectionPoolSettings(context.system.actorSystem)
      .withConnectionSettings(connectionSettings)
      .withResponseEntitySubscriptionTimeout(10.minutes)

    context.pipeToSelf(Http().singleRequest(HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = HttpEntity(ContentTypes.`application/json`, payload.toString)
    ), settings = poolSettings)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.receiveMessage {
      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received HttpResponse, beginning unmarshalling process")
        context.pipeToSelf(Unmarshal(httpResponse.entity).to[String]) {
          case Failure(exception) =>
            ReceiveFailedHttpResponse(new RuntimeException("Unmarshalling to string failed", exception))
          case Success(chatResponse) =>
            Try(chatResponse.parseJson.convertTo[R]) match {
              case Failure(exception) => ReceiveFailedHttpResponse(new RuntimeException(s"Unexpected chat response: $chatResponse", exception))
              case Success(value) => ReceiveUnmarshalling(value)
            }
        }
        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.error("Something went wrong with HttpResponse", exception)
        replyTo !! failureWrapper(exception)
        Tinker.steadily

      case ReceiveUnmarshalling(response) =>
        context.actorContext.log.debug("Unmarshalling succeeding, replying now")
        replyTo !! response
        Tinker.steadily // FIXME
    }
  }
}




///

//private object EXPERIMENTHttpFetchAndUnmarshallGeneric {
//  sealed trait Message[T]
//
//  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]
//
//  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]
//
//  private case class ReceiveUnmarshalling[T](reply: T) extends Message[T]
//
//  // behavior
//
////  def apply[T](uri: String, payload: JsObject, replyTo: SpiritRef[T])(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[T]): Ability[Message[T]] = Tinker.setup { context =>
////    //    def apply[T](uri: String, payload: JsObject, replyTo: SpiritRef[Either[Throwable, T]])(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[T]): Ability[Message[T]] = Tinker.setup { context =>
////    implicit val c: TinkerContext[_] = context
////    implicit val s: ActorSystem[_] = context.system.actorSystem
////
////    val request = Http().singleRequest(HttpRequest(
////      method = HttpMethods.POST,
////      uri = uri,
////      entity = HttpEntity(ContentTypes.`application/json`, payload.toString)
////    ))
////
////    context.pipeToSelf(request) {
////      case Failure(exception) => ReceiveFailedHttpResponse(exception)
////      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
////    }
////
////    Tinker.withMessages {
////      case ReceiveHttpResponse(httpResponse) =>
////        context.actorContext.log.info("Received HttpResponse, beginning unmarshalling process")
////        context.pipeToSelf(Unmarshal(httpResponse.entity).to[T]) {
////          case Failure(exception) => ReceiveFailedHttpResponse(exception)
////          case Success(chatResponse) => ReceiveUnmarshalling(chatResponse)
////        }
////        Tinker.steadily
////
////      case ReceiveFailedHttpResponse(exception) =>
////        context.actorContext.log.error("Something went wrong with HttpResponse", exception)
////        replyTo !! Left(exception)
////        Tinker.steadily
////
////      case ReceiveUnmarshalling(response) =>
////        context.actorContext.log.info("Unmarshalling succeeding, replying now")
////        replyTo !! Right(response)
////        Tinker.steadily // FIXME
////    }
////  }
//}
