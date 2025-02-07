package me.micseydel.actor.ollama

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.ollama.OllamaModel.{ChatResponse, ChatResponseFailure}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.vault.VaultKeeper
import spray.json._

import java.util.Base64
import scala.concurrent.Future
import scala.util.matching.Regex
import scala.util.{Failure, Success}

object FetchChatResponseActor {

  // mailbox

  sealed trait Message

  // behavior and regex

  private val WikiLinkPattern: Regex = """\[\[([\w-]+\.(png|jpg|jpeg|gif|bmp))\|?.*?\]\]""".r

  def apply(prompt: String, model: String, replyTo: SpiritRef[ChatResponse])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    // check if we need to collect and b64-encode attachments
    val maybeSpecialization: Option[Ability[ImageFetchChatResponseActor.Message]] = if (model == "llava") {
      val imageAttachments = WikiLinkPattern.findAllMatchIn(prompt).map(_.group(1)).toList
      if (imageAttachments.nonEmpty) {
        Some(ImageFetchChatResponseActor(prompt, model, imageAttachments, replyTo))
      } else {
        None
      }
    } else {
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

        val uri = OllamaActor.GenerateUri
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

  def apply(prompt: String, model: String, attachmentNames: List[String], replyTo: SpiritRef[ChatResponse])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>

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
          OllamaActor.GenerateUri,
          payload,
          replyTo,
          ChatResponseFailure.apply
        ))

        Tinker.steadily

      case Receive(Left(msg)) =>
        val msg = "Fetch from disk failed"
        context.actorContext.log.error("Fetch from disk failed")
        replyTo !! ChatResponseFailure(msg)
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

    context.pipeToSelf(Http().singleRequest(HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = HttpEntity(ContentTypes.`application/json`, payload.toString)
    ))) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.receiveMessage {
      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received HttpResponse, beginning unmarshalling process")
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
