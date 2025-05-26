package me.micseydel.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{Tinker, TinkerContext}
import me.micseydel.model.RasaResult
import me.micseydel.model.RasaResultProtocol.rasaResultFormat
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object RasaActor {
  sealed trait Message
  final case class GetRasaResult(string: String, model: String, replyTo: ActorRef[RasaResult]) extends Message
  private final case class ReceiveRasaResult(result: RasaResult, replyTo: ActorRef[RasaResult]) extends Message
  private final case class RasaRequestFailed(message: String, throwable: Throwable) extends Message

  def apply(host: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    import context.actorContext.system
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
    implicit val tc: TinkerContext[_] = context

    Tinker.receiveMessage {
      case GetRasaResult(string, model, replyTo) =>
        val payload = Map("string" -> string)
        val responseFuture: Future[HttpResponse] = Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET,
            uri = s"http://$host/entity_extraction/$model",
            entity = HttpEntity(ContentTypes.`application/json`, payload.toJson.toString)
          )
        )

        responseFuture.onComplete {
          case Success(response) =>
            val rawStringFuture = Unmarshal(response.entity).to[String]
            val triedRasaFuture: Future[RasaResult] = rawStringFuture.flatMap { raw =>
              Future.fromTry(Try(raw.parseJson.convertTo[RasaResult])
                .recoverWith {
                  case ucce: UnsupportedContentTypeException =>
                    Failure(new RuntimeException(s"Failed to deserialize raw $raw", ucce))
                })
            }
            context.pipeToSelf(triedRasaFuture) {
              case Success(response) =>
                ReceiveRasaResult(response, replyTo)
              case Failure(exception) =>
                RasaRequestFailed(s"payload $payload failed", exception)
            }
          case Failure(exception) =>
            context.self !! RasaRequestFailed("Future failed", exception)
        }

        Behaviors.same

      case ReceiveRasaResult(result, replyTo) =>
        replyTo ! result
        Behaviors.same
      case RasaRequestFailed(message, exception) =>
        context.actorContext.log.error(message, exception)
        Behaviors.same
    }
  }
}
