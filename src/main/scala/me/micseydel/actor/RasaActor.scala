package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import me.micseydel.model.RasaResult
import me.micseydel.model.RasaResultProtocol.rasaResultFormat
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object RasaActor {
  sealed trait Message
  final case class GetRasaResult(string: String, replyTo: ActorRef[RasaResult]) extends Message
  private final case class ReceiveRasaResult(result: RasaResult, replyTo: ActorRef[RasaResult]) extends Message
  private final case class RasaRequestFailed(message: String, throwable: Throwable) extends Message

  def apply(host: String)(implicit httpExecutionContext: ExecutionContext): Behavior[Message] = Behaviors.setup { context =>
    import context.system
    Behaviors.receiveMessage {
      case GetRasaResult(string, replyTo) =>
        val payload = Map("string" -> string)
        val responseFuture: Future[HttpResponse] = Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET,
            uri = s"http://$host/rasa/intent",
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
            context.self ! RasaRequestFailed("Future failed", exception)
        }

        Behaviors.same

      case ReceiveRasaResult(result, replyTo) =>
        replyTo ! result
        Behaviors.same
      case RasaRequestFailed(message, exception) =>
        context.log.error(message, exception)
        Behaviors.same
    }
  }
}
