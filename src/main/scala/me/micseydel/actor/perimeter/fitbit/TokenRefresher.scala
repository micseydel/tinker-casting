package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.perimeter.fitbit.FitbitModel.{Auth, AuthJsonProtocol}
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import spray.json.DefaultJsonProtocol.{StringJsonFormat, mapFormat}
import spray.json._
import AuthJsonProtocol.authFormat
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

import scala.concurrent.ExecutionContextExecutorService
import scala.util.{Failure, Success, Try}

private[fitbit] object TokenRefresher {
  sealed trait Message

  private case class ReceiveHttpResponse(response: Try[HttpResponse]) extends Message

  private case class FinishUnmarshalling(maybeAuth: Try[String]) extends Message

  def apply(auth: Auth, fitbitActor: ActorRef[FitbitActor.Message], deferred: FitbitActor.Message, authorizationBasic: String)(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem

    val authorizationHeader = headers.RawHeader("Authorization", s"Basic $authorizationBasic")

    val formData = FormData("grant_type" -> "refresh_token", "refresh_token" -> auth.refresh_token).toEntity

    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = "https://api.fitbit.com/oauth2/token",
      headers = List(authorizationHeader),
      entity = formData
    )

    /*
     * curl -i -X POST \
     * https://api.fitbit.com/oauth2/token \
     * -H "Authorization: Basic "  \
     * -H "Content-Type: application/x-www-form-urlencoded"  \
     * --data "grant_type=refresh_token"  \
     * --data "refresh_token="
     */
    context.pipeToSelf(Http().singleRequest(
//      HttpRequest(
//      method = HttpMethods.POST,
//      uri = "https://api.fitbit.com/oauth2/token",
//      headers = auth.httpHeadersBasic,
//      entity = HttpEntity(ContentTypes.`application/json`, Map(
//        "grant_type" -> "refresh_token",
//        "refresh_token" -> auth.refresh_token
//      ).toJson.toString)
//    )
      request
    ))(ReceiveHttpResponse)

    Tinker.withMessages {
      case ReceiveHttpResponse(response) =>
        response match {
          case Success(httpResponse) =>
            context.actorContext.log.info("Unmarshalling...")
            context.pipeToSelf(Unmarshal(httpResponse.entity).to[String])(FinishUnmarshalling)
            Tinker.steadily
          case Failure(exception) =>
            throw exception
        }

      case FinishUnmarshalling(maybeAuth) =>
        context.actorContext.log.info("Done unmarshalling auth, sending back to FitbitActor, then this actor is done :)")
        fitbitActor ! FitbitActor.ReceiveAuthRefresh(maybeAuth.flatMap { authString =>
          Try(authString.parseJson.convertTo[Auth]) match {
            case Failure(exception) => Failure(new RuntimeException(s"Failure deserializing string: $authString", exception))
            case success@Success(_) => success
          }
        }, deferred)
        Tinker.done
    }
  }
}
