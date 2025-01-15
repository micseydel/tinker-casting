package me.micseydel.actor.inactive

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.vault.persistence.TypedJsonRef
import org.slf4j.Logger
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.time.LocalDate
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object GoogleCalendarActor {
  // Authorization: Bearer
  // https://www.googleapis.com/calendar/v3/users/me/calendarList/calendarId

  sealed trait Message

  case class RequestSleep(replyTo: SpiritRef[Nothing], day: LocalDate) extends Message

  private case class FitbitHttpCallResult(msg: String, exception: Option[Throwable]) extends Message

  private case class TriggerAuthRefresh(deferred: Message) extends Message

  private case class ReceiveAuthRefresh(auth: Try[Auth], deferred: Message) extends Message

  //

  private case class Auth(access_token: String, refresh_token: String) {
    def httpHeadersBearer: List[HttpHeader] = List(RawHeader("Authorization", s"Bearer $access_token"))
  }

  private object AuthJsonProtocol extends DefaultJsonProtocol {
    implicit val authFormat: RootJsonFormat[Auth] = jsonFormat2(Auth)
  }

  //

  def apply()(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] =
    Tinker.initializedWithTypedJson("fitbit_auth", AuthJsonProtocol.authFormat) { case (context, typedJsonRef) =>
      context.actorContext.log.info("Initializing Fitbit monitoring")
      val auth: Auth = typedJsonRef.read() match {
        case Failure(exception) =>
          throw new RuntimeException("unexpected failure when accessing fitbit json during initialization", exception)
        case Success(auth) =>
          auth
      }

      ability(auth, typedJsonRef, 0)
    }

  private def ability(auth: Auth, jsonRef: TypedJsonRef[Auth], consecutiveFailures: Int)(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val system: ActorSystem[HttpRequest] = context.system.actorSystem.asInstanceOf[ActorSystem[HttpRequest]]
    implicit val c: TinkerContext[_] = context
    implicit val log: Logger = context.actorContext.log
    context.actorContext.log.info(s"Current consecutiveFailures $consecutiveFailures")

    message match {
      case RequestSleep(replyTo, forDay) =>
        fitbitHttpGetCall(forDay, auth.httpHeadersBearer).onComplete {
          case Success(httpResponse) =>
            val resultString: Future[String] = Unmarshal(httpResponse.entity).to[String]

          //            context.pipeToSelf(resultString) {
          //              case Failure(throwable) =>
          //                FitbitHttpCallResult("oops", Some(throwable))
          //              case Success(string) =>
          //                if (httpResponse.status == StatusCode.int2StatusCode(200)) {
          //                  Try(string.parseJson.convertTo[SleepReport]) match {
          //                    case Success(result) =>
          //                      replyTo !! result
          //                      FitbitHttpCallResult(s"success $string extracted and sent to ${replyTo.actorPath}", None)
          //                    case Failure(throwable) =>
          //                      FitbitHttpCallResult(s"failure $string NOT extracted NOR sent to ${replyTo.actorPath}", Some(throwable))
          //                  }
          //                } else if (httpResponse.status == StatusCode.int2StatusCode(401)) {
          //                  context.log.debug(s"Got a 401 response, triggering refresh")
          //                  TriggerAuthRefresh(message)
          //                } else {
          //                  FitbitHttpCallResult(s"success $string sent to ${replyTo.actorPath}", None)
          //                }
          //            }
          case Failure(throwable) =>
            context.self !! FitbitHttpCallResult("oops", Some(throwable)) // FIXME: the originating code doesn't send it back to self!
        }

        Tinker.steadily

      case TriggerAuthRefresh(deferred) =>
        context.actorContext.log.info("Refreshing token...")
        //        context.castAnonymous(TokenRefresher(auth, context.self.underlying, deferred))
        Tinker.steadily

      case ReceiveAuthRefresh(triedAuth, deferred) =>
        val newAuth = triedAuth match {
          case Failure(exception) => throw exception
          case Success(au) => au
        }

        jsonRef.set(newAuth)
        context.actorContext.log.info("Token refreshed! Re-sending deferred message to self...")

        context.self !! deferred
        ability(newAuth, jsonRef, consecutiveFailures)

      case FitbitHttpCallResult(msg, maybeException) =>
        maybeException match {
          case None =>
            context.actorContext.log.debug("Successful FitbitHttpCallResult!")
            ability(auth, jsonRef, 0)
          case Some(exception) =>
            val newFailureCount = consecutiveFailures + 1
            context.actorContext.log.error(s"Fitbit fetch failed ($newFailureCount): $msg", exception)
            if (newFailureCount >= 3) {
              context.actorContext.log.error(s"Stopping, $newFailureCount failures so preventing a loop")
              Tinker.done
            } else {
              ability(auth, jsonRef, newFailureCount)
            }
        }
    }
  }

  // util

  private def fitbitHttpGetCall(forDay: LocalDate, headers: List[HttpHeader])(implicit log: Logger, system: ActorSystem[HttpRequest]): Future[HttpResponse] = {

    //    val authorizationHeader = headers.RawHeader("Authorization", "Bearer ya29.a0AXooCgsKZgLrOxNpxsme6kSNVCue-BQE3hPlVbtCEuFYDa9sosc2PSNp3ffMA4CAdfuLbX6zBzydH3Q0t2FXr-lrL1MDoN7wlEVOauSfDoI9WWsPZGKOezl676hXetvDwKG5D-jraodnTFPoj8tEvMiKcnuqheJW4VcbaCgYKARwSARISFQHGX2MicmAmM40cKBsRz1oMkItYUw0171")

    //    val request = HttpRequest(
    //      method = HttpMethods.GET,
    //      uri = "https://www.googleapis.com/calendar/v3/users/me/calendarList",
    //      headers = List(authorizationHeader)
    //    )
    //
    //    Http().singleRequest(request)
    ???
  }

  private val MyId = "6MX849"
}
