package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.perimeter.fitbit.FitbitModel.{Auth, AuthJsonProtocol, SleepReport}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.TypedJsonRef
import org.slf4j.Logger
import spray.json._

import java.io.FileNotFoundException
import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object FitbitActor {
  sealed trait Message

  case class RequestSleep(replyTo: SpiritRef[SleepReport], day: LocalDate) extends Message

  private case class FitbitHttpCallResult(msg: String, exception: Option[Throwable]) extends Message

  private[fitbit] case class TriggerAuthRefresh(deferred: Message) extends Message

  private[fitbit] case class ReceiveAuthRefresh(auth: Try[Auth], deferred: Message) extends Message

  def apply(authorizationBasic: Option[String])(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] = {
    val jsonFilename = "fitbit_auth"
    Tinkerer(rgb(20, 20, 20), "⌚️").initializedWithTypedJson(jsonFilename, AuthJsonProtocol.authFormat) { case (context, typedJsonRef) =>
      context.actorContext.log.info("Initializing Fitbit monitoring")

      typedJsonRef.read() match {
        case Failure(fileNotFoundException: FileNotFoundException) =>
          val msg = s"File $jsonFilename.json was missing, check the docs for Fitbit bootstrapping if it's desired"
          if (context.actorContext.log.isDebugEnabled) {
            context.actorContext.log.debug(msg, fileNotFoundException)
          } else {
            context.actorContext.log.warn(msg)
          }
          Tinker.done
        case Failure(exception) =>
          // this blows up the spawner
          throw new RuntimeException("unexpected failure when accessing fitbit json during initialization", exception)

        case Success(auth) =>
          ability(auth, typedJsonRef, 0, authorizationBasic)
      }
    }
  }

  def ability(auth: Auth, jsonRef: TypedJsonRef[Auth], consecutiveFailures: Int, maybeAuthorizationBasic: Option[String])(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val system: ActorSystem[HttpRequest] = context.system.actorSystem.asInstanceOf[ActorSystem[HttpRequest]]
    implicit val c: TinkerContext[_] = context
    implicit val log: Logger = context.actorContext.log
    context.actorContext.log.info(s"Current consecutiveFailures $consecutiveFailures")

    message match {
      case RequestSleep(replyTo, forDay) =>
        fitbitHttpGetCall(forDay, auth.httpHeadersBearer).onComplete {
          case Success(httpResponse) =>
            val resultString: Future[String] = Unmarshal(httpResponse.entity).to[String]

            context.pipeToSelf(resultString) {
              case Failure(throwable) =>
                FitbitHttpCallResult("oops", Some(throwable))
              case Success(string) =>
                if (httpResponse.status == StatusCode.int2StatusCode(200)) {
                  import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.sleepReportFormat
                  Try(string.parseJson.convertTo[SleepReport]) match {
                    case Success(result) =>
                      replyTo !! result
                      FitbitHttpCallResult(s"success $string extracted and sent to ${replyTo.actorPath}", None)
                    case Failure(throwable) =>
                      FitbitHttpCallResult(s"failure $string NOT extracted NOR sent to ${replyTo.actorPath}", Some(throwable))
                  }
                } else if (httpResponse.status == StatusCode.int2StatusCode(401)) {
                  context.actorContext.log.debug(s"Got a 401 response, triggering refresh")
                  TriggerAuthRefresh(message)
                } else {
                  FitbitHttpCallResult(s"success $string sent to ${replyTo.actorPath}", None)
                }
            }
          case Failure(throwable) =>
            FitbitHttpCallResult("oops", Some(throwable))
        }

        Tinker.steadily

      case TriggerAuthRefresh(deferred) =>
        context.actorContext.log.info("Refreshing token...")
        maybeAuthorizationBasic match {
          case Some(authorizationBasic) =>
            context.castAnonymous(TokenRefresher(auth, context.self.underlying, deferred, authorizationBasic))
          case None =>
            context.actorContext.log.warn(s"No auth basic in the config (fitbit.authorizationBasic), not triggering refresh")
        }
        Tinker.steadily

      case ReceiveAuthRefresh(triedAuth, deferred) =>
        val newAuth = triedAuth match {
          case Failure(exception) => throw exception
          case Success(au) => au
        }

        jsonRef.set(newAuth)
        context.actorContext.log.info("Token refreshed! Re-sending deferred message to self...")

        context.self !! deferred
        ability(newAuth, jsonRef, consecutiveFailures, maybeAuthorizationBasic)

      case FitbitHttpCallResult(msg, maybeException) =>
        maybeException match {
          case None =>
            context.actorContext.log.debug("Successful FitbitHttpCallResult!")
            ability(auth, jsonRef, 0, maybeAuthorizationBasic)
          case Some(exception) =>
            val newFailureCount = consecutiveFailures + 1
            context.actorContext.log.error(s"Fitbit fetch failed ($newFailureCount): $msg", exception)
            if (newFailureCount >= 3) {
              context.actorContext.log.error(s"Stopping, $newFailureCount failures so preventing a loop")
              Tinker.done
            } else {
              ability(auth, jsonRef, newFailureCount, maybeAuthorizationBasic)
            }
        }
    }
  }

  // util

  private def fitbitHttpGetCall(forDay: LocalDate, headers: List[HttpHeader])(implicit log: Logger, system: ActorSystem[HttpRequest]): Future[HttpResponse] = {
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)
    // https://dev.fitbit.com/build/reference/web-api/sleep/get-sleep-log-by-date/
    val url = s"https://api.fitbit.com/1.2/user/-/sleep/date/$day.json"

    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = url,
      headers = headers
    ))
  }
}
