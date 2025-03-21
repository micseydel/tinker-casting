package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.perimeter.fitbit.FitbitActor.{RequestSleep, TriggerAuthRefresh}
import me.micseydel.actor.perimeter.fitbit.FitbitModel.{Auth, AuthJsonProtocol, SleepReport}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.TypedJsonRef
import org.slf4j.Logger
import spray.json._

import java.io.FileNotFoundException
import java.time.LocalDate
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object FitbitActor {
  sealed trait Message

  sealed trait Request extends Message

  case class RequestSleep(replyTo: SpiritRef[SleepReport], day: LocalDate) extends Request

//  case class RequestSteps(replyTo: SpiritRef[(LocalDate, Int)], day: LocalDate) extends Request

  private[fitbit] sealed trait Refresh extends Message

  private[fitbit] case class TriggerAuthRefresh(deferred: Request) extends Refresh

  private[fitbit] case class ReceiveAuthRefresh(auth: Try[Auth], deferred: Message) extends Refresh

  def apply(authorizationBasic: Option[String])(implicit Tinker: Tinker): Ability[Message] = {
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

  // FIXME: states - awaitingRequests, refreshing

  private def ability(auth: Auth, jsonRef: TypedJsonRef[Auth], consecutiveFailures: Int, maybeAuthorizationBasic: Option[String])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val system: ActorSystem[HttpRequest] = context.system.actorSystem.asInstanceOf[ActorSystem[HttpRequest]]
    implicit val c: TinkerContext[_] = context
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
    implicit val log: Logger = context.actorContext.log
    context.actorContext.log.info(s"Current consecutiveFailures $consecutiveFailures")

    message match {
      case RequestSleep(replyTo, forDay) =>
        context.actorContext.log.info(s"Spawning anonymous fetcher for $forDay, with replyTo ${replyTo.actorPath}")
        context.castAnonymous(FitbitSleepFetcher(auth, replyTo, forDay, context.self))
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
    }
  }
}


object FitbitSleepFetcher {
  sealed trait Message

  private case class ReceiveUnmarshaled(statusCode: StatusCode, responsePayload: String) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveException(throwable: Throwable) extends Message

  //

  def apply(auth: Auth, replyTo: SpiritRef[SleepReport], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val system: ActorSystem[HttpRequest] = context.system.actorSystem.asInstanceOf[ActorSystem[HttpRequest]]
    implicit val c: TinkerContext[_] = context
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
    implicit val log: Logger = context.actorContext.log

    val day = TimeUtil.localDateTimeToISO8601Date(forDay)

    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.sleepReportFormat

    // https://dev.fitbit.com/build/reference/web-api/sleep/get-sleep-log-by-date/
    context.pipeToSelf(FetcherUtil.fitbitHttpGetCall(s"/1.2/user/-/sleep/date/$day.json", auth.httpHeadersBearer)) {
      case Failure(exception) => ReceiveException(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.receiveMessage {
      case ReceiveHttpResponse(httpResponse) =>
        context.pipeToSelf(Unmarshal(httpResponse.entity).to[String]) {
          case Failure(exception) =>
            ReceiveException(exception)
          case Success(unmarshalled) =>
            ReceiveUnmarshaled(httpResponse.status, unmarshalled)
        }

        context.actorContext.log.debug("Just sent HTTP request, will followup when the reply is received")

        Tinker.steadily

      case ReceiveException(throwable) =>
        context.actorContext.log.error(s"Failed to fetch sleep for $forDay, will NOT be notifying ${replyTo.actorPath}", throwable)
        Tinker.done

      case ReceiveUnmarshaled(statusCode, responsePayload) =>
        if (statusCode == StatusCodes.OK) {
          Try(responsePayload.parseJson.convertTo[SleepReport]) match {
            case Failure(throwable) =>
              context.actorContext.log.error(s"Failed to fetch sleep for $forDay, will NOT be notifying ${replyTo.actorPath}", throwable)

            case Success(sleepReport) =>
              replyTo !! sleepReport
          }
        } else if (statusCode == StatusCodes.Unauthorized) {
          context.actorContext.log.warn(s"Got a status 401, triggering an auth refresh with deferred sleep fetch")
          supervisor !! TriggerAuthRefresh(RequestSleep(replyTo, forDay))
        } else {
          context.actorContext.log.error(s"Unexpected status code: $statusCode (expected a 200, or 401 to trigger auth refres)")
        }

        Tinker.done
    }
  }
}


//object FitbitStepsFetcher {
//  def apply(auth: Auth)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
//    implicit val system: ActorSystem[HttpRequest] = context.system.actorSystem.asInstanceOf[ActorSystem[HttpRequest]]
//    implicit val c: TinkerContext[_] = context
//    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
//    implicit val log: Logger = context.actorContext.log
//    FetcherUtil.fitbitHttpGetCall("/1/user/-/activities/tracker/steps.json", auth.httpHeadersBearer)
//    Tinker.unhandled
//  }
//}

private[fitbit] object FetcherUtil {
  def fitbitHttpGetCall(urlPath: String, headers: List[HttpHeader])(implicit log: Logger, system: ActorSystem[HttpRequest]): Future[HttpResponse] = {
    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = s"https://api.fitbit.com$urlPath",
      headers = headers
    ))
  }
}
