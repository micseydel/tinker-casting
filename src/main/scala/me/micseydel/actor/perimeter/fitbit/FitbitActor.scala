package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.perimeter.fitbit.FitbitActor.{Request, RequestSleep, RequestSteps, TriggerAuthRefresh}
import me.micseydel.actor.perimeter.fitbit.FitbitModel.{Auth, AuthJsonProtocol, SleepReport}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.TypedJsonRef
import spray.json._

import java.io.FileNotFoundException
import java.time.LocalDate
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object FitbitActor {
  sealed trait Message

  sealed trait Request extends Message

  case class RequestSleep(replyTo: SpiritRef[SleepReport], day: LocalDate) extends Request

  case class RequestSteps(replyTo: SpiritRef[String], day: LocalDate) extends Request
  case class RequestHeartRate(replyTo: SpiritRef[String], day: LocalDate) extends Request
  case class RequestCalories(replyTo: SpiritRef[String], day: LocalDate) extends Request
  case class RequestActivities(replyTo: SpiritRef[String], day: LocalDate) extends Request
  case class RequestActiveTimes(replyTo: SpiritRef[String], day: LocalDate) extends Request

  private[fitbit] case class TriggerAuthRefresh(deferred: Request) extends Message

  private[fitbit] case class ReceiveAuthRefresh(auth: Try[Auth], deferred: Message) extends Message

  private[fitbit] case class FinishCooldown() extends Message

  //

  private val JsonFilename = "fitbit_auth"

  def apply(authorizationBasic: Option[String])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(20, 20, 20), "⌚️").initializedWithTypedJson(JsonFilename, AuthJsonProtocol.authFormat) { case (context, typedJsonRef) =>
    context.actorContext.log.info("Initializing Fitbit monitoring")

    authorizationBasic match {
      case Some(authBasic) =>
        typedJsonRef.read() match {
          case Failure(fileNotFoundException: FileNotFoundException) =>
            val msg = s"File $JsonFilename.json was missing, check the docs for Fitbit bootstrapping if it's desired"
            if (context.actorContext.log.isDebugEnabled) {
              context.actorContext.log.debug(msg, fileNotFoundException)
            } else {
              context.actorContext.log.warn(msg)
            }
            Tinker.done
          case Failure(exception) =>
            // FIXME: this blows up the spawner
            throw new RuntimeException("unexpected failure when accessing fitbit json during initialization", exception)

          case Success(auth) =>
            implicit val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
            implicit val jr: TypedJsonRef[Auth] = typedJsonRef

            steadyState(auth, new AuthBasic(authBasic))
        }
      case None =>
        Tinker.ignore
    }
  }

  // FIXME: states - how to test?

  private def steadyState(auth: Auth, authorizationBasic: AuthBasic)(implicit Tinker: Tinker, jsonRef: TypedJsonRef[Auth], timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext

    message match {
      case RequestSleep(replyTo, forDay) =>
        context.actorContext.log.info(s"Spawning anonymous fetcher for $forDay, with replyTo ${replyTo.actorPath}")
        context.castAnonymous(FitbitSleepFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case TriggerAuthRefresh(deferred) =>
        context.actorContext.log.info("Refreshing token...")
        context.castAnonymous(TokenRefresher(auth, context.self.underlying, deferred, authorizationBasic.basic))
        awaitingRefreshToken(authorizationBasic)

      case ReceiveAuthRefresh(triedAuth, deferred) =>
        val newAuth = triedAuth match {
          case Failure(exception) => throw exception
          case Success(au) => au
        }

        jsonRef.set(newAuth)
        context.actorContext.log.info("Token refreshed! Re-sending deferred message to self...")

        context.self !! deferred
        steadyState(newAuth, authorizationBasic)

      // requests

      case RequestSteps(replyTo, forDay) =>
        context.castAnonymous(FitbitStepsFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestHeartRate(replyTo, forDay) =>
        context.castAnonymous(FitbitHeartRateFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestCalories(replyTo, forDay) =>
        context.castAnonymous(FitbitCaloriesFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestActivities(replyTo, forDay) =>
        context.castAnonymous(FitbitActivitiesFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestActiveTimes(replyTo, forDay) =>
        context.castAnonymous(FitbitActiveZoneMinutesFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      //

      case FinishCooldown() =>
        context.actorContext.log.debug("Ignoring request to finish cooldown during steady state (no ongoing cooldown)")
        Tinker.steadily
    }
  }

  private def awaitingRefreshToken(authorizationBasic: AuthBasic)(implicit Tinker: Tinker, jsonRef: TypedJsonRef[Auth], timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context

    Behaviors.withStash[Message](10) { stash =>
      Tinker.receiveMessage {
        case request: Request =>
          stash.stash(request)
          Tinker.steadily

        case TriggerAuthRefresh(deferred) =>
          context.actorContext.log.debug("Awaiting refresh token so ignoring an auth refresh, stashed the deferred message")
          stash.stash(deferred)
          Tinker.steadily

        case ReceiveAuthRefresh(triedAuth, deferred) =>
          val newAuth = triedAuth match {
            case Failure(exception) => throw exception
            case Success(au) => au
          }

          jsonRef.set(newAuth)
          context.actorContext.log.info("Token refreshed! Re-sending deferred message to self...")

          context.self !! deferred
          stash.unstashAll(refreshTokenCooldown(newAuth, authorizationBasic))

        case FinishCooldown() =>
          context.actorContext.log.debug("Ignoring cooldown message sent during ")
          Tinker.steadily
      }
    }
  }


  private def refreshTokenCooldown(auth: Auth, authorizationBasic: AuthBasic)(implicit Tinker: Tinker, jsonRef: TypedJsonRef[Auth], timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.setup { context =>
    val RefreshTokenTimerKey = Some("RefreshTokenTimerKey")

    implicit val c: TinkerContext[_] = context

    val cooldownStart = context.system.clock.now()
    val cooldown = 5.minutes
    context.actorContext.log.info(s"Starting cooldown $cooldown")
    timeKeeper !! TimeKeeper.RemindMeIn(cooldown, context.self, FinishCooldown(), RefreshTokenTimerKey)

    Tinker.receiveMessage {
      case TriggerAuthRefresh(deferred) =>
        context.actorContext.log.info(s"Cooldown $cooldown started at $cooldownStart, processing deferred message NOW")
        context.self !! deferred
        Tinker.steadily

      case ReceiveAuthRefresh(triedAuth, deferred) =>
        val newAuth = triedAuth match {
          case Failure(exception) => throw exception
          case Success(au) => au
        }
        context.self !! deferred
        refreshTokenCooldown(newAuth, authorizationBasic)

      case FinishCooldown() =>
        context.actorContext.log.info(s"Finished $cooldown cooldown, switching back to steadyState")
        steadyState(auth, authorizationBasic)

      // requests

      case RequestSleep(replyTo, forDay) =>
        context.castAnonymous(FitbitSleepFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestSteps(replyTo, forDay) =>
        context.castAnonymous(FitbitStepsFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestHeartRate(replyTo, forDay) =>
        context.castAnonymous(FitbitHeartRateFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestCalories(replyTo, forDay) =>
        context.castAnonymous(FitbitCaloriesFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestActivities(replyTo, forDay) =>
        context.castAnonymous(FitbitActivitiesFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily

      case RequestActiveTimes(replyTo, forDay) =>
        context.castAnonymous(FitbitActiveZoneMinutesFetcher(auth, replyTo, forDay, context.self))
        Tinker.steadily
    }
  }

  //

  private class AuthBasic(val basic: String) extends AnyVal
}


object FitbitSleepFetcher {
  def apply(auth: Auth, replyTo: SpiritRef[SleepReport], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[FitbitFetcherHelper.Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Fetching Fitbit sleep for $forDay")
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)
    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.sleepReportFormat
    // https://dev.fitbit.com/build/reference/web-api/sleep/get-sleep-log-by-date/
    FitbitFetcherHelper(s"/1.2/user/-/sleep/date/$day.json", auth, replyTo, supervisor, RequestSleep(replyTo, forDay))
  }
}


object FitbitStepsFetcher {
  def apply(auth: Auth, replyTo: SpiritRef[String], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[FitbitFetcherHelper.Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Fetching Fitbit steps for $forDay")
    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.StepsFormat
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)

    // /1/user/-/activities/steps/date/[date]/[end-date]/[detail-level].json
    FitbitFetcherHelper(s"/1/user/-/activities/steps/date/$day/$day/15min.json", auth, replyTo, supervisor, RequestSteps(replyTo, forDay))
  }
}

object FitbitCaloriesFetcher {
  def apply(auth: Auth, replyTo: SpiritRef[String], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[FitbitFetcherHelper.Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Starting FitbitCaloriesFetcher for $forDay")
    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.StepsFormat
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)

    FitbitFetcherHelper(s"/1/user/-/activities/calories/date/$day/$day/15min.json", auth, replyTo, supervisor, RequestSteps(replyTo, forDay))
  }
}

object FitbitHeartRateFetcher {
  def apply(auth: Auth, replyTo: SpiritRef[String], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[FitbitFetcherHelper.Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Starting FitbitHeartRateFetcher for $forDay")
    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.StepsFormat
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)

    //https://dev.fitbit.com/build/reference/web-api/intraday/get-heartrate-intraday-by-date/
    FitbitFetcherHelper(s"/1/user/-/activities/heart/date/$day/$day/15min.json", auth, replyTo, supervisor, RequestSteps(replyTo, forDay))
  }
}

private[fitbit] object FetcherUtil {
  def fitbitHttpGetCall(urlPath: String, headers: List[HttpHeader])(implicit system: ActorSystem[HttpRequest]): Future[HttpResponse] = {
    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = s"https://api.fitbit.com$urlPath",
      headers = headers
    ))
  }
}



// also consider https://dev.fitbit.com/build/reference/web-api/intraday/
// - https://dev.fitbit.com/build/reference/web-api/intraday/get-br-intraday-by-date/
// - https://dev.fitbit.com/build/reference/web-api/intraday/get-hrv-intraday-by-date/
// - https://dev.fitbit.com/build/reference/web-api/intraday/get-spo2-intraday-by-date/
object FitbitFetcherHelper {
  sealed trait Message

  private case class ReceiveUnmarshaled(statusCode: StatusCode, responsePayload: String) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveException(throwable: Throwable) extends Message

  //

  def apply[T](path: String, auth: Auth, replyTo: SpiritRef[T], supervisor: SpiritRef[TriggerAuthRefresh], messageForRefresh: Request)(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[T]): Ability[Message] = Tinker.setup { context =>
    implicit val system: ActorSystem[HttpRequest] = context.system.actorSystem.asInstanceOf[ActorSystem[HttpRequest]]
    implicit val c: TinkerContext[_] = context
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext

    context.pipeToSelf(FetcherUtil.fitbitHttpGetCall(path, auth.httpHeadersBearer)) {
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
        context.actorContext.log.error(s"Failed Fitbit fetch for $path, will NOT be notifying ${replyTo.actorPath}", throwable)
        Tinker.done

      case ReceiveUnmarshaled(statusCode, responsePayload) =>
        if (statusCode == StatusCodes.OK) {
          Try(responsePayload.parseJson.convertTo[T]) match {
            case Failure(throwable) =>
              context.actorContext.log.error(s"Failed Fitbit fetch for $path, will NOT be notifying ${replyTo.actorPath}", throwable)

            case Success(sleepReport) =>
              replyTo !! sleepReport
          }
        } else if (statusCode == StatusCodes.Unauthorized) {
          context.actorContext.log.warn(s"Got a status 401, triggering an auth refresh with deferred sleep fetch")
          supervisor !! TriggerAuthRefresh(messageForRefresh)
        } else {
          context.actorContext.log.error(s"Unexpected status code: $statusCode (expected a 200, or 401 to trigger auth refresh); $responsePayload")
        }

        Tinker.done
    }
  }
}

object FitbitActivitiesFetcher {
  def apply(auth: Auth, replyTo: SpiritRef[String], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[FitbitFetcherHelper.Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Starting FitbitActivitiesFetcher for $forDay")

    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.StepsFormat
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)

    // https://dev.fitbit.com/build/reference/web-api/activity-timeseries/get-activity-timeseries-by-date/
        FitbitFetcherHelper(s"/1/user/-/activities/steps/date/$day/1d.json", auth, replyTo, supervisor, RequestSteps(replyTo, forDay))
  }
}

object FitbitActiveZoneMinutesFetcher {
  def apply(auth: Auth, replyTo: SpiritRef[String], forDay: LocalDate, supervisor: SpiritRef[TriggerAuthRefresh])(implicit Tinker: Tinker): Ability[FitbitFetcherHelper.Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Starting FitbitActiveZoneMinutesFetcher for day $forDay")
    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.StepsFormat
    val day = TimeUtil.localDateTimeToISO8601Date(forDay)

        // https://dev.fitbit.com/build/reference/web-api/intraday/get-azm-intraday-by-date/
        FitbitFetcherHelper(s"/1/user/-/activities/active-zone-minutes/date/$day/1d/15min.json", auth, replyTo, supervisor, RequestSteps(replyTo, forDay))
  }
}
