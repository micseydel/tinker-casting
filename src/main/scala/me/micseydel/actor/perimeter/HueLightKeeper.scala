package me.micseydel.actor.perimeter

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.after
import cats.data.NonEmptyList
import me.micseydel.NoOp
import me.micseydel.actor.notifications.ChimeActor
import me.micseydel.actor.notifications.NotificationCenterManager.{Chime, JustSideEffect}
import me.micseydel.actor.perimeter.HueControl.HueConfig
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.model.LightStates.{AnotherGreenLight, BlueLight, RedLight, RelaxedLight}
import me.micseydel.model.{Light, LightState}
import spray.json._

import scala.concurrent.duration.{DurationDouble, DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContextExecutorService, Future, TimeoutException}
import scala.util.{Failure, Success}

object HueLightKeeper {
  sealed trait Message

  // to change the lights
  sealed trait Command extends Message

  case class GetLightState(replyTo: ActorRef[LightState]) extends Command

  case class SetLight(lightState: LightState) extends Command

  /**
   * @param brightnessPct [0, 100]
   */
  case class SetBrightness(brightnessPct: Int) extends Command

  case object FlashTheLight extends Command

  case class DoALightShow() extends Command

  private case class ReceiveLightState(lightState: LightState) extends Message

  def apply(light: Light, hueConfig: HueConfig)(implicit Tinker: Tinker): Ability[Message] =
    setup(light, hueConfig)

  private def setup(light: Light, hueConfig: HueConfig)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(230, 230, 230), "💡").setup { context =>
    implicit val c: TinkerContext[_] = context

    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

    implicit val hc: HueConfig = hueConfig
    val api: SpiritRef[HueLightKeeperAPIActor.Message] = context.cast(HueLightKeeperAPIActor(light), "HueLightKeeperAPIActor")

    // initialize empty, but should get a value shortly
    api !! HueLightKeeperAPIActor.GetLightState(context.messageAdapter(ReceiveLightState).underlying)
    behavior(None)(context.system.actorSystem, Tinker, api, light, timeKeeper)
  }

  // states / behaviors

  private def behavior(cachedLightState: Option[LightState])(implicit actorSystem: ActorSystem[Nothing], Tinker: Tinker, apiForThisLight: SpiritRef[HueLightKeeperAPIActor.Message], light: Light, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case GetLightState(replyTo) =>
        // anytime we grab the light state, we update our self too
        apiForThisLight !! HueLightKeeperAPIActor.GetLightState(replyTo, context.messageAdapter(ReceiveLightState).underlying)
        Tinker.steadily

      case SetBrightness(brightnessPct) =>
        cachedLightState match {
          case None =>
            context.system.notifier !! JustSideEffect(Chime(ChimeActor.Warning(ChimeActor.Material)))
            context.actorContext.log.warn("Not setting brightness because no state is cached; this means it wasn't set explicitly and fetching failed")
            Tinker.steadily
          case Some(state) =>
            val newState = state.withLightPct(brightnessPct)
            context.actorContext.log.debug(s"Updating brightnessPct->$brightnessPct, state $state -> $newState")
            apiForThisLight !! HueLightKeeperAPIActor.SetLightState(newState)
            behavior(Some(newState))
        }

      case FlashTheLight =>
        cachedLightState match {
          case None =>
            context.system.notifier !! JustSideEffect(Chime(ChimeActor.Warning(ChimeActor.Material)))
            context.actorContext.log.warn("Not flashing the lights because no state is cached; this means it wasn't set explicitly and fetching failed")
          case Some(state) =>
            apiForThisLight !! HueLightKeeperAPIActor.SetLightState(RelaxedLight)
            timeKeeper !! TimeKeeper.RemindMeIn(3.seconds, context.self, HueLightKeeper.SetLight(state), None)
        }

        Tinker.steadily

      case SetLight(lightState) =>
        context.actorContext.log.info(s"Setting light $light to $lightState")
        apiForThisLight !! HueLightKeeperAPIActor.SetLightState(lightState)
        behavior(Some(lightState))

      case DoALightShow() =>
        cachedLightState match {
          case None =>
            context.system.notifier !! JustSideEffect(Chime(ChimeActor.Warning(ChimeActor.Material)))
            context.actorContext.log.warn(s"Not doing a light show because no state is cached; this means it wasn't set explicitly and fetching failed")
          case Some(state) =>
            timeKeeper !! TimeKeeper.RemindMeIn(1.2.seconds, apiForThisLight, HueLightKeeperAPIActor.SetLightState(RedLight), None)
            timeKeeper !! TimeKeeper.RemindMeIn(2.4.seconds, apiForThisLight, HueLightKeeperAPIActor.SetLightState(BlueLight), None)
            timeKeeper !! TimeKeeper.RemindMeIn(3.6.seconds, apiForThisLight, HueLightKeeperAPIActor.SetLightState(AnotherGreenLight), None)
            timeKeeper !! TimeKeeper.RemindMeIn(5.seconds, apiForThisLight, HueLightKeeperAPIActor.SetLightState(state), None)
        }

        Tinker.steadily

      case ReceiveLightState(lightState) =>
        cachedLightState match {
          case Some(old) =>
            context.actorContext.log.debug(s"Updating ${light.lightId} from $old to $lightState")
          case None =>
            context.actorContext.log.info(s"Setting light ${light.lightId} to initial state $lightState")
        }
        behavior(Some(lightState))
    }
  }
}


private object HueLightKeeperAPIActor {
  sealed trait Message

  final case class GetLightState(replyTo: NonEmptyList[ActorRef[LightState]]) extends Message
  final case class SetLightState(lightState: LightState) extends Message

  object GetLightState {
    def apply(replyTo: ActorRef[LightState]): GetLightState = GetLightState(NonEmptyList(replyTo, Nil))
    def apply(replyTo1: ActorRef[LightState], replyTo2: ActorRef[LightState]): GetLightState = GetLightState(NonEmptyList(replyTo1, List(replyTo2)))
  }

  private case class ReceiveFromHttp(lightState: LightState, replyTo: List[ActorRef[LightState]]) extends Message
  private case class ReceiveExceptionFromHttp(throwable: Throwable) extends Message
  private case class ReceiveLightPutFromHttp(result: Either[(StatusCode, String), NoOp.type]) extends Message
  // FIXME: add a timeout message and have a timekeeper spawn off a message

  def apply(light: Light)(implicit Tinker: Tinker, hueConfig: HueConfig): Ability[Message] = Tinker.setup { context =>
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
    implicit val s: ActorSystem[_] = context.system.actorSystem
    Tinker.receiveMessage {
      case GetLightState(replyTo) =>
        context.pipeToSelf(getLightState(light)) {
          case Failure(exception) => ReceiveExceptionFromHttp(exception)
          case Success(lightState) => ReceiveFromHttp(lightState, replyTo.toList)
        }
        Tinker.steadily

      case SetLightState(lightState) =>
        context.pipeToSelf(putLightState(light, lightState))  {
          case Failure(exception) => ReceiveExceptionFromHttp(exception)
          case Success(lightPutResult) => ReceiveLightPutFromHttp(lightPutResult)
        }
        Tinker.steadily


      case ReceiveFromHttp(lightState, replyTo) =>
        replyTo.foreach(_ ! lightState)
        Tinker.steadily
      case ReceiveExceptionFromHttp(throwable) =>
        context.actorContext.log.warn(s"HTTP problem with $light", throwable)
        Tinker.steadily
      case ReceiveLightPutFromHttp(result) =>
        result match {
          case Left(err) =>
            context.actorContext.log.warn(s"Something went wrong setting light state:\n$err")
          case Right(NoOp) =>
        }
        Tinker.steadily
    }
  }

  import LightStateJsonProtocol.lightStateFormat

  private def getLightState(light: Light, timeoutDuration: FiniteDuration = 10.seconds)
                   (implicit system: ActorSystem[Nothing], httpExecutionContext: ExecutionContextExecutorService, hueConfig: HueConfig): Future[LightState] = {
    val lightStateResponseFuture: Future[HttpResponse] = Http().singleRequest(HttpRequest(uri = s"http://${hueConfig.ip}/api/${hueConfig.username}/lights/${light.lightId}"))

    val timeoutFuture: Future[Nothing] = after(duration = timeoutDuration, using = system.classicSystem.scheduler) {
      Future.failed(new TimeoutException("Request timed out"))
    }

    val responseFuture: Future[HttpResponse] = Future.firstCompletedOf(Seq(lightStateResponseFuture, timeoutFuture))

    responseFuture.transform {
      case success@Success(_) =>
        success // In case of success, just forward the response
      case Failure(exception) =>
        // needs testing, this can be safely removed if later I haven't seen the following in the logs
        //    Make sure to read the response `entity` body or call `entity.discardBytes()` on it -- in case you deal with `HttpResponse`, use the shortcut `response.discardEntityBytes()`.
        lightStateResponseFuture.foreach(response => response.entity.discardBytes()) // Discard the bytes in case of failure
        Failure(exception) // Forward the failure
    }.flatMap { response =>
      Unmarshal(response.entity).to[String]
    }.map { body =>
      val jsonAst = body.parseJson
      val lightState = jsonAst.asJsObject.getFields("state") match {
        case Seq(state) => state.convertTo[LightState]
        case _ => throw DeserializationException("LightState expected")
      }
      lightState
    }
  }

  private def putLightState(light: Light, lightState: LightState)
                   (implicit system: ActorSystem[Nothing], httpExecutionContext: ExecutionContextExecutorService, hueConfig: HueConfig): Future[Either[(StatusCode, String), NoOp.type]] = {

    // scale from a percentage to out of 255
    val serialized = lightState.copy(bri = (lightState.bri * 255 / 100.0).toInt, on = lightState.bri > 0).toJson
    val request = HttpRequest(
      method = HttpMethods.PUT,
      uri = s"http://${hueConfig.ip}/api/${hueConfig.username}/lights/${light.lightId}/state",
      entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, serialized.toString.getBytes),
    )

    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    responseFuture.flatMap { httpResponse =>
      if (httpResponse.status == StatusCodes.OK) {
        httpResponse.entity.discardBytes()
        Future.successful(Right(NoOp))
      } else {
        Unmarshal(httpResponse.entity).to[String].map(s => Left((httpResponse.status, s)))
      }
    }
  }

  private object LightStateJsonProtocol extends DefaultJsonProtocol {
    implicit val lightStateFormat: RootJsonFormat[LightState] = jsonFormat4(LightState)
  }
}
