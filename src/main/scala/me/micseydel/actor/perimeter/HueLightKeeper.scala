package me.micseydel.actor.perimeter

import akka.actor.{Scheduler, typed}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCode
import akka.util.Timeout
import cats.data.NonEmptyList
import me.micseydel.NoOp
import me.micseydel.actor.inactive.TemplateBasicEmptyModelActorOrSpirit.Message
import me.micseydel.actor.notifications.ChimeActor
import me.micseydel.actor.notifications.NotificationCenterManager.{Chime, JustSideEffect}
import me.micseydel.actor.perimeter.HueControl.{Command, HueConfig, LogLightKeeperFailure, LogLightKeeperResponseInfo}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.model.LightStates.{AnotherGreenLight, BlueLight, RedLight, RelaxedLight}
import me.micseydel.model.{Light, LightState}

import java.time.Duration
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.concurrent.duration.{DurationDouble, DurationInt}
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

  def apply(light: Light, hueConfig: HueConfig)(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] =
    setup(light, hueConfig)

  private def setup(light: Light, hueConfig: HueConfig)(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] = Tinkerer(rgb(230, 230, 230), "💡").setup { context =>
    implicit val c: TinkerContext[_] = context

    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

    implicit val hc: HueConfig = hueConfig
    val api: SpiritRef[HueLightKeeperAPIActor.Message] = context.cast(HueLightKeeperAPIActor(light), "HueLightKeeperAPIActor")

    // initialize empty, but should get a value shortly
    api !! HueLightKeeperAPIActor.GetLightState(context.messageAdapter(ReceiveLightState).underlying)
    behavior(None)(httpExecutionContext, context.system.actorSystem, Tinker, api, light, timeKeeper)
  }

  // states / behaviors

  private def behavior(lastSetState: Option[LightState])(implicit httpExecutionContext: ExecutionContextExecutorService, actorSystem: ActorSystem[Nothing], Tinker: Tinker, api: SpiritRef[HueLightKeeperAPIActor.Message], light: Light, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case GetLightState(replyTo) =>
        // anytime we grab the light state, we update our self too
        api !! HueLightKeeperAPIActor.GetLightState(replyTo, context.messageAdapter(ReceiveLightState).underlying)
        Tinker.steadily

      case SetBrightness(brightnessPct) =>
        lastSetState match {
          case None =>
            context.system.notifier !! JustSideEffect(Chime(ChimeActor.Warning(ChimeActor.Material)))
            context.actorContext.log.warn("Not setting brightness because no state is cached; this means it wasn't set explicitly and fetching failed")
            Tinker.steadily
          case Some(state) =>
            val newState = state.withLightPct(brightnessPct)
            context.actorContext.log.warn(s"Updating brightnessPct->$brightnessPct, state $state -> $newState")
            api !! HueLightKeeperAPIActor.SetLightState(newState)
            behavior(Some(newState))
        }

      case FlashTheLight =>
        lastSetState match {
          case None =>
            context.system.notifier !! JustSideEffect(Chime(ChimeActor.Warning(ChimeActor.Material)))
            context.actorContext.log.warn("Not flashing the lights because no state is cached; this means it wasn't set explicitly and fetching failed")
          case Some(state) =>
            api !! HueLightKeeperAPIActor.SetLightState(RelaxedLight)
            timeKeeper !! TimeKeeper.RemindMeIn(3.seconds, context.self, HueLightKeeper.SetLight(state), None)
        }

        Tinker.steadily

      case SetLight(lightState) =>
        context.actorContext.log.info(s"Setting light $light to $lightState")
        api !! HueLightKeeperAPIActor.SetLightState(lightState)
        behavior(Some(lightState))

      case DoALightShow() =>
        lastSetState match {
          case None =>
            context.system.notifier !! JustSideEffect(Chime(ChimeActor.Warning(ChimeActor.Material)))
            context.actorContext.log.warn(s"Not doing a light show because no state is cached; this means it wasn't set explicitly and fetching failed")
          case Some(state) =>
            timeKeeper !! TimeKeeper.RemindMeIn(1.2.seconds, api, HueLightKeeperAPIActor.SetLightState(RedLight), None)
            timeKeeper !! TimeKeeper.RemindMeIn(2.4.seconds, api, HueLightKeeperAPIActor.SetLightState(BlueLight), None)
            timeKeeper !! TimeKeeper.RemindMeIn(3.6.seconds, api, HueLightKeeperAPIActor.SetLightState(AnotherGreenLight), None)
            timeKeeper !! TimeKeeper.RemindMeIn(5.seconds, api, HueLightKeeperAPIActor.SetLightState(state), None)
        }

        Tinker.steadily

      case ReceiveLightState(lightState) =>
        context.actorContext.log.warn(s"Setting light ${light.lightId} to state $lightState")
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
    implicit val tc: TinkerContext[_] = context
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
    implicit val s: ActorSystem[_] = context.system.actorSystem
    Tinker.receiveMessage {
      case GetLightState(replyTo) =>
        context.pipeToSelf(HTTPHelpers.HueApi.getLightState(light)) {
          case Failure(exception) => ReceiveExceptionFromHttp(exception)
          case Success(lightState) => ReceiveFromHttp(lightState, replyTo.toList)
        }
        Tinker.steadily

      case SetLightState(lightState) =>
        context.pipeToSelf(HTTPHelpers.HueApi.putLightState2(light, lightState))  {
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
}
