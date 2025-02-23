package me.micseydel.actor.perimeter

import akka.actor.{Scheduler, typed}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
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

  // non-public messages

  // for handling the Hue Bridge response
  private sealed trait StateUpdate extends Message

  private case class LogHueResponseInfo(message: String) extends StateUpdate

  private case class LogHueFailure(message: String, throwable: Option[Throwable] = None) extends StateUpdate

  private case class HueApiTimeout(msg: String) extends StateUpdate



  //  case class FlashTheLight(light: Light) extends Message
  //  case class TurnOffLight(light: Light) extends Message

  def apply(light: Light, hueConfig: HueConfig)(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] =
    setup(light, hueConfig)

  private def setup(light: Light, hueConfig: HueConfig)(implicit httpExecutionContext: ExecutionContextExecutorService, Tinker: Tinker): Ability[Message] = Tinkerer(rgb(230, 230, 230), "💡").setup { context =>
    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

    // it's always waiting for a command, or waiting to hear back from Hue
    waitingForCommand(light, timeKeeper, None)(httpExecutionContext, context.system.actorSystem, Tinker, hueConfig)
  }

  // states / behaviors

  private def waitingForCommand(light: Light, timeKeeper: SpiritRef[TimeKeeper.Message], lastSetState: Option[LightState])(implicit httpExecutionContext: ExecutionContextExecutorService, actorSystem: ActorSystem[Nothing], Tinker: Tinker, hueConfig: HueConfig): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case stateUpdate: StateUpdate =>
        context.actorContext.log.debug(s"Was waiting for command, did not expect Hue status update $stateUpdate")
        Tinker.steadily

      case GetLightState(replyTo) =>
        val fut = HTTPHelpers.HueApi.getLightState(light)
        context.pipeToSelf(fut) {
          case Success(capturedLightState) =>
            replyTo ! capturedLightState
            LogHueResponseInfo(s"Captured $capturedLightState for light $light, replying to $replyTo")
          case Failure(throwable) =>
            LogHueFailure("Failed to get light state", Some(throwable))
        }

        Tinker.steadily

      case SetBrightness(brightnessPct) =>
        val fut = HTTPHelpers.HueApi.getLightState(light)
        context.pipeToSelf(fut) {
          case Success(capturedLightState) =>
            SetLight(capturedLightState.copy(bri = brightnessPct, on = brightnessPct > 0))
          case Failure(throwable) =>
            LogHueFailure("Failed to get light state", Some(throwable))
        }

        Tinker.steadily

      case FlashTheLight =>
        implicit val timeout: Timeout = Timeout.create(Duration.ofMillis(30.seconds.toMillis))
        implicit val s: typed.Scheduler = context.system.actorSystem.scheduler
        val log = context.actorContext.log
        context.pipeToSelf(context.self.underlying.ask(HueLightKeeper.GetLightState).map { capturedLightState =>
          // FIXME super hacky!
          context.self !! HueLightKeeper.SetLight(RelaxedLight)
          timeKeeper !! TimeKeeper.RemindMeIn(3.seconds, context.self, HueLightKeeper.SetLight(capturedLightState), None)
          LogHueResponseInfo(s"Captured $capturedLightState, setting light then resetting in 3 seconds")
        }) {
          case Success(captures) =>
            val msg = s"(a) Captured to restore after light show: $captures; lastSetState=$lastSetState"
            log.info(msg)
            LogHueResponseInfo(msg)
          case Failure(exception) =>
            LogHueFailure("Something went wrong collecting all the current light values", Some(exception))
        }

        Tinker.steadily

      case SetLight(lightState) =>
        context.actorContext.log.info(s"Setting light $light to $lightState")
        val fut = HTTPHelpers.HueApi.putLightState(light, lightState)
        context.pipeToSelf(fut) {
          case Success(success) =>
            if (success) {
              LogHueResponseInfo(s"Put $lightState succeeded")
            } else {
              LogHueFailure("Failed to set light state")
            }
          case Failure(throwable) =>
            LogHueFailure("Failed to set light state", Some(throwable))
        }

        val timeoutMessage = HueApiTimeout(s"Failed to set $light to $lightState")
        timeKeeper !! TimeKeeper.RemindMeIn(15.seconds, context.self, timeoutMessage, Some(HueControl))

        waitingForHueResponse(light, timeKeeper, Some(lightState))

      case DoALightShow() =>
        implicit val timeout: Timeout = Timeout.create(Duration.ofMillis(20.seconds.toMillis))
        implicit val s: typed.Scheduler = context.system.actorSystem.scheduler
        val log = context.actorContext.log
        context.pipeToSelf(context.self.underlying.ask(HueLightKeeper.GetLightState).map { capturedLightState =>
          log.info(s"Captured $capturedLightState, doing a light show before restoring")
          // FIXME super hacky!
          timeKeeper !! TimeKeeper.RemindMeIn(1.2.seconds, context.self, HueLightKeeper.SetLight(RedLight), None)
          timeKeeper !! TimeKeeper.RemindMeIn(2.4.seconds, context.self, HueLightKeeper.SetLight(BlueLight), None)
          timeKeeper !! TimeKeeper.RemindMeIn(3.6.seconds, context.self, HueLightKeeper.SetLight(AnotherGreenLight), None)
          timeKeeper !! TimeKeeper.RemindMeIn(5.seconds, context.self, HueLightKeeper.SetLight(capturedLightState), None)
          capturedLightState
        }) {
          case Success(capturedLightState) =>
            val msg = s"(b) Captured to restore after light show: $capturedLightState; lastSetState=$lastSetState"
            log.info(msg)
            LogHueResponseInfo(msg)
          case Failure(exception) =>
            val msg = "Something went wrong collecting all the current light values"
            log.warn(msg, exception)
            LogHueFailure(msg, Some(exception))
        }

        Tinker.steadily
    }
  }

  private def waitingForHueResponse(light: Light, timeKeeper: SpiritRef[TimeKeeper.Message],  lastSetState: Option[LightState])(implicit httpExecutionContext: ExecutionContextExecutorService, actorSystem: ActorSystem[Nothing], Tinker: Tinker, hueConfig: HueConfig): Ability[Message] = Tinker.setup { tinkerContext =>
    Behaviors.withStash(20) { stash =>
      Behaviors.receive { case (context, message) =>
        implicit val c: TinkerContext[_] = tinkerContext
        message match {
          case stateUpdate: StateUpdate =>
            stateUpdate match {
              case LogHueResponseInfo(message) =>
                context.log.info(message)
              case LogHueFailure(message, None) =>
                context.log.error(message)
              case LogHueFailure(message, Some(throwable)) =>
                context.log.error(message, throwable)
              case HueApiTimeout(msg) =>
                context.log.warn(s"Timeout: $msg")
            }
            timeKeeper !! TimeKeeper.Cancel(HueControl)
            stash.unstashAll(waitingForCommand(light, timeKeeper, lastSetState).behavior)

          case command: Command =>
            stash.stash(command)
            Behaviors.same
        }
      }
    }
  }
}
