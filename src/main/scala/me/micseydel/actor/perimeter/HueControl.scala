package me.micseydel.actor.perimeter

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import me.micseydel.NoOp
import me.micseydel.actor.{HueListener, RasaActor}
import me.micseydel.actor.perimeter.HueControl._
import me.micseydel.actor.perimeter.hue.HueNoteRef
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.model.Light.AllList
import me.micseydel.model._
import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.time.Duration
import scala.annotation.unused
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.duration.DurationInt

object HueControl {
  sealed trait Message

  case class StartTinkering(tinker: EnhancedTinker[ActorRef[RasaActor.Message]]) extends Message

  case class NoteUpdated(noOp: NoOp.type) extends Message

  // to change the lights
  sealed trait Command extends Message

  // for handling the Hue Bridge response
  //  private
  sealed trait StateUpdate extends Message

  case class FlashTheLight(light: Light) extends Command

  case class FlashTheLights() extends Command

  case class DoALightShow() extends Command


  case class SetLight(light: Light, lightState: LightState) extends Command

  case class SetAllLights(lightState: LightState) extends Command

  /**
   * @param brightnessPct [0, 100]
   */
  case class SetBrightness(light: Light, brightnessPct: Int) extends Command

  case class SetAllBrightness(brightnessPct: Int) extends Command


  private case class LogLightKeeperResponseInfo(message: String) extends StateUpdate

  private case class LogLightKeeperFailure(message: String, throwable: Option[Throwable] = None) extends StateUpdate

  // behavior

  case class HueConfig(ip: String, username: String)

  def apply(hueConfig: HueConfig)(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = {
    setup(hueConfig)
  }

  private def setup(hueConfig: HueConfig)(implicit httpExecutionContext: ExecutionContextExecutorService): Ability[Message] = Behaviors.setup { context =>
    implicit val timeout: Timeout = Timeout.create(Duration.ofMillis(20.seconds.toMillis))

    Behaviors.withStash(20) { stash =>
      Behaviors.receiveMessage {
        case StartTinkering(tinker) =>
          implicit val t: Tinker = tinker
          val lightKeepers: Map[(String, Light), SpiritRef[HueLightKeeper.Message]] = Light.AllMap.map { case (lightName, light) =>
            // these are as good as first-class SpiritRefs
            (lightName, light) -> tinker.tinkerSystem.wrap(context.spawn(HueLightKeeper(light, hueConfig), s"HueLightKeeper-$lightName"))
          }

          val lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]] = lightKeepers.map { case ((name, _), value) => name -> value }
          val lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]] = lightKeepers.map { case ((_, light), value) => light -> value }

          // it's always waiting for a command, or waiting to hear back from Hue
          stash.unstashAll(finishSetup(lightKeepersByName, lightKeepersByLight)(httpExecutionContext, timeout, tinker))
        case other =>
          stash.stash(other)
          Behaviors.same
      }
    }
  }

  private def finishSetup(lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]], lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]])(implicit httpExecutionContext: ExecutionContextExecutorService, timeout: Timeout, Tinker: EnhancedTinker[ActorRef[RasaActor.Message]]): Ability[Message] = AttentiveNoteMakingTinkerer[Message, NoteUpdated]("Hue Control", rgb(230, 230, 230), "🕹️", NoteUpdated) { (context, noteRef) =>
    // call this actor if needed
    @unused
    val hueListener = context.cast(HueListener(context.self), "HueListener")

    implicit val hueNote: HueNoteRef = new HueNoteRef(noteRef)
    behavior(lightKeepersByName, lightKeepersByLight)
  }

  // states / behaviors

  private def behavior(lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]], lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]])(implicit httpExecutionContext: ExecutionContextExecutorService, timeout: Timeout, Tinker: Tinker, hueNote: HueNoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    implicit val actorSystem: ActorSystem[Nothing] = context.system.actorSystem

    hueNote.setToDefault()

    Tinker.receiveMessage {
      case NoteUpdated(_) =>
        hueNote.checkForCommand() match {
          case None => context.actorContext.log.debug("Detected note update but no command checked")
          case Some(command) =>
            context.actorContext.log.info(s"Triggering command $command")
            context.self !! command
            hueNote.setToDefault()
        }

        Tinker.steadily

      case stateUpdate: StateUpdate =>
        context.actorContext.log.debug(s"Was waiting for command, did not expect Hue status update $stateUpdate")
        Tinker.steadily

      case DoALightShow() =>
        for (lightKeeper <- lightKeepersByName.values) {
          lightKeeper !! HueLightKeeper.DoALightShow()
        }

        Tinker.steadily

      case FlashTheLights() =>
        context.actorContext.log.info("Flashing all the lights")
        for (light <- AllList) {
          context.self !! FlashTheLight(light)
        }
        Tinker.steadily

      case SetBrightness(light, brightnessPct) =>

        lightKeepersByLight.get(light) match {
          case Some(actorRef) =>
            actorRef !! HueLightKeeper.SetBrightness(brightnessPct)
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        Tinker.steadily

      case SetAllBrightness(pct) =>
        context.actorContext.log.info(s"Setting all lights to $pct%")
        for (light <- AllList) {
          context.self !! SetBrightness(light, pct)
        }
        Tinker.steadily

      case FlashTheLight(light) =>
        lightKeepersByLight.get(light) match {
          case Some(actorRef) =>
            actorRef !! HueLightKeeper.FlashTheLight
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        Tinker.steadily

      case SetLight(light, lightState) =>
        lightKeepersByLight.get(light) match {
          case Some(ref) =>
            context.actorContext.log.info(s"Telling light $light to set itself to $lightState")
            ref !! HueLightKeeper.SetLight(lightState)
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        behavior(lightKeepersByName, lightKeepersByLight)

      case SetAllLights(state) =>
        for (light <- AllList) {
          context.self !! HueControl.SetLight(light, state)
        }
        Tinker.steadily

      case StartTinkering(_) =>
        context.actorContext.log.warn("Received a redundant StartTinkering")
        Tinker.steadily
    }
  }
}

object HueControlJsonFormat extends DefaultJsonProtocol {
  implicit object LightJsonFormat extends RootJsonFormat[Light] {
    def write(m: Light): JsValue = {
      JsNumber(m.lightId)
    }

    def read(value: JsValue): Light = {
      value match {
        case JsNumber(num) => new Light(num.toInt)
        case other => throw DeserializationException(s"Unknown type, expected a number but got $other")
      }
    }
  }

  implicit val LightStateJsonFormat: RootJsonFormat[LightState] = jsonFormat4(LightState)

  implicit val flashTheLightJsonFormat: RootJsonFormat[FlashTheLight] = jsonFormat1(FlashTheLight)
  implicit val flashTheLightsJsonFormat: RootJsonFormat[FlashTheLights] = jsonFormat0(FlashTheLights)
  implicit val doALightShowJsonFormat: RootJsonFormat[DoALightShow] = jsonFormat0(DoALightShow)
  implicit val setLightJsonFormat: RootJsonFormat[SetLight] = jsonFormat2(SetLight)
  implicit val setBrightnessJsonFormat: RootJsonFormat[SetBrightness] = jsonFormat2(SetBrightness)
  implicit val setTheLightsJsonFormat: RootJsonFormat[SetAllLights] = jsonFormat1(SetAllLights)
  implicit val setAllBrightnessJsonFormat: RootJsonFormat[SetAllBrightness] = jsonFormat1(SetAllBrightness)

  implicit object HueControlCommandJsonFormat extends RootJsonFormat[HueControl.Command] {
    def write(m: HueControl.Command): JsValue = {
      val (jsObj, typ) = m match {
        case l: FlashTheLight => (l.toJson.asJsObject, "FlashTheLight")
        case l: FlashTheLights => (l.toJson.asJsObject, "FlashTheLights")
        case l: DoALightShow => (l.toJson.asJsObject, "DoALightShow")
        case l: SetLight => (l.toJson.asJsObject, "SetLight")
        case l: SetAllLights => (l.toJson.asJsObject, "SetAllLights")
        case l: SetBrightness => (l.toJson.asJsObject, "SetBrightness")
        case l: SetAllBrightness => (l.toJson.asJsObject, "SetAllBrightness")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): HueControl.Command = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("FlashTheLight")) => value.convertTo[FlashTheLight]
        case Seq(JsString("FlashTheLights")) => value.convertTo[FlashTheLights]
        case Seq(JsString("DoALightShow")) => value.convertTo[DoALightShow]
        case Seq(JsString("SetLight")) => value.convertTo[SetLight]
        case Seq(JsString("SetBrightness")) => value.convertTo[SetBrightness]
        case Seq(JsString("SetAllLights")) => value.convertTo[SetAllLights]
        case Seq(JsString("SetAllBrightness")) => value.convertTo[SetAllBrightness]
        case other => throw DeserializationException(s"Unknown type $other, check the code")
      }
    }
  }
}
