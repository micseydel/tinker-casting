package me.micseydel.actor.perimeter

import akka.util.Timeout
import cats.data.Validated
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common.getValidatedStringFromConfig
import me.micseydel.{Common, NoOp}
import me.micseydel.actor.perimeter.HueControl._
import me.micseydel.actor.perimeter.hue.HueNoteRef
import me.micseydel.dsl.SpiritRef.TinkerIO
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.model.Light.AllList
import me.micseydel.model._
import me.micseydel.vault.persistence.NoteRef
import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.time.Duration
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object HueControl {
  sealed trait Message

  case class NoteUpdated(noOp: NoOp.type) extends Message

  // to change the lights
  sealed trait Command extends Message

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


  // behavior

  case class HueConfig(ip: String, username: String)

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, NoteUpdated]("Hue Control", rgb(230, 230, 230), "🕹️", NoteUpdated) { (context, noteRef) =>
    val validatedConfig = noteRef.readNote().flatMap(_.yamlFrontMatter) match {
      case Failure(exception) =>
        s"Reading frontmatter failed ${Common.getStackTraceString(exception)}".invalidNel
      case Success(map) =>
        val validatedIp = getValidatedStringFromConfig(map, "hueApiIP")
        val validatedUsername = getValidatedStringFromConfig(map, "hueApiUsername")
        validatedIp.andThen { ip =>
          validatedUsername.andThen { username =>
            HueConfig(ip, username).validNel
          }
        }
    }

    validatedConfig match {
      case Validated.Valid(hueConfig: HueConfig) =>
        val lightKeepers: Map[(String, Light), SpiritRef[HueLightKeeper.Message]] = Light.AllMap.map { case (lightName, light) =>
          (lightName, light) -> context.cast(HueLightKeeper(light, hueConfig), s"HueLightKeeper-$lightName")
        }

        val lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]] = lightKeepers.map { case ((name, _), value) => name -> value }
        val lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]] = lightKeepers.map { case ((_, light), value) => light -> value }

        implicit val timeout: Timeout = Timeout.create(Duration.ofMillis(20.seconds.toMillis))
        implicit val hueNote: HueNoteRef = new HueNoteRef(noteRef)
        behavior(lightKeepersByName, lightKeepersByLight)

      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Failed to start HueControl: $e")
        Tinker.ignore
    }
  }

  // states / behaviors

  private def behavior(lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]], lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]])(implicit timeout: Timeout, Tinker: Tinker, hueNote: HueNoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context

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

      case DoALightShow() =>
        for (lightKeeper <- lightKeepersByName.values) {
          lightKeeper !!-> HueLightKeeper.DoALightShow()
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
            actorRef !!-> HueLightKeeper.SetBrightness(brightnessPct)
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        Tinker.steadily

      case SetAllBrightness(pct) =>
        context.actorContext.log.info(s"Setting all lights to $pct%")
        for (light <- AllList) {
          context.self !!-> SetBrightness(light, pct)
        }
        Tinker.steadily

      case FlashTheLight(light) =>
        lightKeepersByLight.get(light) match {
          case Some(actorRef) =>
            actorRef !!-> HueLightKeeper.FlashTheLight
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        Tinker.steadily

      case SetLight(light, lightState) =>
        lightKeepersByLight.get(light) match {
          case Some(ref) =>
            context.actorContext.log.info(s"Telling light $light to set itself to $lightState")
            ref !!-> HueLightKeeper.SetLight(lightState)
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        behavior(lightKeepersByName, lightKeepersByLight)

      case SetAllLights(state) =>
        for (light <- AllList) {
          context.self !! HueControl.SetLight(light, state)
        }
        Tinker.steadily
    }
  }

  private implicit class RichSpiritRef[T](val spiritRef: SpiritRef[T]) extends AnyVal {
    def !!->(message: T)(implicit tc: TinkerContext[_]): Unit = {
      spiritRef !!-> TinkerIO("💡", message)
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
