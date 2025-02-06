package me.micseydel.actor.perimeter

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.pattern.after
import akka.util.Timeout
import cats.data.Validated.{Invalid, Valid}
import me.micseydel.NoOp
import me.micseydel.actor.HueListener
import me.micseydel.actor.notifications.NotificationCenterManager.HueCommand
import me.micseydel.actor.perimeter.HueControl.{DoALightShow, FlashTheLight, FlashTheLights, HueConfig, SetBrightness, SetLight, TurnOffAllLights, TurnOffLight}
import me.micseydel.actor.perimeter.hue.HueNoteRef
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.model.KnownIntent.no_intent
import me.micseydel.model.Light.AllList
import me.micseydel.model.LightStates._
import me.micseydel.model._
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.time.Duration
import scala.annotation.unused
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContextExecutorService, Future, TimeoutException}
import scala.util.{Failure, Success}

object HueControl {
  // mailbox

  sealed trait Message

  case class StartTinkering(tinker: Tinker) extends Message
  case class NoteUpdated(noOp: NoOp.type) extends Message

  // to change the lights
  sealed trait Command extends Message
  // for handling the Hue Bridge response
//  private
  sealed trait StateUpdate extends Message

  case class FlashTheLight(light: Light) extends Command
  case class FlashTheLights() extends Command
  case class TurnOffLight(light: Light) extends Command
  case class TurnOffAllLights() extends Command

  case class DoALightShow() extends Command


  case class SetLight(light: Light, lightState: LightState) extends Command

  /**
   * @param brightnessPct [0, 100]
   */
  case class SetBrightness(light: Light, brightnessPct: Int) extends Command


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

  private def finishSetup(lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]], lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]])(implicit httpExecutionContext: ExecutionContextExecutorService, timeout: Timeout, Tinker: Tinker): Ability[Message] = Tinkerer[Message](rgb(230, 230, 230), "🕹️").withWatchedActorNote("Hue Control", NoteUpdated) { (context, noteRef) =>
    // call this actor if needed
    @unused
    val hueListener = context.cast(HueListener(context.self), "HueListener")

    implicit val markdown: HueNoteRef = new HueNoteRef(noteRef)
    behavior(lightKeepersByName, lightKeepersByLight)
  }

  // states / behaviors

  private def behavior(lightKeepersByName: Map[String, SpiritRef[HueLightKeeper.Message]], lightKeepersByLight: Map[Light, SpiritRef[HueLightKeeper.Message]])(implicit httpExecutionContext: ExecutionContextExecutorService, timeout: Timeout, Tinker: Tinker, hueNote: HueNoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    implicit val actorSystem: ActorSystem[Nothing] = context.system.actorSystem

    Tinker.withMessages {
      case NoteUpdated(_) =>
        for (command <- hueNote.checkForCheckbox()) {
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
            val fut = actorRef.underlying.ask(HueLightKeeper.GetLightState)
              .map { capturedLightState =>
                actorRef !! HueLightKeeper.SetLight(capturedLightState)
                LogLightKeeperResponseInfo(s"Captured $capturedLightState for light $light, setting brightness to $brightnessPct%")
              }
            context.pipeToSelf(fut) {
              case Success(value) => value
              case Failure(exception) =>
                LogLightKeeperFailure(s"Failed to set light $light brightness to $brightnessPct%, probably failed to capture existing to restore it after", Some(exception))
            }
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
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

      case TurnOffLight(light: Light) =>

        lightKeepersByLight.get(light) match {
          case Some(actorRef) =>
            val fut = actorRef.underlying.ask(HueLightKeeper.GetLightState)
              .map { capturedLightState =>
                actorRef !! HueLightKeeper.SetLight(capturedLightState.copy(on = false))
                LogLightKeeperResponseInfo(s"Captured $capturedLightState for $light, turning off")
              }
            context.pipeToSelf(fut) {
              case Success(value) => value
              case Failure(exception) =>
                LogLightKeeperFailure(s"Failed to get light state, unable to turn off (without deleting state)", Some(exception))
            }
          case None =>
            context.actorContext.log.error(s"Light $light not in lightKeepersByLight; keys = ${lightKeepersByLight.keySet}")
        }

        Tinker.steadily

      case TurnOffAllLights() =>
        val theLights: Seq[Light] = AllList // List(ByTheDehumidifier, FrontTable, FrontLitter, ByTheLitterRobot, Bedroom, BackToiletLow, BackToiletHigh)
        for (light <- theLights) {
          context.self !! TurnOffLight(light)
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

      case StartTinkering(_) =>
        context.actorContext.log.warn("Received a redundant StartTinkering")
        Tinker.steadily
    }
  }

  private def resetMarkdown(noteRef: NoteRef): Unit = {
    noteRef.setMarkdown("- [ ] Flash the lights 💡\n- [ ] Do a light show 🌈\n") match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }
  }
}


object HTTPHelpers {

  import spray.json._

  implicit object AnyValFormat extends JsonFormat[AnyVal] {
    def write(x: AnyVal): JsValue = x match {
      case n: Int => JsNumber(n)
      case b: Boolean => JsBoolean(b)
      case _ => serializationError("Do not know how to serialize")
    }

    def read(value: JsValue): AnyVal = value match {
      case JsNumber(n) => n.intValue
      case JsBoolean(b) => b
      case _ => deserializationError("Do not know how to deserialize")
    }
  }

  private object LightStateJsonProtocol extends DefaultJsonProtocol {
    implicit val lightStateFormat: RootJsonFormat[LightState] = jsonFormat4(LightState)
  }


  object HueApi {
    import LightStateJsonProtocol._

    def getLightState(light: Light, timeoutDuration: FiniteDuration = 10.seconds)
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

    def putLightState(light: Light, lightState: LightState)
                     (implicit system: ActorSystem[Nothing], httpExecutionContext: ExecutionContextExecutorService, hueConfig: HueConfig): Future[Boolean] = {
      // scale from a percentage to out of 255
      val serialized = lightState.copy(bri = (lightState.bri *255 /100.0).toInt, on = lightState.bri > 0).toJson
      val request = HttpRequest(
        method = HttpMethods.PUT,
        uri = s"http://${hueConfig.ip}/api/${hueConfig.username}/lights/${light.lightId}/state",
        entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, serialized.toString.getBytes),
      )

      val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

      responseFuture.map { httpResponse =>
        httpResponse.entity.discardBytes()
        httpResponse.status == StatusCodes.OK
      }
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
  implicit val turnOffLightJsonFormat: RootJsonFormat[TurnOffLight] = jsonFormat1(TurnOffLight)
  implicit val turnOffAllLightsJsonFormat: RootJsonFormat[TurnOffAllLights] = jsonFormat0(TurnOffAllLights)
  implicit val doALightShowJsonFormat: RootJsonFormat[DoALightShow] = jsonFormat0(DoALightShow)
  implicit val setLightJsonFormat: RootJsonFormat[SetLight] = jsonFormat2(SetLight)
  implicit val setBrightnessJsonFormat: RootJsonFormat[SetBrightness] = jsonFormat2(SetBrightness)

  implicit object HueControlCommandJsonFormat extends RootJsonFormat[HueControl.Command] {
    def write(m: HueControl.Command): JsValue = {
      val (jsObj, typ) = m match {
        case l: FlashTheLight => (l.toJson.asJsObject, "FlashTheLight")
        case l: FlashTheLights => (l.toJson.asJsObject, "FlashTheLights")
        case l: TurnOffLight => (l.toJson.asJsObject, "TurnOffLight")
        case l: TurnOffAllLights => (l.toJson.asJsObject, "TurnOffAllLights")
        case l: DoALightShow => (l.toJson.asJsObject, "DoALightShow")
        case l: SetLight => (l.toJson.asJsObject, "SetLight")
        case l: SetBrightness => (l.toJson.asJsObject, "SetBrightness")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): HueControl.Command = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("FlashTheLight")) => value.convertTo[FlashTheLight]
        case Seq(JsString("FlashTheLights")) => value.convertTo[FlashTheLights]
        case Seq(JsString("TurnOffLight")) => value.convertTo[TurnOffLight]
        case Seq(JsString("TurnOffAllLights")) => value.convertTo[TurnOffAllLights]
        case Seq(JsString("DoALightShow")) => value.convertTo[DoALightShow]
        case Seq(JsString("SetLight")) => value.convertTo[SetLight]
        case Seq(JsString("SetBrightness")) => value.convertTo[SetBrightness]
        case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(_)) for one of {PushNotification, HueCommand} but got $other")
      }
    }
  }
}
