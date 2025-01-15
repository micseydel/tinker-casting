package me.micseydel.prototyping

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.Common
import me.micseydel.actor.perimeter.HueControl
//import me.micseydel.dsl.TinkerSystemForTesting
import me.micseydel.model.Light
import me.micseydel.model.Light.Bedroom
import me.micseydel.model.LightStates.RelaxedLight

import java.time.ZonedDateTime
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object HueDemo {
//  def main(args: Array[String]): Unit = {
//    val httpThreadPool: ExecutorService = Executors.newFixedThreadPool(10)
//    implicit val httpExecutionContext: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(httpThreadPool)
//
//    implicit val system: ActorSystem[HueControl.Message] = ActorSystem[HueControl.Message](Behaviors.setup { context =>
//      implicit val tinkerSystem: TinkerSystemForTesting = new TinkerSystemForTesting()
//      val hueControl = context.spawnAnonymous(HueControl())
//      Behaviors.receiveMessage { message =>
//        hueControl ! message
//        Behaviors.same
//      }
//    }, "HueHacking")
//
//    val lightChoice = Bedroom
////    system ! HueControl.SetLight(lightChoice, WhiteLight.copy(bri = 255))
//    system ! HueControl.SetLight(lightChoice, RelaxedLight.copy(bri = 10))
//
//    //    setLight(lightChoice)
//    //    setBrightness(lightChoice)
//    //    flashTheLight(lightChoice)
//    //    turnOffLight(lightChoice)
//    //    turnOffAllLights(lightChoice)
//
//    //    system ! HueControl.DoALightShow()
//
////    for (light <- Light.AllList) {
////      system ! HueControl.SetLight(light, WhiteLight)
////    }
//
//    println(s"[${Common.zonedDateTimeToISO8601(ZonedDateTime.now())}] Waiting a bit...")
//    Thread.sleep(5000)
//
//    println(s"[${Common.zonedDateTimeToISO8601(ZonedDateTime.now())}] Done")
//    system.terminate()
//    System.exit(0)
//  }

  private def setLight(lightChoice: Light)(implicit system: ActorSystem[HueControl.Message]): Unit = {
    system ! HueControl.SetLight(lightChoice, RelaxedLight.copy(bri = 100))
  }

  private def setBrightness(lightChoice: Light)(implicit system: ActorSystem[HueControl.Message]): Unit = {
    system ! HueControl.SetBrightness(lightChoice, 50)
  }

  private def flashTheLight(lightChoice: Light)(implicit system: ActorSystem[HueControl.Message]): Unit = {
    system ! HueControl.FlashTheLight(lightChoice)
  }

  private def turnOffLight(lightChoice: Light)(implicit system: ActorSystem[HueControl.Message]): Unit = {
    system ! HueControl.TurnOffLight(lightChoice)
  }

  private def turnOffAllLights()(implicit system: ActorSystem[HueControl.Message]): Unit = {
    system ! HueControl.TurnOffAllLights()
  }
}
