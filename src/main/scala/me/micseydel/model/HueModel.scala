package me.micseydel.model

import scala.util.Try

object Light {
  val ByTheDehumidifier = new Light(2)
  val FrontTable = new Light(3)
  val FrontLitter = new Light(4)
  val ByTheLitterRobot = new Light(5)
  val Bedroom = new Light(8)
  val BackToiletLow = new Light(9)
  val BackToiletHigh = new Light(10)

  val AllList: Seq[Light] = List(ByTheDehumidifier, FrontTable, FrontLitter, ByTheLitterRobot,
    //      Bedroom,
    BackToiletLow, BackToiletHigh)

  val ByTheDehumidifierName = "ByTheDehumidifier"
  val FrontTableName = "FrontTable"
  val FrontLitterName = "FrontLitter"
  val ByTheLitterRobotName = "ByTheLitterRobot"
  val BedroomName = "Bedroom"
  val BackToiletLowName = "BackToiletLow"
  val BackToiletHighName = "BackToiletHigh"

  val AllMap: Map[String, Light] = Map(
    ByTheDehumidifierName -> ByTheDehumidifier,
    FrontTableName -> FrontTable,
    FrontLitterName -> FrontLitter,
    ByTheLitterRobotName -> ByTheLitterRobot,
    //      BedroomName -> Bedroom,
    BackToiletLowName -> BackToiletLow,
    BackToiletHighName -> BackToiletHigh
  )
}

final class Light(val lightId: Int) extends AnyVal

/**
 *
 * @param on  the on/off state of the light
 * @param bri [0, 255]?
 * @param hue white=13248?
 * @param sat [0, 255]?
 */
case class LightState(on: Boolean, bri: Int, hue: Int, sat: Int)

object LightStates {
  val WhiteLight: LightState = LightState(on = true, bri = 254, hue = 13248, sat = 5)
  val RelaxedLight: LightState = LightState(on = true, bri = 254, hue = 7613, sat = 203)
  val BlueishLight: LightState = LightState(on = true, bri = 254, hue = 44573, sat = 230)

  // empirical

  // via batch above
  val SoftPinkLight: LightState = LightState(on = true, bri = 100, hue = 275, sat = 100)

  // via iteration

  val RedLight: LightState = LightState(on = true, bri = 255, hue = 1000, sat = 255)
  val OrangeLight: LightState = LightState(on = true, bri = 255, hue = 4000, sat = 255)
  val YellowLight: LightState = LightState(on = true, bri = 255, hue = 8000, sat = 255)
  val GreenLight: LightState = LightState(on = true, bri = 255, hue = 16000, sat = 255)
  val GreenerLight: LightState = LightState(on = true, bri = 255, hue = 20000, sat = 255)
  val AnotherGreenLight: LightState = LightState(on = true, bri = 255, hue = 30000, sat = 255)
  val CyanLight: LightState = LightState(on = true, bri = 255, hue = 32000, sat = 255)
  val BlueishGreenLight: LightState = LightState(on = true, bri = 255, hue = 35000, sat = 255)
  val FrostyBlueLight: LightState = LightState(on = true, bri = 255, hue = 38000, sat = 255)
  val BlueLight: LightState = LightState(on = true, bri = 255, hue = 43000, sat = 255)
  val PurpleLight: LightState = LightState(on = true, bri = 255, hue = 48000, sat = 255)
  val PinkLight: LightState = LightState(on = true, bri = 255, hue = 55000, sat = 255)
  val HotPinkLight: LightState = LightState(on = true, bri = 255, hue = 60000, sat = 255)
}

sealed trait Color

object Color {
  import LightStates._

  def unapply(string: String): Option[LightState] = string.toLowerCase match {
    case "red" =>
      Some(RedLight)
    case "orange" =>
      Some(OrangeLight)
    case "yellow" =>
      Some(YellowLight)
    case "green" =>
      Some(GreenLight)
    case "cyan" =>
      Some(CyanLight)
    case "blue" =>
      Some(BlueLight)
    case "purple" =>
      Some(PurpleLight)
    case "pink" =>
      Some(PinkLight)
    case "white" =>
      Some(WhiteLight)
    case "relax" | "relaxed" =>
      Some(RelaxedLight)
    case "off" =>
      Some(RelaxedLight.copy(on = false))
    // also frosty blue, couple more greens, hot pink, soft pink
    case _ =>
      None
  }
}

// entity types - color, brightness, light_state(on/off)

object Brightness {
  def unapply(string: String): Option[Int] = {
    string.toIntOption.filter(value => value >= 0 && value <= 100)
  }
}

object LightOn {
  def unapply(string: String): Option[Boolean] = {
    string match {
      case "on" => Some(true)
      case "off" => Some(false)
      case other => None
    }
  }
}
