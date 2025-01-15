package me.micseydel.model

import me.micseydel.Common
import me.micseydel.vault.NoteId
import spray.json.{DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat}

import java.time.ZonedDateTime

sealed trait CatOfMine {
  def emoji: String
}

case object Peanut extends CatOfMine {
  override def emoji = "🥜"
}

case object Butter extends CatOfMine {
  override def emoji = "🧈"
}

object CatOfMine {
  def unapply(string: String): Option[CatOfMine] = string.strip().toLowerCase() match {
    case "peanut" => Some(Peanut)
    case "butter" => Some(Butter)
    case _ => None
  }

  implicit object CatOfMineFormat extends RootJsonFormat[CatOfMine] {
    def write(t: CatOfMine): JsString = JsString(t.toString)

    def read(value: JsValue): CatOfMine = value match {
      case JsString(CatOfMine(choice)) =>
        choice
      case other => throw DeserializationException(s"Expected a string in ${LitterBoxChoice.Choices} but got $other")
    }
  }
}

sealed trait LitterBoxChoice {
  def emoji: String
}


case object FrontLitter extends LitterBoxChoice {
  override def emoji: String = "⏩📦"
}
case object BackLitter extends LitterBoxChoice {
  override def emoji: String = "⏪📦"
}
case object Both extends LitterBoxChoice {
  val Both: Seq[LitterBoxChoice] = Seq(FrontLitter, BackLitter)

  override def emoji: String = "⏪📦⏩"
}
case object LitterRobot extends LitterBoxChoice {
  override def emoji: String = "🤖📦"
}

object LitterBoxChoice {
  def unapply(string: String): Option[LitterBoxChoice] = string.strip().toLowerCase() match {
    case "front" | "front litter" | "frontlitter" | "f" => Some(FrontLitter)
    case "back" | "back litter" | "backlitter" | "b" => Some(BackLitter)
    case "litter robot" | "litterrobot" | "r" => Some(LitterRobot)
    case "both" => Some(Both)
    case _ => None
  }

  def unapply(char: Char): Option[LitterBoxChoice] = unapply(char.toString)

  val Choices: Set[String] = Set("front", "back", "both", "litter robot", "front litter", "back litter", "FrontLitter", "BackLitter", "LitterRobot", "Both")
}



sealed trait CatObservationEvent {
  def when: ZonedDateTime
}

object CatObservationEvent {
  implicit object LitterBoxChoiceFormat extends RootJsonFormat[LitterBoxChoice] {
    def write(t: LitterBoxChoice): JsString = JsString(t.toString)

    def read(value: JsValue): LitterBoxChoice = value match {
      case JsString(LitterBoxChoice(choice)) =>
        choice
      case other => throw DeserializationException(s"Expected a string in ${LitterBoxChoice.Choices} but got $other")
    }
  }
}

case class PostHocLitterObservationEvent(when: ZonedDateTime, litterBoxChoice: LitterBoxChoice, isClean: Boolean) extends CatObservationEvent
case class LitterUsedEvent(when: ZonedDateTime, litterBoxChoice: LitterBoxChoice, maybeCat: Option[CatOfMine]) extends CatObservationEvent
case class LitterSiftedEvent(when: ZonedDateTime, litterBoxChoice: LitterBoxChoice, contents: SiftedContents) extends CatObservationEvent

object PostHocLitterObservationEventJsonProtocol extends DefaultJsonProtocol {
  import Common.ZonedDateTimeJsonFormat
  import CatObservationEvent.LitterBoxChoiceFormat
  implicit val PostHocLitterObservationEventJsonFormat: JsonFormat[PostHocLitterObservationEvent] = jsonFormat3(PostHocLitterObservationEvent)
}

object LitterUsedEventJsonProtocol extends DefaultJsonProtocol {
  import Common.ZonedDateTimeJsonFormat
  import CatObservationEvent.LitterBoxChoiceFormat
  implicit val LitterUsedEventJsonFormat: JsonFormat[LitterUsedEvent] = jsonFormat3(LitterUsedEvent)
}

object LitterSiftedEventJsonProtocol extends DefaultJsonProtocol {
  import Common.ZonedDateTimeJsonFormat

  implicit object LitterBoxChoiceFormat extends RootJsonFormat[LitterBoxChoice] {
    def write(t: LitterBoxChoice): JsString = JsString(t.toString)

    def read(value: JsValue): LitterBoxChoice = value match {
      case JsString(LitterBoxChoice(choice)) =>
        choice
      case other => throw DeserializationException(s"Expected a string in ${LitterBoxChoice.Choices} but got $other")
    }
  }

  implicit object SiftedContentsFormat extends RootJsonFormat[SiftedContents] {
    def write(t: SiftedContents): JsObject = JsObject(t.multiset.map {
      case (key, value) =>
        key.toString -> JsNumber(value)
    })

    def read(value: JsValue): SiftedContents = value match {
      case JsObject(map) =>
        SiftedContents(map.map {
          case (LitterUseType(key), JsNumber(value)) =>
            key -> value.toInt
          case (key, value) =>
            throw DeserializationException(s"Key $key not in ${LitterUseType.Known} or $value not convertible to an int")
        })
      case _ => throw DeserializationException("Expected an object/map")
    }
  }

  implicit val litterSiftedEventFormat: RootJsonFormat[LitterSiftedEvent] = jsonFormat3(LitterSiftedEvent)
}


sealed trait LitterUseType
case object Urination extends LitterUseType
case object Defecation extends LitterUseType

object LitterUseType {
  def unapply(string: String): Option[LitterUseType] = string.strip().toLowerCase() match {
    case "peed" | "pee" | "p" | "urinated" | "urination" => Some(Urination)
    case "pooped" | "poop" | "defecated" | "defecation" => Some(Defecation)
    case _ => None
  }

  val Known: Set[String] = Set("peed", "pee", "p", "urinated", "urination", "pooped", "poop", "defecated", "defecation")
}


case class SiftedContents(multiset: Map[LitterUseType, Int]) {
  def size: Int = multiset.values.sum

  def toEmojis: String = multiset.map {
    case (Urination, count) =>
      "💦" * count
    case (Defecation, count) =>
      "💩" * count
  }.mkString("")
}

object SiftedContents {
  def apply(list: List[LitterUseType]): SiftedContents = {
    def listToMultiset(list: List[LitterUseType]): Map[LitterUseType, Int] = {
      list.groupBy(identity).view.mapValues(_.size).toMap
    }

    SiftedContents(listToMultiset(list))
  }

  def apply(peeCount: Int, poopCount: Int): SiftedContents = {
    new SiftedContents(Map(
      Urination -> peeCount,
      Defecation -> poopCount
    ).filter(_._2 > 0).toMap)
  }

  def unapply(contents: SiftedContents): Option[LitterUseType] = {
    contents.multiset.keys.toList match {
      case List(single) =>
        Some(single)
      case Nil | _ =>
        None
    }
  }
}
