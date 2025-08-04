package me.micseydel.util

import cats.data.NonEmptyList
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsNull, JsNumber, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.nio.file.Path
import java.time.{DateTimeException, Instant, LocalDate, ZoneId, ZonedDateTime}

object JsonUtil {
  object CommonJsonProtocol extends DefaultJsonProtocol {
    implicit object LocalDateTypeJsonFormat extends RootJsonFormat[LocalDate] {
      def write(e: LocalDate): JsString = JsString(e.toString)

      def read(value: JsValue): LocalDate = value match {
        case JsString(s) => LocalDate.parse(s)
        case _ => throw DeserializationException(s"An ISO local date")
      }
    }
  }

  implicit object ZonedDateTimeJsonFormat extends RootJsonFormat[ZonedDateTime] {
    def write(t: ZonedDateTime): JsString = JsString(t.toString)

    def read(value: JsValue): ZonedDateTime = value match {
      case JsString(s) =>
        ZonedDateTime.parse(s)
      case JsNumber(value) =>
        try {
          ZonedDateTime.ofInstant(Instant.ofEpochSecond(value.longValue), ZoneId.systemDefault)
        } catch {
          case e: DateTimeException =>
            throw DeserializationException(s"Failed to extract ZonedDateTime from value $value", e)
        }
      case _ =>
        throw DeserializationException("Expected a string or epoch number")
    }
  }

  implicit object PathJsonFormat extends RootJsonFormat[Path] {
    def write(t: Path): JsString = JsString(t.toString)

    def read(value: JsValue): Path = value match {
      case JsString(s) => Path.of(s)
      case _ => throw DeserializationException("Expected a string")
    }
  }

  object OptionalJsonFormat {
    def apply[Item](itemFormat: JsonFormat[Item]): JsonFormat[Option[Item]] = new JsonFormat[Option[Item]] {

      override def write(optionItem: Option[Item]): JsValue = optionItem match {
        case Some(item) => itemFormat.write(item)
        case None => JsNull
      }

      override def read(json: JsValue): Option[Item] = json match {
        case JsNull => None
        case other => Some(itemFormat.read(other))
      }
    }
  }

  def nonEmptyListJsonFormat[T]()(implicit listJsonFormat: JsonFormat[List[T]], jsonFormat: JsonFormat[T]): JsonFormat[NonEmptyList[T]] = new JsonFormat[NonEmptyList[T]] {
    def write(m: NonEmptyList[T]): JsValue = {
      JsArray(m.toList.toVector.map(_.toJson))
    }

    def read(value: JsValue): NonEmptyList[T] = {
      value.convertTo[List[T]] match {
        case head :: tail => NonEmptyList(head, tail)
        case Nil => throw DeserializationException("list was empty")
      }
    }
  }
}
