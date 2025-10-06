package me.micseydel.util

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import me.micseydel.Common
import me.micseydel.dsl.cast.chronicler.ChroniclerMOCDailyNote.{Completed, DataPointState, StruckThrough, Todo}
import me.micseydel.vault.NoteId

import java.time.{LocalDate, LocalTime, ZoneId, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

//trait ShapelessJsonSupport {
//  import shapeless.Lazy
//
////  implicit def jsonFormat[T](implicit gen: LabelledGeneric.Aux[T, _], h: Lazy[JsonFormat[T]]): JsonFormat[T] = h.value
//implicit def jsonFormat[T](implicit h: Lazy[JsonFormat[T]]): JsonFormat[T] = h.value
//}

object ParseUtil {
  sealed trait LineParseResult[T]

  case class ParseSuccessDatapoint[T](datapoint: T) extends LineParseResult[T]

  case class ParseFailure[T](rawLine: String, reason: NonEmptyList[String], comments: List[String]) extends LineParseResult[T]


  // returns: (batch, after)
  @tailrec
  def batchConsecutiveComments(lines: List[String], accumulator: List[String] = Nil): (List[String], List[String]) = {
    lines match {
      case Nil =>
        (accumulator.reverse, Nil)

      case line :: theRest if line.startsWith("  ") =>
        batchConsecutiveComments(theRest, line :: accumulator)

      case _ =>
        (accumulator, lines)
    }
  }

  def getLinesAfterHeader(markdown: String, header: String): List[String] = {
    markdown
      .split("\n")
      .toList
      .dropWhile(_ != s"# $header").drop(1).filter(_ != "")
  }

  def getNoteId(parts: List[String]): ValidatedNel[String, NoteId] = parts match {
    // List(
    //  ([[Transcription,
    //  for,
    //  mobile_audio_capture_20240218-192922.wav|ref]])
    // )
    case List("([[Transcription", "for", specificsAndStuff) =>
      // mobile_audio_capture_20240218-192922.wav|ref]]
      specificsAndStuff.split(raw"\|", 2).toList match {
        case List(audioName, "ref]])" | "ref]])~~" | "ref]]") =>
          Validated.Valid(NoteId(s"Transcription for $audioName"))

        case other =>
          Validated.Invalid(NonEmptyList.of(s"Expected a 2-element list wav name and `ref]])` but got $other"))
      }

    case other =>
      Validated.Invalid(NonEmptyList.of(s"""Expected [[Transcription for ....wav]] but got ${other.mkString("\n")}"""))
  }

  def getZonedDateTimeFromListLineFront(parts: List[String], day: LocalDate): ValidatedNel[String, (ZonedDateTime, List[String])] = parts match {
    // - \[1:38:03AM\] (contents) ([[Transcription for mobile_audio_capture_20240306-013803.wav|ref]])
    // theRest = List((contents), ([[Transcription, for, mobile_audio_capture_20240306-013803.wav|ref]])
    case "-" :: maybeTime :: theRest =>
      extractTimeFromBrackets(maybeTime, day).map { time =>
        (time, theRest)
      }

    case other =>
      Validated.Invalid(NonEmptyList.of(s"""Expected a Markdown list with some contents but got: `${other.mkString("\n")}`"""))
  }

  def getZonedDateTimeFromListLineFrontWithOptionalPrefix(parts: List[String], day: LocalDate): ValidatedNel[String, (ZonedDateTime, List[String], Option[DataPointState])] = {
    parts match {
      case "-" :: "[" :: "]" :: maybeTime :: theRest =>
        extractTimeFromBrackets(maybeTime, day).map { time =>
          (time, theRest, Some(Todo))
        }

      case "-" :: "[x]" :: maybeTime :: theRest =>
        extractTimeFromBrackets(maybeTime, day).map { time =>
          (time, theRest, Some(Completed))
        }

      case "-" :: maybeTime :: theRest =>

        if (maybeTime.startsWith("~~")) {
          extractTimeFromBrackets(maybeTime.drop(2), day).map { time =>
            (time, theRest, Some(StruckThrough))
          }
        } else {
          extractTimeFromBrackets(maybeTime, day).map { time =>
            (time, theRest, None)
          }
        }

      case other =>
        Validated.Invalid(NonEmptyList.of(s"""Expected a Markdown list with some contents but got: `${other.mkString("\n")}`"""))
    }
  }

  private def extractTimeFromBrackets(maybeTime: String, day: LocalDate): ValidatedNel[String, ZonedDateTime] = {
    // e.g. \[07:29:22PM\] or [07:29:22PM] or even 7:29:22PM

    val withoutFrontBackslash = if (maybeTime.startsWith("\\")) {
      maybeTime.drop(1)
    } else {
      maybeTime
    }

    val withoutOpenBracket = if (withoutFrontBackslash.startsWith("[")) {
      withoutFrontBackslash.drop(1)
    } else {
      withoutFrontBackslash
    }

    val withoutCloseBracket = if (withoutOpenBracket.endsWith("]")) {
      withoutOpenBracket.dropRight(1)
    } else {
      withoutOpenBracket
    }

    val withoutEndingBackslash = if (withoutCloseBracket.endsWith("\\")) {
      withoutCloseBracket.dropRight(1)
    } else {
      withoutCloseBracket
    }

    Try(LocalTime.parse(withoutEndingBackslash, TimeUtil.WithinDayDateTimeFormatter)) match {
      case Success(parsedTime) =>
        val zonedDateTime: ZonedDateTime = ZonedDateTime.of(day, parsedTime, ZoneId.systemDefault())
        Validated.Valid(zonedDateTime)

      case Failure(dtpe: java.time.format.DateTimeParseException) =>
        Validated.Invalid(NonEmptyList.of(dtpe.getMessage))
      case Failure(exception) =>
        Validated.Invalid(NonEmptyList.of(Common.getStackTraceString(exception)))
    }
  }
}
