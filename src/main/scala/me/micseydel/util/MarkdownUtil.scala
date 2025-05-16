package me.micseydel.util

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.vault.LinkId

import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal

object MarkdownUtil {
  def listLineWithTimestampAndRef(time: ZonedDateTime, line: String, refLink: LinkId, beforeTimestamp: Option[String] = None, blockId: Option[String] = None, dateTimeFormatter: DateTimeFormatter = TimeUtil.WithinDayDateTimeFormatter): String = {
    listLineWithTimestamp(time, s"$line (${refLink.wikiLinkWithAlias("ref")})", beforeTimestamp, blockId, dateTimeFormatter)
  }

  def listLineWithTimestamp(time: ZonedDateTime, line: String, beforeTimestamp: Option[String] = None, blockId: Option[String] = None, dateTimeFormatter: DateTimeFormatter = TimeUtil.WithinDayDateTimeFormatter): String = {
    listLineWithTimestampHelper(time, line, beforeTimestamp, blockId, dateTimeFormatter)
  }

  def listLineWithTimestampForDay(time: LocalDate, line: String, beforeTimestamp: Option[String] = None, blockId: Option[String] = None): String = {
    listLineWithTimestampHelper(time, line, beforeTimestamp, blockId, DateTimeFormatter.ISO_LOCAL_DATE)
  }

  private def listLineWithTimestampHelper(time: Temporal, line: String, beforeTimestamp: Option[String] = None, blockId: Option[String] = None, dateTimeFormatter: DateTimeFormatter = TimeUtil.WithinDayDateTimeFormatter) = {
    val before = beforeTimestamp.getOrElse("")
    val timestamp = dateTimeFormatter.format(time)
    s"- $before\\[$timestamp\\] $line" + blockId.map(id => s" ^$id").getOrElse("")
  }

  def removeLinesEndingWithBlockId(blockId: String, lines: String): String = {
    lines
      .split("\n")
      .filterNot(_.endsWith(s" ^$blockId"))
      .mkString("\n")
  }

  def getListFromHeader(markdown: String, headerWithHashes: String): List[String] = {
    val lines = markdown.split("\n").toList

    lines.dropWhile(_ != headerWithHashes) // find the section
      .dropWhile(!_.startsWith("- ")) // ignore whitespace
      .takeWhile(_.startsWith("- ")) // ignore anything after
      .map(line => line.slice(2, line.length))
  }

  def readListOfWikiLinks(markdown: String): ValidatedNel[String, NonEmptyList[String]] = {
    val lines = markdown.split("\n")

    val (toClean, problems) = lines.toList
      .zipWithIndex
      .filter(_._1.nonEmpty)
      .partition { case (line, _) =>
        line.startsWith("- [[") && line.endsWith("]]")
      }

    // may still be empty, written this way for the statically typed non-empty list
    val formattedProblemLines = problems.map {
      case (line, index) =>
        s"""Line $index was not a wikilink list line (starting with "- [[" and ending with ("]]"): $line"""
    }

    formattedProblemLines match {
      case head :: tail =>
        Validated.Invalid(NonEmptyList(head, tail))
      case Nil =>
        val cleanedLines = toClean.map(_._1.drop(4)).dropRight(2)
        cleanedLines match {
          case head :: tail =>
            NonEmptyList(head, tail).validNel
          case Nil =>
            "there were no items".invalidNel
        }
    }
  }
}
