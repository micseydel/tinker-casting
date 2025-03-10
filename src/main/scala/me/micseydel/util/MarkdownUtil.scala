package me.micseydel.util

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
}
