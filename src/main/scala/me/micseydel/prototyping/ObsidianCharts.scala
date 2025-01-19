package me.micseydel.prototyping

import me.micseydel.util.TimeUtil
import spray.json.enrichAny
import spray.json.DefaultJsonProtocol._

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object ObsidianCharts {
  def chart(labels: List[String], series: List[Series]): String = {
    val formattedSeries = series.map {
      case Series(title, data) =>
        s"""    - title: "$title"
           |      data: ${data.toJson.compactPrint}
           |""".stripMargin
    }.mkString("\n")

    s"""```chart
       |  type: "line"
       |  labels: ${labels.toJson.compactPrint}
       |  series:
       |$formattedSeries
       |```""".stripMargin
  }

  case class Series(title: String, data: List[Int])
}
