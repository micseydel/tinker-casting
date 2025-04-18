package me.micseydel.prototyping

import me.micseydel.prototyping.ObsidianCharts.Series
import spray.json.DefaultJsonProtocol._
import spray.json.enrichAny

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

  def chart(series: Series): String = {
    chart(series.data.map(_ => ""), List(series))
  }

  // model

  case class Series(title: String, data: List[Int])
}
