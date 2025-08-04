package me.micseydel.prototyping

import spray.json.DefaultJsonProtocol.*
import spray.json.enrichAny

object ObsidianCharts {
  def chart(labels: List[String], series: List[Series[_]]): String = {
    val formattedSeries = series.map {
      case DoubleSeries(title, data) =>
        s"""    - title: "$title"
           |      data: ${data.toJson.compactPrint}
           |""".stripMargin
      case IntSeries(title, data) =>
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

  def chart(series: List[Series[_]]): String = {
    chart(series.map(_ => ""), series)
  }

  def chart(series: Series[_]): String = {
    chart(series.data.map(_ => ""), List(series))
  }

  // model

  sealed trait Series[T] {
    def title: String
    def data: List[T]
  }

  case class IntSeries(title: String, data: List[Int]) extends Series[Int]
  case class DoubleSeries(title: String, data: List[Double]) extends Series[Double]
}
