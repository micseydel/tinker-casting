package me.micseydel.prototyping

import cats.data.ValidatedNel
import cats.implicits.catsSyntaxValidatedId
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
    chart(series.headOption.toList.flatMap(_.data.map(_ => "")), series)
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

  // FIXME: javadoc
  def averageOfLastN(elements: List[Int], lookback: Int = 7): ValidatedNel[String, List[Double]] = {
    val size = elements.size

    val result: List[Double] = elements.indices.drop(lookback).map { i =>
      val window = elements.slice(i - lookback, i + 1)
      window.sum.toDouble / window.length
    }.toList

    if (result.isEmpty) {
      s"Not enough elements ($size) for lookback ($lookback)".invalidNel
    } else {
      result.valid
    }
  }

  // FIXME proper tests!!!!
  //   the result size should be its input -N, right?
  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3)
    println(averageOfLastN(list, 1))
  }
}
