package me.micseydel.prototyping

import com.typesafe.config.{Config, ConfigException, ConfigFactory}

object ConfigReader {
  def main(args: Array[String]): Unit = {
    val config: Config = ConfigFactory.load("application.conf")

    try {
      val relativeWatchPath = config.getString("vault.root") // config.getString("dka.relative-watch-path")
      println(s"Relative watch path: $relativeWatchPath")
    } catch {
      case e: ConfigException.Missing =>
        println(s"Configuration key not found: ${e.getMessage}")

      case e: ConfigException.WrongType =>
        println(s"Configuration has the wrong type: ${e.getMessage}")

      case e: ConfigException.Parse =>
        println(s"Could not parse the configuration: ${e.getMessage}")

      case e: Exception =>
        println(s"Other error while reading the configuration: ${e.getMessage}")
    }
  }
}
