package me.micseydel.app

import akka.actor
import cats.data.Validated
import me.micseydel.Common
import me.micseydel.dsl.TinkerContainer

import scala.annotation.unused
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.util.TimeUtil
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Paths}
import java.time.ZonedDateTime


object TinkerCasterApp {
  // FIXME: what should the userspace "app" be responsible for?
  //   - minimally?
  //       - Rasa
  //   - ntfy implementation
  def main(args: Array[String]): Unit = {
    AppConfiguration.getConfig() match {
      case Validated.Invalid(errors) =>
        println("FAILED, errors-")
        println(errors)
      case Validated.Valid(config) =>
//        println(s"[${Common.zonedDateTimeToISO8601(ZonedDateTime.now())}] Using config: $config")
        println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Using config with vault root ${config.vaultRoot}, creating json/ subdirectory if needed")
        // ensure json subdirectory exists
        Files.createDirectories(config.vaultRoot.resolve("json"))
        run(config)
    }
  }

  def run(config: AppConfig): Unit = {
    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Starting system...")
    // this line suppresses -
    //   SLF4J: A number (1) of logging calls during the initialization phase have been intercepted and are
    //   SLF4J: now being replayed. These are subject to the filtering rules of the underlying logging system.
    //   SLF4J: See also https://www.slf4j.org/codes.html#replay
    LoggerFactory.getILoggerFactory
    // https://doc.akka.io/docs/akka/current/typed/logging.html#slf4j-api-compatibility wasn't as good

    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Starting system")
    @unused
    val container: actor.ActorSystem = TinkerContainer(config)

    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] System done starting")
  }
}
