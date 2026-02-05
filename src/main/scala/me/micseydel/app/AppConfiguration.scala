package me.micseydel.app

import cats.data.{Validated, ValidatedNel}
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import me.micseydel.vault.VaultPath

import scala.util.{Failure, Success, Try}

object AppConfiguration {
  case class AppConfig(
    vaultRoot: VaultPath,
    mqttConfig: Option[MqttConfig],
    purpleAirReadAPIKey: Option[String],
    tinkerbrainPort: Option[Int],
    pirateWeatherApiKey: Option[String]
  )

  /**
   * Fetches from disk the validated configuration.
   */
  private[app] def getConfig(): ValidatedNel[String, AppConfig] = {
    val config: Config = ConfigFactory.load()

    VaultPath(config.getString("vault.root")) match {
      case invalid@Validated.Invalid(_) =>
        invalid.toValidatedNel

      case Validated.Valid(vaultRoot) =>
        val maybeMqttConfig: Option[MqttConfig] = (Try(config.getConfig("mqtt")) match {
          case Success(value) => Some(value)
          case Failure(_: ConfigException.Missing) => None
          case Failure(exception) => throw exception
        }).flatMap { subConfig =>
          for {
            username <- getOptionalString(subConfig, "username")
            password <- getOptionalString(subConfig, "password")
            brokerUrl <- getOptionalString(subConfig, "brokerUrl")
          } yield MqttConfig(username, password, brokerUrl)
        }

        Validated.Valid(AppConfig(
          vaultRoot,
          maybeMqttConfig,
          getOptionalString(config, "purpleAir.readAPIKey"),
          getOptionalInt(config, "vault.tinkerbrainPort"),
          getOptionalString(config, "pirateWeather.apiKey")
        ))
    }
  }

  private def getOptionalString(config: Config, key: String): Option[String] = {
    try {
      Some(config.getString(key))
    } catch {
      case _: ConfigException.Missing =>
        None
    }
  }

  private def getOptionalInt(config: Config, key: String): Option[Int] = {
    try {
      Some(config.getString(key).toInt)
    } catch {
      case _: ConfigException.Missing =>
        None
    }
  }

  // specialized config

  case class MqttConfig(
                         username: String,
                         password: String,
                         brokerUrl: String
                       )
}
