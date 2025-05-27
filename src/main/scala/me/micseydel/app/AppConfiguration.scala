package me.micseydel.app

import cats.data.{Validated, ValidatedNel}
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import me.micseydel.actor.perimeter.HueControl.HueConfig
import me.micseydel.vault.VaultPath

import scala.util.{Failure, Success, Try}

object AppConfiguration {
  case class AppConfig(
    vaultRoot: VaultPath,
    eventReceiverHost: String,
    eventReceiverPort: Int,
    ntfyKeys: NtfyKeys,
    mqttConfig: Option[MqttConfig],
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
          config.getString("transcription.whisper.event-receiver.host"),
          config.getInt("transcription.whisper.event-receiver.port"),
          NtfyKeys(
            getOptionalString(config, "ntfy-keys.foodTimeKey"),
            getOptionalString(config, "ntfy-keys.highCO2Key"),
            getOptionalString(config, "ntfy-keys.searchSpaceKey")
          ),
          maybeMqttConfig
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

  // specialized config

  case class MqttConfig(
                         username: String,
                         password: String,
                         brokerUrl: String
                       )

  case class NtfyKeys(
                     foodTime: Option[String],
                     highCO2: Option[String],
                     searchSpace: Option[String]
                     )
}
