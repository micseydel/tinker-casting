package me.micseydel.app

import cats.data.{Validated, ValidatedNel}
import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import me.micseydel.actor.perimeter.HueControl.HueConfig
import me.micseydel.vault.VaultPath

import scala.util.{Failure, Success, Try}

object AppConfiguration {
  case class AppConfig(
    vaultRoot: VaultPath,
    aranetConfig: Option[AranetConfig],
    eventReceiverHost: String,
    eventReceiverPort: Int,
    ntfyKeys: NtfyKeys,
    hueConfig: Option[HueConfig],
    mqttConfig: Option[MqttConfig],
    fitbitAuthorizationBasic: Option[String],
    purpleAirUri: Option[String],
    wyzeUri: Option[String],
    rasaHost: String
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

        val aranetConfig: Option[AranetConfig] = getOptionalString(config, "aranet.host").flatMap { host =>
          getOptionalString(config, "aranet.port").map(_.toInt).map { port =>
            AranetConfig(host, port)
          }
        }

        val hueConfig: Option[HueConfig] = for {
          ip <- getOptionalString(config, "hue_api.ip")
          username <- getOptionalString(config, "hue_api.username")
        } yield HueConfig(ip, username)

        Validated.Valid(AppConfig(
          vaultRoot,
          aranetConfig,
          config.getString("transcription.whisper.event-receiver.host"),
          config.getInt("transcription.whisper.event-receiver.port"),
          NtfyKeys(
            getOptionalString(config, "ntfy-keys.foodTimeKey"),
            getOptionalString(config, "ntfy-keys.highCO2Key"),
            getOptionalString(config, "ntfy-keys.searchSpaceKey")
          ),
          hueConfig,
          maybeMqttConfig,
          getOptionalString(config, "fitbit.authorizationBasic"),
          getOptionalString(config, "purpleair.uri"),
          getOptionalString(config, "wyze.uri"),
          config.getString("rasa.host")
        ))
    }
  }

  def getOptionalString(config: Config, key: String): Option[String] = {
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

  case class AranetConfig(host: String, port: Int)
}
