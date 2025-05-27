package me.micseydel.app

import akka.actor
import cats.data.Validated
import me.micseydel.actor.kitties.CatsHelper
import me.micseydel.actor.{CentralNervousSystemMaintenance, GmailExperimentActor, GroceryManagerActor, HueListener, PeriodicNotesCreatorActor, RasaActor, RemindMeListenerActor, kitties}
import me.micseydel.actor.notifications.ChimeActor
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.actor.ollama.OllamaActor
import me.micseydel.actor.perimeter.{HueControl, NtfyerActor}
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerOrchestrator.ConfigToSimplifyAway
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.Chronicler.ChroniclerConfig
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, Tinker, TinkerContainer, TinkerContext}
import me.micseydel.util.TimeUtil
import org.slf4j.LoggerFactory

import java.nio.file.Files
import java.time.ZonedDateTime
import scala.annotation.unused


object TinkerCasterApp {
  def main(args: Array[String]): Unit = {
    AppConfiguration.getConfig() match {
      case Validated.Invalid(errors) =>
        println("FAILED, errors-")
        println(errors)
      case Validated.Valid(config) =>
        println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Using config with vault root ${config.vaultRoot}, creating json/ subdirectory if needed")

        // ensure json subdirectory exists
        Files.createDirectories(config.vaultRoot.resolve("json"))

        println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Starting system...")
        // this line suppresses -
        //   SLF4J: A number (1) of logging calls during the initialization phase have been intercepted and are
        //   SLF4J: now being replayed. These are subject to the filtering rules of the underlying logging system.
        //   SLF4J: See also https://www.slf4j.org/codes.html#replay
        LoggerFactory.getILoggerFactory
        // https://doc.akka.io/docs/akka/current/typed/logging.html#slf4j-api-compatibility wasn't as good

        run(config)
    }
  }

  def run(config: AppConfig): Unit = {
    val notificationCenterAbilities = NotificationCenterAbilities(
      NtfyerActor()(_),
      HueControl()(_),
      ChimeActor()(_)
    )

    val chroniclerConfig = ChroniclerConfig(config.vaultRoot, config.eventReceiverHost, config.eventReceiverPort)

    val orchestratorConfig = ConfigToSimplifyAway(
      config.vaultRoot,
      config.ntfyKeys
    )

    @unused
    val container: actor.ActorSystem =
      TinkerContainer(config, notificationCenterAbilities)(
        centralCastFactory(chroniclerConfig),
        UserTinkerCast(orchestratorConfig)(_: EnhancedTinker[MyCentralCast])
      )

    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] System done starting")
  }

  def centralCastFactory(config: ChroniclerConfig)(Tinker: Tinker, context: TinkerContext[_]): MyCentralCast = {
    context.actorContext.log.info("Creating central cast with Chronicler, Gossiper and Rasa")
    val gossiper = context.cast(Gossiper()(Tinker), "Gossiper")
    val chronicler = context.cast(Chronicler(config, gossiper)(Tinker), "Chronicler")
    val rasaActor = context.cast(RasaActor()(Tinker), "RasaActor")
    MyCentralCast(chronicler, gossiper, rasaActor)
  }
}


case class MyCentralCast(
                    chronicler: SpiritRef[Chronicler.Message],
                    gossiper: SpiritRef[Gossiper.Message],
                    rasa: SpiritRef[RasaActor.Message]
                    )


object UserTinkerCast {

  def apply(config: ConfigToSimplifyAway)(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[ReceiveMqttEvent] = Tinker.setup { context =>
    @unused // registers with gossiper to listen for transcribed voice notes
    val hueListener = context.cast(HueListener(), "HueListener")

    @unused // registers with the Operator
    val gmailActor = context.cast(GmailExperimentActor(), "GmailTestActor")

    @unused // subscribes to gmail via operator
    val groceryManagerActor = context.cast(GroceryManagerActor(), "GroceryManagerActor")

    @unused // uses an internal folder watcher
    val ollamaActor = context.cast(OllamaActor(), "OllamaActor")

    @unused // subscribes to Gossiper
    val remindMeListenerActor = context.cast(RemindMeListenerActor(), "RemindMeListenerActor")

    @unused
    val centralNervousSystemMaintenance: SpiritRef[CentralNervousSystemMaintenance.Message] = context.cast(CentralNervousSystemMaintenance(config), "CentralNervousSystemMaintenance")

    @unused
    val catsHelper: SpiritRef[CatsHelper.Message] = context.cast(kitties.CatsHelper(), "CatsHelper")

    // high level note stuff

    @unused // runs itself via TimeKeeper
    val periodicNotesCreatorActor: SpiritRef[PeriodicNotesCreatorActor.Message] =
      context.cast(PeriodicNotesCreatorActor(config.vaultRoot), "PeriodicNotesCreatorActor")

    Tinker.receiveMessage {
      case ReceiveMqttEvent(topic, payload) =>
        context.actorContext.log.warn(s"Unexpected topic $topic message, payload ${payload.length} bytes")
        Tinker.steadily
    }
  }
}

