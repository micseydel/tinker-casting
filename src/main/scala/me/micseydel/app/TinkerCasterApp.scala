package me.micseydel.app

import cats.data.Validated
import me.micseydel.actor.*
import me.micseydel.actor.google.GoogleAuthManager
import me.micseydel.actor.hue.HueListener
import me.micseydel.actor.kitties.CatsHelper
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.actor.ollama.OllamaActor
import me.micseydel.actor.tasks.RecurringResponsibilityManager
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.*
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.Chronicler.ChroniclerConfig
import me.micseydel.util.TimeUtil

import java.time.ZonedDateTime
import scala.annotation.unused


object TinkerCasterApp {
  def main(args: Array[String]): Unit = {
    AppConfiguration.getConfig() match {
      case Validated.Invalid(errors) =>
        println(s"FAILED, errors-\n$errors")
      case Validated.Valid(config: AppConfig) =>
        println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Starting system: config with vault root ${config.vaultRoot}, creating json/ subdirectory if needed")
        run(config)
    }
  }

  def run(config: AppConfig): Unit = {
    // Chronicler, unlike my other actors, leverages EventReceiver for HTTP responses so needs non-note-config
    val chroniclerConfig = ChroniclerConfig(config.vaultRoot, config.eventReceiverHost, config.eventReceiverPort)

    @unused
    val container =
      TinkerContainer(config, NotificationCenterAbilities.Defaults)(
        centralCastFactory(chroniclerConfig)(_, _), // effectively globals
        UserTinkerCast(config.purpleAirReadAPIKey)(_: EnhancedTinker[MyCentralCast])
      )

    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] System done starting")
  }

  def centralCastFactory(config: ChroniclerConfig)(implicit Tinker: Tinker, context: TinkerContext[_]): MyCentralCast = {
    context.actorContext.log.info("Creating central cast with Chronicler, Gossiper and Rasa")
    val gossiper = context.cast(Gossiper(), "Gossiper")
    val chronicler = context.cast(Chronicler(config, gossiper), "Chronicler")
    val rasaActor = context.cast(RasaActor(), "RasaActor")
    MyCentralCast(chronicler, gossiper, rasaActor)
  }
}


case class MyCentralCast(
  chronicler: SpiritRef[Chronicler.Message],
  gossiper: SpiritRef[Gossiper.Message],
  rasa: SpiritRef[RasaActor.Message]
)


object UserTinkerCast {
  def apply(purpleAirApiKey: Option[String])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[ReceiveMqttEvent] = Tinker.setup { context =>
    @unused // registers with gossiper to listen for transcribed voice notes
    val hueListener = context.cast(HueListener(), "HueListener")

    @unused // registers various services with the Operator
    val google = context.cast(GoogleAuthManager(), "GoogleAuthManager")

    @unused // subscribes to gmail via operator
    val groceryManagerActor = context.cast(GroceryManagerActor(), "GroceryManagerActor")

    @unused // uses an internal folder watcher
    val ollamaActor = context.cast(OllamaActor(), "OllamaActor")

    @unused // subscribes to Gossiper
    val remindMeListenerActor = context.cast(RemindMeListenerActor(), "RemindMeListenerActor")

    @unused
    val centralNervousSystemMaintenance: SpiritRef[CentralNervousSystemMaintenance.Message] = context.cast(CentralNervousSystemMaintenance(), "CentralNervousSystemMaintenance")

    @unused
    val catsHelper: SpiritRef[CatsHelper.Message] = context.cast(kitties.CatsHelper(), "CatsHelper")

    @unused // runs itself via TimeKeeper
    val periodicNotesCreatorActor: SpiritRef[PeriodicNotesCreatorActor.Message] =
      context.cast(PeriodicNotesCreatorActor(), "PeriodicNotesCreatorActor")

    @unused // internally driven by time
    val recurringResponsibilityManager = context.cast(RecurringResponsibilityManager(), "RecurringResponsibilityManager")

    @unused // driven internally by a note
    val soundPlayerTestActor = context.cast(SoundPlayerTestActor(), "SoundPlayerTestActor")

    purpleAirApiKey match {
      case Some(value) =>
        context.cast(PurpleAirCloudActor(value), "PurpleAirCloudActor")
        context.actorContext.log.info("Started PurpleAirCloudActor")
      case None => context.actorContext.log.info("No PurpleAir API key found, not starting PurpleAirCloudActor")
    }


    Tinker.receiveMessage {
      case ReceiveMqttEvent(topic, payload) =>
        context.actorContext.log.warn(s"Unexpected topic $topic message, payload ${payload.length} bytes")
        Tinker.steadily
    }
  }
}

