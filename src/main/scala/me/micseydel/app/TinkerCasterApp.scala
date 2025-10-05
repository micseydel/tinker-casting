package me.micseydel.app

import cats.data.Validated
import me.micseydel.NoOp
import me.micseydel.actor.*
import me.micseydel.actor.google.GoogleAuthManager
import me.micseydel.actor.hue.HueListener
import me.micseydel.actor.kitties.CatsHelper
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.actor.ollama.OllamaActor
import me.micseydel.actor.perimeter.HomeMonitorActor
import me.micseydel.actor.tasks.{RecurringResponsibilityManager, TaskManager}
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.Chronicler.ChroniclerConfig
import me.micseydel.util.TimeUtil
import me.micseydel.vault.VaultPath

import java.nio.file.Path
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
    val taskNotesTasksPath = config.vaultRoot.resolve("TaskNotes/Tasks/")

    @unused
    val container =
      TinkerContainer(config, NotificationCenterAbilities.Defaults)(
        centralCastFactory(config.vaultRoot)(_, _), // effectively globals
        UserTinkerCast(config.purpleAirReadAPIKey, taskNotesTasksPath)(_: EnhancedTinker[MyCentralCast])
      )

    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] System done starting")
  }

  def centralCastFactory(vaultRoot: VaultPath)(implicit Tinker: Tinker, context: TinkerContext[_]): MyCentralCast = {
    context.actorContext.log.info("Creating central cast with Chronicler, Gossiper and Rasa")
    val gossiper = context.cast(Gossiper(), "Gossiper")
    val chronicler = context.cast(Chronicler(vaultRoot, gossiper), "Chronicler")
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
  def apply(purpleAirApiKey: Option[String], taskNotesTasksPath: Path)(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[NoOp.type] = Tinker.setup { context =>
    @unused
    val appRestartTracker = context.cast(AppRestartTracker(), "AppRestartTracker")

    @unused // registers with gossiper to listen for transcribed voice notes
    val hueListener = context.cast(HueListener(), "HueListener")

    @unused // registers various services with the Operator
    val google = context.cast(GoogleAuthManager(), "GoogleAuthManager")

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

//    @unused
//    val taskManager = context.cast(TaskManager(taskNotesTasksPath), "TaskManager")

//    @unused // driven internally by a note
//    val soundPlayerTestActor = context.cast(SoundPlayerTestActor(), "SoundPlayerTestActor")

    @unused // subscribes to gmail via operator
    val groceryManagerActor = context.cast(GroceryManagerActor(), "GroceryManagerActor")

    @unused // driven internally
    val homeMonitor = context.spawn(HomeMonitorActor(purpleAirApiKey), "HomeMonitor")

    Tinker.receiveMessage {
      case NoOp =>
        context.actorContext.log.warn("didn't expect to receive a message")
        Tinker.steadily
    }
  }
}

