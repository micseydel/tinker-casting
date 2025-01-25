package me.micseydel.actor

import me.micseydel.actor.kitties.CatsHelper
import me.micseydel.actor.ollama.OllamaActor
import me.micseydel.actor.wyze.WyzeActor
import me.micseydel.app.AppConfiguration.{AranetConfig, NtfyKeys}
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.vault.VaultPath

import java.nio.file.Path
import scala.annotation.unused

object TinkerOrchestrator {
  case class Config(vaultRoot: VaultPath,
                    audioNoteWatchPath: Path,
                    aranetConfig: Option[AranetConfig],
                    eventReceiverHost: String,
                    eventReceiverPort: Int,
                    ntfyKeys: NtfyKeys,
                    fitbitAuthorizationBasic: Option[String],
                    chimeHost: Option[String],
                    wyzeUri: Option[String]
                   )

  // behavior

  def apply(config: Config)(implicit Tinker: Tinker): Ability[ReceiveMqttEvent] = Tinker.setup[ReceiveMqttEvent] { context =>
//    val locationTracker: SpiritRef[LocationTracker.Message] = context.cast(LocationTracker(), "LocationTracker")

    config.wyzeUri match {
      case Some(wyzeUri) =>
        val wyzeActor = context.cast(WyzeActor(wyzeUri), "WyzeActor")
        context.actorContext.log.info(s"Started WyzeActor for URI $wyzeUri")
      case None =>
        context.actorContext.log.warn("No Wyze URI")
    }


    // !! specializations

    val llmTinkeringActor = context.cast(LLMTinkeringActor(), "LLMTinkeringActor")

    @unused // uses an internal folder watcher
    val ollamaActor = context.cast(OllamaActor(), "OllamaActor")

    @unused // subscribes to Gossiper
    val remindMeListenerActor = context.cast(RemindMeListenerActor(), "RemindMeListenerActor")

    // me :)
    @unused
    val centralNervousSystemMaintenance: SpiritRef[CentralNervousSystemMaintenance.Message] = context.cast(CentralNervousSystemMaintenance(config), "CentralNervousSystemMaintenance")

    // my cats

    @unused
    val catsHelper: SpiritRef[CatsHelper.Message] = context.cast(kitties.CatsHelper(), "CatsHelper")

    // high level note stuff

    @unused // runs itself via TimeKeeper
    val periodicNotesCreatorActor: SpiritRef[PeriodicNotesCreatorActor.Message] =
      context.cast(PeriodicNotesCreatorActor(config.vaultRoot), "PeriodicNotesCreatorActor")

    implicit val tc: TinkerContext[_] = context
    Tinker.withMessages {
//      case event@ReceiveMqttEvent(owntracks.Topic, _) =>
////        locationTracker !! LocationTracker.ReceiveMqtt(event)
//        Tinker.steadily

      case ReceiveMqttEvent(topic, payload) =>
        context.actorContext.log.warn(s"Unexpected topic $topic message, payload ${payload.length} bytes")
        Tinker.steadily
    }
  }
}
