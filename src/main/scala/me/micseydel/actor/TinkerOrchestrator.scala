package me.micseydel.actor

import akka.actor.typed.ActorRef
import me.micseydel.actor.kitties.CatsHelper
import me.micseydel.actor.ollama.OllamaActor
import me.micseydel.app.AppConfiguration.NtfyKeys
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.vault.VaultPath

import scala.annotation.unused

object TinkerOrchestrator {
  case class Config(vaultRoot: VaultPath,
                    ntfyKeys: NtfyKeys,
                    gmail: Option[GmailConfig]
                   )

  // behavior

  def apply(config: Config)(implicit Tinker: EnhancedTinker[ActorRef[RasaActor.Message]]): Ability[ReceiveMqttEvent] = Tinker.setup[ReceiveMqttEvent] { context =>
    config.gmail.foreach { gmailConfig =>
      context.actorContext.log.debug("Casting GmailTestActor")
      context.cast(GmailExperimentActor(gmailConfig), "GmailTestActor")
    }

    @unused // subscribes to gmail
    val groceryManagerActor = context.cast(GroceryManagerActor(), "GroceryManagerActor")

    // !! specializations

//    @unused // driven internally
//    val llmTinkeringActor = context.cast(LLMTinkeringActor(), "LLMTinkeringActor")

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
    Tinker.receiveMessage {
//      case event@ReceiveMqttEvent(owntracks.Topic, _) =>
////        locationTracker !! LocationTracker.ReceiveMqtt(event)
//        Tinker.steadily

      case ReceiveMqttEvent(topic, payload) =>
        context.actorContext.log.warn(s"Unexpected topic $topic message, payload ${payload.length} bytes")
        Tinker.steadily
    }
  }
}
