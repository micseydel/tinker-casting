package me.micseydel.actor

import akka.actor.typed.ActorRef
import me.micseydel.actor.TinkerOrchestrator.Config
import me.micseydel.actor.kitties.CatsHelper
import me.micseydel.actor.ollama.OllamaActor
import me.micseydel.app.AppConfiguration.{AppConfig, NtfyKeys}
import me.micseydel.app.MyCentralCast
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

  def apply[CentralCast](centralCastFactory: (Tinker, TinkerContext[_]) => CentralCast, userCast: EnhancedTinker[CentralCast] => Ability[ReceiveMqttEvent])(implicit Tinker: Tinker): Ability[ReceiveMqttEvent] = Tinker.setup[ReceiveMqttEvent] { context =>

    // FIXME: I need to create the central cast HERE, so shunt the EnhancedTinker DOWN
    //   gmail should actually be in the CentralCast I think... maybe keep with Operator and try both?
    //   try leaving ollama there - makes sense with Whisper and Rasa

    val enhancedTinker: EnhancedTinker[CentralCast] = new EnhancedTinker[CentralCast](context.system, centralCastFactory(Tinker, context))

    userCast(enhancedTinker)
  }
}
