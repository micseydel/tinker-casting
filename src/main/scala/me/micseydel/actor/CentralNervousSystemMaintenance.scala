package me.micseydel.actor

import me.micseydel.actor.TinkerOrchestrator.Config
import me.micseydel.actor.inactive.fitbit.{FitbitActor, FitbitTesterActor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, Tinkerer}

import scala.annotation.unused

object CentralNervousSystemMaintenance {
  sealed trait Message

  def apply(config: Config)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor(100, 100, 200), "😵‍💫").setup { context =>
//    @unused // driven internally by its own casting
//    val halto = context.cast(Halto(fitbitActor, config.ntfyKeys), "Halto")

    @unused // sources events via EventReceiver
    val heartRateMonitor = context.cast(HeartRateMonitorActor(), "HeartRateMonitorActor")

    Tinker.ignore
  }
}
