package me.micseydel.actor

import me.micseydel.actor.TinkerOrchestrator.Config
import me.micseydel.actor.perimeter.fitbit.FitbitActor
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker}

import scala.annotation.unused
import scala.concurrent.ExecutionContextExecutorService

object CentralNervousSystemMaintenance {
  sealed trait Message

  def apply(config: Config)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext
    val fitbitActor: SpiritRef[FitbitActor.Message] = context.cast(FitbitActor(config.fitbitAuthorizationBasic), "FitbitActor")

    context.actorContext.log.info("Started FitbitActor, starting Halto")

    @unused // driven internally by its own casting
    val halto = context.cast(Halto(fitbitActor, config.ntfyKeys), "Halto")

    @unused // sources events via EventReceiver
    val heartRateMonitor = context.cast(HeartRateMonitorActor(), "HeartRateMonitorActor")

    Tinker.ignore
  }
}
