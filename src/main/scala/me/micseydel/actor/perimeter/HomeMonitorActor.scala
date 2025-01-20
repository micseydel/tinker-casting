package me.micseydel.actor.perimeter

import me.micseydel.actor.PurpleAirActor
import me.micseydel.actor.perimeter.AranetActor.AranetResults
import me.micseydel.app.AppConfiguration.AranetConfig
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl._

import scala.annotation.unused
import scala.concurrent.duration.DurationInt

object HomeMonitorActor {
  sealed trait Message

  sealed trait Monitoring extends Message
  case class SubscribeAranet4(subscriber: SpiritRef[AranetResults]) extends Monitoring

  private case object HeartBeat extends Message
  private case class ReceiveForSubscribers(results: AranetResults) extends Message

  //

  def apply(maybeAranetConfig: Option[AranetConfig], ntfyCO2Key: Option[String], maybePurpleAirURI: Option[String])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(100, 100, 255), "🏠").setup { context =>
    implicit val t: TinkerContext[_] = context

    val maybeAranetActor = maybeAranetConfig match {
      case Some(AranetConfig(host, port)) =>
        context.actorContext.log.info("Starting CO2_Monitor")
        val aranet: SpiritRef[AranetActor.Message] = context.cast(AranetActor(AranetActor.Config(host, port, ntfyCO2Key)), "CO2_Monitor")
        Some(aranet)
      case None =>
        context.actorContext.log.info("No Aranet config, NOT starting CO2_Monitor")
        None
    }

    maybePurpleAirURI match {
      case Some(purpleAirURI) =>
        context.actorContext.log.info("Starting PurpleAir actor")
        @unused // driven by an internal timer
        val purpleAir = context.cast(PurpleAirActor(purpleAirURI), "PurpleAirActor")
      case None =>
        context.actorContext.log.warn("No PurpleAir URI in config, will not measure AQI")
    }


    context.actorContext.log.info("Started CO_Monitor; RemindMeEvery(10.minutes) Heartbeat")
    val timeKeeper = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(10.minutes, context.self, HeartBeat, None)

    context.system.operator !! Operator.RegisterHomeMonitor(context.self)

    behavior(maybeAranetActor, Set.empty)
  }

  private def behavior(maybeAranet: Option[SpiritRef[AranetActor.Message]], subscribers: Set[SpiritRef[AranetResults]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val t: TinkerContext[_] = context
    message match {
      case SubscribeAranet4(subscriber) =>
        val newSet = subscribers + subscriber
        context.actorContext.log.info(s"adding subscriber $subscriber, count ${subscribers.size} -> ${newSet.size}")
        behavior(maybeAranet, newSet)

      case HeartBeat =>
        maybeAranet match {
          case Some(aranet) =>
            context.actorContext.log.info(s"HeartBeat, fetching")
            aranet !! AranetActor.Fetch(context.messageAdapter(ReceiveForSubscribers))
          case None =>
            context.actorContext.log.info(s"Ignoring heartbeat, no Aranet config")
        }
        Tinker.steadily

      case ReceiveForSubscribers(results) =>
        context.actorContext.log.info(s"Received results, sending to ${subscribers.size} subscribers")
        for (subscriber <- subscribers) {
          subscriber !! results
        }
        Tinker.steadily
    }
  }
}
