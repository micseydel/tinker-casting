package me.micseydel.actor.perimeter

import me.micseydel.actor.wyze.WyzeActor
import me.micseydel.actor.{AirQualityManagerActor, PurpleAirActor}
import me.micseydel.app.AppConfiguration.AranetConfig
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import me.micseydel.dsl.cast.TimeKeeper

import scala.annotation.unused
import scala.concurrent.duration.DurationInt

object HomeMonitorActor {
  sealed trait Message

  sealed trait Monitoring extends Message
  case class SubscribeAranet4(subscriber: SpiritRef[AranetActor.Result]) extends Monitoring

  private case object HeartBeat extends Message
  private case class ReceiveForSubscribers(results: AranetActor.Result) extends Message

  //

  def apply(maybeAranetConfig: Option[AranetConfig], ntfyCO2Key: Option[String])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(100, 100, 255), "🏠").setup { context =>
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

    context.actorContext.log.info("Starting PurpleAir actor")
    @unused
    val purpleAirActor = context.cast(PurpleAirActor(), "PurpleAirActor")

    context.actorContext.log.info(s"Starting WyzeActor")
    val wyzeActor = context.cast(WyzeActor(), "WyzeActor")

    @unused // driven internally
    val airQualityManagerActor = for {
      aranetActor <- maybeAranetActor
    } yield {
      context.actorContext.log.info("Starting AirQualityManagerActor")
      context.cast(AirQualityManagerActor(purpleAirActor, wyzeActor, aranetActor), "AirQualityManagerActor")
    }

    if (airQualityManagerActor.isEmpty) {
      context.actorContext.log.warn(s"Failed to start AirQualityManagerActor: maybeAranetActor=$maybeAranetActor")
    }

    context.actorContext.log.info("Started CO2_Monitor; RemindMeEvery(10.minutes) Heartbeat")
    val timeKeeper = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(10.minutes, context.self, HeartBeat, None)
    context.self !! HeartBeat

    context.system.operator !! Operator.RegisterHomeMonitor(context.self)

    behavior(maybeAranetActor, Set.empty)
  }

  private def behavior(maybeAranet: Option[SpiritRef[AranetActor.Message]], subscribers: Set[SpiritRef[AranetActor.Result]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
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
