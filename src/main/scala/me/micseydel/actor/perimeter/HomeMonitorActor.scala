package me.micseydel.actor.perimeter

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.{AirQualityManagerActor, PurpleAirActor}
import me.micseydel.actor.perimeter.AranetActor.AranetResults
import me.micseydel.actor.wyze.WyzeActor
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
  case class SubscribeAranet4(subscriber: SpiritRef[AranetActor.Result]) extends Monitoring

  private case object HeartBeat extends Message
  private case class ReceiveForSubscribers(results: AranetActor.Result) extends Message

  //

  def apply(maybeAranetConfig: Option[AranetConfig], ntfyCO2Key: Option[String], maybePurpleAirURI: Option[String], wyzeUri: Option[String])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(100, 100, 255), "🏠").setup { context =>
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

    val maybePurpleAirActor = maybePurpleAirURI match {
      case Some(purpleAirURI) =>
        context.actorContext.log.info("Starting PurpleAir actor")
        Some(context.cast(PurpleAirActor(purpleAirURI), "PurpleAirActor"))
      case None =>
        context.actorContext.log.warn("No PurpleAir URI in config, will not measure AQI")
        None
    }

    val maybeWyzeActor: Option[SpiritRef[WyzeActor.Message]] = wyzeUri match {
      case Some(wyzeUri) =>
        context.actorContext.log.info(s"Starting WyzeActor for URI $wyzeUri")
        Some(context.cast(WyzeActor(wyzeUri), "WyzeActor"))
      case None =>
        context.actorContext.log.warn("No Wyze URI, creating an inert actor")
        None
    }

    @unused // driven internally
    val airQualityManagerActor = for {
      purpleAirActor <- maybePurpleAirActor
      wyzeActor <- maybeWyzeActor
      aranetActor <- maybeAranetActor
    } yield {
      context.actorContext.log.info("Starting AirQualityManagerActor")
      context.cast(AirQualityManagerActor(purpleAirActor, wyzeActor, aranetActor), "AirQualityManagerActor")
    }

    if (airQualityManagerActor.isEmpty) {
      context.actorContext.log.warn(s"Failed to start AirQualityManagerActor: maybePurpleAirActor=$maybePurpleAirActor, maybeWyzeActor=$maybeWyzeActor, maybeAranetActor=$maybeAranetActor")
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
