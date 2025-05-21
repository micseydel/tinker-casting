package me.micseydel.actor.perimeter

import me.micseydel.actor.wyze.WyzeActor
import me.micseydel.actor.{AirQualityManagerActor, PurpleAirActor}
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

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(100, 100, 255), "🏠").setup { context =>
    implicit val t: TinkerContext[_] = context

    val aranetActor: SpiritRef[AranetActor.Message] = context.cast(AranetActor(), "CO2_Monitor")

    context.actorContext.log.info("Starting PurpleAir actor")
    @unused
    val purpleAirActor = context.cast(PurpleAirActor(), "PurpleAirActor")

    context.actorContext.log.info(s"Starting WyzeActor")
    val wyzeActor = context.cast(WyzeActor(), "WyzeActor")

    @unused // driven internally
    val airQualityManagerActor = {
      context.actorContext.log.info("Starting AirQualityManagerActor")
      context.cast(AirQualityManagerActor(purpleAirActor, wyzeActor, aranetActor), "AirQualityManagerActor")
    }

    context.actorContext.log.info("Started CO2_Monitor; RemindMeEvery(10.minutes) Heartbeat")
    val timeKeeper = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(10.minutes, context.self, HeartBeat, None)
    context.self !! HeartBeat

    context.system.operator !! Operator.RegisterHomeMonitor(context.self)

    behavior(aranetActor, Set.empty)
  }

  private def behavior(aranetActor: SpiritRef[AranetActor.Message], subscribers: Set[SpiritRef[AranetActor.Result]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val t: TinkerContext[_] = context
    message match {
      case SubscribeAranet4(subscriber) =>
        val newSet = subscribers + subscriber
        context.actorContext.log.info(s"adding subscriber $subscriber, count ${subscribers.size} -> ${newSet.size}")
        behavior(aranetActor, newSet)

      case HeartBeat =>
        context.actorContext.log.info(s"HeartBeat, fetching")
        aranetActor !! AranetActor.Fetch(context.messageAdapter(ReceiveForSubscribers))
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
