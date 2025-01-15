package me.micseydel.actor.perimeter

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.perimeter.AranetActor.AranetResults
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{Operator, SpiritRef, Tinker, TinkerContext, Tinkerer}

import scala.concurrent.duration.DurationInt

object HomeMonitorActor {
  sealed trait Message

  sealed trait Monitoring extends Message
  case class SubscribeAranet4(subscriber: SpiritRef[AranetResults]) extends Monitoring

  case class StartTinkering(tinker: Tinker) extends Message

  private case object HeartBeat extends Message
  private case class ReceiveForSubscribers(results: AranetResults) extends Message

  def apply(aranet: Tinker => Behavior[AranetActor.Message]): Behavior[Message] = Behaviors.setup { context =>
    context.log.debug("Starting")
    initializing(aranet)
  }

  private def initializing(aranet: Tinker => Ability[AranetActor.Message]): Behavior[Message] = Behaviors.withStash(10) { stash =>
    Behaviors.receive { (context, message) =>
      message match {
        case StartTinkering(tinker) =>
          context.log.info("Initialized")
          stash.unstashAll(finishInitializing(aranet)(tinker))

        case monitoring: Monitoring =>
          context.log.info("Stashing")
          stash.stash(monitoring)
          Behaviors.same

        case msg@(HeartBeat | ReceiveForSubscribers(_)) =>
          context.log.warn(s"wasn't ready for $msg and expected not to get it yet, dropping")
          Behaviors.same
      }
    }
  }

  private def finishInitializing(aranetAbility: Tinker => Ability[AranetActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(100, 100, 255), "🏠").setup { context =>
    implicit val t: TinkerContext[_] = context

    context.actorContext.log.info("Starting CO_Monitor")
    val aranet: SpiritRef[AranetActor.Message] = context.cast(aranetAbility(Tinker), "CO_Monitor")

    context.actorContext.log.info("Started CO_Monitor; RemindMeEvery(10.minutes) Heartbeat")
    val timeKeeper = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(10.minutes, context.self, HeartBeat, None)

    context.system.operator !! Operator.RegisterHomeMonitor(context.self)

    behavior(aranet, Set.empty)
  }

  private def behavior(aranet: SpiritRef[AranetActor.Message], subscribers: Set[SpiritRef[AranetResults]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val t: TinkerContext[_] = context
    message match {
//      case r@RequestCO2(replyTo) =>
//        context.log.info(s"$r, fetching")
//        aranet !! AranetActor.Fetch(replyTo)
//        Tinker.steadily
      case SubscribeAranet4(subscriber) =>
        val newSet = subscribers + subscriber
        context.actorContext.log.info(s"adding subscriber $subscriber, count ${subscribers.size} -> ${newSet.size}")
        behavior(aranet, newSet)

      case HeartBeat =>
        context.actorContext.log.info(s"HeartBeat, fetching")
        aranet !! AranetActor.Fetch(context.messageAdapter(ReceiveForSubscribers))
        Tinker.steadily

      case ReceiveForSubscribers(results) =>
        context.actorContext.log.info(s"Received results, sending to ${subscribers.size} subscribers")
        for (subscriber <- subscribers) {
          subscriber !! results
        }
        Tinker.steadily

      case StartTinkering(tinker) =>
        context.actorContext.log.warn(s"Ignoring redundant tinker initialization: $tinker")
        Tinker.steadily
    }
  }
}
