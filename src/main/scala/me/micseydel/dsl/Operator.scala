package me.micseydel.dsl

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.perimeter.{AranetActor, HomeMonitorActor}
import me.micseydel.dsl.cast.SystemWideTimeKeeper
import me.micseydel.dsl.cast.SystemWideTimeKeeper.ItsMidnight

object Operator {
  sealed trait Message

  final case class SubscribeMidnight(replyTo: SpiritRef[ItsMidnight.type]) extends Message

  sealed trait Register extends Message
  case class RegisterHomeMonitor(registrant: SpiritRef[HomeMonitorActor.Monitoring]) extends Register

  sealed trait Subscribe extends Message
  case class SubscribeAranet4(subscriber: SpiritRef[AranetActor.Result]) extends Subscribe

  // behavior

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("Starting operator")
    val systemWideTimeKeeper: ActorRef[SystemWideTimeKeeper.Message] = context.spawn(SystemWideTimeKeeper(), "SystemWideTimeKeeper")
    behavior(systemWideTimeKeeper, None)
  }

  private def behavior(systemWideTimeKeeper: ActorRef[SystemWideTimeKeeper.Message], homeMonitor: Option[SpiritRef[HomeMonitorActor.Monitoring]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    implicit val sender: Sender = Sender(context.self.path)

    message match {
      case RegisterHomeMonitor(registrant) =>
        homeMonitor match {
          case Some(oldRegistration) =>
            context.log.warn(s"Registering ${registrant.path} as HomeMonitor, replacing ${oldRegistration.path}")
          case None =>
            context.log.info(s"Registering ${registrant.path} as HomeMonitor")
        }
        behavior(systemWideTimeKeeper, Some(registrant))

      case SubscribeAranet4(subscriber) =>
        homeMonitor match {
          case Some(homeMonitorRef) =>
            context.log.info(s"Sending new subscriber ${subscriber.path} to home monitor...")
            homeMonitorRef !!! HomeMonitorActor.SubscribeAranet4(subscriber)
          case None =>
            context.log.warn(s"New subscriber ${subscriber.path} to home monitor but no registered home monitor")
        }
        Behaviors.same

      case SubscribeMidnight(replyTo) =>
        systemWideTimeKeeper ! SystemWideTimeKeeper.SubscribeMidnight(replyTo)
        Behaviors.same
    }
  }
}
