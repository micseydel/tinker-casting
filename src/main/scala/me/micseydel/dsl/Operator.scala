package me.micseydel.dsl

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.perimeter.{AranetActor, HomeMonitorActor}
import me.micseydel.dsl.Tinker.Ability

object Operator {
  sealed trait Message

  sealed trait Register extends Message
  case class RegisterHomeMonitor(registrant: SpiritRef[HomeMonitorActor.Monitoring]) extends Register

  sealed trait Subscribe extends Message
  case class SubscribeAranet4(subscriber: SpiritRef[AranetActor.Result]) extends Subscribe

  // behavior

  def apply(): Ability[Message] = Behaviors.setup { context =>
    context.log.info("Starting operator")
    ability(None)
  }

  private def ability(homeMonitor: Option[SpiritRef[HomeMonitorActor.Monitoring]]): Ability[Message] = Behaviors.receive { (context, message) =>
    implicit val sender: Sender = Sender(context.self.path)

    message match {
      case RegisterHomeMonitor(registrant) =>
        homeMonitor match {
          case Some(oldRegistration) =>
            context.log.warn(s"Registering ${registrant.path} as HomeMonitor, replacing ${oldRegistration.path}")
          case None =>
            context.log.info(s"Registering ${registrant.path} as HomeMonitor")
        }
        ability(Some(registrant))
      case SubscribeAranet4(subscriber) =>
        homeMonitor match {
          case Some(homeMonitorRef) =>
            context.log.info(s"Sending new subscriber ${subscriber.path} to home monitor...")
            homeMonitorRef !!! HomeMonitorActor.SubscribeAranet4(subscriber)
          case None =>
            context.log.warn(s"New subscriber ${subscriber.path} to home monitor but no registered home monitor")
        }
        Behaviors.same
    }
  }
}
