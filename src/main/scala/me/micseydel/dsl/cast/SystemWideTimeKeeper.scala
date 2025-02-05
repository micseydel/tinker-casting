package me.micseydel.dsl.cast

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.{Sender, SpiritRef}

object SystemWideTimeKeeper {
  sealed trait Message
  case object ItsMidnight extends Message
  case class SubscribeMidnight(replyTo: SpiritRef[ItsMidnight.type]) extends Message

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    val timeKeeper = context.spawn(UntrackedTimeKeeper(), "UntrackedTimeKeeper")

    context.log.info("Subscribing ItsMidnight message through TimeKeeper")
    timeKeeper ! UntrackedTimeKeeper.RemindMeDailyAt(0, 0, context.self, ItsMidnight, None)

    behavior(Set.empty)
  }

  private def behavior(subscribers: Set[SpiritRef[ItsMidnight.type]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case SubscribeMidnight(replyTo) =>
        val newSubscribers: Set[SpiritRef[ItsMidnight.type]] = subscribers + replyTo
        context.log.info(s"${replyTo.path} just subscribed to updates for midnight, ${newSubscribers.size} subscribers now")
        behavior(newSubscribers)

      case ItsMidnight =>
        context.log.info(s"Notifying ${subscribers.size} subscribers that it's midnight")
        implicit val sender: Sender = Sender(context.self.path)
        for (subscriber <- subscribers) {
          subscriber !!! ItsMidnight
        }

        Behaviors.same
    }
  }
}
