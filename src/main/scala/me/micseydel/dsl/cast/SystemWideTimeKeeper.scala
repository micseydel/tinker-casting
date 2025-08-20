package me.micseydel.dsl.cast

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.{MillisFromMidnight, MillisSince, MillisUntil}
import me.micseydel.dsl.{Sender, SpiritRef}

import java.time.{LocalDate, ZonedDateTime}

object SystemWideTimeKeeper {
  sealed trait Message
  case object ItsMidnight extends Message
  case class SubscribeMidnight(replyTo: SpiritRef[ZonedDateTime]) extends Message

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    val timeKeeper = context.spawn(UntrackedTimeKeeper(), "UntrackedTimeKeeper")

    context.log.info("Subscribing ItsMidnight message through TimeKeeper")
    timeKeeper ! UntrackedTimeKeeper.RemindMeDailyAt(0, 0, context.self, ItsMidnight, None)

    behavior(Set.empty)
  }

  private def behavior(subscribers: Set[SpiritRef[ZonedDateTime]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case SubscribeMidnight(replyTo) =>
        val newSubscribers: Set[SpiritRef[ZonedDateTime]] = subscribers + replyTo
        context.log.info(s"${replyTo.path} just subscribed to updates for midnight, ${newSubscribers.size} subscribers now")
        behavior(newSubscribers)

      case ItsMidnight =>
        val midnightFor = {
          // FIXME: ZonedDateTime is fine here because Operator needs to be mocked for the tinker system
          val now = ZonedDateTime.now()
          MillisFromMidnight(now) match {
            case MillisUntil(_) => now.plusDays(1)
            case MillisSince(_) => now
          }
        }.withHour(0).withMinute(0)

        context.log.info(s"Notifying ${subscribers.size} subscribers that it's midnight: $subscribers")
        implicit val sender: Sender = Sender(context.self.path)

        for (subscriber <- subscribers) {
          subscriber !!! midnightFor
        }

        Behaviors.same
    }
  }
}
