package me.micseydel.testsupport

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.Sender
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.Gossiper.{SubmitVote, Subscription}
import me.micseydel.model.{BaseModel, LargeModel, NotedTranscription}

object GossiperForTesting {
  def apply(): Behavior[Gossiper.Message] = Behaviors.setup { context =>
    val underlying: ActorRef[Gossiper.Message] = context.spawn(Gossiper(), "Gossiper")

    awaitingSubscriber(underlying)
  }

  /**
   * 1. Forward StartTinkering
   * 2. Buffer Gossiper.Receive
   * 3. Upon Subscriber, forward, then switch to pure forwarding and unload buffer
   */
  private def awaitingSubscriber(underlying: ActorRef[Gossiper.Message]): Behavior[Gossiper.Message] = Behaviors.withStash(10) { stash =>
    Behaviors.receive { (context, message) =>
      message match {
        case st@Gossiper.StartTinkering(_) =>
          context.log.info("Passthrough for StartTinkering")
          underlying ! st
          Behaviors.same

        case s@(_: Subscription) =>
          context.log.info(s"[state=passThrough] Passthrough for subscription, then unstashing ${stash.size} for ${s.subscriber.path}")
          underlying ! s
          stash.unstashAll(passThrough(underlying, Nil))

        case r@Gossiper.Receive(_) =>
          context.log.info(s"Buffering $r")
          stash.stash(r)
          Behaviors.same

        case SubmitVote(vote) => ???
      }
    }
  }

  private def passThrough(underlying: ActorRef[Gossiper.Message], pastTranscriptions: List[NotedTranscription]): Behavior[Gossiper.Message] = Behaviors.receive { (context, message) =>
    implicit val s: Sender = Sender(context.self.path)
    message match {
      case st@Gossiper.StartTinkering(_) =>
        underlying ! st
        Behaviors.same

      case r@Gossiper.Receive(notedTranscription) =>
        underlying ! r
        passThrough(underlying, notedTranscription :: pastTranscriptions)

      case subscription: Subscription =>
        val toSend: List[NotedTranscription] = subscription match {
          case Gossiper.SubscribeHybrid(_) => pastTranscriptions.reverse
          case Gossiper.SubscribeAccurate(_) => pastTranscriptions.reverse
            .filter(_.capture.whisperResult.whisperResultMetadata.model == LargeModel)
        }

        for (transcription <- toSend) {
          subscription.subscriber !!! transcription
        }

        underlying ! subscription

        Behaviors.same

      case SubmitVote(vote) => ???
    }
  }
}
