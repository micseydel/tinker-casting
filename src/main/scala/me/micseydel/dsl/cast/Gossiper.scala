package me.micseydel.dsl.cast

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TinkerBrainUtil.Listeners
import me.micseydel.dsl.{Sender, SpiritRef, Tinker, Tinkerer}
import me.micseydel.model.{BaseModel, LargeModel, NotedTranscription}
import me.micseydel.vault.NoteId

object Gossiper {
  sealed trait Message

  case class StartTinkering(tinker: Tinker) extends Message

  final case class Receive(notedTranscription: NotedTranscription) extends Message

  // rudimentary
  final case class SubmitVote(vote: Vote) extends Message
  final case class Vote(noteId: NoteId, confidence: Either[Double, Boolean], voter: SpiritRef[Vote])

  sealed trait Subscription extends Message {
    def subscriber: SpiritRef[NotedTranscription]
  }

  final case class SubscribeAccurate(subscriber: SpiritRef[NotedTranscription]) extends Subscription

  //  final case class SubscribeFast(subscriber: SpiritRef[NotedTranscription]) extends Subscription
  final case class SubscribeHybrid(subscriber: SpiritRef[NotedTranscription]) extends Subscription

  //

  def apply(): Behavior[Message] = initializing()

  private def initializing(): Behavior[Message] = Behaviors.setup { context =>
    context.log.info(s"Gossiper initializing")

    Behaviors.receiveMessage {
      case StartTinkering(tinker) =>
        finishInitializing()(tinker)
      case other =>
        context.log.warn(s"Did not expect to receive $other before StartTinkering message, ignoring")
        Behaviors.same
    }
  }

  private def finishInitializing()(implicit Tinker: Tinker): Ability[Message] =
    Tinkerer(rgb(255, 190, 230), "🗣️").setup(_ => behavior(Set.empty, Set.empty, Map.empty))

  private def behavior(
                        accurateListeners: Set[SpiritRef[NotedTranscription]],
                        fastListeners: Set[SpiritRef[NotedTranscription]],
                        votes: Map[NoteId, Vote]
                      )(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
      implicit val sender: Sender = Sender(context.self.path)
      implicit val tinkerBrain: ActorRef[TinkerBrain.Message] = context.system.tinkerBrain
      message match {
        case Receive(notedTranscription) =>
          context.actorContext.log.info(s"Received ${notedTranscription.capture.whisperResult.whisperResultMetadata.model} NotedTranscription with NoteId ${notedTranscription.noteId}; notifying ${accurateListeners.size} listeners")

          notedTranscription.capture.whisperResult.whisperResultMetadata.model match {
            case LargeModel =>
              context.actorContext.log.info(s"Sending ${notedTranscription.noteId} to ${accurateListeners.size} listeners (large, accurate model)")
              accurateListeners *!* notedTranscription
            case BaseModel =>
              context.actorContext.log.info(s"Sending ${notedTranscription.noteId} to ${fastListeners.size} listeners (base, fast model)")
              fastListeners *!* notedTranscription
          }

          Tinker.steadily

        case SubscribeAccurate(subscriber) =>
          context.actorContext.log.debug(s"Adding ${subscriber.path} to accurate subscribers")
          behavior(accurateListeners + subscriber, fastListeners, votes)

        case SubscribeHybrid(subscriber) =>
          context.actorContext.log.debug(s"Adding ${subscriber.path} to both fast and accurate subscribers")
          behavior(accurateListeners + subscriber, fastListeners + subscriber, votes)

        case StartTinkering(_) =>
          context.actorContext.log.warn("Received redundant StartTinkering message, ignoring")
          Tinker.steadily

          // experiment; voting on notes may generalize well
        case SubmitVote(vote) =>
          val updatedVotes = votes.updated(vote.noteId, vote)

          for (voteToMaybePropagate <- updatedVotes.values if voteToMaybePropagate.voter != vote.voter) {
            for (voter <- updatedVotes.values.map(_.voter)) {
              voter !!! voteToMaybePropagate
            }
          }

          behavior(accurateListeners, fastListeners, updatedVotes)
          Tinker.steadily
      }
    }
}
