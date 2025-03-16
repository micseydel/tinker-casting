package me.micseydel.dsl.cast

import akka.actor.ActorPath
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.NoOp
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TinkerBrainUtil.Listeners
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.dsl.{Sender, SpiritRef, Tinker, Tinkerer}
import me.micseydel.model.{BaseModel, LargeModel, NotedTranscription}
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success}

object Gossiper {
  sealed trait Message

  case class StartTinkering(tinker: Tinker) extends Message

  final case class Receive(notedTranscription: NotedTranscription) extends Message

  // rudimentary
  final case class SubmitVote(vote: Vote) extends Message

  final case class Vote(noteId: NoteId, confidence: Either[Double, Option[Boolean]], voter: SpiritRef[Vote], voteTime: ZonedDateTime)

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
    NoteMakingTinkerer("Gossiper", rgb(255, 190, 230), "🗣️") { (context, noteRef) =>
      context.actorContext.log.info("Started Gossiper")
      behavior(Set.empty, Set.empty, Map.empty)(Tinker, noteRef)
    }

  /**
   * @param votes the inner key is the result of normalizedUri() called on the voter's ActorPath
   */
  private def behavior(
                        accurateListeners: Set[SpiritRef[NotedTranscription]],
                        fastListeners: Set[SpiritRef[NotedTranscription]],
                        votes: Map[NoteId, Map[String, Vote]]
                      )(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val sender: Sender = Sender(context.self.path)
    implicit val tinkerBrain: ActorRef[TinkerBrain.Message] = context.system.tinkerBrain

    val formattedMarkdown = votes.map { case (noteId, votesOnNote) =>
      val formattedVotesOnNote = votesOnNote.values.map {
        case Vote(_, confidence, voter, voteTime) =>
          MarkdownUtil.listLineWithTimestamp(voteTime, s"${toNormalizedUri(voter.path.toSerializationFormat).drop(35)} -> $confidence", dateTimeFormatter = DateTimeFormatter.ofPattern("h:mm:ss.S"))
      }.mkString("    ", "\n    ", "")
      val filename = noteId.asString.drop(18).dropRight(3) + "wav"
      val firstLine = Chronicler.getCaptureTimeFromAndroidAudioPath(filename) match {
        case Left(msg) =>
          context.actorContext.log.warn(s"Failed to extract time from noteId $filename: $msg")
          noteId.toString
        case Right(time) =>
          MarkdownUtil.listLineWithTimestampAndRef(time, s"[[${noteId.asString.drop(18)}]]", noteId, dateTimeFormatter = DateTimeFormatter.ofPattern("h:mm:ss.S")).drop(2)
      }
      s"$firstLine\n$formattedVotesOnNote"
    }.mkString("- ", "\n- ", "\n")

    noteRef.setMarkdown(formattedMarkdown) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    Tinker.receiveMessage {
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
      case SubmitVote(newVote) =>
        context.actorContext.log.warn(s"new vote $newVote") // FIXME: chatty
        val normalizedUri: String = toNormalizedUri(newVote.voter.path.toSerializationFormat)
        votes.get(newVote.noteId) match {
          case None =>
            val updatedVotes = votes.updated(newVote.noteId, Map(normalizedUri -> newVote))
            behavior(accurateListeners, fastListeners, updatedVotes)
          case Some(voterToVotesMap) =>
            voterToVotesMap.get(normalizedUri) match {
              case None =>
                for (vote <- voterToVotesMap.values) {
                  newVote.voter !!! vote
                }
                val updatedVotes = voterToVotesMap.updated(normalizedUri, newVote)
                behavior(accurateListeners, fastListeners, votes.updated(newVote.noteId, updatedVotes))

              case Some(voteToReplace) =>
                if (voteToReplace.voteTime == newVote.voteTime) {
                  context.actorContext.log.warn(s"Ignoring vote on ${newVote.noteId} by $normalizedUri, already seen the timestamp for the noteId by the voter")
                  Tinker.steadily
                } else {
                  val votersToNotify = voterToVotesMap.values.filterNot(sameVoters(voteToReplace, _))
                  for (cachedVoter <- votersToNotify.map(_.voter)) {
                    cachedVoter !!! newVote
                  }

                  val updatedVotes = voterToVotesMap.updated(normalizedUri, newVote)
                  behavior(accurateListeners, fastListeners, votes.updated(newVote.noteId, updatedVotes))
                }
            }
        }
    }
  }

  private def sameVoters(firstVote: Vote, secondVote: Vote): Boolean =
    toNormalizedUri(firstVote.voter.path.toSerializationFormat) == toNormalizedUri(secondVote.voter.path.toSerializationFormat)

  private def toNormalizedUri(uri: String): String = uri.split("/").toList.reverse.dropWhile(_.startsWith("$")).reverse.mkString("/")
}
