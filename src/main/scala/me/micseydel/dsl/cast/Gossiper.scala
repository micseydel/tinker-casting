package me.micseydel.dsl.cast

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import cats.data.NonEmptyList
import me.micseydel.NoOp
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TinkerBrainUtil.Listeners
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Sender, SpiritRef, Tinker}
import me.micseydel.model.{BaseModel, LargeModel, NotedTranscription}
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef

import Ordering.Implicits._
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success}

object Gossiper {
  sealed trait Message

  case class StartTinkering(tinker: Tinker) extends Message

  final case class Receive(notedTranscription: NotedTranscription) extends Message

  // rudimentary
  final case class SubmitVote(vote: Vote) extends Message

  final case class Vote(noteId: NoteId, confidence: Either[Double, Option[Boolean]], voter: SpiritRef[NonEmptyList[Vote]], voteTime: ZonedDateTime, comments: Option[String]) {
    override def toString = s"""Vote($noteId, $confidence, ${toNormalizedUri(voter.path.toSerializationFormat).drop(35)}, ${voteTime.format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS"))}, $comments)"""
  }

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
   * @param votesMapping the inner key is the result of normalizedUri() called on the voter's ActorPath
   */
  private def behavior(
                        accurateListeners: Set[SpiritRef[NotedTranscription]],
                        fastListeners: Set[SpiritRef[NotedTranscription]],
                        votesMapping: Map[NoteId, Map[String, NonEmptyList[Vote]]]
                      )(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val sender: Sender = Sender(context.self.path)
    implicit val tinkerBrain: ActorRef[TinkerBrain.Message] = context.system.tinkerBrain

    val formattedMarkdown = votesMapping
      .toSeq.sortBy(_._2.values.toList.map(_.head).map(_.voteTime))
      .map { case (noteId, votesOnNote) =>
        val formattedVotesOnNote = votesOnNote.values
          .map {
            case NonEmptyList(Vote(_, confidence, voter, voteTime, maybeComments), theRest) =>
              val priorVotes = if (theRest.nonEmpty) s"$theRest" else ""
              MarkdownUtil.listLineWithTimestamp(voteTime, s"${toNormalizedUri(voter.path.toSerializationFormat).drop(35)} -> $confidence", dateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")) +
                (if (priorVotes.nonEmpty) {
                  s"""
                     |        - comments: $maybeComments
                     |        - prior votes: $priorVotes""".stripMargin
                } else {
                  s"""
                     |        - comments: $maybeComments""".stripMargin
                })
          }.mkString("    ", "\n    ", "")
        val filename = noteId.asString.drop(18).dropRight(3) + "wav"
        val firstLine = Chronicler.getCaptureTimeFromAndroidAudioPath(filename) match {
          case Left(msg) =>
            context.actorContext.log.warn(s"Failed to extract time from noteId $filename: $msg")
            noteId.toString
          case Right(time) =>
            MarkdownUtil.listLineWithTimestampAndRef(time, s"[[${noteId.asString.drop(18)}]]", noteId, dateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")).drop(2)
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
        behavior(accurateListeners + subscriber, fastListeners, votesMapping)

      case SubscribeHybrid(subscriber) =>
        context.actorContext.log.debug(s"Adding ${subscriber.path} to both fast and accurate subscribers")
        behavior(accurateListeners + subscriber, fastListeners + subscriber, votesMapping)

      case StartTinkering(_) =>
        context.actorContext.log.warn("Received redundant StartTinkering message, ignoring")
        Tinker.steadily

      // experiment; voting on *notes* may generalize well, but requires receivers to interpret arbitrarily
      case SubmitVote(newVote) =>
        context.actorContext.log.info(s"new vote $newVote") // FIXME: chatty, but it seemed like stuff was being dropped...
        val normalizedUri: String = toNormalizedUri(newVote.voter.path.toSerializationFormat)
        votesMapping.get(newVote.noteId) match {
          case None =>
            val updatedVotes = votesMapping.updated(newVote.noteId, Map(normalizedUri -> NonEmptyList.of(newVote)))
            behavior(accurateListeners, fastListeners, updatedVotes)
          case Some(voterToVotesMap) =>
            voterToVotesMap.get(normalizedUri) match {
              case None =>
                voterToVotesMap.values.map(_.head).toList match {
                  case head :: tail =>
                    val cachedVotes: NonEmptyList[Vote] = NonEmptyList(head, tail)
                    newVote.voter !!! cachedVotes
                  case Nil =>
                }

                val updatedVotes: Map[String, NonEmptyList[Vote]] = voterToVotesMap.updated(normalizedUri, NonEmptyList.of(newVote))
                behavior(accurateListeners, fastListeners, votesMapping.updated(newVote.noteId, updatedVotes))

              case Some(votes) =>
                val latestVote = votes.head
                if (latestVote.voteTime == newVote.voteTime) {
                  context.actorContext.log.warn(s"Ignoring vote on ${newVote.noteId} by $normalizedUri, already seen the timestamp for the noteId by the voter")
                  Tinker.steadily
                } else {
                  val votersToNotify = voterToVotesMap.values.map(_.head).filterNot(sameVoters(latestVote, _))
                  for (cachedVoter <- votersToNotify.map(_.voter)) {
                    cachedVoter !!! NonEmptyList.of(newVote)
                  }

                  val updatedVotesListForVoter = newVote :: votes
                  val updatedVotesMap = voterToVotesMap.updated(normalizedUri, updatedVotesListForVoter)
                  val updatedVotesMapping = votesMapping.updated(newVote.noteId, updatedVotesMap)

                  behavior(accurateListeners, fastListeners, updatedVotesMapping)
                }
            }
        }
    }
  }

  private def sameVoters(firstVote: Vote, secondVote: Vote): Boolean =
    toNormalizedUri(firstVote.voter.path.toSerializationFormat) == toNormalizedUri(secondVote.voter.path.toSerializationFormat)

  def toNormalizedUri(uri: String): String = {
    val t = uri.split("/").toList.reverse.dropWhile(_.startsWith("$")).reverse.mkString("/")
    t.split('#').toList match {
      case List(wanted, _) => wanted
      case _ => t
    }
  }
}
