package me.micseydel.actor.hue

import cats.data.NonEmptyList
import com.softwaremill.quicklens.ModifyPimp
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper.Vote
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerClock, TinkerContext}
import me.micseydel.model.{LightState, WhisperModel}
import me.micseydel.vault.NoteId
import org.slf4j.Logger

import java.time.LocalDate
import scala.concurrent.duration.FiniteDuration


private[hue] object HelperForModel {
  sealed trait Message

  private case class ReceiveVotes(votes: NonEmptyList[Vote]) extends Message

  private case object Timer extends Message

  //

  // !!! create a deferral passlist instead of these hard-coded options?
  private sealed abstract class State(maybeReminder: Option[Vote], maybeKibble: Option[Vote], whisperModel: WhisperModel, updated: (Vote, State) => State) {

    def integrateVotes(votes: NonEmptyList[Vote])(implicit log: Logger): (State, Option[Either[String, String]]) = {
      val relevantVotes = votes.toList.filter(_.comments.exists(_.contains(whisperModel.toString)))
      relevantVotes.foldRight((this, List[String]())) { case (newVote, (accumulator, reasonsToIgnore)) =>
        val updatedReasonsToIgnore = newVote.confidence match {
          case Right(Some(true)) =>
            s"${newVote.voter} is certain, deferring to it because this indicates a non-Hue note" :: reasonsToIgnore

          case Left(confidence) =>
            log.warn(s"Ignoring confidence $confidence, need to update code to handle non-binary peers")
            reasonsToIgnore

          case Right(None) | Right(Some(false)) => reasonsToIgnore
        }

        (updated(newVote, accumulator), updatedReasonsToIgnore)
      } match {
        case (updatedState, reasonsToIgnore) =>
          // - we defer if there's any reason found to ignore
          // - we proceed only if we have both votes (if we don't have a reason to stop and we have both votes, we know we can proceed)
          // - else, we return None
          val goNoGo = if (reasonsToIgnore.nonEmpty) {
            Some(Left(s"Ignoring this because: " + reasonsToIgnore.mkString(" AND ")))
          } else if (updatedState.bothVotersClear) {
            Some(Right("Both Reminder and Kibble actors have indicated it's not for them so we can proceed"))
          } else {
            None
          }

          (updatedState, goNoGo)
      }
    }

    private def bothVotersClear: Boolean = (maybeReminder.map(_.confidence), maybeKibble.map(_.confidence)) match {
      case (Some(Right(Some(false))), Some(Right(Some(false)))) => true
      case _ => false
    }
  }

  //

  def apply(noteId: NoteId, confidence: Double, deferred: LightState, supervisor: SpiritRef[HueListerHelperForNote.GoNoGo], timeout: FiniteDuration, whisperModel: WhisperModel, forDay: LocalDate)(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    implicit val c: TinkerClock = context.system.clock

    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeIn(timeout, context.self, Timer, Some(Timer)) // FIXME?

    Tinker.userExtension.gossiper !! noteId.voteMeasuredly(confidence, context.messageAdapter(ReceiveVotes), Some(whisperModel.toString))
    def updated(newVote: Vote, accumulator: State): State = {
      def transformer(newVote: Vote): Option[Vote] => Option[Vote] = {
        case existing@Some(Vote(_, _, _, existingVoteTime, _)) if existingVoteTime.isAfter(newVote.voteTime) =>
          context.actorContext.log.info("There was an existing vote that was more recent than the new vote, so keeping the existing")
          existing
        case _ =>
          context.actorContext.log.debug("Integrated new vote")
          Some(newVote)
      }

      accumulator match {
        case concrete@StateForModel(_, _) =>
          if (newVote.voterPathContains("Applications/RemindMeListenerActor")) {
            concrete.modify(_.maybeReminder).using(transformer(newVote))
          } else if (newVote.voterPathContains("CatsHelper/KibbleManagerActor")) {
            concrete.modify(_.maybeKibble).using(transformer(newVote))
          } else {
            concrete
          }
      }
    }

    // all this mess is to
    //   - parameterize the WhisperModel, and
    //   - use caseclass.modify().using()
    case class StateForModel(maybeReminder: Option[Vote], maybeKibble: Option[Vote]) extends State(maybeReminder, maybeKibble, whisperModel, updated)

    // !!! ideally would wait a bit, log then stop (to avoid dead letters)...
    def behavior(state: State)(implicit forDay: LocalDate): Ability[Message] = Tinker.receiveMessage {
      case ReceiveVotes(votes) =>
        implicit val log: Logger = context.actorContext.log
        log.debug("Received {} votes", votes.size)

        state.integrateVotes(votes) match {
          case (updatedState, maybeDecision) =>
            maybeDecision match {
              case None => behavior(updatedState)

              case Some(decision) =>
                timeKeeper !! TimeKeeper.Cancel(Timer)
                supervisor !! HueListerHelperForNote.GoNoGo(noteId, forDay, whisperModel, decision, deferred)
                Tinker.done
            }
        }

      case Timer =>
        supervisor !! HueListerHelperForNote.GoNoGo(noteId, forDay, whisperModel, Right("timer ran out"), deferred)
        Tinker.done
    }

    behavior(StateForModel(None, None))(forDay)
  }
}
