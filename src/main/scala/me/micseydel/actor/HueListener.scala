package me.micseydel.actor

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import me.micseydel.actor.notifications.ChimeActor.Material
import me.micseydel.actor.notifications.NotificationCenterManager.JustSideEffect
import me.micseydel.actor.notifications.{ChimeActor, NotificationCenterManager}
import me.micseydel.actor.perimeter.HueControl
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.Gossiper.Vote
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.dsl.tinkerer.{NoteMakingTinkerer, RasaAnnotatingListener}
import me.micseydel.dsl.tinkerer.RasaAnnotatingListener.RasaAnnotatedNotedTranscription
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext}
import me.micseydel.model.KnownIntent.no_intent
import me.micseydel.model.Light.AllList
import me.micseydel.model.LightStates.RelaxedLight
import me.micseydel.model._
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.{LinkIdJsonProtocol, NoteId}
import me.micseydel.vault.persistence.{NoteRef, TypedJsonRef}
import org.slf4j.Logger
import spray.json.{DefaultJsonProtocol, JsonFormat, RootJsonFormat}

import java.time.ZonedDateTime
import scala.annotation.unused

object HueListener {
  sealed trait Message

  final case class TranscriptionEvent(rasaAnnotatedNotedTranscription: RasaAnnotatedNotedTranscription) extends Message

  final case class ReceiveVotes(votes: NonEmptyList[Vote]) extends Message

  //

  def apply(hueControl: SpiritRef[HueControl.Message])(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("HueListener Experiment", rgb(230, 230, 230), "👂") { (context, noteRef) =>
      @unused // subscribes to Gossiper on our behalf
      val rasaAnnotatedListener = context.cast(RasaAnnotatingListener("lights", Gossiper.SubscribeHybrid(_), context.messageAdapter(TranscriptionEvent), Some("lights_test")), "RasaListener")

      context.actorContext.log.info("HueListener initialized")

      implicit val h: SpiritRef[HueControl.Message] = hueControl
      implicit val adapter: SpiritRef[NonEmptyList[Vote]] = context.messageAdapter(ReceiveVotes)
      implicit val nr: NoteRef = noteRef

      behavior(State(Set.empty, Map.empty))
  }

  private val IgnoredMinutesAgo = 3

  private case class State(alreadySeenSet: Set[(NoteId, WhisperModel)], trackedVotes: Map[(NoteId, WhisperModel), Vote]) {
    def alreadySeen(key: (NoteId, WhisperModel)): Boolean = alreadySeenSet.contains(key)
    def isThisJustAReminder(key: (NoteId, WhisperModel), confidence: Double): Boolean = {
      trackedVotes.get(key).map(_.confidence).exists {
        case Left(justAReminderConfidence) =>
          justAReminderConfidence > confidence
        case Right(None) => false
        case Right(Some(certainty)) => certainty
      }
    }

    def integrate(votes: Map[(NoteId, WhisperModel), Vote]): State = this.copy(trackedVotes = trackedVotes ++ votes)
    def integrate(key: (NoteId, WhisperModel)): State = this.copy(alreadySeenSet = alreadySeenSet ++ key)
  }
  private def behavior(state: State)
                      (implicit Tinker: Tinker,
                       noteRef: NoteRef, hueControl: SpiritRef[HueControl.Message], receiveVotesAdapter: SpiritRef[NonEmptyList[Vote]]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerClock = context.system.clock
    implicit val c: TinkerContext[_] = context
    implicit val log: Logger = context.actorContext.log

    Tinker.receiveMessage {
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(capture, noteId), _)) if state.alreadySeen((noteId, capture.whisperResult.whisperResultMetadata.model)) =>
        context.actorContext.log.debug(s"Already processed $noteId,${capture.whisperResult.whisperResultMetadata.model}, ignoring")
        Tinker.steadily

        // Rasa match
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(whisperResultContent, WhisperResultMetadata(model, _, _, _)), capturedTime), noteId), Some(rasaResult@KnownIntent.set_the_lights(validated)))) =>

        validated match {
          case Valid(SetTheLights(_, maybeColor, maybeBrightness, maybeSetOnOff)) =>
            // FIXME: why is maybeSetOnOff unused? is it because it's broken?

            val key = (noteId, model)
            val isStale = TimeUtil.timeSince(capturedTime).toMinutes > IgnoredMinutesAgo
            val thisAppearsToJustBeAReminder = state.isThisJustAReminder(key, rasaResult.intent.confidence)

            if (isStale) {
              log.warn(s"Ignored $noteId because it was more than $IgnoredMinutesAgo minutes ago")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Warning(Material)), Some(("isstale", 1)))
              Tinker.steadily
            } else if (thisAppearsToJustBeAReminder) {
              log.info(s"NoteId $noteId seems to refer to a reminder, so not making any light update; doing a Success Chime though")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
              Tinker.steadily
            } else {
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Info(Material)))

              val finalLightState = genLightState(maybeColor, maybeBrightness)
              for (light <- AllList) {
                hueControl !! HueControl.SetLight(light, finalLightState)
              }

              context.system.chronicler !! genAckMessage(noteId, model, whisperResultContent.text)
              context.system.gossiper !! noteId.voteMeasuredly(rasaResult.intent.confidence, receiveVotesAdapter, Some(s"$model"))

              context.actorContext.log.debug(s"Adding $noteId to already seen (will not process a second time)")
              behavior(state.integrate(key))
            }

          case Invalid(e) =>
            context.system.gossiper !! noteId.voteConfidently(Some(false), receiveVotesAdapter, Some("failed to extract entities"))
            context.actorContext.log.warn(s"Could not extract entities for set_the_lights ${rasaResult.entities}; $e")
            Tinker.steadily
        }

        // not a light change
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(_, noteId), Some(RasaResult(entities, Intent(_, unrecognizedIntent), _, _, _)))) =>
        if (unrecognizedIntent != no_intent.IntentName) {
          context.actorContext.log.info(s"unrecognizedIntent $unrecognizedIntent (rasaResult.intent.confidence) with entities $entities")
        }
        context.system.gossiper !! noteId.voteConfidently(Some(false), receiveVotesAdapter, Some(s"unrecognized intent: $unrecognizedIntent"))
        Tinker.steadily

        // no Rasa, maybe an exact-match
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(text, _), _), _), noteId), None)) =>
        val flashCommands = List("please flash the lights", "please do a light show")
        if (flashCommands.contains(text.trim.toLowerCase)) {
          hueControl !! HueControl.FlashTheLights()
        } else {
          context.actorContext.log.debug(s"Ignoring <$text>, no Rasa data")
        }

        context.system.gossiper !! noteId.voteConfidently(Some(true), receiveVotesAdapter, Some("exact match"))

        Tinker.steadily

        // non-light changing
      case ReceiveVotes(votes) =>
        val (matched, discard) = votes.toList.partition(v => Gossiper.toNormalizedUri(v.voter.path.toSerializationFormat).endsWith("RemindMeListenerActor"))

        if (discard.nonEmpty && context.actorContext.log.isDebugEnabled) {
          context.actorContext.log.debug(s"Discarded votes $discard")
        }

        val newVotesToTrack = matched.flatMap { vote =>
          val maybeModel = vote.comments match {
            case Some(comment) if comment.contains("BaseModel") => Some(BaseModel)
            case Some(comment) if comment.contains("LargeModel") => Some(LargeModel)
            case Some("trigger phrase not detected") => None
            case _ =>
              context.actorContext.log.warn(s"Expected RemindMeListenerActor's comment to contain a Whisper model in {BaseModel, LargeModel} but comment was: ${vote.comments}")
              None
          }

          maybeModel.map { model =>
            val key = vote.noteId -> model
            if (state.alreadySeen(key)) {
              noteRef.appendConfidently(s"Too late for $vote")
            } else {
              noteRef.appendConfidently(s"Storing vote for later: $vote")
            }

            key -> vote
          }
        }

        if (newVotesToTrack.nonEmpty) {
          val toAdd: Map[(NoteId, WhisperModel), Vote] = newVotesToTrack.toMap
          behavior(state.integrate(toAdd))
        } else {
          Tinker.steadily
        }
    }
  }

  //

  private def normalizeBrightness(brightness: Int)(implicit log: Logger): Int = {
    if (brightness >= 0 && brightness <= 100) {
      brightness
    } else {
      if (brightness > 200 && brightness < 300) {
        brightness - 200
      } else {
        log.warn(s"Weird $brightness, falling back to default 10")
        10
      }
    }
  }

  def genAckMessage(noteId: NoteId, model: WhisperModel, text: String)(implicit clock: TinkerClock): Chronicler.ListenerAcknowledgement = {
    val ackText = model match {
      case BaseModel =>
        s"Updated the lights (fast): $text"
      case LargeModel =>
        s"Updated the lights (accurate): $text"
    }

    Chronicler.ListenerAcknowledgement(noteId, clock.now(), ackText, Some(AutomaticallyIntegrated))
  }

  private def genLightState(maybeLightState: Option[LightState], maybeBrightness: Option[Int])(implicit log: Logger): LightState = {
    val pending = maybeLightState match {
      case None =>
        log.debug("No color, using RelaxedLight")
        RelaxedLight
      case Some(c) => c
    }

    maybeBrightness match {
      case None => pending
      case Some(rawBrightness) => pending.copy(bri = normalizeBrightness(rawBrightness))
    }
  }

  private def justAReminder(maybeVote: Option[Vote], rasaConfidence: Double): Boolean = {
    maybeVote.map(_.confidence).exists {
      case Left(justAReminderConfidence) =>
        justAReminderConfidence > rasaConfidence
      case Right(None) => false
      case Right(Some(certainty)) => certainty
    }
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def appendConfidently(line: String)(implicit clock: TinkerClock): Unit = {
      noteRef.appendOrThrow(MarkdownUtil.listLineWithTimestamp(clock.now(), line, dateTimeFormatter = TimeUtil.MonthDayTimeSecondFormatter))
    }
  }
}
