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
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef

import scala.annotation.unused

object HueListener {
  sealed trait Message

  final case class TranscriptionEvent(rasaAnnotatedNotedTranscription: RasaAnnotatedNotedTranscription) extends Message

  final case class ReceiveVotes(votes: NonEmptyList[Vote]) extends Message

  def apply(hueControl: SpiritRef[HueControl.Message])(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("HueListener Experiment", rgb(230, 230, 230), "👂") { (context, noteRef) =>
    @unused // subscribes to Gossiper on our behalf
    val rasaAnnotatedListener = context.cast(RasaAnnotatingListener("lights", Gossiper.SubscribeHybrid(_), context.messageAdapter(TranscriptionEvent), Some("lights_test")), "RasaListener")

    context.actorContext.log.info("HueListener initialized")

    implicit val h: SpiritRef[HueControl.Message] = hueControl
    implicit val adapter: SpiritRef[NonEmptyList[Vote]] = context.messageAdapter(ReceiveVotes)
    implicit val nr: NoteRef = noteRef

    behavior(Set.empty, Map.empty)
  }

  private def behavior(alreadySeen: Set[(NoteId, WhisperModel)], trackedVotes: Map[(NoteId, WhisperModel), Vote])(implicit Tinker: Tinker, noteRef: NoteRef, hueControl: SpiritRef[HueControl.Message], receiveVotesAdapter: SpiritRef[NonEmptyList[Vote]]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerClock = context.system.clock
    implicit val c: TinkerContext[_] = context

    context.actorContext.log.info(s"alreadySeen size ${alreadySeen.size}")

    Tinker.receiveMessage {
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(capture, noteId), _)) if alreadySeen.contains((noteId, capture.whisperResult.whisperResultMetadata.model)) =>
        context.system.gossiper !! noteId.voteConfidently(None, context.messageAdapter(ReceiveVotes), Some("already seen (or reminder), ignoring"))
        context.actorContext.log.debug(s"Already processed $noteId, ignoring")
        Tinker.steadily

      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(whisperResultContent, WhisperResultMetadata(model, _, _, _)), _), noteId), Some(rasaResult@KnownIntent.set_the_lights(validated)))) =>
        val key = (noteId, model)

        trackedVotes.get(key) match {
          case Some(vote) =>
            noteRef.appendConfidently(s"Could have used this vote to avoid changing the lights! $vote")
          case None =>
        }

        validated match {
          case Valid(SetTheLights(_, maybeColor, maybeBrightness, maybeSetOnOff)) =>
            context.actorContext.log.debug(s"Ignoring maybeSetOnOff $maybeSetOnOff")

            val color = maybeColor match {
              case None =>
                context.actorContext.log.debug("No color, using RelaxedLight")
                RelaxedLight
              case Some(c) => c
            }

            maybeBrightness match {
              case None =>
                for (light <- AllList) {
                  hueControl !! HueControl.SetLight(light, color)
                }
              case Some(brightness) =>
                val adjustedBrightness = if (brightness >= 0 && brightness <= 100) {
                  brightness
                } else {
                  if (brightness > 200 && brightness < 300) {
                    brightness - 200
                  } else {
                    context.actorContext.log.warn(s"Weird $brightness, falling back to default 10")
                    10
                  }
                }
                val colorWithBrightness = color.copy(bri = adjustedBrightness)
                for (light <- AllList) {
                  hueControl !! HueControl.SetLight(light, colorWithBrightness)
                }
                context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Info(Material)))
            }

            val ackMessage: Chronicler.ListenerAcknowledgement = Chronicler.ListenerAcknowledgement(noteId, context.system.clock.now(), model match {
              case BaseModel =>
                s"Updated the lights (fast): ${whisperResultContent.text}"
              case LargeModel =>
                s"Updated the lights (accurate): ${whisperResultContent.text}"
            }, Some(AutomaticallyIntegrated))

            context.system.chronicler !! ackMessage

            context.system.gossiper !! noteId.voteMeasuredly(rasaResult.intent.confidence, receiveVotesAdapter, Some(s"$model"))

            context.actorContext.log.debug(s"Adding $noteId to already seen (will not process a second time)")
            behavior(alreadySeen + key, trackedVotes)

          case Invalid(e) =>
            context.system.gossiper !! noteId.voteConfidently(Some(false), receiveVotesAdapter, Some("failed to extract entities"))
            context.actorContext.log.warn(s"Could not extract entities for set_the_lights ${rasaResult.entities}; $e")
            Tinker.steadily
        }

      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(_, noteId), Some(RasaResult(entities, Intent(_, unrecognizedIntent), _, _, _)))) =>
        if (unrecognizedIntent != no_intent.IntentName) {
          context.actorContext.log.info(s"unrecognizedIntent $unrecognizedIntent (rasaResult.intent.confidence) with entities $entities")
        }
        context.system.gossiper !! noteId.voteConfidently(Some(false), receiveVotesAdapter, Some(s"unrecognized intent: $unrecognizedIntent"))
        Tinker.steadily

      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(text, _), _), _), noteId), None)) =>
        val flashCommands = List("please flash the lights", "please do a light show")
        if (flashCommands.contains(text.trim.toLowerCase)) {
          hueControl !! HueControl.FlashTheLights()
        } else {
          context.actorContext.log.debug(s"Ignoring <$text>, no Rasa data")
        }

        context.system.gossiper !! noteId.voteConfidently(Some(true), receiveVotesAdapter, Some("exact match"))

        Tinker.steadily

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
              if (alreadySeen.contains(key)) {
                noteRef.appendConfidently(s"Too late for $vote")
              } else {
                noteRef.appendConfidently(s"Storing vote for later: $vote")
              }

              key -> vote
          }
        }

        if (newVotesToTrack.nonEmpty) {
          val toAdd: Map[(NoteId, WhisperModel), Vote] = newVotesToTrack.toMap
          behavior(alreadySeen, trackedVotes ++ toAdd)
        } else {
          Tinker.steadily
        }
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def appendConfidently(line: String)(implicit clock: TinkerClock): Unit = {
      noteRef.appendOrThrow(MarkdownUtil.listLineWithTimestamp(clock.now(), line, dateTimeFormatter = TimeUtil.MonthDayTimeSecondFormatter))
    }
  }
}
