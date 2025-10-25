package me.micseydel.actor.hue

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import me.micseydel.actor.notifications.ChimeActor.Material
import me.micseydel.actor.notifications.NotificationCenterManager.{HueCommand, JustSideEffect}
import me.micseydel.actor.notifications.{ChimeActor, NotificationCenterManager}
import me.micseydel.actor.perimeter.HueControl
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.Gossiper.Vote
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.dsl.cast.{Gossiper, TimeKeeper}
import me.micseydel.dsl.tinkerer.RasaAnnotatingListener.RasaAnnotatedNotedTranscription
import me.micseydel.dsl.tinkerer.{NoteMakingTinkerer, RasaAnnotatingListener}
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerClock, TinkerContext}
import me.micseydel.model.*
import me.micseydel.model.KnownIntent.no_intent
import me.micseydel.model.Light.AllList
import me.micseydel.model.LightStates.RelaxedLight
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.time.LocalDate
import scala.annotation.unused

object HueListener {
  sealed trait Message

  final case class TranscriptionEvent(rasaAnnotatedNotedTranscription: RasaAnnotatedNotedTranscription) extends Message

  case class ReceiveDelegated(noteId: NoteId, forDay: LocalDate, lightState: LightState, model: WhisperModel) extends Message

  // just for marking something as not relevant, we ignore any incoming votes
  // REAL voting is handled by delegates
  private case class ReceiveVotes(votes: NonEmptyList[Vote]) extends Message

  //

  private val IgnoredMinutesAgo = 2

  def apply()(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = NoteMakingTinkerer("HueListener Experiment", rgb(230, 230, 230), "👂") { (context, noteRef) =>
    @unused // subscribes to Gossiper on our behalf
    val rasaAnnotatedListener = context.cast(RasaAnnotatingListener("lights", Gossiper.SubscribeHybrid(_), context.messageAdapter(TranscriptionEvent)/*, Some("lights_test"),*/), "RasaListener")

    context.actorContext.log.info("HueListener initialized")

    implicit val nr: NoteRef = noteRef
    implicit val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

    val dailyNoteLookup: LazierLookUpSpiritByKey[NoteId, HueListerHelperForNote.Message] = new LazierLookUpSpiritByKey()

    behavior(dailyNoteLookup)
  }

  // FIXME: latestCompleted re:large state so that older stuff is ignored with constant state?
  private def behavior(dailyNoteLookup: LazierLookUpSpiritByKey[NoteId, HueListerHelperForNote.Message])
                      (implicit Tinker: EnhancedTinker[MyCentralCast],
                       noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerClock = context.system.clock
    implicit val c: TinkerContext[_] = context
    implicit val log: Logger = context.actorContext.log

    Tinker.receiveMessage {

      case ReceiveDelegated(noteId, forDay, lightState, model) =>
        Tinker.userExtension.chronicler !! genAckMessage(noteId, forDay, model, s"Setting lights to $lightState")
        context.actorContext.log.info(s"Processing deferred $lightState update for $noteId, $model")
        context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Info(Material)))
        for (light <- AllList) {
          // FIXME: lightState -> JustSideEffect ???
          context.system.notifier !! NotificationCenterManager.JustSideEffect(HueCommand(HueControl.SetLight(light, lightState)))
        }
        Tinker.steadily

      // Rasa match
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(_, WhisperResultMetadata(model, _, _, _)), capturedTime), noteId), Some(rasaResult@KnownIntent.set_the_lights(validated)))) =>
        // FIXME: check expiry first?
        val isStale = TimeUtil.timeSince(capturedTime).toMinutes > IgnoredMinutesAgo
        if (isStale) {
          log.warn(s"Ignored $noteId because it was more than $IgnoredMinutesAgo minutes ago, even though it was a valid light-changing request")
          context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Warning(Material)), Some(("isstale", 1)))
          Tinker.steadily
        } else {
          validated match {
            case Valid(SetTheLights(_, maybeColor, maybeBrightness, maybeSetOnOff)) =>
              // FIXME: why is maybeSetOnOff unused? is it because it's broken?
              context.actorContext.log.info(s"maybeSetOnOff=$maybeSetOnOff for $noteId")

              val confidence = rasaResult.intent.confidence
              dailyNoteLookup.lookupOrCreate(noteId) {
                // base and/or large whisper may fail, so this must be flexible
                context.cast(HueListerHelperForNote(noteId, context.self), s"""HueListerHelperForNote-${noteId.id.split(" ").toList.last}""")
              } match {
                case (potentiallyUpdated, delegate) =>
                  val finalLightState: LightState = genLightState(maybeColor, maybeBrightness)
                  delegate !! HueListerHelperForNote.ReceiveNoteInfo(capturedTime.toLocalDate, model, finalLightState, confidence)
                  behavior(potentiallyUpdated)
              }

            case Invalid(e) =>
              Tinker.userExtension.gossiper !! noteId.voteConfidently(Some(false), context.messageAdapter(ReceiveVotes), Some("failed to extract entities"))
              context.actorContext.log.warn(s"Could not extract entities for set_the_lights ${rasaResult.entities}; $e")
              Tinker.steadily
          }
        }

      // not a light change
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(_, noteId), Some(RasaResult(entities, Intent(_, unrecognizedIntent), _, _, _)))) =>
        if (unrecognizedIntent != no_intent.IntentName) {
          context.actorContext.log.info(s"unrecognizedIntent $unrecognizedIntent (rasaResult.intent.confidence) with entities $entities")
        }
        Tinker.userExtension.gossiper !! noteId.voteConfidently(Some(false), context.messageAdapter(ReceiveVotes), Some(s"unrecognized intent: $unrecognizedIntent"))
        Tinker.steadily

      // no Rasa, maybe an exact-match
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(text, _), _), _), noteId), None)) =>
        val flashCommands = List("please flash the lights", "please do a light show")
        if (flashCommands.contains(text.trim.toLowerCase)) {
          context.system.notifier !! NotificationCenterManager.JustSideEffect(HueCommand(HueControl.FlashTheLights()))
        } else {
          context.actorContext.log.debug(s"Ignoring <$text>, no Rasa data")
        }

        Tinker.userExtension.gossiper !! noteId.voteConfidently(Some(true), context.messageAdapter(ReceiveVotes), Some("exact match"))

        Tinker.steadily

      case ReceiveVotes(votes) =>
        if (context.actorContext.log.isDebugEnabled) {
          context.actorContext.log.debug(s"Ignoring ${votes.size} votes")
        }

        Tinker.steadily
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

  // FIXME: can chronicler act as a "final" tracker of what happens after voting?!
  private def genAckMessage(noteId: NoteId, forDay: LocalDate, model: WhisperModel, text: String)(implicit clock: TinkerClock): Chronicler.ListenerAcknowledgement = {
    val ackText = model match {
      case BaseModel =>
        s"Updated the lights (fast): $text"
      case LargeModel =>
        s"Updated the lights (accurate): $text"
      case TurboModel => ???
    }

    Chronicler.ListenerAcknowledgement(noteId, forDay, clock.now(), ackText, Some(AutomaticallyIntegrated))
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
}


// FIXME: cleanup!
private class LazierLookUpSpiritByKey[K, S](
                                       map: Map[K, SpiritRef[S]] = Map.empty[K, SpiritRef[S]]
//                                       ,
//                                       caster: (TinkerContext[_], K) => SpiritRef[S]
                                     ) {

//  def :?>(key: K)(implicit tinkerContext: TinkerContext[_]): (LazierLookUpSpiritByKey[K, S], SpiritRef[S]) = lookup(key)

  override def toString: String = s"LazierLookUpSpiritByKey($map)"

//  //
//
//  private
  def lookupOrCreate(key: K)(caster: /*(TinkerContext[_], K)*/  => SpiritRef[S])(implicit tinkerContext: TinkerContext[_]): (LazierLookUpSpiritByKey[K, S], SpiritRef[S]) = {
    map.get(key) match {
      case Some(existing) =>
        (this, existing)

      case None =>
        val fresh = caster//(tinkerContext, key)
        (new LazierLookUpSpiritByKey(map.updated(key, fresh)/*, caster*/), fresh)
    }
  }
}
