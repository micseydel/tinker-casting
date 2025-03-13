package me.micseydel.actor

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
import me.micseydel.dsl.tinkerer.RasaAnnotatingListener
import me.micseydel.dsl.tinkerer.RasaAnnotatingListener.RasaAnnotatedNotedTranscription
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
import me.micseydel.model.KnownIntent.no_intent
import me.micseydel.model.Light.AllList
import me.micseydel.model.LightStates.RelaxedLight
import me.micseydel.model._
import me.micseydel.vault.NoteId

import scala.annotation.unused

object HueListener {
  sealed trait Message

  final case class TranscriptionEvent(rasaAnnotatedNotedTranscription: RasaAnnotatedNotedTranscription) extends Message

  final case class ReceiveVote(vote: Vote) extends Message

  def apply(hueControl: SpiritRef[HueControl.Message])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(230, 230, 230), "👂").setup { context =>
    implicit val c: TinkerContext[_] = context

    @unused // subscribes to Gossiper on our behalf
    val rasaAnnotatedListener = context.cast(RasaAnnotatingListener("lights", Gossiper.SubscribeHybrid(_), context.messageAdapter(TranscriptionEvent), Some("lights_test")), "RasaListener")

    context.actorContext.log.info("HueListener initialized")

    behavior(hueControl)(Set.empty)
  }

  private def behavior(hueControl: SpiritRef[HueControl.Message])(alreadySeen: Set[(NoteId, WhisperModel)])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val tc: TinkerClock = context.system.clock
    implicit val c: TinkerContext[_] = context

    context.actorContext.log.info(s"alreadySeen size ${alreadySeen.size}")

    message match {
      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(capture, noteId), _)) if alreadySeen.contains((noteId, capture.whisperResult.whisperResultMetadata.model)) =>
        context.actorContext.log.debug(s"Already processed $noteId, ignoring")
        Tinker.steadily

      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(whisperResultContent, WhisperResultMetadata(model, _, _, _)), captureTime), noteId), Some(rasaResult@KnownIntent.set_the_lights(validated)))) =>
        val vote = noteId.vote(Left(rasaResult.intent.confidence), context.messageAdapter(ReceiveVote))
        context.system.gossiper !! Gossiper.SubmitVote(vote)

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

            context.actorContext.log.debug(s"Adding $noteId to already seen (will not process a second time)")
            behavior(hueControl)(alreadySeen + ((noteId, model)))

          case Invalid(e) =>
            context.actorContext.log.warn(s"Could not extract entities for set_the_lights ${rasaResult.entities}; $e")
            Tinker.steadily
        }

      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(_, _), Some(RasaResult(entities, Intent(_, unrecognizedIntent), _, _, _)))) =>
        if (unrecognizedIntent != no_intent.IntentName) {
          context.actorContext.log.info(s"unrecognizedIntent $unrecognizedIntent with entities $entities")
        }
        Tinker.steadily

      case TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(text, _), _), _), _), None)) =>
        val flashCommands = List("please flash the lights", "please do a light show")
        if (flashCommands.contains(text.trim.toLowerCase)) {
          hueControl !! HueControl.FlashTheLights()
        } else {
          context.actorContext.log.debug(s"Ignoring <$text>, no Rasa data")
        }

        Tinker.steadily

      case ReceiveVote(vote) =>
        // FIXME: this will be chatty!
        context.actorContext.log.warn(s"Ignoring $vote")
        Tinker.steadily
    }
  }
}
