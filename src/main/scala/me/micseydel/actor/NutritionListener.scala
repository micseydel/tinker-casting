package me.micseydel.actor

import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.dsl._
import me.micseydel.model.NotedTranscription
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.util.TimeUtil

import java.time.ZonedDateTime

object NutritionListener {
  // mailbox

  sealed trait Message

  final case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message

  // outbox

  final case class LastAte(at: ZonedDateTime) extends AnyVal {
    /**
     * "Has 4 hours passed?"
     */
    def isHungry()(implicit tinkerClock: TinkerClock): Boolean = {
      TimeUtil.timeSince(at).toHours >= 4
    }
  }

  // behavior

  def apply(subscriber: SpiritRef[LastAte])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinkerer(rgb(135, 206, 235), "👂").setup { context =>
    implicit val c: TinkerContext[_] = context

    context.actorContext.log.info("Subscribing to accurate transcriptions")
    Tinker.userExtension.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(TranscriptionEvent))

    Tinker.receiveMessage {
      case TranscriptionEvent(m) =>
        context.actorContext.log.debug(s"Received $m")
        markdownCalorieConsumptionTime(m) match {
          case None =>
            context.actorContext.log.debug(s"Ignoring $m")
            Tinker.steadily
          case Some(when) =>
            context.actorContext.log.info(s"Setting [[last_ate]] to $when, notifying foodReminder and subscriber ${subscriber.path}")
            Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(m.noteId, context.system.clock.now(), s"Marked $when as [[last_ate]]", Some(AutomaticallyIntegrated))
            subscriber !! LastAte(when)
            behavior(subscriber)(when)
        }
    }
  }

  private def behavior(subscriber: SpiritRef[LastAte])(lastAte: ZonedDateTime)(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case TranscriptionEvent(notedTranscription) =>
        markdownCalorieConsumptionTime(notedTranscription) match {
          case None =>
            context.actorContext.log.debug(s"Ignoring $notedTranscription")
            Tinker.steadily
          case Some(when) if when == lastAte =>
            context.actorContext.log.info(s"Ignoring $notedTranscription, already processed $lastAte")
            Tinker.steadily

          case Some(when) =>
            context.actorContext.log.info(s"Setting [[last_ate]] to $when (was $lastAte), notifying subscriber ${subscriber.path}")
            subscriber !! LastAte(when)
            Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(notedTranscription.noteId, context.system.clock.now(), s"Marked $when as [[last_ate]]", Some(AutomaticallyIntegrated))
            behavior(subscriber)(when)
        }

    }
  }

  //

  private def markdownCalorieConsumptionTime(notedTranscription: NotedTranscription): Option[ZonedDateTime] = {
    val strings = Seq(
      "i ate",
      "i had breakfast",
      "i had lunch",
      "i had a snack",
      "i had dinner",
      "i had a meal",
      "i had something to eat",
      "i just ate",
      "i had breakfast",
      "i just had lunch",
      "i just had a snack",
      "i just had dinner",
      "i just had a meal",
      "i just had something to eat"
    )

    val text = notedTranscription.capture.whisperResult.whisperResultContent.text.toLowerCase
    lazy val mentionedEating = strings.exists(s => text.contains(s))

    if (text.wordCount < 30 && mentionedEating) {
      Some(notedTranscription.capture.captureTime)
    } else {
      None
    }
  }
}
