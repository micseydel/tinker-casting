package me.micseydel.actor.kitties.kibble

import me.micseydel.actor.kitties.kibble.KibbleModel.{Circular1, Circular2, KibbleContainer, RectangularL, RectangularS}
import me.micseydel.dsl.tinkerer.TinkerListener.{Acknowledged, Ignored}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.dsl.tinkerer.TinkerListener
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.model.{NotedTranscription, TranscriptionCapture, WhisperResult, WhisperResultContent}
import me.micseydel.util.StringImplicits.RichString

object KibbleManagerListenerActor {
  def apply(manager: SpiritRef[KibbleManagerActor.MaybeHeardKibbleMention])(implicit Tinker: Tinker): Ability[TinkerListener.Message] =
    TinkerListener.simpleStateless { (context, notedTranscription) =>
      implicit val tc: TinkerContext[_] = context
      notedTranscription match {
        case nt@NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(text, _), _), captureTime), noteId) =>
          context.actorContext.log.debug(s"received ${text.wordCount} words: $text")

          val lowerText = text.toLowerCase
          val mentionsKibble = lowerText.contains("kibble")
          val mentionsDryFood = lowerText.contains("dry food")
          if (text.wordCount >= 30) {
            context.actorContext.log.debug(s"Ignoring $noteId because of excess word count ${text.wordCount} (mentions kibble=$mentionsKibble, dry food=$mentionsDryFood)")
            Ignored
          } else {
            if (mentionsKibble || mentionsDryFood) {
              manager !! KibbleManagerActor.MaybeHeardKibbleMention(nt)
              Acknowledged(Chronicler.ListenerAcknowledgement.justIntegrated(noteId, "kibble maybe have been integrated"))
//              import me.micseydel.actor.kitties.kibble.KibbleManagerActor.{KibbleDiscarded, KibbleRefill, RemainingKibbleMeasure}

//              // hacky, hard-coded heuristics
//              getGrams(text) match {
//                case None =>
//                  context.actorContext.log.warn(s"Kibble/dry food mentioned but could not identify mass (in grams): $text")
//                  Ignored
//                case Some(mass) =>
//                  if (lowerText.contains("discard")) {
//                    context.actorContext.log.info(s"Detected ${mass}g discarded kibble")
//                    manager !! KibbleDiscarded(mass, captureTime, noteId)
//                    Acknowledged(Chronicler.ListenerAcknowledgement.justIntegrated(noteId, "kibble discarded"))
//                  } else {
//                    getContainer(text) match {
//                      case None =>
//                        context.actorContext.log.warn(s"Kibble/dry food mentioned and identified mass ${mass}g but could not identify container: $text")
//                        Ignored
//                      case Some(container) =>
//                        if (lowerText.contains("refill")) {
//                          context.actorContext.log.info(s"Detected refill for $container of ${mass}g")
//                          manager !! KibbleRefill(container, mass, captureTime, noteId)
//                          Acknowledged(Chronicler.ListenerAcknowledgement.justIntegrated(noteId, "kibble refilled"))
//                        } else if (lowerText.contains("measure")) {
//                          context.actorContext.log.info(s"Detected measure for $container of ${mass}g")
//                          manager !! RemainingKibbleMeasure(container, mass, captureTime, noteId)
//                          Acknowledged(Chronicler.ListenerAcknowledgement.justIntegrated(noteId, "kibble measured"))
//                        } else {
//                          context.actorContext.log.warn(s"Identified kibble/dry food reference for container $container and mass ${mass}g but could not identify choice {refill, measure}: $text")
//                          Ignored
//                        }
//                    }
//                  }
//              }
            } else {
              context.actorContext.log.debug(s"Ignoring because mentionsKibble=$mentionsKibble, mentionsDryFood=$mentionsDryFood")
              Ignored
            }
          }
      }
    }

  private def getGrams(text: String): Option[Int] = {
    // just split on whitespace
    text.splitLikePy.flatMap { textSegment =>
      List(
        textSegment.dropRight(1).toIntOption // in case of punctuation; must be first for lastOption to be correct
        , textSegment.toIntOption
      ).flatten
      // in case I correct myself
    }.lastOption
  }

  private def getContainer(text: String): Option[KibbleContainer] = {
    if (text.contains("circ") && text.contains("prime")) {
      Some(Circular1)
    } else if (text.contains("circ") && text.contains("secondary")) {
      Some(Circular2)
    } else if (text.contains("rect") && text.contains("small")) {
      Some(RectangularS)
    } else if (text.contains("rect") && text.contains("large")) {
      Some(RectangularL)
    } else {
      None
    }
  }
}
