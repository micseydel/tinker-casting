package me.micseydel.actor

import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler.ListenerAcknowledgement
import me.micseydel.dsl.tinkerer.TinkerListener
import me.micseydel.dsl.tinkerer.TinkerListener.{Acknowledged, Ignored}
import me.micseydel.model.NotedTranscription
import me.micseydel.util.StringImplicits.RichString

object HypothesisListener {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[TinkerListener.Message] = {
    behavior()
  }

  private def behavior()(implicit Tinker: Tinker): Ability[TinkerListener.Message] = {
    TinkerListener.simpleStateless { (context, transcription) =>
      transcription match {
        case NotedTranscription(capture, noteId, rasaResult) =>
          val text = capture.whisperResult.whisperResultContent.text
          if (text.toLowerCase.contains("hypothesis")) {
            val details = if (text.wordCount > 20) {
              "A long hypothesis was detected"
            } else {
              s": $text"
            }
            Acknowledged(ListenerAcknowledgement(noteId, context.system.clock.now(), details, None))
          } else {
            Ignored
          }
      }
    }
  }
}
