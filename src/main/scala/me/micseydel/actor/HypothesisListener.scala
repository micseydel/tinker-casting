package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerColor, TinkerContext, Tinkerer}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler.ListenerAcknowledgement
import me.micseydel.dsl.tinkerer.TinkerListener
import me.micseydel.dsl.tinkerer.TinkerListener.{Acknowledged, Ignored}
import me.micseydel.model.{Entity, IntentRanking, NotedTranscription, RasaResult}
import me.micseydel.util.{MarkdownUtil, StringUtil}
import me.micseydel.util.StringImplicits.RichString

object HypothesisListener {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[TinkerListener.Message] = {
//    behavior()
    Behaviors.ignore
  }

  private def behavior()(implicit Tinker: Tinker): Ability[TinkerListener.Message] = Tinkerer(TinkerColor.rgb(30, 250, 70), "🥼").setup { context =>
    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[NotedTranscription]]] = context.cast(DailyNotesRouter(
      "Hypotheses",
      "hypotheses",
      NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat,
      HypothesesMarkdown.apply
    ), "DailyNotesRouter")

    implicit val c: TinkerContext[_] = context

    dailyNotesAssistant !! DailyNotesRouter.Envelope(DailyMarkdownFromPersistedMessagesActor.RegenerateMarkdown(), context.system.clock.now())

    TinkerListener.simpleStateless { (_, transcription) =>
      transcription match {
        case NotedTranscription(capture, noteId) =>
          val text = capture.whisperResult.whisperResultContent.text
          val lowerText = text.toLowerCase

          val maybeDetails = if (lowerText.contains("always") || lowerText.contains("never")) {
            Some("binary thinking detected")
          } else if (lowerText.contains("hypothesis")) {
            Some(if (text.wordCount > 20) {
              "A long hypothesis was detected"
            } else {
              s": $text"
            })
          } else {
            None
          }

          maybeDetails match {
            case None =>
              Ignored

            case Some(details) =>
              dailyNotesAssistant !! DailyNotesRouter.Envelope(DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(transcription), transcription.capture.captureTime)
              Acknowledged(ListenerAcknowledgement(noteId, context.system.clock.now(), details, None))
          }
      }
    }
  }
}

private object HypothesesMarkdown {
  def apply(messages: List[NotedTranscription], clock: TinkerClock): String = {
    messages.map {
      case NotedTranscription(capture, noteId) =>
        MarkdownUtil.listLineWithTimestampAndRef(capture.captureTime, StringUtil.truncateText(capture.whisperResult.whisperResultContent.text), noteId)

//      case NotedTranscription(capture, noteId, Some(RasaResult(entities, intent, intent_ranking, _, _))) =>
//        val base = MarkdownUtil.listLineWithTimestampAndRef(capture.captureTime, StringUtil.truncateText(capture.whisperResult.whisperResultContent.text), noteId)
//
//        val extraLines: List[String] =
//          if (intent.name == "no_intent" && intent.confidence > 0.99) {
//            List("no_intent")
//          } else {
//            List(
//              Some(s"    - intent: $intent"),
//              Some(entities).filter(_.nonEmpty).map("    - entities\n" + _.map {
//                case Entity(confidence_entity, confidence_group, end, entity, extractor, group, start, value) =>
//                  s"        - $entity=$value ($confidence_entity)"
//              }.mkString("\n")),
//              Some(intent_ranking).filter(_.nonEmpty && intent.confidence < 0.9).map(_.map {
//                case IntentRanking(confidence, name) =>
//                  s"    - $name = $confidence"
//              }).map(lines => (intent.name :: lines).mkString("    - ", "\n    - ", "\n"))
//            ).flatten
//          }
//
//        if (intent.name == "no_intent" && intent.confidence > 0.99) {
//          base
//        } else {
//          (base :: extraLines).mkString("\n")
//        }
    }.mkString("", "\n", "\n")
  }
}
