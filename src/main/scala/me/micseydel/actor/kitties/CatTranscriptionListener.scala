package me.micseydel.actor.kitties

import me.micseydel.actor.DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.actor.kitties.TranscriptionAboutCats.{NoIntentJustWordMatch, NotAboutCats, WithIntent, WithIntentFailedExtraction}
import me.micseydel.actor.{DailyMarkdownFromPersistedMessagesActor, DailyNotesRouter}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
import me.micseydel.model._
import me.micseydel.util.{MarkdownUtil, StringUtil}
import me.micseydel.vault.NoteId
import org.slf4j.Logger
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.text.DecimalFormat
import java.time.ZonedDateTime

object CatTranscriptionListener {
  // actor pattern
  sealed trait Message {
    def when: ZonedDateTime
  }

  case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message {
    override def when: ZonedDateTime = notedTranscription.capture.captureTime
  }

  def apply(catsHelper: SpiritRef[CatsHelper.Message])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(CatBrown, "🐱").setup { context =>
    implicit val c: TinkerContext[_] = context
    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[TranscriptionEvent]]] = context.cast(DailyNotesRouter(
      "CatsTranscriptions notes",
      "catstranscriptions_notes",
      MessageListJsonProtocol.transcriptionEventFormat,
      toMarkdown
    ), "DailyNotesRouter")

    context.actorContext.log.info(s"Subscribing to accurate Gossiper messages")
    context.system.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(TranscriptionEvent))

    behavior(catsHelper, dailyNotesAssistant, Set.empty)
  }

  private def behavior(
                        catsHelper: SpiritRef[CatsHelper.Message],
                        dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[TranscriptionEvent]]],
                        alreadyAcked: Set[NoteId]
                      )(implicit Tinker: Tinker): Ability[Message] = Tinker.receive[Message] { (context, message) =>
    implicit val c: TinkerContext[_] = context
    context.actorContext.log.debug("Received message, processing...")
    message match {
      // FIXME: remove/replace this indirection (REGRET)
      case event@TranscriptionAboutCats(WithIntent(_, noteId, captureTime, catMessage, confidence)) =>
        context.actorContext.log.info(s"Received $noteId about cats at capture time $captureTime, extracted ${catMessage.getClass} with confidence $confidence, noting in MOC and sending to CatsHelper")
        catsHelper !! catMessage
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(event), captureTime.toLocalDate)
        val pretty = catMessage match {
          case CatsHelper.PostHocLitterObservation(PostHocLitterObservationEvent(_, litterBoxChoice, isClean), _, _, _) =>
            if (isClean) {
              s"👀🧼 ${litterBoxChoice.emoji}"
            } else {
              s"👀🌋 ${litterBoxChoice.emoji}"
            }
          case CatsHelper.ObservedCatUsingLitter(LitterUsedEvent(_, litterBoxChoice, maybeCat), _, _, _, _) =>
            val catEmoji = maybeCat.map(_.emoji).getOrElse("👂")
            s"$catEmoji (${litterBoxChoice.emoji})"
          case CatsHelper.LitterSifted(LitterSiftedEvent(_, _, contents), _, _, _) =>
            s"🧹 ${contents.toEmojis}"
        }

        if (!alreadyAcked.contains(noteId)) {
          context.system.chronicler !! Chronicler.ListenerAcknowledgement(noteId, context.system.clock.now(), s"Extracted cat message $pretty", Some(AutomaticallyIntegrated))
          behavior(catsHelper, dailyNotesAssistant, alreadyAcked + noteId)
        } else {
          event match {
            case TranscriptionEvent(NotedTranscription(TranscriptionCapture(WhisperResult(_, WhisperResultMetadata(model, performedOn, _, perfCounterElapsed)), _), _, _)) =>
              context.actorContext.log.debug(s"Already ack'd $noteId, ignoring for model $model, performed on $performedOn taking $perfCounterElapsed")
          }
          Tinker.steadily
        }

      case event@TranscriptionAboutCats(WithIntentFailedExtraction(_, noteId, captureTime, intent, confidence, problems)) =>
        context.actorContext.log.debug(s"Received $noteId about cats at capture time $captureTime, failed to extract $intent with confidence $confidence, noting in MOC but NOT sending to CatsHelper because: $problems")
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(event), captureTime.toLocalDate)
        Tinker.steadily
      case event@TranscriptionAboutCats(NoIntentJustWordMatch(_, noteId, captureTime, matchedWords)) =>
        context.actorContext.log.info(s"Received $noteId about cats at capture time $captureTime, no intent detected but found match words with count $matchedWords")
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(event), captureTime.toLocalDate)
        Tinker.steadily
      case TranscriptionAboutCats(NotAboutCats(noteId, potentialWords)) =>
        if (potentialWords.nonEmpty) {
          context.actorContext.log.debug(s"Ignoring $noteId, not about cats, though includes POTENTIAL match words with counts $potentialWords")
        } else {
          context.actorContext.log.debug(s"Ignoring $noteId, not about cats")
        }
        Tinker.steadily

      case other =>
        context.actorContext.log.error(s"Expected a TranscriptionAboutCats but got $other")
        Tinker.steadily
    }
  }

  case object MessageListJsonProtocol extends DefaultJsonProtocol {
    import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat

    implicit val transcriptionEventFormat: RootJsonFormat[TranscriptionEvent] = jsonFormat1(TranscriptionEvent)
  }

  private val PctFormatter = new DecimalFormat("#.##")

  private def toMarkdown(messages: List[TranscriptionEvent], clock: TinkerClock): String = {
    def messageToUnfilteredListLine(message: Message): Either[String, String] = {
      message match {
        case TranscriptionAboutCats(WithIntent(rawTextStart, _, _, _, _)) =>
          Right(s"~~$rawTextStart~~")
        case TranscriptionAboutCats(WithIntentFailedExtraction(rawTextStart, _, _, _, _, _)) =>
          Right(rawTextStart)
        case TranscriptionAboutCats(NoIntentJustWordMatch(rawTextStart, _, _, _)) =>
          Right(rawTextStart)
        case TranscriptionAboutCats(NotAboutCats(noteId, potentialWords)) =>
          if (potentialWords.nonEmpty) {
            Left(s"Ignoring $noteId, not about cats, though includes POTENTIAL match words with counts $potentialWords")
          } else {
            Left(s"Ignoring $noteId, not about cats")
          }
        case other =>
          Left(s"Expected a TranscriptionAboutCats but got $other")
      }
    }

    val allNoteFormatted: String = messages.distinctBy(_.notedTranscription.noteId).flatMap {
      case message@TranscriptionEvent(NotedTranscription(TranscriptionCapture(_, captureTime), ref@NoteId(_), _)) =>
        messageToUnfilteredListLine(message) match {
          case Right(line) =>
            Some(MarkdownUtil.listLineWithTimestampAndRef(captureTime, line, ref))
          case Left(errMsg) =>
//            log.foreach(_.warn(s"Did not detect cat stuff: $errMsg"))
            None
        }
    }.mkString("\n") + "\n"

    val unstructuredNotesFormatted = messages.flatMap {
      case TranscriptionAboutCats(WithIntent(_, _, _, _, _)) =>
        None
      case other =>
        other match {
          case TranscriptionEvent(NotedTranscription(TranscriptionCapture(whisperResult, captureTime), noteId, rasaResult)) =>
            val rawTextStart = StringUtil.truncateText(whisperResult.whisperResultContent.text)
            rasaResult match {
              case Some(RasaResult(_, Intent(intentConfidence, "no_intent"), _, _, _)) if intentConfidence > .85 =>
                Some(MarkdownUtil.listLineWithTimestampAndRef(captureTime, rawTextStart, noteId))

              case Some(RasaResult(entities, Intent(intentConfidence, intentName), intentRanking, _, _)) =>
                val block = if (intentConfidence > 0.70) {
                  List(
                    Some(MarkdownUtil.listLineWithTimestampAndRef(captureTime, rawTextStart, noteId)),
                    Some(s"    - ${PctFormatter.format(intentConfidence * 100)}% confidence for $intentName"),
                    entitiesToLines(entities)
                  ).flatten.mkString("\n")
                } else {
                  val viablePotentialIntents = intentRanking.filter(_.confidence > 0.01)

                  List(
                    List(MarkdownUtil.listLineWithTimestampAndRef(captureTime, rawTextStart, noteId)),
                    if (entities.nonEmpty) {
                      s"    - ${PctFormatter.format(intentConfidence * 100)}% confidence for $intentName" ::
                        entitiesToLines(entities)
                    } else {
                      Nil
                    },
                    if (viablePotentialIntents.nonEmpty) {
                      "    - potential intents" ::
                        viablePotentialIntents.map {
                          case IntentRanking(lessLikelyConfidence, lessLikelyIntent) =>
                            s"        - $lessLikelyIntent ${PctFormatter.format(lessLikelyConfidence * 100)}%"
                        }
                    } else {
                      Nil
                    }
                  ).flatten.mkString("\n")
                }

                Some(block)
              case None =>
                Some(MarkdownUtil.listLineWithTimestampAndRef(captureTime, rawTextStart, noteId))
            }

          case _ =>
            None
        }
    }.mkString("\n")

    s"""# All
       |
       |$allNoteFormatted
       |# Not automatically structured
       |
       |$unstructuredNotesFormatted
       |""".stripMargin
  }

  private def entitiesToLines(entities: List[Entity]): List[String] = {
    if (entities.nonEmpty) {
      entities.map {
        case Entity(confidenceEntity, _, _, entity, _, _, _, value) =>
          s"        - $entity: $value (${PctFormatter.format(confidenceEntity * 100)}%)"
      }
    } else {
      Nil
    }
  }
}
