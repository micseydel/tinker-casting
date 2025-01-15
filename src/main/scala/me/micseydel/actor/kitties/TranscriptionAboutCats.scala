package me.micseydel.actor.kitties

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import me.micseydel.actor.kitties.CatTranscriptionListener.TranscriptionEvent
import me.micseydel.model._
import me.micseydel.util.StringUtil
import me.micseydel.vault.NoteId

import java.time.ZonedDateTime

object TranscriptionAboutCats {
  sealed trait TranscriptionAboutCats

  final case class WithIntent(rawTextStart: String, noteId: NoteId, captureTime: ZonedDateTime, catMessage: CatsHelper.Observation, intentConfidence: Double) extends TranscriptionAboutCats

  final case class WithIntentFailedExtraction(rawTextStart: String, noteId: NoteId, captureTime: ZonedDateTime, intentString: String, intentConfidence: Double, problems: NonEmptyList[String]) extends TranscriptionAboutCats

  final case class NoIntentJustWordMatch(rawTextStart: String, noteId: NoteId, captureTime: ZonedDateTime, matchedWords: Map[String, Int]) extends TranscriptionAboutCats

  final case class NotAboutCats(noteId: NoteId, potentialWords: Map[String, Int]) extends TranscriptionAboutCats

  private def nonIntentResult(rawText: String, noteId: NoteId, captureTime: ZonedDateTime): TranscriptionAboutCats = {
    val rawTextStart = StringUtil.truncateText(rawText)
    val catMentions = recognizedCatMentions(rawText.toLowerCase)
    if (catMentions.nonEmpty) {
      NoIntentJustWordMatch(rawTextStart, noteId, captureTime, catMentions)
    } else {
      val potentialWords = potentialCatMentions(rawText)
      NotAboutCats(noteId, potentialWords)
    }
  }

  def unapply(transcriptionEvent: TranscriptionEvent): Option[TranscriptionAboutCats] = {
    transcriptionEvent match {
      case TranscriptionEvent(NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(rawText, _), _), captureTime), noteId, Some(rasaResult))) =>
        val rawTextStart = StringUtil.truncateText(rawText)
        val validatedCatsHelperMessage = rasaResultToCatsHelperObservation(rasaResult, captureTime, noteId)
        validatedCatsHelperMessage match {
          // successful cat intent
          case Some(Valid(catMessage)) =>
            Some(WithIntent(rawTextStart, noteId, captureTime, catMessage, rasaResult.intent.confidence))

          // there was a cat intent but extraction failed
          case Some(Invalid(problems)) =>
            Some(WithIntentFailedExtraction(rawTextStart, noteId, captureTime, rasaResult.intent.name, rasaResult.intent.confidence, problems))

          // no cat intent
          case None =>
            Some(nonIntentResult(rawText, noteId, captureTime))
        }

      // no cat intent
      case TranscriptionEvent(NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(rawText, _), _), captureTime), noteId, None)) =>
        Some(nonIntentResult(rawText, noteId, captureTime))
    }
  }

  private def recognizedCatMentions(rawText: String): Map[String, Int] = {
    if (rawText.startsWith("I just ate butter chicken")) {
      // the cat and the Trader Joe's microwave meal are not the same...
      Map.empty
    } else {
      val knownWords = Set("peanut", "butter", "cat", "litter")
      countWordMentions(rawText, knownWords)

    }
  }

  private def potentialCatMentions(rawText: String): Map[String, Int] = {
    val knownWords = Set("letter", "front", "back", "better")
    countWordMentions(rawText, knownWords)
  }

  private def countWordMentions(rawText: String, matchWords: Set[String]): Map[String, Int] = {
    matchWords
      .map(word =>
        // https://chat.openai.com/c/d89cd956-d08b-44db-a1f6-f8582baff25b
        // SO looked pretty bad too
        word -> rawText.sliding(word.length).count(window => window == word))
      .toMap
      .filter { case (_, value) =>
        value > 0
      }
  }

  private def rasaResultToCatsHelperObservation(result: RasaResult,
                                            captureTime: ZonedDateTime, noteId: NoteId
                                           ): Option[ValidatedNel[String, CatsHelper.Observation]] = {

    result match {
      case KnownIntent.posthoc_litter_observation(validated) =>
        Some(validated.map {
          case PosthocLitterObservation(confidence, litterbox, isClean) =>
            CatsHelper.PostHocLitterObservation(PostHocLitterObservationEvent(captureTime, litterbox, isClean), result.text, noteId, confidence)
        })
      case KnownIntent.observed_litter_being_used(validated) =>
        Some(validated.map {
          case ObservedLitterBeingUsed(confidence, litterBoxChoice, maybeCatOfMine, maybeUncertainty) =>
            CatsHelper.ObservedCatUsingLitter(
              LitterUsedEvent(captureTime, litterBoxChoice, maybeCatOfMine),
              result.text,
              noteId, confidence,
              // if there's any uncertainty, we are not fully certain
              // treating a lack of expressed uncertainty as certainty is an experiment
              observationFullyCertain = maybeUncertainty.nonEmpty
            )
        })
      case KnownIntent.observe_sifted_contents(validated) =>
        Some(validated.map {
          // FIXME: confidence
          case ObserveSiftedContents(confidence, litterbox, siftedContents) =>
            val event: LitterSiftedEvent = LitterSiftedEvent(captureTime, litterbox, siftedContents)
            CatsHelper.LitterSifted(event, result.text, noteId, confidence)
        })
      case _ =>
        None
    }
  }
}
