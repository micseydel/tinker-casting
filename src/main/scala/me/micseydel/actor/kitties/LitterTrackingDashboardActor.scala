package me.micseydel.actor.kitties

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext}
import me.micseydel.model._
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

object LitterTrackingDashboardActor {
  sealed trait Message

  final case class PartialMatch(notedTranscription: NotedTranscription, rasaRasa: RasaResult) extends Message

  private case class ReceiveNotePing(ping: Ping) extends Message

  //

  val NoteName = "Litter Tracking Dashboard"

  def apply(
          //   litterBoxesHelper: SpiritRef[LitterBoxesHelper.Message]
           )(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](NoteName, CatBrown, "🧪", ReceiveNotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context
      implicit val clock: TinkerClock = context.system.clock
      implicit val l: Logger = context.actorContext.log

      Tinker.receiveMessage {
        case PartialMatch(NotedTranscription(capture, noteId), rasaResult) =>
          context.actorContext.log.info(s"Got a partial match on $noteId")
          noteRef.addToInbox(noteId, capture.captureTime, rasaResult) match {
            case Failure(exception) => throw exception
            case Success(NoOp) =>
          }
          Tinker.steadily

        case ReceiveNotePing(NoOp) =>
          if (context.actorContext.log.isDebugEnabled) {
            context.actorContext.log.debug(s"Ignoring")
          }
          Tinker.steadily
      }
    }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def addToInbox(noteId: NoteId, time: ZonedDateTime, rasaResult: RasaResult)(implicit log: Logger): Try[NoOp.type] = {
      rasaResult match {
        case RasaResult(entities, intent, intent_ranking, text, _) =>
          val formattedEntities = entities.map {
            case Entity(confidence_entity, confidence_group, _, entity, extractor, group, _, value) =>
              s"$entity $value $confidence_entity ($group, $confidence_group)"
          }.mkString("        - ", "\n        - ", "\n")

          val contents = if (intent.name == KnownIntent.observe_sifted_contents.IntentName && intent.confidence > 0.7) {
            //   litterBoxesHelper !! LitterBoxesHelper.LitterSifted(LitterSiftedEvent(time, BackLitter, SiftedContents(List(FIXME))), noteId)
            s"""${MarkdownUtil.listLineWithTimestampAndRef(time, s"(entity extraction failed) $text", noteId)}
               |$formattedEntities""".stripMargin
          } else {
            val formattedIntentRanking = intent_ranking.map {
              case IntentRanking(confidence, name) =>
                s"$name $confidence"
            }.mkString("        - ", "\n        - ", "\n")

            s"""${MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId)}
               |    - intent: $intent
               |    - intent ranking:
               |$formattedIntentRanking
               |    - entities:
               |$formattedEntities""".stripMargin
          }

          log.debug(s"Adding contents: $contents")

          noteRef.append(contents)
      }
    }

    def readyEvents(): Unit = {
      // FIXME inbox - ideally with some way to say "ok now go forward" that updates `litterBoxesHelper`
      // FIXME: what info do I need? can I fill out some, and wait for the rest?
      val litterBoxChoice = BackLitter
      //          val siftedContents = SiftedContents(List()) // FIXME
      //          val event = LitterSiftedEvent(capture.captureTime, litterBoxChoice, siftedContents)
      //                litterBoxesHelper !! LitterBoxesHelper.LitterSifted(event, noteId)
      //          val litterUsedEvent = LitterUsedEvent(capture.captureTime, litterBoxChoice, None) // FIXME
      //                litterBoxesHelper !! LitterBoxesHelper.ObservedCatUsingLitter(litterUsedEvent, noteId)
      //          val postHocLitterObservationEvent = PostHocLitterObservationEvent(capture.captureTime, litterBoxChoice, isClean = false) // FIXME
      //                litterBoxesHelper !! LitterBoxesHelper.PostHocLitterObservation(postHocLitterObservationEvent, noteId)
    }
  }
}
