package me.micseydel.actor.kitties

import me.micseydel.actor.DailyMarkdownFromPersistedMessagesActor.{RegenerateMarkdown, StoreAndRegenerateMarkdown}
import me.micseydel.actor.kitties.CatTranscriptionListener.TranscriptionEvent
import me.micseydel.actor.kitties.CatsHelper.{Message, PartialMatch}
import me.micseydel.actor.kitties.LitterBoxesHelper.{EventCapture, LitterSifted, ObservedCatUsingLitter, PostHocLitterObservation}
import me.micseydel.actor.{DailyMarkdownFromPersistedMessagesActor, DailyNotesRouter}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.RasaAnnotatingListener.RasaAnnotatedNotedTranscription
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerColor, TinkerContext, Tinkerer}
import me.micseydel.model._
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.{LinkIdJsonProtocol, NoteId}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

object LitterBoxesHelper {
  // mailbox

  sealed trait Message

  final case class ReceivePartialMatch(pm: PartialMatch) extends Message

  sealed trait EventCapture extends Message {
    def event: CatObservationEvent
    def when: ZonedDateTime = event.when
  }

  final case class PostHocLitterObservation(event: PostHocLitterObservationEvent, ref: NoteId) extends EventCapture
  final case class ObservedCatUsingLitter(event: LitterUsedEvent, ref: NoteId) extends EventCapture
  final case class LitterSifted(event: LitterSiftedEvent, ref: NoteId) extends EventCapture

  private case class TimeHasPassed() extends Message

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor.CatBrown, "🤖").setup { context =>
    implicit val c: TinkerContext[_] = context

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[EventCapture]]] = context.cast(DailyNotesRouter(
      "Litter boxes",
      "litter_boxes",
      LitterBoxesEventCaptureListJsonProtocol.EventCaptureJsonFormat,
      toMarkdown
    ), "DailyNotesRouter")

    context.actorContext.log.info("Regenerating Markdown in case things have changed")
    dailyNotesAssistant !! DailyNotesRouter.Envelope(RegenerateMarkdown(), context.system.clock.now())

    val justSiftingReport: SpiritRef[LitterBoxReportActor.Message] = context.cast(
      LitterBoxReportActor(),
      "LitterBoxesSifting_MOC"
    )

    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

    behavior(timeKeeper, dailyNotesAssistant, justSiftingReport)
  }

  private def behavior(timeKeeper: SpiritRef[TimeKeeper.Message], dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[EventCapture]]], justSiftingReport: SpiritRef[LitterBoxReportActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      // FIXME: need to set a timer for clumping situations
      case capture@LitterSifted(LitterSiftedEvent(when, _, _), _) =>
        justSiftingReport !! LitterBoxReportActor.LitterSiftedObservation(capture)
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(capture), when.toLocalDate)
        Tinker.steadily


      case nonSiftEventCapture@PostHocLitterObservation(PostHocLitterObservationEvent(when, _, isClean), _) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(nonSiftEventCapture), nonSiftEventCapture.when.toLocalDate)
        if (!isClean) {
          context.actorContext.log.info("Noticed dirty litter, setting a timer for 20 minutes")
          // FIXME: use `when` in case transcription was not instant
          timeKeeper !! TimeKeeper.RemindMeIn(20.minutes, context.self, TimeHasPassed(), None)
        }
        Tinker.steadily

      case nonSiftEventCapture@ObservedCatUsingLitter(LitterUsedEvent(when, _, maybeCat), _) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(nonSiftEventCapture), nonSiftEventCapture.when.toLocalDate)
        // FIXME: use `when` in case transcription was not instant
        timeKeeper !! TimeKeeper.RemindMeIn((if (maybeCat.contains(Butter)) 15 else 20).minutes, context.self, TimeHasPassed(), None)
        Tinker.steadily

      case ReceivePartialMatch(PartialMatch(TranscriptionEvent(RasaAnnotatedNotedTranscription(NotedTranscription(TranscriptionCapture(whisperResult, captureTime), noteId), maybeRasaResult)))) =>
        justSiftingReport !! LitterBoxReportActor.AddToInbox(MarkdownUtil.listLineWithTimestampAndRef(captureTime,
          whisperResult.whisperResultContent.text,  // siftedContents.toEmojis,
          noteId), captureTime)
        Tinker.steadily

      case TimeHasPassed() =>
        context.actorContext.log.info("Time has passed, regenerating markdown")
        dailyNotesAssistant !! DailyNotesRouter.Envelope(RegenerateMarkdown(), context.system.clock.now())
        Tinker.steadily
    }
  }

  //

  private def toMarkdown(events: List[EventCapture], clock: TinkerClock): String = {
    s"""# Summary
       |
       |${State.fromEvents(events)(clock).pretty()}
       |
       |# Events
       |
       |${generateEventList(events)}
       |""".stripMargin
  }

  private def generateEventList(events: List[EventCapture]): String = events.map {
    case PostHocLitterObservation(PostHocLitterObservationEvent(when, litterBoxChoice, isClean), ref) =>
      (when, s"Observed $litterBoxChoice isClean=$isClean", ref)
    case ObservedCatUsingLitter(LitterUsedEvent(when, litterBoxChoice, cat), ref) =>
      (when, s"Observed cat $cat used $litterBoxChoice", ref)
    case LitterSifted(LitterSiftedEvent(when, litterBoxChoice, contents), ref) =>
      (when, s"Sifted $litterBoxChoice and found ${contents.multiset}", ref)
  }.map { case (when, line, ref) =>
    MarkdownUtil.listLineWithTimestampAndRef(when, line, ref)
  }.mkString("\n") + "\n"

  //

  sealed trait LitterBoxState

  private case object Clean extends LitterBoxState
  private case object Clumping extends LitterBoxState
  private case object AwaitingSifting extends LitterBoxState

  case class State(firstBox: Option[(LitterBoxState, NoteId, ZonedDateTime)], secondBox: Option[(LitterBoxState, NoteId, ZonedDateTime)]) {
    def pretty(): String = {
      s"""- FrontLitterBox: ${format(firstBox)}
         |- BackLitterBox: ${format(secondBox)}""".stripMargin
    }

    private def format(maybeBox: Option[(LitterBoxState, NoteId, ZonedDateTime)]): String = {
      maybeBox match {
        case Some((litterBoxState, ref, when)) =>
          val refString = ref.wikiLinkWithAlias("ref")
          val prettyState = litterBoxState match {
            case AwaitingSifting => s"==${litterBoxState.toString}=="
            case _ => litterBoxState.toString
          }

          s"$prettyState (${refString})"

        case None =>
          "no events today"
      }
    }
  }

  private object State {
    def fromEvents(events: List[EventCapture])(implicit tinkerClock: TinkerClock): State = {
      // just need to look at the latest event for each litter box
      val reversedEvents = events.reverse

      val justFrontLitterLatest: Option[(LitterBoxState, NoteId, ZonedDateTime)] = getLatest(reversedEvents, FrontLitter)
      val justBackLitterLatest: Option[(LitterBoxState, NoteId, ZonedDateTime)] = getLatest(reversedEvents, BackLitter)
      val bothLitterLatest = getLatest(reversedEvents, Both)

      val frontLitterLatest = (bothLitterLatest.toSeq ++ justFrontLitterLatest.toSeq).maxByOption(_._3)
      val backLitterLatest = (bothLitterLatest.toSeq ++ justBackLitterLatest.toSeq).maxByOption(_._3)

      State(frontLitterLatest, backLitterLatest)
    }

    private def getLatest(reversedEvents: List[EventCapture], LitterBox: LitterBoxChoice)(implicit tinkerClock: TinkerClock): Option[(LitterBoxState, NoteId, ZonedDateTime)] = {
      reversedEvents.collectFirst {
        case PostHocLitterObservation(PostHocLitterObservationEvent(when, LitterBox, isClean), ref) =>
          val state = if (isClean) {
            Clean
          } else {
            if (TimeUtil.timeSince(when).toMinutes < 20) {
              Clumping
            } else {
              AwaitingSifting
            }
          }

          (state, ref, when)

        case ObservedCatUsingLitter(LitterUsedEvent(when, LitterBox, maybeCat), ref) =>
          val delayMinutes = maybeCat match {
            case Some(Butter) => 15
            case _ => 20
          }
          val state = if (TimeUtil.timeSince(when).toMinutes < delayMinutes) {
            Clumping
          } else {
            AwaitingSifting
          }

          (state, ref, when)

        case LitterSifted(LitterSiftedEvent(when, LitterBox, _), ref) =>
          (Clean, ref, when)
      }
    }
  }
}

case object LitterBoxesEventCaptureListJsonProtocol extends DefaultJsonProtocol {

  import LitterSiftedEventJsonProtocol.litterSiftedEventFormat
  import LitterUsedEventJsonProtocol.LitterUsedEventJsonFormat
  import PostHocLitterObservationEventJsonProtocol.PostHocLitterObservationEventJsonFormat

  implicit val linkIdFormat: JsonFormat[NoteId] = LinkIdJsonProtocol.noteIdFormat
  implicit val postHocLitterObservationFormat: RootJsonFormat[PostHocLitterObservation] = jsonFormat2(PostHocLitterObservation)
  implicit val observedCatUsingLitterFormat: RootJsonFormat[ObservedCatUsingLitter] = jsonFormat2(ObservedCatUsingLitter)
  implicit val litterSiftedFormat: RootJsonFormat[LitterSifted] = jsonFormat2(LitterSifted)

  // copy from CatsHelper.scala
  implicit object EventCaptureJsonFormat extends RootJsonFormat[EventCapture] {
    def write(m: EventCapture): JsValue = {
      val (jsObj, typ) = m match {
        case p: PostHocLitterObservation => (p.toJson.asJsObject, "PostHocLitterObservation")
        case o: ObservedCatUsingLitter => (o.toJson.asJsObject, "ObservedCatUsingLitter")
        case l: LitterSifted => (l.toJson.asJsObject, "LitterSifted")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): EventCapture = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("PostHocLitterObservation")) => value.convertTo[PostHocLitterObservation]
        case Seq(JsString("ObservedCatUsingLitter")) => value.convertTo[ObservedCatUsingLitter]
        case Seq(JsString("LitterSifted")) => value.convertTo[LitterSifted]
        case other => throw DeserializationException(s"Unknown type, expected something in {PostHocLitterObservation, ObservedCatUsingLitter, LitterSifted} but got $other")
      }
    }
  }

  val messageListFormat: RootJsonFormat[List[EventCapture]] = listFormat(EventCaptureJsonFormat)
}
