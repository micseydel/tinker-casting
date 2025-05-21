package me.micseydel.actor.kitties

import akka.actor.typed.ActorRef
import me.micseydel.actor.DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown
import me.micseydel.actor.kitties.CatTranscriptionListener.TranscriptionEvent
import me.micseydel.actor.kitties.kibble.KibbleManagerActor
import me.micseydel.actor.{DailyMarkdownFromPersistedMessagesActor, DailyNotesRouter, RasaActor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.RasaAnnotatingListener.RasaAnnotatedNotedTranscription
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
import me.micseydel.model._
import me.micseydel.vault._
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.text.DecimalFormat
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.unused

object CatsHelper {
  // actor pattern

  sealed trait Message

  final case class PartialMatch(transcriptionEvent: TranscriptionEvent) extends Message

  sealed trait Observation extends Message {
    def event: CatObservationEvent
    def when: ZonedDateTime = event.when
    def transcriptionText: String
    def ref: NoteId
    def intentConfidence: Double
  }

  // FIXME: add seen/heard or AT least ref and certain/uncertain

  final case class PostHocLitterObservation(event: PostHocLitterObservationEvent, transcriptionText: String, ref: NoteId, intentConfidence: Double) extends Observation

  final case class ObservedCatUsingLitter(event: LitterUsedEvent, transcriptionText: String, ref: NoteId, intentConfidence: Double, observationFullyCertain: Boolean) extends Observation

  final case class LitterSifted(event: LitterSiftedEvent, transcriptionText: String, ref: NoteId, intentConfidence: Double) extends Observation

  // helpers

  case object MessageListJsonProtocol extends DefaultJsonProtocol {
    import LitterSiftedEventJsonProtocol.litterSiftedEventFormat
    import LitterUsedEventJsonProtocol.LitterUsedEventJsonFormat
    import PostHocLitterObservationEventJsonProtocol.PostHocLitterObservationEventJsonFormat

    implicit val linkIdFormat: JsonFormat[NoteId] = LinkIdJsonProtocol.noteIdFormat
    //    implicit val catOfMineFormat: RootJsonFormat[CatOfMine] = CatOfMine.CatOfMineFormat

    implicit val postHocLitterObservationFormat: RootJsonFormat[PostHocLitterObservation] = jsonFormat4(PostHocLitterObservation)
    implicit val observedCatUsingLitterFormat: RootJsonFormat[ObservedCatUsingLitter] = jsonFormat5(ObservedCatUsingLitter)
    implicit val litterSiftedFormat: RootJsonFormat[LitterSifted] = jsonFormat4(LitterSifted)

    implicit object MessageJsonFormat extends RootJsonFormat[Observation] {
      def write(m: Observation): JsValue = {
        val (jsObj, typ) = m match {
          case p: PostHocLitterObservation => (p.toJson.asJsObject, "PostHocLitterObservation")
          case o: ObservedCatUsingLitter => (o.toJson.asJsObject, "ObservedCatUsingLitter")
          case l: LitterSifted => (l.toJson.asJsObject, "LitterSifted")
        }
        JsObject(jsObj.fields + ("type" -> JsString(typ)))
      }

      def read(value: JsValue): Observation = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("PostHocLitterObservation")) => value.convertTo[PostHocLitterObservation]
          case Seq(JsString("ObservedCatUsingLitter")) => value.convertTo[ObservedCatUsingLitter]
          case Seq(JsString("LitterSifted")) => value.convertTo[LitterSifted]
          case other => throw DeserializationException(s"Unknown type, expected something in {PostHocLitterObservation, ObservedCatUsingLitter, LitterSifted} but got $other")
        }
      }
    }
    val messageListFormat: RootJsonFormat[List[Observation]] = listFormat(MessageJsonFormat)
  }

  def apply()(implicit Tinker: EnhancedTinker[ActorRef[RasaActor.Message]]): Ability[Message] = Tinkerer(CatBrown, "🐱").setup { context =>
    @unused
    val catTranscriptionListener: SpiritRef[CatTranscriptionListener.Message] = context.cast(CatTranscriptionListener(context.self), "CatTranscriptionListener")

    // has its own internal listener
    @unused
    val kibbleManager = context.cast(KibbleManagerActor(), "KibbleManagerActor")

    val litterBoxesHelper: SpiritRef[LitterBoxesHelper.Message] = context.cast(LitterBoxesHelper(), "LitterBoxesHelper")

    val litterTrackingDashboardActor: SpiritRef[LitterTrackingDashboardActor.Message] = context.cast(LitterTrackingDashboardActor(
    //  litterBoxesHelper
    ), "LitterTrackingDashboardActor")

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[Observation]]] = context.cast(DailyNotesRouter(
      "Structured cats notes",
      "structured_cats_notes",
      MessageListJsonProtocol.MessageJsonFormat,
      toMarkdown
    ), "DailyNotesRouter")

    val catNotificationsManager: SpiritRef[CatNotificationsManager.Message] = context.cast(CatNotificationsManager(), "CatNotificationsManager")

    behavior(
      litterBoxesHelper,
      dailyNotesAssistant,
      catNotificationsManager,
      litterTrackingDashboardActor
    )
  }

  private def behavior(
                        litterBoxesHelper: SpiritRef[LitterBoxesHelper.Message],
                        dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[Observation]]],
                        catNotificationsManager: SpiritRef[CatNotificationsManager.Message],
                        litterTrackingDashboardActor: SpiritRef[LitterTrackingDashboardActor.Message]
      )(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context

    message match {
      case observation@PostHocLitterObservation(event@PostHocLitterObservationEvent(when, litterBoxChoice, isClean), _, ref, _) =>
        context.actorContext.log.debug("Updating CatsHelper notes, LitterBoxesHelper and CatNotificationsManager")
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(observation), when.toLocalDate)
        litterBoxesHelper !! LitterBoxesHelper.PostHocLitterObservation(event, ref)
        if (isClean) {
          context.actorContext.log.info(s"Marking $litterBoxChoice as clean")
          catNotificationsManager !! CatNotificationsManager.LitterClean(litterBoxChoice, ref)
        } else {
          context.actorContext.log.info(s"Marking $litterBoxChoice as dirty as of $when")
          catNotificationsManager !! CatNotificationsManager.LitterUsed(when, litterBoxChoice, None, ref)
        }
        Tinker.steadily

      case observation@ObservedCatUsingLitter(event@LitterUsedEvent(when, litterBoxChoice, maybeCat), _, ref, _, _) =>
        maybeCat match {
          case None =>
            context.actorContext.log.info("No cat, need to write that code")
          case Some(_) =>
            dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(observation), when.toLocalDate)
            litterBoxesHelper !! LitterBoxesHelper.ObservedCatUsingLitter(event, ref)
        }

        catNotificationsManager !! CatNotificationsManager.LitterUsed(when, litterBoxChoice, maybeCat, ref)

        Tinker.steadily

      case observation@LitterSifted(event@LitterSiftedEvent(when, litterBoxChoice, _), _, ref, _) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(observation), when.toLocalDate)
        litterBoxesHelper !! LitterBoxesHelper.LitterSifted(event, ref)
        catNotificationsManager !! CatNotificationsManager.LitterClean(litterBoxChoice, ref)
        Tinker.steadily

      case pm@PartialMatch(TranscriptionEvent(RasaAnnotatedNotedTranscription(notedTranscription, maybeRasaResult))) =>
        maybeRasaResult match {
          case Some(rasaResult) =>
            context.actorContext.log.info(s"Forwarding ${notedTranscription.noteId} to the litter dashboard as a partial match")
            if (rasaResult.intent.name == "observe_sifted_contents") {
              litterBoxesHelper !! LitterBoxesHelper.ReceivePartialMatch(pm)
            } else {
              litterTrackingDashboardActor !! LitterTrackingDashboardActor.PartialMatch(notedTranscription, rasaResult)
            }
          case None =>
            context.actorContext.log.info(s"Ignoring ${notedTranscription.noteId} because there was no Rasa for partial matching")
        }
        Tinker.steadily
    }
  }

  private def toMarkdown(messages: List[Observation], clock: TinkerClock): String = {
    // FIXME: full integration experiment to current state
    def simpleLitterString(choice: LitterBoxChoice): String = choice match {
      case FrontLitter => "front"
      case BackLitter => "back"
      case Both => "both"
      case LitterRobot => "litter robot"
    }

    messages.map {
      case m@PostHocLitterObservation(PostHocLitterObservationEvent(_, boxChoice: LitterBoxChoice, isClean), _, _, _) =>
        val prettyCleanOrDirty = if (isClean) {
          "clean ✅"
        } else {
          "dirty ❗️"
        }
        val pretty = boxChoice match {
          case BackLitter => s"The back litter box was $prettyCleanOrDirty"
          case FrontLitter => s"The front litter box was $prettyCleanOrDirty"
          case Both => s"Both litter boxes were $prettyCleanOrDirty"
          case LitterRobot => s"The litter robot was $prettyCleanOrDirty"
        }
        blockFromTopLine(pretty)(m)
      case m@ObservedCatUsingLitter(LitterUsedEvent(_, litterBoxChoice, maybeCat), _, _, _, observationFullyCertain) =>
        val pretty = s"""${maybeCat.getOrElse("(unknown)")} ${if (observationFullyCertain) "" else "(probably)"} used the ${simpleLitterString(litterBoxChoice)}"""
        blockFromTopLine(pretty)(m)
      case m@LitterSifted(LitterSiftedEvent(_, litterBoxChoice, contents), _, _, _) =>
        val pretty = s"Sifted ${simpleLitterString(litterBoxChoice)} and found ${contents.toEmojis}"
        blockFromTopLine(pretty)(m)
    }.mkString("\n") + "\n"
  }

  private def blockFromTopLine(topLine: String)(message: Observation): String = {
    def quote(s: String) = s"""\"$s\""""
    val dateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
    val timestamp = message.when.format(dateTimeFormatter)
    s"""- \\[$timestamp\\] $topLine
       |    - ${quote(message.transcriptionText)} (${message.ref.wikiLinkWithAlias("ref")})
       |    - Rasa confidence = ~${pctFormatter.format(message.intentConfidence * 100)}%""".stripMargin
  }

  private val pctFormatter = new DecimalFormat("#.##")
}
