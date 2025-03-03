package me.micseydel.actor

import me.micseydel.actor.DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown
import me.micseydel.actor.FrustrationListener.{Event, TranscriptionEvent}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.NeedsAttention
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
import me.micseydel.model._
import me.micseydel.util.{MarkdownUtil, StringUtil}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.annotation.tailrec

object FrustrationListener {
  // mailbox
  sealed trait Message

  sealed trait Event extends Message {
    def when: ZonedDateTime
  }

  case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Event {
    def when: ZonedDateTime = notedTranscription.capture.captureTime
  }

  // outgoing

  final case class DistressDetected(notedTranscription: NotedTranscription, specifics: List[DistressDetection])

  final case class DistressDetection(shortText: String, blockId: String)

  // behavior

  def apply(subscriber: SpiritRef[DistressDetected])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(255, 0, 0), "😬").setup { context =>
    implicit val c: TinkerContext[_] = context
    context.system.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(TranscriptionEvent))

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[Event]]] = context.cast(DailyNotesRouter(
      "Frustrated notes",
      "frustrated_notes",
      MessageListJsonProtocol.EventCaptureJsonFormat,
      FrustrationListenerMarkdownModel.toMarkdown
    ), "DailyNotesRouter")

    behavior(
      dailyNotesAssistant,
      subscriber,
      Set.empty
    )
  }

  private def behavior(
                        dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[Event]]],
                        subscriber: SpiritRef[DistressDetected],
                        alreadySeenTranscriptions: Set[(WhisperModel, ZonedDateTime)]
                      )(implicit Tinker: Tinker): Ability[Message] = Tinker.receive[Message] { (context, message) =>
    context.actorContext.log.debug(s"Received message of type ${message.getClass}")
    implicit val tinkerContext: TinkerContext[_] = context

    message match {
      case event@TranscriptionEvent(transcription@NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent(_, segments), WhisperResultMetadata(model, _, vaultPath, _)), captureTime), noteId)) =>
        val key = (model, captureTime)

        if (alreadySeenTranscriptions.contains(key)) {
          context.actorContext.log.debug(s"Already processed $key, ignoring")
          Tinker.steadily
        } else {
          context.actorContext.log.info(s"Processing key $key; updating: messages stored as JSON, Markdown, ")

          if (containsFrustration(transcription)) {
            context.actorContext.log.info(s"Detected frustration in $vaultPath")
            model match {
              case BaseModel =>
                context.system.chronicler !! Chronicler.ListenerAcknowledgement(noteId, context.system.clock.now(), "(fast) Detected frustration", Some(NeedsAttention))
              case LargeModel =>
                context.system.chronicler !! Chronicler.ListenerAcknowledgement(noteId, context.system.clock.now(), "(accurate) Detected frustration", Some(NeedsAttention))
            }

            context.actorContext.log.info("Sending to daily notes router")
            dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(event), captureTime.toLocalDate)

            val detections: List[DistressDetection] = segments.zipWithIndex.collect {
              case (WhisperSegment(start, text), index) if containsFrustration(text) =>
                // start is float, wtf?
                DistressDetection(text, s"seg-$model-$index")
            }

            context.actorContext.log.info(s"Notifying subscriber ${subscriber.path} of ${detections.size} detections")
            subscriber !! DistressDetected(transcription, detections)

            behavior(
              dailyNotesAssistant,
              subscriber,
              alreadySeenTranscriptions + (model -> captureTime)
            )
          } else {
            context.actorContext.log.debug(s"No frustration for $key")
            Tinker.steadily
          }
        }
    }
  }

  private def containsFrustration(text: String): Boolean = {
    (
      text.contains("distress")
        || text.contains("fucking")
        || text.contains("shit")
        || text.contains("frustrat")
        || text.contains("anxi") // anxiety, anxious
        || text.contains("hypervig")
        || text.contains("stressed")
        || text.contains("overwhelmed")
      )
  }

  private def containsFrustration(transcription: NotedTranscription): Boolean = {
    containsFrustration(transcription.capture.whisperResult.whisperResultContent.text.toLowerCase)
  }

  object MessageListJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat

    implicit val transcriptionEventFormat: RootJsonFormat[TranscriptionEvent] = jsonFormat1(TranscriptionEvent)

    // copy from CatsHelper.scala
    implicit object EventCaptureJsonFormat extends RootJsonFormat[Event] {
      def write(m: Event): JsValue = {
        val (jsObj, typ) = m match {
          case l: TranscriptionEvent => (l.toJson.asJsObject, "TranscriptionEvent")
        }
        JsObject(jsObj.fields + ("type" -> JsString(typ)))
      }

      def read(value: JsValue): Event = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("TranscriptionEvent")) => value.convertTo[TranscriptionEvent]
          case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(_)) for one of {TranscriptionEvent, ReceiveAranetResult} but got $other")
        }
      }
    }

    val messageListFormat: RootJsonFormat[List[Event]] = listFormat(EventCaptureJsonFormat)
  }
}

object FrustrationListenerMarkdownModel {
  def toMarkdown(messages: List[Event], clock: TinkerClock): String = {
    toMarkdown(messages, Nil).reverseIterator.mkString("\n") + "\n"
  }

  @tailrec
  private def toMarkdown(messages: List[Event], result: List[String]): List[String] = {
    messages match {
      case Nil =>
        result

      case TranscriptionEvent(NotedTranscription(capture, noteId)) :: theRest =>
        toMarkdown(
          theRest,
          MarkdownUtil.listLineWithTimestampAndRef(
            capture.captureTime,
            StringUtil.truncateText(capture.whisperResult.whisperResultContent.text),
            noteId
          ) :: result)
    }
  }
}
