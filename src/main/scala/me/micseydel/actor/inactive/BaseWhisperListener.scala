package me.micseydel.actor.inactive

import me.micseydel.model._
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.util.{StringUtil, TimeUtil}
import me.micseydel.vault.NoteId
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime

object BaseWhisperListener {
  sealed trait Message

  private case class VaultAccessibilityError(when: ZonedDateTime, exception: Throwable) extends Message

  case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message {
    def when: ZonedDateTime = notedTranscription.capture.captureTime

    def model: WhisperModel = notedTranscription.capture.whisperResult.whisperResultMetadata.model
  }

//  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
//    val baseNoteName: String = "Base Whisper transcriptions notes"
//    val config: PersistenceWithDailyMarkdownAndJson.Config[TranscriptionEvent] = PersistenceWithDailyMarkdownAndJson.Config(
//      baseNoteName,
//      _.when,
//      toMarkdown,
//      item => s"${item.when}|${item.model}"
//    )
//
//    implicit val sender: Sender = context.sender
//    val moc = context.cast(
//      DailyNoteKeeperWithJsonPersistence[TranscriptionEvent](
//        config,
//        context.(VaultAccessibilityError.tupled, MessageListJsonProtocol.messageListJsonFormat)
//      ),
//      "BaseWhisper_MOC"
//    )
//
//    context.system.gossiper ! Gossiper.SubscribeHybrid(context.messageAdapter(TranscriptionEvent))
//
//    behavior(moc)
//  }
//
//  private def behavior(moc: SpiritRef[DailyNoteKeeperWithJsonPersistence.Message[TranscriptionEvent]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
//    implicit val sender: Sender = context.sender
//    message match {
//      case VaultAccessibilityError(when, exception) =>
//        context.log.error(s"VaultAccessibility error at $when", exception)
//        Tinker.done
//
//      case event@TranscriptionEvent(NotedTranscription(capture, _, _)) =>
//        context.log.info(s"Adding ${capture.captureTime} to MOC for model ${capture.whisperResult.whisperResultMetadata.model}")
//        moc !! DailyNoteKeeperWithJsonPersistence.AddItem(event)
//        Tinker.steadily
//    }
//  }

  sealed trait WhisperAggregationResult {
    def captureTime: ZonedDateTime
  }

  private case class CompletePair(captureTime: ZonedDateTime, base: HalfOfPair, large: HalfOfPair) extends WhisperAggregationResult

  private case class Partial(text: String, model: WhisperModel, performedOn: String, perfCounterElapsed: Double, captureTime: ZonedDateTime) extends WhisperAggregationResult

  private case class HalfOfPair(text: String, perfCounterElapsed: Double)

  private object HalfOfPair {
    def apply(notedTranscription: NotedTranscription): HalfOfPair = {
      HalfOfPair(
        notedTranscription.capture.whisperResult.whisperResultContent.text,
        notedTranscription.capture.whisperResult.whisperResultMetadata.perfCounterElapsed
      )
    }
  }

  private object WhisperAggregationResult {
    def apply(notedTranscription: NotedTranscription): WhisperAggregationResult = {
      notedTranscription match {
        case NotedTranscription(TranscriptionCapture(WhisperResult(whisperResultContent, WhisperResultMetadata(model, performedOn, _, perfCounterElapsed)), captureTime), _) =>
          Partial(whisperResultContent.text, model, performedOn, perfCounterElapsed, captureTime)
      }
    }

    def apply(first: NotedTranscription, second: NotedTranscription): Either[String, WhisperAggregationResult] = {
      if (first.noteId != second.noteId) {
        Left(s"This is intended to aggregate NotedTranscriptions with the same NoteId but different captureTimes, but found ${first.noteId} and ${second.noteId}")
      } else {
        val sharedNoteId = first.noteId
        if (first.capture.captureTime != second.capture.captureTime) {
          Left(s"NotedTranscriptions for note $sharedNoteId had mis-matched capture times: ${first.capture.captureTime} and ${second.capture.captureTime}")
        } else {
          val sharedCaptureTime = first.capture.captureTime
          (first.capture.whisperResult.whisperResultMetadata.model, second.capture.whisperResult.whisperResultMetadata.model) match {
            case (BaseModel, LargeModel) =>
              Right(CompletePair(sharedCaptureTime, HalfOfPair(first), HalfOfPair(second)))
            case (LargeModel, BaseModel) =>
              Right(CompletePair(sharedCaptureTime, HalfOfPair(second), HalfOfPair(first)))
            case other =>
              Left(s"Expected {BaseModel, LargeModel} but got $other")
          }
        }
      }
    }
  }

//  private def toMarkdown(messages: List[BaseWhisperListener.Message]): String = {
//    val simpleAggregation: Map[NoteId, List[NotedTranscription]] = messages.collect {
//      case e@TranscriptionEvent(_) => e
//    }.groupMap {
//      case TranscriptionEvent(notedTranscription) =>
//        notedTranscription.noteId
//    } {
//      case TranscriptionEvent(notedTranscription) =>
//        notedTranscription
//    }
//
//    //    println(simpleAggregation)
//
//    val filteredAggregation: Map[NoteId, WhisperAggregationResult] = simpleAggregation.flatMap {
//      case (noteId, List(first, second)) =>
//        //        print(s"Detected $noteId as having two!")
//        WhisperAggregationResult(first, second) match {
//          case Right(result) =>
//            Some(noteId -> result)
//          case Left(msg) =>
//            //            println(msg)
//            //            log.warn(msg)
//            None
//        }
//      case (noteId, List(one)) =>
//        Some(noteId -> WhisperAggregationResult(one))
//      case (noteId, other) =>
//        //        log.warn(s"Expected a list of two NotedTranscripts with models {LargeModel, BaseModel} (or just one and waiting on the other) but $noteId collected to $other")
//        None
//    }
//
//    val blocks = filteredAggregation.toList.sortBy(_._2.captureTime)
//      .map {
//        case (key, CompletePair(captureTime, HalfOfPair(baseText, basePerfCounterElapsed), HalfOfPair(largeText, largePerfCounterElapsed))) =>
//          def line(s: String, elapsed: Double, m: WhisperModel): String = {
//            s"""${StringUtil.truncateText(s)} (${key.heading(s"$m Start").wikiLinkWithAlias(f"$m took $elapsed%.1fs")})"""
//          }
//
//          val timestamp = captureTime.format(TimeUtil.WithinDayDateTimeFormatter)
//          (s"- \\[$timestamp\\] ${key.asString}" + "\n" +
//            s"    - ${line(baseText, basePerfCounterElapsed, BaseModel)}" + "\n" +
//            s"    - ${line(largeText, largePerfCounterElapsed, LargeModel)}")
//
//        case (key, Partial(text, model, performedOn, perfCounterElapsed, captureTime)) =>
//          val modelTook = f"took $perfCounterElapsed%.1fs on $performedOn"
//          val firstLine = DailyNoteKeeperWithJsonPersistence.listLineWithTimestampAndRef(
//            captureTime,
//            s"Expected a pair but only got $model ($modelTook)",
//            key
//          )
//
//          val secondLine = if (text.wordCount < 30) {
//            text
//          } else {
//            StringUtil.truncateText(text)
//          }
//
//          firstLine +
//            "\n" +
//            s"    - $secondLine"
//      }
//
//
//    blocks.mkString("\n") + "\n"
//  }

  private object MessageListJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat

    implicit val transcriptionEventFormat: JsonFormat[TranscriptionEvent] = jsonFormat1(TranscriptionEvent)

    implicit object MessageJsonFormat extends RootJsonFormat[TranscriptionEvent] {
      def write(m: TranscriptionEvent): JsValue = {
        val (jsObj, typ) = m match {
          case l: TranscriptionEvent => (l.toJson.asJsObject, "TranscriptionEvent")
        }
        JsObject(jsObj.fields + ("type" -> JsString(typ)))
      }

      def read(value: JsValue): TranscriptionEvent = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("TranscriptionEvent")) => value.convertTo[TranscriptionEvent]
          case other => throw DeserializationException(s"Unknown type, expected ${Seq(JsString("TranscriptionEvent"))} but got $other")
        }
      }
    }

    implicit val messageListJsonFormat: RootJsonFormat[List[TranscriptionEvent]] = listFormat(MessageJsonFormat)
  }
}
