package me.micseydel.actor.kitties

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import me.micseydel.actor.DailyNotesRouter
import me.micseydel.actor.kitties.LitterBoxReportActor.{EventCapture, LitterSiftedObservation}
import me.micseydel.actor.kitties.LitterBoxesHelper.LitterSifted
import me.micseydel.actor.kitties.MarkdownWithoutJsonExperiment.{DataPoint, Report}
import me.micseydel.dsl._
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.model._
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.util.ParseUtil.{batchConsecutiveComments, getLinesAfterHeader, getNoteId, getZonedDateTimeFromListLineFront}
import me.micseydel.vault.{LinkIdJsonProtocol, Note, NoteId}
import spray.json._

import java.io.FileNotFoundException
import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.{Failure, Success}


object LitterBoxReportActor {
  // mailbox
  sealed trait Message

  sealed trait EventCapture extends Message {
    def event: CatObservationEvent
  }

  case class LitterSiftedObservation(capture: LitterBoxesHelper.LitterSifted) extends EventCapture {
    override def event: CatObservationEvent = capture.event
  }

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] =
    setup()

  private def setup()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[EventCapture]] = context.cast(DailyNotesRouter(DailyAbility(_, _, _)), "DailyNotesRouter")

    Tinker.receiveMessage {
      case ec@LitterSiftedObservation(_) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(ec, ec.event.when)
        Tinker.steadily
    }
  }
}

private[kitties] object DailyAbility {
  def apply(forDay: LocalDate, color: TinkerColor, emoji: String)(implicit Tinker: Tinker): (String, Ability[EventCapture]) = {
    val isoDate = TimeUtil.localDateTimeToISO8601Date(forDay)
    val noteName = s"Litter boxes sifting ($isoDate)"
//    val jsonName = s"litter_boxes_sifting_$isoDate"

    noteName -> NoteMakingTinkerer[EventCapture](noteName, color, emoji) { (context, noteRef) =>
        Tinker.receiveMessage {
          case observation@LitterSiftedObservation(capture: LitterSifted) =>
            noteRef.read() match {
              case Success(Note(markdown, frontmatter)) =>
                MarkdownWithoutJsonExperiment(markdown, capture.when.toLocalDate) match {
                  case Validated.Valid(reportFromMarkdownParsing: Report) =>
                    val newNote = Note(reportFromMarkdownParsing.append {
                      capture match {
                        case LitterSifted(LitterSiftedEvent(when, _, contents), ref) =>
                          DataPoint(when, contents, ref)
                      }
                    }.toMarkdown, frontmatter)
                    noteRef.setTo(newNote)
                  case Validated.Invalid(e) =>
                    context.actorContext.log.warn(s"Failed to generate the markdown report because: $e")
                }

              case Failure(_: FileNotFoundException) =>
//                context.actorContext.log.info(s"File $noteRef did not exist, creating for the first time")
//
//                val todaysCaptures = typedJsonRef.appendAndGet(observation) match {
//                  case Failure(exception) => throw exception
//                  case Success(latest) => latest
//                }
//
//                val reportFromExistingJson = Report(todaysCaptures.collect {
//                  case LitterSiftedObservation(LitterSifted(LitterSiftedEvent(when, _, contents), ref)) =>
//                    DataPoint(when, contents, ref)
//                })
//
//                noteRef.setRaw(reportFromExistingJson.toMarkdown)
              context.actorContext.log.warn("File not found, taking no action")

              case Failure(exception) =>
                context.actorContext.log.warn(s"Unexpected failure fetching the note", exception)
            }

            Tinker.steadily
        }
    }
  }
}


case object LitterBoxEventCaptureListJsonProtocol extends DefaultJsonProtocol {

  implicit val linkIdFormat: JsonFormat[NoteId] = LinkIdJsonProtocol.noteIdFormat

  //    implicit val postHocLitterObservationFormat: RootJsonFormat[PostHocLitterObservation] = jsonFormat2(PostHocLitterObservation)
  //    implicit val observedCatUsingLitterFormat: RootJsonFormat[ObservedCatUsingLitter] = jsonFormat2(ObservedCatUsingLitter)

  import LitterBoxesEventCaptureListJsonProtocol.litterSiftedFormat

  implicit val litterSiftedObservationFormat: RootJsonFormat[LitterSiftedObservation] = jsonFormat1(LitterSiftedObservation)

  // copy from CatsHelper.scala
  implicit object ReportEventCaptureJsonFormat extends RootJsonFormat[EventCapture] {
    def write(m: EventCapture): JsValue = {
      val (jsObj, typ) = m match {
        //          case p: PostHocLitterObservation => (p.toJson.asJsObject, "PostHocLitterObservation")
        //          case o: ObservedCatUsingLitter => (o.toJson.asJsObject, "ObservedCatUsingLitter")
        case l: LitterSiftedObservation => (l.toJson.asJsObject, "LitterSiftedObservation")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): EventCapture = {
      value.asJsObject.getFields("type") match {
        //          case Seq(JsString("PostHocLitterObservation")) => value.convertTo[PostHocLitterObservation]
        //          case Seq(JsString("ObservedCatUsingLitter")) => value.convertTo[ObservedCatUsingLitter]
        case Seq(JsString("LitterSiftedObservation")) => value.convertTo[LitterSiftedObservation]
        case other => throw new DeserializationException(s"Unknown type: $other")
      }
    }
  }

  //
  val messageListFormat: RootJsonFormat[List[EventCapture]] = listFormat(ReportEventCaptureJsonFormat)
}

private object MarkdownWithoutJsonExperiment {
  def apply(markdown: String, day: LocalDate): ValidatedNel[String, Report] = {
    val linesToParse: List[String] = getLinesAfterHeader(markdown, "Events")

    @tailrec
    def parseLines(linesToParse: List[String], accumulator: List[LineParseResult]): List[LineParseResult] = {
      linesToParse match {
        case Nil =>
          accumulator.reverse

        case line :: theRest =>
          val (comments, remaining) = batchConsecutiveComments(theRest)
          LineParser.apply(line, day) match {
            case ParseSuccessDatapoint(datapoint) =>
              parseLines(remaining, ParseSuccessDatapoint(datapoint.copy(comments = comments)) :: accumulator)
            case pf@ParseFailure(_, _, existingComments) =>
              if (existingComments.nonEmpty) {
                throw new RuntimeException("Comment should have been empty but found: comments")
              }

              parseLines(remaining, pf.copy(comments = comments) :: accumulator)
          }
      }
    }

    val lineParseResults: List[LineParseResult] = parseLines(linesToParse, Nil)

    val (failures, successes) = lineParseResults.partitionMap {
      case ParseFailure(rawLine, reasons, comments) =>
        val joinedReasons = reasons.toList.mkString(",")
        val msg = s"""Found ${reasons.size} failures: $joinedReasons; for line with comments:\n- $rawLine\n${comments.mkString("\n")}"""
        Left(msg)
      case ParseSuccessDatapoint(datapoint) =>
        Right(datapoint)
    }

    if (failures.nonEmpty) {
      Validated.Invalid(NonEmptyList("Not all lines were successfully parsed", failures))
    } else {
      val report = Report(successes.sortBy(_.zonedDateTime))
      Validated.Valid(report)
    }
  }


  private sealed trait LineParseResult

  private case class ParseSuccessDatapoint(datapoint: DataPoint) extends LineParseResult

  private case class ParseFailure(rawLine: String, reason: NonEmptyList[String], comments: List[String]) extends LineParseResult

  // yaml, summary bullets (poo/pee count), event bullets (time, type, ref)
  // - \[02:58:14AM\] 💦 ([[Transcription for mobile_audio_capture_20240217-025814.wav|ref]])
  case class DataPoint(zonedDateTime: ZonedDateTime, siftedContents: SiftedContents, noteId: NoteId, comments: List[String] = Nil)

  case class Report(datapoints: List[DataPoint]) {
    def toMarkdown: String = {
      val distinctDatapoints = datapoints.distinct
      val eventsList: String = distinctDatapoints.distinct.map {
        case DataPoint(zonedDateTime, siftedContents, noteId, maybeComments) =>
          MarkdownUtil.listLineWithTimestampAndRef(zonedDateTime, siftedContents.toEmojis, noteId) +
            Some(maybeComments).filter(_.nonEmpty).map(_.mkString("\n", "\n", "")).getOrElse("")
      }.mkString("\n")

      def total(litterUseType: LitterUseType): Int = distinctDatapoints.map(_.siftedContents.multiset.getOrElse(litterUseType, 0)).sum

      val totalPee = total(Urination)
      val totalPoo = total(Defecation)

      s"""# Summary
         |
         |- Total pee: $totalPee
         |- Total poo: $totalPoo
         |
         |# Events
         |
         |$eventsList
         |""".stripMargin
    }

    def append(datapoint: DataPoint): Report = {
      this.copy(datapoints = (datapoint :: datapoints).distinct.sortBy(_.zonedDateTime))
    }
  }

  private object LineParser {
    // FIXME: if I break the formatting, then received messages can't be encoded in markdown...
    // - without risk of duplication
    // - or loss
    // FIXME it's ok to have compromises for this report because, unlike the general case,
    // I know that updates only happen from voice events, and I'm not worried about updating the report
    // while a voice event is pending (though something like a power outage could make this an issue)
    def apply(line: String, day: LocalDate): LineParseResult = {
      // e.g.
      // List(-, \[07:29:22PM\], , ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]))
      val validated = getZonedDateTimeFromListLineFront(line.split(' ').toList, day).andThen {
        case (entryTime, theRest) =>
          getSiftedContents(theRest).andThen {
            // partsByWhitespace.splitAt(partsByWhitespace.length - 3) -> (sC, tR)
            case (siftedContents, theRemainder) =>
              getNoteId(theRemainder).map { noteId =>
                DataPoint(entryTime, siftedContents, noteId)
              }
          }
      }

      validated match {
        case Validated.Valid(datapoint) =>
          ParseSuccessDatapoint(datapoint)
        case Validated.Invalid(reasons) =>
          ParseFailure(line, reasons, Nil)
      }
    }


    private val LitterUseTypeMap: Map[String, LitterUseType] = Map[String, LitterUseType](
      "💩" -> Defecation,
      "💦" -> Urination
    )

    private def getSiftedContents(partsByWhitespace: List[String]): ValidatedNel[String, (SiftedContents, List[String])] = {
      val (shouldBeEmojis, remainderForNoteId) = partsByWhitespace.splitAt(partsByWhitespace.length - 3)

      def extractRelevantEmoji(high: Char, low: Char): Option[LitterUseType] = {
        LitterUseTypeMap.get(new String(Array(high, low)))
      }

      @tailrec
      def helper(remaining: List[Char])(accumulatedUses: List[LitterUseType], accumulatedRejections: List[String]): (List[LitterUseType], List[String]) = {
        remaining match {
          case firstCodePoint :: secondCodePoint :: theRest =>
            extractRelevantEmoji(firstCodePoint, secondCodePoint) match {
              case Some(emoji) =>
                helper(theRest)(emoji :: accumulatedUses, accumulatedRejections)
              case None =>
                helper(theRest)(accumulatedUses, firstCodePoint.toString :: accumulatedRejections)
            }

          case Nil =>
            (accumulatedUses, accumulatedRejections)

          case justOne :: Nil =>
            (accumulatedUses, justOne.toString :: accumulatedRejections)
        }
      }

      val (emojis, rejected) = helper(shouldBeEmojis.flatten)(Nil, Nil)

      if (emojis.nonEmpty) {
        Validated.Valid((SiftedContents(emojis), remainderForNoteId))
      } else {
        val msg = s"Expected characters (emojis!) in ${LitterUseTypeMap.keys.toSet} but got ${rejected.toSet} ($shouldBeEmojis)"
        Validated.Invalid(NonEmptyList.of(msg))
      }
    }
  }
}
