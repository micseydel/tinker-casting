package me.micseydel.dsl.cast.chronicler

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import ChroniclerMOC.{AutomaticallyIntegrated, NeedsAttention, NoteState, TranscribedMobileNoteEntry}
import ChroniclerMOCDailyNote._
import me.micseydel.model.LargeModel
import me.micseydel.util.ParseUtil.{batchConsecutiveComments, getLinesAfterHeader, getZonedDateTimeFromListLineFrontWithOptionalPrefix}
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.NoteId

import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec

object ChroniclerMOCDailyMarkdown {

  def extractUnacknowledged(markdown: String, forDate: LocalDate): ValidatedNel[String, List[ParseSuccessGenericTimedListItem]] = {
    val allLinesToParse = getLinesAfterHeader(markdown, "Notes without acknowledgements")

    @tailrec
    def parseLines(linesToParse: List[String], accumulator: List[LineParseResult]): List[LineParseResult] = {
      linesToParse match {
        case Nil =>
          accumulator.reverse

        case line :: theRest =>
          val (comments, remaining) = batchConsecutiveComments(theRest)
          TimeSortedSimpleLineParser.apply(line, forDate) match {
            case ps@ParseSuccessGenericTimedListItem(time, contents, maybePrefix, _) =>
              parseLines(remaining, ps.copy(comments = comments) :: accumulator)

            case pf@ParseFailure(_, _, existingComments) =>
              if (existingComments.nonEmpty) {
                throw new RuntimeException("Comment should have been empty but found: comments")
              }

              parseLines(remaining, pf.copy(comments = comments) :: accumulator)
          }
      }
    }

    val (failures, parseResults) = parseLines(allLinesToParse, Nil).partitionMap {
      case ps@ParseSuccessGenericTimedListItem(_, _, _, _) => Right(ps)
      case pf@ParseFailure(_, _, _) => Left(pf)
    }

    failures match {
      case head :: tail =>
        Validated.invalid(NonEmptyList(head, tail).map {
          case pf@ParseFailure(_, _, _) =>
            pf.toString
        })

      case Nil =>
        Validated.Valid(parseResults)
    }
  }

  @tailrec
  def combineConsecutiveTimestampedDatapoints(datapoints: List[ParseSuccessGenericTimedListItem], accumulator: List[ParseSuccessGenericTimedListItem]): List[ParseSuccessGenericTimedListItem] = {
    datapoints match {
      case Nil =>
        accumulator

      case older :: newer :: tail if older.time == newer.time =>

        val combinedComments: List[String] = (older.comments, newer.comments) match {
          case (Nil, Nil) =>
            Nil

          case (Nil, newComments) =>
            newComments

          case (oldComments, Nil) =>
            "    - *comments on incomplete transcription*" :: oldComments.map("    " + _)

          case (oldComments, newComments) =>
            "    - *comments on incomplete transcription*" :: oldComments.map("    " + _) ::: newComments
        }

        combineConsecutiveTimestampedDatapoints(tail, newer.copy(comments = combinedComments) :: accumulator)

      case head :: tail =>
        combineConsecutiveTimestampedDatapoints(tail, head :: accumulator)
    }
  }

  def apply(list: List[PostInitMessage], parseResults: List[ParseSuccessGenericTimedListItem]): String = {
    val (addNotes: Seq[AddNote], acks: Seq[ListenerAcknowledgement]) = list.partitionMap {
      case an: AddNote => Left(an)
      case la: ListenerAcknowledgement => Right(la)
    }

    val acksMap: Map[NoteId, Seq[ListenerAcknowledgement]] = acks.groupMap(_.noteRef)(identity)

    // FIXME: grab the last thing from this and add to notes without acks?
    val allSectionContents: String = toMarkdownList(addNotes.sortBy(_.time), acksMap)

    val notesWithoutAcksSectionContents = if (parseResults.nonEmpty) {
      parseResults.map(_.toMarkdownBlock).mkString("\n")
    } else {
      val notesWithoutAcks = addNotes.filter(addNote => acksMap.contains(addNote.noteEntry.ref))
      toMarkdownList(notesWithoutAcks, Map.empty)
    }

    s"""# All
       |
       |$allSectionContents
       |
       |# Notes without acknowledgements
       |
       |$notesWithoutAcksSectionContents
       |""".stripMargin
  }

  private def toMarkdownList(addNotes: Seq[AddNote], acksMap: Map[NoteId, Seq[ListenerAcknowledgement]]): String = {
    addNotes.map(addNote2String(_, acksMap)).mkString("\n")
  }

  def addNote2String(an: AddNote, acksMap: Map[NoteId, Seq[ListenerAcknowledgement]]): String = {
    an match {
      case AddNote(TranscribedMobileNoteEntry(time, noteId, wordCount)) =>
        val segments = noteId.heading(s"$LargeModel Segments")

        val timeWithoutDate = TimeUtil.zonedDateTimeToTimeWithinDay24Hour(time)

        val formattedWordCount = Some(wordCount)
          .filter(_ > 0)
          .map(c => s" ($c words)")
          .getOrElse("")

        val acknowledgements: List[ListenerAcknowledgement] = acksMap.getOrElse(noteId, Seq.empty).toList

        val listItem = s"${segments.wikiLinkWithAlias(timeWithoutDate)}" + formattedWordCount

        val allDetails: Seq[String] = acknowledgements.map {
          case ListenerAcknowledgement(_, timeOfAck, details, setState) =>
            MarkdownUtil.listLineWithTimestamp(timeOfAck, details)
        }

        MarkdownUtil.listLineWithTimestampAndRef(time, listItem, noteId, beforeTimestamp = actionPrefix(acknowledgements)) + (if (allDetails.nonEmpty) {
          allDetails.mkString("\n    ", "\n    ", "")
        } else {
          ""
        })
    }
  }

  private def actionPrefix(acknowledgements: List[ListenerAcknowledgement]): Option[String] = {
    val noteState: Option[NoteState] = acknowledgements.flatMap(_.setState) match {
      case Nil =>
        None

      case other =>
        if (other.forall(_ == AutomaticallyIntegrated)) {
          Some(AutomaticallyIntegrated)
        } else {
          Some(NeedsAttention)
        }
    }

    noteState.map {
      case ChroniclerMOC.AutomaticallyIntegrated => "[x] "
      case ChroniclerMOC.NeedsAttention => "[ ] "
    }
  }

  sealed trait LineParseResult

  case class ParseSuccessGenericTimedListItem(time: ZonedDateTime, contents: String, prefix: Option[DataPointState], comments: List[String]) extends LineParseResult {
    def toMarkdownBlock: String = {
      val line = prefix.map {
        case Todo =>
          val beforeTimestamp = Some(Todo.prefix)
          MarkdownUtil.listLineWithTimestamp(time, contents, beforeTimestamp = beforeTimestamp)
        case Completed =>
          val beforeTimestamp = Some(Completed.prefix)
          MarkdownUtil.listLineWithTimestamp(time, contents, beforeTimestamp = beforeTimestamp)
        case StruckThrough =>
          MarkdownUtil.listLineWithTimestamp(time, s"~~$contents~~")
      }.getOrElse {
        MarkdownUtil.listLineWithTimestamp(time, contents)
      }

      line + Some(comments).filter(_.nonEmpty).map(_.mkString("\n", "\n", "")).getOrElse("")
    }
  }

  private case class ParseFailure(rawLine: String, reason: NonEmptyList[String], comments: List[String]) extends LineParseResult

  private object TimeSortedSimpleLineParser {
    def apply(line: String, day: LocalDate): LineParseResult = {
      // \[1:38:03AM\] (contents)
      val validated = getZonedDateTimeFromListLineFrontWithOptionalPrefix(line.split(' ').toList, day).map {
        case (entryTime, contentsSeparatedByWhitespace, maybePrefix) =>
          ParseSuccessGenericTimedListItem(entryTime, contentsSeparatedByWhitespace.mkString(" "), maybePrefix, Nil)
      }

      validated match {
        case Validated.Valid(ps) =>
          ps
        case Validated.Invalid(reasons) =>
          ParseFailure(line, reasons, Nil)
      }
    }
  }
}
