package me.micseydel.dsl.cast.chronicler

import cats.data.{NonEmptyList, Validated}
import me.micseydel.NoOp
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{AutomaticallyIntegrated, NeedsAttention, NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.cast.chronicler.ChroniclerMOCDailyNote.*
import me.micseydel.model.LargeModel
import me.micseydel.util.ParseUtil.{batchConsecutiveComments, getZonedDateTimeFromListLineFrontWithOptionalPrefix}
import me.micseydel.util.{FileSystemUtil, MarkdownUtil, ParseUtil, TimeUtil}
import me.micseydel.vault.NoteId
import org.slf4j.{Logger, Marker}

import java.nio.file.Path
import java.time.{LocalDate, LocalTime, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.{Failure, Success}

object ChroniclerMOCDailyMarkdown {

  def updatedMarkdown(markdown: String, message: PostInitMessage)(implicit log: Logger): String = {
    val document = parse(markdown, message.time.toLocalDate)
    (message match {
      case AddNote(noteEntry) =>
        document.addEntry(noteEntry)
      case ack@ListenerAcknowledgement(_, _, _, _) =>
        document.addAcknowledgement(ack)
    }).toMarkdown
  }

  // model

  private case class Document(all: List[LineParseResult], notesWithoutAcknowledgements: List[LineParseResult]) {
    def addEntry(noteEntry: TranscribedMobileNoteEntry)(implicit log: Logger): Document = {
      val exists = all.exists {
        case ParseSuccessGenericTimedListItem(time, contents, prefix, comments) if contents.endsWith(".wav|ref]])") =>
          // FIXME: hacky
          val noteId = "Transcription for " + contents.dropRight("|ref]])".length).split(" ").last
          noteEntry.ref.id == noteId
        case other => false
      }
      if (exists) {
        this
      } else {
        val (successes, failures) = all.partitionMap {
          case s@ParseSuccessGenericTimedListItem(_, _, _, _) => Left(s)
          case f@ParseFailure(_, _, _) => Right(f)
        }

        val wrapped = wrapNoteEntry(noteEntry)
        val updatedAcks = notesWithoutAcknowledgements.appended(wrapped)
        if (failures.nonEmpty) {
          log.warn(s"Parsing resulted in at least one failure will append ${noteEntry.ref} and not bother with sorting; failures = $failures")
          Document(all.appended(wrapped), updatedAcks)
        } else {
          Document((wrapped :: successes).sortBy(_.time), updatedAcks)
        }
      }
    }

    private def wrapNoteEntry(noteEntry: TranscribedMobileNoteEntry): ParseSuccessGenericTimedListItem = {
      val timeWithoutDate = TimeUtil.zonedDateTimeToTimeWithinDay24Hour(noteEntry.time)
      val segments = noteEntry.ref.heading(s"$LargeModel Segments")

      val formattedWordCount = Some(noteEntry.wordCount)
        .filter(_ > 0)
        .map(c => s" ($c words)")
        .getOrElse("")
      val listItem = s"${segments.wikiLinkWithAlias(timeWithoutDate)}" + formattedWordCount
      val contents = MarkdownUtil.listLineWithRef(listItem, noteEntry.ref).drop(2) // FIXME HACK HACK HACK
      ParseSuccessGenericTimedListItem(noteEntry.time, contents, None, Nil)
    }

    private def getNoteId(contents: String)(implicit log: Logger): Option[NoteId] = {
      ParseUtil.getNoteId(contents.split(' ').takeRight(3).toList) match {
        case Validated.Valid(extractedNoteId: NoteId) => Some(extractedNoteId)
        case Validated.Invalid(e) =>
          log.warn(s"Failed to get noteId from $contents: $e")
          None
      }
    }

    private def contentsHasNoteId(contents: String, noteId: NoteId)(implicit log: Logger): Boolean = {
      getNoteId(contents).contains(noteId)
    }

    private def addComment(existing: List[String], listenerAcknowledgement: ListenerAcknowledgement)(implicit log: Logger): List[String] = {
      val formattedTime = TimeUtil.WithinDayDateTimeFormatter.format(listenerAcknowledgement.time)
      val newComment = s"    - \\[$formattedTime\\] ${listenerAcknowledgement.details}"
      existing.map(string => ParseUtil.extractLocalTimeFromBrackets(string.trim.drop(2)).map(_ -> string)).partitionMap {
        case Validated.Valid(tuple) => Left(tuple)
        case Validated.Invalid(errors) => Right(errors)
      } match {
        case (timed, Nil) =>
          val added = (listenerAcknowledgement.timeOfAck.toLocalTime -> newComment) :: timed
          added.sortBy(_._1).map(_._2).distinct
        case (_, errors) =>
          log.warn(s"Failed to parse some line(s), appending comment without sorting; errors = $errors")
          existing.appended(newComment).distinct
      }
    }

    def addAcknowledgement(listenerAcknowledgement: ListenerAcknowledgement)(implicit log: Logger): Document = {
      all.collectFirst {
        case entry@ParseSuccessGenericTimedListItem(_, contents, _, _) if contentsHasNoteId(contents, listenerAcknowledgement.noteRef) =>
          entry
      } match {
        case None =>
          log.warn(s"Listener acknowledgement for id not in all! Ignoring: $listenerAcknowledgement")
          this
        case Some(ParseSuccessGenericTimedListItem(time, contents, _, comments)) =>
          val updatedEntry = ParseSuccessGenericTimedListItem(
            time,
            contents,
            listenerAcknowledgement.setState.map {
              case ChroniclerMOC.NeedsAttention => Todo
              case ChroniclerMOC.AutomaticallyIntegrated => StruckThrough // FIXME: Completed
            },
            addComment(comments, listenerAcknowledgement)
          )

          val filteredNotesWithoutAcknowledgements = notesWithoutAcknowledgements.filter {
            case ParseSuccessGenericTimedListItem(_, contents, _, _) =>
              // we ONLY filter out when we successfully extract the noteId
              !getNoteId(contents).contains(listenerAcknowledgement.noteRef)
            case ParseFailure(_, _, _) => true
          }
          val updatedAll = all.map {
            case ParseSuccessGenericTimedListItem(_, contents, _, _) if contentsHasNoteId(contents, listenerAcknowledgement.noteRef) =>
              updatedEntry
            case other => other
          }
          Document(updatedAll, filteredNotesWithoutAcknowledgements)
      }
    }

    private def sectionListToMarkdownList(section: List[LineParseResult]): String = {
      section.map {
        case ps@ParseSuccessGenericTimedListItem(_, _, _, _) =>
          ps.toMarkdownBlock
        case ParseFailure(rawLine, _, comments) =>
          (rawLine :: comments).mkString("\n")
      }
    }.mkString("\n")

    def toMarkdown: String = {
      val bySection = List(
        "All" -> all,
        "Notes without acknowledgements" -> notesWithoutAcknowledgements
      )

      bySection.filter(_._2.nonEmpty).map { case (header, contents) =>
        s"""# $header
           |
           |${sectionListToMarkdownList(contents)}
           |""".stripMargin
      }.mkString("\n")
    }
  }

  // parsing

  private sealed trait LineParseResult

  private case class ParseSuccessGenericTimedListItem(time: ZonedDateTime, contents: String, prefix: Option[DataPointState], comments: List[String]) extends LineParseResult {
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

      line + Some(comments.reverse).filter(_.nonEmpty).map(_.mkString("\n", "\n", "")).getOrElse("")
    }
  }

  private def parse(markdown: String, forDate: LocalDate): Document = {
    @tailrec
    def parseLines(linesToParse: List[String], accumulator: List[LineParseResult]): List[LineParseResult] = {
      linesToParse match {
        case Nil =>
          accumulator.reverse

        case line :: theRest =>
          val (comments, remaining) = batchConsecutiveComments(theRest)
          lineToLineParseResult(line, forDate) match {
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

    def linesToDocument(lines: List[String]): Document = {
      val nonEmptyLines = lines.filter(_.nonEmpty)

      @tailrec
      def helper(remainingLines: List[String], accumulator: List[String]): (List[LineParseResult], List[LineParseResult]) = {
        remainingLines match {
          case Nil =>
            (parseLines(accumulator.reverse, Nil), Nil)
          case "# Notes without acknowledgements" :: theRest =>
            (parseLines(accumulator.reverse, Nil), parseLines(theRest, Nil))
          case line :: theRest =>
            helper(theRest, line :: accumulator)
        }
      }

      nonEmptyLines match {
        case Nil =>
          Document(Nil, Nil)
        case "# All" :: theRest =>
          val (all, withoutAck) = helper(theRest, Nil)
          Document(all, withoutAck)

        case _ => Document(parseLines(lines, Nil), Nil)
      }
    }

    val lines = markdown.split('\n').toList
    linesToDocument(lines)
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

  private def lineToLineParseResult(line: String, day: LocalDate): LineParseResult = {
    // \[1:38:03AM\] (contents)
    val validated = getZonedDateTimeFromListLineFrontWithOptionalPrefix(line.split(' ').toList, day).map {
      case (entryTime, contentsSeparatedByWhitespace, maybePrefix) =>
        ParseSuccessGenericTimedListItem(entryTime, contentsSeparatedByWhitespace.mkString(" "), maybePrefix, Nil)
    }

    validated match {
      case Validated.Valid(ps: LineParseResult) =>
        ps
      case Validated.Invalid(reasons: NonEmptyList[String]) =>
        ParseFailure(line, reasons, Nil)
    }
  }

  // FIXME use?
  private def addNote2String(an: AddNote, acksMap: Map[NoteId, Seq[ListenerAcknowledgement]]): String = {
    an match {
      case AddNote(TranscribedMobileNoteEntry(time, noteId, wordCount)) =>
        val segments = noteId.heading(s"$LargeModel Segments")

        // FIXME: this is closer to right!
        val timeWithoutDate = TimeUtil.zonedDateTimeToTimeWithinDay24Hour(time)

        val formattedWordCount = Some(wordCount)
          .filter(_ > 0)
          .map(c => s" ($c words)")
          .getOrElse("")

        val acknowledgements: List[ListenerAcknowledgement] = acksMap.getOrElse(noteId, Seq.empty).toList

        // FIXME: revisit
        val listItem = s"${segments.wikiLinkWithAlias(timeWithoutDate)}" + formattedWordCount

        val allDetails: Seq[String] = acknowledgements.map {
          case ListenerAcknowledgement(_, timeOfAck, details, setState) =>
            MarkdownUtil.listLineWithTimestamp(timeOfAck, details)
        }

        // FIXME actionPrefix would be useful
        MarkdownUtil.listLineWithTimestampAndRef(time, listItem, noteId, beforeTimestamp = actionPrefix(acknowledgements)) + (if (allDetails.nonEmpty) {
          allDetails.mkString("\n    ", "\n    ", "")
        } else {
          ""
        })
    }
  }

  private case class ParseFailure(rawLine: String, reason: NonEmptyList[String], comments: List[String]) extends LineParseResult

  //

  def main(args: Array[String]): Unit = {
    val forDate = LocalDate.now()

    val path = "/Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion/Transcribed mobile notes (2025-10-11).md"
    val markdown = FileSystemUtil.getPathContents(Path.of(path))

    val originalDocument = parse(markdown, forDate)

    implicit val l: Logger = new Logger {

      override def getName: String = ???

      override def isTraceEnabled: Boolean = ???

      override def trace(msg: String): Unit = ???

      override def trace(format: String, arg: Any): Unit = ???

      override def trace(format: String, arg1: Any, arg2: Any): Unit = ???

      override def trace(format: String, arguments: Any*): Unit = ???

      override def trace(msg: String, t: Throwable): Unit = ???

      override def isTraceEnabled(marker: Marker): Boolean = ???

      override def trace(marker: Marker, msg: String): Unit = ???

      override def trace(marker: Marker, format: String, arg: Any): Unit = ???

      override def trace(marker: Marker, format: String, arg1: Any, arg2: Any): Unit = ???

      override def trace(marker: Marker, format: String, argArray: Any*): Unit = ???

      override def trace(marker: Marker, msg: String, t: Throwable): Unit = ???

      override def isDebugEnabled: Boolean = ???

      override def debug(msg: String): Unit = ???

      override def debug(format: String, arg: Any): Unit = ???

      override def debug(format: String, arg1: Any, arg2: Any): Unit = ???

      override def debug(format: String, arguments: Any*): Unit = ???

      override def debug(msg: String, t: Throwable): Unit = ???

      override def isDebugEnabled(marker: Marker): Boolean = ???

      override def debug(marker: Marker, msg: String): Unit = ???

      override def debug(marker: Marker, format: String, arg: Any): Unit = ???

      override def debug(marker: Marker, format: String, arg1: Any, arg2: Any): Unit = ???

      override def debug(marker: Marker, format: String, arguments: Any*): Unit = ???

      override def debug(marker: Marker, msg: String, t: Throwable): Unit = ???

      override def isInfoEnabled: Boolean = ???

      override def info(msg: String): Unit = ???

      override def info(format: String, arg: Any): Unit = ???

      override def info(format: String, arg1: Any, arg2: Any): Unit = ???

      override def info(format: String, arguments: Any*): Unit = ???

      override def info(msg: String, t: Throwable): Unit = ???

      override def isInfoEnabled(marker: Marker): Boolean = ???

      override def info(marker: Marker, msg: String): Unit = ???

      override def info(marker: Marker, format: String, arg: Any): Unit = ???

      override def info(marker: Marker, format: String, arg1: Any, arg2: Any): Unit = ???

      override def info(marker: Marker, format: String, arguments: Any*): Unit = ???

      override def info(marker: Marker, msg: String, t: Throwable): Unit = ???

      override def isWarnEnabled: Boolean = ???

      override def warn(msg: String): Unit = println(s"!!! $msg !!!")

      override def warn(format: String, arg: Any): Unit = ???

      override def warn(format: String, arguments: Any*): Unit = ???

      override def warn(format: String, arg1: Any, arg2: Any): Unit = ???

      override def warn(msg: String, t: Throwable): Unit = ???

      override def isWarnEnabled(marker: Marker): Boolean = ???

      override def warn(marker: Marker, msg: String): Unit = ???

      override def warn(marker: Marker, format: String, arg: Any): Unit = ???

      override def warn(marker: Marker, format: String, arg1: Any, arg2: Any): Unit = ???

      override def warn(marker: Marker, format: String, arguments: Any*): Unit = ???

      override def warn(marker: Marker, msg: String, t: Throwable): Unit = ???

      override def isErrorEnabled: Boolean = ???

      override def error(msg: String): Unit = ???

      override def error(format: String, arg: Any): Unit = ???

      override def error(format: String, arg1: Any, arg2: Any): Unit = ???

      override def error(format: String, arguments: Any*): Unit = ???

      override def error(msg: String, t: Throwable): Unit = ???

      override def isErrorEnabled(marker: Marker): Boolean = ???

      override def error(marker: Marker, msg: String): Unit = ???

      override def error(marker: Marker, format: String, arg: Any): Unit = ???

      override def error(marker: Marker, format: String, arg1: Any, arg2: Any): Unit = ???

      override def error(marker: Marker, format: String, arguments: Any*): Unit = ???

      override def error(marker: Marker, msg: String, t: Throwable): Unit = ???
    }
    val noteId = NoteId("Transcription for mobile_audio_capture_20251010-162620.wav")
    val t = ZonedDateTime.now().withHour(23)
    val ackTime = t.plusSeconds(20)
    val document = originalDocument
      .addEntry(TranscribedMobileNoteEntry(t, noteId, -1))
      .addAcknowledgement(ListenerAcknowledgement(noteId, ackTime, "blah blah", None))
      .addEntry(TranscribedMobileNoteEntry(t, noteId, -1))
      .addAcknowledgement(ListenerAcknowledgement(noteId, ackTime, "blah blah", None))

    println(document)

    println(document.toMarkdown)
  }
}
