package me.micseydel.actor.kitties.kibble

import me.micseydel.NoOp
import me.micseydel.actor.kitties.kibble.KibbleModel.{Circular1, Circular2, KibbleContainer, RectangularL, RectangularS}
import me.micseydel.dsl.{Tinker, TinkerClock, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.Gossiper.Vote
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.model.NotedTranscription
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import scala.annotation.{tailrec, unused}
import scala.util.{Failure, Success}

object KibbleManagerActor {
  sealed trait Message

  final case class ReceiveVote(vote: Vote) extends Message

  private[kitties] final case class MaybeHeardKibbleMention(notedTranscription: NotedTranscription) extends Message

  sealed trait Event extends Message

  private[kitties] sealed trait KibbleContainerMeasurement extends Event {
    def container: KibbleContainer

    def massGrams: Int
  }

  private[kitties] case class KibbleRefill(container: KibbleContainer, massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends KibbleContainerMeasurement

  private[kitties] case class RemainingKibbleMeasure(container: KibbleContainer, massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends KibbleContainerMeasurement

  private[kitties] case class KibbleDiscarded(massGrams: Int, time: ZonedDateTime, noteId: NoteId) extends Event

  private val NoteName = "Kibble Tinkering 2.0"

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message](NoteName, CatBrown, "🍚") { (context, noteRef) =>
    @unused // subscribes via Gossiper
    val listener = context.cast(KibbleManagerListenerActor(context.self), "KibbleManagerListenerActor")

    val containers: List[(KibbleContainer, Int)] = noteRef.getContainersOrMaybeThrow()(context.actorContext.log)

    context.actorContext.log.info(s"Starting with containers: $containers")

    implicit val nr: NoteRef = noteRef
    behavior(containers.toMap)
  }

  private def behavior(cachedContainers: Map[KibbleContainer, Int])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerClock = context.system.clock
    implicit val c: TinkerContext[_] = context

    Tinker.receiveMessage {
      case MaybeHeardKibbleMention(notedTranscription) =>
        context.actorContext.log.debug(s"Received ${notedTranscription.noteId}")
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(notedTranscription.capture.captureTime, notedTranscription.capture.whisperResult.whisperResultContent.text, notedTranscription.noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)
        context.system.gossiper !! notedTranscription.noteId.voteConfidently(None, context.messageAdapter(ReceiveVote), Some("Definitely maybe"))
        Tinker.steadily

      case KibbleRefill(container, mass, time, noteId) =>
        val text = s"Refilled $container to ${mass}g"
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)
        noteRef.setContainerOrThrow(container, mass)(context.actorContext.log)

        if (mass < 100 || mass > 500) {
          // uncertain
          context.system.gossiper !! noteId.voteConfidently(None, context.messageAdapter(ReceiveVote), Some("Mass out of range; mis-transcription?"))
          Tinker.steadily
        } else {
          context.system.gossiper !! noteId.voteConfidently(Some(true), context.messageAdapter(ReceiveVote), Some("perfect fit"))
          behavior(cachedContainers.updated(container, mass))
        }

      case RemainingKibbleMeasure(container, mass, time, noteId) =>
        val text = cachedContainers.get(container) match {
          case None =>
            s"Measured $container at ${mass}g"
          case Some(cachedMass) =>
            if (cachedMass > mass) {
              s"Measured $container at ${mass}g, previously ${cachedMass}g therefor dispensed ${cachedMass - mass}g"
            } else {
              s"Measured $container at ${mass}g, previously ${cachedMass}g"
            }
        }
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)

        if (mass < container.baselineWeight || mass > 1000) {
          // uncertain
          context.system.gossiper !! noteId.voteConfidently(None, context.messageAdapter(ReceiveVote), Some("Almost matched; mis-transcription?"))
          Tinker.steadily
        } else {
          noteRef.setContainerOrThrow(container, mass)(context.actorContext.log)
          context.system.gossiper !! noteId.voteConfidently(Some(true), context.messageAdapter(ReceiveVote), Some("perfect fit"))
          behavior(cachedContainers.updated(container, mass))
        }

      case KibbleDiscarded(mass, time, noteId) =>
        val text = s"Discarded ${mass}g kibble"
        val lineToAdd = MarkdownUtil.listLineWithTimestampAndRef(time, text, noteId, dateTimeFormatter = TimeUtil.MonthDayTimeFormatter)
        noteRef.addOrThrow(lineToAdd)(context.actorContext.log)

        if (mass < 0 || mass > 150) {
          // uncertain
          context.system.gossiper !! noteId.voteConfidently(None, context.messageAdapter(ReceiveVote), Some("Mass out of range; mis-transcription?"))
        } else {
          context.system.gossiper !! noteId.voteConfidently(Some(true), context.messageAdapter(ReceiveVote), Some("perfect fit"))
        }

        Tinker.steadily

      case ReceiveVote(vote) =>
        // FIXME: this will be chatty!
        context.actorContext.log.debug(s"Ignoring $vote")
        Tinker.steadily
    }
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def addOrThrow(line: String)(log: Logger): Unit = {
      noteRef
        .readMarkdown()
        .recoverWith {
          case _: FileNotFoundException => Success("")
        }
        .map(addToMarkdown(_, line)(log))
        .flatMap(noteRef.setMarkdown) match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }
    }

    def setContainerOrThrow(container: KibbleContainer, grams: Int)(log: Logger): Unit = {
      noteRef.readMarkdown().map(_.split("\n")).flatMap { lines =>
        val lookingForStart = s"- [[${container.noteName}"
        val newLines = lines.map {
          case line if line.startsWith(lookingForStart) =>
            s"- [[${container.noteName}]] ${grams}g"
          case line => line
        }

        noteRef.setMarkdown(newLines.mkString("\n"))
      } match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }
    }

    def getContainersOrMaybeThrow()(log: Logger): List[(KibbleContainer, Int)] = noteRef.readMarkdown().map(MarkdownUtil.getListFromHeader(_, "## Containers")) match {
      case Failure(exception) => throw exception
      case Success(list) =>
        list.flatMap { listLine =>
          if (listLine.startsWith("[[")) {
            listLine.drop(2).split("]] ").toList match {
              case List(containerWikiLink, rawMassGrams) =>
                val massGrams: Option[Int] = if (rawMassGrams.endsWith("g")) {
                  val massString = rawMassGrams.dropRight(1)
                  massString.toIntOption match {
                    case Some(mass) => Some(mass)
                    case None =>
                      log.warn(s"Failed to convert `$massString` to an integer")
                      None
                  }
                } else {
                  log.warn(s"""Expected mass in grams, e.g. "451g" but got: $rawMassGrams""")
                  None
                }

                // FIXME: replace these case classes/objects with Markdown/object-note based stuff
                val container = containerWikiLink match {
                  case Circular1.noteName => Some(Circular1)
                  case Circular2.noteName => Some(Circular2)
                  case RectangularS.noteName => Some(RectangularS)
                  case RectangularL.noteName => Some(RectangularL)
                  case other =>
                    log.warn(s"Unrecognized container $other, expected one of: {${Circular1.noteName}, ${Circular2.noteName}, ${RectangularS.noteName}, RectangularL.noteName}")
                    None
                }

                container.flatMap(c => massGrams.map(c -> _))

              case _ =>
                log.warn(s"List line did not contain a wikilink as expected: $listLine")
                None
            }
          } else {
            log.warn(s"List line did not start with a wikilink as expected: $listLine")
            None
          }
        }
    }
  }

  private[kitties] def addToMarkdown(original: String, formattedLineToAdd: String)(implicit log: Logger): String = {
    if (original.isEmpty) {
      s"""# Summary
         |
         |## Containers
         |
         |- [[Primary Circular Plastic Food Container]] 1g
         |- [[Secondary Circular Plastic Food Container]] 2g
         |- [[Small Rectangular Plastic Food Container]] 3g
         |- [[Large Rectangular Plastic Food Container]] 4g
         |
         |# Inbox
         |
         |$formattedLineToAdd
         |
         |# History
         |
         |$formattedLineToAdd
         |""".stripMargin
    } else {
      val allLines = original.split("\n").toList

      @tailrec
      def findInbox(lines: List[String], accumulator: List[String]): (List[String], List[String]) = {
        lines match {
          case "# Inbox" :: tail => (accumulator, tail)
          case notYetInbox :: tail => findInbox(tail, notYetInbox :: accumulator)
          case Nil => (accumulator, Nil)
        }
      }

      val (notTouchingReversed, whenTailStarts) = findInbox(allLines, Nil)

      @tailrec
      def findMarkdownListEnded(lines: List[String], accumulator: List[String], seenListStart: Boolean = false): (List[String], List[String]) = {
        lines match {
          case head :: tail if head.startsWith("- ") => findMarkdownListEnded(tail, head :: accumulator, seenListStart = true)
          case "" :: tail if seenListStart => (accumulator, tail) // an empty line indicates the list has ended
          case head :: tail => findMarkdownListEnded(tail, head :: accumulator, seenListStart) // tolerates nested lists
          case Nil => (accumulator, Nil)
        }
      }

      val (completedSofar, theRest) = findMarkdownListEnded(whenTailStarts, "# Inbox" :: notTouchingReversed)

      val (shouldBeFinishedReverse, shouldBeEmpty) = findMarkdownListEnded(theRest, "" :: formattedLineToAdd :: completedSofar)

      val result = ("" :: formattedLineToAdd :: shouldBeFinishedReverse).reverse.mkString("\n")

      if (shouldBeEmpty.nonEmpty) {
        log.warn(s"Should have been empty: $shouldBeEmpty; result = $result")
      }

      result
    }
  }
}
