package me.micseydel.actor.tasks

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object RecurringResponsibilityManager {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Recurring responsibilities (MOC)", TinkerColor.random(), "⛑️") { (context, noteRef) =>
    noteRef.readListOfWikiLinks() match {
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Something went wrong setting up ${noteRef.noteId}: $e")
        Tinker.ignore
      case Validated.Valid(recurringResponsibilities: NonEmptyList[String]) =>
        val specificResponsibilityTrackers = recurringResponsibilities.map { recurringResponsibility =>
          context.actorContext.log.info(s"Casting $recurringResponsibility")
          context.cast(RecurringResponsibilityActor(recurringResponsibility), Common.tryToCleanForActorName(recurringResponsibility))
        }
        Tinker.ignore
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readListOfWikiLinks(): ValidatedNel[String, NonEmptyList[String]] = {
      noteRef.readMarkdown().map(MarkdownUtil.readListOfWikiLinks) match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(result) =>
          result
      }
    }
  }
}
