package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object AppRestartTracker {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("App Restart Tracker", TinkerColor.random(), "🔂") { (context, noteRef) =>
    val time = context.system.clock.now()
    context.actorContext.log.info(s"Updating ${noteRef.noteId} with $time")
    val newLine = s"- $time\n"

    (noteRef.readMarkdownSafer() match {
      case NoteRef.FileDoesNotExist =>
        noteRef.setMarkdown(newLine)

      case NoteRef.Contents(result) =>
        result match {
          case Failure(exception) =>
            context.actorContext.log.warn("Startup tracking failed to read from disk", exception)
            Success(NoOp)

          case Success(existingMarkdown) =>
            noteRef.setMarkdown(newLine + existingMarkdown)
        }
    }) match {
      case Failure(exception) => context.actorContext.log.warn("Startup tracking failed to write to disk", exception)
      case Success(NoOp) =>
    }

    noteRef.readMarkdown().flatMap { markdown =>
      // FIXME: quadratic behavior
      noteRef.setMarkdown(newLine + markdown)
    } match {
      case Failure(exception) => context.actorContext.log.warn("Startup tracking failed", exception)
      case Success(NoOp) =>
    }

    Tinker.ignore
  }
}
