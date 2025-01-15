package me.micseydel.actor

import me.micseydel.dsl.{Tinker, TinkerClock}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.vault.persistence.{JsonlRefT, NoteRef}
import spray.json.JsonFormat

import scala.util.{Failure, Success}

object DailyMarkdownFromPersistedMessagesActor {
  sealed trait Message[+Wrapped] {
//    def wrapped: Wrapped
  }

  final case class StoreAndRegenerateMarkdown[Wrapped](wrapped: Wrapped) extends Message[Wrapped]

  final case class RegenerateMarkdown[Wrapped]() extends Message[Wrapped]

  /**
   * Persistence via JSON.
   */
  def apply[M](noteName: String, jsonName: String, jsonFormat: JsonFormat[M], messagesToMarkdown: (List[M], TinkerClock) => String)(implicit Tinker: Tinker): Ability[Message[M]] =
    Tinker.initializedWithNoteAndPersistedMessages(noteName, jsonName, jsonFormat) { (context, noteRef, typedJsonRef) =>
      context.actorContext.log.info(s"Starting for noteName $noteName and jsonName $jsonName")
      behavior(noteRef, typedJsonRef, messagesToMarkdown)
    }

  private def behavior[M](noteRef: NoteRef, jsonlRef: JsonlRefT[M], messagesToMarkdown: (List[M], TinkerClock) => String)(implicit Tinker: Tinker): Ability[Message[M]] = Tinker.receive { (context, message) =>
    context.actorContext.log.debug(s"Received message wrapping type $message")
    message match {
      case StoreAndRegenerateMarkdown(wrapped) =>
        jsonlRef.appendAndGet(wrapped) match {
          case Failure(exception) =>
            throw new RuntimeException(s"appendAndGet failed for $jsonlRef", exception)
          case Success(latest) =>
            val markdown = messagesToMarkdown(latest, context.system.clock)
            noteRef.setMarkdown(markdown) match {
              case Failure(exception) => throw exception
              case Success(_) =>
                context.actorContext.log.debug(s"${noteRef.noteId} generated markdown: $markdown")
            }
        }


        Tinker.steadily

      case RegenerateMarkdown() =>
        jsonlRef.get() match {
          case Failure(exception) =>
            throw new RuntimeException(s"appendAndGet failed for $jsonlRef", exception)
          case Success(latest) =>

            noteRef.setMarkdown(messagesToMarkdown(latest, context.system.clock)) match {
              case Failure(exception) => throw exception
              case Success(_) =>
            }
        }

        Tinker.steadily
    }
  }
}
