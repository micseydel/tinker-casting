package me.micseydel.actor.ollama

import me.micseydel.actor.ollama.OllamaModel.{ChatResponse, ChatResponseFailure, ChatResponseResult}
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object PromptFromFileManagerActor {
  sealed trait Message

  private case class ReceiveNoteContents(note: Note) extends Message

  private case class ReceiveException(exception: Throwable) extends Message

  private case class ReceivePromptResponse(response: ChatResponse) extends Message

  def apply(hostAndPort:String, filename: String)(implicit Tinker: Tinker): Ability[Message] = {
    val noteName = dropDotMdFromEndOfFileNameIfPresent(filename) // FIXME: hack for .md suffix becoming duplicated
    NoteMakingTinkerer[Message](noteName, TinkerColor.random(), "🥼", Some(OllamaActor.OllamaPromptsSubdirectory)) { (context, noteRef) =>
      context.actorContext.log.info(s"Initialized prompt manager for $filename, doing an async file read...")

      implicit val ec: ExecutionContextExecutor = context.system.actorSystem.executionContext
      context.pipeToSelf(noteRef.readNoteAsync()) {
        case Failure(exception) => ReceiveException(exception)
        case Success(note) => ReceiveNoteContents(note)
      }

      initializing(hostAndPort, noteRef)
    }
  }

  private def initializing(hostAndPort: String, noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveNoteContents(note) =>
        val model = note.yamlFrontMatter.map(_.get("model")) match {
          case Failure(exception) => throw exception
          case Success(Some(model: String)) => model
          case Success(other) =>
            context.actorContext.log.debug(s"$other not expected, expected Some(string), defaulting to llama3")
            "llama3"
        }

        context.actorContext.log.info("Spawning an anonymous FetchChatResponse actor")
        context.castAnonymous(FetchChatResponseActor(hostAndPort, note.markdown, model, context.messageAdapter(ReceivePromptResponse)))

        initialized(noteRef, model)

      case ReceiveException(exception) =>
        context.actorContext.log.error("Failed to access disk", exception)
        Tinker.steadily

      case ReceivePromptResponse(wtf) =>
        context.actorContext.log.error(s"Did not expect a prompt response yet: $wtf")
        Tinker.steadily
    }
  }

  private def initialized(noteRef: NoteRef, model: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveNoteContents(_) =>
        context.actorContext.log.warn(s"Ignoring note, already initialized with model $model")
        Tinker.steadily

      case ReceiveException(exception) =>
        context.actorContext.log.error("Failed to access disk", exception)
        Tinker.steadily

      case ReceivePromptResponse(ChatResponseResult(text, _)) =>
        context.actorContext.log.info(s"Received prompt reply!")
        noteRef.append(s"\n# Ollama response (per $model)\n\n$text\n")
        Tinker.steadily

      case ReceivePromptResponse(ChatResponseFailure(msg, maybeException)) =>
        maybeException match {
          case Some(throwable) =>
            context.actorContext.log.error(msg, throwable)

          case None =>
            context.actorContext.log.warn(s"Something went wrong: $msg")
        }

        Tinker.steadily
    }
  }

  private def dropDotMdFromEndOfFileNameIfPresent(string: String): String = {
    if (string.endsWith(".md")) {
      string.dropRight(3)
    } else {
      string
    }
  }
}
