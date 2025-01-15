package me.micseydel.actor.ollama

import me.micseydel.actor.{ActorNotesFolderWatcherActor, LLMTinkeringActor}
import me.micseydel.actor.ollama.OllamaModel.{ChatResponse, ChatResponseFailure, ChatResponseResult}
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import java.util.regex.Pattern
import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object WhiteSpaceAddingExperimentActor {
  sealed trait Message

  private case class ReceiveNoteContents(note: Note) extends Message

  private case class ReceiveException(exception: Throwable) extends Message

  private case class ReceivePromptResponse(response: ChatResponse) extends Message

  def apply(filename: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.initializedWithNote(
    dropDotMdFromEndOfFileNameIfPresent(filename), // FIXME: hack for .md suffix becoming duplicated
    s"${ActorNotesFolderWatcherActor.ActorNotesSubdirectory}/${LLMTinkeringActor.Folder}"
  ) { (context, noteRef) =>
    context.actorContext.log.info(s"Initialized for $filename, doing an async file read...")

    implicit val ec: ExecutionContextExecutor = context.system.actorSystem.executionContext
    context.pipeToSelf(noteRef.readNoteAsync()) {
      case Failure(exception) => ReceiveException(exception)
      case Success(note) => ReceiveNoteContents(note)
    }

    fetchingNoteContents(noteRef)
  }

  private val PromptPrefix = "Please take the following block of text without newlines, and add them where they should be. Do not, otherwise, modify the text and do not add commentary."

  private def fetchingNoteContents(noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveNoteContents(note) =>
        val normalizedMarkdown = normalizeWhitespace(note.markdown)

        context.actorContext.log.debug("Spawning an anonymous FetchChatResponse actor")

        val prompt = PromptPrefix + "\n\n" + normalizedMarkdown
        context.castAnonymous(FetchChatResponseActor(prompt, "llama3", context.messageAdapter(ReceivePromptResponse)))

        initialized(noteRef, normalizedMarkdown)

      case ReceiveException(exception) =>
        context.actorContext.log.error("Failed to access disk", exception)
        Tinker.steadily

      case ReceivePromptResponse(wtf) =>
        context.actorContext.log.error(s"Did not expect a prompt response yet: $wtf")
        Tinker.steadily
    }
  }

  private def initialized(noteRef: NoteRef, normalizedText: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveNoteContents(_) =>
        context.actorContext.log.warn(s"Ignoring note contents, already received")
        Tinker.steadily

      case ReceiveException(exception) =>
        context.actorContext.log.error("Failed to access disk", exception)
        Tinker.steadily

      case ReceivePromptResponse(ChatResponseResult(resultText, _)) =>
        val normalizedResultText = normalizeWhitespace(resultText)
        context.actorContext.log.info(s"Received prompt reply!")

        val followsTheRule = normalizedText == normalizedResultText
        val emoji = if (followsTheRule) {
          "✅"
        } else {
          "❌"
        }

        val result =
          s"""# LLM whitespace tinkering
             |
             |- Appears to follow the change-the-whitespace-not-the-text rule? $emoji $followsTheRule
             |
             |## Result text
             |
             |$resultText
             |""".stripMargin

        noteRef.append(s"\n$result\n")
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

  // FIXME: hacky copy-paste
  private def dropDotMdFromEndOfFileNameIfPresent(string: String): String = {
    if (string.endsWith(".md")) {
      string.dropRight(3)
    } else {
      string
    }
  }

  private val Pat = Pattern.compile("\\s+")

  private def normalizeWhitespace(text: String): String = {
    // `python " ".(text.split())` FIXME should probably just use regex replace
    Pat.split(text).mkString(" ")
  }
}
