package me.micseydel.actor.llmsaas

import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor
import me.micseydel.actor.FolderWatcherActor.PathUpdatedEvent
import me.micseydel.actor.llmsaas.AnthropicJsonProtocol.Usage
import me.micseydel.actor.llmsaas.LLMCostManager.{LLMCostTrackingFailure, LLMCostTrackingSuccess, TrackUse}
import me.micseydel.actor.llmsaas.OpenAIJsonProtocol.OAUsage
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.VaultKeeper
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

object LLMSaaSTesting {
  sealed trait Message

  private case class ReceivePathUpdatedEvent(event: PathUpdatedEvent) extends Message

  //

  private val PromptsSubdirectory: String = s"_actor_notes/llmsaasprompttesting"

  def apply(maybeAnthropicKey: Option[String], maybeOpenaiKey: Option[String])
           (implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("LLMSaaSTesting", TinkerColor.random(), "🧪") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context

    (maybeAnthropicKey, maybeOpenaiKey) match {
      case (Some(anthropicKey), Some(openAIKey)) =>
        implicit val claude: SpiritRef[ClaudeActor.Command] = context.cast(ClaudeActor(anthropicKey), "ClaudeActor")
        implicit val openai: SpiritRef[OpenAIActor.Command] = context.cast(OpenAIActor(openAIKey), "OpenAIActor")
        implicit val llmCostManager: SpiritRef[LLMCostManager.Message] = context.cast(LLMCostManager(claude, openai), "LLMCostManager")

        context.actorContext.log.info(s"Subscribing to updates on $PromptsSubdirectory in the vault")
        context.system.vaultKeeper !! VaultKeeper.SubscribeUpdatesForFolder(context.messageAdapter(ReceivePathUpdatedEvent).underlying, PromptsSubdirectory)

        behavior()

      case other =>
        context.actorContext.log.warn(s"Expected both keys but found $other")
        Tinker.done
    }
  }

  private def behavior()(implicit Tinker: Tinker, claude: SpiritRef[ClaudeActor.Command], openai: SpiritRef[OpenAIActor.Command], llmCostManager: SpiritRef[LLMCostManager.Message]): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case ReceivePathUpdatedEvent(event) =>
        event match {
          case FolderWatcherActor.PathCreatedEvent(path) =>
            val filename = path.getFileName.toString
            if (filename.toLowerCase.endsWith(".md")) {
              val noteName = path.getFileName.toString.dropRight(3)
              context.actorContext.log.info(s"Casting $noteName")
              context.castAnonymous(LLMSaaSTestingHandler(noteName, PromptsSubdirectory))
            } else {
              context.actorContext.log.debug(s"Path $path is not a markdown file - wat?")
            }

          case other =>
            context.actorContext.log.debug(s"Ignoring $other")
        }

        Tinker.steadily
    }
  }
}

// -------

private object LLMSaaSTestingHandler {
  sealed trait Message

  private case class ReceiveAnthropic(response: ClaudeActor.Response) extends Message

  private case class ReceiveOpenAI(response: OpenAIActor.Response) extends Message

  def apply(noteName: String, subdirectory: String)(implicit Tinker: Tinker, claude: SpiritRef[ClaudeActor.Command], openai: SpiritRef[OpenAIActor.Command], llmCostManager: SpiritRef[LLMCostManager.Message]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "🐜", Some(subdirectory)) { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    implicit val nr: NoteRef = noteRef

    val prompt = noteRef.readMarkdown() match {
      case Failure(exception) => throw exception
      case Success(text) => text
    }

    val inferenceStartTime: ZonedDateTime = context.system.clock.now()
    context.actorContext.log.info(s"Read prompt from $subdirectory/$noteName, using inferenceStartTime=$inferenceStartTime")

    claude !! ClaudeActor.Complete(prompt, context.messageAdapter(ReceiveAnthropic).underlying, ClaudeActor.DefaultModel)
    openai !! OpenAIActor.Complete(prompt, context.messageAdapter(ReceiveOpenAI).underlying, OpenAIActor.DefaultModel)

    // we just hold it in memory rather than dealing with disk as a simplicity trade-off re:risk of a system failure during a running prompt
    val document = Document(inferenceStartTime, prompt, ClaudeActor.DefaultModel, OpenAIActor.DefaultModel, None, None)
    behavior(document)
  }

  private def behavior(document: Document)(implicit Tinker: Tinker, noteRef: NoteRef, claude: SpiritRef[ClaudeActor.Command], openai: SpiritRef[OpenAIActor.Command], llmCostManager: SpiritRef[LLMCostManager.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case ReceiveAnthropic(response) =>
        response match {
          case cs@ClaudeActor.CompletionSuccess(_, Usage(input_tokens, output_tokens)) =>
            llmCostManager !! TrackUse(noteRef.noteId.id, document.inferenceStartTime, "anthropic", document.anthropicModel, LLMCostTrackingSuccess(input_tokens, output_tokens))
            val updatedDocument = document.copy(maybeAnthropicCompletion = Some(cs))
            noteRef.setDocument(updatedDocument)
            behavior(updatedDocument)

          case ClaudeActor.ModelsSuccess(models) =>
            context.actorContext.log.warn(s"Models not requested from Claude, something went wrong! But here they were: $models")
            Tinker.steadily

          case ClaudeActor.Failure(reason) =>
            context.actorContext.log.error(s"Failure from Anthropic: $reason")
            Tinker.steadily
        }

      case ReceiveOpenAI(response) =>
        response match {
          case cs@OpenAIActor.CompletionSuccess(_, OAUsage(prompt_tokens, completion_tokens, _)) =>
            llmCostManager !! TrackUse(noteRef.noteId.id, document.inferenceStartTime, "openai", document.openAIModel, LLMCostTrackingSuccess(prompt_tokens, completion_tokens))
            val updatedDocument = document.copy(maybeOpenaiCompletion = Some(cs))
            noteRef.setDocument(updatedDocument)
            behavior(updatedDocument)

          case OpenAIActor.ModelsSuccess(models) =>
            context.actorContext.log.warn(s"Models not requested from OpenAI, something went wrong! But here they were: $models")
            Tinker.steadily

          case OpenAIActor.Failure(reason) =>
            llmCostManager !! TrackUse(noteRef.noteId.id, document.inferenceStartTime, "openai", document.openAIModel, LLMCostTrackingFailure(reason))
            context.actorContext.log.error(s"Failure from OpenAI: $reason")
            Tinker.steadily
        }
    }
  }


  private case class Document(
                               inferenceStartTime: ZonedDateTime,
                               prompt: String,
                               anthropicModel: String,
                               openAIModel: String,
                               maybeAnthropicCompletion: Option[ClaudeActor.CompletionSuccess],
                               maybeOpenaiCompletion: Option[OpenAIActor.CompletionSuccess],
                             ) {
    def toMarkdown: String = {
      s"""$prompt
         |
         |# Meta
         |
         |- inference started ~$inferenceStartTime with anthropic($anthropicModel) and openai($openAIModel)
         |
         |# Anthropic
         |
         |${maybeAnthropicCompletion.map(_.text).getOrElse("(waiting)")}
         |
         |# OpenAI
         |
         |${maybeOpenaiCompletion.map(_.text).getOrElse("(waiting)")}
         |""".stripMargin
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def setDocument(document: Document): Unit = {
      noteRef.setMarkdown(document.toMarkdown) match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }
    }
  }
}
