package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object LLMSaaSTesting {
  private val Prompt = "In short, how many fingers does a person have?"

  //

  sealed trait Message
  private case class ReceiveAnthropic(response: ClaudeActor.Response) extends Message
  private case class ReceiveOpenAI(response: OpenAIActor.Response) extends Message

  def apply(maybeAnthropicKey: Option[String], maybeOpenaiKey: Option[String])
           (implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("LLMSaaSTesting", TinkerColor.random(), "🧪") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    implicit val nr: NoteRef = noteRef

    (maybeAnthropicKey, maybeOpenaiKey) match {
      case (Some(anthropicKey), Some(openAIKey)) =>
        val claude: SpiritRef[ClaudeActor.Command] = context.cast(ClaudeActor(anthropicKey), "ClaudeActor")
        val openai: SpiritRef[OpenAIActor.Command] = context.cast(OpenAIActor(openAIKey), "OpenAIActor")

        val llmCostManager = context.cast(LLMCostManager(claude, openai), "LLMCostManager")

        //        claude !! ClaudeActor.Complete(Prompt, context.messageAdapter(ReceiveAnthropic).underlying)
        //        openai !! OpenAIActor.Complete(Prompt, context.messageAdapter(ReceiveOpenAI).underlying)

        val document = Document(Prompt, None, None, None, None)
        behavior(document)

      case other =>
        context.actorContext.log.warn(s"Expected both keys but found $other")
        Tinker.done
    }
  }

  private def behavior(document: Document)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case ReceiveAnthropic(response) =>
        response match {
          case cs@ClaudeActor.CompletionSuccess(_, _) =>
            val updatedDocument = document.copy(maybeAnthropicCompletion = Some(cs))
            noteRef.setDocument(updatedDocument)
            behavior(updatedDocument)

          case ClaudeActor.ModelsSuccess(models) =>
            val updatedDocument = document.copy(maybeAnthropicModels = Some(models))
            noteRef.setDocument(updatedDocument)
            behavior(updatedDocument)

          case ClaudeActor.Failure(reason) =>
            context.actorContext.log.error(s"Failure from Anthropic: $reason")
            Tinker.steadily
        }

      case ReceiveOpenAI(response) =>
        response match {
          case cs@OpenAIActor.CompletionSuccess(_, _) =>
            val updatedDocument = document.copy(maybeOpenaiCompletion = Some(cs))
            noteRef.setDocument(updatedDocument)
            behavior(updatedDocument)

          case OpenAIActor.ModelsSuccess(models) =>
            val updatedDocument = document.copy(maybeOpenaiModels = Some(models))
            noteRef.setDocument(updatedDocument)
            behavior(updatedDocument)

          case OpenAIActor.Failure(reason) =>
            context.actorContext.log.error(s"Failure from OpenAI: $reason")
            Tinker.steadily
        }
    }
  }

  //

  private case class Document(
                             prompt: String,
                             maybeAnthropicCompletion: Option[ClaudeActor.CompletionSuccess],
                             maybeAnthropicModels: Option[List[ModelInfo]],
                             maybeOpenaiCompletion: Option[OpenAIActor.CompletionSuccess],
                             maybeOpenaiModels: Option[List[ModelInfo]],
                             ) {
    def toMarkdown: String = {
      s"""- "$prompt"
         |
         |# Anthropic
         |
         |- maybeAnthropicCompletion: $maybeAnthropicCompletion
         |- maybeAnthropicModels: $maybeAnthropicModels
         |
         |# OpenAI
         |
         |- maybeOpenaiCompletion: $maybeOpenaiCompletion
         |- maybeOpenaiModels: $maybeOpenaiModels
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
