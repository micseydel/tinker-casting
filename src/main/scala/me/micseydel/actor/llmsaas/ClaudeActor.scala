package me.micseydel.actor.llmsaas

//https://claude.ai/chat/b25f51b5-0c17-4a6d-b639-bca911127c85

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.headers.*
import akka.stream.Materializer
import spray.json.*

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

// =============================================================================
// ANTHROPIC
// =============================================================================

object AnthropicJsonProtocol extends DefaultJsonProtocol {

  // ── /v1/messages ──────────────────────────────────────────────────────────
  case class Message(role: String, content: String)
  case class CompletionRequest(model: String, max_tokens: Int, messages: List[Message])
  case class ContentBlock(`type`: String, text: String)
  case class Usage(input_tokens: Int, output_tokens: Int)
  case class CompletionResponse(
                                 id: String,
                                 `type`: String,
                                 role: String,
                                 content: List[ContentBlock],
                                 model: String,
                                 stop_reason: String,
                                 usage: Usage
                               )

  implicit val messageFormat: RootJsonFormat[Message]             = jsonFormat2(Message)
  implicit val requestFormat: RootJsonFormat[CompletionRequest]   = jsonFormat3(CompletionRequest)
  implicit val contentBlockFormat: RootJsonFormat[ContentBlock]   = jsonFormat2(ContentBlock)
  implicit val usageFormat: RootJsonFormat[Usage]                 = jsonFormat2(Usage)
  implicit val responseFormat: RootJsonFormat[CompletionResponse] = jsonFormat7(CompletionResponse)

  // ── /v1/models ────────────────────────────────────────────────────────────
  // { "data": [{ "id": "...", "display_name": "...", "created_at": 0, "type": "model" }] }
  case class AnthropicModel(id: String, display_name: String, `type`: String)
  case class AnthropicModelsResponse(data: List[AnthropicModel])

  implicit val anthropicModelFormat: RootJsonFormat[AnthropicModel]          = jsonFormat3(AnthropicModel)
  implicit val anthropicModelsResponseFormat: RootJsonFormat[AnthropicModelsResponse] =
    jsonFormat1(AnthropicModelsResponse)
}

object ClaudeActor {
  import AnthropicJsonProtocol.*

  val DefaultModel = "claude-opus-4-6"

  // ── Public commands ────────────────────────────────────────────────────────
  sealed trait Command

  final case class Complete(
                             prompt: String,
                             replyTo: ActorRef[Response],
                             model: String,
                             maxTokens: Int = LLMSharedDomain.DefaultMaxTokens,
                           ) extends Command

  final case class ListModels(replyTo: ActorRef[Response]) extends Command

  // ── Public responses ───────────────────────────────────────────────────────
  sealed trait Response
  final case class CompletionSuccess(text: String, usage: Usage)  extends Response
  final case class ModelsSuccess(models: List[ModelInfo])          extends Response
  final case class Failure(reason: String)                         extends Response

  // ── Internal signals ───────────────────────────────────────────────────────
  private sealed trait Internal extends Command
  private case class GotCompletion(r: CompletionResponse, replyTo: ActorRef[Response]) extends Internal
  private case class GotModels(models: List[ModelInfo],   replyTo: ActorRef[Response]) extends Internal
  private case class GotError(reason: String,             replyTo: ActorRef[Response]) extends Internal

  // ── Behavior factory ───────────────────────────────────────────────────────
  def apply(apiKey: String): Behavior[Command] =
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val mat: Materializer    = Materializer(context.system)
      val http = Http(context.system)

      val baseHeaders = List(
        RawHeader("x-api-key", apiKey),
        RawHeader("anthropic-version", "2023-06-01")
      )

      def get(uri: String): Future[HttpResponse] =
        http.singleRequest(HttpRequest(uri = uri, headers = baseHeaders))

      def post(uri: String, body: String): Future[HttpResponse] =
        http.singleRequest(HttpRequest(
          method  = HttpMethods.POST,
          uri     = uri,
          headers = baseHeaders,
          entity  = HttpEntity(ContentTypes.`application/json`, body)
        ))

      def strict(f: Future[HttpResponse]): Future[HttpEntity.Strict] =
        f.flatMap(_.entity.toStrict(10.seconds))

      Behaviors.receiveMessage {

        // ── Complete ─────────────────────────────────────────────────────────
        case Complete(prompt, replyTo, model, maxTokens) =>
          val body = CompletionRequest(model, maxTokens, List(Message("user", prompt)))
            .toJson.compactPrint

          context.pipeToSelf(strict(post("https://api.anthropic.com/v1/messages", body))) {
            case Success(e) =>
              GotCompletion(e.data.utf8String.parseJson.convertTo[CompletionResponse], replyTo)
            case scala.util.Failure(ex) =>
              GotError(ex.getMessage, replyTo)
          }
          Behaviors.same

        // ── ListModels ───────────────────────────────────────────────────────
        case ListModels(replyTo) =>
          context.pipeToSelf(strict(get("https://api.anthropic.com/v1/models"))) {
            case Success(e) =>
              val parsed = e.data.utf8String.parseJson.convertTo[AnthropicModelsResponse]
              val infos  = parsed.data.map(m => ModelInfo(m.id, "anthropic"))
              GotModels(infos, replyTo)
            case scala.util.Failure(ex) =>
              GotError(ex.getMessage, replyTo)
          }
          Behaviors.same

        // ── Internal ─────────────────────────────────────────────────────────
        case internal: Internal =>
          internal match {
            case GotCompletion(r, replyTo) =>
              val text = r.content.filter(_.`type` == "text").map(_.text).mkString
              replyTo ! CompletionSuccess(text, r.usage)
            case GotModels(models, replyTo) =>
              replyTo ! ModelsSuccess(models)
            case GotError(reason, replyTo) =>
              replyTo ! Failure(reason)
          }
          Behaviors.same
      }
    }
}
