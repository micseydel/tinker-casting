package me.micseydel.actor.llmsaas

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse}
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.Materializer
import spray.json.*
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

// =============================================================================
// OPENAI
// =============================================================================

object OpenAIJsonProtocol extends DefaultJsonProtocol {

  // ── /v1/chat/completions ──────────────────────────────────────────────────
  case class OAMessage(role: String, content: String)
  case class OARequest(model: String, max_tokens: Int, messages: List[OAMessage])
  case class OAMessageContent(role: String, content: String)
  case class OAChoice(index: Int, message: OAMessageContent, finish_reason: String)
  case class OAUsage(prompt_tokens: Int, completion_tokens: Int, total_tokens: Int)
  case class OACompletionResponse(id: String, `object`: String, model: String,
                                  choices: List[OAChoice], usage: OAUsage)

  implicit val oaMessageFormat: RootJsonFormat[OAMessage]                     = jsonFormat2(OAMessage)
  implicit val oaRequestFormat: RootJsonFormat[OARequest]                     = jsonFormat3(OARequest)
  implicit val oaMessageContentFormat: RootJsonFormat[OAMessageContent]       = jsonFormat2(OAMessageContent)
  implicit val oaChoiceFormat: RootJsonFormat[OAChoice]                       = jsonFormat3(OAChoice)
  implicit val oaUsageFormat: RootJsonFormat[OAUsage]                         = jsonFormat3(OAUsage)
  implicit val oaCompletionResponseFormat: RootJsonFormat[OACompletionResponse] =
    jsonFormat5(OACompletionResponse)

  // ── /v1/models ────────────────────────────────────────────────────────────
  // { "data": [{ "id": "...", "object": "model", "created": 0, "owned_by": "openai" }] }
  case class OAModel(id: String, `object`: String, owned_by: String)
  case class OAModelsResponse(data: List[OAModel])

  implicit val oaModelFormat: RootJsonFormat[OAModel]               = jsonFormat3(OAModel)
  implicit val oaModelsResponseFormat: RootJsonFormat[OAModelsResponse] = jsonFormat1(OAModelsResponse)
}

object OpenAIActor {
  import OpenAIJsonProtocol.*

  val DefaultModel = "gpt-4o"

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
  final case class CompletionSuccess(text: String, usage: OAUsage) extends Response
  final case class ModelsSuccess(models: List[ModelInfo])           extends Response
  final case class Failure(reason: String)                          extends Response

  // ── Error envelope (shared shape for all OpenAI error responses) ──────────
  case class OAErrorDetail(message: String, `type`: String, code: Option[String])
  case class OAErrorResponse(error: OAErrorDetail)

  implicit val oaErrorDetailFormat: RootJsonFormat[OAErrorDetail]     = jsonFormat3(OAErrorDetail)
  implicit val oaErrorResponseFormat: RootJsonFormat[OAErrorResponse] = jsonFormat1(OAErrorResponse)

  // ── Internal signals ───────────────────────────────────────────────────────
  private sealed trait Internal extends Command
  private case class GotCompletion(r: OACompletionResponse, replyTo: ActorRef[Response]) extends Internal
  private case class GotModels(models: List[ModelInfo],      replyTo: ActorRef[Response]) extends Internal
  private case class GotError(reason: String,                replyTo: ActorRef[Response]) extends Internal

  // ── Behavior factory ───────────────────────────────────────────────────────
  def apply(apiKey: String): Behavior[Command] =
    Behaviors.setup { context =>
      implicit val ec: ExecutionContext = context.executionContext
      implicit val mat: Materializer    = Materializer(context.system)
      val http = Http(context.system)

      val authHeader = RawHeader("Authorization", s"Bearer $apiKey")

      def get(uri: String): Future[HttpResponse] =
        http.singleRequest(HttpRequest(uri = uri, headers = List(authHeader)))

      def post(uri: String, body: String): Future[HttpResponse] =
        http.singleRequest(HttpRequest(
          method  = HttpMethods.POST,
          uri     = uri,
          headers = List(authHeader),
          entity  = HttpEntity(ContentTypes.`application/json`, body)
        ))

      //      def strict(f: Future[HttpResponse]): Future[HttpEntity.Strict] =
      //        f.flatMap(_.entity.toStrict(10.seconds))

      // ── shared helper inside Behaviors.setup ─────────────────────────────────
      def handleResponse[A](
                             f: Future[HttpResponse],
                             replyTo: ActorRef[Response]
                           )(parse: String => A)(wrap: A => Internal): Unit = {

        val result: Future[Internal] = f.flatMap { resp =>
          resp.entity.toStrict(10.seconds).map { entity =>
            val body = entity.data.utf8String
            if (resp.status.isSuccess) {
              Try(parse(body))
                .map(wrap)
                .getOrElse(GotError(s"Parse error: $body", replyTo))
            } else {
              // Try to extract the structured error message; fall back to raw body
              val msg = Try(body.parseJson.convertTo[OAErrorResponse])
                .map(_.error.message)
                .getOrElse(s"HTTP ${resp.status.intValue}: $body")
              GotError(msg, replyTo)
            }
          }
        }

        context.pipeToSelf(result) {
          case Success(internal)              => internal
          case scala.util.Failure(ex)        => GotError(ex.getMessage, replyTo)
        }
      }

      Behaviors.receiveMessage {
        case Complete(prompt, replyTo, model, maxTokens) =>
          val body = OARequest(model, maxTokens, List(OAMessage("user", prompt))).toJson.compactPrint
          handleResponse(post("https://api.openai.com/v1/chat/completions", body), replyTo)(
            raw => raw.parseJson.convertTo[OACompletionResponse]
          )(r => GotCompletion(r, replyTo))
          Behaviors.same

        case ListModels(replyTo) =>
          handleResponse(get("https://api.openai.com/v1/models"), replyTo)(
            raw => raw.parseJson.convertTo[OAModelsResponse].data.map(m => ModelInfo(m.id, m.owned_by))
          )(infos => GotModels(infos, replyTo))
          Behaviors.same

        // ── Internal ─────────────────────────────────────────────────────────
        case internal: Internal =>
          internal match {
            case GotCompletion(r, replyTo) =>
              val text = r.choices.headOption.map(_.message.content).getOrElse("")
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
