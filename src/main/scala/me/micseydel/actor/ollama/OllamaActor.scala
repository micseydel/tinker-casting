package me.micseydel.actor.ollama

import me.micseydel.actor.FolderWatcherActor
import me.micseydel.actor.FolderWatcherActor.PathUpdatedEvent
import me.micseydel.actor.ollama.OllamaModel.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.VaultKeeper
import spray.json.*

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

// FIXME https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion
object OllamaActor {

  val OllamaPrompts: String = "ollamaprompts"
  val OllamaPromptsSubdirectory: String = s"_actor_notes/${OllamaActor.OllamaPrompts}"

  sealed trait Message

  private case class ReceiveModels(models: Models) extends Message

  private case class ReceivePathUpdatedEvent(event: PathUpdatedEvent) extends Message

  val NoteName = "Ollama Testing"
  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(NoteName, TinkerColor(7, 164, 223), "🦙") { (context, noteRef) =>
    implicit val c: TinkerContext[_] = context

    val hostAndPort = noteRef.readNote().flatMap(_.yamlFrontMatter) match {
      case Failure(exception) => throw exception
      case Success(frontmatter) =>
        frontmatter.get("server") match {
          case Some(value: String) =>
            context.actorContext.log.info(s"Found $value in [[$NoteName]] for server")
            value
          case None =>
            val default = "localhost:11434"
            context.actorContext.log.info(s"No server key in [[$NoteName]], defaulting to $default")
            default
          case other => throw new RuntimeException(s"Expected a string under the key `server` but got $other")
        }
    }

    context.actorContext.log.info("Requesting Ollama models....")
    context.castAnonymous(FetchModelsActor(hostAndPort, context.messageAdapter(ReceiveModels)))

    context.actorContext.log.info(s"Subscribing to updates to $OllamaPrompts")
    context.system.vaultKeeper !! VaultKeeper.SubscribeUpdatesForFolder(context.messageAdapter(ReceivePathUpdatedEvent).underlying, OllamaPromptsSubdirectory)
//    context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.SubscribeSubdirectory(OllamaPrompts, context.messageAdapter(ReceivePathUpdatedEvent))

    Tinker.receiveMessage {
      case ReceiveModels(Models(models)) =>
        context.actorContext.log.info(s"Received ${models.size} models, writing to markdown")
        val lines = models.map {
          case Model(name, _, _, _, Details(_, _, _, parameter_size, quantization_level)) =>
            s"- $name ($parameter_size at $quantization_level)"
        }
        noteRef.setMarkdown(lines.mkString("\n"))
        Tinker.steadily

      case ReceivePathUpdatedEvent(event) =>
        event match {
          case FolderWatcherActor.PathCreatedEvent(path) =>
            val notename = path.getFileName.toString
            context.actorContext.log.info(s"Path $path created, spawning prompt manager for [[$notename]]")
            context.castAnonymous(PromptFromFileManagerActor(hostAndPort, notename))
            Tinker.steadily
          case FolderWatcherActor.PathModifiedEvent(_) | FolderWatcherActor.PathDeletedEvent(_) =>
            context.actorContext.log.debug(s"Ignoring $event")
            Tinker.steadily
        }
    }
  }
}

object OllamaModel {
  case class Details(
                      format: String,
                      family: String,
                      families: Option[List[String]],
                      parameter_size: String,
                      quantization_level: String
                    )

  case class Model(
                    name: String,
                    modified_at: ZonedDateTime,
                    size: Long,
                    digest: String,
                    details: Details
                  )

  case class Models(models: List[Model])

  sealed trait ChatResponse

  /*
  {
  "model": "llama3.2",
  "created_at": "2023-08-04T19:22:45.499127Z",
  "response": "",
  "done": true,
  "context": [1, 2, 3],
  "total_duration": 10706818083,
  "load_duration": 6338219291,
  "prompt_eval_count": 26,
  "prompt_eval_duration": 130079000,
  "eval_count": 259,
  "eval_duration": 4232710000
  }


  {
  "model": "llama3:70b",
  "created_at": "2025-09-05T17:59:27.539054Z",
  "response": "The answer to 2 + 2 is 4.",
  "done": true,
  "done_reason": "stop",
  "total_duration": 3646721291,
  "load_duration": 57536041,
  "prompt_eval_count": 18,
  "prompt_eval_duration": 1567955125,
  "eval_count": 13,
  "eval_duration": 2020718458
}


   */

  /**
   * https://github.com/ollama/ollama/blob/main/docs/api.md#response
   * @param created_at
   * @param total_duration time spent generating the response
   * @param load_duration time spent in nanoseconds loading the model
   * @param prompt_eval_count number of tokens in the prompt
   * @param prompt_eval_duration time spent in nanoseconds evaluating the prompt
   * @param eval_count number of tokens in the response
   * @param eval_duration time in nanoseconds spent generating the response
   */
  case class ChatResponseResult(
                                 response: String,
                                 model: String,
                                 created_at: String, // format string? "2025-09-05T17:59:27.539054Z"
                                 total_duration: Long,
                                 load_duration: Long,
                                 prompt_eval_count: Long,
                                 prompt_eval_duration: Long,
                                 eval_count: Long,
                                 eval_duration: Long
                               ) extends ChatResponse

  case class ChatResponseFailure(message: String, exception: Option[Throwable] = None) extends ChatResponse

  object ChatResponseFailure {
    def apply(exception: Throwable): ChatResponse = {
      new ChatResponseFailure("something went wrong", Some(exception))
    }
  }
}


// serialization

object OllamaJsonFormat extends DefaultJsonProtocol {

  import me.micseydel.util.JsonUtil.ZonedDateTimeJsonFormat

  implicit val detailsFormat: RootJsonFormat[Details] = jsonFormat5(Details)
  implicit val modelFormat: RootJsonFormat[Model] = jsonFormat5(Model)
  implicit val receiveModelsFormat: RootJsonFormat[Models] = jsonFormat1(Models)

  implicit val chatResponseResultFormat: RootJsonFormat[ChatResponseResult] = jsonFormat9(ChatResponseResult)

  class ExceptionPlaceHolder(val className: String) extends Exception

  private implicit object ThrowableJsonFormat extends RootJsonFormat[Throwable] {
    def write(m: Throwable): JsValue = {
      JsObject("type" -> JsString(m.getClass.toString))
    }

    def read(value: JsValue): Throwable = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString(className)) => new ExceptionPlaceHolder(className)
        case other => throw DeserializationException(s"Excepted type to be a Seq(JsString(classname)), got $other")
      }
    }
  }

  implicit val chatResponseFailureFormat: RootJsonFormat[ChatResponseFailure] = jsonFormat2(ChatResponseFailure.apply)

  implicit object ChatResponseJsonFormat extends RootJsonFormat[ChatResponse] {
    def write(m: ChatResponse): JsValue = {
      val (jsObj, typ) = m match {
        case l: ChatResponseResult => (l.toJson.asJsObject, "ChatResponseResult")
        case l: ChatResponseFailure => (l.toJson.asJsObject, "ChatResponseFailure")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): ChatResponse = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("ChatResponseResult")) => value.convertTo[ChatResponseResult]
        case Seq(JsString("ChatResponseFailure")) => value.convertTo[ChatResponseFailure]
        case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(ChatResponseResult | ChatResponseFailure)) but got $other in object $value")
      }
    }
  }

//  implicit val chatResponseFormat: RootJsonFormat[ChatResponse] = jsonFormat2(ChatResponse)
}
