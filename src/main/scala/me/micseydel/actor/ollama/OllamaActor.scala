package me.micseydel.actor.ollama

import me.micseydel.actor.VaultPathAdapter.VaultPathUpdatedEvent
import me.micseydel.actor.ollama.OllamaModel._
import me.micseydel.actor.{ActorNotesFolderWatcherActor, VaultPathAdapter}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import spray.json._

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

// FIXME https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion
object OllamaActor {

  val OllamaPrompts: String = "ollamaprompts"
  val OllamaPromptsSubdirectory: String = s"${ActorNotesFolderWatcherActor.ActorNotesSubdirectory}/${OllamaActor.OllamaPrompts}"

  sealed trait Message

  private case class ReceiveModels(models: Models) extends Message

  private case class ReceivePathUpdatedEvent(event: VaultPathUpdatedEvent) extends Message

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
    context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.SubscribeSubdirectory(OllamaPrompts, context.messageAdapter(ReceivePathUpdatedEvent))

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
          case VaultPathAdapter.PathCreatedEvent(path) =>
            val notename = path.path.getFileName.toString
            context.actorContext.log.info(s"Path $path created, spawning prompt manager for [[$notename]]")
            context.castAnonymous(PromptFromFileManagerActor(hostAndPort, notename))
            Tinker.steadily
          case VaultPathAdapter.PathModifiedEvent(_) | VaultPathAdapter.PathDeletedEvent(_) =>
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

  case class ChatResponseResult(response: String, model: String) extends ChatResponse
  case class ChatResponseFailure(message: String, exception: Option[Throwable] = None) extends ChatResponse

  object ChatResponseFailure {
    def apply(exception: Throwable): ChatResponse = {
      new ChatResponseFailure("something went wrong", Some(exception))
    }
  }
}


// serialization

object OllamaJsonFormat extends DefaultJsonProtocol {

  import me.micseydel.Common.ZonedDateTimeJsonFormat

  implicit val detailsFormat: RootJsonFormat[Details] = jsonFormat5(Details)
  implicit val modelFormat: RootJsonFormat[Model] = jsonFormat5(Model)
  implicit val receiveModelsFormat: RootJsonFormat[Models] = jsonFormat1(Models)

  implicit val chatResponseResultFormat: RootJsonFormat[ChatResponseResult] = jsonFormat2(ChatResponseResult)

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
