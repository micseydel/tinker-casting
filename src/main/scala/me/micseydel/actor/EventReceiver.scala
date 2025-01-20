package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.server.Directives.concat
import me.micseydel.actor.AudioNoteCapturer.AcceptableFileExts
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TinkerBrain
import me.micseydel.model.{EventType, Tinkering, TranscriptionCompleted, WhisperResult}
import me.micseydel.prototyping.{EventRouting, WebSocketMessageActor, WebSocketRouting}
import me.micseydel.util.FileSystemUtil
import spray.json._

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success}

object EventReceiver {
  // actor mailbox

  sealed trait Message

  case class IncomingEvent(eventType: EventType, payload: String) extends Message
//  case class SubscribeToWhisperResultEvents(replyTo: ActorRef[WhisperResultEvent]) extends Message

  // config

  case class Config(httpHost: String, httpPort: Int)

  // behavior

  def apply(config: Config, replyTo: ActorRef[WhisperResult], tinkerbrain: ActorRef[TinkerBrain.Message]): Behavior[Message] = Behaviors.setup { context =>
    val route = concat(EventRouting.route(context.self), WebSocketRouting.websocketRoute(tinkerbrain))
    context.log.info(f"Starting event receiver HTTP server on port ${config.httpPort}")
    startHttpServer(route, config.httpPort)(context.system)

    Behaviors.receiveMessage {
      case IncomingEvent(TranscriptionCompleted, payload) =>
        import me.micseydel.model.WhisperResultJsonProtocol._
        try {
          val result: WhisperResult = payload.parseJson.convertTo[WhisperResult]

          context.log.debug(s"Received WhisperResult for ${result.whisperResultMetadata.vaultPath}")
          if (!result.whisperResultMetadata.vaultPath.toLowerCase.split("\\.").exists(AcceptableFileExts.contains)) {
            context.log.warn(s"Whisper result filename ${result.whisperResultMetadata.vaultPath} expected to end with .wav!")
          }

          val rawFilename = Paths.get(result.whisperResultMetadata.vaultPath).getFileName.toString
          val filename = s"${rawFilename}_${result.whisperResultMetadata.model}"
          context.log.debug(s"Storing JSON in $filename")

          replyTo ! result
        } catch {
          case e: DeserializationException =>
            context.log.error(s"Deserialization failed for payload $payload", e)
        }
        Behaviors.same

      case IncomingEvent(Tinkering, payload) =>
        context.log.info(s"Received Tinkering payload: $payload")
        Behaviors.same
    }
  }

  private def startHttpServer(routes: Route, port: Int)(implicit system: ActorSystem[_]): Unit = {
    // Akka HTTP still needs a classic ActorSystem to start
    import system.executionContext

    val futureBinding = Http().newServerAt("0.0.0.0", port).bind(routes)
    futureBinding.onComplete {
      case Success(binding) =>
        val address = binding.localAddress
        system.log.info("Server online at http://{}:{}/", address.getHostString, address.getPort)
      case Failure(ex) =>
        system.log.error("Failed to bind HTTP endpoint, terminating system", ex)
        system.terminate()
    }
  }


  object EventJsonProtocol extends DefaultJsonProtocol {
    implicit object EventTypeJsonFormat extends RootJsonFormat[EventType] {
      def write(e: EventType): JsString = JsString(e.toString)

      def read(value: JsValue): EventType = value match {
        case JsString(s) => s match {
          case "TranscriptionCompleted" | "transcription_completed" => TranscriptionCompleted
          case "Tinkering" | "tinkering" => Tinkering
          case _ => throw DeserializationException(s"Expected transcription_completed")
        }
        case _ => throw DeserializationException(s"Expected transcription_completed")
      }
    }

    implicit val eventFormat: RootJsonFormat[IncomingEvent] = jsonFormat2(IncomingEvent)
  }
}

@deprecated
private object JsonKeeper {
  // mailbox

  sealed trait Message[T]

  final case class Write[T](filenameWithoutExtension: String, toWrite: T) extends Message[T]

  final case class ReadRequest[T](filenameWithoutExtension: String, replyTo: ActorRef[T]) extends Message[T]

  // behavior

  def apply[T](jsonPath: Path, resultFormat: RootJsonFormat[T]): Ability[Message[T]] = Behaviors.setup { context =>
    context.log.debug(s"Starting with $jsonPath")

    def fullPath(filename: String) = {
      val path = jsonPath.resolve(filename + ".json")
      context.log.debug(s"Resolving $filename to $path using jsonPath=$jsonPath")
      path
    }

    // intentionally synchronous!
    Behaviors.receiveMessage {
      case ReadRequest(filename, replyTo) =>
        context.log.info(s"Fetching $filename.json raw contents from json folder")
        val rawContents = FileSystemUtil.getPathContents(fullPath(filename))
        context.log.info(s"Converting $filename.json raw contents to case class")
        val contents = rawContents.parseJson.convertTo[T](resultFormat)

        replyTo ! contents

        Behaviors.same

      case Write(filename, toWrite) =>
        context.log.debug(s"Writing to $filename.json in json folder")
        FileSystemUtil.writeToPath(fullPath(filename), toWrite.toJson(resultFormat).prettyPrint)
        Behaviors.same
    }
  }
}
