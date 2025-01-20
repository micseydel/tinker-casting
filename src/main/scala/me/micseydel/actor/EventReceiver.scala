package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.concat
import akka.http.scaladsl.server.Route
import me.micseydel.actor.AudioNoteCapturer.AcceptableFileExts
import me.micseydel.dsl.cast.TinkerBrain
import me.micseydel.model.WhisperResult
import me.micseydel.prototyping.{EventRouting, WebSocketRouting}
import spray.json._

import java.nio.file.Paths
import scala.util.{Failure, Success}

object EventReceiver {
  sealed trait Message

  case class IncomingEvent(eventType: EventType, payload: String) extends Message
  case class ClaimEventType(eventType: EventType, claimant: ActorRef[String]) extends Message

  // config

  case class Config(httpHost: String, httpPort: Int)

  // behavior

  def apply(config: Config, tinkerBrain: ActorRef[TinkerBrain.Message]): Behavior[Message] = Behaviors.setup { context =>
    val route = concat(EventRouting.route(context.self), WebSocketRouting.websocketRoute(tinkerBrain))
    context.log.info(f"Starting event receiver HTTP server on port ${config.httpPort}")
    startHttpServer(route, config.httpPort)(context.system)

    behavior(Map.empty)
  }

  private def behavior(claimants: Map[EventType, ActorRef[String]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case IncomingEvent(eventType, payload) =>
        claimants.get(eventType) match {
          case Some(claimant) =>
            if (context.log.isDebugEnabled) {
              context.log.debug("Forwarding event of type {} and size {} to {}", eventType, payload, claimant)
            }
            claimant ! payload
          case None =>
            context.log.warn(s"Dropped event of type $eventType size ${payload.length}, no claimant")
        }

        Behaviors.same

      case ClaimEventType(eventType, claimant) =>
        claimants.get(eventType) match {
          case Some(existingClaimant) =>
            if (claimant == existingClaimant) {
              context.log.debug(s"Received duplicated claimant $claimant for $eventType")
            } else {
              context.log.warn(s"Unexpected situation, $eventType claimed by $existingClaimant with a new claim by $claimant; ignoring the second one")
            }

            Behaviors.same

          case None =>
            context.log.info(s"Event type $eventType claimed by $claimant")
            behavior(claimants.updated(eventType, claimant))
        }
    }
  }

  // model

  sealed trait EventType
  object TranscriptionCompleted extends EventType
  object HeartRate extends EventType // FIXME: placeholder

  // util

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

  // serde

  object EventJsonProtocol extends DefaultJsonProtocol {
    implicit object EventTypeJsonFormat extends RootJsonFormat[EventType] {
      def write(e: EventType): JsString = JsString(e.toString)

      def read(value: JsValue): EventType = value match {
        case JsString(s) => s match {
          case "TranscriptionCompleted" | "transcription_completed" => TranscriptionCompleted
          case _ => throw DeserializationException(s"Expected transcription_completed")
        }
        case _ => throw DeserializationException(s"Expected transcription_completed")
      }
    }

    implicit val eventFormat: RootJsonFormat[IncomingEvent] = jsonFormat2(IncomingEvent)
  }
}
