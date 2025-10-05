package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives.{concat, handleWebSocketMessages, path}
import akka.http.scaladsl.server.Route
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.{NotUsed, actor}
import me.micseydel.dsl.cast.TinkerBrain
import me.micseydel.prototyping.EventRouting
import spray.json.*
import akka.actor.typed.scaladsl.adapter.*
import me.micseydel.actor.inactive.QuickVoiceCaptureActor

import scala.util.{Failure, Success}

object EventReceiver {
  sealed trait Message

  case class IncomingEvent(eventType: EventType, payload: String) extends Message
  case class ClaimEventType(eventType: EventType, claimant: ActorRef[String]) extends Message

  // config

  case class Config(httpHost: String, httpPort: Int)

  // behavior

  def apply(config: Config, tinkerBrain: ActorRef[TinkerBrain.Message]): Behavior[Message] = Behaviors.setup { context =>

    val route = concat(
      EventRouting.route(context.self),
      WebSocketRouting.websocketRoute(tinkerBrain)
    )
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
//  object TranscriptionCompleted extends EventType
//  object HeartRate extends EventType
//  object Pupil extends EventType

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
//          case "TranscriptionCompleted" | "transcription_completed" => TranscriptionCompleted
//          case "HeartRate" | "heart_rate" => HeartRate
//          case "Pupil" | "pupil" => Pupil
          case _ => throw DeserializationException(s"Expected transcription_completed")
        }
        case _ => throw DeserializationException(s"Expected transcription_completed")
      }
    }

    implicit val eventFormat: RootJsonFormat[IncomingEvent] = jsonFormat2(IncomingEvent)
  }
}

private object WebSocketRouting {
  def websocketRoute(messageActor: ActorRef[TinkerBrain.RegisterClient]): Route = {
    path("ws-messages") {
      handleWebSocketMessages {
        val source = Source.actorRef[BinaryMessage](
          completionMatcher = PartialFunction.empty,
          failureMatcher = PartialFunction.empty,
          bufferSize = 10,
          overflowStrategy = OverflowStrategy.fail
        ).mapMaterializedValue { clientActor =>
          messageActor ! TinkerBrain.RegisterClient(clientActor)
          clientActor
        }

        Flow.fromSinkAndSourceCoupled(Sink.ignore, source)
      }
    }
  }

  // copy-paste from the function above
  def quickVoiceCaptureWebsocketRoute(messageActor: ActorRef[QuickVoiceCaptureActor.Command], untyped: akka.actor.ActorRef): Route = {
    path("quick-voice-capture") {
      handleWebSocketMessages {
        val source: Source[Message, actor.ActorRef] = Source.actorRef[BinaryMessage](
          completionMatcher = PartialFunction.empty,
          failureMatcher = PartialFunction.empty,
          bufferSize = 10,
          overflowStrategy = OverflowStrategy.fail
        ).mapMaterializedValue { clientActor =>
          messageActor ! QuickVoiceCaptureActor.RegisterClient(clientActor)
          clientActor
        }

        val sink: Sink[Message, NotUsed] = Sink.actorRef[Message](untyped, TextMessage("completed"), PartialFunction.empty)

        Flow.fromSinkAndSourceCoupled(sink, source)
      }
    }
  }
}

private object CustomAdapter {
  def apply(quickVoiceCaptureActor: ActorRef[QuickVoiceCaptureActor.Message]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage { message =>
      quickVoiceCaptureActor ! QuickVoiceCaptureActor.IncomingMessage(message)
      Behaviors.same
    }
  }
}
