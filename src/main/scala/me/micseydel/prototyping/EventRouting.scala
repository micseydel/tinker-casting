package me.micseydel.prototyping

import akka.actor.typed.ActorRef
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import me.micseydel.actor.{EventReceiver, QuickVoiceCaptureActor}
import me.micseydel.actor.EventReceiver.IncomingEvent
import spray.json.*

import java.net.InetAddress


object EventRouting {
  def route(actorRef: ActorRef[EventReceiver.IncomingEvent]): Route = {
    // ! asynchronously pass the event off, as long as the structure is valid
    // else, report the error

    implicit def myExceptionHandler: ExceptionHandler =
      ExceptionHandler {
        case ex: DeserializationException =>
          complete(HttpResponse(
            StatusCodes.BadRequest,
            entity = s"Could not deserialize request: ${ex.getMessage}. Possible event types are: TypeA, TypeB, TypeC."
          ))
      }

    val event = handleExceptions(myExceptionHandler) {
      pathPrefix("event") {
        post {
          import me.micseydel.actor.EventReceiver.EventJsonProtocol.*
          entity(as[IncomingEvent]) { event =>
            actorRef ! event
            complete(StatusCodes.Accepted)
          }
        }
      }
    }

    val quickVoiceCapture = handleExceptions(myExceptionHandler) {
      pathPrefix("voice-capture") {
        get {
          val hostname = InetAddress.getLocalHost.getHostName
          // FIXME: need to dynamically populate the port
          val htmlContent = QuickVoiceCaptureActor.html("ws://localhost:5003/quick-voice-capture")
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, htmlContent))
        }
      }
    }

    concat(event, quickVoiceCapture)
  }
}
