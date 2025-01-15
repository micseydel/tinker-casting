package me.micseydel.prototyping

import akka.actor.typed.ActorRef
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import me.micseydel.actor.EventReceiver
import me.micseydel.actor.EventReceiver.IncomingEvent
import spray.json._


object EventRouting {
  def route(actorRef: ActorRef[EventReceiver.IncomingEvent]): Route = {
    // ! asynchronously pass the event off, as long as the structure is valid
    // else, report the error

    // Define exception handler
    implicit def myExceptionHandler: ExceptionHandler =
      ExceptionHandler {
        case ex: DeserializationException =>
          complete(HttpResponse(
            StatusCodes.BadRequest,
            entity = s"Could not deserialize request: ${ex.getMessage}. Possible event types are: TypeA, TypeB, TypeC."
          ))
      }

    handleExceptions(myExceptionHandler) {
      pathPrefix("event") {
        post {
          import me.micseydel.actor.EventReceiver.EventJsonProtocol._
          entity(as[IncomingEvent]) { event =>
            actorRef ! event // sending the event to the actor
            complete(StatusCodes.Accepted) // HTTP 202
          }
        }
      }
    }
  }
}
