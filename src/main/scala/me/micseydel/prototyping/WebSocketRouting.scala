package me.micseydel.prototyping

import akka.actor.typed.ActorRef
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import me.micseydel.dsl.cast.TinkerBrain

object WebSocketRouting {
  def websocketRoute(messageActor: ActorRef[TinkerBrain.RegisterClient]): Route = {
    path("ws-messages") {
      handleWebSocketMessages {
        val source = Source.actorRef[TextMessage](
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
}
