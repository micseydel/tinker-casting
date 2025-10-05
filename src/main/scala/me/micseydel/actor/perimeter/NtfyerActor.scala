package me.micseydel.actor.perimeter

import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model.StatusCode
import akka.stream.StreamTcpException
import me.micseydel.dsl.{Tinker, Tinkerer}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.NetworkPerimeterActor.HttpResponse
import me.micseydel.dsl.cast.NetworkPerimeterActor

import java.util.UUID
import scala.util.{Failure, Success}

object NtfyerActor {
  // mailbox

  sealed trait Message

  case class DoNotify(key: String, message: String) extends Message

//  private
  case class NtfyHttpCallResult(response: NetworkPerimeterActor.HttpResponse) extends Message

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(0, 255, 255), "📧").setup { context =>
    Tinker.receiveMessage {
      case DoNotify(key, message) =>
        val url = s"https://ntfy.sh/$key"
        val perimeterKey = UUID.randomUUID().toString
        val adapter = context.messageAdapter(NtfyHttpCallResult)

//        context.actorContext.log.info(s"Sending HTTP POST request for perimeterKey $perimeterKey")
        context.actorContext.log.error(s"NtfyActor needs to be updated!!!")
        // FIXME
//        context.system.networkPerimeter ! NetworkPerimeterActor.DoHttpPost(url, message, adapter.underlying, perimeterKey)

        Behaviors.same

      case NtfyHttpCallResult(HttpResponse(key, result)) =>
        result match {
          case Success((statusCode, result)) =>
            if (statusCode == StatusCode.int2StatusCode(200)) {
              context.actorContext.log.debug(s"Succeeded: $result")
            } else {
              context.actorContext.log.warn(s"Unexpected status code $statusCode for result $result")
            }
          case Failure(exception:   StreamTcpException) =>
            context.actorContext.log.error(s"NtfyHttpCallResult failed for key $key; is there an internet connection?", exception)
          case Failure(exception) =>
            context.actorContext.log.error(s"NtfyHttpCallResult failed for key $key", exception)
        }

        Behaviors.same
    }
  }
}
