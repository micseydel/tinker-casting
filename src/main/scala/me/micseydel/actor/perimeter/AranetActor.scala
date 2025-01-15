package me.micseydel.actor.perimeter

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.Common.ZonedDateTimeJsonFormat
import me.micseydel.actor.notifications.NotificationCenterManager.{JustSideEffect, PushNotification}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import java.time.ZonedDateTime
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success}

object AranetActor {

  case class Config(aranetHost: String, aranetPort: Int, highCO2Key: Option[String])

  // mailbox
  sealed trait Message

  case class Fetch(replyTo: SpiritRef[AranetResults]) extends Message

  private final case class ReceiveHttpResult(results: AranetResults) extends Message

  private final case class ReceiveHttpFailure(message: String, exception: Throwable) extends Message

  def apply(config: Config)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    behavior(config, lastSeenElevated = false)
  }

  private def behavior(config: Config, lastSeenElevated: Boolean)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val actorSystem: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[_] = context
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext

    Tinker.withMessages {
      case Fetch(replyTo) =>
        context.actorContext.log.info("Making HTTP request for Aranet")
        val responseFuture: Future[HttpResponse] = Http().singleRequest(
          HttpRequest(
            method = HttpMethods.GET,
            uri = s"http://${config.aranetHost}:${config.aranetPort}/ara4s"
          )
        )

        responseFuture.onComplete {
          case Success(response) =>
            import AranetJsonProtocol.payloadFormat
            val unmarshalFuture = Unmarshal(response.entity).to[AranetResults]
            context.pipeToSelf(unmarshalFuture) {
              case Success(response) =>
                replyTo !! response
                ReceiveHttpResult(response)
              case Failure(exception) =>
                ReceiveHttpFailure(s"Unmarshalling failure", exception)
            }
          case Failure(exception) =>
            context.self !! ReceiveHttpFailure("Future failed", exception)
        }

        Tinker.steadily
      case ReceiveHttpResult(aranetResults: AranetResults) =>
        context.actorContext.log.info(s"Notifying listener of ${aranetResults.preferred}")

        val maybeElevated = aranetResults.anyElevated
        val newAbility: Ability[Message] = (maybeElevated, lastSeenElevated) match {
          case (None, false) =>
            context.actorContext.log.info("No recent escalated CO2")
            Tinker.steadily
          case (Some(Aranet(_, elevatedCo2, _, name, _, _, _)), false) =>
            config.highCO2Key match {
              case Some(key) =>
                context.actorContext.log.info(s"Detected high CO2 $elevatedCo2 on $name, sending push notification")
                context.system.notifier !! JustSideEffect(PushNotification(key, s"Device $name has elevated CO2! $elevatedCo2"))

              case None =>
                context.actorContext.log.info(s"Detected high CO2 $elevatedCo2 on $name, but not sending push notification because no Ntfy key was provided")
            }
            behavior(config, lastSeenElevated = true)
          case (None, true) =>
            context.actorContext.log.info(s"Previously-elevated CO2 has decreased, will send push notification next time elevated")
            behavior(config, lastSeenElevated = false)
          case (Some(Aranet(_, elevatedCo2, _, name, _, _, _)), true) =>
            context.actorContext.log.info("Device {} value {} still high", name, elevatedCo2)
            Tinker.steadily
        }

        newAbility

      case ReceiveHttpFailure(message, throwable) =>
        context.actorContext.log.error(message, throwable)
        Tinker.steadily
    }
  }

  // model

  case class Aranet(
                     address: String,
                     co2: Int,
                     humidity: Double,
                     name: String,
                     pressure: Double,
                     rssi: Int,
                     temperature: Double
                   )

  case class Meta(elapsed: Double, captureTime: ZonedDateTime)

  case class AranetResults(aras: Map[String, Aranet], meta: Meta) {
    def preferred: Option[Aranet] = {
      // FIXME
      val primary = aras.get("1A300")
      val secondary = aras.get("24DBE")
      primary.orElse(secondary)
    }

    def anyElevated: Option[Aranet] = aras.values.find(_.co2 > 1000)
  }

  object AranetJsonProtocol extends DefaultJsonProtocol {
    implicit val aranetFormat: RootJsonFormat[Aranet] = jsonFormat7(Aranet)
    implicit val metaFormat: RootJsonFormat[Meta] = jsonFormat2(Meta)
    implicit val payloadFormat: RootJsonFormat[AranetResults] = jsonFormat2(AranetResults.apply)
  }
}
