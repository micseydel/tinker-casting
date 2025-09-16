package me.micseydel.actor.airgradient

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import spray.json.*

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

private object AirGradientPollingActor {
  sealed trait Message

  private case object HeartBeat extends Message

  case class AlterPolling(forceCheckNow: Boolean, intervalUpdate: Option[FiniteDuration]) extends Message

  def apply(uri: String, initialInterval: FiniteDuration, replyTo: SpiritRef[AirGradientSensorData])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[Message] = context

    val airGradientApiActor = context.cast(AirGradientApiActor(uri), "AirGradientApiActor")
    context.actorContext.log.info(s"Making initial request then polling every $initialInterval")
    airGradientApiActor !! AirGradientApiActor.Request(replyTo)

    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(initialInterval, context.self, AirGradientPollingActor.HeartBeat, Some(AirGradientPollingActor.HeartBeat))

    behavior(airGradientApiActor, replyTo, timeKeeper)
  }

  private def behavior(api: SpiritRef[AirGradientApiActor.Message], replyTo: SpiritRef[AirGradientSensorData], timeKeeper: SpiritRef[TimeKeeper.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[Message] = context

    message match {
      case HeartBeat =>
        context.actorContext.log.info("Received heartbeat, making request...")
        api !! AirGradientApiActor.Request(replyTo)
        Tinker.steadily

      case inert@AlterPolling(false, None) =>
        context.actorContext.log.debug(s"Received inert AlterPolling request $inert")
        Tinker.steadily

      case a@AlterPolling(forceCheckNow, maybeDuration) =>
        context.actorContext.log.debug(s"AlterPolling $a")
        if (forceCheckNow) {
          context.actorContext.log.info("A check outside of the usual heartbeat has been requested, making API request now...")
          api !! AirGradientApiActor.Request(replyTo)
        }

        maybeDuration match {
          case Some(updatedInterval) =>
            context.actorContext.log.info(s"Updating polling interval to $updatedInterval")
            timeKeeper !! TimeKeeper.RemindMeEvery(updatedInterval, context.self, AirGradientPollingActor.HeartBeat, Some(AirGradientPollingActor.HeartBeat))
          case None =>
        }

        Tinker.steadily
    }
  }
}


private object AirGradientApiActor {
  sealed trait Message

  case class Request(replyTo: SpiritRef[AirGradientSensorData]) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse, replyTo: SpiritRef[AirGradientSensorData]) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable) extends Message

  private case class ReceiveUnmarshalling(sensorData: AirGradientSensorData, replyTo: SpiritRef[AirGradientSensorData]) extends Message

  def apply(uri: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[?] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    Tinker.receiveMessage {
      case Request(replyTo) =>
        makeRequest(uri, replyTo)
        Tinker.steadily

      case ReceiveHttpResponse(httpResponse, replyTo) =>
        context.actorContext.log.info("Received request response...")

        implicit val rawSensorJsonFormat: RootJsonFormat[AirGradientSensorData] = AirGradientJsonFormat.airGradientSensorDataJsonFormat
        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]
        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[AirGradientSensorData], replyTo)
            } catch {
              case e: DeserializationException =>
                ReceiveFailedHttpResponse(new RuntimeException(s"failed to parse: $models", e))
              case e: Throwable =>
                ReceiveFailedHttpResponse(new RuntimeException(s"something unexpected went wrong", e))
            }
        }

        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.warn(s"Failed to connect to local Air Gradient", exception)
        Tinker.steadily

      case ReceiveUnmarshalling(models, replyTo) =>
        context.actorContext.log.info("Unmarshalling successful, replying and wrapping")
        replyTo !! models
        Tinker.steadily
    }
  }

  // util

  private def makeRequest(uri: String, replyTo: SpiritRef[AirGradientSensorData])(implicit context: TinkerContext[Message], actorSystem: ActorSystem[?]): Unit = {
    context.pipeToSelf(request(uri)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse, replyTo)
    }
  }

  private def request(uri: String)(implicit s: ActorSystem[?]): Future[HttpResponse] = {
    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = uri,
      headers = List(
        headers.Accept(MediaTypes.`application/json`)
      )
    ))
  }
}


object AirGradientJsonFormat extends DefaultJsonProtocol {
  implicit val airGradientSensorDataJsonFormat: RootJsonFormat[AirGradientSensorData] = jsonFormat5(AirGradientSensorData)
}

/*
{
  "pm01": 0,
  "pm02": 0,
  "pm10": 0,
  "pm01Standard": 0,
  "pm02Standard": 0,
  "pm10Standard": 0,
  "pm003Count": 135.5,
  "pm005Count": 95.33,
  "pm01Count": 16.83,
  "pm02Count": 2.17,
  "pm50Count": 0.67,
  "pm10Count": 0.5,
  "atmp": 19.3,
  "atmpCompensated": 19.3,
  "rhum": 65.01,
  "rhumCompensated": 65.01,
  "pm02Compensated": 0,
  "rco2": 464,
  "tvocIndex": 88.25,
  "tvocRaw": 31276.58,
  "noxIndex": 1,
  "noxRaw": 16168.42,
  "boot": 5,
  "bootCount": 5,
  "wifi": -19,
  "ledMode": "pm",
  "serialno": "34b7dabd4bc4",
  "firmware": "3.1.11",
  "model": "I-9PSL"
}

 */
case class AirGradientSensorData(
                                  //"pm01": 0,
                                  //"pm02": 0,
                                  //"pm10": 0,
                                  //"pm01Standard": 0,
                                  //"pm02Standard": 0,
                                  //"pm10Standard": 0,
                                  //"pm003Count": 135.5,
                                  //"pm005Count": 95.33,
                                  //"pm01Count": 16.83,
                                  pm02Count: Double,
                                  //"pm50Count": 0.67,
                                  pm10Count: Double,
                                  //"atmp": 19.3,
                                  //"atmpCompensated": 19.3,
                                  //"rhum": 65.01,
                                  //"rhumCompensated": 65.01,
                                  pm02Compensated: Double,
                                  rco2: Double,
                                  tvocIndex: Double,
                                  //"tvocRaw": 31276.58,
                                  //"noxIndex": 1,
                                  //"noxRaw": 16168.42,
                                  //"boot": 5,
                                  //"bootCount": 5,
                                  //"wifi": -19,
                                  //"ledMode": "pm",
                                  //"serialno": "34b7dabd4bc4",
                                  //"firmware": "3.1.11",
                                  //"model": "I-9PSL"
                                ) {

  //  def zonedDatetime: ZonedDateTime =
  //    ZonedDateTime.parse(DateTime, Formatter)
  //      .withZoneSameInstant(ZoneId.systemDefault())
}

