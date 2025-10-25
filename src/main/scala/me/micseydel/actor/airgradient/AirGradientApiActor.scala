package me.micseydel.actor.airgradient

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import spray.json.*

import scala.concurrent.Future
import scala.util.{Failure, Success}

private object AirGradientApiActor {
  sealed trait Message

  case class Request(replyTo: SpiritRef[AirGradientSensorData]) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse, replyTo: SpiritRef[AirGradientSensorData]) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable, replyTo: SpiritRef[AirGradientSensorData], remainingRetries: Int) extends Message

  private case class ReceiveUnmarshalling(sensorData: AirGradientSensorData, replyTo: SpiritRef[AirGradientSensorData]) extends Message

  def apply(host: String)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[?] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    Tinker.receiveMessage {
      case Request(replyTo) =>
        makeRequest(host, replyTo, remainingRetries = 3)
        Tinker.steadily

      case ReceiveHttpResponse(httpResponse, replyTo) =>
        context.actorContext.log.info("Received request response...")

        implicit val rawSensorJsonFormat: RootJsonFormat[AirGradientSensorData] = AirGradientJsonFormat.airGradientSensorDataJsonFormat
        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]
        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception, replyTo, 0)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[AirGradientSensorData], replyTo)
            } catch {
              case e: DeserializationException =>
                ReceiveFailedHttpResponse(new RuntimeException(s"failed to parse: $models", e), replyTo, 0)
              case e: Throwable =>
                ReceiveFailedHttpResponse(new RuntimeException(s"something unexpected went wrong", e), replyTo, 0)
            }
        }

        Tinker.steadily

      case ReceiveFailedHttpResponse(exception, replyTo, remainingRetries) =>
        if (remainingRetries > 0) {
          exception match {
            case _: java.net.UnknownHostException =>
              context.actorContext.log.info(s"A common transient error has occurred, retrying (remainingRetries = ${remainingRetries - 1})", exception)
              makeRequest(host, replyTo, remainingRetries = remainingRetries - 1)

            case _ =>
              context.actorContext.log.warn(s"Failed to connect to local Air Gradient; there are $remainingRetries remaining retries but this exception is unknown so dropping this request, $replyTo will not receive a reply", exception)
          }
        } else {
          context.actorContext.log.warn(s"Failed to connect to local Air Gradient; no remaining retries so stopping for $replyTo", exception)
        }

        Tinker.steadily

      case ReceiveUnmarshalling(models, replyTo) =>
        context.actorContext.log.info("Unmarshalling successful, replying and wrapping")
        replyTo !! models
        Tinker.steadily
    }
  }

  // util

  private def makeRequest(host: String, replyTo: SpiritRef[AirGradientSensorData], remainingRetries: Int)(implicit context: TinkerContext[Message], actorSystem: ActorSystem[?]): Unit = {
    val uri = s"http://$host/measures/current"
    context.pipeToSelf(request(uri)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception, replyTo, remainingRetries)
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

