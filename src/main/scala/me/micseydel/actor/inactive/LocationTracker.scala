package me.micseydel.actor.inactive

import me.micseydel.actor.inactive.owntracks.OwnTracksPayload
import me.micseydel.actor.{DailyMarkdownFromPersistedMessagesActor, DailyNotesRouter}
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext}
import me.micseydel.util.MarkdownUtil
import spray.json._

import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneId, ZonedDateTime}
import scala.util.{Failure, Success, Try}

object LocationTracker {
  sealed trait Message

  case class ReceiveMqtt(event: ReceiveMqttEvent) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    val dailyNotesAssistant = context.cast(
      DailyNotesRouter(
        "Owntracks updates",
        "owntracks_updates",
        owntracks.JsonFormat.ownTracksPayloadFormat,
        toMarkdown
      ), "DailyNotesRouter")

    behavior(dailyNotesAssistant)
  }

  private def behavior(dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[OwnTracksPayload]]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case ReceiveMqtt(ReceiveMqttEvent(owntracks.Topic, payload)) =>
        context.actorContext.log.info(s"received topic ${owntracks.Topic} message, attempting to deserialize ${payload.length} bytes")
        val json = new String(payload, StandardCharsets.UTF_8)
        context.actorContext.log.info(s"(TURN THIS TO DEBUG) payload json: $json")
        Try(json.parseJson.convertTo(owntracks.JsonFormat.ownTracksPayloadFormat)) match {
          case Failure(de: DeserializationException) if de.cause.isInstanceOf[NoSuchElementException] =>
            val cause = de.cause
//          case Failure(nsee: NoSuchElementException) =>
            if (cause.getMessage == "key not found: created_at" && json == """{"_type":"lwt"}""") {
              context.actorContext.log.debug("not sure what lwt messages are about but I think it's safe to ignore them", de)
            } else {
              context.actorContext.log.error(s"Failed to deserialize owntracks JSON $json, probably a key error", de)
            }
          case Failure(exception) =>
            context.actorContext.log.error(s"Failed to deserialize owntracks JSON $json", exception)
          case Success(ownTracksPayload) =>
            context.actorContext.log.info("JSON extraction successful, persisting to disk")
            dailyNotesAssistant !! DailyNotesRouter.Envelope(DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(ownTracksPayload), ownTracksPayload.createdTime)
        }

        Tinker.steadily

      case ReceiveMqtt(ReceiveMqttEvent(topic, payload)) =>
        context.actorContext.log.warn(s"unexpectedly received: ${payload.length} bytes for topic $topic")
        Tinker.steadily
    }
  }

  //

  private def toMarkdown(elements: List[OwnTracksPayload], clock: TinkerClock): String = {
    elements.map { record =>
      val line = record match {
        case OwnTracksPayload(_type, bssid, ssid, acc, alt, batt, bs, conn, created_at, lat, lon, m, t, tid, tst, vac, vel, beacon, request, steps) =>
          val battery = bs match {
            case Some(1) => "unplugged"
            case Some(2) => "charging"
            case Some(3) => "full"
            case None | Some(0) | Some(_) => "unknown"
          }
          val connectivityStatus = conn match {
            case Some("w") => "wifi"
            case Some("o") => "offline"
            case Some("m") => "mobile data"
            case _ => "(unknown)"
          }
          val monitoringMode = m match {
            case Some(1) => "significant"
            case Some(2) => "move"
            case _ => "(unknown)"
          }
          val trigger = t match {
            case Some("p") => "ping issued randomly by background task (iOS,Android)"
            case Some("c") => "circular region enter/leave event (iOS,Android)"
            case Some("b") => "beacon region enter/leave event (iOS)"
            case Some("r") => "response to a reportLocation cmd message (iOS,Android)"
            case Some("u") => "manual publish requested by the user (iOS,Android)"
            case Some("t") => "timer based publish in move move (iOS)"
            case Some("v") => "updated by Settings/Privacy/Locations Services/System Services/Frequent Locations monitoring (iOS)"
            case None | Some(_) => "(unknown)"
          }
          s"""ssid: $ssid
             |    - battery $battery at $batt%
             |    - Internet connectivity status $connectivityStatus
             |    - lat/lon $lat/$lon
             |    - monitoringMode $monitoringMode
             |    - trigger $trigger
             |    - tst $tst, vac $vac
             |    - velocity $vel""".stripMargin
      }
      MarkdownUtil.listLineWithTimestamp(record.createdTime, line)
    }.mkString("\n") + "\n"
  }
}

object owntracks {
  val Topic = "owntracks/mosquitto/Pixel6"

  // https://owntracks.org/booklet/tech/json/#types

  /**
   *
   * @param _type "location"
   * @param BSSID if available, identifies the access point. (iOS,string/optional)
   * @param SSID if available, is the unique name of the WLAN. (iOS,string/optional)
   * @param acc Accuracy of the reported location in meters without unit (iOS,Android/integer/meters/optional)
   * @param alt Altitude measured above sea level (iOS,Android/integer/meters/optional)
   * @param batt Device battery level (iOS,Android/integer/percent/optional)
   * @param bs Battery Status 0=unknown, 1=unplugged, 2=charging, 3=full (iOS, Android)
   * @param conn Internet connectivity status (route to host) when the message is created (iOS,Android/string/optional/extended data)
   *             w phone is connected to a WiFi connection (iOS,Android)
   *             o phone is offline (iOS,Android)
   *             m mobile data (iOS,Android)
   * @param created_at identifies the time at which the message is constructed (vs. tst which is the timestamp of the GPS fix) (iOS,Android)
   * @param lat latitude (iOS,Android/float/degree/required)
   * @param lon longitude (iOS,Android/float/degree/required)
   * @param m identifies the monitoring mode at which the message is constructed (significant=1, move=2) (iOS/integer/optional)
   * @param t trigger for the location report (iOS,Android/string/optional)
   *          p ping issued randomly by background task (iOS,Android)
   *          c circular region enter/leave event (iOS,Android)
   *          b beacon region enter/leave event (iOS)
   *          r response to a reportLocation cmd message (iOS,Android)
   *          u manual publish requested by the user (iOS,Android)
   *          t timer based publish in move move (iOS)
   *          v updated by Settings/Privacy/Locations Services/System Services/Frequent Locations monitoring (iOS)
   * @param tid Tracker ID used to display the initials of a user (iOS,Android/string/optional) required for http mode
   * @param tst UNIX epoch timestamp in seconds of the location fix (iOS,Android/integer/epoch/required)
   * @param vac vertical accuracy of the alt element (iOS/integer/meters/optional)
   * @param vel velocity (iOS,Android/integer/kmh/optional)
   * @param beacon
   * @param request
   * @param steps
   */
  case class OwnTracksPayload(
                               _type: String,
                               BSSID: Option[String],
                               SSID: Option[String],
                               acc: Option[Int],
                               alt: Option[Int],
                               batt: Option[Int],
                               bs: Option[Int],
                               conn: Option[String],
                               created_at: Long,
                               lat: Option[Double],
                               lon: Option[Double],
                               m: Option[Int],
                               t: Option[String],
                               tid: Option[String],
                               tst: Option[Long],
                               vac: Option[Int],
                               vel: Option[Int],

                               // iOS-only
                               beacon: Option[String],
                               request: Option[String],
                               steps: Option[String]
                             ) {
    // FIXME: get the zoneid from the lat/lon?
    def createdTime: ZonedDateTime = ZonedDateTime.ofInstant(Instant.ofEpochSecond(created_at), ZoneId.systemDefault())
  }

  object JsonFormat extends DefaultJsonProtocol {
    implicit val ownTracksPayloadFormat: JsonFormat[OwnTracksPayload] = jsonFormat20(OwnTracksPayload)
  }
}

