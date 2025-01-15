package me.micseydel.actor.perimeter.fitbit

import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.RawHeader
//import me.micseydel.util.ShapelessJsonSupport
import spray.json.{DefaultJsonProtocol, JsNumber, JsString, JsValue, JsonFormat, RootJsonFormat, deserializationError}

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.MILLISECONDS

object FitbitModel {

  import java.time.ZonedDateTime
  import scala.concurrent.duration.Duration

  sealed trait SleepLevel

  case object Light extends SleepLevel

  case object Wake extends SleepLevel

  case object Deep extends SleepLevel

  case object Rem extends SleepLevel

  case object Restless extends SleepLevel

  case object Asleep extends SleepLevel

  case object Awake extends SleepLevel

  case class SleepDataPoint(dateTime: ZonedDateTime, level: SleepLevel, seconds: Int)

  case class LevelSummary(count: Int, minutes: Int,
                          //                          thirtyDayAvgMinutes: Int
                         )

  case class LevelsData(data: Seq[SleepDataPoint],
                        //                        shortData: Seq[SleepDataPoint],
                        summary: Map[String, LevelSummary])

  case class SleepEntry(
                         dateOfSleep: String,
                         duration: Duration,
                         efficiency: Int,
                         endTime: ZonedDateTime,
                         //                         infoCode: Int,
                         isMainSleep: Boolean,
                         levels: LevelsData,
                         logId: Long,
                         //                         logType: String,
                         minutesAfterWakeup: Int,
                         minutesAsleep: Int,
                         minutesAwake: Int,
                         minutesToFallAsleep: Int,
                         startTime: ZonedDateTime,
                         timeInBed: Int //,
                         //                         `type`: String
                       )

  case class SleepSummary(
                           //                           stages: Map[String, Int],
                           totalMinutesAsleep: Int, totalSleepRecords: Int, totalTimeInBed: Int)

  // FIXME: the underlying sleep report should use nonemptylist
  case class SleepReport(sleep: Seq[SleepEntry], summary: SleepSummary)

  object SleepJsonProtocol extends DefaultJsonProtocol
//    with ShapelessJsonSupport
    {

    implicit object ZonedDateTimeFormat extends JsonFormat[ZonedDateTime] {
      private val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")

      def write(obj: ZonedDateTime): JsValue = JsString(obj.toString)

      def read(json: JsValue): ZonedDateTime = json match {
        case JsString(str) =>
          try {
            ZonedDateTime.parse(str, dateTimeFormatter.withZone(ZoneId.systemDefault()))
          } catch {
            case _: java.time.format.DateTimeParseException =>
              ZonedDateTime.parse(str)
          }
        case _ => deserializationError("Expected ZonedDateTime as JsString")
      }
    }

    implicit object DurationFormat extends JsonFormat[Duration] {
      def write(obj: Duration): JsValue = JsNumber(obj.toMillis)

      def read(json: JsValue): Duration = json match {
        case JsNumber(num) => Duration(num.toLong, MILLISECONDS)
        case _ => deserializationError("Expected Duration as JsNumber (milliseconds)")
      }
    }

    implicit object SleepLevelFormat extends JsonFormat[SleepLevel] {
      def write(obj: SleepLevel): JsValue = JsString(obj.toString.toLowerCase)

      def read(json: JsValue): SleepLevel = json match {
        case JsString("light") => Light
        case JsString("wake") => Wake
        case JsString("deep") => Deep
        case JsString("rem") => Rem
        case JsString("restless") => Restless
        case JsString("asleep") => Asleep
        case JsString("awake") => Awake
        case other => deserializationError(s"Expected a valid sleep level {restless, asleep, awake, light, wake, deep, rem} but got $other")
      }
    }

    implicit val sleepDataPointFormat: RootJsonFormat[SleepDataPoint] = jsonFormat3(SleepDataPoint)
    implicit val levelSummaryFormat: JsonFormat[LevelSummary] = jsonFormat2(LevelSummary)
    implicit val levelsDataFormat: RootJsonFormat[LevelsData] = jsonFormat2(LevelsData)
    implicit val sleepEntryFormat: RootJsonFormat[SleepEntry] = jsonFormat13(SleepEntry)
    implicit val sleepSummaryFormat: RootJsonFormat[SleepSummary] = jsonFormat3(SleepSummary)
    implicit val sleepReportFormat: JsonFormat[SleepReport] = jsonFormat2(SleepReport)
  }

  private[fitbit] case class Auth(access_token: String, refresh_token: String) {
    def httpHeadersBearer: List[HttpHeader] = List(RawHeader("Authorization", s"Bearer $access_token"))

    // FIXME: https://dev.fitbit.com/build/reference/web-api/troubleshooting-guide/oauth2-tutorial/
    // "Basic " + base64encode(client_id + ":" + client_secret)
//    def httpHeadersBasic: List[HttpHeader] = List(RawHeader("Authorization", s"Basic "))
  }

  private[fitbit] object AuthJsonProtocol extends DefaultJsonProtocol {
    implicit val authFormat: RootJsonFormat[Auth] = jsonFormat2(Auth)
  }
}
