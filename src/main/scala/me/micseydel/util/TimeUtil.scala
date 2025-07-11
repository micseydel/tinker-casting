package me.micseydel.util

import me.micseydel.dsl.TinkerClock

import java.time.{Duration, Instant, LocalDate, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationLong, FiniteDuration}

object TimeUtil {
  val WithinDayDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("h:mm:ssa")
  val WithinDay24HourDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
  val MonthDayTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val MonthDayTimeSecondFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  def zonedDateTimeToTimeWithinDay(zonedDateTime: ZonedDateTime): String = zonedDateTime.format(WithinDayDateTimeFormatter)

  def zonedDateTimeToHourMinWithinDay(zonedDateTime: ZonedDateTime): String = zonedDateTime.format(DateTimeFormatter.ofPattern("HH:mm"))

  def zonedDateTimeToTimeWithinDay24Hour(zonedDateTime: ZonedDateTime): String = zonedDateTime.format(WithinDay24HourDateTimeFormatter)

  def localDateToISO8601Date(localDate: LocalDate): String =
    localDate.format(DateTimeFormatter.ISO_LOCAL_DATE)

  def timeSince(earlier: ZonedDateTime)(implicit tinkerClock: TinkerClock): Duration = {
    Duration.between(earlier, tinkerClock.now(earlier.getZone))
  }

  def timeUntil(hour: Int, minute: Int): FiniteDuration = {
    require(hour >= 0 && hour <= 23, "Hour must be in [0, 23]")
    require(minute >= 0 && minute <= 23, "Minute must be in [0, 59]")

    val now = ZonedDateTime.now(ZoneId.systemDefault())
    var targetTime = now.withHour(hour).withMinute(minute).withSecond(0).withNano(0)

    // If the target time is already past or it's the current time, then get the duration until the next day's target time.
    if (now.isAfter(targetTime) || now.isEqual(targetTime)) {
      targetTime = targetTime.plusDays(1)
    }

    val duration = java.time.Duration.between(now, targetTime)
    duration.toMillis.millis // Convert to Scala's FiniteDuration
  }

  def timeUntil(hour: Int, minute: Int, now: ZonedDateTime): FiniteDuration = {
    require(hour >= 0 && hour <= 23, "Hour must be in [0, 23]")
    require(minute >= 0 && minute <= 23, "Minute must be in [0, 59]")

    var targetTime = now.withHour(hour).withMinute(minute).withSecond(0).withNano(0)

    // If the target time is already past or it's the current time, then get the duration until the next day's target time.
    if (now.isAfter(targetTime) || now.isEqual(targetTime)) {
      targetTime = targetTime.plusDays(1)
    }

    val duration = java.time.Duration.between(now, targetTime)
    duration.toMillis.millis // Convert to Scala's FiniteDuration
  }

  def between(from: ZonedDateTime, to: ZonedDateTime): FiniteDuration = {
    FiniteDuration(Duration.between(from, to).toMillis, TimeUnit.MILLISECONDS)
  }

  def daysBetween(firstDay: LocalDate, secondDay: LocalDate): Long = {
    // FYI Duration.between(forDay, newDay) -> `java.time.temporal.UnsupportedTemporalTypeException: Unsupported unit: Seconds`
    ChronoUnit.DAYS.between(firstDay, secondDay)
  }

  def daysSince(from: LocalDate)(implicit tinkerClock: TinkerClock): Long = {
    ChronoUnit.DAYS.between(from, tinkerClock.now())
  }

  val EarliestZonedDateTime: ZonedDateTime = ZonedDateTime.ofInstant(Instant.ofEpochMilli(0), ZoneId.systemDefault())

  def zonedDateTimeToISO8601(zonedDateTime: ZonedDateTime): String =
    zonedDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  def zonedDateTimeToISO8601Date(zonedDateTime: ZonedDateTime): String =
    zonedDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE)

  def localDateTimeToISO8601Date(localDate: LocalDate): String =
    localDate.format(DateTimeFormatter.ISO_LOCAL_DATE)

  //  def zonedDateTimeToISO8601Date(): String =
  //    ZonedDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE)

  def zonedDateTimeToISO8601Date(tinkerClock: TinkerClock): String =
    tinkerClock.now().format(DateTimeFormatter.ISO_LOCAL_DATE)

  /**
   * e.g. 02:37.0
   */
  def getFormattedDuration(duration: Double): String = {
    val minutes = (duration / 60).toInt
    val seconds = (duration % 60).toInt
    val subSeconds = duration - duration.toInt
    f"$minutes%02d:$seconds%02d.$subSeconds%01.0f"
  }

  def minuteToHoursAndMinutes(mins: Long): String = {
    val hours = mins / 60
    val minutes = mins % 60
    s"$hours hours and $minutes minutes"
  }

  val IsoMonthFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM")
}
