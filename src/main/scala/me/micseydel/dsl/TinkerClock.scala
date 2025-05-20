package me.micseydel.dsl

import java.time.{LocalDate, ZoneId, ZonedDateTime}

trait TinkerClock {
  def now(): ZonedDateTime

  def now(zoneId: ZoneId): ZonedDateTime

  def today(): LocalDate
}

class TinkerClockImpl extends TinkerClock {
  def now(): ZonedDateTime = ZonedDateTime.now()

  def now(zoneId: ZoneId): ZonedDateTime = ZonedDateTime.now(zoneId)

  def today(): LocalDate = LocalDate.now()
}
