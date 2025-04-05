package me.micseydel.actor

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.Purple
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.util.TimeUtil
import spray.json.JsonFormat

import java.time.{LocalDate, ZonedDateTime}

object DailyNotesRouter {
  case class Envelope[M](message: M, localDate: LocalDate)

  object Envelope {
    def apply[M](message: M, zonedDateTime: ZonedDateTime): Envelope[M] = Envelope(message, zonedDateTime.toLocalDate)
  }

  // initializers

  def apply[T](baseNoteName: String, jsonName: String, jsonFormat: JsonFormat[T], toMarkdown: (List[T], TinkerClock) => String)(implicit Tinker: Tinker): Ability[Envelope[DailyMarkdownFromPersistedMessagesActor.Message[T]]] = Tinkerer(Purple, "☸️").setup { context =>
    context.actorContext.log.info(s"Creating daily router for base note name $baseNoteName")
    // in the case of DailyMarkdownFromPersistedMessagesActor, we discard the day (for now at least)
    val abilityForDay: (LocalDate, TinkerColor, String) => (String, Ability[DailyMarkdownFromPersistedMessagesActor.Message[T]]) = (captureDate: LocalDate, color: TinkerColor, emoji: String) => {
      val isoDate = TimeUtil.localDateTimeToISO8601Date(captureDate)
      val noteName = s"$baseNoteName ($isoDate)"
      noteName -> DailyMarkdownFromPersistedMessagesActor(
        noteName,
        color, emoji,
        s"${jsonName}_$isoDate",
        jsonFormat,
        toMarkdown
      )
    }

    apply[DailyMarkdownFromPersistedMessagesActor.Message[T]](abilityForDay)
  }

  def apply[M](abilityGenerator: (LocalDate, TinkerColor, String) => (String, Ability[M]))(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinkerer(Purple, "☸️").setup { context =>
    context.actorContext.log.info(s"Creating spirit lookup")
    val freshLokUpSpiritByDay = LookUpSpiritByDay[M] { (context, captureDate) =>
      val opaqacity = TimeUtil.daysSince(captureDate)(context.system.clock) match {
        case 0 => 1.0
        case 1 => 0.7
        case 2 => 0.3
        case other =>
          context.actorContext.log.warn(s"$other days since $captureDate")
          0.1
      }

      val (noteName, abilityForDay) = abilityGenerator(captureDate, Purple.copy(o = opaqacity), "📝️")

      context.cast(abilityForDay, TimeUtil.localDateTimeToISO8601Date(captureDate))
    }

    val today = context.system.clock.today()
    val lastThreeDays = Seq(0, 1, 2).map(daysAgo => today.minusDays(daysAgo))

    implicit val c: TinkerContext[_] = context
    // FIXME: HACK HACK HACK there should be a step between casting an actor and describing it in the tinkerbrain graph view
    val lookUpSpiritByDay = lastThreeDays.foldRight(freshLokUpSpiritByDay) { (day, lookup)=>
      lookup :?> day match {
        case (updatedLookup, _) =>
          updatedLookup
      }
    }

    generic(lookUpSpiritByDay)
  }

  // behavior

  private def generic[M](lookUpSpiritByDay: LookUpSpiritByDay[M])(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[Envelope[M]] = context
    message match {
      case Envelope(message, captureTime) =>
        context.actorContext.log.info(s"Received message for captureTime $captureTime, looking up spirit...")
        val (potentiallyUpdatedLookup, spirit) = lookUpSpiritByDay.:?>(captureTime)(context)
        context.actorContext.log.info(s"Forwarding message for captureTime $captureTime to $spirit")
        spirit !! message
        generic(potentiallyUpdatedLookup)
    }
  }
}

private class LookUpSpiritByDay[S] private(
                                            map: Map[LocalDate, SpiritRef[S]],
                                            caster: (TinkerContext[_], LocalDate) => SpiritRef[S]
                                          ) {

  def :?>(zonedDateTime: ZonedDateTime)(implicit tinkerContext: TinkerContext[_]): (LookUpSpiritByDay[S], SpiritRef[S]) = lookup(zonedDateTime.toLocalDate)

  def :?>(localDate: LocalDate)(implicit tinkerContext: TinkerContext[_]): (LookUpSpiritByDay[S], SpiritRef[S]) = lookup(localDate)

  //

  private def lookup(localDate: LocalDate)(implicit tinkerContext: TinkerContext[_]): (LookUpSpiritByDay[S], SpiritRef[S]) = {
    map.get(localDate) match {
      case Some(existing) =>
        (this, existing)

      case None =>
        val fresh = caster(tinkerContext, localDate)
        (new LookUpSpiritByDay(map.updated(localDate, fresh), caster), fresh)
    }
  }
}

private object LookUpSpiritByDay {
  def apply[S](caster: (TinkerContext[_], LocalDate) => SpiritRef[S]): LookUpSpiritByDay[S] = {
    new LookUpSpiritByDay[S](Map.empty, caster)
  }
}
