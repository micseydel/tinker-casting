package me.micseydel.actor

import me.micseydel.Common
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.Purple
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerContext, Tinkerer}
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
    val abilityForDay: LocalDate => Ability[DailyMarkdownFromPersistedMessagesActor.Message[T]] = (captureDate: LocalDate) => {
      val isoDate = TimeUtil.localDateTimeToISO8601Date(captureDate)
      DailyMarkdownFromPersistedMessagesActor(
        s"$baseNoteName ($isoDate)",
        s"${jsonName}_$isoDate",
        jsonFormat,
        toMarkdown
      )
    }

    apply[DailyMarkdownFromPersistedMessagesActor.Message[T]](abilityForDay)
  }

  def apply[M](ability: LocalDate => Ability[M])(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinker.setup { context =>
    context.actorContext.log.info(s"Creating spirit lookup")
    val lookUpSpiritByDay = LookUpSpiritByDay[M] { (context, captureDate) =>
      context.cast(ability(captureDate), TimeUtil.localDateTimeToISO8601Date(captureDate))
    }

    generic(lookUpSpiritByDay)
  }

  // behavior

  def behavior[M](lookUpSpiritByDay: LookUpSpiritByDay[DailyMarkdownFromPersistedMessagesActor.Message[M]])(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[Envelope[M]] = context
    message match {
      case Envelope(message, captureTime) =>
        val (potentiallyUpdatedLookup, spirit) = lookUpSpiritByDay.:?>(captureTime)(context)
        context.actorContext.log.info(s"Fowarding message for captureTime $captureTime to $spirit")
        spirit !! DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(message)
        behavior(potentiallyUpdatedLookup)
    }
  }

  private def generic[M](lookUpSpiritByDay: LookUpSpiritByDay[M])(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[Envelope[M]] = context
    message match {
      case Envelope(message, captureTime) =>
        context.actorContext.log.info(s"Received message for captureTime $captureTime, looking up spirit...")
        val (potentiallyUpdatedLookup, spirit) = lookUpSpiritByDay.:?>(captureTime)(context)
        context.actorContext.log.info(s"Fowarding message for captureTime $captureTime to $spirit")
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
