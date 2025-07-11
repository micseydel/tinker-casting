package me.micseydel.actor

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.Purple
import me.micseydel.dsl._
import me.micseydel.util.TimeUtil

import java.time.LocalDate

// copy-pasted from DailyNotesRouter with light changes; should def refactor on yearly
object MonthlyNotesRouter {
  case class Envelope[+M](message: M, localDate: LocalDate)

  // initializers

  def apply[M](abilityGenerator: (Month, TinkerColor, String) => (String, Ability[M]))(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinkerer(Purple, "☸️").setup { context =>
    context.actorContext.log.info(s"Creating spirit lookup")
    val freshLookUpSpiritByMonth = LookUpSpiritByKey[Month, M] { (context, forMonth) =>
      val (noteName, abilityForMonth) = abilityGenerator(forMonth, Purple, "📝️")
      context.actorContext.log.info(s"Casting $forMonth")
      context.cast(abilityForMonth, forMonth.isoMonth)
    }

    generic(freshLookUpSpiritByMonth)
  }

  // behavior

  private def generic[M](lookUpSpiritByMonth: LookUpSpiritByKey[Month, M])(implicit Tinker: Tinker): Ability[Envelope[M]] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[Envelope[M]] = context
    message match {
      case Envelope(message, captureTime) =>
        context.actorContext.log.info(s"Received message for captureTime $captureTime, looking up spirit...")
        val (potentiallyUpdatedLookup, spirit) = lookUpSpiritByMonth.:?>(Month(captureTime))(context)
        context.actorContext.log.info(s"Forwarding message for captureTime $captureTime to $spirit")
        spirit !! message
        generic(potentiallyUpdatedLookup)
    }
  }

  final class Month(val isoMonth: String) extends AnyVal

  object Month {
    def apply(localDate: LocalDate): Month = new Month(TimeUtil.IsoMonthFormatter.format(localDate))
  }
}

class LookUpSpiritByKey[K, S](
                                      map: Map[K, SpiritRef[S]],
                                      caster: (TinkerContext[_], K) => SpiritRef[S]
                                    ) {

  def :?>(key: K)(implicit tinkerContext: TinkerContext[_]): (LookUpSpiritByKey[K, S], SpiritRef[S]) = lookup(key)

  override def toString: String = s"LookUpSpiritByKey($map)"

  //

  private def lookup(key: K)(implicit tinkerContext: TinkerContext[_]): (LookUpSpiritByKey[K, S], SpiritRef[S]) = {
    map.get(key) match {
      case Some(existing) =>
        (this, existing)

      case None =>
        val fresh = caster(tinkerContext, key)
        (new LookUpSpiritByKey(map.updated(key, fresh), caster), fresh)
    }
  }
}

object LookUpSpiritByKey {
  def apply[K, S](caster: (TinkerContext[_], K) => SpiritRef[S]): LookUpSpiritByKey[K, S] = {
    new LookUpSpiritByKey[K, S](Map.empty, caster)
  }
}
