package me.micseydel.actor

import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability

object AQITrackerActor {
  sealed trait Message

  // FIXME:
  //  - purple air
  //  - air gradient
  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.unhandled
  }
}
