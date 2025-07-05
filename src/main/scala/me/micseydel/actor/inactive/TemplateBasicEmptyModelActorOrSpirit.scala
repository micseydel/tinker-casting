package me.micseydel.actor.inactive

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability

object TemplateBasicEmptyModelActorOrSpirit {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.unhandled
  }

  def actor(): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.unhandled
  }
}
