package me.micseydel.actor.inactive

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.vault.persistence.NoteRef

object TemplateBasicEmptyModelActorOrSpirit {
  sealed trait Message

  val NoteName = ""
  val Emoji = ""

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(NoteName, TinkerColor.random(), Emoji) { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    implicit val nr: NoteRef = noteRef

    Tinker.unhandled
  }

  def noteWatcher()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer(NoteName, TinkerColor.random(), Emoji, ???) { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    implicit val nr: NoteRef = noteRef

    Tinker.unhandled
  }

  def basic()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.unhandled
  }

  def actor(): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.unhandled
  }
}


object ExampleForLLMs {
  sealed trait Message

  case class ReceiveNotePing(ping: Ping) extends Message

  //

  def NoteName: String = ???
  def Emoji: String = ???

  def noteWatcher()(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](NoteName, TinkerColor.random(), Emoji, ReceiveNotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[?] = context
      implicit val nr: NoteRef = noteRef

      Tinker.receiveMessage {
        case ReceiveNotePing(_) =>
          Tinker.steadily
      }
    }
}
