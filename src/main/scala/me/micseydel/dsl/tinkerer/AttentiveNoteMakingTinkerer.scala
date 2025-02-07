package me.micseydel.dsl.tinkerer

import me.micseydel.actor.ActorNotesFolderWatcherActor
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

object AttentiveNoteMakingTinkerer {
  def apply[M, PR <: M](noteName: String, color: TinkerColor, emoji: String, pingReceiver: Ping => PR)(ability: (TinkerContext[M], NoteRef) => Ability[M])(implicit Tinker: Tinker): Ability[M] =
    NoteMakingTinkerer[M](noteName, color, emoji, Some("_actor_notes")) { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context
      context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.SubscribeNoteRef(noteRef, context.messageAdapter(pingReceiver))
      ability(context, noteRef)
    }
}
