package me.micseydel.actor

import me.micseydel.actor.VaultPathAdapter.VaultPathUpdatedEvent
import me.micseydel.actor.ollama.WhiteSpaceAddingExperimentActor
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext, Tinkerer}

object LLMTinkeringActor {
  sealed trait Message

  private case class ReceivePathUpdatedEvent(event: VaultPathUpdatedEvent) extends Message

  val Folder = "llmtinkering"
  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor(71, 255, 84), "🧪").setup { context =>
    implicit val c: TinkerContext[_] = context

    context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.SubscribeSubdirectory(
      Folder,
      context.messageAdapter(ReceivePathUpdatedEvent)
    )

    Tinker.withMessages {
      case ReceivePathUpdatedEvent(event) =>
        event match {
          case VaultPathAdapter.PathCreatedEvent(path) =>
            val notename = path.path.getFileName.toString
            context.actorContext.log.info(s"Path $path created, spawning WhiteSpaceAddingExperimentActor for [[$notename]]")
            context.castAnonymous(WhiteSpaceAddingExperimentActor(notename))
            Tinker.steadily
          case VaultPathAdapter.PathModifiedEvent(_) | VaultPathAdapter.PathDeletedEvent(_) =>
            context.actorContext.log.debug(s"Ignoring $event")
            Tinker.steadily
        }
    }
  }
}
