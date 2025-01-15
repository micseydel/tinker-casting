package me.micseydel.actor

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import me.micseydel.actor.VaultPathAdapter.VaultPathUpdatedEvent
import me.micseydel.actor.FolderWatcherActor.PathUpdatedEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker}
import me.micseydel.vault.VaultPath

import java.nio.file.Path

/**
 * Provides an easy way to subscribe to watch for updates to \$VAULT_ROOT/_actor_notes/SUBSCRIBED
 */
object ActorNotesFolderWatcherActor {

  val ActorNotesSubdirectory: String = "_actor_notes"

  // mailbox

  sealed trait Message

  final case class StartTinkering(tinker: Tinker) extends Message
  final case class Subscribe(subdirectory: String, replyTo: SpiritRef[VaultPathUpdatedEvent]) extends Message

  // behavior

  def apply(vaultPath: VaultPath): Ability[Message] = Behaviors.setup { context =>
    val actorNotesFolder: Path = vaultPath.resolve(ActorNotesFolderWatcherActor.ActorNotesSubdirectory)
    Behaviors.withStash(20) { stash =>
      Behaviors.receiveMessage {
        case StartTinkering(tinker) =>
          context.log.info("Starting tinkering...")
          stash.unstashAll(initialized(Map.empty)(tinker, actorNotesFolder))
        case other =>
          context.log.info("Not ready yet, stashing...")
          stash.stash(other)
          Behaviors.same
      }
    }
  }

  private def initialized(folders: Map[String, SpiritRef[FolderWatcherActor.Message]])(implicit Tinker: Tinker, actorNotesFolder: Path): Ability[Message] = Tinker.setup { context =>
    Tinker.withMessages {
      case StartTinkering(_) =>
        context.actorContext.log.warn("Did not expect a StartTinkering message after initialization, ignoring")
        Tinker.steadily

      case Subscribe(subdirectory, replyTo) =>
        folders.get(subdirectory) match {
          case Some(_) =>
            context.actorContext.log.error(s"Subdirectory $subdirectory requested twice, need to add requester tracking to figure out if it's a bug or just needs to be ignored")
            Tinker.steadily

          case None =>
            context.actorContext.log.info(s"Creating new folder watcher for subdirectory $subdirectory with subscriber ${replyTo.actorPath}")
            val actorNotesFolderWatcher = context.cast(
              FolderWatcherActor(
                actorNotesFolder.resolve(subdirectory),
                context.castAnonymous(VaultPathAdapter(actorNotesFolder, replyTo)).underlying
              ).behavior,
              s"FolderWatcherActor_for_$subdirectory"
            )
            initialized(folders.updated(subdirectory, actorNotesFolderWatcher))
        }
    }
  }
}

object VaultPathAdapter {
  sealed trait VaultPathUpdatedEvent {
    def path: VaultPath
  }

  case class PathCreatedEvent(path: VaultPath) extends VaultPathUpdatedEvent

  case class PathModifiedEvent(path: VaultPath) extends VaultPathUpdatedEvent

  case class PathDeletedEvent(path: VaultPath) extends VaultPathUpdatedEvent

  object VaultPathUpdatedEvent {
    def apply(actorNotesFolder: Path)(event: PathUpdatedEvent): Validated[String, VaultPathUpdatedEvent] = {
      def validatedVaultPath(path: Path): Validated[String, VaultPath] = VaultPath(actorNotesFolder.resolve(path))
      event match {
        case FolderWatcherActor.PathCreatedEvent(path) =>
          validatedVaultPath(path).map(PathCreatedEvent)
        case FolderWatcherActor.PathModifiedEvent(path) =>
          validatedVaultPath(path).map(PathModifiedEvent)
        case FolderWatcherActor.PathDeletedEvent(path) =>
          validatedVaultPath(path).map(PathDeletedEvent)
      }
    }
  }

  def apply(actorFolderPath: Path, forwardTo: SpiritRef[VaultPathUpdatedEvent]): Behavior[PathUpdatedEvent] = Behaviors.receive { (context, message) =>
    context.log.info("Running experimental adapter...")

    VaultPathUpdatedEvent(actorFolderPath)(message) match {
      case Valid(event) =>
        forwardTo.underlying ! event
      case Invalid(msg) =>
        context.log.warn(s"Error handling VaultPathUpdatedEvent $msg")
    }

    Behaviors.same
  }
}