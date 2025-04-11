package me.micseydel.actor

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.PathUpdatedEvent
import me.micseydel.actor.VaultPathAdapter.VaultPathUpdatedEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.VaultPath
import me.micseydel.vault.persistence.NoteRef

import java.nio.file.Path
import scala.annotation.unused
import scala.util.{Failure, Success}

/**
 * Provides an easy way to subscribe to watch for updates to \$VAULT_ROOT/_actor_notes/SUBSCRIBED
 */
object ActorNotesFolderWatcherActor {

  val ActorNotesSubdirectory: String = "_actor_notes"

  type Ping = NoOp.type

  // mailbox

  sealed trait Message

  final case class StartTinkering(tinker: Tinker) extends Message
  final case class SubscribeSubdirectory(subdirectory: String, replyTo: SpiritRef[VaultPathUpdatedEvent]) extends Message
  final case class SubscribeNoteRef(noteRef: NoteRef, replyTo: SpiritRef[Ping]) extends Message
  final case class ReceiveVaultPathUpdatedEvent(event: VaultPathUpdatedEvent) extends Message

  // behavior

  def apply(vaultPath: VaultPath): Ability[Message] = Behaviors.setup { context =>
    val actorNotesFolder: Path = vaultPath.resolve(ActorNotesFolderWatcherActor.ActorNotesSubdirectory)
    Behaviors.withStash(20) { stash =>
      Behaviors.receiveMessage {
        case StartTinkering(tinker) =>
          context.log.info("Starting tinkering...")
          stash.unstashAll(finishingInitialization(Map.empty, Map.empty)(tinker, actorNotesFolder))
        case other =>
          context.log.info("Not ready yet, stashing...")
          stash.stash(other)
          Behaviors.same
      }
    }
  }

  private def finishingInitialization(
                           folders: Map[String, SpiritRef[FolderWatcherActor.Message]],
                           topLevelNoteWatchers: Map[String, SpiritRef[Ping]]
                         )(implicit Tinker: Tinker, actorNotesFolder: Path): Ability[Message] = NoteMakingTinkerer("ActorNotesFolderWatcherActor", TinkerColor(25, 25, 25), "🦾") { (context, noteRef) =>
    @unused // driven by an internal thread
    val actorNotesFolderWatcher = context.spawn(
      FolderWatcherActor(
        actorNotesFolder,
        context.castAnonymous(VaultPathAdapter(actorNotesFolder, context.messageAdapter(ReceiveVaultPathUpdatedEvent))).underlying
      ),
      s"FolderWatcherActor_for_actor_notes"
    )

    implicit val nr: NoteRef = noteRef
    initialized(folders, topLevelNoteWatchers)
  }

  private def initialized(
                           folders: Map[String, SpiritRef[FolderWatcherActor.Message]],
                           topLevelNoteWatchers: Map[String, SpiritRef[Ping]]
                         )(implicit Tinker: Tinker, actorNotesFolder: Path, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    noteRef.setMarkdown {
      val formattedFolders = folders.map { case (folder, sub) =>
        s"- $folder ${sub.path.toString.drop(24).takeWhile(_ != '$')}"
      }.mkString("\n")

      val formattedTopLevelNoteWatchers = topLevelNoteWatchers.map { case (note, sub) =>
        s"- [[$note]] ${sub.path.toString.drop(24).takeWhile(_ != '$')}"
      }.mkString("\n")

      s"""- Generated ${context.system.clock.now()}
         |# Folders
         |
         |$formattedFolders
         |
         |# Note watchers
         |
         |$formattedTopLevelNoteWatchers
         |""".stripMargin
    } match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }

    Tinker.receiveMessage {
      case StartTinkering(_) =>
        context.actorContext.log.warn("Did not expect a StartTinkering message after initialization, ignoring")
        Tinker.steadily

      case SubscribeSubdirectory(subdirectory, replyTo) =>
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
            initialized(folders.updated(subdirectory, actorNotesFolderWatcher), topLevelNoteWatchers)
        }

      case SubscribeNoteRef(subscriberNoteRef, replyTo) =>
        val noteId = subscriberNoteRef.noteId.id
        topLevelNoteWatchers.get(noteId) match {
          case Some(existing) =>
            context.actorContext.log.warn(s"Ref $replyTo tried to subscribe for $noteId, but $existing had already subscribed; keeping the status quo")
            Tinker.steadily
          case None =>
            context.actorContext.log.info(s"Subscribing $noteId to $replyTo")
            initialized(folders, topLevelNoteWatchers.updated(noteId, replyTo))
        }

      case ReceiveVaultPathUpdatedEvent(event) =>
        event match {
          case VaultPathAdapter.PathModifiedEvent(path) =>
            path.noteName match {
              case None =>
                if (path.toString.endsWith("ollamaprompts")) {
                  context.actorContext.log.debug(s"it looks like a file in ollamaprompts was deleted $event")
                } else {
                  context.actorContext.log.warn(s"unable to handle event $event because a filename ending with .md was expected")
                }
              case Some(noteId) =>
                topLevelNoteWatchers.get(noteId) match {
                  case Some(subscriber) =>
                    implicit val c: TinkerContext[_] = context
                    subscriber !! NoOp
                  case None =>
                    context.actorContext.log.debug(s"No subscriber, ignoring path update for $path")
                }
            }

          case VaultPathAdapter.PathDeletedEvent(_) | VaultPathAdapter.PathCreatedEvent(_) =>
        }

        Tinker.steadily
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
    context.log.debug("Running experimental adapter...")

    VaultPathUpdatedEvent(actorFolderPath)(message) match {
      case Valid(event) =>
        forwardTo.underlying ! event
      case Invalid(msg) =>
        context.log.warn(s"Error handling VaultPathUpdatedEvent $msg")
    }

    Behaviors.same
  }
}
