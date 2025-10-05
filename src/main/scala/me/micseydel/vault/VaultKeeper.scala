package me.micseydel.vault

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.FolderWatcherActor
import me.micseydel.util.FileSystemUtil
import me.micseydel.vault.persistence.{BasicJsonRef, BasicNoteRef, JsonRef, NoteRef}
import me.micseydel.{Common, NoOp}

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

/**
 * State is purely in-memory.
 *
 * This has no dependencies, doesn't do I/O on startup.
 *
 * FIXME: RequestAttachmentsContents change, keep this SUPER light-weight
 */
object VaultKeeper {

  // FIXME: ExclusiveNoteRequest and a specific kind of error for when that happens
  // mailbox
  sealed trait Message
  final case class RequestExclusiveNoteRef(noteId: String, replyTo: ActorRef[NoteRefResponse], subdirectory: Option[String] = None) extends Message
  final case class RequestExclusiveJsonRef(filename: String, replyTo: ActorRef[JsonRefResponse]) extends Message

  final case class RequestAttachmentsContents(attachmentNames: List[String], replyTo: ActorRef[Either[String, List[Array[Byte]]]]) extends Message
  final case class RequestAttachmentContents(attachmentName: String, replyTo: ActorRef[(String, Try[Array[Byte]])]) extends Message

  final case class SubscribeUpdatesForNote(subscriber: ActorRef[Ping], noteId: NoteId, subdirectory: Option[String] = None) extends Message
  final case class SubscribeUpdatesForFolder(subscriber: ActorRef[FolderWatcherActor.PathUpdatedEvent], subdirectory: String) extends Message

  // outgoing messages

  case class NoteRefResponse(noteName: String, noteRefOrWhyNot: Either[String, NoteRef])

  case class JsonRefResponse(jsonName: String, jsonRefOrWhyNot: Either[String, JsonRef])

  // behavior

  def apply(vaultPath: VaultPath): Behavior[Message] = setup(vaultPath)

  private def setup(vaultPath: VaultPath): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("VaultKeeper started")

    val watcherLookup = new LookUpActorByKey[String, FolderWatcherWithSubscribers.Message](Map.empty, { (context, subdirectory) =>
      // FIXME: ideally these names would be tracked better
      context.spawnAnonymous(FolderWatcherWithSubscribers(vaultPath.resolve(subdirectory)))
    })

    behavior(vaultPath, vaultPath.resolve("json"), Set.empty, Set.empty, Map.empty, watcherLookup)
  }

  private def behavior(
                        vaultPath: VaultPath,
                        jsonPath: Path,
                        noteRefCache: Set[String],
                        jsonRefCache: Set[String],
                        rootSubscribers: Map[NoteId, ActorRef[Ping]],
                        watcherLookup: LookUpActorByKey[String, FolderWatcherWithSubscribers.Message]
                      ): Behavior[Message] = Behaviors.receive { (context, message) =>
    implicit val actorContext: ActorContext[Message] = context
    message match {
      case msg@RequestExclusiveNoteRef(noteId, replyTo, subdirectory) =>
        context.log.debug(s"Received message $msg")
        if (noteRefCache.contains(noteId)) {
          val msg = s"NoteId $noteId was requested but already claimed"
          context.log.warn(msg)
          replyTo ! NoteRefResponse(noteId, Left(msg))
          Behaviors.same
        } else {
          context.log.debug(s"Sending back NoteRef for $noteId")
          val reply = NoteRefResponse(noteId, Right(new BasicNoteRef(NoteId(noteId), vaultPath, subdirectory)))
          context.log.debug(s"Sending back $reply to $replyTo")
          replyTo ! reply
          behavior(vaultPath,
            jsonPath,
            //            actorRefCache,
            noteRefCache + noteId,
            jsonRefCache,
            rootSubscribers,
            watcherLookup
          )
        }

      case RequestExclusiveJsonRef(jsonName, replyTo) =>
        if (jsonRefCache.contains(jsonName)){
          replyTo ! JsonRefResponse(jsonName, Left("Redundant RequestExclusiveJsonRef"))
          Behaviors.same
        } else {
          val jsonRef = new BasicJsonRef(jsonName, jsonPath)
          replyTo ! JsonRefResponse(jsonName, Right(jsonRef))
          behavior(
            vaultPath,
            jsonPath,
            noteRefCache,
            jsonRefCache + jsonName,
            rootSubscribers,
            watcherLookup
          )
        }

      case RequestAttachmentsContents(attachmentNames, replyTo) =>
        context.log.info(s"About to fetch attachments: $attachmentNames")
        val paths = attachmentNames.map(vaultPath.resolve("deliberate_knowledge_accretion_attachments").resolve(_))

        Try(paths.map(FileSystemUtil.getPathBytes)) match {
          case Failure(exception) =>
            context.log.error("This is probably the dropped left", exception)
            replyTo ! Left(Common.getStackTraceString(exception))
          case Success(byteArrays) =>
            context.log.info(s"Sending ${byteArrays.length} byte arrays to ${replyTo.path}, total bytes ${byteArrays.map(_.length).sum}")
            replyTo ! Right(byteArrays)
        }

        Behaviors.same

      case RequestAttachmentContents(attachmentName, replyTo) =>
        val path = vaultPath.resolve(attachmentName) // FIXME: clarify this being different from the above!
        replyTo ! (attachmentName -> Try(FileSystemUtil.getPathBytes(path)))
        Behaviors.same

      case SubscribeUpdatesForNote(subscriber, noteId, maybeSubdirectory) =>
        val subdirectory = maybeSubdirectory.getOrElse("") // root is ""
        val maybeUpdatedLookup = watcherLookup :?> subdirectory match {
          case (latest, watcher) =>
            watcher ! FolderWatcherWithSubscribers.SubscribeUpdatesForNote(subscriber, noteId)
            latest
        }

        behavior(vaultPath,
          jsonPath,
          noteRefCache,
          jsonRefCache,
          rootSubscribers.updated(noteId, subscriber),
          maybeUpdatedLookup
        )

      case SubscribeUpdatesForFolder(subscriber, subdirectory) =>
        val maybeUpdatedLookup = watcherLookup :?> subdirectory match {
          case (latest, watcher) =>
            watcher ! FolderWatcherWithSubscribers.SubscribeUpdatesForFolder(subscriber)
            latest
        }

        behavior(vaultPath,
          jsonPath,
          noteRefCache,
          jsonRefCache,
          rootSubscribers,
          maybeUpdatedLookup
        )
    }
  }
}

private object FolderWatcherWithSubscribers {
  sealed trait Message

  final case class SubscribeUpdatesForNote(subscriber: ActorRef[Ping], noteId: NoteId) extends Message
  final case class SubscribeUpdatesForFolder(subscriber: ActorRef[FolderWatcherActor.PathUpdatedEvent]) extends Message

  private case class ReceivePathUpdatedEvent(event: FolderWatcherActor.PathUpdatedEvent) extends Message

  def apply(path: Path): Behavior[Message] = Behaviors.setup { context =>
    context.spawn(FolderWatcherActor(path, context.messageAdapter(ReceivePathUpdatedEvent)), "FolderWatcherActor")
    behavior(Map.empty, Set.empty)
  }

  private def behavior(noteSubscribers: Map[NoteId, ActorRef[Ping]], folderSubscribers: Set[ActorRef[FolderWatcherActor.PathUpdatedEvent]]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case SubscribeUpdatesForNote(subscriber, noteId) =>
        for (replacing <- noteSubscribers.get(noteId) if replacing != subscriber) {
          context.log.warn(s"Replacing existing subscriber ($replacing) with $subscriber (this was not expected; only one subscriber should ever happen)")
        }

        behavior(noteSubscribers.updated(noteId, subscriber), folderSubscribers)

      case ReceivePathUpdatedEvent(event) =>
        folderSubscribers.foreach(_ ! event)
        event match {
          case FolderWatcherActor.PathCreatedEvent(path) => context.log.info(s"Ignoring creation of $path")
          case FolderWatcherActor.PathDeletedEvent(path) => context.log.info(s"Ignoring deletion of $path")
          case FolderWatcherActor.PathModifiedEvent(path) =>
            val filename = path.getFileName.toString
            if (filename.endsWith(".md")) {
              val noteId = NoteId(filename.dropRight(3))
              noteSubscribers.get(noteId) match {
                case None => context.log.info(s"Ignoring update of $path, no subscriber (of ${noteSubscribers.size} subscribers)")
                case Some(subscriber) =>
                  if (context.log.isDebugEnabled) {
                    context.log.debug(s"Pinging $subscriber")
                  }

                  subscriber ! NoOp
              }
            }
        }
        Behaviors.same

      case SubscribeUpdatesForFolder(subscriber) =>
        behavior(noteSubscribers, folderSubscribers + subscriber)
    }
  }
}

// copy-paste with modifications from LookUpSpiritByKey
private class LookUpActorByKey[K, S](
                                       map: Map[K, ActorRef[S]],
                                       caster: (ActorContext[?], K) => ActorRef[S]
                                     ) {

  def :?>(key: K)(implicit tinkerContext: ActorContext[?]): (LookUpActorByKey[K, S], ActorRef[S]) = lookup(key)

  override def toString: String = s"LookUpSpiritByKey($map)"

  //

  private def lookup(key: K)(implicit tinkerContext: ActorContext[?]): (LookUpActorByKey[K, S], ActorRef[S]) = {
    map.get(key) match {
      case Some(existing) =>
        (this, existing)

      case None =>
        val fresh = caster(tinkerContext, key)
        (new LookUpActorByKey(map.updated(key, fresh), caster), fresh)
    }
  }
}
