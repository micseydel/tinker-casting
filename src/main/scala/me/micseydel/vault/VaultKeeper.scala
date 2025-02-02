package me.micseydel.vault

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.Common
import me.micseydel.util.FileSystemUtil
import me.micseydel.vault.persistence.{BasicJsonRef, BasicNoteRef, JsonRef, NoteRef}
import org.yaml.snakeyaml.Yaml
import spray.json.{DefaultJsonProtocol, JsonFormat, RootJsonFormat}

import java.nio.file.Path
import java.util
import scala.jdk.javaapi.CollectionConverters
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

  // outgoing messages

  case class NoteRefResponse(noteName: String, noteRefOrWhyNot: Either[String, NoteRef])

  case class JsonRefResponse(jsonName: String, jsonRefOrWhyNot: Either[String, JsonRef])

  // behavior

  def apply(vaultPath: VaultPath): Behavior[Message] = setup(vaultPath)

  private def setup(vaultPath: VaultPath): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("VaultKeeper started")

    behavior(vaultPath, vaultPath.resolve("json"), Set.empty, Set.empty)
  }

  private def behavior(
                        vaultPath: VaultPath,
                        jsonPath: Path,
                        noteRefCache: Set[String],
                        jsonRefCache: Set[String]
                      ): Behavior[Message] = Behaviors.receive { (context, message) =>
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
            jsonRefCache
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
            jsonRefCache + jsonName
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
    }
  }
}


// model


// https://help.obsidian.md/Linking+notes+and+files/Internal+links#Link+to+a+block+in+a+note
sealed trait LinkId {
  def asString: String

  def wikiLinkWithAlias(alias: String): String = s"[[$asString|$alias]]"
  override def toString: String = s"[[$asString]]"
}


case class NoteId(id: String
                  // FIXME: support for subdirectories?
                  //                  , subdirectory: String
                 ) extends LinkId {
  override def asString: String = id

  def heading(heading: String): HeadingId = HeadingId(heading, this)
}

object LinkIdJsonProtocol extends DefaultJsonProtocol {
  implicit def noteIdFormat: JsonFormat[NoteId] = jsonFormat1(NoteId)
}

case class HeadingId private (heading: String, noteId: NoteId) extends LinkId {
  override def asString: String = s"${noteId.id}#^$heading"
}

case class Note private[vault] (
                                 markdown: String,
                                 // FIXME https://github.com/jcazevedo/moultingyaml
                                 frontmatter: Option[String]
               ) {
  def raw: String = {
    frontmatter match {
      case Some(yaml) =>
        s"""---
           |$yaml
           |---
           |$markdown
           |""".stripMargin
      case None =>
        markdown
    }
  }

  def yamlFrontMatter: Option[Map[String, Any]] = frontmatter.map { yaml =>
    val javaMap: java.util.Map[String, Any] = Note.Yaml.load(yaml)
    val scalaMap: Map[String, Any] = CollectionConverters.asScala(javaMap).toMap
    scalaMap
  }
}

object Note {
  val Yaml = new Yaml()

  def apply(markdown: String, frontmatter: Map[String, Object]): Note  = {
    // workaround for Yaml library
    val asJava: util.Map[String, Object] = new java.util.HashMap[String, Object]()
    for ((key, value) <- frontmatter) {
      asJava.put(key, value)
    }

    Note(markdown, Some(Yaml.dump(asJava)))
  }
}

case class SpiritId private[vault](id: String)

object SpiritId {
  def apply(clazz: Class[_]): SpiritId = {
    // e.g. me.micseydel.vault.VaultKeeper$
    SpiritId(clazz.getName)
  }

  object SpiritIdJsonFormatter extends DefaultJsonProtocol {
    implicit val spiritIDJsonFormat: RootJsonFormat[SpiritId] = jsonFormat1(SpiritId.apply(_: String))
  }
}
