package me.micseydel.testsupport

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.vault.VaultKeeper
import me.micseydel.vault.VaultKeeper.{JsonRefResponse, NoteRefResponse}
import me.micseydel.vault.persistence.{JsonRef, NoteRef}

object TestVaultKeeper {
  def apply(noteRefs: Map[String, NoteRef],
            jsonRefs: Map[String, JsonRef]): Behavior[VaultKeeper.Message] = Behaviors.receive { (context, message) =>
    message match {
      case VaultKeeper.RequestExclusiveNoteRef(noteId, replyTo, subdirectory) =>
        replyTo ! (noteRefs.get(noteId) match {
          case None =>
            NoteRefResponse(noteId, Left(s"Note ID $noteId not pre-loaded for testing, just: ${noteRefs.keySet}"))
          case Some(noteRef) =>
            NoteRefResponse(noteId, Right(noteRef))
        })
        Behaviors.same

      case VaultKeeper.RequestExclusiveJsonRef(jsonName, replyTo) =>
        replyTo ! (jsonRefs.get(jsonName) match {
          case None =>
            val available = jsonRefs.keys.map(key => s"`$key`").mkString(", ")
            JsonRefResponse(jsonName, Left(s"JSON name `$jsonName` not pre-loaded for testing ({$available}, get($jsonName) -> ${jsonRefs.get(jsonName)}, ${jsonRefs.contains(jsonName)})"))
          case Some(noteRef) =>
            JsonRefResponse(jsonName, Right(noteRef))
        })

        Behaviors.same

      case VaultKeeper.RequestAttachmentsContents(attachmentNames, replyTo) =>
        context.log.warn(s"FYI, ignoring request for $attachmentNames with replyTo path ${replyTo.path}")
        Behaviors.same

      case other => ???
    }
  }
}
