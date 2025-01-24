package me.micseydel.actor.notifications

import me.micseydel.NoOp
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import java.io.FileNotFoundException
import scala.util.{Success, Try}

object NotificationCenterManagerMarkdown {

  /**
   * @return the completed
   */
  def clearDone(noteRef: NoteRef): Try[List[NotificationId]] = {
    noteRef.readMarkdown().map(_.split("\n")).flatMap { lines =>
      val (done, notDone) = lines.toList.partition(_.startsWith("- [x]"))

      val toWrite: Option[String] = if (done.nonEmpty) {
        Some(notDone.mkString("\n") + "\n")
      } else {
        None
      }

      toWrite
        .map(noteRef.setMarkdown)
        .getOrElse(Success(NoOp))
        .map { _ =>
          // now that the reads and writes are done, let's return to the caller the completed items
          done.flatMap(
            _.split(" ")
              .lastOption
              .filter(_.startsWith("^"))
              .map(_.drop(1))
          ).map(NotificationId)
        }
    }
  }

  def addNotification(noteRef: NoteRef, notification: Notification): Try[NoOp.type] = {
    notification match {
      case n@Notification(_, _, _, notificationId, _, _) =>
        updateMarkdown(noteRef) { lines =>
          val newListLine = Notification.toMarkdownListLine(n)
          val newLines = lines.filter(!_.endsWith(notificationId.id)) :+ newListLine
          Some(newLines.mkString("\n"))
        }
    }
  }

  def clearNotification(noteRef: NoteRef, notificationId: String): Try[NoOp.type] = {
    updateMarkdown(noteRef) { lines =>
      val updated = lines.filter(!_.endsWith(s" ^$notificationId"))
      if (updated.size < lines.size) {
        Some(updated.mkString("\n"))
      } else {
        None
      }
    }
  }

  private def updateMarkdown(noteRef: NoteRef)(updater: List[String] => Option[String]): Try[NoOp.type] = {
    noteRef.read().flatMap {
      case Note(markdown, _) =>
        updater(markdown.split("\n").toList) match {
          case Some(updatedMarkdown) =>
            noteRef.setMarkdown(updatedMarkdown)
          case None =>
            Success(NoOp)
        }
    }.recoverWith {
      case _: FileNotFoundException =>
        updater(Nil)
          .map(noteRef.setMarkdown)
          .getOrElse(Success(NoOp))
    }
  }
}
