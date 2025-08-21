package me.micseydel.actor.notifications

import me.micseydel.NoOp
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import java.io.FileNotFoundException
import scala.util.{Success, Try}

object NotificationCenterManagerMarkdown {
  private val ClearAll = "- [ ] *Clear all*"
  private val DOClearAll = "- [x] *Clear all*"

  /**
   * @return the completed
   */
  def clearDone(noteRef: NoteRef): Try[List[NotificationId]] = {
    noteRef.readMarkdown().map(_.split("\n")).flatMap { lines =>
      val (done, notDone) = if (lines.headOption.exists(_.startsWith(DOClearAll))) {
        (lines.toList, Nil)
      } else {
        lines.toList.partition(_.startsWith("- [x]"))
      }

      val toWrite: Option[String] = if (done.nonEmpty) {
        if (notDone.headOption.exists(_.startsWith(ClearAll)) && notDone.size <= 4) {
          Some(notDone.drop(1).mkString("", "\n", "\n"))
        } else {
          Some(notDone.mkString("", "\n", "\n"))
        }
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
              .lastOption // FIXME: use regex or something instead
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
          if (newLines.size > 3 && !newLines.headOption.exists(_.startsWith(ClearAll))) {
            Some((s"$ClearAll (${newLines.size})" :: newLines).mkString("", "\n", "\n"))
          } else {
            Some(newLines.mkString("", "\n", "\n"))
          }
        }
    }
  }

  def clearNotification(noteRef: NoteRef, notificationId: String): Try[NoOp.type] = {
    updateMarkdown(noteRef) { lines =>
      linesUpdater(lines, notificationId)
    }
  }

  private def linesUpdater(lines: List[String], notificationId: String): Option[String] = {
    val updated = lines.filter(!_.endsWith(s" ^$notificationId"))
    if (updated.size < lines.size) {
      if (updated.headOption.exists(_.startsWith(ClearAll)) && updated.size <= 4) {
        Some(updated.drop(1).mkString("", "\n", "\n"))
      } else {
        Some(updated.mkString("", "\n", "\n"))
      }
    } else {
      None
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
