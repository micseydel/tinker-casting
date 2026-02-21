package me.micseydel.app.selfsortingarrays.cell.atom

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.InsertionSortCellWrapper
import me.micseydel.dsl.TinkerContext
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success, Try}

object Helper {
  implicit class InsertionSortCellRichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def updateDocument(tag: String, state: InsertionSortCellState, historyToAdd: String, retainHistory: Boolean = true)(implicit self: InsertionSortCellWrapper, context: TinkerContext[?]): Try[NoOp.type] = {
      val historyMarkdownListLines: Seq[String] = if (retainHistory) {
        noteRef.readMarkdownSafer() match {
          case NoteRef.FileDoesNotExist =>
            List(historyToAdd)

          case NoteRef.Contents(Success(markdown)) =>
            val history = markdown.split("\n")
              .dropWhile(!_.startsWith("# History"))
              .drop(1)
              .filter(_.nonEmpty)
            history.toIndexedSeq.appended((historyToAdd))

          case NoteRef.Contents(Failure(exception)) =>
            throw exception
        }
      } else {
        List(historyToAdd)
      }

      val newRaw =
        s"""---
           |tags: [$tag]
           |---
           |- start index: ${self.id}
           |- index: ${state.index}
           |
           |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}|${state.maybeRightNeighbor.map(_.noteName).map(s => s" [[$s]] ->").getOrElse("")}
           |
           |# History
           |
           |${historyMarkdownListLines.mkString("\n")}
           |""".stripMargin

      noteRef.setRaw(newRaw)
    }
  }
}