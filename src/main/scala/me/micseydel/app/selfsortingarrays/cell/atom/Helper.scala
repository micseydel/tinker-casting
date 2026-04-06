package me.micseydel.app.selfsortingarrays.cell.atom

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.BubbleSortCell.{BubbleSortCellWrapper, Message}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success, Try}

object Helper {
  implicit class InsertionSortCellRichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def updateDocument(tag: String, state: InsertionSortCellState, historyToAdd: String)(implicit self: BubbleSortCellWrapper, historyNote: SpiritRef[CellHistoryNote.Message], tinkerContext: TinkerContext[?]): Try[NoOp.type] = {
      historyNote !! CellHistoryNote.AddLines(List(historyToAdd))

      val newRaw =
        s"""---
           |tags: [$tag, cell]
           |---
           |- start index: ${self.id}
           |- index: ${state.index}
           |
           |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}|${state.maybeRightNeighbor.map(_.noteName).map(s => s" [[$s]] ->").getOrElse("")}
           |
           |# History
           |
           |[[${CellHistoryNote.noteName(self.id)}]]
           |""".stripMargin

      noteRef.setRaw(newRaw)
    }
  }

  def InvariantViolation(msg: String, finalState: InsertionSortCellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: BubbleSortCellWrapper, nr: NoteRef, historyHolder: SpiritRef[CellHistoryNote.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    nr.updateDocument("InvariantViolation", finalState, msg)
    Tinker.userExtension.probe !! Probe.FoundABug(msg)
    Tinker.done
  }
}

object CellHistoryNote {
  sealed trait Message

  final case class AddLines(lines: List[String]) extends Message

  def noteName(id: Int): String = s"Cell $id History"

  def apply(id: Int)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(noteName(id), TinkerColor.Purple, "🧫") { (context, noteRef) =>
    noteRef.setMarkdown("# History\n\n")

    Tinker.receiveMessage {
      case AddLines(lines) =>
        noteRef.append(lines.mkString("", "\n", "")) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }
        Tinker.steadily
    }
  }
}