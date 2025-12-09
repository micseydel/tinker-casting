package me.micseydel.app.selfsortingarrays

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Probe.CellProbeState
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.InsertionSortCellWrapper
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object SelfSortingArrayDebugger {
  sealed trait Message

  final case class UpdatedState(id: Int, probe: CellProbeState) extends Message
  final case class ClockTick(count: Int) extends Message
  // FIXME: MessageSend and ClockTick

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("SelfSortingArrayDebugger", TinkerColor.random(), "🐜") { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.setMarkdown("starting...\n") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }
    behavior(Map.empty)
  }

  private def behavior(state: Map[Int, CellProbeState])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case UpdatedState(id, newProbe) =>
        state.get(id).foreach {
          case existing@CellProbeState(_, maybeLeft, maybeRight, _) if maybeLeft.nonEmpty && maybeRight.nonEmpty =>
            if (newProbe != existing) {
              noteRef.appendLine2(s"- ${terseTransition(id, existing, newProbe)}").foreach(throw _)
            }
          case _ => // ignore
        }
        behavior(state.updated(id, newProbe))

      case ClockTick(count) =>
        val ORACLE: List[List[Int]] = List(
          List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), // 0
          List(0, 2, 1, 3, 4, 5, 6, 7, 8, 9),
          List(2, 0, 3, 1, 4, 5, 6, 7, 8, 9),
          List(2, 3, 0, 4, 1, 5, 6, 7, 8, 9), // 3 ✅
          List(2, 3, 4, 0, 1, 6, 5, 7, 8, 9), // 4 ❌ List(2, 3, 4, 0, 1, **5, 6,** 7, 8, 9)
          List(2, 4, 3, 0, 1, 6, 7, 5, 8, 9), // 5 ❌ List(2, 4, 3, 0, 1, 5, 6, 7, 8, 9),
          List(2, 4, 3, 0, 1, 7, 6, 8, 5, 9), // 6
          List(2, 4, 3, 0, 7, 1, 8, 6, 9, 5), // 7
          List(2, 4, 3, 7, 0, 8, 1, 6, 9, 5), // 8
        )

        val currentOrder: List[Int] = state.toList.sortBy(_._2.index).map(_._1)
        ORACLE.drop(count).headOption match {
          case Some(currentOracle) =>
            if (currentOrder == currentOracle) {
              noteRef.appendLine2(s"- ⏰($count) ✅").foreach(throw _)
            } else {
              noteRef.appendLine2(s"""- ⏰($count) ${currentOrder.mkString("\\[", ", ", "]")} (❌should have been ${currentOracle.mkString("\\[", ", ", "]")})""").foreach(throw _)
            }
          case None =>
            noteRef.appendLine2(s"- ⏰($count) ${currentOrder.mkString("\\[", ", ", "]")}").foreach(throw _)
        }
        Tinker.steadily
    }
  }

  private def terseTransition(id: Int, priorState: CellProbeState, newState: CellProbeState): String = {
    def f(t: Option[InsertionSortCellWrapper]): String = t.map(_.id.toString).getOrElse("x")

    s"$id (${f(priorState.maybeLeft)}, ${f(priorState.maybeRight)}) -> (${f(newState.maybeLeft)}, ${f(newState.maybeRight)})"
  }
}
