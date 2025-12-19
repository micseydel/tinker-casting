package me.micseydel.app.selfsortingarrays

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Probe.CellProbeState
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.{CompleteSwap, InsertionSortCellWrapper, NotifyOfSwap}
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object SelfSortingArrayDebugger {
  sealed trait Message

  final case class UpdatedState(id: Int, probe: CellProbeState) extends Message
  final case class ClockTick(count: Int) extends Message
  // FIXME: MessageSend
  final case class FoundABug(details: String) extends Message

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
        val currentOrder: List[Int] = state.toList.sortBy(_._2.index).map(_._1)
        ORACLE.drop(count).headOption match {
          case Some(ClockTickExpectation(currentOracle, expectedMessage)) =>
            if (currentOrder == currentOracle) {
              noteRef.appendLine2(s"- ⏰($count) ✅").foreach(throw _)
            } else {
              noteRef.appendLine2(s"""- ⏰($count) ${currentOrder.mkString("\\[", ", ", "]")} (❌should have been ${currentOracle.mkString("\\[", ", ", "]")})""").foreach(throw _)
            }
          case None =>
            noteRef.appendLine2(s"- ⏰($count) ${currentOrder.mkString("\\[", ", ", "]")}").foreach(throw _)
        }
        Tinker.steadily

      case FoundABug(details) =>
        noteRef.appendLine2(s"- $details").foreach(throw _)
        Tinker.steadily
    }
  }

  private def terseTransition(id: Int, priorState: CellProbeState, newState: CellProbeState): String = {
    def f(t: Option[InsertionSortCellWrapper]): String = t.map(_.id.toString).getOrElse("x")

    s"$id (${f(priorState.maybeLeft)}, ${f(priorState.maybeRight)}) -> (${f(newState.maybeLeft)}, ${f(newState.maybeRight)})"
  }

  case class ClockTickExpectation(theList: List[Int], expectedMessages: List[?])

  case class MessageExpecter(messages: List[InsertionSortCell.Message]) {
    def integrate(message: InsertionSortCell.Message): Unit = {
      ???
    }
  }

  val ORACLE: List[ClockTickExpectation] = List(
    // FIXME: add expected messages!
    //  - create a MessageExpecter object, which can return updated state (like a Behavior!)
    ClockTickExpectation(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Nil), // 0
    ClockTickExpectation(List(0, 2, 1, 3, 4, 5, 6, 7, 8, 9), List(
//      NotifyOfSwap(),
//      CompleteSwap(),
//      NotifyOfSwap()
    )),
    ClockTickExpectation(List(2, 0, 3, 1, 4, 5, 6, 7, 8, 9), Nil /*FIXME*/),
    ClockTickExpectation(List(2, 3, 0, 4, 1, 5, 6, 7, 8, 9), Nil /*FIXME*/), // 3 ✅
    ClockTickExpectation(List(2, 3, 4, 0, 1, 6, 5, 7, 8, 9), Nil /*FIXME*/), // 4 ❌ List(2, 3, 4, 0, 1, **5, 6,** 7, 8, 9)
    ClockTickExpectation(List(2, 4, 3, 0, 1, 6, 7, 5, 8, 9), Nil /*FIXME*/), // 5 ❌ List(2, 4, 3, 0, 1,   5, 6,   7, 8, 9)
    ClockTickExpectation(List(2, 4, 3, 0, 1, 7, 6, 8, 5, 9), Nil /*FIXME*/), // 6
    ClockTickExpectation(List(2, 4, 3, 0, 7, 1, 8, 6, 9, 5), Nil /*FIXME*/), // 7
    ClockTickExpectation(List(2, 4, 3, 7, 0, 8, 1, 6, 9, 5), Nil /*FIXME*/), // 8
  )
}
