package me.micseydel.app.selfsortingarrays

import me.micseydel.app.selfsortingarrays.Probe.CellProbeState
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.InsertionSortCellWrapper
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability

object SelfSortingArrayDebugger {
  sealed trait Message

  final case class UpdatedState(id: Int, probe: CellProbeState) extends Message
  // FIXME: MessageSend and ClockTick

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    behavior(Map.empty)
  }

  private def behavior(state: Map[Int, CellProbeState])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case UpdatedState(id, newProbe) =>
        state.get(id).foreach {
          case existing@CellProbeState(_, maybeLeft, maybeRight, _) if maybeLeft.nonEmpty && maybeRight.nonEmpty =>
            if (newProbe != existing) {
              println(terseTransition(id, existing, newProbe))
            }
          case _ => // ignore
        }
        behavior(state.updated(id, newProbe))
    }
  }

  private def terseTransition(id: Int, priorState: CellProbeState, newState: CellProbeState): String = {
    def f(t: Option[InsertionSortCellWrapper]): String = t.map(_.id.toString).getOrElse("x")

    s"$id (${f(priorState.maybeLeft)}, ${f(priorState.maybeRight)}) -> (${f(newState.maybeLeft)}, ${f(newState.maybeRight)})"
  }
}
