package me.micseydel.app.selfsortingarrays.cell.atom

import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.BubbleSortCell.BubbleSortCellWrapper
import me.micseydel.dsl.{EnhancedTinker, TinkerContext}

case class InsertionSortCellState(index: Int, maybeLeftNeighbor: Option[BubbleSortCellWrapper], maybeRightNeighbor: Option[BubbleSortCellWrapper]) {
  def maybeLeftId: Option[Int] = maybeLeftNeighbor.map(_.id)

  def maybeRightId: Option[Int] = maybeRightNeighbor.map(_.id)

  def probe(implicit cw: BubbleSortCellWrapper): Probe.UpdatedState = Probe.UpdatedState(cw.id, this)

  def wantToSwapWithRight()(implicit self: BubbleSortCellWrapper): Boolean = {
    maybeRightNeighbor.exists(_.value < self.value)
  }

  def sanityChecks(maybePriorState: Option[InsertionSortCellState])(implicit self: BubbleSortCellWrapper, Tinker: EnhancedTinker[SelfSortingArrayCentralCast], tinkerContext: TinkerContext[?]): Unit = {
    // we can't be our own neighbor
    if (maybeLeftNeighbor.map(_.id).contains(self.id)) {
      val msg = s"[${self.id}] Left neighbor ${maybeLeftNeighbor.get.id} is self!"
      Tinker.userExtension.probe !! Probe.FoundABug(msg)
//      throw InvariantViolation(msg)
      ???
    }
    if (maybeRightNeighbor.map(_.id).contains(self.id)) {
      val msg = s"[${self.id}] right neighbor is self!"
      Tinker.userExtension.probe !! Probe.FoundABug(msg)
//      throw InvariantViolation(msg)
      ???
    }

    // if we have two neighbors, they can't be the same
    for {
      leftNeighbor <- maybeLeftNeighbor
      rightNeighbor <- maybeRightNeighbor
    } if (leftNeighbor.id == rightNeighbor.id) {
      val msg = maybePriorState match {
        case Some(priorState) =>
          s"[${self.id}] Can't have left and right be the same: ${leftNeighbor.id}, prior state left&right: ${priorState.maybeLeftNeighbor.map(_.id)} & ${priorState.maybeRightNeighbor.map(_.id)}"
        case None =>
          s"[${self.id}] Can't have left and right be the same: ${leftNeighbor.id} (no prior state)"
      }
      Tinker.userExtension.probe !! Probe.FoundABug(msg)
//      throw InvariantViolation(msg)
      ???
    }
  }

  def locallySorted()(implicit self: BubbleSortCellWrapper): Boolean = {
    maybeLeftNeighbor.forall(self.value >= _.value) &&
      maybeRightNeighbor.forall(self.value <= _.value)
  }
}