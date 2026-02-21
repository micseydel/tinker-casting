package me.micseydel.app.selfsortingarrays.cell

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.atom.InsertionSortCellStateMachine
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// ??? is used in various places to crash if a surprise happens, but it may need to be replaced with Behaviors.same or Tinker.steadily if it's just a duplicate message and not a bug
object InsertionSortCell {

  type InsertionSortCellWrapper = CellWrapper[Message]

  // inbox 📥

  sealed trait Message

  final case class Initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]) extends Message

  case object DoSort extends Message

  // three parts: Begin (left->right), Complete (left<-right), Notify out to non-participants of the swap
  sealed trait SwapProtocol extends Message {
    def originator: Int
  }

  case class BeginSwap(newLeft: Option[InsertionSortCellWrapper], originator: Int) extends SwapProtocol

  case class CompleteSwap(newRightOrReject: Either[NoOp.type, Option[InsertionSortCellWrapper]], originator: Int) extends SwapProtocol

  // for the non-participants to the swap
  case class NotifyOfSwap(replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]], originator: Int) extends SwapProtocol

  case class ClockTick(count: Int) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: InsertionSortCellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    implicit val tc: TinkerContext[?] = context
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    Tinker.userExtension.probe !! Probe.Register(id, value, noteName)
    InsertionSortCellStateMachine.initializing(index)
  }
}
