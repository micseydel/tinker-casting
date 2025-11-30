package me.micseydel.app.selfsortingarrays.cell

import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.{BeginSwap, ClockTick, CompleteSwap, DoSort, Initialize, InsertionSortCellWrapper, NotifyOfSwap, SwapProtocol}
import me.micseydel.dsl.{SpiritRef, TinkerContext}

/**
 * convenience, makes simple comparisons less async
 *
 * @param id starting index
 * @param value the cell's value
 * @param noteName the cell's note name
 * @param spiritRef the cell's ref
 * @tparam CM the cell message
 */
case class CellWrapper[CM](id: Int, value: Int, noteName: String, spiritRef: SpiritRef[CM]) {
  def !~!(message: CM)(implicit tc: TinkerContext[?], probe: SpiritRef[Probe.Message], sender: InsertionSortCellWrapper): Unit = {
    !~!(message, Some(sender.id))
  }

  def !!!(message: CM)(implicit tc: TinkerContext[?], probe: SpiritRef[Probe.Message]): Unit = {
    !~!(message, None)
  }

  private def !~!(message: CM, originator: Option[Int])(implicit tc: TinkerContext[?], probe: SpiritRef[Probe.Message]): Unit = {
    message match {
      case insertionSortCellMessage: InsertionSortCell.Message =>
        insertionSortCellMessage match {
          case _: ClockTick | _: Initialize =>
          case DoSort =>
            probe !! Probe.MessageSend(originator, id, "DoSort")
          case swap: SwapProtocol =>
            probe !! Probe.MessageSend(Some(swap.originator), id, swap match {
              case BeginSwap(newLeft, _) => s"BeginSwap(newLeft=${Probe.getOptionalInsertionSortCellWrapperIdOrX(newLeft)})"
              case CompleteSwap(newRight, _) => s"CompleteSwap(newRight=${Probe.getOptionalInsertionSortCellWrapperIdOrX(newRight)})"
              case NotifyOfSwap(replacementLeftOrRight, _) =>
                val leftOrRight = replacementLeftOrRight match {
                  case Left(value) => s"left=${Probe.getOptionalInsertionSortCellWrapperIdOrX(value)}"
                  case Right(value) => s"right=${Probe.getOptionalInsertionSortCellWrapperIdOrX(value)}"
                }
                s"NotifyOfSwap($leftOrRight)"
            })
        }
      case _=>
    }
    spiritRef !! message
  }
}
