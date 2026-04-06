package me.micseydel.app.selfsortingarrays.cell

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.cell.BubbleSortCell.{BeginSwap, ClockTick, CompleteSwap, DoSort, Initialize, NotifyOfSwap, SwapProtocol}
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
  def wikilink: String = s"[[$noteName]]"

  def !~!(message: CM)(implicit tc: TinkerContext[?], probe: SpiritRef[Probe.Message], sender: CellWrapper[?]): Unit = {
    !~!(message, Some(sender.id))
  }

  def !!!(message: CM)(implicit tc: TinkerContext[?], probe: SpiritRef[Probe.Message]): Unit = {
    !~!(message, None)
  }

  private def !~!(message: CM, originator: Option[Int])(implicit tc: TinkerContext[?], probe: SpiritRef[Probe.Message]): Unit = {
    message match {
      // FIXME: no hard-coded cells, must be generic
      case insertionSortCellMessage: BubbleSortCell.Message =>
        insertionSortCellMessage match {
          case _: ClockTick | _: Initialize =>
          case DoSort =>
            probe !! Probe.MessageSend(originator, id, "DoSort")
          case swap: SwapProtocol =>
            probe !! Probe.MessageSend(Some(swap.originator), id, swap match {
              case BeginSwap(newLeft, _, _) => s"BeginSwap(newLeft=${Probe.getOptionalInsertionSortCellWrapperIdOrX(newLeft)})"
              case CompleteSwap(newRightOrReject, _, _) =>
                newRightOrReject match {
                  case Left(NoOp) =>
                    s"CompleteSwap((reject))"
                  case Right(newRight) =>
                    s"CompleteSwap(newRight=${Probe.getOptionalInsertionSortCellWrapperIdOrX(newRight)})"
                }
              case NotifyOfSwap(latestIndex, cell, _, _) =>
                val leftOrRight = cell match {
                  case Left(value) => s"left($latestIndex)=${value.id}"
                  case Right(value) => s"right($latestIndex)=${value.id}"
                }
                s"NotifyOfSwap($leftOrRight)"
            })
        }
      case _=>
    }
    spiritRef !! message
  }
}
