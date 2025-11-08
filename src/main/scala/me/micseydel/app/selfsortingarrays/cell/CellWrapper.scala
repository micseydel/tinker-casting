package me.micseydel.app.selfsortingarrays.cell

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
  def !!(message: CM)(implicit tc: TinkerContext[?]): Unit = {
    spiritRef !! message
  }
}
