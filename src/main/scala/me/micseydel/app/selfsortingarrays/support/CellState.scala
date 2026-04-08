package me.micseydel.app.selfsortingarrays.support

case class CellState(index: Int, maybeLeftNeighbor: Option[CellWrapper[?]], maybeRightNeighbor: Option[CellWrapper[?]])
