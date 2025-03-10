package me.micseydel.actor.kitties.kibble

object KibbleModel {
  sealed abstract class KibbleContainer(val baselineWeight: Int, val noteName: String)

  final case object Circular1 extends KibbleContainer(122, "Primary Circular Plastic Food Container")

  final case object Circular2 extends KibbleContainer(122, "Secondary Circular Plastic Food Container")

//  final case object RectangularL extends KibbleContainer(189)

  final case object RectangularS extends KibbleContainer(157, "Small Rectangular Plastic Food Container")
}
