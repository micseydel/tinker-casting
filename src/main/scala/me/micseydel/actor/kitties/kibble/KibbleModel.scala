package me.micseydel.actor.kitties.kibble

object KibbleModel {
  sealed abstract class KibbleContainer(val baselineWeight: Int)

  final case object Circular1 extends KibbleContainer(122)

  final case object Circular2 extends KibbleContainer(122)

  final case object RectangularL extends KibbleContainer(189)

  final case object RectangularS extends KibbleContainer(157)
}
