package me.micseydel.dsl

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TinkerBrain.RegisterTinkerer
import me.micseydel.vault.persistence.{NoteRef, TypedJsonRef}
import spray.json.JsonFormat

import scala.util.Random

case class Tinkerer[T](color: TinkerColor, emoji: String, href: Option[String] = None) {
  def setup(factory: TinkerContext[T] => Ability[T])(implicit Tinker: Tinker): Ability[T] = Tinker.setup { context =>
    val registering = RegisterTinkerer(context.self.path, this)
    // FIXME: investigate this further, but this line seems to cause a CACHED logger where the calling class
    //   ends up as "me.micseydel.dsl.Tinkerer" which messages with custom logback stuff
    //    context.actorContext.log.info(s"registering $registering")

    context.system.tinkerBrain ! registering

    factory(context)
  }

  def receive(onMessage: (TinkerContext[T], T) => Ability[T])(implicit Tinker: Tinker): Ability[T] =
    setup(_ => Tinker.receive(onMessage))

  private[dsl] def withNote(noteName: String, subdirectory: Option[String] = None)(f: (TinkerContext[T], NoteRef) => Ability[T])(implicit Tinker: Tinker): Ability[T] = {
    setup(_ => Tinker.initializedWithNote(noteName, subdirectory)(f))
  }

  def initializedWithTypedJson[J](jsonName: String, jsonFormat: JsonFormat[J])(f: (TinkerContext[T], TypedJsonRef[J]) => Ability[T])(implicit Tinker: Tinker): Ability[T] = {
    setup(context => Tinker.withTypedJson(jsonName, jsonFormat)(f(context, _)))
  }
}

case class TinkerColor(r: Int, g: Int, b: Int, o: Double = 1.0) {
  override def toString: String = if (o == 1.0) {
    s"rgb($r, $g, $b)"
  } else {
    s"rgba($r, $g, $b, $o)"
  }
}

object TinkerColor {
  val Yellow: TinkerColor = rgb(255, 255, 0)
  val CatBrown: TinkerColor = rgb(140, 100, 90)
  val Purple: TinkerColor = rgb(102, 15, 213)

  //

  def random(r: Option[Int] = None, g: Option[Int] = None, b: Option[Int] = None, o: Double = 1.0): TinkerColor = {
    TinkerColor(r.getOrElse(Random.nextInt(256)), g.getOrElse(Random.nextInt(256)), b.getOrElse(Random.nextInt(256)), o)
  }

  def rgb(r: Int, g: Int, b: Int, o: Double = 1.0): TinkerColor =
    TinkerColor(r, g, b, o)
}
