package me.micseydel.dsl

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.ActorContext
import me.micseydel.actor.perimeter.NtfyerActor
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.{TimeKeeper, TinkerBrain}
import org.slf4j.Logger

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.Try

trait TinkerContext[T] {
  def cast[U](ability: Ability[U], name: String): SpiritRef[U]

  def castAnonymous[U](ability: Ability[U]): SpiritRef[U]


  def self: SpiritRef[T]

  def sender: Sender = Sender(self.actorPath)

  def pipeToSelf[Value](future: Future[Value])(mapResult: Try[Value] => T): Unit

  def messageAdapter[U: ClassTag](f: U => T): SpiritRef[U]

  def system: TinkerSystem

  def castTimeKeeper()(implicit Tinker: Tinker): SpiritRef[TimeKeeper.Message]

//  def log: Logger
  // FIXME: annoying thing I have to do because the logger uses reflection
  val actorContext: ActorContext[T]

  def spawn[U](behavior: Behavior[U], name: String): ActorRef[U] = actorContext.spawn(behavior, name)
}

class TinkerContextImpl[T](val actorContext: ActorContext[T], val system: TinkerSystem) extends TinkerContext[T] {

  // Akka wrappers

  def cast[U](ability: Ability[U], name: String): SpiritRef[U] = {
    val actorRef = actorContext.spawn(ability, name)
    system.tinkerBrain ! TinkerBrain.SpiritCast(actorRef.path)
    new SpiritRefImpl(actorRef, system.clock, system.tinkerBrain)
  }

  def castAnonymous[U](ability: Ability[U]): SpiritRef[U] = {
    new SpiritRefImpl[U](actorContext.spawnAnonymous(ability), system.clock, system.tinkerBrain)
  }

  def log: Logger = actorContext.log

  def self: SpiritRef[T] = system.wrap(actorContext.self)

  def pipeToSelf[Value](future: Future[Value])(mapResult: Try[Value] => T): Unit = actorContext.pipeToSelf(future)(mapResult)

  def messageAdapter[U: ClassTag](f: U => T): SpiritRef[U] = {
    system.wrap(actorContext.messageAdapter(f))
  }

  // side-effect capture for testing

  def castTimeKeeper()(implicit Tinker: Tinker): SpiritRef[TimeKeeper.Message] = {
    cast(TimeKeeper(), "TimeKeeper")
  }
}
