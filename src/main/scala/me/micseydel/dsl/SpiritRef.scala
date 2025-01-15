package me.micseydel.dsl

import akka.actor.ActorPath
import akka.actor.typed.ActorRef
import me.micseydel.dsl.cast.TinkerBrain

trait SpiritRef[-T] {
  def underlying: ActorRef[T]

  def actorPath: ActorPath

  /**
   * Similar to ActoRef.! but includes tracking (via the context's sender)
   */
  def !!(message: T)(implicit tinkerContext: TinkerContext[_]): Unit

  /**
   * Similar to !! except that the sender is provided explicitly.
   */
  def !!!(message: T)(implicit sender: Sender): Unit

  /**
   * Only for use when other tracking is used.
   */
  def !!!!(message: T): Unit

  // FIXME: other kinds of message sending
  // send (tracked), send on behalf of,
  // !? - Query or ask for information, expecting a quick response.
  // >!> - Forward a message with added tinkering or modification.
  // :: - Combine or sequence actions/messages in a precise order.
  // ^!^ - Elevate a message priority or escalate an issue within the spirit network.
  // ...... %% - Split a message into subparts for parallel processing or analysis.
  // *!* - Broadcast a message to multiple spirits at once, like a multicast.
  // ~!~ - Send a message with a delayed execution or wait for a specific event before proceeding.

  def narrow[U <: T]: SpiritRef[U] = this.asInstanceOf[SpiritRef[U]]

  def path: ActorPath = underlying.path
}

class SpiritRefImpl[-T](
                         val underlying: ActorRef[T],
                         tinkerClock: TinkerClock,
                         tinkerBrain: ActorRef[TinkerBrain.Message]) extends SpiritRef[T] {
  override def toString: String = s"SpiritRef($actorPath)"

  def actorPath: ActorPath = underlying.path

  def !!!(message: T)(implicit sender: Sender): Unit = {
    if (!sender.path.toSerializationFormat.contains("TinkerBrain")) {
      tinkerBrain ! TinkerBrain.SentMessage(
        tinkerClock.now(),
        message.getClass.getName,
        sender,
        underlying
      )
    }

    underlying ! message
  }

  override def !!(message: T)(implicit tinkerContext: TinkerContext[_]): Unit = {
    implicit val sender: Sender = tinkerContext.sender
    this !!! message
  }

  /**
   * Does NOT do tracking, users of this method are responsible for their own tracking.
   */
  override def !!!!(message: T): Unit = {
    underlying ! message
  }
}

case class Sender(path: ActorPath)
