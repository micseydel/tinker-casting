package me.micseydel.dsl

import akka.actor.typed
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.{ActorRef, ActorSystem}
import cats.data.NonEmptyList
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.dsl.cast.TinkerBrain
import me.micseydel.vault.VaultKeeper

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

abstract class TinkerSystem {
  /**
   * The underlying Akka ActorSystem
   */
  def actorSystem: ActorSystem[?]

  /**
   * Wrap an ActorContext with a TinkerContext
   */
  def wrapActorContext[T](actorContext: ActorContext[T]): TinkerContext[T]

  /**
   * Wrap an ActorRef with a SpiritRef
   */
  def wrapActorRef[U](actorRef: ActorRef[U]): SpiritRef[U]

  /**
   * Actor responsible for accessing Markdown notes.
   */
  def vaultKeeper: SpiritRef[VaultKeeper.Message]

  /**
   * \[\[Notification Center\]\]
   */
  def notifier: SpiritRef[NotificationCenterManager.NotificationMessage]

  /**
   * MQTT - for messages outside Akka
   */
  def mqtt: ActorRef[TypedMqtt.Message]

  /**
   * Provides certain basic functionality like email
   */
  def operator: SpiritRef[Operator.Message]

  /**
   * Clock to be used for testing/dependency injeciton.
   */
  def clock: TinkerClock

  def getRandomElement[T](list: NonEmptyList[T]): T

  // FIXME: not sure how to abstract this away without silliness, it seems to need to be part of the interface
  // and tests can just ignore it
  implicit val httpExecutionContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(30))

  private[dsl] def tinkerBrain: ActorRef[TinkerBrain.Message]
}

object TinkerSystem {
  def apply(actorSystem: ActorSystem[?],
            tinkerBrain: ActorRef[TinkerBrain.Message],
            vaultKeeper: ActorRef[VaultKeeper.Message],
            notifier: ActorRef[NotificationCenterManager.NotificationMessage],
            operatorActor: ActorRef[Operator.Message],
            typedMqtt: typed.ActorRef[TypedMqtt.Message]): TinkerSystem = {
    new TinkerSystemImplementation(actorSystem, tinkerBrain, vaultKeeper, notifier, operatorActor, typedMqtt)
  }
}

class TinkerSystemImplementation(
                                  val actorSystem: ActorSystem[?],
                                  val tinkerBrain: ActorRef[TinkerBrain.Message],
                                  vaultKeeperActor: ActorRef[VaultKeeper.Message],
                                  notifierActor: ActorRef[NotificationCenterManager.NotificationMessage],
                                  operatorActor: ActorRef[Operator.Message],
                                  val mqtt: ActorRef[TypedMqtt.Message]
                                ) extends TinkerSystem {
  override def wrapActorContext[T](actorContext: ActorContext[T]): TinkerContext[T] = {
    new TinkerContextImpl[T](actorContext, this)
  }

  def wrapActorRef[T](actorRef: ActorRef[T]): SpiritRef[T] =
    new SpiritRefImpl[T](actorRef, clock, tinkerBrain)

  override def vaultKeeper: SpiritRef[VaultKeeper.Message] = this.wrapActorRef(vaultKeeperActor)

  override def notifier: SpiritRef[NotificationCenterManager.NotificationMessage] = this.wrapActorRef(notifierActor)

  override def operator: SpiritRef[Operator.Message] = wrapActorRef(operatorActor)

  override def clock: TinkerClock = new TinkerClockImpl()

  override def getRandomElement[T](list: NonEmptyList[T]): T =
    list.toList.apply(scala.util.Random.nextInt(list.size))
}
