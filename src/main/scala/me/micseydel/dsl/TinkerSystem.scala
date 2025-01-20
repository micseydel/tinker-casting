package me.micseydel.dsl

import akka.actor.typed
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.{ActorRef, ActorSystem}
import cats.data.NonEmptyList
import me.micseydel.actor.{ActorNotesFolderWatcherActor, EventReceiver}
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.perimeter.HueControl
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.{Gossiper, NetworkPerimeterActor, TinkerBrain}
import me.micseydel.vault.VaultKeeper

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

abstract class TinkerSystem {
  def newContext[T](actorContext: ActorContext[T]): TinkerContext[T]

  def actorSystem: ActorSystem[_]

  private[dsl] def tinkerBrain: ActorRef[TinkerBrain.Message]

  // core abilities
  def vaultKeeper: SpiritRef[VaultKeeper.Message]

  def chronicler: SpiritRef[Chronicler.ListenerAcknowledgement]

  def gossiper: SpiritRef[Gossiper.Message]

  def notifier: SpiritRef[NotificationCenterManager.NotificationMessage]

  def actorNotesFolderWatcherActor: SpiritRef[ActorNotesFolderWatcherActor.Message]

  // perimeter
  def networkPerimeter: ActorRef[NetworkPerimeterActor.DoHttpPost]

  def hueControl: SpiritRef[HueControl.Command]

  def eventReceiver: ActorRef[EventReceiver.ClaimEventType]

  //  def homeMonitor: SpiritRef[HomeMonitorActor.Monitoring]
  def operator: SpiritRef[Operator.Message]

  private[dsl] def chroniclerActor: ActorRef[Chronicler.Message]

  /**
   * Enables !! for message tracking, but does not provide the actor with TinkerSystem access
   */
  def wrap[U](actorRef: ActorRef[U]): SpiritRef[U]

  def clock: TinkerClock

  // FIXME: not sure how to abstract this away without silliness, it seems to need to be part of the interface
  // and tests can just ignore it
  implicit val httpExecutionContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))

  // FIXME: testing, shift this into impl instead of interface, etc
  //  def getRandomElement[T](list: List[T]): Option[T] = list match {
  //    case Nil => None
  //    case _ => list.lift(scala.util.Random.nextInt(list.size))
  //  }
  def getRandomElement[T](list: NonEmptyList[T]): T =
    list.toList.apply(scala.util.Random.nextInt(list.size))

  // this was an experiment where SpiritRefs got serialized, and worked OTHER THAN for adapters
//  def spiritRefJsonFormat[T](): RootJsonFormat[SpiritRef[T]] = {
//    object SpiritRefJsonFormat extends DefaultJsonProtocol {
//      object MessageJsonFormat extends RootJsonFormat[SpiritRef[T]] {
//        private val key = "path"
//
//        def write(m: SpiritRef[T]): JsValue = Map(key -> m.path.toSerializationFormat).toJson.asJsObject
//
//        def read(value: JsValue): SpiritRef[T] = {
//          value.asJsObject.getFields(key) match {
//            case Seq(JsString(path)) =>
//              val underlying = ActorRefResolver(actorSystem).resolveActorRef[T](path)
//              new SpiritRefImpl[T](underlying, clock, tinkerBrain)
//
//            case other => throw DeserializationException(s"Expected an actor path under key `path`, got: $other")
//          }
//        }
//      }
//
//    }
//
//    SpiritRefJsonFormat.MessageJsonFormat
//  }
}

object TinkerSystem {
  def apply(actorSystem: ActorSystem[_], tinkerBrain: ActorRef[TinkerBrain.Message], vaultKeeper: ActorRef[VaultKeeper.Message], chronicler: ActorRef[Chronicler.Message], gossiper: ActorRef[Gossiper.Message], hueControlActor: ActorRef[HueControl.Message], notifier: ActorRef[NotificationCenterManager.NotificationMessage],
            networkPerimeter: ActorRef[NetworkPerimeterActor.DoHttpPost],
            operatorActor: ActorRef[Operator.Message],
            actorNotesFolderWatcherActor: typed.ActorRef[ActorNotesFolderWatcherActor.Message], eventReceiver: ActorRef[EventReceiver.ClaimEventType]): TinkerSystem = {
    new TinkerSystemImplementation(actorSystem, tinkerBrain, vaultKeeper, chronicler, gossiper, hueControlActor, notifier, networkPerimeter, operatorActor, actorNotesFolderWatcherActor, eventReceiver)
  }
}

class TinkerSystemImplementation(
                                  val actorSystem: ActorSystem[_],
                                  val tinkerBrain: ActorRef[TinkerBrain.Message],
                                  vaultKeeperActor: ActorRef[VaultKeeper.Message],
                                  private[dsl] val chroniclerActor: ActorRef[Chronicler.Message],
                                  gossiperActor: ActorRef[Gossiper.Message],
                                  hueControlActor: ActorRef[HueControl.Command],
                                  notifierActor: ActorRef[NotificationCenterManager.NotificationMessage],
                                  val networkPerimeter: ActorRef[NetworkPerimeterActor.DoHttpPost],
                                  operatorActor: ActorRef[Operator.Message],
                                  actorNotesFolderWatcherActor: typed.ActorRef[ActorNotesFolderWatcherActor.Message],
                                  val eventReceiver: ActorRef[EventReceiver.ClaimEventType]
                                ) extends TinkerSystem {
  override def newContext[T](actorContext: ActorContext[T]): TinkerContext[T] = {
    new TinkerContextImpl[T](actorContext, this)
  }

  def wrap[T](actorRef: ActorRef[T]): SpiritRef[T] =
    new SpiritRefImpl[T](actorRef, clock, tinkerBrain)

  override def vaultKeeper: SpiritRef[VaultKeeper.Message] = this.wrap(vaultKeeperActor)

  override def chronicler: SpiritRef[Chronicler.ListenerAcknowledgement] = this.wrap(chroniclerActor)

  override def gossiper: SpiritRef[Gossiper.Message] = this.wrap(gossiperActor)

  override def notifier: SpiritRef[NotificationCenterManager.NotificationMessage] = this.wrap(notifierActor)

  val hueControl: SpiritRef[HueControl.Command] = wrap(hueControlActor)

  override def operator: SpiritRef[Operator.Message] = wrap(operatorActor)

  override def actorNotesFolderWatcherActor: SpiritRef[ActorNotesFolderWatcherActor.Message] = wrap(actorNotesFolderWatcherActor)

  override def clock: TinkerClock = new TinkerClockImpl()
}