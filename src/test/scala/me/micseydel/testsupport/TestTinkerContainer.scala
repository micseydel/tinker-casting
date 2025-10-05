package me.micseydel.testsupport

import akka.actor.ActorPath
import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.util.Timeout
import me.micseydel.actor.{EventReceiver, RasaActor}
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.SideEffect
import me.micseydel.actor.perimeter.{HueControl, NtfyerActor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.Chronicler.ReceiveNotePing
import me.micseydel.dsl.cast.{Gossiper, NetworkPerimeterActor, TimeKeeper, TinkerBrain}
import me.micseydel.testsupport.TimeOrchestratorForTesting.Fetch
import me.micseydel.vault.VaultKeeper
import me.micseydel.vault.persistence.{JsonRef, NoteRef}
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.LoggerFactory

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}


// FIXME frame as "stimulus" scenario and expected output?
/*
 * Spirits, similar to Actors, can *in response to a message it receives*
 * - cast new spirits
 * - send messages to spirits
 * - define behavior for the next message
 * - (!) have side-effects via system-provided facilities (NoteRefs or messages to Ntfy or Hue, or LOGGING)
 * Also, spirits can get SpiritRefs via
 * - casting them
 * - receiving them in their constructor
 * - the system
 * - receiving messages
 */
class SpiritTestKit() {
  val testKit: ActorTestKit = ActorTestKit()

  def createTestProbe[T](name: String): TestProbe[T] = testKit.createTestProbe[T](name)

  def spawn[T](behavior: Behavior[T], name: String): ActorRef[T] = testKit.spawn(behavior, name)

  def cast[T](ability: Ability[T], name: String)(implicit tinkerSystem: TinkerSystem): SpiritRef[T] = tinkerSystem.wrap(spawn(ability, name))

  def shutdownTestKit(): Unit = testKit.shutdownTestKit()
}

class SpiritRefForTesting[-T](val underlying: ActorRef[T]) extends SpiritRef[T] {

  override def actorPath: ActorPath = underlying.path

  override def !!(message: T)(implicit tinkerContext: TinkerContext[_]): Unit = {
    underlying ! message
  }

  override def !!!(message: T)(implicit sender: Sender): Unit = {
    underlying ! message
  }

  /**
   * Only for use when other tracking is used.
   */
  override def !!!!(message: T): Unit = underlying ! message
}

class TinkerSystemForTesting(
                              override val chroniclerActor: ActorRef[Chronicler.Message],
                              val actorSystem: ActorSystem[_],
                              vaultKeeperActor: ActorRef[VaultKeeper.Message],
                              timeKeeper: ActorRef[TimeKeeper.Message],
                              val tinkerBrain: ActorRef[TinkerBrain.Message],
                              notificationCenterManager: ActorRef[NotificationCenterManager.Message],
                              gossiperActor: ActorRef[Gossiper.Message],
                              operatorActor: ActorRef[Operator.Message],
                              hueControlActor: ActorRef[HueControl.Message],
                              tinkerClock: TinkerClock,
                              actorNotesFolderWatcherActorForTest: ActorRef[ActorNotesFolderWatcherActor.Message]
                            ) extends TinkerSystem {
  def wrap[T](actorRef: ActorRef[T]): SpiritRef[T] = new SpiritRefForTesting[T](actorRef)

  override def chronicler: SpiritRef[Chronicler.ListenerAcknowledgement] = wrap(chroniclerActor)

  override def gossiper: SpiritRef[Gossiper.Message] = wrap(gossiperActor)

  override def newContext[T](actorCtx: ActorContext[T]): TinkerContext[T] = new TinkerContext[T] {
    override def cast[U](ability: Ability[U], name: String): SpiritRef[U] = system.wrap(actorContext.spawn(ability, name))

    override val actorContext: ActorContext[T] = actorCtx

    override def self: SpiritRef[T] = system.wrap(actorContext.self)

    override def pipeToSelf[Value](future: Future[Value])(mapResult: Try[Value] => T): Unit = actorContext.pipeToSelf(future)(mapResult)

    override def messageAdapter[U: ClassTag](f: U => T): SpiritRef[U] = system.wrap(actorContext.messageAdapter(f))

    override def system: TinkerSystem = TinkerSystemForTesting.this

    override def castTimeKeeper()(implicit Tinker: Tinker): SpiritRef[TimeKeeper.Message] = {
      system.wrap(timeKeeper)
    }

    override def castAnonymous[U](ability: Ability[U]): SpiritRef[U] = system.wrap(actorContext.spawnAnonymous(ability))
  }

  override def notifier: SpiritRef[NotificationCenterManager.Message] = wrap(notificationCenterManager)

  override def hueControl: SpiritRef[HueControl.Message] = wrap(hueControlActor)

  override def vaultKeeper: SpiritRef[VaultKeeper.Message] = wrap(vaultKeeperActor)

  override def actorNotesFolderWatcherActor: SpiritRef[ActorNotesFolderWatcherActor.Message] = wrap(actorNotesFolderWatcherActorForTest)

  override def networkPerimeter: ActorRef[NetworkPerimeterActor.DoHttpPost] = ???

  override def operator: SpiritRef[Operator.Message] = wrap(operatorActor)

  override def clock: TinkerClock = tinkerClock

  override def eventReceiver: ActorRef[EventReceiver.ClaimEventType] = ???

  override def rasaActor: ActorRef[RasaActor.Message] = ???
}

abstract class TinkerTest(val noteRefs: Map[String, NoteRef],
                          val jsonRefs: Map[String, JsonRef],
                          val testKit: SpiritTestKit) {
//  val testKit: SpiritTestKit = new SpiritTestKit()

  def shutdown(): Unit = testKit.shutdownTestKit()

  val tinkerBrain: ActorRef[TinkerBrain.Message] = testKit.spawn(TinkerBrainForTesting(), "TinkerBrainForTesting")

  // FIXME: introduce a TimeOrchestratorForTesting which
  // - provides a replyTo that an ask() call can use to extract the TimeKeeperForTesting below
  // - initializes TimeKeeperForTesting with deference to itself
  // - it provides an API for testers to manipulate the behavior of the TimeKeeperForTesting

  val timeOrchestrator: ActorRef[TimeOrchestratorForTesting.Message] =
    testKit.spawn(TimeOrchestratorForTesting(), "TimeOrchestratorForTesting")

  implicit val s: Scheduler = testKit.testKit.scheduler
  private val duration = 3.seconds
  implicit val to: Timeout = Timeout(duration)
  val timeKeeperForTesting: ActorRef[TimeKeeper.Message] = Await.ready(timeOrchestrator.ask(Fetch), duration).value match {
    case Some(maybe) =>
      maybe match {
        case Failure(exception) => throw exception
        case Success(value) => value
      }
    case None => throw new RuntimeException("What does None mean here?")
  }

  val tinkerTestProbe: ActorRef[TinkerProbeActor.Message] = testKit.spawn(TinkerProbeActor(), "TinkerProbeActor")

  private val ntfy: ActorRef[NtfyerActor.Message] = testKit.spawn(NtfyForTesting(tinkerTestProbe), "NtfyForTesting")

  private val notifier = testKit.spawn(NotifierForTesting(Some(ntfy)), "NotifierForTesting")

  private val chronicler = testKit.spawn(ChroniclerForTest(), "ChroniclerForTest")

  val gossiper: ActorRef[Gossiper.Message] = testKit.spawn(GossiperForTesting(), "GossiperForTesting")

  private val operator = testKit.spawn(OperatorForTesting(), "OperatorForTesting")

  private val hueControl = testKit.spawn(HueControlForTesting(tinkerTestProbe), "HueControlForTesting")

  // FIXME: what to use instead of ZonedDateTime.now()? Just epoche?
  private val clockContainer = testKit.spawn(ContainerActor(ZonedDateTime.now()), "ContainerActor")
  val tinkerClockForTest = new TinkerClockForTest(clockContainer)

  val actorNotesFolderWatcherActorForTest = testKit.spawn(ActorNotesFolderWatcherActorForTest(), "ActorNotesFolderWatcherActorForTest")

  implicit val tinkerSystem: TinkerSystemForTesting = new TinkerSystemForTesting(
    chronicler,
    testKit.testKit.system,
    testKit.spawn(TestVaultKeeper(noteRefs, jsonRefs), "TestVaultKeeper"),
    timeKeeperForTesting,
    tinkerBrain,
    notifier,
    gossiper,
    operator,
    hueControl,
    tinkerClockForTest,
    actorNotesFolderWatcherActorForTest
  )

  implicit val tinker: Tinker = new Tinker(tinkerSystem)
}

abstract class TestTinkerContainer
  extends AnyWordSpec
    with MockFactory
    with BeforeAndAfterAll
    with Matchers {

  LoggerFactory.getILoggerFactory // suppresses annoying logger output
  // this works, where https://doc.akka.io/docs/akka/current/typed/logging.html#slf4j-api-compatibility does not
}

object ChroniclerForTest {

  def apply(): Behavior[Chronicler.Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case Chronicler.StartTinkering(tinker) => ???
      case message: Chronicler.PostTinkeringInitMessage =>
        message match {
          case Chronicler.TranscriptionStartedEvent(capture) => ???
          case Chronicler.TranscriptionCompletedEvent(event) => ???
          case Chronicler.ReceiveNotedTranscription(event) => ???
          case ack@Chronicler.ListenerAcknowledgement(noteRef, timeOfAck, details, setNoteState) => context.log.info(s"Ignoring $ack")
          case ReceiveNotePing(ping) => ???
        }
        Behaviors.same
    }
  }
}

object TinkerProbeActor {
  sealed trait Message

  final case class RecordSideEffect(sideEffect: SideEffect) extends Message
  final case class GetSideEffects(replyTo: ActorRef[List[SideEffect]]) extends Message

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    behavior(Nil)
  }

  private def behavior(gathered: List[SideEffect]): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case RecordSideEffect(sideEffect) => behavior(sideEffect :: gathered)
      case GetSideEffects(replyTo) =>
        replyTo ! gathered
        Behaviors.same
    }
  }

  //

  private val duration = 3.seconds
  private implicit val to: Timeout = Timeout(duration)

  implicit class RichTinkerProbeActor(val underlying: ActorRef[TinkerProbeActor.Message]) extends AnyVal {
    def getSideEffects()(implicit s: Scheduler): List[SideEffect] = {
      Await.ready(underlying.ask(GetSideEffects), duration).value match {
        case Some(maybe) =>
          maybe match {
            case Failure(exception) => throw exception
            case Success(sideEffects) => sideEffects
          }
        case None => throw new RuntimeException("What does None mean here?")
      }
    }
  }
}

class TinkerClockForTest(container: ActorRef[ContainerActor.Message[ZonedDateTime]])(implicit scheduler: Scheduler) extends TinkerClock {
  def set(time: ZonedDateTime): Unit = {
    container.setValue(time)
  }

  override def now(): ZonedDateTime = container.getValue()

  override def now(zoneId: ZoneId): ZonedDateTime = container.getValue()

  override def today(): LocalDate = container.getValue().toLocalDate
}

object ContainerActor {
  sealed trait Message[T]

  final case class Get[T](replyTo: ActorRef[T]) extends Message[T]
  final case class Set[T](to: T) extends Message[T]

  def apply[T](default: T): Behavior[Message[T]] = Behaviors.setup { context =>
    state(default)
  }

  private def state[T](value: T): Behavior[Message[T]] = Behaviors.receive { (context, message) =>
    message match {
      case Get(replyTo) =>
        replyTo ! value
        Behaviors.same
      case Set(to) =>
        state(to)
    }
  }

  //

  private val duration = 3.seconds
  private implicit val to: Timeout = Timeout(duration)

  implicit class RichContainerActor[T](val underlying: ActorRef[Message[T]]) extends AnyVal {
    def getValue()(implicit s: Scheduler): T = {
      Await.ready(underlying.ask(Get.apply), duration).value match {
        case Some(maybe) =>
          maybe match {
            case Failure(exception) => throw exception
            case Success(value) => value
          }
        case None => throw new RuntimeException("What does None mean here?")
      }
    }

    def setValue(value: T): Unit = {
      underlying ! Set(value)
    }
  }
}

object ActorNotesFolderWatcherActorForTest {
  def apply(): Behavior[ActorNotesFolderWatcherActor.Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case ActorNotesFolderWatcherActor.StartTinkering(tinker) => ???
      case ActorNotesFolderWatcherActor.SubscribeSubdirectory(subdirectory, replyTo) => ???
      case ActorNotesFolderWatcherActor.SubscribeNoteRef(noteRef, replyTo) =>
      // FIXME: ignore
        Behaviors.same
      case ActorNotesFolderWatcherActor.ReceiveVaultPathUpdatedEvent(event) => ???
    }
  }
}
