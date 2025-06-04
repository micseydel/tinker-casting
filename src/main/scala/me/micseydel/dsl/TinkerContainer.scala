package me.micseydel.dsl

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{Behavior, DispatcherSelector}
import akka.actor.{ActorRef, ActorSystem, Props, typed}
import me.micseydel.actor.inactive.owntracks
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.actor.perimeter.HomeMonitorActor
import me.micseydel.actor.{ActorNotesFolderWatcherActor, EventReceiver, PahoWrapperClassicActor}
import me.micseydel.app.AppConfiguration
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.{NetworkPerimeterActor, TinkerBrain}
import me.micseydel.vault.VaultKeeper
import org.slf4j.LoggerFactory
import akka.actor.typed.scaladsl.adapter._

import java.nio.file.Files
import java.util.concurrent.Executors
import scala.annotation.unused
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

case class TinkerContainer(actorSystem: ActorSystem, tinkerCast: typed.ActorRef[RootTinkerBehavior.Message])

object TinkerContainer {
  def apply[CentralCast](appConfig: AppConfig, notificationCenterAbilities: NotificationCenterAbilities)(centralCastFactory: (Tinker, TinkerContext[_]) => CentralCast, userCast: EnhancedTinker[CentralCast] => Ability[ReceiveMqttEvent]): TinkerContainer = {
    implicit val httpExecutionContext: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(20))

    val rootBehavior: Ability[RootTinkerBehavior.Message] = RootTinkerBehavior(
      appConfig,
      TinkerOrchestrator(centralCastFactory, userCast)(_),
      notificationCenterAbilities
    )

    // ensure json subdirectory exists
    Files.createDirectories(appConfig.vaultRoot.resolve("json"))

    suppressSLF4JSpam()

    // the classic actor system is needed in order to get events from MQTT
    // FIXME ...even though none are sourced from there right now, this is worth keeping for now to easily add later
    val actorSystem: ActorSystem = ActorSystem("AkkaActor")

    val tinkerCast = actorSystem.toTyped.systemActorOf(rootBehavior, "TinkerCast")

    TinkerContainer(actorSystem, tinkerCast)
  }

  private def suppressSLF4JSpam(): Unit = {
    // this line suppresses -
    //   SLF4J: A number (1) of logging calls during the initialization phase have been intercepted and are
    //   SLF4J: now being replayed. These are subject to the filtering rules of the underlying logging system.
    //   SLF4J: See also https://www.slf4j.org/codes.html#replay
    LoggerFactory.getILoggerFactory
    // https://doc.akka.io/docs/akka/current/typed/logging.html#slf4j-api-compatibility wasn't as good
  }
}

object RootTinkerBehavior {
  sealed trait Message
  // no messages

  // this is what the `applications` must accept
  case class ReceiveMqttEvent(topic: String, payload: Array[Byte])

  def apply(config: AppConfig, applications: Tinker => Ability[ReceiveMqttEvent], notificationCenterAbilities: NotificationCenterAbilities)(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.setup { context =>
    val jsonPath = config.vaultRoot.resolve("json")

    // generally hidden, internal use only
    val tinkerBrain: typed.ActorRef[TinkerBrain.Message] = context.spawn(TinkerBrain(jsonPath, Map.empty), "TinkerBrain")

    val vaultKeeper: typed.ActorRef[VaultKeeper.Message] = context.spawn(
      VaultKeeper(config.vaultRoot), "VaultKeeper",
      // VaultKeeper is important for Tinker Cast initialization, and does no I/O
      DispatcherSelector.fromConfig("vaultkeeper-high-priority-dispatcher")
    )

    // perimeter

    // FIXME: this needs its own exclusive execution context
    val networkPerimeter: typed.ActorRef[NetworkPerimeterActor.Message] = context.spawn(NetworkPerimeterActor(), "NetworkPerimeterActor")

    val notificationCenterManager: typed.ActorRef[NotificationCenterManager.Message] =
      context.spawn(NotificationCenterManager(notificationCenterAbilities), "NotificationCenterManager")

    val actorNotesFolderWatcherActor: typed.ActorRef[ActorNotesFolderWatcherActor.Message] = context.spawn(
      ActorNotesFolderWatcherActor(config.vaultRoot),
      "ActorNotesFolderWatcherActor"
    )

    val operator: typed.ActorRef[Operator.Message] = context.spawn(Operator(), "Operator")

    // FIXME
    val eventReceiver: typed.ActorRef[EventReceiver.Message] = context.spawn(
      EventReceiver(
        EventReceiver.Config(config.eventReceiverHost, config.eventReceiverPort),
        tinkerBrain
      ),
      "EventReceiver"
    )

    val tinkerSystem = TinkerSystem(
      context.system,
      tinkerBrain,
      vaultKeeper,
      // perimeter
      notificationCenterManager,
      networkPerimeter,
      operator,
      actorNotesFolderWatcherActor,
      eventReceiver
    )

    implicit val tinker: Tinker = new Tinker(tinkerSystem)

    // Cmd+F for "case class StartTinkering" and count
    notificationCenterManager ! NotificationCenterManager.StartTinkering(tinker)
    actorNotesFolderWatcherActor ! ActorNotesFolderWatcherActor.StartTinkering(tinker)

    @unused // driven internally
    val homeMonitor = context.spawn(HomeMonitorActor(), "HomeMonitor")

    //    tinkerBrain ! TinkerBrain.SystemStarted()
    context.log.info("Waiting 3 seconds before announcing system started")
    context.scheduleOnce(3.seconds, tinkerBrain, TinkerBrain.SystemStarted())
    context.scheduleOnce(10.seconds, tinkerBrain, TinkerBrain.WriteNote(tinker))

    // this should be internally-driven, doesn't need messages FROM here
    @unused
    val applicationsActor: typed.ActorRef[ReceiveMqttEvent] = context.spawn(applications(tinker), "Applications")

    config.mqttConfig match {
      case None =>
        context.log.info("No mqtt config, not subscribing to mqtt events")
      case Some(mqttConfig: AppConfiguration.MqttConfig) =>
        val topics: Set[String] = Set(owntracks.Topic)
        context.log.info(s"Starting mqtt actor, listening to ${topics}")
        val props = Props(classOf[PahoWrapperClassicActor], applicationsActor, topics, mqttConfig)
        @unused // this actor receives messages and sends them to tinkercast
        val mqtt: ActorRef = context.system.classicSystem.actorOf(props, "ClassicActor")
    }

    //    // FIXME TinkerBrain -nodelist+edges-> WebSocket
    //    tinkerBrain ! TinkerBrain.ApplicationStarted()

    Behaviors.receiveMessage {
      m =>
        context.log.warn(s"$m")
        Behaviors.same
    }
  }
}


object TinkerOrchestrator {
  def apply[CentralCast](centralCastFactory: (Tinker, TinkerContext[_]) => CentralCast, userCast: EnhancedTinker[CentralCast] => Ability[ReceiveMqttEvent])(implicit Tinker: Tinker): Ability[ReceiveMqttEvent] = Tinker.setup[ReceiveMqttEvent] { context =>
    val enhancedTinker: EnhancedTinker[CentralCast] = new EnhancedTinker[CentralCast](context.system, centralCastFactory(Tinker, context))
    userCast(enhancedTinker)
  }
}
