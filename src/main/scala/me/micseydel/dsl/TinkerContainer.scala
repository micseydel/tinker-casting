package me.micseydel.dsl

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{Behavior, DispatcherSelector}
import akka.actor.{ActorRef, ActorSystem, Props, typed}
import me.micseydel.actor.inactive.owntracks
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.perimeter.HueControl.HueConfig
import me.micseydel.actor.perimeter.{AranetActor, HomeMonitorActor, HueControl, NtfyerActor}
import me.micseydel.actor.{ActorNotesFolderWatcherActor, PahoWrapperClassicActor, TinkerOrchestrator}
import me.micseydel.app.AppConfiguration
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.{Gossiper, NetworkPerimeterActor, TinkerBrain}
import me.micseydel.vault.VaultKeeper

import java.util.concurrent.Executors
import scala.annotation.unused
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object TinkerContainer {
  def apply(config: AppConfig): ActorSystem = {
    implicit val httpExecutionContext: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(20))

    val rootBehavior: Ability[RootTinkerBehavior.Message] = RootTinkerBehavior(
      config,
      tinker => TinkerOrchestrator(TinkerOrchestrator.Config(
        config.vaultRoot,
        config.audioNoteWatchPath,
        config.aranetConfig,
        config.eventReceiverHost,
        config.eventReceiverPort,
        config.ntfyKeys,
        config.fitbitAuthorizationBasic
      ))(tinker), NtfyerActor()(_)
    )

    import akka.actor.typed.scaladsl.adapter._

    // the classic actor system is needed in order to get events from MQTT
    val actorSystem = ActorSystem("AkkaActor")

    val tinkercast: typed.ActorRef[RootTinkerBehavior.Message] = actorSystem.toTyped.systemActorOf(rootBehavior, "TinkerCast")

    actorSystem
  }
}

// FIXME: this needs to be refactored to figure out what is foundational and what sits on top
object RootTinkerBehavior {
  sealed trait Message
  // (no messages)

  // this is what the `applications` must accept
  case class ReceiveMqttEvent(topic: String, payload: Array[Byte])

  // FIXME: applications accepts mqtt events, but should probably/maybe also take voice notes...?
  def apply(config: AppConfig, applications: Tinker => Ability[ReceiveMqttEvent], ntfyAbility: Tinker => Ability[NtfyerActor.Message])(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.setup { context =>
    val jsonPath = config.vaultRoot.resolve("json")

    // generally hidden, internal use only
    val tinkerBrain: typed.ActorRef[TinkerBrain.Message] = context.spawn(TinkerBrain(jsonPath, Map.empty), "TinkerBrain")

    // central cast
    val vaultKeeper: typed.ActorRef[VaultKeeper.Message] = context.spawn(
      VaultKeeper(config.vaultRoot), "VaultKeeper",
      // VaultKeeper is important for Tinker Cast initialization, and does no I/O
      DispatcherSelector.fromConfig("vaultkeeper-high-priority-dispatcher")
    )
    val gossiper: typed.ActorRef[Gossiper.Message] = context.spawn(Gossiper(), "Gossiper")
    val chronicler = context.spawn(Chronicler(config, vaultKeeper, gossiper, tinkerBrain), "Chronicler")

    // perimeter

    // FIXME: this needs its own exclusive execution context
    val networkPerimeter: typed.ActorRef[NetworkPerimeterActor.Message] = context.spawn(NetworkPerimeterActor(), "NetworkPerimeterActor")

    // FIXME: letting this fail because its logging needs to be fixed anyway 🙃
    // FIXME: should be this be part of injected "notification center abilities" (a case class of optionals)?
    val hueControl: typed.ActorRef[HueControl.Message] = context.spawn(HueControl(config.hueConfig.getOrElse(HueConfig("", ""))), "HueControl")

    val notificationCenterManager: typed.ActorRef[NotificationCenterManager.Message] =
      context.spawn(NotificationCenterManager(config.vaultRoot, ntfyAbility), "NotificationCenterManager")

    val actorNotesFolderWatcherActor: typed.ActorRef[ActorNotesFolderWatcherActor.Message] = context.spawn(
      ActorNotesFolderWatcherActor(config.vaultRoot),
      "ActorNotesFolderWatcherActor"
    )
    
    val operator: typed.ActorRef[Operator.Message] = context.spawn(Operator(), "Operator")

    // magic
    val tinkerSystem = TinkerSystem(
      context.system,
      tinkerBrain,
      vaultKeeper,
      chronicler,
      gossiper,
      // perimeter
      hueControl,
      notificationCenterManager,
      networkPerimeter,
      operator,
      actorNotesFolderWatcherActor
    )

    implicit val tinker: Tinker = new Tinker(tinkerSystem)

    // Cmd+F for "case class StartTinkering" and count
    chronicler ! Chronicler.StartTinkering(tinker)
    hueControl ! HueControl.StartTinkering(tinker)
    notificationCenterManager ! NotificationCenterManager.StartTinkering(tinker)
    gossiper ! Gossiper.StartTinkering(tinker)
    actorNotesFolderWatcherActor ! ActorNotesFolderWatcherActor.StartTinkering(tinker)

    // FIXME: this looks like could be simplified, but I'm leaving it for now until I can dig in more
    config.aranetConfig match {
      case Some(aranetConfig) =>
        val homeMonitor = context.spawn(HomeMonitorActor(tinker => AranetActor(AranetActor.Config(aranetConfig.host, aranetConfig.port, config.ntfyKeys.highCO2))(tinker)), "HomeMonitor")
        homeMonitor ! HomeMonitorActor.StartTinkering(tinker)

      case None =>
        context.log.warn(s"No valid Aranet config found, not starting home monitor")
    }


//    tinkerBrain ! TinkerBrain.SystemStarted()
    context.log.info("Waiting 3 seconds before announcing system started")
    context.scheduleOnce(3.seconds, tinkerBrain, TinkerBrain.SystemStarted())
    context.scheduleOnce(10.seconds, tinkerBrain, TinkerBrain.WriteNote(tinker))

    // this should be internally-driven, doesn't need messages FROM here
    @unused
    val applicationsActor: typed.ActorRef[ReceiveMqttEvent] = context.spawn(applications(tinker), "Applications")

    config.mqttConfig match {
      case None =>
        context.log.warn("No mqtt config, no subscribing")
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
