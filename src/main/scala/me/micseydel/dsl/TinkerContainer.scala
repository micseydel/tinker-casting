package me.micseydel.dsl

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{Behavior, DispatcherSelector}
import akka.actor.{ActorRef, ActorSystem, Props, typed}
import me.micseydel.actor.inactive.owntracks
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.perimeter.HueControl.HueConfig
import me.micseydel.actor.perimeter.{HomeMonitorActor, HueControl, NtfyerActor}
import me.micseydel.actor.{ActorNotesFolderWatcherActor, EventReceiver, PahoWrapperClassicActor, RasaActor, TinkerOrchestrator}
import me.micseydel.app.{AppConfiguration, NotificationCenterAbilities}
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.Chronicler.ChroniclerConfig
import me.micseydel.dsl.cast.{Gossiper, NetworkPerimeterActor, TinkerBrain}
import me.micseydel.vault.VaultKeeper

import java.util.concurrent.Executors
import scala.annotation.unused
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object TinkerContainer {
  def apply[CentralCast](appConfig: AppConfig, notificationCenterAbilities: NotificationCenterAbilities)(centralCastFactory: (Tinker, TinkerContext[_]) => CentralCast, userCast: EnhancedTinker[CentralCast] => Ability[ReceiveMqttEvent]): ActorSystem = {
    implicit val httpExecutionContext: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(20))

    // FIXME: simplify this clusterfuck
    val rootBehavior: Ability[RootTinkerBehavior.Message] = RootTinkerBehavior(
      appConfig,
      TinkerOrchestrator(centralCastFactory, userCast)(_),
      notificationCenterAbilities
    )

    import akka.actor.typed.scaladsl.adapter._

    // the classic actor system is needed in order to get events from MQTT
    // FIXME ...even though none are sourced from there right now, this is worth keeping to easily add later
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
  def apply(config: AppConfig, applications: Tinker => Ability[ReceiveMqttEvent], notificationCenterAbilities: NotificationCenterAbilities)(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.setup { context =>
    val jsonPath = config.vaultRoot.resolve("json")

    // generally hidden, internal use only
    val tinkerBrain: typed.ActorRef[TinkerBrain.Message] = context.spawn(TinkerBrain(jsonPath, Map.empty), "TinkerBrain")

    val vaultKeeper: typed.ActorRef[VaultKeeper.Message] = context.spawn(
      VaultKeeper(config.vaultRoot), "VaultKeeper",
      // VaultKeeper is important for Tinker Cast initialization, and does no I/O
      DispatcherSelector.fromConfig("vaultkeeper-high-priority-dispatcher")
    )
//    val gossiper: typed.ActorRef[Gossiper.Message] = context.spawn(Gossiper(), "Gossiper")
//    val chronicler = context.spawn(Chronicler(ChroniclerConfig(config.vaultRoot, config.eventReceiverHost, config.eventReceiverPort), gossiper), "Chronicler")

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

    // magic
    val tinkerSystem = TinkerSystem(
      context.system,
      tinkerBrain,
      vaultKeeper,
//      chronicler,
//      gossiper,
      // perimeter
      notificationCenterManager,
      networkPerimeter,
      operator,
      actorNotesFolderWatcherActor,
      eventReceiver
    )

    implicit val tinker: Tinker = new Tinker(tinkerSystem)

    // Cmd+F for "case class StartTinkering" and count
//    chronicler ! Chronicler.StartTinkering(tinker)
    notificationCenterManager ! NotificationCenterManager.StartTinkering(tinker)
//    gossiper ! Gossiper.StartTinkering(tinker)
    actorNotesFolderWatcherActor ! ActorNotesFolderWatcherActor.StartTinkering(tinker)

    @unused // driven internally
    val homeMonitor = context.spawn(HomeMonitorActor(), "HomeMonitor")

    //    tinkerBrain ! TinkerBrain.SystemStarted()
    context.log.info("Waiting 3 seconds before announcing system started")
    context.scheduleOnce(3.seconds, tinkerBrain, TinkerBrain.SystemStarted())
    context.scheduleOnce(10.seconds, tinkerBrain, TinkerBrain.WriteNote(tinker))

    // FIXME: I can create a DIFFERENT Tinker object here, for user space
    // ...ah, so the user's extension object will take the SystemTinker object, and the UserTinker one will have a second field with their goodies
    // how to contextualize the context? different subclasses, or type parameterization?
    // UserExtensions(various spiritref fields, e.g. Rasa and Ollama, maybe something that registers with the operator for push notifications (which only the notif center will use)

    // FIXME: central cast
    // FIXME: I thought this could be created here, but it's actually going to have to be deeper - in applications()
    //    val perimeter = perimeterFactory(context)
    // FIXME: ok os userspace (TinkerOrchestrator) needs to be created with access to a TinkerContext
    //   problem is, right now tinkercontext immediately means user space

    //    val enhancedTinker = new EnhancedTinker(tinkerSystem, rasaActor)

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
