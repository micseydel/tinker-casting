package me.micseydel.actor

import akka.actor.{Actor, ActorRef, Props, typed}
import com.sandinh.paho.akka.{ConnOptions, MqttPubSub, PSConfig, Subscribe, SubscribeAck, Message => PahoMessage}
import me.micseydel.app.AppConfiguration.MqttConfig
import me.micseydel.dsl.RootTinkerBehavior.ReceiveMqttEvent

import scala.concurrent.duration.DurationInt

class PahoWrapperClassicActor(typedMqtt: typed.ActorRef[ReceiveMqttEvent], topics: Set[String], mqttConfig: MqttConfig) extends Actor {
  private val config = PSConfig(
    brokerUrl = mqttConfig.brokerUrl,
    conOpt = ConnOptions(
      username = mqttConfig.username,
      password = mqttConfig.password
    ),
    stashTimeToLive = 1.minute,
    stashCapacity = 8000,
    reconnectDelayMin = 10.millis,
    reconnectDelayMax = 30.seconds
  )

  this.context.system.log.info(s"Starting mqtt actor")
  private val mqtt: ActorRef = this.context.system.actorOf(Props(classOf[MqttPubSub], config))
  for (topic <- topics) {
    this.context.system.log.info(s"Subscribing to $topic")
    mqtt ! Subscribe(topic, self)
  }

  def receive: Receive = waitingForSubScribeAck

  private def waitingForSubScribeAck: Receive = {
    case SubscribeAck(Subscribe(topic, `self`, _), fail) if topics.contains(topic) =>
      if (fail.isEmpty) context become ready(typedMqtt)
      else this.context.system.log.error(fail.get, s"Can't subscribe to $topic")

    case other =>
      this.context.system.log.warning(s"Received ack for subscription on $other topic, but did not subscribe (ignoring")
  }

  def ready(typedMqtt: typed.ActorRef[ReceiveMqttEvent]): Receive = {
    case pm: PahoMessage =>
      this.context.system.log.info(s"Forwarding $pm")
      typedMqtt ! ReceiveMqttEvent(pm.topic, pm.payload)

    case msg =>
      this.context.system.log.info(s"unexpected ?? $msg")
  }
}
