package me.micseydel.actor

import akka.actor.{Actor, ActorRef, Props, typed}
import com.sandinh.paho.akka.{ConnOptions, MqttPubSub, PSConfig, Publish, Subscribe, SubscribeAck}
import me.micseydel.app.AppConfiguration.MqttConfig
import me.micseydel.dsl.SpiritRef
import me.micseydel.dsl.TypedMqtt.MqttMessage

import scala.concurrent.duration.DurationInt

class PahoWrapperClassicActor(typedMqtt: typed.ActorRef[Nothing], mqttConfig: MqttConfig) extends Actor {
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

  def receive: Receive = ready(Map.empty)(typedMqtt)

  def ready(subscribersMap: Map[String, Set[typed.ActorRef[MqttMessage]]])(implicit typedMqtt: typed.ActorRef[Nothing]): Receive = {
    case publish: com.sandinh.paho.akka.Publish =>
      mqtt ! publish

    case subscribe: com.sandinh.paho.akka.Subscribe =>
      this.context.system.log.info(s"subscribing: $subscribe")
      mqtt ! subscribe

    case message: com.sandinh.paho.akka.Message =>
      for {
        subscribers <- subscribersMap.get(message.topic)
        subscriber <- subscribers
      } subscriber ! MqttMessage(message.topic, message.payload)

    case SubscribeMqttTopic(topic: String, subscriber: typed.ActorRef[MqttMessage]) =>
      this.context.system.log.info(s"SubscribeMqttTopic: $topic $subscriber")
      context become ready(subscribersMap.updatedWith(topic) {
        case None =>
          Some(Set(subscriber))
        case Some(existingSubscribersForTopic) =>
          Some(existingSubscribersForTopic + subscriber)
      })
      mqtt ! Subscribe(topic, self)

    case ack: SubscribeAck =>
      ack.fail match {
        case Some(value) => this.context.system.log.warning(value, s"Ack failed for ${ack.subscribe}")
        case None => this.context.system.log.info(s"Sub'd to ${ack.subscribe.topic}")
      }

    case msg =>
      this.context.system.log.info(s"unexpected ?? $msg")
  }
}

case class SubscribeMqttTopic(topic: String, subscriber: typed.ActorRef[MqttMessage])
