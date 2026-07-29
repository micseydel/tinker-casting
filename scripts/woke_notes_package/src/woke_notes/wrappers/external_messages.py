import logging
import os
import random
from time import ctime
from collections import defaultdict
from dataclasses import dataclass

import pykka

from paho.mqtt import client as paho_mqtt_client
from paho.mqtt.client import Client

from .note_api import PrimitiveNoteAPI
from ..wrappers.note_api import NoteAPI


@dataclass
class MqttConfig:
    username: str
    password: str
    broker: str
    port: int


def get_config_from_env() -> MqttConfig | None:
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")
    broker = os.environ.get("mqttBroker")
    port = os.environ.get("mqttBrokerPort")

    if username and password and broker and port:
        return MqttConfig(username, password, broker, int(port))
    else:
        return None


@dataclass
class MqttSubscription:
    topic: str
    subscriber: pykka.ActorRef


@dataclass
class MqttPublish:
    topic: str
    payload: bytes


class ExternalMessages(pykka.ThreadingActor):
    mqtt_client: Client = None

    def __init__(self, config: MqttConfig, note_path: str):
        super().__init__()
        self.my_note: NoteAPI = None
        self.mqtt_config = config
        self.mqtt_connected = False
        self.note_path = note_path

        # mapping of topics to subscriber ActorRefs
        self.subscribers = defaultdict(set)

    # actor stuff

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='[%(asctime)s %(pathname)s:%(lineno)s] - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')

        self.my_note = NoteAPI(PrimitiveNoteAPI(self.note_path))

        if self.mqtt_config is not None:
            client_num = random.randint(0, 100)
            client_id = f'FIXME-TESTING-{client_num}'  # FIXME
            self.mqtt_client = paho_mqtt_client.Client(paho_mqtt_client.CallbackAPIVersion.VERSION2, client_id)
            self.mqtt_client.username_pw_set(self.mqtt_config.username, self.mqtt_config.password)

            self.mqtt_client.on_connect = self.on_mqtt_connect
            self.mqtt_client.connect(self.mqtt_config.broker, self.mqtt_config.port)

            # starts mqtt thread in the background (necessary for keep alive)
            self.mqtt_client.loop_start()

        self.my_note.set_file_contents(f"- started {ctime()}\n")

    def on_receive(self, msg):
        if isinstance(msg, MqttSubscription):
            if self.mqtt_client is not None:
                logging.debug(f"Subscribing to {msg.topic}: {msg.subscriber}")
                self.mqtt_client.subscribe(msg.topic)
                self.subscribers[msg.topic].add(msg.subscriber)
                self.my_note.append(f"- \\[{ctime()}] subscribed to `{msg.topic}`: {msg.subscriber}\n")
            else:
                logging.warning(f"Ignoring subscription to {msg.topic}, no mqtt config")
        elif isinstance(msg, MqttPublish):
            if self.mqtt_client is not None:
                self.mqtt_client.publish(msg.topic, msg.payload)
                logging.debug(f"published {len(msg.payload)} bytes to `{msg.topic}`")
            else:
                logging.warning(f"Ignoring publish to {msg.topic}, no mqtt config")
        else:
            logging.warning(f"Unknown message type: {type(msg)}")

    def on_stop(self):
        logging.info(f"[on_stop] calling loop_stop()")
        if self.mqtt_client is not None:
            self.mqtt_client.loop_stop()
        self.my_note.set_file_contents(f"- done {ctime()}\n")
        logging.info(f"[on_stop] stopped mqtt_client")

    # mqtt stuff

    def on_mqtt_message(self, client: paho_mqtt_client.Client, userdata, msg: paho_mqtt_client.MQTTMessage):
        topic = msg.topic
        payload = msg.payload

        topic_subscribers = self.subscribers[msg.topic]  # FIXME: wildcard wrappers
        if not topic_subscribers:
            logging.warning(f"Received an mqtt message of size {len(payload)} for topic {topic} but there are no subscribers ({self.subscribers})")

        for sub in topic_subscribers:
            sub.tell(MqttPublish(topic, payload))

    def on_mqtt_connect(self, client, userdata, flags, reason_code, properties):
        logging.info(
            f"[on_connect] previously_connected={self.mqtt_connected} | various={client, userdata, flags, reason_code, properties}")
        self.mqtt_client.on_message = self.on_mqtt_message
        self.mqtt_connected = True
