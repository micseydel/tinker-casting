import os
import logging
import threading

import pykka

from watchdog.events import FileModifiedEvent

from note_api import NoteAPI
from wrappers.external_messages import MqttPublish, MqttSubscription


class MqttWrapper:
    def __init__(self, mqtt_actor, note_actor):
        self.mqtt_actor = mqtt_actor
        self.note_actor = note_actor

    def publish(self, topic, msg):
        self.mqtt_actor.tell(MqttPublish(topic, msg))

    def subscribe(self, topic):
        self.mqtt_actor.tell(MqttSubscription(topic, self.note_actor))


class WokeNote(pykka.ThreadingActor):
    def __init__(self, vault_path, note_name, mqtt, default_topic):
        # runs in the spawner's context, whereas on_start runs in the actor thread
        super().__init__()
        self.vault_path = vault_path
        self.note_name = note_name
        self.topic = default_topic
        self.mqtt = MqttWrapper(mqtt, self.actor_ref)
        self.mqtt.subscribe(default_topic)
        self.vault_name = os.path.split(vault_path.rstrip("/"))[1]

        self.note_path = os.path.join(vault_path, f"{note_name}.md")

        # defined in on_start
        self.compiled_script = None
        self.note_api = None
        self.script_scope = None

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')
        self.note_api = NoteAPI(self.note_path)

    def on_receive(self, message):
        logging.info(f"[{self.note_path}] {message}")
        if isinstance(message, FileModifiedEvent):
            self.on_note_modified()
        elif isinstance(message, MqttPublish):
            self.on_mqtt_message(message.topic, message.payload)
        else:
            logging.warning(f"Unexpected type {type(message)} {message}")

    # Woke Note default on_receive API

    def on_note_modified(self):
        logging.info("[woke_note.on_note_modified] FIXME")  # FIXME

    def on_mqtt_message(self, topic, payload):
        logging.info("[woke_note.on_mqtt_message] FIXME")  # FIXME

    # actor

    def on_stop(self):
        pass  # FIXME close note? (after updating the API wrapper)

    # util

    def _delayed_function_call(self, seconds, function):
        threading.Timer(seconds, function).start()


