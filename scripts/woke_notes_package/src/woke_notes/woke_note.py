import logging
import os
from typing import TypeVar, Any, ClassVar

import pykka
from pykka import Actor, ActorRef
from watchdog.events import FileModifiedEvent, FileCreatedEvent

from .vault_router import VaultRouter
from .wrappers.note_api import NoteAPI, PrimitiveNoteAPI
from .wrappers.external_messages import MqttPublish, MqttSubscription, MqttConfig, ExternalMessages
from .wrappers.file_watcher import VaultWatcher, VaultNoteSubscription


class MqttWrapper:
    def __init__(self, mqtt_actor: pykka.ActorRef, note_actor: pykka.ActorRef):
        self.mqtt_actor = mqtt_actor
        self.note_actor = note_actor

    def publish(self, topic, msg):
        self.mqtt_actor.tell(MqttPublish(topic, msg))

    def subscribe(self, topic):
        self.mqtt_actor.tell(MqttSubscription(topic, self.note_actor))


WN = TypeVar("WN", bound="WokeNote")


class WokeNoteSupport:
    vault_path: str = None
    vault_name: str = None
    file_path = None
    external_messages_actor_ref = None
    vault_router = None

    def __init__(self, vault_path: str, mqtt_config: MqttConfig):
        self.vault_path = vault_path
        # rstrip() here is a hacky way to get os.path.split to treat the directory like a file
        self.vault_name = os.path.split(vault_path.rstrip("/"))[1]
        self.vault_watcher = VaultWatcher.start(self.vault_path)
        self.file_path = os.path.join(self.vault_path, "Woke Notes Mqtt Orchestrator.md")
        self.external_messages_actor_ref = ExternalMessages.start(mqtt_config, self.file_path)
        self.vault_router = VaultRouter.start()

    def stop(self):
        self.vault_watcher.stop()
        self.external_messages_actor_ref.stop()


class WokeNote(pykka.ThreadingActor):
    support: ClassVar[WokeNoteSupport] = None

    @classmethod
    def start_background_actors(
            cls: type[WN],
            vault_path: str,
            mqtt_config: MqttConfig,
    ):
        assert cls.support is None, "Background actors already started"  # FIXME: make this idempotent?
        cls.support = WokeNoteSupport(vault_path, mqtt_config)

    @classmethod
    def stop_background_actors(cls):
        cls.support.stop()
        cls.support = None

    # FIXME: because a user might accidentally call start(), I should consider NOT subclassing ThreadingActor here
    #   ...that might fit the multiprocessing model better anyway
    @classmethod
    def wake(
            cls: type[WN],
            note_name: str,
            *args: Any,
            **kwargs: Any,
    ) -> ActorRef[Actor]:
        assert cls.support is not None, "Did you run WokeNote.configure(vault_path, mqtt_config)?"

        # .start() comes from pykka.ThreadingActor
        actor_ref = cls.start(note_name,
                              # e.g. this is where e.g. ScriptedNote.scripts_dir gets passed through
                              *args, **kwargs)

        # a WokeNote adds events from mqtt and note updates
        # FIXME: ...and timers??!
        cls.support.vault_watcher.tell(VaultNoteSubscription(note_name, actor_ref))

        default_topic = _default_topic(cls.support.vault_name, note_name)
        cls.support.external_messages_actor_ref.tell(MqttSubscription(default_topic, actor_ref))

        return actor_ref

    def __init__(self, note_name: str, default_topic=None, *args, **kwargs):
        assert self.support is not None, "Did you forget to run WokeNote.configure(vault_path, mqtt_config)?"
        # runs in the spawner's context, whereas on_start runs in the actor thread
        super().__init__(*args, **kwargs)
        self.note_name = note_name

        # FIXME: sus out "topic" vs "default*"
        self.default_topic = self.topic = default_topic or _default_topic(self.support.vault_name, note_name)
        self.mqtt = MqttWrapper(self.support.external_messages_actor_ref, self.actor_ref)

        self.note_path = os.path.join(self.support.vault_path, f"{note_name}.md")

        # defined in on_start - consider wrapping in a getter so that an assert of on_start can happen
        self.my_note: NoteAPI = None

    def on_start(self):
        """Initializes logging for the thread and creates my_note"""
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')
        self.my_note = NoteAPI(PrimitiveNoteAPI(self.note_path))

    def on_receive(self, message):
        # logging.debug(f"[[{self.note_name}]] Receive {message}")
        if isinstance(message, (FileModifiedEvent, FileCreatedEvent)):
            self.on_note_modified()
        elif isinstance(message, MqttPublish):
            self.on_mqtt_message(message.topic, message.payload)
        else:
            # FIXME: pull on_timer up to here? the most basic messages:
            #  note modified (stateless message), timer up (Python), mqtt (bytes), pykka (other/Python)
            self.on_other_message(message)

    # Woke Note default on_receive API

    def on_note_modified(self):
        logging.info(f"[woke_note.on_note_modified] override this {self}")

    def on_mqtt_message(self, topic: str, payload: bytes):
        logging.info(f"[woke_note.on_mqtt_message] override this {self} (ignoring ({len(payload)} on {topic})")

    def on_other_message(self, message):
        logging.warning(f"Unexpected type {type(message)} {message} (self={type(self)}/{self})")

    # actor

    def on_stop(self):
        pass  # FIXME close note? (after updating the note API/wrapper)


def _default_topic(vault_name, note_name):
    return f"{vault_name}/[[{note_name}]]"
