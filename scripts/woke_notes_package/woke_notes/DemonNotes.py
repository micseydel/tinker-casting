import logging
import os
import time
import json

import pykka
from watchdog.events import FileModifiedEvent

from woke_note import WokeNote
from wrappers.external_messages import MqttPublish, MqttSubscription
from wrappers.file_watcher import VaultNoteSubscription


class DemonNotesManager(pykka.ThreadingActor):
    def __init__(self, vault_path, mqtt, vault_watcher):
        super().__init__()
        self.note_name = "Demon Notes"
        self.note_path = os.path.join(vault_path, f"{self.note_name}.md")

        self.mqtt = mqtt
        self.vault_watcher = vault_watcher
        self.vault_path = vault_path
        self.vault_name = os.path.split(vault_path.rstrip("/"))[1]

    def on_start(self):
        super().on_start()

        to_spawn = []
        with open(self.note_path) as f:
            for line_number, line in enumerate(f, 1):
                if not (line.startswith("- [[") and line.endswith("]]\n")):
                    logging.info(f"Expected a list of wikilinks but line {line_number} was {line}")
                    return
                to_spawn.append(line[4:-3])

        for note_name in to_spawn:
            topic = f"{self.vault_name}/[[{note_name}]]"
            demon = DemonNote.start(self.vault_path, note_name, self.mqtt)
            self.vault_watcher.tell(VaultNoteSubscription(note_name, demon))
            self.mqtt.tell(MqttSubscription(topic, demon))


class DemonNote(WokeNote):
    def __init__(self, vault_path, note_name, mqtt):
        self.vault_name = os.path.split(vault_path.rstrip("/"))[1]
        super().__init__(vault_path, note_name, mqtt, f"{self.vault_name}/[[{note_name}]]")
        self.script_path = None

    # FIXME: this was copy-paste from LiterateNote, think more about it
    def on_start(self):
        # defines note_api
        super().on_start()

        self.script_scope: dict = {
            "logging": logging,
            "ctime": time.ctime,
            "json": json,
            "sleep": time.sleep,

            # utils
            "mqtt": self.mqtt,
            "note_name": self.note_name,
            "vault_name": self.vault_name,
            "topic": self.topic,  # FIXME: this should change with the frontmatter
            # "delayed_function_call": self._delayed_function_call,
            "set_timer": self.set_timer,

            "my_note": self.note_api,
            # DSL and lifecycle tinkering - to be expanded as needed
            "default_topic": self.topic,
            "on_mqtt_message": lambda topic, message: None,
            "on_note_modified": lambda: None,
            "on_start": lambda: None,
            "on_timer": lambda payload: None,
        }

        self.script_path = f"{self.note_path}#Code"

        self.compiled_script = None
        self.__load_script_from_note()
        try:
            self.script_scope["on_start"]()
        except Exception as e:
            logging.exception(f"on_start call failed", e)

    def on_receive(self, message):
        # super on_receive ignored intentionally, those things are managed explicitly here

        # script_scope behaviors are defined in __recompile_script, which gets them from the scripts directory
        if isinstance(message, FileModifiedEvent):
            try:
                self.script_scope["on_note_modified"]()
            except Exception as e:
                logging.exception(f"on_note_modified call failed", e)
        elif isinstance(message, MqttPublish):
            try:
                self.script_scope["on_mqtt_message"](message.topic, message.payload)
            except Exception as e:
                logging.exception(f"on_mqtt_message call failed", e)
        elif message == "SCRIPT_MODIFIED":
            logging.info(f"Hot reloading [[{self.note_name}]]")
            self.__load_script_from_note()
        else:
            try:
                message_type, payload = message
                if message_type == "TIMER":
                    try:
                        self.script_scope["on_timer"](payload)
                    except Exception as e:
                        logging.exception(f"on_timer call failed", e)
                else:
                    logging.warning(f"Unexpected type {message_type}:- {message}")
            except ValueError:
                logging.warning(f"Unexpected type {type(message)} {message}")

    def __load_script_from_note(self):
        with open(self.note_path) as f:
            # consume until the right header
            for line in f:
                if line == "# Code\n":
                    break

            # consume until the code
            for line in f:
                if line == "```python\n":
                    break

            script_lines = []
            for line in f:
                if line == "```\n":
                    break
                else:
                    script_lines.append(line)

            script = "".join(script_lines)
            logging.info(f"Using script:\n```{script}```")

        try:
            self.compiled_script = compile(script, self.script_path, "exec")
        except SyntaxError as e:
            logging.exception(f"Ignored {self.script_path} read {len(script)} bytes but there was a syntax error", e)
        except Exception as e:
            logging.exception(f"Something went wrong ({e}) with the script {self.script_path}", e)
        else:
            logging.info(f"[{self.note_name}] recompilation complete, executing now...")
            # this may update the scope with potentially new on* event functions
            exec(self.compiled_script, self.script_scope)
