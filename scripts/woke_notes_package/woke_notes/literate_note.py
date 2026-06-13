import os
import time
import json
import logging

from watchdog.events import FileModifiedEvent

from woke_note import WokeNote
from wrappers.external_messages import MqttPublish


# hot-reload variant of WokeNote
class LiterateNote(WokeNote):
    def __init__(self, vault_path, scripts_dir, note_name, mqtt, default_topic):
        super().__init__(vault_path, note_name, mqtt, default_topic)
        self.scripts_dir = scripts_dir
        self.script_path = os.path.join(scripts_dir, f"{note_name}.py")
        self.default_topic = default_topic

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
            "topic": self.topic, # FIXME: this should change with the frontmatter
            "delayed_function_call": self._delayed_function_call,

            # DSL
            "default_topic": self.default_topic,
            "on_mqtt_message": lambda topic, message: None,
            "on_note_modified": lambda: None,
            "my_note": self.note_api
            # FIXME: what about: after_start (when script is first loaded), after_hotreload, on_script_modified, pykka ?
        }

        self.compiled_script = None
        self.__recompile_script(False)

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
            self.__recompile_script(True)
        else:
            logging.warning(f"Unexpected type {type(message)} {message}")

    def __recompile_script(self, verbose):
        with open(self.script_path) as f:
            script = f.read()

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
