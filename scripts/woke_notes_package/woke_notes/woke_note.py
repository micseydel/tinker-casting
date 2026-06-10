import os
import logging
from time import ctime

import pykka

from watchdog.events import FileModifiedEvent

from note_api import NoteAPI
from wrappers.external_messages import MqttPublish


# https://docs.python.org/3/library/functions.html#open
# FIXME: can this open the note once and flush where it relies on quick open+closes?


class WokeNote(pykka.ThreadingActor):
    def __init__(self, vault_path, scripts_dir, note_name, mqtt):
        # runs in the spawner's context, whereas on_start runs in the actor thread
        super().__init__()
        self.vault_path = vault_path
        self.scripts_dir = scripts_dir
        self.note_name = note_name
        self.mqtt = mqtt

        self.note_path = os.path.join(vault_path, f"{note_name}.md")
        self.script_path = os.path.join(scripts_dir, f"{note_name}.py")

        # defined in on_start
        self.compiled_script = None
        self.note_api = None
        self.script_scope = None

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')

        with open(self.script_path) as f:
            script = f.read()

        # FIXME: tinker with breaking this line
        self.compiled_script = compile(script, self.script_path, "exec")

        self.note_api = NoteAPI(self.note_path)

        self.script_scope = {
            # built-ins
            "logging": logging,
            "ctime": ctime,
            "mqtt": self.mqtt,
            "MqttPublish": MqttPublish,

            # DSL
            "on_mqtt_message": lambda topic, message: None,
            "on_note_modified": lambda: None,
            "my_note": self.note_api
            # FIXME: what about: after_start (when script is first loaded), after_hotreload, on_script_modified, pykka ?
        }

        # FIXME consider documenting known security limitations (e.g. a script could overwrite logging?) or fixing them
        # this should update the scope with potentially new on* event functions
        exec(self.compiled_script, self.script_scope)

    def on_receive(self, message):
        print(f"[{self.note_path}] {message}")
        if isinstance(message, FileModifiedEvent):
            logging.info("Calling on_note_modified")
            self.script_scope["on_note_modified"]()
        elif isinstance(message, MqttPublish):
            logging.info("Calling on_mqtt_message")
            self.script_scope["on_mqtt_message"](message.topic, message.payload)
        elif message == "SCRIPT_MODIFIED":
            logging.info(f"Hot reloading [[{self.note_name}]]")
            with open(self.script_path) as f:
                script = f.read()
            # FIXME: tinker with breaking this line
            self.compiled_script = compile(script, self.script_path, "exec")
            exec(self.compiled_script, self.script_scope)
        else:
            logging.warning(f"Unexpected type {type(message)} {message}")

    def on_stop(self):
        pass  # FIXME close note?
