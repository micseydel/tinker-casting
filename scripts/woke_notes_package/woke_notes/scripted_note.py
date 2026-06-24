import logging
import os
from time import ctime
from typing import TypeVar

from watchdog.events import FileModifiedEvent

from wrappers.scripting import CompiledScript
from woke_note import WokeNote
from wrappers.external_messages import MqttPublish
from wrappers.file_watcher import FolderWatcher

SN = TypeVar("SN", bound="ScriptedNote")


class ScriptedNote(WokeNote):
    def __init__(self, note_name, script_path):
        super().__init__(note_name)
        self.script_path: str = script_path

        # this is defined in on_start
        self.script_wrapper: ScriptedNoteScriptWrapper = None

    def on_start(self):
        # defines note_api
        super().on_start()

        self.script_wrapper = ScriptedNoteScriptWrapper(self.actor_ref, self.my_note, self.topic, self.mqtt,
                                                        self.script_path)
        self.script_wrapper.on_start()

    def on_receive(self, message):
        # super on_receive ignored intentionally, those things are managed explicitly here

        if isinstance(message, FileModifiedEvent):
            self.script_wrapper.on_note_modified()
        elif isinstance(message, MqttPublish):
            self.script_wrapper.on_mqtt_message(message.topic, message.payload)
        elif message == "SCRIPT_MODIFIED":
            logging.info(f"Hot reloading [[{self.note_name}]]")
            self.script_wrapper.recompile_script()
        else:
            try:
                message_type, payload = message
                if message_type == "TIMER":
                    self.script_wrapper.on_timer(payload)
                else:
                    logging.warning(f"Unexpected type {message_type}:- {message}")
            except ValueError:
                logging.warning(f"Unexpected type {type(message)} {message}")


class ScriptedNotesOrchestrator(WokeNote):
    def __init__(self, note_name: str, scripts_dir: str, *args, **kwargs):
        super().__init__(note_name, *args, **kwargs)
        self.scripts_dir = scripts_dir
        self.scripts_watcher = None
        self.woke_notes = {}  # from note name to ActorRef

    def on_start(self):
        super().on_start()

        # FIXME: need to receive NEW file creations too! not just modified!
        self.scripts_watcher = FolderWatcher.start(self.scripts_dir, ".py", self.actor_ref)

        python_scripts = [script for script in os.listdir(self.scripts_dir) if script.lower().endswith(".py")]
        note_names_for_scripts = [os.path.splitext(script)[0] for script in python_scripts]

        logging.info(f"Starting with scripts {python_scripts}; spawning WokeNotes now")

        for note_name in note_names_for_scripts:
            actor_ref = ScriptedNote.wake(note_name, os.path.join(self.scripts_dir, f"{note_name}.py"))
            self.woke_notes[note_name] = actor_ref

        scripted_notes_list = "\n".join(f"    - [[{nn}]]" for nn in note_names_for_scripts)
        self.my_note.set_contents(f"""- generated {ctime()}
- scripted notes:
{scripted_notes_list}
""")

    def on_receive(self, message):
        if isinstance(message, FileModifiedEvent):
            if message.src_path.startswith(self.support.vault_path):
                self.on_note_modified()
            elif message.src_path.startswith(self.scripts_dir):
                _, filename = os.path.split(message.src_path)
                maybe_note_name, maybe_md = os.path.splitext(filename)
                if maybe_md.lower() == ".py":
                    self.woke_notes[maybe_note_name].tell("SCRIPT_MODIFIED")
                    logging.warning(f"NOT HOT RELOADING {message.src_path}")
                else:
                    logging.info(f"Did not recognize file ext {maybe_md}, ignoring {message.src_path}")
            else:
                logging.warning(
                    f"unexpected path `{message.src_path}` (did not start with scripts dir `{self.scripts_dir}` or vault dir `{self.support.vault_path}`)")
        elif isinstance(message, MqttPublish):
            self.on_mqtt_message(message.topic, message.payload)
        else:
            logging.warning(f"Unexpected message, type {type(message)}: {message}")

    def on_note_modified(self):
        pass  # does not matter to this orchestrator right now


class ScriptedNoteScriptWrapper(CompiledScript):
    def __init__(self, actor_ref, my_note, topic, mqtt, script_path):
        super().__init__(actor_ref, my_note, topic, mqtt, script_path)

    def get_script(self) -> str:
        with open(self.script_path) as f:
            script = f.read()

        return script
