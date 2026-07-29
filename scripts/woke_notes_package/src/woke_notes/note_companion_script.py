import logging
import os
from time import ctime
from typing import TypeVar

from pykka import ActorRef
from watchdog.events import FileModifiedEvent, FileCreatedEvent

from .woke_note import WokeNote, MqttWrapper
from .wrappers.note_api import NoteAPI
from .wrappers.scripting import ScriptHarness, ScriptPath
from .wrappers.external_messages import MqttPublish
from .wrappers.file_watcher import FolderWatcher

NCS = TypeVar("NCS", bound="NoteCompanionScript")


class NoteCompanionScript(WokeNote):
    def __init__(self, note_name, script_path: ScriptPath):
        super().__init__(note_name)

        self.script_path = script_path

        # this is defined in on_start
        self.script_wrapper: ScriptedNoteHarness = None

    def on_start(self):
        # defines note_api
        super().on_start()

        self.script_wrapper = ScriptedNoteHarness(
            self.actor_ref, self.my_note,
            self.topic, self.mqtt,
            self.support.vault_router,
            self.script_path,
        )
        self.script_wrapper.on_start()

    def on_receive(self, message: object):
        logging.info(f"CANARY [[{self.note_name}]]")
        # super on_receive ignored intentionally, those things are managed explicitly here

        if (isinstance(message, FileModifiedEvent)
                # FIXME: on_note_created ?
                or isinstance(message, FileCreatedEvent)):
            logging.info(f"CANARY [[{self.note_name}]] calling on_note_modified()")
            self.script_wrapper.on_note_modified()
        elif isinstance(message, MqttPublish):
            self.script_wrapper.on_mqtt_message(message.topic, message.payload)
        elif message == "SCRIPT_MODIFIED":
            logging.info(f"Hot reloading [[{self.note_name}]]")
            self.script_wrapper.compile_script(this_is_a_recompile=True)
        else:
            try:
                message_type, key, payload = message
                if message_type == "TIMER":
                    self.script_wrapper.on_timer(key, payload)
                else:
                    logging.warning(f"Unexpected type {message_type}:- {message}")
            except ValueError:
                logging.warning(f"Unexpected type {type(message)} {message}")

    def on_stop(self):
        self.script_wrapper.on_stop()


class TemplateScript:
    def __init__(self, script_name: str) -> None:
        """script name is WITHOUT extension (since they're all py right now anyway), located in SCRIPTS/templates"""
        self.script_name = script_name


class NoteCompanionScriptsManager(WokeNote):
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
            actor_ref = NoteCompanionScript.wake(note_name, ScriptPath(self.scripts_dir, note_name))
            self.woke_notes[note_name] = actor_ref

        self.__update_note()

    def on_receive(self, message: object):
        if isinstance(message, FileModifiedEvent):
            if message.src_path.startswith(self.support.vault_path):
                self.on_note_modified()
            elif message.src_path.startswith(self.scripts_dir):
                _, filename = os.path.split(message.src_path)
                maybe_note_name, maybe_md = os.path.splitext(filename)
                if maybe_md.lower() == ".py":
                    try:
                        self.woke_notes[maybe_note_name].tell("SCRIPT_MODIFIED")
                    except KeyError:
                        logging.warning(f"{maybe_note_name} was not in {list(self.woke_notes.keys())}")
                        raise
                    logging.warning(f"NOT HOT RELOADING {message.src_path}")
                else:
                    logging.info(f"Did not recognize file ext {maybe_md}, ignoring {message.src_path}")
            else:
                logging.warning(
                    f"unexpected modified path `{message.src_path}` (did not start with scripts dir `{self.scripts_dir}` or vault dir `{self.support.vault_path}`)")
        elif isinstance(message, FileCreatedEvent):
            if message.src_path.startswith(self.support.vault_path):
                pass  # FIXME - on_note_created event?
            elif message.src_path.startswith(self.scripts_dir):
                script_path_to_wake = message.src_path
                note_name: str = os.path.splitext(os.path.split(script_path_to_wake)[1])[0]
                if note_name in self.woke_notes:
                    logging.warning(f"Got a file created event for script {script_path_to_wake} but it was already created (ignoring)")
                else:
                    logging.info(f"Detected new script {script_path_to_wake}, loading it and adding it for hotreloading...")
                    actor_ref = NoteCompanionScript.wake(note_name, script_path_to_wake)
                    self.woke_notes[note_name] = actor_ref
                    self.__update_note()
            else:
                logging.warning(
                    f"unexpected new path `{message.src_path}` (did not start with scripts dir `{self.scripts_dir}` or vault dir `{self.support.vault_path}`)")
        elif isinstance(message, MqttPublish):
            self.on_mqtt_message(message.topic, message.payload)
        else:
            logging.warning(f"Unexpected message, type {type(message)}: {message}")

    def on_note_modified(self) -> None:
        pass  # does not matter to this orchestrator right now

    def __update_note(self) -> None:
        scripted_notes_list = "\n".join(f"    - [[{nn}]]" for nn in self.woke_notes.keys())
        self.my_note.set_file_contents(f"""- generated {ctime()}
- scripted notes:
{scripted_notes_list}
""")

    def on_stop(self):
        self.scripts_watcher.stop()
        logging.info(f"Stopped scripts watcher, stopping {len(self.woke_notes)} notes...")
        for actor in self.woke_notes.values():
            actor.stop()
        logging.info("Successfully stopped!")


class ScriptedNoteHarness(ScriptHarness):
    def __init__(self, actor_ref: ActorRef, my_note: NoteAPI, topic: str, mqtt: MqttWrapper, vault_router: ActorRef, script_path: ScriptPath):
        super().__init__(actor_ref, my_note, topic, mqtt, vault_router, script_path)
        self._scripts = []
        self.script_scope["wake"] = self.__wake_subordinate
        self.script_scope["TemplateScript"] = TemplateScript

    def __wake_subordinate(self, filename: str, template_script: TemplateScript) -> ActorRef:
        if isinstance(template_script, TemplateScript):
            script = NoteCompanionScript.wake(  # FIXME: fk wake->start if possible!
                filename,
                ScriptPath(os.path.join(self.script_path.scripts_dir, "templates"), template_script.script_name)
            )
            self._scripts.append(script)
            return script
        else:
            raise NotImplementedError

    def get_script(self) -> str:
        with open(self.script_path.script_path) as f:
            script = f.read()

        return script

    def on_stop(self) -> None:
        logging.info(f"stopping ScriptedNoteHarness")
        try:
            for subordinate in self._scripts:
                subordinate.stop()
            self.script_scope["on_stop"]()
        except Exception as e:
            logging.exception(f"on_stop call failed {e}")
