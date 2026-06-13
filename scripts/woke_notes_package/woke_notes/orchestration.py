import logging
import os

import pykka
from watchdog.events import FileModifiedEvent

from literate_note import LiterateNote
from note_api import NoteAPI
from wrappers.external_messages import ExternalMessages, MqttSubscription
from wrappers.file_watcher import VaultWatcher, FolderWatcher, VaultNoteSubscription


def formatted_note_contents(woke_notes) -> str:
    # fixme when I have non-literate WokeNotes, include them as a separate section
    formatted_woke_notes = "\n".join(f"    - [[{note}]]" for note in woke_notes)
    return f"""- [ ] (inert button to later gate hot reloading)
- see also:
    - [[Woke Notes Mqtt Orchestrator]]
    - [[Woke Notes VaultWatcher]]
- Literate Notes:
{formatted_woke_notes}"""


class Orchestrator(pykka.ThreadingActor):
    def __init__(self, mqtt_config, vault_path, scripts_dir):
        # this runs in the spawner's context, whereas on_start runs in the actor thread
        super().__init__()
        self.my_note = None
        self.vault_watcher = None
        self.scripts_watcher = None
        self.mqtt = None

        self.vault_path = vault_path
        self.scripts_dir = scripts_dir
        # rstrip() here is a hacky way to get os.path.split to treat the directory like a file
        self.vault_name = os.path.split(vault_path.rstrip("/"))[1]
        self.mqtt_config = mqtt_config

        self.woke_notes = {}  # from note name to ActorRef

    def on_start(self):
        self.my_note = NoteAPI(os.path.join(self.vault_path, "Woke Notes Orchestrator.md"))
        self.vault_watcher = VaultWatcher.start(self.vault_path)
        self.scripts_watcher = FolderWatcher.start(self.scripts_dir, ".py", self.actor_ref)
        self.mqtt = ExternalMessages.start(self.mqtt_config, os.path.join(self.vault_path, "Woke Notes Mqtt Orchestrator.md"))

        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')

        python_scripts = [script for script in os.listdir(self.scripts_dir) if script.lower().endswith(".py")]
        note_names_for_scripts = [os.path.splitext(script)[0] for script in python_scripts]

        logging.info(f"Starting with scripts {python_scripts}; spawning WokeNotes now")
        # FIXME: populate a note that doesn't start the hotreloading until a button is pushed

        for note_name in note_names_for_scripts:
            topic = f"{self.vault_name}/[[{note_name}]]"
            self.woke_notes[note_name] = LiterateNote.start(self.vault_path, self.scripts_dir, note_name, self.mqtt, topic)
            self.vault_watcher.tell(VaultNoteSubscription(note_name, self.woke_notes[note_name]))
            self.mqtt.tell(MqttSubscription(topic, self.woke_notes[note_name]))
            logging.info(f"Subscribed [[{note_name}]] to topic {topic}")

        self.my_note.set_contents(formatted_note_contents(self.woke_notes.keys()))

    def on_receive(self, message):
        if isinstance(message, FileModifiedEvent):
            if message.src_path.startswith(self.scripts_dir):
                _, filename = os.path.split(message.src_path)
                maybe_note_name, maybe_md = os.path.splitext(filename)
                if maybe_md.lower() == ".py":
                    self.woke_notes[maybe_note_name].tell("SCRIPT_MODIFIED")
                    logging.warning(f"NOT HOT RELOADING {message.src_path}")
                else:
                    logging.info(f"Did not recognize file ext {maybe_md}, ignoring {message.src_path}")
            else:
                logging.warning(f"unexpected path `{message.src_path}` (did not start with scripts dir `{self.scripts_dir}`)")
        else:
            logging.warning(f"Unexpected message, type {type(message)}: {message}")

    def on_stop(self):
        pass
