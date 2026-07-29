import logging
import os
from time import ctime

from .note_embedded_script import NoteEmbeddedScriptsManager
from .note_companion_script import NoteCompanionScriptsManager
from .woke_note import WokeNote


NoteCompanionScriptsManager_NOTE_NAME = "Note Companion Scripts Manager"
NoteEmbeddedScriptsManager_NOTE_NAME = "Note Embedded Scripts Manager"


class Orchestrator(WokeNote):
    """
    This is the top level orchestrator - it starts (and links to) FIXME
    """

    def __init__(self, note_name, scripts_dir, *args, **kwargs):
        # this runs in the spawner's context, whereas on_start runs in the actor thread
        super().__init__(note_name, *args, **kwargs)

        self.scripts_dir = scripts_dir
        self.started = []

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='[%(asctime)s %(pathname)s:%(lineno)s] - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')
        super().on_start()

        if self.scripts_dir is not None:
            self.started.append(NoteCompanionScriptsManager.wake(NoteCompanionScriptsManager_NOTE_NAME, scripts_dir=self.scripts_dir))

        experimental_embedded_scripts_note_path = os.path.join(self.support.vault_path, f"{NoteEmbeddedScriptsManager_NOTE_NAME}.md")
        deploy_experimental_embedded_script_notes = os.path.isfile(experimental_embedded_scripts_note_path)

        # FIXME: update for no scripts dir
        self.my_note.set_file_contents(f"""- generated {ctime()}
- [[{NoteCompanionScriptsManager_NOTE_NAME}]]
- [[{NoteEmbeddedScriptsManager_NOTE_NAME}]]
""" if deploy_experimental_embedded_script_notes else f"""- generated {ctime()}
- [[{NoteCompanionScriptsManager_NOTE_NAME}]]
""")

        if deploy_experimental_embedded_script_notes:
            self.started.append(NoteEmbeddedScriptsManager.wake(NoteEmbeddedScriptsManager_NOTE_NAME))

    def on_note_modified(self):
        pass  # ignore

    def on_stop(self):
        total = len(self.started)
        logging.info(f"About to stop {total} woke notes")
        for i, started in enumerate(self.started, 1):
            logging.info(f"Stopping {started} ({total - i} remaining)")
            started.stop()
        logging.info(f"Stopped {total} woke notes")
