import logging
import os
from time import ctime

from literate_note import LiterateNotesManager
from scripted_note import ScriptedNotesOrchestrator
from woke_note import WokeNote


class Orchestrator(WokeNote):
    def __init__(self, note_name, scripts_dir, *args, **kwargs):
        # this runs in the spawner's context, whereas on_start runs in the actor thread
        super().__init__(note_name, *args, **kwargs)

        self.scripts_dir = scripts_dir

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')
        super().on_start()

        ScriptedNotesOrchestrator_note_name = "ScriptedNotesOrchestrator"
        LiterateNotesManager_note_name = "LiterateNotesManager (EXPERIMENTAL)"

        ScriptedNotesOrchestrator.wake(ScriptedNotesOrchestrator_note_name, scripts_dir=self.scripts_dir)

        deploy_experimental_literate_notes = os.path.isfile(
            os.path.join(self.support.vault_path, f"{LiterateNotesManager_note_name}.md"))
        self.my_note.set_contents(f"""- generated {ctime()}
- [[{ScriptedNotesOrchestrator_note_name}]]
- [[{LiterateNotesManager_note_name}]]
""" if deploy_experimental_literate_notes else f"""- generated {ctime()}
- [[{ScriptedNotesOrchestrator_note_name}]]
""")

        if deploy_experimental_literate_notes:
            LiterateNotesManager.wake(LiterateNotesManager_note_name)

    def on_note_modified(self):
        pass  # ignore

    def on_stop(self):
        pass
