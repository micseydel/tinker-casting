import logging

from watchdog.events import FileModifiedEvent

from woke_note import WokeNote
from wrappers.external_messages import MqttPublish
from wrappers.scripting import CompiledScript


class LiterateNotesManager(WokeNote):
    def __init__(self, note_name, *args, **kwargs):
        super().__init__(note_name, *args, **kwargs)

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
            LiterateNote.wake(note_name)


class LiterateNote(WokeNote):
    def __init__(self, note_name):
        super().__init__(note_name)

        # these are defined in on_start
        self.script_path: str = None
        self.script_wrapper: LiterateNoteScriptWrapper = None

    def on_start(self):
        # defines note_api
        super().on_start()

        self.script_wrapper = LiterateNoteScriptWrapper(self.actor_ref, self.my_note, self.topic, self.mqtt)
        self.script_wrapper.on_start()

    def on_receive(self, message):
        # super on_receive ignored intentionally, those things are managed explicitly here

        if isinstance(message, FileModifiedEvent):
            logging.debug(f"[LiterateNote.on_receive] calling on_note_modified")
            try:
                self.script_wrapper.on_note_modified()
                self.script_wrapper.recompile_script()  # FIXME hacky
            except Exception as e:
                logging.exception(f"Something unexpected happened when trying to hot reload {e}")
        elif isinstance(message, MqttPublish):
            logging.debug(f"[LiterateNote.on_receive] calling on_mqtt_message")
            self.script_wrapper.on_mqtt_message(message.topic, message.payload)
        else:
            try:
                message_type, payload = message
                if message_type == "TIMER":
                    logging.debug(f"[LiterateNote.on_receive] calling on_timer")
                    self.script_wrapper.on_timer(payload)
                else:
                    logging.warning(f"Unexpected type {message_type}:- {message}")
            except ValueError:
                logging.warning(f"Unexpected type {type(message)} {message}")


class LiterateNoteScriptWrapper(CompiledScript):
    def __init__(self, actor_ref, my_note, topic, mqtt):
        super().__init__(actor_ref, my_note, topic, mqtt, f"{my_note.note_path}#Code")

    def get_script(self) -> str:
        with open(self.my_note.note_path) as f:
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

        return script
