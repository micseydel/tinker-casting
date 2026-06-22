import json
import logging
import threading
import time

from wrappers.note_api import NoteAPI, timestamped_markdown_list_line


class CompiledScript:
    my_note: NoteAPI

    def __init__(self, actor_ref, my_note, topic, mqtt, script_path):
        self.my_note = my_note
        self.script_path = script_path
        self.actor_ref = actor_ref
        self.script_scope: dict = {
            "logging": logging,
            "ctime": time.ctime,
            "json": json,
            "sleep": time.sleep,

            # utils
            "mqtt": mqtt,
            "note_name": my_note.note_name,
            # "vault_name": self.vault_name,
            "topic": topic,  # FIXME: this should change with the frontmatter
            "set_timer": self.set_timer,
            "timestamped_markdown_list_line": timestamped_markdown_list_line,

            "my_note": my_note,
            # DSL and lifecycle tinkering - to be expanded as needed
            "default_topic": topic,
            "on_mqtt_message": lambda t, message: None,
            "on_note_modified": lambda: None,
            "on_start": lambda: None,
            "on_timer": lambda payload: None,
        }

        self.prior_script = None
        self.compiled_script = None
        self.recompile_script()

    def get_script(self) -> str:
        raise NotImplementedError("subclasses need to implement this")

    def set_timer(self, delay_seconds: float, payload: bytes):
        threading.Timer(delay_seconds, lambda: self.actor_ref.tell(("TIMER", payload))).start()

    def recompile_script(self):
        script = self.get_script()
        if script == self.prior_script:
            logging.debug("ignoring recompilation request, script is unchanged (even though markdown was changed)")
            return

        try:
            self.compiled_script = compile(script, self.script_path, "exec")
        except SyntaxError as e:
            logging.exception(f"Ignored {self.script_path} read {len(script)} bytes but there was a syntax error")
        except Exception as e:
            logging.exception(f"Something went wrong ({e}) with the script {self.script_path}")
        else:
            logging.info(f"[[{self.my_note.note_name}#Code]] recompilation complete, executing to update the scope now...")
            # this may update the scope with potentially new on* event functions
            exec(self.compiled_script, self.script_scope)
            self.prior_script = script

    def on_start(self):
        try:
            self.script_scope["on_start"]()
        except Exception as e:
            logging.exception(f"on_start call failed {e}")

    def on_note_modified(self):
        try:
            self.script_scope["on_note_modified"]()
        except Exception as e:
            logging.exception(f"on_note_modified call failed {e}")

    def on_mqtt_message(self, topic, payload):
        try:
            self.script_scope["on_mqtt_message"](topic, payload)
        except Exception as e:
            logging.exception(f"on_mqtt_message call failed {e}")

    def on_timer(self, payload):
        try:
            self.script_scope["on_timer"](payload)
        except Exception as e:
            logging.exception(f"on_timer call failed {e}")
