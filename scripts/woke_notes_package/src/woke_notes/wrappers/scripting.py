import datetime
import json
import logging
import threading
import time
import requests
from typing import Dict
from typing import TYPE_CHECKING  # FIXME: remove

from pykka import ActorRef

from ..util import Clock, TimeUtil
from ..vault_router import Subscribe, Publish
from ..woke_note import MqttWrapper
from ..wrappers.note_api import NoteAPI, datetimestamped_markdown_list_line, timestamped_markdown_list_line


class ScriptHarness:
    my_note: NoteAPI

    def __init__(self, actor_ref: ActorRef, my_note: NoteAPI, topic: str, mqtt: MqttWrapper, vault_router: ActorRef, script_path: str):
        self.my_note = my_note
        self.script_path = script_path
        self.actor_ref = actor_ref

        clock = Clock()
        timeutil = TimeUtil(clock)

        # keep in line with dsl.py!
        self.script_scope: dict = {
            "TYPE_CHECKING": TYPE_CHECKING,  # FIXME: this should just be False, right?

            "logging": logging,
            "ctime": time.ctime,
            "json": json,
            "sleep": time.sleep,
            "requests": requests,
            "datetime": datetime,
            "today": clock.today,

            # utils
            "mqtt": mqtt,
            "note_name": my_note.note_name,
            # "vault_name": self.vault_name,
            "topic": topic,  # FIXME: this should change with the frontmatter
            "set_timer": self.set_timer,
            "cancel_timer": self.cancel_timer,
            "datetimestamped_markdown_list_line": datetimestamped_markdown_list_line,
            "timestamped_markdown_list_line": timestamped_markdown_list_line,
            "subscribe_internal": lambda t, s: vault_router.tell(Subscribe(t, s)),
            "publish_internal": lambda t, p: vault_router.tell(Publish(t, p)),
            "publish_to_ntfy": publish_to_ntfy,
            "next_occurrence": timeutil.next_occurrence,
            "seconds_until": timeutil.seconds_until,

            "my_note": my_note,
            # DSL and lifecycle tinkering - to be expanded as needed
            "default_topic": topic,
            "on_mqtt_message": lambda t, message: None,
            "on_note_modified": lambda: None,
            "on_start": lambda: None,
            "on_timer": lambda key, payload: None,
        }

        self.prior_script = None
        self.compiled_script = None
        self.recompile_script()
        self.timers: Dict[str, threading.Timer] = {}

    def get_script(self) -> str:
        raise NotImplementedError("subclasses need to implement this")

    def set_timer(self, delay_seconds: float | int, payload: bytes | None = None, key: str | None = None) -> None:
        if key is not None and key in self.timers:
            self.timers.pop(key).cancel()

        timer = threading.Timer(delay_seconds, lambda: self.actor_ref.tell(("TIMER", key, payload)))
        timer.start()
        if key is not None:
            self.timers[key] = timer

    def cancel_timer(self, key: str) -> None:
        if key in self.timers:
            timer = self.timers.pop(key)
            timer.cancel()

    def recompile_script(self) -> None:
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

    def on_start(self) -> None:
        try:
            self.script_scope["on_start"]()
        except Exception as e:
            logging.exception(f"on_start call failed {e}")

    def on_note_modified(self) -> None:
        try:
            self.script_scope["on_note_modified"]()
        except Exception as e:
            logging.exception(f"on_note_modified call failed {e}")

    def on_mqtt_message(self, topic: str, payload: bytes) -> None:
        try:
            self.script_scope["on_mqtt_message"](topic, payload)
        except Exception as e:
            logging.exception(f"on_mqtt_message call failed {e}")

    def on_timer(self, key: str, payload: object) -> None:
        try:
            self.script_scope["on_timer"](key, payload)
        except Exception as e:
            logging.exception(f"on_timer call failed {e}")


# FIXME: move to "services" or something, with weather, PurpleAir, etc
def publish_to_ntfy(channel: str, message: str) -> None:
    # from https://docs.ntfy.sh/publish/
    requests.post(f"https://ntfy.sh/{channel}", data=message.encode(encoding='utf-8'))
