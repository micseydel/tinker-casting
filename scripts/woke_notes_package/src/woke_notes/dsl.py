"""
used for typing, not at runtime

runtime comes from scriping.CompiledScript.script_scope
"""

import logging
import json
import datetime
from time import ctime, sleep

import requests

from .util import TimeUtil, Clock

from .woke_note import MqttWrapper
from .wrappers.note_api import datetimestamped_markdown_list_line, timestamped_markdown_list_line, NoteAPI

my_note: NoteAPI
topic: str
default_topic: str
mqtt: MqttWrapper

_clock = Clock()
_timeutils = TimeUtil(_clock)
next_occurrence = _timeutils.next_occurrence
seconds_until = _timeutils.seconds_until
today = _clock.today


def set_timer(delay_seconds: float | int, payload: bytes | None = None, key: str | None = None) -> None: pass


def cancel_timer(key: str) -> None: pass


def subscribe_internal(topic: str, subscriber) -> None: pass


def publish_internal(topic: str, payload: bytes) -> None: pass


def on_mqtt_message(topic: str, payload: bytes) -> None: pass


def on_note_modified() -> None: pass


def on_start() -> None: pass


def on_timer(key: str, payload: object) -> None: pass


def publish_to_ntfy(channel: str, message: str) -> None: pass
