import datetime
import json
import logging
import os
import time

from woke_notes.util import TimeUtil
from woke_notes.wrappers.note_api import datetimestamped_markdown_list_line, \
    timestamped_markdown_list_line, NoteAPI


class MockTimer:
    def __init__(self):
        self.perviously_set = None

    def set_timer(self, *args, **kwargs):
        self.perviously_set = (args, kwargs)


class MockClock:
    def __init__(self, _now: datetime.datetime, _today: datetime.date):
        self._now = _now
        self._today = _today

    def now(self, tz) -> datetime.datetime:
        return self._now

    def today(self, tz) -> datetime.date:
        return self._today


class MockNtfy:
    def __init__(self):
        self.called = None

    def publish_to_ntfy(self, channel, message):
        self.called = (channel, message)

def gen_script_scope(note_name, set_timer, timeutil, mock_note_primitive, today, publish_to_ntfy) -> dict:
    return {
        "TYPE_CHECKING": False,

        "logging": logging,
        "ctime": time.ctime,
        "json": json,
        "sleep": lambda _: None,
        "datetime": datetime,

        # utils
        "note_name": note_name,
        "set_timer": set_timer,
        # FIXME: are these used?
        "datetimestamped_markdown_list_line": datetimestamped_markdown_list_line,
        "timestamped_markdown_list_line": timestamped_markdown_list_line,
        "publish_to_ntfy": publish_to_ntfy,

        "next_occurrence": timeutil.next_occurrence,
        "seconds_until": timeutil.seconds_until,

        # shortcuts
        "today": today,

        "my_note": NoteAPI(mock_note_primitive),
    }


def harness(mocked_note, today, now, timer, publish_to_ntfy):
    # FIXME: now should match today
    if now.tzinfo is None:
        now = now.replace(tzinfo=datetime.timezone.utc)

    # noinspection PyTypeChecker
    timeutil = TimeUtil(MockClock(now, now.date))

    script_scope: dict = gen_script_scope(
        mocked_note.note_name,
        timer.set_timer,
        timeutil,
        mocked_note,
        lambda: today,
        publish_to_ntfy,
    )

    script_path = os.path.join(os.path.split(__file__)[0],
                               f"../src/woke_notes/example_scripts/{mocked_note.note_name}.py")

    with open(script_path) as f:
        script = f.read()
    compiled_script = compile(script, script_path, "exec")
    exec(compiled_script, script_scope)

    script_scope["on_start"]()

    return script_scope
