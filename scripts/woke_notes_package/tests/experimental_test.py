import datetime
import json
import logging
import os
import time

from woke_notes.util import TimeUtil
from woke_notes.wrappers.note_api import MockedNoteAPI, datetimestamped_markdown_list_line, \
    timestamped_markdown_list_line

if_not_done_by = "18:16:35-07:00"


def raw_test_note(iso_date, if_not_done_by=if_not_done_by): return f"""---
ifNotDoneBy: {if_not_done_by}
channel: THE_CHANNEL
message: "(canary)"
---
- [ ] Mark as done
- [[Example - Daily Responsibility ({iso_date})]]
- [[Example - Daily Responsibility (2026-07-13)]]
"""


class MockTimer:
    def __init__(self):
        self.perviously_set = False

    def set_timer(self, *args, **kwargs):
        # FIXME *args
        self.perviously_set = True


class MockClock:
    def __init__(self, _now: datetime.datetime, _today: datetime.date):
        self._now = _now
        self._today = _today

    def now(self, tz) -> datetime.datetime:
        return self._now

    def today(self, tz) -> datetime.date:
        return self._today


def gen_script_scope(note_name, set_timer, my_note, today) -> dict:
    now = datetime.datetime(2026, 7, 18, 17, 35)
    if now.tzinfo is None:
        now = now.replace(tzinfo=datetime.timezone.utc)

    # noinspection PyTypeChecker
    timeutil = TimeUtil(MockClock(now, now.date))

    return {
        "TYPE_CHECKING": False,

        "logging": logging,
        "ctime": time.ctime,
        "json": json,
        "sleep": lambda _: None,
        # "requests": requests,
        "datetime": datetime,

        # utils
        "note_name": note_name,
        "set_timer": set_timer,
        # FIXME: are these used?
        "datetimestamped_markdown_list_line": datetimestamped_markdown_list_line,
        "timestamped_markdown_list_line": timestamped_markdown_list_line,
        "publish_to_ntfy": None,  # FIXME

        # FIXME: keep these, right?
        "next_occurrence": timeutil.next_occurrence,
        "seconds_until": timeutil.seconds_until,

        # shortcuts
        "today": today,

        "my_note": my_note,
    }


def test_EXPERIMENT():
    note_name = "Example - Daily Responsibility"
    script_path = os.path.join(os.path.split(__file__)[0], f"../src/woke_notes/example_scripts/{note_name}.py")

    today = datetime.date(2026, 7, 17)

    my_note = MockedNoteAPI(note_name, raw_test_note(today.isoformat()))

    timer = MockTimer()
    script_scope: dict = gen_script_scope(note_name, timer.set_timer, my_note, lambda: today)

    with open(script_path) as f:
        script = f.read()
    compiled_script = compile(script, script_path, "exec")
    exec(compiled_script, script_scope)

    ##

    # FIXME: test that empty note does nothing

    print("should be False:", timer.perviously_set)
    script_scope["on_start"]()
    assert timer.perviously_set

    # timer.perviously_set = False
    #
    # script_scope["on_note_modified"]()
    #
    # timer.perviously_set = False
    #
    # script_scope["on_timer"]()
