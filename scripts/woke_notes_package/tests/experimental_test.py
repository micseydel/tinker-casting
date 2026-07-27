import datetime

from utils import MockTimer, MockNtfy, harness
from woke_notes.wrappers.note_api import MockedNoteAPI

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


def test_donothing():
    note_name = "Example - Daily Responsibility"
    today = datetime.date(2026, 7, 17)

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(note_name, None)

    harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 17, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    assert timer.perviously_set is None
    assert mock_ntfy.called is None
    assert not mocked_note.file_contents_set


def test_inert_start_with_existing_contents():
    note_name = "Example - Daily Responsibility"
    today = datetime.date(2026, 7, 17)
    raw_note_contents = raw_test_note(today.isoformat())

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(note_name, raw_note_contents)

    harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 17, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    args, kwargs = timer.perviously_set

    assert args == (27695, None)
    assert kwargs == {"key": 'Example - Daily Responsibility/TIMER'}

    assert mock_ntfy.called is None

    assert not mocked_note.file_contents_set


def test_WAT():
    note_name = "Example - Daily Responsibility"
    # yesterday = datetime.date(2026, 7, 16)
    today = datetime.date(2026, 7, 17)
    raw_note_contents = raw_test_note(today.isoformat()).replace("- [ ] ", "- [x] ")

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(note_name, raw_note_contents)

    harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 0, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    assert timer.perviously_set[0] == (88895, None)
    assert timer.perviously_set[1].get("key") == 'Example - Daily Responsibility/TIMER'

    assert mock_ntfy.called is None

    assert not mocked_note.file_contents_set


def test_inert_on_note_modified():
    note_name = "Example - Daily Responsibility"
    today = datetime.date(2026, 7, 17)

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(note_name, None)

    script_scope = harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 17, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    script_scope["on_note_modified"]()

    assert timer.perviously_set is None
    assert mock_ntfy.called is None
    assert not mocked_note.file_contents_set


def test_resetbutton_on_note_modified():
    note_name = "Example - Daily Responsibility"
    today = datetime.date(2026, 7, 17)

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(
        note_name,
        raw_test_note(today.isoformat()).replace("- [ ] ", "- [x] ")
    )

    script_scope = harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 17, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    timer.previously_set = None
    script_scope["on_note_modified"]()

    args, kwargs = timer.perviously_set
    # FIXME: this is successing only because of start()!
    assert args == (27695, None)
    assert kwargs == {"key": 'Example - Daily Responsibility/TIMER'}

    assert mock_ntfy.called is None
    assert mocked_note.file_contents_set == raw_test_note(today.isoformat())


def test_complex_on_note_modified():
    note_name = "Example - Daily Responsibility"

    yesterday = datetime.date(2026, 7, 16)
    today = datetime.date(2026, 7, 17)

    base_raw_note = raw_test_note(yesterday.isoformat())

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(
        note_name,
        base_raw_note.replace("- [ ] ", "- [x] ")
    )

    script_scope = harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 17, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    timer.previously_set = None
    script_scope["on_note_modified"]()

    args, kwargs = timer.perviously_set
    assert args == (27695, None)
    assert kwargs == {"key": 'Example - Daily Responsibility/TIMER'}

    assert mock_ntfy.called is None

    lines = base_raw_note.split("\n")
    lines.insert(6, "- [[Example - Daily Responsibility (2026-07-17)]]")

    assert mocked_note.file_contents_set == '\n'.join(lines)


def test_on_timer():
    note_name = "Example - Daily Responsibility"
    today = datetime.date(2026, 7, 17)

    timer = MockTimer()
    mock_ntfy = MockNtfy()

    yesterday = datetime.date(2026, 7, 16)
    base_raw_note = raw_test_note(yesterday.isoformat())
    mocked_note = MockedNoteAPI(note_name, base_raw_note)

    script_scope = harness(
        mocked_note,
        today,
        datetime.datetime(2026, 7, 18, 17, 35),
        timer,
        mock_ntfy.publish_to_ntfy,
    )

    timer.previously_set = None

    script_scope["on_timer"](None, 'Example - Daily Responsibility/TIMER')

    assert timer.perviously_set[0][0] == 27695
    assert mock_ntfy.called == ("THE_CHANNEL", "(canary)")
    assert not mocked_note.file_contents_set
