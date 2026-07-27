import datetime

from utils import MockTimer, MockNtfy, harness
from woke_notes.wrappers.note_api import MockedNoteAPI

NOTE_NAME = "Next Notes Mockup"
TODAY = datetime.date(2026, 7, 17)
NOW = datetime.datetime(2026, 7, 18, 17, 35)


def raw_test_note(next_date="2026-07-19",
                  history=("2026-07-12", "2026-07-05"),
                  interval=7):
    hist = "\n".join(f"- [[{NOTE_NAME} ({d})]]" for d in history)
    return f"""---
default_interval_days: {interval}
---
- [ ] Create next: [[{NOTE_NAME} ({next_date})]]

## History

{hist}
"""


def pressed(raw):
    return raw.replace("- [ ] ", "- [x] ")


def build_harness(mocked_note, timer, mock_ntfy):
    return harness(
        mocked_note,
        TODAY,
        NOW,
        timer,
        mock_ntfy.publish_to_ntfy,
    )


def test_inert_on_note_modified():
    # No file at all -> button is not "pressed" -> nothing happens.
    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(NOTE_NAME, None)

    script_scope = build_harness(mocked_note, timer, mock_ntfy)
    script_scope["on_note_modified"]()

    assert timer.perviously_set is None
    assert mock_ntfy.called is None
    assert not mocked_note.file_contents_set


def test_pressed_creates_advances_and_records():
    # monkeypatch.chdir(tmp_path)

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    mocked_note = MockedNoteAPI(NOTE_NAME, pressed(raw_test_note("2026-07-19")))

    script_scope = build_harness(mocked_note, timer, mock_ntfy)
    script_scope["on_note_modified"]()

    # No timer/ntfy side effects for this note.
    assert timer.perviously_set is None
    assert mock_ntfy.called is None

    # Button advanced by default_interval_days (7); prior next-note prepended.
    assert mocked_note.file_contents_set == raw_test_note(
        "2026-07-26",
        history=("2026-07-19", "2026-07-12", "2026-07-05"),
    )

    # The next note was created on disk (via open(), per the recorded decision).
    # created = tmp_path / f"{NOTE_NAME} (2026-07-19).md"
    # assert created.read_text() == (
    #     "---\n"
    #     "created: 2026-07-17\n"
    #     "---\n"
    #     f"# {NOTE_NAME} (2026-07-19)\n"
    #     "\n"
    #     f"(created by [[{NOTE_NAME}]])\n"
    # )


def test_pressed_skips_history_when_already_present():
    timer = MockTimer()
    mock_ntfy = MockNtfy()
    # The next note is already recorded in History.
    mocked_note = MockedNoteAPI(
        NOTE_NAME,
        pressed(raw_test_note("2026-07-19", history=("2026-07-19", "2026-07-12"))),
    )

    script_scope = build_harness(mocked_note, timer, mock_ntfy)
    script_scope["on_note_modified"]()

    # Button still advances, but History is not duplicated.
    assert mocked_note.file_contents_set == raw_test_note(
        "2026-07-26",
        history=("2026-07-19", "2026-07-12"),
    )


def test_pressed_appends_to_empty_history():

    timer = MockTimer()
    mock_ntfy = MockNtfy()
    raw = f"""---
default_interval_days: 7
---
- [ ] Create next: [[{NOTE_NAME} (2026-07-19)]]

## History
"""
    mocked_note = MockedNoteAPI(NOTE_NAME, pressed(raw))

    script_scope = build_harness(mocked_note, timer, mock_ntfy)
    script_scope["on_note_modified"]()

    # With no existing entries, the wikilink is appended after the header.
    assert mocked_note.file_contents_set == (
        "---\n"
        "default_interval_days: 7\n"
        "---\n"
        f"- [ ] Create next: [[{NOTE_NAME} (2026-07-26)]]\n"
        "\n"
        "## History\n"
        "\n"
        f"- [[{NOTE_NAME} (2026-07-19)]]"
    )
    # assert (tmp_path / f"{NOTE_NAME} (2026-07-19).md").exists()
