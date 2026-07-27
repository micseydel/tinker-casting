# Next Notes Mockup — decisions & choice points

Log of the non-obvious decisions made while implementing
`src/woke_notes/example_scripts/Next Notes Mockup.py` and
`tests/next_notes_mockup_test.py` from the prompt in
`Next Notes (2026-07-27).md`.

## The one genuine ambiguity (asked, not guessed)

**"Create the next note" has no cross-note write API in scope.** The harness
(`tests/utils.py::gen_script_scope`) binds `my_note` to the *current* note only,
and injects no vault/create-note helper. So "create the next note" cannot be
done through `my_note`.

- Options considered: (a) `logging.warning` stub like the calendar line, (b)
  actually create via `open()` — overriding the "use `my_note` not `open()`"
  constraint for this one step, (c) invent + inject a new note-creation API.
- **Decision: (b) — create via `open()`.** Chosen by the user when asked. The
  "use `my_note` rather than `open()`" constraint therefore applies only to
  reads/writes of the *current* note; the *next* note is written with `open()`.
- Consequence: the created file lands relative to CWD (no vault path is
  available in scope). Tests isolate this with `monkeypatch.chdir(tmp_path)` so
  no artifacts pollute the repo.

## Behavior of `on_note_modified` when the button is pressed

Given a pressed dashboard note like:

```
---
default_interval_days: 7
---
- [x] Create next: [[Next Notes Mockup (2026-07-19)]]

## History

- [[Next Notes Mockup (2026-07-12)]]
- [[Next Notes Mockup (2026-07-05)]]
```

the function:

1. Reads the next-note name from the wikilink in the button line
   (`Next Notes Mockup (2026-07-19)`).
2. Creates that note on disk via `open()` (see decision above) with a minimal
   template: its own `created:` frontmatter, an `# H1`, and a backlink to the
   dashboard.
3. `logging.warning`s that a calendar event should have been created but wasn't.
4. Advances the button's date by `frontmatter.get("default_interval_days")`
   (7 → `2026-07-26`), resetting `- [x]` back to `- [ ]` and **preserving the
   label text** (`Create next: `) rather than hard-coding it.
5. Prepends `- [[Next Notes Mockup (2026-07-19)]]` to `## History`, **only if
   not already present**.
6. Writes the current note back via `my_note.set_markdown(...)`, which preserves
   the existing frontmatter.

## Smaller choice points

- **No `if TYPE_CHECKING: from ..dsl import *` guard.** The reference scripts
  include it for IDE support, but the script is `exec`'d in a scope that already
  provides every global it uses. Under plain `coverage.py` (no
  `[tool.coverage]` config exists in `pyproject.toml`, no `pytest-cov`), that
  guard's body is an unreachable/uncovered line and would block the required
  100%. Omitted deliberately; a comment in the file explains why.
- **`on_start` is a no-op.** Unlike `Example - Daily Responsibility`, this note
  has no timer to arm; everything is driven by the button. It still must be
  defined because the harness calls `script_scope["on_start"]()` and
  `gen_script_scope` provides no default.
- **String handling uses `split("\n")` / `"\n".join(...)`** (not `splitlines()`)
  so trailing newlines round-trip exactly, keeping output byte-stable for the
  test assertions.
- **History insertion** finds the first existing `- [[` entry after the
  `## History` header and inserts before it; if there are none, it appends. This
  avoids a blank-line-sensitivity branch and keeps both paths testable.
- **Next date parsing** assumes the wikilink ends in ` (YYYY-MM-DD)` and bumps
  it with `datetime.timedelta`, reusing the link's base name.

## Tests (target: 100% coverage of the implementation)

`tests/next_notes_mockup_test.py`, modeled on
`test_resetbutton_on_note_modified`:

- `test_inert_on_note_modified` — no file → button not pressed → nothing happens
  (covers the `if note:` false branch).
- `test_pressed_creates_advances_and_records` — happy path; asserts the rewritten
  dashboard *and* the created next-note file contents.
- `test_pressed_skips_history_when_already_present` — next note already in
  History → button advances but History isn't duplicated (covers
  `history_line not in lines` false).
- `test_pressed_appends_to_empty_history` — `## History` with no entries →
  wikilink appended (covers the "no existing entry" append branch).

Coverage was NOT run inside Claude Code (user prefers to review/run in their
IDE). If confirming from the CLI: `coverage run -m pytest
tests/next_notes_mockup_test.py && coverage report --include='*Next Notes Mockup.py'`.

## Process note (for the experiment)

I over-explored before implementing on the first pass (read `dsl.py`,
`scripting.py`, and extra example scripts the prompt didn't require). The prompt
already supplied the skeleton, note format, required behaviors, the full
`NoteAPI` surface, target path, reference test, and coverage bar — enough to
implement after reading only the harness, `MockedNoteAPI`/`NoteAPI`, and the
reference test/script. The correct move for the lone real ambiguity was to
*ask* (via a question), not to *search the codebase* for an answer that wasn't
in it.
