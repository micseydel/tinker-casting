Your task is to finish this function:

```python
def on_note_modified():
    note = my_note.note_if_markdown_starts_with_pressed_button()
    if note:
        frontmatter, markdown = note
```

The `note`'s raw text is something like

```
---
default_interval_days: 7
---
- [ ] Create next: [[Next Notes Mockup (2026-07-19)]]

## History

- [[Next Notes Mockup (2026-07-12)]]
- [[Next Notes Mockup (2026-07-05)]]

```

(This is an Obsidian note - where `---` at the top indicates YAML at the start of the markdown.)

When the button is pressed, the code needs to
- Create the next note, by reading it from the markdown button line
- Prepend a wikilink to it in the history (if not already there)
- Update the button with the new next date (using `frontmatter.get("default_interval_days"))`
- Use `logging.warning` to indicate that a calendar event should have been created but wasn't
- Use my_note rather than Python's `open()`

Here's what you need to know about my_note:

```
class NoteAPI:
    def __init__(self, primitive_note_api) -> None:
        self._primitive_note_api = primitive_note_api
        self.note_name = primitive_note_api.note_name
        self.yaml = YAML(typ='safe')

    # these primitives methods are pass-throughts for _primitive_note_api, which is dependency injected because it has side-effects; the real implementation always flushes writes

    def get_file_contents(self) -> str | None:

    def set_file_contents(self, contents: str) -> None:

    def append(self, string: str) -> None:

    def already_exists(self) -> bool:

    # helpful utils

    def set_markdown(self, markdown: str) -> None:

    def get_note(self) -> None | Tuple[object | None, str]:
        """None if the file does not exist, (yaml, markdown) if it does, where the yaml may be None (not present) but the markdown might be an empty string but not None"""

    def note_if_markdown_starts_with_pressed_button(self) -> None | Tuple[object | None, str]:

    def reset_button_at_start_of_markdown(self):

    def upsert_markdown(self, upserter):

    def get_raw_frontmatter(self) -> str | None:

    def get_frontmatter(self) -> object | None:

    def append_timestamped_markdown_list_line(self, line) -> None:

    def append_datetimestamped_markdown_list_line(self, line) -> None:

    def frontmatter_and_markdown_if_button_pressed(self) -> None | Tuple[object, str]:

    def append_md_ll(self, line) -> None:

    def markdown_if_starts_with_pressed_button(self):
```

Place the complete function file in `src/woke_notes/example_scripts/Next Notes Mockup.py`. You should also generate a test file in `tests/`, with multiple tests modeled after this different use-case but same harness:

```
def test_resetbutton_on_note_modified():  
    note_name = "Example - Daily Responsibility"  
    today = datetime.date(2026, 7, 17)  

    # these two are just to ensure they did not happen  
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
  
    script_scope["on_note_modified"]()  
  
    assert timer.perviously_set   is None
    assert mock_ntfy.called is None  

    assert mocked_note.file_contents_set == raw_test_note(today.isoformat())
```

Your test should accomplish 100% coverage of your generated implementation function. Store all decisions, choice points and such in a markdown file, `Next Notes Mockup.claudecode.md` in `claude_code_experimentation/`.