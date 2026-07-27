if TYPE_CHECKING:  # IDE support
    from ..dsl import *

def on_start():
    logging.info(f"Started... note exists? {my_note.already_exists()} (overwriting either way - {my_note.note_api.note_path})")
    my_note.set_file_contents("""---
default_interval_days: 7
---
- [ ] Create next: [[Next Notes Mockup (2026-07-19)]]

## History
 
- [[Next Notes Mockup (2026-07-12)]]
- [[Next Notes Mockup (2026-07-05)]]
""")

def on_note_modified():
    logging.info("on_note_modified")
    note = my_note.note_if_markdown_starts_with_pressed_button()
    if note:
        frontmatter, markdown = note

        logging.info("Button pressed! ...")

        lines = markdown.split("\n")
        button = lines[0]

        # The next note to create is named by the wikilink in the button line,
        # e.g. "- [x] Create next: [[Next Notes Mockup (2026-07-19)]]".
        next_target = _wikilink_target(button)

        # Create the next note. Per the recorded decision this uses open()
        # rather than my_note, which only ever binds to the current note.
        _create_next_note(next_target)

        # A real deployment would also drop a calendar event; the mockup can't.
        logging.warning(
            f"A calendar event should have been created for [[{next_target}]] but wasn't"
        )

        # Advance the button to the note after this one, keeping the label text.
        interval_days = frontmatter.get("default_interval_days", 7)
        following_target = _bump_target(next_target, interval_days)
        label = button[len("- [x] "):button.index("[[")]
        lines[0] = f"- [ ] {label}[[{following_target}]]"

        # Record the just-created note at the top of History, if not already there.
        history_line = f"- [[{next_target}]]"
        if history_line not in lines:
            _prepend_to_history(lines, history_line)

        my_note.set_markdown("\n".join(lines))
        logging.info("Done, setting markdown...")


# utils


def _wikilink_target(text: str) -> str:
    """Return the NAME inside the first [[NAME]] wikilink in `text`."""
    start = text.index("[[") + 2
    end = text.index("]]", start)
    return text[start:end]


def _bump_target(target: str, interval_days: int) -> str:
    """"Base (YYYY-MM-DD)" -> same base with the date advanced `interval_days`."""
    base = target[:target.rindex(" (")]
    date_str = target[target.rindex("(") + 1:target.rindex(")")]
    date = datetime.datetime.strptime(date_str, "%Y-%m-%d").date()
    following = date + datetime.timedelta(days=interval_days)
    return f"{base} ({following.isoformat()})"


def _create_next_note(target: str) -> None:
    logging.warning(f"target {target} must be created manually")
    # with open(f"{target}.md", "w") as f:
    #     f.write(
    #         f"---\n"
    #         f"created: {today().isoformat()}\n"
    #         f"---\n"
    #         f"# {target}\n"
    #         f"\n"
    #         f"(created by [[{note_name}]])\n"
    #     )


def _prepend_to_history(lines: list, history_line: str) -> None:
    """Insert `history_line` before the first existing History entry (or append)."""
    header_index = lines.index("## History")
    insert_at = len(lines)
    for i in range(header_index + 1, len(lines)):
        if lines[i].startswith("- [["):
            insert_at = i
            break
    lines.insert(insert_at, history_line)
