from datetime import timedelta

if TYPE_CHECKING:  # IDE support
    from ..dsl import *

DEFAULT_INTERVAL_DAYS = 7


def on_start():
    if my_note.already_exists():
        return

    today_today = today()
    my_note.set_file_contents(f"""---
default_interval_days: {DEFAULT_INTERVAL_DAYS}
---
- [ ] Create next: [[{my_note.note_name} ({today_today.isoformat()})]]

## History

- [[{my_note.note_name} ({(today_today + timedelta(days=-DEFAULT_INTERVAL_DAYS)).isoformat()})]]
- [[{my_note.note_name} ({(today_today + timedelta(days=-DEFAULT_INTERVAL_DAYS*2)).isoformat()})]]
- (anything from the line above downward is basically ignored)
- (once you create one real link, e.g. with the button, you can delete these extra lines with no problem)
""")


def on_note_modified():
    logging.info("[Next-Notes Moc.py:on_note_modified] CANARY")
    note = my_note.note_if_markdown_starts_with_pressed_button()
    if note:
        frontmatter, markdown = note

        logging.info("Button pressed! ...")

        lines = markdown.split("\n")
        button = lines[0]

        # The next note to create is named by the wikilink in the button line,
        # e.g. "- [x] Create next: [[Next Notes Mockup (2026-07-19)]]".
        next_target = _wikilink_target(button)

        logging.warning(
            f"The user needs to manually create: a calendar event(?) and [[{next_target}]]. Any Next* aliasing has also been left untouched."
        )
        # FIXME: here I can wake() the next target, if only to move the alias along... how to model that?

        # Advance the button to the note after this one, keeping the label text.
        interval_days = frontmatter.get("default_interval_days", 7)
        following_target = _bump_target(next_target, interval_days)
        label = button[len("- [x] "):button.index("[[")]
        lines[0] = f"- [ ] {label}[[{following_target}]]"

        # Record the just-created note at the top of History, if not already there.
        history_line = f"- [[{next_target}]]"
        if history_line not in lines:
            sleep(0.25)
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


def _prepend_to_history(lines: list, history_line: str) -> None:
    """Insert `history_line` before the first existing History entry (or append)."""
    header_index = lines.index("## History")
    insert_at = len(lines)
    for i in range(header_index + 1, len(lines)):
        if lines[i].startswith("- [["):
            insert_at = i
            break
    lines.insert(insert_at, history_line)
