from datetime import timedelta
from typing import io

if TYPE_CHECKING:  # IDE support
    from dsl import *


class Frontmatter:
    pass


class Document:
    frontmatter: Frontmatter
    markdown: str


def on_start():
    note = my_note.get_note()
    if note is not None:
        frontmatter, markdown = note

        next_date = frontmatter.get("next_date")
        default_interval_days = frontmatter.get("default_interval_days")

        _button, current_next_note, *history = markdown.splitlines()

        if current_next_note[:8] != "- Next: " or not current_next_note.endswith(")]]"):
            logging.warning(f"Expected second line to start with `- Next: ` then a wikilink of the format [[Note (YYYY-MM-DD)]]")
            return

        current_next_date = current_next_note[-13:-3]
        if next_date is None:
            next_date = datetime.datetime.strptime(current_next_date, "%Y-%m-%d") + timedelta(days=default_interval_days)


def on_note_modified():
    note = my_note.note_if_markdown_starts_with_pressed_button()
    if note:
        frontmatter, markdown = note
        # FIXME
        stream = io.StringIO()
        my_note.yaml.dump(data, stream)

        # Extract the YAML string
        yaml_string = stream.getvalue()


# utils

# FIXME: factor out?
def markdown_list_prepender(markdown: str) -> str:
    """
    - reset the button
    - prepend today's date (after the button)
    """
    lines = markdown.splitlines()
    lines[0] = "- [ ] Mark as done"
    line_to_prepend = f"- [[{my_note.note_name} ({datetime.date.today().isoformat()})]]"
    lines.insert(1, line_to_prepend)
    lines[-1] = lines[-1] + "\n"
    return "\n".join(lines)
