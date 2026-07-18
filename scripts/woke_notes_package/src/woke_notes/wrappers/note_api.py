import logging
import os
from time import ctime
from typing import Tuple

from ruamel.yaml import YAML


# https://docs.python.org/3/library/functions.html#open
# FIXME: can this open the note once and flush where it relies on quick open+closes? could make this a context manager
class NoteAPI:
    def __init__(self, note_path: str) -> None:
        self.note_path = note_path
        self.note_name = os.path.splitext(os.path.split(note_path)[1])[0]
        self.yaml = YAML(typ='safe')  # FIXME - round trip is probably preferable; also, is this thread safe?

    def append(self, string: str) -> None:
        with open(self.note_path, 'a') as f:
            f.write(string)

    def set_file_contents(self, contents: str) -> None:
        with open(self.note_path, 'w') as f:
            f.write(contents)

    def set_markdown(self, markdown: str) -> None:
        raw_front_matter = None
        try:
            raw_front_matter = self.get_raw_frontmatter()
        except FileNotFoundError:
            pass

        if raw_front_matter is not None:
            with open(self.note_path, 'w') as f:
                f.write(f"---\n"
                        f"{raw_front_matter.rstrip()}\n"
                        f"---\n"
                        f"{markdown}")
        else:
            with open(self.note_path, 'w') as f:
                f.write(markdown)

    def already_exists(self) -> bool:
        return os.path.exists(self.note_path)

    def get_note(self) -> None | Tuple[object | None, str]:
        try:
            with open(self.note_path) as f:
                try:
                    maybe_frontmatter_start = next(f)
                except StopIteration:
                    return None, ""

                frontmatter = None
                if maybe_frontmatter_start == "---\n":
                    frontmatter_lines = []
                    for line in f:
                        if line == "---\n":
                            # done with frontmatter
                            break
                        frontmatter_lines.append(line)

                    frontmatter = self.yaml.load(''.join(frontmatter_lines))
                else:
                    raise Exception("no end to frontmatter")

                markdown = ''.join(f)  # remaining
                return frontmatter, markdown
        except FileNotFoundError:
            return None

    def note_if_markdown_starts_with_pressed_button(self) -> None | Tuple[object | None, str]:
        maybe_note = self.get_note()
        if maybe_note is not None:
            frontmatter, markdown = maybe_note
            if markdown.startswith("- [x] "):
                return frontmatter, markdown
            else:
                return None
        else:
            return None

    def reset_button_at_start_of_markdown(self):
        with open(self.note_path) as f:
            frontmatter_start = next(f)
            if frontmatter_start != "---\n":
                logging.warning(f"Expected frontmatter but first line was {frontmatter_start}")
                return None

            frontmatter_lines = []
            for line in f:
                if line == "---\n":
                    # done with frontmatter
                    break
                frontmatter_lines.append(line)

            frontmatter = ''.join(frontmatter_lines)
            markdown_lines = list(f)
            markdown_lines[0] = markdown_lines[0].replace("- [x]", "- [ ]")

        with open(self.note_path, 'w') as f:
            f.write(
                f"""---
{frontmatter}---
{''.join(markdown_lines)}""")

    def upsert_markdown(self, upserter):
        with open(self.note_path) as f:
            frontmatter_start = next(f)
            if frontmatter_start != "---\n":
                logging.warning(f"Expected frontmatter but first line was {frontmatter_start}")
                return None

            frontmatter_lines = []
            for line in f:
                if line == "---\n":
                    # done with frontmatter
                    break
                frontmatter_lines.append(line)

            frontmatter = ''.join(frontmatter_lines)
            markdown = ''.join(f)  # remaining

        with open(self.note_path, 'w') as f:
            f.write(
                f"""---
{frontmatter}---
{upserter(markdown)}""")

    def get_raw_frontmatter(self) -> str | None:
        try:
            with open(self.note_path) as f:
                _raw_note_to_note(f, self.yaml)
        except FileNotFoundError:
            return None

    def get_frontmatter(self) -> object | None:
        raw = self.get_raw_frontmatter()
        if raw is None:
            return None
        # FIXME: does this really return Any?
        return self.yaml.load(raw)

    def append_timestamped_markdown_list_line(self, line) -> None:
        self.append(timestamped_markdown_list_line(line))

    def append_datetimestamped_markdown_list_line(self, line) -> None:
        self.append(datetimestamped_markdown_list_line(line))

    def frontmatter_and_markdown_if_button_pressed(self) -> None | Tuple[object, str]:
        return self.note_if_markdown_starts_with_pressed_button()

    def append_md_ll(self, line) -> None:
        self.append_datetimestamped_markdown_list_line(line)

    def markdown_if_starts_with_pressed_button(self):
        with open(self.note_path) as f:
            try:
                maybe_frontmatter_start = next(f)
            except StopIteration:
                return None

            # just get past the front matter
            if maybe_frontmatter_start == "---\n":
                for line in f:
                    if line == "---\n":
                        # done with frontmatter
                        break
                markdown = ''.join(f)
            else:
                markdown = maybe_frontmatter_start + ''.join(f)

        if markdown.startswith("- [x] "):
            return markdown
        else:
            return None


def datetimestamped_markdown_list_line(line: str) -> str:
    return f"- \\[{ctime()}] {line}\n"


def timestamped_markdown_list_line(line) -> str:
    # FIXME: use proper formatting
    return f"- \\[{ctime()[11:19]}] {line}\n"


def _raw_note_to_note(raw_note_lines, yaml) -> Tuple[object, str]:
    try:
        maybe_frontmatter_start = next(raw_note_lines)
    except StopIteration:
        return None, ""

    frontmatter = None
    if maybe_frontmatter_start == "---\n":
        frontmatter_lines = []
        for line in raw_note_lines:
            if line == "---\n":
                # done with frontmatter
                break
            frontmatter_lines.append(line)

        frontmatter = yaml.load(''.join(frontmatter_lines))
    else:
        raise Exception("no end to frontmatter")

    markdown = ''.join(raw_note_lines)  # remaining
    return frontmatter, markdown


class MockedNoteAPI:
    def __init__(self, note_name, raw_note: str) -> None:
        self.note_name = note_name

        yaml = YAML(typ='safe')
        lines = raw_note.splitlines(keepends=True)
        # print(lines)
        self.note = _raw_note_to_note(iter(lines), yaml)

    def get_note(self) -> object | None:
        return self.note
