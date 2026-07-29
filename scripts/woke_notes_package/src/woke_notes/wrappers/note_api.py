import logging
import os
from time import ctime
from typing import Tuple

from ruamel.yaml import YAML


class PrimitiveNoteAPI:
    note_path: str
    note_name: str

    def __init__(self, note_path: str) -> None:
        self.note_path = note_path
        self.note_name = os.path.splitext(os.path.split(note_path)[1])[0]

    def get_file_contents(self) -> str | None:
        try:
            with open(self.note_path) as f:
                return f.read()
        except FileNotFoundError:
            return None

    def set_file_contents(self, contents: str) -> None:
        with open(self.note_path, 'w') as f:
            f.write(contents)

    def append(self, string: str) -> None:
        with open(self.note_path, 'a') as f:
            f.write(string)

    def already_exists(self) -> bool:
        return os.path.exists(self.note_path)


def string_to_genlines(string):
    """
    hack that may be replaced, so that I can keep existing code that uses the lines of a file as a generator
    """
    return iter(string.splitlines(keepends=True))


class NoteAPI:
    def __init__(self, note_api: PrimitiveNoteAPI) -> None:
        self.note_api = note_api
        self.note_name = note_api.note_name
        self.note_path = note_api.note_path

        # FIXME - round trip is probably preferable; also, is this thread safe?
        self.yaml = YAML(typ='safe')

    # composition rather than inheritance

    def get_file_contents(self) -> str | None:
        return self.note_api.get_file_contents()

    def set_file_contents(self, contents: str) -> None:
        self.note_api.set_file_contents(contents)

    def append(self, string: str) -> None:
        self.note_api.append(string)

    def already_exists(self) -> bool:
        return self.note_api.already_exists()

    # helpful utils

    def set_markdown(self, markdown: str) -> None:
        raw_front_matter = None
        try:
            raw_front_matter = self.get_raw_frontmatter()
        except FileNotFoundError:
            pass

        if raw_front_matter is not None:
            self.set_file_contents(
                f"---\n"
                f"{raw_front_matter.rstrip()}\n"
                f"---\n"
                f"{markdown}"
            )
        else:
            self.set_file_contents(markdown)

    def get_note(self) -> None | Tuple[object | None, str]:
        contents = self.get_file_contents()
        if contents is None:
            return None

        lines = string_to_genlines(contents)

        try:
            maybe_frontmatter_start = next(lines)
        except StopIteration:
            return None, ""

        frontmatter = None
        if maybe_frontmatter_start == "---\n":
            frontmatter_lines = []
            for line in lines:
                if line == "---\n":
                    # done with frontmatter
                    break
                frontmatter_lines.append(line)

            frontmatter = self.yaml.load(''.join(frontmatter_lines))
        else:
            raise Exception("no end to frontmatter")  # FIXME

        markdown = ''.join(lines)  # remaining
        return frontmatter, markdown

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
        contents = self.get_file_contents()
        if contents is None:
            return None

        lines = string_to_genlines(contents)

        frontmatter_start = next(lines)
        if frontmatter_start != "---\n":
            logging.warning(f"Expected frontmatter but first line was {frontmatter_start}")
            return None

        frontmatter_lines = []
        for line in lines:
            if line == "---\n":
                # done with frontmatter
                break
            frontmatter_lines.append(line)

        frontmatter = ''.join(frontmatter_lines)
        markdown_lines = list(lines)
        markdown_lines[0] = markdown_lines[0].replace("- [x]", "- [ ]")

        # FIXME: use upsert_markdown
        self.set_file_contents(f"""---
{frontmatter}---
{''.join(markdown_lines)}""")

    def upsert_markdown(self, upserter):
        contents = self.get_file_contents()
        if contents is None:
            return None

        lines = string_to_genlines(contents)
        # FIXME - this should only open the file once! instead of get+set
        frontmatter_start = next(lines)
        if frontmatter_start != "---\n":
            logging.warning(f"Expected frontmatter but first line was {frontmatter_start}")
            return None

        frontmatter_lines = []
        for line in lines:
            if line == "---\n":
                # done with frontmatter
                break
            frontmatter_lines.append(line)

        frontmatter = ''.join(frontmatter_lines)
        markdown = ''.join(lines)  # remaining

        self.set_file_contents(f"""---
{frontmatter}---
{upserter(markdown)}""")


    def get_raw_frontmatter(self) -> str | None:
        contents = self.get_file_contents()
        if contents is None:
            return None

        lines = string_to_genlines(contents)

        return _raw_note_to_frontmatter_and_markdown(lines)[0]


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
        contents = self.get_file_contents()
        if contents is None:
            return None

        lines = string_to_genlines(contents)

        try:
            maybe_frontmatter_start = next(lines)
        except StopIteration:
            return None

        # just get past the front matter
        if maybe_frontmatter_start == "---\n":
            for line in lines:
                if line == "---\n":
                    # done with frontmatter
                    break
            markdown = ''.join(lines)
        else:
            markdown = maybe_frontmatter_start + ''.join(lines)

        if markdown.startswith("- [x] "):
            return markdown
        else:
            return None


def datetimestamped_markdown_list_line(line: str) -> str:
    return f"- \\[{ctime()}] {line}\n"


def timestamped_markdown_list_line(line) -> str:
    # FIXME: use proper formatting
    return f"- \\[{ctime()[11:19]}] {line}\n"


def _raw_note_to_frontmatter_and_markdown(raw_note_lines) -> Tuple[str | None, str]:
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

        frontmatter = ''.join(frontmatter_lines)
    else:
        raise Exception("no end to frontmatter")

    markdown = ''.join(raw_note_lines)  # remaining
    return frontmatter, markdown


class MockedNoteAPI:
    def __init__(self, note_name, raw_note: str | None) -> None:
        self.note_name = note_name
        self.note_path = f"MOCKED/{note_name}"

        # yaml = YAML(typ='safe')
        self.raw_note = raw_note

        self.file_contents_set = None

    def get_file_contents(self) -> str | None:
        return self.raw_note

    def set_file_contents(self, contents: str) -> None:
        self.file_contents_set = contents

    def append(self, string: str) -> None:
        raise NotImplementedError("append")

    def already_exists(self) -> bool:
        return self.raw_note is not None
