import logging
import os
from time import ctime
from typing import Tuple

from ruamel.yaml import YAML


# https://docs.python.org/3/library/functions.html#open
# FIXME: can this open the note once and flush where it relies on quick open+closes? could make this a context manager
class NoteAPI:
    def __init__(self, note_path):
        self.note_path = note_path
        self.note_name = os.path.splitext(os.path.split(note_path)[1])[0]
        self.yaml = YAML(typ='safe')  # FIXME - round trip is probably preferable; also, is this thread safe?

    def append(self, string):
        with open(self.note_path, 'a') as f:
            f.write(string)

    def set_file_contents(self, contents):
        with open(self.note_path, 'w') as f:
            f.write(contents)

    def set_markdown(self, markdown) -> None:
        raw_front_matter = self.get_raw_frontmatter().rstrip()
        with open(self.note_path, 'w') as f:
            f.write(f"---\n"
                    f"{raw_front_matter}\n"
                    f"---\n"
                    f"{markdown}")

    def already_exists(self) -> bool:
        return os.path.exists(self.note_path)

    def note_if_markdown_starts_with_pressed_button(self) -> None | Tuple[object, str]:
        with open(self.note_path) as f:
            try:
                frontmatter_start = next(f)
            except StopIteration:
                return None

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

        if markdown.startswith("- [x] "):
            return self.yaml.load(frontmatter), markdown
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
        with open(self.note_path) as f:
            try:
                frontmatter_start = next(f)
            except StopIteration:
                return None

            if frontmatter_start != "---\n":
                logging.warning(f"Expected frontmatter but first line was {frontmatter_start}")
                return None

            frontmatter_lines = []
            for line in f:
                if line == "---\n":
                    # done with frontmatter, no need to read the markdown here
                    break
                frontmatter_lines.append(line)

        return ''.join(frontmatter_lines)

    def get_frontmatter(self):
        return self.yaml.load(self.get_raw_frontmatter())

    def append_timestamped_markdown_list_line(self, line) -> None:
        self.append(timestamped_markdown_list_line(line))

    def frontmatter_and_markdown_if_button_pressed(self) -> None | Tuple[object, str]:
        return self.note_if_markdown_starts_with_pressed_button()

    def append_md_ll(self, line) -> None:
        self.append_timestamped_markdown_list_line(line)

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

    # def simple_reset_button_at_start_of_markdown_and_append(self, line):
    #     with open(self.note_path) as f:
    #         pass  # FIXME - or standard upserters...?


def timestamped_markdown_list_line(line) -> str:
    return f"- \\[{ctime()}] {line}\n"
