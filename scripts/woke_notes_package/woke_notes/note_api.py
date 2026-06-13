import logging

from ruamel.yaml import YAML


# FIXME: note_if_markdown_starts_with_pressed_button is being used in the Introvert script in place of something simpler


# https://docs.python.org/3/library/functions.html#open
# FIXME: can this open the note once and flush where it relies on quick open+closes?
class NoteAPI:
    def __init__(self, note_path):
        self.note_path = note_path
        self.yaml = YAML(typ='safe')  # FIXME - round trip is probably preferable; also, is this thread safe?

    def append(self, string):
        with open(self.note_path, 'a') as f:
            f.write(string)

    def set_contents(self, contents):
        with open(self.note_path, 'w') as f:
            f.write(contents)

    def note_if_markdown_starts_with_pressed_button(self):
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

    def get_frontmatter(self):
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

        frontmatter = ''.join(frontmatter_lines)

        return self.yaml.load(frontmatter)
