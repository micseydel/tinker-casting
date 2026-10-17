import datetime
import re
from collections import defaultdict
from dataclasses import dataclass

DATE_TIME_FORMAT = "%Y-%m-%dT%H:%M:%S"


@dataclass(frozen=True, eq=True)
class DatedMarkdownBullet:
    date: datetime.date
    contents: str
    ref: str
    block_id: str

    def to_markdown(self) -> str:
        # FIXME: figure out all the contents.strip() calls
        return f"- \\[{self.date}] {self.contents.strip()} ([[{self.ref}|ref]]) ^{self.block_id}"

    @staticmethod
    def from_markdown(markdown: str) -> 'DatedMarkdownBullet':
        raw_date, contents, ref, block_id = line_to_4parts(markdown, FULL_PATTERN)
        date = datetime.date.fromisoformat(raw_date)
        return DatedMarkdownBullet(date, contents.strip(), ref, block_id)


@dataclass(frozen=True, eq=True)
class TimestampedMarkdownBullet:
    time: datetime.datetime
    contents: str
    ref: str
    block_id: str

    def to_markdown(self) -> str:
        formatted_time = self.time.strftime(DATE_TIME_FORMAT)
        return f"- \\[{formatted_time}] {self.contents.strip()} ([[{self.ref}|ref]]) ^{self.block_id}"

    @staticmethod
    def from_markdown(markdown: str) -> 'TimestampedMarkdownBullet':
        raw_time, contents, ref, block_id = line_to_4parts(markdown, FULL_PATTERN)
        time = datetime.datetime.strptime(raw_time, DATE_TIME_FORMAT)
        return TimestampedMarkdownBullet(time, contents.strip(), ref, block_id)


@dataclass(frozen=True, eq=True)
class TimestampedMarkdownBulletWithoutBlockId:
    time: datetime.datetime
    contents: str
    ref: str

    def to_markdown(self) -> str:
        formatted_time = self.time.strftime(DATE_TIME_FORMAT)
        return f"- \\[{formatted_time}] {self.contents.strip()} ([[{self.ref}|ref]])"

    @staticmethod
    def from_markdown(markdown: str) -> 'TimestampedMarkdownBulletWithoutBlockId':
        raw_time, contents, ref = line_to_3parts(markdown, PATTERN_WITHOUT_BLOCK_ID)
        time = datetime.datetime.strptime(raw_time, DATE_TIME_FORMAT)
        return TimestampedMarkdownBulletWithoutBlockId(time, contents.strip(), ref)


###


# - \[2026-09-10T07:47:58] ('discarded', '166') ([[#^1789051678|ref]]) ^block_id
FULL_PATTERN = re.compile(r"- \\\[([^]]+)\]\s*"
                          r"(.+)\s*"  # r"(\([^)]+\))\s*"
                          r"\(\[\[([^|]+)\|ref\]\]\)\s*"  # r"\(\[\[#\^([^|]+)\|ref\]\]\)\s*"
                          r"\^(.+)$"  # r"\^(.+)?"
                          )

# - \[2026-09-10T07:47:58] ('discarded', '166') ([[#^1789051678|ref]])
PATTERN_WITHOUT_BLOCK_ID = re.compile(r"- \\\[([^]]+)\]\s*"
                                      r"(.+)\s*"
                                      r"\(\[\[([^|]+)\|ref\]\]\)"
                                      )


def line_to_3parts(line, pattern: re.Pattern) -> tuple[str, str, str]:
    match = pattern.search(line)
    if match:
        time_part, contents, ref = match.groups()
        return time_part, contents, ref

    raise ValueError(f"Invalid line format; line: {line}")


def line_to_4parts(line, pattern: re.Pattern) -> tuple[str, str, str, str]:
    match = pattern.search(line)
    if match:
        time_part, contents, ref, block_id = match.groups()
        return time_part, contents, ref, block_id

    raise ValueError(f"Invalid line format; line: {line}")


def lines_by_toplevel_sections(markdown: str) -> dict:
    by_toplevel_sections = defaultdict(list)

    section = ""
    for line in markdown.splitlines():
        if line.startswith("# "):
            section = line[2:]
        else:
            by_toplevel_sections[section].append(line)

    return by_toplevel_sections
