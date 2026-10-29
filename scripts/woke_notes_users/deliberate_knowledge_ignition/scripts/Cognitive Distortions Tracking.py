from typing import NamedTuple, Generator, Any

from nervous_notes.nervous_note_router import BatchPublishToNervousNote

from libraries.markdown import DATE_TIME_FORMAT

if TYPE_CHECKING:  # IDE support
    from nervous_notes.dsl import *


TRANSCRIPTION_TOPIC = "[[Gossiper]]/publish/WhisperLarge"


def on_start():
    mqtt.subscribe(TRANSCRIPTION_TOPIC)


def on_mqtt_message(topic_for_message: str, message: bytes):
    if topic_for_message == TRANSCRIPTION_TOPIC:
        incoming_data: dict = json.loads(message.decode())
        process_messages([incoming_data])
    else:
        logging.warning(f"ignoring message for type {topic_for_message} ({len(message)} bytes)")


def on_other_message(message: Any):
    if isinstance(message, BatchPublishToNervousNote):
        process_messages(message.messages.messages)
    else:
        logging.warning(f"ignoring message for type {type(message)} (only expected BatchPublishToNervousNote)")

##


class MatchingSegment(NamedTuple):
    segment_start: float
    text: str
    ref_id: str


class MatchingNote(NamedTuple):
    capture_datetime: datetime.datetime
    note_id: str
    # match_words -> segments
    segments: tuple[set[str], list[MatchingSegment]]


def process_messages(messages: list[dict]):
    matches: list[MatchingNote] = list(matching_notes(messages))
    if matches:
        document = Document()
        document.add(matches)

        # FIXME less important:
        #  ping [[Notification Center]]
        #  do an ack
    else:
        pass  # FIXME: vote (either way)


# FIXME: move to yaml
WATCH_WORDS = ["always", "never", "every", "should"]


class Document:
    # def add(self, note_id: str, capture_datetime: datetime.datetime, matches: tuple[set[str], list[tuple[float, str, str]]]):
    def add(self, matches: list[MatchingNote]):
        note = my_note.get_note()
        if note:
            frontmatter, markdown = note
        else:
            logging.warning(f"No note found for [[{my_note.note_name}]] so ignoring {len(matches)} potential matches")
            return

        new_markdown_parts = []

        lines_to_process = iter(markdown.strip().split('\n'))

        for before_inbox_line in lines_to_process:
            new_markdown_parts.append(before_inbox_line)
            if before_inbox_line == "# Inbox":
                new_markdown_parts.append('')
                break

        def inbox_to_blocks() -> Generator[tuple[datetime.datetime, list[str]], None, None]:
            pending_block: tuple[datetime.datetime, list[str]] = None
            for inbox_line in lines_to_process:
                if not inbox_line:
                    continue

                if inbox_line.startswith("- \\["):
                    if pending_block is not None:
                        yield pending_block
                    block_time = datetime.datetime.strptime(inbox_line[4:23], DATE_TIME_FORMAT)
                    pending_block = (block_time, [inbox_line])
                else:
                    pending_block[1].append(inbox_line)

        blocks: list[tuple[datetime.datetime, list[str]]] = list(inbox_to_blocks())
        blocks.extend(format_matches(matches))
        blocks.sort(reverse=True)

        for time, block in blocks:
            new_markdown_parts.extend(block)

        my_note.set_markdown('\n'.join(new_markdown_parts))


def format_matches(matches: list[MatchingNote]) -> Generator[tuple[datetime.datetime, list[str]], None, None]:
    for capture_datetime, note_id, (matched_words, matched_segments) in matches:
        lines_to_add = [f"- \\[{capture_datetime.strftime(DATE_TIME_FORMAT)}] {matched_words} ([[{note_id}|ref]])"]
        for start, text, block_id in matched_segments:
            lines_to_add.append(f"    - \\[{format_time(start)}] {text} ([[{note_id}#^{block_id}|ref]])")
        yield capture_datetime, lines_to_add


def format_time(seconds_float) -> str:
    whole_seconds = int(seconds_float)
    sub_seconds = int((seconds_float - whole_seconds) * 1000)

    minutes = whole_seconds // 60
    seconds = whole_seconds % 60

    return f"{minutes:02d}:{seconds:02d}.{sub_seconds:03d}"


def matching_notes(candidates: list[dict]) -> Generator[MatchingNote, None, None]:
    for candidate in candidates:
        capture = candidate["capture"]
        segments = capture["whisperResult"]["whisperResultContent"]["segments"]

        matched_words: set[str] = set()
        matched_segments = []
        for index, segment in enumerate(segments):
            lowered_text = segment["text"].lower()

            highlighted_text = lowered_text
            for word in WATCH_WORDS:
                if word in lowered_text:
                    matched_words.add(word)
                    highlighted_text = highlighted_text.replace(word, f"=={word}==")

            if lowered_text != highlighted_text:
                start = segment["start"]
                # FIXME: the block id should come with the NotedTranscription (Scala)
                #  e.g. making this 0-padded there would break things here
                block_id = f"seg-LargeModel-{index}"
                matched_segments.append(MatchingSegment(start, highlighted_text, block_id))

        if matched_segments:
            note_id = candidate["noteId"]["id"]
            capture_datetime = datetime.datetime.strptime(capture["captureTime"][:19], DATE_TIME_FORMAT)

            yield MatchingNote(capture_datetime, note_id, (matched_words, matched_segments))
