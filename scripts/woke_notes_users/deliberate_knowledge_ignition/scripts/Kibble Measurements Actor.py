from typing import Any

from nervous_notes.nervous_note_router import BatchPublishToNervousNote

from libraries.kibble.document import KibbleDocument
from libraries.kibble.kibble import mentions_kibble
from libraries.markdown import DATE_TIME_FORMAT
if TYPE_CHECKING:  # IDE support
    from nervous_notes.dsl import *

TRANSCRIPTION_TOPIC = "[[Kibble Measurements Actor]]"  # "[[Gossiper]]/publish/WhisperLarge"  # FIXME - move to frontmatter?


def on_start():
    if (note := my_note.get_note()) is not None:
        frontmatter, markdown = note
    else:
        # FIXME: since empties yaml is needed, just put a warning
        my_note.set_file_contents("""- [ ] *Reprocess*

# Computed Daily Consumption

# Extracted Events

# Raw History
""")

    mqtt.subscribe(TRANSCRIPTION_TOPIC)


def on_note_modified():
    logging.debug("Note modified...")
    note = my_note.note_if_markdown_starts_with_pressed_button()
    if note is not None:
        document: KibbleDocument = KibbleDocument(note)
        document.reprocess()
        sleep(0.25)
        my_note.set_markdown(document.to_markdown())


def on_other_message(message: Any):
    if isinstance(message, BatchPublishToNervousNote):
        process_messages(message.messages.messages)
    else:
        logging.warning(f"ignoring message for type {type(message)} (only expected BatchPublishToNervousNote)")


def on_mqtt_message(topic_for_message: str, message):
    if topic_for_message == TRANSCRIPTION_TOPIC:
        try:
            incoming_data = json.loads(message.decode())
        except Exception as e:
            logging.exception(f"something unexpected ({e}) went wrong while extracting {{note_id, lowered_text, capture_datetime}} from mqtt JSON payload")
            return

        process_messages([incoming_data])
    else:
        logging.warning(f"Unknown topic {topic_for_message}, only subscribed to {TRANSCRIPTION_TOPIC}")

###


def process_messages(messages: list[dict]):
    note = my_note.get_note()
    if note is None:
        logging.warning(f"[[{my_note.note_name}]] was empty - was it deleted?")
        return

    frontmatter, _ = note
    max_word_count = frontmatter.get("max_word_count", 100)

    document: KibbleDocument = KibbleDocument(note)

    try:
        for message in messages:
            match = extract_essentials_for_match(message, max_word_count)
            if match is not None:
                note_id, lowered_text, capture_datetime = match
                document.integrate_transcription(capture_datetime, lowered_text, note_id)

        my_note.set_markdown(document.to_markdown())
    except Exception as e:
        logging.exception(f"something unexpected ({e}) went wrong, discarding {len(messages)} messages")


def extract_essentials_for_match(message: dict, max_word_count: int) -> tuple[str, str, datetime.datetime] | None:
    note_id = message["noteId"]["id"]

    capture_datetime = None
    try:
        capture_datetime = datetime.datetime.strptime(message["capture"]["captureTime"][:19], DATE_TIME_FORMAT)
        lowered_text = message["capture"]["whisperResult"]["whisperResultContent"]["text"].lower().strip()  # FIXME
    except Exception as e:
        raise RuntimeError(f"Failure extracting essential details for note_id {note_id} (capture_datetime={capture_datetime or '<extraction failed>'})") from e
    else:
        word_count = len(lowered_text.split())
        if word_count > max_word_count or not mentions_kibble(lowered_text):
            warning_msg = (f"skipping [[{note_id}]], word_count={word_count} and "
                           f"mentions_kibble={mentions_kibble(lowered_text)} for "
                           f"{capture_datetime} (max_word_count={max_word_count})")
            logging.debug(warning_msg)
            return None

        return note_id, lowered_text, capture_datetime
