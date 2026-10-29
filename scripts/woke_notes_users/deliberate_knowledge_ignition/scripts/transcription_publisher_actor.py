from datetime import timedelta

if TYPE_CHECKING:  # IDE support
    from nervous_notes.dsl import *

import os
import json
from glob import glob

vault_json = "/Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion/json"


def on_start():
    note = my_note.get_note()
    if note is None:
        my_note.set_file_contents("""\
---
start: 2026-10-24
stop: 2026-10-29
publish_to_note_id: note_id_without_brackets
publish_to_mqtt_topic:
---
- [ ] publish all

> [!info] Frontmatter
> - start and stop are required dates
> - publish_to_note_id and publish_to_mqtt_topic are both optional but nothing happens without one
> - must_contain_one_of is an optional list of words

""")
    logging.warning("note was empty")


def on_note_modified():
    # logging.info("Note modified...")
    if (note := my_note.note_if_markdown_starts_with_pressed_button()):
        frontmatter, markdown = note

        assert frontmatter is not None, "expected frontmatter with `start` isodate field"

        start: datetime.date = frontmatter["start"]
        stop: datetime.date = frontmatter["stop"]
        publish_to_note_id = frontmatter.get("publish_to_note_id")
        publish_to_mqtt_topic = frontmatter.get("publish_to_mqtt_topic")
        must_contain_one_of = frontmatter.get("must_contain_one_of")

        if not publish_to_note_id and not publish_to_mqtt_topic:
            logging.warning(f"needed at least one of publish_to_note_id or publish_to_mqtt_topic")
            return

        candidates = []

        day_to_process = start
        while day_to_process <= stop:
            year = day_to_process.year
            month = day_to_process.month
            day = day_to_process.day

            # e.g. transcription_for_mobile_audio_capture_20260909-104628.wav.json
            globbing_for = os.path.join(vault_json, f"transcription_for_mobile_audio_capture_{year}{month:02}{day:02}-*.json")
            globbed = glob(globbing_for)
            candidates.extend(globbed)

            day_to_process += timedelta(days=1)

        logging.info(f"!!! gathered {len(candidates)} candidates !!!")

        sleep(1)  # wait a bit for startup

        published_to_mqtt = 0
        batch_to_publish = []
        candidates.sort()
        for candidate in candidates:
            with open(candidate) as f:
                for line in f:
                    must_contain_one_of_gate = True
                    if must_contain_one_of:
                        must_contain_one_of_gate = any(word in line.lower() for word in must_contain_one_of)
                    if "LargeModel" in line and must_contain_one_of_gate:
                        event = json.loads(line)
                        before, capture_daytime = candidate.split('-')
                        capture_time = capture_daytime.split('.')[0]
                        note_id = f"Transcription for mobile_audio_capture_{before[-8:]}-{capture_time}.wav"
                        # transcription_for_mobile_audio_capture_20260909-104628.wav.json
                        # =>
                        # Transcription for mobile_audio_capture_202609104628.wav

                        date = before[-8:]
                        captureTime = capture_time[:2] + ":" + capture_time[2:4] + ":" + capture_time[4:6]
                        captureDateTime = date[:4] + '-' + date[4:6] + '-' + date[6:] + "T" + captureTime

                        data = {
                            "capture": {
                                "whisperResult": event["event"],
                                "captureTime": captureDateTime,
                            },
                            "noteId": {"id": note_id},
                        }
                        if publish_to_mqtt_topic:
                            mqtt.publish(publish_to_mqtt_topic, json.dumps(data))
                            published_to_mqtt += 1
                        if publish_to_note_id:
                            batch_to_publish.append((data))

        # FIXME any benefit to publishing length-1 as a non-batch?
        if batch_to_publish:
            batch_publish_to_nervous_note(publish_to_note_id, batch_to_publish)

        logging.info(f"!!! published {published_to_mqtt} events to {publish_to_mqtt_topic or '(no given mqtt topic)'} mqtt topic, "
                     f"{len(batch_to_publish)} to "
                     + (f"[[{publish_to_note_id}]]" if publish_to_note_id else "(no given note id)") +
                     f" !!!")

        my_note.reset_button_at_start_of_markdown()
