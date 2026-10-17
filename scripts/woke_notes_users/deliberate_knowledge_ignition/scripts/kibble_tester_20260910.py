from datetime import timedelta

if TYPE_CHECKING:  # IDE support
    from nervous_notes.dsl import *

import os
import json
from glob import glob

vault_json = "/Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion/json"


def on_start():
    # note = my_note.get_note()
    # if note is None:
    #     my_note.set_markdown("- [ ] publish all")
    logging.warning("note was empty")


def on_note_modified():
    logging.info("Note modified...")
    if (note := my_note.note_if_markdown_starts_with_pressed_button()):
        frontmatter, markdown = note

        assert frontmatter is not None, "expected frontmatter with `start` isodate field"

        start: datetime.date = frontmatter["start"]
        stop: datetime.date = frontmatter["stop"]

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

        published = 0
        candidates.sort()
        for candidate in candidates:
            with open(candidate) as f:
                for line in f:
                    # logging.info("!!! found a line!")
                    if "LargeModel" in line and ("dry food" in line.lower() or "kibble" in line.lower()):
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
                            "noteId": note_id,
                        }
                        mqtt.publish("[[Kibble Measurements Actor]]", json.dumps(data))
                        published += 1
        logging.info(f"!!! published {published} events !!!")

        my_note.reset_button_at_start_of_markdown()
