import hashlib
import logging
import os
import sys
from time import ctime, time, sleep

from kokoro import KPipeline
import soundfile as sf

from woke_notes import WokeProcess, get_config_from_env, WokeNote
from woke_notes import woke_process

from constants import *  # FIXME

NOTE_NAME = "Kokoro Woke Note (TESTING)"


class CompletedTTS:
    def __init__(self, voice: str, text: str, output_dir: str, wavs: list, cached: bool):
        # FIXME: date!
        self.voice = voice
        self.text = text
        self.output_dir = output_dir
        self.wavs = wavs
        self.cached = cached

    def __str__(self):  # FIXME: dataclasses for this? Python version?
        return f"CompletedTTS({self.voice}, {self.text}, {self.output_dir}, {self.wavs}, {self.cached})"

    def to_markdown(self, vault_path):
        lines = [f"- ({self.voice}) `{self.text}`"]

        for wav in self.wavs:
            lines.append(f"    - ![[{self.output_dir.replace(vault_path, '').lstrip('/')}/{wav}]]")

        return "\n".join(lines)


class WokeKokoroProcess(WokeProcess):
    def on_start(self):
        super().on_start()
        if not self.my_note.already_exists():
            self.my_note.set_file_contents(DEFAULT_CONTENTS)
        else:
            config = self.my_note.get_frontmatter()
            if (output_dir := config.get("output_dir")) is not None:
                existing_work_folders = os.listdir(output_dir)
                if existing_work_folders:
                    existing_work = []
                    for folder in existing_work_folders:
                        # logging.warning(f"Folder: {folder}")
                        voice, hash = folder.split("__")
                        full_folder_path = os.path.join(output_dir, folder)
                        wavs = os.listdir(full_folder_path)
                        wavs.remove("meta.json")
                        with open(os.path.join(output_dir, folder, "meta.json")) as f:
                            meta = json.load(f)
                            text = meta["text"]

                        existing_work.append(CompletedTTS(voice, text, full_folder_path, wavs, cached=True))

                    markdown_table = "\n".join(
                        existing.to_markdown(self.support.vault_path) for existing in existing_work)

                    self.my_note.set_markdown(
                        f"- [ ] Generate\n\n"
                        f"# Existing\n\n"
                        f"{markdown_table}\n\n"
                        f"# Voices\n\n"
                        f"- from https://huggingface.co/hexgrad/Kokoro-82M/blob/main/VOICES.md on 2026-06-01\n\n"
                        f"{VOICES}\n"
                    )

    def on_note_modified(self):
        """pass the Frontmatter config to the worker process on button push"""
        maybe_note = self.my_note.note_if_markdown_starts_with_pressed_button()
        if maybe_note is not None:
            config, markdown = maybe_note
            self.worker_conn.send(config)
            sleep(0.25)  # just in case 🙄
            # FIXME: ideally this would wait until the work item is done
            self.my_note.reset_button_at_start_of_markdown()

    def on_work_item_complete(self, result):
        completed_tts: CompletedTTS
        play_after_creation: bool
        completed_tts, play_after_creation = result

        # FIXME: stop ignoring after the first wav, even though it's so much easier
        first_wav = completed_tts.wavs[0]
        # FIXME: do more of an upsert!
        if play_after_creation:
            self.my_note.append_timestamped_markdown_list_line(
                f"Playing {first_wav} (completed_tts.cached={completed_tts.cached}")
            self.mqtt.publish("[[Sound Player]]", os.path.join(completed_tts.output_dir, first_wav))
        else:
            if not completed_tts.cached:
                self.my_note.append_timestamped_markdown_list_line(f"Created {first_wav}")
            else:
                self.my_note.append_timestamped_markdown_list_line(f"Already cached: {first_wav}")


def get_from_cache_or_generate(pipeline: KPipeline, voice: str, text: str, output_dir: str) -> CompletedTTS:
    m = hashlib.sha256()
    m.update(text.encode())
    text_hash = m.hexdigest()

    output_dir_for_this_text = os.path.join(output_dir, f"{voice}__{text_hash}")
    if os.path.isdir(output_dir_for_this_text):
        raw_files = os.listdir(output_dir_for_this_text)
        if "meta.json" in raw_files:
            raw_files.remove("meta.json")
        else:
            # FIXME: this is just a re-generation if the file is lost
            with open(os.path.join(output_dir_for_this_text, "meta.json"), "w") as f:
                json.dump({"text": text, "generated": ctime(), "regenerated": True}, f)
        raw_files.sort(key=lambda x: int(x.split("_")[0]))
        list(map(lambda filename: os.path.join(output_dir_for_this_text, filename), raw_files))
        return CompletedTTS(voice, text, output_dir_for_this_text, raw_files, True)
    else:
        os.mkdir(output_dir_for_this_text)
        logging.info(f"Doing voice {voice}... ")
        start = time()
        generator = pipeline(text, voice=voice)
        i = None
        raw_files = []
        for i, (gs, ps, audio) in enumerate(generator):
            wav_filename = f"{i}_{voice}_.wav"
            wav_path = os.path.join(output_dir_for_this_text, wav_filename)
            raw_files.append(wav_path)
            logging.info(f"((i, gs, ps = {i}, {gs}, {ps})) About to write {voice} output to {wav_path}")
            sf.write(wav_path, audio, 24000)
        took = time() - start
        logging.info(f"took {took}s, wrote {i + 1} wav files, about to dump the text as meta...")
        with open(os.path.join(output_dir_for_this_text, "meta.json"), "w") as f:
            json.dump({"text": text, "generated": ctime()}, f)

        return CompletedTTS(voice, text, output_dir_for_this_text, raw_files, False)


class Worker:
    def __init__(self, worker_conn):
        self.worker_conn = worker_conn
        # a = 'American English'
        self.pipeline: KPipeline = KPipeline(
            lang_code='a',
            repo_id='hexgrad/Kokoro-82M'
        )

    def process_item(self, config):
        voice = config["voice"]
        text = config["text"]
        output_dir = config["output_dir"]

        tts: CompletedTTS = get_from_cache_or_generate(self.pipeline, voice, text, output_dir)
        play_after_creation = config.get("play_after_creation", False)

        self.worker_conn.send((tts, play_after_creation))


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    try:
        _, vault_path = sys.argv
    except ValueError:
        print("Expected two CLI args, vault path and then scripts dir\n", file=sys.stderr)
        raise

    mqtt_config = get_config_from_env()

    logging.info(
        f"Using vault {vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}")

    WokeNote.start_background_actors(vault_path, mqtt_config)

    try:
        woke_process.runner(NOTE_NAME, WokeKokoroProcess, Worker)
    finally:
        WokeNote.stop_background_actors()
