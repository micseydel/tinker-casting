import collections
import io
import json
import logging
import os
import sys
import time
from base64 import b64encode
from collections import deque
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Generator, Union

import setproctitle
import speech_recognition as sr
from pydub import AudioSegment
from woke_notes import WokeNote, get_config_from_env

TRANSCRIBER_TOPIC = "python/transcription/large"
SAMPLE_RATE = 16000
VERBOSE = False


@dataclass(frozen=True)
class SpeechRecognizerConfig:
    energy: int = 500
    pause: float = 1.2  # 0.8
    dynamic_energy: bool = False


@dataclass(frozen=True)
class RawClip:
    path: str
    duration_seconds: float
    time_taken: datetime


class AudioCaptureAssistant:
    def __init__(self, speech_recognizer_config: SpeechRecognizerConfig, speechrec_config_update_channel: deque[SpeechRecognizerConfig], verbose: bool = False):
        self.speechrec_config_update_channel = speechrec_config_update_channel
        self.speech_recognizer_config = speech_recognizer_config
        self.verbose = verbose

        self.speech_recognizer = sr.Recognizer()
        self.speech_recognizer.energy_threshold = speech_recognizer_config.energy
        self.speech_recognizer.dynamic_energy_threshold = speech_recognizer_config.dynamic_energy
        self.pause = self.speech_recognizer.pause_threshold = speech_recognizer_config.pause

    def gen_snippet_group(self, output_folder: str) -> Generator:
        """
        Yields snippets. Does not yield pause, stop or resume CONTROL snippets.
        :param output_folder:
        :return:
        """
        try:
            with sr.Microphone(sample_rate=SAMPLE_RATE) as source:
                os.makedirs(output_folder, exist_ok=True)
                while True:
                    try:
                        new_config = self.speechrec_config_update_channel.pop()
                        if new_config != self.speech_recognizer_config:
                            logging.info(f"Updating config from {self.speech_recognizer_config} to {new_config}")
                            self.speech_recognizer_config = new_config
                            self.speech_recognizer.energy_threshold = self.speech_recognizer_config.energy
                            self.speech_recognizer.dynamic_energy_threshold = self.speech_recognizer_config.dynamic_energy
                            self.pause = self.speech_recognizer.pause_threshold = self.speech_recognizer_config.pause
                    except IndexError:
                        pass
                    # this STOPS blocking at the end of the recording plus the pause_threshold
                    audio: sr.AudioData = self.speech_recognizer.listen(source)
                    captured_time = datetime.now()
                    data = io.BytesIO(audio.get_wav_data())
                    audio_clip: AudioSegment = AudioSegment.from_file(data)

                    taken_time = captured_time - timedelta(seconds=audio_clip.duration_seconds + self.pause)
                    filename = taken_time.isoformat().replace(':', '-') + '.wav'

                    path = os.path.join(output_folder, filename)

                    audio_clip.export(path, format="wav")

                    yield RawClip(path, audio_clip.duration_seconds, taken_time)
        except KeyboardInterrupt:
            return


class WokeListener(WokeNote):
    config: SpeechRecognizerConfig

    def __init__(self, note_name: str, settings_update_channel: deque[SpeechRecognizerConfig]):
        super().__init__(note_name)
        self.speechrec_config_update_channel = settings_update_channel

    def on_start(self):
        super().on_start()

        config = self.__get_config_from_frontmatter()
        if config is None:
            logging.debug(f"No note, creating with default yaml...")
            config = SpeechRecognizerConfig()
            self.my_note.set_file_contents(f"""---
energy: 500
pause: 1.2
dynamic_energy: false
---
- \\[{time.ctime()}] config: (500, 1.2, false)
""")
        elif not isinstance(config, SpeechRecognizerConfig):
            logging.warning(f"Expected non-None {{energy, pause, dynamic_energy}} but got: {config}")
            config = SpeechRecognizerConfig()

        self.config = config

    def on_note_modified(self):
        latest_config = self.__get_config_from_frontmatter()
        if isinstance(latest_config, SpeechRecognizerConfig):
            if latest_config != self.config:
                self.config = latest_config
                logging.info("Telling the main thread the config is updated")
                self.speechrec_config_update_channel.append(latest_config)
                self.my_note.append_timestamped_markdown_list_line(f"config: ({latest_config.energy}, {latest_config.pause}, {latest_config.dynamic_energy})")
        else:
            logging.warning(f"Invalid config - something must have gone wrong since initial startup")

    def on_mqtt_message(self, topic, message):
        logging.info(f"Transcription completed...")

        payload = json.loads(message)
        # payload.get("whisperResultMetadata") # meh
        whisper_result_content = payload.get("whisperResultContent")
        text = whisper_result_content.get("text")  # ignoring segments...

        cleaned = text.strip()
        if not cleaned:
            logging.warning("🧢 no text!")
            return
        
        # FIXME: markdown? yaml?
        common_hallucinations = {"Thank you.", "you", "I'm sorry.", "Thanks for watching!", "All right.",  "Good night."}

        if cleaned in common_hallucinations:
            logging.warning("🧢 cleaned is a common hallucination and was ignored")  # FIXME: debug
            return

        self.my_note.append_timestamped_markdown_list_line(cleaned)

    def on_other_message(self, message):
        if isinstance(message, RawClip):
            logging.info(f"Processing raw clip {message.time_taken} ({message.duration_seconds}s)...")
            with open(message.path, "rb") as f:
                wav_bytes = f.read()
            b64encoded_contents = b64encode(wav_bytes).decode("utf-8")
            json_payload = {
                "responseTopic": self.default_topic,
                "vaultPath": f"(none but {message.path})",
                "b64Encoded": b64encoded_contents,
            }
            outgoing_message = json.dumps(json_payload)
            encoded = outgoing_message.encode()
            self.mqtt.publish(TRANSCRIBER_TOPIC, encoded)
        else:
            logging.warning(f"Unexpected type {type(message)} {message} (self={type(self)}/{self})")

    def __get_config_from_frontmatter(self) -> Union[SpeechRecognizerConfig, tuple[object, object, object], None]:
        frontmatter = self.my_note.get_frontmatter()
        if frontmatter is None:
            return None

        energy = frontmatter.get("energy")
        pause = frontmatter.get("pause")
        dynamic_energy = frontmatter.get("dynamic_energy")
        if energy is not None and pause is not None and dynamic_energy is not None:
            config = SpeechRecognizerConfig(energy, pause, dynamic_energy)
            return config
        else:
            return energy, pause, dynamic_energy


def main():
    # de facto single-direction, from the actor to the main thread
    speechrec_config_update_channel: deque[SpeechRecognizerConfig] = collections.deque(maxlen=1)

    listener = WokeListener.wake("Always Listening Actor Testing (2026-07-06)", speechrec_config_update_channel)

    ass = AudioCaptureAssistant(SpeechRecognizerConfig(), speechrec_config_update_channel)

    # time to the second FIXME: configure
    folder = datetime.now().isoformat()[:19].replace(":", "-")
    dir_path = os.path.join("captured", folder)
    os.mkdir(dir_path)

    logging.info("Capturing now...")
    for c in ass.gen_snippet_group(output_folder=dir_path):
        listener.tell(c)

    listener.stop()


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')

    setproctitle.setproctitle(sys.argv[0])

    try:
        _, vault_path = sys.argv
    except ValueError:
        print("Expected two CLI args, vault path and then example_scripts dir\n", file=sys.stderr)
        raise

    mqtt_config = get_config_from_env()

    logging_msg = f"Using vault {vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}"
    logging.info(logging_msg)

    # FIXME: does this require a WokeProcess ?
    WokeNote.start_background_actors(vault_path, mqtt_config)

    main()

    WokeNote.stop_background_actors()
