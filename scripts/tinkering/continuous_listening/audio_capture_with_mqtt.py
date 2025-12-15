import io
import os
import sys
import json
import shutil
import random
from tempfile import TemporaryDirectory
from dataclasses import dataclass
from string import ascii_letters
from datetime import datetime, timedelta
from typing import Optional, Generator, List
from base64 import b64encode

from pydub import AudioSegment
import speech_recognition as sr
import setproctitle

from time import perf_counter

from mqtt_manager import MqttPublisher
from util import print_with_time, TEST_TOPIC, TRANSCRIBER_TOPIC


SAMPLE_RATE = 16000

VERBOSE = False


@dataclass(frozen=True)
class SpeechRecognizerConfig:
    # defaults from main.py args
    energy: int = 500
    pause: float = 1.2  # 0.8
    dynamic_energy: bool = False


@dataclass(frozen=True)
class RawClip:
    path: str
    duration_seconds: float
    time_taken: datetime


class AudioCaptureAssistant:
    def __init__(self, mqtt_publisher: MqttPublisher, speech_recognizer_config: SpeechRecognizerConfig, verbose: bool = False):
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
            with sr.Microphone(sample_rate=SAMPLE_RATE) as source, TemporaryDirectory() as td:
                while True:
                    # this STOPS blocking at the end of the recording plus the pause_threshold
                    audio: sr.AudioData = self.speech_recognizer.listen(source)
                    captured_time = datetime.now()
                    data = io.BytesIO(audio.get_wav_data())
                    audio_clip: AudioSegment = AudioSegment.from_file(data)

                    taken_time = captured_time - timedelta(seconds=audio_clip.duration_seconds + self.pause)
                    filename = taken_time.isoformat().replace(':', '-') + '.wav'

                    os.makedirs(output_folder, exist_ok=True)
                    path = os.path.join(output_folder, filename)

                    audio_clip.export(path, format="wav")

                    yield RawClip(path, audio_clip.duration_seconds, taken_time)
        except KeyboardInterrupt:
            return


if __name__ == "__main__":
    setproctitle.setproctitle(sys.argv[0])
    try:
        _, energy, pause, dynamic_energy = sys.argv
        speech_recognizer_config = SpeechRecognizerConfig(int(energy), float(pause), bool(dynamic_energy))
    except ValueError:
        print_with_time("please provide energy (int), pause (float in seconds), dynamic_energy (boolean)")
        raise

    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # FIXME: investigate what randomness means here
    client_num = random.randint(0, 100)
    client_id = f'subscriber-{client_num}'

    mqtt_publisher = MqttPublisher(client_id, broker, port, username, password)


    ass = AudioCaptureAssistant(mqtt_publisher, speech_recognizer_config)

    # time to the second
    folder = datetime.now().isoformat()[:19].replace(":", "-")
    dir_path = os.path.join("captured", folder)
    os.mkdir(dir_path)

    print("Capturing now...")
    prior_message = None
    for c in ass.gen_snippet_group(output_folder=dir_path):
        with open(c.path, "rb") as f:
            wav_bytes = f.read()
            print(f"{c.time_taken} ({c.duration_seconds}s) - publishing {c.path}")
            encoded_contents = b64encode(wav_bytes).decode("utf-8")
            # print(type(encoded_contents), len(encoded_contents), encoded_contents[:10])
            json_payload = {
                "responseTopic": TEST_TOPIC,
                "vaultPath": f"(none but {c.path})",
                "b64Encoded": encoded_contents,
            }
            payload = outgoing_message= json.dumps(json_payload)
            result = mqtt_publisher.publish(TRANSCRIBER_TOPIC, payload.encode())
            


            status_code, message_n = result
            if status_code == 0:
                if VERBOSE: print_with_time(f"Result #{message_n} published successfully (mqtt_publisher.is_connected() = {mqtt_publisher.is_connected()})")
                response_topic = TRANSCRIBER_TOPIC
                prior_message = (message_n, response_topic, outgoing_message)
            else:
                if status_code == 7:
                    if prior_message is None:
                        print_with_time("Status code 7, but no prior message!")
                    else:
                        print_with_time(f"Result #{message_n} failed publishing to {response_topic} with status_code=7, will reconnect, then retry the PRIOR message then the current one")
                else:
                    print_with_time(f"Result #{message_n} failed publishing ({status_code}) (mqtt_publisher.is_connected={mqtt_publisher.is_connected()}), trying to reconnect now...")

                mqtt_publisher.reconnect()

                if status_code == 7 and prior_message is not None:
                    (prior_message_n, prior_response_topic, prior_outgoing_message) = prior_message
                    print_with_time(f"Retrying prior message {prior_message_n} now, sending {len(prior_outgoing_message)} bytes to {prior_response_topic}")
                    mqtt_publish_result = mqtt_publisher.publish(prior_response_topic, prior_outgoing_message)
                    print_with_time("FYI, the result was:", mqtt_publish_result)

                print_with_time(f"Trying #{message_n} now...")
                mqtt_publish_result = mqtt_publisher.publish(response_topic, outgoing_message)
                print_with_time("FYI, the result was:", mqtt_publish_result)

