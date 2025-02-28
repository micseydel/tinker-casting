import io
import os
import shutil
import struct
from time import ctime
from dataclasses import dataclass
from string import ascii_letters
from datetime import datetime, timedelta
from typing import Optional, Generator, List

import pyaudio
import numpy as np
from pydub import AudioSegment
import speech_recognition as sr


@dataclass(frozen=True)
class SpeechRecognizerConfig:
    # defaults from main.py args
    energy: int = 500
    pause: float = 1.2  # 0.8
    dynamic_energy: bool = False


def log(s) -> None:
    print(f"[{ctime()}] {s}")


# class WakeWordDetector:
#     def __init__(self, porcupine, buffer_duration=2):
#         self.porcupine = porcupine
#         self.buffer_duration = buffer_duration
#         self.buffer_size = int(porcupine.sample_rate * buffer_duration)
#         self.audio_buffer = np.zeros(self.buffer_size, dtype=np.int16)
#
#     def process_audio(self, audio_stream):
#         while True:
#             audio_frame = get_next_audio_frame(self.porcupine, audio_stream)
#
#             # Update the circular buffer
#             self.audio_buffer = np.roll(self.audio_buffer, -len(audio_frame))
#             self.audio_buffer[-len(audio_frame):] = audio_frame
#
#             keyword_index = self.porcupine.process(audio_frame)
#             if keyword_index >= 0:
#                 return keyword_index, self.audio_buffer


class AudioCaptureAssistant:
    def __init__(self,
                 speech_recognizer_config: SpeechRecognizerConfig = SpeechRecognizerConfig(),
                 verbose: bool = False):
        self.speech_recognizer_config = speech_recognizer_config
        self.verbose = verbose

        self.speech_recognizer = sr.Recognizer()
        self.speech_recognizer.energy_threshold = speech_recognizer_config.energy
        self.speech_recognizer.dynamic_energy_threshold = speech_recognizer_config.dynamic_energy
        self.pause = self.speech_recognizer.pause_threshold = speech_recognizer_config.pause

    # def capture(self, audio_stream, output_folder, porcupine) -> None:
    #     with PyAudioSource(audio_stream, porcupine.sample_rate) as source:
    #         audio: sr.AudioData = self.speech_recognizer.listen(source)
    #         # audio: sr.AudioData = self.speech_recognizer.listen(source, 3) # 3 seconds timeout
    #
    #         # this STOPS blocking at the end of the recording plus the pause_threshold
    #         # audio: sr.AudioData = self.speech_recognizer.listen(source)
    #         captured_time = datetime.now()
    #         data = io.BytesIO(audio.get_wav_data())
    #     audio_clip: AudioSegment = AudioSegment.from_file(data)
    #
    #     taken_time = captured_time - timedelta(seconds=audio_clip.duration_seconds + self.pause)
    #     timestamp = taken_time.strftime("%Y%m%d-%H%M%S")
    #     filename = f"desktop_audio_capture_{timestamp}.wav"
    #
    #     os.makedirs(output_folder, exist_ok=True)
    #     path = os.path.join(output_folder, filename)
    #
    #     audio_clip.export(path, format="wav")
    #
    #     log(f"Wrote {path} to disk, recording took {taken_time}")

    def capture(self, audio_stream, output_folder, porcupine, wake_word_audio):
        # Calculate the amount of audio to discard
        wake_word_samples = len(wake_word_audio)

        # Discard the wake word audio
        audio_stream.read(wake_word_samples)

        with PyAudioSource(audio_stream, porcupine.sample_rate) as source:
            audio: sr.AudioData = self.speech_recognizer.listen(source)
            captured_time = datetime.now()
            data = io.BytesIO(audio.get_wav_data())

        audio_clip: AudioSegment = AudioSegment.from_file(data)

        taken_time = captured_time - timedelta(seconds=audio_clip.duration_seconds)
        timestamp = taken_time.strftime("%Y%m%d-%H%M%S")
        filename = f"desktop_audio_capture_{timestamp}.wav"

        os.makedirs(output_folder, exist_ok=True)
        path = os.path.join(output_folder, filename)

        audio_clip.export(path, format="wav")

        log(f"Wrote {path} to disk, recording took {taken_time}")


# # adapted from https://stackoverflow.com/a/65726784/1157440
# def get_next_audio_frame(porcupine, audio_stream):
#     pcm = audio_stream.read(porcupine.frame_length)
#     pcm = struct.unpack_from("h" * porcupine.frame_length, pcm)
#     return pcm


# https://claude.ai/chat/eecda5fc-8378-491e-98c7-930f09134980
# and https://stackoverflow.com/questions/69571374/audio-recording-in-python-with-pyaudio-error-pamaccore-auhal-msg-audi to a limited extent
class PyAudioSource(sr.AudioSource):
    def __init__(self, pyaudio_stream, sample_rate):
        self.pyaudio_stream = pyaudio_stream
        self.SAMPLE_RATE = sample_rate
        self.SAMPLE_WIDTH = pyaudio.get_sample_size(pyaudio.paInt16)
        self.CHUNK = 1024
        self.stream = None

    def __enter__(self):
        self.stream = self.pyaudio_stream
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        pass

    def stream(self):
        while True:
            data = self.pyaudio_stream.read(self.CHUNK)
            if not data:
                break
            yield data

class WakeWordDetector:
    def __init__(self, porcupine, buffer_duration=2):
        self.porcupine = porcupine
        self.buffer_duration = buffer_duration
        self.buffer_size = int(porcupine.sample_rate * buffer_duration)
        self.audio_buffer = np.zeros(self.buffer_size, dtype=np.int16)

    def process_audio(self, audio_stream):
        while True:
            try:
                audio_frame = get_next_audio_frame(self.porcupine, audio_stream)

                # Update the circular buffer
                self.audio_buffer = np.roll(self.audio_buffer, -len(audio_frame))
                self.audio_buffer[-len(audio_frame):] = audio_frame

                keyword_index = self.porcupine.process(audio_frame)
                if keyword_index >= 0:
                    return keyword_index, self.audio_buffer
            except OSError as e:
                if e.errno == -9981:  # Input overflowed
                    log("Input buffer overflow detected. Resetting stream...")
                    time.sleep(0.1)  # Give a short pause before continuing
                    continue
                else:
                    raise  # Re-raise if it's a different OSError

def get_next_audio_frame(porcupine, audio_stream):
    try:
        pcm = audio_stream.read(porcupine.frame_length, exception_on_overflow=False)
        pcm = struct.unpack_from("h" * porcupine.frame_length, pcm)
        return pcm
    except OSError as e:
        if e.errno == -9981:  # Input overflowed
            log("Input buffer overflow in get_next_audio_frame. Returning empty frame.")
            return [0] * porcupine.frame_length
        else:
            raise
