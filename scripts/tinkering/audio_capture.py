import io
import os
import shutil
from tempfile import TemporaryDirectory
from dataclasses import dataclass
from string import ascii_letters
from datetime import datetime, timedelta
from typing import Optional, Generator, List

import whisper
from pydub import AudioSegment
import speech_recognition as sr

from time import perf_counter


STOP_WORD_CHARS = set(ascii_letters + " ")

SAMPLE_RATE = 16000


@dataclass(frozen=True)
class SpeechRecognizerConfig:
    # defaults from main.py args
    energy: int = 500
    pause: float = 1.2  # 0.8
    dynamic_energy: bool = False


@dataclass(frozen=True)
class Snippet:
    path: str
    transcript: str
    duration_seconds: float
    time_taken: datetime
    # location: str = GPS from device?


@dataclass(frozen=True)
class RawClip:
    path: str
    duration_seconds: float
    time_taken: datetime


def transcription_to_command_tokens(transcription: str) -> List[str]:
    # filter down to desired characters, then split by whitespace
    return (''.join(char for char in transcription.lower() if char in STOP_WORD_CHARS)).split()


class AudioCaptureAssistant:
    def __init__(self, model: str = "base",
                 speech_recognizer_config: SpeechRecognizerConfig = SpeechRecognizerConfig(),
                 verbose: bool = False, prompt: Optional[str] = None):
        self.speech_recognizer_config = speech_recognizer_config
        self.verbose = verbose

        # there are no english models for large
        if model != "large":
            # always use english models
            model = model + ".en"
        self.audio_model = whisper.load_model(model)

        self.speech_recognizer = sr.Recognizer()
        self.speech_recognizer.energy_threshold = speech_recognizer_config.energy
        self.speech_recognizer.dynamic_energy_threshold = speech_recognizer_config.dynamic_energy
        self.pause = self.speech_recognizer.pause_threshold = speech_recognizer_config.pause

        self.show_timing = model == "large"

        self.prompt = prompt

    def _block_until_clip(self, source, output_folder) -> RawClip:
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

        return RawClip(path, audio_clip.duration_seconds, taken_time)

    def _block_for_snippet(self, source, output_folder) -> Snippet:
        while True:
            raw_clip = self._block_until_clip(source, output_folder)
            predicted_text = self.transcribe(raw_clip.path)
            if predicted_text is None:
                shutil.move(raw_clip.path, raw_clip.path + ".nospeech")
                if self.verbose:
                    print("No speech detected, renamed file to *.nospeech, excluding from snippet group")
                continue

            return Snippet(
                path=raw_clip.path,
                transcript=predicted_text,
                duration_seconds=raw_clip.duration_seconds,
                time_taken=raw_clip.time_taken
            )

    def _block_until_wake(self, source):
        with TemporaryDirectory() as td:
            while True:
                match transcription_to_command_tokens(self._block_for_snippet(source, td).transcript):
                    case ["resume", "transcript" | "transcription" | "listening" | "recording"]:
                        break

    def transcribe(self, wave_clip_path: str) -> Optional[str]:
        start = perf_counter()

        # `fp16=False` seems to be necessary on my M1 Mac to suppress a warning
        if self.prompt is not None:
            result = self.audio_model.transcribe(wave_clip_path, fp16=False, language='english')
        else:
            result = self.audio_model.transcribe(wave_clip_path, fp16=False, language='english', prompt=self.prompt)

        elapsed = perf_counter() - start
        if self.show_timing:
            print(f"Transcription took {elapsed:.2f} seconds")

        segments = result["segments"]
        if not segments:
            return None

        predicted_text = result["text"]
        return predicted_text

    def gen_snippet_group(self, output_folder: str) -> Generator:
        """
        Yields snippets. Does not yield pause, stop or resume CONTROL snippets.
        :param output_folder:
        :return:
        """
        try:
            with sr.Microphone(sample_rate=SAMPLE_RATE) as source:
                while True:
                    snippet = self._block_for_snippet(source, output_folder)

                    match transcription_to_command_tokens(snippet.transcript):
                        case ["stop" | "end" | "halt", "transcription" | "recording" | "audio"]:
                            print("Stop phrase detected, stopping audio capture")
                            break
                        case ["pause", "transcription" | "recording" | "audio"]:
                            print("Pausing audio capture...")
                            self._block_until_wake(source)
                            print("Resuming audio capture")
                        case _:
                            yield snippet
        except KeyboardInterrupt:
            return


if __name__ == "__main__":
    ass = AudioCaptureAssistant()

    # time to the second
    folder = datetime.now().isoformat()[:19].replace(":", "-")
    dir_path = os.path.join("captured", folder)
    os.mkdir(dir_path)

    print("Capturing now...")
    for s in ass.gen_snippet_group(output_folder=dir_path):
        print(f"{s.time_taken} ({s.duration_seconds}s) - {s.transcript}")
