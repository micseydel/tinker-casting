import os
import sys
import time
import struct

import pyaudio
import pvporcupine
import speech_recognition as sr
import chime

from audio_capture import log, AudioCaptureAssistant, SpeechRecognizerConfig, WakeWordDetector


ACCESS_KEY = os.environ["ACCESS_KEY"]
WAKEWORD_MODEL_PATH = os.environ["WAKEWORD_MODEL_PATH"]
WAKEWORD_MODEL_PATH2 = os.environ["WAKEWORD_MODEL_PATH2"]
dir_path = os.environ["SNIPPET_OUTPUT_DIR"]

# def wait_for_wakeword():
#     pa = pyaudio.PyAudio()
#     audio_stream = pa.open(
#                         rate=porcupine.sample_rate,
#                         channels=1,
#                         format=pyaudio.paInt16,
#                         input=True,
#                         frames_per_buffer=porcupine.frame_length)
#
#     while True:
#         audio_frame = get_next_audio_frame(porcupine, audio_stream)
#         keyword_index = porcupine.process(audio_frame)
#         if keyword_index >= 0:
#             return keyword_index


# assistant = AudioCaptureAssistant(SpeechRecognizerConfig(dynamic_energy=True))
# log(f"Assistant started and writing to {dir_path}")
# def capture_audio(wakeword):
#     pa = pyaudio.PyAudio()
#     audio_stream = pa.open(
#                         rate=porcupine.sample_rate,
#                         channels=1,
#                         format=pyaudio.paInt16,
#                         input=True,
#                         frames_per_buffer=porcupine.frame_length)
#     log(f"Capturing audio clip...")
#     assistant.capture(audio_stream, dir_path, porcupine)


# def main():
#     try:
#         while True:
#             log("Listening for wakeword...")
#             wakeword = wait_for_wakeword()
#             log(f"Detected wake word {wakeword}! Listening until {assistant.speech_recognizer_config.pause} pause")
#             chime.info()
#             capture_audio(wakeword)
#             chime.success()
#     except KeyboardInterrupt:
#         log("Exiting...")

# def main():
#     porcupine = pvporcupine.create(
#         access_key=ACCESS_KEY,
#         keyword_paths=[WAKEWORD_MODEL_PATH, WAKEWORD_MODEL_PATH2]
#     )
#
#     try:
#         pa = pyaudio.PyAudio()
#         audio_stream = pa.open(
#             rate=porcupine.sample_rate,
#             channels=1,
#             format=pyaudio.paInt16,
#             input=True,
#             frames_per_buffer=porcupine.frame_length)
#
#         assistant = AudioCaptureAssistant(SpeechRecognizerConfig(dynamic_energy=True))
#         wake_word_detector = WakeWordDetector(porcupine)
#         log(f"Assistant started and writing to {dir_path}")
#
#         while True:
#             log("Listening for wakeword...")
#             wakeword, wake_word_audio = wake_word_detector.process_audio(audio_stream)
#             log(f"Detected wake word {wakeword}! Listening until {assistant.speech_recognizer_config.pause} pause")
#             chime.info()
#             assistant.capture(audio_stream, dir_path, porcupine, wake_word_audio)
#             chime.success()
#     except KeyboardInterrupt:
#         log("Exiting...")
#     finally:
#         audio_stream.stop_stream()
#         audio_stream.close()
#         pa.terminate()

def main():
    porcupine = pvporcupine.create(
        access_key=ACCESS_KEY,
        keyword_paths=[WAKEWORD_MODEL_PATH, WAKEWORD_MODEL_PATH2]
    )

    pa = None
    audio_stream = None
    try:
        pa = pyaudio.PyAudio()
        audio_stream = pa.open(
            rate=porcupine.sample_rate,
            channels=1,
            format=pyaudio.paInt16,
            input=True,
            frames_per_buffer=porcupine.frame_length)

        assistant = AudioCaptureAssistant(SpeechRecognizerConfig(dynamic_energy=True))
        wake_word_detector = WakeWordDetector(porcupine)
        log(f"Assistant started and writing to {dir_path}")

        while True:
            try:
                log("Listening for wakeword...")
                wakeword, wake_word_audio = wake_word_detector.process_audio(audio_stream)
                log(f"Detected wake word {wakeword}! Listening until {assistant.speech_recognizer_config.pause} pause")
                chime.info()
                assistant.capture(audio_stream, dir_path, porcupine, wake_word_audio)
                chime.success()
            except OSError as e:
                if e.errno == -9981:  # Input overflowed
                    log("Input buffer overflow in main loop. Resetting stream...")
                    time.sleep(0.5)  # Give a longer pause before continuing
                    continue
                else:
                    raise
    except KeyboardInterrupt:
        log("Exiting...")
    finally:
        if audio_stream is not None:
            try:
                audio_stream.stop_stream()
                audio_stream.close()
            except OSError:
                log("Error while closing audio stream. It may already be closed.")
        if pa is not None:
            pa.terminate()

if __name__ == "__main__":
    main()











