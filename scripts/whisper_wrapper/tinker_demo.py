#!/usr/bin/env python3

# copied from rec_unlimited_play.py and transcribe_and_generate_markdown.py

import os
import sys
import queue
import shutil
import tempfile
from time import ctime, perf_counter
from datetime import datetime

import whisper
import sounddevice
import soundfile as sf
import numpy  # Make sure NumPy is loaded before it is used in the callback
assert numpy  # avoid "imported but unused" message (W0611)

from transcribe_and_generate_markdown import get_length, float_to_hour_minute_second, get_segments, segments_to_markdown

# soundfile expects an int, sounddevice provides a float:
SAMPLERATE = int(sounddevice.query_devices(None, 'input')['default_samplerate'])


def log(s, *args, **kwargs) -> None:
    print(f"[{ctime()}] {s}", *args, **kwargs)



# FIXME any better way than this global?
q = queue.Queue()

def callback(indata, frames, time, status):
    """This is called (from a separate thread) for each audio block."""
    if status:
        log(status, file=sys.stderr)
    q.put(indata.copy())


def save_recording(dest_path: str) -> str:
    filename = f'desktop_audio_capture_{datetime.now().strftime("%Y%m%d-%H%M%S")}.wav'

    path = os.path.join(dest_path, filename)

    temp_file_path = tempfile.mktemp(suffix=".wav")
    try:
        with sf.SoundFile(temp_file_path, mode='x', samplerate=SAMPLERATE, channels=1) as file:
            with sounddevice.InputStream(samplerate=SAMPLERATE, callback=callback):
                log('<Ctrl+C> to finish the recording')
                while True:
                    file.write(q.get())
    except KeyboardInterrupt:
        log('\nRecording finished: ' + repr(filename))
    shutil.move(temp_file_path, path)
    return filename


def main():
    destination_dir = sys.argv[1]
    model_choice = "large"  # FIXME: add to argv

    log(f"Storing to {destination_dir} and then will transcribe with Whisper model {model_choice}...")
    os.makedirs(destination_dir, exist_ok=True)

    # try:
    input("Press <ENTER> when to record, Ctrl+C after that to complete")
    audio_filename = save_recording(destination_dir) # blocks!
    audio_path = os.path.join(destination_dir, audio_filename)
    # except KeyboardInterrupt:
        # log("Canceled.")
    log(f"Saved {audio_path}. Loading Whisper model...")

    model = whisper.load_model(model_choice)

    raw_length = get_length(audio_path)
    length = float_to_hour_minute_second(raw_length)

    log(f"Model loaded, beginning transcription for duration {length}")

    t0 = perf_counter()
    segments = get_segments(model, audio_path)
    t1 = perf_counter()

    transcription_time = t1 - t0
    work_relative_to_duration_pct = transcription_time / raw_length * 100
    log(f"Done, took {float_to_hour_minute_second(transcription_time)} ({work_relative_to_duration_pct:0.1f}%) of the source; generating markdown")

    markdown_filename = f"Transcription for {os.path.splitext(audio_filename)[0]}.md"
    markdown_path = os.path.join(destination_dir, markdown_filename)
    markdown = segments_to_markdown(segments, audio_path)
    with open(markdown_path, "w") as f:
        f.write(markdown)

    playground_note_path = os.path.join(destination_dir, "Playground.md")
    with open(playground_note_path, "a+") as f:
        print(f"- ![[{markdown_filename[:-3]}]]", file=f)
    log(f"Appended embed for path {audio_path} to list in {playground_note_path}")

    log("Done, Obsidian should update soon\n")


if __name__ == "__main__":
    main()
