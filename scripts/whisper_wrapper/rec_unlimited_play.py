#!/usr/bin/env python3
"""Create a recording with arbitrary duration.
The soundfile module (https://python-soundfile.readthedocs.io/)
has to be installed!
"""

import os
import sys
import queue
import shutil
import tempfile
from datetime import datetime

import sounddevice
import soundfile as sf
import numpy  # Make sure NumPy is loaded before it is used in the callback
assert numpy  # avoid "imported but unused" message (W0611)

# soundfile expects an int, sounddevice provides a float:
SAMPLERATE = int(sounddevice.query_devices(None, 'input')['default_samplerate'])

# FIXME any better way than this global?
q = queue.Queue()

def callback(indata, frames, time, status):
    """This is called (from a separate thread) for each audio block."""
    if status:
        print(status, file=sys.stderr)
    q.put(indata.copy())


def save_recording(dest_path: str) -> str:
    filename = f'desktop_audio_capture_{datetime.now().strftime("%Y%m%d-%H%M%S")}.wav'

    path = os.path.join(dest_path, filename)
    print(f"Will save result to {path}")

    temp_file_path = tempfile.mktemp(suffix=".wav")
    try:
        with sf.SoundFile(temp_file_path, mode='x', samplerate=SAMPLERATE, channels=1) as file:
            with sounddevice.InputStream(samplerate=SAMPLERATE, callback=callback):
                print('<Ctrl+C> to finish the recording')
                while True:
                    file.write(q.get())
    except KeyboardInterrupt:
        print('\nRecording finished: ' + repr(filename))
    shutil.move(temp_file_path, path)
    return filename


def main():
    destination_path = sys.argv[1]
    print(f"Storing to {destination_path}...")
    try:
        while True:
            input("<Enter> to record")
            filename_base = save_recording(destination_path) # blocks!
            print(f"Saved {filename_base}")
            # tags = input("Tags (spaces will be replaced with underscores): ")
            # if tags:
            #     from_path = f'{filename_base}.wav'
            #     to_path = f'{filename_base}_{tags.replace(" ", "_")}.wav'
            #     print(f"Moving {from_path} to {to_path}")
            #     shutil.move(from_path, to_path)
    except KeyboardInterrupt:
        print("Done with recording.")

if __name__ == "__main__":
    main()
