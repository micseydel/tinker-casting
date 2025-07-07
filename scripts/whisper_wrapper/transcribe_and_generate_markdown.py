import os
import sys
import json
import whisper

from time import perf_counter, ctime

def log(s) -> None:
    print(f"[{ctime()}] {s}")

def get_segments(model, path: str) -> dict:
    result_path = os.path.splitext(path)[0] + ".json"
    if os.path.exists(result_path):
        # log(f"Loading transcript from disk for {os.path.splitext(path)[-1]}")
        with open(result_path) as existing_f:
            result = json.load(existing_f)
    else:
        # log("Using Whisper, may take some time...")
        result = model.transcribe(path, fp16=False, language='English')

        # log("Saving Whisper results to disk")
        try:
            with open(result_path, 'w') as new_f:
                json.dump(result, new_f)
        except Exception as e:
            log("Failed to dump result to disk:")
            log(result)
            raise e

    # plaintext is available too... under 'text'?
    return result["segments"]

def float_to_hour_minute_second(seconds: float) -> str:
        i = int(seconds)
        return f"{i // 3600:02}:{(i % 3600) // 60:02}:{i % 60:02}"

def segments_to_markdown(segments, audio_path: str) -> str:
    def generate_bookmarks():
        return [
            {
                "start": segment["start"],
                "text": segment["text"],
            } for segment in segments
        ]

    def generate_formatted_bookmarks():
        bookmarks = generate_bookmarks()
        return "\n".join(
            f"{float_to_hour_minute_second(bookmark['start'])} --- {bookmark['text']}"
            for bookmark in bookmarks
        )

    def generate_text_segments():
        return "\n".join(f"- \\[{float_to_hour_minute_second(segment['start'])}\\] {segment['text']}" for segment in segments)

    return f"""\
![[{os.path.split(audio_path)[-1]}]]

# Segments

{generate_text_segments()}
"""

#     return f"""\
# ---
# tags:
# - transcription
# ---

# # Segments

# {generate_text_segments()}

# # Audio

# ```audio-player
# [[{os.path.split(audio_path)[1]}]]
# {generate_formatted_bookmarks()}
# ```
# """


def files_to_transcribe(audio_file_dir):
    for in_file in os.listdir(audio_file_dir):
        ext = os.path.splitext(in_file)[1].lower()
        if ext in {".mp3", ".wav", ".mp4", ".aac", ".m4a", ".mov", ".webm"}:
            # audio path

            markdown_filename = os.path.splitext(in_file)[0] + ".md"
            markdown_path = os.path.join(audio_file_dir, markdown_filename)
            if os.path.exists(markdown_path):
                log(f"File {markdown_filename} already exists, skipping {in_file}")
                continue

            path = os.path.join(audio_file_dir, in_file)
            yield path
        else:
            log(f"Skipping file {in_file} with ext {ext}")







import subprocess
# https://stackoverflow.com/a/3844467/1157440
def get_length(filename):
    result = subprocess.run(["ffprobe", "-v", "error", "-show_entries",
                             "format=duration", "-of",
                             "default=noprint_wrappers=1:nokey=1", filename],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT)
    return float(result.stdout)









def main():
    audio_file_dir = sys.argv[1]

    if len(sys.argv) > 2:
        model_to_load = sys.argv[2]
    else:
        model_to_load = "large"

    log(f"Using dir {audio_file_dir}")

    files = list(files_to_transcribe(audio_file_dir))

    model = None
    if files:
        log("Loading the Whisper model...")
        model = whisper.load_model(model_to_load)
        log(f"\nThere are {len(files)} files to transcribe\n")

    for path_no, path in enumerate(files, 1):
        markdown_path = os.path.splitext(path)[0] + ".md"
        # if os.path.exists(markdown_path):
        #     print(f"File {markdown_path} already exists, skipping")
        #     continue

        raw_length = get_length(path)
        length = float_to_hour_minute_second(raw_length)
        
        log(f"Transcribing path #{path_no}/{len(files)} - {path} ({length})")

        t0 = perf_counter()
        segments = get_segments(model, path)
        t1 = perf_counter()
        transcription_time = t1 - t0
        work_relative_to_duration_pct = transcription_time / raw_length * 100
        log(f"Done, took {float_to_hour_minute_second(transcription_time)} ({work_relative_to_duration_pct:0.1f}%)")

        log("Generating markdown")
        markdown = segments_to_markdown(segments, path)

        with open(markdown_path, "w") as f:
            f.write(markdown)
        # log("Done\n")
    else:
        files = os.listdir(audio_file_dir)
        json_files = [f for f in files if f.lower().endswith("json") and f[:-4]+"md" not in files]
        log(f"Found {len(json_files)} JSON files without matching markdown")
        for jf in json_files:
            path = os.path.join(audio_file_dir, jf)
            with open(path) as jfp:
                segments = json.load(jfp)["segments"]
                markdown = segments_to_markdown(segments, path)

                markdown_path = os.path.splitext(path)[0] + ".md"
                with open(markdown_path, "w") as f:
                    f.write(markdown)
        log("Done!\n")
        

if __name__ == '__main__':
    main()
