import sys
import audio_capture

from time import ctime

config = audio_capture.SpeechRecognizerConfig(
    energy = 500,
    pause = 1.2,  # 0.8
    dynamic_energy = False
)

assistant = audio_capture.AudioCaptureAssistant(
    speech_recognizer_config = config
    # , prompt=""
)

print(f"[{ctime()}] Using config {config}")

for snippet in assistant.gen_snippet_group(output_folder=sys.argv[1]):
    time = snippet.time_taken.isoformat()[11:19]
    words_per_second = len(snippet.transcript.split()) / snippet.duration_seconds

    text = f"[{time} ({snippet.duration_seconds:.1f}s) ({words_per_second:.2f} WPS)] {snippet.transcript}"

    # "GUITAR SLITHS"?
    if snippet.transcript.strip() in ("Thanks for watching!", "Thank you."):
        text = f"[{time} ({snippet.duration_seconds:.1f}s) ({words_per_second:.2f} WPS)] (HALLUCINATION) {snippet.transcript}"
    elif words_per_second > 10 or words_per_second < 0.1:
        text = f"[{time} ({snippet.duration_seconds:.1f}s) ({words_per_second:.2f} (!!) WPS)] (HALLUCINATION) {snippet.transcript}"
    else:
        text = f"[{time} ({snippet.duration_seconds:.1f}s) ({words_per_second:.2f} WPS)] {snippet.transcript}"

    print(text)
