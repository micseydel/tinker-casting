import time


TEST_TOPIC = "TESTING_2025-12-15"
TRANSCRIBER_TOPIC = "python/transcription/base"


def print_with_time(s, *args, **kwargs) -> None:
    print(f"[{time.ctime()}] {s}", *args, **kwargs)

