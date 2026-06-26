import logging
import sys
from multiprocessing import Pipe, Process
from multiprocessing.connection import Connection
from time import ctime, sleep

from .woke_note import WokeNote
from .wrappers.external_messages import get_config_from_env

NOTE_NAME = "WokeProcessCompanion TESTING"
NOTE_CONTENTS = "- [ ] test\n"


class WokeProcess(WokeNote):
    def __init__(self, note_name: str, worker_conn: Connection):
        super().__init__(note_name)
        self.worker_conn = worker_conn  # for sending requests

    def on_other_message(self, message):
        try:
            typ, payload = message
        except ValueError:
            super().on_other_message(message)
        else:
            if typ == "on_work_item_complete":
                self.on_work_item_complete(payload)

    def on_work_item_complete(self, result):
        sleep(0.25)  # just in case 🙄
        self.my_note.set_contents(NOTE_CONTENTS + f"- result `{result}`\n")


# class Worker:
# =======
#             else:
#                 super().on_other_message(message)
#
#     def on_work_item_complete(self, result):
#         logging.info("[WokeProcessCompanion.on_work_item_complete] OVERRIDE THIS")
#
#
class ExampleWokeProcess(WokeProcess):
    def on_start(self):
        super().on_start()
        self.my_note.set_file_contents(NOTE_CONTENTS)
        logging.info(f"[WokeProcessCompanion.on_start] note reset")

    def on_note_modified(self):
        maybe_markdown = self.my_note.markdown_if_starts_with_pressed_button()
        if maybe_markdown is not None:
            self.worker_conn.send(ctime())

    def on_work_item_complete(self, result):
        sleep(0.25)  # just in case 🙄
        self.my_note.set_file_contents(NOTE_CONTENTS + f"- result `{result}`\n")


class ExampleWorker:
    # >>>>>>> 20260623_kokoro_using_WN_package
    def __init__(self, worker_conn: Connection):
        self.worker_conn = worker_conn

    def process_item(self, item):
        self.worker_conn.send(f"DONE! {item}")


def worker_proc(worker_conn: Connection, worker_factory):
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    # FIXME - technically logging can overlap! (use a different file)

    worker = worker_factory(worker_conn)
    while True:
        work_item = worker_conn.recv()
        logging.info(f"received item {work_item}, processing...")
        try:
            worker.process_item(work_item)
        except Exception as e:
            logging.exception(f"Processing failed {e}: {work_item}")
            raise  # FIXME - this should probably not raise


def runner(note_name, woke_process_factory, worker_factory):
    main_conn, worker_conn = Pipe(duplex=True)

    woke_note = woke_process_factory.wake(note_name, main_conn)

    worker = Process(target=worker_proc, args=(worker_conn, worker_factory))
    worker.start()

    try:
        while True:
            worker_result = main_conn.recv()
            woke_note.tell(("on_work_item_complete", worker_result))
    except KeyboardInterrupt:
        logging.info("Ctrl+C detected, exiting...")
    finally:
        worker.terminate()
        woke_note.stop()
        main_conn.close()
        worker_conn.close()
        worker.join()


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    try:
        _, vault_path = sys.argv
    except ValueError:
        print("Expected one CLI arg: vault path\n", file=sys.stderr)
        raise

    mqtt_config = get_config_from_env()

    logging.info(
        f"Using vault {vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}")

    WokeNote.start_background_actors(vault_path, mqtt_config)
    logging.info("Background processes starting, entering multiprocessing stage now...")
    runner(NOTE_NAME, ExampleWokeProcess, ExampleWorker)
    logging.info("Background process completed, stopping background threads in the main process...")
    WokeNote.stop_background_actors()
