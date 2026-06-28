import logging
from multiprocessing import Pipe, Process
from multiprocessing.connection import Connection

from .woke_note import WokeNote


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
        logging.warning(f"[WokeProcess.on_work_item_complete {self}] OVERRIDE THIS (ignoring {result}")


def worker_proc(worker_conn: Connection, worker_factory):
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    # FIXME - technically logging can overlap! (use a different file)

    worker = worker_factory(worker_conn)
    while True:
        work_item = worker_conn.recv()
        try:
            worker.process_item(work_item)
        except Exception as e:
            logging.exception(f"Processing failed, ignoring {e}: {work_item}")


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
