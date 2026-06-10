import os
import logging
from time import ctime
from dataclasses import dataclass

import pykka

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, FileModifiedEvent, FileSystemEvent

from note_api import NoteAPI


@dataclass
class VaultNoteSubscription:
    note_name: str
    subscriber: pykka.ActorRef


class VaultWatcher(pykka.ThreadingActor):
    def __init__(self, vault_path):
        super().__init__()
        self.my_note = None
        self.vault_path = vault_path

        self.observer = None
        self.handler = None
        self.subscribers = {}

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')

        self.my_note = NoteAPI(os.path.join(self.vault_path, "Woke Notes VaultWatcher.md"))
        self.my_note.set_contents(f"- started {ctime()}\n")

        self.handler = FolderWatcherEventHandler(self.actor_ref)
        self.observer = Observer()
        self.observer.schedule(self.handler, self.vault_path, recursive=True)
        # important to start the observer thread here rather than in __init__()
        self.observer.start()
        logging.info(f"Watching: {self.vault_path}")

    def on_receive(self, msg):
        if isinstance(msg, VaultNoteSubscription):
            note_name = msg.note_name
            existing_subscriber = self.subscribers.get(note_name.lower())
            if existing_subscriber is not None:
                logging.warning(f"overwriting [[{note_name}]] subscriber {existing_subscriber} with {msg.subscriber}")
                self.my_note.append(f"- \\[{ctime()}] overwriting [[{note_name}]] subscriber {existing_subscriber} with {msg.subscriber}\n")
            else:
                logging.info(f"Setting [[{note_name}]] subscriber to {msg.subscriber}")
                self.my_note.append(f"- \\[{ctime()}] Setting [[{note_name}]] subscriber to {msg.subscriber}\n")

            self.subscribers[msg.note_name.lower()] = msg.subscriber
        elif isinstance(msg, FileModifiedEvent):
            path_to, file_name = os.path.split(msg.src_path)
            maybe_note_name, ext = os.path.splitext(file_name)
            if ext.lower() == ".md":
                subscriber = self.subscribers.get(maybe_note_name.lower())
                if subscriber is not None:
                    subscriber.tell(msg)
                else:
                    logging.debug(f"No subscriber for {maybe_note_name}")
            else:
                logging.debug(f"expected .md but got: {msg.src_path}")
        elif isinstance(msg, FileSystemEvent):
            logging.debug(f"Ignoring message type {type(msg)}")
        else:
            logging.warning(f"Unexpected message type {type(msg)}, expected VaultNoteSubscription or FileModifiedEvent (with support for more FileSystemEvents coming later)")

    def on_stop(self):
        if self.observer:
            self.observer.stop()
            self.observer.join(timeout=2.0)
        logging.info("File watcher stopped.")


class FolderWatcher(pykka.ThreadingActor):
    def __init__(self, watch_path, ext, subscriber):
        super().__init__()
        self.watch_path = watch_path
        if not ext.startswith("."):
            raise "watch_path must start with ."
        self.ext = ext

        self.observer = None
        self.handler = None

        self.subscriber = subscriber

    def on_start(self):
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')

        self.handler = FolderWatcherEventHandler(self.actor_ref)
        self.observer = Observer()
        self.observer.schedule(self.handler, self.watch_path, recursive=True)
        # import to start the observer thread here rather than in __init__()
        self.observer.start()
        logging.info(f"Watching: {self.watch_path}")

    def on_receive(self, msg):
        if isinstance(msg, FileModifiedEvent):
            path_to, file_name = os.path.split(msg.src_path)
            maybe_note_name, ext_under_test = os.path.splitext(file_name)
            if self.ext.lower() == ext_under_test.lower():
                self.subscriber.tell(msg)
            else:
                logging.debug(f"expected .md but got: {msg.src_path}")
        else:
            logging.warning(f"Unexpected message type {type(msg)}, expected FileModifiedEvent")

    def on_stop(self):
        if self.observer:
            self.observer.stop()
            self.observer.join(timeout=2.0)
        logging.info("File watcher stopped.")


# just bridges watchdog to the actor
class FolderWatcherEventHandler(FileSystemEventHandler):
    def __init__(self, actor_ref):
        super().__init__()
        self.actor_ref = actor_ref

    def on_modified(self, event):
        self.actor_ref.tell(event)

    # def on_created(self, event):
    #     self.actor_ref.tell({"type": "created", "path": event.src_path})
    # etc
