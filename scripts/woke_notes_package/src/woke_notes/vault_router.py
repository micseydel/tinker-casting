import logging
from collections import defaultdict
from dataclasses import dataclass
from typing import Set, Dict

from pykka import ActorRef, ThreadingActor


@dataclass
class Subscribe:
    topic: str
    subscriber: ActorRef


@dataclass
class Publish:
    topic: str
    payload: object


class VaultRouter(ThreadingActor):
    def __init__(self):
        super().__init__()
        self.subscribers: Dict[str, Set[ActorRef]] = defaultdict(set)

    def on_message(self, message):
        if isinstance(message, Subscribe):
            self.subscribers[message.topic].add(message.subscriber)
        elif isinstance(message, Publish):
            for subscriber in self.subscribers[message.topic]:
                subscriber.tell(message.payload)
        else:
            logging.warning(f"Unknown message type: {type(message)}")
