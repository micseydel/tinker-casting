import os
import sys
import time
import socket
import json
import base64
import traceback
import pathlib
import random

from multiprocessing import Process, Manager
from tempfile import NamedTemporaryFile
from urllib.error import URLError
from typing import Optional, Tuple
from pprint import pprint

from mqtt_manager import MqttManager
from util import print_with_time, TEST_TOPIC, TRANSCRIBER_TOPIC


import setproctitle


VERBOSE = False


def long_running(q, broker, port, username, password, client_num) -> None:
    pid = os.getpid()
    proc_title = f"CONTINUOUS_LISTENING_{pid}"
    setproctitle.setproctitle(proc_title)
    client_id = f'publisher-{client_num}'
    print_with_time(f"Starting long-runnning process with pid {pid}, mqtt client id {client_id} and process title {proc_title}")
    mqtt_manager = MqttManager(q, client_id, broker, port, username, password)

    prior_message = None
    while True:
        try:
            incoming_data = q.get()
            if VERBOSE: print_with_time(f"Processing incoming data; there are approximately {q.qsize()} items waiting")

            # print_with_time(incoming_data)
            whisperResultContent = incoming_data["whisperResultContent"]
            text = whisperResultContent["text"]
            if text.strip():
                print_with_time(f"Received text: {text}")
            else:
                print_with_time(f"empty {incoming_data['whisperResultMetadata']['vaultPath']}")
        except KeyboardInterrupt:
            print_with_time("(KeyboardInterruptPROC) Done; any output like 'There appear to be 1 leaked semaphore objects to clean up at shutdown' can be ignored")


def run():
    setproctitle.setproctitle(sys.argv[0])
    # _, model_choice = sys.argv

    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # FIXME: investigate what randomness means here
    client_num = random.randint(0, 100)
    client_id = f'subscriber-{client_num}'
    subscription_topic = TEST_TOPIC
    
    print_with_time(f"pid {os.getpid()}, subscribing client {client_id} to {subscription_topic}...")

    manager = Manager()
    q = manager.Queue()
    mqtt_manager = MqttManager(q, client_id, broker, port, username, password, subscription_topic)
    
    worker = Process(target=long_running, args=(q, broker, port, username, password, client_num))
    worker.start()
    
    try:
        mqtt_manager.loop_forever()
    except KeyboardInterrupt:
        print_with_time("(KeyboardInterruptMAIN) Done; any output like 'There appear to be 1 leaked semaphore objects to clean up at shutdown' can be ignored")

    worker.terminate()


if __name__ == '__main__':
    run()
