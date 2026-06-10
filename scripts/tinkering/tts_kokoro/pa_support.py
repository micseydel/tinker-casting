import os
import sys
import random
import logging
from time import time, ctime
from multiprocessing import Process, Manager
from dataclasses import dataclass

from paho.mqtt import client as mqtt_client
from paho.mqtt.enums import MQTTErrorCode

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler


@dataclass
class MqttConfig:
    username: str
    password: str
    broker: str
    port: int

def get_config_from_env():
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")
    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))

    return MqttConfig(username, password, broker, port)


class AsyncMqttWrapper:
    def __init__(self, q):
        self.q = q

    def subscribe(self, topic):
        self.q.put(("subscribe", topic))

    def publish(self, topic, msg):
        self.q.put(("publish", topic, msg))

#


class EventHandler(FileSystemEventHandler):
    def __init__(self, note_path, actor_q):
        self.note_path = note_path
        self.actor_q = actor_q


    def on_modified(self, event):
        if event.src_path == self.note_path:
            self.actor_q.put(event)

# FIXME: wrap this q
def long_running(note_dir, note_path, actor_factory, q, mqtt_q):
    # FIXME: technically this is problematic because it can be interlaced with the logging done by the main process
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    # print(1, ctime())
    # logging.info("Starting long running process, initializing actor...")

    python_actor = None
    try:
        python_actor = actor_factory(note_dir, note_path)
        python_actor.setup(AsyncMqttWrapper(mqtt_q))
    except:
        logging.exception("Failed to setup PythonActor")
        raise

    # print(2, ctime())
    
    while True:
        incoming_data = None
        try:
            incoming_data = q.get()
        except KeyboardInterrupt:
            logging.info("[long_running] App termination via KeyboardInterrupt detected")
            break

        try:
            python_actor.on_message(incoming_data)
        except:
            logging.exception("There was an exception while processing a message")


def runner(mqtt_config, note_dir, note_path, actor_factory):
    client_num = random.randint(0, 100)
    client_id = f'subscriber-TESTING-{client_num}'
    tlclient = mqtt_client.Client(mqtt_client.CallbackAPIVersion.VERSION2, client_id)
    tlclient.username_pw_set(mqtt_config.username, mqtt_config.password)

    proc_manager = Manager()
    mqtt_q = proc_manager.Queue()
    actor_q = proc_manager.Queue()

    event_handler = EventHandler(note_path, actor_q)
    observer = Observer()
    observer.schedule(event_handler, note_dir, recursive=False) # need to ignore .git, .obsidian, what else?
    observer.start() # starts file watching thread in the background

    connection_established = False

    def on_message(client, userdata, msg):
        logging.info(f"[runner.on_message] {client} {userdata} > {msg}")
        # FIXME check msg.topic?
        actor_q.put(msg.payload)
    def on_connect(client, userdata, flags, reason_code, properties):
        # logging.info(f"[runner.on_connect] {client}, {userdata}, {flags}, {reason_code}, {properties}")
        client.on_message = on_message

        nonlocal connection_established

        if not connection_established:
            logging.info("[runner.on_connect] Initial mqtt connection established")
            actor_q.put("Initial mqtt connection established")
            connection_established = True

    tlclient.on_connect = on_connect
    tlclient.connect(mqtt_config.broker, mqtt_config.port)
    
    tlclient.loop_start() # starts mqtt thread in the background (necessary for keep alive)

    logging.info("mqtt and watchdog threads started, creating worker process now...")
    worker = Process(target=long_running, args=(note_dir, note_path, actor_factory, actor_q, mqtt_q))
    worker.start() # start the process that will listen on its queue

    # the main process runs background threads, and feeds mqtt messages to the actor queue
    try:
        while True:
            payload = mqtt_q.get()
            command = payload[0]
            if command == "subscribe":
                topic = payload[1]
                tlclient.subscribe(topic)
            elif command == "publish":
                _, topic, msg = payload
                tlclient.publish(topic, msg)
            else:
                logging.info(f"Unrecognized command {command}")
    except KeyboardInterrupt:
        logging.info(f"keyboard interrupt, exiting. (For debugging:: mqtt connection established? {connection_established})")
    else:
        logging.info("Terminating the worker process and then waiting for it to stop...")
        worker.terminate()
        worker.join()
