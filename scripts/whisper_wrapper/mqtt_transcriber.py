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

import whisper
from paho.mqtt import client as mqtt_client
from paho.mqtt.enums import MQTTErrorCode
import setproctitle


VERBOSE = False


# FIXME: this needs to use a proper logger, since stdout is potentially written to concurrently
def print_with_time(s, *args, **kwargs) -> None:
    print(f"[{time.ctime()}] {s}", *args, **kwargs)


class Transcriber:
    def __init__(self, model_choice: str) -> None:
        self.model_choice = model_choice
        print_with_time("Loading model")
        start = time.perf_counter()
        self.model = whisper.load_model(model_choice)
        elapsed = time.perf_counter() - start
        print_with_time(f"Model {model_choice} loaded in {elapsed:.1f}s")

    def __call__(self, filename) -> Optional[Tuple[float, dict]]:
        try:
            start = time.perf_counter()
            # `fp16=False` seems to be necessary on Apple Silicon to suppress a warning
            result = self.model.transcribe(filename, fp16=False, language='english')
            elapsed = time.perf_counter() - start
            return (elapsed, result)
        except RuntimeError:
            print_with_time("Transcription failed WEIRDLY for", filename, "did it exist?", os.path.isfile(filename))
            traceback.print_exc()
            return None
        except Exception:
            print_with_time("Transcription failed unexpectedly for", filename)
            traceback.print_exc()
            return None



class MqttManager:
    def __init__(self, queue, client_id, broker, port, username, password, subscription_topic = None) -> None:
        self.client = self.connect_mqtt(client_id, broker, port, username, password, subscription_topic, queue)

    def connect_mqtt(self, client_id, broker, port, username, password, topic, queue) -> mqtt_client:
        print_with_time(f"connect_mqtt called for client_id {client_id}; subscribed topic = {topic}")
        subscribed = False
        def on_connect(client, userdata, flags, reason_code, properties):
            # FIXME: on_connect should publish a message for tracking
            print_with_time(f"on_connect called for client_id {client_id}; subscribed topic = {topic}")
            if topic is not None:
                nonlocal subscribed
                if reason_code == 0:
                    re = 'RE-' if subscribed else ''
                    print_with_time(f"{re}Connected to MQTT Broker! {re}Subscribing to {topic}")
                    self.subscribe(queue, topic, client)
                    subscribed = True
                else:
                    print_with_time(f"Failed to connect, return code {reason_code} and properties {properties}")

        client = mqtt_client.Client(mqtt_client.CallbackAPIVersion.VERSION2, client_id)
        client.username_pw_set(username, password)
        client.on_connect = on_connect
        client.connect(broker, port)
        return client


    def subscribe(self, queue, topic, client: mqtt_client):
        print_with_time(f"subscribe called for subscribed topic = {topic}")
        def on_message(client, userdata, msg):
            if VERBOSE: print_with_time(f"on_message called")
            try:
                incoming_data = json.loads(msg.payload.decode())
            except Exception:
                print_with_time(f"Something went wrong (are these stack traces the same?) {traceback.format_exc()}")
                traceback.print_exc()
                return

            vaultPath = incoming_data.get("vaultPath")
            missing_keys = {"vaultPath", "responseTopic", "b64Encoded"} - incoming_data.keys()
            if missing_keys:
                if vaultPath is not None:
                    print_with_time(f"Ignoring request for {vaultPath}, had missing keys: {missing_keys}")
                else:
                    print_with_time(f"Ignoring request, had missing keys: {missing_keys}")
                return

            print_with_time(f"Enqueuing {vaultPath} into queue with approximate size {queue.qsize()}")
            queue.put(incoming_data)

        client.subscribe(topic)
        client.on_message = on_message

    def loop_forever(self): self.client.loop_forever()

    def publish(self, topic, msg): return self.client.publish(topic, msg)

    # FIXME why does this always seem to be False?
    def is_connected(self): return self.client.is_connected()

    def reconnect(self): return self.client.reconnect()


def long_running(q, model_choice, broker, port, username, password, client_num) -> None:
    pid = os.getpid()
    proc_title = f"transcriber_for_{pid}"
    setproctitle.setproctitle(proc_title)
    client_id = f'publisher-{model_choice}-{client_num}'
    print_with_time(f"Starting long-runnning process with pid {pid}, mqtt client id {client_id} and process title {proc_title}")
    try:
        transcriber = Transcriber(model_choice)
        mqtt_manager = MqttManager(q, client_id, broker, port, username, password)
    except Exception:
        print_with_time(f"Something went wrong starting the transcriber: {traceback.format_exc()}")
        return

    while True:
        try:
            incoming_data = q.get()
            if VERBOSE: print_with_time(f"Processing incoming data; there are approximately {q.qsize()} items waiting")
            vault_path = incoming_data["vaultPath"]
            response_topic = incoming_data["responseTopic"]
            contents = incoming_data["b64Encoded"]

            temp = NamedTemporaryFile()
            temp.write(base64.b64decode(contents))

            print_with_time(f"📝 {vault_path}... ", end='', flush=True)

            transcription_result = transcriber(temp.file.name)
            if transcription_result is None:
                print_with_time("Transcription failed")
                return

            elapsed, whisper_result = transcription_result
            print(f"completed in {elapsed:.1f}s")

            data = json.dumps({
                    "whisperResultContent": whisper_result,
                    "whisperResultMetadata": {
                        "model": transcriber.model_choice,
                        "performedOn": socket.gethostname(),
                        "vaultPath": vault_path,
                        "perfCounterElapsed": elapsed,
                    },
                })

            outgoing_message = data.encode()
            print_with_time(f"Publishing {len(outgoing_message)} bytes to mqtt now on {response_topic}; mqtt_manager.is_connected() = {mqtt_manager.is_connected()}")
            mqtt_publish_result = mqtt_manager.publish(response_topic, outgoing_message)
            # FIXME: retain the prior recipient, and say "hey I just finished the next thing so if you didn't get your thing you should have gotten it by now so ask me to resend it" (and store some prior JSON)
            status_code, message_n = mqtt_publish_result
            if status_code == 0:
                if VERBOSE: print_with_time(f"Result #{message_n} published successfully (mqtt_manager.is_connected() = {mqtt_manager.is_connected()})")
            else:
                print_with_time(f"Result #{message_n} failed publishing ({status_code}) (mqtt_manager.is_connected={mqtt_manager.is_connected()}), trying to reconnect now...")
                mqtt_manager.reconnect()
                print_with_time("Trying to re-publish now...")
                mqtt_publish_result = mqtt_manager.publish(response_topic, outgoing_message)
                print_with_time("FYI, the result was:", mqtt_publish_result)
        except:
            print_with_time(f"Something went wrong processing a message: {traceback.format_exc()}; mqtt_manager.is_connected() = {mqtt_manager.is_connected()}")




def run():
    setproctitle.setproctitle(sys.argv[0])
    _, model_choice = sys.argv

    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # FIXME: investigate what randomness means here
    client_num = random.randint(0, 100)
    client_id = f'subscriber-{model_choice}-{client_num}'
    topic = f"python/transcription/{model_choice}"
    
    print_with_time(f"pid {os.getpid()}, subscribing client {client_id} to {topic}...")

    manager = Manager()
    q = manager.Queue()
    mqtt_manager = MqttManager(q, client_id, broker, port, username, password, topic)
    
    worker = Process(target=long_running, args=(q, model_choice, broker, port, username, password, client_num))
    worker.start()
    
    try:
        mqtt_manager.loop_forever()
    except KeyboardInterrupt:
        print_with_time("(KeyboardInterrupt) Done; any output like 'There appear to be 1 leaked semaphore objects to clean up at shutdown' can be ignored")

    worker.terminate()


if __name__ == '__main__':
    run()
