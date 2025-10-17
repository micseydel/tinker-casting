import os
import sys
import time
import socket
import json
import base64
import traceback
import pathlib
import random

from tempfile import NamedTemporaryFile
from urllib.error import URLError
from typing import Optional
from pprint import pprint

import whisper
from paho.mqtt import client as mqtt_client


def print_with_time(s, *args, **kwargs) -> None:
    print(f"[{time.ctime()}] {s}", *args, **kwargs)


def gen_transcriber(model_choice):
    print_with_time("Loading model")
    start = time.perf_counter()
    model = whisper.load_model(model_choice)
    elapsed = time.perf_counter() - start
    print_with_time(f"Model {model_choice} loaded in {elapsed:.1f}s")

    def transcriber(filename):
        try:
            start = time.perf_counter()
            # `fp16=False` seems to be necessary on Apple Silicon to suppress a warning
            result = model.transcribe(filename, fp16=False, language='english')
            elapsed = time.perf_counter() - start
            return (elapsed, result)
        except Exception:
            print_with_time("Transcription failed unexpectedly for", filename)
            traceback.print_exc()
            return None

    return transcriber


def connect_mqtt(client_id, broker, port, username, password, transcriber, topic, model_choice) -> mqtt_client:
    subscribed = False
    def on_connect(client, userdata, flags, rc):
        nonlocal subscribed
        if rc == 0:
            re = 'RE-' if subscribed else ''
            print_with_time(f"{re}Connected to MQTT Broker! {re}Subscribing to {topic}")
            subscribe(model_choice, transcriber, topic, client)
            subscribed = True
        else:
            print_with_time(f"Failed to connect, return code %d\n", rc)

    client = mqtt_client.Client(client_id)
    client.username_pw_set(username, password)
    client.on_connect = on_connect
    client.connect(broker, port)
    return client


def subscribe(model_choice, transcriber, topic, client: mqtt_client):
    def on_message(client, userdata, msg):
        try:
            incoming_data = json.loads(msg.payload.decode())
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
                        "model": model_choice,
                        "performedOn": socket.gethostname(),
                        "vaultPath": vault_path,
                        "perfCounterElapsed": elapsed,
                    },
                })
            with open("last.json", "w") as f:
                f.write(data)

            outgoing_message = data.encode()
            print_with_time(f"Publishing {len(outgoing_message)} bytes now to mqtt now on {response_topic}")
            mqtt_publish_result = client.publish(response_topic, outgoing_message)
            # result: [0, 1]
            status = mqtt_publish_result[0]
            if status == 0:
                print_with_time(f"Result published successfully")
            else:
                print_with_time(f"Failed to send message to topic {response_topic}: {mqtt_publish_result}")
        except:
            print_with_time(f"Something went wrong processing a message: {traceback.format_exc()}")

    client.subscribe(topic)
    client.on_message = on_message


def run():
    _, model_choice = sys.argv

    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # Generate a Client ID with the subscribe prefix.
    client_id = f'subscribe-{random.randint(0, 100)}'

    topic = f"python/transcription/{model_choice}"

    transcriber = gen_transcriber(model_choice)
    
    print_with_time(f"pid {os.getpid()}, subscribing to {topic}...")

    client = connect_mqtt(client_id, broker, port, username, password, transcriber, topic, model_choice)
    try:
        client.loop_forever()
    except KeyboardInterrupt:
        print_with_time("(KeyboardInterrupt) Done")


if __name__ == '__main__':
    run()
