import os
import sys
import time
import socket
import json
import base64
import traceback
import pathlib
import random

from urllib.error import URLError
from typing import Optional, Tuple
from pprint import pprint

from paho.mqtt import client as mqtt_client
from paho.mqtt.enums import MQTTErrorCode
import setproctitle
from paho.mqtt import client as mqtt_client


NoteName = "Butter Sneeze Tracker"

ReceivingTopicGossiper = "[[Gossiper]]/publish/WhisperLarge"
ReceivingTopicLitterSiftings = "[[Litter Sifting Chart (last 30 days)]]/publish/LitterReportForDay"

def print_with_time(s, *args, **kwargs) -> None:
    print(f"[{time.ctime()}] {s}", *args, **kwargs)


class PythonActor:
    def __init__(self, vault_root):
        self.vault_root = vault_root

    def connect_mqtt(self, client):
        self.mqtt_client = client # will need this for ack'ing transcriptions, voting, etc

    def on_message(self, client, userdata, msg):
        incoming_topic = msg.topic
        incoming_data = json.loads(msg.payload.decode())

        print(f"Received on topic {incoming_topic} data {incoming_data}")


def run():
    setproctitle.setproctitle(sys.argv[0])
    _, vault_root = sys.argv

    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # FIXME: investigate what randomness means here
    client_num = random.randint(0, 100)
    client_id = f'{NoteName}-{client_num}'

    actor = PythonActor(vault_root)
    
    print_with_time(f"pid {os.getpid()}, client {client_id} connecting now...")

    client = mqtt_client.Client(mqtt_client.CallbackAPIVersion.VERSION2, client_id)
    actor.connect_mqtt(client)
    client.username_pw_set(username, password)
    subscription_topics = [ReceivingTopicGossiper, ReceivingTopicLitterSiftings]
    def on_connect(client, userdata, flags, reason_code, properties):
        client.on_message = actor.on_message
        print_with_time(f"Subscribing to {subscription_topics}")
        for topic in subscription_topics:
            client.subscribe(topic)

    client.on_connect = on_connect
    client.connect(broker, port)
    

    try:
        client.loop_forever()
    except KeyboardInterrupt:
        print_with_time("(KeyboardInterrupt) Done; any output like 'There appear to be 1 leaked semaphore objects to clean up at shutdown' can be ignored")

    worker.terminate()


if __name__ == '__main__':
    run()
