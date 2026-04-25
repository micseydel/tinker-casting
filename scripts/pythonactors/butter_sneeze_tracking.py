import os
import sys
import time
import socket
import json
import base64
import traceback
import pathlib
import random
import logging
import datetime

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

ReceivingTopics = [ReceivingTopicGossiper, ReceivingTopicLitterSiftings]

def print_with_time(s, *args, **kwargs) -> None:
    print(f"[{time.ctime()}] {s}", *args, **kwargs)


class PythonActor:
    def __init__(self, document):
        self.document = document

    def connect_mqtt(self, client):
        self.mqtt_client = client

    def on_message(self, client, userdata, msg):
        incoming_topic = msg.topic
        logging.info("Received message on topic %s with size %s bytes", incoming_topic, len(msg.payload))
        if incoming_topic not in ReceivingTopics:
            logging.warn(f"Unexpected topic {incoming_topic}, ignoring {len(msg.payload)} bytes")
            return

        try:
            incoming_data = json.loads(msg.payload.decode())
        except Exception as e:
            logging.exception("Decoding or parsing JSON failed ({%s})", e)
            return

        if incoming_topic == ReceivingTopicGossiper:
            lowered_text = incoming_data["capture"]["whisperResult"]["whisperResultContent"]["text"].lower()
            if "sneez" in lowered_text:
                self.document.appendToInbox(f"- Noticed a sneeze! {lowered_text}\n")
                self.mqtt_client.publish("[[Chronicler]]", json.dumps({
                    "noteId": incoming_data["capture"]["captureTime"]["noteId"],
                    "noteCreationDate": captureTime[:10],
                    "timeOfAck": datetime.datetime.now().astimezone().isoformat(),
                    "details": "sneez detected ([[Butter Sneeze Tracking|ref]])",
                    "setNoteState": "AutomaticallyIntegrated",
                }))
            else:
                logging.info("Ignoring: %s", lowered_text)
        elif incoming_topic == ReceivingTopicLitterSiftings:
            self.document.appendToInbox(f"- Received litter report for day {incoming_data['forDay']}, {len(incoming_data['report']['datapoints'])} events\n")

class Document:
    def __init__(self, fh):
        self.fh = fh

    def appendToInbox(self, line):
        self.fh.read() # lazy-coded seek to the end
        self.fh.write(line)
        self.fh.flush()


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

    with open(os.path.join(vault_root, f"{NoteName}.md"), 'r+') as fh:
        document = Document(fh)
        actor = PythonActor(document)
        
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
            print_with_time("(KeyboardInterrupt) Done")


if __name__ == '__main__':
    run()
