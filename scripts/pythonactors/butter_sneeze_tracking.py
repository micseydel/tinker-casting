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

        try:
            if incoming_topic == ReceivingTopicGossiper:
                lowered_text = incoming_data["capture"]["whisperResult"]["whisperResultContent"]["text"].lower()
                if "sneez" in lowered_text:
                    print_with_time("Sneez* detected")
                    captureTime = incoming_data["capture"]["captureTime"]
                    noteId = incoming_data["noteId"]
                    self.document.appendToInbox(f"- \\[{datetime.datetime.now().isoformat()[:19]}] Noticed a sneeze! {lowered_text} ([[{noteId['id']}|ref]])\n")
                    print_with_time("Appended to inbox, publishing ack now...")
                    self.mqtt_client.publish("[[Chronicler]]", json.dumps({
                        "noteId": noteId,
                        "noteCreationDate": captureTime[:10],
                        "timeOfAck": datetime.datetime.now().astimezone().isoformat(),
                        "details": f"sneez detected ([[{NoteName}|ref]])",
                        "setNoteState": "AutomaticallyIntegrated",
                    }))
                else:
                    logging.info("Ignoring: %s", lowered_text) # FIXME remove
            elif incoming_topic == ReceivingTopicLitterSiftings:
                print_with_time("Litter sifting detected")
                for_day = incoming_data['forDay']
                self.document.add_sifting(for_day, incoming_data['report'])
                self.document.appendToInbox(f"- \\[{datetime.datetime.now().isoformat()[:19]}] Received litter report for day {incoming_data['forDay']}, {len(incoming_data['report']['datapoints'])} events ([[Litter boxes sifting ({for_day})|ref]])\n")
            else:
                logging.warn("Unexpected topic %s (should have been impossible here)", incoming_topic)
        except Exception as e:
            logging.exception("Something went wrong ({%s})", e)


class Document:
    def __init__(self, markdown_path, json_path):
        self.markdown_path = markdown_path

        # FIXME: need to have SIFTINGS key and... what's the HITL for sneezes? or empty siftings?

        self.json_path = json_path
        self.siftings = {}
        try:
            with open(self.json_path) as jsonf:
                self.siftings = json.load(jsonf)
        except Exception as e:
            logging.exception(f"failed to load siftings from disk ({e})")

    def add_sifting(self, for_day, sifting):
        if self.siftings.get(for_day) == sifting:
            return  # existing, no need for change

        self.siftings[for_day] = sifting
        self.update_chart()

        # self.jsonf.truncate(0)
        with open(self.json_path, 'w') as jsonf:
            json.dump(self.siftings, jsonf, indent=4)
        # self.jsonf.flush() #FIXME delete?

    def update_chart(self):
        tuples = [(for_day, len(sifting["datapoints"])) for (for_day, sifting) in self.siftings.items()]
        sifting_counts_by_day = sorted(tuples, key=lambda x: datetime.datetime.fromisoformat(x[0]))
        labels = [x[0] for x in sifting_counts_by_day]
        data = [x[1] for x in sifting_counts_by_day]

        chart = {
            "type": "line",
            "labels": labels,
            "series": [
                {
                    "title": "Siftings in day",
                    "data": data,
                },
            ],
        }

        # self.markdownf.seek(0)
        new_lines = []
        with open(self.markdown_path) as markdownfr:
            for line in markdownfr:
                new_lines.append(line)
                if line == "```chart\n":
                    break
            next(markdownfr)  # discard the old chart; naive, assumes one line of json (rather than yaml)
            new_lines.append(json.dumps(chart))
            new_lines.append("\n")
            new_lines.extend(markdownfr)

        # self.markdownf.truncate(0)
        with open(self.markdown_path, 'w') as markdownfw:
            markdownfw.write("".join(new_lines))
        # self.markdownf.flush()

    def appendToInbox(self, line):
        # self.markdownf.read() # lazy-coded seek to the end
        # 
        # self.markdownf.flush()
        with open(self.markdown_path, 'a') as markdownf:
            markdownf.write(line)


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

    markdown_path = os.path.join(vault_root, f"{NoteName}.md")
    json_path = os.path.join(vault_root, "json", f"{NoteName.lower().replace(' ', '_')}.json")

    document = Document(markdown_path, json_path)
    document.update_chart()
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
    else:
        client.disconnect()


if __name__ == '__main__':
    run()
