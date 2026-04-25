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
    def __init__(self, vault_root):
        self.path = os.path.join(vault_root, NoteName)

    def connect_mqtt(self, client):
        self.mqtt_client = client # will need this for ack'ing transcriptions, voting, etc

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
                
                # FIXME  
                print_with_time(f"Noticed a sneeze! (sending a broken ack)", lowered_text)
                msg = json.dumps({}) #ListenerAcknowledgement(noteId: NoteId, noteCreationDate: LocalDate, timeOfAck: ZonedDateTime, details: String, setNoteState: Option[NoteState])
                
                self.mqtt_client.publish("[[Chronicler]]", msg)
            else:
                print_with_time(f"Ignoring:", lowered_text)
        elif incoming_topic == ReceivingTopicLitterSiftings:
            print_with_time(f"Received litter report for day {incoming_data['forDay']}, {len(incoming_data['report']['dataPoints'])} events")


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
