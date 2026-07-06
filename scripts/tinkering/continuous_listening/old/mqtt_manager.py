import json
import traceback

from paho.mqtt import client as mqtt_client
from paho.mqtt.enums import MQTTErrorCode

from util import print_with_time

VERBOSE=False

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
            queue.put(incoming_data)

        client.subscribe(topic)
        client.on_message = on_message

    def loop_forever(self): self.client.loop_forever()

    def publish(self, topic, msg): return self.client.publish(topic, msg)

    # FIXME why does this ALWAYS seem to be False?
    def is_connected(self): return self.client.is_connected()

    def reconnect(self): return self.client.reconnect()



# simplified hack of the above
class MqttPublisher:
    def __init__(self, client_id, broker, port, username, password) -> None:
        self.client = self.connect_mqtt(client_id, broker, port, username, password)

    def connect_mqtt(self, client_id, broker, port, username, password) -> mqtt_client:
        print_with_time(f"connect_mqtt called for client_id {client_id}")
        client = mqtt_client.Client(mqtt_client.CallbackAPIVersion.VERSION2, client_id)
        client.username_pw_set(username, password)
        client.connect(broker, port)
        return client

    def loop_forever(self): self.client.loop_forever()

    def publish(self, topic, msg): return self.client.publish(topic, msg)

    # FIXME why does this ALWAYS seem to be False?
    def is_connected(self): return self.client.is_connected()

    def reconnect(self): return self.client.reconnect()
