import asyncio
import bitstruct
import struct
import sys
import os
import random

from bleak import BleakClient, BleakScanner, exc
from paho.mqtt import client as paho_client


HR_MEAS = "00002A37-0000-1000-8000-00805F9B34FB"


def connect_mqtt(client_id, broker, port, username, password) -> paho_client:
    def on_connect(mqtt_client, userdata, flags, rc):
        if rc == 0:
            print("Connected to MQTT Broker!")
        else:
            print("Failed to connect, return code %d\n", rc)

    mqtt_client = paho_client.Client(client_id)
    mqtt_client.username_pw_set(username, password)
    mqtt_client.on_connect = on_connect
    mqtt_client.connect(broker, port)
    return mqtt_client


async def run(address, mqtt_client, debug=False):
    async with BleakClient(address) as bt_client:
        connected = await bt_client.is_connected()
        print("Connected: {0}".format(connected))

        def hr_val_handler(sender, data):
            # print(f"HR Measurement raw = {sender}: {data} (types {type(sender)} and {type(data)})")
            (hr_fmt,
             snsr_detect,
             snsr_cntct_spprtd,
             nrg_expnd,
             rr_int) = bitstruct.unpack("b1b1b1b1b1<", data)
            if hr_fmt:
                hr_val, = struct.unpack_from("<H", data, 1)
            else:
                hr_val, = struct.unpack_from("<B", data, 1)
            print(f"HR Value: {hr_val}")
            result = mqtt_client.publish("simple_heart_rate", str(hr_val))
            # result: [0, 1]
            status = result[0]
            if status == 0:
                # print(f"Send `{msg}` to topic `{out_topic}`")
                pass
            else:
                print(f"Failed to send message to topic {out_topic}")

        await bt_client.start_notify(HR_MEAS, hr_val_handler)

        while await bt_client.is_connected():
            await asyncio.sleep(1)

async def print_devices(out):
    devices = await BleakScanner.discover()
    likely_candidates = []
    for d in devices:
        print(d, file=out)
        if 'Polar' in (d.name or ""):
            likely_candidates.append(d)

    if likely_candidates:
        print("\nMost likely candidates:", file=out)
        for candidate in likely_candidates:
            print(candidate, file=out)
    else:
        print("\nNo good candidates (expected 'Polar' in a name)", file=out)


if __name__ == "__main__":
    try:
        _, address = sys.argv
    except ValueError:
        print("An address and URL are required parameters, fetching then printing the available addresses now...\n", file=sys.stderr)
        asyncio.run(print_devices(sys.stderr))
        exit(1)

    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # Generate a Client ID with the subscribe prefix.
    client_id = f'subscribe-{random.randint(0, 100)}'

    print(f"pid {os.getpid()}")

    mqtt_client = connect_mqtt(client_id, broker, port, username, password)

    print(f"Using {address} and connecting to mqtt {broker}")
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(run(address, mqtt_client))
    except KeyboardInterrupt:
        print("Exiting per Ctrl+C")
    except exc.BleakDeviceNotFoundError:
        print(f"Device {address} not found, scanning for any likely candidates now...", file=sys.stderr)
        asyncio.run(print_devices(sys.stderr))
        exit(1)

