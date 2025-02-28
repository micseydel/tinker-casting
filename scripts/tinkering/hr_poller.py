import asyncio
import bitstruct
import struct
import sys

import requests
from bleak import BleakClient, BleakScanner


HR_MEAS = "00002A37-0000-1000-8000-00805F9B34FB"

HEADERS = {
    "Content-Type": "application/json"
}

def send_to_server(url, heart_rate):
    data = {
        "eventType": "HeartRate",
        "payload": str(heart_rate)
    }

    response = requests.post(url, json=data, headers=HEADERS)

    if response.status_code != 202:
        print(f"Expected status code 202 but got {response.status_code} and ${response.text}")


async def run(address, url, debug=False):
    async with BleakClient(address) as client:
        connected = await client.is_connected()
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
            send_to_server(url, hr_val)

        await client.start_notify(HR_MEAS, hr_val_handler)

        while await client.is_connected():
            await asyncio.sleep(1)

async def print_devices():
    devices = await BleakScanner.discover()
    likely_candidates = []
    for d in devices:
        print(d)
        if 'Polar' in (d.name or ""):
            likely_candidates.append(d)

    if likely_candidates:
        print("\nMost likely candidates:")
        for candidate in likely_candidates:
            print(candidate)


if __name__ == "__main__":
    try:
        _, address, url = sys.argv
    except ValueError:
        print("An address and URL are required parameters, fetching then printing the available addresses now...\n")
        asyncio.run(print_devices())
        exit(1)

    print(f"Using {address} and connecting to {url}")
    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(run(address, url))
    except KeyboardInterrupt:
        print("Exiting per Ctrl+C")

