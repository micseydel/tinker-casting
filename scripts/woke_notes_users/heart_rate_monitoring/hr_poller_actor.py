import asyncio
import bitstruct
import struct
import sys
import os
import random
import logging
from time import time, ctime

from bleak import BleakClient, BleakScanner, exc
from paho.mqtt import client as paho_client

from woke_notes import WokeNote, get_config_from_env


HR_MEAS = "00002A37-0000-1000-8000-00805F9B34FB"
# FIXME: battery? breathing? steps?


class WokeLHRistener(WokeNote):
    def on_other_message(self, message):
        try:
            kind, sent_time, hr_val = message
            if kind == "simple_heart_rate":
                logging.info(f"{ctime(sent_time)} {hr_val}")
            else:
                logging.warning(f"expected kind was simple_heart_rate but got {kind}; sent_time, hr_val = {sent_time}, {hr_val}")
        except ValueError:
            logging.exception(f"weird message {message}")


async def run(address, actor_ref, debug=False):
    async with BleakClient(address) as bt_client:
        connected = await bt_client.is_connected()
        logging.info("Connected: {0}".format(connected))

        def hr_val_handler(sender, data):
            (hr_fmt,
             snsr_detect,
             snsr_cntct_spprtd,
             nrg_expnd,
             rr_int) = bitstruct.unpack("b1b1b1b1b1<", data)
            if hr_fmt:
                hr_val, = struct.unpack_from("<H", data, 1)
            else:
                hr_val, = struct.unpack_from("<B", data, 1)
            actor_ref.tell(("simple_heart_rate", time(), hr_val))

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


def main():
    try:
        _, vault_path, address = sys.argv
    except ValueError:
        print("An address and URL are required parameters, fetching then printing the available addresses now...\n", file=sys.stderr)
        asyncio.run(print_devices(sys.stderr))
        exit(1)

    mqtt_config = get_config_from_env()
    WokeNote.start_background_actors(vault_path, mqtt_config)
    actor = WokeLHRistener.wake("WokeLHRistener")

    loop = asyncio.get_event_loop()
    try:
        loop.run_until_complete(run(address, actor))
    except KeyboardInterrupt:
        print("Exiting per Ctrl+C")
    except exc.BleakDeviceNotFoundError:
        print(f"Device {address} not found, scanning for any likely candidates now...", file=sys.stderr)
        asyncio.run(print_devices(sys.stderr))
        exit(1)
    finally:
        WokeNote.stop_background_actors()


if __name__ == "__main__":
    main()

