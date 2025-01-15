import sys
import asyncio

from flask import Flask, request, jsonify
import time
from claranet4.lib import *  # FIXME: discover_ara4s and what else?

app = Flask(__name__)


@app.route('/ara4s', methods=['GET'])
async def ara4s_exp():
    result = await get_aras_result(15)
    return jsonify(result)


async def read_ara(uuid, timeout_seconds):
    try:
        result = await asyncio.wait_for(claranet_read_async(uuid), timeout_seconds)
        return result
    except asyncio.TimeoutError:
        print(f"Read timed out for UUID {uuid} after {timeout_seconds} seconds")
    except Exception as e:
        print(f"An error occurred while reading UUID {uuid}: {e}")
    return None


async def get_aras_result(timeout_seconds):
    start = time.perf_counter()

    # ara_1, ara_2 = await asyncio.gather(
    #     read_ara(ARA_1_UUID, timeout_seconds),
    #     read_ara(ARA_2_UUID, timeout_seconds)
    # )

    result = await asyncio.wait_for(read_ara(araUuid, timeout_seconds), timeout_seconds)

    elapsed = time.perf_counter() - start
    print(f"Aras fetching took {elapsed:.1f}s")

    # aras = {key: value for key, value in zip(["1", "2"], [ara_1, ara_2]) if value is not None}

    aras = {araId: result} if result is not None else {}

    return {
        "aras": aras,
        "meta": {
            "elapsed": elapsed,
            "captureTime": int(time.time())
        },
    }



# FIXME: these two functions were copy-pasted from the library with modifications for async

async def claranet_read_async(address: str = "", timeout: int = 5) -> Reading:
    if address:
        device = await claranet_find_device_async(address)
    else:
        ara4_devices = discover_ara4s(timeout=timeout)
        if not ara4_devices:
            raise DeviceError("No Aranet4 devices discovered")
        else:
            device = ara4_devices[0]
    # logging.info(f"Selected {device.name} ({device.rssi}dBm)")
    measurements = await request_measurements(device.address)
    return Reading(device, measurements).__dict__


async def claranet_find_device_async(address) -> Device:
    """Find Device by address"""
    r = await BleakScanner.find_device_by_address(address)
    if r:
        return Device(address=r.address, name=str(r.name), rssi=r.rssi)
    else:
        raise DeviceError(f"Could not find device {address}")

# FIXME: /these two functions were copy-pasted from the library with modifications for async



if __name__ == '__main__':
    try:
        # FIXME: globals
        _, port, araId, araUuid = sys.argv
        port = int(port)
    except ValueError as e:
        print("Expected: a port (int), aranet id (string), aranet uuid (string)")
        raise e
    else:
        app.run(
            # host='0.0.0.0',  # listen on the network, not just localhost
            port=int(port),
            debug=True,
            # I haven't tested hot reloading for whisper models
            use_reloader=False
        )
