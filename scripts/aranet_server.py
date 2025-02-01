import sys
import asyncio

from bleak import BleakScanner
from flask import Flask, request, jsonify
import time
from claranet4.lib import discover, Device, DeviceError, request_measurements, Reading

app = Flask(__name__)

# FIXME: copy paste from the library, changed for async
async def discover_ara4s(substring: str = "Aranet4") -> list[Device]:
    devices = await discover()
    ara4_devices = [d for d in devices if substring in d.name]
    return ara4_devices

@app.route('/ara4s', methods=['GET'])
async def get_ara4s():
    aras = await discover_ara4s()

    addresses = [ara.address for ara in aras]
    result = await get_aras_result(addresses, 15)

    return jsonify(result)


async def read_ara(address, timeout_seconds):
    try:
        result = await asyncio.wait_for(claranet_read_async(address), timeout_seconds)
        return result
    except asyncio.TimeoutError:
        print(f"Read timed out for address {address} after {timeout_seconds} seconds")
    except Exception as e:
        print(f"An error occurred while reading address {address}: {e}")
    return None


async def get_aras_result(addresses, timeout_seconds):
    start = time.perf_counter()

    results = await asyncio.gather(
        *[read_ara(address, timeout_seconds) for address in addresses]
    )

    elapsed = time.perf_counter() - start
    print(f"Aras fetching took {elapsed:.1f}s")

    return {
        "aras": results,
        "meta": {
            "elapsed": elapsed,
            "captureTime": int(time.time())
        },
    }



# FIXME: these two functions were copy-pasted from the library with modifications for async

async def claranet_read_async(address: str = "", timeout: int = 5) -> dict: # -> Reading:
    if address:
        device = await claranet_find_device_async(address)
    else:
        ara4_devices = discover_ara4s() # discover_ara4s(timeout=timeout)
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
        _, port = sys.argv
        port = int(port)
    except ValueError as e:
        print("Expected: a port (int)")
        raise e
    else:
        app.run(
            # host='0.0.0.0',  # listen on the network, not just localhost
            port=port,
            debug=True,
            # I haven't tested hot reloading for whisper models
            use_reloader=False
        )
