import os
import sys
import time
from typing import Sequence

from wyze_sdk import Client
from wyze_sdk.api.devices import PlugsClient
from wyze_sdk.errors import WyzeApiError
from flask import Flask, request, jsonify
from wyze_sdk.models.devices import Plug

app = Flask(__name__)


@app.route('/wyze/device', methods=['GET'])
def get_wyze_device_list():
    devices = [e.to_dict() for e in client.devices_list()]
    result = {"wyze_device_list": devices}
    return jsonify(result), 200


def to_dict_better(p: Plug):
    d = p.to_dict()
    
    if p.is_online:
        d["is_on"] = p.is_on

    return d


@app.route('/wyze/plug', methods=['GET'])
def get_wyze_plug_list():
    devices = [to_dict_better(e) for e in client.plugs.list()]
    result = {"wyze_plug_list": devices}
    return jsonify(result), 200


@app.route('/wyze/plug', methods=['POST'])
def set_plug_state():
    input_json = request.get_json()

    device_mac = input_json.get("device_mac")
    if device_mac is None:
        return jsonify({"err": f"JSON did not have key device_mac {input_json}"}), 400
    else:
        is_on = input_json.get("is_on")
        if is_on is None:
            return jsonify({"err": f"JSON did not have key is_on {input_json}"}), 400
        else:
            plug = client.plugs.info(device_mac=device_mac)
            print(f"Setting state {is_on} for plug {plug}")
            if is_on:
                client.plugs.turn_on(device_mac=plug.mac, device_model=plug.product.model)
            else:
                client.plugs.turn_off(device_mac=plug.mac, device_model=plug.product.model)

    return jsonify({}), 204


###


def get_environment_variable(name: str) -> str:
    value = os.environ.get(name)
    if value is None:
        print_help()
        raise Exception(f"Required environment variable {name} not present")

    return value


def print_help():
    print(f"This script requires the port provided as a CLI param and the following environment variables: "
          f"{{WYZE_EMAIL, WYZE_PASSWORD, WYZE_KEY_ID, WYZE_API_KEY}}")


if __name__ == "__main__":
    _, port = sys.argv
    port = int(port)

    email = get_environment_variable("WYZE_EMAIL")
    password = get_environment_variable("WYZE_PASSWORD")
    key_id = get_environment_variable("WYZE_KEY_ID")
    api_key = get_environment_variable("WYZE_API_KEY")

    # FIXME global, lazy
    client = Client(email=email, password=password, key_id=key_id, api_key=api_key)

    app.run(
            # host='0.0.0.0',  # listen on the network, not just localhost
            port=port,
            debug=True,
            # I haven't tested hot reloading for whisper models
            use_reloader=False
        )
