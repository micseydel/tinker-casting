import os
import sys
import base64
import traceback
import pathlib

from tempfile import NamedTemporaryFile
from urllib.error import URLError

from flask import Flask, request, jsonify
from multiprocessing import Process, Manager
from typing import Optional
import time
import socket
import whisper
import json
import urllib.request

app = Flask(__name__)

from pprint import pprint

from rasa_wrapper import ModelWrapper


def http_call(callback_url, data):
    # Convert the Python dictionary to a JSON string
    json_data = json.dumps(data)

    # Convert the JSON string to bytes, for transmission
    data_bytes = json_data.encode('utf-8')

    # Create a request object
    req = urllib.request.Request(callback_url, data=data_bytes, headers={'content-type': 'application/json'})

    print(f"Performing callback to {callback_url}... ", end='')
    # Make the request and read the response
    try:
        resp = urllib.request.urlopen(req, timeout=10)
    except URLError as e:
        print("Callback !!! FAILED !!!!", e)
        traceback.print_exc()
    else:
        response_text = resp.read().decode('utf-8')
        expected_response_text = "The request has been accepted for processing, but the processing has not been completed."
        if resp.status != 202 or response_text != expected_response_text:
            print(f"Callback response ({resp.status}):\n{response_text}")
        else:
            print("Callback completed normally")


def long_running(q, model_choice, vault_root):
    print("Loading model")
    start = time.perf_counter()
    model = whisper.load_model(model_choice)
    elapsed = time.perf_counter() - start
    print(f"Model {model_choice} loaded in {elapsed:.1f}s, starting to read from the queue now...")

    while True:
        incoming_data = q.get()
        vault_path = incoming_data["vault_path"]

        full_path = incoming_data.get("$tempPath", os.path.join(vault_root, vault_path))
        print(f"Transcribing {vault_path}... ", end='', flush=True)

        try:
            start = time.perf_counter()
            # `fp16=False` seems to be necessary on my M1 Mac to suppress a warning
            result = model.transcribe(full_path, fp16=False, language='english')
            elapsed = time.perf_counter() - start
        except Exception:
            print("Transcription failed unexpectedly for", full_path)
            traceback.print_exc()
            continue

        callback_url = incoming_data["callback_url"]

        print(f"completed in {elapsed:.1f}s, sending callback now")

        data = {
            "eventType": "TranscriptionCompleted",
            "payload": json.dumps({
                "whisperResultContent": result,
                "whisperResultMetadata": {
                    "model": model_choice,
                    "performedOn": socket.gethostname(),
                    "vaultPath": vault_path,
                    "perfCounterElapsed": elapsed,
                },
            }),
        }
        http_call(callback_url, data)

        temp_path = incoming_data.get("$tempPath")
        if temp_path:
            os.remove(temp_path)


def valid_vault_path(vault_root: str, vault_path: str) -> Optional[str]:
    if not vault_path.endswith('.wav'):
        return f"Bad path does not end with .wav: {vault_path}"

    host_path = os.path.join(vault_root, vault_path)

    # initial_check = time.perf_counter()
    if not os.path.isfile(host_path):
        return f"{host_path} did not exist"
        # time.sleep(1)  # HACK
        # if not os.path.isfile(host_path):
        #     print("Path", host_path, "not found after sleeping, sleeping some more...")
        #     time.sleep(3)
        #     if not os.path.isfile(host_path):
        #         time.sleep(10)
        #         if not os.path.isfile(host_path):
        #             msg = f"Bad path does not exist after {time.perf_counter()-initial_check:.1f}: {host_path} (root {vault_root})"
        #             print(msg)
        #             return msg

    return None


@app.route('/enqueue/vault_path', methods=['POST'])
def enqueue_path():
    vault_root = app.config['VAULT_ROOT']
    data = request.get_json()

    # FIXME: upon 400, return JSON differentiating between...

    # FIXME: missing keys
    for key in ("callback_url", "vault_path"):
        if key not in data:
            message = f"Data with keys {data.keys()} did not have `{key}`"
            print(message, "but did find", data.keys())
            return jsonify({'message': message}), 400

    # FIXME: change this to a 400, return json error code, allow client to retry
    path = data["vault_path"]
    path_err = valid_vault_path(vault_root, path)
    if path_err is not None:
        log_message = f"Enqueued unknown path {path_err} with callback {data['callback_url']}"
        response_message = f'Enqueued unknown path: {path_err}'
    else:
        log_message = f"Enqueueing path {path} with callback {data['callback_url']}"
        response_message = 'Data enqueued'

    print(log_message)
    q.put(data)
    return jsonify({'message': response_message}), 202


@app.route('/enqueue/upload', methods=['POST'])
def enqueue_with_upload():
    data = request.get_json()
    for key in ("callback_url", "vault_path"):
        if key not in data:
            message = f"Data with keys {data.keys()} did not have `{key}`"
            print(message, "but did find", data.keys())
            return jsonify({'message': message}), 400

    contents = data["encoded"]
    del data["encoded"]

    temp = NamedTemporaryFile(delete=False, dir="queued_temp_files")
    temp.write(base64.b64decode(contents))

    data["$tempPath"] = temp.name
    q.put(data)

    return jsonify({'message': f"Enqueued uploaded {data['vault_path']}"}), 202


@app.route('/rasa/intent', methods=['GET'])
def rasa_hacking():
    data = request.get_json()

    if "string" not in data:
        message = f"Expected key `string` in data but only found keys {data.keys()}"
        return jsonify({'message': message}), 400

    start = time.perf_counter()
    result = rasa_model.parse_string(data["string"])
    elapsed = time.perf_counter() - start
    print(f"Rasa parsing took {elapsed:.1f}s")

    # return jsonify({'rasa_result': result}), 200
    print("Returning result:")
    pprint(result)
    return jsonify(result), 200


if __name__ == '__main__':
    try:
        _, model, port, vault_root = sys.argv

        accepted_models = ("base", "large")
        if model not in accepted_models:
            raise ValueError(f"model {model} not in {accepted_models}")

        if not os.path.isdir(vault_root):
            raise ValueError(f"{vault_root} was not a directory")
    except ValueError as e:
        print("Something went wrong starting up the server")
        raise e
    else:
        print("Using model", model, "port", port, "and vault root", vault_root, "and creating queued_temp_files directory if it doesn't exist")

        # FIXME: try disabling this, it might be causing the model to load twice
        # multiprocessing.set_start_method('fork')

        pathlib.Path("queued_temp_files").mkdir(exist_ok=True)

        manager = Manager()
        q = manager.Queue()
        worker = Process(target=long_running, args=(q, model, vault_root))
        worker.start()

        print("EXPERIMENT: loading Rasa...")
        rasa_model = ModelWrapper()

        app.config.from_mapping({"VAULT_ROOT": vault_root})
        app.run(
            host='0.0.0.0',  # listen on the network, not just localhost
            port=int(port),
            debug=True,
            # I haven't tested hot reloading for whisper models
            use_reloader=False
        )
