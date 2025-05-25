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


def http_call(callback_url, data):
    json_data = json.dumps(data)

    data_bytes = json_data.encode('utf-8')

    req = urllib.request.Request(callback_url, data=data_bytes, headers={'content-type': 'application/json'})

    print(f"Performing callback to {callback_url}... ", end='')
    try:
        resp = urllib.request.urlopen(req, timeout=10)
    except URLError as e:
        print(f"Callback !!! FAILED !!!! for url {callback_url}", e)
        traceback.print_exc()
    else:
        response_text = resp.read().decode('utf-8')
        expected_response_text = "The request has been accepted for processing, but the processing has not been completed."
        if resp.status != 202 or response_text != expected_response_text:
            print(f"Callback response ({resp.status}):\n{response_text}")
        else:
            print("Callback completed normally")


def long_running(q, model_choice):
    print("Loading model")
    start = time.perf_counter()
    model = whisper.load_model(model_choice)
    elapsed = time.perf_counter() - start
    print(f"Model {model_choice} loaded in {elapsed:.1f}s, starting to read from the queue now...")

    while True:
        incoming_data = q.get()
        vault_path = incoming_data["vault_path"]

        full_path = incoming_data.get("$tempPath")
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


if __name__ == '__main__':
    try:
        _, model, port = sys.argv

        accepted_models = ("base", "large")
        if model not in accepted_models:
            raise ValueError(f"model {model} not in {accepted_models}")
    except ValueError as e:
        print("Something went wrong starting up the server")
        raise e
    else:
        print("Using model", model, "port", port, "and creating queued_temp_files directory if it doesn't exist")

        # FIXME: try disabling this, it might be causing the model to load twice
        # multiprocessing.set_start_method('fork')

        pathlib.Path("queued_temp_files").mkdir(exist_ok=True)

        manager = Manager()
        q = manager.Queue()
        worker = Process(target=long_running, args=(q, model, vault_root))
        worker.start()

        app.config.from_mapping({"VAULT_ROOT": vault_root})
        app.run(
            host='0.0.0.0',  # listen on the network, not just localhost
            port=int(port),
            debug=True,
            # I haven't tested hot reloading for whisper models
            use_reloader=False
        )
