import sys

### NEEDS TESTING

import chime
from paho.mqtt import client as mqtt_client

app = Flask(__name__)

CHIME_FUNCTIONS = {
    "success": chime.success,
    "warning": chime.warning,
    "error": chime.error,
    "info": chime.info,
}

# @app.route('/chime', methods=['POST'])
# async def dothechime():
#     data = request.get_json()

#     function = data.get("function")
#     theme = data.get("theme")
#     if function is not None:
#         fetched_function = CHIME_FUNCTIONS.get(function)
#         if fetched_function is not None:
#             if theme is not None:
#                 try:
#                     chime.theme(theme)
#                 except ValueError:
#                     return jsonify({"error": f"theme {theme} not in {chime.themes()}"}), 400

#             fetched_function()
#             result = {"success": f"function {function} called"}
#             return jsonify(result), 202
#         else:
#             return jsonify({"error": f"function '{function}' not in options: {CHIME_FUNCTIONS.keys()}"}), 400
#     else:
#         return jsonify({"error": "no 'function' key provided in JSON"}), 400


# @app.route('/chime', methods=['GET'])
# async def getchime():
#     result = {"themes": chime.themes(), "functions": list(CHIME_FUNCTIONS.keys())}
#     return jsonify(result)


# if __name__ == '__main__':
#     try:
#         _, port = sys.argv
#         port = int(port)
#     except ValueError as e:
#         raise RuntimeError("Expected: a port (int) CLI parameter") from e
#     else:
#         app.run(
#             host='0.0.0.0',  # listen on the network, not just localhost
#             port=int(port),
#             debug=True,
#             use_reloader=False
#         )

def print_with_time(s, *args, **kwargs) -> None:
    print(f"[{time.ctime()}] {s}", *args, **kwargs)


def connect_mqtt(client_id, broker, port, username, password) -> mqtt_client:
    def on_connect(client, userdata, flags, rc):
        if rc == 0:
            print_with_time(f"Connected to MQTT Broker!")
        else:
            print_with_time(f"Failed to connect, return code %d\n", rc)

    client = mqtt_client.Client(client_id)
    client.username_pw_set(username, password)
    client.on_connect = on_connect
    client.connect(broker, port)
    return client


def subscribe(topic, client: mqtt_client):
    # print_with_time("Loading model")
    # start = time.perf_counter()
    # model = whisper.load_model(model_choice)
    # elapsed = time.perf_counter() - start
    # print_with_time(f"Model {model_choice} loaded in {elapsed:.1f}s")

    def on_message(client, userdata, msg):
        try:
            data = msg.payload.decode()
            function = data.get("function")
            theme = data.get("theme")
            if function is not None:
                fetched_function = CHIME_FUNCTIONS.get(function)
                if fetched_function is not None:
                    if theme is not None:
                        try:
                            chime.theme(theme)
                        except ValueError:
                            # return jsonify({"error": f"theme {theme} not in {chime.themes()}"}), 400
                            print_with_time(f"theme {theme} not in {chime.themes()}")
                            return

                    fetched_function()
            #         result = {"success": f"function {function} called"}
            #         return jsonify(result), 202
            #     else:
            #         return jsonify({"error": f"function '{function}' not in options: {CHIME_FUNCTIONS.keys()}"}), 400
            # else:
            #     return jsonify({"error": "no 'function' key provided in JSON"}), 400

            # incoming_data = json.loads(msg.payload.decode())
            # vault_path = incoming_data["vaultPath"]
            # response_topic = incoming_data["responseTopic"]
            # contents = incoming_data["b64Encoded"]

            # temp = NamedTemporaryFile(delete=False, dir="queued_temp_files")
            # temp.write(base64.b64decode(contents))

            # print_with_time(f"📝 {vault_path}... ", end='', flush=True)

            # try:
            #     start = time.perf_counter()
            #     # `fp16=False` seems to be necessary on my M1 Mac to suppress a warning
            #     result = model.transcribe(temp.file.name, fp16=False, language='english')
            #     elapsed = time.perf_counter() - start
            # except Exception:
            #     print_with_time("Transcription failed unexpectedly for", temp.file.name)
            #     traceback.print_exc()
            #     return

            # print(f"completed in {elapsed:.1f}s, publishing result to mqtt now on {response_topic}")

            # data = json.dumps({
            #         "whisperResultContent": result,
            #         "whisperResultMetadata": {
            #             "model": model_choice,
            #             "performedOn": socket.gethostname(),
            #             "vaultPath": vault_path,
            #             "perfCounterElapsed": elapsed,
            #         },
            #     })

            # result = client.publish(response_topic, data)
            # # result: [0, 1]
            # status = result[0]
            # if status == 0:
            #     # print_with_time(f"Send `{msg}` to topic `{out_topic}`")
            #     pass
            # else:
            #     print_with_time(f"Failed to send message to topic {response_topic}")

            # temp_path = incoming_data.get("$tempPath")
            # if temp_path:
            #     os.remove(temp_path)
        except:
            print_with_time(f"Something went wrong processing a message: {traceback.format_exc()}")

    client.subscribe(topic)
    client.on_message = on_message


def run():
    broker = os.environ.get("mqttBroker")
    port = int(os.environ.get("mqttBrokerPort"))
    username = os.environ.get("mqttUsername")
    password = os.environ.get("mqttPassword")

    # Generate a Client ID with the subscribe prefix.
    client_id = f'subscribe-{random.randint(0, 100)}'

    try:
        _, topic = sys.argv
    except ValueError:
        topic = "python/chime"
        print_with_time(f"no topic provided as a command line, using default: {topic}")
    
    print_with_time(f"pid {os.getpid()}, mqtt client_id {client_id}, subscribing to topic {topic}...")

    client = connect_mqtt(client_id, broker, port, username, password)
    subscribe(topic, client)
    client.loop_forever()


if __name__ == '__main__':
    run()
