import sys

from flask import Flask, request, jsonify
import time

app = Flask(__name__)

from pprint import pprint

from rasa_wrapper import ModelWrapper


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
        _, port = sys.argv
    except ValueError as e:
        print("Something went wrong starting up the server")
        raise e
    else:
        print("Using port", port, "; loading Rasa now...")
        rasa_model = ModelWrapper()

        print("Starting Flask...")
        app.run(
            host='0.0.0.0',  # listen on the network, not just localhost
            port=int(port),
            debug=True,
            use_reloader=False
        )
