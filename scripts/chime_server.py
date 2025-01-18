import sys

from flask import Flask, request, jsonify
import chime

app = Flask(__name__)

CHIME_FUNCTIONS = {
    "success": chime.success,
    "warning": chime.warning,
    "error": chime.error,
    "info": chime.info,
}

@app.route('/chime', methods=['POST'])
async def dothechime():
    data = request.get_json()

    function = data.get("function")
    theme = data.get("theme")
    if function is not None:
        fetched_function = CHIME_FUNCTIONS.get(function)
        if fetched_function is not None:
            if theme is not None:
                try:
                    chime.theme(theme)
                except ValueError:
                    return jsonify({"error": f"theme {theme} not in {chime.themes()}"}), 400

            fetched_function()
            result = {"success": f"function {function} called"}
            return jsonify(result), 202
        else:
            return jsonify({"error": f"function '{function}' not in options: {CHIME_FUNCTIONS.keys()}"}), 400
    else:
        return jsonify({"error": "no 'function' key provided in JSON"}), 400


@app.route('/chime', methods=['GET'])
async def getchime():
    result = {"themes": chime.themes(), "functions": list(CHIME_FUNCTIONS.keys())}
    return jsonify(result)


if __name__ == '__main__':
    try:
        _, port = sys.argv
        port = int(port)
    except ValueError as e:
        raise RuntimeError("Expected: a port (int) CLI parameter") from e
    else:
        app.run(
            host='0.0.0.0',  # listen on the network, not just localhost
            port=int(port),
            debug=True,
            use_reloader=False
        )
