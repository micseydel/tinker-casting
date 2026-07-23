"""
Minimal OTel HTTP/JSON receiver for Claude Code telemetry.

Logs every metric datapoint and log event to a local JSONL file,
one JSON object per line, flushed + fsynced immediately so nothing
is lost to an in-memory buffer if the process dies mid-dev.

Run:
    pip install fastapi uvicorn   # inside a venv, see note in chat
    uvicorn otel_backend:app --port 4318

Point Claude Code at it:
    export CLAUDE_CODE_ENABLE_TELEMETRY=1
    export OTEL_METRICS_EXPORTER=otlp
    export OTEL_LOGS_EXPORTER=otlp
    export OTEL_EXPORTER_OTLP_PROTOCOL=http/json
    export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
    export OTEL_METRIC_EXPORT_INTERVAL=10000
    export OTEL_LOGS_EXPORT_INTERVAL=5000
    claude
"""

import json
import os
import sys
import threading
import time
import logging
from datetime import datetime, timezone

from fastapi import FastAPI, Request

from woke_notes import WokeNote, get_config_from_env


PUBLISH_TO_MQTT = False  # FIXME


class InstrumentationNote(WokeNote):
    def __init__(self, note_name):
        super().__init__(note_name)

    def on_start(self):
        LOG_PATH = os.environ.get("OTEL_JSONL_PATH", "./claude-code-telemetry.jsonl")
        self.f = open(LOG_PATH, 'a', encoding="utf-8")

    def on_stop(self):
        self.f.close()

    def on_other_message(self, message):
        try:
            _type, payload = message
        except ValueError:
            return

        self.f.write(payload + "\n")
        self.f.flush()
        os.fsync(self.f.fileno())
        if PUBLISH_TO_MQTT:
            self.mqtt.publish(TOPIC, payload)

logging.basicConfig(level=logging.INFO,
                    format='%(asctime)s - %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S')

vault_path = "."

mqtt_config = get_config_from_env()

logging.info(
    f"Using vault {vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}")

WokeNote.start_background_actors(vault_path, mqtt_config)

logger = InstrumentationNote.start("OTel Claude Logger")


app = FastAPI()



# One lock shared by both handlers so concurrent metric/log exports
# from Claude Code never interleave partial lines in the file.
#_write_lock = threading.Lock()  # FIXME: get rid of this


def append_jsonl(record: dict) -> None:
    """Append a single JSON object as a line, then flush + fsync immediately."""
    record.setdefault("_received_at", datetime.now(timezone.utc).isoformat())
    line = json.dumps(record, separators=(",", ":"))
    logger.tell(("record", line))
#    with _write_lock:
#        with open(LOG_PATH, "a", encoding="utf-8") as f:
#            f.write(line + "\n")
#            f.flush()
#            os.fsync(f.fileno())


def get_attr_value(value_obj):
    """OTLP JSON wraps values as {"stringValue": ...} / {"intValue": ...} / etc."""
    if value_obj is None:
        return None
    for key in ("stringValue", "intValue", "doubleValue", "boolValue"):
        if key in value_obj:
            return value_obj[key]
    return value_obj


def attrs_to_dict(attrs: list) -> dict:
    return {a["key"]: get_attr_value(a.get("value")) for a in attrs}


@app.post("/v1/metrics")
async def receive_metrics(request: Request):
    body = await request.json()
    for rm in body.get("resourceMetrics", []):
        resource_attrs = attrs_to_dict(rm.get("resource", {}).get("attributes", []))
        for sm in rm.get("scopeMetrics", []):
            for metric in sm.get("metrics", []):
                name = metric["name"]
                unit = metric.get("unit")
                container = metric.get("sum") or metric.get("gauge") or {}
                for dp in container.get("dataPoints", []):
                    value = dp.get("asInt")
                    if value is None:
                        value = dp.get("asDouble")
                    append_jsonl({
                        "kind": "metric",
                        "name": name,
                        "unit": unit,
                        "value": value,
                        "attributes": attrs_to_dict(dp.get("attributes", [])),
                        "resource": resource_attrs,
                    })
    return {}


@app.post("/v1/logs")
async def receive_logs(request: Request):
    body = await request.json()
    for rl in body.get("resourceLogs", []):
        resource_attrs = attrs_to_dict(rl.get("resource", {}).get("attributes", []))
        for sl in rl.get("scopeLogs", []):
            for record in sl.get("logRecords", []):
                attrs = attrs_to_dict(record.get("attributes", []))
                append_jsonl({
                    "kind": "event",
                    "event_name": attrs.get("event.name"),
                    "attributes": attrs,
                    "resource": resource_attrs,
                })
    return {}


@app.get("/health")
async def health():
    return {"status": "ok", "log_path": os.path.abspath(LOG_PATH)}


#try:
#    input()
#finally:
#    print("Stopping!")
#    WokeNote.stop_background_actors()
#    logger.stop()

