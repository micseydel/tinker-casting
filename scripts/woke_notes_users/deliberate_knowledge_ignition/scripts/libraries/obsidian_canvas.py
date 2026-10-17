import json
from dataclasses import dataclass, field

# https://jsoncanvas.org/spec/1.0/

# sides
LEFT = "left"
RIGHT = "right"
TOP = "top"
BOTTOM = "bottom"

# colors
RED = "1"
ORANGE = "2"
YELLOW = "3"
GREEN = "4"
CYAN = "5"
PURPLE = "6"

# node types
GROUP = "group"
TEXT = "text"
# FILE = "file"
# LINK = "link"


@dataclass
class Canvas:
    filepath: str
    nodes: list[dict] = field(default_factory=list)
    edges: list[dict] = field(default_factory=list)

    def add_text_node(self, id: str, x: int, y: int, width: int, height: int, text: str, color: str | None = None):
        node = {
            "id": id,
            "type": TEXT,
            "x": x,
            "y": y,
            "width": width,
            "height": height,
            "text": text,
        }
        if color is not None:
            node["color"] = color
        self.nodes.append(node)

    def add_group_node(self, id: str, x: int, y: int, width: int, height: int, label: str, color: str | None = None):
        group = {
            "id": id,
            "type": GROUP,
            "x": x,
            "y": y,
            "width": width,
            "height": height,
            "label": label,
        }
        if color is not None:
            group["color"] = color
        self.nodes.append(group)

    def add_edge(self, id: str, fromNode: str, fromSide: str, toNode: str, toSide: str, color: str | None = None):
        edge = {
            "id": id,
            "fromNode": fromNode,
            "fromSide": fromSide,
            "toNode": toNode,
            "toSide": toSide,
        }
        if color is not None:
            edge["color"] = color
        self.edges.append(edge)

    def flush(self) -> None:
        # FIXME: json serialization isn't free, is it?
        canvas = {"nodes": self.nodes, "edges": self.edges}
        with open(self.filepath, "w") as f:
            json.dump(canvas, f, indent=4)
