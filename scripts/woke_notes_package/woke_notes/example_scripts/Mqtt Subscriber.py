def on_start():
    my_note.set_file_contents(f"""---
in_topic: "{topic}"
---
""")

def on_mqtt_message(topic, message):
    my_note.append_timestamped_markdown_list_line(f"received on `{topic}`: {message}")
