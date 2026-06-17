def on_start():
    my_note.set_contents(f"""---
in_topic: {default_topic}
out_topic: {default_topic}/publish
message_to_send: "Hello :)"
---
- [ ] Send message
- Instructions:
    - press the button above to send `message_to_send` to `out_topic`
    - (this can be observed via mosquitto_pub)
    - send a message to `in_topic` and observe it listed below
    - `out_topic` is read dynamically but `in_topic` is provided for informational purposes only
- ---
""")


def on_mqtt_message(topic, message):
    my_note.append_timestamped_markdown_list_line(f"received on `{topic}`: {message}")


def on_note_modified():
    if (maybe_note := my_note.note_if_markdown_starts_with_pressed_button()) is not None:
        frontmatter, markdown = maybe_note
    else:
        logging.debug("Note modified but button not pressed")
        return
    
    if (out_topic := frontmatter.get("out_topic")) is None:
        logging.warning("Button pressed but Frontmatter out_topic was empty")

    if (message_to_send := frontmatter.get("message_to_send")) is None:
        logging.warning("Button pressed but Frontmatter message_to_send was empty")
        return

    logging.info(f"[{note_name}] publishing {message_to_send} to mqtt")
    mqtt.publish(out_topic, message_to_send)

    logging.info(f"[{note_name}] resetting markdown...")

    sleep(.25) # hack to prevent a race condition with Obsidian's display
    my_note.upsert_markdown(lambda markdown: upserter(out_topic, message_to_send, markdown))
    logging.info(f"[{note_name}] Markdown reset")

def upserter(out_topic, message_to_send, markdown):
    lines = markdown.splitlines()
    lines[0] = f"- [ ] Send (last sent ~{ctime()})"
    lines.append_timestamped_markdown_list_line(f"sending on `{out_topic}`: {message_to_send}")

    return "\n".join(lines)
