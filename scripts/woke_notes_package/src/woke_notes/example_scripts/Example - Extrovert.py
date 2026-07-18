def on_start():
    my_note.set_file_contents(f"""---
in_topic: {default_topic}
out_topic: {default_topic}/publish
message_to_send: "Hello :)"
---
- [ ] Send message
- Instructions:
    - `out_topic` is updated live, but `in_topic` is currently provided but not read back
- ---
""")

def on_mqtt_message(topic, message):
    my_note.append_datetimestamped_markdown_list_line(f"Received on {topic} message: {message.decode()}")

def on_note_modified():
    if (maybe_note := my_note.note_if_markdown_starts_with_pressed_button()) is not None:
        frontmatter, markdown = maybe_note
    else:
        return # not a note update we care about

    if (out_topic := frontmatter.get("out_topic")) is None:
        logging.warning("Button pressed but Frontmatter out_topic was empty")
        return

    if (message_to_send := frontmatter.get("message_to_send")) is None:
        logging.warning("Button pressed but Frontmatter message_to_send was empty")
        return

    # introvert expects `(sender)> (message)` where sender is who they will reply to
    mqtt.publish(out_topic, f"{topic}> {message_to_send}")

    sleep(.25) # hack so Obsidian sees the update (otherwise it seems to miss it sometimes)
    my_note.reset_button_at_start_of_markdown()
