my_note.set_contents(f"""---
in_topic: {default_topic}
out_topic: {default_topic}/publish
message_to_send: "Hello :)"
---
- [ ] Send message
""")

def on_mqtt_message(topic, message):
    my_note.append(f"- \\[{ctime()}] Received on {topic} message: {message.decode()}\n")

def on_note_modified():
    maybe_note = my_note.note_if_markdown_starts_with_pressed_button()
    if maybe_note is not None:
        frontmatter, markdown = maybe_note
    else:
        logging.debug("Note modified but button not pressed")
        return

    out_topic = frontmatter.get("out_topic")
    if out_topic is None:
        logging.warning("Button pressed but Frontmatter out_topic was empty")

    message_to_send = frontmatter.get("message_to_send")
    if message_to_send is None:
        logging.warning("Button pressed but Frontmatter message_to_send was empty")
        return

    logging.debug(f"[{note_name}] Sending to {out_topic}")
    mqtt.publish(out_topic, f"{topic}> {message_to_send}")


    sleep(.25) # omfg
    # FIXME: lazy, this should just modify the first line
    my_note.upsert_markdown(lambda markdown: markdown.replace("- [x]", "- [ ]"))
