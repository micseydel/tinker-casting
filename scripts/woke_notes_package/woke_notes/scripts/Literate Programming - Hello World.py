# FIXME: set note default contents - need an on_start ??

def on_mqtt_message(topic, message):
    my_note.append(f"- \\[{ctime()}] received on `{topic}`: {message}\n")


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

    logging.info(f"[{note_name}] publishing {message_to_send} to mqtt")
    mqtt.publish(out_topic, message_to_send)

    logging.info(f"[{note_name}] resetting markdown...")

    sleep(.25) # omfg
    my_note.upsert_markdown(lambda markdown: upserter(out_topic, message_to_send, markdown))
    logging.info(f"[{note_name}] Markdown reset")

def upserter(out_topic, message_to_send, markdown):
    lines = markdown.splitlines()
    lines[0] = f"- [ ] Send (last sent ~{ctime()})"
    lines.append(f"- \\[{ctime()}] sending on `{out_topic}`: {message_to_send}\n")

    return "\n".join(lines)
