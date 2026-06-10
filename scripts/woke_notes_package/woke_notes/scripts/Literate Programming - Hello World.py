# FIXME: set note default contents - need an on_start ??

def on_mqtt_message(topic, message):
    my_note.append(f"- \\[{ctime()}] {topic}: {message}\n")


def on_note_modified():
    maybe_note = my_note.note_if_button_pressed()
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

    # FIXME: wrapper so I can say `mqtt.publish(topic, msg)`
    mqtt.tell(MqttPublish(out_topic, message_to_send))


    # FIXME: lazy, this should just modify the first line
    my_note.upsert_markdown(lambda markdown: markdown.replace("- [x]", "- [ ]"))
