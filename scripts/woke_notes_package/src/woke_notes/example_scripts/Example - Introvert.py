def on_start():
    my_note.set_file_contents(f"""---
in_topic: {default_topic}
reply_with: "👋"
---
- [ ] Pause replying
- Instructions:
    - Change `reply_with` to whatever you would like (`in_topic` does not read back updates currently)
    - delay_seconds is an optional field, assumed to be 0 (seconds)
- ---
""")

sending = True

def on_mqtt_message(topic, message):
    config = my_note.get_frontmatter()

    try:
        # expects:
        # sending_topic> message
        sender, what_they_sent = message.decode().split(">", 1)
    except ValueError:
        my_note.append_datetimestamped_markdown_list_line(f"Received a weirdly formatted message: {message}")
        return

    if not sending:
        my_note.append_datetimestamped_markdown_list_line(f"{sender} sent {what_they_sent}, not replying (sending paused)")
        return

    delay_seconds = config.get("delay_seconds", 0)

    if (reply_with := config.get("reply_with")) is not None:
        if delay_seconds:
            set_timer(delay_seconds, (sender, reply_with))
            my_note.append_datetimestamped_markdown_list_line(f"Received {what_they_sent}, replying with {reply_with} after {delay_seconds}s")
        else:
            my_note.append_datetimestamped_markdown_list_line(f"Received {what_they_sent}, replying with {reply_with}")
            mqtt.publish(sender, reply_with)
    else:
        my_note.append_datetimestamped_markdown_list_line(f"{sender} sent {what_they_sent}, not replying (no reply_with frontmatter)")

def on_timer(key, payload):
    sender, reply_with = payload
    mqtt.publish(sender, reply_with)


def on_note_modified():
    global sending  # nonlocal doesn't work because sending is not enclosed in a function

    maybe_note = my_note.note_if_markdown_starts_with_pressed_button()
    if maybe_note is not None:
        sending = False
    else:
        sending = True
