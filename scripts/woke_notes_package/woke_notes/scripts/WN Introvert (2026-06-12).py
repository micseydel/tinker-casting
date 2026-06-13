my_note.set_contents(f"""---
in_topic: {topic}
reply_with: "👋"
---
- [ ] Pause sending
""")

sending = True

def on_mqtt_message(topic, message):
    config = my_note.get_frontmatter()

    sender, what_they_sent = message.decode().split(">", 1)
    if not sending:
        my_note.append(f"- \\[{ctime()}] {sender} sent {what_they_sent}, not replying (sending paused)\n")
        return

    reply_with = config.get("reply_with")
    if reply_with is not None:
        my_note.append(f"- \\[{ctime()}] Received {what_they_sent}, replying with {reply_with}\n")
        mqtt.publish(sender, reply_with)
    else:
        my_note.append(f"- \\[{ctime()}] {sender} sent {what_they_sent}, not replying (no reply_with frontmatter)\n")
    

def on_note_modified():
    global sending  # nonlocal doesn't work because sending is not enclosed in a function

    maybe_note = my_note.note_if_markdown_starts_with_pressed_button()
    if maybe_note is not None:
        sending = False
    else:
        sending = True
