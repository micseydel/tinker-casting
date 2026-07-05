def on_start():
    mqtt.publish("[[Google Calendar]]", json.dumps({"type": "get_two_weeks", "replyTo": default_topic}))

def on_mqtt_message(topic, raw_payload):
    logging.info(f"Received {len(raw_payload)} bytes on {topic}, parsing JSON now")
    payload = json.loads(raw_payload)
    events = payload.get("events")

    if events is None:
        logging.warning(f"Empty events! {raw_payload}")
        return

    sections = []
    for i, event in enumerate(events):
        date_time = event['start'].get('dateTime')
        if date_time is not None:
            sections.append(
                f"# {event['summary']} ({date_time[:10]})\n\n"
                "```json\n"
                f"{json.dumps(event, indent=4)}\n"
                "```\n"
            )

    formatted_sections = '\n'.join(sections)

    my_note.set_markdown(f"""- {ctime()}
- {len(events)} total events for the next 2 weeks

{formatted_sections}
""")

# on_start()  # FIXME: hack!!
