import chime

def on_start():
	my_note.set_file_contents("""---
delay_seconds: 0
---
- [ ] Chime
""")

def on_note_modified():
	maybe_note = my_note.frontmatter_and_markdown_if_button_pressed()
	if maybe_note is not None:
		frontmatter, markdown = maybe_note
		delay = frontmatter.get("delay_seconds")
		if delay:
			set_timer(delay)
			logging.info(f"Will chime after delay {delay}s")
		else:
			logging.info("Chiming immediately...")
			chime.success()
		sleep(0.25)  # so Obsidian doesn't miss the file change
		my_note.set_markdown("- [ ] Chime\n")

def on_timer(payload):
	logging.info("Chiming immediately...")
	chime.success()

# def on_mqtt_message(topic: str, payload: bytes):








































# FIXME: chime.themes()
longer = """---
{theme: big-sur}
---
# Issue Command

- [ ] Success
- [ ] Warning
- [ ] Info
- [ ] Error

# Select a Theme

""" + "\n".join(f"- [ ] {theme}" for theme in chime.themes())

# e.g.
# - [ ] big-sur
# - [ ] chime
# - [ ] mario
# - [ ] material
# - [ ] pokemon
# - [ ] sonic
# - [ ] zelda
