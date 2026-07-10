def on_start():
    my_note.set_file_contents(f"""---
duration_seconds: 1
---
- [ ] Start timer
""")


def on_note_modified():
    if (maybe_note := my_note.note_if_markdown_starts_with_pressed_button()) is not None:
        frontmatter, markdown = maybe_note
    else:
        return # not a note update we care about

    if (duration_seconds := frontmatter.get("duration_seconds")) is None:
        logging.warning("Button pressed but Frontmatter duration_seconds was empty")
        return
    
    set_timer(duration_seconds, duration_seconds)
    sleep(.25) # hack so Obsidian sees the update (otherwise it seems to miss it sometimes)
    my_note.upsert_markdown(lambda markdown: upserter(duration_seconds, markdown))

def on_timer(key, duration_seconds):
    my_note.append_datetimestamped_markdown_list_line(f"Finished {duration_seconds}s timer")

def upserter(duration_seconds, markdown):
    lines = markdown.splitlines()
    lines[0] = f"- [ ] Start timer"
    lines.append(f"- \\[{ctime()}] Just set a timer for {duration_seconds}s\n")

    return "\n".join(lines)
