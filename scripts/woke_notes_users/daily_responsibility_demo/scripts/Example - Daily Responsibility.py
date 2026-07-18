if TYPE_CHECKING:  # IDE support
    from dsl import *


def on_start():
    reset_timer_from_note()  # THERE's ALWAYS _a_ TIMER, for later today or tomorrow


def on_note_modified():
    note = my_note.note_if_markdown_starts_with_pressed_button()
    if note:
        frontmatter, markdown = note
        today = datetime.date.today().isoformat()
        sleep(0.25)
        if today not in markdown:
            updated_markdown = markdown_list_prepender(markdown)
            reset_timer_from_note(note)
            my_note.set_markdown(updated_markdown)
        else:
            my_note.reset_button_at_start_of_markdown()


def on_timer(_key, _payload):
    note = frontmatter, _markdown = my_note.get_note()
    channel = frontmatter.get("channel")
    message = frontmatter.get("message")
    logging.info(f"Timer called, publishing to {channel}: {message}")
    publish_to_ntfy(channel, message)
    reset_timer_from_note(note)


# utils

def markdown_list_prepender(markdown: str) -> str:
    """
    - reset the button
    - prepend today's date (after the button)
    """
    lines = markdown.splitlines()
    lines[0] = "- [ ] Mark as done"
    line_to_prepend = f"- [[{my_note.note_name} ({datetime.date.today().isoformat()})]]"
    lines.insert(1, line_to_prepend)
    lines[-1] = lines[-1] + "\n"
    return "\n".join(lines)


def reset_timer_from_note(note=None):
    """
    resets the timer for later today or tomorrow
    """
    if note is None:
        note = my_note.get_note()

    if note is not None:
        frontmatter, markdown = note

        next_notification = next_occurrence(frontmatter.get("ifNotDoneBy"))
        today = datetime.date.today()

        today_marked_as_done = today.isoformat() in markdown
        next_notification_is_for_today = ((today.year, today.month, today.day) ==
                                          (next_notification.year, next_notification.month, next_notification.day))
        
        if today_marked_as_done and next_notification_is_for_today:
            next_notification += datetime.timedelta(days=1)

        delay = seconds_until(next_notification)
        logging.info(f"Setting timer for {next_notification}, delay {delay}s")
        set_timer(
            delay,
            None,  # no payload for this timer
            key=f"{my_note.note_name}/TIMER"  # there's just one timer
        )
    else:
        logging.warning("No note; need yaml for {channel, message, ifNotDoneBy} (e.g. ifNotDoneBy = 20:32:00-07:00)")
