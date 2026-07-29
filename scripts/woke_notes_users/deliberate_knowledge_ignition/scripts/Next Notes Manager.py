if TYPE_CHECKING:  # IDE support
    from ..dsl import *


def on_start():
    frontmatter, markdown = my_note.get_note()

    lines = markdown.strip().split('\n')

    failed_matches = []
    for line in lines:
        if not line.startswith('- [[') or not line.endswith(']]'):
            failed_matches.append(line)

    if failed_matches:
        logging.warning(f"Failed matches! {failed_matches} (lines: {lines})")
        return

    template_to_use = frontmatter.get("template_to_use", "Next-Notes MOC")
    logging.info(f"Using template: {template_to_use}")

    next_notes = [line[4:-2] for line in lines]
    for next_note in next_notes:
        logging.info(f"waking [[{next_note}]]")
        wake(next_note, TemplateScript(template_to_use))
