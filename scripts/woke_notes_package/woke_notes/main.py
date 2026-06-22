import logging
import sys

from orchestration import Orchestrator
from woke_note import WokeNote
from wrappers.external_messages import get_config_from_env

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    try:
        _, vault_path, scripts_dir = sys.argv
    except ValueError:
        print("Expected two CLI args, vault path and then scripts dir\n", sys.stderr)
        raise

    mqtt_config = get_config_from_env()

    logging.info(f"Using vault {vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}, and scripts_dir {scripts_dir}")

    # FIXME: update the readme!
    WokeNote.start_background_actors(vault_path, mqtt_config)
    # FIXME: can I delete the password to let it get garbage collected?

    # pykka.DeadLetterRouter.default_router().start() #?
    threaded_pykka_actors = Orchestrator.wake("Woke Notes Orchestrator", scripts_dir)
