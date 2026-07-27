import logging
import argparse

from .orchestration import Orchestrator
from .woke_note import WokeNote
from .wrappers.external_messages import get_config_from_env


logging.basicConfig(level=logging.INFO,
                    format='%(asctime)s - %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S')

parser = argparse.ArgumentParser()
parser.add_argument("vault_path", help="Path to the vault directory", type=str)

# FIXME: need to make sure this is properly optional, in the Orchestrator
parser.add_argument("--scripts", help="The directory for scripts")

args = parser.parse_args()

mqtt_config = get_config_from_env()
if mqtt_config:
    logging.info(f"Using vault {args.vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}, and scripts_dir {args.scripts}")
else:
    logging.info(f"Failed to get mqtt config, proceeding with None config...")

WokeNote.start_background_actors(args.vault_path, mqtt_config)
# FIXME: can I delete the password to let it get garbage collected?

# pykka.DeadLetterRouter.default_router().start() #?
orchestrator = Orchestrator.wake("Woke Notes Orchestrator", args.scripts)

# FIXME: what to do instead?
try:
    while True:
        input()
except KeyboardInterrupt:
    logging.info("KeyboardInterrupt detected, stopping...")
    orchestrator.stop()
    logging.info("Orchestrator stopped")
