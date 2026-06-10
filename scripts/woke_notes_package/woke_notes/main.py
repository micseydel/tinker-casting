import logging
import sys

from orchestration import Orchestrator
from wrappers.external_messages import get_config_from_env

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    try:
        vault_path = sys.argv[1]
        scripts_dir = sys.argv[2]
    except IndexError:
        print("Expected two CLI args, vault path and then scripts dir\n", sys.stderr)
        raise

    mqtt_config = get_config_from_env()

    logging.info(f"Using vault {vault_path}, mqtt broker {mqtt_config.broker}:{mqtt_config.port} for user {mqtt_config.username}, and scripts_dir {scripts_dir}")

    # pykka.DeadLetterRouter.default_router().start() #?
    hot_reloader = Orchestrator.start(mqtt_config, vault_path, scripts_dir)
