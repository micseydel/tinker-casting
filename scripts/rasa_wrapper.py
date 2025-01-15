from pprint import pprint, pformat

import os
import asyncio
from time import perf_counter
from datetime import datetime

from rasa.core.agent import Agent


class ModelWrapper:
    def __init__(self):
        self._load_model()

    def parse_string(self, content: str) -> dict:
        self._reload_model_if_needed()

        async def parse_input_text(agent, text):
            result = await agent.parse_message(text)
            return result

        return asyncio.run(parse_input_text(self.agent, content))

    def _reload_model_if_needed(self) -> None:
        latest_model_path = newest_file_in_dir("models/")
        if self.model_path != latest_model_path:
            now = datetime.now()
            old_name = name_for_model_path(self.model_path)
            new_name = name_for_model_path(latest_model_path)
            print(f"\n\n[{now}] Found new model, replacing {old_name} with {new_name}")
            self._load_model()

    def _load_model(self) -> None:
        model_path = newest_file_in_dir("models/")
        model_name = name_for_model_path(model_path)

        print('\n'*4)
        print(f"========================================================================")
        print(f"| [{datetime.now()}] Loading model {model_name}... |")
        print(f"| What follows is output that cannot be suppressed, print()s from Rasa  |")
        print(f"| and/or its dependencies 💩💩                                         |")
        print(f"========================================================================")
        print('\n'*4)

        t0 = perf_counter()
        self.agent = Agent.load(model_path)
        self.model_path = model_path
        t1 = perf_counter()

        print('\n'*4)
        print(f"=========================================================================")
        print(f"|                         Nasty output over                              |")
        print(f"| [{datetime.now()}] Model {model_name} loaded, |")
        print(f"|                       took {t1 - t0:0.1f}s                             |")
        print(f"=========================================================================")
        print('\n'*4)


def name_for_model_path(model_path) -> str:
    return os.path.split(model_path)[1][:-7]


def newest_file_in_dir(directory) -> str:
    files = (f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f)) and f != ".DS_Store")
    paths = (os.path.join(directory, file) for file in files)
    return max(paths, key=os.path.getctime)
