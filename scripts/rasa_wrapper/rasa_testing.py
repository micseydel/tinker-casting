import sys
from pprint import pprint
from dataclasses import dataclass
from datetime import datetime

import rasa_wrapper

import yaml


@dataclass
class RasaEntity:
    entity_name: str
    entity_value: str
    confidence: float


@dataclass
class RasaResult:
    intent_name: str
    intent_confidence: float
    entities: list[RasaEntity]


rasa_model = rasa_wrapper.ModelWrapper(sys.argv[1])
print(f"[{datetime.now()}] Model loaded")

nlu_yml = yaml.safe_load(open(sys.argv[2]).read())

intents = [block for block in nlu_yml['nlu'] if "intent" in block]

# HACK
# examples = next(block for block in nlu_yml['nlu'] if block.get("intent") == "observe_sifted_contents")["examples"]


def hacky_parser(line: str) -> list[str | dict]:
    """
    strs are just regular text, dicts are entities like
    {entity_name: (raw_entity, corrected_entity)}
    """
    first_split = line.split('[', 1)
    if len(first_split) == 1:
        return []

    beginning, first_entity_andtherest = first_split

    raw_entity, entity_name_andtherest = first_entity_andtherest.split('](', 1)

    try:
        entity_name, corrected_entity_andtherest = entity_name_andtherest.split(':', 1)
    except ValueError:
        print("Tried to split on colon (:) -", entity_name_andtherest)
        raise

    corrected_entity, the_rest = corrected_entity_andtherest.split(')', 1)

    return [beginning, {entity_name: (raw_entity, corrected_entity)}] + hacky_parser(the_rest)


def original(item):
    key, value = item
    return f"{value[0]}"


def canon(item):
    key, value = item
    return f"{value[1]} {key}(s)"


def gen_results(examples):
    for raw_line in examples:
        parsed_from_simple = hacky_parser(raw_line)
        parsed_from_simple_entities = [e for e in parsed_from_simple if isinstance(e, dict)]

        full_canon = "".join(e if isinstance(e, str) else canon(next(iter(e.items()))) for e in parsed_from_simple)
        full_original = "".join(e if isinstance(e, str) else original(next(iter(e.items()))) for e in parsed_from_simple)

        parsed_by_rasa = rasa_model.parse_string(full_original)

        intent = parsed_by_rasa["intent"]
        entities = parsed_by_rasa["entities"]

        yield raw_line, parsed_from_simple_entities, entities, full_original, full_canon, intent["name"]


# FIXME: cleanup hackiness to keep punting printing until after the underlying library vomit
def f():
    for intent in intents:
        intent_name = intent["intent"]
        # print(intent_name)
        examples = [line[2:] for line in intent["examples"].splitlines()]

        result = list(gen_results(examples))
        # print('\n'*3)
        failed = 0
        succeeded = 0
        failures = []
        for raw_line, parsed_from_simple_entities, entities, full_original, full_canon, result_intent_name in result:
            combined_parsed_simple = {k: v for d in parsed_from_simple_entities for k, v in d.items()}

            if not entities:
                if result_intent_name == intent_name or (intent_name == "no_intent" and result_intent_name == "nlu_fallback"):
                    succeeded += 1
                else:
                    failed += 1
                    failures.append(
                        (raw_line, full_original, full_canon, parsed_from_simple_entities,
                         {"error": f"WRONG INTENT, expected {intent_name} but got {result_intent_name}"}
                         ))
                continue

            for entity in entities:
                entity_name = entity["entity"]
                if entity_name in combined_parsed_simple:
                    value = combined_parsed_simple[entity_name][1]
                else:
                    value = None

                if entity["value"] != value:
                    # print(" ", {k: v for k, v in entity.items() if (k not in ("extractor", "processors", "start", "end"))})
                    # print(" ", full_original[entity["start"]:entity["end"]])
                    failures.append((raw_line, full_original, full_canon, parsed_from_simple_entities, entity))
                    failed += 1
                else:
                    succeeded += 1

        yield intent_name, failed, succeeded, failures


t = list(f())
print('\n'*6)
saw_failure = False
for intent_name, failed, succeeded, failures in t:
    print(f"{intent_name} done! Succeeded: {succeeded} ... Failed: {failed}")

    if failed or failures:
        saw_failure = True

    for (raw_line, full_original, full_canon, parsed_from_simple_entities, entity) in failures:
        print("- ", raw_line)
        # print((
        #     # raw_line, full_original, full_canon,
        #     parsed_from_simple_entities, entity))
        combined_parsed_simple = {k: v for d in parsed_from_simple_entities for k, v in d.items()}
        # pprint(combined_parsed_simple)
        # pprint(entity)
        pprint({k: v for k, v in entity.items() if (k not in ("extractor", "processors", "start", "end"))})
        key = None
        try:
            key = entity["entity"]
            expected = combined_parsed_simple[key][1]
        except KeyError as e:
            if key is not None:
                raise Exception(f"Key {key} was not in {combined_parsed_simple}") from e

            raise Exception(f"Did not find key `entity` in: {entity}") from e
        got = entity["value"]
        print("  - Expected:", expected)
        print("  - But got: ", got)

    print('\n\n')

if not saw_failure:
    print("Success!")
