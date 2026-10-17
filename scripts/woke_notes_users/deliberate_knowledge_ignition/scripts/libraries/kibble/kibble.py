import logging
import re
import datetime
from collections import defaultdict
from copy import copy
from typing import Generator, Iterator

from libraries.kibble.managers import BowlManger, StockManager, raw_refill_to_stock, StockUpdate
from libraries.kibble.model import Event, EventLine, EventBlock

# these events mean that kibble entered the tupperware
REFILLED_EVENT = "refilled"
BATCH_REFILL_EVENT = "BATCH_REFILL_EVENT"

# this event, combined with refills and prior measurements, indicates kibble leaving the tupperware
MEASURED_EVENT = "measured"

# these events allow a reset of uncertainty
DISCARDED_EVENT = "discarded"
CARRYOVER_EVENT = "carryover"

FINISHED_CONTAINER_EVENT = "FINISHED_CONTAINER_EVENT"


def simple_batch_by_time(everything: list[Event], max_gap_minutes=30) -> \
        Generator[list[Event], None, None]:
    if not everything:
        return

    max_gap = datetime.timedelta(minutes=max_gap_minutes)

    items: Iterator[Event] = iter(sorted(everything, key=lambda e: e.event_time))  # FIXME rich comparison
    batch: list[Event] = [next(items)]
    for event_line in items:
        if event_line.event_time - batch[-1].event_time >= max_gap:
            yield batch
            batch = [event_line]
        else:
            batch.append(event_line)

    yield batch


def bundle_refills(events: list[EventLine]) \
        -> Generator["Event", None, None]:
    batch: None | tuple[datetime.datetime, list["EventLine"]] = None

    def batch_to_eventblock(b: tuple[datetime.datetime, list["EventLine"]]) -> EventBlock:
        batch_start_time, composing_events = b
        el = composing_events[0]
        return EventBlock.from_events(
            composing_events,
            (BATCH_REFILL_EVENT, dict((el.event[1], el.event[2]) for el in composing_events)),
            f"#^{el.block_id}",
            f"bundled-{el.block_id.split('-')[1]}"
        )

    for event_line in events:
        event = event_line.event
        event_type = event[0]
        for_day = event_line.date()

        if event_type == REFILLED_EVENT:
            if batch is None:
                batch = event_line.event_time, [event_line]
            else:
                batch_starttime, batched_refills = batch
                batch_date = batch_starttime.date()
                assert for_day == batch_date, \
                    f"batching refills failed, had {batch_starttime} ({batch_date}) but event {event} for day {for_day} did not match days"
                batched_refills.append(event_line)
        else:
            if batch is not None:
                yield batch_to_eventblock(batch)
                batch = None

            if event_type in (MEASURED_EVENT, CARRYOVER_EVENT, DISCARDED_EVENT, FINISHED_CONTAINER_EVENT):
                yield event_line.copy_refing_this(lambda bid: f"bundled-{bid.lstrip('#^').split('-')[-1]}")
            else:
                raise ValueError(f"Unknown event type {event_type}")

    if batch is not None:
        yield batch_to_eventblock(batch)


def mentions_kibble(text):
    return "kibble" in text or "dry food" in text in text


def get_last_int(text) -> int | None:
    results = re.findall(r'\d+', text)
    return int(results[-1]) if results else None


##


def process_events(empties, event_lines: list[EventLine]) -> \
        tuple[list[Event], list[Event], list[Event], dict[str, object]]:
    raw_events_batched_refills: list[Event] = list(bundle_refills(event_lines))

    audit: dict[str, object] = defaultdict(int)

    stock_and_raw_bowl_changes: list[Event] = []
    stock_manager = StockManager(empties)
    stock: None | dict[str, int] = None
    for event in raw_events_batched_refills:
        event_type = event.event_type()

        synthesized_event: Event
        if event_type == BATCH_REFILL_EVENT:
            synthesized_event = stock_manager.integrate_refills(event, event.event[1])
        elif event_type == MEASURED_EVENT:
            synthesized_event = stock_manager.integrate_measurement(event, event.event[1], event.event[2])
        elif event_type == FINISHED_CONTAINER_EVENT:
            synthesized_event = stock_manager.integrate_finished(event, event.event[1])
        elif event_type in (CARRYOVER_EVENT, DISCARDED_EVENT):
            if event_type == DISCARDED_EVENT: audit[event_type] += event.event[1]
            synthesized_event = event.copy_refing_this(lambda bid: f"stockchange-{bid.split('-')[-1]}")
        else:
            if event_type == REFILLED_EVENT:
                raise ValueError(
                    f"{REFILLED_EVENT} unexpected here - it should have been batched first into a {BATCH_REFILL_EVENT}")
            else:
                expected_events = {MEASURED_EVENT, CARRYOVER_EVENT, DISCARDED_EVENT, FINISHED_CONTAINER_EVENT,
                                   BATCH_REFILL_EVENT}
                raise ValueError(f"Unknown event {event_type}, expected: {expected_events}")

        if event_type in (BATCH_REFILL_EVENT, MEASURED_EVENT, FINISHED_CONTAINER_EVENT):
            audit["dispensed_accordingto_stock_manager"] += synthesized_event.event[-1]

        stock_and_raw_bowl_changes.append(synthesized_event)

        if synthesized_event.event_type() == "stock_update":
            sevent: StockUpdate = synthesized_event.event

            if sevent.update_type == "refill":
                stock = copy(sevent.new_stock)
            elif sevent.update_type == "dispensed":
                if stock is not None:
                    if isinstance(sevent.dispensed_container, dict):
                        to_deduct = sevent.dispensed_container.items()
                        assert sum(x[1] for x in to_deduct) == sevent.dispensed_value
                    else:
                        to_deduct = [(sevent.dispensed_container, sevent.dispensed_value)]
                    for container, v in to_deduct:
                        stock[container] -= v
                        assert stock[container] >=0
                    #     for container, dispensed in sevent.dispensed_container.items():
                    #         stock[container] -= dispensed
                    # else:
                    #     stock[sevent.dispensed_container] -= sevent.dispensed_value
                    assert sevent.new_stock == stock, f"{sevent.new_stock} != {stock}"
                else:
                    assert sevent.dispensed_value == 28, f"{sevent.dispensed_value} was not 28"
                    day = synthesized_event.date()
                    expected = datetime.date(2026, 8, 28)
                    assert day == expected,\
                        f"{day} did not match {expected}, {type(day), type(expected)}"
            else:
                raise RuntimeError(f"Unknown event type {sevent.event_type} for {event.date()}")

    bowl_events: list[Event] = []
    batched_stock_and_raw_bowl_changes: list[list[Event]] = list(simple_batch_by_time(stock_and_raw_bowl_changes))
    bowl = BowlManger()
    for event_batch in batched_stock_and_raw_bowl_changes:
        bowl_event = bowl.integrate(event_batch)
        if bowl_event is not None:
            bowl_events.append(bowl_event)

            if bowl_event.event_type() == "dispensed":
                audit["dispensed_accordingto_bowl_manager"] += bowl_event.event[1]
            # FIXME: MIXED_MANUAL is where a little of dispensed is lost...
            elif bowl_event.event_type() in ("consumed", "MIXED_MANUAL"):
                audit["consumed"] += bowl_event.event[1]
            else:
                logging.warning(f"Unknown event type: {bowl_event.event_type()}")

    # auditing

    batch_refill_sums_correct = [sum(raw_refill_to_stock(empties, e.event[1]).values())
                                 for e in raw_events_batched_refills if e.event_type() == BATCH_REFILL_EVENT]
    total_sum_batch_refills_corrected = sum(batch_refill_sums_correct)

    remaining_stock = sum(stock_manager.latest_stock.contents.values())

    audit["less_raw"] = {
        # "batch_refill_sums": batch_refill_sums_correct,
        "remaining_stock": remaining_stock,
        "total_sum_batch_refills": total_sum_batch_refills_corrected,
        "dispensed_accordingto_stock_manager - discarded - consumed": audit["dispensed_accordingto_stock_manager"] - audit["discarded"] - audit["consumed"],
    }

    return raw_events_batched_refills, stock_and_raw_bowl_changes, bowl_events, audit


# FIXME: get rid of these entirely?
REFILLED_PAT = re.compile("refilled (the |a )?(.+) dry food")
MEASURED_PAT = re.compile("measured (the |a )(.+) dry food")


def extract(text) -> None | tuple:
    if "discard" in text:
        return (DISCARDED_EVENT, get_last_int(text))
    elif REFILLED_EVENT in text:
        match = REFILLED_PAT.search(text)
        if match:
            container = match.group(2)
            return (REFILLED_EVENT, container, get_last_int(text))
        else:
            return (REFILLED_EVENT, None, get_last_int(text))
    elif ("carry" in text or "carried" in text) and "over" in text:
        return (CARRYOVER_EVENT, get_last_int(text))
    elif MEASURED_EVENT in text:
        match = MEASURED_PAT.search(text)
        if match:
            container = match.group(2)
            return (MEASURED_EVENT, container, get_last_int(text))
        else:
            return (MEASURED_EVENT, None, get_last_int(text))
    else:
        return None


