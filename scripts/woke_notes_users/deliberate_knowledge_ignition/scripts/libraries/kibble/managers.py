import logging
from typing import NamedTuple

from libraries.kibble.model import Event, EventLine, EventBlock, Dispensing, LatestStock


class BowlEvent(NamedTuple):
    bowl_event_type: str
    dispensed: int
    current_total: int
    explanation: str


class BowlManger:
    latest: tuple[int, Event | None] = (0, None)

    # this is like an assert, it throws if it becomes negative
    dispensing: Dispensing | None = None

    def integrate(self, batch: list[Event]) -> Event | None:
        existing_contents, last_measurement = self.latest

        def make_event(event: Event, event_type: str, sub_events: list, value: int, bowl: int, explanation: str, time=None):
            return EventBlock.from_events(
                        sub_events,
                        BowlEvent(event_type, value, bowl, explanation if value > 0 else f"=={explanation}=="),
                        f"#^{event.block_id}",
                        f"bowl-{event.block_id.split('-')[1]}",
                        time or event.event_time
                    )

        updated_contents: int
        new_measurement: Event
        if len(batch) == 1:
            event = batch[0]
            event_type = event.event_type()

            if event_type == "stock_update":
                _, stock, stock_update_type, container, dispensed = event.event
                if dispensed:
                    updated_contents = existing_contents + dispensed
                    if self.dispensing is None and dispensed == 28:
                        pass  # FIXME: HACK
                    else:
                        self.dispensing -= dispensed

                    if stock_update_type == "refill":
                        dispensing = sum(stock.values())
                        self.dispensing = Dispensing(dispensing, event)

                    if last_measurement is None:
                        new_measurement = EventLine(
                            event.event_time,
                            BowlEvent("dispensed", dispensed, dispensed, f"this was the first measurement"),
                            f"#^{event.block_id}",
                            f"bowl-{event.block_id.split('-')[1]}"
                        )
                    else:
                        new_measurement = make_event(
                            event,
                            "dispensed",
                            [last_measurement, event],
                            dispensed,
                            updated_contents,
                            f"dispensed {dispensed} on top of {existing_contents}"
                        )
                    self.latest = (updated_contents, new_measurement)
                else:
                    assert (stock_update_type, container, dispensed) == ("refill", None, 0), f"wat {stock_update_type, container, dispensed}"
                    dispensing = sum(stock.values())
                    self.dispensing = Dispensing(dispensing, event)
                    return None
            elif event_type == "discarded":
                assert last_measurement is not None, f"discarded event cannot be the first event, self.latest was f{self.latest} indicating no prior measurement"

                updated_contents = 0
                discarded = event.event[1]
                consumed = existing_contents - discarded

                explanation = f"bowl contained {existing_contents} and discarded={discarded}"
                new_measurement = make_event(event, "consumed", [last_measurement, event], consumed, updated_contents, explanation)
                self.latest = (updated_contents, new_measurement)
            elif event_type == "carryover":
                # FIXME: carryover needs proper testing
                # assert last_measurement is not None, f"carryover event cannot be the first event, self.latest was f{self.latest} indicating no prior measurement"
                #
                # updated_contents = carryover = event.event[1]
                # consumed = existing_contents - carryover
                #
                # explanation = f"bowl contained {existing_contents} and carryover={carryover}"
                # new_measurement = make_event(event, "consumed", [last_measurement, event], consumed, updated_contents, explanation)
                # self.latest = (updated_contents, new_measurement)
                return None
            else:
                msg = f"{event.to_markdown()}"
                raise Exception(msg)
        elif len(batch) == 2:
            event_types = [b.event[0] for b in batch]
            assert "stock_update" in event_types

            # FIXME: carryover needs proper testing
            # assert event_types in (["stock_update", "discarded"], ["stock_update", "carryover"], ["discarded", "stock_update"],
            #                        ["carryover", "stock_update"]), \
            #     f"event_types: {event_types} for batch {batch}"
            if event_types[0] != "stock_update":
                batch = list(reversed(batch))
            assert batch[1].event_type() in ("discarded", "carryover")

            # if batch[1].event_type() == "carryover":
            #     return None

            dispensed_event, consumption_clarified = batch

            _, stock, stock_update_type, _, dispensed = dispensed_event.event

            if self.dispensing is None and dispensed == 28:
                pass  # FIXME: HACK
            else:
                try:
                    self.dispensing -= dispensed
                except TypeError as e:
                    raise RuntimeError(f"dispensed={dispensed} from {dispensed_event.to_markdown()}") from e
            # self.dispensing -= dispensed
            if stock_update_type == "refill":
                dispensing = sum(stock.values())
                self.dispensing = Dispensing(dispensing, dispensed_event)

            intermediate_contents = existing_contents + dispensed

            clarifying_event_type = consumption_clarified.event_type()
            if clarifying_event_type == "discarded":
                updated_contents = 0
                discarded = consumption_clarified.event[1]
                consumed = intermediate_contents - discarded
            elif clarifying_event_type == "carryover":
                # FIXME: carryover needs proper testing
                # updated_contents = carryover = consumption_clarified.event[1]
                # consumed = intermediate_contents - carryover
                return None
            else:
                raise Exception()

            explanation = f"existing_contents={existing_contents} then dispensed={dispensed} to {intermediate_contents}, before {consumption_clarified.event}"
            new_measurement = make_event(consumption_clarified, "consumed", batch, consumed, updated_contents, explanation)
        else:
            # NEWLINE = "\n"
            # raise Exception(f"{NEWLINE.join(b.to_markdown() for b in batch)}")
            assert len(batch) == 3, f"unexpected case, prior precedent was HACKY HACKY HACKY - batch = {batch}"
            # FIXME: this a dirty, DIRTY HACK
            first_event = batch[0]
            day = str(first_event.date())

            if day == "2026-09-09":
                dispensed = 68
                discarded = 107
                subsequently_dispensed = 73
            elif day == "2026-10-07":
                dispensed = 80
                discarded = 114
                subsequently_dispensed = 30
            else:
                raise Exception(f"{batch}")

            # FIXME: third event is a refill
            self.dispensing -= dispensed + subsequently_dispensed
            assert batch[-1].event_type() == "stock_update" and batch[-1].event[2] == "refill"
            dispensing = sum(batch[-1].event[1].values())
            self.dispensing = Dispensing(dispensing, batch[-1])

            consumed = existing_contents + dispensed - discarded
            updated_contents = subsequently_dispensed
            explanation = f"(HACK) manual patch for existing_contents={existing_contents} + dispensed={dispensed} - discarded={discarded}, with contents updated to subsequently_dispensed={subsequently_dispensed}"
            new_measurement = EventBlock.from_events(
                batch,
                ("MIXED_MANUAL", consumed, updated_contents, explanation),
                f"#^{first_event.block_id}",
                f"bowl-{first_event.block_id.split('-')[1]}",
                first_event.event_time
            )

        assert updated_contents >= 0
        self.latest = (updated_contents, new_measurement)
        return new_measurement


class StockUpdate(NamedTuple):
    event_type: str

    new_stock: dict[str, int]
    update_type: str

    dispensed_container: None | str | dict
    dispensed_value: int

    def __post_init__(self):
        assert self.event_type == "stock_update"

    def total_remaining_stock(self) -> int:
        return sum(self.new_stock.values())


class StockManager:
    def __init__(self, empties: dict[str, int]):
        self.empties = empties
        self.latest_stock: LatestStock | None = None
        self.dispensing: Dispensing | None = None

    def integrate_refills(self, event: Event, raw_refills: dict[str, int]) -> Event:
        new_stock = raw_refill_to_stock(self.empties, raw_refills)
        assert all(refilled > 0 for container, refilled in new_stock.items()), \
            (f"refills must be greater than the empty value for a container, "
             f"but empties was {self.empties} with {raw_refills} and calculated stock {new_stock}")

        stock_change: Event
        ref, block_id = self._ref_and_block_id(event)

        if self.latest_stock is not None:
            prior_stock, _, last_container_measurement = self.latest_stock

            remaining = [(container, value) for container, value in prior_stock.items() if value != 0]

            if len(remaining) == 1:
                implicitly_dispensed = remaining[0][1]
                # logging.warning(f"implicitly_dispensed {implicitly_dispensed} from {remaining[0]}")
                self.dispensing -= implicitly_dispensed
                container = remaining[0][0]

                if last_container_measurement is None or last_container_measurement.container != container:
                    logging.warning(f"Implicit dispense detected, but was None or its container did not match implicitly dispensing container {container}: {last_container_measurement}")

                stock_change = EventBlock.from_events(
                    [last_container_measurement.event, event],
                    StockUpdate("stock_update", new_stock, "refill", container, implicitly_dispensed),
                    ref, block_id,
                    event.event_time
                )
                self.latest_stock = LatestStock(new_stock, event, None)
            else:
                raise ValueError(
                    f"[{event.date()}] refill failed because there was more than one undispensed kibble container: {remaining},\n"
                    f"  ignoring measured batch refill {raw_refills},\n"
                    f"  not updating to {new_stock}")
        else:
            stock_change = EventLine(
                event.event_time,
                StockUpdate("stock_update", new_stock, "refill", None, 0),
                ref, block_id
            )

            self.latest_stock = LatestStock(new_stock, event, None)

        self.dispensing = Dispensing(sum(new_stock.values()), event)

        return stock_change

    def integrate_finished(self, event: Event, container: str) -> EventBlock:
        assert self.latest_stock is not None, f"can't finished {container} because latest_stock was None"

        contents, refill_event, latest_container_measurement = self.latest_stock

        dispensed = contents[container]

        ref, block_id = self._ref_and_block_id(event)

        updated = self.latest_stock.after_dispensing(event, container, dispensed)
        self.dispensing -= dispensed

        stock_change_event = EventBlock.from_events(
            [
                latest_container_measurement.event if latest_container_measurement is not None else refill_event,
                event],
            StockUpdate("stock_update", updated.contents, "dispensed", container, dispensed),
            ref, block_id,
            event.event_time
        )

        self.latest_stock = updated

        return stock_change_event

    def integrate_measurement(self, event: Event, container: str, measurement: int) -> EventBlock:
        assert self.latest_stock is not None, f"no refill has happened, so could not integrate event: {event.to_markdown()}"

        remaining_stock, _, last_container_measurement = self.latest_stock
        assert container in remaining_stock, f"{container} was not in {remaining_stock}"

        stock_change: EventBlock
        ref, block_id = self._ref_and_block_id(event)

        if last_container_measurement is None or (last_container_measurement.container == container):
            # simple case for when we don't have to make an inferred measurement across containers

            assert measurement > self.empties[container], f"[{event.date()}] something went wrong - measured container {container} at {measurement}, but empty value is {self.empties[container]}"
            updated_remaining = measurement - self.empties[container]
            assert updated_remaining >= 0

            assert remaining_stock[container] > updated_remaining, (f"[{event.date()}] the remaining kibble {updated_remaining} in {container} was less than the stock {remaining_stock[container]} ({remaining_stock})\n"
                                                                    f"updated_remaining was {updated_remaining} because measurement={measurement} for container {container} with empty weight {self.empties[container]}\n")
            dispensed = remaining_stock[container] - updated_remaining
            self.dispensing -= dispensed

            updated_stock = self.latest_stock.after_dispensing(event, container, dispensed)
            stock_change = EventBlock.from_events(
                [
                    last_container_measurement.event if last_container_measurement is not None else self.latest_stock.refill_event,
                    event
                ],
                StockUpdate("stock_update", updated_stock.contents, "dispensed", container, dispensed),
                ref, block_id,
                event.event_time
            )

            self.latest_stock = updated_stock
        else:
            # more complicated case - today's container is different from yesterday, implying yesterday's was finished

            implicitly_dispensed = remaining_stock[last_container_measurement.container]

            updated_remaining = measurement - self.empties[container]
            assert updated_remaining >= 0, (f"[{event.date()}] updated_remaining was {updated_remaining}\n"
                                            f"  updated_remaining = measurement - self.empties[container]"
                                            f"  = {measurement} - {self.empties[container]}")
            assert container in remaining_stock, f"`{container}` was not in {remaining_stock}"
            first_dispense_from_refilled_container = remaining_stock[container] - updated_remaining

            assert implicitly_dispensed >= 0
            assert first_dispense_from_refilled_container > 0, (
                f"[{event.date()}] first_dispense_from_refilled_container was non-positive, {first_dispense_from_refilled_container}\n"
                f"  first_dispense_from_refilled_container = self.stock[container] - updated_remaining\n"
                f"  = {remaining_stock[container]} - {updated_remaining}")

            dispensed = implicitly_dispensed + first_dispense_from_refilled_container
            assert dispensed > 0
            self.dispensing -= dispensed

            updated_stock = self.latest_stock.after_dispensing(
                event, container, first_dispense_from_refilled_container, last_container_measurement.container)

            stock_change = EventBlock.from_events(
                [last_container_measurement.event, event],
                StockUpdate("stock_update", updated_stock.contents,
                            "dispensed",
                            container if not implicitly_dispensed else {container: first_dispense_from_refilled_container, f"{last_container_measurement.container}": implicitly_dispensed},
                            dispensed),
                ref, block_id,
                event.event_time
            )

            self.latest_stock = updated_stock

        return stock_change

    @staticmethod
    def _ref_and_block_id(event: Event) -> tuple[str, str]:
        ref = f"#^{event.block_id}"
        block_id = f"stockchange-{event.block_id.split('-')[1]}"
        return ref, block_id


def raw_refill_to_stock(empties, raw_refills) -> dict:
    return {container: measurement - empties[container]
            for container, measurement in raw_refills.items()}
