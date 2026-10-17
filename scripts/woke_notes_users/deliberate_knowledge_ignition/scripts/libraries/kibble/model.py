import datetime
import itertools
from ast import literal_eval
from copy import copy
from dataclasses import dataclass
from typing import Callable, NamedTuple

from libraries.markdown import TimestampedMarkdownBullet, TimestampedMarkdownBulletWithoutBlockId


# FIXME: SupportsRichComparison?
class Event:
    event_time: datetime.datetime
    event: tuple
    ref: str
    block_id: str

    def event_type(self):
        return self.event[0]

    def to_markdown(self) -> str:
        raise NotImplementedError()

    def date(self):
        return self.event_time.date()

    def copy_refing_this(self, ref_f: Callable[[str], str]) -> "EventLine":
        try:
            ref = ref_f(self.block_id)
        except Exception as e:
            raise RuntimeError(f"transformation of {self.block_id} failed") from e
        return EventLine(self.event_time, self.event, f"#^{self.block_id}", ref)


@dataclass(frozen=True, eq=True, order=True)
class EventLine(Event):
    event_time: datetime.datetime
    event: tuple
    ref: str
    block_id: str

    @staticmethod
    def from_markdown(string: str) -> "EventLine":
        bullet = TimestampedMarkdownBullet.from_markdown(string)
        return EventLine(bullet.time, literal_eval(bullet.contents), bullet.ref, bullet.block_id)

    def to_markdown(self) -> str:
        return TimestampedMarkdownBullet(
            self.event_time, str(self.event), self.ref, self.block_id
        ).to_markdown()


@dataclass(frozen=True, eq=True, order=True)
class EventBlock(Event):
    event_time: datetime.datetime
    event: tuple
    ref: str
    block_id: str
    composed_of: list[TimestampedMarkdownBulletWithoutBlockId]

    @staticmethod
    def from_events(sub_events: list[Event], event: tuple, ref: str, block_id: str, event_time: datetime.datetime | None = None) -> "EventBlock":
        event_time: datetime.datetime = event_time or min(subevent.event_time for subevent in sub_events)

        composed_of: list[TimestampedMarkdownBulletWithoutBlockId] = [
            TimestampedMarkdownBulletWithoutBlockId(subevent.event_time, str(subevent.event), subevent.ref)
            for subevent in sub_events
        ]

        return EventBlock(event_time, event, ref, block_id, composed_of)

    def to_markdown(self) -> str:
        lines = itertools.chain(
            [TimestampedMarkdownBullet(self.event_time, str(
                tuple(  # FIXME: retain this support for NamedTuples?
                    self.event
                )
            ), self.ref, self.block_id).to_markdown()],
            ("    " + sub.to_markdown() for sub in self.composed_of)
        )

        return "\n".join(lines)

    def date(self): return self.event_time.date()


# FIXME: named tuple?
@dataclass(frozen=True)
class LastContainerMeasurement:
    event: Event
    container: str  # fixme make a method?


class LatestStock(NamedTuple):
    contents: dict[str, int]
    refill_event: Event
    last_container_measurement: LastContainerMeasurement | None

    def after_dispensing(self, event: Event, container: str, dispensed: int, also_finished: str | None = None) -> "LatestStock":
        contents, refill_event, _ = self
        updated_contents = copy(contents)
        assert updated_contents[container] >= dispensed, f"can't dispensed to <0, but {container} was {updated_contents[container]} and dispensed was {dispensed}"
        updated_contents[container] -= dispensed
        if also_finished:
            updated_contents[also_finished] = 0
        return LatestStock(updated_contents, refill_event, LastContainerMeasurement(event, container))


@dataclass(frozen=True)
class Dispensing:
    value: int
    event: Event

    def __sub__(self, other):
        if not isinstance(other, int):
            raise ValueError(f"Cannot subtract {type(other)} {other}")

        updated_value = self.value - other
        if updated_value < 0:
            # logging.warning(f"  CANARY self.dispensing lowered from {self.value} to {updated_value}")
            raise ValueError(f"self.dispensing lowered from {self.value} to {updated_value} (CANNOT GO NEGATIVE)")

        return Dispensing(updated_value, self.event)

