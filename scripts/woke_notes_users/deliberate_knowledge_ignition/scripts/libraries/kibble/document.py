import datetime
import json
import logging
from ast import literal_eval
from collections import defaultdict

from libraries.kibble.kibble import extract, process_events, EventLine
from libraries.kibble.model import Event
from libraries.markdown import TimestampedMarkdownBullet, TimestampedMarkdownBulletWithoutBlockId, \
    lines_by_toplevel_sections

NEWLINE = "\n"


# https://charts.phib.ro/Meta/Charts/Basics
# FIXME: chart object with to_code_block? then I could stack them more easily...
def simple_chart(labels: list[str], title: str, data: list[int],
                 include_seven_day_average=False, include_average=False, include_zero=False):
    series = [{"title": title, "data": data}]
    chart = {
        "type": "line",
        "labels": labels,
        "series": series,
    }

    if include_average:
        average = sum(data) / len(data)
        series.append({
            "title": "average",
            "data": [average] * len(data)
        })

    if include_seven_day_average:
        running = []
        running_average = []
        series.append({
            "title": "7-day running average",
            "data": running_average
        })
        for item in data:
            running.append(item)
            running = running[-7:]
            running_average.append(sum(running) / len(running))

    if include_zero:
        series.append({
            "title": "zero",
            "data": [0] * len(data)
        })

    return f"```chart\n{json.dumps(chart)}\n```\n"


def discarded_chart(discarded_by_day: dict[datetime.date, int]) -> str:
    labels = []
    data = []
    for day, discarded in sorted(discarded_by_day.items()):
        labels.append(str(day))
        data.append(discarded)

    return "From [[#Extracted Events]] discards\n\n" + simple_chart(labels, "Discarded", data, include_average=True)


def dispensed_chart(dispensed_by_day: dict[datetime.date, int]) -> str:
    labels = []
    data = []
    for day, dispensed in sorted(dispensed_by_day.items()):
        labels.append(str(day))
        data.append(dispensed)

    return "From [[#Stock and raw bowl events]] dispensed events\n\n" + simple_chart(
        labels, "Dispensed", data, include_seven_day_average=True, include_average=True)


def consumed_high_level_chart(dispensed_by_day: dict[datetime.date, int], discarded_by_day: dict[datetime.date, int]) -> str:
    labels = []
    data = []

    from_day = min(min(dispensed_by_day.keys()), min(discarded_by_day.keys()))
    to_day = max(max(dispensed_by_day.keys()), max(discarded_by_day.keys()))

    day = from_day
    while day <= to_day:
        labels.append(str(day))

        dispensed = dispensed_by_day[day]
        discarded = discarded_by_day[day]
        consumed = dispensed - discarded
        data.append(consumed)

        day += datetime.timedelta(days=1)

    average = sum(data) / len(data)

    last_21_days = data[-21:]
    average_last_21_days = sum(last_21_days) / len(last_21_days)
    last_7_days = data[-7:]
    average_last_7_days = sum(last_7_days) / len(last_7_days)

    return (f"- From [[#Stock and raw bowl events]] dispensed events and [[#Extracted Events]] discards\n"
            f"- average={average:.1f}\n"
            f"- average_last_21_days={average_last_21_days:.1f}\n"
            f"- average_last_7_days={average_last_7_days:.1f}\n\n") + simple_chart(
        labels, "Consumed (dispensed-discarded)", data,
        include_seven_day_average=True, include_average=True, include_zero=True
    )


def consumed_via_bowl_events_chart(events: list[tuple[Event, int]]) -> str:
    labels = []
    data = []
    for event, value in sorted(events, key=lambda e: e[0].event_time):
        labels.append(str(event.event_time))
        data.append(value)

    return simple_chart(labels, "Consumed (via bowl events)", data,
                        include_average=True, include_zero=True)


def bowl_running_total_chart(bowl_running_total: list[tuple[datetime.datetime, int]]) -> str:
    labels = []
    data = []
    for time, value in sorted(bowl_running_total):
        labels.append(str(time))
        data.append(value)

    return simple_chart(labels, "Consumed (via bowl events)", data)


def stock_by_day_chart(stock_by_day):
    labels = []
    data = []
    for day, value in sorted(stock_by_day.items()):
        labels.append(str(day))
        data.append(value)

    return "From [[#Stock and raw bowl events]] stock_updated events\n\n" + simple_chart(labels, "Stock by day", data, include_average=True)


def consumed_experimental_chart(dispensed_by_day: dict[datetime.date, int],
                                discarded_by_day: dict[datetime.date, int]) -> str:
    labels = []
    # data = []
    # FIXME: carry discards forward on days with no dispense?
    from_day = min(min(dispensed_by_day.keys()), min(discarded_by_day.keys()))
    to_day = max(max(dispensed_by_day.keys()), max(discarded_by_day.keys()))

    day = from_day
    dispensed_data = []
    discarded_data = []
    consumed_data = []

    experimental_data = []
    punted_discard = []
    while day <= to_day:
        labels.append(str(day))

        dispensed = dispensed_by_day[day]
        discarded = discarded_by_day[day]

        consumed = dispensed - discarded

        dispensed_data.append(dispensed)
        discarded_data.append(discarded)
        consumed_data.append(consumed)

        if consumed > 0:
            experimental_data.append(consumed)
        else:
            experimental_data(dispensed)

        day += datetime.timedelta(days=1)

    series = [
        # {"title": "dispensed_data", "data": dispensed_data},
        # {"title": "discarded_data", "data": discarded_data},
        # {"title": "consumed_data", "data": consumed_data},
        {"title": "experimental_data", "data": experimental_data},
    ]
    chart = {
        "type": "line",
        "labels": labels,
        "series": series,
    }

    return f"```chart\n{json.dumps(chart)}\n```\n"


class KibbleDocument:
    history: list[TimestampedMarkdownBullet]
    extracted_events: list[EventLine]
    inbox: list[TimestampedMarkdownBulletWithoutBlockId]

    empties: dict[str, int]

    def __init__(self, note: tuple):
        frontmatter, markdown = note

        self.empties = frontmatter["empties"]

        by_section = lines_by_toplevel_sections(markdown)

        self.history = [TimestampedMarkdownBullet.from_markdown(line)
                        for line in by_section["Raw History"] if line.strip() and not line.startswith("> ")]

        self.extracted_events = [EventLine.from_markdown(line)
                                 for line in by_section["Extracted Events"] if line.strip() and not line.startswith("> ")]

        raw_inbox = by_section.get("Inbox", [])
        self.inbox = [TimestampedMarkdownBulletWithoutBlockId.from_markdown(line)
                      for line in raw_inbox if line.strip() and not line.startswith("> ")]

        self.sort()

    def sort(self) -> None:
        self.history.sort(key=lambda b: b.time)
        self.extracted_events.sort(key=lambda e: e.event_time, reverse=True)
        self.inbox.sort(key=lambda e: e.time)

    def integrate_transcription(self, capture_datetime: datetime.datetime, lowered_text: str, note_id: str):
        block_id_int = str(int(capture_datetime.timestamp()))
        block_id = f"extracted-{block_id_int}"
        ref = f"#^{block_id_int}"

        history_bullet = TimestampedMarkdownBullet(capture_datetime, lowered_text, note_id, block_id_int)
        if history_bullet in self.history:
            logging.warning(f"ignoring duplicate for [[{note_id}]]")
            return

        self.history.append(history_bullet)
        self.history.sort(key=lambda b: b.time)

        extraction = extract(lowered_text)

        if extraction is None:
            raw_extraction = ("raw", lowered_text)
            logging.info(f"failed to extract from {lowered_text}")
            event = TimestampedMarkdownBulletWithoutBlockId(capture_datetime, str(raw_extraction), ref)
            self.inbox.append(event)
        elif extraction[1] is None:
            event = TimestampedMarkdownBulletWithoutBlockId(capture_datetime, str(extraction), ref)
            self.inbox.append(event)
        else:
            event = EventLine(capture_datetime, extraction, ref, block_id)
            self.extracted_events.append(event)

        assert len(self.extracted_events) == len(set(self.extracted_events))
        assert len(self.inbox) == len(set(self.inbox))

        self.sort()

    def reprocess(self):
        for item in self.inbox:
            self.extracted_events.append(EventLine(item.time, literal_eval(item.contents), item.ref, f"{item.ref[2:]}"))

        self.inbox = []
        self.sort()

    def to_markdown(self) -> str:
        if self.inbox:
            formatted_inbox = ("> [!info] Fix these then hit the [[#^reprocess]] button to integrate them into [[#Extracted Events]]\n\n" +
                               "\n".join(item.to_markdown() for item in self.inbox))
        else:
            formatted_inbox = "> [!abstract] Awesome, no inbox items"

        extracted_events_lines = "\n".join(item.to_markdown() for item in self.extracted_events)
        formatted_history = "\n".join(item.to_markdown() for item in self.history)

        events_batched_refills, stock_and_raw_bowl_changes, bowl_events, audit = \
            process_events(self.empties, list(sorted(self.extracted_events)))

        discarded_by_day: dict[datetime.date, int] = defaultdict(int)
        for event in self.extracted_events:
            if event.event_type() == "discarded":
                discarded = event.event[-1]
                discarded_by_day[event.date()] += discarded

        dispensed_by_day: dict[datetime.date, int] = defaultdict(int)
        stock_by_day: dict[datetime.date, int] = {}
        for event in stock_and_raw_bowl_changes:
            if event.event_type() == "stock_update":
                day = event.date()

                dispensed = event.event[-1]
                dispensed_by_day[day] += dispensed

                remaining = sum(event.event[1].values())
                if day in stock_by_day:
                    continue  # FIXME: do another not by day
                    # raise RuntimeError(f"Duplicate day: {day}, {stock_by_day[day]} and {remaining}")
                stock_by_day[day] = remaining

        negatives = []
        consumed_events_via_bowl = []
        bowl_running_total: list[tuple[datetime.datetime, int]] = []
        for event in bowl_events:
            event_type, value, bowl, explanation = event.event
            bowl_running_total.append((event.event_time, bowl))
            if event_type == "consumed":
                consumed_events_via_bowl.append((event, value))
            elif event_type in ("MIXED_MANUAL", "dispensed"):
                pass
            else:
                raise ValueError(event_type)
            if value < 0:
                negatives.append(event)

        formatted_negatives = ""  # FIXME
        # if negatives:
        #     formatted_negatives = f"\n## Warning\n\n> [!warning] there were {len(negatives)} negatives\n\n" + "\n".join(neg.to_markdown() for neg in negatives) + "\n"

        return f"""- [ ] ^reprocess

# Inbox

{formatted_inbox}

# Charts

## Consumed (high level)

{consumed_high_level_chart(dispensed_by_day, discarded_by_day)}

## Stock by day

{stock_by_day_chart(stock_by_day)}

## Dispensed by day

{dispensed_chart(dispensed_by_day)}

## Discards by day

{discarded_chart(discarded_by_day)}

# Debugging

```json
{json.dumps(audit, indent=4)}
```

# Synthetic Events

> [!info] For auditing - modify [[#Extracted Events]] if needed

## Bowl events

> [!warning] This is buggy and would need to be revisited

{NEWLINE.join(e.to_markdown() for e in reversed(bowl_events))}

## Stock and raw bowl events

{NEWLINE.join(e.to_markdown() for e in reversed(stock_and_raw_bowl_changes))}

## Events (bundled refills)

{NEWLINE.join(e.to_markdown() for e in reversed(events_batched_refills))}

# Extracted Events

> [!info] These are non-batched extracted events, and can be modified

{extracted_events_lines}

# Raw History

> [!info] This facilitates idempotence

{formatted_history}
"""
