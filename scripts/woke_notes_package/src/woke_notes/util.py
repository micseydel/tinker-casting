from datetime import datetime, timedelta, timezone, time, tzinfo, date
import re


# noinspection PyMethodMayBeStatic
class Clock:
    def now(self, tz: tzinfo | None = None) -> datetime:
        return datetime.now(tz)

    def today(self) -> date:
        return date.today()


class TimeUtil:
    def __init__(self, clock: Clock):
        self.clock = clock

    def seconds_until(self, later: datetime) -> float | int:
        # if later.tzinfo is None:
        #     later = later.replace(tzinfo=timezone.utc)
        now = self.clock.now(timezone.utc)
        return (later - now).total_seconds()

    # generated with claude-opus-4-6 on 2026-07-13 to avoid pytz (not rigorously tested)
    def next_occurrence(self, time_str: str, now: datetime = None) -> datetime:
        """
        Given a time string like "07:35:00-07:00", return the next datetime
        at which that time occurs relative to 'now'.

        The time string format is "HH:MM:SS±HH:MM" where the suffix is a
        UTC offset (timezone offset).

        Parameters:
            time_str: A string like "07:35:00-07:00" representing a time of day
                      with a UTC offset.
            now: A datetime representing "now". If None, uses datetime.now(timezone.utc).

        Returns:
            A timezone-aware datetime representing the next occurrence of the
            given time.
        """
        if now is None:
            now = self.clock.now(timezone.utc)

        # If 'now' is naive, assume UTC
        if now.tzinfo is None:
            now = now.replace(tzinfo=timezone.utc)

        # Parse the time string
        # Expected format: HH:MM:SS±HH:MM or HH:MM:SS (no offset)
        pattern = r'^(\d{2}):(\d{2}):(\d{2})([+-]\d{2}:\d{2})?$'
        match = re.match(pattern, time_str)
        if not match:
            raise ValueError(f"Invalid time string format: '{time_str}'. "
                             f"Expected format: HH:MM:SS or HH:MM:SS±HH:MM")

        hour = int(match.group(1))
        minute = int(match.group(2))
        second = int(match.group(3))
        offset_str = match.group(4)

        # Parse the UTC offset
        if offset_str:
            sign = 1 if offset_str[0] == '+' else -1
            off_parts = offset_str[1:].split(':')
            offset_hours = int(off_parts[0])
            offset_minutes = int(off_parts[1])
            utc_offset = timedelta(hours=sign * offset_hours,
                                   minutes=sign * offset_minutes)
            tz = timezone(utc_offset)
        else:
            # No offset provided; assume UTC
            tz = timezone.utc

        # Build a target time in the target timezone
        target_time = time(hour, minute, second, tzinfo=tz)

        # Convert 'now' into the target timezone so we can compare dates
        now_in_tz = now.astimezone(tz)

        # Build candidate datetime for today in the target timezone
        candidate = datetime.combine(now_in_tz.date(), target_time)

        # If candidate is in the past or exactly now, move to tomorrow
        if candidate <= now_in_tz:
            candidate += timedelta(days=1)

        return candidate


# FIXME: remove
if __name__ == '__main__':
    test_clock = TimeUtil(Clock())

    # Example usage
    now = datetime.now(timezone.utc)
    print(f"Now (UTC): {now.isoformat()}")

    for time_str in [
        "07:35:00-07:00", "23:00:00+05:30", "12:00:00", "07:35:00-08:00"
    ]:
        result = test_clock.next_occurrence(time_str, now=now)
        print(f"Next occurrence of '{time_str}': {result.isoformat()}")
        print(f"  (in UTC: {result.astimezone(timezone.utc).isoformat()})")
        print(f"  {result - now}")
