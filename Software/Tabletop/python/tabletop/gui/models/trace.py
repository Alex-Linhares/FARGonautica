"""The text trace of a run as the CLI prints it, one line tagged with the events it
describes (PANES.md P10; Numbo GUI, `log_view.py`; G-042). Qt-free.

`TraceModel(history)` feeds the history's events, as they come (`sync`), to two of the
engine's own `observe.TextTrace`s printing nowhere, the normal and the `--quiet` form, so
the lines are the CLI's byte for byte (`python3 -m tabletop P --seed N`, and `--quiet`;
D-065, `docs/trace-format.md`). The engine prints a codelet's line only when the next
codelet is chosen (or the run ends), so every line is a `TraceLine` with two indexes:
`start`, the first event of the block it describes (the `CodeletChosen` of a codelet line
and of the notes indented under it; 0 for the first line and the start's notes; the
`RunEnded` for the report line), and `end`, the event at which the CLI printed it. The
pane greys the lines whose `start` is after the position shown and tints the block the
position falls in; a click seeks to a line's `start` (G-042). `text()` is the file
`Save trace` writes: the lines of the form chosen, each ending in a newline, as `print`
wrote them.
"""

import bisect
from dataclasses import dataclass

from tabletop import events as ev
from tabletop.observe import TextTrace

__all__ = ["TraceLine", "TraceModel"]


class _Sink:
    """A stream that drops what is printed: `TextTrace` prints to standard output when
    given no stream, and the model wants only its `lines` (G-042)."""

    def write(self, text) -> None:
        pass

    def flush(self) -> None:
        pass


@dataclass(frozen=True)
class TraceLine:
    """One line of the trace: its `text` as the CLI printed it, the index of the event its
    block `start`s with (where a click seeks) and the index of the event at whose arrival
    the CLI printed it, its `end` (G-042)."""
    text: str
    start: int
    end: int


class TraceModel:
    """The trace lines of a history, in the normal and the quiet form (see the module
    docstring; G-042). `quiet` chooses the form `lines` and `text()` give."""

    def __init__(self, history):
        self.history = history
        self.quiet = False
        self.position = -1
        self.clear()

    def clear(self) -> None:
        """Forget the run (a `RunStarted` does it; G-042)."""
        self._traces = {False: TextTrace(stream=_Sink(), quiet=False),
                        True: TextTrace(stream=_Sink(), quiet=True)}
        self._lines = {False: [], True: []}
        self._starts = {False: [], True: []}    # the lines' starts, nondecreasing
        self._block_start = 0
        self._scanned = 0
        self._first = None              # the first event fed: another one is a new run

    @property
    def lines(self) -> list:
        """The `TraceLine`s of the form chosen, in the order printed (G-042)."""
        return self._lines[self.quiet]

    @property
    def count(self) -> int:
        """How many lines the form chosen has (G-042)."""
        return len(self._lines[self.quiet])

    def sync(self) -> int:
        """Feed the history's events not fed yet; returns how many (Seek-Whence GUI,
        `models/timeline.py`'s `sync`; G-042)."""
        events = self.history.events
        n = len(events)
        start = self._scanned
        if start > n or (n and start and events[0] is not self._first):
            self.clear()                # the history was replaced under us
            start = 0
        for index in range(start, n):
            self.feed(index, events[index])
        self._scanned = n
        return n - start

    def feed(self, index: int, event) -> None:
        """Give one event, at its index, to both traces and tag the lines it made the CLI
        print (G-042): all with the block's start, but the report line, which starts at
        the `RunEnded` itself."""
        if index == 0:
            if self._scanned or self._lines[False]:
                self.clear()
            self._first = event
        for quiet, trace in self._traces.items():
            before = len(trace.lines)
            trace.on_event(event)
            new = trace.lines[before:]
            lines = self._lines[quiet]
            for k, text in enumerate(new):
                report = isinstance(event, ev.RunEnded) and k == len(new) - 1
                start = index if report else self._block_start
                lines.append(TraceLine(text, start, index))
                self._starts[quiet].append(start)
        if isinstance(event, ev.CodeletChosen):
            self._block_start = index

    def set_position(self, index: int) -> None:
        """The event the views show now (Numbo GUI, `log_view.py`)."""
        self.position = index

    def set_quiet(self, quiet: bool) -> None:
        """Show the `--quiet` form (the first and last lines) or the normal one (G-042)."""
        self.quiet = bool(quiet)

    def text(self) -> str:
        """The trace as the CLI's standard output: every line of the form chosen followed
        by a newline, and nothing when there is no line (D-065; G-042)."""
        return "".join(line.text + "\n" for line in self.lines)

    def line_of(self, position: int) -> int | None:
        """The row of the last line whose block starts at or before the position, or None
        before the first (G-042)."""
        row = bisect.bisect_right(self._starts[self.quiet], position) - 1
        return row if row >= 0 else None

    def block_rows(self, position: int) -> tuple:
        """The rows of the lines of the block the position falls in: those sharing the
        start of `line_of(position)`'s line; empty before the first (G-042)."""
        row = self.line_of(position)
        if row is None:
            return ()
        starts = self._starts[self.quiet]
        start = starts[row]
        first = bisect.bisect_left(starts, start)
        last = bisect.bisect_right(starts, start) - 1
        return tuple(range(first, last + 1))

    def is_future(self, row: int, position: int) -> bool:
        """Whether the line's block starts after the position (greyed; G-042)."""
        return self.lines[row].start > position
