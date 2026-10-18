"""The timeline's model (P10): the markers on the slider, the parse history strip, the
position text and the go-to box, read off the history (PANES.md P10; Numbo GUI,
`timeline.py`; G-042, G-043). Qt-free.

`TimelineModel(history)` keeps, scanning the history's events once each (`sync`):
  - `markers`, one `Marker` per event worth a mark on the slider: a term tick at every
    `term` event [M86 p. 164], a guess at every `guess` event, green once the user's
    `ok` and red once a `no` has come [M86 pp. 166, 175], a refutation at every
    `refuted` event [M86 p. 168], a slip at every `class-changed` event [M86 pp. 99,
    174] and a blast at every `blast` or `blasted` event [M86 pp. 75, 92];
  - `parses`, one `ParseEntry` per term boundary: the parse of the snapshot in force at
    each `prompt` event (the tap always takes one there, G-003), and at the `ended`
    event, as `show-parse` prints it [M86 p. 174]. That is the time-lapse of
    Hofstadter's Figure 1-1 [FCCA p. 41] and of the mountain chain solved by
    successive bracketing [FCCA pp. 64-65];
  - `position`, the event the views show.

The pure functions place and style them for a strip of a given width (`marker_x`,
`parse_cells`, `marker_style`, `marker_tooltip`), give the position text (`text`),
and read the go-to box (`parse_goto`: `term 5`, `clock 812`, `event 402` or a bare
term number; `goto_index` finds the event in the history).
"""

import dataclasses
from dataclasses import dataclass

__all__ = ["MARKER_KINDS", "Marker", "ParseEntry", "MarkerStyle", "TimelineModel",
           "marker_x", "index_at", "parse_cells", "marker_style", "marker_tooltip",
           "parse_goto", "goto_index"]

# The marks on the slider, in the order they are drawn (later kinds over earlier ones).
MARKER_KINDS = ("term", "guess", "refutation", "slip", "blast")

# A slider's handle keeps the groove this far from the strip's ends.
INSET = 8


@dataclass(frozen=True)
class Marker:
    """One mark on the slider: the `index` of its event, its `kind` (`MARKER_KINDS`),
    the short `label` drawn beside it (a term's number) and the `text` of its tooltip;
    a guess's `verdict` is None until answered, then `ok` or `no` [M86 pp. 164-175]
    (G-042)."""
    index: int
    kind: str
    label: str
    text: str
    verdict: str | None = None


@dataclass(frozen=True)
class ParseEntry:
    """The parse at one term boundary: the `index` of the `prompt` (or `ended`) event,
    the number of `terms` entered by then and the `parse` as `show-parse` prints it
    [M86 p. 174] (G-042)."""
    index: int
    terms: int
    parse: str


@dataclass(frozen=True)
class MarkerStyle:
    """How a marker is drawn: its `color` (an `#rrggbb` string), its `shape` (`tick`,
    `triangle`, `diamond`, `bar`) and its `row` on the strip, 0 at the top (G-043)."""
    color: str
    shape: str
    row: int


def _parse_of(snapshot) -> str:
    return "" if snapshot is None else snapshot.parse


class TimelineModel:
    """The markers and the parses of a history (see the module docstring; G-042)."""

    def __init__(self, history):
        self.history = history
        self.position = -1
        self.clear()

    def clear(self) -> None:
        """Forget the run (a `start` event does it; G-042)."""
        self.markers = []
        self.parses = []
        self.terms = 0
        self._scanned = 0

    @property
    def count(self) -> int:
        """How many events the history has (Numbo GUI, `timeline.py`)."""
        return len(self.history)

    def sync(self) -> int:
        """Scan the history's events not scanned yet and add their markers and parses;
        returns how many were scanned (G-042)."""
        events = self.history.events
        n = len(events)
        start = self._scanned
        if start > n:           # the history was cleared under us
            self.clear()
            start = 0
        for index in range(start, n):
            self._scan(index, events[index])
        self._scanned = n
        return n - start

    def _scan(self, index, event) -> None:
        kind = event.kind
        if kind == "start":
            self.clear()
        elif kind == "term":
            self.terms += 1
            self.markers.append(Marker(index, "term", str(event.n),
                                       f"term {event.n}: {event.term}"))
        elif kind == "guess":
            self.markers.append(Marker(index, "guess", "?",
                                       f"guess: {event.form}  {event.block}"))
        elif kind == "answered":
            for k in range(len(self.markers) - 1, -1, -1):
                marker = self.markers[k]
                if marker.kind == "guess":
                    if marker.verdict is None:
                        verdict = "ok" if event.accepted else "no"
                        self.markers[k] = dataclasses.replace(
                            marker, verdict=verdict, label=verdict,
                            text=f"{marker.text}  answered {event.answer} ({verdict})")
                    break
        elif kind == "refuted":
            self.markers.append(Marker(
                index, "refutation", "!",
                f"refuted: {event.form} predicted {event.predicted}, term {event.n} "
                f"is {event.term}"))
        elif kind == "class-changed":
            self.markers.append(Marker(index, "slip", "~",
                                       f"slip: {event.old} --> {event.new} ({event.reason})"))
        elif kind == "blast":
            self.markers.append(Marker(index, "blast", "*",
                                       f"blast: {len(event.burst)} gloms burst"))
        elif kind == "blasted":
            self.markers.append(Marker(index, "blast", "*",
                                       f"blast ({event.reason}): gnoths {' '.join(event.gnoths)}"))
        elif kind == "prompt":
            self.parses.append(ParseEntry(index, self.terms,
                                          _parse_of(self.history.state_at(index))))
        elif kind == "ended":
            self.parses.append(ParseEntry(index, self.terms,
                                          _parse_of(self.history.state_at(index))))

    def set_position(self, index: int) -> None:
        """The event the views show now (Numbo GUI, `timeline.py`)."""
        self.position = index

    def markers_of(self, kind: str) -> list:
        """The markers of one kind, in order (G-042)."""
        return [m for m in self.markers if m.kind == kind]

    def text(self, index=None) -> str:
        """The position text: the event's number, the count, its kind, the term and the
        clock in force, or `no events` (Numbo GUI, `timeline.py`'s `_text`)."""
        h = self.history
        n = len(h)
        if index is None:
            index = self.position
        if n == 0 or index < 0:
            return "no events"
        index = min(index, n - 1)
        text = f"event {index + 1} of {n}: {h.events[index].kind}"
        term, clock = h.term_at(index), h.clock_at(index)
        if term is not None:
            text += f"   term {term}"
        if clock is not None:
            text += f"   clock {clock}"
        return text


# ---- placing and styling (pure) -----------------------------------------------------------------

def marker_x(index: int, count: int, width: int, inset: int = INSET) -> float:
    """Where event `index` of `count` falls on a strip `width` wide, the slider's groove
    kept `inset` from both ends (G-043)."""
    if count <= 1:
        return float(inset)
    return inset + (width - 2 * inset) * index / (count - 1)


def index_at(x: float, count: int, width: int, inset: int = INSET) -> int:
    """The event nearest to `x` on the strip: the inverse of `marker_x` (G-043)."""
    if count <= 1:
        return 0
    span = max(1, width - 2 * inset)
    index = round((x - inset) * (count - 1) / span)
    return max(0, min(count - 1, index))


def parse_cells(parses, count: int, width: int, inset: int = INSET) -> list:
    """The parse strip's cells: `(x0, x1, entry)` per entry, from its event to the next
    entry's (the last reaches the end of the strip) (G-043) [FCCA pp. 41, 64-65]."""
    cells = []
    for k, entry in enumerate(parses):
        x0 = marker_x(entry.index, count, width, inset)
        if k + 1 < len(parses):
            x1 = marker_x(parses[k + 1].index, count, width, inset)
        else:
            x1 = float(width - inset)
        cells.append((x0, max(x1, x0), entry))
    return cells


def marker_style(marker) -> MarkerStyle:
    """A marker's colour, shape and row: term ticks grey on the top row; a guess a
    diamond, grey while unanswered, green once `ok`, red once `no`; a refutation an
    orange triangle; a slip a purple diamond; a blast a black bar (G-043)."""
    kind = marker.kind
    if kind == "term":
        return MarkerStyle("#6a6a6a", "tick", 0)
    if kind == "guess":
        color = {"ok": "#1a9a3c", "no": "#c62828"}.get(marker.verdict, "#9a9a9a")
        return MarkerStyle(color, "diamond", 1)
    if kind == "refutation":
        return MarkerStyle("#e67e00", "triangle", 1)
    if kind == "slip":
        return MarkerStyle("#7b3fa0", "diamond", 1)
    return MarkerStyle("#1a1a1a", "bar", 1)


def marker_tooltip(marker) -> str:
    """The marker's text with its event's index (G-043)."""
    return f"{marker.text}  (event {marker.index + 1})"


# ---- the go-to box ---------------------------------------------------------------------------------

def parse_goto(text: str) -> tuple | None:
    """Read the go-to box: `term 5`, `clock 812` or `event 402` (PANES.md P10), a bare
    number meaning a term; `(kind, n)`, or None when it does not read (G-043)."""
    words = text.strip().lower().split()
    if len(words) == 1 and words[0].isdigit():
        return ("term", int(words[0]))
    if len(words) == 2 and words[0] in ("term", "clock", "event") and words[1].isdigit():
        return (words[0], int(words[1]))
    return None


def goto_index(history, kind: str, n: int) -> int | None:
    """The event index of term `n`'s `term` event, of clock `n`'s `chosen` event, or of
    event number `n` (counted from 1 as the position text counts); None when there is
    none (G-043)."""
    if kind == "term":
        return history.term_index(n)
    if kind == "clock":
        return history.clock_index(n)
    if kind == "event":
        return n - 1 if 1 <= n <= len(history) else None
    return None
