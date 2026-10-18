"""The timeline's model (P11): the markers on the slider, the strip of T under it, the
position text and the go-to-codelet lookup, read off the history (PANES.md P11; Numbo
GUI, `timeline.py`, through the Seek-Whence GUI's `models/timeline.py`; G-043). Qt-free.

`TimelineModel(history)` keeps, scanning the history's events once each (`sync`):
  - `markers`, one `Marker` per event PANES.md P11 marks: the first `ObjectNoticed`, each
    `CorrespondenceBuilt`, each `CompetitionHeld`, each `WorldviewChanged`, each
    `ContextPushed` and `ContextPopped` (rare: ANSWERS.md 11), the first
    `AnswerCodeletPosted` and the `AnswerGiven`;
  - `readings`, the temperature readings of the run through a composed
    `TemperatureModel` (the plot's series, G-028), for the thin strip of T;
  - `position`, the event the views show.
The pure functions place and style them for a strip of a given width (`marker_x`,
`index_at`, `marker_style`, `marker_tooltip`, `strip_points`) and give the position text
(`text`); `codelet_index` finds the event of codelet N for the go-to box.
"""

from dataclasses import dataclass

from tabletop import events as ev
from tabletop.gui.models.temperature import TemperatureModel

__all__ = ["MARKER_KINDS", "INSET", "Marker", "MarkerStyle", "TimelineModel", "marker_x",
           "index_at", "marker_style", "marker_tooltip", "strip_points", "codelet_index"]

# The marks on the slider, in the order they are drawn (later kinds over earlier ones).
MARKER_KINDS = ("noticed", "answer-posted", "push", "pop", "role", "correspondence", "change",
                "competition", "answer")
# A slider's handle keeps the groove this far from the strip's ends.
INSET = 8


@dataclass(frozen=True)
class Marker:
    """One mark on the slider: the `index` of its event, its `kind` (`MARKER_KINDS`), the
    short `label` drawn beside a tick and the `text` of its tooltip (G-043)."""
    index: int
    kind: str
    label: str
    text: str


@dataclass(frozen=True)
class MarkerStyle:
    """How a marker is drawn: its `color` (an `#rrggbb` string), its `shape` (`tick`,
    `diamond`, `triangle`, `triangle-down`, `bar`, `dot`) and its `row` on the strip, 0
    at the top (G-043, G-062)."""
    color: str
    shape: str
    row: int


class TimelineModel:
    """The markers, the readings and the position of a history (see the module
    docstring; G-043)."""

    def __init__(self, history):
        self.history = history
        self.position = -1
        self.clear()

    def clear(self) -> None:
        """Forget the run (a `RunStarted` does it; G-043)."""
        self.markers = []
        self.temperature = TemperatureModel()
        self._noticed = False
        self._answer_posted = False
        self._scanned = 0
        self._first = None

    @property
    def count(self) -> int:
        """How many events the history has (Numbo GUI, `timeline.py`)."""
        return len(self.history)

    @property
    def readings(self) -> list:
        """The temperature readings so far, each with its event `index`, `temperature`
        and `held` (G-028, G-043)."""
        return self.temperature.readings

    def sync(self) -> int:
        """Scan the history's events not scanned yet; returns how many (Seek-Whence GUI,
        `models/timeline.py`; G-043)."""
        events = self.history.events
        n = len(events)
        start = self._scanned
        if start > n or (n and start and events[0] is not self._first):
            self.clear()                # the history was replaced under us
            start = 0
        for index in range(start, n):
            self._scan(index, events[index])
        self._scanned = n
        return n - start

    def _scan(self, index, event) -> None:
        if index == 0:
            if self._scanned or self.markers or self.readings:
                self.clear()
            self._first = event
        self.temperature.on_event(event)
        marker = self._marker_of(index, event)
        if marker is not None:
            self.markers.append(marker)

    def _marker_of(self, index, event):
        """The marker of an event, or None (PANES.md P11; G-043)."""
        if isinstance(event, ev.ObjectNoticed):
            if self._noticed:
                return None
            self._noticed = True
            return Marker(index, "noticed", "n",
                          f"first object noticed: {event.object} in the {event.region} region")
        if isinstance(event, ev.CorrespondenceBuilt):
            return Marker(index, "correspondence", "c",
                          f"{event.correspondence} built: {event.henry}={event.eliza}")
        if isinstance(event, ev.CompetitionHeld):
            if not event.rivals:            # an uncontested entry (D-040)
                return Marker(index, "competition", "!",
                              f"{event.newcomer} entered the Worldview without a fight "
                              f"at codelet {event.clock}")
            winner = "the Rival Worldview won" if event.rival_wins else "the Old Worldview held"
            return Marker(index, "competition", "!",
                          f"competition at codelet {event.clock}: {event.newcomer} against "
                          f"{', '.join(event.rivals)}: {winner}")
        if isinstance(event, ev.WorldviewChanged):
            return Marker(index, "change", "W",
                          f"Worldview changed ({event.reason}): added {' '.join(event.added) or '-'}, "
                          f"removed {' '.join(event.removed) or '-'}, rating {event.rating:.0f}")
        if isinstance(event, ev.ContextPushed):
            return Marker(index, "push", "^", f"context push: {event.correspondence}")
        if isinstance(event, ev.ContextPopped):
            return Marker(index, "pop", "v", f"context pop: {event.correspondence}")
        if isinstance(event, ev.RoleDiscarded):
            # A role change (D1 of the roles loop's answers; D-079): a tag lost to a rival,
            # broken by the checker or gone with its structure (G-062).
            return Marker(index, "role", "r",
                          f"role change at codelet {event.clock}: {event.item} is no longer "
                          f"{event.role} ({event.tag} {event.reason}, by {event.by})")
        if isinstance(event, ev.RoleRecast):
            tiers = ", ".join(f"{c} in tier {t}" for t, c, _w in event.tiers[:3])
            return Marker(index, "role", "R",
                          f"re-cast of {event.touched} at codelet {event.clock}: "
                          f"[{' '.join(event.signature)}] {tiers}"
                          + (" (tie)" if event.ambiguous else ""))
        if isinstance(event, ev.AnswerCodeletPosted):
            if self._answer_posted:
                return None
            self._answer_posted = True
            return Marker(index, "answer-posted", "a",
                          f"first find-answer codelet posted at urgency {event.urgency:.1f}")
        if isinstance(event, ev.AnswerGiven):
            tag = f"({event.label})" if event.label else "(none)"
            return Marker(index, "answer", "E", f"answer: Eliza touches {event.answer} {tag}")
        return None

    def set_position(self, index: int) -> None:
        """The event the views show now (Numbo GUI, `timeline.py`)."""
        self.position = index

    def markers_of(self, kind: str) -> list:
        """The markers of one kind, in order (G-043)."""
        return [m for m in self.markers if m.kind == kind]

    def text(self, index=None) -> str:
        """The position text: the event's number, the count, its name and the codelet
        under way, or `no events` (Numbo GUI, `timeline.py`'s `_text`)."""
        h = self.history
        n = len(h)
        if index is None:
            index = self.position
        if n == 0 or index < 0:
            return "no events"
        index = min(index, n - 1)
        text = f"event {index + 1} of {n}: {h.events[index].name}"
        clock = h.codelet_at(index)
        return text + (f"   codelet {clock}" if clock else "   start")


# ---- placing and styling (pure) -------------------------------------------------------------


def marker_x(index: int, count: int, width: int, inset: int = INSET) -> float:
    """Where event `index` of `count` falls on a strip `width` wide, the slider's groove
    kept `inset` from both ends (Seek-Whence GUI, `models/timeline.py`; G-043)."""
    if count <= 1:
        return float(inset)
    return inset + (width - 2 * inset) * index / (count - 1)


def index_at(x: float, count: int, width: int, inset: int = INSET) -> int:
    """The event nearest to `x` on the strip: the inverse of `marker_x` (Seek-Whence GUI,
    `models/timeline.py`; G-043)."""
    if count <= 1:
        return 0
    span = max(1, width - 2 * inset)
    index = round((x - inset) * (count - 1) / span)
    return max(0, min(count - 1, index))


def marker_style(marker) -> MarkerStyle:
    """A marker's colour, shape and row (G-043): the first notice a grey tick and the
    first find-answer posting an orange tick on the top row, a push a purple triangle and
    a pop one pointing down, a role change a violet dot (G-062); on the lower row a
    correspondence a blue diamond, a Worldview change a green diamond, a competition a
    magenta bar and the answer a black bar."""
    kind = marker.kind
    if kind == "role":
        return MarkerStyle("#5b2a86", "dot", 0)
    if kind == "noticed":
        return MarkerStyle("#6a6a6a", "tick", 0)
    if kind == "answer-posted":
        return MarkerStyle("#e67e00", "tick", 0)
    if kind == "push":
        return MarkerStyle("#7b3fa0", "triangle", 0)
    if kind == "pop":
        return MarkerStyle("#7b3fa0", "triangle-down", 0)
    if kind == "correspondence":
        return MarkerStyle("#1f5fbf", "diamond", 1)
    if kind == "change":
        return MarkerStyle("#1a9a3c", "diamond", 1)
    if kind == "competition":
        return MarkerStyle("#e6007e", "bar", 1)
    return MarkerStyle("#1a1a1a", "bar", 1)


def marker_tooltip(marker) -> str:
    """The marker's text with its event's number (G-043)."""
    return f"{marker.text}  (event {marker.index + 1})"


def strip_points(readings, count: int, width: int, height: int, inset: int = INSET) -> list:
    """The thin strip of T under the slider (PANES.md P11; G-028, G-043): one `(x, y,
    held)` per reading, x by its event as `marker_x` places it, y from `height - 1` at
    T 0 to 0 at T 100."""
    points = []
    for reading in readings:
        x = marker_x(reading.index, count, width, inset)
        t = max(0.0, min(100.0, reading.temperature))
        y = (height - 1) * (1 - t / 100.0)
        points.append((x, y, reading.held))
    return points


def codelet_index(history, n: int) -> int | None:
    """The event index of codelet `n`'s `CodeletChosen`, or None (PANES.md P11: go to
    codelet N; Numbo GUI's `jump_to_iteration`; G-043)."""
    return history.codelet_index(n)
