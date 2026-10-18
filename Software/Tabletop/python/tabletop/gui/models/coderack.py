"""The Coderack's model (P4 and P4b; G-036 to G-038), Qt-free: the rack as the events
tell it, the strip's bars in French's order, the codelet just chosen, the removals, the
list's rows and the catalogue legend.

French's screen shows "a representation of the Coderack, indicating the percentage of
particular codelet types waiting to run. The total number of codelets run so far is also
indicated." [F95 p. 97]: a long box under the table with one bar per type at its label,
the percent printed above the bar and "Number of codelets run: N" over the box's right
end [F95 pp. 98-107 (img)]. The rack is rebuilt from the events alone (G-036): a
`CodeletPosted` hangs a codelet at the end (its `clock` is the codelet's `posted_at`,
D-053); a `CodeletChosen` gives every codelet the chance it had at the draw, recomputed
from the event's exponent `a` with the engine's own weights (D-060; a pure function of
the urgencies, no draw) and takes the chosen one off; a `CodeletRemoved` takes off the
codelet with that type, arguments and `posted_at` (D-053); the strip's shares are the
counts by type in the strip's order (`config.CODELET_TYPES`), as `Coderack.shares` gives
them. So the model is exact at every event index, checked against every snapshot of the
golden runs, and seeking rebuilds it. The geometry of the strip is a pure function of
the model and the size (`strip_layout`; G-037); the list's rows, the chosen band and
the removals greyed for one event are the rack list's (P4b, GUI design after the Numbo
GUI's `coderack_view.py`; G-038); the legend is `codelets.CATALOGUE` (D-051).
"""

from dataclasses import dataclass, replace

from tabletop import events as ev
from tabletop.codelets import CATALOGUE
from tabletop.config import CODELET_TYPES, codelet_label
from tabletop.gui.models.temperature import NOT_FRENCHS
from tabletop.temperature import softened_weights

__all__ = ["Bar", "COLUMNS", "Choice", "CoderackModel", "NOT_FRENCHS", "Removal", "Row",
           "STRIP_ORDER", "StripLayout", "Waiting", "band_lines", "bar_at", "chances_of",
           "codelet_text", "hover_text", "legend_lines", "legend_text", "row_text",
           "row_tooltip", "strip_layout"]

# The types in the strip's left-to-right order [F95 p. 97] [F95 p. 107 (img)] (D-051), the
# legend's 23 and then the role codelets (D-081).
STRIP_ORDER = tuple(t for t, _ in CODELET_TYPES)
ENTRY = {row.name: row for row in CATALOGUE}
# The strip's proportions (G-037): the margin, the height of the label lines under the
# box (two lines, the odd labels a line lower, so that 23 labels fit), the bar's share of
# its slot, the number's line above the bar.
MARGIN, LABEL_HEIGHT, BAR_SHARE, NUMBER_HEIGHT = 4.0, 20.0, 0.6, 10.0
# The list's columns (G-038): the heading and the relative width of each.
COLUMNS = (("#", 3), ("codelet", 16), ("raw", 4), ("urgency", 6), ("gen", 3), ("posted", 5),
           ("age", 4), ("chance", 6), ("by", 9))


@dataclass(frozen=True)
class Waiting:
    """One codelet on the rack as the events say it (G-036): the form "(codelet-name
    codelet-age codelet-urgency)" [F95 p. 77] with its generation tag [F95 p. 81]:
    `type`, `args`, `raw` urgency, `generation`, adjusted `urgency` (D-052), `posted_at`
    (the `CodeletPosted` event's clock, D-053), `by` (who posted it) and `chance`, the
    probability it had at the last draw (D-060), None before any."""
    type: str
    args: tuple
    raw: float
    generation: int
    urgency: float
    posted_at: int
    by: str
    chance: float | None = None

    @property
    def label(self) -> str:
        """The strip's label [F95 p. 107 (img)] (D-051)."""
        return codelet_label(self.type)

    def age(self, clock: int) -> int:
        """The codelets run since it was hung (D-053) [F95 p. 76]."""
        return clock - self.posted_at


@dataclass(frozen=True)
class Choice:
    """The codelet just chosen (G-038): the `CodeletChosen` event's `index`, `clock`
    (the codelet's ordinal), `type`, `args`, `generation`, `urgency`, `chance`, `a`,
    `temperature` and `rack_size` [F95 p. 77] [F95 p. 92]; the `waiting` entry it was on
    the rack as (None when `alike` codelets matched it, or none did); then what it did, from its
    `CodeletRan` (`action`, `detail`, the types `posted`) and `CodeletFizzled`
    (`fizzled`, `reason`) [F95 p. 73]."""
    index: int
    clock: int
    type: str
    args: tuple
    generation: int
    urgency: float
    chance: float
    a: float
    temperature: float
    rack_size: int
    waiting: object = None
    alike: int = 1
    action: str = ""
    detail: str = ""
    posted: tuple = ()
    fizzled: bool = False
    reason: str = ""


@dataclass(frozen=True)
class Removal:
    """A codelet taken off the rack unrun (D-053) [F95 p. 76] [F95 p. 82]: the
    `CodeletRemoved` event's `index` and `clock`, the `position` it had on the rack, the
    `waiting` entry and the `reason` (`age` or `capacity`) (G-038)."""
    index: int
    clock: int
    position: int
    waiting: Waiting
    reason: str


@dataclass(frozen=True)
class Bar:
    """One slot of the strip (G-037): the `type` and `label`, the `count` waiting and
    the `percent` of the rack (0 for a type with none), the slot's `slot_x` and
    `slot_width`, the bar's `x`, `y`, `width` and `height` (0 when none), the `number`
    printed above the bar (empty when none), and `chosen`, whether the codelet just
    chosen is of this type (the bar is outlined)."""
    type: str
    label: str
    count: int
    percent: float
    slot_x: float
    slot_width: float
    x: float
    y: float
    width: float
    height: float
    number: str
    chosen: bool


@dataclass(frozen=True)
class StripLayout:
    """The strip's geometry for one size (G-037): the `bars` in the strip's order, the
    `box` (x, y, width, height), the `counter` text "Number of codelets run: N" (the pane
    shows it over the box's right end [F95 p. 107 (img)]), the `label_y` of the labels
    under the box, and the `width` and `height` laid out for."""
    bars: tuple
    box: tuple
    counter: str
    label_y: float
    width: float
    height: float

    def bar(self, codelet_type: str):
        """The bar of a type, or None (G-037)."""
        for bar in self.bars:
            if bar.type == codelet_type:
                return bar
        return None


@dataclass(frozen=True)
class Row:
    """One row of the rack list (G-038): the `position` on the rack (0 the oldest), the
    `waiting` entry, its `age` at the clock shown, `removed` (greyed, for the one event
    of its removal) and the `reason` then."""
    position: int
    waiting: Waiting
    age: int
    removed: bool = False
    reason: str = ""


def chances_of(urgencies, a: float) -> list:
    """The chance of each codelet at the exponent a, as `Coderack.chances` computes it
    (D-060) [F95 p. 92 (img)]: u^a over the total, without a draw; equal shares when the
    total is 0; empty for an empty rack (G-036)."""
    weights = softened_weights(list(urgencies), a)
    total = sum(weights)
    if total <= 0:
        return [1.0 / len(weights)] * len(weights) if weights else []
    return [w / total for w in weights]


def codelet_text(codelet_type: str, args) -> str:
    """A codelet as the trace prints it, the label with its arguments [F95 p. 107 (img)]
    (D-051)."""
    return f"{codelet_label(codelet_type)}({' '.join(str(a) for a in args)})"


class CoderackModel:
    """The Coderack as the events tell it (G-036): the `rack` (a list of `Waiting`, the
    oldest first), the `clock` (the codelets run, French's counter [F95 p. 97]), the
    `choice` just made, the `removals` so far, the largest rack this run (`max_size`),
    the `events` seen, `mismatches` (an event the rack could not follow: 0 on the golden
    runs until the roles loop's item 09, when a draw among twins of two generations on
    surround-05 seed 2 put the events-only rack one codelet out for its last nine draws,
    G-036's dated note; a re-based rack follows every run) and `rebased_at`, the index
    of the snapshot the rack was last re-based on (`rebase`), -1 before any. `on_event` feeds it; `counts`, `shares`, `rows`,
    `layout` read it."""

    def __init__(self):
        self.reset()

    def reset(self) -> None:
        """Forget the run (a `RunStarted` begins a new one, D-065)."""
        self.problem = None
        self.rack = []
        self.clock = 0
        self.events = 0
        self.choice = None
        self.removals = []
        self.max_size = 0
        self.mismatches = 0
        self.rebased_at = -1
        self.ended = False

    @property
    def index(self) -> int:
        """The index of the last event taken, -1 before any (G-036)."""
        return self.events - 1

    @property
    def chosen_fresh(self) -> bool:
        """Whether the event shown is the choice itself (the band is tinted then; Numbo
        GUI, `coderack_view.py`; G-038)."""
        return self.choice is not None and self.choice.index == self.index

    @property
    def chosen_type(self):
        """The type of the codelet just chosen, outlined on the strip from its choice to
        the next (G-037), or None before any."""
        return self.choice.type if self.choice is not None else None

    def on_event(self, event) -> None:
        """Take one event: a posting, a choice, what the codelet did, a removal, the start
        or the end (D-065; G-036)."""
        if isinstance(event, ev.RunStarted):
            self.reset()
            self.problem = event.problem
        index = self.events
        self.events += 1
        self.clock = event.clock
        if isinstance(event, ev.CodeletChosen):
            self.choice = Choice(index, event.clock, event.codelet, tuple(event.args),
                                 event.generation, event.urgency, event.chance, event.a,
                                 event.temperature, event.rack_size)
        elif isinstance(event, ev.CodeletRan) and self.choice is not None:
            self.choice = replace(self.choice, action=event.action, detail=event.detail,
                                  posted=tuple(event.posted))
        elif isinstance(event, ev.CodeletFizzled) and self.choice is not None:
            self.choice = replace(self.choice, fizzled=True, reason=event.reason)
        elif isinstance(event, ev.RunEnded):
            self.ended = True
        self.apply(event, index)

    def apply(self, event, index=None) -> None:
        """Change the rack as the event says (G-036): a posting hangs the codelet at the
        end; a choice gives every codelet its chance at the event's `a` (D-060) and takes
        the first codelet with the chosen one's type, arguments, generation and urgency
        off (twins alike in all four, differing only in `posted_at`, are told apart by
        nothing in the event, so the oldest goes: `rebase` puts the rack right at the
        next snapshot); a removal takes off the codelet with the event's type,
        arguments and `posted_at` (D-053), else the oldest of that type and arguments.
        With an `index`, the removal is recorded; without (a re-application after a
        `rebase`) only the rack changes."""
        if isinstance(event, ev.CodeletPosted):
            self.rack.append(Waiting(event.codelet, tuple(event.args), event.raw,
                                     event.generation, event.urgency, event.clock, event.by))
            self.max_size = max(self.max_size, len(self.rack))
        elif isinstance(event, ev.CodeletChosen):
            self._choose(event, index)
        elif isinstance(event, ev.CodeletRemoved):
            self._remove(event, index)

    def _choose(self, event, index) -> None:
        """The draw (see `apply`); `choice.waiting` is the rack's entry when the codelets
        that matched are alike in `posted_at` and `by` too (one, or identical twins), else
        None (G-038)."""
        chances = chances_of((c.urgency for c in self.rack), event.a)
        self.rack = [replace(c, chance=p) for c, p in zip(self.rack, chances)]
        if index is not None and event.rack_size != len(self.rack):
            self.mismatches += 1
        key = (event.codelet, tuple(event.args), event.generation, event.urgency)
        matches = [i for i, c in enumerate(self.rack)
                   if (c.type, c.args, c.generation, c.urgency) == key]
        if not matches:
            if index is not None:
                self.mismatches += 1
            return
        distinct = {(self.rack[i].posted_at, self.rack[i].by) for i in matches}
        waiting = self.rack.pop(matches[0])
        if self.choice is not None and self.choice.type == event.codelet:
            self.choice = replace(self.choice, waiting=waiting if len(distinct) == 1 else None,
                                  alike=len(matches))

    def _remove(self, event, index) -> None:
        """A removal for age or capacity (see `apply`); recorded with the event's
        `posted_at`, which is the engine's word (D-053)."""
        key = (event.codelet, tuple(event.args))
        matches = [i for i, c in enumerate(self.rack) if (c.type, c.args) == key]
        exact = [i for i in matches if self.rack[i].posted_at == event.posted_at]
        if not matches:
            if index is not None:
                self.mismatches += 1
            return
        found = exact[0] if exact else matches[0]
        waiting = self.rack.pop(found)
        if index is not None:
            self.removals.append(Removal(index, event.clock, found,
                                         replace(waiting, posted_at=event.posted_at),
                                         event.reason))

    def rebase(self, history, position: int) -> bool:
        """Put the rack as the newest snapshot at or before `position` has it (the
        engine's own word, `Snapshot.coderack`), then re-apply the rack changes of the
        events since; nothing when that snapshot was already used. True when re-based
        (G-036). The view calls this at every redraw, so the list is exact at every
        step boundary while the run is live and at every index when seeking. The
        removals recorded since that snapshot are recorded again as the events are
        re-applied, so that the position a greyed row is put back at (G-038) is read on
        the re-based rack whether the events were played or replayed by a seek (a seek
        replays them with no re-basing between, and the rack drifts by its twins)."""
        at = history.snapshot_index(position)
        if at is None or at <= self.rebased_at:
            return False
        snapshot = history.state_at(position)
        self.rack = [Waiting(d["type"], tuple(d["args"]), d["raw"], d["generation"],
                             d["urgency"], d["posted_at"], d["by"], d["chance"])
                     for d in snapshot.coderack["codelets"]]
        self.removals = [r for r in self.removals if r.index <= at]
        for index, event in enumerate(history.events[at + 1:position + 1], start=at + 1):
            if isinstance(event, ev.CodeletRemoved):
                self._remove(event, index)
            else:
                self.apply(event)
        self.rebased_at = at
        return True

    # ---- what the rack holds (G-036) ---------------------------------------------------------

    def counts(self) -> list:
        """How many codelets of each type wait, as (type, count) in the strip's order,
        types with none left out, as `Coderack.counts` (D-051) [F95 p. 97]."""
        found = {}
        for c in self.rack:
            found[c.type] = found.get(c.type, 0) + 1
        return [(t, found[t]) for t in STRIP_ORDER if t in found]

    def shares(self) -> list:
        """The strip's bars as (label, percent) in the strip's order for the types
        present, summing to 100, as `Coderack.shares` [F95 p. 97] [F95 p. 107 (img)]."""
        total = len(self.rack)
        return [(codelet_label(t), 100.0 * n / total) for t, n in self.counts()]

    def total_urgency(self) -> float:
        """The "Total Coderack urgency" [F95 p. 77]: the adjusted urgencies summed (D-052)."""
        return sum(c.urgency for c in self.rack)

    def snapshot_codelets(self) -> tuple:
        """The rack in the snapshot's form (`snapshot.coderack_of`), for the tests
        (G-036)."""
        return tuple({"type": c.type, "label": c.label, "args": c.args, "raw": c.raw,
                      "urgency": c.urgency, "generation": c.generation,
                      "posted_at": c.posted_at, "age": c.age(self.clock), "by": c.by,
                      "chance": c.chance} for c in self.rack)

    def removed_now(self) -> list:
        """The removals of the event shown, greyed in the list for that one event
        (G-038)."""
        return [r for r in self.removals if r.index == self.index]

    def rows(self) -> tuple:
        """The list's rows (G-038): every codelet waiting, the oldest first, with its age
        at the clock shown; the codelets the event shown removed are put back in grey at
        the positions they had."""
        rows = [Row(i, c, c.age(self.clock)) for i, c in enumerate(self.rack)]
        for removal in self.removed_now():
            rows.insert(min(removal.position, len(rows)),
                        Row(removal.position, removal.waiting, removal.waiting.age(self.clock),
                            True, removal.reason))
        return tuple(Row(i, r.waiting, r.age, r.removed, r.reason) for i, r in enumerate(rows))

    def layout(self, width, height) -> StripLayout:
        """The strip's geometry for a size (G-037)."""
        return strip_layout(self, width, height)

    def tooltip(self, codelet_type: str) -> str:
        """The hover text of a bar (G-037)."""
        return hover_text(self, codelet_type)


# ---- the strip (P4; G-037) ----------------------------------------------------------------------


def strip_layout(model, width, height) -> StripLayout:
    """The strip as French's screen drew it [F95 p. 97] [F95 p. 107 (img)]: the box, one
    slot per type in the strip's order with a bar as high as the type's percent of the
    rack and the percent printed above it, the labels under the box, the counter
    "Number of codelets run: N" for the pane to show over the box's right end; the bar
    of the codelet just chosen outlined (G-037). A pure function of the model and the
    size."""
    width, height = float(width), float(height)
    box_x = MARGIN
    box_y = MARGIN
    box_w = max(1.0, width - 2 * MARGIN)
    box_h = max(1.0, height - box_y - LABEL_HEIGHT - MARGIN)
    inner_h = max(0.0, box_h - NUMBER_HEIGHT - 2)
    slot_w = box_w / len(STRIP_ORDER)
    bar_w = slot_w * BAR_SHARE
    counts = dict(model.counts())
    total = len(model.rack)
    bars = []
    for i, codelet_type in enumerate(STRIP_ORDER):
        count = counts.get(codelet_type, 0)
        percent = 100.0 * count / total if total else 0.0
        slot_x = box_x + i * slot_w
        bar_h = inner_h * percent / 100.0
        bars.append(Bar(codelet_type, codelet_label(codelet_type), count, percent, slot_x, slot_w,
                        slot_x + (slot_w - bar_w) / 2, box_y + box_h - 1 - bar_h, bar_w, bar_h,
                        str(int(round(percent))) if count else "",
                        codelet_type == model.chosen_type))
    counter = f"Number of codelets run: {model.clock}"
    return StripLayout(tuple(bars), (box_x, box_y, box_w, box_h), counter, box_y + box_h,
                       width, height)


def bar_at(layout, x, y):
    """The slot under a point (the whole column, from the box's top to the labels), or
    None (G-037)."""
    for bar in layout.bars:
        if bar.slot_x <= x < bar.slot_x + bar.slot_width and layout.box[1] <= y \
                <= layout.label_y + LABEL_HEIGHT:
            return bar
    return None


def legend_lines() -> tuple:
    """The catalogue legend (D-051, D-081): one line per type in the strip's order, the
    label, the full name, the band, the job and whether the job is from the text (with its
    pages), inferred from the name, or the re-interpretation's (G-037)."""
    lines = []
    for row in CATALOGUE:
        source = (f"from the text {row.pages}" if row.job_source == "text"
                  else f"inferred from the name (D-051) {row.pages}" if row.job_source == "inferred"
                  else f"re-interpretation (D-081) {row.pages}")
        lines.append(f"{row.label}: {row.name} ({row.band}). {row.job} [{source}]")
    return tuple(lines)


def legend_text() -> str:
    """The legend as one text, with its heading (G-037)."""
    return "\n".join((f"The {len(CATALOGUE)} codelet types in the strip's order [F95 p. 97] "
                       "(D-051; the role codelets after the legend's 23, D-081):",)
                      + legend_lines())


def hover_text(model, codelet_type: str) -> str:
    """The hover text of a bar (G-037): the type's legend entry, how many wait and their
    share at the clock shown, and whether the codelet just chosen is one."""
    row = ENTRY[codelet_type]
    count = dict(model.counts()).get(codelet_type, 0)
    total = len(model.rack)
    percent = 100.0 * count / total if total else 0.0
    source = (f"job from the text {row.pages}" if row.job_source == "text"
              else f"job inferred from the name (D-051) {row.pages}" if row.job_source == "inferred"
              else f"job of the re-interpretation (D-081) {row.pages}")
    lines = [f"{row.label}: {row.name} ({row.band})", row.job, source,
             f"{count} waiting of {total} ({percent:.0f}%) at codelet {model.clock}"]
    if model.chosen_type == codelet_type and model.choice is not None:
        lines.append(f"codelet {model.choice.clock}, the one just chosen, is a {row.label}")
    return "\n".join(lines)


# ---- the list (P4b; G-038) ----------------------------------------------------------------------


def band_lines(model) -> tuple:
    """The chosen band's two lines (G-038; Numbo GUI, `coderack_view.py`'s `chosen_lines`):
    which codelet, its chance, the exponent and the rack size [F95 p. 77] [F95 p. 92];
    then its urgency, generation, who posted it and when, and what it did [F95 p. 73]."""
    c = model.choice
    if c is None:
        return ("no codelet chosen yet", f"{len(model.rack)} waiting")
    first = (f"codelet {c.clock}: {codelet_text(c.type, c.args)}  chance {c.chance:.3f} "
             f"of {c.rack_size} at a {c.a:.2f} (T {c.temperature:.1f})")
    parts = [f"urgency {c.urgency:g}, gen {c.generation}"]
    if c.waiting is not None:
        parts.append(f"raw {c.waiting.raw:g}, by {c.waiting.by} at codelet "
                     f"{c.waiting.posted_at} (age {c.clock - c.waiting.posted_at})")
    elif c.alike > 1:
        parts.append(f"one of {c.alike} alike on the rack")
    if c.fizzled:
        parts.append(f"fizzled: {c.reason}")
    elif c.action:
        did = c.detail or c.action
        if c.posted:
            did += f"; posted {' '.join(codelet_label(t) for t in c.posted)}"
        parts.append(did)
    else:
        parts.append("running")
    return (first, "; ".join(parts))


def row_text(row: Row) -> tuple:
    """A row's cells in `COLUMNS`' order (G-038)."""
    c = row.waiting
    chance = "" if c.chance is None else f"{c.chance:.3f}"
    return (str(row.position + 1), codelet_text(c.type, c.args), f"{c.raw:g}", f"{c.urgency:.2f}",
            str(c.generation), str(c.posted_at), str(row.age), chance,
            c.by + (f" (removed: {row.reason})" if row.removed else ""))


def row_tooltip(row: Row) -> str:
    """The hover text of a row (G-038): the codelet in the form of [F95 p. 77], its
    catalogue entry and its numbers."""
    c = row.waiting
    entry = ENTRY[c.type]
    chance = "none yet" if c.chance is None else f"{c.chance:.4f}"
    lines = [f"{codelet_text(c.type, c.args)}: {entry.name} ({entry.band})", entry.job,
             f"raw urgency {c.raw:g}, generation {c.generation}, adjusted {c.urgency:.3f} "
             f"(D-052)", f"posted by {c.by} at codelet {c.posted_at}, age {row.age} (D-053)",
             f"chance at the last draw {chance} (D-060)"]
    if row.removed:
        lines.append(f"removed at this event for {row.reason} (D-053)")
    return "\n".join(lines)
