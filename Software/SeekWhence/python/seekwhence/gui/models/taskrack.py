"""The taskrack pane's model (P6), Qt-free: the tasks waiting on the rack, grouped by type;
the task just chosen, with its chance; the cleanings; the history of chosen tasks and the
tallies since the last term (PANES.md P6; G-027, G-028, G-029).

The rack is where the tasks wait: a task may "set out other tasks -- place them on the
taskrack, where they will stay until chosen and run" [M86 p. 41]. "All tasks have
'urgencies' (integer weights), and more urgent tasks will have a greater chance of being
chosen than less urgent ones do" [M86 p. 42]; one draw chooses a task with probability
proportional to its urgency raised to `urgency_power` (DECISION D-007). "At certain times
some cleaning is done (removing old tasks of certain types)" [M86 p. 42] (DECISION D-010),
and an old task that is chosen may find "the Seek-Whence world quite different than when it
was created" [M86 p. 42] and do nothing: a stale task (DECISION D-009). Each task type
works in one arena, the cytoplasm, the socratoplasm, the platoplasm or the user's console
(DECISION D-057), which gives the rows their colours. There is no figure of the taskrack in
the thesis or in FCCA: the drawing is GUI design, not Meredith's (ANSWERS.md 4), after the
Numbo GUI's coderack view (`coderack_view.py`: the chosen codelet on a band, the bins as
rows with count bars).

The model is a view's helper (`seekwhence.gui.taskrack_view`), fed like the stats model
(G-016): `on_event(event)` for every event, then `update(snapshot)` with the snapshot in
force at that event. The rack is a shadow kept from the events, `posted`, `chosen` and
`cleaned` (the cleaning rule of DECISION D-010 applied to the counts the event carries),
and every new snapshot's rack, read from `Taskrack.tasks` itself, is the authority: the
shadow is replaced by it where they differ and `corrections` counts the times (none in the
appendix runs or on blackburn-11; G-027). The chosen task's chance is its weight over the
total weight of the rack before the draw, the rack after it plus the task itself (G-007).

`rack_lines` and the text functions are pure functions of the model, so the tests check
every line against it (Numbo GUI, `coderack_view.py`'s `coderack_rows`).
"""

import dataclasses
import re

from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.snapshot import TaskRecord
from seekwhence.trace import item_text, task_line

__all__ = ["ARENAS", "ARENA_COLORS", "BANNER", "AT_ONCE_TYPES", "STRUCTURE_NAME", "ChosenTask", "Cleaning",
           "RackRow", "Line", "TaskrackModel", "rack_lines", "chosen_text", "chance_text",
           "cleaning_text", "tally_text", "history_text", "task_text", "names_in",
           "format_args"]

# DECISION D-057: the four arenas, in the order the rows are coloured and the filter lists them.
ARENAS = ("cytoplasm", "socratoplasm", "platoplasm", "user")
ARENA_COLORS = {"cytoplasm": "#1b7f79", "socratoplasm": "#6a3d9a", "platoplasm": "#b35c00",
                "user": "#4a5a78"}
BANNER = 30             # events a cleaning banner stays up (G-028)
# DECISION D-012: the Hfilter of a new term is set out and run at once, ahead of any draw
# (`World.run_now`): posted, then chosen as the very next event. It had no chance to show.
AT_ONCE_TYPES = ("Hfilter",)
STRUCTURE_NAME = re.compile(r"\b(glint|glom|spark|bond|gnoth)\d+\b")


@dataclasses.dataclass(frozen=True)
class ChosenTask:
    """The task chosen at `clock` (`TaskChosen`): `number` (None for `empty-task`,
    DECISION D-013), `type`, `args` as the trace prints each one, `urgency`, `arena`,
    `weight`, its `chance` at the draw (None for `empty-task` and for a task run
    `at_once`, ahead of any draw, DECISION D-012), `at` (the model's event count then),
    and, once a `stale` event has named it, `stale` with the `reason` [M86 pp. 41-42]
    (G-027)."""
    clock: int
    number: int | None
    type: str
    args: tuple
    urgency: int
    arena: str
    weight: int
    chance: float | None
    at: int
    at_once: bool = False
    stale: bool = False
    reason: str | None = None


@dataclasses.dataclass(frozen=True)
class Cleaning:
    """One cleaning of the rack (`TasksCleaned`): the `moment` (`term`, `reform` or
    `hypothesis`), the `removed` (type, count) pairs and `at`, the model's event count
    then (DECISION D-010) (G-028)."""
    moment: str
    removed: tuple
    at: int


@dataclasses.dataclass(frozen=True)
class RackRow:
    """One type's waiting tasks: `type`, `arena`, `count`, the summed `weight`, its
    `share` of the rack's total weight (the chance the next draw falls on the type,
    DECISION D-007) and the `tasks` in posting order [M86 pp. 41-42] (G-027)."""
    type: str
    arena: str
    count: int
    weight: int
    share: float
    tasks: tuple


@dataclasses.dataclass(frozen=True)
class Line:
    """One line the rack list paints (G-028): `kind` (`chosen`, `banner`, `row`,
    `task`, `note`), `text`, `arena` (the colour, or None), `bar` (a share of the total
    weight for a `row`, else None), `count` (the row's count as text), `number` (a
    task's), `type` (a row's, for expanding), `tint` (the band), `grey` (a stale task),
    `selected` (the task names something selected) and `tooltip`."""
    kind: str
    text: str
    arena: str | None = None
    bar: float | None = None
    count: str = ""
    number: int | None = None
    type: str | None = None
    tint: bool = False
    grey: bool = False
    selected: bool = False
    tooltip: str = ""


def format_args(args) -> str:
    """The arguments as the transcripts print them, `(glint1 glint3)`, from the text
    items the model keeps; empty for none (`doing task Dissolver`) [M86 p. 164]."""
    return "(" + " ".join(args) + ")" if args else ""


def names_in(args) -> tuple:
    """The structures a task's arguments name, `glint1`, `glom2`, `bond1`, `spark3`,
    `gnoth1`, in order without repeats: what a click on the task selects (PANES.md P6;
    G-025, G-028)."""
    found = []
    for item in args:
        for match in STRUCTURE_NAME.finditer(item):
            if match.group(0) not in found:
                found.append(match.group(0))
    return tuple(found)


def chance_text(chance) -> str:
    """A chance as a percentage, `12.5%`; empty for None (G-027)."""
    return "" if chance is None else f"{100 * chance:.1f}%"


def task_text(task: TaskRecord) -> str:
    """A waiting task as a line: `#12  (glint1 glint3)  urgency 10  posted at 7`
    [M86 pp. 41-42] (G-028)."""
    args = format_args(task.args)
    return (f"#{task.number}  " + (f"{args}  " if args else "")
            + f"urgency {task.urgency}  posted at {task.created_at}")


def chosen_text(chosen: ChosenTask | None) -> str:
    """The chosen task as the trace prints it, `doing task Sparkler-plus on (glint1
    glint1 2)` [M86 p. 164], with `stale` after it once it has said so [M86 p. 42];
    `nothing chosen yet` before the first (Numbo GUI, `coderack_view.py`'s
    `chosen_text`; G-028)."""
    if chosen is None:
        return "nothing chosen yet"
    text = task_line(chosen.type, chosen.args)
    return text + ("  stale" if chosen.stale else "")


def cleaning_text(cleaning: Cleaning | None) -> str:
    """The banner: `term: removed 3 Sparkler, 2 Tester` (PANES.md P6; DECISION D-010)."""
    if cleaning is None:
        return ""
    parts = ", ".join(f"{count} {type}" for type, count in cleaning.removed)
    return f"{cleaning.moment}: removed {parts}"


def history_text(chosen: ChosenTask) -> str:
    """A history row: the clock, then the task as the trace prints it, `52  doing task
    Hfilter on (glint4)`, with `stale` when it was [M86 p. 164] (G-028)."""
    return f"{chosen.clock}  {chosen_text(chosen)}"


def tally_text(model) -> str:
    """The per-type tallies since the last term, `since term 3: Sparkler 4 · Tester 2`,
    or `since the start:` before any term (PANES.md P6; G-029)."""
    when = f"since term {model.terms}" if model.terms else "since the start"
    parts = " · ".join(f"{type} {n}" for type, n in model.tallies.items())
    return f"{when}: {parts}" if parts else f"{when}: nothing chosen"


class TaskrackModel:
    """The rack's shadow and the chosen tasks (see the module docstring; G-027, G-028,
    G-029). `waiting`: the tasks on the rack by number, in posting order; `chosen`: the
    task running now; `history`: every task chosen this run; `tallies`: how many of each
    type since the last term; `cleaning`: the last cleaning; `terms`: terms entered;
    `events`: events seen this run; `corrections`: snapshots that disagreed with the
    shadow; `selection`: the shared selection (G-025)."""

    def __init__(self, selection: Selection | None = None):
        self.selection = selection if selection is not None else Selection()
        self.reset()

    def reset(self) -> None:
        """Forget the run (a `start` event) (Numbo GUI, `coderack_model.py`'s `reset`)."""
        self.urgency_power = 1
        self.waiting: dict = {}
        self.chosen = None
        self.history: list = []
        self.tallies: dict = {}
        self.cleaning = None
        self.terms = 0
        self.events = 0
        self.max_waiting = 0
        self.corrections = 0
        self._snapshot = None
        self._chosen_fresh = False
        self._last_posted = None

    # -- reading ---------------------------------------------------------------------------

    @property
    def tasks(self) -> tuple:
        """The waiting tasks in posting order, as `Taskrack.tasks` [M86 p. 41]."""
        return tuple(self.waiting.values())

    @property
    def total_weight(self) -> int:
        """The sum of the waiting tasks' weights (DECISION D-007)."""
        return sum(t.weight for t in self.waiting.values())

    @property
    def chosen_fresh(self) -> bool:
        """Whether the latest event was the choice (Numbo GUI, `coderack_view.py`)."""
        return self._chosen_fresh

    def chances(self) -> dict:
        """Each waiting task's chance at the next draw, by number: its weight over the
        total (DECISION D-007); they sum to 1 when any task waits."""
        total = self.total_weight
        return {n: t.weight / total for n, t in self.waiting.items()} if total else {}

    def draw_chances(self) -> dict:
        """The chances at the last draw, by number: the chosen task's and those of the
        tasks it left waiting, over the total before the draw (G-007, G-027); they sum
        to 1. Empty before the first choice and for `empty-task`."""
        c = self.chosen
        if c is None or c.number is None or c.at_once:
            return {}
        total = self.total_weight + c.weight
        chances = {c.number: c.weight / total}
        for n, t in self.waiting.items():
            chances[n] = t.weight / total
        return chances

    def rows(self, arena: str | None = None) -> tuple:
        """The waiting tasks grouped by type in order of first appearance on the rack
        (`Taskrack.count_by_type`), each a `RackRow`; `arena` keeps one arena's rows
        (PANES.md P6; DECISION D-057; G-027)."""
        groups: dict = {}
        for task in self.waiting.values():
            groups.setdefault(task.type, []).append(task)
        total = self.total_weight
        rows = []
        for type, tasks in groups.items():
            if arena is not None and tasks[0].arena != arena:
                continue
            weight = sum(t.weight for t in tasks)
            rows.append(RackRow(type, tasks[0].arena, len(tasks), weight,
                                weight / total if total else 0.0, tuple(tasks)))
        return tuple(rows)

    def banner(self) -> Cleaning | None:
        """The last cleaning while its banner is up, `BANNER` events from it (G-028)."""
        c = self.cleaning
        return c if c is not None and self.events - c.at < BANNER else None

    def select_task(self, number: int | None) -> tuple:
        """Select, in the shared selection, the structures a task's arguments name, the
        task waiting or in the history by `number` (a second click clears); returns the
        names (PANES.md P6; G-025, G-028)."""
        task = self.waiting.get(number)
        if task is None:
            for chosen in reversed(self.history):
                if chosen.number == number:
                    task = chosen
                    break
        if task is None:
            return ()
        names = names_in(task.args)
        self.selection.toggle("task", f"task{number}", names)
        return names

    def names_selected(self, args) -> bool:
        """Whether the arguments name anything in the shared selection (PANES.md P2: "the
        tasks whose arguments name it"; G-025)."""
        return any(self.selection.includes(name) for name in names_in(args))

    # -- the events ------------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Keep the shadow: `posted` adds, `chosen` removes and becomes the running task,
        `stale` marks it, `cleaned` removes by the rule of DECISION D-010, `term` resets
        the tallies (G-027, G-029)."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self.urgency_power = int(dict(event.config).get("urgency_power", 1))
        self.events += 1
        self._chosen_fresh = kind == "chosen"
        if kind == "posted":
            self._post(event)
        elif kind == "chosen":
            self._choose(event)
        elif kind == "stale":
            self._stale(event)
        elif kind == "cleaned":
            self._clean(event)
        elif kind == "term":
            self.terms = event.n
            self.tallies = {}
        if kind != "posted":
            self._last_posted = None

    def _weight(self, urgency: int) -> int:
        return urgency ** self.urgency_power

    def _post(self, event) -> None:
        record = TaskRecord(event.number, event.type, tuple(item_text(a) for a in event.args),
                            event.urgency, event.arena, event.created_at, self.terms,
                            self._weight(event.urgency))
        self.waiting[event.number] = record
        self.max_waiting = max(self.max_waiting, len(self.waiting))
        self._last_posted = event.number

    def _choose(self, event) -> None:
        """The task leaves the rack; its chance is its weight over the rack's total
        before the draw (G-007), unless it was run at once (DECISION D-012)."""
        weight = self._weight(event.urgency)
        at_once = event.type in AT_ONCE_TYPES and event.number == self._last_posted
        chance = None
        if event.number is not None:
            total = self.total_weight
            if event.number in self.waiting:
                del self.waiting[event.number]
            else:
                total += weight         # a task the shadow never saw posted
            if not at_once:
                chance = weight / total if total else None
        self.chosen = ChosenTask(event.clock, event.number, event.type,
                                 tuple(item_text(a) for a in event.args), event.urgency,
                                 event.arena, weight, chance, self.events, at_once)
        self.history.append(self.chosen)
        self.tallies[event.type] = self.tallies.get(event.type, 0) + 1

    def _stale(self, event) -> None:
        c = self.chosen
        if c is None or c.type != event.type:
            return
        self.chosen = dataclasses.replace(c, stale=True, reason=event.reason)
        if self.history and self.history[-1] is c:
            self.history[-1] = self.chosen

    def _clean(self, event) -> None:
        """Remove what the event counts: at `term`, each type's tasks posted before the
        previous term, else all of that type; the oldest `count` when the rule does not
        give the count (DECISION D-010)."""
        for type, count in event.removed:
            old = [n for n, t in self.waiting.items() if t.type == type
                   and (event.moment != "term" or t.at_term < self.terms)]
            if len(old) != count:
                old = [n for n, t in self.waiting.items() if t.type == type][:count]
            for n in old:
                del self.waiting[n]
        self.cleaning = Cleaning(event.moment, tuple(event.removed), self.events)

    # -- the snapshot ----------------------------------------------------------------------

    def update(self, snapshot) -> None:
        """Take the snapshot in force (None before the first): its rack, read from
        `Taskrack.tasks`, replaces the shadow where they differ and `corrections` counts
        it; a snapshot already taken costs nothing (G-027)."""
        if snapshot is None or snapshot is self._snapshot:
            return
        self._snapshot = snapshot
        tasks = snapshot.rack.tasks
        if tasks != self.tasks:
            self.corrections += 1
            self.waiting = {t.number: t for t in tasks}
            self.max_waiting = max(self.max_waiting, len(self.waiting))


def rack_lines(model: TaskrackModel, expanded=frozenset(), arena: str | None = None) -> tuple:
    """The lines of the rack list: the chosen task on its band (tinted while it runs,
    grey and with its reason when stale), the chance it had, the cleaning banner while
    it is up, how many tasks wait, then one `row` per type with its count and share,
    followed by its tasks when the type is in `expanded`; `arena` filters the rows
    (PANES.md P6; Numbo GUI, `coderack_view.py`; G-027, G-028)."""
    lines = []
    c = model.chosen
    selected = c is not None and model.names_selected(c.args)
    tip = (f"{chosen_text(c)}\nurgency {c.urgency}, arena {c.arena}"
           + (f"\nchance at the draw: {chance_text(c.chance)}" if c.chance is not None else "")
           + ("\nrun at once, ahead of any draw (DECISION D-012)" if c.at_once else "")
           + (f"\nstale: {c.reason}" if c.stale else "")) if c is not None else ""
    lines.append(Line("chosen", chosen_text(c), c.arena if c is not None else None,
                      number=c.number if c is not None else None, tint=c is not None,
                      grey=c is not None and c.stale, selected=selected, tooltip=tip))
    if c is not None:
        when = f"clock {c.clock}, urgency {c.urgency}"
        if c.at_once:
            when += ", run at once ahead of any draw (DECISION D-012)"
        elif c.chance is not None:
            when += f", chance {chance_text(c.chance)} (weight {c.weight} of "
            when += f"{model.total_weight + c.weight} before the draw)"
        lines.append(Line("note", when, number=c.number, tint=True,
                          grey=c.stale, tooltip=tip))
    banner = model.banner()
    if banner is not None:
        lines.append(Line("banner", cleaning_text(banner),
                          tooltip="cleaning (DECISION D-010): old tasks of certain types "
                                  "removed [M86 p. 42]"))
    n = len(model.waiting)
    lines.append(Line("note", f"{n} waiting (at most {model.max_waiting} this run), "
                              f"total weight {model.total_weight}"))
    for row in model.rows(arena):
        lines.append(Line("row", row.type, row.arena, bar=row.share, count=str(row.count),
                          type=row.type,
                          selected=any(model.names_selected(t.args) for t in row.tasks),
                          tooltip=f"{row.count} {row.type}: weight {row.weight} of "
                                  f"{model.total_weight}, {chance_text(row.share)} of the next "
                                  f"draw; arena {row.arena}"))
        if row.type in expanded:
            for task in row.tasks:
                lines.append(Line("task", task_text(task), row.arena, number=task.number,
                                  selected=model.names_selected(task.args),
                                  tooltip=f"task #{task.number} {task.type} "
                                          f"{format_args(task.args)}\nurgency {task.urgency}, "
                                          f"weight {task.weight}, posted at clock "
                                          f"{task.created_at} with {task.at_term} terms "
                                          f"entered; chance at the next draw "
                                          f"{chance_text(task.weight / model.total_weight)}"))
    return tuple(lines)
