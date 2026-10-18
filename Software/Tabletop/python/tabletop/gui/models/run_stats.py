"""The stats pane's model (PANES.md P12; Numbo GUI, `run_stats.py`'s `RunStats`, through
the Seek-Whence GUI's `models/run_stats.py`; G-045). Qt-free.

`RunStats` observes a run's events and is a function of them alone, exact at every
index, so a seek shows the numbers of the past: from `RunStarted` the problem, seed,
profile and the fields `changed` from `standard` (D-066; ANSWERS.md 3: a run whose
configuration differs from its profile says so); from every event the count, the clock
(codelets run) and the generator's draws (D-064); from `CodeletChosen` the codelet last
chosen; from `TemperatureUpdated` T, whether it is still held (D-049), the reason and Q
[F95 p. 91]; from `WorldviewChanged` the Worldview's names and its rating at the change
(D-040), the rating as it stands coming from the snapshot in force, dated (D-062); the
structures built and removed, the competitions, the changes and the fizzles counted
from their events; from `AnswerGiven` and `RunEnded` the answer, how it was read (D-057)
and French's report line, `observe.report_sentence` of the end event's fields, which
equals `run.report_line` of the run's `Result` [F95 p. 145]. `update(snapshot,
invariants)` reads what the events do not carry: the Worldview's rating as it stands
(`Snapshot.worldview`, the structure value, D-062) and `invariants.check` (D-077) on the
state, taken by the tap beside each snapshot (`models/tap.py`, G-045), empty when every
invariant holds, dated by the snapshot's clock; a non-empty list is an engine finding
and is shown, never hidden (PANES.md P12). `finish(outcome)` adds what no event says: a
stopped run or an error (the run controller's `RunOutcome`). `texts()` gives every
field as text, as the Numbo GUI's stats panel does.
"""

from tabletop import events as ev
from tabletop import observe

__all__ = ["FIELDS", "RunStats", "config_note", "number_text", "run_text"]

# The fields and their titles, in the order the pane shows them (PANES.md P12; Numbo
# GUI, `stats_view.py`'s FIELDS).
FIELDS = (("state", "State"), ("source", "Source"), ("run", "Run"),
          ("config", "Configuration"), ("events", "Events"), ("codelets", "Codelets run"),
          ("draws", "Draws"), ("codelet", "Codelet"), ("temperature", "T"), ("q", "Q"),
          ("worldview", "Worldview"), ("structures", "Structures"),
          ("competitions", "Competitions"), ("changes", "Changes"), ("fizzles", "Fizzles"),
          ("outcome", "Outcome"), ("result", "Result"), ("report", "Report"),
          ("invariants", "Invariants"))
STANDARD = "standard"


def number_text(value) -> str:
    """A number as the pane prints it: floats with one decimal, as the dumps print T
    [F95 p. 98 (img)], None as nothing (Numbo GUI, `stats_view.py`)."""
    if value is None:
        return ""
    if isinstance(value, float):
        return f"{value:.1f}"
    return str(value)


def run_text(problem, profile, seed) -> str:
    """`surround-05, seed 3`, with the profile when it is not `standard`: the Run line,
    which agrees with the window's title and the controls (PANES.md P9; G-045)."""
    if problem is None:
        return ""
    text = f"{problem}, seed {seed}"
    return text if profile == STANDARD else f"{text} ({profile})"


def config_note(profile, changed) -> str:
    """The configuration line (ANSWERS.md 3; G-045): the profile, then the fields that
    differ from `standard` as `RunStarted.changed` reports them (D-066), `profile` itself
    left out: `standard (the defaults)`, `human: type_weight=50`, `standard: alpha=100`
    for an edited run."""
    if profile is None:
        return ""
    pairs = [f"{name}={value}" for name, value in changed if name != "profile"]
    if not pairs:
        return f"{profile} (the defaults)" if profile == STANDARD else profile
    return f"{profile}: {', '.join(pairs)}"


class RunStats:
    """The stats of the run whose events it observes (see the module docstring; Numbo
    GUI, `run_stats.py`; G-045). A `RunStarted` resets it."""

    def __init__(self):
        self.reset()

    def reset(self) -> None:
        """Forget the run (Numbo GUI, `run_stats.py`)."""
        self.problem = None
        self.seed = None
        self.profile = None
        self.changed = ()
        self.events = 0
        self.clock = 0
        self.draws = 0
        self.codelet = ""
        self.temperature = None
        self.held = False
        self.by = ""
        self.q = None
        self.worldview = ()         # the names in the Worldview, in the order entered
        self.rating = None          # the rating at the last change (WorldviewChanged)
        self.current = None         # the rating as it stands (the snapshot in force)
        self.rated_at = None        # the clock of that snapshot
        self.groups_built = 0
        self.groups_removed = 0
        self.correspondences_built = 0
        self.correspondences_removed = 0
        self.competitions = 0
        self.changes = 0
        self.fizzles = 0
        self.answer = None          # the AnswerGiven event
        self.ended = None           # the RunEnded event
        self.stopped = None         # the events published when stopped (finish)
        self.error = None           # the error that ended the run (finish)
        self.faults = None          # invariants.check's sentences (update); None: not read
        self.checked_at = None      # the clock of the snapshot the faults date from

    def on_event(self, event) -> None:
        """Apply one event (the observer protocol, D-065) (G-045)."""
        if isinstance(event, ev.RunStarted):
            self.reset()
            self.problem, self.seed, self.profile = event.problem, event.seed, event.profile
            self.changed = tuple(event.changed)
        self.events += 1
        self.clock = event.clock
        self.draws = event.draws
        if isinstance(event, ev.CodeletChosen):
            self.codelet = observe.codelet_text(event.codelet, event.args, event.generation)
        elif isinstance(event, ev.TemperatureUpdated):
            self.temperature, self.held, self.by, self.q = (event.temperature, event.held,
                                                            event.by, event.q)
        elif isinstance(event, ev.WorldviewChanged):
            names = [n for n in self.worldview if n not in event.removed]
            self.worldview = tuple(names + [n for n in event.added if n not in names])
            self.rating = event.rating
            self.changes += 1
        elif isinstance(event, ev.CompetitionHeld):
            self.competitions += 1
        elif isinstance(event, ev.CodeletFizzled):
            self.fizzles += 1
        elif isinstance(event, ev.GroupBuilt):
            self.groups_built += 1
        elif isinstance(event, ev.GroupRemoved):
            self.groups_removed += 1
        elif isinstance(event, ev.CorrespondenceBuilt):
            self.correspondences_built += 1
        elif isinstance(event, ev.CorrespondenceRemoved):
            self.correspondences_removed += 1
        elif isinstance(event, ev.AnswerGiven):
            self.answer = event
        elif isinstance(event, ev.RunEnded):
            self.ended = event

    def update(self, snapshot, invariants=None) -> None:
        """Read what the events do not carry from the snapshot in force (G-045): the
        Worldview's rating as it stands (`worldview["total"]`, the structure value,
        D-062) and the invariants the tap checked beside it (D-077): `invariants` is the
        tap's reading (`faults`, a tuple of sentences), both dated by the snapshot's
        clock; None (before the first snapshot) leaves them unread."""
        if snapshot is None:
            self.current = self.rated_at = self.faults = self.checked_at = None
            return
        self.current = snapshot.worldview.get("total")
        self.rated_at = snapshot.clock
        if invariants is None:
            self.faults, self.checked_at = None, None
            return
        self.faults = tuple(invariants.get("faults", ()))
        self.checked_at = snapshot.clock

    def finish(self, outcome) -> None:
        """The run controller's `RunOutcome`: a stopped run or an error, which no event
        says (Numbo GUI, `stats_view.py`'s `finish`; G-045)."""
        self.stopped = outcome.events if outcome.stopped else None
        self.error = outcome.error

    # ---- the texts (Numbo GUI, `stats_view.py`'s `texts`) -----------------------------------

    def temperature_text(self) -> str:
        """`34.8 (by schedule)`, `80.0 (held, by start)` (D-049, D-050; G-045)."""
        if self.temperature is None:
            return ""
        held = "held, " if self.held else ""
        return f"{number_text(self.temperature)} ({held}by {self.by})"

    def worldview_text(self) -> str:
        """`1 correspondence: cp1; rating 80.4 (as of codelet 81)`, the rating as the
        snapshot in force has it (D-062), else `rating 75.9 at the change` from the
        events; `empty` (D-040; G-045)."""
        if self.problem is None:
            return ""
        if not self.worldview:
            return "empty"
        n = len(self.worldview)
        word = "correspondence" if n == 1 else "correspondences"
        text = f"{n} {word}: {', '.join(self.worldview)}; rating "
        if self.current is not None:
            return text + f"{number_text(self.current)} (as of codelet {self.rated_at})"
        return text + f"{number_text(self.rating)} at the change"

    def structures_text(self) -> str:
        """`groups 2 built, 0 removed; correspondences 2 built, 0 removed` (G-045)."""
        if self.problem is None:
            return ""
        return (f"groups {self.groups_built} built, {self.groups_removed} removed; "
                f"correspondences {self.correspondences_built} built, "
                f"{self.correspondences_removed} removed")

    def outcome_text(self) -> str:
        """`answered`, `capped`, `stopped after N events`, `error: ...`, or nothing while
        the run goes on (D-061; G-045)."""
        if self.error is not None:
            return f"error: {self.error!r}"
        if self.ended is not None:
            return "capped" if self.ended.capped else "answered"
        if self.stopped is not None:
            return f"stopped after {self.stopped} events"
        return ""

    def result_text(self) -> str:
        """`Result`'s fields the other rows do not show, at the end (D-057, D-062, D-063):
        the answer with its label, `literal`, how it was read and through which
        correspondence, the structure value and the Worldview (G-045)."""
        if self.ended is None:
            return ""
        e = self.ended
        text = f"answer {e.answer} ({e.label or 'none'})"
        if e.literal:
            text += " literal"
        if self.answer is not None:
            text += f", through {self.answer.through}"
            if self.answer.via:
                text += f" ({self.answer.via})"
        text += f"; structure {e.structure_value:.0f}"
        names = ", ".join(name for name, _, _ in e.worldview)
        return text + (f"; Worldview {names}" if names else "; Worldview empty")

    def report_text(self) -> str:
        """French's report line at the end, from the `RunEnded` event's fields
        [F95 p. 145]: `run.report_line` of the run's `Result` gives the same line
        (G-045)."""
        e = self.ended
        if e is None:
            return ""
        return observe.report_sentence(e.answer, e.label, e.literal, e.capped,
                                       e.structure_value, e.codelets, e.temperature)

    def invariants_text(self) -> str:
        """`none violated (as of codelet N)`, or how many and which (D-077; G-045);
        `not checked yet` before the first snapshot."""
        if self.problem is None:
            return ""
        if self.faults is None:
            return "not checked yet"
        if not self.faults:
            return f"none violated (as of codelet {self.checked_at})"
        n = len(self.faults)
        return f"{n} violated (as of codelet {self.checked_at}): " + "; ".join(self.faults)

    @property
    def faulty(self) -> bool:
        """Whether the invariants line reports a violation: shown in red (PANES.md P12;
        G-045)."""
        return bool(self.faults)

    def texts(self, state: str = "", source: str = "") -> dict:
        """Every field of `FIELDS` as text (Numbo GUI, `stats_view.py`'s `texts`); `state`
        is the run controller's, `source` says where the events come from."""
        started = self.problem is not None
        return {"state": state, "source": source,
                "run": run_text(self.problem, self.profile, self.seed),
                "config": config_note(self.profile, self.changed),
                "events": str(self.events) if started else "",
                "codelets": str(self.clock) if started else "",
                "draws": str(self.draws) if started else "",
                "codelet": self.codelet, "temperature": self.temperature_text(),
                "q": "" if self.q is None else f"{self.q:.3f}",
                "worldview": self.worldview_text(), "structures": self.structures_text(),
                "competitions": str(self.competitions) if started else "",
                "changes": str(self.changes) if started else "",
                "fizzles": str(self.fizzles) if started else "",
                "outcome": self.outcome_text(), "result": self.result_text(),
                "report": self.report_text(), "invariants": self.invariants_text()}
