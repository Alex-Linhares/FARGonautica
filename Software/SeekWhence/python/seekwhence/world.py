"""The `World` of one run: the three arenas, the taskrack, the counters and the shared state
[M86 pp. 42-47].

The Seek-Whence world is "three levels, with two potential intermediating structures -- the
hypothesis and the template" [M86 p. 43]: the cytoplasm (glints, sparks, bonds, gloms), the
socratoplasm (gnoths) and the platoplasm (the ideals) [M86 pp. 43-44], with the hypothesis
beside the socratoplasm and the template beside the cytoplasm (Figure 1 [M86 p. 44]). One
`World` holds all of that for one run, made fresh from a `Config` and a seed, as the Numbo
port makes a `World` per run; nothing is shared between runs, and every random choice goes
through its one `Rng`.

Tasks are plain functions on the `World`, registered by type in `handlers`; posting a task
stamps it with the clock and the number of terms entered (for the cleaning of DECISION
D-010); running one publishes the events of `seekwhence.events`. Structures are named
`glintN`, `sparkN`, ... from counters that never reuse a number (DECISION D-014), and a task
that finds its structures gone says so with `stale` and does nothing (DECISION D-009).
"""

from dataclasses import fields

from seekwhence import events
from seekwhence.config import TASK_TYPES, Config
from seekwhence.platoplasm import Platoplasm
from seekwhence.rng import Rng
from seekwhence.taskrack import ARENAS, Task, Taskrack

# The structure kinds the transcripts number, each counted from 1 (DECISION D-014).
NAMED_KINDS = ("glint", "spark", "bond", "glom", "gnoth", "box", "pstruc")


class Arena:
    """A level of the world that holds named structures: the cytoplasm or the socratoplasm
    [M86 pp. 43-44]. Structures are kept in creation order under their names (`glom7`); a
    destroyed structure is removed and its name is never reused (DECISION D-014). The later
    modules keep their own views (the gloms, the bonds, the gnoths) and use the arena for the
    existence checks every task makes first (DECISION D-009)."""

    def __init__(self, level: str):
        self.level = level
        self._members: dict = {}

    def __len__(self) -> int:
        return len(self._members)

    def __contains__(self, name: str) -> bool:
        return name in self._members

    def __iter__(self):
        return iter(self._members.values())

    @property
    def names(self) -> tuple:
        """The names of the structures present, in creation order (DECISION D-014)."""
        return tuple(self._members)

    def add(self, structure):
        """Put a structure, which must have a `name`, into the arena [M86 p. 43]."""
        name = getattr(structure, "name", None)
        if not isinstance(name, str) or not name:
            raise TypeError(f"a structure needs a name, got {structure!r}")
        if name in self._members:
            raise ValueError(f"{name} is already in the {self.level}")
        self._members[name] = structure
        return structure

    def remove(self, name: str):
        """Take the structure called `name` out of the arena, returning it; a KeyError if
        it is not there ("A gnoth ceases to exist only when its subglom collection is
        empty" [M86 p. 44]; gloms are destroyed by Bursters and Dissolvers [M86 p. 70])."""
        return self._members.pop(name)

    def find(self, name: str):
        """The structure called `name`, or None when it is not in the arena
        (DECISION D-009)."""
        return self._members.get(name)


class World:
    """Everything of one run [M86 pp. 42-47], made fresh from a `Config` and a seed.

    Fields: `config`, `seed`, `rng` (the run's only generator); `events`, the `Subject` the
    run publishes to; `clock`, how many tasks have run; `terms`, the terms entered so far;
    `cytoplasm`, `socratoplasm` (`Arena`s) and `platoplasm` (`Platoplasm`); `counters`, one
    per named kind (DECISION D-014) plus `task`; `taskrack`; `handlers`, the task functions
    by type; and the shared state the later modules fill: `hypothesis`, `template`,
    `reigning` (the reigning class [M86 p. 99]), `threshold` (the bond-strength threshold
    [M86 p. 105]), `freeze_dried` (the old hypotheses [M86 p. 91]) and `cast_failures`
    (the Gnoth-caster failures of the current reformulation episode, DECISION D-050);
    `answerer`, whoever
    answers a guess (DECISION D-064), and `done`, set when the user accepts a guess
    (`bye` [M86 p. 166]). `urgencies` is `Config.urgencies` as a dictionary
    (DECISION D-008).
    """

    def __init__(self, config: Config | None = None, seed: int = 0, subject=None):
        self.config = config if config is not None else Config()
        self.seed = seed
        self.rng = Rng(seed)
        self.events = subject if subject is not None else events.Subject()
        self.clock = 0
        self.terms: list = []
        self.cytoplasm = Arena("cytoplasm")
        self.socratoplasm = Arena("socratoplasm")
        self.platoplasm = Platoplasm(self.config)
        self.counters: dict = {kind: 0 for kind in NAMED_KINDS}
        self.counters["task"] = 0
        self.taskrack = Taskrack(self.config.urgency_power)
        self.urgencies = dict(self.config.urgencies)
        self.handlers: dict = {}
        self.hypothesis = None
        self.template = None
        self.reigning = None
        self.threshold = None
        self.freeze_dried: list = []
        self.cast_failures = 0
        self.answerer = None
        self.done = False

    # ---- names and existence -------------------------------------------------------------

    def name(self, kind: str) -> str:
        """The next name of a kind, `glint1`, `glint2`, ...: one counter per kind per run,
        never reusing a number (DECISION D-014)."""
        if kind not in NAMED_KINDS:
            raise ValueError(f"unknown kind {kind!r}; the named kinds are {NAMED_KINDS}")
        self.counters[kind] += 1
        return f"{kind}{self.counters[kind]}"

    def find(self, name: str):
        """The named structure in the cytoplasm or the socratoplasm, or None
        (DECISION D-009)."""
        found = self.cytoplasm.find(name)
        return found if found is not None else self.socratoplasm.find(name)

    def exists(self, *names: str) -> bool:
        """Whether every named structure is still in the world: the check a task makes
        before acting, "to see that both gloms still exist" [M86 p. 64] (DECISION D-009)."""
        return all(self.find(name) is not None for name in names)

    # ---- the taskrack ------------------------------------------------------------------

    def post(self, type: str, *args, urgency: int | None = None) -> Task:
        """Set out a task: "place them on the taskrack where they will stay until chosen and
        run" [M86 p. 41]. The urgency is the type's from `Config.urgencies` (DECISION D-008)
        unless given (Sparkler-plus, DECISION D-015; Glomtester, DECISION D-020)."""
        if type not in TASK_TYPES:
            raise ValueError(f"unknown task type {type!r}")
        if urgency is None:
            urgency = self.urgencies[type]
        self.counters["task"] += 1
        task = Task(self.counters["task"], type, tuple(args), urgency, ARENAS[type],
                    self.clock, len(self.terms))
        self.taskrack.post(task)
        self.events.publish(events.TaskPosted(task.number, task.type, task.args,
                                              task.urgency, task.arena, task.created_at))
        return task

    def clean(self, moment: str) -> int:
        """Remove old tasks at one of the three cleaning moments (DECISION D-010): `term`
        (the types of `cleaned_on_term` posted before the previous term), `reform` (the
        reformulation types) or `hypothesis` (the template types); returns how many."""
        if moment == "term":
            removed = self.taskrack.clean(self.config.cleaned_on_term,
                                          before_term=len(self.terms) - 1)
        elif moment == "reform":
            removed = self.taskrack.clean(self.config.cleaned_on_reform)
        elif moment == "hypothesis":
            removed = self.taskrack.clean(self.config.cleaned_on_hypothesis)
        else:
            raise ValueError(f"unknown cleaning moment {moment!r}")
        if removed:
            counts: dict = {}
            for task in removed:
                counts[task.type] = counts.get(task.type, 0) + 1
            self.events.publish(events.TasksCleaned(moment, tuple(counts.items())))
        return len(removed)

    def run_task(self, task: Task) -> None:
        """Run one task chosen from the rack: advance the clock, publish `TaskChosen`, and
        call the type's handler with the world and the task's arguments [M86 p. 41]. A type
        with no handler is an error of the program, not a stale task."""
        if task.type not in self.handlers:
            raise LookupError(f"no handler for task type {task.type!r}")
        self.clock += 1
        self.events.publish(events.TaskChosen(self.clock, task.number, task.type, task.args,
                                              task.urgency, task.arena))
        self.handlers[task.type](self, *task.args)

    def run_now(self, type: str, *args) -> Task:
        """Set out a task and run it at once, ahead of any draw: the Hfilter of a new term
        (`doing task Hfilter` is the first task line after a term entered under a hypothesis
        in all six such places in the transcripts [M86 pp. 166, 168, 172, 175, 176, 179];
        DECISION D-012). The task is published as posted and as chosen, so the trace reads as
        for any other task, and the clock advances; the rack and the `Rng` are untouched."""
        task = self.post(type, *args)
        self.taskrack.remove(task)
        self.run_task(task)
        return task

    def stale(self, type: str, args: tuple, reason: str) -> None:
        """Report that a task found its structures gone and did nothing [M86 p. 42]
        (DECISION D-009); the few tasks that print a line do so themselves."""
        self.events.publish(events.TaskStale(type, tuple(args), reason))

    # ---- terms and messages ----------------------------------------------------------------

    def enter_term(self, term: int) -> int:
        """Record a new term of the sequence: the terms are "nonnegative integers,
        presented one term at a time" [MHN p. 3]. Cleans the rack (DECISION D-010) and
        publishes `TermEntered`; returns the term's number from 1. The glint and the tasks
        a new term releases are made by the cytoplasm modules."""
        if isinstance(term, bool) or not isinstance(term, int) or term < 0:
            raise ValueError(f"a term is a nonnegative integer, got {term!r}")
        self.terms.append(term)
        cleaned = self.clean("term")
        self.events.publish(events.TermEntered(len(self.terms), term, cleaned))
        return len(self.terms)

    def say(self, text: str) -> None:
        """Publish one printed line as the transcripts print it [M86 pp. 164-179]."""
        self.events.publish(events.Message(text))

    def config_pairs(self) -> tuple:
        """The configuration as (field, value) pairs, for the `RunStarted` event
        (DECISION D-069: a run is its seed and its configuration)."""
        return tuple((f.name, getattr(self.config, f.name)) for f in fields(self.config))
