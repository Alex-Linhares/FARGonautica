"""The taskrack: tasks wait on it "until chosen and run" [M86 p. 41].

"All operations are carried out in task series, which run in simulated parallel. The tasks
comprising various series are chosen at random for processing, so no assumptions can be made
about which of two competing tasks will run first" [M86 p. 41]. "All tasks have 'urgencies'
(integer weights), and more urgent tasks will have a greater chance of being chosen than less
urgent ones do" [M86 p. 42]. "At certain times some cleaning is done (removing old tasks of
certain types)" [M86 p. 42]. This module holds the task record and the rack with those three
operations: posting, the urgency-weighted draw (DECISION D-007) and cleaning (DECISION D-010).
The rack holds no generator and no clock: the run's `Rng` is passed to `choose`, and the
`World` stamps each task with the time it was posted. What a task does when run is the
business of the module that defines it; the rack knows only the record.
"""

from dataclasses import dataclass

from seekwhence.config import TASK_TYPES

# DECISION D-057: the arena each task type operates in; "Each task operates in one of three
# arenas: conceptual (platoplasm), short-term analytical (socratoplasm), or perceptual
# (cytoplasm)" [MHN p. 3]. Call-term and empty-task talk to the user and belong to none.
ARENAS = {
    "Call-term": "user", "empty-task": "user",
    "Sparkler": "cytoplasm", "Sparkler-plus": "cytoplasm", "Tester": "cytoplasm",
    "Bonder": "cytoplasm", "Glom-scout": "cytoplasm", "Glomtester": "cytoplasm",
    "Glommer": "cytoplasm", "Glom-inspector": "cytoplasm", "Burster": "cytoplasm",
    "Dissolver": "cytoplasm",
    "Template-scout": "cytoplasm", "Template-applier": "cytoplasm",
    "Template-resolver": "cytoplasm", "Template-evaluator": "cytoplasm",
    "Template-reviewer": "cytoplasm",
    "Plato-scout": "platoplasm", "Plato-evaluator": "platoplasm", "Slip-scout": "platoplasm",
    "Gnoth-maker": "socratoplasm", "Hypothesizer": "socratoplasm", "Hfilter": "socratoplasm",
    "Gnoth-setter": "socratoplasm", "Reformulator": "socratoplasm",
    "Gnoth-operator": "socratoplasm", "Capper": "socratoplasm",
    "Bond-assessor": "socratoplasm", "Gnoth-caster": "socratoplasm",
}
assert set(ARENAS) == set(TASK_TYPES)


@dataclass(frozen=True)
class Task:
    """One task on the rack: "an uninterruptible (and generally small) segment of a
    computational process" [M86 p. 182].

    Fields: `number`, its ordinal among the tasks posted in the run (never reused);
    `type`, one of `TASK_TYPES` as the transcripts print it (`Sparkler-plus`); `args`, the
    names and numbers it was given (the transcripts' `(glint2 glint1 10)` [M86 p. 164]);
    `urgency`, its integer weight [M86 p. 42]; `arena` (DECISION D-057); `created_at`, the
    run's clock (tasks run so far) when it was posted; `at_term`, how many terms had been
    entered then, which the cleaning of DECISION D-010 reads.
    """
    number: int
    type: str
    args: tuple
    urgency: int
    arena: str
    created_at: int
    at_term: int

    def __post_init__(self):
        if self.type not in TASK_TYPES:
            raise ValueError(f"unknown task type {self.type!r}")
        if isinstance(self.urgency, bool) or not isinstance(self.urgency, int) \
                or self.urgency < 1:
            raise ValueError(f"an urgency is a positive integer weight [M86 p. 42], "
                             f"got {self.urgency!r}")
        if not isinstance(self.args, tuple):
            raise TypeError(f"args must be a tuple, got {type(self.args).__name__}")


class Taskrack:
    """The rack of waiting tasks [M86 pp. 41-42]: post, choose, clean.

    Tasks are kept in posting order; the order matters only to make the same seed give the
    same draw (DECISION D-007). `urgency_power` is the exponent of DECISION D-007
    (`Config.urgency_power`): a task's weight is its urgency raised to it.
    """

    def __init__(self, urgency_power: int = 1):
        if isinstance(urgency_power, bool) or not isinstance(urgency_power, int) \
                or urgency_power < 0:
            raise ValueError(f"urgency_power must be a non-negative integer, "
                             f"got {urgency_power!r}")
        self.urgency_power = urgency_power
        self._tasks: list = []

    def __len__(self) -> int:
        return len(self._tasks)

    @property
    def tasks(self) -> tuple:
        """The waiting tasks in posting order [M86 p. 41]."""
        return tuple(self._tasks)

    def post(self, task: Task) -> Task:
        """Place a task on the rack, "where [it] will stay until chosen and run"
        [M86 p. 41]."""
        if not isinstance(task, Task):
            raise TypeError(f"not a Task: {task!r}")
        self._tasks.append(task)
        return task

    def remove(self, task: Task) -> None:
        """Take one named task off the rack without a draw (`World.run_now`, DECISION
        D-012); an error if it is not waiting."""
        self._tasks.remove(task)

    def weights(self) -> list:
        """Each waiting task's weight, its urgency raised to `urgency_power`
        (DECISION D-007)."""
        return [task.urgency ** self.urgency_power for task in self._tasks]

    def choose(self, rng) -> Task:
        """Take one task off the rack, chosen with probability proportional to its weight
        in one draw from the run's `Rng` (DECISION D-007; "more urgent tasks will have a
        greater chance of being chosen" [M86 p. 42])."""
        if not self._tasks:
            raise LookupError("choose from an empty taskrack")
        position = rng.weighted_index(self.weights())
        return self._tasks.pop(position)

    def clean(self, types, before_term: int | None = None) -> list:
        """Remove the waiting tasks of the given `types`, all of them or only those posted
        when fewer than `before_term` terms had been entered; returns the removed tasks in
        rack order ("removing old tasks of certain types" [M86 p. 42]; DECISION D-010)."""
        types = set(types)
        removed = [t for t in self._tasks if t.type in types
                   and (before_term is None or t.at_term < before_term)]
        if removed:
            gone = set(id(t) for t in removed)
            self._tasks = [t for t in self._tasks if id(t) not in gone]
        return removed

    def count_by_type(self) -> dict:
        """How many tasks of each type are waiting, in order of first appearance
        [M86 p. 41]."""
        counts: dict = {}
        for task in self._tasks:
            counts[task.type] = counts.get(task.type, 0) + 1
        return counts
