"""The run loop: run tasks until the program asks for the next term [M86 pp. 41-42, 125].

"A particular task may create, access, or modify some data structure, may request information
from the user, or may set out other tasks" [M86 p. 41]. The loop takes one task at a time from
the taskrack, chosen by urgency through the run's `Rng` (DECISION D-007), runs it, and stops
to prompt when a `Call-term` task has run ("At this point, the system typically calls for the
next term in order to test its new hypothesis" [M86 p. 125]; DECISION D-012), when the rack is
empty (`doing task empty-task`, DECISION D-013) or when the per-term cap is reached
(DECISION D-011). A whole-run cap ends the run as "capped" (DECISION D-011); the program never
quits on its own. The loop is: choose, run, count, stop.

`new_world` makes a `World` with every task type's handler installed; the modules of the
cytoplasm, the templates, the gnoths and the reformulation add theirs to `TASKS` as they are
built, so this module is the one place that knows all of them. A type whose module were
not built would get a placeholder that only reports itself stale (`NOT_BUILT`, empty now
that the casting module completes the set). `enter_term` is
likewise the one place that knows every level a new term passes through.
"""

from seekwhence import (bonds, casting, events, filtering, gloms, gnoths, operations,
                        reforms, slipping, templates)
from seekwhence.config import TASK_TYPES, Config
from seekwhence.taskrack import ARENAS
from seekwhence.world import World


def call_term(world: World) -> None:
    """The `Call-term` task: ask the user for the next term [M86 pp. 165, 171, 174, 178].
    The task itself does nothing to the world; the run loop stops to prompt when it has run
    (DECISION D-012), and the session or the command line does the asking."""
    del world


# The task handlers by type: this module's Call-term and each module's own table (SPEC 2.4).
TASKS = {"Call-term": call_term, **bonds.TASKS, **gloms.TASKS, **templates.TASKS,
         **gnoths.TASKS, **filtering.TASKS, **slipping.TASKS, **operations.TASKS,
         **reforms.TASKS, **casting.TASKS}

# The task types whose modules are not built yet (SPEC 2.4): none, now that every module is
# built; a task of such a type would be set out and, when run, do nothing but report itself
# stale. The mechanism is kept for the tests and for any later extension.
NOT_BUILT = tuple(t for t in TASK_TYPES if t not in TASKS and t != "empty-task")


def not_built(type: str):
    """A placeholder handler for a task type whose module a later item builds: it publishes
    `TaskStale` with the reason "not built yet" and changes nothing (DECISION D-009 applies
    to it as to any task that finds nothing to do)."""
    def handler(world: World, *args) -> None:
        world.stale(type, args, "not built yet")
    handler.__name__ = f"not_built_{type}"
    return handler


def new_world(config: Config | None = None, seed: int = 0, observers=(),
              answerer=None) -> World:
    """A fresh `World` for one run with every known task handler installed, the
    observers subscribed and the `answerer` that answers guesses (a `ScriptedAnswerer`
    with no script, which accepts, when none is given; DECISION D-064); publishes
    `RunStarted` (`-> (startup)` [M86 p. 164]). A run is made from its configuration and
    its seed alone."""
    world = World(config, seed)
    world.handlers.update({type: not_built(type) for type in NOT_BUILT})
    world.handlers.update(TASKS)
    world.answerer = answerer if answerer is not None else filtering.ScriptedAnswerer()
    for observer in observers:
        world.events.subscribe(observer)
    world.events.publish(events.RunStarted(seed, world.config_pairs()))
    return world


def enter_term(world: World, term: int):
    """Enter one term of the sequence: the cytoplasm's work (`bonds.new_term`: the glint and
    the tasks a term releases [M86 pp. 61, 164]) and then the top-down checks, in order of
    level: the term is filtered past the template while one is working and no hypothesis
    exists ("until a hypothesis exists, all new terms will be 'filtered' past the template"
    [M86 p. 74]; `templates.filter_term`); and once a hypothesis exists an Hfilter task is
    set out and run at once, before any other task is drawn (`doing task Hfilter` is the
    first task line after the term in all six such places in the transcripts [M86 pp. 166,
    168, 172, 175, 176, 179]; `filtering.hfilter`; DECISION D-012 as amended in item 20).
    The cytoplasm's tasks for the term stay on the rack and run after it, as run 2's sparks
    13-17 and run 3's Sparkler-plus lines follow the filtering line [M86 pp. 168, 172]. A
    term entered after the user accepted a guess is recorded but sets out no Hfilter.
    Returns the glint."""
    glint = bonds.new_term(world, term)
    templates.filter_term(world)
    if world.hypothesis is not None and not world.done:
        world.run_now("Hfilter")
    return glint


def run_until_prompt(world: World) -> str:
    """Run tasks until the program should ask for the next term, and say why: `call-term`
    (DECISION D-012), `empty` (DECISION D-013), `term-cap` or `capped` (DECISION D-011), or
    `done` when the user has accepted a guess (`bye` [M86 p. 166]; nothing is run then).

    "The tasks comprising various series are chosen at random for processing, so no
    assumptions can be made about which of two competing tasks will run first" [M86 p. 41].
    Every stop but `capped` publishes `TermRequested`; `capped` publishes `RunCapped` and
    leaves the rack as it is.
    """
    config = world.config
    ran = 0
    while True:
        if world.done:
            return "done"
        if world.clock >= config.max_tasks:
            world.events.publish(events.RunCapped(world.clock))
            return "capped"
        if ran >= config.max_tasks_per_term:
            return _prompt(world, "term-cap", ran)
        if len(world.taskrack) == 0:
            world.clock += 1
            world.events.publish(events.TaskChosen(world.clock, None, "empty-task", (), 0,
                                                   ARENAS["empty-task"]))
            return _prompt(world, "empty", ran + 1)
        task = world.taskrack.choose(world.rng)
        world.run_task(task)
        ran += 1
        if task.type == "Call-term":
            return _prompt(world, "call-term", ran)


def _prompt(world: World, reason: str, ran: int) -> str:
    world.events.publish(events.TermRequested(reason, ran))
    return reason
