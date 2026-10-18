"""Whole runs: `run_problem(problem, config, seed)` builds a fresh `World`, steps it until a
find-answer codelet runs or the cap stops it, and returns a `Result`; `run_batch` runs a
problem over the seeds of a batch (D-061 to D-064, D-066).

"At the end of each run, Tabletop indicates the object touched by Eliza, the sequence of
structures that led to that answer, the amount of resources expended, and the type and
quality of structures present at the end of the run." [F95 p. 145] FCCA's monitor "recorded
the answer given, the final structure-value, and the run-length (i.e., total number of
codelets)" [FCCA p. 392]. A `Result` carries those, and the run's seed and profile so that
it can be replayed alone. "Almost all of the configurations were run fifty times." [F95
p. 114]: a batch takes its runs' seeds from one batch seed (`rng.run_seeds`, D-064).
"""

from dataclasses import dataclass

from tabletop import observe as OB
from tabletop import worldview as WV
from tabletop.config import PROFILES, Config
from tabletop.problems import Problem, load_problem
from tabletop.accel import ENGINE, run_seeds
from tabletop.world import World


@dataclass(frozen=True)
class Result:
    """What one run reports [F95 p. 145; FCCA p. 392] (D-057, D-061, D-062, D-066): the
    `problem` name, `seed` and `profile`, the `engine` that ran it (`compiled` or `python`,
    D-107), with the fields `changed` from `standard`; the
    `answer` id, its `label` (`E1`, empty when none), whether it was `literal` (D-063),
    `through` (`correspondence`, `group`, `default`) and the correspondence it was read off
    (`via`); `capped` (D-061); the `structure_value` (D-062), the `codelets` run and the
    final `temperature`; the `worldview` as (name, end, end) tuples; the counts of `groups`
    and `correspondences` in the Workspace, of `competitions` and `changes`; `draws`,
    the generator's draw count at the end (D-064); and `observer_errors`, the (observer,
    event, error) triples of the observers that raised (D-065), empty when none did."""

    problem: str
    seed: int
    profile: str
    engine: str
    changed: tuple
    answer: str
    label: str
    literal: bool
    through: str
    via: str
    capped: bool
    structure_value: float
    codelets: int
    temperature: float
    worldview: tuple
    groups: int
    correspondences: int
    competitions: int
    changes: int
    draws: int
    observer_errors: tuple = ()


def result_of(world: World) -> Result:
    """The `Result` of a finished world (D-062)."""
    ws, answer = world.workspace, world.answer
    changed = tuple(sorted(world.config.differences(PROFILES["standard"]).items()))
    return Result(problem=world.problem.name, seed=world.seed, profile=world.config.profile,
                  engine=ENGINE, changed=changed, answer=answer.object, label=answer.label,
                  literal=answer.literal, through=answer.through, via=answer.via,
                  capped=world.capped, structure_value=world.structure_value(),
                  codelets=world.clock, temperature=world.temperature.value,
                  worldview=tuple(WV.summary(world)), groups=len(ws.groups),
                  correspondences=len(ws.correspondences), competitions=len(ws.competitions),
                  changes=len(ws.changes), draws=world.rng.draws,
                  observer_errors=tuple(world.events.errors))


def run_problem(problem: Problem, config: Config, seed: int, observers=()) -> Result:
    """One whole run from a fresh world (D-064, D-066): codelets run "until, eventually, a
    codelet will run that says, 'OK, stop and designate as answer whatever is currently
    mapped onto the touched object.'" [F95 p. 55], or the cap (D-061). Same arguments,
    same result, with any `observers` attached (D-065: they receive every event and cannot
    change the run)."""
    world = World(problem, config, seed, observers)
    while not world.finished:
        world.step()
    return result_of(world)


def fingerprint(problem: str, seed: int, profile: str = "standard") -> tuple:
    """The SHA-256 hex digest of a run's JSON-lines trace and the generator's draw count
    at its end (D-064, D-065; `docs/trace-format.md`): the run of the named problem (a
    file stem of `problems/`, `surround-05`) with this seed under this profile, with the
    hashing observer attached and no other. Same arguments, same result, in any process."""
    hasher = OB.Hasher()
    result = run_problem(load_problem(problem), PROFILES[profile], seed, [hasher])
    return hasher.digest(), result.draws


def trace_lines(problem: Problem, config: Config, seed: int, quiet: bool = False) -> list:
    """The text trace of one run as a list of lines (D-065; `docs/trace-format.md`), for
    the tests' golden traces; nothing is printed."""
    trace = OB.TextTrace(stream=_Null(), quiet=quiet)
    run_problem(problem, config, seed, [trace])
    return trace.lines


class _Null:
    def write(self, text: str) -> None:
        pass


def run_batch(problem: Problem, config: Config, batch_seed: int, count: int) -> list:
    """`count` runs of a problem, each from its own seed drawn from `batch_seed`
    (`rng.run_seeds`, D-064), as French ran each configuration "fifty times" [F95 p. 114].
    Any run of the batch can be replayed alone from its `Result.seed`."""
    return [run_problem(problem, config, seed) for seed in run_seeds(batch_seed, count)]


def report_line(result: Result) -> str:
    """The last line of the text trace, in the style of French's reports [F95 p. 145]
    (`docs/trace-format.md`): `Eliza touches small-glass3 (E1). Structure 186. Codelets 64.
    T 2.0.`, with `literal` after a literal answer and `(capped)` after the count of a
    capped run (D-061, D-063)."""
    return OB.report_sentence(result.answer, result.label, result.literal, result.capped,
                              result.structure_value, result.codelets, result.temperature)
