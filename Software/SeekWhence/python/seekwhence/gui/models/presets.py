"""The inputs of a run, and the presets that fill them (Numbo GUI, `controls.py`'s
puzzles; PANES.md P11; G-009).

A run is its seed and its configuration (DECISION D-069), plus, for Seek-Whence, the
console lines the user types and the answers to the guesses [M86 pp. 164-179]. `RunInputs`
holds all of that as plain data; the window starts a `Session` from it and the session
file (item 13) saves it. This item (02) gives the three appendix runs with Meredith's
inputs, commands included, and her answers, on the golden seeds of `tests/data/`
(`bench.inputs_of`, `bench.answers_of`); item 03 adds the interactive run, whose terms
the user types in the sequence pane (`interactive_inputs`, G-015); item 14 adds the
Blackburn dozen and the other lists of `seekwhence.sequences` (`sequence_inputs`), fed
as the bench feeds them (DECISION D-070), and the `continuation` a run's guesses are
judged against (G-048). Qt-free.
"""

import dataclasses

from seekwhence import bench, sequences
from seekwhence.config import Config

__all__ = ["APPENDIX_SEEDS", "DEFAULT_PRESET", "INTERACTIVE", "SEQUENCE_TERMS", "RunInputs",
           "appendix_ids", "appendix_inputs", "custom_inputs", "interactive_inputs",
           "sequence_inputs", "sequence_title"]

# The appendix runs and their golden seeds (tests/data/; PANES.md P11) [M86 pp. 164-179].
APPENDIX_SEEDS = {"run1_3737": 54, "run2_373373": 54, "run3_121231234": 30}
DEFAULT_PRESET = "run1_3737"
# The preset id of a run whose terms the user types in the GUI and whose guesses the user
# answers (PANES.md P1; G-015).
INTERACTIVE = "interactive"
# How many terms of a catalogue sequence are fed, the bench's `--terms` default (DECISION
# D-070; G-048).
SEQUENCE_TERMS = 16

# Their titles, as the thesis names the sequences [M86 pp. 164, 167, 170].
_TITLES = {"run1_3737": "run 1: 3 7 3 7 [M86 pp. 164-166]",
           "run2_373373": "run 2: 3 7 3 3 7 3 [M86 pp. 167-169]",
           "run3_121231234": "run 3: 1 2 1 2 3 1 2 3 4 [M86 pp. 170-179]"}


@dataclasses.dataclass(frozen=True)
class RunInputs:
    """Everything a run is started from (G-009): the `preset` id (`custom` for free
    terms), the `Config`, the `seed`, the scripted console `lines` (terms and commands) or
    None for a console stream, the scripted `answers`, whether guesses are `interactive`
    (answered by the user in the GUI, G-008) rather than scripted or judged, and a
    `title` for the window."""
    preset: str
    config: Config
    seed: int
    lines: tuple | None
    answers: tuple
    interactive: bool = False
    title: str = ""
    # The true continuation the guesses are judged against, the bench's knowing user
    # (`bench.ContinuationAnswerer`, DECISION D-070), or None for the session's own
    # judgement (G-048).
    continuation: tuple | None = None


def appendix_ids() -> tuple:
    """The ids of the three appendix runs, in the thesis's order [M86 pp. 164-179]."""
    return tuple(APPENDIX_SEEDS)


def appendix_inputs(run_id: str, seed=None, interactive=False) -> RunInputs:
    """The inputs of an appendix run: Meredith's console lines and answers from
    `tests/data/expectations.json` [M86 pp. 164-179], verbose or terse as the printout is
    (DECISION D-055), on its golden seed unless `seed` is given (G-009)."""
    spec = bench.run_spec(bench.load_expectations(), run_id)
    return RunInputs(preset=run_id, config=Config(verbose=spec["prints_task_lines"]),
                     seed=APPENDIX_SEEDS[run_id] if seed is None else int(seed),
                     lines=tuple(bench.inputs_of(spec)), answers=tuple(bench.answers_of(spec)),
                     interactive=interactive, title=_TITLES[run_id])


def custom_inputs(config, seed, lines, answers=(), interactive=False) -> RunInputs:
    """Inputs given directly, as the determinism contract and the tests give them
    (TASK.md; G-009)."""
    return RunInputs(preset="custom", config=config, seed=int(seed),
                     lines=None if lines is None else tuple(lines), answers=tuple(answers),
                     interactive=interactive,
                     title="custom" if lines is None else " ".join(str(x) for x in lines))


def interactive_inputs(seed, config=None) -> RunInputs:
    """The inputs of a run the user drives: no scripted lines (the terms and commands come
    from the sequence pane's box, through a `TermFeed`, G-015) and guesses answered in the
    guess panel (G-008); "a user presents to the system numbers" [M86 p. 37]."""
    return RunInputs(preset=INTERACTIVE, config=Config() if config is None else config,
                     seed=int(seed), lines=None, answers=(), interactive=True,
                     title="interactive: type the terms yourself")


def sequence_title(seq) -> str:
    """`blackburn-2: 1 2 3 4 5 6 7`, a catalogue sequence as the presets name it, the
    terms the source prints (DECISION D-070; G-048)."""
    return f"{seq.name}: {seq.printed}"


def sequence_inputs(name: str, seed=1, judged=True, config=None,
                    n_terms=SEQUENCE_TERMS, interactive=False) -> RunInputs:
    """The inputs of a run on a sequence of `seekwhence.sequences` [M86 pp. 6-7, 38, 88,
    145-150; FCCA pp. 53-54]: its first `n_terms` terms fed one at a time as the bench
    feeds them, the guesses judged against the true continuation when `judged` (the
    bench's rule, DECISION D-070), else by the session against the terms fed (DECISION
    D-033); `interactive`: the user types the terms and answers (G-048). `KeyError` for
    a name not in the catalogue."""
    seq = sequences.find(name)
    lines = None if interactive else tuple(str(t) for t in seq.prefix(n_terms))
    return RunInputs(preset=seq.name, config=Config() if config is None else config,
                     seed=int(seed), lines=lines, answers=(), interactive=interactive,
                     title=sequence_title(seq),
                     continuation=None if interactive or not judged else tuple(seq.terms))
