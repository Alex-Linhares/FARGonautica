"""The three appendix runs, plain, as the reference every GUI observer is compared with
(Numbo GUI, `test_run_controller.py`'s `reference`): Meredith's inputs on the golden seeds
of tests/data/ [M86 pp. 164-179], the gate's cases. Not a test file: the test files and
the conftest import it.
"""

import io
from dataclasses import dataclass

from seekwhence import bench
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.session import Session
from seekwhence.trace import event_record

# The appendix runs on the golden seeds of tests/data/ (the gate's cases).
APPENDIX = (("run1_3737", 54), ("run2_373373", 54), ("run3_121231234", 30))


@dataclass
class Reference:
    """One appendix run, plain: its inputs and what a `Session` alone gives."""
    run_id: str
    seed: int
    config: Config
    lines: list
    answers: list
    events: list
    records: list
    text: str
    draws: int
    outcome: str
    show: list          # Session.show_lines() at the end
    summary: str


def appendix_inputs(run_id):
    """The config, console lines and answers of an appendix run (`bench`)."""
    spec = bench.run_spec(bench.load_expectations(), run_id)
    return (Config(verbose=spec["prints_task_lines"]), bench.inputs_of(spec),
            bench.answers_of(spec))


def run_plain(run_id, seed, observers=()):
    """An appendix run with extra observers; returns the `Reference`."""
    config, lines, answers = appendix_inputs(run_id)
    rec, out = Recorder(), io.StringIO()
    session = Session(config, seed, out=out, lines=list(lines), answers=list(answers),
                      observers=[rec, *observers])
    session.run()
    return Reference(run_id, seed, config, list(lines), list(answers), rec.events,
                     [event_record(e) for e in rec.events], out.getvalue(),
                     session.world.rng.draws, session.outcome, session.show_lines(),
                     session.summary())
