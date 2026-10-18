"""Loop0004 item 14: the controls pane's model (`seekwhence/gui/models/controls.py`) and
the sequence presets (`seekwhence/gui/models/presets.py`), Qt-free (G-048, G-049).

- The catalogue: the three appendix runs with Meredith's inputs and answers on their
  golden seeds [M86 pp. 164-179], Custom, then every sequence of `seekwhence.sequences`
  with its citation and reported result [M86 pp. 6-7, 145-147] (DECISION D-070).
- Every preset loads and runs to its end through the Qt-free run controller with the tap,
  at max speed, under a small per-term cap; the appendix presets end `ok` on their seeds
  and equal the plain runs.
- The answering modes: scripted (the preset's lines and answers), judged (the bench's
  `ContinuationAnswerer`, equal to `bench.run_sequence` on the same seed) and interactive
  (no lines); the caps and the trace go into the configuration (DECISIONS D-011, D-055).
- The parsers and their errors; `values_of` is the inverse of `run_inputs`; the settings
  round trip with every bad value replaced by its default; the delays; a random seed.
"""

import io

import pytest

from seekwhence import bench, sequences
from seekwhence.config import Config
from seekwhence.gui.models import controls as cm
from seekwhence.gui.models import presets
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.run_controller import RunController
from seekwhence.gui.models.tap import Tap
from seekwhence.session import OUTCOMES, Session
from seekwhence.trace import event_record

from golden import APPENDIX

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)
SMALL_CAP = "150"       # tasks per term: every preset ends in well under a second


def run_controller(inputs):
    """A run of the inputs through the Qt-free controller with the tap (as `headless`
    does): the records, the draws and the outcome."""
    log = EventLog()
    tap = Tap(log)
    controller = RunController(observers=[tap], on_session=tap.attach_session)
    answerer = None
    if inputs.continuation is not None:
        answerer = lambda console: bench.ContinuationAnswerer(console, inputs.continuation)
    controller.start_session(inputs.config, inputs.seed, lines=inputs.lines,
                             answers=inputs.answers, answerer=answerer, paused=True)
    controller.run_to_end()
    assert controller.wait(60)
    outcome = controller.outcome
    assert outcome.error is None, outcome.error
    return [event_record(e) for e in log.events], outcome.result["draws"], outcome.result["outcome"]


def plain(inputs):
    out = io.StringIO()
    holder = {}

    def make(console):
        holder["a"] = bench.ContinuationAnswerer(console, inputs.continuation)
        return holder["a"]
    session = Session(inputs.config, inputs.seed, out=out, lines=list(inputs.lines),
                      answers=list(inputs.answers),
                      answerer=make if inputs.continuation is not None else None)
    session.run()
    return ([event_record(e) for e in session.recorder.events], session.world.rng.draws,
            session.outcome, out.getvalue())


# -- the catalogue (G-048) ------------------------------------------------------------------------

def test_the_catalogue_lists_the_appendix_runs_custom_and_every_sequence():
    cat = cm.catalogue()
    ids = [e.id for e in cat]
    assert ids[:4] == [RUN1, RUN2, RUN3, cm.CUSTOM]
    assert ids[4:] == [s.name for s in sequences.CATALOGUE]
    assert [e.group for e in cat[:4]] == ["appendix"] * 3 + ["custom"]
    by_id = {e.id: e for e in cat}
    assert by_id[RUN1].seed == 54 and by_id[RUN2].seed == 54 and by_id[RUN3].seed == 30
    assert by_id[RUN1].verbose and not by_id[RUN2].verbose and by_id[RUN3].verbose
    assert by_id[RUN3].citation == '[M86 pp. 170-179]'
    b2 = by_id["blackburn-2"]
    assert b2.label == "blackburn-2: 1 2 3 4 5 6 7" and b2.group == "blackburn"
    assert b2.citation == '[M86 pp. 6-7, 145]' and b2.reported == "solved"
    assert b2.result_text == '[M86 pp. 6-7, 145]  reported: solved'
    assert "Meredith's result: solved" in b2.tooltip and '[M86 pp. 6-7, 145]' in b2.tooltip
    assert by_id["blackburn-1"].reported == "unsolved"
    assert by_id["blackburn-3"].reported == "not attempted"
    assert by_id["fcca-1"].citation == '[FCCA p. 53]' and by_id["fcca-1"].reported == ""
    assert by_id["fcca-1"].result_text == '[FCCA p. 53]'
    assert set(e.group for e in cat[4:]) == set(cm.GROUP_TITLES) == set(sequences.GROUPS)
    assert cm.entry_of("p88-5").label == "p88-5: 3 7 3 7 3 7"
    with pytest.raises(KeyError):
        cm.entry_of("no-such")


def test_preset_lines():
    assert cm.preset_lines(RUN1) == ("3", "7", "3", "show-hypothesis", "7")  # [M86 p. 165]
    assert cm.preset_lines(RUN3)[:3] == ("1", "2", "show-hypothesis")   # [M86 p. 170]
    assert "show" in cm.preset_lines(RUN3)
    assert cm.preset_lines(cm.CUSTOM) == ()
    assert cm.preset_lines("blackburn-2") == tuple(str(n) for n in range(1, 17))
    assert len(cm.preset_lines("blackburn-11")) == presets.SEQUENCE_TERMS == 16


# -- every preset runs to its end (G-048) --------------------------------------------------------------

@pytest.mark.parametrize("preset", [e.id for e in cm.catalogue()])
def test_every_preset_loads_and_runs_to_its_end(preset):
    values = cm.ControlValues(preset=preset, cap_term=SMALL_CAP, terms="3 7 3 7",
                              seed=str(cm.entry_of(preset).seed),
                              answering="scripted" if preset == cm.CUSTOM else "judged")
    inputs = cm.run_inputs(values)
    assert inputs.preset == preset and inputs.lines
    assert inputs.config.max_tasks_per_term == 150
    records, draws, outcome = run_controller(inputs)
    assert outcome in OUTCOMES
    # a run whose input ran out (`eof`) ends at its last prompt (DECISION D-069)
    assert records[0]["kind"] == "start" and records[-1]["kind"] in ("ended", "capped", "prompt")
    assert outcome != "eof" or records[-1]["kind"] == "prompt"
    if inputs.continuation is not None:
        assert inputs.continuation[:len(inputs.lines)] == tuple(int(x) for x in inputs.lines
                                                                if str(x).isdigit())
    assert (records, draws, outcome) == plain(inputs)[:3]


@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_the_appendix_presets_end_ok_on_their_seeds(appendix_runs, run_id):
    values = cm.ControlValues(preset=run_id, seed=str(SEEDS[run_id]),
                              verbose=cm.entry_of(run_id).verbose)
    inputs = cm.run_inputs(values)
    assert inputs.seed == SEEDS[run_id] and inputs.config.verbose == (run_id != RUN2)
    assert inputs.lines == tuple(appendix_runs[run_id].lines)
    assert inputs.answers == tuple(appendix_runs[run_id].answers)
    records, draws, outcome = run_controller(inputs)
    assert outcome == "ok" == appendix_runs[run_id].outcome
    assert records == appendix_runs[run_id].records and draws == appendix_runs[run_id].draws


def test_a_judged_sequence_run_equals_the_benchs(tmp_path):
    inputs = presets.sequence_inputs("blackburn-2", seed=1)
    assert inputs.continuation == tuple(sequences.find("blackburn-2").terms)
    assert inputs.lines == tuple(str(n) for n in range(1, 17)) and inputs.answers == ()
    records, draws, outcome = run_controller(inputs)
    bench_run = bench.run_sequence(sequences.find("blackburn-2"), 1, 16, Config())
    assert outcome == bench_run["outcome"]
    assert records == [event_record(e) for e in bench_run["session"].recorder.events]
    assert draws == bench_run["session"].world.rng.draws


def test_the_answering_modes():
    scripted = cm.run_inputs(cm.ControlValues(preset="blackburn-2", answering="scripted"))
    assert scripted.lines and scripted.answers == () and scripted.continuation is None
    assert not scripted.interactive
    judged = cm.run_inputs(cm.ControlValues(preset="blackburn-2", answering="judged"))
    assert judged.lines == scripted.lines and judged.continuation is not None
    assert len(judged.continuation) == 40
    interactive = cm.run_inputs(cm.ControlValues(preset="blackburn-2", answering="interactive"))
    assert interactive.lines is None and interactive.interactive
    assert interactive.continuation is None and interactive.answers == ()
    # the appendix run judged: the continuation is its terms, the answers dropped
    judged = cm.run_inputs(cm.ControlValues(preset=RUN3, seed="30", answering="judged"))
    # the printout's script stops at the accepted guess, after six terms [M86 p. 179]
    assert judged.continuation == (1, 2, 1, 2, 3, 1) and judged.answers == ()
    # custom
    custom = cm.run_inputs(cm.ControlValues(preset=cm.CUSTOM, terms="3 7 3 7 show",
                                            answering="judged"))
    assert custom.lines == ("3", "7", "3", "7", "show") and custom.continuation == (3, 7, 3, 7)
    assert custom.title == "custom: 3 7 3 7 show"
    with pytest.raises(cm.InputError):
        cm.run_inputs(cm.ControlValues(preset=cm.CUSTOM, terms="show", answering="judged"))


def test_the_caps_and_the_trace_go_into_the_configuration():
    inputs = cm.run_inputs(cm.ControlValues(cap_term="300", cap_total="4000", verbose=False))
    assert inputs.config.max_tasks_per_term == 300 and inputs.config.max_tasks == 4000
    assert not inputs.config.verbose
    assert inputs.config.urgencies == Config().urgencies
    edited = Config(guess_min_hits=4)
    inputs = cm.run_inputs(cm.ControlValues(), edited)
    assert inputs.config.guess_min_hits == 4 and inputs.config.max_tasks_per_term == 2000


# -- the parsers (Numbo GUI, run_stats.py) ------------------------------------------------------------

def test_the_parsers():
    assert cm.parse_seed(" 54 ") == 54 and cm.parse_seed(0) == 0
    assert cm.parse_cap("20") == 20
    assert cm.parse_terms("3, 7 3 7 show-parse BYE") == ("3", "7", "3", "7", "show-parse", "BYE")
    assert cm.parse_terms("") == ()
    for bad, parser in (("x", cm.parse_seed), ("-1", cm.parse_seed), ("0", cm.parse_cap),
                        ("many", cm.parse_cap), ("3 x", cm.parse_terms), ("-3", cm.parse_terms)):
        with pytest.raises(cm.InputError) as error:
            parser(bad)
        assert "Traceback" not in str(error.value)
    with pytest.raises(cm.InputError):
        cm.run_inputs(cm.ControlValues(seed="many"))
    with pytest.raises(cm.InputError):
        cm.run_inputs(cm.ControlValues(answering="telepathy"))
    with pytest.raises(cm.InputError):
        cm.run_inputs(cm.ControlValues(preset="eleventy"))


def test_values_of_is_the_inverse_of_run_inputs():
    for values in (cm.ControlValues(),
                   cm.ControlValues(preset="blackburn-2", seed="3", answering="judged",
                                    cap_term="99", verbose=False, speed=2),
                   cm.ControlValues(preset=cm.CUSTOM, terms="3 7 3 7", seed="5"),
                   cm.ControlValues(preset="fcca-3", answering="interactive")):
        assert cm.values_of(cm.run_inputs(values), values) == values
    # the window's other inputs show as what they are
    shown = cm.values_of(presets.interactive_inputs(5))
    assert shown.preset == cm.CUSTOM and shown.answering == "interactive" and shown.seed == "5"
    shown = cm.values_of(presets.appendix_inputs(RUN3, seed=7))
    assert shown.preset == RUN3 and shown.seed == "7" and shown.answering == "scripted"
    shown = cm.values_of(presets.custom_inputs(Config(), 2, [3, 7, 3, 7]))
    assert shown.preset == cm.CUSTOM and shown.terms == "3 7 3 7"
    shown = cm.values_of(presets.sequence_inputs("p88-5", 4), cm.ControlValues(speed=9))
    assert shown.preset == "p88-5" and shown.answering == "judged" and shown.speed == 9


# -- the settings round trip (G-049) ----------------------------------------------------------------

def test_the_settings_round_trip():
    values = cm.ControlValues(preset="blackburn-5", terms="1 2", seed="12", cap_term="500",
                              cap_total="9000", answering="judged", verbose=False, speed=3)
    saved = values.to_settings()
    assert set(saved) == {f"controls/{k}" for k in cm.SETTINGS_KEYS}
    assert cm.ControlValues.from_settings(saved) == values
    # as an INI gives them back: strings
    assert cm.ControlValues.from_settings({k: str(v) for k, v in saved.items()}) == values
    assert cm.ControlValues.from_settings({}) == cm.ControlValues()


def test_bad_settings_give_the_defaults():
    got = cm.ControlValues.from_settings({"controls/preset": "eleventy", "controls/seed": "many",
                                          "controls/speed": 99, "controls/answering": "x",
                                          "controls/verbose": "maybe", "controls/cap_term": "0",
                                          "controls/terms": "3 x"})
    assert got == cm.ControlValues()
    got = cm.ControlValues.from_settings({"controls/preset": RUN3, "controls/seed": "bad"})
    assert got.preset == RUN3 and got.seed == "30"
    got = cm.ControlValues.from_settings({"controls/preset": RUN2})
    assert not got.verbose and got.seed == "54"


def test_the_delays_and_a_random_seed():
    assert cm.DELAYS[0] == 1.0 and cm.DELAYS[-1] == 0 and cm.DEFAULT_DELAY in cm.DELAYS
    assert list(cm.DELAYS) == sorted(cm.DELAYS, reverse=True)
    assert cm.delay_text(0) == "max" and cm.delay_text(1.0) == "1 s"
    assert cm.delay_text(0.01) == "10 ms" and cm.delay_text(0.5) == "500 ms"

    class Rng:
        def randrange(self, a, b):
            return a + 41
    assert cm.random_seed(Rng()) == 42
    seed = cm.random_seed()
    assert isinstance(seed, int) and 1 <= seed < 1_000_000_000
