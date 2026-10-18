"""The two traces and the fingerprint (D-065; `docs/trace-format.md`): the golden text
traces of two seeds, the JSON-lines records, and the determinism across processes."""

import io
import json
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel
from tabletop import events as EV
from tabletop import observe as OB
from tabletop.config import PROFILES
from tabletop.problems import load_problem
from tabletop.run import fingerprint, report_line, run_problem, trace_lines

HERE = Path(__file__).resolve().parent
# The goldens of the engine that is running (D-107): compiled/ or python/.
GOLDEN = HERE / "data" / "golden" / accel.info()["engine"]
PYTHON_DIR = HERE.parent
TRACE_FORMAT = PYTHON_DIR / "docs" / "trace-format.md"
CASES = [("surround-05", 1), ("blockage-06", 2)]
LINE = re.compile(r"^ *(\d+)  T=(\d+\.\d)  a=(\d+\.\d)  ([A-Z0-9-]+)\(([^)]*)\)(?:/(\d+))?  -> (\S+)")


@pytest.mark.parametrize("problem, seed", CASES)
def test_golden_text_trace(problem, seed):
    """The text trace of a seed is byte for byte what `tests/data/golden/ENGINE/` holds
    for the engine that is running. A change of the engine's behaviour changes these
    files on purpose, with a Re-interpretation log entry."""
    expected = (GOLDEN / f"{problem}-seed-{seed}.txt").read_text().splitlines()
    lines = trace_lines(load_problem(problem), PROFILES["standard"], seed)
    assert lines == expected


def test_golden_fingerprints():
    """The fingerprints of the gate's four cases, as the running engine's
    `tests/data/golden/ENGINE/fingerprints.json` records them (the determinism check of
    the gate uses the same cases)."""
    recorded = json.loads((GOLDEN / "fingerprints.json").read_text())
    assert len(recorded) == 4
    for key, (digest, draws) in recorded.items():
        problem, seed = key.split()
        assert fingerprint(problem, int(seed)) == (digest, draws), key


def test_text_trace_shape():
    """One line per codelet in the documented columns, a first and a last line as
    `docs/trace-format.md` says (D-065)."""
    problem, config = load_problem("surround-05"), PROFILES["standard"]
    lines = trace_lines(problem, config, 3)
    result = run_problem(problem, config, 3)
    assert lines[0] == ("tabletop surround-05  seed 3  profile standard  engine "
                        f"{accel.info()['engine']}  Henry touches cup1")
    assert lines[-1] == report_line(result)
    codelet_lines = [m for l in lines if (m := LINE.match(l))]
    assert len(codelet_lines) == result.codelets
    assert [int(m.group(1)) for m in codelet_lines] == list(range(1, result.codelets + 1))
    assert codelet_lines[0].group(2) == f"{config.initial_temperature:.1f}"
    assert codelet_lines[-1].group(4) == "FND-ANS"
    assert {m.group(7) for m in codelet_lines} <= {
        "found", "built", "fizzled", "proposed", "passed", "failed", "picked", "described",
        "entered", "fight", "removed", "answer"}
    notes = [l for l in lines[1:-1] if not LINE.match(l)]
    assert notes and all(l.startswith("      ") for l in notes)
    assert any(l.startswith("      slipnet posts") for l in notes)
    assert any(" by " in l and "->" in l for l in notes)       # a temperature change
    assert any("jolt " in l for l in lines)


def test_quiet_trace_prints_the_first_and_last_lines_only():
    problem, config = load_problem("surround-05"), PROFILES["standard"]
    quiet = trace_lines(problem, config, 3, quiet=True)
    full = trace_lines(problem, config, 3)
    assert quiet == [full[0], full[-1]]


def test_report_line_forms():
    assert OB.report_sentence("small-glass3", "E1", False, False, 186.4, 64, 2.04) == \
        "Eliza touches small-glass3 (E1). Structure 186. Codelets 64. T 2.0."
    assert OB.report_sentence("cup1", "", True, True, 0, 3000, 80.0) == \
        "Eliza touches cup1 (none) literal. Structure 0. Codelets 3000 (capped). T 80.0."


def test_codelet_text():
    assert OB.codelet_text("find-neighbor", ("fork2",), 2) == "FD-NNBR(fork2)/2"
    assert OB.codelet_text("find-group", ()) == "FND-GP()"


def test_text_trace_survives_a_closed_stream():
    class Closed(io.StringIO):
        def write(self, text):
            raise BrokenPipeError()

    trace = OB.TextTrace(Closed())
    result = run_problem(load_problem("blockage-01"), PROFILES["standard"], 1, [trace])
    assert trace.stream is None and trace.lines[-1] == report_line(result)
    assert not result.observer_errors


# ---- the JSON-lines trace ----------------------------------------------------------------------


def test_json_lines_records():
    stream = io.StringIO()
    observer = OB.JsonLines(stream)
    result = run_problem(load_problem("surround-05"), PROFILES["standard"], 3, [observer])
    lines = stream.getvalue().splitlines()
    assert len(lines) == observer.count
    records = [json.loads(l) for l in lines]
    assert [list(r)[:3] for r in records] == [["event", "clock", "draws"]] * len(records)
    assert records[0]["event"] == "RunStarted" and records[-1]["event"] == "RunEnded"
    assert records[0]["problem"] == "surround-05" and records[0]["changed"] == []
    assert records[-1]["answer"] == result.answer and records[-1]["draws"] == result.draws
    for line in lines:
        assert json.dumps(json.loads(line), separators=(",", ":")) == line
    names = {cls.__name__ for cls in EV.EVENT_TYPES}
    assert {r["event"] for r in records} <= names
    for r in records:
        cls = getattr(EV, r["event"])
        assert list(r)[3:] == [f.name for f in __import__("dataclasses").fields(cls)][2:]


def test_json_line_writes_floats_with_repr_and_tuples_as_lists():
    event = EV.SlipnetUpdated(clock=1, draws=2, changed=(("cup", 51.0), ("opposite", 138.7)),
                              lengths=())
    assert OB.json_line(event) == \
        '{"event":"SlipnetUpdated","clock":1,"draws":2,"changed":[["cup",51.0],["opposite",138.7]],"lengths":[]}'


def test_the_documented_example_records_are_real():
    """The example lines of `docs/trace-format.md` are the first records of the run they
    name, verbatim."""
    text = TRACE_FORMAT.read_text()
    block = text[text.index('{"event":"RunStarted"'):]
    # the document shows the reference engine's records; the running engine names itself
    examples = [l.replace('"engine":"compiled"', f'"engine":"{accel.ENGINE}"')
                for l in block.splitlines() if l.startswith("{")][:4]
    first = json.loads(examples[0])
    stream = io.StringIO()
    run_problem(load_problem(first["problem"]), PROFILES[first["profile"]], first["seed"],
                [OB.JsonLines(stream)])
    assert stream.getvalue().splitlines()[:len(examples)] == examples


# ---- the fingerprint ---------------------------------------------------------------------------


def test_fingerprint_is_the_hash_of_the_json_lines_and_the_draw_count():
    stream = io.StringIO()
    observer = OB.JsonLines(stream)
    result = run_problem(load_problem("surround-05"), PROFILES["standard"], 3, [observer])
    import hashlib
    digest = hashlib.sha256(stream.getvalue().encode("utf-8")).hexdigest()
    assert fingerprint("surround-05", 3) == (digest, result.draws)


def test_fingerprint_differs_across_seeds_and_profiles_and_is_stable_in_process():
    a, b = fingerprint("surround-05", 3), fingerprint("surround-05", 4)
    assert a != b and a == fingerprint("surround-05", 3)
    assert fingerprint("surround-05", 3, "french-faults") != a
    # The profile's name is in the first record, so even a profile whose knobs equal
    # `standard`'s (`buridan`, D-076) gives another digest; `human` differs in a knob too.
    assert fingerprint("surround-05", 3, "buridan")[0] != a[0]
    assert fingerprint("surround-05", 3, "human")[0] != a[0]


def test_fingerprint_agrees_across_processes_with_different_hash_seeds():
    """Two processes with different `PYTHONHASHSEED` give the same fingerprint and draw
    count (TASK.md's contract; the gate's check 12 runs the same comparison)."""
    code = ("from tabletop.run import fingerprint; import json; "
            "print(json.dumps(fingerprint('surround-05', 3)))")
    outputs = []
    for hashseed in ("1", "4242"):
        env = dict(os.environ, PYTHONHASHSEED=hashseed)
        proc = subprocess.run([sys.executable, "-c", code], cwd=PYTHON_DIR, env=env,
                              capture_output=True, text=True, timeout=120)
        assert proc.returncode == 0, proc.stderr
        outputs.append(json.loads(proc.stdout))
    assert outputs[0] == outputs[1] == list(fingerprint("surround-05", 3))
