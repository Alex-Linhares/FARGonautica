"""Loop0003 item 5: session recording and replay (numbo/session.py).

A session file is JSON lines: a versioned header, one line per typed event
(observe.py, every kind), and an end line with the event count.  These tests
check:
  - every event kind goes through a session file and comes back as the same
    typed event, with the same Python types (0 is not 0.0, tuples stay
    tuples, a codelet's {"obj": ...} / {"str": ...} arguments stay dicts);
  - the recorder is an observer that changes nothing in a run, and a
    recorded run reads back as exactly the events the run published;
  - record -> replay -> the models (tree, Pnet, coderack) equal the live
    run's after every event, and an oracle trace writer fed by the replay
    writes the live run's trace byte for byte;
  - an oracle trace from lisp/tests/oracle/lib/full-run.lisp (SBCL) replays: its
    problem and seed are re-run and the re-run is checked against the trace
    line by line as it goes;
  - corrupted or truncated files, of both kinds, give a clear SessionError
    naming the file and line.
"""

import concurrent.futures
import dataclasses
import hashlib
import io
import json
import os

import pytest

import full_runs
from full_runs import PUZZLES
from numbo import harness, observe, session
from numbo.models.coderack_model import CoderackModel
from numbo.models.pnet_model import PnetModel
from numbo.models.tree_model import TreeModel
from numbo.session import SessionError, TraceMismatch
from numbo.trace import OracleTraceWriter


# ---------------------------------------------------------------------------
# Helpers

def same(a, b):
    """A and B are the same event data, with the same Python types."""
    if dataclasses.is_dataclass(a):
        return type(a) is type(b) and all(
            same(getattr(a, f.name), getattr(b, f.name)) for f in dataclasses.fields(a))
    if isinstance(a, tuple):
        return type(b) is tuple and len(a) == len(b) and all(map(same, a, b))
    if isinstance(a, dict):
        return (type(b) is dict and list(a) == list(b)
                and all(same(a[k], b[k]) for k in a))
    return type(a) is type(b) and a == b


class Recorder:
    def __init__(self):
        self.events = []

    def on_event(self, event):
        self.events.append(event)


def run(puzzle, seed, cap=20000, observers=(), rng_events=True):
    out, trace = io.StringIO(), io.StringIO()
    result = harness.run_config(list(PUZZLES[puzzle - 1]), seed=seed, max_iterations=cap,
                                out=out, trace=trace, rng_events=rng_events,
                                observers=observers)
    return result, out.getvalue(), trace.getvalue()


STEP = observe.DecompositionStep(op="PLUS114-6-V2", a="CYTO-TARGET-6-V2", va=6,
                                 b="CYTO-BLOCK120-V1", vb=120, result="CYTO-TARGET")
STEP2 = observe.DecompositionStep(op="TIMES2-3", a="X", va="-", b="Y", vb=-3, result="Z")

# One event of every kind, with awkward values.
SAMPLES = [
    observe.RunStarted(problem=(114, 11, 20, 7, 1, 6), seed=1, max_iterations=None,
                       rng="splitmix64", pnet=("ONE", "TWO")),
    observe.RngDraw(n=3, value=12345678901234567890),
    observe.SetupChoose(codelet="READ-TARGET", args=(), urgency=7,
                        rack=((600, 0), (7, 2)), draws=((1, 0.25), (2, 1))),
    observe.SetupChoose(codelet=None, args=None, urgency=None, rack=(), draws=()),
    observe.IterationBegan(n=0, x=12, temperature=48.5, rack=((7, 1), (0, 0))),
    observe.IterationBegan(n=1, x=13, temperature=0.0, rack=()),
    observe.CodeletChosen(n=4, codelet="LINK-TO-PNET",
                          args=({"obj": "cyto-node", "name": "CYTO-BRICK1"}, {"str": "a\n\"b\" é"},
                                ":KEY", None, True, (1, (2.0, "S"))),
                          urgency=4, draws=((21, 5),)),
    observe.CodeletChosen(n=None, codelet=None, args=None, urgency=None, draws=()),
    observe.CodeletPosted(codelet="KILL-NODE", args=({"obj": "cyto-node", "name": None},),
                          urgency=0),
    observe.NodeCreated(name="CYTO-BRICK1", type="2b", value=11, status=":FREE", level=10,
                        activation=0.5, success=None),
    observe.OpNodeCreated(name="PLUS6-1-V3", op="PLUS", result="CYTO-TARGET-6-V2",
                          operands=("CYTO-BRICK3", "CYTO-BRICK4"), level=5),
    observe.NodeChanged(name="CYTO-BRICK1", field="neighbors",
                        value=(("PLUS6-1-V3", "OPERAND"),)),
    observe.NodeChanged(name="CYTO-BRICK1", field="neighbors", value=None),
    observe.CurrentTargetChanged(name="CYTO-TARGET", interest=0),
    observe.Disconnect(node="CYTO-BLOCK7-V1", node_type="4bl", node_value=7, op="PLUS",
                       op_type="5g", op_value=None),
    observe.NodeKilled(name="CYTO-BLOCK7-V1", cytoplasm=("CYTO-BRICK1", "CYTO-TARGET")),
    observe.NodeKilled(name="X"),
    observe.TargetReplaced(target="CYTO-TARGET-6-V2", block="CYTO-BLOCK6-V3", rebound=False),
    observe.PnetActivations(activations=(0, 0.0, 1.5, 100)),
    observe.PnetInitialized(activations=(10, 10)),
    observe.PnodesChanged(field="instances", values=(("TWO", (("2b", "CYTO-BRICK1"),)),
                                                     ("ONE", None))),
    observe.CoderackCreated(name="CODERACK-1", levels=(600, 300, 7, 4, 1, 0)),
    observe.RackEmptied(),
    observe.Decomposition(steps=(STEP, STEP2)),
    observe.Solved(iterations=45, decomposition=(STEP,)),
    observe.GaveUp(iterations=7),
    observe.Capped(iterations=500),
    observe.RunError(iterations=3, message="The value\n  NIL\nis not of type NUMBER"),
]


def session_text(events, **kw):
    buf = io.StringIO()
    session.write_session(buf, events, **kw)
    return buf.getvalue()


# ---------------------------------------------------------------------------
# The format

def test_the_samples_cover_every_event_kind():
    assert {type(e) for e in SAMPLES} == set(observe.EVENT_TYPES.values())


def test_every_event_kind_round_trips_with_its_types():
    text = session_text(SAMPLES)
    back = list(session.read_session(io.StringIO(text)))
    assert len(back) == len(SAMPLES)
    for a, b in zip(SAMPLES, back):
        assert same(a, b), (a, b)
    # And encode/decode on their own.
    for e in SAMPLES:
        assert same(session.decode_event(json.loads(json.dumps(session.encode_event(e)))), e)


def test_the_file_has_a_versioned_header_and_an_end_line():
    lines = session_text(SAMPLES[:3]).splitlines()
    assert json.loads(lines[0]) == {"format": "numbo-session", "version": session.VERSION}
    assert session.VERSION == 1
    assert json.loads(lines[1])["kind"] == "start"
    assert json.loads(lines[-1]) == {"end": "numbo-session", "events": 3}
    assert len(lines) == 5
    # One event per line, its kind first, then its fields in order.
    assert list(json.loads(lines[2])) == ["kind", "n", "value"]


def test_load_session_and_the_session_object():
    s = session.load_session(io.StringIO(session_text(SAMPLES)))
    assert s.version == 1 and len(s.events) == len(SAMPLES)
    assert s.start == SAMPLES[0] and s.ended
    partial = session.load_session(io.StringIO(session_text(SAMPLES[:5])))
    assert not partial.ended


def test_files_on_disk(tmp_path):
    path = tmp_path / "s.numbo.jsonl"
    session.write_session(path, SAMPLES)
    assert all(map(same, session.read_session(path), SAMPLES))
    assert session.detect_format(path) == "session"


# ---------------------------------------------------------------------------
# The recorder, on live runs

def test_the_recorder_records_exactly_the_published_events(tmp_path):
    path = tmp_path / "p1s1.jsonl"
    recorder = session.SessionRecorder(path)
    memory = Recorder()
    run(1, 1, observers=[recorder, memory])
    assert recorder.closed and recorder.events_written == len(memory.events)
    back = list(session.read_session(path))
    assert len(back) == len(memory.events) > 200
    assert all(map(same, back, memory.events))
    kinds = {e.kind for e in back}
    assert {"pnet-initialized", "pnodes-changed", "coderack-created", "node-changed",
            "op-node-created", "decomposition", "done"} <= kinds


def test_the_recorder_changes_nothing_in_a_run(tmp_path):
    assert run(3, 8) == run(3, 8, observers=[session.SessionRecorder(tmp_path / "s.jsonl")])


def test_a_recorder_closed_mid_run_writes_a_complete_partial_session():
    buf = io.StringIO()
    recorder = session.SessionRecorder(buf)
    for e in SAMPLES[:6]:
        recorder.on_event(e)
    recorder.close()
    recorder.close()                          # closing twice is harmless
    s = session.load_session(io.StringIO(buf.getvalue()))
    assert len(s.events) == 6 and not s.ended
    with pytest.raises(SessionError, match="closed"):
        recorder.on_event(SAMPLES[6])


def test_the_recorder_closes_itself_when_the_run_ends():
    buf = io.StringIO()
    recorder = session.SessionRecorder(buf)
    recorder.on_event(SAMPLES[0])
    recorder.on_event(observe.GaveUp(iterations=3))
    assert recorder.closed
    assert json.loads(buf.getvalue().splitlines()[-1]) == {"end": "numbo-session", "events": 2}


# ---------------------------------------------------------------------------
# Record -> replay -> the models equal the live run's after every event

class Snapshots:
    """After every event, a digest of the models (subscribed before this):
    their compared state and what views use (ghost ages, the last choice,
    what changed).  Every CHECKPOINT events, the windowed forest too."""

    CHECKPOINT = 250

    def __init__(self, tree, pnet, rack):
        self.tree, self.pnet, self.rack = tree, pnet, rack
        self.digests = []
        self.forests = []

    def on_event(self, event):
        t, p, r = self.tree, self.pnet, self.rack
        snapshot = (t.state(), sorted(t.ghost_since.items()), t.events_seen,
                    p.state(), sorted(p.changed),
                    r.state(), r.chosen, r.last_posted, r.changed, r.max_total)
        self.digests.append(hashlib.blake2b(repr(snapshot).encode()).digest())
        if len(self.digests) % self.CHECKPOINT == 0 or isinstance(event, observe.RunEnded):
            self.forests.append(repr(t.forest(ghost_window=50)))


def models_and_snapshots():
    tree, pnet, rack = TreeModel(), PnetModel(), CoderackModel()
    snaps = Snapshots(tree, pnet, rack)
    errors = []
    subject = observe.Subject(on_error=errors.append)
    for o in (tree, pnet, rack, snaps):
        subject.subscribe(o)
    return subject, snaps, errors


def record_and_replay(puzzle, seed, cap, path):
    """A process-pool job.  Live run with the models and a recorder, then a
    replay of the file into fresh models; plain data back."""
    live, live_snaps, live_errors = models_and_snapshots()
    recorder = session.SessionRecorder(path)
    memory = Recorder()
    result, _, trace = run(puzzle, seed, cap, observers=[live, recorder, memory])
    replayed, snaps, errors = models_and_snapshots()
    writer_out = io.StringIO()
    n = session.replay(session.read_session(path),
                       [replayed, OracleTraceWriter(writer_out, rng_events=True)])
    first_bad = next((i for i, (a, b) in enumerate(zip(live_snaps.digests, snaps.digests))
                      if a != b), None)
    return {"outcome": result["outcome"], "events": len(memory.events), "replayed": n,
            "errors": [repr(e.exception) for e in live_errors + errors],
            "digests": (len(live_snaps.digests), len(snaps.digests)),
            "first_bad": None if first_bad is None else (first_bad, memory.events[first_bad]),
            "forests_equal": live_snaps.forests == snaps.forests,
            "forests": len(snaps.forests),
            "trace_equal": writer_out.getvalue() == trace}


REPLAY_RUNS = [(1, 1, 20000),      # solved
               (3, 8, 20000),      # the kill-block gap
               (4, 1, 20000),      # "Obvious.": CYTO-TARGET rebound
               (1, 40, 20000),     # an error outcome
               (6, 1, 20000),      # 16k events
               (3, 1, 3000)]       # capped


@pytest.fixture(scope="module")
def replays(tmp_path_factory):
    d = tmp_path_factory.mktemp("sessions")
    with concurrent.futures.ProcessPoolExecutor(
            max_workers=min(len(REPLAY_RUNS), os.cpu_count() or 1)) as pool:
        jobs = {r: pool.submit(record_and_replay, *r, str(d / f"{r[0]}-{r[1]}.jsonl"))
                for r in REPLAY_RUNS}
        return {r: job.result() for r, job in jobs.items()}


@pytest.mark.parametrize("puzzle,seed,cap", REPLAY_RUNS)
def test_replayed_models_equal_the_live_ones_after_every_event(replays, puzzle, seed, cap):
    r = replays[puzzle, seed, cap]
    assert r["errors"] == []
    assert r["replayed"] == r["events"]
    assert r["digests"] == (r["events"], r["events"])
    assert r["first_bad"] is None, r["first_bad"]
    assert r["forests_equal"] and r["forests"] >= 1


@pytest.mark.parametrize("puzzle,seed,cap", REPLAY_RUNS)
def test_a_replay_writes_the_live_oracle_trace_byte_for_byte(replays, puzzle, seed, cap):
    assert replays[puzzle, seed, cap]["trace_equal"]


def test_the_replay_runs_cover_the_outcomes(replays):
    outcomes = {r["outcome"] for r in replays.values()}
    assert outcomes >= {"solved", "error", "capped"}
    assert max(r["events"] for r in replays.values()) > 10000


def test_replay_reports_observer_errors_and_goes_on():
    class Bad:
        def on_event(self, event):
            raise RuntimeError("boom")

    errors, memory = [], Recorder()
    n = session.replay(SAMPLES, [Bad(), memory], on_error=errors.append)
    assert n == len(SAMPLES) and len(memory.events) == len(SAMPLES)
    assert len(errors) == len(SAMPLES)


# ---------------------------------------------------------------------------
# Oracle traces (SBCL, lisp/tests/oracle/lib/full-run.lisp)

ORACLE_RUNS = [((1, 1, 20000), True), ((1, 1, 20000), False), ((3, 8, 20000), True),
               ((1, 40, 20000), True), ((3, 1, 500), True)]


@pytest.fixture(scope="module")
def oracle_traces(tmp_path_factory):
    d = tmp_path_factory.mktemp("oracle")
    out = {}
    for (puzzle, seed, cap), rng in ORACLE_RUNS:
        base = str(d / f"{puzzle}-{seed}-{cap}-{rng}")
        result = full_runs.oracle_run(PUZZLES[puzzle - 1], seed, cap, base, rng_events=rng)
        out[(puzzle, seed, cap), rng] = (base + ".jsonl", result)
    return out


@pytest.mark.parametrize("spec,rng", ORACLE_RUNS)
def test_an_oracle_trace_replays_and_verifies(oracle_traces, spec, rng):
    path, expected = oracle_traces[spec, rng]
    assert session.detect_format(path) == "oracle"
    live, live_snaps, _ = models_and_snapshots()
    run(*spec, observers=[live], rng_events=rng)
    replayed, snaps, errors = models_and_snapshots()
    divergences = []
    out = io.StringIO()
    r = session.replay_oracle_trace(path, [replayed], out=out,
                                    on_divergence=divergences.append)
    assert divergences == [] and r.divergence is None and errors == []
    with open(path, encoding="utf-8") as f:
        assert r.lines == sum(1 for _ in f)
    assert r.rng_events == rng
    assert r.result["outcome"] == expected["outcome"]
    assert r.result["iterations"] == expected["iterations"]
    assert out.getvalue() == expected["output"]
    # The replay's observers saw the same run as a live one.
    assert snaps.digests == live_snaps.digests


def corrupt(tmp_path, path, edit, name="bad.jsonl"):
    with open(path, encoding="utf-8") as f:
        lines = f.read().splitlines(keepends=True)
    lines = edit(lines)
    bad = tmp_path / name
    bad.write_text("".join(lines), encoding="utf-8")
    return bad


def iteration_line(lines, n):
    return next(i for i, l in enumerate(lines)
                if json.loads(l)["ev"] == "iteration" and json.loads(l)["n"] == n)


def test_a_changed_oracle_trace_is_caught_as_it_goes(oracle_traces, tmp_path):
    path, _ = oracle_traces[(1, 1, 20000), True]

    def edit(lines):
        i = iteration_line(lines, 20)
        e = json.loads(lines[i])
        e["temperature"] = e["temperature"] + 1
        lines[i] = json.dumps(e) + "\n"
        edit.line = i + 1
        return lines

    bad = corrupt(tmp_path, path, edit)
    memory = Recorder()
    seen_at_divergence = []
    with pytest.raises(TraceMismatch) as info:
        session.replay_oracle_trace(
            bad, [memory], out=io.StringIO(),
            on_divergence=lambda d: seen_at_divergence.append((d, len(memory.events))))
    message = str(info.value)
    assert f"line {edit.line}" in message and str(bad) in message
    assert "temperature" in message
    (d, seen), = seen_at_divergence
    assert d.line == edit.line
    # Found while the re-run was at that iteration, not after it.
    iteration_20 = next(i for i, e in enumerate(memory.events)
                        if isinstance(e, observe.IterationBegan) and e.n == 20)
    assert iteration_20 < seen < len(memory.events)
    # Without raising, the divergence is in the result.
    r = session.replay_oracle_trace(bad, out=io.StringIO(), check=False)
    assert r.divergence.line == edit.line


def test_a_truncated_oracle_trace_is_caught(oracle_traces, tmp_path):
    path, _ = oracle_traces[(1, 1, 20000), True]
    bad = corrupt(tmp_path, path, lambda lines: lines[:100])
    with pytest.raises(TraceMismatch, match=r"ends after line 100.*(truncated)"):
        session.replay_oracle_trace(bad, out=io.StringIO())


def test_an_oracle_trace_cut_mid_line_is_caught(oracle_traces, tmp_path):
    path, _ = oracle_traces[(1, 1, 20000), True]
    bad = corrupt(tmp_path, path, lambda lines: lines[:99] + [lines[99][:20]])
    with pytest.raises(SessionError, match=r"line 100.*not valid JSON"):
        session.replay_oracle_trace(bad, out=io.StringIO())


def test_an_oracle_trace_with_extra_lines_is_caught(oracle_traces, tmp_path):
    path, _ = oracle_traces[(1, 1, 20000), True]
    bad = corrupt(tmp_path, path, lambda lines: lines + ['{"ev":"rack-emptied"}\n'])
    with pytest.raises(TraceMismatch, match=r"line %d.*after the re-run ended" % (
            sum(1 for _ in open(path)) + 1)):
        session.replay_oracle_trace(bad, out=io.StringIO())


def test_an_oracle_trace_must_start_with_its_start_event(tmp_path, oracle_traces):
    path, _ = oracle_traces[(1, 1, 20000), True]
    bad = corrupt(tmp_path, path, lambda lines: lines[1:])
    with pytest.raises(SessionError, match="start"):
        session.replay_oracle_trace(bad, out=io.StringIO())
    other = corrupt(tmp_path, path, lambda lines: [lines[0].replace("splitmix64", "mt")] +
                    lines[1:], name="rng.jsonl")
    with pytest.raises(SessionError, match="splitmix64"):
        session.replay_oracle_trace(other, out=io.StringIO())


def test_replay_file_detects_the_format(oracle_traces, tmp_path):
    path, _ = oracle_traces[(1, 1, 20000), True]
    memory = Recorder()
    r = session.replay_file(path, [memory], out=io.StringIO())
    assert r.format == "oracle" and r.oracle.divergence is None
    s = tmp_path / "s.jsonl"
    session.write_session(s, memory.events)
    again = Recorder()
    r = session.replay_file(s, [again])
    assert r.format == "session" and r.events == len(memory.events)
    assert all(map(same, again.events, memory.events))


# ---------------------------------------------------------------------------
# Corrupted and truncated session files

def check_error(text, pattern, tmp_path):
    path = tmp_path / "broken.jsonl"
    path.write_text(text, encoding="utf-8")
    with pytest.raises(SessionError, match=pattern) as info:
        session.load_session(path)
    assert str(path) in str(info.value)
    with pytest.raises(SessionError, match=pattern):
        list(session.read_session(path))
    return info.value


GOOD = session_text(SAMPLES[:4])
GOOD_LINES = GOOD.splitlines(keepends=True)


def test_an_empty_file(tmp_path):
    check_error("", "empty", tmp_path)


def test_not_a_session_file(tmp_path):
    check_error('{"hello": 1}\n', r"line 1.*not a numbo session", tmp_path)
    check_error("garbage\n", r"line 1.*not valid JSON", tmp_path)
    with pytest.raises(SessionError, match="neither"):
        p = tmp_path / "x.jsonl"
        p.write_text('{"hello": 1}\n')
        session.detect_format(p)


def test_a_newer_version(tmp_path):
    e = check_error(GOOD.replace('"version":1', '"version":2', 1),
                    r"version 2.*reads version 1", tmp_path)
    assert e.line == 1


def test_a_truncated_session(tmp_path):
    check_error("".join(GOOD_LINES[:-1]), r"truncated after line 5.*no end line", tmp_path)
    check_error("".join(GOOD_LINES[:-1]) + GOOD_LINES[-1][:7],
                r"line 6.*not valid JSON.*truncated", tmp_path)


def test_a_corrupted_event_line(tmp_path):
    lines = list(GOOD_LINES)
    lines[2] = "{#" + lines[2][1:]
    check_error("".join(lines), r"line 3.*not valid JSON", tmp_path)


def test_an_unknown_kind_and_bad_fields(tmp_path):
    def with_line(i, obj):
        lines = list(GOOD_LINES)
        lines[i] = json.dumps(obj) + "\n"
        return "".join(lines)

    check_error(with_line(2, {"kind": "teleport", "n": 1}), r"line 3.*unknown event kind 'teleport'",
                tmp_path)
    check_error(with_line(2, {"kind": "rng", "n": 1}), r"line 3.*rng.*missing.*value", tmp_path)
    check_error(with_line(2, {"kind": "rng", "n": 1, "value": 2, "x": 3}),
                r"line 3.*rng.*unexpected.*x", tmp_path)
    check_error(with_line(2, {"n": 1, "value": 2}), r"line 3.*no kind", tmp_path)
    check_error(with_line(2, [1, 2]), r"line 3.*not a JSON object", tmp_path)
    check_error(with_line(4, {"kind": "decomposition", "steps": [{"op": "PLUS"}]}),
                r"line 5.*decomposition step", tmp_path)


def test_a_wrong_count_and_lines_after_the_end(tmp_path):
    check_error(GOOD.replace('"events":4', '"events":5'), r"line 6.*5 events.*4", tmp_path)
    check_error(GOOD + GOOD_LINES[1], r"line 7.*after the end line", tmp_path)
