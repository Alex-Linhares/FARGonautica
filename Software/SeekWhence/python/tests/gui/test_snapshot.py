"""Loop0004 item 01: the snapshot and the tap (`seekwhence/gui/models/snapshot.py`,
`tap.py`), Qt-free.

- A run with the tap gives the same event records, trace text, draws and outcome as a
  plain run, for the three appendix runs on their golden seeds [M86 pp. 164-179].
- A snapshot never mutates the world: the hypothesis's `covered`, the boxes' `pstruc_val`s,
  the counters and the draws are unchanged around every call.
- `state_at` agrees with a snapshot taken at that point by an independent observer.
- Unchanged sections are shared with the previous snapshot.
- The snapshot holds what `show` prints: the `show` block rebuilt from it equals
  `Session.show_lines()` [M86 pp. 167-168, 174-175, 178-179].
- Known facts at the ends of the runs: run 1 `(Cycle 3 2 (3 7))`, validity 1 [M86 p. 166];
  run 3 `(S-group 1 (Countup 2))`, the freeze-dried `(Countup 1)` and `(S-group 1 2)`, the
  reigning class `S-group` [M86 pp. 174-179].
- The keyframe rule for long runs (G-003), forced on an appendix run.
"""

import io

import pytest

from seekwhence.events import Recorder
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.snapshot import SHARED, Snapshot, snapshot
from seekwhence.gui.models.tap import ALWAYS, Tap
from seekwhence.session import Session
from seekwhence.trace import event_record

from golden import APPENDIX

RUN_IDS = [run_id for run_id, _ in APPENDIX]


def tapped(ref, **tap_args):
    """The reference run again, with a tap; returns (log, trace text, session)."""
    log, out = EventLog(), io.StringIO()
    tap = Tap(log, **tap_args)
    session = Session(ref.config, ref.seed, out=out, lines=list(ref.lines),
                      answers=list(ref.answers), observers=[tap])
    tap.attach(session.world)
    session.run()
    return log, out.getvalue(), session


# ---- the tap changes nothing ------------------------------------------------------------------

@pytest.mark.parametrize("run_id", RUN_IDS)
def test_a_run_with_the_tap_is_the_same_run(appendix_runs, run_id):
    ref = appendix_runs[run_id]
    log, text, session = tapped(ref)
    assert [event_record(e) for e in log.events] == ref.records
    assert text == ref.text
    assert session.world.rng.draws == ref.draws
    assert session.outcome == ref.outcome
    assert log.snapshots and all(isinstance(s, Snapshot) for s in log.snapshots.values())
    # Every chosen event of an appendix run (under 200 tasks) has a snapshot, and so do the
    # prompt, guess, answer and end events.
    for i, e in enumerate(log.events):
        assert (i in log.snapshots) == (e.kind == "chosen" or e.kind in ALWAYS), (i, e.kind)


class Guard:
    """Reads the world's state before and after a snapshot at every event; the two must
    agree (the pure-read contract, TASK.md rule 2)."""

    def __init__(self):
        self.world = None
        self.calls = 0
        self.violations = []

    def state(self):
        w = self.world
        h = w.hypothesis
        boxes = None if h is None else [(b.name, b.pstruc_val, b.ready) for b in h.box.boxes]
        covered = None if h is None else (h.covered, h.validity, list(h.pending))
        return (covered, boxes, dict(w.counters), w.rng.draws, w.taskrack.tasks,
                w.cytoplasm.names, w.socratoplasm.names, w.reigning, w.threshold)

    def on_event(self, event):
        before = self.state()
        snapshot(self.world)
        snapshot(self.world, source="template")
        self.calls += 1
        after = self.state()
        if before != after:
            self.violations.append((event.kind, before, after))


@pytest.mark.parametrize("run_id", RUN_IDS)
def test_a_snapshot_never_mutates_the_world(appendix_runs, run_id):
    ref = appendix_runs[run_id]
    guard, rec, out = Guard(), Recorder(), io.StringIO()
    session = Session(ref.config, ref.seed, out=out, lines=list(ref.lines),
                      answers=list(ref.answers), observers=[rec, guard])
    guard.world = session.world
    session.run()
    assert guard.calls == len(ref.events) - 1       # every event but `start`
    assert guard.violations == []
    assert [event_record(e) for e in rec.events] == ref.records
    assert out.getvalue() == ref.text and session.world.rng.draws == ref.draws


# ---- state_at and sharing ------------------------------------------------------------------------

class Shadow:
    """An independent observer taking a fresh, uncached, unshared snapshot at the same
    events as the tap."""

    def __init__(self):
        self.world = None
        self.index = -1
        self.taken = {}
        self.source = None

    def on_event(self, event):
        self.index += 1
        if event.kind == "hypothesis":
            self.source = event.source
        elif event.kind == "candidate":
            self.source = "candidate"
        if event.kind == "chosen" or event.kind in ALWAYS:
            self.taken[self.index] = snapshot(self.world, source=self.source)


@pytest.mark.parametrize("run_id", RUN_IDS)
def test_state_at_agrees_with_a_snapshot_taken_at_that_point(appendix_runs, run_id):
    ref = appendix_runs[run_id]
    log, shadow, out = EventLog(), Shadow(), io.StringIO()
    tap = Tap(log)
    session = Session(ref.config, ref.seed, out=out, lines=list(ref.lines),
                      answers=list(ref.answers), observers=[tap, shadow])
    tap.attach(session.world)
    shadow.world = session.world
    session.run()
    assert set(log.snapshots) == set(shadow.taken)
    for i in log.snapshots:
        assert log.snapshots[i] == shadow.taken[i], i
    for i in range(len(log.events)):
        at = log.snapshot_index(i)
        if at is None:
            assert log.state_at(i) is None and i < min(shadow.taken)
        else:
            assert at <= i and log.state_at(i) == shadow.taken[at]
            assert all(j not in shadow.taken for j in range(at + 1, i + 1))


@pytest.mark.parametrize("run_id", RUN_IDS)
def test_unchanged_sections_are_shared_with_the_previous_snapshot(appendix_runs, run_id):
    log, _, _ = tapped(appendix_runs[run_id])
    snaps = [log.snapshots[i] for i in sorted(log.snapshots)]
    for a, b in zip(snaps, snaps[1:]):
        for name in SHARED:
            x, y = getattr(a, name), getattr(b, name)
            if x == y:
                assert x is y, name
    distinct = {name: len({id(getattr(s, name)) for s in snaps}) for name in SHARED}
    assert distinct["platoplasm"] < len(snaps) / 4
    assert distinct["glints"] < len(snaps) / 2
    assert 1 <= distinct["template"] <= 6


def test_a_snapshot_with_and_without_the_cache_are_equal(appendix_runs):
    ref = appendix_runs["run3_121231234"]
    log, _, session = tapped(ref)
    fresh = snapshot(session.world, source="candidate")
    assert fresh == log.last_snapshot
    assert fresh == snapshot(session.world, previous=log.last_snapshot, source="candidate")


# ---- what show prints [M86 pp. 167-168, 174-175, 178-179] --------------------------------------

def show_lines_from(snap):
    """The `show` block rebuilt from a snapshot (`Session.show_lines`, DECISION D-054)."""
    lines = ["terms of the sequence:", " ".join(str(t) for t in snap.terms), "", "bonds:"]
    lines.extend(b.show_line for b in snap.bonds)
    lines.extend(["", "gloms:"])
    for g in snap.gloms:
        gap = "  " if g.pseudo else " "
        lines.append(f"{g.name} {g.type_text} --> {value_text(g.print_value)}{gap}"
                     f"terms {g.start} to {g.end}")
    lines.extend(["", "gnoths:", ""])
    for i, gnoth in enumerate(snap.gnoths):
        if i:
            lines.append("")
        lines.extend(f"{field}: {text}" for field, text in gnoth.fields)
    return lines


def value_text(value):
    from seekwhence.forms import value_text as engine_value_text
    return engine_value_text(value)


@pytest.mark.parametrize("run_id", RUN_IDS)
def test_the_last_snapshot_holds_what_show_prints(appendix_runs, run_id):
    ref = appendix_runs[run_id]
    log, _, session = tapped(ref)
    snap = log.last_snapshot
    assert show_lines_from(snap) == ref.show == session.show_lines()
    assert snap.parse == session.show_parse()
    assert snap.hypothesis.form == session.show_hypothesis()
    assert snap.clock == session.world.clock and snap.done
    assert snap.terms == tuple(session.world.terms)


# ---- known facts at the ends of the runs ----------------------------------------------------------

def test_run_1_ends_on_the_cycle_with_validity_1(appendix_runs):
    """`(Cycle 3 2 (3 7))`, the guess `3 7 3 7` accepted [M86 p. 166]."""
    log, _, _ = tapped(appendix_runs["run1_3737"])
    snap = log.last_snapshot
    assert snap.terms == (3, 7, 3, 7) and snap.done
    h = snap.hypothesis
    assert (h.form, h.validity, h.source, h.covered) == ("(Cycle 3 2 (3 7))", 1, "template", 4)
    assert [(g.name, g.frame, g.plato_class, g.range) for g in snap.gnoths] == \
        [("gnoth1", 0, "Cycle", (1, 2))]
    assert snap.catchall is not None and snap.catchall.glints == ("glint3", "glint4")
    assert snap.parse == "(((3 7)))"
    assert snap.reformulation.reigning is None and snap.reformulation.freeze_dried == ()
    glom1 = snap.glom("glom1")
    assert (glom1.type_text, glom1.print_value, glom1.subgloms) == \
        ("(Same print-value fence)", (3, 7), ("glint1", "glint2"))
    assert glom1.structure == (("(Cycle 3 2 (3 7))", "pure"),)
    assert [g.value for g in snap.glints] == [3, 7, 3, 7]
    # The box tree of Figure 3's kind [M86 p. 83]: a Cycle over two constants.
    assert h.structure_text.splitlines()[0] == "box4 printstruc: pstruc4"
    assert [p.type for p in h.pstrucs] == ["Cycle", "Constant", "Constant"]
    assert h.pstrucs[0].elements == ("box5", "box6")
    # The snapshot at the guess, before the answer, shows the run not yet done.
    at_guess = [i for i, e in enumerate(log.events) if e.kind == "guess"][0]
    assert not log.snapshots[at_guess].done and log.snapshots[at_guess].hypothesis.validity == 1


def test_run_3_ends_on_the_s_group_with_its_freeze_dried_list(appendix_runs):
    """`(S-group 1 (Countup 2))`, the freeze-dried `(Countup 1)` (2 terms) and
    `(S-group 1 2)` (4 terms), the reigning class `S-group` [M86 pp. 174-179]."""
    log, _, _ = tapped(appendix_runs["run3_121231234"])
    snap = log.last_snapshot
    assert snap.terms == (1, 2, 1, 2, 3, 1) and snap.done
    h = snap.hypothesis
    assert (h.form, h.validity, h.source) == ("(S-group 1 (Countup 2))", 1, "candidate")
    r = snap.reformulation
    assert (r.reigning, r.threshold) == ("S-group", 10.0)
    assert [(f.form, f.terms_explained, f.text) for f in r.freeze_dried] == [
        ("(Countup 1)", 2, "(Countup 1) (2 terms)"),
        ("(S-group 1 2)", 4, "(S-group 1 2) (4 terms)")]
    assert snap.parse == "((1 2) (1 2 3))"
    assert [(g.name, g.frame, g.plato_class, g.range) for g in snap.gnoths] == \
        [("gnoth2", 1, "S-group", (1, 2)), ("gnoth3", 2, "S-group", (3, 5))]
    lines = h.structure_text.splitlines()
    assert lines[0] == "box15 printstruc: pstruc15"
    assert "pstruc15 type: S-group boxes: (box15) n-val: box16 k-val: box17" in lines
    s_group = [t for t in snap.platoplasm.types if t.name == "S-group"][0]
    assert set(s_group.actualizations) == {"gnoth2", "gnoth3"}
    assert dict(s_group.s_links)["Countup"] == 0.1 and dict(s_group.s_links)["Tuple"] == 0.4
    assert [t.name for t in snap.platoplasm.types][:2] == ["Constant", "Countup"]
    # The slip-check of p. 176: a snapshot after it shows the gnoths' bond lists set.
    assert any(g.internal_bonds is not None for g in snap.gnoths)


# ---- the rack ---------------------------------------------------------------------------------------

def test_the_rack_is_read_after_the_draw_with_weights(appendix_runs):
    """At a `chosen` event the chosen task is off the rack already (G-007); every waiting
    task's weight is its urgency to the `urgency_power` (DECISION D-007)."""
    ref = appendix_runs["run2_373373"]
    log, _, _ = tapped(ref)
    power = ref.config.urgency_power
    seen = 0
    for i, e in enumerate(log.events):
        if e.kind != "chosen" or e.number is None:
            continue
        rack = log.snapshots[i].rack
        assert e.number not in [t.number for t in rack.tasks]
        assert all(t.weight == t.urgency ** power for t in rack.tasks)
        assert rack.total_weight == sum(t.weight for t in rack.tasks)
        counts = {}
        for t in rack.tasks:
            counts[t.type] = counts.get(t.type, 0) + 1
        assert dict(rack.counts) == counts
        seen += 1
    assert seen > 50
    assert max(len(s.rack.tasks) for s in log.snapshots.values()) <= 28   # ENGINE-API.md 1.2


# ---- the keyframe rule (G-003) -----------------------------------------------------------------------

def test_the_keyframe_rule_thins_the_snapshots_of_a_long_run(appendix_runs):
    ref = appendix_runs["run3_121231234"]
    log, text, session = tapped(ref, dense_tasks=20, keyframe=64)
    assert text == ref.text and session.world.rng.draws == ref.draws
    taken = sorted(log.snapshots)
    chosen_at = [i for i in taken if log.events[i].kind == "chosen"]
    dense = [i for i in chosen_at if log.events[i].clock <= 20]
    sparse = [i for i in chosen_at if log.events[i].clock > 20]
    assert len(dense) == 20 and sparse
    for i in sparse:
        before = [j for j in taken if j < i]
        assert i - before[-1] >= 64
    # Between two sparse chosen events the gap is under 64 + the events of one task.
    for a, b in zip(sparse, sparse[1:]):
        others = [j for j in taken if a < j < b]
        if not others:
            assert b - a < 64 + 60
    for i, e in enumerate(log.events):
        if e.kind in ALWAYS:
            assert i in log.snapshots
    assert log.last_snapshot.hypothesis.form == "(S-group 1 (Countup 2))"
    # A seek between keyframes gets the last keyframe.
    i = sparse[1]
    assert log.state_at(i + 1) is log.snapshots[i]
