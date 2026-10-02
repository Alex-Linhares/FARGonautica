"""loop0003 item 10, the Qt-free half: the event log model
(numbo/models/event_log.py), the tree layout's history for scrubbing
(numbo/models/layout_history.py), and the solution check of a replayed run,
which has no printed text (run_stats.solution_text)."""

import io
import random
import time

import pytest

from numbo import harness, observe, solution_checker
from numbo.models import event_log, layout_history, run_stats
from numbo.models.event_log import EventLog, event_text
from numbo.models.layout_history import LayoutHistory
from numbo.models.tree_layout import TreeLayout
from numbo.models.tree_model import TreeModel

P1 = (114, 11, 20, 7, 1, 6)
P3 = (31, 3, 5, 24, 3, 14)


class Recorder:
    def __init__(self):
        self.events = []

    def on_event(self, event):
        self.events.append(event)


def record(problem, seed, cap=20000, rng_events=False):
    rec, out = Recorder(), io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap, out=out,
                                trace=io.StringIO() if rng_events else None,
                                rng_events=rng_events, observers=[rec])
    return rec.events, result, out.getvalue()


@pytest.fixture(scope="module")
def p1s1():
    return record(P1, 1, rng_events=True)


@pytest.fixture(scope="module")
def p3s8():
    return record(P3, 8)


# ---------------------------------------------------------------------------
# event_text

def sample_events():
    from numbo.observe import (CodeletChosen, CodeletPosted, CoderackCreated,
                               CurrentTargetChanged, Decomposition, DecompositionStep,
                               Disconnect, GaveUp, IterationBegan, NodeChanged, NodeCreated,
                               NodeKilled, OpNodeCreated, PnetActivations, PnetInitialized,
                               PnodesChanged, RackEmptied, RngDraw, RunError, RunStarted,
                               SetupChoose, Solved, Capped, TargetReplaced)
    step = DecompositionStep("PLUS6-1-V2", "CYTO-BRICK5", 6, "CYTO-TARGET-1-V2", 1,
                             "CYTO-TARGET")
    return [
        RunStarted((114, 11, 20, 7, 1, 6), 1, 20000, "splitmix64", ("ONE", "TWO")),
        RngDraw(3, 12345678901234567890),
        SetupChoose("LOOK-FOR-NEW-BLOCK", (), 4, ((4, 2),), ((1, 7),)),
        SetupChoose(None, None, None, (), ()),
        IterationBegan(3, 4, 52.25, ((300, 1), (4, 6))),
        CodeletChosen(3, "CONST-BLX", ({"obj": "cyto-node", "name": "CYTO-BRICK4"},
                                       {"obj": "cyto-node", "name": "CYTO-BRICK1"}, 84),
                      84, ((2, 5),)),
        CodeletChosen(9, None, None, None, ()),
        CodeletPosted("DECOMP+", ({"obj": "cyto-node", "name": "CYTO-BLOCK120-V1"},
                                  ("QUOTE", "CYTO-TARGET")), 300),
        NodeCreated("CYTO-BRICK1", "2b", 11, "free", 1, 100, None),
        OpNodeCreated("TIMES6-20-V1", "TIMES", "CYTO-BLOCK120-V1",
                      ("CYTO-BRICK2", "CYTO-BRICK5"), 1),
        NodeChanged("CYTO-BRICK2", "status", "linked"),
        NodeChanged("CYTO-BRICK2", "neighbors", (("TIMES6-20-V1", "OPERAND"),)),
        CurrentTargetChanged("CYTO-TARGET-6-V1", 0.5),
        CurrentTargetChanged(None, None),
        Disconnect("CYTO-BLOCK11-V5", "4bl", 11, "PLUS8-3-V6", "5g", "PLUS"),
        NodeKilled("CYTO-BLOCK11-V5", ("CYTO-TARGET", "CYTO-BRICK1")),
        TargetReplaced("CYTO-TARGET-6-V1", "CYTO-BLOCK6-V3", False),
        TargetReplaced("CYTO-TARGET", "CYTO-BLOCK25-V4", True),
        PnetActivations((0, 1.5, 200)),
        PnetInitialized((0, 0, 0)),
        PnodesChanged("activation", (("SIX", 20), ("SEVEN", 0.0))),
        PnodesChanged("instances", (("SIX", (("2b", "CYTO-BRICK5"),)), ("ONE", None))),
        CoderackCreated("CODERACK", (300, 200, 100, 4)),
        RackEmptied(),
        Decomposition((step,)),
        Solved(45, (step,)),
        GaveUp(3289),
        Capped(20000),
        RunError(29, "SEND: NIL does not handle the message :SET-ACTIVATION"),
    ]


def test_every_event_kind_has_a_one_line_text():
    samples = sample_events()
    assert {type(e) for e in samples} == set(observe.EVENT_TYPES.values())
    for e in samples:
        text = event_text(e)
        assert isinstance(text, str) and text and "\n" not in text, e


def test_event_texts_say_what_happened():
    texts = [event_text(e) for e in sample_events()]
    assert texts[0] == "114 from 11 20 7 1 6, seed 1, at most 20000 iterations"
    assert texts[1] == "draw 3: 12345678901234567890"
    assert texts[2] == "set-up: look-for-new-block (urgency 4)"
    assert texts[3] == "set-up: nothing (the rack was empty)"
    assert texts[4] == "iteration 3: x 4, temperature 52.25, 7 codelets waiting"
    assert texts[5] == "iteration 3: const-blx CYTO-BRICK4 CYTO-BRICK1 84 (urgency 84)"
    assert texts[6] == "iteration 9: nothing (the rack was empty)"
    assert texts[7] == "decomp+ CYTO-BLOCK120-V1 'CYTO-TARGET (urgency 300)"
    assert texts[8] == "CYTO-BRICK1: brick 11, free, activation 100"
    assert texts[9] == "TIMES6-20-V1: CYTO-BLOCK120-V1 = TIMES of CYTO-BRICK2 CYTO-BRICK5"
    assert texts[10] == "CYTO-BRICK2 status: linked"
    assert texts[11] == "CYTO-BRICK2 neighbors: TIMES6-20-V1 (OPERAND)"
    assert texts[12] == "CYTO-TARGET-6-V1 (interest 0.5)"
    assert texts[13] == "none"
    assert texts[14] == "CYTO-BLOCK11-V5 (block 11) and PLUS8-3-V6"
    assert texts[15] == "CYTO-BLOCK11-V5 (2 nodes left)"
    assert texts[16] == "CYTO-BLOCK6-V3 replaces CYTO-TARGET-6-V1"
    assert texts[17] == "CYTO-BLOCK25-V4 is the target (Obvious.)"
    assert texts[18] == "spread: 2 of 3 pnodes active"
    assert texts[19] == "3 pnodes reset"
    assert texts[20] == "activation: SIX 20, SEVEN 0.0"
    assert texts[21] == "instances: SIX (CYTO-BRICK5), ONE (none)"
    assert texts[22] == "CODERACK, urgencies 300 200 100 4"
    assert texts[23] == "the coderack is empty"
    assert texts[24] == "1 step: CYTO-TARGET = CYTO-BRICK5 PLUS6-1-V2 CYTO-TARGET-1-V2"
    assert texts[25] == "solved after 45 iterations"
    assert texts[26] == "gave up after 3289 iterations"
    assert texts[27] == "capped at 20000 iterations"
    assert texts[28] == ("error after 29 iterations: SEND: NIL does not handle the message "
                         ":SET-ACTIVATION")


def test_long_texts_are_cut():
    e = observe.NodeKilled("X", tuple(f"CYTO-BRICK{i}" for i in range(500)))
    assert len(event_text(e)) <= event_log.MAX_TEXT
    e = observe.PnodesChanged("activation", tuple((f"P{i}", i) for i in range(500)))
    assert len(event_text(e)) <= event_log.MAX_TEXT
    assert event_text(e).endswith("…")


def test_the_texts_of_a_real_run(p1s1):
    events = p1s1[0]
    texts = [event_text(e) for e in events]
    assert all(t and "\n" not in t for t in texts)
    assert texts[-1] == "solved after 45 iterations"


# ---------------------------------------------------------------------------
# EventLog

def test_the_log_keeps_every_event_and_its_iterations(p1s1):
    events = p1s1[0]
    log = EventLog()
    shown = [log.on_event(e) for e in events]
    assert all(shown)
    assert log.events == events and len(log) == len(events)
    assert log.visible == list(range(len(events)))
    starts = [i for i, e in enumerate(events) if isinstance(e, observe.IterationBegan)]
    assert [log.iteration_index(n) for n in range(45)] == starts
    assert log.iteration_index(45) is None and log.iteration_index(-1) is None
    assert log.iterations() == (0, 44)
    assert log.iteration_at(0) is None
    assert log.iteration_at(starts[0] - 1) is None
    for n, i in enumerate(starts):
        assert log.iteration_at(i) == n
        assert log.iteration_at(i + 1) == n
    assert log.iteration_at(len(events) - 1) == 44
    counts = {}
    for e in events:
        counts[e.kind] = counts.get(e.kind, 0) + 1
    assert log.counts == counts


def test_a_start_event_begins_a_new_log(p1s1, p3s8):
    log = EventLog()
    for e in p1s1[0]:
        log.on_event(e)
    for e in p3s8[0]:
        log.on_event(e)
    assert log.events == p3s8[0]
    assert sum(log.counts.values()) == len(p3s8[0])


def test_filtering_by_kind(p1s1):
    events = p1s1[0]
    hidden = {"rng", "post", "pnodes-changed"}
    log = EventLog()
    log.set_hidden(hidden)
    shown = [log.on_event(e) for e in events]
    want = [i for i, e in enumerate(events) if e.kind not in hidden]
    assert log.visible == want
    assert shown == [e.kind not in hidden for e in events]
    assert log.hidden == frozenset(hidden)
    # Changing the filter later gives the same rows.
    log2 = EventLog()
    log2.load(events)
    assert log2.visible == list(range(len(events)))
    log2.set_hidden(hidden)
    assert log2.visible == want
    log2.set_hidden(())
    assert log2.visible == list(range(len(events)))
    log2.set_hidden(event_log.KINDS)
    assert log2.visible == []


def test_row_of_finds_the_last_shown_event_at_or_before(p1s1):
    events = p1s1[0]
    log = EventLog()
    log.load(events)
    log.set_hidden({"post", "rng"})
    for index in range(len(events)):
        row = log.row_of(index)
        before = [i for i in log.visible if i <= index]
        if before:
            assert log.visible[row] == before[-1]
        else:
            assert row is None
    assert log.row_of(-1) is None


def test_kinds_lists_every_event_kind():
    assert set(event_log.KINDS) == set(observe.EVENT_TYPES)
    assert len(event_log.KINDS) == len(set(event_log.KINDS))


def test_the_log_is_fast_on_long_runs():
    events = record((146, 12, 2, 5, 7, 18), 1)[0]      # about 16k events
    log = EventLog()
    log.set_hidden({"post"})
    t = time.perf_counter()
    for _ in range(6):                                  # about 96k events
        log.clear()
        for e in events:
            log.on_event(e)
    per_event = (time.perf_counter() - t) / (6 * len(events))
    assert per_event < 10e-6, per_event
    t = time.perf_counter()
    log.set_hidden({"post", "pnodes-changed"})
    assert time.perf_counter() - t < 0.05


# ---------------------------------------------------------------------------
# The solution check of a replay (no printed text)

@pytest.mark.parametrize("problem,seed,cap", [(P1, 1, 20000), (P3, 8, 20000),
                                              ((25, 8, 5, 5, 11, 2), 1, 20000),
                                              (P1, 40, 20000), (P3, 1, 300),
                                              ((146, 12, 2, 5, 7, 18), 1, 20000)])
def test_the_check_from_the_events_equals_the_check_of_the_printed_text(problem, seed, cap):
    events, result, output = record(problem, seed, cap)
    from_output = run_stats.RunStats()
    from_events = run_stats.RunStats()
    for e in events:
        from_output.on_event(e)
        from_events.on_event(e)
    from_output.finish(output)
    from_events.finish(None)
    assert from_events.check == from_output.check
    assert from_events.check == solution_checker.check_solution(output, list(problem))


def test_solution_text_reads_like_decompose():
    step = observe.DecompositionStep("PLUS6-1-V2", "CYTO-BRICK5", 6, "CYTO-TARGET-1-V2", 1,
                                     "CYTO-TARGET")
    text = run_stats.solution_text(observe.Solved(3, (step,)))
    assert text == ("Done :\nOperation PLUS6-1-V2 has been applied \nto CYTO-BRICK5 ( 6) "
                    "and to CYTO-TARGET-1-V2 ( 1)\nto get CYTO-TARGET\n")
    assert run_stats.solution_text(observe.GaveUp(3)) == ""
    assert run_stats.solution_text(None) == ""


# ---------------------------------------------------------------------------
# LayoutHistory: the tree layout at any event, as a live run drew it

def measure(label):
    lines = label.split("\n")
    return (8 * max(len(x) for x in lines) + 12, 14 * len(lines) + 6)


def label(node):
    from numbo.models.tree_layout import node_label
    text = node_label(node)
    return text


def make_layouter():
    return TreeLayout(measure, label=label)


GHOSTS = 50


def live_layouts(events):
    """What a view that lays out after every event draws: on_event, then
    (a new run resets the layout) the layout of the windowed forest."""
    model, layouter, layouts = TreeModel(), make_layouter(), []
    for e in events:
        model.on_event(e)
        if e.kind == "start":
            layouter.reset()
        layouts.append(layouter.layout(model.forest(ghost_window=GHOSTS)))
    return layouts


@pytest.mark.parametrize("run,every", [("p1s1", 64), ("p1s1", 7), ("p3s8", 32)])
def test_the_history_gives_the_live_layout_at_any_event(request, run, every):
    events = request.getfixturevalue(run)[0]
    live = live_layouts(events)
    rng = random.Random(4)
    # Cold: nothing recorded, queries in random order (back and forth).
    history = LayoutHistory(make_layouter, GHOSTS, every=every)
    order = list(range(len(events)))
    rng.shuffle(order)
    for i in order[:150] + [len(events) - 1, 0, len(events) - 1]:
        assert history.layout_at(events, i) == live[i], i
    assert history.layout_at(events, -1) is None
    assert all(k % every == 0 for k in history.checkpoints)
    assert all(history.checkpoints[k] == live[k] for k in history.checkpoints)
    # Warm: checkpoints recorded by a live pass (as the window records them).
    history = LayoutHistory(make_layouter, GHOSTS, every=every)
    for i, layout in enumerate(live):
        history.record(i, layout)
    assert set(history.checkpoints) == set(range(0, len(events), every))
    for i in order[:150]:
        assert history.layout_at(events, i) == live[i], i


def test_backward_queries_restart_from_a_checkpoint_not_from_zero(p3s8):
    events = p3s8[0]
    history = LayoutHistory(make_layouter, GHOSTS, every=32)
    history.layout_at(events, len(events) - 1)
    calls = []
    real = TreeModel.forest

    def counting(self, *a, **k):
        calls.append(1)
        return real(self, *a, **k)

    TreeModel.forest = counting
    try:
        history.layout_at(events, 500)
    finally:
        TreeModel.forest = real
    assert len(calls) <= 32


def test_advance_extends_the_frontier_in_chunks(p3s8):
    events = p3s8[0]
    live = live_layouts(events)
    history = LayoutHistory(make_layouter, GHOSTS, every=16)
    assert history.frontier == -1
    while history.frontier < len(events) - 1:
        before = history.frontier
        after = history.advance(events, 100)
        assert after == history.frontier == min(before + 100, len(events) - 1)
    assert history.advance(events, 100) == len(events) - 1
    assert set(history.checkpoints) == set(range(0, len(events), 16))
    assert all(history.checkpoints[k] == live[k] for k in history.checkpoints)
    history.clear()
    assert history.frontier == -1 and history.checkpoints == {}


def test_the_new_models_are_qt_free():
    for module in (event_log, layout_history):
        with open(module.__file__, encoding="utf-8") as f:
            assert "PySide6" not in f.read()
