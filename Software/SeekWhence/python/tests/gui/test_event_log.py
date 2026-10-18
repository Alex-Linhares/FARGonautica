"""Loop0004 item 01: the event log model (`seekwhence/gui/models/event_log.py`), Qt-free,
after the Numbo GUI's `tests/test_event_log.py`: `event_text`, the lookups by term, clock
and event index, the rows and the hidden kinds, the snapshots and `state_at`.
"""

import pytest

from seekwhence import events as ev
from seekwhence.gui.models.event_log import KINDS, MAX_TEXT, EventLog, event_text
from seekwhence.gui.models.tap import Tap

from golden import APPENDIX

RUN_IDS = [run_id for run_id, _ in APPENDIX]


def logged(ref):
    log = EventLog()
    log.load(ref.events)
    return log


# ---- event_text -------------------------------------------------------------------------------

def test_kinds_are_the_engines_event_kinds():
    assert KINDS == tuple(ev.EVENT_TYPES) and len(KINDS) >= 50


@pytest.mark.parametrize("run_id", RUN_IDS)
def test_event_text_is_one_short_line_for_every_event(appendix_runs, run_id):
    for e in appendix_runs[run_id].events:
        text = event_text(e)
        assert text and "\n" not in text and len(text) <= MAX_TEXT
        if e.kind == "chosen":
            assert text.startswith("doing task ") and f"(clock {e.clock})" in text
        elif e.kind == "term":
            assert text == f"n {e.n}, term {e.term}, cleaned {e.cleaned}"


def test_event_text_prints_forms_and_tuples_as_the_trace_does(appendix_runs):
    events = appendix_runs["run1_3737"].events
    guess = [e for e in events if e.kind == "guess"][0]
    assert event_text(guess) == "form (Cycle 3 2 (3 7)), block 3 7 3 7, terms (3 7 3 7)"
    chosen = [e for e in events if e.kind == "chosen" and e.args][0]
    assert event_text(chosen).startswith(f"doing task {chosen.type} on (")
    assert event_text(ev.Message("x" * 400)).endswith("…")
    assert len(event_text(ev.Message("x" * 400))) == MAX_TEXT


# ---- the log of a run ----------------------------------------------------------------------------

@pytest.mark.parametrize("run_id", RUN_IDS)
def test_the_log_keeps_every_event_in_order_with_counts(appendix_runs, run_id):
    ref = appendix_runs[run_id]
    log = logged(ref)
    assert len(log) == len(ref.events) and log.events == ref.events
    assert sum(log.counts.values()) == len(ref.events)
    assert log.counts["start"] == 1 and log.counts["ended"] == 1
    assert log.visible == list(range(len(ref.events)))
    assert log.snapshots == {} and log.last_snapshot is None
    assert all(log.state_at(i) is None for i in range(len(log)))


def test_terms_and_clocks_are_looked_up_both_ways(appendix_runs):
    ref = appendix_runs["run3_121231234"]
    log = logged(ref)
    terms = [(i, e.n) for i, e in enumerate(ref.events) if e.kind == "term"]
    assert log.terms() == (1, 6) and len(terms) == 6
    for i, n in terms:
        assert log.term_index(n) == i and log.term_at(i) == n
        assert log.term_at(i - 1) == (n - 1 if n > 1 else None)
    assert log.term_index(7) is None and log.term_at(0) is None
    assert log.term_at(len(log) - 1) == 6
    clocks = [(i, e.clock) for i, e in enumerate(ref.events) if e.kind == "chosen"]
    assert clocks and log.clocks() == (1, clocks[-1][1]) and clocks[-1][1] == len(clocks)
    for i, c in clocks:
        assert log.clock_index(c) == i and log.clock_at(i) == c
    assert log.clock_at(0) is None and log.clock_index(0) is None
    assert log.clock_at(len(log) - 1) == clocks[-1][1]
    last = clocks[-1][0]
    assert log.clock_at(last + 1) == clocks[-1][1]


def test_rows_follow_the_hidden_kinds(appendix_runs):
    ref = appendix_runs["run1_3737"]
    log = logged(ref)
    log.set_hidden({"posted", "chosen"})
    shown = [i for i, e in enumerate(ref.events) if e.kind not in ("posted", "chosen")]
    assert log.visible == shown
    assert log.row_of(0) == 0
    first_hidden = next(i for i, e in enumerate(ref.events) if e.kind in ("posted", "chosen"))
    assert log.row_of(first_hidden) == shown.index(max(j for j in shown if j <= first_hidden))
    assert log.row_of(len(log) - 1) == len(shown) - 1
    # New events respect the hidden set; set_hidden back shows everything.
    assert log.record(ev.TaskPosted(999, "Tester", (), 10, "cytoplasm", 0)) is False
    assert log.record(ev.Message("hello")) is True
    assert log.visible[-1] == len(log) - 1
    log.set_hidden(())
    assert log.visible == list(range(len(log)))
    assert log.row_of(-1) is None


def test_a_start_event_begins_a_new_log(appendix_runs):
    r1, r3 = appendix_runs["run1_3737"], appendix_runs["run3_121231234"]
    log = EventLog()
    for e in r1.events:
        log.on_event(e)
    assert len(log) == len(r1.events)
    for e in r3.events:
        log.on_event(e)
    assert log.events == r3.events and log.counts["start"] == 1
    assert log.terms() == (1, 6)
    log.clear()
    assert len(log) == 0 and log.terms() is None and log.clocks() is None


def test_snapshots_sit_beside_the_events(appendix_runs):
    ref = appendix_runs["run2_373373"]
    log = EventLog()
    marks = []
    for i, e in enumerate(ref.events):
        snap = ("snap", i) if e.kind in ("chosen", "ended") else None
        log.record(e, snap)
        if snap is not None:
            marks.append(i)
    assert sorted(log.snapshots) == marks
    assert log.last_snapshot == ("snap", marks[-1])
    for i in range(len(log)):
        at = log.snapshot_index(i)
        expected = max((m for m in marks if m <= i), default=None)
        assert at == expected
        assert log.state_at(i) == (None if at is None else ("snap", at))


def test_the_tap_without_a_world_records_events_without_snapshots(appendix_runs):
    ref = appendix_runs["run1_3737"]
    log = EventLog()
    tap = Tap(log)
    for e in ref.events:
        tap.on_event(e)
    assert log.events == ref.events and log.snapshots == {}
    assert tap.source == "template"
