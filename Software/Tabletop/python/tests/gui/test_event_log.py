"""Loop0006 item 01: the event log (`tabletop/gui/models/event_log.py`), Numbo's
`EventLog` with the engine's events, Qt-free (G-006).

- Loaded with a golden run: the events, their `record_of` records, the counts by name,
  the codelet lookups (`codelet_index`, `codelet_at`, `codelets`), the shown rows, the
  hidden names; a `RunStarted` begins a new log.
- `state_at` without snapshots is None; with the history, the last at or before.
"""

import pytest

from tabletop import events as ev
from tabletop.gui.models.event_log import NAMES, EventLog

from golden import golden_cases


def test_names_are_the_engine_s_event_classes_in_order():
    assert NAMES == tuple(cls.__name__ for cls in ev.EVENT_TYPES)
    assert NAMES[0] == "RunStarted" and NAMES[1] == "RunEnded"
    assert "CodeletChosen" in NAMES and "AnswerGiven" in NAMES


@pytest.mark.parametrize("problem,seed", golden_cases()[:1])
def test_a_loaded_log_indexes_the_run(golden_runs, problem, seed):
    ref = golden_runs[(problem, seed)]
    log = EventLog()
    log.load(ref.events)
    assert len(log) == len(ref.events) == 2690         # surround-05 seed 1 since v5's fix, D-116 to D-120 (3174 in v4, D-115; 1355 in v3, D-114; 901 since item 09; 1204 at item 08; 647 at item 07; 549 at item 05)
    assert tuple(log.events) == ref.events and tuple(log.records) == ref.records
    assert sum(log.counts.values()) == len(log)
    assert log.counts["RunStarted"] == 1 and log.counts["RunEnded"] == 1
    assert log.counts["CodeletChosen"] == ref.result.codelets == 228     # since v5's fix, D-116 to D-120 (304 in v4, D-115; 118 in v3, D-114; 62 since item 09; 89 at item 08; 55 at item 07)
    assert log.codelets() == (1, 228)
    chosen = [i for i, e in enumerate(ref.events) if e.name == "CodeletChosen"]
    for k, index in enumerate(chosen):
        assert log.codelet_index(k + 1) == index
        assert log.codelet_at(index) == k + 1
        if k + 1 < len(chosen):
            assert log.codelet_at(chosen[k + 1] - 1) == k + 1
    assert log.codelet_at(0) == 0 and log.codelet_at(chosen[0] - 1) == 0
    assert log.codelet_index(0) is None and log.codelet_index(305) is None   # v4 (119 in v3)
    assert log.codelet_at(len(log) - 1) == 228
    assert log.events[-1].name == "RunEnded" and log.events[-2].name == "AnswerGiven"
    assert log.state_at(len(log) - 1) is None and log.last_snapshot is None


def test_rows_and_hidden_names(golden_runs):
    ref = golden_runs[golden_cases()[0]]
    log = EventLog()
    shown = [log.on_event(e) for e in ref.events]
    assert all(shown) and log.visible == list(range(len(log)))
    assert log.row_of(0) == 0 and log.row_of(len(log) - 1) == len(log) - 1
    log.set_hidden({"CodeletPosted", "SlipnetUpdated"})
    hidden = {i for i, e in enumerate(ref.events) if e.name in ("CodeletPosted",
                                                                 "SlipnetUpdated")}
    assert log.visible == [i for i in range(len(log)) if i not in hidden]
    assert len(log.visible) < len(log)
    first_hidden = min(hidden)
    assert log.row_of(first_hidden) == log.row_of(first_hidden - 1)
    assert log.row_of(0) == 0
    # A hidden event recorded live is not shown.
    e = ref.events[first_hidden]
    assert log.record(e) is False and log.visible[-1] != len(log) - 1
    log.set_hidden(())
    assert log.visible == list(range(len(log)))


def test_a_run_started_begins_a_new_log(golden_runs):
    a = golden_runs[golden_cases()[0]]
    b = golden_runs[golden_cases()[1]]
    log = EventLog()
    log.load(a.events)
    log.snapshots.record(3, "a snapshot")
    assert log.state_at(10) == "a snapshot"
    for e in b.events:
        log.record(e)
    assert tuple(log.events) == b.events and len(log) == 3830     # surround-05 seed 2: 328 codelets since v5's fix, D-116 to D-120 (253 codelets and 2710 events in v4, D-115; 978 codelets and 9144 events in v3, D-114; 92 codelets and 1162 events since item 09 (99 codelets and 1283 events at item 08; 69 and 843 at item 07; 129 and 1389 at item 05)
    assert log.state_at(10) is None, "the snapshots went with the old run"
    assert log.counts["RunStarted"] == 1
    log.clear()
    assert len(log) == 0 and log.codelets() is None and log.visible == []
