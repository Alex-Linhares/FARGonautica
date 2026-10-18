"""Loop0004 item 07: the taskrack pane's model (`seekwhence/gui/models/taskrack.py`),
Qt-free (G-027, G-028, G-029).

- The shadow rack equals `Taskrack.tasks` at every event of the three appendix runs and
  of blackburn-1 (which has cleanings), read by an independent observer, and equals the
  snapshot's rack at every snapshot, with the dense rule and with keyframes only; the
  snapshot never has to correct it.
- The chances sum to 1: at every draw, the chosen task's and those of the tasks left
  waiting; and the waiting tasks' at the next draw (DECISION D-007).
- Run 1's first task, `doing task Sparkler-plus on (glint1 glint1 2)` [M86 p. 164], its
  chance and its staleness; the Hfilter run at once (DECISION D-012).
- The rows by type in order of first appearance, with their shares; the arena filter
  (DECISION D-057); the cleaning rule and the banner (DECISION D-010); the tallies since
  the last term; the names a task's arguments select; the lines; a 26,000-task history
  in bounded time.
"""

import io
import time

import pytest

from seekwhence import bench, events
from seekwhence import sequences as sq
from seekwhence.config import Config
from seekwhence.gui.models import taskrack as tr
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, appendix_inputs

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)
# A longer run with cleanings (two `hypothesis` cleanings, DECISION D-010) and about
# 4,000 tasks: blackburn-1 on seed 1, fed as the bench feeds it.
CLEANED = sq.find("blackburn-1")


class RackReader:
    """An independent observer: the numbers of the tasks on the rack at every event,
    read from `world.taskrack.tasks` (an attribute read, ENGINE-API.md section 2)."""

    def __init__(self):
        self.world = None
        self.racks = []

    def on_event(self, event):
        rack = self.world.taskrack.tasks if self.world is not None else ()
        self.racks.append(tuple(t.number for t in rack))


def logged(run_id=None, dense_tasks=None, sequence=None, seed=1):
    """A run's `EventLog` with the tap's snapshots and the reader's racks."""
    if sequence is None:
        config, lines, answers = appendix_inputs(run_id)
        seed = SEEDS[run_id]
        extra = {}
    else:
        config, lines, answers = Config(), [str(t) for t in sequence.prefix(16)], []
        extra = {"answerer": lambda console: bench.ContinuationAnswerer(console, sequence.terms)}
    log, reader = EventLog(), RackReader()
    tap = Tap(log) if dense_tasks is None else Tap(log, dense_tasks=dense_tasks)
    session = Session(config, seed, out=io.StringIO(), lines=list(lines),
                      answers=list(answers), observers=[reader, tap], **extra)
    reader.world = session.world
    tap.attach_session(session)
    session.run()
    return log, reader


@pytest.fixture(scope="module")
def logs():
    return {run_id: logged(run_id) for run_id, _ in APPENDIX}


def walk(log, check=None):
    """The model fed every event with the snapshot in force, as the view feeds it
    (G-027); `check(index, model)` after each."""
    model = tr.TaskrackModel()
    for i, event in enumerate(log.events):
        model.on_event(event)
        model.update(log.state_at(i))
        if check is not None:
            check(i, model)
    return model


def model_at(log, index):
    model = tr.TaskrackModel()
    for i in range(index + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def indexes(log, kind):
    return [i for i, e in enumerate(log.events) if e.kind == kind]


# -- the shadow equals the rack ---------------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_the_shadow_equals_the_rack_at_every_event(logs, run_id):
    log, reader = logs[run_id]
    at_snapshots = 0

    def check(i, model):
        nonlocal at_snapshots
        assert tuple(model.waiting) == reader.racks[i], i
        snapshot = log.state_at(i)
        if snapshot is not None and log.snapshot_index(i) == i:
            at_snapshots += 1
            assert model.tasks == snapshot.rack.tasks
            assert model.total_weight == snapshot.rack.total_weight
            assert tuple((r.type, r.count) for r in model.rows()) == snapshot.rack.counts

    model = walk(log, check)
    assert model.corrections == 0
    assert len(model.history) == len(indexes(log, "chosen"))
    assert at_snapshots == len(indexes(log, "chosen")) + len(indexes(log, "prompt")) \
        + len(indexes(log, "guess")) + len(indexes(log, "answered")) + 1        # G-003


def test_the_shadow_holds_with_keyframes_only_and_with_cleanings():
    log, reader = logged(sequence=CLEANED, dense_tasks=0)
    cleanings = [log.events[i] for i in indexes(log, "cleaned")]
    assert [c.moment for c in cleanings] == ["hypothesis", "hypothesis"]    # D-010

    def check(i, model):
        assert tuple(model.waiting) == reader.racks[i], i

    model = walk(log, check)
    assert model.corrections == 0
    assert model.cleaning is not None and model.cleaning.moment == "hypothesis"
    log3, reader3 = logged(RUN3, dense_tasks=0)
    model3 = walk(log3, lambda i, m: tuple(m.waiting) == reader3.racks[i] or pytest.fail(str(i)))
    assert model3.corrections == 0


# -- the chances -------------------------------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN3])
def test_the_chances_sum_to_1(logs, run_id):
    log, _ = logs[run_id]
    draws = 0

    def check(i, model):
        nonlocal draws
        chances = model.chances()
        if model.waiting:
            assert abs(sum(chances.values()) - 1) < 1e-9
            assert set(chances) == set(model.waiting)
        if log.events[i].kind == "chosen":
            at_draw = model.draw_chances()
            c = model.chosen
            if c.number is None or c.at_once:
                assert at_draw == {} and c.chance is None
            else:
                draws += 1
                assert abs(sum(at_draw.values()) - 1) < 1e-9
                assert at_draw[c.number] == c.chance
                assert set(at_draw) == set(model.waiting) | {c.number}
                snapshot = log.state_at(i)
                if log.snapshot_index(i) == i:
                    assert c.chance == c.weight / (snapshot.rack.total_weight + c.weight)

    walk(log, check)
    assert draws > 0


def test_the_weight_is_the_urgency_to_the_power(logs):
    log, _ = logs[RUN1]
    assert dict(log.events[0].config)["urgency_power"] == 1             # D-007
    model = walk(log)
    assert all(c.weight == c.urgency for c in model.history)
    power = tr.TaskrackModel()
    power.on_event(events.RunStarted(1, (("urgency_power", 2),)))
    power.on_event(events.TaskPosted(1, "Tester", ("spark1",), 3, "cytoplasm", 0))
    assert power.tasks[0].weight == 9


# -- run 1 [M86 p. 164] ------------------------------------------------------------------------

def test_run_1s_first_tasks_and_their_chances(logs):
    """The first term's glint sets out its Sparkler-plus (urgency 2) and a Dissolver
    (urgency 1); the Dissolver is drawn first, with a third of the weight, then the
    Sparkler-plus alone on the rack: `doing task Dissolver`, `doing task Sparkler-plus on
    (glint1 glint1 2)` [M86 p. 164]."""
    log, _ = logs[RUN1]
    first, second = indexes(log, "chosen")[:2]
    model = model_at(log, first)
    c = model.chosen
    assert tr.chosen_text(c) == "doing task Dissolver"                        # [M86 p. 164]
    assert c.clock == 1 and c.urgency == 1 and c.arena == "cytoplasm"
    assert c.chance == pytest.approx(1 / 3) and tr.chance_text(c.chance) == "33.3%"
    assert model.chosen_fresh and model.history == [c]
    assert model.tallies == {"Dissolver": 1}
    lines = tr.rack_lines(model)
    assert lines[0].kind == "chosen" and lines[0].tint and not lines[0].grey
    assert lines[0].text == tr.chosen_text(c) and lines[0].arena == "cytoplasm"
    assert lines[1].kind == "note" and "chance 33.3% (weight 1 of 3 before the draw)" in lines[1].text
    model = model_at(log, second)
    c = model.chosen
    assert tr.chosen_text(c) == "doing task Sparkler-plus on (glint1 glint1 2)"   # [M86 p. 164]
    assert c.clock == 2 and c.chance == 1.0 and model.waiting == {}
    assert model.tallies == {"Dissolver": 1, "Sparkler-plus": 1}
    assert not model_at(log, second + 1).chosen_fresh


def test_a_stale_task_is_marked_from_its_stale_event(logs):
    log, _ = logs[RUN1]
    stale = indexes(log, "stale")[0]
    event = log.events[stale]
    before = model_at(log, stale - 1)
    assert not before.chosen.stale and not before.history[-1].stale
    model = model_at(log, stale)
    c = model.chosen
    assert c.stale and c.reason == event.reason and c.type == event.type
    assert model.history[-1] is c
    assert tr.chosen_text(c).endswith("  stale") and tr.history_text(c).endswith("  stale")
    lines = tr.rack_lines(model)
    assert lines[0].grey and lines[1].grey and event.reason in lines[0].tooltip


def test_the_hfilter_is_run_at_once(logs):
    log, _ = logs[RUN3]
    chosen = [i for i in indexes(log, "chosen") if log.events[i].type == "Hfilter"]
    assert len(chosen) == 4             # one per term entered under a hypothesis [M86 pp. 172-179]
    for i in chosen:
        assert log.events[i - 1].kind == "posted"                       # D-012
        model = model_at(log, i)
        c = model.chosen
        assert c.at_once and c.chance is None and model.draw_chances() == {}
        assert "run at once ahead of any draw (DECISION D-012)" in tr.rack_lines(model)[1].text
    model = walk(log)
    assert not any(c.at_once for c in model.history if c.type != "Hfilter")


# -- the rows, the filter, the tallies ------------------------------------------------------

def test_the_rows_group_by_type_with_their_shares(logs):
    log, _ = logs[RUN3]
    fullest = max(range(len(log.events)), key=lambda i: len(model_at(log, i).waiting)
                  if log.events[i].kind == "chosen" and log.snapshot_index(i) == i else -1)
    index = fullest
    model = model_at(log, index)
    assert len(model.waiting) >= 20 and model.max_waiting == 29   # run 3's rack at its fullest
    rows = model.rows()
    assert [r.type for r in rows] == [t for t, _ in log.state_at(index).rack.counts]
    assert sum(r.count for r in rows) == len(model.waiting)
    assert sum(r.weight for r in rows) == model.total_weight
    assert abs(sum(r.share for r in rows) - 1) < 1e-9
    for row in rows:
        assert all(t.type == row.type and t.arena == row.arena for t in row.tasks)
        assert row.weight == sum(t.weight for t in row.tasks)
    assert {r.arena for r in model.rows("cytoplasm")} <= {"cytoplasm"}
    assert sum(len(model.rows(a)) for a in tr.ARENAS) == len(rows)


def test_the_tallies_count_the_tasks_since_the_last_term(logs):
    log, _ = logs[RUN1]
    model = walk(log)
    last_term = indexes(log, "term")[-1]
    expected = {}
    for e in log.events[last_term + 1:]:
        if e.kind == "chosen":
            expected[e.type] = expected.get(e.type, 0) + 1
    assert model.tallies == expected and model.terms == 4
    assert tr.tally_text(model).startswith("since term 4: ")
    fresh = tr.TaskrackModel()
    assert tr.tally_text(fresh) == "since the start: nothing chosen"


def test_the_cleaning_rule_and_the_banner():
    """A synthetic run: Sparklers posted before and after a term; the `term` cleaning
    removes the ones posted before the previous term, a `hypothesis` cleaning every
    task of its types (DECISION D-010)."""
    model = tr.TaskrackModel()
    model.on_event(events.RunStarted(1, (("urgency_power", 1),)))
    model.on_event(events.TermEntered(1, 3, 0))
    for n in (1, 2, 3):
        model.on_event(events.TaskPosted(n, "Sparkler", (), 5, "cytoplasm", 0))
    model.on_event(events.TaskPosted(4, "Tester", ("spark1",), 10, "cytoplasm", 0))
    model.on_event(events.TaskPosted(5, "Template-scout", (), 20, "cytoplasm", 0))
    model.on_event(events.TermEntered(2, 7, 0))
    model.on_event(events.TaskPosted(6, "Sparkler", (), 5, "cytoplasm", 2))
    assert [t.at_term for t in model.tasks] == [1, 1, 1, 1, 1, 2]
    # Entering the third term: the Sparklers and the Tester posted before term 2 go.
    model.on_event(events.TasksCleaned("term", (("Sparkler", 3), ("Tester", 1))))
    assert tuple(model.waiting) == (5, 6)
    assert tr.cleaning_text(model.cleaning) == "term: removed 3 Sparkler, 1 Tester"
    assert model.banner() is model.cleaning
    lines = tr.rack_lines(model)
    assert [l.kind for l in lines[:3]] == ["chosen", "banner", "note"]
    assert lines[1].text == "term: removed 3 Sparkler, 1 Tester"
    model.on_event(events.TermEntered(3, 3, 4))
    model.on_event(events.TasksCleaned("hypothesis", (("Template-scout", 1),)))
    assert tuple(model.waiting) == (6,)
    for _ in range(tr.BANNER - 1):
        model.on_event(events.TaskPosted(100 + _, "Tester", ("spark2",), 10, "cytoplasm", 3))
    assert model.banner() is not None
    model.on_event(events.TaskPosted(999, "Tester", ("spark2",), 10, "cytoplasm", 3))
    assert model.banner() is None and model.cleaning is not None
    assert tr.cleaning_text(None) == ""


def test_a_count_the_rule_does_not_give_removes_the_oldest():
    model = tr.TaskrackModel()
    model.on_event(events.RunStarted(1, ()))
    for n in (1, 2, 3):
        model.on_event(events.TaskPosted(n, "Sparkler", (), 5, "cytoplasm", 0))
    model.on_event(events.TasksCleaned("term", (("Sparkler", 2),)))
    assert tuple(model.waiting) == (3,)


# -- the selection and the names ---------------------------------------------------------------

def test_the_names_a_tasks_arguments_select():
    assert tr.names_in(("Same", "print-value", "(remote)", "glint1", "glint3")) == \
        ("glint1", "glint3")
    assert tr.names_in(("(Succ print-value group (glint13 glint14 glint15))", "10")) == \
        ("glint13", "glint14", "glint15")
    assert tr.names_in(("glom2", "glom2", "bond1", "spark3", "gnoth1", "5")) == \
        ("glom2", "bond1", "spark3", "gnoth1")
    assert tr.names_in(()) == ()


def test_select_task_toggles_the_shared_selection(logs):
    log, _ = logs[RUN1]
    selection = Selection()
    model = tr.TaskrackModel(selection)
    for i, event in enumerate(log.events):
        model.on_event(event)
        model.update(log.state_at(i))
    waiting = next(t for t in model.tasks if tr.names_in(t.args))
    names = model.select_task(waiting.number)
    assert names == tr.names_in(waiting.args)
    assert selection.kind == "task" and selection.name == f"task{waiting.number}"
    assert all(selection.includes(n) for n in names)
    assert model.names_selected(waiting.args)
    assert any(l.selected for l in tr.rack_lines(model, {waiting.type}) if l.kind == "task")
    assert model.select_task(waiting.number) == names and selection.empty      # toggled off
    first = model.history[1]            # the Sparkler-plus on (glint1 glint1 2) [M86 p. 164]
    assert model.select_task(first.number) == ("glint1",)
    assert selection.includes("glint1")
    assert model.select_task(424242) == () and selection.includes("glint1")
    selection.select("glint", "glint1")
    assert model.names_selected(first.args) and tr.rack_lines(model)[0].selected is False


# -- the lines ---------------------------------------------------------------------------------

def test_the_rack_lines_list_the_expanded_types(logs):
    log, _ = logs[RUN2]
    model = model_at(log, indexes(log, "prompt")[3])
    rows = model.rows()
    assert rows
    folded = tr.rack_lines(model)
    assert [l.kind for l in folded] == ["chosen", "note", "note"] + ["row"] * len(rows)
    assert [l.text for l in folded if l.kind == "row"] == [r.type for r in rows]
    assert [l.count for l in folded if l.kind == "row"] == [str(r.count) for r in rows]
    assert [l.bar for l in folded if l.kind == "row"] == [r.share for r in rows]
    opened = tr.rack_lines(model, {rows[0].type})
    tasks = [l for l in opened if l.kind == "task"]
    assert len(tasks) == rows[0].count and opened[3].kind == "row"
    assert [l.number for l in tasks] == [t.number for t in rows[0].tasks]
    assert tasks[0].text == tr.task_text(rows[0].tasks[0])
    assert tasks[0].text.startswith(f"#{rows[0].tasks[0].number}  ")
    assert f"urgency {rows[0].tasks[0].urgency}  posted at {rows[0].tasks[0].created_at}" \
        in tasks[0].text
    cyto = tr.rack_lines(model, arena="cytoplasm")
    assert {l.arena for l in cyto if l.kind == "row"} <= {"cytoplasm"}
    assert f"{len(model.waiting)} waiting (at most {model.max_waiting} this run)" in folded[2].text


def test_the_texts():
    assert tr.chosen_text(None) == "nothing chosen yet"
    assert tr.format_args(()) == "" and tr.format_args(("glint1", "10")) == "(glint1 10)"
    assert tr.chance_text(None) == "" and tr.chance_text(0.125) == "12.5%"
    empty = tr.ChosenTask(7, None, "empty-task", (), 1, "user", 1, None, 9)
    assert tr.chosen_text(empty) == "doing task empty-task"                  # D-013
    assert tr.history_text(empty) == "7  doing task empty-task"
    model = tr.TaskrackModel()
    model.on_event(events.RunStarted(1, ()))
    model.on_event(events.TaskChosen(1, None, "empty-task", (), 1, "user"))
    assert model.chosen.chance is None and model.draw_chances() == {}
    assert tr.rack_lines(model)[1].text == "clock 1, urgency 1"


def test_a_start_resets_the_model(logs):
    log, _ = logs[RUN1]
    model = walk(log)
    assert model.history and model.waiting
    model.on_event(log.events[0])
    assert model.history == [] and model.waiting == {} and model.chosen is None
    assert model.tallies == {} and model.terms == 0 and model.events == 1


def test_a_26000_task_history_in_bounded_time():
    model = tr.TaskrackModel()
    model.on_event(events.RunStarted(1, (("urgency_power", 1),)))
    start = time.perf_counter()
    for n in range(1, 26001):
        model.on_event(events.TaskPosted(n, "Tester", (f"spark{n}",), 10, "cytoplasm", n - 1))
        model.on_event(events.TaskChosen(n, n, "Tester", (f"spark{n}",), 10, "cytoplasm"))
        if n % 1000 == 0:
            tr.rack_lines(model)
    elapsed = time.perf_counter() - start
    assert len(model.history) == 26000 and model.waiting == {}
    assert elapsed < 5.0, elapsed
    start = time.perf_counter()
    for _ in range(100):
        tr.rack_lines(model)
        tr.history_text(model.history[-1])
    assert time.perf_counter() - start < 0.5
