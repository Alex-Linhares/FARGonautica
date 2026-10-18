"""The table model (item 03; PANES.md P1; G-017, G-018), Qt-free: the layout of
`surround-05` places every object of the problem file at its position, in its
orientation, with its footprint; after seed 3 the E arrow is on `spoon3` (no label, since
v5's fix D-116 to D-120; `cup2` (E2) since item 08; `small-glass1` (E1) at item 07, `cup2`
(E2) at item 05, `small-glass1` before that) and the H arrow on `cup1`; the marks follow
the events (the descriptor-before-notice case on seed 8, since item 07); a model fed from
snapshots alone equals one fed the problem; an older snapshot never rolls the picture
back; the arrows' geometry; the hover text.

The run facts are v5's (the structural Blockage fix, D-116 to D-120): seed 3 has 59
codelets, 720 events, the answer spoon3 (none), T 43.6, re-pinned from the engine. Item
08's (roles loop0007) were 44 codelets, 507 events, cup2 (E2), T 41.0; re-pinned from the
engine like items 03b, 04b, 05 and 07. Item 09's runs left seed 3 unchanged and seed 8's
events up to spoon1's notice too, as v5's do (seed 8's run diverges later: 65 codelets,
824 events on v5; 86 and 1097 at item 09; 72 and 957 at item 08)."""

import dataclasses

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.gui.models import table as tm
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.run_controller import RunController
from tabletop.gui.models.tap import Tap
from tabletop.objects import footprint
from tabletop.problems import load_problem

QUICK = ("surround-05", 3)       # the front page's quick start: Eliza touches spoon3 (none) since v5
                                 # (D-116 to D-120); cup2 (E2) since item 08
DESCRIBED = ("surround-05", 8)   # find-middle-objs describes spoon1 before noticing it (seed 3
                                 # never notices spoon1 since item 07; it was seed 3 before;
                                 # checked again after item 09's runs: same events)


@pytest.fixture(scope="module")
def problem():
    return load_problem(QUICK[0])


@pytest.fixture(scope="module")
def quick_run():
    return plain_run(*QUICK, V2)


@pytest.fixture(scope="module")
def described_run():
    """The run with a descriptor before a notice (the module's second and last run: the
    `blockage-06` run that `test_a_run_started_of_another_problem` made is a hand-built
    `RunStarted` now, so the module still runs twice)."""
    return plain_run(*DESCRIBED, V2)


@pytest.fixture(scope="module")
def tapped_log(problem):
    """The quick run's `EventLog` with the tap's snapshots (`headless.run`'s way)."""
    log = EventLog()
    tap = Tap(log)
    controller = RunController(observers=[tap], on_world=tap.attach, on_step=tap.after_step)
    controller.start_run(problem, V2_PROFILES["standard"], QUICK[1])
    controller.run_to_end()
    controller.wait()
    assert not controller.is_alive() and controller.outcome.error is None
    assert log.snapshots.last is not None
    return log


def fed(problem, events, upto=None):
    """A model given the problem and the events up to `upto` (inclusive)."""
    model = tm.TableModel()
    model.set_problem(problem)
    for event in events[:None if upto is None else upto + 1]:
        model.on_event(event)
    return model


def test_the_layout_places_every_object_of_the_problem_file(problem):
    model = tm.TableModel()
    model.set_problem(problem)
    lay = model.layout()
    assert (lay.width, lay.height) == (110.0, 100.0)
    assert [o.id for o in lay.objects] == list(problem.ids())
    for o in problem.objects:
        drawn = lay.object(o.id)
        assert (drawn.type, drawn.x, drawn.y, drawn.orientation) == \
            (o.type, o.x, o.y, o.orientation)
        assert (drawn.width, drawn.height) == footprint(o.type, o.orientation)
        assert not drawn.noticed and drawn.region is None and drawn.descriptors == ()
    assert lay.names == (tm.Name("Henry", 55.0, 0.0), tm.Name("Eliza", 55.0, 100.0))
    assert lay.centre == ((0.0, 50.0), (110.0, 50.0))
    assert lay.touch is not None and lay.touch.object == "cup1" and lay.touch.letter == "H"
    assert lay.answer is None and lay.answer_label == ""


def test_a_fork_pointing_left_lies_on_its_side(problem):
    fork = next(o for o in problem.objects if o.type == "fork")
    turned = dataclasses.replace(fork, orientation="left")
    turned_problem = dataclasses.replace(problem, objects=(turned,))
    drawn = tm.table_of_problem(turned_problem)[0]
    assert (drawn.width, drawn.height) == footprint("fork", "left") == (9.6, 3.4)


def test_after_seed_3_the_e_arrow_is_on_spoon3_and_the_h_arrow_on_cup1(problem, quick_run):
    # Since v5's fix (D-116 to D-120) the quick run answers spoon3 after 59 codelets, an
    # object French prints no bar for ("none": the label is empty). Since item 08's runs it
    # was short (44 codelets) and answered cup2 (E2); item 07's answered small-glass1 (E1)
    # after 77 codelets.
    assert quick_run.report == \
        "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."
    model = fed(problem, quick_run.events)
    lay = model.layout()
    assert model.finished and model.answer == ("spoon3", "")
    assert lay.answer.letter == "E" and lay.answer.object == "spoon3"
    assert lay.answer_label == ""
    assert lay.touch.letter == "H" and lay.touch.object == "cup1"
    spoon3, cup1 = lay.object("spoon3"), lay.object("cup1")   # the answered object (spoon3 since v5; cup2 at item 08)
    # The head touches the top edge of the box; the tail sits above, E to the right, H to
    # the left, as the dumps place the letters (G-018).
    assert lay.answer.head[1] == pytest.approx(spoon3.box[1] - 0.4)
    assert lay.answer.tail[1] < lay.answer.head[1] and lay.answer.tail[0] > spoon3.x
    assert lay.touch.head[1] == pytest.approx(cup1.box[1] - 0.4)
    assert lay.touch.tail[1] < lay.touch.head[1] and lay.touch.tail[0] < cup1.x
    # Before the answer there is no E arrow (AnswerGiven is event 718 of 720; 505 of 507 at item 08).
    answered = next(i for i, e in enumerate(quick_run.events) if isinstance(e, ev.AnswerGiven))
    assert answered == 718 and len(quick_run.events) == 720
    assert fed(problem, quick_run.events, answered - 1).layout().answer is None
    assert fed(problem, quick_run.events, answered).layout().answer.object == "spoon3"


def marks_of(problem, events):
    """Replay the events, checking every fiftieth and the last that the model's marks
    are the notices and descriptors so far; returns them."""
    ids = set(problem.ids())
    noticed = {}
    descriptors = {}
    for i, event in enumerate(events):
        if isinstance(event, ev.ObjectNoticed):
            noticed[event.object] = event.region
        elif isinstance(event, ev.DescriptionAdded) and event.item in ids:
            descriptors.setdefault(event.item, [])
            if event.descriptor not in descriptors[event.item]:
                descriptors[event.item].append(event.descriptor)
        if i % 50 == 0 or i == len(events) - 1:
            lay = fed(problem, events, i).layout()
            for o in lay.objects:
                assert o.noticed == (o.id in noticed), (i, o.id)
                assert o.region == noticed.get(o.id), (i, o.id)
                assert o.descriptors == tuple(descriptors.get(o.id, ())), (i, o.id)
    assert fed(problem, events).events == len(events)
    return noticed, descriptors


def test_the_marks_follow_the_events(problem, quick_run, described_run):
    # The quick run notices ten of the thirteen objects and never spoon1 (since item 07;
    # since item 08 the three unnoticed are knife2, spoon1 and spoon2).
    noticed, descriptors = marks_of(problem, quick_run.events)
    assert len(noticed) == 10 and "spoon1" not in noticed and "spoon1" not in descriptors
    assert set(problem.ids()) - set(noticed) == {"knife2", "spoon1", "spoon2"}
    assert descriptors["small-glass1"] and descriptors["cup2"]
    # A descriptor may come before the notice: on seed 8 find-middle-objs describes spoon1
    # as middle three events before noticing it (events 367 and 370, codelet 30, since
    # item 08's runs, and still after item 09's, which change seed 8's run only after
    # the notice; events 210 and 213, codelet 9, at item 07). Seed 3 showed this
    # before item 07 (events 208 and 211, codelet 13); it no longer does.
    noticed, descriptors = marks_of(problem, described_run.events)
    assert descriptors["spoon1"][0] == "middle" and "spoon1" in noticed
    events = described_run.events
    middle = next(i for i, e in enumerate(events)
                  if isinstance(e, ev.DescriptionAdded) and e.item == "spoon1")
    seen = next(i for i, e in enumerate(events)
                if isinstance(e, ev.ObjectNoticed) and e.object == "spoon1")
    assert (middle, seen, events[middle].clock) == (367, 370, 30)


def test_a_model_fed_from_snapshots_alone_equals_one_fed_the_problem(problem, tapped_log):
    log = tapped_log
    blind = tm.TableModel()                 # no problem: the positions come from a snapshot
    for i, event in enumerate(log.events):
        blind.on_event(event)
        blind.update(log.state_at(i))
    expected = fed(problem, log.events)
    assert blind.layout() == expected.layout()
    assert blind.problem == expected.problem == "surround-05"
    assert blind.touched == "cup1"
    # The first events have no snapshot in force (G-006): the blind model is empty then.
    early = tm.TableModel()
    early.on_event(log.events[0])
    early.update(log.state_at(0))
    assert early.layout().objects == () and early.touched == "cup1"


def test_an_older_snapshot_never_rolls_the_picture_back(problem, tapped_log):
    log = tapped_log
    first = log.snapshots.indexes[0]
    model = fed(problem, log.events)
    before = model.layout()
    model.update(log.state_at(first))
    assert model.layout() == before
    assert before.answer is not None


def test_a_run_started_of_another_problem_forgets_the_table(problem, quick_run):
    model = fed(problem, quick_run.events)
    other = load_problem("blockage-06")
    # A hand-built `RunStarted` of blockage-06 (the engine's first event: D-017, D-066),
    # not a run of it: the module's runs are the quick run and the described run.
    start = ev.RunStarted(clock=0, draws=0, problem="blockage-06", seed=1, profile="standard",
                          engine="compiled", changed=(), touched=other.touched, objects=tuple(other.ids()))
    model.on_event(start)
    assert model.problem == "blockage-06" and model.objects == {} and model.answer is None
    assert model.touched == other.touched and model.layout().touch is None
    # The same problem again keeps the objects and clears the run.
    model = fed(problem, quick_run.events)
    model.on_event(quick_run.events[0])
    lay = model.layout()
    assert [o.id for o in lay.objects] == list(problem.ids())
    assert lay.answer is None and not any(o.noticed for o in lay.objects)
    assert model.events == 1 and not model.finished


def test_the_arrow_comes_from_below_near_the_top_edge():
    high = tm.TableObject("cup9", "cup", 20.0, 3.0, None, 7.9, 4.0)
    arrow = tm.arrow_for("H", high, 100.0)
    assert arrow.head[1] == pytest.approx(high.box[3] + 0.4)
    assert arrow.tail[1] > arrow.head[1]
    low = tm.TableObject("cup8", "cup", 20.0, 97.0, None, 7.9, 4.0)
    arrow = tm.arrow_for("E", low, 100.0)
    assert arrow.head[1] == pytest.approx(low.box[1] - 0.4) and arrow.tail[1] < arrow.head[1]


def test_the_hover_text(problem, quick_run):
    model = fed(problem, quick_run.events)
    # The answer is spoon3 since v5's fix (D-116 to D-120), with the empty label of an
    # object French prints no bar for ("none"); it was cup2 (E2) since item 08 and
    # small-glass1 (E1) at item 07. cup2 and small-glass1 are then objects without a mark.
    text = model.tooltip("spoon3")
    assert text.startswith("spoon3: spoon pointing up\nat (13.9, 74.7), Eliza's half; footprint 3.4 x 9.6\n")
    assert "noticed, last seen in the lower-left region" in text
    assert "descriptors: spoon, opposite" in text
    assert text.endswith("Eliza's answer (E, )")
    cup2 = model.tooltip("cup2")
    assert cup2.startswith("cup2: cup\nat (87.6, 77.3), Eliza's half; footprint 7.9 x 4\n")
    assert "noticed, last seen in the lower-right region" in cup2
    assert "descriptors: cup, opposite, isolated" in cup2 and "answer" not in cup2
    other = model.tooltip("small-glass1")
    assert other.startswith("small-glass1: small glass\n"
                            "at (26.1, 75.9), Eliza's half; footprint 4.5 x 5.7\n")
    assert "descriptors: small glass" in other and "answer" not in other
    touched = model.tooltip("cup1")
    assert "Henry's half" in touched and touched.endswith("touched by Henry (H)")
    fork = model.tooltip("fork1")
    assert fork.startswith("fork1: fork pointing up\n")
    assert model.tooltip("teapot") == ""
    fresh = tm.TableModel()
    fresh.set_problem(problem)
    assert "not noticed yet" in fresh.tooltip("fork1")
    assert tm.half_of(model.objects["cup1"], 100.0) == "Henry"
    assert tm.half_of(model.objects["cup2"], 100.0) == "Eliza"
