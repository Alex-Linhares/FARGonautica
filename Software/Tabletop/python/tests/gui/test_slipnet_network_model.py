"""The Slipnet network's model (item 08; `models/slipnet_network.py`; G-033 to G-035),
Qt-free: the three bands hold the 63 nodes by level and the lines are the table's 95
links with a stem per label; at every snapshot index of three runs the effective
lengths built from the events equal the snapshot's to a tenth and the context equals the
snapshot's; every labeled link starts at its floor length and the first update of
`surround-05` seed 1 shortens the links "opposite" labels; the banner says "no context
push in this run" on a run with none; on `ch2-fig11` seed 3 the push and the pop are
drawn as their events say, and the banner counts the pair at the end; a new run forgets;
the geometry is pure; the hover texts."""

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop import slipnet_def as sd
from tabletop.config import PROFILES
from tabletop.gui.models import slipnet_network as sn
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.tap import Tap
from tabletop.problems import load_problem
from tabletop.world import World

QUICK = ("surround-05", 3)
PUSH_RUN = ("ch2-fig11", 3)          # the first by seed of the 82 runs with a push in a bounded search
                                     # of 15,900 runs: every problem, seeds 1-300 (G-035; since item 09's
                                     # runs, with the FIGURE, GROUND, ECHO, TWIN and TYPE_KEY detectors;
                                     # `ch2-fig08` seed 2 pushes no more). Since v5's fix (D-116 to D-120): one
                                     # push and one pop of cp4, and still the first seed with a push in 1-300
                                     # (four pushes and four pops, all of cp4, from item 09's runs to v4).
TENTH = 0.051                        # the events round the lengths to a tenth (D-019)
FLOOR = 1 - 0.6 / 150                # a labeled link's share of its length at the floor


def fed(events):
    model = sn.SlipnetNetworkModel()
    for event in events:
        model.on_event(event)
    return model


def tapped(problem, seed):
    """The run with the tap as the world's observer, snapshots after every step."""
    log = EventLog()
    tap = Tap(log)
    world = World(load_problem(problem), V2_PROFILES["standard"], seed, observers=[tap])
    tap.attach(world)
    while not world.finished:
        world.step()
        tap.after_step(world)
    return log


@pytest.fixture(scope="module")
def quick():
    return plain_run(*QUICK, V2)


@pytest.fixture(scope="module")
def push_run():
    return plain_run(*PUSH_RUN, V2)


@pytest.fixture(scope="module")
def push_pop(push_run):
    """The indexes of the push and the pop (the run has one of each since v5's fix,
    D-116 to D-120; four of each from item 09's runs to v4)."""
    events = push_run.events
    push = next(i for i, e in enumerate(events) if isinstance(e, ev.ContextPushed))
    pop = next(i for i, e in enumerate(events) if isinstance(e, ev.ContextPopped))
    return push, pop


# -- the bands and the lines (G-034) --------------------------------------------------------------

def test_the_bands_hold_every_node_by_level_and_the_lines_are_the_table():
    model = sn.SlipnetNetworkModel()
    lay = model.layout(640, 400)
    assert len(lay.dots) == 63 and {d.name for d in lay.dots} == set(sd.NODE_NAMES)
    for level in (1, 2, 3):
        names = [n.name for n in sn.nodes_of_level(level)]
        assert sorted(names) == sorted(n.name for n in sd.NODES if n.level == level)
    assert set(sn.BAND_ORDER[1]) == {n.name for n in sd.NODES if n.level == 1}
    by_level = {level: [d for d in lay.dots if d.level == level] for level in (1, 2, 3)}
    assert max(d.y for d in by_level[3]) < min(d.y for d in by_level[2])
    assert max(d.y for d in by_level[2]) < min(d.y for d in by_level[1])
    assert len(by_level[1]) == 11 and len(by_level[2]) == 42 and len(by_level[3]) == 10
    assert len({round(d.y, 3) for d in by_level[2]}) == 3      # three staggered sub-rows
    # Ten nodes are over STAGGER_ABOVE (8), so the top band is staggered too.
    assert len({round(d.y, 3) for d in by_level[3]}) == 3 == sn.SUB_ROWS
    for dot in lay.dots:
        assert 0 < dot.x < 640 and sn.BANNER_HEIGHT < dot.y < 400 - sn.CAPTION_HEIGHT
        assert dot.fill == 0.0 and dot.activation == 1.0 and 3 <= dot.radius <= 9
    assert [line.name for line in lay.lines] == [link.name for link in sd.LINKS]
    assert len(lay.lines) == 95
    assert sum(line.loop for line in lay.lines) == 4 == sum(l.kind == sd.IDENTITY for l in sd.LINKS)
    assert len(lay.stems) == 36 == sum(len(l.labels) for l in sd.LINKS if l.kind == sd.LABELED)
    assert all(not stem.active for stem in lay.stems)
    assert lay.banner == sn.NO_PUSH and lay.caption.startswith(sn.NOT_FRENCHS)
    assert lay.dot("cup").label == "cup" and lay.line("cup-saucer").labels == ("used with",)
    assert lay.dot("nothing") is None and lay.line("nothing") is None
    assert lay.band_y[0] < lay.band_y[1] < lay.band_y[2]


# -- the lengths from the events (G-033) ---------------------------------------------------------

def test_every_labeled_link_starts_at_its_floor_length():
    model = sn.SlipnetNetworkModel()
    for link in sd.LINKS:
        # The floor is rounded to a tenth, as the engine reports the lengths (D-019).
        expected = round(link.length * FLOOR, 1) if link.kind == sd.LABELED else float(link.length)
        assert model.effective[link.name] == pytest.approx(expected)
        assert not model.shortened(link.name)
    assert sn.floor_length(sn.link_named("cup-saucer"), 0.0) == 25.0
    assert sn.floor_length(sn.link_named("cup-liquid holder"), 0.6) == 15.0


def test_the_first_update_shortens_the_links_opposite_labels(golden_runs):
    events = plain_run("surround-05", 1, V2).events          # the v2 defaults (D-114)
    first = next(i for i, e in enumerate(events) if isinstance(e, ev.SlipnetUpdated))
    model = fed(events[:first + 1])
    lengths = dict(events[first].lengths)
    assert model.activation["opposite"] == 136.2
    opposites = ("open object-closed object", "left-right", "above-below",
                 "mirror symmetry-diagonal symmetry", "figure-ground")
    assert set(model.shortens("opposite")) == set(opposites)
    for name in opposites:
        assert name in lengths and model.effective[name] == lengths[name]
        assert model.shortened(name)
    lay = model.layout(640, 400)
    assert lay.dot("opposite").label_node and lay.dot("opposite").shortening
    assert lay.dot("cup").label_node is False and not lay.dot("cup").shortening
    assert lay.line("left-right").shortened and lay.line("left-right").text == f"{lengths['left-right']:.1f}"
    assert lay.line("left-right").shrink == pytest.approx(1 - lengths["left-right"] / 40)
    active = {(s.label, s.link) for s in lay.stems if s.active}
    assert ("opposite", "left-right") in active and ("diagonal symmetry", "left-right") not in active
    assert lay.dot("cup").fill > 0 and lay.dot("opposite").fill > lay.dot("cup").fill


@pytest.mark.parametrize("problem,seed", [QUICK, ("blockage-06", 2), PUSH_RUN])
def test_the_lengths_and_the_context_equal_the_snapshots(problem, seed):
    log = tapped(problem, seed)
    assert len(log.snapshots.indexes) > 40
    inside = 0
    for index in log.snapshots.indexes:
        model = fed(log.events[:index + 1])
        snapshot = log.state_at(index)
        assert model.clock == snapshot.clock
        for link in snapshot.links:
            name = f"{link['a']}-{link['b']}"
            assert abs(model.effective[name] - link["effective"]) < TENTH, (index, name)
        assert model.snapshot_context() == snapshot.context
        inside += snapshot.context is not None
    # One step ends inside a subworld on the push run, since v5's fix (D-116 to D-120): four
    # (one push each) from item 09's runs to v4, two on `ch2-fig08` seed 2 at item 08.
    assert inside == (1 if (problem, seed) == PUSH_RUN else 0)


# -- the banner on a run with no push (G-035; ANSWERS.md 11) -------------------------------------

def test_the_banner_says_no_push_on_a_run_with_none(quick):
    events = quick.events
    model = sn.SlipnetNetworkModel()
    assert model.banner() == sn.NO_PUSH == "no context push in this run"
    for event in events:
        model.on_event(event)
        assert model.banner() == sn.NO_PUSH and model.context is None
    assert model.ended and model.pushes == [] and model.pops == [] and model.last_pop is None
    assert model.events == len(events) and model.index == len(events) - 1
    assert model.reset_nodes() == frozenset() and model.restored_nodes() == frozenset()
    assert model.layout(300, 200).banner == sn.NO_PUSH


# -- the push and the pop (G-035) ----------------------------------------------------------------

def test_the_push_is_drawn_as_its_event_says(push_run, push_pop):
    events = push_run.events
    push, _pop = push_pop
    event = events[push]
    # Since item 09's runs: examine-cp(cp4) pushes at codelet 73 (event index 849 since v5's
    # fix, D-116 to D-120; 851 from item 09 to v4); on `ch2-fig08` seed 2 before, cp1 at
    # codelet 75 (index 828).
    assert push == 849 and event.correspondence == "cp4" and event.clock == 73
    names = tuple(name for name, _ in event.reset)
    assert names == ("mirror symmetry", "diagonal symmetry", "left", "sameness", "many", "horizontal")
    before = fed(events[:push])
    assert before.banner() == sn.NO_PUSH and before.activation["sameness"] > 60
    model = fed(events[:push + 1])
    assert model.context == sn.Push(push, 73, "cp4", tuple(event.reset))
    assert model.context.nodes == names and model.pushes == [model.context]
    assert model.snapshot_context() == {"correspondence": "cp4", "reset": names, "pushed_at": 73}
    assert model.banner() == ("inside cp4 since codelet 73: 6 nodes reset to 1 (mirror symmetry, "
                              "diagonal symmetry, left, sameness, many, horizontal); pushes 1, pops 0")
    assert all(model.activation[name] == 1.0 for name in names)
    assert model.reset_nodes() == frozenset(names) and model.restored_nodes() == frozenset()
    lay = model.layout(640, 400)
    assert {d.name for d in lay.dots if d.reset} == set(names)
    assert not any(d.restored for d in lay.dots)
    text = model.tooltip("sameness")
    assert "reset to 1 by the push of cp4 at codelet 73 (was 85.8)" in text
    assert text.startswith("sameness: activation 1.0 at codelet 73")
    # The context stays through the subworld's codelet, to the pop.
    later = fed(events[:_pop])
    assert later.context == model.context and later.banner() == model.banner()


def test_the_pop_is_drawn_as_its_event_says(push_run, push_pop):
    events = push_run.events
    _push, pop = push_pop
    event = events[pop]
    # The next codelet, create-cp(spoon4 cup1) at codelet 74, pops (event index 857 since
    # v5's fix, D-116 to D-120; 859 from item 09 to v4).
    assert pop == 857 and event.correspondence == "cp4" and event.clock == 74
    model = fed(events[:pop + 1])
    assert model.context is None and model.pushes and len(model.pops) == 1
    assert model.last_pop == sn.Pop(pop, 74, "cp4", tuple(event.reinitialized),
                                    tuple(event.spread), tuple(event.fixed), tuple(event.restored))
    assert model.snapshot_context() is None
    assert model.banner() == ("popped cp4 at codelet 74: 6 reinitialized, spread into 3, "
                              "fixed +300 over 10; pushes 1, pops 1")
    restored = dict(event.restored)
    # "left", "many" and "horizontal" are restored by the fixed term alone (+60, no spread)
    # on this run, as "many" and "horizontal" were on `ch2-fig08` seed 2 before item 09.
    assert restored["many"] == 61.0 and model.activation["many"] == 61.0
    assert restored["horizontal"] == 61.0 and restored["left"] == 61.0
    assert model.restored_nodes() == frozenset(restored) and model.reset_nodes() == frozenset()
    lay = model.layout(640, 400)
    assert {d.name for d in lay.dots if d.restored} == set(restored)
    assert not any(d.reset for d in lay.dots)
    text = model.tooltip("many")
    assert "restored to 61.0 by the pop of cp4 at codelet 74: +60 fixed (2 slippage concepts)" in text
    assert "+0.0 spread" not in text
    assert "restored to 93.7 by the pop of cp4 at codelet 74: +2.7 spread, +90 fixed (3 slippage concepts)" \
        in model.tooltip("sameness")
    # The marks last through the pop's codelet, and go when the clock moves on.
    same_clock = next(i for i in range(pop + 1, len(events)) if events[i].clock == 74)
    assert fed(events[:same_clock + 1]).restored_nodes() == frozenset(restored)
    next_clock = next(i for i in range(pop + 1, len(events)) if events[i].clock > 74)
    assert next_clock == 869                  # the CodeletChosen of codelet 75 (propose-role-cp); 871 from item 09 to v4
    after = fed(events[:next_clock + 1])
    assert after.restored_nodes() == frozenset() and after.last_pop == model.last_pop
    assert after.banner() == model.banner()
    # This pair is the run's only one since v5's fix (D-116 to D-120): no second push follows,
    # and the banner at the end counts the one pair. (From item 09's runs to v4 examine-cp(cp4)
    # pushed again at codelets 87, 94 and 109, event indexes 994, 1047 and 1228, the next
    # codelet popped each time, indexes 1001, 1054 and 1235, the banner at the end counted
    # four pairs, and at the second pop "many" was restored by a little spread alone.)
    assert [i for i, e in enumerate(events) if isinstance(e, ev.ContextPushed)] == [_push]
    assert [i for i, e in enumerate(events) if isinstance(e, ev.ContextPopped)] == [pop]
    end = fed(events)
    assert end.ended and len(end.pushes) == 1 and len(end.pops) == 1 and end.context is None
    assert end.banner() == ("popped cp4 at codelet 74: 6 reinitialized, spread into 3, "
                            "fixed +300 over 10; pushes 1, pops 1")
    assert end.layout(300, 200).banner == end.banner()


def test_a_new_run_forgets_the_context_and_the_lengths(push_run, quick):
    model = fed(push_run.events)
    assert model.pushes and model.last_pop is not None and model.problem == "ch2-fig11"
    model.on_event(quick.events[0])
    assert model.problem == "surround-05" and model.context is None and model.pushes == []
    assert model.last_pop is None and model.events == 1 and model.banner() == sn.NO_PUSH
    assert model.effective["cup-saucer"] == pytest.approx(25 * FLOOR)
    assert all(a == 1.0 for a in model.activation.values())


# -- the geometry and the hover texts (G-034) ---------------------------------------------------

def test_the_geometry_is_pure_and_finds_what_is_under_a_point(quick):
    model = fed(quick.events)
    a, b = model.layout(500, 300), model.layout(500, 300)
    assert a == b and a is not b
    assert model.layout(500, 300, "cup") != a
    assert (a.width, a.height) == (500.0, 300.0)
    cup = a.dot("cup")
    assert sn.dot_at(a, cup.x, cup.y) is cup
    assert sn.dot_at(a, cup.x + 1.5 * cup.radius, cup.y) is cup
    assert sn.dot_at(a, 2, 2) is None
    line = a.line("cup-saucer")
    mx, my = line.midpoint
    hit = sn.line_at(a, mx, my)
    assert hit is not None and not hit.loop
    assert sn.line_at(a, 1, 1) is None
    assert sn.line_at(a, a.dot("left").x, a.dot("left").y - 10, tolerance=0.1) is None
    chosen = model.layout(500, 300, "cup")
    assert chosen.dot("cup").selected and not chosen.dot("saucer").selected
    assert chosen.line("cup-saucer").selected and chosen.line("cup-liquid holder").selected
    assert not chosen.line("knife-fork").selected
    assert model.selection_of("cup") == ("node", "cup", ())


def test_the_hover_texts(quick):
    model = fed(quick.events)
    text = model.tooltip("opposite")
    assert text.startswith("opposite: activation 150.0 at codelet 59")     # since v5's fix (D-116 to D-120); 147.0 at codelet 44 from item 08's runs to v2, 150.0 at codelet 77 before
    assert "level 3, depth 80" in text
    assert ("label node of open object-closed object, left-right, above-below, "
            "mirror symmetry-diagonal symmetry, figure-ground; shortening") in text
    # The label node's one link of its own is its role link (ROLE_LINKS).
    assert "links (1):" in text and "key - opposite [role], length 40" in text
    cup = model.tooltip("cup").splitlines()
    assert cup[0] == f"cup: activation {model.activation['cup']:.1f} at codelet 59"
    assert cup[1] == "level 1, depth 20" and "label node" not in model.tooltip("cup")
    assert any(l.strip().startswith("cup ISA liquid holder, length 15") for l in cup)
    assert model.link_tooltip("cup-liquid holder") == "cup ISA liquid holder, length 15"
    assert model.link_tooltip("left-left") == "left to itself [mirror symmetry], length 10"
    labeled = model.link_tooltip("left-right")
    assert labeled.startswith("left - right [opposite, diagonal symmetry], length 40, effective ")
    assert "(opposite at 150.0)" in labeled
    assert sn.link_named("cup-saucer").labels == ("used with",)
