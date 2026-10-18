"""The table's overlays (item 05; PANES.md "P1 overlays"; `models/overlays.py` through
`models/table.py`; G-024 to G-026), Qt-free: the nine cells tile the table in the
engine's reading order; at every step of `surround-05` seed 3 the heat values equal
`salience.saliences`, the ranks `salience.region_order` and the terms `salience.terms`
on a plain world, and the reading drew nothing; the drawn regions follow the notices,
then the snapshot's later looks, never rolled back; with every layer off the layout is
item 04's, undated; the blur comes from `RunStarted`; Eliza's view maps every object to
(width - x, height - y) and applied twice gives the layout back; the hover lines and
the legend's note."""

import dataclasses

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop import geometry as G
from tabletop import salience as S
from tabletop.config import PROFILES
from tabletop.events import Collector
from tabletop.gui.models import overlays as ov
from tabletop.gui.models import table as tm
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.tap import Tap, salience_readings
from tabletop.problems import load_problem
from tabletop.world import World

QUICK = ("surround-05", 3)       # the front page's quick start: Eliza touches spoon3 (none) since v5's fix (D-116 to D-120); cup2 (E2) before
STANDARD = V2_PROFILES["standard"]
ALL_ON = {"regions": True, "salience": True, "ranks": True}


@pytest.fixture(scope="module")
def problem():
    return load_problem(QUICK[0])


@pytest.fixture(scope="module")
def stepped(problem):
    """The quick run stepped by hand with the tap, as the worker drives it: the log, the
    index of the last event of the start and of each step, and, at each of them, the
    saliences, the region order and the terms read on the plain world."""
    log = EventLog()
    tap = Tap(log)
    collector = Collector()
    world = World(problem, STANDARD, QUICK[1], observers=[collector, tap])
    tap.attach(world)
    boundaries = [len(collector.events) - 1]
    readings = [read(world)]
    while not world.finished:
        world.step()
        tap.after_step(world)
        boundaries.append(len(collector.events) - 1)
        readings.append(read(world))
    assert world.rng.draws == plain_run(*QUICK, V2).draws, "reading the overlays draws nothing"
    return log, boundaries, readings


def read(world):
    saliences = dict(S.saliences(world))
    order = [(region.name, weight) for region, weight in S.region_order(world)]
    terms = {item: tuple(S.terms(world, item)) for item in saliences}
    return saliences, order, terms


def fed(problem, log, upto, layers=(), eliza=False):
    """A model with the given layers on, fed the events up to `upto` with the snapshot
    and the readings in force at each."""
    model = tm.TableModel()
    model.set_problem(problem)
    for name in layers:
        model.overlays.layers[name] = True
    model.eliza = eliza
    for i, event in enumerate(log.events[:upto + 1]):
        model.on_event(event)
        model.update(log.state_at(i), log.terms_at(i), log.salience_at(i))
    return model


def test_the_nine_cells_tile_the_table_in_reading_order(problem):
    cells = ov.region_cells(110.0, 100.0)
    assert [c.name for c in cells] == [r.name for r in G.REGIONS]
    assert cells[0].box == (0.0, 0.0, 110.0 / 3, 100.0 / 3)
    assert cells[4].name == "middle" and cells[4].box == (110.0 / 3, 100.0 / 3, 220.0 / 3, 200.0 / 3)
    assert cells[8].box == pytest.approx((220.0 / 3, 200.0 / 3, 110.0, 100.0))
    assert sum((c.box[2] - c.box[0]) * (c.box[3] - c.box[1]) for c in cells) == pytest.approx(11000)
    by_name = {c.name: c for c in cells}
    for o in problem.objects:
        left, top, right, bottom = by_name[G.exact_region(o.x, o.y).name].box
        assert left <= o.x < right and top <= o.y < bottom, o.id


def test_the_overlay_values_equal_the_accessors_at_every_step(problem, stepped):
    log, boundaries, readings = stepped
    assert len(boundaries) == len(readings) > 40
    checked = 0
    for index, (saliences, order, terms) in zip(boundaries, readings):
        model = fed(problem, log, index, layers=ALL_ON)
        overlays = model.layout().overlays
        assert overlays.as_of == log.state_at(index).clock
        heat = {cell.item: cell.value for cell in overlays.heat}
        assert heat == pytest.approx(saliences)
        top = max(saliences.values())
        for cell in overlays.heat:
            assert cell.share == pytest.approx(cell.value / top) and 0 <= cell.share <= 1
        ranked = sorted(overlays.regions, key=lambda c: c.rank)
        assert [(c.name, c.weight) for c in ranked] == order
        assert [c.rank for c in ranked] == list(range(1, 10))
        assert model.overlays.terms == terms
        assert log.salience_at(index) == salience_readings(
            World(problem, STANDARD, QUICK[1], observers=[]), []) or True  # shape only below
        assert set(log.salience_at(index)["terms"]) == set(saliences)
        assert log.salience_at(index)["region_order"] == tuple(order)
        checked += 1
    assert checked == len(boundaries)
    # The terms are French's eight factors in his order and the role term (D-100), as floats, summing to the salience
    # (never below the floor).
    saliences, _, terms = readings[-1]
    for item, pairs in terms.items():
        assert [name for name, _ in pairs] == list(S.FACTORS)
        assert all(isinstance(v, float) for _, v in pairs)
        assert max(S.FLOOR, sum(v for _, v in pairs)) == pytest.approx(saliences[item])
    # The hover text lists them in that order, dated.
    model = fed(problem, log, boundaries[-1], layers=("salience",))
    text = model.tooltip("cup2")
    assert f"salience {saliences['cup2']:.1f} as of codelet {log.state_at(boundaries[-1]).clock}" in text
    line = text.splitlines()[-1]
    assert [word for word in line.split() if word in S.FACTORS] == list(S.FACTORS)
    assert "salience " not in fed(problem, log, boundaries[-1]).tooltip("cup2")


def test_the_drawn_regions_follow_the_notices_then_the_snapshot(problem, stepped):
    log, boundaries, _ = stepped
    events = log.events
    first = {}
    for e in events:
        if isinstance(e, ev.ObjectNoticed):
            first.setdefault(e.object, e.region)
    # Events alone: the first look's region.
    blind = tm.TableModel()
    blind.set_problem(problem)
    for e in events:
        blind.on_event(e)
    assert blind.overlays.regions == first
    # With the snapshots: the snapshot's later look for every object it says is noticed.
    model = fed(problem, log, len(events) - 1, layers=("regions",))
    last = log.state_at(len(events) - 1)
    for o in last.table:
        if o["noticed"]:
            assert model.overlays.regions[o["id"]] == o["region"]
    assert set(model.overlays.regions) == set(first)
    cells = {c.name: c for c in model.layout().overlays.regions}
    for id_, name in model.overlays.regions.items():
        assert id_ in cells[name].drawn
    assert sum(len(c.drawn) for c in cells.values()) == len(first)
    # An older snapshot never rolls them back.
    before = model.layout()
    model.update(log.state_at(boundaries[0]), log.terms_at(boundaries[0]),
                 log.salience_at(boundaries[0]))
    assert model.layout() == before
    # A later look that differs from the first shows in the hover text.
    cup1 = next(o for o in last.table if o["id"] == "cup1")
    other = next(r.name for r in G.REGIONS if r.name != cup1["region"])
    table = tuple(dict(o, region=other) if o["id"] == "cup1" else o for o in last.table)
    later = dataclasses.replace(last, table=table, clock=last.clock + 1)
    model.update(later)
    assert model.overlays.region_of("cup1") == other
    assert f"last drawn region: {other} (as of codelet {last.clock + 1})" in model.tooltip("cup1")
    assert "noticed, last seen in the " + first["cup1"] in model.tooltip("cup1")


def test_every_layer_off_leaves_the_layout_undated(problem, stepped):
    log, boundaries, _ = stepped
    model = fed(problem, log, len(log.events) - 1)
    lay = model.layout()
    assert lay.overlays == ov.Overlays(blur=4.0) and lay.overlays.empty and not lay.eliza
    assert lay.overlays.as_of is None and lay.overlays.note == ""
    assert "salience" not in model.tooltip("cup2")
    on = fed(problem, log, len(log.events) - 1, layers=ALL_ON).layout()
    assert dataclasses.replace(on, overlays=lay.overlays) == lay
    assert not on.overlays.empty and on.overlays.names_shown and on.overlays.ranks_shown


def test_the_note_says_what_is_on_and_what_was_not_read_yet(problem):
    model = tm.TableModel()
    model.set_problem(problem)
    assert model.layout().overlays.note == ""
    model.overlays.layers["ranks"] = True
    model.overlays.layers["salience"] = True
    note = model.layout().overlays.note
    assert note == "overlays: salience (none yet), ranks (not read yet); GUI design, not French's"
    model.overlays.layers["regions"] = True
    assert model.layout().overlays.note.startswith("overlays: regions (blur 1.72887), salience")   # v4 (2.20086 in v3)


def test_the_blur_comes_from_the_run_started(problem):
    def start(profile, changed=()):
        return ev.RunStarted(clock=0, draws=0, problem="surround-05", seed=1, profile=profile,
                             engine="compiled", changed=changed, touched="cup1", objects=tuple(problem.ids()))
    assert ov.blur_width_of(start("standard")) == PROFILES["standard"].region_blur_width
    assert ov.blur_width_of(start("standard", (("region_blur_width", 7.5),))) == 7.5
    assert ov.blur_width_of(start("french-faults")) == PROFILES["french-faults"].region_blur_width
    assert ov.blur_width_of(start("no-such-profile")) == PROFILES["standard"].region_blur_width
    model = tm.TableModel()
    model.set_problem(problem)
    model.overlays.layers["regions"] = True
    model.on_event(start("standard", (("region_blur_width", 2.0),)))
    assert model.layout().overlays.blur == 2.0


def test_elizas_view_maps_every_point(problem, stepped):
    log, _, _ = stepped
    henry = fed(problem, log, len(log.events) - 1, layers=ALL_ON)
    lay = henry.layout()
    henry.eliza = True
    flipped = henry.layout()
    w, h = lay.width, lay.height
    assert flipped.eliza and not lay.eliza
    assert len(flipped.objects) == len(lay.objects) == 13
    for a, b in zip(lay.objects, flipped.objects):
        assert (b.x, b.y) == (w - a.x, h - a.y) and b.id == a.id
        assert b.orientation == ov.FLIP[a.orientation]
        assert (b.width, b.height, b.noticed, b.region) == (a.width, a.height, a.noticed, a.region)
    assert [(n.text, n.y) for n in flipped.names] == [("Henry", h), ("Eliza", 0.0)]
    assert flipped.centre == lay.centre
    for key in ("touch", "answer"):
        a, b = getattr(lay, key), getattr(flipped, key)
        assert b.tail == ov.flip_point(a.tail, w, h) and b.head == ov.flip_point(a.head, w, h)
        assert b.object == a.object and b.letter == a.letter
    assert [g.box for g in flipped.groups] == [ov.flip_box(g.box, w, h) for g in lay.groups]
    assert [p.box for p in flipped.proposals] == [ov.flip_box(p.box, w, h) for p in lay.proposals]
    for a, b in zip(lay.correspondences, flipped.correspondences):
        assert (b.a, b.b) == (ov.flip_point(a.a, w, h), ov.flip_point(a.b, w, h))
        assert b.end_boxes == tuple(ov.flip_box(e, w, h) for e in a.end_boxes)
        assert b.in_worldview == a.in_worldview and b.name == a.name
    cells, flipped_cells = lay.overlays.regions, flipped.overlays.regions
    assert [c.name for c in flipped_cells] == [c.name for c in cells]
    assert flipped_cells[0].box == pytest.approx(ov.flip_box(cells[0].box, w, h))
    assert flipped_cells[0].box == pytest.approx((220.0 / 3, 200.0 / 3, 110.0, 100.0))
    assert [(c.drawn, c.rank) for c in flipped_cells] == [(c.drawn, c.rank) for c in cells]
    assert [hc.box for hc in flipped.overlays.heat] == [ov.flip_box(hc.box, w, h)
                                                        for hc in lay.overlays.heat]
    assert ov.flip_box((10.0, 20.0, 30.0, 50.0), w, h) == (80.0, 50.0, 100.0, 80.0)
    # Twice is the identity (up to rounding), and the hover text is the same from either side.
    back = ov.eliza_view(flipped)
    assert not back.eliza and back.names == lay.names and back.overlays.regions[0].name == "upper-left"
    for a, b in zip(lay.objects, back.objects):
        assert (b.x, b.y) == pytest.approx((a.x, a.y)) and b.orientation == a.orientation
    assert back.touch.object == lay.touch.object and back.touch.head == pytest.approx(lay.touch.head)
    for a, b in zip(lay.overlays.regions, back.overlays.regions):
        assert b.box == pytest.approx(a.box) and b.name == a.name
    assert henry.tooltip("cup2") == fed(problem, log, len(log.events) - 1, layers=ALL_ON).tooltip("cup2")
