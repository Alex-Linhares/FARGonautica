"""loop0003 item 9: the Pnet view and the coderack view
(numbo/gui/pnet_view.py, numbo/gui/coderack_view.py).

pytest-qt, offscreen.  Each view holds its model on the GUI thread; on_event
updates the model, and redraw() brings the items in line and paints now.
These tests check:
  - the Pnet view's 88 items and its edges equal the structure and the
    layout, and after every event each item's style equals what a shadow
    PnetModel fed the same events says (shading by activation, cytoplasm
    instances marked, config's pseudo-instances not, the latest pnode
    change highlighted);
  - the coderack view's rows equal a shadow CoderackModel's bins after
    every event (counts, the codelets waiting, the chosen codelet and its
    bin highlighted);
  - redraw() paints now, on_event doesn't;
  - the main window has both as docks and draws a run in them;
  - render_png writes each view to a PNG.
"""

import io
import math

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPointF, QRectF                  # noqa: E402
from PySide6.QtGui import QImage                            # noqa: E402

import full_runs                                            # noqa: E402
from numbo import harness, observe                          # noqa: E402
from numbo.gui import coderack_view as cv                   # noqa: E402
from numbo.gui import controls as controls_module           # noqa: E402
from numbo.gui import pnet_view as pv                       # noqa: E402
from numbo.gui.main_window import MainWindow                # noqa: E402
from numbo.models.coderack_model import Choice, Codelet, CoderackModel  # noqa: E402
from numbo.models.pnet_model import PnetModel, pnet_layout, pnet_structure  # noqa: E402

TIMEOUT = 30000


def record(problem, seed, cap=20000):
    evs = []

    class R:
        def on_event(self, event):
            evs.append(event)

    harness.run_config(list(problem), seed=seed, max_iterations=cap, out=io.StringIO(),
                       observers=[R()])
    return evs


@pytest.fixture(scope="module")
def p1s1():
    return record(full_runs.PUZZLES[0], 1)


@pytest.fixture(scope="module")
def p6s1():
    # Puzzle 6 seed 1 gives up after 3289 iterations; its first 4000 events
    # have blocks made and killed, so instances come and go.
    return record(full_runs.PUZZLES[5], 1)[:4000]


@pytest.fixture
def pnet(qtbot):
    v = pv.PnetView()
    qtbot.addWidget(v)
    v.resize(900, 360)
    v.show()
    return v


@pytest.fixture
def rack(qtbot):
    v = cv.CoderackView()
    qtbot.addWidget(v)
    v.resize(420, 300)
    v.show()
    return v


# -- the Pnet view --------------------------------------------------------------------

def expected_pnet_highlight(model, name):
    return name in model.changed and len(model.changed) < len(model.structure.names)


def check_pnet(view, shadow, geometry=True):
    """The view's styles equal SHADOW's; with GEOMETRY, also the items'
    places and the edges, which never change (checked at a run's first and
    last events, to keep the every-event check fast)."""
    s, lay = pnet_structure(), pnet_layout(pv.CELL)
    assert view.grid is lay
    assert view.model == shadow
    assert set(view.node_items) == set(s.names)
    for name, item in view.node_items.items():
        assert item.style == pv.pnode_style(s.node(name), shadow.activation(name),
                                            shadow.instances(name),
                                            expected_pnet_highlight(shadow, name))
    marked = {n for n, i in view.node_items.items() if i.style.instances}
    assert marked == set(shadow.with_cyto_instances())
    # The pnodes, and one layer holding every edge (painted once, cached).
    assert len(view.scene().items()) == len(view.node_items) + 1
    assert view.edge_layer.scene() is view.scene()
    if not geometry:
        return
    w, h = lay.node_size
    for name, item in view.node_items.items():
        x, y = lay.positions[name]
        assert item.pos() == QPointF(x - w / 2, y - h / 2)
        assert item.rect == QRectF(0, 0, w, h)
    lines = view.edge_layer.lines
    assert set(lines) == set(s.edges())
    for (a, b, _), line in lines.items():
        assert {(line.x1(), line.y1()), (line.x2(), line.y2())} == \
            {lay.positions[a], lay.positions[b]}
    assert view.edge_layer.zValue() < min(i.zValue() for i in view.node_items.values())


@pytest.mark.parametrize("run", ["p1s1", "p6s1"])
def test_the_pnet_view_matches_the_model_after_every_event(pnet, request, run):
    events = request.getfixturevalue(run)
    shadow = PnetModel()
    painted = pnet.paints
    marked_ever, highlighted = set(), 0
    for i, e in enumerate(events):
        shadow.on_event(e)
        pnet.on_event(e)
        pnet.redraw()
        check_pnet(pnet, shadow, geometry=i in (0, len(events) - 1))
        assert pnet.paints > painted
        painted = pnet.paints
        marked_ever |= {n for n, i in pnet.node_items.items() if i.style.instances}
        highlighted += any(i.style.highlight for i in pnet.node_items.values())
    # The bricks' numbers (11 has no pnode) and the target's nearest number
    # (114: ONE-HUNDRED) get instances, and single-pnode changes are shown.
    if run == "p1s1":
        assert marked_ever >= {"TWENTY", "SEVEN", "ONE", "SIX", "ONE-HUNDRED"}
    else:
        assert len(marked_ever) > 5
    assert highlighted > 10
    # config's pseudo-instances (operations, link types) are not marked.
    assert shadow.instances("ADD") and not pnet.node_items["ADD"].style.instances
    assert shadow.instances("OPERAND") and not pnet.node_items["OPERAND"].style.instances


def test_activation_shades_on_a_square_root_scale():
    level = pv.activation_level
    assert level(None) == 0 and level(0) == 0 and level(-3) == 0
    assert level(pv.MAX_ACTIVATION) == 1 and level(10 * pv.MAX_ACTIVATION) == 1
    assert level(pv.MAX_ACTIVATION / 4) == 0.5
    # Most activations are small: 5 (of 200) still shows.
    assert abs(level(5) - math.sqrt(5 / pv.MAX_ACTIVATION)) <= 0.5 / pv.SHADES
    assert level(5) > 0.15
    values = [level(a) for a in (0, 1, 5, 20, 80, 150, 200)]
    assert values == sorted(values) and len(set(values)) == len(values)
    # Shades are in SHADES steps, so a change too small to see restyles nothing.
    assert all(v * pv.SHADES == round(v * pv.SHADES) for v in values)
    assert level(100.0) == level(100.4)


def test_pnode_styles():
    s = pnet_structure()
    two = s.node("TWO")
    cold = pv.pnode_style(two, 0, None, False)
    hot = pv.pnode_style(two, 200, None, False)
    assert cold.fill != hot.fill and cold.level == 0 and hot.level == 1
    # The text stays readable on the darkest fill.
    assert hot.text != cold.text
    brick = pv.pnode_style(two, 0, (("2b", "CYTO-BRICK1"), ("4bl", "CYTO-BLOCK2-V1")), False)
    assert brick.instances == 2 and brick.border_width > cold.border_width
    pseudo = pv.pnode_style(s.node("ADD"), 0, (("6g", "node-add"),), False)
    assert pseudo.instances == 0
    assert pv.pnode_style(two, 0, None, True).highlight
    # Each kind has its own stripe color.
    kinds = {n.kind for n in s.nodes}
    assert len({pv.pnode_style(next(n for n in s.nodes if n.kind == k), 0, None,
                               False).stripe for k in kinds}) == len(kinds)


def test_a_spread_shades_but_highlights_nothing(pnet, p1s1):
    shadow = PnetModel()
    for e in p1s1:
        shadow.on_event(e)
        pnet.on_event(e)
        pnet.redraw()
        if e.kind in ("pnet", "pnet-initialized"):
            assert not any(i.style.highlight for i in pnet.node_items.values())
        if e.kind == "pnodes-changed":
            assert {n for n, i in pnet.node_items.items() if i.style.highlight} == \
                {n for n, _ in e.values}


def test_the_pnet_view_redraw_paints_now_and_fits(pnet, p1s1):
    for e in p1s1[:30]:
        before = pnet.paints
        pnet.on_event(e)
        assert pnet.paints == before
        pnet.redraw()
        assert pnet.paints == before + 1
    lay = pnet_layout(pv.CELL)
    shown = pnet.mapFromScene(QRectF(0, 0, lay.width, lay.height)).boundingRect()
    assert pnet.viewport().rect().adjusted(-1, -1, 1, 1).contains(shown)
    pnet.resize(1400, 500)
    shown = pnet.mapFromScene(QRectF(0, 0, lay.width, lay.height)).boundingRect()
    assert pnet.viewport().rect().adjusted(-1, -1, 1, 1).contains(shown)


def test_tooltips_say_the_models_current_state(pnet, p1s1):
    for e in p1s1:
        pnet.on_event(e)
    pnet.redraw()
    tip = pnet.tooltip("TWENTY")
    assert tip.startswith("TWENTY (number)\nactivation ")
    assert "CYTO-BRICK2 (2b)" in tip
    assert "node-add (6g, config)" in pnet.tooltip("ADD")


def test_a_new_run_resets_the_pnet_view(pnet, p1s1, p6s1):
    for e in p1s1:
        pnet.on_event(e)
    pnet.redraw()
    shadow = PnetModel()
    for e in p6s1[:50]:
        shadow.on_event(e)
        pnet.on_event(e)
    pnet.redraw()
    check_pnet(pnet, shadow)


# -- the coderack view ------------------------------------------------------------------

def check_rack(view, shadow, fresh):
    assert view.model == shadow
    rows = view.rows
    assert tuple(r.urgency for r in rows) == shadow.levels()
    assert tuple((r.urgency, r.count) for r in rows) == shadow.counts()
    for r in rows:
        waiting = shadow.bin(r.urgency)
        assert sum(n for _, n in r.groups) == r.count == len(waiting)
        assert [g for g, _ in r.groups] == list(dict.fromkeys(c.codelet for c in waiting))
        assert r.changed == (r.urgency in shadow.changed)
        c = shadow.chosen
        assert r.chosen == bool(fresh and c is not None and c.codelet is not None
                                and c.codelet.urgency == r.urgency)
    assert view.chosen_fresh == fresh
    assert view.chosen_text == cv.chosen_text(shadow.chosen)
    assert view.total == shadow.total


@pytest.mark.parametrize("run", ["p1s1", "p6s1"])
def test_the_coderack_view_matches_the_model_after_every_event(rack, request, run):
    events = request.getfixturevalue(run)
    shadow = CoderackModel()
    painted = rack.paints
    chosen_rows = 0
    for e in events:
        shadow.on_event(e)
        rack.on_event(e)
        rack.redraw()
        fresh = e.kind in ("codelet-chosen", "setup-choose")
        check_rack(rack, shadow, fresh)
        assert rack.paints > painted
        painted = rack.paints
        chosen_rows += any(r.chosen for r in rack.rows)
    assert chosen_rows > 40


def test_chosen_text():
    assert cv.chosen_text(None) == "nothing chosen yet"
    c = Codelet("CONST-BLX", ({"obj": "cyto-node", "name": "CYTO-BRICK4"}, 84), 150)
    assert cv.chosen_text(Choice(n=12, codelet=c, setup=False)) == \
        "iteration 12: const-blx CYTO-BRICK4 84  (urgency 150)"
    assert cv.chosen_text(Choice(n=None, codelet=c, setup=True)).startswith("set-up: const-blx")
    assert cv.chosen_text(Choice(n=3, codelet=None, setup=False)) == \
        "iteration 3: nothing (the rack was empty)"


def test_the_chosen_codelet_is_drawn_on_two_lines():
    # A long form would be elided on one line, so the view draws when and
    # how urgent on the first line and the form, whole, on the second.
    c = Codelet("DECOMP+", ({"obj": "cyto-node", "name": "CYTO-BLOCK120-V1"},
                            {"obj": "cyto-node", "name": "CYTO-TARGET"}), 300)
    assert cv.chosen_lines(Choice(n=30, codelet=c, setup=False)) == \
        ("iteration 30, urgency 300", "decomp+ CYTO-BLOCK120-V1 CYTO-TARGET")
    assert cv.chosen_lines(None) == ("nothing chosen yet", "")
    assert cv.chosen_lines(Choice(n=3, codelet=None, setup=False)) == \
        ("iteration 3", "nothing (the rack was empty)")


def test_the_coderack_view_redraw_paints_now(rack, p1s1):
    for e in p1s1[:30]:
        before = rack.paints
        rack.on_event(e)
        assert rack.paints == before
        rack.redraw()
        assert rack.paints == before + 1


def test_bars_never_overflow_their_scale(rack, p6s1):
    for e in p6s1:
        rack.on_event(e)
        rack.redraw()
        assert all(r.count <= rack.scale for r in rack.rows)
    assert rack.scale >= cv.MIN_SCALE


def test_a_new_run_resets_the_coderack_view(rack, p1s1, p6s1):
    for e in p1s1:
        rack.on_event(e)
    rack.redraw()
    shadow = CoderackModel()
    largest = 0
    for e in p6s1[:300]:
        shadow.on_event(e)
        rack.on_event(e)
        largest = max([largest] + [n for _, n in shadow.counts()])
    rack.redraw()
    check_rack(rack, shadow, p6s1[299].kind in ("codelet-chosen", "setup-choose"))
    # The scale is the run's own: the largest bin so far, not the old run's.
    assert rack.scale == max(cv.MIN_SCALE, largest)


def test_render_png_writes_each_view(pnet, rack, p1s1, tmp_path):
    for e in p1s1:
        pnet.on_event(e)
        rack.on_event(e)
    pnet.redraw()
    rack.redraw()
    for view, name in ((pnet, "pnet.png"), (rack, "rack.png")):
        path = tmp_path / name
        view.render_png(str(path))
        image = QImage(str(path))
        assert not image.isNull() and image.width() > 100 and image.height() > 100


def test_render_png_is_never_stale(qtbot, p1s1, tmp_path):
    """A second render_png shows the scene as it is now, not the item cache
    of the first (DeviceCoordinateCache keeps a pixmap per paint device)."""
    from numbo.gui import tree_view as tv
    for make in (pv.PnetView, tv.TreeView):
        views = []
        for _ in range(2):
            v = make()
            qtbot.addWidget(v)
            v.resize(900, 400)
            v.show()
            views.append(v)
        old, fresh = views
        for e in p1s1[:110]:
            old.on_event(e)
            old.redraw()
        old.render_png(str(tmp_path / "first.png"))
        for e in p1s1[110:180]:
            old.on_event(e)
            old.redraw()
        for e in p1s1[:180]:          # (the tree layout depends on the previous one)
            fresh.on_event(e)
            fresh.redraw()
        a = old.render_png(str(tmp_path / "again.png"))
        b = fresh.render_png(str(tmp_path / "fresh.png"))
        assert a.size() == b.size()
        assert a == b, make.__name__


# -- the main window -------------------------------------------------------------------

def test_the_main_window_draws_the_run_in_both_docks(qtbot):
    w = MainWindow()
    qtbot.addWidget(w)
    w.show()
    assert w.pnet_dock.widget() is w.pnet_view
    assert w.coderack_dock.widget() is w.coderack_view
    pnet_shadow, rack_shadow = PnetModel(), CoderackModel()

    class Check:
        """Subscribed before the views: checks they drew each event before
        the next was published."""
        def __init__(self):
            self.count = 0
            self.fresh = False

        def on_event(self, event):
            if self.count:
                check_pnet(w.pnet_view, pnet_shadow)
                check_rack(w.coderack_view, rack_shadow, self.fresh)
            pnet_shadow.on_event(event)
            rack_shadow.on_event(event)
            self.fresh = event.kind in ("codelet-chosen", "setup-choose")
            self.count += 1

    check = Check()
    for v in (w.pnet_view, w.coderack_view):
        w._views.unsubscribe(v)
    w.add_view(check)
    for v in (w.pnet_view, w.coderack_view):
        w._views.subscribe(v)
    w.controls.seed_edit.setText("1")
    w.controls.speed_slider.setValue(len(controls_module.DELAYS) - 1)
    w.controls.play_button.click()
    qtbot.waitUntil(lambda: w.outcome is not None, timeout=TIMEOUT)
    check_pnet(w.pnet_view, pnet_shadow)
    check_rack(w.coderack_view, rack_shadow, check.fresh)
    assert check.count == 286
    # The View menu toggles the new docks too.
    titles = [a.text() for a in w.menuBar().actions()[0].menu().actions()]
    assert "Pnet" in titles and "Coderack" in titles
    w.close()


def test_events_the_views_ignore_change_nothing(pnet, rack):
    pnet.on_event(observe.RackEmptied())
    rack.on_event(observe.RackEmptied())
    pnet.redraw()
    rack.redraw()
    assert all(r.count == 0 for r in rack.rows)
