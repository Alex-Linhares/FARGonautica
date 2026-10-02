"""loop0003 item 8: the AST canvas (numbo/gui/tree_view.py).

pytest-qt, offscreen.  The canvas is a QGraphicsView observer holding a
TreeModel and a TreeLayout; after every event its redraw() lays out the
model's forest, brings the scene's items in line with the layout, and paints.
These tests check:
  - the scene's items equal the model and the layout after every event of
    puzzle 1 seed 1 and puzzle 3 seed 8 (names, boxes, labels, styles,
    edges, highlight, nothing else in the scene);
  - the styling rules (colors by type, status, ghosts fading, activation,
    the latest change highlighted, the current target);
  - redraw() paints now (the run waits for it);
  - zoom, pan and fit-to-view;
  - the main window shows the canvas and draws a run in it;
  - render_png writes the scene to a PNG.
"""

import io

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, QPointF, QRectF, Qt     # noqa: E402
from PySide6.QtGui import QImage, QWheelEvent               # noqa: E402
from PySide6.QtWidgets import QGraphicsView                 # noqa: E402

import full_runs                                            # noqa: E402
from numbo import harness, observe                         # noqa: E402
from numbo.gui import controls as controls_module            # noqa: E402
from numbo.gui import tree_view as tv                       # noqa: E402
from numbo.gui.main_window import MainWindow                # noqa: E402
from numbo.models.tree_model import TreeModel               # noqa: E402

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
def p3s8():
    return record(full_runs.PUZZLES[2], 8)


@pytest.fixture
def view(qtbot):
    v = tv.TreeView()
    qtbot.addWidget(v)
    v.resize(900, 600)
    v.show()
    return v


def feed(view, events):
    for e in events:
        view.on_event(e)
        view.redraw()


# -- the scene against the model, after every event -------------------------------

def expected_highlight(model, name):
    if name not in model.last_change:
        return None
    return "fresh" if model.last_change_at == model.events_seen else "recent"


def check_scene(view, shadow):
    """The scene equals what SHADOW (a TreeModel fed the same events) and
    a fresh layout of its forest say it should be."""
    forest = shadow.forest(ghost_window=tv.GHOST_WINDOW)
    nodes = {n.name: n for n in forest.walk()}
    layout = view.layout
    assert layout is view.layouter.previous
    assert view.model == shadow
    assert set(view.node_items) == set(layout.boxes) == set(nodes)
    for name, item in view.node_items.items():
        box, node = layout.boxes[name], nodes[name]
        assert item.node == node
        assert item.label == tv.view_label(node).text
        assert item.pos() == QPointF(box.x, box.y)
        assert item.rect == QRectF(0, 0, box.width, box.height)
        assert item.style == tv.node_style(node, shadow.ghost_age(name),
                                           expected_highlight(shadow, name))
        assert item.scene() is view.scene()
    assert set(view.edge_items) == set(layout.edges)
    for (parent, child), edge in view.edge_items.items():
        a, b = layout.boxes[parent], layout.boxes[child]
        path = edge.path()
        assert path.pointAtPercent(0) == QPointF(a.cx, a.bottom)
        assert path.pointAtPercent(1) == QPointF(b.cx, b.y)
    # Nothing else is in the scene.
    assert len(view.scene().items()) == len(view.node_items) + len(view.edge_items)
    # The scene rect holds the whole layout, ghosts' band included.
    # (QRectF.contains is False for an empty rect: the start's empty forest.)
    rect = view.scene().sceneRect()
    if layout.boxes:
        assert rect.contains(QRectF(0, layout.top, layout.width, layout.height))


@pytest.mark.parametrize("run", ["p1s1", "p3s8"])
def test_the_scene_matches_the_model_after_every_event(view, request, run):
    events = request.getfixturevalue(run)
    shadow = TreeModel()
    painted = view.paints
    highlighted = expected = 0
    for e in events:
        shadow.on_event(e)
        view.on_event(e)
        view.redraw()
        check_scene(view, shadow)
        # Every event is painted, now.
        assert view.paints > painted
        painted = view.paints
        highlighted += any(i.style.highlight == "fresh" for i in view.node_items.values())
        expected += bool(shadow.changed & set(view.node_items))
    # Tree changes are frequent and are shown (p1s1: 44 of 286 events).
    assert highlighted == expected > 40
    if run == "p1s1":
        target = view.node_items["CYTO-TARGET"]
        assert target.label == "114\n(6 x 20) - (7 - 1)"
    else:
        # The kill-block gap: the killed block is a dead leaf under the
        # orphaned operation, drawn as a ghost.
        dead = view.node_items["CYTO-BLOCK11-V5"]
        assert not dead.node.alive and dead.style.dashed
        assert view.node_items["CYTO-TARGET"].label == "31\n((3 x 5) - ([11] - 3)) + 24"


def test_a_new_run_starts_a_new_scene(view, p1s1, p3s8):
    feed(view, p1s1[:150])
    feed(view, p3s8[:40])
    shadow = TreeModel()
    for e in p3s8[:40]:
        shadow.on_event(e)
    check_scene(view, shadow)
    assert view.node_items["CYTO-BRICK1"].node.value == 3     # puzzle 3's bricks
    assert view.node_items["CYTO-TARGET"].node.value == 31


# -- styling ------------------------------------------------------------------------

def node(type="2b", value=7, status="free", activation=50, alive=True, current=False,
         name="N", children=(), expression="7", symbol=None, op=None):
    from numbo.models.tree_model import TreeNode
    return TreeNode(name=name, type=type, value=value, status=status, level=1,
                    activation=activation, success=1, alive=alive, op=op, symbol=symbol,
                    expression=expression, current=current, children=children)


def test_each_type_has_its_own_color():
    fills = {t: tv.node_style(node(type=t), None, None).fill for t in tv.TYPE_COLORS}
    assert set(fills) == {"1t", "3dt", "4bl", "2b", "5g"}
    assert len(set(fills.values())) == 5


def test_status_shows_in_the_border():
    free = tv.node_style(node(status="free"), None, None)
    linked = tv.node_style(node(status="linked"), None, None)
    assert linked.border_width > free.border_width
    assert not free.dashed and not linked.dashed and free.opacity == linked.opacity == 1


def test_ghosts_fade_with_age():
    ages = [tv.node_style(node(alive=False, status="killed"), age, None)
            for age in (0, 10, 25, tv.GHOST_WINDOW)]
    assert all(s.dashed for s in ages)
    opacities = [s.opacity for s in ages]
    assert opacities == sorted(opacities, reverse=True)
    assert opacities[0] < 1 and opacities[-1] > 0
    # A dead leaf of a live tree (the kill-block gap) has no age: still faded.
    assert tv.node_style(node(alive=False, status="killed"), None, None).opacity < 1


def test_activation_is_a_bar():
    assert tv.node_style(node(activation=0), None, None).bar == 0
    assert tv.node_style(node(activation=150), None, None).bar == 0.5
    assert tv.node_style(node(activation=300), None, None).bar == 1
    assert tv.node_style(node(activation=900), None, None).bar == 1
    assert tv.node_style(node(activation=None), None, None).bar == 0
    assert tv.node_style(node(type="5g", activation=None, symbol="+"), None, None).bar is None


def test_highlight_and_current_target():
    assert tv.node_style(node(), None, "fresh").highlight == "fresh"
    assert tv.node_style(node(), None, None).highlight is None
    plain = tv.node_style(node(type="1t"), None, None)
    current = tv.node_style(node(type="1t", current=True), None, None)
    assert current.border != plain.border and current.border_width > plain.border_width


def test_labels_have_the_value_and_the_expression():
    assert tv.view_label(node()).text == "7"
    block = node(type="4bl", value=120, expression="6 x 20",
                 children=(node(type="5g", symbol="x", op="TIMES", value=None),))
    assert tv.view_label(block).text == "120\n6 x 20"
    # Drawn with typographic symbols; the expressions keep check_solution's.
    assert tv.view_label(node(type="5g", symbol="-", op="PLUS", value=None)).text == "−"
    assert tv.view_label(node(type="5g", symbol="x", op="TIMES", value=None)).text == "×"
    assert tv.view_label(node(type="5g", symbol="+", op="PLUS", value=None)).text == "+"
    assert tv.view_label(node(type="5g", op="PLUS", value=None)).op
    # A ghost operation has no symbol: its operation.
    assert tv.view_label(node(type="5g", name="PLUS3-11-V5", value=None,
                              alive=False)).text == "PLUS"


# -- painting -------------------------------------------------------------------------

def test_redraw_paints_now(view, p1s1):
    for e in p1s1[:30]:
        before = view.paints
        view.on_event(e)
        assert view.paints == before          # on_event only updates the model
        view.redraw()
        assert view.paints == before + 1


def test_render_png_writes_the_scene(view, p1s1, tmp_path):
    feed(view, p1s1)
    path = tmp_path / "solved.png"
    view.render_png(str(path))
    image = QImage(str(path))
    assert not image.isNull()
    rect = view.scene().sceneRect()
    assert image.width() >= rect.width() and image.height() >= rect.height()


# -- zoom, pan and fit ------------------------------------------------------------------

def visible(view):
    """The layout's extent, in viewport coordinates, lies in the viewport."""
    lay = view.layout
    shown = view.mapFromScene(QRectF(0, lay.top, lay.width, lay.height)).boundingRect()
    return view.viewport().rect().adjusted(-1, -1, 1, 1).contains(shown)


def test_auto_fit_keeps_the_forest_in_view(view, p3s8):
    assert view.auto_fit
    for e in p3s8:
        view.on_event(e)
        view.redraw()
    assert visible(view)
    # A small forest is not blown up past MAX_FIT_SCALE.
    view.on_event(p3s8[0])
    view.redraw()
    assert view.scale_factor() <= tv.MAX_FIT_SCALE


def test_zoom_and_fit(view, p3s8, qtbot):
    feed(view, p3s8)
    s = view.scale_factor()
    view.zoom_in()
    assert view.scale_factor() == pytest.approx(s * tv.ZOOM_STEP)
    assert not view.auto_fit                   # the user's zoom stays
    view.on_event(p3s8[-1])
    view.redraw()
    assert view.scale_factor() == pytest.approx(s * tv.ZOOM_STEP)
    view.zoom_out()
    view.zoom_out()
    assert view.scale_factor() == pytest.approx(s / tv.ZOOM_STEP)
    view.reset_zoom()
    assert view.scale_factor() == pytest.approx(1)
    view.fit_to_view()
    assert view.auto_fit and visible(view)


def test_the_wheel_zooms_and_dragging_pans(view, p3s8):
    feed(view, p3s8)
    assert view.dragMode() == QGraphicsView.DragMode.ScrollHandDrag
    s = view.scale_factor()
    center = view.viewport().rect().center()
    wheel = QWheelEvent(QPointF(center), QPointF(view.mapToGlobal(center)), QPoint(0, 0),
                        QPoint(0, 120), Qt.MouseButton.NoButton, Qt.KeyboardModifier.NoModifier,
                        Qt.ScrollPhase.NoScrollPhase, False)
    view.wheelEvent(wheel)
    assert view.scale_factor() > s and not view.auto_fit


def test_keys_zoom_and_fit(view, p3s8, qtbot):
    feed(view, p3s8)
    s = view.scale_factor()
    qtbot.keyClick(view, Qt.Key.Key_Plus)
    assert view.scale_factor() > s
    qtbot.keyClick(view, Qt.Key.Key_Minus)
    assert view.scale_factor() == pytest.approx(s)
    qtbot.keyClick(view, Qt.Key.Key_F)
    assert view.auto_fit


# -- the main window ----------------------------------------------------------------------

def test_the_main_window_draws_the_run_on_the_canvas(qtbot):
    w = MainWindow()
    qtbot.addWidget(w)
    w.show()
    assert isinstance(w.centralWidget(), tv.TreeCanvas)
    assert w.tree_view is w.centralWidget().view
    shadow = TreeModel()

    class Check:
        """Runs before the canvas (subscribed first), checks it after the
        previous event: the canvas has drawn each event before the next."""
        def __init__(self):
            self.count = 0

        def on_event(self, event):
            if self.count:
                check_scene(w.tree_view, shadow)
            shadow.on_event(event)
            self.count += 1

    check = Check()
    w._views.unsubscribe(w.tree_view)
    w.add_view(check)
    w._views.subscribe(w.tree_view)
    w.controls.seed_edit.setText("1")
    w.controls.speed_slider.setValue(len(controls_module.DELAYS) - 1)
    w.controls.play_button.click()
    qtbot.waitUntil(lambda: w.outcome is not None, timeout=TIMEOUT)
    check_scene(w.tree_view, shadow)
    assert check.count == 286
    assert w.tree_view.node_items["CYTO-TARGET"].label == "114\n(6 x 20) - (7 - 1)"
    canvas = w.centralWidget()
    canvas.fit_button.click()
    assert w.tree_view.auto_fit
    w.close()
