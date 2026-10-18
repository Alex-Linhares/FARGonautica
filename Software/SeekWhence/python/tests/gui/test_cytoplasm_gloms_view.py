"""Loop0004 item 06: the cytoplasm pane, part 2, in the window, offscreen, pytest-qt
(`seekwhence/gui/cytoplasm_view.py`; G-024, G-025, G-026; Numbo GUI, `test_tree_view.py`
and `test_polish.py`).

- Run 2's hulls nest, `glom7` over `glom6` and `glint5` [M86 p. 62], on the top-down
  layer (the View menu's action, the canvas's button), on by default and grey
  (ANSWERS.md 10.4; G-026, G-053).
- The hulls after the blast (blackburn-8, seed 1): ghosts fading, the bonds kept
  [M86 p. 92].
- A click on a hull selects the glom and its glints in the window's shared selection; a
  click on empty canvas clears it; the sequence pane's cell click selects the glint (G-025).
- The pixel probe (Numbo's `test_polish.py`): at 10 sampled events of a live run the
  painted viewport equals a fresh render, and at 10 sampled seeks the scene equals one a
  fresh view builds from the same events.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPointF, QRectF, Qt                              # noqa: E402
from PySide6.QtGui import QImage, QPainter                                  # noqa: E402
from PySide6.QtTest import QTest                                            # noqa: E402

from seekwhence import sequences                                            # noqa: E402
from seekwhence.config import Config                                        # noqa: E402
from seekwhence.gui.cytoplasm_view import CytoplasmView                     # noqa: E402
from seekwhence.gui.models import cytoplasm as cy                           # noqa: E402
from seekwhence.gui.models import presets                                   # noqa: E402

from golden import APPENDIX                                                 # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def blackburn_inputs(name, seed, n_terms=12):
    seq = sequences.find(name)
    return presets.RunInputs("custom", Config(), seed, tuple(str(t) for t in seq.terms[:n_terms]),
                             (), title=f"{name} seed {seed}")


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def indexes(window, kind, name=None):
    return [i for i, e in enumerate(window.history.events)
            if e.kind == kind and (name is None or getattr(e, "name", None) == name)]


def click(view, scene_point):
    QTest.mouseClick(view.viewport(), Qt.MouseButton.LeftButton, Qt.KeyboardModifier.NoModifier,
                     view.mapFromScene(scene_point))


# -- the hulls (G-024, G-026) -----------------------------------------------------------------------

def test_run_2_nests_glom7_over_glom6_and_glint5(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN2))
    canvas = window.cytoplasm_view
    view = canvas.view
    assert canvas.layer_actions["top-down"].isChecked()   # on by default (ANSWERS.md 10.4)
    assert window.layers_menu.actions() == list(canvas.layer_actions.values())
    canvas.layer_actions["top-down"].setChecked(False)   # the View menu's action
    assert list(view.hull_items) == ["glom1"]          # the one real glom once the layer is off
    canvas.layer_actions["top-down"].setChecked(True)
    assert canvas.layer_buttons["top-down"].isChecked() and view.model.layers["top-down"]
    assert set(view.hull_items) == {"glom1", "glom4", "glom5", "glom6", "glom7"}
    outer, inner = view.hull_items["glom7"], view.hull_items["glom6"]
    glint5 = view.glint_items["glint5"]
    assert outer.sceneBoundingRect().contains(inner.sceneBoundingRect().adjusted(5, 5, -5, -5))
    assert outer.sceneBoundingRect().contains(glint5.sceneBoundingRect().adjusted(5, 5, -5, -5))
    assert outer.zValue() < inner.zValue() < glint5.zValue()
    assert outer.style.dotted and inner.style.dotted and not view.hull_items["glom1"].style.dotted
    assert outer.shape_.label == "glom7  frame glom" and inner.shape_.label == "glom6  gnoth-glom"
    assert view.hull_items["glom1"].shape_.label == "glom1  Cycle (Cycle 3 2 (3 7)) pure"
    assert view.hull_items["glom1"].style.bold                                       # [M86 p. 69]
    assert outer.toolTip().startswith("glom7 pseudo --> ((3) 7)  terms 4 to 5")       # [M86 p. 167]
    assert view.sceneRect().contains(view.scene().itemsBoundingRect())
    canvas.layer_buttons["top-down"].click()           # the canvas's button, off again
    assert not canvas.layer_actions["top-down"].isChecked()
    assert list(view.hull_items) == ["glom1"]


def test_the_hulls_after_the_blast(window, qtbot):
    run_to_end(qtbot, window, blackburn_inputs("blackburn-8", 1))
    canvas = window.cytoplasm_view
    view = canvas.view
    canvas.layer_actions["top-down"].setChecked(True)
    (at,) = indexes(window, "blasted")
    assert window.seek(at - 1)
    assert set(view.hull_items) == {"glom6", "glom7"}
    assert all(item.opacity() == 1.0 for item in view.hull_items.values())
    assert window.seek(at)                                                  # [M86 p. 92]
    assert set(view.hull_items) == {"glom6", "glom7"} and view.model.gloms == {}
    assert view.hull_items["glom7"].toolTip().endswith("(blasted)")
    assert set(view.arc_items) >= {"bond1", "bond2", "bond3"}
    assert window.seek(at + cy.FADE // 2)
    assert view.hull_items["glom7"].opacity() == pytest.approx(0.5)
    assert window.seek(at + cy.FADE)
    assert view.hull_items == {} and set(view.arc_items) >= {"bond1", "bond2", "bond3"}
    assert len(view.glint_items) == 3
    assert window.seek(len(window.history) - 1) and view.hull_items


def test_the_proposal_markers(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view = window.cytoplasm_view.view
    posted = [i for i, e in enumerate(window.history.events)
              if e.kind == "posted" and e.type == "Glomtester"]
    assert window.seek(posted[0])
    (number, item), = view.marker_items.items()
    assert item.style.flash and not item.style.filled and item.shape_.label == "Same -cover"
    assert item.toolTip().startswith("Same -cover proposed --> glint1")             # [M86 p. 165]
    assert view.element_at(view.mapFromScene(QPointF(item.shape_.x, item.shape_.y - 3))) == "glint1"
    glommer = next(i for i, e in enumerate(window.history.events)
                   if e.kind == "posted" and e.type == "Glommer")
    assert window.seek(glommer)
    waiting = view.marker_items[window.history.events[glommer].number]
    assert waiting.style.filled and waiting.shape_.x2 > waiting.shape_.x1
    assert window.seek(len(window.history) - 1) and view.marker_items == {}


# -- the selection (G-025) ----------------------------------------------------------------------------

def test_clicking_a_hull_selects_the_glom_and_its_glints(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN2))
    canvas = window.cytoplasm_view
    view = canvas.view
    canvas.layer_actions["top-down"].setChecked(True)
    heard = []
    view.selected.connect(heard.append)
    hull = view.hull_items["glom7"].shape_
    click(view, QPointF(hull.left + 12, hull.top + 8))          # the label band
    assert (window.selection.kind, window.selection.name) == ("glom", "glom7")
    assert window.selection.members == {"glom7", "glom6", "glint4", "glint5"}
    assert view.hull_items["glom7"].style.selected and view.hull_items["glom6"].style.selected
    assert view.glint_items["glint4"].style.border == cy.HIGHLIGHT_BORDER
    assert view.glint_items["glint5"].style.border == cy.HIGHLIGHT_BORDER
    assert view.glint_items["glint3"].style.border == cy.GLINT_BORDER
    assert heard == ["glom7"]
    # a glint: itself alone; a second click on it clears
    glint = view.glint_items["glint1"].box
    click(view, QPointF(glint.x, -glint.height / 2))
    assert window.selection.members == {"glint1"} and not view.hull_items["glom7"].style.selected
    click(view, QPointF(glint.x, -glint.height / 2))
    assert window.selection.empty and view.glint_items["glint1"].style.border == cy.GLINT_BORDER
    assert heard == ["glom7", "glint1", ""]
    # empty canvas clears; the sequence pane's click selects the glint
    canvas.select("glom1")
    assert window.selection.includes("glint2")
    rect = view.sceneRect()
    click(view, QPointF(rect.right() - 2, rect.bottom() - 2))
    assert window.selection.empty
    window.sequence_view.strip.cell_clicked.emit(3)
    assert (window.selection.kind, window.selection.name) == ("glint", "glint3")
    assert view.glint_items["glint3"].style.border == cy.HIGHLIGHT_BORDER
    assert canvas.highlight_term(9) is None and window.selection.empty


# -- the pixel probe (Numbo GUI, test_polish.py) ------------------------------------------------------

def shown(w, widget):
    """What the window's backing store holds for the visible part of `widget` (what was
    painted, not a fresh render)."""
    r = widget.visibleRegion().boundingRect()
    top = widget.mapTo(w, r.topLeft())
    img = w.screen().grabWindow(w.winId(), top.x(), top.y(), r.width(), r.height()).toImage()
    return img.convertToFormat(QImage.Format.Format_RGB32)


def fresh(widget):
    """A fresh render of the visible part of `widget`."""
    r = widget.visibleRegion().boundingRect()
    return widget.grab(r).toImage().convertToFormat(QImage.Format.Format_RGB32)


def same_picture(a, b, tolerance=2):
    """Equal, or no channel of any pixel more than `tolerance` apart (an antialiased
    edge can round one step differently)."""
    if a == b:
        return True
    if a.size() != b.size():
        return False
    da = bytes(a.constBits())[:a.sizeInBytes()]
    db = bytes(b.constBits())[:b.sizeInBytes()]
    return all(abs(x - y) <= tolerance for x, y in zip(da, db))


def scene_image(view):
    """The whole scene rendered at scale 1, white behind it."""
    rect = view.sceneRect()
    image = QImage(int(rect.width()) + 1, int(rect.height()) + 1, QImage.Format.Format_RGB32)
    image.fill(Qt.GlobalColor.white)
    painter = QPainter(image)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    view.scene().render(painter, QRectF(image.rect()), rect)
    painter.end()
    return image


def test_the_view_equals_a_fresh_render_at_10_sampled_events(window, qtbot):
    """Run 3 played to the end: at ten sampled events the cytoplasm's painted pixels
    equal a fresh render of it (nothing stale), and at ten sampled seeks its scene
    equals the scene a fresh view builds from the same events (the incremental
    updates equal a rebuild)."""
    canvas = window.cytoplasm_view
    view = canvas.view
    canvas.layer_actions["top-down"].setChecked(True)
    canvas.layer_actions["sparks"].setChecked(True)
    checked, mismatches = [], []

    def drawn(index):
        if index % 60 == 31:                 # 31, 91, ..., 571 of run 3's 602 events
            checked.append(index)
            if not same_picture(shown(window, view.viewport()), fresh(view.viewport())):
                mismatches.append(index)

    window.event_drawn.connect(drawn)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    window.event_drawn.disconnect(drawn)
    assert len(checked) == 10 and mismatches == []

    n = len(window.history)
    samples = [n * k // 10 + 3 for k in range(10)]
    rebuilt_mismatches = []
    for index in samples:
        assert window.seek(index)
        other = CytoplasmView(window.history)
        qtbot.addWidget(other)
        other.resize(view.size())
        other.set_layer("top-down", True)
        other.set_layer("sparks", True)
        for i in range(index + 1):
            other.set_position(i)
            other.on_event(window.history.events[i])
        other.redraw(now=False)
        assert other.layout == view.layout
        assert set(other.hull_items) == set(view.hull_items)
        assert set(other.arc_items) == set(view.arc_items)
        if not same_picture(scene_image(view), scene_image(other)):
            rebuilt_mismatches.append(index)
    assert rebuilt_mismatches == []
