"""Loop0006 item 15: the palette and editor pane (`tabletop/gui/palette_view.py`; G-050,
G-051), offscreen with pytest-qt, one shared window with the palette dock floated
(docked again at the end). PANES.md P7's tests and interactions.

- The strip: the eleven cells in Figure 2.1's order [F95 p. 26 (img)], the counts beside
  them equal to the table's [F95 p. 96 (img)], the legend saying the icons and the editor
  are the GUI's; a click on a cell adds one of the type.
- A drop from the palette adds an object at the drop point; dragging an item moves it (a
  real mouse drag on the canvas), clamped inside the table; the keys H, E, R and Delete
  and the context menu's entries touch, mark, turn and remove the selected object; the
  arrows follow.
- An invalid table shows `validate`'s messages in the pane and Do this! is off and
  refused; a built table runs through the window on Do this! (the records and result
  equal a plain run of the editor's `Problem`), Run it again takes the next seed, and the
  title, the controls and the stats say the edited table.
- Editing is refused while a run is live: the strip, the canvas, the buttons and the
  fields are off, a drop and a key change nothing, and the edit line says so.
- Save config (the patched dialog) writes a file that Load config reads back, `problems/`
  is untouched, a path inside it is refused and said; a bad file is said.
- Edit > Edit table raises the dock; the minimum sizes; the pane as a PNG.
"""

import hashlib
from pathlib import Path

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QMimeData, QPoint, QPointF, Qt                    # noqa: E402
from PySide6.QtGui import QDropEvent                                          # noqa: E402
from PySide6.QtTest import QTest                                              # noqa: E402
from PySide6.QtWidgets import QFileDialog                                     # noqa: E402

from tabletop import objects as O                                             # noqa: E402
from tabletop import problems as P                                            # noqa: E402
from tabletop.config import PROFILES                                          # noqa: E402
from tabletop.gui import palette_view as pv                                   # noqa: E402
from tabletop.gui.main_window import title_for                                # noqa: E402
from tabletop.gui.models import palette as pm                                 # noqa: E402
from tabletop.gui.table_view import SCALE                                     # noqa: E402
from tabletop.run import run_problem                                          # noqa: E402

TIMEOUT = 30000   # ms


def library_hashes() -> dict:
    return {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(P.PROBLEMS_DIR.iterdir())}


@pytest.fixture(scope="module")
def shown(module_window, qapp):
    """The shared window with the palette dock floated and large enough to probe; docked
    again at the end so that it does not take the next module's mouse moves."""
    w = module_window
    w.resize(1600, 1000)
    w.palette_dock.setFloating(True)
    w.palette_dock.resize(520, 560)
    w.palette_dock.move(40, 40)
    qapp.processEvents()
    QTest.qWait(40)
    assert w.palette_dock.isFloating() and w.palette_dock.isVisible()
    yield w
    w.palette_dock.setFloating(False)


@pytest.fixture
def window(shown):
    """The shared window with no run live, the editor cleared and the defaults back."""
    w = shown
    w.stop()
    if w.replay_source is not None:
        w.close_replay()
    w.set_next_run(problem="surround-05", profile="standard", seed=3)
    w.palette_view.editor.clear()
    w.statusBar().clearMessage()
    return w


def build(editor):
    """The model test's small table, built through the editor."""
    editor.add("cup", 30, 15)
    editor.add("fork", 20, 15)
    editor.add("cup", 80, 82)
    editor.add("spoon", 90, 82)
    editor.touch("cup1")
    editor.toggle_answer("cup2")


def drop(canvas, type_, x, y):
    """A palette drag of `type_` dropped at table units (x, y)."""
    mime = QMimeData()
    mime.setData(pv.MIME_TYPE, type_.encode())
    point = canvas.mapFromScene(QPointF(x * SCALE, y * SCALE))
    event = QDropEvent(QPointF(point), Qt.DropAction.CopyAction, mime,
                       Qt.MouseButton.LeftButton, Qt.KeyboardModifier.NoModifier)
    canvas.dropEvent(event)
    return event.isAccepted()


def wait_finished(qtbot, window):
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    return window.outcome


def test_the_strip_is_frenchs_palette_with_the_counts(window, qtbot):
    pane = window.palette_view
    strip = pane.strip
    assert [strip.type_at(strip.cell_rect(i).center()) for i in range(len(O.TYPES))] == list(O.TYPES)
    assert strip.type_at(QPoint(strip.width() - 3, strip.cell_rect(3).center().y())) == "cup"
    assert strip.type_at(QPoint(2, strip.height() - 1)) is None
    pane.load_problem(P.load_problem("surround-05"))
    qtbot.wait(20)
    assert strip.counts == pane.editor.counts() and strip.counts["fork"] == 4
    assert pane.status.text == "13 objects; can run; a table French would set"
    assert pane.name_edit.text() == "surround-05" and pane.family_combo.currentText() == "surround"
    assert "new drawings, not French's" in pane.legend.text()
    assert "GUI design, not French's" in pane.legend.text()
    assert "F95 p. 26" in strip.toolTip() and "F95 p. 96" in strip.toolTip()
    assert "GUI design, not French's" in pane.pages and "F95 p. 96" in pane.pages
    # The count boxes are painted: white inside, with dark ink for the digit.
    image = strip.grab().toImage()
    box_x = strip.cell_rect(0).right() + pv.COUNT_W // 2
    row = strip.cell_rect(0).center().y()
    pixels = [image.pixelColor(box_x + dx, row + dy) for dx in range(-6, 7) for dy in range(-6, 7)]
    assert any(c.lightness() < 90 for c in pixels) and any(c.lightness() > 240 for c in pixels)
    # A click on a cell adds one of its type at the centre of Henry's row.
    before = len(pane.editor.objects)
    qtbot.mouseClick(strip, Qt.MouseButton.LeftButton, pos=strip.cell_rect(5).center())
    assert len(pane.editor.objects) == before + 1 and pane.editor.objects[-1].type == "plate"
    assert pane.editor.objects[-1].y == 15.0 and strip.counts["plate"] == 1


def test_a_drop_adds_an_object_and_a_drag_moves_it(window, qtbot):
    pane = window.palette_view
    canvas = pane.canvas
    assert drop(canvas, "cup", 30, 15)
    assert [o.id for o in pane.editor.objects] == ["cup1"]
    cup = pane.editor.objects[0]
    assert abs(cup.x - 30) < 0.5 and abs(cup.y - 15) < 0.5     # the view's pixels, to a tenth
    assert canvas.selected_id() == "cup1" and "cup1" in canvas.items_by_id
    assert pane.strip.counts["cup"] == 1
    assert drop(canvas, "fork", 200, -50)              # clamped inside the table
    fork = pane.editor.object("fork1")
    assert fork.x < 110 and fork.y > 0 and fork.orientation == "up"
    # A real drag on the canvas moves the cup.
    item = canvas.items_by_id["cup1"]
    start = canvas.mapFromScene(item.pos())
    end = canvas.mapFromScene(QPointF(60 * SCALE, 15 * SCALE))
    qtbot.mousePress(canvas.viewport(), Qt.MouseButton.LeftButton, pos=start)
    for i in range(1, 6):
        qtbot.mouseMove(canvas.viewport(), pos=start + (end - start) * i / 5)
        qtbot.wait(5)
    qtbot.mouseRelease(canvas.viewport(), Qt.MouseButton.LeftButton, pos=end)
    cup = pane.editor.object("cup1")
    assert abs(cup.x - 60) < 3.0 and abs(cup.y - 15) < 3.0   # the drag threshold and rounding
    assert item.pos() == QPointF(cup.x * SCALE, cup.y * SCALE)     # snapped to the model
    assert "cup1: cup" in item.toolTip()
    # The model's move clamps and the item follows.
    canvas.moved("cup1", -40, 15)
    assert pane.editor.object("cup1").x == pytest.approx(O.footprint("cup")[0] / 2 + pm.EDGE, abs=0.06)
    assert item.pos().x() == pytest.approx(pane.editor.object("cup1").x * SCALE)


def test_the_keys_and_the_context_menu_edit_the_selected_object(window, qtbot):
    pane = window.palette_view
    canvas = pane.canvas
    build(pane.editor)
    assert [a.letter for a in pane.editor.arrows()] == ["H", "E"]
    assert len(canvas.arrow_items) == 2
    canvas.select("fork1")
    canvas.setFocus()
    qtbot.keyClick(canvas, Qt.Key.Key_R)
    assert pane.editor.object("fork1").orientation == "down"
    qtbot.keyClick(canvas, Qt.Key.Key_H)
    assert pane.editor.touched == "fork1"
    qtbot.keyClick(canvas, Qt.Key.Key_E)
    assert pane.editor.answers == {"E1": "cup2", "E2": "fork1"}
    assert [a.letter for a in pane.editor.arrows()] == ["H", "E", "E"]
    qtbot.keyClick(canvas, Qt.Key.Key_E)
    assert pane.editor.answers == {"E1": "cup2"}
    qtbot.keyClick(canvas, Qt.Key.Key_Delete)
    assert pane.editor.object("fork1") is None and pane.editor.touched is None
    assert "fork1" not in canvas.items_by_id and canvas.selected_id() is None
    # The context menu's entries, and what they do.
    texts = [t for t, _ in canvas.context_actions("spoon1")]
    assert texts == ["Touch (H)", "Mark as expected answer (E)", "Rotate (R)", "Delete"]
    assert [t for t, _ in canvas.context_actions("cup2")] == \
        ["Touch (H)", "Unmark expected answer (E)", "Delete"]
    dict(canvas.context_actions("spoon1"))["Rotate (R)"]()
    assert pane.editor.object("spoon1").orientation == "down"
    dict(canvas.context_actions("spoon1"))["Touch (H)"]()
    assert pane.editor.touched == "spoon1"
    # A double click touches.
    item = canvas.items_by_id["cup1"]
    qtbot.mouseDClick(canvas.viewport(), Qt.MouseButton.LeftButton,
                      pos=canvas.mapFromScene(item.pos()))
    assert pane.editor.touched == "cup1"


def test_an_invalid_table_cannot_be_run_and_shows_the_messages(window):
    pane = window.palette_view
    assert not pane.buttons["run"].isEnabled() and not pane.buttons["again"].isEnabled()
    html = pane.messages_label.text()
    assert "cannot run: no objects" in html and "no answers" in html
    assert pv.ERROR in html
    assert not window.run_edited_table()
    assert "cannot run" in window.statusBar().currentMessage()
    assert window.outcome is None and not window.controller.is_alive()
    pane.editor.add("cup", 30, 15)
    pane.editor.add("cup", 31, 16)
    pane.editor.touch("cup1")
    pane.editor.toggle_answer("cup2")
    assert pane.buttons["run"].isEnabled()
    html = pane.messages_label.text()
    assert "can run" in html and "cup1 and cup2 overlap" in html and pv.WARNING in html
    assert "F95 p. 38" in html
    assert pane.edit_line.text().startswith("editing: 2 objects; can run, but not a table")


def test_editing_is_refused_while_a_run_is_live(window, qtbot):
    pane = window.palette_view
    build(pane.editor)
    before = pane.editor.to_dict()
    window.step()                              # a run paused at its first event
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    assert not pane.editor.enabled and not pane.strip.enabled
    assert not pane.canvas.isInteractive() and not pane.canvas.acceptDrops()
    assert all(not pane.buttons[k].isEnabled() for k in pane.buttons)
    assert not pane.name_edit.isEnabled() and not pane.family_combo.isEnabled()
    assert pane.edit_line.text().startswith("editing off while the run is live (paused:")
    assert not drop(pane.canvas, "cup", 50, 50)
    pane.canvas.select("fork1")
    qtbot.keyClick(pane.canvas, Qt.Key.Key_Delete)
    pane.canvas.moved("cup1", 50, 50)
    pane.clear()
    assert pane.editor.to_dict() == before
    assert not window.run_edited_table()
    assert "refused while a run is live" in window.statusBar().currentMessage()
    window.stop()
    assert not window.controller.is_alive()
    qtbot.waitUntil(lambda: window.state in ("stopped", "finished"), timeout=TIMEOUT)
    assert pane.editor.enabled and pane.buttons["run"].isEnabled()
    pane.editor.add("plate", 55, 50)
    assert len(pane.editor.objects) == 5


def test_save_and_load_config_through_the_dialogs(window, monkeypatch, tmp_path):
    pane = window.palette_view
    build(pane.editor)
    pane.name_edit.setText("mine")
    pane.name_edit.editingFinished.emit()
    assert pane.editor.name == "mine"
    before = library_hashes()
    target = tmp_path / "mine.json"
    monkeypatch.setattr(QFileDialog, "getSaveFileName",
                        lambda *a, **k: (str(target), "Problem (*.json)"))
    pane.buttons["save"].click()
    assert target.exists() and window.statusBar().currentMessage() == f"table saved: {target}"
    assert library_hashes() == before
    # Into the library's folder: refused and said.
    inside = P.PROBLEMS_DIR / "zz-mine.json"
    monkeypatch.setattr(QFileDialog, "getSaveFileName", lambda *a, **k: (str(inside), ""))
    pane.buttons["save"].click()
    assert not inside.exists() and library_hashes() == before
    assert window.statusBar().currentMessage().startswith(f"cannot save {inside}")
    # Load it back after a clear.
    pane.clear()
    assert pane.editor.objects == []
    monkeypatch.setattr(QFileDialog, "getOpenFileName", lambda *a, **k: (str(target), ""))
    pane.buttons["load"].click()
    assert [o.id for o in pane.editor.objects] == ["cup1", "fork1", "cup2", "spoon1"]
    assert pane.editor.name == "mine" and pane.name_edit.text() == "mine"
    assert window.statusBar().currentMessage() == f"table loaded: {target}"
    # A bad file and a missing one are said, not raised.
    bad = tmp_path / "bad.json"
    bad.write_text("{}", encoding="utf-8")
    assert not window.load_config(bad)
    assert window.statusBar().currentMessage().startswith(f"cannot load {bad}")
    assert not window.load_config(tmp_path / "none.json")
    assert [o.id for o in pane.editor.objects] == ["cup1", "fork1", "cup2", "spoon1"]
    # The library's own files load, through the dialog's default folder.
    assert pm.problem_dir_of(None) == str(P.PROBLEMS_DIR)
    assert window.load_config(P.PROBLEMS_DIR / "surround-05.json")
    assert pane.editor.based_on == "surround-05" and pane.strip.counts["fork"] == 4


def test_edit_table_raises_the_dock_and_the_sizes_are_small(window, tmp_path):
    window.palette_dock.hide()
    window.edit_table_action.trigger()
    assert window.palette_dock.isVisible()
    pane = window.palette_view
    assert pane.minimumSizeHint().width() <= 120 and pane.minimumSizeHint().height() <= 100
    assert pane.strip.width() == pv.CELL_W + pv.COUNT_W + 2
    build(pane.editor)
    pane.render_png(tmp_path / "palette.png")
    assert (tmp_path / "palette.png").stat().st_size > 1000
    assert pane.pane_id == "P7" and window.palette_dock.windowTitle() == "Palette and editor"


def test_a_bare_pane_before_any_run():
    pane = pv.PaletteView()
    assert pane.edit_line.text() == "editing: 0 objects; cannot run: no objects"
    assert pane.events == 0 and pane.paints == 0
    assert pane.canvas.items_by_id == {} and pane.canvas.arrow_items == []
    assert [pane.family_combo.itemText(i) for i in range(pane.family_combo.count())] == \
        list(pm.FAMILIES)
    pane.set_state("running")
    assert not pane.editor.enabled and pane.edit_line.text().startswith("editing off")
    pane.set_state("finished")
    assert pane.editor.enabled
