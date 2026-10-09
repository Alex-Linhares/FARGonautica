"""The main window (seqsee/gui/qt/mainwindow.py): the canvas, the View menu, redraw on resize,
Save as PNG/SVG and Help → About.

Mirrors lib/Tk/Seqsee.pm's Populate (the Menustrip: View with the 11 @ViewOptions, Save → "as
EPS", Help → About... / Help On..., and the canvas) as built by lib/SGUI.pm's CreateWidgets
with config/GUI_sparse.conf (``-width 780 -height 450``). The golden gui_window.json
(oracle/gui_window.pl) records the menus, what each View entry puts in @Parts, the canvas's
size and background, and what the Help entries do.
"""
import html
from pathlib import Path

import pytest

import golden
import gui_recipes
from gui_screens import WINDOW_SCREENS
from seqsee import s
from seqsee.gui import snapshot
from seqsee.gui.draw import coderack, views

pytestmark = pytest.mark.gui

QtCore = pytest.importorskip("PySide6.QtCore")
QtGui = pytest.importorskip("PySide6.QtGui")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")

from seqsee.gui.qt import mainwindow, render  # noqa: E402

PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"
PERL_DIR = PY / "docs" / "gui" / "perl"
GOLDEN = golden.load("gui_window")[0]


def _menu(name):
    return next(m for m in GOLDEN["menus"] if m["label"] == name)


@pytest.fixture
def win(qtbot):
    w = mainwindow.MainWindow()
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


def _snap(recipe):
    gui_recipes.build(recipe)
    return snapshot.take()


def _expected(win, view, snap, **kw):
    w, h = win.canvas_size()
    return views.compose(view, snap, w, h, measure=win.measure, **kw)


# ---- the shell, against the Perl widget -----------------------------------------------------
def test_golden_menus_are_view_save_help():
    assert [m["label"] for m in GOLDEN["menus"]] == ["View", "Save", "Help"]
    assert [e["label"] for e in _menu("Save")["entries"]] == ["as EPS"]


def test_view_menu_lists_the_eleven_views_in_perl_order(win):
    golden_titles = [e["label"] for e in _menu("View")["entries"]]
    assert [a.text() for a in win.view_actions] == golden_titles
    assert len(golden_titles) == 11
    # Perl's menus, plus Panes (the docks, item 019) and Run (the key bindings, item 016).
    assert win.menu_titles() == ["View", "Panes", "Run", "Save", "Help"]


def test_each_view_entry_shows_the_parts_perl_shows(win):
    for k, case in enumerate(GOLDEN["views"][1:]):
        win.view_actions[k].trigger()
        assert win.view == k
        parts = [p[0] for p in views.VIEW_OPTIONS[win.view].parts]
        assert parts == case["parts"], case["label"]
        assert win.view_actions[k].isChecked()
        assert sum(a.isChecked() for a in win.view_actions) == 1


def test_initial_view_is_populates(qtbot):
    w = mainwindow.MainWindow()
    qtbot.addWidget(w)
    assert [p[0] for p in views.VIEW_OPTIONS[w.view].parts] == GOLDEN["views"][0]["parts"]
    w2 = mainwindow.MainWindow(options={"view": "5"})
    qtbot.addWidget(w2)
    assert w2.view == 5 and w2.view_actions[5].isChecked()


def test_canvas_size_and_background_are_perls(win):
    c = GOLDEN["canvas"]
    assert (mainwindow.CANVAS_WIDTH, mainwindow.CANVAS_HEIGHT) == (c["width"], c["height"])
    assert win.canvas_size() == (c["width"], c["height"])
    assert mainwindow.CANVAS_BACKGROUND == c["background"]
    bg = win.canvas.scene().backgroundBrush().color()
    assert bg.name().upper() == c["background"]


def test_save_menu_offers_png_and_svg(win):
    assert [a.text() for a in win.save_actions] == ["as PNG...", "as SVG..."]


def test_help_menu_has_about(win):
    # Perl's About... entry has no action (it prints "[About...]"); here it opens a box.
    assert GOLDEN["help_prints"]["About..."] == "[About...]\n"
    assert win.about_action.text() == "About..."
    text = mainwindow.about_text()
    assert "Seqsee" in text and "Mahabal" in text and "PySide6" in text


def test_about_opens_a_message_box(win, monkeypatch):
    shown = []
    monkeypatch.setattr(QtWidgets.QMessageBox, "about",
                        lambda parent, title, text: shown.append((parent, title, text)))
    win.about_action.trigger()
    assert shown == [(win, "About Seqsee", mainwindow.about_text())]


# ---- drawing ------------------------------------------------------------------------------
def test_no_snapshot_draws_an_empty_canvas(win):
    assert win.composition is None
    assert render.op_items(win.canvas.scene()) == []


def test_snapshot_is_composed_with_qt_metrics(win):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    expected = _expected(win, 0, snap)
    assert win.composition.ops == expected.ops
    items = render.op_items(win.canvas.scene())
    assert [i.data(render.OP_ROLE) for i in items] == list(expected.ops)
    assert win.canvas.scene().sceneRect() == QtCore.QRectF(0, 0, 780, 450)


def test_view_change_redraws(win):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    for k in range(len(views.VIEW_OPTIONS)):
        win.set_view(k)
        assert win.composition.ops == _expected(win, k, snap).ops
        assert len(render.op_items(win.canvas.scene())) == len(win.composition.ops)
    win.set_view("Workspace + Stream")
    assert win.view == 9 and win.view_actions[9].isChecked()


def test_view_change_resets_the_list_pages(win):
    """SetupParts calls each list's Setup, which sets PageNumber to 0 (PERL-QUIRK)."""
    snap = _snap("groups_many")
    win.show_snapshot(snap)
    win.set_view(8)
    win.set_page(views.GROUPS_LIST, 1)
    assert win.composition.ops == _expected(win, 8, snap, pages={views.GROUPS_LIST: 1}).ops
    assert win.composition.lists[views.GROUPS_LIST].page_number == 1
    win.set_view(0)
    assert win.pages == {}
    assert win.composition.lists[views.GROUPS_LIST].page_number == 0


def test_attention_needed_draws_the_arrows(win):
    snap = _snap("attention_groups")
    win.show_snapshot(snap)
    win.set_view(1)
    win.set_attention_needed(True)
    assert win.composition.ops == _expected(win, 1, snap, attention_needed=True).ops
    assert win.composition.ops[-1].text == "PLEASE SEE BELOW"
    win.set_attention_needed(False)
    assert win.composition.ops == _expected(win, 1, snap).ops


def test_a_died_part_is_reported_in_the_status_bar(win):
    win.show_snapshot(_snap("groups_relations"))
    win.set_view("Workspace + Rules")
    assert win.composition.died_part == views.RULES_LIST
    assert views.RULES_LIST in win.statusBar().currentMessage()
    win.set_view(1)
    assert win.statusBar().currentMessage() == ""


def test_a_drawing_bug_does_not_kill_the_window(win, monkeypatch):
    win.show_snapshot(_snap("groups_relations"))
    before = win.composition

    def boom(*a, **k):
        raise ValueError("bad draw")

    monkeypatch.setattr(mainwindow.views, "compose", boom)
    win.redraw()
    assert win.composition is before
    assert "bad draw" in win.statusBar().currentMessage()


def test_coderack_rows_persist_like_perls_history(win):
    """DrawIt's ``$HistoryOfRunnable{$_} ||= 0`` keeps a family's row once a Coderack view
    has drawn it; the window passes those families back as ``known_families``."""
    snap = _snap("coderack_small")
    win.set_view(5)
    win.show_snapshot(snap)
    assert set(coderack.rack_families(snap)) <= set(win.known_families)
    s.reset_all()
    empty = snapshot.take()
    win.show_snapshot(empty)
    assert win.composition.ops == _expected(win, 5, empty,
                                            known_families=win.known_families).ops
    win.reset_run()
    assert win.known_families == ()


# ---- resize ---------------------------------------------------------------------------------
def test_resize_redraws_at_the_new_size(win, qtbot):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    old = win.canvas_size()
    win.resize(win.width() + 220, win.height() + 250)
    qtbot.waitUntil(lambda: win.canvas_size() != old and win.composed_size == win.canvas_size())
    w, h = win.canvas_size()
    assert (w, h) == (old[0] + 220, old[1] + 250)
    assert win.composition.ops == views.compose(0, snap, w, h, measure=win.measure).ops
    assert win.canvas.scene().sceneRect() == QtCore.QRectF(0, 0, w, h)
    # The canvas is never scrolled or scaled: scene coordinates are canvas pixels.
    assert win.canvas.mapToScene(0, 0) == QtCore.QPointF(0, 0)
    assert win.canvas.transform().isIdentity()


def test_redraws_are_coalesced_into_one_per_event_loop_turn(win, qtbot):
    win.show_snapshot(_snap("groups_relations"))
    n = win.redraw_count
    for dw in range(1, 6):
        win.resize(win.width() + 10, win.height())
    qtbot.waitUntil(lambda: win.redraw_count > n)
    qtbot.wait(20)
    assert win.redraw_count - n == 1


# ---- saving ---------------------------------------------------------------------------------
def test_save_png(win, tmp_path):
    win.show_snapshot(_snap("groups_relations"))
    path = tmp_path / "view.png"
    win.save_image(path)
    img = QtGui.QImage(str(path))
    assert (img.width(), img.height()) == win.canvas_size()
    assert img.pixelColor(2, 2).name().upper() == mainwindow.CANVAS_BACKGROUND
    assert any(img.pixelColor(x, y).name().upper() != mainwindow.CANVAS_BACKGROUND
               for x in range(0, 500, 5) for y in range(0, 225, 5))


def test_save_svg(win, tmp_path):
    win.show_snapshot(_snap("groups_relations"))
    path = tmp_path / "view.svg"
    win.save_image(path)
    text = path.read_text()
    assert text.lstrip().startswith("<?xml") and "<svg" in text
    assert 'viewBox="0 0 780 450"' in text
    texts = [op.text for op in win.composition.ops if op.TYPE == "text" and op.text]
    assert texts
    for t in texts:
        for line in t.split("\n"):
            assert html.escape(line, quote=False) in text, line


def test_save_rejects_unknown_formats(win, tmp_path):
    with pytest.raises(ValueError):
        win.save_image(tmp_path / "view.eps")


def test_save_dialog_adds_the_chosen_suffix(win, tmp_path, monkeypatch):
    win.show_snapshot(_snap("groups_relations"))
    target = tmp_path / "picture"
    monkeypatch.setattr(QtWidgets.QFileDialog, "getSaveFileName",
                        lambda *a, **k: (str(target), mainwindow.SVG_FILTER))
    win.save_actions[1].trigger()
    assert (tmp_path / "picture.svg").exists()
    monkeypatch.setattr(QtWidgets.QFileDialog, "getSaveFileName",
                        lambda *a, **k: ("", ""))
    win.save_actions[0].trigger()       # cancelled: nothing happens
    assert sorted(p.name for p in tmp_path.iterdir()) == ["picture.svg"]


# ---- runner -------------------------------------------------------------------------------
class _FakeRunner(QtCore.QObject):
    snapshot = QtCore.Signal(object)
    error = QtCore.Signal(str, str)
    state_changed = QtCore.Signal(str)
    command_done = QtCore.Signal(str, object)
    message = QtCore.Signal(object)
    question = QtCore.Signal(object)
    question_closed = QtCore.Signal(object)
    state = "idle"

    def quit(self, timeout=None):
        return True


def test_attach_runner_shows_its_snapshots_and_errors(win):
    r = _FakeRunner()
    win.attach_runner(r)
    snap = _snap("groups_relations")
    r.snapshot.emit(snap)
    # queued (item 023): drawn at the next turn of the event loop, or when flushed
    assert win.snapshot is None
    win.flush_snapshot()
    assert win.snapshot is snap and win.composition is not None
    r.error.emit("ZeroDivisionError: boom", "Traceback ...")
    assert "boom" in win.statusBar().currentMessage()


def test_drawing_reads_only_the_snapshot(win):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    before = win.composition.ops
    s.reset_all()           # the live model changes; the window keeps showing the snapshot
    win.redraw()
    assert win.composition.ops == before


# ---- screenshot ---------------------------------------------------------------------------
def test_window_screenshot(win, qtbot, tmp_path, request):
    """The whole window showing groups_relations in view 0, next to the Perl widget's
    screenshot (oracle/gui_window.pl). ``pytest --write-screens`` saves it under
    python/docs/gui/screens/."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    for name, recipe in WINDOW_SCREENS.items():
        assert (PERL_DIR / f"{name}.png").exists()
        win.show_snapshot(_snap(recipe))
        win.set_view(0)
        qtbot.wait(10)
        path = out_dir / f"{name}.png"
        assert win.grab().save(str(path), "PNG")
        assert (SCREENS_DIR / f"{name}.png").exists()
