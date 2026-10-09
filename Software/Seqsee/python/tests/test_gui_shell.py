"""The modern shell (seqsee/gui/qt/panes.py, seqsee/gui/qt/mainwindow.py): every pane as a
dock, the seed and max-steps fields, and the window/dock layout remembered with QSettings.

The panes are the drawing modules that lib/Tk/Seqsee.pm's @ViewOptions place on the canvas
(lib/SGUI/Workspace.pm, Workspace_Attention.pm, Slipnet.pm, Coderack.pm, Stream.pm,
Relations.pm, List/Groups.pm, List/Categories.pm, List/Rules.pm, List/Stream.pm) plus
lib/Tk/SCommentary.pm. A pane dock draws one module over its whole canvas, as SetupParts would
for a view whose only part is that module (``views.pane_view``); the drawings are checked
against the Perl goldens by the per-module tests. The seed and max-steps fields are
Seqsee.pl's ``--seed`` / ``--max_steps`` options. Perl has no docks and no remembered layout.
"""
from pathlib import Path

import pytest

import gui_recipes
from seqsee import global_ as Global
from seqsee import sworkspace, util
from seqsee.gui import snapshot
from seqsee.gui.draw import views

pytestmark = pytest.mark.gui

QtCore = pytest.importorskip("PySide6.QtCore")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")

from PySide6.QtCore import QSettings, Qt  # noqa: E402

from seqsee.gui.qt import mainwindow, panes  # noqa: E402
from seqsee.gui.runner import Runner  # noqa: E402

PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"


class FakeRunner(QtCore.QObject):
    """Records the commands (positional arguments, and keyword arguments in ``kwargs``)."""
    snapshot = QtCore.Signal(object)
    question = QtCore.Signal(object)
    question_closed = QtCore.Signal(object)
    message = QtCore.Signal(object)
    error = QtCore.Signal(str, str)
    command_done = QtCore.Signal(str, object)
    state_changed = QtCore.Signal(str)

    def __init__(self):
        super().__init__()
        self.calls = []
        self.kwargs = []
        self.state = "idle"

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)

        def call(*a, **k):
            self.calls.append((name,) + a)
            self.kwargs.append(k)
        return call


@pytest.fixture
def win(qtbot):
    w = mainwindow.MainWindow()
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


@pytest.fixture
def fake(win):
    r = FakeRunner()
    win.attach_runner(r)
    return r


def _snap(recipe):
    gui_recipes.build(recipe)
    return snapshot.take()


def _settings(path):
    return QSettings(str(path), QSettings.IniFormat)


# ---- the panes ----------------------------------------------------------------------------
def test_every_module_of_the_views_is_a_pane():
    used = {p[0] for v in views.VIEW_OPTIONS for p in v.parts}
    assert {p.part for p in panes.PANES} == used
    assert len(panes.PANES) == 10
    assert len({p.object_name for p in panes.PANES}) == 10
    assert [p.title for p in panes.PANES] == [
        "Workspace", "Attention", "Slipnet", "Coderack", "Stream", "Relations",
        "Groups", "Categories", "Rules", "Stream list"]


def test_pane_view_is_one_part_over_the_whole_canvas():
    v = views.pane_view(views.SLIPNET)
    assert v.parts == ((views.SLIPNET, 0, 0, 100, 100),)
    assert views.part_rects(v, 400, 300) == [(views.SLIPNET, 0, 0, 400, 300)]
    # the Workspace pane is the "Workspace" view
    snap = _snap("groups_relations")
    assert views.compose(views.pane_view(views.WORKSPACE), snap, 500, 200).ops == \
        views.compose(1, snap, 500, 200).ops


def test_panes_menu_toggles_every_dock_and_the_commentary(win):
    assert win.menu_titles() == ["View", "Panes", "Run", "Save", "Help"]
    texts = [a.text() for a in win.pane_menu.actions() if not a.isSeparator()]
    assert texts == [p.title for p in panes.PANES] + ["Commentary"]
    for p in panes.PANES:
        dock = win.pane_docks[p.part]
        assert isinstance(dock, QtWidgets.QDockWidget)
        assert dock.objectName() == p.object_name and dock.windowTitle() == p.title
        assert dock.isHidden()                   # the fixed views come first
    win.pane_docks[views.CODERACK].toggleViewAction().trigger()
    assert win.pane_docks[views.CODERACK].isVisible()


def test_visible_panes_draw_each_snapshot(win, qtbot):
    dock = win.pane_docks[views.SLIPNET]
    dock.show()
    qtbot.waitUntil(dock.isVisible)
    hidden = win.pane_docks[views.STREAM]
    snap = _snap("slipnet_small")
    win.show_snapshot(snap)
    w, h = dock.canvas_size()
    assert w > 0 and h > 0
    expected = views.compose(views.pane_view(views.SLIPNET), snap, w, h, measure=win.measure)
    assert dock.composition.ops == expected.ops
    assert len(dock.canvas.scene().items()) >= len(expected.ops)
    assert hidden.composition is None            # hidden panes don't draw
    hidden.show()
    qtbot.waitUntil(lambda: hidden.composition is not None)   # drawn when shown


def test_every_pane_draws(win, qtbot):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    for p in panes.PANES:
        dock = win.pane_docks[p.part]
        dock.show()
        qtbot.waitUntil(lambda: dock.composition is not None)
        w, h = dock.canvas_size()
        assert dock.composition.ops == views.compose(
            views.pane_view(p.part), snap, w, h, measure=win.measure,
            known_families=win.known_families).ops, p.title


def test_a_dying_pane_says_so_and_the_window_goes_on(win, qtbot):
    # PERL-QUIRK (views): the Rules list always dies in DrawIt.
    dock = win.pane_docks[views.RULES_LIST]
    dock.show()
    win.show_snapshot(_snap("groups_relations"))
    qtbot.waitUntil(lambda: dock.composition is not None)
    assert dock.composition.died
    assert dock.windowTitle() == "Rules (died)"
    assert dock.toolTip()
    win.show_snapshot(_snap("empty"))         # still drawing


def test_pane_resize_redraws_at_the_new_size(win, qtbot):
    dock = win.pane_docks[views.WORKSPACE]
    dock.setFloating(True)
    dock.show()
    win.show_snapshot(_snap("six_elements"))
    dock.resize(500, 260)
    qtbot.waitUntil(lambda: dock.composed_size == dock.canvas_size())
    assert dock.canvas_size()[0] > 400


def test_coderack_pane_keeps_coderack_rows(win, qtbot):
    win.set_view(1)                            # the Workspace view: no Coderack
    dock = win.pane_docks[views.CODERACK]
    dock.show()
    qtbot.waitUntil(dock.isVisible)
    gui_recipes.build("coderack_small")
    from seqsee.gui.draw import coderack
    snap = snapshot.take()
    win.show_snapshot(snap)
    assert set(coderack.rack_families(snap)) <= set(win.known_families)


def test_pane_lists_page_independently(win, qtbot):
    dock = win.pane_docks[views.GROUPS_LIST]
    dock.show()
    qtbot.waitUntil(dock.isVisible)
    win.show_snapshot(_snap("groups_many"))
    dock.set_page(1)
    assert dock.pages == {views.GROUPS_LIST: 1}
    assert win.pages == {}
    assert dock.composition.lists[views.GROUPS_LIST].page_number == 1


# ---- seed and max steps --------------------------------------------------------------------
def test_seed_and_max_steps_fields(win, qtbot):
    assert win.seed_edit.text() == "" and win.seed() is None      # random (Seqsee.pl default)
    assert win.max_steps() == 100000                             # config/seqsee.conf
    qtbot.keyClicks(win.seed_edit, "4x2-")     # the validator refuses x and -
    assert win.seed_edit.text() == "42" and win.seed() == 42
    win.max_steps_box.setValue(500)
    assert win.max_steps() == 500


def test_options_fill_the_fields(qtbot):
    w = mainwindow.MainWindow(options={"seed": "7", "max_steps": "250"})
    qtbot.addWidget(w)
    assert w.seed() == 7 and w.max_steps() == 250


def test_restart_is_a_fresh_run_with_the_fields(win, fake):
    win.seed_edit.setText("3")
    win.max_steps_box.setValue(400)
    win.last_sequence = "1 1 2 1 2 3"
    win.restart_action.trigger()
    assert fake.calls[-1] == ("new_sequence", "1 1 2 1 2 3")
    assert fake.kwargs[-1] == {"seed": 3, "max_steps": 400}


def test_restart_without_a_sequence_asks_for_one(win, fake):
    win.restart_action.trigger()
    assert win.seq_dialog is not None and win.seq_dialog.isVisible()
    assert [c for c in fake.calls if c[0] == "new_sequence"] == []


def test_accept_passes_the_seed_and_max_steps_follows_the_box(win, fake):
    win.seed_edit.setText("9")
    win.accept_sequence("1 2 3", ["1", "2", "3"])
    assert fake.calls[-1] == ("accept_sequence", ["1", "2", "3"])
    assert fake.kwargs[-1] == {"seed": 9, "max_steps": 100000}
    win.max_steps_box.setValue(1234)
    assert fake.calls[-1] == ("set_max_steps", 1234)


def test_runner_seed_and_max_steps(qtbot):
    r = Runner(min_interval=1 / 30)
    r.start()
    done = []
    r.command_done.connect(lambda n, res: done.append(n))
    try:
        r.accept_sequence(["1", "2", "3"], seed=5)
        qtbot.waitUntil(lambda: "accept_sequence" in done, timeout=20000)
        assert int(util.perl_num(r.options["seed"])) == 5
        r.set_max_steps(7)
        r.continue_()
        qtbot.waitUntil(lambda: "continue" in done, timeout=20000)
        assert Global.Steps_Finished == 7
        assert len(sworkspace.get_elements()) >= 3
        r.set_max_steps(None)                  # back to the run's options
        assert r.max_steps is None
    finally:
        assert r.quit(timeout=10)


# ---- remembered layout ---------------------------------------------------------------------
def test_without_settings_nothing_is_remembered(win):
    assert win.settings is None
    win.save_settings()                        # does nothing, doesn't fail


def test_layout_round_trip_through_a_settings_file(qtbot, tmp_path):
    path = tmp_path / "seqsee.ini"
    w = mainwindow.MainWindow(settings=_settings(path))
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    w.set_view(5)
    w.seed_edit.setText("11")
    w.max_steps_box.setValue(321)
    w.resize(760, 700)                         # restoreGeometry keeps it on the 800 px screen
    w.pane_docks[views.SLIPNET].show()
    w.pane_docks[views.CODERACK].show()
    w.addDockWidget(Qt.LeftDockWidgetArea, w.pane_docks[views.CODERACK])
    w.commentary_dock.hide()
    qtbot.wait(10)
    w.close()                                  # closeEvent saves
    assert path.exists()
    s = _settings(path)
    assert set(s.allKeys()) >= {"geometry", "state", "view", "seed", "max_steps"}

    w2 = mainwindow.MainWindow(settings=_settings(path))
    qtbot.addWidget(w2)
    w2.show()
    qtbot.waitExposed(w2)
    assert w2.view == 5 and w2.view_actions[5].isChecked()
    assert w2.seed() == 11 and w2.max_steps() == 321
    assert (w2.width(), w2.height()) == (760, 700)
    assert not w2.pane_docks[views.SLIPNET].isHidden()
    assert not w2.pane_docks[views.CODERACK].isHidden()
    assert w2.dockWidgetArea(w2.pane_docks[views.CODERACK]) == Qt.LeftDockWidgetArea
    assert w2.dockWidgetArea(w2.pane_docks[views.SLIPNET]) == Qt.RightDockWidgetArea
    assert w2.pane_docks[views.STREAM].isHidden()
    assert w2.commentary_dock.isHidden()


def test_options_beat_remembered_settings(qtbot, tmp_path):
    path = tmp_path / "seqsee.ini"
    s = _settings(path)
    s.setValue("view", 3)
    s.setValue("seed", "8")
    s.setValue("max_steps", 99)
    s.sync()
    w = mainwindow.MainWindow(options={"view": "2", "seed": "4"}, settings=_settings(path))
    qtbot.addWidget(w)
    assert w.view == 2 and w.seed() == 4 and w.max_steps() == 99


def test_bad_settings_are_ignored(qtbot, tmp_path):
    path = tmp_path / "seqsee.ini"
    s = _settings(path)
    s.setValue("view", "nonsense")
    s.setValue("state", "not a state")
    s.setValue("max_steps", -5)
    s.sync()
    w = mainwindow.MainWindow(settings=_settings(path))
    qtbot.addWidget(w)
    assert w.view == 0 and w.max_steps() >= 1


def test_default_settings_are_seqsees():
    s = mainwindow.default_settings()
    assert s.organizationName() == "Seqsee" and s.applicationName() == "Seqsee"


# ---- screenshot ----------------------------------------------------------------------------
def test_docks_screenshot(qtbot, win, tmp_path, request):
    """The window with the Slipnet and Coderack panes docked on the right (no Perl
    counterpart: Perl has no docks). ``pytest --write-screens`` saves it under
    docs/gui/screens/."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    win.resize(1200, 760)
    win.set_view(1)
    for part in (views.SLIPNET, views.CODERACK, views.GROUPS_LIST):
        win.pane_docks[part].show()
    gui_recipes.build("groups_relations")
    win.show_snapshot(snapshot.take())
    qtbot.wait(20)
    path = out_dir / "window_docks.png"
    assert win.grab().save(str(path), "PNG")
