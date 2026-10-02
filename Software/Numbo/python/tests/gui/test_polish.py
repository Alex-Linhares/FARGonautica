"""loop0003 item 11: long runs, robustness, polish.

The window's state remembered between sessions (QSettings, in a file the test
owns), the keyboard shortcuts (space = play/pause, right arrow = step), an
error outcome shown cleanly on the canvas, and no memory growth from run to
run in one window.  pytest-qt, offscreen.
"""

import gc
import os
import threading
import tracemalloc

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings, Qt          # noqa: E402
from PySide6.QtGui import QImage                  # noqa: E402
from PySide6.QtTest import QTest                  # noqa: E402
from PySide6.QtWidgets import QGraphicsItem, QGraphicsView  # noqa: E402

from numbo.gui import controls as controls_module  # noqa: E402
from numbo.gui.main_window import MainWindow      # noqa: E402

TIMEOUT = 30000
MAX_SPEED = len(controls_module.DELAYS) - 1


@pytest.fixture
def thread_errors(monkeypatch):
    errors = []
    monkeypatch.setattr(threading, "excepthook", lambda args: errors.append(args))
    yield errors
    assert errors == []


@pytest.fixture
def ini(tmp_path):
    return str(tmp_path / "numbo-gui.ini")


def make_window(qtbot, settings=None):
    w = MainWindow(settings=settings)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


@pytest.fixture
def window(qtbot, thread_errors):
    w = make_window(qtbot)
    yield w
    w.close()
    assert not w.controller.is_alive()


def run_to_end(qtbot, w, puzzle=1, seed=1, cap="20000"):
    c = w.controls
    c.puzzle_combo.setCurrentIndex(puzzle - 1)
    c.seed_edit.setText(str(seed))
    c.cap_edit.setText(cap)
    c.speed_slider.setValue(MAX_SPEED)
    with qtbot.waitSignal(w.run_finished, timeout=TIMEOUT):
        c.run_to_end_button.click()
    return w.outcome


# -- QSettings -------------------------------------------------------------------

def test_the_window_state_is_remembered(qtbot, ini, thread_errors, monkeypatch):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    w.resize(1400, 900)
    qtbot.waitUntil(lambda: (w.width(), w.height()) == (1400, 900))
    geometry = bytes(w.saveGeometry())
    w.pnet_dock.hide()
    c = w.controls
    c.puzzle_combo.setCurrentIndex(2)
    c.seed_edit.setText("8")
    c.cap_edit.setText("5000")
    c.speed_slider.setValue(3)
    w.close()

    # Qt fits a restored geometry to the screen, and the offscreen screen
    # (800 x 800) is smaller than the window: check what is restored.
    restored = []
    real = MainWindow.restoreGeometry
    monkeypatch.setattr(MainWindow, "restoreGeometry",
                        lambda self, g: restored.append(bytes(g)) or real(self, g))
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert restored == [geometry]
        assert w2.pnet_dock.isHidden() and not w2.log_dock.isHidden()
        c = w2.controls
        assert c.puzzle_combo.currentIndex() == 2
        assert (c.seed_edit.text(), c.cap_edit.text()) == ("8", "5000")
        assert c.speed_slider.value() == 3
        assert w2.controller.delay == controls_module.DELAYS[3]
    finally:
        w2.close()


def test_a_custom_problem_is_remembered(qtbot, ini, thread_errors):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    c = w.controls
    c.puzzle_combo.setCurrentText(controls_module.CUSTOM)
    c.custom_edit.setText("6 1 1 1 1 1")
    w.close()
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.controls.puzzle_combo.currentText() == controls_module.CUSTOM
        assert w2.controls.custom_edit.text() == "6 1 1 1 1 1"
        assert w2.controls.custom_edit.isEnabled()
    finally:
        w2.close()


def test_bad_settings_give_the_defaults(qtbot, ini, thread_errors, capfd):
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue("window/geometry", b"not a geometry")
    s.setValue("window/state", "junk")
    s.setValue("controls/puzzle", "eleventy")
    s.setValue("controls/speed", 99)
    s.setValue("controls/seed", 12)            # not a string: shown as text
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        c = w.controls
        assert c.puzzle_combo.currentIndex() == 0
        assert c.speed_slider.value() == controls_module.DELAYS.index(
            controls_module.DEFAULT_DELAY)
        assert c.seed_edit.text() == "12"
        assert not w.pnet_dock.isHidden()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


def test_a_window_without_settings_writes_none(qtbot, tmp_path, thread_errors):
    QSettings.setPath(QSettings.Format.IniFormat, QSettings.Scope.UserScope, str(tmp_path))
    w = make_window(qtbot)
    w.close()
    assert list(tmp_path.iterdir()) == []


def test_a_smoke_run_reads_and_writes_no_settings(tmp_path):
    """--smoke runs with the defaults and leaves the user's settings alone
    (QSettings "numbo"/"numbo-gui" lives under XDG_CONFIG_HOME)."""
    import subprocess
    import sys
    env = dict(os.environ, XDG_CONFIG_HOME=str(tmp_path), QT_QPA_PLATFORM="offscreen")
    proc = subprocess.run([sys.executable, "-m", "numbo.gui", "--smoke"], env=env,
                          capture_output=True, text=True, timeout=120,
                          cwd=os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
    assert proc.returncode == 0, proc.stderr
    assert proc.stdout.splitlines() == ["outcome: solved, 45 iterations (seed 1)",
                                        "check: valid: 114 = (6 x 20) - (7 - 1)"]
    assert list(tmp_path.iterdir()) == []


# -- shortcuts --------------------------------------------------------------------

def key(qtbot, w, k, modifier=Qt.KeyboardModifier.NoModifier):
    w.activateWindow()
    qtbot.waitActive(w)
    w.tree_view.setFocus()
    QTest.keyClick(w.tree_view, k, modifier)


def test_right_arrow_steps_one_event(qtbot, window):
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_Right)
    assert window.position == 0 and window.controller.state == "paused"
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_Right)
    assert window.position == 1
    qtbot.wait(150)
    assert window.position == 1


def test_space_plays_and_pauses(qtbot, window):
    c = window.controls
    c.puzzle_combo.setCurrentIndex(2)       # puzzle 3 seed 1: long
    c.speed_slider.setValue(controls_module.DELAYS.index(0.005))
    key(qtbot, window, Qt.Key.Key_Space)
    qtbot.waitUntil(lambda: window.position > 20, timeout=TIMEOUT)
    assert window.controller.state == "running"
    key(qtbot, window, Qt.Key.Key_Space)
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    qtbot.wait(100)
    held = window.position
    qtbot.wait(200)
    assert window.position == held
    key(qtbot, window, Qt.Key.Key_Space)
    qtbot.waitUntil(lambda: window.position > held + 5, timeout=TIMEOUT)
    window.stop()


def test_shift_right_steps_an_iteration_and_escape_stops(qtbot, window):
    key(qtbot, window, Qt.Key.Key_Right, Qt.KeyboardModifier.ShiftModifier)
    qtbot.waitUntil(lambda: window.stats.model.iteration == 0, timeout=TIMEOUT)
    assert window.history.events[window.position].kind == "iteration"
    key(qtbot, window, Qt.Key.Key_Escape)
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped and not window.controller.is_alive()


def test_typing_in_a_field_does_not_trigger_the_shortcuts(qtbot, window):
    window.activateWindow()
    qtbot.waitActive(window)
    edit = window.controls.custom_edit
    window.controls.puzzle_combo.setCurrentText(controls_module.CUSTOM)
    edit.setFocus()
    QTest.keyClicks(edit, "6 1 1")
    QTest.keyClick(edit, Qt.Key.Key_Right)
    qtbot.wait(100)
    assert edit.text() == "6 1 1"
    assert window.controller.state == "idle" and window.position == -1


def test_the_sliders_leave_the_arrows_to_the_shortcuts(window):
    """The sliders take no keyboard focus (they would lose their arrow keys
    to the shortcuts anyway); the line edits and the spin box keep theirs."""
    assert window.controls.speed_slider.focusPolicy() == Qt.FocusPolicy.NoFocus
    assert window.timeline.slider.focusPolicy() == Qt.FocusPolicy.NoFocus
    assert window.controls.seed_edit.focusPolicy() != Qt.FocusPolicy.NoFocus


def test_the_run_menu_lists_the_shortcuts(window):
    actions = {a.text().replace("&", ""): a.shortcut().toString()
               for a in window.run_menu.actions() if not a.isSeparator()}
    assert actions["Play / Pause"] == "Space"
    assert actions["Step"] == "Right"
    assert actions["Step iteration"] == "Shift+Right"
    assert actions["Stop"] == "Esc"


def test_left_arrow_goes_back_one_event_after_a_run(qtbot, window):
    run_to_end(qtbot, window)
    end = window.position
    key(qtbot, window, Qt.Key.Key_Left)
    assert window.position == end - 1
    key(qtbot, window, Qt.Key.Key_Right)       # not live: forward one event
    assert window.position == end


# -- an error outcome --------------------------------------------------------------

def test_an_error_outcome_is_shown_cleanly(qtbot, window, capfd):
    outcome = run_to_end(qtbot, window, puzzle=1, seed=40)
    assert outcome.error is None and not outcome.stopped
    banner = window.canvas.banner
    assert banner.isVisible()
    assert banner.level == "error"
    text = banner.text()
    assert "Error after 29 iterations" in text
    assert "SEND: NIL does not handle the message :SET-ACTIVATION" in text
    assert "reactivate-cyto race" in text
    assert "error" in window.statusBar().currentMessage()
    assert window.stats.text("outcome").startswith("error, 29 iterations")
    # The canvas still shows the run's last trees.
    assert window.tree_view.node_items
    assert "Traceback" not in capfd.readouterr().err


def test_the_banner_follows_the_run(qtbot, window):
    run_to_end(qtbot, window, puzzle=1, seed=1)
    assert window.canvas.banner.isVisible() and window.canvas.banner.level == "good"
    assert "114 = (6 x 20) - (7 - 1)" in window.canvas.banner.text()
    # A new run hides it until it ends; a seek back hides it too.
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        window.controls.step_button.click()
    assert not window.canvas.banner.isVisible()
    window.stop()
    run_to_end(qtbot, window, puzzle=3, seed=8)
    assert window.canvas.banner.level == "warn"
    assert window.seek(10)
    assert not window.canvas.banner.isVisible()
    assert window.seek(len(window.history) - 1)
    assert window.canvas.banner.isVisible() and window.canvas.banner.level == "warn"
    window.canvas.banner.close_button.click()
    assert not window.canvas.banner.isVisible()


# -- memory --------------------------------------------------------------------------

def test_no_memory_growth_from_run_to_run(qtbot, window):
    """Puzzle 1 seed 1, six times in one window.  The history is per run, so
    after the first run (caches warm up) the memory held must stay flat: a
    leak per event (286 events a run) or per run would show."""
    sizes, objects, items = [], [], []
    tracemalloc.start()
    try:
        for _ in range(6):
            run_to_end(qtbot, window)
            qtbot.wait(20)
            gc.collect()
            sizes.append(tracemalloc.get_traced_memory()[0])
            objects.append(len(gc.get_objects()))
            items.append(len(window.tree_view.scene().items()))
    finally:
        tracemalloc.stop()
    assert len(window.history) == 286
    assert len(set(items)) == 1
    # Python objects: exactly flat once warm (measured: 39,757 every run).
    assert max(objects[1:]) - min(objects[1:]) < 50, objects
    # Bytes: the engine alone grows about 10 KB a run here, then levels off
    # (caches; scratch-layout/engine_memory.py), and PySide keeps a few
    # small objects (measured: 5-12 KB a run in all).  A leak of 100 bytes
    # an event would be 28 KB a run.
    growth = (sizes[-1] - sizes[1]) / (len(sizes) - 2)
    assert growth < 24 * 1024, sizes


# -- the every-event redraw, painted incrementally ------------------------------------

def shown(w, widget):
    """What the window's backing store holds for the visible part of WIDGET
    (what was painted, not a fresh render: the offscreen screen grabs the
    backing store)."""
    r = widget.visibleRegion().boundingRect()
    top = widget.mapTo(w, r.topLeft())
    img = w.screen().grabWindow(w.winId(), top.x(), top.y(), r.width(), r.height()).toImage()
    return img.convertToFormat(QImage.Format.Format_RGB32)


def fresh(widget, reset_caches=False):
    """A fresh render of the visible part of WIDGET.  RESET_CACHES (a
    graphics view's viewport): its item caches are emptied first, so each
    item is drawn from its state (a cached pixmap is as stale as the screen
    when an item failed to mark itself dirty), through its cache as the view
    draws it.  Only for items that never move: a DeviceCoordinateCache
    pixmap is kept when an item moves, so a box moved by a fraction of a
    pixel since it was cached has slightly different edges (measured on the
    tree canvas: 306 pixels, up to 40 apart)."""
    r = widget.visibleRegion().boundingRect()
    view = widget.parentWidget()
    if reset_caches and isinstance(view, QGraphicsView):
        for item in view.scene().items():
            mode = item.cacheMode()
            if mode != QGraphicsItem.CacheMode.NoCache:
                item.setCacheMode(QGraphicsItem.CacheMode.NoCache)
                item.setCacheMode(mode)
    return widget.grab(r).toImage().convertToFormat(QImage.Format.Format_RGB32)


def same_picture(a, b, tolerance=2):
    """A and B are the same picture: equal, or no channel of any pixel more
    than TOLERANCE apart.  (An antialiased edge can round one step
    differently in the backing store than in a grab's pixmap: measured, one
    pixel 1 apart.  A stale paint differs by far more.)"""
    if a == b:
        return True
    if a.size() != b.size():
        return False
    da = bytes(a.constBits())[:a.sizeInBytes()]
    db = bytes(b.constBits())[:b.sizeInBytes()]
    return all(abs(x - y) <= tolerance for x, y in zip(da, db))


class PixelProbe:
    """At each event_drawn: every view's painted pixels equal a fresh render
    of it, and the tree canvas was painted exactly once while the window
    handled the event (from the probe's on_event, a view of the window, to
    event_drawn; a paint between events, such as an input changed before a
    run, isn't the event's).  (Its
    items' styles are checked against the model at every event by
    test_tree_view.py's check_scene; the Pnet, painted only where pnodes
    changed, is checked here with its caches emptied.)"""

    def __init__(self, w, every=1):
        self.w, self.every = w, every
        self.widgets = {"tree": w.tree_view.viewport(), "pnet": w.pnet_view.viewport(),
                        "rack": w.coderack_view, "log": w.log_view.list.viewport(),
                        "stats": w.stats}
        self.mismatches = []
        self.checked = 0
        self.tree_paints = []
        self.last = w.tree_view.paints
        w.add_view(self)
        w.event_drawn.connect(self.drawn)

    def on_event(self, event):
        self.last = self.w.tree_view.paints

    def drawn(self, index):
        tv = self.w.tree_view
        self.tree_paints.append(tv.paints - self.last)
        if index % self.every == 0:
            self.checked += 1
            for name, widget in self.widgets.items():
                a, b = shown(self.w, widget), fresh(widget, reset_caches=name == "pnet")
                if not same_picture(a, b):
                    if not self.mismatches and os.environ.get("NUMBO_PIXEL_DEBUG"):
                        a.save(f"gui_snapshots/mismatch-{name}-shown.png")
                        b.save(f"gui_snapshots/mismatch-{name}-fresh.png")
                    self.mismatches.append((index, name))


def test_every_event_is_painted_and_the_pixels_are_the_events(qtbot, window):
    """Puzzle 1 seed 1 played at full speed.  After each event (before its
    acknowledgment), what the window shows of every view equals a fresh
    render of that view: the incremental paints (the log's blitted scroll,
    the Pnet's dirty items) leave nothing stale."""
    probe = PixelProbe(window)
    run_to_end(qtbot, window)
    assert probe.checked == 286
    assert probe.mismatches == []
    assert set(probe.tree_paints) == {1}, [i for i, n in enumerate(probe.tree_paints) if n != 1]


def test_the_pixels_stay_right_with_a_filter_and_a_replay(qtbot, window, tmp_path):
    """Kinds hidden in the log, then a session replay played on from the
    middle (the greyed future rows turning black one by one)."""
    window.log_view.kind_actions["post"].setChecked(False)
    window.log_view.kind_actions["node-changed"].setChecked(False)
    run_to_end(qtbot, window, puzzle=3, seed=8)
    path = tmp_path / "s.jsonl"
    assert window.save_session(path)
    window.log_view.kind_actions["post"].setChecked(True)
    assert window.open_file(path)
    assert window.seek(500)
    probe = PixelProbe(window, every=3)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.play()
    assert window.position == len(window.history) - 1
    assert probe.checked > 150
    assert probe.mismatches == []
