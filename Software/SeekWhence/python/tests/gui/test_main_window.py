"""Loop0004 item 02: the main window (`seekwhence/gui/main_window.py`), after the Numbo
GUI's `tests/gui/test_gui_shell.py` and `test_polish.py`. pytest-qt, offscreen.

- The window is laid out as PANES.md section 3: the docks, their areas and tab groups,
  the hidden Configuration dock, the timeline toolbar, the six menus, Reset layout.
- A scripted run through the window equals the plain run (events, text, draws, outcome);
  every event comes on the GUI thread while the worker waits; so does
  `headless.run(window=True)`, the gate's MainWindow comparison.
- Stop mid-run, and stop or close with a guess pending, leave no live thread.
- An interactive guess answered by clicks (No, then OK; OK) gives the same events as the
  scripted answers [M86 pp. 166, 175].
- Settings, the default layout file and Reset layout round-trip (ANSWERS.md 3, 8).
- Each visible view is painted exactly once per event (a counting probe, as Numbo's).
"""

import io
import json
import threading

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings, Qt                    # noqa: E402
from PySide6.QtWidgets import QDockWidget                   # noqa: E402

from seekwhence.events import Recorder as EngineRecorder     # noqa: E402
from seekwhence.gui import headless                          # noqa: E402
from seekwhence.gui.main_window import AREAS, PANES, TIMELINE_NAME, MainWindow  # noqa: E402
from seekwhence.gui.models import layout_file, presets       # noqa: E402
from seekwhence.session import Session                       # noqa: E402
from seekwhence.trace import event_record                    # noqa: E402

from golden import APPENDIX                                  # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


class Recorder:
    """A GUI-thread view: records each event, the thread it came on, and how many events
    the worker had published by then (Numbo GUI, `test_gui_shell.py`)."""

    def __init__(self, window):
        self.window = window
        self.events = []
        self.published = []
        self.threads = set()

    def on_event(self, event):
        self.events.append(event)
        self.published.append(self.window.controller.events_published)
        self.threads.add(threading.current_thread() is threading.main_thread())


def plain(inputs, answers=None):
    """The same run without the GUI: records, text, draws, outcome."""
    rec, out = EngineRecorder(), io.StringIO()
    session = Session(inputs.config, inputs.seed, out=out, lines=list(inputs.lines),
                      answers=list(inputs.answers if answers is None else answers),
                      observers=[rec])
    session.run()
    return ([event_record(e) for e in rec.events], out.getvalue(), session.world.rng.draws,
            session.outcome)


def records_of(window):
    return [event_record(e) for e in window.history.events]


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def make_window(qtbot, settings=None, layout_path=None):
    w = MainWindow(settings=settings, layout_path=layout_path)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


def on_screen(widget):
    """Whether the widget has pixels on the window: a dock tabbed under another is
    `isVisible()` in Qt 6 but has an empty visible region."""
    return not widget.visibleRegion().isEmpty()


def placeholders(window):
    """Every view that takes events: the controls pane (item 14) takes none (G-049)."""
    return [window.sequence_view, window.cytoplasm_view] + [
        getattr(window, f"{attr}_view") for attr, *_ in PANES if attr != "controls"]


# -- the layout (PANES.md section 3) -------------------------------------------------------------

def test_the_window_is_laid_out_as_panes_md(window):
    docks = {d.objectName(): d for d in window.findChildren(QDockWidget)}
    assert set(docks) == {name for _a, name, *_ in PANES}
    for _attr, name, title, _pane, _pages, area, under, shown in PANES:
        dock = docks[name]
        assert dock.windowTitle() == title
        assert dock.features() & QDockWidget.DockWidgetFeature.DockWidgetMovable
        assert window.dockWidgetArea(dock) == AREAS[area], name
        assert dock.toggleViewAction().isChecked() == shown, name
        if under is not None:
            if not shown:       # Qt lists the tab siblings of visible docks only (G-058)
                dock.show()
            assert docks[under] in window.tabifiedDockWidgets(dock), name
            if not shown:
                dock.hide()
    assert window.config_dock.isHidden()
    assert on_screen(window.hypothesis_view) and not on_screen(window.reformulation_view)
    assert on_screen(window.console_view) and not on_screen(window.log_view)
    assert window.timeline_bar.objectName() == TIMELINE_NAME
    assert window.toolBarArea(window.timeline_bar) == Qt.ToolBarArea.BottomToolBarArea
    assert window.splitter.count() == 2 and window.splitter.orientation() == Qt.Orientation.Vertical
    assert window.centralWidget() is window.splitter
    assert window.controller.state == "idle" and window.state == "idle"
    assert window.layout_source == "built-in"


def test_the_menus_are_session_edit_run_view_presets_help(window):
    titles = [a.text() for a in window.menuBar().actions()]
    assert titles == ["&Session", "&Edit", "&Run", "&View", "&Presets", "&Help"]
    view = [a.text() for a in window.view_menu.actions() if a.text()]
    for _attr, _name, title, *_ in PANES:
        assert title in view
    assert "Timeline" in view
    assert "&Reset layout" in view and "Reset to &built-in layout" in view
    assert "Save layout as &default for this code…" in view
    run = {a.text(): a.shortcut().toString() for a in window.run_menu.actions() if a.text()}
    assert run["Play / Pause"] == "Space" and run["Step"] == "Right"
    assert run["Step term"] == "Shift+Right" and run["Stop"] == "Esc"
    assert run["Back one event"] == "Left" and run["New run"] == "Ctrl+N"
    assert "Step task" in run and "Run to end" in run
    assert window.open_action.isEnabled()                  # item 13
    assert not window.save_action.isEnabled()              # no history yet


def test_edit_configuration_shows_the_hidden_dock(window):
    assert window.config_dock.isHidden()
    window.configuration_action.trigger()
    assert window.config_dock.isVisible()


def test_a_preset_sets_the_inputs_and_the_title(window):
    action = [a for a in window.presets_menu.actions() if a.text().startswith("run 3")][0]
    action.trigger()
    assert window.inputs.preset == RUN3 and window.inputs.seed == 30
    assert window.inputs.lines[:2] == (1, 2) and window.inputs.answers == ("nope", "ok")
    assert "run 3" in window.windowTitle()


# -- runs through the window -------------------------------------------------------------------

def test_a_scripted_run_through_the_window_equals_the_plain_run(window, qtbot):
    recorder = Recorder(window)
    window.add_view(recorder)
    drawn = []
    window.event_drawn.connect(drawn.append)
    inputs = presets.appendix_inputs(RUN1)
    outcome = run_to_end(qtbot, window, inputs)
    records, text, draws, result = plain(inputs)
    assert records_of(window) == records
    assert [event_record(e) for e in recorder.events] == records
    assert outcome.output == text
    assert outcome.result["draws"] == draws and outcome.result["outcome"] == result == "ok"
    assert outcome.result["hypothesis"] == "(Cycle 3 2 (3 7))"       # [M86 p. 166]
    assert len(records) == 174
    # Every event came on the GUI thread, while the worker waited on it.
    assert recorder.threads == {True}
    assert recorder.published == list(range(1, len(records) + 1))
    assert drawn == list(range(len(records)))
    assert window.position == len(records) - 1
    for view in placeholders(window):
        assert view.events == len(records)
    assert window.state == "finished" and not window.controller.is_alive()
    assert window.statusBar().currentMessage() == outcome.result["summary"]


def test_headless_through_the_window_equals_the_plain_run(qtbot, thread_errors):
    inputs = presets.appendix_inputs(RUN2)
    got = headless.run(inputs.config, inputs.seed, list(inputs.lines), list(inputs.answers),
                       window=True)
    records, _text, draws, result = plain(inputs)
    assert got == {"records": records, "draws": draws, "outcome": result}
    assert len(records) == 297


def test_stop_mid_run_leaves_no_live_thread(window, qtbot):
    inputs = presets.appendix_inputs(RUN3)
    window.start_session(inputs, paused=True)
    for n in range(1, 4):
        window.step()
        qtbot.waitUntil(lambda: window.position == n - 1, timeout=TIMEOUT)
    assert window.controller.is_alive()
    window.stop()
    assert not window.controller.is_alive()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped and window.state == "stopped"
    assert len(window.history) == 3
    # A run after a stop is unchanged.
    run_to_end(qtbot, window, inputs)
    assert records_of(window) == plain(inputs)[0]


def test_the_space_and_escape_actions_drive_the_controller(window, qtbot):
    window.set_inputs(presets.appendix_inputs(RUN1))
    window.play_pause()                       # starts a run, playing
    qtbot.waitUntil(lambda: window.state in ("running", "finished"), timeout=TIMEOUT)
    window.play_pause()                       # pauses it, if it is still going
    qtbot.waitUntil(lambda: window.state in ("paused", "finished"), timeout=TIMEOUT)
    window.stop()
    assert not window.controller.is_alive()


# -- the interactive guess (G-008) -------------------------------------------------------------------

def click(qtbot, button):
    assert button.isEnabled()
    qtbot.mouseClick(button, Qt.MouseButton.LeftButton)


def test_an_interactive_guess_answered_no_then_ok_equals_the_scripted_answers(window, qtbot):
    inputs = presets.appendix_inputs(RUN3, interactive=True)
    shown = []
    window.guess_shown.connect(shown.append)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        # The display is already at the guess: the gate drew it before the engine asked.
        assert window.history.events[window.position].kind == "guess"
        assert window.guess_panel.pending.block == "(1 2)(1 2)(1 2)"      # [M86 p. 175]
        assert "I have a guess!" in window.guess_panel.message.text()
        click(qtbot, window.guess_panel.no_button)
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        assert window.guess_panel.pending.block == "(1 2)(1 2 3)(1 2 3 4)"  # [M86 p. 179]
        click(qtbot, window.guess_panel.ok_button)
    records, text, draws, result = plain(inputs, answers=["no", "ok"])
    assert records_of(window) == records
    assert window.outcome.output == text
    assert window.outcome.result["draws"] == draws and result == "ok"
    assert [p.block for p in shown] == ["(1 2)(1 2)(1 2)", "(1 2)(1 2 3)(1 2 3 4)"]
    assert not window.guess_panel.waiting and not window.guess_panel.ok_button.isEnabled()


def test_an_interactive_guess_answered_ok_equals_run_1(window, qtbot):
    inputs = presets.appendix_inputs(RUN1, interactive=True)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        assert window.guess_panel.pending.form == "(Cycle 3 2 (3 7))"      # [M86 p. 166]
        click(qtbot, window.guess_panel.ok_button)
    records, text, draws, result = plain(presets.appendix_inputs(RUN1))     # answers: ok
    assert records_of(window) == records and window.outcome.output == text
    assert window.outcome.result["draws"] == draws


def test_an_answer_with_no_guess_waiting_is_refused(window):
    assert not window.answer_guess("ok")
    assert not window.guess_panel.ok_button.isEnabled()


def test_stop_with_a_guess_pending_leaves_no_live_thread(window, qtbot):
    inputs = presets.appendix_inputs(RUN1, interactive=True)
    window.start_session(inputs)
    window.controller.run_to_end()
    qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
    window.stop()
    assert not window.controller.is_alive()
    assert not window.guess_panel.waiting
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped
    # The next run is unaffected.
    scripted = presets.appendix_inputs(RUN1)
    run_to_end(qtbot, window, scripted)
    assert records_of(window) == plain(scripted)[0]


def test_closing_with_a_guess_pending_joins_the_worker(qtbot, thread_errors):
    w = make_window(qtbot)
    w.start_session(presets.appendix_inputs(RUN1, interactive=True))
    w.controller.run_to_end()
    qtbot.waitUntil(lambda: w.guess_panel.waiting, timeout=TIMEOUT)
    assert w.shutdown()
    assert not w.controller.is_alive()
    w.close()


# -- the history: seeking -------------------------------------------------------------------------

def test_seeking_after_a_run(window, qtbot):
    inputs = presets.appendix_inputs(RUN1)
    run_to_end(qtbot, window, inputs)
    n = len(window.history)
    drawn = []
    window.event_drawn.connect(drawn.append)
    window.back_one()
    assert window.position == n - 2 and drawn == [n - 2]
    window.step_or_forward()
    assert window.position == n - 1
    assert window.seek(0) and window.position == 0
    assert all(view.events == 1 for view in placeholders(window))
    assert window.seek(10) and all(view.events == 11 for view in placeholders(window))
    assert not window.seek(n)


# -- the every-event redraw (Numbo GUI, test_polish.py) -------------------------------------------

class PaintProbe:
    """At each `event_drawn`: every visible placeholder was painted exactly once since
    the probe's `on_event`, and no hidden one at all."""

    def __init__(self, window):
        self.window = window
        self.views = placeholders(window)
        self.before = {}
        self.anomalies = []
        self.checked = 0
        window.add_view(self)
        window.event_drawn.connect(self.drawn)

    def on_event(self, event):
        self.before = {v: v.paints for v in self.views}

    def drawn(self, index):
        self.checked += 1
        for view in self.views:
            paints = view.paints - self.before.get(view, view.paints)
            expected = 1 if on_screen(view) else 0
            if paints != expected:
                self.anomalies.append((index, view.pane_id, paints, expected))


def test_each_view_is_painted_exactly_once_per_event(window, qtbot):
    probe = PaintProbe(window)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert probe.checked == 174
    assert probe.anomalies == []


# -- QSettings, the default layout file and Reset layout (ANSWERS.md 3, 8) -----------------------------

def test_the_window_state_and_inputs_are_remembered(qtbot, ini, thread_errors):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    w.platoplasm_dock.hide()
    w.set_inputs(presets.appendix_inputs(RUN3, seed=7))
    w.close()
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.layout_source == "settings"
        assert w2.platoplasm_dock.isHidden() and not w2.taskrack_dock.isHidden()
        assert w2.inputs.preset == RUN3 and w2.inputs.seed == 7
        # Reset layout: the built-in (there is no default file here); the saved state
        # is cleared.
        w2.reset_layout()
        assert not w2.platoplasm_dock.isHidden()
        assert w2.layout_source == "built-in"
        assert not QSettings(ini, QSettings.Format.IniFormat).contains("window/state")
    finally:
        w2.close()


def test_bad_settings_give_the_defaults(qtbot, ini, thread_errors, capfd):
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue("window/geometry", b"not a geometry")
    s.setValue("window/state", "junk")
    s.setValue("controls/preset", "eleventy")
    s.setValue("controls/seed", "many")
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w.layout_source == "built-in"
        assert w.inputs.preset == presets.DEFAULT_PRESET
        assert not w.platoplasm_dock.isHidden()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


def test_a_window_without_settings_writes_none(qtbot, tmp_path, thread_errors):
    QSettings.setPath(QSettings.Format.IniFormat, QSettings.Scope.UserScope, str(tmp_path))
    w = make_window(qtbot, layout_path=tmp_path / "default_layout.json")
    w.close()
    assert list(tmp_path.iterdir()) == []


def test_save_layout_as_default_and_a_fresh_window_opens_with_it(qtbot, tmp_path, thread_errors):
    path = tmp_path / "default_layout.json"
    w = make_window(qtbot, layout_path=path)
    assert w.layout_source == "built-in"           # no file yet
    w.taskrack_dock.hide()
    w.reformulation_dock.raise_()
    assert w.save_layout_as_default() == path
    w.close()
    text = path.read_text()
    data = json.loads(text)
    assert data["format"] == layout_file.FORMAT and data["version"] == layout_file.VERSION
    assert "geometry" not in text and len(text.encode()) < layout_file.MAX_BYTES
    placements = {d["name"]: d for d in data["docks"]}
    assert placements["taskrack-dock"]["visible"] is False
    assert placements["hypothesis-dock"]["tabbed_with"] == ["reformulation-dock"]

    w2 = make_window(qtbot, layout_path=path)
    try:
        assert w2.layout_source == "default file"
        assert w2.taskrack_dock.isHidden()
        assert on_screen(w2.reformulation_view) and not on_screen(w2.hypothesis_view)
        w2.reset_builtin_layout()
        assert w2.layout_source == "built-in" and not w2.taskrack_dock.isHidden()
        assert on_screen(w2.hypothesis_view) and not on_screen(w2.reformulation_view)
        w2.reset_layout()
        assert w2.layout_source == "default file" and w2.taskrack_dock.isHidden()
    finally:
        w2.close()


def test_the_users_settings_override_the_default_file(qtbot, ini, tmp_path, thread_errors):
    path = tmp_path / "default_layout.json"
    w = make_window(qtbot, layout_path=path)
    w.taskrack_dock.hide()
    w.save_layout_as_default()
    w.close()
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat), layout_path=path)
    assert w2.layout_source == "default file" and w2.taskrack_dock.isHidden()
    w2.taskrack_dock.show()
    w2.stats_dock.hide()
    w2.close()
    w3 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat), layout_path=path)
    try:
        assert w3.layout_source == "settings"
        assert w3.stats_dock.isHidden() and not w3.taskrack_dock.isHidden()
        w3.reset_layout()                           # back to the file's arrangement
        assert w3.layout_source == "default file"
        assert w3.taskrack_dock.isHidden() and not w3.stats_dock.isHidden()
    finally:
        w3.close()


@pytest.mark.parametrize("text", [
    "not json at all",
    json.dumps({"format": "something-else", "version": 1}),
    json.dumps({"format": layout_file.FORMAT, "version": 99, "docks": [], "state": "AAAA"}),
    json.dumps({"format": layout_file.FORMAT, "version": 1, "state": "AAAA",
                "docks": [{"name": "no-such-dock", "visible": True, "area": "left"}]}),
    json.dumps({"format": layout_file.FORMAT, "version": 1, "state": "AAAA", "docks": []}),
])
def test_a_bad_default_file_falls_back_to_the_built_in(qtbot, tmp_path, thread_errors, capfd, text):
    path = tmp_path / "default_layout.json"
    path.write_text(text)
    w = make_window(qtbot, layout_path=path)
    try:
        assert w.layout_source == "built-in"
        assert "built-in layout" in w.statusBar().currentMessage()
        assert not w.taskrack_dock.isHidden() and w.config_dock.isHidden()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


def test_the_shipped_default_layout_equals_the_built_in(qtbot, thread_errors):
    record = layout_file.read_layout(layout_file.DEFAULT_PATH)
    assert record is not None and record.version == layout_file.VERSION
    assert layout_file.DEFAULT_PATH.stat().st_size < layout_file.MAX_BYTES
    assert "geometry" not in layout_file.DEFAULT_PATH.read_text()
    by_name = {d.name: d for d in record.docks}
    for _attr, name, _title, _pane, _pages, area, under, shown in PANES:
        assert by_name[name].visible == shown and by_name[name].area == area, name
        if under is not None and shown:     # a hidden dock records no tab siblings (G-058)
            assert under in by_name[name].tabbed_with
    w = make_window(qtbot, layout_path=layout_file.DEFAULT_PATH)
    try:
        assert w.layout_source == "default file"
        for _attr, name, *_rest, shown in PANES:
            assert w.docks[name].toggleViewAction().isChecked() == shown, name
        assert on_screen(w.hypothesis_view) and on_screen(w.console_view)
        assert not on_screen(w.reformulation_view) and not on_screen(w.log_view)
    finally:
        w.close()
