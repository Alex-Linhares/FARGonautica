"""The controls (seqsee/gui/qt/controls.py and the main window's toolbar, Run menu, key
bindings, InterstepSleep slider, codelet count and status bar).

Mirrors config/GUI_sparse.conf ([buttons] Start / Pause / Quit, [bindings], [Scale],
[SCodeletCount]) as lib/SGUI.pm's CreateWidgets / SetupButtons / SetupBindings build it,
Seqsee.pl's Interaction_step / _step_n / _crawl / _continue, and lib/Tk/SCodeletCount.pm
(the label shows ``$Global::Steps_Finished || 0``). The golden gui_controls.json
(oracle/gui_controls.pl) records the button frame, each button's and key binding's effect
(the Interaction_* call it makes, or the globals it changes) and the count's font and text.
"""
from pathlib import Path

import pytest

import golden
import gui_recipes
from seqsee import global_ as Global
from seqsee.gui import snapshot

pytestmark = pytest.mark.gui

QtCore = pytest.importorskip("PySide6.QtCore")
QtGui = pytest.importorskip("PySide6.QtGui")
QtWidgets = pytest.importorskip("PySide6.QtWidgets")

from seqsee.gui.qt import controls, mainwindow, render  # noqa: E402
from seqsee.gui.runner import Runner  # noqa: E402

PY = Path(__file__).resolve().parents[1]
SCREENS_DIR = PY / "docs" / "gui" / "screens"
PERL_DIR = PY / "docs" / "gui" / "perl"
GOLDEN = golden.load("gui_controls")[0]
SEQ = "1 1 2 1 2 3"


def perl_command(effect):
    """The runner command a Perl button/binding effect corresponds to: (command, arg)."""
    calls = effect["calls"]
    if calls:
        assert len(calls) == 1, calls
        name, *args = calls[0]
        if name == "Interaction_continue":
            return ("continue", None)
        if name == "Interaction_step":
            return ("step", None)
        if name == "Interaction_step_n":
            assert set(args[0]) == {"n"}
            return ("step_n", args[0]["n"])
        if name == "Interaction_crawl":
            return ("crawl", args[0])
        if name == "SGUI::ask_seq":
            return ("new_sequence", None)
        if name in ("exit", "Tk::exit"):
            return ("quit", None)
        raise AssertionError(name)
    before, after = effect["before"], effect["after"]
    if before["debugMAX"] != after["debugMAX"]:
        return ("debug_max", None)
    if after["Break_Loop"] == 1:        # (the second press finds it set already)
        return ("pause", None)
    raise AssertionError(effect)


class FakeRunner(QtCore.QObject):
    """Records the commands; emits what a Runner emits."""
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
        self.state = "idle"

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        return lambda *a, **k: self.calls.append((name,) + a)


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


# ---- the tables, against GUI_sparse.conf as Perl reads it ------------------------------------
def test_bindings_match_the_perl_bindings():
    perl = {}
    for b in GOLDEN["bindings"]:
        first, second = (perl_command(e) for e in b["effects"])
        assert first == second
        perl[b["sequence"]] = first
    ours = {b.key: (b.command, b.arg) for b in controls.BINDINGS}
    assert ours == perl
    assert sorted(perl) == sorted("dfgscqpxhjklm")


def test_m_toggles_debug_max_from_undef():
    m = next(b for b in GOLDEN["bindings"] if b["sequence"] == "m")
    assert [e["after"]["debugMAX"] for e in m["effects"]] == [1, 0]
    assert m["effects"][0]["before"]["debugMAX"] is None
    assert controls.toggled_debug_max(None) == 1
    assert controls.toggled_debug_max(1) == 0
    assert controls.toggled_debug_max(0) == 1


def test_buttons_match_the_perl_button_frame():
    buttons = [b for b in GOLDEN["buttons"] if b["class"] == "Tk::Button"]
    assert [(b["text"], b["side"]) for b in buttons] == [
        ("Start", "left"), ("Pause", "left"), ("Quit", "left")]
    assert [(t, c) for t, c in controls.BUTTONS] == [
        (b["text"], perl_command(b["effect"])[0]) for b in buttons]


def test_count_font_and_place_match_perl():
    count = next(b for b in GOLDEN["buttons"] if b["class"] == "Tk::SCodeletCount")
    assert count["side"] == "right"
    assert controls.COUNT_FONT == GOLDEN["count"]["font"]
    actual = GOLDEN["count"]["font_actual"]
    f = render.qfont(controls.COUNT_FONT)
    assert f.pixelSize() == -actual["-size"] == 20
    assert f.bold() == (actual["-weight"] == "bold")


def test_count_text_is_steps_or_zero():
    for steps, text in GOLDEN["count"]["text"].items():
        assert controls.count_text(None if steps == "undef" else int(steps)) == text


def test_slider_range_is_the_scale_config():
    sc = GOLDEN["scale_config"]
    assert (controls.SLEEP_MIN, controls.SLEEP_MAX, controls.SLEEP_TICK) == (
        int(sc["-from"]), int(sc["-to"]), int(sc["-tickinterval"]))
    assert sc["-variable"].endswith("InterstepSleep")


# ---- the window's widgets -------------------------------------------------------------------
def test_toolbar_has_start_pause_quit_first_and_the_count(win):
    texts = [a.text() for a in win.toolbar.actions() if a.text()]
    assert texts[:3] == ["Start", "Pause", "Quit"]
    assert win.count_label.text() == "0"
    assert win.count_label.font().pixelSize() == 20
    assert win.count_label.font().bold()
    assert win.toolbar.widgetForAction(win.count_action) is not None


def test_count_and_slider_are_visible_at_the_initial_size(win, qtbot):
    """Nothing hides behind the toolbar's overflow button at the 780 px canvas width."""
    qtbot.wait(10)
    assert win.canvas_size() == (mainwindow.CANVAS_WIDTH, mainwindow.CANVAS_HEIGHT)
    for widget in (win.count_label, win.sleep_slider, win.sleep_value_label):
        assert widget.isVisible()
        right = widget.mapTo(win, QtCore.QPoint(widget.width(), 0)).x()
        assert right <= win.width()
    for a in list(win.button_actions) + [win.run_actions[k] for k in "shjklxm"]:
        assert win.toolbar.widgetForAction(a).isVisible(), a.text()


def test_run_menu_lists_every_binding_with_its_key(win):
    assert win.menu_titles() == ["View", "Panes", "Run", "Save", "Help"]
    assert set(win.run_actions) == {b.key for b in controls.BINDINGS}
    for b in controls.BINDINGS:
        a = win.run_actions[b.key]
        assert a.shortcut() == QtGui.QKeySequence(b.key.upper())
        assert a.text()


def test_status_bar_widgets_start_empty(win):
    labels = win.status_labels
    assert set(labels) == {"steps", "elements", "groups", "state"}
    assert labels["steps"].text() == "Steps: 0"
    assert labels["elements"].text() == "Elements: 0"
    assert labels["groups"].text() == "Groups: 0"
    assert labels["state"].text() == "No model"


def test_snapshot_updates_count_and_status(win):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    assert win.count_label.text() == controls.count_text(snap.steps)
    assert win.status_labels["elements"].text() == f"Elements: {snap.element_count}"
    assert win.status_labels["groups"].text() == f"Groups: {len(snap.groups)}"
    assert win.status_labels["steps"].text() == f"Steps: {snap.steps}"


def test_snapshot_with_steps(win):
    import dataclasses
    snap = dataclasses.replace(_snap("six_elements"), steps=17)
    win.show_snapshot(snap)
    assert win.count_label.text() == "17"
    assert win.status_labels["steps"].text() == "Steps: 17"
    assert win.status_labels["elements"].text() == "Elements: 6"


def test_died_message_and_status_widgets_coexist(win):
    snap = _snap("groups_relations")
    win.show_snapshot(snap)
    win.statusBar().showMessage("Model error: boom")
    assert win.status_labels["groups"].isVisible()


# ---- driving the runner ---------------------------------------------------------------------
@pytest.mark.parametrize("binding", controls.BINDINGS, ids=lambda b: b.key)
def test_each_run_action_drives_the_runner(win, fake, monkeypatch, binding):
    monkeypatch.setattr(win, "close", lambda: fake.calls.append(("close",)))
    monkeypatch.setattr(win, "ask_seq", lambda: fake.calls.append(("ask_seq",)))
    win.run_actions[binding.key].trigger()
    expected = {
        "step": [("step",)],
        "step_n": [("step_n", binding.arg)],
        "crawl": [("crawl", binding.arg)],
        "continue": [("continue_",)],
        "pause": [("pause",)],
        "new_sequence": [("ask_seq",)],
        "quit": [("close",)],
        "debug_max": [("set_debug_max", 1)],
    }[binding.command]
    assert fake.calls == expected


def test_keys_trigger_the_bindings(win, fake, qtbot):
    win.activateWindow()
    qtbot.waitActive(win)
    for key in "shjkldfgcp":
        qtbot.keyClick(win.canvas, key)
    assert fake.calls == [("step",), ("step_n", 5), ("step_n", 25), ("step_n", 50),
                          ("step_n", 100), ("crawl", 30), ("crawl", 300), ("crawl", 700),
                          ("continue_",), ("pause",)]


def test_buttons_drive_the_runner(win, fake, monkeypatch):
    monkeypatch.setattr(win, "close", lambda: fake.calls.append(("close",)))
    for a in win.button_actions:
        a.trigger()
    assert fake.calls == [("continue_",), ("pause",), ("close",)]


def test_debug_max_toggles(win, fake):
    m = win.run_actions["m"]
    m.trigger()
    m.trigger()
    m.trigger()
    assert fake.calls == [("set_debug_max", 1), ("set_debug_max", 0), ("set_debug_max", 1)]
    assert m.isChecked()


def test_slider_sets_interstep_sleep(win, fake):
    s = win.sleep_slider
    assert (s.minimum(), s.maximum(), s.tickInterval()) == (0, 40, 10)
    s.setValue(25)
    assert fake.calls == [("set_interstep_sleep", 25)]
    assert win.sleep_value_label.text() == "25 ms"


def test_crawl_and_continue_move_the_slider_quietly(win, fake):
    win.run_actions["d"].trigger()          # Interaction_crawl(30): InterstepSleep = 30
    assert win.sleep_slider.value() == 30
    assert win.interstep_sleep == 30
    win.run_actions["g"].trigger()          # 700 ms: beyond the slider, which shows its maximum
    assert win.sleep_slider.value() == 40 and win.interstep_sleep == 700
    assert win.sleep_value_label.text() == "700 ms"
    win.run_actions["c"].trigger()          # Interaction_continue: InterstepSleep = 0
    assert win.sleep_slider.value() == 0 and win.interstep_sleep == 0
    assert fake.calls == [("crawl", 30), ("crawl", 700), ("continue_",)]


def test_new_sequence_keeps_sleep_and_debug_max(win, fake):
    """SGUI::ask_seq keeps $Global::InterstepSleep and debugMAX; the port's new run resets
    them, so the window sets them again."""
    win.sleep_slider.setValue(15)
    win.run_actions["m"].trigger()
    fake.calls.clear()
    fake.command_done.emit("new_sequence", {})
    assert fake.calls == [("set_interstep_sleep", 15), ("set_debug_max", 1)]


def test_state_label_follows_the_runner(win, fake):
    labels = win.status_labels
    fake.state_changed.emit("idle")
    assert labels["state"].text() == "Idle"
    fake.state_changed.emit("running")
    assert labels["state"].text() == "Running"
    fake.state_changed.emit("waiting")
    assert labels["state"].text() == "Waiting for an answer"
    win.run_actions["p"].trigger()
    fake.state_changed.emit("running")
    fake.state_changed.emit("idle")
    assert labels["state"].text() == "Paused"
    win.run_actions["s"].trigger()
    fake.state_changed.emit("running")
    fake.state_changed.emit("idle")
    assert labels["state"].text() == "Idle"
    fake.state_changed.emit("stopped")
    assert labels["state"].text() == "Stopped"


def test_commands_without_a_runner_say_so(win):
    win.run_actions["s"].trigger()
    assert "No model" in win.statusBar().currentMessage()


def test_ask_seq_accepts_a_sequence(win, fake):
    """x opens SGUI::ask_seq's window (test_gui_seqentry.py has the details)."""
    win.known_families = ("x",)
    win.run_actions["x"].trigger()
    assert fake.calls == [] and win.seq_dialog.isVisible()
    win.seq_dialog.combo.setEditText("1 2 3 4")
    win.seq_dialog.go_button.click()
    assert fake.calls == [("accept_sequence", ["1", "2", "3", "4"])]
    assert win.known_families == ()


def test_start_sequence_is_a_fresh_run(win, fake):
    win.known_families = ("x",)
    win.start_sequence("1 2 3 4")
    assert fake.calls == [("new_sequence", "1 2 3 4")]
    assert win.known_families == ()


def test_quit_closes_the_window_and_stops_the_runner(win, fake):
    win.run_actions["q"].trigger()
    assert not win.isVisible()
    assert ("quit",) in fake.calls


# ---- the runner's new setters ---------------------------------------------------------------
def test_runner_setters_write_the_globals():
    r = Runner()
    saved = Global.InterstepSleep, Global.debugMAX
    try:
        r.set_interstep_sleep(12)
        assert Global.InterstepSleep == 12
        r.set_debug_max(1)
        assert Global.debugMAX == 1
        r.set_debug_max(0)
        assert Global.debugMAX == 0
    finally:
        Global.InterstepSleep, Global.debugMAX = saved


def test_real_runner_step_updates_the_count(win, qtbot):
    r = Runner()
    win.attach_runner(r)
    r.start()
    try:
        r.new_sequence(SEQ, seed=1)
        with qtbot.waitSignal(r.command_done, timeout=20000,
                              check_params_cb=lambda n, _: n == "new_sequence"):
            pass
        with qtbot.waitSignal(r.command_done, timeout=20000):
            win.run_actions["h"].trigger()      # 5 steps
        qtbot.waitUntil(lambda: win.count_label.text() == "5", timeout=5000)
        assert win.status_labels["steps"].text() == "Steps: 5"
        assert win.status_labels["elements"].text() == "Elements: 6"
        qtbot.waitUntil(lambda: win.status_labels["state"].text() == "Idle", timeout=5000)
    finally:
        assert r.quit()


def test_real_runner_slider_and_pause(win, qtbot):
    r = Runner()
    win.attach_runner(r)
    r.start()
    saved = Global.InterstepSleep
    try:
        r.new_sequence(SEQ, seed=1)
        with qtbot.waitSignal(r.command_done, timeout=20000):
            pass
        win.run_actions["d"].trigger()          # crawl 30 ms per step
        qtbot.waitUntil(lambda: win.status_labels["state"].text() == "Running", timeout=5000)
        win.sleep_slider.setValue(5)            # changes the sleep of the running crawl
        assert Global.InterstepSleep == 5
        with qtbot.waitSignal(r.command_done, timeout=20000):
            win.run_actions["p"].trigger()
        qtbot.waitUntil(lambda: win.status_labels["state"].text() == "Paused", timeout=5000)
    finally:
        assert r.quit()
        Global.InterstepSleep = saved


# ---- screenshot -----------------------------------------------------------------------------
def test_controls_screenshot(win, qtbot, tmp_path, request):
    """The toolbar with the count at 17, next to Perl's button frame
    (docs/gui/perl/controls_buttons.png). ``pytest --write-screens`` saves it."""
    import dataclasses
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    assert (PERL_DIR / "controls_buttons.png").exists()
    win.show_snapshot(dataclasses.replace(_snap("six_elements"), steps=17))
    qtbot.wait(10)
    path = out_dir / "controls_buttons.png"
    assert win.toolbar.grab().save(str(path), "PNG")
    assert (SCREENS_DIR / "controls_buttons.png").exists()
