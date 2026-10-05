"""Drive the Qt GUI through its own widgets, offscreen (loop0003 item 05).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
The Qt counterpart of drive_gui.py:

  QT_QPA_PLATFORM=offscreen python3 python/tests/drive_qt_gui.py OUTDIR [SCENARIO...]

The window is made as `python3 -m metacat.qt` makes it (metacat.qt.app.setup:
every panel in its pane, the control strip, the engine thread), with trace.ss's
writer installed first and the Commentary and Trace windows wrapped by
headless.install_recorders, so that each run driven through the GUI writes a
trace, compared with its golden.  The GUI thread runs QApplication.exec(); a
driver thread presses the buttons, types into the command line and answers the
dialogs, each through the bridge's blocking call onto the GUI thread (as a
user's event would arrive), and waits for the engine thread between steps.
Each scenario prints "ok NAME ..." or raises; the last line is a JSON object
with the measurements.  The script exits 0 when every scenario passed, and by
itself in any case (a watchdog ends it after 10 minutes).
"""
from __future__ import annotations

import json
import os
import sys
import threading
import time
import traceback
from pathlib import Path

os.environ["QT_QPA_PLATFORM"] = "offscreen"
os.environ.pop("WAYLAND_DISPLAY", None)

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent))
ROOT = HERE.parent.parent
GOLDEN = ROOT / "tests" / "golden"

OUT = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("/tmp/drive-qt-gui")
OUT.mkdir(parents=True, exist_ok=True)

from PySide6.QtCore import QPoint, Qt  # noqa: E402
from PySide6.QtTest import QTest  # noqa: E402
from PySide6.QtWidgets import QApplication, QLineEdit, QPushButton  # noqa: E402

from metacat import chez, demos, engine, objects, run, setup, trace_writer, view_globals  # noqa: E402
from metacat.objects import tell  # noqa: E402

ANSWERS = []
STATUS = [1]
RESULTS = {}


def golden_end(name):
    last = json.loads((GOLDEN / name).read_text().splitlines()[-1])
    return last["t"], last["rng"]


# --- the GUI, with the trace writer -------------------------------------------------

def make_gui():
    qapp = QApplication.instance() or QApplication(["metacat-tests"])
    engine.load()
    original_halt = objects.report_error_and_halt
    trace_writer.on_answer = lambda ev: ANSWERS.append(
        tell(tell(ev, "get-answer-string"), "print-name"))
    trace_writer.on_halt = original_halt
    trace_writer.install_trace()
    from metacat import headless
    from metacat.qt import app
    from metacat.qt.mainwindow import MainWindow
    window = MainWindow()
    app.setup(window)
    headless.install_recorders()
    window.show()
    return qapp, window


try:
    QAPP, WINDOW = make_gui()
    BRIDGE = WINDOW.bridge
    CP = setup.g_control_panel
    W = tell(CP, "get-widgets")
except BaseException:   # noqa: BLE001 - the resize listener would keep the process up
    traceback.print_exc()
    sys.stdout.flush()
    os._exit(1)


def on_main(fn, *args):
    """fn(*args) on the GUI thread, waiting for its value (a user's event)"""
    return BRIDGE.invoker.call(lambda: fn(*args), timeout=60)


def wait_for(cond, what, secs=10):
    t = time.time()
    while not cond():
        time.sleep(0.02)
        if time.time() - t > secs:
            raise AssertionError(what)


def check(cond, what):
    if not cond:
        raise AssertionError(what)


def state():
    return setup.g_codelet_count, chez.random_seed()


def input_mode():
    return on_main(lambda: W["go-button"].isEnabled() and not W["stop-button"].isEnabled())


def wait_idle(secs=300):
    t = time.time()
    while True:
        time.sleep(0.02)
        if not BRIDGE.busy() and input_mode():
            return
        if time.time() - t > secs:
            raise RuntimeError("the engine did not stop")


def enter(text):
    def do():
        e = W["command-line"]
        e.setText(text)
        QTest.keyClick(e, Qt.Key_Return)
    on_main(do)


def set_line(text):
    on_main(lambda: W["command-line"].setText(text))


def click(name):
    on_main(lambda: W[name].click())


def info():
    return on_main(lambda: W["info-label"].text())


def invoke_menu(menu, label):
    def do():
        for action in menu.actions():
            if action.text() == label:
                action.trigger()
                return
        raise KeyError(label)
    on_main(do)


def dialogs(title):
    return on_main(lambda: [w for w in QApplication.topLevelWidgets()
                            if w.isVisible() and w.windowTitle() == title])


def answer_input_dialog(action_label, value):
    invoke_menu(W["options-menu"], action_label)
    wait_for(lambda: len(dialogs("Input")) == 1, "the input dialog opens")
    (dialog,) = dialogs("Input")

    def do():
        entry = dialog.findChild(QLineEdit)
        entry.setText(str(value))
        QTest.keyClick(entry, Qt.Key_Return)
    on_main(do)
    wait_for(lambda: dialogs("Input") == [], "the input dialog closes")


def clear_memory():
    """the Clear Memory dialog's Yes, with the engine parked (item 06 makes the
    dialog)"""
    import metacat
    check(not BRIDGE.busy(), "the engine is idle")
    on_main(lambda: tell(metacat.memory.g_memory, "clear"))


class Traced:
    """A GUI run written to OUT/NAME as trace.ss does, compared with its golden."""

    def __init__(self, golden, strings, seed):
        self.golden = golden
        self.strings, self.seed = strings, seed

    def __enter__(self):
        ANSWERS.clear()
        self.path = OUT / self.golden
        self.port = open(self.path, "w")
        trace_writer.PORT = self.port
        setup.g_codelet_count = 0
        trace_writer.trace_start(self.strings, self.seed, 10000, False)
        return self

    def __exit__(self, kind, value, tb):
        if kind is None:
            trace_writer.trace_end("suspend", ANSWERS)
        trace_writer.PORT = None
        self.port.close()
        if kind is None:
            got = self.path.read_text()
            want = (GOLDEN / self.golden).read_text()
            if got != want:
                g, w = got.splitlines(), want.splitlines()
                i = next((k for k, (a, b) in enumerate(zip(g, w)) if a != b), min(len(g), len(w)))
                raise AssertionError("%s differs from its golden at line %d:\n got  %s\n want %s"
                                     % (self.golden, i + 1, g[i] if i < len(g) else "<end>",
                                        w[i] if i < len(w) else "<end>"))
        return False


def responsiveness(n=10):
    """the GUI thread's round trips while the engine runs, and the panes' paints"""
    delays = []
    for _ in range(n):
        t0 = time.time()
        on_main(lambda: None)
        delays.append(time.time() - t0)
        time.sleep(0.05)
    return delays


def grab(name):
    """the whole window, as it is now, into OUT/name"""
    from metacat.qt.grab import grab_png

    def do():
        WINDOW.sync()
        QAPP.processEvents()
        grab_png(WINDOW, str(OUT / name))
    on_main(do)


def paints():
    return on_main(lambda: sum(p.view.paints for p in WINDOW.panes.values()))


# --- the scenarios -------------------------------------------------------------------

def start():
    check(on_main(WINDOW.windowTitle) == "Metacat", "the window's title")
    check(info() == "Please enter a problem:", "info label")
    check(not input_mode() and not on_main(lambda: W["go-button"].isEnabled()),
          "the buttons start disabled, as in gui.ss")
    enter("abc 12x")
    time.sleep(0.1)
    check(info() == "Invalid input!", "Invalid input! shown: %r" % info())
    time.sleep(1.0)
    check(info() == "Please enter a problem:", "the message comes back")
    on_main(lambda: W["speed-slider"].setValue(100))
    got = (view_globals.p_num_of_flashes, view_globals.p_flash_pause,
           view_globals.p_snag_pause, view_globals.p_text_scroll_pause)
    check(got == (1, 1, 1, 1), "the speed slider at Fast: %r" % (got,))
    print("ok start", flush=True)


def full_run():
    clear_memory()
    with Traced("abc-abd-ijk_1.jsonl", ["abc", "abd", "ijk"], 1):
        enter("abc abd ijk 1")
        wait_idle()
        check(info() == " abc -> abd; ijk -> ?       seed:  1 ", repr(info()))
        check(state() == (0, 1), "initialized, stopped before the first codelet: %r" % (state(),))
        check(tell(CP, "get-current-problem") == ["abc", "abd", "ijk", False, 1], "problem")
        click("go-button")
        wait_idle()
    check(state() == golden_end("abc-abd-ijk_1.jsonl"), "the golden's end: %r" % (state(),))
    check(ANSWERS == ["ijd"], ANSWERS)
    print("ok full-run", state(), flush=True)


def step_mode():
    clear_memory()
    answer_input_dialog("Step mode interval", 40)
    check(run.p_step_cycles == 40, "step interval 40")
    with Traced("abc-abd-ijk_1.jsonl", ["abc", "abd", "ijk"], 1):
        set_line("abc abd ijk 1")
        click("step-button")
        wait_idle()
        check(setup.g_codelet_count == 0 and run.g_step_mode_p is True, "step mode, at 0")
        counts = []
        for _ in range(3):
            click("step-button")
            wait_idle()
            counts.append(setup.g_codelet_count)
        check(counts == [40, 80, 120], "three steps of 40: %r" % counts)
        click("go-button")
        wait_idle()
    check(run.g_step_mode_p is False, "Go turns step mode off")
    check(state() == golden_end("abc-abd-ijk_1.jsonl"), "the golden's end: %r" % (state(),))
    answer_input_dialog("Step mode interval", 1)
    print("ok step-mode", counts, flush=True)


def demo_stop_go():
    clear_memory()
    golden = "abc-abd-xyz_3852097033.jsonl"
    with Traced(golden, ["abc", "abd", "xyz"], 3852097033):
        on_main(lambda: tell(CP, "run-demo", demos.run7))
        wait_idle()
        check(info() == " abc -> abd; xyz -> ?       seed:  3852097033 ", repr(info()))
        check(state() == (0, 3852097033), "demo initialized: %r" % (state(),))
        painted = paints()
        click("go-button")
        t = time.time()
        while setup.g_codelet_count < 300:
            time.sleep(0.01)
            check(time.time() - t < 120, "the run started")
        delays = responsiveness()
        check(run.g_running_p is True and max(delays) < 0.5,
              "responsive while running: %r" % delays)
        running_paints = paints() - painted
        check(running_paints >= 10, "the panes repaint while the engine runs: %d"
              % running_paints)
        RESULTS["responsive_max_s"] = round(max(delays), 3)
        RESULTS["paints_while_running"] = running_paints
        print("ok responsive max %.3f s, %d paints" % (max(delays), running_paints), flush=True)
        click("stop-button")
        wait_idle()
        stopped = setup.g_codelet_count
        check(0 < stopped < 2170, "stopped mid-run at %d" % stopped)
        check(info() == " abc -> abd; xyz -> ?       seed:  3852097033 ", "info kept")
        click("go-button")
        wait_idle()
    check(state() == golden_end(golden), "the golden's end: %r" % (state(),))
    check(ANSWERS == ["wyz"], ANSWERS)
    grab("window-run7.png")
    print("ok demo-stop-go stopped at", stopped, flush=True)


def breakpoint_run():
    clear_memory()
    answer_input_dialog("Set breakpoint", 100)
    check(run.g_break_time == 100, "breakpoint 100")
    check(on_main(lambda: W["breakpoint-label"].text()) == "Breakpoint set for time step 100",
          "breakpoint label")
    with Traced("abc-abd-ijk_1.jsonl", ["abc", "abd", "ijk"], 1):
        enter("abc abd ijk 1")
        wait_idle()
        click("go-button")
        wait_idle()
        check(setup.g_codelet_count == 100, "stopped at the breakpoint: %d" % setup.g_codelet_count)
        click("go-button")
        wait_idle()
    check(state() == golden_end("abc-abd-ijk_1.jsonl"), "the golden's end: %r" % (state(),))
    invoke_menu(W["options-menu"], "Clear breakpoint")
    check(run.g_break_time is False, "breakpoint cleared")
    check(on_main(lambda: W["breakpoint-label"].text()) == "", "label cleared")
    print("ok breakpoint", flush=True)


def reset():
    clear_memory()
    with Traced("abc-abd-ijk_1.jsonl", ["abc", "abd", "ijk"], 1):
        set_line("")
        click("reset-button")
        wait_idle()
        check(state() == (0, 1), "Reset re-initializes the problem: %r" % (state(),))
        click("go-button")
        wait_idle()
    check(state() == golden_end("abc-abd-ijk_1.jsonl"), "the golden's end: %r" % (state(),))
    print("ok reset", flush=True)


def justify():
    clear_memory()
    golden = "abc-abd-ijk-abd_1.jsonl"
    with Traced(golden, ["abc", "abd", "ijk", "abd"], 1):
        enter("abc abd ijk abd 1")
        wait_idle()
        check(info() == " abc -> abd; ijk -> abd       seed:  1 ", repr(info()))
        check(setup.p_justify_mode is True, "justify mode")
        click("go-button")
        wait_idle()
    check(state() == golden_end(golden), "the golden's end: %r" % (state(),))
    check(ANSWERS == ["abd"], ANSWERS)
    print("ok justify", flush=True)


def toggles():
    """50 rapid Go/Stop toggles, then the run to its end: no deadlock, the GUI
    answers throughout, and the trace is the golden's.  Each Go is pressed
    when the panel is back in input mode (Go is disabled while running), and
    Stop as soon as the run shows (0-20 ms later).  A Stop can be lost: go
    switches to run mode before it clears *interrupt?* (run.ss), so a Stop
    pressed in between is undone, as in gui.py; the run then goes on to its
    answer, and the toggling ends there (another Go would resume the run
    after its answer, which the golden does not do)."""
    clear_memory()
    golden = "abc-abd-xyz_3852097033.jsonl"
    with Traced(golden, ["abc", "abd", "xyz"], 3852097033):
        enter("abc abd xyz 3852097033")
        wait_idle()
        t0 = time.time()
        worst = 0.0
        stops = []
        for k in range(50):
            wait_idle(60)
            if ANSWERS:
                break
            t = time.time()
            click("go-button")
            wait_for(lambda: on_main(lambda: W["stop-button"].isEnabled()) or not BRIDGE.busy(),
                     "the run shows", 10)
            time.sleep(0.005 * (k % 5))
            click("stop-button")
            worst = max(worst, time.time() - t)
            stops.append(setup.g_codelet_count)
        wait_idle(60)
        toggled = len(stops)
        RESULTS["toggles"] = toggled
        RESULTS["toggles_s"] = round(time.time() - t0, 3)
        RESULTS["toggle_worst_s"] = round(worst, 3)
        RESULTS["toggles_stopped_at"] = setup.g_codelet_count
        check(toggled >= 10, "at least 10 toggles before the answer: %d" % toggled)
        if not ANSWERS:
            click("go-button")
            wait_idle()
    check(state() == golden_end(golden), "the golden's end: %r" % (state(),))
    check(ANSWERS == ["wyz"], ANSWERS)
    print("ok toggles %d, %.2f s" % (toggled, RESULTS["toggles_s"]), flush=True)


def timing():
    """run 7 from Go to its answer at the slider's fast end"""
    clear_memory()
    golden = "abc-abd-xyz_3852097033.jsonl"
    with Traced(golden, ["abc", "abd", "xyz"], 3852097033):
        enter("abc abd xyz 3852097033")
        wait_idle()
        t0 = time.time()
        click("go-button")
        wait_idle()
        RESULTS["run7_s"] = round(time.time() - t0, 2)
    check(state() == golden_end(golden), "the golden's end: %r" % (state(),))
    print("ok timing run7 %.2f s" % RESULTS["run7_s"], flush=True)


def clicks():
    """the mouse and keyboard scenario of click_scenario.py (loop0003 item 07): the
    same as drive_gui.py's clicks, through QTest's events; writes OUT/clicks.json,
    which test_qt_clicks.py compares with the tkinter GUI's"""
    import click_scenario
    result = click_scenario.run(QtClicks())
    click_scenario.write(result, OUT / "clicks.json")
    print("ok clicks", len(result["steps"]), flush=True)


class QtClicks:
    """click_scenario's adapter: QTest's mouse and key events on the widgets"""
    on_main = staticmethod(on_main)
    wait_idle = staticmethod(wait_idle)
    enter = staticmethod(enter)
    click = staticmethod(click)
    info = staticmethod(info)
    answer_input_dialog = staticmethod(answer_input_dialog)
    grab = staticmethod(grab)

    BUTTONS = {"left": (Qt.LeftButton, Qt.NoModifier),
               "shift": (Qt.LeftButton, Qt.ShiftModifier),
               "control": (Qt.LeftButton, Qt.ControlModifier),
               "right": (Qt.RightButton, Qt.NoModifier),
               "shift-right": (Qt.RightButton, Qt.ShiftModifier),
               "middle": (Qt.MiddleButton, Qt.NoModifier),
               "double": (Qt.LeftButton, Qt.NoModifier)}
    KEYS = {"return": (Qt.Key_Return, Qt.NoModifier),
            "kp-enter": (Qt.Key_Enter, Qt.KeypadModifier),
            "shift-return": (Qt.Key_Return, Qt.ShiftModifier)}

    @staticmethod
    def answers():
        return ANSWERS

    @staticmethod
    def wait_engine(secs=300):
        wait_for(lambda: not BRIDGE.busy(), "the engine stops", secs)

    @staticmethod
    def set_speed_fast():
        on_main(lambda: W["speed-slider"].setValue(100))

    @staticmethod
    def invoke_option(label):
        invoke_menu(W["options-menu"], label)

    @staticmethod
    def press_dialog(title, label):
        def do():
            (dialog,) = [w for w in QApplication.topLevelWidgets()
                         if w.isVisible() and w.windowTitle() == title]
            (b,) = [b for b in dialog.findChildren(QPushButton) if b.text() == label]
            b.click()
        on_main(do)

    @staticmethod
    def key_line(text, key):
        def do():
            e = W["command-line"]
            e.setText(text)
            QTest.keyClick(e, *QtClicks.KEYS[key])
        on_main(do)

    @staticmethod
    def visible_size(win):
        viewport = tell(win, "get-toplevel").pane.view.viewport()
        return viewport.width(), viewport.height()

    @staticmethod
    def press(win, x, y, kind):
        """the press as the platform delivers it, through the window (a double
        click: press, release, double click, release), so that Qt routes it to
        the widget under the pointer; a hidden pane gets it directly"""
        viewport = tell(win, "get-toplevel").pane.view.viewport()
        button, modifiers = QtClicks.BUTTONS[kind]

        def do():
            if viewport.isVisible():
                target = viewport.window().windowHandle()
                at = viewport.mapTo(viewport.window(), QPoint(x, y))
            else:
                target, at = viewport, QPoint(x, y)
            if kind == "double":
                QTest.mouseDClick(target, button, modifiers, at)
            else:
                QTest.mouseClick(target, button, modifiers, at)
        on_main(do)


SCENARIOS = [start, full_run, step_mode, demo_stop_go, breakpoint_run, reset, justify,
             toggles, timing]
ON_REQUEST = [clicks]


def driver():
    try:
        time.sleep(0.3)
        names = sys.argv[2:]
        for scenario in SCENARIOS + ON_REQUEST:
            if names and scenario.__name__ not in names and scenario is not start:
                continue
            if not names and scenario in ON_REQUEST:
                continue
            scenario()
        STATUS[0] = 0
    except BaseException:   # noqa: BLE001
        traceback.print_exc()
        sys.stdout.flush()
    finally:
        print(json.dumps(RESULTS), flush=True)
        BRIDGE.invoker.post(QAPP.quit)


def watchdog():
    time.sleep(600)
    print("watchdog: the driver did not finish", flush=True)
    os._exit(2)


if __name__ == "__main__":
    threading.Thread(target=watchdog, daemon=True).start()
    threading.Thread(target=driver, daemon=True).start()
    QAPP.exec()
    sys.stdout.flush()
    os._exit(STATUS[0])
