"""The mouse and keyboard scenario, the same for the tkinter and Qt GUIs (loop0003 item 07).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

drive_gui.py (tkinter, under xvfb-run) and drive_qt_gui.py (Qt, offscreen) both
run `run(adapter)` as their `clicks` scenario.  The adapter is the toolkit's
half: it types and presses Enter, presses buttons, answers dialogs, and sends
real mouse events to a window's canvas (Tk's event_generate, QTest's mouse
clicks).  Every binding of the inventory (tk-gui-inventory.json) is used:

- Enter on the command line and in the breakpoint dialog;
- a left click on the Workspace: nothing while... at a breakpoint it resumes
  the run, in display mode it restores the current state, in theme edit mode
  it raises the dialog; right, shift and middle clicks there do nothing;
- left clicks on the Temporal Trace and the Episodic Memory select an event or
  an answer (two answers: the comparison), a second click unselects it; a
  double click is two presses, a control click is a left click;
- left, right and shift clicks on the Top, Vertical and Bottom Themes in theme
  edit mode: the first click edits the window's theme type, then left selects
  +100, right and shift select -100, a second click unselects; Clamp Themes
  clamps the pattern and the run goes on with it;
- clicks on the panes without press handlers (Slipnet, Coderack, Temperature,
  Commentary, EEG) change nothing.

Clicks aim at model objects (an event, an answer, a theme): the adapter's
window may be any size, so the pixel is found by the model's own hit test on
the visible pixels, as mouse_press converts them.  The result is a list of
(step, model snapshot) and the trace lines that trace.ss writes during the
scenario; the pixels are kept apart, since they depend on the window sizes.
The two toolkits' results must be equal (test_qt_clicks.py).
"""
from __future__ import annotations

import io
import json
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
GOLDEN = ROOT / "tests" / "golden"


def _m():
    import metacat
    return metacat


def tell(*args):
    from metacat.objects import tell as t
    return t(*args)


def window(name):
    from metacat import setup
    return {"workspace": setup.g_workspace_window,
            "slipnet": setup.g_slipnet_window,
            "coderack": setup.g_coderack_window,
            "temperature": setup.g_temperature_window,
            "trace": setup.g_trace_window,
            "memory": setup.g_memory_window,
            "commentary": setup.g_comment_window,
            "eeg": setup.g_EEG_window,
            "top": setup.g_top_themes_window,
            "vertical": setup.g_vertical_themes_window,
            "bottom": setup.g_bottom_themes_window}[name]


# --- the model's state ----------------------------------------------------------------

def named(x):
    """x with its model objects as trace.ss names them (lists kept)"""
    from metacat import trace_writer
    if isinstance(x, (list, tuple)):
        return [named(y) for y in x]
    n = trace_writer.name(x)
    if n is trace_writer.NULL:
        return None
    return n if isinstance(n, (int, float)) else str(n)


def snapshot():
    """the model state that the clicks can change (call with the engine idle)"""
    from metacat import chez, run, setup
    from metacat.gui import theme_graphics
    m = _m()
    events = tell(m.trace.g_trace, "get-all-events")
    answers = tell(m.memory.g_memory, "get-all-descriptions")
    themespace = m.themes.g_themespace

    def themes(kind):
        rows = ([named(tell(t, "get-dimension")), named(tell(t, "get-relation")),
                 tell(t, "get-activation")]
                      for t in tell(themespace, "get-themes", kind))
        return sorted(rows, key=json.dumps)
    clamp = tell(m.trace.g_trace, "get-last-event", "clamp")
    return {
        "t": setup.g_codelet_count,
        "rng": chez.random_seed(),
        "running": run.g_running_p is not False,
        "display_mode": run.g_display_mode_p is not False,
        "interrupt": run.g_interrupt_p is not False,
        "theme_edit_mode": theme_graphics.g_theme_edit_mode_p is not False,
        "events": len(events),
        "highlighted_events": [i for i, e in enumerate(events)
                               if tell(e, "highlighted?") is not False],
        "answers": len(answers),
        "highlighted_answers": [i for i, a in enumerate(answers)
                                if tell(a, "highlighted?") is not False],
        "themes": {k: themes(k) for k in ("top-bridge", "vertical-bridge", "bottom-bridge")},
        "clamps": tell(m.trace.g_trace, "get-num-of-events", "clamp"),
        "last_clamp": (False if clamp is False else
                       [str(tell(clamp, "get-clamp-type")),
                        named(tell(clamp, "get-clamped-theme-patterns"))]),
        "break_time": run.g_break_time,
    }


# --- aiming --------------------------------------------------------------------------

def pixel_map(a, name, target_at, stride=1, box=None):
    """{id(target): (target, [(i, j), ...])} for the visible pixels of window
    `name` (those in box = (i1, j1, i2, j2) if given), each converted as
    Viewport.mouse_press converts it, where target_at(x, y) answers a target
    (False for none)"""
    from metacat.gui import sgl

    def scan():
        win = window(name)
        vp = tell(win, "get-vp")
        w, h = a.visible_size(win)
        i1, j1, i2, j2 = (1, 1, w - 2, h - 2) if box is None else box
        i1, j1, i2, j2 = max(i1, 1), max(j1, 1), min(i2, w - 2), min(j2, h - 2)
        ox, oy = sgl.my_screen_to_canvas_x(vp, 1), sgl.my_screen_to_canvas_y(vp, 1)
        ys = [(j, vp.pixel_to_y(j + oy)) for j in range(j1, j2 + 1, stride)]
        out = {}
        for i in range(i1, i2 + 1, stride):
            x = vp.pixel_to_x(i + ox)
            for j, y in ys:
                t = target_at(x, y)
                if t is not False and t is not None:
                    out.setdefault(id(t), (t, []))[1].append((i, j))
        return out
    return a.on_main(scan)


def aim(a, name, target, target_at):
    """a pixel in the middle of the target's visible pixels (found on a coarse
    grid, then pixel by pixel around it)"""
    coarse = pixel_map(a, name, target_at, stride=3).get(id(target))
    if not coarse:
        raise AssertionError("no visible pixel of the target in %s" % name)
    xs = [p[0] for p in coarse[1]]
    ys = [p[1] for p in coarse[1]]
    box = (min(xs) - 3, min(ys) - 3, max(xs) + 3, max(ys) + 3)
    pixels = pixel_map(a, name, target_at, box=box)[id(target)][1]
    xs = sorted(p[0] for p in pixels)
    ys = sorted(p[1] for p in pixels)
    mid = (xs[len(xs) // 2], ys[len(ys) // 2])
    if mid in pixels:
        return mid
    return min(pixels, key=lambda p: (p[0] - mid[0]) ** 2 + (p[1] - mid[1]) ** 2)


def centre(a, name):
    w, h = a.on_main(lambda: a.visible_size(window(name)))
    return w // 2, h // 2


class Recorder:
    """trace.ss's lines while the scenario runs (trace_writer.PORT)"""

    def __enter__(self):
        from metacat import trace_writer
        self.port = io.StringIO()
        trace_writer.PORT = self.port
        return self

    def __exit__(self, *exc):
        from metacat import trace_writer
        trace_writer.PORT = None
        return False

    def lines(self):
        return self.port.getvalue().splitlines()


# --- the scenario --------------------------------------------------------------------

def run(a):
    """the scenario through adapter a; returns {"steps", "pixels", "traces"}"""
    from metacat import run as run_mod, setup, trace_writer
    m = _m()
    steps, pixels, traces = [], {}, {}

    def settle():
        """the engine idle, and the panel in input mode unless a dialog
        disabled it (theme edit mode)"""
        from metacat.gui import theme_graphics
        a.wait_engine()
        if theme_graphics.g_theme_edit_mode_p is False:
            a.wait_idle()

    def snap(step):
        settle()
        steps.append([step, a.on_main(snapshot)])

    def press(name, kind, at, pause=0.2):
        pixels.setdefault(name, []).append([kind, list(at)])
        a.press(window(name), at[0], at[1], kind)
        time.sleep(pause)
        settle()

    a.set_speed_fast()
    a.on_main(lambda: tell(m.memory.g_memory, "clear"))

    # 1. a run to its breakpoint (Enter in the dialog and on the command line);
    #    the clicks that do nothing; the left click that resumes it
    golden = (GOLDEN / "abc-abd-ijk_1.jsonl").read_text().splitlines()
    with Recorder() as rec:
        a.answers().clear()
        setup.g_codelet_count = 0
        trace_writer.trace_start(["abc", "abd", "ijk"], 1, 10000, False)
        a.answer_input_dialog("Set breakpoint", 100)
        # the command line's <Key-Return>: the keypad's Enter is another key
        a.key_line("abc abd ijk 1", "kp-enter")
        time.sleep(0.2)
        steps.append(["keypad Enter on the command line", a.info()])
        a.enter("abc abd ijk 1")
        a.wait_idle()
        steps.append(["Return on the command line", a.info()])
        a.click("go-button")
        snap("at the breakpoint")
        for kind in ("right", "shift", "middle", "shift-right"):
            press("workspace", kind, (40, 40))
        snap("workspace right, shift, middle and shift-right clicks at the breakpoint")
        a.invoke_option("Clear breakpoint")
        press("workspace", "left", (40, 40), pause=0.4)
        snap("workspace left click resumes")
        trace_writer.trace_end("suspend", a.answers())
    traces["run1"] = rec.lines()
    if rec.lines() != golden:
        raise AssertionError("the resumed run differs from its golden")

    # 2. a second answer for the Memory
    with Recorder() as rec:
        a.key_line("abc abd ijk 2", "shift-return")   # <Key-Return> ignores Shift
        a.wait_idle()
        steps.append(["shift-Return on the command line", a.info()])
        a.click("go-button")
        snap("second run")
    traces["run2"] = rec.lines()

    # 3. the Trace and the Memory
    with Recorder() as rec:
        trace = m.trace.g_trace

        def event_at(x, y):
            return tell(trace, "get-mouse-selected-event", x, y)
        events = a.on_main(lambda: tell(trace, "get-all-events"))
        seen = pixel_map(a, "trace", event_at, stride=3)
        visible = sorted(k for k, e in enumerate(events) if id(e) in seen)
        steps.append(["visible trace events", visible])
        target = events[visible[-1]]
        at = aim(a, "trace", target, event_at)
        press("trace", "left", at)
        snap("trace left click selects an event")
        a.grab("clicks-trace-event.png")
        for kind in ("right", "shift"):
            press("trace", kind, at)
        snap("trace right and shift clicks")
        press("trace", "left", at)
        snap("trace left click again unselects it")
        at2 = aim(a, "trace", events[visible[0]], event_at)
        press("trace", "control", at2)
        snap("trace control click selects another event")
        press("workspace", "left", (40, 40))
        snap("workspace left click restores the current state")

        memory = m.memory.g_memory

        def answer_at_xy(x, y):
            return tell(memory, "get-mouse-selected-answer", x, y)
        answers = a.on_main(lambda: tell(memory, "get-all-descriptions"))
        steps.append(["answers", a.on_main(
            lambda: [str(tell(x, "get-answer-print-name")) for x in answers])])

        def answer_at(k):
            return aim(a, "memory", answers[k], answer_at_xy)
        press("memory", "left", answer_at(0), pause=0.4)
        snap("memory left click selects an answer")
        press("memory", "left", answer_at(1), pause=0.4)
        snap("memory left click on a second answer compares them")
        a.grab("clicks-memory-compare.png")
        press("memory", "double", answer_at(0), pause=0.4)
        snap("memory double click: two presses")
        for kind in ("right", "shift"):
            press("memory", kind, answer_at(1))
        snap("memory right and shift clicks")
        press("workspace", "left", (40, 40))
        snap("workspace left click, not in display mode, continues the run")
        for name in ("slipnet", "coderack", "temperature", "commentary", "eeg"):
            for kind in ("left", "shift", "right"):
                press(name, kind, centre(a, name))
        snap("clicks on the panes without press handlers")
    traces["trace-and-memory"] = rec.lines()

    # 4. theme edit mode: the clamp scenario
    themespace = m.themes.g_themespace
    with Recorder() as rec:
        a.invoke_option("Clamp theme pattern")
        snap("theme edit mode")
        press("workspace", "left", (40, 40))
        press("trace", "left", (5, 5))
        press("memory", "left", (5, 5))
        snap("workspace, trace and memory clicks raise the dialog")
        press("top", "left", centre(a, "top"))
        press("vertical", "right", centre(a, "vertical"))
        press("bottom", "left", centre(a, "bottom"))
        snap("first clicks edit the theme types (not the bottom one outside justify mode)")

        def theme_at(name, kind_, k):
            from metacat.utilities import select_meth
            th = a.on_main(lambda: tell(themespace, "get-themes", kind_))[k]
            return aim(a, name, th, lambda x, y: select_meth(
                tell(themespace, "get-themes", kind_), "clicked?", x, y))
        top0 = theme_at("top", "top-bridge", 0)
        press("top", "left", top0)
        press("top", "right", theme_at("top", "top-bridge", 1))
        press("top", "shift", theme_at("top", "top-bridge", 2))
        press("vertical", "left", theme_at("vertical", "vertical-bridge", 0))
        press("vertical", "shift", theme_at("vertical", "vertical-bridge", 1))
        snap("theme left, right and shift clicks")
        a.grab("clicks-theme-edit.png")
        press("top", "left", top0)
        press("top", "left", top0)
        press("vertical", "right", theme_at("vertical", "vertical-bridge", 1))
        snap("second clicks unselect, a third selects again")
        a.press_dialog("Confirm", "Clamp Themes")
        snap("Clamp Themes")
        t = a.on_main(lambda: setup.g_codelet_count)
        a.answer_input_dialog("Set breakpoint", t + 150)
        a.click("go-button")
        snap("the run goes on with the clamp")
        a.invoke_option("Clear breakpoint")
        a.on_main(lambda: tell(m.trace.g_trace, "undo-last-clamp")
                  if run_mod.g_running_p is False else None)
    traces["theme-clamp"] = rec.lines()
    return {"steps": steps, "pixels": pixels, "traces": traces}


def write(result, path):
    Path(path).write_text(json.dumps(result, indent=1) + "\n")


def comparable(result):
    """the part both toolkits must give equally (the pixels depend on the sizes),
    each trace as its line count, its SHA-256 and its last line"""
    import hashlib

    def digest(lines):
        text = "".join(line + "\n" for line in lines)
        return {"lines": len(lines), "sha256": hashlib.sha256(text.encode()).hexdigest(),
                "last": lines[-1] if lines else None}
    return {"steps": result["steps"],
            "traces": {k: digest(v) for k, v in result["traces"].items()}}
