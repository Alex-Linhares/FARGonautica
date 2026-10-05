"""Drive the Qt GUI's control strip, menus and dialogs, offscreen (loop0003 item 06).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  QT_QPA_PLATFORM=offscreen python3 python/tests/drive_qt_menus.py OUTDIR

The window is made as `python3 -m metacat.qt` makes it (metacat.qt.app.setup),
on an offscreen screen of the size the tkinter inventory was taken on
(python/tests/data/tk-gui-inventory.json's "screen", so that
select-control-panel-fonts picks the same fonts).  A driver thread then does
what tk_gui_inventory.py did to the tkinter GUI, through the GUI thread:

- it records the menu bar (every item: kind, label, state, font, check state,
  demo problem) and the control panel in the inventory's five states (initial;
  input, after Enter on "abc abd xyz 7"; disabled; run; self-watching off),
  into OUTDIR/report.json, which test_qt_menus.py compares with the inventory;
- it triggers every menu item and dialog and checks that each does what
  gui.py's does (each scenario prints "ok NAME" or raises).

The script exits 0 when every scenario passed, and by itself in any case (a
watchdog ends it after 10 minutes).
"""
from __future__ import annotations

import json
import os
import sys
import threading
import time
import traceback
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent))
ROOT = HERE.parent.parent
INVENTORY = json.loads((HERE / "data" / "tk-gui-inventory.json").read_text())

OUT = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("/tmp/drive-qt-menus")
OUT.mkdir(parents=True, exist_ok=True)

# the offscreen screen at the inventory's size (Qt's offscreen plugin reads its
# screens from a JSON file)
_sw, _sh = INVENTORY["screen"]
_screens = OUT / "offscreen-screens.json"
_screens.write_text(json.dumps({"synchronousWindowSystemEvents": False, "windowFrameMargins": False,
                                "screens": [{"name": "offscreen", "x": 0, "y": 0, "width": _sw,
                                             "height": _sh, "logicalDpi": 96,
                                             "logicalBaseDpi": 96, "dpr": 1}]}))
os.environ["QT_QPA_PLATFORM"] = "offscreen:configfile=%s" % _screens
os.environ.pop("WAYLAND_DISPLAY", None)

from PySide6.QtCore import Qt  # noqa: E402
from PySide6.QtTest import QTest  # noqa: E402
from PySide6.QtWidgets import (QApplication, QLabel, QPushButton,  # noqa: E402
                               QTextEdit)

import metacat  # noqa: E402
from metacat import demos, engine, setup  # noqa: E402
from metacat.gui import gui, swl  # noqa: E402
from metacat.objects import tell  # noqa: E402

STATUS = [1]
REPORT = {}


def make_gui():
    qapp = QApplication.instance() or QApplication(["metacat-tests"])
    engine.load()
    from metacat.qt import app
    from metacat.qt.mainwindow import MainWindow
    window = MainWindow()
    app.setup(window)
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

from metacat.qt import controls  # noqa: E402


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


def settle():
    """the GUI thread's posted work done"""
    on_main(lambda: None)
    on_main(lambda: None)


def enter(text):
    def do():
        e = W["command-line"]
        e.setText(text)
        QTest.keyClick(e, Qt.Key_Return)
    on_main(do)


def info():
    return on_main(lambda: W["info-label"].text())


# --- the menu bar, read ---------------------------------------------------------------

def menu_path_action(path):
    """the QAction at "Menu > Item > ..." in the menu bar"""
    def find():
        actions = WINDOW.menuBar().actions()
        a = None
        for label in path.split(" > "):
            found = [x for x in actions if not x.isSeparator() and x.text() == label]
            if len(found) != 1:
                raise KeyError(path)
            a = found[0]
            actions = a.menu().actions() if a.menu() else []
        return a
    return on_main(find)


def trigger(path):
    a = menu_path_action(path)
    on_main(a.trigger)
    settle()


def entry(a):
    if a.isSeparator():
        return {"kind": "separator"}
    e = {"label": a.text(), "state": "normal" if a.isEnabled() else "disabled"}
    font = a.property("swl-font")
    e["font"] = list(font) if font else None
    if a.menu() is not None:
        e["kind"] = "cascade"
        e["items"] = [entry(x) for x in a.menu().actions()]
        return e
    kind = a.property("swl-kind") or "command"
    e["kind"] = kind
    if kind == "check":
        e["selected"] = a.isChecked()
    elif a.isCheckable():
        e["highlighted"] = a.isChecked()
    if a.data() is not None:
        e["problem"] = list(a.data())
    return e


def menu_tree():
    return on_main(lambda: [entry(a) for a in WINDOW.menuBar().actions()])


def menu_states():
    def walk(actions, prefix, out):
        for a in actions:
            if a.isSeparator():
                continue
            out[prefix + a.text()] = "normal" if a.isEnabled() else "disabled"
            if a.menu() is not None:
                walk(a.menu().actions(), prefix + a.text() + " > ", out)
        return out
    return on_main(lambda: walk(WINDOW.menuBar().actions(), "", {}))


def fn_name(f):
    return f"{f.__module__}.{f.__qualname__}" if f is not None else None


def control_state():
    def read():
        cl = W["command-line"]
        out = {k: "normal" if W[k].isEnabled() else "disabled" for k in
               ("command-line", "step-button", "go-button", "stop-button", "reset-button",
                "speed-slider")}
        font, fg, bg, justify = CP.command_line_look
        out["command-line-text"] = cl.text()
        out["command-line-justify"] = justify
        out["command-line-foreground"] = controls.css_color(fg)
        out["command-line-background"] = controls.css_color(bg)
        out["command-line-font"] = " ".join(str(x) for x in swl.tcl_word(font))
        out["enter-action"] = fn_name(CP.command_line_action)
        out["info-label"] = W["info-label"].text()
        out["breakpoint-label"] = W["breakpoint-label"].text()
        out["self-watching-warning-visible"] = W["self-watching-warning-label"].isVisibleTo(
            W["frame"])
        return out
    out = on_main(read)
    out["menus"] = menu_states()
    return out


def visible_panes():
    return on_main(lambda: sorted(n for n, p in WINDOW.panes.items() if p.isVisibleTo(WINDOW)))


def dialogs(title):
    return on_main(lambda: [w for w in QApplication.topLevelWidgets()
                            if w.isVisible() and w.windowTitle() == title])


def press(dialog, label):
    def do():
        (b,) = [b for b in dialog.findChildren(QPushButton) if b.text() == label]
        b.click()
    on_main(do)
    settle()


def labels(dialog):
    return on_main(lambda: [lb.text() for lb in dialog.findChildren(QLabel)])


def grab(widget, name):
    from metacat.qt.grab import grab_png
    on_main(lambda: grab_png(widget, str(OUT / (name + ".png"))))


def grab_menus():
    """each menu, popped up, into OUT/menu-NAME.png"""
    from PySide6.QtCore import QPoint
    for name in ("help-menu", "demos-menu", "view-menu", "options-menu", "memory-menu",
                 "clamp-codelets-menu", "comment-font-face-menu", "comment-font-size-menu"):
        m = W[name]
        on_main(lambda: m.popup(QPoint(100, 100)))
        grab(m, "menu-" + name)
        on_main(m.hide)


# --- the scenarios -------------------------------------------------------------------

def inventory():
    """the menu bar and the five states, as tk_gui_inventory.py recorded them"""
    settle()
    states = {"initial": control_state()}
    REPORT["menus"] = menu_tree()
    from metacat.gui import fonts as gfonts
    REPORT["faces"] = {"serif": gfonts.serif, "sans-serif": gfonts.sans_serif,
                       "fancy": gfonts.fancy}
    REPORT["menubar-font"] = on_main(lambda: WINDOW.menuBar().property("swl-font"))
    enter("abc abd xyz 7")
    wait_idle()
    states["input"] = control_state()
    tell(CP, "switch-to-disabled-mode")
    settle()
    states["disabled"] = control_state()
    tell(CP, "switch-to-input-mode")
    tell(CP, "switch-to-run-mode")
    settle()
    states["run"] = control_state()
    tell(CP, "switch-to-input-mode")
    settle()
    trigger("Options > Self-watching mode")
    states["self-watching-off"] = control_state()
    states["self-watching-off"]["window-width"] = on_main(WINDOW.width)
    states["self-watching-off"]["hidden_windows"] = [
        n for n in WINDOW.panes if n not in visible_panes() and n != "EEG"]
    trigger("Options > Self-watching mode")
    REPORT["states"] = states
    REPORT["visible-after-self-watching-on"] = visible_panes()
    print("ok inventory", flush=True)


def demos_menu():
    """every demo item runs init-new-problem with its problem and is highlighted
    until the next problem; the engine is not run here (thread-break recorded)"""
    thunks = []
    real = gui.thread_break
    gui.thread_break = lambda thread, ignore, thunk: thunks.append(thunk)
    try:
        results = {}

        def walk(prefix, items):
            for e in items:
                if e["kind"] == "cascade":
                    walk(prefix + e["label"] + " > ", e["items"])
                elif "problem" in e:
                    path = prefix + e["label"]
                    trigger(path)
                    a = menu_path_action(path)
                    others = [p for p, h in highlighted().items() if h and p != path]
                    results[path] = {"problem": list(tell(CP, "get-current-problem")),
                                     "info": info(), "highlighted": on_main(a.isChecked),
                                     "others": others}
                    check(on_main(a.isChecked), "%s highlighted" % path)
                    check(others == [], "only %s highlighted: %r" % (path, others))
        (demos_entry,) = [e for e in menu_tree() if e["label"] == "Demos"]
        walk("Demos > ", demos_entry["items"])
        REPORT["demos"] = results
        check(len(thunks) == len(results), "one engine thunk per demo")
        # a problem typed in unhighlights the demo
        enter("abc abd xyz 7")
        settle()
        check(not any(highlighted().values()), "Enter unhighlights the demos")
    finally:
        gui.thread_break = real
    # one demo for real: Run 7 parks at its start, ready to go
    trigger("Demos > Run 7:  abc -> abd; xyz -> ?")
    wait_idle()
    a, b, c, seed = demos.run7
    check(tell(CP, "get-current-problem") == [a, b, c, False, seed], "run 7 is the current problem")
    check(setup.g_codelet_count == 0, "parked at the start")
    print("ok demos", len(results), flush=True)


def highlighted():
    def walk(prefix, actions, out):
        for a in actions:
            if a.menu() is not None:
                walk(prefix + a.text() + " > ", a.menu().actions(), out)
            elif a.isCheckable() and a.property("swl-kind") != "check":
                out[prefix + a.text()] = a.isChecked()
        return out
    demos_action = menu_path_action("Demos")
    return on_main(lambda: walk("Demos > ", demos_action.menu().actions(), {}))


def view_menu():
    names = {"Workspace": "workspace", "Slipnet": "slipnet", "Coderack": "coderack",
             "Temperature": "temperature", "Temporal Trace": "trace", "Commentary": "commentary",
             "Episodic Memory": "memory", "Top Themes": "top-themes",
             "Bottom Themes": "bottom-themes", "Vertical Themes": "vertical-themes", "EEG": "EEG"}
    start = visible_panes()
    check("EEG" not in start and len(start) == 10, "the EEG starts hidden: %r" % start)
    for label, name in names.items():
        a = menu_path_action("View > " + label)
        was = name in visible_panes()
        check(on_main(a.isChecked) == was, "%s's check follows its pane" % label)
        trigger("View > " + label)
        check((name in visible_panes()) == (not was), "%s toggled" % label)
        check(on_main(a.isChecked) == (not was), "%s's check toggled" % label)
        ctl = [c for c in W["window-controllers"] if tell(c, "get-text") == label][0]
        check(tell(ctl, "visible?") == (not was), "%s's controller" % label)
        trigger("View > " + label)
        check((name in visible_panes()) == was, "%s back" % label)
    # both Themes panes hidden: their splitter goes too, and comes back
    trigger("View > Top Themes")
    trigger("View > Bottom Themes")
    check(not on_main(lambda: WINDOW.splitters["themes"].isVisibleTo(WINDOW)),
          "an empty splitter is hidden")
    trigger("View > Top Themes")
    check(on_main(lambda: WINDOW.splitters["themes"].isVisibleTo(WINDOW)),
          "the splitter comes back with a pane")
    trigger("View > Bottom Themes")
    trigger("View > Hide all panes")
    check(visible_panes() == [], "Hide all panes")
    trigger("View > Show all panes")
    check(visible_panes() == sorted(names.values()), "Show all panes: %r" % visible_panes())
    # drag a handle, then Reset layout: the default panes and sizes come back
    on_main(lambda: WINDOW.splitters["top"].moveSplitter(400, 2))
    on_main(lambda: WINDOW._handle_dragged(400, 2))
    trigger("View > Reset layout")
    check(visible_panes() == start, "Reset layout shows the default panes")
    check(on_main(lambda: WINDOW.default_layout), "Reset layout follows the window again")
    got = on_main(WINDOW.splitter_sizes)
    from metacat.qt.mainwindow import default_sizes
    c = on_main(lambda: (WINDOW.centralWidget().width(), WINDOW.centralWidget().height()))
    want = default_sizes(*c)
    for name in ("rows", "top", "middle"):
        check(got[name][:len(want[name])] == want[name] or abs(
            sum(a - b for a, b in zip(got[name], want[name]))) <= 4,
            "Reset layout's %s sizes %r, want %r" % (name, got[name], want[name]))
    print("ok view", flush=True)


def options_menu():
    # check items: each flips its switch, as gui.py's
    for label, get in (("Eliza mode", lambda: setup.p_eliza_mode),
                       ("Slipnet graphics", lambda: setup.p_slipnet_graphics),
                       ("Coderack graphics", lambda: setup.p_coderack_graphics),
                       ("Show codelet counts", lambda: setup.p_codelet_count_graphics),
                       ("Show last codelet type", lambda: setup.p_highlight_last_codelet),
                       ("Verbose mode", lambda: tell(CP, "verbose-mode?"))):
        a = menu_path_action("Options > " + label)
        before = bool(get())
        check(on_main(a.isChecked) == before, "%s starts checked as its switch" % label)
        trigger("Options > " + label)
        check(bool(get()) == (not before), "%s toggled" % label)
        check(on_main(a.isChecked) == (not before), "%s's check" % label)
        trigger("Options > " + label)
        check(bool(get()) == before, "%s back" % label)
    # Slipnet graphics off blanks the Slipnet: few items left
    slip = setup.g_slipnet_window
    dl = tell(slip, "get-toplevel").canvas.display_list

    def items():
        return len(str(dl.tcl("find", "all")).split())
    full = items()
    trigger("Options > Slipnet graphics")
    blank = items()
    trigger("Options > Slipnet graphics")
    check(blank < full and items() == full,
          "Slipnet graphics off blanks it, on restores it (%r, %r, %r)" % (full, blank, items()))
    # self-watching off: the warning, the Themes panes hidden, the clamp items disabled
    trigger("Options > Self-watching mode")
    check(setup.p_self_watching_enabled is False, "self-watching off")
    check(on_main(lambda: W["self-watching-warning-label"].isVisibleTo(W["frame"])), "warning")
    hidden = [n for n in ("top-themes", "bottom-themes", "vertical-themes")
              if n not in visible_panes()]
    check(len(hidden) == 3, "the Themes panes hidden: %r" % hidden)
    for path in ("Options > Clamp theme pattern", "Options > Clamp codelet pattern",
                 "Options > Undo last clamp"):
        check(not on_main(menu_path_action(path).isEnabled), path + " disabled")
    trigger("Options > Self-watching mode")
    check(setup.p_self_watching_enabled is True, "self-watching on")
    check(not on_main(lambda: W["self-watching-warning-label"].isVisibleTo(W["frame"])),
          "no warning")
    check(all(n in visible_panes() for n in ("top-themes", "bottom-themes", "vertical-themes")),
          "the Themes panes back")
    # the codelet pattern clamps (Run 7 is the current problem, parked)
    trace = metacat.trace.g_trace
    for label, kind in (("Top-down", "top-down"), ("Bottom-up", "bottom-up"), ("Rule", "rule"),
                        ("Bridge", "bridge"), ("Group", "group")):
        trigger("Options > Clamp codelet pattern > %s codelet pattern" % label)
        ev = tell(trace, "get-last-event", "clamp")
        check(ev is not False and tell(ev, "get-clamp-type") == "manual-clamp", label)
        check(tell(ev, "get-clamped-codelet-patterns") == [gui.clamp_codelets_pattern(kind)],
              "%s's pattern" % label)
    trigger("Options > Undo last clamp")
    check(tell(trace, "within-clamp-period?") is False, "Undo last clamp")
    # the commentary fonts
    from metacat.gui import commentary_graphics as cg
    trigger("Options > Commentary font face > serif bold")
    got = tell(cg.p_comment_window_font, "get-swl-font")
    from metacat.gui import fonts as gfonts
    check((got.face, got.size, list(got.style)) == (gfonts.serif, 12, ["bold"]),
          "serif bold: %r" % (got,))
    sizes = on_main(lambda: {a.text(): list(a.property("swl-font")) for a in
                             W["comment-font-size-menu"].actions()})
    check(all(f[0] == gfonts.serif and f[2:] == ["bold"] for f in sizes.values()),
          "the size menu in the new face: %r" % sizes)
    trigger("Options > Commentary font size > large")
    got = tell(cg.p_comment_window_font, "get-swl-font")
    check((got.face, got.size, list(got.style)) == (gfonts.serif, 18, ["bold"]),
          "large: %r" % (got,))
    faces = on_main(lambda: {a.text(): (a.isChecked(), list(a.property("swl-font"))[1])
                             for a in W["comment-font-face-menu"].actions()
                             if not a.isSeparator()})
    check([t for t, (h, _) in faces.items() if h] == ["serif bold"], "face highlight %r" % faces)
    check(all(s == 18 for _, s in faces.values()), "the face menu at size 18")
    hl = on_main(lambda: [a.text() for a in W["comment-font-size-menu"].actions()
                          if a.isChecked()])
    check(hl == ["large"], "size highlight %r" % hl)
    REPORT["comment-font"] = [got.face, got.size, list(got.style)]
    trigger("Options > Commentary font face > sans-serif bold italic")
    trigger("Options > Commentary font size > medium")
    print("ok options", flush=True)


def help_dialog():
    trigger("Help > Metacat help")
    wait_for(lambda: len(dialogs("Help")) == 1, "the Help window opens")
    (h,) = dialogs("Help")
    grab(h, "help-window")
    text = on_main(lambda: h.findChild(QTextEdit).toPlainText())
    want = (ROOT / "chez_scheme" / "original" / "help.txt").read_text(encoding="latin-1")
    check(text == want.rstrip("\n") or text == want, "help.txt shown")
    check(on_main(lambda: h.findChild(QTextEdit).isReadOnly()), "read-only")
    trigger("Help > Metacat help")
    check(dialogs("Help") == [h], "Help again raises the one window")
    # Tk's line count (index end-1c) counts the empty line after the last newline,
    # as the document's block count does
    REPORT["help"] = {"first-line": text.splitlines()[0],
                      "lines": on_main(lambda: h.findChild(QTextEdit).document().blockCount()),
                      "font": on_main(lambda: list(h.property("swl-font"))),
                      "wrap": on_main(lambda: h.findChild(QTextEdit).lineWrapMode()
                                      == QTextEdit.WidgetWidth)}
    on_main(h.close)
    settle()
    check(dialogs("Help") == [], "Help closed")
    trigger("Help > Metacat help")
    check(len(dialogs("Help")) == 1, "Help opens again after a close")
    on_main(dialogs("Help")[0].close)
    print("ok help", flush=True)


def clear_memory_dialog():
    # a run with an answer, so that there is something to clear
    enter("abc abd ijk 1")
    wait_idle()
    on_main(lambda: W["speed-slider"].setValue(100))
    on_main(W["go-button"].click)
    wait_for(lambda: BRIDGE.busy() or not input_mode(), "the run starts", 30)
    wait_idle()
    mem = metacat.memory.g_memory
    check(tell(mem, "get-all-descriptions") != [], "an answer in the Memory")
    trigger("Memory > Clear Memory")
    (d,) = dialogs("Confirm")
    grab(d, "clear-memory-dialog")
    REPORT["clear-memory"] = {"labels": labels(d), "disabled-state": control_state()}
    check(not input_mode(), "the panel is disabled while the dialog is up")
    trigger("Memory > Clear Memory")
    check(dialogs("Confirm") == [d], "Clear Memory again raises the one dialog")
    press(d, "Cancel")
    check(dialogs("Confirm") == [] and input_mode(), "Cancel: closed, input mode")
    check(tell(mem, "get-all-descriptions") != [], "Cancel keeps the answers")
    trigger("Memory > Clear Memory")
    (d,) = dialogs("Confirm")
    on_main(d.close)     # the window manager's close: as Cancel
    settle()
    check(dialogs("Confirm") == [] and input_mode(), "close: input mode")
    check(tell(mem, "get-all-descriptions") != [], "close keeps the answers")
    trigger("Memory > Clear Memory")
    (d,) = dialogs("Confirm")
    press(d, "Yes")
    check(dialogs("Confirm") == [] and input_mode(), "Yes: closed, input mode")
    check(tell(mem, "get-all-descriptions") == [], "Yes empties the Memory")
    print("ok clear-memory", flush=True)


def theme_edit_dialog():
    from metacat.gui import theme_graphics
    themes = metacat.themes
    trace = metacat.trace.g_trace
    themespace = themes.g_themespace
    before = tell(themespace, "get-partial-state", "top-bridge")
    trigger("Options > Clamp theme pattern")
    (d,) = dialogs("Confirm")
    grab(d, "theme-edit-dialog")
    REPORT["theme-edit"] = {"labels": labels(d),
                            "background": on_main(lambda: d.property("swl-background"))}
    check(theme_graphics.g_theme_edit_mode_p is True, "theme edit mode on")
    check(not input_mode(), "disabled during theme editing")
    check(tell(CP, "ready-to-edit?", "top-bridge") is True, "top-bridge ready")
    check(tell(CP, "ready-to-edit?", "bottom-bridge") is False, "no bottom themes here")
    trigger("Options > Clamp theme pattern")
    check(dialogs("Confirm") == [d], "the one dialog raised")
    tell(CP, "raise-theme-edit-dialog")
    check(tell(themespace, "get-partial-state", "top-bridge")[2] == [],
          "the themes deleted for editing")
    # Cancel: the themes as they were, no clamp
    last = tell(trace, "get-last-event", "clamp")
    tell(CP, "edit-theme-type", "top-bridge")
    check(tell(CP, "ready-to-edit?", "top-bridge") is False, "edited once")
    press(d, "Cancel")
    check(theme_graphics.g_theme_edit_mode_p is False and input_mode(), "Cancel: mode off")
    after = tell(themespace, "get-partial-state", "top-bridge")

    def norm(state):    # restore-state recreates the themes, in its own order
        return [state[0], sorted(map(str, state[1])), sorted(map(str, state[2])),
                sorted(map(str, state[3]))]
    check(norm(after) == norm(before), "Cancel restores:\n%r\n%r" % (before, after))
    check(tell(trace, "get-last-event", "clamp") is last, "Cancel clamps nothing")
    # Clamp Themes: a manual clamp of the edited theme type's pattern
    trigger("Options > Clamp theme pattern")
    (d,) = dialogs("Confirm")
    tell(CP, "edit-theme-type", "top-bridge")
    pattern_themes = [t for t in tell(themespace, "get-themes", "top-bridge")]
    check(pattern_themes, "the Top Themes exist")
    tell(pattern_themes[0], "set-activation", 100)
    press(d, "Clamp Themes")
    check(theme_graphics.g_theme_edit_mode_p is False and input_mode(), "Clamp: mode off")
    ev = tell(trace, "get-last-event", "clamp")
    check(ev is not last and tell(ev, "get-clamp-type") == "manual-clamp", "a manual clamp")
    check(len(tell(ev, "get-clamped-theme-patterns")) == 1, "one theme pattern clamped")
    tell(trace, "undo-last-clamp")
    # with no problem: the error
    saved = CP.problem
    CP.problem = False
    trigger("Options > Clamp theme pattern")
    check(info() == "No current problem!" and dialogs("Confirm") == [], "no problem")
    trigger("Options > Clamp codelet pattern > Rule codelet pattern")
    check(info() == "No current problem!", "no problem for codelet clamps")
    CP.problem = saved
    wait_for(lambda: info() != "No current problem!", "the message comes back", 3)
    print("ok theme-edit", flush=True)


def save_commentary():
    path = OUT / "commentary.txt"
    if path.exists():
        path.unlink()
    controls.set_file_dialog(lambda title, mode, d: (REPORT.setdefault("save-title", title),
                                                      str(path))[1])
    trigger("Options > Save commentary to file")
    text = path.read_text()
    lines = tell(setup.g_comment_window, "get-lines")
    want = "".join((str(ln) + "\n") if isinstance(ln, str) else "\n" * ln for ln in lines)
    check(text == want and len(text) > 100, "the commentary saved: %r" % text[:200])
    controls.set_file_dialog(lambda title, mode, d: False)
    trigger("Options > Save commentary to file")    # cancelled: nothing written
    check(path.read_text() == text, "a cancelled dialog writes nothing")
    print("ok save-commentary", len(text), flush=True)


def menus_in_run_mode():
    """during a real run: only Stop, the slider, Help and View answer"""
    enter("abc abd xyz 3852097033")
    wait_idle()
    on_main(lambda: W["speed-slider"].setValue(50))
    on_main(W["go-button"].click)
    wait_for(lambda: not input_mode(), "the run starts", 30)
    REPORT["running"] = control_state()
    on_main(W["stop-button"].click)
    wait_idle()
    REPORT["stopped"] = control_state()
    on_main(WINDOW.sync)
    grab(WINDOW, "window")
    grab_menus()
    print("ok run-mode", flush=True)


SCENARIOS = [inventory, demos_menu, view_menu, options_menu, help_dialog, clear_memory_dialog,
             theme_edit_dialog, save_commentary, menus_in_run_mode]


def driver():
    try:
        time.sleep(0.3)
        for scenario in SCENARIOS:
            scenario()
        STATUS[0] = 0
    except BaseException:   # noqa: BLE001
        traceback.print_exc()
        sys.stdout.flush()
    finally:
        (OUT / "report.json").write_text(json.dumps(REPORT, indent=1, default=str) + "\n")
        print("done", flush=True)
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
