"""Drive Metacat's GUI through its own widgets, under Xvfb (loop0002 item 15).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
The counterpart of racket/gui-tests/control-panel-test.rkt.

  env -u WAYLAND_DISPLAY xvfb-run -a -s "-screen 0 2560x1600x24" python3 python/tests/drive_gui.py OUTDIR

Never on the owner's screen.  The GUI is made as `python3 -m metacat.gui` makes it
(metacat.gui.app.setup), with trace.ss's writer installed first (as
headless.prepare does) and the Commentary and Trace windows wrapped by
headless.install_recorders, so that each run driven through the GUI writes a
trace.  Tk's main thread runs mainloop; a driver thread presses the buttons,
types into the command line, invokes menu entries and clicks on canvases (each
a Tk call that tkinter hands to the main thread, as a user's event would be),
and waits for the engine thread between steps.  Each scenario prints
"ok NAME ..." or raises; the script exits 0 when every scenario passed, and by
itself in any case (a watchdog ends it after 10 minutes).
"""
from __future__ import annotations

import ctypes
import ctypes.util
import io
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
GOLDEN = ROOT / "tests" / "golden"

OUT = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("/tmp/drive-gui")
OUT.mkdir(parents=True, exist_ok=True)

from metacat import chez, engine, objects, run, setup, trace_writer, view_globals  # noqa: E402
from metacat.objects import tell  # noqa: E402

ANSWERS = []
STATUS = [1]


def golden_end(name):
    import json
    last = json.loads((GOLDEN / name).read_text().splitlines()[-1])
    return last["t"], last["rng"]


# --- the GUI, with the trace writer -------------------------------------------------

def make_gui():
    engine.load()
    original_halt = objects.report_error_and_halt
    trace_writer.on_answer = lambda ev: ANSWERS.append(
        tell(tell(ev, "get-answer-string"), "print-name"))
    trace_writer.on_halt = original_halt
    trace_writer.install_trace()
    from metacat.gui import app
    from metacat import headless
    app.setup()
    headless.install_recorders()
    return app


app = make_gui()
from metacat.gui import gui  # noqa: E402
root = app.g_root
CP = setup.g_control_panel
W = tell(CP, "get-widgets")


def on_main(fn, *args):
    """Run fn in Tk's main thread and wait for its value (a user's event)."""
    box = []
    done = threading.Event()

    def call():
        try:
            box.append(("ok", fn(*args)))
        except BaseException as e:   # noqa: BLE001
            box.append(("error", e))
        done.set()
    root.after(0, call)
    if not done.wait(60):
        raise RuntimeError("the main thread did not answer")
    kind, value = box[0]
    if kind == "error":
        raise value
    return value


def wait_for(cond, what, secs=10):
    t = time.time()
    while not cond():
        time.sleep(0.05)
        if time.time() - t > secs:
            raise AssertionError(what)


def check(cond, what):
    if not cond:
        raise AssertionError(what)


def state():
    return setup.g_codelet_count, chez.random_seed()


def input_mode():
    return on_main(lambda: str(W["go-button"].cget("state")) == "normal"
                   and str(W["stop-button"].cget("state")) == "disabled")


def wait_idle(secs=300):
    t = time.time()
    while True:
        time.sleep(0.05)
        if not app.engine_busy_p() and input_mode():
            return
        if time.time() - t > secs:
            raise RuntimeError("the engine did not stop")


def enter(text):
    def do():
        e = W["command-line"]
        e.delete(0, "end")
        e.insert(0, text)
        # Tk sends a key to the window with the focus, as the user's typing
        e.focus_force()
        e.update()
        e.event_generate("<Return>")
    on_main(do)


def set_line(text):
    def do():
        W["command-line"].delete(0, "end")
        W["command-line"].insert(0, text)
    on_main(do)


def click(name):
    on_main(lambda: W[name].invoke())


def info():
    return on_main(lambda: W["info-label"].cget("text"))


def menu_index(menu, label):
    def find():
        last = menu.index("end")
        for i in range(0 if last is None else last + 1):
            if menu.type(i) not in ("separator", "tearoff") and menu.entrycget(i, "label") == label:
                return i
        raise KeyError(label)
    return on_main(find)


def invoke_menu(menu, label):
    i = menu_index(menu, label)
    on_main(lambda: menu.invoke(i))


def toplevel_named(title):
    def find():
        return [w for w in root.winfo_children()
                if w.winfo_class() == "Toplevel" and w.title() == title]
    return on_main(find)


def answer_input_dialog(action_label, value):
    invoke_menu(W["options-menu"], action_label)
    time.sleep(0.2)
    (dialog,) = toplevel_named("Input")

    def do():
        entry = [w for w in dialog.winfo_children() if w.winfo_class() == "Entry"][0]
        entry.delete(0, "end")
        entry.insert(0, str(value))
        entry.focus_force()
        entry.update()
        entry.event_generate("<Return>")
    on_main(do)
    wait_for(lambda: toplevel_named("Input") == [], "the input dialog closes")


def press_dialog_button(title, label):
    (dialog,) = toplevel_named(title)

    def do():
        stack = [dialog]
        while stack:
            w = stack.pop()
            if w.winfo_class() == "Button" and w.cget("text") == label:
                w.invoke()
                return
            stack.extend(w.winfo_children())
        raise KeyError(label)
    on_main(do)


def clear_memory():
    on_main(lambda: W["main-menu"].invoke(W["main-menu"].index("Clear Memory")))
    time.sleep(0.2)
    check(not input_mode(), "the panel is disabled while the Clear Memory dialog is up")
    press_dialog_button("Confirm", "Yes")
    time.sleep(0.2)
    check(input_mode(), "back to input mode after the dialog")
    check(tell(_m().memory.g_memory, "get-all-descriptions") == [], "the Memory is empty")


def _m():
    import metacat
    return metacat


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
        # the start line's "t" is the codelet count, which init-mcat sets to 0 in
        # a fresh process; here the last run left its own (the engine is idle)
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


# --- the scenarios -------------------------------------------------------------------

def windows():
    check(on_main(lambda: W["frame"].title()) == "Metacat Control Panel", "panel title")
    check(info() == "Please enter a problem:", "info label")
    check(not input_mode() and on_main(lambda: str(W["go-button"].cget("state"))) == "disabled",
          "the buttons start disabled, as in gui.ss")
    ctls = W["window-controllers"]
    check(len(ctls) == 12, "12 window controllers")
    vis = [tell(c, "visible?") for c in ctls]
    check(vis == [True] * 10 + [False, False], "EEG and Logo start hidden: %r" % vis)
    states = on_main(lambda: [tell(c, "get-toplevel").top.state()
                              if hasattr(tell(c, "get-toplevel"), "top") else None
                              for c in ctls[:11]])
    check(states == ["normal"] * 10 + ["withdrawn"], "the toplevels' states: %r" % states)
    titles = on_main(lambda: [tell(c, "get-toplevel").top.title() for c in ctls[:11]])
    check(titles[0] == "Workspace" and titles[1] == "Slipnet", titles)
    # no window overlaps the control panel
    cp = on_main(lambda: (W["frame"].winfo_rootx(), W["frame"].winfo_rooty(),
                          W["frame"].winfo_width(), W["frame"].winfo_height()))
    for c in ctls[:10]:
        top = tell(c, "get-toplevel").top
        x, y = on_main(lambda: (top.winfo_rootx(), top.winfo_rooty()))
        check(x >= cp[0] + cp[2] or y >= cp[1] + cp[3],
              "%s clear of the control panel" % on_main(top.title))
    print("ok windows", cp, flush=True)


def invalid_input():
    enter("abc 12x")
    time.sleep(0.1)
    check(info() == "Invalid input!", "Invalid input! shown: %r" % info())
    time.sleep(1.0)
    check(info() == "Please enter a problem:", "the message comes back")
    # the speed slider at Fast
    on_main(lambda: W["speed-slider"].set(100))
    time.sleep(0.3)
    on_main(lambda: root.update_idletasks())
    got = (view_globals.p_num_of_flashes, view_globals.p_flash_pause,
           view_globals.p_snag_pause, view_globals.p_text_scroll_pause)
    check(got == (1, 1, 1, 1), "the speed slider at Fast: %r" % (got,))
    print("ok invalid-input", flush=True)


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
    print("ok step-mode", counts, flush=True)


def demo_stop_go():
    clear_memory()
    golden = "abc-abd-xyz_3852097033.jsonl"
    with Traced(golden, ["abc", "abd", "xyz"], 3852097033):
        invoke_menu(W["demos-menu"], "Run 7:  abc -> abd; xyz -> ?")
        wait_idle()
        check(info() == " abc -> abd; xyz -> ?       seed:  3852097033 ", repr(info()))
        check(state() == (0, 3852097033), "demo initialized: %r" % (state(),))
        i = menu_index(W["demos-menu"], "Run 7:  abc -> abd; xyz -> ?")
        check(on_main(lambda: W["demos-menu"].entrycget(i, "background")) != "", "highlighted")
        click("go-button")
        t = time.time()
        while setup.g_codelet_count < 300:
            time.sleep(0.01)
            check(time.time() - t < 120, "the run started")
        # the GUI stays responsive while the engine runs
        delays = []
        for _ in range(10):
            t0 = time.time()
            on_main(lambda: None)
            delays.append(time.time() - t0)
            time.sleep(0.05)
        check(run.g_running_p is True and max(delays) < 0.5,
              "responsive while running: %r" % delays)
        print("ok responsive max %.3f s" % max(delays), flush=True)
        click("stop-button")
        wait_idle()
        stopped = setup.g_codelet_count
        check(0 < stopped < 2170, "stopped mid-run at %d" % stopped)
        check(info() == " abc -> abd; xyz -> ?       seed:  3852097033 ", "info kept")
        click("go-button")
        wait_idle()
    check(state() == golden_end(golden), "the golden's end: %r" % (state(),))
    check(ANSWERS == ["wyz"], ANSWERS)
    grab_png("screen-run7.png")
    print("ok demo-stop-go stopped at", stopped, flush=True)


def breakpoint_click():
    clear_memory()
    answer_input_dialog("Set breakpoint", 100)
    check(run.g_break_time == 100, "breakpoint 100")
    check(on_main(lambda: W["breakpoint-label"].cget("text")) == "Breakpoint set for time step 100",
          "breakpoint label")
    with Traced("abc-abd-ijk_1.jsonl", ["abc", "abd", "ijk"], 1):
        enter("abc abd ijk 1")
        wait_idle()
        click("go-button")
        wait_idle()
        check(setup.g_codelet_count == 100, "stopped at the breakpoint: %d" % setup.g_codelet_count)
        # a click on the Workspace resumes the run (workspace-window-press-handler)
        canvas = tell(setup.g_workspace_window, "get-vp").canvas.widget
        on_main(lambda: canvas.event_generate("<ButtonPress-1>", x=40, y=40))
        time.sleep(0.3)
        wait_idle()
    check(state() == golden_end("abc-abd-ijk_1.jsonl"), "the golden's end: %r" % (state(),))
    invoke_menu(W["options-menu"], "Clear breakpoint")
    check(run.g_break_time is False, "breakpoint cleared")
    check(on_main(lambda: W["breakpoint-label"].cget("text")) == "", "label cleared")
    print("ok breakpoint-click", flush=True)


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


def menus():
    ctls = W["window-controllers"]
    ws = ctls[0]
    wmenu = W["windows-menu"]
    invoke_menu(wmenu, "Hide Workspace")
    time.sleep(0.1)
    check(on_main(lambda: tell(ws, "get-toplevel").top.state()) == "withdrawn", "hidden")
    invoke_menu(wmenu, "Show Workspace")
    time.sleep(0.1)
    check(on_main(lambda: tell(ws, "get-toplevel").top.state()) == "normal", "shown")
    invoke_menu(wmenu, "Show EEG")
    time.sleep(0.1)
    check(tell(ctls[10], "visible?") is True, "EEG shown")
    # Options: Eliza mode and Verbose mode toggle their switches
    eliza = setup.p_eliza_mode
    invoke_menu(W["options-menu"], "Eliza mode")
    check(setup.p_eliza_mode is (not eliza), "Eliza mode toggled")
    invoke_menu(W["options-menu"], "Eliza mode")
    check(setup.p_eliza_mode is eliza, "Eliza mode back")
    # self-watching off: the warning, the theme windows hidden, the clamp items disabled
    invoke_menu(W["options-menu"], "Self-watching mode")
    time.sleep(0.1)
    check(setup.p_self_watching_enabled is False, "self-watching off")
    check(on_main(lambda: W["self-watching-warning-label"].winfo_ismapped()) == 1, "warning")
    check([tell(c, "visible?") for c in ctls[7:10]] == [False] * 3, "theme windows hidden")
    invoke_menu(W["options-menu"], "Self-watching mode")
    time.sleep(0.1)
    check(setup.p_self_watching_enabled is True, "self-watching on")
    check(on_main(lambda: W["self-watching-warning-label"].winfo_ismapped()) == 0, "no warning")
    # the theme edit dialog
    invoke_menu(W["options-menu"], "Clamp theme pattern")
    time.sleep(0.2)
    from metacat.gui import theme_graphics
    check(theme_graphics.g_theme_edit_mode_p is True, "theme edit mode on")
    check(not input_mode(), "disabled during theme editing")
    press_dialog_button("Confirm", "Cancel")
    time.sleep(0.2)
    check(theme_graphics.g_theme_edit_mode_p is False and input_mode(), "theme edit mode off")
    # a codelet pattern clamp
    invoke_menu(W["options-menu"], "Clamp codelet pattern")   # a cascade: nothing happens
    sub = W["clamp-codelets-menu"]
    invoke_menu(sub, "Bottom-up codelet pattern")
    trace = _m().trace.g_trace
    ev = tell(trace, "get-last-event", "clamp")
    check(ev is not False and tell(ev, "get-clamp-type") == "manual-clamp", "a manual clamp")
    invoke_menu(W["options-menu"], "Undo last clamp")
    # Help
    on_main(lambda: W["main-menu"].invoke(W["main-menu"].index("Help")))
    time.sleep(0.2)
    (helpwin,) = toplevel_named("Help")
    first = (ROOT / "chez_scheme" / "original" / "help.txt").read_text().splitlines()[0]
    text = on_main(lambda: [w for w in _descendants(helpwin) if w.winfo_class() == "Text"][0]
                   .get("1.0", "1.end"))
    check(text == first, "help.txt shown: %r" % text)
    # the commentary font
    sizes = W["comment-font-size-menu"]
    invoke_menu(sizes, "large")
    from metacat.gui import commentary_graphics as cg
    got = tell(cg.p_comment_window_font, "get-swl-font")
    check((got.size, got.style) == (18, ["bold", "italic"]), "the large commentary font: %r" % got)
    invoke_menu(sizes, "medium")
    print("ok menus", flush=True)


def _descendants(w):
    out = [w]
    for c in w.winfo_children():
        out.extend(_descendants(c))
    return out


def save_commentary():
    path = OUT / "commentary.txt"
    gui.set_file_dialog(lambda title, mode, d: str(path))
    invoke_menu(W["options-menu"], "Save commentary to file")
    text = path.read_text()
    lines = tell(setup.g_comment_window, "get-lines")
    want = "".join((str(l) + "\n") if isinstance(l, str) else "\n" * l for l in lines)
    check(text == want and len(text) > 100, "the commentary saved: %r" % text[:200])
    print("ok save-commentary", len(text), flush=True)


def resize():
    top = tell(setup.g_workspace_window, "get-toplevel").top
    before = tell(setup.g_workspace_window, "get-size")
    on_main(lambda: top.geometry("1000x750"))
    t = time.time()
    while tell(setup.g_workspace_window, "get-size") == before:
        time.sleep(0.1)
        check(time.time() - t < 10, "the Workspace window was resized")
    time.sleep(1.0)
    after = tell(setup.g_workspace_window, "get-size")
    check(after == [1000, 750], "the new visible size: %r" % after)
    print("ok resize", before, after, flush=True)


def grab_png(name):
    on_main(lambda: root.update_idletasks())
    time.sleep(1.0)
    w, h = on_main(lambda: (root.winfo_screenwidth(), root.winfo_screenheight()))
    pixels = grab_screen(w, h)
    import render_sgl_fixture as r
    write_rgb_png(OUT / name, w, h, pixels, r)
    return w, h


def screenshot():
    # the Help window closed, the Workspace back at its size
    for helpwin in toplevel_named("Help"):
        on_main(lambda: helpwin.destroy())
    top = tell(setup.g_workspace_window, "get-toplevel").top
    on_main(lambda: top.geometry("800x600"))
    wait_for(lambda: tell(setup.g_workspace_window, "get-size") == [800, 600], "size back")
    w, h = grab_png("screen.png")
    print("ok screenshot", w, h, flush=True)


class XImage(ctypes.Structure):
    _fields_ = [("width", ctypes.c_int), ("height", ctypes.c_int), ("xoffset", ctypes.c_int),
                ("format", ctypes.c_int), ("data", ctypes.c_void_p),
                ("byte_order", ctypes.c_int), ("bitmap_unit", ctypes.c_int),
                ("bitmap_bit_order", ctypes.c_int), ("bitmap_pad", ctypes.c_int),
                ("depth", ctypes.c_int), ("bytes_per_line", ctypes.c_int),
                ("bits_per_pixel", ctypes.c_int)]


def grab_screen(w, h):
    """The whole screen as RGB bytes: XGetImage on the root window (32 bits per pixel,
    little-endian BGRX, as Xvfb's 24-bit TrueColor screen gives)."""
    x11 = ctypes.CDLL(ctypes.util.find_library("X11"))
    x11.XOpenDisplay.restype = ctypes.c_void_p
    x11.XOpenDisplay.argtypes = [ctypes.c_char_p]
    x11.XDefaultRootWindow.restype = ctypes.c_ulong
    x11.XDefaultRootWindow.argtypes = [ctypes.c_void_p]
    x11.XGetImage.restype = ctypes.POINTER(XImage)
    x11.XGetImage.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ctypes.c_int, ctypes.c_int,
                              ctypes.c_uint, ctypes.c_uint, ctypes.c_ulong, ctypes.c_int]
    x11.XCloseDisplay.argtypes = [ctypes.c_void_p]
    display = x11.XOpenDisplay(None)
    image = x11.XGetImage(display, x11.XDefaultRootWindow(display), 0, 0, w, h,
                          (1 << 64) - 1, 2).contents
    check(image.bits_per_pixel == 32 and image.byte_order == 0, "BGRX image")
    raw = ctypes.string_at(image.data, image.bytes_per_line * h)
    x11.XCloseDisplay(display)
    out = bytearray()
    for y in range(h):
        row = raw[y * image.bytes_per_line: y * image.bytes_per_line + 4 * w]
        rgb = bytearray(3 * w)
        rgb[0::3], rgb[1::3], rgb[2::3] = row[2::4], row[1::4], row[0::4]
        out += rgb
    return bytes(out)


def write_rgb_png(path, w, h, rgb, r):
    import struct
    import zlib
    raw = bytearray()
    for y in range(h):
        raw.append(0)
        raw += rgb[3 * w * y: 3 * w * (y + 1)]

    def chunk(kind, data):
        return (struct.pack(">I", len(data)) + kind + data
                + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF))
    path.write_bytes(b"\x89PNG\r\n\x1a\n"
                     + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
                     + chunk(b"IDAT", zlib.compress(bytes(raw), 6))
                     + chunk(b"IEND", b""))


def clicks():
    """the mouse and keyboard scenario of click_scenario.py (loop0003 item 07), for
    the Qt GUI's comparison: writes OUT/clicks.json.  Only when named."""
    import click_scenario
    result = click_scenario.run(TkClicks())
    click_scenario.write(result, OUT / "clicks.json")
    print("ok clicks", len(result["steps"]), flush=True)


class TkClicks:
    """click_scenario's adapter: Tk's own events on the widgets"""
    on_main = staticmethod(on_main)
    wait_idle = staticmethod(wait_idle)
    enter = staticmethod(enter)
    click = staticmethod(click)
    answer_input_dialog = staticmethod(answer_input_dialog)
    grab = staticmethod(grab_png)
    press_dialog = staticmethod(press_dialog_button)

    # the presses a user makes, as Tk's event patterns (hosts.TkHost binds
    # <ButtonPress-1>, <Shift-ButtonPress-1> and <ButtonPress-3>)
    PATTERNS = {"left": ["<ButtonPress-1>", "<ButtonRelease-1>"],
                "shift": ["<Shift-ButtonPress-1>", "<Shift-ButtonRelease-1>"],
                "control": ["<Control-ButtonPress-1>", "<Control-ButtonRelease-1>"],
                "right": ["<ButtonPress-3>", "<ButtonRelease-3>"],
                "shift-right": ["<Shift-ButtonPress-3>", "<Shift-ButtonRelease-3>"],
                "middle": ["<ButtonPress-2>", "<ButtonRelease-2>"],
                "double": ["<ButtonPress-1>", "<ButtonRelease-1>",
                           "<ButtonPress-1>", "<ButtonRelease-1>"]}

    @staticmethod
    def answers():
        return ANSWERS

    info = staticmethod(info)

    @staticmethod
    def key_line(text, key):
        """text on the command line, then a key, as the user types them"""
        keysym = {"return": "<Return>", "kp-enter": "<KP_Enter>",
                  "shift-return": "<Shift-Return>"}[key]

        def do():
            e = W["command-line"]
            e.delete(0, "end")
            e.insert(0, text)
            e.focus_force()
            e.update()
            e.event_generate(keysym)
        on_main(do)

    @staticmethod
    def wait_engine(secs=300):
        wait_for(lambda: not app.engine_busy_p(), "the engine stops", secs)

    @staticmethod
    def set_speed_fast():
        on_main(lambda: W["speed-slider"].set(100))
        time.sleep(0.3)
        on_main(lambda: root.update_idletasks())

    @staticmethod
    def invoke_option(label):
        invoke_menu(W["options-menu"], label)
        time.sleep(0.2)

    @staticmethod
    def visible_size(win):
        widget = tell(win, "get-toplevel").widget
        return widget.winfo_width(), widget.winfo_height()

    @staticmethod
    def press(win, x, y, kind):
        widget = tell(win, "get-toplevel").widget

        def do():
            for pattern in TkClicks.PATTERNS[kind]:
                widget.event_generate(pattern, x=x, y=y)
        on_main(do)


def timing():
    """run 7 from Go to its answer (the speed slider as invalid_input leaves it:
    Fast), for the Qt GUI's comparison (drive_qt_gui.py's timing; loop0003
    item 05).  Only when named on the command line."""
    clear_memory()
    golden = "abc-abd-xyz_3852097033.jsonl"
    with Traced(golden, ["abc", "abd", "xyz"], 3852097033):
        enter("abc abd xyz 3852097033")
        wait_idle()
        t0 = time.time()
        click("go-button")
        wait_idle()
        elapsed = time.time() - t0
    check(state() == golden_end(golden), "the golden's end: %r" % (state(),))
    print("ok timing run7 %.2f s" % elapsed, flush=True)


SCENARIOS = [windows, invalid_input, full_run, step_mode, demo_stop_go, breakpoint_click,
             reset, menus, save_commentary, resize, screenshot]
ON_REQUEST = [timing, clicks]


def driver():
    try:
        time.sleep(0.5)
        for scenario in SCENARIOS + ON_REQUEST:
            if len(sys.argv) > 2 and scenario.__name__ not in sys.argv[2:]:
                continue
            if len(sys.argv) <= 2 and scenario in ON_REQUEST:
                continue
            scenario()
        STATUS[0] = 0
    except BaseException:   # noqa: BLE001
        traceback.print_exc()
        sys.stdout.flush()
    finally:
        root.after(0, root.quit)


def watchdog():
    time.sleep(600)
    print("watchdog: the driver did not finish", flush=True)
    os._exit(2)


if __name__ == "__main__":
    threading.Thread(target=watchdog, daemon=True).start()
    threading.Thread(target=driver, daemon=True).start()
    out = io.StringIO()
    root.mainloop()
    sys.stdout.flush()
    os._exit(STATUS[0])
