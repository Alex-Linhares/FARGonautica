"""Item 14: the windows of the graphics files (constants.ss's graphics part,
general-graphics.ss's windows, workspace-graphics.ss) and the views.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Structural checks (every top-level define of the .ss files has its Python
name; docstrings name their origin; no tkinter at import; the engine never
imports metacat.gui) and the behaviour of make-graphics-window against a
recording canvas: coordinate transforms, caching and flush, erase, clear,
flash, text metrics from the offscreen metric, and the host's title.
"""
from __future__ import annotations

import importlib
import inspect
import re
import subprocess
import sys
from fractions import Fraction
from pathlib import Path

import pytest

from metacat import chez as C
from metacat.names import scheme_to_python
from metacat.objects import tell

HERE = Path(__file__).resolve().parent
PY = HERE.parent
ROOT = PY.parent
ORIGINAL = ROOT / "chez_scheme" / "original"

MY_MODULES = ["metacat.gui.constants", "metacat.gui.general_graphics",
              "metacat.gui.workspace_graphics", "metacat.gui.views"]


def top_level_defines(text):
    return re.findall(r"^\(define (\S+)", text, re.M)


def constants_graphics_defines():
    text = (ORIGINAL / "constants.ss").read_text()
    start = text.index(";; Default window sizes")
    end = text.index(";; Probability distributions")
    names = top_level_defines(text[start:end])
    # swl-color and *color-names* are gui/colors.py's
    return [n for n in names if n not in ("swl-color", "*color-names*")]


# ---------------------------------------------------------------------------
# Structure

def test_constants_defines_present():
    K = importlib.import_module("metacat.gui.constants")
    missing = [n for n in constants_graphics_defines() if not hasattr(K, scheme_to_python(n))]
    assert missing == []


def test_general_graphics_defines_present():
    gg = importlib.import_module("metacat.gui.general_graphics")
    engine_gg = importlib.import_module("metacat.general_graphics")
    names = top_level_defines((ORIGINAL / "general-graphics.ss").read_text())
    missing = [n for n in names
               if not hasattr(gg, scheme_to_python(n))
               and not hasattr(engine_gg, scheme_to_python(n))]
    assert missing == []


def test_workspace_graphics_defines_present():
    wg = importlib.import_module("metacat.gui.workspace_graphics")
    names = top_level_defines((ORIGINAL / "workspace-graphics.ss").read_text())
    missing = [n for n in names if not hasattr(wg, scheme_to_python(n))]
    assert missing == []


@pytest.mark.parametrize("modname", MY_MODULES)
def test_docstrings_name_their_origin(modname):
    mod = importlib.import_module(modname)
    assert "GNU General Public License" in mod.__doc__
    assert "Translated to Python (2026)" in mod.__doc__
    prefixes = ("constants.ss: ", "general-graphics.ss: ", "workspace-graphics.ss: ",
                "racket/gui/views.rkt: ", "port:")
    bad = []
    for name, obj in vars(mod).items():
        if name.startswith("_"):
            continue
        if (inspect.isfunction(obj) or inspect.isclass(obj)) and obj.__module__ == modname:
            doc = obj.__doc__ or ""
            if not doc.startswith(prefixes):
                bad.append(name)
    assert bad == []


def test_no_tkinter_at_import():
    code = ("import sys\n"
            "import " + ", ".join(MY_MODULES) + "\n"
            "print('tkinter' in sys.modules)\n")
    out = subprocess.run([sys.executable, "-c", code], cwd=PY, capture_output=True,
                         text=True, check=True).stdout.strip()
    assert out == "False"


def test_engine_never_imports_gui():
    offenders = []
    for path in sorted((PY / "metacat").glob("*.py")):
        text = path.read_text()
        if re.search(r"^\s*(from metacat\.gui|from metacat import gui|import metacat\.gui"
                     r"|import tkinter|from tkinter)", text, re.M):
            offenders.append(path.name)
    assert offenders == []


# ---------------------------------------------------------------------------
# make-graphics-window on a recording canvas

class RecordingCanvas:
    def __init__(self, background):
        self.background = background
        self.commands = []
        self.items = 0

    def tcl(self, *args):
        self.commands.append(args)
        if args and args[0] == "create":
            self.items += 1
            return C.String(str(self.items))
        return C.String("")

    def get_background_color(self):
        return self.background

    def set_background_color_bang(self, color):
        self.background = color


@pytest.fixture
def gui():
    from metacat import engine, setup, view_globals
    from metacat.gui import constants, fonts, general_graphics, hosts, sgl
    engine.load()
    if not fonts.g_hidden_canvas:
        hosts.install_offscreen_fonts()
    sgl.load()
    constants.load()
    general_graphics.load()

    class RecordingHost(hosts.OffscreenHost):
        def __init__(self, scrolling, destroy_action):
            super().__init__(scrolling, destroy_action)
            self.scroll_regions = []

        def make_canvas(self, visible_w, visible_h, bg_color):
            self.width, self.height = visible_w, visible_h
            self.canvas = RecordingCanvas(bg_color)
            return self.canvas

        def set_scroll_region_bang(self, x1, y1, x2, y2):
            self.scroll_regions.append((x1, y1, x2, y2))

    saved = {n: getattr(view_globals, n) for n in
             ("g_fg_color", "p_flash_pause", "p_num_of_flashes")}
    saved_ws = (setup.g_workspace_window, setup.p_workspace_graphics)
    hosts.set_window_host_maker(RecordingHost)
    try:
        yield general_graphics
    finally:
        hosts.set_window_host_maker(hosts.OffscreenHost)
        for n, v in saved.items():
            setattr(view_globals, n, v)
        setup.g_workspace_window, setup.p_workspace_graphics = saved_ws


def canvas_of(window):
    return tell(window, "get-vp").canvas


def test_transforms(gui):
    from metacat.gui import constants as K
    w = gui.make_horizontal_scrollable_graphics_window(200, 100, 400)
    vp = tell(w, "get-vp")
    assert tell(w, "object-type") == "graphics-window"
    assert tell(w, "get-x-max") == 4
    assert tell(w, "get-y-max") == 1
    assert tell(w, "get-width-per-pixel") == Fraction(1, 100)
    assert tell(w, "get-height-per-pixel") == Fraction(1, 100)
    assert vp.pixel_to_x(50) == Fraction(1, 2)
    assert vp.pixel_to_y(10) == Fraction(9, 10)
    assert vp.x_to_pixel(Fraction(1, 2), 0) == 50
    assert vp.y_to_pixel(Fraction(9, 10), 0) == 10
    assert vp.x_to_pixel(Fraction(1, 4), Fraction(1, 4)) == 50
    assert vp.x_to_pixel(0.333, 0) == 33 and isinstance(vp.x_to_pixel(0.333, 0), int)
    assert tell(w, "get-visible-x-max") == 2
    assert tell(w, "get-visible-y-min") == 0
    assert tell(w, "get-center-coord") == [2, Fraction(1, 2)]
    assert tell(w, "get-size") == [200, 100]
    host = tell(w, "get-toplevel")
    assert host.scroll_regions == [(0, 0, 400, 100)]
    assert host.scrolling == "horizontal"
    assert canvas_of(w).background == K.c_white
    u = gui.make_unscrollable_graphics_window(400, 300, K.c_red)
    assert tell(u, "get-y-max") == Fraction(3, 4)
    assert tell(u, "get-toplevel").scrolling == "none"
    assert canvas_of(u).background == K.c_red
    v = gui.make_vertical_scrollable_graphics_window(100, 50, 400)
    assert tell(v, "get-y-max") == 4
    assert tell(v, "get-toplevel").scrolling == "vertical"
    s = gui.make_scrollable_graphics_window(100, 50)
    assert tell(s, "get-toplevel").scrolling == "both"


def test_caching_draw_flush(gui):
    from metacat import view_globals
    from metacat.gui import constants as K
    view_globals.g_fg_color = False
    w = gui.make_unscrollable_graphics_window(100, 100)
    canvas = canvas_of(w)
    p1 = ["line", [0, 0], [1, 1]]
    p2 = ["rectangle", [0, 0], [Fraction(1, 2), Fraction(1, 2)]]
    p3 = ["line", [0, 1], [1, 0]]
    assert tell(w, "cache-mode?") is False
    assert tell(w, "caching-on") == "done"
    assert tell(w, "cache-mode?") is True
    tell(w, "draw", p1)
    tell(w, "draw", p2)
    tell(w, "erase", p3)
    assert canvas.commands == []
    assert tell(w, "get-cached-pexp") == ["let-sgl", [], p1, p2, ["erase", K.c_white, p3]]
    assert tell(w, "flush") == "done"
    creates = [c for c in canvas.commands if c[0] == "create"]
    assert [c[1] for c in creates] == ["line", "rectangle", "line"]
    assert creates[2][c_index(creates[2], "-fill")] == K.c_white
    assert tell(w, "cache-mode?") is False
    assert tell(w, "get-cached-pexp") == ["let-sgl", []]
    # with *fg-color*, draw wraps the pexp
    view_globals.g_fg_color = K.c_red
    tell(w, "caching-on")
    tell(w, "draw", p1)
    assert tell(w, "get-cached-pexp") == ["let-sgl", [], ["let-sgl", [["foreground-color", K.c_red]], p1]]
    tell(w, "clear-pending-flush")
    assert tell(w, "cache-mode?") is False
    assert tell(w, "get-cached-pexp") == ["let-sgl", []]


def c_index(cmd, opt):
    return list(cmd).index(opt) + 1


def test_draw_with_tag_and_erase_direct(gui):
    from metacat import view_globals
    from metacat.gui import constants as K
    view_globals.g_fg_color = K.c_black
    w = gui.make_unscrollable_graphics_window(100, 100)
    canvas = canvas_of(w)
    tell(w, "draw", ["line", [0, 0], [1, 1]], "flash")
    cmd = canvas.commands[-1]
    assert cmd[c_index(cmd, "-tags")] == "flash"
    assert cmd[c_index(cmd, "-fill")] == K.c_black
    tell(w, "erase-on-background", K.c_green, ["line", [0, 0], [1, 1]])
    cmd = canvas.commands[-1]
    assert cmd[c_index(cmd, "-fill")] == K.c_green
    tell(w, "delete", "flash")
    assert canvas.commands[-1] == ("delete", "flash")
    tell(w, "move-pixels", 3, 4, "all")
    assert canvas.commands[-1] == ("move", "all", 3, 4)


def test_clear(gui):
    from metacat.gui import constants as K
    w = gui.make_unscrollable_graphics_window(100, 100, K.c_pink)
    canvas = canvas_of(w)
    tell(w, "caching-on")
    tell(w, "clear")
    assert tell(w, "get-cached-pexp") == ["let-sgl", [], ["clear", K.c_pink]]
    tell(w, "clear-pending-flush")
    tell(w, "set-background-color", K.c_blue)
    tell(w, "clear")
    assert canvas.commands == [("delete", "all")]
    assert canvas.background == K.c_blue


def test_flash(gui, monkeypatch):
    from metacat import utilities, view_globals
    view_globals.p_flash_pause = 0
    view_globals.p_num_of_flashes = 3
    w = gui.make_unscrollable_graphics_window(100, 100)
    canvas = canvas_of(w)
    assert tell(w, "flash", ["line", [0, 0], [1, 1]]) == "done"
    assert canvas.commands == []
    pauses = []
    monkeypatch.setattr(utilities, "pause", lambda ms: pauses.append(ms))
    view_globals.p_flash_pause = 5
    view_globals.p_num_of_flashes = 2
    tell(w, "flash", ["line", [0, 0], [1, 1]])
    shape = [(c[0], c[c_index(c, "-tags")]) if c[0] == "create" else c for c in canvas.commands]
    assert shape == [("create", "background"), ("create", "flash"), ("delete", "flash"),
                     ("create", "flash"), ("delete", "flash"), ("delete", "background")]
    assert pauses == [5, 5, 5]


def test_text_metrics(gui):
    from metacat.gui import fonts, hosts
    w = gui.make_unscrollable_graphics_window(200, 100)
    font = fonts.make_mfont(fonts.serif, -20, ["bold", "italic"])
    swl_font = tell(font, "get-swl-font")
    bb = hosts.offscreen_metric("x", swl_font)
    cw, ch = bb[2] - bb[0], bb[3] - bb[1]
    wpp = tell(w, "get-width-per-pixel")
    hpp = tell(w, "get-height-per-pixel")
    assert tell(w, "get-character-width", C.String("x"), font) == cw * wpp
    assert tell(w, "get-character-height", C.String("x"), font) == ch * hpp
    assert tell(w, "get-string-height", font) == ch * hpp
    assert tell(w, "get-string-width", C.String("abc"), font) == 3 * cw * wpp
    baseline = tell(font, "get-baseline-offset")
    assert tell(w, "get-text-offset", font) == baseline * hpp
    box = tell(w, "get-character-bounding-box", C.String("x"), font, [Fraction(1, 2), Fraction(1, 4)])
    assert box == [[Fraction(1, 2) - wpp, Fraction(1, 4) + hpp * (-baseline - 1)],
                   [Fraction(1, 2) + wpp * (cw + 1), Fraction(1, 4) + hpp * (ch + 1 - baseline)]]


def test_host_messages(gui):
    w = gui.make_unscrollable_graphics_window(100, 50)
    host = tell(w, "get-toplevel")
    assert tell(w, "set-window-title", C.String("Workspace")) == "done"
    assert host.get_title() == "Workspace"
    tell(w, "set-position", 10, 20)
    assert host.geometry == "+10+20"
    tell(w, "remember-position")
    tell(w, "set-position", 30, 40)
    tell(w, "restore-position")
    assert host.geometry == "+10+20"
    assert tell(w, "scrollbar-present?", "vertical") is False
    assert gui.get_scrollbar_from_frame(host, "vertical", True) is False
    assert tell(w, "set-icon-label", C.String("x")) == "ignored"
    assert tell(w, "get-info") == [["visible:", 100, "x", 50], ["canvas:", 100, "x", 50], ["scroll:"]]


def test_views_workspace_window(gui):
    from metacat import setup, view_globals
    from metacat.gui import views, workspace_graphics as wg
    views.load_views()
    assert view_globals.restore_current_state is wg.restore_current_state
    w = views.attach_workspace_view(800)
    assert setup.g_workspace_window is w
    assert setup.p_workspace_graphics is True
    assert view_globals.p_flash_pause == 0
    assert tell(w, "object-type") == "workspace-window"
    assert tell(w, "get-size") == [800, 600]
    assert views.window_items(w) > 0
    assert tell(view_globals.p_letter_font, "object-type") == "mfont"
