# Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
"""The Slipnet, Temperature, Coderack, Commentary and EEG windows (loop0002 item 14).

Structural checks of gui/slipnet_graphics.py, gui/temperature_graphics.py,
gui/coderack_graphics.py, gui/commentary_graphics.py and gui/eeg_graphics.py
against their .ss files, and behaviour checks of each window against a
recording graphics window (`FakeWindow`) standing for general-graphics.ss's
windows.  The panels battery (test_panels.py) checks the layout table,
mercury-pexp, draw-thermometer and the EEG object against the oracle.
"""
from __future__ import annotations

import importlib
import inspect
import re
import subprocess
import sys
import types
from fractions import Fraction
from pathlib import Path

import pytest

import metacat
from metacat import chez, engine, setup, view_globals
from metacat.names import scheme_to_python
from metacat.objects import SchemeObject, tell

ROOT = Path(__file__).resolve().parents[2]
ORIGINAL = ROOT / "chez_scheme" / "original"
String = chez.String

FILES = {
    "slipnet-graphics": "metacat.gui.slipnet_graphics",
    "temperature-graphics": "metacat.gui.temperature_graphics",
    "coderack-graphics": "metacat.gui.coderack_graphics",
    "commentary-graphics": "metacat.gui.commentary_graphics",
    "eeg-graphics": "metacat.gui.eeg_graphics",
}
# eeg-graphics.ss's engine part (the model records into the EEG without a window)
EEG_ENGINE_NAMES = {"%EEG-table%", "%EEG-buffer-size%", "make-EEG", "*EEG*"}


def defines(stem):
    text = (ORIGINAL / (stem + ".ss")).read_text()
    return re.findall(r"^\(define\s+(\S+)", text, re.M)


def is_lambda_define(stem, name):
    text = (ORIGINAL / (stem + ".ss")).read_text()
    return re.search(r"^\(define\s+" + re.escape(name) + r"\s*\n\s*\(lambda", text, re.M) is not None


# ---------------------------------------------------------------------------
# Structure

@pytest.mark.parametrize("stem", sorted(FILES))
def test_every_define_is_translated(stem):
    module = importlib.import_module(FILES[stem])
    missing = []
    for name in defines(stem):
        if stem == "eeg-graphics" and name in EEG_ENGINE_NAMES:
            where = importlib.import_module("metacat.eeg_graphics")
        else:
            where = module
        if not hasattr(where, scheme_to_python(name)):
            missing.append(name)
    assert missing == []


@pytest.mark.parametrize("stem", sorted(FILES))
def test_docstrings_name_their_origin(stem):
    module = importlib.import_module(FILES[stem])
    doc = module.__doc__
    assert "GNU General Public License" in doc
    assert "Translated to Python (2026) from %s.ss" % stem in doc
    assert "racket/gui/%s.rktl" % stem in doc
    wrong = []
    for name in defines(stem):
        if stem == "eeg-graphics" and name in EEG_ENGINE_NAMES:
            continue
        if is_lambda_define(stem, name):
            fn = getattr(module, scheme_to_python(name))
            if not (fn.__doc__ or "").startswith("%s.ss: %s" % (stem, name)):
                wrong.append(name)
    for _, obj in inspect.getmembers(module, inspect.isfunction):
        if obj.__module__ == module.__name__ and not obj.__name__.startswith("_"):
            if not (obj.__doc__ or "").startswith(stem + ".ss: ") and \
                    not (obj.__doc__ or "").startswith("port: "):
                wrong.append(obj.__name__)
    assert wrong == []


def test_no_tkinter_at_import():
    code = ("import sys; import " + ", ".join(FILES.values()) +
            "; assert 'tkinter' not in sys.modules, 'tkinter imported'")
    result = subprocess.run([sys.executable, "-c", code], cwd=ROOT / "python",
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr


def test_engine_does_not_import_these_modules():
    for path in (ROOT / "python" / "metacat").glob("*.py"):
        text = path.read_text()
        for mod in FILES.values():
            assert mod not in text, (path.name, mod)


# ---------------------------------------------------------------------------
# A recording graphics window

class FakeWindow(SchemeObject):
    """A stand-in for general-graphics.ss's windows: records every message and
    answers the getters the panels ask with a fixed metric."""

    def __init__(this, kind, *args):
        this.kind = kind
        this.args = args
        this.log = []
        this.x_pixels = args[0]
        this.y_pixels = args[1]

    def otherwise(this, self, msg, args):
        this.log.append((msg,) + tuple(args))
        x = this.x_pixels
        if msg == "object-type":
            return "graphics-window"
        if msg == "get-visible-w":
            return this.x_pixels
        if msg == "get-visible-h":
            return this.y_pixels
        if msg in ("get-width-per-pixel", "get-height-per-pixel"):
            return Fraction(1, x)
        if msg == "get-string-width":
            return Fraction(6 * len(args[0]), x)
        if msg in ("get-character-height", "get-string-height"):
            return Fraction(12, x)
        if msg == "get-visible-x-max":
            return 1
        if msg == "get-y-max":
            return Fraction(10)
        if msg == "get-visible-y-min":
            return Fraction(8)
        return "done"

    def sent(this, msg):
        return [entry[1:] for entry in this.log if entry[0] == msg]


def flatten(x):
    if isinstance(x, list):
        for y in x:
            yield from flatten(y)
    else:
        yield x


@pytest.fixture
def views(monkeypatch):
    """The engine loaded, offscreen fonts, the gui modules loaded, and
    general-graphics.ss's window makers replaced by FakeWindow.  Engine state
    the windows change (codelet types, slipnodes, view globals) is restored."""
    engine.load()
    from metacat.gui import hosts
    hosts.install_offscreen_fonts()
    from metacat.gui import constants as K
    K.set_window_size_defaults(1)
    made = []

    def maker(kind):
        def make(*args):
            w = FakeWindow(kind, *args)
            made.append(w)
            return w
        return make

    gg = importlib.import_module("metacat.gui.general_graphics") \
        if importlib.util.find_spec("metacat.gui.general_graphics") else None
    if gg is None:
        gg = types.ModuleType("metacat.gui.general_graphics")
        monkeypatch.setitem(sys.modules, "metacat.gui.general_graphics", gg)
        import metacat.gui as gui_pkg
        monkeypatch.setattr(gui_pkg, "general_graphics", gg, raising=False)
    for name in ("make_unscrollable_graphics_window",
                 "make_horizontal_scrollable_graphics_window",
                 "make_scrollable_text_window"):
        monkeypatch.setattr(gg, name, maker(name), raising=False)
    eg = metacat.general_graphics
    monkeypatch.setattr(eg, "disk", getattr(eg, "disk", lambda c, d: ["filled-arc", c, [d, d], 0, 360]),
                        raising=False)
    monkeypatch.setattr(eg, "circle", getattr(eg, "circle", lambda c, d: ["arc", c, [d, d], 0, 360]),
                        raising=False)
    monkeypatch.setattr(eg, "solid_box", getattr(
        eg, "solid_box", lambda x0, y0, x1, y1: ["filled-rectangle", [x0, y0], [x1, y1]]),
        raising=False)
    # state the windows change
    for name in ("p_coderack_codelet_count_font",):
        monkeypatch.setattr(view_globals, name, getattr(view_globals, name))
    for name in ("p_eliza_mode", "p_justify_mode", "p_slipnet_graphics",
                 "p_coderack_graphics", "p_codelet_count_graphics",
                 "p_highlight_last_codelet"):
        monkeypatch.setattr(setup, name, getattr(setup, name))
    saved = []
    for obj in list(metacat.coderack.g_codelet_types) + list(metacat.slipnet.g_slipnet_nodes):
        for slot in type(obj).__slots__:
            if hasattr(obj, slot):
                saved.append((obj, slot, getattr(obj, slot)))
    for stem, mod in FILES.items():
        m = importlib.import_module(mod)
        m.load()
    yield made
    for obj, slot, value in saved:
        setattr(obj, slot, value)


# ---------------------------------------------------------------------------
# Commentary

def comment_window(made):
    from metacat.gui import commentary_graphics as cg
    w = cg.make_comment_window()
    text = [m for m in made if m.kind == "make_scrollable_text_window"][-1]
    return w, text


def test_comment_window_initialize(views):
    from metacat.gui import commentary_graphics as cg, constants as K
    w, text = comment_window(views)
    assert text.args == (K.p_default_comment_window_width, K.p_default_comment_window_height,
                         K.p_virtual_comment_window_length,
                         K.p_comment_window_background_color)
    assert text.sent("set-icon-label") == [(K.p_comment_window_icon_label,)]
    assert ("new-font", cg.p_comment_window_font) in text.log
    assert ("centering-off",) in text.log
    draws = text.sent("draw")
    assert len(draws) == 1
    assert "(Move scroll bar to bottom)" in list(flatten(draws[0][0]))
    # reminder-y is halfway between y-max and the visible y-min
    assert draws[0][0][2][1] == [Fraction(1, 2), 9]
    assert tell(w, "object-type") == "comment-window"


@pytest.mark.parametrize("eliza", [True, False])
def test_comment_window_new_problem(views, eliza):
    setup.p_eliza_mode = eliza
    setup.p_justify_mode = False
    w, text = comment_window(views)
    tell(w, "new-problem", "abc", "abd", "xyz", False)
    paragraphs = text.sent("draw-paragraph")
    expected = ('Okay, if "abc" changes to "abd", what does "xyz" change to?  Hmm...'
                if eliza else
                'Beginning run:  If "abc" changes to "abd", what does "xyz" change to?')
    assert paragraphs == [(expected,)]
    assert isinstance(paragraphs[0][0], String)


def test_comment_window_justify_and_switch_modes(views):
    setup.p_eliza_mode = True
    setup.p_justify_mode = True
    w, text = comment_window(views)
    tell(w, "new-problem", "abc", "abd", "xyz", "wyz")
    tell(w, "add-comment", ["one", " two"], ["three"])
    assert text.sent("draw-paragraph") == [
        ('Let\'s see... "abc" changes to "abd", and "xyz" changes to "wyz".  Hmm...',),
        ("one two",)]
    setup.p_eliza_mode = False
    tell(w, "switch-modes")
    (paragraphs,), = text.sent("set-paragraphs")
    assert paragraphs == [1, "three", 1,
                          'Beginning justify run:  "abc" changes to "abd", and '
                          '"xyz" changes to "wyz"...']
    assert text.log[-1] == ("redraw",)
    tell(w, "clear")
    tell(w, "switch-modes")
    assert text.sent("set-paragraphs")[-1] == ([],)


def test_comment_window_delegates(views):
    w, text = comment_window(views)
    assert tell(w, "raise") == "done"
    assert text.log[-1] == ("raise",)


# ---------------------------------------------------------------------------
# Temperature

def test_temperature_window(views):
    from metacat.gui import temperature_graphics as tg, constants as K
    w = tg.make_temperature_window()
    g = [m for m in views if m.kind == "make_unscrollable_graphics_window"][-1]
    width = K.p_default_temperature_width
    assert g.args == (width, chez.exact_ceiling(Fraction(5, 2) * width),
                      K.p_temperature_background_color)
    # 1.2: the icon label is the title, still #f when it is set
    assert g.sent("set-icon-label") == [(False,)]
    assert g.sent("set-window-title") == [(K.p_temperature_window_title,)]
    tags = [d[1] for d in g.sent("draw") if len(d) > 1]
    assert tags == ["title", "bar", "value"]
    # draw-thermometer: the body and 11 gradations, untagged
    assert len([d for d in g.sent("draw") if len(d) == 1]) == 12
    assert "0" in list(flatten(g.sent("draw")[-1][0]))
    assert tg.p_temperature_title_font is not False
    n = len(g.log)
    assert tell(w, "update-graphics", 0) == "done"
    assert len(g.log) == n
    tell(w, "update-graphics", 57)
    assert [e[0] for e in g.log[n:]] == ["retag", "draw", "delete", "delete", "draw"]
    bar = g.log[n + 1]
    assert bar[2] == "bar"
    level = Fraction(1, 2) + Fraction(57, 100) * Fraction(29, 20)
    assert bar[1] == tg.mercury_pexp(Fraction(1, 2) - Fraction(1, 20) + Fraction(2, width),
                                     Fraction(1, 2),
                                     Fraction(1, 2) + Fraction(1, 20) - Fraction(2, width),
                                     level, K.p_thermometer_mercury_color)
    assert "57" in list(flatten(g.log[-1][1]))
    assert isinstance([x for x in flatten(g.log[-1][1]) if x == "57"][0], String)


def test_temperature_title_abbreviated(views):
    from metacat.gui import temperature_graphics as tg
    tg.make_temperature_window(20)
    g = [m for m in views if m.kind == "make_unscrollable_graphics_window"][-1]
    title = [d for d in g.sent("draw") if len(d) > 1 and d[1] == "title"][0][0]
    assert "Temp." in list(flatten(title))


def test_mercury_pexp_line_and_box():
    from metacat.gui import temperature_graphics as tg
    assert tg.mercury_pexp(1, 2, 1, 3, "c") == \
        ["let-sgl", [["foreground-color", "c"]], ["line", [1, 2], [1, 3]]]
    assert tg.mercury_pexp(1, 2, 4, 3, "c") == \
        ["let-sgl", [["foreground-color", "c"]], ["filled-rectangle", [1, 2], [4, 3]]]


# ---------------------------------------------------------------------------
# Coderack

def test_coderack_window_initialize(views):
    from metacat.gui import coderack_graphics as cg, constants as K
    setup.p_coderack_graphics = True
    setup.p_codelet_count_graphics = True
    w = cg.make_coderack_window()
    g = [m for m in views if m.kind == "make_unscrollable_graphics_window"][-1]
    width = K.p_default_coderack_width
    assert g.args == (width, chez.exact_ceiling(Fraction(26, 10) * width),
                      K.p_coderack_background_color)
    types_ = metacat.coderack.g_codelet_types
    assert all(t.coderack_window is w for t in types_)
    assert view_globals.p_coderack_codelet_count_font is not False
    assert view_globals.p_coderack_codelet_count_font is not cg.p_coderack_codelet_sum_font
    draws = g.sent("draw")
    title = [d for d in draws if d[1] == "title"]
    assert "Coderack" in list(flatten(title[0][0]))
    skeleton = [d for d in draws if d[1] == "skeleton"][0][0]
    words = list(flatten(skeleton))
    assert "Codelet Type" in words
    assert "Selection Probability" in words or "Probability" in words
    # the skeleton: subtitles, the divider, one slot per codelet type, two lines
    assert len(skeleton) == 2 + 2 + len(types_) + 2
    assert [d[1] for d in draws].count("bar") == len(types_)
    assert [d[1] for d in draws].count("sum") == 1
    assert [d[1] for d in draws].count("total") == 1
    assert tell(w, "get-display-state") == "normal"


def test_coderack_window_patterns_and_clear(views):
    from metacat.gui import coderack_graphics as cg
    w = cg.make_coderack_window(230)
    g = [m for m in views if m.kind == "make_unscrollable_graphics_window"][-1]
    t = metacat.coderack.g_codelet_types[0]
    tell(w, "display-patterns", [["codelet-pattern", [t, 50]]], "Pattern")
    assert tell(w, "get-display-state") == "pattern"
    assert [d for d in g.sent("draw") if d[1] == "title"][-1][0][2][2] == "Pattern"
    tell(w, "clear")
    assert tell(w, "get-display-state") == "normal"
    assert tell(w, "get-last-codelet-type") is False


# ---------------------------------------------------------------------------
# Slipnet

def test_slipnet_window(views):
    from metacat.gui import slipnet_graphics as sg, constants as K
    from metacat import utilities as u
    table = sg.g_13x5_layout_table
    assert u.row_dimension(table) == 13 and u.column_dimension(table) == 5
    w = sg.make_slipnet_window(table)
    g = [m for m in views if m.kind == "make_unscrollable_graphics_window"][-1]
    width = K.p_default_13x5_slipnet_width
    assert g.args == (width, chez.exact_ceiling(19 * Fraction(width, 40)),
                      K.p_slipnet_background_color)
    delta = Fraction(1, 40)
    a = metacat.slipnet.plato_a
    for i in range(13):
        for j in range(5):
            if u.table_ref(table, i, j) is a:
                assert tell(a, "get-graphics-coord") == [3 * delta * i + 2 * delta,
                                                         3 * delta * j + 3 * delta]
    draws = g.sent("draw")
    assert [d[1] for d in draws] == ["title", "label"]
    labels = draws[1][0][2]
    assert len(labels) == 2 + 59
    assert tell(w, "object-type") == "slipnet-window"
    n = len(g.log)
    tell(w, "update-graphics")
    acts = [e for e in g.log[n:] if e[0] == "draw"]
    assert len(acts) == len(metacat.slipnet.g_slipnet_nodes)
    assert g.log[n] == ("retag", "activation", "garbage") and g.log[-1] == ("delete", "garbage")


# ---------------------------------------------------------------------------
# EEG

def test_eeg_window(views, monkeypatch):
    from metacat.gui import eeg_graphics as eg, constants as K
    eng = metacat.eeg_graphics if hasattr(metacat, "eeg_graphics") else None
    if eng is None or not hasattr(eng, "p_EEG_table"):
        eng = types.ModuleType("metacat.eeg_graphics")
        monkeypatch.setitem(sys.modules, "metacat.eeg_graphics", eng)
        monkeypatch.setattr(metacat, "eeg_graphics", eng, raising=False)
    table = [[0, String("Workspace Activity"), String("white"), 100, False, None],
             [1, String("Average Workspace Activity"), String("yellow"), 100, True, None],
             [2, String("Temperature"), String("red"), 100, True, None]]
    monkeypatch.setattr(eng, "p_EEG_table", table, raising=False)

    class FakeEEG(SchemeObject):
        def otherwise(this, self, msg, args):
            assert msg == "get-current-value"
            return {1: 40, 2: 80}[args[0]]

    monkeypatch.setattr(eng, "g_EEG", FakeEEG(), raising=False)
    w = eg.make_EEG_window()
    g = [m for m in views if m.kind == "make_horizontal_scrollable_graphics_window"][-1]
    assert g.args == (K.p_EEG_window_width, K.p_EEG_window_height, K.p_virtual_EEG_length,
                      K.p_EEG_background_color)
    title = g.sent("draw")[0][0]
    assert " Average Workspace Activity (yellow) and Temperature (red) " in list(flatten(title))
    tell(w, "plot-current-values")
    curves = [d for d in g.sent("draw") if d[1] == "curve"]
    max_height = 1 - Fraction(12, K.p_EEG_window_width)
    assert curves[0][0] == ["let-sgl", [["foreground-color", "yellow"]],
                            ["line", [0, max_height], [0, Fraction(40, 100) * max_height]]]
    assert curves[1][0][2][2] == [0, Fraction(80, 100) * max_height]
    tell(w, "plot-current-values")
    curves = [d for d in g.sent("draw") if d[1] == "curve"]
    assert curves[2][0][2][2][0] == Fraction(1, 400)
    assert ("caching-on",) in g.log and ("flush",) in g.log


# ---------------------------------------------------------------------------
# The real windows (gui/general_graphics.py) on offscreen hosts

@pytest.fixture
def real_views(monkeypatch):
    """As `views`, but with general-graphics.ss's real windows, offscreen."""
    engine.load()
    from metacat.gui import hosts, sgl, constants as K, general_graphics as gg
    hosts.install_offscreen_fonts()
    monkeypatch.setattr(hosts, "_maker", hosts.OffscreenHost)
    for m in (K, sgl, gg):
        m.load()
    K.set_window_size_defaults(1)
    for name in ("p_coderack_codelet_count_font", "g_fg_color", "p_default_fg_color"):
        monkeypatch.setattr(view_globals, name, getattr(view_globals, name))
    for name in ("p_eliza_mode", "p_justify_mode", "p_slipnet_graphics",
                 "p_coderack_graphics", "p_codelet_count_graphics",
                 "p_highlight_last_codelet"):
        monkeypatch.setattr(setup, name, getattr(setup, name))
    saved = []
    for obj in list(metacat.coderack.g_codelet_types) + list(metacat.slipnet.g_slipnet_nodes):
        for slot in type(obj).__slots__:
            if hasattr(obj, slot):
                saved.append((obj, slot, getattr(obj, slot)))
    for mod in FILES.values():
        importlib.import_module(mod).load()
    yield
    for obj, slot, value in saved:
        setattr(obj, slot, value)


def items(window):
    """the number of canvas items drawn on a panel's graphics window"""
    g = getattr(window, "graphics_window", None) or window.text_window.graphics_window
    return g.vp.canvas.items


def test_real_windows_draw(real_views):
    from metacat.gui import (slipnet_graphics as sg, temperature_graphics as tg,
                             coderack_graphics as crg, commentary_graphics as cg,
                             eeg_graphics as eg)
    setup.p_eliza_mode = True
    setup.p_justify_mode = False
    w = cg.make_comment_window()
    n = items(w)
    tell(w, "new-problem", "abc", "abd", "xyz", False)
    assert items(w) > n
    w = tg.make_temperature_window()
    n = items(w)
    assert n > 12
    tell(w, "update-graphics", 40)
    assert items(w) == n + 2
    w = sg.make_slipnet_window(sg.g_13x5_layout_table)
    n = items(w)
    assert n >= 60
    metacat.slipnet.plato_a.activation = 100     # (restored by the fixture)
    tell(w, "update-graphics")
    assert items(w) == n + 1      # zero activations draw nothing
    w = crg.make_coderack_window()
    n = items(w)
    assert n > len(metacat.coderack.g_codelet_types)
    tell(w, "update-graphics")
    assert items(w) > n
    if hasattr(metacat, "eeg_graphics") and hasattr(metacat.eeg_graphics, "p_EEG_table"):
        w = eg.make_EEG_window()
        assert items(w) == 1
