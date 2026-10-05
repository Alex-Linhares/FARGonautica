"""Item 13: the SGL interpreter (metacat/gui/sgl.py) and fonts (metacat/gui/fonts.py).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Three kinds of check, every expected value from Chez:
- tests/diff/sgl-battery.scm, test for test, against python/fixtures/sgl/: every
  message the interpreter sends its viewport, with its arguments, for every form,
  binding, origin, erasure and tag (b:vp is a recording viewport, as in
  tests/diff/sgl-chez-setup.ss);
- the Tcl command stream: python/oracle/sgl-fixture.scm drawn by the Python
  <viewport> on a recording canvas must send, command for command, what the
  original sends Tk (python/fixtures/sgl-tcl/, captured by
  python/oracle/capture_sgl_tcl.py from the oracle's swl:tcl-eval).  Text is
  measured on a fake hidden canvas with the capture's fixed metric, so fonts.ss's
  get-pixel-size is in the stream too;
- the rendering on a real tkinter Canvas under Xvfb (test_render_on_tk, slow tier):
  the fixture as PNG (a grab of the window), with pixel checks; the PNG is for
  inspection next to racket/tests/snapshots/sgl-fixture.png
  (python/tests/snapshots/sgl-fixture.png is one, made by hand with
  render_sgl_fixture.py).
"""
from __future__ import annotations

import ast
import filecmp
import hashlib
import importlib
import inspect
import os
import re
import subprocess
import sys
from fractions import Fraction
from pathlib import Path

import pytest

import scheme_reader
from chez_fixtures import chez, manifest
from scheme_canon import canon

from metacat import chez as C
from metacat.names import scheme_to_python
from metacat.objects import tell

HERE = Path(__file__).resolve().parent
PY = HERE.parent
ROOT = PY.parent
ORIGINAL = ROOT / "chez_scheme" / "original"
FIXTURE_SCM = PY / "oracle" / "sgl-fixture.scm"
STREAMS = PY / "fixtures" / "sgl-tcl"
PRELUDE_FAMILIES = ["times", "helvetica", "courier"]   # prelude.ss's swl:font-families


def S(text):
    return scheme_reader.read(text)


def mods():
    sgl = importlib.import_module("metacat.gui.sgl")
    fonts = importlib.import_module("metacat.gui.fonts")
    colors = importlib.import_module("metacat.gui.colors")
    return sgl, fonts, colors


@pytest.fixture(scope="module")
def m():
    """The three modules, loaded with the oracle prelude's font families."""
    sgl, fonts, colors = mods()
    fonts.load(PRELUDE_FAMILIES)
    sgl.load()
    saved = fonts.g_hidden_canvas
    yield sgl, fonts, colors
    fonts.g_hidden_canvas = saved


# ---------------------------------------------------------------------------
# tests/diff/sgl-chez-setup.ss: the recording viewport b:vp

def canon_arg(x, colors):
    """sgl-chez-setup.ss: b:canon-arg (colours (rgb r g b), other objects 'obj)"""
    if isinstance(x, colors.Rgb):
        return ["rgb", x.r, x.g, x.b]
    if isinstance(x, (bool, int, float, Fraction, str)):
        return x
    if isinstance(x, list):
        return [canon_arg(a, colors) for a in x]
    return "obj"


class Recorder:
    """sgl-chez-setup.ss: b:vp.  Records every message but get-background-color."""

    def __init__(self, colors):
        self.colors = colors
        self.bg = colors.swl_color(C.String("light grey"))
        self.calls = []

    def get_background_color(self):
        return self.bg

    def __getattr__(self, name):
        if name.startswith("__"):
            raise AttributeError(name)
        scheme = name[:-5] + "!" if name.endswith("_bang") else name
        scheme = scheme.replace("_", "-")

        def record(*args):
            self.calls.append([scheme] + [canon_arg(a, self.colors) for a in args])
            return "ok"
        return record


def record(vp, thunk):
    """sgl-chez-setup.ss: b:record"""
    vp.calls = []
    r = thunk()
    return [r, list(vp.calls)]


def battery_cases(sgl, colors):
    vp = Recorder(colors)
    bg = vp.bg

    def draw(pexp, *tag):
        return record(vp, lambda: sgl.draw_bang(vp, S(pexp), *tag))

    init_env = sgl.init_env
    ca = lambda x: canon_arg(x, colors)
    return {
        "env-init": lambda: [ca(sgl.lookup(init_env, s)) for s in
                             ["foreground-color", "background-color", "font",
                              "text-justification", "text-mode", "line-width",
                              "line-style", "origin", "unknown"]],
        "env-line-styles": lambda: [sgl.lookup(sgl.extend(init_env, "line-style", st), "line-style")
                                    for st in ["dotted", "dashed", "solid", "other"]],
        "env-colors": lambda: [
            ca(sgl.lookup(sgl.extend(init_env, "foreground-color", C.String("red")), "foreground-color")),
            ca(sgl.lookup(sgl.extend(init_env, "background-color", C.String("navy blue")),
                          "background-color")),
            ca(sgl.lookup(sgl.extend(init_env, "erase-color", C.String("pink")), "erase-color")),
            ca(sgl.lookup(sgl.extend(init_env, "foreground-color", bg), "foreground-color"))],
        "env-origin-not-bound": lambda: [
            sgl.extend(init_env, "origin", [5, 5])("origin"),
            sgl.extend_star(init_env, S("((origin (1 2)) (line-width 3))"))("line-width"),
            sgl.extend_star(init_env, S("((line-width 3) (line-width 4))"))("line-width"),
            sgl.empty_env("anything")],
        "dash-pattern": lambda: sgl.graphics_dash_pattern(),
        "polyline-coords": lambda: sgl.generate_polyline_coords(
            S("((0 0) (1 2) (3 4))"), 10, 20,
            lambda x, o: C.mul(10, C.add(x, o)), lambda y, o: C.sub(100, C.add(y, o))),
        "empty": lambda: draw("()"),
        "rectangle": lambda: draw("(rectangle (1 2) (30 40))"),
        "rectangle-rational": lambda: draw("(rectangle (1/3 2.5) (30 -40))"),
        "filled-rectangle": lambda: draw("(filled-rectangle (10 10) (20 30))"),
        "arc": lambda: draw("(arc (50 50) (20 10) 30 120)"),
        "arc-full": lambda: draw("(arc (50 50) (21 11) 0 360)"),
        "arc-over": lambda: draw("(arc (50 50) (20 20) 90 400)"),
        "filled-arc": lambda: draw("(filled-arc (50 50) (20 10) -45 90)"),
        "filled-arc-full": lambda: draw("(filled-arc (5 5) (3 3) 0 360)"),
        "line-one": lambda: draw("(line (0 0) (10 10))"),
        "line-many": lambda: draw("(line (0 0) (10 10) (20 0) (30 30))"),
        "polyline": lambda: draw("(polyline (0 0) (10 10) (20 0) (30 30) (40 0))"),
        "polygon": lambda: draw("(polygon (0 0) (10 10) (20 0))"),
        "filled-polygon": lambda: draw("(filled-polygon (0 0) (10 10) (20 0) (5 -5))"),
        "polypoints": lambda: draw("(polypoints (0 0) (2 2) (4 4))"),
        "dashed-polypoints": lambda: draw("(dashed-polypoints (0 0) (8 0) (16 0))"),
        "ring": lambda: draw("(ring (50 50) 20 10)"),
        "ring-arc": lambda: draw("(ring (50 50) 21 9 45 270)"),
        "text-plain": lambda: draw('(text "abc")'),
        "text-at": lambda: draw('(text (10 20) "abc")'),
        "text-relative": lambda: draw('(text (text-relative (2 -1)) "abc")'),
        "clear": lambda: draw("(clear)"),
        "clear-color": lambda: draw('(clear "white")'),
        "rule": lambda: draw("(rule top ((a b) (c d)) (rectangle (0 0) (5 5)))"),
        "invalid": lambda: draw("(squiggle (0 0))"),
        "invalid-nested": lambda: draw("(let-sgl () (rectangle (0 0) (1 1)) (squiggle))"),
        "let-sgl-empty": lambda: draw("(let-sgl () (rectangle (0 0) (1 1)) (line (0 0) (1 1)))"),
        "let-sgl-all-bindings": lambda: draw("""
            (let-sgl ((origin (10 20)) (line-width 3) (foreground-color "red")
                      (background-color "blue") (line-style dashed) (font big-font)
                      (text-justification center) (text-mode image))
              (rectangle (0 0) (5 5)) (filled-rectangle (0 0) (5 5))
              (arc (0 0) (4 4) 0 90) (filled-arc (0 0) (4 4) 0 90)
              (line (0 0) (1 1)) (polyline (0 0) (1 1) (2 0))
              (polygon (0 0) (1 1) (2 0)) (filled-polygon (0 0) (1 1) (2 0))
              (ring (0 0) 4 2) (ring (0 0) 4 2 0 180)
              (polypoints (0 0)) (dashed-polypoints (0 0))
              (text "x") (text (1 1) "y") (text (text-relative (1 1)) "z"))"""),
        "let-sgl-nested-origins": lambda: draw("""
            (let-sgl ((origin (10 20)))
              (rectangle (0 0) (1 1))
              (let-sgl ((origin (1/2 -3)) (line-style dotted))
                (rectangle (0 0) (1 1))
                (let-sgl ((origin (0.25 0.5)))
                  (text (1 1) "deep")
                  (line (0 0) (1 1))))
              (rectangle (0 0) (1 1)))"""),
        "let-sgl-shadowing": lambda: draw("""
            (let-sgl ((foreground-color "red") (line-width 2))
              (line (0 0) (1 1))
              (let-sgl ((foreground-color "green")) (line (0 0) (1 1)))
              (line (0 0) (1 1)))"""),
        "let-sgl-justifications": lambda: draw("""
            (let-sgl ()
              (let-sgl ((text-justification left)) (text "l"))
              (let-sgl ((text-justification center)) (text "c"))
              (let-sgl ((text-justification right)) (text "r")))"""),
        "let-sgl-solid": lambda: draw("(let-sgl ((line-style solid)) (polyline (0 0) (1 1)))"),
        "erase-string": lambda: draw("""
            (erase "white"
              (let-sgl ((foreground-color "red") (background-color "blue"))
                (rectangle (0 0) (1 1)) (filled-rectangle (0 0) (1 1))
                (arc (0 0) (1 1) 0 360) (filled-arc (0 0) (1 1) 0 10)
                (line (0 0) (1 1)) (polyline (0 0) (1 1))
                (polygon (0 0) (1 1) (2 2)) (filled-polygon (0 0) (1 1) (2 2))
                (ring (0 0) 4 2) (ring (0 0) 4 2 0 90)
                (polypoints (0 0)) (dashed-polypoints (0 0))
                (text "gone")))"""),
        "erase-inside-let-sgl": lambda: draw("""
            (let-sgl ((origin (5 5)) (line-width 2))
              (rectangle (0 0) (1 1))
              (erase "yellow" (let-sgl ((origin (1 1))) (rectangle (0 0) (1 1))))
              (rectangle (0 0) (1 1)))"""),
        "erase-nested": lambda: draw('(erase "white" (erase "red" (line (0 0) (1 1))))'),
        "erase-bang": lambda: record(vp, lambda: sgl.erase_bang(
            vp, S('(let-sgl () (rectangle (0 0) (2 2)) (text "t"))'))),
        "erase-color-object": lambda: record(vp, lambda: sgl.draw_exp(
            vp, S("(erase (rgb 1 2 3) (line (0 0) (1 1)))"), init_env, 0, 0, False, "all")),
        "tag-given": lambda: draw('(let-sgl () (rectangle (0 0) (1 1)) (text "a"))', "flash"),
        "tag-eraser": lambda: draw("(rectangle (0 0) (1 1))", "eraser"),
        "draw-exp-direct": lambda: record(vp, lambda: sgl.draw_exp(
            vp, S("(let-sgl ((origin (1 1))) (rectangle (0 0) (1 1)))"),
            sgl.extend(init_env, "foreground-color", C.String("orange")), 7, 8,
            C.String("x"), "mytag")),
        "draw-exps-direct": lambda: record(vp, lambda: sgl.draw_exps(
            vp, S('((line (0 0) (1 1)) (text "q") ())'), init_env, 2, 3, C.String("x"), "tags")),
        "fg-color-object": lambda: record(vp, lambda: sgl.draw_bang(
            vp, ["let-sgl", [["foreground-color", bg]], S("(rectangle (0 0) (1 1))")])),
    }


SGL_TESTS = list(manifest("sgl"))


def test_every_battery_test_is_translated(m):
    sgl, fonts, colors = m
    assert sorted(battery_cases(sgl, colors)) == sorted(SGL_TESTS)


@pytest.mark.parametrize("name", SGL_TESTS)
def test_sgl_battery(m, name):
    sgl, fonts, colors = m
    case = battery_cases(sgl, colors)[name]
    expected = chez("sgl", name)
    if expected == "ERROR":
        with pytest.raises(C.SchemeError):
            case()
    else:
        assert canon(case()) == expected


# ---------------------------------------------------------------------------
# The Tcl command stream (python/oracle/sgl-tcl.ss's conventions)

def metric(string, font):
    """sgl-tcl.ss: $metric, the fixed text metric of the fake hidden canvas"""
    size, style = font.size, font.style
    px = -size if size < 0 else C.exact_round(Fraction(size * 4, 3))
    cw = (3 * px) // 5 + 1 + (1 if "bold" in style else 0)
    w = cw * len(string)
    h = px + px // 4 + 2
    return [1, 2, 1 + w, 2 + h]


def stream_canon(x, colors, fonts):
    """sgl-tcl.ss: $canon"""
    if isinstance(x, colors.Rgb):
        return ["rgb", x.r, x.g, x.b]
    if isinstance(x, fonts.SwlFont):
        return ["font", x.face, x.size, list(x.style)]
    if isinstance(x, (list, tuple)):
        return [stream_canon(a, colors, fonts) for a in x]
    return x


class RecordingCanvas:
    """A Tk canvas that records the commands it gets (sgl-tcl.ss's swl:tcl-eval and
    <canvas> base); the hidden canvas answers bbox with the fixed metric."""

    def __init__(self, name, log, colors):
        self.name, self.log = name, log
        self.bg = colors.swl_color(C.String("white"))
        self.last_text = None

    def tcl(self, *args):
        self.log.append(["tcl", self.name, *args])
        if self.name == "hidden":
            if args[0] == "create":
                opts = dict(zip(args[4::2], args[5::2]))
                self.last_text = (opts["-text"], opts["-font"])
                return C.String("1")
            if args[0] == "bbox":
                return metric(*self.last_text)
        return C.String("")

    def get_background_color(self):
        return self.bg

    def set_background_color_bang(self, c):
        self.log.append(["swl", self.name, "set-background-color!", c])
        self.bg = c


def fixture_forms():
    forms = scheme_reader.read_all(FIXTURE_SCM.read_text())
    return {f[0]: f[1:] for f in forms if f[0] != "viewport"}, \
        [f[1:] for f in forms if f[0] == "viewport"]


def expand(pexp, fonts_table):
    """sgl-tcl.ss: $expand (cells and font names)"""
    if not isinstance(pexp, list) or not pexp:
        return pexp
    if pexp[0] == "cell":
        x, y, label = pexp[1:4]
        return expand(["let-sgl", [["origin", [x, y]]],
                       ["let-sgl", [["font", "label-font"], ["foreground-color", C.String("grey40")]],
                        ["text", [4, 92], label]],
                       ["let-sgl", [["foreground-color", C.String("grey80")]],
                        ["rectangle", [0, 0], [155, 105]]],
                       *pexp[4:]], fonts_table)
    if (pexp[0] == "font" and len(pexp) == 2 and isinstance(pexp[1], str)
            and pexp[1] in fonts_table):
        return ["font", fonts_table[pexp[1]]]
    return [expand(p, fonts_table) for p in pexp]


def make_viewport(sgl, canvas, w, h, xmin, ymin, xmax, ymax):
    """sgl-tcl.ss: $make-viewport (racket/tests/sgl-fixture.rkt's transforms)"""
    wpp = C.div(C.sub(xmax, xmin), w)
    hpp = C.div(C.sub(ymax, ymin), h)
    return sgl.Viewport(
        canvas,
        lambda i: C.add(xmin, C.mul(wpp, i)),
        lambda j: C.sub(ymax, C.mul(hpp, j)),
        lambda x, offset: C.exact_floor(C.div(C.sub(C.add(x, offset), xmin), wpp)),
        lambda y, offset: C.exact_floor(C.div(C.sub(ymax, C.add(y, offset)), hpp)))


def run_ops(sgl, fonts, colors, vp, ops, fonts_table):
    """sgl-tcl.ss: $run-op over the fixture's ops"""
    for op in ops:
        if op[0] == "draw":
            sgl.draw_bang(vp, expand(op[1], fonts_table), *op[2:])
        elif op[0] == "erase":
            sgl.erase_bang(vp, expand(op[1], fonts_table))
        elif op[0] == "send":
            args = [colors.swl_color(a[1]) if isinstance(a, list) and a[0] == "color" else a
                    for a in op[2:]]
            getattr(vp, scheme_to_python(op[1]))(*args)
        else:
            raise ValueError(op)


def make_fonts(fonts, sections):
    return {name: fonts.make_mfont(getattr(fonts, scheme_to_python(face)), size, style)
            for name, face, size, style in sections["fonts"]}


def python_stream(m, name):
    sgl, fonts, colors = m
    sections, viewports = fixture_forms()
    spec = next(v for v in viewports if v[0] == name)
    log = []
    fonts.g_hidden_canvas = RecordingCanvas("hidden", log, colors)
    vp = make_viewport(sgl, RecordingCanvas(name, log, colors), *spec[1:])
    run_ops(sgl, fonts, colors, vp, sections["ops"], make_fonts(fonts, sections))
    return [C.write_string(stream_canon(c, colors, fonts)) for c in log]


@pytest.mark.parametrize("name", ["v1", "v2"])
def test_tcl_stream(m, name):
    expected = (STREAMS / (name + ".txt")).read_text().splitlines()
    got = python_stream(m, name)
    for i, (e, g) in enumerate(zip(expected, got)):
        assert g == e, "command %d differs" % (i + 1)
    assert len(got) == len(expected)


def test_tcl_stream_covers_every_form_and_command():
    """What the frozen stream reaches: every Tk item kind and option the viewport
    methods use, every tag command, the hidden canvas, rational coordinates."""
    text = (STREAMS / "v1.txt").read_text() + (STREAMS / "v2.txt").read_text()
    for kind in ["rectangle", "oval", "arc", "line", "polygon", "text"]:
        assert "create %s " % kind in text, kind
    for opt in ["outline", "fill", "width", "dash", "tags", "style", "start", "extent",
                "anchor", "font", "text", "state"]:
        assert "\\x2D;%s " % opt in text, opt
    for cmd in ["move", "raise", "itemconfigure", "scale", "delete", "bbox"]:
        assert " %s " % cmd in text, cmd
    assert "set-background-color!" in text
    assert "pieslice" in text and "\\x2D;style arc" in text
    assert re.search(r"create text \d+/2 ", text)
    assert '"- "' in text and '". "' in text


def test_tcl_stream_fixture_is_fresh():
    """SOURCES unchanged: the capture's inputs are the ones the stream came from."""
    sys.path.insert(0, str(PY / "oracle"))
    import capture_sgl_tcl
    assert (STREAMS / "SOURCES").read_text() == capture_sgl_tcl.sources()


@pytest.mark.slow
def test_tcl_stream_recapture_is_identical(tmp_path):
    subprocess.run([sys.executable, str(PY / "oracle" / "capture_sgl_tcl.py"), "--out",
                    str(tmp_path)], check=True, capture_output=True)
    names = sorted(p.name for p in STREAMS.iterdir())
    assert sorted(p.name for p in tmp_path.iterdir()) == names
    match, mismatch, errors = filecmp.cmpfiles(STREAMS, tmp_path, names, shallow=False)
    assert mismatch == [] and errors == []


# ---------------------------------------------------------------------------
# Fonts (fonts.ss), on the fake hidden canvas

def test_fonts(m):
    sgl, fonts, colors = m
    assert fonts.select_face(["nosuchface", "helvetica"], ["helvetica", "arial"],
                             "sans-serif") == "helvetica"
    assert fonts.select_face(["a"], [], "serif") == "times"
    assert fonts.select_face(["a"], [], "sans-serif") == "helvetica"
    assert fonts.select_face(["a"], [], "fancy") == "times"
    # with the prelude's families, as in the oracle
    assert (fonts.serif, fonts.sans_serif, fonts.fancy) == ("times", "helvetica", "times")
    log = []
    fonts.g_hidden_canvas = RecordingCanvas("hidden", log, colors)
    f = fonts.make_mfont("times", -20, ["bold"])
    assert tell(f, "object-type") == "mfont"
    assert tell(f, "get-pixel-size", C.String("abc")) == [3 * 14, 27, 5]
    assert tell(f, "get-pixel-width", C.String("ab")) == 28
    assert tell(f, "get-pixel-height") == 27
    assert tell(f, "get-baseline-offset") == 5
    # Tk's `font actual' stands for the request, in points at 96 dpi (as in Racket)
    assert tell(f, "get-actual-values") == ["times", 15, "bold"]
    assert tell(f, "resize", 10) == "done"
    assert tell(f, "get-pixel-size", C.String("M")) == [8, 14, 3]
    # a string face becomes a symbol; a bare style symbol is a one-element style
    g = fonts.make_fixed_font(C.String("courier"), 12, [])
    assert tell(g, "object-type") == "fixed-font"
    assert tell(g, "get-face") == "courier"
    assert isinstance(fonts.swl_font("times", 10, "bold").style, list)
    assert fonts.swl_font("times", 10, "bold", "italic").style == ["bold", "italic"]
    assert fonts.swl_font("times", 10, ["bold"]).style == ["bold"]
    fonts.g_hidden_canvas = False
    with pytest.raises(C.SchemeError):
        tell(f, "get-pixel-size", C.String("x"))


def test_small_fonts_switch(m):
    """make-fixed-font switches to |small fonts| below 7 points when Tk has it."""
    sgl, fonts, colors = m
    saved = fonts.swl_font_families
    try:
        fonts.swl_font_families = lambda: ["small fonts", "times"]
        f = fonts.make_fixed_font("times", 6, ["bold"])
        assert tell(f, "get-swl-font").face == "small fonts"
        assert tell(f, "get-swl-font").style == ["normal"]
        fonts.swl_font_families = lambda: ["times"]
        assert tell(fonts.make_fixed_font("times", 6, ["bold"]), "get-swl-font").face == "times"
    finally:
        fonts.swl_font_families = saved


def test_colors():
    sgl, fonts, colors = mods()
    text = (ORIGINAL / "constants.ss").read_text()
    block = text[text.index("(define *color-names*"):]
    table = scheme_reader.read(block)[2][1]
    assert [[n, r, g, b] for n, r, g, b in colors.g_color_names] == table
    assert len(table) == 752
    assert colors.swl_color(C.String("navy blue")) == colors.Rgb(0, 0, 128)
    assert colors.c_orange == colors.Rgb(255, 165, 0)
    assert colors.c_black == colors.Rgb(0, 0, 0)


def test_tcl_words():
    """How the arguments the original sends reach Tk through tkinter."""
    sgl, fonts, colors = mods()
    swl = importlib.import_module("metacat.gui.swl")
    assert swl.tcl_word(colors.Rgb(255, 0, 16)) == "#ff0010"
    assert swl.tcl_word(Fraction(249, 2)) == 124.5
    assert swl.tcl_word(7) == 7
    assert swl.tcl_word(C.String("dark violet")) == "dark violet"
    assert swl.tcl_word("-outline") == "-outline"
    assert swl.tcl_word(fonts.SwlFont("helvetica", 18, ["bold", "italic"])) == \
        ("helvetica", 18, "bold", "italic")
    assert swl.tcl_word(["a", "b"]) == ("a", "b")


def test_remove_unsupported_tcl_args(m):
    sgl, fonts, colors = m
    args = ["create", "line", 1, 2, "-dash", C.String("- "), "-tags", "t", "-state", "hidden"]
    assert sgl.remove_unsupported_tcl_args(args) == ["create", "line", 1, 2, "-tags", "t"]
    assert sgl.tcl_eval is fonts_swl().swl_tcl_eval     # Tk 8.5 >= 8.3


def fonts_swl():
    return importlib.import_module("metacat.gui.swl")


def test_viewport_mouse_and_resize(m):
    sgl, fonts, colors = m
    log = []
    canvas = RecordingCanvas("vp", log, colors)
    canvas.tcl = lambda *args: (log.append(args), C.String("10.0"))[1]
    vp = make_viewport(sgl, canvas, 100, 100, 0, 0, 200, 100)
    seen = []
    vp.set_mouse_handlers_bang(lambda w, x, y: seen.append(("left", x, y)),
                               lambda w, x, y: seen.append(("right", x, y)))
    vp.mouse_press(5, 6, {"left-button"})
    vp.mouse_press(5, 6, {"right-button"})
    vp.mouse_press(5, 6, {"shift", "left-button"})
    vp.mouse_press(5, 6, {"middle-button"})
    # port: modifier= matches the modifiers exactly, so a control-click is ignored
    vp.mouse_press(5, 6, {"control", "left-button"})
    vp.mouse_press(5, 6, {"control", "right-button"})
    # canvasx/canvasy of 1 is 10.0: x = 2 * (5 + 10), y = 100 - (6 + 10)
    assert seen == [("left", 30, 84), ("right", 30, 84), ("right", 30, 84)]
    # #f leaves a handler as it was (exists?)
    vp.set_mouse_handlers_bang(False, False)
    vp.mouse_press(1, 1, {"left-button"})
    assert seen[3:] == [("left", 22, 89)]
    sizes = []
    vp.set_resize_handler_bang(lambda w, wd, ht: sizes.append((wd, ht)))
    vp.configure(300, 200)
    assert sizes == [(300, 200)]


def test_init_env_font_is_bare(m):
    """anomalies: init-env's default font is a bare SWL font, which draw-text cannot
    tell; the Python port fails as the original does."""
    sgl, fonts, colors = m
    log = []
    vp = make_viewport(sgl, RecordingCanvas("vp", log, colors), 100, 100, 0, 0, 100, 100)
    with pytest.raises(Exception):
        sgl.draw_bang(vp, S('(text "x")'))


# ---------------------------------------------------------------------------
# Structure

GUI = {"sgl": "sgl-interpreter.ss", "fonts": "fonts.ss"}


def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


@pytest.mark.parametrize("name", sorted(GUI))
def test_every_definition_has_its_python_function(name):
    mod = importlib.import_module("metacat.gui." + name)
    missing = [n for n in defines(ORIGINAL / GUI[name])
               if not hasattr(mod, scheme_to_python(n))]
    assert missing == [], missing


@pytest.mark.parametrize("name", sorted(GUI))
def test_docstrings_name_their_origin(name):
    mod = importlib.import_module("metacat.gui." + name)
    for fname, fn in vars(mod).items():
        if (inspect.isfunction(fn) and fn.__module__ == mod.__name__
                and fn.__name__ == fname and not fname.startswith("_")):
            assert fn.__doc__ and fn.__doc__.split(":")[0] in (GUI[name], "port"), fname


def test_viewport_has_every_public_method():
    sgl = importlib.import_module("metacat.gui.sgl")
    text = (ORIGINAL / "sgl-interpreter.ss").read_text()
    public = text[text.index("(public"):text.index("(define generate-polyline-coords")]
    names = re.findall(r"^    \(([a-z!\-]+) \(", public, re.M)
    assert len(names) == 26
    for n in names:
        assert callable(getattr(sgl.Viewport, scheme_to_python(n), None)), n


def test_import_needs_no_display():
    """The interpreter and fonts import (and the stream tests run) without Tk."""
    code = ("import sys; import metacat.gui.sgl, metacat.gui.fonts, metacat.gui.colors;"
            "assert 'tkinter' not in sys.modules, 'tkinter imported'")
    env = dict(os.environ)
    env.pop("DISPLAY", None)
    subprocess.run([sys.executable, "-c", code], cwd=PY, env=env, check=True)


def test_engine_never_imports_gui():
    for path in sorted((PY / "metacat").glob("*.py")):
        tree = ast.parse(path.read_text())
        found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
                 for a in n.names} | {n.module for n in ast.walk(tree)
                                      if isinstance(n, ast.ImportFrom)}
        assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), path


# ---------------------------------------------------------------------------
# Rendering on a real Tk canvas, under Xvfb

@pytest.mark.slow
def test_render_on_tk(tmp_path):
    """render_sgl_fixture.py draws the fixture on a tkinter Canvas under Xvfb, grabs
    the window from the X server and checks pixels (hidden items, raise, delete,
    retag, move, erase, clear, fills)."""
    import render_sgl_fixture
    out = tmp_path / "sgl-fixture.png"
    env = dict(os.environ)
    env.pop("WAYLAND_DISPLAY", None)
    env["GDK_BACKEND"] = "x11"
    proc = subprocess.run(["xvfb-run", "-a", "-s", "-screen 0 1280x1024x24", sys.executable,
                           str(HERE / "render_sgl_fixture.py"), str(out), "--check"],
                          cwd=PY, env=env, capture_output=True, text=True, timeout=120)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert proc.stdout.count("\nok ") == len(render_sgl_fixture.CHECKS)
    assert out.read_bytes()[:8] == b"\x89PNG\r\n\x1a\n"
