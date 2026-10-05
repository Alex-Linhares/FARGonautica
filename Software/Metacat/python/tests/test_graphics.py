"""The engine's part of the graphics files against Chez (loop0002, the graphics
engine): general-graphics.ss's pexp builders and text helpers, group-graphics.ss,
bridge-graphics.ss, rule-graphics.ss and eeg-graphics.ss's EEG object.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/graphics-battery.scm, translated with the battery's fakes
(b:window, b:fake, b:const, b:group, b:bridge ...): CASES maps each test name to a
function that rebuilds the battery expression in Python and returns its value,
whose b:canon text must equal the frozen Chez output in python/fixtures/graphics/
(flonum coordinates to the last bit).  Points (b:pt x y) are
chez.make_rectangular; the battery's + and - signs are chez.add and chez.sub.
The battery's top-level b:set-global! forms (*workspace-window*, the fonts,
=white=) are made by the module fixture, which restores them at the end, as it
does *workspace*, %nice-graphics% and *temperature*.

Beyond the battery: the EEG object against the panels battery's EEG-table and
EEG-recording fixtures (python/fixtures/panels/), every definition of the
engine's part of the five files has its Python name, docstrings name their
origin, the modules never import tkinter or metacat.gui, every graphics name the
model reaches through the package exists, and engine.load() loads the modules.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import re
from fractions import Fraction as F
from pathlib import Path

import pytest

import scheme_reader
from chez_fixtures import chez as fixture, manifest
from name_mapping import ORIGINAL
from scheme_canon import canon
from test_chez import log, with_log          # helpers.scm: log!, with-log
from metacat import chez, engine
from metacat.chez import String
from metacat.names import scheme_to_python
from metacat.objects import Lambda, tell

PY = Path(__file__).resolve().parents[1]

CASES: dict = {}


def case(name):
    def register(fn):
        assert name not in CASES, name
        CASES[name] = fn
        return fn
    return register


def M(name):
    """An engine module, imported at call time (so that collection works before
    the module exists, and the cases fail one by one)."""
    return importlib.import_module("metacat." + name)


def gg():
    return M("general_graphics")


def S(text):
    """Quoted Scheme data (strings become chez.String)."""
    return scheme_reader.read(text)


def P(name):
    """A slipnode or other top-level value by its Scheme name."""
    return chez.top_level_value(name)


def level(name):
    """%proposed%, %evaluated%, %built%"""
    return engine.get_global(name)


PLUS, MINUS = chez.add, chez.sub        # the battery's + and -


def pt(x, y):
    """graphics-battery.scm: b:pt"""
    return chez.make_rectangular(x, y)


def window_fn(self, m, *args):
    """graphics-battery.scm: b:window, a fake Workspace window that logs each
    message and its arguments"""
    log([m] + list(args))
    if m == "get-string-width":
        return chez.mul(F(1, 100), len(args[0]))
    if m == "get-character-width":
        return chez.mul(F(1, 80), len(args[0]))
    if m == "get-character-height":
        return F(1, 30)
    if m == "get-width-per-pixel":
        return F(1, 800)
    if m == "get-rule-coord":
        return [F(3, 4), chez.sub(F(1, 3), chez.mul(F(1, 2), args[1]))]
    if m == "get-spanning-vertical-bridge-right-x":
        return F(3, 10)
    return "done"


WINDOW = Lambda(window_fn)


def const(v):
    """graphics-battery.scm: b:const"""
    return lambda *args: v


def fake(type_, props):
    """graphics-battery.scm: b:fake.  props is an alist of (message, value); a
    procedure is called with the message's arguments (objects are given as
    const(object), since they are procedures too)."""
    def fn(self, m, *args):
        if m == "object-type":
            return type_
        for key, value in props:
            if key == m:
                return value(*args) if callable(value) else value
        log(["unexpected", type_, m])
        return False
    return Lambda(fn)


def logger(tag):
    def fn(p):
        log([tag, p])
        return "done"
    return fn


@pytest.fixture(scope="module", autouse=True)
def battery_globals():
    """The battery's top-level b:set-global! forms, undone afterwards."""
    engine.load()
    names = ["*workspace-window*", "%bridge-label-font%", "%rule-font%", "=white=",
             "*workspace*", "%nice-graphics%", "*temperature*"]
    saved = {n: engine.get_global(n) for n in names}
    engine.set_global("*workspace-window*", WINDOW)
    engine.set_global("%bridge-label-font%", "bridge-label-font")
    engine.set_global("%rule-font%", "rule-font")
    engine.set_global("=white=", "white")
    yield
    for n, v in saved.items():
        engine.set_global(n, v)


# ---------------------------------------------------------------------------
# general-graphics.ss: constants and simple shapes

@case("pi-constants")
def _():
    g = gg()
    return [g.pi, g.radians_per_degree, g.degrees_per_radian, g.p_dot_interval,
            g.p_dash_length, g.p_dash_density]


@case("platform")
def _():
    g = gg()
    return [g.g_tcl_or_tk_version_8_3_p,
            chez.memq(g.g_platform, ["linux", "windows", "macintosh"]),
            engine.get_global("%nice-graphics%")]


@case("circles")
def _():
    g = gg()
    return [g.circle([1, 2], 3), g.disk([F(1, 2), F(1, 3)], 0.25), g.pie_slice([0, 0], 2, 30, 120)]


@case("boxes")
def _():
    g = gg()
    return [g.outline_box(0, 0, 1, 2), g.solid_box(F(1, 3), F(1, 4), 0.5, 0.75)]


@case("ovaloids")
def _():
    g = gg()
    return [g.centered_ovaloid(F(1, 2), F(1, 3), F(1, 5), F(1, 7), F(1, 4)),
            g.filled_centered_ovaloid(0.5, 0.25, 0.1, 0.3, 0.5)]


@case("rounded-boxes")
def _():
    g = gg()
    return [g.centered_rounded_box(F(1, 2), F(1, 3), F(1, 5), F(1, 7), F(1, 100)),
            g.filled_centered_rounded_box(F(1, 2), F(1, 3), F(1, 5), F(1, 7), F(1, 100), F(1, 800)),
            g.centered_rounded_box(0.5, 0.4, 0.2, 0.1, 0.01)]


@case("octagons")
def _():
    g = gg()
    return [g.centered_octagon(F(1, 2), F(1, 3), F(1, 10)), g.filled_centered_octagon(0.25, 0.75, 0.3)]


# ---------------------------------------------------------------------------
# Dotted, dashed, zigzag and jagged lines

@case("dotted-line-horizontal")
def _():
    g = gg()
    return g.dotted_line(F(1, 10), F(1, 3), F(1, 2), F(1, 3), g.p_dot_interval)


@case("dotted-line-vertical")
def _():
    return gg().dotted_line(F(1, 10), F(1, 10), F(1, 10), F(1, 2), F(1, 125))


@case("dotted-line-diagonal")
def _():
    g = gg()
    return g.dotted_line(0, 0, F(1, 2), F(1, 3), g.p_dot_interval)


@case("dotted-line-short")
def _():
    g = gg()
    return g.dotted_line(0.1, 0.2, 0.1001, 0.2, g.p_dot_interval)


@case("dotted-box")
def _():
    g = gg()
    return g.dotted_box(0.2, 0.3, 0.35, 0.4, g.p_dot_interval)


@case("dashed-line-horizontal")
def _():
    g = gg()
    return g.dashed_line(F(1, 10), F(1, 3), F(1, 2), F(1, 3), F(35, 100), g.p_dash_length)


@case("dashed-line-diagonal")
def _():
    g = gg()
    return g.dashed_line(0.6, 0.2, 0.3, 0.45, g.p_dash_density, g.p_dash_length)


@case("dashed-box")
def _():
    return gg().dashed_box(0.2, 0.3, 0.35, 0.4, F(49, 100), F(1, 200))


@case("dashed-line-short")
def _():
    g = gg()
    return [g.dashed_line(0.1, 0.2, 0.11, 0.2, F(1, 2), F(1, 200)),
            g.dashed_line(F(1, 10), F(1, 5), F(1, 10), F(9, 40), F(35, 100), F(1, 200))]


@case("dashed-box-dense")
def _():
    return gg().dashed_box(F(1, 5), F(3, 10), F(7, 20), F(2, 5), F(1, 4), F(1, 200))


@case("zigzag-plus")
def _():
    return gg().zigzag_line(pt(0, 0), pt(F(1, 2), F(1, 3)), F(1, 100), PLUS)


@case("zigzag-minus")
def _():
    return gg().zigzag_line(pt(0.2, 0.5), pt(0.25, 0.1), F(1, 100), MINUS)


@case("zigzag-points-flonum")
def _():
    return gg().zigzag_line_points(pt(0.31, 0.27), pt(0.31, 0.52), 0.01, PLUS)


@case("centered-zigzag-plus")
def _():
    return gg().centered_zigzag_line(pt(0.2, 0.5), pt(0.25, 0.1), F(1, 100), PLUS)


@case("centered-zigzag-minus")
def _():
    return gg().centered_zigzag_line(pt(F(1, 4), F(1, 2)), pt(F(1, 3), F(1, 5)), F(1, 100), MINUS)


@case("jagged")
def _():
    return gg().jagged_line(0, 0, F(1, 2), F(1, 3), F(1, 50))


@case("nice-graphics-off")
def _():
    g = gg()
    engine.set_global("%nice-graphics%", False)
    r = [g.dotted_line(0, 0, F(1, 2), F(1, 3), g.p_dot_interval),
         g.dotted_box(0.2, 0.3, 0.35, 0.4, g.p_dot_interval),
         g.dashed_line(0.6, 0.2, 0.3, 0.45, g.p_dash_density, g.p_dash_length),
         g.dashed_box(0.2, 0.3, 0.35, 0.4, F(49, 100), F(1, 200)),
         g.dotted_circular_arc(0, 0, F(1, 2), 0, F(1, 10), g.p_dot_interval),
         g.dotted_elliptical_arc(0.1, 0.3, 0.5, 0.35, 0.06, g.p_dot_interval)]
    engine.set_global("%nice-graphics%", True)
    return r


# ---------------------------------------------------------------------------
# Arcs and arrows

@case("circular-arcs")
def _():
    g = gg()
    return [g.circular_arc(0, 0, F(1, 2), 0, F(1, 10)), g.circular_arc(0.1, 0.2, 0.4, 0.3, 0.05),
            g.circular_arc(0, 0, 1, 1, 0)]


@case("dotted-circular-arc")
def _():
    g = gg()
    return g.dotted_circular_arc(0.1, 0.2, 0.4, 0.3, 0.05, g.p_dot_interval)


@case("dashed-circular-arc")
def _():
    g = gg()
    return [g.dashed_circular_arc(0.1, 0.2, 0.4, 0.3, 0.05), g.dashed_circular_arc(0, 0, F(1, 2), 0, 0)]


@case("circular-arc-points")
def _():
    return gg().circular_arc_points(0.1, 0.2, 0.4, 0.3, 0.05, F(1, 100))


@case("elliptical-arcs")
def _():
    g = gg()
    return [g.elliptical_arc(0.1, 0.3, 0.5, 0.3, 0.06),
            g.elliptical_arc(0.1, 0.3, 0.5, 0.35, 0.06),
            g.elliptical_arc(0.1, 0.35, 0.5, 0.3, 0.02),
            g.elliptical_arc(F(1, 10), F(3, 10), F(1, 2), F(3, 10), 0),
            g.elliptical_arc(F(1, 10), F(3, 10), F(1, 2), F(3, 10), F(3, 50))]


@case("dotted-elliptical-arcs")
def _():
    g = gg()
    return [g.dotted_elliptical_arc(0.1, 0.3, 0.5, 0.3, 0.06, g.p_dot_interval),
            g.dotted_elliptical_arc(0.1, 0.3, 0.5, 0.35, 0.06, g.p_dot_interval),
            g.dotted_elliptical_arc(0.1, 0.3, 0.5, 0.3, 0, g.p_dot_interval)]


@case("dashed-elliptical-arcs")
def _():
    g = gg()
    return [g.dashed_elliptical_arc(0.1, 0.3, 0.5, 0.3, 0.06),
            g.dashed_elliptical_arc(0.1, 0.35, 0.5, 0.3, 0.06),
            g.dashed_elliptical_arc(0.1, 0.3, 0.5, 0.3, 0)]


@case("elliptical-arc-points")
def _():
    g = gg()
    return [g.elliptical_arc_points(0.1, 0.3, 0.5, 0.3, 0.06, F(1, 125)),
            g.elliptical_arc_points(0.1, 0.35, 0.5, 0.3, 0.02, F(1, 125))]


@case("arrowheads")
def _():
    g = gg()
    return [g.arrowhead(F(1, 2), F(1, 3), 0, F(1, 100), 60), g.arrowhead(0.4, 0.6, 180, F(3, 500), 60),
            g.arrowhead(0.25, 0.5, 45, 0.01, 30)]


@case("double-arrows")
def _():
    g = gg()
    return [g.centered_double_arrow(F(1, 2), 0.45, 0, F(7, 200), F(1, 200), F(3, 200), 60),
            g.centered_double_headed_double_arrow(F(1, 2), F(1, 3), 90, 0.05, 0.01, 0.02, 60)]


# ---------------------------------------------------------------------------
# Text helpers

@case("text-helpers")
def _():
    g = gg()
    return [g.remove_leading_blanks(String("   ab c")), g.remove_leading_blanks(String("")),
            g.remove_leading_blanks(String("    ")),
            g.separate_into_words(String("the quick  brown fox")),
            g.separate_into_words(String("word")),
            g.find_next_space_position(String("ab cd"), 0),
            g.find_next_space_position(String("abcd"), 1)]


@case("break-into-lines")
def _():
    g = gg()
    return with_log(lambda: [
        g.break_into_lines(WINDOW, "font", 0.2, String("the quick brown fox jumps over the lazy dog")),
        g.break_into_lines(WINDOW, "font", 0.05, String("a verylongwordindeed b")),
        g.break_into_lines(WINDOW, "font", 1, String(""))])


# ---------------------------------------------------------------------------
# group-graphics.ss

def group(span, direction, level_, *letcat):
    """graphics-battery.scm: b:group"""
    return fake("group", [
        ("get-graphics-x1", F(1, 10)), ("get-graphics-y1", F(3, 10)),
        ("get-graphics-x2", chez.add(F(1, 10), chez.mul(span, F(1, 20)))),
        ("get-graphics-y2", 0.4),
        ("get-letter-span", span), ("get-direction", const(direction)),
        ("get-proposal-level", level_),
        ("get-letcat-graphics-pexp", False if not letcat else letcat[0])])


@case("group-dashed-line-density")
def _():
    return chez.map_(M("group_graphics").group_dashed_line_density, [1, 2, 3, 4, 5, 6, 8, 10, 12])


@case("group-pexps")
def _():
    gr = M("group_graphics")
    letcat = '(let-sgl ((font f)) (text (1/5 2/5) "A"))'
    argss = [[1, P("plato-right"), level("%proposed%")], [2, P("plato-left"), level("%evaluated%")],
             [3, P("plato-right"), level("%built%")], [1, P("plato-left"), level("%built%")],
             [4, False, level("%built%")], [5, False, level("%evaluated%")],
             [2, False, level("%built%"), S(letcat)],
             [2, False, level("%proposed%"), S(letcat)]]
    return chez.map_(lambda args: gr.make_group_pexp(group(*args), args[2]), argss)


@case("group-grope")
def _():
    gr = M("group_graphics")
    return with_log(lambda: gr.draw_group_grope(group(3, P("plato-right"), level("%proposed%"))))


def graphics_group(level_, drawn_p, drawn_coincident, overlapping, highest):
    """graphics-battery.scm: b:graphics-group"""
    return fake("group", [
        ("get-graphics-x1", 0.1), ("get-graphics-y1", 0.3),
        ("get-graphics-x2", 0.25), ("get-graphics-y2", 0.4),
        ("get-letter-span", 3), ("get-direction", const(P("plato-right"))),
        ("get-proposal-level", level_), ("get-letcat-graphics-pexp", False),
        ("drawn?", drawn_p), ("get-graphics-pexp", ["old-pexp"]),
        ("get-drawn-coincident-group", const(drawn_coincident)),
        ("get-drawn-overlapping-groups", const(overlapping)),
        ("get-highest-level-coincident-group", const(highest)),
        ("set-graphics-pexp", logger("set-graphics-pexp"))])


@case("group-graphics-ops")
def _():
    gr = M("group_graphics")
    pr, ev, bu = level("%proposed%"), level("%evaluated%"), level("%built%")
    other = graphics_group(ev, True, False, [], False)
    ops = ["flash", "flash", "flash", "set-pexp-and-draw", "set-pexp-and-draw", "erase", "erase",
           "update-level", "update-level", "update-level", "update-level"]
    groups = [graphics_group(pr, True, False, [], False),
              graphics_group(ev, False, other, [], False),
              graphics_group(pr, False, other, [], False),
              graphics_group(pr, False, False, [], False),
              graphics_group(pr, False, other, [], False),
              graphics_group(bu, True, False, [], False),
              graphics_group(pr, False, False, [other, other], other),
              graphics_group(ev, True, False, [], False),
              graphics_group(ev, False, False, [], False),
              graphics_group(bu, False, other, [], False),
              graphics_group(pr, False, other, [], False)]
    return chez.map_(lambda op, g: with_log(lambda: gr.group_graphics(op, g)), ops, groups)


# ---------------------------------------------------------------------------
# bridge-graphics.ss

def _coord(h, v):
    return lambda o: h if o == "horizontal" else v


LETTER = fake("letter", [("get-graphics-pexp", ["letter-pexp"]),
                         ("string-spanning-group?", False),
                         ("get-bridge-graphics-coord", _coord(pt(F(1, 10), F(2, 5)), pt(F(1, 10), 0.38)))])
LETTER2 = fake("letter", [("get-graphics-pexp", ["letter2-pexp"]),
                          ("string-spanning-group?", False),
                          ("get-bridge-graphics-coord", _coord(pt(0.3, 0.41), pt(0.12, 0.1)))])


def spanning(x, y):
    """graphics-battery.scm: b:spanning"""
    return fake("group", [("string-spanning-group?", True),
                          ("get-group-spanning-bridge-graphics-coord", lambda o: pt(x, y))])


def bridge(orientation, from_, to, spanning_p, obj1, obj2, *more):
    """graphics-battery.scm: b:bridge"""
    return fake("bridge", list(more) + [
        ("get-orientation", orientation),
        ("get-from-graphics-coord", from_), ("get-to-graphics-coord", to),
        ("group-spanning-bridge?", spanning_p),
        ("get-object1", const(obj1)), ("get-object2", const(obj2)),
        ("get-bridge-label-number", 2)])


LEVELS = ("%proposed%", "%evaluated%", "%built%")


@case("horizontal-bridge-pexps")
def _():
    br = M("bridge_graphics")
    g = fake("group", [])

    def each(lv):
        return [br.make_bridge_pexp(bridge("horizontal", pt(F(1, 10), F(2, 5)), pt(F(3, 10), 0.41), False,
                                           LETTER, LETTER2), lv),
                br.make_bridge_pexp(bridge("horizontal", pt(0.1, 0.4), pt(0.3, 0.4), False,
                                           g, LETTER2), lv),
                br.make_bridge_pexp(bridge("horizontal", pt(0.05, 0.45), pt(0.6, 0.47), True,
                                           g, g), lv),
                br.make_bridge_pexp(bridge("horizontal", pt(0.05, 0.47), pt(0.6, 0.45), True,
                                           g, g), lv)]
    return chez.map_(each, [level(n) for n in LEVELS])


@case("vertical-bridge-pexps")
def _():
    br = M("bridge_graphics")

    def each(lv):
        return with_log(lambda: [
            br.make_bridge_pexp(bridge("vertical", pt(F(1, 10), 0.38), pt(0.12, 0.1), False,
                                       LETTER, LETTER2), lv),
            br.make_bridge_pexp(bridge("vertical", pt(0.2, 0.38), pt(0.15, 0.1), False,
                                       LETTER, LETTER2), lv),
            br.make_bridge_pexp(bridge("vertical", pt(0.25, 0.4), pt(0.26, 0.12), True,
                                       LETTER, LETTER2), lv)])
    return chez.map_(each, [level(n) for n in LEVELS])


@case("bridge-gropes")
def _():
    br = M("bridge_graphics")
    return with_log(lambda: [
        br.draw_bridge_grope("horizontal", LETTER, LETTER2),
        br.draw_bridge_grope("vertical", LETTER, LETTER2),
        br.draw_bridge_grope("horizontal", spanning(0.05, 0.45), spanning(0.6, 0.47)),
        br.draw_bridge_grope("vertical", spanning(0.25, 0.4), spanning(0.26, 0.12))])


def graphics_bridge(level_, drawn_p, drawn_coincident, flipped_p, highest, obj2):
    """graphics-battery.scm: b:graphics-bridge"""
    return bridge("horizontal", pt(0.1, 0.4), pt(0.3, 0.41), False, LETTER, obj2,
                  ("get-proposal-level", level_), ("drawn?", drawn_p),
                  ("get-graphics-pexp", ["old-bridge-pexp"]),
                  ("get-drawn-coincident-bridge", const(drawn_coincident)),
                  ("flipped-group1?", flipped_p), ("flipped-group2?", flipped_p),
                  ("get-original-group1", "original-group1-not-drawable"),
                  ("get-original-group2", const(fake("group", [("get-graphics-pexp", ["og2"])]))),
                  ("get-highest-level-coincident-bridge", const(highest)),
                  ("set-graphics-pexp", logger("set-graphics-pexp")))


@case("bridge-graphics-ops")
def _():
    br = M("bridge_graphics")
    pr, ev, bu = level("%proposed%"), level("%evaluated%"), level("%built%")
    other = graphics_bridge(ev, True, False, False, False, LETTER2)
    g = fake("group", [("get-graphics-pexp", ["g-pexp"])])
    ops = ["flash", "flash", "flash", "set-pexp-and-draw", "set-pexp-and-draw", "erase", "erase",
           "erase", "update-level", "update-level", "update-level", "update-level"]
    bridges = [graphics_bridge(pr, True, False, False, False, LETTER2),
               graphics_bridge(ev, False, other, False, False, LETTER2),
               graphics_bridge(pr, False, other, False, False, LETTER2),
               graphics_bridge(pr, False, False, False, False, LETTER2),
               graphics_bridge(pr, False, other, False, False, LETTER2),
               graphics_bridge(bu, True, False, False, False, LETTER2),
               graphics_bridge(bu, False, False, False, other, g),
               graphics_bridge(bu, False, False, False, False, LETTER2),
               graphics_bridge(ev, True, False, False, False, LETTER2),
               graphics_bridge(ev, False, False, False, False, LETTER2),
               graphics_bridge(bu, False, other, False, False, LETTER2),
               graphics_bridge(pr, False, other, False, False, LETTER2)]
    return chez.map_(lambda op, b: with_log(lambda: br.bridge_graphics(op, b)), ops, bridges)


# ---------------------------------------------------------------------------
# rule-graphics.ss

def rule(type_, clauses):
    """graphics-battery.scm: b:rule"""
    return fake("rule", [
        ("get-english-transcription", clauses), ("get-rule-type", type_),
        ("set-graphics-pexp", logger("set-graphics-pexp")),
        ("set-auxiliary-rule-graphics-info", lambda *args: (log(["aux"] + list(args)), "done")[1])])


@case("rule-graphics")
def _():
    rg = M("rule_graphics")
    rules = [rule("top", [String("Replace letter-category of rightmost letter by successor")]),
             rule("bottom", [String("Replace letter-category of rightmost group"),
                             String("by successor"), String("and swap length")]),
             rule("top", [String("Don't replace anything")])]
    return chez.map_(lambda r: with_log(lambda: rg.initialize_rule_graphics(r)), rules)


@case("new-rule-pexps")
def _():
    rg = M("rule_graphics")
    return with_log(lambda: [rg.make_new_rule_pexp("top", [String("Replace C by D")]),
                             rg.make_new_rule_pexp("bottom", [String("one"), String("two three")])])


@case("update-rule-pexps")
def _():
    rg = M("rule_graphics")
    p = ["let-sgl", [],
         ["rule", "top", [String("Replace C by D")], ["old"]],
         ["erase", "white", ["let-sgl", [["font", "f"]],
                             ["rule", "bottom", [String("a"), String("b")], ["old2"]]]],
         ["text", [0, 0], String("unchanged")]]
    r = rg.update_rule_pexps_bang(p)
    # the original updates p in place (set-car!); Racket's returns an updated copy
    return r if isinstance(r, list) and r and r[0] == "let-sgl" else p


@case("bridge-label-numbers")
def _():
    br = M("bridge_graphics")

    def numbered(n):
        return fake("bridge", [("get-bridge-label-number", n), ("get-bridge-type", "vertical")])
    b1, b3, b2, new = numbered(1), numbered(3), numbered(2), numbered(False)
    real_workspace = engine.get_global("*workspace*")

    def each(bridges):
        engine.set_global("*workspace*",
                          Lambda(lambda self, m, *args: bridges if m == "get-bridges" else "done"))
        return br.new_bridge_label_number(new)
    results = chez.map_(each, [[], [new], [b1, new], [b3, b1, new], [b2, b1, b3, new], [b3]])
    engine.set_global("*workspace*", real_workspace)
    return results


GRAPHICS_TESTS = list(manifest("graphics"))


def test_every_battery_test_is_translated():
    assert sorted(CASES) == sorted(GRAPHICS_TESTS)
    assert len(CASES) == 48


def first_difference(got, expected):
    i = next((k for k, (a, b) in enumerate(zip(got, expected)) if a != b), min(len(got), len(expected)))
    return (f"differs at character {i} of {len(expected)}:\n"
            f"  python: ...{got[max(0, i - 150):i + 150]}\n  chez:   ...{expected[max(0, i - 150):i + 150]}")


@pytest.mark.parametrize("name", GRAPHICS_TESTS)
def test_graphics_battery(name):
    expected = fixture("graphics", name)
    got = canon(CASES[name]())
    same = got == expected
    assert same, first_difference(got, expected)


def test_update_rule_pexps_is_in_place():
    """rule-graphics.ss's set-car!: the pexp the Workspace window holds is the one
    updated, as in the original (anomalies: shared rule pexps)."""
    rg = M("rule_graphics")
    inner = ["rule", "top", [String("Replace C by D")], ["old"]]
    p = ["let-sgl", [], ["erase", "white", inner]]
    rg.update_rule_pexps_bang(p)
    assert p[2][2] is inner and inner[3] != ["old"] and inner[3][0] == "let-sgl"


# ---------------------------------------------------------------------------
# chez.py's complex arithmetic, which the line builders rely on.  The battery's
# coordinates do not reach every rule, so these are values printed by Chez 10
# (`scheme --script` on the same expressions; the magnitudes are three of the
# 36 arguments, out of 20,000 random ones, where Python's math.hypot differs
# from Chez's magnitude, which is libm's hypot).

def test_complex_arithmetic_as_chez():
    mr = chez.make_rectangular
    assert chez.write_string([
        chez.magnitude(mr(0.8455286293882489, -0.5842277562918272)),
        chez.magnitude(mr(6.784814197066562e-4, -3.367961553872014e-4)),
        chez.magnitude(mr(-0.019587380004819632, -0.00684343012715365)),
        [chez.angle(0.5), chez.angle(-0.0), chez.angle(F(-1, 2)), chez.angle(mr(F(1, 2), F(1, 3)))],
        [chez.make_polar(F(1, 2), 0), chez.make_polar(F(1, 2), 0.0), chez.cos(0), chez.sin(0),
         chez.tan(0), chez.acos(1), chez.acos(1.0)],
        [chez.sub(0.5, mr(0.1, 0.0)), chez.add(F(1, 2), mr(0.1, -0.0)), chez.mul(0, mr(0.1, 0.3)),
         chez.mul(0.0, mr(0.1, 0.3)), chez.div(mr(F(1, 10), F(3, 10)), 10),
         chez.sub(mr(F(1, 2), F(1, 3)), mr(F(1, 2), F(1, 3))), chez.sub(mr(F(1, 2), F(1, 3)))]]) == (
        "(1.027735731760336 7.574752056475247e-4 0.02074844551667527"
        " (0 3.141592653589793 3.141592653589793 0.5880026035475675)"
        " (1/2 0.5+0.0i 1 0 0 0 0.0)"
        " (0.4-0.0i 0.6-0.0i 0 0.0+0.0i 1/100+3/100i 0 -1/2-1/3i))")
    assert chez.angle(0.5) == 0 and type(chez.angle(0.5)) is int
    with pytest.raises(chez.SchemeError):
        chez.angle(0)


# ---------------------------------------------------------------------------
# eeg-graphics.ss's EEG object, against panels-battery.scm's EEG tests

def test_eeg_table():
    eeg = M("eeg_graphics")
    got = canon([entry[:5] for entry in eeg.p_EEG_table])
    assert got == fixture("panels", "EEG-table")
    assert eeg.p_EEG_buffer_size == 40


def test_eeg_recording():
    """panels-battery.scm: EEG-recording (47 recordings of a fake Workspace)"""
    eeg = M("eeg_graphics")
    activity = [100]
    engine.set_global("*workspace*",
                      fake("workspace", [("get-activity", lambda: activity[0])]))
    EEG = eeg.g_EEG
    tell(EEG, "initialize")
    acc = []
    for i in range(47):
        activity[0] = chez.sub(100, 3 * i, F(1, 3) if i % 2 == 1 else 0)
        engine.set_global("*temperature*", 4 * i if i < 20 else 100 - i)
        tell(EEG, "record-current-values")
        acc.append([tell(EEG, "get-current-values"), tell(EEG, "get-current-value", 1),
                    tell(EEG, "get-average-value", 2, 3)])
    got = canon([acc, tell(EEG, "get-previous-values", 0), tell(EEG, "get-previous-values", 2, 5),
                 tell(EEG, "get-average-value", 0), tell(EEG, "get-max-variation", 0),
                 tell(EEG, "get-max-variation", 2, 8)])
    assert got == fixture("panels", "EEG-recording"), first_difference(got, fixture("panels", "EEG-recording"))
    assert tell(EEG, "object-type") == "EEG"


# ---------------------------------------------------------------------------
# Structure

def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


# general-graphics.ss's window part (gui/general_graphics.py) and eeg-graphics.ss's
# window part (gui/), as racket/engine/*.rktl split them
WINDOW_PART = {
    "general_graphics": {"%default-fg-color%", "%default-bg-color%",
                         "%default-scrollable-text-window-font%", "*fg-color*",
                         "toplevel-destroy-action", "%resize-listener-pause%",
                         "*resize-message-queue*", "start-resize-listener",
                         "make-scrollable-graphics-window", "make-unscrollable-graphics-window",
                         "make-horizontal-scrollable-graphics-window",
                         "make-vertical-scrollable-graphics-window", "make-graphics-window",
                         "get-scrollbar-from-frame", "make-scrollable-text-window"},
    "eeg_graphics": {"%max-EEG-window-cycles%", "%EEG-title-font%", "select-EEG-font",
                     "make-EEG-window", "new-EEG-window"},
}
ORIGINS = {"general_graphics": "general-graphics.ss", "group_graphics": "group-graphics.ss",
           "bridge_graphics": "bridge-graphics.ss", "rule_graphics": "rule-graphics.ss",
           "eeg_graphics": "eeg-graphics.ss"}


@pytest.mark.parametrize("name", sorted(ORIGINS))
def test_every_definition_has_its_python_name(name):
    mod = M(name)
    wanted = [n for n in defines(ORIGINAL / ORIGINS[name]) if n not in WINDOW_PART.get(name, ())]
    if name == "general_graphics":     # metacat.ss's, as racket/engine/general-graphics.rktl
        wanted += ["*platform*", "*tcl/tk-version*", "*tcl/tk-version-8_3?*"]
    missing = [n for n in wanted if not hasattr(mod, scheme_to_python(n))]
    assert missing == [], (name, missing)


def test_window_parts_are_not_in_the_engine():
    for name, part in WINDOW_PART.items():
        mod = M(name)
        assert [n for n in part if hasattr(mod, scheme_to_python(n))] == [], name


@pytest.mark.parametrize("name", sorted(ORIGINS))
def test_docstrings_name_their_origin(name):
    mod = M(name)
    for fname, fn in vars(mod).items():
        if (inspect.isfunction(fn) and fn.__module__ == mod.__name__
                and not fname.startswith("_")):
            assert fn.__doc__ and fn.__doc__.split(":")[0] in (ORIGINS[name], "port"), (name, fname)


@pytest.mark.parametrize("name", sorted(ORIGINS))
def test_engine_graphics_import_no_gui(name):
    tree = ast.parse(inspect.getsource(M(name)))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), (name, found)


def test_every_graphics_name_the_model_reaches_exists():
    """The model calls the graphics through the package (_metacat.bridge_graphics...)."""
    refs = set()
    for path in sorted((PY / "metacat").glob("*.py")):
        refs |= set(re.findall(r"_metacat\.(\w+_graphics)\.(\w+)", path.read_text()))
    assert len(refs) >= 12
    missing = [(m, n) for m, n in sorted(refs) if not hasattr(M(m), n)]
    assert missing == []


def test_engine_load_loads_them():
    loaded = {m.__name__ for m in engine.translated_modules()}
    for name in ORIGINS:
        assert "metacat." + name in loaded, name
