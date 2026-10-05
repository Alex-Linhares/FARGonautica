"""The panels of item 14 against Chez (loop0002 item 15): tests/diff/panels-battery.scm.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
The counterpart of racket/tests/panels-diff-test.rkt.

Every test of tests/diff/panels-battery.scm, translated, in the battery's order and
in one engine, with the battery's top-level forms between them (the fake
Themespace and theme cluster, the panel made on the fake window b:g, the fake
events, groups and answers, b:activity).  The battery's code runs on the engine
(metacat/*.py: relation-name, group-event-pexp-text-string, the EEG object and the
pexp builders) and the views (metacat/gui/*_graphics.py), loaded by
metacat.gui.views.load_views() with the offscreen fonts.  Colours and fonts are
SWL stubs under Chez and gui.colors.Rgb / gui.fonts objects here, so b:clean
turns every non-datum into 'obj, as in the battery.  The value of each test,
printed by scheme_canon.canon (helpers.scm's b:canon), must equal the frozen Chez
output in python/fixtures/panels/.

`run_through(name)` evaluates the battery's forms up to and including the test
NAME, once each, so a single test can run alone (pytest -k) and still see the
state the forms before it left.  Every engine global the battery changes is
restored by the module fixture.
"""
from __future__ import annotations

import importlib
from fractions import Fraction as F
from types import SimpleNamespace

import pytest

from chez_fixtures import chez as fixture, manifest
from scheme_canon import canon
from metacat import chez, engine
from metacat.chez import String
from metacat.names import scheme_to_python
from metacat.objects import Lambda, tell

BATTERY = "panels"


def M(name):
    """A module, imported at call time (so that collection works before the
    modules of this item exist, and the cases fail one by one)."""
    return importlib.import_module("metacat." + name)


def V(module, scheme_name):
    """A top-level value of a graphics file, by its Scheme name."""
    return getattr(M(module), scheme_to_python(scheme_name))


def P(name):
    """A slipnode by its Scheme name (the battery refers to them as globals)."""
    return chez.top_level_value(name)


set_global = engine.set_global          # b:set-global!


# helpers.scm and the battery's helpers ----------------------------------------------

B = SimpleNamespace(log=[])


def log_bang(x):
    """helpers.scm: log!"""
    B.log.append(x)
    return x


def with_log(thunk):
    """helpers.scm: with-log"""
    B.log = []
    r = thunk()
    return [r, list(B.log)]


def b_clean(x):
    """panels-battery.scm: b:clean.  Numbers, strings, symbols, booleans and '()
    are kept, pairs cleaned, anything else (colours, fonts, objects, void,
    characters, vectors) is 'obj."""
    if x is True or x is False:
        return x
    if isinstance(x, chez.Char):
        return "obj"
    if isinstance(x, (str, int, float, F, complex, chez.ExactComplex)):
        return x
    if isinstance(x, (list, tuple)) and not isinstance(x, chez.Vector):
        return [b_clean(e) for e in x]
    if isinstance(x, chez.Pair):
        return chez.Pair(b_clean(x.car), b_clean(x.cdr))
    return "obj"


def b_with_log(thunk):
    """panels-battery.scm: b:with-log"""
    return b_clean(with_log(thunk))


def b_const(v):
    """panels-battery.scm: b:const"""
    return lambda *args: v


def b_fake(type_, props):
    """panels-battery.scm: b:fake.  props: (message, value-or-procedure) pairs;
    a procedure is called with the message's arguments; other messages are
    logged and answer 'done."""
    props = list(props)

    def fn(self, m, *args):
        if m == "object-type":
            return type_
        for key, value in props:          # assq: the first match
            if key == m:
                return value(*args) if callable(value) else value
        log_bang(b_clean([type_, m, *args]))
        return "done"
    return Lambda(fn)


def _g(self, m, *args):
    """panels-battery.scm: b:g, a fake graphics window"""
    log_bang(b_clean([m, *args]))
    if m == "get-string-width":
        return chez.mul(F(1, 8), len(args[0]))
    if m == "get-string-height":
        return F(1, 2)
    if m == "get-character-height":
        return F(3, 50)
    if m == "get-text-offset":
        return F(1, 80)
    if m == "get-width-per-pixel":
        return F(1, 600)
    if m == "get-height-per-pixel":
        return F(1, 500)
    return "done"


b_g = Lambda(_g)


def b_name(node):
    """panels-battery.scm: b:name"""
    return tell(node, "get-short-name") if node is not False else False


def S(text):
    return String(text)


# The battery, form by form ----------------------------------------------------------

FORMS = []          # ("test", name, thunk) or ("form", None, thunk), in the battery's order
CASES = {}


def battery_test(name):
    def register(fn):
        assert name not in CASES, name
        CASES[name] = fn
        FORMS.append(("test", name, fn))
        return fn
    return register


def form(fn):
    FORMS.append(("form", None, fn))
    return fn


# slipnet-graphics.ss

@battery_test("slipnet-layout-table")
def _():
    table = V("gui.slipnet_graphics", "*13x5-layout-table*")
    return [[b_name(node) for node in row] for row in table]


@battery_test("slipnet-layout-dimensions")
def _():
    u = M("utilities")
    table = V("gui.slipnet_graphics", "*13x5-layout-table*")
    return [u.row_dimension(table), u.column_dimension(table)]


# temperature-graphics.ss

@battery_test("mercury-pexp")
def _():
    mercury_pexp = V("gui.temperature_graphics", "mercury-pexp")
    white = V("gui.constants", "=white=")
    return [b_clean(mercury_pexp(F(1, 2), F(1, 2), F(1, 2), F(3, 4), "red")),
            b_clean(mercury_pexp(F(9, 20), F(1, 2), F(11, 20), 0.875, "red")),
            b_clean(mercury_pexp(0.45, 0.5, 0.55, 1.95, white))]


@battery_test("draw-thermometer")
def _():
    draw_thermometer = V("gui.temperature_graphics", "draw-thermometer")
    return b_with_log(lambda: draw_thermometer(b_g, [F(1, 2), F(3, 10)], F(3, 10)))


@battery_test("draw-thermometer-off-centre")
def _():
    draw_thermometer = V("gui.temperature_graphics", "draw-thermometer")
    return b_with_log(lambda: draw_thermometer(b_g, [0.6, 0.25], 0.2))


# theme-graphics.ss: names, orders, layout

def TG(name):
    return V("gui.theme_graphics", name)


def relation_name(r):
    return M("theme_graphics").relation_name(r)


def themespace():
    return engine.get_global("*themespace*")


@battery_test("themespace-window-layout")
def _():
    return TG("*themespace-window-layout*")


@battery_test("panel-order")
def _():
    return [b_name(n) for n in TG("*panel-order*")]


@battery_test("panel-theme-order")
def _():
    return [b_name(n) for n in TG("*panel-theme-order*")]


@battery_test("relation-names")
def _():
    return [relation_name(r) for r in
            [False, P("plato-identity"), P("plato-opposite"), P("plato-successor"),
             P("plato-predecessor"), P("plato-a")]]


@battery_test("dimension-names")
def _():
    return [[TG("dimension-name")(d), TG("abbreviated-dimension-name")(d)]
            for d in list(TG("*panel-order*")) + [P("plato-length"), P("plato-letter")]]


@battery_test("sort-wrt-panel-order")
def _():
    u = M("utilities")
    return [b_name(n) for n in
            u.sort_wrt_order(chez.remq(P("plato-bond-category"),
                                       tell(themespace(), "get-dimensions")),
                             TG("*panel-order*"))]


@battery_test("themespace-relations")
def _():
    u = M("utilities")

    def for_type(type_):
        return chez.map_(
            lambda dim: [b_name(n) for n in
                         u.sort_wrt_order(tell(themespace(), "get-relations", type_, dim),
                                          TG("*panel-theme-order*"))],
            chez.remq(P("plato-bond-category"), tell(themespace(), "get-dimensions")))
    return chez.map_(for_type, ["top-bridge", "bottom-bridge", "vertical-bridge"])


@form
def _():
    B.relation_sets = [
        [P("plato-identity")],
        [P("plato-identity"), P("plato-successor")],
        [P("plato-identity"), P("plato-successor"), P("plato-predecessor"), False],
        [P("plato-identity"), P("plato-opposite"), False]]


def named_entries(info):
    return [[b_name(entry[0])] + list(entry[1:]) for entry in info]


@battery_test("horizontal-panel-info")
def _():
    compute = TG("compute-horizontal-panel-info")(
        F(1, 4), F(7, 30), F(9, 100), F(1, 80), F(1, 60), F(3, 50), F(1, 25), F(1, 100))
    return chez.map_(lambda rs: named_entries(compute(rs, [F(1, 2), F(7, 30)])),
                     B.relation_sets)


@battery_test("horizontal-panel-info-flonum")
def _():
    compute = TG("compute-horizontal-panel-info")(
        0.25, 0.2333, 0.09, 0.0125, 0.0166, 0.06, 0.04, 0.01)
    return chez.map_(lambda rs: named_entries(compute(rs, [0.0, 0.0])), B.relation_sets)


@battery_test("vertical-panel-info")
def _():
    compute = TG("compute-vertical-panel-info")(
        F(1, 2), F(59, 32), F(1, 5), 0, F(1, 60), F(3, 50), F(1, 25), F(1, 100),
        lambda r: chez.mul(F(1, 90), len(relation_name(r))))
    return chez.map_(lambda rs: named_entries(compute(rs, [F(1, 2), F(59, 16)])),
                     B.relation_sets)


# theme-graphics.ss: a panel on a fake window, with a fake Themespace

@form
def _():
    B.dominant = False
    B.cluster_themes = []
    B.cluster = b_fake("theme-cluster",
                       [("get-dominant-theme", lambda: B.dominant),
                        ("get-themes", lambda: B.cluster_themes)])
    B.pressure_p = False
    set_global("*themespace*",
               b_fake("themespace",
                      [("get-cluster", lambda type_, dim: B.cluster),
                       ("thematic-pressure?", lambda type_: B.pressure_p)]))


def b_make_theme(relation, activation):
    """panels-battery.scm: b:make-theme"""
    return b_fake("theme",
                  [("get-relation", b_const(relation)),
                   ("get-activation", b_const(activation)),
                   ("get-normal-pexp", b_const(["normal", relation_name(relation)])),
                   ("get-highlight-pexp", b_const(["highlight", relation_name(relation)]))])


@form
def _():
    B.iden = b_make_theme(P("plato-identity"), 80)
    B.succ = b_make_theme(P("plato-successor"), -40)
    B.diff = b_make_theme(False, 100)


@form
def _():
    B.panel_info = TG("compute-horizontal-panel-info")(
        F(1, 4), F(7, 30), F(9, 100), F(1, 80), F(1, 60), F(3, 50), F(1, 25), F(1, 100))(
        [P("plato-identity"), P("plato-successor"), False], [F(1, 4), 0])


@form
def _():
    B.panel = TG("make-panel")(
        b_g, P("plato-letter-category"), "top-bridge", B.panel_info, S("Letter Category"),
        [F(3, 8), F(1, 40)], [F(1, 4), 0], [F(1, 2), F(7, 30)], [F(1, 2), 0], [F(1, 4), F(7, 30)],
        F(9, 100), F(1, 10),
        "dim-normal", "dim-highlight", "rel-normal", "rel-highlight")


@battery_test("panel-initialize")
def _():
    B.cluster_themes = [B.iden, B.succ]
    return b_with_log(lambda: tell(B.panel, "initialize"))


@battery_test("panel-pexps")
def _():
    return b_clean([tell(B.panel, "get-normal-panel-pexp"),
                    tell(B.panel, "get-highlighted-panel-pexp"),
                    [b_name(r) for r in tell(B.panel, "get-relations")]])


@battery_test("panel-relations")
def _():
    def thunk():
        tell(B.panel, "add-relation", False)
        tell(B.panel, "remove-relation", P("plato-successor"))
        return [[b_name(r) for r in tell(B.panel, "get-relations")],
                tell(B.panel, "get-normal-panel-pexp")]
    return b_with_log(thunk)


@battery_test("panel-no-relations")
def _():
    def thunk():
        tell(B.panel, "delete-all-relations")
        return [tell(B.panel, "get-normal-panel-pexp"),
                tell(B.panel, "get-highlighted-panel-pexp")]
    return b_with_log(thunk)


@battery_test("panel-set-theme-graphics-parameters")
def _():
    def thunk():
        tell(B.panel, "set-theme-graphics-parameters", B.iden)
        return tell(B.panel, "set-theme-graphics-parameters", B.diff)
    return b_with_log(thunk)


@battery_test("panel-draw-dominant")
def _():
    B.cluster_themes = [B.iden, B.succ, B.diff]
    tell(B.panel, "add-relation", P("plato-identity"))
    tell(B.panel, "add-relation", P("plato-successor"))
    B.dominant = B.iden

    def thunk():
        tell(B.panel, "draw-panel")
        return tell(B.panel, "get-highlighted-theme")
    return b_with_log(thunk)


@battery_test("panel-update-same-dominant")
def _():
    return b_with_log(lambda: tell(B.panel, "update-graphics"))


@battery_test("panel-update-new-dominant")
def _():
    B.dominant = B.succ
    return b_with_log(lambda: tell(B.panel, "update-graphics"))


@battery_test("panel-update-no-dominant")
def _():
    B.dominant = False
    return b_with_log(lambda: tell(B.panel, "update-graphics"))


@battery_test("panel-activations")
def _():
    vg = M("view_globals")

    def thunk():
        tell(B.panel, "set-highlight-color", "yellow")
        tell(B.panel, "draw-absolute-activation", [F(1, 3), F(1, 10)], -60)
        tell(B.panel, "draw-absolute-activation", [F(1, 3), F(1, 10)], 25)
        B.pressure_p = True
        tell(B.panel, "decrease-absolute-activation", [F(1, 3), F(1, 10)], -30)
        B.pressure_p = False
        tell(B.panel, "decrease-absolute-activation", [F(1, 3), F(1, 10)], 30)
        tell(B.panel, "erase-activation", [F(1, 3), F(1, 10)])
        B.dominant = B.iden
        tell(B.panel, "draw-panel")
        tell(B.panel, "decrease-absolute-activation", [F(1, 3), F(1, 10)], 70)
        tell(B.panel, "erase-activation", [F(1, 3), F(1, 10)])
        return [vg.g_fg_color is vg.p_default_fg_color,
                tell(B.panel, "dimension?", P("plato-letter-category")),
                tell(B.panel, "dimension?", P("plato-length")),
                tell(B.panel, "get-activation-diameter")]
    return b_with_log(thunk)


# trace-graphics.ss: event icons on a fake window

def b_letter(descriptor):
    """panels-battery.scm: b:letter"""
    return b_fake("letter", [("get-descriptor-for", lambda facet: descriptor)])


def b_group_of(direction, facet, objects):
    """panels-battery.scm: b:group-of"""
    return b_fake("group",
                  [("get-direction", b_const(direction)),
                   ("get-bond-facet", b_const(facet)),
                   ("get-constituent-objects", b_const(objects)),
                   ("get-descriptor-for", lambda f: P("plato-c"))])


@form
def _():
    B.groups = [
        b_group_of(P("plato-right"), P("plato-letter-category"),
                   [b_letter(P("plato-a")), b_letter(P("plato-b")), b_letter(P("plato-c"))]),
        b_group_of(P("plato-left"), P("plato-letter-category"),
                   [b_letter(P("plato-x")), b_letter(P("plato-y"))]),
        b_group_of(False, P("plato-length"),
                   [b_letter(P("plato-one")), b_letter(P("plato-two")),
                    b_group_of(False, P("plato-letter-category"), [])])]


@form
def _():
    def type_(t):
        return ("get-type", b_const(t))
    B.events = (
        [b_fake("answer-event",
                [type_("answer"),
                 ("get-answer-string",
                  b_const(b_fake("workspace-string", [("print-name", S("xyd"))])))])]
        + [b_fake("clamp-event", [type_("clamp")])]
        + chez.map_(lambda node: b_fake("concept-activation-event",
                                        [type_("concept-activation"),
                                         ("get-slipnode", b_const(node))]),
                    [P("plato-opposite"), P("plato-a"), P("plato-alphabetic-position-category")])
        + chez.map_(lambda group: b_fake("group-event",
                                         [type_("group"), ("get-group", b_const(group))]),
                    B.groups)
        + chez.map_(lambda t: b_fake("rule-event",
                                     [type_("rule"), ("get-rule-type", b_const(t))]),
                    ["top", "bottom"])
        + [b_fake("concept-mapping-event",
                  [type_("concept-mapping"),
                   ("get-concept-mapping",
                    b_const(b_fake("concept-mapping", [("print-name", S("rmost=>lmost"))])))])]
        + [b_fake("snag-event", [type_("snag")])])


def b_event_pexp_info(event):
    """panels-battery.scm: b:event-pexp-info"""
    t = tell(event, "get-type")
    name = {"answer": "answer-event-pexp-info", "clamp": "clamp-event-pexp-info",
            "concept-activation": "concept-activation-event-pexp-info",
            "group": "group-event-pexp-info", "rule": "rule-event-pexp-info",
            "concept-mapping": "concept-mapping-event-pexp-info",
            "snag": "snag-event-pexp-info"}.get(t)
    return V("gui.trace_graphics", name) if name is not None else None


@battery_test("trace-constants")
def _():
    return [V("gui.trace_graphics", "%group-event-icon-arrowhead-length%"),
            V("gui.trace_graphics", "%minimum-event-width%"),
            V("gui.trace_graphics", "%minimum-event-height%")]


@battery_test("event-pexp-info")
def _():
    return chez.map_(
        lambda event: b_with_log(lambda: b_event_pexp_info(event)(b_g, event, F(3, 5), F(1, 2))),
        B.events)


@battery_test("event-pexp-info-flonum")
def _():
    return chez.map_(lambda event: b_clean(b_event_pexp_info(event)(b_g, event, 2.2, 0.5)),
                     B.events)


@battery_test("group-event-text-strings")
def _():
    return chez.map_(M("trace_graphics").group_event_pexp_text_string, B.groups)


@battery_test("group-event-arrowheads")
def _():
    arrowhead = V("gui.trace_graphics", "group-event-pexp-arrowhead")
    return [arrowhead(P("plato-right"), F(1, 2), F(3, 4)),
            arrowhead(P("plato-left"), 0.5, 0.75),
            arrowhead(False, F(1, 2), F(3, 4))]


# memory-graphics.ss: icons on a fake window

@battery_test("memory-icon-pexp-info")
def _():
    get_info = V("gui.memory_graphics", "get-memory-icon-pexp-info")

    def each(name):
        answer = b_fake("answer-description", [("print-name", S(name))])
        box = [False]

        def thunk():
            box[0] = get_info(b_g, answer, F(1, 2), F(19, 20))
            return "done"
        log = b_with_log(thunk)
        info = box[0]
        return [log, b_clean(info[1:]),
                chez.map_(lambda a: b_clean(info[0](a)), [0, 37, 100])]
    return chez.map_(each, ["abc -> abd, xyz -> wyz", "abc -> abd, xyz -> SNAG", ""])


# The Trace and Memory windows' mouse handlers, with fake windows and model

def b_window(name):
    """panels-battery.scm: b:window"""
    def fn(self, m, *args):
        log_bang(b_clean([name, m, *args]))
        return "done"
    return Lambda(fn)


def _toggle():
    B.highlighted_p = not B.highlighted_p      # set! returns void


@form
def _():
    B.highlighted_p = False
    B.event = b_fake("answer-event",
                     [("toggle-highlight", _toggle),
                      ("highlighted?", lambda: B.highlighted_p),
                      ("display", lambda: log_bang("event-display"))])
    B.previous = False
    B.answer = b_fake("answer-description",
                      [("toggle-highlight", _toggle),
                       ("highlighted?", lambda: B.highlighted_p),
                       ("display", lambda: log_bang("answer-display"))])
    B.snag = b_fake("snag-description",
                    [("toggle-highlight", _toggle),
                     ("highlighted?", lambda: B.highlighted_p),
                     ("display", lambda: log_bang("snag-display"))])
    B.selected = False


def b_install_handler_fakes():
    """panels-battery.scm: b:install-handler-fakes"""
    def select_event(x, y):
        log_bang(["select-event", x, y])
        return B.selected

    def select_answer(x, y):
        log_bang(["select-answer", x, y])
        return B.selected
    set_global("*running?*", False)
    set_global("*trace*", b_fake("temporal-trace", [("get-mouse-selected-event", select_event)]))
    set_global("*memory*", b_fake("memory",
                                  [("get-mouse-selected-answer", select_answer),
                                   ("get-other-highlighted-answer", lambda a: B.previous)]))
    set_global("*themespace*", b_fake("themespace", []))
    set_global("*workspace-window*", b_window("workspace-window"))
    set_global("*slipnet-window*", b_window("slipnet-window"))
    set_global("*coderack-window*", b_window("coderack-window"))
    set_global("*temperature-window*", b_window("temperature-window"))
    set_global("*comment-window*", b_window("comment-window"))
    set_global("*temperature*", 37)


@battery_test("trace-press-handler")
def _():
    b_install_handler_fakes()
    handler = V("gui.trace_graphics", "trace-window-press-handler")

    def first():
        B.selected = False
        return handler("win", F(1, 2), F(1, 3))

    def second():
        B.selected = B.event
        return handler("win", 3, F(1, 2))

    def fourth():
        set_global("*running?*", True)
        handler("win", 3, F(1, 2))
        return set_global("*running?*", False)
    return [b_with_log(first), b_with_log(second),
            b_with_log(lambda: handler("win", 3, F(1, 2))), b_with_log(fourth)]


@battery_test("memory-press-handler")
def _():
    b_install_handler_fakes()
    B.highlighted_p = False
    handler = V("gui.memory_graphics", "memory-window-press-handler")

    def first():
        B.selected = False
        return handler("win", F(1, 2), F(1, 3))

    def second():
        B.selected = B.answer
        return handler("win", F(1, 2), 2)

    def fourth():
        B.selected = B.snag
        B.previous = B.answer
        return handler("win", F(1, 2), 2)
    return [b_with_log(first), b_with_log(second),
            b_with_log(lambda: handler("win", F(1, 2), 2)), b_with_log(fourth)]


# eeg-graphics.ss: the EEG object over a sequence of Workspace activities

@battery_test("EEG-table")
def _():
    return [list(entry[:5]) for entry in V("eeg_graphics", "%EEG-table%")]


@battery_test("EEG-constants")
def _():
    return [V("eeg_graphics", "%EEG-buffer-size%"),
            V("gui.eeg_graphics", "%max-EEG-window-cycles%")]


@form
def _():
    B.activity = 100


@battery_test("EEG-recording")
def _():
    set_global("*workspace*", b_fake("workspace", [("get-activity", lambda: B.activity)]))
    eeg = engine.get_global("*EEG*")
    tell(eeg, "initialize")
    acc = []
    for i in range(47):
        B.activity = chez.sub(100, chez.mul(3, i), F(1, 3) if i % 2 == 1 else 0)
        set_global("*temperature*", chez.mul(4, i) if i < 20 else chez.sub(100, i))
        tell(eeg, "record-current-values")
        acc.append([tell(eeg, "get-current-values"),
                    tell(eeg, "get-current-value", 1),
                    tell(eeg, "get-average-value", 2, 3)])
    return [acc,
            tell(eeg, "get-previous-values", 0),
            tell(eeg, "get-previous-values", 2, 5),
            tell(eeg, "get-average-value", 0),
            tell(eeg, "get-max-variation", 0),
            tell(eeg, "get-max-variation", 2, 8)]


# The runner ----------------------------------------------------------------------------

RESULTS: dict = {}
_done = [0]          # the number of FORMS evaluated so far


class Failed:
    def __init__(self, exc):
        self.exc = exc


def run_through(name):
    """Evaluate the battery's forms in order, up to and including the test NAME."""
    while name not in RESULTS:
        kind, test_name, thunk = FORMS[_done[0]]
        _done[0] += 1
        try:
            value = thunk()
        except Exception as exc:          # a form or test that fails leaves the rest running
            value = Failed(exc)
        if kind == "test":
            RESULTS[test_name] = value
    return RESULTS[name]


# globals the battery assigns (b:set-global!) or that the views' loading sets
SAVED_GLOBALS = ["*themespace*", "*running?*", "*trace*", "*memory*", "*workspace*",
                 "*workspace-window*", "*slipnet-window*", "*coderack-window*",
                 "*temperature-window*", "*comment-window*", "*temperature*", "*fg-color*"]


@pytest.fixture(scope="module", autouse=True)
def battery_engine():
    engine.load()
    from metacat.gui import hosts
    hosts.install_offscreen_fonts()
    saved = {}
    for name in SAVED_GLOBALS:
        try:
            saved[name] = engine.get_global(name)
        except chez.SchemeError:
            pass
    vg = M("view_globals")
    saved_view_globals = dict(vars(vg))
    try:
        from metacat.gui import views
    except ImportError:                    # not written yet: load what exists
        views = None
        from metacat.gui import constants, sgl
        sgl.load()
        constants.load()
        for stem in ("theme_graphics", "trace_graphics", "memory_graphics"):
            try:
                mod = M("gui." + stem)
            except ImportError:
                continue
            if hasattr(mod, "load"):
                mod.load()
    if views is not None:
        views.load_views()
    yield
    for name, value in saved.items():
        set_global(name, value)
    for k, v in saved_view_globals.items():
        if not k.startswith("__"):
            setattr(vg, k, v)


def first_difference(got, expected):
    i = next((k for k, (a, b) in enumerate(zip(got, expected)) if a != b),
             min(len(got), len(expected)))
    return (f"differs at character {i} of {len(expected)}:\n"
            f"  python: ...{got[max(0, i - 150):i + 150]}\n"
            f"  chez:   ...{expected[max(0, i - 150):i + 150]}")


def test_every_battery_test_is_translated():
    assert list(CASES) == list(manifest(BATTERY))
    assert len(CASES) == 36


@pytest.mark.parametrize("name", manifest(BATTERY))
def test_panels_battery(name):
    expected = fixture(BATTERY, name)
    value = run_through(name)
    if isinstance(value, Failed):
        raise value.exc
    got = canon(value)
    same = got == expected      # not `assert got == expected`: pytest's diff of long strings is slow
    assert same, first_difference(got, expected)


# Beyond the battery: the structure of this item's three modules ---------------------

import inspect                      # noqa: E402
import re                           # noqa: E402
import subprocess                   # noqa: E402
import sys                          # noqa: E402
from pathlib import Path            # noqa: E402

ROOT = Path(__file__).resolve().parents[2]
ORIGINAL = ROOT / "chez_scheme" / "original"
PANEL_FILES = {
    "theme-graphics": "metacat.gui.theme_graphics",
    "trace-graphics": "metacat.gui.trace_graphics",
    "memory-graphics": "metacat.gui.memory_graphics",
}
# the definitions the engine has (the model calls them), used from there
ENGINE_PARTS = {"relation-name": "metacat.theme_graphics",
                "group-event-pexp-text-string": "metacat.trace_graphics"}


def _defines(stem):
    return re.findall(r"^\(define\s+(\S+)", (ORIGINAL / (stem + ".ss")).read_text(), re.M)


def _is_lambda_define(stem, name):
    text = (ORIGINAL / (stem + ".ss")).read_text()
    return re.search(r"^\(define\s+" + re.escape(name) + r"\s*\n\s*\(lambda", text,
                     re.M) is not None


@pytest.mark.parametrize("stem", sorted(PANEL_FILES))
def test_every_define_is_translated(stem):
    module = importlib.import_module(PANEL_FILES[stem])
    missing = []
    for name in _defines(stem):
        where = importlib.import_module(ENGINE_PARTS.get(name, PANEL_FILES[stem]))
        if not hasattr(where, scheme_to_python(name)):
            missing.append(name)
        elif name in ENGINE_PARTS and hasattr(module, scheme_to_python(name)):
            missing.append(name + " (also in the views)")
    assert missing == []


@pytest.mark.parametrize("stem", sorted(PANEL_FILES))
def test_docstrings_name_their_origin(stem):
    module = importlib.import_module(PANEL_FILES[stem])
    doc = module.__doc__
    assert "GNU General Public License" in doc
    assert "Translated to Python (2026) from %s.ss" % stem in doc
    assert "racket/gui/%s.rktl" % stem in doc
    wrong = []
    for name in _defines(stem):
        if name not in ENGINE_PARTS and _is_lambda_define(stem, name):
            fn = getattr(module, scheme_to_python(name))
            if not (fn.__doc__ or "").startswith("%s.ss: %s" % (stem, name)):
                wrong.append(name)
    for _, obj in inspect.getmembers(module, inspect.isfunction):
        if obj.__module__ == module.__name__ and not obj.__name__.startswith("_"):
            if not (obj.__doc__ or "").startswith((stem + ".ss: ", "port: ")):
                wrong.append(obj.__name__)
    assert wrong == []


def test_no_tkinter_at_import():
    code = ("import sys; import " + ", ".join(PANEL_FILES.values()) +
            "; assert 'tkinter' not in sys.modules, 'tkinter imported'")
    result = subprocess.run([sys.executable, "-c", code], cwd=ROOT / "python",
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr


def test_engine_does_not_import_these_modules():
    for path in (ROOT / "python" / "metacat").glob("*.py"):
        text = path.read_text()
        for mod in PANEL_FILES.values():
            assert mod not in text, (path.name, mod)


def test_relation_names_pexp_is_unbound_as_in_chez():
    """1.2: make-panel's get-relation-names-pexp returns relation-names-pexp,
    which nothing defines (anomalies_and_quirks.md)"""
    run_through("panel-initialize")
    with pytest.raises(chez.UnboundVariable, match="relation-names-pexp is not bound"):
        tell(B.panel, "get-relation-names-pexp")


def test_memory_icon_colours():
    """memory-graphics.ss: the icon's grey level is %memory-background-grey-level%
    (50) plus the activation's share of the rest, rounded half to even (b:clean
    hides colours from the battery): 37 -> 50 + round(18.5) = grey68."""
    from metacat.gui import colors, memory_graphics as mg

    def rgb(c):
        return (c.r, c.g, c.b)
    assert rgb(mg.memory_background_color()) == rgb(colors.swl_color(S("grey50")))
    for activation, level in [(0, 50), (37, 68), (39, 70), (100, 100), (F(1, 3), 50)]:
        assert rgb(mg.memory_icon_activation_color(activation)) == \
            rgb(colors.swl_color(S("grey%d" % level))), activation
