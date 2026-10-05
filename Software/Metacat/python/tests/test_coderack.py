"""constants.py, setup.py, coderack.py and descriptions.py against Chez (loop0002 item 04).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/coderack-battery.scm, translated: CASES maps each test
name to a function that rebuilds the battery expression in Python, in the same
order of random draws and side effects, and returns its value.  Its b:canon text
must equal the frozen Chez output in python/fixtures/coderack/.

The battery's forms run in order and share the engine's state (the coderack,
the codelet types' clamps and procedures, the globals), so the cases run in the
battery's order, in one engine.  The battery's top-level forms between tests
(installing the fakes, resetting the modes) run before the test they precede
(TOP_LEVEL).  Its fakes for the Workspace, Themespace, Temporal Trace and
top-down slipnodes are translated here; they stand in for globals of modules not
translated yet (engine_stubs.engine_module).  Every battery `map` is chez.map_,
since several of them have effects.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import io
import re
from contextlib import ExitStack, redirect_stdout
from fractions import Fraction as F

import pytest

from chez_fixtures import chez as fixture, manifest
from engine_stubs import engine_module
from name_mapping import ORIGINAL
from scheme_canon import canon
from test_chez import LOG, SEEDS, iota, log, repeat, seeded  # helpers.scm
from metacat import chez, sugar, utilities
from metacat.chez import String
from metacat.names import scheme_to_python
from metacat.objects import INVALID, Lambda, tell

from metacat import engine, constants, setup, coderack, descriptions

CASES: dict = {}
TOP_LEVEL: dict = {}


def case(name):
    def register(fn):
        assert name not in CASES, name
        CASES[name] = fn
        return fn
    return register


def capture(thunk):
    """b:capture: what thunk prints, as a Scheme string."""
    out = io.StringIO()
    with redirect_stdout(out):
        thunk()
    return String(out.getvalue())


set_global = engine.set_global          # b:set-global!


def rack_obj():
    return coderack.g_coderack


def types_():
    return coderack.g_codelet_types


def ct(name):
    """A codelet type by its Scheme name (the battery refers to them as globals)."""
    return chez.top_level_value(name)


# Helpers ------------------------------------------------------------------------------

def index_of(x, l):
    """coderack-battery.scm: b:index-of"""
    for i, y in enumerate(l):
        if y is x:
            return i
    return False


def bin_index(b):
    """coderack-battery.scm: b:bin-index"""
    return index_of(b, tell(rack_obj(), "get-all-bins"))


def type_names(ts):
    """coderack-battery.scm: b:type-names"""
    return chez.map_(lambda t: tell(t, "get-codelet-type-name"), ts)


def codelet_data(c):
    """coderack-battery.scm: b:codelet"""
    return [tell(c, "get-codelet-type-name"),
            tell(c, "get-relative-urgency"),
            tell(c, "get-time-stamp"),
            tell(c, "get-index-in-bin"),
            bin_index(tell(c, "get-coderack-bin")),
            tell(c, "proposed-structure-argument?")]


def rack():
    """coderack-battery.scm: b:rack"""
    r = rack_obj()
    return [tell(r, "get-num-of-codelets"),
            tell(r, "empty?"),
            tell(r, "get-total-urgency-sum"),
            tell(r, "get-highest-bin-urgency"),
            chez.map_(lambda b: [tell(b, "get-num-of-codelets"),
                                 tell(b, "get-urgency"),
                                 tell(b, "get-urgency-sum"),
                                 chez.map_(codelet_data, tell(b, "get-codelets"))],
                      tell(r, "get-all-bins")),
            chez.map_(codelet_data, tell(r, "get-all-codelets"))]


URGENCIES = [0, 7, F(15, 2), 21, 35, 49, 63, 77, 91, 100, 3, 10, 14, 28, 50, 99, 1, 42, 70, 85,
             F(102, 5), 7.5, 33.3, 66.6, F(1, 3), 99.99, 13, 57, 64, 12]


def post_n(n, offset):
    """coderack-battery.scm: b:post-n"""
    for i in range(n):
        k = i + offset
        type_ = types_()[k % len(types_())]
        urgency = URGENCIES[(7 * k) % len(URGENCIES)]
        codelet = tell(type_, "make-codelet", urgency)
        set_global("*codelet-count*", setup.g_codelet_count + 1 + k % 3)
        tell(rack_obj(), "post", codelet)


def choose_n(n):
    """coderack-battery.scm: b:choose-n"""
    def one():
        c = tell(rack_obj(), "choose-codelet")
        s = chez.random_seed()
        return [tell(c, "get-codelet-type-name"), tell(c, "get-relative-urgency"),
                tell(c, "get-time-stamp"), s]
    return repeat(n, one)


WINDOW_LOG: list = []


def log_window_fn(self, msg, *args):
    """coderack-battery.scm: b:log-window"""
    if msg == "set-last-codelet-type":
        WINDOW_LOG.append(["set-last-codelet-type", tell(args[0], "get-codelet-type-name")])
    else:
        WINDOW_LOG.append([msg, *args])
    return "done"


LOG_WINDOW = Lambda(log_window_fn)

# fakes for not-yet-ported model objects; WS["settings"] is read by the fake workspace
WS = {"settings": {}}
DELETED: list = []


def setting(key):
    """coderack-battery.scm: b:setting"""
    return WS["settings"][key]


def fake_workspace_fn(self, msg, *args):
    """coderack-battery.scm: b:fake-workspace"""
    simple = {"get-average-intra-string-unhappiness": "intra",
              "get-min-mapping-strength": "min-mapping",
              "get-average-unhappiness": "unhappiness",
              "get-possible-rule-types": "rule-types",
              "get-rough-num-of-unrelated-objects": "unrelated",
              "get-bonds": "bonds",
              "get-rough-num-of-ungrouped-objects": "ungrouped",
              "get-rough-num-of-unmapped-objects": "unmapped",
              "get-max-inter-string-unhappiness": "inter"}
    if msg == "object-type":
        return "workspace"
    if msg in simple:
        return setting(simple[msg])
    if msg == "supported-rule-exists?":
        return chez.memq(args[0], setting("supported"))
    if msg == "delete-proposed-structure":
        DELETED.append(tell(args[0], "get-value"))
        return "done"
    return INVALID


def fake_themespace_fn(self, msg, *args):
    """coderack-battery.scm: b:fake-themespace"""
    if msg == "object-type":
        return "themespace"
    if msg == "thematic-pressure?":
        return setting("pressure")
    if msg == "get-active-bridge-theme-types":
        return setting("bridge-themes")
    if msg == "get-max-positive-theme-activation":
        return 0 if len(args[0]) == 0 else setting("theme-activation")
    return INVALID


def fake_trace_fn(self, msg, *args):
    """coderack-battery.scm: b:fake-trace"""
    if msg == "object-type":
        return "trace"
    if msg == "within-snag-period?":
        return setting("snag")
    if msg == "within-clamp-period?":
        return setting("clamp")
    return INVALID


TOP_DOWN_LOG: list = []


def make_fake_node(n):
    """coderack-battery.scm: b:make-fake-node"""
    def fn(self, msg, *args):
        if msg == "object-type":
            return "slipnode"
        if msg == "attempt-to-post-top-down-codelets":
            TOP_DOWN_LOG.append(n)
            return "done"
        return INVALID
    return Lambda(fn)


def make_fake_structure(type_, n):
    """coderack-battery.scm: b:make-fake-structure"""
    def fn(self, msg, *args):
        if msg == "object-type":
            return type_
        if msg == "get-value":
            return n
        if msg == "print":
            return chez.printf("<fake ~a ~a>~%", type_, n)
        return INVALID
    return Lambda(fn)


SETTINGS_LIST = [
    {"intra": 30, "min-mapping": 40, "unhappiness": 55, "rule-types": [], "supported": [],
     "unrelated": "few", "bonds": [], "ungrouped": "few", "unmapped": "some", "inter": 45,
     "pressure": False, "bridge-themes": [], "theme-activation": 0, "snag": False, "clamp": False},
    {"intra": 80, "min-mapping": 10, "unhappiness": 90, "rule-types": ["top"], "supported": ["top"],
     "unrelated": "many", "bonds": ["b1"], "ungrouped": "many", "unmapped": "many", "inter": 100,
     "pressure": True, "bridge-themes": ["top-bridge"], "theme-activation": 85,
     "snag": True, "clamp": False},
    {"intra": 0, "min-mapping": 100, "unhappiness": 0, "rule-types": ["top", "bottom"],
     "supported": ["bottom"], "unrelated": "some", "bonds": ["b1", "b2"], "ungrouped": "some",
     "unmapped": "few", "inter": 33.3, "pressure": False,
     "bridge-themes": ["top-bridge", "bottom-bridge"], "theme-activation": 40,
     "snag": False, "clamp": True},
    {"intra": 12.5, "min-mapping": 77.7, "unhappiness": F(102, 5), "rule-types": ["top"],
     "supported": ["top", "bottom"], "unrelated": "many", "bonds": ["b1"], "ungrouped": "few",
     "unmapped": "some", "inter": 64, "pressure": True, "bridge-themes": [],
     "theme-activation": 0, "snag": False, "clamp": False},
]


def reset():
    """coderack-battery.scm: b:reset"""
    set_global("*codelet-count*", 0)
    set_global("*temperature*", 0)
    tell(rack_obj(), "initialize")


# The engine, with the battery's stand-ins --------------------------------------------

# Globals of modules that are not translated yet, which the coderack reads.
STAND_INS = {
    "workspace": {"g_workspace": False},
    "themes": {"g_themespace": False},
    "trace": {"g_trace": False},
    "slipnet": {"g_top_down_slipnodes": False},
    "run": {"g_display_mode_p": False, "g_step_mode_p": False, "p_step_cycles": 1},
}


@pytest.fixture(scope="module", autouse=True)
def battery_engine():
    engine.load()
    with ExitStack() as stack:
        for name, attrs in STAND_INS.items():
            stack.enter_context(engine_module(name, **attrs))
        # setup.ss's window globals are changed by setup-commands; restore them
        saved = {k: getattr(setup, k) for k in vars(setup) if k.startswith(("g_", "p_"))}
        # the battery gives breaker a fake procedure (define-codelet-procedure*);
        # later test files run the real codelets, so restore every procedure
        procs = [(t, t.codelet_proc) for t in coderack.g_codelet_types]
        yield
        for k, v in saved.items():
            setattr(setup, k, v)
        for t, proc in procs:
            t.codelet_proc = proc


# Section 1: setup.ss defaults, before anything changes them ---------------------------

@case("setup-defaults")
def _():
    return [setup.g_codelet_count, setup.g_temperature,
            setup.p_eliza_mode, setup.p_justify_mode, setup.p_self_watching_enabled,
            setup.p_verbose, setup.p_workspace_graphics, setup.p_slipnet_graphics,
            setup.p_coderack_graphics, setup.p_codelet_count_graphics,
            setup.p_highlight_last_codelet, setup.p_nice_graphics]


@case("setup-windows")
def _():
    return [setup.g_workspace_window, setup.g_slipnet_window, setup.g_coderack_window,
            setup.g_themespace_window, setup.g_top_themes_window, setup.g_bottom_themes_window,
            setup.g_vertical_themes_window, setup.g_memory_window, setup.g_comment_window,
            setup.g_trace_window, setup.g_temperature_window, setup.g_EEG_window,
            setup.g_control_panel, setup.g_repl_thread]


@case("coderack-constants")
def _():
    c = coderack
    return [c.p_max_coderack_size, c.p_num_of_coderack_bins,
            c.p_extremely_low_urgency, c.p_very_low_urgency, c.p_low_urgency, c.p_medium_urgency,
            c.p_high_urgency, c.p_very_high_urgency, c.p_extremely_high_urgency]


def install_fakes():
    """The battery's top level: switch the display off and install the fakes."""
    set_global("%coderack-graphics%", False)
    set_global("%codelet-count-graphics%", False)
    set_global("%workspace-graphics%", False)
    set_global("%slipnet-graphics%", False)
    set_global("*workspace*", Lambda(fake_workspace_fn))
    set_global("*themespace*", Lambda(fake_themespace_fn))
    set_global("*trace*", Lambda(fake_trace_fn))
    set_global("*top-down-slipnodes*", chez.map_(make_fake_node, iota(5)))
    for t in types_():
        tell(t, "set-graphics-parameters", LOG_WINDOW, False, False, False, False, False, False,
             False, False)


TOP_LEVEL["urgency-value-table"] = install_fakes


# Urgencies and bins -------------------------------------------------------------------

@case("urgency-value-table")
def _():
    return coderack.p_urgency_value_table


@case("urgency-names")
def _():
    return chez.map_(coderack.urgency_name,
                     list(range(1, 102)) + [-1, 0, 7, 7.0, F(15, 2), 7.000001, 21, 21.5, 35, 49,
                                            49.0, 63, 77, 91, 100, 1000])


@case("coderack-bin-indices")
def _():
    return chez.map_(lambda u: bin_index(tell(rack_obj(), "get-coderack-bin", u)),
                     iota(101) + [-5, 0, F(1, 2), F(100, 7), F(99, 7), F(101, 7),
                                  14.285714285714286, 14.285714285714285, F(200, 7), 99.9, 100,
                                  100.0, 120, 7.5, 33.3, 66.6, 85.71428571428571, F(600, 7)])


@case("bin-urgencies-by-temperature")
def _():
    def at(t):
        set_global("*temperature*", t)
        return [t, tell(rack_obj(), "get-highest-bin-urgency"),
                chez.map_(lambda b: tell(b, "get-urgency"), tell(rack_obj(), "get-all-bins"))]
    return chez.map_(at, iota(100) + [0])


@case("codelet-type-names")
def _():
    return type_names(coderack.g_codelet_types)


@case("thematic-codelet-types")
def _():
    return type_names(coderack.g_thematic_codelet_types)


@case("bottom-up-codelet-types")
def _():
    return type_names(coderack.g_bottom_up_codelet_types)


@case("self-watching-codelet-types")
def _():
    return type_names(coderack.g_self_watching_codelet_types)


@case("codelet-type-object-type")
def _():
    return chez.map_(lambda t: tell(t, "object-type"), types_())


@case("graphics-labels-off")
def _():
    return chez.map_(lambda t: tell(t, "get-graphics-labels"), types_())


@case("graphics-labels-on")
def _():
    set_global("%codelet-count-graphics%", True)
    labels = chez.map_(lambda t: tell(t, "get-graphics-labels"), types_())
    set_global("%codelet-count-graphics%", False)
    return labels


# Posting and choosing -----------------------------------------------------------------

@case("post-30")
def _():
    reset()
    post_n(30, 0)
    return rack()


@case("choose-by-seed-and-temperature")
def _():
    def by_seed(seed):
        def by_temperature(t):
            reset()
            post_n(40, seed)
            set_global("*temperature*", t)
            return seeded(seed, lambda: [choose_n(25), rack()])
        return chez.map_(by_temperature, [0, 15, 35, 50, 64, 85, 100])
    return chez.map_(by_seed, [1, 2, 3, 7, 42, 1000, 123456789, 4294967295])


@case("choose-until-empty")
def _():
    def by_seed(seed):
        reset()
        post_n(17, seed)
        set_global("*temperature*", 40)
        return seeded(seed, lambda: [choose_n(17), rack()])
    return chez.map_(by_seed, SEEDS)


@case("overflow-deletes")
def _():
    def by_seed(seed):
        reset()
        set_global("*temperature*", 30)

        def body():
            post_n(160, seed)
            return rack()
        return seeded(seed, body)
    return chez.map_(by_seed, [1, 2, 3, 42, 65536, 2147483648, 3141592653])


@case("overflow-deletes-proposed-structures")
def _():
    def by_seed(seed):
        reset()
        DELETED.clear()

        def body():
            for i in range(130):
                type_ = types_()[i % 27]
                arg = make_fake_structure(["bond", "group", "bridge", "description", "letter"][i % 5], i)
                c = tell(type_, "make-codelet", URGENCIES[i % 30], arg)
                set_global("*codelet-count*", setup.g_codelet_count + 1)
                tell(rack_obj(), "post", c)
            return [list(DELETED), rack()]
        return seeded(seed, body)
    return chez.map_(by_seed, [1, 5, 99])


@case("removal-weights")
def _():
    reset()
    post_n(50, 3)
    set_global("*codelet-count*", 500)
    set_global("*temperature*", 70)
    return chez.map_(lambda c: tell(c, "get-removal-weight"), tell(rack_obj(), "get-all-codelets"))


# Deferred codelets --------------------------------------------------------------------

def post_deferred(posted, deferred, step):
    def by_seed(seed):
        reset()
        post_n(posted, seed)

        def body():
            for i in range(deferred):
                type_ = types_()[(step * i) % 27]
                tell(rack_obj(), "add-deferred-codelet", tell(type_, "make-codelet", URGENCIES[i % 30]))
            tell(rack_obj(), "post-deferred-codelets")
            return rack()
        return seeded(seed, body)
    return chez.map_(by_seed, [1, 2, 3, 77])


@case("post-deferred-small")
def _():
    return post_deferred(70, 45, 5)


@case("post-deferred-large")
def _():
    return post_deferred(60, 137, 11)


@case("post-deferred-exactly-100")
def _():
    reset()
    post_n(10, 0)

    def body():
        for i in range(100):
            tell(rack_obj(), "add-deferred-codelet",
                 tell(ct("bottom-up-bond-scout"), "make-codelet", URGENCIES[i % 30]))
        tell(rack_obj(), "post-deferred-codelets")
        return rack()
    return seeded(9, body)


# Clamping and urgency adjustments -----------------------------------------------------

@case("clamp-unclamp")
def _():
    reset()
    post_n(54, 0)
    bond_builder = ct("bond-builder")
    before = rack()
    r1 = tell(bond_builder, "clamp", 90)
    clamped = [tell(bond_builder, "clamped?"), tell(bond_builder, "get-clamped-urgency")]
    after_clamp = rack()
    new = tell(bond_builder, "make-codelet", 10)
    new_urgency = tell(new, "get-relative-urgency")
    r2 = tell(bond_builder, "clamp", 90)
    r3 = tell(bond_builder, "clamp", 20)
    after_reclamp = rack()
    r4 = tell(bond_builder, "unclamp")
    r5 = tell(bond_builder, "unclamp")
    after_unclamp = rack()
    return [before, r1, clamped, after_clamp, new_urgency, r2, r3, after_reclamp, r4, r5,
            after_unclamp, tell(bond_builder, "clamped?")]


@case("adjust-and-set-urgencies")
def _():
    reset()
    post_n(54, 1)
    r = rack_obj()
    out = []
    for msg, type_name, arg in [("adjust-urgencies", "rule-scout", 30),
                                ("adjust-urgencies", "rule-scout", -200),
                                ("adjust-urgencies", "breaker", F(1, 3)),
                                ("set-urgencies", "jootser", 77),
                                ("reset-urgencies", "jootser", None),
                                ("reset-urgencies", "rule-scout", None)]:
        args = () if arg is None else (arg,)
        out.append(tell(r, msg, ct(type_name), *args))
        out.append(rack())
    return out


@case("codelets-of-type")
def _():
    reset()
    post_n(80, 2)
    return chez.map_(lambda t: chez.map_(codelet_data, tell(rack_obj(), "get-codelets-of-type", t)),
                     types_())


@case("choose-after-clamp")
def _():
    def by_seed(seed):
        reset()
        post_n(60, seed)
        tell(ct("answer-finder"), "clamp", 100)
        tell(ct("description-builder"), "clamp", 5)
        set_global("*temperature*", 55)
        r = seeded(seed, lambda: choose_n(30))
        tell(ct("answer-finder"), "unclamp")
        tell(ct("description-builder"), "unclamp")
        return [r, rack()]
    return chez.map_(by_seed, [1, 2, 3, 4, 5])


@case("update-all-selection-probabilities")
def _():
    reset()
    post_n(33, 5)
    set_global("*temperature*", 45)
    r1 = tell(rack_obj(), "update-all-selection-probabilities")
    r2 = tell(rack_obj(), "initialize")
    r3 = tell(rack_obj(), "update-all-selection-probabilities")
    return [r1, r2, r3]


@case("delete-all-codelets")
def _():
    reset()
    DELETED.clear()
    tell(rack_obj(), "post", tell(ct("bond-builder"), "make-codelet", 50, make_fake_structure("bond", 1)))
    tell(rack_obj(), "post", tell(ct("group-builder"), "make-codelet", 50, make_fake_structure("group", 2)))
    tell(rack_obj(), "post",
         tell(ct("description-builder"), "make-codelet", 50, make_fake_structure("description", 3)))
    post_n(5, 0)
    r = tell(rack_obj(), "delete-all-codelets")
    return [r, list(reversed(DELETED)), rack()]


# Codelets -----------------------------------------------------------------------------

@case("codelet-accessors")
def _():
    reset()
    set_global("*codelet-count*", 17)
    s1 = make_fake_structure("bridge", 4)
    s2 = make_fake_structure("workspace", 5)
    c = tell(ct("top-down-description-scout"), "make-codelet", F(102, 5), s1, s2)
    r = tell(rack_obj(), "post", c)
    return [r, tell(c, "object-type"), tell(c, "get-relative-urgency"),
            tell(c, "codelet-type?", ct("top-down-description-scout")),
            tell(c, "codelet-type?", ct("bond-builder")),
            tell(c, "proposed-structure-argument?"),
            tell(tell(c, "get-proposed-structure"), "get-value"),
            tell(tell(c, "get-argument", 0), "get-value"),
            tell(tell(c, "get-argument", 1), "get-value"),
            tell(c, "get-time-stamp"), tell(c, "get-index-in-bin"),
            bin_index(tell(c, "get-coderack-bin"))]


@case("codelet-run-and-fizzle")
def _():
    reset()
    WINDOW_LOG.clear()
    LOG.clear()

    def breaker(x, y):
        log(["breaker", tell(x, "get-value"), y])
        if tell(x, "get-value") > 3:
            sugar.fizzle()
        log(["breaker-after", tell(x, "get-value")])
        return "finished"
    sugar.define_codelet_procedure_star("breaker", breaker)

    def jootser():
        log("jootser")
        return "jootser-done"
    sugar.define_codelet_procedure_star("jootser", jootser)
    c1 = tell(ct("breaker"), "make-codelet", 20, make_fake_structure("letter", 2), "a")
    c2 = tell(ct("breaker"), "make-codelet", 20, make_fake_structure("letter", 5), "b")
    c3 = tell(ct("jootser"), "make-codelet", 50)
    r1 = tell(c1, "run")
    r2 = tell(c2, "run")
    r3 = tell(c3, "run")
    return [r1, r2, r3, list(LOG), list(WINDOW_LOG), sugar.fizzle]


@case("codelet-type-print")
def _():
    reset()
    rule_scout = ct("rule-scout")
    tell(rule_scout, "clamp", 65)
    s1 = capture(lambda: utilities.print_(rule_scout))
    r = tell(rule_scout, "unclamp")
    s2 = capture(lambda: utilities.print_(rule_scout))
    s3 = capture(lambda: utilities.print_(ct("bottom-up-description-scout")))
    return [s1, r, s2, s3]


@case("codelet-print")
def _():
    reset()
    set_global("*codelet-count*", 3)
    c1 = tell(ct("bond-evaluator"), "make-codelet", F(102, 5), make_fake_structure("bond", 7))
    c2 = tell(ct("top-down-bond-scout:category"), "make-codelet", 33.3,
              make_fake_structure("slipnode", 8), make_fake_structure("string", 9))
    c3 = tell(ct("breaker"), "make-codelet", 7)
    tell(rack_obj(), "post", c1)
    tell(rack_obj(), "post", c3)
    pr = utilities.print_
    return [capture(lambda: pr(c1)), capture(lambda: pr(c2)), capture(lambda: pr(c3))]


@case("coderack-print")
def _():
    reset()
    post_n(40, 4)
    set_global("*temperature*", 62)
    return [capture(lambda: utilities.print_(rack_obj())),
            capture(lambda: tell(rack_obj(), "show-bin", 2)),
            capture(lambda: tell(rack_obj(), "show-bin", 6))]


# Bottom-up and top-down posting (with the fakes) --------------------------------------

def posting_row(ts):
    """coderack-battery.scm: b:posting-row"""
    return chez.map_(lambda t: [tell(t, "get-codelet-type-name"),
                                coderack.post_codelet_probability(t),
                                coderack.num_of_codelets_to_post(t),
                                coderack.bottom_up_urgency(t)], ts)


@case("posting-parameters")
def _():
    def by_settings(settings):
        WS["settings"] = settings

        def by_modes(modes):
            set_global("%justify-mode%", modes[0])
            set_global("%self-watching-enabled%", modes[1])
            set_global("*temperature*", modes[2])
            return [posting_row(types_()),
                    coderack.thematic_codelet_urgency(ct("thematic-bridge-scout"))]
        return chez.map_(by_modes, [[False, True, 30], [True, True, 80], [False, False, 0],
                                    [True, False, 100]])
    return chez.map_(by_settings, SETTINGS_LIST)


@case("posting-with-clamps")
def _():
    WS["settings"] = SETTINGS_LIST[0]
    set_global("%justify-mode%", False)
    set_global("%self-watching-enabled%", True)
    tell(ct("breaker"), "clamp", 70)
    tell(ct("answer-justifier"), "clamp", 70)
    tell(ct("jootser"), "clamp", 33)
    row = posting_row(types_())
    tell(ct("breaker"), "unclamp")
    tell(ct("answer-justifier"), "unclamp")
    tell(ct("jootser"), "unclamp")
    return row


@case("add-bottom-up-codelets")
def _():
    def by_settings(settings):
        WS["settings"] = settings

        def by_seed(seed):
            reset()
            post_n(20, seed)
            set_global("%justify-mode%", seed % 2 == 1)
            set_global("%self-watching-enabled%", seed < 3)
            set_global("*temperature*", 20 * seed)

            def body():
                coderack.add_bottom_up_codelets()
                tell(rack_obj(), "post-deferred-codelets")
                return rack()
            return seeded(seed, body)
        return chez.map_(by_seed, [1, 2, 3, 4])
    return chez.map_(by_settings, SETTINGS_LIST)


@case("add-top-down-codelets")
def _():
    def by_settings(settings):
        WS["settings"] = settings

        def by_seed(seed):
            reset()
            TOP_DOWN_LOG.clear()
            post_n(95, seed)
            set_global("%self-watching-enabled%", seed % 2 == 1)

            def body():
                coderack.add_top_down_codelets()
                tell(rack_obj(), "post-deferred-codelets")
                return [list(TOP_DOWN_LOG), rack()]
            return seeded(seed, body)
        return chez.map_(by_seed, [1, 2, 3])
    return chez.map_(by_settings, SETTINGS_LIST)


def reset_modes():
    set_global("%justify-mode%", False)
    set_global("%self-watching-enabled%", True)


TOP_LEVEL["threshold-distributions"] = reset_modes


# constants.ss: the translation-temperature threshold distributions --------------------

@case("threshold-distributions")
def _():
    def by_seed(seed):
        def body():
            return chez.map_(lambda d: [tell(d, "object-type"), repeat(12, lambda: tell(d, "choose-value"))],
                             [constants.p_very_low_translation_temperature_threshold_distribution,
                              constants.p_low_translation_temperature_threshold_distribution,
                              constants.p_medium_translation_temperature_threshold_distribution,
                              constants.p_high_translation_temperature_threshold_distribution,
                              constants.p_very_high_translation_temperature_threshold_distribution])
        return seeded(seed, body)
    return chez.map_(by_seed, SEEDS)


# setup.ss user commands (with logging windows) ----------------------------------------

CMD_LOG: list = []


def make_log_window(name):
    """coderack-battery.scm: b:make-log-window"""
    def fn(self, msg, *args):
        CMD_LOG.append([name, msg, *args])
        return False if msg == "verbose-mode?" else "done"
    return Lambda(fn)


@case("setup-commands")
def _():
    set_global("*comment-window*", make_log_window("comment"))
    set_global("*slipnet-window*", make_log_window("slipnet"))
    set_global("*coderack-window*", make_log_window("coderack"))
    set_global("*control-panel*", make_log_window("control-panel"))
    set_global("*display-mode?*", False)
    CMD_LOG.clear()
    s = setup
    r = []
    for command, flag in [(s.eliza_mode_off, "p_eliza_mode"), (s.eliza_mode_on, "p_eliza_mode"),
                          (s.slipnet_off, "p_slipnet_graphics"), (s.slipnet_on, "p_slipnet_graphics"),
                          (s.coderack_off, "p_coderack_graphics"), (s.coderack_on, "p_coderack_graphics"),
                          (s.codelet_counts_on, "p_codelet_count_graphics"),
                          (s.codelet_counts_off, "p_codelet_count_graphics")]:
        r.append(command())
        r.append(getattr(setup, flag))
    r.append(s.verbose_on())
    r.append(s.verbose_off())
    set_global("%slipnet-graphics%", False)
    set_global("%coderack-graphics%", False)
    return [r, list(CMD_LOG)]


# descriptions.ss: what can run before the Workspace is ported -------------------------

def make_fake_description(type_, descriptor):
    """coderack-battery.scm: b:make-fake-description"""
    def fn(self, msg, *args):
        if msg == "object-type":
            return "description"
        if msg == "get-description-type":
            return type_
        if msg == "get-descriptor":
            return descriptor
        return INVALID
    return Lambda(fn)


DESCS = chez.map_(lambda k: make_fake_description(k % 2, k % 3), iota(6))


@case("descriptions-equal")
def _():
    return chez.map_(lambda d1: chez.map_(lambda d2: descriptions.descriptions_equal_p(d1, d2), DESCS),
                     DESCS)


@case("description-member")
def _():
    return chez.map_(lambda d: [descriptions.description_member_p(d, []),
                                descriptions.description_member_p(d, [DESCS[0]]),
                                descriptions.description_member_p(d, DESCS[1:])],
                     DESCS)


@case("description-codelet-types-have-procedures")
def _():
    return chez.map_(lambda t: tell(t, "get-codelet-type-name"),
                     [ct("bottom-up-description-scout"), ct("top-down-description-scout"),
                      ct("description-evaluator"), ct("description-builder")])


# The tests ----------------------------------------------------------------------------

def test_every_battery_test_is_translated():
    assert list(CASES) == list(manifest("coderack"))
    assert len(CASES) == 43


@pytest.mark.parametrize("name", manifest("coderack"))
def test_coderack_battery(name):
    if name in TOP_LEVEL:
        TOP_LEVEL[name]()
    got = canon(CASES[name]())
    expected = fixture("coderack", name)
    same = got == expected      # not `assert got == expected`: pytest's diff of MB strings takes minutes
    assert same, first_difference(got, expected)


def first_difference(got, expected):
    i = next((k for k, (a, b) in enumerate(zip(got, expected)) if a != b), min(len(got), len(expected)))
    return (f"differs at character {i} of {len(expected)}:\n"
            f"  python: ...{got[max(0, i - 150):i + 150]}\n  chez:   ...{expected[max(0, i - 150):i + 150]}")


# Beyond the battery -------------------------------------------------------------------

MODULES = ((constants, "constants.ss"), (setup, "setup.ss"), (coderack, "coderack.ss"),
           (descriptions, "descriptions.ss"))


def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


# The graphics parts, translated by the GUI items (13-15), and names
# Racket also left out of the engine (racket/engine/constants.rktl, setup.rktl).
GRAPHICS = {"constants.ss": "window sizes, colours, fonts, titles",
            "setup.ss": {"setup", "enable-resizing"}}


def test_every_model_definition_has_its_python_function():
    for mod, origin in MODULES:
        names = defines(ORIGINAL / origin)
        if origin == "constants.ss":
            start = names.index("make-probability-distribution")
            names = names[start:]
        elif origin == "setup.ss":
            names = [n for n in names if n not in GRAPHICS["setup.ss"]]
        missing = [n for n in names if not hasattr(mod, scheme_to_python(n))]
        assert missing == [], (origin, missing)


def test_codelet_types_are_module_attributes_and_top_level_values():
    for t in coderack.g_codelet_types:
        name = tell(t, "get-codelet-type-name")
        assert getattr(coderack, scheme_to_python(name)) is t
        assert chez.top_level_value(name) is t


def test_description_codelet_types_have_procedures():
    for name in ["bottom-up-description-scout", "top-down-description-scout",
                 "description-evaluator", "description-builder"]:
        assert callable(ct(name).codelet_proc), name


def test_set_global_rejects_unknown_names():
    with pytest.raises(chez.SchemeError):
        engine.set_global("*no-such-global*", 1)


def test_docstrings_name_their_origin():
    for mod, origin in MODULES:
        for name, fn in vars(mod).items():
            if callable(fn) and getattr(fn, "__module__", None) == mod.__name__ and not name.startswith("_"):
                assert fn.__doc__ and fn.__doc__.split(":")[0] == origin, (mod.__name__, name)


def test_engine_modules_import_no_gui():
    for mod in (constants, setup, coderack, descriptions, engine,
                importlib.import_module("metacat.view_globals")):
        tree = ast.parse(inspect.getsource(mod))
        names = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
                 for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
        assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in names), mod


# python/oracle/batteries/coderack-extra-battery.scm ------------------------------------
# Written after the code (a mutation survived: adjust-urgency clipping with Python's
# min and max); same rules as the battery above.

EXTRA: dict = {}


def x_urgencies(type_):
    """coderack-extra-battery.scm: x:urgencies"""
    return chez.map_(lambda c: [tell(c, "get-relative-urgency"), bin_index(tell(c, "get-coderack-bin"))],
                     tell(rack_obj(), "get-codelets-of-type", type_))


def x_post_all(type_, urgencies):
    """coderack-extra-battery.scm: x:post-all"""
    for u in urgencies:
        set_global("*codelet-count*", setup.g_codelet_count + 1)
        tell(rack_obj(), "post", tell(type_, "make-codelet", u))


def x_adjust_urgency_clipping():
    def by_delta(delta):
        reset()
        x_post_all(ct("rule-scout"), [7.5, 33.3, 0, 100, 50, 99.99, F(1, 3), F(102, 5), 0.0, 100.0])
        tell(rack_obj(), "adjust-urgencies", ct("rule-scout"), delta)
        return x_urgencies(ct("rule-scout"))
    return chez.map_(by_delta, [-200, 500, -200.0, 500.0, 0.5, -0.5, F(1, 3), 0, 0.0])


def x_clamp_exactness():
    reset()
    bond_builder = ct("bond-builder")
    x_post_all(bond_builder, [10, 20.5, 30])
    r1 = tell(bond_builder, "clamp", 90)
    u1 = tell(bond_builder, "get-clamped-urgency")
    r2 = tell(bond_builder, "clamp", 90.0)
    u2 = tell(bond_builder, "get-clamped-urgency")
    a2 = x_urgencies(bond_builder)
    r3 = tell(bond_builder, "clamp", 45.5)
    u3 = tell(bond_builder, "get-clamped-urgency")
    a3 = x_urgencies(bond_builder)
    r4 = tell(bond_builder, "unclamp")
    a4 = x_urgencies(bond_builder)
    return [r1, u1, r2, u2, a2, r3, u3, a3, r4, a4]


EXTRA["adjust-urgency-clipping"] = x_adjust_urgency_clipping
EXTRA["clamp-exactness"] = x_clamp_exactness


def test_every_extra_test_is_translated():
    assert list(EXTRA) == list(manifest("coderack-extra"))


@pytest.mark.parametrize("name", manifest("coderack-extra"))
def test_coderack_extra_battery(name):
    got = canon(EXTRA[name]())
    expected = fixture("coderack-extra", name)
    same = got == expected
    assert same, first_difference(got, expected)
