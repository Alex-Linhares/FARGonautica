"""slipnet.py and images.py against Chez (loop0002 item 05).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/slipnet-battery.scm, translated: CASES maps each test
name to a function that rebuilds the battery expression in Python, in the same
order of random draws and side effects, and returns its value.  Its b:canon text
must equal the frozen Chez output in python/fixtures/slipnet/.  This covers the
initial slipnet as loaded (every node and link), and 20 calls of
update-slipnet-activations from fixed states with all activations, frozen flags
and the generator state after every call, compared to the last bit.

The battery's forms run in order and share the engine's state (the slipnodes'
activations, the coderack), so the cases run in the battery's order, in one
engine.  The battery's top-level forms between tests (installing its logging
monitor, the fake Themespace and the logging temp-adjusted-probability, the
reset before the images) run before the test they follow (TOP_LEVEL).  Globals
of files not translated yet are stand-ins (engine_stubs.engine_module):
run.ss's %update-cycle-length%, trace.ss's monitor-slipnode-activation-change,
formulas.ss's temp-adjusted-probability, *themespace* and *workspace*.  Every
battery `map` is chez.map_, since several of them draw random numbers or
change images midway.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import io
import re
from contextlib import ExitStack, redirect_stdout

import pytest

from chez_fixtures import chez as fixture, manifest
from engine_stubs import engine_module
from name_mapping import ORIGINAL
from scheme_canon import canon
from test_chez import SEEDS, repeat, seeded   # helpers.scm
from metacat import chez, utilities
from metacat.chez import String
from metacat.names import scheme_to_python
from metacat.objects import INVALID, Lambda, tell

from metacat import engine, setup, coderack, slipnet, images

CASES: dict = {}
TOP_LEVEL: dict = {}


def case(name):
    def register(fn):
        assert name not in CASES, name
        CASES[name] = fn
        return fn
    return register


def top_level(name):
    """A battery top-level form that runs just before test `name`."""
    def register(fn):
        TOP_LEVEL[name] = fn
        return fn
    return register


def capture(thunk):
    """helpers.scm: b:capture: what thunk prints, as a Scheme string."""
    out = io.StringIO()
    with redirect_stdout(out):
        thunk()
    return String(out.getvalue())


set_global = engine.set_global          # b:set-global!
map_ = chez.map_


def P(name):
    """A slipnode by its Scheme name (the battery refers to them as globals)."""
    return chez.top_level_value(name)


def nodes():
    return slipnet.g_slipnet_nodes


class Failed(Exception):
    pass


def try_(f):
    """slipnet-battery.scm: b:try: the value, or 'failed if fail was called."""
    def fail():
        raise Failed()
    try:
        return f(fail)
    except Failed:
        return "failed"


# Helpers ------------------------------------------------------------------------------

def name(node):
    """slipnet-battery.scm: b:name"""
    return tell(node, "get-name-symbol") if node is not False else False


def names(ns):
    """slipnet-battery.scm: b:names"""
    return map_(name, ns)


def deep(x):
    """slipnet-battery.scm: b:deep"""
    if isinstance(x, list):
        return [deep(y) for y in x]          # let*: car before cdr
    if callable(x):
        type_ = tell(x, "object-type")
        if type_ == "slipnode":
            return tell(x, "get-name-symbol")
        if type_ == "slipnet-link":
            return ["link", tell(x, "print-name")]
        if type_ == "image":
            return ["image", deep(tell(x, "generate"))]
        if type_ == "string-image":
            return ["string-image", deep(tell(x, "generate"))]
        return ["object", type_]
    return x


def link(l):
    """slipnet-battery.scm: b:link"""
    return [tell(l, "print-name"), tell(l, "get-link-type"),
            name(tell(l, "get-from-node")), name(tell(l, "get-to-node")),
            name(tell(l, "get-label-node")), tell(l, "get-link-length"),
            tell(l, "get-intrinsic-degree-of-assoc"), tell(l, "get-degree-of-assoc")]


def links(ls):
    """slipnet-battery.scm: b:links"""
    return map_(link, ls)


def node(n):
    """slipnet-battery.scm: b:node"""
    return [tell(n, "get-name-symbol"), tell(n, "get-short-name"),
            tell(n, "get-lowercase-name"), tell(n, "get-uppercase-name"),
            tell(n, "get-CM-short-name"), tell(n, "print-name"),
            tell(n, "get-conceptual-depth"), tell(n, "get-activation"),
            tell(n, "frozen?"), tell(n, "get-intrinsic-link-length"),
            tell(n, "get-shrunk-link-length"), tell(n, "get-degree-of-assoc"),
            tell(n, "category?"), tell(n, "instance?"),
            name(tell(n, "get-category")), names(tell(n, "get-instance-nodes")),
            tell(n, "get-graphics-coord"), tell(n, "get-graphics-label-coord")]


def node_links(n):
    """slipnet-battery.scm: b:node-links"""
    return [tell(n, "get-name-symbol"),
            ["incoming", links(tell(n, "get-incoming-links"))],
            ["category", links(tell(n, "get-category-links"))],
            ["property", links(tell(n, "get-property-links"))],
            ["lateral", links(tell(n, "get-lateral-links"))],
            ["sliplinks", links(tell(n, "get-lateral-sliplinks"))],
            ["labeled", links(tell(n, "get-links-labeled-by-node"))],
            ["outgoing", links(tell(n, "get-outgoing-links"))]]


def activations():
    """slipnet-battery.scm: b:activations"""
    return map_(lambda n: tell(n, "get-activation"), nodes())


def frozen():
    """slipnet-battery.scm: b:frozen"""
    return map_(lambda n: 0 if tell(n, "frozen?") is False else 1, nodes())


def relation_table(rel):
    """slipnet-battery.scm: b:relation-table"""
    return map_(lambda n1: String("".join(
        map_(lambda n2: "0" if rel(n1, n2) is False else "1", nodes()))), nodes())


def record_case(table):
    """A (lambda msg (record-case (cdr msg) ... (else 'invalid-message-indicator)))
    stand-in: table maps message names to procedures of the message's arguments."""
    def fn(self, msg, *args):
        if msg in table:
            return table[msg](*args)
        return INVALID
    return Lambda(fn)


# The initial slipnet, as loaded (before any reset) ------------------------------------

@case("slipnet-constants")
def _():
    return [slipnet.p_max_activation, slipnet.p_workspace_activation,
            slipnet.p_full_activation_threshold, len(nodes())]


@case("slipnet-node-lists")
def _():
    return [names(nodes()), names(slipnet.g_slipnet_letters),
            names(slipnet.g_slipnet_numbers), names(slipnet.g_top_down_slipnodes),
            names(slipnet.g_initially_clamped_slipnodes)]


@case("slipnet-nodes-initial")
def _():
    return map_(node, nodes())


@case("slipnet-links-initial")
def _():
    return map_(node_links, nodes())


@case("slipnet-link-count")
def _():
    return sum(map_(lambda n: len(tell(n, "get-outgoing-links")), nodes()))


@case("slipnet-node-top-level-values")
def _():
    return map_(lambda n: n is chez.top_level_value(tell(n, "get-name-symbol")), nodes())


@case("slipnet-link-top-level-values")
def _():
    def each_link(l):
        sfrom = tell(tell(l, "get-from-node"), "get-name-symbol")
        sto = tell(tell(l, "get-to-node"), "get-name-symbol")
        return l is chez.top_level_value(sfrom[6:] + "-" + sto[6:] + "-link")
    return map_(lambda n: map_(each_link, tell(n, "get-outgoing-links")), nodes())


@case("slipnet-print")
def _():
    return [capture(lambda: tell(P("plato-a"), "print")),
            capture(lambda: tell(P("plato-letter-category"), "print")),
            capture(lambda: tell(tell(P("plato-a"), "get-lateral-links")[0], "print"))]


# Relations between nodes --------------------------------------------------------------

@case("get-label-table")
def _():
    return map_(lambda n1: map_(lambda n2: name(slipnet.get_label(n1, n2)), nodes()), nodes())


@case("linked-table")
def _():
    return relation_table(slipnet.linked_p)


@case("related-table")
def _():
    return relation_table(slipnet.related_p)


@case("slip-linked-table")
def _():
    return relation_table(slipnet.slip_linked_p)


@case("relationship-between")
def _():
    a, b, c, d = P("plato-a"), P("plato-b"), P("plato-c"), P("plato-d")
    lists = [[a, b, c], [c, b, a], [a, a, a], [a, c], [a, b, d],
             [P("plato-one"), P("plato-two"), P("plato-three")],
             [P("plato-three"), P("plato-two")], [a, False, b],
             [P("plato-leftmost"), P("plato-rightmost")],
             [P("plato-left"), P("plato-right"), P("plato-left")]]
    return map_(lambda l: name(slipnet.relationship_between(l)), lists)


@case("relationship-between-one")
def _():
    return name(slipnet.relationship_between([P("plato-a")]))


@case("relationship-between-none")
def _():
    return name(slipnet.relationship_between([]))


RELATION_NODES = ["plato-identity", "plato-opposite", "plato-successor", "plato-predecessor",
                  "plato-group-category", "plato-bond-category", "plato-letter-category",
                  "plato-length"]


@case("get-related-node-table")
def _():
    rels = [P(r) for r in RELATION_NODES]
    return map_(lambda n: map_(lambda rel: name(tell(n, "get-related-node", rel)), rels), nodes())


@case("inverse-table")
def _():
    return map_(lambda n: name(slipnet.inverse(n)), [False] + nodes())


@case("platonic-predicates")
def _():
    return map_(lambda n: [slipnet.platonic_letter_p(n), slipnet.platonic_number_p(n),
                           slipnet.platonic_relation_p(n), slipnet.platonic_literal_p(n)],
                nodes())


@case("platonic-numbers")
def _():
    return [map_(lambda n: name(slipnet.number_to_platonic_number(n)), [1, 2, 3, 4, 5, 6, 7]),
            map_(slipnet.platonic_number_to_number, slipnet.g_slipnet_numbers)]


@case("coattail-slippage-probability")
def _():
    return map_(lambda n: map_(lambda l: slipnet.coattail_slippage_probability(False, False, n, l),
                               tell(n, "get-lateral-sliplinks")),
                nodes())


# Descriptor predicates, with fake workspace objects -----------------------------------

def make_fake_object(type_, length, spanning, leftmost, rightmost, middle, whole, letter_cat):
    """slipnet-battery.scm: b:make-fake-object"""
    return record_case({
        "object-type": lambda: type_,
        "get-group-length": lambda: length,
        "string-spanning-group?": lambda: spanning,
        "leftmost-in-string?": lambda: leftmost,
        "rightmost-in-string?": lambda: rightmost,
        "middle-in-string?": lambda: middle,
        "spans-whole-string?": lambda: whole,
        "get-descriptor-for": lambda cat: letter_cat if cat is P("plato-letter-category") else False})


def fake_objects():
    """slipnet-battery.scm: b:fake-objects"""
    a, b, m, z = P("plato-a"), P("plato-b"), P("plato-m"), P("plato-z")
    return [make_fake_object("letter", 1, False, True, False, False, False, a),
            make_fake_object("letter", 1, False, False, True, False, False, z),
            make_fake_object("letter", 1, False, False, False, True, False, m),
            make_fake_object("letter", 1, False, True, True, False, True, a),
            make_fake_object("group", 1, False, True, False, False, False, b),
            make_fake_object("group", 2, False, False, True, False, False, False),
            make_fake_object("group", 3, True, True, True, False, True, False),
            make_fake_object("group", 4, False, False, False, True, False, z),
            make_fake_object("group", 5, False, True, False, False, False, a),
            make_fake_object("group", 6, True, True, True, False, True, False)]


@case("possible-descriptor-table")
def _():
    objs = fake_objects()
    return map_(lambda n: map_(lambda o: 0 if tell(n, "possible-descriptor?", o) is False else 1,
                               objs),
                nodes())


@case("possible-descriptors")
def _():
    objs = fake_objects()
    return map_(lambda n: [name(n),
                           map_(lambda o: [tell(n, "description-possible?", o),
                                           names(tell(n, "get-possible-descriptors", o))],
                                objs)],
                utilities.filter_(lambda n: tell(n, "category?"), nodes()))


# Reset, clamping and the activation messages ------------------------------------------

MONITOR_LOG: list = []


def monitor(n, old, new):
    """slipnet-battery.scm: b:monitor"""
    MONITOR_LOG.append([name(n), old, new])


def take_monitor_log():
    """slipnet-battery.scm: b:take-monitor-log"""
    l = list(MONITOR_LOG)
    MONITOR_LOG.clear()
    return l


def reset_slipnet():
    """slipnet-battery.scm: b:reset-slipnet"""
    for n in nodes():
        tell(n, "reset")
    MONITOR_LOG.clear()


@top_level("reset-values")
def _():
    set_global("monitor-slipnode-activation-change", monitor)


@case("reset-values")
def _():
    reset_slipnet()
    return [activations(), frozen()]


@case("node-messages")
def _():
    n = P("plato-successor")
    r1 = tell(n, "set-activation", 30)
    a1 = tell(n, "get-activation")
    r2 = tell(n, "increment-activation-buffer", 25)
    r3 = tell(n, "flush-activation-buffer")
    a2 = tell(n, "get-activation")
    r4 = tell(n, "activate-from-workspace")
    r5 = tell(n, "flush-activation-buffer")
    a3 = tell(n, "get-activation")
    r6 = tell(n, "decrement-activation-buffer", 7)
    r7 = tell(n, "flush-activation-buffer")
    a4 = tell(n, "get-activation")
    r8 = tell(n, "freeze")
    r9 = tell(n, "set-activation", 3)
    r10 = tell(n, "update-activation", 4)
    r11 = tell(n, "increment-activation-buffer", 9)
    r12 = tell(n, "flush-activation-buffer")
    a5 = tell(n, "get-activation")
    r13 = tell(n, "unfreeze")
    r14 = tell(n, "update-activation", 64)
    a6 = tell(n, "get-activation")
    r15 = tell(n, "clamp", 100)
    a7 = [tell(n, "get-activation"), tell(n, "frozen?")]
    deg = [tell(n, "get-degree-of-assoc"), links(tell(n, "get-links-labeled-by-node"))]
    r16 = tell(n, "reset")
    return [[r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16],
            [a1, a2, a3, a4, a5, a6, a7], deg, take_monitor_log(), tell(n, "get-activation")]


@case("decay-and-spread-single")
def _():
    def each(n):
        reset_slipnet()
        tell(n, "set-activation", 100)
        tell(n, "decay-activation")
        tell(n, "spread-activation")
        for m in nodes():
            tell(m, "flush-activation-buffer")
        return [name(n), activations()]
    return map_(each, nodes())


# update-slipnet-activations: 20 updates from fixed states -----------------------------

THEME_LOG: list = []
ACTIVE_THEMES: list = []


def make_fake_theme(k, n, amount):
    """slipnet-battery.scm: b:make-fake-theme"""
    def spread():
        THEME_LOG.append(k)
        tell(n, "increment-activation-buffer", amount)
        return "done"
    return record_case({"object-type": lambda: "theme", "spread-activation-to-slipnet": spread})


FAKE_THEMESPACE = record_case({"object-type": lambda: "themespace",
                               "get-all-active-themes": lambda: list(ACTIVE_THEMES)})


@top_level("twenty-updates-k0")
def _():
    set_global("*themespace*", FAKE_THEMESPACE)


def set_state(k):
    """slipnet-battery.scm: b:set-state"""
    reset_slipnet()
    for i, n in enumerate(nodes()):
        tell(n, "set-activation", (i * 37 + k * 11) % 101)
    if k % 2 == 1:
        for n in slipnet.g_initially_clamped_slipnodes:
            tell(n, "clamp", 100)
    MONITOR_LOG.clear()
    THEME_LOG.clear()


def twenty_updates(k, seed, themes):
    """slipnet-battery.scm: b:twenty-updates"""
    set_state(k)
    ACTIVE_THEMES[:] = themes
    chez.random_seed(seed)
    acc = []
    for i in range(20):
        if i == 10:
            for n in slipnet.g_initially_clamped_slipnodes:
                tell(n, "unfreeze")
        slipnet.update_slipnet_activations()
        acts = activations()
        fr = frozen()
        s = chez.random_seed()
        acc.append([i, acts, fr, s])
    mlog = take_monitor_log()
    tlog = list(THEME_LOG)
    ACTIVE_THEMES.clear()
    return [acc, len(mlog), mlog, tlog]


case("twenty-updates-k0")(lambda: twenty_updates(0, 1, []))
case("twenty-updates-k1")(lambda: twenty_updates(1, 42, []))
case("twenty-updates-k2")(lambda: twenty_updates(
    2, 123456789, [make_fake_theme(1, P("plato-successor"), 30),
                   make_fake_theme(2, P("plato-rightmost"), 55)]))
case("twenty-updates-k3")(lambda: twenty_updates(
    3, 2147483648, [make_fake_theme(3, P("plato-opposite"), 80)]))
case("twenty-updates-k4")(lambda: twenty_updates(4, 3141592653, []))
case("twenty-updates-k5")(lambda: twenty_updates(5, 99, []))
case("twenty-updates-k6")(lambda: twenty_updates(6, 65536, []))
case("twenty-updates-k7")(lambda: twenty_updates(7, 7, [make_fake_theme(4, P("plato-a"), 100)]))


@case("twenty-updates-seeds")
def _():
    def each(seed):
        r = twenty_updates(seed % 9, seed, [])
        return map_(lambda step: [step[1], step[3]], r[0])
    return map_(each, SEEDS)


@case("twenty-updates-from-reset")
def _():
    reset_slipnet()
    for n in slipnet.g_initially_clamped_slipnodes:
        tell(n, "clamp", 100)
    chez.random_seed(5)

    def one():
        slipnet.update_slipnet_activations()
        return [activations(), chez.random_seed()]
    return repeat(20, one)


# Property links, slippages, top-down codelets -----------------------------------------

TAP_LOG: list = []


def logging_tap(p):
    TAP_LOG.append(p)
    return p


@top_level("similar-property-links")
def _():
    set_global("temp-adjusted-probability", logging_tap)


@case("similar-property-links")
def _():
    def each(seed):
        def thunk():
            TAP_LOG.clear()
            a = links(tell(P("plato-a"), "get-similar-property-links"))
            z = links(tell(P("plato-z"), "get-similar-property-links"))
            b = links(tell(P("plato-b"), "get-similar-property-links"))
            return [a, z, b, list(TAP_LOG)]
        return seeded(seed, thunk)
    return map_(each, SEEDS)


SLIP_LOG: list = []


def make_fake_slippage(d1, d2, label, cm_type):
    """slipnet-battery.scm: b:make-fake-slippage"""
    return record_case({"object-type": lambda: "concept-mapping",
                        "get-descriptor1": lambda: d1, "get-descriptor2": lambda: d2,
                        "get-label": lambda: label, "get-CM-type": lambda: cm_type})


def _applied(s):
    SLIP_LOG.append(["applied", name(tell(s, "get-descriptor1"))])
    return "done"


def _coattail(n, label, n2, s):
    SLIP_LOG.append(["coattail", name(n), name(label), name(n2), name(tell(s, "get-descriptor1"))])
    return "done"


FAKE_SLIPLOG = record_case({"object-type": lambda: "sliplog",
                            "applied": _applied, "coattail": _coattail})


def slippage_lists():
    """slipnet-battery.scm: b:slippage-lists"""
    return [
        [make_fake_slippage(P("plato-leftmost"), P("plato-rightmost"), P("plato-opposite"),
                            P("plato-string-position-category"))],
        [make_fake_slippage(P("plato-successor"), P("plato-predecessor"), P("plato-opposite"),
                            P("plato-bond-category"))],
        [make_fake_slippage(P("plato-left"), P("plato-right"), P("plato-opposite"),
                            P("plato-direction-category")),
         make_fake_slippage(P("plato-letter"), P("plato-group"), False, P("plato-object-category"))],
        [make_fake_slippage(P("plato-a"), P("plato-z"), P("plato-opposite"),
                            P("plato-letter-category")),
         make_fake_slippage(P("plato-one"), P("plato-two"), P("plato-successor"),
                            P("plato-length"))]]


SLIPPAGE_NODES = ["plato-leftmost", "plato-rightmost", "plato-successor", "plato-predecessor",
                  "plato-left", "plato-right", "plato-alphabetic-first", "plato-alphabetic-last",
                  "plato-letter", "plato-a", "plato-one", "plato-predgrp", "plato-succgrp",
                  "plato-single"]


def apply_slippages_to(slippages):
    def each(n):
        SLIP_LOG.clear()
        r = name(tell(n, "apply-slippages", slippages, FAKE_SLIPLOG))
        return [r, list(SLIP_LOG)]
    return map_(each, [P(n) for n in SLIPPAGE_NODES])


@case("apply-slippages")
def _():
    lists = slippage_lists()
    return map_(lambda seed: seeded(seed, lambda: map_(apply_slippages_to, lists)),
                [1, 2, 3, 42, 1000])


@case("apply-slippages-full-activation")
def _():
    reset_slipnet()
    tell(P("plato-opposite"), "clamp", 100)
    lists = slippage_lists()
    r = seeded(7, lambda: apply_slippages_to(lists[0]))
    reset_slipnet()
    return r


FAKE_WORKSPACE = record_case({
    "object-type": lambda: "workspace",
    "get-average-intra-string-unhappiness": lambda: 60,
    "get-min-mapping-strength": lambda: 30,
    "get-average-unhappiness": lambda: 45,
    "get-rough-num-of-unrelated-objects": lambda: "some",
    "get-bonds": lambda: ["b1"],
    "get-rough-num-of-ungrouped-objects": lambda: "many"})


def codelets():
    """slipnet-battery.scm: b:codelets"""
    return map_(lambda c: [tell(c, "get-codelet-type-name"), tell(c, "get-relative-urgency"),
                           deep(tell(c, "get-argument", 0)), deep(tell(c, "get-argument", 1))],
                tell(coderack.g_coderack, "get-all-codelets"))


@case("top-down-codelets")
def _():
    set_global("%coderack-graphics%", False)
    set_global("%workspace-graphics%", False)
    set_global("%slipnet-graphics%", False)
    set_global("*workspace*", FAKE_WORKSPACE)
    set_global("*codelet-count*", 0)
    set_global("*temperature*", 50)

    def each(seed):
        def thunk():
            reset_slipnet()
            tell(coderack.g_coderack, "initialize")
            for i, n in enumerate(nodes()):
                tell(n, "set-activation", (i * 29 + seed) % 101)
            for n in nodes():
                tell(n, "attempt-to-post-top-down-codelets")
            tell(coderack.g_coderack, "post-deferred-codelets")
            return codelets()
        return seeded(seed, thunk)
    return map_(each, [1, 2, 3, 42, 1000, 65535])


# Images -------------------------------------------------------------------------------

@top_level("image-operations")
def _():
    reset_slipnet()


def letter_images(ns):
    """slipnet-battery.scm: b:letter-images"""
    return map_(images.make_letter_image, ns)


def image_data(image):
    """slipnet-battery.scm: b:image-data"""
    return [deep(tell(image, "generate")), name(tell(image, "get-letter")),
            name(tell(image, "get-bond-facet")), name(tell(image, "get-letter-relation")),
            name(tell(image, "get-length-relation")), name(tell(image, "get-direction")),
            name(tell(image, "get-length")), tell(image, "letter-image?"),
            len(tell(image, "get-sub-images")), deep(tell(image, "get-swapped-image")),
            deep(tell(image, "get-instantiated-object"))]


def image_makers():
    """slipnet-battery.scm: b:image-makers"""
    mi, li = images.make_image, letter_images
    a, b, c, m, q = P("plato-a"), P("plato-b"), P("plato-c"), P("plato-m"), P("plato-q")
    x, y, z = P("plato-x"), P("plato-y"), P("plato-z")
    lc, succ, pred = P("plato-letter-category"), P("plato-successor"), P("plato-predecessor")
    iden, right, left, length = P("plato-identity"), P("plato-right"), P("plato-left"), P("plato-length")
    return [
        lambda: images.make_letter_image(a),
        lambda: mi(a, lc, succ, iden, right, li([a, b, c])),
        lambda: mi(c, lc, pred, iden, left, li([c, b, a])),
        lambda: mi(x, lc, succ, iden, right, li([x, y, z])),
        lambda: mi(m, lc, iden, iden, False, li([m, m, m])),
        lambda: mi(a, lc, succ, iden, right,
                   [mi(a, lc, iden, iden, False, li([a, a])),
                    mi(b, lc, iden, iden, False, li([b, b])),
                    mi(c, lc, iden, iden, False, li([c, c]))]),
        lambda: mi(a, length, succ, succ, right,
                   [mi(a, lc, iden, iden, False, li([a])),
                    mi(b, lc, iden, iden, False, li([b, b])),
                    mi(c, lc, iden, iden, False, li([c, c, c]))]),
        lambda: mi(a, lc, False, iden, right, li([a, q]))]


def operations():
    """slipnet-battery.scm: b:operations"""
    def t(msg, *args):
        return lambda i, fail: tell(i, msg, *args, fail)

    def succ_then_reverse(i, fail):
        tell(i, "new-start-letter", P("plato-successor"), fail)
        return tell(i, "reverse-medium", P("plato-letter-category"), fail)

    def length_succ_then_start_pred(i, fail):
        tell(i, "new-length", P("plato-successor"), fail)
        return tell(i, "new-start-letter", P("plato-predecessor"), fail)

    return [
        ["none", lambda i, fail: "done"],
        ["start-succ", t("new-start-letter", P("plato-successor"))],
        ["start-pred", t("new-start-letter", P("plato-predecessor"))],
        ["start-iden", t("new-start-letter", P("plato-identity"))],
        ["start-x", t("new-start-letter", P("plato-x"))],
        ["start-b", t("new-start-letter", P("plato-b"))],
        ["start-false", t("new-start-letter", False)],
        ["alpha-first", t("new-alpha-position-category", P("plato-alphabetic-first"))],
        ["alpha-last", t("new-alpha-position-category", P("plato-alphabetic-last"))],
        ["alpha-opp", t("new-alpha-position-category", P("plato-opposite"))],
        ["length-succ", t("new-length", P("plato-successor"))],
        ["length-pred", t("new-length", P("plato-predecessor"))],
        ["length-iden", t("new-length", P("plato-identity"))],
        ["length-one", t("new-length", P("plato-one"))],
        ["length-two", t("new-length", P("plato-two"))],
        ["length-five", t("new-length", P("plato-five"))],
        ["length-a", t("new-length", P("plato-a"))],
        ["reverse-dir", t("reverse-direction")],
        ["reverse-letters", t("reverse-medium", P("plato-letter-category"))],
        ["reverse-lengths", t("reverse-medium", P("plato-length"))],
        ["letter", t("letter")],
        ["group", t("group")],
        ["shorten", t("shorten")],
        ["extend-succ", t("extend", P("plato-successor"), P("plato-identity"))],
        ["extend-iden-succ", t("extend", P("plato-identity"), P("plato-successor"))],
        ["extend-a-three", t("extend", P("plato-a"), P("plato-three"))],
        ["singleton", t("letter->singleton-group")],
        ["succ-then-reverse", succ_then_reverse],
        ["length-succ-then-start-pred", length_succ_then_start_pred]]


@case("image-operations")
def _():
    ops = operations()

    def each(make):
        def one(op):
            image = make()
            before = image_data(image)
            r = try_(lambda fail: op[1](image, fail))
            after = image_data(image)
            copy = image_data(tell(image, "copy"))
            leaves = []
            tell(image, "leaf-walk", lambda i: leaves.append(name(tell(i, "get-letter"))))
            interior = []
            tell(image, "postorder-interior-walk", lambda i: interior.append(deep(tell(i, "generate"))))
            state = deep(tell(image, "get-state"))
            reset = tell(image, "reset")
            after_reset = image_data(image)
            return [op[0], before, r, after, copy, leaves, interior, state, reset, after_reset]
        return map_(one, ops)
    return map_(each, image_makers())


@case("image-swapped-and-state")
def _():
    makers = image_makers()
    i1 = makers[1]()
    i2 = makers[0]()
    r1 = tell(i1, "update-swapped-image", i2)
    s = tell(i1, "get-state")
    r2 = try_(lambda fail: tell(i1, "reverse-medium", P("plato-letter-category"), fail))
    mid = image_data(i1)
    r3 = tell(i1, "new-state", s)
    back = image_data(i1)
    r4 = tell(i1, "reset")
    return [r1, r2, mid, r3, back, r4, image_data(i1)]


@case("image-print")
def _():
    makers = image_makers()

    def each(k):
        image = makers[k]()
        return capture(lambda: tell(image, "print"))
    return map_(each, [0, 1, 2, 3, 5, 6, 7])


def make_fake_string(imgs):
    """slipnet-battery.scm: b:make-fake-string"""
    def constituents():
        return map_(lambda image: record_case({"object-type": lambda: "letter",
                                               "get-image": lambda: image}), imgs)
    return record_case({"object-type": lambda: "string", "get-constituent-objects": constituents})


def string_image_data(si):
    """slipnet-battery.scm: b:string-image-data"""
    return [deep(tell(si, "generate")), deep(tell(si, "get-sub-images")),
            deep(tell(si, "get-ordered-sub-images")), name(tell(si, "get-length"))]


def string_makers():
    """slipnet-battery.scm: b:string-makers"""
    makers = image_makers()
    return [lambda: letter_images([P("plato-a"), P("plato-b"), P("plato-c")]),
            lambda: letter_images([P("plato-x"), P("plato-y"), P("plato-z")]),
            lambda: [makers[1](), images.make_letter_image(P("plato-q"))],
            lambda: [makers[5](), makers[6]()]]


def string_operations():
    """slipnet-battery.scm: b:string-operations"""
    def t(msg, *args):
        return lambda si, fail: tell(si, msg, *args, fail)

    def replace_all(args):
        def op(si, fail):
            n = len(tell(si, "get-sub-images"))
            return tell(si, "replace-all", "new-length", [P(a) for a in args[:n]], fail)
        return op

    return [
        ["none", lambda si, fail: "done"],
        ["start-succ", t("new-start-letter", P("plato-successor"))],
        ["start-a", t("new-start-letter", P("plato-a"))],
        ["alpha", t("new-alpha-position-category", P("plato-predecessor"))],
        ["length", t("new-length", P("plato-two"))],
        ["appearance", lambda si, fail: tell(si, "new-appearance", [P("plato-q"), P("plato-r")])],
        ["reverse-dir", t("reverse-direction")],
        ["reverse-letters", t("reverse-medium", P("plato-letter-category"))],
        ["reverse-lengths", t("reverse-medium", P("plato-length"))],
        ["replace-all", replace_all(["plato-two", "plato-successor", "plato-three"])],
        ["replace-all-fail", replace_all(["plato-two", "plato-a", "plato-three"])],
        ["letter", t("letter")],
        ["group", t("group")]]


@case("string-image-operations")
def _():
    ops = string_operations()

    def each(make):
        def one(op):
            imgs = make()
            si = images.make_string_image(make_fake_string(imgs), P("plato-right"))
            before = string_image_data(si)
            r = try_(lambda fail: op[1](si, fail))
            after = string_image_data(si)
            w = []
            tell(si, "do-walk", "leaf-walk", lambda i: w.append(name(tell(i, "get-letter"))))
            reset = tell(si, "reset")
            after_reset = string_image_data(si)
            return [op[0], before, r, after, w, reset, after_reset, tell(si, "object-type")]
        return map_(one, ops)
    return map_(each, string_makers())


@case("string-image-left")
def _():
    si = images.make_string_image(
        make_fake_string(letter_images([P("plato-a"), P("plato-b"), P("plato-c")])),
        P("plato-left"))
    return string_image_data(si)


@case("change-length-first")
def _():
    curs = [False, P("plato-one"), P("plato-two"), P("plato-three"), P("plato-five")]
    args = [P(n) for n in ["plato-predecessor", "plato-successor", "plato-identity", "plato-one",
                           "plato-two", "plato-three", "plato-five", "plato-a"]]
    return map_(lambda arg: map_(lambda cur: images.change_length_first_p(arg, cur), curs), args)


@case("enumerate-letter")
def _():
    cases = [("plato-a", "plato-successor", 3), ("plato-x", "plato-successor", 3),
             ("plato-x", "plato-successor", 4), ("plato-c", "plato-predecessor", 3),
             ("plato-c", "plato-predecessor", 4), ("plato-m", "plato-identity", 5),
             ("plato-m", False, 1), ("plato-m", False, 2),
             ("plato-one", "plato-successor", 5), ("plato-one", "plato-successor", 6)]

    def each(args):
        start, relation, n = P(args[0]), (P(args[1]) if args[1] else False), args[2]
        return try_(lambda fail: names(images.enumerate_letter(start, relation, n, fail)))
    return map_(each, cases)


def format_slipnode(node):
    """rules.ss: format-slipnode (a stand-in until rules.py, item 17)"""
    return chez.format_("<~a>", tell(node, "get-short-name"))


@case("reveal-slipnodes")
def _():
    if "format-slipnode" not in chez.TOP_LEVEL:
        chez.define_top_level_value("format-slipnode", format_slipnode)
    length, succ, a = P("plato-length"), P("plato-successor"), P("plato-a")
    return [utilities.reveal(length),
            utilities.reveal([length, [succ, a], "x", 3]),
            utilities.reveal([[P("plato-letter-category"), P("plato-identity")],
                              [P("plato-string-position-category"), P("plato-opposite")]])]


# The engine, with the battery's stand-ins --------------------------------------------

def _no_monitor(*args):
    raise AssertionError("monitor-slipnode-activation-change called before the battery set it")


def _no_tap(*args):
    raise AssertionError("temp-adjusted-probability called before the battery set it")


# Globals of files not translated yet, which slipnet.ss reads.
STAND_INS = {
    "run": {"p_update_cycle_length": 15},
    "trace": {"monitor_slipnode_activation_change": _no_monitor},
    "formulas": {"temp_adjusted_probability": _no_tap},
    "themes": {"g_themespace": False},
    "workspace": {"g_workspace": False},
}


@pytest.fixture(scope="module", autouse=True)
def battery_engine():
    engine.load()
    with ExitStack() as stack:
        for mod, attrs in STAND_INS.items():
            stack.enter_context(engine_module(mod, **attrs))
        saved_setup = {k: getattr(setup, k) for k in vars(setup) if k.startswith(("g_", "p_"))}
        saved_top_level = dict(chez.TOP_LEVEL)
        yield
        for k, v in saved_setup.items():
            setattr(setup, k, v)
        chez.TOP_LEVEL.clear()
        chez.TOP_LEVEL.update(saved_top_level)
        reset_slipnet()
        tell(coderack.g_coderack, "initialize")


def first_difference(got, expected):
    i = next((k for k, (a, b) in enumerate(zip(got, expected)) if a != b), min(len(got), len(expected)))
    return (f"differs at character {i} of {len(expected)}:\n"
            f"  python: ...{got[max(0, i - 150):i + 150]}\n  chez:   ...{expected[max(0, i - 150):i + 150]}")


def test_every_battery_test_is_translated():
    assert list(CASES) == list(manifest("slipnet"))
    assert len(CASES) == 47


@pytest.mark.parametrize("name", manifest("slipnet"))
def test_slipnet_battery(name):
    if name in TOP_LEVEL:
        TOP_LEVEL[name]()
    expected = fixture("slipnet", name)
    if expected == "ERROR":
        # chez: car/cdr of '() is an error; Python's l[0] raises IndexError
        with pytest.raises((chez.SchemeError, IndexError)):
            CASES[name]()
        return
    got = canon(CASES[name]())
    same = got == expected      # not `assert got == expected`: pytest's diff of MB strings takes minutes
    assert same, first_difference(got, expected)


# Beyond the battery -------------------------------------------------------------------

MODULES = ((slipnet, "slipnet.ss"), (images, "images.ss"))


def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


def test_every_definition_has_its_python_function():
    for mod, origin in MODULES:
        missing = [n for n in defines(ORIGINAL / origin) if not hasattr(mod, scheme_to_python(n))]
        assert missing == [], (origin, missing)


def test_slipnodes_are_module_attributes_and_top_level_values():
    assert len(nodes()) == 59
    for n in nodes():
        sym = tell(n, "get-name-symbol")
        assert getattr(slipnet, scheme_to_python(sym)) is n
        assert chez.top_level_value(sym) is n


def test_docstrings_name_their_origin():
    for mod, origin in MODULES:
        for fname, fn in vars(mod).items():
            if callable(fn) and getattr(fn, "__module__", None) == mod.__name__ and not fname.startswith("_"):
                assert fn.__doc__ and fn.__doc__.split(":")[0] == origin, (mod.__name__, fname)


def test_engine_modules_import_no_gui():
    for mod in (slipnet, images):
        tree = ast.parse(inspect.getsource(mod))
        found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
                 for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
        assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), mod


def test_load_order_has_slipnet_and_images_after_formulas():
    order = engine.LOAD_ORDER
    assert order.index("coderack") < order.index("slipnet") < order.index("images")
    assert importlib.import_module("metacat.slipnet") in engine.translated_modules()


# python/oracle/batteries/slipnet-extra-battery.scm -------------------------------------
# Written after the code (mutations survived: Image's replace-all applied left to
# right, extend always changing the start letter first); same rules as above.

EXTRA: dict = {}


def x_names(x):
    """slipnet-extra-battery.scm: x:names"""
    if isinstance(x, list):
        return [x_names(y) for y in x]
    if callable(x):
        return tell(x, "get-name-symbol")
    return x


def x_abcde():
    """slipnet-extra-battery.scm: x:abcde"""
    return images.make_image(P("plato-a"), P("plato-letter-category"), P("plato-successor"),
                             P("plato-identity"), P("plato-right"),
                             letter_images([P(n) for n in ["plato-a", "plato-b", "plato-c",
                                                           "plato-d", "plato-e"]]))


def x_image_replace_all_fail():
    def each(k):
        image = x_abcde()
        args = map_(lambda i: False if i == k else slipnet.g_slipnet_letters[i + 20], [0, 1, 2, 3, 4])
        r = try_(lambda fail: tell(image, "replace-all", "new-start-letter", args, fail))
        return [k, r, x_names(tell(image, "generate"))]
    return map_(each, [0, 1, 2, 3, 4])


def x_xyz_of_groups():
    """slipnet-extra-battery.scm: x:xyz-of-groups"""
    return images.make_image(
        P("plato-x"), P("plato-length"), P("plato-identity"), P("plato-identity"), P("plato-right"),
        [images.make_image(P("plato-x"), P("plato-letter-category"), P("plato-successor"),
                           P("plato-identity"), P("plato-right"),
                           letter_images([P("plato-x"), P("plato-y"), P("plato-z")]))])


def x_extend_length_first():
    def each(args):
        image = x_xyz_of_groups()
        r = try_(lambda fail: tell(image, "extend", args[0], args[1], fail))
        return [x_names(args), r, x_names(tell(image, "generate")),
                x_names(tell(image, "get-letter-relation")),
                x_names(tell(image, "get-length-relation"))]
    pairs = [("plato-successor", "plato-predecessor"), ("plato-successor", "plato-one"),
             ("plato-successor", "plato-two"), ("plato-successor", "plato-three"),
             ("plato-successor", "plato-identity"), ("plato-predecessor", "plato-predecessor"),
             ("plato-identity", "plato-one"), ("plato-a", "plato-two")]
    return map_(each, [[P(a), P(b)] for a, b in pairs])


EXTRA["image-replace-all-fail"] = x_image_replace_all_fail
EXTRA["extend-length-first"] = x_extend_length_first
EXTRA["number-to-platonic-number-range"] = lambda: x_names(
    [slipnet.number_to_platonic_number(5), slipnet.number_to_platonic_number(6)])
EXTRA["number-to-platonic-number-zero"] = lambda: slipnet.number_to_platonic_number(0)


def test_every_extra_test_is_translated():
    assert list(EXTRA) == list(manifest("slipnet-extra"))


@pytest.mark.parametrize("name", manifest("slipnet-extra"))
def test_slipnet_extra_battery(name):
    expected = fixture("slipnet-extra", name)
    if expected == "ERROR":
        with pytest.raises(chez.SchemeError):
            EXTRA[name]()
        return
    got = canon(EXTRA[name]())
    assert got == expected, first_difference(got, expected)
