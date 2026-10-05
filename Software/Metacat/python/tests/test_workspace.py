"""The Workspace, its objects, strings and structures, and the formulas against Chez
(loop0002 item 06).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/workspace-battery.scm, translated, with the procedures of
tests/diff/workspace-dump.scm that it loads: CASES maps each test name to a
function that rebuilds the battery expression in Python, in the same order of
random draws and side effects, and returns its value.  Its b:canon text must equal
the frozen Chez output in python/fixtures/workspace/.  The main tests build the
initial workspace of every problem of tests/problems.txt (read at run time, as the
battery does) for each of its seeds, as run.ss's init-mcat builds it
(b:init-problem), and dump every string, letter, description, importance,
unhappiness and salience value, the workspace averages and mapping strengths, all
slipnet activations, the EEG messages and the generator state after each seed.

The battery's forms run in order and share the engine's state, so the cases run
in the battery's order, in one engine.  Its top-level forms (the fake Themespace,
the logging *EEG*, contains?, %workspace-graphics% off) are set up by the module
fixture, before the first test.  Globals of files not translated yet are
stand-ins (engine_stubs.engine_module): run.ss's %update-cycle-length% and
*temperature-clamped?*, themes.ss's *themespace*, eeg-graphics.ss's *EEG* and
groups.ss's contains? (its own definition, as the battery gives it).  Every
battery `map` whose procedure has effects is b:map-in-order (a Python loop) or
chez.map_; the queries' maps are pure.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import io
import re
from contextlib import ExitStack, redirect_stdout
from fractions import Fraction as F
from pathlib import Path

import pytest

from chez_fixtures import chez as fixture, manifest
from engine_stubs import engine_module
from name_mapping import ORIGINAL
from scheme_canon import canon
from test_chez import SEEDS, iota, repeat, seeded   # helpers.scm
from metacat import chez, utilities
from metacat.chez import String
from metacat.names import scheme_to_python
from metacat.objects import Lambda, tell

from metacat import engine, constants, setup, slipnet

REPO = Path(__file__).resolve().parents[2]
WS_MODULES = ("workspace", "workspace_objects", "workspace_structures", "workspace_strings",
              "workspace_structure_formulas", "formulas")

CASES: dict = {}


def case(name):
    def register(fn):
        assert name not in CASES, name
        CASES[name] = fn
        return fn
    return register


def M(name):
    """An engine module of this item, imported at call time (so that collection
    works before the module exists, and the cases fail one by one)."""
    return importlib.import_module("metacat." + name)


def capture(thunk):
    """b:capture: what thunk prints, as a Scheme string."""
    out = io.StringIO()
    with redirect_stdout(out):
        thunk()
    return String(out.getvalue())


set_global = engine.set_global          # b:set-global!
get_global = engine.get_global


def P(name):
    """A slipnode by its Scheme name (the battery refers to them as globals)."""
    return chez.top_level_value(name)


def ws():
    return get_global("*workspace*")


def g(name):
    return get_global(name)


class Unexpected(chez.SchemeError):
    pass


# workspace-dump.scm ---------------------------------------------------------------------

def read_problems():
    """workspace-dump.scm: b:problems (tests/problems.txt as (strings seeds))"""
    problems = []
    for line in (REPO / "tests" / "problems.txt").read_text().split("\n"):
        line = line.split("#")[0]
        fields = [f for f in line.split("|") if f != ""]
        if len(fields) < 3:
            continue
        strings = [w for w in fields[0].split(" ") if w != ""]
        seeds = [int(w) for w in fields[1].split(" ") if w != ""]
        problems.append([strings, seeds])
    return problems


PROBLEMS = read_problems()


def add_string_position_descriptions_to_letters(string):
    """workspace-dump.scm: b:add-string-position-descriptions-to-letters"""
    string_length = tell(string, "get-length")
    leftmost_letter = tell(string, "get-letter", 0)
    if string_length == 1:
        tell(leftmost_letter, "new-description", P("plato-string-position-category"),
             P("plato-single"))
    else:
        rightmost_letter = tell(string, "get-letter", string_length - 1)
        tell(leftmost_letter, "new-description", P("plato-string-position-category"),
             P("plato-leftmost"))
        tell(rightmost_letter, "new-description", P("plato-string-position-category"),
             P("plato-rightmost"))
        if string_length % 2 == 1:
            middle_letter = tell(string, "get-letter", string_length // 2)
            tell(middle_letter, "new-description", P("plato-string-position-category"),
                 P("plato-middle"))


def update_workspace_values():
    """workspace-dump.scm: b:update-workspace-values"""
    for structure in tell(ws(), "get-structures"):
        tell(structure, "update-strength")
    objects = tell(ws(), "get-objects")
    for obj in objects:
        tell(obj, "update-raw-importance")
    tell(g("*initial-string*"), "update-all-relative-importances")
    tell(g("*modified-string*"), "update-all-relative-importances")
    tell(g("*target-string*"), "update-all-relative-importances")
    if g("%justify-mode%") is not False:
        tell(g("*answer-string*"), "update-all-relative-importances")
    for obj in objects:
        tell(obj, "update-object-values")
    tell(g("*initial-string*"), "update-average-intra-string-unhappiness")
    tell(g("*modified-string*"), "update-average-intra-string-unhappiness")
    tell(g("*target-string*"), "update-average-intra-string-unhappiness")
    if g("%justify-mode%") is not False:
        tell(g("*answer-string*"), "update-average-intra-string-unhappiness")
    tell(ws(), "update-average-unhappiness-values")


def init_problem(strings, seed):
    """workspace-dump.scm: b:init-problem"""
    answer_sym = strings[3] if len(strings) == 4 else False
    set_global("%justify-mode%", answer_sym is not False)
    chez.random_seed(seed)
    set_global("*codelet-count*", 0)
    set_global("*temperature*", 100)
    set_global("*temperature-clamped?*", False)
    for node in slipnet.g_slipnet_nodes:
        tell(node, "reset")
    mws = M("workspace_strings").make_workspace_string
    # init-workspace, in Chez's order: the let's inits last first
    answer_string = mws("answer", answer_sym) if answer_sym is not False else False
    target_string = mws("target", strings[2])
    modified_string = mws("modified", strings[1])
    initial_string = mws("initial", strings[0])
    tell(ws(), "initialize", initial_string, modified_string, target_string, answer_string)
    set_global("*initial-string*", initial_string)
    set_global("*modified-string*", modified_string)
    set_global("*target-string*", target_string)
    set_global("*answer-string*", answer_string)
    set_global("*top-strings*", [initial_string, modified_string])
    set_global("*bottom-strings*", [target_string, answer_string])
    set_global("*vertical-strings*", [initial_string, target_string])
    set_global("*non-answer-strings*", [initial_string, modified_string, target_string])
    set_global("*all-strings*", [initial_string, modified_string, target_string, answer_string])
    add_string_position_descriptions_to_letters(g("*initial-string*"))
    add_string_position_descriptions_to_letters(g("*modified-string*"))
    add_string_position_descriptions_to_letters(g("*target-string*"))
    justify = g("%justify-mode%") is not False
    if justify:
        add_string_position_descriptions_to_letters(g("*answer-string*"))
    if (tell(g("*initial-string*"), "get-length") == 1
            or tell(g("*modified-string*"), "get-length") == 1
            or tell(g("*target-string*"), "get-length") == 1
            or (justify and tell(g("*answer-string*"), "get-length") == 1)):
        tell(P("plato-object-category"), "set-activation", slipnet.p_max_activation)
    for obj in tell(ws(), "get-objects"):
        for descriptor in utilities.tell_all(tell(obj, "get-descriptions"), "get-descriptor"):
            tell(descriptor, "set-activation", slipnet.p_max_activation)
    update_workspace_values()
    return "done"


def nm(node):
    """workspace-dump.scm: b:nm"""
    return tell(node, "get-name-symbol") if node is not False else False


def deep_nm(x):
    """workspace-dump.scm: b:deep-nm"""
    if isinstance(x, list):
        return [deep_nm(y) for y in x]          # let*: car before cdr
    if callable(x):
        return nm(x)
    return x


def dump_description(d):
    """workspace-dump.scm: b:dump-description"""
    return [nm(tell(d, "get-description-type")), nm(tell(d, "get-descriptor")),
            tell(d, "print-name"), tell(d, "get-proposal-level"),
            tell(d, "get-strength"), tell(d, "get-time-stamp")]


def dump_object(obj):
    """workspace-dump.scm: b:dump-object"""
    return [tell(obj, "object-type"), tell(obj, "ascii-name"), tell(obj, "print-name"),
            tell(obj, "get-id-num"), tell(obj, "get-left-string-pos"),
            tell(obj, "get-right-string-pos"), tell(obj, "which-string"),
            nm(tell(obj, "get-letter-category")),
            chez.map_(dump_description, tell(obj, "get-descriptions")),
            ["importance", tell(obj, "get-raw-importance"), tell(obj, "get-relative-importance")],
            ["unhappiness", tell(obj, "get-intra-string-unhappiness"),
             tell(obj, "get-inter-string-unhappiness", "horizontal"),
             tell(obj, "get-inter-string-unhappiness", "vertical"),
             tell(obj, "get-average-unhappiness")],
            ["salience", tell(obj, "get-intra-string-salience"),
             tell(obj, "get-inter-string-salience", "horizontal"),
             tell(obj, "get-inter-string-salience", "vertical"),
             tell(obj, "get-average-salience")],
            ["bonds", tell(obj, "get-left-bond"), tell(obj, "get-right-bond"),
             tell(obj, "get-enclosing-group"),
             tell(obj, "get-bridge", "horizontal"), tell(obj, "get-bridge", "vertical")],
            deep_nm(tell(tell(obj, "get-image"), "generate"))]


def dump_string(s):
    """workspace-dump.scm: b:dump-string"""
    if s is False:
        return False
    return [tell(s, "object-type"), tell(s, "print-name"), tell(s, "ascii-name"),
            tell(s, "generic-name"), tell(s, "symbol-name"), tell(s, "get-string-type"),
            tell(s, "get-length"), tell(s, "get-max-object-capacity"),
            chez.map_(nm, tell(s, "get-letter-categories")),
            tell(s, "get-average-intra-string-unhappiness"),
            len(tell(s, "get-groups")), len(tell(s, "get-all-bonds")),
            deep_nm(tell(s, "generate-image-letters")),
            chez.map_(dump_object, tell(s, "get-objects"))]


def dump_workspace():
    """workspace-dump.scm: b:dump-workspace"""
    w = ws()
    return [chez.map_(dump_string, g("*all-strings*")),
            ["ids", chez.map_(lambda o: tell(o, "get-id-num"), tell(w, "get-objects"))],
            ["workspace",
             tell(w, "get-average-intra-string-unhappiness"),
             tell(w, "get-average-inter-string-unhappiness", "top"),
             tell(w, "get-average-inter-string-unhappiness", "bottom"),
             tell(w, "get-average-inter-string-unhappiness", "vertical"),
             tell(w, "get-average-unhappiness"),
             tell(w, "get-mapping-strength", "top"),
             tell(w, "get-mapping-strength", "bottom"),
             tell(w, "get-mapping-strength", "vertical"),
             len(tell(w, "get-structures")),
             tell(w, "get-possible-rule-types"),
             len(tell(w, "get-all-proposed-bridges"))]]


def activations():
    """workspace-dump.scm: b:activations"""
    return chez.map_(lambda n: [nm(n), tell(n, "get-activation"), tell(n, "frozen?")],
                     slipnet.g_slipnet_nodes)


# workspace-battery.scm: fakes and helpers -------------------------------------------------

def fake_themespace_fn(self, msg, *args):
    """workspace-battery.scm: b:fake-themespace"""
    if msg == "get-active-themes":
        return []
    raise Unexpected("fake-themespace", "unexpected message ~s", msg)


EEG_LOG: list = []


def fake_eeg_fn(self, msg, *args):
    """workspace-battery.scm: b:fake-eeg"""
    EEG_LOG.insert(0, msg)
    return "done"


def contains_p(object1, object2):
    """workspace-battery.scm: the battery's contains? (groups.ss's own definition)"""
    return utilities.group_p(object1) and tell(object1, "nested-member?", object2)


def seq(*thunks):
    """workspace-battery.scm: b:seq (thunks in order)"""
    return [t() for t in thunks]


def map_in_order(f, l):
    """workspace-battery.scm: b:map-in-order"""
    return [f(x) for x in l]


def same_p(a, b):
    """workspace-battery.scm: b:same?"""
    return canon(a) == canon(b)


def string_name(s):
    return tell(s, "print-name") if s is not False else False


def ascii(o):
    """workspace-battery.scm: b:ascii"""
    return tell(o, "ascii-name") if o is not False else False


def asciis(l):
    return chez.map_(ascii, l)


def problem_dump(i):
    """workspace-battery.scm: b:problem-dump"""
    strings, seeds = PROBLEMS[i]
    init_problem(strings, seeds[0])
    first = dump_workspace()
    acts = activations()
    eeg = list(reversed(EEG_LOG))

    def each(seed):
        init_problem(strings, seed)
        d = dump_workspace()
        state = chez.random_seed()
        return [seed, state, same_p(d, first)]
    per_seed = map_in_order(each, seeds)
    EEG_LOG.clear()
    return [strings, first, acts, eeg, per_seed]


def problem_dumps_from(i):
    """workspace-battery.scm: b:problem-dumps-from"""
    out = []
    while i < len(PROBLEMS):
        out.append(problem_dump(i))
        i += 1
    return out


@case("ws-problem-count")
def _():
    return len(PROBLEMS)


for _i in range(36):
    CASES[f"ws-problem-{_i:02d}"] = (lambda i: lambda: problem_dump(i))(_i)

CASES["ws-problem-rest"] = lambda: problem_dumps_from(36)


# Queries on the initial workspace of a problem ------------------------------------------

def types_():
    """workspace-battery.scm: b:types"""
    return [P("plato-object-category"), P("plato-letter-category"),
            P("plato-string-position-category"), P("plato-alphabetic-position-category"),
            P("plato-length"), P("plato-group-category"), P("plato-direction-category"),
            P("plato-bond-category")]


def print_names(ds):
    return chez.map_(lambda d: tell(d, "print-name"), ds)


def object_queries(obj):
    """workspace-battery.scm: b:object-queries"""
    wsm = M("workspace")
    nodes = slipnet.g_slipnet_nodes
    return [ascii(obj),
            chez.map_(nm, tell(obj, "get-all-description-types")),
            chez.map_(nm, tell(obj, "get-all-descriptors")),
            chez.map_(lambda d: tell(d, "relevant?"), tell(obj, "get-descriptions")),
            print_names(tell(obj, "get-relevant-descriptions")),
            print_names(tell(obj, "get-distinguishing-descriptions")),
            print_names(tell(obj, "get-relevant-distinguishing-descriptions")),
            print_names(tell(obj, "get-descriptions-for-rule")),
            deep_nm(tell(obj, "get-concept-pattern")),
            chez.map_(lambda t: nm(tell(obj, "get-descriptor-for", t)), types_()),
            chez.map_(lambda t: tell(obj, "description-type-present?", t), types_()),
            tell(obj, "all-description-types-present?", types_()),
            chez.map_(lambda n: tell(obj, "descriptor-present?", n), nodes),
            chez.map_(lambda n: tell(obj, "distinguishing-descriptor?", n), nodes),
            asciis(tell(obj, "get-all-left-neighbors")),
            asciis(tell(obj, "get-all-right-neighbors")),
            ascii(tell(obj, "get-ungrouped-left-neighbor")),
            ascii(tell(obj, "get-ungrouped-right-neighbor")),
            [tell(obj, "leftmost-in-string?"), tell(obj, "middle-in-string?"),
             tell(obj, "rightmost-in-string?"), tell(obj, "spans-whole-string?"),
             tell(obj, "string-spanning-group?"), tell(obj, "get-letter-span"),
             tell(obj, "get-nesting-level"), tell(obj, "get-num-of-incident-bonds"),
             tell(obj, "get-num-of-spanning-bridges"),
             tell(obj, "mapped?", "vertical"), tell(obj, "mapped?", "horizontal"),
             tell(obj, "mapped?", "both")],
            [wsm.unrelated_p(obj), wsm.ungrouped_p(obj), wsm.unmapped_p(obj)],
            [tell(obj, "get-letters"), tell(obj, "nested-member?", obj),
             tell(obj, "singleton-group?"), tell(obj, "make-flipped-version") is obj,
             nm(tell(obj, "get-platonic-length")),
             nm(tell(obj, "get-initial-letter-category")),
             tell(obj, "in-string?", tell(obj, "get-string"))]]


def string_queries(s):
    """workspace-battery.scm: b:string-queries"""
    wsm, wsf, wso = M("workspace"), M("workspace_structure_formulas"), M("workspace_objects")
    return [tell(s, "print-name"),
            [tell(s, "top-string?"), tell(s, "vertical-string?"), tell(s, "bottom-string?"),
             tell(s, "translated?"), tell(s, "string-type?", "initial")],
            chez.map_(lambda c: tell(s, "get-bond-category-relevance", c),
                      [P("plato-sameness"), P("plato-successor"), P("plato-predecessor")]),
            chez.map_(lambda d: tell(s, "get-direction-relevance", d),
                      [P("plato-left"), P("plato-right")]),
            wsm.spanning_group_possible_p(s),
            asciis(tell(s, "get-constituent-objects")),
            asciis(tell(s, "get-top-level-objects")),
            tell(s, "singleton-group?"),
            tell(s, "whole-group?"),
            tell(s, "spanning-group-exists?"),
            tell(s, "get-spanning-group"),
            [tell(s, "get-left-string-pos"), tell(s, "get-right-string-pos"),
             tell(s, "get-nesting-level"), nm(tell(s, "get-bond-facet")),
             tell(s, "get-bridge", "vertical")],
            chez.map_(lambda t: wsf.description_type_support(t, s), types_()),
            chez.map_(lambda n: wsf.descriptor_support(n, s), slipnet.g_slipnet_nodes),
            ascii(wso.lowest_level_object(tell(s, "get-objects"))),
            ascii(wso.highest_level_object(tell(s, "get-objects"))),
            map_in_order(
                lambda od: asciis(tell(s, "get-object-description-ref-objects", od)),
                [["string", P("plato-string-position-category"), P("plato-whole")],
                 [P("plato-letter"), P("plato-letter-category"), P("plato-a")],
                 [P("plato-letter"), P("plato-string-position-category"), P("plato-leftmost")],
                 [P("plato-letter"), P("plato-string-position-category"), P("plato-rightmost")],
                 [P("plato-letter"), P("plato-string-position-category"), P("plato-middle")],
                 [P("plato-group"), P("plato-group-category"), P("plato-samegrp")]])]


def threshold_distributions():
    """workspace-battery.scm: b:threshold-distributions"""
    return [constants.p_very_low_translation_temperature_threshold_distribution,
            constants.p_low_translation_temperature_threshold_distribution,
            constants.p_medium_translation_temperature_threshold_distribution,
            constants.p_high_translation_temperature_threshold_distribution,
            constants.p_very_high_translation_temperature_threshold_distribution]


def index_of(x, l):
    """workspace-battery.scm: b:index-of (by eq?)"""
    for i, y in enumerate(l):
        if y is x:
            return i
    return False


def workspace_queries():
    """workspace-battery.scm: b:workspace-queries.  check-if-rules-possible sets
    the rule possibilities that later items read; at the start of a run they are
    #f either way, so the list's order of evaluation does not matter."""
    w = ws()
    formulas = M("formulas")

    def temperature():
        formulas.update_temperature()
        return g("*temperature*")
    return [asciis(tell(w, "get-objects")),
            asciis(tell(w, "get-all-letters")),
            [tell(w, "get-bonds"), tell(w, "get-groups"), tell(w, "get-all-groups"),
             tell(w, "get-all-bridges"), tell(w, "get-all-rules"), tell(w, "get-clamped-rules")],
            asciis(tell(w, "get-possible-bridge-objects", "top")),
            asciis(tell(w, "get-possible-bridge-objects", "vertical")),
            map_in_order(lambda s: [string_name(tell(w, "get-other-string", s, "horizontal")),
                                    string_name(tell(w, "get-other-string", s, "vertical"))],
                         [g("*initial-string*"), g("*target-string*")]),
            tell(w, "get-activity"),
            tell(w, "get-youngest-structures-average-age"),
            tell(w, "get-max-inter-string-unhappiness"),
            tell(w, "get-min-mapping-strength"),
            chez.map_(lambda t: tell(w, "maximal-mapping?", t), ["top", "vertical"]),
            chez.map_(lambda t: tell(w, "spanning-bridge-exists?", t), ["top", "vertical"]),
            chez.map_(lambda t: tell(w, "get-all-slippages", t), ["top", "vertical"]),
            tell(w, "get-all-vertical-CMs"),
            tell(w, "get-proposed-bridges", "top"),
            tell(w, "get-proposed-vertical-bridges", tell(g("*initial-string*"), "get-letter", 0)),
            tell(w, "get-proposed-horizontal-bridges",
                 tell(g("*modified-string*"), "get-letter", 0)),
            tell(w, "object-exists?", tell(g("*target-string*"), "get-letter", 0)),
            tell(w, "check-if-rules-possible"),
            tell(w, "get-possible-rule-types"),
            chez.map_(lambda t: tell(w, "rule-possible?", t), ["top", "bottom"]),
            chez.map_(lambda t: tell(w, "supported-rule-exists?", t), ["top", "bottom"]),
            index_of(formulas.current_translation_temperature_threshold_distribution(),
                     threshold_distributions()),
            temperature()]


def seeded_choices(seed, temperature):
    """workspace-battery.scm: b:seeded-choices"""
    set_global("*temperature*", temperature)

    def thunk():
        w = ws()
        objects = tell(w, "get-objects")
        i, m, t = g("*initial-string*"), g("*modified-string*"), g("*target-string*")

        def description_name(d):
            return tell(d, "print-name") if d is not False else False

        def per_object(obj):
            return seq(
                lambda: ascii(tell(obj, "choose-left-neighbor")),
                lambda: ascii(tell(obj, "choose-right-neighbor")),
                lambda: ascii(tell(obj, "choose-neighbor")),
                lambda: description_name(tell(obj, "choose-relevant-description-by-activation")),
                lambda: description_name(
                    tell(obj, "choose-relevant-distinguishing-description-by-depth")))
        return seq(
            lambda: ascii(tell(w, "choose-object", "get-intra-string-salience")),
            lambda: ascii(tell(w, "choose-object", "get-inter-string-salience", "vertical")),
            lambda: ascii(tell(w, "choose-object", "get-relative-importance")),
            lambda: ascii(tell(i, "choose-object", "get-average-salience")),
            lambda: ascii(tell(t, "choose-object-with-description-type",
                               P("plato-string-position-category"), "get-relative-importance")),
            lambda: ascii(tell(t, "choose-object-with-description-type",
                               P("plato-length"), "get-relative-importance")),
            lambda: ascii(tell(m, "choose-leftmost-object")),
            lambda: ascii(tell(t, "get-random-letter")),
            lambda: repeat(5, lambda: tell(t, "get-num-of-bonds-to-scan")),
            lambda: tell(w, "get-rough-num-of-unrelated-objects"),
            lambda: tell(w, "get-rough-num-of-ungrouped-objects"),
            lambda: tell(w, "get-rough-num-of-unmapped-objects"),
            lambda: map_in_order(per_object, objects))
    return seeded(seed, thunk)


def problem_queries(i):
    """workspace-battery.scm: b:problem-queries"""
    strings, seeds = PROBLEMS[i]
    init_problem(strings, seeds[0])
    objects = map_in_order(object_queries, tell(ws(), "get-objects"))
    strings_q = map_in_order(string_queries,
                             g("*all-strings*") if g("%justify-mode%") is not False
                             else [g("*initial-string*"), g("*modified-string*"),
                                   g("*target-string*")])
    wsq = workspace_queries()
    choices = map_in_order(lambda st: seeded_choices(st[0], st[1]),
                           [[1, 100], [42, 100], [7, 35], [3141592653, 0]])
    return [strings, objects, strings_q, wsq, choices]


for _i in (0, 6, 7, 14, 15, 16, 21, 22, 25, 28):
    CASES[f"ws-queries-{_i:02d}"] = (lambda i: lambda: problem_queries(i))(_i)


# Workspace objects with fake bonds, groups and bridges ----------------------------------

def fake_structure(type_, strength, **props):
    """workspace-battery.scm: b:fake-structure (props by message name, with
    Scheme names: fake_structure('bond', 50, **{'get-bond-category': ...}))"""
    def fn(self, msg, *args):
        if msg == "object-type":
            return type_
        if msg == "get-strength":
            return strength
        if msg == "update-strength":
            return "done"
        if msg in props:
            return props[msg](*args)
        raise Unexpected("fake-structure", "unexpected message ~s", msg)
    return Lambda(fn)


def object_values(obj):
    """workspace-battery.scm: b:object-values"""
    return [ascii(obj),
            tell(obj, "get-intra-string-unhappiness"),
            tell(obj, "get-inter-string-unhappiness", "horizontal"),
            tell(obj, "get-inter-string-unhappiness", "vertical"),
            tell(obj, "get-average-unhappiness"),
            tell(obj, "get-intra-string-salience"),
            tell(obj, "get-inter-string-salience", "horizontal"),
            tell(obj, "get-inter-string-salience", "vertical"),
            tell(obj, "get-average-salience"),
            tell(obj, "get-raw-importance"),
            tell(obj, "get-relative-importance")]


def all_object_values():
    """workspace-battery.scm: b:all-object-values"""
    return chez.map_(object_values, tell(ws(), "get-objects"))


def workspace_averages():
    w = ws()
    return [tell(w, "get-average-intra-string-unhappiness"),
            tell(w, "get-average-inter-string-unhappiness", "top"),
            tell(w, "get-average-inter-string-unhappiness", "bottom"),
            tell(w, "get-average-inter-string-unhappiness", "vertical"),
            tell(w, "get-average-unhappiness"),
            tell(w, "get-mapping-strength", "top"),
            tell(w, "get-mapping-strength", "bottom"),
            tell(w, "get-mapping-strength", "vertical")]


def with_structures(strengths):
    """workspace-battery.scm: b:with-structures"""
    wsm = M("workspace")
    init_problem(["abc", "abd", "ijk", "abd"], 1)
    i = tell(g("*initial-string*"), "get-letters")
    m = tell(g("*modified-string*"), "get-letters")
    t = tell(g("*target-string*"), "get-letters")
    a = tell(g("*answer-string*"), "get-letters")
    s1, s2, s3 = strengths
    bond = fake_structure("bond", s1, **{"get-bond-category": lambda: P("plato-successor")})
    sbond = fake_structure("bond", s2, **{"get-bond-category": lambda: P("plato-sameness")})
    hbridge = fake_structure("bridge", s3)
    vbridge = fake_structure("bridge", s1)
    gbridge = fake_structure("bridge", s2)
    group = fake_structure("group", s3, **{
        "get-bridge": lambda o: gbridge if o == "horizontal" else False,
        "nested-member?": lambda o: False,
        "get-nesting-level": lambda: 0})
    # initial: a-b bonded (a leftmost: one bond; b middle: two bonds)
    tell(i[0], "update-right-bond", bond)
    tell(i[1], "update-left-bond", bond)
    tell(i[1], "update-right-bond", sbond)
    tell(i[2], "update-left-bond", sbond)
    tell(i[0], "add-outgoing-bond", bond)
    tell(i[1], "add-incoming-bond", sbond)
    # bridges
    tell(i[0], "update-bridge", "horizontal", hbridge)
    tell(i[0], "update-bridge", "vertical", vbridge)
    tell(t[0], "update-bridge", "vertical", vbridge)
    tell(m[0], "update-bridge", "horizontal", hbridge)
    tell(a[0], "update-bridge", "horizontal", gbridge)
    # a group around the target's b and c, and one around the answer's
    tell(t[1], "update-enclosing-group", group)
    tell(t[2], "update-enclosing-group", group)
    tell(a[1], "update-enclosing-group", group)
    tell(m[1], "clamp-salience")
    update_workspace_values()
    v = all_object_values()
    wsv = workspace_averages()
    misc = [tell(i[0], "get-num-of-incident-bonds"),
            tell(i[1], "get-nesting-level"),
            wsm.unrelated_p(i[0]), wsm.unrelated_p(i[1]),
            wsm.ungrouped_p(t[1]), wsm.unmapped_p(i[0]), wsm.unmapped_p(t[0]),
            wsm.unmapped_p(m[0]), wsm.unmapped_p(a[0]),
            ascii(tell(t[1], "get-ungrouped-left-neighbor")),
            ascii(tell(t[0], "get-ungrouped-right-neighbor")),
            print_names(tell(m[1], "get-distinguishing-descriptions"))]
    tell(m[1], "unclamp-salience")
    return [v, wsv, misc]


CASES["ws-structures-1"] = lambda: with_structures([100, 50, 0])
CASES["ws-structures-2"] = lambda: with_structures([37, 81, 99])
CASES["ws-structures-3"] = lambda: with_structures([0, 0, 0])
CASES["ws-structures-4"] = lambda: with_structures([13, 100, 61])


@case("ws-descriptions")
def _():
    init_problem(["abc", "abd", "xyz"], 1)
    x = tell(g("*target-string*"), "get-letter", 0)
    return seq(
        lambda: tell(x, "new-description", P("plato-alphabetic-position-category"),
                     P("plato-alphabetic-last")),
        lambda: tell(x, "attach-description", P("plato-length"), P("plato-one")),
        lambda: print_names(tell(x, "get-descriptions")),
        lambda: tell(x, "description-present?", tell(x, "get-descriptions")[0]),
        lambda: tell(x, "delete-description-type", P("plato-letter-category")),
        lambda: print_names(tell(x, "get-descriptions")),
        lambda: tell(x, "update-raw-importance"),
        lambda: tell(x, "get-raw-importance"),
        lambda: tell(x, "update-description-strengths"),
        lambda: chez.map_(lambda d: tell(d, "get-strength"), tell(x, "get-descriptions")),
        lambda: deep_nm(tell(x, "get-concept-pattern")))


# Workspace strings: bonds, groups and storage expansion with fakes ---------------------

def fake_bond(from_, to, category):
    """workspace-battery.scm: b:fake-bond"""
    return fake_structure("bond", 50, **{
        "get-from-object": lambda: from_,
        "get-to-object": lambda: to,
        "get-left-object": lambda: from_,
        "get-right-object": lambda: to,
        "get-bond-category": lambda: category})


def fake_group(string, left, right):
    """workspace-battery.scm: b:fake-group"""
    state = {"id": 0}
    leftmost = tell(string, "get-letter", left)
    rightmost = tell(string, "get-letter", right)

    def set_id_num(n):
        state["id"] = n
        return "done"
    return fake_structure("group", 70, **{
        "set-id-num": set_id_num,
        "get-id-num": lambda: state["id"],
        "get-left-string-pos": lambda: left,
        "get-right-string-pos": lambda: right,
        "get-leftmost-object": lambda: leftmost,
        "get-rightmost-object": lambda: rightmost,
        "get-direction": lambda: P("plato-right"),
        "get-enclosing-group": lambda: False,
        "spans-whole-string?": lambda: 1 + (right - left) == tell(string, "get-length"),
        "descriptor-present?": lambda d: False,
        "nested-member?": lambda o: False,
        "get-raw-importance": lambda: 10 * (left + right),
        "update-relative-importance": lambda v: "done"})


@case("ws-string-bonds")
def _():
    init_problem(["abc", "abd", "mrrjjj"], 1)
    s = g("*target-string*")
    l = tell(s, "get-letters")
    b1 = fake_bond(l[0], l[1], P("plato-successor"))
    b2 = fake_bond(l[1], l[2], P("plato-sameness"))
    return seq(
        lambda: tell(s, "add-bond", b1),
        lambda: tell(s, "add-bond", b2),
        lambda: len(tell(s, "get-bonds")),
        lambda: tell(s, "add-proposed-bond", b1),
        lambda: tell(s, "add-proposed-bond", b2),
        lambda: len(tell(s, "get-all-bonds")),
        lambda: tell(s, "delete-proposed-bond", b1),
        lambda: len(tell(s, "get-all-bonds")),
        lambda: tell(s, "delete-proposed-bonds", l[1]),
        lambda: len(tell(s, "get-all-bonds")),
        lambda: tell(s, "get-bond-category-relevance", P("plato-successor")),
        lambda: tell(s, "get-bond-category-relevance", P("plato-sameness")),
        lambda: tell(s, "delete-bond", b1),
        lambda: len(tell(s, "get-bonds")),
        lambda: tell(s, "delete-all-proposed-bonds"))


@case("ws-string-groups")
def _():
    init_problem(["abc", "abd", "mrrjjj"], 1)
    s = g("*target-string*")
    g1 = fake_group(s, 1, 2)
    g2 = fake_group(s, 3, 5)
    g3 = fake_group(s, 0, 5)
    return seq(
        lambda: tell(s, "get-max-object-capacity"),
        lambda: tell(s, "add-group", g1),
        lambda: tell(s, "add-proposed-group", g2),
        lambda: len(tell(s, "get-all-groups")),
        lambda: len(tell(s, "get-all-objects")),
        lambda: tell(s, "add-group", g2),
        lambda: [tell(g1, "get-id-num"), tell(g2, "get-id-num")],
        lambda: len(tell(s, "get-left-edged-groups", 3)),
        lambda: len(tell(s, "get-right-edged-groups", 5)),
        lambda: len(tell(s, "get-all-other-coincident-groups", g1, 1, 2, P("plato-right"))),
        lambda: len(tell(s, "get-all-other-coincident-groups", g1, 3, 5, P("plato-right"))),
        lambda: tell(s, "get-equivalent-group", g1) is g1,
        lambda: asciis(tell(tell(s, "get-letter", 4), "get-all-left-neighbors")),
        lambda: tell(s, "spanning-group-exists?"),
        lambda: tell(s, "add-group", g3),
        lambda: tell(s, "spanning-group-exists?"),
        lambda: tell(s, "get-max-object-capacity"),
        lambda: tell(s, "update-all-relative-importances"),
        lambda: chez.map_(lambda o: tell(o, "get-relative-importance"), tell(s, "get-letters")),
        lambda: tell(s, "delete-group", g1),
        lambda: len(tell(s, "get-groups")),
        lambda: tell(s, "delete-proposed-group", g2),
        lambda: tell(s, "delete-all-proposed-groups"),
        lambda: len(tell(s, "get-all-groups")))


@case("ws-string-expand")
def _():
    init_problem(["abc", "abd", "xyz"], 1)
    s = g("*initial-string*")
    return seq(
        lambda: tell(s, "get-max-object-capacity"),
        lambda: tell(s, "add-group", fake_group(s, 0, 1)),
        lambda: tell(s, "add-group", fake_group(s, 1, 2)),
        lambda: tell(s, "get-max-object-capacity"),
        lambda: tell(s, "add-group", fake_group(s, 0, 2)),
        lambda: tell(s, "get-max-object-capacity"),
        lambda: tell(ws(), "get-proposed-bridges", "top"),
        lambda: tell(ws(), "get-proposed-bridges", "vertical"))


@case("ws-string-names")
def _():
    init_problem(["abc", "abd", "xyz"], 1)
    s = g("*initial-string*")
    # chez: this (list ...) is evaluated left to right (the fixture shows the
    # generic name before and after mark-as-translated)
    return [tell(s, "ascii-name"),
            tell(s, "generic-name"),
            capture(lambda: tell(s, "print")),
            tell(s, "mark-as-translated"),
            tell(s, "generic-name"),
            capture(lambda: tell(s, "print")),
            tell(s, "object-type"),
            ascii(tell(s, "get-letter", 2)),
            tell(tell(s, "get-letter", 2), "print-name")]


# Workspace: bridges and rules with fakes -------------------------------------------------

def fake_bridge(type_, obj1, obj2, strength, spanning_p):
    """workspace-battery.scm: b:fake-bridge"""
    def boost_themes():
        EEG_LOG.insert(0, type_)
        return "done"
    return fake_structure("bridge", strength, **{
        "get-bridge-type": lambda: type_,
        "get-orientation": lambda: "vertical" if type_ == "vertical" else "horizontal",
        "get-object1": lambda: obj1,
        "get-object2": lambda: obj2,
        "get-original-object1": lambda: obj1,
        "get-original-object2": lambda: obj2,
        "spanning-bridge?": lambda: spanning_p,
        "get-slippages": lambda: [type_],
        "get-non-symmetric-slippages": lambda: [],
        "get-all-concept-mappings": lambda: ["cm"],
        "get-covered-letters": lambda: tell(obj1, "get-letters") + tell(obj2, "get-letters"),
        "boost-themes": boost_themes})


def fake_rule(type_, name, supported_p):
    """workspace-battery.scm: b:fake-rule"""
    def fn(self, msg, *args):
        if msg == "get-rule-type":
            return type_
        if msg == "supported?":
            return supported_p
        if msg == "name":
            return name
        if msg == "equal?":
            return name == tell(args[0], "name")
        raise Unexpected("fake-rule", "unexpected message ~s", msg)
    return Lambda(fn)


@case("ws-bridges")
def _():
    init_problem(["abc", "abd", "xyz"], 1)
    EEG_LOG.clear()
    w = ws()
    i = tell(g("*initial-string*"), "get-letters")
    m = tell(g("*modified-string*"), "get-letters")
    t = tell(g("*target-string*"), "get-letters")
    tb = fake_bridge("top", i[0], m[0], 80, False)
    tb2 = fake_bridge("top", i[0], m[0], 60, False)
    vb = fake_bridge("vertical", i[1], t[1], 40, True)
    pb = fake_bridge("vertical", i[0], t[2], 20, False)
    return seq(
        lambda: tell(w, "add-bridge", tb),
        lambda: tell(w, "add-bridge", vb),
        lambda: tell(w, "add-proposed-bridge", pb),
        lambda: tell(w, "add-proposed-bridge", tb2),
        lambda: len(tell(w, "get-all-bridges")),
        lambda: len(tell(w, "get-all-proposed-bridges")),
        lambda: len(tell(w, "get-proposed-bridges", "vertical")),
        lambda: len(tell(w, "get-proposed-vertical-bridges", i[0])),
        lambda: len(tell(w, "get-proposed-vertical-bridges", t[2])),
        lambda: len(tell(w, "get-proposed-horizontal-bridges", m[0])),
        lambda: len(tell(w, "get-all-other-coincident-bridges", tb2, i[0], m[0])),
        lambda: tell(w, "bridge-present?", tb),
        lambda: tell(w, "spanning-bridge-exists?", "vertical"),
        lambda: tell(w, "get-spanning-bridge", "vertical") is vb,
        lambda: tell(w, "get-all-slippages", "top"),
        lambda: tell(w, "get-all-vertical-CMs"),
        lambda: tell(w, "maximal-mapping?", "vertical"),
        lambda: tell(w, "spread-activation-to-themespace"),
        lambda: list(reversed(EEG_LOG)),
        lambda: len(tell(w, "get-structures")),
        lambda: update_workspace_values(),
        lambda: [tell(w, "get-mapping-strength", "top"),
                 tell(w, "get-mapping-strength", "vertical"),
                 tell(w, "get-average-unhappiness")],
        lambda: tell(w, "delete-proposed-bridge", pb),
        lambda: tell(w, "delete-proposed-vertical-bridges", i[0]),
        lambda: tell(w, "delete-proposed-horizontal-bridges", m[0]),
        lambda: len(tell(w, "get-all-proposed-bridges")),
        lambda: tell(w, "delete-bridge", tb),
        lambda: tell(w, "delete-all-proposed-bridges"),
        lambda: len(tell(w, "get-all-bridges")))


@case("ws-rules")
def _():
    init_problem(["abc", "abd", "xyz"], 1)
    w = ws()
    r1 = fake_rule("top", "r1", True)
    r2 = fake_rule("top", "r2", False)
    r3 = fake_rule("top", "r1", False)
    return seq(
        lambda: tell(w, "rule-exists?", "top"),
        lambda: tell(w, "add-rule", r2),
        lambda: tell(w, "supported-rule-exists?", "top"),
        lambda: tell(w, "add-rule", r1),
        lambda: tell(w, "supported-rule-exists?", "top"),
        lambda: chez.map_(lambda r: tell(r, "name"), tell(w, "get-all-rules")),
        lambda: chez.map_(lambda r: tell(r, "name"), tell(w, "get-all-supported-rules")),
        lambda: tell(w, "get-equivalent-rule", r3) is r1,
        lambda: tell(w, "rule-present?", r3),
        lambda: tell(w, "delete-rule", r1),
        lambda: tell(w, "rule-present?", r3),
        lambda: tell(w, "clamp-rule", r1),
        lambda: len(tell(w, "get-clamped-rules")),
        lambda: tell(w, "unclamp-rules"),
        lambda: len(tell(w, "get-clamped-rules")))


def fake_temperature_workspace(unhappiness, top_possible, top_supported, bottom_possible,
                               bottom_supported):
    """workspace-battery.scm: b:fake-temperature-workspace"""
    def fn(self, msg, *args):
        if msg == "get-average-unhappiness":
            return unhappiness
        if msg == "rule-possible?":
            return top_possible if args[0] == "top" else bottom_possible
        if msg == "supported-rule-exists?":
            return top_supported if args[0] == "top" else bottom_supported
        raise Unexpected("fake-workspace", "unexpected message ~s", msg)
    return Lambda(fn)


@case("update-temperature")
def _():
    saved = ws()

    def each(args):
        set_global("%justify-mode%", args[0])
        set_global("*temperature-clamped?*", args[1])
        set_global("*temperature*", 55)
        set_global("*workspace*", fake_temperature_workspace(*args[2:]))
        M("formulas").update_temperature()
        return g("*temperature*")
    r = map_in_order(each, [[False, False, 0, True, True, False, False],
                            [False, False, 0, True, False, False, False],
                            [False, False, 37, False, True, False, False],
                            [False, False, 100, True, True, False, False],
                            [False, True, 0, True, True, False, False],
                            [True, False, 42, True, True, False, False],
                            [True, False, 42, True, True, True, True],
                            [True, False, 99, True, True, True, False],
                            [False, False, 41, False, False, False, False],
                            [False, False, 43, False, False, False, False]])
    set_global("*workspace*", saved)
    set_global("*temperature-clamped?*", False)
    set_global("%justify-mode%", False)
    return r


# Workspace structures ---------------------------------------------------------------------

def structure(internal, external, compatibility):
    """workspace-battery.scm: b:structure"""
    base = M("workspace_structures").make_workspace_structure()

    def fn(self, msg, *args):
        if msg == "calculate-internal-strength":
            return internal
        if msg == "calculate-external-strength":
            return external
        if msg == "get-thematic-compatibility":
            return compatibility
        return base(self, msg, *args)          # (apply base msg)
    return Lambda(fn)


STRENGTH_CASES = [[0, 0, 0], [100, 0, 0], [0, 100, 0], [50, 50, 0], [73, 21, 0],
                  [100, 100, 0], [30, 90, F(1, 2)], [30, 90, F(-1, 2)], [60, 40, 1],
                  [60, 40, -1], [99, 1, 0.3], [12, 88, -0.7], [45, 45, 0.05]]


@case("ws-structure-strength")
def _():
    def each(c):
        s = structure(*c)
        tell(s, "update-strength")
        return [c, tell(s, "get-strength"), tell(s, "get-weakness")]
    return map_in_order(each, STRENGTH_CASES)


@case("ws-structure-misc")
def _():
    set_global("*codelet-count*", 17)
    s = structure(50, 50, 0)
    set_global("*codelet-count*", 40)
    wsm = M("workspace")
    r = seq(
        lambda: tell(s, "object-type"),
        lambda: tell(s, "get-time-stamp"),
        lambda: tell(s, "get-age"),
        lambda: tell(s, "proposed?"),
        lambda: tell(s, "update-proposal-level", wsm.p_evaluated),
        lambda: tell(s, "get-proposal-level"),
        lambda: tell(s, "update-proposal-level", wsm.p_built),
        lambda: tell(s, "proposed?"),
        lambda: tell(s, "drawn?"),
        lambda: tell(s, "set-drawn?", True),
        lambda: tell(s, "drawn?"),
        lambda: tell(s, "update-enclosing-group", "g"),
        lambda: tell(s, "get-enclosing-group"),
        lambda: tell(M("workspace_structures").make_workspace_structure(),
                     "get-thematic-compatibility"))
    set_global("*codelet-count*", 0)
    return r


@case("wins-fight")
def _():
    wss = M("workspace_structures")

    def per_temp(temp):
        set_global("*temperature*", temp)
        return map_in_order(
            lambda seed: seeded(seed, lambda: repeat(6, lambda: wss.wins_fight_p(
                structure(70, 30, 0), 1, structure(40, 60, 0), F(3, 2)))),
            SEEDS)
    return map_in_order(per_temp, [100, 50, 10, 0])


@case("wins-all-fights")
def _():
    wss = M("workspace_structures")
    set_global("*temperature*", 60)

    def thunk():
        c = structure(60, 60, 0)
        ds = [structure(50, 20, 0), structure(90, 90, 0), structure(10, 10, 0)]
        r1 = wss.wins_all_fights_p(c, 1, ds, 1)
        r2 = wss.wins_all_fights_p(c, 2, ds, [1, F(1, 2), 3])
        r3 = wss.wins_all_fights_p(c, 1, [], 1)
        return [r1, r2, r3]
    return map_in_order(lambda seed: seeded(seed, thunk), SEEDS)


# formulas.ss and workspace-structure-formulas.ss ---------------------------------------

TEMPS = [0, 1, 10, 25, 35, 50, 64, 75, 90, 99, 100]
PROBS = [0, 0.0, 0.0001, 0.003, 0.01, 0.05, 0.1, 0.25, 0.3333, 0.5, 0.5000001,
         0.6, 0.75, 0.9, 0.99, 1, 1.0, F(1, 3), F(1, 1000), F(2, 3)]


@case("temp-adjusted-probability")
def _():
    def per_temp(temp):
        set_global("*temperature*", temp)
        return chez.map_(M("formulas").temp_adjusted_probability, PROBS)
    return map_in_order(per_temp, TEMPS)


@case("temp-adjusted-values")
def _():
    def per_temp(temp):
        set_global("*temperature*", temp)
        return M("formulas").temp_adjusted_values([0, 1, 2, 3, 10, 33, 50, 77, 99, 100,
                                                   F(1, 3), 2.5, 1000])
    return map_in_order(per_temp, TEMPS)


def fake_group_for_probability(length, supporting, support):
    """workspace-battery.scm: b:fake-group-for-probability"""
    def fn(self, msg, *args):
        if msg == "get-group-length":
            return length
        if msg == "get-num-of-local-supporting-groups":
            return supporting
        if msg == "get-local-support":
            return support
        raise Unexpected("fake-group", "unexpected message ~s", msg)
    return Lambda(fn)


@case("length-description-probability")
def _():
    wsf = M("workspace_structure_formulas")

    def per_act(act):
        tell(P("plato-length"), "set-activation", act)

        def per_temp(temp):
            set_global("*temperature*", temp)
            return chez.map_(lambda n: wsf.length_description_probability(
                fake_group_for_probability(n, 0, 0)), iota(7))
        return map_in_order(per_temp, TEMPS)
    return map_in_order(per_act, [0, 30, 77, 100])


@case("single-letter-group-probability")
def _():
    wsf = M("workspace_structure_formulas")

    def per_act(act):
        tell(P("plato-length"), "set-activation", act)

        def per_temp(temp):
            set_global("*temperature*", temp)
            return chez.map_(lambda n: wsf.single_letter_group_probability(
                fake_group_for_probability(1, n, 20 * n)), iota(5))
        return map_in_order(per_temp, TEMPS)
    return map_in_order(per_act, [0, 30, 77, 100])


@case("translation-threshold-distribution")
def _():
    def each(strings):
        init_problem(strings, 1)
        return index_of(M("formulas").current_translation_temperature_threshold_distribution(),
                        threshold_distributions())
    return map_in_order(each, [["a", "b", "z"], ["abc", "abd", "xyz"], ["abc", "abd", "mrrjjj"]])


@case("ws-objects-helpers")
def _():
    init_problem(["abc", "abd", "mrrjjj"], 1)
    wso, wsm = M("workspace_objects"), M("workspace")
    t = tell(g("*target-string*"), "get-letters")
    # rough-num-of-objects draws, and b:seeded reseeds; (rough-num-of-objects 0)
    # is 'few whatever the draw, so the list's order of evaluation does not show
    return [wso.disjoint_objects_p(t[0], t[1]),
            wso.disjoint_objects_p(t[0], t[0]),
            wso.lone_spanning_object_p(t[0], t[1]),
            wso.both_spanning_groups_p(t[0], t[1]),
            wso.both_spanning_objects_p(t[0], t[1]),
            wsm.rough_num_of_objects(0),
            seeded(5, lambda: repeat(10, lambda: wsm.rough_num_of_objects(3))),
            ascii(tell(t[0], "get-instantiated-image-object"))]


@case("tanh-mapping-arguments")
def _():
    return chez.map_(lambda n: chez.tanh(chez.mul(F(1, 40), n - 101)), iota(201))


@case("tanh-doubles")
def _():
    def draw():
        x = chez.random(1.0)
        e = chez.random(12)
        return chez.tanh(chez.mul(chez.sub(x, 0.5), chez.expt(2.0, e - 4)))
    return seeded(11, lambda: repeat(300, draw))


@case("tanh-special")
def _():
    return chez.map_(chez.tanh, [0, 0.0, -0.0, 1, -1, F(1, 2), 20, 30.0, -400.0, 1e-300])


@case("ws-importance")
def _():
    def each(types):
        init_problem(["abc", "abd", "mrrjjj"], 1)
        for t in types:
            tell(t, "set-activation", slipnet.p_max_activation)
        tell(P("plato-c"), "set-activation", 37)
        tell(P("plato-j"), "set-activation", 71)
        tell(tell(g("*target-string*"), "get-letter", 2), "update-enclosing-group",
             fake_structure("group", 55, **{"get-bridge": lambda o: False,
                                             "nested-member?": lambda o: False,
                                             "get-nesting-level": lambda: 0}))
        update_workspace_values()
        return [all_object_values(),
                tell(ws(), "get-average-unhappiness"),
                chez.map_(lambda s: tell(s, "get-average-intra-string-unhappiness"),
                          [g("*initial-string*"), g("*modified-string*"), g("*target-string*")])]
    return map_in_order(each, [[P("plato-letter-category")],
                               [P("plato-string-position-category")],
                               [P("plato-letter-category"), P("plato-string-position-category"),
                                P("plato-object-category")]])


@case("ws-maximal-mapping")
def _():
    def each(strengths):
        init_problem(["abc", "abd", "xyz"], 1)
        w = ws()
        wsm = M("workspace")
        i = tell(g("*initial-string*"), "get-letters")
        m = tell(g("*modified-string*"), "get-letters")
        t = tell(g("*target-string*"), "get-letters")
        b1 = fake_bridge("top", i[0], m[0], strengths[0], False)
        b2 = fake_bridge("top", i[1], m[1], strengths[1], False)
        b3 = fake_bridge("top", i[2], m[2], strengths[2], False)
        v1 = fake_bridge("vertical", i[0], t[0], strengths[1], False)

        def for_each(f, l):
            r = None
            for x in l:
                r = f(x)
            return r          # chez: for-each returns the last value
        return seq(
            lambda: tell(w, "add-bridge", b1),
            lambda: tell(w, "add-bridge", b2),
            lambda: tell(w, "maximal-mapping?", "top"),
            lambda: tell(w, "add-bridge", b3),
            lambda: tell(w, "add-bridge", v1),
            lambda: for_each(lambda o: tell(o, "update-bridge", "horizontal", b1), i),
            lambda: for_each(lambda o: tell(o, "update-bridge", "horizontal", b2), m),
            lambda: tell(i[0], "update-bridge", "vertical", v1),
            lambda: tell(t[0], "update-bridge", "vertical", v1),
            lambda: tell(w, "maximal-mapping?", "top"),
            lambda: [wsm.spanning_group_possible_p(g("*initial-string*")),
                     wsm.spanning_group_possible_p(g("*modified-string*")),
                     wsm.spanning_group_possible_p(g("*target-string*"))],
            lambda: update_workspace_values(),
            lambda: [tell(w, "get-mapping-strength", "top"),
                     tell(w, "get-mapping-strength", "vertical"),
                     tell(w, "get-min-mapping-strength"),
                     tell(w, "get-max-inter-string-unhappiness"),
                     tell(w, "get-average-unhappiness")],
            lambda: all_object_values())
    return map_in_order(each, [[100, 100, 100], [90, 70, 50], [33, 66, 99], [1, 2, 3]])


# The engine, with the battery's stand-ins --------------------------------------------

# Globals of files not translated yet, which these files read.
STAND_INS = {
    "run": {"p_update_cycle_length": 15, "g_temperature_clamped_p": False},
    "themes": {"g_themespace": Lambda(fake_themespace_fn)},
    "eeg_graphics": {"g_EEG": Lambda(fake_eeg_fn)},
    "groups": {"contains_p": contains_p},
}


def _module_globals(mod):
    return {k: v for k, v in vars(mod).items() if k.startswith(("g_", "p_"))}


@pytest.fixture(scope="module", autouse=True)
def battery_engine():
    engine.load()
    with ExitStack() as stack:
        for mod, attrs in STAND_INS.items():
            stack.enter_context(engine_module(mod, **attrs))
        saved = [(setup, _module_globals(setup))]
        for name in WS_MODULES:
            try:
                mod = M(name)
            except ImportError:
                continue
            saved.append((mod, _module_globals(mod)))
        saved_top_level = dict(chez.TOP_LEVEL)
        set_global("%workspace-graphics%", False)
        yield
        for mod, values in saved:
            for k, v in values.items():
                setattr(mod, k, v)
        chez.TOP_LEVEL.clear()
        chez.TOP_LEVEL.update(saved_top_level)
        for node in slipnet.g_slipnet_nodes:
            tell(node, "reset")


def first_difference(got, expected):
    i = next((k for k, (a, b) in enumerate(zip(got, expected)) if a != b), min(len(got), len(expected)))
    return (f"differs at character {i} of {len(expected)}:\n"
            f"  python: ...{got[max(0, i - 150):i + 150]}\n  chez:   ...{expected[max(0, i - 150):i + 150]}")


def test_every_battery_test_is_translated():
    assert sorted(CASES) == sorted(manifest("workspace"))
    assert len(CASES) == 75


def test_problems_are_read_as_the_battery_reads_them():
    assert fixture("workspace", "ws-problem-count") == str(len(PROBLEMS))


@pytest.mark.parametrize("name", manifest("workspace"))
def test_workspace_battery(name):
    expected = fixture("workspace", name)
    if expected == "ERROR":
        with pytest.raises(chez.SchemeError):
            CASES[name]()
        return
    got = canon(CASES[name]())
    same = got == expected      # not `assert got == expected`: pytest's diff of long strings is slow
    assert same, first_difference(got, expected)


# Beyond the battery -------------------------------------------------------------------

ORIGINS = {"workspace": "workspace.ss", "workspace_objects": "workspace-objects.ss",
           "workspace_structures": "workspace-structures.ss",
           "workspace_strings": "workspace-strings.ss",
           "workspace_structure_formulas": "workspace-structure-formulas.ss",
           "formulas": "formulas.ss"}


def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


@pytest.mark.parametrize("name", WS_MODULES)
def test_every_definition_has_its_python_function(name):
    mod = M(name)
    missing = [n for n in defines(ORIGINAL / ORIGINS[name])
               if not hasattr(mod, scheme_to_python(n))]
    assert missing == [], (name, missing)


@pytest.mark.parametrize("name", WS_MODULES)
def test_docstrings_name_their_origin(name):
    mod = M(name)
    for fname, fn in vars(mod).items():
        if callable(fn) and getattr(fn, "__module__", None) == mod.__name__ and not fname.startswith("_"):
            assert fn.__doc__ and fn.__doc__.split(":")[0] == ORIGINS[name], (name, fname)


@pytest.mark.parametrize("name", WS_MODULES)
def test_engine_modules_import_no_gui(name):
    tree = ast.parse(inspect.getsource(M(name)))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), name


def test_modules_are_loaded_in_metacat_ss_order():
    loaded = engine.translated_modules()
    for name in WS_MODULES:
        assert M(name) in loaded
    order = engine.LOAD_ORDER
    assert order.index("workspace") < order.index("workspace_strings") < order.index("formulas")
    assert M("workspace").g_workspace is not False


def test_initial_workspace_draws_nothing():
    """porting-notes.md item 06: building the initial workspace makes no draw."""
    strings, seeds = PROBLEMS[6]
    init_problem(strings, seeds[0])
    chez.random_seed(12345)
    init_state = chez.random_seed()
    # init_problem reseeds; build again without reseeding through the parts
    mws = M("workspace_strings").make_workspace_string
    mws("initial", "abc")
    assert chez.random_seed() == init_state


# python/oracle/batteries/workspace-extra-battery.scm --------------------------------
# Written after the code (mutations survived: the 1/2 factor on a group's vertical
# bridge, choosing a description by activation, neighbours in reverse, relevance
# over n rather than n - 1 objects, unrelated? of a middle letter with one bond,
# the oldest structures taken for the youngest, an exact (min 1 ...) in get-activity);
# same rules as above.  It runs in the same engine, after the battery.

EXTRA: dict = {}


def x_fake(type_, strength, **props):
    """workspace-extra-battery.scm: x:fake"""
    return fake_structure(type_, strength, **props)


def x_fake_group(string, left, right, salience):
    """workspace-extra-battery.scm: x:fake-group"""
    state = {"id": 0}
    leftmost = tell(string, "get-letter", left)
    rightmost = tell(string, "get-letter", right)

    def set_id_num(n):
        state["id"] = n
        return "done"
    return x_fake("group", 70, **{
        "set-id-num": set_id_num,
        "get-id-num": lambda: state["id"],
        "ascii-name": lambda: String(chez.format_("group:~a-~a", left, right)),
        "get-left-string-pos": lambda: left,
        "get-right-string-pos": lambda: right,
        "get-leftmost-object": lambda: leftmost,
        "get-rightmost-object": lambda: rightmost,
        "get-enclosing-group": lambda: False,
        "get-intra-string-salience": lambda: salience})


def x_bond(category, direction):
    """workspace-extra-battery.scm: x:bond"""
    return x_fake("bond", 50, **{"get-bond-category": lambda: category,
                                 "get-direction": lambda: direction})


def x_values(obj):
    """workspace-extra-battery.scm: x:values"""
    return [tell(obj, "ascii-name"),
            tell(obj, "get-intra-string-unhappiness"),
            tell(obj, "get-inter-string-unhappiness", "horizontal"),
            tell(obj, "get-inter-string-unhappiness", "vertical"),
            tell(obj, "get-average-unhappiness"),
            tell(obj, "get-inter-string-salience", "horizontal"),
            tell(obj, "get-inter-string-salience", "vertical"),
            tell(obj, "get-average-salience")]


def x_group_vertical_bridge():
    init_problem(["abc", "abd", "ijk"], 1)
    vbridge = x_fake("bridge", 37)
    group = x_fake("group", 64, **{"get-bridge": lambda o: vbridge if o == "vertical" else False,
                                   "nested-member?": lambda o: False,
                                   "get-nesting-level": lambda: 0})
    i = tell(g("*initial-string*"), "get-letters")
    t = tell(g("*target-string*"), "get-letters")
    tell(i[0], "update-enclosing-group", group)
    tell(t[1], "update-enclosing-group", group)
    tell(t[2], "update-enclosing-group", group)
    update_workspace_values()
    return chez.map_(x_values, tell(ws(), "get-objects"))


def x_relevant_description_choices():
    init_problem(["abc", "abd", "xyz"], 1)
    for name, act in [("plato-letter-category", 100), ("plato-string-position-category", 100),
                      ("plato-object-category", 100), ("plato-a", 5), ("plato-letter", 40),
                      ("plato-leftmost", 80), ("plato-x", 15)]:
        tell(P(name), "set-activation", act)

    def thunk():
        objects = tell(ws(), "get-objects")

        def once():
            acc = []
            for obj in objects:
                d1 = tell(obj, "choose-relevant-description-by-activation")
                d2 = tell(obj, "choose-relevant-distinguishing-description-by-depth")
                acc.append([tell(d1, "print-name"),
                            tell(d2, "print-name") if d2 is not False else False])
            return acc
        return repeat(4, once)
    return seeded(5, thunk)


def x_neighbours_with_groups():
    init_problem(["abc", "abd", "mrrjjj"], 1)
    s = g("*target-string*")
    g1 = x_fake_group(s, 1, 2, 90)
    g2 = x_fake_group(s, 3, 5, 10)
    g3 = x_fake_group(s, 2, 3, 55)
    tell(s, "add-group", g1)
    tell(s, "add-group", g2)
    tell(s, "add-group", g3)

    def once():
        l3 = tell(s, "get-letter", 3)
        l2 = tell(s, "get-letter", 2)
        a = ascii(tell(l3, "choose-left-neighbor"))
        b = ascii(tell(l2, "choose-right-neighbor"))
        c = ascii(tell(l3, "choose-neighbor"))
        d = ascii(tell(l2, "choose-neighbor"))
        return [a, b, c, d]
    return seeded(9, lambda: repeat(6, once))


def x_relevance_with_bonds():
    init_problem(["abc", "abd", "mrrjjj"], 1)
    s = g("*target-string*")
    l = tell(s, "get-letters")
    tell(l[0], "update-right-bond", x_bond(P("plato-successor"), P("plato-right")))
    tell(l[1], "update-right-bond", x_bond(P("plato-sameness"), False))
    tell(l[3], "update-right-bond", x_bond(P("plato-sameness"), False))
    tell(l[4], "update-right-bond", x_bond(P("plato-sameness"), False))
    return [tell(s, "get-bond-category-relevance", P("plato-sameness")),
            tell(s, "get-bond-category-relevance", P("plato-successor")),
            tell(s, "get-bond-category-relevance", P("plato-predecessor")),
            tell(s, "get-direction-relevance", P("plato-right")),
            tell(s, "get-direction-relevance", P("plato-left"))]


def x_unrelated_one_bond():
    init_problem(["abc", "abd", "xyz"], 1)
    wsm = M("workspace")
    l = tell(g("*initial-string*"), "get-letters")
    bond = x_bond(P("plato-successor"), P("plato-right"))
    tell(l[1], "update-left-bond", bond)
    tell(l[0], "update-right-bond", bond)
    return [wsm.unrelated_p(l[0]), wsm.unrelated_p(l[1]), wsm.unrelated_p(l[2]),
            tell(l[1], "get-num-of-incident-bonds")]


def x_density_boundaries():
    init_problem(["abcdef", "abcdeg", "ijklmn"], 1)
    strings = [g("*initial-string*"), g("*modified-string*"), g("*target-string*")]
    acc = []
    for k in range(15):
        s = strings[k // 5]
        from_ = tell(s, "get-letter", k % 5)
        to = tell(s, "get-letter", 1 + k % 5)
        bond = x_fake("bond", 50, **{"get-from-object": (lambda f: lambda: f)(from_),
                                     "get-to-object": (lambda t: lambda: t)(to),
                                     "get-left-object": (lambda f: lambda: f)(from_),
                                     "get-right-object": (lambda t: lambda: t)(to),
                                     "get-bond-category": lambda: P("plato-successor")})
        tell(s, "add-bond", bond)
        acc.append(index_of(M("formulas").current_translation_temperature_threshold_distribution(),
                            threshold_distributions()))
    return acc


def x_activity_and_ages():
    init_problem(["abc", "abd", "xyz"], 1)
    w = ws()
    acc = []
    for age in [13, 12, 700, 1000, 3, 2, 501, 1]:
        def rule_fn(self, msg, *args, age=age):
            if msg == "get-rule-type":
                return "top"
            if msg == "get-age":
                return age
            raise Unexpected("fake-rule", "unexpected message ~s", msg)
        tell(w, "add-rule", Lambda(rule_fn))
        acc.append([tell(w, "get-youngest-structures-average-age"), tell(w, "get-activity")])
    return acc


def x_activity_float_ties():
    def each(ages):
        init_problem(["abc", "abd", "xyz"], 1)
        for age in ages:
            def rule_fn(self, msg, *args, age=age):
                if msg == "get-rule-type":
                    return "top"
                if msg == "get-age":
                    return age
                raise Unexpected("fake-rule", "unexpected message ~s", msg)
            tell(ws(), "add-rule", Lambda(rule_fn))
        return [tell(ws(), "get-youngest-structures-average-age"), tell(ws(), "get-activity")]
    # chez: map's order (each case reinitializes the workspace)
    return chez.map_(each, [[272, 273], [287, 288], [1, 2]])


EXTRA["group-vertical-bridge"] = x_group_vertical_bridge
EXTRA["relevant-description-choices"] = x_relevant_description_choices
EXTRA["neighbours-with-groups"] = x_neighbours_with_groups
EXTRA["relevance-with-bonds"] = x_relevance_with_bonds
EXTRA["unrelated-one-bond"] = x_unrelated_one_bond
EXTRA["density-boundaries"] = x_density_boundaries
EXTRA["activity-and-ages"] = x_activity_and_ages
EXTRA["activity-float-ties"] = x_activity_float_ties


def test_every_extra_test_is_translated():
    assert list(EXTRA) == list(manifest("workspace-extra"))


@pytest.mark.parametrize("name", manifest("workspace-extra"))
def test_workspace_extra_battery(name):
    expected = fixture("workspace-extra", name)
    got = canon(EXTRA[name]())
    assert got == expected, first_difference(got, expected)
