"""Rules and answers, codelet by codelet, against Chez (loop0002 item 09).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/rule-battery.scm, through codelet_harness.py with the
bridges setting and b:rules? on: run.ss's initial codelets, every bottom-up
codelet type posted by the original's add-bottom-up-codelets (self-watching
off), check-if-rules-possible at every update, and all of *top-down-slipnodes*.
For every problem of tests/problems.txt and each of its seeds, the trace up to
the codelet that reports the first answer (or 2500 codelets): besides what items
07-08 record, every rule built (type, English, clauses, strength, quality values,
supporting bridges), the rule monitor, snags, the answer and the commentary that
answers.ss writes.  Then the summary of the first answers, and twelve rule
matrices: after a run, every rule applied (apply-rule with ignore-snag) and
translated (translate, which draws), with the generator state after each.

rule-battery.scm's fakes for the files not translated yet (the Trace, the Memory,
answer and snag events, the abstract descriptions, monitor-new-rules, the
Commentary window, suspend, answer-justifier's procedure) are translated below.
The real definitions the oracle has loaded (trace.ss's
equivalent-workspace-objects?, general-graphics.ss's find-next-space-position
and themes.ss's `diff`) are the engine's own since item 10.  run.ss's
update-everything and post-initial-codelets are the harness's (the battery sets
them so).

Tiers: the fast tier runs the first seed of problem 0 for 300 codelets against
the start of its fixture, and the local rule-extra battery (transcribe-to-english
and its caddr-of-#f crash).  The full battery is marked slow; its cases run in a
pool of processes forked after the engine and the harness are set up, as in
test_codelets.py.  first-answers is assembled from the problems' runs, in order.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import multiprocessing
import os
import re
import traceback
from contextlib import ExitStack

import pytest

from chez_fixtures import chez as fixture, manifest
from engine_stubs import engine_module
from name_mapping import ORIGINAL
from scheme_canon import canon
from metacat import chez, coderack, engine, slipnet, utilities
from metacat.names import scheme_to_python
from metacat.objects import Lambda, tell

import codelet_harness as h
from test_codelets import first_line_difference, lines_of

MODULES = ("rules", "answers")
ORIGINS = {"rules": "rules.ss", "answers": "answers.ss"}
BATTERY = "rule"
CODELET_CAP = 2500          # b:codelet-cap


def M(name):
    return importlib.import_module("metacat." + name)


def _module_globals(mod):
    return {k: v for k, v in vars(mod).items() if k.startswith(("g_", "p_"))}


# rule-battery.scm's fakes ------------------------------------------------------------------

def nms(nodes):
    """rule-battery.scm: b:nms"""
    return [h.nm(n) for n in nodes]


def slippage_log_data(log):
    """rule-battery.scm: b:slippage-log-data"""
    return [h.datum(tell(log, "get-directly-applied-slippages")),
            h.datum(tell(log, "get-coattail-slippages")),
            h.datum(tell(log, "get-coattail-inducing-slippages")),
            [h.bridge_data(b) for b in tell(log, "get-slippage-bridges")]]


def answer_data(answer_string, top_rule, bottom_rule, vertical_bridges, groups, top_refs,
                bottom_refs, slippage_log, unjustified, quality):
    """rule-battery.scm: b:answer-data"""
    return ["answer",
            nms(tell(answer_string, "get-letter-categories")),
            tell(answer_string, "print-name"),
            [h.group_data(g) for g in tell(answer_string, "get-groups")],
            h.rule_data(top_rule), h.rule_data(bottom_rule),
            [tell(bottom_rule, "get-quality"), tell(bottom_rule, "get-uniformity"),
             tell(bottom_rule, "get-abstractness"), tell(bottom_rule, "get-succinctness")],
            tell(bottom_rule, "translated?"),
            h.datum(tell(bottom_rule, "get-supporting-horizontal-bridges")),
            [h.bridge_data(b) for b in vertical_bridges],
            [h.obj_id(g) for g in groups],
            h.datum(top_refs), h.datum(bottom_refs),
            slippage_log_data(slippage_log),
            h.datum(unjustified),
            quality]


def fake_answer_event(initial, modified, target, answer_string, top_rule, bottom_rule,
                      vertical_bridges, groups, top_refs, bottom_refs, slippage_log,
                      unjustified):
    """rule-battery.scm: b:fake-answer-event"""
    temperature = h.get_global("*temperature*")

    def fn(self, msg, *args):
        if msg == "get-type":
            return "answer"
        if msg == "type?":
            return args[0] == "answer"
        if msg == "get-temperature":
            return temperature
        if msg == "get-quality":
            return utilities.round_(utilities.weighted_average(
                [tell(top_rule, "get-quality"), utilities.hundred_minus(temperature)],
                [60, 40]))
        if msg == "b:data":
            return answer_data(answer_string, top_rule, bottom_rule, vertical_bridges, groups,
                               top_refs, bottom_refs, slippage_log, unjustified,
                               tell(self, "get-quality"))
        raise chez.SchemeError("fake-answer-event", "unexpected message ~s", msg)
    return Lambda(fn)


def fake_snag_event(failure_result, rule, translated_rule, vertical_bridges, slippage_log,
                    rule_ref_objects):
    """rule-battery.scm: b:fake-snag-event"""
    def fn(self, msg, *args):
        if msg == "get-type":
            return "snag"
        if msg == "type?":
            return args[0] == "snag"
        if msg == "get-explanation":
            return chez.String("the " + failure_result[0] + " failed")
        if msg == "activate":
            return h.event(["snag-activate"])
        if msg == "b:data":
            return ["snag", h.datum(failure_result), h.rule_data(rule),
                    h.rule_data(translated_rule),
                    [h.bridge_data(b) for b in vertical_bridges],
                    slippage_log_data(slippage_log), h.datum(rule_ref_objects)]
        raise chez.SchemeError("fake-snag-event", "unexpected message ~s", msg)
    return Lambda(fn)


TRACE_EVENTS: list = []     # b:trace-events (newest first)
SNAG_PERIOD = [False]       # b:snag-period?
FIRST_ANSWER = [False]      # b:first-answer
FIRST_ANSWERS: list = []    # b:first-answers, oldest first, each entry as b:compact text


def fake_trace_fn(self, msg, *args):
    """rule-battery.scm: b:fake-trace"""
    if msg == "add-event":
        ev = args[0]
        TRACE_EVENTS.insert(0, ev)
        if tell(ev, "type?", "snag") is not False:
            SNAG_PERIOD[0] = True
        data = tell(ev, "b:data")
        if data[0] == "answer" and FIRST_ANSWER[0] is False:
            FIRST_ANSWER[0] = data
        h.event(["trace-event", data])
        return "done"
    if msg == "get-num-of-events":
        return sum(1 for ev in TRACE_EVENTS if tell(ev, "type?", args[0]) is not False)
    if msg == "get-all-events":
        return list(reversed(TRACE_EVENTS))
    if msg == "within-clamp-period?":
        return False
    if msg == "within-snag-period?":
        return SNAG_PERIOD[0]
    if msg == "progress-since-last-snag":
        return 50
    if msg == "undo-snag-condition":
        SNAG_PERIOD[0] = False
        h.set_global("*temperature-clamped?*", False)
        h.event(["undo-snag-condition"])
        return "done"
    raise chez.SchemeError("fake-trace", "unexpected message ~s", msg)


def fake_memory_fn(self, msg, *args):
    """rule-battery.scm: b:fake-memory"""
    if msg == "answer-present?":
        letters, rule, translated_rule = args
        h.event(["memory-answer-present?", nms(letters), h.rule_data(rule),
                 h.rule_data(translated_rule)])
        return False
    if msg == "snag-present?":
        h.event(["memory-snag-present?", h.rule_data(args[0])])
        return False
    if msg == "get-equivalent-snag":
        return False
    raise chez.SchemeError("fake-memory", "unexpected message ~s", msg)


def fake_comment_window_fn(self, msg, *args):
    """rule-battery.scm: b:fake-comment-window"""
    return h.event(["comment", msg, *args])


def abstract_answer_description(ev):
    """rule-battery.scm: abstract-answer-description (memory.ss's, recording)"""
    return h.event(["abstract-answer-description"])


def abstract_snag_description(ev):
    """rule-battery.scm: abstract-snag-description (memory.ss's, recording)"""
    return h.event(["abstract-snag-description"])


def monitor_new_rules(rule):
    """rule-battery.scm: monitor-new-rules (trace.ss's, recording)"""
    return h.event(["new-rule", h.rule_data(rule)])


def suspend():
    """rule-battery.scm: suspend (run.ss's: ends the run)"""
    h.ANSWERED[0] = True
    return h.event(["suspend"])


def answer_justifier_proc():
    """rule-battery.scm: answer-justifier's procedure (justify.ss's: records its call)"""
    h.event(["answer-justifier"])
    return "done"


STAND_INS = {
    "run": {"suspend": suspend, "update_everything": h.update_everything,
            "post_initial_codelets": h.post_initial_codelets},
    "trace": {"g_trace": Lambda(fake_trace_fn), "make_answer_event": fake_answer_event,
              "make_snag_event": fake_snag_event, "monitor_new_rules": monitor_new_rules},
    "memory": {"g_memory": Lambda(fake_memory_fn),
               "abstract_answer_description": abstract_answer_description,
               "abstract_snag_description": abstract_snag_description},
}


def stand_ins():
    """The harness's stand-ins merged with this battery's."""
    merged = {mod: dict(attrs) for mod, attrs in h.STAND_INS.items()}
    for mod, attrs in STAND_INS.items():
        merged.setdefault(mod, {}).update(attrs)
    return merged


@pytest.fixture(scope="module", autouse=True)
def harness_engine():
    engine.load()
    with ExitStack() as stack:
        for mod, attrs in stand_ins().items():
            stack.enter_context(engine_module(mod, **attrs))
        saved = []
        for name in ("setup", "slipnet", "coderack", "workspace", "formulas", "bonds",
                     "groups", "concept_mappings", "descriptions", "bridges",
                     "breakers") + MODULES:
            try:
                mod = M(name)
            except ImportError:
                continue
            saved.append((mod, _module_globals(mod)))
        saved_top_level = dict(chez.TOP_LEVEL)
        saved_justifier = coderack.answer_justifier.codelet_proc
        h.install()
        h.enable_bridges(True)
        h.RULES[0] = True
        h.set_global("*comment-window*", Lambda(fake_comment_window_fn))
        tell(coderack.answer_justifier, "set-codelet-procedure", answer_justifier_proc)
        yield
        tell(coderack.answer_justifier, "set-codelet-procedure", saved_justifier)
        h.RULES[0] = False
        h.ANSWERED[0] = False
        h.enable_bridges(False)
        for mod, values in saved:
            for k, v in values.items():
                setattr(mod, k, v)
        chez.TOP_LEVEL.clear()
        chez.TOP_LEVEL.update(saved_top_level)
        for type_ in coderack.g_codelet_types:
            tell(type_, "set-graphics-parameters", *([False] * 9))
        for node in slipnet.g_slipnet_nodes:
            tell(node, "reset")
        tell(coderack.g_coderack, "initialize")


def compare(name, got, battery=BATTERY):
    expected = fixture(battery, name)
    same = got == expected      # not `assert got == expected`: pytest's diff of long strings is slow
    if not same and expected.startswith('"'):
        pytest.fail(f"{name}: " + first_line_difference(got, expected))
    assert same, (name, got[:2000], expected[:2000])


# The traces ---------------------------------------------------------------------------------

def run_problem(strings, seed, k):
    """rule-battery.scm: b:run-problem"""
    TRACE_EVENTS.clear()
    SNAG_PERIOD[0] = False
    FIRST_ANSWER[0] = False
    trace = h.run_codelets(strings, seed, k)
    count = h.get_global("*codelet-count*")
    if h.ANSWERED[0]:
        fa = FIRST_ANSWER[0]
        entry = ["answered", strings, seed, count, fa[1], fa[15], fa[4][2], fa[5][2]]
    else:
        entry = ["no-answer", strings, seed, count]
    FIRST_ANSWERS.append(h.compact(entry))
    return trace


def trace_lines(strings, seed, k):
    """rule-battery.scm: b:trace-lines"""
    prefix = h.compact(strings) + " " + str(seed) + " "
    return "".join(prefix + h.compact(entry) + "\n" for entry in run_problem(strings, seed, k))


def problem_trace(i):
    """rule-battery.scm: b:problem-trace"""
    strings, seeds = h.PROBLEMS[i]
    return chez.String("".join(trace_lines(strings, seed, CODELET_CAP) for seed in seeds))


def apply_data(rule, string):
    """rule-battery.scm: b:apply-data"""
    rules = M("rules")
    result = rules.apply_rule(rule, string, rules.ignore_snag)
    return [h.datum(result), nms(tell(string, "generate-image-letters"))]


def examine_rule(rule):
    """rule-battery.scm: b:examine-rule"""
    type_ = tell(rule, "get-rule-type")
    from_ = h.get_global("*initial-string*" if type_ == "top" else "*target-string*")
    to = h.get_global("*target-string*" if type_ == "top" else "*initial-string*")
    entry = h.rule_entry(rule)
    works = tell(rule, "currently-works?")
    applied = apply_data(rule, from_)
    result = M("answers").translate(rule)
    if result is not False:
        translated = result[0]
        applied_to = apply_data(translated, to)
        translation = [h.rule_data(translated),
                       [h.bridge_data(b) for b in result[1]],
                       slippage_log_data(result[2]),
                       [h.obj_id(o) for o in result[3]],
                       h.datum(result[4]),
                       h.datum(result[5]),
                       applied_to]
    else:
        translation = False
    return [entry, works, applied, translation, chez.random_seed()]


def rule_matrix(strings, seed, k):
    """rule-battery.scm: b:rule-matrix"""
    run_problem(strings, seed, k)
    acc = [examine_rule(r) for r in tell(h.get_global("*workspace*"), "get-all-rules")]
    return chez.String(h.compact(acc))


MATRICES = {
    "rule-matrix-abc-abd-xyz": (["abc", "abd", "xyz"], 1, 1500),
    "rule-matrix-abc-abd-xyz-2": (["abc", "abd", "xyz"], 2, 1500),
    "rule-matrix-abc-abd-mrrjjj": (["abc", "abd", "mrrjjj"], 2, 1500),
    "rule-matrix-abc-abd-kji": (["abc", "abd", "kji"], 3, 1500),
    "rule-matrix-abc-abd-iijjkk": (["abc", "abd", "iijjkk"], 1, 1500),
    "rule-matrix-abc-aabbcc-kkjjii": (["abc", "aabbcc", "kkjjii"], 1, 2000),
    "rule-matrix-eqe-qeq-abbbc": (["eqe", "qeq", "abbbc"], 2, 2000),
    "rule-matrix-apc-abc-opc": (["apc", "abc", "opc"], 2, 1500),
    "rule-matrix-abc-ccbbaa-ijk": (["abc", "ccbbaa", "ijk"], 1, 1500),
    "rule-matrix-xqc-xqd-mrrjjj-mrrjjjj": (["xqc", "xqd", "mrrjjj", "mrrjjjj"], 2, 1500),
    "rule-matrix-rst-rsu-xyz-uyz": (["rst", "rsu", "xyz", "uyz"], 1, 1500),
    "rule-matrix-abc-abd-mrrjjj-mrrkkk": (["abc", "abd", "mrrjjj", "mrrkkk"], 1, 1500),
}


def run_case(name):
    """The value of rule-battery.scm's test `name` (first-answers aside)."""
    if name == "rules-problem-count":
        return len(h.PROBLEMS)
    if name in MATRICES:
        return rule_matrix(*MATRICES[name])
    if re.fullmatch(r"rules-\d\d", name):
        return problem_trace(int(name[len("rules-"):]))
    raise KeyError(name)


# The fast tier -------------------------------------------------------------------------

def test_first_seed_of_problem_0():
    strings, seeds = h.PROBLEMS[0]
    k = 300
    got = trace_lines(strings, seeds[0], k).split("\n")[:-1]
    prefix = h.compact(strings) + " " + str(seeds[0]) + " "
    expected = [l for l in lines_of(fixture(BATTERY, "rules-00")) if l.startswith(prefix)]
    # the start and the first k codelets with their updates; the end entry differs
    assert len(expected) > len(got)
    for n, (a, b) in enumerate(zip(got[:-1], expected), 1):
        assert a == b, first_line_difference('"' + a + '"', '"' + b + '"') + f" (line {n})"
    assert len(got) - 1 == 1 + k + k // 15


def test_every_battery_test_is_translated():
    names = list(manifest(BATTERY))
    assert len(names) == 50
    for name in names:
        assert name in ("rules-problem-count", "first-answers") or name in MATRICES or \
            re.fullmatch(r"rules-\d\d", name), name
    assert fixture(BATTERY, "rules-problem-count") == str(len(h.PROBLEMS))


# The whole battery, in parallel --------------------------------------------------------

def _run(name):
    FIRST_ANSWERS.clear()
    try:
        return name, canon(run_case(name)), list(FIRST_ANSWERS)
    except BaseException:
        return name, "EXCEPTION\n" + traceback.format_exc(), []


@pytest.fixture(scope="module")
def battery_results():
    names = [n for n in manifest(BATTERY) if n != "first-answers"]
    # the longest first (by the size of Chez's output), so that the pool ends evenly
    sizes = {n: len(fixture(BATTERY, n)) for n in names}
    names.sort(key=lambda n: -sizes[n])
    ctx = multiprocessing.get_context("fork")
    with ctx.Pool(min(len(names), os.cpu_count() or 1)) as pool:
        results = {name: (got, answers) for name, got, answers in pool.imap_unordered(_run, names)}
    # rule-battery.scm's first-answers: every problem's runs, in battery order
    entries = []
    for i in range(len(h.PROBLEMS)):
        entries.extend(results[f"rules-{i:02d}"][1])
    results["first-answers"] = (canon(chez.String("(" + " ".join(entries) + ")")), [])
    return {name: got for name, (got, _) in results.items()}


@pytest.mark.slow
@pytest.mark.parametrize("name", manifest(BATTERY))
def test_rule_battery(name, battery_results):
    got = battery_results[name]
    assert not got.startswith("EXCEPTION"), got
    compare(name, got)


@pytest.mark.slow
def test_what_the_runs_reach():
    """The fixtures reach every rule and answer codelet type, top and bottom rules,
    answers, snags and their commentary (racket/tests/rule-diff-test.rkt's checks)."""
    text = "\n".join(fixture(BATTERY, n) for n in manifest(BATTERY))
    for type_ in ["rule-scout", "rule-evaluator", "rule-builder", "answer-finder",
                  "answer-justifier"]:
        assert re.search(r"\([0-9]+ " + re.escape(type_) + " ", text), type_
    for pattern in [r"\(new-rule \(rule top ", r"\(new-rule \(rule bottom ",
                    r"\(trace-event \(answer ", r"\(trace-event \(snag ",
                    r'\(comment add-comment \("The answer ', r'\(comment add-comment \("Uh-oh',
                    r"\(memory-answer-present\? "]:
        assert re.search(pattern, text), pattern
    assert len(re.findall(r"\(answered ", fixture(BATTERY, "first-answers"))) >= 30


# Beyond the battery -----------------------------------------------------------------------

def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


@pytest.mark.parametrize("name", MODULES)
def test_every_definition_has_its_python_function(name):
    mod = M(name)
    missing = [n for n in defines(ORIGINAL / ORIGINS[name])
               if not hasattr(mod, scheme_to_python(n))]
    assert missing == [], (name, missing)


@pytest.mark.parametrize("name", MODULES)
def test_docstrings_name_their_origin(name):
    mod = M(name)
    for fname, fn in vars(mod).items():
        if callable(fn) and getattr(fn, "__module__", None) == mod.__name__ and not fname.startswith("_"):
            assert fn.__doc__ and fn.__doc__.split(":")[0] == ORIGINS[name], (name, fname)


@pytest.mark.parametrize("name", MODULES)
def test_engine_modules_import_no_gui(name):
    tree = ast.parse(inspect.getsource(M(name)))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), name


def test_codelet_procedures_are_installed():
    for name in ["rule-scout", "rule-evaluator", "rule-builder", "answer-finder"]:
        assert getattr(coderack, scheme_to_python(name)).codelet_proc is not False, name


def test_format_slipnode_is_a_top_level_value():
    assert chez.top_level_value("format-slipnode") is M("rules").format_slipnode


def test_modules_are_loaded_in_metacat_ss_order():
    loaded = engine.translated_modules()
    for name in MODULES:
        assert M(name) in loaded
    order = engine.LOAD_ORDER
    assert order.index("images") < order.index("rules") < order.index("answers")


# python/oracle/batteries/rule-extra-battery.scm (local; the transcribe-* and change-phrase-*
# tests were written before the code, the quality-* tests after it) ------------------------

OD_WHOLE = lambda: [slipnet.plato_group, slipnet.plato_string_position_category,  # noqa: E731
                    slipnet.plato_whole]
OD_RMOST = lambda: [slipnet.plato_letter, slipnet.plato_string_position_category,  # noqa: E731
                    slipnet.plato_rightmost]
OD_C = lambda: [slipnet.plato_letter, slipnet.plato_letter_category, slipnet.plato_c]  # noqa: E731


def transcribe(type_, clauses):
    """rule-extra-battery.scm: x:transcribe"""
    return M("rules").transcribe_to_english(type_, clauses)


def _extra_cases():
    s = slipnet
    return {
        "transcribe-group-category-without-bond-facet": lambda: transcribe("top", [
            ["intrinsic", [OD_WHOLE()], [["self", s.plato_group_category, s.plato_predgrp]]]]),
        "transcribe-group-category-with-bond-facet": lambda: transcribe("top", [
            ["intrinsic", [OD_WHOLE()], [["self", s.plato_group_category, s.plato_predgrp],
                                         ["self", s.plato_bond_facet, s.plato_letter_category]]]]),
        "transcribe-group-category-with-length-facet": lambda: transcribe("top", [
            ["intrinsic", [OD_WHOLE()], [["self", s.plato_group_category, s.plato_succgrp],
                                         ["self", s.plato_bond_facet, s.plato_length]]]]),
        "transcribe-group-category-subobjects": lambda: transcribe("top", [
            ["intrinsic", [OD_WHOLE()],
             [["subobjects", s.plato_group_category, s.plato_predgrp]]]]),
        "transcribe-no-clauses": lambda: transcribe("bottom", []),
        "transcribe-verbatim": lambda: transcribe("top", [
            ["verbatim", [s.plato_x, s.plato_y, s.plato_z, s.plato_a]]]),
        "transcribe-letter-changes": lambda: transcribe("top", [
            ["intrinsic", [OD_RMOST()], [["self", s.plato_letter_category, s.plato_successor]]],
            ["intrinsic", [OD_C()], [["self", s.plato_letter_category, s.plato_d]]]]),
        "transcribe-direction-and-length": lambda: transcribe("bottom", [
            ["intrinsic", [OD_WHOLE()],
             [["subobjects", s.plato_direction_category, s.plato_right],
              ["self", s.plato_length, s.plato_four],
              ["subobjects", s.plato_length, s.plato_two]]]]),
        "transcribe-string-position": lambda: transcribe("top", [
            ["intrinsic", [OD_RMOST()],
             [["self", s.plato_string_position_category, s.plato_leftmost]]]]),
        "transcribe-alphabetic-position": lambda: transcribe("top", [
            ["intrinsic", [OD_RMOST()],
             [["self", s.plato_alphabetic_position_category, s.plato_alphabetic_first]]]]),
        "transcribe-long-lines": lambda: transcribe("top", [
            ["intrinsic", [OD_RMOST()], [["self", s.plato_letter_category, s.plato_predecessor]]],
            ["intrinsic", [OD_WHOLE()], [["subobjects", s.plato_direction_category, s.plato_left]]],
            ["extrinsic", [OD_RMOST(), OD_C()],
             [s.plato_letter_category, s.plato_string_position_category]]]),
        "change-phrase-group-category-crash": lambda: M("rules").get_change_phrase(
            chez.String("the whole string"), False, False)(
                ["self", s.plato_group_category, s.plato_predgrp]),
        "change-phrase-group-category-subobjects": lambda: M("rules").get_change_phrase(
            chez.String("the whole string"), True, False)(
                ["subobjects", s.plato_group_category, s.plato_predgrp]),
    }


def quality_values(type_, clauses):
    """rule-extra-battery.scm: x:quality-values"""
    rule = M("rules").make_rule(type_, clauses)
    tell(rule, "set-quality-values")
    return [tell(rule, "get-english-transcription"), tell(rule, "get-uniformity"),
            tell(rule, "get-abstractness"), tell(rule, "get-succinctness"),
            tell(rule, "get-intrinsic-quality"), tell(rule, "get-quality")]


def _quality_cases():
    """rule-extra-battery.scm's quality-* tests (written after the code)"""
    s = slipnet
    return {
        "quality-letter-changes": lambda: quality_values("top", [
            ["intrinsic", [OD_RMOST()], [["self", s.plato_letter_category, s.plato_successor]]],
            ["intrinsic", [OD_C()], [["self", s.plato_letter_category, s.plato_d]]]]),
        "quality-mixed-changes": lambda: quality_values("top", [
            ["intrinsic", [OD_RMOST()], [["self", s.plato_letter_category, s.plato_successor],
                                         ["self", s.plato_length, s.plato_two]]],
            ["intrinsic", [OD_WHOLE()], [["subobjects", s.plato_direction_category, s.plato_left],
                                         ["subobjects", s.plato_letter_category, s.plato_c]]]]),
        "quality-extrinsic": lambda: quality_values("top", [
            ["extrinsic", [OD_RMOST(), OD_C()],
             [s.plato_letter_category, s.plato_string_position_category]],
            ["intrinsic", [OD_WHOLE()], [["self", s.plato_length, s.plato_four]]]]),
        "quality-verbatim": lambda: quality_values("top", [
            ["verbatim", [s.plato_x, s.plato_y, s.plato_z]]]),
    }


def test_every_extra_test_is_translated():
    assert list(_extra_cases()) + list(_quality_cases()) == list(manifest("rule-extra"))


@pytest.fixture(scope="module")
def extra_problem():
    from test_workspace import init_problem
    init_problem(["abc", "ccbbaa", "ijk"], 3)


@pytest.mark.parametrize("name", manifest("rule-extra"))
def test_rule_extra_battery(name, extra_problem):
    expected = fixture("rule-extra", name)
    case = {**_extra_cases(), **_quality_cases()}[name]
    if expected == "ERROR":
        # 1.2: the caddr of #f in get-change-phrase (anomalies: "`caddr` of `#f` in
        # `transcribe-to-english`")
        with pytest.raises(chez.SchemeError):
            case()
    else:
        assert canon(case()) == expected
