"""tests/diff/codelet-harness.scm in Python (test helper, loop0002 item 07).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The codelet-level differential harness: bonds.ss, groups.ss and
concept-mappings.ss run codelet by codelet.  `run_codelets` is a copy of run.ss's
run-mcat loop restricted to the bond and group codelet types (the harness's own
header explains the restrictions); `compact` is b:compact, the trace line printer.
Each definition below is the harness's definition of the same name (b:x → x).
The procedures of workspace-dump.scm come from test_workspace.py, which translates
them.  Item 07 uses the bonds-and-groups setting; item 08 adds the bridges
setting (`enable_bridges`, b:enable-bridges!); item 09 the rules setting
(`RULES`, b:rules?, with `ANSWERED`, b:answered), which test_rules.py turns on.

`install()` makes the harness's top-level settings and fakes (b:set-global! ...):
call it once, after engine.load(), inside the stand-ins of STAND_INS.
"""
from __future__ import annotations

import io
from contextlib import redirect_stdout

from scheme_canon import canon
from test_workspace import (PROBLEMS, deep_nm, dump_description, dump_object, get_global,
                            init_problem, nm, set_global, update_workspace_values)
from metacat import chez, coderack, setup, slipnet, sugar, utilities
from metacat.chez import String
from metacat.objects import Lambda, procedure_p, tell

# Settings and fakes ---------------------------------------------------------------------

EVENTS: list = []          # b:events (most recent first, as in the harness)


def event(e):
    """codelet-harness.scm: b:event!"""
    EVENTS.insert(0, e)
    return "done"


def fake_themespace_fn(self, msg, *args):
    """codelet-harness.scm: b:fake-themespace"""
    if msg == "get-active-themes":
        return []
    if msg == "get-all-active-themes":
        return []
    if msg == "add-theme-if-possible":
        theme_type, dimension, relation = args
        event(["add-theme", theme_type, nm(dimension), nm(relation)])
        return False
    if msg == "update-dominant-themes":
        return event(["update-dominant-themes", args[0]])
    if msg == "get-dominant-theme-pattern":
        return [args[0]]
    raise chez.SchemeError("fake-themespace", "unexpected message ~s", msg)


def null_themespace_window_fn(self, msg, *args):
    """codelet-harness.scm: b:null-themespace-window"""
    return event(["themespace-window", msg])


def fake_eeg_fn(self, msg, *args):
    """codelet-harness.scm: b:fake-eeg"""
    return "done"


def null_workspace_window_fn(self, msg, *args):
    """codelet-harness.scm: b:null-workspace-window"""
    return event(["window", msg])


def null_coderack_window_fn(self, msg, *args):
    """codelet-harness.scm: b:null-coderack-window"""
    return "done"


def monitor_slipnode_activation_change(node, previous, new):
    """codelet-harness.scm: monitor-slipnode-activation-change (trace.ss's, recording)"""
    return event(["activation", nm(node), previous, new])


def monitor_new_groups(group, flipped_p):
    """codelet-harness.scm: monitor-new-groups (trace.ss's, recording)"""
    return event(["new-group", group_data(group), flipped_p, tell(group, "get-strength")])


def monitor_new_concept_mappings(cms, bridge):
    """codelet-harness.scm: monitor-new-concept-mappings (trace.ss's, recording)"""
    return event(["new-cms", cm_names(cms), bridge_data(bridge)])


# Globals of files not translated yet that the harness sets or the engine reads.
STAND_INS = {
    "run": {"p_update_cycle_length": 15, "g_temperature_clamped_p": False,
            "g_step_mode_p": False, "p_step_cycles": 1, "g_display_mode_p": False},
    "themes": {"g_themespace": Lambda(fake_themespace_fn)},
    "eeg_graphics": {"g_EEG": Lambda(fake_eeg_fn)},
    "trace": {"monitor_slipnode_activation_change": monitor_slipnode_activation_change,
              "monitor_new_groups": monitor_new_groups,
              "monitor_new_concept_mappings": monitor_new_concept_mappings},
}


def install():
    """codelet-harness.scm: the top-level b:set-global! forms and set-graphics-parameters."""
    set_global("*workspace-window*", Lambda(null_workspace_window_fn))
    set_global("*themespace-window*", Lambda(null_themespace_window_fn))
    set_global("%workspace-graphics%", False)
    set_global("%slipnet-graphics%", False)
    set_global("%coderack-graphics%", False)
    set_global("%verbose%", False)
    set_global("%self-watching-enabled%", False)
    set_global("*top-down-slipnodes*", bond_group_slipnodes())
    BRIDGES[0] = False
    BOTTOM_UP_TYPES[:] = [coderack.bottom_up_bond_scout, coderack.group_scout__whole_string]
    window = Lambda(null_coderack_window_fn)
    for type_ in coderack.g_codelet_types:
        tell(type_, "set-graphics-parameters", window, False, False, False, False, False,
             False, False, False)


def compact(x):
    """codelet-harness.scm: b:compact"""
    if isinstance(x, (list, tuple)) and not isinstance(x, chez.Vector):
        return "(" + " ".join(compact(y) for y in x) + ")"
    if isinstance(x, str) and not isinstance(x, (String, chez.Char)):
        return x
    return canon(x)


# Structures as data ----------------------------------------------------------------------

def obj_id(obj):
    """codelet-harness.scm: b:obj-id"""
    return [tell(obj, "which-string"), tell(obj, "object-type"),
            tell(obj, "get-left-string-pos"), tell(obj, "get-right-string-pos")]


def bond_data(bond):
    """codelet-harness.scm: b:bond-data"""
    return ["bond", obj_id(tell(bond, "get-from-object")), obj_id(tell(bond, "get-to-object")),
            nm(tell(bond, "get-bond-category")), nm(tell(bond, "get-bond-facet")),
            nm(tell(bond, "get-direction")), nm(tell(bond, "get-from-object-descriptor")),
            nm(tell(bond, "get-to-object-descriptor"))]


def group_data(group):
    """codelet-harness.scm: b:group-data"""
    return ["group", obj_id(group), nm(tell(group, "get-group-category")),
            nm(tell(group, "get-bond-facet")), nm(tell(group, "get-direction")),
            [obj_id(o) for o in tell(group, "get-constituent-objects")],
            [bond_data(b) for b in tell(group, "get-constituent-bonds")]]


def bridge_data(bridge):
    """codelet-harness.scm: b:bridge-data"""
    return ["bridge", tell(bridge, "get-bridge-type"), obj_id(tell(bridge, "get-object1")),
            obj_id(tell(bridge, "get-object2")), tell(bridge, "flipped-group1?"),
            tell(bridge, "flipped-group2?")]


def cm_names(cms):
    """codelet-harness.scm: b:cm-names"""
    return [tell(cm, "print-name") for cm in cms]


def strings():
    """codelet-harness.scm: b:strings"""
    s = [get_global("*initial-string*"), get_global("*modified-string*"),
         get_global("*target-string*")]
    if get_global("%justify-mode%") is not False:
        s.append(get_global("*answer-string*"))
    return s


def structures():
    """codelet-harness.scm: b:structures"""
    out = []
    for s in strings():
        for b in tell(s, "get-bonds"):
            out.append([bond_data(b), tell(b, "get-proposal-level"), tell(b, "get-strength"),
                        tell(b, "get-time-stamp")])
        for g in tell(s, "get-groups"):
            out.append([group_data(g), tell(g, "get-proposal-level"), tell(g, "get-strength"),
                        tell(g, "get-time-stamp"),
                        [dump_description(d) for d in tell(g, "get-descriptions")]])
    for b in tell(get_global("*workspace*"), "get-all-bridges"):
        out.append([bridge_data(b), tell(b, "get-proposal-level"), tell(b, "get-strength"),
                    tell(b, "get-time-stamp"), cm_names(tell(b, "get-concept-mappings")),
                    cm_names(tell(b, "get-bond-concept-mappings")),
                    cm_names(tell(b, "get-symmetric-slippages"))])
    if RULES[0]:
        out.extend(rule_entry(r) for r in tell(get_global("*workspace*"), "get-all-rules"))
    return out


def datum(x):
    """codelet-harness.scm: b:datum (rule clauses, failure results, transforms as data)"""
    if isinstance(x, list) and not isinstance(x, chez.Vector):
        return [datum(y) for y in x]
    if isinstance(x, chez.Pair):
        a = datum(x.car)
        return chez.Pair(a, datum(x.cdr))
    if not procedure_p(x):
        return x
    if utilities.slipnode_p(x):
        return nm(x)
    if utilities.letter_p(x) or utilities.group_p(x):
        return obj_id(x)
    if utilities.workspace_string_p(x):
        return ["string", tell(x, "get-string-type")]
    if utilities.bridge_p(x):
        return bridge_data(x)
    if utilities.concept_mapping_p(x):
        return ["cm", tell(x, "print-name")]
    return "object"


def rule_data(rule):
    """codelet-harness.scm: b:rule-data"""
    return ["rule", tell(rule, "get-rule-type"), tell(rule, "get-english-transcription"),
            datum(tell(rule, "get-rule-clauses"))]


def rule_entry(rule):
    """codelet-harness.scm: b:rule-entry"""
    return [rule_data(rule), tell(rule, "get-proposal-level"), tell(rule, "get-strength"),
            tell(rule, "get-time-stamp"),
            [tell(rule, "get-quality"), tell(rule, "get-relative-quality"),
             tell(rule, "get-uniformity"), tell(rule, "get-abstractness"),
             tell(rule, "get-succinctness")],
            tell(rule, "supported?"),
            datum(tell(rule, "get-tagged-supporting-horizontal-bridges")),
            tell(rule, "get-theme-pattern")]


def difference(after, before):
    """codelet-harness.scm: b:difference (compared by data, as equal? does)"""
    keys = [canon(entry[0]) for entry in before]
    return [entry for entry in after if canon(entry[0]) not in keys]


def description_counts():
    """codelet-harness.scm: b:description-counts"""
    return map_objects(lambda obj: len(tell(obj, "get-descriptions")))


def proposed_counts():
    """codelet-harness.scm: b:proposed-counts"""
    counts = [[len(tell(s, "get-all-bonds")), len(tell(s, "get-all-groups"))] for s in strings()]
    if BRIDGES[0]:
        ws = get_global("*workspace*")
        counts = counts + [len(tell(ws, "get-proposed-bridges", "top")),
                           len(tell(ws, "get-proposed-bridges", "vertical")),
                           description_counts()]
    return counts


def activations():
    """codelet-harness.scm: b:activations"""
    return [tell(n, "get-activation") for n in slipnet.g_slipnet_nodes]


def dump_ws_object(obj):
    """codelet-harness.scm: b:dump-ws-object"""
    if utilities.letter_p(obj):
        return dump_object(obj)
    return [group_data(obj), tell(obj, "print-name"), tell(obj, "get-id-num"),
            [dump_description(d) for d in tell(obj, "get-descriptions")],
            ["importance", tell(obj, "get-raw-importance"), tell(obj, "get-relative-importance")],
            ["unhappiness", tell(obj, "get-intra-string-unhappiness"),
             tell(obj, "get-inter-string-unhappiness", "horizontal"),
             tell(obj, "get-inter-string-unhappiness", "vertical"),
             tell(obj, "get-average-unhappiness")],
            ["salience", tell(obj, "get-intra-string-salience"),
             tell(obj, "get-inter-string-salience", "horizontal"),
             tell(obj, "get-inter-string-salience", "vertical"),
             tell(obj, "get-average-salience")],
            ["strength", tell(obj, "get-proposal-level"), tell(obj, "get-strength")],
            deep_nm(tell(tell(obj, "get-image"), "generate"))]


# The run loop ----------------------------------------------------------------------------

UNCLAMP_TIME = [0]          # b:unclamp-time
BRIDGES = [False]           # b:bridges?
BOTTOM_UP_TYPES: list = []  # b:bottom-up-types (set by enable_bridges)
RULES = [False]             # b:rules?
ANSWERED = [False]          # b:answered


def bond_group_slipnodes():
    """codelet-harness.scm: b:bond-group-slipnodes"""
    s = slipnet
    return [s.plato_left, s.plato_right, s.plato_predecessor, s.plato_successor,
            s.plato_sameness, s.plato_predgrp, s.plato_succgrp, s.plato_samegrp]


def clamp_initial_slipnodes():
    """codelet-harness.scm: b:clamp-initial-slipnodes"""
    for node in slipnet.g_initially_clamped_slipnodes:
        tell(node, "clamp", slipnet.p_max_activation)
    UNCLAMP_TIME[0] = setup.g_codelet_count + 50 * get_global("%update-cycle-length%")


def post_initial_codelets():
    """codelet-harness.scm: b:post-initial-codelets"""
    cr = coderack.g_coderack
    i = 2 * len(tell(get_global("*workspace*"), "get-objects"))
    while i > 0:
        tell(cr, "add-deferred-codelet",
             tell(coderack.bottom_up_bond_scout, "make-codelet", coderack.p_very_low_urgency))
        if BRIDGES[0]:
            tell(cr, "add-deferred-codelet",
                 tell(coderack.bottom_up_bridge_scout, "make-codelet",
                      coderack.p_very_low_urgency))
        i -= 1
    tell(cr, "post-deferred-codelets")


def enable_bridges(on):
    """codelet-harness.scm: b:enable-bridges!"""
    BRIDGES[0] = on
    c = coderack
    if on:
        BOTTOM_UP_TYPES[:] = [c.bottom_up_bond_scout, c.group_scout__whole_string,
                              c.bottom_up_bridge_scout, c.important_object_bridge_scout,
                              c.bottom_up_description_scout, c.breaker]
        set_global("*top-down-slipnodes*",
                   bond_group_slipnodes() + [slipnet.plato_string_position_category,
                                             slipnet.plato_alphabetic_position_category,
                                             slipnet.plato_length])
    else:
        BOTTOM_UP_TYPES[:] = [c.bottom_up_bond_scout, c.group_scout__whole_string]
        set_global("*top-down-slipnodes*", bond_group_slipnodes())


def add_bottom_up_codelets():
    """codelet-harness.scm: b:add-bottom-up-codelets over b:bottom-up-types"""
    for codelet_type in BOTTOM_UP_TYPES:
        if utilities.prob_p(coderack.post_codelet_probability(codelet_type)):
            urgency = coderack.bottom_up_urgency(codelet_type)
            i = coderack.num_of_codelets_to_post(codelet_type)
            while i > 0:
                tell(coderack.g_coderack, "add-deferred-codelet",
                     tell(codelet_type, "make-codelet", urgency))
                i -= 1


def update_everything():
    """codelet-harness.scm: b:update-everything"""
    if RULES[0]:
        tell(get_global("*workspace*"), "check-if-rules-possible")
    update_workspace_values()
    # run.ss's end of a snag period, with rule-battery.scm's fake Trace
    if RULES[0]:
        trace = get_global("*trace*")
        if tell(trace, "within-snag-period?") is not False:
            progress_achieved = tell(trace, "progress-since-last-snag")
            sugar.stochastic_if_star(lambda: utilities.percent(progress_achieved),
                                     lambda: tell(trace, "undo-snag-condition"))
    slipnet.update_slipnet_activations()
    import metacat.formulas as formulas
    formulas.update_temperature()
    if RULES[0]:
        coderack.add_bottom_up_codelets()
    else:
        add_bottom_up_codelets()
    coderack.add_top_down_codelets()
    tell(coderack.g_coderack, "post-deferred-codelets")


def step():
    """codelet-harness.scm: b:step"""
    codelet = tell(coderack.g_coderack, "choose-codelet")
    before = structures()
    EVENTS.clear()
    tell(codelet, "run")
    set_global("*codelet-count*", 1 + setup.g_codelet_count)
    after = structures()
    built = difference(after, before)
    broken = difference(before, after)
    rng = chez.random_seed()
    return [setup.g_codelet_count, tell(codelet, "get-codelet-type-name"),
            tell(codelet, "get-relative-urgency"), tell(codelet, "get-time-stamp"), rng,
            ["built", built], ["broken", broken], ["events", list(reversed(EVENTS))],
            ["proposed", proposed_counts()]]


def map_objects(f):
    """codelet-harness.scm: b:map-objects"""
    return [f(o) for o in tell(get_global("*workspace*"), "get-objects")]


def run_codelets(strings_, seed, k):
    """codelet-harness.scm: b:run-codelets, as a list of trace entries"""
    init_problem(strings_, seed)
    ANSWERED[0] = False
    tell(coderack.g_coderack, "initialize")
    clamp_initial_slipnodes()
    post_initial_codelets()
    cycle_length = get_global("%update-cycle-length%")
    trace = [["start", strings_, seed, chez.random_seed(), map_objects(dump_ws_object)]]
    while setup.g_codelet_count != k and not ANSWERED[0]:
        entry = step()
        trace.append(entry)
        if setup.g_codelet_count == UNCLAMP_TIME[0]:
            for node in slipnet.g_initially_clamped_slipnodes:
                tell(node, "unfreeze")
        if tell(coderack.g_coderack, "empty?") is not False:
            post_initial_codelets()
            clamp_initial_slipnodes()
        if setup.g_codelet_count % cycle_length == 0:
            EVENTS.clear()
            update_everything()
            trace.append(["update", setup.g_codelet_count, setup.g_temperature, activations(),
                          tell(coderack.g_coderack, "get-num-of-codelets"), chez.random_seed(),
                          list(reversed(EVENTS)),
                          (map_objects(dump_ws_object)
                           if setup.g_codelet_count % (4 * cycle_length) == 0 else [])])
    trace.append(["end", structures(), map_objects(dump_ws_object), activations(),
                  setup.g_temperature, chez.random_seed()])
    return trace


# codelet-battery.scm ----------------------------------------------------------------------

CODELET_CAP = 400


def trace_lines(strings_, seed, k):
    """codelet-battery.scm and bridge-battery.scm: b:trace-lines"""
    prefix = compact(strings_) + " " + str(seed) + " "
    return "".join(prefix + compact(entry) + "\n" for entry in run_codelets(strings_, seed, k))


def problem_trace(i):
    """codelet-battery.scm: b:problem-trace"""
    strings_, seeds = PROBLEMS[i]
    return String("".join(trace_lines(strings_, seed, CODELET_CAP) for seed in seeds))


def long_trace(strings_, seed, k):
    """codelet-battery.scm: b:long-trace"""
    return String(trace_lines(strings_, seed, k))


LONG = {
    "codelets-long-aaabaaa-1": (["eqe", "qeq", "abbba", "aaabaaa"], 1, 2000),
    "codelets-long-aaabaaa-2": (["eqe", "qeq", "abbba", "aaabaaa"], 2, 2000),
    "codelets-long-aaabaaa-4": (["eqe", "qeq", "abbba", "aaabaaa"], 4, 2000),
    "codelets-long-bbbxbbb-1": (["eqe", "qeq", "bxxxb", "bbbxbbb"], 1, 2000),
    "codelets-long-mrrjjjj-2": (["xqc", "xqd", "mrrjjj", "mrrjjjj"], 2, 2000),
    "codelets-long-iijjkk-3": (["abc", "abd", "iijjkk"], 3, 2000),
    "codelets-long-xxixx-3": (["eeqee", "qeeq", "xxixx"], 3, 2000),
}


def capture(thunk):
    """b:capture"""
    out = io.StringIO()
    with redirect_stdout(out):
        thunk()
    return String(out.getvalue())


def cm_data(cm):
    """codelet-battery.scm: b:cm-data"""
    link = tell(cm, "get-slipnet-link")
    sym = tell(cm, "symmetric-mapping")
    import metacat.concept_mappings as cms
    return [tell(cm, "print-name"), tell(cm, "english-name"), tell(cm, "long-name"),
            capture(lambda: tell(cm, "print")),
            nm(tell(cm, "get-CM-type")),
            ([nm(tell(link, "get-from-node")), nm(tell(link, "get-to-node")),
              tell(link, "get-degree-of-assoc")] if link is not False else False),
            tell(cm, "CM-type?", slipnet.plato_letter_category),
            tell(cm, "CM-type?", slipnet.plato_string_position_category),
            tell(cm, "bond-concept-mapping?"),
            tell(cm, "reversible-CM-type?"),
            nm(tell(cm, "get-descriptor1")), nm(tell(cm, "get-descriptor2")),
            nm(tell(cm, "get-label")),
            tell(cm, "slippage?"), tell(cm, "identity?"), tell(cm, "opposite-mapping?"),
            tell(cm, "identity/opposite-mapping?"), tell(cm, "relevant?"),
            tell(cm, "distinguishing?"), tell(cm, "relevant-distinguishing?"),
            tell(cm, "distinguishing-identity/opposite?"),
            tell(cm, "get-degree-of-assoc"), tell(cm, "get-conceptual-depth"),
            tell(cm, "get-strength"), tell(cm, "get-slippability"),
            deep_nm(tell(cm, "get-concept-pattern")),
            tell(sym, "print-name"), sym is cm, tell(cm, "symmetric?", sym),
            tell(sym, "symmetric?", cm), cms.CMs_equal_p(cm, sym),
            tell(cm, "previously-relevant?"), tell(cm, "object-type")]


def categories():
    """codelet-battery.scm: b:categories"""
    return [n for n in slipnet.g_slipnet_nodes if tell(n, "category?") is not False]


def category_cms(c, object1, object2):
    """codelet-battery.scm: b:category-cms"""
    import metacat.concept_mappings as cms
    instances = tell(c, "get-instance-nodes")
    out = []
    for d1 in instances:
        for d2 in instances:
            out.append(cms.make_concept_mapping(object1, c, d1, object2, c, d2))
    return out


def category_cm_test(c):
    """codelet-battery.scm: b:category-cm-test"""
    import metacat.concept_mappings as cms
    init_problem(["abc", "abd", "mrrjjj"], 1)
    l = category_cms(c, tell(get_global("*initial-string*"), "get-letter", 0),
                     tell(get_global("*target-string*"), "get-letter", 0))
    return [nm(c), len(l), [cm_data(cm) for cm in l],
            [tell(cm, "print-name") for cm in cms.remove_duplicate_CMs(l)]]


def workspace_cm_test(strings_, seed, k):
    """codelet-battery.scm: b:workspace-cm-test"""
    import metacat.concept_mappings as cms
    run_codelets(strings_, seed, k)
    objects1 = tell(get_global("*initial-string*"), "get-objects")
    objects2 = tell(get_global("*target-string*"), "get-objects")
    l = []
    for o1 in objects1:
        for o2 in objects2:
            for d1 in tell(o1, "get-descriptions"):
                for d2 in tell(o2, "get-descriptions"):
                    if tell(d2, "get-description-type") is tell(d1, "get-description-type"):
                        l.append(cms.make_concept_mapping(
                            o1, tell(d1, "get-description-type"), tell(d1, "get-descriptor"),
                            o2, tell(d2, "get-description-type"), tell(d2, "get-descriptor")))
    data = [cm_data(cm) for cm in l]
    EVENTS.clear()
    for cm in l:
        tell(cm, "activate-descriptions")
        tell(cm, "activate-label")
    for n in slipnet.g_slipnet_nodes:
        tell(n, "flush-activation-buffer")
    return [len(l), [obj_id(o) for o in objects1], [obj_id(o) for o in objects2], data,
            [tell(cm, "print-name") for cm in cms.remove_duplicate_CMs(l)],
            activations(), list(reversed(EVENTS))]


CM_WORKSPACE = {
    "cm-workspace-abc-abd-xyz": (["abc", "abd", "xyz"], 1, 600),
    "cm-workspace-abc-abd-mrrjjj": (["abc", "abd", "mrrjjj"], 2, 600),
    "cm-workspace-eqe-qeq-abbbc": (["eqe", "qeq", "abbbc"], 3, 600),
    "cm-workspace-abc-abd-kji": (["abc", "abd", "kji"], 1, 600),
    "cm-workspace-aabc-aabd-ijkk": (["aabc", "aabd", "ijkk"], 2, 600),
}


def run_case(name):
    """The value of codelet-battery.scm's test `name`."""
    if name == "codelets-problem-count":
        return len(PROBLEMS)
    if name.startswith("codelets-long-"):
        return long_trace(*LONG[name])
    if name.startswith("codelets-"):
        return problem_trace(int(name[len("codelets-"):]))
    if name == "cm-category-count":
        return [nm(c) for c in categories()]
    if name.startswith("cm-categories-"):
        return category_cm_test(categories()[int(name[len("cm-categories-"):])])
    if name in CM_WORKSPACE:
        return workspace_cm_test(*CM_WORKSPACE[name])
    raise KeyError(name)
