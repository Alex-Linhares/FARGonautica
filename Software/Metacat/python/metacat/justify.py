"""justify.ss: the answer-justifier codelet, clamping rules, rule unification and
the comparison of rule-clause lists.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from justify.ss, with
racket/engine/justify.rktl as a worked translation.

The file defines no objects.  The answer-justifier codelet procedure is given to
its codelet type by load() (define-codelet-procedure* needs the type, which
coderack.load makes).  The closures that unify-rules, compare-rule-clause-lists
and retention-probability make are Python closures; the two continuation-point*
forms are sugar.continuation_point_star.  traverse-rule-clauses walks the clause
trees recursively over their nesting only (a loop over each list's elements),
so its depth is the clauses' depth.

Earlier files are imported directly: answers.ss (translate, report-new-answer,
get-rule-supporting-groups, make-translated-string), concept-mappings.ss
(make-concept-mapping, remove-duplicate-CMs), slipnet.ss (slip-linked? and the
plato- nodes), rules.ss (verbatim-clause?), setup.ss (%verbose%), workspace.ss
(*workspace*, *initial-string*, *target-string*, *answer-string*).  Names from
files translated later are read through the package at call time: trace.ss
(*trace*, make-clamp-event, monitor-new-rules, %top-down-codelet-pattern%,
%thematic-codelet-pattern%), themes.ss (*themespace*) and memory.ss
(*memory*).  answers.ss and bridges.ss call remove-whole/single-concept-mappings
and compare-rule-clause-lists from here.  The engine never imports tkinter.

Evaluation order (audited against Chez's): the draws are answer-justifier's
stochastic-pick-by-method (one stochastic-pick after a tell-all through
chez.map_), translate (answers.ss; draws through apply-slippages and its
prob? 0.4 filter), the unification section's stochastic-pick, and
get-vertical-theme-pattern-to-clamp's (filter (compose prob? get-probability)
...), which utilities.ss's filter runs first to last.  The last is an argument
of the final clamp-rules call, whose other arguments ((list ...) of two rules
and two get-concept-pattern messages) only read, so its place among them is not
observable; it is computed first here.  The other clamp-rules calls pass
get-dominant-theme-pattern (themes.ss: a map-compress over the clusters, which
only reads) and get-concept-pattern (reads).  The three-binding let of mapping
strengths, the two-binding lets of rule1/rule2, the appends of supporting
groups (a variable and get-rule-supporting-groups, which reads), clamp-rules'
append and concept-mapping-proc's make-concept-mapping arguments only read.
traverse-rule-clauses walks a list's rest before its first element (the
inner walk is an argument of the outer one), so a length mismatch fails before
any element is compared, and the elements are visited last to first; the
Python keeps that order (concept-mapping-proc makes concept mappings, and fail
escapes).  vprintf and vprint are macros that evaluate their arguments only
with %verbose% on; the sites with computed arguments are gated likewise.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, sugar
from metacat import answers, concept_mappings, rules, setup, slipnet, workspace
from metacat.objects import tell
from metacat.sugar import vprint, vprintf
from metacat.utilities import (cd, compose, exists_p, fifth, filter_, filter_meth, first,
                               fourth, hundred_minus, percent, prob_p, remq_duplicates,
                               round_to_100ths, second, select, select_meth, sixth,
                               stochastic_pick, stochastic_pick_by_method, tell_all, third)


def _list_p(x) -> bool:
    """Chez: list? (on the model's data: a Python list or tuple)"""
    t = type(x)
    return t is list or t is tuple


def _null_p(x) -> bool:
    """Chez: null?"""
    return _list_p(x) and len(x) == 0


def _symbol_p(x) -> bool:
    """Chez: symbol? (symbols are str: docs/python-translation-plan.md)"""
    return isinstance(x, str)


def answer_justifier():
    """justify.ss: answer-justifier (the codelet procedure)"""
    # a three-binding let; get-mapping-strength only reads (and the values are unused)
    top_map_strength = tell(workspace.g_workspace, "get-mapping-strength", "top")  # noqa: F841
    vertical_map_strength = tell(workspace.g_workspace, "get-mapping-strength", "vertical")  # noqa: F841
    bottom_map_strength = tell(workspace.g_workspace, "get-mapping-strength", "bottom")  # noqa: F841
    chosen_rule = stochastic_pick_by_method(
        tell(workspace.g_workspace, "get-all-supported-rules"), "get-strength")
    if not exists_p(chosen_rule):
        vprintf("Couldn't find a supported rule. Fizzling.~n")
        sugar.fizzle()

    vprintf("Chose rule:~n")
    vprint(chosen_rule)
    rule_type = tell(chosen_rule, "get-rule-type")
    other_rules = tell(workspace.g_workspace, "get-rules",
                       "bottom" if rule_type == "top" else "top")
    result = answers.translate(chosen_rule)

    if not exists_p(result):
        vprintf("Couldn't translate chosen rule...~n")
    # ...and skip directly to unification section

    if exists_p(result):
        translated_rule = first(result)
        supporting_vertical_bridges = second(result)
        slippage_log = third(result)
        vertical_mapping_supporting_groups = fourth(result)
        from_ref_objs = fifth(result)
        to_ref_objs = sixth(result)
        ref_objs1 = from_ref_objs if rule_type == "top" else to_ref_objs
        ref_objs2 = to_ref_objs if rule_type == "top" else from_ref_objs
        matching_rule = select_meth(other_rules, "equal?", translated_rule)

        vprintf("Chosen rule translated:~n")
        vprint(translated_rule)

        if exists_p(matching_rule):
            vprintf("Found a matching rule:~n")
            vprint(matching_rule)
            # a two-binding let that only reads
            rule1 = chosen_rule if rule_type == "top" else matching_rule
            rule2 = matching_rule if rule_type == "top" else chosen_rule
            if tell(_metacat.memory.g_memory, "answer-present?",
                    tell(workspace.g_answer_string, "get-letter-categories"),
                    rule1, rule2) is not False:
                vprintf("Already found this answer. Fizzling.~n")
                sugar.fizzle()
            if tell(matching_rule, "supported?") is not False:
                # append's arguments only read
                all_supporting_groups = remq_duplicates(
                    vertical_mapping_supporting_groups
                    + answers.get_rule_supporting_groups(rule1, rule2))
                answers.report_new_answer(
                    workspace.g_answer_string, rule1, rule2, supporting_vertical_bridges,
                    all_supporting_groups, ref_objs1, ref_objs2, slippage_log, [])
                sugar.fizzle()
            vprintf("The matching rule is not currently supported...~n")
            vprintf("Attempting to clamp rules...~n")
            if tell(_metacat.trace.g_trace, "permission-to-clamp?") is not False:
                vprintf("Permission granted. Clamping rules...~n")
                # the arguments only read
                clamp_rules(
                    [chosen_rule, matching_rule],
                    tell(_metacat.themes.g_themespace, "get-dominant-theme-pattern",
                         "vertical-bridge"),
                    tell(matching_rule, "get-concept-pattern"))
            sugar.fizzle()

        vprintf("Couldn't find a matching rule...~n")
        # The ref-objs1 check avoids cases in which a bottom rule gets
        # translated to a top rule that works but doesn't refer to anything
        # in the initial string.  Example:  xqd -> xqd; mrrjjj -> mrrjjjj
        # The bottom rule "Increase length of j group by one" may get translated
        # as "Increase length of letter j by one", which works (vacuously)
        # when applied to xqd -> xqd.
        if (tell(translated_rule, "currently-works?") is not False
                and not _null_p(ref_objs1)):
            vprintf("Translated rule works...~n")
            # a two-binding let that only reads
            rule1 = chosen_rule if rule_type == "top" else translated_rule
            rule2 = translated_rule if rule_type == "top" else chosen_rule
            if tell(_metacat.memory.g_memory, "answer-present?",
                    tell(workspace.g_answer_string, "get-letter-categories"),
                    rule1, rule2) is not False:
                vprintf("Already found this answer. Fizzling.~n")
                sugar.fizzle()
            tell(translated_rule, "set-quality-values")
            # This sets translated-rule's supporting-horizontal-bridges
            # and theme-pattern:
            if rule_type == "top":
                answers.make_translated_string(translated_rule, workspace.g_target_string)
            else:
                answers.make_translated_string(translated_rule, workspace.g_initial_string)
            tell(workspace.g_workspace, "add-rule", translated_rule)
            _metacat.trace.monitor_new_rules(translated_rule)
            if tell(translated_rule, "supported?") is not False:
                # append's arguments only read
                all_supporting_groups = remq_duplicates(
                    vertical_mapping_supporting_groups
                    + answers.get_rule_supporting_groups(rule1, rule2))
                answers.report_new_answer(
                    workspace.g_answer_string, rule1, rule2, supporting_vertical_bridges,
                    all_supporting_groups, ref_objs1, ref_objs2, slippage_log, [])
                sugar.fizzle()
            vprintf("The translated rule is not currently supported...~n")
            vprintf("Attempting to clamp rules...~n")
            if tell(_metacat.trace.g_trace, "permission-to-clamp?") is not False:
                vprintf("Permission granted. Clamping rules...~n")
                # the arguments only read
                clamp_rules(
                    [chosen_rule, translated_rule],
                    tell(_metacat.themes.g_themespace, "get-dominant-theme-pattern",
                         "vertical-bridge"),
                    tell(translated_rule, "get-concept-pattern"))
            sugar.fizzle()
        vprintf("Translated rule doesn't work...~n")

    # Unification section
    vprintf("Attempting to unify chosen rule with some other existing rule...~n")
    if _null_p(other_rules):
        vprintf("No other rules exist. Fizzling.~n")
        sugar.fizzle()
    if tell(_metacat.trace.g_trace, "permission-to-clamp?") is False:
        vprintf("Permission to clamp patterns denied. Fizzling.~n")
        sugar.fizzle()
    strength = tell(chosen_rule, "get-strength")
    # chez: map's order of application (the procedure only reads)
    other_weights = chez.map_(
        lambda r: hundred_minus(abs(chez.sub(strength, tell(r, "get-strength")))),
        other_rules)
    other_rule = stochastic_pick(other_rules, other_weights)
    vprintf("Chose a rule for unification:~n")
    vprint(other_rule)
    unifying_theme_pattern = unify_rules(chosen_rule, other_rule)
    if not exists_p(unifying_theme_pattern):
        vprintf("Couldn't unify rules. Fizzling.~n")
        sugar.fizzle()
    vprintf("Rules can be unified. Clamping rules...~n")
    # chez: of clamp-rules' arguments only get-vertical-theme-pattern-to-clamp
    # draws (prob?); the others only read, so the order is not observable
    vertical_theme_pattern = get_vertical_theme_pattern_to_clamp(unifying_theme_pattern)
    return clamp_rules(
        [chosen_rule, other_rule],
        vertical_theme_pattern,
        tell(chosen_rule, "get-concept-pattern"),
        tell(other_rule, "get-concept-pattern"))


def clamp_rules(rules, vertical_theme_pattern, *concept_patterns):
    """justify.ss: clamp-rules.  (clamp-rules rules vertical-theme-pattern . concept-patterns)"""
    # append's arguments only read
    all_patterns = (tell_all(rules, "get-theme-pattern")
                    + [vertical_theme_pattern]
                    + list(concept_patterns)
                    + [_metacat.trace.p_top_down_codelet_pattern,
                       _metacat.trace.p_thematic_codelet_pattern])
    clamp_event = _metacat.trace.make_clamp_event("justify-clamp", all_patterns, rules,
                                                  "workspace")
    # This should happen first so that concept-activation events resulting
    # from the clamp will appear in the trace after the clamp event:
    tell(_metacat.trace.g_trace, "add-event", clamp_event)
    return tell(clamp_event, "activate")


def unify_rules(from_rule, to_rule):
    """justify.ss: unify-rules (a vertical-bridge theme pattern, or #f)"""
    if tell(from_rule, "verbatim?") is not False or tell(to_rule, "verbatim?") is not False:
        return False

    def body(return_):
        from_rule_clauses = tell(from_rule, "get-rule-clauses")
        to_rule_clauses = tell(to_rule, "get-rule-clauses")

        def fail():
            return return_(False)
        unifying_concept_mappings = traverse_rule_clauses(
            from_rule_clauses, to_rule_clauses, fail, concept_mapping_proc)
        # chez: map's order of application (the procedure only reads)
        theme_pattern_entries = chez.map_(
            lambda cm: [tell(cm, "get-CM-type"), tell(cm, "get-label")],
            remove_whole_or_single_concept_mappings(unifying_concept_mappings))
        return ["vertical-bridge"] + theme_pattern_entries
    return sugar.continuation_point_star(body)


# The slippages returned by get-unifying-slippages depend on the direction
# of rule translation (top-to-bottom vs. bottom-to-top).  The function
# also assumes that the given rules can be unified.

def get_unifying_slippages(from_rule, to_rule):
    """justify.ss: get-unifying-slippages.  1.2: fail is #f, so rules that
    can't be unified make traverse-rule-clauses apply #f (a Chez error;
    a TypeError here)."""
    # 1.2: fail is #f (the function assumes the rules can be unified)
    return concept_mappings.remove_duplicate_CMs(
        remove_whole_or_single_concept_mappings(
            filter_meth(
                traverse_rule_clauses(
                    tell(from_rule, "get-rule-clauses"),
                    tell(to_rule, "get-rule-clauses"),
                    False, concept_mapping_proc),
                "slippage?")))


# single=>whole, whole=>single, single=>single, and whole=>whole
# concept-mappings are ignored, because they would lead to clamping
# the StrPos:diff theme, which would only confuse the program:

def remove_whole_or_single_concept_mappings(concept_mappings_):
    """justify.ss: remove-whole/single-concept-mappings.  1.2: only the first
    such concept mapping (select) is removed (with remq, every occurrence of
    that one object); #f when there is none, which remq removes nothing for."""
    def whole_or_single_p(cm):
        return (tell(cm, "CM-type?", slipnet.plato_string_position_category) is not False
                and (tell(cm, "get-descriptor1") is slipnet.plato_single
                     or tell(cm, "get-descriptor1") is slipnet.plato_whole))
    # 1.2: removes only the first match (select), not all of them
    return chez.remq(select(whole_or_single_p, concept_mappings_), concept_mappings_)


def traverse_rule_clauses(clauses1, clauses2, fail, proc):
    """justify.ss: traverse-rule-clauses.  Walks the two clause trees in
    parallel, calling (proc x1 x2 fail results) on each pair of non-symbol
    leaves; fail is called (and its value returned) on a mismatch."""
    def walk(x1, x2, results):
        if _null_p(x1) and _null_p(x2):
            return results
        if _null_p(x1) or _null_p(x2):
            return fail()
        if _list_p(x1) and _list_p(x2):
            # chez: (walk (1st x1) (1st x2) (walk (rest x1) (rest x2) results)):
            # the rests are walked first, so lists of different lengths fail
            # before any element is visited, and elements go last to first
            if len(x1) != len(x2):
                return fail()
            for i in range(len(x1) - 1, -1, -1):
                results = walk(x1[i], x2[i], results)
            return results
        if _list_p(x1) or _list_p(x2):
            return fail()
        if _symbol_p(x1) and _symbol_p(x2):
            return results if chez.eq_p(x1, x2) else fail()
        if _symbol_p(x1) and x1 == "string":
            return results if x2 is slipnet.plato_group else fail()
        if _symbol_p(x2) and x2 == "string":
            return results if x1 is slipnet.plato_group else fail()
        if _symbol_p(x1) or _symbol_p(x2):
            return fail()
        return proc(x1, x2, fail, results)
    return walk(clauses1, clauses2, [])


def verbatim_rule_clause_list_p(rc_list):
    """justify.ss: verbatim-rule-clause-list?"""
    return (len(rc_list) == 1
            and rules.verbatim_clause_p(first(rc_list)) is not False)


def compare_rule_clause_lists(rc_list1, rc_list2):
    """justify.ss: compare-rule-clause-lists ('() if equal, a list of the
    differing (n1 n2) leaf pairs, or #f if the structures differ).
    1.2: the verbatim test checks rc-list1 twice and never rc-list2."""
    # 1.2: tests rc-list1 twice (rc-list2 is never tested)
    if verbatim_rule_clause_list_p(rc_list1) and verbatim_rule_clause_list_p(rc_list1):
        return [] if chez.equal_p(rc_list1, rc_list2) else False

    def body(return_):
        return traverse_rule_clauses(rc_list1, rc_list2, lambda: return_(False),
                                     rule_clause_comparison_proc)
    return sugar.continuation_point_star(body)


def rule_clause_comparison_proc(n1, n2, fail, results):
    """justify.ss: rule-clause-comparison-proc"""
    if chez.eq_p(n1, n2):
        return results
    return [[n1, n2]] + results


def concept_mapping_proc(n1, n2, fail, results):
    """justify.ss: concept-mapping-proc"""
    if not chez.eq_p(n1, n2) and slipnet.slip_linked_p(n1, n2) is False:
        return fail()
    if not exists_p(tell(n1, "get-category")):
        return results
    # make-concept-mapping's arguments only read
    return [concept_mappings.make_concept_mapping(
        False, tell(n1, "get-category"), n1,
        False, tell(n1, "get-category"), n2)] + results


def get_vertical_theme_pattern_to_clamp(unifying_pattern):
    """justify.ss: get-vertical-theme-pattern-to-clamp.  1.2: prints "None."
    when the final pattern exists (it always does), and nothing else about it."""
    theme_type = first(unifying_pattern)
    pattern_entries = unifying_pattern[1:]
    get_probability = retention_probability(pattern_entries)
    # utilities.ss's filter: first to last, one prob? each
    final_pattern_entries = add_direction_entry(
        replace_bond_category_entry(
            filter_(compose(prob_p, get_probability), pattern_entries)))
    if _null_p(final_pattern_entries):
        final_theme_pattern = unifying_pattern
    else:
        final_theme_pattern = [theme_type] + final_pattern_entries
    vprintf("------------------------------------------------~n")
    vprintf("Rule unification possible with the unifying pattern:~n")
    if setup.p_verbose is not False:
        # vprintf evaluates its arguments only with %verbose% on
        vprintf("Retention probabilities: ~a~n",
                chez.map_(compose(round_to_100ths, retention_probability(pattern_entries)),
                          unifying_pattern[1:]))
    vprintf("Final pattern:~n")
    # 1.2: the test is the wrong way round (and the pattern is never printed)
    if exists_p(final_theme_pattern):
        vprintf("None.~n")
    vprintf("------------------------------------------------~n")
    return final_theme_pattern


def retention_probability(pattern_entries):
    """justify.ss: retention-probability (pattern-entries is unused)"""
    def get_probability(entry):
        if first(entry) is slipnet.plato_string_position_category:
            return 1
        return chez.mul(percent(cd(first(entry))),
                        percent(50 if second(entry) is slipnet.plato_identity else 100))
    return get_probability


# The following two heuristics improve the chances that a useful unifying theme
# pattern will get clamped:
#
# (1) If an entry exists in the pattern for StrPos:iden (or StrPos:opp), add an
#     entry for Dir:iden (or Dir:opp), since the idea of opposite direction is
#     closely related to opposite string-position.
#
# (2) If an entry for BondCtgy exists, replace it with the corresponding entry for
#     GroupCtgy, since bond-category themes don't play much of a role in the
#     creation of bridges by thematic-bridge-scout codelets.

def add_direction_entry(pattern_entries):
    """justify.ss: add-direction-entry"""
    # a two-binding let that only reads
    StrPos_entry = chez.assq(slipnet.plato_string_position_category, pattern_entries)
    Dir_entry = chez.assq(slipnet.plato_direction_category, pattern_entries)
    if (exists_p(StrPos_entry)
            and exists_p(second(StrPos_entry))
            and not exists_p(Dir_entry)):
        return [[slipnet.plato_direction_category, second(StrPos_entry)]] + pattern_entries
    return pattern_entries


def replace_bond_category_entry(pattern_entries):
    """justify.ss: replace-bond-category-entry"""
    # a two-binding let that only reads
    BondCtgy_entry = chez.assq(slipnet.plato_bond_category, pattern_entries)
    GroupCtgy_entry = chez.assq(slipnet.plato_group_category, pattern_entries)
    if exists_p(BondCtgy_entry):
        if exists_p(GroupCtgy_entry):
            return chez.remq(BondCtgy_entry, pattern_entries)
        return ([[slipnet.plato_group_category, second(BondCtgy_entry)]]
                + chez.remq(BondCtgy_entry, pattern_entries))
    return pattern_entries


def load():
    """justify.ss: the define-codelet-procedure* form (answer-justifier)."""
    sugar.define_codelet_procedure_star("answer-justifier", answer_justifier)
