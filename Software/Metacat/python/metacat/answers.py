"""answers.ss: reporting answers and snags, the commentary's explanations and
comparisons of answers, the answer-finder codelet, and rule translation.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from answers.ss, with
racket/engine/answers.rktl as a worked translation.

make-slippage-log's closure is the SlippageLog class (docs/python-translation-
plan.md, "Objects"); it delegates what it doesn't answer to base-object.  The
answer-finder codelet procedure is given to its codelet type by load()
(define-codelet-procedure* needs the type, which coderack.load makes).  The
closures that process-snag, translate-rule-clause, translate-object-description,
irrelevant-translated-string-group?, apply-to-change and apply-to-dimension
return are Python closures.

rules.ss loads before this file and is imported directly (punctuate, apply-rule,
ignore-snag, make-rule, verbatim-/extrinsic-/intrinsic-clause?, select-change).
Names from files translated later, or not translated yet, are read through the
package at call time: run.ss (update-everything, suspend, post-initial-codelets,
*temperature-clamped?*), trace.ss (*trace*, make-answer-event, make-snag-event,
entries, theme-pattern-entries-equal?), memory.ss (*memory*,
abstract-answer-description, abstract-snag-description), themes.ss
(*themespace*, and diff, its REPL abbreviation for the "different" relation,
which is #f: anomalies_and_quirks.md, "Rules and answers lean on later files and
on a REPL abbreviation"), justify.ss (remove-whole/single-concept-mappings,
compare-rule-clause-lists), bridge-graphics.ss (make-bridge-pexp) and
rule-graphics.ss (initialize-rule-graphics), the last two only with
%workspace-graphics% on, as the original gates them.  *comment-window*,
*workspace-window*, *temperature-window*, *temperature*, %justify-mode% and
%workspace-graphics% are setup's; the workspace strings and %built% are
workspace's; the four slippage colours are view_globals' (#f until the views
set them).  Every string the commentary receives, and every string a function
here returns, is a chez.String, so that write prints it as a string.  The
engine never imports tkinter.

Evaluation order (audited against Chez's): the draws are answer-finder's two
stochastic-if*s (each draws its coin first) and its stochastic-pick,
translate-rule-clause's (filter (lambda (d) (prob? 0.4)) ...) (utilities.ss's
filter tests first to last), and the slipnodes' apply-slippages (a prob? for
coattail slippages, plus the slippage log's state).  translate maps
translate-rule-clause over the clauses, which maps translate-object-description
over the object descriptions and the translator over the dimensions or changes:
all through chez.map_, since the procedures draw and fail can escape midway.
apply-to-change and apply-to-object-description build a (list ...) with two or
three apply-slippages calls; Chez's list evaluates its arguments left to right
(docs/python-translation-plan.md, "Evaluation order"), so they are sequential
statements in that order.  make-translated-rule-bridges's
(append (map ...) (map ...)) only makes bridges (no draw, no shared state), so
its order is not observable headless; it is kept left to right, as the Racket
port has it.  Every other multi-argument call and multi-binding let
(answer-finder's two mapping strengths, the add-comment calls, make-answer-event,
make-snag-event, make-concept-mapping, translate-object-description's enclosing
groups) only reads.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, sugar
from metacat import (bridges, concept_mappings, coderack, formulas, groups, rules, setup,
                     slipnet, view_globals, workspace, workspace_strings)
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.sugar import say, vprint, vprintf
from metacat.utilities import (all_but_last, all_exist_p, all_same_p, average,
                               base_object, capitalize_string, cd, compose, compress, cube,
                               exists_p, filter_, filter_meth, filter_out, first, flatmap,
                               flatten, fourth, fifth, group_p, intersect, intersect_pred,
                               last, map_compress, member_equal_p, member_p, one_minus,
                               partition, percent, prob_p, remove_duplicates, remove_elements,
                               remq_duplicates, remq_elements, round_, second, select,
                               select_extreme, select_meth, sixth, stochastic_pick, tell_all,
                               third, workspace_string_p)

String = chez.String


def _fmt(control, *args):
    """(format control arg ...) as a Scheme string."""
    return String(chez.format_(control, *args))


def _string_append(*strings):
    """(string-append s ...) as a Scheme string."""
    return String("".join(strings))


def _append_all(lists):
    """(apply append lists)"""
    out = []
    for part in lists:
        out.extend(part)
    return out


def _null_p(l):
    return len(l) == 0


def report_new_answer(answer_string, top_rule, bottom_rule, supporting_vertical_bridges,
                      supporting_groups, top_rule_ref_objects, bottom_rule_ref_objects,
                      slippage_log, unjustified_slippages):
    """answers.ss: report-new-answer"""
    _metacat.run.update_everything()
    if tell(_metacat.trace.g_trace, "within-clamp-period?") is not False:
        tell(_metacat.trace.g_trace, "undo-last-clamp")
    # the arguments only read
    answer_event = _metacat.trace.make_answer_event(
        workspace.g_initial_string, workspace.g_modified_string, workspace.g_target_string,
        answer_string, top_rule, bottom_rule, supporting_vertical_bridges, supporting_groups,
        top_rule_ref_objects, bottom_rule_ref_objects, slippage_log, unjustified_slippages)
    tell(_metacat.trace.g_trace, "add-event", answer_event)
    # the two add-comment arguments only read
    if not _null_p(unjustified_slippages):
        tell(setup.g_comment_window, "add-comment",
             [String("Okay, I'm stumped.  This answer makes no sense to me."),
              _fmt("  I see no way to make the necessary ~a slippage~a here.",
                   rules.punctuate(tell_all(unjustified_slippages, "english-name")),
                   String("") if len(unjustified_slippages) == 1 else String("s"))],
             [_fmt("Run terminated.  Unable to make the necessary ~a slippage~a.",
                   rules.punctuate(tell_all(unjustified_slippages, "english-name")),
                   String("") if len(unjustified_slippages) == 1 else String("s"))])
    elif setup.p_justify_mode is not False:
        quality = tell(answer_event, "get-quality")
        quality_phrase = answer_quality_phrase(quality)
        if quality < 60:
            second_part = _fmt(", but it's a ~a answer, in my opinion.", quality_phrase)
        else:
            second_part = _fmt(".  I think it's a ~a answer~a",
                               quality_phrase,
                               String("!") if (quality < 30 or quality >= 85) else String("."))
        tell(setup.g_comment_window, "add-comment",
             [String("Aha!  I see why this answer makes sense"), second_part],
             [_fmt("Successfully justified answer.  Answer quality = ~a.",
                   tell(answer_event, "get-quality"))])
    else:
        first_part = _fmt("The answer \"~a\" ~aoccurs to me",
                          tell(answer_string, "print-name"),
                          (String("also ")
                           if tell(_metacat.trace.g_trace, "get-num-of-events", "answer") > 1
                           else String("")))
        quality = tell(answer_event, "get-quality")
        quality_phrase = answer_quality_phrase(quality)
        punctuation = String("!") if (quality < 30 or quality >= 85) else String(".")
        if quality < 60:
            second_part = _fmt(", but that's ~a~a", quality_phrase, punctuation)
        else:
            second_part = _fmt(".  I think this answer is ~a~a", quality_phrase, punctuation)
        tell(setup.g_comment_window, "add-comment",
             [first_part, second_part],
             [_fmt("Found the answer \"~a\".  Answer quality = ~a.",
                   tell(answer_string, "print-name"),
                   tell(answer_event, "get-quality"))])
    if setup.p_workspace_graphics is not False:
        tell(setup.g_workspace_window, "draw-current-answer")
    # Build an abstract characterization of the answer from the information
    # in the workspace and the trace, and store it in memory:
    _metacat.memory.abstract_answer_description(answer_event)
    _metacat.run.suspend()
    if setup.p_workspace_graphics is not False:
        return tell(setup.g_workspace_window, "erase-current-answer")
    return None


def give_up():
    """answers.ss: give-up"""
    _metacat.run.update_everything()
    tell(setup.g_comment_window, "add-comment",
         [String("Excuse me -- I think I'll go get some more punch.")],
         [String("Run terminated.")])
    return _metacat.run.suspend()


def answer_quality_phrase(quality):
    """answers.ss: answer-quality-phrase"""
    if quality < 50:
        return String("really terrible")
    if quality < 60:
        return String("pretty bad")
    if quality < 70:
        return String("pretty dumb")
    if quality < 75:
        return String("pretty mediocre")
    if quality < 80:
        return String("halfway decent")
    if quality < 85:
        return String("pretty good")
    if quality < 90:
        return String("very good")
    return String("great")


def most_recent_group_and_concept_mapping_events():
    """answers.ss: most-recent-group-and-concept-mapping-events"""
    group_and_concept_mapping_events = filter_(
        lambda event: (member_p(tell(event, "get-type"), ["concept-mapping", "group"])
                       and tell(event, "relevant-for-answer-description?")),
        tell(_metacat.trace.g_trace, "get-all-events"))
    equivalent_event_clusters = partition(
        lambda ev1, ev2: tell(ev1, "equal?", ev2),
        group_and_concept_mapping_events)
    # chez: map's order of application (the procedure only reads)
    return chez.map_(pick_most_recent_event, equivalent_event_clusters)


def pick_most_recent_event(events):
    """answers.ss: pick-most-recent-event"""
    return select_extreme(chez.min_, lambda event: tell(event, "get-age"), events)


def in_patterns_p(dimension, pattern_or_s):
    """answers.ss: in-patterns?"""
    return exists_p(get_entry(dimension, pattern_or_s))


def get_entry(dimension, pattern_or_s):
    """answers.ss: get-entry"""
    if _null_p(pattern_or_s):
        return False
    # symbol? only tells a symbol (a theme type) from a list (a pattern)
    if isinstance(first(pattern_or_s), str):
        return chez.assq(dimension, _metacat.trace.entries(pattern_or_s))
    return chez.assq(dimension, flatmap(_metacat.trace.entries, pattern_or_s))


def whole_string_identity_concept_mapping_p(cm_type, initial_group, target_group):
    """answers.ss: whole-string-identity-concept-mapping?"""
    if not (exists_p(initial_group) and exists_p(target_group)):
        return False
    spanning_bridge = bridges.get_bridge_between("vertical", initial_group, target_group)
    if not exists_p(spanning_bridge):
        return False
    cm = tell(spanning_bridge, "get-concept-mapping", cm_type)
    if not exists_p(cm):
        return False
    return tell(cm, "identity?")


def abstract_answer_description_theme_pattern(important_events):
    """answers.ss: abstract-answer-description-theme-pattern"""
    dominant_vertical_theme_pattern = tell(_metacat.themes.g_themespace,
                                           "get-dominant-theme-pattern", "vertical-bridge")
    whole_initial_string_event = select(
        lambda event: (tell(event, "type?", "group") is not False
                       and tell(event, "string-type?", "initial")),
        important_events)
    whole_target_string_event = select(
        lambda event: (tell(event, "type?", "group") is not False
                       and tell(event, "string-type?", "target")),
        important_events)
    if exists_p(whole_initial_string_event):
        initial_spanning_group = tell(whole_initial_string_event, "get-group")
    else:
        initial_spanning_group = False
    if exists_p(whole_target_string_event):
        target_spanning_group = tell(whole_target_string_event, "get-group")
    else:
        target_spanning_group = False
    important_cm_events = filter_meth(important_events, "type?", "concept-mapping")
    important_cm_patterns = tell_all(important_cm_events, "get-theme-pattern")

    def entry_for(cm_type):
        # Check for non-identity themes in the cm-patterns first:
        if in_patterns_p(cm_type, important_cm_patterns):
            return get_entry(cm_type, important_cm_patterns)
        if cm_type is slipnet.plato_string_position_category:
            if in_patterns_p(slipnet.plato_direction_category, important_cm_patterns):
                # StringPos and Direction themes should agree if possible:
                return [slipnet.plato_string_position_category,
                        second(get_entry(slipnet.plato_direction_category,
                                         important_cm_patterns))]
            # Otherwise check for a dominant StringPos theme:
            dominant_StringPos_theme = chez.assq(
                slipnet.plato_string_position_category,
                _metacat.trace.entries(dominant_vertical_theme_pattern))
            if exists_p(dominant_StringPos_theme):
                return dominant_StringPos_theme
            # Always include a string-position theme no matter what:
            return [slipnet.plato_string_position_category, slipnet.plato_identity]
        # Don't include bond-facet identity themes:
        if cm_type is slipnet.plato_bond_facet:
            return False
        # Check for whole-string identity themes:
        if whole_string_identity_concept_mapping_p(
                cm_type, initial_spanning_group, target_spanning_group) is not False:
            return [cm_type, slipnet.plato_identity]
        return False
    # Only themes of the following types contribute to
    # answer-description thematic characterizations:
    abstracted_theme_pattern_entries = map_compress(
        entry_for,
        [slipnet.plato_alphabetic_position_category,
         slipnet.plato_string_position_category,
         slipnet.plato_direction_category,
         slipnet.plato_group_category,
         slipnet.plato_bond_facet])
    return ["vertical-bridge"] + abstracted_theme_pattern_entries


def get_theme_supporting_concept_mappings(theme_pattern, theme_supporting_bridges):
    """answers.ss: get-theme-supporting-concept-mappings"""
    all_concept_mappings = _metacat.justify.remove_whole_or_single_concept_mappings(
        _append_all(tell_all(theme_supporting_bridges, "get-all-concept-mappings")))

    def supports(cm):
        entry = [tell(cm, "get-CM-type"), tell(cm, "get-label")]
        return (member_equal_p(entry, _metacat.trace.entries(theme_pattern))
                or (first(entry) is slipnet.plato_bond_category
                    and member_equal_p([slipnet.plato_group_category, second(entry)],
                                       _metacat.trace.entries(theme_pattern))))
    return filter_(supports, all_concept_mappings)


def get_unjustified_theme_pattern(unjustified_slippages):
    """answers.ss: get-unjustified-theme-pattern"""
    # chez: map's order of application (the procedure only reads)
    pattern_entries = chez.map_(lambda s: [tell(s, "get-CM-type"), tell(s, "get-label")],
                                unjustified_slippages)
    if not exists_p(chez.assq(slipnet.plato_bond_facet, pattern_entries)):
        return ["vertical-bridge"] + pattern_entries
    # Since a BondFacet theme implies the existence of groups, add
    # accompanying (unjustified) group-category and direction themes, if
    # they don't already exist.  Since the unjustified BondFacet theme
    # by itself does not imply a particular group-category or direction
    # relation (i.e., identity vs. opposite), identity themes are added
    # in the default case:
    if exists_p(chez.assq(slipnet.plato_group_category, pattern_entries)):
        GroupCtgy_entry = chez.assq(slipnet.plato_group_category, pattern_entries)
    else:
        GroupCtgy_entry = [slipnet.plato_group_category, slipnet.plato_identity]
    if exists_p(chez.assq(slipnet.plato_direction_category, pattern_entries)):
        Direction_entry = chez.assq(slipnet.plato_direction_category, pattern_entries)
    else:
        Direction_entry = [slipnet.plato_direction_category, slipnet.plato_identity]
    return ["vertical-bridge"] + remove_duplicates(
        [GroupCtgy_entry, Direction_entry] + pattern_entries)


def punctuate_with_commas(conj, l):
    """answers.ss: punctuate-with-commas"""
    if _null_p(l):
        return String("")
    if len(l) == 1:
        return first(l)
    if len(l) == 2:
        return _fmt("~a, ~a ~a", first(l), conj, second(l))
    return _string_append(*([chez.format_("~a, ", x) for x in all_but_last(1, l)]
                            + [chez.format_("~a ~a", conj, last(l))]))


def intersect_themes(themes1, themes2):
    """answers.ss: intersect-themes"""
    return intersect_pred(_metacat.trace.theme_pattern_entries_equal_p, themes1, themes2)


def get_snag_justified_themes(answer):
    """answers.ss: get-snag-justified-themes"""
    unjustified_themes = tell(answer, "get-unjustified-themes")
    all_themes = tell(answer, "get-themes") + unjustified_themes
    snag_description = tell(_metacat.memory.g_memory, "get-equivalent-snag", answer)
    if exists_p(snag_description):
        snag_avoiding_themes = remove_elements(tell(snag_description, "get-themes"), all_themes)
    else:
        snag_avoiding_themes = []
    return intersect_themes(snag_avoiding_themes, unjustified_themes)


def get_snag_explanation(answer):
    """answers.ss: get-snag-explanation"""
    snag = tell(_metacat.memory.g_memory, "get-equivalent-snag", answer)
    if exists_p(snag):
        return tell(snag, "get-explanation")
    return String("")


def explain(answer):
    """answers.ss: explain"""
    initial = tell(answer, "get-initial-print-name")
    target = tell(answer, "get-target-print-name")
    themes = tell(answer, "get-themes")
    snag_justified_themes = get_snag_justified_themes(answer)
    snag_explanation = get_snag_explanation(answer)
    unjustified_themes = remove_elements(snag_justified_themes,
                                         tell(answer, "get-unjustified-themes"))
    explanation = _fmt("This answer is based ~a.",
                       theme_phrases("on ", "and", "ing",
                                     themes, snag_justified_themes, unjustified_themes,
                                     initial, target, snag_explanation, True, False, False))
    return tell(setup.g_comment_window, "add-comment",
                [explanation,
                 _fmt("  Personally, I think this answer is ~a.",
                      answer_quality_phrase(tell(answer, "get-quality")))],
                [explanation,
                 _fmt("  Answer quality = ~a.", tell(answer, "get-quality"))])


def theme_phrases(prep, conj, verb_ending, themes, snag_justified_themes, unjustified_themes,
                  initial, target, snag_explanation, add_caveats_p, the_strings_p, two_strings):
    """answers.ss: theme-phrases"""
    all_themes = themes + unjustified_themes
    StringPos_theme = chez.assq(slipnet.plato_string_position_category, all_themes)
    # Direction-theme should always be the same as StringPos-theme:
    AlphaPos_theme = chez.assq(slipnet.plato_alphabetic_position_category, all_themes)
    GroupCtgy_theme = chez.assq(slipnet.plato_group_category, all_themes)
    BondFacet_theme = chez.assq(slipnet.plato_bond_facet, all_themes)
    if exists_p(two_strings):
        strings = _fmt("two strings ~a", two_strings)
    else:
        strings = _fmt("~a and ~a", initial, target)
    diff = _metacat.themes.diff

    def caveat(theme):
        if add_caveats_p is False:
            return String("")
        if member_equal_p(theme, unjustified_themes):
            return String(" (although there is no good reason for doing so)")
        if member_equal_p(theme, snag_justified_themes):
            return _string_append(
                " (which avoids a snag that would otherwise ",
                chez.format_("arise from the fact that ~a)", snag_explanation))
        return String("")

    # The conds below with no matching clause are void, which ~a prints as #<void>.
    phrases = []
    if exists_p(StringPos_theme):
        if (not exists_p(GroupCtgy_theme)
                or member_equal_p(GroupCtgy_theme, unjustified_themes)):
            group_part = String("")
        elif second(GroupCtgy_theme) is slipnet.plato_identity:
            group_part = String("groups of the same type ")
        elif second(GroupCtgy_theme) is slipnet.plato_opposite:
            group_part = String("symmetric predecessor and successor groups ")
        elif second(GroupCtgy_theme) is diff:
            group_part = String("different kinds of groups ")
        else:
            group_part = None
        if second(StringPos_theme) is slipnet.plato_identity:
            direction_part = String("going in the same direction")
        elif second(StringPos_theme) is slipnet.plato_opposite:
            direction_part = String("going in opposite directions")
        elif second(StringPos_theme) is diff:
            direction_part = String("neither going in the same nor in opposite directions")
        else:
            direction_part = None
        phrases.append(_fmt("~asee~a ~a as ~a~a~a",
                            prep, verb_ending, strings, group_part, direction_part,
                            caveat(StringPos_theme)))
    else:
        phrases.append(False)
    if (exists_p(GroupCtgy_theme)
            and not exists_p(StringPos_theme)
            and not member_equal_p(GroupCtgy_theme, unjustified_themes)):
        if second(GroupCtgy_theme) is slipnet.plato_identity:
            group_part = String("groups of the same type")
        elif second(GroupCtgy_theme) is slipnet.plato_opposite:
            group_part = String("symmetric predecessor and successor groups")
        elif second(GroupCtgy_theme) is diff:
            group_part = String("different kinds of groups")
        else:
            group_part = None
        if exists_p(BondFacet_theme):
            facet_part = _string_append(" by viewing one string in terms of letters ",
                                        "and the other in terms of numbers")
        else:
            facet_part = String("")
        phrases.append(_fmt("~asee~a ~a as ~a~a~a",
                            prep, verb_ending, strings, group_part, facet_part,
                            caveat(GroupCtgy_theme)))
    else:
        phrases.append(False)
    if (exists_p(BondFacet_theme)
            and (exists_p(StringPos_theme)
                 or not exists_p(GroupCtgy_theme)
                 or member_equal_p(GroupCtgy_theme, unjustified_themes))):
        phrases.append(_fmt("~aview~a one of ~a in terms of letters ~a",
                            prep, verb_ending,
                            (String("the strings")
                             if (exists_p(StringPos_theme) or the_strings_p is not False)
                             else strings),
                            _fmt("and the other in terms of numbers~a",
                                 caveat(BondFacet_theme))))
    else:
        phrases.append(False)
    if exists_p(AlphaPos_theme):
        if second(AlphaPos_theme) is slipnet.plato_identity:
            relation_part = String("sameness")
        elif second(AlphaPos_theme) is slipnet.plato_opposite:
            relation_part = String("symmetry")
        else:
            relation_part = None
        phrases.append(_fmt("~asee~a alphabetic-position ~a between ~a~a",
                            prep, verb_ending, relation_part,
                            (String("the strings")
                             if (exists_p(StringPos_theme) or the_strings_p is not False)
                             else strings),
                            caveat(AlphaPos_theme)))
    else:
        phrases.append(False)
    return punctuate_with_commas(conj, compress(phrases))


def compare_answers(answer1, answer2):
    """answers.ss: compare-answers"""
    comparison = [get_answer_comparison_text(answer1, answer2)]
    return tell(setup.g_comment_window, "add-comment", comparison, comparison)


def get_answer_comparison_text(self, other):
    """answers.ss: get-answer-comparison-text"""
    # The let* only reads; Python's order is the let*'s.
    initial1 = tell(self, "get-initial-print-name")
    modified1 = tell(self, "get-modified-print-name")
    target1 = tell(self, "get-target-print-name")
    answer1 = tell(self, "get-answer-print-name")
    initial2 = tell(other, "get-initial-print-name")
    modified2 = tell(other, "get-modified-print-name")
    target2 = tell(other, "get-target-print-name")
    answer2 = tell(other, "get-answer-print-name")
    problem1 = _fmt("\"~a -> ~a, ~a -> ?\"", initial1, modified1, target1)
    problem2 = _fmt("\"~a -> ~a, ~a -> ?\"", initial2, modified2, target2)
    change1 = _fmt("the change from ~a to ~a", initial1, modified1)
    change2 = _fmt("the change from ~a to ~a", initial2, modified2)
    full_answer1 = _fmt("~a to the problem ~a", answer1, problem1)
    full_answer2 = _fmt("~a to the problem ~a", answer2, problem2)
    answer1_phrase = answer1 if problem1 == problem2 else full_answer1
    answer2_phrase = answer2 if problem1 == problem2 else full_answer2
    both_answers_phrase = _fmt("the answer ~a and the answer ~a", answer1_phrase, full_answer2)
    # These are all the ideas, justified or not, that underlie answer1:
    all_themes1 = tell(self, "get-themes") + tell(self, "get-unjustified-themes")
    # These are all the ideas, justified or not, that underlie answer2:
    all_themes2 = tell(other, "get-themes") + tell(other, "get-unjustified-themes")
    snag_justified_themes1 = get_snag_justified_themes(self)
    snag_justified_themes2 = get_snag_justified_themes(other)
    unjustified_themes1 = remove_elements(snag_justified_themes1,
                                          tell(self, "get-unjustified-themes"))
    unjustified_themes2 = remove_elements(snag_justified_themes2,
                                          tell(other, "get-unjustified-themes"))
    all_unjustified_themes = remove_duplicates(unjustified_themes1 + unjustified_themes2)
    snag_explanation1 = get_snag_explanation(self)
    snag_explanation2 = get_snag_explanation(other)
    common_themes = intersect_themes(all_themes1, all_themes2)
    # These are all the _types_ of ideas, justified or not, that underlie
    # both answers:
    common_dimensions = [first(theme) for theme in common_themes]
    # These are all the _types_ of ideas, justified or not, that are common
    # to both answers, but about which the answers "disagree".  Example: one
    # answer based on StringPos:iden and the other based on StringPos:opp
    differing_dimensions = remq_elements(
        common_dimensions,
        intersect([first(theme) for theme in all_themes1],
                  [first(theme) for theme in all_themes2]))
    # These ideas, justified or not, underlie answer1 only:
    answer1_only_themes = remove_elements(common_themes, all_themes1)
    # These ideas, justified or not, underlie answer2 only:
    answer2_only_themes = remove_elements(common_themes, all_themes2)
    # These are the ideas, justified or not, that answer1 "disagrees" with
    # answer2 about.  Example: if answer1 has StringPos:iden and answer2
    # has StringPos:opp, StringPos:iden is one of answer1's differing themes:
    differing_themes1 = filter_(lambda theme: member_p(first(theme), differing_dimensions),
                                all_themes1)
    # These are the ideas, justified or not, that answer2 "disagrees" with
    # answer1 about.  Example: if answer1 has StringPos:iden and answer2
    # has StringPos:opp, StringPos:opp is one of answer2's differing themes:
    differing_themes2 = filter_(lambda theme: member_p(first(theme), differing_dimensions),
                                all_themes2)
    # These ideas, justified or not, underlie answer1 but are completely
    # absent from answer2.  The answers do not even "disagree" about
    # these ideas, because answer2 doesn't have an idea of the same type:
    unique_themes1 = remove_elements(differing_themes1, answer1_only_themes)
    # These ideas, justified or not, underlie answer2 but are completely
    # absent from answer1.  The answers do not even "disagree" about
    # these ideas, because answer1 doesn't have an idea of the same type:
    unique_themes2 = remove_elements(differing_themes2, answer2_only_themes)
    # These unjustified ideas are common to both answers:
    common_unjustified_themes = intersect_themes(unjustified_themes1, unjustified_themes2)
    # These unjustified ideas underlie answer1 only:
    answer1_only_unjustified_themes = remove_elements(common_unjustified_themes,
                                                      unjustified_themes1)
    # These unjustified ideas underlie answer2 only:
    answer2_only_unjustified_themes = remove_elements(common_unjustified_themes,
                                                      unjustified_themes2)
    # These ideas are common to both answers, but are unjustified
    # only in the case of answer1:
    common_answer1_only_unjustified_themes = remove_elements(differing_themes1,
                                                             answer1_only_unjustified_themes)
    # These ideas are common to both answers, but are unjustified
    # only in the case of answer2:
    common_answer2_only_unjustified_themes = remove_elements(differing_themes2,
                                                             answer2_only_unjustified_themes)
    # This is true if there exist ideas that are common to both answers,
    # but which are justified for only one of the answers:
    justification_differences_p = (not _null_p(common_answer1_only_unjustified_themes)
                                   or not _null_p(common_answer2_only_unjustified_themes))
    common_unjustified1_snag_justified2_themes = intersect_themes(
        common_answer1_only_unjustified_themes, snag_justified_themes2)
    if _null_p(common_unjustified1_snag_justified2_themes):
        snag_part1 = String("")
    else:
        snag_part1 = _fmt(
            ", where ~a avoids a snag that would otherwise ~a",
            theme_phrases("", "and", "ing",
                          common_unjustified1_snag_justified2_themes, [],
                          common_unjustified1_snag_justified2_themes,
                          initial2, target2, "", False, True, False),
            _fmt("arise from the fact that ~a", snag_explanation2))
    no_justification1 = _fmt(
        "in the former case, there is no compelling ~a",
        _fmt("reason ~a, unlike in the latter case with ~a and ~a~a",
             theme_phrases("to ", "or", "",
                           common_answer1_only_unjustified_themes,
                           snag_justified_themes1, unjustified_themes1,
                           initial1, target1, "", False, True, False),
             initial2, target2, snag_part1))
    common_snag_justified1_unjustified2_themes = intersect_themes(
        common_answer2_only_unjustified_themes, snag_justified_themes1)
    if _null_p(common_snag_justified1_unjustified2_themes):
        snag_part2 = String("")
    else:
        snag_part2 = _fmt(
            ", where ~a avoids a snag that would otherwise ~a",
            theme_phrases("", "and", "ing",
                          common_snag_justified1_unjustified2_themes, [],
                          common_snag_justified1_unjustified2_themes,
                          initial1, target1, "", False, True, False),
            _fmt("arise from the fact that ~a", snag_explanation1))
    no_justification2 = _fmt(
        "in the latter case, there is no compelling ~a",
        _fmt("reason ~a, unlike in the former case with ~a and ~a~a",
             theme_phrases("to ", "or", "",
                           common_answer2_only_unjustified_themes,
                           snag_justified_themes2, unjustified_themes2,
                           initial2, target2, "", False, True, False),
             initial1, target1, snag_part2))
    num_theme_differences = (len(differing_dimensions)
                             + len(unique_themes1)
                             + len(unique_themes2))
    theme_differences_p = not (num_theme_differences == 0)
    # Compare the rules
    rule1_clauses = tell(self, "get-top-rule-clauses")
    rule2_clauses = tell(other, "get-top-rule-clauses")
    rule1_abstractness = tell(self, "get-top-rule-abstractness")
    rule2_abstractness = tell(other, "get-top-rule-abstractness")
    # <rule-differences> ::= #f | ((<slipnode1> <slipnode2>) ...)
    rule_differences = _metacat.justify.compare_rule_clause_lists(rule1_clauses, rule2_clauses)
    if exists_p(rule_differences):
        num_rule_differences = len(filter_out(
            lambda nodes: cd(first(nodes)) == cd(second(nodes)),
            rule_differences))
    else:
        num_rule_differences = -1
    rule_differences_p = not (num_rule_differences == 0)
    if not exists_p(rule_differences):
        rule_difference_phrase = String("in a completely different way")
    else:
        difference = chez.sub(rule1_abstractness, rule2_abstractness)
        if difference > 0:
            rule_difference_phrase = String("in a more abstract way")
        elif difference < 0:
            rule_difference_phrase = String("in a more literal way")
        else:
            rule_difference_phrase = String("somewhat differently")
    # In the following special case, answer2 gets mentioned before answer1
    # in the output, so if both answers happen to be the same string, we
    # need to refer to answer2 as the "first" answer instead of the "second"
    # answer.  In all other cases answer1 gets mentioned first:
    answer2_mentioned_first_p = theme_differences_p and _null_p(answer1_only_themes)
    answer1_order = String("second") if answer2_mentioned_first_p else String("first")
    answer2_order = String("first") if answer2_mentioned_first_p else String("second")
    # Example answer1-ref strings: "dyz", "the first dyz", "the second dyz"
    answer1_ref = _fmt("the ~a ~a", answer1_order, answer1) if answer1 == answer2 else answer1
    answer2_ref = _fmt("the ~a ~a", answer2_order, answer2) if answer1 == answer2 else answer2
    rule_explanation_template = _string_append(
        "~a difference between ~a",
        chez.format_(" is that ~a is viewed ~a for ~a than~a in ~a.",
                     change1,
                     rule_difference_phrase,
                     (_fmt("the ~a answer", answer1_order) if answer1 == answer2
                      else _fmt("the answer ~a", answer1)),
                     String(" it is") if change1 == change2 else _fmt(" ~a", change2),
                     (_fmt("the ~a answer's case", answer2_order) if answer1 == answer2
                      else _fmt("the case of ~a", answer2))))
    quality1 = tell(self, "get-quality")
    quality2 = tell(other, "get-quality")
    average_theme_abstractness1 = average_theme_abstractness(self)
    average_theme_abstractness2 = average_theme_abstractness(other)
    answer1_incoherent_p = answer_incoherent_p(self)
    answer2_incoherent_p = answer_incoherent_p(other)

    def similarities(caveats_p):
        if initial1 == initial2 and target1 == target2:
            two_strings = _fmt("(~a and ~a in both cases)", initial1, target1)
        else:
            two_strings = _fmt("(~a and ~a in one case and ~a and ~a in the other)",
                               initial1, target1, initial2, target2)
        return _fmt("rely ~a",
                    theme_phrases("on ", "and", "ing",
                                  common_themes, [], all_unjustified_themes, False, False, "",
                                  caveats_p, False, two_strings))

    # The first part: the themes, the rules and the justifications
    if not theme_differences_p:
        # No theme differences
        if not rule_differences_p and not justification_differences_p:
            # No differences at all
            if change1 == change2:
                viewed = _fmt("~a is viewed in essentially the same way in both cases",
                              change1)
            else:
                viewed = _fmt("~a is viewed in essentially the same way as ~a",
                              change1, change2)
            part1 = _fmt("The answer ~a is essentially the same as the answer ~a.~a",
                         answer1_phrase, full_answer2,
                         _fmt("  Both answers ~a.  Furthermore, ~a.",
                              similarities(True), viewed))
        elif not justification_differences_p:
            # Only rule differences
            part1 = _string_append(
                chez.format_(rule_explanation_template, "The only essential",
                             both_answers_phrase),
                chez.format_("  Both answers ~a.", similarities(True)))
        # Justification differences
        elif _null_p(common_answer1_only_unjustified_themes):
            part1 = _fmt("The answer ~a is similar to the answer ~a, since both ~a.~a",
                         answer1_phrase, full_answer2, similarities(False),
                         _fmt("  However, ~a.", no_justification2))
        elif _null_p(common_answer2_only_unjustified_themes):
            part1 = _fmt("The answer ~a is similar to the answer ~a, since both ~a.~a",
                         answer1_phrase, full_answer2, similarities(False),
                         _fmt("  However, ~a.", no_justification1))
        else:
            part1 = _fmt("The answer ~a is similar to the answer ~a, since both ~a.~a",
                         answer1_phrase, full_answer2, similarities(False),
                         _fmt("  However, ~a.  Likewise, ~a.",
                              no_justification1, no_justification2))
    # Theme differences
    elif _null_p(answer1_only_themes):
        # In this case we are mentioning answer2 first:
        part1 = _fmt("The answer ~a is based ~a~a.",
                     full_answer2,
                     String("") if _null_p(common_themes) else String("in part "),
                     theme_phrases("on ", "and", "ing",
                                   answer2_only_themes, snag_justified_themes2,
                                   unjustified_themes2, initial2, target2, snag_explanation2,
                                   True, False, False))
    elif _null_p(answer2_only_themes):
        part1 = _fmt("The answer ~a is based ~a~a.",
                     full_answer1,
                     String("") if _null_p(common_themes) else String("in part "),
                     theme_phrases("on ", "and", "ing",
                                   answer1_only_themes, snag_justified_themes1,
                                   unjustified_themes1, initial1, target1, snag_explanation1,
                                   True, False, False))
    else:
        part1 = _fmt("The answer ~a is based ~a~a, while the answer ~a is based ~a~a.",
                     full_answer1,
                     String("") if _null_p(common_themes) else String("in part "),
                     theme_phrases("on ", "and", "ing",
                                   answer1_only_themes, snag_justified_themes1,
                                   unjustified_themes1, initial1, target1, snag_explanation1,
                                   True, False, False),
                     answer2_phrase,
                     String("") if _null_p(common_themes) else String("in part "),
                     theme_phrases("on ", "and", "ing",
                                   answer2_only_themes, snag_justified_themes2,
                                   unjustified_themes2, initial2, target2, snag_explanation2,
                                   True, False, False))
    # The ideas unique to answer1
    if not _null_p(unique_themes1):
        part2 = _fmt("~a ~a, the idea ~a does not arise.",
                     (String("  In contrast, in")
                      if (_null_p(answer1_only_themes) or _null_p(answer2_only_themes))
                      else String("  In")),
                     # Here we haven't explicitly mentioned the second answer yet:
                     (_fmt("the case of the answer ~a", answer2_phrase)
                      if _null_p(answer2_only_themes)
                      else _fmt("~a's case", answer2_ref)),
                     theme_phrases("of ", "and", "ing",
                                   unique_themes1, snag_justified_themes1, unjustified_themes1,
                                   initial2, target2, "", False, False, False))
    else:
        part2 = String("")
    # The ideas unique to answer2
    if not _null_p(unique_themes2):
        part3 = _fmt("~a ~a, the idea ~a does not arise.",
                     (String("  In contrast, in")
                      if ((_null_p(answer1_only_themes) or _null_p(answer2_only_themes))
                          and _null_p(unique_themes1))
                      else String("  In")),
                     # Here we haven't explicitly mentioned the first answer yet:
                     (_fmt("the case of the answer ~a", answer1_phrase)
                      if _null_p(answer1_only_themes)
                      else _fmt("~a's case", answer1_ref)),
                     theme_phrases("of ", "and", "ing",
                                   unique_themes2, snag_justified_themes2, unjustified_themes2,
                                   initial1, target1, "", False, False, False))
    else:
        part3 = String("")
    # Another key difference: the rules
    if (not rule_differences_p
            or (not theme_differences_p and not justification_differences_p)):
        part4 = String("")
    else:
        part4 = _fmt(rule_explanation_template,
                     "  Another key",
                     (_fmt("the two ~a answers", answer1) if answer1 == answer2
                      else String("the answers")))
    # Incoherence
    if answer1_incoherent_p:
        incoherent1 = _string_append(
            chez.format_("  The answer ~a, however, seems incoherent to me, since it involves ",
                         answer1),
            chez.format_("seeing~a between ~a and ~a (~a), while ",
                         (" abstract similarities" if len(all_themes1) > 1
                          else " an abstract similarity"),
                         initial1, target1,
                         theme_phrases("", "and", "ing",
                                       all_themes1, snag_justified_themes1, unjustified_themes1,
                                       initial1, target1, "", False, False, False)),
            chez.format_("at the same time viewing ~a in a more literal way.", change1))
    else:
        incoherent1 = String("")
    if answer2_incoherent_p:
        incoherent2 = _string_append(
            chez.format_("  The answer ~a~a seems incoherent~a, since it involves ",
                         answer2,
                         " also" if answer1_incoherent_p else ", however,",
                         "" if answer1_incoherent_p else " to me"),
            chez.format_("seeing~a between ~a and ~a (~a), while ",
                         (" abstract similarities" if len(all_themes2) > 1
                          else " an abstract similarity"),
                         initial2, target2,
                         theme_phrases("", "and", "ing",
                                       all_themes2, snag_justified_themes2, unjustified_themes2,
                                       initial2, target2, "", False, False, False)),
            chez.format_("at the same time viewing ~a in a more literal way.", change2))
    else:
        incoherent2 = String("")
    part5 = _string_append(incoherent1, incoherent2)

    # The verdict
    def answer1_better(intro, reason):
        return _fmt("~aI'd say ~a is the better answer~a.", intro, answer1_ref, reason)

    def answer2_better(intro, reason):
        return _fmt("~aI'd say ~a is the better answer~a.", intro, answer2_ref, reason)

    def neither_better(intro):
        quality1_phrase = answer_quality_phrase(quality1)
        quality2_phrase = answer_quality_phrase(quality2)
        if quality1_phrase == quality2_phrase:
            return _fmt("~aI'd say they're both ~a answers.", intro, quality1_phrase)
        return _fmt("~aI'd say ~a is ~a and ~a is ~a.",
                    intro, answer1_ref, quality1_phrase, answer2_ref, quality2_phrase)

    if answer1_incoherent_p and answer2_incoherent_p:
        if ((average_theme_abstractness1 < average_theme_abstractness2
             and len(all_themes1) <= len(all_themes2))
                or (len(all_themes1) < len(all_themes2)
                    and average_theme_abstractness1 <= average_theme_abstractness2)):
            part6 = answer1_better(
                "  Overall, though, ",
                _fmt(", because it doesn't seem quite as incoherent as ~a", answer2_ref))
        elif ((average_theme_abstractness2 < average_theme_abstractness1
               and len(all_themes2) <= len(all_themes1))
              or (len(all_themes2) < len(all_themes1)
                  and average_theme_abstractness2 <= average_theme_abstractness1)):
            part6 = answer2_better(
                "  Overall, though, ",
                _fmt(", because it doesn't seem quite as incoherent as ~a", answer1_ref))
        else:
            part6 = neither_better("  All in all, ")
    elif not answer1_incoherent_p and answer2_incoherent_p:
        part6 = answer1_better("  All in all, ", ", since it is more coherent")
    elif not answer2_incoherent_p and answer1_incoherent_p:
        part6 = answer2_better("  All in all, ", ", since it is more coherent")
    elif len(unjustified_themes1) < len(unjustified_themes2):
        part6 = answer1_better("  All in all, ",
                               (", since it involves no unjustified ideas"
                                if len(unjustified_themes1) == 0
                                else ", since it involves fewer unjustified ideas"))
    elif len(unjustified_themes2) < len(unjustified_themes1):
        part6 = answer2_better("  All in all, ",
                               (", since it involves no unjustified ideas"
                                if len(unjustified_themes2) == 0
                                else ", since it involves fewer unjustified ideas"))
    elif (not theme_differences_p
          and not (num_rule_differences == -1)
          and rule1_abstractness > rule2_abstractness):
        part6 = answer1_better(
            "  All in all, ",
            (_fmt(", since it involves seeing ~a in a more abstract way", change1)
             if change1 == change2
             else _fmt(", since ~a is seen in a more abstract way than ~a", change1, change2)))
    elif (not theme_differences_p
          and not (num_rule_differences == -1)
          and rule2_abstractness > rule1_abstractness):
        part6 = answer2_better(
            "  All in all, ",
            (_fmt(", since it involves seeing ~a in a more abstract way", change2)
             if change1 == change2
             else _fmt(", since ~a is seen in a more abstract way than ~a", change2, change1)))
    elif len(all_themes1) > len(all_themes2):
        part6 = answer1_better("  All in all, ", ", since it is based on a richer set of ideas")
    elif len(all_themes2) > len(all_themes1):
        part6 = answer2_better("  All in all, ", ", since it is based on a richer set of ideas")
    else:
        part6 = neither_better("  All in all, ")
    return _string_append(part1, part2, part3, part4, part5, part6)


def average_theme_abstractness(answer):
    """answers.ss: average-theme-abstractness"""
    themes = tell(answer, "get-themes") + tell(answer, "get-unjustified-themes")
    # chez: map's order of application (the procedure only reads)
    return round_(average(chez.map_(theme_abstractness, themes)))


def theme_abstractness(theme):
    """answers.ss: theme-abstractness"""
    dimension = first(theme)
    relation = second(theme)
    dimension_abstractness = cd(dimension)
    if relation is slipnet.plato_identity:
        relation_abstractness = 0
    elif relation is _metacat.themes.diff:
        relation_abstractness = 50
    else:
        relation_abstractness = cd(relation)
    return round_(average(dimension_abstractness, relation_abstractness))


def answer_incoherent_p(answer):
    """answers.ss: answer-incoherent?"""
    theme_abstractness_ = average_theme_abstractness(answer)
    rule_abstractness = tell(answer, "get-top-rule-abstractness")
    abstractness_difference = chez.sub(theme_abstractness_, rule_abstractness)
    return (theme_abstractness_ > 50
            and rule_abstractness < theme_abstractness_
            and abstractness_difference > 25)


def coherence_phrase(coherence):
    """answers.ss: coherence-phrase"""
    if coherence < -50:
        return String("very incoherent")
    if coherence < -10:
        return String("incoherent")
    if coherence < 0:
        return String("somewhat incoherent")
    if coherence < 10:
        return String("very coherent")
    return String("coherent")


def answer_finder():
    """answers.ss: answer-finder (the codelet procedure)"""
    # a two-binding let; get-mapping-strength only reads
    top_strength = tell(workspace.g_workspace, "get-mapping-strength", "top")
    vertical_strength = tell(workspace.g_workspace, "get-mapping-strength", "vertical")
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(cube(chez.mul(percent(top_strength), percent(vertical_strength)))):
        say("Mappings are not strong enough. Fizzling.")
        sugar.fizzle()
    say("Trying to translate a rule...")
    say("Top mapping strength is ", top_strength)
    say("Vertical mapping strength is ", vertical_strength)
    supported_rules = tell(workspace.g_workspace, "get-supported-rules", "top")
    if _null_p(supported_rules):
        say("No supported rules exist. Fizzling.")
        sugar.fizzle()
    rule_weights = formulas.temp_adjusted_values(tell_all(supported_rules, "get-strength"))
    rule = stochastic_pick(supported_rules, rule_weights)
    vprintf("Answer-finder codelet chose rule using weights ~a:~n", rule_weights)
    vprint(rule)
    degree_of_support = tell(rule, "get-degree-of-support")
    if setup.p_verbose is not False:
        say("Degree of support is ", round_(degree_of_support))
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(percent(degree_of_support)):
        say("Not enough support for chosen rule. Fizzling.")
        sugar.fizzle()
    if tell(rule, "currently-works?") is False:
        say("Chosen rule no longer works. Fizzling.")
        sugar.fizzle()
    # result ::= #f
    #          | (<translated-rule> <vertical-bridges> <slippage-log>
    #             <vertical-mapping-supporting-groups>)
    result = translate(rule)
    if not exists_p(result):
        say("Couldn't translate chosen rule. Fizzling.")
        sugar.fizzle()
    translated_rule = first(result)
    supporting_vertical_bridges = second(result)
    slippage_log = third(result)
    vertical_mapping_supporting_groups = fourth(result)
    rule_ref_objects = fifth(result)
    translated_rule_ref_objects = sixth(result)
    say("Rule translated. Building an answer...")
    # result ::= #f | ({<object-transform> | <string-transform>} ...)
    # <object-transform> ::= (<ref-object> (<transform> ...))
    # <string-transform> ::= (<string> (<transform> ...))
    # <transform> ::= (<dimension> <descriptor>)
    snag_action = process_snag(rule, translated_rule, supporting_vertical_bridges,
                               slippage_log, rule_ref_objects)
    result = rules.apply_rule(translated_rule, workspace.g_target_string, snag_action)
    if not exists_p(result):
        sugar.fizzle()
    if tell(_metacat.memory.g_memory, "answer-present?",
            tell(workspace.g_target_string, "generate-image-letters"),
            rule, translated_rule) is not False:
        say("Already found this answer. Fizzling.")
        sugar.fizzle()
    tell(translated_rule, "set-quality-values")
    # make-translated-string sets translated-rule's supporting bridges
    # and thematic-pattern:
    translated_string = make_translated_string(translated_rule, workspace.g_target_string)
    all_supporting_groups = remq_duplicates(
        vertical_mapping_supporting_groups + get_rule_supporting_groups(rule, translated_rule))
    return report_new_answer(translated_string, rule, translated_rule,
                             supporting_vertical_bridges, all_supporting_groups,
                             rule_ref_objects, translated_rule_ref_objects, slippage_log, [])


# get-rule-supporting-groups gets all of the groups (but not the letters) that
# support both of the given rules.  A group supports a rule if either:
# (1) the rule explicitly refers to the group (i.e., the group is a reference object)
# (2) the group is connected to a horizontal bridge that supports the rule
# (3) the group is a nested member of a group of type (2) above

def get_rule_supporting_groups(top_rule, bottom_rule):
    """answers.ss: get-rule-supporting-groups"""
    supporting_horizontal_bridges = (tell(top_rule, "get-supporting-horizontal-bridges")
                                     + tell(bottom_rule, "get-supporting-horizontal-bridges"))
    top_rule_ref_objects = tell(workspace.g_initial_string, "get-all-reference-objects",
                                top_rule)
    bottom_rule_ref_objects = tell(workspace.g_target_string, "get-all-reference-objects",
                                   bottom_rule)
    all_rule_reference_groups = filter_out(compose(lambda x: x is False, group_p),
                                           top_rule_ref_objects + bottom_rule_ref_objects)

    def bridge_groups(b):
        # the lambda's let binds object1 and object2 (only reads) but doesn't use them
        return [groups.get_all_nested_groups(tell(b, "get-object1")),
                groups.get_all_nested_groups(tell(b, "get-object2"))]
    # chez: map's order of application (the procedure only reads)
    return remq_duplicates(flatten([all_rule_reference_groups,
                                    chez.map_(bridge_groups, supporting_horizontal_bridges)]))


def make_translated_string(rule, string1):
    """answers.ss: make-translated-string"""
    result = rules.apply_rule(rule, string1, rules.ignore_snag)
    letter_categories = tell(string1, "generate-image-letters")
    string_type_ = tell(string1, "get-string-type")
    if string_type_ == "initial":
        string_type = "modified"
    elif string_type_ == "target":
        string_type = "answer"
    else:
        string_type = None
    string2 = workspace_strings.new_workspace_string(string_type, letter_categories)
    object_transforms = filter_out(compose(workspace_string_p, first), result)
    string_transform = select(compose(workspace_string_p, first), result)
    image1 = tell(string1, "get-image")
    tell(string2, "mark-as-translated")
    position = [0]

    def instantiate_leaf(leaf_image):
        tell(leaf_image, "instantiate-as-letter", string2, position[0])
        position[0] = chez.add1(position[0])
        return "done"
    tell(image1, "do-walk", "leaf-walk", instantiate_leaf)
    tell(string2, "set-letter-list")
    if setup.p_workspace_graphics is not False:
        tell(setup.g_workspace_window, "init-translated-string-graphics",
             string2, tell(rule, "get-rule-type"))
    tell(image1, "do-walk", "postorder-interior-walk",
         lambda interior_image: tell(interior_image, "instantiate-as-group", string2))
    attach_length_to_appropriate_groups(object_transforms)
    horizontal_bridges = make_translated_rule_bridges(object_transforms, string_transform)
    irrelevant_groups = filter_(irrelevant_translated_string_group_p(horizontal_bridges),
                                tell(string2, "get-groups"))
    for group in irrelevant_groups:
        tell(string2, "delete-group", group)
    tell(rule, "set-translated-rule-information", horizontal_bridges)
    return string2


def attach_length_to_appropriate_groups(object_transforms):
    """answers.ss: attach-length-to-appropriate-groups"""
    for ot in object_transforms:
        object_ = first(ot)
        transforms = second(ot)
        instantiated_object = tell(object_, "get-instantiated-image-object")
        Length_transform = chez.assq(slipnet.plato_length, transforms)
        BondFacet_transform = chez.assq(slipnet.plato_bond_facet, transforms)
        # if* bodies are sequences: with a Length transform both lengths are
        # attached, and the BondFacet test follows in the same body
        if exists_p(instantiated_object) and group_p(instantiated_object):
            if exists_p(Length_transform):
                if (group_p(object_)
                        and tell(object_, "description-type-present?",
                                 slipnet.plato_length) is False):
                    groups.attach_length_description(object_)
                groups.attach_length_description(instantiated_object)
            if (exists_p(BondFacet_transform)
                    and second(BondFacet_transform) is slipnet.plato_length):
                for subobject in tell(instantiated_object, "get-constituent-objects"):
                    groups.attach_length_description(subobject)
    return None


def make_translated_rule_bridges(object_transforms, string_transform):
    """answers.ss: make-translated-rule-bridges"""
    if exists_p(string_transform):
        unmapped_string_subobjects = remq_elements(
            [first(ot) for ot in object_transforms],
            tell(first(string_transform), "get-constituent-objects"))
    else:
        unmapped_string_subobjects = []
    instantiated_object_transforms = filter_(
        lambda ot: exists_p(tell(first(ot), "get-instantiated-image-object")),
        object_transforms)
    # (append (map ...) (map ...)): making bridges draws nothing and changes no
    # shared state, so the order of the two maps is not observable (kept as Racket's)
    # chez: map's order of application
    horizontal_bridges = (
        chez.map_(lambda ot: bridges.make_horizontal_bridge(
                      first(ot), tell(first(ot), "get-instantiated-image-object"), second(ot)),
                  instantiated_object_transforms)
        + chez.map_(lambda subobject: bridges.make_horizontal_bridge(
                        subobject, tell(subobject, "get-instantiated-image-object"), []),
                    unmapped_string_subobjects))
    for bridge in horizontal_bridges:
        tell(bridge, "mark-as-translated-rule-bridge")
        if setup.p_workspace_graphics is not False:
            tell(bridge, "update-proposal-level", workspace.p_built)
            tell(bridge, "set-graphics-pexp",
                 _metacat.bridge_graphics.make_bridge_pexp(bridge, workspace.p_built))
    return horizontal_bridges


# Example of irrelevant group:
# Swapping m and [jjj] in string [m][rr][jjj] results in new string [[jjj]][rr]m
# Outer [[jjj]] group is irrelevant and should be removed from new string.

def irrelevant_translated_string_group_p(horizontal_bridges):
    """answers.ss: irrelevant-translated-string-group?"""
    mapped_translated_string_objects = tell_all(horizontal_bridges, "get-object2")
    mapped_nested_groups = flatmap(groups.get_all_nested_groups,
                                   filter_(group_p, mapped_translated_string_objects))
    return lambda group: not member_p(group, mapped_nested_groups)


# <failure-result> ::=
#   (SWAP <objects> <dimension>)
# | (CONFLICT <object1> <dimension1> <object2> <dimension2>)
# | (CHANGE <object> <transform>)
# Note: The <object> of a CHANGE can be a workspace string.
# <transform> ::= (<dimension> <descriptor>)
#               | (<GroupCtgy> <relation> <BondFacet>)

def process_snag(rule, translated_rule, supporting_vertical_bridges, slippage_log,
                 rule_ref_objects):
    """answers.ss: process-snag"""
    def snag_action(failure_result):
        # the arguments only read
        snag_event = _metacat.trace.make_snag_event(
            failure_result, rule, translated_rule, supporting_vertical_bridges,
            slippage_log, rule_ref_objects)
        tell(_metacat.trace.g_trace, "add-event", snag_event)
        if tell(_metacat.memory.g_memory, "snag-present?", rule) is False:
            _metacat.memory.abstract_snag_description(snag_event)
        # the two add-comment arguments only read
        tell(setup.g_comment_window, "add-comment",
             [_fmt("Uh-oh, I seem to have run into a little problem~a.  ~a.",
                   (String(" again")
                    if tell(_metacat.trace.g_trace, "get-num-of-events", "snag") > 1
                    else String("")),
                   capitalize_string(tell(snag_event, "get-explanation")))],
             [_fmt("Hit ~a snag:  ~a.",
                   (String("another")
                    if tell(_metacat.trace.g_trace, "get-num-of-events", "snag") > 1
                    else String("a")),
                   capitalize_string(tell(snag_event, "get-explanation")))])
        if setup.p_workspace_graphics is not False:
            tell(setup.g_workspace_window, "draw-current-snag")
            tell(setup.g_workspace_window, "erase-current-snag")
        tell(workspace.g_initial_string, "delete-all-proposed-bonds")
        tell(workspace.g_initial_string, "delete-all-proposed-groups")
        tell(workspace.g_modified_string, "delete-all-proposed-bonds")
        tell(workspace.g_modified_string, "delete-all-proposed-groups")
        tell(workspace.g_target_string, "delete-all-proposed-bonds")
        tell(workspace.g_target_string, "delete-all-proposed-groups")
        tell(workspace.g_workspace, "delete-all-proposed-bridges")
        setup.g_temperature = 100
        _metacat.run.g_temperature_clamped_p = True
        if setup.p_workspace_graphics is not False:
            tell(setup.g_temperature_window, "update-graphics", 100)
        tell(snag_event, "activate")
        # Deleting all codelets automatically erases all proposed bonds, groups,
        # and bridges, so we don't need to worry about explicitly erasing them
        # when they get deleted from the workspace:
        tell(coderack.g_coderack, "delete-all-codelets")
        _metacat.run.post_initial_codelets()
        return _metacat.run.update_everything()
    return snag_action


# --------------------------------- Rule Translation ----------------------------------

class SlippageLog(SchemeObject):
    """answers.ss: make-slippage-log (the closure)"""
    __slots__ = ("rule_type", "directly_applied_slippages", "coattail_slippage_table",
                 "slippage_bridges")

    def __init__(this, rule_type):
        this.rule_type = rule_type
        this.directly_applied_slippages = []
        # coattail-slippage-table is a list of the form
        # ((<coattail-slippage> <inducing-slippage>) ...)
        this.coattail_slippage_table = []
        this.slippage_bridges = []

    @message("object-type")
    def object_type(this, self):
        return "slippage-log"

    @message("print")
    def print_(this, self):
        if this.rule_type == "top":
            direction = String("top-to-bottom")
        elif this.rule_type == "bottom":
            direction = String("bottom-to-top")
        else:
            direction = None
        sugar.printf("~nTranslation direction: ~a~n", direction)
        sugar.printf("Directly-applied slippages:~n")
        if _null_p(this.directly_applied_slippages):
            sugar.printf("  None~n")
        else:
            for slippage in this.directly_applied_slippages:
                sugar.printf("  ~a~n", tell(slippage, "print-name"))
        sugar.printf("Coattail-slippages:~n")
        if _null_p(this.coattail_slippage_table):
            sugar.printf("  None~n")
        else:
            for entry in this.coattail_slippage_table:
                sugar.printf("  ~a induced by ~a~n",
                             tell(first(entry), "print-name"),
                             tell(second(entry), "print-name"))
        return sugar.printf("~n")

    @message("get-directly-applied-slippages")
    def get_directly_applied_slippages(this, self):
        return this.directly_applied_slippages

    @message("get-coattail-slippages")
    def get_coattail_slippages(this, self):
        return [first(entry) for entry in this.coattail_slippage_table]

    @message("get-coattail-inducing-slippages")
    def get_coattail_inducing_slippages(this, self):
        return [second(entry) for entry in this.coattail_slippage_table]

    @message("get-applied-slippages")
    def get_applied_slippages(this, self):
        return remq_duplicates(this.directly_applied_slippages
                               + tell(self, "get-coattail-inducing-slippages"))

    @message("coattail-inducing-slippage?")
    def coattail_inducing_slippage_p(this, self, slippage):
        return chez.ormap(lambda x: slippage is second(x), this.coattail_slippage_table)

    @message("get-slippage-bridges")
    def get_slippage_bridges(this, self):
        return remq_duplicates(this.slippage_bridges)

    @message("get-bridge")
    def get_bridge(this, self, slippage):
        return select(lambda b: member_p(slippage, tell(b, "get-slippages")),
                      this.slippage_bridges)

    @message("get-slippage-to-highlight")
    def get_slippage_to_highlight(this, self, slippage):
        vertical_bridge = tell(self, "get-bridge", slippage)
        non_symmetric_slippages = tell(vertical_bridge, "get-non-symmetric-slippages")
        if member_p(slippage, non_symmetric_slippages):
            return slippage
        return select_meth(non_symmetric_slippages, "symmetric?", slippage)

    @message("get-highlight-color")
    def get_highlight_color(this, self, highlighted_slippage, applied_slippage):
        if tell(self, "coattail-inducing-slippage?", applied_slippage) is not False:
            if highlighted_slippage is applied_slippage:
                return view_globals.p_coattail_inducing_slippage_color
            return view_globals.p_dim_coattail_inducing_slippage_color
        if highlighted_slippage is applied_slippage:
            return view_globals.p_vertical_slippage_color
        return view_globals.p_dim_vertical_slippage_color

    @message("applied")
    def applied(this, self, slippage):
        this.directly_applied_slippages = [slippage] + this.directly_applied_slippages
        this.slippage_bridges = ([tell(tell(slippage, "get-object1"), "get-bridge", "vertical")]
                                 + this.slippage_bridges)
        return "done"

    @message("coattail")
    def coattail(this, self, node1, label, node2, inducing_slippage):
        # the arguments only read
        coattail_slippage = concept_mappings.make_concept_mapping(
            "coattail", tell(node1, "get-category"), node1,
            "coattail", tell(node1, "get-category"), node2)
        this.coattail_slippage_table = ([[coattail_slippage, inducing_slippage]]
                                        + this.coattail_slippage_table)
        this.slippage_bridges = ([tell(tell(inducing_slippage, "get-object1"),
                                       "get-bridge", "vertical")]
                                 + this.slippage_bridges)
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_slippage_log(rule_type):
    """answers.ss: make-slippage-log"""
    return SlippageLog(rule_type)


def translate(rule):
    """answers.ss: translate"""
    def body(return_):
        rule_type = tell(rule, "get-rule-type")
        if rule_type == "top":
            translation_direction = "top-to-bottom"
            from_string = workspace.g_initial_string
            to_string = workspace.g_target_string
        elif rule_type == "bottom":
            translation_direction = "bottom-to-top"
            from_string = workspace.g_target_string
            to_string = workspace.g_initial_string
        else:
            translation_direction = from_string = to_string = None
        rule_clauses = tell(rule, "get-rule-clauses")
        slippage_log = make_slippage_log(rule_type)

        def fail():
            return return_(False)
        # chez: map's order of application (the procedure draws and can escape)
        result = chez.map_(translate_rule_clause(from_string, to_string, slippage_log, fail),
                           rule_clauses)
        translated_clauses = map_compress(first, result)
        if chez.andmap(valid_rule_clause_p, translated_clauses) is False:
            return False
        translated_rule = rules.make_rule("bottom" if rule_type == "top" else "top",
                                          translated_clauses)
        from_string_ref_objects = filter_out(
            workspace_string_p, tell(from_string, "get-all-reference-objects", rule))
        to_string_ref_objects = filter_out(
            workspace_string_p, tell(to_string, "get-all-reference-objects", translated_rule))
        supporting_vertical_bridges = remq_duplicates(
            tell(slippage_log, "get-slippage-bridges")
            + intersect(compress(tell_all(from_string_ref_objects, "get-bridge", "vertical")),
                        compress(tell_all(to_string_ref_objects, "get-bridge", "vertical"))))
        vertical_mapping_supporting_groups = remq_duplicates(flatmap(
            lambda b: (groups.get_all_nested_groups(tell(b, "get-object1"))
                       + groups.get_all_nested_groups(tell(b, "get-object2"))),
            supporting_vertical_bridges))
        if setup.p_workspace_graphics is not False:
            _metacat.rule_graphics.initialize_rule_graphics(translated_rule)
        tell(translated_rule, "mark-as-translated", rule, translation_direction)
        return [translated_rule,
                supporting_vertical_bridges,
                slippage_log,
                vertical_mapping_supporting_groups,
                from_string_ref_objects,
                to_string_ref_objects]
    return sugar.continuation_point_star(body)


# translate-rule-clause returns a list of the form
#    (<translated-clause> (<slippage> ...))
# where (<slippage> ...) is a list of all the slippages applicable to the rule clause

def translate_rule_clause(from_string, to_string, slippage_log, fail):
    """answers.ss: translate-rule-clause"""
    def translate_clause(rule_clause):
        if rules.verbatim_clause_p(rule_clause) is not False:
            return [rule_clause, []]
        object_descriptions = second(rule_clause)
        # chez: map's order of application (the procedure draws and can escape)
        result = chez.map_(translate_object_description(from_string, to_string,
                                                        slippage_log, fail),
                           object_descriptions)
        translated_object_descriptions = [first(r) for r in result]
        applicable_object_description_slippages = flatmap(second, result)
        enclosing_slippages = remq_duplicates(flatmap(third, result))
        possible_transform_slippages = (applicable_object_description_slippages
                                        + enclosing_slippages)
        # utilities.ss's filter tests first to last: one prob? per dimension, in order
        ignored_dimensions = filter_(
            lambda d: prob_p(0.4),
            remq_duplicates(tell_all(possible_transform_slippages, "get-CM-type")))
        applicable_transform_slippages = filter_out(
            lambda s: member_p(tell(s, "get-CM-type"), ignored_dimensions),
            possible_transform_slippages)
        clause_type = first(rule_clause)
        if clause_type == "extrinsic":
            translator = apply_to_dimension(applicable_transform_slippages, slippage_log)
        elif clause_type == "intrinsic":
            translator = apply_to_change(applicable_transform_slippages, slippage_log)
        else:
            translator = None
        # only the map draws; chez: map's order of application
        translated_rule_clause = [clause_type,
                                  translated_object_descriptions,
                                  chez.map_(translator, third(rule_clause))]
        all_applicable_slippages = (applicable_object_description_slippages
                                    + applicable_transform_slippages)
        return [translated_rule_clause, all_applicable_slippages]
    return translate_clause


# For an intrinsic-clause, this eliminates the (self <ObjCtgy> <group>) change if
# the object-description denotes a group, or the (self <ObjCtgy> <letter>) change if
# object-description denotes a letter.  For an extrinsic-clause that doesn't denote
# subobjects, the <ObjCtgy> dimension is eliminated if the object-descriptions all
# specify objects of the same type:

def remove_redundant_ObjCtgy_change(rule_clause):
    """answers.ss: remove-redundant-ObjCtgy-change"""
    tag = rule_clause[0]
    if tag == "extrinsic":
        object_descriptions = rule_clause[1]
        dimensions = rule_clause[2]
        if (member_p(slipnet.plato_object_category, dimensions)
                and not (len(object_descriptions) == 1)
                and all_same_p([first(od) for od in object_descriptions]) is not False):
            new_dimensions = chez.remq(slipnet.plato_object_category, dimensions)
            if _null_p(new_dimensions):
                return False
            return ["extrinsic", object_descriptions, new_dimensions]
        return rule_clause
    if tag == "intrinsic":
        object_descriptions = rule_clause[1]
        changes = rule_clause[2]
        # a two-binding let; both only read
        ObjCtgy__self_change = rules.select_change(slipnet.plato_object_category, "self",
                                                   changes)
        object_type = first(first(object_descriptions))
        if (exists_p(ObjCtgy__self_change)
                and third(ObjCtgy__self_change) is object_type):
            new_changes = chez.remq(ObjCtgy__self_change, changes)
            if _null_p(new_changes):
                return False
            return ["intrinsic", object_descriptions, new_changes]
        return rule_clause
    return None


# translate-object-description returns a list of the form
# (<translated-object-description>
#  <applicable-object-description-slippages>
#  <enclosing-bond-slippages>)
# where <applicable-object-description-slippages> is a list of slippages
# _applicable_ to object-description (but not necessarily applied to it).
# Whether these slippages are actually _used_ in the translation process
# is a probabilistic decision made by the 'apply-slippages slipnode method.
# The slippages that actually get used are recorded in the slippage-log.

def translate_object_description(from_string, to_string, slippage_log, fail):
    """answers.ss: translate-object-description"""
    def translate_description(object_description):
        from_objects = tell(from_string, "get-object-description-ref-objects",
                            object_description)
        if _null_p(from_objects):
            fail()
        vertical_bridges = tell_all(from_objects, "get-bridge", "vertical")
        if all_exist_p(vertical_bridges) is False:
            if whole_string_object_description_p(object_description) is not False:
                translated = translate_whole_string_object_description(object_description,
                                                                       to_string)
            else:
                translated = object_description
            return [translated, [], []]
        # a two-binding let; both only read
        all_enclosing_groups1 = tell_all(vertical_bridges, "get-enclosing-group1")
        all_enclosing_groups2 = tell_all(vertical_bridges, "get-enclosing-group2")
        if (all_same_p(all_enclosing_groups1) is False
                or all_same_p(all_enclosing_groups2) is False):
            fail()
        all_bridge_slippages = _append_all(tell_all(vertical_bridges, "get-slippages"))
        translation_direction = ("down" if tell(from_string, "top-string?") is not False
                                 else "up")
        if translation_direction == "down":
            symmetric_message = "get-symmetric-slippages"
        else:
            symmetric_message = "get-non-symmetric-slippages"
        symmetric_bridge_slippages = _append_all(tell_all(vertical_bridges, symmetric_message))
        applicable_object_description_slippages = filter_out(
            lambda s: (member_p(s, symmetric_bridge_slippages)
                       and not (tell(s, "get-label") is slipnet.plato_opposite)),
            all_bridge_slippages)
        translated_object_description = apply_to_object_description(
            applicable_object_description_slippages, object_description, slippage_log)
        enclosing_group1 = first(all_enclosing_groups1)
        enclosing_group2 = first(all_enclosing_groups2)
        if (exists_p(enclosing_group1)
                and exists_p(enclosing_group2)
                and bridges.bridge_between_p("vertical", enclosing_group1,
                                             enclosing_group2) is not False):
            enclosing_bridge = tell(enclosing_group1, "get-bridge", "vertical")
        else:
            enclosing_bridge = False
        if exists_p(enclosing_bridge):
            all_enclosing_bond_slippages = tell(enclosing_bridge, "get-bond-slippages")
        else:
            all_enclosing_bond_slippages = []
        return [translated_object_description,
                applicable_object_description_slippages,
                all_enclosing_bond_slippages]
    return translate_description


# This actually could be done with just the plato-whole line, but
# the 'string line is included for completeness:

def whole_string_object_description_p(object_description):
    """answers.ss: whole-string-object-description?"""
    return (chez.eq_p(first(object_description), "string")
            or third(object_description) is slipnet.plato_whole)


def translate_whole_string_object_description(object_description, to_string):
    """answers.ss: translate-whole-string-object-description"""
    if tell(to_string, "spanning-group-exists?") is not False:
        return [slipnet.plato_group, slipnet.plato_string_position_category, slipnet.plato_whole]
    return ["string", slipnet.plato_string_position_category, slipnet.plato_whole]


def apply_to_change(slippages, slippage_log):
    """answers.ss: apply-to-change"""
    def apply_change(change):
        # chez: (list ...) evaluates its arguments left to right, and both
        # apply-slippages calls can draw and log slippages
        descriptor_type = tell(second(change), "apply-slippages", slippages, slippage_log)
        descriptor = tell(third(change), "apply-slippages", slippages, slippage_log)
        return [first(change), descriptor_type, descriptor]
    return apply_change


def apply_to_dimension(slippages, slippage_log):
    """answers.ss: apply-to-dimension"""
    return lambda dimension: tell(dimension, "apply-slippages", slippages, slippage_log)


def apply_to_object_description(slippages, object_description, slippage_log):
    """answers.ss: apply-to-object-description"""
    # chez: (list ...) evaluates its arguments left to right, and the
    # apply-slippages calls can draw and log slippages
    if chez.eq_p(first(object_description), "string"):
        object_type = first(object_description)
    else:
        object_type = tell(first(object_description), "apply-slippages", slippages,
                           slippage_log)
    description_type = tell(second(object_description), "apply-slippages", slippages,
                            slippage_log)
    descriptor = tell(third(object_description), "apply-slippages", slippages, slippage_log)
    return [object_type, description_type, descriptor]


def valid_rule_clause_p(rule_clause):
    """answers.ss: valid-rule-clause?"""
    value = rules.verbatim_clause_p(rule_clause)
    if value is not False:
        return value
    if (rules.extrinsic_clause_p(rule_clause) is not False
            and chez.andmap(valid_object_description_p, second(rule_clause)) is not False):
        value = (len(second(rule_clause)) > 1
                 or not (first(first(second(rule_clause))) is slipnet.plato_letter))
        if value is not False:
            return value
    if (rules.intrinsic_clause_p(rule_clause) is not False
            and valid_object_description_p(first(second(rule_clause))) is not False):
        return chez.andmap(valid_change_p, third(rule_clause))
    return False


def valid_object_description_p(object_description):
    """answers.ss: valid-object-description?"""
    return tell(third(object_description), "get-category") is second(object_description)


def valid_change_p(change):
    """answers.ss: valid-change?"""
    value = slipnet.platonic_relation_p(third(change))
    if value is not False:
        return value
    return tell(third(change), "get-category") is second(change)


def load():
    """answers.ss: the define-codelet-procedure* form (answer-finder)."""
    sugar.define_codelet_procedure_star("answer-finder", answer_finder)
