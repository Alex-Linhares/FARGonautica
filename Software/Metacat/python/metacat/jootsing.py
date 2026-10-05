"""jootsing.ss: self-watching.  The jootser codelet (jumping out of the system
after recurring clamps or snags) and the progress-watcher codelet (undoing
clamps, and clamping the rule-codelet pattern when nothing much is happening).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from jootsing.ss, with
racket/engine/jootsing.rktl as a worked translation.

The file defines no objects.  The jootser and progress-watcher codelet
procedures are given to their codelet types by load() (define-codelet-procedure*
needs the types, which coderack.load makes); the four parameters
(%satisfactory-rule-quality%, %settling-period%, %max-clamp-period%,
%grace-period%) are module constants, since they need nothing else.

Earlier files are imported directly: answers.ss (translate, give-up,
report-new-answer, get-rule-supporting-groups), coderack.ss (the codelet types
answer-justifier and answer-finder, %very-low-urgency%,
%extremely-high-urgency%), setup.ss (%self-watching-enabled%, %justify-mode%,
%verbose%, *codelet-count*, *comment-window*), workspace.ss (*workspace* and the
four strings) and justify.ss (get-unifying-slippages).  Names from files
translated later are read through the package at call time: trace.ss (*trace*,
make-clamp-event, entries, theme-pattern-entries-equal?,
negate-theme-pattern-entry, against-background, %bottom-up-codelet-pattern%,
%rule-codelet-pattern%) and memory.ss (*memory*).  The comment window is sent
add-comment ungated, as in the original.  The engine never imports tkinter.

Evaluation order (audited against Chez's): the draws are the stochastic-if*s
(jootser's clamp and snag ones, joots-from-justify-clamps', progress-watcher's
two; each draws its coin first, then evaluates its probability, which here
only reads), jootser's stochastic-filter (utilities.ss: first to last, the
inclusion probability computed, then one prob?), and translate
(joots-from-justify-clamps; answers.ss).  The multi-binding lets (jootser's
clamp-type and jootsing-probability, get-clamp-jootsing-probability's
clamp-type and num-of-clamps, joots-from-justify-clamps' two rules and its
supporting groups and bottom-rule reference objects) and the multi-argument
calls (add-comment, make-clamp-event, report-new-answer, post-codelet*,
format) only read: get-clamp-jootsing-probability prints only with %verbose%
on, and then only after both bindings' values would be needed, so its order
against the clamp-type read is not observable.  maps go through chez.map_
(tell-all included); partition and select-extreme keep utilities.ss's order.
vprintf and vprint are macros that evaluate their arguments only with
%verbose% on; the sites with computed arguments are gated likewise.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, sugar
from metacat import answers, coderack, justify, setup, workspace
from metacat.objects import tell
from metacat.sugar import say, vprintf
from metacat.utilities import (average, exists_p, fifth, filter_meth, filter_out,
                               filter_out_meth, first, flatmap, fourth, maximum, minimum,
                               one_minus, partition, percent, quoted_string, remove_duplicates,
                               remq_duplicates, reveal, round_, round_to_100ths, second,
                               select_extreme, sixth, stochastic_filter, tell_all, third,
                               times_100, weighted_average, workspace_string_p)

String = chez.String

# <theme-overlap-table> ::= ((<entry> <overlap-value>) ...)
# <entry> ::= (<dimension> <relation>)


def _null_p(l):
    return len(l) == 0


def _append_all(lists):
    """(apply append lists)"""
    out = []
    for part in lists:
        out.extend(part)
    return out


def jootser():
    """jootsing.ss: jootser (the codelet procedure)"""
    if setup.p_self_watching_enabled is False:
        say("Self-watching disabled. Fizzling.")
        sugar.fizzle()
    last_event = tell(_metacat.trace.g_trace, "get-last-event", "any")
    if not exists_p(last_event):
        vprintf("No last event. Fizzling.~n")
        sugar.fizzle()
    # Wait until not in a clamp period to check for repeated clamping:
    if tell(_metacat.trace.g_trace, "within-clamp-period?") is not False:
        vprintf("Currently within a clamp period. Fizzling.~n")
        sugar.fizzle()
    # Check for recurring clamps (manual clamps are ignored):
    vprintf("Checking for recurring clamps...~n")
    clamps = filter_out_meth(get_most_recent_event_set("clamp"),
                             "clamp-type?", "manual-clamp")
    if len(clamps) < 3:
        vprintf("Didn't notice any recurring clamps...~n")
    else:
        # a two-binding let; get-clamp-jootsing-probability only reads (and
        # prints only with %verbose% on)
        clamp_type = tell(first(clamps), "get-clamp-type")
        jootsing_probability = get_clamp_jootsing_probability(clamps)
        vprintf("Hmm...I'm beginning to notice a clamping pattern...~n")
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < jootsing_probability:
            vprintf("Jootsing from ~as...~n", clamp_type)
            # case without else: no other clamp type reaches here
            if clamp_type == "rule-codelet-clamp":
                joots_from_rule_codelet_clamps(clamps)
            elif clamp_type == "snag-response-clamp":
                joots_from_snag_response_clamps(clamps)
            elif clamp_type == "justify-clamp":
                joots_from_justify_clamps(clamps)
            sugar.fizzle()
        vprintf("Jootsing from clamps failed...~n")
    # Check for snags:
    vprintf("Checking for recurring snags...~n")
    snags = get_most_recent_event_set("snag")
    if len(snags) < 3:
        vprintf("Didn't notice any recurring snags. Fizzling.~n")
        sugar.fizzle()
    num_of_snags = len(snags)
    snag_theme_patterns = tell_all(snags, "get-snag-theme-pattern")
    # chez: map's order of application (the procedure only reads)
    theme_overlap_table = chez.map_(
        lambda equivalent_entries: [first(equivalent_entries),
                                    times_100(chez.div(len(equivalent_entries),
                                                       num_of_snags))],
        partition(_metacat.trace.theme_pattern_entries_equal_p,
                  flatmap(_metacat.trace.entries, snag_theme_patterns)))
    max_theme_overlap = maximum(chez.map_(second, theme_overlap_table))
    jootsing_probability = chez.mul(percent(max_theme_overlap),
                                    percent(chez.min_(100, chez.mul(10, num_of_snags))))
    vprintf("Hmm...I'm beginning to notice a snag pattern...~n")
    vprintf("Theme overlap table:~n")
    vprintf("Max theme overlap = ~a~n", max_theme_overlap)
    vprintf("Number of snags = ~a~n", num_of_snags)
    if setup.p_verbose is not False:
        vprintf("Snag jootsing probability = ~a~n", round_to_100ths(jootsing_probability))
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(jootsing_probability):
        vprintf("Jootsing from snags failed. Fizzling.~n")
        sugar.fizzle()
    if tell(_metacat.trace.g_trace, "permission-to-clamp?") is False:
        vprintf("Permission to clamp themes denied. Fizzling.~n")
        sugar.fizzle()
    all_possible_pattern_entries = remove_duplicates(
        flatmap(_metacat.trace.entries, snag_theme_patterns))
    snag_objects = _append_all(tell_all(snags, "get-snag-objects"))
    snag_object_descriptions = remq_duplicates(
        _append_all(tell_all(snag_objects, "get-descriptions")))

    def inclusion(entry):
        overlap = second(chez.assoc(entry, theme_overlap_table))
        snag_descriptions_for_theme = filter_meth(snag_object_descriptions,
                                                  "description-type?", first(entry))
        average_description_depth = average(
            tell_all(snag_descriptions_for_theme, "get-conceptual-depth"))
        inclusion_probability = chez.mul(percent(overlap), percent(average_description_depth))
        if setup.p_verbose is not False:
            vprintf("~a entry: overlap = ~a, avg-dd = ~a, weight = ~a~n",
                    reveal(entry), overlap, average_description_depth,
                    round_to_100ths(inclusion_probability))
        return inclusion_probability
    # utilities.ss's stochastic-filter: first to last, one prob? each
    chosen_pattern_entries = stochastic_filter(inclusion, all_possible_pattern_entries)
    # chez: map's order of application (the procedure only reads)
    negative_theme_pattern = ["vertical-bridge"] + chez.map_(
        _metacat.trace.negate_theme_pattern_entry, chosen_pattern_entries)
    if _null_p(chosen_pattern_entries):
        vprintf("Couldn't make negative theme pattern. Fizzling.~n")
        sugar.fizzle()
    clamp_event = _metacat.trace.make_clamp_event(
        "snag-response-clamp",
        [negative_theme_pattern, _metacat.trace.p_bottom_up_codelet_pattern],
        [], "workspace")
    tell(_metacat.trace.g_trace, "add-event", clamp_event)
    tell(clamp_event, "activate")
    sugar.fizzle()


def get_clamp_jootsing_probability(clamps):
    """jootsing.ss: get-clamp-jootsing-probability"""
    # a two-binding let that only reads
    clamp_type = tell(first(clamps), "get-clamp-type")
    num_of_clamps = len(clamps)
    # chez: map's order of application (the procedure only reads)
    elapsed_time_weights = chez.map_(
        lambda clamp: times_100(chez.div(tell(clamp, "get-time"), setup.g_codelet_count)),
        clamps)
    average_progress = round_(weighted_average(tell_all(clamps, "get-progress-achieved"),
                                               elapsed_time_weights))
    # case without else: void for any other clamp type (then * raises, as in Chez)
    if clamp_type == "justify-clamp":
        clamp_type_factor = (1 if tell(tell(_metacat.trace.g_trace, "get-last-event", "any"),
                                       "type?", "clamp") is not False
                             else 0)
    elif clamp_type == "snag-response-clamp":
        clamp_type_factor = 1
    elif clamp_type == "rule-codelet-clamp":
        clamp_type_factor = 0.5
    else:
        clamp_type_factor = None
    # chez: (* exact 0.5) of an exact 0 is exact 0 (chez.mul)
    jootsing_probability = chez.mul(one_minus(percent(average_progress)),
                                    percent(chez.min_(100, chez.mul(10, num_of_clamps))),
                                    clamp_type_factor)
    if setup.p_verbose is not False:
        vprintf("progress values (last clamp first):  ~a~n",
                tell_all(clamps, "get-progress-achieved"))
        vprintf("progress weights (last clamp first): ~a~n", elapsed_time_weights)
        vprintf("average progress = ~a~n", average_progress)
        vprintf("Clamp jootsing probability = ~a~n", round_to_100ths(jootsing_probability))
    return jootsing_probability


# This function returns a list of equivalent snag or clamp events:
def get_most_recent_event_set(event_type):
    """jootsing.ss: get-most-recent-event-set"""
    # case without else: void for any other event type (then filter-meth fails)
    if event_type == "snag":
        all_recent_events = tell(_metacat.trace.g_trace, "get-new-events-since-last", "answer")
    elif event_type == "clamp":
        all_recent_events = tell(_metacat.trace.g_trace, "get-all-events")
    else:
        all_recent_events = None
    equivalent_event_sets = partition(
        lambda ev1, ev2: tell(ev1, "equal?", ev2),
        filter_meth(all_recent_events, "type?", event_type))
    most_recent_event_set = select_extreme(
        chez.min_,
        lambda events: minimum(tell_all(events, "get-age")),
        equivalent_event_sets)
    if exists_p(most_recent_event_set):
        return most_recent_event_set
    return []


def joots_from_rule_codelet_clamps(clamps):
    """jootsing.ss: joots-from-rule-codelet-clamps"""
    tell(setup.g_comment_window, "add-comment",
         [String("I just can't seem to come up with any better rules.")],
         [String("Jootsing from unsuccessful rule-codelet clamps.")])
    return answers.give_up()


def joots_from_snag_response_clamps(clamps):
    """jootsing.ss: joots-from-snag-response-clamps"""
    tell(setup.g_comment_window, "add-comment",
         [String("This is getting boring.  I can't think of anything else to try.")],
         [String("Jootsing from unsuccessful snag-response clamps.")])
    return answers.give_up()


def joots_from_justify_clamps(clamps):
    """jootsing.ss: joots-from-justify-clamps"""
    # a two-binding let that only reads
    top_rule = tell(first(clamps), "get-rule", "top")
    bottom_rule = tell(first(clamps), "get-rule", "bottom")
    if tell(_metacat.memory.g_memory, "answer-present?",
            tell(workspace.g_answer_string, "get-letter-categories"),
            top_rule, bottom_rule) is not False:
        say("Already justified this answer. Fizzling.")
        sugar.fizzle()
    # Try to give up:
    if (tell(top_rule, "currently-works?") is False
            or tell(bottom_rule, "currently-works?") is False):
        vprintf("Can't give up. Rule(s) don't currently work. Fizzling.~n")
        sugar.fizzle()
    result = answers.translate(top_rule)
    if not exists_p(result):
        vprintf("Can't give up. Couldn't translate rule. Fizzling.~n")
        sugar.fizzle()
    translated_rule = first(result)
    supporting_vertical_bridges = second(result)
    slippage_log = third(result)
    vertical_mapping_supporting_groups = fourth(result)
    # Workspace-strings (if any) have already been filtered out:
    top_rule_ref_objects = fifth(result)
    translated_rule_ref_objects = sixth(result)  # noqa: F841
    unjustified_slippages = justify.get_unifying_slippages(translated_rule, bottom_rule)
    if _null_p(unjustified_slippages):
        vprintf("No unjustified slippages. Posting answer-justifier.~n")
        sugar.post_codelet_star(coderack.p_extremely_high_urgency, coderack.answer_justifier)
        sugar.fizzle()
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    coin_flip = chez.random(1.0)
    if coin_flip < one_minus(chez.div(1, len(unjustified_slippages))):
        vprintf("Too many unjustified slippages. Fizzling.~n")
        sugar.fizzle()
    # Time to give up:
    # a two-binding let that only reads
    all_supporting_groups = remq_duplicates(
        vertical_mapping_supporting_groups
        + answers.get_rule_supporting_groups(top_rule, bottom_rule))
    bottom_rule_ref_objects = filter_out(
        workspace_string_p,
        tell(workspace.g_target_string, "get-all-reference-objects", bottom_rule))
    return answers.report_new_answer(
        workspace.g_answer_string, top_rule, bottom_rule, supporting_vertical_bridges,
        all_supporting_groups, top_rule_ref_objects, bottom_rule_ref_objects,
        slippage_log, unjustified_slippages)


# ----------------------------------------------------------------------------

p_satisfactory_rule_quality = 80

# This is the length of time that must pass after the occurrence of any event
# before progress-watcher codelets can check on the current progress:
p_settling_period = 250

# This is the maximum length of time a clamp lasts.  Clamps can be undone
# before the end of this period by progress-watcher codelets:
p_max_clamp_period = 750

# This is the length of time after unclamping during which no new clamps
# can be made:
p_grace_period = 100


def progress_watcher():
    """jootsing.ss: progress-watcher (the codelet procedure)"""
    vprintf("Progress Watcher running (time ~a)...~n", setup.g_codelet_count)
    if setup.p_self_watching_enabled is False:
        vprintf("Self-watching disabled. Fizzling.~n")
        sugar.fizzle()
    if tell(_metacat.trace.g_trace, "within-clamp-period?") is not False:
        time_since_last_event = tell(_metacat.trace.g_trace, "get-elapsed-time", "any")
        if time_since_last_event > p_settling_period:
            last_clamp = tell(_metacat.trace.g_trace, "get-last-event", "clamp")
            tell(_metacat.trace.g_trace, "undo-last-clamp")
            progress_achieved = tell(last_clamp, "get-progress-achieved")
            vprintf("Progress achieved = ~a~n", progress_achieved)
            # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
            coin_flip = chez.random(1.0)
            if coin_flip < percent(progress_achieved):
                sugar.post_codelet_star(
                    progress_achieved,
                    (coderack.answer_justifier if setup.p_justify_mode is not False
                     else coderack.answer_finder))
            sugar.fizzle()
        vprintf("Too soon since last event to draw any conclusions. Fizzling.~n")
        sugar.fizzle()
    # Not within a clamp period.
    current_activity = tell(workspace.g_workspace, "get-activity")
    if chez.zero_p(current_activity):
        vprintf("~n*******************************~n")
        vprintf("*  Nothing much is happening  *~n")
        vprintf("*******************************~n")
        vprintf("Checking on current rules...~n")
        top_rules = tell(workspace.g_workspace, "get-rules", "top")
        bottom_rules = tell(workspace.g_workspace, "get-rules", "bottom")
        max_top_rule_quality = maximum(tell_all(top_rules, "get-quality"))
        max_bottom_rule_quality = maximum(tell_all(bottom_rules, "get-quality"))
        poor_top_rule_quality_p = max_top_rule_quality < p_satisfactory_rule_quality
        poor_bottom_rule_quality_p = (setup.p_justify_mode is not False
                                      and max_bottom_rule_quality < p_satisfactory_rule_quality)
        if poor_top_rule_quality_p or poor_bottom_rule_quality_p:
            vprintf("Current rules are not good enough.~n")
            vprintf("Attempting to clamp codelet pattern...~n")
            if tell(_metacat.trace.g_trace, "permission-to-clamp?") is False:
                vprintf("Permission to clamp pattern denied. Fizzling.~n")
                sugar.fizzle()
            if setup.p_justify_mode is not False:
                clamp_probability = one_minus(percent(chez.min_(max_top_rule_quality,
                                                                max_bottom_rule_quality)))
            else:
                clamp_probability = one_minus(percent(max_top_rule_quality))
            vprintf("max top-rule quality = ~a~n", max_top_rule_quality)
            vprintf("max bottom-rule quality = ~a~n", max_bottom_rule_quality)
            if setup.p_verbose is not False:
                vprintf("Clamp probability = ~a~n", round_to_100ths(clamp_probability))
            # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
            coin_flip = chez.random(1.0)
            if coin_flip < clamp_probability:
                # format's arguments only read
                if poor_top_rule_quality_p and poor_bottom_rule_quality_p:
                    how_strings_change_description = String(chez.format_(
                        "either ~a or ~a.",
                        how_strings_change(workspace.g_initial_string,
                                           workspace.g_modified_string),
                        how_strings_change(workspace.g_target_string,
                                           workspace.g_answer_string)))
                elif poor_top_rule_quality_p:
                    how_strings_change_description = String(chez.format_(
                        "~a.",
                        how_strings_change(workspace.g_initial_string,
                                           workspace.g_modified_string)))
                else:
                    how_strings_change_description = String(chez.format_(
                        "~a.",
                        how_strings_change(workspace.g_target_string,
                                           workspace.g_answer_string)))
                tell(setup.g_comment_window, "add-comment",
                     [String("I'm getting frustrated.  I still don't see a good way to describe "),
                      how_strings_change_description],
                     [String("No satisfactory rules yet exist for describing "),
                      how_strings_change_description])
                clamped_rule_codelet_pattern = _metacat.trace.against_background(
                    coderack.p_very_low_urgency, _metacat.trace.p_rule_codelet_pattern)
                clamp_event = _metacat.trace.make_clamp_event(
                    "rule-codelet-clamp",
                    # Pay attention to new rule events:
                    [clamped_rule_codelet_pattern], [], "rule")
                tell(_metacat.trace.g_trace, "add-event", clamp_event)
                tell(clamp_event, "activate")
                sugar.fizzle()
            vprintf("Permission granted but attempt failed. Fizzling.~n")
            sugar.fizzle()
        return vprintf("The rules seem to be of decent quality. Fizzling.~n")
    return None


def how_strings_change(string1, string2):
    """jootsing.ss: how-strings-change"""
    return String(chez.format_("how ~a changes to ~a",
                               quoted_string(string1),
                               quoted_string(string2)))


def load():
    """jootsing.ss: the two define-codelet-procedure* forms (jootser, progress-watcher)."""
    sugar.define_codelet_procedure_star("jootser", jootser)
    sugar.define_codelet_procedure_star("progress-watcher", progress_watcher)
