"""trace.ss: the Temporal Trace (its events, snag and clamp periods) and the
theme, concept and codelet patterns.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from trace.ss, with
racket/engine/trace.rktl as a worked translation.  (This is the translation of
trace.ss, not the writer of the JSON golden traces.)

make-temporal-trace's closure is the TemporalTrace class, which delegates what
it doesn't answer to base-object.  make-generic-event's closure is GenericEvent
(delegating to base-object); the closures of make-answer-event,
make-clamp-event, make-concept-activation-event, make-concept-mapping-event,
make-group-event, make-rule-event and make-snag-event are AnswerEvent,
ClampEvent, ConceptActivationEvent, ConceptMappingEvent, GroupEvent, RuleEvent
and SnagEvent, each of which makes its own GenericEvent first and delegates
the rest to it (docs/python-translation-plan.md, "Objects").  The progress
evaluators of clamp and snag events are Python closures.  load() makes the
codelet patterns (they need coderack's codelet types and urgencies, which
coderack.load makes) and then *trace*, in the file's order.

The original reads complement-codelet-pattern in the clamp event's
get-complement-codelet-pattern clause, but no file defines it (the procedure
get-complement-codelet-pattern is a different thing).  Nothing sends that
message; here, as in racket/engine/pending.rktl, reading it raises Chez's
"variable complement-codelet-pattern is not bound" (anomalies_and_quirks.md,
"complement-codelet-pattern is never defined").

Cross-module references.  Files loaded earlier are imported directly:
coderack (*codelet-types*, the codelet types, %very-high-urgency%,
%extremely-high-urgency%, urgency-name), concept_mappings (CMs-equal?), groups
(same-letter-category?, same-group-category?, same-group-direction?), rules
(rules-equal?, punctuate), setup (*codelet-count*, *temperature*, the windows,
%verbose%, %justify-mode%, %self-watching-enabled%, the graphics switches),
slipnet (%max-activation%, the plato- nodes, platonic-relation?), workspace
(*workspace*, *initial-string*, *target-string*) and view_globals (the
constants.ss colours the events draw with, *fg-color* and %default-fg-color%,
#f until the views set them).  Names of files loaded later, or not translated
yet, are read through the package at call time: run.ss (*display-mode?*,
*temperature-clamped?*), themes.ss (*themespace*, %max-theme-activation%,
bridge-type->theme-type), justify.ss (get-unifying-slippages), jootsing.ss
(%grace-period%, %max-clamp-period%), trace-graphics.ss
(group-event-pexp-text-string, which names every group event: its print name is
in the golden traces) and theme-graphics.ss (relation-name, print-pattern only).
The windows are sent their messages exactly as in the original: the ungated
ones (*trace-window*'s initialize and add-event, *comment-window*'s
add-comment) reach the headless null windows too.  Every print name, comment
and string handed to a window is a chez.String.  The engine never imports
tkinter.

Evaluation order (audited against Chez's; trace.rktl has no port: changes).
Nothing in this file draws a random number.  The effects are the Trace's own
state, the clamps (rules, themes, slipnodes, codelet types), the commentary and
the windows, and every one of them sits in a body, a let* or an if*, so
Python's order is Chez's.  The multi-binding lets (make-generic-event's
snapshot of the Workspace and Themespace, make-group-event's, undo-last-clamp's
last-clamp and progress-achieved, get-new-structures-since-last's,
concept-mapping-importance's six bindings, make-concept-activation-event's) and
the multi-argument calls (the add-comment lists, the format calls,
weighted-average's list, get-unifying-slippages' two select-meths) only read,
so their order is not observable; they are kept left to right, as the Racket
port has them.  maps go through chez.map_ (tell-all, flatmap and
adjacency-map included), as the original's map, though the procedures mapped
here (progress evaluators, pattern builders, phrase makers) only read.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez, sugar
from metacat import (coderack, concept_mappings, groups, rules, setup, slipnet, view_globals,
                     workspace)
from metacat.objects import SchemeObject, base_object, delegate, message, tell
from metacat.sugar import vprintf
from metacat.utilities import (average, bond_p, bridge_p, cd, compress,
                               exists_p, filter_, filter_meth, filter_out, first, flatmap,
                               get_first, group_p, hundred_minus, letter_p, maximum, member_p,
                               percent, print_, quoted_string, remove_duplicates,
                               remq_duplicates, remq_elements, rest, round_, second, select,
                               select_meth, sets_equal_p, sets_equal_pred_p, tell_all, third,
                               times_100, weighted_average, workspace_string_p)

String = chez.String


def _fmt(control, *args):
    """(format control arg ...) as a Scheme string."""
    return String(chez.format_(control, *args))


def _string_append(strings):
    """(apply string-append strings) as a Scheme string; like Chez, every
    argument must be a string."""
    for s in strings:
        if not isinstance(s, str):
            raise chez.SchemeError("string-append", "~s is not a string", s)
    return String("".join(strings))


def _append_all(lists):
    """(apply append lists)"""
    out = []
    for part in lists:
        out.extend(part)
    return out


def _null_p(l):
    return len(l) == 0


def _and(a, b_thunk):
    """(and a b) as a value: a if it is #f, else b (chez: only #f is false)."""
    if a is False:
        return False
    return b_thunk()


def _record_case_args(datum, n):
    """record-case's binding of n formals with car/cdr: extra elements are
    ignored, too few raise (as Chez's record-case does)."""
    if len(datum) - 1 < n:
        raise chez.SchemeError("car", "~s is not a pair", list(datum[len(datum):]))
    return datum[1:n + 1]


# ----------------------------------------------------------------------------
# The Temporal Trace

class TemporalTrace(SchemeObject):
    """trace.ss: make-temporal-trace (the closure)"""
    __slots__ = ("event_list", "next_event_number", "last_clamp_time", "last_unclamp_time",
                 "within_clamp_period_p", "within_snag_period_p")

    def __init__(this):
        this.event_list = []
        this.next_event_number = 1
        this.last_clamp_time = False
        this.last_unclamp_time = False
        this.within_clamp_period_p = False
        this.within_snag_period_p = False

    @message("object-type")
    def object_type(this, self):
        return "temporal-trace"

    @message("print")
    def print_(this, self):
        sugar.printf("~nTEMPORAL TRACE:~n")
        for event in reversed(this.event_list):
            sugar.printf("-----------------------------------~n")
            print_(event)
        return sugar.printf("~n")

    @message("get-mouse-selected-event")
    def get_mouse_selected_event(this, self, x, y):
        return select(lambda event: tell(event, "within-bounding-box?", x, y), this.event_list)

    @message("initialize")
    def initialize(this, self):
        this.event_list = []
        this.next_event_number = 1
        this.last_clamp_time = False
        this.last_unclamp_time = False
        this.within_clamp_period_p = False
        this.within_snag_period_p = False
        tell(setup.g_trace_window, "initialize")
        return "done"

    @message("unhighlight-all-events")
    def unhighlight_all_events(this, self):
        for event in this.event_list:
            tell(event, "unhighlight")
        return "done"

    @message("unhighlight-all-events-except")
    def unhighlight_all_events_except(this, self, event):
        def each(e):
            if e is not event:
                return tell(e, "unhighlight")
            return None
        return sugar.for_star(each, this.event_list)

    @message("get-event")
    def get_event(this, self, n):
        return select(lambda event: tell(event, "get-event-number") == n, this.event_list)

    @message("get-all-events")
    def get_all_events(this, self):
        return this.event_list

    @message("get-highlighted-event")
    def get_highlighted_event(this, self):
        return select_meth(this.event_list, "highlighted?")

    @message("get-events")
    def get_events(this, self, event_type):
        return filter_meth(this.event_list, "type?", event_type)

    @message("get-num-of-events")
    def get_num_of_events(this, self, event_type):
        return len(filter_meth(this.event_list, "type?", event_type))

    @message("get-num-of-clamps")
    def get_num_of_clamps(this, self, clamp_type):
        return len(filter_(lambda event: _and(tell(event, "type?", "clamp"),
                                              lambda: tell(event, "clamp-type?", clamp_type)),
                           this.event_list))

    # If event-type/s is a list, the most recent event that matches
    # any of the types in event-type/s is returned:
    @message("get-last-event")
    def get_last_event(this, self, event_type_or_s):
        if isinstance(event_type_or_s, str):
            return select_meth(this.event_list, "type?", event_type_or_s)
        return select(lambda event: member_p(tell(event, "get-type"), event_type_or_s),
                      this.event_list)

    @message("get-new-events-since-last")
    def get_new_events_since_last(this, self, event_type_or_s):
        last_event = tell(self, "get-last-event", event_type_or_s)
        if not exists_p(last_event):
            return this.event_list
        return get_first(chez.sub(len(this.event_list), tell(last_event, "get-event-number")),
                         this.event_list)

    @message("get-new-structures-since-last")
    def get_new_structures_since_last(this, self, event_type_or_s):
        # the two bindings only read
        last_event = tell(self, "get-last-event", event_type_or_s)
        current_workspace_structures = tell(workspace.g_workspace, "get-structures")
        if not exists_p(last_event):
            return current_workspace_structures
        return remq_elements(tell(last_event, "get-structures"), current_workspace_structures)

    @message("get-elapsed-time")
    def get_elapsed_time(this, self, event_type):
        last_event = tell(self, "get-last-event", event_type)
        if exists_p(last_event):
            return tell(last_event, "get-age")
        return setup.g_codelet_count

    @message("get-last-clamp-time")
    def get_last_clamp_time(this, self):
        return this.last_clamp_time

    @message("get-last-unclamp-time")
    def get_last_unclamp_time(this, self):
        return this.last_unclamp_time

    @message("within-clamp-period?")
    def within_clamp_period_p_(this, self):
        return this.within_clamp_period_p

    @message("within-grace-period?")
    def within_grace_period_p(this, self):
        return (this.within_clamp_period_p is False
                and exists_p(this.last_unclamp_time)
                and setup.g_codelet_count < chez.add(this.last_unclamp_time,
                                                     _metacat.jootsing.p_grace_period))

    @message("permission-to-clamp?")
    def permission_to_clamp_p(this, self):
        if setup.p_self_watching_enabled is False:
            return False
        return (this.within_clamp_period_p is False
                and tell(self, "within-grace-period?") is False)

    @message("clamp-period-expired?")
    def clamp_period_expired_p(this, self):
        if this.within_clamp_period_p is False:
            return False
        return setup.g_codelet_count > chez.add(this.last_clamp_time,
                                                _metacat.jootsing.p_max_clamp_period)

    @message("progress-since-last-clamp")
    def progress_since_last_clamp(this, self):
        last_clamp = tell(self, "get-last-event", "clamp")
        new_events_since_last_clamp = tell(self, "get-new-events-since-last", "clamp")
        progress_evaluator = tell(last_clamp, "get-progress-evaluator")
        return maximum(chez.map_(progress_evaluator, new_events_since_last_clamp))

    @message("undo-last-clamp")
    def undo_last_clamp(this, self):
        if this.within_clamp_period_p is not False:
            vprintf("**** UNDOING CLAMP (time ~a) ****~n", setup.g_codelet_count)
            # the two bindings only read
            last_clamp = tell(self, "get-last-event", "clamp")
            progress_achieved = tell(self, "progress-since-last-clamp")
            this.within_clamp_period_p = False
            this.last_unclamp_time = setup.g_codelet_count
            tell(last_clamp, "update-progress-achieved", progress_achieved)
            clamp_type = tell(last_clamp, "get-clamp-type")
            # the add-comment arguments only read
            if clamp_type == "rule-codelet-clamp":
                eliza = [String("Well, my latest effort to think up new rules resulted in"),
                         _fmt(" ~a progress.", clamp_progress_amount_phrase(progress_achieved)),
                         _fmt("  Guess it was ~a effort, in retrospect.",
                              clamp_progress_adjective_phrase(progress_achieved))]
            elif clamp_type == "snag-response-clamp":
                eliza = [_fmt("Looks like I made ~a headway in coming up with",
                              clamp_progress_amount_phrase(progress_achieved)),
                         String(" new ideas.")]
            elif clamp_type == "justify-clamp":
                eliza = [String("Looks like that last brilliant idea I had resulted in "),
                         _fmt("~a progress.", clamp_progress_amount_phrase(progress_achieved)),
                         _fmt("  Guess it was ~a idea, in retrospect.",
                              clamp_progress_adjective_phrase(progress_achieved))]
            elif clamp_type == "manual-clamp":
                eliza = [String("That last suggestion of yours resulted in "),
                         _fmt("~a progress.", clamp_progress_amount_phrase(progress_achieved)),
                         _fmt("  Guess it was ~a idea, in retrospect.",
                              clamp_progress_adjective_phrase(progress_achieved))]
            else:
                eliza = None      # 1.2: a case without else
            clamp_type = tell(last_clamp, "get-clamp-type")
            if clamp_type == "rule-codelet-clamp":
                kind = String("rule-codelet")
            elif clamp_type == "snag-response-clamp":
                kind = String("snag-response")
            elif clamp_type == "justify-clamp":
                kind = String("justify")
            elif clamp_type == "manual-clamp":
                kind = String("manual")
            else:
                kind = None       # 1.2: a case without else
            tell(setup.g_comment_window, "add-comment",
                 eliza,
                 [_fmt("Unclamping patterns.  Progress achieved by ~a clamp = ~a.",
                       kind, progress_achieved)])
            tell(last_clamp, "deactivate")
        return "done"

    @message("current-answer?")
    def current_answer_p(this, self):
        return (exists_p(tell(self, "get-last-event", "answer"))
                and chez.zero_p(tell(self, "get-elapsed-time", "answer")))

    @message("within-snag-period?")
    def within_snag_period_p_(this, self):
        return this.within_snag_period_p

    @message("immediate-snag-condition?")
    def immediate_snag_condition_p(this, self):
        if this.within_snag_period_p is False:
            return False
        return chez.zero_p(tell(self, "get-elapsed-time", "snag"))

    @message("progress-since-last-snag")
    def progress_since_last_snag(this, self):
        last_snag = tell(self, "get-last-event", "snag")
        new_structures_since_last_snag = tell(self, "get-new-structures-since-last", "snag")
        progress_evaluator = tell(last_snag, "get-progress-evaluator")
        return maximum(chez.map_(progress_evaluator, new_structures_since_last_snag))

    @message("undo-snag-condition")
    def undo_snag_condition(this, self):
        if this.within_snag_period_p is not False:
            vprintf("**** UNDOING SNAG (time ~a) ****~n", setup.g_codelet_count)
            last_snag = tell(self, "get-last-event", "snag")
            this.within_snag_period_p = False
            tell(last_snag, "update-progress-achieved", tell(self, "progress-since-last-snag"))
            _metacat.run.g_temperature_clamped_p = False
            tell(last_snag, "deactivate")
        return "done"

    @message("add-event")
    def add_event(this, self, new_event):
        tell(new_event, "set-event-number", this.next_event_number)
        this.next_event_number = this.next_event_number + 1
        this.event_list = [new_event] + this.event_list
        if tell(new_event, "type?", "clamp") is not False:
            this.within_clamp_period_p = True
            this.last_clamp_time = setup.g_codelet_count
        if tell(new_event, "type?", "snag") is not False:
            this.within_snag_period_p = True
        tell(setup.g_trace_window, "add-event", new_event)
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_temporal_trace():
    """trace.ss: make-temporal-trace"""
    return TemporalTrace()


def clamp_progress_amount_phrase(progress_achieved):
    """trace.ss: clamp-progress-amount-phrase"""
    if progress_achieved == 0:
        return String("zero")
    if progress_achieved < 50:
        return String("very little")
    if progress_achieved < 80:
        return String("some")
    return String("a lot of")


def clamp_progress_adjective_phrase(progress_achieved):
    """trace.ss: clamp-progress-adjective-phrase"""
    if progress_achieved == 0:
        return String("a pretty useless")
    if progress_achieved < 50:
        return String("not such a great")
    if progress_achieved < 80:
        return String("an okay")
    return String("a pretty good")


# ----------------------------------------------------------------------------
# Events
#
# event-type is one of:
# snag, answer, clamp, concept-activation, concept-mapping, rule, group

class GenericEvent(SchemeObject):
    """trace.ss: make-generic-event (the closure)"""
    __slots__ = ("event_type", "event_number", "time_of_occurrence", "temperature",
                 "workspace_structures", "clamped_rules", "active_theme_types",
                 "complete_themespace_patterns", "dominant_themespace_patterns",
                 "normal_graphics_pexp", "highlight_graphics_pexp", "highlighted_p",
                 "bounding_box_x1", "bounding_box_y1", "bounding_box_x2", "bounding_box_y2")

    def __init__(this, event_type):
        this.event_type = event_type
        # the let's bindings only read
        this.event_number = False
        this.time_of_occurrence = setup.g_codelet_count
        this.temperature = setup.g_temperature
        this.workspace_structures = tell(workspace.g_workspace, "get-structures")
        this.clamped_rules = tell(workspace.g_workspace, "get-clamped-rules")
        themespace = _metacat.themes.g_themespace
        this.active_theme_types = tell(themespace, "get-active-theme-types")
        this.complete_themespace_patterns = tell(themespace, "get-all-complete-theme-patterns")
        this.dominant_themespace_patterns = tell(themespace, "get-all-dominant-theme-patterns")
        this.normal_graphics_pexp = False
        this.highlight_graphics_pexp = False
        this.highlighted_p = False
        this.bounding_box_x1 = False
        this.bounding_box_y1 = False
        this.bounding_box_x2 = False
        this.bounding_box_y2 = False

    @message("object-type")
    def object_type(this, self):
        return "generic-event"

    @message("print-name")
    def print_name(this, self):
        return _fmt("Event #~a  Type: ~a  Time: ~a  Temperature: ~a",
                    this.event_number, this.event_type, this.time_of_occurrence,
                    this.temperature)

    @message("print")
    def print_(this, self):
        return sugar.printf("~a~n", tell(self, "print-name"))

    @message("print-themespace-patterns")
    def print_themespace_patterns(this, self):
        sugar.printf("~nComplete Themespace patterns:~n~n")
        for pattern in this.complete_themespace_patterns:
            print_pattern(pattern)
        sugar.printf("~nDominant Themespace patterns:~n~n")
        return sugar.for_star(print_pattern, this.dominant_themespace_patterns)

    @message("type?")
    def type_p(this, self, type_):
        return type_ == "any" or type_ == this.event_type

    @message("set-graphics-pexps")
    def set_graphics_pexps(this, self, normal_pexp, highlight_pexp):
        this.normal_graphics_pexp = normal_pexp
        this.highlight_graphics_pexp = highlight_pexp
        return "done"

    @message("set-bounding-box")
    def set_bounding_box(this, self, x1, y1, x2, y2):
        this.bounding_box_x1 = x1
        this.bounding_box_y1 = y1
        this.bounding_box_x2 = x2
        this.bounding_box_y2 = y2
        return "done"

    @message("within-bounding-box?")
    def within_bounding_box_p(this, self, x, y):
        return (x >= this.bounding_box_x1 and x <= this.bounding_box_x2
                and y >= this.bounding_box_y1 and y <= this.bounding_box_y2)

    @message("get-normal-graphics-pexp")
    def get_normal_graphics_pexp(this, self):
        return this.normal_graphics_pexp

    @message("get-highlight-graphics-pexp")
    def get_highlight_graphics_pexp(this, self):
        return this.highlight_graphics_pexp

    @message("highlighted?")
    def highlighted_p_(this, self):
        return this.highlighted_p

    @message("highlight")
    def highlight(this, self):
        if this.highlighted_p is False:
            this.highlighted_p = True
            tell(setup.g_trace_window, "draw", this.highlight_graphics_pexp)
        return "done"

    @message("unhighlight")
    def unhighlight(this, self):
        if this.highlighted_p is not False:
            this.highlighted_p = False
            tell(setup.g_trace_window, "draw", this.normal_graphics_pexp)
        return "done"

    @message("toggle-highlight")
    def toggle_highlight(this, self):
        return tell(self, "unhighlight" if this.highlighted_p is not False else "highlight")

    @message("set-event-number")
    def set_event_number(this, self, n):
        this.event_number = n
        return "done"

    @message("get-event-number")
    def get_event_number(this, self):
        return this.event_number

    @message("get-type")
    def get_type(this, self):
        return this.event_type

    @message("get-age")
    def get_age(this, self):
        return chez.sub(setup.g_codelet_count, this.time_of_occurrence)

    @message("get-time")
    def get_time(this, self):
        return this.time_of_occurrence

    @message("get-temperature")
    def get_temperature(this, self):
        return this.temperature

    @message("get-structures")
    def get_structures(this, self):
        return this.workspace_structures

    @message("get-active-theme-types")
    def get_active_theme_types(this, self):
        return this.active_theme_types

    @message("get-complete-themespace-patterns")
    def get_complete_themespace_patterns(this, self):
        return this.complete_themespace_patterns

    @message("get-dominant-themespace-patterns")
    def get_dominant_themespace_patterns(this, self):
        return this.dominant_themespace_patterns

    @message("get-complete-themespace-pattern")
    def get_complete_themespace_pattern(this, self, theme_type):
        return chez.assq(theme_type, this.complete_themespace_patterns)

    @message("get-dominant-themespace-pattern")
    def get_dominant_themespace_pattern(this, self, theme_type):
        return chez.assq(theme_type, this.dominant_themespace_patterns)

    @message("display-workspace-state")
    def display_workspace_state(this, self):
        window = setup.g_workspace_window
        tell(window, "draw-codelet-count", this.time_of_occurrence)
        view_globals.g_fg_color = view_globals.p_faded_workspace_structure_color
        tell(window, "draw-all-letters")
        tell(window, "workspace-arrow", "top")
        tell(window, "workspace-arrow", "bottom")
        for g in filter_(group_p, this.workspace_structures):
            tell(window, "display-group", g)
        prev_color = view_globals.p_bridge_label_background_color
        view_globals.p_bridge_label_background_color = \
            view_globals.p_faded_bridge_label_background_color
        for b in filter_(bridge_p, this.workspace_structures):
            tell(window, "display-bridge", b)
        view_globals.p_bridge_label_background_color = prev_color
        for r in this.clamped_rules:
            tell(window, "draw", tell(r, "get-clamped-graphics-pexp"))
        view_globals.g_fg_color = view_globals.p_default_fg_color
        return "done"

    @message("display")
    def display(this, self, *args):
        return chez.error(False, "event has no display method: ~a", tell(self, "print-name"))

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_generic_event(event_type):
    """trace.ss: make-generic-event"""
    return GenericEvent(event_type)


def _blank_themespace():
    """The events' common display preamble for the Themespace."""
    themespace = _metacat.themes.g_themespace
    if tell(themespace, "current-state-displayed?") is not False:
        tell(themespace, "save-current-state")
    tell(themespace, "delete-everything")
    tell(themespace, "thematic-pressure-off")


class AnswerEvent(SchemeObject):
    """trace.ss: make-answer-event (the closure)"""
    __slots__ = ("initial_string", "modified_string", "target_string", "answer_string",
                 "top_rule", "bottom_rule", "supporting_vertical_bridges", "supporting_groups",
                 "top_rule_ref_objects", "bottom_rule_ref_objects", "slippage_log",
                 "unjustified_slippages", "generic_event", "answer_description")

    def __init__(this, initial_string, modified_string, target_string, answer_string,
                 top_rule, bottom_rule, supporting_vertical_bridges, supporting_groups,
                 top_rule_ref_objects, bottom_rule_ref_objects, slippage_log,
                 unjustified_slippages):
        this.initial_string = initial_string
        this.modified_string = modified_string
        this.target_string = target_string
        this.answer_string = answer_string
        this.top_rule = top_rule
        this.bottom_rule = bottom_rule
        this.supporting_vertical_bridges = supporting_vertical_bridges
        this.supporting_groups = supporting_groups
        this.top_rule_ref_objects = top_rule_ref_objects
        this.bottom_rule_ref_objects = bottom_rule_ref_objects
        this.slippage_log = slippage_log
        this.unjustified_slippages = unjustified_slippages
        this.generic_event = make_generic_event("answer")
        this.answer_description = False

    @message("object-type")
    def object_type(this, self):
        return "answer-event"

    @message("print-name")
    def print_name(this, self):
        return _fmt("[Answer ~a]", tell(this.answer_string, "print-name"))

    @message("problem-print-name")
    def problem_print_name(this, self):
        return _fmt("~a => ~a; ~a => ?",
                    tell(this.initial_string, "print-name"),
                    tell(this.modified_string, "print-name"),
                    tell(this.target_string, "print-name"))

    @message("problem-answer-print-name")
    def problem_answer_print_name(this, self):
        return _fmt("~a => ~a; ~a => ~a",
                    tell(this.initial_string, "print-name"),
                    tell(this.modified_string, "print-name"),
                    tell(this.target_string, "print-name"),
                    tell(this.answer_string, "print-name"))

    @message("print")
    def print_(this, self):
        print_(this.generic_event)
        return sugar.printf("Answer \"~a\"  (Quality = ~a)~n",
                            tell(self, "problem-answer-print-name"),
                            tell(self, "get-quality"))

    @message("print-slippages")
    def print_slippages(this, self):
        return print_(this.slippage_log)

    @message("get-initial-string")
    def get_initial_string(this, self):
        return this.initial_string

    @message("get-modified-string")
    def get_modified_string(this, self):
        return this.modified_string

    @message("get-target-string")
    def get_target_string(this, self):
        return this.target_string

    @message("get-answer-string")
    def get_answer_string(this, self):
        return this.answer_string

    @message("get-initial-letters")
    def get_initial_letters(this, self):
        return tell(this.initial_string, "get-letter-categories")

    @message("get-modified-letters")
    def get_modified_letters(this, self):
        return tell(this.modified_string, "get-letter-categories")

    @message("get-target-letters")
    def get_target_letters(this, self):
        return tell(this.target_string, "get-letter-categories")

    @message("get-answer-letters")
    def get_answer_letters(this, self):
        return tell(this.answer_string, "get-letter-categories")

    @message("get-rule")
    def get_rule(this, self, rule_type):
        if rule_type == "top":
            return this.top_rule
        if rule_type == "bottom":
            return this.bottom_rule
        return None       # 1.2: a case without else

    @message("get-supporting-bridges")
    def get_supporting_bridges(this, self, type_):
        if type_ == "top":
            return tell(this.top_rule, "get-supporting-horizontal-bridges")
        if type_ == "vertical":
            return this.supporting_vertical_bridges
        if type_ == "bottom":
            return tell(this.bottom_rule, "get-supporting-horizontal-bridges")
        return None       # 1.2: a case without else

    @message("get-slippage-log")
    def get_slippage_log(this, self):
        return this.slippage_log

    @message("get-supporting-groups")
    def get_supporting_groups(this, self):
        return this.supporting_groups

    @message("get-rule-ref-objects")
    def get_rule_ref_objects(this, self, rule_type):
        if rule_type == "top":
            return this.top_rule_ref_objects
        if rule_type == "bottom":
            return this.bottom_rule_ref_objects
        return None       # 1.2: a case without else

    @message("unjustified?")
    def unjustified_p(this, self):
        return not _null_p(this.unjustified_slippages)

    @message("get-unjustified-slippages")
    def get_unjustified_slippages(this, self):
        return this.unjustified_slippages

    @message("get-answer-description")
    def get_answer_description(this, self):
        return this.answer_description

    @message("set-answer-description")
    def set_answer_description(this, self, answer):
        this.answer_description = answer
        return "done"

    @message("get-absolute-quality")
    def get_absolute_quality(this, self):
        return round_(weighted_average(
            [tell(this.top_rule, "get-quality"), hundred_minus(tell(self, "get-temperature"))],
            [60, 40]))

    @message("get-relative-quality")
    def get_relative_quality(this, self):
        return round_(weighted_average(
            [tell(this.top_rule, "get-relative-quality"),
             hundred_minus(tell(self, "get-temperature"))],
            [60, 40]))

    @message("get-quality")
    def get_quality(this, self):
        return tell(self, "get-absolute-quality")

    @message("get-strength")
    def get_strength(this, self):
        return tell(self, "get-quality")

    @message("equal?")
    def equal_p(this, self, event):
        return tell(event, "type?", "answer")

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        # Display themes:
        _blank_themespace()
        # Display answer-description's theme patterns:
        impose_theme_pattern(tell(this.answer_description, "get-vertical-theme-pattern"))
        impose_theme_pattern(tell(this.answer_description, "get-top-theme-pattern"))
        impose_theme_pattern(tell(this.answer_description, "get-bottom-theme-pattern"))
        # Blank slipnet window:
        tell(setup.g_slipnet_window, "blank-window")
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display answer temperature:
        tell(setup.g_temperature_window, "update-graphics", tell(self, "get-temperature"))
        if setup.p_verbose is not False and tell(self, "unjustified?") is not False:
            sugar.printf("Unjustified slippages:~n")
            return sugar.for_star(print_, this.unjustified_slippages)
        return None

    @message("display-workspace")
    def display_workspace(this, self):
        window = setup.g_workspace_window
        if setup.p_justify_mode is False:
            header = String("Found new answer")
        elif tell(self, "unjustified?") is not False:
            header = String("Settled for unjustified answer")
        else:
            header = String("Justified answer")
        tell(window, "draw-event-header", header, self)
        # Restore Workspace appearance:
        tell(self, "display-workspace-state")
        tell(window, "draw-all-letters")
        if setup.p_justify_mode is False:
            # chez: the extra 'answer tag is ignored by the window's record-case
            # (anomalies: "Chez's record-case ignores extra arguments")
            tell(window, "draw-string-letters", this.answer_string, "answer")
            view_globals.g_fg_color = view_globals.p_faded_workspace_structure_color
            for g in tell(this.answer_string, "get-groups"):
                tell(window, "display-group", g)
            view_globals.g_fg_color = view_globals.p_default_fg_color
        # Highlight vertical mapping:
        tell(window, "highlight-bridges", view_globals.p_vertical_bridge_color,
             tell(self, "get-supporting-bridges", "vertical"))
        # Highlight applied vertical slippages:
        tell(window, "highlight-applied-slippages", this.slippage_log)
        if setup.p_verbose is not False:
            print_(this.slippage_log)
        # Highlight top mapping:
        for obj in this.top_rule_ref_objects:
            tell(window, "display-in-color", obj, view_globals.p_top_bridge_color)
        tell(window, "highlight-bridges", view_globals.p_top_bridge_color,
             tell(self, "get-supporting-bridges", "top"))
        # Highlight bottom mapping:
        for obj in this.bottom_rule_ref_objects:
            tell(window, "display-in-color", obj, view_globals.p_bottom_bridge_color)
        tell(window, "highlight-bridges", view_globals.p_bottom_bridge_color,
             tell(self, "get-supporting-bridges", "bottom"))
        # Display rules:
        tell(window, "display-rule", this.top_rule, view_globals.p_top_rule_color)
        return tell(window, "display-rule", this.bottom_rule, view_globals.p_bottom_rule_color)

    @message("make-answer-description-pexp")
    def make_answer_description_pexp(this, self, theme_bridges, theme_CMs):
        window = setup.g_workspace_window
        tell(window, "caching-on")
        tell(window, "clear")
        tell(window, "draw-header",
             String("Unjustified Answer Description") if tell(self, "unjustified?") is not False
             else String("Answer Description"))
        # chez: the extra tags are ignored by the window's record-case
        tell(window, "draw-string-letters", this.initial_string, "initial")
        tell(window, "workspace-arrow", "top")
        tell(window, "draw-string-letters", this.modified_string, "modified")
        tell(window, "draw-string-letters", this.target_string, "target")
        tell(window, "workspace-arrow", "bottom")
        tell(window, "draw-string-letters", this.answer_string, "answer")
        for g in this.supporting_groups:
            tell(window, "display-group", g)
        for b in theme_bridges:
            object1 = tell(b, "get-object1")
            object2 = tell(b, "get-object2")
            if group_p(object1):
                tell(window, "display-group", object1)
            tell(window, "display-bridge", b)
            if group_p(object2):
                tell(window, "display-group", object2)
        # Highlight vertical mapping:
        tell(window, "highlight-bridges", view_globals.p_vertical_bridge_color,
             tell(self, "get-supporting-bridges", "vertical"))
        # Highlight the concept-mappings supporting the theme pattern and
        # the slippages supporting rule translation:
        for cm in theme_CMs:
            tell(window, "display-in-color", cm,
                 view_globals.p_theme_supporting_concept_mapping_color)
        for s in tell(this.slippage_log, "get-applied-slippages"):
            h = tell(this.slippage_log, "get-slippage-to-highlight", s)
            tell(window, "display-in-color", h, view_globals.p_vertical_slippage_color)
        # Display top mapping:
        for obj in this.top_rule_ref_objects:
            tell(window, "display-in-color", obj, view_globals.p_top_bridge_color)
        tell(window, "highlight-bridges", view_globals.p_top_bridge_color,
             tell(self, "get-supporting-bridges", "top"))
        # Display bottom mapping:
        for obj in this.bottom_rule_ref_objects:
            tell(window, "display-in-color", obj, view_globals.p_bottom_bridge_color)
        tell(window, "highlight-bridges", view_globals.p_bottom_bridge_color,
             tell(self, "get-supporting-bridges", "bottom"))
        # Display rules:
        tell(window, "display-rule", this.top_rule, view_globals.p_top_rule_color)
        tell(window, "display-rule", this.bottom_rule, view_globals.p_bottom_rule_color)
        pexp = tell(window, "get-cached-pexp")
        tell(window, "clear-pending-flush")
        return pexp

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_answer_event(initial_string, modified_string, target_string, answer_string,
                      top_rule, bottom_rule, supporting_vertical_bridges, supporting_groups,
                      top_rule_ref_objects, bottom_rule_ref_objects, slippage_log,
                      unjustified_slippages):
    """trace.ss: make-answer-event"""
    return AnswerEvent(initial_string, modified_string, target_string, answer_string,
                       top_rule, bottom_rule, supporting_vertical_bridges, supporting_groups,
                       top_rule_ref_objects, bottom_rule_ref_objects, slippage_log,
                       unjustified_slippages)


def complement_codelet_pattern():
    """trace.ss: complement-codelet-pattern, read by the clamp event's
    get-complement-codelet-pattern clause and never defined by the original
    (anomalies: "`complement-codelet-pattern` is never defined").  Raises Chez's "variable
    complement-codelet-pattern is not bound"."""
    raise chez.UnboundVariable("complement-codelet-pattern")


class ClampEvent(SchemeObject):
    """trace.ss: make-clamp-event (the closure)"""
    __slots__ = ("clamp_type", "patterns", "rules", "progress_focus", "generic_event",
                 "specified_theme_patterns", "specified_concept_patterns",
                 "specified_codelet_patterns", "theme_related_concept_patterns",
                 "clamped_theme_patterns", "clamped_concept_patterns",
                 "clamped_codelet_patterns", "unifying_slippages", "progress_evaluator",
                 "progress_achieved")

    def __init__(this, clamp_type, patterns, rules_, progress_focus):
        this.clamp_type = clamp_type
        this.patterns = patterns
        this.rules = rules_
        this.progress_focus = progress_focus
        # the let*, in order
        this.generic_event = make_generic_event("clamp")
        this.specified_theme_patterns = filter_(theme_pattern_p, patterns)
        this.specified_concept_patterns = filter_(concept_pattern_p, patterns)
        this.specified_codelet_patterns = filter_(codelet_pattern_p, patterns)
        this.theme_related_concept_patterns = chez.map_(get_associated_concept_pattern,
                                                        this.specified_theme_patterns)
        this.clamped_theme_patterns = this.specified_theme_patterns
        this.clamped_concept_patterns = (this.specified_concept_patterns
                                         + this.theme_related_concept_patterns)
        this.clamped_codelet_patterns = this.specified_codelet_patterns
        if clamp_type == "justify-clamp":
            # the two arguments only read
            this.unifying_slippages = _metacat.justify.get_unifying_slippages(
                select_meth(rules_, "type?", "top"),
                select_meth(rules_, "type?", "bottom"))
        else:
            this.unifying_slippages = False

        def progress_evaluator(event):
            """trace.ss: make-clamp-event's progress-evaluator"""
            if tell(event, "type?", progress_focus) is not False:
                return tell(event, "get-strength")
            return 0
        this.progress_evaluator = progress_evaluator
        this.progress_achieved = 0

    @message("object-type")
    def object_type(this, self):
        return "clamp-event"

    @message("print-name")
    def print_name(this, self):
        return String("[Clamp]")

    @message("print")
    def print_(this, self):
        return sugar.printf("~a (~a)~%", tell(this.generic_event, "print-name"), this.clamp_type)

    @message("print-patterns")
    def print_patterns(this, self):
        # 1.2: a generic event has no print-patterns clause (tell halts)
        tell(this.generic_event, "print-patterns")
        sugar.printf("~nClamped patterns:~n~n")
        for pattern in tell(self, "get-all-clamped-patterns"):
            print_pattern(pattern)
        if not _null_p(this.rules):
            sugar.printf("~nClamped rules:~n~n")
            return sugar.for_star(print_, this.rules)
        return None

    @message("get-clamped-theme-patterns")
    def get_clamped_theme_patterns(this, self):
        return this.clamped_theme_patterns

    @message("get-clamped-concept-patterns")
    def get_clamped_concept_patterns(this, self):
        return this.clamped_concept_patterns

    @message("get-clamped-codelet-patterns")
    def get_clamped_codelet_patterns(this, self):
        return this.clamped_codelet_patterns

    @message("get-theme-related-concept-patterns")
    def get_theme_related_concept_patterns(this, self):
        return this.theme_related_concept_patterns

    @message("get-complement-codelet-pattern")
    def get_complement_codelet_pattern(this, self):
        # 1.2: complement-codelet-pattern is never defined (anomalies_and_quirks.md)
        return complement_codelet_pattern()

    @message("get-all-clamped-patterns")
    def get_all_clamped_patterns(this, self):
        return (this.clamped_theme_patterns + this.clamped_concept_patterns
                + this.clamped_codelet_patterns)

    @message("get-rules")
    def get_rules(this, self):
        return this.rules

    @message("get-rule")
    def get_rule(this, self, rule_type):
        if rule_type == "top":
            return select_meth(this.rules, "type?", "top")
        if rule_type == "bottom":
            return select_meth(this.rules, "type?", "bottom")
        return None       # 1.2: a case without else

    @message("get-unifying-slippages")
    def get_unifying_slippages(this, self):
        return this.unifying_slippages

    @message("get-clamp-type")
    def get_clamp_type(this, self):
        return this.clamp_type

    @message("clamp-type?")
    def clamp_type_p(this, self, type_):
        return type_ == this.clamp_type

    @message("get-progress-focus")
    def get_progress_focus(this, self):
        return this.progress_focus

    @message("get-progress-evaluator")
    def get_progress_evaluator(this, self):
        return this.progress_evaluator

    @message("get-progress-achieved")
    def get_progress_achieved(this, self):
        return this.progress_achieved

    @message("update-progress-achieved")
    def update_progress_achieved(this, self, new_value):
        this.progress_achieved = new_value
        return "done"

    @message("get-strength")
    def get_strength(this, self):
        return 100

    @message("equal?")
    def equal_p(this, self, event):
        if tell(event, "type?", "clamp") is False:
            return False
        if tell(event, "clamp-type?", this.clamp_type) is False:
            return False
        if not chez.eq_p(this.progress_focus, tell(event, "get-progress-focus")):
            return False
        return sets_equal_pred_p(rules.rules_equal_p, this.rules, tell(event, "get-rules"))

    @message("activate")
    def activate(this, self):
        clamp_type = this.clamp_type
        trace = g_trace
        # the add-comment arguments only read
        if clamp_type == "rule-codelet-clamp":
            eliza = [String("I'll just have to try a little harder...")]
        elif clamp_type == "snag-response-clamp":
            eliza = [String("All right, I've had enough of this!  "),
                     String("Let's try something different for a change...")]
        elif clamp_type == "justify-clamp":
            eliza = [_fmt("Aha!  I have ~a idea...",
                          String("another") if tell(trace, "get-num-of-events", "clamp") > 1
                          else String("an"))]
        elif clamp_type == "manual-clamp":
            eliza = [_fmt("Thank you for ~a interesting suggestion!  ",
                          String("another")
                          if tell(trace, "get-num-of-clamps", "manual-clamp") > 1
                          else String("that")),
                     String("Let me think about it...")]
        else:
            eliza = None      # 1.2: a case without else
        if clamp_type == "rule-codelet-clamp":
            plain = String("Clamping rule-codelet pattern...")
        elif clamp_type == "snag-response-clamp":
            plain = String("Clamping negative theme pattern...")
        elif clamp_type == "justify-clamp":
            plain = String("Clamping theme patterns...")
        elif clamp_type == "manual-clamp":
            plain = String("Manually clamping patterns...")
        else:
            plain = None      # 1.2: a case without else
        tell(setup.g_comment_window, "add-comment", eliza, [plain])
        tell(trace, "undo-snag-condition")
        # Rules
        if not _null_p(this.rules):
            for rule in this.rules:
                tell(workspace.g_workspace, "clamp-rule", rule)
        # Theme-patterns
        if not _null_p(this.clamped_theme_patterns):
            for pattern in this.clamped_theme_patterns:
                clamp_theme_pattern(pattern)
        # Concept-patterns
        if not _null_p(this.clamped_concept_patterns):
            for pattern in this.clamped_concept_patterns:
                clamp_concept_pattern(pattern)
            if setup.p_slipnet_graphics is not False:
                tell(setup.g_slipnet_window, "update-graphics")
        # Codelet-patterns
        if not _null_p(this.clamped_codelet_patterns):
            for pattern in this.clamped_codelet_patterns:
                clamp_codelet_pattern(pattern)
            if setup.p_coderack_graphics is not False:
                tell(setup.g_coderack_window, "update-graphics")
        return "done"

    @message("deactivate")
    def deactivate(this, self):
        # Rules
        if not _null_p(this.rules):
            for rule in this.rules:
                tell(workspace.g_workspace, "unclamp-rule", rule)
        # Theme-patterns
        if not _null_p(this.clamped_theme_patterns):
            for pattern in this.clamped_theme_patterns:
                unclamp_theme_pattern(pattern)
        # Concept-patterns
        if not _null_p(this.clamped_concept_patterns):
            for pattern in this.clamped_concept_patterns:
                unclamp_concept_pattern(pattern)
            if setup.p_slipnet_graphics is not False:
                tell(setup.g_slipnet_window, "update-graphics")
        # Codelet-patterns
        if not _null_p(this.clamped_codelet_patterns):
            for pattern in this.clamped_codelet_patterns:
                unclamp_codelet_pattern(pattern)
            if setup.p_coderack_graphics is not False:
                tell(setup.g_coderack_window, "delete", "urgency")
                if setup.p_codelet_count_graphics is not False:
                    tell(setup.g_coderack_window, "delete", "count")
                tell(setup.g_coderack_window, "update-graphics")
        return "done"

    @message("display")
    def display(this, self):
        vprintf("Progress achieved by clamp = ~a~n", this.progress_achieved)
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        # Display clamped themes:
        _blank_themespace()
        for pattern in this.clamped_theme_patterns:
            impose_theme_pattern(pattern)
            tell(_metacat.themes.g_themespace, "thematic-pressure-on", first(pattern))
        # Display concept-patterns:
        tell(setup.g_slipnet_window, "display-patterns",
             this.clamped_concept_patterns,
             view_globals.p_clamp_event_concept_pattern_color,
             String("Concept Pattern"))
        # Display codelet patterns:
        tell(setup.g_coderack_window, "display-patterns",
             this.clamped_codelet_patterns, String("Codelet Pattern"))
        # Display temperature:
        return tell(setup.g_temperature_window, "update-graphics", tell(self, "get-temperature"))

    @message("display-workspace")
    def display_workspace(this, self):
        tell(setup.g_workspace_window, "draw-event-header",
             String("Manually clamped patterns") if this.clamp_type == "manual-clamp"
             else String("Clamped patterns"),
             self)
        # Restore Workspace appearance:
        tell(self, "display-workspace-state")
        # Display clamped rules:
        return sugar.for_star(
            lambda rule: tell(setup.g_workspace_window, "draw",
                              tell(rule, "get-clamped-graphics-pexp")),
            this.rules)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_clamp_event(clamp_type, patterns, rules_, progress_focus):
    """trace.ss: make-clamp-event"""
    return ClampEvent(clamp_type, patterns, rules_, progress_focus)


class ConceptActivationEvent(SchemeObject):
    """trace.ss: make-concept-activation-event (the closure)"""
    __slots__ = ("slipnode", "generic_event", "print_name_", "concept_pattern")

    def __init__(this, slipnode):
        this.slipnode = slipnode
        # the let's bindings: only the first has effects (it reads the state)
        this.generic_event = make_generic_event("concept-activation")
        this.print_name_ = _fmt("(~a)", tell(slipnode, "get-short-name"))
        this.concept_pattern = ["concepts", [slipnode, slipnet.p_max_activation]]

    @message("object-type")
    def object_type(this, self):
        return "concept-activation-event"

    @message("print-name")
    def print_name(this, self):
        return this.print_name_

    @message("print")
    def print_(this, self):
        print_(this.generic_event)
        return sugar.printf("Concept: ~a~n", full_slipnode_name(this.slipnode))

    @message("get-slipnode")
    def get_slipnode(this, self):
        return this.slipnode

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        return this.concept_pattern

    @message("get-strength")
    def get_strength(this, self):
        return tell(this.slipnode, "get-conceptual-depth")

    @message("equal?")
    def equal_p(this, self, event):
        return (tell(event, "type?", "concept-activation") is not False
                and tell(event, "get-slipnode") is this.slipnode)

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        # Blank themespace window:
        _blank_themespace()
        # Display concept pattern:
        tell(setup.g_slipnet_window, "display-patterns",
             [this.concept_pattern],
             view_globals.p_concept_activation_event_concept_pattern_color,
             String("Concept Activation"))
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display temperature:
        return tell(setup.g_temperature_window, "update-graphics", tell(self, "get-temperature"))

    @message("display-workspace")
    def display_workspace(this, self):
        tell(setup.g_workspace_window, "draw-event-header",
             _fmt("Activation of ~a concept", full_slipnode_name(this.slipnode)), self)
        # Restore Workspace appearance:
        return tell(self, "display-workspace-state")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_concept_activation_event(slipnode):
    """trace.ss: make-concept-activation-event"""
    return ConceptActivationEvent(slipnode)


class ConceptMappingEvent(SchemeObject):
    """trace.ss: make-concept-mapping-event (the closure)"""
    __slots__ = ("cm", "bridge", "generic_event", "bridge_type", "print_name_", "slippage_p",
                 "theme_pattern", "cm_strength", "concept_pattern")

    def __init__(this, cm, bridge):
        this.cm = cm
        this.bridge = bridge
        # the let*, in order
        this.generic_event = make_generic_event("concept-mapping")
        this.bridge_type = tell(bridge, "get-bridge-type")
        bridge_type = this.bridge_type
        if bridge_type == "top":
            letter = String("T")
        elif bridge_type == "vertical":
            letter = String("V")
        elif bridge_type == "bottom":
            letter = String("B")
        else:
            letter = None     # 1.2: a case without else
        this.print_name_ = _fmt("~a:~a", letter, tell(cm, "print-name"))
        this.slippage_p = tell(cm, "slippage?")
        this.theme_pattern = [
            _metacat.themes.bridge_type_to_theme_type(tell(bridge, "get-bridge-type")),
            [tell(cm, "get-CM-type"), tell(cm, "get-label")]]
        this.cm_strength = tell(cm, "get-strength")
        this.concept_pattern = tell(cm, "get-concept-pattern")

    @message("object-type")
    def object_type(this, self):
        return "concept-mapping-event"

    @message("print-name")
    def print_name(this, self):
        return this.print_name_

    @message("print")
    def print_(this, self):
        print_(this.generic_event)
        return sugar.printf("~a ~a~n",
                            String("Slippage") if this.slippage_p is not False
                            else String("Concept-mapping"),
                            tell(this.cm, "print-name"))

    @message("type?")
    def type_p(this, self, type_):
        return type_ == "workspace" or tell(this.generic_event, "type?", type_)

    @message("CM-type?")
    def CM_type_p(this, self, type_):
        return chez.eq_p(type_, tell(this.cm, "get-CM-type"))

    @message("bridge-type?")
    def bridge_type_p(this, self, type_):
        return type_ == this.bridge_type

    @message("relevant-for-answer-description?")
    def relevant_for_answer_description_p(this, self):
        return this.bridge_type == "vertical" and tell(self, "currently-present?")

    @message("get-concept-mapping")
    def get_concept_mapping(this, self):
        return this.cm

    @message("get-CM-type")
    def get_CM_type(this, self):
        return tell(this.cm, "get-CM-type")

    @message("get-bridge")
    def get_bridge(this, self):
        return this.bridge

    @message("currently-present?")
    def currently_present_p(this, self):
        return tell(workspace.g_workspace, "bridge-present?", this.bridge)

    @message("get-equivalent-bridge")
    def get_equivalent_bridge(this, self):
        return tell(workspace.g_workspace, "get-equivalent-bridge", this.bridge)

    @message("get-bridge-type")
    def get_bridge_type(this, self):
        return this.bridge_type

    @message("get-theme-pattern")
    def get_theme_pattern(this, self):
        return this.theme_pattern

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        return this.concept_pattern

    @message("get-strength")
    def get_strength(this, self):
        return this.cm_strength

    @message("slippage?")
    def slippage_p_(this, self):
        return this.slippage_p

    @message("equal?")
    def equal_p(this, self, event):
        return (tell(event, "type?", "concept-mapping") is not False
                and concept_mappings.CMs_equal_p(this.cm, tell(event, "get-concept-mapping")))

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        _blank_themespace()
        impose_theme_pattern(this.theme_pattern)
        # Display concept-mapping concepts:
        tell(setup.g_slipnet_window, "display-patterns",
             [this.concept_pattern],
             view_globals.p_concept_mapping_event_concept_pattern_color,
             _fmt("~a Concepts", String("Slippage") if this.slippage_p is not False
                  else String("Concept-mapping")))
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display temperature:
        return tell(setup.g_temperature_window, "update-graphics", tell(self, "get-temperature"))

    @message("display-workspace")
    def display_workspace(this, self):
        window = setup.g_workspace_window
        tell(window, "draw-event-header",
             _fmt("Made ~a ~a", tell(this.cm, "print-name"),
                  String("slippage") if this.slippage_p is not False
                  else String("concept-mapping")),
             self)
        # Restore Workspace appearance:
        tell(self, "display-workspace-state")
        # Highlight bridge and concept-mapping (if possible):
        tell(window, "highlight-bridges",
             view_globals.p_workspace_event_structure_color, [this.bridge])
        if tell(this.bridge, "concept-mapping-graphics-active?") is not False:
            return tell(window, "display-in-color",
                        this.cm, view_globals.p_workspace_event_structure_color)
        return None

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_concept_mapping_event(cm, bridge):
    """trace.ss: make-concept-mapping-event"""
    return ConceptMappingEvent(cm, bridge)


class GroupEvent(SchemeObject):
    """trace.ss: make-group-event (the closure)"""
    __slots__ = ("group", "flipped_p", "generic_event", "print_name_", "group_category",
                 "direction", "group_strength", "concept_pattern")

    def __init__(this, group, flipped_p):
        this.group = group
        this.flipped_p = flipped_p
        # the let's bindings: the first reads the state, the others only read
        this.generic_event = make_generic_event("group")
        text = _metacat.trace_graphics.group_event_pexp_text_string(group)
        direction = tell(group, "get-direction")
        if direction is slipnet.plato_right:
            this.print_name_ = _fmt(">~a>", text)
        elif direction is slipnet.plato_left:
            this.print_name_ = _fmt("<~a<", text)
        else:
            this.print_name_ = _fmt("[~a]", text)
        this.group_category = tell(group, "get-group-category")
        this.direction = tell(group, "get-direction")
        this.group_strength = tell(group, "get-strength")
        this.concept_pattern = tell(group, "get-concept-pattern")

    @message("object-type")
    def object_type(this, self):
        return "group-event"

    @message("print-name")
    def print_name(this, self):
        return this.print_name_

    @message("print")
    def print_(this, self):
        print_(this.generic_event)
        return print_(this.group)

    @message("type?")
    def type_p(this, self, type_):
        return type_ == "workspace" or tell(this.generic_event, "type?", type_)

    @message("relevant-for-answer-description?")
    def relevant_for_answer_description_p(this, self):
        return _and(tell(self, "spanning?"), lambda: tell(self, "currently-present?"))

    @message("spanning?")
    def spanning_p(this, self):
        return tell(this.group, "spans-whole-string?")

    @message("string-type?")
    def string_type_p(this, self, string_type):
        return chez.eq_p(tell(this.group, "which-string"), string_type)

    # (the record-case has a second get-string-type clause, identical and never reached)
    @message("get-string-type")
    def get_string_type(this, self):
        return tell(this.group, "which-string")

    @message("spans?")
    def spans_p(this, self, string_type):
        return _and(tell(self, "spanning?"), lambda: tell(self, "string-type?", string_type))

    @message("flipped?")
    def flipped_p_(this, self):
        return this.flipped_p

    @message("get-group")
    def get_group(this, self):
        return this.group

    @message("currently-present?")
    def currently_present_p(this, self):
        return tell(tell(this.group, "get-string"), "group-present?", this.group)

    @message("get-equivalent-group")
    def get_equivalent_group(this, self):
        return tell(tell(this.group, "get-string"), "get-equivalent-group", this.group)

    @message("get-group-category")
    def get_group_category(this, self):
        return this.group_category

    @message("get-direction")
    def get_direction(this, self):
        return this.direction

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        return this.concept_pattern

    @message("get-strength")
    def get_strength(this, self):
        return this.group_strength

    @message("equal?")
    def equal_p(this, self, event):
        return (tell(event, "type?", "group") is not False
                and equivalent_workspace_objects_p(this.group, tell(event, "get-group")))

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        # Blank themespace window:
        _blank_themespace()
        # Display concepts in group's descriptions:
        tell(setup.g_slipnet_window, "display-patterns",
             [this.concept_pattern],
             view_globals.p_group_event_concept_pattern_color,
             String("Group Descriptors"))
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display temperature:
        return tell(setup.g_temperature_window, "update-graphics", tell(self, "get-temperature"))

    @message("display-workspace")
    def display_workspace(this, self):
        if this.flipped_p is not False:
            header = _fmt("Flipped ~a", unflipped_group_name(this.group))
        else:
            header = _fmt("Built ~a", full_workspace_object_name(this.group))
        tell(setup.g_workspace_window, "draw-event-header", header, self)
        # Restore Workspace appearance:
        tell(self, "display-workspace-state")
        # Highlight group:
        return tell(setup.g_workspace_window, "display-in-color", this.group,
                    view_globals.p_workspace_event_structure_color)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_group_event(group, flipped_p):
    """trace.ss: make-group-event"""
    return GroupEvent(group, flipped_p)


def equivalent_workspace_objects_p(object1, object2):
    """trace.ss: equivalent-workspace-objects?"""
    if tell(object1, "object-type") != tell(object2, "object-type"):
        return False
    if not chez.eq_p(tell(object1, "which-string"), tell(object2, "which-string")):
        return False
    if tell(object1, "get-left-string-pos") != tell(object2, "get-left-string-pos"):
        return False
    if tell(object1, "get-right-string-pos") != tell(object2, "get-right-string-pos"):
        return False
    if letter_p(object1):
        return groups.same_letter_category_p(object1, object2)
    return (groups.same_group_category_p(object1, object2)
            and groups.same_group_direction_p(object1, object2)
            and tell(object1, "get-group-length") == tell(object2, "get-group-length")
            and chez.andmap(equivalent_workspace_objects_p,
                            tell(object1, "get-constituent-objects"),
                            tell(object2, "get-constituent-objects")))


def _rule_type_case(rule_type, top, bottom):
    """(case rule-type (top top) (bottom bottom)), void otherwise."""
    if rule_type == "top":
        return top
    if rule_type == "bottom":
        return bottom
    return None           # 1.2: a case without else


class RuleEvent(SchemeObject):
    """trace.ss: make-rule-event (the closure)"""
    __slots__ = ("rule", "generic_event", "rule_type", "print_name_", "supporting_bridges",
                 "reference_objects", "relative_quality", "concept_pattern")

    def __init__(this, rule):
        this.rule = rule
        # the let*, in order
        this.generic_event = make_generic_event("rule")
        this.rule_type = tell(rule, "get-rule-type")
        rule_type = this.rule_type
        this.print_name_ = _rule_type_case(rule_type, String("[Top Rule]"),
                                           String("[Bottom Rule]"))
        this.supporting_bridges = tell(rule, "get-supporting-horizontal-bridges")
        if rule_type == "top":
            objects = tell(workspace.g_initial_string, "get-all-reference-objects", rule)
        elif rule_type == "bottom":
            objects = tell(workspace.g_target_string, "get-all-reference-objects", rule)
        else:
            objects = None    # 1.2: a case without else
        this.reference_objects = filter_out(workspace_string_p, objects)
        this.relative_quality = tell(rule, "get-relative-quality")
        this.concept_pattern = tell(rule, "get-concept-pattern")

    @message("object-type")
    def object_type(this, self):
        return "rule-event"

    @message("print-name")
    def print_name(this, self):
        return this.print_name_

    @message("print")
    def print_(this, self):
        print_(this.generic_event)
        return print_(this.rule)

    @message("type?")
    def type_p(this, self, type_):
        return type_ == "workspace" or tell(this.generic_event, "type?", type_)

    @message("get-rule")
    def get_rule(this, self):
        return this.rule

    @message("get-rule-type")
    def get_rule_type(this, self):
        return this.rule_type

    @message("get-supporting-bridges")
    def get_supporting_bridges(this, self):
        return this.supporting_bridges

    @message("get-reference-objects")
    def get_reference_objects(this, self):
        return this.reference_objects

    @message("get-relative-quality")
    def get_relative_quality(this, self):
        return this.relative_quality

    @message("get-concept-pattern")
    def get_concept_pattern(this, self):
        return this.concept_pattern

    @message("get-strength")
    def get_strength(this, self):
        return this.relative_quality

    @message("equal?")
    def equal_p(this, self, event):
        return (tell(event, "type?", "rule") is not False
                and tell(this.rule, "equal?", tell(event, "get-rule")))

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        # Display rule's theme-pattern:
        _blank_themespace()
        impose_theme_pattern(tell(this.rule, "get-theme-pattern"))
        # Display rule concepts:
        tell(setup.g_slipnet_window, "display-patterns",
             [this.concept_pattern],
             _rule_type_case(this.rule_type,
                             view_globals.p_top_rule_event_concept_pattern_color,
                             view_globals.p_bottom_rule_event_concept_pattern_color),
             _rule_type_case(this.rule_type, String("Top Rule Concepts"),
                             String("Bottom Rule Concepts")))
        if setup.p_verbose is not False:
            # Display relative quality:
            sugar.printf("~n----------------------------------------------------------~n")
            print_(this.rule)
            tell(this.rule, "show")
            sugar.printf("Quality = ~a~n", tell(this.rule, "get-quality"))
            sugar.printf("Relative quality at creation = ~a~n", this.relative_quality)
            sugar.printf("Relative quality now = ~a", tell(this.rule, "get-relative-quality"))
            sugar.printf("~n----------------------------------------------------------~n")
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display temperature:
        return tell(setup.g_temperature_window, "update-graphics", tell(self, "get-temperature"))

    @message("display-workspace")
    def display_workspace(this, self):
        window = setup.g_workspace_window
        tell(window, "draw-event-header",
             _fmt("Built ~arule", String("translated ") if tell(this.rule, "translated?")
                  is not False else String("")),
             self)
        # Restore appropriate mapping:
        tell(self, "display-workspace-state")
        if tell(this.rule, "translated?") is not False:
            tell(window, "display-rule", tell(this.rule, "get-original-rule"),
                 view_globals.p_faded_workspace_structure_color)
        # Highlight reference objects:
        for obj in this.reference_objects:
            tell(window, "display-in-color", obj,
                 _rule_type_case(this.rule_type, view_globals.p_top_bridge_color,
                                 view_globals.p_bottom_bridge_color))
        # Highlight supporting bridges:
        tell(window, "highlight-bridges",
             _rule_type_case(this.rule_type, view_globals.p_top_bridge_color,
                             view_globals.p_bottom_bridge_color),
             this.supporting_bridges)
        return tell(window, "display-rule", this.rule,
                    _rule_type_case(this.rule_type, view_globals.p_top_rule_color,
                                    view_globals.p_bottom_rule_color))

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_rule_event(rule):
    """trace.ss: make-rule-event"""
    return RuleEvent(rule)


def get_snag_theme_pattern(snag_concept_mappings):
    """trace.ss: get-snag-theme-pattern"""
    pattern_entries = remove_duplicates(
        chez.map_(lambda cm: [tell(cm, "get-CM-type"), tell(cm, "get-label")],
                  snag_concept_mappings))
    return ["vertical-bridge"] + pattern_entries


def get_snag_concept_pattern(snag_objects):
    """trace.ss: get-snag-concept-pattern"""
    return ["concepts"] + chez.map_(
        lambda descriptor: [descriptor, slipnet.p_max_activation],
        remq_duplicates(_append_all(tell_all(snag_objects, "get-all-descriptors"))))


class SnagEvent(SchemeObject):
    """trace.ss: make-snag-event (the closure)"""
    __slots__ = ("failure_result", "rule", "translated_rule", "supporting_vertical_bridges",
                 "slippage_log", "rule_ref_objects", "generic_event", "snag_type",
                 "snag_objects", "snag_bridges", "snag_concept_mappings", "snag_theme_pattern",
                 "snag_concept_pattern", "progress_evaluator", "progress_achieved")

    def __init__(this, failure_result, rule, translated_rule, supporting_vertical_bridges,
                 slippage_log, rule_ref_objects):
        this.failure_result = failure_result
        this.rule = rule
        this.translated_rule = translated_rule
        this.supporting_vertical_bridges = supporting_vertical_bridges
        this.slippage_log = slippage_log
        this.rule_ref_objects = rule_ref_objects
        # the let*, in order
        this.generic_event = make_generic_event("snag")
        this.snag_type = first(failure_result)
        key = failure_result[0]
        if key == "SWAP":
            objects, _dim = _record_case_args(failure_result, 2)
            this.snag_objects = objects
        elif key == "CONFLICT":
            object1, _dim1, object2, _dim2 = _record_case_args(failure_result, 4)
            this.snag_objects = [object1, object2]
        elif key == "CHANGE":
            object_, _transform = _record_case_args(failure_result, 2)
            this.snag_objects = [object_]
        else:
            this.snag_objects = None      # 1.2: a record-case without else
        this.snag_bridges = compress(tell_all(this.snag_objects, "get-bridge", "vertical"))
        if _null_p(this.snag_bridges):
            this.snag_concept_mappings = tell(workspace.g_workspace, "get-all-vertical-CMs")
        else:
            this.snag_concept_mappings = _append_all(
                tell_all(this.snag_bridges, "get-all-concept-mappings"))
        this.snag_theme_pattern = get_snag_theme_pattern(this.snag_concept_mappings)
        this.snag_concept_pattern = get_snag_concept_pattern(this.snag_objects)

        def progress_evaluator(workspace_structure):
            """trace.ss: make-snag-event's progress-evaluator"""
            if not bond_p(workspace_structure):
                return tell(workspace_structure, "get-strength")
            return 0
        this.progress_evaluator = progress_evaluator
        this.progress_achieved = 0

    @message("object-type")
    def object_type(this, self):
        return "snag-event"

    @message("print-name")
    def print_name(this, self):
        return String("[Snag]")

    @message("print")
    def print_(this, self):
        print_(this.generic_event)
        # 1.2: the failure results are tagged SWAP, CONFLICT and CHANGE, and Chez
        # 10 is case-sensitive, so this case never matches (void is printed) and
        # the plural "s" is always used
        if this.snag_type == "swap":
            kind = String("Swap")
        elif this.snag_type == "conflict":
            kind = String("Conflict")
        elif this.snag_type == "change":
            kind = String("Change")
        else:
            kind = None
        sugar.printf("~a-snag involving object~a:~n", kind,
                     String("") if this.snag_type == "change" else String("s"))
        return sugar.for_star(print_, this.snag_objects)

    @message("print-patterns")
    def print_patterns(this, self):
        sugar.printf("Snag theme pattern:~n")
        print_pattern(this.snag_theme_pattern)
        # Rule thematic pattern was recorded at the time of rule creation.
        # translated-rule failed, so it has no theme-pattern:
        sugar.printf("Rule thematic pattern:~n")
        return print_pattern(tell(this.rule, "get-theme-pattern"))

    @message("get-explanation")
    def get_explanation(this, self):
        failure_result = this.failure_result
        key = failure_result[0]
        if key == "SWAP":
            objects, dimension = _record_case_args(failure_result, 2)
            return _fmt("no ~a swap is possible between ~a in ~a",
                        tell(dimension, "get-lowercase-name"),
                        rules.punctuate(chez.map_(snag_object_phrase, objects)),
                        tell(tell(first(objects), "get-string"), "print-name"))
        if key == "CONFLICT":
            object1, dimension1, object2, dimension2 = _record_case_args(failure_result, 4)
            return _fmt("changing ~a of ~a conflicts with changing ~a of ~a in ~a",
                        _fmt("the ~a", tell(dimension1, "get-lowercase-name")),
                        snag_object_phrase(object1),
                        _fmt("the ~a", tell(dimension2, "get-lowercase-name")),
                        snag_object_phrase(object2),
                        tell(tell(object1, "get-string"), "print-name"))
        if key == "CHANGE":
            object_, transform = _record_case_args(failure_result, 2)
            string = tell(object_, "get-string")
            if first(transform) is slipnet.plato_group_category:
                return _fmt("reversing the starting and ending ~a of ~a ~a",
                            tell(third(transform), "get-lowercase-name"),
                            snag_object_phrase(object_),
                            _fmt("is not possible in ~a", tell(string, "print-name")))
            if slipnet.platonic_relation_p(second(transform)):
                target = _fmt("its ~a", tell(second(transform), "get-lowercase-name"))
            else:
                target = _fmt("`~a'", tell(second(transform), "get-lowercase-name"))
            return _fmt("changing the ~a of ~a to ~a is not possible in ~a",
                        tell(first(transform), "get-lowercase-name"),
                        snag_object_phrase(object_),
                        target,
                        tell(string, "print-name"))
        return None       # 1.2: a record-case without else

    @message("get-failure-result")
    def get_failure_result(this, self):
        return this.failure_result

    @message("get-rule")
    def get_rule(this, self, rule_type):
        return _rule_type_case(rule_type, this.rule, this.translated_rule)

    @message("get-supporting-bridges")
    def get_supporting_bridges(this, self, bridge_type):
        # Snags have no supporting bottom bridges since translated-rule failed
        if bridge_type == "top":
            return tell(this.rule, "get-supporting-horizontal-bridges")
        if bridge_type == "vertical":
            return this.supporting_vertical_bridges
        return None       # 1.2: a case without else

    @message("get-slippage-log")
    def get_slippage_log(this, self):
        return this.slippage_log

    @message("get-rule-ref-objects")
    def get_rule_ref_objects(this, self):
        return this.rule_ref_objects

    @message("get-snag-type")
    def get_snag_type(this, self):
        return this.snag_type

    @message("get-snag-objects")
    def get_snag_objects(this, self):
        return this.snag_objects

    @message("get-snag-bridges")
    def get_snag_bridges(this, self):
        return this.snag_bridges

    @message("get-snag-concept-mappings")
    def get_snag_concept_mappings(this, self):
        return this.snag_concept_mappings

    @message("get-snag-theme-pattern")
    def get_snag_theme_pattern(this, self):
        return this.snag_theme_pattern

    @message("get-snag-concept-pattern")
    def get_snag_concept_pattern(this, self):
        return this.snag_concept_pattern

    @message("get-progress-evaluator")
    def get_progress_evaluator(this, self):
        return this.progress_evaluator

    @message("get-progress-achieved")
    def get_progress_achieved(this, self):
        return this.progress_achieved

    @message("update-progress-achieved")
    def update_progress_achieved(this, self, new_value):
        this.progress_achieved = new_value
        return "done"

    @message("get-strength")
    def get_strength(this, self):
        return 100

    @message("equal?")
    def equal_p(this, self, event):
        if tell(event, "type?", "snag") is False:
            return False
        if tell(this.translated_rule, "equal?", tell(event, "get-rule", "bottom")) is False:
            return False
        return sets_equal_pred_p(equivalent_workspace_objects_p,
                                 this.snag_objects, tell(event, "get-snag-objects"))

    @message("activate")
    def activate(this, self):
        tell(g_trace, "undo-last-clamp")
        for obj in this.snag_objects:
            tell(obj, "clamp-salience")
        clamp_concept_pattern(this.snag_concept_pattern)
        if setup.p_slipnet_graphics is not False:
            tell(setup.g_slipnet_window, "update-graphics")
        return "done"

    @message("deactivate")
    def deactivate(this, self):
        for obj in this.snag_objects:
            tell(obj, "unclamp-salience")
        unclamp_concept_pattern(this.snag_concept_pattern)
        if setup.p_slipnet_graphics is not False:
            tell(setup.g_slipnet_window, "update-graphics")
        return "done"

    @message("display")
    def display(this, self):
        vprintf("Progress achieved by snag = ~a~n", this.progress_achieved)
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "clear")
        tell(self, "display-workspace")
        # Display snag theme pattern:
        _blank_themespace()
        impose_theme_pattern(this.snag_theme_pattern)
        # Display concept pattern:
        tell(setup.g_slipnet_window, "display-patterns",
             [this.snag_concept_pattern],
             view_globals.p_snag_event_concept_pattern_color,
             String("Snag Object Descriptors"))
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display maximum temperature:
        return tell(setup.g_temperature_window, "update-graphics", 100)

    def _draw_snag(this, self, window):
        """The common part of display-workspace and make-snag-description-pexp,
        from Highlight top mapping on."""
        # Highlight top mapping:
        for obj in this.rule_ref_objects:
            tell(window, "display-in-color", obj, view_globals.p_top_bridge_color)
        tell(window, "highlight-bridges", view_globals.p_top_bridge_color,
             tell(self, "get-supporting-bridges", "top"))
        # Highlight snag objects:
        for obj in this.snag_objects:
            if not workspace_string_p(obj):
                tell(window, "display-in-color", obj, view_globals.p_snag_color)
        # Display rules:
        tell(window, "display-rule", this.rule, view_globals.p_top_rule_color)
        tell(window, "display-rule", this.translated_rule, view_globals.p_snag_color)
        return tell(window, "question-mark", "draw", String("???"), view_globals.p_snag_color)

    @message("display-workspace")
    def display_workspace(this, self):
        window = setup.g_workspace_window
        tell(window, "draw-event-header", String("Snag"), self)
        # Restore Workspace appearance:
        tell(self, "display-workspace-state")
        # Highlight vertical mapping:
        tell(window, "highlight-bridges", view_globals.p_vertical_bridge_color,
             tell(self, "get-supporting-bridges", "vertical"))
        # Highlight applied vertical slippages:
        tell(window, "highlight-applied-slippages", this.slippage_log)
        return this._draw_snag(self, window)

    @message("make-snag-description-pexp")
    def make_snag_description_pexp(this, self, theme_bridges, theme_CMs):
        window = setup.g_workspace_window
        tell(window, "caching-on")
        tell(window, "clear")
        tell(window, "draw-header", String("Snag Description"))
        tell(window, "draw-all-letters")
        tell(window, "workspace-arrow", "top")
        tell(window, "workspace-arrow", "bottom")
        for b in theme_bridges:
            object1 = tell(b, "get-object1")
            object2 = tell(b, "get-object2")
            if group_p(object1):
                tell(window, "display-group", object1)
            tell(window, "display-bridge", b)
            if group_p(object2):
                tell(window, "display-group", object2)
        # Highlight vertical mapping:
        tell(window, "highlight-bridges", view_globals.p_vertical_bridge_color,
             tell(self, "get-supporting-bridges", "vertical"))
        # Highlight the concept-mappings supporting the theme pattern and
        # the slippages supporting rule translation:
        for cm in theme_CMs:
            tell(window, "display-in-color", cm,
                 view_globals.p_theme_supporting_concept_mapping_color)
        for s in tell(this.slippage_log, "get-applied-slippages"):
            h = tell(this.slippage_log, "get-slippage-to-highlight", s)
            tell(window, "display-in-color", h, view_globals.p_vertical_slippage_color)
        # Display top mapping, snag objects and rules:
        this._draw_snag(self, window)
        pexp = tell(window, "get-cached-pexp")
        tell(window, "clear-pending-flush")
        return pexp

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.generic_event)


def make_snag_event(failure_result, rule, translated_rule, supporting_vertical_bridges,
                    slippage_log, rule_ref_objects):
    """trace.ss: make-snag-event"""
    return SnagEvent(failure_result, rule, translated_rule, supporting_vertical_bridges,
                     slippage_log, rule_ref_objects)


def snag_object_phrase(object_):
    """trace.ss: snag-object-phrase"""
    if workspace_string_p(object_):
        return _fmt("the string ~a", quoted_string(object_))
    if letter_p(object_):
        return _fmt("the letter ~a", tell(object_, "print-name"))
    if group_p(object_):
        return _fmt("the ~a group",
                    _string_append(tell_all(tell(object_, "get-letters"), "print-name")))
    return None           # 1.2: a cond without else


_FULL_SLIPNODE_NAMES = [
    ("plato_alphabetic_position_category", "Alphabetic-Position"),
    ("plato_bond_facet", "Bond-Facet"),
    ("plato_object_category", "Object-Category"),
    ("plato_letter_category", "Letter-Category"),
    ("plato_length", "Length"),
    ("plato_bond_category", "Bond-Category"),
    ("plato_group_category", "Group-Category"),
    ("plato_direction_category", "Direction"),
    ("plato_string_position_category", "String-Position"),
    ("plato_opposite", "Opposite"),
    ("plato_identity", "Identity"),
    ("plato_predgrp", "predecessor-group"),
    ("plato_succgrp", "successor-group"),
    ("plato_samegrp", "sameness-group"),
]


def full_slipnode_name(slipnode):
    """trace.ss: full-slipnode-name"""
    for attr, name in _FULL_SLIPNODE_NAMES:
        if slipnode is getattr(slipnet, attr):
            return String(name)
    return tell(slipnode, "get-lowercase-name")


def unflipped_group_name(group):
    """trace.ss: unflipped-group-name"""
    if tell(group, "get-group-category") is slipnet.plato_predgrp:
        return String("successor-group")
    if tell(group, "get-group-category") is slipnet.plato_succgrp:
        return String("predecessor-group")
    return None           # 1.2: a cond without else


def full_workspace_object_name(object_):
    """trace.ss: full-workspace-object-name"""
    if letter_p(object_):
        return String("letter")
    if tell(object_, "get-group-category") is slipnet.plato_samegrp:
        return String("sameness-group")
    if tell(object_, "get-group-category") is slipnet.plato_predgrp:
        return String("predecessor-group")
    if tell(object_, "get-group-category") is slipnet.plato_succgrp:
        return String("successor-group")
    return None           # 1.2: a cond without else


# ----------------------------------------------------------------------------
# Workspace and Slipnet event monitoring

p_concept_mapping_importance_threshold = 65
p_concept_activation_importance_threshold = 85
p_group_importance_threshold = 100
p_rule_importance_threshold = 67


def monitor_slipnode_activation_change(slipnode, previous_activation, new_activation):
    """trace.ss: monitor-slipnode-activation-change"""
    importance = concept_activation_importance(slipnode, previous_activation, new_activation)
    if importance >= p_concept_activation_importance_threshold:
        concept_activation_event = make_concept_activation_event(slipnode)
        return tell(g_trace, "add-event", concept_activation_event)
    return None


def monitor_new_concept_mappings(concept_mappings_, bridge):
    """trace.ss: monitor-new-concept-mappings"""
    def each(cm):
        importance = concept_mapping_importance(cm, bridge)
        if importance >= p_concept_mapping_importance_threshold:
            concept_mapping_event = make_concept_mapping_event(cm, bridge)
            return tell(g_trace, "add-event", concept_mapping_event)
        return None
    return sugar.for_star(each, concept_mappings_)


def monitor_new_groups(group, flipped_p):
    """trace.ss: monitor-new-groups"""
    importance = group_importance(group, flipped_p)
    if importance >= p_group_importance_threshold:
        group_event = make_group_event(group, flipped_p)
        return tell(g_trace, "add-event", group_event)
    return None


def monitor_new_rules(rule):
    """trace.ss: monitor-new-rules"""
    importance = rule_importance(rule)
    if importance >= p_rule_importance_threshold:
        rule_event = make_rule_event(rule)
        return tell(g_trace, "add-event", rule_event)
    return None


def concept_activation_importance(slipnode, previous_activation, new_activation):
    """trace.ss: concept-activation-importance"""
    delta = chez.sub(new_activation, previous_activation)
    return times_100(chez.mul(percent(chez.abs_(delta)), percent(cd(slipnode))))


def group_importance(group, flipped_p):
    """trace.ss: group-importance"""
    if (flipped_p is not False
            or tell(group, "spans-whole-string?") is not False
            or (tell(group, "singleton-group?") is not False
                and tell(group, "description-type-present?", slipnet.plato_length)
                is not False)):
        return 100
    return tell(group, "get-strength")


def rule_importance(rule):
    """trace.ss: rule-importance"""
    if tell(rule, "get-uniformity") == 100:
        return 100
    return tell(rule, "get-relative-quality")


def concept_mapping_importance(cm, bridge):
    """trace.ss: concept-mapping-importance"""
    if tell(cm, "slippage?") is False:
        return 0
    if tell(_metacat.themes.g_themespace, "supported-by-active-theme?", cm, bridge) is not False:
        return 100
    # the let's bindings only read
    CM_type = tell(cm, "get-CM-type")
    descriptor1 = tell(cm, "get-descriptor1")
    descriptor2 = tell(cm, "get-descriptor2")
    label = tell(cm, "get-label")
    _object1 = tell(bridge, "get-object1")   # bound, never used, as in the original
    _object2 = tell(bridge, "get-object2")
    if CM_type is slipnet.plato_bond_category:
        return 0
    # the list's elements only read
    return round_(weighted_average(
        [cd(CM_type),
         100 if tell(bridge, "spanning-bridge?") is not False else 0,
         average(cd(descriptor1), cd(descriptor2)),
         cd(label) if exists_p(label) else 50],
        [4, 2, 2, 3]))


# --------------------------------- Patterns -----------------------------------
#
# NOTE: there is a slight discrepency between the description of theme-patterns
# given on page 146 of my PhD thesis and the structure of theme-patterns in the
# code here.  Vertical theme-patterns use the tag symbol 'vertical-bridge, not
# 'vertical-themes as shown on page 146.  I used 'vertical-themes in the thesis
# simply for clarity.
#
#         <pattern> ::= <theme-pattern>
#                     | <concept-pattern>
#                     | <codelet-pattern>
#
#   <theme-pattern> ::= (<theme-type> <entry> ...)
#      <theme-type> ::= top-bridge | vertical-bridge | bottom-bridge
#           <entry> ::= (<dimension> <relation>)
#                     | (<dimension> <relation> <activation>)
#
# <concept-pattern> ::= (concepts <entry> ...)
#           <entry> ::= (<slipnode> <activation>)
#
# <codelet-pattern> ::= (codelets <entry> ...)
#           <entry> ::= (<codelet-type> <urgency>)

_THEME_TYPES = ["top-bridge", "vertical-bridge", "bottom-bridge"]


def theme_pattern_p(pattern):
    """trace.ss: theme-pattern?"""
    return member_p(first(pattern), _THEME_TYPES)


def concept_pattern_p(pattern):
    """trace.ss: concept-pattern?"""
    return chez.eq_p(first(pattern), "concepts")


def codelet_pattern_p(pattern):
    """trace.ss: codelet-pattern?"""
    return chez.eq_p(first(pattern), "codelets")


def same_pattern_type_p(pattern1, pattern2):
    """trace.ss: same-pattern-type?"""
    return chez.eq_p(first(pattern1), first(pattern2))


def pattern_type_present_p(pattern, patterns):
    """trace.ss: pattern-type-present?"""
    return exists_p(chez.assq(first(pattern), patterns))


def negate_theme_pattern_entry(entry):
    """trace.ss: negate-theme-pattern-entry"""
    if len(entry) == 3:
        negative_activation = chez.sub(third(entry))
    else:
        negative_activation = chez.sub(_metacat.themes.p_max_theme_activation)
    return [first(entry), second(entry), negative_activation]


# These functions ignore positive/negative theme activations (if any),
# slipnode activations, and codelet urgencies:

def patterns_equal_p(pattern1, pattern2):
    """trace.ss: patterns-equal?"""
    if not same_pattern_type_p(pattern1, pattern2):
        return False
    if concept_pattern_p(pattern1):
        return concept_patterns_equal_p(pattern1, pattern2)
    if codelet_pattern_p(pattern1):
        return codelet_patterns_equal_p(pattern1, pattern2)
    if theme_pattern_p(pattern1):
        return theme_patterns_equal_p(pattern1, pattern2)
    return None           # 1.2: a cond without else


def theme_patterns_equal_p(pattern1, pattern2):
    """trace.ss: theme-patterns-equal?"""
    return (chez.eq_p(first(pattern1), first(pattern2))
            and sets_equal_pred_p(theme_pattern_entries_equal_p,
                                  entries(pattern1), entries(pattern2)))


def theme_pattern_entries_equal_p(entry1, entry2):
    """trace.ss: theme-pattern-entries-equal?"""
    return (chez.eq_p(first(entry1), first(entry2))
            and chez.eq_p(second(entry1), second(entry2)))


def concept_patterns_equal_p(pattern1, pattern2):
    """trace.ss: concept-patterns-equal?"""
    return sets_equal_p(chez.map_(first, entries(pattern1)),
                        chez.map_(first, entries(pattern2)))


def codelet_patterns_equal_p(pattern1, pattern2):
    """trace.ss: codelet-patterns-equal?"""
    return sets_equal_p(chez.map_(first, entries(pattern1)),
                        chez.map_(first, entries(pattern2)))


def entries(l):
    """trace.ss: entries ((define entries rest))"""
    return rest(l)


def print_pattern(pattern):
    """trace.ss: print-pattern"""
    sugar.printf("(~a", first(pattern))
    kind = first(pattern)
    if kind == "concepts":
        for entry in entries(pattern):
            sugar.printf("~n  (~a ~a)", tell(first(entry), "get-short-name"), second(entry))
    elif kind in ("top-bridge", "vertical-bridge", "bottom-bridge"):
        for entry in entries(pattern):
            sugar.printf("~n  (~a ~a~a)",
                         tell(first(entry), "get-short-name"),
                         _metacat.theme_graphics.relation_name(second(entry)),
                         _fmt(" ~a", third(entry)) if len(entry) == 3 else String(""))
    elif kind == "codelets":
        for entry in entries(pattern):
            name = coderack.urgency_name(second(entry))
            sugar.printf("~n  (~a ~a)",
                         tell(first(entry), "get-codelet-type-name"),
                         name if exists_p(name) else second(entry))
    return sugar.printf(")~n")


# ----------------------------------------------------------------------
# Theme-patterns

def get_associated_concept_pattern(theme_pattern):
    """trace.ss: get-associated-concept-pattern"""
    def each(entry):
        dim = first(entry)
        rel = second(entry)
        act = third(entry) if len(entry) == 3 else _metacat.themes.p_max_theme_activation
        tail = ([[rel, slipnet.p_max_activation if act > 0 else 0]]
                if rel is slipnet.plato_opposite else [])
        return [[dim, slipnet.p_max_activation]] + tail
    return ["concepts"] + remove_duplicates(flatmap(each, entries(theme_pattern)))


# impose-theme-pattern does not affect the frozen status of clusters
# or the thematic-pressure status of the themespace.

def impose_theme_pattern(theme_pattern):
    """trace.ss: impose-theme-pattern"""
    theme_type = first(theme_pattern)

    def each(entry):
        dim = first(entry)
        rel = second(entry)
        act = third(entry) if len(entry) == 3 else _metacat.themes.p_max_theme_activation
        return tell(_metacat.themes.g_themespace, "set-theme-activation", theme_type, dim, rel,
                    act)
    return sugar.for_star(each, entries(theme_pattern))


def clamp_theme_pattern(theme_pattern):
    """trace.ss: clamp-theme-pattern"""
    theme_type = first(theme_pattern)
    themespace = _metacat.themes.g_themespace
    tell(themespace, "delete-theme-type", theme_type)
    impose_theme_pattern(theme_pattern)
    tell(_metacat.themes.g_themespace, "freeze-theme-type", theme_type)
    return tell(_metacat.themes.g_themespace, "thematic-pressure-on", theme_type)


def unclamp_theme_pattern(theme_pattern):
    """trace.ss: unclamp-theme-pattern"""
    theme_type = first(theme_pattern)
    tell(_metacat.themes.g_themespace, "unfreeze-theme-type", theme_type)
    return tell(_metacat.themes.g_themespace, "thematic-pressure-off", theme_type)


# ----------------------------------------------------------------------
# Concept-patterns

def clamp_concept_pattern(concept_pattern):
    """trace.ss: clamp-concept-pattern"""
    def each(entry):
        node = first(entry)
        act = second(entry)
        return tell(node, "clamp", act)
    return sugar.for_star(each, entries(concept_pattern))


def unclamp_concept_pattern(concept_pattern):
    """trace.ss: unclamp-concept-pattern"""
    return sugar.for_star(lambda entry: tell(first(entry), "unfreeze"), entries(concept_pattern))


# ----------------------------------------------------------------------
# Codelet-patterns

def get_complement_codelet_pattern(urgency, codelet_patterns):
    """trace.ss: get-complement-codelet-pattern"""
    all_specified_pattern_entries = flatmap(entries, codelet_patterns)
    all_specified_codelet_types = remq_duplicates(chez.map_(first, all_specified_pattern_entries))
    unspecified_codelet_types = remq_elements(all_specified_codelet_types,
                                              coderack.g_codelet_types)
    complement_pattern_entries = chez.map_(lambda type_: [type_, urgency],
                                           unspecified_codelet_types)
    return ["codelets"] + complement_pattern_entries


def against_background(urgency, *codelet_patterns):
    """trace.ss: against-background"""
    codelet_patterns = list(codelet_patterns)
    # the two bindings only read
    specified_entries = flatmap(entries, codelet_patterns)
    complement_entries = entries(get_complement_codelet_pattern(urgency, codelet_patterns))
    return ["codelets"] + specified_entries + complement_entries


def clamp_codelet_pattern(codelet_pattern):
    """trace.ss: clamp-codelet-pattern"""
    def each(entry):
        codelet_type = first(entry)
        urgency = second(entry)
        return tell(codelet_type, "clamp", urgency)
    return sugar.for_star(each, entries(codelet_pattern))


def unclamp_codelet_pattern(codelet_pattern):
    """trace.ss: unclamp-codelet-pattern"""
    return sugar.for_star(lambda entry: tell(first(entry), "unclamp"), entries(codelet_pattern))


# not all of these codelet-patterns are used (made by load(): they need the
# codelet types)

p_top_down_codelet_pattern = None
p_bottom_up_codelet_pattern = None
p_thematic_codelet_pattern = None
p_rule_codelet_pattern = None
p_bond_codelet_pattern = None
p_group_codelet_pattern = None
p_bridge_codelet_pattern = None
p_description_codelet_pattern = None
p_answer_codelet_pattern = None

# ----------------------------------------------------------------------

g_trace = None


def _codelets(*entries_):
    """`(codelets (,type ,urgency) ...)"""
    return ["codelets"] + [[t, u] for t, u in entries_]


def load():
    """trace.ss: the top-level defines that need other modules: the codelet
    patterns (coderack's codelet types and urgencies) and *trace*, in the
    file's order."""
    global p_top_down_codelet_pattern, p_bottom_up_codelet_pattern, p_thematic_codelet_pattern
    global p_rule_codelet_pattern, p_bond_codelet_pattern, p_group_codelet_pattern
    global p_bridge_codelet_pattern, p_description_codelet_pattern, p_answer_codelet_pattern
    global g_trace
    c = coderack
    very_high = c.p_very_high_urgency
    extremely_high = c.p_extremely_high_urgency
    p_top_down_codelet_pattern = _codelets(
        (c.top_down_bond_scout__direction, very_high),
        (c.top_down_group_scout__direction, very_high),
        (c.top_down_bond_scout__category, very_high),
        (c.top_down_group_scout__category, very_high),
        (c.top_down_description_scout, very_high),
        (c.bond_evaluator, extremely_high),
        (c.bond_builder, extremely_high),
        (c.group_evaluator, extremely_high),
        (c.group_builder, extremely_high),
        (c.description_evaluator, extremely_high),
        (c.description_builder, extremely_high))
    p_bottom_up_codelet_pattern = _codelets(
        (c.bottom_up_bond_scout, very_high),
        (c.bond_evaluator, extremely_high),
        (c.bond_builder, extremely_high),
        (c.group_scout__whole_string, very_high),
        (c.group_evaluator, extremely_high),
        (c.group_builder, extremely_high),
        (c.bottom_up_bridge_scout, very_high),
        (c.important_object_bridge_scout, very_high),
        (c.bridge_evaluator, extremely_high),
        (c.bridge_builder, extremely_high),
        (c.bottom_up_description_scout, very_high),
        (c.description_evaluator, extremely_high),
        (c.description_builder, extremely_high),
        (c.rule_scout, very_high),
        (c.rule_evaluator, extremely_high),
        (c.rule_builder, extremely_high))
    p_thematic_codelet_pattern = _codelets(
        (c.thematic_bridge_scout, extremely_high))
    p_rule_codelet_pattern = _codelets(
        (c.rule_scout, very_high),
        (c.rule_evaluator, extremely_high),
        (c.rule_builder, extremely_high))
    p_bond_codelet_pattern = _codelets(
        (c.bottom_up_bond_scout, very_high),
        (c.bond_evaluator, extremely_high),
        (c.bond_builder, extremely_high))
    p_group_codelet_pattern = _codelets(
        (c.group_scout__whole_string, very_high),
        (c.group_evaluator, extremely_high),
        (c.group_builder, extremely_high))
    p_bridge_codelet_pattern = _codelets(
        (c.bottom_up_bridge_scout, very_high),
        (c.important_object_bridge_scout, very_high),
        (c.bridge_evaluator, extremely_high),
        (c.bridge_builder, extremely_high))
    p_description_codelet_pattern = _codelets(
        (c.bottom_up_description_scout, very_high),
        (c.description_evaluator, extremely_high),
        (c.description_builder, extremely_high))
    p_answer_codelet_pattern = _codelets(
        (c.answer_finder, extremely_high),
        (c.answer_justifier, extremely_high))
    g_trace = make_temporal_trace()
