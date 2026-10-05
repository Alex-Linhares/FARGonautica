"""memory.ss: the Memory, answer and snag descriptions, their abstraction from
the Trace's answer and snag events, and reminding (the distance between two
answers).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from memory.ss, with
racket/engine/memory.rktl as a worked translation.

The three record-case closures are classes (docs/python-translation-plan.md,
"Objects"): make-memory's is Memory, make-answer-description's
AnswerDescription and make-snag-description's SnagDescription; each delegates
what it doesn't answer to base-object.  Where a closure variable and a message
share a name (highlighted?, and the answer description's get-normal-icon-pexp,
a procedure the Memory window installs), the attribute keeps the mapped name and
the method gets a trailing underscore, as rules.py does.  *memory* is made by
load(); %distance-threshold% needs nothing and is a module constant.

rules.ss and answers.ss load before this file and are imported directly
(rule-clause-lists-equal?; abstract-answer-description-theme-pattern,
most-recent-group-and-concept-mapping-events, get-unjustified-theme-pattern,
get-theme-supporting-concept-mappings, get-snag-justified-themes,
intersect-themes, answer-incoherent?).  Names from files translated later, or
not translated yet, are read through the package at call time: run.ss
(*this-run*, read when a description is made; *display-mode?*, set by display),
themes.ss (*themespace*, display only), justify.ss (compare-rule-clause-lists)
and trace.ss (entries, impose-theme-pattern).  *memory-window*,
*comment-window*, *workspace-window*, *slipnet-window*, *coderack-window*,
*temperature-window* and %workspace-graphics% are setup's; *workspace* and the
three workspace strings are workspace's.  As in the original, the Memory sends
initialize, add-memory-icon, erase-memory-icon and draw to *memory-window*
ungated, and calls the icon procedure that add-memory-icon installs
(set-graphics-info); headless, the harness's window provides it (oracle
prelude.ss, install-headless-windows!).  The pexps for the Workspace window are
made only with %workspace-graphics% on.  Every string the commentary receives,
and every print name made here, is a chez.String.  The engine never imports
tkinter.

Evaluation order (audited against Chez's): memory.ss draws no random numbers
and calls nothing that draws.  The sites with several effects are sequenced as
in Chez: the let*s of make-answer-description, make-snag-description,
abstract-answer-description, abstract-snag-description and
calculate-answer-distance are statements in order; add-answer-description sets
the activation, sends add-memory-icon, then compares the new answer with every
stored one, newest first (for*, first to last), each compare updating the
icon and maybe adding a comment, and only then conses the new answer on;
add-snag-description conses before sending add-memory-icon; display's
themespace and window messages are statements in order.  The tell-alls of the
print names use chez.map_ (they only read).  The multi-argument calls
(make-answer-description's seventeen arguments, make-snag-description's six,
the equal? messages of snag-present?, get-equivalent-snag, answer-present? and
answers-equal?, compare's add-comment, and the eq? of the two
answer-incoherent? calls) only read, so their order is not observable; they
are kept left to right, as the Racket port has them.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import answers, chez, rules, setup, sugar, workspace
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (base_object, cd, exists_p, filter_, filter_meth, filter_out,
                               first, hundred_minus, intersect, member_p, ormap_meth,
                               partition, print_, remove_elements, remq_elements, round_,
                               second, select, select_meth, tell_all, times_100)

String = chez.String


def _fmt(control, *args):
    """(format control arg ...) as a Scheme string."""
    return String(chez.format_(control, *args))


def _string_append_all(strings):
    """(apply string-append strings) as a Scheme string."""
    return String("".join(strings))


def _null_p(l):
    return len(l) == 0


def _check_reals(who, *xs):
    # chez: comparing with #f (a bounding box never set) is an error; Python's
    # bool is an int and would compare quietly
    for x in xs:
        if x is False or x is True:
            raise chez.SchemeError(who, "~s is not a real number", x)


class Memory(SchemeObject):
    """memory.ss: make-memory (the closure)"""
    __slots__ = ("answer_descriptions", "snag_descriptions", "all_descriptions")

    def __init__(this):
        this.answer_descriptions = []
        this.snag_descriptions = []
        this.all_descriptions = []

    @message("object-type")
    def object_type(this, self):
        return "memory"

    @message("print")
    def print_(this, self):
        return sugar.for_star(lambda answer: print_(answer), this.all_descriptions)

    @message("clear")
    def clear(this, self):
        this.answer_descriptions = []
        this.snag_descriptions = []
        this.all_descriptions = []
        tell(setup.g_memory_window, "initialize")
        return "done"

    @message("clear-activations")
    def clear_activations(this, self):
        for answer in this.answer_descriptions:
            tell(answer, "update-activation", 0)
        return "done"

    @message("delete")
    def delete(this, self, answer):
        if member_p(answer, this.answer_descriptions):
            this.answer_descriptions = chez.remq(answer, this.answer_descriptions)
            this.all_descriptions = chez.remq(answer, this.all_descriptions)
            tell(setup.g_memory_window, "erase-memory-icon", answer)
            return "done"
        if member_p(answer, this.snag_descriptions):
            this.snag_descriptions = chez.remq(answer, this.snag_descriptions)
            this.all_descriptions = chez.remq(answer, this.all_descriptions)
            tell(setup.g_memory_window, "erase-memory-icon", answer)
            return "done"
        return sugar.printf("Not currently in memory.~n")

    @message("get-answers")
    def get_answers(this, self):
        return this.answer_descriptions

    @message("get-snags")
    def get_snags(this, self):
        return this.snag_descriptions

    @message("get-all-descriptions")
    def get_all_descriptions(this, self):
        return this.all_descriptions

    @message("get-highlighted-description")
    def get_highlighted_description(this, self):
        return select_meth(this.all_descriptions, "highlighted?")

    @message("unhighlight-all-answers")
    def unhighlight_all_answers(this, self):
        for answer in this.all_descriptions:
            tell(answer, "unhighlight")
        return "done"

    @message("unhighlight-all-answers-except")
    def unhighlight_all_answers_except(this, self, exception):
        # the value is the last one-armed if*'s (for*'s value)
        return sugar.for_star(
            lambda answer: sugar.if_star(not chez.eq_p(answer, exception),
                                         lambda: tell(answer, "unhighlight")),
            this.all_descriptions)

    @message("get-mouse-selected-answer")
    def get_mouse_selected_answer(this, self, x, y):
        return select(lambda answer: tell(answer, "within-bounding-box?", x, y),
                      this.all_descriptions)

    @message("get-other-highlighted-answer")
    def get_other_highlighted_answer(this, self, exception):
        def other_highlighted(answer):
            if chez.eq_p(answer, exception):
                return False
            return tell(answer, "highlighted?")
        return select(other_highlighted, this.answer_descriptions)

    @message("snag-present?")
    def snag_present_p(this, self, snag_rule):
        # the arguments only read
        return ormap_meth(this.snag_descriptions, "equal?",
                          tell(workspace.g_initial_string, "get-letter-categories"),
                          tell(workspace.g_modified_string, "get-letter-categories"),
                          tell(workspace.g_target_string, "get-letter-categories"),
                          tell(snag_rule, "get-rule-clauses"))

    @message("get-equivalent-snag")
    def get_equivalent_snag(this, self, answer):
        # the arguments only read
        return select_meth(this.snag_descriptions, "equal?",
                           tell(answer, "get-initial-letters"),
                           tell(answer, "get-modified-letters"),
                           tell(answer, "get-target-letters"),
                           tell(answer, "get-top-rule-clauses"))

    @message("answer-present?")
    def answer_present_p(this, self, answer_letters, top_rule, bottom_rule):
        # the arguments only read
        return ormap_meth(this.answer_descriptions, "equal?",
                          tell(workspace.g_initial_string, "get-letter-categories"),
                          tell(workspace.g_modified_string, "get-letter-categories"),
                          tell(workspace.g_target_string, "get-letter-categories"),
                          answer_letters,
                          tell(top_rule, "get-rule-clauses"),
                          tell(bottom_rule, "get-rule-clauses"))

    @message("add-answer-description")
    def add_answer_description(this, self, new_answer):
        # This needs to be set before calling add-memory-icon:
        tell(new_answer, "set-activation", 100)
        tell(setup.g_memory_window, "add-memory-icon", new_answer)
        # newest first; the new answer is not stored yet
        for answer in this.answer_descriptions:
            tell(answer, "compare", new_answer)
        this.answer_descriptions = [new_answer] + this.answer_descriptions
        this.all_descriptions = [new_answer] + this.all_descriptions
        return "done"

    @message("add-snag-description")
    def add_snag_description(this, self, new_snag):
        this.snag_descriptions = [new_snag] + this.snag_descriptions
        this.all_descriptions = [new_snag] + this.all_descriptions
        tell(setup.g_memory_window, "add-memory-icon", new_snag)
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_memory():
    """memory.ss: make-memory"""
    return Memory()


class AnswerDescription(SchemeObject):
    """memory.ss: make-answer-description (the closure)"""
    __slots__ = ("initial_letters", "modified_letters", "target_letters", "answer_letters",
                 "top_rule_clauses", "bottom_rule_clauses", "top_rule_phrases",
                 "bottom_rule_phrases", "top_rule_abstractness", "bottom_rule_abstractness",
                 "temperature", "quality", "vertical_theme_pattern", "top_theme_pattern",
                 "bottom_theme_pattern", "unjustified_theme_pattern", "unjustified_slippages",
                 "initial_print_name", "modified_print_name", "target_print_name",
                 "answer_print_name", "activation", "get_normal_icon_pexp",
                 "highlight_icon_pexp", "highlighted_p", "bounding_box_x1", "bounding_box_y1",
                 "bounding_box_x2", "bounding_box_y2", "answer_description_pexp", "this_run")

    def __init__(this, initial_letters, modified_letters, target_letters, answer_letters,
                 top_rule_clauses, bottom_rule_clauses, top_rule_phrases, bottom_rule_phrases,
                 top_rule_abstractness, bottom_rule_abstractness, temperature, quality,
                 vertical_theme_pattern, top_theme_pattern, bottom_theme_pattern,
                 unjustified_theme_pattern, unjustified_slippages):
        this.initial_letters = initial_letters
        this.modified_letters = modified_letters
        this.target_letters = target_letters
        this.answer_letters = answer_letters
        this.top_rule_clauses = top_rule_clauses
        this.bottom_rule_clauses = bottom_rule_clauses
        this.top_rule_phrases = top_rule_phrases
        this.bottom_rule_phrases = bottom_rule_phrases
        this.top_rule_abstractness = top_rule_abstractness
        this.bottom_rule_abstractness = bottom_rule_abstractness
        this.temperature = temperature
        this.quality = quality
        this.vertical_theme_pattern = vertical_theme_pattern
        this.top_theme_pattern = top_theme_pattern
        this.bottom_theme_pattern = bottom_theme_pattern
        this.unjustified_theme_pattern = unjustified_theme_pattern
        this.unjustified_slippages = unjustified_slippages
        # the let*, in order
        this.initial_print_name = _string_append_all(tell_all(initial_letters, "print-name"))
        this.modified_print_name = _string_append_all(tell_all(modified_letters, "print-name"))
        this.target_print_name = _string_append_all(tell_all(target_letters, "print-name"))
        this.answer_print_name = _string_append_all(tell_all(answer_letters, "print-name"))
        this.activation = 0
        # a procedure (activation -> icon pexp) that the Memory window installs
        this.get_normal_icon_pexp = False
        this.highlight_icon_pexp = False
        this.highlighted_p = False
        this.bounding_box_x1 = False
        this.bounding_box_y1 = False
        this.bounding_box_x2 = False
        this.bounding_box_y2 = False
        this.answer_description_pexp = False
        this.this_run = _metacat.run.g_this_run

    @message("object-type")
    def object_type(this, self):
        return "answer-description"

    @message("print-name")
    def print_name(this, self):
        return _fmt("~a -> ~a, ~a -> ~a",
                    this.initial_print_name, this.modified_print_name,
                    this.target_print_name, this.answer_print_name)

    @message("problem-print-name")
    def problem_print_name(this, self):
        return _fmt("~a -> ~a, ~a -> ?",
                    this.initial_print_name, this.modified_print_name, this.target_print_name)

    @message("print")
    def print_(this, self):
        return sugar.printf("Answer Description \"~a\"  (Quality = ~a)~n",
                            tell(self, "print-name"),
                            this.quality)

    @message("get-initial-letters")
    def get_initial_letters(this, self):
        return this.initial_letters

    @message("get-modified-letters")
    def get_modified_letters(this, self):
        return this.modified_letters

    @message("get-target-letters")
    def get_target_letters(this, self):
        return this.target_letters

    @message("get-answer-letters")
    def get_answer_letters(this, self):
        return this.answer_letters

    @message("get-initial-print-name")
    def get_initial_print_name(this, self):
        return this.initial_print_name

    @message("get-modified-print-name")
    def get_modified_print_name(this, self):
        return this.modified_print_name

    @message("get-target-print-name")
    def get_target_print_name(this, self):
        return this.target_print_name

    @message("get-answer-print-name")
    def get_answer_print_name(this, self):
        return this.answer_print_name

    @message("get-top-rule-clauses")
    def get_top_rule_clauses(this, self):
        return this.top_rule_clauses

    @message("get-bottom-rule-clauses")
    def get_bottom_rule_clauses(this, self):
        return this.bottom_rule_clauses

    @message("get-top-rule-phrases")
    def get_top_rule_phrases(this, self):
        return this.top_rule_phrases

    @message("get-bottom-rule-phrases")
    def get_bottom_rule_phrases(this, self):
        return this.bottom_rule_phrases

    @message("get-top-rule-abstractness")
    def get_top_rule_abstractness(this, self):
        return this.top_rule_abstractness

    @message("get-bottom-rule-abstractness")
    def get_bottom_rule_abstractness(this, self):
        return this.bottom_rule_abstractness

    @message("get-temperature")
    def get_temperature(this, self):
        return this.temperature

    @message("get-quality")
    def get_quality(this, self):
        return this.quality

    @message("get-vertical-theme-pattern")
    def get_vertical_theme_pattern(this, self):
        return this.vertical_theme_pattern

    @message("get-top-theme-pattern")
    def get_top_theme_pattern(this, self):
        return this.top_theme_pattern

    @message("get-bottom-theme-pattern")
    def get_bottom_theme_pattern(this, self):
        return this.bottom_theme_pattern

    # Top and bottom theme patterns are stored in answer descriptions
    # but are not used in comparing answers with each other:
    @message("get-themes")
    def get_themes(this, self):
        return _metacat.trace.entries(this.vertical_theme_pattern)

    @message("get-unjustified-theme-pattern")
    def get_unjustified_theme_pattern(this, self):
        return this.unjustified_theme_pattern

    @message("get-unjustified-themes")
    def get_unjustified_themes(this, self):
        return _metacat.trace.entries(this.unjustified_theme_pattern)

    @message("get-unjustified-slippages")
    def get_unjustified_slippages(this, self):
        return this.unjustified_slippages

    @message("get-this-run")
    def get_this_run(this, self):
        return this.this_run

    @message("unjustified?")
    def unjustified_p(this, self):
        return not _null_p(this.unjustified_slippages)

    @message("answers-equal?")
    def answers_equal_p(this, self, answer):
        # the arguments only read
        return tell(self, "equal?",
                    tell(answer, "get-initial-letters"),
                    tell(answer, "get-modified-letters"),
                    tell(answer, "get-target-letters"),
                    tell(answer, "get-answer-letters"),
                    tell(answer, "get-top-rule-clauses"),
                    tell(answer, "get-bottom-rule-clauses"))

    @message("equal?")
    def equal_p(this, self, i_letters, m_letters, t_letters, a_letters, top_clauses,
                bot_clauses):
        # (and ...): the value is the last test's
        if not chez.equal_p(i_letters, this.initial_letters):
            return False
        if not chez.equal_p(m_letters, this.modified_letters):
            return False
        if not chez.equal_p(t_letters, this.target_letters):
            return False
        if not chez.equal_p(a_letters, this.answer_letters):
            return False
        r = rules.rule_clause_lists_equal_p(top_clauses, this.top_rule_clauses)
        if r is False:
            return False
        return rules.rule_clause_lists_equal_p(bot_clauses, this.bottom_rule_clauses)

    @message("get-activation")
    def get_activation(this, self):
        return this.activation

    @message("set-activation")
    def set_activation(this, self, value):
        this.activation = value
        return "done"

    @message("update-activation")
    def update_activation(this, self, new_value):
        if new_value != this.activation:
            # the icon procedure the Memory window installed (#f before
            # add-memory-icon: applying it is an error, as in Chez)
            tell(setup.g_memory_window, "draw", this.get_normal_icon_pexp(new_value))
        this.activation = new_value
        return "done"

    @message("get-answer-distance")
    def get_answer_distance(this, self, other_answer):
        return calculate_answer_distance(self, other_answer)

    @message("compare")
    def compare(this, self, new_answer):
        distance = calculate_answer_distance(self, new_answer)
        new_activation = hundred_minus(times_100(chez.min_(1, chez.div(distance,
                                                                       p_distance_threshold))))
        tell(self, "update-activation", new_activation)
        if new_activation > 0:
            if new_activation > 70:
                strength = String("strongly reminds me")
            elif new_activation > 30:
                strength = String("reminds me somewhat")
            else:
                strength = String("vaguely reminds me")
            # the arguments only read
            tell(setup.g_comment_window, "add-comment",
                 [_fmt("This answer ~a of the answer ~a to ~a.",
                       strength,
                       this.answer_print_name,
                       _fmt("the problem \"~a\"", tell(self, "problem-print-name")))],
                 [String("This answer is reminiscent of the answer "),
                  _fmt("~a to ~a.  Reminding strength = ~a.",
                       this.answer_print_name,
                       _fmt("the problem \"~a\"", tell(self, "problem-print-name")),
                       new_activation)])
        return "done"

    @message("get-answer-description-pexp")
    def get_answer_description_pexp(this, self):
        return this.answer_description_pexp

    @message("get-normal-icon-pexp")
    def get_normal_icon_pexp_(this, self):
        return this.get_normal_icon_pexp(this.activation)

    @message("get-highlight-icon-pexp")
    def get_highlight_icon_pexp(this, self):
        return this.highlight_icon_pexp

    @message("set-answer-description-pexp")
    def set_answer_description_pexp(this, self, pexp):
        this.answer_description_pexp = pexp
        return "done"

    @message("set-graphics-info")
    def set_graphics_info(this, self, proc, highlight_icon):
        this.get_normal_icon_pexp = proc
        this.highlight_icon_pexp = highlight_icon
        return "done"

    @message("get-bounding-box-x1")
    def get_bounding_box_x1(this, self):
        return this.bounding_box_x1

    @message("get-bounding-box-y1")
    def get_bounding_box_y1(this, self):
        return this.bounding_box_y1

    @message("get-bounding-box-x2")
    def get_bounding_box_x2(this, self):
        return this.bounding_box_x2

    @message("get-bounding-box-y2")
    def get_bounding_box_y2(this, self):
        return this.bounding_box_y2

    @message("set-bounding-box")
    def set_bounding_box(this, self, x1, y1, x2, y2):
        this.bounding_box_x1 = x1
        this.bounding_box_y1 = y1
        this.bounding_box_x2 = x2
        this.bounding_box_y2 = y2
        return "done"

    @message("within-bounding-box?")
    def within_bounding_box_p(this, self, x, y):
        return _within_bounding_box(this, x, y)

    @message("highlighted?")
    def highlighted_p_(this, self):
        return this.highlighted_p

    @message("highlight")
    def highlight(this, self):
        if this.highlighted_p is False:
            this.highlighted_p = True
            tell(setup.g_memory_window, "draw", this.highlight_icon_pexp)
        return "done"

    @message("unhighlight")
    def unhighlight(this, self):
        if this.highlighted_p is not False:
            this.highlighted_p = False
            tell(setup.g_memory_window, "draw", this.get_normal_icon_pexp(this.activation))
        return "done"

    @message("toggle-highlight")
    def toggle_highlight(this, self):
        return tell(self, "unhighlight" if this.highlighted_p is not False else "highlight")

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "draw", this.answer_description_pexp)
        if tell(_metacat.themes.g_themespace, "current-state-displayed?") is not False:
            tell(_metacat.themes.g_themespace, "save-current-state")
        tell(_metacat.themes.g_themespace, "delete-everything")
        tell(_metacat.themes.g_themespace, "thematic-pressure-off")
        _metacat.trace.impose_theme_pattern(this.vertical_theme_pattern)
        _metacat.trace.impose_theme_pattern(this.top_theme_pattern)
        _metacat.trace.impose_theme_pattern(this.bottom_theme_pattern)
        # Blank slipnet window:
        tell(setup.g_slipnet_window, "blank-window")
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display answer temperature:
        return tell(setup.g_temperature_window, "update-graphics", this.temperature)

    @message("display-workspace")
    def display_workspace(this, self):
        return tell(setup.g_workspace_window, "draw", this.answer_description_pexp)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def _within_bounding_box(this, x, y):
    """within-bounding-box? of both descriptions:
    (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))"""
    _check_reals(">=", x, this.bounding_box_x1)
    if not x >= this.bounding_box_x1:
        return False
    _check_reals("<=", x, this.bounding_box_x2)
    if not x <= this.bounding_box_x2:
        return False
    _check_reals(">=", y, this.bounding_box_y1)
    if not y >= this.bounding_box_y1:
        return False
    _check_reals("<=", y, this.bounding_box_y2)
    return y <= this.bounding_box_y2


def make_answer_description(initial_letters, modified_letters, target_letters, answer_letters,
                            top_rule_clauses, bottom_rule_clauses,
                            top_rule_phrases, bottom_rule_phrases,
                            top_rule_abstractness, bottom_rule_abstractness,
                            temperature, quality, vertical_theme_pattern, top_theme_pattern,
                            bottom_theme_pattern, unjustified_theme_pattern,
                            unjustified_slippages):
    """memory.ss: make-answer-description"""
    return AnswerDescription(initial_letters, modified_letters, target_letters, answer_letters,
                             top_rule_clauses, bottom_rule_clauses,
                             top_rule_phrases, bottom_rule_phrases,
                             top_rule_abstractness, bottom_rule_abstractness,
                             temperature, quality, vertical_theme_pattern, top_theme_pattern,
                             bottom_theme_pattern, unjustified_theme_pattern,
                             unjustified_slippages)


class SnagDescription(SchemeObject):
    """memory.ss: make-snag-description (the closure)"""
    __slots__ = ("rule_clauses", "translated_rule_clauses", "rule_phrases",
                 "translated_rule_phrases", "snag_explanation", "theme_pattern",
                 "initial_letters", "modified_letters", "target_letters",
                 "initial_print_name", "modified_print_name", "target_print_name",
                 "normal_icon_pexp", "highlight_icon_pexp", "highlighted_p",
                 "bounding_box_x1", "bounding_box_y1", "bounding_box_x2", "bounding_box_y2",
                 "snag_description_pexp", "this_run")

    def __init__(this, rule_clauses, translated_rule_clauses, rule_phrases,
                 translated_rule_phrases, snag_explanation, theme_pattern):
        this.rule_clauses = rule_clauses
        this.translated_rule_clauses = translated_rule_clauses
        this.rule_phrases = rule_phrases
        this.translated_rule_phrases = translated_rule_phrases
        this.snag_explanation = snag_explanation
        this.theme_pattern = theme_pattern
        # the let*, in order
        this.initial_letters = tell(workspace.g_initial_string, "get-letter-categories")
        this.modified_letters = tell(workspace.g_modified_string, "get-letter-categories")
        this.target_letters = tell(workspace.g_target_string, "get-letter-categories")
        this.initial_print_name = tell(workspace.g_initial_string, "print-name")
        this.modified_print_name = tell(workspace.g_modified_string, "print-name")
        this.target_print_name = tell(workspace.g_target_string, "print-name")
        this.normal_icon_pexp = False
        this.highlight_icon_pexp = False
        this.highlighted_p = False
        this.bounding_box_x1 = False
        this.bounding_box_y1 = False
        this.bounding_box_x2 = False
        this.bounding_box_y2 = False
        this.snag_description_pexp = False
        this.this_run = _metacat.run.g_this_run

    @message("object-type")
    def object_type(this, self):
        return "snag-description"

    @message("print-name")
    def print_name(this, self):
        return _fmt("~a -> ~a, ~a -> SNAG",
                    this.initial_print_name,
                    this.modified_print_name,
                    this.target_print_name)

    @message("problem-print-name")
    def problem_print_name(this, self):
        return _fmt("~a -> ~a, ~a -> ?",
                    this.initial_print_name,
                    this.modified_print_name,
                    this.target_print_name)

    @message("print")
    def print_(this, self):
        return sugar.printf("Snag Description \"~a\"~n", tell(self, "print-name"))

    @message("get-initial-letters")
    def get_initial_letters(this, self):
        return this.initial_letters

    @message("get-modified-letters")
    def get_modified_letters(this, self):
        return this.modified_letters

    @message("get-target-letters")
    def get_target_letters(this, self):
        return this.target_letters

    @message("get-initial-print-name")
    def get_initial_print_name(this, self):
        return this.initial_print_name

    @message("get-modified-print-name")
    def get_modified_print_name(this, self):
        return this.modified_print_name

    @message("get-target-print-name")
    def get_target_print_name(this, self):
        return this.target_print_name

    @message("get-rule-clauses")
    def get_rule_clauses(this, self):
        return this.rule_clauses

    @message("get-translated-rule-phrases")
    def get_translated_rule_phrases(this, self):
        return this.translated_rule_phrases

    @message("get-explanation")
    def get_explanation(this, self):
        return this.snag_explanation

    @message("get-theme-pattern")
    def get_theme_pattern(this, self):
        return this.theme_pattern

    @message("get-themes")
    def get_themes(this, self):
        return _metacat.trace.entries(this.theme_pattern)

    @message("get-this-run")
    def get_this_run(this, self):
        return this.this_run

    @message("equal?")
    def equal_p(this, self, i_letters, m_letters, t_letters, rc_list):
        # (and ...): the value is the last test's
        if not chez.equal_p(i_letters, this.initial_letters):
            return False
        if not chez.equal_p(m_letters, this.modified_letters):
            return False
        if not chez.equal_p(t_letters, this.target_letters):
            return False
        return rules.rule_clause_lists_equal_p(rc_list, this.rule_clauses)

    # This is used by the memory-window's add-memory-icon method:
    @message("get-activation")
    def get_activation(this, self):
        return 0

    @message("get-snag-description-pexp")
    def get_snag_description_pexp(this, self):
        return this.snag_description_pexp

    @message("get-normal-icon-pexp")
    def get_normal_icon_pexp(this, self):
        return this.normal_icon_pexp

    @message("get-highlight-icon-pexp")
    def get_highlight_icon_pexp(this, self):
        return this.highlight_icon_pexp

    @message("set-snag-description-pexp")
    def set_snag_description_pexp(this, self, pexp):
        this.snag_description_pexp = pexp
        return "done"

    @message("set-graphics-info")
    def set_graphics_info(this, self, proc, highlight_icon):
        # Snag icons are always the same color as the memory background:
        this.normal_icon_pexp = proc(0)
        this.highlight_icon_pexp = highlight_icon
        return "done"

    @message("get-bounding-box-x1")
    def get_bounding_box_x1(this, self):
        return this.bounding_box_x1

    @message("get-bounding-box-y1")
    def get_bounding_box_y1(this, self):
        return this.bounding_box_y1

    @message("get-bounding-box-x2")
    def get_bounding_box_x2(this, self):
        return this.bounding_box_x2

    @message("get-bounding-box-y2")
    def get_bounding_box_y2(this, self):
        return this.bounding_box_y2

    @message("set-bounding-box")
    def set_bounding_box(this, self, x1, y1, x2, y2):
        this.bounding_box_x1 = x1
        this.bounding_box_y1 = y1
        this.bounding_box_x2 = x2
        this.bounding_box_y2 = y2
        return "done"

    @message("within-bounding-box?")
    def within_bounding_box_p(this, self, x, y):
        return _within_bounding_box(this, x, y)

    @message("highlighted?")
    def highlighted_p_(this, self):
        return this.highlighted_p

    @message("highlight")
    def highlight(this, self):
        if this.highlighted_p is False:
            this.highlighted_p = True
            tell(setup.g_memory_window, "draw", this.highlight_icon_pexp)
        return "done"

    @message("unhighlight")
    def unhighlight(this, self):
        if this.highlighted_p is not False:
            this.highlighted_p = False
            tell(setup.g_memory_window, "draw", this.normal_icon_pexp)
        return "done"

    @message("toggle-highlight")
    def toggle_highlight(this, self):
        return tell(self, "unhighlight" if this.highlighted_p is not False else "highlight")

    @message("display")
    def display(this, self):
        _metacat.run.g_display_mode_p = True
        tell(setup.g_workspace_window, "draw", this.snag_description_pexp)
        if tell(_metacat.themes.g_themespace, "current-state-displayed?") is not False:
            tell(_metacat.themes.g_themespace, "save-current-state")
        tell(_metacat.themes.g_themespace, "delete-everything")
        tell(_metacat.themes.g_themespace, "thematic-pressure-off")
        _metacat.trace.impose_theme_pattern(this.theme_pattern)
        # Blank slipnet window:
        tell(setup.g_slipnet_window, "blank-window")
        # Blank coderack window:
        tell(setup.g_coderack_window, "blank-window", String("Coderack"))
        # Display answer temperature:
        return tell(setup.g_temperature_window, "update-graphics", 100)

    @message("display-workspace")
    def display_workspace(this, self):
        return tell(setup.g_workspace_window, "draw", this.snag_description_pexp)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_snag_description(rule_clauses, translated_rule_clauses, rule_phrases,
                          translated_rule_phrases, snag_explanation, theme_pattern):
    """memory.ss: make-snag-description"""
    return SnagDescription(rule_clauses, translated_rule_clauses, rule_phrases,
                           translated_rule_phrases, snag_explanation, theme_pattern)


def abstract_answer_description(answer_event):
    """memory.ss: abstract-answer-description"""
    # the let*, in order
    answer_description_vertical_theme_pattern = \
        answers.abstract_answer_description_theme_pattern(
            answers.most_recent_group_and_concept_mapping_events())
    unjustified_theme_pattern = answers.get_unjustified_theme_pattern(
        tell(answer_event, "get-unjustified-slippages"))
    top_rule = tell(answer_event, "get-rule", "top")
    bottom_rule = tell(answer_event, "get-rule", "bottom")
    theme_supporting_bridges = filter_meth(
        tell(workspace.g_workspace, "get-bridges", "vertical"),
        "supports-theme-pattern?", answer_description_vertical_theme_pattern)
    theme_supporting_concept_mappings = answers.get_theme_supporting_concept_mappings(
        answer_description_vertical_theme_pattern,
        theme_supporting_bridges)
    # the arguments only read
    new_answer = make_answer_description(
        tell(answer_event, "get-initial-letters"),
        tell(answer_event, "get-modified-letters"),
        tell(answer_event, "get-target-letters"),
        tell(answer_event, "get-answer-letters"),
        tell(top_rule, "get-rule-clauses"),
        tell(bottom_rule, "get-rule-clauses"),
        tell(top_rule, "get-english-transcription"),
        tell(bottom_rule, "get-english-transcription"),
        tell(top_rule, "get-abstractness"),
        tell(bottom_rule, "get-abstractness"),
        tell(answer_event, "get-temperature"),
        # This is relative quality:
        tell(answer_event, "get-quality"),
        answer_description_vertical_theme_pattern,
        tell(top_rule, "get-theme-pattern"),
        tell(bottom_rule, "get-theme-pattern"),
        unjustified_theme_pattern,
        tell(answer_event, "get-unjustified-slippages"))
    if setup.p_workspace_graphics is not False:
        tell(new_answer, "set-answer-description-pexp",
             tell(answer_event, "make-answer-description-pexp",
                  theme_supporting_bridges,
                  theme_supporting_concept_mappings))
    tell(answer_event, "set-answer-description", new_answer)
    return tell(g_memory, "add-answer-description", new_answer)


def abstract_snag_description(snag_event):
    """memory.ss: abstract-snag-description"""
    # the let*, in order
    # Retain only the dominant themes:
    snag_theme_pattern = ["vertical-bridge"] + chez.map_(
        first,
        filter_out(lambda cluster: len(cluster) > 1,
                   partition(lambda entry1, entry2: chez.eq_p(first(entry1), first(entry2)),
                             _metacat.trace.entries(
                                 tell(snag_event, "get-snag-theme-pattern")))))
    rule = tell(snag_event, "get-rule", "top")
    translated_rule = tell(snag_event, "get-rule", "bottom")
    snag_bridges = tell(snag_event, "get-snag-bridges")
    if not _null_p(snag_bridges):
        theme_supporting_bridges = snag_bridges
    else:
        theme_supporting_bridges = filter_meth(
            tell(workspace.g_workspace, "get-bridges", "vertical"),
            "supports-theme-pattern?", snag_theme_pattern)
    theme_supporting_concept_mappings = answers.get_theme_supporting_concept_mappings(
        snag_theme_pattern,
        theme_supporting_bridges)
    # the arguments only read
    new_snag = make_snag_description(
        tell(rule, "get-rule-clauses"),
        tell(translated_rule, "get-rule-clauses"),
        tell(rule, "get-english-transcription"),
        tell(translated_rule, "get-english-transcription"),
        tell(snag_event, "get-explanation"),
        snag_theme_pattern)
    if setup.p_workspace_graphics is not False:
        tell(new_snag, "set-snag-description-pexp",
             tell(snag_event, "make-snag-description-pexp",
                  theme_supporting_bridges,
                  theme_supporting_concept_mappings))
    return tell(g_memory, "add-snag-description", new_snag)


# If the calculated distance between two answers is equal to
# or greater than %distance-threshold%, reminding will not
# occur.  This value should never be set to zero.  Setting
# it to one effectively turns off reminding.

p_distance_threshold = 5

# The distance between two identical answers (all strings
# and both rules exactly equal) is zero, and the distance
# between two non-identical answers is always at least one.


def calculate_answer_distance(answer1, answer2):
    """memory.ss: calculate-answer-distance"""
    if tell(answer1, "answers-equal?", answer2) is not False:
        return 0
    # the let*, in order
    all_themes1 = tell(answer1, "get-themes") + tell(answer1, "get-unjustified-themes")
    all_themes2 = tell(answer2, "get-themes") + tell(answer2, "get-unjustified-themes")
    unjustified_themes1 = remove_elements(
        answers.get_snag_justified_themes(answer1),
        tell(answer1, "get-unjustified-themes"))
    unjustified_themes2 = remove_elements(
        answers.get_snag_justified_themes(answer2),
        tell(answer2, "get-unjustified-themes"))
    common_themes = answers.intersect_themes(all_themes1, all_themes2)
    common_dimensions = chez.map_(first, common_themes)
    differing_dimensions = remq_elements(
        common_dimensions,
        intersect(chez.map_(first, all_themes1), chez.map_(first, all_themes2)))
    answer1_only_themes = remove_elements(common_themes, all_themes1)
    answer2_only_themes = remove_elements(common_themes, all_themes2)
    differing_themes1 = filter_(lambda theme: member_p(first(theme), differing_dimensions),
                                all_themes1)
    differing_themes2 = filter_(lambda theme: member_p(first(theme), differing_dimensions),
                                all_themes2)
    unique_themes1 = remove_elements(differing_themes1, answer1_only_themes)
    unique_themes2 = remove_elements(differing_themes2, answer2_only_themes)
    common_unjustified_themes = answers.intersect_themes(unjustified_themes1,
                                                         unjustified_themes2)
    answer1_only_unjustified_themes = remove_elements(common_unjustified_themes,
                                                      unjustified_themes1)
    answer2_only_unjustified_themes = remove_elements(common_unjustified_themes,
                                                      unjustified_themes2)
    common_answer1_only_unjustified_themes = remove_elements(differing_themes1,
                                                             answer1_only_unjustified_themes)
    common_answer2_only_unjustified_themes = remove_elements(differing_themes2,
                                                             answer2_only_unjustified_themes)
    theme_distance = (len(differing_dimensions)
                      + 2 * len(unique_themes1)
                      + 2 * len(unique_themes2))
    rule_differences = _metacat.justify.compare_rule_clause_lists(
        tell(answer1, "get-top-rule-clauses"),
        tell(answer2, "get-top-rule-clauses"))
    if exists_p(rule_differences):
        num_rule_differences = len(filter_out(
            lambda nodes: cd(first(nodes)) == cd(second(nodes)),
            rule_differences))
    else:
        num_rule_differences = -1
    rule1_abstractness = tell(answer1, "get-top-rule-abstractness")
    rule2_abstractness = tell(answer2, "get-top-rule-abstractness")
    if num_rule_differences == -1:
        rule_distance = round_(chez.div(chez.abs_(chez.sub(rule1_abstractness,
                                                           rule2_abstractness)),
                                        10))
    else:
        rule_distance = 2 * num_rule_differences
    justification_distance = (len(common_answer1_only_unjustified_themes)
                              + len(common_answer2_only_unjustified_themes))
    # the two calls only read
    if chez.eq_p(answers.answer_incoherent_p(answer1), answers.answer_incoherent_p(answer2)):
        incoherence_distance = 0
    else:
        incoherence_distance = 1
    total_distance = chez.add(1, theme_distance, rule_distance,
                              justification_distance, incoherence_distance)
    return total_distance


g_memory = False


def load():
    """memory.ss: (define *memory* (make-memory))"""
    global g_memory
    g_memory = make_memory()
