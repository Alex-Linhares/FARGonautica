"""coderack.ss: codelet types, codelets, the coderack and its bins, and codelet posting.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from coderack.ss, with
racket/engine/coderack.rktl as a worked translation.

Closures are SchemeObject classes (docs/python-translation-plan.md, "Objects").
The codelet closure of make-codelet is the Codelet class; it shares the
variables of its codelet type's closure (the codelet count, the selection
probability, the procedure and the window), so it keeps that object as `owner`.
Lists of codelets are Python lists, rebuilt (never mutated) on every cons and
remq, so that a list handed out by get-all-codelets stays as it was.

*codelet-types*, the three codelet-type lists and *coderack* need
codelet-type-list* and so are made by load(), which also makes each codelet type
a module attribute (coderack.bond_builder) and a top-level value.  Globals of
modules translated later (*workspace*, *themespace*, *trace*,
*top-down-slipnodes*, run.ss's *step-mode?* and %step-cycles%) are read through
the package at call time (_metacat.workspace.g_workspace).  The engine never
imports tkinter.
"""
from __future__ import annotations

import sys

import metacat as _metacat
from metacat import chez, sugar
from metacat import setup, view_globals
from metacat.chez import String
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (ascending_index_list, base_object, exists_p, filter_meth,
                               first, floor, hundred_minus, last, make_table, member_p, nth, pause,
                               percent, print_, random_pick, rest, round_, second,
                               stochastic_pick_by_method, sum_, table_ref, table_set_bang,
                               tell_all)

p_max_coderack_size = 100
p_num_of_coderack_bins = 7

# Codelet urgencies

p_extremely_low_urgency = 7
p_very_low_urgency = 21
p_low_urgency = 35
p_medium_urgency = 49
p_high_urgency = 63
p_very_high_urgency = 77
p_extremely_high_urgency = 91


def urgency_name(urgency_value):
    """coderack.ss: urgency-name"""
    if urgency_value <= p_extremely_low_urgency:
        return "extremely-low-urgency"
    if urgency_value <= p_very_low_urgency:
        return "very-low-urgency"
    if urgency_value <= p_low_urgency:
        return "low-urgency"
    if urgency_value <= p_medium_urgency:
        return "medium-urgency"
    if urgency_value <= p_high_urgency:
        return "high-urgency"
    if urgency_value <= p_very_high_urgency:
        return "very-high-urgency"
    return "extremely-high-urgency"


def urgency_color(urgency_value):
    """coderack.ss: urgency-color (the colours are view globals)"""
    v = view_globals
    if urgency_value <= p_extremely_low_urgency:
        return v.p_extremely_low_urgency_color
    if urgency_value <= p_very_low_urgency:
        return v.p_very_low_urgency_color
    if urgency_value <= p_low_urgency:
        return v.p_low_urgency_color
    if urgency_value <= p_medium_urgency:
        return v.p_medium_urgency_color
    if urgency_value <= p_high_urgency:
        return v.p_high_urgency_color
    if urgency_value <= p_very_high_urgency:
        return v.p_very_high_urgency_color
    return v.p_extremely_high_urgency_color


def _make_urgency_value_table():
    table = make_table(p_num_of_coderack_bins, 101)

    def each(coderack_bin_number, temperature):
        table_set_bang(table, coderack_bin_number, temperature,
                       round_(chez.expt(chez.add1(coderack_bin_number),
                                        chez.div(chez.add(hundred_minus(temperature), 10), 15.0))))
    sugar.for_each_table_element_star(table, each)
    return table


p_urgency_value_table = _make_urgency_value_table()


def _step_highlighting_p():
    """(and %coderack-graphics% *step-mode?* (= %step-cycles% 1)) as a test."""
    return (setup.p_coderack_graphics is not False
            and _metacat.run.g_step_mode_p is not False
            and _metacat.run.p_step_cycles == 1)


class CodeletType(SchemeObject):
    """coderack.ss: make-codelet-type (the closure)"""
    __slots__ = ("codelet_type_name", "graphics_labels", "codelet_proc", "codelet_count",
                 "selection_probability", "urgency_clamped_p", "clamped_relative_urgency",
                 "slot_pexp", "get_urgency_pexp", "get_highlight_pexp", "bar_left",
                 "bar_bottom", "bar_top", "codelet_count_coord", "compute_bar_width",
                 "previous_bar_width", "codelet_pattern_value", "coderack_window")

    def __init__(this, codelet_type_name, graphics_labels):
        this.codelet_type_name = codelet_type_name
        this.graphics_labels = graphics_labels
        this.codelet_proc = False
        this.codelet_count = 0
        this.selection_probability = 0
        this.urgency_clamped_p = False
        this.clamped_relative_urgency = False
        this.slot_pexp = False
        this.get_urgency_pexp = False
        this.get_highlight_pexp = False
        this.bar_left = False
        this.bar_bottom = False
        this.bar_top = False
        this.codelet_count_coord = False
        this.compute_bar_width = False
        this.previous_bar_width = 0
        this.codelet_pattern_value = False
        this.coderack_window = False

    @message("object-type")
    def object_type(this, self):
        return "codelet-type"

    @message("print")
    def print_(this, self):
        return chez.printf("Codelet type: ~a~a~n", this.codelet_type_name,
                           chez.format_(" (clamped at urgency ~a)", this.clamped_relative_urgency)
                           if this.urgency_clamped_p else "")

    @message("get-slot-pexp")
    def get_slot_pexp(this, self):
        return this.slot_pexp

    @message("get-codelet-type-name")
    def get_codelet_type_name(this, self):
        return this.codelet_type_name

    @message("clamped?")
    def clamped_p(this, self):
        return this.urgency_clamped_p

    @message("get-clamped-urgency")
    def get_clamped_urgency(this, self):
        return this.clamped_relative_urgency

    @message("clamp")
    def clamp(this, self, urgency):
        if (not this.urgency_clamped_p) or not chez_num_eq(urgency, this.clamped_relative_urgency):
            this.urgency_clamped_p = True
            this.clamped_relative_urgency = urgency
            tell(g_coderack, "set-urgencies", self, urgency)
            if setup.p_coderack_graphics is not False:
                tell(this.coderack_window, "draw", this.get_urgency_pexp(urgency), "urgency")
                if setup.p_codelet_count_graphics is not False:
                    tell(self, "draw-codelet-count")
        return "done"

    @message("unclamp")
    def unclamp(this, self):
        if this.urgency_clamped_p:
            this.urgency_clamped_p = False
            this.clamped_relative_urgency = False
            tell(g_coderack, "reset-urgencies", self)
        return "done"

    @message("get-graphics-labels")
    def get_graphics_labels(this, self):
        if setup.p_codelet_count_graphics is False:
            return this.graphics_labels
        if len(this.graphics_labels) == 1:
            return [String(chez.format_("~as", first(this.graphics_labels)))]
        return [first(this.graphics_labels), String(chez.format_("~as", second(this.graphics_labels)))]

    @message("highlight")
    def highlight(this, self, color):
        tell(this.coderack_window, "draw", this.get_highlight_pexp(color), "highlight")
        if setup.p_codelet_count_graphics is not False:
            tell(this.coderack_window, "draw",
                 ["let-sgl", [["font", view_globals.p_coderack_codelet_count_font],
                              ["background-color", color],
                              ["text-justification", "right"],
                              ["text-mode", "image"]],
                  ["text", this.codelet_count_coord,
                   String(chez.format_("     ~a", this.codelet_count))]],
                 "highlight")
        return "done"

    @message("unhighlight")
    def unhighlight(this, self):
        return tell(this.coderack_window, "delete", "highlight")

    @message("set-graphics-parameters")
    def set_graphics_parameters(this, self, g, pexp, proc1, proc2, coord, x1, y1, y2, bar_proc):
        this.coderack_window = g
        this.slot_pexp = pexp
        this.get_urgency_pexp = proc1
        this.get_highlight_pexp = proc2
        this.codelet_count_coord = coord
        this.bar_left = x1
        this.bar_bottom = y1
        this.bar_top = y2
        this.compute_bar_width = bar_proc
        return "done"

    @message("reset-values-to-zero")
    def reset_values_to_zero(this, self):
        this.codelet_count = 0
        this.selection_probability = 0
        return "done"

    # This must be separate from reset-values-to-zero method:
    @message("reset-bar-width")
    def reset_bar_width(this, self):
        this.previous_bar_width = 0
        return "done"

    @message("draw-graphics")
    def draw_graphics(this, self):
        if this.urgency_clamped_p:
            tell(this.coderack_window, "draw",
                 this.get_urgency_pexp(this.clamped_relative_urgency), "urgency")
        bar_width = this.compute_bar_width(this.selection_probability)
        tell(this.coderack_window, "draw",
             _metacat.general_graphics.solid_box(this.bar_left, this.bar_bottom,
                                                 chez.add(this.bar_left, bar_width), this.bar_top),
             "bar")
        this.previous_bar_width = bar_width
        if setup.p_codelet_count_graphics is not False:
            tell(self, "draw-codelet-count")
        return "done"

    @message("display-urgency")
    def display_urgency(this, self, urgency):
        tell(this.coderack_window, "draw", this.get_urgency_pexp(urgency), "urgency")
        this.codelet_pattern_value = urgency
        return "done"

    @message("draw-pattern-value")
    def draw_pattern_value(this, self):
        if exists_p(this.codelet_pattern_value):
            tell(this.coderack_window, "draw",
                 this.get_urgency_pexp(this.codelet_pattern_value), "urgency")
        return "done"

    @message("clear-pattern-value")
    def clear_pattern_value(this, self):
        this.codelet_pattern_value = False
        return "done"

    @message("update-bar-graphics")
    def update_bar_graphics(this, self):
        bar_width = this.compute_bar_width(this.selection_probability)
        tell(this.coderack_window, "draw",
             _metacat.general_graphics.solid_box(this.bar_left, this.bar_bottom,
                                                 chez.add(this.bar_left, bar_width), this.bar_top),
             "bar")
        this.previous_bar_width = bar_width
        if setup.p_codelet_count_graphics is not False:
            tell(self, "draw-codelet-count")
        return "done"

    @message("draw-codelet-count")
    def draw_codelet_count(this, self):
        return tell(this.coderack_window, "draw-codelet-count",
                    this.codelet_count_coord, this.codelet_count,
                    urgency_color(this.clamped_relative_urgency) if this.urgency_clamped_p
                    else view_globals.p_coderack_background_color)

    @message("set-codelet-procedure")
    def set_codelet_procedure(this, self, proc):
        this.codelet_proc = proc
        return "done"

    @message("make-codelet")
    def make_codelet(this, self, *args):
        # First element of args is always the codelet urgency.  If codelet has
        # a proposed structure, it will always be the second element of args.
        return Codelet(this, self, args)

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def chez_num_eq(a, b):
    """coderack.ss: (= a b) on urgencies (a Chez error if either is not a number)."""
    chez._check(a, "=")
    chez._check(b, "=")
    return a == b


class Codelet(SchemeObject):
    """coderack.ss: make-codelet-type (the closure its make-codelet message makes)"""
    __slots__ = ("owner", "codelet_type", "original_urgency", "relative_urgency", "coderack_bin",
                 "index_in_bin", "codelet_arguments", "proposed_structure_argument_p",
                 "time_stamp")

    def __init__(this, owner, codelet_type, args):
        this.owner = owner                  # the codelet type's closure
        this.codelet_type = codelet_type    # self of the make-codelet message
        this.original_urgency = first(args)
        this.relative_urgency = (owner.clamped_relative_urgency if owner.urgency_clamped_p
                                 else this.original_urgency)
        this.coderack_bin = tell(g_coderack, "get-coderack-bin", this.relative_urgency)
        this.index_in_bin = False
        this.codelet_arguments = list(rest(args))
        # Don't need to worry about description arguments since
        # descriptions aren't structures stored in the Workspace:
        this.proposed_structure_argument_p = (
            len(this.codelet_arguments) > 0
            and member_p(tell(first(this.codelet_arguments), "object-type"), ["bond", "group", "bridge"]))
        this.time_stamp = False

    @message("object-type")
    def object_type(this, self):
        return "codelet"

    @message("print")
    def print_(this, self):
        chez.printf("~a codelet of urgency ~a (time-stamp ~a)~%",
                    this.owner.codelet_type_name, round_(this.relative_urgency), this.time_stamp)
        if len(this.codelet_arguments) > 0:
            chez.printf("Codelet argument:~%")
            print_(first(this.codelet_arguments))
            if len(this.codelet_arguments) == 2:
                return chez.printf("(scope is ~a).~%", tell(second(this.codelet_arguments), "object-type"))
        return None

    @message("run")
    def run(this, self):
        owner = this.owner
        tell(owner.coderack_window, "set-last-codelet-type", this.codelet_type)
        if _step_highlighting_p():
            tell(this.codelet_type, "highlight", view_globals.p_current_codelet_color)
        owner.codelet_proc(*this.codelet_arguments)
        if _step_highlighting_p():
            pause(view_globals.p_codelet_highlight_pause)
            return tell(this.codelet_type, "unhighlight")
        return None

    @message("get-relative-urgency")
    def get_relative_urgency(this, self):
        return this.relative_urgency

    @message("codelet-type?")
    def codelet_type_p(this, self, type_):
        return type_ is this.codelet_type

    @message("get-codelet-type-name")
    def get_codelet_type_name(this, self):
        return this.owner.codelet_type_name

    @message("increment-selection-probability")
    def increment_selection_probability(this, self, delta):
        owner = this.owner
        owner.codelet_count = chez.add1(owner.codelet_count)
        owner.selection_probability = chez.add(delta, owner.selection_probability)
        return "done"

    @message("proposed-structure-argument?")
    def proposed_structure_argument_p_(this, self):
        return this.proposed_structure_argument_p

    @message("get-removal-weight")
    def get_removal_weight(this, self):
        return chez.mul(chez.sub(setup.g_codelet_count, this.time_stamp),
                        chez.add1(chez.sub(tell(g_coderack, "get-highest-bin-urgency"),
                                           tell(this.coderack_bin, "get-urgency"))))

    @message("get-proposed-structure")
    def get_proposed_structure(this, self):
        return first(this.codelet_arguments)

    @message("get-argument")
    def get_argument(this, self, n):
        return nth(n, this.codelet_arguments)

    @message("get-coderack-bin")
    def get_coderack_bin(this, self):
        return this.coderack_bin

    @message("get-index-in-bin")
    def get_index_in_bin(this, self):
        return this.index_in_bin

    @message("get-time-stamp")
    def get_time_stamp(this, self):
        return this.time_stamp

    @message("set-index-in-bin")
    def set_index_in_bin(this, self, index):
        this.index_in_bin = index
        return "done"

    @message("set-time-stamp")
    def set_time_stamp(this, self):
        this.time_stamp = setup.g_codelet_count
        return "done"

    @message("set-urgency")
    def set_urgency(this, self, new_value):
        new_bin = tell(g_coderack, "get-coderack-bin", new_value)
        if new_bin is not this.coderack_bin:
            tell(this.coderack_bin, "remove-codelet", self)
            tell(new_bin, "add-codelet", self)
            this.coderack_bin = new_bin
        this.relative_urgency = new_value
        return "done"

    @message("reset-urgency")
    def reset_urgency(this, self):
        return tell(self, "set-urgency", this.original_urgency)

    @message("adjust-urgency")
    def adjust_urgency(this, self, delta):
        new_value = chez.add(this.relative_urgency, delta)
        new_bin = tell(g_coderack, "get-coderack-bin", new_value)
        if new_bin is not this.coderack_bin:
            tell(this.coderack_bin, "remove-codelet", self)
            tell(new_bin, "add-codelet", self)
            this.coderack_bin = new_bin
        this.relative_urgency = chez.min_(100, chez.max_(0, new_value))
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_codelet_type(codelet_type_name, graphics_labels):
    """coderack.ss: make-codelet-type"""
    return CodeletType(codelet_type_name, graphics_labels)


class CoderackBin(SchemeObject):
    """coderack.ss: make-coderack-bin (the closure)"""
    __slots__ = ("bin_number", "codelet_vector", "current_index", "codelet_list")

    def __init__(this, bin_number):
        this.bin_number = bin_number
        this.codelet_vector = chez.make_vector(p_max_coderack_size)
        this.current_index = 0
        this.codelet_list = []

    @message("object-type")
    def object_type(this, self):
        return "coderack-bin"

    @message("print")
    def print_(this, self):
        return chez.printf("  Coderack bin #~a (~a codelets, bin urgency = ~a)~%",
                           this.bin_number, this.current_index,
                           table_ref(p_urgency_value_table, this.bin_number, setup.g_temperature))

    @message("update-selection-probabilities")
    def update_selection_probabilities(this, self, total_urgency_sum):
        if this.current_index != 0:
            bin_urgency_sum = chez.mul(this.current_index,
                                       table_ref(p_urgency_value_table, this.bin_number,
                                                 setup.g_temperature))
            bin_relative_urgency = chez.div(bin_urgency_sum, total_urgency_sum)
            codelet_probability = chez.div(bin_relative_urgency, this.current_index)
            for codelet in this.codelet_list:
                tell(codelet, "increment-selection-probability", codelet_probability)
        return "done"

    @message("get-codelets")
    def get_codelets(this, self):
        return this.codelet_list

    @message("get-num-of-codelets")
    def get_num_of_codelets(this, self):
        return this.current_index

    @message("get-urgency")
    def get_urgency(this, self):
        return table_ref(p_urgency_value_table, this.bin_number, setup.g_temperature)

    @message("get-urgency-sum")
    def get_urgency_sum(this, self):
        return chez.mul(this.current_index,
                        table_ref(p_urgency_value_table, this.bin_number, setup.g_temperature))

    @message("add-codelet")
    def add_codelet(this, self, codelet):
        this.codelet_vector[this.current_index] = codelet
        tell(codelet, "set-index-in-bin", this.current_index)
        this.current_index = chez.add1(this.current_index)
        this.codelet_list = [codelet] + this.codelet_list
        tell(codelet, "set-time-stamp")
        return "done"

    @message("choose-random-codelet")
    def choose_random_codelet(this, self):
        return this.codelet_vector[chez.random(this.current_index)]

    @message("remove-codelet")
    def remove_codelet(this, self, codelet):
        index = tell(codelet, "get-index-in-bin")
        this.current_index = chez.sub1(this.current_index)
        if index != this.current_index:
            swap_codelet = this.codelet_vector[this.current_index]
            this.codelet_vector[index] = swap_codelet
            tell(swap_codelet, "set-index-in-bin", index)
        tell(codelet, "set-index-in-bin", False)
        this.codelet_list = chez.remq(codelet, this.codelet_list)
        return "done"

    @message("clear-codelets")
    def clear_codelets(this, self):
        this.current_index = 0
        this.codelet_list = []
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_coderack_bin(bin_number):
    """coderack.ss: make-coderack-bin"""
    return CoderackBin(bin_number)


class Coderack(SchemeObject):
    """coderack.ss: make-coderack (the closure)"""
    __slots__ = ("bins", "highest_urgency_bin", "codelet_list", "current_num", "deferred_codelets")

    def __init__(this):
        this.bins = chez.map_(make_coderack_bin, ascending_index_list(p_num_of_coderack_bins))
        this.highest_urgency_bin = last(this.bins)
        this.codelet_list = []
        this.current_num = 0
        # Deferred codelets are added only during calls to (add-top-down-codelets)
        # or (add-bottom-up-codelets). They get posted immediately afterwards, so
        # deferred-codelets should effectively always be '().
        this.deferred_codelets = []

    @message("object-type")
    def object_type(this, self):
        return "coderack"

    @message("print")
    def print_(this, self):
        chez.printf("Coderack:~%")
        result = None
        for b in this.bins:
            result = print_(b)
        return result

    @message("show-bin")
    def show_bin(this, self, n):
        chez.printf("Codelets in coderack bin #~a:~%", n)
        result = None
        for codelet in tell(nth(n, this.bins), "get-codelets"):
            result = print_(codelet)
        return result

    @message("empty?")
    def empty_p(this, self):
        return len(this.codelet_list) == 0

    @message("initialize")
    def initialize(this, self):
        for b in this.bins:
            tell(b, "clear-codelets")
        this.codelet_list = []
        this.current_num = 0
        this.deferred_codelets = []
        result = None
        for codelet_type in g_codelet_types:
            tell(codelet_type, "reset-values-to-zero")
            tell(codelet_type, "reset-bar-width")
            tell(codelet_type, "clear-pattern-value")
            # chez: for*'s value is the last body's (anomalies: "sort, remq, for-each and one-armed if differ")
            result = tell(codelet_type, "unclamp")
        return result

    # this method only affects the graphics, not the actual selection
    # probabilities or numbers of codelets on the coderack:
    @message("update-all-selection-probabilities")
    def update_all_selection_probabilities(this, self):
        for codelet_type in g_codelet_types:
            tell(codelet_type, "reset-values-to-zero")
        total_urgency_sum = sum_(tell_all(this.bins, "get-urgency-sum"))
        if not chez.zero_p(total_urgency_sum):
            for b in this.bins:
                tell(b, "update-selection-probabilities", total_urgency_sum)
        return "done"

    @message("get-num-of-codelets")
    def get_num_of_codelets(this, self):
        return this.current_num

    @message("get-all-codelets")
    def get_all_codelets(this, self):
        return this.codelet_list

    @message("get-codelets-of-type")
    def get_codelets_of_type(this, self, type_):
        return filter_meth(this.codelet_list, "codelet-type?", type_)

    @message("get-all-bins")
    def get_all_bins(this, self):
        return this.bins

    @message("get-total-urgency-sum")
    def get_total_urgency_sum(this, self):
        return sum_(tell_all(this.bins, "get-urgency-sum"))

    @message("get-highest-bin-urgency")
    def get_highest_bin_urgency(this, self):
        return tell(this.highest_urgency_bin, "get-urgency")

    @message("get-coderack-bin")
    def get_coderack_bin(this, self, urgency):
        if urgency >= 100:
            i = chez.sub1(p_num_of_coderack_bins)
        elif urgency <= 0:
            i = 0
        else:
            i = floor(chez.mul(percent(urgency), p_num_of_coderack_bins))
        return nth(i, this.bins)

    @message("add-deferred-codelet")
    def add_deferred_codelet(this, self, c):
        this.deferred_codelets = [c] + this.deferred_codelets
        return "done"

    # deferred-codelets are either bottom-up or top-down scout codelets,
    # and thus never have proposed-structures as arguments, so they can
    # be freely deleted without regard to workspace graphics:
    @message("post-deferred-codelets")
    def post_deferred_codelets(this, self):
        total_deferred = len(this.deferred_codelets)
        if total_deferred >= p_max_coderack_size:
            excess_deferred = total_deferred - p_max_coderack_size
            if excess_deferred > 0:
                for _ in range(excess_deferred):
                    c = random_pick(this.deferred_codelets)
                    this.deferred_codelets = chez.remq(c, this.deferred_codelets)
            tell(self, "delete-all-codelets")
        else:
            num_to_delete = (this.current_num + total_deferred) - p_max_coderack_size
            if num_to_delete > 0:
                tell(self, "delete-codelets", num_to_delete)
        for codelet in this.deferred_codelets:
            b = tell(codelet, "get-coderack-bin")
            tell(b, "add-codelet", codelet)
            this.codelet_list = [codelet] + this.codelet_list
            this.current_num = chez.add1(this.current_num)
        this.deferred_codelets = []
        return "done"

    @message("post")
    def post(this, self, codelet):
        if this.current_num == p_max_coderack_size:
            tell(self, "delete-codelets", 1)
        b = tell(codelet, "get-coderack-bin")
        tell(b, "add-codelet", codelet)
        this.codelet_list = [codelet] + this.codelet_list
        this.current_num = chez.add1(this.current_num)
        return "done"

    @message("choose-codelet")
    def choose_codelet(this, self):
        # Coderack should never be empty here:
        b = stochastic_pick_by_method(this.bins, "get-urgency-sum")
        codelet = tell(b, "choose-random-codelet")
        tell(b, "remove-codelet", codelet)
        this.codelet_list = chez.remq(codelet, this.codelet_list)
        this.current_num = chez.sub1(this.current_num)
        return codelet

    @message("delete-all-codelets")
    def delete_all_codelets(this, self):
        for codelet in this.codelet_list:
            if tell(codelet, "proposed-structure-argument?"):
                tell(_metacat.workspace.g_workspace, "delete-proposed-structure",
                     tell(codelet, "get-proposed-structure"))
        for b in this.bins:
            tell(b, "clear-codelets")
        this.codelet_list = []
        this.current_num = 0
        return "done"

    @message("delete-codelets")
    def delete_codelets(this, self, num_to_delete):
        for _ in range(num_to_delete):
            codelet = stochastic_pick_by_method(this.codelet_list, "get-removal-weight")
            b = tell(codelet, "get-coderack-bin")
            if tell(codelet, "proposed-structure-argument?"):
                tell(_metacat.workspace.g_workspace, "delete-proposed-structure",
                     tell(codelet, "get-proposed-structure"))
            tell(b, "remove-codelet", codelet)
            this.codelet_list = chez.remq(codelet, this.codelet_list)
        this.current_num = chez.sub(this.current_num, num_to_delete)
        return "done"

    @message("set-urgencies")
    def set_urgencies(this, self, codelet_type, new_value):
        for codelet in this.codelet_list:
            if tell(codelet, "codelet-type?", codelet_type):
                tell(codelet, "set-urgency", new_value)
        return "done"

    @message("reset-urgencies")
    def reset_urgencies(this, self, codelet_type):
        for codelet in this.codelet_list:
            if tell(codelet, "codelet-type?", codelet_type):
                tell(codelet, "reset-urgency")
        return "done"

    @message("adjust-urgencies")
    def adjust_urgencies(this, self, codelet_type, delta):
        for codelet in this.codelet_list:
            if tell(codelet, "codelet-type?", codelet_type):
                tell(codelet, "adjust-urgency", delta)
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def make_coderack():
    """coderack.ss: make-coderack"""
    return Coderack()


def post_codelet_probability(codelet_type):
    """coderack.ss: post-codelet-probability"""
    ws = _metacat.workspace.g_workspace
    if ((setup.p_justify_mode is not False and codelet_type is answer_finder)
            or (setup.p_justify_mode is False and codelet_type is answer_justifier)
            or (setup.p_self_watching_enabled is False
                and member_p(codelet_type, g_self_watching_codelet_types))):
        return 0
    if tell(codelet_type, "clamped?"):
        return percent(tell(codelet_type, "get-clamped-urgency"))
    name = tell(codelet_type, "get-codelet-type-name")
    if name in ("bottom-up-bond-scout", "top-down-bond-scout:category",
                "top-down-bond-scout:direction", "top-down-group-scout:category",
                "top-down-group-scout:direction", "group-scout:whole-string"):
        return percent(tell(ws, "get-average-intra-string-unhappiness"))
    if name in ("bottom-up-bridge-scout", "important-object-bridge-scout"):
        return percent(hundred_minus(tell(ws, "get-min-mapping-strength")))
    if name in ("bottom-up-description-scout", "top-down-description-scout"):
        return percent(tell(ws, "get-average-unhappiness"))
    if name == "rule-scout":
        return 0.5 if len(tell(ws, "get-possible-rule-types")) == 0 else 1
    if name == "answer-finder":
        # chez: #f only (supported-rule-exists? may answer a list)
        if tell(ws, "supported-rule-exists?", "top") is not False:
            return percent(hundred_minus(setup.g_temperature))
        return 0
    if name == "answer-justifier":
        if (tell(ws, "supported-rule-exists?", "top") is not False
                or tell(ws, "supported-rule-exists?", "bottom") is not False):
            return percent(hundred_minus(setup.g_temperature))
        return 0
    if name == "breaker":
        return percent(setup.g_temperature)
    if name == "progress-watcher":
        return 1 if tell(_metacat.themes.g_themespace, "thematic-pressure?") is not False else 0.25
    if name == "jootser":
        trace = _metacat.trace.g_trace
        if (tell(trace, "within-snag-period?") is not False
                or tell(trace, "within-clamp-period?") is not False):
            return 0.4
        return 0.1
    if name == "thematic-bridge-scout":
        themespace = _metacat.themes.g_themespace
        active_bridge_theme_types = tell(themespace, "get-active-bridge-theme-types")
        if len(active_bridge_theme_types) == 0:
            return 0
        return percent(tell(themespace, "get-max-positive-theme-activation", active_bridge_theme_types))
    # 1.2: a case without else; the other codelet types give void
    return None


def num_of_codelets_to_post(codelet_type):
    """coderack.ss: num-of-codelets-to-post"""
    ws = _metacat.workspace.g_workspace
    if ((setup.p_justify_mode is not False and codelet_type is answer_finder)
            or (setup.p_justify_mode is False and codelet_type is answer_justifier)):
        return 0
    name = tell(codelet_type, "get-codelet-type-name")
    if name in ("bottom-up-bond-scout", "top-down-bond-scout:category",
                "top-down-bond-scout:direction"):
        return _case_rough(tell(ws, "get-rough-num-of-unrelated-objects"), 2, 4, 6)
    if name in ("top-down-group-scout:category", "top-down-group-scout:direction",
                "group-scout:whole-string"):
        if len(tell(ws, "get-bonds")) == 0:
            return 0
        return _case_rough(tell(ws, "get-rough-num-of-ungrouped-objects"), 1, 2, 3)
    if name in ("bottom-up-bridge-scout", "important-object-bridge-scout"):
        return _case_rough(tell(ws, "get-rough-num-of-unmapped-objects"), 2, 5, 6)
    if name in ("bottom-up-description-scout", "top-down-description-scout"):
        return 2
    if name == "rule-scout":
        return chez.max_(1, chez.mul(2, len(tell(ws, "get-possible-rule-types"))))
    if name in ("answer-finder", "answer-justifier", "breaker"):
        return 1
    if name == "thematic-bridge-scout":
        return round_(chez.mul(10, percent(tell(ws, "get-max-inter-string-unhappiness"))))
    if name == "progress-watcher":
        return 2
    if name == "jootser":
        return 1 if setup.p_justify_mode is not False else 2
    # 1.2: a case without else; the other codelet types give void
    return None


def _case_rough(amount, few, some, many):
    """(case amount (few ...) (some ...) (many ...)), void otherwise."""
    if amount == "few":
        return few
    if amount == "some":
        return some
    if amount == "many":
        return many
    return None


def add_top_down_codelets():
    """coderack.ss: add-top-down-codelets"""
    for node in _metacat.slipnet.g_top_down_slipnodes:
        tell(node, "attempt-to-post-top-down-codelets")
    for codelet_type in g_thematic_codelet_types:
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < post_codelet_probability(codelet_type):
            urgency = thematic_codelet_urgency(codelet_type)
            for _ in range(num_of_codelets_to_post(codelet_type)):
                tell(g_coderack, "add-deferred-codelet", tell(codelet_type, "make-codelet", urgency))


def add_bottom_up_codelets():
    """coderack.ss: add-bottom-up-codelets"""
    for codelet_type in g_bottom_up_codelet_types:
        # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
        coin_flip = chez.random(1.0)
        if coin_flip < post_codelet_probability(codelet_type):
            urgency = bottom_up_urgency(codelet_type)
            for _ in range(num_of_codelets_to_post(codelet_type)):
                tell(g_coderack, "add-deferred-codelet", tell(codelet_type, "make-codelet", urgency))


def bottom_up_urgency(codelet_type):
    """coderack.ss: bottom-up-urgency"""
    name = tell(codelet_type, "get-codelet-type-name")
    if name in ("answer-finder", "answer-justifier"):
        return hundred_minus(setup.g_temperature)
    if name == "breaker":
        return p_extremely_low_urgency
    if name in ("progress-watcher", "jootser"):
        return p_medium_urgency
    return p_low_urgency


def thematic_codelet_urgency(codelet_type):
    """coderack.ss: thematic-codelet-urgency"""
    if tell(codelet_type, "get-codelet-type-name") == "thematic-bridge-scout":
        themespace = _metacat.themes.g_themespace
        return tell(themespace, "get-max-positive-theme-activation",
                    tell(themespace, "get-active-bridge-theme-types"))
    # 1.2: a case without else
    return None


# *codelet-types* must be defined before the coderack is defined,
# before any codelet-type-procedures are defined, and before any
# top-down codelet-types are attached to slipnodes.

CODELET_TYPE_SPECS = [
    ("bottom-up-bond-scout", ["Bottom-up", "bond scout"]),
    ("top-down-bond-scout:category", ["Top-down bond", "(category) scout"]),
    ("top-down-bond-scout:direction", ["Top-down bond", "(direction) scout"]),
    ("bond-evaluator", ["Bond evaluator"]),
    ("bond-builder", ["Bond builder"]),
    ("top-down-group-scout:category", ["Top-down group", "(category) scout"]),
    ("top-down-group-scout:direction", ["Top-down group", "(direction) scout"]),
    ("group-scout:whole-string", ["Whole-string", "group scout"]),
    ("group-evaluator", ["Group evaluator"]),
    ("group-builder", ["Group builder"]),
    ("bottom-up-bridge-scout", ["Bottom-up", "bridge scout"]),
    ("important-object-bridge-scout", ["Important-object", "bridge scout"]),
    ("bridge-evaluator", ["Bridge", "evaluator"]),
    ("bridge-builder", ["Bridge", "builder"]),
    ("bottom-up-description-scout", ["Bottom-up", "descrip. scout"]),
    ("top-down-description-scout", ["Top-down", "descrip. scout"]),
    ("description-evaluator", ["Description", "evaluator"]),
    ("description-builder", ["Description", "builder"]),
    ("rule-scout", ["Rule scout"]),
    ("rule-evaluator", ["Rule evaluator"]),
    ("rule-builder", ["Rule builder"]),
    ("answer-finder", ["Answer finder"]),
    ("answer-justifier", ["Answer justifier"]),
    ("thematic-bridge-scout", ["Thematic", "bridge scout"]),
    ("progress-watcher", ["Progress watcher"]),
    ("jootser", ["Jootser"]),
    ("breaker", ["Breaker"]),
]


def load():
    """coderack.ss: the top-level defines of *codelet-types* (each codelet type
    also a module attribute and top-level value), *thematic-codelet-types*,
    *bottom-up-codelet-types*, *self-watching-codelet-types* and *coderack*, in
    the file's order."""
    global g_codelet_types, g_thematic_codelet_types, g_bottom_up_codelet_types
    global g_self_watching_codelet_types, g_coderack
    this = sys.modules[__name__]
    # chez: the labels are strings, which the panels draw as text (anomalies: "The graphics and rules.ss tell strings from symbols")
    g_codelet_types = sugar.codelet_type_list_star(
        [(name, [String(label) for label in labels]) for name, labels in CODELET_TYPE_SPECS],
        module=this)
    g_thematic_codelet_types = [thematic_bridge_scout]
    g_bottom_up_codelet_types = [
        bottom_up_bond_scout,
        group_scout__whole_string,
        bottom_up_bridge_scout,
        important_object_bridge_scout,
        bottom_up_description_scout,
        rule_scout,
        answer_finder,
        answer_justifier,
        progress_watcher,
        jootser,
        breaker]
    g_self_watching_codelet_types = [
        thematic_bridge_scout,
        progress_watcher,
        jootser]
    g_coderack = make_coderack()
