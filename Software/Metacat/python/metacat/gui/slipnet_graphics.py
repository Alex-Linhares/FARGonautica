"""slipnet-graphics.ss: the Slipnet window and its 13x5 layout.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from slipnet-graphics.ss, with
racket/gui/slipnet-graphics.rktl as a worked translation.

new-slipnet-window's closure is `SlipnetWindow`, delegating to its unscrollable
graphics window (gui/general_graphics.py).  Making the window gives every
slipnode in the layout its graphics coordinates (set-graphics-coord), which only
the graphics read.  port: %slipnet-title-font% and %slipnode-label-font%, which
the original creates by set! in select-slipnet-fonts, are module globals here
(as in racket/gui); *13x5-layout-table* is made by `load()`, after the slipnet.
The original's commented-out check-for-label-overlap is not translated.  This
module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, setup, sugar
from metacat.chez import add, div, mul, sub
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (column_dimension, exists_p, first, percent, rest, round_,
                               row_dimension, second, table_ref, ceiling)

String = chez.String
F = Fraction

# port: the original never defines these two fonts (select-slipnet-fonts
# creates them by set! on the top level)
p_slipnet_title_font = False
p_slipnode_label_font = False


def select_slipnet_fonts(win_width, win_height):
    """slipnet-graphics.ss: select-slipnet-fonts"""
    global p_slipnet_title_font, p_slipnode_label_font
    from metacat.gui import fonts
    desired_title_height = round_(mul(F(80, 1000), win_height))
    desired_label_height = round_(mul(F(35, 1000), win_height))
    p_slipnet_title_font = fonts.make_mfont(fonts.sans_serif, sub(desired_title_height),
                                            ["bold", "italic"])
    p_slipnode_label_font = fonts.make_mfont(fonts.fancy, sub(desired_label_height),
                                             ["italic"])


def make_slipnet_window(slipnet_layout_table, *optional_args):
    """slipnet-graphics.ss: make-slipnet-window"""
    from metacat.gui import constants as K
    width = K.p_default_13x5_slipnet_width if len(optional_args) == 0 else first(optional_args)
    window = new_slipnet_window(slipnet_layout_table, width)
    tell(window, "initialize")
    return window


def new_slipnet_window(slipnet_layout_table, x_pixels):
    """slipnet-graphics.ss: new-slipnet-window"""
    from metacat.gui import constants as K, general_graphics as gg
    x_dim = row_dimension(slipnet_layout_table)
    y_dim = column_dimension(slipnet_layout_table)
    x_deltas = add(mul(3, x_dim), 1)
    y_deltas = add(mul(3, y_dim), 4)
    y_pixels = ceiling(mul(y_deltas, div(x_pixels, x_deltas)))
    delta = div(1, x_deltas)
    graphics_window = gg.make_unscrollable_graphics_window(
        x_pixels, y_pixels, K.p_slipnet_background_color)
    tell(graphics_window, "set-icon-label", K.p_slipnet_icon_label)
    if exists_p(K.p_slipnet_icon_image):
        tell(graphics_window, "set-icon-image", K.p_slipnet_icon_image)
    if exists_p(K.p_slipnet_window_title):
        tell(graphics_window, "set-window-title", K.p_slipnet_window_title)
    select_slipnet_fonts(x_pixels, y_pixels)
    slipnode_labels_pexp = []

    def each(i, j):
        nonlocal slipnode_labels_pexp
        slipnode = table_ref(slipnet_layout_table, i, j)
        if exists_p(slipnode):
            x = add(mul(mul(3, delta), i), mul(2, delta))
            y = add(mul(mul(3, delta), j), mul(3, delta))
            center = [x, y]
            label_coord = [x, sub(y, mul(F(13, 8), delta))]
            tell(slipnode, "set-graphics-coord", center)
            tell(slipnode, "set-graphics-label-coord", label_coord)
            slipnode_labels_pexp = ([["text", label_coord, tell(slipnode, "get-short-name")]]
                                    + slipnode_labels_pexp)
    sugar.for_each_table_element_star(slipnet_layout_table, each)
    slipnode_labels_pexp = (["let-sgl", [["text-justification", "center"]]]
                            + slipnode_labels_pexp)
    return SlipnetWindow(graphics_window, x_pixels, y_pixels, delta, y_deltas,
                         slipnode_labels_pexp)


class SlipnetWindow(SchemeObject):
    """slipnet-graphics.ss: new-slipnet-window's closure"""

    def __init__(this, graphics_window, x_pixels, y_pixels, delta, y_deltas,
                 slipnode_labels_pexp):
        this.graphics_window = graphics_window
        this.x_pixels = x_pixels
        this.y_pixels = y_pixels
        this.max_activation_diameter = mul(2, delta)
        this.title_coord = [F(1, 2), mul(sub(y_deltas, 2), delta)]
        this.current_title_pexp = False
        this.slipnode_labels_pexp = slipnode_labels_pexp

    @message("object-type")
    def object_type(this, self):
        return "slipnet-window"

    @message("garbage-collect")
    def garbage_collect(this, self):
        return tell(self, "update-graphics")

    @message("update-graphics")
    def update_graphics(this, self):
        g = this.graphics_window
        tell(g, "retag", "activation", "garbage")
        sugar.for_star(lambda node: tell(node, "draw-activation-graphics", self),
                       _metacat.slipnet.g_slipnet_nodes)
        return tell(g, "delete", "garbage")

    @message("erase-all-activations")
    def erase_all_activations(this, self):
        return tell(this.graphics_window, "delete", "activation")

    @message("draw-activation")
    def draw_activation(this, self, center, activation, frozen_p):
        from metacat.gui import constants as K
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["foreground-color",
                                  K.p_frozen_slipnode_activation_color if frozen_p is not False
                                  else K.p_slipnode_activation_color]],
                     _metacat.general_graphics.disk(
                         center, mul(percent(activation), this.max_activation_diameter))],
                    "activation")

    @message("draw-title")
    def draw_title(this, self, text):
        tell(self, "erase-title")
        if exists_p(text):
            this.current_title_pexp = ["let-sgl", [["text-justification", "center"]],
                                       ["text", this.title_coord, text]]
            tell(this.graphics_window, "draw",
                 ["let-sgl", [["font", p_slipnet_title_font]], this.current_title_pexp],
                 "title")
        return "done"

    @message("erase-title")
    def erase_title(this, self):
        if exists_p(this.current_title_pexp):
            tell(this.graphics_window, "delete", "title")
            this.current_title_pexp = False
        return "done"

    @message("resize")
    def resize(this, self, new_width, new_height):
        g = this.graphics_window
        old_width = this.x_pixels
        old_height = this.y_pixels
        this.x_pixels = new_width
        this.y_pixels = new_height
        select_slipnet_fonts(this.x_pixels, this.y_pixels)
        tell(g, "retag", "title", "garbage")
        tell(g, "retag", "label", "garbage")
        tell(g, "draw", ["let-sgl", [["font", p_slipnet_title_font]], this.current_title_pexp],
             "title")
        tell(g, "draw", ["let-sgl", [["font", p_slipnode_label_font]],
                         this.slipnode_labels_pexp],
             "label")
        tell(g, "delete", "garbage")
        return tell(g, "rescale", "activation",
                    div(new_width, old_width), div(new_height, old_height))

    @message("initialize")
    def initialize(this, self):
        g = this.graphics_window
        tell(g, "clear")
        tell(self, "draw-title", String("Slipnet Activation"))
        return tell(g, "draw", ["let-sgl", [["font", p_slipnode_label_font]],
                                this.slipnode_labels_pexp],
                    "label")

    @message("clear")
    def clear(this, self):
        return tell(self, "blank-window")

    @message("blank-window")
    def blank_window(this, self):
        tell(self, "draw-title", String("Slipnet Activation"))
        return tell(this.graphics_window, "delete", "activation")

    # title = #f just erases the current title (if any)
    @message("display-patterns")
    def display_patterns(this, self, concept_patterns, color, title):
        tell(self, "draw-title", title)
        tell(this.graphics_window, "delete", "activation")

        def each_pattern(pattern):
            return sugar.for_star(
                lambda entry: tell(self, "display-activation", first(entry), second(entry),
                                   color),
                rest(pattern))
        return sugar.for_star(each_pattern, concept_patterns)

    @message("display-activation")
    def display_activation(this, self, node, value, color):
        center = tell(node, "get-graphics-coord")
        gr = _metacat.general_graphics
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [],
                     ["let-sgl", [["foreground-color", color]],
                      gr.disk(center, mul(percent(value), this.max_activation_diameter))],
                     gr.circle(center, this.max_activation_diameter)],
                    "activation")

    @message("restore-current-state")
    def restore_current_state(this, self):
        tell(self, "draw-title", String("Slipnet Activation"))
        tell(this.graphics_window, "delete", "activation")
        if setup.p_slipnet_graphics is not False:
            return sugar.for_star(lambda node: tell(node, "draw-activation-graphics", self),
                                  _metacat.slipnet.g_slipnet_nodes)
        return None

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


g_13x5_layout_table = False


def load():
    """port: slipnet-graphics.ss's *13x5-layout-table*, made after the slipnet."""
    global g_13x5_layout_table
    s = _metacat.slipnet
    g_13x5_layout_table = sugar.slipnet_layout_table_star([
        [s.plato_opposite, s.plato_string_position_category, s.plato_leftmost,
         s.plato_middle, s.plato_rightmost, s.plato_whole, s.plato_single,
         s.plato_object_category, s.plato_letter, s.plato_group,
         s.plato_alphabetic_position_category, s.plato_alphabetic_first,
         s.plato_alphabetic_last],
        [s.plato_identity, s.plato_direction_category, s.plato_left, s.plato_right,
         s.plato_bond_category, s.plato_predecessor, s.plato_successor, s.plato_sameness,
         s.plato_group_category, s.plato_predgrp, s.plato_succgrp, s.plato_samegrp,
         s.plato_letter_category],
        [s.plato_a, s.plato_b, s.plato_c, s.plato_d, s.plato_e, s.plato_f, s.plato_g,
         s.plato_h, s.plato_i, s.plato_j, s.plato_k, s.plato_l, s.plato_m],
        [s.plato_n, s.plato_o, s.plato_p, s.plato_q, s.plato_r, s.plato_s, s.plato_t,
         s.plato_u, s.plato_v, s.plato_w, s.plato_x, s.plato_y, s.plato_z],
        [False, False, False, s.plato_length, s.plato_one, s.plato_two, s.plato_three,
         s.plato_four, s.plato_five, s.plato_bond_facet, False, False, False]])
