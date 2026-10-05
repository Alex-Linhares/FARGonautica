"""eeg-graphics.ss: the EEG window (the views' part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from eeg-graphics.ss, with
racket/gui/eeg-graphics.rktl as a worked translation.

port: the EEG object (make-EEG, *EEG*), %EEG-table% and %EEG-buffer-size% are
in the engine (metacat/eeg_graphics.py): workspace.ss and run.ss use them
whether or not the window exists.  This module has the window:
%max-EEG-window-cycles%, %EEG-title-font% (set by select-EEG-font, as in the
original), make-EEG-window and new-EEG-window, whose closure is `EEGWindow`,
delegating to its horizontally scrollable graphics window
(gui/general_graphics.py).  Arithmetic is Chez's (chez.py).  This module does
not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, sugar
from metacat.chez import div, mul, sub
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (exists_p, fifth, filter_map, first, fourth, percent, round_,
                               second, third)

String = chez.String
F = Fraction

p_max_EEG_window_cycles = 400
p_EEG_title_font = False


def select_EEG_font(win_width, win_height):
    """eeg-graphics.ss: select-EEG-font"""
    global p_EEG_title_font
    from metacat.gui import fonts
    desired_font_height = round_(mul(F(18, 120), win_height))
    p_EEG_title_font = fonts.make_mfont(fonts.serif, sub(desired_font_height), ["italic"])


def make_EEG_window(*optional_args):
    """eeg-graphics.ss: make-EEG-window"""
    from metacat.gui import constants as K
    width = K.p_EEG_window_width if len(optional_args) == 0 else first(optional_args)
    height = K.p_EEG_window_height if len(optional_args) < 2 else second(optional_args)
    window = new_EEG_window(width, height)
    tell(window, "initialize")
    return window


def new_EEG_window(x_pixels, y_pixels):
    """eeg-graphics.ss: new-EEG-window"""
    from metacat.gui import constants as K, general_graphics as gg
    select_EEG_font(x_pixels, y_pixels)
    graphics_window = gg.make_horizontal_scrollable_graphics_window(
        x_pixels, y_pixels, K.p_virtual_EEG_length, K.p_EEG_background_color)
    window = EEGWindow(graphics_window, x_pixels, y_pixels)
    tell(graphics_window, "set-icon-label", K.p_EEG_icon_label)
    if exists_p(K.p_EEG_icon_image):
        tell(graphics_window, "set-icon-image", K.p_EEG_icon_image)
    if exists_p(K.p_EEG_window_title):
        tell(graphics_window, "set-window-title", K.p_EEG_window_title)
    return window


plot_p = fifth


class EEGWindow(SchemeObject):
    """eeg-graphics.ss: new-EEG-window's closure"""

    def __init__(this, graphics_window, x_pixels, y_pixels):
        g = graphics_window
        this.graphics_window = g
        this.x_pixels = x_pixels
        this.y_pixels = y_pixels
        this.visible_x_max = tell(g, "get-visible-x-max")
        this.cycle_width = div(this.visible_x_max, p_max_EEG_window_cycles)
        this.title_height = tell(g, "get-string-height", p_EEG_title_font)
        this.title_x = mul(F(1, 2), this.visible_x_max)
        this.title_y = sub(1, this.title_height)
        this.max_height = this.title_y
        this.entries_to_plot = False
        this.colors = False
        this.previous_points = False
        this.num_cycles = False
        this.title = False
        this.pexps = []

    @message("object-type")
    def object_type(this, self):
        return "EEG-window"

    @message("plot-current-values")
    def plot_current_values(this, self):
        g = this.graphics_window
        eeg = _metacat.eeg_graphics.g_EEG
        x = mul(this.num_cycles, this.cycle_width)
        current_points = chez.map_(
            lambda i: [x, mul(percent(tell(eeg, "get-current-value", i)), this.max_height)],
            this.entries_to_plot)
        tell(g, "caching-on")

        def each(p0, p1, c):
            pexp = ["let-sgl", [["foreground-color", c]], ["line", p0, p1]]
            tell(g, "draw", pexp, "curve")
            this.pexps = [pexp] + this.pexps
        sugar.for_star(each, this.previous_points, current_points, this.colors)
        tell(g, "flush")
        this.previous_points = current_points
        this.num_cycles = chez.add1(this.num_cycles)
        return "done"

    @message("initialize")
    def initialize(this, self):
        from metacat.gui import constants as K
        from metacat.rules import punctuate
        g = this.graphics_window
        table = _metacat.eeg_graphics.p_EEG_table
        this.title = String(chez.format_(
            " ~a ",
            punctuate(filter_map(plot_p,
                                 lambda entry: chez.format_("~a (~a)", second(entry),
                                                            third(entry)),
                                 table))))
        tell(g, "clear")
        tell(g, "draw",
             ["let-sgl", [["foreground-color", K.p_EEG_title_color],
                          ["font", p_EEG_title_font],
                          ["text-justification", "center"]],
              ["text", [this.title_x, this.title_y], this.title]],
             "title")
        this.entries_to_plot = filter_map(plot_p, first, table)
        this.colors = filter_map(plot_p, third, table)
        this.previous_points = filter_map(
            plot_p, lambda entry: [0, mul(percent(fourth(entry)), this.max_height)], table)
        this.num_cycles = 0
        this.pexps = []
        return "done"

    @message("resize")
    def resize(this, self, new_width, new_height):
        from metacat.gui import constants as K
        g = this.graphics_window
        this.x_pixels = new_width
        this.y_pixels = new_height
        select_EEG_font(this.x_pixels, this.y_pixels)
        this.title_height = tell(g, "get-string-height", p_EEG_title_font)
        this.title_x = chez.max_(mul(F(1, 2), tell(g, "get-visible-x-max")),
                                 mul(F(1, 2), tell(g, "get-string-width", this.title,
                                                   p_EEG_title_font)))
        this.title_y = sub(1, this.title_height)
        tell(g, "retag", "all", "garbage")
        tell(g, "draw",
             ["let-sgl", [["foreground-color", K.p_EEG_title_color],
                          ["font", p_EEG_title_font],
                          ["text-justification", "center"]],
              ["text", [this.title_x, this.title_y], this.title]],
             "title")
        tell(g, "draw", ["let-sgl", []] + this.pexps)
        return tell(g, "delete", "garbage")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


def load():
    """port: nothing to make at load time (the font is made with the window);
    here for views.load_views's uniform order."""
    return None
