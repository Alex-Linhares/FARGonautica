"""coderack-graphics.ss: the Coderack window.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from coderack-graphics.ss, with
racket/gui/coderack-graphics.rktl as a worked translation.

new-coderack-window's closure is `CoderackWindow`, delegating to its
unscrollable graphics window (gui/general_graphics.py).  Its
initialize-parameters lays out one slot per codelet type and hands each type
its slot (coderack.ss's set-graphics-parameters): the codelet types then draw
their own bars, urgencies and counts on this window.

port: the fonts select-coderack-fonts creates by set! are module globals here,
except %coderack-codelet-count-font%, which coderack.ss's codelet types read:
it is an engine global (metacat/view_globals.py), set with engine.set_global,
as racket/engine/view-globals.rktl has it.  Arithmetic is Chez's (chez.py).
This module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, engine, setup, sugar, view_globals
from metacat.chez import add, div, mul, sub
from metacat.objects import SchemeObject, delegate, message, tell, tell_all
from metacat.utilities import (ceiling, exists_p, first, maximum, nth, rest, round_,
                               second)

String = chez.String
F = Fraction

# port: the original never defines these fonts (select-coderack-fonts creates
# them by set! on the top level); %coderack-codelet-count-font% is
# view_globals.p_coderack_codelet_count_font
p_coderack_title_font = False
p_coderack_subtitle_font = False
p_coderack_codelet_type_font = False
p_coderack_codelet_sum_font = False


def select_coderack_fonts(win_width, win_height):
    """coderack-graphics.ss: select-coderack-fonts"""
    global p_coderack_title_font, p_coderack_subtitle_font, p_coderack_codelet_type_font
    global p_coderack_codelet_sum_font
    from metacat.gui import fonts
    desired_title_height = round_(mul(F(40, 1000), win_height))
    desired_subtitle_height = round_(mul(F(20, 1000), win_height))
    desired_type_height = round_(mul(F(14, 1000), win_height))
    desired_count_height = round_(mul(F(19, 1000), win_height))
    desired_sum_height = round_(mul(F(19, 1000), win_height))
    p_coderack_title_font = fonts.make_mfont(fonts.sans_serif, sub(desired_title_height),
                                             ["bold", "italic"])
    p_coderack_subtitle_font = fonts.make_mfont(fonts.serif, sub(desired_subtitle_height),
                                                ["bold", "italic"])
    p_coderack_codelet_type_font = fonts.make_mfont(fonts.sans_serif,
                                                    sub(desired_type_height), ["normal"])
    engine.set_global("%coderack-codelet-count-font%",
                      fonts.make_mfont(fonts.sans_serif, sub(desired_count_height),
                                       ["normal"]))
    p_coderack_codelet_sum_font = fonts.make_mfont(fonts.sans_serif, sub(desired_sum_height),
                                                   ["normal"])


def make_coderack_window(*optional_args):
    """coderack-graphics.ss: make-coderack-window"""
    from metacat.gui import constants as K
    width = K.p_default_coderack_width if len(optional_args) == 0 else first(optional_args)
    window = new_coderack_window(width)
    tell(window, "initialize")
    return window


def new_coderack_window(x_pixels):
    """coderack-graphics.ss: new-coderack-window"""
    from metacat.gui import constants as K, general_graphics as gg
    window_height = F(26, 10)
    y_pixels = ceiling(mul(window_height, x_pixels))
    graphics_window = gg.make_unscrollable_graphics_window(
        x_pixels, y_pixels, K.p_coderack_background_color)
    window = CoderackWindow(graphics_window, x_pixels, y_pixels, window_height)
    tell(graphics_window, "set-icon-label", K.p_coderack_icon_label)
    if exists_p(K.p_coderack_icon_image):
        tell(graphics_window, "set-icon-image", K.p_coderack_icon_image)
    if exists_p(K.p_coderack_window_title):
        tell(graphics_window, "set-window-title", K.p_coderack_window_title)
    return window


def _codelet_types():
    return _metacat.coderack.g_codelet_types


def _coderack():
    return _metacat.coderack.g_coderack


class CoderackWindow(SchemeObject):
    """coderack-graphics.ss: new-coderack-window's closure"""

    def __init__(this, graphics_window, x_pixels, y_pixels, window_height):
        this.graphics_window = graphics_window
        this.x_pixels = x_pixels
        this.y_pixels = y_pixels
        this.window_height = window_height
        this.coderack_top = sub(window_height, F(2, 5))
        this.num_codelet_types = len(_codelet_types())
        this.max_bar_right = F(19, 20)
        this.title_coord = [F(1, 2), sub(window_height, F(4, 25))]
        this.pixel_w = False
        this.pixel_h = False
        this.double_line_thickness = False
        this.slot_height = False
        this.max_codelet_count_width = False
        this.codelet_sum_font_height = False
        this.bar_height = False
        this.max_label_width = False
        this.slot_right = False
        this.label_x = False
        this.sum_coord = False
        this.sum_label_coord = False
        this.sum_y = False
        this.bar_left = False
        this.max_bar_width = False
        this.slot_title_coord = False
        this.prob_title_coord = False
        this.prob_title = False
        this.skeleton_graphics_pexp = False
        this.current_title_pexp = False
        this.current_title = String("Coderack")
        this.last_codelet_type = False
        this.display_state = "normal"

    @message("object-type")
    def object_type(this, self):
        return "coderack-window"

    @message("get-display-state")
    def get_display_state(this, self):
        return this.display_state

    @message("get-last-codelet-type")
    def get_last_codelet_type(this, self):
        return this.last_codelet_type

    @message("set-last-codelet-type")
    def set_last_codelet_type(this, self, codelet_type):
        if setup.p_coderack_graphics is not False and exists_p(this.last_codelet_type):
            tell(this.last_codelet_type, "unhighlight")
        this.last_codelet_type = codelet_type
        return "done"

    @message("highlight-last-codelet")
    def highlight_last_codelet(this, self):
        from metacat.gui import constants as K
        if exists_p(this.last_codelet_type):
            tell(this.last_codelet_type, "highlight", K.p_last_codelet_color)
        return "done"

    @message("unhighlight-last-codelet")
    def unhighlight_last_codelet(this, self):
        if exists_p(this.last_codelet_type):
            tell(this.last_codelet_type, "unhighlight")
        return "done"

    @message("initialize-parameters")
    def initialize_parameters(this, self):
        g = this.graphics_window
        this.x_pixels = tell(g, "get-visible-w")
        this.y_pixels = tell(g, "get-visible-h")
        select_coderack_fonts(this.x_pixels, this.y_pixels)
        this.pixel_w = tell(g, "get-width-per-pixel")
        this.pixel_h = tell(g, "get-height-per-pixel")
        this.double_line_thickness = mul(2, this.pixel_h)
        this.slot_height = div(sub(this.coderack_top, this.double_line_thickness),
                               add(1, this.num_codelet_types))
        this.max_codelet_count_width = tell(g, "get-string-width", String("100"),
                                            view_globals.p_coderack_codelet_count_font)
        this.codelet_sum_font_height = tell(g, "get-character-height", String(" "),
                                            p_coderack_codelet_sum_font)
        this.bar_height = mul(F(3, 4), this.slot_height)
        this.sum_y = sub(mul(F(1, 2), this.slot_height),
                         mul(F(3, 10), this.codelet_sum_font_height))
        labels = []
        for ls in tell_all(_codelet_types(), "get-graphics-labels"):
            labels = labels + list(ls)
        this.max_label_width = maximum(chez.map_(
            lambda label: tell(g, "get-string-width", label, p_coderack_codelet_type_font),
            labels))
        if setup.p_codelet_count_graphics is not False:
            this.slot_right = add(mul(F(3, 2), this.max_codelet_count_width),
                                  this.max_label_width)
            this.label_x = mul(F(5, 4), this.max_codelet_count_width)
            this.sum_coord = [mul(F(2, 5), this.slot_right), this.sum_y]
            this.sum_label_coord = [mul(F(9, 20), this.slot_right), this.sum_y]
        else:
            this.slot_right = add(mul(F(1, 2), this.max_codelet_count_width),
                                  this.max_label_width)
            this.label_x = mul(F(1, 4), this.max_codelet_count_width)
        this.bar_left = add(this.slot_right, this.pixel_w)
        this.max_bar_width = sub(this.max_bar_right, this.bar_left)
        this.slot_title_coord = [mul(F(1, 2), this.slot_right),
                                 add(this.coderack_top, this.bar_height)]
        prob_title_x = div(add(this.slot_right, 1), 2)
        prob_title_width = tell(g, "get-string-width", String("Selection Probability"),
                                p_coderack_subtitle_font)
        this.prob_title_coord = [prob_title_x, add(this.coderack_top, this.bar_height)]
        this.prob_title = (String("Probability")
                           if add(prob_title_x, div(prob_title_width, 2)) > 1
                           else String("Selection Probability"))
        sugar.for_star_from_to(0, sub(this.num_codelet_types, 1),
                               lambda i: this._set_slot(self, i))
        this.skeleton_graphics_pexp = (
            ["let-sgl", [],
             ["let-sgl", [["font", p_coderack_subtitle_font],
                          ["text-justification", "center"]],
              ["text", this.slot_title_coord, String("Codelet Type")],
              ["text", this.prob_title_coord, this.prob_title]],
             ["line", [this.slot_right, this.coderack_top], [this.slot_right, 0]]]
            + list(tell_all(_codelet_types(), "get-slot-pexp"))
            + [["line", [0, this.slot_height], [this.slot_right, this.slot_height]],
               ["line", [0, add(this.slot_height, this.double_line_thickness)],
                [this.slot_right, add(this.slot_height, this.double_line_thickness)]]])
        return "done"

    def _set_slot(this, self, i):
        # the body of initialize-parameters's (for* i from 0 to ...) loop
        slot_height = this.slot_height
        codelet_type = nth(i, _codelet_types())
        top_y = sub(this.coderack_top, mul(i, slot_height))
        top_label_y = sub(top_y, mul(F(9, 20), slot_height))
        bot_y = sub(this.coderack_top, mul(add(i, 1), slot_height))
        bot_label_y = sub(top_y, mul(F(17, 20), slot_height))
        mid_label_y = div(add(top_label_y, bot_label_y), 2)
        codelet_count_coord = ([this.max_codelet_count_width,
                                sub(top_y, mul(F(13, 20), slot_height))]
                               if setup.p_codelet_count_graphics is not False else False)
        bar_top = sub(top_y, mul(F(1, 2), sub(slot_height, this.bar_height)))
        bar_bottom = sub(bar_top, this.bar_height)
        labels = tell(codelet_type, "get-graphics-labels")
        if len(labels) == 1:
            label_pexp = ["let-sgl", [["font", p_coderack_codelet_type_font]],
                          ["text", [this.label_x, mid_label_y], first(labels)]]
        else:
            label_pexp = ["let-sgl", [["font", p_coderack_codelet_type_font]],
                          ["text", [this.label_x, top_label_y], first(labels)],
                          ["text", [this.label_x, bot_label_y], second(labels)]]
        slot_pexp = ["let-sgl", [],
                     ["line", [0, top_y], [this.slot_right, top_y]],
                     label_pexp]
        slot_background_pexp = _metacat.general_graphics.solid_box(
            0, add(bot_y, this.pixel_h), sub(this.slot_right, this.pixel_w),
            sub(top_y, this.pixel_h))

        def get_urgency_pexp(urgency):
            return ["let-sgl", [],
                    ["let-sgl", [["foreground-color", _metacat.coderack.urgency_color(urgency)]],
                     slot_background_pexp],
                    label_pexp]

        def get_highlight_pexp(color):
            return ["let-sgl", [],
                    ["let-sgl", [["foreground-color", color]],
                     slot_background_pexp],
                    label_pexp]

        pixel_w = this.pixel_w
        max_bar_width = this.max_bar_width

        def bar_width(prob):
            if chez.zero_p(prob):
                return 0
            # Show at least a little bar for low probabilities:
            return chez.max_(pixel_w, mul(prob, max_bar_width))

        return tell(codelet_type, "set-graphics-parameters",
                    self, slot_pexp, get_urgency_pexp, get_highlight_pexp,
                    codelet_count_coord, this.bar_left, bar_bottom, bar_top, bar_width)

    @message("garbage-collect")
    def garbage_collect(this, self):
        g = this.graphics_window
        tell(g, "retag", "bar", "garbage")
        tell(g, "retag", "urgency", "garbage")
        tell(g, "retag", "count", "garbage")
        tell(g, "retag", "sum", "garbage")
        tell(g, "retag", "highlight", "garbage")
        sugar.for_star(lambda codelet_type: tell(codelet_type, "draw-graphics"),
                       _codelet_types())
        if setup.p_codelet_count_graphics is not False:
            tell(self, "draw-codelet-sum", tell(_coderack(), "get-num-of-codelets"))
        if setup.p_highlight_last_codelet is not False:
            tell(self, "highlight-last-codelet")
        return tell(g, "delete", "garbage")

    @message("clear")
    def clear(this, self):
        g = this.graphics_window
        tell(self, "draw-title", String("Coderack"))

        def reset(codelet_type):
            tell(codelet_type, "reset-values-to-zero")
            tell(codelet_type, "reset-bar-width")
            return tell(codelet_type, "clear-pattern-value")
        sugar.for_star(reset, _codelet_types())
        tell(g, "retag", "urgency", "garbage")
        tell(g, "retag", "bar", "garbage")
        tell(g, "retag", "count", "garbage")
        tell(g, "retag", "sum", "garbage")
        tell(g, "retag", "total", "garbage")
        tell(g, "retag", "highlight", "garbage")
        tell(g, "delete", "garbage")
        this.last_codelet_type = False
        if setup.p_coderack_graphics is not False and \
                setup.p_codelet_count_graphics is not False:
            sugar.for_star(lambda codelet_type: tell(codelet_type, "draw-codelet-count"),
                           _codelet_types())
            tell(self, "draw-codelet-sum", 0)
            tell(g, "draw", this._total_pexp(), "total")
        this.display_state = "normal"
        return "done"

    def _total_pexp(this):
        return ["let-sgl", [["font", p_coderack_codelet_sum_font]],
                ["text", this.sum_label_coord, String("Total")]]

    @message("update-graphics")
    def update_graphics(this, self):
        g = this.graphics_window
        tell(_coderack(), "update-all-selection-probabilities")
        tell(g, "retag", "bar", "garbage")
        tell(g, "retag", "count", "garbage")
        tell(g, "retag", "sum", "garbage")
        tell(g, "retag", "highlight", "garbage")
        sugar.for_star(lambda codelet_type: tell(codelet_type, "update-bar-graphics"),
                       _codelet_types())
        if setup.p_codelet_count_graphics is not False:
            tell(self, "draw-codelet-sum", tell(_coderack(), "get-num-of-codelets"))
        if setup.p_highlight_last_codelet is not False:
            tell(self, "highlight-last-codelet")
        tell(g, "delete", "garbage")
        return "done"

    @message("draw-codelet-count")
    def draw_codelet_count(this, self, coord, num, color):
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["font", view_globals.p_coderack_codelet_count_font],
                                 ["background-color", color],
                                 ["text-justification", "right"],
                                 ["text-mode", "image"]],
                     ["text", coord,
                      String(chez.format_("     ~a", num if exists_p(num) else String("")))]],
                    "count")

    @message("draw-codelet-sum")
    def draw_codelet_sum(this, self, num):
        from metacat.gui import constants as K
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["background-color", K.p_coderack_background_color],
                                 ["font", p_coderack_codelet_sum_font],
                                 ["text-justification", "right"],
                                 ["text-mode", "image"]],
                     ["text", this.sum_coord,
                      String(chez.format_("     ~a", num if exists_p(num) else String("")))]],
                    "sum")

    @message("draw-title")
    def draw_title(this, self, text):
        tell(self, "erase-title")
        if exists_p(text):
            this.current_title = text
            this.current_title_pexp = ["let-sgl", [["font", p_coderack_title_font],
                                                   ["text-justification", "center"]],
                                       ["text", this.title_coord, text]]
            tell(this.graphics_window, "draw", this.current_title_pexp, "title")
        return "done"

    @message("erase-title")
    def erase_title(this, self):
        if exists_p(this.current_title_pexp):
            tell(this.graphics_window, "delete", "title")
            this.current_title = False
            this.current_title_pexp = False
        return "done"

    @message("draw-graphics")
    def draw_graphics(this, self):
        g = this.graphics_window
        tell(self, "draw-title", this.current_title)
        tell(g, "draw", this.skeleton_graphics_pexp, "skeleton")
        if this.display_state == "normal":
            if setup.p_coderack_graphics is not False:
                sugar.for_star(lambda codelet_type: tell(codelet_type, "draw-graphics"),
                               _codelet_types())
                if setup.p_codelet_count_graphics is not False:
                    tell(self, "draw-codelet-sum", tell(_coderack(), "get-num-of-codelets"))
                    tell(g, "draw", this._total_pexp(), "total")
                if setup.p_highlight_last_codelet is not False:
                    tell(self, "highlight-last-codelet")
        elif this.display_state == "pattern":
            sugar.for_star(lambda codelet_type: tell(codelet_type, "draw-pattern-value"),
                           _codelet_types())
        return "done"

    @message("initialize")
    def initialize(this, self):
        tell(self, "initialize-parameters")
        tell(this.graphics_window, "clear")
        return tell(self, "draw-graphics")

    @message("resize")
    def resize(this, self, new_width, new_height):
        g = this.graphics_window
        this.x_pixels = new_width
        this.y_pixels = new_height
        tell(self, "initialize-parameters")
        tell(g, "retag", "all", "garbage")
        tell(self, "draw-graphics")
        return tell(g, "delete", "garbage")

    # title = #f just erases the current title (if any):
    @message("blank-window")
    def blank_window(this, self, title):
        g = this.graphics_window
        tell(self, "draw-title", title)
        sugar.for_star(lambda codelet_type: tell(codelet_type, "clear-pattern-value"),
                       _codelet_types())
        tell(g, "retag", "urgency", "garbage")
        tell(g, "retag", "count", "garbage")
        tell(g, "retag", "sum", "garbage")
        tell(g, "retag", "total", "garbage")
        tell(g, "retag", "bar", "garbage")
        tell(g, "retag", "highlight", "garbage")
        tell(g, "delete", "garbage")
        this.display_state = "blank"
        return "done"

    @message("display-patterns")
    def display_patterns(this, self, codelet_patterns, title):
        tell(self, "blank-window", title)

        def each_pattern(pattern):
            return sugar.for_star(lambda entry: tell(first(entry), "display-urgency",
                                                     second(entry)),
                                  rest(pattern))
        sugar.for_star(each_pattern, codelet_patterns)
        this.display_state = "pattern"
        return "done"

    @message("restore-current-state")
    def restore_current_state(this, self):
        g = this.graphics_window
        tell(_coderack(), "update-all-selection-probabilities")
        tell(self, "blank-window", String("Coderack"))
        if setup.p_coderack_graphics is not False:
            sugar.for_star(lambda codelet_type: tell(codelet_type, "draw-graphics"),
                           _codelet_types())
            if setup.p_codelet_count_graphics is not False:
                tell(self, "draw-codelet-sum", tell(_coderack(), "get-num-of-codelets"))
                tell(g, "draw", this._total_pexp(), "total")
            if setup.p_highlight_last_codelet is not False:
                tell(self, "highlight-last-codelet")
        this.display_state = "normal"
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


def load():
    """port: nothing to make at load time (the fonts are made with the window);
    here for views.load_views's uniform order."""
    return None
