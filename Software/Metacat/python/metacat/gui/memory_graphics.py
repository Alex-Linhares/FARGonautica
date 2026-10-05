"""memory-graphics.ss: the Episodic Memory window and its answer icons.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from memory-graphics.ss, with
racket/gui/memory-graphics.rktl as a worked translation.

Verbatim, in the original's order.  new-memory-window's closure is
`MemoryWindow`, delegating to its vertically scrollable graphics window
(gui/general_graphics.py).  %memory-answer-icon-font% is a module global, #f
until select-memory-font makes it (when the window is made), as in the
original.  The rounded boxes are the engine's pexp builders
(metacat/general_graphics.py), read at call time.  The model's globals
(*running?*, *trace*, *memory*, *comment-window*, *control-panel*),
compare-answers and restore-current-state (view_globals, installed by the
Workspace window's module) are read qualified, at call time.  Arithmetic is
Chez's (chez.py).  This module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import answers, chez, memory, run, setup, trace, view_globals
from metacat.chez import add, mul, sub
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (exists_p, first, fourth, hundred_minus, percent, round_,
                               second, snag_description_p, third)

String = chez.String
F = Fraction

p_memory_answer_icon_font = False


def select_memory_font(win_width, win_height):
    """memory-graphics.ss: select-memory-font"""
    global p_memory_answer_icon_font
    from metacat.gui import fonts
    desired_font_height = round_(mul(F(1, 20), chez.min_(win_width, win_height)))
    p_memory_answer_icon_font = fonts.make_mfont(
        fonts.sans_serif, sub(desired_font_height), ["bold", "italic"])


def resize_memory_font(win_width, win_height):
    """memory-graphics.ss: resize-memory-font"""
    return tell(p_memory_answer_icon_font, "resize",
                round_(mul(F(1, 20), chez.min_(win_width, win_height))))


def memory_background_color():
    """memory-graphics.ss: memory-background-color"""
    from metacat.gui import colors, constants as K
    return colors.swl_color(String(
        "grey" + chez.number_to_string(K.p_memory_background_grey_level)))


def memory_icon_activation_color(activation):
    """memory-graphics.ss: memory-icon-activation-color"""
    from metacat.gui import colors, constants as K
    grey_level = add(K.p_memory_background_grey_level,
                     round_(mul(percent(activation),
                                hundred_minus(K.p_memory_background_grey_level))))
    return colors.swl_color(String("grey" + chez.number_to_string(grey_level)))


def memory_window_press_handler(win, x, y):
    """memory-graphics.ss: memory-window-press-handler"""
    from metacat.gui import theme_graphics
    if theme_graphics.g_theme_edit_mode_p is not False:
        return tell(setup.g_control_panel, "raise-theme-edit-dialog")
    if run.g_running_p is False:
        selected_answer = tell(memory.g_memory, "get-mouse-selected-answer", x, y)
        if exists_p(selected_answer):
            tell(trace.g_trace, "unhighlight-all-events")
            tell(selected_answer, "toggle-highlight")
            if tell(selected_answer, "highlighted?") is not False:
                previous_answer = tell(memory.g_memory, "get-other-highlighted-answer",
                                       selected_answer)
                if not snag_description_p(selected_answer) and exists_p(previous_answer):
                    tell(setup.g_comment_window, "add-comment",
                         [String("Let's see...")],
                         [String("Comparing answers...")])
                tell(memory.g_memory, "unhighlight-all-answers-except", selected_answer)
                tell(selected_answer, "display")
                if not snag_description_p(selected_answer) and exists_p(previous_answer):
                    return answers.compare_answers(selected_answer, previous_answer)
                return None                     # a one-armed if* (void)
            return view_globals.restore_current_state()
    return None


def make_memory_window(*optional_args):
    """memory-graphics.ss: make-memory-window"""
    from metacat.gui import constants as K
    width = K.p_default_memory_width if len(optional_args) == 0 else first(optional_args)
    height = K.p_default_memory_height if len(optional_args) < 2 else second(optional_args)
    window = new_memory_window(width, height)
    tell(window, "initialize")
    return window


def new_memory_window(x_pixels, y_pixels):
    """memory-graphics.ss: new-memory-window"""
    from metacat.gui import constants as K, general_graphics as gg
    graphics_window = gg.make_vertical_scrollable_graphics_window(
        x_pixels, y_pixels, K.p_virtual_memory_length, memory_background_color())
    tell(graphics_window, "set-icon-label", K.p_memory_window_icon_label)
    if exists_p(K.p_memory_window_icon_image):
        tell(graphics_window, "set-icon-image", K.p_memory_window_icon_image)
    if exists_p(K.p_memory_window_title):
        tell(graphics_window, "set-window-title", K.p_memory_window_title)
    tell(graphics_window, "set-mouse-handlers", memory_window_press_handler, False)
    select_memory_font(x_pixels, y_pixels)
    return MemoryWindow(graphics_window)


class MemoryWindow(SchemeObject):
    """memory-graphics.ss: new-memory-window's closure"""
    __slots__ = ("graphics_window", "top_y", "memory_icon_spacing", "icon_erase_border",
                 "next_y", "center_x")

    def __init__(this, graphics_window):
        this.graphics_window = graphics_window
        # the let*, in order (1.2: initialize recomputes the spacing and next-y;
        # anomalies: "The Memory window's first icon spacing is dead code")
        this.top_y = tell(graphics_window, "get-y-max")
        this.memory_icon_spacing = mul(F(5, 4), tell(graphics_window, "get-string-height",
                                                     p_memory_answer_icon_font))
        this.icon_erase_border = mul(F(1, 2), this.memory_icon_spacing)
        this.next_y = sub(this.top_y, this.memory_icon_spacing)
        this.center_x = F(1, 2)

    @message("object-type")
    def object_type(this, self):
        return "memory-window"

    def _place_icon(this, answer):
        """the let* shared by add-memory-icon and resize: the icon's pexps and
        bounding box"""
        memory_icon_pexp_info = get_memory_icon_pexp_info(
            this.graphics_window, answer, this.center_x, this.next_y)
        get_normal_icon_pexp = first(memory_icon_pexp_info)
        highlight_icon_pexp = second(memory_icon_pexp_info)
        memory_icon_width = third(memory_icon_pexp_info)
        memory_icon_height = fourth(memory_icon_pexp_info)
        x1 = sub(this.center_x, mul(F(1, 2), memory_icon_width))
        x2 = add(this.center_x, mul(F(1, 2), memory_icon_width))
        y1 = sub(this.next_y, memory_icon_height)
        y2 = this.next_y
        tell(answer, "set-graphics-info", get_normal_icon_pexp, highlight_icon_pexp)
        tell(answer, "set-bounding-box", x1, y1, x2, y2)
        return get_normal_icon_pexp, highlight_icon_pexp, memory_icon_height

    @message("add-memory-icon")
    def add_memory_icon(this, self, answer):
        get_normal_icon_pexp, highlight_icon_pexp, memory_icon_height = this._place_icon(answer)
        tell(this.graphics_window, "draw",
             get_normal_icon_pexp(tell(answer, "get-activation")))
        this.next_y = sub(this.next_y, memory_icon_height, this.memory_icon_spacing)
        return "done"

    @message("erase-memory-icon")
    def erase_memory_icon(this, self, answer):
        return tell(this.graphics_window, "erase",
                    ["filled-rectangle",
                     [sub(tell(answer, "get-bounding-box-x1"), this.icon_erase_border),
                      sub(tell(answer, "get-bounding-box-y1"), this.icon_erase_border)],
                     [add(tell(answer, "get-bounding-box-x2"), this.icon_erase_border),
                      add(tell(answer, "get-bounding-box-y2"), this.icon_erase_border)]])

    @message("initialize")
    def initialize(this, self):
        tell(this.graphics_window, "clear")
        resize_memory_font(tell(this.graphics_window, "get-visible-w"),
                           tell(this.graphics_window, "get-visible-h"))
        this.memory_icon_spacing = mul(F(5, 4), tell(this.graphics_window, "get-string-height",
                                                     p_memory_answer_icon_font))
        this.next_y = sub(this.top_y, this.memory_icon_spacing)
        return "done"

    @message("resize")
    def resize(this, self, new_width, new_height):
        resize_memory_font(new_width, new_height)
        this.memory_icon_spacing = mul(F(5, 4), tell(this.graphics_window, "get-string-height",
                                                     p_memory_answer_icon_font))
        this.icon_erase_border = mul(F(1, 2), this.memory_icon_spacing)
        this.next_y = sub(this.top_y, this.memory_icon_spacing)
        tell(this.graphics_window, "retag", "all", "garbage")
        for descrip in list(reversed(tell(memory.g_memory, "get-all-descriptions"))):
            get_normal_icon_pexp, highlight_icon_pexp, memory_icon_height = \
                this._place_icon(descrip)
            tell(this.graphics_window, "draw",
                 highlight_icon_pexp if tell(descrip, "highlighted?") is not False
                 else get_normal_icon_pexp(tell(descrip, "get-activation")))
            this.next_y = sub(this.next_y, memory_icon_height, this.memory_icon_spacing)
        return tell(this.graphics_window, "delete", "garbage")

    @message("redraw")
    def redraw(this, self):
        tell(this.graphics_window, "caching-on")
        tell(this.graphics_window, "clear")
        for descrip in tell(memory.g_memory, "get-all-descriptions"):
            tell(this.graphics_window, "draw",
                 tell(descrip, "get-highlight-icon-pexp")
                 if tell(descrip, "highlighted?") is not False
                 else tell(descrip, "get-normal-icon-pexp"))
        tell(this.graphics_window, "flush")
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


# get-memory-icon-pexp-info returns a list of the form:
# (<get-normal-icon-pexp> <highlight-icon-pexp> <icon-width> <icon-height>)

def get_memory_icon_pexp_info(g, answer, center_x, next_y):
    """memory-graphics.ss: get-memory-icon-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    pixel = tell(g, "get-width-per-pixel")
    rounded_box_sizing_factor = F(3, 2)
    font = p_memory_answer_icon_font
    text_string = tell(answer, "print-name")
    text_width = tell(g, "get-string-width", text_string, font)
    text_height = tell(g, "get-string-height", font)
    icon_height = mul(rounded_box_sizing_factor, text_height)
    radius = mul(F(1, 2), icon_height)
    icon_width = add(text_width, mul(2, radius))
    xc = center_x
    yc = sub(next_y, mul(F(1, 2), icon_height))
    outline = GG.centered_rounded_box(xc, yc, icon_width, icon_height, radius)
    background = GG.filled_centered_rounded_box(xc, yc, icon_width, icon_height, radius,
                                                pixel)
    text_pexp = ["let-sgl", [["font", font], ["text-justification", "center"],
                             ["origin", [xc, yc]]],
                 ["text", ["text-relative", [0, F(-2, 5)]], text_string]]
    normal_icon_pexp = [
        "let-sgl", [["foreground-color", memory_background_color()], ["line-width", 2]],
        outline,
        ["let-sgl", [["foreground-color", K.c_black], ["line-width", 1]],
         outline,
         text_pexp]]

    def get_normal_icon_pexp(activation):
        color = memory_icon_activation_color(activation)
        return ["let-sgl", [["foreground-color", color]],
                background,
                normal_icon_pexp]
    highlight_icon_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_black]],
         background],
        ["let-sgl", [["foreground-color", K.c_yellow], ["line-width", 2]],
         outline,
         ["let-sgl", [["font", font], ["text-justification", "center"],
                      ["origin", [xc, yc]]],
          ["text", ["text-relative", [0, F(-2, 5)]], text_string]]]]
    return [get_normal_icon_pexp, highlight_icon_pexp, icon_width, icon_height]
