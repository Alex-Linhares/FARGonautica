"""trace-graphics.ss: the Temporal Trace window and its event icons.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from trace-graphics.ss, with
racket/gui/trace-graphics.rktl as a worked translation.

The definitions are the original's, in its order, but for
group-event-pexp-text-string, which is the engine's (metacat/trace_graphics.py:
trace.ss names every group event with it) and is used from there.
new-trace-window's closure is `TraceWindow`, delegating to its horizontally
scrollable graphics window (gui/general_graphics.py).  The seven event-icon fonts
are module globals, #f until select-trace-fonts makes them (when the window is
made), as in the original.  The shapes (ovaloids, rounded boxes, octagons,
arrowheads) are the engine's pexp builders (metacat/general_graphics.py), and
%group-arrowhead-angle% is group-graphics.ss's (metacat/group_graphics.py), all
read at call time.  The model's globals (*running?*, *trace*, *memory*,
*control-panel*) and restore-current-state (view_globals, installed by the
Workspace window's module) are read qualified, at call time.  Arithmetic is
Chez's (chez.py).  This module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import chez, memory, run, setup, slipnet, trace, view_globals
from metacat import trace_graphics as engine_trace_graphics
from metacat.chez import add, mul, sub
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import exists_p, first, fourth, round_, second, third

String = chez.String
F = Fraction

p_answer_event_icon_font = False
p_clamp_event_icon_font = False
p_concept_activation_event_icon_font = False
p_concept_mapping_event_icon_font = False
p_group_event_icon_font = False
p_rule_event_icon_font = False
p_snag_event_icon_font = False


def select_trace_fonts(win_width, win_height):
    """trace-graphics.ss: select-trace-fonts"""
    global p_answer_event_icon_font, p_clamp_event_icon_font
    global p_concept_activation_event_icon_font, p_concept_mapping_event_icon_font
    global p_group_event_icon_font, p_rule_event_icon_font, p_snag_event_icon_font
    from metacat.gui import fonts
    desired_answer_height = round_(mul(F(17, 60), win_height))
    desired_clamp_height = round_(mul(F(17, 60), win_height))
    desired_concept_height = round_(mul(F(15, 60), win_height))
    desired_cmap_height = round_(mul(F(15, 60), win_height))
    desired_group_height = round_(mul(F(17, 60), win_height))
    desired_rule_height = round_(mul(F(17, 60), win_height))
    desired_snag_height = round_(mul(F(11, 60), win_height))
    p_answer_event_icon_font = fonts.make_mfont(
        fonts.sans_serif, sub(desired_answer_height), ["bold", "italic"])
    p_clamp_event_icon_font = fonts.make_mfont(
        fonts.fancy, sub(desired_clamp_height), ["bold", "italic"])
    p_concept_activation_event_icon_font = fonts.make_mfont(
        fonts.fancy, sub(desired_concept_height), ["bold", "italic"])
    p_concept_mapping_event_icon_font = fonts.make_mfont(
        fonts.fancy, sub(desired_cmap_height), ["bold", "italic"])
    p_group_event_icon_font = fonts.make_mfont(
        fonts.serif, sub(desired_group_height), ["bold", "italic"])
    p_rule_event_icon_font = fonts.make_mfont(
        fonts.serif, sub(desired_rule_height), ["bold", "italic"])
    p_snag_event_icon_font = fonts.make_mfont(
        fonts.sans_serif, sub(desired_snag_height), ["bold"])


def resize_trace_fonts(win_width, win_height):
    """trace-graphics.ss: resize-trace-fonts"""
    tell(p_answer_event_icon_font, "resize", round_(mul(F(17, 60), win_height)))
    tell(p_clamp_event_icon_font, "resize", round_(mul(F(17, 60), win_height)))
    tell(p_concept_activation_event_icon_font, "resize", round_(mul(F(15, 60), win_height)))
    tell(p_concept_mapping_event_icon_font, "resize", round_(mul(F(15, 60), win_height)))
    tell(p_group_event_icon_font, "resize", round_(mul(F(17, 60), win_height)))
    tell(p_rule_event_icon_font, "resize", round_(mul(F(17, 60), win_height)))
    return tell(p_snag_event_icon_font, "resize", round_(mul(F(11, 60), win_height)))


p_group_event_icon_arrowhead_length = F(2, 25)
p_minimum_event_width = 1
p_minimum_event_height = F(13, 20)


def trace_window_press_handler(win, x, y):
    """trace-graphics.ss: trace-window-press-handler"""
    from metacat.gui import theme_graphics
    if theme_graphics.g_theme_edit_mode_p is not False:
        return tell(setup.g_control_panel, "raise-theme-edit-dialog")
    if run.g_running_p is False:
        selected_event = tell(trace.g_trace, "get-mouse-selected-event", x, y)
        if exists_p(selected_event):
            tell(memory.g_memory, "unhighlight-all-answers")
            tell(selected_event, "toggle-highlight")
            if tell(selected_event, "highlighted?") is not False:
                tell(trace.g_trace, "unhighlight-all-events-except", selected_event)
                return tell(selected_event, "display")
            return view_globals.restore_current_state()
    return None


def make_trace_window(*optional_args):
    """trace-graphics.ss: make-trace-window"""
    from metacat.gui import constants as K
    width = K.p_default_trace_width if len(optional_args) == 0 else first(optional_args)
    height = K.p_default_trace_height if len(optional_args) < 2 else second(optional_args)
    window = new_trace_window(width, height)
    tell(window, "initialize")
    return window


def new_trace_window(x_pixels, y_pixels):
    """trace-graphics.ss: new-trace-window"""
    from metacat.gui import constants as K, general_graphics as gg
    graphics_window = gg.make_horizontal_scrollable_graphics_window(
        x_pixels, y_pixels, K.p_virtual_trace_length, K.p_trace_background_color)
    tell(graphics_window, "set-icon-label", K.p_trace_icon_label)
    if exists_p(K.p_trace_icon_image):
        tell(graphics_window, "set-icon-image", K.p_trace_icon_image)
    if exists_p(K.p_trace_window_title):
        tell(graphics_window, "set-window-title", K.p_trace_window_title)
    tell(graphics_window, "set-mouse-handlers", trace_window_press_handler, False)
    select_trace_fonts(x_pixels, y_pixels)
    return TraceWindow(graphics_window)


class TraceWindow(SchemeObject):
    """trace-graphics.ss: new-trace-window's closure"""
    __slots__ = ("graphics_window", "event_spacing", "next_x", "center_y")

    def __init__(this, graphics_window):
        this.graphics_window = graphics_window
        this.event_spacing = F(1, 5)
        this.next_x = this.event_spacing
        this.center_y = F(1, 2)

    @message("object-type")
    def object_type(this, self):
        return "trace-window"

    @message("add-event")
    def add_event(this, self, event):
        t = tell(event, "get-type")
        if t == "answer":
            get_event_pexp_info = answer_event_pexp_info
        elif t == "clamp":
            get_event_pexp_info = clamp_event_pexp_info
        elif t == "concept-activation":
            get_event_pexp_info = concept_activation_event_pexp_info
        elif t == "group":
            get_event_pexp_info = group_event_pexp_info
        elif t == "rule":
            get_event_pexp_info = rule_event_pexp_info
        elif t == "concept-mapping":
            get_event_pexp_info = concept_mapping_event_pexp_info
        elif t == "snag":
            get_event_pexp_info = snag_event_pexp_info
        else:
            get_event_pexp_info = None      # a case without else (void)
        event_pexp_info = get_event_pexp_info(this.graphics_window, event, this.next_x,
                                              this.center_y)
        normal_pexp = first(event_pexp_info)
        highlight_pexp = second(event_pexp_info)
        event_width = third(event_pexp_info)
        event_height = fourth(event_pexp_info)
        x1 = this.next_x
        x2 = add(this.next_x, event_width)
        y1 = sub(this.center_y, mul(F(1, 2), event_height))
        y2 = add(this.center_y, mul(F(1, 2), event_height))
        tell(event, "set-graphics-pexps", normal_pexp, highlight_pexp)
        tell(event, "set-bounding-box", x1, y1, x2, y2)
        tell(this.graphics_window, "draw", normal_pexp)
        this.next_x = add(this.next_x, event_width, this.event_spacing)
        return "done"

    @message("initialize")
    def initialize(this, self):
        tell(this.graphics_window, "clear")
        this.next_x = this.event_spacing
        resize_trace_fonts(tell(this.graphics_window, "get-visible-w"),
                           tell(this.graphics_window, "get-visible-h"))
        return "done"

    @message("resize")
    def resize(this, self, new_width, new_height):
        resize_trace_fonts(new_width, new_height)
        tell(this.graphics_window, "retag", "all", "garbage")
        for event in tell(trace.g_trace, "get-all-events"):
            tell(this.graphics_window, "draw",
                 tell(event, "get-highlight-graphics-pexp")
                 if tell(event, "highlighted?") is not False
                 else tell(event, "get-normal-graphics-pexp"))
        return tell(this.graphics_window, "delete", "garbage")

    @message("redraw")
    def redraw(this, self):
        tell(this.graphics_window, "caching-on")
        tell(this.graphics_window, "clear")
        for event in tell(trace.g_trace, "get-all-events"):
            tell(this.graphics_window, "draw",
                 tell(event, "get-highlight-graphics-pexp")
                 if tell(event, "highlighted?") is not False
                 else tell(event, "get-normal-graphics-pexp"))
        tell(this.graphics_window, "flush")
        return "done"

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


# *-event-pexp-info procedures return a list of the form:
# (<normal-pexp> <highlight-pexp> <event-width> <event-height>)

def _icon_text(font, xc, yc, text):
    """(let-sgl ((font ,font) (text-justification center) (origin (,xc ,yc)))
         (text (text-relative (0 -7/20)) ,text))"""
    return ["let-sgl", [["font", font], ["text-justification", "center"],
                        ["origin", [xc, yc]]],
            ["text", ["text-relative", [0, F(-7, 20)]], text]]


def answer_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: answer-event-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    ovalness = F(3, 4)
    ovaloid_sizing_factor = F(3, 2)
    font = p_answer_event_icon_font
    answer_text_string = String(chez.format_(
        "Answer ~a", tell(tell(event, "get-answer-string"), "print-name")))
    text_width = tell(g, "get-string-width", answer_text_string, font)
    text_height = tell(g, "get-string-height", font)
    width = chez.max_(p_minimum_event_width, mul(ovaloid_sizing_factor, text_width))
    height = chez.max_(p_minimum_event_height, mul(ovaloid_sizing_factor, text_height))
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    normal_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_white]],
         GG.filled_centered_ovaloid(xc, yc, width, height, ovalness)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.centered_ovaloid(xc, yc, width, height, ovalness),
         _icon_text(font, xc, yc, answer_text_string)]]
    highlight_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.p_answer_event_icon_highlight_color]],
         GG.filled_centered_ovaloid(xc, yc, width, height, ovalness)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.centered_ovaloid(xc, yc, width, height, ovalness),
         _icon_text(font, xc, yc, answer_text_string)]]
    return [normal_pexp, highlight_pexp, width, height]


def clamp_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: clamp-event-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    corner_radius = F(1, 5)
    rounded_box_sizing_factor = F(3, 2)
    text = String("Clamp")
    font = p_clamp_event_icon_font
    pixel = tell(g, "get-height-per-pixel")
    text_width = tell(g, "get-string-width", text, font)
    text_height = tell(g, "get-string-height", font)
    width = chez.max_(p_minimum_event_width, mul(rounded_box_sizing_factor, text_width))
    height = chez.max_(p_minimum_event_height, mul(rounded_box_sizing_factor, text_height))
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    normal_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_white]],
         GG.filled_centered_rounded_box(xc, yc, width, height, corner_radius, pixel)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.centered_rounded_box(xc, yc, width, height, corner_radius),
         _icon_text(font, xc, yc, text)]]
    highlight_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.p_clamp_event_icon_highlight_color]],
         GG.filled_centered_rounded_box(xc, yc, width, height, corner_radius, pixel)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.centered_rounded_box(xc, yc, width, height, corner_radius),
         _icon_text(font, xc, yc, text)]]
    return [normal_pexp, highlight_pexp, width, height]


def concept_activation_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: concept-activation-event-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    ovalness = 1
    text = tell(tell(event, "get-slipnode"), "get-short-name")
    font = p_concept_activation_event_icon_font
    text_width = tell(g, "get-string-width", text, font)
    text_height = tell(g, "get-string-height", font)
    width = add(text_width, mul(F(3, 2), text_height))
    height = mul(2, text_height)
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    normal_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_white]],
         GG.filled_centered_ovaloid(xc, yc, width, height, ovalness)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.centered_ovaloid(xc, yc, width, height, ovalness),
         _icon_text(font, xc, yc, text)]]
    highlight_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.p_concept_activation_event_icon_highlight_color]],
         GG.filled_centered_ovaloid(xc, yc, width, height, ovalness)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.centered_ovaloid(xc, yc, width, height, ovalness),
         _icon_text(font, xc, yc, text)]]
    return [normal_pexp, highlight_pexp, width, height]


def group_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: group-event-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    group = tell(event, "get-group")
    direction = tell(group, "get-direction")
    text = engine_trace_graphics.group_event_pexp_text_string(group)
    font = p_group_event_icon_font
    text_width = tell(g, "get-string-width", text, font)
    text_height = tell(g, "get-string-height", font)
    width = add(text_width, text_height)
    height = mul(F(3, 2), text_height)
    x1 = left_x
    x2 = add(left_x, width)
    y1 = sub(center_y, mul(F(1, 2), height))
    y2 = add(center_y, mul(F(1, 2), height))
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    normal_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_white]],
         GG.solid_box(x1, y1, x2, y2)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.outline_box(x1, y1, x2, y2),
         *group_event_pexp_arrowhead(direction, xc, y2),
         _icon_text(font, xc, yc, text)]]
    highlight_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.p_group_event_icon_highlight_color]],
         GG.solid_box(x1, y1, x2, y2)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.outline_box(x1, y1, x2, y2),
         *group_event_pexp_arrowhead(direction, xc, y2),
         _icon_text(font, xc, yc, text)]]
    return [normal_pexp, highlight_pexp, width, height]


# port: group-event-pexp-text-string is in the engine (metacat/trace_graphics.py),
# since trace.ss names every group event with it.


def group_event_pexp_arrowhead(direction, xc, y2):
    """trace-graphics.ss: group-event-pexp-arrowhead"""
    from metacat import general_graphics as GG, group_graphics
    if exists_p(direction):
        orientation_angle = 0 if direction is slipnet.plato_right else 180
        return [GG.arrowhead(xc, y2, orientation_angle,
                             p_group_event_icon_arrowhead_length,
                             group_graphics.p_group_arrowhead_angle)]
    return []


def _rule_type_case(event, top, bottom):
    """(case (tell event 'get-rule-type) (top ...) (bottom ...)), no else"""
    rule_type = tell(event, "get-rule-type")
    if rule_type == "top":
        return top
    if rule_type == "bottom":
        return bottom
    return None


def rule_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: rule-event-pexp-info"""
    from metacat.gui import constants as K
    text = _rule_type_case(event, String("Top Rule"), String("Bottom Rule"))
    font = p_rule_event_icon_font
    text_width = tell(g, "get-string-width", text, font)
    text_height = tell(g, "get-string-height", font)
    width = add(text_width, mul(F(3, 2), text_height))
    height = mul(F(3, 2), text_height)
    border = mul(F(1, 10), height)
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    x1 = left_x
    y1 = sub(center_y, mul(F(1, 2), height))
    x2 = add(left_x, width)
    y2 = add(center_y, mul(F(1, 2), height))
    lower_left = [x1, y1]
    upper_left = [x1, y2]
    upper_right = [x2, y2]
    border_lower_left = [add(x1, border), add(y1, border)]
    border_upper_right = [sub(x2, border), sub(y2, border)]
    normal_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.p_trace_background_color],
                     ["line-width", 2]],
         ["polyline", lower_left, upper_left, upper_right]],
        ["let-sgl", [["foreground-color", K.c_white]],
         ["filled-rectangle", lower_left, upper_right]],
        ["let-sgl", [["foreground-color", K.c_black]],
         ["rectangle", lower_left, upper_right],
         ["rectangle", border_lower_left, border_upper_right],
         _icon_text(p_rule_event_icon_font, xc, yc, text)]]
    highlight_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_white]],
         ["filled-rectangle", lower_left, upper_right]],
        ["let-sgl", [["foreground-color",
                      _rule_type_case(event, K.p_top_rule_event_icon_highlight_color,
                                      K.p_bottom_rule_event_icon_highlight_color)]],
         ["rectangle", lower_left, upper_right],
         ["rectangle", border_lower_left, border_upper_right],
         ["let-sgl", [["line-width", 2]],
          ["polyline", lower_left, upper_left, upper_right]],
         _icon_text(p_rule_event_icon_font, xc, yc, text)]]
    return [normal_pexp, highlight_pexp, width, height]


def concept_mapping_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: concept-mapping-event-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    text = tell(tell(event, "get-concept-mapping"), "print-name")
    font = p_concept_mapping_event_icon_font
    text_width = tell(g, "get-string-width", text, font)
    text_height = tell(g, "get-string-height", font)
    width = add(text_width, text_height)
    height = mul(F(3, 2), text_height)
    x1 = left_x
    x2 = add(left_x, width)
    y1 = sub(center_y, mul(F(1, 2), height))
    y2 = add(center_y, mul(F(1, 2), height))
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    normal_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.c_white]],
         GG.solid_box(x1, y1, x2, y2)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.outline_box(x1, y1, x2, y2),
         _icon_text(font, xc, yc, text)]]
    highlight_pexp = [
        "let-sgl", [],
        ["let-sgl", [["foreground-color", K.p_concept_mapping_event_icon_highlight_color]],
         GG.solid_box(x1, y1, x2, y2)],
        ["let-sgl", [["foreground-color", K.c_black]],
         GG.outline_box(x1, y1, x2, y2),
         _icon_text(font, xc, yc, text)]]
    return [normal_pexp, highlight_pexp, width, height]


def snag_event_pexp_info(g, event, left_x, center_y):
    """trace-graphics.ss: snag-event-pexp-info"""
    from metacat import general_graphics as GG
    from metacat.gui import constants as K
    text = String("SNAG")
    font = p_snag_event_icon_font
    width = F(7, 10)
    height = F(7, 10)
    border = mul(F(13, 100), width)
    xc = add(left_x, mul(F(1, 2), width))
    yc = center_y
    normal_pexp = [
        "let-sgl", [["background-color", K.c_white]],
        GG.centered_octagon(xc, yc, width),
        GG.centered_octagon(xc, yc, sub(width, border)),
        _icon_text(font, xc, yc, text)]
    highlight_pexp = [
        "let-sgl", [["foreground-color", K.c_white],
                    ["background-color", K.p_snag_event_icon_highlight_color]],
        GG.centered_octagon(xc, yc, width),
        ["let-sgl", [["line-width", 2]],
         GG.centered_octagon(xc, yc, sub(width, border))],
        _icon_text(font, xc, yc, text)]
    return [normal_pexp, highlight_pexp, width, height]
