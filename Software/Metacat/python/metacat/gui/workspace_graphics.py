"""workspace-graphics.ss: the Workspace window.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from workspace-graphics.ss, with
racket/gui/workspace-graphics.rktl as a worked translation.

Verbatim but for the changes marked "port:" (docs/porting-notes.md, item 13).
The eleven fonts select-workspace-fonts creates by set! (never defined in the
original) and restore-current-state are engine globals
(metacat/view_globals.py): select-workspace-fonts and `load()` install them
there with engine.set_global, and the window reads the fonts from there.  The
colours the model reads (%vertical-bridge-color%, ...) are read from there too;
the others come from gui/constants.py.  The pexp builders (centered-double-arrow)
and rule-graphics.ss's procedures are the engine's, looked up at call time.
This module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import (chez, engine, memory, run, setup, slipnet, themes, trace,
                     utilities, view_globals as V, workspace)
from metacat.gui import constants as K
from metacat.gui import fonts
from metacat.gui import general_graphics as GG
from metacat.objects import SchemeObject, delegate, message, tell, tell_all
from metacat.utilities import exists_p

F = Fraction
_HALF = F(1, 2)

p_workspace_arrow_length = F(7, 200)


def _set(name, value):
    # port: set! of a global the model reads goes to the engine
    engine.set_global(name, value)


def select_workspace_fonts(win_width, win_height):
    """workspace-graphics.ss: select-workspace-fonts"""
    def desired(n):
        return utilities.round_(chez.mul(F(n, 600), win_height))
    desired_title_height = desired(29)
    desired_count_height = desired(15)
    desired_letter_height = desired(28)
    desired_group_lcat_height = desired(21)
    desired_relevant_group_height = desired(17)
    desired_irrelevant_group_height = desired(13)
    desired_bridge_label_height = desired(13)
    desired_relevant_cmap_height = desired(13)
    desired_irrelevant_cmap_height = desired(13)
    desired_cmap_superscript_height = desired(11)
    desired_rule_height = desired(15)
    serif, sans_serif = fonts.serif, fonts.sans_serif
    mf = fonts.make_mfont
    _set("%workspace-title-font%", mf(sans_serif, -desired_title_height, ["bold", "italic"]))
    _set("%codelet-count-font%", mf(sans_serif, -desired_count_height, ["italic"]))
    _set("%letter-font%", mf(serif, -desired_letter_height, ["bold", "italic"]))
    _set("%group-letter-category-font%",
         mf(serif, -desired_group_lcat_height, ["bold", "italic"]))
    _set("%relevant-group-length-font%",
         mf(serif, -desired_relevant_group_height, ["bold", "italic"]))
    _set("%irrelevant-group-length-font%",
         mf(serif, -desired_irrelevant_group_height, ["normal"]))
    _set("%bridge-label-font%", mf(sans_serif, -desired_bridge_label_height, ["italic"]))
    _set("%relevant-concept-mapping-font%",
         mf(serif, -desired_relevant_cmap_height, ["bold", "italic"]))
    _set("%irrelevant-concept-mapping-font%",
         mf(serif, -desired_irrelevant_cmap_height, ["italic"]))
    _set("%concept-mapping-list-superscript-font%",
         mf(sans_serif, -desired_cmap_superscript_height, ["normal"]))
    _set("%rule-font%", mf(serif, -desired_rule_height, ["italic"]))


def resize_workspace_fonts(win_width, win_height):
    """workspace-graphics.ss: resize-workspace-fonts"""
    def size(n):
        return utilities.round_(chez.mul(F(n, 600), win_height))
    tell(V.p_workspace_title_font, "resize", size(29))
    tell(V.p_codelet_count_font, "resize", size(15))
    tell(V.p_letter_font, "resize", size(28))
    tell(V.p_group_letter_category_font, "resize", size(21))
    tell(V.p_relevant_group_length_font, "resize", size(17))
    tell(V.p_irrelevant_group_length_font, "resize", size(13))
    tell(V.p_bridge_label_font, "resize", size(13))
    tell(V.p_relevant_concept_mapping_font, "resize", size(13))
    tell(V.p_irrelevant_concept_mapping_font, "resize", size(13))
    tell(V.p_concept_mapping_list_superscript_font, "resize", size(11))
    return tell(V.p_rule_font, "resize", size(15))


# port: SWL's thread-break.  The Workspace window's press handler interrupts
# the REPL thread to (go); the GUI (item 15) installs a handler that hands the
# thunk to its engine thread; without one (offscreen views) it raises, as in
# racket/gui/views.rkt.
def _no_thread_break_handler(thread, ignore, k):
    """port: the thread-break handler of offscreen views: there is no engine
    thread to interrupt"""
    raise chez.SchemeError("thread-break", "no engine thread to interrupt")


thread_break_handler = _no_thread_break_handler


def set_thread_break_handler(handler):
    """port: install SWL's thread-break (handler(thread, ignore, k))"""
    global thread_break_handler
    thread_break_handler = handler


def thread_break(thread, ignore, k):
    """port: SWL's thread-break, through the settable handler"""
    return thread_break_handler(thread, ignore, k)


def _theme_edit_mode_p():
    # theme-graphics.ss's *theme-edit-mode?* (gui/theme_graphics.py), #f
    # until that module is loaded
    import sys
    tg = sys.modules.get("metacat.gui.theme_graphics")
    return getattr(tg, "g_theme_edit_mode_p", False) if tg is not None else False


def workspace_window_press_handler(win, x, y):
    """workspace-graphics.ss: workspace-window-press-handler"""
    if run.g_running_p is not False:
        run.g_interrupt_p = True
    elif _theme_edit_mode_p() is not False:
        tell(setup.g_control_panel, "raise-theme-edit-dialog")
    elif run.g_display_mode_p is not False:
        V.restore_current_state()
    else:
        thread_break(setup.g_repl_thread, False, run.go)


def restore_current_state():
    """workspace-graphics.ss: restore-current-state"""
    tell(trace.g_trace, "unhighlight-all-events")
    tell(memory.g_memory, "unhighlight-all-answers")
    tell(setup.g_workspace_window, "redraw")
    tell(themes.g_themespace, "restore-current-state")
    tell(setup.g_slipnet_window, "restore-current-state")
    tell(setup.g_coderack_window, "restore-current-state")
    return tell(setup.g_temperature_window, "update-graphics", setup.g_temperature)


def make_workspace_window(*optional_args):
    """workspace-graphics.ss: make-workspace-window"""
    width = K.p_default_workspace_width if not optional_args else optional_args[0]
    window = new_workspace_window(width)
    tell(window, "initialize")
    return window


def _engine_gg():
    import metacat.general_graphics as gg
    return gg


def _rule_graphics():
    import metacat.rule_graphics as rg
    return rg


class WorkspaceWindow(SchemeObject):
    """workspace-graphics.ss: new-workspace-window (the closure)"""
    __slots__ = ("x_pixels", "y_pixels", "window_height", "graphics_window",
                 "letter_x_width", "letter_height", "pixel_value",
                 "max_string_field_width", "max_gap_width", "max_spanning_group_height",
                 "max_spanning_vertical_bridge_offset", "left_center", "right_center",
                 "top_line", "bottom_line", "title_y", "title_coord",
                 "codelet_count_text_x", "codelet_count_digits_x", "codelet_count_y",
                 "length_previously_relevant_p", "last_length_font",
                 "spanning_vertical_bridge_right_x", "spanning_cm_list_offset",
                 "cm_list_x_origin", "cm_list_y_origin", "cm_list_width")

    def __init__(this, x_pixels):
        this.x_pixels = x_pixels
        this.window_height = window_height = F(3, 4)
        this.y_pixels = y_pixels = utilities.ceiling(chez.mul(window_height, x_pixels))
        gw = this.graphics_window = GG.make_unscrollable_graphics_window(
            x_pixels, y_pixels, K.p_workspace_background_color)
        tell(gw, "set-mouse-handlers", workspace_window_press_handler, False)
        tell(gw, "set-icon-label", K.p_workspace_icon_label)
        if exists_p(K.p_workspace_icon_image):
            tell(gw, "set-icon-image", K.p_workspace_icon_image)
        if exists_p(K.p_workspace_window_title):
            tell(gw, "set-window-title", K.p_workspace_window_title)
        select_workspace_fonts(x_pixels, y_pixels)
        S = chez.String
        this.letter_x_width = tell(gw, "get-character-width", S("x"), V.p_letter_font)
        this.letter_height = tell(gw, "get-character-height", S(" "), V.p_letter_font)
        this.pixel_value = tell(gw, "get-width-per-pixel")
        this.max_string_field_width = F(3, 10)
        this.max_gap_width = chez.mul(4, this.letter_x_width)
        this.max_spanning_group_height = chez.mul(F(16, 5), this.letter_height)
        this.max_spanning_vertical_bridge_offset = F(3, 100)
        this.left_center = F(1, 4)
        this.right_center = F(3, 4)
        this.top_line = chez.sub(window_height, F(7, 25))
        this.bottom_line = chez.sub(window_height, F(57, 100))
        this._compute_title_and_count()
        this.length_previously_relevant_p = False
        this.last_length_font = False
        this.spanning_vertical_bridge_right_x = False
        this.spanning_cm_list_offset = F(1, 50)
        this.cm_list_x_origin = False
        this.cm_list_y_origin = chez.sub(this.bottom_line,
                                         chez.mul(F(7, 10), this.max_spanning_group_height))
        this.cm_list_width = chez.mul(11, tell(gw, "get-character-width", S("m"),
                                               V.p_relevant_concept_mapping_font))

    def _compute_title_and_count(this):
        # the let*'s title-y ... codelet-count-y, also update-fonts' set!s
        gw = this.graphics_window
        S = chez.String
        this.title_y = chez.sub(this.window_height, chez.mul(
            F(16, 9), tell(gw, "get-string-height", V.p_workspace_title_font)))
        this.title_coord = [_HALF, this.title_y]
        this.codelet_count_text_x = chez.sub(_HALF, chez.mul(_HALF, tell(
            gw, "get-string-width", S("(Codelets run: 888)"), V.p_codelet_count_font)))
        this.codelet_count_digits_x = chez.add(this.codelet_count_text_x, tell(
            gw, "get-string-width", S("(Codelets run: "), V.p_codelet_count_font))
        this.codelet_count_y = chez.sub(this.title_y, chez.mul(
            F(3, 2), tell(gw, "get-string-height", V.p_codelet_count_font)))

    def _draw_workspace_arrow(this, line):
        """workspace-graphics.ss: draw-workspace-arrow (the let*'s lambda)"""
        L = p_workspace_arrow_length
        return tell(this.graphics_window, "draw", _engine_gg().centered_double_arrow(
            _HALF, chez.add(line, chez.mul(F(1, 3), this.letter_height)),
            0, L, chez.mul(F(1, 7), L), chez.mul(F(3, 7), L), 60))

    def _question_mark(this, op, mark, *optional_args):
        """workspace-graphics.ss: question-mark (the let*'s lambda)"""
        pexp = ["let-sgl", [["font", V.p_letter_font], ["text-justification", "center"]],
                ["text", [this.right_center, this.bottom_line], mark]]
        if op == "draw":
            if optional_args:
                V.g_fg_color = optional_args[0]
            tell(this.graphics_window, "draw", pexp)
            V.g_fg_color = V.p_default_fg_color
        elif op == "erase":
            tell(this.graphics_window, "erase", pexp)
        return "done"

    @message("object-type")
    def object_type(this, self):
        return "workspace-window"

    @message("get-spanning-vertical-bridge-right-x")
    def get_spanning_vertical_bridge_right_x(this, self):
        return this.spanning_vertical_bridge_right_x

    @message("get-spanning-cm-list-offset")
    def get_spanning_cm_list_offset(this, self):
        return this.spanning_cm_list_offset

    @message("get-rule-coord")
    def get_rule_coord(this, self, rule_type, rule_height):
        if rule_type == "top":
            line = this.top_line
        elif rule_type == "bottom":
            line = this.bottom_line
        else:
            line = None   # 1.2: case without else
        return [this.right_center,
                chez.sub(line,
                         # Fudge to make the rule borders even:
                         chez.add(chez.mul(_HALF, this.max_spanning_group_height),
                                  this.pixel_value),
                         chez.mul(_HALF, rule_height))]

    @message("get-cm-list-coord")
    def get_cm_list_coord(this, self, label_num):
        return [chez.add(this.cm_list_x_origin, chez.mul(chez.sub1(label_num), this.cm_list_width)),
                this.cm_list_y_origin]

    @message("update-graphics")
    def update_graphics(this, self):
        gw = this.graphics_window
        tell(gw, "caching-on")
        tell(self, "repair-built-bridges")
        tell(self, "update-group-length-graphics")
        tell(self, "update-concept-mapping-graphics")
        tell(self, "update-codelet-count", setup.g_codelet_count)
        return tell(gw, "flush")

    @message("draw-header")
    def draw_header(this, self, text):
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["font", V.p_workspace_title_font],
                                 ["text-justification", "center"]],
                     ["text", this.title_coord, text]])

    @message("draw-event-header")
    def draw_event_header(this, self, text, event):
        event_number = tell(event, "get-event-number")
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["font", V.p_workspace_title_font],
                                 ["text-justification", "center"]],
                     ["text", this.title_coord,
                      chez.String(chez.format_("Event ~a:  ~a", event_number, text))]])

    # draw-codelet-count draws "(Codelets run: nnn)"
    @message("draw-codelet-count")
    def draw_codelet_count(this, self, codelet_count):
        tell(this.graphics_window, "draw",
             ["let-sgl", [["font", V.p_codelet_count_font]],
              ["text", [this.codelet_count_text_x, this.codelet_count_y],
               chez.String("(Codelets run:")]])
        return tell(self, "update-codelet-count", codelet_count)

    # update-codelet-count draws "nnn)"
    @message("update-codelet-count")
    def update_codelet_count(this, self, n):
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["font", V.p_codelet_count_font], ["text-mode", "image"]],
                     ["text", [this.codelet_count_digits_x, this.codelet_count_y],
                      chez.String(chez.format_("~a)     ", n))]])

    @message("draw-bridge")
    def draw_bridge(this, self, bridge):
        gw = this.graphics_window
        orientation = tell(bridge, "get-orientation")
        if orientation == "horizontal":
            tell(gw, "draw", tell(bridge, "get-graphics-pexp"))
        elif orientation == "vertical":
            tell(gw, "draw",
                 ["let-sgl", [["background-color", V.p_bridge_label_background_color],
                              ["text-mode", "image"]],
                  tell(bridge, "get-graphics-pexp")])
            if tell(bridge, "concept-mapping-graphics-active?") is not False:
                if tell(bridge, "group-spanning-bridge?") is False:
                    tell(self, "concept-mapping-list-superscript", "draw", bridge)
                for cm in tell(bridge, "get-all-concept-mappings"):
                    tell(self, "draw-concept-mapping", cm)
        return tell(bridge, "set-drawn?", True)

    @message("erase-bridge")
    def erase_bridge(this, self, bridge):
        gw = this.graphics_window
        orientation = tell(bridge, "get-orientation")
        if orientation == "horizontal":
            tell(gw, "erase", tell(bridge, "get-graphics-pexp"))
        elif orientation == "vertical":
            tell(gw, "erase", ["let-sgl", [["text-mode", "image"]],
                               tell(bridge, "get-graphics-pexp")])
            if tell(bridge, "concept-mapping-graphics-active?") is not False:
                if tell(bridge, "group-spanning-bridge?") is False:
                    tell(self, "concept-mapping-list-superscript", "erase", bridge)
                for cm in tell(bridge, "get-all-concept-mappings"):
                    tell(self, "erase-concept-mapping", cm)
        return tell(bridge, "set-drawn?", False)

    # Concept-mappings
    @message("draw-concept-mapping")
    def draw_concept_mapping(this, self, cm):
        pexp = tell(cm, "get-graphics-pexp")
        currently_relevant_p = tell(cm, "relevant?")
        font = (V.p_relevant_concept_mapping_font if currently_relevant_p is not False
                else V.p_irrelevant_concept_mapping_font)
        tell(this.graphics_window, "draw", ["let-sgl", [["font", font]], pexp])
        return tell(cm, "update-previously-relevant?", currently_relevant_p)

    @message("erase-concept-mapping")
    def erase_concept_mapping(this, self, cm):
        pexp = tell(cm, "get-graphics-pexp")
        previously_relevant_p = tell(cm, "previously-relevant?")
        font = (V.p_relevant_concept_mapping_font if previously_relevant_p is not False
                else V.p_irrelevant_concept_mapping_font)
        return tell(this.graphics_window, "erase", ["let-sgl", [["font", font]], pexp])

    @message("concept-mapping-list-superscript")
    def concept_mapping_list_superscript(this, self, op, bridge):
        return tell(this.graphics_window, op,
                    ["let-sgl", [["font", V.p_concept_mapping_list_superscript_font],
                                 ["text-justification", "right"],
                                 ["origin", tell(bridge, "get-cm-list-coord")]],
                     ["text", ["text-relative", [0, F(1, 3)]],
                      chez.String(chez.format_("~a", tell(bridge, "get-bridge-label-number")))]])

    @message("update-concept-mapping-graphics")
    def update_concept_mapping_graphics(this, self):
        gw = this.graphics_window
        for cm in tell(workspace.g_workspace, "get-all-vertical-CMs"):
            currently_relevant_p = tell(cm, "relevant?")
            previously_relevant_p = tell(cm, "previously-relevant?")
            if not chez.eq_p(currently_relevant_p, previously_relevant_p):
                current_font = (V.p_relevant_concept_mapping_font
                                if currently_relevant_p is not False
                                else V.p_irrelevant_concept_mapping_font)
                last_font = (V.p_relevant_concept_mapping_font
                             if previously_relevant_p is not False
                             else V.p_irrelevant_concept_mapping_font)
                pexp = tell(cm, "get-graphics-pexp")
                tell(gw, "erase", ["let-sgl", [["font", last_font]], pexp])
                tell(gw, "draw", ["let-sgl", [["font", current_font]], pexp])
                tell(cm, "update-previously-relevant?", currently_relevant_p)
        return "done"

    # Letters and groups
    @message("draw-all-letters")
    def draw_all_letters(this, self):
        for letter in tell(workspace.g_workspace, "get-all-letters"):
            tell(this.graphics_window, "draw", tell(letter, "get-graphics-pexp"))
        return "done"

    @message("draw-group")
    def draw_group(this, self, group):
        gw = this.graphics_window
        tell(gw, "draw", tell(group, "get-graphics-pexp"))
        if tell(group, "length-graphics-active?") is not False:
            tell(gw, "draw", ["let-sgl", [["font", this.last_length_font]],
                              tell(group, "get-length-graphics-pexp")])
        return tell(group, "set-drawn?", True)

    @message("erase-group")
    def erase_group(this, self, group):
        gw = this.graphics_window
        tell(gw, "erase", tell(group, "get-graphics-pexp"))
        if tell(group, "length-graphics-active?") is not False:
            tell(gw, "erase", ["let-sgl", [["font", this.last_length_font]],
                               tell(group, "get-length-graphics-pexp")])
        return tell(group, "set-drawn?", False)

    @message("draw-group-length")
    def draw_group_length(this, self, group):
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["font", this.last_length_font]],
                     tell(group, "get-length-graphics-pexp")])

    @message("update-group-length-graphics")
    def update_group_length_graphics(this, self):
        gw = this.graphics_window
        length_currently_relevant_p = slipnet.fully_active_p(slipnet.plato_length)
        if not chez.eq_p(length_currently_relevant_p, this.length_previously_relevant_p):
            current_length_font = (V.p_relevant_group_length_font
                                   if length_currently_relevant_p is not False
                                   else V.p_irrelevant_group_length_font)
            for group in tell(workspace.g_workspace, "get-groups"):
                if tell(group, "length-graphics-enabled?") is not False:
                    pexp = tell(group, "get-length-graphics-pexp")
                    tell(gw, "erase", ["let-sgl", [["font", this.last_length_font]], pexp])
                    tell(gw, "draw", ["let-sgl", [["font", current_length_font]], pexp])
            this.length_previously_relevant_p = length_currently_relevant_p
            this.last_length_font = current_length_font
        return "done"

    # Rules
    @message("draw-rule")
    def draw_rule(this, self, rule, color):
        tell(this.graphics_window, "draw",
             ["let-sgl", [["foreground-color", color]], tell(rule, "get-graphics-pexp")])
        return tell(rule, "set-drawn?", True)

    @message("erase-rule")
    def erase_rule(this, self, rule):
        tell(this.graphics_window, "erase", tell(rule, "get-graphics-pexp"))
        return tell(rule, "set-drawn?", False)

    # String letters
    @message("draw-string-letters")
    def draw_string_letters(this, self, string, *_ignored):
        # chez: record-case ignores extra arguments; trace.ss sends a tag
        # (docs/anomalies_and_quirks.md, "Chez's record-case ignores extra arguments")
        for letter in tell(string, "get-letters"):
            tell(this.graphics_window, "draw", tell(letter, "get-graphics-pexp"))
        return "done"

    @message("erase-string-letters")
    def erase_string_letters(this, self, string):
        for letter in tell(string, "get-letters"):
            tell(this.graphics_window, "erase", tell(letter, "get-graphics-pexp"))
        return "done"

    def _in_color(this, self, object_, color, group_msg, cm_msg):
        # draw-in-color and display-in-color, which differ only in the
        # messages for groups and concept-mappings
        gw = this.graphics_window
        V.g_fg_color = color
        if utilities.group_p(object_):
            tell(self, group_msg, object_)
        elif utilities.concept_mapping_p(object_):
            tell(self, cm_msg, object_)
        elif utilities.vertical_bridge_p(object_):
            tell(gw, "draw", tell(object_, "get-graphics-pexp"))
            if tell(object_, "group-spanning-bridge?") is False:
                # graphics-pexp is (let-sgl () <zigzag-pexp> <label-pexp>).
                # Draw the label in the default color on a yellow background:
                V.g_fg_color = V.p_default_fg_color
                tell(gw, "draw",
                     ["let-sgl", [["background-color", V.p_bridge_label_background_color],
                                  ["text-mode", "image"]],
                      tell(object_, "get-graphics-pexp")[3]])
        else:
            tell(gw, "draw", tell(object_, "get-graphics-pexp"))
        V.g_fg_color = V.p_default_fg_color
        return "done"

    @message("draw-in-color")
    def draw_in_color(this, self, object_, color):
        return this._in_color(self, object_, color, "draw-group", "draw-concept-mapping")

    @message("initialize")
    def initialize(this, self):
        gw = this.graphics_window
        tell(gw, "caching-on")
        tell(gw, "clear")
        tell(self, "draw-header", chez.String("Workspace"))
        tell(self, "draw-codelet-count", 0)
        this.length_previously_relevant_p = False
        this.last_length_font = V.p_irrelevant_group_length_font
        tell(gw, "flush")
        run.g_display_mode_p = False
        return "done"

    @message("draw-problem")
    def draw_problem(this, self, initial_string, modified_string, target_string, answer_string):
        gw = this.graphics_window
        tell(self, "initialize")
        tell(gw, "caching-on")
        tell(self, "init-string-graphics", initial_string, this.left_center, "top")
        tell(self, "init-string-graphics", modified_string, this.right_center, "top")
        tell(self, "init-string-graphics", target_string, this.left_center, "bottom")
        if setup.p_justify_mode is not False:
            tell(self, "init-string-graphics", answer_string, this.right_center, "bottom")
        tell(self, "draw-string-letters", initial_string)
        this._draw_workspace_arrow(this.top_line)
        tell(self, "draw-string-letters", modified_string)
        tell(self, "draw-string-letters", target_string)
        this._draw_workspace_arrow(this.bottom_line)
        if setup.p_justify_mode is not False:
            tell(self, "draw-string-letters", answer_string)
        else:
            this._question_mark("draw", chez.String("?"))
        this.cm_list_x_origin = chez.max_(
            this.spanning_cm_list_offset,
            chez.sub(this.left_center,
                     chez.mul(_HALF, tell(target_string, "get-graphics-width")),
                     chez.mul(_HALF, this.cm_list_width)))
        spanning_vertical_bridge_left_x = chez.max_(
            tell(initial_string, "get-spanning-group-x2"),
            tell(target_string, "get-spanning-group-x2"))
        available_space = chez.sub(chez.sub(_HALF, chez.mul(_HALF, p_workspace_arrow_length)),
                                   spanning_vertical_bridge_left_x)
        this.spanning_vertical_bridge_right_x = chez.add(
            spanning_vertical_bridge_left_x,
            chez.min_(chez.mul(_HALF, available_space),
                      this.max_spanning_vertical_bridge_offset))
        tell(gw, "flush")
        return "done"

    @message("init-translated-string-graphics")
    def init_translated_string_graphics(this, self, string, placement):
        return tell(self, "init-string-graphics", string, this.right_center, placement)

    @message("init-string-graphics")
    def init_string_graphics(this, self, string, x_center, string_placement):
        gw = this.graphics_window
        letter_font = V.p_letter_font
        letters = tell(string, "get-letters")
        num_of_letters = len(letters)
        letter_widths = chez.map_(
            lambda letter: tell(gw, "get-character-width", tell(letter, "print-name"),
                                letter_font),
            letters)
        total_letter_width = utilities.sum_(letter_widths)
        if num_of_letters == 1:
            gap_width = 0
        else:
            gap_width = chez.min_(this.max_gap_width,
                                  chez.div(chez.sub(this.max_string_field_width,
                                                    total_letter_width),
                                           chez.sub1(num_of_letters)))
        string_width = chez.add(total_letter_width, chez.mul(chez.sub1(num_of_letters), gap_width))
        next_x = chez.sub(x_center, chez.mul(_HALF, string_width))
        letter_height = this.letter_height
        for letter, letter_width in zip(letters, letter_widths):
            print_name = tell(letter, "print-name")
            if string_placement == "top":
                text_coord = [next_x, this.top_line]
            elif string_placement == "bottom":
                text_coord = [next_x, this.bottom_line]
            else:
                text_coord = None   # 1.2: case without else
            bbox = tell(gw, "get-character-bounding-box", print_name, letter_font, text_coord)
            mid_x = chez.mul(_HALF, chez.add(bbox[0][0], bbox[1][0]))
            top_y = chez.add(chez.mul(F(-1, 6), letter_height), bbox[1][1])
            bot_y = chez.add(chez.mul(F(1, 6), letter_height), bbox[0][1])
            tell(letter, "set-graphics-pexp",
                 ["let-sgl", [["font", letter_font]], ["text", text_coord, print_name]])
            tell(letter, "set-graphics-text-coord", text_coord)
            tell(letter, "set-graphics-coords", bbox[0], bbox[1])
            if string_placement == "top":
                y = bot_y
            elif string_placement == "bottom":
                y = top_y
            else:
                y = None   # 1.2: case without else
            tell(letter, "set-bridge-graphics-coords",
                 utilities.coord(mid_x, y), utilities.coord(mid_x, top_y))
            next_x = chez.add(next_x, letter_width, gap_width)
        if num_of_letters == 1:
            x_enclosure_delta = total_letter_width
        else:
            x_enclosure_delta = chez.div(gap_width, max(3, num_of_letters))
        if num_of_letters == 1:
            y_enclosure_delta = total_letter_width
        else:
            y_enclosure_delta = chez.min_(
                chez.mul(F(1, 4), letter_height),
                chez.div(chez.sub(this.max_spanning_group_height, letter_height),
                         chez.mul(2, chez.sub1(num_of_letters))))
        spanning_group_width = chez.add(
            string_width, chez.mul(2, chez.sub1(num_of_letters), x_enclosure_delta))
        spanning_group_height = chez.add(
            letter_height, chez.mul(2, chez.sub1(num_of_letters), y_enclosure_delta))
        y_center = chez.mul(_HALF, chez.add(tell(letters[0], "get-graphics-y1"),
                                            tell(letters[0], "get-graphics-y2")))
        spanning_group_x1 = chez.sub(x_center, chez.mul(_HALF, spanning_group_width))
        spanning_group_y1 = chez.sub(y_center, chez.mul(_HALF, spanning_group_height))
        spanning_group_x2 = chez.add(x_center, chez.mul(_HALF, spanning_group_width))
        spanning_group_y2 = chez.add(y_center, chez.mul(_HALF, spanning_group_height))
        return tell(string, "set-graphics-info",
                    x_enclosure_delta, y_enclosure_delta, string_width,
                    spanning_group_x1, spanning_group_y1,
                    spanning_group_x2, spanning_group_y2)

    # draw-current-answer and draw-current-snag draw concept-mappings
    # and group-lengths in the currently appropriate font (relevant
    # vs. irrelevant):

    @message("draw-current-answer")
    def draw_current_answer(this, self):
        gw = this.graphics_window
        answer = tell(trace.g_trace, "get-last-event", "answer")
        answer_string = tell(answer, "get-answer-string")
        top_rule = tell(answer, "get-rule", "top")
        bottom_rule = tell(answer, "get-rule", "bottom")
        tell(gw, "caching-on")
        tell(self, "update-codelet-count", setup.g_codelet_count)
        # Draw answer string if necessary:
        if setup.p_justify_mode is False:
            this._question_mark("erase", chez.String("?"))
            tell(self, "draw-string-letters", answer_string)
            for g in tell(answer_string, "get-groups"):
                tell(self, "draw-group", g)
        # Highlight vertical mapping:
        tell(self, "highlight-current-bridges", V.p_vertical_bridge_color,
             tell(answer, "get-supporting-bridges", "vertical"))
        # Highlight applied vertical slippages:
        tell(self, "highlight-current-slippages", tell(answer, "get-slippage-log"))
        # Highlight top mapping:
        tell(self, "highlight-current-bridges", V.p_top_bridge_color,
             tell(answer, "get-supporting-bridges", "top"))
        # Highlight bottom mapping:
        tell(self, "highlight-current-bridges", V.p_bottom_bridge_color,
             tell(answer, "get-supporting-bridges", "bottom"))
        # Draw rules:
        tell(self, "draw-rule", top_rule, V.p_top_rule_color)
        tell(self, "draw-rule", bottom_rule, V.p_bottom_rule_color)
        tell(gw, "flush")
        return "done"

    @message("erase-current-answer")
    def erase_current_answer(this, self):
        gw = this.graphics_window
        answer = tell(trace.g_trace, "get-last-event", "answer")
        answer_string = tell(answer, "get-answer-string")
        tell(gw, "caching-on")
        tell(self, "erase-rule", tell(answer, "get-rule", "top"))
        tell(self, "erase-rule", tell(answer, "get-rule", "bottom"))
        # Erase bottom mapping if running in normal mode:
        if setup.p_justify_mode is False:
            for b in tell(answer, "get-supporting-bridges", "bottom"):
                tell(self, "erase-bridge", b)
            for g in tell(answer_string, "get-groups"):
                tell(self, "erase-group", g)
            tell(self, "erase-string-letters", answer_string)
            this._question_mark("draw", chez.String("?"))
        # This unhighlights everything else:
        tell(self, "repair-all-graphics")
        return tell(gw, "flush")

    @message("draw-current-snag")
    def draw_current_snag(this, self):
        gw = this.graphics_window
        snag = tell(trace.g_trace, "get-last-event", "snag")
        rule = tell(snag, "get-rule", "top")
        translated_rule = tell(snag, "get-rule", "bottom")
        tell(gw, "caching-on")
        tell(self, "update-codelet-count", setup.g_codelet_count)
        # Highlight vertical mapping:
        tell(self, "highlight-current-bridges", V.p_vertical_bridge_color,
             tell(snag, "get-supporting-bridges", "vertical"))
        # Highlight applied vertical slippages:
        tell(self, "highlight-current-slippages", tell(snag, "get-slippage-log"))
        # Highlight top mapping:
        tell(self, "highlight-current-bridges", V.p_top_bridge_color,
             tell(snag, "get-supporting-bridges", "top"))
        # Highlight snag objects:
        for object_ in tell(snag, "get-snag-objects"):
            if not utilities.workspace_string_p(object_):
                tell(self, "draw-in-color", object_, V.p_snag_color)
        # Draw rules:
        tell(self, "draw-rule", rule, V.p_top_rule_color)
        tell(self, "draw-rule", translated_rule, V.p_snag_color)
        this._question_mark("erase", chez.String("?"))
        this._question_mark("draw", chez.String("???"), V.p_snag_color)
        return tell(gw, "flush")

    @message("erase-current-snag")
    def erase_current_snag(this, self):
        gw = this.graphics_window
        snag = tell(trace.g_trace, "get-last-event", "snag")
        rule = tell(snag, "get-rule", "top")
        translated_rule = tell(snag, "get-rule", "bottom")
        crossout_pexp = make_snag_crossout_pexp(
            translated_rule, utilities.ceiling(chez.div(this.x_pixels, 160)))
        tell(gw, "caching-on")
        tell(self, "draw", crossout_pexp)
        tell(gw, "flush")
        utilities.pause(V.p_snag_pause)
        tell(gw, "caching-on")
        this._question_mark("erase", chez.String("???"))
        this._question_mark("draw", chez.String("?"))
        tell(self, "erase", crossout_pexp)
        tell(self, "erase-rule", translated_rule)
        tell(self, "erase-rule", rule)
        # This unhighlights everything else:
        tell(self, "repair-all-graphics")
        return tell(gw, "flush")

    @message("workspace-arrow")
    def workspace_arrow(this, self, line):
        if line == "top":
            return this._draw_workspace_arrow(this.top_line)
        if line == "bottom":
            return this._draw_workspace_arrow(this.bottom_line)
        return None

    @message("question-mark")
    def question_mark(this, self, op, mark, color):
        return this._question_mark(op, mark, color)

    @message("resize")
    def resize(this, self, new_width, new_height):
        gw = this.graphics_window
        this.x_pixels = new_width
        this.y_pixels = new_height
        tell(self, "update-fonts")
        tell(self, "update-rule-pexps")
        tell(gw, "retag", "all", "garbage")
        highlighted_event = tell(trace.g_trace, "get-highlighted-event")
        highlighted_answer = tell(memory.g_memory, "get-highlighted-description")
        if exists_p(highlighted_event):
            tell(highlighted_event, "display-workspace")
        elif exists_p(highlighted_answer):
            tell(highlighted_answer, "display-workspace")
        else:
            tell(self, "refresh-graphics")
        return tell(gw, "delete", "garbage")

    @message("update-fonts")
    def update_fonts(this, self):
        resize_workspace_fonts(this.x_pixels, this.y_pixels)
        this._compute_title_and_count()
        return "done"

    @message("update-rule-pexps")
    def update_rule_pexps(this, self):
        rg = _rule_graphics()
        for rule in tell(workspace.g_workspace, "get-all-rules"):
            rg.initialize_rule_graphics(rule)
        other_rules = tell_all(tell(trace.g_trace, "get-events", "answer")
                               + tell(trace.g_trace, "get-events", "snag"),
                               "get-rule", "bottom")
        for rule in other_rules:
            if tell(rule, "translated?") is not False:
                rg.initialize_rule_graphics(rule)
        # port: as in the Racket port, the updated pexp that update-rule-pexps!
        # returns is stored back (the original relies on its set-car!s;
        # docs/porting-notes.md, item 13)
        for answer in tell(memory.g_memory, "get-answers"):
            tell(answer, "set-answer-description-pexp",
                 rg.update_rule_pexps_bang(tell(answer, "get-answer-description-pexp")))
        for snag in tell(memory.g_memory, "get-snags"):
            tell(snag, "set-snag-description-pexp",
                 rg.update_rule_pexps_bang(tell(snag, "get-snag-description-pexp")))
        return "done"

    @message("redraw")
    def redraw(this, self):
        gw = this.graphics_window
        tell(gw, "caching-on")
        tell(gw, "clear")
        tell(self, "refresh-graphics")
        tell(gw, "flush")
        run.g_display_mode_p = False
        return "done"

    @message("refresh-graphics")
    def refresh_graphics(this, self):
        tell(self, "draw-header", chez.String("Workspace"))
        tell(self, "draw-codelet-count", setup.g_codelet_count)
        if exists_p(run.g_this_run):
            tell(self, "repair-all-graphics")
            if tell(trace.g_trace, "current-answer?") is not False:
                tell(self, "draw-current-answer")
            elif tell(trace.g_trace, "immediate-snag-condition?") is not False:
                tell(self, "draw-current-snag")
            elif setup.p_justify_mode is False:
                this._question_mark("draw", chez.String("?"))
        return "done"

    @message("garbage-collect")
    def garbage_collect(this, self):
        gw = this.graphics_window
        tell(gw, "retag", "all", "garbage")
        tell(self, "refresh-graphics")
        tell(gw, "delete", "garbage")
        return "done"

    # Repair graphics:
    @message("repair-all-graphics")
    def repair_all_graphics(this, self):
        gw = this.graphics_window
        ws = workspace.g_workspace
        tell(self, "draw-string-letters", workspace.g_initial_string)
        this._draw_workspace_arrow(this.top_line)
        tell(self, "draw-string-letters", workspace.g_modified_string)
        tell(self, "draw-string-letters", workspace.g_target_string)
        this._draw_workspace_arrow(this.bottom_line)
        if setup.p_justify_mode is not False:
            tell(self, "draw-string-letters", workspace.g_answer_string)
        for g in tell(ws, "get-all-groups"):
            if tell(g, "drawn?") is not False:
                tell(self, "draw-group", g)
        for b in tell(ws, "get-all-bridges") + tell(ws, "get-all-proposed-bridges"):
            if tell(b, "drawn?") is not False:
                tell(self, "draw-bridge", b)
        for r in tell(ws, "get-clamped-rules"):
            tell(gw, "draw", tell(r, "get-clamped-graphics-pexp"))
        return "done"

    @message("repair-built-bridges")
    def repair_built_bridges(this, self):
        gw = this.graphics_window
        for bridge in tell(workspace.g_workspace, "get-all-bridges"):
            bridge_type = tell(bridge, "get-bridge-type")
            if bridge_type in ("top", "bottom"):
                pexp = tell(bridge, "get-graphics-pexp")
            elif bridge_type == "vertical":
                pexp = ["let-sgl", [["background-color", V.p_bridge_label_background_color],
                                    ["text-mode", "image"]],
                        tell(bridge, "get-graphics-pexp")]
            else:
                pexp = None   # 1.2: case without else
            tell(gw, "draw", pexp)
        return "done"

    # The following methods are used when displaying structures that
    # do not necessarily exist at the current *codelet-count* time.
    # The group-length and concept-mapping fonts are always the same,
    # rather than being chosen dynamically as relevant vs. irrelevant.

    @message("display-in-color")
    def display_in_color(this, self, object_, color):
        return this._in_color(self, object_, color, "display-group", "display-concept-mapping")

    @message("display-bridge")
    def display_bridge(this, self, bridge):
        gw = this.graphics_window
        orientation = tell(bridge, "get-orientation")
        if orientation == "horizontal":
            tell(gw, "draw", tell(bridge, "get-graphics-pexp"))
        elif orientation == "vertical":
            tell(gw, "draw",
                 ["let-sgl", [["background-color", V.p_bridge_label_background_color],
                              ["text-mode", "image"]],
                  tell(bridge, "get-graphics-pexp")])
            if tell(bridge, "group-spanning-bridge?") is False:
                tell(self, "concept-mapping-list-superscript", "draw", bridge)
            for cm in tell(bridge, "get-all-concept-mappings"):
                tell(self, "display-concept-mapping", cm)
        return "done"

    @message("display-concept-mapping")
    def display_concept_mapping(this, self, cm):
        pexp = tell(cm, "get-graphics-pexp")
        font = V.p_relevant_concept_mapping_font
        return tell(this.graphics_window, "draw", ["let-sgl", [["font", font]], pexp])

    @message("display-group")
    def display_group(this, self, group):
        gw = this.graphics_window
        tell(gw, "draw", tell(group, "get-graphics-pexp"))
        if tell(group, "length-graphics-active?") is not False:
            tell(gw, "draw", ["let-sgl", [["font", V.p_relevant_group_length_font]],
                              tell(group, "get-length-graphics-pexp")])
        return "done"

    @message("display-rule")
    def display_rule(this, self, rule, color):
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["foreground-color", color]], tell(rule, "get-graphics-pexp")])

    @message("highlight-bridges")
    def highlight_bridges(this, self, color, bridges):
        for b in bridges:
            tell(self, "display-in-color", tell(b, "get-object1"), color)
            tell(self, "display-in-color", b, color)
            tell(self, "display-in-color", tell(b, "get-object2"), color)

    @message("highlight-applied-slippages")
    def highlight_applied_slippages(this, self, slippage_log):
        for s in tell(slippage_log, "get-applied-slippages"):
            h = tell(slippage_log, "get-slippage-to-highlight", s)
            tell(self, "display-in-color", h, tell(slippage_log, "get-highlight-color", h, s))

    # These two methods use 'draw-in-color instead of 'display-in-color:

    @message("highlight-current-bridges")
    def highlight_current_bridges(this, self, color, bridges):
        for b in bridges:
            tell(self, "draw-in-color", tell(b, "get-object1"), color)
            tell(self, "draw-in-color", b, color)
            tell(self, "draw-in-color", tell(b, "get-object2"), color)

    @message("highlight-current-slippages")
    def highlight_current_slippages(this, self, slippage_log):
        for s in tell(slippage_log, "get-applied-slippages"):
            h = tell(slippage_log, "get-slippage-to-highlight", s)
            tell(self, "draw-in-color", h, tell(slippage_log, "get-highlight-color", h, s))

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


def new_workspace_window(x_pixels):
    """workspace-graphics.ss: new-workspace-window"""
    return WorkspaceWindow(x_pixels)


def make_snag_crossout_pexp(rule, line_width):
    """workspace-graphics.ss: make-snag-crossout-pexp"""
    center_coord = tell(rule, "get-rule-graphics-center-coord")
    x_length = F(1, 10)
    y_length = chez.mul(F(3, 2), tell(rule, "get-rule-graphics-height"))
    x1 = chez.sub(center_coord[0], chez.mul(_HALF, x_length))
    y1 = chez.sub(center_coord[1], chez.mul(_HALF, y_length))
    x2 = chez.add(center_coord[0], chez.mul(_HALF, x_length))
    y2 = chez.add(center_coord[1], chez.mul(_HALF, y_length))
    return ["let-sgl", [["line-width", line_width]],
            ["line", [x1, y1], [x2, y2], [x1, y2], [x2, y1]]]


def load():
    """port: install restore-current-state in the engine (run.ss's go calls it
    in display mode), as loading the file defined it on the shared top level"""
    engine.set_global("restore-current-state", restore_current_state)
