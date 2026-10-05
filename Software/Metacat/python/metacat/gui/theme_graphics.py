"""theme-graphics.ss: the Themespace window, its three theme windows and their panels.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from theme-graphics.ss, with
racket/gui/theme-graphics.rktl as a worked translation.

The definitions are the original's, in its order, but for relation-name, which
is the engine's (metacat/theme_graphics.py: trace.ss's print-pattern names
relations with it) and is used from there.  The closures of
new-themespace-window, make-bridge-themes-window and make-panel are
`ThemespaceWindow`, `BridgeThemesWindow` and `Panel`; their variables are
attributes.  *panel-order* and *panel-theme-order* list slipnodes, so `load()`
makes them once the slipnet exists.  Nothing here is read by the model: the
model talks to the windows through setup.py's *themespace-window* and
*top-themes-window* ... globals, which the views set.  *theme-edit-mode?* is
this module's (gui.ss sets it; workspace-, trace- and memory-graphics read it).
The model's globals (*themespace*, *control-panel*, %justify-mode%, *fg-color*)
are read and set qualified, at call time.  Arithmetic is Chez's (chez.py).
This module does not import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import chez, setup, slipnet, themes, view_globals
from metacat import theme_graphics as engine_theme_graphics
from metacat.chez import add, div, mul, sub
from metacat.objects import SchemeObject, delegate, delegate_to_all, message, tell
from metacat.utilities import (ascending_index_list, base_object, ceiling, exists_p,
                               fourth, map_compress, percent, round_, second,
                               select_meth, sort_wrt_order, tell_all, third)

String = chez.String
F = Fraction


def _map_add(a, b):
    """(map + a b) on two coordinates"""
    return chez.map_(add, a, b)


# <layout> = (<top-window-layout> <bottom-window-layout> <vertical-window-layout>)
# <window-layout> =  (<theme-type> <rows> <cols> <panel-orientation>)
# where <rows> or <cols> = * means "unbounded"

g_themespace_window_layout = [
    ["top-bridge", 2, "*", "horizontal"],
    ["bottom-bridge", 2, "*", "horizontal"],
    ["vertical-bridge", "*", 2, "vertical"]]

g_theme_edit_mode_p = False

# ------------------------------------------------------------------------------------
# mouse handlers


def theme_window_left_press_handler(theme_type):
    """theme-graphics.ss: theme-window-left-press-handler"""
    def handler(win, x, y):
        if g_theme_edit_mode_p is not False:
            if tell(setup.g_control_panel, "ready-to-edit?", theme_type) is not False:
                return tell(setup.g_control_panel, "edit-theme-type", theme_type)
            theme = select_meth(tell(themes.g_themespace, "get-themes", theme_type),
                                "clicked?", x, y)
            if exists_p(theme):
                dimension = tell(theme, "get-dimension")
                relation = tell(theme, "get-relation")
                activation = tell(theme, "get-activation")
                themes.set_themes(theme_type, dimension, 0)
                if not activation == 100:
                    return themes.set_themes(theme_type, dimension, relation, 100)
        return None
    return handler


def theme_window_right_press_handler(theme_type):
    """theme-graphics.ss: theme-window-right-press-handler"""
    def handler(win, x, y):
        if g_theme_edit_mode_p is not False:
            if tell(setup.g_control_panel, "ready-to-edit?", theme_type) is not False:
                return tell(setup.g_control_panel, "edit-theme-type", theme_type)
            theme = select_meth(tell(themes.g_themespace, "get-themes", theme_type),
                                "clicked?", x, y)
            if exists_p(theme):
                dimension = tell(theme, "get-dimension")
                relation = tell(theme, "get-relation")
                activation = tell(theme, "get-activation")
                themes.set_themes(theme_type, dimension, 0)
                if not activation == -100:
                    return themes.set_themes(theme_type, dimension, relation, -100)
        return None
    return handler


def set_theme_window_mouse_handlers(window, theme_type):
    """theme-graphics.ss: set-theme-window-mouse-handlers"""
    return tell(window, "set-mouse-handlers",
                theme_window_left_press_handler(theme_type),
                theme_window_right_press_handler(theme_type))

# ------------------------------------------------------------------------------------

# (make-themespace-window <layout>
#    [<top-window-pixel-size> <bottom-window-pixel-size> <vertical-window-pixel-size>])
# Examples:
# (make-themespace-window *themespace-window-layout*)
# (make-themespace-window *themespace-window-layout* '(785 130) '(785 130) '(150 595))


def make_themespace_window(*args):
    """theme-graphics.ss: make-themespace-window"""
    window = new_themespace_window(*args)
    tell(window, "initialize")
    return window


def new_themespace_window(layout, *optional_args):
    """theme-graphics.ss: new-themespace-window"""
    from metacat.gui import constants as K
    top_window = (make_bridge_themes_window(layout[0]) if len(optional_args) == 0
                  else make_bridge_themes_window(layout[0], optional_args[0]))
    bottom_window = (make_bridge_themes_window(second(layout)) if len(optional_args) == 0
                     else make_bridge_themes_window(second(layout), second(optional_args)))
    vertical_window = (make_bridge_themes_window(third(layout)) if len(optional_args) == 0
                       else make_bridge_themes_window(third(layout), third(optional_args)))
    all_windows = [top_window, bottom_window, vertical_window]
    set_theme_window_mouse_handlers(top_window, "top-bridge")
    set_theme_window_mouse_handlers(bottom_window, "bottom-bridge")
    set_theme_window_mouse_handlers(vertical_window, "vertical-bridge")
    tell(top_window, "set-icon-label", K.p_top_bridge_themes_icon_label)
    if exists_p(K.p_top_bridge_themes_icon_image):
        tell(top_window, "set-icon-image", K.p_top_bridge_themes_icon_image)
    if exists_p(K.p_top_bridge_themes_window_title):
        tell(top_window, "set-window-title", K.p_top_bridge_themes_window_title)
    tell(bottom_window, "set-icon-label", K.p_bottom_bridge_themes_icon_label)
    if exists_p(K.p_bottom_bridge_themes_icon_image):
        tell(bottom_window, "set-icon-image", K.p_bottom_bridge_themes_icon_image)
    if exists_p(K.p_bottom_bridge_themes_window_title):
        tell(bottom_window, "set-window-title", K.p_bottom_bridge_themes_window_title)
    tell(vertical_window, "set-icon-label", K.p_vertical_bridge_themes_icon_label)
    if exists_p(K.p_vertical_bridge_themes_icon_image):
        tell(vertical_window, "set-icon-image", K.p_vertical_bridge_themes_icon_image)
    if exists_p(K.p_vertical_bridge_themes_window_title):
        tell(vertical_window, "set-window-title", K.p_vertical_bridge_themes_window_title)
    return ThemespaceWindow(top_window, bottom_window, vertical_window, all_windows)


class ThemespaceWindow(SchemeObject):
    """theme-graphics.ss: new-themespace-window's closure"""
    __slots__ = ("top_window", "bottom_window", "vertical_window", "all_windows")

    def __init__(this, top_window, bottom_window, vertical_window, all_windows):
        this.top_window = top_window
        this.bottom_window = bottom_window
        this.vertical_window = vertical_window
        this.all_windows = all_windows

    @message("object-type")
    def object_type(this, self):
        return "themespace-window"

    @message("get-window")
    def get_window(this, self, theme_type):
        if theme_type == "top-bridge":
            return this.top_window
        if theme_type == "bottom-bridge":
            return this.bottom_window
        if theme_type == "vertical-bridge":
            return this.vertical_window
        return None                     # a case without else

    @message("set-theme-graphics-parameters")
    def set_theme_graphics_parameters(this, self, theme):
        window = tell(self, "get-window", tell(theme, "get-theme-type"))
        return tell(window, "set-theme-graphics-parameters", theme)

    @message("set-theme-graphics-parameters-and-draw")
    def set_theme_graphics_parameters_and_draw(this, self, theme):
        window = tell(self, "get-window", tell(theme, "get-theme-type"))
        return tell(window, "set-theme-graphics-parameters-and-draw", theme)

    @message("erase-theme")
    def erase_theme(this, self, theme):
        window = tell(self, "get-window", tell(theme, "get-theme-type"))
        return tell(window, "erase-theme", theme)

    @message("erase-all-themes")
    def erase_all_themes(this, self, *theme_types):
        return tell(self, "apply-method", "erase-all-themes", list(theme_types))

    @message("update-graphics")
    def update_graphics(this, self, *theme_types):
        return tell(self, "apply-method", "update-graphics", list(theme_types))

    @message("initialize")
    def initialize(this, self, *theme_types):
        return tell(self, "apply-method", "initialize", list(theme_types))

    @message("redraw")
    def redraw(this, self, *theme_types):
        return tell(self, "apply-method", "redraw", list(theme_types))

    @message("garbage-collect")
    def garbage_collect(this, self):
        for window in this.all_windows:
            tell(window, "garbage-collect")
        return "done"

    @message("update-thematic-pressure")
    def update_thematic_pressure(this, self, *theme_types):
        return tell(self, "apply-method", "update-thematic-pressure", list(theme_types))

    @message("apply-method")
    def apply_method(this, self, method_name, theme_types):
        if len(theme_types) == 0:
            for window in this.all_windows:
                tell(window, method_name)
        else:
            for theme_type in theme_types:
                tell(tell(self, "get-window", theme_type), method_name)
        return "done"

    @message("raise-window")
    def raise_window(this, self):
        tell(this.top_window, "raise-window")
        tell(this.vertical_window, "raise-window")
        if setup.p_justify_mode is not False:
            return tell(this.bottom_window, "raise-window")
        return None

    def otherwise(this, self, msg, args):
        return delegate_to_all(self, msg, args,
                               this.top_window, this.vertical_window, this.bottom_window)


g_panel_order = False           # made by load()
g_panel_theme_order = False     # made by load()


def _theme_font(orientation, win_height, style):
    """the body shared by the four select-theme-...-font procedures"""
    from metacat.gui import fonts
    if orientation == "horizontal":
        desired_font_height = round_(mul(F(15, 140), win_height))
        return fonts.make_mfont(fonts.serif, sub(desired_font_height), style)
    if orientation == "vertical":
        desired_font_height = round_(mul(F(23, 1000), win_height))
        return fonts.make_mfont(fonts.serif, sub(desired_font_height), style)
    return None


def select_theme_dimension_normal_font(orientation, win_width, win_height):
    """theme-graphics.ss: select-theme-dimension-normal-font"""
    return _theme_font(orientation, win_height, ["italic"])


def select_theme_dimension_highlight_font(orientation, win_width, win_height):
    """theme-graphics.ss: select-theme-dimension-highlight-font"""
    return _theme_font(orientation, win_height, ["bold", "italic"])


def select_theme_relation_normal_font(orientation, win_width, win_height):
    """theme-graphics.ss: select-theme-relation-normal-font"""
    return _theme_font(orientation, win_height, ["italic"])


def select_theme_relation_highlight_font(orientation, win_width, win_height):
    """theme-graphics.ss: select-theme-relation-highlight-font"""
    return _theme_font(orientation, win_height, ["bold", "italic"])


def resize_theme_fonts(dim_normal_font, dim_highlight_font, rel_normal_font,
                       rel_highlight_font, orientation, win_width, win_height):
    """theme-graphics.ss: resize-theme-fonts"""
    if orientation == "horizontal":
        tell(dim_normal_font, "resize", round_(mul(F(15, 140), win_height)))
        tell(dim_highlight_font, "resize", round_(mul(F(15, 140), win_height)))
        tell(rel_normal_font, "resize", round_(mul(F(15, 140), win_height)))
        return tell(rel_highlight_font, "resize", round_(mul(F(15, 140), win_height)))
    if orientation == "vertical":
        tell(dim_normal_font, "resize", round_(mul(F(23, 1000), win_height)))
        tell(dim_highlight_font, "resize", round_(mul(F(23, 1000), win_height)))
        tell(rel_normal_font, "resize", round_(mul(F(23, 1000), win_height)))
        return tell(rel_highlight_font, "resize", round_(mul(F(23, 1000), win_height)))
    return None


def select_bridge_themes_window_pixel_size(layout):
    """theme-graphics.ss: select-bridge-themes-window-pixel-size"""
    from metacat.gui import constants as K
    kind = layout[0]
    if kind == "top-bridge":
        return K.p_top_theme_window_size
    if kind == "bottom-bridge":
        return K.p_bottom_theme_window_size
    if kind == "vertical-bridge":
        return K.p_vertical_theme_window_size
    if kind == "fake":
        return K.p_top_theme_window_size
    return None


def make_bridge_themes_window(layout, *optional_args):
    """theme-graphics.ss: make-bridge-themes-window (the let*, in order)"""
    from metacat.gui import constants as K, general_graphics as gg
    pixels = (select_bridge_themes_window_pixel_size(layout) if len(optional_args) == 0
              else optional_args[0])
    x_pixels = pixels[0]
    y_pixels = second(pixels)
    theme_type = layout[0]
    bg_color__pressure_on = K.p_theme_background_color__thematic_pressure_on
    bg_color__pressure_off = K.p_theme_background_color__thematic_pressure_off
    graphics_window = gg.make_unscrollable_graphics_window(
        x_pixels, y_pixels, K.p_theme_background_color__thematic_pressure_off)
    dimensions = sort_wrt_order(
        chez.remq(slipnet.plato_bond_category, tell(themes.g_themespace, "get-dimensions")),
        g_panel_order)
    panel_relations = chez.map_(
        lambda dimension: sort_wrt_order(
            tell(themes.g_themespace, "get-relations", theme_type, dimension),
            g_panel_theme_order),
        dimensions)
    num_panels = len(dimensions)
    max_panel_relations = chez.max_(*[len(rs) for rs in panel_relations])
    row_major_p = chez.number_p(third(layout))
    rows = ceiling(div(num_panels, third(layout))) if row_major_p else second(layout)
    cols = third(layout) if row_major_p else ceiling(div(num_panels, second(layout)))
    panel_orientation = fourth(layout)
    panel_x_length = div(1, cols)
    panel_y_length = div(y_pixels, mul(x_pixels, rows))
    panel_x_pixels = div(x_pixels, cols)            # (unused, as in the original)
    panel_y_pixels = div(y_pixels, rows)            # (unused, as in the original)
    dim_normal_font = select_theme_dimension_normal_font(panel_orientation, x_pixels, y_pixels)
    dim_highlight_font = select_theme_dimension_highlight_font(panel_orientation, x_pixels,
                                                               y_pixels)
    rel_normal_font = select_theme_relation_normal_font(panel_orientation, x_pixels, y_pixels)
    rel_highlight_font = select_theme_relation_highlight_font(panel_orientation, x_pixels,
                                                              y_pixels)
    dim_name_height = tell(graphics_window, "get-character-height", String(" "),
                           dim_highlight_font)
    dim_name_offset = tell(graphics_window, "get-text-offset", dim_highlight_font)
    rel_name_height = tell(graphics_window, "get-character-height", String(" "),
                           rel_highlight_font)
    rel_name_offset = tell(graphics_window, "get-text-offset", rel_highlight_font)

    def get_rel_name_width(relation):
        return tell(graphics_window, "get-string-width",
                    engine_theme_graphics.relation_name(relation), rel_highlight_font)
    all_relations = [r for rs in panel_relations for r in rs]
    max_rel_name_width = chez.max_(*chez.map_(get_rel_name_width, all_relations))
    if panel_orientation == "horizontal":
        top_margin = mul(F(1, 4), rel_name_height)
    elif panel_orientation == "vertical":
        top_margin = 0
    else:
        top_margin = None
    bottom_margin = mul(F(1, 4), dim_name_height)
    if panel_orientation == "horizontal":
        get_dim_name = dimension_name
    elif panel_orientation == "vertical":
        get_dim_name = abbreviated_dimension_name
    else:
        get_dim_name = None
    dim_name_coord = [mul(F(1, 2), panel_x_length), add(bottom_margin, dim_name_offset)]
    if panel_orientation == "horizontal":
        max_activation_diameter = chez.min_(
            div(panel_x_length, max_panel_relations),
            sub(panel_y_length, dim_name_height, rel_name_height, top_margin, bottom_margin))
    elif panel_orientation == "vertical":
        max_activation_diameter = sub(
            chez.min_(sub(panel_x_length, max_rel_name_width),
                      div(sub(panel_y_length, dim_name_height, top_margin, bottom_margin),
                          max_panel_relations)),
            # Fudge to avoid corrupting panel outline:
            mul(3, tell(graphics_window, "get-width-per-pixel")))
    else:
        max_activation_diameter = None
    full_activation_diameter = mul(F(9, 10), max_activation_diameter)
    if row_major_p:
        def compute_lower_left(n):
            return [mul(panel_x_length, chez.remainder(n, cols)),
                    mul(panel_y_length, sub(rows, chez.quotient(n, cols), 1))]
    else:
        def compute_lower_left(n):
            return [mul(panel_x_length, chez.quotient(n, rows)),
                    mul(panel_y_length, sub(rows, chez.remainder(n, rows), 1))]
    if panel_orientation == "horizontal":
        compute_panel_info = compute_horizontal_panel_info(
            panel_x_length, panel_y_length, full_activation_diameter,
            top_margin, bottom_margin, dim_name_height, rel_name_height, rel_name_offset)
    elif panel_orientation == "vertical":
        compute_panel_info = compute_vertical_panel_info(
            panel_x_length, panel_y_length, full_activation_diameter,
            top_margin, bottom_margin, dim_name_height, rel_name_height,
            rel_name_offset, get_rel_name_width)
    else:
        compute_panel_info = None

    def make_one_panel(i, dimension, relations):
        lower_left = compute_lower_left(i)
        panel_info = compute_panel_info(relations, lower_left)
        upper_right = _map_add(lower_left, [panel_x_length, panel_y_length])
        lower_right = [upper_right[0], second(lower_left)]
        upper_left = [lower_left[0], second(upper_right)]
        dim_name = get_dim_name(dimension)
        coord = _map_add(lower_left, dim_name_coord)
        return make_panel(
            graphics_window, dimension, theme_type, panel_info,
            dim_name, coord, lower_left, upper_right, lower_right, upper_left,
            full_activation_diameter, max_activation_diameter,
            dim_normal_font, dim_highlight_font,
            rel_normal_font, rel_highlight_font)
    panels = chez.map_(make_one_panel, ascending_index_list(num_panels), dimensions,
                       panel_relations)
    return BridgeThemesWindow(
        x_pixels, y_pixels, theme_type, bg_color__pressure_on, bg_color__pressure_off,
        graphics_window, panel_orientation, dim_normal_font, dim_highlight_font,
        rel_normal_font, rel_highlight_font, panels)


class BridgeThemesWindow(SchemeObject):
    """theme-graphics.ss: make-bridge-themes-window's closure (the variables its
    methods use)"""
    __slots__ = ("x_pixels", "y_pixels", "theme_type", "bg_color__pressure_on",
                 "bg_color__pressure_off", "graphics_window", "panel_orientation",
                 "dim_normal_font", "dim_highlight_font", "rel_normal_font",
                 "rel_highlight_font", "panels", "thematic_pressure_p")

    def __init__(this, x_pixels, y_pixels, theme_type, bg_color__pressure_on,
                 bg_color__pressure_off, graphics_window, panel_orientation,
                 dim_normal_font, dim_highlight_font, rel_normal_font, rel_highlight_font,
                 panels):
        this.x_pixels = x_pixels
        this.y_pixels = y_pixels
        this.theme_type = theme_type
        this.bg_color__pressure_on = bg_color__pressure_on
        this.bg_color__pressure_off = bg_color__pressure_off
        this.graphics_window = graphics_window
        this.panel_orientation = panel_orientation
        this.dim_normal_font = dim_normal_font
        this.dim_highlight_font = dim_highlight_font
        this.rel_normal_font = rel_normal_font
        this.rel_highlight_font = rel_highlight_font
        this.panels = panels
        this.thematic_pressure_p = False

    @message("object-type")
    def object_type(this, self):
        return "bridge-themes-window"

    @message("get-all-panels")
    def get_all_panels(this, self):
        return this.panels

    @message("show-fonts")
    def show_fonts(this, self):
        chez.printf("---------------------------------------~n")
        chez.printf("~a themespace window~n", this.theme_type)
        chez.printf("Dimension-normal font:~n")
        tell(this.dim_normal_font, "print")
        chez.printf("Dimension-highlight font:~n")
        tell(this.dim_highlight_font, "print")
        chez.printf("Relation-normal font:~n")
        tell(this.rel_normal_font, "print")
        chez.printf("Relation-highlight font:~n")
        tell(this.rel_highlight_font, "print")
        return "done"

    @message("display-edit-mode-message")
    def display_edit_mode_message(this, self):
        if tell(setup.g_control_panel, "ready-to-edit?", this.theme_type) is not False:
            center = tell(this.graphics_window, "get-center-coord")
            return tell(this.graphics_window, "draw",
                        ["let-sgl", [["font", this.dim_normal_font],
                                     ["text-justification", "center"]],
                         ["text", center, String("Click to select themes")]])
        return None

    @message("get-background-colors")
    def get_background_colors(this, self):
        return [this.bg_color__pressure_on, this.bg_color__pressure_off]

    @message("set-background-colors")
    def set_background_colors(this, self, color1, color2):
        this.bg_color__pressure_on = color1
        this.bg_color__pressure_off = color2
        for panel in this.panels:
            tell(panel, "set-background-colors", color1, color2)
        return tell(this.graphics_window, "set-background-color", color2)

    # Some themes in themespace may not appear in the
    # themespace-window, such as bond-category themes.
    # Such themes do not have associated panels:
    @message("set-theme-graphics-parameters")
    def set_theme_graphics_parameters(this, self, theme):
        panel = select_meth(this.panels, "dimension?", tell(theme, "get-dimension"))
        if exists_p(panel):
            tell(panel, "set-theme-graphics-parameters", theme)
            tell(panel, "add-relation", tell(theme, "get-relation"))
        return "done"

    @message("set-theme-graphics-parameters-and-draw")
    def set_theme_graphics_parameters_and_draw(this, self, theme):
        panel = select_meth(this.panels, "dimension?", tell(theme, "get-dimension"))
        if exists_p(panel):
            tell(panel, "set-theme-graphics-parameters", theme)
            tell(panel, "add-relation", tell(theme, "get-relation"))
            tell(this.graphics_window, "caching-on")
            tell(panel, "draw-panel")
            tell(this.graphics_window, "flush")
        return "done"

    @message("update-thematic-pressure")
    def update_thematic_pressure(this, self):
        from metacat.gui import constants as K
        if not chez.eq_p(this.thematic_pressure_p,
                         tell(themes.g_themespace, "thematic-pressure?", this.theme_type)):
            this.thematic_pressure_p = not (this.thematic_pressure_p is not False)
            highlight_color = (K.p_panel_highlight_color__thematic_pressure_on
                               if this.thematic_pressure_p is not False
                               else K.p_panel_highlight_color__thematic_pressure_off)
            background_color = (this.bg_color__pressure_on
                                if this.thematic_pressure_p is not False
                                else this.bg_color__pressure_off)
            tell(this.graphics_window, "set-background-color", background_color)
            for panel in this.panels:
                tell(panel, "set-highlight-color", highlight_color)
            tell(self, "redraw")
        return "done"

    @message("erase-theme")
    def erase_theme(this, self, theme):
        panel = tell(theme, "get-graphics-panel")
        if exists_p(panel):
            tell(panel, "remove-relation", tell(theme, "get-relation"))
            tell(this.graphics_window, "caching-on")
            tell(panel, "draw-panel")
            tell(this.graphics_window, "flush")
        return "done"

    @message("erase-all-themes")
    def erase_all_themes(this, self):
        for panel in this.panels:
            tell(panel, "delete-all-relations")
        tell(self, "redraw")
        return "done"

    @message("update-graphics")
    def update_graphics(this, self):
        tell(this.graphics_window, "caching-on")
        for panel in this.panels:
            tell(panel, "update-graphics")
        return tell(this.graphics_window, "flush")

    @message("initialize")
    def initialize(this, self):
        tell(this.graphics_window, "caching-on")
        tell(this.graphics_window, "clear")
        for panel in this.panels:
            tell(panel, "initialize")
        tell(self, "update-thematic-pressure")
        return tell(this.graphics_window, "flush")

    @message("garbage-collect")
    def garbage_collect(this, self):
        tell(this.graphics_window, "retag", "all", "garbage")
        for panel in this.panels:
            tell(panel, "redraw-panel")
        tell(this.graphics_window, "unhide", "highlight")
        return tell(this.graphics_window, "delete", "garbage")

    @message("resize")
    def resize(this, self, new_width, new_height):
        this.x_pixels = new_width
        this.y_pixels = new_height
        resize_theme_fonts(this.dim_normal_font, this.dim_highlight_font,
                           this.rel_normal_font, this.rel_highlight_font,
                           this.panel_orientation, this.x_pixels, this.y_pixels)
        tell(this.graphics_window, "retag", "all", "garbage")
        for panel in this.panels:
            tell(panel, "redraw-panel")
        tell(this.graphics_window, "unhide", "highlight")
        tell(this.graphics_window, "delete", "garbage")
        if g_theme_edit_mode_p is not False:
            tell(self, "display-edit-mode-message")
        return "done"

    @message("redraw")
    def redraw(this, self):
        tell(this.graphics_window, "caching-on")
        # Ensure that edges around highlighted panels get
        # reset to the window's original background color:
        tell(this.graphics_window, "clear")
        for panel in this.panels:
            tell(panel, "draw-panel")
        return tell(this.graphics_window, "flush")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


# panel-info ::= (<relation-info> ...)
# <relation-info> ::= (<relation> <center-coord> <name-coord> <name>)
# Example:  (<plato-successor> (0.25 0.5) (0.3 0.7) "succ")

def make_panel(graphics_window, dimension, theme_type, panel_info, dim_name,
               dim_name_coord, lower_left, upper_right, lower_right, upper_left,
               full_activation_diameter, max_activation_diameter,
               dim_normal_font, dim_highlight_font, rel_normal_font, rel_highlight_font):
    """theme-graphics.ss: make-panel"""
    return Panel(graphics_window, dimension, theme_type, panel_info, dim_name,
                 dim_name_coord, lower_left, upper_right, lower_right, upper_left,
                 full_activation_diameter, max_activation_diameter,
                 dim_normal_font, dim_highlight_font, rel_normal_font, rel_highlight_font)


class Panel(SchemeObject):
    """theme-graphics.ss: make-panel's closure"""
    __slots__ = ("graphics_window", "dimension", "theme_type", "panel_info", "dim_name",
                 "dim_name_coord", "lower_left", "upper_right", "lower_right", "upper_left",
                 "full_activation_diameter", "max_activation_diameter",
                 "dim_normal_font", "dim_highlight_font", "rel_normal_font",
                 "rel_highlight_font", "delta", "ll", "ul", "ur", "lr", "outline_pexp",
                 "relations", "relation_names_pexps", "normal_panel_pexp",
                 "highlighted_panel_pexp", "theme_cluster", "highlight_color",
                 "highlighted_theme", "bg_color__pressure_on", "bg_color__pressure_off")

    def __init__(this, graphics_window, dimension, theme_type, panel_info, dim_name,
                 dim_name_coord, lower_left, upper_right, lower_right, upper_left,
                 full_activation_diameter, max_activation_diameter,
                 dim_normal_font, dim_highlight_font, rel_normal_font, rel_highlight_font):
        from metacat.gui import constants as K
        this.graphics_window = graphics_window
        this.dimension = dimension
        this.theme_type = theme_type
        this.panel_info = panel_info
        this.dim_name = dim_name
        this.dim_name_coord = dim_name_coord
        this.lower_left = lower_left
        this.upper_right = upper_right
        this.lower_right = lower_right
        this.upper_left = upper_left
        this.full_activation_diameter = full_activation_diameter
        this.max_activation_diameter = max_activation_diameter
        this.dim_normal_font = dim_normal_font
        this.dim_highlight_font = dim_highlight_font
        this.rel_normal_font = rel_normal_font
        this.rel_highlight_font = rel_highlight_font
        # the let*, in order
        delta = mul(2, tell(graphics_window, "get-width-per-pixel"))
        this.delta = delta
        this.ll = [add(lower_left[0], delta), add(second(lower_left), delta)]
        this.ul = [add(upper_left[0], delta), sub(second(upper_left), delta)]
        this.ur = [sub(upper_right[0], delta), sub(second(upper_right), delta)]
        this.lr = [sub(lower_right[0], delta), add(second(lower_right), delta)]
        this.outline_pexp = ["polyline", this.ll, this.lr, this.ur, this.ul, this.ll]
        this.relations = []
        this.relation_names_pexps = []
        this.normal_panel_pexp = []
        this.highlighted_panel_pexp = []
        this.theme_cluster = False
        this.highlight_color = K.p_panel_highlight_color__thematic_pressure_off
        this.highlighted_theme = False
        this.bg_color__pressure_on = K.p_theme_background_color__thematic_pressure_on
        this.bg_color__pressure_off = K.p_theme_background_color__thematic_pressure_off

    @message("object-type")
    def object_type(this, self):
        return "theme-panel"

    @message("get-relations")
    def get_relations(this, self):
        return this.relations

    @message("get-relation-names-pexp")
    def get_relation_names_pexp(this, self):
        # 1.2: relation-names-pexp is never defined (the variable is
        # relation-names-pexps); Chez raises when this runs, and nothing sends
        # the message (anomalies: "relation-names-pexp is never defined")
        raise chez.UnboundVariable("relation-names-pexp")

    @message("get-normal-panel-pexp")
    def get_normal_panel_pexp(this, self):
        return this.normal_panel_pexp

    @message("get-highlighted-panel-pexp")
    def get_highlighted_panel_pexp(this, self):
        return this.highlighted_panel_pexp

    @message("get-graphics-window")
    def get_graphics_window(this, self):
        return this.graphics_window

    @message("get-highlighted-theme")
    def get_highlighted_theme(this, self):
        return this.highlighted_theme

    @message("get-cluster")
    def get_cluster(this, self):
        return this.theme_cluster

    @message("get-activation-diameter")
    def get_activation_diameter(this, self):
        return this.full_activation_diameter

    @message("set-background-colors")
    def set_background_colors(this, self, color1, color2):
        this.bg_color__pressure_on = color1
        this.bg_color__pressure_off = color2
        return "done"

    @message("set-highlight-color")
    def set_highlight_color(this, self, color):
        this.highlight_color = color
        return "done"

    @message("dimension?")
    def dimension_p(this, self, dim):
        return chez.eq_p(dim, this.dimension)

    @message("add-relation")
    def add_relation(this, self, relation):
        this.relations = [relation] + this.relations
        return tell(self, "update-relation-names")

    @message("remove-relation")
    def remove_relation(this, self, relation):
        this.relations = chez.remq(relation, this.relations)
        return tell(self, "update-relation-names")

    @message("delete-all-relations")
    def delete_all_relations(this, self):
        this.relations = []
        return tell(self, "update-relation-names")

    @message("update-relation-names")
    def update_relation_names(this, self):
        if len(this.relations) == 0:
            this.relation_names_pexps = []
            this.normal_panel_pexp = []
            this.highlighted_panel_pexp = []
        else:
            def name_pexp(r):
                info = chez.assq(r, this.panel_info)
                if exists_p(info):
                    return ["text", third(info), fourth(info)]
                return False
            this.relation_names_pexps = map_compress(name_pexp, this.relations)
            this.normal_panel_pexp = [
                "let-sgl", [["text-justification", "center"]],
                this.outline_pexp,
                ["let-sgl", [["font", this.rel_normal_font]], *this.relation_names_pexps],
                ["let-sgl", [["font", this.dim_normal_font]],
                 ["text", this.dim_name_coord, this.dim_name]]]
            this.highlighted_panel_pexp = [
                "let-sgl", [["text-justification", "center"]],
                this.outline_pexp,
                ["let-sgl", [["font", this.rel_normal_font]], *this.relation_names_pexps],
                ["let-sgl", [["font", this.dim_highlight_font]],
                 ["text", this.dim_name_coord, this.dim_name]]]
        return "done"

    @message("set-theme-graphics-parameters")
    def set_theme_graphics_parameters(this, self, theme):
        relation = tell(theme, "get-relation")
        info = chez.assq(relation, this.panel_info)
        center_coord = second(info)
        text_coord = third(info)
        normal_pexp = ["let-sgl", [["font", this.rel_normal_font],
                                   ["text-justification", "center"]],
                       ["text", text_coord, fourth(info)]]
        highlight_pexp = ["let-sgl", [["font", this.rel_highlight_font],
                                      ["text-justification", "center"]],
                          ["text", text_coord, fourth(info)]]
        return tell(theme, "set-graphics-parameters",
                    self, center_coord, text_coord, normal_pexp, highlight_pexp)

    @message("draw-panel")
    def draw_panel(this, self):
        g = this.graphics_window
        dominant_theme = tell(this.theme_cluster, "get-dominant-theme")
        if exists_p(dominant_theme):
            tell(g, "erase-on-background", this.highlight_color,
                 ["filled-rectangle", this.ll, this.ur])
            tell(g, "draw", this.highlighted_panel_pexp)
        else:
            tell(g, "erase", ["filled-rectangle", this.lower_left, this.upper_right])
            tell(g, "draw", this.normal_panel_pexp)
        for theme in tell(this.theme_cluster, "get-themes"):
            if chez.eq_p(theme, dominant_theme):
                tell(g, "erase-on-background", this.highlight_color,
                     tell(theme, "get-normal-pexp"))
                tell(g, "draw", tell(theme, "get-highlight-pexp"))
            tell(theme, "draw-activation-graphics")
        this.highlighted_theme = dominant_theme
        return "done"

    # this method is used only for garbage collection
    @message("redraw-panel")
    def redraw_panel(this, self):
        g = this.graphics_window
        if not len(tell(this.theme_cluster, "get-themes")) == 0:
            dominant_theme = tell(this.theme_cluster, "get-dominant-theme")
            vp = tell(g, "get-vp")
            if exists_p(dominant_theme):
                # SWL's (send vp draw-hidden-filled-rectangle ...)
                vp.draw_hidden_filled_rectangle(this.highlight_color,
                                                this.ll[0], second(this.ll),
                                                this.ur[0], second(this.ur), "highlight")
            tell(g, "draw", this.outline_pexp)
            tell(g, "draw",
                 ["let-sgl", [["text-justification", "center"],
                              ["font", (this.dim_highlight_font if exists_p(dominant_theme)
                                        else this.dim_normal_font)]],
                  ["text", this.dim_name_coord, this.dim_name]])
            for theme in tell(this.theme_cluster, "get-themes"):
                if chez.eq_p(theme, dominant_theme):
                    tell(g, "draw", tell(theme, "get-highlight-pexp"))
                else:
                    tell(g, "draw", tell(theme, "get-normal-pexp"))
                tell(theme, "draw-activation-graphics")
        return "done"

    @message("update-graphics")
    def update_graphics(this, self):
        g = this.graphics_window
        themes_ = tell(this.theme_cluster, "get-themes")
        dominant_theme = tell(this.theme_cluster, "get-dominant-theme")
        if chez.eq_p(dominant_theme, this.highlighted_theme):
            for theme in themes_:
                tell(theme, "update-activation-graphics")
        elif exists_p(this.highlighted_theme) and exists_p(dominant_theme):
            tell(g, "erase-on-background", this.highlight_color,
                 tell(this.highlighted_theme, "get-highlight-pexp"))
            tell(g, "draw", tell(this.highlighted_theme, "get-normal-pexp"))
            tell(g, "erase-on-background", this.highlight_color,
                 tell(dominant_theme, "get-normal-pexp"))
            tell(g, "draw", tell(dominant_theme, "get-highlight-pexp"))
            for theme in themes_:
                tell(theme, "update-activation-graphics")
            this.highlighted_theme = dominant_theme
        else:
            tell(self, "draw-panel")
        return "done"

    @message("draw-absolute-activation")
    def draw_absolute_activation(this, self, coord, activation):
        from metacat import general_graphics as GG
        from metacat.gui import constants as K
        view_globals.g_fg_color = (K.p_negative_theme_activation_color
                                   if chez.negative_p(activation)
                                   else K.p_positive_theme_activation_color)
        tell(this.graphics_window, "draw",
             GG.disk(coord, mul(percent(chez.abs_(activation)), this.full_activation_diameter)))
        view_globals.g_fg_color = view_globals.p_default_fg_color
        return "done"

    @message("decrease-absolute-activation")
    def decrease_absolute_activation(this, self, coord, activation):
        from metacat.gui import constants as K
        diameter = mul(percent(chez.abs_(activation)), this.full_activation_diameter)
        # not sure if this is really necessary, but just to be safe...
        activation_color = (K.p_negative_theme_activation_color
                            if chez.negative_p(activation)
                            else K.p_positive_theme_activation_color)
        if exists_p(this.highlighted_theme):
            return tell(this.graphics_window, "draw",
                        ["let-sgl", [["foreground-color", this.highlight_color],
                                     ["background-color", activation_color]],
                         ["ring", coord, this.max_activation_diameter, diameter]])
        return tell(this.graphics_window, "draw",
                    ["let-sgl", [["foreground-color",
                                  # again, just to be safe...
                                  (this.bg_color__pressure_on
                                   if tell(themes.g_themespace, "thematic-pressure?",
                                           this.theme_type) is not False
                                   else this.bg_color__pressure_off)],
                                 ["background-color", activation_color]],
                     ["ring", coord, this.max_activation_diameter, diameter]])

    @message("erase-activation")
    def erase_activation(this, self, coord):
        from metacat import general_graphics as GG
        if exists_p(this.highlighted_theme):
            return tell(this.graphics_window, "erase-on-background", this.highlight_color,
                        GG.disk(coord, this.max_activation_diameter))
        return tell(this.graphics_window, "erase", GG.disk(coord, this.max_activation_diameter))

    @message("initialize")
    def initialize(this, self):
        this.theme_cluster = tell(themes.g_themespace, "get-cluster", this.theme_type,
                                  this.dimension)
        this.relations = tell_all(tell(this.theme_cluster, "get-themes"), "get-relation")
        tell(self, "update-relation-names")
        return tell(self, "draw-panel")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, base_object)


def compute_horizontal_panel_info(x_length, y_length, full_activation_diameter, top_margin,
                                  bottom_margin, dim_name_height, rel_name_height,
                                  rel_name_offset):
    """theme-graphics.ss: compute-horizontal-panel-info"""
    def panel_info(relations, lower_left):
        num_relations = len(relations)
        required_x_space = mul(num_relations, full_activation_diameter)
        x_spacing = div(sub(x_length, required_x_space), chez.add1(num_relations))
        required_y_space = add(rel_name_height, full_activation_diameter, dim_name_height,
                               top_margin, bottom_margin)
        y_spacing = chez.min_(mul(F(1, 4), full_activation_diameter),
                              mul(F(1, 3), sub(y_length, required_y_space)))
        y_margin = add(top_margin,
                       mul(F(1, 2), sub(y_length, required_y_space, y_spacing)))
        names = chez.map_(engine_theme_graphics.relation_name, relations)
        name_y = add(sub(y_length, y_margin, rel_name_height), rel_name_offset)
        center_y = sub(y_length, y_margin, rel_name_height, y_spacing,
                       mul(F(1, 2), full_activation_diameter))

        def entry(i, relation, name):
            x = add(mul(chez.add1(i), x_spacing),
                    mul(add(i, F(1, 2)), full_activation_diameter))
            center_coord = _map_add(lower_left, [x, center_y])
            name_coord = _map_add(lower_left, [x, name_y])
            return [relation, center_coord, name_coord, name]
        return chez.map_(entry, ascending_index_list(num_relations), relations, names)
    return panel_info


def compute_vertical_panel_info(x_length, y_length, full_activation_diameter, top_margin,
                                bottom_margin, dim_name_height, rel_name_height,
                                rel_name_offset, get_rel_name_width):
    """theme-graphics.ss: compute-vertical-panel-info"""
    def panel_info(relations, lower_left):
        num_relations = len(relations)
        max_name_width = chez.max_(*chez.map_(get_rel_name_width, relations))
        required_x_space = add(max_name_width, full_activation_diameter)
        x_spacing = chez.min_(mul(F(1, 2), full_activation_diameter),
                              mul(F(1, 3), sub(x_length, required_x_space)))
        left_margin = mul(F(1, 2), sub(x_length, required_x_space, x_spacing))
        required_y_space = add(mul(num_relations, full_activation_diameter),
                               dim_name_height, bottom_margin)
        y_spacing = div(sub(y_length, required_y_space), chez.add1(num_relations))
        names = chez.map_(engine_theme_graphics.relation_name, relations)
        name_x = add(left_margin, mul(F(1, 2), max_name_width))
        center_x = add(left_margin, max_name_width, x_spacing,
                       mul(F(1, 2), full_activation_diameter))

        def entry(i, relation, name):
            y = sub(y_length, mul(chez.add1(i), y_spacing),
                    mul(add(i, F(1, 2)), full_activation_diameter))
            center_coord = _map_add(lower_left, [center_x, y])
            name_y = add(y, mul(F(-1, 2), rel_name_height), rel_name_offset)
            name_coord = _map_add(lower_left, [name_x, name_y])
            return [relation, center_coord, name_coord, name]
        return chez.map_(entry, ascending_index_list(num_relations), relations, names)
    return panel_info


# port: relation-name is in the engine (metacat/theme_graphics.py), since
# trace.ss's print-pattern names relations with it.


def dimension_name(dimension):
    """theme-graphics.ss: dimension-name"""
    if dimension is slipnet.plato_letter_category:
        return String("Letter Category")
    if dimension is slipnet.plato_alphabetic_position_category:
        return String("Alphabetic Position")
    if dimension is slipnet.plato_direction_category:
        return String("Direction")
    if dimension is slipnet.plato_object_category:
        return String("Object Type")
    if dimension is slipnet.plato_string_position_category:
        return String("String Position")
    if dimension is slipnet.plato_group_category:
        return String("Group Type")
    if dimension is slipnet.plato_bond_category:
        return String("Bond Type")
    if dimension is slipnet.plato_bond_facet:
        return String("Bond Facet")
    return tell(dimension, "get-short-name")


def abbreviated_dimension_name(dimension):
    """theme-graphics.ss: abbreviated-dimension-name"""
    if dimension is slipnet.plato_letter_category:
        return String("Letter Ctgy.")
    if dimension is slipnet.plato_alphabetic_position_category:
        return String("Alpha. Pos.")
    if dimension is slipnet.plato_string_position_category:
        return String("String Pos.")
    return dimension_name(dimension)


def load():
    """port: the top-level definitions that need the slipnet (*panel-order* and
    *panel-theme-order*), made when the views are loaded."""
    global g_panel_order, g_panel_theme_order
    g_panel_order = [
        slipnet.plato_letter_category, slipnet.plato_length,
        slipnet.plato_string_position_category, slipnet.plato_group_category,
        slipnet.plato_direction_category, slipnet.plato_alphabetic_position_category,
        slipnet.plato_object_category, slipnet.plato_bond_facet, slipnet.plato_bond_category]
    g_panel_theme_order = [slipnet.plato_identity, slipnet.plato_successor,
                           slipnet.plato_predecessor, slipnet.plato_opposite, False]
