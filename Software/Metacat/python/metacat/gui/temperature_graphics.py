"""temperature-graphics.ss: the Temperature window and its thermometer.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from temperature-graphics.ss,
with racket/gui/temperature-graphics.rktl as a worked translation.

new-temperature-window's closure is `TemperatureWindow`, delegating to its
unscrollable graphics window (gui/general_graphics.py).  The two fonts are
module globals set by select-temperature-fonts, as in the original.  Arithmetic
is Chez's (chez.py): the coordinates are exact fractions.  This module does not
import tkinter.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez
from metacat.chez import add, div, mul, sub
from metacat.objects import SchemeObject, delegate, message, tell
from metacat.utilities import (ascending_index_list, ceiling, exists_p, first, percent,
                               round_, second)

String = chez.String
F = Fraction

p_temperature_value_font = False
p_temperature_title_font = False

# On Windows, the smallest possible x-dimension for an SWL window is about
# 100 pixels, due to the presence of the window manager buttons on the title
# bar, so for temperature windows smaller than this, a "fudge factor" is
# required in order to keep the thermometer centered in the window.
p_smallest_window_width = 100


def select_temperature_fonts(win_width, win_height):
    """temperature-graphics.ss: select-temperature-fonts"""
    global p_temperature_title_font, p_temperature_value_font
    from metacat.gui import fonts
    desired_title_height = round_(mul(F(6, 100), win_height))
    desired_value_height = round_(mul(F(6, 100), win_height))
    p_temperature_title_font = fonts.make_mfont(fonts.sans_serif, chez.sub(desired_title_height),
                                                ["normal"])
    p_temperature_value_font = fonts.make_mfont(fonts.serif, chez.sub(desired_value_height),
                                                ["italic"])


def make_temperature_window(*optional_args):
    """temperature-graphics.ss: make-temperature-window"""
    from metacat.gui import constants as K
    width = K.p_default_temperature_width if len(optional_args) == 0 else first(optional_args)
    window = new_temperature_window(width)
    tell(window, "initialize")
    return window


def new_temperature_window(x_pixels):
    """temperature-graphics.ss: new-temperature-window"""
    from metacat.gui import constants as K, general_graphics as gg
    y_pixels = ceiling(mul(F(5, 2), x_pixels))
    graphics_window = gg.make_unscrollable_graphics_window(
        x_pixels, y_pixels, K.p_temperature_background_color)
    window = TemperatureWindow(graphics_window, x_pixels, y_pixels)
    # 1.2: title is still #f here, so the icon label is #f (anomalies: "The
    # Temperature window's icon label is `#f`")
    tell(graphics_window, "set-icon-label", window.title)
    if exists_p(K.p_temperature_icon_image):
        tell(graphics_window, "set-icon-image", K.p_temperature_icon_image)
    if exists_p(K.p_temperature_window_title):
        tell(graphics_window, "set-window-title", K.p_temperature_window_title)
    return window


class TemperatureWindow(SchemeObject):
    """temperature-graphics.ss: new-temperature-window's closure"""

    def __init__(this, graphics_window, x_pixels, y_pixels):
        this.graphics_window = graphics_window
        this.x_pixels = x_pixels
        this.y_pixels = y_pixels
        this.x = False
        this.y = False
        this.bulb_center = False
        this.bulb_diameter = False
        this.left = False
        this.right = False
        this.text_x = False
        this.zero_level = False
        this.scale_length = False
        this.title_y = False
        this.pixel = False
        this.bar_left = False
        this.bar_right = False
        this.title = False
        this.current_value = 0

    @message("object-type")
    def object_type(this, self):
        return "temperature-window"

    @message("initialize-parameters")
    def initialize_parameters(this, self):
        from metacat.gui import sgl
        g = this.graphics_window
        this.x_pixels = tell(g, "get-visible-w")
        this.y_pixels = tell(g, "get-visible-h")
        if sgl.g_platform == "windows" and this.x_pixels < p_smallest_window_width:
            this.x = add(F(1, 2), div(sub(p_smallest_window_width, this.x_pixels),
                                      mul(2, this.x_pixels)))
        else:
            this.x = F(1, 2)
        this.y = F(3, 10)
        this.bulb_center = [this.x, this.y]
        this.bulb_diameter = F(3, 10)
        this.left = sub(this.x, F(1, 20))
        this.right = add(this.x, F(1, 20))
        this.text_x = add(this.x, F(3, 25))
        this.zero_level = F(1, 2)
        this.scale_length = F(29, 20)
        this.title_y = add(this.y, F(19, 10))
        this.pixel = tell(g, "get-width-per-pixel")
        this.bar_left = add(this.left, mul(2, this.pixel))
        this.bar_right = sub(this.right, mul(2, this.pixel))
        select_temperature_fonts(this.x_pixels, this.y_pixels)
        if this.x_pixels < tell(p_temperature_title_font, "get-pixel-width",
                                String("Temperature")):
            this.title = String("Temp.")
        else:
            this.title = String("Temperature")
        return "done"

    @message("draw-graphics")
    def draw_graphics(this, self):
        from metacat.gui import constants as K
        g = this.graphics_window
        tell(g, "draw",
             ["let-sgl", [["font", p_temperature_title_font],
                          ["text-justification", "center"]],
              ["text", [this.x, this.title_y], this.title]],
             "title")
        draw_thermometer(g, this.bulb_center, this.bulb_diameter)
        new_level = add(this.zero_level, mul(percent(this.current_value), this.scale_length))
        tell(g, "draw",
             mercury_pexp(this.bar_left, this.zero_level, this.bar_right, new_level,
                          K.p_thermometer_mercury_color),
             "bar")
        return tell(g, "draw",
                    ["let-sgl", [["font", p_temperature_value_font],
                                 ["origin", [this.text_x, new_level]]],
                     ["text", ["text-relative", [0, F(-2, 5)]],
                      String(chez.format_("~a", this.current_value))]],
                    "value")

    @message("update-graphics")
    def update_graphics(this, self, new_value):
        from metacat.gui import constants as K
        g = this.graphics_window
        if new_value != this.current_value:
            tell(g, "retag", "bar", "garbage")
            new_level = add(this.zero_level, mul(percent(new_value), this.scale_length))
            tell(g, "draw",
                 mercury_pexp(this.bar_left, this.zero_level, this.bar_right, new_level,
                              K.p_thermometer_mercury_color),
                 "bar")
            tell(g, "delete", "garbage")
            tell(g, "delete", "value")
            tell(g, "draw",
                 ["let-sgl", [["font", p_temperature_value_font],
                              ["origin", [this.text_x, new_level]]],
                  ["text", ["text-relative", [0, F(-2, 5)]],
                   String(chez.format_("~a", new_value))]],
                 "value")
            this.current_value = new_value
        return "done"

    @message("resize")
    def resize(this, self, new_width, new_height):
        g = this.graphics_window
        tell(self, "initialize-parameters")
        tell(g, "retag", "all", "garbage")
        tell(self, "draw-graphics")
        return tell(g, "delete", "garbage")

    @message("initialize")
    def initialize(this, self):
        tell(self, "initialize-parameters")
        this.current_value = 0
        tell(this.graphics_window, "clear")
        return tell(self, "draw-graphics")

    def otherwise(this, self, msg, args):
        return delegate(self, msg, args, this.graphics_window)


def mercury_pexp(left, from_level, right, to_level, color):
    """temperature-graphics.ss: mercury-pexp"""
    if left == right:  # Chez =: exact comparison, as Python's
        return ["let-sgl", [["foreground-color", color]],
                ["line", [left, from_level], [left, to_level]]]
    return ["let-sgl", [["foreground-color", color]],
            ["filled-rectangle", [left, from_level], [right, to_level]]]


def draw_thermometer(g, bulb_center, bulb_diameter):
    """temperature-graphics.ss: draw-thermometer"""
    from metacat.gui import colors, constants as K
    disk = _metacat.general_graphics.disk
    mercury = K.p_thermometer_mercury_color
    white = colors.c_white
    x = first(bulb_center)
    y = second(bulb_center)
    width = mul(F(1, 3), bulb_diameter)
    bottom = y
    top = add(y, mul(17, width))
    left = sub(x, mul(F(1, 2), width))
    right = add(x, mul(F(1, 2), width))
    pixel = tell(g, "get-width-per-pixel")
    bar_left = add(left, mul(2, pixel))
    bar_right = sub(right, mul(2, pixel))
    zero_level = add(y, mul(2, width))
    max_level = sub(top, mul(F(1, 2), width))
    gradation = mul(F(1, 10), sub(max_level, zero_level))
    mark_left = sub(x, mul(F(3, 2), width))
    bigmark_left = sub(x, mul(2, width))
    mark_right = sub(x, width)
    inner = sub(width, mul(2, pixel))
    tell(g, "draw",
         ["let-sgl", [],
          ["let-sgl", [["foreground-color", colors.c_grey]],
           ["line", [left, bottom], [left, top]],
           ["line", [right, bottom], [right, top]],
           ["let-sgl", [["foreground-color", white]],
            ["filled-arc", [x, top], [inner, inner], 0, 180]],
           ["arc", [x, top], [width, width], 0, 180]],
          ["let-sgl", [["foreground-color", mercury]],
           disk(bulb_center, bulb_diameter)],
          mercury_pexp(bar_left, bottom, bar_right, zero_level, mercury),
          mercury_pexp(bar_left, zero_level, bar_right, top, white),
          ["let-sgl", [["foreground-color", white],
                       ["background-color", mercury]],
           ["ring", bulb_center, mul(F(11, 16), bulb_diameter),
            mul(F(3, 8), bulb_diameter), 110, 63],
           # this gets rid of any residual white pixels at the bulb center
           ["let-sgl", [["foreground-color", mercury]],
            disk(bulb_center, mul(F(1, 8), bulb_diameter))]]])

    def mark(n):
        level = add(zero_level, mul(n, gradation))
        if chez.zero_p(chez.modulo(n, 5)):
            return tell(g, "draw", ["line", [bigmark_left, level], [mark_right, level]])
        return tell(g, "draw", ["line", [mark_left, level], [mark_right, level]])
    return chez.for_each(mark, ascending_index_list(11))


def load():
    """port: nothing to make at load time (the fonts are made with the window);
    here for views.load_views's uniform order."""
    return None
