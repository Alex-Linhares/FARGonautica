"""The graphics part of constants.ss: window sizes, colours, window titles.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from constants.ss, with
racket/gui/constants.rktl as a worked translation.

The model part of constants.ss (the probability distributions) is
metacat/constants.py; swl-color and *color-names* are gui/colors.py.  The
colours the model files read are engine globals (metacat/view_globals.py), #f
until the views are loaded: `load()` installs them there with
engine.set_global, as racket/gui/views.rkt's define does.  Verbatim otherwise.
This module does not import tkinter.
"""
from __future__ import annotations

from metacat import chez, utilities
from metacat.gui import sgl
from metacat.gui.colors import swl_color
from metacat.names import scheme_to_python

# Default window sizes
p_default_trace_width = False
p_default_trace_height = False
p_virtual_trace_length = False
p_default_coderack_width = False
p_default_13x5_slipnet_width = False
p_default_temperature_width = False
p_default_memory_width = False
p_default_memory_height = False
p_virtual_memory_length = False
p_default_comment_window_width = False
p_default_comment_window_height = False
p_virtual_comment_window_length = False
p_EEG_window_width = False
p_EEG_window_height = False
p_virtual_EEG_length = False
p_top_theme_window_size = False
p_bottom_theme_window_size = False
p_vertical_theme_window_size = False
p_default_workspace_width = False

# standard screen sizes: 800x600 1024x768 1152x864 1280x1024 1400x1050


def swl_screen_width():
    """port: SWL's swl:screen-width; set-window-size-defaults reads it but sizes
    windows by its scale argument only (racket/gui/views.rkt's value)"""
    return 1280


def swl_screen_height():
    """port: SWL's swl:screen-height (see swl_screen_width)"""
    return 1024


def set_window_size_defaults(scale):
    """constants.ss: set-window-size-defaults"""
    global p_default_trace_width, p_default_trace_height, p_virtual_trace_length, p_default_coderack_width, p_default_13x5_slipnet_width, p_default_temperature_width, p_default_memory_width, p_default_memory_height, p_virtual_memory_length, p_default_comment_window_width, p_default_comment_window_height, p_virtual_comment_window_length, p_EEG_window_width, p_EEG_window_height, p_virtual_EEG_length, p_top_theme_window_size, p_bottom_theme_window_size, p_vertical_theme_window_size, p_default_workspace_width
    screen_width = swl_screen_width()     # noqa: F841 (1.2: read, never used)
    screen_height = swl_screen_height()   # noqa: F841

    def width(w):
        return utilities.round_(chez.mul(scale, w))

    def height(h):
        return utilities.round_(chez.mul(scale, h))
    p_default_13x5_slipnet_width = width(650)
    p_default_coderack_width = width(230)
    p_default_temperature_width = width(70)
    p_default_trace_width = width(1000)
    p_default_trace_height = height(69)
    p_virtual_trace_length = width(7000)
    p_default_memory_width = width(260)
    p_default_memory_height = height(400)
    p_virtual_memory_length = height(2000)
    p_default_comment_window_width = width(300)
    p_default_comment_window_height = height(600)
    p_virtual_comment_window_length = height(4000)
    p_EEG_window_width = width(900)
    p_EEG_window_height = height(120)
    p_virtual_EEG_length = width(2000)
    p_top_theme_window_size = [width(600), height(140)]
    p_bottom_theme_window_size = [width(600), height(140)]
    p_vertical_theme_window_size = [width(160), height(590)]
    p_default_workspace_width = width(800)
    return "done"


# Common color names
c_white = swl_color(chez.String("white"))
c_black = swl_color(chez.String("black"))
c_grey = swl_color(chez.String("grey"))
c_red = swl_color(chez.String("red"))
c_green = swl_color(chez.String("green"))
c_blue = swl_color(chez.String("blue"))
c_yellow = swl_color(chez.String("yellow"))
c_pink = swl_color(chez.String("pink"))
c_orange = swl_color(chez.String("orange"))

#----------------------------------------------------------------------

# Control Panel
p_gui_command_line_color = swl_color(chez.String("azure"))
p_gui_speed_controls_color = swl_color(chez.String("lavender"))
p_gui_checkbox_select_color = swl_color(chez.String("royal blue"))
p_gui_menu_item_on_color = swl_color(chez.String("black"))
p_gui_menu_item_off_color = swl_color(chez.String("grey55"))

# bug workaround: for some reason on mac OS X, set-background-color!
# seems to affect the menu font *foreground* color instead of the
# background color, and grey85 is not very visible as a foreground color.
p_gui_menu_background_color = (swl_color(chez.String("black")) if sgl.g_platform == "macintosh"
                               else swl_color(chez.String("grey85")))

p_gui_help_window_color = swl_color(chez.String("bisque"))
p_gui_run_mode_foreground_color = swl_color(chez.String("green"))
p_gui_run_mode_background_color = swl_color(chez.String("black"))

# Workspace
p_workspace_background_color = swl_color(chez.String("white"))

# Slippages
p_vertical_slippage_color = swl_color(chez.String("magenta"))
p_dim_vertical_slippage_color = swl_color(chez.String("dark magenta"))
p_coattail_inducing_slippage_color = swl_color(chez.String("magenta"))
p_dim_coattail_inducing_slippage_color = swl_color(chez.String("dark magenta"))

# Bridges
p_top_bridge_color = swl_color(chez.String("red"))
p_vertical_bridge_color = swl_color(chez.String("dark violet"))
p_bottom_bridge_color = swl_color(chez.String("blue"))
p_bridge_label_background_color = swl_color(chez.String("yellow"))
p_faded_bridge_label_background_color = swl_color(chez.String("grey97"))

# Rules
p_top_rule_color = swl_color(chez.String("firebrick2"))
p_bottom_rule_color = swl_color(chez.String("medium blue"))

# Snags
p_snag_color = swl_color(chez.String("orange"))

# Answer descriptions
p_theme_supporting_concept_mapping_color = swl_color(chez.String("forest green"))

# Slipnet:
p_slipnet_background_color = swl_color(chez.String("lemon chiffon"))
p_slipnode_activation_color = swl_color(chez.String("midnight blue"))
p_frozen_slipnode_activation_color = swl_color(chez.String("deep sky blue"))

# Themespace:
p_theme_background_color__thematic_pressure_off = swl_color(chez.String("grey"))
p_theme_background_color__thematic_pressure_on = swl_color(chez.String("spring green"))
p_panel_highlight_color__thematic_pressure_off = swl_color(chez.String("lemon chiffon"))
p_panel_highlight_color__thematic_pressure_on = swl_color(chez.String("yellow"))
p_positive_theme_activation_color = swl_color(chez.String("forest green"))
p_negative_theme_activation_color = swl_color(chez.String("firebrick2"))
p_theme_edit_mode_color = swl_color(chez.String("white"))

# Temporal Trace:
p_trace_background_color = swl_color(chez.String("aquamarine"))
p_faded_workspace_structure_color = swl_color(chez.String("grey"))
p_workspace_event_structure_color = swl_color(chez.String("magenta"))

# Temporal Trace icon highlight colors
p_answer_event_icon_highlight_color = swl_color(chez.String("yellow"))
p_clamp_event_icon_highlight_color = swl_color(chez.String("spring green"))
p_concept_activation_event_icon_highlight_color = swl_color(chez.String("cyan"))
p_concept_mapping_event_icon_highlight_color = swl_color(chez.String("violet"))
p_group_event_icon_highlight_color = swl_color(chez.String("violet"))
p_top_rule_event_icon_highlight_color = p_top_rule_color
p_bottom_rule_event_icon_highlight_color = p_bottom_rule_color
p_snag_event_icon_highlight_color = swl_color(chez.String("red"))

# Comment window
p_comment_window_background_color = swl_color(chez.String("pink"))

# Concept-pattern colors
p_clamp_event_concept_pattern_color = p_frozen_slipnode_activation_color
p_concept_activation_event_concept_pattern_color = p_slipnode_activation_color
p_concept_mapping_event_concept_pattern_color = swl_color(chez.String("violet"))
p_group_event_concept_pattern_color = swl_color(chez.String("violet"))
p_top_rule_event_concept_pattern_color = p_top_rule_color
p_bottom_rule_event_concept_pattern_color = p_bottom_rule_color
p_snag_event_concept_pattern_color = p_snag_color

# Coderack:
p_coderack_background_color = swl_color(chez.String("misty rose"))
p_last_codelet_color = swl_color(chez.String("hot pink"))
p_current_codelet_color = swl_color(chez.String("yellow"))

p_extremely_low_urgency_color = swl_color(chez.String("grey40"))
p_very_low_urgency_color = swl_color(chez.String("grey50"))
p_low_urgency_color = swl_color(chez.String("grey60"))
p_medium_urgency_color = swl_color(chez.String("grey70"))
p_high_urgency_color = swl_color(chez.String("grey80"))
p_very_high_urgency_color = swl_color(chez.String("grey90"))
p_extremely_high_urgency_color = swl_color(chez.String("grey100"))

# Episodic Memory:
# grey75 = grey, grey100 = white, grey0 = black
p_memory_background_grey_level = 50

# Temperature:
p_temperature_background_color = swl_color(chez.String("LightCyan2"))
p_thermometer_mercury_color = swl_color(chez.String("firebrick2"))

# EEG:
p_EEG_background_color = swl_color(chez.String("black"))
p_EEG_title_color = swl_color(chez.String("white"))

# Mcat logo:
p_logo_background_color = swl_color(chez.String("light sky blue"))
# (define %logo-font% (swl-font sans-serif 18 'bold 'italic)): made by load(),
# port: as fonts.ss's sans-serif is chosen when the views start
p_logo_font = False


# The names above that the model files read (installed in the engine by load())
MODEL_GLOBALS = [
    "=white=", "=black=", "=grey=", "=red=", "=green=", "=blue=", "=yellow=",
    "=pink=", "=orange=",
    "%vertical-slippage-color%", "%dim-vertical-slippage-color%",
    "%coattail-inducing-slippage-color%", "%dim-coattail-inducing-slippage-color%",
    "%top-bridge-color%", "%vertical-bridge-color%", "%bottom-bridge-color%",
    "%bridge-label-background-color%", "%faded-bridge-label-background-color%",
    "%top-rule-color%", "%bottom-rule-color%", "%snag-color%",
    "%theme-supporting-concept-mapping-color%", "%faded-workspace-structure-color%",
    "%workspace-event-structure-color%", "%clamp-event-concept-pattern-color%",
    "%concept-activation-event-concept-pattern-color%",
    "%concept-mapping-event-concept-pattern-color%",
    "%group-event-concept-pattern-color%", "%top-rule-event-concept-pattern-color%",
    "%bottom-rule-event-concept-pattern-color%", "%snag-event-concept-pattern-color%",
    "%coderack-background-color%", "%current-codelet-color%",
    "%extremely-low-urgency-color%", "%very-low-urgency-color%", "%low-urgency-color%",
    "%medium-urgency-color%", "%high-urgency-color%", "%very-high-urgency-color%",
    "%extremely-high-urgency-color%",
]


def _set(name, value):
    """port: set! of a global of this file; also in the engine if the model reads it"""
    globals()[scheme_to_python(name)] = value
    if name in MODEL_GLOBALS:
        from metacat import engine
        engine.set_global(name, value)


# incomplete
def b_or_w_mode():
    """constants.ss: b/w-mode"""
    _set("%vertical-slippage-color%", swl_color(chez.String("black")))
    _set("%dim-vertical-slippage-color%", swl_color(chez.String("black")))
    _set("%coattail-inducing-slippage-color%", swl_color(chez.String("black")))
    _set("%dim-coattail-inducing-slippage-color%", swl_color(chez.String("black")))
    _set("%top-bridge-color%", swl_color(chez.String("black")))
    _set("%vertical-bridge-color%", swl_color(chez.String("black")))
    _set("%bottom-bridge-color%", swl_color(chez.String("black")))
    _set("%bridge-label-background-color%", swl_color(chez.String("grey93")))
    _set("%faded-bridge-label-background-color%", swl_color(chez.String("grey97")))
    _set("%top-rule-color%", swl_color(chez.String("black")))
    _set("%bottom-rule-color%", swl_color(chez.String("black")))
    _set("%snag-color%", swl_color(chez.String("black")))
    _set("%theme-supporting-concept-mapping-color%", swl_color(chez.String("black")))
    _set("%slipnode-activation-color%", swl_color(chez.String("black")))
    _set("%frozen-slipnode-activation-color%", swl_color(chez.String("grey50")))
    _set("%faded-workspace-structure-color%", swl_color(chez.String("grey40")))
    _set("%workspace-event-structure-color%", swl_color(chez.String("black")))
    # Concept-patterns
    _set("%clamp-event-concept-pattern-color%", p_frozen_slipnode_activation_color)
    _set("%concept-activation-event-concept-pattern-color%", p_slipnode_activation_color)
    _set("%concept-mapping-event-concept-pattern-color%", swl_color(chez.String("grey50")))
    _set("%group-event-concept-pattern-color%", swl_color(chez.String("grey50")))
    _set("%top-rule-event-concept-pattern-color%", swl_color(chez.String("grey50")))
    _set("%bottom-rule-event-concept-pattern-color%", swl_color(chez.String("grey50")))
    _set("%snag-event-concept-pattern-color%", p_snag_color)
    # Comment window
    _set("%comment-window-background-color%", swl_color(chez.String("white")))
    _set("%default-comment-window-width%", 500)
    _set("%default-comment-window-height%", 450)
    return "ok"


#----------------------------------------------------------------------
# Window titles and icons

p_workspace_icon_label = chez.String("Workspace")
p_workspace_icon_image = False
p_workspace_window_title = chez.String("Workspace")
p_temperature_icon_image = False
p_temperature_window_title = chez.String("Temperature")
p_slipnet_icon_label = chez.String("Slipnet")
p_slipnet_icon_image = False
p_slipnet_window_title = chez.String("Slipnet")
p_coderack_icon_label = chez.String("Coderack")
p_coderack_icon_image = False
p_coderack_window_title = chez.String("Coderack")
p_top_bridge_themes_icon_label = chez.String("Top Themes")
p_top_bridge_themes_icon_image = False
p_top_bridge_themes_window_title = chez.String("Top Themes")
p_bottom_bridge_themes_icon_label = chez.String("Bottom Themes")
p_bottom_bridge_themes_icon_image = False
p_bottom_bridge_themes_window_title = chez.String("Bottom Themes")
p_vertical_bridge_themes_icon_label = chez.String("Vertical Themes")
p_vertical_bridge_themes_icon_image = False
p_vertical_bridge_themes_window_title = chez.String("Vertical Themes")
p_trace_icon_label = chez.String("Temporal Trace")
p_trace_icon_image = False
p_trace_window_title = chez.String("Temporal Trace")
p_memory_window_icon_label = chez.String("Episodic Memory")
p_memory_window_icon_image = False
p_memory_window_title = chez.String("Episodic Memory")
p_comment_window_icon_label = chez.String("Commentary")
p_comment_window_icon_image = False
p_comment_window_title = chez.String("Commentary")
p_EEG_icon_label = chez.String("EEG")
p_EEG_icon_image = False
p_EEG_window_title = chez.String("EEG")
p_logo_icon_label = chez.String("Logo")
p_logo_icon_image = False
p_logo_window_title = chez.String("Logo")


def load():
    """port: install the colours the model reads in the engine, and make
    %logo-font% now that fonts.ss's sans-serif is chosen (fonts.load())."""
    global p_logo_font
    from metacat import engine
    from metacat.gui import fonts
    for name in MODEL_GLOBALS:
        engine.set_global(name, globals()[scheme_to_python(name)])
    p_logo_font = fonts.swl_font(fonts.sans_serif, 18, "bold", "italic")
