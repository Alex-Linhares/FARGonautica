"""setup.ss: the global counters, the window globals, the configuration switches
and the user-interface commands.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from setup.ss, with racket/engine/setup.rktl
as a worked translation.

(setup) and enable-resizing create and arrange the windows, which the engine
cannot do; they belong to the GUI (item 15), which sets the window globals below
through engine.set_global.  Other modules read and assign these globals
qualified (setup.g_temperature = 50), so that every assignment is seen.  The
engine never imports tkinter.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import view_globals
from metacat.chez import String as _String, printf
from metacat.objects import tell

g_codelet_count = 0
g_temperature = 0

g_workspace_window = False
g_slipnet_window = False
g_coderack_window = False
g_themespace_window = False
g_top_themes_window = False
g_bottom_themes_window = False
g_vertical_themes_window = False
g_memory_window = False
g_comment_window = False
g_trace_window = False
g_temperature_window = False
g_EEG_window = False
g_control_panel = False

# Default configuration:
p_eliza_mode = True
p_justify_mode = False
p_self_watching_enabled = True
p_verbose = False
p_workspace_graphics = True
p_slipnet_graphics = True
p_coderack_graphics = True
p_codelet_count_graphics = True
p_highlight_last_codelet = True
p_nice_graphics = True

g_repl_thread = False


# ------------------------------------------------------------------
# User-interface commands
#
# *display-mode?* (run.ss) and *memory* (memory.ss) belong to modules translated
# later; they are read through the package at call time.

def eliza_mode_on():
    """setup.ss: eliza-mode-on"""
    global p_eliza_mode
    p_eliza_mode = True
    tell(g_comment_window, "switch-modes")
    return "ok"


def eliza_mode_off():
    """setup.ss: eliza-mode-off"""
    global p_eliza_mode
    p_eliza_mode = False
    tell(g_comment_window, "switch-modes")
    return "ok"


def slipnet_on():
    """setup.ss: slipnet-on"""
    global p_slipnet_graphics
    p_slipnet_graphics = True
    if _metacat.run.g_display_mode_p is False:
        tell(g_slipnet_window, "restore-current-state")
    return "ok"


def slipnet_off():
    """setup.ss: slipnet-off"""
    global p_slipnet_graphics
    p_slipnet_graphics = False
    if _metacat.run.g_display_mode_p is False:
        tell(g_slipnet_window, "blank-window")
    return "ok"


def coderack_on():
    """setup.ss: coderack-on"""
    global p_coderack_graphics
    p_coderack_graphics = True
    if _metacat.run.g_display_mode_p is False:
        tell(g_coderack_window, "restore-current-state")
    return "ok"


def coderack_off():
    """setup.ss: coderack-off"""
    global p_coderack_graphics
    p_coderack_graphics = False
    if _metacat.run.g_display_mode_p is False:
        # chez: a string, which the window draws as text (anomalies: "The graphics and rules.ss tell strings from symbols")
        tell(g_coderack_window, "blank-window", _String("Coderack"))
    return "ok"


def codelet_counts_on():
    """setup.ss: codelet-counts-on"""
    global p_codelet_count_graphics
    p_codelet_count_graphics = True
    tell(g_coderack_window, "initialize")
    return "ok"


def codelet_counts_off():
    """setup.ss: codelet-counts-off"""
    global p_codelet_count_graphics
    p_codelet_count_graphics = False
    tell(g_coderack_window, "initialize")
    return "ok"


def clearmem():
    """setup.ss: clearmem"""
    tell(_metacat.memory.g_memory, "clear")
    return "ok"


def verbose_on():
    """setup.ss: verbose-on"""
    if tell(g_control_panel, "verbose-mode?") is False:
        tell(g_control_panel, "toggle-verbose-mode")
    return "ok"


def verbose_off():
    """setup.ss: verbose-off"""
    if tell(g_control_panel, "verbose-mode?") is not False:
        tell(g_control_panel, "toggle-verbose-mode")
    return "ok"


def speed():
    """setup.ss: speed"""
    printf("Current speed settings:~n")
    printf("  %num-of-flashes%           ~a~%", view_globals.p_num_of_flashes)
    printf("  %flash-pause%              ~a ms~%", view_globals.p_flash_pause)
    printf("  %snag-pause%               ~a ms~%", view_globals.p_snag_pause)
    printf("  %codelet-highlight-pause%  ~a ms~%", view_globals.p_codelet_highlight_pause)
    printf("  %text-scroll-pause%        ~a ms~%", view_globals.p_text_scroll_pause)

