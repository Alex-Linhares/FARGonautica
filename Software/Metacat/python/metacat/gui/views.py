"""The views: the graphics files' windows, loaded and attached to a run.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from racket/gui/views.rkt (the port's part
of it: the SWL stand-ins are gui/hosts.py, gui/swl.py and the modules
themselves).

`load_views()` is the counterpart of metacat.ss loading the graphics files into
the program's one top level: it loads the gui modules in metacat.ss's order,
and their load() installs in the engine the names the model reads (colours,
fonts, *fg-color*, restore-current-state, ...).  It changes nothing in a run by
itself.  `attach_workspace_view` and `attach_views` make the windows as
setup.ss's (setup) does and turn the graphics switches on; call them before
init-mcat (headless.run_problem's views argument).  Watching a run never
changes it.  This module does not import tkinter.
"""
from __future__ import annotations

import importlib
import importlib.util

from metacat import setup, view_globals

# metacat.ss's load order of the graphics files (gui modules)
GUI_LOAD_ORDER = [
    "constants", "general_graphics", "slipnet_graphics", "workspace_graphics",
    "temperature_graphics", "coderack_graphics", "theme_graphics", "trace_graphics",
    "memory_graphics", "commentary_graphics", "eeg_graphics",
]

_loaded = False


def _gui_module(name):
    return importlib.import_module("metacat.gui." + name)


def load_views():
    """racket/gui/views.rkt: the module body (the includes, in metacat.ss's
    order).  Once per process: the offscreen fonts unless fonts.ss's hidden
    canvas exists (a Tk GUI made it), sgl.load(), then each gui module's load()."""
    global _loaded
    if _loaded:
        return
    from metacat import engine
    from metacat.gui import fonts, hosts, sgl
    engine.load()
    if not fonts.g_hidden_canvas:
        hosts.install_offscreen_fonts()
    sgl.load()
    for name in GUI_LOAD_ORDER:
        if importlib.util.find_spec("metacat.gui." + name) is None:
            continue   # port: not translated yet
        module = _gui_module(name)
        if hasattr(module, "load"):
            module.load()
    _loaded = True


def _full_speed_no_flashing():
    # gui.ss's speed settings as at full speed with no flashing (the control
    # panel's speed slider sets them once it exists)
    view_globals.p_num_of_flashes = 1
    view_globals.p_flash_pause = 0
    view_globals.p_snag_pause = 0
    view_globals.p_codelet_highlight_pause = 0
    view_globals.p_text_scroll_pause = 0


def attach_workspace_view(width=800):
    """racket/gui/views.rkt: attach-workspace-view!.  The Workspace window, as
    (setup) makes it, with workspace graphics on.  Returns the window."""
    load_views()
    _full_speed_no_flashing()
    setup.g_workspace_window = _gui_module("workspace_graphics").make_workspace_window(width)
    setup.p_workspace_graphics = True
    return setup.g_workspace_window


def attach_views(scale=1):
    """racket/gui/views.rkt: attach-views!.  Every window, as setup.ss's (setup)
    makes them (without the logo and the control panel, item 15), with every
    graphics switch on as setup.ss defines them.  Returns a dict of the windows
    by name."""
    load_views()
    _full_speed_no_flashing()
    _gui_module("constants").set_window_size_defaults(scale)
    sg = _gui_module("slipnet_graphics")
    tg = _gui_module("theme_graphics")
    setup.g_workspace_window = _gui_module("workspace_graphics").make_workspace_window()
    setup.g_slipnet_window = sg.make_slipnet_window(sg.g_13x5_layout_table)
    setup.g_coderack_window = _gui_module("coderack_graphics").make_coderack_window()
    setup.g_themespace_window = tg.make_themespace_window(tg.g_themespace_window_layout)
    from metacat.objects import tell
    setup.g_top_themes_window = tell(setup.g_themespace_window, "get-window", "top-bridge")
    setup.g_bottom_themes_window = tell(setup.g_themespace_window, "get-window", "bottom-bridge")
    setup.g_vertical_themes_window = tell(setup.g_themespace_window, "get-window",
                                          "vertical-bridge")
    setup.g_memory_window = _gui_module("memory_graphics").make_memory_window()
    setup.g_comment_window = _gui_module("commentary_graphics").make_comment_window()
    setup.g_trace_window = _gui_module("trace_graphics").make_trace_window()
    setup.g_temperature_window = _gui_module("temperature_graphics").make_temperature_window()
    setup.g_EEG_window = _gui_module("eeg_graphics").make_EEG_window()
    setup.p_workspace_graphics = True
    setup.p_slipnet_graphics = True
    setup.p_coderack_graphics = True
    setup.p_codelet_count_graphics = True
    setup.p_highlight_last_codelet = True
    return {
        "workspace": setup.g_workspace_window,
        "slipnet": setup.g_slipnet_window,
        "coderack": setup.g_coderack_window,
        "top-themes": setup.g_top_themes_window,
        "bottom-themes": setup.g_bottom_themes_window,
        "vertical-themes": setup.g_vertical_themes_window,
        "memory": setup.g_memory_window,
        "commentary": setup.g_comment_window,
        "trace": setup.g_trace_window,
        "temperature": setup.g_temperature_window,
        "EEG": setup.g_EEG_window,
    }


def window_items(window):
    """port: the number of items created on a window's (offscreen) canvas"""
    from metacat.objects import tell
    return tell(window, "get-vp").canvas.items


def set_thread_break_handler(handler):
    """racket/gui/views.rkt: set-thread-break-handler! (the Workspace window's
    click handler interrupts the engine thread through it)"""
    return _gui_module("workspace_graphics").set_thread_break_handler(handler)


# the variables of the graphics files that the control panel set!s
_SETTABLE = {
    "%comment-window-font%": "commentary_graphics",
    "*theme-edit-mode?*": "theme_graphics",
}


def set_view_global(sym, value):
    """racket/gui/views.rkt: set-view-global!, set! of a variable of the
    graphics files from the control panel, as gui.ss set!s them on the shared
    top level"""
    from metacat import chez
    from metacat.names import scheme_to_python
    if sym not in _SETTABLE:
        raise chez.SchemeError("set-view-global!", "not a settable view global: ~s", sym)
    setattr(_gui_module(_SETTABLE[sym]), scheme_to_python(sym), value)
