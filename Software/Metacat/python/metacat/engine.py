"""The engine's load order and its global top level.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Part of its translation to Python (2026); the counterpart of metacat.ss
(the load order) and of racket/engine.rkt's set-global!.

Every engine module holds definitions only; a top-level define whose value needs
another engine module is made by that module's load().  `load()` imports the
modules translated so far and calls their load() in metacat.ss's order, once.
`set_global(name, value)` is a battery's b:set-global! and a driver's set! of a
global: it maps the Scheme name to the module that defines it
(docs/python-translation-plan.md, "The global top level and Python modules").
The engine never imports tkinter.
"""
from __future__ import annotations

import importlib
import importlib.util
import sys

from metacat import chez
from metacat.names import scheme_to_python

# metacat.ss's load order, as Python module names; view_globals stands for the
# graphics files' globals that the model reads (racket/engine/view-globals.rktl).
LOAD_ORDER = [
    "sugar", "utilities", "fonts", "constants", "setup", "coderack", "descriptions",
    "bonds", "groups", "bridges", "breakers", "workspace", "workspace_objects",
    "workspace_structures", "workspace_strings", "concept_mappings",
    "workspace_structure_formulas", "run", "formulas", "slipnet", "images", "rules",
    "answers", "themes", "justify", "trace", "jootsing", "memory", "sgl_interpreter",
    "general_graphics", "slipnet_graphics", "workspace_graphics", "temperature_graphics",
    "group_graphics", "bridge_graphics", "rule_graphics", "coderack_graphics",
    "theme_graphics", "trace_graphics", "memory_graphics", "commentary_graphics",
    "eeg_graphics", "demos", "gui",
]
GLOBAL_MODULES = LOAD_ORDER + ["view_globals"]

_loaded = False


def translated_modules():
    """The engine modules of LOAD_ORDER that exist so far, imported, in order.
    A package is not an engine module: "gui" (gui.ss) is the view
    metacat/gui/gui.py, and metacat.gui is never imported here."""
    modules = []
    for name in LOAD_ORDER:
        full = "metacat." + name
        spec = importlib.util.find_spec(full)
        if spec is not None and spec.submodule_search_locations is None:
            modules.append(importlib.import_module(full))
    return modules


def load():
    """metacat.ss: load every file in order (here: call each module's load(),
    once per process)."""
    global _loaded
    if _loaded:
        return
    _loaded = True
    for module in translated_modules():
        if hasattr(module, "load"):
            module.load()


def _module_of(name):
    attr = scheme_to_python(name)
    for mod_name in GLOBAL_MODULES:
        module = sys.modules.get("metacat." + mod_name)
        if module is not None and attr in vars(module):
            return module, attr
    raise chez.SchemeError("set-global!", "~s is not a global of the engine", name)


def set_global(name, value):
    """b:set-global! / set! of a global by its Scheme name (*temperature* ...)."""
    module, attr = _module_of(name)
    setattr(module, attr, value)


def get_global(name):
    """The value of a global by its Scheme name."""
    module, attr = _module_of(name)
    return getattr(module, attr)
