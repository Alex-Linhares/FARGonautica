"""Stand-ins for engine modules that are not translated yet (test helper).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

A battery's (define name value) or b:set-global! may concern a global that belongs
to a later engine module (*workspace* to workspace.py, *trace* to trace.py ...).
`engine_module(name, **attrs)` sets those attributes on the real module if it
exists, else on a temporary stand-in registered as metacat.<name>, and restores
everything afterwards.  The engine reads such globals at call time through the
package (metacat.workspace.g_workspace), so a stand-in is seen.
"""
from __future__ import annotations

import importlib
import importlib.util
import sys
import types
from contextlib import contextmanager

import metacat


@contextmanager
def engine_module(name, **attrs):
    """The battery's (define name value) for names that belong to a later engine
    module (metacat.setup, metacat.slipnet, metacat.coderack ...): set them on the
    real module if it exists, else on a temporary stand-in; restore afterwards."""
    full = "metacat." + name
    existed = full in sys.modules or importlib.util.find_spec(full) is not None
    if existed:
        mod = importlib.import_module(full)
    else:
        mod = types.ModuleType(full)
        sys.modules[full] = mod
        setattr(metacat, name, mod)
    saved = {k: getattr(mod, k) for k in attrs if hasattr(mod, k)}
    for k, v in attrs.items():
        setattr(mod, k, v)
    try:
        yield mod
    finally:
        for k in attrs:
            if k in saved:
                setattr(mod, k, saved[k])
            elif hasattr(mod, k):
                delattr(mod, k)
        if not existed:
            del sys.modules[full]
            delattr(metacat, name)
