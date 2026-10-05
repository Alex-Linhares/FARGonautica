"""The single-window GUI: every panel of Metacat in one Qt main window (loop0003).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).

`python3 -m metacat.qt` opens it.  PySide6 is an optional dependency
(`pip install -e 'python[qt]'`): the engine (metacat/*.py) and the tkinter GUI
(metacat/gui/) never import this package, and this module imports no Qt, so
`has_pyside6()` can be asked without PySide6.  The design is
docs/qt-gui-plan.md.
"""
from __future__ import annotations


def has_pyside6():
    """port: whether PySide6's widgets can be imported"""
    try:
        import PySide6.QtWidgets  # noqa: F401
    except ImportError:
        return False
    return True
