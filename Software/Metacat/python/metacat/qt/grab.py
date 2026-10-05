"""Screenshots: a widget grabbed to a PNG file (for the tests and the docs).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
"""
from __future__ import annotations

from pathlib import Path


def grab_png(widget, path):
    """Render widget (and its children) with QWidget.grab() into the PNG file
    path, creating its folder.  Works offscreen.  Returns the path."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    if not widget.grab().save(str(path), "PNG"):
        raise OSError("could not write %s" % path)
    return path
