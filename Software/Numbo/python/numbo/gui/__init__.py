"""The live Qt GUI (not 1987 source; loop0003): python3 -m numbo.gui.

PySide6 views over the Qt-free models in numbo.models.  Nothing in the
engine imports this package, so the engine and the CLI work without
PySide6.  See main_window.py for the threading and the redraw handshake.
"""
