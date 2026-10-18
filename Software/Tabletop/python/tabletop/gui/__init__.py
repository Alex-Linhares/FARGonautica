"""The live Qt GUI of the Tabletop reconstruction: python3 -m tabletop.gui.

PySide6 views over the Qt-free models in `tabletop.gui.models`, after the Numbo GUI
(`numbo/gui/`) through the Seek-Whence GUI (`seekwhence/gui/`), the two precedents for
every convention. Nothing in the engine imports this package, so the engine and the CLI
work without PySide6 (G-001). The GUI only observes a run: it draws nothing from the
run's generator and never runs a codelet, so a seed gives the same run with or without it
(`docs/engine-api.md`, "The observer rules").
"""
