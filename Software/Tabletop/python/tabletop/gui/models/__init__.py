"""The Qt-free models of the GUI's panes (G-001): each pane has a model here, tested with
plain pytest and importable without PySide6, and a thin Qt view in `tabletop/gui/` that only
paints it (Numbo GUI, `numbo/models/`; Seek-Whence GUI, `seekwhence/gui/models/`). Nothing
here imports Qt, and nothing here draws from the engine's generator.
"""
