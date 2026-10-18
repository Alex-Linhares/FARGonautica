"""The live Qt GUI of the Seek-Whence reconstruction: python3 -m seekwhence.gui.

PySide6 views over the Qt-free models in `seekwhence.gui.models`, after the Numbo GUI
(`numbo/gui/`; ANSWERS.md 7). Nothing in the engine imports this package, so the engine
and the CLI work without PySide6 (ANSWERS.md 1, G-001). The GUI only observes a
`seekwhence.session.Session`: it never draws from the run's `Rng` and never runs a task,
so a seed gives the same run with or without it.
"""
