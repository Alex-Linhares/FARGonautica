"""Qt-free models of a run, kept from its events and from read-only snapshots of the
`World` (Numbo GUI, `numbo/models/`; G-001: they live inside `seekwhence/gui/` so that the
engine package outside `gui/` stays as loop0003 left it). Each pane has one model here and
one thin Qt view in `seekwhence/gui/`; the models are tested with plain pytest, without
PySide6. The history is `snapshot` (the `World` as plain data),
`tap` (the observer), `event_log` (events and snapshots) and `run_controller` (the worker
thread and its gate), items 01's four modules (G-003, G-004, G-005).
"""
