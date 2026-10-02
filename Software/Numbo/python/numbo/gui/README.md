# python/numbo/gui

The PySide6 GUI: `python3 -m numbo.gui` (or `numbo-gui`). Each view is an
observer drawing one of the Qt-free models in [`../models/`](../models/), and
the run waits after every event until every view has redrawn.

| Module | What it is |
|---|---|
| `app.py`, `__main__.py` | The entry point and its arguments (`--puzzle`, `--seed`, `--play`, `--smoke`). |
| `main_window.py` | The main window: dockable panes over a run controller. Events cross from the worker thread through queued signals. Session > Open / Save, the Run menu and its shortcuts, remembered settings. |
| `paint.py` | The every-event redraw, with one window paint per event. |
| `tree_view.py` | The AST canvas: the cytoplasm's trees, styled by type, status, activation and the latest change; zoom, pan, fit. |
| `pnet_view.py` | The Pnet: 88 nodes shaded by activation, with the nodes that have cytoplasm instances marked. |
| `coderack_view.py` | The urgency bins, their counts and the codelets waiting. |
| `log_view.py` | The event log: one row per event, filterable by kind. |
| `timeline.py` | The slider over the run's events, back and forward, go to an iteration. |
| `controls.py` | Puzzle, seed and cap; play, pause, step, stop; speed. |
| `stats_view.py` | Iteration, temperature, codelets waiting, outcome and the solution check. |
| `render.py` | A scene rendered to a PNG, used by the tests and the screenshots. |
