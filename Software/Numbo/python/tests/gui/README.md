# python/tests/gui

pytest-qt tests of the GUI, offscreen. They are skipped if PySide6 is missing.

| File | What it checks |
|---|---|
| `test_gui_shell.py` | The main window, controls and stats: play, pause, step, stop, closing mid-run, bad input. |
| `test_tree_view.py` | The tree canvas matches the model after every event; the highlights; the snapshots. |
| `test_pnet_coderack_views.py` | The Pnet and coderack views against their models. |
| `test_log_replay.py` | The event log, Session > Save / Open, scrubbing, and opening an SBCL trace. |
| `test_polish.py` | Settings, shortcuts, the error banner, memory, and what's painted after every event. |
