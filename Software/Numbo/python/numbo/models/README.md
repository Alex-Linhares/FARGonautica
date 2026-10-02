# python/numbo/models

Qt-free models of a run, each kept up to date from the run's events (each one
is an observer). The GUI's views draw them, and they are tested without Qt
(`python/tests/test_*.py`).

| Module | What it models |
|---|---|
| `tree_model.py` | The cytoplasm as a forest of arithmetic trees: targets and derived targets, blocks, free bricks, killed "ghost" nodes; expressions in the solution checker's notation. Checked against the live World after every event. |
| `tree_layout.py` | A deterministic tidy-tree (Reingold–Tilford) layout of that forest that stays stable from event to event. |
| `layout_history.py` | The tree layout at any event of a run (snapshots every 64 events), for scrubbing. |
| `pnet_model.py` | The Pnet's 88 nodes, links and current activations, on a fixed grid. |
| `coderack_model.py` | The codelets waiting in each urgency bin, and the codelet just chosen. |
| `event_log.py` | The run's history: every event, counts by kind, where each iteration starts; one line of text per event. |
| `run_stats.py` | The run's inputs (the chapter's puzzles, parsing) and the stats panel's model. |
| `run_controller.py` | Runs Numbo, a session or an SBCL trace on a worker thread behind a per-event gate: each event waits until it is drawn. Play, pause, step, stop. |
