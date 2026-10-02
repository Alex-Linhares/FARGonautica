# python/tests

The Python tests (pytest). Every expected value about the engine comes from
the SBCL port: either from a fixture (`../fixtures/`, written by
`lisp/tests/oracle/`), or from running SBCL side by side.

```sh
python3 -m pytest python/tests -q          # from Software/Numbo/; needs sbcl for the oracle tests
```

Qt draws offscreen (`conftest.py` sets `QT_QPA_PLATFORM`). Tests that need
SBCL or PySide6 are skipped when those are missing.

**The engine against the oracle**: `test_rng`, `test_franz`,
`test_franz_census`, `test_pnet_def`, `test_pnet_functions`, `test_coderack`,
`test_cyto_def`, `test_codelets_a`/`_b`/`_c`, `test_main_loop`,
`test_solution_checker`, `test_cli`, and **`test_full_runs`**: 220 full runs
(11 chapter puzzles × 20 seeds), Python versus SBCL, compared event by event.

**Observation and models**: `test_observe`, `test_session`,
`test_tree_model`, `test_tree_layout`, `test_pnet_coderack_models`,
`test_event_log`, `test_run_controller`, `test_run_stats`.

**The GUI**: [`gui/`](gui/).

**Housekeeping**: `test_fixtures_current` (fixtures match a fresh
regeneration), `test_readme` (the README's commands run as written),
`test_packaging`, `test_skeleton`.

**Helpers**: `conftest.py`, `full_runs.py` (the differential runner, also a
script), `codelet_cases.py`, `cyto_state.py` and `world_state.py` (the
fixtures' state encodings).
