# python/tests_slow

The slow suite (ANSWERS.md D2 of the roles loop, 2026-10-14): every test that takes more than
about two seconds on its own, or surveys many seeds, moved here out of `tests/` so that the
default suite stays fast. Run from `Software/Tabletop/python/`:

```sh
python3 -m pytest -q tests_slow      # or: bash run-tests.sh --slow
bash run-tests.sh --all              # both suites
```

`pytest.ini` here is the suite's configuration (pytest takes it when every argument is under
this folder): no `-m 'not slow'`, and `pythonpath` naming `tests/` and `tests/gui/`, so that
each `*_slow.py` file imports its helpers and fixtures from the fast file it was moved from
(`tests/test_benches.py` for `test_benches_slow.py`, and so on), which keeps the quick checks
of the same feature. `gui/conftest.py` re-exports `tests/gui/conftest.py`'s fixtures. Every
test carries the `slow` marker as a safety net. It is not timed by the loop's gate; run it
when a change touches what it covers, and record its result in the loop's PROGRESS.md.

| File | What is here, and why it is slow |
|---|---|
| `test_benches_slow.py` | The rho lesions and faults at sample size (several surveys); the `bench all` smoke run at full size (fifteen benches at one run each). |
| `test_calibrate_slow.py` | The human scan with and without worker processes. |
| `test_invariants_slow.py` | 1,000 checked runs keep every invariant and none is capped; twenty random tables under `standard` and five under `french-faults`. |
| `test_quotes_slow.py` | `check_quotes.py --page` and `--find` as subprocesses over the PDFs. |
| `test_readme_slow.py` | The command blocks of the front page and of `python/README.md`, run and compared with the output shown (the fast suite runs the quick start alone). |
| `test_run_slow.py` | Many seeds on three problems end with an answer; every problem runs to an answer with one seed. |
| `test_context_slow.py` | Every problem's whole run pushes and pops contexts in pairs and draws nothing for it. |
| `test_determinism_slow.py` | Self-determinism of the compiled engine (D-112): the families problems x 5 seeds digest the same in two processes under two `PYTHONHASHSEED`s, and the four golden cases give their recorded fingerprints in both (the fast suite's `test_determinism.py` does one case and a two-problem workload). |
| `test_equivalence_slow.py` | The statistical equivalence of the pure engine (D-113): `tools/equivalence.py` at its defaults, every families problem at 200 runs on each engine (two processes), every total variation within the tolerance 0.25 and the simulated null under it (the fast suite's `test_equivalence.py` runs three problems x 30 runs on the compiled engine against the recorded pure distributions). |
| `test_stated_slow.py` | The `stated` bench at a few runs, and the statements the mechanisms guarantee over many runs. |
| `test_variants_slow.py` | `tools/run_on_baseline.py` running the variants bench on a commit from a throwaway worktree. |
| `gui/test_coderack_model_slow.py` | The capped run's history stays responsive. |
| `gui/test_config_view_slow.py` | An edit during a run, and the next run using the edit, through the window. |
| `gui/test_controls_view_slow.py` | The same seed repeats the run through the window. |
| `gui/test_gui_readme_slow.py` | `scripts/docs_screenshots.py` renders the four pictures offscreen. |
| `gui/test_log_view_slow.py` | Seeking refused while a run is live (a second run through the window). |
| `gui/test_main_window_slow.py` | The headless run through the window equals the plain run; the run actions drive the controller. |
| `gui/test_packaging_slow.py` | The `--smoke` command as a subprocess: with a problem and seed, and with the defaults from a bare environment (the fast suite keeps a capped bare-environment run). |
| `gui/test_palette_view_slow.py` | "Do this!" runs the edited table through the window. |
| `gui/test_results_model_slow.py` | A batch of fifty equals `run_batch`. |
| `gui/test_results_view_slow.py` | The batch line painted once per event over the whole quick start through the window (the fast suite keeps a capped run with the same probe). |
| `gui/test_robustness_slow.py` | The pixel probe at sampled events and after every event; memory over five runs; the capped run at max speed; the window at 1280 x 800; a state naming other docks. |
| `gui/test_session_view_slow.py` | Save then Open, part way, tampered, the menu's dialogs, an edited table, Play again after a stopped replay: each a whole run or replay through the window. |
| `gui/test_tap_slow.py` | The forced capped run takes keyframes and stays the same run. |
| `gui/test_trace_model_slow.py` | The capped run fed to the trace model in bounded time. |
