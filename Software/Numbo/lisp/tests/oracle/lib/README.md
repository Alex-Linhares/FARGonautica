# lisp/tests/oracle/lib

Helpers for the oracle scripts. They live here so `python/scripts/regen_fixtures.sh`,
which runs only `lisp/tests/oracle/*.lisp`, doesn't treat them as capture scripts.

| File | What it is |
|---|---|
| `world-state.lisp` | The World-state encoding (Pnet, cytoplasm, coderack, RNG, free globals) used by the codelet fixtures. Its Python twin is `python/tests/world_state.py`. |
| `full-run.lisp` | One full oracle run in a fresh SBCL process, writing its trace and outcome to files. `python/tests/full_runs.py` uses it for the 220-run SBCL-vs-Python comparison. |
