# lisp/tests

The SBCL port's tests. Run them all from anywhere:

```sh
bash lisp/tests/run-tests.sh      # or ./run-tests.sh in Software/Numbo/, which also runs the Python tests
```

Each `.lisp` file runs on its own, from `lisp/`, and exits 0 if every check passes:
`sbcl --non-interactive --load tests/FILE.lisp`.

| File | What it checks |
|---|---|
| `run-tests.sh` | Runs everything below, in order. Graphics are kept off (`WINDOW_GFX` unset). |
| `read-pass.lisp` | Every ported source file READs cleanly, and has no non-ASCII OCR debris. |
| `franz-compat-tests.lisp` | The Franz Lisp built-ins (`src/franz-compat.lisp`). |
| `flavors-compat-tests.lisp` | Flavors on CLOS (`src/flavors-compat.lisp`), and the real flavor files loading. |
| `coderack-tests.lisp` | The reconstructed coderack (`src/coderack.lisp`), including its weighted choice, checked statistically. |
| `graphics-tests.lisp` | The graphics stubs (`src/graphics-stubs.lisp`). |
| `pnet-compile-tests.lisp` | The Pnet files compile cleanly; `initialize-pnet` works. |
| `cyto-codelets-compile-tests.lisp` | The cytoplasm and codelets compile, with no undefined functions in the whole system. |
| `compile-helpers.lisp` | Shared by the two compile tests. |
| `boot-tests.lisp` | First boot: `(config 31 3 5 24 3 14)` runs 500 iterations headless. |
| `solution-tests.lisp` | A run to completion (seed 18), checked by `src/solution-checker.lisp`. |
| `validation-tests.lisp`, `trace-tools.lisp` | The port against the 1987 trace `trace3.31` and the chapter's puzzles. |
| `chapter-runs.lisp` | Not a test: regenerates the tables of `src/RESULTS.md`. |
| `readme-test.sh` | Every `sh` block of `src/README.md` runs as written. |
| `oracle-mode-tests.sh` | Oracle mode is opt-in: a default load is unchanged. |
| `oracle-tests.lisp` | The oracle hooks (`src/oracle.lisp`): shared RNG, double floats, copying `sortcar`, JSON-lines trace. |
| [`oracle/`](oracle/) | Not tests: the scripts that capture the Python implementation's fixtures. |
