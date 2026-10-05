# chez_scheme/oracle/tests/: checks of the oracle itself

These seven Chez Scheme scripts check that the [oracle](../README.md) is sound. Chez 10
must read the original. The random-number generator must match its written
specification. The original must run headless and reproducibly, and its documented demo
runs must replay. Tracing must not change a run, and the golden traces must be
reproducible from scratch. They test the oracle, not the ports: a port is tested against
the oracle's output by the Racket and Python test suites.

`bash tests/run-tests.sh` runs every `*.ss` file here, after the Racket tests, with
`scheme --script` from the repository root. A check passes by exiting 0 and fails by
exiting non-zero, printing `FAIL:` lines or a count of failures. To run one alone:

```bash
scheme --script chez_scheme/oracle/tests/rng-check.ss
```

| Check | What it verifies |
|---|---|
| `reader-check.ss` | The Chez version string starts with `Chez Scheme Version 10`. `chez_scheme/original/` holds exactly 45 `.ss` files, and Chez's reader reads every form of each one (the files are only read, never loaded). It prints the total, currently `read 1454 top-level forms from 45 files`. |
| `rng-check.ss` | The generator specification in [`docs/trace-format.md`](../../../docs/trace-format.md#randomness-plan-item-01) (the 32-bit LCG `S*72931 + 90763387 mod 2^32`, and how `(random n)` and `(random x)` build their results) is written out in portable exact arithmetic and compared with Chez's built-in `random`, value for value, bit for bit, and in the generator state after every draw. It covers 11 seeds × 19 arguments (integers from 1 to `most-positive-fixnum`, flonums from `1e-300` to `100.0`) × 200 draws, plus 2,000 interleaved integer and `1.0` draws as the model makes them. It also checks that `random-seed` rejects 0, 2^32 and −1. |
| `headless-run-check.ss` | `run.ss` loads and runs the unmodified original. For 4 problems (`abc abd xyz`, `abc abd ijk`, `eqe qeq abbbc`, `abc abd mrrjjj`) × seeds 1, 2, 3, with a 20,000-codelet cap, each run must exit 0 and print an `Answer:` line. A second run of the same problem and seed must print byte-identical output. |
| `demo-replay-check.ss` | Seeds chosen under the 1999-era Chez still replay under Chez 10. Five runs documented in the comments of `original/demos.ss` must give exactly the documented answers at the documented codelet counts: misc1 (justifies `mmmrrj` at 7794), misc2 (justifies `abd` at 1126), misc4 (`b` at 453, then `y` at 945), misc5 (`flz` 1695, `dlz` 1710, `hlz` 1721) and the commented-out misc9 (`dyz` at 2257). Each run is a fresh process, so the Episodic Memory starts empty, as `demos.ss` says it should. misc3 and misc6–misc8 are left out because they do not replay as documented. |
| `trace-check.ss` | For 4 runs (`abc abd xyz` seed 3; `abc abd mrrjjj` seed 2; `a b z` seed 3861033416 with `--keep-going`; the justify run `abc abd ijk abd` seed 3386544399), `run.ss --trace` must exit 0 and print **exactly** what the same run prints without `--trace`, so tracing draws nothing and changes nothing. Each trace must pass `validate-trace.py` with the event types that run must produce (for example `codelet,build,break,temperature,slipnet,themes,answer,comment,event` for the first). A second traced run must write the same trace byte for byte. |
| `golden-check.ss` | Runs `make-golden.ss --check`. Every trace listed in `tests/problems.txt` is regenerated into a temporary directory and must equal the committed `tests/golden/*.jsonl` byte for byte, with no file missing and none extra. Then every golden must pass `validate-trace.py`. It never writes into `tests/golden/`. This is the slowest check, since it reruns all 109 golden runs in parallel. |
| `workspace-init-check.ss` | `tests/diff/workspace-dump.scm`'s `b:init-problem` is a copy of the Workspace part of the original's `init-mcat`. The Racket port's workspace battery used it before `run.ss` was ported. For every problem in `tests/problems.txt` with its first seed, the copy must build the same initial Workspace (strings, letters, descriptions, salience, importance and unhappiness values), compared through `b:canon`, as the real `init-mcat`. The copy must also make no random draws. |

The checks that start other runs (`headless-run-check`, `demo-replay-check`,
`trace-check`, `golden-check`) find Chez as `scheme`, or as `chezscheme` if there is no
`scheme`. They write their scratch files under `/tmp` and delete them afterwards.
