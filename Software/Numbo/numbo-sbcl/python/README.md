# Numbo (Defays, 1987) in Python

This directory holds a Python translation of Daniel Defays' Numbo. The SBCL
port in [`../src/`](../src/README.md) is the **oracle**: the translation was
written test-first against it, and it reproduces it run for run. With the
same seed, the Python and the SBCL port in oracle mode make the same codelet
choices, create and kill the same nodes, print the same text and write the
same JSON-lines event trace. That includes the 1987 flaws the port keeps:
the `kill-block` gap, the `reactivate-cyto` race, `(max)` = 0, and
truncating division.

The layout follows the Lisp: one module per Lisp file and one function per
Lisp function, with the same names (`look-for-new-block` is
`look_for_new_block`). Each docstring names its Lisp origin
(`codelets.lisp: look-for-new-block`), so the two can be read side by side.
Each place that reproduces a 1987 quirk has a `# 1987:` comment citing
`src/PORTING_NOTES.md`.

## Requirements

- **Python 3.12** (`python3`). The package uses only the standard library.
- **pytest 7.4** to run the tests.
- **SBCL 2.6.0** to run the differential tests and to regenerate fixtures.
  Tests that need it are skipped when `sbcl` is not on `PATH`.

Nothing needs to be installed to run Numbo: run it from this directory, or
put this directory on `PYTHONPATH`. An editable install also works:

```text
python3 -m pip install -e python        # from Software/Numbo/numbo-sbcl/
```

## Running a puzzle

Puzzle 1 of the chapter: reach 114 from the bricks 11 20 7 1 6. From
Software/Numbo/numbo-sbcl/:

```sh
cd python
python3 -m numbo 114 11 20 7 1 6 --seed 1
```

The output ends with:

```
Done : Operation PLUS114-6-V2 has been applied
to CYTO-TARGET-6-V2 ( 6) and to CYTO-BLOCK120-V1 ( 120)
to get CYTO-TARGET
Operation PLUS6-1-V3 has been applied
to CYTO-BRICK3 ( 7) and to CYTO-BRICK4 ( 1)
to get CYTO-TARGET-6-V2
Operation TIMES20-6-V1 has been applied
to CYTO-BRICK5 ( 6) and to CYTO-BRICK2 ( 20)
to get CYTO-BLOCK120-V1
outcome: solved, 45 iterations (seed 1)
check: valid: 114 = (6 x 20) - (7 - 1)
```

Everything up to "to get CYTO-BLOCK120-V1" is what the 1987 `config` prints,
character for character as the SBCL port prints it in oracle mode. The last
two lines are the CLI's summary. The first gives the outcome and the number
of main-loop iterations. The second gives the solution checker's verdict.
Read 114 = (6 x 20) - (7 - 1) as the chapter's Fig. III-3: 20 x 6 - (7 - 1).

Options:

| Option | Meaning |
|---|---|
| `--seed N` | seed of the shared RNG (default 1). The same seed gives the same run, in Python and in the oracle. |
| `--max-iterations N` | stop after N main-loop iterations (default 20000), or `none` for no cap |
| `--quiet` | print only the two summary lines |
| `--verbose` | set `%verbose%`: print the "About to post codelet" lines, as in trace3.31 |
| `--trace FILE` | write the JSON-lines event trace (`src/oracle.lisp`'s format) to FILE |
| `--rng-events` | add every RNG draw to the trace |

The outcome is `solved` ("Done :" was printed), `gave-up` (the coderack was
empty after the last retry), `capped`, or `error` (a Lisp error, with its
message). The exit status is 0 when the run printed a valid solution and 1
otherwise.

"Done :" is not always a valid solution. Puzzle 3 with seed 8 runs into the
1987 `kill-block` gap: its decomposition goes through a block that was killed
earlier, and the checker says so:

```sh
cd python
python3 -m numbo 31 3 5 24 3 14 --seed 8 --quiet; echo "exit status $?"
```

```
outcome: solved, 225 iterations (seed 8)
check: invalid: CYTO-BLOCK11-V5 (11) is used but never derived, and is not a brick
exit status 1
```

### From Python

`numbo.harness.run_config` is the port of `src/harness.lisp`'s `run-config`.
`numbo.solution_checker.check_solution` is the port of
`src/solution-checker.lisp`:

```sh
cd python
python3 -c '
import io
from numbo import harness, solution_checker
out = io.StringIO()
problem = [6, 3, 3, 17, 11, 22]
print(harness.run_config(problem, seed=1, max_iterations=500, out=out))
print(solution_checker.check_solution(out.getvalue(), problem))
'
```

This prints:

```
{'outcome': 'solved', 'iterations': 30, 'seed': 1, 'problem-solved': 1, 'error': None}
(True, None, '6 = 3 + 3')
```

`run_config(problem, seed=1, max_iterations=500, verbose=False, trace=None,
rng_events=False, out=None)` makes each run in a fresh `World`, the object
that holds the 1987 globals. `trace` is a file name or a text stream.

## Tests

From Software/Numbo/numbo-sbcl/:

```text
./tests/run-tests.sh                 # everything: the Lisp tests, then pytest
python3 -m pytest python/tests -q    # the Python tests only (about 30 s)
```

The Python tests compare the Python with the oracle at every level:

| Test file | What it checks against the oracle |
|---|---|
| `test_rng.py`, `test_franz.py`, `test_franz_census.py` | the shared RNG, bit for bit; the Franz Lisp semantics (truncating `/`, `mod` = rem, `(max)` = 0, `sortcar`, print names), on 1090 oracle cases |
| `test_pnet_def.py`, `test_pnet_functions.py` | the 88 pnodes, field by field; spreading activation and the posts after 1, 2 and 10 cycles. Doubles must be equal, with no epsilon. |
| `test_coderack.py`, `test_cyto_def.py` | coderack picks and draws; the cytoplasm methods |
| `test_codelets_a.py`, `_b.py`, `_c.py` | each codelet on World states captured from real runs: the full state after the call, output, RNG draws and globals. All 53 `codelets.lisp` functions are covered. |
| `test_main_loop.py` | full event streams of 7 runs |
| `test_full_runs.py` | **11 puzzles × seeds 1–20, plus 2 error seeds.** Each run's event stream (with RNG draws), printed text, outcome and checker verdict must equal the oracle's, which is run in a fresh SBCL process. It also rebuilds `RESULTS.md`'s tables from the oracle's runs. |
| `test_solution_checker.py` | the checker on 126 oracle cases |
| `test_cli.py`, `test_readme.py` | the CLI's output and trace against the oracle's; every `sh` block of this README, run from a fresh shell |

## Fixtures

`python/fixtures/*.json` (18 MB, committed) are written by the capture
scripts `tests/oracle/*.lisp`, which load the SBCL port in oracle mode.
Regenerate them with:

```text
python/scripts/regen_fixtures.sh            # into python/fixtures/
python/scripts/regen_fixtures.sh /tmp/fx    # or anywhere else
```

`test_fixtures_current.py` regenerates them into temporary directories on
every test run. It checks that two regenerations are byte-identical, and that
they equal the committed files. A fixture that no longer matches the oracle
is an error, not something to update by hand.

Full traces are too big to commit. The full-run test makes them on the fly:
`tests/oracle/lib/full-run.lisp` runs one oracle run in a fresh SBCL process.
For a sweep by hand:

```text
python3 python/tests/full_runs.py --puzzles 1-11 --seeds 1-20   # Python vs. oracle
python3 python/scripts/chapter_runs.py --write                  # regenerate RESULTS.md
```

## How the translation was verified

It was built in 13 test-first steps (`iterations.md`; the log is
`PROGRESS.md`). Each step first captured the oracle's behaviour as fixtures
and wrote failing tests, then wrote the Python until they passed. Mutation
controls followed: deliberate bugs, such as Python `%` for `mod`, a local
variable for a 1987 global, or fixing the `kill-block` gap, must each make a
test fail.

- **Oracle mode** (`src/oracle.lisp`, opt-in) makes the SBCL port
  comparable. It reads the sources with double floats, replaces `random` with
  the shared splitmix64 RNG, makes `sortcar` copy before sorting, and writes
  the JSON-lines trace. Default mode is unchanged.
- **Trace equality is the criterion.** All 220 chapter runs (184 solved, 34
  gave up, 2 capped, up to 65706 events each) and the two error runs have
  equal event streams, printed texts and verdicts. The comparison is of
  parsed JSON, where 3 and 3.0 differ.
- **Results**: [`RESULTS.md`](RESULTS.md) has the 11 puzzles × 20 seeds and
  compares them with `src/RESULTS.md` (default mode, another RNG).

## Layout

| Module | Port of |
|---|---|
| `numbo/franz.py` | `src/franz-compat.lisp`: the Franz Lisp built-ins (symbols, print names, division, `sortcar`, ...) |
| `numbo/flavors.py` | `send` from `src/flavors-compat.lisp` |
| `numbo/rng.py` | the shared RNG of `src/oracle.lisp` |
| `numbo/world.py` | the 1987 globals (`src/globals.lisp`), as a `World` object; symbol values and plists |
| `numbo/pnet_def.py`, `numbo/pnet_functions.py` | `pnet-def.lisp` (generated from it by `scripts/gen_pnet_def.py`), `pnet-functions.lisp` |
| `numbo/cyto_def.py`, `numbo/codelets.py` | `cyto-def.lisp`, `codelets.lisp` |
| `numbo/coderack.py` | `coderack.lisp` (RECONSTRUCTED in the port) |
| `numbo/init.py`, `numbo/start.py` | `init.lisp`, `start.lisp` (`init-chiffre`, `config`) |
| `numbo/harness.py` | `harness.lisp` (`run-config`, the iteration cap) |
| `numbo/trace.py` | the trace hooks of `oracle.lisp` |
| `numbo/solution_checker.py` | `solution-checker.lisp` |
| `numbo/__main__.py` | the CLI (no Lisp counterpart) |

## Known limitations

- **Oracle mode only.** The Python has no default mode. SBCL's own
  `random` (MT19937 with SBCL's seeding) is not reproduced, so the Python
  cannot replay `src/README.md`'s default-mode runs, such as puzzle 3 with
  seed 18 in 1328 iterations. Its runs are the oracle-mode ones.
- **The 1987 flaws are kept.** The `kill-block` gap can print an invalid
  "Done :" (2 of the 220 chapter runs; the checker flags them). The
  `reactivate-cyto` race can end a run with an error at iteration 29: in
  oracle mode, puzzle 1 seeds 40 and 323, and 2 more of seeds 21–400.
- **Error messages.** The two errors real runs reach have SBCL's texts:
  "SEND: NIL does not handle the message ..." and "The variable X is
  unbound.". Other Lisp errors, which no chapter run reaches, carry Python's
  messages.
- **No graphics.** `WINDOW_GFX` must be unset or empty. With it set, the
  graphics calls raise `GraphicsNotPorted`.
- **Fresh state per run.** Each run gets a fresh `World`, as each oracle run
  gets a fresh SBCL process. Several `config` calls in one SBCL image can
  see state left by earlier ones (see `RESULTS.md`). The Python does not
  model that.
- **Speed.** About 20000 iterations per second; the 220 chapter runs take a
  few seconds on a multi-core machine.
