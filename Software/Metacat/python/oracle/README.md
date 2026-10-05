# `python/oracle/`: capturing Chez's answers

The scripts here run the **unedited original** under Chez Scheme 10 and freeze what it
prints into [`python/fixtures/`](../fixtures/README.md), where the Python tests compare
against it. Every expected value in the Python port's tests came from these captures,
never from the Python code. They all go through the repository's oracle in
[`chez_scheme/oracle/`](../../chez_scheme/oracle/), which loads the 44 original files
through `prelude.ss` (SWL stubs, `extend-syntax`, instrumentation from outside) without
changing them. This folder also holds the Python port's own Chez batteries (`batteries/`),
for cases that the frozen batteries in [`tests/diff/`](../../tests/diff/) don't reach, and
two benchmark scripts.

You need Chez Scheme 10 as `scheme` or `chezscheme` on the PATH. Run everything from the
repository root (the scripts also find it themselves).

## What's here

| File | What it does |
|---|---|
| `capture.py` | Runs a differential battery under Chez through `chez_scheme/oracle/diff-eval.ss` and writes its output, split per test, to `python/fixtures/<battery>/` |
| `list-tests.ss` | Prints a battery's `(test NAME EXPR)` names in order, as Chez's reader sees them. `capture.py` uses it to split the output without guessing |
| `batteries/*.scm` | The Python port's own batteries (below) |
| `capture_extra_seeds.py` | Runs the 720 extra-seed runs in the oracle and freezes each run's outcome into `python/fixtures/extra-seeds/` |
| `capture_sgl_tcl.py` | Records the Tcl command stream of the original's SGL interpreter into `python/fixtures/sgl-tcl/` |
| `sgl-tcl.ss` | The Chez side of that capture: loads the original through the prelude, makes `swl:tcl-eval` record every command (answering text `bbox`es from a fixed metric), reloads the unedited `sgl-interpreter.ss` and draws the fixture on each viewport |
| `sgl-fixture.scm` | The fixture: every SGL form, the tag operations and degenerate shapes, as data. It is read by `sgl-tcl.ss` (Chez) and by `python/tests/test_sgl.py` (Python). The pictures are those of `racket/tests/sgl-fixture.rkt` |
| `count-calls.ss` | Counts `tell` and `delegate` calls in a real oracle run. Item 01 used the count to project the cost of message dispatch in Python |
| `bench_runs.py` | Times every golden run as a process, Python and oracle side by side, checks that both print the same output, and writes the table of [`docs/python-run-times.md`](../../docs/python-run-times.md) |
| `bench_speed.py` | The CPU time of five fixed Python runs, interleaved across copies of `python/`. Item 12's speed-ups were measured with it |

## Capturing batteries

A *battery* is a Scheme file of `(test NAME EXPR)` forms. `diff-eval.ss` evaluates
`tests/diff/helpers.scm`, then the battery, with the whole original loaded. For each test
it prints one record, `NAME => VALUE` (VALUE in `b:canon` notation; it may span lines, or
be `ERROR`). `capture.py`:

1. asks Chez for the test names (`list-tests.ss`) and runs the battery;
2. splits the output into one value per test, and checks that joining the values again
   gives Chez's output byte for byte;
3. writes `MANIFEST`, `NNN-NAME.txt` per test, and `SOURCES` (Chez's version and the
   sha256 of every input), replacing the old directory.

```bash
python3 python/oracle/capture.py --all            # every battery, in parallel (about a minute)
python3 python/oracle/capture.py utilities        # one battery: a name, NAME-battery.scm or a path
python3 python/oracle/capture.py chez             # python/oracle/batteries/chez-battery.scm
python3 python/oracle/capture.py --all --out DIR  # elsewhere (the freshness test does this)
```

A battery name is looked up first in `batteries/`, then in `tests/diff/`. The `sgl` battery
also loads `tests/diff/sgl-chez-setup.ss` between the helpers and the battery, as the
Racket port's runner does.

## The port's own batteries: `batteries/`

`tests/diff/` is frozen; it was written for the Racket port. Where the Python translation
needed Chez values those batteries lack, an item added a battery here. Each one is captured
by `capture.py` like the others, into `python/fixtures/<name>/`, and its Python side is at
the end of the named test file. They follow the frozen batteries' rule: no two
side-effecting subexpressions in one call or one `let`, so that Chez's evaluation order
can't matter.

| Battery | Item | Tests | Python side | What it pins |
|---|---|---:|---|---|
| `chez-battery.scm` | 02 | 63 | `test_chez.py` | Chez itself: the generator's value and state after every draw, exactness and contagion, rounding, libm bits of `sqrt`/`exp`/`log`/`expt`, the printer (`display`, `write`, `format`), `sort`'s results and predicate calls, `map`'s order |
| `utilities-extra-battery.scm` | 03 | 4 | `test_utilities.py` | Ties (a probability equal to the draw) in `prob?` and `stochastic-if*`, `select-extreme` ties, mixed exactness |
| `coderack-extra-battery.scm` | 04 | 2 | `test_coderack.py` | Urgency clipping with flonums, clamp exactness |
| `slipnet-extra-battery.scm` | 05 | 4 | `test_slipnet.py` | `replace-all` stopped midway, `extend`'s length-first order, `number->platonic-number` bounds |
| `workspace-extra-battery.scm` | 06 | 8 | `test_workspace.py` | A group's vertical bridge, relevant descriptions, neighbours among groups, relevance with bonds, the bond-density and activity boundaries |
| `codelet-extra-battery.scm` | 07 | 13 | `test_codelets.py` | Local densities and supports (rounded, not floored), and group-builder flipping several bonds in `map`'s order |
| `bridge-extra-battery.scm` | 08 | 1 | `test_bridges.py` | A coherent vertical bridge whose internal strength stays under 100 |
| `rule-extra-battery.scm` | 09 | 17 | `test_rules.py` | `transcribe-to-english` on hand-made clauses, including the `caddr`-of-`#f` crash, and `get-change-phrase` |
| `trace-extra-battery.scm` | 10 | 3 | `test_golden.py` | What the goldens never reach: concept-mapping importances near the threshold of 65, a group of strength 99, partly active themes spreading to the Slipnet |
| `gui-battery.scm` | 15 | 8 | `test_gui.py` | The widget-free parts of gui.ss and demos.ss: the command-line parser, the Step/Go/Reset decision on each input, the speed slider's settings, the figure titles, the clamp menu's patterns, the demo problems |

## Capturing whole runs: the extra seeds

```bash
python3 python/oracle/capture_extra_seeds.py [--seeds N] [--jobs J] [--out DIR]
```

This script runs the jobs of `tests/extra-seeds.py`, the final audit of the Racket port.
For every problem line of `tests/problems.txt`, it takes the same strings, cap and
keep-going flag, with N (default 20) non-golden seeds drawn from
`random.Random(20261003)`, which gives 720 runs. Each runs once as `scheme --script
chez_scheme/oracle/run.ss ... --trace FILE` in a fresh process. The script keeps, per run,
the exit code, all of stdout, the first line of stderr, and the trace's sha256 and line
count. The traces themselves (about 300 MB) are not kept. The output is
`python/fixtures/extra-seeds/runs.jsonl` plus `SOURCES`.

## Capturing the SGL interpreter's Tcl stream

```bash
python3 python/oracle/capture_sgl_tcl.py [--out DIR]
```

This script runs `scheme --script python/oracle/sgl-tcl.ss python/oracle/sgl-fixture.scm`.
The original's `sgl-interpreter.ss` and `fonts.ss` draw the fixture on each of its
viewports, and every command they send toward Tk is written as a datum, such as
`(tcl v1 create rectangle 0 480 640 0 \x2D;outline (rgb 0 0 0) ...)` (Chez writes the
leading `-` of an option symbol as `\x2D;`) or
`(swl v1 set-background-color! "ivory")`. The output is one `NAME.txt` per viewport
(`v1.txt`, `v2.txt`) plus `SOURCES`. Text sizes come from a fixed metric, the same one
`python/tests/test_sgl.py` uses, so the stream doesn't depend on installed fonts.

## Rules

- Never edit a fixture by hand, and never re-capture to make a failing port pass. A capture
  is only redone when its inputs change, and then the change is in `SOURCES`.
- Each capture is deterministic: re-running it writes the same bytes. The slow tests
  re-capture into a temporary directory and compare (see
  [`python/fixtures/README.md`](../fixtures/README.md#freshness)).
- Don't put any other files in `batteries/`: `capture.py --all` takes every
  `*-battery.scm` there.

## Benchmarks

```bash
python3 python/oracle/bench_runs.py [OUT.md] [--jobs N]     # every golden, Python vs oracle (default 8 at a time)
python3 python/oracle/bench_speed.py [--repeat R] [--jobs J] [DIR ...]   # five fixed runs, CPU time
```

The results and their reading are in
[`docs/python-run-times.md`](../../docs/python-run-times.md).
