# tests/: the repository's test entry point, golden traces and differential batteries

> **FARGonautica's copy has no `tests/golden/`.** Regenerate the 109 traces with
> `scheme --script chez_scheme/oracle/make-golden.ss`, or get them from
> [github.com/fargonauts/metacat](https://github.com/fargonauts/metacat/tree/main/tests/golden).

This folder holds the shared test infrastructure for checking the ports against the
original Metacat. [`run-tests.sh`](run-tests.sh) runs every test of the Racket port and
every check of the Chez oracle. [`problems.txt`](problems.txt) lists the 36 problems and
109 seeded runs that serve as the reference. [`golden/`](golden/) holds the 109 JSON-lines
traces of those runs, recorded from the unmodified original by the
[Chez oracle](../chez_scheme/oracle/README.md). [`diff/`](diff/) holds the differential
batteries, smaller file-by-file comparisons that the oracle evaluates and that both
ports must match. The remaining scripts are tools outside the suite: an extra-seed
audit, a timing benchmark and a screenshot helper. `golden/` and `diff/` are described
here rather than in READMEs of their own, because tests check that those folders hold
exactly the expected files.

| Path | What it is |
|---|---|
| `run-tests.sh` | the single entry point: Racket tests, GUI tests on a virtual display, Chez oracle checks |
| `problems.txt` | the problem × seed list behind the golden traces (and the extra seeds and the benchmark) |
| [`golden/`](#golden-the-109-golden-traces) | 109 golden traces, `*.jsonl`, about 40 MB, produced only by the oracle |
| [`diff/`](#diff-the-differential-batteries) | 10 differential batteries and 4 helper files, evaluated by Chez and by both ports |
| `extra-seeds.py` | the oracle against the Racket port on 20 non-golden seeds per problem line (720 runs) |
| `bench-runs.rkt` | run times of the Racket CLI against the oracle, per problem |
| `gui-screenshot.rkt` | a screenshot of the Racket GUI after a run, on a virtual display |
| `compiled/` | Racket's build output for the two `.rkt` files (ignored by git) |

## `run-tests.sh`

```bash
bash tests/run-tests.sh      # about 9 minutes; stops at the first failure
```

The script works from the repository root (it `cd`s there itself) and runs, in order:

1. `raco make` on every `.rkt` file under `racket/`, so that no stale `.zo` file is
   tested (plain `racket` and `raco test` don't recompile a module's dependencies).
2. `raco test racket/`: the Racket port's tests, including the golden tests and the
   differential tests, which run Chez live.
3. `raco test racket/gui-tests/*.rkt` under
   `env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a -s "-screen 0 1920x1200x24"`. The
   GUI tests only ever open windows on a virtual X display. `WAYLAND_DISPLAY` is unset
   because GTK would otherwise prefer Wayland and draw on the real screen.
4. Every check in [`chez_scheme/oracle/tests/`](../chez_scheme/oracle/tests/README.md),
   each with `scheme --script` (or `chezscheme`).

It needs Racket 8 with `racket/gui`, Chez Scheme 10, Python 3, `xvfb-run`, and the tools
the GUI tests use (`xwininfo`; optionally `bwrap`). The Python port has its own entry
point, `bash python/run-tests.sh` (see [`python/README.md`](../python/README.md)). The
regression gate `python3 ralph_loops/loop0001/gate.py` runs this script after checking
that `chez_scheme/original/` is untouched.

## `problems.txt`

One problem per line. `#` starts a comment, and blank lines are skipped:

```
INITIAL MODIFIED TARGET [ANSWER] | SEED ... | CAP [| keep-going]
abc abd xyz              | 3852097033 3009318743 2006188493 692549763 | 10000 # run7, fig5.10, fig5.11, misc9
abc aabbcc kkjjii        | 912835776 1 2                           | 1500  | keep-going  # misc3
```

- A fourth string makes it a **justify** run (Metacat explains the given answer).
- `SEED ...`: one golden run per seed. A strings-and-seed pair may appear only once.
- `CAP`: the codelet cap (`run.ss --max-codelets`). A run ends at its first answer or
  give-up (where the original pauses for Go), at the cap, or when the original halts.
- `keep-going`: continue past answers until the cap (`run.ss --keep-going`).

The 36 lines give **109 runs**. 25 lines are the problems of the original's `demos.ss`:
the sample runs of section 5.2 (run1–run8), the answer-comparison families of 5.2.3,
figures 5.4–5.11 and misc1–misc8. Their documented seeds come first, then small seeds
that make 3–5 runs per problem. The other 11 lines are classic problems discussed in the
dissertation (`abc→abd` with `mrrjjj`, `ijk`, `iijjkk`, `kji`, `kkjjii`; `rst→rsu; xyz`;
`xqc→xqd; mrrjjj`; `eqe→qeq; abbba`; `apc→abc; opc`; `abc→ccbbaa; ijk`;
`abc→aabbdd; ijkl`), with seeds 1–3. Caps are 10,000 codelets, except 17,000 for
`eqe qeq abbba aaabaaa` and shorter caps for the keep-going runs misc3–misc5. Comments in
the file mark two cases. `eqe qeq abbba aaabaaa` seed 3 makes the original halt
(`report-error-and-halt` in `answer-justifier`), and that halt is part of the golden set.
`abc ccbbaa ijk` uses seeds 1, 2 and 4, because seed 3 raises a Chez error in the
original (`transcribe-to-english`, rules.ss).

Readers of the file: `chez_scheme/oracle/make-golden.ss`,
`chez_scheme/oracle/tests/workspace-init-check.ss` and `tests/diff/workspace-dump.scm`;
the Racket tests `golden-test.rkt`, `demos-test.rkt` and the codelet-level diff tests;
the Python tests; `extra-seeds.py` and `bench-runs.rkt`.

## `golden/`: the 109 golden traces

Each file is the complete [JSON-lines trace](../docs/trace-format.md) of one run of
`problems.txt` by the unmodified original. It records every codelet chosen (with its
urgency and the generator state), every structure built and broken, every temperature,
Slipnet and Themespace update, every Temporal Trace event, answer and Commentary
paragraph, and the end of the run.

- **Naming**: the strings joined by `-`, then `_` and the seed:
  `abc-abd-xyz_3852097033.jsonl` (Run 7), `abc-abd-xyz-wyz_3100511611.jsonl` (a justify
  run), `a-b-z_3861033416.jsonl`. The largest files are the 17,000-codelet runs of
  `eqe-qeq-abbba-aaabaaa`. In all, the goldens contain 272,957 codelets.
- **How they are made**: only by
  `scheme --script chez_scheme/oracle/make-golden.ss`. That script runs
  `chez_scheme/oracle/run.ss ... --trace` once per run, each in a fresh Chez process,
  in parallel, and rewrites the folder.
- **How they are checked**:
  - against the oracle: `chez_scheme/oracle/tests/golden-check.ss` regenerates them
    all into a temporary directory (`make-golden.ss --check`), requires byte-for-byte
    equality with no file missing or extra, and validates each with
    `chez_scheme/oracle/validate-trace.py`;
  - against the Racket port: `racket/tests/golden-test.rkt` (with `golden-harness.rkt`
    and `golden-pool.rkt`) runs every golden in a fresh engine and requires the same
    bytes. It also checks that `problems.txt` lists exactly the files in `golden/`;
  - against the Python port: `python/tests/test_golden.py` (with `golden_harness.py`)
    does the same.
- **Never hand-edit them, and never regenerate them to make a failing port pass.** A
  difference means the port is wrong, or (after a change to the oracle's
  instrumentation) that the instrumentation changed and the docs must say so. The Python
  port's gate (`ralph_loops/loop0002/gate.py`) also requires `golden/` and `diff/` to be
  identical to the commit where the Racket port was finished.

## `diff/`: the differential batteries

A battery is a file of Scheme forms. `(test NAME EXPR)` prints `NAME => <canonical
value>` (or `NAME => ERROR`), and any other form is just evaluated. Each battery is
evaluated three times, and the outputs must be identical line for line:

- **by Chez with the whole original loaded**:
  `scheme --script chez_scheme/oracle/diff-eval.ss tests/diff/helpers.scm tests/diff/NAME-battery.scm`;
- **by the Racket port**: `racket/tests/diff-runner.rkt` evaluates it in a namespace of
  `racket/base` plus the port's modules, runs Chez live for the other side, and compares;
- **by the Python port**: `python/oracle/capture.py` captures Chez's output once per
  test into `python/fixtures/NAME/`. The Python tests rebuild each battery test in
  Python and compare its canonical value with the fixture. Some also check that every
  test name in the battery is covered; `test_utilities.py` does, for example.

Battery forms follow two rules, so that the two *evaluators* cannot differ. No two
side-effecting subexpressions may appear in one call or one `let`, because Chez does
not evaluate them left to right. Lists passed to `map` must be computed (`b:iota`,
`b:copy`), never literal, because Chez inlines `map` over short constant lists in a
different order. Parts of the model that were not ported yet when a battery was written
are replaced by logging fakes, in both runners, through `b:set-global!`.

| Battery | Lines | What it compares | Racket test | Python test |
|---|---:|---|---|---|
| `utilities-battery.scm` | 653 | `racket/compat.rkt` (Chez-isms: `random`, `sort`, `map` order, `remq`, exact rounding, number printing, …) and `utilities.ss`: the object system, list and table helpers, every random helper | `utilities-diff-test.rkt` | `test_utilities.py`, `test_chez.py`, `test_object_prototype.py` |
| `coderack-battery.scm` | 698 | `constants.ss`, `setup.ss`, `coderack.ss`, `descriptions.ss`: urgencies, posting and choosing codelets, against fake Workspace, Themespace, Trace and Slipnet | `coderack-diff-test.rkt` | `test_coderack.py` |
| `slipnet-battery.scm` | 807 | `slipnet.ss` and `images.ss`: nodes, links, activation updates, images | `slipnet-diff-test.rkt` | `test_slipnet.py` |
| `workspace-battery.scm` | 936 | `workspace.ss`, `workspace-objects.ss`, `workspace-structures.ss`, `workspace-strings.ss`, `workspace-structure-formulas.ss`, `formulas.ss`; mainly the initial workspace of every problem and seed in `problems.txt`, with the generator state afterwards | `workspace-diff-test.rkt` | `test_workspace.py` |
| `codelet-battery.scm` | 224 | `bonds.ss`, `groups.ss`, `concept-mappings.ss`, codelet by codelet: for every problem and seed, a trace of the first codelets of a run with only bond and group codelets enabled | `codelet-diff-test.rkt` | `test_codelets.py` |
| `bridge-battery.scm` | 182 | `bridges.ss`, `breakers.ss`: the same harness with bridge and description scouts and the breaker on (bridges and their concept mappings, descriptions, Themespace boosts), then longer runs | `bridge-diff-test.rkt` | `test_bridges.py` |
| `rule-battery.scm` | 328 | `rules.ss`, `answers.ss`: the same harness with rules and answers, up to each run's first answer (rules built and broken, snags, the answer, its commentary), then rules applied and translated directly | `rule-diff-test.rkt` | `test_rules.py` |
| `sgl-battery.scm` | 187 | the SGL interpreter (`sgl-interpreter.ss`) against the ports' renderers: every viewport message for expressions covering every form and every `let-sgl` binding, nested origins, erasing and tags | `sgl-diff-test.rkt` (with `sgl-recorder.rkt`) | `test_sgl.py` |
| `graphics-battery.scm` | 344 | the picture-expression builders of `general-`, `group-`, `bridge-` and `rule-graphics.ss`, every coordinate to the last bit, against a recording Workspace window | `graphics-diff-test.rkt` | `test_graphics.py` |
| `panels-battery.scm` | 418 | the panel code that works without a window (`slipnet-`, `coderack-`, `temperature-`, `theme-`, `trace-`, `memory-`, `commentary-`, `eeg-graphics.ss`): thermometer, event and memory icons, Themespace layout, mouse handlers, the EEG object | `panels-diff-test.rkt` | `test_panels.py`, `test_graphics.py` |

Helper files, which are not batteries:

| File | Lines | Role |
|---|---:|---|
| `helpers.scm` | 67 | shared by every battery and evaluated first: `b:canon` (the canonical printed form of a value), `b:seeded`, `b:iota`, `b:copy`, … Each runner adds `b:capture` and `b:set-global!` |
| `codelet-harness.scm` | 465 | the codelet-level harness loaded by the codelet, bridge and rule batteries: `b:run-codelets` is a copy of `run.ss`'s main loop with only some codelet types enabled, recording one trace line per event (the Python side is `python/tests/codelet_harness.py`) |
| `workspace-dump.scm` | 236 | `b:init-problem` (a copy of the Workspace part of `init-mcat`) and `b:dump-workspace`; loaded by `workspace-battery.scm` and by `chez_scheme/oracle/tests/workspace-init-check.ss`, which checks that the copy matches the real `init-mcat` |
| `sgl-chez-setup.ss` | 50 | the Chez side of the SGL battery, evaluated between `helpers.scm` and `sgl-battery.scm`: redefines the prelude's `send` to record viewport messages and reloads the original's `sgl-interpreter.ss`, unmodified, against it |

`diff/` is frozen. The Python port's extra Chez vectors are in
`python/oracle/batteries/` (`chez-battery.scm`, `*-extra-battery.scm`) and are captured
the same way.

## Tools outside the suite

**`extra-seeds.py`**: the final equivalence audit of the Racket port.

```bash
python3 tests/extra-seeds.py              # 720 runs, about 2.5 min on 32 cores
python3 tests/extra-seeds.py --seeds 1    # 36 runs
python3 tests/extra-seeds.py --keep DIR   # keep the traces in DIR
```

For each line of `problems.txt`, with the same strings, cap and keep-going flag, it draws
`--seeds` (default 20) seeds that are not golden seeds from a fixed generator
(`random.Random(20261003)`), so a rerun repeats the same runs. Each run goes through
`chez_scheme/oracle/run.ss --trace` and `racket racket/cli.rkt --trace` in fresh
processes. The two traces, the two outputs and the exit codes must be byte-identical.
`--jobs J` sets the parallelism (default: the number of CPUs; it runs J/2 pairs at once).
Nothing is written to `golden/`. It exits 0 when every run agrees. The last result,
720 of 720 identical, is in [`docs/extra-seeds.md`](../docs/extra-seeds.md). The Python
port covers the same 720 runs with `python/tests/test_extra_seeds.py`, against oracle
results frozen by `python/oracle/capture_extra_seeds.py`.

**`bench-runs.rkt`**: run times.

```bash
racket tests/bench-runs.rkt [OUTPUT.md]
```

This runs every golden run once with `racket racket/cli.rkt` and once with the oracle,
one process at a time (use an idle machine), and checks that both print the same output.
It also measures each program's startup separately (a run with `--max-codelets 1`, median
of 5). It writes a Markdown table per problem to OUTPUT.md or stdout. The published
numbers are in [`docs/run-times.md`](../docs/run-times.md).

**`gui-screenshot.rkt`**: a screenshot of the whole Racket GUI (control panel and every
window) after a run driven through the control panel. Run it only on a virtual display:

```bash
env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a -s "-screen 0 1920x1200x24" \
  racket tests/gui-screenshot.rkt OUT.png PROBLEM... [--break N] [--speed S]
# e.g.  ... racket tests/gui-screenshot.rkt OUT.png abc abd mrrjjj 1 --break 513
```

It types the problem (strings, then an optional seed) into the command line and presses
Go. It stops at codelet N with `--break` (the Options menu's breakpoint), and otherwise at
the first answer. Then it grabs the screen with Python's PIL (`ImageGrab`). The
screenshots in [`docs/screenshots/`](../docs/screenshots/) were taken this way, for
example `run7-wyz.png`:

![The Racket port's GUI after Run 7 (abc → abd; xyz → ?, seed 3852097033), answer wyz](../docs/screenshots/run7-wyz.png)
