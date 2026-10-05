# TASK: Metacat in Python — a test-driven translation, with a Tk GUI

## Philosophy
- **The original is the specification; the oracle decides.** `chez_scheme/original/` is
  Metacat 1.2. Its behaviour is pinned by the Chez oracle (`chez_scheme/oracle/`) and the
  109 golden traces in `tests/golden/`. The Python port must reproduce them *event for
  event*: same codelets, structures, temperatures, answers and commentary, for the same
  seed. "Roughly like Metacat" is not the goal.
- **Test-driven translation.** No Python is written before a failing test pins its
  behaviour, and **every expected value comes from Chez**, never from what the Python
  happens to produce. The Chez differential batteries in `tests/diff/*.scm` already
  exercise every layer (utilities, coderack, slipnet, workspace, codelets, bridges, rules,
  SGL, graphics, panels). For each battery, freeze Chez's output per test into
  `python/fixtures/`, translate each test into a pytest case that sets up the same state in
  Python, and only then translate the code until the case passes. Record in PROGRESS.md
  which tests were written first and that they failed before the code existed.
- **Faithful, not improved.** Reproduce Metacat 1.2 as it is, bugs included. That means
  the crashes and halts listed in `docs/anomalies_and_quirks.md`, Chez's evaluation order,
  `map` order, `sort` algorithm, exact rationals and printer. Fixing anything is out of
  scope. Mark every place where Python reproduces a quirk with a `# chez:` or `# 1.2:`
  comment that cites the doc entry.
- **Readable Python, Scheme-shaped structure.** Use one Python module per `.ss` file
  (`workspace_strings.py` for `workspace-strings.ss`) and one function per Scheme
  definition, with a fixed name mapping, so that the two can be read side by side. Each
  function's docstring names its origin (`bonds.ss: bond-builder`).
- **The Racket port is a worked translation, not the oracle.** `racket/` already solved
  most translation problems: it found every evaluation-order site, the `map` and `sort`
  orders, the Chez printer, and the model/graphics couplings. Its `port:` comments, along
  with `docs/porting-notes.md`, `docs/trace-format.md` and
  `docs/anomalies_and_quirks.md`, are required reading before translating a file. When
  Racket and Chez disagree, Chez wins, but they never have on the goldens.
- **The engine knows nothing about the GUI.** `python/metacat/` model modules never import
  `tkinter`. Views subscribe to engine hooks, and watching a run must never change it: no
  RNG draws and no state changes.
- **Look at what you draw.** Render every panel to PNG offscreen, inspect it with the Read
  tool, and compare with `docs/screenshots/` (the Racket port) and
  `docs/reference/figures/` (the dissertation).
- **Log the strange.** Anything surprising goes in `docs/anomalies_and_quirks.md`, in its
  entry format. That includes Python-specific traps (float formatting, recursion depth,
  dict order).
- **Small, verifiable steps.** If something can't be verified, it isn't done.

## Current Focus
`python3 -m metacat abc abd xyz --seed 3852097033` prints exactly what
`chez_scheme/oracle/run.ss` prints for the same arguments, and `--trace` writes the golden
file byte for byte. `python3 -m metacat.gui` opens the Tk windows (control panel,
Workspace, Slipnet, Coderack, Temperature, Themes, Trace, Memory, Commentary) and runs
problems the way the original's SWL GUI did. The standard library is all it needs.

## Target Problems (in order)
See `iterations.md`. Work on the first item not marked `[x]` (done) or `[!]` (blocked).

## Acceptance Criteria (per item)
- [ ] The item's own criteria in `iterations.md` are met
- [ ] Tests were written before the code, with expected values from Chez, and failed
      without the code
- [ ] `python3 ralph_loops/loop0002/gate.py` passes: the original and the references are
      unchanged, and `python/run-tests.sh` is green, including every golden-trace
      equivalence run that exists so far
- [ ] No regressions

## Completion Conditions
An item is DONE when either:
1. **Solved**: Meets all criteria, OR
2. **Blocked**: Documented in PROGRESS.md with specific blockers (what was tried, exact
   error output, what is needed to proceed)

The loop is complete when every item in `iterations.md` is DONE or BLOCKED. Only then add
`LOOP_COMPLETE` to PROGRESS.md.

## Context
- **Frozen references (never edit; the gate checks them against commit `9684b16`):**
  - `chez_scheme/original/`: the source. Load order is in `metacat.ss`.
  - `chez_scheme/oracle/`: the headless harness. `run.ss` is the CLI, `trace.ss` the
    trace, `diff-eval.ss` evaluates a battery against the loaded original, and
    `make-golden.ss` makes the goldens.
  - `racket/`: the finished, equivalent Racket port.
  - `tests/golden/`: 109 traces.
  - `tests/diff/`: the Chez batteries and helpers.
  - `tests/problems.txt` and `tests/extra-seeds.py` may be read and reused but not edited.
- **Docs to read first:** `docs/code-map.md`, `docs/trace-format.md` (trace format, PRNG
  spec, number formatting), `docs/porting-notes.md`, `docs/anomalies_and_quirks.md`,
  `docs/divergences.md`, `docs/follow-ups.md`.
- **Target layout:**
  ```
  python/
    pyproject.toml             ; package "metacat", stdlib only; pytest for tests
    run-tests.sh               ; the single entry point the gate runs
    README.md
    metacat/                   ; chez.py (Chez semantics), objects.py (tell/delegate),
                               ; sugar.py (syntactic-sugar.ss), one module per .ss file,
                               ; headless.py, trace.py, __main__.py (CLI), gui/ (tkinter)
    oracle/                    ; Scheme + Python scripts that capture fixtures from Chez
    fixtures/                  ; frozen Chez outputs (committed), with a freshness check
    tests/                     ; pytest
  ```
- **Toolchain** (all installed): Python 3.12.13 (`python3`, Anaconda) with pytest 7.4 and
  tkinter (Tk 8.6); Chez Scheme 10 (`scheme`) for capturing fixtures; Racket 8.18 to read
  and run the reference port; `xvfb-run` and 32 cores. Use only the standard library plus
  pytest. Justify any other dependency in PROGRESS.md.
- **Constraints:**
  - never edit the frozen references;
  - never hand-edit fixtures or goldens, or regenerate them to match a broken port;
  - engine modules never import tkinter;
  - keep the GPL headers, and add "translated to Python" lines.

## Important Notes
- **Randomness.** The port must implement Chez 10's global `random`/`random-seed` exactly,
  as specified in `docs/trace-format.md` (a 32-bit LCG). `(random 1.0)` must give
  bit-identical doubles. Test vectors come from Chez.
- **Exact arithmetic.** Urgencies and many model quantities are exact rationals (`102/5`).
  Use `fractions.Fraction` wherever Chez is exact, and floats only where Chez has
  flonums. A stray float changes codelet choices. Chez's `round` rounds half to even, and
  utilities.ss redefines `round`, `floor` and friends to return exact integers.
- **Evaluation order.** Chez evaluates call arguments in an unspecified order (observed
  right to left; `let` right to left; inlined primitives left to right). Python is left to
  right. Every site where order changes the order of random draws or other side effects
  must be written as explicit sequential statements in Chez's order. The Racket port marks
  these sites; find them there.
- **`map`, `sort`, `remq`, `for-each`.** Chez's `map` applies the procedure in a peculiar
  order (pairs from the end; inlined orders at literal lists). Its `sort` is its own merge
  sort with a specific sequence of predicate calls. `remq` removes all occurrences.
  `for-each` returns the last value. Reproduce each one where it matters (see
  `racket/compat.rkt` and porting-notes.md).
- **Printing.** Commentary and traces must match byte for byte, so number formatting has
  to follow Chez's printer (flonum syntax, `"n/d"` rationals in traces, symbol and string
  escapes). Never use `repr`/`str` on a float where Chez's printer is meant.
- **Objects.** About 4,000 `(tell obj 'msg ...)` sites dispatch messages through
  `record-case` closures with `delegate` inheritance. Pick one Python representation in
  item 01 and keep it uniform: `tell` must stay a cheap call. Watch recursion depth too,
  since Scheme recursion that relied on tail calls may need loops in Python.
- **Continuations.** `continuation-point*` is an escape (use exceptions). `break`/`go` in
  run.ss re-enters a run. In Python, structure the run loop so a stopped run can resume
  (a generator or explicit state), keeping the codelet sequence identical.
- **Speed.** Expect Python to be 20–100× slower than Chez (about 0.1 ms per codelet in
  Chez). Full golden runs must therefore run in parallel (`multiprocessing`, 32 cores),
  and the gate must stay under about 15 minutes. Keep a fast tier for every iteration and
  the full suite for the items that need it, and say in run-tests.sh which is which.
- **GUI tests never on the owner's screen.** The desktop is Wayland. Run everything that
  opens Tk windows under `xvfb-run -a` (Tk is X11-only, so xvfb-run's DISPLAY is enough).
  Every GUI script must exit by itself, or it hangs the gate.
- **The Tk choice is deliberate.** The original's SGL interpreter emitted Tk canvas
  commands through SWL. tkinter's Canvas has the same items, tags, dashes and anchors.
  So the Python SGL interpreter can produce the same canvas operations, and a recorded
  Tcl command stream from the Chez oracle (its `swl:tcl-eval` stub) can serve as the
  fixture.
- Do not commit or push; the driver commits after the gate passes and pushes to `origin`.
