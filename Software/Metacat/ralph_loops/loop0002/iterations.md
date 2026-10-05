# loop0002 — Metacat 1.2 to Python, by test-driven translation: items

- [x] **00 Skeleton and the fixture pipeline.** Create `python/` as in TASK.md's layout:
    `pyproject.toml` (package `metacat`, stdlib only, pytest as a test extra), a stub
    `metacat/__init__.py`, `README.md`, and `run-tests.sh`, which runs pytest and fails on
    the first error. Build the pipeline that every later item uses:
    `python/oracle/capture.py BATTERY` runs one of `tests/diff/*.scm` under Chez with the
    whole original loaded (through `chez_scheme/oracle/diff-eval.ss`, without editing it)
    and writes Chez's output, **split per test**, to `python/fixtures/<battery>/`. Add a
    freshness test: re-capturing gives byte-identical files (mark it slow if needed, and
    say where it runs). Capture all batteries now. Tests: the pipeline round-trips; the
    fixture count per battery equals the battery's test count; `run-tests.sh` passes.

- [x] **01 The translation plan.** Write `docs/python-translation-plan.md`, the equivalent
    of numbo's translation audit. Read the Racket port's `compat.rkt`, `utilities.rkt`,
    every `port:` comment in `racket/engine/*.rktl`, and the docs listed in TASK.md. Then
    record:
    - every Chez semantic the engine depends on, and its Python strategy (exactness,
      rounding, the printer, the PRNG, evaluation-order sites with file and line, `map`,
      `sort`, `remq`, `for-each`, one-armed `if`, `case` with single datums, symbols vs
      strings, top-level values created at run time, `eval` on names, continuations);
    - the object representation (`tell`, `delegate`, `record-case`, `base-object`), with
      a micro-benchmark of the candidates;
    - how the original's global top level maps to Python modules, given mutual
      recursion and `set!` of globals across files;
    - the name mapping (`foo-bar?` → `foo_bar_p`, `*x*`, `%x%`, `=x=`, `!`);
    - the order of the work, and the risks ranked.
    Tests: the object-system prototype with its micro-benchmark, as a pytest file.

- [x] **02 Chez semantics: `chez.py`.** The PRNG (test vectors from Chez, including
    `(random 1.0)` doubles and state after every draw), exact rounding, the printer
    (`display`/`write`/`format` with `~a ~s ~% ~n ~~`, flonums, rationals, symbols,
    strings, characters, lists), `map` order, `sort` (results and predicate-call
    sequence), `remq`/`remv`/`remove`, `for-each`'s value, `1+`/`-1+`, and the
    top-level value table. Expected values come from small Chez capture scripts in
    `python/oracle/`. Tests: those vectors, written first.

- [x] **03 Objects, sugar and utilities.** `objects.py` (as decided in item 01),
    `sugar.py` (every macro in syntactic-sugar.ss as a Python function, decorator or
    explicit pattern, with `stochastic-if*` drawing exactly one `(random 1.0)`), and
    `utilities.py` (utilities.ss, function for function). Tests: **every test of
    `tests/diff/utilities-battery.scm`**, translated to pytest against its frozen
    fixture.

- [x] **04 Constants, setup, coderack, descriptions.** Translate the model parts of
    `constants.ss` and `setup.ss`, plus `coderack.ss` and `descriptions.ss`. Tests: every
    test of `coderack-battery.scm` against its fixture (bin selection at every
    temperature, `choose-codelet` over seeds with the RNG state after each choice,
    overflow deletion, deferred posting, …).

- [x] **05 Slipnet and images.** Translate `slipnet.ss` and `images.ss`. Tests: every test of
    `slipnet-battery.scm`; the initial slipnet dump equals Chez's; activation spreading
    and decay over 20 updates match to the last bit.

- [x] **06 Workspace.** Translate `workspace.ss`, `workspace-objects.ss`,
    `workspace-structures.ss`, `workspace-strings.ss`, `workspace-structure-formulas.ss`
    and `formulas.ss`. Tests: `workspace-battery.scm`, and `workspace-dump.scm` for the
    initial workspace of every problem in `tests/problems.txt`.

- [x] **07 Bonds, groups, concept mappings.** Translate `bonds.ss`, `groups.ss` and
    `concept-mappings.ss`. Tests: `codelet-battery.scm` through the Python equivalent of
    `codelet-harness.scm`.

- [x] **08 Bridges and breakers.** Translate `bridges.ss` and `breakers.ss`. Tests:
    `bridge-battery.scm`.

- [x] **09 Rules and answers.** Translate `rules.ss` and `answers.ss` (including
    `transcribe-to-english`'s `caddr`-of-`#f` crash path). Tests: `rule-battery.scm`.

- [x] **10 Themes, justification, trace, jootsing, memory.** Translate `themes.ss`,
    `justify.ss`, `trace.ss`, `jootsing.ss` and `memory.ss`. Include the headless window
    stand-ins the model needs, such as each codelet type's coderack window and memory's
    icon procedure; see porting-notes.md. Tests: whichever battery tests cover them, plus
    golden-trace *prefixes*. With the run loop still missing, drive the engine as the
    Racket port's harness did, and compare each golden up to the first codelet that
    needs unported code.

- [x] **11 Full runs, the trace writer and the CLI.** Translate the headless parts of
    `run.ss`. Add `metacat/trace.py`, which writes trace-format.md byte for byte, and
    `metacat/__main__.py`, which has the same arguments, output and exit codes as
    `chez_scheme/oracle/run.ss`. Tests:
    - **every one of the 109 goldens matches event for event**, run in parallel;
    - the CLI's stdout and exit code equal the live oracle's on the cases
      `racket/tests/cli-test.rkt` covers: an answer, a cap, justify, keep-going, the
      halt run, the crash run and bad arguments;
    - `break`/`go` resumes a stopped run at the same codelet.
    This test stays in run-tests.sh (in the tier the gate runs) for the rest of the
    loop. Record the run time per problem in `docs/python-run-times.md`.

- [x] **12 Extra seeds and speed.** Run every problem with 20 non-golden seeds in the oracle
    and in Python (adapt `tests/extra-seeds.py` into `python/tests/`) and require
    identical traces on all 720 runs. Then profile. Make only speed-ups that keep every
    trace identical (for example, `__slots__`, cheaper `tell` dispatch, avoiding repeated
    list copies), and record each one with its gain. Settle the test tiers so the gate
    stays under about 15 minutes.

- [x] **13 The SGL interpreter on tkinter.** Translate `sgl-interpreter.ss` and `fonts.ss`
    to `metacat/gui/sgl.py`, drawing on a `tkinter.Canvas` with the same items, tags,
    dashes, anchors and fonts that the original sent to Tk. Tests:
    - `sgl-battery.scm` against its fixture;
    - the canvas operations for a fixture of every SGL form equal the Tcl command
      stream the original emits for the same forms, captured from the oracle's
      `swl:tcl-eval` stub by a script in `python/oracle/`;
    - render the fixture to PNG (Tk `postscript` or a screen grab under Xvfb),
      inspect it, and compare with `racket/tests/snapshots/sgl-fixture.png`.

- [x] **14 The panels.** Translate `general-graphics.ss` and every `*-graphics.ss` (workspace,
    bridges, groups, rules, slipnet, coderack, temperature, themes, trace, memory,
    commentary, EEG) as views that subscribe to engine hooks. Tests:
    - `graphics-battery.scm` and `panels-battery.scm` against their fixtures;
    - render every panel at several points of run7 (`abc abd xyz` 3852097033) and one
      justify run, inspect the PNGs, and compare with `docs/screenshots/`;
    - a run with all views attached still matches its golden.

- [x] **15 The control panel and windows.** Translate `gui.ss` to tkinter: the control panel
    (command line, Go, Step, Stop, Reset, speed), the menus (Demos with
    `demos.ss`'s runs, Windows, Options, Memory, Help, Save commentary), and window layout
    and resizing. The engine runs so the GUI stays responsive: a worker thread with a
    queue, or `after`-driven stepping. Choose one and record why. Tests, under
    `xvfb-run`: drive the GUI through a full run, step mode, stop/restart and a demo;
    screenshot the whole screen and inspect it; the GUI run's trace equals the golden.

- [x] **16 Packaging and docs.** `python3 -m metacat` (CLI) and `python3 -m metacat.gui`
    work from a clean checkout, and `pip install -e python` installs a `metacat`
    command. Write `python/README.md` (with screenshots) and add the Python port to the
    top-level README.md. Tests: install into a fresh venv and run `abc abd xyz` there.

- [x] **17 Final audit.** Re-run everything: the 109 goldens, the 720 extra seeds, and the
    GUI checks. Confirm no engine module imports tkinter. Re-read the translation plan
    against the code; list the remaining `# chez:` and `# 1.2:` sites in the plan;
    update `docs/anomalies_and_quirks.md` and `docs/follow-ups.md` (a Python section).
    Do this all in the foreground and finish the PROGRESS entry yourself.
