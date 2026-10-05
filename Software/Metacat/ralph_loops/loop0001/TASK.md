# TASK: Metacat in modern Scheme — a faithful Racket port with a racket/gui interface

## Philosophy
- **The original is the specification.** `chez_scheme/original/` is James Marshall's Metacat 1.2
  (Chez Scheme + SWL, GPL), imported unchanged in commit `9f072c0`. It is never edited;
  the gate checks this. Where the port and the original disagree, the original is right
  unless a divergence is written down in `docs/divergences.md` with the reason.
- **Faithful first, idiomatic later.** Port the model code as closely as possible: same
  names, same message-passing objects (`tell`), same order of random draws, same
  arithmetic. Rename, restructure or modernise only where Racket forces it, and record
  each such change in `docs/porting-notes.md`. Clean-ups belong to a later loop.
- **An oracle, not eyeballing.** The original runs headless under Chez Scheme 10 in
  `chez_scheme/oracle/`, loading the files from `chez_scheme/original/` *unmodified* through a prelude that stubs
  SWL and swaps in a portable seeded PRNG. Golden traces from the oracle live in
  `tests/golden/`. The Racket engine must reproduce them event for event (codelets run,
  structures built and broken, temperature, answers, commentary). Golden files are only
  ever produced by the oracle, never edited by hand, and never regenerated to make a
  failing port pass.
- **The engine knows nothing about the GUI.** The Racket model code does not require
  `racket/gui`. Graphics hooks (the SGL drawing language and the `*display-mode?*`
  style switches the original uses) go through an interface that a headless run leaves
  empty. Watching a run must never change its course: no RNG draws, no state changes.
- **Tests first.** For each item, write the failing test, then the code. Record in
  PROGRESS.md which tests were written first and that they failed before the code.
- **Look at what you draw.** For every GUI panel, render it to a PNG offscreen and inspect
  the image with the Read tool. Compare with the figures in Marshall's dissertation (item
  12 fetches them). Describe in PROGRESS.md what you saw and fix layouts that look wrong.
- **Small, verifiable steps.** If something can't be verified, it isn't done.
- **Log the strange.** Anything surprising goes in `docs/anomalies_and_quirks.md`, in its
  entry format: bugs in the original, runs that behave oddly, Chez/Racket quirks, hidden
  couplings, things nobody can explain yet. Add it when you see it, even if it turns out
  to be nothing, and update the entry's status when it's explained.

## Current Focus
`racket racket/main.rkt` (or the `metacat` executable) opens a window where you
type a problem such as `abc abd xyz`, pick a seed, press Run, and watch the Workspace,
Slipnet, Coderack, Temperature, Themespace, Trace, Memory and Commentary panels as Metacat
works — the same views as the original SWL program. `racket racket/cli.rkt abc
abd xyz --seed 7` runs headless and prints the answers and commentary.

## Target Problems (in order)
See `iterations.md`. Work on the first item not marked `[x]` (done) or `[!]` (blocked).

## Acceptance Criteria (per item)
- [ ] The item's own criteria in `iterations.md` are met
- [ ] Tests for the item were written before the code, and failed without it
- [ ] `python3 ralph_loops/loop0001/gate.py` passes: `chez_scheme/original/` unchanged, and
      `tests/run-tests.sh` green, including every golden-trace equivalence run that
      exists so far
- [ ] No regressions

## Completion Conditions
An item is DONE when either:
1. **Solved**: Meets all criteria, OR
2. **Blocked**: Documented in PROGRESS.md with specific blockers (what was tried, exact
   error output, what is needed to proceed)

The loop is complete when every item in `iterations.md` is DONE or BLOCKED. Only then add
`LOOP_COMPLETE` to PROGRESS.md.

## Context
- **Repo layout** (create directories as items need them):
  - `chez_scheme/original/` — original source, read-only. Load order is in `chez_scheme/original/metacat.ss`.
    Graphics funnel through `sgl-interpreter.ss` (a symbolic drawing language on
    Tk canvases) and `general-graphics.ss`; the widgets and control panel are `gui.ss`;
    toolkit calls (`swl:`, `send`, `make <class>`, threads) are confined to `gui.ss`,
    `sgl-interpreter.ss`, `general-graphics.ss`, `fonts.ss`, `setup.ss`, `run.ss`,
    `constants.ss`, `metacat.ss`.
  - `chez_scheme/oracle/` — Chez Scheme 10 harness: the SWL stub prelude, the portable PRNG, trace
    instrumentation, and the golden-trace generator.
  - `racket/` — the port (modules directly in this folder, GUI in `racket/gui/`): `compat.rkt` (Chez-isms: `extend-syntax` forms as
    `syntax-rules`, `printf`/`format` directives, `1st`/`2nd`, …), the engine modules,
    `cli.rkt`, and `gui/` for everything that requires `racket/gui`.
  - `tests/run-tests.sh` — the single entry point for every test; `tests/golden/` — oracle
    traces; `tests/problems.txt` — the problem × seed list.
  - `docs/` — `porting-notes.md`, `divergences.md`, `trace-format.md`, reference figures.
- **Toolchain**: Racket 8.x (CS) with `racket/gui` and `raco`; Chez Scheme 10 as `scheme`
  (or `chezscheme`). Installing packages needs the owner (`sudo apt install racket
  chezscheme`); if a tool is missing, mark the item blocked and say exactly what to install.
- **Testing**: `bash tests/run-tests.sh`; `raco test racket/`; the gate above.
- **Constraints**: never edit `chez_scheme/original/`; never hand-edit or blindly regenerate
  `tests/golden/`; engine modules never require `racket/gui`; keep the GPL headers on
  ported files and add "ported to Racket" lines.

## Important Notes
- **Randomness is the crux of equivalence.** Metacat draws from Chez's `random` and
  `random-seed` (`utilities.ss`: `randomize`, `prob?`, `random-pick`, `weighted-pick`, …).
  Chez's and Racket's generators differ, so the oracle prelude replaces `random` and
  `random-seed` with a small portable PRNG specified in `docs/trace-format.md`, and the
  port implements exactly the same one. `(random 1.0)` must produce bit-identical doubles.
- **Iteration order is the second crux.** Anything that iterates a hash table, sorts
  with ties, or depends on `eq?` hashing can make runs diverge. Find these as you port and
  reproduce the original's order deterministically; note each one in porting-notes.md.
- **`extend-syntax`** is the old Kohlbecker/Dybvig macro system (keywords listed in the
  first form). Chez 10 doesn't ship it; the oracle prelude must provide it (a `syntax-case`
  implementation, as in the Chez Scheme examples) and the port rewrites the 22 uses in
  `syntactic-sugar.ss` as `syntax-rules`.
- **Threads.** The original uses SWL threads only for a window-resize listener and to
  interrupt the REPL thread from GUI buttons (`thread-break *repl-thread*`). In the port
  the engine runs in its own Racket thread and the GUI talks to it by messages; Step,
  Run, Pause and Stop must work without races.
- **GUI tests run on a virtual display, never the owner's screen.** `xvfb-run` is
  installed: run every test or script that opens a `racket/gui` window as
  `xvfb-run -a racket ...`, including from `tests/run-tests.sh`. A GUI script must
  exit by itself (`(exit 0)` or closing its frames and returning from the
  eventspace), otherwise it hangs the gate. Offscreen rendering with `racket/draw`
  bitmaps needs no display at all.
- Marshall's dissertation (`https://science.slc.edu/~jmarshall/metacat/dissertation.pdf`)
  describes every panel and the intended behaviour; use it when the code is unclear.
- Do not commit or push yourself; the driver commits after the gate passes and pushes to `origin` (git@github.com:fargonauts/metacat.git).
