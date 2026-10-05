# loop0001 — Metacat 1.2 to Racket, through the code in load order: items

- [x] **00 Toolchain and skeleton.** Check that `racket` (8.x CS, with `racket/gui`),
    `raco` and Chez Scheme 10 (`scheme` or `chezscheme`) are installed; if any is
    missing, mark this item blocked and write the exact install command. Create
    `racket/` with `info.rkt` and a stub `main.rkt`, `chez_scheme/oracle/`, `docs/`
    (`porting-notes.md`, `divergences.md`), and `tests/run-tests.sh`, which runs
    `raco test racket/` and every oracle check that exists, and fails on the first
    error. Write `docs/code-map.md`: one paragraph per file in `chez_scheme/original/` (what it
    defines, what it depends on, its line count, and whether it touches SWL), so later
    items don't need to rediscover the structure. Test: `run-tests.sh` passes with one
    trivial test in each of Racket and Chez.

- [x] **01 The original, headless, under Chez 10.** `chez_scheme/oracle/prelude.ss` makes the
    files in `chez_scheme/original/` load unmodified in `scheme --script`: `extend-syntax` (as a
    `syntax-case` macro), stub modules for the `swl:*` imports, stub threads,
    `swl:tcl-eval` and the widget classes as no-ops, and the configuration variables
    `metacat.ss` expects. `chez_scheme/oracle/run.ss abc abd xyz --seed N --max-codelets K` sets up
    a problem with the display turned off, runs it, and prints the answers and the
    commentary. Decide the randomness plan here and write it in `docs/trace-format.md`:
    prefer reproducing Chez 10's own `random`/`random-seed` exactly in Racket, so the
    oracle needs no PRNG substitution; if that isn't practical, swap in a specified
    portable PRNG in the prelude. (The seeds in `demos.ss` were chosen under a 1999-era
    Chez generator, so the demos' documented behaviour won't replay either way; say so.)
    Tests: `abc abd xyz`, `abc abd ijk`, `eqe qeq abbbc` and `abc abd mrrjjj` each reach
    an answer with three seeds, and the same seed twice gives identical output.

- [x] **02 Traces and golden files.** Instrument the oracle, from the prelude, by
    wrapping top-level procedures after loading (never by editing `chez_scheme/original/`), to emit
    a JSON-lines trace: codelet run (type, urgency, time step), structure built or broken,
    temperature at each update, slipnet activations every N steps, answers, rule and
    theme events, commentary lines. Specify the format in `docs/trace-format.md`. Write
    `tests/problems.txt` (the problems in `demos.ss`, plus the classic set from the
    dissertation, each with 3-5 seeds and a codelet cap that keeps every run under
    about 30 s) and `chez_scheme/oracle/make-golden.ss`, which writes `tests/golden/*.jsonl`. Commit
    the goldens. Test: regenerating them reproduces the committed files byte for byte.

- [x] **03 The compatibility layer.** `racket/compat.rkt`: everything in
    `syntactic-sugar.ss` as `syntax-rules` (`for*`, `repeat*`, `if*`, `stochastic-if*`,
    `continuation-point*`, `say`, the slipnet link forms, `post-codelet*`,
    `define-codelet-procedure*`, …), Chez `printf`/`format` directives, the PRNG decided
    in item 01, and whatever else the engine needs from Chez that Racket lacks or names
    differently (mutable pairs, `1+`, `add1`, `void`, records, `sort` argument order,
    `assq`/`assoc` on mutable lists, …). Port `utilities.ss` on top of it. Tests:
    differential checks against Chez for every utility, including the random ones
    (`prob?`, `random-pick`, `weighted-pick`, `weighted-select`,
    `bounded-random-partition`), producing the same values for the same seed.

- [x] **04 Constants, setup, coderack, descriptions.** Port `constants.ss` (minus
    graphics constants, which wait for the GUI items), `setup.ss`, `coderack.ss` and
    `descriptions.ss`. Decide here how the engine is organised as Racket modules given
    the original's global, mutually recursive top-level definitions (one module that
    `include`s per-file pieces, or modules with a shared state module), and write the
    reasoning in `porting-notes.md`. Tests: the coderack's bins, urgencies and selection
    match the oracle for the same posted codelets and seed.

- [x] **05 Slipnet and images.** Port `slipnet.ss` and `images.ss`. Tests: the initial
    slipnet (nodes, links, lengths, conceptual depths) dumped from Racket equals the
    oracle's dump; activation spreading and decay over 20 updates from a fixed state
    match to the last bit.

- [x] **06 Workspace objects and strings.** Port `workspace.ss`, `workspace-objects.ss`,
    `workspace-structures.ss`, `workspace-strings.ss`, `workspace-structure-formulas.ss`
    and `formulas.ss`. Tests: for every problem in `tests/problems.txt`, the initial
    workspace (letters, descriptions, initial salience and importance values) matches
    the oracle.

- [x] **07 Bonds, groups, concept mappings.** Port `bonds.ss`, `groups.ss` and
    `concept-mappings.ss`. Tests: the codelet-level differential harness — run the
    oracle and the port side by side for the first K codelets of each problem, with
    only these codelet types enabled if needed, and compare the trace prefix.

- [x] **08 Bridges and breakers.** Port `bridges.ss` and `breakers.ss`. Tests: trace
    prefixes match for longer K; bridges built and broken are identical.

- [x] **09 Rules and answers.** Port `rules.ss` and `answers.ss`. Tests: rules are
    abstracted and applied identically; the first answer on every golden run matches.

- [x] **10 Themes, justification, trace, jootsing, memory.** Port `themes.ss`,
    `justify.ss`, `trace.ss`, `jootsing.ss` and `memory.ss`: the self-watching half of
    Metacat. Tests: theme and trace events match the goldens.

- [x] **11 Full runs and the CLI.** Port `run.ss` (headless parts) and write
    `racket/cli.rkt`. Tests: **every golden run in `tests/golden/` matches event
    for event**, and the CLI prints the same answers and commentary as `chez_scheme/oracle/run.ss`.
    This test stays in `run-tests.sh` for the rest of the loop. Record run time per
    problem against the oracle.

- [x] **12 The SGL interpreter on racket/draw.** Fetch the dissertation PDF into
    `docs/reference/` and extract the screenshots of each panel. Port
    `sgl-interpreter.ss` to draw on a `racket/draw` `dc<%>` (rectangles, arcs, rings,
    polylines, dashed lines, text with justification, `let-sgl` origin/colour/font/line
    width, `erase`, `clear`, `rule`), with `fonts.ss`. Tests: render a fixture of every
    SGL form to PNG offscreen and inspect it with Read; a pixel-level snapshot test
    guards against regressions.

- [x] **13 Workspace, bridge, group and rule graphics.** Port `general-graphics.ss`,
    `workspace-graphics.ss`, `bridge-graphics.ss`, `group-graphics.ss` and
    `rule-graphics.ss` as views that subscribe to the engine's hooks. Tests: render the
    workspace at several points of a golden run to PNG, inspect, and compare with the
    dissertation's figures; a headless run with the views attached still matches the
    goldens (watching changes nothing).

- [x] **14 The other panels.** Port `slipnet-graphics.ss`, `coderack-graphics.ss`,
    `temperature-graphics.ss`, `theme-graphics.ss`, `trace-graphics.ss`,
    `memory-graphics.ss`, `commentary-graphics.ss` and `eeg-graphics.ss`. Tests: as in 13,
    per panel.

- [x] **15 The control panel and windows.** Port `gui.ss` to `racket/gui`: the control
    panel (problem entry, seed, Run, Step, Pause, Stop, Reset, step size, speed), the
    menus (including "Save commentary to file"), the panel windows and their layout, and
    window resizing. The engine runs in its own thread; the GUI drives it by messages.
    Tests: drive the GUI headlessly (or in an `xvfb-run` session) through a full run,
    step mode and a stop/restart; screenshot the whole window and inspect it.

- [x] **16 Demos, packaging, README.** Port `demos.ss` (with the seed caveat from item
    01), build a standalone executable with `raco exe` + `raco distribute`, and write
    `README.md`: what Metacat is, credit to James Marshall and Melanie Mitchell's
    Copycat, the GPL, how to run the GUI and CLI, how the oracle works, screenshots.
    Tests: the distributed executable runs `abc abd xyz` from a clean directory.

- [!] **17 Final audit.** Re-read `docs/divergences.md` and `porting-notes.md` against
    the code; run the full suite; run every golden problem with 20 extra seeds in both
    the oracle and the port and confirm identical traces; check that no engine module
    requires `racket/gui`; list any follow-ups for a second loop (idiomatic clean-up,
    performance, new features).
