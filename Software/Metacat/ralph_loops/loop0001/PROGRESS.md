# Progress Log

## Ralph Loop 0001 Status
- **Started**: 2026-10-02
- **Target**: 18 items
- **Current**: 17/18 SOLVED

---

## Iteration 1 — 2026-10-02 20:34
Item 00 (Toolchain and skeleton): **SOLVED**.

### Completed
- Toolchain present: Racket v8.18 [cs] (`/usr/bin/racket`, `raco`), `racket/gui/base`
  loads; Chez Scheme Version 10.0.0 as both `scheme` and `chezscheme`.
- Skeleton: `racket/info.rkt` (collection `metacat`), stub `racket/main.rkt` (GPL
  header + "ported to Racket" line; provides `metacat-version` and `main`; does not
  require racket/gui yet, `racket racket/main.rkt` prints a not-implemented message),
  `chez_scheme/oracle/README.md`, `docs/porting-notes.md`, `docs/divergences.md`.
- `tests/run-tests.sh`: `set -euo pipefail`; runs `raco test racket/`, then every
  `chez_scheme/oracle/tests/*.ss` with `scheme --script` (falls back to `chezscheme`),
  stops at the first failure, and fails if no Chez check exists. Checked that a
  temporary `(exit 1)` check makes it exit 1.
- Tests written first: `racket/tests/skeleton-test.rkt` (requires `../main.rkt`,
  checks `metacat-version` = "1.2" and `main` is a procedure) and
  `chez_scheme/oracle/tests/reader-check.ss` (Chez version is 10; the Chez reader
  reads every one of the 45 `.ss` files in `chez_scheme/original/`: 1454 top-level
  forms). Before the skeleton existed, `run-tests.sh` failed with
  "cannot open module file ... racket/main.rkt"; the Chez check checks the
  environment and the original, not new code, so it passed from the start.
- `docs/code-map.md`: one paragraph per file in `chez_scheme/original/` (load order,
  definitions, dependencies computed by identifier matching, line counts, SWL use),
  plus general facts for the port: the `tell`/`record-case` object system, every
  RNG entry point and the files that draw, `sort` uses (Chez `(sort pred list)`,
  stable), no hashtables, redefined exact `round`/`floor`/…
- Findings recorded there: besides the files TASK.md lists, toolkit calls also occur
  in `utilities.ss` (`pause` = `thread-sleep`), `workspace-graphics.ss`
  (`thread-break *repl-thread*`) and `theme-graphics.ss` (one `send vp`).
  TASK.md's `weighted-pick` does not exist; the original calls it `stochastic-pick`.
  `constants.ss` also draws (a probability-distribution object). `metacat.ss`
  needs `*platform*`, `*metacat-directory*`, `*file-dialog-directory*` defined
  (commented out in the distribution), so the oracle prelude must provide them.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED.

### Blockers
- None.

### Next
- Item 01 in iterations.md.

---

## Iteration 2 — 2026-10-02 20:52
Item 01 (The original, headless, under Chez 10): **SOLVED**.

### Completed
- `chez_scheme/oracle/prelude.ss`: loads `chez_scheme/original/metacat.ss`
  unmodified (all 44 files) under `scheme --script`. Provides `extend-syntax`
  as a `syntax-case` macro (fenders and `with` evaluated at expansion time
  after substitution), empty `swl:oop`…`swl:threads` modules, inert
  `make`/`create`/`send`/`define-class`, `swl:tcl-eval`, `swl:font-families`,
  screen size, no-op threads and message queues, `*platform*`,
  `*metacat-directory*`, `*file-dialog-directory*`, an unbuffered muteable
  stdout (load chatter hidden; syntactic-sugar.ss's `printf` captures the port
  at load time), strict null windows for a headless run, and an error handler
  that prints a backtrace with source positions.
- `chez_scheme/oracle/run.ss INITIAL MODIFIED TARGET [ANSWER] [--seed N]
  [--max-codelets K] [--keep-going]`: graphics switches off, `init-mcat` then
  `run-mcat` as the Control Panel does; prints `Comment:` lines (the original
  Commentary window drawing on a recording text window), `Answer:` lines
  (answer, quality, codelet count, temperature) and a summary. Stops at the
  first answer/give-up (where the original pauses for Go) unless
  `--keep-going`; `--max-codelets` is the original's own `*break-time*`.
  Justify runs (4 strings) work. A run takes about 1 s.
- **Randomness plan** (`docs/trace-format.md`): keep Chez 10's own
  `random`/`random-seed`, with no PRNG swap. Read from the Chez v10.0.0 C source
  (`c/prim5.c`, `c/number.c`): a 32-bit LCG `S := S*72931 + 90763387 mod 2^32`;
  integers take the high halves of 2 (or 4) steps, `mod n`; flonums build a
  52-bit mantissa from 4 steps. Specified exactly, so item 03 can port it.
  (Chez 10's `make-pseudo-random-generator` is MRG32k3a like Racket's, but
  that is a different generator from the global `random`.)
- **Finding 1, the demo seeds DO replay**, contrary to the expectation in
  TASK.md/iterations.md: misc1 (mmmrrj at 7794), misc2 (abd at 1126), misc4
  (b at 453, y at 945), misc5 (flz, dlz, hlz at 1721) and the commented-out
  misc9 (dyz at 2257) come out exactly as `demos.ss` documents. misc3 and the
  "not used" misc6–8 do not. run1–8/fig5.x still need comparing against the
  dissertation (item 12).
- **Finding 2, evaluation order**: Chez does not evaluate arguments left to
  right (`(f (show 1) (show 2) (show 3))` prints `312`, `let` goes right to
  left, inlined `+`/`cons` go left to right). Racket goes left to right, so
  every multi-argument site with draws or other side effects must be ported
  in Chez's order. Recorded in trace-format.md and porting-notes.md.
- **Finding 3, model state tied to the graphics** (porting-notes.md): codelet
  types hold a private Coderack-window reference that every codelet `run`
  messages; memory descriptions call icon procedures installed by the Memory
  window; `group-graphics 'erase` is called ungated (groups.ss:727). Lists
  every window message a headless run sends.
- Tests, all in `tests/run-tests.sh` (Chez part about 25 s):
  - `chez_scheme/oracle/tests/headless-run-check.ss`: `abc abd xyz`,
    `abc abd ijk`, `eqe qeq abbbc`, `abc abd mrrjjj` × seeds 1, 2, 3 all
    reach an answer (xyz/xyd/yyz, ijd/ijl/ijl, baaaq/qeeeq/qbbbq,
    mrrjjk/mrrjjk/mrrjjjj), and each run repeated gives byte-identical
    output.
  - `chez_scheme/oracle/tests/rng-check.ss`: the spec, written in exact
    arithmetic, matches Chez's `random`/`random-seed` draw for draw and
    state for state; checked that changing a constant or the mantissa mask
    makes it fail (43551 and 7175 failures).
  - `chez_scheme/oracle/tests/demo-replay-check.ss`: the 5 demo runs above.
- Tests-first, honestly: I spiked the prelude first (to find out what loading
  needed) and wrote the checks before `run.ss` was finished. Checked that
  `headless-run-check.ss` fails without `run.ss` (12 failures: "failed for
  chez_scheme/oracle/run.ss: no such file or directory"), and that
  `demo-replay-check.ss` failed at first on misc5/misc9 because of the
  codelet cap semantics (the answer at count K is found by codelet K+1),
  which is now documented in run.ss. The RNG check tests a specification
  against Chez itself, so it passed as soon as the spec was right.
- `chez_scheme/oracle/README.md` updated. `python3 ralph_loops/loop0001/gate.py`:
  GATE PASSED. `chez_scheme/original/` untouched.

### Blockers
- None.

### Next
- Item 02 (traces and golden files). Notes for it: instrument by wrapping
  top-level procedures after `load-metacat` (as run.ss does with
  `abstract-answer-description`, `break`); codelet runs can be seen by
  wrapping each codelet type's procedure or `step-mcat`; the demo seeds
  replay, so `demos.ss` problems with their own seeds are good golden
  candidates. Watch for null-window errors on new problems: the null windows
  are strict on purpose, and any new message must be checked to be a command
  before it is allowed.

---

## Iteration 3 — 2026-10-02 21:09
Item 02 (Traces and golden files): **SOLVED**.

### Completed
- `chez_scheme/oracle/trace.ss`: JSON-lines trace instrumentation, entirely
  from outside the original. It wraps top-level procedures after loading
  (`build-bond/-group/-bridge/-description`, `break-bond/-group/-bridge`,
  `update-temperature`, `update-slipnet-activations`,
  `abstract-answer-description`, `report-error-and-halt`). It forwards
  `*coderack*` (to see each `choose-codelet`) and `*workspace*` (to see
  `add-rule`) through closures that pass the original object as `self`, and
  records Temporal Trace events from the Trace window plus commentary from
  the Commentary hook. Twelve event types: start, codelet (type, urgency,
  posted, rng state), build/break (bond, group, bridge, description, rule),
  temperature, slipnet (all activations every 15 codelets plus rng state),
  themes (when changed), event (answer/snag/clamp/rule/group/
  concept-mapping/concept-activation), answer, comment, halt, end.
- `run.ss --trace FILE`; the summary now also prints `Stopped: suspend|cap|halt`.
- `docs/trace-format.md`: the format, down to field order, string escapes
  and number formatting (exact rationals as `"n/d"`, Chez flonum syntax),
  how `t` counts codelets, what is deliberately left out, and the golden set.
- `tests/problems.txt`: 36 problems, 109 runs. These are the 25 problems of
  `demos.ss` with their documented seeds plus small seeds (3–5 per problem;
  misc3–5 with `keep-going`), and 11 classic problems from the dissertation
  (picked by how often the dissertation text names them) with seeds 1–3.
  Caps are 10000 (17000 for eqe-aaabaaa). Each run takes 1–8 s.
- `chez_scheme/oracle/make-golden.ss` writes `tests/golden/*.jsonl` (one
  fresh Chez process per run, `nproc` in parallel, about 13 s here).
  `--check` regenerates into /tmp and compares byte for byte, and reports
  missing or extra files. Goldens are written and committed: 109 files,
  40 MB uncompressed, 4.8 MB gzipped.
- `chez_scheme/oracle/validate-trace.py`: a structural checker for traces
  (fields and order, types, start/end, one codelet line per codelet,
  activation count, required event types). The Racket port can reuse it.
- Tests written first, and seen failing before the code:
  `chez_scheme/oracle/tests/trace-check.ss` (4 runs, including a justify run
  and a keep-going run: tracing does not change the stdout, the trace is
  valid and has the required event types, and it is byte-identical when
  repeated). Before the code it failed with run.ss's usage error on
  `--trace`. `chez_scheme/oracle/tests/golden-check.ss` (`make-golden.ss
  --check` plus validation of every golden) failed with "make-golden.ss:
  no such file or directory". I also checked that the checks catch
  breakage: one changed rng value in a golden gives "differs: ...
  line 100", an extra golden file gives "1 of 109 do not match", and
  deleting a line gives "codelet event 149 has t = 150". Each golden was
  restored afterwards (`cmp` identical).
- Findings (in porting-notes.md):
  1. the original itself fails on some runs. `eqe qeq abbba aaabaaa`
     seed 3 calls `report-error-and-halt` at codelet 4004, and `(reset)`
     makes `--script` exit 255. run.ss now ends the run like `break`
     (Stopped: halt), and that run is in the golden set. `abc ccbbaa ijk`
     seed 3 raises a Chez error (`caddr` of `#f` in `transcribe-to-english`);
     the set avoids it by using seed 4.
  2. Urgencies are often exact rationals (3601 codelet lines), so the port
     must keep Chez's exact arithmetic.
  3. run4's documented seed (abc abd xyz dyz) gives up without an answer in
     the oracle, as misc3 already did.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (about 48 s).
  `chez_scheme/original/` untouched.

### Blockers
- None.

### Next
- Item 03 in iterations.md. For the equivalence runs: the Racket port should
  write the same trace (trace-format.md) and compare it byte for byte with
  `tests/golden/`. The `rng` fields on codelet and slipnet lines show the
  first codelet whose draws diverge. Emit exact rationals as `"n/d"`, and
  reproduce the `halt` path of `report-error-and-halt`.

---

## Iteration 4 — 2026-10-02 21:41
Item 03 (The compatibility layer): **SOLVED**.

### Completed
- `racket/compat.rkt` covers syntactic-sugar.ss and the Chez built-ins
  Metacat relies on:
  - Chez 10's `random`/`random-seed` (the LCG of trace-format.md, bit for
    bit, including the 4-step path for ranges above 2^32−1);
  - one-armed `if`;
  - `map` in Chez's application order (pairs from the end for 1–2 lists,
    last to first for 3+);
  - `for-each` returning the last value;
  - `sort` with Chez's argument order and Chez 10's own algorithm (list
    merge below 25 elements, which sorts the second half first; Shivers's
    vector merge above);
  - all-occurrence `remq`/`remv`/`remove`, `1+`, `-1+`;
  - a table of top-level values (`define-top-level-value`, `top-level-value`, …);
  - Chez's printer: `number->string` (flonum layout rule from s/print.ss),
    `display`, `write` (symbol/char/string escapes), `format`, `fprintf`,
    `printf`, `newline`, `error`;
  - `record-case`, `reset`/`reset-handler`, `collect`, `real-time`;
  - the definitions of syntactic-sugar.ss and all 22 macros, plus
    module-level forms `define-slipnet-node-list*` and
    `define-codelet-type-list*`.
- `racket/utilities.rkt`: utilities.ss line for line on top of compat. The
  only changes are marked `port:`: the `scheme-round` family via `only-in`,
  `ask` with `peek-char`, `eval` → `top-level-value`, `pause` → `sleep`,
  and `pairwise-map` in Chez's evaluation order.
- Differential tests: `tests/diff/utilities-battery.scm` (196 tests). It
  covers every utility, including the random ones (`prob?`, `~`,
  `random-pick`, `stochastic-pick` (TASK.md's weighted-pick),
  `stochastic-pick-by-method`, `weighted-index`, `stochastic-select`,
  `weighted-select`, `stochastic-filter`, `bounded-random-partition`,
  `randomize`) over 15 seeds each, with the generator state afterwards. It
  also checks the call order of every higher-order utility through logging
  procedures, `sort` results and predicate-call sequences, flonum printing
  (literals plus 350 random doubles across magnitudes), `format ~a/~s` on
  every kind of datum, and every macro (slipnet and coderack macros against
  logging stand-ins).
  - Chez side: `chez_scheme/oracle/diff-eval.ss` loads the whole original
    through prelude.ss and evaluates the battery.
  - Racket side: `racket/tests/utilities-diff-test.rkt` evaluates it in
    racket/base + compat + utilities, runs Chez, and compares line for line.
  - The battery takes about 1.5 s in all.
- `racket/tests/compat-test.rkt` (32 checks) covers what the battery cannot
  express: known-answer RNG values from Chez, the module-level definers,
  `fizzle` across modules, keyword matching by name, `record-case` hygiene,
  `reset`/`report-error-and-halt`, `error` messages, and `mcat`'s
  expansion-time token check.
- Tests-first, honestly: the battery and `diff-eval.ss` were written and
  run against Chez before compat.rkt existed. The Racket runner came
  after a first draft of compat/utilities. Its first run failed:
  - one-armed `if` is a syntax error in Racket;
  - after that fix, 15 tests failed. Chez's `for-each` returns the last
    value (for* loops pass it on), `for*` evaluates its bounds left to
    right, `append` in `pairwise-map` evaluates the recursive call first,
    and Chez's sort sorts the second half first. Two tests were artifacts
    of Racket interning literal strings and flonums (now noted, and the
    tests use computed values).

  Each was fixed until all 196 agree. Mutation checks, each restored
  afterwards: changing the LCG multiplier fails 20 tests; changing the
  map order fails 10 or more (RNG and order tests); moving the flonum
  exponent threshold fails 2; changing a symbol-escape rule fails 1.
  compat-test.rkt was written after the code.
- Findings, in `docs/porting-notes.md` (new item 03 section):
  - Chez's argument evaluation order depends on the call's shape (a table
    of observations), and cp0 inlines `map` over literal lists in an order
    of its own;
  - Racket's `call/cc` misbehaved inside rackunit, so `continuation-point*`
    uses `call/ec` (the original only escapes upwards);
  - `(ascending-index-list 0)` loops forever in the original;
  - pairs are immutable (only rule-graphics.ss:77 mutates one);
  - module-level expression values get printed by Racket, so engine
    modules must discard them.
- `chez_scheme/oracle/README.md` documents `diff-eval.ss`.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED.
  `chez_scheme/original/` untouched.

### Blockers
- None.

### Next
- Item 04 in iterations.md. Notes for the engine port:
  - require `compat.rkt` and `utilities.rkt`;
  - Racket forbids `set!` on imported variables and module cycles, and
    the original's 44 files are mutually recursive with globals set! across
    files, so one engine module that `include`s the ported files in load
    order (or a module language whose module-begin also voids top-level
    expression values) is probably the simplest faithful structure;
  - every call with two or more effectful arguments needs Chez's order,
    checked against the goldens;
  - slipnet.ss should use `define-slipnet-node-list*`, and rules.ss must
    register `format-slipnode` as a top-level value.

---

## Iteration 5 — 2026-10-02 21:58
Item 04 (Constants, setup, coderack, descriptions): **SOLVED**.

### Completed
- **Engine organisation, decided and written up** (porting-notes.md, "The
  engine's module structure"): one module, `racket/engine.rkt`, that
  `include`s the ported files `racket/engine/*.rktl` in metacat.ss's load
  order, after requiring compat.rkt and utilities.rkt. Reasons: the original's
  files refer to each other in both directions and `set!` each other's
  globals; Racket forbids module cycles and `set!` on imports, and one module
  gives exactly the semantics of Chez's top level loaded in order while each
  `.rktl` stays a line-for-line copy of its `.ss`.
  - `racket/engine-lang.rkt`: the engine's module language, racket/base
    whose `#%module-begin` wraps module-level expressions in `void` (Chez's
    top level drops their values; racket/base would print them).
  - `racket/engine/pending.rktl`: stand-ins, grouped by original file, for
    names whose files are not ported yet (procedures raise "not ported yet",
    variables are `#f`), including the graphics constants coderack.ss names.
    Later items delete their names there; a leftover is a duplicate
    definition and fails to compile.
  - `set-global!` (exported): sets a listed engine global from outside
    (tests, future CLI and GUI), since importers cannot `set!`.
- Ported: `constants.ss` (only the probability distributions; colours, fonts,
  window sizes and titles wait for the GUI items), `setup.ss` (globals and
  user commands; `setup`/`enable-resizing` create windows and move to the
  GUI layer), `coderack.ss` (unchanged but for `define-codelet-type-list*`),
  `descriptions.ss` (unchanged). GPL headers kept, "Ported to Racket" lines
  added.
- compat.rkt: Chez's `case`, which accepts a single datum as a clause key
  (`(rule-scout ...)` in coderack.ss); Racket rejected it at compile time.
  Covered by a new differential test in utilities-battery.scm (now 197).
- Tests:
  - `tests/diff/coderack-battery.scm` (43 tests) +
    `racket/tests/coderack-diff-test.rkt`: the same battery under Chez with
    the original loaded and under Racket with the engine; outputs must agree
    line for line (about 0.9 MB of output). Covers the urgency table, bin
    selection for integer/rational/flonum urgencies, bin urgencies at every
    temperature, posting (time stamps, bin indices, list order),
    `choose-codelet` over 8 seeds × 7 temperatures with the RNG state after
    every choice, overflow deletion (incl. proposed structures reported to
    the Workspace), removal weights, deferred posting below/at/above the
    limit, clamping and urgency adjustment, codelet `run`/`fizzle`, printing,
    `post-codelet-probability`/`num-of-codelets-to-post`/`bottom-up-urgency`,
    `add-bottom-up-codelets` and `add-top-down-codelets` with fake
    Workspace/Themespace/Trace/slipnodes, the threshold distributions, the
    setup.ss commands, and the description helpers.
  - `racket/tests/engine-test.rkt` (11 checks, written after the code):
    loading prints nothing, `set-global!` works and rejects unlisted names,
    codelet types are module variables and top-level values, pending
    stand-ins raise, and loading the engine declares no racket/gui or
    racket/draw module.
  - Infrastructure: the battery helpers moved to `tests/diff/helpers.scm`;
    `diff-eval.ss` takes several files and defines `b:set-global!`; the
    Racket runner is now `racket/tests/diff-runner.rkt`, shared by both
    differential tests.
- Tests-first, honestly: the battery was written and run under Chez before
  any engine code existed (one battery bug found there: a codelet argument
  must be an object). Before the engine compiled, the Racket test could not
  run at all (Racket rejected Chez's single-datum `case` clause). The first
  Racket run after porting failed most checks, because the battery's
  namespace had its own engine instance, so `set-global!` changed another
  copy (a runner bug). Once that was fixed, one test still failed because of
  a battery bug: `map` over a literal `(list ...)`, which Chez's cp0 inlines
  in its own order. Both were fixed, and then all 43 tests agreed with no change
  to the ported model code. Mutation checks on coderack.rktl, each restored:
  reversing the in-bin pick fails 3 tests, changing the urgency exponent 17,
  the removal weight 1, the deferred-excess pick 1, the bin's list order 12.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED. `chez_scheme/original/`
  untouched.

### Blockers
- None. The description codelets (`bottom-up-description-scout`, …,
  `propose-description`, `build-description`) and `make-description` need
  the Workspace, workspace structures, formulas and Slipnet, so only the
  helpers `descriptions-equal?`/`description-member?` are tested now. The rest
  will be checked by the golden traces once those files are ported.

### Next
- Item 05 in iterations.md. Notes:
  - add each ported file as an `include` in engine.rkt, in load order, and
    delete its names from `engine/pending.rktl`;
  - add globals that are set from outside to `set-global!`'s list;
  - single-datum `case` clauses now work; other Chez-isms may still appear
    at compile time;
  - the headless driver must give every codelet type a null Coderack window
    (`set-graphics-parameters`), as the oracle prelude does, or `run` fails
    on `#f`;
  - new differential batteries can reuse `diff-runner.rkt` with fakes for
    the parts not ported yet, as coderack-battery.scm does.

---

## Iteration 6 — 2026-10-02 22:10
Item 05 (Slipnet and images): **SOLVED**.

### Completed
- Ported `slipnet.ss` → `racket/engine/slipnet.rktl` and `images.ss` →
  `racket/engine/images.rktl`, included by `racket/engine.rkt` after
  descriptions.rktl, in load order. The only change: `slipnet-node-list*`
  becomes the module-level `define-slipnet-node-list*` (compat.rkt already
  had it), so each `plato-...` node is a module variable as well as a
  top-level value. images.ss is unchanged. GPL headers kept, "Ported to
  Racket" lines added.
- `engine/pending.rktl`: removed the slipnet.ss stand-ins; added
  `%update-cycle-length%` (run.ss's constant 15, used by slipnode `reset`),
  `make-letter`, `make-group`, `make-group-pexp`,
  `monitor-slipnode-activation-change`. `set-global!` now also lists
  `monitor-slipnode-activation-change` and `temp-adjusted-probability`, so
  that tests can replace them in both runners.
- Tests:
  - `tests/diff/slipnet-battery.scm` (48 tests) +
    `racket/tests/slipnet-diff-test.rkt`, on the shared `diff-runner.rkt`.
    Chez (original loaded) and Racket (engine) give identical output, about
    1.1 MB.
    - **Initial slipnet dump**: all 59 nodes (names, conceptual depth,
      activation, intrinsic/shrunk link lengths, category/instances) and
      every link list of every node (type, ends, label, length, intrinsic and
      current degree of association; 202 links), plus nodes and links as
      top-level values.
    - **Activation**: 20 `update-slipnet-activations` from 8 fixed states
      (clamped nodes, unfreezing after 10 updates, fake themes spreading
      activation). Every update records all activations, frozen flags, the
      generator state and every `monitor-slipnode-activation-change` call.
      Also 15 more seeds and the start-of-run state. All exact, so equality
      is to the last bit.
    - Also: decay and spread from each node alone; every activation message;
      relations over all node pairs; descriptor predicates on fake objects;
      similar property links, `apply-slippages` with coattail slippages and
      top-down codelet posting over several seeds.
    - **Images**: 8 letter/group images × 29 operations, each followed by
      copy, walks, state and reset; string images over a fake string × 13
      operations; `change-length-first?`; `enumerate-letter`.
  - `racket/tests/engine-test.rkt` (+4 checks): node count, nodes and links
    as module variables and top-level values.
- Tests-first: the battery was written and run under Chez before any port
  code. Two battery bugs showed up there: a fake Workspace lacked messages
  that top-down posting sends, and a `replace-all` test used lists of
  unequal length. With the item-04 engine, the Racket test failed at once
  (`plato-identity: undefined`). After the port, all 48 tests agreed on the
  first run, with no change to the ported code.
- Mutation checks, each restored afterwards:

  | Mutation | Tests failed |
  | --- | --- |
  | decay `round`→`floor` | 10 |
  | jump probability cube→square | 9 |
  | reverse order of the jump draws | 9 |
  | spread from above-threshold instead of fully active nodes | 9 |
  | no `min` cap on flush | 10 |
  | outgoing-link order | 1 |
  | shrunk length 40%→80% | 3 |
  | reverse-medium start letter | 2 |
  | `enumerate-letter` order | 3 |

  A left-to-right `for-each` in images' `replace-all` first failed nothing.
  I then added a `replace-all-fail` case (a middle image fails), and it now
  fails 1. So Chez's `map` order matters when an image operation escapes
  midway.
- Docs:
  - porting-notes.md has a new item 05 section: changes, stand-ins, where
    the draws are, exact arithmetic, map order in images, tests.
  - anomalies_and_quirks.md has three new entries:
    1. A string image answers `new-alpha-position-category` by sending
       `new-start-letter`. This is a suspected copy-paste bug: under Chez,
       `abc` becomes three `AlphaPos:first` nodes. rules.ss's
       `transform-image` can reach it.
    2. `relationship-between` of fewer than two nodes errors, and printing
       a direction-less group image errors. Both are latent.
    3. A string image's `reset` forces direction right. Not a bug, since the
       only caller uses right.
  - chez_scheme/oracle/README.md mentions the new battery.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (`raco test racket/`:
  341 tests). `chez_scheme/original/` untouched.

### Blockers
- None. These need the Workspace and are left to the golden traces once
  their files are ported:
  - the real `monitor-slipnode-activation-change` (trace.ss);
  - `temp-adjusted-probability` (formulas.ss);
  - `instantiate-as-letter`/`instantiate-as-group` (workspace-objects.ss,
    groups.ss).

### Next
- Item 06 in iterations.md. Notes:
  - rules.ss calls `transform-image`, which reaches the suspected
    `new-alpha-position-category` bug; port it faithfully.
  - Remove the pending stand-ins of each file as it is ported, including
    `%update-cycle-length%` when run.ss comes; it is a real value in
    pending.rktl, not `#f`.
  - The fakes in slipnet-battery.scm (fake string, fake slippage and
    sliplog, fake themes) can be reused.

---

## Iteration 7 — 2026-10-02 22:28
Item 06 (Workspace objects and strings): **SOLVED**.

### Completed
- Ported `workspace.ss`, `workspace-objects.ss`, `workspace-structures.ss`,
  `workspace-strings.ss`, `workspace-structure-formulas.ss` and `formulas.ss`
  to `racket/engine/*.rktl`, unchanged, with GPL headers kept and "Ported to
  Racket" lines added. `racket/engine.rkt` includes them after
  descriptions.rktl in metacat.ss's load order.
- `compat.rkt`: `tanh`, Chez's own primitive via `ffi/unsafe/vm`'s
  `vm-primitive`. racket/base has no `tanh`, and racket/math's may differ in
  the last bit.
- `engine/pending.rktl`:
  - removed the stand-ins of the ported files;
  - added stand-ins from bonds.ss, groups.ss, bridges.ss, breakers.ss,
    rules.ss, trace.ss, the graphics files and `*EEG*`, plus
    `*temperature-clamped?*`, which the original never defines (init-mcat
    creates it with `set!`).
- `set-global!` also lists the string globals, `*temperature-clamped?*`,
  `*EEG*` and `contains?`.
- Tests:
  - **`tests/diff/workspace-battery.scm`** (77 tests, about 0.7 MB, identical
    under Chez and Racket) + `racket/tests/workspace-diff-test.rkt`, plus
    `tests/diff/workspace-dump.scm`.
    - For **every problem in `tests/problems.txt`** (read at run time: 36
      problems, all 109 problem × seed pairs), the initial workspace as
      init-mcat builds it: strings, letters, every description (type,
      descriptor, proposal level, strength, time stamp), raw and relative
      importance, intra/inter/average unhappiness and salience, workspace
      averages and mapping strengths. Also all slipnet activations, the EEG
      messages and the generator state after each seed.
    - Also: live queries on 10 problems of every shape, seeded choices at 4
      seed/temperature pairs, fake bonds/groups/bridges/rules (every branch
      of the unhappiness, salience and mapping-strength formulas, storage
      expansion), workspace structures, `wins-fight?`/`wins-all-fights?`,
      `temp-adjusted-probability`/`-values`, the group probability formulas,
      `update-temperature` and `tanh`.
  - **`chez_scheme/oracle/tests/workspace-init-check.ss`** (Chez only). The
    battery's copy of init-mcat's Workspace steps (`b:init-problem`) gives
    the same dump as the original's real `init-mcat` (with the real
    Themespace, EEG and `contains?`) for all 36 problems, and draws nothing.
    I checked that it catches changes: activating descriptors to 50 instead
    of 100 makes 1 problem differ, and skipping the target's position
    descriptions makes all 36 differ.
  - `racket/tests/engine-test.rkt` (+5 checks): `*workspace*` exists at load
    time, a headless workspace string, formulas at temperature 100.
- Tests-first: the battery and dump were written and run under Chez before
  any port code was included. Four battery bugs showed up there (a wrong
  node name, a missing fake message, `tell` of `#f` for a non-justify
  answer string, `lowest-level-object` returning one object). Before the
  port, the Racket test failed: `set-global!: not a settable engine global:
  *EEG*`. After the port, 71 of 72 tests agreed on the first run. The one
  failure was a battery test that used `get-real-object`, which needs
  bridges.ss's `equivalent-workspace-objects?`, so I removed that call. No
  ported model code changed.
- **Testing infrastructure bug found and fixed.** At first no mutation of
  the ported code made the battery fail. The differential runner loaded a
  stale `engine_rkt.zo`: the default load handler does not notice edits to
  included `.rktl` files. `diff-runner.rkt` now loads through the
  compilation manager, with the handler created inside the fresh namespace
  (otherwise it skips the modules). The compilation manager also compares
  timestamps in whole seconds. Details are in porting-notes.md and
  anomalies_and_quirks.md.
- Mutation checks, run with a one-second pause around each edit and each
  restored afterwards:

  | Mutation | Tests failed |
  | --- | --- |
  | intra-salience 80% → 70% | 39 |
  | intra-salience 20% → 30% importance | 40 |
  | two-bond unhappiness 1/6 → 1/5 | 3 |
  | group importance factor 2/3 → 1/2 | 1 |
  | raw-importance cap 300 → 200 | 1 |
  | group-bridge weakness 1/2 → 1/3 | 3 |
  | reversed letter list | 49 |
  | relative importance without rounding | 1 |
  | bond-scan distribution ^2 → ^3 | 8 |
  | maximal-mapping tanh 1/40 → 1/30 | 1 |
  | half-mapping 1/2 → 1/3 | 4 |
  | rough count `(~ 2)` → `(~ 3)` | 2 |
  | temp-adjusted-probability 10− → 11− | 3 |
  | temperature weights 70/30 → 60/40 | 2 |
  | structure strength weights swapped | 39 |
  | length-description probability 1 → 0.9 | 1 |
  | letter ascii-name format | 52 |
  | descriptions appended instead of consed | 51 |
  | average intra unhappiness → average unhappiness | 4 |

  Before the `ws-importance` and `ws-maximal-mapping` tests existed, the 2/3,
  rounding and tanh mutations passed. At the start of a run every raw
  importance is 0, so they need fully active description types and a
  maximal non-spanning mapping.
- Docs:
  - porting-notes.md: new item 06 section.
  - anomalies_and_quirks.md: four new entries. Raw importance is always 0
    at the start (descriptors are activated, description types are not);
    `*temperature-clamped?*` is never defined; the stale `.zo` and
    whole-second timestamps; `tanh` is missing from racket/base.
  - chez_scheme/oracle/README.md: mentions the new battery and check.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (`raco test racket/`:
  424 tests). `chez_scheme/original/` untouched.

### Blockers
- None. Not covered yet, because they need files not ported:
  - `get-real-object` and `get-equivalent-bridge` for non-member bridges
    (bridges.ss);
  - `get-equivalent-bond`/`-flipped-bond`/`-group` beyond the member case
    (bonds.ss, groups.ss);
  - `delete-invalid-string-position-middle-descriptions` with bridges
    (breakers.ss);
  - workspace objects' `print` (trace.ss's `full-workspace-object-name`);
  - the real Themespace.

  The golden traces will check these once those files are ported.

### Next
- Item 07 in iterations.md. Notes:
  - remove the bonds.ss/groups.ss stand-ins from engine/pending.rktl
    (`same-bond-*`, `opposite-bond-*`, `same-group-*`, `contains?`,
    `make-group`);
  - keep `contains?` settable only if a battery still needs it;
  - `b:init-problem` in tests/diff/workspace-dump.scm gives a ready initial
    workspace for any problem in both runners, and the battery's fakes
    (`b:fake-structure`, `b:fake-bridge`) can be reused;
  - after editing an included `.rktl`, wait a second before re-running
    tests if a mutation must take effect.

---

## Iteration 8 — 2026-10-02 22:56
Item 07 (Bonds, groups, concept mappings): **SOLVED**.

### Completed
- Ported `bonds.ss`, `groups.ss` and `concept-mappings.ss` to
  `racket/engine/*.rktl`, with GPL headers kept and "Ported to Racket" lines
  added. engine.rkt includes them in metacat.ss's load order. Apart from the
  headers they are verbatim, except for one `port:` line in groups.rktl (the
  evaluation-order fix below).
- `racket/engine/group-graphics.rktl`: group-graphics.ss's `group-graphics`
  procedure, verbatim. groups.ss calls it ungated when group-builder
  consolidates sameness groups. It only messages `*workspace-window*`; the
  rest of group-graphics.ss waits for the GUI items.
- `engine/pending.rktl`:
  - removed the bonds/groups stand-ins;
  - added stand-ins for bridges.ss, trace.ss and graphics names
    (`monitor-new-groups`, `incompatible-*-CMs?`, `outline-box`, `arrowhead`,
    …);
  - added two names the original never defines: `same-direction?` (called by
    the unused `bonds-equal?`; the stand-in raises, as Chez would) and two
    group fonts that workspace-graphics.ss creates by `set!`.
  - `set-global!` also lists `monitor-new-groups`.
- **The codelet-level differential harness**:
  - Files: `tests/diff/codelet-harness.scm`, `tests/diff/codelet-battery.scm`,
    `racket/tests/codelet-diff-test.rkt`.
  - The harness copies run.ss's `run-mcat` loop with only the 10 bond and
    group codelet types enabled:
    - bond scouts are the only initial codelets;
    - bottom-up posting covers bond scouts and whole-string group scouts;
    - only the bond and group top-down slipnodes post;
    - self-watching is off;
    - `update-everything` leaves out rules, Trace periods and themes.
  - It runs in Chez with the original loaded and in Racket with the engine.
  - The trace has one line per codelet: type, urgency, time stamp, generator
    state, structures built and broken (with strengths), Workspace-window
    messages, slipnode and new-group monitor calls, and proposed counts.
  - It also has one line per update cycle: temperature, all activations,
    Coderack size and generator state, plus every Workspace object every 4th
    update.
  - The Racket test compares the traces line by line and reports the first
    difference, with problem, seed and codelet (e.g. "traces differ at line
    10 … (8 bond-evaluator 70 …" vs "… 63 …").
  - Runs: all 109 problem × seed pairs for 400 codelets, plus 7 runs of 2000
    codelets that reach group-builder's sameness consolidation. That is
    about 60,000 lines; the test takes about 22 s.
  - The test also checks that all 10 codelet types run, that bonds and
    groups are built and broken, and that `group-graphics` is called.
- Concept mappings are only made through bridges, so they never appear in
  these runs. The battery tests them directly:
  - every message, for every pair of instances of the 9 slipnet categories;
  - real description pairs of letters and groups after 600 codelets on 5
    problems, with `remove-duplicate-CMs` and the activation effects.
- `racket/tests/engine-test.rkt` (+8 checks): a headless bond, its flipped
  version, a concept mapping, the `same-direction?` stand-in, and
  `group-graphics`.
- Tests-first, honestly: I copied the files and got the engine compiling
  first, to find the missing names. The engine.rkt includes were in place
  before the battery existed.
  - The battery was written and run under Chez before the Racket side ran
    at all.
  - With the HEAD engine.rkt/pending.rktl, i.e. without the port, the Racket
    test fails: `set-global!: not a settable engine global:
    monitor-new-groups`.
  - With the port, the 400-codelet runs agreed on the first run.
  - Adding the 2000-codelet runs found a real divergence: `abc abd iijjkk`
    seed 3, the update after codelet 735.
    - A group's `get-local-density` does `(append (neighbors … 'choose-left-neighbor)
      (neighbors … 'choose-right-neighbor))`. Both arguments can draw, and
      Chez evaluates the second first (checked: `RL`).
    - Fixed with a `let*`, marked `port:`.
    - I audited the three files: no other call has two drawing arguments.
- Exploration (not in the gate): the harness at **2000 codelets on all 109
  runs** (218,000 codelets, 90 MB of trace) is byte-identical under Chez and
  Racket.
- Mutation checks, each restored afterwards (1 s pauses around edits):

  | Mutation | Caught |
  | --- | --- |
  | bond-degree-of-assoc 11 → 10 | yes (line 10) |
  | bond-evaluator without `1-` | yes |
  | bond local support 0.6 → 0.5 | yes |
  | bond compatibility factor 0.7 → 0.8 | yes |
  | bond-builder group weight → 1 | yes |
  | bond direction left/right swapped | yes |
  | `intersect` argument order in choose-bond-facet | no (equivalent: both objects list facets in the same order) |
  | group length factor 40 → 20 | yes |
  | group local support 0.6 → 0.5 | yes |
  | group-evaluation-probability /5 → /6 | yes |
  | group bond-factor weight 0.98 → 0.97 | yes |
  | whole-string scout `random-pick` → `car` | yes |
  | CM strength without `^2` | yes |
  | unlabeled CM degree 5 → 6 | yes |
  | reversible CM type group → length | yes |
  | `activate-label` without flush | yes |

- Docs:
  - porting-notes.md: new item 07 section, plus `append`'s order in the
    evaluation-order table.
  - anomalies_and_quirks.md: four new entries. `same-direction?` is
    undefined; Chez evaluates `append`'s second argument first; group fonts
    are never defined; concept mappings are only made through bridges.
  - chez_scheme/oracle/README.md mentions the new battery.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (`raco test racket/`:
  451 tests). `chez_scheme/original/` untouched.

### Blockers
- None. Not covered yet, because they need bridges.ss/breakers.ss:
  - `get-incompatible-bridge(s)` of bonds and groups with real bridges;
  - `break-group` with bridges (`delete-proposed-*-bridges`, `break-bridge`);
  - flipped groups (`make-flipped-version` via bridges, `monitor-new-groups`
    with flipped? = #t).

  The goldens can't be compared yet either: real runs post bottom-up
  bridge scouts among the initial codelets, so the golden prefix diverges
  at the first bridge scout.

### Next
- Item 08 (bridges and breakers). Notes:
  - extend codelet-harness.scm: enable the bridge types (post the initial
    bridge scouts as run.ss does, add `bottom-up-bridge-scout` and
    `important-object-bridge-scout` to `b:bottom-up-types`) and the
    breaker;
  - with only rule, answer and self-watching codelets left out, the
    harness then gets close to the real run;
  - remove the bridges.ss stand-ins (`incompatible-*-CMs?`, `break-bridge`,
    …) from pending.rktl;
  - check every call with two drawing arguments against Chez's order;
    `append` evaluates its second argument first;
  - run some long (2000-codelet) runs: the 400-codelet prefix missed the
    `append` bug.

---

## Iteration 9 — 2026-10-02 23:30
Item 08 (Bridges and breakers): **SOLVED**.

### Completed
- Ported `bridges.ss` and `breakers.ss` to `racket/engine/bridges.rktl` and
  `breakers.rktl`, verbatim apart from the GPL headers' "Ported to Racket"
  lines. engine.rkt includes them right after groups.rktl, in metacat.ss's
  load order. No model line changed.
  - Evaluation-order audit: every draw (bridge-type pick, `choose-object`,
    `stochastic-pick-by-method`, `stochastic-if*`, `random-pick`,
    `wins-fight?`, and the builds and breaks in bridge-builder) sits in a
    `let*`, a sequence or an `and`/`or`. No call has two drawing arguments.
- `engine/pending.rktl`:
  - removed the bridges.ss stand-ins;
  - moved `equivalent-workspace-objects?` (trace.ss) and
    `rule-describable-bridge?` (rules.ss), which had been filed under
    bridges.ss, to their own files;
  - added stand-ins for themes.ss, trace.ss (`monitor-new-concept-mappings`,
    `entries`), justify.ss and bridge-graphics.ss names.
  - Every bridge calls four small pure themes.ss helpers
    (`bridge-type->theme-type`, `bridge-theme-compatibility-sigmoid` with
    `beta`, `descriptions-affect-themespace?`, `ignore-descriptions?`), so
    pending.rktl holds early verbatim copies of them. Item 10 moves them back.
  - `set-global!` also lists `monitor-new-concept-mappings`.
- Tests:
  - **The codelet harness with bridges**:
    - `tests/diff/codelet-harness.scm` gets a `b:bridges?` setting
      (`b:enable-bridges!`). It posts run.ss's initial bond and bridge scouts
      and every bottom-up type except rules, answers and self-watching (so
      both bridge scouts, the description scout and the breaker run). It also
      uses the original's full `*top-down-slipnodes*`, so the description
      codelets run.
    - A fake Themespace with no active theme records bridge-builder's boosts.
    - The trace now also records bridges built and broken (type, objects,
      flipped groups, strength, concept mappings, bond CMs, symmetric
      slippages), proposed bridges, description counts per object, and
      `monitor-new-concept-mappings` calls.
    - Battery: `tests/diff/bridge-battery.scm`; test:
      `racket/tests/bridge-diff-test.rkt`.
    - Runs: all 109 problem × seed pairs for **1000 codelets** (item 07 ran
      400). That is about 116,000 lines and 48 MB, identical under Chez and
      Racket.
    - The test also checks what the runs reach: all bridge, description and
      breaker codelet types; top, vertical and bottom bridges built; bridges
      broken; the breaker breaking structures; a bridge with a flipped group;
      monitor and Themespace calls.
    - With `b:bridges?` off, item 07's traces are unchanged.
  - **The bridge matrix**: 9 runs of 1500 codelets. Afterwards, a fresh
    bridge for every horizontal and vertical object pair, with:
    - its CMs and their strengths, coherence, internal and external
      strength, incompatible bridges and bond;
    - `direction-incompatible-bridges` for every pair of directed groups;
    - every pair of built bridges (incompatible, supporting, enclosing).
  - `racket/tests/engine-test.rkt` (+11 checks): the bridge procedures, the
    breaker codelet type, the early themes.ss copies. A pending check now
    uses `rule-describable-bridge?`.
- Tests-first, honestly:
  - The battery ran under Chez before the Racket test existed. One harness
    bug showed up there: an `apply append` nesting error in `b:structures`.
  - With HEAD's engine.rkt and pending.rktl, the Racket test fails:
    `set-global!: not a settable engine global: monitor-new-concept-mappings`.
  - The port's own code (bridges.rktl, breakers.rktl, engine includes,
    pending changes) was written before the battery. All 116,000 lines
    agreed on the first run.
- **Mutation checks**: 19 mutations in bridges.rktl and breakers.rktl
  (porting-notes.md has the table).
  - 16 are caught: CM-count factors, singleton factor, external strengths,
    scout weights, evaluator, fight weights, symmetric slippages,
    `remq-elements`, slip-linked, all three breaker mutations, and so on.
    The CM-label test in `incompatible-horizontal-CMs?` was caught only
    after I added the bridge matrix.
  - 3 are equivalent on these runs and not caught:
    - the coherence factor 2.5 → 2.0 (strengths clip at 100 either way);
    - the two partition-predicate swaps in `direction-incompatible-bridges`
      (subobject bridges come in string order, so the second predicate is
      never consulted).
- **Exploration** (not in the gate): the bridges harness at **3000 codelets
  on all 109 runs** is byte-identical under Chez and Racket. That is 327,000
  codelets and 151 MB, with 2160 bridges built, 105 breaker breaks and 6
  flipped-group bridges.
- Docs:
  - porting-notes.md: new item 08 section.
  - anomalies_and_quirks.md: three new entries.
    1. `letter-category-mappable-objects?` compares object1's group category
       with itself, a bug in the original that is always true.
    2. Bridges call themes.ss and trace.ss on every bridge, and
       `*themespace-window*` is messaged ungated.
    3. Dead code: the singleton-group proposers are never called.
  - The concept-mappings entry is updated.
  - chez_scheme/oracle/README.md mentions the new battery.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (`raco test racket/`:
  483 tests; the gate takes about 2.5 min, about 85 s of it the new test).
  `chez_scheme/original/` untouched.

### Blockers
- None. Still not compared with the goldens: real runs also post
  rule-scout, answer-finder, answer-justifier, progress-watcher and jootser
  (drawing for their posting probabilities at every update). The real
  Themespace and Trace aren't ported either, so the golden prefixes still
  diverge at the first update.

### Next
- Item 09 (rules and answers). Notes:
  - add rule-scout, answer-finder and answer-justifier to the harness's
    `b:bottom-up-types` and call `check-if-rules-possible` in
    `b:update-everything`;
  - remove `rule-describable-bridge?` and `verbatim-clause?` from
    pending.rktl;
  - rules.ss calls `get-real-object` → `equivalent-workspace-objects?`
    (trace.ss): copy it early, as was done for the themes.ss helpers, or
    make it settable;
  - the harness may now be close enough to compare golden prefixes up to the
    first self-watching event, if the fake Themespace and Trace are
    replaced as items 09–10 port them;
  - the bridges test is the slowest in the gate (about 85 s). If the gate
    gets too long, lower `b:codelet-cap` in bridge-battery.scm from 1000.

---

## Iteration 10 — 2026-10-03 00:09
Item 09 (Rules and answers): **SOLVED**, with one criterion moved to item 11
(see "Not met here" below).

### Completed
- Ported `rules.ss` and `answers.ss` to `racket/engine/rules.rktl` and
  `answers.rktl`, verbatim apart from the GPL headers' "Ported to Racket"
  lines. engine.rkt includes them right after images.rktl, in metacat.ss's
  load order. No model line changed.
  - Evaluation-order audit: every draw sits in a `let*`, a sequence, a
    `map` or a utilities.ss `filter`. These are rule-scout, rule abstraction
    (`bounded-random-partition`, `stochastic-if*`, `prob?`), template
    instantiation (`stochastic-pick`), rule-evaluator, answer-finder and
    translation's `(prob? 0.4)` filter. No call has two drawing arguments.
- `engine/pending.rktl`:
  - removed `rule-describable-bridge?` and `verbatim-clause?`;
  - added stand-ins from run.ss (`go`, `post-initial-codelets`, `suspend`,
    `update-everything`), trace.ss (`monitor-new-rules`, `make-answer-event`,
    `make-snag-event`, `theme-pattern-entries-equal?`), memory.ss
    (`abstract-answer-description`, `abstract-snag-description`), justify.ss
    (`compare-rule-clause-lists`), rule-graphics.ss and bridge-graphics.ss,
    plus four slippage colours from constants.ss;
  - added **early verbatim copies** of three pure definitions that the
    model calls on every rule or answer: general-graphics.ss's
    `find-next-space-position` (every `make-rule` → `transcribe-to-english`),
    trace.ss's `equivalent-workspace-objects?` (`get-real-object`, for every
    answer) and themes.ss's `diff` (`#f`, compared in answers.ss's theme
    phrases).
  - `set-global!` lists the nine new hooks the harness replaces.
- **Tests**:
  - **The codelet harness with rules and answers**:
    - `tests/diff/codelet-harness.scm` gets a `b:rules?` setting. Updates
      start with `check-if-rules-possible`, post bottom-up codelets with the
      original's own `add-bottom-up-codelets` over all bottom-up types
      (self-watching off), and end snag periods as run.ss does.
    - A run stops after the codelet that reports its first answer.
    - Battery: `tests/diff/rule-battery.scm`. It has fakes, the same in both
      runners, for the Trace (events in a list, a snag period with constant
      progress), the Memory, answer and snag events, the abstract
      descriptions, the rule monitor, the Commentary window, `suspend` and
      answer-justifier's procedure.
    - The trace adds every rule built (English transcription, clauses,
      strength, quality values, supporting bridges), snags, every answer
      (letters and groups of the translated string, both rules, bridges,
      slippage log, quality), Memory queries and the commentary that
      answers.ss writes.
    - Runs: all 109 problem × seed pairs, up to the first answer or 2500
      codelets. That is 220,531 lines and about 100 MB, identical under Chez
      and Racket. 45 of the 52 non-justify runs reach an answer (`xyd`,
      `xyz`, `ijl`, `mrrjjk`, `qeeeq`, …).
    - `first-answers`: a summary of every run's first answer (codelet,
      letters, quality, rule and translated rule in English).
    - **The rule matrix** (12 runs): every rule of the Workspace applied
      with `apply-rule` (transforms and image letters) and translated with
      `translate` (translated rule, bridges, slippage log, groups, reference
      objects), then the translated rule applied to the other string. The
      generator state is recorded after each rule.
    - Test: `racket/tests/rule-diff-test.rkt`. It also checks that rule
      scouts, evaluators, builders, answer finders and answer justifiers
      run, that top and bottom rules are built, and that answers, snags,
      commentary and Memory queries occur. It checks that at least 30 runs
      answer and that the matrix examines rules.
  - `racket/tests/engine-test.rkt` (+17 checks): the rules/answers
    procedures and codelet types, and the early copies.
  - `diff-runner.rkt`: with `METACAT_DIFF_DEBUG=1`, it prints the message
    behind each `ERROR` result.
- Tests-first, honestly:
  - The port's code was written first, to find out what the engine needed
    in order to compile.
  - The battery ran under Chez before the Racket test existed.
  - Against a scratch worktree of HEAD (no port) with the new battery and
    test, the Racket test fails: `set-global!: not a settable engine global:
    *memory*`.
  - With the port, the first Racket run agreed for 58,704 lines, then hit
    `equivalent-workspace-objects?: not ported yet` on `eqe qeq abbbc`. This
    was fixed with the early copy. After that everything agreed as the
    battery grew (snag-period fake, summary, matrix).
- **Mutation checks**: 18 mutations in rules.rktl and answers.rktl, run on a
  reduced battery and each restored afterwards (porting-notes.md has the
  table). 16 are caught, from 1 line (the translation's ignore probability)
  to 36,428. Two are not caught, even on the full battery:
  - `sort-templates`' extrinsic order (no run builds two extrinsic
    templates);
  - process-snag keeping proposed bridges. This one is equivalent:
    `delete-all-codelets` deletes them anyway, as the original's comment
    says.
- Docs:
  - porting-notes.md: new item 09 section.
  - anomalies_and_quirks.md: two new entries. Rules and answers lean on
    later files, including a REPL abbreviation, and only the Trace unclamps
    the temperature after a snag. Rules are never removed (`delete-rule` is
    never sent).
  - chez_scheme/oracle/README.md mentions the new battery.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (`raco test racket/`:
  519 tests; the gate now takes about 5 min, about 2.7 min of it the new
  test). `chez_scheme/original/` untouched.

### Not met here
- **"The first answer on every golden run matches"** cannot be checked
  before items 10–11.
  - The golden runs have the real Themespace, with themes from codelet 15.
    They also have the Trace (snag and clamp periods, events) and
    self-watching codelets. answers.ss itself calls trace.ss, memory.ss and
    run.ss for every answer.
  - So the goldens' first answers come at other codelets and can differ:
    `abc abd xyz` seed 3852097033 gives `wyz` at codelet 2170 in the golden
    and `xyd` at codelet 1174 in the harness.
  - What was checked instead: the first answer of every harness run, Chez
    vs Racket, for all 109 problem × seed pairs, event for event.
  - Item 11's "every golden run matches event for event" covers the goldens'
    first answers. That item must not skip it.

### Blockers
- None.

### Next
- Item 10 (themes, justification, trace, jootsing, memory). Notes:
  - move the early copies back out of engine/pending.rktl: the themes.ss
    helpers (`bridge-type->theme-type`, `descriptions-affect-themespace?`,
    `ignore-descriptions?`, `beta`, the sigmoid, `diff`) and trace.ss's
    `equivalent-workspace-objects?`;
  - justify.ss gives answer-justifier its procedure. The 57 justify runs in
    rule-battery.scm only record answer-justifier calls through a fake, so
    they could then run for real;
  - the harness fakes (`b:fake-trace`, `b:fake-memory`, the event fakes,
    `b:fake-themespace`) can be replaced one by one with the real objects.
    At that point the harness converges on run.ss, and golden prefixes
    become comparable;
  - `abc ccbbaa ijk` seed 3 crashes the original in `transcribe-to-english`
    (anomalies). The port should crash at the same codelet once full runs
    exist;
  - the gate is about 5 min now. If a new test adds a lot, lower
    `b:codelet-cap` in bridge-battery.scm (1000) or rule-battery.scm (2500).

---

## Iteration 11 — 2026-10-03 00:55
Item 10 (Themes, justification, trace, jootsing, memory): **SOLVED**. All 109
golden runs now match the port byte for byte, which covers more than the
item asked for (theme and trace events).

### Completed
- Ported `themes.ss`, `justify.ss`, `trace.ss`, `jootsing.ss` and
  `memory.ss` to `racket/engine/*.rktl`, verbatim apart from the GPL
  headers' "Ported to Racket" lines (checked with `diff`). engine.rkt
  includes them right after answers.rktl, in metacat.ss's load order. No
  model line changed.
  - Evaluation-order audit: every draw (theme activation, thematic bridge
    scout, answer-justifier, theme-pattern retention, jootser,
    progress-watcher) sits in a `let*`, a sequence, a utilities.ss `filter`
    or a compat `map`/`tell-all`. No call has two drawing arguments.
- `engine/pending.rktl`:
  - removed the stand-ins of the five files, and the early copies from items
    08–09 (`bridge-type->theme-type`, `descriptions-affect-themespace?`,
    `ignore-descriptions?`, `beta`, the sigmoid, `diff`,
    `equivalent-workspace-objects?`), which are now the files' own;
  - added `*this-run*` (run.ss), `*fg-color*`/`%default-fg-color%` and 18
    constants.ss colours that events keep for drawing;
  - added `complement-codelet-pattern`, which the original never defines,
    as an identifier macro that raises as Chez would;
  - added early verbatim copies of trace-graphics.ss's
    `group-event-pexp-text-string` (it makes every group event's name, which
    is in the trace) and theme-graphics.ss's `relation-name`.
- `set-global!` also lists the procedures and objects a run's trace wraps
  (`*coderack*`, the build/break procedures, `update-temperature`,
  `update-slipnet-activations`), plus `*this-run*` and `*display-mode?*`.
  utilities.rkt exports `set-report-error-and-halt!` (marked `port:`).
- **`racket/tests/golden-harness.rkt`**: the port's trace in the golden
  format. It is the Racket counterpart of the oracle's prelude.ss headless
  windows (with commentary-graphics.ss's comment-window logic on a
  recording window), trace.ss's JSON writer and wrappers, and run.ss's
  driver, around a copy of the original run.ss's `init-mcat`, `run-mcat`,
  `update-everything` and helpers. Item 11 ports run.ss into the engine and
  replaces that copy.
- **`racket/tests/golden-test.rkt`**: runs every golden of
  `tests/problems.txt` in a fresh engine, spread over up to 16 places
  (about 25 s), and requires byte-for-byte equality, reporting the first
  differing line.
  - Result: **109/109 identical**. That is 272,957 codelets, 12,952
    `themes` lines and 1,326 Temporal Trace events (all seven types: answer,
    snag, clamp, rule, group, concept-mapping, concept-activation). It also
    covers 115 answers, 608 commentary paragraphs and the original's `halt`
    on `eqe qeq abbba aaabaaa` seed 3.
  - It also checks that problems.txt lists exactly the golden files, and
    that the goldens contain every event type and the thematic,
    justification and jootsing codelets.
  - It runs the oracle live on `abc ccbbaa ijk` seed 3, which crashes the
    original. The port raises the same `caddr` error after the same 1062
    trace lines.
- `racket/tests/engine-test.rkt` (+15 checks): the Themespace, Temporal Trace
  and Memory exist at load time; the new codelet types and procedures; the
  `relation-name` copy.
- Tests-first, honestly:
  - The five `.rktl` files came first, to find what the engine needed in
    order to compile. Then the harness and the test.
  - Against a scratch worktree of HEAD (no port; only the harness's hooks
    added), the test fails 111 of 123 checks: every run raises `application:
    not a procedure ... given: #f`.
  - With the port, `a b z` seed 1 matched at once. The full set first failed
    from the second run on, because the harness reused one engine. The
    Memory and the codelet count outlive a run, and the oracle uses a fresh
    process per golden. With a fresh engine per run, all 109 matched, with no
    change to the ported code.
- **Mutation checks**: 14 mutations across the five files, each restored
  afterwards. 13 are caught, with 3 to 109 runs differing (porting-notes.md
  has the table). The one not caught, thematic-bridge-scout's cluster
  probability `^2` → `^3`, is equivalent on these runs: active clusters'
  maximum activations are always exactly 0 or 100 (900 vs 117 in the
  goldens), and `prob?` short-circuits at both.
- Docs:
  - porting-notes.md: new item 10 section.
  - trace-format.md: a note on the port's traces.
  - anomalies_and_quirks.md:
    - new entries: `complement-codelet-pattern` is never defined; the
      Memory outlives a run; trace events and the EEG reach into graphics
      files;
    - updated entries: the halt run and the `caddr` crash, both reproduced
      by the port.
- `python3 ralph_loops/loop0001/gate.py`: GATE PASSED (`raco test racket/`:
  661 tests; gate about 5 min). `chez_scheme/original/` untouched.

### Blockers
- None.

### Next
- Item 11 (full runs and the CLI). Notes:
  - Port run.ss into the engine (`racket/engine/run.rktl`, in load order
    after workspace-structure-formulas.rktl), and delete the run.ss stand-ins
    from pending.rktl (`go`, `suspend`, `update-everything`,
    `post-initial-codelets`, `*this-run*`, `*display-mode?*`, …). Then make
    golden-harness.rkt use the engine's `init-mcat`/`run-mcat` instead of its
    copy.
    - `break`/`quiet-break` need a hook (a settable global) for headless
      drivers.
    - `waiter-prompt-and-read`, `console-input-port` and `swl:` names at
      run.ss's top level need care.
  - The trace writer in golden-harness.rkt (JSON, wrappers, headless windows)
    can move to a non-test module that `racket/cli.rkt` uses for `--trace`.
  - **Each run needs a fresh engine** (new namespace or process). Otherwise
    the Memory carries answers over.
  - golden-test.rkt already compares every golden; item 11 must keep it
    (with the engine's run.ss) and add the CLI vs `chez_scheme/oracle/run.ss`
    stdout comparison and run times per problem. Racket takes about 60 s on
    one core for all 109 runs; Chez about 13 s on 32 cores in parallel.

---

## Iteration 12 — 2026-10-03 01:31
Item 11 (Full runs and the CLI): **SOLVED**. The engine now runs on its own
ported run.ss. All 109 golden runs match event for event, and on all 109 the
printed output equals the live oracle's byte for byte.

### Completed
- **`run.ss` → `racket/engine/run.rktl`**, included right after
  workspace-structure-formulas.rktl (metacat.ss's load order). It is verbatim
  apart from three `port:` changes:
  - `prompt`/`no-prompt` (an SWL 0.9u REPL workaround) are left out, along
    with `swl:sync-display`;
  - `break` and `quiet-break` use Racket's re-entrant `call/cc`, so `go`
    resumes a stopped run. compat's `continuation-point*` is an escape-only
    `call/ec`, and `go` is the original's only re-entrant use of a
    continuation;
  - `init-workspace`'s `let` is written as a `let*` in Chez's order
    (right to left). The mutation check shows this order makes no difference.
- `engine/pending.rktl`:
  - removed the run.ss stand-ins;
  - added `restore-current-state` (workspace-graphics.ss);
  - added `*initial-slipnode-unclamp-time*`, which the original never
    defines. It is like `*temperature-clamped?*`: init-mcat creates it by
    `set!`, and Racket refused to compile run.rktl without it.
  - `set-global!` now also lists `*running?*`, `*interrupt?*`,
    `*break-time*`, `*step-mode?*`, `%step-cycles%`, `break` and
    `quiet-break`.
- **`racket/headless.rkt`** (no racket/gui) is the port's counterpart of
  `chez_scheme/oracle/run.ss` + prelude.ss's headless windows + trace.ss.
  - It holds the code moved out of item 10's test harness.
  - `run-problem` calls the engine's own `init-mcat`/`run-mcat`. It prints
    what the oracle prints and writes the trace to an optional port.
  - `racket/tests/golden-harness.rkt` is now a thin wrapper around it.
- **`racket/cli.rkt`**: `racket racket/cli.rkt INITIAL MODIFIED TARGET
  [ANSWER] [--seed N] [--max-codelets K] [--keep-going] [--trace FILE]`.
  - It uses the oracle's argument rules, output and exit codes: 2 for bad
    arguments, 1 when the run raises.
  - Without `--seed` the seed comes from the clock (`randomize`) and is
    printed.
- Tests:
  - `racket/tests/golden-test.rkt` (kept, now on the engine's run.ss):
    **109/109 traces identical to tests/golden/**. It now also runs the
    oracle live in parallel and compares each run's printed output with
    it: **109/109 identical**. It also checks that the outputs cover
    commentary, answers, suspend's message, `Codelets run:` at a cap,
    `Stopped: cap`, `Stopped: halt` with `Ooops:`, and `Answers: none`.
    The crash run (`abc ccbbaa ijk` seed 3) is still checked. Takes about
    40 s.
  - **`racket/tests/cli-test.rkt`** runs the CLI as a program against the
    oracle program on these runs: an answer, no cap, a cap before any
    answer, a justify run, keep-going, and the halt run. In each, stdout
    and the exit code must match and stderr must be empty.
    - `--trace` writes the golden file byte for byte and leaves the output
      unchanged.
    - A clock seed is printed, and the oracle replays the run from it.
    - The crash run fails in both programs, with the same stdout up to the
      crash and the `caddr` error.
    - 9 bad argument lists exit 2 in both programs.
    - 69 checks, about 15 s.
  - **`racket/tests/run-test.rkt`**: the engine's own `break`/`go`.
    - One run is stopped at codelets 150, 300 and 450 and resumed with
      `go`. Its RNG state, count, temperature and output equal those of
      an unstopped run, and the control panel gets the expected
      input/run mode switches.
    - Step mode (`ss 40`) and `go` without a break are covered too.
  - `racket/tests/engine-test.rkt`: run.ss's procedures and constants. The
    pending check now uses `restore-current-state`.
- Tests-first, honestly:
  - `cli-test.rkt` was written before `cli.rkt` existed and failed 39 of 69
    checks (cannot open module file).
  - The stdout comparison in golden-test.rkt was written after
    headless.rkt. It failed only on my own too-specific regex
    (`Codelets run: 10000`; caps differ), which I fixed.
  - run.rktl came before its tests. Compiling it found the undefined
    `*initial-slipnode-unclamp-time*`. The first full suite then failed 1
    of 956 checks: engine-test still expected the `post-initial-codelets`
    stand-in.
  - run-test.rkt was written after the port. It passed once its helper
    used compat's `random-seed`.
  - With the engine's run.ss replacing item 10's copy, all 109 goldens
    matched at once.
- **Mutation checks** on run.rktl, 10 mutations (table in porting-notes.md).
  - 8 are caught, with 3 to 109 runs differing in trace and output: update
    cycle, clamp cycles, initial codelets, spread order, unclamp, snag
    undo, middle description, single-letter activation.
  - 2 are equivalent: the string-making order, and the null window's
    `garbage-collect`.
- **Run times** (`tests/bench-runs.rkt`, outside the suite; table per problem
  in **`docs/run-times.md`**). Each of the 109 runs was timed as a process,
  one at a time, and the outputs were identical on 109 of 109.
  - Totals: Chez 104 s, Racket 70 s.
  - Startup: Chez 0.71 s (it compiles the original's sources on every
    start), Racket 0.14 s.
  - Per codelet the port is about 2× slower: ≈200 vs ≈97 ms per 1000
    codelets.
  - On the longest problem (`eqe qeq abbba aaabaaa`, 34,601 codelets over
    3 seeds) the port's processes are 1.35× slower in total.
  - Short runs finish sooner in the port, because of its faster startup.
- Docs:
  - porting-notes.md: new item 11 section.
  - anomalies_and_quirks.md: a new entry, `go` is the one re-entrant
    continuation, with what the GUI must respect (a prompt per caller; set
    the reset handler, don't parameterize it). The entry on
    `*temperature-clamped?*` now also covers `*initial-slipnode-unclamp-time*`.
  - trace-format.md, chez_scheme/oracle/README.md, and the CLI line in
    CLAUDE.md.
- `python3 ralph_loops/loop0001/gate.py`: **GATE PASSED** (about 6 min on
  this machine). `chez_scheme/original/` untouched. divergences.md: still none.

### Blockers
- None.

### Next
- Item 12 (the SGL interpreter on racket/draw). Notes for the GUI items:
  - Drive runs through the engine's own run.ss, as `racket/headless.rkt`
    does, but keep the engine's `break`/`go`.
    - Run `run-mcat` and `go` each inside `call-with-continuation-prompt`
      in the engine thread.
    - Set (don't parameterize) compat's `reset-handler`, as
      racket/tests/run-test.rkt does.
    - The control panel must accept `switch-to-input-mode`,
      `switch-to-run-mode` and `set-verbose-step-mode`.
  - Use one engine instance per run, or accept the original's behaviour
    (the Memory keeps answers across runs, as in the original program).
  - The early copies in pending.rktl (`find-next-space-position`,
    `group-event-pexp-text-string`, `relation-name`) and the graphics
    stand-ins move back to their graphics files as those are ported.
  - The gate is about 6 min.

---

## Iteration 13 — 2026-10-03 02:01
Item 12 (The SGL interpreter on racket/draw): **SOLVED**.

### Completed
- **Dissertation and screenshots** in `docs/reference/`:
  - `dissertation.pdf` (306 pages, 2.2 MB), fetched from Marshall's site.
  - `figures/`: all 197 raster images larger than 100 × 100 pixels, extracted
    unchanged with `pdfimages -png -p` and named by page (`pPPP-NNN.png`),
    1.2 MB.
  - `README.md` indexes them by panel: Workspace (Figs. 2.4–2.6, 3.1,
    3.11–3.18 and the Chapter 5 runs), the Trace's event views, Answer
    Description, Slipnet (Copycat's, Fig. 1.2) and the concept-pattern
    views, Coderack, Top/Bottom/Vertical Themes, Temporal Trace,
    Commentary, Episodic Memory. No figure shows the Temperature or EEG
    windows on their own.
- **`racket/gui/sgl.rkt`**: sgl-interpreter.ss on racket/draw. It requires
  racket/draw and never racket/gui, so it renders offscreen.
  - The interpreter (`draw!`, `erase!`, `draw-exps`, `draw-exp`, `lookup`,
    `extend`, `extend*`, `init-env`, …) is line for line the original's.
    racket/class's `send` has SWL's syntax.
  - SWL's `<viewport>` becomes `viewport%`. It has the same `draw-...` methods
    and guards. Where the original created a Tk canvas item, it records the
    same item (kind, pixel coordinates, options, tags) in a display list.
  - Tag operations work on that list as Tk's do: `move`, `move-pixels`,
    `raise`, `unhide`, `retag`, `rescale`, `delete`. So do `clear` and
    `draw-hidden-filled-rectangle`.
  - `render dc` paints the list on any `dc<%>`, emulating Tk on X11:
    - shapes are aliased;
    - Tk's dash strings follow tkCanvUtil.c's `DashConvert`;
    - pie slices are drawn with their radii;
    - text is anchored at its bottom centre;
    - thin lines stop one pixel short, as X11's do.
  - The GUI items get hooks: a change callback, a scroll position for
    `mouse-press`, and `set-flush-event-queue!`.
- **`racket/gui/fonts.rkt`**: fonts.ss.
  - SWL's `<font>` becomes `swl-font%`. Tk sizes are kept: points (converted
    at a fixed 96 dpi) or negative pixels.
  - Faces go to Pango with a family fallback. `select-face` falls back to
    times/helvetica here, as it would have under Tk.
  - Text is measured on a private bitmap dc instead of the logo window's
    hidden canvas. `make-mfont` and `make-fixed-font` are verbatim apart
    from that measuring line.
  - **`racket/gui/colors.rkt`**: `swl-color`, the 752-entry
    `*color-names*` and `=white=` … `=orange=` from constants.ss. Colours
    are immutable `color%` objects.
- **Tests**:
  - **Differential battery**: `tests/diff/sgl-battery.scm` (50 tests) with
    `racket/tests/sgl-diff-test.rkt`.
    - The prelude's `send` drops its arguments. So the Chez side
      (`tests/diff/sgl-chez-setup.ss`) redefines `send` to record, and
      reloads the *original* sgl-interpreter.ss against a recording
      viewport. `diff-runner.rkt`'s `check-battery` got a `#:chez-setup`
      option for this.
    - The Racket side is `racket/tests/sgl-recorder.rkt`.
    - Every viewport message and every argument must match the original's:
      every form, every `let-sgl` binding, nested and rational origins,
      justifications, erasing, tags, `clear`, `rule`, invalid expressions
      and the environment.
  - **`racket/tests/sgl-test.rkt`** (71 checks):
    - colours, dash conversion and fonts;
    - the recorded items, including text centre and baseline exactly as
      `draw-text` computes them;
    - every tag operation, mouse presses, change callbacks;
    - painted pixels (fill, outline, background, hidden items, a dashed
      line's 6-on-6-off pixels);
    - that racket/gui/base is never declared;
    - **a pixel-for-pixel snapshot** of `racket/tests/sgl-fixture.rkt`
      against `racket/tests/snapshots/sgl-fixture.png`.
      `METACAT_UPDATE_SNAPSHOTS=1` regenerates it, and a mismatch writes
      `/tmp/sgl-fixture-actual.png`.
    - The fixture covers every SGL form plus move/raise/delete/retag/
      hidden/unhide. `racket racket/tests/sgl-fixture.rkt OUT.png` renders
      it by hand.
- Tests first, honestly:
  - The battery and the Chez setup came first. Under Chez they printed 50
    results, which showed that `init-env` has no background colour and that
    erased items are tagged `eraser`.
  - Next came the recorder and the Racket test. That test failed: `cannot
    open module file … racket/gui/sgl.rkt`.
  - After the port, all 50 tests agreed on the first run.
  - sgl-test.rkt and the fixture were written after the code. They then
    found three rendering bugs, fixed below.
- **What I saw in the renderings** (Read on the PNG and on 3–4× crops):
  - First render: every cell came out right. Problems found:
    - The label text had colour fringes: `'smoothed` is subpixel on this
      desktop, so the port now uses `'partly-smoothed`, i.e. greyscale. I
      checked that no pixel of the label has a hue now.
    - The dash test showed 7-pixel dashes, because racket/draw's 1-pixel
      lines include their end point.
  - Fixing the dashes by shortening every segment wiped out the flattened
    arcs (sub-pixel segments). Now each whole run, whether a path or a
    single dash, stops one pixel step short.
  - The width-2 circle looked octagonal. Solid arcs and ovals now use
    racket/draw's own curves.
  - Final image: dashed and dotted rectangles, a width-3 red square with
    mitred corners, the arc, the oval, the dashed lower half-arc and the
    pie are all clean. Polypoints are single pixels, and dashed polypoints
    are 4-pixel dashes.
  - The three justifications sit correctly against the red guide lines;
    `text-relative` offsets by M-widths, and the image-mode text has its
    background box.
  - Of the erase cells, only the drawn parts are covered. The nested-origin
    squares step as intended.
  - Moved, raised, deleted (including a retagged text item) and
    unhidden items all behave as expected.
  - Against the dissertation's Workspace screenshots (e.g. figures
    p077-093, p150-280): the same aliased one-pixel lines, bold italic serif
    letters and short-dash rule boxes.
- **Mutation checks**: 14 mutations of sgl.rkt, each restored afterwards. All
  are caught, by the battery, the pixel tests or both (table in
  porting-notes.md). The last of them exposed a real bug: filled ovals
  ignored their outline colour, which matters for rings. Fixed.
- Docs:
  - porting-notes.md: new item 12 section.
  - divergences.md, its first entry: Tk's canvas is emulated on racket/draw,
    with fonts at a fixed 96 dpi, measuring without the logo window, and
    `get-actual-values` reporting the request.
  - anomalies_and_quirks.md, three new entries:
    1. `init-env`'s default font is a bare SWL font that `draw-text`
       cannot `tell` (latent; the port fails the same way).
    2. racket/draw's thin lines include their end point.
    3. Erasing adds items instead of deleting them.
  - chez_scheme/oracle/README.md mentions the new battery.
- `python3 ralph_loops/loop0001/gate.py`: **GATE PASSED** (`raco test
  racket/`: 1112 tests). `chez_scheme/original/` is untouched, and so are
  the goldens.

### Blockers
- None. Not done here, on purpose:
  - `create-mcat-logo` (a racket/gui window) waits for the control panel;
    until then `*scrollbar-width*`/`*scrollbar-height*` are `#f`.
  - The panel colour and font constants of constants.ss come with the
    panels.

### Next
- Item 13 (Workspace, bridge, group and rule graphics). Notes:
  - general-graphics.ss's `make-graphics-window` should create a
    `viewport%` with its `pixel->x` … `y->pixel` procedures. It must keep
    `width-per-pixel`/`height-per-pixel` mutable, because the viewport
    calls the procedures afresh for every item. racket/tests/sgl-fixture.rkt's
    `make-test-viewport` copies those formulas.
  - The panel files read engine globals and the engine's pending stand-ins
    (`find-next-space-position`, `group-event-pexp-text-string`,
    `relation-name`, the colour and font `#f`s). One option is a GUI module
    that requires engine.rkt and sgl.rkt and includes the panel `.rktl`
    files. Graphics then reach the engine only through its hooks, and the
    headless goldens must still match with views attached.
  - Always bind a font before `text` (see anomalies).
  - For PNG inspection: `render-viewport` in sgl-fixture.rkt shows how to
    paint a viewport into a bitmap.

---

## Iteration 14 — 2026-10-03 02:46
Item 13 (Workspace, bridge, group and rule graphics): **SOLVED**.

### Completed
- **Where the code went** (porting-notes.md, item 13). The model calls the graphics
  files directly when `%workspace-graphics%` is on, so the port splits them by need:
  - **Engine** (no racket/draw): general-graphics.ss's pexp builders and text helpers
    (`engine/general-graphics.rktl`), and group-, bridge- and rule-graphics.ss
    (`engine/*-graphics.rktl`, verbatim, included in load order).
    - `engine/view-globals.rktl` declares as `#f` what the model reads but the
      graphics define: constants.ss colours, `=white=` …, `*fg-color*`, the 11
      Workspace fonts, `restore-current-state`. All are in `set-global!`.
    - pending.rktl has no pending procedures left. The early copy of
      `find-next-space-position` is gone.
  - **Views**: `racket/gui/views.rkt` (racket/draw, no racket/gui) includes
    `gui/constants.rktl` (constants.ss's graphics part), `gui/general-graphics.rktl`
    (`make-graphics-window`, the text window) and `gui/workspace-graphics.rktl`.
    - `racket/gui/engine-route.rkt` gives it a `define`/`set!` that, for names
      imported from the engine, expand to `set-global!`. So the included files stay
      verbatim, and loading views.rkt installs colours and fonts in the engine, as
      metacat.ss loading the files did.
    - SWL's toplevel and frame become a window host: offscreen `window-host%` now,
      replaceable with `set-window-host-maker!` by item 15.
    - `attach-workspace-view!` makes the window as `(setup)` does and turns workspace
      graphics on, with gui.ss's speed settings at full speed and no flashing.
      `window->bitmap` and `save-window-png` take pictures.
  - `racket/headless.rkt`: `run-problem` takes `#:views thunk`. The headless EEG
    windows accept the two messages that workspace graphics add.
- **Port changes** (marked `port:`):
  - `update-rule-pexps!` returns a copy instead of `set-car!`, and the window stores it;
  - in `make-graphics-window`: the host, `viewport%` with `set-scroll-region!`, the
    title, and scrolling without waiting for a Tk scrollbar;
  - SWL thread and queue stand-ins on Racket threads.
- **Bugs found and fixed on the way:**
  1. compat's `record-case` applied a lambda, so extra arguments raised. Chez binds
     with `car`/`cdr` and ignores them, and trace.ss sends the Workspace window
     `draw-string-letters` with an extra tag. Every run with graphics on crashed at
     its first answer until this was fixed.
  2. Erasing by overpainting left grey fringes around antialiased text (a ghost "?",
     smears after concept mappings that changed font). Text is now aliased, as X11's
     core fonts were in the dissertation's screenshots (divergences.md). The
     sgl-fixture snapshot was regenerated after I inspected it (only text pixels
     changed).
  3. `viewport%` appended each item to its display list, which is quadratic over a
     run. It now prepends; `get-items` and `render` still see oldest first.
  4. Stale `.zo` files made the first gate run fail (39 failures in cli-test.rkt), so
     tests/run-tests.sh now runs `raco make` on every module before `raco test`.
- **Tests**:
  - `tests/diff/graphics-battery.scm` (50 tests) + `racket/tests/graphics-diff-test.rkt`,
    Chez with the original vs the engine:
    - every pexp builder, with flonum coordinates bit for bit (`make-polar`, `angle`
      and `acos` agree);
    - group and bridge pexps at every proposal level, gropes, and every
      `group-graphics`/`bridge-graphics` operation against a recording window;
    - `initialize-rule-graphics`, `make-new-rule-pexp`, `update-rule-pexps!`,
      `new-bridge-label-number`.
  - **`racket/tests/workspace-view-test.rkt`**:
    - **All 109 golden runs with the Workspace window attached give traces identical
      to tests/golden/ (watching changes nothing).** The windows were drawn into: over
      10,000 display items in all.
    - The original's crash run crashes at the same point, with the same trace, with
      the view attached.
    - Pixel snapshots of six scenes (`racket/tests/snapshots/workspace-*.png`).
    - views.rkt never loads racket/gui.
    - About 50 s. Runs go through the new `racket/tests/golden-pool.rkt`, which
      golden-test.rkt now uses too.
  - compat-test.rkt: `record-case` with extra, missing and rest arguments.
    engine-test.rkt: the builders and view globals.
- **Tests first, honestly:**
  - The `record-case` tests were written first and failed (2 of 37, arity mismatch).
  - The port of the graphics code came before the battery and the view test: I needed
    a working window to see what to test. The battery agreed on the first run once its
    fakes stopped calling slipnodes (which are procedures) as methods. Before that, 4
    tests were `ERROR` on both sides, which I noticed and fixed.
  - The view test's first runs failed on harness problems: a stale `.zo`, bitmaps from
    another namespace's class system, and snapshots that didn't exist yet.
- **Mutation checks**, each restored afterwards:

  | Mutation | Caught by | Failures |
  | --- | --- | --- |
  | group arrowhead angle 60 → 45 | battery | 2 |
  | bridge arc height 15/100 → 20/100 | battery | 2 |
  | dashed-line minimum dashes 3 → 2 | battery | 1 (only after adding `dashed-line-short`) |
  | rule y-extra 3/5 → 1/2 | battery | 1 |
  | centered zigzag `opp` swapped | battery | 4 |
  | bridge label numbering off by one | battery | 1 |
  | letter font 28/600 → 27/600 | snapshots | 6 |
  | answer's top rule drawn in the bottom colour | snapshots | 3 |
  | window `draw` ignoring `*fg-color*` | snapshots | 5 |
  | `bridge-graphics` draws one random number | goldens with views | 119 |
  | `record-case` back to applying a lambda | goldens with views | 103 |

- **What I saw in the renderings** (Read on each PNG, plus 4× crops):
  - `mrrjjj-513` (`abc abd mrrjjj` seed 1, at the 513 codelets of the dissertation's
    figure p224-541): the same layout as that figure. Bold italic serif letters, the
    double arrows, top bonds as elliptical arcs and a dotted proposed one, a group box
    around `abc` with its arrowhead, groups `R` and `J` with letter-category labels,
    zigzag vertical bridges with yellow number labels, a dotted proposed bridge, and
    the concept-mapping lists (`¹lmost=>lmost letter=>letter`) at the bottom left.
  - `mrrjjj-answer`: the answer `mrrjjk`. Top bridges in red, bottom in blue, the
    slipped vertical bridge in violet, the rules in their double-bordered red and blue
    boxes.
    - First render: a ghost "?" and smeared superscripts, which led to aliased text.
    - Clean after the fix. Label "3" looked boxless at 1×, but the crop shows its
      yellow box.
  - `xyz-snag-event`: the first try had "Event 5: Snag" over "Workspace", because the
    scene skipped the event's `clear`. Fixed in the harness. Now it looks like the
    dissertation's event views (p233-621): faded grey structures, the snag object `z`
    and the translated rule in orange, `???` in orange.
  - `xyz-answer` and `xyz-answer-description`: run7's `wyz`, as in Figs. 5.10/5.11.
    Crossed vertical bridges, the spanning-group bridge with its concept-mapping list,
    `pred=>succ` highlighted in magenta. The description adds green theme-supporting
    mappings under the title "Answer Description".
  - `xyd-justify`: the justify run's answer string `xyd` with its bonds.
  - Italic digits overhang their yellow boxes by a pixel (anomalies; the box
    arithmetic is the original's).
- Docs:
  - porting-notes.md: new item 13 section;
  - divergences.md: windows, aliased text, speed settings;
  - anomalies_and_quirks.md: `record-case`, text fringes, image-text boxes, graphics
    state without random draws, shared rule pexps, the stale-`.zo` update;
  - chez_scheme/oracle/README.md.
- `python3 ralph_loops/loop0001/gate.py`: **GATE PASSED** (`raco test racket/`: 1438 tests;
  about 6 min). `chez_scheme/original/` untouched;
  tests/golden/ untouched.

### Blockers
- None.

### Next
- Item 14 (the other panels). Notes:
  - Add each graphics file to views.rkt's includes in load order.
  - Names the engine reads move from pending.rktl to view-globals.rktl (and
    `set-global!`): `%coderack-codelet-count-font%`, the EEG's `*EEG*`, and the early
    copies `group-event-pexp-text-string` and `relation-name`.
  - engine-route.rkt's `define` installs them.
  - Each window is made with `make-graphics-window`, which gives an offscreen host;
    `window->bitmap` takes its picture.
  - Attach views through `run-problem`'s `#:views`, and keep workspace-view-test's
    "goldens with views" check, extended to all the panels.
  - The Trace's and Memory's `display` methods drive the Slipnet, Themespace,
    Coderack and Temperature windows, so their views are testable once those exist.
  - The gate is about 7–8 min.

---

## Iteration 15 — 2026-10-03 04:50
Item 14 (The other panels): **SOLVED**. Every panel of the original program is
ported. With all eleven windows attached, all 109 golden runs still match byte for
byte.

### Completed
- **Ported** slipnet-, coderack-, temperature-, theme-, trace-, memory-, commentary-
  and eeg-graphics.ss as `racket/gui/*-graphics.rktl`. They are included by
  racket/gui/views.rkt in metacat.ss's load order and are verbatim apart from `port:`
  changes:
  - fonts that the original creates by `set!` without ever defining them;
  - `relation-names-pexp`, which the original never defines (see the anomalies below);
  - comments where definitions moved to the engine.
- **Engine split**, following item 13: the parts the model uses without a window are
  verbatim in the engine:
  - `engine/trace-graphics.rktl`: `group-event-pexp-text-string`;
  - `engine/theme-graphics.rktl`: `relation-name`;
  - `engine/eeg-graphics.rktl`: the EEG object, `%EEG-table%`, `*EEG*`.
  - The early copies and the pending `*EEG*` are gone.
  - `%coderack-codelet-count-font%` is a view global.
  - engine/pending.rktl now holds only gui.ss's speed settings.
- **`attach-views!`** (views.rkt) makes every window as `(setup)` does and turns every
  graphics switch on. The logo and the control panel wait for item 15.
- **racket/headless.rkt**:
  - Headless runs now use the engine's real EEG, as the oracle does.
  - The commentary and Trace-event recording moved into wrappers around whichever
    windows are installed, so the real Commentary and Trace windows can be attached.
    CLI output and goldens are unchanged.
- **Tests**:
  - `tests/diff/panels-battery.scm` (38 tests) + `racket/tests/panels-diff-test.rkt`:
    Chez with the original vs the engine + views.rkt. The two sides give identical
    output, and no test errors on either side (checked). It covers:
    - the thermometer and mercury pexps;
    - Themespace layout and names, and the horizontal and vertical panel layout
      procedures;
    - a theme panel object on a fake window with a fake Themespace (every drawing and
      update branch);
    - all seven Trace event icons;
    - Memory icons;
    - the Trace and Memory windows' mouse handlers;
    - the EEG object over 47 recordings;
    - the Slipnet layout table.
  - `racket/tests/views-test.rkt` (item 13's workspace-view-test.rkt, renamed):
    - **all 109 golden runs with every window attached give traces identical to
      tests/golden/**, and every window is drawn into;
    - the crash run crashes at the same point;
    - **48 pixel snapshots over 8 scenes**, including a click on a clamp event through
      the original Trace press handler, and clicks comparing two answers through the
      Memory press handler.
    - Item 13's six Workspace snapshots are pixel-identical with all panels attached.
  - engine-test.rkt: the moved definitions and the real EEG.
- **Tests first, honestly**:
  - The panel files and a first rendering of each window came before the tests.
  - Against a scratch worktree of HEAD with only the new tests, the battery fails
    (`compute-horizontal-panel-info: undefined`) and the view test does not compile
    (`attach-views!: unbound identifier`).
  - With the port, the goldens with all views matched on the first run.
  - My own first tests failed:
    - a blank-picture check on windows a scene leaves empty;
    - a too-high item threshold for short commentaries;
    - a Memory battery test that cleaned the icon procedure before calling it (ERROR
      on both sides, which I caught by reading the Chez output).
- **Mutation checks**: 14 mutations, all restored afterwards (the table is in
  porting-notes.md, item 14).
  - 13 are caught, by the battery, by the pictures or by both.
  - The answer-icon sizing mutation passed the battery until the fake window's text
    widths were made large enough for the minimum width not to win.
  - One is equivalent: the Memory's first icon spacing, which `initialize` always
    recomputes.
  - **A Slipnet window that draws one random number in `update-graphics` makes all
    109 goldens-with-views differ.**
- **What I saw** (Read on every PNG, with crops, against the dissertation's figures):
  - Slipnet matches Fig. 1.2's 13×5 grid; after the clamp click it shows "Concept
    Pattern".
  - Coderack matches Fig. 4.8 (p169-312, p237-649): two-line labels, counts, bars,
    "100 Total", the last codelet type highlighted. After the clamp click it shows
    "Codelet Pattern".
  - Top and Vertical Themes match Fig. 4.1's panel order and two-column vertical
    layout. A seemingly missing outline was only lost in downscaling.
  - Trace icons match Fig. 4.13, and the clicked clamp is highlighted.
  - Memory matches Fig. 4.17, with the clicked answer black with yellow.
  - The Commentary shows the answer comparison as in Chapter 5. The margin is one
    space, 4 px.
  - Thermometer and EEG: no reference figures. Both look right; a crop showed that
    the EEG verticals are pure red.
- Docs:
  - porting-notes.md: new item 14 section;
  - anomalies_and_quirks.md: `relation-names-pexp` is never defined; the dead Memory
    spacing; updates to the default-font, graphics-coupling and draws-nothing entries;
  - divergences.md: test name;
  - chez_scheme/oracle/README.md: the new battery.

### Blockers
- None.

### Next
- Item 15 (the control panel and main window). Notes:
  - `attach-views!` builds every window but the logo and the control panel. Each
    window's host is offscreen; `set-window-host-maker!` installs on-screen hosts.
  - engine/pending.rktl's last stand-ins are gui.ss's speed settings
    (`%num-of-flashes%` …). The control panel's speed slider sets them;
    `attach-views!` sets them to full speed for now.
  - The theme edit mode (`*theme-edit-mode?*`, theme-graphics.rktl) and the control
    panel messages the handlers send (`ready-to-edit?`, `edit-theme-type`,
    `raise-theme-edit-dialog`) are gui.ss's.
  - Mouse handlers: views-harness.rkt shows how to click through the original
    handlers. `thread-break` in views.rkt still raises: the engine thread is item 15's.
  - views-test.rkt now takes about 70 s, and the gate about 8 min.

---

## Iteration 16 — 2026-10-03 04:10
Item 15 (The control panel and windows): **SOLVED**. `racket racket/main.rkt` opens the
control panel and every window on the screen. Runs driven through the panel's own widgets
on a virtual display match the goldens: a full run, step mode, Stop and Go, a breakpoint
resumed by a click on the Workspace, and Reset.

### Completed
- **gui.ss → `racket/gui/gui.rktl`**, included by the new **`racket/gui/gui.rkt`**
  (the only module besides tests that requires racket/gui).
  - Verbatim: the command-line parser, the button, breakpoint, step-interval,
    save-commentary and speed actions, every message of the control panel object, the
    window controllers and the clamp menu logic.
  - Rewritten on racket/gui (marked port:): the widgets, the menus (Help, Demos,
    Windows, Options, Memory), the dialogs (confirm, input) and the help window.
  - Widget colours and menu-item fonts are left out (divergences.md).
- **setup.ss's `setup` and `enable-resizing` → `racket/gui/setup.rktl`**, and
  `create-mcat-logo` in gui.rkt. **demos.ss → `racket/engine/demos.rktl`**, verbatim.
  `racket/main.rkt` runs `setup`, taking an optional scale argument.
- **On-screen windows**: `screen-host%` is a frame with a canvas painting the viewport's
  display list, installed through views.rkt's window-host maker, so items 13–14's window
  code is unchanged.
  - A 50 ms timer repaints changed windows and keeps Tk-style scrollbars (shown only
    when needed) in step with the scroll region.
  - Mouse presses go to the original press handlers.
  - Resizing a frame goes through the original resize handler and listener thread.
  - `arrange-windows!` tiles the windows (the original left that to the window manager).
- **The engine thread** stands for the REPL thread. `thread-break` hands it thunks
  (init-mcat + run-mcat, `go`); each runs inside a prompt until `break` → `(reset)`.
  Model errors return the panel to input mode.
- **Bugs found and fixed on the way**:
  1. **GTK ignored Xvfb** because the owner's session sets `WAYLAND_DISPLAY`. My first
     three scratch runs (a few seconds each) opened windows **on the owner's screen**
     before I noticed: `xwininfo` showed an empty Xvfb root and the screen grab was
     black. Every GUI run now uses `env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run`
     (tests/run-tests.sh, CLAUDE.md), and the GUI test refuses to run if
     `WAYLAND_DISPLAY` is set. Logged in anomalies, and saved in my memory notes.
  2. views.rkt's SWL message-queue stand-in **deadlocked the GUI thread**: a receive
     took the semaphore and the message in two steps. A receive is now atomic, and
     `critical-section` runs in atomic mode.
  3. Scrollbars appearing after layout overlapped the Commentary's text: racket/gui's
     `on-size` doesn't report them. Also, the original's single resize queue keeps only
     the last of simultaneous resizes, so the Commentary never reflowed and kept a stale
     scroll position. Now the tick watches the client size, and scrollbars are shown
     before windows become resizable, so frames grow around them.
- **Tests**:
  - **`racket/gui-tests/control-panel-test.rkt`** (105 checks, about 9 s on Xvfb, run
    by tests/run-tests.sh; racket/info.rkt omits gui-tests from plain `raco test`). It
    drives the panel's real widgets: Enter in the command line, the Step/Go/Stop/Reset
    buttons, the slider, menu items, dialogs, and clicks on the Workspace canvas.
    - Setup: 12 window controllers, with EEG and Logo hidden. Every window is a visible
      on-screen frame and none overlaps the control panel.
    - Invalid input turns the label red and restores it after 700 ms. The speed slider
      at Fast gives the original's settings.
    - **Full run** (`abc abd ijk` seed 1): Enter initializes and stops at 0 (the
      original's `quiet-break`), Go runs to **395 codelets with the golden's generator
      state**. Printed output and commentary checked.
    - **Step mode**: Step gives 1 codelet; the step interval is set to 50 through the
      Options dialog; then 50, 100, and Go finishes at the golden state.
    - **Breakpoint** 300 set through its dialog: Go stops with "Codelets run: 300",
      Clear breakpoint works, and a **click on the Workspace resumes** (the original's
      handler calls `go`) to the golden state.
    - **Stop and restart** (run 7, 2170 codelets): the panel is in run mode ("running...",
      Stop enabled), Stop interrupts mid-run, Go resumes to the golden state; **Reset**
      re-initializes (0 codelets, seed state) and Go matches the golden again.
    - Clear Memory through its confirm dialog (the panel is disabled while it's up).
      Save commentary to file writes exactly the window's lines.
    - Windows menu hide/show; Self-watching off and on (the warning label and the theme
      windows follow); resizing the Workspace frame resizes the window; a Demos item
      initializes Run 7 and is checked.
  - Mutation checks, each restored afterwards:

    | Mutation | Failures |
    | --- | --- |
    | Stop does nothing | 3 |
    | init without `quiet-break` | 18 |
    | speed 100 gives 2 flashes | 1 |
    | resize without `configure` | 3 |
    | demo item not highlighted | 1 |
    | command line not cleared on a new problem | 0 (equivalent: `switch-to-input-mode` clears it) |

  - Tests first, honestly: no. A scratch script drove the first on-screen run (it found
    bugs 1 and 2) before the test existed, and the test was written against working
    code. Without the port the test cannot load (`racket/gui/gui.rkt` does not exist).
    Its first run failed 7 checks, all harness mistakes (output capture, a racy
    run-mode check, a too-strict layout check), and it hung at exit until the refresh
    timer was stopped.
- **What I saw** (screenshots of the whole 1920×1200 virtual screen with
  `tests/gui-screenshot.rkt`, read with the Read tool and cropped):
  - `abc abd mrrjjj` seed 1 with a breakpoint at 513: the Workspace is the same as item
    13's offscreen `mrrjjj-513`.
    - The control panel (top left) shows the menu bar, " abc -> abd; mrrjjj -> ?  seed: 1",
      an azure command line, the Slow/Speed/Fast slider, and Step/Go/Reset enabled
      with Stop disabled.
    - Temperature under the panel; Coderack and Commentary to the right of the
      Workspace; Slipnet, the theme windows and Memory in the second row; the Temporal
      Trace with its horizontal scrollbar in the third row.
  - First shot: the Commentary's first line ran under the scrollbar ("wha|t"), which led
    to fix 3. After it, run 7's `wyz` screen shows the commentary wrapped inside the
    window and scrolled to the newest paragraph. The Trace shows Identity, x-y-z, a-b-c,
    Top Rule, SNAG, Clamp and Opposite events, and the Memory shows "SNAG" and "wyz",
    as in Figs. 5.10/5.11.
  - Xvfb has no window manager, so there are no title bars. The Vertical Themes window
    (590 px tall) ends about 30 px below a 1200-px screen.
- Docs: porting-notes.md (item 15 section); divergences.md (the racket/gui control
  panel, layout, logo, engine thread); anomalies_and_quirks.md (Wayland vs Xvfb,
  `on-size` and scrollbars, the queue deadlock, one resize queue for all windows);
  CLAUDE.md (the GUI test command).
- `python3 ralph_loops/loop0001/gate.py`: **GATE PASSED** (`raco test racket/`: 2557 tests;
  GUI tests: 105). `chez_scheme/original/` and `tests/golden/` untouched.

### Blockers
- None. Not done: theme edit mode (Clamp theme pattern) has no automated test beyond
  compiling; its dialog and the press handlers are ported. Colours of native widgets
  can't be set in racket/gui.

### Next
- Item 16 in iterations.md. Notes:
  - Run anything that loads racket/gui as
    `env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a -s "-screen 0 1920x1200x24" ...`.
  - `tests/gui-screenshot.rkt OUT.png abc abd xyz 7 [--break N]` grabs the screen.
  - The control panel answers `get-widgets` for driving it.
  - The gate is about 8–9 min.
  - demos.ss is already ported (`racket/engine/demos.rktl`, needed by the Demos menu);
    item 16 needs only its tests and the seed caveat.
- Naming: the item's Run and Pause are the original's Go and Stop. Stop interrupts, and
  Go resumes the stopped run. The original has no separate Pause button, so the port
  keeps its four buttons. The seed is typed after the problem ("abc abd xyz 7"), as in
  gui.ss.

---

## Iteration 17 — 2026-10-03 04:40
Item 16 (Demos, packaging, README): **SOLVED**. The standalone program, built with
`raco exe` + `raco distribute`, runs `abc abd xyz` from a clean directory, in a sandbox
without Racket. It prints what racket/cli.rkt prints and writes the Run 7 golden trace
byte for byte. It also opens the GUI.

### Completed
- **Demos.** demos.ss was already ported verbatim (`racket/engine/demos.rktl`, item 15).
  - **The seed caveat**, written up in **`docs/demos.md`**: a table of every demo with
    what demos.ss's comments or the dissertation's Chapter 5 document (answer, time
    step, page), against what the oracle and the port do. A subagent read Chapter 5.
  - **These replay:** Runs 1, 2, 3, 4, 5 and 7, Figs. 5.7/5.8, misc1/2/4/5/9.
    - Run 4 was listed in item 01 as not replaying. It does: the dissertation's Run 4
      also gives up at 3228.
  - **These don't:** Run 6 (5976 vs 6196), Run 8 (qeeeq at 1013 vs no answer),
    fig5.5-bottom (qxeeq vs qeeq), fig5.11 (a continuation of another run), eqe-qeeeq
    (qcccb), misc3 and misc6–8.
  - The anomalies entry and porting-notes are corrected. demos.md also explains why the
    port follows the oracle, and repeats demos.ss's advice to clear the Memory first.
- **The standalone program.**
  - `racket/metacat.rkt`: no arguments (or a scale) opens the GUI; otherwise it is the
    CLI (cli.rkt now provides `cli-main`); `--help`.
  - `make-dist.sh [DEST]` (default `build/metacat`, gitignored) runs raco make, then
    `raco exe --gui`, then `raco distribute`, and adds README and LICENSE. About 6 s,
    70 MB.
  - **Bug found:** main.rkt's `dynamic-require` of a runtime path isn't embedded by
    raco exe, so the distributed GUI exited 1. main.rkt and metacat.rkt now use
    `lazy-require`. Requiring them still doesn't load racket/gui.
- **README.md** rewritten:
  - what Metacat is (Copycat's successor, self-watching);
  - credits (James Marshall; Melanie Mitchell's Copycat, FARG) and the GPL v2+;
  - running the GUI (problem syntax, seeds, justify, buttons, Demos), the CLI and the
    standalone program;
  - the oracle method in five steps, tests and their requirements, layout, the loop;
  - three screenshots in `docs/screenshots/`.
- **Tests**:
  - **`racket/gui-tests/dist-test.rkt`** (run by run-tests.sh under xvfb-run; about
    10 s). It builds the distribution into a temporary directory and runs it from
    another empty directory with a minimal environment. When `bwrap` exists (it does
    here), the program runs in a sandbox where /usr/share/racket,
    /usr/lib/x86_64-linux-gnu/racket, /home (so the repo) and /tmp are empty.
    - `metacat abc abd xyz --seed 3852097033 --max-codelets 10000 --trace F`: exit 0,
      stdout equal to cli.rkt's, F equal to the golden;
    - `metacat abc abd xyz` with a clock seed answers;
    - bad arguments exit 2;
    - `metacat` with no arguments opens the control panel and the 10 other windows
      (`xwininfo`), stays up and writes nothing to stderr.
  - **`racket/tests/demos-test.rkt`** (about 2.5 s):
    - the 35 demo problems equal the original's (read from demos.ss), and misc6–9 stay
      undefined;
    - every demo problem and seed is a golden run;
    - 12 documented outcomes replay in the port through the CLI (answers with codelets,
      and the final count).
  - **control-panel-test.rkt** (now 211 checks): each of the 35 Demos menu items, in
    gui.ss's order, loads its problem and seed (codelets 0, generator state = seed,
    info label) and is the only item checked.
- **Tests first, honestly:** no. metacat.rkt and make-dist.sh were spiked first, by
  building and running the exe by hand. The tests came after, but each was checked to
  fail against broken code:
  - with HEAD's main.rkt, dist-test fails 2 of 3 ("the GUI is still up": actual 1);
  - with the GUI dispatch disabled in metacat.rkt, it fails 1;
  - one changed seed in demos.rktl fails demos-test (the GUI test reads the engine's
    own value, so it doesn't catch that);
  - Run 3's menu item loading run4 fails 2 GUI checks.
  - My own first dist-test runs failed or hung on harness mistakes: a missing
    `--max-codelets` for the golden, a sandboxed GUI that outlived its killed bwrap
    (now `--die-with-parent`; anomalies), and a stdout check lost to SIGKILL buffering.
- **What I saw** (Read on the PNGs): the full-screen Run 7 shot shows `wyz` with both
  rules, crossed bridges, the Coderack, a commentary of snags and the answer, the
  Slipnet, themes, a Trace of snags and clamps, and Memory with SNAG and wyz, as in
  Figs. 5.10/5.11. The `abc abd mrrjjj` seed 1 Workspace at 513 codelets matches item
  13's picture and the dissertation's p224 figure.
- Docs: demos.md (new); anomalies (demo seeds revised; `dynamic-require` under raco
  exe; bwrap orphan); porting-notes (item 16 section, run4 note); CLAUDE.md
  (make-dist line).
- `python3 ralph_loops/loop0001/gate.py`: **GATE PASSED** (`raco test racket/`: 2560
  tests; GUI tests: 214). `chez_scheme/original/` and `tests/golden/` untouched.

### Blockers
- None. Only Linux was built and tested. On macOS and Windows, make-dist.sh's `raco exe
  --gui` makes an app or exe whose layout differs, and the sandbox paths in dist-test
  are Linux-only.

### Next
- Item 17 (final audit) in iterations.md. Notes:
  - docs/demos.md lists the demos that don't replay; the audit may want to say this in
    divergences.md (it is not a port divergence: the oracle agrees with the port).
  - The gate is about 10 min. The GUI tests now include a 70 MB build (dist-test), in
    a temporary directory that is deleted afterwards.

## Iteration 18 — 2026-10-03 04:58:28
### Completed
- (driver) session ended with outcome `ok` without marking the item
### Blockers
- see session_it18.log
### Next
- revisit or re-open this item

---
