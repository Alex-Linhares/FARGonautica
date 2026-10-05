# chez_scheme/oracle/: the original Metacat, headless, under Chez Scheme 10

This harness runs the **unmodified** Metacat 1.2 source in [`../original/`](../README.md)
under Chez Scheme 10 with no windows. It is the oracle for both ports. It prints a run's
commentary and answers exactly as the original produces them. It writes JSON-lines traces
of every codelet, structure, temperature, theme and event. It generated and re-checks the
109 golden traces in [`tests/golden/`](../../tests/README.md#golden-the-109-golden-traces),
and it evaluates the differential batteries in `tests/diff/` with the whole original
loaded. All adaptation happens from outside. A prelude supplies what the 1999–2017
environment (Chez Scheme 6–8, SWL 0.9, Tcl/Tk) provided and Chez 10 lacks. The tracing
wraps the original's top-level procedures after loading, and it never changes a run.

```
$ scheme --script chez_scheme/oracle/run.ss abc abd xyz --seed 3852097033
Problem: abc -> abd; xyz -> ?  seed 3852097033
Comment: Okay, if "abc" changes to "abd", what does "xyz" change to?  Hmm...
Comment: Uh-oh, I seem to have run into a little problem.  Changing the letter-category of the letter z to its successor is not possible in xyz.
...
Comment: All right, I've had enough of this!  Let's try something different for a change...
Comment: Looks like I made a lot of headway in coming up with new ideas.
Comment: The answer "wyz" occurs to me.  I think this answer is great!
Answer: wyz  quality 91  codelet 2170  temperature 15
Type (go) or click on the Workspace to continue...
Stopped: suspend
Codelets: 2170
Temperature: 15
Answers: (wyz)
```

(Run 7 of Marshall's dissertation; about 0.85 s.) The ports print the same bytes:
`racket racket/cli.rkt abc abd xyz --seed 3852097033` and
`python3 -m metacat abc abd xyz --seed 3852097033` (from `python/`).

## Files

| File | Lines | What it is |
|---|---:|---|
| `prelude.ss` | 318 | makes `../original/metacat.ss` load and run under Chez 10: `extend-syntax`, SWL stubs, settings, muteable stdout, headless windows, an error handler with a backtrace |
| `run.ss` | 177 | the command line: runs one problem, prints commentary, answers and a summary, and optionally writes a trace |
| `trace.ss` | 308 | the trace instrumentation and its JSON writer ([`docs/trace-format.md`](../../docs/trace-format.md)) |
| `make-golden.ss` | 197 | writes `tests/golden/*.jsonl` from `tests/problems.txt`, in parallel, or with `--check` compares them byte for byte |
| `diff-eval.ss` | 64 | evaluates differential batteries (`tests/diff/*.scm`) against the loaded original |
| `validate-trace.py` | 150 | checks that files are well-formed traces |
| [`tests/`](tests/README.md) | | the oracle's own checks, run by `tests/run-tests.sh` |

Requirements: Chez Scheme 10.0 (`scheme`, or `chezscheme` as Debian and Ubuntu name it;
the scripts try both) and Python 3 (for `validate-trace.py`). Run everything from the
repository root.

## `prelude.ss`: what Chez 10 lacks

`prelude.ss` is loaded before the original. Its sections:

1. **`extend-syntax`**, the old Chez macro system used by all 22 macros of
   `syntactic-sugar.ss`. It is rewritten with `syntax-case` and supports fenders and
   `with` templates. The `with` templates generate top-level names such as `plato-a` or
   `a-b-link`, which the Slipnet definition language relies on.
2. **Empty modules** for the five `swl:*` imports in `metacat.ss`.
3. **SWL stand-ins.** `make`, `create` (which ignores its `with` options) and
   `define-class` produce inert stub objects. `send` ignores every message except the
   few that must return a value (`get-actual-values`, `get-width`, `get-height`).
   `swl:font-families`, `swl:tcl-eval`, `swl:sync-display` and the others are stubs; the
   screen is 1280 × 1024. Threads are no-ops, except that `thread-kill` (the original's
   reaction to bad settings) exits with status 2. The oracle never calls `(setup)`, so
   no window is ever created; these stubs only serve load-time definitions of fonts and
   colours.
4. **Settings** that `metacat.ss` ships commented out: `*platform*` = `linux`,
   `*metacat-directory*` = `../original/`, `*file-dialog-directory*` = `/tmp/`.
5. **A muteable stdout**, so the load-time chatter (`Metacat loaded.` …) is hidden.
   `syntactic-sugar.ss`'s `printf` captures the port at load time and must still reach
   the real stdout afterwards. `load-metacat` loads `metacat.ss` with output muted and
   then restores the directory.
6. **Headless windows** (`install-headless-windows!`). This turns
   `%workspace-graphics%`, `%slipnet-graphics%` and `%coderack-graphics%` off and replaces
   each window global with a **null window**. A null window accepts exactly the messages a
   headless run sends and raises an error on any other message. A display call that could
   feed a value back into the model would therefore be noticed, not silently ignored.
   The Memory window gives each answer and snag an icon that draws nothing. Each codelet
   type gets a null Coderack-graphics reference. The Control Panel answers only
   `set-verbose-step-mode`. The **Commentary window is the original's**
   (`make-comment-window`), drawing on a recording text window, so the original itself
   decides how each paragraph is worded (including Eliza mode). Each paragraph goes to
   `$commentary` and to `$commentary-hook`.
7. **Errors**: a handler that prints the condition and up to 40 procedure names from the
   stack (Chez's script mode would print only the message), then exits 1.

The prelude does **not** touch the random-number generator.

## `run.ss`: the command line

```
scheme --script chez_scheme/oracle/run.ss INITIAL MODIFIED TARGET [ANSWER]
       [--seed N] [--max-codelets K] [--keep-going] [--trace FILE] [--verbose]
```

| Argument | Meaning |
|---|---|
| `INITIAL MODIFIED TARGET` | the problem, e.g. `abc abd xyz` for "abc → abd; xyz → ?" |
| `ANSWER` | a fourth string makes it a **justify** run (`%justify-mode%`): Metacat explains the given answer |
| `--seed N` | `1 ≤ N ≤ 4294967295`. Without it the seed comes from the clock (`randomize`), as in the original, and the run cannot be replayed |
| `--max-codelets K` | stop after codelet K, using the original's own breakpoint (`runtil`). No cap by default |
| `--keep-going` | don't stop at an answer. The original pauses for Go after each answer or give-up; this behaves as if Go were pressed at once, until the cap |
| `--trace FILE` | also write the JSON-lines trace to FILE. The run and its output are unchanged |
| `--verbose` | the original's verbose mode (the Options menu checkbox, `%verbose%`): the model's `vprintf` output is printed too |

What it does: it loads `prelude.ss` and `trace.ss`, then `load-metacat` and
`install-headless-windows!`. Then it calls `init-mcat` (what the Control Panel does for a
new problem) and `run-mcat`. The original's `break` and `quiet-break`, which wait for the
REPL, are replaced so that they end the run, or return at once under `--keep-going`.

Output, line by line:

| Line | When |
|---|---|
| `Problem: abc -> abd; xyz -> ?  seed N` | first. With an ANSWER, it is shown in place of `?` |
| `Comment: ...` | each Commentary paragraph, as the original draws it |
| `Answer: X  quality Q  codelet N  temperature T` | each answer. `N` is `*codelet-count*` when it is reported, so the answer came from codelet N+1. This is the count the original displays and the "time" quoted in `demos.ss` |
| `Type (go) or click on the Workspace to continue...`, `Codelets run: N` | the original's own console messages, at a pause or at the cap |
| `Ooops: bad message "M" sent to object of type T` | the original's `report-error-and-halt` (an object got a message it does not understand). The run ends there |
| `Stopped: suspend` / `cap` / `halt` | why the run ended: the original paused for Go (after an answer or giving up), `--max-codelets` was reached, or the original halted |
| `Codelets: N`, `Temperature: T`, `Answers: (…)` or `none` | the summary |

Exit status: 0 for a completed run (including `halt`), 2 for a usage error or a bad seed,
and 1 when Chez raises an error (with a backtrace on stderr). The golden set avoids the one
known case of a Chez error, `abc ccbbaa ijk` seed 3, which fails in
`transcribe-to-english` (rules.ss).

More examples:

```bash
scheme --script chez_scheme/oracle/run.ss abc abd xyz --seed 7 --max-codelets 20000
scheme --script chez_scheme/oracle/run.ss abc abd xyz wyz --seed 5              # justify wyz
scheme --script chez_scheme/oracle/run.ss abc abd xyz --seed 1 --max-codelets 4000 --keep-going
scheme --script chez_scheme/oracle/run.ss abc abd xyz --seed 3852097033 --trace /tmp/run7.jsonl
```

## `trace.ss`: the trace

`trace.ss` is loaded after the prelude. `(install-trace! port)` is called before
`init-mcat`, and `trace-start` / `trace-end` write the first and last lines. It works
entirely from outside the original:

- **Top-level procedures are wrapped with `set!`**: `build-bond`, `break-bond`,
  `build-group`, `break-group`, `build-bridge`, `break-bridge`, `build-description`,
  `update-temperature`, `update-slipnet-activations`, `abstract-answer-description` and
  `report-error-and-halt`. Callers reach them through their top-level bindings, so the
  wrappers see every call.
- **`*coderack*` and `*workspace*` are replaced by forwarding closures**. These note
  each codelet the Coderack hands out and each rule added to the Workspace. `self`
  inside the object is still the original object.
- **The null Trace window is replaced** by one that records each Temporal Trace event
  (answer, snag, clamp, rule, group, concept-mapping, concept-activation).
  `$commentary-hook` records Commentary paragraphs.

Every wrapper reads model state only through side-effect-free getters, then calls the
original procedure with the original arguments. Reading `(random-seed)` with no argument
does not draw. So a traced run is the same run, and
[`tests/trace-check.ss`](tests/README.md) checks that. The JSON writer prints exact
non-integer rationals as `"n/d"` strings and flonums in Chez's shortest round-trip form.
The twelve event types, their fields and their field order are specified in
[`docs/trace-format.md`](../../docs/trace-format.md). Example lines from
`tests/golden/abc-abd-xyz_3852097033.jsonl` (2,669 lines):

```
{"t":0,"ev":"start","format":1,"problem":["abc","abd","xyz",null],"seed":3852097033,"max_codelets":10000,"keep_going":false,"slipnodes":["a","b",...]}
{"t":2170,"ev":"end","reason":"suspend","temperature":15,"answers":["wyz"],"rng":4089168737}
```

## `make-golden.ss`: the golden traces

```bash
scheme --script chez_scheme/oracle/make-golden.ss            # (re)write tests/golden/
scheme --script chez_scheme/oracle/make-golden.ss --check    # regenerate into /tmp and compare
```

The script reads [`tests/problems.txt`](../../tests/README.md#problemstxt) (lines of the
form `INITIAL MODIFIED TARGET [ANSWER] | SEED ... | CAP [| keep-going]`). For every
problem and seed it runs `run.ss --seed S --max-codelets CAP [--keep-going] --trace ...`,
each in a fresh Chez process, with `nproc` runs in parallel (through `xargs -P`). It
writes `tests/golden/<strings joined by ->_<seed>.jsonl`, e.g.
`abc-abd-xyz_3852097033.jsonl`, and first deletes any other `.jsonl` file there. It fails
if any run fails or if a strings-and-seed pair is listed twice. With `--check` it writes
into a temporary directory instead and reports every file that differs, is missing, or is
not listed in `problems.txt`. It exits 1 on any difference and otherwise prints
`make-golden: all 109 golden traces reproduced byte for byte`.
[`docs/trace-format.md`](../../docs/trace-format.md) measures about 13 s on 32 cores.

Golden traces come only from here. **They are never edited by hand, and never
regenerated to make a failing port pass.**

## `diff-eval.ss`: differential batteries

```bash
scheme --script chez_scheme/oracle/diff-eval.ss tests/diff/helpers.scm tests/diff/utilities-battery.scm
```

`diff-eval.ss` loads the original through the prelude and reads each FILE in turn, form
by form. A form `(test NAME EXPR)` prints `NAME => <canonical value>` (the canonical form
comes from `b:canon` in `tests/diff/helpers.scm`), or `NAME => ERROR` if it raises. Any
other form is just evaluated. The script also defines `b:capture` (what a thunk prints)
and `b:set-global!` (`set-top-level-value!`, for batteries that set the original's
globals). It takes any number of files: the SGL battery needs
`tests/diff/sgl-chez-setup.ss` between `helpers.scm` and `sgl-battery.scm`. The same
battery is evaluated by the Racket port (`racket/tests/diff-runner.rkt`), and by the
Python port through fixtures captured with `python/oracle/capture.py`, and the outputs
must be identical line for line. The batteries and the tests that consume them are listed
in [`tests/README.md`](../../tests/README.md#diff-the-differential-batteries).

## `validate-trace.py`: trace structure

```bash
python3 chez_scheme/oracle/validate-trace.py tests/golden/*.jsonl
python3 chez_scheme/oracle/validate-trace.py FILE --require codelet,build,answer
```

The script checks that every line is a JSON object starting with `t` and `ev`, that `t`
never decreases, and that each event type has its fields, with their types and in their
order. It also checks that `start` comes first and `end` last, that there is one `codelet`
line per codelet (`end.t` of them for `cap`, `end.t + 1` for `suspend` and `halt`), and
that every `slipnet` line has one activation per slipnode. `--require` lists event types
that every file must contain. It is silent on success. At the first problem it says why
and exits 1.

## Randomness

The oracle uses **Chez 10's own global `random` and `random-seed`, unmodified**. The
ports reimplement them exactly. The global generator is a 32-bit linear congruential
generator:

    step(S) = (S * 72931 + 90763387) mod 2^32

`(random n)` for a fixnum `n` takes two steps and combines the high 16 bits of each (four
steps when `n` > 2^32 − 1). `(random 1.0)` takes four steps to build a 52-bit mantissa, so
it is exactly `M/2^52`. `(random-seed n)` accepts 1 ≤ n ≤ 2^32 − 1. The bit-level
specification, taken from Chez's C sources, is in
[`docs/trace-format.md`](../../docs/trace-format.md#randomness-plan-item-01), and
[`tests/rng-check.ss`](tests/README.md) checks it draw for draw. Two consequences:

- Chez 10's `make-pseudo-random-generator` objects (MRG32k3a, like Racket's `random`)
  are a **different** generator. Metacat never uses them, and the ports must not use
  their language's own `random`.
- This generator seems unchanged since 1999, so **the seeds in `demos.ss` still
  replay**: misc1, misc2, misc4, misc5 and the commented-out misc9 come out exactly as
  their comments say ([`tests/demo-replay-check.ss`](tests/README.md)). misc3 does not
  replay exactly, and neither do the "not used" misc6–misc8.

Every trace's `codelet`, `slipnet` and `end` lines carry `rng`, the generator state at
that point. A port that drifts can therefore be located to within one codelet.

## Evaluation order and `map` order

Reproducing the generator is half the job. The other half is making the draws **in the
same order**, and Chez does not evaluate left to right. Observed under `scheme --script`,
which is how the oracle loads the original:

| Expression | Order of evaluation |
|---|---|
| `(f (show 1) (show 2) (show 3))`, user procedure | 3 1 2 |
| `(list (show 1) (show 2) (show 3))` | 3 1 2 |
| 2-binding `let` at top level, `((lambda (x y) x) (show 1) (show 2))` | 2 1 |
| `(+ (show 1) (show 2))`, `(cons (show 1) (show 2))` (inlined primitives) | 1 2 |

Inside a procedure the order can differ again: a 2-binding `let` inside a lambda went
1 2. There is no simple rule, so every call with two or more effectful arguments is ported
in Chez's order and checked against the oracle. Chez's library **`map`** also has its own
order. For one or two lists it goes from the end towards the front in pairs (7 elements:
7 5 6 3 4 1 2). For three or more lists it goes last to first. When the compiler inlines
`map` over a literal or short quoted list, the order is the compiler's. Chez's `sort` is
its own merge sort, called as `(sort pred list)`. The details and each affected site are
in [`docs/porting-notes.md`](../../docs/porting-notes.md) and
[`docs/anomalies_and_quirks.md`](../../docs/anomalies_and_quirks.md). Because of this, the
differential batteries never put two side-effecting expressions in one call, and they
build the lists passed to `map` at run time.

## Checks

`bash tests/run-tests.sh` runs each `tests/*.ss` here with `scheme --script` from the
repository root. A check fails by exiting non-zero. Run one alone with, for example,
`scheme --script chez_scheme/oracle/tests/rng-check.ss`. See
[`tests/README.md`](tests/README.md).

## Related tools elsewhere

- `python3 tests/extra-seeds.py` runs `run.ss --trace` against `racket racket/cli.rkt
  --trace` on 20 non-golden seeds per problem line (720 runs) and compares the traces,
  output and exit codes byte for byte ([`docs/extra-seeds.md`](../../docs/extra-seeds.md)).
- `racket/tests/golden-test.rkt` and `racket/tests/cli-test.rkt` run `run.ss` live.
  `python/tests/test_cli.py` does the same for the Python port.
