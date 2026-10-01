# Numbo (Defays, 1987) on SBCL

This directory holds Daniel Defays' 1987 Numbo, a Franz Lisp + Flavors program
described in "Numbo: A Study in Cognition and Recognition"
(Fluid Concepts and Creative Analogies, ch. 3), ported to run on SBCL. It is a faithful port, not
a rewrite. The six original files run almost exactly as printed in 1987.
Compatibility layers supply the Franz Lisp and Flavors features the code
expects, and the coderack, which is missing from the printout, is
reconstructed from the chapter. Every deviation is logged in
[`PORTING_NOTES.md`](PORTING_NOTES.md).

The originals are in `../../numbo-digitized/*.l` and are never
modified.

## Requirements

- SBCL. Tested with 2.6.0. Install with `sudo apt install sbcl`.
- Nothing else: no Quicklisp, no graphics.

## Running it

All commands are run from `Software/Numbo/numbo-sbcl/`, the directory above this file.

### A solved puzzle in one command

Puzzle 3 of the chapter, the one recorded in `trace3.31`: reach 31 from the
bricks 3 5 24 3 14. With seed 18 it is solved after 1328 iterations:

```sh
sbcl --non-interactive --load src/load.lisp \
     --eval "(print (numbo::run-config '(31 3 5 24 3 14) :seed 18 :max-iterations nil))" \
     2>/dev/null
```

The output ends with:

```
Node PLUS28-3-V30 created
Done : Operation PLUS28-3-V30 has been applied 
to CYTO-BLOCK28-V29 ( 28) and to CYTO-BRICK1 ( 3)
to get CYTO-TARGET
Operation TIMES2-14-V29 has been applied 
to CYTO-BRICK5 ( 14) and to CYTO-BLOCK2-V28 ( 2)
to get CYTO-BLOCK28-V29
Operation PLUS2-3-V28 has been applied 
to CYTO-BRICK4 ( 3) and to CYTO-BRICK2 ( 5)
to get CYTO-BLOCK2-V28

(:OUTCOME :SOLVED :ITERATIONS 1328 :SEED 18 :PROBLEM-SOLVED 1)
```

This reads as 31 = (14 x (5 - 3)) + 3. A `PLUSa-b` node is named after its
operands and its result in whichever order it was built, so `PLUS2-3` here
means 5 - 3 = 2.

The chapter says Numbo did *not* solve puzzle 3 (p.152), and the port mostly
doesn't either: most seeds give up. Seed 18 is one of the few that succeed.
For a puzzle that every seed solves quickly, try one of these:

```sh
# 114 from 11 20 7 1 6: (20 x 6) - (7 - 1), as in the chapter's Fig. III-3, in about 45 iterations
sbcl --non-interactive --load src/load.lisp \
     --eval "(numbo::run-config '(114 11 20 7 1 6) :seed 1)" 2>/dev/null

# 6 from 3 3 17 11 22: 3 + 3
sbcl --non-interactive --load src/load.lisp \
     --eval "(numbo::run-config '(6 3 3 17 11 22) :seed 1)" 2>/dev/null
```

`2>/dev/null` hides about 1400 lines of compiler warnings, most of them about
the free variables the 1987 code uses as globals (see "Known limitations").
Leave it off to see them. The per-file load report
(`;; numbo load: <file> ok`) goes to stdout and stays visible.

### The original entry point

`config` (start.lisp) is the 1987 top level. It needs `init-chiffre`
(init.lisp) first, which sets the parameters:

```sh
sbcl --non-interactive --load src/load.lisp \
     --eval "(progn (numbo::init-chiffre) (numbo::config 114 11 20 7 1 6))" 2>/dev/null
```

`config` has no iteration cap, and it uses whatever `*random-state*` is
current. It returns after "Done :" or when it gives up.

### `run-config` (src/harness.lisp)

```lisp
(numbo::run-config '(target brick1 ... brick5)
                   :seed 1               ; seeds *random-state*; same seed, same run
                   :max-iterations 500   ; nil = no cap
                   :verbose nil)         ; t = print "About to post codelet" lines, as in trace3.31
```

It runs `(init-chiffre)` and then `config`, unchanged, and returns a plist.
`:outcome` is one of:

- `:solved`: "Done :" was printed;
- `:gave-up`: the coderack was empty after the last retry;
- `:capped`: the iteration cap was reached.

The default cap is 500, so pass `:max-iterations nil` for hard puzzles.

### Checking a solution (src/solution-checker.lisp)

"Done :" does not always mean the solution is right (see the `kill-block` gap
below). `check-solution` parses the printed decomposition and checks it. Every
leaf must be a brick with the right value, each brick may be used at most
once, and every step must be valid arithmetic that reaches the target:

```sh
sbcl --non-interactive --load src/load.lisp --eval "
  (let ((out (with-output-to-string (*standard-output*)
               (numbo::run-config '(31 3 5 24 3 14) :seed 18 :max-iterations nil))))
    (print (multiple-value-list (numbo::check-solution out '(31 3 5 24 3 14)))))" 2>/dev/null
```

This prints `(T NIL "31 = (14 x (5 - 3)) + 3")`.

It returns `(values valid-p reason expression)`.

### From a REPL

```lisp
(load "src/load.lisp")          ; path relative to Software/Numbo/numbo-sbcl/
(in-package :numbo)
(run-config '(116 20 2 16 14 6) :seed 3 :max-iterations nil)
```

After loading, `cl-user::*numbo-load-failures*` should be `nil`.

### Tests and reports

- `./tests/run-tests.sh`: the whole test suite. It runs headless and exits 0
  on pass. One of its checks, `tests/readme-test.sh`, runs every `sh` block
  in this README as written and compares the first one's output with the
  output shown above.
- `sbcl --non-interactive --load tests/chapter-runs.lisp`: regenerates the
  tables in [`RESULTS.md`](RESULTS.md), 11 chapter puzzles × 20 seeds, in
  about 2 s.

## Layout

| File | What it is |
|---|---|
| `pnet-def.lisp`, `pnet-functions.lisp`, `pnet-graphics.lisp`, `cyto-def.lisp`, `codelets.lisp`, `init.lisp`, `start.lisp` | The 1987 source, ported. The diff against `../../numbo-digitized/*.l` is the 11 OCR fixes and 2 `declare`→`declaim` edits listed below. |
| `package.lisp` | The `NUMBO` package. It shadows the CL symbols the Franz code uses with other meanings (`if defun defvar defmethod make-instance / mod min max member print intern find-package round ratio type`). |
| `franz-compat.lisp` | Franz Lisp shims: keyword `if` (`then`/`else`/`elseif`/`thenret`), lexpr `defun`, `add1`, `concat`, `uconcat`, `quotient`, integer `/`, `sortcar`, `vref`, and others (census in PORTING_NOTES item 3). |
| `flavors-compat.lisp` | Flavors on CLOS: `defflavor`, `(defmethod (flavor :msg) ...)` with instance variables as free variables, `send`, `make-instance`, and `my` (RECONSTRUCTED). |
| `coderack.lisp` | RECONSTRUCTED coderack: `cr-make-coderack`, `cr-hang`, `cr-choose`, `cr-empty?`, `cr-empty-coderack`. Chooses with probability urgency / total urgency (chapter p.143). |
| `graphics-stubs.lisp` | No-op stubs for the 10 window primitives. `%graphics%` stays nil. |
| `globals.lisp` | `defvar`s with no value, for the Franz free globals (compile hygiene only, no values change). |
| `harness.lisp` | `run-config`: seed, iteration cap, verbose switch. Not part of the 1987 code. |
| `solution-checker.lisp` | `check-solution`. Not part of the 1987 code. |
| `load.lisp` | Loads everything in order and reports each file. |
| `PORTING_NOTES.md` | Every change and decision, with scan page references. |
| `RESULTS.md` | The chapter puzzles over 20 seeds, compared with the chapter. |

## What was changed, and why

Summary of [`PORTING_NOTES.md`](PORTING_NOTES.md). See it for the original
and new text of each change.

**Edits to the ported 1987 files (13 in all):**

- **11 OCR fixes**, each checked against the scanned printout
  (`../../numbo.Daniel.Defays.1987.pdf`):
  - `O` read for `0` (2 places);
  - `p1us2-5` and `:code1ets`;
  - `(carx)`, and a stray `·`;
  - `find-activation` and `:node`, each missing an `s`;
  - a misplaced paren in `compare-b-to-t` that pushed `cr-hang`'s arguments out of the call;
  - two lines dropped from `start.lisp`;
  - `reactivate-ctyo` → `reactivate-cyto`. The scan has the correct
    spelling, so this was an OCR error, not a 1987 typo.
- **2 dialect edits**: a top-level Franz `(declare ...)` became `(declaim ...)`
  in `pnet-functions.lisp` and `pnet-graphics.lisp`.

**Everything else is in compatibility files, not the source:**

- **Franz Lisp.** The shims follow Franz semantics where the code depends on
  them. For example, `/` is integer division, `mod` is CL `rem`, `concat`
  interns a symbol, and `(defun f n ...)` is a lexpr.
  - `defvar` on a CL symbol only assigns it if unbound. This avoids SBCL's
    package lock for `(defvar *print-array*)`.
  - `(max)` returns 0. The original applies `max` to an empty list at the start
    of every run. This Franz behaviour is *assumed*.
- **Flavors** are mapped onto CLOS. Instance variables appear in method bodies
  as symbol macros over `slot-value`.
- **Shadowed CL symbols.** Some CL symbols are shadowed because the 1987 code
  `setq`s them as globals or defines functions with their names: `type`,
  `min`, `round`, `ratio`.
- **Reconstructed code**, marked `;; RECONSTRUCTED:` with chapter citations:
  - the coderack;
  - `my`, read as `(send self ...)`. It is used 16 times and defined nowhere.
- **Stubbed graphics.** Graphics are out of scope and are stubbed.

**Not changed:** no parameter was tuned, and no algorithm was altered.

## Known limitations

- **Puzzle 3 is rarely solved.** It is solved by 2 of seeds 1–20 and 3 of
  seeds 1–100. This matches the chapter, where Numbo failed on it, but it means
  you need a known-good seed (18, 13, 78) to see it succeed.
- **The `kill-block` gap (1987 behaviour, kept).** `kill-block` only cascades
  through some parents. A killed block can leave its operation node behind,
  and `propagate-success` can then succeed through it. The run prints
  "Done :" with an invalid decomposition; seed 93 on puzzle 3 is an example.
  Use `check-solution` to tell these runs apart.
- **The `reactivate-cyto` race (1987 behaviour, kept).** If `reactivate-cyto`
  runs at iteration 28, before a brick's `link-to-pnet` codelet has run, the
  run fails with `SEND: NIL does not handle :SET-ACTIVATION`. This happens on
  about 1 seed in 100, for example seed 16 on puzzle 3.
- **Differences from trace3.31.** The port has the trace's kinds of events,
  naming, invariants and opening codelet posts. It posts far more
  `look-for-bl+` codelets. The printout's `codelets.l`, `start.l` and `init.l`
  are dated after the trace, so the trace was probably made with earlier
  versions of those files. Details are in PORTING_NOTES item 11.
- **The coderack is a reconstruction.** Its selection rule follows the chapter,
  but the details are guesses: one bin per urgency level, and urgency-0
  codelets run only when nothing else is left. Success rates depend on it.
  For example, puzzle 11 is easier here than the chapter suggests.
- **Output is upper case** (`CYTO-BRICK1`, not `cyto-brick1`), because SBCL
  prints symbols that way. Franz printed them in lower case.
- **Compiler noise.** Loading prints many "undefined variable" warnings to
  stderr. They come from free variables the 1987 code shares through dynamic
  scope but that are also bound lexically elsewhere (`node`, `res`, `type`,
  ...), so they are deliberately not proclaimed special. Code that relied on
  Franz's dynamic binding of a caller's `let` would behave differently here;
  no case of this has been found.
- **No graphics.** `WINDOW_GFX` must be unset or empty.
- **Runs are reproducible only within the same SBCL version.** A seed
  reproduces a run exactly with the same SBCL. Another version's random
  number generator may give different runs.
