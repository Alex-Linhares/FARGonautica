# Numbo on SBCL

Daniel Defays' 1987 Numbo, running on a modern Common Lisp ([SBCL](https://www.sbcl.org/)).

The 1987 source in [`../numbo-digitized/`](../numbo-digitized/) was written for
Franz Lisp with Flavors, and part of it (the coderack) is missing from the
printout. This directory makes it run with as few changes as possible:

- the six original files are ported almost exactly as printed. The only edits are 11 OCR fixes, checked against [the scan](../numbo.Daniel.Defays.1987.pdf), and 2 `declare` → `declaim` changes;
- compatibility layers supply Franz Lisp built-ins and Flavors (on top of CLOS);
- the coderack is reconstructed from Defays' chapter;
- graphics are stubbed out (text output only).

Every change is logged with its reason in [`src/PORTING_NOTES.md`](src/PORTING_NOTES.md).

## Quick start

Install SBCL (tested with 2.6.0):

```sh
sudo apt install sbcl        # Debian/Ubuntu
brew install sbcl            # macOS
```

From this directory, solve "reach 114 from the bricks 11 20 7 1 6", which takes about 45 iterations:

```sh
sbcl --non-interactive --load src/load.lisp \
     --eval "(print (numbo::run-config '(114 11 20 7 1 6) :seed 1 :max-iterations 20000))" \
     2>/dev/null
```

Numbo prints its working (nodes created and killed in the cytoplasm), then the
solution, (20 x 6) - (7 - 1), the same one as the chapter's sample run.
The last line is a summary such as `(:OUTCOME :SOLVED :ITERATIONS ... :SEED 1 ...)`.
`2>/dev/null` hides compiler warnings about the 1987 code's undeclared globals.

The puzzle from the 1987 trace, `trace3.31` (reach 31 from 3 5 24 3 14), is
hard: the chapter says Numbo didn't solve it. Seed 18 is one that does:

```sh
sbcl --non-interactive --load src/load.lisp \
     --eval "(print (numbo::run-config '(31 3 5 24 3 14) :seed 18 :max-iterations nil))" \
     2>/dev/null
```

To run it the 1987 way, from a REPL (`sbcl`, then):

```lisp
(load "src/load.lisp")
(in-package :numbo)
(init-chiffre)
(config 114 11 20 7 1 6)
```

## Tests

```sh
./tests/run-tests.sh
```

There are 14 test groups: Franz and Flavors layers, coderack, compilation,
a boot smoke test, a run to completion checked by a solution checker,
comparison with the 1987 trace and the chapter's puzzles, and the README
commands.

## More

- [`src/README.md`](src/README.md): full usage. Covers `run-config` options, `check-solution`, the file layout and known limitations.
- [`src/RESULTS.md`](src/RESULTS.md): the chapter's 11 puzzles × 20 seeds each, compared with what the chapter reports.
- [`src/PORTING_NOTES.md`](src/PORTING_NOTES.md): every change, reconstruction and finding, with scan page references.

Reference: Daniel Defays, "Numbo: A Study in Cognition and Recognition",
chapter 3 of Douglas Hofstadter and the Fluid Analogies Research Group,
*Fluid Concepts and Creative Analogies* (Basic Books, 1995).
