# Numbo on SBCL, and in Python

Daniel Defays' 1987 Numbo, running on a modern Common Lisp ([SBCL](https://www.sbcl.org/)),
plus a Python translation (in [`python/`](python/)) that behaves exactly like it, run for run.

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

## Python version

[`python/`](python/) is a test-driven translation of the SBCL port. It uses only the
standard library and needs nothing installed. From this directory:

```sh
cd python
python3 -m numbo 114 11 20 7 1 6 --seed 1
```

This prints Numbo's working, then:

```
outcome: solved, 45 iterations (seed 1)
check: valid: 114 = (6 x 20) - (7 - 1)
```

The Python version always runs in the SBCL port's *oracle mode*: a shared
splitmix64 random generator, double floats, and a copying `sortcar` (see
"Oracle hooks" in [`src/PORTING_NOTES.md`](src/PORTING_NOTES.md)). In that mode, the
two versions produce **identical event streams**: every codelet, node, random
draw and printed line. This is checked on all 11 chapter puzzles × 20 seeds by the
test suite, which runs SBCL and Python side by side. The 1987 flaws are reproduced,
not fixed: the occasional invalid "Done :" and the early reactivate-cyto race.

- [`python/README.md`](python/README.md): usage, the Python API, and how the translation was verified.
- [`python/RESULTS.md`](python/RESULTS.md): the chapter's puzzles in oracle mode.
- [`docs/python_translation_audit.md`](docs/python_translation_audit.md): the plan and risk analysis the translation followed.

## Tests

```sh
./tests/run-tests.sh
```

There are 17 test groups. They cover:

- the Franz and Flavors layers, the coderack and compilation;
- a boot smoke test and a run to completion checked by a solution checker;
- comparison with the 1987 trace and the chapter's puzzles;
- the README commands and the oracle hooks;
- the Python tests (pytest, about 4,500 tests), including the side-by-side SBCL/Python comparison.

Python 3.12+ and pytest are needed for the last group.

## More

- [`src/README.md`](src/README.md): full usage. Covers `run-config` options, `check-solution`, the file layout and known limitations.
- [`src/RESULTS.md`](src/RESULTS.md): the chapter's 11 puzzles × 20 seeds each, compared with what the chapter reports.
- [`src/PORTING_NOTES.md`](src/PORTING_NOTES.md): every change, reconstruction and finding, with scan page references.

Reference: Daniel Defays, "Numbo: A Study in Cognition and Recognition",
chapter 3 of Douglas Hofstadter and the Fluid Analogies Research Group,
*Fluid Concepts and Creative Analogies* (Basic Books, 1995).
