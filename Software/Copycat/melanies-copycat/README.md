# Melanie Mitchell's Copycat, running on SBCL

This folder is Mitchell's original Copycat, the Lucid Common Lisp program
of her 1990 thesis and of *Fluid Concepts and Creative Analogies* chapter 5,
running again on a current open-source Lisp. The engine is unchanged; a
small compatibility layer makes SBCL read the 1989 text the way Lucid did.
It runs headless (no graphics) and answers the letter-string problems
(`abc → abd; ijk → ?`), printing the rule it found, the answer, the final
temperature and the number of codelets it ran.

```
$ ./run-copycat.sh abc abd xyz 3
Problem: abc -> abd :: xyz -> ?  (seed 3)
Rule: Replace letter with string-position-category "rmost" by letter with letter-category "D"
Translated rule: Replace letter with string-position-category "rmost" by letter with letter-category "D"
Answer: xyd
Temperature: 18
Codelets run: 2875
Snags: 7
```

## Where the code comes from

The sources are Mitchell's distribution as archived in
[`../Melanie-Mitchell-[LISP]/`](../Melanie-Mitchell-%5BLISP%5D/) (from
[her page](https://melaniemitchell.me/ExplorationsContent/how-to-get-copycat.html);
the `ccat.tar.gz` there holds the same files). That archive is read-only;
`src/` is a copy of it, file for file and byte for byte, with the `.l`
extension and the `(in-package 'user)` lines kept, as `tests/src-diff.sh`
checks. There are 28 engine files (11,500 lines) and 19 graphics files
(5,500 lines, including the C side of Lucid's window interface) that are
copied but not loaded.

The code is Lucid Common Lisp on Sun workstations, 1989 to 1993, with the
Flavors object system (`defflavor`, `defmethod (flavor :message)`, `send`)
and Lucid's SunView graphics. Lucid went out of business in 1994 and no
open-source Lisp has Flavors, which is why the program has been unrunnable
for thirty years except through re-implementations in other languages
(the top-level `README.md` lists them).

## How to run it

You need SBCL (`apt install sbcl`; 2.6 was used here). Everything is run
from this folder.

```sh
./run-copycat.sh abc abd ijk            # one run, seeded from the clock
./run-copycat.sh abc abd ijk 7          # seed 7: the same run every time
./run-copycat.sh --sweep 100 abc abd xyz           # 100 seeds, answer table
./run-copycat.sh --sweep 100 abc abd xyz 1 xyz.log # ... one line per run in xyz.log
bash tests/run-tests.sh                 # the tests, about a minute
```

From Lisp:

```sh
sbcl --non-interactive --load load.lisp --eval '(copycat:run "abc" "abd" "ijk" :seed 1)'
```

```lisp
(load "load.lisp")
(copycat:run "abc" "abd" "mrrjjj" :seed 20)        ; prints the summary, returns a plist
(copycat:run "abc" "abd" "xyz" :seed 2 :trace t)   ; prints every codelet as it runs
(copycat:sweep "abc" "abd" "kji" :seeds 50)        ; an answer table
(in-package :copycat)
(init-ccat 'abc 'abd 'ijk :no-graphics t)          ; Mitchell's own entry point
(send *answer-string* :pstring)                    ; => "ijl"
(copystat 'abc 'abd 'ijk 10 "/tmp/stats/")         ; her batch tool: .data, .verbose-data,
(copystat-summary "ABC-ABD-IJK" "/tmp/stats/")     ; .random-state and .summary files
```

`copycat.asd` loads the same files through ASDF (`(asdf:load-system "copycat")`)
for Quicklisp users; `load.lisp` needs nothing but SBCL.

A seed is an integer; it is turned into an SBCL random state and handed to
Mitchell's `init-ccat`, whose `:random-state` argument was her own way of
replaying a run (her `copystat` saves the state of every run to a file and
`get-random-state` reads one back; that still works, and a test checks it).
Every random draw in the program goes through that one state, so a seed
fixes the whole run: the codelets chosen, the structures built, the answer.
See `PORTING_NOTES.md`, section 4.

A run takes a fraction of a second; `xyz`, with its snags, up to two.

## What was changed, and why

Nothing in Mitchell's files. All the work is in `compat/` and is explained
line by line in [`PORTING_NOTES.md`](PORTING_NOTES.md):

- `compat/lucid-compat.lisp`: the Lucid behaviour the code relies on and
  ANSI CL does not have. `(in-package 'user)`, a `loop` that accepted an
  empty `do` before the next clause, quoted `(lambda ...)` lists used as
  functions (the slipnet's description testers), `memq`, `string-append`,
  `append-symbols`, Lucid's `run-program`, `production-mode`, the
  `flavors::flavor-type-of` function, and `with-open-file`'s default on an
  existing output file.
- `compat/flavors-compat.lisp`: the subset of Flavors the code uses, on
  CLOS. Flavors are classes, instance variables are slots, methods are
  closures in a per-flavor table that `send` looks up along the class
  precedence list; inside a method the instance variables are
  symbol-macros over the slots, except where a parameter has the same name.
  It follows the Numbo port's layer (`Software/Numbo/lisp/src/`).
- `compat/graphics-stubs.lisp` and `compat/coderack-graphics-methods.lisp`:
  no-op stubs for the 25 display functions the engine calls behind its
  `%...-graphics%` flags, and four small definitions from
  `coderack-graphics.l` that the coderack calls on every codelet, copied
  unchanged.
- `load.lisp`: loads the above and then Mitchell's files in the order of her
  `ccat-sys.l`, reading them into the `COPYCAT` package. `harness.lisp`:
  `copycat:run`, `copycat:sweep` and the command-line entry.

Mitchell's own slips that were noticed on the way (an answer-string length
set to a vector, a misspelt proclamation, a verbose message printed in the
wrong order) are listed in the notes, section 5, and left as they are.

## What is not ported: the graphics

Mitchell's screen, the one in the FCCA screen dumps, is drawn by
`src/graphics-files/`: the workspace with its letters, bonds, groups and
bridges, the slipnet as a grid of boxes, the coderack bar graph, the
temperature thermometer, and menus to set parameters and step the run.
All of it goes through about sixty Lucid foreign functions into
`lucid-graphics.c`, which draws on a SunView pixwin (lines, dashed and
jagged lines, circle arcs, parabolas, text in Sun's fixed-width fonts), and
through Lucid's `make-viewport` and menu facility. None of that exists on
a modern system.

The engine does not need it: `init-ccat :no-graphics t` turns every
`%...-graphics%` flag off, and every drawing call sits behind one of those
flags. A GUI for this port would be built the way the Numbo and Metacat
ports build theirs, as an observer: the engine's state is all in globals
(`*workspace*`, `*coderack*`, `*slipnet*`, `*temperature*`,
`*codelet-count*`) and `run.l`'s `step-ccat` is the one place a view needs
to be refreshed from. Porting the 5,500 lines of Lucid drawing code to
some window toolkit would be the other route; the drawing primitives are
listed in `src/graphics-files/lucid-graphics.w` and the per-structure
`:draw`/`:erase`/`:flash` methods are in the `*-graphics.l` files.

## Results

The six classic problems, 100 seeds each, all ending with an answer
(`RESULTS.md` has the table against the book's 1,000-run bar graphs):

| Problem | Answers (100 seeds) | FCCA chapter 5, 1,000 runs |
|---|---|---|
| ijk | ijl 97, ijd 2, ijk 1 | ijl, then ijd |
| xyz | xyd 78, wyz 12, yyz 7, xyz 2, dyz 1 | xyd 771, wyz a distant second, yyz, fringe |
| iijjkk | iijjll 80, iijjkl 18, iijjkd 1, iijjkk 1 | (not in chapter 5) |
| kji | kjh 61, kjj 22, lji 16, kjd 1 | kjh 547, kjj and lji nearly tied for second |
| mrrjjj | mrrkkk 76, mrrjjk 18, mrrddd 2, mrrjkk 2, mrrjjjj 2 | mrrkkk, mrrjjk, then mrrjkk and mrrjjjj (39) |
| xyd | xye 98, xyd 2 | (not a published problem) |

## Tests

`bash tests/run-tests.sh` checks, each in a fresh SBCL: that the archive is
untouched in git and that `src/` equals it; the compatibility layer on its
own (32 checks); that the whole system loads with no error and no function
left undefined; that the six problems finish with an answer for three
seeds each, that a seed reproduces a run, and one pinned run (`ijk` seed 1:
`ijl`, 389 codelets, temperature 13); the shell wrapper; and `copystat`
with a replay from its saved random state.

## Relation to Scott Burson's revival

Scott Burson made the same code run on SBCL in February 2025
(https://github.com/slburson/copycat): files renamed and rearranged, a
Flavors layer on closer-mop in `src/defs.lisp`, and some twenty edits
inside the sources, with no license on the repository. His work was read
as a reference for what would break and is cited for that; the layer here
is independent, written from the Lucid and Flavors semantics the code
needs and from the Numbo port's layer, and it leaves Mitchell's files as
they are. `PORTING_NOTES.md`, section 7, lists the differences.

## Files

```
README.md  PORTING_NOTES.md  RESULTS.md
load.lisp  copycat.asd  harness.lisp  run-copycat.sh
compat/    package.lisp lucid-compat.lisp flavors-compat.lisp
           graphics-stubs.lisp coderack-graphics-methods.lisp
src/       Mitchell's 28 engine files (.l), lucid-util.w, graphics-files/
tests/     run-tests.sh and the test files
results/   the 100-seed logs and sweep-summary.txt behind RESULTS.md
```
