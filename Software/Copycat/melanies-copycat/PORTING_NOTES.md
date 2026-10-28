# Porting notes: Mitchell's Copycat on SBCL

What it took to run the archived Lucid Common Lisp source of Copycat on
SBCL 2.6, headless, and what was found on the way. The rule followed is the
one the Numbo port set (`Software/Numbo/lisp/src/PORTING_NOTES.md`): the
original text is kept, differences between the Lisps are absorbed by a
compatibility layer, and every edit to an original file is marked
`;; PORT-2026:` in the file and listed here.

## 1. Edits to Mitchell's files

**None.** Every file in `src/` is byte-identical to
`../Melanie-Mitchell-[LISP]/` (checked by `tests/src-diff.sh`; the list of
allowed differences, `tests/src-diff-allowed.txt`, is empty). The `.l`
extension and the `(in-package 'user)` lines are kept. `ccat.tar.gz` in the
archive holds the same files (checked with `cmp`, October 2026) and is not
copied.

So there are no `PORT-2026` entries. Everything below is in `compat/`,
`load.lisp` and `harness.lisp`.

## 2. How the sources were found to differ from ANSI CL

Method: load the unmodified files into SBCL in the order of Mitchell's
`ccat-sys.l`, read each error or warning, and decide whether it is a
dialect difference (shim it), a missing graphics function (stub it) or a
bug of Mitchell's (note it). Then run the six classic problems over 100
seeds each and watch for runtime errors. Scott Burson's 2025 revival
(https://github.com/slburson/copycat) was read as a map of what to expect;
its repository has no license, so none of its code is used, and its
solutions differ from these in several places (section 7).

### 2.1 Lucid Common Lisp differences (`compat/lucid-compat.lisp`)

| What the source does | Where | What SBCL says | Treatment |
|---|---|---|---|
| `(in-package 'user)` | every file | `'USER is not a string designator` (CLtL1's `in-package` was a function taking a symbol) | `in-package` is shadowed; the form selects the `COPYCAT` package whatever its argument |
| `(production-mode)` | ccat-sys.l:52 | undefined function (a Lucid compiler switch) | a no-op |
| `(loop ... do collect i)`, `(loop ... do when ...)` | initialization.l:191, 228; answer.l:66; correspondences.l:577 | `A compound form was expected, but COLLECT found` | `loop` is shadowed: a `do`/`doing` directly followed by a loop keyword (or last) is dropped, the rest goes to `cl:loop` unchanged |
| `(loop for i from 1 to 1000 do ())` | util.l:371, 376, 381 (the pause functions) | `... but NIL found` | the same shim drops `do ()` |
| `(funcall '(lambda (object) ...) object)` | slipnet-def.l:1171 and answer.l:117 call the description testers, which slipnet-def.l stores as quoted lambda lists (12 of them) | `(LAMBDA (X) X) is not a (OR FUNCTION SYMBOL)` (CLtL1 let a lambda expression stand for a function) | `funcall` is shadowed: a quoted lambda is compiled once (cached on the list, a literal in the source) and called; symbols and functions pass through |
| `(sort list #'> :key '(lambda (x) ...))` | copystat.l:185 | the same | `sort` is shadowed the same way |
| `memq` | 25 calls | undefined | `(member x l :test #'eq)` |
| `string-append` | 13 calls (copystat.l, util.l, workspace-strings.l) | undefined | `concatenate 'string` over `string` of each argument. Symbols are accepted because `copystat` hands the same arguments to `string-append` and to `init-ccat`, which takes their `symbol-name`: in Lucid one of the two accepted both, and this is the reading that lets `(copystat 'abc 'abd 'ijk 100)` work |
| `append-symbols` | 14 calls (util.l, breakers.l, slipnet-def.l, slipnet-links.l) | undefined | intern the concatenated print names in `COPYCAT`. Lucid's reader upcased symbols, as SBCL's does, so `(append-symbols 'plato- "A")` is `plato-a`, the slipnode's variable |
| `(run-program "cp" :arguments (list ...))` | copystat.l:137 | SBCL's `run-program` takes the arguments positionally | Lucid's signature on top of `sb-ext:run-program` with `:search t` |
| `flavors::flavor-type-of` | util.l:457 (`flavor-type`, used with `eq` in workspace-structure-formulas.l:34) | no package `FLAVORS` | a `FLAVORS` package with that one function: the class name of the object |
| `(with-open-file (s f :direction :output :if-does-not-exist :create) ...)` on a file that exists | copystat.l:96, rewriting the `.random-state` file after each run | `error opening ...: File exists` (the default `:if-exists` for a `:newest` version is `:new-version`, which SBCL treats as an error) | `with-open-file` is shadowed: an output file with no `:if-exists` gets `:supersede`, which is what Lucid did on SunOS, where files have no versions. Only copystat.l and util.l open files |
| `(proclaim '(inline if*))` before `(defmacro if* ...)` | lucid-util.l:7 | a style warning (`IF* is being redefined as a macro when it was previously assumed to be a function`), harmless | nothing |
| `(declare (dynamic-extent args))` in the `if*` macro | lucid-util.l:19 | accepted | nothing |

Mitchell's own `lucid-util.l` (`sqr`, `if*`) and `file-system.l`
(`defsys`, a Flavors-based make) load as they are and are used:
`ccat-sys.l` calls `defsys` and proclaims every global special, so those
three files are loaded first, as they were in her Lucid image.

### 2.2 Flavors (`compat/flavors-compat.lisp`)

A census of all 22 `defflavor` and 223 `defmethod` forms:

- `defflavor` options used: `:gettable-instance-variables`,
  `:settable-instance-variables`, `:initable-instance-variables`, always in
  the bare form (all ivars). Ivars with defaults: `(direction-category nil)`
  style. Component flavors: yes, including one with two (`group` is a
  `workspace-object` and a `workspace-structure`). Lucid let the component
  list be omitted when empty (`file-system.l`: the first option follows the
  ivars directly); the layer accepts that.
- `defmethod`: `(defmethod (flavor :message) lambda-list body)`, primary
  methods only; no `:before`/`:after`, no whoppers, no `:init` methods, no
  `send self :init`. Lambda lists use `&optional`, `&rest`, `&aux`. Ivars
  are read free and written with `setq`; `self` is the instance.
- Three methods bind a parameter or `&aux` variable with the name of an
  ivar: `(correspondence :other-group) (group)`,
  `(description :apply-slippages) (object ...)`,
  `(workspace-structure :calculate-total-strength) (&aux internal-strength external-strength ...)`.
  In Flavors the lexical binding wins, and the three bodies only make sense
  that way (the first two use the argument they were given; the third reads
  the slots through `send` and would compute the same either way). The
  layer leaves those names out of the ivar symbol-macros for that method.
- Every method is defined after its flavor, so no reordering is needed
  (Burson moved five `defflavor`s to an early file; that is a property of
  his implementation, not of the code).
- `make-instance` with `:ivar` keywords is `cl:make-instance` directly: each
  initable ivar is a slot with an `:initarg`. Unset ivars are `nil`, as in
  Flavors (the code tests `group`, `graphics-obj` and others with `if`
  before they are set).
- `(typep x 'bond)` works because flavors are CLOS classes.
- `send` looks the message up along the class precedence list and caches
  the result per class and message; the cache is cleared whenever a method
  is defined.

The design is the Numbo port's (`Software/Numbo/lisp/src/flavors-compat.lisp`),
with the parameter-shadowing rule, the optional component list and the send
cache added.

### 2.3 Graphics (`compat/graphics-stubs.lisp`, `compat/coderack-graphics-methods.lisp`)

The display code is in `src/graphics-files/` (5,500 lines, not loaded). It
draws through 60-odd Lucid foreign functions (`load-lucid-graphics-sun4.l`,
`lucid-graphics.c`: SunView pixwin lines, dashed and jagged lines, circle
arcs, parabolas, text in Sun fonts, xor drawing for animation) plus Lucid's
`make-viewport` and menus (`ccat-menu.l`).

The engine already runs without it: `init-ccat`'s `:no-graphics` switches
every `%...-graphics%` flag off (`init-constants` turns them on by default,
`%demo-graphics%` included) and all drawing is behind those flags. What
remains for the compiler's sake:

- 25 functions the engine calls only when a flag is on, stubbed as no-ops:
  `open-window`, `clear-window`, the four menu functions, `display-ccat`,
  `redraw-graphics`, the `init-`/`display-`/`update-` functions for the
  workspace, coderack, minimal coderack, slipnet, temperature and
  descriptions, `init-answer-graphics`, and the three "grope" animations
  (`draw-bond-grope`, `draw-group-grope`, `draw-correspondence-grope`).
  `tests/load-tests.lisp` checks that no function the compiler saw
  referenced is left undefined after the load.
- Four definitions from `coderack-graphics.l` that the engine calls on every
  codelet, flag or no flag: `(coderack :add-codelet-to-graphics)`,
  `(coderack :delete-codelet-from-graphics)` (they keep the per-type,
  per-bin counts in `codelet-urgency-array` that the bar graph reads) and
  their helpers `get-codelet-number`, `get-codelet-name`. They are copied
  unchanged (lines 81-110) into `compat/coderack-graphics-methods.lisp`,
  loaded right after `coderack.l`.
- The window variables `load-lucid-graphics-sun4.l` proclaims
  (`%graphics-viewport-width%` etc.), read by the `open-window` call that
  `:no-graphics` skips.

Methods such as `(group :draw)` or `(bond :erase-spline)` are defined only
in the graphics files; they are sent only under the flags, so with graphics
off nothing is missing (an unknown message would be a `send` error, and
none occurs in 600 runs).

## 3. Loading

`load.lisp` loads `compat/` and then Mitchell's files in the order of the
`defsys` list in `ccat-sys.l`, minus the 14 graphics files, with
`lucid-util`, `file-system` and `ccat-sys` first. Each file is read with
`*package*` bound to `COPYCAT`, which is what makes `(in-package 'user)`
and the shadowed symbols take effect. Files are loaded as source (SBCL
compiles each top-level form); loading takes about 1.5 s. Compiler output
is muffled unless `cl-user::*copycat-verbose-load*` is `t`: the 1989 code
produces a few hundred style warnings (forward references, unused `&aux`
variables, variables used before `ccat-sys.l`'s proclamation is seen) and
no errors.

`copycat.asd` loads the same files through ASDF, with `compile-file`; it
needs a `cl-source-file.l` component class for the extension and an
`:around-compile` hook that binds `*package*` to `COPYCAT`. Tested with
`(asdf:load-asd ...)` and `(asdf:load-system "copycat")` on ASDF 3.3.1.

## 4. Seeding and reproducibility

Mitchell's `init-ccat` takes `:random-state`. Without it, it calls
`(make-random-state t)`, a fresh state from the clock; with it, a copy of
the given state. Either way it then copies `*random-state*` into
`*random-state-this-run*`, which `copystat` appends to a `.random-state`
file after every run and `util.l`'s `get-random-state` reads back, so any
recorded run can be replayed (she did this to study individual runs; the
screen dumps in FCCA chapter 5 are such replays). `tests/copystat-tests.lisp`
checks that a replay reproduces its run's line in the `.data` file.

All randomness is `cl:random` on `*random-state*`: `util.l`'s
`select-list-position`, `select-assoc`, `random-list-item`, `flip-coin`,
`blur`, and `coderack.l`'s `:choose` (line 305). There is no other source
(no hash tables, no clock, no `sort` whose order matters: the two `sort`
calls are in `(group :print)` and `copystat-summary`).

`copycat:run`'s `:seed` is an integer given to `sb-ext:seed-random-state`;
the resulting state goes to `init-ccat`. The same seed gives the same run
(same answer, codelet count, temperature); `tests/run-tests.lisp` pins
`abc abd ijk` seed 1 to `ijl` in 389 codelets at temperature 13, so a
change in the layer that alters the course of a run is noticed. SBCL's
`random` is a Mersenne Twister and has been stable across releases; a
different Lisp would give different runs for the same seed, and so would
Lucid, whose generator is unknown. Only the distributions can be compared
(`RESULTS.md`).

## 5. Mitchell's own quirks, noted and left alone

None of these stops the program; none is changed.

1. `answer.l:83`: `(send *answer-string* :set-length (make-vector n))` sets
   the answer string's `length` ivar to a vector rather than a number (the
   two lines above it, for `letter-vector` and `object-position-vector`,
   are the pattern it was copied from). Nothing reads the answer string's
   `:length` afterwards; `:pstring` walks `letter-list`.
2. `correspondences.l:334`: `(format t "Chose obj2: " (send obj2 :print))`
   prints the object before the label, since the argument is evaluated
   first. Verbose mode only.
3. `ccat-sys.l:217`: `long-correspondence-dash-length%` is missing its
   leading `%`; the intended `%long-correspondence-dash-length%` is
   proclaimed at line 196 anyway. Graphics only.
4. `coderack.l:331` and `:371`: the two "coderack is empty" messages of
   `:remove-codelets` differ by "any more"; cosmetic.
5. `run.l:38`: "My answer is ..." is printed only when `%verbose%` is on, so
   a headless `init-ccat` prints nothing; the answer is in
   `(send *answer-string* :pstring)`. `copycat:run` prints it.
6. `init-constants` resets `%verbose%` and `%slightly-verbose%` at the start
   of every run, so a trace cannot be switched on from outside without an
   edit; `copycat:run :trace t` wraps `init-constants` with
   `sb-int:encapsulate` instead.
7. `copystat` needs symbols, `(copystat 'abc 'abd 'ijk 100 "dir/")`, for
   the reason given under `string-append` above. Its `:directory` defaults
   to `"~/"`, which SBCL does not expand; give a full path.
8. `formulas.l:108`, `test-get-temperature-adjusted-probability`, appends to
   a file `testfile` in the current directory when called. It is never
   called by the program.
9. `file-system.l`: the reload logic adds 15 seconds to load times because
   "the write-time seems to be about 10 seconds ahead of what it should be.
   I don't know why this is." Not used here; `load.lisp` loads everything.
10. The `%graphics-rate%` pauses in `util.l` are busy loops calibrated for a
    1989 Sun; they only run with graphics on.
11. FCCA chapter 5, footnote 3, notes a problem with how temperature is
    calculated in this version (`ijd` ends at about the same temperature as
    `ijl`). It is a property of the program, reproduced here (see
    `RESULTS.md`).

## 6. Performance

On this machine (SBCL 2.6.0): load 1.5 s; `abc abd ijk` about 0.05 s per
run; `abc abd xyz`, with its snags, 0.5 s on average and 2 s at the worst
(13,000 codelets); the 600 runs of `RESULTS.md` in a few minutes. The
`send` cache is the only optimization; Mitchell's `(production-mode)` is a
no-op.

## 7. Relation to Burson's revival

Burson's version (February 2025) is the same code with a different layer:
`.l` renamed `.lisp`, the sources rearranged under `src/` and
`src/graphics/`, trailing whitespace removed, a `copycat` package that
shadows `defmethod` only, Flavors on CLOS via closer-mop with funcallable
instances, five `defflavor`s moved to an early file, and about twenty
edits inside the sources marked `[SLB]` (the `loop ... do` forms, the
quoted lambdas rewritten as `(lambda ...)`, `symbol-name` replaced by
`string-upcase` so strings can be passed, `eval` replaced by `symbol-value`,
`copystat` rewritten to avoid its temp file, new `:verbose`,
`:slightly-verbose`, `:single-step` and `:init-only` arguments to
`init-ccat`, the pause loops replaced by `sleep`, and a few descriptions
tests changed from `(send (send d :description-type) :active?)` to
`(send d :relevant?)`). It has no seed argument, no headless entry point
and no tests, and reports no results.

Here the sources are untouched and every difference lives in `compat/`; the
layer was written from the Lucid and Flavors behaviour the code depends on,
with the Numbo port's layer as the model. His repository was useful as a
checklist of what would break; nothing in it is copied.
