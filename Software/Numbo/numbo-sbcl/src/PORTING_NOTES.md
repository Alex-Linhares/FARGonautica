# Porting Notes

Every change to a ported source file (`src/*.lisp` copied from
`../../numbo-digitized/*.l`) is logged here.

Entry format: file, line/function, original text, new text, reason
(OCR fix / Franz dialect / Flavors / missing code / typo).

Scan page references are to `../../numbo.Daniel.Defays.1987.pdf`.
PDF page = file page + offset: pnet-def +2, pnet-functions +22,
pnet-graphics +27, cyto-def +31, codelets +37, init +62, start +65.

## Changes

### Item 2: reader pass (OCR fixes checked against the scan)

All 7 files READ without error even before these fixes. These were found by
a symbol census (non-ASCII and one-off symbols) and a proofread of the
scan against the digitized text. Every fix restores what the scan shows.
None of them adds or removes a line except in start.lisp (see below), so line
numbers in the other files still match the originals.

Proofreading coverage: every code page of pnet-def, pnet-functions,
cyto-def, codelets, init and start was compared line by line with the
scan, with closing-paren runs counted on 250-300 dpi renders.
pnet-graphics was compared only at page resolution. Graphics are stubbed
and `%graphics%` stays nil, so that file's code never runs.

| File | Line / function | Original | New | Reason |
|---|---|---|---|---|
| cyto-def.lisp | 248, `replace-function` | `(defun replace-function (l l1 l2) ·` | `(defun replace-function (l l1 l2)` | OCR fix: stray `·` (non-ASCII) would READ as a symbol in the body. Not on the scan (PDF p.36). |
| cyto-def.lisp | 302, `(cytoplasm :suppress-node)` | `((eq (carx) l) nil)` | `((eq (car x) l) nil)` | OCR fix: dropped space; scan (PDF p.37) has `(car x)`. |
| pnet-def.lisp | 69, `init-pnet` node-2 | `(p1us2-5 result+)` | `(plus2-5 result+)` | OCR fix: digit 1 for letter l; scan (PDF p.4) has `plus2-5`. |
| pnet-functions.lisp | 97, `(pnode :link-length)` | `(&optional (k O.1))` | `(&optional (k 0.1))` | OCR fix: letter O for zero (read as symbol `O.1`); scan (PDF p.24) has `0.1`. |
| codelets.lisp | 82, `compare-b-to-t` | `(= O (sim block target))` | `(= 0 (sim block target))` | OCR fix: letter O for zero; scan (PDF p.39) has `0`. |
| codelets.lisp | 102, `compare-b-to-t` | `%fourth-urgency%))))` | `%fourth-urgency%)))))` | OCR fix (paren): scan (PDF p.39) has 5 closers. The digitizer read 4 and made up the difference on line 106 (next row), so the file stayed balanced but the structure was wrong. As printed, the `digits-in-common` `cond` is a second form of the `let` body, not part of the `(t ...)` clause of the first `cond`. |
| codelets.lisp | 106, `compare-b-to-t` | `(list 'decompi cyto-block cyto-current-target)` | `(list 'decompi cyto-block cyto-current-target` | OCR fix (paren): scan has no `)` here. With it, the `(list 'quote (digits-in-common ...))` argument and `%second-urgency%` fell outside the `decompi` form and the `cr-hang` call. |
| codelets.lisp | 1063, `look-for-new-block` | `(find-activation blocks)` | `(find-activations blocks)` | OCR fix: dropped `s`; scan (PDF p.56) has `find-activations` (defined at line 513). |
| codelets.lisp | 1138, `propagate-success` | `(send *cytoplasm* :node)` | `(send *cytoplasm* :nodes)` | OCR fix: dropped `s`; scan (PDF p.57) has `:nodes`, as do all other call sites. |
| pnet-def.lisp | 854, `init-pnet` times2-20 | `:code1ets` | `:codelets` | OCR fix: digit 1 for letter l; would be an unknown init keyword. Scan (PDF p.17) has `:codelets`. |
| start.lisp | after 54, `config` | `(read-brick 5)` followed directly by the `;I start at x = 11` comment | `(read-brick 5)` + two `(eval (cr-choose *coderack*))` lines | OCR fix: two lines dropped in digitization; scan (PDF p.66) has them, like every other `read-brick` call. **Adds 2 lines**, so start.lisp lines after 54 are +2 from the original. |
| start.lisp | 69 (now 71), `config` | `(refresh-everything)(reactivate-ctyo))` | `(refresh-everything)(reactivate-cyto))` | OCR fix: the scan (PDF p.67) clearly reads `reactivate-cyto`. The "known typo" listed in TASK.md / item 6 is a digitization error, not in Defays' printout. |

### Digitizer reconstructions of lines cut off on the scan (kept as is)

The digitizers already filled these in. Each reconstruction is the only one
that balances, and we leave it unchanged:
- `codelets.lisp:159` (`const-bl+`): the scan ends at `'-v *name-coun`; `ter*))` is reconstructed.
- `codelets.lisp:1208` (`read-target`): the scan ends at `"free" 9`; the second `9` of `99` is reconstructed (99 matches the target level used at line 1343).
- `codelets.lisp:844`, `892`: the ends of commented-out lines (`cyto-blo` + `ck2`). Comments only.

### Noted, not changed (Franz dialect; handled in later items)

- `codelets.lisp:1299` `(*mod num div)`: `*mod` is a Franz Lisp built-in, not OCR damage. Shimmed in item 3.
- `pnet-def.lisp:260` `:name 'twelfe`: the original spelling, used only as a name. Left alone.

### Item 3: Franz Lisp compatibility layer

#### Source edits

| File | Line / function | Original | New | Reason |
|---|---|---|---|---|
| pnet-functions.lisp | 3, top level | `(declare (macros t))` | `(declaim (macros t))` | Franz dialect: a top-level `declare` is a Franz compiler directive. In CL it is an error ("no function DECLARE"). `CL:DECLARE` cannot be shadowed without breaking every `(declare (special ...))` inside function bodies. `franz-compat.lisp` declares `macros` as a known declaration, so the `declaim` has no effect. |
| pnet-graphics.lisp | 8, top level | `(declare (special` | `(declaim (special` | Franz dialect: the same issue. A top-level `(declare (special ...))` in Franz is a global special proclamation, which is exactly what `declaim` does in CL. |

No other source line was changed for item 3. Everything else is handled by
shims in `src/franz-compat.lisp` and shadowed symbols in `src/package.lisp`.

#### Franz built-ins used by the source (census)

How the list was made: READ every form of the 7 ported files and collect
every symbol that appears as the car of a list. Then drop CL symbols,
keywords, and functions the source defines with `defun`. The rest was
sorted by hand into Franz built-ins (below), Flavors, the coderack,
graphics, and non-calls (variables in `let`/`cond`/`defflavor` lists,
quoted data such as node names `node-31` and `plus2-5`). Functions passed
by name (`(apply 'add ...)`, `(apply 'max ...)`, `(mapcar 'diff300 ...)`)
were grepped separately. The covered list is checked by
`tests/franz-compat-tests.lisp`, which fails if any entry is not defined in
NUMBO.

| Built-in | Uses | Where (examples) | Shim semantics (Franz Manual, Opus 38) |
|---|---|---|---|
| `if` (keyword form) | 47 | everywhere | `then`, `thenret`, `elseif`, `else`; no keywords means CL `if`. Expands to `cond`. |
| `defun` with an atom arglist (lexpr) | 1 | codelets:22 `(defun check-temperature function () ...)` | Atom = lexpr: takes any number of args and binds the atom to their count. `()` is the first body form. Checked against the scan (PDF p.38): `function` is in the original. Optional `expr` type is stripped; `fexpr`/`macro` signal an error (unused). |
| `plus`, `add` | 9, 4 (+3 `apply 'add`) | activation arithmetic | Generic `+` |
| `times` | 20 | | Generic `*` |
| `difference`, `diff` | 2, 3 | pnet-functions:243, codelets:15 | Subtract the rest from the first; `(difference x)` = x |
| `quotient` | 11 | codelets:1183, 1308 | Integer args: truncating division. Any flonum: float division. |
| `/` | 3 (+6 graphics) | codelets:339, 1299-1300, 1343 | Same as `quotient`. **This matters**: the source's `round` needs `(/ 31 10)` = 3, where CL gives 31/10. |
| `*quo` | 2 | codelets:450, 452 | Truncated integer quotient |
| `mod` | 10 | codelets, start | Franz `mod` = remainder, with the dividend's sign (CL `rem`) |
| `*mod` | 1 | codelets:1299 `round` | Balanced residue in [\|y\|/2 - \|y\| + 1, \|y\|/2] |
| `minus` | 2 | pnet-functions:125 (quoted), graphics | Negation |
| `add1` | 7 | | `(+ x 1)` (`sub1` added too, unused) |
| `fix` | 8 | codelets:1161, graphics | Round down (`floor`), as the manual says |
| `nequal` | 22 | codelets | `(not (equal x y))` |
| `memq` | 1 | codelets:727 | `member` with `eq` |
| `member` | 1 | codelets:835 | Franz `member` tests with `equal` (CL uses `eql`) |
| `sortcar` | 2 | pnet-functions:13, codelets:555 | Sort by car. Both calls pass a nil predicate, which means alphabetical (`alphalessp` on print names) |
| `concat` | 17 | node names `(concat 'node- 31)` | Concatenate print names and intern in NUMBO (see the case rule below) |
| `uconcat` | 1 | codelets:1191 | Same, but returns an uninterned symbol |
| `get-pname` | 2 | init:141, start:97 (graphics dump file names) | Franz print name as a string |
| `string-length` | 1 | init:64 | Length of a string or of a symbol's print name |
| `getenv` | 1 | init:64 `(getenv "WINDOW_GFX")` | `sb-ext:posix-getenv`. Returns `""` when unset, which is what the caller's `string-length` needs. |
| `new-vector`, `vref`, `vset` | 1 each | pnet-graphics:112-146 | `make-array`/`svref` |
| `print` | 1 | init:99 `(print r) (terpri)` | Franz `print` = `prin1` (CL `print` adds a leading newline and a trailing space) |
| `intern`, `find-package` | 1 each | codelets:1191 `(intern (uconcat "brick" i) (find-package "keyword"))` | `intern` also accepts a symbol. `find-package` also accepts a lower-case name. Together they give `:brick1`, the cytoplasm message the next line sends. |

Shadowed in `package.lisp` because the source defines its own function
under a CL name (otherwise a CL package-lock error): `round` (codelets:1287,
nearest multiple of 5/10/50) and `ratio` (codelets:1173).

Franz has no package system, so the `intern` / `find-package` call on
codelets:1191 is unusual. It looks like Defays had some Common-Lisp-style
helpers on his system. Since only that one line uses them, the shims are
written to make exactly that line work.

#### Case rule for print names

Franz is case-sensitive and the source is lower case. SBCL reads
`node-31` as `NODE-31`. The name functions (`concat`, `uconcat`,
`get-pname`, `alphalessp`, `string-length`) translate with the
readtable-case `:invert` rule: an all-lower-case name is upcased, an
all-upper-case name is downcased, a mixed-case name is kept. So
`(concat 'node- 31)` is `eq` to the symbol `node-31` read from source, and
`(get-pname 'gr12)` is `"gr12"`, as in Franz.

#### Known differences (not changed)

- Flonums: Franz flonums are doubles. SBCL reads `0.9` as a single float.
  `*read-default-float-format*` is left alone, because changing it globally
  would affect everything else running in the image. This changes only the
  precision of activation arithmetic.
- `fix` on negative non-integers rounds down, as the manual says. The source
  only calls it on non-negative values.

#### Noted for later items

- `init.lisp:3` `(defvar *print-array*)`: SBCL reports a package-lock
  violation ("globally declaring *PRINT-ARRAY* SPECIAL"), because the
  variable is a CL special already. This is not a Franz built-in; it is left
  for item 9.
- (Done in item 4; see "RECONSTRUCTED: `my`" below.)
  `my` (16 calls in pnet-functions, e.g. `(my :activation)`) is used inside
  `pnode` methods and defined nowhere in the source. It is the Flavors-side
  "send to self" idiom, presumably `(send self ...)`, to be provided by
  item 4. The `(declare (macros t))` at the top of the file suggests it was
  a macro in Defays' environment. `send`, `defflavor`,
  `defmethod`, `make-instance` and `compile-flavor-methods` are item 4 too.
- Coderack `cr-*` (item 5). Graphics primitives `open-window`,
  `clear-window`, `draw-rect`, `draw-unfilled-rect`, `draw-text`,
  `draw-number`, `erase-rect`, `dump-window`, `window-width` and
  `window-height` are item 6.

### Item 4: Flavors on CLOS

#### Source edits

None. The ported files are unchanged by this item. Everything is in
`src/flavors-compat.lisp` and `src/package.lisp`.

#### Package changes (`src/package.lisp`)

- `DEFMETHOD` and `MAKE-INSTANCE` are shadowed. Flavors' `(defmethod (flavor
  :msg) ...)` and keyword `make-instance` replace the CL ones; a symbol
  method name still goes to `cl:defmethod`.
- `TYPE` is shadowed. The cytoplasm methods in cyto-def.lisp (`:cyto-brick-block-nodes`,
  `:find-new-target`, `:free-blocks`, `:free-cyto-nodes`,
  `:free-secondary-cyto-nodes`) do `(setq type (send (car x) :type))`, and
  `cytoplasm` has no `type` ivar, so this sets a global variable. With
  `CL:TYPE` that is an SBCL package-lock error at run time ("Lock on package
  COMMON-LISP violated when setting the value of TYPE"). The source never uses
  `type` as a CL type specifier, so shadowing it has no other effect. It is
  also an ivar of `cyto-node` and `cyto-context`.

#### Flavors used by the source (census)

Every `defflavor` form (pnet-def: `pnode`; cyto-def: `cytoplasm`,
`cyto-node`, `cyto-current-target`, `cyto-context`) has plain-symbol ivars
with no init values, an empty component list, and exactly the three options
`:settable-instance-variables :inittable-instance-variables
:gettable-instance-variables` in bare form (all ivars). Comments sit between
the component list and the options; the reader drops them.

Every `defmethod` form (pnet-functions: 14 on `pnode`; pnet-graphics: 3 on
`pnode`; cyto-def: 15 on `cytoplasm` / `cyto-node`) has the form
`(defmethod (flavor :msg) lambda-list body...)`. All are primary methods
(no `:before`/`:after`/whoppers), and lambda lists use only `&optional`.
`pnode :modify-threshold` starts its body with `(declare (special ...))`.

Other Flavors forms: `send` (always with a literal keyword message),
`make-instance` with a quoted flavor name and keyword init,
`compile-flavor-methods` (pnet-def top level and inside init.l's
`init-chiffre`), and `my`.

#### How the layer works

- A flavor is a CLOS class. Each ivar is a slot with `:initform nil` and,
  if inittable, an `:initarg` keyword. Uninitialized ivars are nil, not
  unbound. Franz Flavors instances start out filled with nil, and the
  source depends on it (e.g. `(send node :instances)` on a fresh node).
- Messages are kept in a per-flavor hash table (message → function of
  `self` + lambda list). `send` searches the class precedence list and
  signals an error for an unhandled message or a non-instance. Symbols are
  not dereferenced: the source `eval`s symbolic neighbor names itself
  (`initialize-pnet-2`, `(send (eval pnode) ...)`).
- In a method body, each ivar of the flavor (and its components) is a
  `symbol-macrolet` over `(slot-value self 'ivar)`, so free references
  and `setq`s reach the instance. A `let` of the same name shadows it.
  Free variables that are not ivars stay global, as in Franz.
- `:gettable-` generates `:ivar` messages and `:settable-` generates
  `:set-ivar`. Settable implies gettable and inittable. Both the Flavors
  spelling `:initable-` and the source's `:inittable-` are accepted, and the
  `(:option var...)` list form is supported. Unknown options are errors.
- `make-instance` sends `:init` (with the init plist) if the flavor
  handles it. The source defines no `:init`.
- `compile-flavor-methods` expands to nil.

#### RECONSTRUCTED: `my`

`(my :msg args...)` = `(send self :msg args...)`. It is called 16 times in
pnet-functions, always inside `pnode` methods, and defined nowhere in the
digitized source or in the scan. The messages it is called with are both
gettable ivars (`(my :activation)`) and methods
(`(my :activation-decay-factor)`), so it must send rather than read a slot.

#### Census check

`tests/flavors-compat-tests.lisp` READs all 7 source files, collects every
literal message in `(send x :msg ...)` and `(my :msg ...)`, and checks
that each is handled by one of the five flavors once pnet-def, pnet-functions,
pnet-graphics and cyto-def are loaded. None is unhandled.

### Item 5: Coderack (RECONSTRUCTED)

#### Source edits

None. `src/coderack.lisp` is new code. No ported file was changed.

#### What was missing, and where the definition comes from

The scan and the digitized files define none of `cr-make-coderack`,
`cr-hang`, `cr-choose`, `cr-empty?` or `cr-empty-coderack`.
`create-coderack` is **not** missing: it is defined in `codelets.lisp`
(CREATE-CODERACK FUNCTION, codelets:207) on top of `cr-make-coderack`, so
`coderack.lisp` does not define it.

Sources for the reconstruction (chapter page numbers are the printed pages
of the chapter):
- p.135: codelets are chosen probabilistically from the Coderack, "its
  likelihood of being chosen is proportional to its urgency". A codelet
  stays on the rack "until chosen to run".
- p.143: the chance of a codelet being chosen next is "the ratio of the
  codelet's urgency to the sum of all the urgencies of all the codelets in
  the Coderack".
- p.150: selection is two-phase, once at posting and again at choosing.
- Call sites (`grep -n 'cr-' src/*.lisp`):

| Call | Where | What it says about the signature |
|------|-------|----------------------------------|
| `(cr-make-coderack 'my-coderack (list %upper-urgency% ... %fifth-urgency% 0))` | codelets:208 | the rack is named by a symbol, and made with a list of urgency levels: `(600 300 150 7 4 1 0)` after `init-chiffre` |
| `(setq *coderack* 'my-coderack)` | codelets:211 | every other cr- call gets the symbol |
| `(cr-hang *coderack* form urgency)` | ~45 sites in codelets, pnet-functions, init, start | the urgency is always one of the levels, or 0 from `find-interest-in-pnet` |
| `(eval (cr-choose *coderack*))` | start, init `quick` | returns the codelet form; the caller evals it |
| `(cr-empty? *coderack*)` | start:78 | predicate, checked before `cr-choose` |
| `(cr-empty-coderack *coderack*)` | start:32, 69 | removes every codelet |
| `; (cr-choose *coderack* t)` with `; (eval (car rescod))` | start:80, 103-104 (commented out) | with a true 2nd argument, the result is a list whose car is the form |

#### Reconstruction choices (not settled by the sources)

- **Bins.** The rack holds one bin per urgency level, as in the Jumbo/Copycat
  coderacks the chapter refers to. A bin is chosen with weight
  `level × count`, then a codelet is chosen uniformly inside it. So each
  codelet's chance is urgency / total urgency, exactly as on p.143. The
  chosen codelet is removed.
- **Named by a symbol.** The rack structure is kept on the name's property
  list (`(get 'my-coderack 'coderack)`). `cr-get` also accepts the structure
  itself.
- **Urgency 0.** Its p.143 probability is 0, so it is never chosen while any
  positive-urgency codelet is on the rack. If only urgency-0 codelets are
  left, one is chosen uniformly. The rack is not empty (`cr-empty?` is nil),
  and start.lisp only calls `cr-choose` on a non-empty rack, so it has to
  return something.
- **Unknown urgency is an error.** `cr-hang` with an urgency that is not one
  of the rack's levels signals an error, so mismatches surface as porting
  bugs. The source never does this: every urgency comes from the
  `%...-urgency%` globals the rack was made with.
- **`(cr-choose rack t)` returns `(form urgency)`.** Inferred from the
  commented-out `(eval (car rescod))`. The live source never passes `t`.
- **Empty rack.** `cr-choose` returns nil.
- **RNG.** CL `random` on `*random-state*`, the same generator the codelets
  use. Seed it with `(setq *random-state* (sb-ext:seed-random-state n))`.
- **Helpers not called by the source:** `cr-get` (name → rack) and
  `cr-count` (number of codelets). The tests use them.

#### Tests

`tests/coderack-tests.lisp` (65 checks) covers:
- make, hang, choose, emptiness, clearing, and independent racks;
- draining 60 codelets, each comes out exactly once;
- urgency 0, error cases, and reproducibility under a fixed seed;
- statistical tests of weighted choice (4.5σ bounds on 20k-40k seeded
  trials, including several codelets per bin and all of init-chiffre's
  levels at once);
- the source's own `create-coderack` (codelets.lisp) with init-chiffre's
  urgency values.

A negative control (uniform bin choice instead of weighted) fails 17 checks.

### Item 6: Graphics stubs and the `reactivate-cyto` typo

#### Source edits

None. The graphics code in `pnet-graphics.lisp`, `start.lisp` and `init.lisp`
is unchanged. The stubs are in a new file, `src/graphics-stubs.lisp`, loaded
before `pnet-def.lisp`.

#### `reactivate-ctyo` → `reactivate-cyto`

Nothing more to change. This was already fixed as an OCR fix in item 2 (see
the start.lisp row above): the scan reads `reactivate-cyto`. Confirmed: both
calls in `start.lisp` (lines 71 and 100) use `reactivate-cyto`, which
`init.lisp:103` defines, and no ported file contains `ctyo`. A test checks
this.

#### Stubs (census)

Method: load every file inside one `with-compilation-unit`, then list the
functions that are still not `fboundp` (SBCL's undefined-function warnings).
Before the stubs, that gave the 10 primitives below, plus `reactivate-cyto`
and `refresh-everything`. Those two are defined in `init.lisp`, which does
not load yet (the `*print-array*` package lock, item 9).

| Stub | Called from | Returns |
|---|---|---|
| `open-window ()` | pnet-graphics `init-pnet-graphics` | nil |
| `clear-window ()` | pnet-graphics `init-pnet-graphics` | nil |
| `window-height ()` | pnet-graphics `init-pnet-graphics` | 800 (`*stub-window-height*`) |
| `window-width ()` | pnet-graphics `init-pnet-graphics` | 512 (`*stub-window-width*`) |
| `draw-rect (x1 y1 x2 y2)` | pnet-graphics `drawbox` (filled box) | nil |
| `draw-unfilled-rect (x1 y1 x2 y2)` | pnet-graphics `outline-regions` | nil |
| `erase-rect (x1 y1 x2 y2)` | pnet-graphics `erasebox`, `shrink-box`, `(pnode :draw-box)` | nil |
| `draw-text (x y string)` | pnet-graphics `outline-regions` | nil |
| `draw-number (x y n)` | pnet-graphics `(pnode :draw-box)` | nil |
| `dump-window (name)` | start.lisp `config`, init.lisp `refresh-everything` | nil |

- **Window size.** `setup-regions` divides by the window size, so the size
  stubs have to return numbers. 800 high × 512 wide is a nominal size. It is
  taller than wide because `setup-regions` says it assumes "height > width".
  With these values the whole graphics path runs (the test does this) and
  draws nothing.
- **Call counting.** Each stub counts its calls in `*graphics-stub-calls*`,
  so the tests can check that the graphics code reached it. The list of stubs
  is in `*graphics-stubs*`.
- **`%graphics%` stays nil.** `init.lisp` has `(defvar %graphics% nil)`, and
  `init-chiffre` sets it to t only when `WINDOW_GFX` is non-empty.
  `tests/run-tests.sh` now runs `unset WINDOW_GFX`.

#### Tests

`tests/graphics-tests.lisp` (26 checks) covers:
- each stub, and that `pnet-graphics.lisp` loads;
- the census: every function still undefined after a full load is defined
  in a source file, every stub is called somewhere in the source, and no
  stub redefines a source function;
- with `%graphics%` bound to t, `init-pnet-graphics`, `display-pnet` and
  `update-pnet-display` (including a box shrink) and `dump-window` run on the
  real 88-node `*pnet*`, and every stub gets called;
- the `reactivate-cyto` check, and the `%graphics%` default.

A negative control (stubs unbound and `graphics-stubs` dropped from the load
list) fails 16 checks, including the census.

### Item 7: Compile the Pnet files

#### Source edits

None. `pnet-def.lisp` and `pnet-functions.lisp` go through `compile-file`
unchanged, with no errors and no undefined-function warnings.

#### New file `src/globals.lisp` (loaded after `graphics-stubs`, before `pnet-def`)

Before this item, compiling the two files gave 207 "undefined variable"
WARNINGs. Each one was a Franz free global: a variable SETQed without any
declaration. Franz treats that as an assignment to the global value, and so
does SBCL at run time, so the warnings did not change behavior. To clear them
without editing the source, `globals.lisp` proclaims the true globals special
with **value-less `defvar`s**. A value-less `defvar` binds nothing and sets no
value. These names are proclaimed:

- the 91 pnode holders that `init-pnet` SETQs (`node-1` … `times10-15`,
  `operand result+ resultx similar operation instance`, `plus minus times`);
- `*pnet*`, which is SETQed at top level in `pnet-def`;
- forward declarations for every global that `init.lisp` DEFVARs (`%k%`,
  `%first-threshold%`, `*coderack*`, …). `init.lisp` loads later, and its own
  `defvar`s still set the initial values, because the variables are unbound
  until then. `*print-array*` is not included, because it is a CL symbol
  (item 9).

Rule: a name is proclaimed only if **no source file binds it lexically** and
it is **not a flavor instance variable**. Instance variables are
`symbol-macrolet`s, which cannot be global specials. A proclaimed name that
some function binds with LET would make that binding visible to a free SETQ
in a function it calls, which would change behavior. The test checks this
rule against every source file.

#### Warnings left in place (original code, not changed)

| File / function | Warning | Why it stays |
|---|---|---|
| pnet-functions `(pnode :hotter-neighbor-activation)` | undefined variable `NODE` | `(setq node (car link))` with no binding. Many codelets bind `node` with LET, so proclaiming it special would leak this SETQ into them. Left undeclared, it is a global SETQ here and a lexical local everywhere else, as in compiled Franz. |
| pnet-functions `(pnode :suppress-instances)` | undefined variable `RES` | `(setq res nil)` with no binding. Same reasoning: `res` is a LET local in dozens of functions. |
| pnet-functions `initialize-codelet` | unused `x`, `base-urgency`, `arguments` (style) | as written in 1987 |
| pnet-functions `(pnode :link-length)` | unused `&optional k` (style) | as written in 1987 (the body uses `%k%`) |

Because of the two `NODE`/`RES` WARNINGs, `compile-file` of `pnet-functions`
returns failure-p = T. `pnet-def` compiles with no warnings at all.

#### Undefined functions

None. Everything the two files call is defined in the shims or in the files
themselves (`cr-hang` is in `coderack.lisp`).

#### Observations

- `init-pnet` creates 91 pnodes. `*pnet*` lists 88 of them: `plus`, `minus`
  and `times` are created but are never in `*pnet*`.
- `(initialize-pnet)` needs the `init.lisp` parameters
  (`%initial-activation%`, `%first-threshold%`, …).
- `(initialize-pnet-2)` resolves every neighbor symbol of every pnode to a
  pnode, so no neighbor name in pnet-def is an OCR casualty.

#### Tests

`tests/pnet-compile-tests.lisp` (47 checks):
- compiles both files to a temp directory, and checks there are no errors,
  no warnings for pnet-def, exactly `{NODE, RES}` undefined variables for
  pnet-functions, no undefined functions, and the 4 listed style warnings;
- the `globals.lisp` census (the rule above, plus full coverage of the
  `init-pnet` holders and the `init.lisp` defvars);
- the 91 holders and 88 `*pnet*` pnodes created by the compiled `init-pnet`;
- `(initialize-pnet)` with `init.lisp`'s defvar values (it resets every
  pnode and normalizes codelet thresholds), `(initialize-pnet-2)`, and one
  `spread-activation-in-pnet` cycle.

A negative control (`operand` swapped for `res` in `globals.lisp`) fails 7
checks.

### Item 8: Compile the cytoplasm and codelets

#### Source edits

None. `cyto-def.lisp` and `codelets.lisp` go through `compile-file`
unchanged, with no errors and no undefined-function warnings.

#### Package change (`src/package.lisp`): `MIN` shadowed

`eliminate` (codelets.lisp:508–510) does `(setq min (car x))` and then
`(remove-dd min list)`. `min` is never bound, so it is a Franz free global
(checked on the scan, PDF p.46). With `CL:MIN`, SBCL warns "violating package
lock on COMMON-LISP:MIN" at compile time and signals a package-lock error at
run time. `MIN` is now shadowed, the same treatment as `TYPE` in item 4.
`franz-compat.lisp` defines the function `numbo::min` as `CL:MIN`, so the
three function calls (`pnet-functions:268`, `codelets:158`, `codelets:1183`)
are unchanged.

#### `src/globals.lisp`: new proclamations (same rule as item 7)

Names that no source file binds lexically and that are not flavor instance
variables:

| Names | Where they are used free |
|---|---|
| `*cytoplasm* *context* *temperature*` | SETQed by `init-cytoplasm` (cyto-def). Used everywhere. |
| `*problem-solved*` | `propagate-success`, `replace-target`, `config` |
| `cyto-target` | `read-target`, `compare-b-to-t`, `replace-target`, `reactivate-cyto`, `config`. Created by the `create-cyto-node` codelet with `(set name ...)`. |
| `%operand% %result+% %resultx%` | `repump`, `config` |
| `a brick bricki cyto-bricki` | `read-brick` |
| `cont n` | `propagate-success` |
| `pp` | `replace-target` |
| `diffrel` | `sim` |
| `div` | `round` |
| `liste` | `look-for-new-block`. Its LET binds `list` but the body uses `liste`; the scan (PDF p.55) has the same, so this is not an OCR error. |
| `similarity` | `look-for-diff` |
| `reserve weights` | `check-temperature` |
| `min` | `eliminate` (see above) |
| `lv` | `cyto-node :lower-neighbor`, `:lower-dtarget-neighbor`, `:upper-neighbor` |

Each scratch variable is SETQed before it is read inside the same function,
so a global is the right meaning.

#### Warnings left in place (original code, not changed)

Undefined variables, all of them **bound lexically somewhere else** in the
source (so proclaiming them special would turn those LETs dynamic), left
undeclared like `NODE`/`RES` in item 7:

- cyto-def: `type`, `status`. These are cytoplasm methods that SETQ names
  which are ivars of `cyto-node`, not of `cytoplasm`. `type` must stay
  unproclaimed: it is also an ivar (`symbol-macrolet`).
- codelets: `activation address current-target cyto-block1 cyto-block2 diff
  n1 n2 n3 new nn node1 node2 oper target values-to-find` (plus `node`, `res`,
  `type` and `status` again; SBCL reports each name only once per image).
  Example: `look-for-blx` uses `current-target`, `values-to-find`, `node1`,
  `node2`, `cyto-block1` and `cyto-block2` free, while its twin
  `look-for-bl+` binds the same names in a LET.

Style warnings: unused locals as written in 1987, for example `list`
(`look-for-new-block`, see above), `st1`/`st2`, `activation`, `cyto-block`,
`target`, `node`, `type`, `level`, and `current-target` "assigned but never
read". `compile-file` of both files returns failure-p = T because of the
undefined-variable WARNINGs.

#### Undefined functions (whole system)

**None.** When all seven source files are compiled in one compilation unit,
nothing is reported undefined. That includes `init.lisp` and `start.lisp`,
with init.lisp's `(defvar *print-array*)` package-lock error continued past
for the census only; that error is item 9. A full `load.lisp` load still
fails on that line.

#### Observations (for item 9)

- Mixed case in the printout: `Cyto-current-target` (codelets:101, in
  `compare-b-to-t`, scan PDF p.39) and `Neighbors` (cyto-def:323). Franz is
  case-sensitive, so in 1987 these were distinct, unbound symbols. The first
  would have been an unbound-variable error whenever `(sim block
  current-target)` is 3. SBCL's reader upcases both, so the port silently
  gets what Defays meant. Not changed.
- All free variables now have global (special) semantics, and everything
  bound with LET is lexical, which matches *compiled* Franz. If the 1987 run
  was *interpreted*, Franz bindings were dynamic, and a function could have
  seen a caller's LET-bound `node`/`res`/... through a free reference. No
  case where the code depends on that has been found yet; watch for it in the
  runtime errors of item 9.

#### Tests

`tests/cyto-codelets-compile-tests.lisp` (52 checks), wired into
`run-tests.sh` as "cyto/codelets compile tests". It uses
`tests/compile-helpers.lisp`, the helpers factored out of the item 7 test.
- Compiles pnet-def … codelets file by file. For cyto-def and codelets it
  checks: no errors, the exact undefined-variable sets above, no other full
  warnings, no package-lock warnings, no undefined functions, and only
  unused-variable style warnings.
- Whole-system census: one compilation unit, zero undefined functions, the
  only error is the `*print-array*` lock. A control file that calls an
  undefined function shows that the census catches it.
- `globals.lisp` census: the new names are special, never bound lexically,
  not ivars, and actually used. Every name left undeclared is bound
  lexically somewhere. `type` and `min` are NUMBO symbols.
- Compiled code runs: `eliminate` (the docstring example, and the global
  `min` it sets), `round`, `sim`, `remove-dd`, `randlist`, `ratio`. Then on a
  real cytoplasm for `(31 3 5 24 3 14)`, `read-brick 1` hangs one
  `create-cyto-node` codelet, which `cr-choose` returns; evaluating it
  creates the free "2b" cyto-node `cyto-brick1` with value 3.

Negative controls: dropping `(defvar liste)` from globals.lisp fails 5
checks. Un-shadowing `MIN` makes globals.lisp fail to load
(package lock), so the test exits 1.

### Item 9: Compile init and start; first boot

**No ported source file was edited.** Both runtime problems were Franz
dialect differences and are handled in `franz-compat.lisp`, with
`package.lisp` shadowing the two symbols.

| Where | Problem | Fix | Reason |
|---|---|---|---|
| init.lisp:3 `(defvar *print-array*)` | SBCL package lock: DEFVAR proclaims `CL:*PRINT-ARRAY*` special. A full load stopped here. | `DEFVAR` shadowed. For a CL symbol, the shim only assigns the value if the variable is unbound (here: no value, so nothing happens). Every other `defvar` is `CL:DEFVAR`. | Franz dialect (no package locks). The reader gives `CL:*PRINT-ARRAY*`, the real printer flag, so init-chiffre's `(setq *print-array* nil) ; don't print circular vectors` still works as Defays meant. Shadowing `*PRINT-ARRAY*` instead would have made that SETQ a dead variable. |
| codelets.lisp `temperature` | `(apply 'max misfort)` with `misfort` = nil, because there are no blocks or dtargets yet. The first `check-temperature` (iteration 3, x = 15) of every run failed with "invalid number of arguments: 0". | `MAX` shadowed: `(max)` returns 0, otherwise `CL:MAX`. | Franz dialect, **assumed**. The 1987 run cannot have failed here, so Franz `(max)` returned a number. 0 means "no misfortune", the same as `mean`'s value for an empty list. Not verified against a Franz manual or source. |

**Harness (new file, not ported source): `src/harness.lisp`**, loaded last by
`load.lisp`. `(run-config problem :seed s :max-iterations n)` does the following:
- seeds `*random-state*`, resets `*iteration*` to 0, runs `(init-chiffre)`, then `(apply #'config problem)`;
- returns `(:outcome :capped|:solved|:gave-up :iterations n :seed s :problem-solved p)`.

The cap works without changing `config`. `MOD` and `CR-EMPTY-CODERACK` are encapsulated with `sb-int:encapsulate`. Every iteration of the main loop calls one of them right after `(setq *iteration* y)`. The first call that sees `*iteration*` = n throws out of `config`, so exactly n iterations have run. Codelet calls to `mod` during iteration n-1 see n-1 and pass. With `:max-iterations nil` there is no cap.

**First boot results** (`(config 31 3 5 24 3 14)`, headless):
- The full load reports no failures, and `(init-chiffre)` runs.
- Seeds 1–8 each ran 2000 iterations without error.
- 500 iterations take about 10 ms.
- Seed 1 with no cap ran **11576 iterations without error and then gave up**: the coderack emptied after the last retry, start.lisp's `(t (return))`. It never printed "Done :".
- None of seeds 1–8 solved the problem within 2000 iterations. **This is item 10's problem.**
- The output has the shape of `trace3.31`:
  - the target and the 5 bricks are created, in a random order;
  - blocks and dtargets are created and killed. With seed 31, the second block and the first dtarget (`CYTO-BLOCK27-V2` / `PLUS3-24-V2`, `CYTO-TARGET-4-V3` / `PLUS27-4-V3`) are the same ones as in the trace.

**Observations for items 10–11:**
- `trace3.31` contains "About to post codelet ..." lines. That message is printed only when `%verbose%` is true (pnet-functions:152), but `init-chiffre` sets `%verbose%` to nil. So the 1987 trace was made with `%verbose%` set to t by hand after `init-chiffre`. To compare against the trace, set it the same way.
- Node names print in upper case (`Node CYTO-TARGET created`), where the trace has lower case. This is because `format ~a` prints SBCL's upcased symbols. It is cosmetic, and not changed.

**Tests:**
- `tests/boot-tests.lisp` has 24 checks and is wired into `run-tests.sh` as "boot smoke test". It checks:
  - the full load;
  - `init-chiffre`'s side effects, including `cl:*print-array*` being nil;
  - a 500-iteration seed-31 run, which must end `:capped` with no error, plus checks on the output's shape;
  - reproducibility: the same seed gives byte-identical output, and a different seed gives different output;
  - that a 10-iteration cap stops at exactly 10.
- Runtime errors inside a run are caught and reported as check failures.
- Negative control: restoring CL's `(max)` behavior fails 9 checks.
- `tests/franz-compat-tests.lisp` gained 18 checks for `MAX` and `DEFVAR` (178 in total).
- `tests/cyto-codelets-compile-tests.lisp`: the whole-system census no longer expects one package-lock continue for init.lisp. It now requires zero.

### Item 10: Run to completion

**No ported source file was edited, and no parameter was tuned.** Puzzle 3
runs to "Done :" as written. It just rarely does, which is what the chapter
reports.

**The chapter says 1987 Numbo did not solve this puzzle.** On p.152 (Fig.
III-6, puzzle 3 = target 31, bricks 3 5 24 3 14), Defays writes: "Notice
that neither Numbo nor the human subject solved the problem (even though it
has four different solutions!)". `trace3.31` also stops before any "Done".
So a run that does not solve it is not, by itself, a sign of a porting bug.

**The port solves the chapter's easy puzzles as the chapter describes:**

| Puzzle | Seeds 1–10 | Iterations | Solution (all seeds) | Chapter |
|---|---|---|---|---|
| #7: 6 from 3 3 17 11 22 | 10/10 | 22–31 | 3 + 3 | "immediately comes up with the solution 3 + 3" (p.153) |
| #8: 11 from 2 5 1 25 23 | 10/10 | 27–544 | (2 x 5) + 1 | "it will immediately answer 2 x 5 + 1" (p.153) |
| #1: 114 from 11 20 7 1 6 | 10/10 | 38–48 | (20 x 6) - (7 - 1) | the sample run, Fig. III-3 (p.144): the same solution |

That is strong evidence that the pipeline works as Defays describes: target
and bricks read in, Pnet activation, blocks, derived targets,
`replace-target`, `propagate-success` and `decompose`. Item 11 will run these
puzzles over 20 seeds.

**Puzzle 3, seeds 1–100, at most 20000 iterations each:**

| Outcome | Seeds |
|---|---|
| "Done :" with a **valid** solution, 31 = (14 x (5 - 3)) + 3 | 3 (seeds 13, 18, 78; 6525, 1328 and 14089 iterations) |
| "Done :" with an **invalid** decomposition (see below) | 8 (22, 32, 67, 73, 77, 82, 86, 93) |
| gave up (coderack empty after the last retry) | 83 |
| capped at 20000 | 5 (10, 45, 52, 55, 81) |
| runtime error (see below) | 1 (16) |

The test uses **seed 18**: it solves the problem in 1328 iterations (a few
ms), and the checker accepts the result.

**Solution checker (new file, not ported source): `src/solution-checker.lisp`**,
loaded after `harness`. `(check-solution output problem)` returns
`(values valid-p reason expression)`. It works like this:
- It parses the paragraphs that `decompose` prints after "Done :".
- It rebuilds the tree from the root, which is taken to equal the target.
- It requires every leaf to be a `CYTO-BRICKi` with the i-th brick's value, and each brick to be used at most once.
- Every other operand must itself be derived.
- Every step must be valid arithmetic.

`decompose` prints the two operation-node neighbours other than the node being
expanded, so "to get N" means:
- for a PLUS node, N = a + b or N = |a - b|;
- for a TIMES node, N = a x b or N = a / b (exact; `decompx` makes such derived targets).

The checker accepts whichever relation holds.

**Original behaviour found (not changed, documented):**

1. **`kill-block` leaves dangling operation nodes, so some "Done :" results are invalid.**
   - `kill-block` (codelets.lisp:684-688) only cascades when the parent of a block's upper operation node is a block (`"4bl"`) or the target (`"1t"`). Checked on the scan, PDF p.49: there is no `"3dt"` clause.
   - Suppose `decomp+` has used a block against a *derived* target, e.g. block 9 vs dtarget 7, which makes `PLUS7-2` and dtarget 2. If that block is later killed, its own children are freed, but the operation node above it is not removed.
   - The killed block keeps its success = 1, so once dtarget 2 is matched, `propagate-success` climbs through it and sets `*problem-solved*`.
   - `decompose` then prints the killed block as an operand, with no derivation of its own. Seed 93: `31 = 24 + 7`, `7 = 9 - 2`, `2 = 5 - 3`, and "Node CYTO-BLOCK9-V3 killed" comes before "Done". The 3 x 3 that made the 9 had freed brick 4, which 5 - 3 then reused.
   - The checker rejects these runs ("... is used but never derived, and is not a brick").
   - This is not a porting error. The level-ordered tree logic is as printed, and it does not involve dynamic vs. lexical binding: `kill-block` uses only its own LET variables and arguments.
2. **`reactivate-cyto` can run before every brick is linked to the Pnet** (seed 16).
   - The first `(reactivate-cyto)` runs at x = 40, which is main-loop iteration 28.
   - On seed 16, brick 4's `link-to-pnet` codelet (urgency 600) was still on the coderack at that point, so its `plinks` was nil.
   - `(send (eval nil) :set-activation ...)` then signals "SEND: NIL does not handle the message :SET-ACTIVATION".
   - Franz Flavors `send` on nil would also have failed, so this is a scheduling race in the 1987 code, hit about 1 time in 100 here. The coderack's selection rule is a reconstruction (item 5), so the exact frequency depends on it. Not changed.
   - In oracle mode (shared RNG) the race has a second form. If a brick's `read-brick` codelet has not run yet either, `(eval 'cyto-brickN)` fails first, with "The variable CYTO-BRICK3 is unbound." (puzzle 1, oracle seed 323). Over oracle seeds 1–400 of the 11 puzzles, 4 runs error, all at x = 40: puzzle 1 seed 40 and puzzle 3 seeds 162 and 272 in the SEND form, and seed 323 in this one. `python/tests/test_full_runs.py` runs seeds 40 and 323.

**Tests:** `tests/solution-tests.lisp` has 25 checks and is wired into `run-tests.sh`. It covers:
- The checker on hand-written decompositions: a valid one; a brick used twice; a wrong brick value; bad arithmetic; the wrong target; no "Done :"; nothing after it; a truncated paragraph; seed 93's dangling block; division through a TIMES dtarget; and the "Obvious." root block.
- Seed 18: `(:outcome :solved :iterations 1328)`, the checker accepts `31 = (14 x (5 - 3)) + 3`, and the output is byte-identical on a rerun.
- Seed 93: it reaches "Done :", and the checker rejects it.
- Puzzles #7, #8 and #1 with seed 1: each is solved with the chapter's solution.
- Negative control: disabling the "brick used twice" rule fails 1 check.

### Item 11: Validation against trace3.31 and the chapter

**No ported source file was edited, and no parameter was tuned.** One non-source
change: `run-config` (src/harness.lisp) gained a `:verbose` key, which sets
`%verbose%` to t *after* `init-chiffre` (which sets it to nil). That gives the
"About to post codelet" lines trace3.31 has.

New test-side files (not ported source):
- `tests/trace-tools.lisp`: parses output into events, defines the event invariants and the counts. It is in its own package `NUMBO-TRACE`, because a LET of a name the source proclaims special (`n`, `a`, ...) would be clobbered by the codelets; that happened while writing it.
- `tests/chapter-runs.lisp`: the 11 chapter puzzles × 20 seeds. It writes the tables in `src/RESULTS.md` (about 2 s).
- `tests/validation-tests.lisp`: 47 checks, in `run-tests.sh`.

**What trace3.31 is.** It is 64 lines: page 1 and the top of page 2 of a May 19 1987, 17:05 run of `(config 31 3 5 24 3 14)`, with `%verbose%` t. It has 48 node events (32 created, 16 killed) and 5 posts (4 `look-for-blx`, 1 `look-for-diff`), and it stops mid-run with no "Done". Its opening follows Fig. III-6's Protocol 1 (p.152): 3 x 3 = 9, no; 24 + 3 = 27; 4?; 1? no; ... 24, 7?; 2? no. The protocol is a condensed rendering, though: it has steps (e.g. 3 x 5 = 15) that are not in the trace's first page.

**File dates matter.** The printout headers give each file's date:

| File | Printed | vs. trace (May 19 17:05) |
|---|---|---|
| pnet-def.l | May 5 11:13 | before |
| pnet-functions.l | May 5 14:02 | before |
| cyto-def.l | May 19 14:00 | before |
| start.l | May 21 09:22 | **after** |
| codelets.l | May 21 09:31 | **after** |
| init.l | Jun 24 08:56 | **after** (5 weeks) |

So the trace was made with earlier versions of codelets.l, start.l and init.l than the ones we have. Exact agreement is expected only for what depends on the Pnet files.

**Similarities (checked by `tests/validation-tests.lisp`, seeds 1–20, 1000 iterations, verbose):**
1. **Same kinds of event, same format.** Every run prints only `Node X created`, `Node X killed` and `About to post codelet F (args)` event lines. The node names have the trace's shapes: `cyto-target`, `cyto-brickN`, `cyto-blockV-vK`, `cyto-target-V-vK`, `plusA-B-vK`, `timesA-B-vK`.
2. **Same structural invariants**, which hold on trace3.31 and on all 19 error-free runs:
   - the target and the five bricks are created once each;
   - versioned nodes are created in pairs (cyto node, then its op node, same version);
   - versions go 1, 2, 3, ... with no gaps;
   - kills come in pairs (op node, then the cyto node of the same version), of live nodes only, and puzzle nodes are never killed.
   - Controls show the invariant checker catches each kind of violation.
3. **Same opening.** The trace's first post is `look-for-blx (30 3 10)`, after four bricks are read, followed by `look-for-blx (30 5 6)`. The port's first post is the same `look-for-blx (30 3 10)` on 19/20 seeds, and 11/20 post `(30 5 6)` right after it, as the trace does.
   - The exception is seed 16, where a derived target forms before the bricks are all read; that is the item-10 `reactivate-cyto` race.
   - These two posts come from `activate`/`repump` and spreading in the Pnet (round(31) = 30 → node-30 → `times3-10`, `times5-6`). So the port's spreading arithmetic reproduces the 1987 numbers at the threshold.
4. **Same first moves.** The trace's ops (`times3-3`, `plus3-24`, `plus27-4`, `plus4-1`, `plus24-7`, `plus5-2`, `plus2-3` ...) are all moves the port makes in the first 40 node events of some seed: e.g. 9 = 3 x 3 then killed (seed 18), 27 = 3 + 24 → dtarget 4 → dtarget 1 (seeds 6, 7, 15, 19, 20), and 24 + 7 (most seeds).
5. **Same overall fate.** The trace has no "Done" in what survives, the chapter says that run did not solve the puzzle, and the port solves puzzle 3 on 2/20 seeds.

**Differences (documented, not changed):**
1. **Upper vs. lower case.** Franz printed symbols in lower case (`Node cyto-target created`); SBCL prints `Node CYTO-TARGET created`. Cosmetic. The tools compare with `string-downcase`, and `check-solution` uses `string-equal`.
2. **Many more posts, mostly `look-for-bl+`.**
   - In the first 48 node events, the trace has 5 posts and no `look-for-bl+`. The port has 24–78 (mean ≈ 55 over seeds 1–40), of which ≈ 38 are `look-for-bl+`.
   - Investigated, and no porting bug found:
     - *Stale `*iteration*`*: `modify-threshold` builds `(max 30 (add I (minus *iteration*) 60))` with `I` = `*iteration*` at post time, so a session that had already run other puzzles would raise thresholds during the reading phase. Presetting `*iteration*` to 200 or 1000 changes nothing (55 → 55, 56.5).
     - *Destructive `sortcar`* in `:activation-decay-factor`: CL `sort` can drop entries of the pnode's `instances` slot. It happened once in 40 runs, and a copying `sortcar` gives the same density (55.3) and solve rate. Kept as is; Franz `sortcar` is also destructive.
     - *Older parameters*: with init.l's top-level `defvar` values instead of `init-chiffre`'s (`%target-activation%` 180, `%dtarget-activation%` 100, `%brick-plus%` 25, `%dtarget-plus%` 50, `%upper-threshold%` 90, `%temperature-threshold%` 80, `%second-urgency%` 70), posts fall to 22.6 per 48 node events, `look-for-bl+` to 11.4, and puzzle-3 solves rise to 5/40. That is closer to the trace, but not equal.
   - `look-for-bl+` posts come from plus pnodes next to a newly activated dtarget (e.g. dtarget 4 → `plus1-3`, `plus2-4`, ...). Each link carries spreadable activation / √link-length, where link-length depends on the `result+` link node's activation, which `repump` sets (63–88 in the instrumented seed-18 run).
   - Conclusion: the plus/times balance (`activate` → node-multiply = 4√v → `repump`'s `%resultx%`/`%result+%`) and the activation parameters are in codelets.l and init.l, which postdate the trace. The trace's lack of `look-for-bl+` posts most likely reflects those earlier versions. Not tuned (TASK.md: no algorithm changes).
3. **No `;; gc:` lines.** The trace has a Franz GC message (`;; gc: flonum +25 (135)`); SBCL prints none.
4. **`Bricks :` spacing.** The trace prints `   Bricks : 3 5 24 3 14` (3 spaces); start.l's format string (May 21, 2 days later) has 1. Kept as printed.
5. **The trace has no `Graphics is OFF.`** because init-chiffre had been run before the `(config ...)` that was captured. The port prints it from `run-config`'s `init-chiffre`.

**Chapter puzzles:** see `src/RESULTS.md`. The big gaps investigated:
- **#11 (41 from 5 16 22 25 1)** is solved 20/20 (median 176 iterations), but the chapter says Numbo "has problems" with it. The mechanism matches the chapter: no pnode can suggest 16 + 25, because the only plus pnodes past 10 are `plus5-10`. On seed 14 the block 41 = 16 + 25 is made by `look-for-new-block`, the random background codelet start.l hangs every 5 iterations, which is the chapter's "stumble across a solution through the random combinations it conducts as a background activity".
  - How *often* that random codelet runs relative to Pnet-driven ones depends on the coderack's selection rule, which is reconstructed (item 5). The chapter gives no rate. The difference is one of degree. No porting bug found.
- **#6 (146)** is solved 1/20. The chapter only reports human data for #6, so there is no Numbo claim to compare with.
- **#10 (127)**: the chapter's 4 x 30 + 7 (via 6 x 5) never appeared in 20 seeds; the "more obvious" 6 x 22 - 5 dominates (13/17), which matches the chapter's description of the obvious routes. ((6 x 4) x 5) + 7 appeared once.

## Oracle hooks (loop0002)

loop0002 translates Numbo to Python and uses this port as the oracle: the
Python must produce the same event stream, run for run. These hooks make that
possible. **They are opt-in.** In default mode none of them is loaded, and the
port behaves exactly as before: all loop0001 test groups pass unchanged, and
README's seed-18 run still solves puzzle 3 in 1328 iterations.

**No ported source file was edited.** Everything is in `src/oracle.lisp` (new)
and `src/load.lisp`.

### Turning it on

- Set `(defvar cl-user::*numbo-oracle* t)` before loading `src/load.lisp`, or
  set the environment variable `NUMBO_ORACLE` to anything except `""` or `"0"`.
- `load.lisp` then loads `oracle.lisp` right after `package.lisp`, before
  `franz-compat`, so every later file reads the symbols it shadows.
- Once every file has loaded, `load.lisp` calls `numbo::oracle-install`.
- Run with `(numbo::oracle-run-config problem :seed s :max-iterations n :trace "file.jsonl")`.
  - It takes the same keys as `run-config`, plus `:rng-events` and `:float-check`.
  - It returns `run-config`'s plist, plus `:single-floats` and `:doubles-seen`.
  - A Lisp error ends the run with `:outcome :error` and `:error` holding the message.

### The hooks

| Hook | Where | Original behavior | Oracle mode | Reason |
|---|---|---|---|---|
| Double floats | `load.lisp` `numbo-load-file` | SBCL reads `0.9` as a single float | `*read-default-float-format*` is bound to `double-float` **only while each numbo file is loaded**. Afterwards the global value is still `single-float` (tested). | Franz flonums were doubles, and so are Python floats. The audit's top risk: threshold comparisons on activations would diverge. |
| `FLOAT`, `SQRT` shadowed | `oracle.lisp` | `(float 1)` and `(sqrt 16)` give single floats in CL, even when the literals are doubles | `(float x)` gives a double. `(sqrt r)` of a rational gives a double. Otherwise these are CL's functions. | Floats computed from integers: `:link-length` `(quotient (float 1) %length%)`, `activate`'s `(sqrt val)`, `ratio`, `sim`. `franz-compat`'s `quotient` also uses this `float`, because `franz-compat` loads after the shadowing. |
| `RANDOM` shadowed | `oracle.lisp` | the 7 `(random n)` calls (coderack x2, codelets x5) use CL's `*random-state*` | splitmix64, with rejection sampling for `n` (specification below). It is seeded by `oracle-run-config` with the run's seed, before `init-chiffre`. | A generator that both languages can implement bit for bit. CL's `*random-state*` plays no part (tested). |
| Copying `SORTCAR` | `oracle-install` replaces `franz-compat`'s `sortcar` | destructive `sort`. It can reorder or truncate a pnode's `instances` slot (item 11: about 1 run in 40) | `(stable-sort (copy-list list) ...)`. The caller's list is untouched. | Python's `sorted` never mutates. Same order as before: SBCL's list `sort` is already a stable merge sort. |
| JSON-lines trace | `sb-int:encapsulate` on `config`, `cr-choose`, `cr-hang`, `mod`, `cr-empty-coderack`, `create-cyto-node`, `create-op-node`, `disconnect`, `spread-activation-in-pnet`, `decompose` | none | One JSON object per line (events below). With no trace open, each encapsulation just calls through. | Comparing structured events instead of printed text. |

### The RNG, bit for bit

- **State.** An unsigned 64-bit integer. `(oracle-seed s)` sets it to `s mod 2^64`.
- **Next output** (all arithmetic mod 2^64):
  1. `state += 0x9E3779B97F4A7C15`
  2. `z = state`
  3. `z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9`
  4. `z = (z ^ (z >> 27)) * 0x94D049BB133111EB`
  5. return `z ^ (z >> 31)`
- **`(random n)`.** `n` must be an integer with 1 <= n <= 2^64; anything else is an error, as `(random 0)` already is in CL.
  1. `limit = 2^64 - (2^64 mod n)`
  2. Draw until `x < limit`.
  3. Return `x mod n`.
  - Even `(random 1)` draws.
- **Check value.** Seed 0's first output is `0xE220A8397B1DCDAF`, Vigna's reference value.
- **Test vectors.** `tests/oracle/rng-vectors.lisp` writes `python/fixtures/rng_vectors.json`:
  - the first 20 outputs for seeds 0, 1 and 18;
  - for each of those seeds, 20 `(random n)` calls, with `n` from 1 up to 2^64. They include `2^63 + 1`, where about half the draws are rejected; seeds 0 and 1 really do reject.
- **Cross-check.** An independent Python implementation of the specification above (a scratch check, not committed) reproduced the whole file.
- `tests/oracle-tests.lisp` checks that the committed file is what the oracle writes now.

### Trace events

- **Encoding of Lisp data** (codelet args, decomposition values):
  - integer → number; double → number, printed as the shortest round-trip form;
  - `nil` → `null`; `t` → `true`;
  - symbol → its upper-case name (`"CYTO-BLOCK27-V2"`); keyword → `":NAME"`;
  - string → `{"str": "..."}`, so `"free"` and the symbol `free` stay distinct;
  - list → array; dotted pair → `{"cons": [a, b]}`;
  - flavor instance → `{"obj": "cyto-node", "name": ...}`.
  - Any other value (a single float, a ratio, ...) is an **error**.
- **Plain JSON strings.** Fields that are always names or type strings (`name`, `type`, `codelet`, `op`, ...).
- **`args`.** Always an array (`[]` for `(look-for-new-block)`).

| Event | Fields | Written when |
|---|---|---|
| `start` | `problem`, `seed`, `max_iterations`, `rng`, `pnet` (the 88 `*pnet*` names in order) | first |
| `setup-choose` | `codelet`, `args`, `urgency`, `rack` | each of the 13 `(eval (cr-choose *coderack*))` before config's main loop |
| `iteration` | `n` (config's `y`), `x`, `temperature`, `rack` (`[[urgency, count], ...]`), `codelet`, `args`, `urgency` (null if the iteration chose nothing) | each main-loop iteration |
| `post` | `codelet`, `args`, `urgency` | every `cr-hang` |
| `node-created` | `name`, `type`, `value` | `create-cyto-node`, `create-op-node` (type `"5g"`, value null) |
| `node-killed` | `name`, `type`, `value` | `disconnect`: the op node, then the cyto node |
| `pnet` | `act`: the 88 activations, in `start`'s order | after each `spread-activation-in-pnet` |
| `rack-emptied` | | every `cr-empty-coderack` |
| `rng` | `n`, `value` | each draw, only with `:rng-events t`. Draws made inside `cr-choose` go into the choosing event's `rng` field instead. |
| `done` | `iterations`, `decomposition` (`[{op, a, va, b, vb, result}]`, parsed from what `decompose` printed) | "Done :" |
| `gave-up` / `capped` / `error` | `iterations` (as `run-config`); `error` adds `message` | last |

**How the main loop is detected.** `config` itself is not edited.
- **Iteration start.** As in `harness.lisp`, the first call to `mod` or `cr-empty-coderack` that sees a new `*iteration*` starts an iteration. This only counts after the set-up phase.
- **End of set-up.** The set-up phase ends after its 13th `cr-choose`, and only once that codelet has been evaluated. The 13th codelet can call `mod` itself (seed 14: `compare-b-to-t` → `digits-in-common`), which the first version mistook for iteration 0. For that one choice, the hook hands `config` the form `(oracle-end-setup 'form)`. It evaluates the codelet as `config`'s `eval` would, then switches phase.
- **Cap order.** The harness's iteration-cap encapsulation is installed *outside* the oracle's, so the iteration it stops at is never begun in the trace.

**What gets written when.**
- An `iteration` event is held until its choice is known: either `cr-choose` fills in the codelet, or the next event or the end of the run writes it with null.
- The `rack` and `temperature` fields are taken at the start of the iteration.

**`temperature` without side effects.**
- `temperature` → `collect-misfortune` SETQs the free global `current-target`, which `look-for-blx` reads.
- The hook therefore calls it inside `oracle-call-without-global-effects`, which restores any NUMBO symbol value it changed.
- Tested: the printed output of a run is byte-identical with the trace, RNG events and float checks on, and with no trace at all. The test includes puzzle 3, which passes x = 400, where `config` calls `temperature` itself.

### Every float is a double (tested)

- **The world walk.** `oracle-find-single-floats` walks everything reachable from the run's state, and returns any float that is not a double:
  - every NUMBO symbol's value and property list (the coderack lives on `my-coderack`'s plist);
  - from those: conses, vectors, hash tables, and every slot of pnodes, cyto-nodes and structures.
- **When it runs.** With `:float-check t`, at the start of every main-loop iteration and at the end of the run.
- **The trace writer.** It refuses any non-double float.
- **Results.** `tests/oracle-tests.lisp` runs the 11 chapter puzzles × seeds 1, 2 and 14 (cap 3000) with the float check on. No single float was found, and doubles were seen in every run.
- **Controls:**
  - a single float planted in a global is found;
  - the writer rejects `2.5f0`;
  - with `numbo::float` reverted to CL behavior (by hand, not in the suite), the first `pnet` event fails with "149.1228 (SINGLE-FLOAT) is not ... a finite double-float".

### Oracle-mode results

The 11 chapter puzzles × seeds 1–20, capped at 20000 iterations, take about 30 s:

- 184 solved, 34 gave up, 2 capped, **0 errors**.
- Traces are up to 7.6 MB per run (96 MB for all 220). They are not committed.
- No printed output contains a `d0` float.

The rates differ from `src/RESULTS.md` because the RNG is different, and so are the float precision and the sortcar behavior. Python item 13 compares them.

### Tests

- `tests/oracle-tests.lisp` (833 checks), in `run-tests.sh` as "oracle hooks". It covers:
  - the mode;
  - RNG reference values, rejection, errors, and the fixture;
  - the copying sortcar;
  - trace structure on 33 runs (every line is valid JSON for a Lisp parser, and, on the 11 seed-1 traces, for python3's `json` too; event types; the outcome; iteration numbering; 13 set-up choices; node events equal to the printed `Node ...` lines; decomposition);
  - doubles only;
  - determinism;
  - hooks that only observe.
- `tests/oracle-mode-tests.sh`, in `run-tests.sh` as "oracle mode is opt-in". In a default load (and with `NUMBO_ORACLE=0` or empty):
  - `RANDOM`, `FLOAT` and `SQRT` are CL's;
  - there is no `*oracle*` and no `oracle-install`;
  - `%first-decay-rate%` is a single float.

  `NUMBO_ORACLE=1` and `*numbo-oracle*` both turn oracle mode on.
