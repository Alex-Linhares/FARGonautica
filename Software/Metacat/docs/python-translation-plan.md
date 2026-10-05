# Translating Metacat 1.2 to Python: the plan

Loop0002, item 01, 2026-10-03. This is the counterpart of numbo's
`docs/python_translation_audit.md`: what the translation has to reproduce, how Python
will do it, and in which order. Items 02–17 of `ralph_loops/loop0002/iterations.md`
follow it. If an item finds a decision here wrong, it fixes this file and says so in
PROGRESS.md.

Sources read: `racket/compat.rkt` and `racket/utilities.rkt` in full, every `port:`
comment in `racket/engine/*.rktl` and `racket/engine.rkt`, `docs/code-map.md`,
`docs/trace-format.md`, `docs/porting-notes.md`, `docs/anomalies_and_quirks.md`,
`docs/divergences.md`, `docs/follow-ups.md`, and the original itself (counts below come
from `chez_scheme/original/`).

Prototypes and measurements made for this item:
- `python/tests/object_prototypes.py` and `test_object_prototype.py`: four object
  representations (six variants), checked against the Chez fixtures of
  utilities-battery.scm's object tests, plus the micro-benchmark;
- `python/tests/name_mapping.py` and `test_name_mapping.py`: the name mapping, checked
  on every name the original defines;
- `python/oracle/count-calls.ss`: counts `tell` and `delegate` calls in a real oracle run.

## Verdict

**Feasible, mechanical in most places, and testable all the way down.** The hard
problems were solved by the Racket port (loop0001): the PRNG, Chez's `map` and `sort`
orders, the evaluation-order sites, the printer, and the model/graphics couplings. Each
has a reference implementation in `racket/compat.rkt` and a Chez fixture in
`python/fixtures/`. What is new in Python:

1. **Truthiness.** In Scheme only `#f` is false. Python also treats `0`, `0.0`,
   `Fraction(0)`, `[]` and `""` as false. This is the biggest new risk; see "Booleans
   and truthiness".
2. **Arithmetic contagion.** Python has `int`, `Fraction` and `float`, but its `/`,
   `max`, `min`, `sqrt`, `exp`, `**` and `0 * x` don't follow Chez's exactness rules.
3. **No first-class continuations.** Escapes become exceptions. The one re-entrant use
   (`break`/`go`) becomes a blocked engine thread in the GUI and an exception headless.
4. **Speed.** Chez makes about 1,000 `tell` calls per codelet. Dispatch alone costs
   about 0.2 ms per codelet in Python, so a codelet will cost a few milliseconds, 20–50×
   Chez. The goldens have to run in parallel.
5. **Modules.** Python modules can be circular, and later files can assign to a
   module's globals. Load order still has to be imposed by hand.

Size: 21,700 non-blank, non-comment lines of Scheme. About 14,600 are the model
(utilities to memory.ss plus run.ss), 4,900 the SGL interpreter and panels, and 1,200
demos and gui.ss.

| Files | Lines | Item |
|---|---:|---|
| syntactic-sugar.ss, utilities.ss | 877 | 03 |
| constants.ss, setup.ss, coderack.ss, descriptions.ss | 1,856 | 04 |
| slipnet.ss, images.ss | 1,003 | 05 |
| workspace*.ss, formulas.ss | 1,870 | 06 |
| bonds.ss, groups.ss, concept-mappings.ss | 1,573 | 07 |
| bridges.ss, breakers.ss | 1,507 | 08 |
| rules.ss, answers.ss | 2,952 | 09 |
| themes.ss, justify.ss, trace.ss, jootsing.ss, memory.ss | 3,590 | 10 |
| run.ss | 288 | 11 |
| sgl-interpreter.ss, fonts.ss | 508 | 13 |
| general-graphics.ss and the 12 other *-graphics.ss | 4,396 | 14 (some engine parts earlier, as in Racket) |
| demos.ss, gui.ss | 1,177 | 15 |

## The Chez semantics the engine depends on

Each row gives the Python strategy and the module that implements it. `chez.py` is item 02;
`objects.py`, `sugar.py` and `utilities.py` are item 03. The tests are the Chez fixtures
(`python/fixtures/utilities/` and others), plus small capture scripts in `python/oracle/`
where a battery has no vector.

### Numbers

| Chez | Python | Notes |
|---|---|---|
| exact integer | `int` | bignums never occur in the model, but `int` is unbounded anyway |
| exact non-integer rational (`102/5`) | `fractions.Fraction` | **always normalised**: a result with denominator 1 becomes `int` (`chez.norm`), because `Fraction(2, 1)` is not an `int` for `integer?`-style tests, `random`, indexing or trace printing |
| flonum | `float` | |
| `(/ a b)` on exact numbers | `chez.div(a, b)`, giving `int` or `Fraction` | **never Python `/`** on values that are exact in Chez: `/` makes a float, and a float urgency changes codelet choices. `%` (utilities.ss: `(/ n 100)`) is the most common case |
| `(* 0 x)` with flonum `x` | `chez.mul` | Chez gives **exact 0** (checked: `(* 0 1.5)` → `0`, `(/ 0 2.5)` → `0`), Python gives `0.0`. Use `chez.mul`/`chez.div` wherever one operand can be a flonum. *Item 02 correction:* `+` and `-` differ too, at signed zeros only: exact 0 is their identity, so `(+ 0 -0.0)` and `(- 0 0.0)` are `-0.0` where Python gives `0.0` (`chez.add`/`chez.sub`; anomalies_and_quirks.md) |
| `max`, `min` | `chez.max_`, `chez.min_` | inexact contagion: `(max 3 2.0)` → `3.0`, `(min 1 2.0)` → `1.0`; Python keeps the `int` |
| `sqrt`, `exp`, `log`, `expt` | `chez.sqrt` etc. | exact in, exact out where the result is exact: `(sqrt 16)` → `4`, `(sqrt 1/4)` → `1/2`, `(exp 0)` → `1`, `(log 1)` → `0`, `(expt 1/2 2)` → `1/4`, `(expt 0.0 0)` → `1` (exact!). Otherwise IEEE doubles from libm (`math.sqrt` etc.), bit-equal on item 02's vectors. *Item 02:* `expt` has more rules: only a `1/2` power is an exact root (`(expt 8 1/3)` → `2.0`), an exact 1 base gives `1`, an exact 0 base gives `0` for a positive power, and `(expt 0.0 -1)` is `+inf.0` (anomalies_and_quirks.md) |
| `tanh` | `math.tanh` | workspace.ss's mapping strengths; Racket took Chez's own primitive, item 02 checked `math.tanh` against Chez bit for bit on about 1,100 arguments (`tanh-*` fixtures): equal |
| `exact->inexact` of a ratnum | `chez.inexact` (`float(Fraction)`) | correctly rounded in both; equal on item 02's 400 random ratnums |
| utilities.ss `round`, `floor`, `ceiling`, `truncate` (exact results) | `round_` (Python `round`: half to even, returns `int`), `math.floor`, `math.ceil`, `math.trunc`. *Item 17:* as built they are `chez.exact_round`, `exact_floor`, `exact_ceiling`, `exact_truncate` (utilities.py) | Chez's `round` is half to even too (`(round 5/2)` → `2`, `(round 7/2)` → `4`, `(round 2.5)` → `2.0`) |
| `=`, `<`, … | Python operators | mixed exact/inexact comparisons are exact in both |
| `eqv?`/`equal?` on numbers | `chez.eqv_p`, `chez.equal_p` | `(equal? 2 2.0)` is `#f` but Python's `2 == 2.0` is `True`, and likewise `Fraction(1, 2) == 0.5`. Only where a list compared with `equal?` can hold numbers of mixed exactness (memory.ss, rules.ss, justify.ss sites) |
| `1+`, `-1+`, `add1`, `sub1` | `add1`, `sub1` | |
| `random`, `random-seed` | `chez.random`, `chez.random_seed` | the 32-bit LCG of trace-format.md; `(random 1.0)` is `M/2^52` exactly; one module-level state. Must accept an `int` only (normalise Fractions first). *Item 17:* `chez.random` normalises a Fraction with denominator 1 itself |
| flonum printing | `chez.number_to_string` | shortest round-trip digits (Python's `repr` gives the same digits **except at exact ties, where Chez rounds the last digit up and Python to even**, item 02), laid out Chez's way: positional when the leading digit's exponent e is in (−4, 10), else `d.ddde<exp>` with no `+` and no exponent padding. So `1e21`, `1e-7`, `1.234567890125e11`, `1000000000.0`. Python's `repr` writes `1e+21`, `1e-07`, `123456789012.5` |
| ratnum printing | `str(Fraction)` | `"102/5"`, `"-1/2"`, as Chez; normalised ints print as ints |

Only 84 flonum literals appear in the model files (formulas.ss, bridges.ss and rules.ss
have 12 each). Everything else stays exact, so most arithmetic is `int` and `Fraction`.
That makes it safe *and* slow (see Risks).

### Evaluation order

Chez evaluates call arguments and `let` bindings in an order that depends on the shape of
the call (porting-notes.md, item 03). At top level, `(f a b c)` evaluates c, a, b;
`(list ...)` is left to right; a 2-binding `let` is right to left. Inside a procedure,
`(append a b)` evaluates b first. This item's own check also found a 2-binding `let`
inside a lambda that went *left to right* (`12`), unlike the top-level case. There is
no rule. **Python is left to right everywhere.** Wherever two or more arguments or
bindings have order-dependent side effects (random draws, posting codelets, building
structures, trace events, output), the translation spells out Chez's order as
sequential statements and marks the site `# chez: evaluation order`.

The sites, from the Racket port's per-file audits (porting-notes.md, items 04–11) and
`racket/compat.rkt`:

| Site (original) | What | Python |
|---|---|---|
| groups.ss:368–370, a group's `get-local-density` | `(append (neighbors self 'choose-left-neighbor) (neighbors self 'choose-right-neighbor))`; both draw | compute the **right** neighbours first (racket/engine/groups.rktl:371, `port:`). Found by a 2000-codelet harness run (`abc abd iijjkk` seed 3, codelet 735) |
| utilities.ss:690–700, `pairwise-map` | `(append (map ...) (pairwise-map (rest l)))` | the **recursive call first**, then the `map` (racket/utilities.rkt:720) |
| run.ss:234–239, `init-workspace` | a 4-binding `let` making the strings | last to first, as Racket does (run.rktl:229). It draws nothing, so it's equivalent, but keep the order |
| syntactic-sugar.ss:82, `for*` from/to | the bounds | `exp1` then `exp2` |
| syntactic-sugar.ss:121, `stochastic-if*` | the coin and the probability | **draw `(random 1.0)` first**, then evaluate `prob`: the probability expression may itself draw or read state |
| utilities.ss:400, `~` | `(random ...)` in a `let`, then `(prob? 0.5)` in the body | sequential already |
| workspace-structures.ss:70, `wins-fight?` | challenger's strength updated before the defender's | a body sequence, already ordered |
| descriptions.ss `make-description`, bridges.ss `get-incompatible-bridges`, `bridge-builder`, `breaker`, rules.ss `(list 'intrinsic (list od) (map ...))` | several arguments, at most one with effects | no change needed (audited in Racket items 04, 08, 09) |

Every other draw sits in a `let*`, a body, an `and`/`or`/`cond`, or a `map`/`filter`
whose order `chez.py` reproduces. The goldens (109 runs) and the extra seeds (720 runs)
showed no other site in Racket, which evaluates left to right like Python. So these
tables are complete for runs that reach the same code. Each item still re-audits its
files: for every call or `let` with two effectful subexpressions, it records the site in
the item's PROGRESS entry.

### Lists, `map`, `sort`, `remq`, `for-each`

| Chez | Python |
|---|---|
| proper list | Python `list`, **never mutated after construction** (the model never mutates pairs; only rule-graphics.ss:77 has a `set-car!`, which the Racket port turned into a copy). `cons` onto a list → `[x] + ls`; `cdr` loops → index loops (lists are short: strings ≤ 7 letters, coderack ≤ 100) |
| vector, table (vector of vectors) | Python `list` (fixed length), list of lists |
| dotted pair | none found in the model's data (the `(cons ...)` sites, 126, are audited per file); a non-list `cdr` is a `chez.Pair(car, cdr)` (item 02; not a tuple, since `*args` tuples are lists) |
| `map` (1 or 2 lists) | `chez.map_`: applies f to pairs from the end towards the front (7 elements: 7 5 6 3 4 1 2); 3+ lists last to first. Used **everywhere** `map` appears with a procedure that has effects (`tell-all`, `delegate-to-all`, images.ss's `replace-all`, themes.ss's `pick-positive-theme`, `filter`-like helpers); plain list comprehensions only where the function is pure. Chez also inlines `map` over literal lists in an order of its own; Racket used the library order at every site and matched all goldens, so Python does the same |
| `for-each` | a `for` loop; `chez.for_each` where its value (the last application's, `None` for `'()`) is used (the `for*` forms pass it on) |
| `andmap`, `ormap` | first to last, stopping early (battery `andmap-order`, `ormap-order`) |
| `sort` | `chez.sort(pred, ls)`: Chez 10's merge sort (below 25 elements a top-down list merge sort that sorts the second half first, else Shivers's vector merge sort), with Chez's sequence of predicate calls. **Never `sorted()`**: the predicates are `<`/`>` on keys with ties (`sort-by-method 'get-age <`), and stability alone doesn't give Chez's order for non-strict predicates. Sites: utilities.ss `sort-wrt-order`, `sort-by-method`; rules.ss:926, 1292, 1344; workspace.ss:188, themes.ss:508 and others through `sort-by-method` |
| `remq`, `remv`, `remove` | remove **every** occurrence; `remq` by `chez.eq_p` (53 sites) |
| `memq`, `assq` | by `chez.eq_p` |
| `list-index`, `list-tail`, `append` | utilities.ss's own definitions, translated |

### `eq?` and identity

`eq?` (469 sites in the model) becomes:
- `is` when either operand is a model object, a slipnode, a list, `#f`/`#t`, or `'()`
  compared by identity (`null?` is `len(x) == 0`, `not x` only where `x` is known to be a
  list);
- `==` when the operands are symbols (Python `str`, not reliably interned) or fixnums.

`chez.eq_p(a, b)` (`a is b`, or both are `str`, or both are non-bool `int`, and they
are equal; *item 17:* also two equal `Char`s, and any two empty lists, so `'()` is not
compared by identity) is the version for `memq`, `assq`, `remq` and calls where the operand types
vary. Model objects never define `__eq__`, so `==` on them is identity too. Python
lists and tuples compare by content, so a list is never compared with `==` where Scheme
used `eq?`.

### Booleans and truthiness

`#f` → `False`, `#t` → `True`, unspecified value (`void`) → `None`, `'()` → `[]`.

**In Scheme only `#f` is false.** `0`, `'()`, `""` and `0.0` are true. Python's `if x`,
`x or y`, `x and y`, `not x`, `filter(None, ...)` and `any`/`all` treat them as false.
Rules:
- `(if x ...)`, `cond`, `when`, `unless`, `and`, `or`, `not`, `exists?`, `compress`:
  use Python truthiness **only** when `x` is known to be a boolean, or an object or
  `#f`. Model objects are always truthy (they don't define `__bool__` or `__len__`).
  Otherwise write `x is not False` / `x is False`.
- `(or a b)` as a *value* (it returns `a`) → `a if a is not False else b`, unless `a`
  is boolean or object-or-`#f`.
- Numbers, lists and strings are never tested with plain `if`.
- `exists?` is `x is not False`.

Every site where a value could be `0` or `'()` gets a `# chez: #f only` comment. Item
03's utilities tests and the batteries catch most slips. A slip shows up as a codelet
taking a different branch, which the traces locate.

### Symbols, strings and characters

The model uses `symbol?` only to tell a symbol from a *list* (answers.ss:140,
themes.ss:595, trace.ss:83, justify.ss:242, 245), never from a string. It never prints
with `write`/`~s`; the only `~s` is in run.ss's `no-prompt` error and in fonts.ss's
debugging. It never compares a string with `eq?`. So:
- **symbols are Python `str`** (`'bond` → `"bond"`), and so are Scheme strings.
  `string->symbol` and `symbol->string` are the identity. `symbol_p(x)` is
  `isinstance(x, str)`, which is all the model's tests need. *Item 17:* there is no
  public `symbol_p`; justify.py's private `_symbol_p` is `isinstance(x, str)`, and
  sugar.py's excludes `chez.String` and `chez.Char`.
- characters are 1-character `str` (only `symbol->letter-categories` and the string
  utilities use them).
- *Item 02:* where the printer's `write` (`~s`) must tell them apart, a Scheme string is a
  `chez.String` and a character a `chez.Char` (both `str` subclasses). A plain `str` is
  written as a symbol. A vector that must print as `#(...)` is a `chez.Vector` (a `list`
  subclass). `display` prints all of these alike.
- `INVALID = sys.intern("invalid-message-indicator")` is compared with `is`, so every
  producer uses the constant.
- The test canonicaliser (helpers.scm's `b:canon`) prints `'sym` and `"str"`
  differently. Python tests either know which fields are strings or accept both
  spellings for a `str`. That costs nothing, since the model never relies on the
  distinction.

Risk: a future site that does rely on it. Item 03 greps for `symbol?`, `string?`, `eq?`
on string literals and `~s` again and records the result.

### One-armed `if`, `case`, `record-case`

- `(if test then)` (common) → `if test: then`. As a *value* it is `None` when false. The
  `for*` forms and `delegate` pass such values on, so keep `None`.
- `case` with single datums (`(case x (rule-scout ...))`, coderack.ss:474, 523) →
  `if x == "rule-scout"`, or `in (...)` for a list of keys. Chez compares with `eqv?`;
  the keys are symbols and small integers, for which `==` agrees.
- `record-case` on data (trace.ss events) → `if`/`elif` on `ls[0]`, binding the formals
  **as Chez does: with car/cdr, so extra arguments are ignored and too few raise**
  (anomalies: "Chez's record-case ignores extra arguments"). On objects, see "Objects".

### Top-level values created at run time, and `eval`

`chez.py` keeps one table, as racket/compat.rkt does:
`define_top_level_value(name, value)`, `top_level_value(name)`,
`set_top_level_value_bang(name, value)` (binds an unbound name, as Chez does in its interaction environment; Racket raised. Item 02 fixture), `top_level_bound_p(name)`. An unbound `top_level_value` raises `chez.UnboundVariable`.
Users:
- slipnet.ss:371, `establish-link`, defines each of the 202 links as `a-b-link`; the link
  macros then message it through `top_level_value`;
- `slipnet-node-list*` and `codelet-type-list*` (syntactic-sugar.ss:149, 236) define each
  `plato-...` node and codelet type. In Python they are also module attributes
  (`slipnet.plato_a`, `coderack.bottom_up_bond_scout`), made in the same order of
  `make-slipnode`/`make-codelet-type` calls. This is the Racket port's
  `define-slipnet-node-list*`;
- utilities.ss:240, `symbol->letter-categories`, the original's only `eval` (of
  `plato-a` etc.) → `top_level_value("plato-a")`;
- `reveal-obj` (utilities.ss) reads `format-slipnode` (rules.ss) as a top-level value.
  rules.py must register it; Racket item 09 missed this and item 17 fixed it. *(Item 17:
  done; rules.py's `load()` registers it.)*

Names the original never defines but `set!`s or reads (`*temperature-clamped?*`,
`*initial-slipnode-unclamp-time*`, read before any `set!`; `same-direction?`,
`complement-codelet-pattern`, never defined; anomalies) are defined in their natural
module as Racket's `pending.rktl` does: the first two `False`, the last two raising
`UnboundVariable` as Chez would.

### Continuations

- **`continuation-point*`** (15 sites: bonds.ss:90, groups.ss:295, 860, utilities.ss:75,
  86, rules.ss:1267, answers.ss:1283, justify.ss:184, 261, run.ss:100, 114, 148, and the
  codelet wrapper of `define-codelet-procedure*`) only ever escapes upwards, except at
  run.ss:100/114. → `sugar.continuation_point_star(body)` (*item 17:* the name as built): `body` receives an escape
  procedure that raises a private `_Escape(token, value)`. The form catches only its own
  token and returns the value. An escape after the form has returned raises an error,
  as with call/ec. `fizzle` (the escape of the running codelet, a global set by every
  codelet) is `sugar.fizzle`, set through the codelet wrapper.
- **`break`/`quiet-break`/`go`** (run.ss:97–133) are the one re-entrant use: `break` can
  be reached from deep inside a codelet (answers.ss:81, 92 call `suspend`, which calls
  `break`), and `go` resumes the rest of that codelet. Python can't capture that.
  - **headless** (CLI, goldens): as the oracle's run.ss does, `break` is replaced by a
    driver procedure that ends the run by raising `StopRun(reason)`, or returns at once
    with `--keep-going`.
  - **GUI** (item 15): the engine runs in a worker thread, and `break` blocks it on a
    `threading.Event` until `go` (from the control panel) sets it. The code after the
    break point then continues, as the continuation would. `*interrupt?*` and the step
    mode are flags the GUI thread sets. Views are called on the engine thread and must
    only queue drawing for Tk's thread (`after` polling), so they can't change the run.
    *Item 17:* as built (item 15), no `after` polling: `swl.ThreadSafeTk` hands each Tcl
    call to Tk's thread through a queue and a pipe that Tk's event loop watches, and the
    engine thread waits for the result.
  - `reset` (Chez's REPL abort) → `raise Reset()`; `report-error-and-halt` reaches it.
- **Python recursion depth.** The model recurses over short lists, and `remq`-style
  helpers are written as loops. The engine sets `sys.setrecursionlimit(10000)` as a
  margin. *(Item 17: only the CLI's `main` does, in `metacat/__main__.py`; the goldens,
  the batteries and the GUI run within Python's default limit.)* The known infinite recursion (an object without `object-type` sent a bad
  message: report-error-and-halt recurses forever, in the original too) shows up as a
  `RecursionError`. The prototype test hit it while it was being written.

### Printing

`chez.display`, `chez.write`, `chez.format_` (`~a ~s ~% ~n ~~`, either case; others raise),
`chez.printf` and `newline` as in compat.rkt §4: `display` abbreviates `(quote x)` as
`'x` and `write` doesn't; symbols are written with Chez's `\xHH;` escapes; characters
and strings are written with Chez's names and escapes; `#<void>`; Python lists print as
Scheme lists, `True`/`False` as `#t`/`#f`, `None` as `#<void>` (item 02 correction;
fixture `format-void`). Symbols with non-ASCII characters outside R6RS's constituent
categories are written with `\xHH;` (item 02). Never `str()`/`repr()` a number where Chez prints it. The trace writer
(trace-format.md) has its own JSON rules: exact rationals as `"n/d"` strings, flonums
through `number_to_string`.

`printf` writes to the *current* `sys.stdout` at call time (syntactic-sugar.ss captured
the port at load time; the Racket port does the same as Python here).

## Objects

### The original

`(lambda msg (let ((self (1st msg))) (record-case (rest msg) clause ... (else
(delegate msg parent ...)))))`: 72 `record-case` forms (52 in the model; a few dispatch
on data, not messages), 4,243
`tell` sites (3,161 in the model), 60 `delegate` uses. `(tell obj 'm a)` applies `obj`
to `(obj m a)` and halts on `'invalid-message-indicator`. `delegate` passes the same
message, **self included**, to each parent in turn. So a parent's code that does
`(tell self ...)` talks to the child, and the parent's closure variables are its own.
Parents are separate objects: a bond's `workspace-structure`, a group's
`workspace-object` and `workspace-structure` (two parents, in that order), graphics
windows delegating to a `graphics-window`, fonts to a `font`. Several objects end with
`(delegate msg base-object)`, the root that knows only `object-type`. An object without
an `else` clause returns void for an unknown message, which `tell` does **not** treat as
an error. Objects are procedures, and `procedure?` tells them from other values
(`print`, `say-object`, `slipnode?`). The trace instrumentation replaces `*coderack*`
and `*workspace*` with forwarders `(lambda msg (apply original original (cdr msg)))`.

### How often

`python/oracle/count-calls.ss` runs the unedited oracle with `tell` and `delegate`
wrapped:

| Run | Codelets | `tell` | per codelet | `delegate` (falls through) |
|---|---:|---:|---:|---:|
| abc abd xyz, seed 3852097033 | 2,170 | 2,242,151 | 1,033 | 275,578 (12%) |
| abc abd mrrjjj, seed 1 | 1,250 | 1,353,867 | 1,083 | 199,956 (15%) |
| eqe qeq abbba, seed 2 | 988 | 830,331 | 840 | 108,192 (13%) |
| abc abd iijjkk, seed 3 | 4,204 | 4,960,867 | 1,180 | 867,794 (17%) |

About **1,000 messages per codelet**, one in six going through `delegate`.

### Candidates and the micro-benchmark

`python3 python/tests/object_prototypes.py` (Python 3.12.13, this machine; ns per
operation, minimum of 5 repeats). The test object has 40 messages and delegates to a
parent with 15, which delegates to `base-object` (bond → workspace-structure →
base-object):

| Candidate | 1st message | 20th of 40 | 40th of 40 | delegated (parent's 8th) | delegated twice (`object-type` at the root) | create child + parent |
|---|---:|---:|---:|---:|---:|---:|
| A closure + `if`/`elif` chain (literal record-case) | 149 | 336 | 507 | 709 | 756 | 190 |
| B closure + dict of inner closures | 227 | 228 | 229 | 386 | 449 | 4,389 |
| C class per object, dict of messages, instance callable as `(self, msg, *args)` | 266 | 263 | 264 | 474 | 581 | 145 |
| C2 = C, `tell` looks up the dict itself | 157 | 157 | 158 | 644 | 742 | 145 |
| **C3 = C, every object a `SchemeObject`; `tell` and `delegate` look up the dict** | **173** | **172** | **173** | **297** | **396** | **146** |
| D Python inheritance, `tell` = `getattr` by mangled name | 147 | 146 | 148 | 149 | 141 | 110 |
| (a plain method call `obj.get_m20()`) | | 28 | | | | |

- **A** is the most literal, but its cost grows with the message's position: the
  largest record-cases have 92 clauses (workspace-objects.ss), 91 (workspace-strings.ss)
  and 72 (bridges.ss).
- **B** makes one closure per message for every object: 30× C's creation cost, and
  the model creates many short-lived objects (descriptions, proposed structures,
  images).
- **D** is the fastest, but it merges a parent's state into the child. It can't
  express delegation to a separate, existing object (windows, fonts, the battery's
  `delegate` test), and child and parent variables with the same name collide
  (`string` in both a bond and its workspace-structure). It changes the structure the
  plan wants to keep side by side.
- **C3 keeps the original's protocol exactly and costs about 0.2 µs per message**: at
  1,000 messages per codelet that is about 0.2 ms per codelet. A would cost about twice
  that.

All of A, B, C and C3 pass the same Chez-fixture tests (`tell`, `tell-args`,
`tell-alias`, `tell-invalid`, `base-object`, `delegate`, `delegate-to-all`,
`delegate-to-all-order`, `delegate-to-all-invalid`, `tell-all-order`,
`record-case-no-else`) and the self-through-delegation and forwarder checks.
Mutations caught: a left-to-right `tell-all` (4 failures), `delegate` passing the
parent instead of `self` (C and C3, 1 each), `tell` not halting (7), a missing `else`
returning invalid (1).

### Decision: C3

```python
class Bond(SchemeObject):
    """bonds.ss: make-bond (the closure's variables are attributes of `this`)."""
    __slots__ = ("workspace_structure", "from_object", "to_object", "bond_category", ...)

    def __init__(this, from_object, to_object, bond_category, bond_facet,
                 from_object_descriptor, to_object_descriptor):
        this.workspace_structure = make_workspace_structure()   # the let*, in order
        ...

    @message("object-type")
    def object_type(this, self):
        return "bond"

    @message("get-string")
    def get_string(this, self):
        return this.string

    def otherwise(this, self, msg, args):                      # (else (delegate msg ...))
        return delegate(self, msg, args, this.workspace_structure)


def make_bond(from_object, to_object, bond_category, bond_facet,
              from_object_descriptor, to_object_descriptor):
    """bonds.ss: make-bond"""
    return Bond(from_object, to_object, bond_category, bond_facet,
                from_object_descriptor, to_object_descriptor)
```

- `objects.py` (item 03): `SchemeObject` (`__slots__`, `MESSAGES` built by
  `__init_subclass__` from `@message(name, ...)` methods, `__call__(this, self, msg,
  *args)` for the protocol, `otherwise` returning `None` like a record-case without
  `else`), `message`, `tell`, `delegate`, `delegate_to_all`, `tell_all`, `BASE_OBJECT`,
  `Forwarder`, `INVALID`, `procedure_p` (`callable`). `tell(obj, msg, *args)` stays a
  plain function, so it can be passed to `map` and `sort-by-method`.
- `this` is the object whose closure it is; `self` is the receiver from the message.
  The method signature `(this, self, *formals)` keeps both visible. A `set!` of a
  closure variable is `this.x = ...`.
- **Messages keep their Scheme names as strings** (`tell(bond, "get-string")`), so the
  4,000 call sites read like the original, `sort-by-method` and `tell-all` take message
  names as data, and `Ooops: bad message "..."` and the trace's `halt` event print them
  unchanged. The Python method names follow the name mapping.
- `record-case` clauses with several keys (`((alias1 alias2) () ...)`) list all the
  names in `@message`. Rest formals `(a . more)` become `*more` (a tuple: convert with
  `list()` where the list escapes).
- **Arity.** Chez's record-case ignores extra arguments and raises on missing ones;
  Python raises on both. The one known extra-argument site (trace.ss:442, 473–478,
  `draw-string-letters` with a tag, graphics only) gets an `*_ignored` parameter and a
  `# chez:` comment. Item 14 checks for others with the panels battery.
- Construction follows the closure's `let`/`let*` order exactly. Some `make-...` bodies
  create parents or draw before the `lambda`.
- Objects with nested `record-case` (coderack.ss:267) dispatch in a method body.

Item 12 may speed up dispatch further (local aliases of `tell` in hot loops, caching the
parent's table), keeping every trace identical. *(Item 17: item 12 did neither; it
inlined `tell` in `tell_all` and sped up chez.py and utilities.py instead; see "As built
(item 12)".)*

### As built (item 03)

`python/metacat/objects.py` is C3, with these details fixed by the utilities battery:
- `delegate(self, msg, args, *parents)` and `delegate_to_all(self, msg, args, *objects)`
  take the message split into its parts, as they are called from `otherwise(this, self,
  msg, args)`. `delegate` passes `self` on; `delegate_to_all` gives each object itself
  as self and runs in `chez.map_` order. A parent whose record-case has no `else`
  answers void (`None`), which `delegate` returns as an answer, as in Chez.
- `Lambda(fn)` wraps a plain `(lambda msg ...)` object, `fn(self, msg, *args)`, for
  stand-ins and objects that dispatch by hand. `Forwarder`, `base_object` (also
  `BASE_OBJECT`), `procedure_p` (`callable`), `Reset` and `INVALID` are as planned.
- `MESSAGES` also collects `@message` methods from SchemeObject base classes, so a Python
  subclass may share clauses. Delegation to a separate object is still `delegate`.
- `tell` calls `report_error_and_halt` through the module global, so a run's driver
  replaces `objects.report_error_and_halt` (run.ss's `set!`). utilities.py re-exports the
  object procedures, but replacing them there would not reach `tell`.
- Measured (this machine): `tell` 135 ns, delegated once 247 ns, twice 367 ns, creating
  a child and its parent 111 ns.

`python/metacat/sugar.py` makes every extend-syntax form a function:
- bodies the macro would delay are thunks (`stochastic_if_star(prob_thunk, exps_thunk)`
  draws the coin, then calls `prob_thunk`; `if_star`; `repeat_star_*`;
  `continuation_point_star(body)` passes the escape to `body`);
- a form with several patterns is one function per pattern: `for_star(f, *lists)` and
  `for_star_from_to(lo, hi, f)` (the caller evaluates `lo` then `hi`),
  `repeat_star_times`/`_forever`/`_until`, `category_links_star(instances, c, len)` and
  `instance_links_star(c, instances, len)` for the `all-lengths:` forms, keyword
  arguments `length=`, `label=` and `two_way=` for `lateral-link*` and
  `lateral-sliplink*`;
- `mcat`'s validity test is a fender in the original, so bad tokens raise
  `chez.SchemeError` (a syntax error) without telling the control panel;
- the names a macro leaves free are read at call time from the engine module that
  defines them: `metacat.setup.p_verbose` and `.g_control_panel`,
  `metacat.coderack.g_coderack` and `.make_codelet_type`,
  `metacat.slipnet.make_slipnode` and `.establish_link`. Those modules don't exist yet;
  tests provide them with `engine_module(...)` in test_utilities.py, which patches the real
  module once it exists (*item 17:* they all exist now; `engine_module` moved to
  tests/engine_stubs.py);
- `slipnet_node_list_star(specs, module=None)` and `codelet_type_list_star(specs,
  module=None)` define top-level values, and also module attributes (through
  `names.scheme_to_python`) when given the module;
- `define_codelet_procedure_star(name, proc)` looks the codelet type up as a top-level
  value and sets `sugar.fizzle` while the codelet runs. Codelet code calls
  `sugar.fizzle()`, always qualified;
- in model code, `for*`, `if*` and `stochastic-if*` are usually written inline
  (`for x in l:`; `coin = chez.random(1.0)` then `if coin < p:`). The functions are for
  sites that use the form's value.

`python/metacat/utilities.py` has one function per definition, under the mapped name
(`1st` → `first`, `~` → `rough`, `filter` → `filter_` ...); a test checks that every
`define` of utilities.ss and syntactic-sugar.ss has its Python name. Vectors and tables
are `chez.Vector`. `coord` is `chez.make_rectangular`, which needs `chez.ExactComplex` for
exact coordinates (anomalies entry); coordinate arithmetic is left to the graphics
items. `(ascending-index-list 0)` loops forever, as in the original and the Racket port.

**Strings vs symbols, re-checked (item 03).** The graphics (`string?` in
sgl-interpreter.ss, general-graphics.ss, fonts.ss, gui.ss) and rules.ss:269
(`filter-out symbol?`) *do* tell strings from symbols. The items that translate those
files must keep the distinction there (anomalies: "The graphics and rules.ss tell strings
from symbols").

### As built (item 04)

- **`engine.py`** has metacat.ss's load order (`LOAD_ORDER`, Python module names).
  `load()` imports the modules translated so far and calls their `load()`, once per
  process. `set_global(name, value)` / `get_global(name)` map the Scheme name through
  `names.scheme_to_python` to the first module of the load order (plus `view_globals`)
  that defines it, and raise `chez.SchemeError` otherwise.
- **References to files not translated yet** go through the package at call time:
  `import metacat as _metacat`, then `_metacat.workspace.g_workspace`,
  `_metacat.run.g_display_mode_p`, `_metacat.groups.contains_p`. Once such a module exists
  this still works (engine.py imports it); later items may switch a module to
  `from metacat import workspace` when the target exists. Tests provide the globals of
  missing modules with `tests/engine_stubs.engine_module`.
- **`view_globals.py`** mirrors racket/engine/view-globals.rktl: the colours and fonts the
  model reads (`urgency-color`, the codelet types' graphics methods, trace events), the
  speed settings and `restore-current-state`, all `#f` (or raising) until the views set
  them. constants.py keeps only the model's part of constants.ss.
- **setup.py**: `setup` and `enable-resizing` are the GUI's (item 15; *item 17:* in
  gui/app.py). Other modules read
  and assign the globals qualified (`setup.g_temperature`).
- **coderack.py**: the codelet closure that make-codelet makes is the `Codelet` class. It
  shares variables with its codelet type's closure (count, selection probability,
  procedure, window), so it keeps that object as `owner` and the message's receiver as
  `codelet_type`. Codelet lists are rebuilt on every cons and `remq`, never mutated, so a
  list handed out stays as it was. `case` without `else` returns `None` (void), as the
  battery's `post-codelet-probability` rows show. `load()` makes `*codelet-types*` with
  `codelet_type_list_star(..., module=coderack)`, whose labels are `chez.String`s (the
  graphics tell strings from symbols), then the three type lists and `*coderack*`.
- **descriptions.py**: `load()` runs the four `define-codelet-procedure*` forms.
- **Arithmetic on values that may be `#f`** goes through `chez.add`/`sub`/`mul`, since
  Python's `bool` is an `int` (anomalies entry).

### As built (item 05)

- **slipnet.py**: make-slipnode's closure is `Slipnode`, make-slipnet-link's `SlipnetLink`.
  Link lists are rebuilt on every cons, never mutated. `load()` runs the file's top-level
  forms in order: `*slipnet-nodes*` through `slipnet_node_list_star(..., module=slipnet)`
  (each node a module attribute `slipnet.plato_a` and a top-level value), the four node
  lists, top-down codelet types, intrinsic link lengths, descriptor predicates, then the
  202 links through the sugar link functions (each link only a top-level value, `a-b-link`).
  `%update-cycle-length%` (run.ss), `monitor-slipnode-activation-change` (trace.ss),
  `temp-adjusted-probability` (formulas.ss), `*themespace*` and `*workspace*` are read
  through the package at call time, so `engine.set_global` can replace them.
- **images.py**: `Image` and `StringImage`. `fail` is any procedure that does not return
  (it raises in the tests; rules.py will pass its escape; *item 17:* it does, from
  `continuation_point_star` in apply-rule). `replace-all` and every
  `tell-all` keep `chez.map_`'s order, since `fail` can escape midway and leave the images
  map already reached changed (pinned by `slipnet-extra`'s `image-replace-all-fail`).
  `make-letter`, `make-group` and `make-group-pexp` are read through the package.
- **Arithmetic**: activations stay exact; `reset`, `decay-activation`, `spread-activation` and
  `flush-activation-buffer` still go through `chez.expt`/`mul`/`div`/`min_`/`add`, so a flonum
  from a theme would be handled as in Chez.
- **Test isolation**: test files share one engine; files whose cases change engine state or
  the top level restore it afterwards (anomalies entry).

### As built (item 06)

- **Modules**: workspace.py (`Workspace`; `load()` makes `*workspace*`), workspace_objects.py
  (`Letter` delegating to `WorkspaceObject`, which groups will share), workspace_strings.py
  (`WorkspaceString`), workspace_structures.py (`WorkspaceStructure`), and the plain
  functions of workspace_structure_formulas.py and formulas.py. They import each other
  qualified at the top (cycles are fine: attributes are read at call time).
- **Names from files not translated yet** are read through the package only where the
  original evaluates them: `_metacat.bonds.same_bond_category_p`, `_metacat.groups`,
  `_metacat.bridges.bridge_between_p`/`break_bridge`, `_metacat.rules.verbatim_clause_p`/
  `rule_describable_bridge_p` (inside a lambda, so that filtering an empty bridge list never
  touches the module), `_metacat.trace`, `_metacat.group_graphics.group_graphics("erase",
  s)`, `_metacat.eeg_graphics.g_EEG` and `_metacat.run.g_temperature_clamped_p` (run.ss
  creates `*temperature-clamped?*` with `set!`; run.py must define it; *item 17:* it
  does).
- **Tables** are `chez.Vector`s of rows. `flatten` doesn't descend into a `Vector`, so
  `get-proposed-bridges` turns rows into lists first, as `vector->list` does.
- **Scheme strings**: `print-name`, `ascii-name` and `generic-name` return `chez.String`;
  `symbol-name` a plain str. descriptions.py's `print-name` now returns a `String` too (it
  returned format's plain str, which b:canon took for a symbol).
- **Exactness**: unhappiness, salience and importance stay exact (`102/5`-style averages are
  rounded by `utilities.round_`). Flonums enter only through `temp-adjusted-*`,
  `(min 1.0 ...)` in `get-activity`, `tanh` in mapping strengths and `get-weakness`'s
  `expt`. Thresholds compare the exact value with the flonum (anomalies entry "Exact bond
  densities meet flonum thresholds").
- Measured (this machine): building the initial workspace of `abc abd xyz` 1.6 ms,
  `update-workspace-values` 1.1 ms, the Workspace's `choose-object` 24 µs.

### As built (item 07)

- **Modules**: bonds.py (`Bond`, the four bond codelets, `build-bond`/`break-bond` and the
  bond predicates), groups.py (`Group` delegating to a `WorkspaceObject` and a
  `WorkspaceStructure`, as letters do; the five group codelets; `contains?`), and
  concept_mappings.py (`ConceptMapping`, `CMs-equal?`, `remove-duplicate-CMs`). Each `load()`
  installs its codelet procedures in the file's order.
- **group_graphics.py** holds only `group-graphics`, which the model calls ungated when
  group-builder consolidates sameness groups (it sends `caching-on`, `flush` and maybe
  `erase-group`/`draw-group` to `*workspace-window*`). The rest of group-graphics.ss
  (`make-group-pexp`, `draw-group-grope`, the arrowhead constants) is the panels item's;
  groups.py and images.py reach those names through `_metacat.group_graphics` only with
  `%workspace-graphics%` on. *(Item 17: item 14 completed group_graphics.py in the
  engine: `make-group-pexp`, `draw-group-grope` and the arrowheads are there now.)*
- **Names from files not translated yet**, read at call time: `_metacat.bridges.break_bridge`,
  `incompatible_horizontal_CMs_p`/`incompatible_vertical_CMs_p` (only when bridges exist),
  `_metacat.trace.monitor_new_groups`, `_metacat.general_graphics` (graphics on).
- **Evaluation order**: groups.ss's `get-local-density` draws the right neighbours before the
  left ones (Chez evaluates `append`'s second argument first); bonds.ss's is a `let*`, left
  first. group-builder's bond maps (`adjacency-map` and the flipped-bond `map`) use Chez's map
  order, and its fights stop at the first loss (`andmap`). Every `stochastic-if*` draws its
  coin first; in top-down-group-scout:category the probability itself draws afterwards.
- **The harness**: python/tests/codelet_harness.py translates tests/diff/codelet-harness.scm
  (bonds-and-groups setting only; `b:bridges?`/`b:rules?` paths wait for items 08–09;
  *item 17:* both added there). The
  59 cases run in a fork pool (about 6 s on 32 cores); about 1.5 ms per codelet,
  harness dumps included.

### As built (item 08)

- **Modules**: bridges.py (`HorizontalBridge` and `VerticalBridge`, the two closures of
  make-horizontal-bridge and make-vertical-bridge; the clauses they share word for word sit
  once in a private `_BridgeClauses` base, and both delegate the rest to a
  `WorkspaceStructure`; the four bridge codelets; `propose-bridge`, `build-bridge`,
  `break-bridge` and the predicates) and breakers.py (the breaker). Each `load()` installs
  its codelet procedures.
- **Names from files not translated yet**, read at call time: themes.ss's
  `bridge-type->theme-type` (every bridge made), `bridge-theme-compatibility-sigmoid` (every
  strength update), `descriptions-affect-themespace?` and `*themespace*` (every bridge
  built), plus `check-descriptions` & co. (only with an active theme); trace.ss's
  `monitor-new-concept-mappings` and `entries`; justify.ss's
  `remove-whole/single-concept-mappings`; bridge-graphics.ss (graphics on only). The test
  harness supplies the three ungated themes.ss helpers as stand-ins until item 10's
  themes.py (anomalies: "Bridges call themes.ss on every bridge"). *(Item 17: removed
  by item 10.)*
- **Evaluation order**: no site needed reordering. Every draw (the stochastic-picks of
  bridge type and objects, `stochastic-if*`, `random-pick`, the fights) sits in a `let*`,
  a body or an `and`/`cond`; the multi-argument calls and multi-binding `let`s (the appends
  of the incompatible-bridge lists, `wins-all-fights?`'s arguments, `make-concept-mapping`'s)
  only read. Maps use `chez.map_`; `cross-product-for-each` (boost-themes) and
  `all-possible-bridge-CMs` keep utilities.ss's order.
- **Speed**: the 530 runs of 1000 codelets take 217 s of CPU, about 0.4 ms per codelet,
  14 s on 32 cores.

### As built (item 09)

- **Modules**: rules.py (`Rule`, the closure of make-rule, delegating to a
  `WorkspaceStructure`; `ExtrinsicChangeDescription` and `IntrinsicChangeDescription`; the
  three rule codelets; abstraction, application, quality and the English transcription)
  and answers.py (answer-finder, `report-new-answer`, snags, the slippage log as a
  `SlippageLog` class, `translate`, and the answer-description and comparison phrases that
  justify.ss and memory.ss use). Each `load()` installs its codelet procedures. rules.py's
  also makes `*rule-dimension-order*` and registers `format-slipnode` as a top-level value.
- **Names from files not translated yet**, read through the package at call time:
  run.ss (`update-everything`, `suspend`, `post-initial-codelets`,
  `*temperature-clamped?*`, which process-snag sets); trace.ss (`*trace*`,
  `make-answer-event`, `make-snag-event`, `monitor-new-rules`,
  `equivalent-workspace-objects?`, `entries`); memory.ss (`*memory*`,
  `abstract-answer/snag-description`); themes.ss (`*themespace*`, `diff`); justify.ss;
  general-graphics.ss's `find-next-space-position` (every make-rule); and the graphics,
  gated. test_rules.py supplies rule-battery.scm's fakes, plus verbatim copies of
  `find-next-space-position`, `equivalent-workspace-objects?` and `diff`.
- **Strings**: every English phrase, commentary line and format result is a
  `chez.String`, so the battery's `(comment add-comment ("..." ...))` events and
  get-concept-pattern's `(filter-out symbol? ...)` see strings, not symbols.
- **Evaluation order**: no site needed reordering. answers.ss's `apply-to-change` and
  `apply-to-object-description` call `apply-slippages` two or three times inside a
  `(list ...)`; those calls can draw (coattail `prob?`) and log. A Chez probe (a `list` of
  logged calls inside a lambda, under `scheme --script`) evaluates them left to right, and
  the Python does the same. The battery can't tell them apart, because only the descriptor
  position ever draws. Every `stochastic-if*` draws its coin first; maps with effects use
  `chez.map_`; translate-rule-clause's `prob? 0.4` filter goes first to last.
- **Crash path**: `get-change-phrase`'s `(3rd #f)` raises `chez.SchemeError("caddr", ...)`
  (anomalies: "`caddr` of `#f` in `transcribe-to-english`").
- **Speed**: the rule battery (36 problems, every seed, up to 2500 codelets or the first
  answer, plus twelve matrices) takes about 9 min of CPU, 38 s on 32 cores.

### As built (item 10)

- **Modules**: themes.py (`Themespace`, theme clusters and bridge themes as SchemeObject
  classes; thematic-bridge-scout; the REPL abbreviations `top`, `bot`, `ver`, `diff` = #f
  as constants and `lcat`, `iden` … set by `load()`), justify.py (answer-justifier,
  `compare-rule-clause-lists`, `traverse-rule-clauses`, the theme pattern to clamp),
  trace.py (the Temporal Trace and its events, the monitors with their importance
  thresholds, the codelet patterns made by `load()`), jootsing.py (jootser and
  progress-watcher) and memory.py (the Memory, answer and snag descriptions). Each `load()`
  makes its file's object (`*themespace*`, `*trace*`, `*memory*`) and installs its codelet
  procedures, so a loaded engine has every model object, as the original's load does.
- **trace.py is trace.ss.** Item 11's iterations.md text and TASK.md's layout also call the
  golden-trace writer `trace.py`; the module-per-file rule gives that name to trace.ss, so
  item 11 must put the writer under another name (for example `metacat/tracing.py` or
  inside `headless.py`). *(Item 17: it is `metacat/trace_writer.py`.)*
- **Early partial graphics modules**, as group_graphics.py: general_graphics.py
  (`find-next-space-position`, every rule's English), trace_graphics.py
  (`group-event-pexp-text-string`, every group event's name, which is in the trace) and
  theme_graphics.py (`relation-name`, trace.ss's `print-pattern`). Pure, verbatim; the
  panels item adds the rest of each file. *(Item 17: done differently: the views' parts
  went to `metacat/gui/`'s modules of the same names, and the engine's theme_graphics.py
  and trace_graphics.py still hold one function each.)*
- **Stand-ins removed**: the themes.ss helpers in codelet_harness.py's `STAND_INS`, and
  test_rules.py's copies of `equivalent-workspace-objects?`, `find-next-space-position` and
  `diff`. The batteries' own fakes (fake Themespace, Trace, Memory, monitors) stay; they
  patch the real modules through `engine_module` and restore them.
- **Evaluation order**: no site needed reordering (as in the Racket port, whose five files
  have no `port:` changes). Every `stochastic-if*` draws its coin first (themes'
  spread-activation-to-slipnet, the jootser's and progress-watcher's tests). Maps with
  effects use `chez.map_` (`tell-all clusters 'pick-positive-theme` draws once per cluster
  in Chez's map order; a mutant mapping first to last changes one golden).
  justify.ss's `traverse-rule-clauses` walks the rests of two lists before their firsts
  (the inner walk is an argument of the outer one), so it fails on a length mismatch
  before visiting anything and visits elements last to first; it is a length check and a
  reverse loop.
- **The golden harness** (python/tests/golden_harness.py, golden_run.py; *item 17:*
  superseded by item 11, which moved the loop to `metacat/run.py`, the windows and driver
  to `metacat/headless.py` and the writer to `metacat/trace_writer.py`, and deleted
  golden_run.py): the oracle's
  headless windows (prelude.ss), with the Racket port's headless Commentary window; the
  trace.ss writer and wrappers, installed by setting module attributes
  (`coderack.g_coderack`, `bonds.build_bond`, `formulas.update_temperature`,
  `memory.abstract_answer_description`, `objects.report_error_and_halt` …: the engine
  reads them at call time, so intra-module calls see the wrappers too; nothing imports
  them with `from ... import`); run.ss's driver around golden_run.py, run.ss's loop
  translated in the tests and registered as `metacat.run`. Each golden runs in a fresh
  fork (`maxtasksperchild=1`) of a fresh Python process that has loaded the engine once.
- **Speed**: the 109 goldens (272,957 codelet events; *item 17:* their final counts sum
  to 272,857, because the 99 runs that suspend and the halt run stop inside a codelet, before the count
  goes up) take about 6 min of CPU, 35 s on 32 cores:
  about 1.4 ms per codelet, all updates included. The longest golden (17,000 codelets) sets
  the wall time.

### As built (item 11)

- **Modules**: run.py (run.ss, all 33 definitions), headless.py (the oracle's prelude.ss
  headless windows and run.ss driver, promoted from the tests), trace_writer.py (the
  oracle's trace.ss JSON writer and wrappers; trace.py is trace.ss, so the writer has
  another name than iterations.md's `metacat/trace.py`) and `__main__.py` (the CLI).
  The test-side golden_run.py is gone; golden_harness.py only reads problems.txt and runs
  forks.
- **break/go**: `run.toplevel(thunk)` is the REPL evaluating one command. It runs the
  thunk in a daemon "engine" thread and waits on a queue. `break_`'s `(reset)` raises
  `objects.Reset`, which break's own continuation point catches: in an engine thread it
  posts "reset" to the waiting command and parks on an Event; `go` calls the
  `Breakpoint` (break's continuation), which re-targets the parked thread's queue to
  itself, releases the Event and waits, so break returns `'ignore` and the run goes on
  from inside the codelet. Only one thread runs model code at a time, so the run is the
  same run (test_run.py). Outside `toplevel` (headless drivers, scripts) Reset propagates.
  `init-mcat`/`clear-breakpoint` unwind a parked thread they drop (`_Abandoned`, a
  BaseException no model code catches). A Breakpoint resumes once: Chez could re-enter
  a continuation, which no run does. Item 15 can call `go` from a GUI worker rather
  than Tk's thread (*item 17:* it does: `app.EngineThread`).
- **Trace wrappers when there is no trace**: they compute nothing when `trace_writer.PORT`
  is None; they only read, so the run is the same either way (the CLI's output with and
  without `--trace` is compared).
- **Error output**: an error of the original (the `caddr` crash) prints the oracle's first
  line, `Error: Exception in caddr: incorrect list structure #f`, then a Python traceback,
  and exits 1.
- **Speed**: 1.23 ms per codelet over the goldens' 272,857 codelets, 9× Chez
  (docs/python-run-times.md); startup 0.05 s.

### As built (item 12)

- **Extra seeds**: the oracle's side of the 720 runs is frozen as hashes and stdout
  (`python/fixtures/extra-seeds/`, by `python/oracle/capture_extra_seeds.py`), not as
  traces, and the gate re-captures it to check freshness. The Python side runs through
  `golden_harness.run_in_fresh_process(..., digest=True)`, which reduces each run to what
  the CLI would give: exit code, stdout, the error line and the trace's sha256.
- **Speed-ups** keep the structure of the Scheme: they are faster versions of the Chez
  primitives (chez.py's arithmetic, `memq`, `remq`, `andmap`, `ormap`) and of a few
  utilities (`weighted-index`, `tell-all`, `sort-by-method`), plus the trace writer's
  wrappers. None reorders a draw or skips a `tell` that has an effect. Two depend on
  facts of the model, stated where they are made: `memq`'s identity path relies on
  `eq?` being identity for everything but symbols, fixnums, characters and `'()`
  (`eq_p`), and `sort-by-method`'s key cache on the model's sort keys being getters
  (`get-left-string-pos`, `get-age`, `get-quality`, `get-absolute-activation`). A new
  `sort-by-method` with a key that has effects would have to drop the cache.
  docs/python-run-times.md has each one's gain.

### As built (item 13)

- **Package**: `metacat/gui/` holds the views. `sgl.py` (sgl-interpreter.ss),
  `fonts.py` (fonts.ss), `colors.py` (constants.ss's `swl-color`, `*color-names*` and
  `=white=` … `=orange=`) and `swl.py` (SWL's `swl:tcl-eval`, `swl:tcl->scheme`,
  `swl:sync-display`, and `TkCanvas`). None imports tkinter at module level, so they
  import and the stream tests run without a display. The engine never imports
  `metacat.gui` (test_sgl.py walks every engine module's imports).
- **The viewport sends Tcl, as the original did.** Unlike the Racket port, which had no
  Tk and emulated the canvas on racket/draw, `Viewport` keeps every `<viewport>` method
  with its `tcl-eval` calls, argument for argument. A window is any object with
  `tcl(*args)`. `swl.TkCanvas` wraps a tkinter Canvas and turns each argument into a Tcl
  word (`tcl_word`: an `Rgb` becomes `#rrggbb`, an `SwlFont` its font description, a
  `Fraction` a float, a list a Tcl list), then calls the widget command. So the items,
  tags, dashes (`"- "`, `". "`), anchors and fonts are Tk's own, and move, raise,
  delete, retag, unhide and scale are Tk's commands. `tcl_eval` is `swl_tcl_eval`,
  because Tk 8.5 is at least 8.3. `remove_unsupported_tcl_args` is translated but unused,
  as in the original.
- **Fonts**: text is measured as fonts.ss measures it: a text item is created on
  `*hidden-canvas*`, Tk is asked for its `bbox`, and then `delete all`.
  `create_mcat_logo(root)` makes the logo window and that hidden canvas. `SwlFont`'s
  `get_actual_values` reports the request in points at 96 dpi, as the Racket port does
  (Tk's `font actual` would make the < 7-point test depend on the machine). `serif`,
  `sans_serif`, `fancy` and `sgl.init_env` are made by `fonts.load()` and `sgl.load()`
  when the views start, since asking Tk for its families needs a display.
  `fonts.load(families)` fixes the family list (the tests use the oracle prelude's).
- **SGL data**: chez.py's representation, read from Scheme text in the tests by
  `tests/scheme_reader.py`. Strings are `chez.String` wherever the original tests
  `string?` (anomalies: "The graphics and rules.ss tell strings from symbols").
- **`mouse-press`** takes the event's modifiers as a set of symbols (`left-button`,
  `right-button`, `shift`, …) and matches them exactly, which is how this port reads
  SWL's `(event-case ((modifier= mods)) ...)`.
- **Tests** (`tests/test_sgl.py`): the 48 tests of tests/diff/sgl-battery.scm; the Tcl
  stream of `python/oracle/sgl-fixture.scm` (racket/tests/sgl-fixture.rkt's pictures plus
  offscreen extras) on two viewports (1:1 and 2:1), against
  `python/fixtures/sgl-tcl/`, captured from the oracle's `swl:tcl-eval` by
  `python/oracle/capture_sgl_tcl.py`; and, under Xvfb, `tests/render_sgl_fixture.py`,
  which draws the fixture on a real Canvas, grabs the window with XGetImage and checks
  16 pixels. `tests/snapshots/sgl-fixture.png` is that rendering, for comparison with
  racket/tests/snapshots/sgl-fixture.png. The two differ only in text metrics.

### As built (item 14)

- **Split between engine and views**, as in the Racket port (porting-notes.md, items 13
  and 14). The engine (`metacat/*.py`, no tkinter, no `metacat.gui`) has what the model
  calls whether or not a window exists: `general_graphics.py` (the pexp builders and text
  helpers, `*platform*`, `*tcl/tk-version-8_3?*`), `group_graphics.py`,
  `bridge_graphics.py`, `rule_graphics.py`, `theme_graphics.py` (`relation-name`),
  `trace_graphics.py` (`group-event-pexp-text-string`) and `eeg_graphics.py`
  (`%EEG-table%`, `make-EEG`, `*EEG*`; headless.py's null `*EEG*` is gone, so headless
  runs use the engine's EEG, as the oracle does). The views (`metacat/gui/`) have the
  windows: `constants.py` (constants.ss's graphics part), `general_graphics.py`
  (`make-graphics-window` as `GraphicsWindow`, the scrollable text window, the resize
  listener on a thread and a `queue.Queue`), and `slipnet_`, `workspace_`,
  `temperature_`, `coderack_`, `theme_`, `trace_`, `memory_`, `commentary_` and
  `eeg_graphics.py`. Each has a `load()`; `views.load_views()` calls them in
  metacat.ss's order, after `fonts.load()` and `sgl.load()`.
- **Hooks**: the original's. The model reaches windows only through `setup.g_*_window`,
  the switches (`setup.p_workspace_graphics` ...) and `view_globals.py`. A view's
  top-level define of a name the model reads (colours, the Workspace and Coderack fonts,
  `restore-current-state`, `*fg-color*`) is installed by its `load()` with
  `engine.set_global`. `views.attach_views(scale)` is racket/gui/views.rkt's
  `attach-views!`: every window as `(setup)` makes it, every switch on, full speed
  without flashing. `headless.run_problem(..., views=attach_views)` attaches them before
  the run, then wraps the views' Commentary and Trace windows with the recorders
  (`install_recorders`, racket/headless.rkt's `install-recorders!`; the Trace wrapper is
  `trace_writer.wrap_trace_window`).
- **Window hosts** (`gui/hosts.py`): SWL's `<toplevel>` and frame are one host, which
  gives the viewport its canvas. `OffscreenHost` (the default) draws nothing and counts
  items; text is measured by `OffscreenHiddenCanvas`, the sgl-tcl.ss fixed metric. So
  runs with every view attached need no display. `TkHost` (via
  `set_window_host_maker(tk_host_maker(root))`) puts each window in a Toplevel with a
  tkinter Canvas, for pictures and item 15's GUI.
- **Port changes** (`# port:`): the host replaces SWL's widgets
  (`get-scrollbar-from-frame` asks the host, `reposition-vertical-scrollbar` calls
  `host.set_vertical_view`); `swl:sync-display` flushes through sgl's hook; SWL's
  `thread-break` (a click in the Workspace window `(go)`es the REPL) raises until item 15
  installs a handler (`views.set_thread_break_handler`; *item 17:* gui/app.py installs
  gui.py's `thread-break`); fonts made by `set!` in the
  `select-...-fonts` procedures are module globals; `relation-names-pexp`, never defined
  in the original, raises Chez's unbound-variable error. Record-case clauses ignore extra
  arguments where the original relies on it (trace.ss sends `draw-string-letters` a tag;
  without that every run crashed at its first answer with the views on).
  `update-rule-pexps!` mutates in place, as the original's `set-car!` does, and returns
  the pexp, so either caller style works.
- **chez.py**: exact and flonum complex arithmetic part by part with signed zeros,
  `magnitude` (libm `hypot`, i.e. `abs(complex)`, not `math.hypot`), `angle` (exact 0 for
  positive reals), `make-polar`, `cos`/`sin`/`tan`/`acos` with Chez's exact results
  (anomalies_and_quirks.md).
- **Tests**: `test_graphics.py` (the 48 tests of graphics-battery.scm and the engine
  modules' structure), `test_panels.py` (the 36 tests of panels-battery.scm), 
  `test_gui_windows.py` and `test_gui_panels_a.py` (the windows on recording canvases,
  structure), `test_views.py` (one short golden with every view attached in the fast
  tier; the 109 goldens and the crash run with every view attached, and the eight
  rendered scenes of `tests/render_views.py` under Xvfb, in the slow tier).


### As built (item 15)

- `metacat/gui/gui.py` is gui.ss: one function or class per definition (61). The
  control panel (`ControlPanel`) and the window controllers are `SchemeObject`s with the
  original's messages. They add `get-widgets` (for tests), `visible?` and `engine-error`.
  SWL widgets are tkinter's own (Toplevel, Label, Entry, Scale, Button, Frame), packed in
  gui.ss's order with its options. SWL's menu items are objects (`MenuItem`, `Menu`):
  they keep their options until the main menu is attached, then pass them to their Tk
  entries. Tk's X11 menubar holds Help and Clear Memory as commands, as SWL's did.
  `SwlToplevel.destroy` runs the destroy-request handler first, as SWL's did, and the
  dialogs depend on that. The `(pause 700)` in the GUI thread becomes `after(700)`.
- `metacat/demos.py` is demos.ss. `metacat/gui/app.py` holds setup.ss's `setup` and
  `enable-resizing`, the engine thread, and the window layout (racket/gui/gui.rkt's
  `arrange-windows!`). `python3 -m metacat.gui [SCALE]` calls `app.main`.
- **The engine runs in a worker thread with a queue**, not in `after`-driven steps.
  `EngineThread` stands for the REPL thread. SWL's `thread-break` (`gui.thread_break`)
  queues thunks on it, and it runs each one through `run.toplevel`. A break parks the
  run (item 11), and the next thunk (`go`) resumes it. The `after` alternative was
  rejected because suspend breaks from inside a codelet (answers.ss). Only a parked thread
  can resume there; stepping from `after` would need codelets split in two.
- Tk is only ever touched from the main thread. `swl.ThreadSafeTk`, the root's `.tk`,
  marshals calls from other threads through a queue and a pipe
  (docs/anomalies_and_quirks.md). The control panel's actions never wait for the
  engine.
- `TkHost` (hosts.py) now has show/hide, the real geometry and the window manager's close
  (toplevel-destroy-action). A resizable window turns `<Configure>` into the viewport's
  `configure`, and the resize listener redraws it. Mouse presses go to the viewport's
  `mouse-press` with SWL's modifiers. `create-mcat-logo` uses constants.ss's logo colour
  and font.
- Tests: `tests/test_gui.py` and `tests/drive_gui.py`. gui-battery.scm pins the parser,
  the speed slider, the figure titles, the demos and the clamp patterns. The driver works
  the widgets under xvfb-run, and each GUI run's trace equals its golden.

### As built (item 16)

- `python/pyproject.toml` declares the packages `metacat` and `metacat.gui` (only
  `metacat` before, so a regular install left out the GUI), `metacat/gui/help.txt` as
  package data, and the commands `metacat` → `metacat.__main__:main` and `metacat-gui` →
  `metacat.gui.app:main`. Both `main`s read `sys.argv` when called with no arguments;
  the CLI's `main` raises the recursion limit itself, since a console script never runs
  the `__main__` block.
- help.txt is a byte-for-byte copy of `chez_scheme/original/help.txt` inside the package
  (gui.py read it from the checkout before); a test keeps them equal. Nothing in the
  package reads files outside it.
- Tests: `tests/test_install.py` (a clean copy of `python/`, `pip install -e` and a regular
  install into fresh venvs; the CLI's stdout against the live oracle, its trace against
  the golden, the GUI opening under xvfb-run).

### Audit (item 17)

Re-run on 2026-10-04, under a load average of about 30 from another user's jobs:
the 109 goldens, the 109 goldens with every view attached, the 720 extra seeds and their
oracle re-capture (4 min 47 s for the three files), then everything else, including the
GUI driven through its widgets under xvfb-run and the venv installs (2 min 34 s). All 1407
tests passed. With the audit's new tests the gate runs 1412, all passing (6 min 57 s).

No engine module imports tkinter or `metacat.gui`, at the top or inside a function
(checked on the AST). At run time the audit found one leak: `engine.translated_modules()`
took `"gui"` in `LOAD_ORDER` (gui.ss) for the `metacat.gui` *package* and imported its
`__init__` on `engine.load()`. No tkinter came with it and no `load()` ran, so runs were
unchanged. It now skips packages, and `test_gui.py::test_a_headless_run_loads_no_gui_module`
checks a fresh process: every engine module imported, `engine.load()`, a headless run,
and no `tkinter` or `metacat.gui*` in `sys.modules`. The test failed before the fix.

The plan was re-read against the code (a read-only subagent checked every name, path
and stated behaviour; I checked its findings). Nine statements were wrong or stale and
thirteen future-tense ones had since been done, some differently. They are corrected in
place, each marked "item 17", and the originals are left as written. Risk 6's
promised tests never existed: tests/test_engine_modules.py now imports every engine
module alone in a fresh interpreter (no load, no draw) and checks the cross-module
`from`-imports on the AST. Three mutants (a `from metacat.bonds import build_bond` in
rules.py, a draw at import time and a cross-module read at import time) all fail it.

#### The `# chez:` and `# 1.2:` sites

A `# chez:` comment marks a place where the Python reproduces a Chez semantic that
Python lacks; a `# 1.2:` comment marks a quirk of Metacat 1.2 itself, kept. The list
below is generated from the code's comments (not docstrings) by
`python/tests/quirk_sites.py --write`, and `tests/test_quirk_sites.py` fails when it
drifts. The kinds are a rough sort by the comment's words. Sites are named by module and
enclosing function, not line.

<!-- quirk-sites:begin (python/tests/quirk_sites.py --write) -->

243 sites: 168 `# chez:` (Chez's semantics that Python must reproduce) and 75 `# 1.2:` (Metacat 1.2's own quirks, kept).

| `# chez:` kind | Sites |
|---|---|
| map's order | 81 |
| stochastic-if* coin first | 33 |
| evaluation order | 17 |
| recursion and sequence order | 10 |
| truthiness (only #f is false) | 7 |
| characters, strings and symbols | 6 |
| sort | 6 |
| numbers and printing | 4 |
| other | 2 |
| record-case and case | 2 |

| Module | `# chez:` | `# 1.2:` |
|---|---|---|
| `metacat.answers` | 11 | 0 |
| `metacat.bonds` | 3 | 1 |
| `metacat.breakers` | 3 | 0 |
| `metacat.bridge_graphics` | 1 | 2 |
| `metacat.bridges` | 10 | 6 |
| `metacat.chez` | 2 | 0 |
| `metacat.coderack` | 5 | 3 |
| `metacat.concept_mappings` | 3 | 0 |
| `metacat.descriptions` | 3 | 0 |
| `metacat.eeg_graphics` | 1 | 0 |
| `metacat.formulas` | 1 | 1 |
| `metacat.general_graphics` | 0 | 2 |
| `metacat.group_graphics` | 0 | 1 |
| `metacat.groups` | 11 | 2 |
| `metacat.gui.colors` | 0 | 1 |
| `metacat.gui.gui` | 4 | 0 |
| `metacat.gui.sgl` | 0 | 1 |
| `metacat.gui.temperature_graphics` | 0 | 1 |
| `metacat.gui.theme_graphics` | 0 | 1 |
| `metacat.gui.workspace_graphics` | 1 | 4 |
| `metacat.images` | 2 | 2 |
| `metacat.jootsing` | 9 | 0 |
| `metacat.justify` | 4 | 4 |
| `metacat.memory` | 1 | 0 |
| `metacat.objects` | 2 | 1 |
| `metacat.qt.controls` | 1 | 0 |
| `metacat.rule_graphics` | 1 | 0 |
| `metacat.rules` | 31 | 2 |
| `metacat.run` | 3 | 0 |
| `metacat.setup` | 1 | 0 |
| `metacat.slipnet` | 4 | 0 |
| `metacat.sugar` | 2 | 1 |
| `metacat.themes` | 11 | 2 |
| `metacat.trace` | 2 | 21 |
| `metacat.trace_graphics` | 1 | 1 |
| `metacat.trace_writer` | 1 | 0 |
| `metacat.utilities` | 13 | 1 |
| `metacat.workspace` | 10 | 6 |
| `metacat.workspace_objects` | 3 | 3 |
| `metacat.workspace_strings` | 5 | 4 |
| `metacat.workspace_structure_formulas` | 0 | 1 |
| `metacat.workspace_structures` | 2 | 0 |

`# 1.2:` sites (module, function: comment):

- `metacat.bonds`, `bonds_equal_p`: the last test calls same-direction?, which nothing defines (anomalies: "bonds-equal? calls same-direction?,...
- `metacat.bridge_graphics`, `make_bridge_pexp`: a case without else
- `metacat.bridge_graphics`, `draw_bridge_grope`: a case without else
- `metacat.bridges`, `_BridgeClauses.boost_themespace_activations`: not gated by %workspace-graphics% (anomalies: "Bridges call themes.ss on every bridge")
- `metacat.bridges`, `_BridgeClauses._calculate_external_strength`: a one-argument * (anomalies: "Dead code in bridges.ss")
- `metacat.bridges`, `direction_incompatible_bridges`: cond without else: void, which partition applies (an error) if it ever compares two bridges
- `metacat.bridges`, `propose_singleton_group`: never called (anomalies: "Dead code in bridges.ss")
- `metacat.bridges`, `try_to_propose_singleton_group`: never called (anomalies: "Dead code in bridges.ss")
- `metacat.bridges`, `letter_category_mappable_objects_p`: object1's group category twice (anomalies: "letter-category-mappable-objects? compares a group with itself")
- `metacat.coderack`, `post_codelet_probability`: a case without else; the other codelet types give void
- `metacat.coderack`, `num_of_codelets_to_post`: a case without else; the other codelet types give void
- `metacat.coderack`, `thematic_codelet_urgency`: a case without else
- `metacat.formulas`, `current_translation_temperature_threshold_distribution`: the exact density is compared with the flonums exactly, so 1/5, 2/5 and 4/5 fall into the hotter class (ano...
- `metacat.general_graphics`, `remove_leading_blanks`: an all-blank line comes back unchanged
- `metacat.general_graphics`, `grid.line`: a case without else
- `metacat.group_graphics`, `make_group_pexp`: a cond without else
- `metacat.groups`, `group_builder`: not gated by %workspace-graphics%
- `metacat.groups`, `group_builder`: not gated by %workspace-graphics%
- `metacat.gui.colors`, `swl_color`: an unknown name fails in (cadr #f)
- `metacat.gui.sgl`, `Viewport.draw_text`: case without else gives void
- `metacat.gui.temperature_graphics`, `new_temperature_window`: title is still #f here, so the icon label is #f (anomalies: "The Temperature window's icon label is `#f`")
- `metacat.gui.theme_graphics`, `Panel.get_relation_names_pexp`: relation-names-pexp is never defined (the variable is relation-names-pexps); Chez raises when this runs, an...
- `metacat.gui.workspace_graphics`, `WorkspaceWindow.get_rule_coord`: case without else
- `metacat.gui.workspace_graphics`, `WorkspaceWindow.init_string_graphics`: case without else
- `metacat.gui.workspace_graphics`, `WorkspaceWindow.init_string_graphics`: case without else
- `metacat.gui.workspace_graphics`, `WorkspaceWindow.repair_built_bridges`: case without else
- `metacat.images`, `StringImage.reset`: forgets the direction it was made with (anomalies: "A string image's reset forgets its original direction")
- `metacat.images`, `StringImage.new_alpha_position_category`: sends new-start-letter (anomalies: "A string image's new-alpha-position-category sends new-start-letter")
- `metacat.justify`, `get_unifying_slippages`: fail is #f (the function assumes the rules can be unified)
- `metacat.justify`, `remove_whole_or_single_concept_mappings`: removes only the first match (select), not all of them
- `metacat.justify`, `compare_rule_clause_lists`: tests rc-list1 twice (rc-list2 is never tested)
- `metacat.justify`, `get_vertical_theme_pattern_to_clamp`: the test is the wrong way round (and the pattern is never printed)
- `metacat.objects`, `report_error_and_halt`: recurses forever for an object without object-type (porting-notes.md, item 03)
- `metacat.rules`, `apply_transforms`: (2nd (assq plato-bond-facet transforms)) is cadr of #f, an error, if no BondFacet transform comes with the...
- `metacat.rules`, `get_change_phrase.phrase`: (3rd BondFacet-change) is caddr of #f, a Chez error, when the clause has no BondFacet change (anomalies: "`...
- `metacat.sugar`, `mcat`: the validity test is an extend-syntax fender, so bad tokens are a syntax error
- `metacat.themes`, `Themespace.thematic_pressure_on`: not gated by a graphics switch; the headless null window absorbs it
- `metacat.themes`, `ThemeCluster.__init__`: alpha is fixed here, so set-sensitivity has no effect on it
- `metacat.trace`, `TemporalTrace.undo_last_clamp`: a case without else
- `metacat.trace`, `TemporalTrace.undo_last_clamp`: a case without else
- `metacat.trace`, `AnswerEvent.get_rule`: a case without else
- `metacat.trace`, `AnswerEvent.get_supporting_bridges`: a case without else
- `metacat.trace`, `AnswerEvent.get_rule_ref_objects`: a case without else
- `metacat.trace`, `ClampEvent.print_patterns`: a generic event has no print-patterns clause (tell halts)
- `metacat.trace`, `ClampEvent.get_complement_codelet_pattern`: complement-codelet-pattern is never defined (anomalies_and_quirks.md)
- `metacat.trace`, `ClampEvent.get_rule`: a case without else
- `metacat.trace`, `ClampEvent.activate`: a case without else
- `metacat.trace`, `ClampEvent.activate`: a case without else
- `metacat.trace`, `ConceptMappingEvent.__init__`: a case without else
- `metacat.trace`, `_rule_type_case`: a case without else
- `metacat.trace`, `RuleEvent.__init__`: a case without else
- `metacat.trace`, `SnagEvent.__init__`: a record-case without else
- `metacat.trace`, `SnagEvent.print_`: the failure results are tagged SWAP, CONFLICT and CHANGE, and Chez 10 is case-sensitive, so this case never...
- `metacat.trace`, `SnagEvent.get_explanation`: a record-case without else
- `metacat.trace`, `SnagEvent.get_supporting_bridges`: a case without else
- `metacat.trace`, `snag_object_phrase`: a cond without else
- `metacat.trace`, `unflipped_group_name`: a cond without else
- `metacat.trace`, `full_workspace_object_name`: a cond without else
- `metacat.trace`, `patterns_equal_p`: a cond without else
- `metacat.trace_graphics`, `group_event_pexp_text_string.descriptor_string`: a cond without else
- `metacat.utilities`, `ascending_index_list`: (accumulate (sub1 n) '()) counts down from n - 1 and never reaches zero, so n = 0 loops forever (porting-no...
- `metacat.workspace`, `Workspace.get_possible_bridge_objects`: (apply append <void>) is an error
- `metacat.workspace`, `Workspace.get_activity`: (min 1.0 ...) makes the ratio a flonum before 100* rounds it (anomalies: "Exact bond densities meet flonum...
- `metacat.workspace`, `Workspace.get_proposed_bridges`: (vector->list <void>) is an error
- `metacat.workspace`, `Workspace.get_all_other_coincident_bridges`: (remq bridge <void>) is an error
- `metacat.workspace`, `Workspace.delete_all_proposed_bridges`: for-each-vector-element* loops forever on an empty vector (ascending-index-list 0; porting-notes.md, item 03)
- `metacat.workspace`, `Workspace.maximal_mapping_p`: tell-all on <void> is an error
- `metacat.workspace_objects`, `WorkspaceObject.distinguishing_descriptor_p`: a cond without else; tell-all on void then fails, as in Chez
- `metacat.workspace_objects`, `WorkspaceObject.update_average_unhappiness`: case without else; round then fails on void
- `metacat.workspace_objects`, `WorkspaceObject.update_average_salience`: case without else; round then fails on void
- `metacat.workspace_strings`, `WorkspaceString.__init__`: (ascending-index-list 0) loops forever, so an empty string never gets here
- `metacat.workspace_strings`, `WorkspaceString.delete_all_proposed_bonds`: for-each-vector-element* loops forever on an empty table (ascending-index-list 0)
- `metacat.workspace_strings`, `WorkspaceString.delete_all_proposed_groups`: for-each-vector-element* loops forever on an empty table (ascending-index-list 0)
- `metacat.workspace_strings`, `WorkspaceString.get_relevance`: a single non-spanning object divides by zero, as in Chez
- `metacat.workspace_structure_formulas`, `description_type_support`: a string without objects divides by zero, as in the original

`# chez:` sites (module, function: comment):

- `metacat.answers`, `most_recent_group_and_concept_mapping_events`: map's order of application (the procedure only reads)
- `metacat.answers`, `get_unjustified_theme_pattern`: map's order of application (the procedure only reads)
- `metacat.answers`, `average_theme_abstractness`: map's order of application (the procedure only reads)
- `metacat.answers`, `answer_finder`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.answers`, `answer_finder`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.answers`, `get_rule_supporting_groups`: map's order of application (the procedure only reads)
- `metacat.answers`, `make_translated_rule_bridges`: map's order of application
- `metacat.answers`, `translate.body`: map's order of application (the procedure draws and can escape)
- `metacat.answers`, `translate_rule_clause.translate_clause`: map's order of application (the procedure draws and can escape)
- `metacat.answers`, `apply_to_change.apply_change`: (list ...) evaluates its arguments left to right, and both apply-slippages calls can draw and log slippages
- `metacat.answers`, `apply_to_object_description`: (list ...) evaluates its arguments left to right, and the apply-slippages calls can draw and log slippages
- `metacat.bonds`, `Bond.get_local_density`: a let*: all the left neighbours (drawn) before the right ones
- `metacat.bonds`, `bond_evaluator`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.bonds`, `choose_bond_facet`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.breakers`, `breaker`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.breakers`, `breaker`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.breakers`, `breaker`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.bridge_graphics`, `bridge_graphics`: a two-binding let whose bindings only ask the bridge
- `metacat.bridges`, `_BridgeClauses.get_average_theme_support`: map's order of application (the procedure only computes)
- `metacat.bridges`, `_BridgeClauses.get_theme_support_values`: map's order of application
- `metacat.bridges`, `bottom_up_bridge_scout`: map's order of application (get-mapping-strength only reads)
- `metacat.bridges`, `bottom_up_bridge_scout`: map's order of application (the procedure only computes)
- `metacat.bridges`, `bottom_up_bridge_scout`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.bridges`, `important_object_bridge_scout`: map's order of application (get-mapping-strength only reads)
- `metacat.bridges`, `important_object_bridge_scout`: map's order of application (the procedure only computes)
- `metacat.bridges`, `important_object_bridge_scout`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.bridges`, `bridge_evaluator`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.bridges`, `try_to_propose_singleton_group`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.chez`, `_flonum_digits`: when x lies exactly halfway between the two shortest candidates, Python's repr rounds the last digit half t...
- `metacat.chez`, `(module level)`: R6RS constituents beyond ASCII; other characters are written \xHH; in a symbol (U+00AB, U+00AD, U+00A0, U+0...
- `metacat.coderack`, `Coderack.initialize`: for*'s value is the last body's (anomalies: "sort, remq, for-each and one-armed if differ")
- `metacat.coderack`, `post_codelet_probability`: #f only (supported-rule-exists? may answer a list)
- `metacat.coderack`, `add_top_down_codelets`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.coderack`, `add_bottom_up_codelets`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.coderack`, `load`: the labels are strings, which the panels draw as text (anomalies: "The graphics and rules.ss tell strings f...
- `metacat.concept_mappings`, `ConceptMapping.distinguishing_p`: (and ...) returns the last test's value, which need not be a boolean
- `metacat.concept_mappings`, `ConceptMapping.relevant_distinguishing_p`: #f only
- `metacat.concept_mappings`, `ConceptMapping.distinguishing_identity_or_opposite_p`: #f only
- `metacat.descriptions`, `Description.__init__`: a let's order is unspecified (porting-notes.md says right to left here); both bindings are free of side eff...
- `metacat.descriptions`, `Description.get_theme_support_values`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.descriptions`, `description_evaluator`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.eeg_graphics`, `EEG.initialize`: map's order of application (the thunks only read)
- `metacat.formulas`, `temp_adjusted_values`: map's order of application (the procedure is pure)
- `metacat.groups`, `Group.__init__`: let* order, binding by binding
- `metacat.groups`, `Group.get_local_density`: evaluation order: both arguments of append draw (choose-...-neighbor), and Chez evaluates append's second a...
- `metacat.groups`, `top_down_group_scout__category`: stochastic-if* draws its coin before the probability, which may draw itself (get-local-support -> get-local...
- `metacat.groups`, `group_evaluator`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.groups`, `group_builder`: andmap goes first to last and stops at the first loss (and its draws)
- `metacat.groups`, `group_builder`: adjacency-map is a two-list map, in Chez's order (build-bond has effects)
- `metacat.groups`, `group_builder`: adjacency-map is a two-list map, in Chez's order (build-bond has effects)
- `metacat.groups`, `group_builder`: map's order of application (break-bond and build-bond have effects)
- `metacat.groups`, `propose_group`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.groups`, `polarize_bonds.body`: map's order of application (an escape can end it midway)
- `metacat.groups`, `get_all_nested_groups`: map's order of application (the procedure only reads)
- `metacat.gui.gui`, `_alphabetic_p`: char-alphabetic? is Unicode's Alphabetic property, str.isalpha the letter categories (anomalies: "str.isalp...
- `metacat.gui.gui`, `_numeric_p`: char-numeric? is Unicode's Numeric property (½ and Arabic-Indic digits are numeric; fixture char-noise), as...
- `metacat.gui.gui`, `_downcase`: char-downcase maps one character to one
- `metacat.gui.gui`, `ControlPanel.theme_edit_mode_off`: map's order (the patterns only read)
- `metacat.gui.workspace_graphics`, `WorkspaceWindow.draw_string_letters`: record-case ignores extra arguments; trace.ss sends a tag (docs/anomalies_and_quirks.md, "Chez's record-cas...
- `metacat.images`, `StringImage.replace_all`: map's order of application; fail can escape midway (porting-notes.md, item 05)
- `metacat.images`, `Image.replace_all`: map's order of application; fail can escape midway (porting-notes.md, item 05)
- `metacat.jootsing`, `jootser`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.jootsing`, `jootser`: map's order of application (the procedure only reads)
- `metacat.jootsing`, `jootser`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.jootsing`, `jootser`: map's order of application (the procedure only reads)
- `metacat.jootsing`, `get_clamp_jootsing_probability`: map's order of application (the procedure only reads)
- `metacat.jootsing`, `get_clamp_jootsing_probability`: (* exact 0.5) of an exact 0 is exact 0 (chez.mul)
- `metacat.jootsing`, `joots_from_justify_clamps`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.jootsing`, `progress_watcher`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.jootsing`, `progress_watcher`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.justify`, `answer_justifier`: map's order of application (the procedure only reads)
- `metacat.justify`, `answer_justifier`: of clamp-rules' arguments only get-vertical-theme-pattern-to-clamp draws (prob?); the others only read, so...
- `metacat.justify`, `unify_rules.body`: map's order of application (the procedure only reads)
- `metacat.justify`, `traverse_rule_clauses.walk`: (walk (1st x1) (1st x2) (walk (rest x1) (rest x2) results)): the rests are walked first, so lists of differ...
- `metacat.memory`, `_check_reals`: comparing with #f (a bounding box never set) is an error; Python's bool is an int and would compare quietly
- `metacat.objects`, `tell_all`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.objects`, `delegate_to_all`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.qt.controls`, `QtControlPanel.theme_edit_mode_off`: map's order (the patterns only read)
- `metacat.rule_graphics`, `_rule_layout`: map's order of application (the window answers each width)
- `metacat.rules`, `Rule.set_abstracted_rule_information`: map's order of application (the procedure only reads)
- `metacat.rules`, `Rule.set_translated_rule_information`: map's order of application (the procedure only builds lists)
- `metacat.rules`, `Rule.get_degree_of_support`: map's order of application (the procedure only reads)
- `metacat.rules`, `Rule.get_concept_pattern`: map's order of application (the procedure only builds lists)
- `metacat.rules`, `Rule.revise_abstracted_rule_information`: map's order of application (the procedure only reads)
- `metacat.rules`, `rule_scout`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.rules`, `rule_scout`: map's order of application (the procedure only reads)
- `metacat.rules`, `rule_scout`: map's order of application (instantiating a template draws)
- `metacat.rules`, `rule_evaluator`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.rules`, `abstract_change_descriptions`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.rules`, `abstract_change_descriptions`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.rules`, `abstract_change_descriptions`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.rules`, `abstract_change_descriptions`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.rules`, `sort_templates`: Chez's sort algorithm and predicate calls
- `metacat.rules`, `instantiate_rule_clause_template`: map's order of application (instantiate-change-template draws)
- `metacat.rules`, `instantiate_rule_clause_template`: map's order of application (choose-description-for-rule draws)
- `metacat.rules`, `sort_change_templates`: Chez's sort algorithm and predicate calls
- `metacat.rules`, `ExtrinsicChangeDescription.__init__`: map's order of application (the procedure only reads)
- `metacat.rules`, `changes_implied_by_string_position_swaps`: map's order of application (the procedure only reads)
- `metacat.rules`, `apply_rule.body`: map's order of application (the procedure only builds lists)
- `metacat.rules`, `check_for_conflicts`: map's order of application (the procedure only reads)
- `metacat.rules`, `apply_transforms`: Chez's sort algorithm and predicate calls
- `metacat.rules`, `get_extrinsic_transforms.clause_transforms`: map's order of application (attach-length-description, fail)
- `metacat.rules`, `get_extrinsic_transforms`: map's order of application (attach-length-description, fail)
- `metacat.rules`, `get_dimension_transforms.dimension_transforms`: map's order of application (attach-length-description)
- `metacat.rules`, `get_dimension_transforms.dimension_transforms`: map's order of application (the procedure only builds lists)
- `metacat.rules`, `get_intrinsic_transforms`: map's order of application
- `metacat.rules`, `transcribe_to_english`: map's order of application (the phrases only read; a crash in one clause's phrases is the same whichever cl...
- `metacat.rules`, `get_rule_clause_phrases.phrases`: map's order of application (the phrases only read)
- `metacat.rules`, `get_swap_phrase`: map's order of application (the phrases only read)
- `metacat.rules`, `punctuate`: map's order of application (format only computes)
- `metacat.run`, `Breakpoint.__call__`: a continuation can be re-entered; a thread cannot
- `metacat.run`, `init_workspace`: let inits last first; none of them draws
- `metacat.run`, `update_everything`: the coin first (stochastic-if*)
- `metacat.setup`, `coderack_off`: a string, which the window draws as text (anomalies: "The graphics and rules.ss tell strings from symbols")
- `metacat.slipnet`, `Slipnode.spread_activation`: (* a b c) multiplies left to right
- `metacat.slipnet`, `Slipnode.attempt_to_post_top_down_codelets`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.slipnet`, `update_slipnet_activations`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.slipnet`, `number_to_platonic_number`: list-tail of a negative index is an error (Python's l[-1] is not)
- `metacat.sugar`, `for_star`: for-each's value is the last application's (anomalies: "sort, remq, for-each and one-armed if differ")
- `metacat.sugar`, `stochastic_if_star`: the coin first, then the probability (anomalies: "Chez doesn't evaluate arguments left to right"; plan, "Ev...
- `metacat.themes`, `Themespace.__init__`: map's order of application (making clusters draws nothing)
- `metacat.themes`, `Themespace.get_complete_state`: map's order of application (the procedure only reads)
- `metacat.themes`, `Themespace.get_all_complete_theme_patterns`: map's order of application (the procedure only reads)
- `metacat.themes`, `Themespace.get_all_dominant_theme_patterns`: map's order of application (the procedure only reads)
- `metacat.themes`, `ThemeCluster.__init__.net_effect`: (* alpha 0) is exact 0 and (tanh 0) exact 0
- `metacat.themes`, `ThemeCluster.update_dominant_theme`: Chez's sort and its predicate calls (utilities.sort_by_method)
- `metacat.themes`, `BridgeTheme.spread_activation_to_slipnet`: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
- `metacat.themes`, `BridgeTheme.spread_activation_to_slipnet`: stochastic-if* draws its coin before the probability
- `metacat.themes`, `thematic_bridge_scout`: map's order of application (the procedure only reads; its two tells only read)
- `metacat.themes`, `thematic_bridge_scout`: tell-all's map order (one stochastic-pick-by-method per cluster)
- `metacat.themes`, `thematic_bridge_scout.selection_entry`: #f only ('() is a condition list)
- `metacat.trace`, `AnswerEvent.display_workspace`: the extra 'answer tag is ignored by the window's record-case (anomalies: "Chez's record-case ignores extra...
- `metacat.trace`, `AnswerEvent.make_answer_description_pexp`: the extra tags are ignored by the window's record-case
- `metacat.trace_graphics`, `group_event_pexp_text_string`: map's order of application (the procedure only reads)
- `metacat.trace_writer`, `names`: map over pure getters; the order is not observable
- `metacat.utilities`, `exists_p`: only #f is false (docs/python-translation-plan.md, "Booleans and truthiness")
- `metacat.utilities`, `all_same_p`: eq? on flonums is identity (fixture all-same)
- `metacat.utilities`, `flatmap`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.utilities`, `sort_wrt_order`: Chez's sort algorithm and predicate calls (anomalies: "sort, remq, for-each and one-armed if differ")
- `metacat.utilities`, `sort_by_method`: Chez's sort algorithm and predicate calls (anomalies: "sort, remq, for-each and one-armed if differ") speed...
- `metacat.utilities`, `rough`: the size is drawn (let binding) before the sign (body)
- `metacat.utilities`, `select_extreme`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.utilities`, `adjacency_map`: map's order of application (anomalies: "Chez's map applies its procedure in a strange order")
- `metacat.utilities`, `cross_product_filter_map`: f recurses on (rest l1) before g walks l2, so l1 runs last to first
- `metacat.utilities`, `cross_product_map_filter`: l1 last to first, as in cross_product_filter_map
- `metacat.utilities`, `pairwise_map`: the recursive call before the map (anomalies: "Chez evaluates append's second argument first")
- `metacat.utilities`, `partition`: (insert (1st l) (partition (rest l))): the rest is partitioned first
- `metacat.utilities`, `bounded_random_partition`: every pick (and draw) happens before the first insert
- `metacat.workspace`, `_table_lists`: map's order is irrelevant here (vector->list is pure)
- `metacat.workspace`, `Workspace._four`: append's argument order is unspecified; these tells are pure
- `metacat.workspace`, `Workspace.get_youngest_structures_average_age`: Chez's sort and its predicate calls (ties on age)
- `metacat.workspace`, `Workspace.get_equivalent_bridge`: a let's order is unspecified; both bindings are pure
- `metacat.workspace`, `Workspace.check_if_rules_possible`: subset?'s two arguments are pure (rule-describable-bridge? only reads)
- `metacat.workspace`, `Workspace.choose_object`: tell-all in map's order
- `metacat.workspace`, `Workspace.update_average_unhappiness_values`: a let's order is unspecified; these bindings are pure
- `metacat.workspace`, `spanning_group_possible_p.possible_for.relation`: a let's order is unspecified; both bindings are pure
- `metacat.workspace`, `spanning_group_possible_p.possible_for`: adjacency-map is a two-list map (chez.map_ order); relation is pure
- `metacat.workspace`, `rough_num_of_objects`: (~ 4) is drawn only when the first test fails (cond)
- `metacat.workspace_objects`, `Letter.__init__`: let* order (the workspace object, then the image, ...)
- `metacat.workspace_objects`, `WorkspaceObject.get_concept_pattern`: map's order (the procedure is pure; kept for uniformity)
- `metacat.workspace_objects`, `WorkspaceObject.choose_neighbor`: append's arguments draw nothing, so their order doesn't matter
- `metacat.workspace_strings`, `WorkspaceString.__init__`: let* order
- `metacat.workspace_strings`, `WorkspaceString.choose_object_with_description_type`: the weights are computed (with tell-all's order) before the null? test
- `metacat.workspace_strings`, `WorkspaceString.get_constituent_objects`: Chez's sort (sort-by-method)
- `metacat.workspace_strings`, `WorkspaceString.get_all_reference_objects`: map's order of application
- `metacat.workspace_strings`, `WorkspaceString.get_reference_objects`: map's order of application
- `metacat.workspace_structures`, `wins_fight_p`: a body sequence: the challenger's strength is updated before the defender's
- `metacat.workspace_structures`, `wins_all_fights_p`: andmap goes first to last and stops at the first loss, so the draws stop there too

<!-- quirk-sites:end -->

## Names

`python/metacat/names.py` (`scheme_to_python`; moved into the package by item 03) maps every name the
original defines (about 1,300) to a valid, non-reserved Python identifier, and
`test_name_mapping.py` checks that the mapping is injective on all of them.

| Scheme | Python | Example |
|---|---|---|
| `-` | `_` | `make-bond` → `make_bond` |
| `?` | `_p` | `foo-bar?` → `foo_bar_p`, `CMs-equal?` → `CMs_equal_p` |
| `!` | `_bang` | `vector-increment!` → `vector_increment_bang` |
| `->` | `_to_` | `bridge-type->theme-type` → `bridge_type_to_theme_type` |
| `*x*` (global variable) | `g_x` | `*temperature*` → `g_temperature`, `*EEG*` → `g_EEG` |
| `%x%` (parameter: tunable constants and switches) | `p_x` | `%verbose%` → `p_verbose` |
| `=x=` (colour) | `c_x` | `=white=` → `c_white` |
| trailing `*` (the extend-syntax forms) | `_star` | `stochastic-if*` → `stochastic_if_star` (item 03 decides which of them become functions and which become statements) |
| `:` | `__` | `group-scout:whole-string` → `group_scout__whole_string` |
| `/` | `_or_` | `ObjCtgy/Length-change?` → `ObjCtgy_or_Length_change_p` |
| `.` | `_` | `fig5.10` → `fig5_10` |
| Python keyword or builtin | trailing `_` | `break` → `break_`, `print` → `print_`, `round` → `round_`, `filter` → `filter_`, `sum` → `sum_` |
| case | kept | `plato-a` → `plato_a` |
| exceptions | fixed table | `1st` … `8th` → `first` … `eighth`; `1-`, `10-`, `100-` → `one_minus`, `ten_minus`, `hundred_minus`; `100*` → `times_100`; `%`, `20%`, `40%`, `80%` → `percent`, `percent_20`, …; `^2`, `^3` → `square`, `cube`; `~` → `rough`; `?` (themes.ss's help) → `theme_help`; `180/pi`, `pi/180` → `degrees_per_radian`, `radians_per_degree` |

Locals and parameters follow the same rules. Message names stay Scheme strings (above).
Module names: `workspace-strings.ss` → `workspace_strings.py`. Every function's
docstring starts with its origin (`"""bonds.ss: bond-builder"""`).

## The global top level and Python modules

The original loads 44 files into one top level (metacat.ss's order). Definitions refer
to each other across files in both directions (coderack.ss names the codelet procedures
of eleven later files; descriptions.ss calls back into later files). Files also `set!`
each other's globals: 27 globals are assigned outside their defining file (`*temperature*`
by answers.ss, formulas.ss and run.ss; the workspace strings by run.ss; the mode switches
by gui.ss; `*fg-color*` by four graphics files; ...). Drivers and tests replace about 150
more from outside (the trace wrappers `build-bond`, `break-group`, … and the fakes of
the batteries; Racket's `set-global!` list in racket/engine.rkt).

Racket had to include everything into one module. Python can do better:

1. **One module per `.ss` file** (`metacat/bonds.py` for bonds.ss), one function per
   definition, in the original's order within the file.
2. **Definitions only at import time.** A module's top level holds `def`s, classes and
   constants that need nothing from another engine module except `chez`, `objects`,
   `sugar` and `utilities`. Every top-level `define` whose value needs another module
   (building the slipnet's nodes and 202 links, the codelet types, `*workspace*`,
   `*themespace*`, `*trace*`, `*memory*`, ...) is computed
   in the module's `load()` function, which assigns the module global. So modules can
   import each other freely, cycles included (`from metacat import bonds, groups` at
   the top; Python ≥ 3.7 resolves partially initialised submodules).
3. **Load order is explicit**: `metacat/engine.py` imports every engine module, then
   `load()` calls each module's `load()` in metacat.ss's order (syntactic-sugar,
   utilities, constants, setup, coderack, descriptions, bonds, groups, bridges,
   breakers, workspace, workspace-objects, workspace-structures, workspace-strings,
   concept-mappings, workspace-structure-formulas, run, formulas, slipnet, images, rules,
   answers, themes, justify, trace, jootsing, memory, then the engine parts of the
   graphics files, demos). A reference to a later module during load fails, as in Chez.
   *(Item 17: `engine.LOAD_ORDER` also names `fonts`, `sgl_interpreter` and `gui`, which
   have no engine module; `translated_modules()` skips them, and skips the `metacat.gui`
   package.)*
4. **References.** Inside a module, names are used unqualified. Module globals are
   looked up at call time, so `setattr(bonds, "build_bond", wrapper)` reaches in-file
   callers too. **Across modules, always qualified**: `bonds.build_bond(...)`,
   `setup.g_temperature`. `chez`, `objects`, `sugar` and `utilities` are the exception:
   their names are imported directly (`from metacat.utilities import tell, prob_p, ...`)
   because nothing rebinds them. Never `from metacat.bonds import build_bond`, which
   copies the binding and silently defeats wrappers and `set!`. *(Item 17: as built,
   bridge_graphics.py, group_graphics.py and rule_graphics.py `from`-import pure drawing
   helpers of general_graphics.py, and bridge_graphics.py `both_spanning_groups_p`;
   nothing rebinds them. tests/test_engine_modules.py allows exactly these.)*
5. **Assignment.** `(set! *temperature* 50)` in formulas.ss → `setup.g_temperature = 50`.
   Drivers and tests use `engine.set_global("*temperature*", 50)`, which maps the Scheme
   name to its defining module and attribute and raises if there is none. This is
   Racket's `set-global!` without the whitelist: Python modules allow `setattr`, and
   the check stops typos.
6. **A fresh engine per run.** The Memory and counters outlive a run (anomalies), and
   the oracle runs every golden in a fresh process. The Python golden runner loads the
   engine once, then forks one worker per run (`multiprocessing` with the `fork` start
   method), so every run starts from the state right after load.
7. **Engine modules never import tkinter.** Graphics code the model calls (group-graphics'
   `erase`, `group-event-pexp-text-string`, `relation-name`, the EEG object, rule pexps)
   is translated into engine modules that build data and send messages to window
   objects. The headless windows (null objects accepting the oracle's message list,
   porting-notes.md item 01) live in `headless.py`. A test walks the imports of every
   engine module (as racket/tests/no-gui-test.rkt does).

## Python-specific traps to log

These go into `docs/anomalies_and_quirks.md` when the code meets them:
- `dict` order is insertion order. Dicts serve only for dispatch and the top-level
  table, never for an order the model observes.
- `float.__repr__` differs from Chez's printer (above), and `str(Fraction(4, 2))` is
  `"2"` but its type isn't `int`.
- `bool` is a subclass of `int`: `True == 1`, `isinstance(True, int)`. `chez.eq_p` and
  the printer check `bool` first.
- Default recursion limit 1000.
- `round` on a `float` returns `int` (good). `round(x, n)` is never used; utilities.ss's
  `round-to-10ths` etc. are translated literally.
- Tuple vs list from `*args`.

## Order of the work

As in `iterations.md`, with these notes:

1. **02 `chez.py`**: PRNG, numbers (norm, div, mul, max/min, sqrt/exp/log/expt, tanh
   check), printer, `map_`, `sort`, `remq` family, `eq_p`/`equal_p`, `for_each`, top-level
   table, `UnboundVariable`. Capture scripts for vectors the utilities battery lacks:
   exact-zero products, contagion, exact sqrt/exp/log/expt, `float(Fraction)` rounding,
   evaluation-order probes for the record.
2. **03 `objects.py`, `sugar.py`, `utilities.py`**: C3 from the prototype; the 22 macros
   as functions (`stochastic_if_star(prob_thunk, body_thunk)` draws first), decorators
   (`define_codelet_procedure_star`; *item 17:* a plain function `(name, proc)` called in
   each `load()`) or explicit loops (`for*`, `repeat*`); the name
   mapping moves to `metacat/names.py`. All 197 utilities tests.
3. **04–10** the model, battery by battery, each through `engine.load()` and a Python
   version of the battery's harness (`tests/diff/codelet-harness.scm` → a pytest helper
   module). Each item: fixture tests first and failing, then the translation, an
   evaluation-order audit of its files, then `# chez:`/`# 1.2:` comments.
4. **11** run.ss, `trace.py` (*item 17:* `trace_writer.py`), `headless.py`, the CLI, and the 109 goldens in parallel.
   From here on the goldens are in the gate's tier.
5. **12** the 720 extra seeds, then profiling. Expected hot spots: `tell`, `Fraction`
   arithmetic, `chez.map_`, list copying. *(Item 17: measured: `tell`, then chez.py's
   type checks, `get-removal-weight` and `memq`; docs/python-run-times.md.)*
6. **13–15** SGL on `tkinter.Canvas` (fixture: the oracle's `swl:tcl-eval` stream),
   panels as views, the control panel with the engine on a worker thread (the `break`
   design above), all under `xvfb-run`.
7. **16–17** packaging, README, final audit.

## Risks, ranked

1. **Truthiness** (new in Python). `0`, `Fraction(0)`, `[]` and `""` are false in Python
   and true in Scheme. A slip quietly takes the other branch. Mitigation: the rules
   above, `# chez: #f only` comments, and the batteries, which exercise branches with
   zero activations and empty lists. The traces show a slip within one codelet.
2. **Exactness.** A float where Chez is exact (Python `/`, `math.sqrt` of a perfect
   square, `0 * float`, `max` contagion) changes urgencies and probabilities. Mitigation:
   `chez.div`/`mul`/`max_`, a `Fraction` normalisation helper, and item 02's vectors.
   The trace prints urgencies as `"n/d"`, so a stray float shows up at once
   (`20.4` instead of `"102/5"`).
3. **Speed.** About 1,000 messages per codelet, plus `Fraction` arithmetic. Projection:
   2–5 ms per codelet, so 5–25 s for a typical golden and up to about 85 s for the
   17,000-codelet `eqe qeq abbba aaabaaa` run. On 32 cores the 109 goldens (273,000
   codelets) take about 1–2 minutes and the 720 extra seeds (2.15 M codelets) about
   5–10 minutes, which is too slow for every gate. Mitigation: a fork-after-load worker
   pool, a fast tier per item, the full golden suite in the gate from item 11, and the
   extra seeds in a slow tier. Item 12 measures and decides. *(Item 17: measured 1.23 ms
   per codelet before item 12's speed-ups, about 9× Chez; the 109 goldens take about
   35 s on 32 cores and the 720 extra seeds about 2 min, both in the gate.)*
4. **Evaluation order.** Python is uniformly left to right, which removes Racket's
   surprises but keeps Chez's. The known sites are listed above. Unknown ones show up as
   an `rng` mismatch in a trace. Mitigation: per-item audits, and the codelet-level
   harness batteries, which have long runs (2000–3000 codelets) that reached
   groups.ss's site where 400-codelet runs didn't.
5. **`map`/`sort` order with side effects.** One shared `chez.map_` and `chez.sort`,
   never `map()`/`sorted()` where effects or ties exist. The utilities battery pins
   both, predicate calls included.
6. **Module load order and globals.** A `from x import y` of a rebindable name, or
   cross-module work at import time, breaks the wrappers or the load order silently.
   Mitigation: the rules above, plus a test that imports each engine module alone (no
   cross-module calls at import time) and an AST check that engine modules don't
   `from`-import names from each other. *(Item 17: neither existed until the final
   audit; both are in tests/test_engine_modules.py now, with the four pure-helper
   imports above allowed by name.)*
7. **The `break`/`go` resume** in the GUI (threads), and the views' thread discipline.
   Mitigation: item 15's control-panel test runs stop/resume runs to the golden's
   codelet count and generator state, as Racket's did.
8. **Symbols as `str`.** Safe for the model as audited. A later site that needs the
   distinction would need a `Symbol` type. Low.
9. **Faithful bugs.** The `report-error-and-halt` runs, the `caddr` crash of `abc ccbbaa
   ijk` seed 3, the recursion when `object-type` is missing, the latent errors in the
   anomalies file. Python must crash or halt at the same codelet, with the same output.
   The goldens and cli tests include the halt and the crash. Python's exception for the
   crash is a `TypeError`/`IndexError` where Chez says `caddr`, so the CLI maps it to the
   oracle's exit code 1 and stdout; stderr text may differ (as in Racket). *(Item 17: as
   built, chez.py raises `SchemeError("caddr", ...)`, and the CLI's first stderr line is
   the oracle's own, `Error: Exception in caddr: incorrect list structure #f`.)*

## What makes it easier than it looks

- The oracle, 109 goldens, 720 extra-seed results, and 604 frozen battery fixtures
  already exist. Every expected value is one file read away (`chez("battery", "test")`).
- The Racket port is a line-for-line, golden-equivalent translation of the same files.
  Where a Scheme form is unclear, the `.rktl` next to it shows a working reading.
- No hash tables, no threads, no `eval` beyond one name lookup, and only 8 direct
  `random` calls plus the utilities.ss helpers.
