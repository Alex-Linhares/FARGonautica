# Progress Log

## Ralph Loop 0002 Status
- **Started**: 2026-10-03
- **Target**: 18 items
- **Current**: 18/18 SOLVED

---

## Iteration 1 — 2026-10-03 18:45

### Completed
Item 00, skeleton and fixture pipeline: **SOLVED**.
- `python/` skeleton: `pyproject.toml` (package `metacat`, no runtime dependencies, `test`
  extra = pytest, a `slow` marker, `pythonpath = ["."]`), a stub `metacat/__init__.py`
  with the GPL and "translated to Python" lines, `README.md`, and `run-tests.sh` (`pytest -x`).
  Tiers: `bash python/run-tests.sh` is the full tier and is what the gate runs (about 55 s
  today). `--fast` skips `@pytest.mark.slow` tests (under a second).
- `python/oracle/capture.py BATTERY... | --all [--out DIR]` runs
  `chez_scheme/oracle/diff-eval.ss` (unedited) on `tests/diff/helpers.scm`, then
  `sgl-chez-setup.ss` for sgl (the Racket runner's `#:chez-setup`), then the battery,
  from the repo root. It splits the output per test and writes
  `python/fixtures/<battery>/{MANIFEST, NNN-NAME.txt, SOURCES}`. The split doesn't guess
  where a multi-line value ends: the test names, in order, come from Chez's reader
  (`python/oracle/list-tests.ss`, new, read-only). Before writing anything, the capture
  checks that joining the pieces again gives Chez's output byte for byte, and it fails on
  a non-zero exit or any stderr output. With `--all`, the ten batteries run in parallel.
- Captured all 10 batteries: 604 tests (bridge 46, codelet 59, coderack 43, graphics 48,
  panels 36, rule 50, sgl 48, slipnet 47, utilities 197, workspace 75). That's 168 MB
  of text (rule 94 MB, bridge 47 MB, codelet 23 MB, the largest file 4 MB) and about
  13 MB gzipped, so git's packs stay small. I kept plain text so the fixtures stay
  readable and diffable.
- `python/tests/chez_fixtures.py`: the loader for later items, `chez(battery, test)` →
  Chez's text. `python/tests/scheme_forms.py` is a minimal Scheme datum scanner that
  counts a battery's `(test ...)` forms independently of Chez.
- `python/tests/test_fixtures.py` (37 tests):
  - the fixture count per battery equals the battery's test count, as read by the Python
    scanner and compared with MANIFEST and the file names;
  - the split/join round trip on every battery, plus synthetic multi-line and malformed
    cases;
  - file and battery naming;
  - the scanner on comments, `#;`, strings and characters;
  - `SOURCES` unchanged: sha256 of diff-eval.ss, prelude.ss, list-tests.ss and every
    `tests/diff/` file, plus the Chez version. This is the fast freshness check.
  - **slow** `test_recapture_is_byte_identical`: re-captures every battery into a
    tmpdir and compares the files byte for byte. It takes about 53 s and runs in the
    full tier, so it runs in the gate.
- Test-first: I wrote the tests before any fixture existed and ran them against an
  empty `fixtures/`: 33 failed, 3 passed (only the pure-function ones passed). The
  capture script was written in the same step as the tests, not before them. After
  capturing, all 37 pass. A first run of the multi-line split test also caught a wrong
  expectation in the test itself. A name list that skips a test can still parse when the
  skipped record looks like part of a value. That ambiguity is why the names come from
  Chez's reader. I fixed the test case.
- A separate manual Chez run made earlier gave a rule output identical to the joined
  fixtures, and the slow re-capture matches too: the batteries are deterministic.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED.

### Blockers
None.

### Next
Item 01: the translation plan (`docs/python-translation-plan.md`) and the
object-system prototype with its micro-benchmark. Later items load expected values with
`from chez_fixtures import chez`. If new Chez-side setup is needed for a fixture, add a
capture script under `python/oracle/` instead of editing `tests/diff/`, which is frozen.

---

## Iteration 2 — 2026-10-03 18:43

### Completed
Item 01, the translation plan: **SOLVED**.
- `docs/python-translation-plan.md`, the counterpart of numbo's translation audit. I read
  compat.rkt, utilities.rkt, every `port:` comment and the six docs first. It records:
  - every Chez semantic the engine depends on and its Python strategy:
    - numbers: `Fraction` normalised to `int`, `chez.div`/`mul`/`max_`/`min_`/
      `sqrt`/`exp`/`log`/`expt`, the PRNG, rounding, the flonum printer;
    - evaluation-order sites with file and line (groups.ss:368–370, utilities.ss:690
      `pairwise-map`, run.ss:234, `for*`, `stochastic-if*`, `~`, `wins-fight?`);
    - `map`, `sort`, `remq`, `for-each`, `eq?`, truthiness, symbols vs strings,
      one-armed `if`, `case`, `record-case`, top-level values and `eval`;
    - continuations: escapes become exceptions; `break`/`go` becomes an exception
      headless and a blocked engine thread in the GUI;
    - printing.
  - the object representation, with the benchmark (below);
  - the module structure: one module per `.ss` file, definitions only at import time,
    and a `load()` per module called in metacat.ss's order by `engine.py`. In-file
    references are unqualified; cross-module references are always qualified
    (`setup.g_temperature`); `chez`/`objects`/`sugar`/`utilities` are imported
    directly. `engine.set_global` takes Scheme names. Goldens run in workers forked
    after load.
  - the name mapping;
  - the order of the work and nine risks, ranked. Truthiness comes first: it's new
    in Python, and Racket didn't have it.
- New Chez facts, checked under `scheme --script`:
  - `(* 0 1.5)` → `0` and `(/ 0 2.5)` → `0` (exact);
  - `(max 3 2.0)` → `3.0`;
  - `(exp 0)` → `1`, `(log 1)` → `0` and `(expt 0.0 0)` → `1`;
  - a 2-binding `let` inside a lambda evaluates left to right (`12`), unlike the
    documented top-level `21`. Logged in anomalies_and_quirks.md as an update to the
    evaluation-order entry.
- `python/oracle/count-calls.ss` (new) runs the unedited oracle run.ss with `tell` and
  `delegate` wrapped. About 1,000 `tell`s per codelet, 12–17% of them delegated:
  2,242,151 over the 2,170 codelets of abc abd xyz seed 3852097033. Output unchanged
  (wyz at 2170).
- Prototypes and tests:
  - `python/tests/object_prototypes.py`: candidates A (closure + if/elif), B (closure +
    dict of closures), C/C2/C3 (class per object with a message dict and the original
    `(self, msg, *args)` protocol) and D (Python inheritance), plus `bench()`.
    `python3 python/tests/object_prototypes.py` prints the table that is in the plan.
  - **Decision: C3.** About 173 ns per message wherever it sits in the record-case,
    297 ns delegated once, and 146 ns to create a child + parent. A costs 507 ns for
    the 40th message; B costs 4.4 µs to create an object; D can't delegate to
    separate objects.
  - `test_object_prototype.py` (32 tests). A, B, C and C3 are checked against the Chez
    fixtures of utilities-battery.scm's `tell`, `tell-args`, `tell-alias`,
    `tell-invalid`, `base-object`, `delegate`, `delegate-to-all`(`-order`,
    `-invalid`), `tell-all-order` and `record-case-no-else`, with a minimal `b:canon`.
    Other tests cover self through delegation and forwarders, the `chez_map1` order,
    and every candidate answering the benchmark object alike. The micro-benchmark
    asserts only the orderings the decision rests on, with wide margins, and prints
    the table.
  - `python/tests/name_mapping.py` + `test_name_mapping.py` (29 tests). The mapping is
    valid, non-reserved and injective on all of the original's 1,309 names (defines,
    extend-syntax forms, codelet types, slipnodes), with 25 pinned examples.
- Tests-first, honestly: here the prototypes *are* the code under test, so they were
  written together with their tests. What the tests caught while being written:
  - D failed the halt path ('ChildD' isn't callable);
  - the self-through-delegation test hit the original's own infinite recursion (an
    object without `object-type` that gets a bad message), and now delegates to
    `base-object`;
  - a wrong expectation (`plato-LetterCtgy` isn't a name; the nodes are
    `plato-letter-category`);
  - the first version of the self test covered only C. A `delegate_c3` mutation passed,
    so the test now runs for C and C3.
  Mutation checks, all restored afterwards:

  | Mutation | Tests failing |
  | --- | --- |
  | `tell-all` left to right | 4 |
  | `delegate` (C) passing the parent as self | 1 |
  | `delegate_c3` passing the parent as self | 1 |
  | `tell` not halting | 7 |
  | `otherwise` without `else` returning invalid | 1 |

- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (98 tests, 45 s).

### Blockers
None.

### Next
Item 02, `chez.py`. The plan's "Numbers" and "Lists" tables list what it needs. Capture
the extra vectors (exact zero products, contagion, exact sqrt/exp/log/expt,
`float(Fraction)` rounding, `math.tanh` against Chez) with a script in `python/oracle/`.
`object_prototypes.chez_map1` is a first `map_` to promote. Item 03 builds `objects.py`
from candidate C3 and moves `name_mapping.py` into the package as `metacat/names.py`.


---

## Iteration 3 — 2026-10-03 19:05

### Completed
Item 02, `chez.py`: **SOLVED**.
- **Vectors from Chez.** `python/oracle/batteries/chez-battery.scm` (new, 63 tests) holds
  the vectors that the frozen `tests/diff/` batteries lack. `capture.py` now also finds
  batteries in `python/oracle/batteries/`; a local battery's own file joins its `SOURCES`,
  so the other ten batteries' `SOURCES` are unchanged. It is captured into
  `python/fixtures/chez/` (1.9 MB) through the same unedited diff-eval.ss, and the slow
  re-capture freshness test covers it. Contents:
  - the generator: the value of every draw and the state after it (15 seeds × 54
    arguments, including the 4-step path for n > 2^32 − 1), a 1,500-draw Metacat-like
    run from seed 3852097033, and the bad seeds and arguments;
  - arithmetic: `+ - * / max min` over 12 × 12 exact, inexact, signed-zero and infinite
    operands, called both as procedure values and as inlined primitives (they agree);
    n-ary and unary forms, `quotient`/`remainder`/`modulo`, the predicates, `1+`/`-1+`;
  - rounding: Chez's own (`#%round` …) and utilities.ss's exact versions;
  - `exact->inexact` on 400 random ratnums;
  - `sqrt`/`exp`/`log`/`tanh` on 237 exact and 161 flonum arguments, `tanh` on the 601
    mapping strengths, and `expt` as a 23 × 23 table, 40 extra cases and the model's
    own shapes;
  - the printer: `number->string` on 3,000 doubles from random bits, 300 per decade,
    5,240 near ties and the edge cases; `write`/`display` of every character, string and
    symbol code point below 256 and a few beyond; symbol specials; `display`, `write`,
    `~a` and `~s` on nested data and quote abbreviations; `format` errors; `printf`;
  - lists: `map` with 1–4 lists up to 31 elements and length mismatches, `for-each` and
    `andmap`/`ormap` values, `sort` results and predicate-call logs up to 150 elements
    (`<`, `<=`, `>`), on pairs and presorted lists; `remq`/`remv`/`remove`,
    `memq`…`assoc`, `eqv?`/`equal?`;
  - top-level values.
- **Tests first.** `python/tests/test_chez.py` (62 tests covering all 63 fixtures, plus `scheme_canon.py`:
  helpers.scm's `b:canon`/`b:num` for Python values). It rebuilds each battery
  expression in Python, in the same order of draws (Chez's `map` where the battery uses
  `map`), and compares canonical text with the fixture. Besides the new fixtures, it
  covers the Chez-level tests of the utilities battery: `rng-*`, `map*-order`,
  `for-each*`, `andmap`/`ormap-order`, `sort-*`, `number->string-*`, `format-*`,
  `printf-output`, `remq`/`remv`/`remove`, `1+` and `rounding`. I wrote the tests before
  `chez.py` existed and ran them: collection failed with `ImportError: cannot import name
  'chez'`, so every test failed. The capture script came first. Its first run showed
  two battery mistakes, which I fixed in the battery, not in the fixtures: a `b:seeded`
  result used as a list, and literal bad `format` strings, which become compile-time
  warnings that diff-eval reports as ERROR.
- **`python/metacat/chez.py`.** The first version passed 55 of 60. What the Chez vectors
  then taught (all now in `docs/anomalies_and_quirks.md`):
  - Chez prints a double that lies exactly halfway between the two shortest digit
    strings with the **upper** one; Python's `repr` rounds half to even
    (1586243275893042.25 → `…423e15` vs `…422e15`). `_flonum_digits` corrects `repr`.
    I added the `number->string-ties` test so the rule is pinned (17 ties among 5,240).
  - In symbols, Chez writes non-ASCII characters that aren't R6RS constituents as
    `\xHH;` (U+0080–U+00A0, «, », soft hyphen, U+2028/9, U+FEFF). racket/compat.rkt
    writes them as is, which is harmless for the model.
  - `expt`: only a `1/2` power is an exact root (`(expt 8 1/3)` → `2.0`); exact base 1 → `1`;
    exact base 0 → `0` for positive powers, `1.0` for `0.0`, an error for negative ones;
    `0.0` to a negative power → `+inf.0`; negative base to a non-integer power →
    `exp(p log b)`, bit-equal to Chez. I added `expt-extra` after these findings.
  - Exact 0 is the identity of `+` and `-`: `(+ 0 -0.0)` → `-0.0`, `(- 0 0.0)` → `-0.0`
    (Python: `0.0`). This corrects the plan's "`+` and `-` agree".
  - `set-top-level-value!` binds an unbound name in Chez (Racket raised).
  - Python raises where Chez gives infinities (`1/0.0`, `0.0**-1`, `math.log(0.0)`,
    `round(inf)`).
  - Confirmed equal: `math.tanh`, `math.exp`, `math.log`, `math.sqrt`, `**` (libm `pow`)
    and `float(Fraction)` are bit-equal to Chez on every vector.
  Representations: symbols and Scheme strings are `str`. `chez.String` and `chez.Char`
  mark a string or character only where `write` must tell it from a symbol;
  `chez.Pair` is a pair with a non-list cdr; `chez.Vector` is a vector that must print
  `#(...)`; `None` is void and prints `#<void>`.
- **Mutation checks** (each applied to chez.py, run, then restored; `cmp` confirmed):
  all 22 caught.

  | Mutation | Failing |
  |---|---|
  | LCG multiplier 72931 → 72933 | 17 |
  | float draw takes 3 bits of s1, not 4 | 11 |
  | int draw takes the low half of s2 | 10 |
  | exact 0 × flonum gives `0.0` | 3 |
  | `(+ 0 x)` through Python | 2 |
  | `max` without inexact contagion | 2 |
  | one-list `map` left to right | 6 |
  | list merge sort: first half first | 2 |
  | `sorted()` for 25+ elements | 3 |
  | `remq` removes the first occurrence only | 1 |
  | `for-each` returns void | 2 |
  | printer without the tie rule (plain `repr`) | 3 |
  | exponent written `e+21` | 10 |
  | positional layout up to e = 10 | 7 |
  | `write` abbreviates `(quote x)` | 2 |
  | every non-ASCII symbol character as is | 1 |
  | `round` half away from zero | 2 |
  | flonum^int by repeated multiplication | 2 |
  | `tanh` from `exp` | 1 |
  | `set-top-level-value!` raises when unbound | 1 |
  | `(eqv? 0.0 -0.0)` true | 2 |
  | `float(Fraction)` as `float(n)/float(d)` | 1 |

- Speed, for item 12: `random(1.0)` 0.61 µs, `random(7)` 0.30 µs, `map_` over 10
  elements 0.53 µs, `sort` of 10 elements 2.5 µs, `mul(Fraction, int)` 2.3 µs,
  `div(37, 100)` 0.66 µs, `number_to_string` 4.8 µs.
- Docs: `docs/python-translation-plan.md` corrected. `+`/`-` at signed zeros, `expt`,
  the printer's ties, `None` printing `#<void>` (the plan said "nothing visible"), the
  `String`/`Char`/`Pair`/`Vector` representations, and `set_top_level_value_bang`.
  `python/README.md` covers the local batteries and chez.py. `test_fixtures.py` now
  expects 10 frozen batteries plus `chez`.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (163 tests, 53 s).

### Blockers
None. Out of scope, by design: Chez's exact complex numbers (`1+2i`) have no Python
counterpart and never occur in Metacat. The two fixture values that contain one are
taken as is (`scheme_canon.Raw`), and `sqrt` of a negative perfect square raises
`NotImplementedError`. `random` of a bignum range (beyond 2^60 − 1) is also
unsupported, since Metacat never draws one. Procedures print as `#<procedure>`, without
Chez's names.

### Next
Item 03: `objects.py` (candidate C3), `sugar.py` and `utilities.py`, against all 197
utilities tests. Use `chez.map_` for `tell-all` and every `map` with effects, and
`chez.sort`/`chez.remq`. Bind `utilities.round_` etc. to `chez.exact_round`…`exact_truncate`
(they already pass the `rounding` fixture). Use `chez.add1`/`sub1` for `1+`/`-1+`, and
`chez.format_`/`printf`. Use `chez.mul`/`div`/`max_`/`min_` wherever an operand can be a
flonum, `chez.expt`/`sqrt`/`exp`/`tanh` everywhere. `scheme_canon.canon` replaces the
prototype tests' minimal canon. Move `name_mapping.py` into `metacat/names.py`.

---

## Iteration 4 — 2026-10-03 19:28

### Completed
Item 03, objects, sugar and utilities: **SOLVED**.
- **Tests first.** `python/tests/test_utilities.py` has one case for each of the 197 tests
  of `tests/diff/utilities-battery.scm`. `CASES` maps each test name to a Python
  rebuild of the battery expression, with the same order of draws and effects, and
  compares its `b:canon` text with the frozen fixture (`ERROR` fixtures must raise
  `chez.SchemeError`). `test_every_battery_test_is_translated` checks that the names
  equal the MANIFEST's 197. The battery's own definitions (`make-fake`, the stand-ins
  for `make-slipnode`, `establish-link`, `make-codelet-type`, `*coderack*`,
  `*control-panel*`, `%verbose%`) are translated in the test. `engine_module(...)` puts
  them on `metacat.setup`/`slipnet`/`coderack`: on the real module once it exists, on a
  temporary one until then. I wrote the whole file, with the API it expects, before any
  of the three modules existed. Run: collection failed with `ImportError: cannot import
  name 'objects' from 'metacat'`, so all 197 cases failed. Structural tests are in the
  same file: every `define` of utilities.ss and syntactic-sugar.ss, and each of the 22
  extend-syntax forms, has its mapped Python function; every public function's docstring
  names its origin; there are no tkinter imports; `self` is the receiver through
  delegation; forwarders; `report_error_and_halt` can be replaced; an escape is caught
  only by its own `continuation-point*`, and fails after the form returns.
- **Code.**
  - `python/metacat/objects.py`: candidate C3 (`SchemeObject`, `@message`, `tell`,
    `delegate(self, msg, args, *parents)`, `delegate_to_all`, `tell_all`,
    `base_object`, `Lambda`, `Forwarder`, `procedure_p`, `INVALID`, `Reset`).
  - `python/metacat/sugar.py`: every extend-syntax form as a function, with thunks for
    delayed bodies. `stochastic_if_star` draws exactly one `(random 1.0)`, before the
    probability. A form with several patterns is one function per pattern. Free names
    are read from the engine modules at call time.
  - `python/metacat/utilities.py`: utilities.ss function for function, in the file's
    order.
  - `name_mapping.py` moved into the package as `python/metacat/names.py`; the test
    helper keeps only `original_names()`.
  - `chez.py` additions: `ExactComplex`, `make_rectangular`/`real_part`/`imag_part`
    (exact `3+4i`, needed by `coord`), `make_vector`, `string_to_number`, `atan`.
    `scheme_canon` prints `ExactComplex`.

  After the first full write, all 197 cases passed on the first run. The two
  non-fixture tests that failed were wrong expectations in my own tests:
  - the no-GUI check grepped the source text, and the module docstring says "never
    imports tkinter" (it now walks the AST, as test_chez.py does);
  - the delegation test expected a parent *without* an `else` clause to answer invalid.
    It answers void, as in Chez, and the test now checks both kinds.
- **Mutation checks** (`/tmp/mutate.py`, not kept; each mutation applied to the module,
  then the file run, then restored). 40 mutations, all caught except one equivalent
  mutant:

  | Mutation | Failing |
  |---|---|
  | pairwise-map: map before the recursion | 3 |
  | cross-product: l1 first to last | 6 |
  | partition: insert first to last | 2 |
  | bounded-random-partition: insert in pick order | 2 |
  | tell-all left to right | 1 |
  | delegate passes the parent as self | 1 |
  | delegate-to-all left to right | 1 |
  | `~` draws the sign first | 1 |
  | `prob?` with `>=` | 1 (after utilities-extra, below; 0 before) |
  | `exists?` by Python truthiness | 1 |
  | remove-duplicates keeps the first | 2 |
  | weighted-index with `<=` | 1 |
  | average with Python `/` | 2 |
  | log10 without the 1e-15 nudge | 2 |
  | round-to-100ths with Python `round` | 1 |
  | all-same? by `==` | 1 |
  | map-leaves maps `'()` as a list | 1 |
  | flatmap / select-extreme map left to right | 1 / 1 |
  | sort-by-method with `sorted()` | 1 |
  | stochastic-pick without `exact->inexact` | 2 |
  | `sgn` 0 → 0 | 3 |
  | make-table default 0 | 2 |
  | rotate counterclockwise | 2 |
  | `event?` returns `#t` | 1 |
  | stochastic-if*: probability before the coin | 1 |
  | stochastic-if* with `<=` | 1 |
  | for* from/to exclusive | hangs (`(ascending-index-list 0)`, the faithful loop): caught by timeout |
  | for* returns void | 3 |
  | repeat* times returns a value | 1 |
  | continuation-point* catches any escape | 1 |
  | fizzle not reset | 1 |
  | say ignores `%verbose%` | 1 |
  | mcat without its fender | 3 |
  | link: label before length | 1 |
  | valid-number? accepts 0 | 2 |
  | tell does not halt on invalid | 2 |
  | make-rectangular keeps an exact 0 imaginary part | 1 |
  | select-extreme `assv` → `assoc` | 0: equivalent (both compare numbers by `eqv?`) |

  To kill the `prob?` survivor I added a local battery,
  `python/oracle/batteries/utilities-extra-battery.scm` (4 tests: `prob?-ties`,
  `stochastic-if*-ties`, `select-extreme-ties`, `misc`). It is captured into
  `python/fixtures/utilities-extra/` through the unedited diff-eval.ss and covered by the
  slow re-capture test. Unlike the 197, it was written *after* the code, and its Python
  cases passed at once. `test_fixtures.py` now expects the local batteries `chez` and
  `utilities-extra`.
- **Evaluation-order audit** of utilities.ss and syntactic-sugar.ss (calls or `let`s with
  two effectful parts):
  - `pairwise-map`'s `append`: the recursion first (fixture);
  - `stochastic-if*`: the coin, then the probability;
  - `for*` from/to: `exp1`, then `exp2`;
  - `~`: the size (`let`), then the sign;
  - `cross-product-filter-map`/`-map-filter`, `map-leaves` and `filter-map`: `cons` goes
    left to right; the recursion on the rest of l1 comes first (fixtures);
  - `partition`/`bounded-random-partition`: every pick, then the inserts in reverse;
  - `tell-all`, `delegate-to-all`, `flatmap`, `select-extreme`, `adjacency-map` and
    `weighted-average`: `chez.map_` order;
  - `sort-by-method`: two `tell`s as one predicate's arguments. Python goes left to right;
    they are pure for every sort key in the model (noted in the docstring).

  Every site carries a `# chez:` or `# 1.2:` comment (20 in the three modules).
- **Re-grep that the plan asked for** (`symbol?`, `string?`, `eq?` on string literals,
  `~s`). New finding: the graphics (`string?` in sgl-interpreter.ss:386/398/432,
  general-graphics.ss:418, fonts.ss:93, gui.ss:363) and rules.ss:269
  (`filter-out symbol?`) do tell strings from symbols. I logged it as an open hidden
  coupling, so the items for those files keep the distinction. No `eq?` on a string
  literal; `~s` only in run.ss's `no-prompt` error and fonts.ss's debugging.
- **Docs.**
  - `docs/anomalies_and_quirks.md`: three new entries: Python interns only
    identifier-like string constants (so `INVALID` must be one shared object; checked
    with two modules), Python's `complex` can't hold Chez's exact complex numbers, and
    the strings-vs-symbols coupling above.
  - `docs/python-translation-plan.md`: a new "As built (item 03)" subsection with the
    objects/sugar/utilities API decisions; the Names section points to `metacat/names.py`.
  - `python/README.md`: the new modules.
- Speed (for item 12): `tell` 135 ns, delegated once 247 ns, twice 367 ns, child+parent
  creation 111 ns, `prob_p` 0.64 µs, `stochastic_if_star` 0.75 µs.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (379 tests, 36 s).

### Blockers
None. Deliberately left for later items:
- arithmetic on `ExactComplex` and complex coordinates (`magnitude`, `+` on coords);
  the graphics need it;
- `ask` (the REPL prompt; no run uses it) is translated minimally: symbols and numbers on
  one line, not a full Scheme reader.

### Next
Item 04 onwards: model modules import `from metacat.utilities import tell, prob_p, ...`
and `from metacat import sugar`, and always call `sugar.fizzle()` qualified. setup.py
must define `p_verbose` and `g_control_panel`, coderack.py `g_coderack` and
`make_codelet_type`, slipnet.py `make_slipnode` and `establish_link` (sugar reads them
there). `slipnet_node_list_star(specs, module=slipnet)` and
`codelet_type_list_star(specs, module=coderack)` also set the module attributes.
Objects return `objects.INVALID` (never a spelled-out string) from `otherwise`. Run
drivers replace `objects.report_error_and_halt`.

---

## Iteration 5 — 2026-10-03 20:19

### Completed
Item 04, constants, setup, coderack and descriptions: **SOLVED**.
- **Tests first.** `python/tests/test_coderack.py` has one case for each of the 43 tests of
  `tests/diff/coderack-battery.scm`. Each case rebuilds the battery expression in Python
  with the same order of draws and effects. Every battery `map` is `chez.map_`, since
  several of them reset and post inside the map. Each case compares its `b:canon` text with
  the frozen fixture. The battery's fakes are translated in the test: the logging window,
  workspace, themespace, trace, top-down nodes, proposed structures, descriptions and the
  settings alists. So are its top-level forms between tests (installing the fakes before
  `urgency-value-table`, resetting the modes before `threshold-distributions`), and they
  run before the test they precede. The cases run in the battery's order in one engine,
  as the battery does. Globals of modules not translated yet (`*workspace*`, `*themespace*`,
  `*trace*`, `*top-down-slipnodes*`, run.ss's `*display-mode?*`/`*step-mode?*`/
  `%step-cycles%`) are stand-ins from `tests/engine_stubs.py`. That file is
  `engine_module`, moved out of test_utilities.py, which now imports it. Other tests check
  that:
  - every model `define` of the four files has its Python name;
  - the codelet types are module attributes and top-level values;
  - the four description types have procedures;
  - `set_global` rejects unknown names;
  - docstrings name their origin;
  - no module imports tkinter.

  I wrote the file before any of the modules existed and ran it: collection failed with
  `ImportError: cannot import name 'engine' from 'metacat'`, so every case failed.
- **Code** (all new):
  - `metacat/constants.py`: the threshold distributions;
  - `metacat/view_globals.py`: the colours, fonts and speed settings the model reads, `#f`
    until the views set them, as in racket/engine/view-globals.rktl;
  - `metacat/setup.py`: the globals and user commands (`setup` and `enable-resizing` are
    the GUI's, item 15);
  - `metacat/coderack.py`: codelet types, codelets (a `Codelet` class that keeps its type's
    closure as `owner`), bins, the coderack, posting probabilities and counts, bottom-up
    and top-down posting. `load()` makes `*codelet-types*` through
    `sugar.codelet_type_list_star(..., module=coderack)`, then the three type lists and
    `*coderack*`;
  - `metacat/descriptions.py`: `make-description`, the four codelet procedures (installed
    by `load()`), `propose-`/`build-description`, `descriptions-equal?`,
    `description-member?`;
  - `metacat/engine.py`: `LOAD_ORDER` (metacat.ss's order), `load()` (each module's
    `load()`, once) and `set_global`/`get_global` by Scheme name.

  After the first write, the 43 cases (50 tests) passed on the first run. The only failure
  was in the test itself (`manifest()` returns a tuple).
- **Mutation checks** (`/tmp/mut/mutate*.py`, not kept; each mutation applied, the test
  file run with `-x`, then the file restored; `git status` confirmed it was clean). 37
  mutations; all caught except the equivalent ones:

  | Mutation | Result |
  |---|---|
  | bin add-codelet appends instead of consing | caught |
  | choose-random-codelet picks a wrong index | caught |
  | remove-codelet without the swap | caught |
  | choose-codelet over the bins reversed | caught |
  | delete-codelets over the codelet list reversed | 4 failing |
  | `get-coderack-bin` `>= 100` → `> 100` | 17 failing |
  | post's overflow test `=` → `>` | 2 |
  | deferred `>= 100` → `> 100` | 1 |
  | excess deferred codelets not random-picked | 1 |
  | add-deferred-codelet appends | 4 |
  | rule-scout probability exact 1/2 instead of 0.5 | 2 |
  | jootser probability 0.25 | 2 |
  | jootser bottom-up urgency | 3 |
  | thematic count `floor` instead of `round` | 1 |
  | `post-codelet-probability`'s missing else gives 0 instead of void | 2 |
  | unclamp without reset-urgencies | 2 |
  | time stamp `*codelet-count*` + 1 | caught |
  | plural label on the first line | 1 |
  | codelet print without `round` | 2 |
  | "(scope is ...)" for one argument | 1 |
  | description counted as a proposed structure | 2 |
  | top-down slipnodes not told | caught |
  | delete-codelets does not decrease the count | 4 |
  | a distribution weight changed | 1 |
  | verbose-on's test inverted | 1 |
  | `blank-window` given a symbol instead of a string | 1 |
  | `%eliza-mode%` default `#f` | 1 |
  | descriptions-equal? ignores the descriptor | 2 |
  | description-member? returns the element | 1 |
  | **adjust-urgency with Python `min`/`max`** | **0 at first**: see below |
  | urgency table with an exact exponent `/15` | 0: equivalent (same rounded table) |
  | `get-coderack-bin` `<= 0` → `< 0` | 0: equivalent (urgency 0 maps to bin 0 either way) |
  | bottom-up posting `coin <= p`, p evaluated first | 0: equivalent (no draw in p; ties need coin = p exactly) |
  | clamp always re-applies | 0: equivalent headless (same urgencies) |
  | `initialize` returns `'done` literally | 0: equivalent |

  To kill the `min`/`max` survivor I added a local battery,
  `python/oracle/batteries/coderack-extra-battery.scm` (2 tests:
  `adjust-urgency-clipping`, with flonum and exact urgencies pushed past 0 and 100 by nine
  deltas, and `clamp-exactness`, clamping at 90 then 90.0). It is captured into
  `python/fixtures/coderack-extra/` through the unedited diff-eval.ss and covered by the
  slow re-capture test. Unlike the 43, it was written *after* the code; its Python cases
  passed at once, and the mutant now fails. `test_fixtures.py` now expects the local
  batteries `chez`, `coderack-extra` and `utilities-extra`.
- **Evaluation order.** No call or `let` in coderack.ss, setup.ss or constants.ss has two
  effectful parts (as porting-notes.md says for the Racket port). The draws are
  `stochastic-pick-by-method` (bins by urgency sum, deletion by removal weight), `random`
  (the codelet in a bin), `random-pick` (excess deferred codelets) and `stochastic-if*`
  in the posting loops. Those loops draw the coin before the probability, with a
  `# chez:` comment. descriptions.ss's `make-description` `let` is pure, and its comment
  says so. Racket's notes call that let right to left; item 02 measured left to right in
  a test lambda. The order doesn't matter here.
- **Docs.**
  - `docs/anomalies_and_quirks.md`: two Python traps. Python's `bool` is an `int`
    (`100 - False` is 100 where Chez raises), so arithmetic on values that may be `#f` goes
    through `chez.sub`… And pytest's diff of multi-MB strings takes minutes, so the battery
    asserts report the first differing character instead.
  - `docs/python-translation-plan.md`: a new "As built (item 04)" subsection.
  - `python/README.md`: the new modules.
- Speed (for item 12): choose-codelet + post on a coderack of 99 codelets: 31 µs; post into
  a full coderack, which deletes one codelet by removal weight: 275 µs.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (435 tests, 33 s).

### Blockers
None. Left for later items, by design:
- descriptions.py's `make-description`, its codelets, `propose-description` and
  `build-description` are translated but not yet run: they need the Workspace, Slipnet,
  formulas and themes. The workspace and codelet batteries (items 06–07) pin them.
- The codelet types' graphics methods (`highlight`, `draw-graphics`,
  `update-bar-graphics`, `draw-codelet-count`) are translated but call
  `general_graphics.solid_box` and the coderack window. The panels item (14) tests them.
- Every codelet type's coderack window is `#f` until `set-graphics-parameters`, as in the
  original. The headless driver (items 10–11) must install a null window in each,
  as the oracle prelude's `install-headless-windows!` does.

### Next
Item 05, slipnet and images: `slipnet.py` and `images.py`, each with a `load()` that
builds the nodes (`sugar.slipnet_node_list_star(specs, module=slipnet)`), the links and
`*top-down-slipnodes*`. engine.py calls them in metacat.ss's order. Its test should take
the battery's stand-ins from `tests/engine_stubs.py` and call `engine.load()` once.
`engine.set_global` finds any module in `LOAD_ORDER` that exists, stand-ins included.
Reach modules that don't exist yet through `_metacat.<module>` at call time.

---

## Iteration 6 — 2026-10-03 20:39

### Completed
Item 05, slipnet and images: **SOLVED**.
- **Tests first.** `python/tests/test_slipnet.py` has one case for each of the 47 tests of
  `tests/diff/slipnet-battery.scm`. Each case rebuilds the battery expression in Python
  with the same order of draws and effects (every battery `map` is `chez.map_`) and compares
  its `b:canon` text with the frozen fixture. The two `ERROR` fixtures
  (`relationship-between-one`/`-none`) must raise. The cases run in the battery's order in
  one engine. The battery's top-level forms (its logging monitor, the fake Themespace, the
  logging `temp-adjusted-probability`, the reset before the images) run before the test
  they precede. Globals of files not translated yet are stand-ins from
  `tests/engine_stubs.py`: run.ss's `%update-cycle-length%`, trace.ss's monitor, formulas.ss's
  `temp-adjusted-probability`, `*themespace*` and `*workspace*`. rules.ss's `format-slipnode`
  (for `reveal`) is a temporary top-level value. The battery covers:
  - the **initial slipnet dump**: all 59 nodes, every link list with its degrees of
    association, 202 links, the nodes and links as top-level values;
  - **20 calls of `update-slipnet-activations`** from 8 fixed states, plus 15 seeds and the
    start-of-run state. All activations, frozen flags and the generator state after every
    call are compared exactly.
  Other tests check that every `define` of slipnet.ss and images.ss has its Python
  function, that the nodes are module attributes and top-level values, that docstrings
  name their origin, that there is no tkinter import, and the load order. I wrote the file
  before either module existed and ran it: collection failed with
  `ImportError: cannot import name 'slipnet' from 'metacat'`.
- **Code** (new):
  - `metacat/slipnet.py`: `Slipnode`, `SlipnetLink`, the relations (`get-label`,
    `relationship-between`, `linked?` …), `update-slipnet-activations` and the platonic
    helpers. `load()` runs the file's top-level forms in order: the node lists, the
    top-down codelet types, intrinsic link lengths, descriptor predicates and the links,
    through sugar.py's link functions.
  - `metacat/images.py`: `Image`, `StringImage`, `change-length-first?` and
    `enumerate-letter`.

  All 53 tests passed on the first run.
- **Mutation checks** (`/tmp/mut5/mutate.py`, not kept; each mutant run against
  test_slipnet.py, then restored; `git status` confirmed). 37 mutants:
  - **caught (28):** decay with a float rate; flush without `min`; clamp/update-activation
    without the monitor; set-activation ignoring frozen; links appended; outgoing order;
    relationship-between without `all-same?`; similar property links without
    `temp-adjusted-probability`; apply-slippages without the CM-type test; top-down
    urgency without `%`; top-down posting without its coin; predecessor links ascending;
    `>` in above-threshold; `>=` in number->platonic; leftmost ignoring string-spanning;
    the `LettCtgy` short name; string replace-all left to right; new-start-letter's
    `tell-all` left to right; string reset keeping its direction; string
    `new-alpha-position-category` "fixed"; enumerate-letter without its relation check;
    image print's `>`; image reset keeping the swapped image; shorten `< 1`; leaf-walk
    not reversed; reverse-medium's start letter; change-length-first `<=`.
  - **killed by a new local battery (3):** Image's `replace-all` left to right, `extend`
    always changing the start letter first, and `number->platonic-number` without the
    `n` < 1 guard. `python/oracle/batteries/slipnet-extra-battery.scm` (4 tests:
    `image-replace-all-fail`, `extend-length-first`, `number-to-platonic-number-range`,
    `-zero`) is captured into `python/fixtures/slipnet-extra/` through the unedited
    diff-eval.ss, and the slow re-capture test covers it. It was written *after* the
    code; its Python cases passed at once, and the three mutants now fail.
  - **equivalent (6):**
    - the partially-active jump with its probability computed before the coin, or with
      `<=`: the probability draws nothing, and a tie needs coin = p exactly;
    - spread with `floor`: only fully active nodes spread, at activation 100, so the
      amount is an integer;
    - the shrunk link length with `floor`: 40% of 60, 0 and 80 are integers;
    - get-related-node taking the last match: on this slipnet the same-category node is
      always last (the four cases are leftmost, rightmost, left and right over Opposite);
    - new-length `n <= len` → `n < len`: both do nothing when `n` = `len`.
- **Evaluation order.** No call or `let` in slipnet.ss or images.ss has two effectful
  parts (as the Racket port found). The draws are the `stochastic-if*` in
  `update-slipnet-activations` and `attempt-to-post-top-down-codelets` (coin first,
  with `# chez:` comments), `prob?` in `get-similar-property-links` (filter order) and in
  `apply-slippages`. Order matters in images through `fail`: `replace-all` and `tell-all`
  use `chez.map_`.
- **A test-isolation trap**, found and fixed. pytest runs every file in one engine.
  test_utilities.py's slipnet-macro cases leave fake `plato-p`/`plato-q`/`plato-z` and
  links in the top level, so any later file would have seen them. A probe test showed
  it. test_utilities.py now restores `chez.TOP_LEVEL` in a module fixture, and so does
  test_slipnet.py, which also resets the slipnodes and the coderack at the end.
- **Docs.**
  - `docs/anomalies_and_quirks.md`: two Python entries, negative indexes wrapping where
    Chez raises, and the shared engine leaking fakes across test files.
  - `docs/python-translation-plan.md`: "As built (item 05)".
  - `python/README.md`: the new modules and tests.
  - `test_fixtures.py` now expects the local battery `slipnet-extra`.
- Speed, for item 12: `update-slipnet-activations` takes 224 µs (59 nodes, no themes),
  `get-related-node` 1.9 µs and `get-label` 1.1 µs.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (496 tests, 32 s).

### Blockers
None. Left for later items, by design:
- `Image`'s `instantiate-as-letter` and `instantiate-as-group` are translated but not run.
  They need workspace-objects.py's `make-letter`, groups.py's `make-group`,
  group-graphics.py's `make-group-pexp` and workspace.py's `%built%`, all read through
  the package. Items 06–07 and rules (item 17) exercise them.
- `draw-activation-graphics` is untested; it waits for the slipnet panel (item 14).

### Next
Item 06, workspace. Engine modules may now import `slipnet` directly (`slipnet.plato_a`,
`slipnet.relationship_between`). The workspace test needs `run.p_update_cycle_length`
only if it resets slipnodes; trace.py's monitor is still a stand-in. Test files that
change engine state should restore it when they end (see the new anomalies entry).
test_slipnet.py's module fixture is a model for that.

---

## Iteration 7 — 2026-10-03 21:04

### Completed
Item 06, the Workspace: **SOLVED**.
- **Tests first.** `python/tests/test_workspace.py` has one case for each of the 75 tests of
  `tests/diff/workspace-battery.scm`, with the procedures of `tests/diff/workspace-dump.scm`
  it loads translated in the test (`b:init-problem` as `init_problem`, the dumps,
  `b:update-workspace-values`). Each case rebuilds the battery expression in Python with
  the same order of draws and effects and compares its `b:canon` text with the frozen
  fixture. `tests/problems.txt` is read at run time the way the battery reads it (36
  problems, every seed). So the initial workspace of every problem is compared for every
  seed, and so are the generator state after each seed, the slipnet activations and the
  EEG messages. Other cases cover:
  - live queries on 10 problem shapes;
  - fakes for bonds, groups, bridges and rules;
  - strings' tables and storage expansion;
  - `update-temperature`, structure strengths and `wins-fight?`;
  - the formulas and `tanh`.

  Stand-ins (`engine_stubs.engine_module`) cover run.ss's `%update-cycle-length%` and
  `*temperature-clamped?*`, plus the battery's own fake `*themespace*`, logging `*EEG*`
  and `contains?`. The module fixture restores the setup and workspace globals, the top
  level and the slipnodes. Structural tests check that:
  - every `define` of the six files has its Python name;
  - docstrings name their origin;
  - no module imports tkinter;
  - the modules load in order and `*workspace*` exists after load;
  - building a workspace draws nothing.

  I wrote the file before any of the six modules existed and ran it: **90 failed, 7
  passed**. The 7 were the two meta tests, `ws-problem-count`, `ws-problem-rest` (an empty
  list) and the three `tanh` tests, which only exercise chez.py. The failures were
  `ModuleNotFoundError` for `metacat.workspace_strings` etc.
- **Code** (new): `metacat/workspace.py` (`Workspace`; `load()` makes `*workspace*`),
  `workspace_objects.py` (`Letter`, `WorkspaceObject`), `workspace_strings.py`
  (`WorkspaceString`), `workspace_structures.py` (`WorkspaceStructure`, `wins-fight?`),
  `workspace_structure_formulas.py` and `formulas.py`. Two subagents translated them into a
  staging directory while I wrote the tests, and I copied them into the package only after
  the failing run. I reviewed them against the originals. With the modules in place, all
  97 tests passed on the first run.

  One fix to item 04's code: descriptions.py's `print-name` returned `format`'s plain str,
  which `b:canon` prints as a symbol. It now returns a `chez.String`; the dumps print it
  as `"StringPos:lmost"`.
- **Mutation checks** (`/tmp/mut6/mutate.py`, not kept; each mutant applied, then
  test_workspace.py run with `-x`, then the file restored; `git status` confirmed). 38
  mutants in two batches:
  - **caught by the frozen battery (25):**
    - workspace objects: the 2/3 factor in a group, the 300 cap, the 1/3 and 1/6 bond
      factors, the 1/2 factor on a group's horizontal bridge;
    - unhappiness and salience: the target's horizontal unhappiness without justify mode,
      the justify-mode average with 2 terms, clamped salience 99, intra salience with
      `floor`, inter salience weights swapped, average salience not rounded, the target's
      justify-mode salience test inverted;
    - strings: bond-scan values reversed, capacity 2n+1, relative importance 1/(n+1),
      random letter reversed;
    - workspace: mapping not halved, tanh 1/50, `~` with `<=`, built bridge left out of
      the coincident bridges;
    - formulas: temp-adjusted-values exponent, temperature weights 60/40, an exact
      `low-prob-factor`, intrinsic-strength weights, challenger strength not updated, the
      length-description cases `> 4` and `= 2`.
  - **killed by a new local battery (8):** the 1/2 factor on a group's *vertical* bridge,
    choosing a description by depth instead of activation, neighbours reversed,
    relevance over n rather than n − 1 objects, `unrelated?` of a middle letter with
    `< 1`, the oldest structures taken as the youngest, an exact `(min 1 ...)` in
    `get-activity`, and the bond density compared as a float.
    `python/oracle/batteries/workspace-extra-battery.scm` (8 tests: `group-vertical-bridge`,
    `relevant-description-choices`, `neighbours-with-groups`, `relevance-with-bonds`,
    `unrelated-one-bond`, `density-boundaries`, `activity-and-ages`, `activity-float-ties`)
    loads the unedited workspace-dump.scm. It is captured into
    `python/fixtures/workspace-extra/` through the unedited diff-eval.ss and covered by the
    slow re-capture test. It was written *after* the code; its Python cases passed at once,
    and the mutants now fail. For the `(min 1 ...)` mutant I first searched every average
    age k/1, k/2 and k/3 up to 3000 for one where exact and flonum rounding differ: 545/2
    and 575/2.
  - **equivalent (2):** `(>= density 0.6)` → `>`, since no exact density equals the double
    nearest 0.6; and `(> compatibility 0)` → `>=`, since at compatibility 0 the thematic
    weight is 0.
- **A quirk of the original, found by the new battery:** `current-translation-temperature-
  threshold-distribution` compares an exact density with `0.8`/`0.6`/`0.4`/`0.2`. The
  doubles nearest 0.8, 0.4 and 0.2 lie above 4/5, 2/5 and 1/5, so those densities fall into
  the next-hotter class. `get-activity`'s `(min 1.0 ...)` has the same kind of edge. Logged
  in anomalies_and_quirks.md, with a Python entry: `Fraction`/`float` comparisons are exact,
  as Chez's are, and converting first would break them. Both sites carry `# 1.2:` comments.
- **Evaluation order.** No call or `let` in the six files has two effectful parts, as the
  Racket port found. The draws are:
  - `stochastic-pick`: `choose-object`, `choose-description-for-rule`, `wins-fight?`;
  - `stochastic-pick-by-method`: neighbours, descriptions, the leftmost object;
  - `random-pick` (`get-random-letter`), the bond-scan distribution, and `~` in
    `rough-num-of-objects` (the second `~` only when the first test fails).

  `wins-all-fights?` stops drawing at the first loss (`andmap`). In the battery, the one
  list with effects (`ws-string-names`, around `mark-as-translated`) is evaluated left to
  right, as the fixture shows.
- **Docs**:
  - `docs/anomalies_and_quirks.md`: the two entries above;
  - `docs/python-translation-plan.md`: "As built (item 06)";
  - `docs/porting-notes.md`: a correction to where three stand-ins are defined
    (`equivalent-workspace-objects?` is trace.ss's, `rule-describable-bridge?` rules.ss's,
    `break-bridge` bridges.ss's);
  - `python/README.md`: the new modules and tests;
  - `test_fixtures.py` now expects the local battery `workspace-extra`.
- Speed, for item 12: building the initial workspace of `abc abd xyz` takes 1.6 ms,
  `update-workspace-values` 1.1 ms, and the Workspace's `choose-object` 24 µs.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (605 tests, 37 s).

### Blockers
None. Left for later items, by design:
- Translated but not run yet, because they need later files:
  - bond and group equality (`get-equivalent-bond`/`-group` call bonds.ss's and groups.ss's
    predicates);
  - `get-equivalent-bridge` (bridges.ss's `bridge-between?`);
  - `delete-invalid-string-position-middle-descriptions` (`break-bridge`);
  - `get-reference-objects` (`verbatim-clause?`);
  - `get-real-object` (trace.ss);
  - `delete-proposed-structure` with graphics on;
  - workspace objects' `print` (`full-workspace-object-name`).

  Items 07–09 and the golden runs exercise them.
- `*temperature-clamped?*` is read as `_metacat.run.g_temperature_clamped_p`; run.py (item
  10) must define it (`False`), since run.ss creates it with `set!`.
- `*EEG*` is `_metacat.eeg_graphics.g_EEG`; the headless driver must give it a null object
  until eeg_graphics.py exists.

### Next
Item 07: bonds, groups and concept mappings. Groups delegate to
`workspace_objects.make_workspace_object(...)`, as letters do, and to
`workspace_structures.make_workspace_structure()`. The workspace modules already reach
`_metacat.bonds.same_bond_category_p`, `_metacat.groups.same_group_category_p`/
`same_group_direction_p` and `contains_p` through the package. test_workspace.py's
`init_problem` and stand-ins are a model for building a real initial workspace in the
bond and group tests.

---

## Iteration 8 — 2026-10-03 21:23

### Completed
Item 07, bonds, groups and concept mappings: **SOLVED**.
- **Tests first.** `python/tests/codelet_harness.py` translates `tests/diff/codelet-harness.scm`
  (the bonds-and-groups setting: the restricted run-mcat loop, `b:step`, `b:structures`,
  `b:difference`, `b:compact`, the fakes and recording monitors). It also translates
  codelet-battery.scm's `b:problem-trace`, `b:long-trace`, `b:cm-data`, `b:category-cm-test`
  and `b:workspace-cm-test`. The workspace-dump.scm procedures come from test_workspace.py.
  `python/tests/test_codelets.py` covers all 59 tests of `tests/diff/codelet-battery.scm`:
  - 36 problems × every seed × 400 codelets;
  - seven 2000-codelet runs that reach group-builder's sameness consolidation and its
    ungated `group-graphics 'erase`;
  - the concept mappings of every category and of real descriptions after 600 codelets.

  A failure names the first differing trace line (problem, seed, codelet). The full battery
  runs in a fork pool (`multiprocessing`, fork after `engine.load()` and the harness
  set-up) and is in the slow tier; it takes about 6 s on 32 cores. The fast tier runs the
  first seed of problem 0 against the start of its fixture, plus two concept-mapping
  cases. Structural tests check:
  - every `define` of the three files has its Python name;
  - docstrings name their origin;
  - there is no tkinter import (group_graphics.py included);
  - the codelet procedures are installed;
  - the modules load in metacat.ss's order;
  - the traces reach every enabled codelet type, structures built and broken, and
    `(window caching-on)`, as codelet-diff-test.rkt checks.

  I wrote both files before any of the modules existed and ran them:
  - fast and structural tier: 15 failed (`AttributeError: module 'metacat' has no
    attribute 'groups'`, `ModuleNotFoundError: metacat.bonds`);
  - battery: 57 failed, 2 passed. The two passing cases were `codelets-problem-count`
    and `cm-category-count`, which need no new module.
- **Code** (new): `metacat/bonds.py`, `metacat/groups.py`, `metacat/concept_mappings.py`,
  and `metacat/group_graphics.py`, which holds only `group-graphics` (the model calls it
  ungated); the rest of that file is the panels item's. Two subagents translated them
  into a staging directory while I wrote the harness, and I copied them in after the
  failing run. With them in place, **all 59 battery cases matched Chez byte for byte on
  the first run**, and so did the full suite. A spot check: the 2000-codelet
  `aaabaaa` seed 1 trace is 863,524 characters, equal to the fixture, and takes 3.0 s.
  One review fix: group_graphics.py read `*workspace-window*` once per call; it now reads
  it at each use, as the original does.
- **Mutation checks** (`/tmp/mut7/mutate.py`, not kept; each mutant applied, then
  test_codelets.py run with `-x`, then the file restored; `git status` confirmed). 21
  mutants:
  - **caught by the frozen battery (15):**
    - groups: left groups' objects not reversed; the local-support factor 0.6 → 0.5;
      propose-group's neighbours left first (the Chez `append` site); group-evaluator's
      test inverted; the group fights not stopping at the first loss; the
      length-description test inverted; scan-bonds not reversed; the evaluation sigmoid
      /5 → /6;
    - bonds: the local-support factor; bond local density right neighbours first;
      bond-evaluator drawing a second coin; `bond-degree-of-assoc` 11 → 10;
    - concept mappings: strength and slippability without the square; the concept
      pattern always including the label.
  - **killed by a new local battery (3):** group and bond local density floored instead of
    rounded, and group-builder's flipped-bond `map` left to right.
    `python/oracle/batteries/codelet-extra-battery.scm` has 13 tests:
    - `local-densities-*` (7): after a harness run, every built bond's and group's local
      density and support, drawn three times under a seed. I found the problems with 2/3
      bond densities first by a Python search.
    - `group-builder-flips-1..6`: group-builder on a predgrp whose three bonds are the
      flipped versions of built successor bonds.

    It loads the unedited codelet-harness.scm, is captured into
    `python/fixtures/codelet-extra/` through the unedited diff-eval.ss, and the slow
    re-capture test covers it. I wrote it *after* the code; its Python cases passed at
    once, and the mutants now fail. The density cases are in the slow tier.
  - **equivalent here (3):**
    - `get-highest-level-coincident-group` `>` → `>=`: only reached for drawn groups, so
      it is graphics-only;
    - bond importance 50/100 swapped: only bridges read it (item 08);
    - the bond's left/right `<` → `<=`: two bonded objects never share a position.
- **Evaluation order.**
  - groups.ss's `get-local-density`: `(append left right)` draws the right neighbours
    first (racket/engine/groups.rktl:371).
  - bonds.ss's `get-local-density` is a `let*`, left first.
  - group-builder's `adjacency-map`s and flipped-bond `map` use `chez.map_`; its fights
    use `andmap` (they stop drawing at the first loss).
  - Every `stochastic-if*` draws its coin before the probability. In
    top-down-group-scout:category, the probability draws too (`get-local-support`), after
    the coin.
  - Everything else with two effectful parts is a `let*` or a sequence. The remaining
    argument lists (scout weights, `append`s of incompatible bridges) are pure.
  - 21 `# chez:`/`# 1.2:` comments.
- **Docs**:
  - `docs/anomalies_and_quirks.md`: an update to the `same-direction?` entry
    (`bonds.same_direction_p` raises `UnboundVariable`), and a new entry: the battery's
    cases give Chez's traces in fresh forks, evidence for the fork-after-load golden
    runner.
  - `docs/python-translation-plan.md`: "As built (item 07)".
  - `python/README.md`: the new modules and tests.
  - `test_fixtures.py` now expects the local battery `codelet-extra`.
- Speed, for item 12: about 1.5 ms per codelet in the harness, dumps included; the fast
  tier takes 3.5 s.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (697 tests, 58 s).

### Blockers
None. Left for later items, by design:
- Translated but not run yet: the bridge paths of bonds and groups (incompatible bridges,
  `break-bridge`, `incompatible-*-CMs?`) and bond importance. Item 08's bridge battery
  (the harness with `b:bridges?`) exercises them.
- The rest of group-graphics.ss: `make-group-pexp`, `draw-group-grope` and the arrowhead
  constants. Groups' graphics methods reach them only with `%workspace-graphics%` on; the
  panels item (14) translates them.

### Next
Item 08, bridges and breakers. Extend `tests/codelet_harness.py` with the `b:bridges?`
setting (`b:enable-bridges!`: the bridge and description scouts, breaker, the extra
top-down nodes, the bridge part of `b:structures` and `b:proposed-counts`). Its
`structures()` already lists bridges. bridges.py must provide `break_bridge`,
`incompatible_horizontal_CMs_p`, `incompatible_vertical_CMs_p` and `bridge_between_p`
(read through the package by groups/bonds/workspace), and the harness's stand-in trace
module already has `monitor_new_concept_mappings`. The fork-pool pattern of
test_codelets.py carries over to the bridge battery.

---

## Iteration 9 — 2026-10-03 21:57

### Completed
Item 08, bridges and breakers: **SOLVED**.
- **Tests first.**
  - `python/tests/codelet_harness.py` now has the bridges setting of
    `tests/diff/codelet-harness.scm`:
    - `enable_bridges` (b:enable-bridges!): the bridge, description and breaker bottom-up
      types, plus *top-down-slipnodes* with StrPosCtgy, AlphaPosCtgy and Length;
    - bridge scouts among the initial codelets;
    - `proposed_counts` with the proposed bridges and `description_counts`.
  - `python/tests/test_bridges.py` covers all 46 tests of `tests/diff/bridge-battery.scm`:
    - 36 problems × every seed × 1000 codelets;
    - nine bridge matrices (`b:fresh-bridge-data`, `b:direction-cm-data`,
      `b:bridge-pair-data` translated).

    The full battery runs in a fork pool in the slow tier (14 s on 32 cores). The fast tier
    runs the first 300 codelets of problem 0.

    Structural tests check:
    - every `define` has its Python name;
    - docstrings name their origin;
    - there is no tkinter import;
    - the five codelet procedures are installed;
    - the load order;
    - the fixtures reach every bridge, description and breaker type, plus bridges built,
      broken and flipped, `add-theme` and `new-cms`.

    I wrote both before either module existed and ran them: the fast and structural tier
    gave **9 failed, 1 passed** (`ModuleNotFoundError: metacat.bridges`; the codelet run
    hit `TypeError: 'bool' object is not callable`, a codelet type without a procedure).
    The battery gave **45 failed, 1 passed** (`bridges-problem-count`). The item-07 fast
    tier still passed with the extended harness.
- **Code** (new): `metacat/bridges.py` and `metacat/breakers.py`. A subagent translated them
  into a staging directory while I wrote the tests, and I copied them in after the failing
  run. The first run then stopped at `metacat.themes` having no `bridge_type_to_theme_type`.
  Every bridge calls three themes.ss helpers ungated (anomalies: "Bridges call themes.ss on
  every bridge"), so the harness's `STAND_INS["themes"]` now carries test-side translations
  of `bridge-type->theme-type`, `descriptions-affect-themespace?` (with
  `ignore-descriptions?`) and `bridge-theme-compatibility-sigmoid`. They are the real
  definitions in the oracle, and themes.py replaces them later. After that, **all 46 battery
  cases matched Chez byte for byte on the first run** (47 MB of traces). A spot check
  confirmed that `bridges-05` is 1,436,094 characters, equal to the fixture.
- **Mutation checks** (`/tmp/mut8/mutate.py`, not kept; each mutant applied, then
  test_bridges.py run with `-x`, then the file restored; `cmp` with the staged copy
  confirmed). 16 mutants:
  - **caught by the frozen battery (13):**
    - the number-of-mappings factors (0.8 → 0.9, 1.6 → 1.5);
    - the singleton-letter factor 0.1 → 0.2;
    - the scout's slippage product without `1-`;
    - the evaluator's probability without `1-`;
    - the bond fight weights 3/2 swapped;
    - no ObjCtgy mapping added in build-bridge;
    - break-bridge leaving the bridge in the Workspace;
    - a bridge's letter span counting only object1;
    - the external strength cap 100 → 99;
    - the breaker picking the first structure, using p1 instead of p1·p2, and an inverted
      temperature test.
  - **killed by a new local battery (1):** the vertical bridge's internal-coherence factor
    2.5 → 2.0. `python/oracle/batteries/bridge-extra-battery.scm` (1 test,
    `fresh-bridges-abc-abd-glz-2`) loads the unedited codelet-harness.scm with bridges on,
    runs abc abd glz with seed 2 for 1500 codelets, and dumps every fresh vertical bridge's
    relevant distinguishing mappings, coherence and internal strength. The a–z bridge is
    coherent at 75. I found the case with a Python search first. The battery is captured
    into `python/fixtures/bridge-extra/` through the unedited diff-eval.ss and covered by
    the slow re-capture test. I wrote it *after* the code: its Python case passed at once,
    and the mutant now fails.
  - **not killed (2):**
    - the *horizontal* coherence factor: a search of every problem and seed at seven points
      (100–1500 codelets) found no coherent horizontal bridge under the cap (new anomalies
      entry);
    - the spanning-bridge theme boost ×2 → ×1: the harness's fake Themespace never returns
      a theme, so there is nothing to boost. The themes item (10) covers it.
- **Evaluation order.** No site needed reordering. The subagent's audit agrees with the
  Racket port, which has no `port:` changes in bridges.rktl or breakers.rktl:
  - every draw sits in a `let*`, a body or an `and`/`cond`. The draws are the
    stochastic-picks of the bridge type and objects, `stochastic-if*` (coin first: bridges.ss
    935, 1029, 1166, 1432; breakers.ss 22, 38, 41), `random-pick` and the fights;
  - the multi-argument calls and multi-binding `let`s only read (the appends of the
    incompatible bridges, `wins-all-fights?`'s and `make-concept-mapping`'s arguments, the
    breaker's p1/p2);
  - maps use `chez.map_`;
  - `cross-product-for-each` in `boost-themes` keeps utilities.ss's order.

  bridges.py has 16 `# chez:`/`# 1.2:` comments and breakers.py 3.
- **Docs**:
  - `docs/anomalies_and_quirks.md`: an update to "Bridges call themes.ss on every bridge"
    (the Python harness's stand-ins) and a new entry, "A horizontal bridge's
    internal-coherence factor never shows in the batteries";
  - `docs/python-translation-plan.md`: "As built (item 08)";
  - `python/README.md`: the new modules and tests;
  - `test_fixtures.py` now expects the local battery `bridge-extra`.
- Speed, for item 12: the 530 runs of 1000 codelets take 217 s of CPU, about 0.4 ms per
  codelet including the harness's dumps.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (759 tests, 63 s).

### Blockers
None. Left for later items, by design:
- Translated but not run yet:
  - the theme paths of bridges (`incompatible-with-theme?`, `supported-by-theme?`,
    `supports-theme-pattern?` through `_metacat.trace.entries` and
    `_metacat.justify.remove_whole_or_single_concept_mappings`), the spanning boost;
  - the graphics paths (`bridge_graphics`, `draw_bridge_grope`, `new_bridge_label_number`,
    `activate-concept-mapping-graphics`), all behind `%workspace-graphics%`.
- The themes.ss helpers live in the test harness's stand-ins. themes.py (item 10) must define
  `bridge_type_to_theme_type`, `descriptions_affect_themespace_p`, `ignore_descriptions_p`
  and `bridge_theme_compatibility_sigmoid`, and the headless driver needs them.

### Next
Item 09 (rules and answers, per iterations.md). Extend `codelet_harness.py` with the
`b:rules?` setting (`check-if-rules-possible`, the original `add-bottom-up-codelets`, the
snag-period end with rule-battery.scm's fake Trace, `b:answered` set by `suspend`, and
`b:rule-entry`/`b:datum` in `structures`). The fork-pool pattern of test_bridges.py carries
over. rules.ss reaches `equivalent-workspace-objects?`, `find-next-space-position`, themes.ss's
`diff` and the Themespace's `get-dominant-theme-pattern` (anomalies: "Rules and answers lean on
later files"). Give them harness stand-ins as item 08 did for the themes helpers.

---

## Iteration 10 — 2026-10-03 22:28

### Completed
Item 09, rules and answers: **SOLVED**.
- **Tests first.**
  - `python/tests/codelet_harness.py` now has the rules setting of
    `tests/diff/codelet-harness.scm`:
    - `RULES`/`ANSWERED` (b:rules?, b:answered);
    - `datum`, `rule_data`, `rule_entry`, with the rules in `structures`;
    - `update_everything` with `check-if-rules-possible`, the end of a snag period through
      the Trace and the original's `add-bottom-up-codelets`;
    - a run that stops after the codelet that reports the first answer.
  - `python/tests/test_rules.py` covers all 50 tests of `tests/diff/rule-battery.scm`:
    - 36 problems × every seed, up to the first answer or 2500 codelets;
    - `first-answers`, put together from the problems' runs in battery order;
    - twelve rule matrices: apply-rule with ignore-snag, translate, and the generator state
      after each rule.
  - The battery's fakes are translated in the test: the Trace, Memory, answer and snag
    events, abstract descriptions, monitor-new-rules, the Commentary window, suspend and
    answer-justifier's procedure. So are verbatim copies of the later files' definitions
    that the oracle has loaded: trace.ss's `equivalent-workspace-objects?`,
    general-graphics.ss's `find-next-space-position` and themes.ss's `diff` (#f).
  - Tiers: the slow tier runs the battery in a fork pool. The fast tier runs problem 0's
    first seed for 300 codelets.
  - Structural tests check:
    - every `define` has its Python name;
    - docstrings name their origin;
    - there is no tkinter import;
    - the codelet procedures are installed;
    - `format-slipnode` is the top-level value;
    - the load order;
    - the fixtures reach every rule and answer codelet type, top and bottom rules, answers,
      snags and both kinds of commentary.
  - **The crash path.** The battery never crashes. The harness doesn't reach the crash on
    abc ccbbaa ijk seed 3 either (I checked under Chez: no answer and no error in 2500
    codelets). So I wrote a local battery first:
    `python/oracle/batteries/rule-extra-battery.scm`, captured into
    `python/fixtures/rule-extra/`. Its `transcribe-*` and `change-phrase-*` tests (13)
    call `transcribe-to-english` and `get-change-phrase` on hand-made clauses over abc
    ccbbaa ijk. Two of them are Chez's `caddr`-of-`#f` ERROR, which comes from
    `(3rd BondFacet-change)` at rules.ss:1868 (from the backtrace of
    `run.ss abc ccbbaa ijk --seed 3`). The others pin the phrases and the line breaking.
  - Run before either module existed:
    - fast and structural tier: **23 failed, 2 passed** (the two passing tests check that
      every test is translated);
    - slow battery: **50 failed, 1 passed** (`test_what_the_runs_reach`, which reads only
      the fixtures).
- **Code** (new): `metacat/rules.py` (2,707 lines) and `metacat/answers.py` (1,607 lines).
  Two subagents translated them into a staging directory while I wrote the tests, and I
  copied them in after the failing run. **All 50 battery cases matched Chez byte for byte
  on the first run** (94 MB of traces, 38 s on 32 cores), and so did the crash cases.
  Fix outside the item: test_coderack.py left coderack-battery's fake two-argument
  `breaker` procedure installed. It now restores every codelet procedure when it ends
  (new anomalies entry).
- **Mutation checks** (`/tmp/mut9/mutate.py`, not kept; each mutant applied, then
  test_rules.py run with `-x`, then the file restored; `git status` confirmed). 15 mutants:
  - **caught by the frozen battery (10):**
    - translate's `prob? 0.4` → 0.5;
    - answer-finder's weights without `temp-adjusted-values`;
    - process-snag not clamping the temperature;
    - the maximum line length 60 → 50;
    - the uniformity factor `exp(4(u − 1))` → 5;
    - the verbatim rule type's pick list reversed;
    - rule-scout not drawing its rule type;
    - "also" after the first answer;
    - "again" only after the second snag;
    - two `punctuate` changes.
  - **killed by the local battery (1):** compute-rule-intrinsic-quality's cohesion factor
    5 → 4. The value is never read (new anomalies entry). I added `quality-*` tests (4) to
    rule-extra-battery.scm after the code. They read uniformity, abstractness, succinctness,
    intrinsic quality and quality from hand-made rules. Their Python cases passed at once,
    and the mutant now fails.
  - **equivalent (3):**
    - answer-finder's support test `<` → `<=` (a tie needs coin = p exactly);
    - the two `apply-slippages` order swaps in `apply-to-change` and
      `apply-to-object-description`. A Chez probe (logged calls in a `list` inside a lambda,
      `scheme --script`) gives left to right, which the Python follows. Only the descriptor
      position ever draws or logs, so the swaps never change a run.
- **Evaluation order.** No site needed reordering. That agrees with the Racket port, which
  has no `port:` changes in rules.rktl or answers.rktl.
  - Every `stochastic-if*` draws its coin first.
  - Maps with effects use `chez.map_`: instantiate-rule-clause-template, transforms,
    translate's clause, object-description and translator maps.
  - The `(list ...)` sites above go left to right, as Chez does.
  - rules.py has 33 `# chez:`/`# 1.2:` comments and answers.py 11.
- **Docs**:
  - `docs/anomalies_and_quirks.md`:
    - an update to "`caddr` of `#f` in `transcribe-to-english`" (the exact site and the
      Python pin);
    - an update to "Rules and answers lean on later files" (the Python stand-ins);
    - new entries: "A battery's fake codelet procedure outlives its test file" and "A rule's
      intrinsic quality is computed and never read".
  - `docs/python-translation-plan.md`: "As built (item 09)".
  - `python/README.md`: the new modules and tests.
  - `test_fixtures.py` now expects the local battery `rule-extra`.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (842 tests, 102 s).

### Blockers
None. Left for later items, by design:
- Translated but not run yet:
  - answers.ss's answer descriptions and comparisons (theme phrases, `theme-abstractness`,
    `compare-answers`…), which justify.ss and memory.ss call;
  - the rule graphics paths (`%workspace-graphics%`);
  - `apply-transforms`'s `cadr` of `#f` on a GroupCtgy transform without a BondFacet
    transform (the subagent kept it latent, as in the original).
- The stand-ins live in test_rules.py: run.ss's `update-everything`, `suspend` and
  `post-initial-codelets`; trace.ss's `*trace*`, events, `monitor-new-rules` and
  `equivalent-workspace-objects?`; memory.ss's `*memory*` and abstract descriptions;
  themes.ss's `diff`; general-graphics.ss's `find-next-space-position`.
- The full-run crash on abc ccbbaa ijk seed 3 is for the golden-run items.

### Next
Item 10: themes, justify, trace, jootsing and memory. themes.py must define `diff` (#f),
`bridge_type_to_theme_type`, `descriptions_affect_themespace_p`, `ignore_descriptions_p`,
`bridge_theme_compatibility_sigmoid` and `g_themespace`. trace.py must define
`equivalent_workspace_objects_p`, `make_answer_event`, `make_snag_event`,
`monitor_new_rules` and `g_trace`. memory.py must define `g_memory` and
`abstract_answer/snag_description`. Then the harness stand-ins for them can go (where the
batteries' own fakes don't replace them). answers.py sets
`_metacat.run.g_temperature_clamped_p`, so run.py (item 11) must define it.

---

## Iteration 11 — 2026-10-03 23:04

### Completed
Item 10, themes, justification, trace, jootsing and memory: **SOLVED**. All 109 golden
traces match byte for byte, whole runs and not just prefixes.
- **Tests first.**
  - `python/tests/golden_harness.py` translates three things: the oracle's prelude.ss
    headless windows, trace.ss's JSON writer and wrappers, and run.ss's driver.
    - The windows include each codelet type's coderack window and the Memory window's
      `add-memory-icon`, which installs a no-op icon procedure. The Commentary window is
      the Racket port's headless one.
    - The wrappers set module attributes: `*coderack*`, the build and break procedures,
      `*workspace*`'s add-rule, `update-temperature`, `update-slipnet-activations`, the
      Trace window, `abstract-answer-description` and `report-error-and-halt`.
    - The driver is headless-break, plus the start, end and summary lines.
  - `python/tests/golden_run.py` translates run.ss's run loop (`init-mcat`, `run-mcat`,
    `update-everything` …) in the tests, as the Racket port's item 10 did. It is
    registered as `metacat.run`, and item 11 moves it into the package.
  - Every golden runs in a fresh fork of a fresh Python process. The Memory outlives a
    run, and the test session's engine is shared with other test files.
  - `python/tests/test_golden.py` has these tests:
    - all 109 goldens, byte for byte, with the first differing line on failure, plus a
      Comment/Answer count check on stdout (slow tier);
    - `a b z` seed 1, 1000 codelets (fast tier);
    - the live oracle on `abc ccbbaa ijk` seed 3: Python must raise the same `caddr` error
      after the same 1062 trace lines (slow tier);
    - the goldens reach every event type, every Temporal Trace event type, and the
      thematic, justify and jootsing codelets;
    - structural tests for the five modules: every `define` has its Python name,
      docstrings name their origin, no tkinter, the objects exist after load, the codelet
      procedures are installed, load order, and `complement-codelet-pattern` raises.
  - I wrote all three files before any of the modules existed and ran them: **23 failed,
    2 passed, 109 errors**. The two that passed read only the goldens. Every run failed
    on `module 'metacat' has no attribute 'memory'`.
- **Code** (new):
  - `metacat/themes.py`, `justify.py`, `trace.py`, `jootsing.py` and `memory.py`.
  - Partial graphics modules holding only the pure helpers that the model and the trace
    need, as group_graphics.py does: `general_graphics.py` (`find-next-space-position`),
    `trace_graphics.py` (`group-event-pexp-text-string`) and `theme_graphics.py`
    (`relation-name`).
  - Four subagents translated the five files into a staging directory while I wrote the
    harness and tests, and I reviewed them before copying them in. The first scratch run
    stopped at the missing `general_graphics`. With it added, **a b z seed 1 matched at
    once, and then all 109 goldens did**: 272,957 codelets, every event type, 115 answers,
    the halt run, and the crash run against the live oracle. No model line needed a fix.
    The first full test run had one failure, a docstring test on the never-defined
    `complement-codelet-pattern` stand-in, which I fixed.
- **Stand-ins removed** (now the engine's own):
  - codelet_harness.py: the themes.ss helpers;
  - test_rules.py: `equivalent-workspace-objects?`, `find-next-space-position` and `diff`.

  The batteries' own fakes stay. They patch the real modules and restore them, and all
  battery suites still pass.
- **Mutation checks** (`/tmp/mut10/mutate.py`, not kept). Each mutant ran against all 109
  goldens, and the files were restored afterwards (checked with md5sum). 12 mutants:
  - **caught by the goldens (9):**

    | Mutant | Goldens differing |
    |---|---|
    | theme decay 25 → 24 | 109 |
    | group event names without dashes | 75 |
    | `traverse-rule-clauses` first to last | 22 |
    | max clamp period 750 → 700 | 14 |
    | jootser's snag test without `1-` | 8 |
    | retention probability 50 → 60 | 5 |
    | Memory distance threshold 5 → 4 | 5 |
    | grace period 100 → 99 | 4 |
    | thematic-bridge-scout's `pick-positive-theme` first to last instead of Chez's map order | 1 |

  - **killed by a new local battery (3):**
    - a theme's spread to the Slipnet with the square instead of the cube;
    - the concept-mapping importance threshold 65 → 60;
    - the group importance threshold 100 → 99.

    On the goldens, active themes are always fully active, no importance falls in 60–64,
    and no group has strength 99. `python/oracle/batteries/trace-extra-battery.scm` has 3
    tests: `concept-mapping-importance`, `group-importance` and `theme-spread-to-slipnet`.
    It uses the real Themespace and Trace with recording windows. It is captured into
    `python/fixtures/trace-extra/` through the unedited diff-eval.ss and covered by the
    slow re-capture test. I wrote it *after* the code: its Python side, which runs in a
    fork, passed at once, and each of the three mutants now fails it.
- **Evaluation order.** No site needed reordering, which agrees with the Racket port: its
  five files have no `port:` changes.
  - Every `stochastic-if*` draws its coin first.
  - Maps with effects use `chez.map_`.
  - `traverse-rule-clauses` walks the rests before the firsts, so it is a length check
    and a reverse loop (the golden mutant above).
  - answer-justifier computes the one drawing argument of its last `clamp-rules` call
    first; the others only read.
  - Comment counts: trace.py 23 `# chez:`/`# 1.2:`, themes.py 13, jootsing.py 9,
    justify.py 8, memory.py 1. memory.ss draws nothing.
- **A quirk of the original** (new anomalies entry): rules.ss tags failures `'SWAP`,
  `'CONFLICT` and `'CHANGE`, but the snag event's `print` dispatches on lower-case `swap`
  etc. Chez 10 is case-sensitive, so it prints `#<void>-snag involving objects:`. Python
  reproduces it with a `# 1.2:` comment.
- **Docs**:
  - `docs/anomalies_and_quirks.md`:
    - new entries on the snag `print` case and on what the goldens never exercise;
    - Python updates to the halt, the `caddr` crash, `complement-codelet-pattern`, the
      Memory outliving a run, the graphics couplings, "Bridges call themes.ss" and "Rules
      and answers lean on later files".
  - `docs/python-translation-plan.md`: "As built (item 10)". It also notes that trace.py
    is trace.ss, so item 11's trace writer needs another name.
  - `python/README.md`: the new modules and tests.
  - `test_fixtures.py` now expects the local battery `trace-extra`.
- Speed, for item 12: the 109 goldens take about 6 min of CPU (about 1.4 ms per codelet)
  and 35 s on 32 cores. The fast tier takes 4.6 s.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (983 tests, 2 min 12 s).

### Blockers
None. Left for later items, by design:
- run.ss is translated only in the tests (`python/tests/golden_run.py`), without the REPL
  commands (`ss`, `runtil`, `go`, `rerun`, `break`'s continuation). Item 11 moves it to
  `metacat/run.py`. It also adds `break`/`go` and the CLI, and promotes golden_harness.py's
  writer and driver into the package.
- The stdout of a Python run, `Problem:`/`Comment:`/`Answer:`/summary, is produced by the
  harness but compared with the oracle only in its Comment and Answer counts. Item 11
  compares the CLI's output with the live oracle's.
- Translated but not run headless: the graphics paths (gated by the switches, panels
  item), the Memory's `display`, the Temporal Trace's `display-workspace-state`, and
  `print` methods.

### Next
Item 11: full runs, the trace writer and the CLI.
- Move `tests/golden_run.py` into `metacat/run.py`, with the REPL parts.
- Put the JSON writer and wrappers of `tests/golden_harness.py` into the package under a
  name other than `trace.py` (that is trace.ss).
- Write `__main__.py` with the oracle run.ss's arguments, output and exit codes.
- Make test_golden.py use the package's driver. Its fresh-process fork pool is already the
  parallel golden runner the item asks for.
- `golden_harness.run_problem` already returns run.ss's stdout lines for the CLI
  comparison.


---

## Iteration 12 — 2026-10-03 23:34

### Completed
Item 11, full runs, the trace writer and the CLI: **SOLVED**.
- **Code** (new, in the package):
  - `metacat/run.py`: run.ss in full, all 33 definitions. It has the run loop from
    item 10's test-side `golden_run.py`, which is now deleted, plus the REPL commands:
    `ss`, `runtil`, `clear-breakpoint`, `break`, `quiet-break`, `go`, `suspend`,
    `rerun`, and `prompt`/`no-prompt`, which are never used because `swl:version` is
    "0.9x".
  - **break/go**: `run.toplevel(thunk)` stands for the REPL evaluating one command. It
    runs the command in an engine thread. break's `(reset)` parks that thread at the
    break point, and `toplevel` returns. `go` calls the `Breakpoint` (break's
    continuation): the parked thread resumes and break returns `'ignore`, even from the
    middle of a codelet. `go` then waits until the next break. Only one thread runs model
    code at a time. Outside `toplevel` (the headless drivers) `Reset` propagates.
    `init-mcat` unwinds a parked run that it drops.
  - `metacat/headless.py`: the oracle's prelude.ss headless windows and its run.ss driver
    (`run_problem`, `headless_break`, the Answer and Ooops lines, the summary), moved from
    `tests/golden_harness.py`.
  - `metacat/trace_writer.py`: trace.ss's JSON writer and wrappers. iterations.md says
    `metacat/trace.py`, but that name is trace.ss, the Temporal Trace (item 10), so the
    writer got another name. The wrappers compute nothing when there is no trace port.
  - `metacat/__main__.py`: `python3 -m metacat` takes run.ss's arguments, prints the
    same output and uses the same exit codes: 0, 2 for usage errors, and 1 for the
    original's crash. On a crash the first stderr line is the oracle's, then a Python
    traceback follows.
  - `tests/golden_harness.py` now only reads problems.txt and runs forks around
    `headless.run_problem`. `test_golden.py`'s extra battery uses the real `metacat.run`.
- **Tests**:
  - `tests/test_cli.py` (30 tests) runs the CLI and the live oracle in parallel, about
    6 s. Each case must give the same stdout and exit code:
    - racket/tests/cli-test.rkt's cases: an answer, no cap, a cap, justify, keep-going,
      verbose, the halt run (eqe qeq abbba aaabaaa seed 3), and the crash run (abc ccbbaa
      ijk seed 3: exit 1 and the same first error line);
    - twelve bad argument lists;
    - `--trace`, which must write golden abc-abd-xyz_3852097033 byte for byte, as
      `--verbose --trace` must for 3009318743;
    - a run seeded from the clock, which the oracle must replay.

    Fast tier: the usage errors, without the oracle.
  - `tests/test_run.py` (14 tests) and `tests/run_scenarios.py`, each scenario in a
    fresh process. They are the counterpart of racket/tests/run-test.rkt:
    - a run stopped at 150, 300 and 450 with `go` in between is the same run (generator
      state, count and temperature) as one run straight to 450;
    - the control panel's mode switches;
    - `*running?*` is true during the resumed run;
    - step mode (`ss 40`);
    - `go` with no break;
    - **a break inside a codelet**: abc abd xyz seed 3 suspends during codelet 2428,
      while the count is still 2427. Resumed with `go` through the breaks at 2429 and
      2659 to the breakpoint at 3000, it reaches the same state as the oracle-equivalent
      `--keep-going` run;
    - a new run drops a parked break;
    - the definition, docstring and no-tkinter checks.
  - **All 109 goldens match event for event** through the package's driver, in parallel
    (test_golden.py, 32 forks, about 35 s).
  - **Tests first, partly.** I wrote run.py, headless.py and trace_writer.py first, moving
    code that item 10's tests had already pinned against the 109 goldens. The CLI cases
    were checked by hand against the oracle before test_cli.py existed. Then I wrote
    test_cli.py and test_run.py and showed that they fail without the new modules (moved
    aside): test_cli.py gave **6 failed, 24 errors**, and test_run.py and test_golden.py
    failed at collection (`ImportError: cannot import name 'headless'`). With the
    modules in place, everything passed.
- **Mutation checks** (`/tmp/mut11.py`, not kept; each file restored, then the gate
  re-run). 11 mutants:
  - 10 caught: no "stopped", init-mcat not unwinding the parked break, no
    switch-to-run-mode, the ss message, keep-going ignoring the cap (it hangs, caught by
    the timeout), the Answer line's spacing, the parser taking any word as a string, exit
    code 3, the themes event emitted every time, no seed check.
  - 1 survived at first: `go` not setting `*running?*`. The `running_during_go` check
    now kills it.
- **Run times**: `python/oracle/bench_runs.py` writes `docs/python-run-times.md`. It runs
  every golden run once as a process in Python and in the oracle and checks that both
  give the same output. Over all runs Python takes 1.23 ms per codelet and Chez
  0.13 ms, about 9×. Startup takes 0.05 s in Python and 0.71 s in Chez. The longest
  problem, eqe qeq abbba aaabaaa, takes 56 s over its 3 seeds against Chez's 7.4 s.
- **Docs**:
  - `docs/python-translation-plan.md`: "As built (item 11)";
  - `docs/anomalies_and_quirks.md`: two Python entries: a break inside a codelet needs a
    parked thread, and `str.isalpha` vs `char-alphabetic?`;
  - `python/README.md`: the new modules and tests;
  - `python/run-tests.sh`: the tier comment.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1027 tests, 2 min 54 s).

### Blockers
None. Notes:
- `*EEG*` is still a null stand-in installed by `headless.prepare()`, because
  eeg-graphics.ss belongs to the panels item (14).
- A `Breakpoint` resumes once. Chez could re-enter a continuation that was already
  used, but nothing in the original does.
- `go` waits on the calling thread until the next break. The GUI (item 15) should call it
  from a worker thread, or poll, not from Tk's thread.

### Next
Item 12, extra seeds and speed. `golden_harness.run_in_fresh_process` and
`headless.run_problem` can run the 720 extra-seed runs. `python/oracle/bench_runs.py` gives
the oracle's side and its timing. Profile with `python3 -m cProfile -m metacat abc abd xyz
--seed 3 --max-codelets 3000`.

---

## Iteration 13 — 2026-10-04 00:32

### Completed
Item 12, extra seeds and speed: **SOLVED**. All 720 extra-seed runs match the oracle byte
for byte, both before and after the speed-ups.
- **The oracle's side.** `python/oracle/capture_extra_seeds.py` (new) builds
  tests/extra-seeds.py's jobs: every problem line of tests/problems.txt, with 20
  non-golden seeds from `random.Random(20261003)`, drawn the same way, so these are the
  720 runs the Racket port was audited on. It runs each once in the unedited oracle
  (`run.ss ... --trace FILE`, fresh process, 32 at a time, about 1 min 20 s). It freezes each
  run's exit code, whole stdout, first stderr line, trace sha256 and trace line count
  into `python/fixtures/extra-seeds/runs.jsonl` (760 KB). The traces themselves (2.6 M
  lines, about 300 MB) are not kept. `SOURCES` holds the sha256 of run.ss, trace.ss,
  prelude.ss, problems.txt, tests/extra-seeds.py and the script, plus the Chez version.
  In the oracle, the 720 end as 618 suspends, 101 caps and 1 halt, with no crash.
  A second capture into a temp directory was byte-identical.
- **Tests** (`python/tests/test_extra_seeds.py`, new):
  - fast tier: the fixture's sources are unchanged; the jobs are extra-seeds.py's 720,
    none of them golden; and what the runs reach (suspend, cap, halt);
  - slow tier: all 720 runs through the package's driver, each in a fresh fork of a fresh
    process (`golden_harness.run_in_fresh_process(..., digest=True)`, new: it reduces a
    run to what `python3 -m metacat ... --trace` gives). Exit code, stdout, error line and
    trace hash and length must all equal the oracle's. A failure lists each differing
    run as a CLI command;
  - slow tier: the oracle's 720 again, byte-identical to the fixture (freshness, as for
    the batteries).

  `test_fixtures.py` now skips the `extra-seeds/` directory when it lists batteries.
- **Tests first?** The code under test is items 04–11's port, which already existed. The
  first run of the 720 passed at once: 3 min 7 s wall, 54 CPU-min. Mutation checks
  (`/tmp/mut12.py`, not kept; each file restored and checked with `git diff`) show that
  the extra seeds see more than the goldens:

  | Mutant | 109 goldens | 720 extra seeds |
  |---|---|---|
  | `%concept-mapping-importance-threshold%` 65 → 60 | pass | **3 differ** |
  | `%group-importance-threshold%` 100 → 99 | pass | **5 differ** |
  | spanning-bridge theme boost ×2 → ×1 | 57 fail | 378 differ |
  | theme spread to the Slipnet, square for cube | pass | pass (only trace-extra-battery.scm kills it) |

- **Profile** (cProfile, `abc abd xyz --seed 3` and the halt problem seed 7, 13,929
  codelets). `tell` dominates: 36 M calls in the long run, plus 5.7 M delegations. Next
  came Chez arithmetic's type checks (40 M `_check` calls), `get-removal-weight` (1.6 M:
  every post to a full coderack weighs all codelets), `memq`/`eq?` (14 M), and the trace
  wrappers (1.8 M messages through `*coderack*`'s and `*workspace*`'s wrappers).
- **Speed-ups** (each marked `speed (item 12)` in the code; gains in
  docs/python-run-times.md):
  1. chez.py arithmetic fast paths (fixnum/fixnum and flonum/flonum; two-argument `+`/`*`;
     `_check` as a set lookup). `(+ 3 4)` 361 → 158 ns.
  2. `memq`/`remq` by identity when `eq?` is identity; `member?` without the tail copy.
     2061 → 322 ns over 20 objects.
  3. One-list `andmap`/`ormap`: 2136 → 653 ns.
  4. `weighted-index` without copying the rest of the list: 39.8 → 16.3 µs over 100.
  5. The trace wrappers forward unwatched messages directly: 1.8 M fewer calls.
  6. `tell-all` inlines `tell`: 5.2 → 3.6 µs over 20.
  7. `sort-by-method` caches each key on first request (the four keys are getters):
     20.6 → 17.1 µs over 20.

  Overall, Python function calls fell 36% (short run) and 29% (halt run). The 720 runs
  take 40.8 CPU-min instead of 54.2 (−25%) and 2 min 4 s instead of 3 min 7 s. Another
  user's 20-process job shared the machine throughout, so CPU times varied by up to
  ±15%. The per-step gains are therefore given as micro-benchmarks and call counts,
  measured on cumulative copies of the package (`/tmp/var/S0..S7`, built from the diff's
  hunks). `python/oracle/bench_speed.py` (new) interleaves repetitions across such
  copies; best of 6, total CPU went from 25.6 s to 16.1 s.
  After the speed-ups: 109/109 goldens, 720/720 extra seeds and every battery still pass.
  Not done: `__slots__` (little to gain in 3.12) and caching the highest bin's urgency in
  `delete-codelets`, which would change coderack.ss's shape.
- **Tiers**, settled in `python/run-tests.sh`:
  - `--fast`: 729 tests, about 7 s;
  - full (the gate): 1032 tests, which adds the Chez re-captures, the 109 goldens, the
    CLI against the live oracle, the 720 extra seeds and their oracle re-capture. It took
    5 min 45 s under the shared load, well under 15 min.
- **Docs**:
  - docs/python-run-times.md: a new "Speed-ups" section;
  - docs/python-translation-plan.md: "As built (item 12)", including the two model facts
    the speed-ups rely on;
  - docs/anomalies_and_quirks.md: an update to "The goldens never exercise ... the
    trace's importance thresholds" (the extra seeds reach both);
  - python/README.md.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1032 tests, 5 min 45 s).

### Blockers
None. Notes:
- The run-times table in docs/python-run-times.md (`bench_runs.py`) predates the speed-ups.
  I did not regenerate it during this item because the machine was loaded. Item 17's
  final audit can rerun `python3 python/oracle/bench_runs.py docs/python-run-times.md` on
  an idle machine. Note that it rewrites the file, so the Speed-ups section would have to
  be appended again.
- `sort-by-method`'s key cache assumes pure sort keys. A new caller with an effectful key
  would need the uncached form (docs/python-translation-plan.md, item 12).

### Next
Item 13, the SGL interpreter on tkinter: `sgl-interpreter.ss` and `fonts.ss` against the sgl
battery's 48 fixtures (`python/fixtures/sgl/`). Recall from item 03 that the graphics tell
strings from symbols (`string?` in sgl-interpreter.ss:386/398/432, fonts.ss:93), so text
arguments must stay `chez.String` where the original has strings. Engine modules must not
import tkinter; the interpreter's canvas operations can be recorded headless for the tests.

---

## Iteration 14 — 2026-10-04 01:01

### Completed
Item 13, the SGL interpreter on tkinter: **SOLVED**.
- **The oracle's Tcl stream.** The prelude's `swl:tcl-eval` returns `""` and its
  `define-class` skips `<viewport>`, so nothing in the oracle sent Tcl yet. Three new
  files in `python/oracle/` capture it:
  - `sgl-fixture.scm`: the fixture as data. It has the pictures of
    racket/tests/sgl-fixture.rkt: every form, every `let-sgl` binding, nested and rational
    origins, erase, clear, rule, and the tag operations (move, move-pixels, raise, retag,
    delete, hidden rectangles and unhide). Offscreen extras add `erase!`, `rescale`,
    degenerate shapes and a symbol colour. It is drawn on two viewports: v1 is
    640 × 480 at 1:1, v2 is 320 × 240 at 2:1.
  - `sgl-tcl.ss` loads the unedited original through the prelude, as diff-eval.ss does,
    and adds:
    - a recording `swl:tcl-eval`. Windows are written by name, colours as `(rgb r g b)`,
      SWL fonts as `(font face size style)`. The hidden canvas answers `bbox` from a
      fixed text metric.
    - a `define-class` that makes `<viewport>` a closure over its ivars, with a
      `<canvas>` base;
    - a calling `send`.

    It then reloads the unedited sgl-interpreter.ss. fonts.ss runs unchanged, so its
    `get-pixel-size` (create, bbox, delete on the hidden canvas) is in the stream too.
  - `capture_sgl_tcl.py` runs it and writes `python/fixtures/sgl-tcl/{v1,v2}.txt`
    (318 commands each) and `SOURCES`. A second capture is byte-identical (the slow
    tier re-captures it).
- **Tests first.** `python/tests/test_sgl.py` (69 tests) and the helper
  `tests/scheme_reader.py` (Scheme text to chez.py data) were written before any
  `metacat/gui` module existed. Run then: **9 failed, 4 passed, 56 errors**, all
  `ModuleNotFoundError: metacat.gui`. The 4 that passed only read fixtures or engine
  files. The tests:
  - all 48 tests of `tests/diff/sgl-battery.scm` against `python/fixtures/sgl/`, with
    sgl-chez-setup.ss's recording `b:vp` translated;
  - the Tcl stream of both viewports, command for command, through a recording window
    and a fake hidden canvas with the capture's metric. Also checked: the stream covers
    every item kind, option and tag command, and the fixture is fresh;
  - fonts (`select-face`, mfont/fixed-font messages, `resize`, the small-fonts switch,
    the bare-font error), colours (the 752 names against constants.ss), Tcl words,
    `remove-unsupported-tcl-args`, mouse handlers and resizing;
  - structure: every `define` of the two files has its Python name, docstrings name
    their origin, the viewport has all 26 public methods, the modules import without a
    display or tkinter, and no engine module imports `metacat.gui`;
  - slow: `tests/render_sgl_fixture.py` under `xvfb-run` draws the fixture on a real
    tkinter Canvas, with text measured by Tk on `create-mcat-logo`'s hidden canvas, then
    grabs the window and checks 16 pixels: the ivory clear, a hidden item, unhide,
    delete with retag, raise, move, erase, a pie slice, a polygon fill. The grab uses
    XGetImage through ctypes, and the PNG is written with zlib.
- **Code** (new): `python/metacat/gui/`:
  - `sgl.py`: sgl-interpreter.ss. `Viewport` keeps every `<viewport>` method with its
    `tcl-eval` calls, argument for argument.
  - `fonts.py`: fonts.ss.
  - `colors.py`: constants.ss's colour part, with `Rgb`. The table is generated from
    constants.ss and checked against it.
  - `swl.py`: `swl_tcl_eval`, `tcl_word` and `TkCanvas`, which hands the commands to a
    tkinter Canvas's widget command.

  With the code in place, **the 48 battery cases and both Tcl streams matched Chez on
  the first run**. The three first failures were mistakes in the tests:
  - `set-mouse-handlers! #f #f` keeps the old handlers;
  - the original has 26 public methods, not 25;
  - the docstring check caught the `init_env` lambda.
- **Look at what you draw.** The Tk rendering (`python/tests/snapshots/sgl-fixture.png`)
  matches `racket/tests/snapshots/sgl-fixture.png` cell for cell: shapes, dashes,
  dotted lines, pie slices, rings, erase, raise, delete and unhide. Only the text metrics
  differ (Tk vs Pango), which moves the relative-text letters by a few pixels. A first
  version rendered through `canvas postscript` and ghostscript, and lost the background
  (new anomalies entry); the X grab replaced it.
- **Mutation checks** (`/tmp/mut13/mutate.py`, not kept; each mutant applied, test_sgl.py
  run, the file restored, then `diff -r` against a copy). Of 23 mutants:
  - **20 caught at once:** the justification sign, the dashed point length, extend
    binding origin, the dotted dash string, draw-exps order, the arc `>= 360`, the ring's
    inner outline, baseline floor for round, the relative y sign, the move y sign,
    raise without `all`, erase without swl-color, the default tag, the hidden canvas not
    cleared, the image box inset, origin y, clear without delete, swl-font's style test,
    the M width, the relative x offset.
  - **3 survived:**
    - the filled-rectangle guard `or` → `and`;
    - `mouse-press` matching modifiers by inclusion;
    - `lookup` converting symbol colours too.

    I added degenerate shapes and a symbol colour to the fixture, recaptured from Chez,
    and added control-clicks to the mouse test. All 23 are now caught.
- **Evaluation order.** The interpreter draws nothing at random. The only argument
  lists with effects are the hidden canvas's nested `tcl-eval` (inner first, as written)
  and draw-text's `let*` (in order).
- **Docs**:
  - `docs/anomalies_and_quirks.md`: "Text items at half pixels: the original sends Tk
    exact ratios" (`249/2`; Python sends the ratio to its window and a float to Tk),
    "Tk's canvas PostScript leaves out the background", and an update to "The graphics
    and rules.ss tell strings from symbols";
  - `docs/python-translation-plan.md`: "As built (item 13)";
  - `python/README.md`: the new modules and tests;
  - `python/run-tests.sh`: the tier comment.

  `test_fixtures.py` skips `sgl-tcl/`, which is not a battery.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1101 tests, 5 min 42 s).

### Blockers
None. Notes for later items:
- `fonts.create_mcat_logo` uses white and times 24 bold italic in place of
  `%logo-background-color%` and `%logo-font%`, which are in constants.ss's graphics part
  (item 14). The scrollbar sizes come from tkinter Scrollbars.
- `SwlFont.get_actual_values` reports the request in points at 96 dpi, as the Racket
  port does, not Tk's `font actual`. The render script sets `tk scaling` to 96/72; the
  GUI (item 15) should do the same so that pictures don't depend on the display.

### Next
Item 14, the panels. general-graphics.ss's `make-graphics-window` should build
`sgl.Viewport(swl.TkCanvas(canvas), ...)` with its own transforms (test_sgl.py's
`make_viewport` follows racket/tests/sgl-fixture.rkt's). Call `fonts.load()` and
`sgl.load()` when the views start, and write colour names as `chez.String`. The
graphics battery's fixtures are in `python/fixtures/graphics/` and
`python/fixtures/panels/`. `oracle/sgl-tcl.ss`'s `define-class`/`send`/`swl:tcl-eval`
recipe can capture the Tcl stream of other panels if the batteries aren't enough.

---

## Iteration 15 — 2026-10-04 02:25

### Completed
Item 14, the panels: **SOLVED**.
- **Code.** Four subagents translated the files in parallel against a contract I wrote
  first (`metacat/gui/hosts.py`). I then integrated and reviewed their work.
  - Engine side (no tkinter, no `metacat.gui`):
    - `general_graphics.py` now has all of general-graphics.ss except the windows;
    - `group_graphics.py` is complete;
    - new `bridge_graphics.py`, `rule_graphics.py` and `eeg_graphics.py` (`%EEG-table%`,
      `make-EEG`, `*EEG*`). headless.py's stand-in `*EEG*` is gone.
    - chez.py: complex arithmetic and `magnitude`/`angle`/`make-polar`/`cos`/`sin`/`acos`
      with Chez's exact results.
  - Views (`metacat/gui/`): `constants.py`, `general_graphics.py` (`GraphicsWindow`, the
    scrollable text window, the resize listener), and `slipnet_`, `workspace_`,
    `temperature_`, `coderack_`, `theme_`, `trace_`, `memory_`, `commentary_` and
    `eeg_graphics.py`.
  - `hosts.py`: `OffscreenHost` (counts items and measures text with sgl-tcl.ss's fixed
    metric, so it needs no display) and `TkHost` (a Toplevel and a Canvas).
  - `views.py`: `load_views`, `attach_views`, `attach_workspace_view`.
  - Driver: `headless.run_problem(..., views=...)` attaches the views, then wraps their
    Commentary and Trace windows (`install_recorders`, and
    `trace_writer.wrap_trace_window`, split out of `install_trace`). The null EEG window
    also accepts `plot-current-values`, for runs with only the Workspace window.
- **Tests first.** Each test file was written and run before its code existed:

  | File | Before the code | Now |
  |---|---|---|
  | test_graphics.py (graphics-battery.scm's 48 tests + structure) | 65 failed, 5 passed | 71 passed |
  | test_panels.py (panels-battery.scm's 36 tests + structure) | 33 failed, 4 passed | 47 passed |
  | test_gui_windows.py | 6 failed, 8 errors, 3 passed | 17 passed |
  | test_gui_panels_a.py | 12 failed, 11 errors, 1 passed | 25 passed |
  | test_views.py | failed (no `metacat.gui.views`) | 113 passed |

  The batteries' header says 50 graphics tests and 38 panels tests, but their MANIFESTs
  have 48 and 36. The cases cover every MANIFEST entry, and a test checks that.
- **Watching changes nothing.** test_views.py runs all 109 goldens with every window
  attached (offscreen, all graphics switches on). Every trace is identical, and every
  window draws, more than 1000 items each over the goldens. The crash run (abc ccbbaa
  ijk seed 3) raises the same `caddr` error after the same trace. That takes 38 s on
  32 cores. The fast tier runs a b z seed 1 with views. A mutant view that draws one
  random number in the Temperature window's update fails it at trace line 3.
- **Look at what you draw.** `tests/render_views.py` (slow tier, under `xvfb-run`)
  renders 8 scenes on real Tk canvases, 56 pictures in all, and grabs them with
  XGetImage:
  - run7 at 300 and 800 codelets;
  - run7's snag event view;
  - run7's answer;
  - run7's answer description;
  - a click on run7's last clamp event in the Trace window;
  - the justify run abc abd xyz xyd seed 1760747975;
  - two answers compared in the Memory window (abc abd glz).

  `python/tests/snapshots/views/` keeps one rendering of each. I inspected them:
  - The Workspace, Slipnet, Coderack, Commentary, Temperature, theme panels, Trace,
    Memory and EEG at run7's answer match `docs/screenshots/run7-wyz.png` item for item:
    the same bridges, rules, concept-mapping lists and labels, activations, codelet
    counts, comments, event icons and the snag/wyz icons.
  - The glz comparison matches `racket/tests/snapshots/workspace-glz-compare.png`.
  - Only Tk's text metrics differ: fonts come out a little larger, so the
    concept-mapping list touches the top rule box.
- **Mutation checks** by the subagents (each restored and verified with `cmp`; one of
  them found that a same-size restore can run a stale .pyc, so they used
  `PYTHONDONTWRITEBYTECODE=1`):
  - engine: 13 of 15 caught. The survivors are equivalent on these inputs: the octagon
    `(* a (/ 1 k))` → `(/ a k)`, and the EEG thunk map order (the thunks only read).
  - theme, trace and memory panels: 11 of 11 caught. The icon grey-level mutant
    survived the battery at first, because `b:clean` hides colours; a new
    `test_memory_icon_colours` now catches it.
- **Docs.**
  - `docs/python-translation-plan.md`: "As built (item 14)".
  - `docs/anomalies_and_quirks.md`: Chez's complex arithmetic and `magnitude`'s
    `hypot`, the stale-bytecode trap in mutation checks, the Temperature window's `#f`
    icon label, and updates to "exact complex" and "shared rule pexps".
  - `python/README.md` and the tier comment in `python/run-tests.sh`.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1374 tests, 6 min 26 s).

### Blockers
None. Notes for item 15:
- `TkHost` doesn't pass Tk's `<Configure>` events to the viewport. Resizing windows
  (`make-resizable`) and mouse clicks need the GUI to bind them: `vp.configure(w, h)`
  and `vp.mouse_press(i, j, mods)`.
- A click in the Workspace window `(go)`es the run through
  `views.set_thread_break_handler`. It raises until the control panel's engine thread
  installs a handler.
- gui.ss's speed settings are still `view_globals.p_num_of_flashes` and the other speed
  variables. `attach_views` sets them to full speed with no flashing.
- Not yet tested: the Memory window's icons over several runs in one process, and the
  scrollable text window's unit behaviour (it is covered only through the
  Commentary).

### Next
Item 15, the control panel and windows (`gui.ss`). Build on `views.attach_views` with
`hosts.set_window_host_maker(hosts.tk_host_maker(root))`, `fonts.create_mcat_logo(root)`
and `tk scaling` 96/72, as `tests/render_views.py` does. Run the engine in a worker
thread (run.py's `toplevel`/`go` already park a break) or step it with `after`. Bind
mouse and resize events to the viewports. The GUI run's trace can be checked with
`headless`-style recorders (`install_recorders`).



---

## Iteration 16 — 2026-10-04 02:35

### Completed
Item 15, the control panel and windows: **SOLVED**.
- **Expected values from Chez.** `python/oracle/batteries/gui-battery.scm` (new, 8 tests)
  is captured into `python/fixtures/gui/` through the unedited diff-eval.ss, which loads
  gui.ss and demos.ss on the prelude's SWL stubs. It covers:
  - `tokenize-string` on 29 inputs (case, noise, digits glued to letters, non-ASCII
    letters and digits, tabs and newlines, out-of-range seeds);
  - the Step/Go/Reset decision on each input;
  - `char-noise?`;
  - the speed slider's four settings for every value 0–100, and the speed constants;
  - the figure titles, all 35 demo problems, and the five clamp-codelets patterns (by
    codelet type name).
- **Tests first.** `python/tests/test_gui.py` and `python/tests/drive_gui.py` were written
  before any code. Run then: **18 failed, 2 passed**. The two that passed were the
  MANIFEST check and "no engine module imports the GUI". Everything else failed with
  `ModuleNotFoundError: metacat.gui.gui`, `metacat.demos`, or `cannot import name 'app'`.
  Now every test passes. test_gui.py checks:
  - the battery;
  - every one of gui.ss's 61 and demos.ss's 36 definitions has its Python name;
  - docstrings name their origin;
  - demos.py imports no GUI, gui.py and app.py import tkinter only inside functions, and
    no engine module imports `metacat.gui`;
  - slow tier: `drive_gui.py` under `xvfb-run`.
- **drive_gui.py** builds the GUI as `python3 -m metacat.gui` does, with trace.ss's writer
  installed and the Commentary and Trace windows recorded. Tk's main thread runs
  `mainloop`, and a driver thread uses the widgets: it types into the command line,
  presses the buttons, invokes menu entries and dialog buttons, and clicks on canvases.
  Its scenarios:
  - **windows:** titles; the 12 window controllers with EEG and Logo hidden; the buttons
    start disabled; no window overlaps the control panel.
  - **invalid input:** "Invalid input!" appears, then goes; the slider at Fast gives
    (1 1 1 1).
  - **full run** (abc abd ijk 1): Enter stops at codelet 0 with seed 1; Go runs to the
    answer ijd.
  - **step mode:** the step interval is set to 40 through its dialog; Step gives 40, 80
    and 120 codelets; Go finishes.
  - **demo, stop and go:** Demos ▸ Run 7; Go; Stop mid-run (at codelet 466–480 in
    practice); Go again to wyz at 2170. While the engine runs, the main thread answers
    within 0.002 s.
  - **breakpoint and click:** a breakpoint at 100 through the dialog stops the run there,
    and a click on the Workspace canvas resumes it (workspace-window-press-handler, then
    thread-break).
  - **reset:** Reset re-initializes the problem.
  - **menus:** hiding and showing windows, EEG, Eliza mode, self-watching off and on (the
    warning label and the theme windows), the theme edit dialog (Cancel), a manual
    codelet clamp and its undo, Help (shows help.txt), the commentary font size.
  - **save commentary:** the file equals the Commentary's lines.
  - **resize:** the Workspace at 1000×750 gets that visible size through the resize
    listener.
  - **screenshot:** the whole screen, grabbed with XGetImage on the root window.

  **Every GUI run's trace equals its golden byte for byte** (abc-abd-ijk_1 four times:
  full, step, breakpoint and click, reset; abc-abd-xyz_3852097033 once, after the demo
  and stop/go), and so does each end state (codelet count, generator state). The whole
  driver takes about 20 s.
- **Code** (new): `metacat/gui/gui.py` (gui.ss), `metacat/demos.py` (demos.ss),
  `metacat/gui/app.py` (setup.ss's `setup` and `enable-resizing`, the engine thread, the
  window layout) and `metacat/gui/__main__.py`. Changed:
  - `gui/hosts.py`: `TkHost` show/hide, real geometry, the window manager's close, and
    `<Configure>` and mouse presses passed to the viewport;
  - `gui/general_graphics.py`: the host gets its viewport;
  - `gui/fonts.py`: the logo uses constants.ss's colour and font;
  - `gui/swl.py`: `ThreadSafeTk`;
  - `chez.py`: `string_to_number` reads ASCII digits only.
- **Engine thread vs `after`: a worker thread with a queue.** `app.EngineThread` stands
  for the REPL thread. The control panel's `thread-break`s queue thunks on it, and each
  runs through item 11's `run.toplevel`, so break and go work as in the CLI tests.
  `after`-driven stepping was rejected because suspend breaks from inside a codelet, and
  only a parked thread can resume there. Recorded in the plan ("As built (item 15)") and
  app.py's docstring.
- **A crash found and fixed:** tkinter's own cross-thread Tk calls segfaulted in
  `mainloop` during the first GUI run. All Tk calls from other threads now go through
  `swl.ThreadSafeTk`, a queue and a pipe watched by Tk's event loop (new anomalies entry).
- **Look at what you draw.** I inspected `python/tests/snapshots/gui-run7.png`, the whole
  2560×1600 screen right after run7's answer. It shows the control panel (menubar Help,
  Demos, Windows, Options, Clear Memory; the problem and seed; the slider; the buttons),
  the Temperature at 15, the Workspace with wyz and both rules, the Coderack, the
  Commentary ending "The answer "wyz" occurs to me", the Slipnet, the theme panels, the
  Memory (SNAG, wyz) and the Trace (SNAG and Clamp icons). Item for item, that is the
  content of `docs/screenshots/run7-wyz.png`. `gui-final.png` is the screen at the end of
  the driver.
- **Mutation checks** (`/tmp/mut15.py`, not kept; gui.py restored and checked with `cmp`):
  6 of 6 caught.
  - the driver caught: Stop not setting `*interrupt?*`; init-new-problem without
    quiet-break; Step turning step mode off; a window controller's show not showing;
  - the battery caught: the flash range floor 2 → 3; the tokenizer accepting digits after
    letters.
- **Docs**:
  - `docs/anomalies_and_quirks.md`: three entries (the Tk cross-thread crash, non-ASCII
    digits in `string->number`, generated keys going to the focus window);
  - `docs/python-translation-plan.md`: "As built (item 15)";
  - `python/README.md`;
  - the tier comment in `python/run-tests.sh`;
  - `test_fixtures.py` now expects the local battery `gui`.
- `python3 -m metacat.gui` under Xvfb prints "Initializing windows...done" and waits in
  Tk's main loop.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1397 tests, 6 min 46 s).

### Blockers
None. Notes:
- Widget colours follow gui.ss. Tk 8.6's disabled-entry colours are set to the entry's own
  colours, so "running..." shows green on black as in SWL's Tk.
- Model errors in a GUI run go to `engine-error` (input mode and an "Error: ..." line), as
  in the Racket port. In the original they went to the REPL.
- help.txt is read from `chez_scheme/original/`. Item 16's packaging must make it
  reachable from an installed package.

### Next
Item 16, packaging and docs: `pip install -e python` with a `metacat` command
(`metacat.__main__`), and a GUI entry (`metacat.gui.app:main`). Ship help.txt with the
package, or find it from the checkout. Use `python/tests/snapshots/gui-run7.png` for the
README screenshots.

---

## Iteration 17 — 2026-10-04 02:58

### Completed
Item 16, packaging and docs: **SOLVED**.
- **Tests first.** I wrote `python/tests/test_install.py` before changing the package and
  ran it: **8 failed, 1 passed**. The one that passed was the GUI opening from a clean
  copy, which doesn't touch help.txt. The failures were:
  - no `[project.scripts]`;
  - `app.main` needing an argument;
  - `packages = ["metacat"]` only, so **a regular install left out `metacat/gui`**
    (`site-packages/metacat/gui/app.py` missing), a real packaging bug;
  - help.txt read from `chez_scheme/original/` through `parents[3]`;
  - the editable venv had no `metacat` command;
  - the clean-checkout trace differed from the golden at byte 99. That one was my test's
    mistake: the golden was recorded with `--max-codelets 10000`, and the test now passes
    that too.

  The tests:
  - fast: the two commands are declared, both `main`s take no required argument, the
    packages and package data are declared, the shipped help.txt equals the original's
    byte for byte, and no package file reads outside the package;
  - slow, each in a tmpdir:
    - a clean copy of `python/` (`git ls-files -co --exclude-standard`, without fixtures and
      tests, no `chez_scheme/` beside it) runs `python3 -m metacat abc abd xyz --seed
      3852097033 --max-codelets 10000 --trace` with no PYTHONPATH. Its stdout must equal
      the live oracle's, and its trace must equal the golden byte for byte.
      `python3 -m metacat.gui` there must print "Initializing windows...done" under
      xvfb-run; the test then kills it.
    - `pip install -e` of that copy into a fresh venv. `metacat` is run from another
      directory, `metacat` must import from the copy, and the stdout and trace are
      checked as above.
    - a regular install into a fresh venv: the same checks, plus `gui/` and `help.txt` in
      site-packages, and `metacat-gui` opens.

  The venvs use `--system-site-packages` only so that pip finds setuptools offline
  (`--no-build-isolation --no-index --no-deps`). New anomalies entry. I also checked by
  hand the literal `pip install -e python` on the checkout in a venv (`metacat abc abd
  xyz --seed 7` → wyz), then removed the egg-info and the venv.
- **Code.**
  - `pyproject.toml`: packages `metacat` and `metacat.gui`, package data `help.txt`, and
    the scripts `metacat` (`metacat.__main__:main`) and `metacat-gui`
    (`metacat.gui.app:main`).
  - `metacat/gui/help.txt` is a copy of the original's. gui.py reads it beside itself
    (`port:` comment).
  - `app.main(argv=None)` reads `sys.argv`.
  - The CLI's `main` raises the recursion limit itself, because the console script skips
    the `__main__` block.
- **Docs.**
  - `python/README.md` now opens for users. It has the port's claim, a screenshot of the
    whole GUI at Run 7's answer, and how to run it from a checkout or installed. It covers
    the CLI's arguments and exit codes, the GUI's command line, buttons and menus, two
    Workspace pictures (Run 7, and the glz answer description) and how the port was made.
    The developer sections follow, plus a packaging section.
  - The screenshot is `docs/screenshots/python-run7-wyz.png`, cropped from
    `python/tests/snapshots/gui-run7.png` (item 15's Xvfb grab) to the windows. I used
    Anaconda's PIL once for the crop; nothing in the package or tests needs it. I
    inspected it: control panel, Temperature 15, Workspace with wyz and both rules,
    Coderack, Commentary, Slipnet, themes, Trace, Memory.
  - Top-level `README.md`: a new section, "Also in Python", with commands and the
    screenshot. The layout lists `python/`. "How it was made" mentions loop0002.
  - `docs/python-translation-plan.md`: "As built (item 16)".
  - `docs/anomalies_and_quirks.md`: "Python: a fresh venv has no setuptools ...".
  - `python/run-tests.sh`: the tier comment.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1406 tests, 7 min 12 s).

### Blockers
None.

### Next
Item 17, the final audit. Rerun `python3 python/oracle/bench_runs.py
docs/python-run-times.md` on an idle machine (item 12's note: it rewrites the file, so
append the Speed-ups section again). The README's run figures (109 goldens, 720 extra
seeds) should be confirmed then.

---

## Iteration 18 — 2026-10-04 03:45

### Completed
Item 17, the final audit: **SOLVED**.
- **Everything re-run**, in the foreground, under a load average of about 30 from
  another user's jobs:
  - test_golden.py, test_extra_seeds.py and test_views.py: the 109 goldens, the 720 extra
    seeds against the oracle's frozen results plus their live re-capture, and the 109
    goldens with every view attached. 256 passed in 4 min 47 s.
  - everything else: the batteries, the CLI against the live oracle, the Tk renders, the
    GUI driven through its widgets under xvfb-run (every GUI run's trace equals its
    golden), and the venv installs. 1151 passed in 2 min 34 s.
  - the gate twice: 1409 tests in 6 min 56 s mid-item, then 1412 at the end (below).
- **No engine module imports tkinter.** The AST check found no `tkinter` or `metacat.gui`
  import outside `metacat/gui/`, at the top or nested. The run-time check found one leak:
  `engine.translated_modules()` treated `"gui"` in `LOAD_ORDER` (gui.ss) as the
  `metacat.gui` package and imported its `__init__` on `engine.load()`. It brought no
  tkinter, ran no `load()` and changed no run. I wrote
  `test_gui.py::test_a_headless_run_loads_no_gui_module` first: in a fresh process it
  imports every engine module, calls `engine.load()`, runs a headless run, and checks
  `sys.modules`. It failed with `['metacat.gui']`. Fix: `translated_modules()` skips
  packages. New anomalies entry.
- **The plan re-read against the code.** A read-only subagent checked every name, path
  and behaviour the plan states, and I checked what it found. Nine statements were wrong
  or stale, for example:
  - `continuation_point` is really `continuation_point_star`;
  - only the CLI raises the recursion limit;
  - the GUI uses `ThreadSafeTk`, not `after` polling;
  - the CLI's crash error is a `SchemeError`;
  - the `define_codelet_procedure_star` "decorator" is a plain function;
  - `trace_writer.py`;
  - the `from`-imports of graphics helpers;
  - the codelet total.

  Thirteen future-tense statements had since been done, some differently. I corrected
  each in place, marked "item 17". The two codelet totals are both right: 272,957 events,
  and 272,857 final counts, because 99 suspends and 1 halt stop inside a codelet (new
  anomalies entry).
- **Risk 6's promised tests never existed.** `tests/test_engine_modules.py` (new) has
  them now:
  - every engine module imports alone in a fresh interpreter, without loading the engine
    or drawing a random number;
  - an AST check limits cross-module `from`-imports to chez/objects/sugar/utilities/names,
    plus four named pure drawing helpers that nothing rebinds or wraps.

  This test checks existing code, so it passed when written. Mutation checks, each
  restored and verified with `cmp`: a `from metacat.bonds import build_bond` in rules.py,
  a random draw at memory.py's import and a cross-module read at its import all fail it.
- **The `# chez:` and `# 1.2:` sites are listed in the plan.** The plan has a new
  "Audit (item 17)" section. Its list is generated from the code's real comments
  (tokenize, so docstrings don't count) by `python/tests/quirk_sites.py --write`.
  `tests/test_quirk_sites.py` fails when the list drifts. It failed before the list
  existed. There are 242 sites:
  - 167 `# chez:`: 80 map order, 33 stochastic-if* coin first, 17 evaluation order, 10
    recursion or sequence order, 7 truthiness, 6 characters/strings, 6 sort, 4 numbers,
    2 record-case, 2 other;
  - 75 `# 1.2:`.

  Each site is named by module and function, with a per-module table.
- **Docs**:
  - `docs/follow-ups.md`: a new "Python (loop0002)" section covering the safety net and
    tiers, idiomatic clean-up (objects, module boundaries, the model/graphics coupling,
    the test-side translations, the speed-up assumptions, latent paths), performance and
    features;
  - `docs/anomalies_and_quirks.md`: the two entries above;
  - `docs/python-translation-plan.md`: the audit section and the in-place corrections;
  - `python/README.md`: the audit's tests.
- The README's figures (109 goldens, 720 extra seeds, every window attached) are
  confirmed by this audit's runs.
- `python3 ralph_loops/loop0002/gate.py`: GATE PASSED (1412 tests, 6 min 57 s).

### Blockers
None. Not done: re-running `python/oracle/bench_runs.py` for docs/python-run-times.md,
which needs an idle machine (the load average was about 30 throughout). It is listed in
docs/follow-ups.md.

### Next
The loop is complete. See docs/follow-ups.md, "Python (loop0002)", for a next loop.

LOOP_COMPLETE
