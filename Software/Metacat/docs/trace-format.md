# Trace format and randomness

This file specifies what the Racket port must reproduce bit for bit to be
compared with the Chez Scheme oracle. Item 01 settles the randomness plan
(below). Item 02 specifies the JSON-lines trace format (end of file).

## Randomness plan (item 01)

**Decision: the oracle uses Chez Scheme 10's own `random` and `random-seed`,
unmodified, and the port reimplements them exactly.** No PRNG is swapped into
the oracle. The generator is a 32-bit linear congruential generator, simple
to reproduce in Racket with exact integers; `chez_scheme/oracle/tests/rng-check.ss`
checks this specification against Chez's built-ins, draw for draw and seed for
seed (11 seeds × 19 arguments × 200 draws, plus 2000 interleaved integer/float
draws), and it fails if a constant or bit field is changed.

Source: Chez Scheme v10.0.0, `c/prim5.c` (`s_fxrandom`, `s_flrandom`,
`s_random_seed`, `s_set_random_seed`), `c/number.c` (`S_random_double`) and
`s/5_3.ss` (`random`, `random-seed`). The comment there says the formula is
"based on Knuth". Note that Chez 10's `make-pseudo-random-generator` objects
(MRG32k3a, the same as Racket's) are a *different* generator from the global
`random`; Metacat uses only the global one, and Racket's `random` must not be
used for it.

### State
One unsigned 32-bit integer `S` (per thread in Chez; Metacat is single-threaded).

    step(S) = (S * 72931 + 90763387) mod 2^32

Each call of `step` replaces `S` with the new value; "the next value" below
means: perform `step` and take the new `S`.

### `(random-seed)` and `(random-seed n)`
`(random-seed)` returns `S`. `(random-seed n)` sets `S := n`; `n` must be an
exact integer with 1 ≤ n ≤ 2^32 − 1 (0 is rejected). The initial state of a
fresh Chez process is irrelevant: Metacat always seeds (`init-mcat`).

### `(random n)`, exact positive fixnum `n`
    s1 = next value; s2 = next value
    t  = floor(s1 / 2^16) + (s2 AND #xFFFF0000)     ; high halves of s1 and s2
    if n <= 2^32 - 1:  result = t mod n              ; 2 steps
    else:              s3 = next; s4 = next          ; 4 steps
                       t = (t * 2^16 + floor(s3 / 2^16)) mod 2^64
                       t = (t * 2^16 + floor(s4 / 2^16)) mod 2^64
                       result = t mod n
(`t` lives in a 64-bit `uptr`, hence the `mod 2^64`.) Bignum `n` (beyond
`most-positive-fixnum`) loops over fixnum draws; Metacat never draws with
such an `n` and the port need not support it.

### `(random x)`, positive flonum `x`
    s1, s2, s3, s4 = the next four values               ; 4 steps
    M = (floor(s1/2^16) mod 16) * 2^48 + floor(s2/2^16) * 2^32
        + floor(s3/2^16) * 2^16 + floor(s4/2^16)        ; 52-bit mantissa
    result = (1.M - 1.0) * x  =  fl*(M / 2^52, x)
The C code builds the double `1.M` (exponent 0) and subtracts 1.0, which is
exact, so the result is the flonum `M/2^52` (exactly representable) times
`x` with one IEEE multiplication. `(random 1.0)` is `M/2^52` exactly.

### What Metacat draws
`(random 1.0)` in `stochastic-if*`, `prob?`, `stochastic-pick` and friends;
`(random n)` with small `n` in `random-pick` etc.; `(random-seed seed)` in
`init-mcat`. Without a seed the Control Panel calls `randomize`
(seed from `(real-time)`) and then reads `(random-seed)`; `run.ss` does the
same when `--seed` is omitted, so such runs are not reproducible.

### The demo seeds replay
TASK.md and item 01 expected the seeds in `demos.ss`, chosen under the
1999-era Chez generator, not to replay. They do: the generator above appears
unchanged since then, and `chez_scheme/oracle/tests/demo-replay-check.ss`
confirms that five documented runs come out exactly as the comments in
`demos.ss` say (answer and time step): misc1 (justifies mmmrrj, 7794 steps),
misc2 (justifies abd, 1126), misc4 (b at 453, then y at 945), misc5 (flz, dlz,
then hlz at 1721) and the commented-out misc9 (dyz at 2257). misc3 does not
replay exactly (documented: kji 1240, kkkjjjiii 1470, kkjjii 1485; oracle:
kkjjii 1240, kkkjjjiii 1264, kkjjii 1280, kji 1317), nor do the "not used"
misc6–misc8. The dissertation's Chapter 5 runs (run1–run8, fig5.x) have not
been compared yet; that needs the dissertation's figures (item 12).

### Evaluation order: the other half of reproducing the draws
Reproducing the generator is not enough: the *order* of draws must match.
Chez does not evaluate procedure-call arguments left to right. Observed under
`scheme --script` (which is how the oracle loads the original):

    (f (show 1) (show 2) (show 3))   prints 312   ; user procedure, 3 args
    (list (show 1) (show 2) (show 3)) prints 312
    (let ((a (show 1)) (b (show 2))) ...) prints 21
    ((lambda (x y) x) (show 1) (show 2)) prints 21
    (+ (show 1) (show 2))             prints 12   ; inlined primitive
    (cons (show 1) (show 2))          prints 12

Racket evaluates left to right. Wherever the original has two or more
argument (or `let`-binding) expressions that draw random numbers or have
other order-dependent side effects (posting codelets, building structures,
trace events), the port must spell out Chez's order explicitly, and the
golden traces are the check. Each such site goes in `porting-notes.md`.

## Trace format (item 02)

A trace is a JSON-lines file: one JSON object per line, each line ending in
`\n`, no blank lines. The oracle writes it with
`chez_scheme/oracle/run.ss ... --trace FILE` (instrumentation in
`chez_scheme/oracle/trace.ss`); the golden traces in `tests/golden/` come from
`chez_scheme/oracle/make-golden.ss` (problem list `tests/problems.txt`). The
Racket port must write the **same bytes** for the same run, so everything
below, down to field order and number formatting, is part of the format.
`chez_scheme/oracle/validate-trace.py` checks the structure.

### Lines
Every line is an object whose first two fields are
- `t`: `*codelet-count*` when the event happened, i.e. the number of
  codelets that had *finished* running. Events during the codelet with
  `t = k` (the (k+1)th codelet) carry `t = k`; the updates that run-mcat
  makes after codelet k finishes carry `t = k+1`. Never decreases.
- `ev`: the event type, one of the twelve below.

The remaining fields follow, **in the order listed**. No spaces anywhere
(`{"t":0,"ev":"codelet",...}`).

### Values
- `true`/`false`; `null` for a missing object (Scheme `#f` where an object
  was expected, e.g. a bond without direction).
- Strings: `"` and `\` escaped as `\"` and `\\`, newline `\n`, tab `\t`,
  other control characters `\u00XX` (uppercase hex, e.g. `\u001B`). Everything else
  written as is (the model only produces ASCII).
- Symbols: their name as a string.
- Numbers: an exact integer as decimal digits (`-` if negative). **An exact
  non-integer rational as a string `"n/d"`** in lowest terms, e.g.
  `"102/5"`: Chez arithmetic stays exact (codelet urgencies computed from
  activations come out like this), so the port must keep exact rationals
  exact, not convert them to floats. A finite flonum as Chez's
  `number->string`: the shortest digits that read back to the same double,
  with `.0` if there is no fraction or exponent (`21.0`, `0.5`,
  `0.3333333333333333`, `1e-7`, `1e21`, `1.2345678901234567e19`; Racket
  writes `1e-07`, so the port must reformat exponents); a subnormal loses
  Chez's `|53`-style precision suffix. A non-finite flonum as the string
  `"+inf.0"`, `"-inf.0"` or `"+nan.0"`. (No flonum occurs in the current
  golden traces; the only non-integers are rational urgencies.)
- Names of model objects (`$name` in trace.ss): a slipnode by its short
  name (`succ`, `LetterCtgy`, `lmost`); a letter by its `ascii-name`
  (`a:0` = letter a at position 0); a group by its `ascii-name`
  (`[>]:*` = rightward group spanning the string, `[3]:0,2`, … ; `null` if
  not set yet); a workspace string by its `generic-name` (`initial string`,
  `modified string`, `target string`, `answer string`, with `translated `
  in front for a translated string); a concept mapping by its `print-name`
  (`lmost=>rmost`, `LettCtgy=>LettCtgy`).

### Event types
| `ev` | when | fields after `t`, `ev` |
|---|---|---|
| `start` | first line, before `init-mcat` | `format` (1), `problem` [initial, modified, target, answer or `null`], `seed`, `max_codelets` (or `null`), `keep_going`, `slipnodes` (names of `*slipnet-nodes*`, in order; `slipnet` events follow this order) |
| `codelet` | a codelet is chosen (in `step-mcat`, after `choose-codelet`, before it runs) | `type` (codelet type name), `urgency` (its relative urgency), `posted` (its time stamp: `*codelet-count*` when it was put on the Coderack), `rng` (generator state `S` right after the choice, i.e. the state the codelet starts from) |
| `build` | entry to `build-bond`, `build-group`, `build-bridge`, `build-description`, and the workspace's `add-rule` (rules, from `rule-builder` and justify.ss) | `kind`, then the structure fields below; a group also has `flipped` (the `flipped?` argument) |
| `break` | entry to `break-bond`, `break-group`, `break-bridge` (nested breaks follow in the order the original makes them) | `kind`, then the structure fields |
| `temperature` | after each `update-temperature` (every 15 codelets) | `value` (`*temperature*`), `clamped` (`*temperature-clamped?*`) |
| `slipnet` | after each `update-slipnet-activations` (every 15 codelets, after the Themespace update) | `activations` (one per slipnode, start order), `rng` (generator state) |
| `themes` | after each slipnet update, **only if** different from the previous `themes` line | `active` (active theme types), `themes`: one `[theme-type, dimension, relation, activation, frozen]` per theme, in the Themespace's order (`get-complete-state`) |
| `event` | the Temporal Trace records an event (`add-event` sends it to the Trace window) | `type` (`answer`, `snag`, `clamp`, `rule`, `group`, `concept-mapping`, `concept-activation`), `number` (event number), `name` (its `print-name`: `[Answer xyd]`, `[Snag]`, `[Clamp]`, `[Top Rule]`, `>x-y-z>`, `T:lmost=>lmost`, `(Identity)`, …), `time`, `temperature` (as recorded in the event) |
| `answer` | an answer is reported (`abstract-answer-description`, called once per answer by `report-new-answer`) | `answer` (answer string), `quality`, `temperature` |
| `comment` | a Commentary paragraph is drawn | `text` |
| `halt` | the original calls `report-error-and-halt` (an object got a message it does not understand) | `message` (the message name), `object` (the receiver's object type) |
| `end` | last line | `reason`: `suspend` (the original paused for Go after an answer or giving up), `cap` (`--max-codelets` reached) or `halt`; `temperature`, `answers` (in order found), `rng` |

Structure fields, by `kind`:
- `bond`: `string`, `from`, `to`, `category` (bond category), `direction`
  (`null` for sameness), `facet` (bond facet).
- `group`: `string`, `name`, `category` (group category), `direction`,
  `facet`, `objects` (constituent objects, in the group's order).
- `bridge`: `type` (`top`, `bottom`, `vertical`), `object1`, `object2`,
  `mappings` (all concept mappings, the bridge's order).
- `description`: `string`, `object`, `type` (description type),
  `descriptor`.
- `rule`: `type` (`top`/`bottom`), `english` (the rule's English
  transcription, a list of strings).

Example (`tests/golden/abc-abd-xyz_692549763.jsonl`):

    {"t":0,"ev":"codelet","type":"bottom-up-bridge-scout","urgency":21,"posted":0,"rng":1229177231}
    {"t":15,"ev":"temperature","value":100,"clamped":false}
    {"t":48,"ev":"build","kind":"bond","string":"initial string","from":"a:0","to":"b:1","category":"succ","direction":"right","facet":"LetterCtgy"}
    {"t":2257,"ev":"answer","answer":"dyz","quality":72,"temperature":24}
    {"t":2257,"ev":"end","reason":"suspend","temperature":24,"answers":["dyz"],"rng":4247345911}

### Counting codelets
There is one `codelet` line per codelet run, with `t` = 0, 1, 2, … in turn.
A run that ends with `cap` stops between codelets, so it has `end.t` codelet
lines; one that ends with `suspend` or `halt` stops *inside* codelet number
`end.t` (the count is not incremented), so it has `end.t + 1`. (The Answer
lines of run.ss and the times in demos.ss are `end.t`.)

### What the trace leaves out, on purpose
Codelets that fizzle (no event; the next codelet line shows the run went
on), proposals and evaluations of structures, codelets posted, description
removal by `delete-invalid-string-position-middle-descriptions`, and
slipnode activations between updates. The `rng` fields locate the first
divergent random draw to within one codelet or update cycle, which is
what a debugging port needs most.

### Tracing changes nothing
Every wrapper reads model state only through getters (`get-...`,
`print-name`, `ascii-name`, `generic-name`, `object-type`) and then calls
the original procedure with the original arguments. `*coderack*` and
`*workspace*` are replaced by forwarding closures (`(lambda msg (apply
original original (cdr msg)))`, so `self` inside is still the original
object). Reading `(random-seed)` does not draw.
`chez_scheme/oracle/tests/trace-check.ss` checks that a traced run prints
exactly what an untraced run prints.

### The golden set (tests/problems.txt)
109 runs: the 25 problems of `demos.ss` (sample runs of section 5.2,
answer-comparison families of 5.2.3, figures 5.4–5.11, misc1–misc8) with
their documented seeds, plus small seeds to make 3–5 per problem; and 11
classic problems discussed in the dissertation (`abc→abd` with `mrrjjj`,
`ijk`, `iijjkk`, `kji`, `kkjjii`; `rst→rsu; xyz`; `xqc→xqd; mrrjjj`;
`eqe→qeq; abbba`; `apc→abc; opc`; `abc→ccbbaa; ijk`; `abc→aabbdd; ijkl`)
with seeds 1–3. Caps are 10000 codelets (17000 for `eqe qeq abbba
aaabaaa`, whose documented run answers at 16668); misc3–misc5 run with
`keep-going` until just after their last documented answer. Each run takes
1–8 s; `make-golden.ss` runs them in parallel (`nproc` jobs), about 13 s
on 32 cores. Total about 40 MB uncompressed, under 5 MB compressed.

### The port's traces (items 10-11)
`racket/headless.rkt` (since item 11; `racket/tests/golden-harness.rkt` in
item 10) writes the same format from the Racket engine, and
`racket racket/cli.rkt ... --trace FILE` writes it to a file: the same JSON writer (exact rationals as `"n/d"`, flonums through
compat.rkt's Chez `number->string`), the same wrappers set through the
engine's `set-global!`, the same headless windows, and the same driver.
`racket/tests/golden-test.rkt` runs every golden of `tests/problems.txt`
in a fresh engine and requires byte-for-byte equality; since item 10 all
109 match, and since item 11 with the engine's own run.ss.
