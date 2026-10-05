# Porting notes

Every place where the Racket port renames, restructures or modernises the
original (because Racket forces it), and every iteration-order or RNG subtlety
found while porting. One entry per change: original file and definition, what
the port does instead, and why.

## Layout
- The port lives in `racket/` (engine modules directly in the folder, GUI in
  `racket/gui/`). `racket/main.rkt` is the GUI entry point and `racket/cli.rkt`
  the headless one. Chez-isms are collected in `racket/compat.rkt`.

## The headless oracle (item 01)
How `chez_scheme/oracle/prelude.ss` gets the unmodified original to run under
Chez 10 without SWL, and what that reveals for the port.

- **Loading.** `metacat.ss` is loaded as is. The prelude defines the three
  settings it insists on (`*platform*`, `*metacat-directory*`,
  `*file-dialog-directory*`), empty modules `swl:oop` … `swl:threads` for its
  `import`s, and `extend-syntax` as a `syntax-case` macro. Fenders and `with`
  bindings refer to pattern variables as quoted data (`'formal`,
  `'(token ...)`), so after substitution they are evaluated with `eval` at
  expansion time; `with` results get the lexical context of the macro use
  (they name top-level variables such as `plato-a`, `a-b-link`). The port
  replaces all 22 macros with `syntax-rules`/`syntax-case` (item 03); the
  only fenders are the `for*` shape tests and `mcat`'s token check.
- **SWL stand-ins.** `make`/`create` build inert records, `send` ignores
  everything except `get-actual-values` on fonts (fonts.ss reads the size
  back at load time), `define-class` (sgl-interpreter.ss's `<viewport>`) is
  skipped, `swl:font-families`, `swl:screen-width`, threads and message
  queues are no-ops. None of this runs after loading except through windows.
- **Windows.** The oracle never calls `(setup)`. The window globals of
  `setup.ss` get null objects that accept only the messages a headless run
  sends and raise an error on any other, so a display query whose answer
  could feed back into the model cannot pass silently. The graphics
  switches `%workspace-graphics%`, `%slipnet-graphics%`, `%coderack-graphics%`
  are turned off. Messages still sent with the display off (in 12 runs of
  4000 codelets, 4 problems × 3 seeds, `--keep-going`):
  `*workspace-window*` garbage-collect, caching-on, flush (from
  `group-graphics 'erase`, called ungated at groups.ss:727);
  `*themespace-window*` erase-all-themes, update-thematic-pressure,
  update-graphics, set-theme-graphics-parameters-and-draw, garbage-collect;
  `*memory-window*` add-memory-icon, draw; `*trace-window*` initialize,
  add-event; `*temperature-window*` initialize, update-graphics;
  `*EEG-window*` initialize; `*slipnet-window*`, `*coderack-window*` clear;
  `*control-panel*` set-verbose-step-mode. All are commands whose results
  are ignored. The port's headless graphics interface must accept the same.
- **Model state set by the graphics.** Two places where the model calls
  something that only a window installs, even with the display off:
  1. each codelet type's private `coderack-window` (coderack.ss), set by
     `set-graphics-parameters` from coderack-graphics.ss; a codelet's `run`
     always sends it `set-last-codelet-type`. The oracle installs a null
     window in every codelet type.
  2. answer and snag descriptions in memory.ss call the icon-drawing
     procedure `get-normal-icon-pexp` that the Memory window's
     `add-memory-icon` gives them (memory-graphics.ss), e.g. in
     `update-activation`. The headless memory window gives them one that
     draws nothing.
  The reverse also exists: some graphics-gated code sets model-object
  state, e.g. `set-shrunk-singleton?` on groups (groups.ss, under
  `%workspace-graphics%`). Item 12+ must check that such state never feeds
  back into the run, or a GUI run would differ from a headless one.
- **Run control.** `break`/`quiet-break` (run.ss) wait at the REPL for
  `(go)`. `chez_scheme/oracle/run.ss` rebinds both top-level variables after
  loading: by default the run ends at the first `suspend` (answer or give-up);
  with `--keep-going` they return at once, which is what `(go)` amounts to
  (the break continuation returns `'ignore`). `--max-codelets K` is the
  original's own `*break-time*` (`runtil`). Answers are reported by wrapping
  `abstract-answer-description`, which `report-new-answer` calls once per
  answer; commentary is the original Commentary window (`make-comment-window`)
  drawing on a recording text window.
- **Output port.** syntactic-sugar.ss redefines `printf`/`newline` to write
  to the `current-output-port` captured at load time, so the prelude
  installs an unbuffered forwarding port first, muted while loading (hides
  "Metacat loaded…") and live afterwards.
- **Evaluation order** and the **RNG**: see `trace-format.md`. Chez does
  not evaluate call arguments left to right (`(f a b c)` evaluates c, a, b
  in the observed case); every site where that changes the order of random
  draws or other side effects must be ported with an explicit order.

## Traces (item 02)
- **Instrumentation from outside.** `chez_scheme/oracle/trace.ss` wraps
  top-level procedures with `set!` after loading (`build-bond`,
  `break-bond`, `build-group`, `break-group`, `build-bridge`,
  `break-bridge`, `build-description`, `update-temperature`,
  `update-slipnet-activations`, `abstract-answer-description`,
  `report-error-and-halt`); callers reach them through their top-level
  bindings, including the recursive `break-group`. Codelet procedures
  cannot be wrapped that way (`define-codelet-procedure*` hands the
  procedure to its codelet type at load time), so codelets are seen by
  forwarding `*coderack*`'s `choose-codelet`, and rules by forwarding
  `*workspace*`'s `add-rule`. The forwarding closures pass the original
  object as `self`. Note that top-down codelets receive `*workspace*` as
  their scope argument (slipnet.ss), so they hold the forwarder; only
  `tell` is ever applied to it. (Written before the port had traces. In
  the end the port does the same: racket/headless.rkt's `install-trace!`
  installs the same wrappers and forwarders, with `set-global!` instead of
  `set!`; item 10.)
- **Exact rationals.** Codelet urgencies are often exact non-integer
  rationals (`(* (% conceptual-depth) activation)` and friends: 3601 of the
  golden codelet lines, e.g. `102/5`). Racket's numeric tower keeps them
  exact as Chez does; the port must not introduce flonums where Chez has
  exact arithmetic, and the trace writes them as `"n/d"` strings.
- **The original fails on some runs.** Two kinds, both in the model, with
  the display off and no tracing:
  1. `report-error-and-halt` (an object gets a message it does not
     understand) prints `Ooops: ...` and calls `(reset)`; under the SWL
     REPL this abandons the run, under `scheme --script` it exits 255.
     Seen on `eqe qeq abbba aaabaaa` seed 3 at codelet 4004
     (`answer-justifier` sends `get-constituent-objects` to a letter).
     run.ss prints the same message and ends the run (`Stopped: halt`);
     the trace has a `halt` event. The golden set includes this run, so
     the port must halt at the same point.
  2. A Chez error: `abc ccbbaa ijk` seed 3, `caddr` of `#f` in
     `transcribe-to-english` (rules.ss), called from `make-rule`. Under
     the SWL REPL this too would abandon the run. The golden set avoids it
     (seed 4 instead); the oracle exits 1 with a backtrace. Whether these
     happened under the 1999 Chez is unknown (a different argument
     evaluation order could change the path).
- **Seeds that do not replay.** Besides misc3 (item 01), run4
  (`abc abd xyz dyz` 2836825623, documented answer dyz) gives up without
  an answer at codelet 3228 in the oracle. It is kept in the golden set:
  the golden records what the oracle does. (Item 16: the dissertation's
  Run 4 gives up at 3228 too, so run4 does replay. The full comparison is
  in docs/demos.md.)

## The compatibility layer (item 03)
`racket/compat.rkt` (syntactic-sugar.ss plus Chez built-ins) and
`racket/utilities.rkt` (utilities.ss, line for line). Engine modules require
both; their bindings shadow racket/base's. Everything below is checked
against Chez by `racket/tests/utilities-diff-test.rkt`, which evaluates
`tests/diff/utilities-battery.scm` (196 tests) both under Chez with the
whole original loaded (`chez_scheme/oracle/diff-eval.ss`) and in Racket, and
compares the outputs line for line.

**Shadowed or added built-ins** (each reproduces Chez 10):
- `random`, `random-seed`: Chez's generator (trace-format.md). Racket's
  `random` is never used. Bignum ranges above 2^60−1 are rejected (Metacat
  never draws with them).
- `if`: one-armed `(if test then)` is legal in Chez and common in the
  original; compat's `if` adds `(void)` as the missing arm.
- `map`: **Chez's application order.** For one or two lists Chez's library
  map applies the procedure to pairs of elements from the end towards the
  front (7 elements: 7 5 6 3 4 1 2); for three or more lists, last to
  first. Racket goes first to last. This matters wherever the mapped
  procedure draws random numbers or has other effects (`tell-all`,
  `delegate-to-all`, …).
  *Caveat for porting call sites:* Chez's compiler inlines `map` when a
  list argument is a literal `(list …)` or a quoted list of at most four
  elements, and the inlined order is the compiler's (observed: 3 2 1 for a
  quoted 3-element list in one context, 1 2 3 in another). Such sites with
  side-effecting procedures must be checked against the goldens.
- `for-each`: Chez returns the value of the last application (void for an
  empty list); the original's `for*` loops pass that value on.
- `sort`: `(sort pred list)` with Chez 10's own algorithm (s/5_6.ss):
  below 25 elements a top-down list merge sort that sorts the *second* half
  first, otherwise Shivers's opportunistic vector merge sort. The battery
  compares results for non-strict predicates (`<=`, `>=`) and the sequence
  of predicate calls, both of which differ from Racket's `sort`.
- `remq`, `remv`, `remove`: remove *every* occurrence (Racket's `remq` and
  `remove` remove only the first). The model calls `remq` about 50 times.
- `1+`, `-1+`.
- `number->string`, `display`, `write`, `format`, `fprintf`, and
  syntactic-sugar.ss's `printf`/`newline`: Chez's printer. Flonums are
  positional when the exponent of the leading digit is in (−4, 10) and
  `d.ddde<exp>` otherwise (`1e-4`, `1e10`, `1.234567890125e11`,
  `1000000000.0`), with Chez's `|n` precision suffix on subnormals.
  `display` abbreviates `(quote x)` as `'x`, `write` does not. Symbols are
  written with Chez's `\xHH;` escapes (`\x31;+`, `a\x20;b`), characters with
  Chez's names (`#\nul`, `#\delete`), strings with Chez's escapes.
  Procedures print as `#<procedure name>` (Chez also prints source
  positions for anonymous ones; not reproduced, the model never prints
  procedures). Directives: `~a ~s ~% ~n ~~`, the only ones Metacat uses.
  `printf` writes to the current output port at the time of the call, where
  syntactic-sugar.ss captured the port current when it was loaded.
- `error`: Chez's `(error who format-string arg …)`, `who` may be `#f`.
- `record-case`: dispatches with `case` on `(car exp)`, binds the formals
  with `apply`.
- `reset`/`reset-handler`: Chez's `(reset)` abandons the computation (the
  REPL's reset handler; under `--script` the process exits 255). compat's
  default handler raises a `metacat-reset` value, for the run loop to catch;
  `report-error-and-halt` reaches it through `tell`.
- `collect` (a no-op: the original calls `(collect 4)` between runs),
  `real-time` (milliseconds, only used by `randomize`).
- **Top-level values.** `define-top-level-value`, `set-top-level-value!`,
  `top-level-value`, `top-level-bound?` work on one table, because Racket
  modules have no global environment. The original creates top-level
  variables at run time from computed names (`establish-link` in slipnet.ss
  names each link `a-b-link`), and utilities.ss's
  `symbol->letter-categories` reads `plato-a` etc. with `eval`; the port
  reads them with `top-level-value`. `reveal-obj` (a debugging aid) calls
  `format-slipnode` (rules.ss) through `(top-level-value 'format-slipnode)`,
  so the rules port must register it there. (Item 09 missed this; item 17
  found it and registers it in racket/engine.rkt. jootsing.ss calls
  `reveal` in verbose mode.)

**The 22 macros.** Written with `syntax-case` rather than `syntax-rules`
where they must build identifiers (`plato-` + name, `a-b-link`) or check
fenders. Differences forced by Racket:
- extend-syntax keywords (`each in from to do times forever until -->
  <--> length: label: all-lengths: conceptual-depth: urgency:`) are matched
  by name, so a local variable called `from` or `to` does not break `for*`.
- Names that the original resolves in the global top level where the macro
  is used (`tell`, `*coderack*`, `*control-panel*`, `%verbose%`,
  `say-object`, `print`, `make-slipnode`, `establish-link`,
  `make-codelet-type`, `rotate-90-degrees-clockwise`, the `plato-` nodes)
  get the lexical context of the macro keyword, so they refer to the
  engine's bindings at the use site. compat itself does not depend on
  utilities (it keeps a private copy of `ascending-index-list`).
- `slipnet-node-list*` and `codelet-type-list*` are used as expressions in
  `(define *slipnet-nodes* (slipnet-node-list* …))`, and define a global
  per node or type. The expression forms register top-level values only;
  the module-level forms `(define-slipnet-node-list* *slipnet-nodes* …)` and
  `(define-codelet-type-list* *codelet-types* …)` also define each name as a
  module-level variable, in the same order of `make-slipnode` calls. The
  link macros look the new link up with `top-level-value`.
- `fizzle` is a compat variable; codelets set it through `set-fizzle!`,
  since Racket forbids `set!` on an imported variable.
- `continuation-point*` uses `call/ec`. The original uses `call/cc` only in
  this macro, and only to escape upwards (`return`, `fizzle`, `fail`);
  Racket's full `call/cc` captures up to the nearest prompt and misbehaved
  inside rackunit checks. A late jump now raises an error instead of
  re-entering.
- `for*` from/to evaluates the bounds first `exp1`, then `exp2`, as the
  oracle does.
- At module level, Racket prints the value of an expression form (e.g. the
  `'done` returned by `define-codelet-procedure*` or a link macro); the
  engine modules must discard those values (e.g. a module language that
  wraps top-level expressions in `void`).

**utilities.rkt** is utilities.ss with these changes (marked `port:`):
`scheme-round` etc. come from `(only-in racket/base [round scheme-round])`
rather than `(define scheme-round round)`, because a module-level
`(define round …)` shadows the import for the whole module; `ask` peeks the
first character (no `unread-char`) and gets a `clear-input-port`;
`symbol->letter-categories` and `reveal-obj` use `top-level-value` (above);
`pause` uses `sleep`; `clear-input-port` is defined here (Racket has none);
`set-report-error-and-halt!` lets headless drivers replace
`report-error-and-halt` (item 10); and **`pairwise-map` evaluates its recursive call
before the `map`**, as Chez evaluates `(append (map …) (pairwise-map …))`.

**Evaluation order, more observations.** Under `scheme --script`, Chez's
order of argument evaluation depends on the shape of the call:
`(g s1 s2 s3 s4)` → 3 4 1 2, `(g s1 s2 s3 4)` → 3 1 2, `(g s1 2 s3 s4)`
→ 3 4 1, `(g 1 s2 3 s4)` → 4 2, `(g s1 s2 3 4)` → 1 2,
`(+ s1 s2 s3)` → 3 1 2, `(cons s1 s2)` → 1 2, `(list s1 s2 s3 s4)` → 1 2 3 4,
a 2-binding `let` → 2 1 but a 3-binding `let` → 1 2 3 (all at top level;
inside procedures it can differ). Inside a procedure, `(append s1 s2)` → 2 1 (item 07,
groups.ss's `get-local-density`). There is no simple rule; each port of a
call with two or more effectful arguments is checked against the oracle,
by a logging test or by the goldens.

**Other Chez/Racket differences to watch for in the engine:**
- Racket interns literal strings and flonums (`read-syntax`), so two
  literals `"a"` or `1.5` are `eq?` in Racket but not in Chez. Values
  computed at run time behave alike. `memq`/`eq?` on string or flonum
  literals in the model must be checked when ported.
- Pairs are immutable in Racket. The model never mutates pairs; only
  rule-graphics.ss:77 (`set-car!` on a picture expression) does, and the GUI
  port must restructure it.
- `(ascending-index-list 0)` loops forever in the original (so does
  `for-each-vector-element*` on an empty vector); the port keeps this.
- An object that does not understand `object-type` sends
  `report-error-and-halt` into infinite recursion, in both.
- Chez reads `0+1.0i` as `0.0+1.0i`; Racket keeps an exact zero real part.
  Only the graphics use complex numbers (`coord`).

## The engine's module structure (item 04)
**Decision: one engine module, `racket/engine.rkt`, that `include`s the
ported files, `racket/engine/*.rktl`, in the load order of `metacat.ss`.**
compat.rkt (syntactic-sugar.ss and Chez built-ins) and utilities.rkt
(utilities.ss) stay separate modules that the engine requires, since they
need nothing from later files.

Why, against the alternative of one module per file plus a shared-state
module:
- The original's 44 files are loaded into one global top level. Definitions
  refer to each other across files in both directions (coderack.ss names
  the codelet procedures of eleven later files; descriptions.ss calls
  `make-workspace-structure`, `contains?`, `temp-adjusted-probability`, ...;
  those files call `*coderack*` back). Racket forbids cyclic module
  dependencies, so per-file modules would need every forward reference
  rewritten as an indirection (parameters, boxes, late-bound hooks): about
  4,000 `tell` call sites are fine, but hundreds of direct calls would change.
- Files `set!` each other's globals (`*temperature*`, `*codelet-count*`, the
  mode switches, the window globals, `*workspace*`, ...). An importer cannot
  `set!` a module variable; a shared-state module would turn each into a
  getter/setter pair, changing code all over the model.
- Inside one module, the semantics are those of Chez's top level loaded in
  order: any procedure body may refer to any definition, module-level
  expressions run in load order, and a reference to a not-yet-defined
  variable at load time fails in both. Each `.rktl` stays a line-for-line
  copy of its `.ss` file, which keeps diffs against the original small.
- Cost: compiling the engine compiles all included files together (under
  a second now; `raco make` caches it), and the files are not separately
  testable modules. The differential batteries test them through the
  engine's exports instead.

Mechanics:
- **`racket/engine-lang.rkt`** is the engine's module language: racket/base
  whose `#%module-begin` partially expands each form and wraps expressions
  (not definitions, requires, provides) in `void`, so that values such as the
  `'done` of `define-codelet-procedure*` are discarded as at Chez's top
  level instead of printed. `include` splices through `begin`, which it
  handles form by form.
- **`racket/engine/pending.rktl`** defines stand-ins for names that the
  ported files refer to but whose files are not ported yet (procedures that
  raise "not ported yet"; variables holding `#f`), grouped by original file,
  including the graphics constants coderack.ss refers to. Each later item
  deletes the names of the files it ports; forgetting to is a duplicate
  definition, which Racket rejects at compile time. Nothing there is used at
  load time.
- **`set-global!`** (exported by engine.rkt) is how anything outside the
  engine (tests, the future CLI and GUI) sets an engine global:
  `(set-global! '*temperature* 50)`. It is a `case` over an explicit list of
  names; each item adds the globals of the files it ports that are set from
  outside. Reading works through the normal exports
  (`(provide (all-defined-out))`), which see the current value of a mutated
  variable. Listing a variable there also makes it mutable, so Racket does
  not inline it as a constant.
- Note for tests: a fresh namespace instantiates its own copy of the engine,
  so the battery runner takes `set-global!` from inside the battery's
  namespace (`racket/tests/diff-runner.rkt`).

Per-file notes:
- **constants.ss**: only the probability distributions
  (`make-probability-distribution` and the five
  `%...-translation-temperature-threshold-distribution%`) are model
  constants; the window sizes, colours, fonts and titles wait for the GUI
  items. The colours and fonts coderack.ss refers to (`urgency-color`, the
  codelet-type graphics methods) are stand-ins in pending.rktl.
- **setup.ss**: the globals and the user commands are ported unchanged;
  `setup` and `enable-resizing` create and arrange the windows, so they move
  to the GUI layer (racket/gui/), which will install its windows with
  `set-global!`.
- **coderack.ss**: unchanged except that `(define *codelet-types*
  (codelet-type-list* ...))` becomes `(define-codelet-type-list*
  *codelet-types* ...)`, which also defines each codelet type as a
  module-level variable (`breaker`, `rule-scout`, ...) in the same order of
  `make-codelet-type` calls. The coderack draws only through
  `stochastic-pick-by-method` (bin choice weighted by urgency sums; deletion
  weighted by `get-removal-weight`), `random` (codelet within a bin) and
  `random-pick` (excess deferred codelets); no call has two effectful
  arguments, so evaluation order does not matter here.
- **descriptions.ss**: unchanged. `make-description`'s two-binding `let`
  is evaluated right to left by Chez (make-workspace-structure first) and
  left to right by Racket; both bindings are free of side effects (to be
  confirmed when workspace-structures.ss is ported). The description codelets
  need the Workspace and Slipnet; they are checked by the golden traces once
  those are ported.
- **Chez `case`**: Chez accepts a single datum as a clause key,
  `(case x (rule-scout ...))`, meaning `((rule-scout) ...)`; coderack.ss
  uses this. compat.rkt now exports a `case` that wraps such keys. Chez
  compares keys with `eqv?`, Racket with `equal?`; the model's keys are
  symbols and numbers, where they agree.
- **Codelet types and the Coderack window.** As in the oracle, a codelet's
  `run` tells its type's private `coderack-window` `set-last-codelet-type`
  whatever the graphics switches say; that window is `#f` until
  `set-graphics-parameters`. The headless driver (item for run.ss/cli.rkt)
  must install a null window in every codelet type, as
  `install-headless-windows!` does in the oracle prelude.

**Tests** (`racket/tests/coderack-diff-test.rkt`, battery
`tests/diff/coderack-battery.scm`, 43 tests): the setup.ss defaults; the
urgency table (7 bins × 101 temperatures); `urgency-name` and bin selection
for integer, rational and flonum urgencies; bin urgencies at every
temperature; codelet-type lists and graphics labels; posting with time
stamps, bin indices and codelet order; `choose-codelet` over 8 seeds × 7
temperatures with the generator state after every choice; emptying the
coderack; overflow deletion (with and without proposed structures, which
are reported to the Workspace); removal weights; deferred posting below,
at and above the size limit; clamp/unclamp, adjust/set/reset urgencies and
choosing while clamped; codelet accessors, `run` and `fizzle`; printing;
`post-codelet-probability`, `num-of-codelets-to-post`,
`bottom-up-urgency`, `add-bottom-up-codelets` and `add-top-down-codelets`
with fake Workspace, Themespace, Trace and top-down slipnodes; the threshold
distributions; the setup.ss commands; `descriptions-equal?` and
`description-member?`. The battery helpers moved to `tests/diff/helpers.scm`,
which both runners load first.

## The Slipnet and images (item 05)

- **slipnet.ss**: unchanged except that `(define *slipnet-nodes*
  (slipnet-node-list* ...))` becomes `(define-slipnet-node-list*
  *slipnet-nodes* ...)`, which also defines each `plato-...` node as a
  module-level variable, in the same order of `make-slipnode` calls. The
  link macros (`lateral-link*`, ...) name each new link only as a top-level
  value (`a-b-link`, via `establish-link`'s `define-top-level-value`) and
  message it with `(top-level-value 'a-b-link)`; no other file refers to a
  link by its name. The module-level code (top-down codelet types, intrinsic
  link lengths, descriptor predicates, the 202 links) runs at load time as in
  the original, after coderack.rktl has defined the codelet types.
- **images.ss**: unchanged.
- **Stand-ins** (engine/pending.rktl) that these files need until their
  files are ported: `%update-cycle-length%` (run.ss's constant 15, which
  slipnode `reset` uses for the decay rate; it is a value, not `#f`),
  `make-letter` (workspace-objects.ss), `make-group` (groups.ss),
  `make-group-pexp` (group-graphics.ss), `monitor-slipnode-activation-change`
  (trace.ss). `monitor-slipnode-activation-change` and
  `temp-adjusted-probability` are in `set-global!`'s list so that the battery
  can replace them by logging fakes in both runners (Chez:
  `set-top-level-value!`, as the original refers to them as top-level
  variables). `*top-down-slipnodes*` is now defined by slipnet.rktl and stays
  settable.
- **Random draws and order.** The Slipnet draws in
  `update-slipnet-activations` (one `stochastic-if*` per partially active
  node, in `*slipnet-nodes*` order: a jump to full activation with
  probability (a/100)^3), in `get-similar-property-links` (one `prob?` per
  property link), in `apply-slippages` (coattail slippages) and in
  `attempt-to-post-top-down-codelets` (`stochastic-if*` per codelet type).
  No call has two effectful arguments. Activation arithmetic is exact
  (`rate-of-decay` = 1 − depth/100, spread = round(assoc/100 × activation)
  with utilities.ss's exact `round`), so "the last bit" is exact equality.
- **Order in images.** `replace-all` and `tell-all` use `map` with side
  effects, and an operation can escape midway through its `fail`
  continuation, leaving the images it already changed changed. Which ones
  depends on the order in which `map` applies its procedure, so compat.rkt's
  Chez-order `map` matters here. The battery's `replace-all-fail` case
  catches it (a left-to-right `for-each` in `replace-all` fails it).

**Tests** (`racket/tests/slipnet-diff-test.rkt`, battery
`tests/diff/slipnet-battery.scm`, 48 tests, run by Chez and Racket with
identical output, about 1.1 MB): the initial slipnet as loaded (every node's
names, depth, activation, link lengths, category/instance relations; every
link list of every node with type, ends, label, length and degrees of
association; 202 links; nodes and links as top-level values; printing);
`get-label`, `linked?`, `related?`, `slip-linked?` over all 59 × 59 pairs;
`relationship-between`, `get-related-node` for 8 relations, `inverse`, the
platonic predicates and numbers; descriptor predicates over fake workspace
objects; reset; every activation message, with the monitor calls; decay and
spread from each node alone; **20 calls of `update-slipnet-activations`**
from 8 fixed states (with/without clamped nodes, unfrozen after 10 updates,
with fake themes spreading activation) recording all activations, frozen
flags and the generator state after every update, plus 15 seeds and the
start-of-run state; similar property links, `apply-slippages` (coattail
slippages, also with Opposite fully active) and top-down codelet posting over
several seeds; and images: 8 letter/group images × 29 operations (each
followed by copy, leaf walk, postorder walk, state, reset), swapped
image/state round trip, printing, string images over a fake string × 13
operations, `change-length-first?`, `enumerate-letter`.

## The Workspace, its objects and strings, and the formulas (item 06)

- **Ported unchanged**: `workspace.ss`, `workspace-objects.ss`,
  `workspace-structures.ss`, `workspace-strings.ss`,
  `workspace-structure-formulas.ss` and `formulas.ss` → `racket/engine/*.rktl`,
  included by engine.rkt after descriptions.rktl in metacat.ss's order
  (bonds/groups/bridges/breakers, concept-mappings.ss and run.ss, which come
  between them in the original, are not ported yet). No line of model code
  changed. `(define *workspace* (make-workspace))` runs at load time, as in
  the original.
- **`tanh`** (workspace.ss, the mapping strength of a maximal mapping):
  racket/base has none, and racket/math's is computed in Racket and may
  differ from Chez's in the last bit. compat.rkt takes Chez's own primitive
  through `(vm-primitive 'tanh)` (Racket CS runs on Chez Scheme; its Chez is
  10.3, the oracle's 10.0; the battery checks 500+ arguments bit for bit).
- **`*temperature-clamped?*` has no definition in the original.** formulas.ss
  reads it, answers.ss and trace.ss `set!` it, and run.ss's `init-mcat`
  creates it with `(set! *temperature-clamped?* #f)` on Chez's top level.
  A Racket module needs a definition: engine/pending.rktl defines it (`#f`)
  under run.ss, and the run.ss item must move it there.
- **Stand-ins added** (engine/pending.rktl), all called only inside procedure
  bodies: `same-bond-category?`, `same-bond-direction?`,
  `opposite-bond-category?`, `opposite-bond-direction?` (bonds.ss);
  `same-group-category?`, `same-group-direction?` (groups.ss);
  `bridge-between?`, `equivalent-workspace-objects?`, `rule-describable-bridge?`
  (bridges.ss); `break-bridge` (breakers.ss); `verbatim-clause?` (rules.ss);
  (correction, loop0002 item 06: the original defines `equivalent-workspace-objects?`
  in trace.ss, `rule-describable-bridge?` in rules.ss and `break-bridge` in bridges.ss);
  `full-workspace-object-name` (trace.ss, used by workspace objects' `print`);
  `group-graphics`, `bridge-graphics`; and `*EEG*` (eeg-graphics.ss), which the
  Workspace's `initialize` messages. Removed: `*workspace*`, `%proposed%`,
  `%evaluated%`, `%built%`, `make-letter`, `make-workspace-structure`,
  `temp-adjusted-probability`.
- **`set-global!`** now also lists the workspace.ss string globals
  (`*initial-string*` … `*all-strings*`, which run.ss's `init-workspace`
  sets), `*temperature-clamped?*`, `*EEG*` and `contains?`.
- **Random draws and order.** These files draw only through `stochastic-pick`
  (`choose-object`, `choose-...-neighbor`, `choose-description-for-rule`,
  `wins-fight?`), `stochastic-pick-by-method`, `random-pick`
  (`get-random-letter`), a probability distribution (`get-num-of-bonds-to-scan`)
  and `~` (`rough-num-of-objects`). No call has two effectful arguments, and
  `wins-fight?` updates the challenger's strength before the defender's in a
  body, not in an argument list. Building the initial workspace draws nothing
  (the checks below confirm the generator state is untouched).
- **The initial workspace at the start of a run.** `init-mcat` activates each
  letter's *descriptors* fully, but not the description *types*
  (`relevant?` = `fully-active?` of the type), so every raw importance is 0
  and every object of a string gets relative importance round(100/n). The
  saliences at the start therefore come from unhappiness alone.
- **Testing infrastructure fix: stale compiled engine.** The differential
  runner (`racket/tests/diff-runner.rkt`) requires the engine into a fresh
  namespace. The default load handler only compares engine.rkt's own date with
  its `.zo`, so after editing an included `.rktl` the battery silently ran the
  old engine (found because no mutation in this item made the battery fail).
  The runner now loads through the compilation manager, whose handler must be
  created inside the new namespace (it skips modules of other module
  registries). Note also that the compilation manager compares timestamps in
  whole seconds: a source edited in the same second as the last compile is
  not recompiled, so mutation scripts must wait a second around each edit.
  The mutation checks of items 04 and 05 were made with `raco make` between
  edits, so they stand.

**Tests**:
- `racket/tests/workspace-diff-test.rkt`, battery
  `tests/diff/workspace-battery.scm` (77 tests, about 0.7 MB of output, identical
  under Chez and Racket), with `tests/diff/workspace-dump.scm`:
  - **for every problem in `tests/problems.txt`** (read at run time; 36
    problems, 109 problem × seed pairs), the initial workspace built as
    `init-mcat` builds it (`b:init-problem`, a copy of the Workspace part of
    run.ss under `b:` names), for each seed: every string (names, type,
    length, object capacity, letter categories, image letters, average
    unhappiness) and every letter (id, positions, letter category, each
    description with its proposal level, strength and time stamp, raw and
    relative importance, intra/inter/average unhappiness and salience, bonds,
    bridges, group, image), the workspace averages and mapping strengths,
    all slipnet activations, the EEG messages, and the generator state after;
  - live queries on 10 problems of every shape (3 or 4 strings, lengths 1–7):
    relevant and distinguishing descriptions, descriptions for rules, concept
    patterns, descriptor and type tests for all nodes, neighbours, positions,
    relevance of bond categories and directions, `spanning-group-possible?`,
    `description-type-support`/`descriptor-support`, reference objects,
    rule possibilities, translation-threshold distribution, `update-temperature`,
    and seeded choices (objects, neighbours, descriptions, bonds to scan,
    rough counts) under 4 seed/temperature pairs;
  - objects with fake bonds, groups and bridges (every branch of the
    unhappiness and salience formulas, a clamped salience), fully active
    description types (raw importance, the 2/3 factor in a group), strings
    with fake bonds and groups (tables, edge vectors, coincident groups,
    storage expansion and the Workspace's bridge reallocation), Workspace
    bridges and rules with fakes, maximal mappings (the tanh branch), workspace
    structures (strength, weakness, age, proposal levels), `wins-fight?` and
    `wins-all-fights?` over 15 seeds and 4 temperatures, `temp-adjusted-probability`
    and `temp-adjusted-values` over 11 temperatures, the group probability
    formulas, and `tanh`.
  - Stand-ins in both runners: `*themespace*` (no active themes, as at the
    start of a run), `*EEG*` (logs its messages), `contains?` (groups.ss's own
    definition).
- `chez_scheme/oracle/tests/workspace-init-check.ss` (Chez only): for every
  problem and its first seed, the dump after `b:init-problem` equals the dump
  after the original's real `init-mcat` (with the real Themespace, EEG and
  `contains?`), and `b:init-problem` draws nothing. This is what makes the
  battery's copy of run.ss trustworthy until run.ss is ported.

## Bonds, groups and concept mappings (item 07)

**Changes**: `bonds.ss`, `groups.ss` and `concept-mappings.ss` →
`racket/engine/bonds.rktl`, `groups.rktl`, `concept-mappings.rktl`, included
by engine.rkt in metacat.ss's load order (bonds and groups after
descriptions.rktl, concept mappings after workspace-strings.rktl). One line
of model code changed:
- **groups.ss, a group's `get-local-density`**: `(append (neighbors self
  'choose-left-neighbor) (neighbors self 'choose-right-neighbor))`. Both
  arguments draw: `choose-left-neighbor`/`choose-right-neighbor` pick at
  random when a letter and a group are both neighbours. Chez evaluates
  `append`'s second argument first (checked: `(append (show 'L) (show 'R))`
  inside a procedure prints `RL`), so the port binds the right neighbours
  first in a `let*`, marked `port:`. Found by the harness below: `abc abd
  iijjkk` seed 3 left the oracle at the update after codelet 735, where the
  slipnet's jump draws were shifted by one. The 400-codelet runs never
  reached it, the 2000-codelet run did. This is the only call in the three
  files with two drawing arguments; the others (`let` with two
  `descriptor-support`s, `append` of incompatible bridges, `cons` of
  neighbours) have at most one.
- **`group-graphics`** (group-graphics.ss) is now an engine procedure,
  verbatim, in `racket/engine/group-graphics.rktl`: group-builder calls
  `(group-graphics 'erase proposed-group)` ungated when it consolidates
  sameness groups, so a headless run needs it. It only sends messages to
  `*workspace-window*` (`caching-on`, `flush`, and `draw-group`/`erase-group`
  for drawn groups). The rest of group-graphics.ss waits for the Workspace
  panel.

**Stand-ins** (engine/pending.rktl). Removed: `same-bond-*`,
`opposite-bond-*`, `same-group-*`, `contains?`, `make-group`,
`group-graphics`. Added: `incompatible-horizontal-CMs?`,
`incompatible-vertical-CMs?` (bridges.ss; `break-bridge` moved under
bridges.ss too, where it is defined); `monitor-new-groups` (trace.ss);
`outline-box`, `arrowhead` (general-graphics.ss); `draw-group-grope`,
`%small-group-arrowhead-length%`, `%group-arrowhead-angle%`
(group-graphics.ss); `%group-letter-category-font%` and
`%relevant-group-length-font%`, which the original never defines
(workspace-graphics.ss creates them by `set!`); and `same-direction?`, which
the original never defines either (bonds.ss's `bonds-equal?`, itself never
called, refers to it): it raises as Chez would. All these are read only in
graphics-gated code or in procedures not called yet.

**`set-global!`** also lists `monitor-new-groups` (the harness records its
calls); `contains?` stays (workspace-battery.scm still replaces it).

**Concept mappings** are made in these files only inside
`get-incompatible-bridge` (bonds and groups), which needs a bridge; with no
bridges they are never made during a run, so the battery tests them
directly. Bridges (item 08) will exercise them in runs.

**Tests**:
- **The codelet-level differential harness**: `racket/tests/codelet-diff-test.rkt`,
  battery `tests/diff/codelet-battery.scm`, harness
  `tests/diff/codelet-harness.scm`. `b:run-codelets` is a copy of run.ss's
  `run-mcat` loop (`step-mcat`, unclamping, re-posting on an empty
  Coderack, `update-everything` every 15 codelets) with only the bond and
  group codelet types enabled: initial codelets are bottom-up bond scouts
  only, bottom-up posting covers `bottom-up-bond-scout` and
  `group-scout:whole-string`, `*top-down-slipnodes*` holds only the 8 bond
  and group nodes, self-watching is off, and `update-everything` leaves out
  rules, the Trace's snag/clamp periods and the Themespace. The trace has
  one line per codelet (type, urgency, time stamp, generator state,
  structures built and broken with strengths, Workspace-window messages,
  `monitor-slipnode-activation-change` and `monitor-new-groups` calls,
  numbers of proposed structures) and per update (temperature, every
  activation, Coderack size, generator state; every object of the
  Workspace every 4th update). The Racket test compares the lines one by one
  and reports the first difference with its problem, seed and codelet.
  Runs: all 109 problem × seed pairs of tests/problems.txt for 400
  codelets, plus 7 runs of 2000 codelets on problems where group-builder
  consolidates sameness groups (the ungated `group-graphics` path); about
  60,000 lines. The test also checks that all 10 enabled codelet types run,
  that bonds and groups are built and broken, and that `group-graphics` is
  called.
- Concept mappings, in the same battery: every message of a mapping
  (names, link, predicates, degree of association, depth, strength,
  slippability, concept pattern, symmetric mapping, `CMs-equal?`) for every
  pair of instances of each of the 9 slipnet categories, and for every pair
  of same-type descriptions of initial and target objects (letters and
  groups) after 600 codelets of 5 problems, with `remove-duplicate-CMs` and
  the activations left by `activate-descriptions`/`activate-label`.
- Exploration, not in the gate: the same harness for **2000 codelets on all
  109 runs** (218,000 codelets, 90 MB of trace) is byte-identical under Chez
  and Racket (Chez about 33 s, Racket about 58 s).

## Bridges and breakers (item 08)

**Changes**: `bridges.ss` and `breakers.ss` → `racket/engine/bridges.rktl` and
`breakers.rktl`, verbatim apart from the GPL header's "Ported to Racket"
lines, included by engine.rkt right after groups.rktl (metacat.ss's load
order). No model line changed.

**Evaluation order, audited**: the draws in these files are
`stochastic-pick` of the bridge type, `choose-object`,
`stochastic-pick-by-method`, `stochastic-if*`, `random-pick`,
`wins-fight?`/`wins-all-fights?`, and, through bridge-builder,
`build-group`/`break-group`/`break-bond`. Every one sits in a `let*`, a
sequence or an `and`/`or`; no call or `let` has two drawing arguments.
`propose-group` (draws) is reached only from `propose-singleton-group` and
`try-to-propose-singleton-group`, which nothing calls. Calls whose arguments
are evaluated in a different order in Racket (`append` in
`get-incompatible-bridges`, the `let`s of `bridge-builder` and `breaker`)
have no side effects.

**Stand-ins** (engine/pending.rktl). Removed: `bridge-between?`,
`incompatible-horizontal-CMs?`, `incompatible-vertical-CMs?`, `break-bridge`
(now defined by bridges.rktl). `equivalent-workspace-objects?` (trace.ss) and
`rule-describable-bridge?` (rules.ss) had been listed under bridges.ss by
mistake and moved to their files. Added:
- themes.ss: `check-descriptions`, `conflicts-with-theme?`,
  `supported-by-theme?` as raising stand-ins (only called with an active
  theme); and **early verbatim copies** of `bridge-type->theme-type`,
  `descriptions-affect-themespace?`, `ignore-descriptions?`, `beta` and
  `bridge-theme-compatibility-sigmoid`. Every bridge calls the first when it
  is made, the sigmoid when its strength is updated, and bridge-builder the
  second; they are pure (no draws, no state), so copying them early changes
  nothing. Item 10 deletes the copies when it ports themes.ss.
- trace.ss: `monitor-new-concept-mappings` (called by every `build-bridge`),
  `entries`; justify.ss: `remove-whole/single-concept-mappings` (both only
  used by `supports-theme-pattern?`, i.e. memory.ss);
- bridge-graphics.ss: `draw-bridge-grope`, `new-bridge-label-number`
  (graphics-gated).

**`set-global!`** also lists `monitor-new-concept-mappings`.

**Tests**:
- `racket/tests/bridge-diff-test.rkt`, battery `tests/diff/bridge-battery.scm`,
  on the item 07 harness (`tests/diff/codelet-harness.scm`) with its new
  `b:bridges?` setting (`b:enable-bridges!`):
  - initial codelets as run.ss posts them (bottom-up bond and bridge scouts,
    interleaved);
  - bottom-up types: those of `*bottom-up-codelet-types*` except rule-scout,
    answer-finder, answer-justifier, progress-watcher and jootser, i.e. bond
    scout, whole-string group scout, both bridge scouts, the description scout
    and the breaker;
  - `*top-down-slipnodes*` as in the original (the bond/group nodes plus
    StrPosCtgy, AlphaPosCtgy and Length, whose top-down codelets are
    description scouts), so descriptions.ss's codelets now run too;
  - a fake Themespace with no active theme that records bridge-builder's
    boosts (`add-theme-if-possible`, answering #f, and
    `update-dominant-themes`) and a null Themespace window recording
    `update-graphics`.
  The trace adds, to item 07's: every built bridge (type, objects, flipped
  groups, proposal level, strength, time stamp, concept mappings, bond
  concept mappings, symmetric slippages), proposed top and vertical bridges,
  the number of descriptions of every object, and
  `monitor-new-concept-mappings` calls. Runs: all 109 problem × seed pairs for
  **1000 codelets** (item 07: 400), about 116,000 lines, 48 MB. The test
  checks that the bridge, description and breaker codelets all run, that top,
  vertical and bottom (justify) bridges are built and bridges broken, that
  the breaker breaks structures, that a bridge with a flipped group is built,
  and that the monitor and Themespace boosts are called.
- With `b:bridges?` off, the item 07 battery's traces are unchanged (bridges
  only add an empty list to its structures).
- Also in the battery, **bridges examined directly** (`b:bridge-matrix`, 9
  runs of 1500 codelets): after the run, a fresh bridge for every pair of
  initial × modified objects (horizontal) and initial × target objects
  (vertical), with its concept mappings and their strengths, internal
  coherence, internal and external strength, incompatible bridges and bond,
  `reverse-direction-orientation?`, `letter-category-mappable-objects?`,
  `singleton-letter-factor`; `direction-incompatible-bridges` for every pair
  of directed groups under both direction mappings; and every pair of built
  bridges of a type (incompatible, supporting, CM-list incompatibility,
  enclosing). Nothing there draws.
- Tests-first: the battery ran under Chez before the Racket test existed (one
  harness bug found there: `b:structures` passed the bridges list to `apply
  append` as its last argument). With HEAD's engine.rkt and pending.rktl
  (no port) the Racket test fails: `set-global!: not a settable engine
  global: monitor-new-concept-mappings`. With the port, all lines agreed on
  the first run; no model line had to change.
- Mutation checks (each restored, 1 s pauses around edits; compared with the
  saved Chez output):

  | Mutation | Lines differing |
  | --- | --- |
  | horizontal CM-count factor 1.2 → 1.3 | 78165 |
  | vertical CM-count factor 1.2 → 1.3 | 2590 |
  | horizontal internal-coherence factor 2.5 → 2.0 | 0 (strengths clip at 100) |
  | singleton-letter factor 0.1 → 0.2 | 621 |
  | horizontal external strength halved | 69537 |
  | vertical external strength halved | 78746 |
  | bridge-scout type weights without `100-` | 116216 |
  | important-object scout by salience | error (caught) |
  | bridge-evaluator without `1-` | 110334 |
  | bridge vs bond fight weights 3:2 → 2:3 | 2739 |
  | build-bridge without symmetric slippages | 8130 |
  | direction partition `< >` → `< <`, `> <` → `> >` | 0 (equivalent here, below) |
  | direction-incompatible: `remq-elements` dropped | 21185 |
  | horizontal incompatible-CMs label test dropped | 1 (only the bridge matrix) |
  | vertical mappable: `slip-linked?` dropped | 73813 |
  | breaker temperature test inverted | 56231 |
  | breaker group×bond probability → bond only | 80 |
  | breaker picks first structure | 59618 |

  The partition mutations are equivalent on these runs: `partition` inserts
  from the end of the list, and a group's subobject bridges come in string
  order, so the second predicate is never consulted. The coherence factor
  multiplies strengths that already exceed 100 with either value.
- Exploration, not in the gate: the bridges harness for **3000 codelets on
  all 109 runs** (327,000 codelets, 349,055 lines, 151 MB) is byte-identical
  under Chez and Racket (Chez 77 s, Racket 135 s): 2160 bridges built, 105
  structures broken by the breaker, 6 bridges with flipped groups.
- The goldens still can't be compared: real runs post rule-scout,
  answer-finder and self-watching codelets (and draw for their posting
  probabilities) from the first update on.

## Rules and answers (item 09)

**Changes**: `rules.ss` and `answers.ss` → `racket/engine/rules.rktl` and
`answers.rktl`, verbatim apart from the GPL header's "Ported to Racket"
lines, included by engine.rkt right after images.rktl (metacat.ss's load
order). No model line changed.

**Evaluation order, audited**: the draws are rule-scout's
`stochastic-if*`/`random-pick`, `abstract-change-descriptions`
(`bounded-random-partition`, `random`, `stochastic-if*`, `prob?`),
`instantiate-change-template` and `choose-description-for-rule`
(`stochastic-pick`), rule-evaluator's `stochastic-if*`, answer-finder's
`stochastic-if*`/`stochastic-pick`, and `translate-rule-clause`'s
`(filter (lambda (d) (prob? 0.4)) ...)`. Each sits in a `let*`, a sequence, a
`map` (whose order compat.rkt reproduces) or a utilities.ss `filter`. In
`(list 'intrinsic (list od) (map ...))` and `(add-extrinsic-change-description
... (prob? ...))` only one argument draws. No call has two drawing arguments.

**Stand-ins** (engine/pending.rktl). Removed: `rule-describable-bridge?`,
`verbatim-clause?`, `equivalent-workspace-objects?` (now an early copy, below).
Added:
- run.ss: `go`, `post-initial-codelets`, `suspend`, `update-everything`
  (answers.ss calls them when it reports an answer or a snag);
- trace.ss: `monitor-new-rules`, `make-answer-event`, `make-snag-event`,
  `theme-pattern-entries-equal?`; memory.ss: `abstract-answer-description`,
  `abstract-snag-description`; justify.ss: `compare-rule-clause-lists`;
- rule-graphics.ss: `initialize-rule-graphics`; bridge-graphics.ss:
  `make-bridge-pexp` (both graphics-gated); constants.ss's four slippage
  colours (`%vertical-slippage-color%`, …), read by the slippage log's
  `get-highlight-color`, which only the graphics call;
- **early verbatim copies** of pure definitions that the model calls on
  every rule or answer: general-graphics.ss's `find-next-space-position`
  (`transcribe-to-english`, called by every `make-rule`), trace.ss's
  `equivalent-workspace-objects?` (the Workspace's `get-real-object`, called
  by `set-translated-rule-information` for every answer), and themes.ss's
  `diff` (= `#f`, the "different" relation of answers.ss's theme phrases).
  Items 10 and 12–13 move them back.

**`set-global!`** also lists `monitor-new-rules`, `*memory*`,
`make-answer-event`, `make-snag-event`, `abstract-answer-description`,
`abstract-snag-description`, `suspend`, `update-everything` and
`post-initial-codelets`, so that the harness can replace them in both runners.

**Tests**:
- `racket/tests/rule-diff-test.rkt`, battery `tests/diff/rule-battery.scm`,
  on the codelet harness with the new `b:rules?` setting (on top of
  `b:bridges?`):
  - `b:update-everything` starts with `check-if-rules-possible`, as run.ss's
    does, and posts bottom-up codelets with the original's own
    `add-bottom-up-codelets` over all of `*bottom-up-codelet-types*` (with
    self-watching off, progress-watcher and jootser get probability 0, and
    answer-finder or answer-justifier 0 depending on the mode);
  - it also ends snag periods as run.ss does (`within-snag-period?`,
    `progress-since-last-snag`, `stochastic-if*`, `undo-snag-condition`);
  - a run stops after the codelet that reports its first answer.
  Fakes, the same in both runners, for the files not ported yet:
  - a Trace: events in a list, no clamp period, a snag period whose
    progress is always 50;
  - a Memory: no answer or snag is ever present;
  - answer events (quality computed as `get-absolute-quality` does) and snag
    events (`activate` does nothing);
  - `abstract-answer/snag-description`, `monitor-new-rules`, the Commentary
    window, `suspend` (ends the run);
  - answer-justifier's procedure (justify.ss), which records its call.
  The trace adds, to item 08's: every rule (type, English transcription,
  clauses, proposal level, strength, time stamp, quality, relative quality,
  uniformity, abstractness, succinctness, `supported?`, tagged supporting
  bridges, theme pattern), the rule monitor, snags (failure result, both
  rules, vertical bridges, slippage log, reference objects), each answer
  (letters, name and groups of the translated string, both rules and the
  translated rule's quality values and supporting bridges, vertical bridges,
  supporting groups, reference objects, slippage log, quality), Memory
  queries and the commentary answers.ss writes. Runs: all 109 problem × seed
  pairs up to the first answer or 2500 codelets.
  - The 57 justify runs build bottom rules and post answer-justifiers but
    never answer, since justify.ss is not ported.
  - 45 of the 52 other runs reach an answer.
  Then `first-answers`, a summary of every run's first answer (codelet,
  letters, quality, both rules in English).
- **Rules examined directly** (`b:rule-matrix`, 12 runs): after a run, for every
  rule of the Workspace, the rule, `currently-works?`, `apply-rule` (with
  `ignore-snag`) on the string it describes, with the transforms per object
  and the image's letters, and `translate` (translated rule, vertical bridges,
  slippage log, supporting groups, reference objects), with the translated
  rule applied to the other string. The generator state is recorded after
  each rule, because `translate` draws.
- With `b:rules?` off, the item 07 and 08 traces are unchanged (rules add an
  empty list to the structures; the new update steps are skipped).
- Not comparable yet: **the goldens' first answers**. The golden runs have
  the real Themespace (themes appear from codelet 15), Trace and
  self-watching codelets, all ported by items 10–11, so their first answers
  come at other codelets (`abc abd xyz` seed 3852097033: `wyz` at codelet
  2170 in the golden, `xyd` at 1174 in the harness). Item 11's full-run
  comparison checks them, first answers included.
- Tests-first: the port's code (the two `.rktl` files, the engine includes,
  the stand-ins) was written first, to learn what the engine needed in order
  to compile. Then the battery ran under Chez before the Racket test existed.
  - Against a scratch worktree of HEAD (no port) with the new battery and
    test, the Racket test fails: `set-global!: not a settable engine global:
    *memory*`.
  - With the port, the first Racket run agreed for 58,704 lines, then raised
    an error on `eqe qeq abbbc` seed 3557912874:
    `equivalent-workspace-objects?: not ported yet`. The Workspace's
    `get-real-object` calls it for every answer, so it became an early copy.
    After that, everything agreed.
  - The battery then grew. At first only 36 of 109 runs answered:
    `abc abd xyz` never did, because after its first snag the temperature
    stayed clamped. So the fake Trace got its snag period. Then came the
    first-answer summary and the rule matrix. The final battery's 220,531
    lines agree, and no model line had to change.
  - `racket/tests/diff-runner.rkt` now prints the message behind each
    `ERROR` result when `METACAT_DIFF_DEBUG` is set.
- Mutation checks, on a reduced battery (8 problems' runs, the summary, 4
  rule matrices; 36,524 lines) compared with its saved Chez output, each
  mutation restored afterwards with 1 s pauses around the edits:

  | Mutation | Lines differing |
  | --- | --- |
  | verbatim-rule probability 0.01 → 0.02 | 36428 |
  | rule-evaluator without `1-` | 36121 |
  | rule-scout `random-pick` → `car` of possible rule types | 36139 |
  | swap-abstraction probability 0.75 → 0.5 | 35712 |
  | subobjects-abstraction probability 0.75 → 0.5 | 27095 |
  | swap partition into all bridges instead of `(add1 (random n))` | 36139 |
  | rule quality weights 3:2 → 2:3 | 35996 |
  | succinctness 4/(3+n) → 4/(2+n) | 35996 |
  | `sort-templates` extrinsic `>` → `<` | 0 (also 0 on the full battery) |
  | maximum rule line length 60 → 30 | 148 |
  | apply-rule nesting order `>` → `<` | 3 |
  | answer-finder `^3` → `^2` | 35905 |
  | answer-finder degree of support without `1-` | 36118 |
  | translation's ignore probability 0.4 → 0.5 | 1 |
  | irrelevant translated-string groups kept | 15 |
  | answer comment "occurs" → "occurred" | 20 |
  | process-snag keeps proposed bridges | 0 (also 0 on the full battery; equivalent) |
  | rule-supporting groups without nested groups | 19 |

  - The `sort-templates` mutation only matters for two extrinsic templates
    with different numbers of dimensions, which no run builds.
  - The snag mutation is equivalent: `delete-all-codelets`, called right
    after, deletes every proposed structure that has a codelet, and every
    proposed bridge does. The original's comment there says the same.

## Themes, justification, trace, jootsing, memory (item 10)

**Changes**: `themes.ss`, `justify.ss`, `trace.ss`, `jootsing.ss` and
`memory.ss` → `racket/engine/*.rktl`, verbatim apart from the GPL header's
"Ported to Racket" lines, included by engine.rkt right after answers.rktl
(metacat.ss's load order). No model line changed. Load time now also builds
the real `*themespace*`, `*trace*` and `*memory*` (each file ends by
defining its object), as the original's load does.

**Evaluation order, audited**: the draws are in themes.ss (theme
activation's `stochastic-if*`, `pick-positive-theme`, thematic-bridge-scout's
`stochastic-pick`, cluster `filter` with `prob?`, `stochastic-pick-by-method`,
`stochastic-select`, `look-for-auxiliary-slippages`' `prob?`,
`propose-description-based-on-theme`), justify.ss (answer-justifier's
`stochastic-pick-by-method` and `stochastic-pick`, the theme pattern's
`(filter (compose prob? get-probability) ...)`), jootsing.ss (jootser's and
progress-watcher's `stochastic-if*`, `stochastic-filter`). Each sits in a
`let*`, a sequence, a utilities.ss `filter`, or a `map`/`tell-all` (whose
order compat.rkt reproduces: `(tell-all clusters 'pick-positive-theme)`
draws once per cluster). No call has two drawing arguments. The goldens
check this on 109 runs.

**Stand-ins** (engine/pending.rktl). Removed: everything of themes.ss,
justify.ss, trace.ss and memory.ss, including the early copies of items
08–09 (`bridge-type->theme-type`, `descriptions-affect-themespace?`,
`ignore-descriptions?`, `beta`, `bridge-theme-compatibility-sigmoid`, `diff`,
`equivalent-workspace-objects?`), which are now the files' own. Added:
- run.ss: `*this-run*` (memory.ss records it with each answer and snag;
  init-mcat sets it);
- `complement-codelet-pattern`, which the original never defines: the
  Temporal Trace's `get-complement-codelet-pattern` message (never sent)
  returns it. An identifier macro that raises "variable ... is not bound",
  as Chez would;
- general-graphics.ss's `%default-fg-color%` and `*fg-color*`, and 18
  constants.ss colours that trace.ss's events keep for their drawings;
- **early verbatim copies** of trace-graphics.ss's
  `group-event-pexp-text-string` (every group event's print name is made
  with it, and the print name is in the trace) and theme-graphics.ss's
  `relation-name` (trace.ss's debugging `print-pattern`). Both are pure.

**`set-global!`** also lists what a run's driver or trace replaces:
`*coderack*`, `build-bond`, `break-bond`, `build-group`, `break-group`,
`build-bridge`, `break-bridge`, `build-description`, `update-temperature`,
`update-slipnet-activations`, `*this-run*`, `*display-mode?*`. utilities.rkt
exports `set-report-error-and-halt!` (marked `port:`), since importers cannot
`set!` `report-error-and-halt`, which run.ss's driver replaces.

**The golden comparison** (`racket/tests/golden-test.rkt`, on
`racket/tests/golden-harness.rkt`): the harness is the Racket counterpart of
the oracle's prelude.ss headless windows, trace.ss instrumentation and
run.ss driver, around a copy of the original run.ss's `init-mcat`,
`run-mcat`, `update-everything` and helpers (item 11 ports run.ss itself and
replaces the copy). Notes:
- Each run needs a **fresh engine** (a new namespace): the oracle runs every
  golden in its own Chez process, and the Memory keeps its answers and snags
  from one run to the next (init-mcat only clears their activations), as do
  other counters. Reusing one engine made the second run's `start` line
  differ at once.
- The Commentary window is commentary-graphics.ss's (its eliza/non-eliza
  paragraph logic) on a recording text window, as in the oracle.
- `*EEG*` (eeg-graphics.ss) is a null object accepting `initialize`: a
  headless run only initializes it (the Workspace's `initialize`); recording
  is gated by `%workspace-graphics%` and the model never reads it.
- Runs are spread over places (16 at most); about 25 s on a loaded 32-core
  machine, 60 s on one core.

**Tests**:
- `racket/tests/golden-test.rkt`: **all 109 golden runs match byte for byte**
  (272,957 codelets; 12,952 `themes` lines; 1,326 Temporal Trace `event`
  lines of all seven types; 115 answers; 608 commentary paragraphs; the
  `halt` of `eqe qeq abbba aaabaaa` seed 3). Every codelet type runs,
  thematic-bridge-scout, answer-justifier, progress-watcher and jootser
  included. It also runs the oracle on `abc ccbbaa ijk` seed 3, which crashes
  the original (anomalies_and_quirks.md), and checks that the port raises
  the same `caddr` error after the same 1062 trace lines.
- `racket/tests/engine-test.rkt` (+15 checks): the three objects exist at
  load time, the four codelet types and the new procedures, the early copy
  of `relation-name`.
- Tests-first, honestly: the five `.rktl` files and the engine includes were
  written first, to find what the engine needed in order to compile (the
  stand-ins above). The harness and the test came next. Against a scratch
  worktree of HEAD (no port; only the harness's hooks added to `set-global!`
  and utilities.rkt), the test fails 111 of 123 checks: every run raises
  `application: not a procedure ... given: #f` (`*trace*`, `*themespace*`,
  `*memory*` are still stand-ins). With the port, the first run (`a b z`
  seed 1) matched at once; the full set first failed from the second run
  on, which was the harness reusing one engine (above). With a fresh engine
  per run, all 109 matched, with no change to the ported model code.
- Mutation checks, each restored afterwards (1 s pauses around the edits):

  | Mutation | Runs differing (of 109) |
  | --- | --- |
  | themes: theme boost 7 → 6 | 109 |
  | themes: theme decay 25 → 20 | 109 |
  | themes: positive→negative weight −75 → −70 | 5 |
  | themes: thematic-bridge-scout cluster probability `^2` → `^3` | 0 (equivalent, below) |
  | themes: bridge-theme sigmoid `beta` 4 → 3 | 17 |
  | justify: retention probability of identity 50 → 60 | 5 |
  | justify: answer-justifier's other-rule weights, difference doubled | 25 |
  | trace: answer quality weights 60:40 → 50:50 | 87 |
  | trace: rule-event threshold 67 → 70 | 3 |
  | trace: group-event threshold 100 → 90 | 66 |
  | trace: concept-activation threshold 85 → 80 | 102 |
  | jootsing: settling period 250 → 200 | 29 |
  | jootsing: maximum clamp period 750 → 700 | 14 |
  | memory: distance threshold 5 → 4 | 5 |

  The `^2` → `^3` mutation is equivalent on these runs: when a thematic
  bridge scout runs, every cluster of an active theme type has a maximum
  positive activation of 0 or 100 (the goldens' `themes` lines: 900 clusters
  at 100, 117 at 0, none between), and `prob?` answers 0 and 1 without
  drawing.

## Full runs and the CLI (item 11)

**Changes**: `run.ss` → `racket/engine/run.rktl`, included by engine.rkt right
after workspace-structure-formulas.rktl (metacat.ss's load order: run.ss comes
before formulas.ss). Three changes, marked `port:`:
- `prompt` and `no-prompt` are left out. They patch the SWL 0.9u REPL's
  waiter (`waiter-prompt-and-read`, `console-input-port`); `prompt` even calls
  the waiter at load time. `break` and `quiet-break` drop their calls to them
  and to `swl:sync-display`.
- `break` and `quiet-break` capture their continuation with Racket's
  `call/cc` instead of `continuation-point*`, which compat.rkt implements
  with `call/ec` (item 03). `go` resumes a stopped run by calling that
  continuation after `(reset)` has left it: the only re-entrant use of a
  continuation in the original (anomalies_and_quirks.md). The caller of
  `run-mcat` and of `go` each needs a continuation prompt, and the reset
  handler must be set rather than parameterized (racket/tests/run-test.rkt
  shows how).
- `init-workspace`'s `let` becomes a `let*` listing the inits last to first,
  Chez's order. It is equivalent: making the strings draws nothing and
  numbers nothing (the mutation that makes them left to right changes no
  golden).

Everything else is verbatim: the step mode (`ss`, `step-mode-on/off`),
`runtil`, `go`, `suspend` (whose "Type (go) or click on the Workspace to
continue..." is part of the oracle's output), `rerun`, `run-mcat` (whose
"Codelets run: K" at the breakpoint is too), `init-mcat`, `update-everything`.

**Stand-ins** (engine/pending.rktl): removed the run.ss names (`go`,
`suspend`, `update-everything`, `post-initial-codelets`, `*this-run*`,
`*display-mode?*`, `*step-mode?*`, `%step-cycles%`, `%update-cycle-length%`).
Added `restore-current-state` (workspace-graphics.ss; `go` calls it when
`*display-mode?*` is on). `*initial-slipnode-unclamp-time*` joins
`*temperature-clamped?*` as a name the original never defines (init-mcat
creates it by `set!`). `set-global!` also lists `*running?*`, `*interrupt?*`,
`*break-time*`, `*step-mode?*`, `%step-cycles%`, `break`, `quiet-break` (the
GUI's control panel and headless drivers set them).

**racket/headless.rkt** is the port's counterpart of chez_scheme/oracle/run.ss
with prelude.ss's headless windows and trace.ss's instrumentation (moved out
of item 10's racket/tests/golden-harness.rkt, which is now a thin wrapper).
`(run-problem strings seed cap keep-going? [trace-port])` calls the engine's
`init-mcat` and `run-mcat`, prints what the oracle prints (Problem line,
`Comment:` as each paragraph is drawn, `Answer:`, `Ooops:` from
`report-error-and-halt`, the summary) and writes the trace. `break` and
`quiet-break` are replaced, as the oracle does, by a procedure that ends the
run (or with keep-going returns as `go` would). One problem per engine
instance: the Memory and the codelet counters outlive a run.

**racket/cli.rkt**: `racket racket/cli.rkt INITIAL MODIFIED TARGET [ANSWER]
[--seed N] [--max-codelets K] [--keep-going] [--trace FILE]`, with the oracle's
argument rules, output and exit codes (2 for bad arguments; 1 when the run
raises, as on `abc ccbbaa ijk` seed 3). Without `--seed` the seed comes from
the clock through utilities.ss's `randomize`, as in the oracle. The only
textual difference is the program name in the usage message.

**Tests**:
- `racket/tests/golden-test.rkt` now runs the engine's own run.ss, and also
  runs the oracle live (Chez processes in parallel, while the places run
  the port) and compares each run's printed output with the oracle's, byte
  for byte: **109/109 traces and 109/109 outputs identical**. It checks that
  the outputs include commentary, answers, suspend's message, a cap's
  `Codelets run:`, `Stopped: cap`, `Stopped: halt` with `Ooops:`, and
  `Answers: none`. About 40 s.
- `racket/tests/cli-test.rkt` runs `racket racket/cli.rkt` as a program against
  `scheme --script chez_scheme/oracle/run.ss`: an answer, no cap, a cap before
  any answer, a justify run, keep-going, the halt run (same stdout, same exit
  code, empty stderr); `--trace` writes the golden file and doesn't change the
  output; a clock seed is printed and replays the same run in the oracle; the
  crash run fails in both with the same stdout up to the crash; nine bad
  argument lists exit 2 in both, with nothing on stdout.
- `racket/tests/run-test.rkt`: the engine's own `break`/`go` (above) and
  step mode.
- `racket/tests/engine-test.rkt`: run.ss's procedures and constants.
- Run times: `racket tests/bench-runs.rkt docs/run-times.md` (not in the
  suite) times each run as a process, one at a time; see docs/run-times.md.
- Mutation checks on run.rktl, against golden-test.rkt, each restored
  afterwards (1 s pauses around the edits):

  | Mutation | Traces / outputs differing (of 109) |
  | --- | --- |
  | update cycle 15 → 16 | 108 (+1 raises) / 108 |
  | initial clamp cycles 50 → 49 | 21 / 21 |
  | initial codelets 2× → 3× objects | 109 / 109 |
  | Themespace spreads before the Workspace spreads to it | 109 / 6 |
  | initially clamped nodes never unfrozen | 71 / 71 |
  | snag condition never undone | 15 / 15 |
  | middle description for even lengths | 106 / 106 |
  | single-letter problems: object category not activated | 3 / 3 |
  | strings made left to right in init-workspace | 0 (equivalent, above) |
  | no `garbage-collect` to the Themespace window | 0 (a null window) |

## The SGL interpreter on racket/draw (item 12)

**Files**: `racket/gui/sgl.rkt` (sgl-interpreter.ss), `racket/gui/fonts.rkt`
(fonts.ss), `racket/gui/colors.rkt` (the colour part of constants.ss:
`swl-color`, `*color-names*`, `=white=` … `=orange=`). They require racket/draw
and racket/class, plus compat.rkt and utilities.rkt, never racket/gui, so they
render offscreen without a display (sgl-test.rkt checks that racket/gui/base is
not even declared). They are ordinary modules, not part of engine.rkt: the
engine still knows nothing about drawing. Item 13 decides how the panel files
reach them (the panels read engine globals).

**What is verbatim**: `draw!`, `erase!`, `draw-exps`, `draw-exp`, `lookup`,
`extend`, `extend*`, `empty-env`, `init-env`, `graphics-dash-pattern`,
`generate-polyline-coords`, `nop-event-handler`, `default-press-handler`, and
fonts.ss's `make-mfont`, `get-actual-font-values`, `get-actual-font-size`,
`make-fixed-font` (but for the measuring line), the face lists, `select-face`,
`serif`/`sans-serif`/`fancy`. racket/class's `send` has SWL's syntax
`(send obj msg arg ...)`, so `send vp draw-...` lines are unchanged.

**What changed, and why** (marked `port:` in the files):
- SWL's `<viewport>` class (a Tk canvas) → `viewport%`. It takes the same
  four coordinate procedures (`pixel->x` … `y->pixel`, made by
  general-graphics.ss's `make-graphics-window`) and keeps every `draw-...`
  method with its arguments and its `unless` guards. Where the original made a
  Tk item with `tcl-eval ... 'create`, the port appends the same item (kind
  rectangle/oval/arc/line/polygon/text, the same pixel coordinates, the same
  options: outline, fill, width, dash string, start, extent, style, text,
  anchor, font, state) to a display list. `move`, `move-pixels`, `raise`,
  `unhide`, `retag`, `rescale`, `delete` act on the list as Tk acts on
  items with a tag (`all` matches everything; `raise` keeps the raised items'
  order). `render dc` paints the list; `set-changed-callback!` lets a GUI
  canvas repaint; `set-scroll-position!` replaces Tk's `canvasx`/`canvasy`
  in `mouse-press`, whose button argument is now `'left`, `'right` or
  `'shift-left` instead of an SWL event-modifier set.
- Painting emulates Tk 8.5 on X11: shapes unsmoothed (aliased, like the
  dissertation's screenshots), Tk's dash strings converted as tkCanvUtil.c's
  `DashConvert` does (`"- "` → 6 on 6 off, `". "` → 2 on 6 off, scaled by
  the width), thin lines ending one pixel before their end point as X11's
  butt caps do (anomalies), pie slices with their radii, text anchored at the
  bottom centre (`-anchor s`). Solid arcs and ovals use racket/draw's own
  curves; dashed ones are flattened to polylines first.
- `tcl-eval`, `remove-unsupported-tcl-args` (Tk 8.0 workarounds) and
  `my-screen->canvas-x/y` (an SWL 0.9u workaround) have no counterpart.
- `*flush-event-queue*` was `swl:sync-display`; it is `void` until a GUI sets
  it with `set-flush-event-queue!`.
- `*platform*` (`'linux`; `'unix` until item 17), `*tcl/tk-version-8_3?*` and `%nice-graphics%` are
  module constants; `graphics-dash-pattern` is their only reader and gives
  `"- "` on Unix either way.
- fonts.ss: SWL's `<font>` → `swl-font%` (face, size, style list; methods
  `get-family`, `get-size`, `get-style`, `get-actual-values`, and `get-font`
  for the racket/draw font). Sizes: positive = points, converted at a fixed
  96 dpi; negative = pixels, as in Tk. Faces go to Pango by name with a
  family fallback (times → roman, helvetica → swiss), so fontconfig picks
  the substitute as Tk's Xft did. `swl:font-families` is racket/draw's face
  list as lower-case symbols; on this machine none of the preferred faces is
  installed, so `select-face` falls back to `times`, `helvetica`, `times`, as
  it would have under Tk. `get-pixel-size` measures on a private bitmap dc
  (`*hidden-canvas*`), so no logo window is needed; `create-mcat-logo` (a
  racket/gui window) comes with the control panel, as do
  `*scrollbar-width*`/`*scrollbar-height*` (`#f` until then). Text
  antialiasing was greyscale (`'partly-smoothed`); item 13 made text aliased
  (`'unsmoothed`), because the graphics erase text by overpainting. These are
  in divergences.md.
- constants.ss's `(make <rgb> r g b)` → an immutable `color%`;
  `swl-color-rgb` gives the components back (for tests).

**Tests**:
- `tests/diff/sgl-battery.scm` (50 tests) + `racket/tests/sgl-diff-test.rkt`:
  the interpreter against the original. The prelude's `send` throws its
  arguments away, so the Chez side (`tests/diff/sgl-chez-setup.ss`, a new
  `#:chez-setup` option of `diff-runner.rkt`'s `check-battery`) redefines
  `send` to call the receiver, defines a recording viewport and **reloads the
  original's sgl-interpreter.ss** against it. The Racket side
  (`racket/tests/sgl-recorder.rkt`) is a `recorder%` with the same methods.
  Every message to the viewport, with every argument (colours as `(rgb r g b)`,
  fonts as `'obj`), must be identical, for every form, every `let-sgl`
  binding, nested and rational origins, all three justifications, erasing
  (top level, nested, inside `let-sgl`, with a colour object), tags, `clear`,
  `rule`, invalid expressions (both raise after drawing what came before),
  and the environment (`lookup`'s dash strings and colours, `extend` ignoring
  `origin`).
- `racket/tests/sgl-test.rkt` (71 checks): colours; dash conversion; fonts
  (styles, sizes, `print`, `show-char-info`, `resize`, the pixel matrix and
  baseline offset); the viewport's items for shapes and text (centre and
  baseline exactly as `draw-text` computes them, image-mode background),
  degenerate shapes, `clear`, every tag operation, mouse presses with scroll
  offset; painted pixels (fill, outline, background, hidden items, a dashed
  line's 6-on-6-off pixels); change callbacks; and a pixel-for-pixel snapshot
  of `racket/tests/sgl-fixture.rkt` (every form, plus the tag operations) in
  `racket/tests/snapshots/sgl-fixture.png`. `METACAT_UPDATE_SNAPSHOTS=1`
  rewrites the snapshot; on a mismatch the actual image goes to
  `/tmp/sgl-fixture-actual.png`. The snapshot depends on the installed fonts.
- Mutation checks on sgl.rkt, each restored afterwards:

  | Mutation | sgl-diff-test / sgl-test failures |
  | --- | --- |
  | origin y taken from x | 2 / 5 |
  | full oval from sweep > 360 instead of ≥ | 2 / 0 |
  | dotted → dashed string | 2 / 1 |
  | arc box y1 sign | 5 / 1 |
  | `extend` binds `origin` | 1 / 0 |
  | left justification offset sign | 0 / 4 |
  | text-relative y without the baseline | 0 / 2 |
  | dash lengths scaled by width+1 | 0 / 7 |
  | dashed polypoint 3 pixels instead of 4 | 0 / 1 |
  | `raise` puts the raised items below | 0 / 4 |
  | thin lines not shortened | 0 / 2 |
  | text 1 pixel higher | 0 / 1 |
  | hidden items painted | 0 / 2 |
  | ring's inner disc outlined in fg | 0 / 1 (after a fix: filled ovals ignored their outline colour) |

## Workspace, bridge, group and rule graphics (item 13)

**Where the graphics code goes.** In the original, the graphics files are loaded into
the same top level as the model, and the model calls them directly whenever
`%workspace-graphics%` is on. The port splits them by what they need:
- **The engine** (racket/engine.rkt, no racket/draw) gets the code that only builds SGL
  expressions or messages `*workspace-window*`:
  - `engine/general-graphics.rktl`: general-graphics.ss without its windows, i.e. the
    pexp builders (circles, boxes, dotted/dashed/zigzag lines, arcs, arrows) and the
    text helpers. It also has metacat.ss's `*platform*` (`'linux`, as the oracle's
    prelude) and `*tcl/tk-version-8_3?*`;
  - `engine/group-graphics.rktl`, `engine/bridge-graphics.rktl`,
    `engine/rule-graphics.rktl`, verbatim but for the change below.

  The early copy of `find-next-space-position` in pending.rktl is gone: it is the
  file's own again.
- **The views** (racket/gui/views.rkt, racket/draw but not racket/gui) include
  `gui/constants.rktl` (constants.ss's graphics part: window sizes, colours, titles;
  `swl-color` and `*color-names*` stay in colors.rkt), `gui/general-graphics.rktl`
  (`make-graphics-window` and the scrollable text window) and
  `gui/workspace-graphics.rktl`, in metacat.ss's load order.
- **Globals both sides use.** engine/view-globals.rktl declares, as `#f`, every colour,
  font and procedure that the model reads but the graphics files define: the
  constants.ss colours, `=white=` …, `%default-fg-color%`/`*fg-color*`, the 11 fonts
  that `select-workspace-fonts` creates by `set!`, and `restore-current-state`. All of
  them are in `set-global!`'s list. racket/gui/engine-route.rkt gives views.rkt a
  `define` and a `set!` that, for a name imported from engine.rkt, expand to
  `(set-global! 'name value)` (module-level `define`s only; every `set!`). Everything
  else is racket/base's. So the included files stay verbatim, and loading views.rkt
  installs them in the engine as loading the files did in the original.
- **Hooks.** The model's hooks to the views are the original's: `*workspace-window*`
  and `%workspace-graphics%`. `attach-workspace-view!` makes the window as `(setup)`
  does and turns graphics on. racket/headless.rkt's `run-problem` takes
  `#:views thunk`, called after the headless windows are installed and before
  `init-mcat`. With workspace graphics on, the model also sends `*EEG*`
  `record-current-values` and `*EEG-window*` `plot-current-values`, so the headless
  null windows accept those.

**Changes (marked `port:`)**:
- rule-graphics.ss's `update-rule-pexps!` returns an updated copy instead of using
  `set-car!` (Racket's pairs are immutable), and workspace-graphics.ss's
  `update-rule-pexps` stores the copy with `set-answer-description-pexp` /
  `set-snag-description-pexp` (anomalies: shared rule pexps).
- general-graphics.ss's `make-graphics-window`:
  - SWL's toplevel and its frame or scrollframe become one window host
    (`make-window-host`): `window-host%` offscreen, with title, geometry and no
    scrollbars; item 15 installs on-screen hosts with `set-window-host-maker!`;
  - the viewport is sgl.rkt's `viewport%` with `set-scroll-region!`;
  - `set-window-title` asks the host instead of the viewport's grandparent;
  - `reposition-vertical-scrollbar` scrolls the viewport (`set-scroll-position!`)
    instead of waiting for a Tk scrollbar to appear;
  - `get-scrollbar-from-frame` asks the host.
- SWL stand-ins in views.rkt: `swl:sync-display` calls sgl.rkt's
  `*flush-event-queue*` hook; `swl:screen-width`/`-height` return 1280×1024 (only read,
  never used, by `set-window-size-defaults`); the resize listener's message queues,
  `thread-fork`, `thread-sleep` and `critical-section` sit on Racket threads and a
  semaphore; `thread-break` (the Workspace window's click handler, which `(go)`es the
  REPL thread) raises until the control panel's engine thread exists;
  `*theme-edit-mode?*` is `#f` until theme-graphics.ss is ported.
- compat's `record-case` binds formals with `car`/`cdr`, as Chez does, so extra
  arguments are ignored (anomalies; trace.ss relies on it).
- sgl.rkt's `viewport%` keeps its display list newest first, since appending each item
  was quadratic over a run. `get-items` and `render` still see the items oldest first.
- fonts.rkt draws text aliased (`'unsmoothed`), because the graphics erase text by
  overpainting (divergences.md).

**Tests**:
- `tests/diff/graphics-battery.scm` (50 tests) + `racket/tests/graphics-diff-test.rkt`,
  Chez with the original vs the engine. It covers every pexp builder of
  general-graphics.ss (flonum coordinates compared to the last bit, through
  `make-polar`, `angle` and `acos`, which agree), the text helpers, `make-group-pexp` at
  every proposal level, direction and span, `make-bridge-pexp` (horizontal, vertical,
  spanning, letters and groups), bridge and group gropes, every `bridge-graphics` and
  `group-graphics` operation against a recording Workspace window, `initialize-rule-graphics`,
  `make-new-rule-pexp`, `update-rule-pexps!` (the port's copy against the original's
  mutated pexp), and `new-bridge-label-number`. All agreed on the first run, once the
  battery's fakes stopped calling slipnodes as methods.
- `racket/tests/workspace-view-test.rkt`:
  - all 109 golden runs with the Workspace window attached (on places, through the new
    `racket/tests/golden-pool.rkt`, which golden-test.rkt now uses too): traces
    identical to tests/golden/, and the window drawn into (over 10,000 items in all);
  - the original's crash run (`abc ccbbaa ijk` seed 3) with the view attached crashes
    in `caddr` at the same point, with the same trace as without;
  - pixel snapshots of six scenes (racket/tests/views-harness.rkt):
    - `mrrjjj-513`: `abc abd mrrjjj` seed 1 at 513 codelets, the moment of the
      dissertation's p224-541;
    - `mrrjjj-answer`: the same run at its answer;
    - `xyz-snag-event`: run7 (`abc abd xyz` seed 3852097033), the Trace's view of its
      snag;
    - `xyz-answer` and `xyz-answer-description`: run7's answer `wyz`, and its
      description in the Memory;
    - `xyd-justify`: a justify run;
  - views.rkt loads racket/draw and not racket/gui.
- racket/tests/engine-test.rkt: the builders are engine procedures, and the view
  globals are `#f` until views are loaded.

## The other panels (item 14)

slipnet-, coderack-, temperature-, theme-, trace-, memory-, commentary- and
eeg-graphics.ss are ported as `racket/gui/*-graphics.rktl`, included by
racket/gui/views.rkt in metacat.ss's load order (slipnet before workspace, then
temperature, coderack, theme, trace, memory, commentary, eeg). They are verbatim but for
the changes marked `port:` and the parts that moved to the engine.

**Split between engine and views**, following item 13: the engine gets what the model
calls whether or not a window exists, verbatim, included after rule-graphics.rktl:
- `engine/trace-graphics.rktl`: `group-event-pexp-text-string` (trace.ss names every
  group event with it);
- `engine/theme-graphics.rktl`: `relation-name` (trace.ss's `print-pattern`);
- `engine/eeg-graphics.rktl`: `%EEG-table%`, `%EEG-buffer-size%`, `make-EEG` and `*EEG*`
  (workspace.ss initializes the EEG, run.ss's `update-everything` feeds it).

The early copies of the first two in engine/pending.rktl are gone, and so is the
pending `*EEG*`. `%coderack-codelet-count-font%`, which coderack.ss's codelet types read,
moved to engine/view-globals.rktl (and `set-global!`), so coderack-graphics.rktl's
`set!` installs it in the engine. engine/pending.rktl now only holds gui.ss's speed
settings, which item 15 takes.

**Changes marked `port:`**:
- slipnet- and coderack-graphics.rktl define the fonts their `select-...-fonts`
  create by `set!` (`%slipnet-title-font%`, `%slipnode-label-font%`,
  `%coderack-title-font%`, `-subtitle-`, `-codelet-type-`, `-codelet-sum-font%`). The
  original never defines them, and a module cannot `set!` an undefined name. This is
  the same fix as item 13's Workspace fonts.
- theme-graphics.rktl: `relation-names-pexp`, which the original never defines
  (anomalies), is an identifier macro that raises Chez's "not bound" error.
- The definitions that moved to the engine are replaced by a comment in the views
  files.
- views.rkt no longer defines its stand-in `*theme-edit-mode?*`: theme-graphics.rktl
  defines it.

**Attaching the views**: `attach-views! [scale]` makes every window as setup.ss's
`(setup)` does: `set-window-size-defaults`, then the Workspace, Slipnet (`*13x5-layout-table*`),
Coderack, Themespace (and its three theme windows), Memory, Commentary, Trace, Temperature
and EEG windows. It turns every graphics switch on, as setup.ss defines them, and sets
gui.ss's speed settings to full speed with no flashing. The logo and the control panel
are item 15's. It returns the windows by name.

**Headless driver** (racket/headless.rkt):
- The null `*EEG*` is gone, so headless runs use the engine's EEG, like the oracle.
- The commentary and the Trace window's events used to be printed and emitted by the
  headless windows themselves. They are now recorded by wrappers (`install-recorders!`)
  around whichever windows are installed when a run starts, headless or the views'.
  Each wrapper passes itself as `self`, so the real Commentary window's own `(tell
  self 'add-comment ...)` in `new-problem` is recorded too. The CLI's output and every
  golden are unchanged.

**Tests**:
- `tests/diff/panels-battery.scm` (38 tests) + `racket/tests/panels-diff-test.rkt`:
  Chez with the original loaded, against the engine plus views.rkt. Colours and fonts
  are SWL stubs under Chez and racket/draw objects in Racket, so `b:clean` turns
  non-data into `'obj`; numbers are compared exactly (flonums to the last bit). It
  covers:
  - the Slipnet layout table;
  - `mercury-pexp` and `draw-thermometer` on a fake window;
  - the Themespace layout, panel orders, relation and dimension names, and the
    Themespace's relations sorted for the panels;
  - `compute-horizontal-panel-info` and `compute-vertical-panel-info` (exact and
    flonum);
  - a theme panel (`make-panel`) on a fake window with a fake Themespace and cluster:
    initialize, add and remove relations, theme graphics parameters, drawing with and
    without a dominant theme, the three `update-graphics` branches, and drawing,
    decreasing and erasing activations (with `*fg-color*` restored);
  - all seven Trace event icons (`*-event-pexp-info`) on fake events, including group
    events in both directions and none, and both rule types;
  - `group-event-pexp-text-string` and the event arrowheads;
  - `get-memory-icon-pexp-info` and its icon procedure at three activations;
  - the Trace and Memory windows' mouse handlers against fake windows and model
    objects (selecting nothing, highlighting, unhighlighting through
    `restore-current-state`, ignored while running, a snag after an answer);
  - the EEG object over 47 recordings (current values, averages, previous values,
    variation), and its table.
- `racket/tests/views-test.rkt` (item 13's workspace-view-test.rkt, renamed and
  extended):
  - all 109 golden runs with **every** window attached (`attach-views!`): traces
    identical to tests/golden/, and each window drawn into;
  - the crash run crashes in the same place;
  - pixel snapshots of 48 window pictures over 8 scenes (racket/tests/views-harness.rkt).
    Item 13's six Workspace snapshots are unchanged: with all panels attached they come
    out pixel for pixel the same. The scenes added here are:
    - `xyz-clamp-click`: a click on the last clamp event of run7 in the Trace window,
      through the original `trace-window-press-handler`. The click point is found by
      asking the Temporal Trace (`get-mouse-selected-event`).
    - `glz-compare`: clicks on the `flz` and `dlz` icons of the Memory window after the
      keep-going run `abc abd glz` seed 1108779034, through
      `memory-window-press-handler`. This gives the answer description and the
      Commentary's comparison of the two answers.
  - Windows a scene leaves blank are not pictured (the bottom themes outside justify
    runs and answer displays, for example).

**Tests first, honestly**: the panel files came first (copying them showed what the
engine split needed), and so did a first rendering of every window. The battery, the
harness changes and the extended view test came after. Against a scratch worktree of
HEAD with only the new tests added, the battery fails (`compute-horizontal-panel-info:
undefined`) and the view test does not compile (`attach-views!: unbound identifier`).
With the port, the 109 goldens matched on the first run with every view attached.

**Mutation checks** (each restored afterwards; "pictures" counts the 48 snapshots that
differ):

| Mutation | Battery | Pictures |
| --- | --- | --- |
| `mercury-pexp` `=` → `<` | 3 tests | 5 |
| horizontal panel x-spacing without `add1` | 9 tests | 6 |
| answer-event oval sizing 3/2 → 4/3 | 2 tests (only after the fake text widths were made large enough for the minimum width not to win) | 1 |
| Coderack slot height `(+ 1 n)` → `(+ 2 n)` | – | 5 |
| Slipnet activation diameter 2δ → 9/4δ | – | 4 |
| Memory icon spacing 5/4 → 3/2 in the `let*` | – | 0: equivalent, `initialize` recomputes it (anomalies) |
| the same in `initialize` | – | 3 |
| EEG previous values off by one | 1 test | 3 |
| EEG cycle width 1/400 → 1/300 | – | 3 |
| Commentary eliza/normal paragraphs swapped | – | 4 |
| panel activation without restoring `*fg-color*` | 1 test | most pictures of 3 scenes (the colour leaks into other windows) |
| Trace event spacing doubled | – | 4 |
| Slipnet `update-graphics` draws `(random 2)` | – | 13 pictures; **all 109 goldens with views differ** (views-test: 147 failures) |

**What I saw** (Read on each PNG, plus crops), compared with the dissertation's figures:
- Slipnet (`slipnet-*.png`; Fig. 1.2, p040-042): the same 13×5 grid of italic labels
  under activation disks, with the bold italic title "Slipnet Activation". After the
  clamp click it reads "Concept Pattern", with one outlined disk on StringPos.
- Coderack (Fig. 4.8, p169-312, p237-649): the same layout, with "Coderack", "Codelet
  Type" / "Selection Probability", 27 slots with two-line labels and counts, bars, the
  last codelet type highlighted, the double line and "100 Total". After the clamp click
  it reads "Codelet Pattern": slots shaded by the pattern's urgencies, no bars or counts.
- Themes (Fig. 4.1, p150-280): the Top Themes window has two rows of panels in panel
  order (Letter Category, String Position, Object Type, Alphabetic Position), with
  dominant panels highlighted in the pressure-off colour. The Vertical Themes window
  has two columns of narrow panels, with String Pos. in the second row as in the
  figure. A crop showed that a panel outline I thought missing was only lost in
  downscaling.
- Trace (Fig. 4.13, p181-333): oval concept-activation and answer icons, group boxes
  with arrowheads above the text (`x-y-z`), double-bordered rule boxes, octagonal SNAG
  signs and rounded Clamp boxes. The clicked clamp is highlighted in green.
- Memory (Fig. 4.17, p215-470): rounded icons, the snag dark and the answers lighter by
  activation, a clicked answer black with yellow text and outline, as in the figure.
- Commentary (Fig. 4.14, p195-354): bold italic sans-serif paragraphs that scroll up,
  including the two-answer comparison ("The only essential difference between the
  answer dlz and the answer flz ..."). The margin is one space (4 pixels).
- Temperature: a thermometer with a bulb, a white highlight ring, ten gradations
  (every fifth longer) and the value in italics next to the mercury. No figure shows
  it.
- EEG: average activity in yellow and temperature in red on black, with the title
  "Average Workspace Activity (yellow) and Temperature (red)". A crop confirmed pure
  red verticals where the downscaled image looked grey. No figure shows it.

## The control panel and windows (item 15)

### Where the code went
- `racket/gui/gui.rkt` (requires racket/gui) holds what SWL and Tk gave the
  original. It includes `gui.rktl` (gui.ss) and `setup.rktl` (setup.ss's `setup` and
  `enable-resizing`; the rest of setup.ss is in the engine).
  - `screen-host%`: an on-screen window host, a frame with a canvas that paints the
    viewport's display list. `setup` installs it with views.rkt's
    `set-window-host-maker!`, so every graphics window from items 13–14 is a frame on
    the screen with no change to the window code. A 50 ms timer
    (`start-gui-refresh!`) repaints windows whose display list changed. It also keeps
    the manual scrollbars in step with the viewport's scroll region (shown only when
    needed, like Tk's scrollframe) and watches the client size, which `on-size` does not
    report when scrollbars appear (anomalies). Mouse presses go to the viewport's
    `mouse-press`, i.e. the original press handlers.
  - The engine thread stands for the REPL thread (`*repl-thread*`). views.rkt's
    `thread-break` now calls a handler (`set-thread-break-handler!`); gui.rkt's sends
    the thunk to the engine thread. The thread runs each thunk inside a prompt, with
    compat's reset handler *set* (not parameterized) to an escape back to its loop, so
    `break`/`quiet-break` → `(reset)` end the thunk and a later `go`, which re-enters
    the captured continuation, ends the same way (item 11's notes).
    `engine-busy?`/`engine-idle-evt` let tests wait.
  - `create-mcat-logo` (fonts.ss): a Logo frame; it sets fonts.rkt's scrollbar sizes
    (`set-scrollbar-size!`, new) to Tk's 15 pixels.
  - `arrange-windows!` tiles the windows (divergences.md); `setup` calls it before and
    after making the control panel.
- `racket/engine/demos.rktl`: demos.ss, verbatim, in the engine after the graphics
  files (it needs no graphics).
- gui.ss's speed settings moved from engine/pending.rktl to engine/view-globals.rktl;
  pending.rktl now holds only names the original never defines.
- engine-route.rkt also routes `set!` of views.rkt variables (`%comment-window-font%`,
  `*theme-edit-mode?*`) to views.rkt's new `set-view-global!`.
- `racket/main.rkt` runs `setup` (dynamic-require, so requiring main.rkt stays
  headless). `racket racket/main.rkt [SCALE]`.

### gui.rktl against gui.ss
- Verbatim: `tokenize-string`, `char-noise?`, the button actions, the breakpoint,
  step-interval and save-commentary actions, `speed-slider-action`, `figure`,
  `clamp-codelets-menu-item`'s pattern logic, and every message of the control panel
  object except the widget calls in them.
- Rewritten (marked port:): widget creation (racket/gui creates widgets in their
  parent, in display order, so `pack` and the spacers go), menus (created in their
  parent menu, so the menu procedures take the parent first; `get-demos-button` & co.
  became the menus themselves), dialogs (`swl-dialog%`, `input-field%`), the help
  window (a text% editor), `set-menu-item-color` (checks the item).
  `make-control-panel` uses `letrec` because a menu item's action may name an item
  created after it.
- New messages, marked port:: `get-widgets` (tests), `get-info-title`,
  `get-theme-edit-dialog`, `get-clearmem-dialog`, `engine-error`; the window
  controller gets `make-menu-item` and `visible?`.
- A faithful detail worth knowing: entering a problem (Enter, or Go/Reset with text in
  the command line) only initializes it and stops (`quiet-break`); the run starts
  with Go, Step or a click on the Workspace. The buttons start disabled; Enter is the
  first way in.

### Threads
- GUI callbacks run in the eventspace thread. The engine thread sends the control
  panel `switch-to-run-mode`/`switch-to-input-mode` and draws into display lists.
  racket/gui methods may be called from any Racket thread, and display lists are
  replaced by consing, so painting reads a consistent list.
- Stop sets `*interrupt?*` (the original's action). The run loop sees it after the
  current codelet, and `break` returns the engine thread to its loop.
- views.rkt's SWL message queue had a race that deadlocked the GUI thread (anomalies);
  a receive is now atomic.

### Tests
- `racket/gui-tests/control-panel-test.rkt` (105 checks, about 9 s), run by
  tests/run-tests.sh as
  `env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a -s "-screen 0 1920x1200x24" raco test racket/gui-tests/*.rkt`.
  racket/info.rkt omits `gui-tests` from plain `raco test racket/`.
  - It covers setup's windows, layout, invalid input and the speed slider. A full run
    (`abc abd ijk` seed 1) ends at the golden's 395 codelets and generator state. So do
    a run in step mode (interval set through the Options dialog) and a run stopped at
    a breakpoint and resumed by a click on the Workspace. Run 7 (2170 codelets) stopped
    by Stop and resumed with Go, and again after Reset, matches its golden. It also
    covers Save commentary (file = the window's lines), Clear Memory through its
    dialog, the Windows menu, self-watching off/on, resizing the Workspace frame and a
    Demos item.
  - Mutations (each restored): Stop doing nothing (3 failures); no `quiet-break` after
    init (18); speed 100 → 2 flashes (1); no `configure` on resize (3); demo item not
    highlighted (1). Not clearing the command line in `update-current-problem` is
    equivalent, since `switch-to-input-mode` clears it too.
- `tests/gui-screenshot.rkt OUT.png PROBLEM... [--break N]` grabs the whole virtual
  screen (Python PIL) after a run driven through the panel. It is not in the suite.

## Demos, the standalone program, README (item 16)

### Demos
- demos.ss was already in the engine (`racket/engine/demos.rktl`, verbatim, item 15),
  because the Demos menu reads it. Item 16 adds its tests and the seed caveat.
- **The seed caveat**, `docs/demos.md`: a table of every demo with what demos.ss or the
  dissertation documents (answer, time step, page), against what the oracle (and so
  the port) does. A subagent read the dissertation's Chapter 5 for it.
  - Replay: Runs 1–5 and 7, Figs. 5.7/5.8, misc1/2/4/5/9. Run 4 replays too: the
    dissertation's run also gives up at 3228.
  - Don't replay: Runs 6 and 8, fig5.5-bottom, fig5.11 (a continuation of another run),
    eqe-qeeeq, misc3, misc6–8.
  - The port follows the oracle wherever the two differ.
- `racket/tests/demos-test.rkt`:
  - all 35 demo problems in the engine equal the original demos.ss, read with `read`;
    misc6–9 stay undefined;
  - every demo problem and seed is a golden run in tests/problems.txt;
  - 12 documented outcomes replay in the port through `racket/cli.rkt` (answers,
    codelets and the final count), in parallel processes, in about 2.5 s.
- control-panel-test.rkt: each of the 35 Demos menu items, submenus included, in gui.ss's
  order, loads its problem and seed (codelets 0, generator state = seed, info label) and
  is the only item checked (211 checks now).

### The standalone program
- `racket/metacat.rkt` is the executable's entry point. With no arguments, or with one
  number (the scale), it opens the GUI as racket/main.rkt does. Anything else goes to
  racket/cli.rkt's `main`, which cli.rkt now also provides as `cli-main`, with the same
  output and exit codes.
- **`lazy-require` instead of `dynamic-require`**: `raco exe` embeds only modules it can
  see. main.rkt's `dynamic-require` of a `define-runtime-path` made the distributed GUI
  exit 1 (anomalies). main.rkt and metacat.rkt now use `lazy-require`. Requiring either
  module still doesn't load racket/gui.
- `make-dist.sh [DEST]` (default `build/metacat`, gitignored) runs `raco make`, then
  `raco exe --gui`, then `raco distribute`, and copies README.md and LICENSE. It takes
  about 6 s and makes 70 MB: bin/metacat, the gracket CS runtime in lib/plt, and gui.ss's
  help.txt, which `define-runtime-path` brings along.
- `racket/gui-tests/dist-test.rkt` (run under xvfb-run with the GUI tests, about 10 s):
  - It builds the distribution into a temporary directory and runs it from another
    empty directory, with a minimal environment.
  - When `bwrap` exists, the program runs in a sandbox where /usr/share/racket,
    /usr/lib/x86_64-linux-gnu/racket, /home and /tmp are empty.
  - `metacat abc abd xyz --seed 3852097033 --max-codelets 10000 --trace F` prints what
    cli.rkt prints, and F equals the Run 7 golden byte for byte. Without a seed it
    answers. Bad arguments exit 2.
  - `metacat` with no arguments opens all 11 windows of `setup` and the control panel
    (listed with `xwininfo`), stays up, and writes nothing to stderr.
  - Mutations: HEAD's main.rkt (dynamic-require) fails 2 checks; GUI dispatch disabled
    in metacat.rkt fails 1.

### README and screenshots
- README.md covers what Metacat is, the credits (Marshall; Mitchell's Copycat), the
  GPL, the GUI, the CLI and the standalone program, the oracle, the tests and the
  layout.
- Screenshots in `docs/screenshots/`, taken on Xvfb with tests/gui-screenshot.rkt:
  - `run7-wyz.png`: the whole screen after Run 7;
  - crops of the Workspace for Run 7 and for `abc abd mrrjjj` seed 1 at 513 codelets.

## Final audit (item 17)

### What was checked
- **Extra seeds**: every problem line of tests/problems.txt with 20 seeds that are not
  golden seeds, oracle against port: **720/720 identical** traces, printed output and
  exit codes (2.15 million codelets, one `report-error-and-halt`). Script:
  `tests/extra-seeds.py`; results: docs/extra-seeds.md.
- **Verbose mode**: all 109 golden runs with `%verbose%` on, oracle against port: 1.55
  million lines of `vprintf` output, byte-identical once the bug below was fixed. Both
  `chez_scheme/oracle/run.ss` and `racket/cli.rkt` take `--verbose` now (gui.ss's
  verbose checkbox: the headless control panel answers `set-verbose-step-mode` with
  `(or value verbose?)`, as gui.ss does). cli-test.rkt compares two verbose runs that
  reach jootsing.ss's `reveal` line, and checks that `--verbose` leaves the trace equal
  to the golden.
- **No racket/gui in the engine**: racket/tests/no-gui-test.rkt walks the transitive
  imports (all phases) of compat, utilities, engine-lang, engine, headless, cli, main
  and metacat: none reaches racket/gui (or mred/mrlib) or racket/draw. The views modules
  (gui/sgl, fonts, colors, engine-route, views) reach racket/draw but not racket/gui. As
  controls, the walk finds racket/draw from views.rkt and racket/gui from gui.rkt, and
  adding `(require racket/gui/base)` to headless.rkt fails 4 checks. No
  `racket/engine/*.rktl` has a `require` of its own. engine-test.rkt keeps its
  `module-declared?` check.
- **Docs against the code**: this file, divergences.md and the stale statuses in
  anomalies_and_quirks.md, read by three subagents and checked by hand, plus a `diff`
  of every verbatim `.rktl` against its `.ss`. Every non-header difference is marked
  `port:` and described here.

### Fixed
- **`format-slipnode` was not a top-level value** (anomalies). Item 03 noted that the
  rules port must register it for utilities.rkt's `reveal-obj`; item 09 didn't. Verbose
  jootsers raised in the port. racket/engine.rkt now registers it right after
  rules.rktl. Tests first: the `reveal-slipnodes` test in slipnet-battery.scm and the
  verbose cli-test runs failed (`ERROR`, then a differing output) before the fix.
- Unmarked port changes now carry `port:` comments: utilities.rkt's `pause` and
  `clear-input-port`, gui.rktl's `get-info-title`, setup.rktl's `start-gui-refresh!`.
  rule-graphics.rktl's header no longer says "verbatim" without its one port change.
- sgl.rkt's `*platform*` is `'linux`, like engine/general-graphics.rktl's. It was
  `'unix`, which metacat.ss doesn't list. Only `'windows` is ever tested, so nothing
  changes.
- engine/pending.rktl: its header and two unused macros dated from when it held
  stand-ins. It now says what it holds: the four names the original never defines.
  Stale comments in racket/engine.rkt, engine-test.rkt, engine/setup.rktl and
  engine/constants.rktl are updated.

### Corrections to earlier sections
The sections above are a per-item log, true when written. These statements are out of
date (the sections are left as written, except where marked):
- **pending.rktl stand-ins** (items 04–11): every stand-in and early copy listed there is
  gone. The files that define them are ported, and the colours, fonts and
  `restore-current-state` are view globals in engine/view-globals.rktl (item 13).
  pending.rktl holds only `*temperature-clamped?*`, `*initial-slipnode-unclamp-time*`,
  `same-direction?` and `complement-codelet-pattern`. In particular, `*temperature-clamped?*`
  (item 06) was never moved to run.rktl. Item 14's "pending.rktl now only holds gui.ss's
  speed settings" was true only until item 15, and even then it held these four names.
- **group-graphics.ss** (item 07): "the rest waits for the Workspace panel". Item 13
  ported the whole file into engine/group-graphics.rktl.
- **`group-event-pexp-text-string` and `relation-name`** (item 10): no longer early copies,
  but the original's definitions in engine/trace-graphics.rktl and
  engine/theme-graphics.rktl (item 14).
- **rule-graphics.ss's `set-car!`** (item 03, "the GUI port must restructure it"): done in
  the engine file engine/rule-graphics.rktl (item 13), not in the GUI layer.
- **The port's traces** (item 02, corrected inline): racket/headless.rkt wraps and
  forwards as trace.ss does.
- **Item 13's `racket/tests/workspace-view-test.rkt`** is now views-test.rkt (item 14),
  and its hook is `attach-views!`. `attach-workspace-view!` still exists.
- **Battery counts**: sgl-battery.scm, graphics-battery.scm and panels-battery.scm hold
  48, 48 and 36 tests. The 50, 50 and 38 quoted in items 12–14 include the two checks
  that `check-battery` adds per battery.
- **The crash run** (item 10: "after the same 1062 trace lines"): golden-test.rkt checks
  an identical prefix of more than 1000 lines, not the exact count.

## One window (loop0003 item 10)
- **Hooks, not copies.** The one-window GUI (racket/gui/one-window.rkt) reuses gui.rkt
  and gui.rktl through two hooks that are off in the multi-window GUI:
  `screen-host%`'s optional `pane-parent` init field (`make-pane-host-maker`): the canvas
  is created in that panel instead of a frame of its own; `set-geometry!`, `raise` and
  the frame's min sizes do nothing, show and hide act on the canvas; and
  `set-control-panel-frame-maker!`, which make-control-panel (gui.rktl) asks for its
  frame. `setup-one-window` is setup.rktl's `setup` with these two set, without
  `arrange-windows!`; gui.rkt now also exports `make-engine-thread`.
- **Letterboxing** is the host's: a pane of a window with `none` scrolling keeps the
  window's first (w+2):(h+2) ratio (the ratio make-resizable gave SWL's aspect bounds),
  and `paint` clears the canvas in the viewport's background, then renders the display
  list through `set-initial-matrix` at the offset (render-items sets the origin itself).
  Mouse presses subtract the offset.
- **The resize queue.** In one window, all panes change size at once, and the
  original's listener keeps only the last pending resize, so a pane's `canvas-resized`
  waits while `thread-msg-waiting?` on `*resize-message-queue*`: the refresh timer
  retries every 50 ms, so the panes take their sizes one per listener pause (250 ms).
- **Tracing a GUI run.** headless.rkt's `trace-gui-runs!` installs the trace's
  wrappers and recorders around whatever windows are installed (the GUI's) and writes to
  a port, without its headless windows or `break`. A GUI run's trace then matches its
  golden's lines between `start` and `end`, which only run-problem writes.
