# `metacat/`: the engine

The Python package `metacat` is Metacat 1.2's model, translated from the original's Chez
Scheme files in [`chez_scheme/original/`](../../chez_scheme/original/). There is one module
per `.ss` file and one function per definition, in the file's order. A few modules
reproduce what Chez itself provided (`chez.py`), the original's object system
(`objects.py`) and the oracle's headless driver (`headless.py`, `trace_writer.py`,
`__main__.py`). None of these modules imports tkinter: the windows are in the subpackage
[`gui/`](gui/README.md), which the engine never imports. For the port as a whole, and how to
run it, see [`python/README.md`](../README.md).

## Modules and their origins

Every engine module's docstring starts with its origin and says what it translates
("Translated to Python (2026) from bonds.ss, with racket/engine/bonds.rktl as a worked
translation"). The Racket files named there are the Racket port's translation of the same
file, which was read before translating.

### Chez and the object system

| Module | Lines | Origin | What it holds |
|---|---:|---|---|
| `chez.py` | 1564 | Chez Scheme 10's built-ins | Data representations (`String`, `Char`, `Pair`, `Vector`; symbols are `str`), `SchemeError`, the global `random`/`random-seed` (a 32-bit LCG, bit-identical `(random 1.0)` doubles), exact arithmetic with Chez's contagion (`add`, `sub`, `mul`, `div`, `max_`, `expt`, `sqrt`...), rounding, `map_` in Chez's order, `for_each`, `sort` (Chez's merge sort, with the same predicate calls), `remq`/`memq`/`assq` and friends, `eq_p`/`eqv_p`/`equal_p`, the top-level value table, and the printer (`number_to_string`, `display`, `write`, `format_`, `printf`) |
| `objects.py` | 197 | utilities.ss (the object procedures) | `SchemeObject` (a record-case closure as a class, with `@message("scheme-name")` methods), `tell`, `delegate`, `delegate_to_all`, `tell_all`, `Lambda`, `Forwarder`, `INVALID`, `Reset` |
| `sugar.py` | 395 | syntactic-sugar.ss | The 22 `extend-syntax` forms as functions (`stochastic_if_star`, `continuation_point_star`, `for_star`, `repeat_star_times`, `category_link_star`, `post_codelet_star`, `define_codelet_procedure_star`...) |
| `utilities.py` | 1106 | utilities.ss | The general utilities, function for function (`prob_p`, `random_pick`, `stochastic_pick`, `weighted_index`, `round_`, `sort_by_method`...). It re-exports the object procedures |
| `names.py` | 64 | (the plan) | `scheme_to_python`: the fixed Scheme → Python name mapping |
| `engine.py` | 86 | metacat.ss | `LOAD_ORDER`, `load()`, `set_global`/`get_global` by Scheme name |

### The model

| Module | Lines | Origin |
|---|---:|---|
| `constants.py` | 70 | constants.ss, the model part: the translation-temperature threshold distributions |
| `setup.py` | 157 | setup.ss: the global counters, the window globals, the configuration switches (`%verbose%`, `%justify-mode%`...) and the user commands |
| `view_globals.py` | 92 | Colours, fonts and speed settings that the model reads but the graphics files define. They are `False` (`#f`) until the views set them (after racket/engine/view-globals.rktl) |
| `coderack.py` | 948 | coderack.ss: codelet types, codelets, bins, the Coderack, bottom-up and top-down posting |
| `descriptions.py` | 294 | descriptions.ss |
| `bonds.py` | 676 | bonds.ss |
| `groups.py` | 1270 | groups.ss |
| `bridges.py` | 1865 | bridges.ss (horizontal and vertical bridges share their common clauses in `_BridgeClauses`) |
| `breakers.py` | 71 | breakers.ss |
| `workspace.py` | 899 | workspace.ss |
| `workspace_objects.py` | 923 | workspace-objects.ss |
| `workspace_structures.py` | 154 | workspace-structures.ss (`wins-fight?`) |
| `workspace_strings.py` | 740 | workspace-strings.ss |
| `concept_mappings.py` | 311 | concept-mappings.ss |
| `workspace_structure_formulas.py` | 70 | workspace-structure-formulas.ss |
| `run.py` | 501 | run.ss: `init-mcat`, `run-mcat`, `update-everything`, and the REPL commands (`ss`, `runtil`, `break`, `go`, `rerun`...) |
| `formulas.py` | 99 | formulas.ss: temperature-adjusted probabilities and values, `update-temperature` |
| `slipnet.py` | 891 | slipnet.ss: 59 nodes and 202 links, made by `load()` |
| `images.py` | 524 | images.ss |
| `rules.py` | 2707 | rules.ss, including the `caddr`-of-`#f` crash of `transcribe-to-english` |
| `answers.py` | 1607 | answers.ss |
| `themes.py` | 1719 | themes.ss |
| `justify.py` | 448 | justify.ss |
| `trace.py` | 2519 | trace.ss: the *Temporal Trace* (not the golden-trace writer; that is `trace_writer.py`) |
| `jootsing.py` | 426 | jootsing.ss |
| `memory.py` | 1014 | memory.ss |
| `demos.py` | 120 | demos.ss: the dissertation's demo problems and seeds |

### The engine's part of the graphics files

The model calls a few graphics procedures even in a headless run, so, as in the Racket port,
these parts of the graphics files are engine modules. They only build SGL expressions
("pexps", Python lists) and send messages to window objects. They never draw a random number
or import tkinter. The window parts of the same files are in [`gui/`](gui/README.md).

| Module | Lines | Origin |
|---|---:|---|
| `general_graphics.py` | 626 | general-graphics.ss: the pexp builders (circles, boxes, dotted, dashed and zigzag lines, arrowheads...) and the text helpers (`find-next-space-position`, which rules.ss calls on every rule) |
| `group_graphics.py` | 142 | group-graphics.ss (all of it; `group-graphics` is called even with graphics off) |
| `bridge_graphics.py` | 328 | bridge-graphics.ss (all of it) |
| `rule_graphics.py` | 113 | rule-graphics.ss (all of it) |
| `eeg_graphics.py` | 151 | eeg-graphics.ss: the EEG object (`*EEG*`), which workspace.ss and run.ss feed |
| `theme_graphics.py` | 37 | theme-graphics.ss: only `relation-name`, which trace.ss prints with |
| `trace_graphics.py` | 55 | trace-graphics.ss: only `group-event-pexp-text-string`, whose names are in the golden traces |

### Headless runs and the CLI

| Module | Lines | Origin |
|---|---:|---|
| `headless.py` | 290 | The oracle's `prelude.ss` (headless null windows) and `run.ss` driver. `run_problem(strings, seed, max_codelets, keep_going, trace_port, verbose, views=None)` prints what the oracle prints and returns `(reason, answers)`. `prepare()` loads the engine and installs the trace writer |
| `trace_writer.py` | 373 | The oracle's `trace.ss`: the JSON-lines trace of [`docs/trace-format.md`](../../docs/trace-format.md), written by wrappers around the Coderack, the build and break procedures, `add-rule`, the temperature and the Slipnet updates. The wrappers only read |
| `__main__.py` | 127 | `python3 -m metacat` (and the `metacat` command): the oracle `run.ss`'s arguments, output and exit codes |
| `__init__.py` | 9 | `__version__ = "1.2.0"` |

The GUI-only files of the original (`sgl-interpreter.ss`, `fonts.ss`, the other
`*-graphics.ss` windows and `gui.ss`) are in [`gui/`](gui/README.md).

## How it fits together

**Load order.** The original `load`s 44 files into one global top level, in metacat.ss's
order, and the files refer to each other in both directions. Here:

1. A module's top level holds only definitions. These need nothing from another engine
   module except `chez`, `objects`, `sugar`, `utilities` and `names`. So every module
   imports alone, without loading the engine or drawing a random number
   (`tests/test_engine_modules.py` checks this).
2. A top-level `define` whose value needs other modules (the Slipnet's nodes and links, the
   codelet types, `*workspace*`, `*themespace*`, `*trace*`, `*memory*`...) is computed in
   the module's `load()`.
3. `engine.load()` imports the modules of `engine.LOAD_ORDER`, which is metacat.ss's list,
   and calls each `load()` once, in that order. Names in the list with no engine module
   (`fonts`, `sgl_interpreter`, `gui` and the window-only graphics files) are skipped.

```python
from metacat import engine
engine.load()
engine.set_global("*temperature*", 50)   # a set! of a global, by its Scheme name
```

**References across modules are qualified and looked up at call time**: you write
`bonds.build_bond(...)` and `setup.g_temperature`, never `from metacat.bonds import
build_bond`. So when a driver or a test replaces a procedure on its module (the trace
writer wraps `build_bond`, the batteries install fakes), every caller sees the change, as
in the original's single top level. The only `from`-imports allowed are from chez, objects,
sugar, utilities and names, plus four named pure drawing helpers. A test checks this on the
AST.

**Objects.** About 4,000 `(tell obj 'msg ...)` sites dispatch through record-case closures
with `delegate` inheritance. Each `make-...` closure is a `SchemeObject` subclass. The
closure's variables become instance attributes, and each record-case clause becomes a
method marked `@message("get-string")`, called with the object and the receiver. Messages
keep their Scheme names as strings: `tell(bond, "get-string")`. The plan's "Objects"
section explains why this design (candidate C3) won.

**Truthiness.** Only `#f` is false in Scheme, so a value that may be `0`, `'()` or `""` is
tested with `is False` / `is not False`, never with plain `if x:`.

**Numbers.** Exact rationals are `fractions.Fraction`, normalised to `int` when the
denominator is 1. Arithmetic that may meet a flonum or an exact quotient goes through
`chez.add`, `chez.mul`, `chez.div`... and never through Python's `/`. A stray float would
change codelet choices.

**Continuations.** `continuation-point*` escapes are exceptions. `break` and `go` re-enter a
run: `run.toplevel(thunk)` runs a command in an engine thread, a break parks that thread,
and `go` resumes it. The headless driver instead ends the run at a break (`StopRun`), or
continues at once with `--keep-going`.

**A run changes the engine for good.** The Memory and the codelet count outlive a run, as
in the original. So run one problem per process, or per fork of a process where
`headless.prepare()` has run, which is how the tests run the goldens.

## Naming conventions

Python names come from `names.scheme_to_python`. `tests/test_name_mapping.py` checks that
the mapping is valid and injective on the original's roughly 1,300 names.

| Scheme | Python | Example |
|---|---|---|
| `-` | `_` | `make-bond` → `make_bond` |
| `?` | `_p` | `CMs-equal?` → `CMs_equal_p` |
| `!` | `_bang` | `vector-increment!` → `vector_increment_bang` |
| `->` | `_to_` | `bridge-type->theme-type` → `bridge_type_to_theme_type` |
| `*x*` (global) | `g_x` | `*temperature*` → `g_temperature` |
| `%x%` (parameter) | `p_x` | `%verbose%` → `p_verbose` |
| `=x=` (colour) | `c_x` | `=white=` → `c_white` |
| trailing `*` (macro) | `_star` | `stochastic-if*` → `stochastic_if_star` |
| `:` | `__` | `group-scout:whole-string` → `group_scout__whole_string` |
| Python keyword or builtin | trailing `_` | `round` → `round_`, `break` → `break_` |
| exceptions | fixed table | `1st` → `first`, `100-` → `hundred_minus`, `%` → `percent` |

Message names stay Scheme strings. Module names follow the file: `workspace-strings.ss` →
`workspace_strings.py`. Every function's docstring starts with its origin:

```python
def bond_builder(proposed_bond):
    """bonds.ss: bond-builder (the codelet procedure)"""
```

## Quirk comments: `# chez:` and `# 1.2:`

Wherever the Python reproduces something on purpose that plain Python would do differently,
a comment says so:

- `# chez:` marks a Chez semantic that Python lacks: `map`'s order of application,
  `stochastic-if*` drawing its coin *before* its probability, argument evaluation order
  (Chez evaluates right to left at many sites), truthiness, `sort`, numbers.
- `# 1.2:` marks a quirk of Metacat 1.2 itself, kept as is. It usually cites its entry in
  [`docs/anomalies_and_quirks.md`](../../docs/anomalies_and_quirks.md).

```python
    # chez: stochastic-if* draws its coin before the probability (sugar.stochastic_if_star)
    # 1.2: a one-argument * (anomalies: "Dead code in bridges.ss")
```

There are 242 sites: 167 `# chez:` (80 of them for `map`'s order) and 75 `# 1.2:`. They are
listed by module and function in
[`docs/python-translation-plan.md`](../../docs/python-translation-plan.md), "The `# chez:`
and `# 1.2:` sites". That list is generated by `python3 python/tests/quirk_sites.py
--write`, and `tests/test_quirk_sites.py` fails when it drifts from the code. Speed-ups
that keep runs identical are marked `speed (item 12)`, and GUI adaptations `port:`.

## License

Every module keeps Marshall's copyright notice and adds a "Translated to Python (2026)"
line. GPL v2 or later, as the original
([`chez_scheme/original/LICENSE`](../../chez_scheme/original/LICENSE)).
