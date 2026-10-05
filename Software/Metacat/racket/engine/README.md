# racket/engine/: the ported model files

These are the Racket copies of Metacat 1.2's model files, one `.rktl` file per original
`.ss` file. They are not modules. [`../engine.rkt`](../engine.rkt) `include`s them, in
the load order of the original's `metacat.ss`, into a single module: the **engine**. Each
file is a line-for-line copy of its original in [`chez_scheme/original/`](../../chez_scheme/original/),
with Marshall's copyright header, a "Ported to Racket, 2026" line and, where Racket
forced a change, a comment marked `port:`. The engine never requires `racket/gui` or
`racket/draw`. The windows are in [`../gui/`](../gui/README.md).

## Why one module of included files

The original `load`s 44 files into one global top level. Definitions refer to each
other across files in both directions (coderack.ss names the codelet procedures of
eleven later files, and those files call `*coderack*` back), and files `set!` each
other's globals (`*temperature*`, `*codelet-count*`, the mode switches, the window
globals). Racket modules can't be mutually recursive, and a module can't `set!` a
variable it imports. So the port keeps the original's shape: one module, the files
spliced in with `include`, in load order. Inside it, any procedure can refer to any
definition, and module-level expressions run in load order, as on Chez's top level.
The decision and its costs are in [`docs/porting-notes.md`](../../docs/porting-notes.md),
item 04.

The mechanics, all in `racket/`:

- **`engine.rkt`** is written in `#lang s-exp "engine-lang.rkt"`. It requires
  `compat.rkt` (syntactic-sugar.ss and the Chez built-ins) and `utilities.rkt`
  (utilities.ss), the first two files of the load order. They stay separate modules
  because they need nothing from later files. Then it includes the files below.
- **`engine-lang.rkt`** is racket/base with a `#%module-begin` that discards the values
  of module-level expressions. Chez's top level drops the `'done` of a
  `define-codelet-procedure*` or a `category-link*`; racket/base would print it.
- **`(provide (all-defined-out) set-global!)`**: everything defined here is exported.
  Importers read the current value of a mutated variable through the export, but they
  can't `set!` it. They call `(set-global! 'name value)` instead, which is a `case`
  over an explicit list of the globals that are set from outside: the run state, the
  window globals, the graphics switches, the colours and fonts the views install, and
  the procedures the headless driver wraps (`build-bond`, `update-temperature`, ...).
  Any other name raises "not a settable engine global". Listing a variable there also
  stops Racket from inlining it as a constant.
- **`define-top-level-value`** (compat.rkt) registers names that the original looks up
  at run time with `top-level-value`: the codelet types, the slipnodes and slipnet
  links, and `format-slipnode`, which engine.rkt registers right after `rules.rktl` for
  utilities.rkt's `reveal-obj`.

Because the engine is one module instance, some of its state outlives a run (the
Episodic Memory keeps its answers, codelet types keep their counts). The oracle runs
each problem in a fresh Chez process, so the port runs one problem per process
(`cli.rkt`) or per namespace (the tests).

## The files, in load order

The left column is the include order in `engine.rkt`. Original files that aren't here
are listed after the table.

| File | Original | What it holds |
| --- | --- | --- |
| `constants.rktl` | constants.ss (model part) | The probability distributions (`make-probability-distribution` and the translation-temperature threshold distributions). The window sizes, colours and titles are in `../gui/constants.rktl`; `swl-color` and the colour names in `../gui/colors.rkt`. |
| `setup.rktl` | setup.ss | Global run state (`*codelet-count*`, `*temperature*`), the window globals, the mode switches and the user commands. `setup` and `enable-resizing`, which create the windows, are in `../gui/setup.rktl`. |
| `coderack.rktl` | coderack.ss | Urgencies, codelets, codelet types and the Coderack: posting, choosing (weighted by urgency), overflow deletion, deferred codelets, bottom-up and top-down posting. `port:` the codelet-type list is defined with `define-codelet-type-list*`, which also makes each type (`breaker`, `rule-scout`, ...) a module-level variable. |
| `descriptions.rktl` | descriptions.ss | `make-description` and the description codelets (scouts, evaluator, builder). Unchanged. |
| `bonds.rktl` | bonds.ss | `make-bond` and the bond codelets. |
| `groups.rktl` | groups.ss | `make-group` and the group codelets, including whole-string group scouts. `port:` one call where both arguments draw random numbers is ordered as Chez evaluates it. |
| `bridges.rktl` | bridges.ss | Horizontal and vertical bridges (mappings between strings) and the bridge codelets. |
| `breakers.rktl` | breakers.ss | The `breaker` codelet. |
| `workspace.rktl` | workspace.ss | The Workspace object and the string globals (`*initial-string*`, `*target-string*`, ...). |
| `workspace-objects.rktl` | workspace-objects.ss | Letters and the parent object of letters and groups (descriptions, salience, happiness). |
| `workspace-structures.rktl` | workspace-structures.ss | The parent object of bonds, groups, bridges and descriptions, and the fight procedures (`wins-fight?`). |
| `workspace-strings.rktl` | workspace-strings.ss | Workspace strings: their letters, groups, bonds and statistics. |
| `concept-mappings.rktl` | concept-mappings.ss | Concept mappings (descriptor pairs, slippages). |
| `workspace-structure-formulas.rktl` | workspace-structure-formulas.ss | Description and group-length probabilities and supports. |
| `run.rktl` | run.ss | The main loop (`init-mcat`, `run-mcat`, `update-everything`), `go`, `break`, step mode. `port:` `prompt`/`no-prompt` are left out (they patched SWL's REPL); `break` and `quiet-break` capture a re-entrant continuation with `call/cc` so that `(go)` resumes the run; one `let` is ordered as Chez evaluates it. |
| `formulas.rktl` | formulas.ss | `temp-adjusted-probability`, `update-temperature` and related formulas. |
| `slipnet.rktl` | slipnet.ss | Slipnodes, links, activation spreading and decay, slippage, the slipnet definition. `port:` the node list is defined with a module-level form that also makes each slipnode a variable. |
| `images.rktl` | images.ss | Images of letters, groups and strings, used to apply rules. |
| `rules.rktl` | rules.ss | Rules: clauses, English transcription, quality, application to a string, the rule codelets. The largest file. |
| `answers.rktl` | answers.ss | `answer-finder`, reporting answers and giving up, answer comparison and its commentary. |
| `themes.rktl` | themes.ss | The Themespace: themes, their activation, spreading and decay. |
| `justify.rktl` | justify.ss | `answer-justifier`: explaining a given answer (justify mode). |
| `trace.rktl` | trace.ss | The Temporal Trace: events (answer, snag, clamp, rule, group, concept mapping, concept activation), snag and clamp periods. |
| `jootsing.rktl` | jootsing.ss | Self-watching: the `jootser` and `progress-watcher` codelets, clamping. |
| `memory.rktl` | memory.ss | The Episodic Memory: answer and snag descriptions, reminding. |
| `general-graphics.rktl` | general-graphics.ss (pexp builders) | The procedures that build SGL expressions ("pexps") and the text helpers, which the model calls when `%workspace-graphics%` is on (and `rules.ss` on every rule). The window part is `../gui/general-graphics.rktl`. |
| `group-graphics.rktl` | group-graphics.ss | Group pictures for the Workspace window. Verbatim. |
| `bridge-graphics.rktl` | bridge-graphics.ss | Bridge pictures for the Workspace window. Verbatim. |
| `rule-graphics.rktl` | rule-graphics.ss | Rule text in the Workspace window. `port:` `update-rule-pexps!` returns a copy instead of using `set-car!` (Racket pairs are immutable). |
| `theme-graphics.rktl` | theme-graphics.ss (`relation-name` only) | Used by trace.ss's `print-pattern`. The Themespace windows are in `../gui/theme-graphics.rktl`. |
| `trace-graphics.rktl` | trace-graphics.ss (`group-event-pexp-text-string` only) | Names every group event. The Trace window is in `../gui/trace-graphics.rktl`. |
| `eeg-graphics.rktl` | eeg-graphics.ss (the EEG object) | The EEG object and its table, which run.ss and workspace.ss use whether or not the window exists. The window is in `../gui/eeg-graphics.rktl`. |
| `demos.rktl` | demos.ss | The dissertation's demo problems and seeds, read by the control panel's Demos menu. Unchanged. See [`docs/demos.md`](../../docs/demos.md). |
| `view-globals.rktl` | (port only) | The colours, fonts, `restore-current-state` and gui.ss speed settings that model code reads but graphics files define. They are `#f` here until the views ([`../gui/views.rkt`](../gui/views.rkt)) install real values with `set-global!`. A headless run never reads them. |
| `pending.rktl` | (port only) | Four names the original refers to but never defines: `*temperature-clamped?*` and `*initial-slipnode-unclamp-time*` (created by `set!` at run time in the original), `same-direction?` and `complement-codelet-pattern` (never called; they raise as an unbound variable would under Chez). |

Original files that live elsewhere:

| Original | Port |
| --- | --- |
| metacat.ss (the loader) | the include list of [`../engine.rkt`](../engine.rkt); the entry points are `../main.rkt`, `../cli.rkt`, `../metacat.rkt` |
| syntactic-sugar.ss | [`../compat.rkt`](../compat.rkt) (the 22 `extend-syntax` macros as `syntax-rules`) |
| utilities.ss | [`../utilities.rkt`](../utilities.rkt) |
| fonts.ss, sgl-interpreter.ss | [`../gui/fonts.rkt`](../gui/fonts.rkt), [`../gui/sgl.rkt`](../gui/sgl.rkt) |
| the window code of constants.ss, general-graphics.ss, theme-, trace- and eeg-graphics.ss; slipnet-, workspace-, temperature-, coderack-, memory- and commentary-graphics.ss | [`../gui/*.rktl`](../gui/README.md), included by `../gui/views.rkt` |
| gui.ss, setup.ss's `setup` | [`../gui/gui.rktl`](../gui/gui.rktl), [`../gui/setup.rktl`](../gui/setup.rktl), included by `../gui/gui.rkt` |

## The stand-in mechanism (historical)

While the port was being built, `pending.rktl` held stand-ins for names defined in
files not yet ported: procedures that raised "not ported yet" and variables holding
`#f`. Each work item deleted the names of the file it ported. A forgotten one was a
duplicate definition, which Racket rejects at compile time. Since item 15 every file is
ported, and `pending.rktl` holds only the four names listed above. Nothing in it runs at
load time.

## Conventions

- **Verbatim first.** Code is copied from the `.ss` file with its layout and comments.
  Most files differ from their original only in the header. A `diff` of each `.rktl`
  against its `.ss` was part of the final audit (porting-notes.md, item 17).
- **`port:` comments** mark every change Racket forced. Each is explained in
  [`docs/porting-notes.md`](../../docs/porting-notes.md). Behaviour that differs from the
  original on purpose goes in [`docs/divergences.md`](../../docs/divergences.md), and
  bugs or oddities of the original (kept as they are) in
  [`docs/anomalies_and_quirks.md`](../../docs/anomalies_and_quirks.md).
- **Chez semantics come from `compat.rkt`**, not from edits here: one-armed `if`,
  Chez's `case`, `map` in Chez's evaluation order, Chez's `sort` and argument order,
  `random`/`random-seed` with Chez's generator, Chez's number printing, `record-case`.
  A `.rktl` file has no `require` of its own.
- **No GUI.** The model talks to its windows only through globals such as
  `*workspace-window*` and switches such as `%workspace-graphics%`, as in the original.
  Headless, those windows are null objects ([`../headless.rkt`](../headless.rkt)).
- [`docs/code-map.md`](../../docs/code-map.md) has one paragraph per original file
  (what it defines, what it depends on). Read it before changing a file here.

## Testing

The files are checked through the engine's exports, against the original running under
Chez Scheme: differential batteries per group of files (coderack, slipnet, workspace,
bonds and groups, bridges, rules and answers, graphics) and the 109 golden traces. See
[`../tests/README.md`](../tests/README.md).
