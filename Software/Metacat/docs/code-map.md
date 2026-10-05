# Code map of `chez_scheme/original/` (Metacat 1.2)

One paragraph per file, in the load order of `metacat.ss`, then the non-code
files. For each: what it defines, what it depends on (the other files whose
top-level definitions it references, most-used first; computed by matching
identifiers, so treat it as a guide), its line count, and whether it touches
SWL.

"SWL" below means direct toolkit calls: `swl:` procedures, `send` to SWL
widgets, `create <class>`, or SWL threads (`thread-sleep`, `thread-break`,
`thread-self`, `thread-send`/`thread-receive`). "Graphics hooks" means the file
calls the Metacat window objects (`*workspace-window*`, `*trace-window*`, …)
or tests the display switches (`%workspace-graphics%`, `%slipnet-graphics%`,
`*display-mode?*`, …); these must go through the port's headless-capable
graphics interface. Most model files have graphics hooks; only the files
marked SWL talk to the toolkit itself.

Note: TASK.md lists the files with toolkit calls as `gui.ss`,
`sgl-interpreter.ss`, `general-graphics.ss`, `fonts.ss`, `setup.ss`, `run.ss`,
`constants.ss`, `metacat.ss`. Three more have isolated ones:
`utilities.ss` (`pause` = `thread-sleep`), `workspace-graphics.ss`
(`thread-break *repl-thread*` in a mouse handler) and `theme-graphics.ss`
(one `send vp draw-hidden-filled-rectangle`).

## General facts for the port
- **Objects** are closures that dispatch on a message with `record-case`;
  `tell` (utilities.ss) applies the object to the message and halts on
  `'invalid-message-indicator`. Inheritance is `delegate` to a parent object
  (`base-object`, `make-workspace-structure`, …).
- **Randomness** is all Chez `random`/`random-seed`: `randomize`, `prob?`,
  `random-pick`, `stochastic-pick`, `stochastic-pick-by-method`,
  `stochastic-select`, `stochastic-filter`, `bounded-random-partition`
  (utilities.ss), `stochastic-if*` (syntactic-sugar.ss, draws `(random 1.0)`),
  `wins-fight?` (workspace-structures.ss), and `random-seed` in `init-mcat`
  (run.ss); TASK.md's `weighted-pick` is called `stochastic-pick` here.
  Files that draw directly or via these helpers: utilities, constants,
  syntactic-sugar, coderack, bonds, breakers, bridges, descriptions, groups,
  jootsing, justify, rules, slipnet, themes, workspace, workspace-objects,
  workspace-structures, workspace-strings, answers, run, gui.
- **Ordering**: `sort` is used only in utilities.ss (`sort-wrt-order`,
  `sort-by-method`) and rules.ss; Chez's `sort` is a stable merge sort with
  `(sort pred list)` argument order (Racket: `(sort list pred)`, also stable).
  No Chez hashtables are used anywhere; "tables" are vectors of vectors
  (`make-table`, utilities.ss) and association lists.
- utilities.ss redefines `truncate`, `ceiling`, `floor`, `round` (keeping the
  Chez ones as `scheme-truncate`, …) so that they return exact integers.
- `syntactic-sugar.ss` contains all 22 `extend-syntax` macros.
- Chez's reader accepts all 45 `.ss` files: 1454 top-level forms
  (checked by `chez_scheme/oracle/tests/reader-check.ss`).

## Files in load order

**metacat.ss** (129 lines, SWL). The loader. Imports the SWL modules
(`swl:oop`, `swl:macros`, `swl:generics`, `swl:option`, `swl:threads`), checks
the user-edited settings `*platform*`, `*metacat-directory*`,
`*file-dialog-directory*` and `*tcl/tk-version*` (all commented out in the
distribution, so the oracle prelude must define them), defines
`*tcl/tk-version-8_3?*`, `cd`s into the source directory and `load`s the other
44 files in the order below. Depends on nothing; uses `thread-kill` on
configuration errors.

**syntactic-sugar.ss** (253 lines, no SWL). The 22 `extend-syntax` macros:
`mcat`, `for*` (each/in/from/to/do), `for-each-vector-element*`,
`for-each-table-element*`, `repeat*` (times/forever/until), `if*`,
`stochastic-if*` (one `(random 1.0)` draw), `continuation-point*`, `say`,
`say!`, `vprintf`, `vprint`, the Slipnet definition languages
`slipnet-node-list*`, `slipnet-layout-table*`, `category-link*`,
`instance-link*`, `property-link*`, `lateral-link*`, `lateral-sliplink*`, and
the coderack forms `codelet-type-list*`, `post-codelet*`,
`define-codelet-procedure*`. Also plain definitions: `*largest-random-seed*`
(4294967295), `concatenate-symbols`, `printf`/`newline` wrappers, and the
input-token validators `valid-token-list?`, `valid-number?`,
`symbol-or-valid-number?`. Depends on utilities, slipnet, coderack, setup (in
macro expansions only, so load order is not violated).

**utilities.ss** (928 lines, SWL: `thread-sleep` in `pause`). The object
system (`base-object`, `tell`, `tell-all`, `delegate`, `delegate-to-all`,
`ask`, `print`, `say-object`, `report-error-and-halt`), type testers (`bond?`,
`letter?`, `group?`, `bridge?`, `rule?`, `slipnode?`, …), list utilities
(`compose`, `exists?`, `all-exist?`, `compress`, `flatmap`, `1st`… `nth`,
`remq-elements`, …), exact rounding (`truncate`, `ceiling`, `floor`, `round`,
`round-to-10ths`/`100ths`/`1000ths`), stable sorting (`sort-wrt-order`,
`sort-by-method`), all the random helpers (`randomize`, `prob?`,
`random-pick`, `stochastic-pick`, `stochastic-pick-by-method`,
`stochastic-select`, `stochastic-filter`, `weighted-index`,
`bounded-random-partition`), letter/string conversion
(`symbol->letter-categories`, `string-upcase`, `capitalize-string`), 2-D
tables (`make-table`, `table-ref`, `table-set!`, …), vector arithmetic and
geometry for graphics (`rotate-90-degrees-clockwise`, …). Depends only on
syntactic-sugar (plus one reference each to themes and rules inside procedure
bodies).

**fonts.ss** (188 lines, SWL). Font handling over Tk: `swl-font`
(`create <font>`), `make-mfont`, `get-actual-font-values`,
`get-actual-font-size`, `make-fixed-font`, face lists `*serif-faces*`,
`*sans-serif-faces*`, `*fancy-faces*` chosen from `swl:font-families`
(`select-face`, `serif`, `sans-serif`, `fancy`), scrollbar sizes, a
`*hidden-canvas*` for text measurement, and the logo window
(`*mcat-logo*`, `create-mcat-logo`). Depends on utilities, constants,
general-graphics, syntactic-sugar. GUI only.

**constants.ss** (1074 lines, SWL: `swl:screen-width`/`-height` in
`set-window-size-defaults`). Default window sizes (`%default-…-width%`,
filled in by `set-window-size-defaults` from the screen size), the colour
palette (`swl-color`, `*color-names*`, `=white=`, `=black=`, `=red=`, …) and
every named colour, font and size constant used by the panels and the control
panel (`%workspace-background-color%`, `%gui-…-color%`, …). Mostly data for
the graphics; the model files reference it for a few constants. Depends on
utilities, fonts.

**setup.ss** (169 lines, SWL: `thread-self`, `swl:version`). Global run
state (`*codelet-count*`, `*temperature*`), the window globals
(`*workspace-window*`, `*slipnet-window*`, `*coderack-window*`,
`*themespace-window*`, `*top-/bottom-/vertical-themes-window*`,
`*memory-window*`, `*comment-window*`, `*trace-window*`,
`*temperature-window*`, `*EEG-window*`, `*control-panel*`), the mode switches
(`%eliza-mode%`, `%justify-mode%`, `%self-watching-enabled%`, `%verbose%`,
`%workspace-graphics%`, `%slipnet-graphics%`, `%coderack-graphics%`,
`%codelet-count-graphics%`, `%highlight-last-codelet%`, `%nice-graphics%`),
`*repl-thread*`, `(setup)` which creates every window, `enable-resizing`, and
the user commands `eliza-mode-on/off`, `slipnet-on/off`, `coderack-on/off`,
`codelet-counts-on/off`, `clearmem`, `verbose-on/off`, `speed`. Depends on
every *-graphics.ss file and gui (for window constructors). Headless runs
need the globals but not `setup`.

**coderack.ss** (655 lines, no SWL; graphics hook via `*coderack-window*`).
Codelet urgencies (`%extremely-low-urgency%` … `%extremely-high-urgency%`,
`%urgency-value-table%`, `urgency-name`, `urgency-color`),
`%max-coderack-size%` (100) and `%num-of-coderack-bins%` (7),
`make-codelet-type`, `make-coderack-bin`, `make-coderack` (posting, choosing
and removing codelets by weighted bin draw), `post-codelet-probability`,
`num-of-codelets-to-post`, `add-top-down-codelets`, `add-bottom-up-codelets`,
`bottom-up-urgency`, `thematic-codelet-urgency`, and the lists
`*codelet-types*`, `*thematic-codelet-types*`, `*bottom-up-codelet-types*`,
`*self-watching-codelet-types*`, and the global `*coderack*`. Depends on
utilities, constants, setup, syntactic-sugar, and the codelet procedures in
groups, bonds, bridges, descriptions, rules, jootsing, themes, justify,
answers, breakers (referenced by name in the codelet-type list, resolved at
run time).

**descriptions.ss** (205 lines, no SWL). `make-description` and the
description codelets `bottom-up-description-scout`,
`top-down-description-scout`, `description-evaluator`, `description-builder`,
plus `propose-description`, `build-description`, `descriptions-equal?`,
`description-member?`. Depends on utilities, syntactic-sugar, workspace,
setup, groups, formulas, workspace-structures, themes, slipnet.

**bonds.ss** (550 lines, no SWL). `make-bond` and the bond codelets
`bottom-up-bond-scout`, `top-down-bond-scout:category`,
`top-down-bond-scout:direction`, `bond-evaluator`, `bond-builder`, plus
`propose-bond`, `build-bond`, `break-bond`, and predicates
(`bonds-equal?`, `directed?`, `same-bond-category?`, `opposite-bond-…?`,
`bonded?`, `incompatible-bond-candidates?`), `choose-bond-facet`,
`bond-degree-of-assoc`, `get-bond-facets`, `get-bond-category`. Depends on
utilities, workspace, syntactic-sugar, groups, bridges, themes,
workspace-structures, setup, formulas, slipnet, workspace-objects,
workspace-structure-formulas, concept-mappings.

**groups.ss** (1065 lines, no SWL; graphics hooks: group graphics, grope
flashing). `make-group`, `new-group`, the group codelets
`top-down-group-scout:category`, `top-down-group-scout:direction`,
`group-scout:whole-string`, `group-evaluator`, `group-builder`, plus
`group-evaluation-probability`, `length-group?`, `propose-group`,
`attach-length-description`, bond scanning (`right-adjacent-bonds`,
`polarize-bonds`, `scan-bonds`, `get-next-bond`), `build-group`,
`break-group`, `contains?`, `get-common-groups`, `same-group-…?`,
`directed-group?`, `get-all-nested-groups`. Depends on utilities, workspace,
syntactic-sugar, bonds, setup, group-graphics, workspace-structure-formulas,
bridges, workspace-structures, slipnet, general-graphics, descriptions,
workspace-objects, themes, images, concept-mappings, trace.

**bridges.ss** (1704 lines, no SWL; many graphics hooks: bridge graphics and
gropes). `make-horizontal-bridge`, `make-vertical-bridge`, the bridge
codelets `bottom-up-bridge-scout`, `important-object-bridge-scout`,
`bridge-evaluator`, `bridge-builder`, plus `propose-bridge`, `build-bridge`,
`break-bridge`, singleton groups (`propose-singleton-group`,
`try-to-propose-singleton-group`, `singleton-letter?`), incompatibility and
support between bridges and concept mappings (`group-incompatible-bridges`,
`direction-incompatible-bridges`, `supporting-horizontal-bridges?`,
`incompatible-horizontal-CMs?`, the vertical variants, …), and
`all-possible-bridge-CMs`. Depends on utilities, themes, syntactic-sugar,
workspace, groups, setup, slipnet, bonds, bridge-graphics, workspace-objects,
workspace-structures, trace, concept-mappings, coderack, justify, formulas,
workspace-structure-formulas, group-graphics, descriptions.

**breakers.ss** (47 lines, no SWL). The single `breaker` codelet, which at
low temperature fizzles and otherwise picks a structure and breaks it with
temperature-adjusted probability. Depends on utilities, syntactic-sugar,
bridges, groups, formulas, bonds, setup, workspace.

**workspace.ss** (719 lines, no SWL; graphics hooks). The string globals
(`*initial-string*`, `*modified-string*`, `*target-string*`,
`*answer-string*`, `*top-strings*`, `*bottom-strings*`,
`*vertical-strings*`, `*non-answer-strings*`, `*all-strings*`), structure
states `%proposed%`/`%evaluated%`/`%built%`, `%expiration-period%`,
`%num-youngest-structures%`, `make-workspace` (the `*workspace*` object:
choosing objects and structures by salience/strength, mapping strengths,
unhappiness, rule and answer bookkeeping), and the predicates
`spanning-group-possible?`, `rough-num-of-objects`, `unrelated?`,
`ungrouped?`, `unmapped?`. Depends on utilities, setup, syntactic-sugar,
rules, trace, eeg-graphics, slipnet, bridge-graphics, group-graphics,
formulas, bridges.

**workspace-objects.ss** (672 lines, no SWL). `make-letter`, `new-letter`,
`make-workspace-object` (the parent object of letters and groups:
descriptions, bonds, bridges, salience, happiness, enclosing groups, images),
`lowest-level-object`, `highest-level-object`, `disjoint-objects?`,
`lone-spanning-object?`, `both-spanning-groups?`, `both-spanning-objects?`.
Depends on utilities, syntactic-sugar, setup, slipnet, descriptions, trace,
images, groups, formulas, workspace, bonds.

**workspace-structures.ss** (92 lines, no SWL). `make-workspace-structure`,
the parent object of bonds, groups, bridges and descriptions (time stamp,
proposal level, strength, enclosing group), and the fight procedures
`wins-fight?` (one `stochastic-pick` on temperature-adjusted strengths) and
`wins-all-fights?` (`andmap`, so it stops drawing at the first lost fight).
Depends on utilities, groups, setup, formulas, workspace.

**workspace-strings.ss** (496 lines, no SWL). `make-workspace-string`,
`new-workspace-string`: a letter string with its letters, groups, bonds,
spanning objects, rule-application images and string-level statistics, with
random object choice. Depends on utilities, bonds, syntactic-sugar, groups,
workspace-objects, workspace, rules, constants, images, bridges, formulas,
setup.

**concept-mappings.ss** (182 lines, no SWL). `make-concept-mapping`
(descriptor pairs with label, strength, slippage/identity/relevance
predicates), `CMs-equal?`, `remove-duplicate-CMs`. Depends on utilities,
slipnet, syntactic-sugar.

**workspace-structure-formulas.ss** (67 lines, no SWL).
`length-description-probability`, `single-letter-group-probability`,
`descriptor-support`, `description-type-support`. Depends on utilities,
formulas.

**run.ss** (346 lines, SWL: `swl:sync-display`, `swl:version`; graphics
hooks). The main loop and run control: `%update-cycle-length%` (15),
`%initial-slipnode-clamp-cycles%` (50), `%garbage-collect-cycles%` (100),
`*this-run*`, `*running?*`, `*interrupt?*`, `*breakpoint-continuation*`,
`*break-time*`, `*step-mode?*`, `%step-cycles%`, `*display-mode?*`, the REPL
commands `ss`, `step-mode-on/off`, `runtil`, `clear-breakpoint`, `prompt`,
`no-prompt`, `break`, `quiet-break`, `go`, `suspend`, `rerun`, `run-mcat`,
`step-mcat`, `init-mcat` (calls `random-seed`), `init-workspace`,
`clamp-initial-slipnodes`, `post-initial-codelets`,
`add-string-position-descriptions-to-letters`, `update-everything`,
`update-workspace-values`, `update-all-graphics`, and `(collect 4)` between
runs. Breakpoints and Step use first-class continuations. Depends on setup,
workspace, syntactic-sugar, slipnet, utilities, coderack, general-graphics,
bridges, formulas, bonds, trace, themes, workspace-strings,
workspace-graphics, eeg-graphics, memory.

**formulas.ss** (79 lines, no SWL). `temp-adjusted-probability`,
`temp-adjusted-values`, `current-translation-temperature-threshold-distribution`,
`update-temperature`. Depends on utilities, constants, workspace, setup.

**slipnet.ss** (910 lines, no SWL). `%max-activation%`,
`%workspace-activation%`, `%full-activation-threshold%`, `make-slipnode`,
`make-slipnet-link`, `establish-link`, `coattail-slippage-probability`,
`get-label`, `relationship-between`, `related?`, `linked?`, `slip-linked?`,
`update-slipnet-activations` (random full-activation jumps),
`fully-active?`, `above-threshold?`, `partially-active?`, the node lists
`*slipnet-nodes*`, `*slipnet-letters*`, `*slipnet-numbers*`,
`*top-down-slipnodes*`, `*initially-clamped-slipnodes*`, every `plato-…`
node and link (defined with the syntactic-sugar link languages), and
`platonic-letter?`, `platonic-number?`, `number->platonic-number`, `inverse`.
Depends on utilities, syntactic-sugar, coderack, themes, groups, bonds,
formulas, workspace, trace, descriptions, run.

**images.ss** (393 lines, no SWL). The rule-application machinery: images
of letters, groups and strings (`make-letter-image`, `make-string-image`,
`make-image`, `change-length-first?`, `enumerate-letter`), which record what
an object looks like after a rule changes it. Depends on utilities, slipnet,
syntactic-sugar, workspace-objects, workspace, setup, sgl-interpreter,
groups, group-graphics.

**rules.ss** (2163 lines, no SWL). The largest file. `make-rule` (rule
clauses, English transcription, quality, application to a string),
`rule-characterization`, rule equality, the rule codelets `rule-scout`,
`rule-evaluator`, `rule-builder`, `%verbatim-rule-probability%`,
`possible-to-instantiate?`, `activate-rule-descriptors-from-workspace`,
change abstraction (`abstract-change-descriptions`, schemas and swaps:
`get-common-change-schemas`, `concept-mappings->schema`, `get-all-swaps`,
`select-swap`, …), enclosing-object helpers, and rule-clause templates. Has
`sort` calls and many random draws. Depends on utilities, syntactic-sugar,
workspace, slipnet, coderack, setup, justify, groups, bridges, answers,
general-graphics, themes, rule-graphics, workspace-structures,
workspace-objects, formulas, trace, images.

**answers.ss** (1558 lines, no SWL; graphics hooks: commentary and answer
display). `report-new-answer`, `give-up`, the `answer-finder` codelet, answer
comparison and commentary generation (`answer-quality-phrase`, `explain`,
`theme-phrases`, `compare-answers`, `get-answer-comparison-text`,
`coherence-phrase`, `answer-incoherent?`), snag handling (`process-snag`,
`get-snag-explanation`, `get-snag-justified-themes`), and rule translation
(`make-translated-string`, `translate`, `translate-rule-clause`,
`translate-object-description`, `make-slippage-log`, `apply-to-change`).
Depends on utilities, rules, syntactic-sugar, setup, workspace, trace,
constants, memory, run, bridges, justify, groups, themes, slipnet,
rule-graphics, bridge-graphics, formulas, workspace-strings, coderack,
concept-mappings.

**themes.ss** (1235 lines, no SWL; graphics hooks). Themespace:
`%max-theme-activation%`, `%dominant-theme-margin%`, spread/boost/decay
amounts, the sign-dependent weight procedures, `make-themespace`
(`*themespace*`: theme clusters, activations, clamping, theme patterns),
`make-theme-cluster`, `make-generic-theme`, `make-bridge-theme`, the
`thematic-bridge-scout` codelet and its helpers
(`look-for-auxiliary-slippages`, `propose-description-based-on-theme`,
`conditions-for-bridge`, `theme-support-tester`, `supported-by-theme?`,
`conflicts-with-theme?`, …), `bridge-theme-compatibility-sigmoid`, and the
short dimension aliases `top`, `bot`, `ver`, `lcat`, `len`, `dir`, `spos`,
`apos`, `otype`, `gtype`, `btype`, `facet`, `iden`, `succ`, `pred`, `opp`.
Depends on utilities, syntactic-sugar, bridges, descriptions,
workspace-objects, setup, coderack, workspace, slipnet, run,
workspace-graphics, trace, concept-mappings.

**justify.ss** (352 lines, no SWL). The `answer-justifier` codelet (justify
mode: explain a given answer) and its helpers: `clamp-rules`,
`unify-rules`, `get-unifying-slippages`, rule-clause comparison
(`traverse-rule-clauses`, `compare-rule-clause-lists`, …),
`get-vertical-theme-pattern-to-clamp`, `retention-probability`. Depends on
utilities, trace, syntactic-sugar, answers, workspace, concept-mappings,
slipnet, themes, rules, memory.

**trace.ss** (1672 lines, no SWL; the heaviest user of graphics hooks).
The Temporal Trace (`make-temporal-trace`): events (`make-generic-event`,
`make-answer-event`, `make-clamp-event`, `make-concept-activation-event`,
`make-concept-mapping-event`, `make-group-event`, `make-rule-event`,
`make-snag-event`), importance thresholds and monitors
(`monitor-slipnode-activation-change`, `monitor-new-concept-mappings`,
`monitor-new-groups`, `monitor-new-rules`), naming helpers for commentary
(`full-slipnode-name`, `full-workspace-object-name`, …), and the
theme/concept/codelet pattern language used for clamping
(`theme-pattern?`, `patterns-equal?`, `negate-theme-pattern-entry`, …). Its
comment says coderack.ss must be loaded first (codelet patterns refer to
`*codelet-types*`). Depends on utilities, constants, setup, groups, rules,
bonds, coderack, bridges, themes, descriptions, syntactic-sugar, workspace,
slipnet, justify, jootsing, general-graphics, run, theme-graphics, answers,
concept-mappings, trace-graphics.

**jootsing.ss** (344 lines, no SWL). Self-watching: the `jootser` and
`progress-watcher` codelets, clamp jootsing (`get-clamp-jootsing-probability`,
`joots-from-rule-codelet-clamps`, `joots-from-snag-response-clamps`,
`joots-from-justify-clamps`), `%satisfactory-rule-quality%`,
`%settling-period%`, `%max-clamp-period%`, `%grace-period%`,
`how-strings-change`. Depends on utilities, trace, answers, syntactic-sugar,
workspace, setup, justify, coderack, memory.

**memory.ss** (586 lines, no SWL; graphics hooks via `*memory-window*`).
Episodic memory: `make-memory` (`*memory*`: stored answers and snags,
reminding), `make-answer-description`, `make-snag-description`,
`abstract-answer-description`, `abstract-snag-description`,
`%distance-threshold%`, `calculate-answer-distance`. Depends on utilities,
setup, answers, workspace, trace, run, syntactic-sugar, themes, rules,
justify.

**sgl-interpreter.ss** (466 lines, SWL). The interpreter for SGL, the
symbolic graphics language all panels draw in (`rectangle`,
`filled-rectangle`, `arc`, `line`, `polyline`, `text`, `let-sgl` with
origin/line-width/colours/font/justification, `ring`, `polygon`, …):
`draw!`, `erase!`, `draw-exps`, `draw-exp`, the environment
(`lookup`, `extend`, `extend*`, `empty-env`, `init-env`), coordinate
conversion (`my-screen->canvas-x/y`), `generate-polyline-coords`,
`graphics-dash-pattern`, event handlers (`nop-event-handler`,
`default-press-handler`), `tcl-eval`, `*flush-event-queue*`. It draws on SWL
viewports with `send vp draw-…`. Depends on utilities, fonts, constants,
metacat, syntactic-sugar, setup. The natural seam for the port's drawing
interface.

**general-graphics.ss** (1125 lines, SWL). Window construction and generic
drawing: `make-graphics-window` and the scrollable/unscrollable variants
(`create <viewport>`, `<toplevel>`), the resize listener
(`start-resize-listener`, `*resize-message-queue*`, a thread using
`thread-send`/`thread-receive`), `make-scrollable-text-window` with word
wrapping (`break-into-lines`, `separate-into-words`), and SGL shape builders
(`circle`, `disk`, `pie-slice`, `outline-box`, `solid-box`,
`centered-ovaloid`, `centered-rounded-box`, `centered-octagon`,
`dotted-line`, `dashed-line`, `zigzag-line`, `jagged-line`, …), `pi`,
`%default-fg-color%`/`%default-bg-color%`. Depends on utilities, fonts,
syntactic-sugar, constants, setup, metacat, themes, sgl-interpreter, gui.

**slipnet-graphics.ss** (212 lines, no SWL). The Slipnet panel:
`select-slipnet-fonts`, `make-slipnet-window`, `new-slipnet-window`, and
`*13x5-layout-table*` (node grid layout, activation drawn as filled squares).
Depends on utilities, constants, syntactic-sugar, general-graphics, fonts,
setup, workspace-graphics, slipnet.

**workspace-graphics.ss** (824 lines, SWL: `thread-break *repl-thread*` in
`workspace-window-press-handler`). The Workspace panel: string layout,
letters, bonds, groups, bridges, descriptions, rules, answer and snag
crossouts (`make-snag-crossout-pexp`), `%workspace-arrow-length%`,
`select-workspace-fonts`, `resize-workspace-fonts`,
`restore-current-state`, `make-workspace-window`, `new-workspace-window`.
Depends on utilities, constants, setup, workspace, run, general-graphics,
fonts, rule-graphics, theme-graphics, syntactic-sugar, themes, trace,
slipnet, memory.

**temperature-graphics.ss** (200 lines, no SWL). The Temperature panel
(thermometer): fonts, `%smallest-window-width%`, `make-temperature-window`,
`new-temperature-window`, `mercury-pexp`, `draw-thermometer`. Depends on
constants, utilities, fonts, general-graphics, syntactic-sugar.

**group-graphics.ss** (152 lines, no SWL). Drawing of groups in the
Workspace (boxes with direction arrowheads, grope flashes):
arrowhead constants, `group-dashed-line-density`, `group-graphics`,
`make-group-pexp`, `draw-group-grope`, `make-group-grope-pexp`. Depends on
general-graphics, workspace, utilities, setup, syntactic-sugar.

**bridge-graphics.ss** (326 lines, no SWL). Drawing of bridges (horizontal
arcs, vertical lines, spanning variants, grope flashes): `bridge-graphics`,
`make-bridge-pexp`, `elliptical-/circular-horizontal-bridge-arc`,
`make-vertical-bridge-pexp`, `draw-bridge-grope`, …,
`new-bridge-label-number`. Depends on general-graphics, utilities,
workspace, workspace-objects, setup, syntactic-sugar.

**rule-graphics.ss** (122 lines, no SWL). Rule text in the Workspace:
`initialize-rule-graphics`, `update-rule-pexps!`, `make-new-rule-pexp`.
Depends on general-graphics, utilities, setup, syntactic-sugar, constants.

**coderack-graphics.ss** (364 lines, no SWL). The Coderack panel (codelet
counts per type, last codelet run highlighted): `select-coderack-fonts`,
`make-coderack-window`, `new-coderack-window`. Depends on utilities,
constants, setup, coderack, fonts, general-graphics, syntactic-sugar,
workspace-graphics.

**theme-graphics.ss** (773 lines, SWL: one `send vp
draw-hidden-filled-rectangle`). The Themespace panels (top, bottom and
vertical bridge themes in a grid of dimension × relation cells, with clamping
by mouse in theme-edit mode): `*themespace-window-layout*`,
`*theme-edit-mode?*`, mouse handlers, `make-themespace-window`,
`new-themespace-window`, `*panel-order*`, `*panel-theme-order*`, font
selection, `make-bridge-themes-window`, `make-panel`, panel layout
computations, `relation-name`, `dimension-name`,
`abbreviated-dimension-name`. Depends on constants, utilities, themes,
general-graphics, fonts, syntactic-sugar, setup.

**trace-graphics.ss** (474 lines, no SWL). The Temporal Trace panel (event
icons along a time line): icon fonts, `select-trace-fonts`,
`resize-trace-fonts`, `trace-window-press-handler`, `make-trace-window`,
`new-trace-window`, and one `…-pexp-info` procedure per event type. Depends
on constants, general-graphics, utilities, fonts, slipnet, syntactic-sugar,
run, trace, workspace-graphics, setup, memory, group-graphics,
theme-graphics.

**memory-graphics.ss** (226 lines, no SWL). The Memory panel (answer and
snag icons, clickable to show the stored answer): `select-memory-font`,
`resize-memory-font`, colours, `memory-window-press-handler`,
`make-memory-window`, `new-memory-window`, `get-memory-icon-pexp-info`.
Depends on constants, utilities, general-graphics, fonts, setup,
syntactic-sugar, run, answers, trace, workspace-graphics, memory,
theme-graphics.

**commentary-graphics.ss** (104 lines, no SWL). The Commentary window (a
scrollable text window, with Eliza mode for a plainer style):
`%comment-window-font%`, `%comment-window-reminder-font%`,
`make-comment-window`, `new-comment-window`. Depends on constants, utilities,
fonts, setup, general-graphics.

**eeg-graphics.ss** (235 lines, no SWL). The EEG window (plots of
temperature, coderack composition and other values over time):
`%EEG-table%`, `%EEG-buffer-size%`, `%max-EEG-window-cycles%`, `make-EEG`
(`*EEG*`, which records values every update cycle even without a window),
`make-EEG-window`, `new-EEG-window`. Depends on utilities, constants,
syntactic-sugar, fonts, rules, setup, workspace, general-graphics.

**demos.ss** (122 lines, no SWL). The demonstration problems from Chapter 5
of the dissertation as procedures: `demo`, `run1`…`run8`, `abc-xyd`,
`rst-xyu`, `eqe-baaab`, `fig5.4-top`, `fig5.10`, `misc1`…`misc5`, … Each
calls `mcat` with fixed strings (and sometimes a seed and modes). Depends on
utilities, setup. Useful source for `tests/problems.txt`.

**gui.ss** (1239 lines, SWL: the bulk of the toolkit code). The Control
Panel: fonts and colours, `make-control-panel` (command line, Step/Go/Stop/
Reset buttons, speed slider, menus built with `create-menu`,
`create-submenu`, `menu-item`, `check-menu-item`), button actions
(`step-button-action`, `go-button-action`, `stop-button-action`,
`reset-button-action`, which interrupt the REPL with
`thread-break *repl-thread*`), input parsing (`tokenize-string`,
`char-noise?`), dialogs (`confirm-dialog`, `input-dialog`, help from
help.txt via `read-file`/`help-action`), breakpoints and step interval,
`save-commentary-action`, the speed slider and the animation pauses
(`%max-num-of-flashes%`, `%max-flash-pause%`, `%max-snag-pause%`,
`%text-scroll-pause%`, `%codelet-highlight-pause%`). Depends on setup, demos,
constants, utilities, run, trace, fonts, syntactic-sugar, themes,
theme-graphics, workspace-graphics, commentary-graphics, sgl-interpreter,
coderack, memory.

## Non-code files

**README.txt** (5 lines). Says to edit the settings in metacat.ss and points
to the Metacat web page.

**help.txt** (101 lines). The text shown by the Control Panel's Help:
version history, how to type problems (`abc cba pqrs 123456`: strings, then
an optional seed), the buttons, menus and windows.

**LICENSE** (340 lines). GNU General Public License, version 2.
