# Anomalies and quirks

A field log of bugs, anomalies, UFO sightings and strange things found in Metacat, in Chez
Scheme, or in the port. Anything surprising goes here, even if it turns out to be nothing.
The other docs have narrower jobs: `porting-notes.md` records how the port copes with each
of these, and `divergences.md` records where the port deliberately behaves differently.

**Entry format:** a short title, then:
- **Seen:** where and when, with the item or iteration and the problem, seed and codelet if any.
- **What:** what happens, with exact output if short.
- **Evidence:** how to reproduce it (a command, a test, a golden file).
- **Status:** `open` · `explained` · `worked around` · `won't fix` · `not a bug`, plus a line of explanation.

Kinds: 🐛 bug in the original · 🌀 anomaly (behaviour nobody can explain yet) ·
⚙️ Chez/Racket quirk · 🔗 hidden coupling · 🛸 unexplained / UFO.

---

## 🐛 Bugs in the original

### `report-error-and-halt` in `answer-justifier`
- **Seen:** iteration 3 (item 02), `eqe qeq abbba aaabaaa` seed 3, codelet 4004.
- **What:** `answer-justifier` sends `get-constituent-objects` to a letter, which doesn't
  understand it. The original prints `Ooops: ...` and calls `(reset)`, which under SWL
  abandons the run and under `scheme --script` exits 255.
- **Evidence:** `tests/golden/` contains this run, ending in a `halt` event;
  `chez_scheme/oracle/run.ss eqe qeq abbba aaabaaa --seed 3` prints `Stopped: halt`.
- **Status:** won't fix (it's the original's behaviour). The port halts at the same
  codelet: since iteration 11 (item 10) racket/tests/golden-test.rkt compares this run's
  whole trace, `halt` event included.
- **Update (loop0002 iteration 11, item 10):** the Python port halts at the same codelet:
  python/tests/test_golden.py compares this run's whole trace, `halt` event included.

### `caddr` of `#f` in `transcribe-to-english`
- **Seen:** iteration 3 (item 02), `abc ccbbaa ijk` seed 3.
- **What:** a Chez error in `transcribe-to-english` (rules.ss), called from `make-rule`.
  Under the SWL REPL it would abandon the run.
- **Evidence:** `scheme --script chez_scheme/oracle/run.ss abc ccbbaa ijk --seed 3` exits 1
  with a backtrace.
- **Status:** open. The golden set uses seed 4 instead. Nobody knows whether it happened
  under 1999-era Chez, where argument evaluation order may have differed.
- **Update (iteration 11, item 10):** the port crashes at the same point, raising
  `caddr: contract violation ... given: #f`, after the same 1062 trace lines.
  racket/tests/golden-test.rkt runs the oracle on this problem and checks both.
- **Update (loop0002 iteration 10, item 09):** the crash is `get-change-phrase`'s
  `(3rd BondFacet-change)`, rules.ss line 1868: a `self` GroupCtgy change with no BondFacet
  change among the rule's changes leaves `BondFacet-change` `#f` (Chez: "Exception in
  caddr: incorrect list structure #f", frames rules.ss 71194 → 67689 → transcribe-to-english
  → make-rule). The Python port raises `chez.SchemeError("caddr", ...)` at the same place.
  `python/oracle/batteries/rule-extra-battery.scm` pins it from Chez on hand-made clauses
  (`transcribe-group-category-without-bond-facet`, `change-phrase-group-category-crash`,
  both ERROR), next to the same clause with a BondFacet change, which transcribes. The
  rule-battery harness doesn't reach the crash on abc ccbbaa ijk seed 3 (no themes or
  self-watching; no answer and no error in 2500 codelets), so the full run is left to the
  golden runs.
- **Update (loop0002 iteration 11, item 10):** the Python port's full run crashes at the
  same point too: python/tests/test_golden.py runs the oracle live on abc ccbbaa ijk seed 3
  and checks that the Python run raises `chez.SchemeError` from `caddr` with the same 1062
  trace lines written before it.

### `bonds-equal?` calls `same-direction?`, which nothing defines
- **Seen:** iteration 8 (item 07), compiling bonds.ss.
- **What:** bonds.ss's `bonds-equal?` ends with `(same-direction? bond1 bond2)`; no file
  defines `same-direction?` (bonds.ss has `same-bond-direction?`). Chez's top level
  compiles the reference and would raise "variable same-direction? is not bound" if
  `bonds-equal?` were ever called; nothing calls it.
- **Evidence:** `grep -n "same-direction?" chez_scheme/original/*.ss` (one hit);
  racket/tests/engine-test.rkt checks the port's stand-in raises.
- **Status:** won't fix (latent). engine/pending.rktl defines a stand-in that raises.
- **Update (loop0002 iteration 8, item 07):** the Python port does the same:
  `bonds.same_direction_p` raises `chez.UnboundVariable("same-direction?")`, and
  `bonds_equal_p` reaches it only when its first three tests hold. Nothing calls
  `bonds-equal?` in the codelet battery either.

### A snag event's `print` never names its kind: the failure tags are upper case
- **Seen:** loop0002 iteration 11 (item 10), translating trace.ss to Python.
- **What:** rules.ss tags failure results with upper-case symbols (`'SWAP`, `'CONFLICT`,
  `'CHANGE`, rules.ss lines 1272, 1289, 1302), and trace.ss's `record-case`s use the same
  upper-case keys, so they work. But the snag event's `print` clause dispatches with
  `(case snag-type (swap "Swap") (conflict "Conflict") (change "Change"))` and
  `(eq? snag-type 'change)`, in lower case. Chez 10 is case-sensitive, so nothing matches:
  the line printed is `#<void>-snag involving objects:`, always with the plural. Under the
  1999-era Chez, which folded symbols to lower case, it worked.
- **Evidence:** trace.ss lines 1083–1087; `(case 'SWAP (swap "Swap") (else 'nomatch))`
  gives `nomatch` under `scheme --script`. Only the Temporal Trace's debugging `print`
  shows it; no trace or commentary text depends on it.
- **Status:** won't fix (faithful). racket/engine/trace.rktl and python/metacat/trace.py
  (a `# 1.2:` comment) reproduce Chez 10's behaviour.

### `complement-codelet-pattern` is never defined
- **Seen:** iteration 11 (item 10), compiling trace.ss.
- **What:** the Temporal Trace answers `get-complement-codelet-pattern` with the variable
  `complement-codelet-pattern`, which no file defines (trace.ss has the procedure
  `get-complement-codelet-pattern`, a different thing). Nothing sends that message, so
  Chez never notices.
- **Evidence:** `grep -n "complement-codelet-pattern" chez_scheme/original/*.ss`.
- **Status:** won't fix (latent). engine/pending.rktl makes it an identifier macro that
  raises "variable complement-codelet-pattern is not bound", as Chez would.
- **Update (loop0002 iteration 11, item 10):** the Python port's
  `trace.complement_codelet_pattern()` raises `chez.UnboundVariable`, and the clamp event's
  `get-complement-codelet-pattern` clause calls it (test_golden.py checks that it raises).

### A string image's `new-alpha-position-category` sends `new-start-letter`
- **Seen:** iteration 6 (item 05), reading images.ss and checking it under Chez.
- **What:** `make-string-image` answers `new-alpha-position-category` by sending each
  sub-image `new-start-letter` with the same argument (a copy-paste of the
  `new-start-letter` clause just above it). A letter image takes a non-relation argument
  as its new letter, so applying "alphabetic position → first" to a whole string makes
  every letter image's start letter the node `AlphaPos:first` itself:
  `abc` generates `(plato-alphabetic-first plato-alphabetic-first plato-alphabetic-first)`.
  `transform-image` (rules.ss:1384) sends this message for
  `plato-alphabetic-position-category` transforms, so a rule whose clause changes a whole
  string's alphabetic position would reach it.
- **Evidence:** in `tests/diff/slipnet-battery.scm`, apply
  `(tell si 'new-alpha-position-category plato-alphabetic-first fail)` to a string image
  of `abc`; the battery's `string-image-operations` test covers the message with a
  relation argument.
- **Status:** open (suspected bug, faithfully ported). Whether any golden run reaches it is
  not known yet; rules.ss (item 06) will tell.

### Latent errors: `relationship-between` of fewer than two nodes; printing a group image without a direction
- **Seen:** iteration 6 (item 05), differential battery.
- **What:** `(relationship-between (list plato-a))` takes `(1st '())` (adjacency-map gives
  no relations, and `all-same?` of `'()` is true); `(relationship-between '())` takes
  `(rest '())`. Both are Chez errors. Separately, `'print` on a group image whose direction
  is `#f` (a sameness group) sends `get-lowercase-name` to `#f`.
- **Evidence:** tests `relationship-between-one`, `relationship-between-none` in
  `tests/diff/slipnet-battery.scm` print `ERROR` under Chez and Racket alike.
- **Status:** won't fix. images.ss only calls `relationship-between` on two or more
  sub-images, and `print` is a debugging aid.

### `letter-category-mappable-objects?` compares a group with itself
- **Seen:** iteration 9 (item 08), reading bridges.ss.
- **What:** for two groups, the test is `(related? (tell object1 'get-group-category)
  (tell object1 'get-group-category))`: `object1` twice, so it is always true and any
  two groups (that have letter-category descriptions) may be mapped on LettCtgy. The
  intent was surely `object2` in the second place. The other three cases are fine.
- **Evidence:** bridges.ss line 1521; called by `horizontal-mappable-descriptions?`
  for every horizontal bridge's LettCtgy description pair.
- **Status:** won't fix (ported verbatim; the goldens depend on it).

### `init-env`'s default font is a bare SWL font, which `draw-text` cannot `tell`
- **Seen:** iteration 13 (item 12), rendering the SGL fixture.
- **What:** sgl-interpreter.ss's `init-env` binds `font` to `(swl-font sans-serif 10)`,
  an SWL `<font>` instance. The viewport's `draw-text` asks the font for its size with
  `(tell font 'get-pixel-size text)`, which only works for the closures `make-fixed-font`
  and `make-mfont` return. So a `(text ...)` drawn without a `(font ...)` binding in an
  enclosing `let-sgl` fails, unless SWL instances happen to be applicable (not checked:
  SWL is not available).
- **Evidence:** `racket/tests/sgl-test.rkt` (`check-exn` on `(draw! vp '(text "x"))`):
  the port fails with "application: not a procedure" on the `swl-font%` object.
- **Status:** open, latent. Every panel seen so far binds a font before drawing text;
  items 13–14 will show whether any panel relies on the default. Ported verbatim.
- **Update (iteration 15, item 14):** all panels are ported and every `text` they draw
  has a `font` binding around it; 109 golden runs with every window attached and 48
  pictures never reach the default.

### `relation-names-pexp` is never defined (theme-graphics.ss)
- **Seen:** iteration 15 (item 14), compiling theme-graphics.ss in Racket.
- **What:** a theme panel (`make-panel`) answers `get-relation-names-pexp` with
  `relation-names-pexp`, but its variable is `relation-names-pexps` (plural). Chez
  compiles the reference to an unbound top-level variable; Racket rejects the module.
  Nothing sends `get-relation-names-pexp`.
- **Evidence:** `grep -n "relation-names-pexp\b" chez_scheme/original/theme-graphics.ss`
  (line 510).
- **Status:** worked around, latent. racket/gui/theme-graphics.rktl defines
  `relation-names-pexp` as an identifier macro that raises Chez's error ("variable
  relation-names-pexp is not bound"), as engine/pending.rktl does for
  `complement-codelet-pattern`.

### The Memory window's first icon spacing is dead code
- **Seen:** iteration 15 (item 14), a mutation check.
- **What:** `new-memory-window` computes `memory-icon-spacing` (and `next-y`) in its
  `let*`, but `make-memory-window` always sends `initialize` next, which recomputes both
  with the same formula. Changing the first one changes nothing.
- **Evidence:** memory-graphics.ss lines 82–86 and 119–122; the mutation table in
  porting-notes.md, item 14.
- **Status:** not a bug (harmless redundancy); ported verbatim.

### The Temperature window's icon label is `#f`
- **Seen:** loop0002 item 14, translating temperature-graphics.ss to Python.
- **What:** `new-temperature-window` sends `(tell graphics-window 'set-icon-label title)`
  right after its `let*`, where `title` is still `#f`: the title ("Temperature" or
  "Temp.") is only chosen later, by `initialize-parameters`. constants.ss has no
  `%temperature-icon-label%`, unlike the other windows. So the icon label is `#f`.
- **Evidence:** temperature-graphics.ss lines 48–75; python/tests/test_gui_panels_a.py's
  `test_temperature_window` checks the `set-icon-label` message.
- **Status:** won't fix (it's the original's behaviour); ported verbatim in Racket and
  Python.

### Exact bond densities meet flonum thresholds: 1/5, 2/5 and 4/5 fall into the hotter class
- **Seen:** loop0002 iteration 7 (item 06), writing python/oracle/batteries/workspace-extra-battery.scm.
- **What:** formulas.ss's `current-translation-temperature-threshold-distribution` computes
  the bond density as an exact rational (bonds over letters minus one) and compares it with
  `(>= density 0.8)`, `0.6`, `0.4`, `0.2`. Chez compares an exact rational with a flonum
  exactly, and the doubles nearest 0.8, 0.4 and 0.2 lie just *above* 4/5, 2/5 and 1/5
  (the one nearest 0.6 lies below 3/5). So a density of exactly 1/5, 2/5 or 4/5 picks the
  next-hotter distribution, while 3/5 picks "low" as written. For 15 possible bonds
  (`abcdef abcdeg ijklmn`), k = 3, 6, 12 give classes 4, 3, 1 rather than 3, 2, 0. The
  same thing happens in the Workspace's `get-activity`: `(min 1.0 (/ age 500))` makes the
  ratio a flonum before `100*` rounds it, and an average age of 545/2 or 575/2 rounds
  the other way from the exact value (activity 45 and 43; exact rounding would give 46
  and 42).
- **Evidence:** `density-boundaries`, `activity-float-ties` in
  python/oracle/batteries/workspace-extra-battery.scm (fixtures in
  python/fixtures/workspace-extra/).
- **Status:** won't fix (it's the original's behaviour). Python's `Fraction` compares with
  `float` exactly too, so a plain `>=` reproduces it; `float(density) >= 0.8` would not
  (a mutation the battery catches).

## 🌀 Anomalies

### Most documented demo seeds replay exactly; a few don't
- **Seen:** iteration 2 (item 01). Revised in iteration 17 (item 16), which compared every
  demo with the dissertation's Chapter 5.
- **What:** the seeds in `demos.ss` were chosen by Marshall around 1999–2003. Under Chez 10
  most of them still give the same answers at the same codelet counts:
  - misc1: mmmrrj at 7794
  - misc2: abd at 1126
  - misc4: b at 453, y at 945
  - misc5: flz, dlz, hlz at 1721
  - misc9: dyz at 2257
  - Run 2: mrrkkk at 1747
  - Run 3: uyz at 3163
  - Run 4: no answer; it gives up at 3228. Item 01 listed run4 as not replaying because
    demos.ss names the answer dyz, but the dissertation's Run 4 never finds the rule
    either, and a Jootser ends it at 3228 (p. 226). So it does replay.
  - Run 5: gives up at 4493
  - Run 7: wyz at 2170
  - Figs. 5.7/5.8: ijll at 1172, hjkk at 733

  These don't reproduce:
  - misc3, and the "not used" misc6–8;
  - Run 6: aaabccc at 5976, where the dissertation gives up at 6196;
  - Run 8: qeeeq at 1013, where the dissertation never answers and a Jootser ends the run
    at 5933;
  - fig5.5-bottom: qxeeq, where the figure shows qeeq;
  - the eqe-qeeeq demo: it answers qcccb (Metacat itself calls it "really terrible"; see
    `docs/screenshots/racket-eqe-abbbc.png`);
  - fig5.11: xyd, where the dissertation shows yyz. The dissertation says that run
    continues the one in Fig. 4.12, so a seed alone can't reproduce it.

  Also, fig5.4-bottom (seed 175910650) and fig5.5-bottom (seed 4109591222) both stop at
  codelet 2899, with different traces and different answers (qeeq and qxeeq). This looks
  like a coincidence.
- **Evidence:** `chez_scheme/oracle/tests/demo-replay-check.ss` (the original);
  `racket/tests/demos-test.rkt` (the port); the table in `docs/demos.md`; the goldens of
  every demo seed.
- **Status:** open. Chez's global `random` has evidently been the same 32-bit LCG for
  decades, and the replaying runs include long ones (misc1 runs 7794 codelets), so most of
  the program's draws are unchanged. The misses could come from code changes between the
  dissertation (1999) and version 1.2, from changes in Chez's argument evaluation order,
  or from a lost setting.

### A string image's `reset` forgets its original direction
- **Seen:** iteration 6 (item 05).
- **What:** `make-string-image` takes a direction, but `reset` always sets it to
  `plato-right`, so a string image made with `plato-left` generates its letters reversed
  until the first reset, and in order afterwards.
- **Evidence:** battery test `string-image-left` (and `string-image-operations`).
- **Status:** not a bug. The only caller, workspace-strings.ss:30, makes every string
  image with `plato-right`.


### Every raw importance is 0 at the start of a run
- **Seen:** iteration 7 (item 06), dumping the initial workspace of every problem.
- **What:** `init-mcat` sets each letter's descriptors (`a`, `letter`, `leftmost`, ...) to
  full activation, but a description counts toward raw importance only if it is
  `relevant?`, which tests the description *type* (`LetterCtgy`, `StringPos`, ...). The
  types start inactive, so every raw importance is 0, every object in a string gets the
  same relative importance (33 in `abc`, 17 in `mrrjjj`), and the initial saliences come
  from unhappiness alone. Importance only starts to matter once the types become active.
- **Evidence:** battery tests `ws-problem-*` in `tests/diff/workspace-battery.scm`
  (`(importance 0 33)` for each letter of `abc`); `workspace-init-check.ss` checks the
  same dump after the real `init-mcat`.
- **Status:** explained (the original's behaviour, ported as is). Whether Marshall meant
  it is not known; the comment in run.ss only says `set-activation` avoids trace events.

### The goldens never exercise a partly active theme's spread or the trace's importance thresholds
- **Seen:** loop0002 iteration 11 (item 10), mutation checks of the Python translation.
- **What:** three mutants of the Python port pass all 109 goldens:
  - a theme's `spread-activation-to-slipnet` with the square of its activation instead of
    the cube: in every golden, the themes that `get-all-active-themes` returns are fully
    active (|activation| = 100), so the draw is against 0 or 1 either way (the Racket
    port's item 10 found the same for thematic-bridge-scout's cluster probability);
  - `%concept-mapping-importance-threshold%` 65 → 60: no concept mapping's importance
    falls between 60 and 64 in the goldens;
  - `%group-importance-threshold%` 100 → 99: no group event candidate has strength 99.
- **Evidence:** python/oracle/batteries/trace-extra-battery.scm reaches all three (an
  importance of 63 for succgrp=>predgrp on a non-spanning bridge, a group of strength 99,
  themes at activations 30, 50, 80 and −60), and its fixtures kill the three mutants.
- **Status:** explained.
- **Update (loop0002, item 12):** the 720 extra-seed runs (python/tests/test_extra_seeds.py)
  reach both thresholds: the 65 → 60 mutant changes 3 of them and the 100 → 99 mutant 5,
  while each still passes all 109 goldens. The square-for-cube mutant passes the 720 too,
  so a partly active theme's spread still shows only in trace-extra-battery.scm.

### A Stop pressed just after Go can be lost (original)
- **Seen:** 2026-10-04, loop0003 item 05, `drive_qt_gui.py`'s 50 Go/Stop toggles: run 7
  went on to its answer during the toggling.
- **What:** run.ss's `go` first sends `switch-to-run-mode` to the control panel (which
  enables Stop), then does `(set! *interrupt?* #f)`. `stop-button-action` only does
  `(set! *interrupt?* #t)`. A Stop pressed after Stop is enabled but before `go` clears the
  flag is undone, and the run goes on. In the original the window is the time SWL took to
  run the control panel's widget calls; in the tkinter GUI it is the time `ThreadSafeTk`
  takes to hand the call back; in the Qt GUI `switch-to-run-mode` is posted and returns
  at once, so the window is a few bytecodes of the engine thread. A second Go queued
  before the first one shows also resumes the run again, past an answer if it is reached
  meanwhile.
- **Evidence:** `chez_scheme/original/run.ss` lines 124–133 and gui.ss's
  `stop-button-action`; `python/tests/drive_qt_gui.py`'s `toggles`.
- **Status:** won't fix (the original's behaviour, and the trace is unaffected: the flag
  draws no random number). The toggles test presses Go only in input mode, Stop once the
  run shows, and stops toggling at the answer.

### Qt's offscreen platform warns when a dialog is shown
- **Seen:** 2026-10-04, loop0003 item 05, the breakpoint and step interval dialogs under
  `QT_QPA_PLATFORM=offscreen`.
- **What:** each dialog prints `This plugin does not support propagateSizeHints()` on
  stderr. It's Qt's offscreen plugin saying it has no window manager to tell a size hint
  to; the dialog works.
- **Evidence:** `QT_QPA_PLATFORM=offscreen python3 python/tests/drive_qt_gui.py /tmp/dq
  step_mode`.
- **Status:** harmless; ignored.

### Qt's offscreen screen ignores its own `dpr` key for windows
- **Seen:** 2026-10-05, loop0003 item 08, the high-DPI test.
- **What:** an offscreen screen configured with `"dpr": 2` in the plugin's JSON file
  reports `devicePixelRatio() == 2`, but its windows have `devicePixelRatioF() == 1` and
  `grab()` gives 1x pixmaps. `QT_SCALE_FACTOR=2` on a screen twice as large in device
  pixels gives what a 200 % desktop gives: logical sizes halved, windows and grabs at 2x.
- **Evidence:** `QT_QPA_PLATFORM=offscreen:configfile=S.json python3 -c "..."` with
  `dpr` 2 against `QT_SCALE_FACTOR=2` (2026-10-05, PySide6 6.11.2).
- **Status:** worked around: `python/tests/drive_qt_layout.py` emulates `WxH@2` with a
  screen of 2W×2H and `QT_SCALE_FACTOR=2`.

### A hidden splitter child takes its handle with it, and a tool bar hides what doesn't fit
- **Seen:** 2026-10-05, loop0003 item 08.
- **What:** when the Slipnet is hidden, the middle row's visible sizes add up to 4 px more
  than before: `QSplitter` hides the hidden child's handle too. And the control strip is a
  `QToolBar`'s widget: the tool bar doesn't pass the strip's minimum width on to the
  window, so a narrower window would put the end of the strip (Stop, Reset) behind an
  extension button. The window's minimum width is therefore set from the strip's
  `minimumSizeHint` (1684 px + the bar's margins with the 1080p fonts).
- **Evidence:** `python/tests/test_qt_layout.py::test_hiding_a_pane_closes_the_gap` and
  `test_the_window_has_a_minimum_size`.
- **Status:** explained (Qt's documented behaviour); the minimum width works around the
  second.

## ⚙️ Chez / Racket quirks

### Chez doesn't evaluate arguments left to right
- **Seen:** iteration 2 (item 01).
- **What:** `(f (show 1) (show 2) (show 3))` prints `312`. `let` goes right to left, while
  inlined `+` and `cons` go left to right. Racket always goes left to right.
- **Status:** worked around. Every call site where argument order changes the order of
  random draws must be ported in Chez's order (`porting-notes.md`, `trace-format.md`).
- **Update (loop0002, item 01):** a 2-binding `let` *inside a lambda* went left to right
  under `scheme --script`: `((lambda () (let ((a (show 1)) (b (show 2))) 0)))` prints
  `12`, while the documented top-level `let` prints `21`. In the same script, a 3-argument
  call printed `312` both at top level and inside a lambda. The order depends on context,
  so a site can't be read off a rule; docs/python-translation-plan.md lists the known
  sites, which Python (left to right, like Racket) must order by hand.

### Chez's `map` applies its procedure in a strange order
- **Seen:** iteration 4 (item 03).
- **What:** for one or two lists, the library `map` goes from the end towards the front in
  pairs (7 elements: 7 5 6 3 4 1 2). For three or more lists it goes last to first. When
  the compiler inlines `map` on a literal or short quoted list, the order is the
  compiler's: 3 2 1 in one context, 1 2 3 in another.
- **Status:** worked around in `racket/compat.rkt` for the library case. The inlined cases
  are open; they must be checked against the goldens site by site.

### `sort`, `remq`, `for-each` and one-armed `if` differ between Chez and Racket
- **Seen:** iteration 4 (item 03).
- **What:**
  - `sort`: Chez takes `(sort pred list)` with its own merge sort, which sorts the second
    half first and gives different results with `<=`/`>=`.
  - `remq`/`remove`: Chez removes every occurrence; Racket removes only the first.
  - `for-each`: Chez returns the last application's value.
  - `if`: Chez allows a one-armed `(if test then)`.
- **Status:** worked around in `racket/compat.rkt`. The differential battery in
  `tests/diff/` checks each one against Chez.

### A raw `.zo` load ignores changes to included files; the compilation manager counts whole seconds
- **Seen:** iteration 7 (item 06), mutation checks on the ported workspace files.
- **What:** the differential runner requires engine.rkt into a fresh namespace. The
  default load handler uses `compiled/engine_rkt.zo` if it isn't older than engine.rkt
  itself, so after an edit to an included `engine/*.rktl` the battery ran the old engine
  and every mutation passed. Two more traps: the compilation manager's load handler skips
  modules whose namespace has a different module registry from the one it was created in,
  so it has to be created inside the new namespace; and it compares timestamps in whole
  seconds, so a file edited in the same second as the last compile isn't recompiled.
- **Evidence:** before the fix, changing `(list 70 30)` in formulas.rktl left
  `raco test racket/tests/workspace-diff-test.rkt` green until `raco make racket/engine.rkt`;
  after it, 2 tests fail.
- **Status:** worked around in `racket/tests/diff-runner.rkt` (compilation manager,
  created inside the namespace). Mutation scripts wait one second around each edit.
- **Update (iteration 14, item 13):** the same trap across modules. headless.rkt's
  `run-problem` gained a keyword argument, but `raco test racket/` and plain `racket`
  (cli-test.rkt runs `racket racket/cli.rkt`) loaded the old `cli_rkt.zo`, compiled
  against the old headless.rkt: "instantiate-linklet: mismatch; reference to a variable
  that is not exported". The gate failed with 39 failures in cli-test.rkt.
  tests/run-tests.sh now runs `raco make` on every module of racket/ before
  `raco test`. A mutation restored in the same second as the mutated compile also left
  one stale `.zo` (bridge-graphics.rktl); the mutation script now waits before
  restoring too.

### Chez evaluates `append`'s second argument first
- **Seen:** iteration 8 (item 07): the codelet harness, `abc abd iijjkk` seed 3, the
  update after codelet 735 (only in a 2000-codelet run; the 400-codelet runs pass).
- **What:** a group's `get-local-density` (groups.ss) computes
  `(append (neighbors self 'choose-left-neighbor) (neighbors self 'choose-right-neighbor))`,
  and both calls can draw. Chez evaluates the right one first; Racket the left one. The
  port drew one number early, which surfaced as different slipnodes jumping to full
  activation in the next `update-slipnet-activations`.
- **Evidence:** `(define (show x) (display x) (list x))` then
  `((lambda () (append (show 'L) (show 'R))))` prints `RL` under `scheme --script`;
  racket/tests/codelet-diff-test.rkt (the `codelets-long-iijjkk-3` run).
- **Status:** worked around (groups.rktl binds the right neighbours first, marked `port:`).

### `tanh` is not in racket/base
- **Seen:** iteration 7 (item 06), compiling workspace.ss.
- **What:** Chez has `tanh` built in; Racket has it only in racket/math, which computes
  it in Racket code and can differ from libm in the last bit.
- **Status:** worked around: compat.rkt uses Chez's own primitive through
  `ffi/unsafe/vm`'s `vm-primitive` (Racket CS's Chez is 10.3; the battery checks 500+
  values bit for bit against Chez 10.0).

### `go` is the one place the original re-enters a continuation
- **Seen:** iteration 12 (item 11), porting run.ss.
- **What:** `break` and `quiet-break` capture a continuation with `continuation-point*`
  (a full `call/cc` in syntactic-sugar.ss) and then `(reset)` to the REPL; `go` resumes
  the run by calling that continuation after the REPL has moved on. Every other use of
  `continuation-point*` only escapes upwards, which is why compat.rkt implements it with
  `call/ec` (item 03). With `call/ec`, `go` would jump into a dead escape continuation.
- **Evidence:** racket/tests/run-test.rkt stops a run at codelets 150, 300 and 450 with
  the engine's own `break` and resumes it with `go`; the generator state, codelet count
  and temperature equal those of the same run never stopped.
- **Status:** worked around: run.rktl's `break` and `quiet-break` use Racket's `call/cc`
  directly (marked `port:`). Racket's full continuations reach up to the nearest prompt,
  so the caller of `run-mcat` and of `go` must each install one
  (`call-with-continuation-prompt`), and the reset handler must be set, not
  parameterized: a parameterization is part of the captured continuation, so `go` would
  bring back the first caller's dead escape. The GUI item has to respect both.

### racket/draw's unsmoothed 1-pixel lines include their end point; Tk's don't
- **Seen:** iteration 13 (item 12), the SGL fixture's dashes and polypoints.
- **What:** with smoothing `'unsmoothed`, `draw-line` with a 1-pixel pen from x=2 to
  x=8 paints 7 pixels, and a zero-length line paints one. X11 (and so Tk) draws a
  butt-capped line of width 1 from p to q over the pixels before q: 6 pixels, so a
  Tk dash "- " is 6 on, 6 off, and Tk's polypoints (lines one pixel long) are single
  pixels. Pens of width 3 already paint exactly 6. Also, `make-font #:smoothing
  'smoothed` gives subpixel (coloured) antialiasing on this desktop; `'partly-smoothed`
  is the greyscale one.
- **Evidence:** racket/tests/sgl-test.rkt checks the dash pixels along a line (fails
  with `(1 1 1 1 1 1 1 0 …)` without the fix).
- **Status:** worked around in racket/gui/sgl.rkt: a thin run (a whole path, or one
  dash) stops one pixel step short of its last point; fonts used `'partly-smoothed`
  until item 13, and are `'unsmoothed` since (see "Erasing antialiased text leaves
  fringes").

### Chez's `record-case` ignores extra arguments; the port's raised
- **Seen:** iteration 14 (item 13), the first run with the Workspace window attached.
- **What:** the Temporal Trace (trace.ss lines 442 and 473–478) sends the Workspace
  window `(draw-string-letters string 'answer)`, with a tag the window's method
  `(draw-string-letters (string) ...)` doesn't take. Chez's `record-case` binds the
  formals with `car`/`cdr` (`(expand '(record-case r [(a) (x) x]))` shows it), so the
  extra argument is ignored and too few arguments raise "car: () is not a pair". compat's
  `record-case` applied a `lambda`, which raised an arity error at the first answer of
  every run once workspace graphics were on.
- **Evidence:** racket/tests/compat-test.rkt (extra, missing and rest arguments);
  without the fix, racket/tests/workspace-view-test.rkt (now views-test.rkt) fails on every run that reaches
  an answer.
- **Status:** worked around. compat's `record-case` binds as Chez does (item 13).

### The graphics erase text by overpainting, which leaves fringes around antialiased text
- **Seen:** iteration 14 (item 13), the first renders of the Workspace window.
- **What:** to erase, the original draws the same pexp again in the background colour.
  Over antialiased text this leaves grey fringes: a ghost "?" where the answer's letters
  went, and smears after concept mappings whose font changed from irrelevant (italic) to
  relevant (bold italic).
- **Evidence:** set `#:smoothing 'partly-smoothed` in racket/gui/fonts.rkt and run
  `racket racket/tests/views-harness.rkt mrrjjj-answer /tmp/a.png`.
- **Status:** worked around. Text is drawn aliased, as X11's core fonts drew it in the
  dissertation's screenshots (divergences.md).

### Image-mode text boxes are narrower than italic glyphs
- **Seen:** iteration 14 (item 13), the bridge labels (yellow boxes) in 4× crops.
- **What:** `draw-text` sizes an image-mode text's background box from the text's
  width. Italic digits overhang it by a pixel or two on the right.
- **Evidence:** the snapshots in racket/tests/snapshots/workspace-*.png.
- **Status:** not a bug, as far as anyone can tell: Tk sized the box the same way. Item 12
  ported sgl-interpreter.ss's arithmetic verbatim. Kept.

### GTK ignores Xvfb when `WAYLAND_DISPLAY` is set
- **Seen:** iteration 16 (item 15), the first GUI runs under `xvfb-run`.
- **What:** the owner's session is Wayland (`WAYLAND_DISPLAY=wayland-0`). GTK prefers
  Wayland over `DISPLAY`, so `xvfb-run racket ...` opened the windows on the owner's
  screen instead of the virtual display. The root window of Xvfb had no children and a
  screen grab was black. Three short scratch runs (a few seconds each) showed windows on
  the owner's screen before this was noticed.
- **Evidence:** `env | grep WAYLAND`; `xwininfo -root -tree` inside `xvfb-run` lists no
  windows unless `WAYLAND_DISPLAY` is unset.
- **Status:** worked around. Every GUI run uses
  `env -u WAYLAND_DISPLAY GDK_BACKEND=x11 xvfb-run -a ...` (tests/run-tests.sh,
  tests/gui-screenshot.rkt's header), and racket/gui-tests/control-panel-test.rkt
  refuses to run while `WAYLAND_DISPLAY` is set.

### A racket/gui canvas's `on-size` does not see its scrollbars
- **Seen:** iteration 16 (item 15), the Commentary window.
- **What:** showing a manual scrollbar shrinks a canvas's client area, but `on-size`
  reports the whole canvas, so no resize happened. The Commentary's first paragraph ran
  under its vertical scrollbar. Also, a canvas's minimum client size counts only the
  scrollbars shown when it is set.
- **Evidence:** the screenshot steps in PROGRESS.md (iteration 16).
- **Status:** worked around in racket/gui/gui.rkt's `screen-host%`: the refresh tick
  compares the client size with the viewport's, and `set-resizable!` shows the
  scrollbars and sets the minimum size again before the window becomes resizable.

### The SWL message-queue stand-in deadlocked the GUI thread
- **Seen:** iteration 16 (item 15), the first on-screen run.
- **What:** views.rkt's stand-in for SWL's `thread-receive-msg` took the semaphore and
  the message in two steps. When the resize listener thread ran between them, the
  resize handler (in the GUI thread, inside `critical-section`) saw a waiting message,
  waited on the semaphore, and blocked forever.
- **Evidence:** a GUI run hung with a backtrace in `thread-receive-msg` under
  `critical-section`.
- **Status:** explained and fixed: a receive is now atomic, and `critical-section` runs
  in Racket's atomic mode.

### `dynamic-require` of a runtime path breaks under `raco exe`
- **Seen:** iteration 17 (item 16), building the standalone program.
- **What:** racket/main.rkt loaded the GUI with `(dynamic-require gui-path 'setup)`, where
  `gui-path` came from `define-runtime-path`. That works with `racket`, but `raco exe` does
  not embed a module that is only reached that way. The distributed `metacat` then exits
  with status 1 as soon as it opens the GUI. Headless runs were not affected, because they
  never reach that module.
- **Evidence:** with HEAD's racket/main.rkt, `racket/gui-tests/dist-test.rkt` fails 2 of 3
  checks ("the GUI is still up": actual 1).
- **Status:** worked around. main.rkt and racket/metacat.rkt use `lazy-require`, which
  registers the module with `raco exe` and still loads racket/gui only when the GUI starts.

### A process in a bubblewrap sandbox outlives its killed `bwrap`
- **Seen:** iteration 17 (item 16), in the first version of dist-test.rkt.
- **What:** killing `bwrap` left the sandboxed GUI running, re-parented to the session.
  It kept the test's stdout pipe open, so the test hung on reading it.
- **Evidence:** `ps` showed `/opt/metacat/bin/../lib/plt/gracketcs-8.18 ...` with parent
  4284 (the session) after the test had killed bwrap.
- **Status:** worked around. The test passes `--die-with-parent --unshare-pid` to bwrap.

### Chez's printer rounds a halfway last digit up; Python's `repr` rounds it to even
- **Seen:** loop0002 iteration 3 (item 02), `number->string-bits` and `-decades` in
  python/oracle/batteries/chez-battery.scm.
- **What:** both print the shortest digits that read back to the double. When the double
  lies exactly halfway between the two shortest candidates, Chez takes the upper one and
  Python the even one: 1586243275893042.25 prints as `1.5862432758930423e15` in Chez and
  `1.5862432758930422e15` in Python (`repr`), and 71684848459136.625 as `...136.63` and
  `...136.62`. Racket happened to agree with Chez on every value its tests printed.
- **Evidence:** `python/fixtures/chez/*number-%3Estring-ties.txt` (17 ties among 5,240 doubles);
  `python3 -c "print(repr(1586243275893042.25))"`.
- **Status:** worked around. `chez._flonum_digits` starts from `repr` and moves a halfway
  last digit up, keeping it if it still reads back. `test_number_to_string_ties`.

### Chez writes non-ASCII symbol characters as `\xHH;` unless they are R6RS constituents
- **Seen:** loop0002 iteration 3 (item 02), `write-symbols`.
- **What:** in a symbol, `write` prints a character above 127 as is only if its Unicode
  category is Lu, Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So or Co (plus Nd,
  Mc, Me after the first character). U+0080–U+00A0, U+00AB `«`, U+00AD, U+00BB `»`,
  U+2028, U+2029 and U+FEFF become `\x80;` and so on. racket/compat.rkt writes every
  non-ASCII character as is. The model's symbols are ASCII, so it never mattered.
- **Evidence:** `python/fixtures/chez/042-write-symbols.txt`.
- **Status:** worked around in chez.py (`unicodedata.category`). Racket's difference
  is harmless and left alone (racket/ is frozen).

### Exact 0 is the identity of `+` and `-`: `(+ 0 -0.0)` is `-0.0`
- **Seen:** loop0002 iteration 3 (item 02), `arith-add`, `arith-sub`, `arith-nary`.
- **What:** Chez returns the other operand when one is exact 0. So `(+ 0 -0.0)` is
  `-0.0`, `(- 0 0.0)` is `-0.0` (negation), and `(max 0 -0.0)` and `(min 0 -0.0)` are
  `-0.0`. Python gives `0.0` for `0 + -0.0` and `0 - 0.0`. With `(* 0 x)` and `(/ 0 x)`
  (exact 0 for any flonum `x`, even `+inf.0` or `0.0`), this is where Python's mixed
  arithmetic differs from Chez's.
- **Evidence:** `python/fixtures/chez/004-arith-add.txt` and the other `arith-*` tables;
  the `-inline` variants agree with the procedure calls.
- **Status:** worked around: `chez.add`/`sub`/`mul`/`div`/`max_`/`min_`. Only the sign
  of a zero changes, and that only shows when it's printed or divided by.

### Chez's `expt`: exact roots only for 1/2, exact results for exact 0 and 1 bases
- **Seen:** loop0002 iteration 3 (item 02), `expt-table`, `expt-extra`, `expt-model`.
- **What:** an exact 0 power gives exact `1` (`(expt 0.0 0)` → `1`). An exact base to an
  exact integer power is exact. A power of `1/2` is `sqrt` (`(expt 4 1/2)` → `2`), but
  other exact roots are inexact (`(expt 8 1/3)` → `2.0`, `(expt 4 3/2)` → `8.0`). An exact
  1 base gives exact `1` for any power. An exact 0 base gives exact `0` for a positive
  power (`(expt 0 0.95)` → `0`), `1.0` for `0.0`, and an error for a negative power.
  `(expt 0.0 -1)` is `+inf.0`, where Python raises `ZeroDivisionError`. A negative base to
  a non-integer power is `exp(p log b)` (complex). Otherwise it's libm's `pow`, as Python's
  `**` (`(expt 1.1 27)`, not repeated multiplication).
- **Evidence:** `python/fixtures/chez/*expt*`.
- **Status:** explained; chez.py's `expt` follows it. The model's `(expt strength 0.95)`
  meets the exact-0 case whenever a strength is 0.

### `set-top-level-value!` binds an unbound name in Chez
- **Seen:** loop0002 iteration 3 (item 02), `top-level-values`.
- **What:** in Chez's interaction environment, `(set-top-level-value! 'x 4)` of an
  unbound `x` binds it, while `top-level-value` of an unbound name raises.
  racket/compat.rkt raises for both. The model only sets names it has defined.
- **Status:** explained; chez.py does as Chez.

### A bad literal `format` string makes Chez's compiler warn, which fails a whole battery test
- **Seen:** loop0002 iteration 3 (item 02), writing chez-battery.scm.
- **What:** `(format "~a")`, `(format "x" 1)` and `(format "~q" 1)` with literal strings
  draw "Warning in compile: too few arguments for control string", even inside a handler.
  diff-eval.ss's handler sees the warning condition and prints `ERROR` for the test.
  Called through a variable (`c:format`), they raise at run time as expected.
- **Status:** worked around in the battery.

### Python raises where IEEE arithmetic (and Chez) give an infinity
- **Seen:** loop0002 iteration 3 (item 02).
- **What:** `1 / 0.0`, `0.0 ** -1`, `math.log(0.0)` and `float(10**400)` raise in
  Python. Chez gives `+inf.0`, `+inf.0`, `-inf.0` and `+inf.0`. `math.exp(1000)` raises
  `OverflowError`. Also `round(math.inf)` raises, where Chez's `round` returns `+inf.0`.
- **Status:** worked around in chez.py (`div`, `expt`, `log`, `inexact`, `exp`, `round_`).
  Plain Python operators on model floats must not meet these cases.

### Python interns only identifier-like string constants, so `'invalid-message-indicator` needs one shared object
- **Seen:** loop0002 iteration 4 (item 03), writing objects.py.
- **What:** two modules that each write the literal `"invalid-message-indicator"` get two
  different objects: `a.X is b.Y` is `False`. Python interns string constants
  automatically only when they look like identifiers, and the hyphens rule that out. The
  same goes for a symbol built at run time (`"-".join(...)`). `tell` and `delegate` test
  the indicator with `is`, so a producer that spells it out would not halt where Chez
  halts.
- **Evidence:** `python3 -c 'import a, b; print(a.X is b.Y)'` with the literal in a.py and
  b.py prints `False`.
- **Status:** worked around: every producer uses `objects.INVALID` (a `sys.intern`ed
  constant), and other symbols are compared with `==`, never `is`
  (docs/python-translation-plan.md, "eq? and identity").

### Python's `complex` cannot hold Chez's exact complex numbers
- **Seen:** loop0002 iteration 4 (item 03), utilities battery `coords`.
- **What:** `(coord 3 4)` (`make-rectangular`) is the exact `3+4i` in Chez, and
  `(x-coord c)` gives back the exact 3. Python's `complex` holds two floats. Also,
  `(make-rectangular 1.5 0)` is the real `1.5` (an exact zero imaginary part vanishes),
  `(make-rectangular 1 2.0)` is `1.0+2.0i`, and `(imag-part 1.5)` is the exact `0`.
- **Evidence:** fixtures `coords` and `number->string-exact` of the utilities battery.
- **Status:** worked around: `chez.ExactComplex` and `chez.make_rectangular`,
  `real_part`, `imag_part`, printed by `number_to_string`. Arithmetic on coordinates
  (the graphics: `magnitude`, `+` on coords) isn't there yet; the graphics items add it.
- **Update (loop0002, the graphics engine):** chez.py now does `+ - * /` on
  `ExactComplex` and `complex` with Chez's rules, plus `magnitude`, `angle`,
  `make-polar`, `cos`, `sin`, `tan` and `acos` (next entry).

### Chez's complex arithmetic: part by part, signed zeros, libm's `hypot`, exact angles
- **Seen:** loop0002, the graphics engine (general-graphics.ss's line builders, Python).
- **What:** probed under `scheme --script`:
  - a real and a complex number combine part by part, the real's imaginary part being
    an exact 0: `(- 0.5 0.1+0.0i)` is `0.4-0.0i`, `(+ 1/2 0.1-0.0i)` is `0.6-0.0i`,
    `(* 2 0.1-0.0i)` is `0.2-0.0i`; `(* 0 z)` is exact `0` but `(* 0.0 z)` is
    `0.0+0.0i`; a complex divided by a real is divided part by part. Python's `complex`
    turns the real into `x+0j` first and loses the signed zeros;
  - `magnitude` of a flonum complex is libm's `hypot` (Python's `abs(complex)`);
    Python's `math.hypot` has its own algorithm and differs in the last bit on 36 of
    20,000 random arguments (`0.8455286293882489-0.5842277562918272i`: Chez
    `1.027735731760336`, `math.hypot` `1.0277357317603362`);
  - `(angle 0.5)` and `(angle 1/2)` are the exact `0`, `(angle -0.0)` is `pi`,
    `(angle 0)` is an error; `(make-polar 1/2 0)` is `1/2` but `(make-polar 1/2 0.0)`
    is `0.5+0.0i`; `(cos 0)` is `1`, `(sin 0)`, `(tan 0)` and `(acos 1)` are `0`.
  These keep exact horizontal lines exact (dotted-line's points stay rationals).
- **Evidence:** python/tests/test_graphics.py, `test_complex_arithmetic_as_chez` (Chez's
  printed values) and the graphics battery (flonum coordinates to the last bit).
- **Status:** worked around: chez.py section 4b. Division by a complex number raises
  `NotImplementedError` (Metacat never divides by one).

### Mutation checks: a restored file can run as its mutant (stale `.pyc`)
- **Seen:** loop0002, the graphics engine's mutation checks.
- **What:** a mutant that keeps the file's size (`sign = add if ... else sub` swapped),
  restored with `cp` in the same second, is not recompiled: Python's `.pyc` check is the
  source's size and mtime in whole seconds, so the next run imported the mutant's
  bytecode and a correct file failed a test.
- **Evidence:** reproduced by mutating and restoring python/metacat/bridge_graphics.py
  within one second, then running python/tests/test_graphics.py.
- **Status:** worked around: run mutation checks with `PYTHONDONTWRITEBYTECODE=1` and
  delete the module's `__pycache__` entry after each restore.

### Parallel scene renders grab each other's windows (port test bug, fixed)
- **Seen:** loop0002 iteration 15, gate fix 1: `test_render_scenes` failed with
  `temperature-run7-300.png: blank` (one colour), although the scene renders correctly by itself.
- **What:** python/tests/render_views.py runs the eight scenes as parallel processes on one
  Xvfb display. Each one moves its windows to `+0+0`, raises them and grabs them with
  XGetImage, which reads the screen rather than the window's own contents. So when another
  scene raised a window between our lift and our grab, the picture showed that window's
  plain background. Whether it happens depends on timing and machine load.
- **Evidence:** reproduced only under the full suite's load; `render_views.py OUTDIR run7-300`
  alone always gave 5 colours for the Temperature window.
- **Status:** fixed: the raise-and-grab step holds an exclusive `fcntl` lock on
  `OUTDIR/.grab-lock`, so only one scene grabs at a time (the runs still go in parallel).

### Python's `bool` is an `int`, so a Scheme `#f` that reaches arithmetic computes silently
- **Seen:** loop0002 iteration 5 (item 04), translating coderack.ss's `get-removal-weight`
  (`(* (- *codelet-count* time-stamp) ...)`), where `time-stamp` is `#f` until the codelet
  is posted.
- **What:** `100 - False` is `100` and `3 * True` is `3` in Python; Chez raises
  `-: #f is not a number`. A model slip that lets `#f` into arithmetic would give a number
  instead of the original's error, and the run would go on differently.
- **Evidence:** `python3 -c 'print(100 - False)'` prints `100`; `chez.sub(5, False)` raises
  `SchemeError`.
- **Status:** worked around: arithmetic on a model quantity that may be `#f` goes through
  `chez.add`/`sub`/`mul`/`div`, which check their operands (`chez._is_number` excludes
  `bool`); `chez_num_eq` in coderack.py does the same for `=`.

### pytest's diff of two multi-megabyte strings takes minutes
- **Seen:** loop0002 iteration 5 (item 04), mutation checks of the coderack battery.
- **What:** `assert canon(value) == fixture` on a failing case of 100 KB-1 MB made pytest
  spend 1-2 minutes (or more) building its assertion explanation, even with `--tb=no`,
  so a mutant looked like a hang.
- **Evidence:** a mutant of `CoderackBin.choose_random_codelet` with the plain assertion
  timed out at 120 s; with the assertion below it fails in 0.2 s.
- **Status:** worked around: test_coderack.py compares first (`same = got == expected`),
  then asserts `same` with a message showing the first differing character and its
  context.

### Python's negative indexes wrap around where Chez's `list-ref` and `car` raise
- **Seen:** loop0002 iteration 6 (item 05), translating slipnet.ss's
  `number->platonic-number`, `(nth (- n 1) *slipnet-numbers*)`.
- **What:** for `n` = 0, Chez's `(list-ref l -1)` is an error; Python's `l[-1]` quietly
  gives the last element (`plato-five`). In the same way, utilities.py's `first`, `nth`
  and `last` are plain indexing, so `(1st '())` raises `IndexError`, not a
  `chez.SchemeError` (both stop the run, which is what matters, but tests that expect a
  Chez `ERROR` have to accept either).
- **Evidence:** `python/oracle/batteries/slipnet-extra-battery.scm`,
  `number-to-platonic-number-zero` (Chez: `ERROR`); `relationship-between-one` in
  `tests/diff/slipnet-battery.scm` (the `1st` of `'()`).
- **Status:** worked around: `slipnet.number_to_platonic_number` raises `SchemeError` for
  `n` < 1. Elsewhere an index is never negative in the model; a translation that computes
  one (`n - 1`, `len - k`) should check it.

### The test files share one engine, so one battery's fake top-level values leak into later files
- **Seen:** loop0002 iteration 6 (item 05).
- **What:** pytest runs every test file in one process, and `engine.load()` runs once. The
  Chez batteries each ran in a fresh Chez. test_utilities.py's slipnet-macro cases define
  `plato-p`, `plato-q`, `plato-z` and some links as fake top-level values (as
  utilities-battery.scm does), and they stayed there: any later file (test_workspace.py
  and after, alphabetically) would have seen `(top-level-value 'plato-z)` → `"node-z"`,
  and the link macros would have reached the fakes.
- **Evidence:** before the fix, running `tests/test_slipnet.py tests/test_utilities.py`
  and then a probe asserting `chez.top_level_value("plato-z") is slipnet.plato_z` failed.
- **Status:** worked around: test_utilities.py and test_slipnet.py restore
  `chez.TOP_LEVEL` after their cases (module fixtures), and test_slipnet.py resets the
  slipnodes and the coderack when it ends. Later test files that change engine state should
  do the same.

### A battery's fake codelet procedure outlives its test file
- **Seen:** loop0002 iteration 10 (item 09), the first gate run with test_rules.py.
- **What:** coderack-battery.scm gives `breaker` a fake two-argument procedure
  (`define-codelet-procedure*`). test_coderack.py ran it in the shared engine and left it
  installed, so test_rules.py, which runs later, failed the moment the real coderack chose
  a breaker: `TypeError: breaker() missing 2 required positional arguments`. test_bridges.py
  runs breakers too, but runs before test_coderack.py, so this had stayed hidden.
- **Evidence:** `bash python/run-tests.sh` before the fix (test_rules.py passed alone).
- **Status:** worked around: test_coderack.py's module fixture restores every codelet type's
  procedure when it ends. This is the same trap as the entry above.

### Python's comparisons of `Fraction` with `float` are exact, like Chez's
- **Seen:** loop0002 iteration 7 (item 06), formulas.py.
- **What:** `Fraction(4, 5) >= 0.8` is `False` in Python, as `(>= 4/5 0.8)` is `#f` in
  Chez: both compare the exact value of the double. This is what the model needs (see
  "Exact bond densities meet flonum thresholds"), but it is easy to break by converting
  first (`float(x) >= 0.8` is `True`), or by computing a quantity as a float where Chez keeps it exact.
- **Evidence:** `density-boundaries` in python/oracle/batteries/workspace-extra-battery.scm.
- **Status:** not a bug. Model code compares the exact value directly and never converts
  it first.

### The codelet battery's cases give the same traces in fresh forks as in one Chez process
- **Seen:** loop0002 iteration 8 (item 07), python/tests/test_codelets.py.
- **What:** diff-eval.ss runs codelet-battery.scm's 59 cases one after another in one Chez
  process, so state left by one case could reach the next (codelet types' counts, the
  Coderack, slipnode fields that `reset` doesn't clear, the generator). The Python test runs
  each case in its own process, forked from the state right after `engine.load()` and the
  harness's set-up, and all 59 traces equal Chez's byte for byte. `b:init-problem`,
  `reset` and `initialize` clear everything the traces observe, which is evidence for the
  plan's fork-after-load golden runner (docs/python-translation-plan.md, "A fresh engine
  per run").
- **Evidence:** `bash python/run-tests.sh -k test_codelet_battery`, 59 cases in a
  32-process pool, about 6 s, against python/fixtures/codelet/.
- **Status:** not a bug. A later battery whose traces did depend on order would show up
  as a failure in this kind of pool and not under Chez.

### Python: a break inside a codelet can only be resumed from a parked thread
- **Seen:** loop0002 iteration 12 (item 11), `abc abd xyz` seed 3: answer-finder reports
  `yyz` during codelet 2428 (the count still reads 2427) and calls `suspend`, so run.ss's
  `break` stops the run in the middle of that codelet.
- **What:** `break` captures a continuation with `call/cc` and calls `(reset)`; `go` calls
  the continuation, and the rest of the codelet runs. Python has no re-entrant
  continuations, and an exception would unwind the codelet. metacat/run.py runs a command
  that may break in an engine thread (`run.toplevel`); the break parks that thread and
  `go` releases it. A Chez continuation can also be called again after the run went on,
  which a thread can't do: the port raises "this break can no longer be resumed". No
  caller in the original does it (`*breakpoint-continuation*` is replaced at each break).
- **Evidence:** python/tests/test_run.py, `test_a_break_inside_a_codelet_resumes_that_codelet`:
  the run stops at 2427, 2429 and 2659 (the three answers) and 3000 (the breakpoint), and
  its generator state, codelet count and temperature at 3000 equal the oracle's
  `--keep-going` run's.
- **Status:** worked around (docs/python-translation-plan.md, "As built (item 11)").

### Python: `str.isalpha` is close to, but not, Chez's `char-alphabetic?`
- **Seen:** loop0002 iteration 12 (item 11), the CLI's argument parser (run.ss's
  `parse-args`: a word that starts with a letter is a string of the problem).
- **What:** Chez's `char-alphabetic?` is Unicode's Alphabetic property; Python's
  `str.isalpha` is the general categories L*. They differ on a few characters (letter
  numbers such as Roman numerals, some combining marks). Both accept every ASCII letter
  and reject digits and `-`.
- **Evidence:** python/tests/test_cli.py compares the CLI and the oracle on twelve bad
  argument lists, `1xyz` among them; all are ASCII.
- **Status:** won't fix. Only a problem typed with such characters would see it, and the
  original's letters are a–z.

### Text items at half pixels: the original sends Tk exact ratios
- **Seen:** iteration 14 (loop0002 item 13), capturing the Tcl stream of the SGL fixture
  from the oracle (`python/oracle/sgl-tcl.ss`).
- **What:** the viewport's `draw-text` puts a text item at
  `(+ (x->pixel 0 ox) text-relative-x-offset justification-offset)`, and the
  justification offset is `(* 1/2 width)` or `(* -1/2 width)`. For an odd text width
  the x coordinate is an exact ratio: the original sends
  `(tcl v1 create text 249/2 383 -text "cleared" ...)` to `swl:tcl-eval`. How SWL
  turned a ratnum into a Tcl word is unknown (SWL is not available). Tk reads canvas
  coordinates as doubles, so `249/2` itself would be an error; SWL presumably sent
  `124.5`.
- **Evidence:** `python/fixtures/sgl-tcl/v1.txt` (`grep "create text [0-9]*/2"`);
  `python/tests/test_sgl.py::test_tcl_stream_covers_every_form_and_command`.
- **Status:** worked around. The Python viewport sends the same exact ratio to its
  window (the stream test compares them), and `swl.tcl_word` turns a `Fraction` into
  a float when the command reaches Tk through tkinter. Tk then places the text at the
  half pixel as it does for any fractional coordinate.

### Tk's canvas PostScript leaves out the background
- **Seen:** iteration 14 (loop0002 item 13), rendering the SGL fixture to PNG.
- **What:** `canvas postscript` paints the items but not the canvas's background
  colour, so the fixture's ivory background (from `(clear "ivory")`) came out white
  through ghostscript, and the page was 643 × 483 pixels at 96 dpi.
- **Evidence:** a first version of `python/tests/render_sgl_fixture.py`, which used
  `postscript` + `gs`, failed its ivory pixel checks.
- **Status:** worked around. The script grabs the canvas window from the Xvfb server with
  Xlib's `XGetImage` (through ctypes) and writes the PNG itself with zlib, so the PNG
  holds exactly the pixels Tk drew. That needs neither PIL nor ghostscript.

### Python: tkinter's cross-thread Tk calls crash; the GUI marshals them itself
- **Seen:** loop0002 iteration 16 (item 15), the first GUI run driven under Xvfb.
- **What:** Tcl here is threaded, so tkinter lets any thread call Tk: it hands the call
  to the main thread and waits. With the engine drawing from its own thread (about 45,000
  canvas commands in run7), the process died with a segmentation fault in `mainloop`
  during the first run. Tcl objects that tkinter returns can be freed in the calling
  thread, and Tcl's per-thread memory does not allow that.
- **Evidence:** `python3 -X faulthandler python/tests/drive_gui.py` before the fix: the
  engine thread was in `sgl.draw_exps` (bridge-builder's graphics), the main thread in
  `mainloop`.
- **Status:** worked around. `gui/swl.py`'s `ThreadSafeTk` is the root's `.tk` (every
  widget shares it). A call from another thread is queued, a pipe that Tk's event loop
  watches wakes the main thread, and the result comes back as plain Python values.
  drive_gui.py then ran all its scenarios without a crash. The main thread answers within
  a few milliseconds during a run.

### Python: `int()` and `\d` read non-ASCII digits; Chez's `string->number` does not
- **Seen:** loop0002 iteration 16 (item 15), gui-battery.scm's `tokenize-string`.
- **What:** gui.ss's command-line parser reads "١٢" (Arabic-Indic digits) as a number
  token, because `char-numeric?` accepts those digits. Chez's `string->number` then gives
  `#f`, where Python's `int("١٢")` gives 12. Likewise "½" is numeric to both, and
  `string->number` gives `#f` for it.
- **Evidence:** python/fixtures/gui/000-tokenize-string.txt.
- **Status:** fixed in chez.py: `string_to_number` returns `False` for any non-ASCII
  string. The original's quirk is kept: a token list with `#f` in it is invalid input.

### Python tests: Tk sends a generated key to the window with the focus
- **Seen:** loop0002 iteration 16 (item 15), drive_gui.py.
- **What:** `event_generate("<Return>")` on the command line went to the input dialog
  that had just taken the focus, not to the command line. Go then read the typed problem
  and started a new one, so the run stood at codelet 0 instead of the breakpoint.
- **Status:** test-side. The driver gives the field the focus first (`focus_force`), as a
  user's click would.

### Python: a fresh venv has no setuptools, so an offline `pip install` can't build
- **Seen:** loop0002 iteration 17 (item 16), writing python/tests/test_install.py.
- **What:** since Python 3.12, `python3 -m venv` puts only pip in a new venv. `pip install
  -e python` then builds in an isolated environment and downloads setuptools, which
  fails without a network. The editable install also writes `metacat.egg-info/` into the
  source directory it installs.
- **Evidence:** `python3 -m venv /tmp/v && /tmp/v/bin/pip list` lists only pip.
- **Status:** worked around in the tests: the venvs are made with
  `--system-site-packages` (Anaconda's setuptools 75.1) and pip runs with
  `--no-build-isolation --no-index --no-deps`, on a clean copy of `python/` in a temporary
  directory, so the checkout gets no egg-info. A user with a network needs none of this.

### Two codelet totals for the goldens: 272,957 and 272,857
- **Seen:** loop0002 iteration 18 (item 17, final audit): the plan quoted 272,957 codelets
  for the 109 goldens in one place (item 10) and 272,857 in another
  (docs/python-run-times.md, item 11).
- **What:** both are right. The goldens hold 272,957 `codelet` events. The final
  codelet counts (the `end` event's `t`, and the CLI's `Codelets:` line) sum to 272,857.
  A run that suspends (99 goldens) or halts (1) stops inside its last codelet, before
  run.ss increments `*codelet-count*`, so that codelet has an event but no count. The 9
  capped runs end between codelets.
- **Evidence:** count the `"ev":"codelet"` lines of `tests/golden/*.jsonl` and sum
  each file's last `t`. The difference per run is 1 for every suspend and halt and 0 for
  every cap.
- **Status:** explained (the original's behaviour; item 11's break-inside-a-codelet test
  shows the same at 2427/2428).

### Python: `engine.load()` imported the `metacat.gui` package (port bug, fixed)
- **Seen:** loop0002 iteration 18 (item 17, final audit): a headless run with every
  engine module imported had `metacat.gui` in `sys.modules`.
- **What:** `engine.LOAD_ORDER` names metacat.ss's files, and the last one, gui.ss,
  maps to `gui`. `translated_modules()` imported `metacat.<name>` for every name that
  `find_spec` found, and `metacat.gui` is the views *package*, so its `__init__` was
  imported. The `__init__` holds only a docstring: no tkinter came with it, nothing was
  loaded or drawn, and no run changed. The static checks didn't see it, because they
  only look at each module's own import statements, and the package's `__init__` has
  none. gui.ss's translation is the view `metacat/gui/gui.py`.
- **Evidence:** `python/tests/test_gui.py::test_a_headless_run_loads_no_gui_module`
  (fails on the old `translated_modules`).
- **Status:** fixed. `translated_modules()` skips packages.

### `racket/metacat.rkt`'s header names `tests/make-dist.sh`, which doesn't exist (port doc bug)
- **Seen:** 2026-10-04, while writing the folder READMEs.
- **What:** the header comment of `racket/metacat.rkt` (line 22) says the standalone
  executable is built by `tests/make-dist.sh`. The script is `make-dist.sh` at the
  repository root.
- **Evidence:** `grep -n make-dist racket/metacat.rkt`; `ls make-dist.sh tests/make-dist.sh`.
- **Status:** open. It's a one-line comment fix. It was left alone because loop0002's gate
  freezes `racket/` (apart from README files). The READMEs give the correct path.

### Python's fast test tier still needs Chez for two freshness checks
- **Seen:** 2026-10-04, while writing `python/tests/README.md`.
- **What:** the fast tier is meant to run from the committed fixtures alone. But the
  `SOURCES` freshness tests in `test_extra_seeds.py` and `test_sgl.py` go through
  `python/oracle/capture.py`, which runs `scheme --version` (capture.py:99) to record the Chez
  version. Without Chez installed, those two tests fail.
- **Evidence:** run the fast tier with `scheme` off the `PATH`.
- **Status:** open. Documented in `python/tests/README.md`. A fix would skip these two
  tests, or read the version from the fixtures, when Chez is missing.

### pytest-qt stops the whole pytest run when no Qt binding imports
- **Seen:** loop0003 iteration 2 (item 01), writing `test_qt_tests_skip_without_pyside6`
  in python/tests/test_qt_skeleton.py.
- **What:** this machine has the pytest-qt plugin (4.5.0) installed. With PySide6 made
  unimportable, its `pytest_configure` fails before any test is collected
  (`INTERNALERROR> ... pytestqt/qt_compat.py ... _guess_qt_api`), so every test of the
  suite errors, not just the Qt ones. Blocking it needs its entry-point name,
  `-p no:pytest-qt`; `-p no:pytestqt` (the module name) is silently accepted and does
  nothing.
- **Evidence:** from python/, `python3 -m pytest -o addopts= tests/test_qt_skeleton.py`
  with `PYTHONPATH` set to a folder holding a `PySide6/__init__.py` that raises
  `ImportError`; the test above runs pytest that way, with the project's `addopts`.
- **Status:** worked around: python/pyproject.toml's pytest `addopts` has
  `-p no:pytest-qt`. The Qt tests use conftest.py's own `qapp` fixture, so they need no
  plugin, and they skip cleanly without PySide6.

### An offscreen QApplication makes later forks warn
- **Seen:** loop0003 iteration 2 (item 01), the first gate run with the Qt tests.
- **What:** `QApplication` with `QT_QPA_PLATFORM=offscreen` starts one OS thread of its own
  (`/proc/self/task` goes from 1 to 2). The session's `qapp` fixture keeps it alive, and
  `test_rules.py`, which runs after `test_qt_skeleton.py` in file order, forks
  (`golden_harness`): Python 3.12 printed 32 times "This process is multi-threaded, use
  of fork() may lead to deadlocks in the child". Nothing deadlocked, but a fork can copy
  a lock the other thread holds.
- **Evidence:** the gate log of that run (`1438 passed, 32 warnings`); without the Qt
  tests, `python3 -m pytest tests/test_rules.py` gives no warnings.
- **Status:** worked around: python/tests/conftest.py runs every `test_qt_*.py` file after
  all the others (`test_qt_tests_run_after_the_others` checks it).

### Tk 8.6 gives five colour names web values, not X11's
- **Seen:** loop0003 iteration 3 (item 02), replaying the synthetic canvas stream into a
  tkinter Canvas under Xvfb.
- **What:** Tk 8.6.13 answers `gray` and `grey` as `#808080`, `green` as `#008000`,
  `maroon` as `#800000` and `purple` as `#800080`. X11's rgb.txt, and so constants.ss's
  `*color-names*` (python/metacat/gui/colors.py), give `#bebebe`, `#00ff00`, `#b03060` and
  `#a020f0`. The other 747 names agree. Tk 8.6 adopted the web's values (TIP 403). Tk
  also reads `#abc` as `#aabbcc`, whereas X11's XParseColor gives `#a0b0c0`.
- **Evidence:** `python/tests/data/tk-display-lists.json`, synthetic stream, items 34–37;
  `winfo rgb . gray` under Xvfb.
- **Status:** explained. The panels never send these names to Tk: they pass `Rgb`s made
  by `swl-color` from constants.ss's table, which Tk receives as `#rrggbb`. The Qt canvas
  (python/metacat/qt/displaylist.py, `color_rgb`) reads colour words as Tk 8.6 does.

### Tk 8.6.13's bounding boxes, measured: the polygon outline rule
- **Seen:** loop0003 iteration 3 (item 02), the first comparison of the Qt display list with
  Tk's.
- **What:** Tk's `bbox` follows per-kind rules (an outline's bloat, one or two pixels of
  fudge, C truncation of the first point and rounding of the others). The rules were taken
  from Tk's sources and checked on 192 items. One did not match the source as remembered.
  A polygon with an outline grows by `(int)(width + 1) / 2` pixels plus 1 (0, 1, 1, 1, 2,
  2, 3 for widths 0, 1, 1.5, 2, 3, 4, 5), not by `(int)(width + 0.5)` plus 1. Also,
  `canvasx` and `canvasy` round the screen coordinate to a whole pixel before adding the
  scroll offset (`canvasy 7.5` is `8.0`), and a text item with `-fill ""` has an empty
  bbox, like a hidden one.
- **Evidence:** `python/tests/test_qt_canvas.py::test_display_list_is_tks_with_tks_metrics`,
  against `python/tests/data/tk-display-lists.json` (the slow tier regenerates it with
  `canvas_streams.py` under Xvfb).
- **Status:** explained: the Qt canvas copies what Tk does, not what its source seemed to
  say.

### Qt's metrics of a font are taller than Tk's (Xft) for the same face
- **Seen:** loop0003 iteration 3 (item 02), the Qt display lists with Qt's own fonts.
- **What:** both toolkits pick Nimbus Sans and Nimbus Roman through fontconfig, and the
  text widths agree within a pixel ("cleared" in 11-pixel Helvetica: 36 in both). The
  heights differ: Qt's `ascent + descent` is 15 for 11-pixel Helvetica, where Tk's
  linespace is 12, and 30 against 27 for 22-pixel bold italic Times. So a Qt text item's
  bbox is up to 5 pixels taller than Tk's, and its baseline sits a pixel lower.
- **Evidence:** `test_display_list_with_qt_fonts` (tolerance 6 pixels on text extents
  only); `QFontMetrics(QFont("helvetica")).ascent()` with `setPixelSize(11)` is 12, while
  Tk's `font metrics {helvetica -11}` gives `-ascent 10 -descent 2`.
- **Status:** explained and worked around (loop0003 item 03). The Tk here is Anaconda's,
  built without Xft: it draws X core fonts (the X server's Type 1 rasteriser, reading
  `/usr/share/fonts/X11/Type1`), and a core font's ascent and descent are the extents of
  its glyphs' ink, not the face's line metrics. Qt's `ascent()` is the OS/2 table's win
  ascent (1.075 em for Nimbus Sans). `metacat/qt/fonts.py` now takes the ascent and descent
  from the ink of the font's printable Latin-1 glyphs: over 252 fonts
  (`python/tests/data/tk-fonts-colors.json`) the linespace is within 3 pixels of Tk's, and
  within 1 for 79% of them, and the ascent within 1 for all
  (`test_qt_fonts.py::test_heights_are_close_to_tks`). The text
  items of the canvas streams are now within 2 pixels of Tk's. Only the pictures depend
  on it: the engine never measures text.

### Tk's font measure and Qt's horizontalAdvance part at 22-pixel bold Helvetica
- **Seen:** loop0003 iteration 4 (item 03), the widths of the reference samples.
- **What:** 97% of the 2,016 sample widths are identical in Tk and Qt, and 98% within a
  pixel. The rest are bold Helvetica at 22 pixels ("Bond builders": Tk 151, Qt 145) and
  24-point Times (2 or 3 pixels).
- **Evidence:** `python/tests/test_qt_fonts.py::test_widths_are_tks`.
- **Status:** explained: two hinters. The X server hints the Type 1 `.pfb` files, FreeType
  (in Qt) the OpenType `.otf` files of the same URW designs, and their rounded advances
  differ at a few sizes. Only pictures depend on widths.

### Qt names fontconfig families with a foundry, and resolves aliases its own way
- **Seen:** loop0003 iteration 4 (item 03), `QFontDatabase.families()`.
- **What:** Qt lists "Nimbus Sans [UKWN]", "Nimbus Sans [URW ]" and "Nimbus Sans [URW]"
  for one family, with fontconfig's foundry in brackets. The alias names fonts.ss prefers
  are not listed at all, yet `QFont("palatino")` gives P052 and `QFont("times new roman")`
  Liberation Serif, while an unknown name gives Noto Sans. Anaconda's `fc-match` (first on
  the PATH) answers KaTeX_AMS for everything; it doesn't read the system's configuration,
  which Qt and the system fontconfig do.
- **Evidence:** `python/tests/test_qt_fonts.py::test_families_are_fontconfigs`.
- **Status:** worked around: `qt/fonts.families()` strips the bracket, lower-cases, and
  adds a preferred face when fontconfig maps it onto something other than its generic
  fallback (docs/divergences.md, Python Qt GUI).

### X11 draws a wide diagonal line with a jog
- **Seen:** loop0003 iteration 3 (item 02), comparing the SGL fixture drawn by Qt with
  `docs/screenshots/panels/sgl-fixture-python.png`.
- **What:** in the "erase" cell, Tk (X server rasterisation, Xvfb) draws the 5-pixel white
  line with a one-pixel step halfway along. Qt draws it straight. Dashes also start at
  slightly different phases, and 2-pixel circles are a little rounder in Qt.
- **Evidence:** `docs/screenshots/panels/sgl-fixture-qt.png` against
  `sgl-fixture-python.png`; 97.7% of the pixels agree within 24 levels per channel.
- **Status:** not a bug: two rasterisers. Recorded in `docs/divergences.md` (Python Qt
  GUI).

### Qt paints the panes slowly while the engine runs in another thread (the GIL)
- **Seen:** 2026-10-04, loop0003 item 04, run7 with the window resized from the GUI thread
  while `headless.run_problem` ran in a worker thread.
- **What:** one `QApplication.processEvents()` took about 4 seconds, and only one of the
  planned resizes (one every 300 ms) happened in a 7-second run. The GUI thread was
  inside Qt's C++ code, painting the scenes: each `TkItem.paint` and `boundingRect` is a
  Python call, and each has to wait for the engine thread to give up the GIL (every 5 ms by
  default). With `sys.setswitchinterval(0.0002)` the GUI ran, but the run took 68 s instead
  of 7 s.
- **Evidence:** `render_qt_panes.py`'s first `--resize` version (engine in a thread);
  stacks taken with `sys._current_frames()` showed the main thread in `processEvents`.
- **Status:** fixed in loop0003 item 05 (the update below). It was open after item 04,
  which drove the engine in the GUI thread and resized at the run's `update-everything`,
  so that its test didn't depend on it.
  Item 05 has to keep the GUI thread's Python work per refresh small (fewer Python calls
  per painted item, or painting cached pictures) and measure it.
- **Update (loop0003 item 05): fixed.** What costs is a Python function that Qt calls from
  C++ while the GUI thread doesn't hold the GIL: each call waits for the engine thread to
  let go. Calls the GUI thread makes from Python keep the GIL, and so do the C++ calls
  they lead to. Measured in isolation: 500 Python-subclassed items repainted from
  `QApplication.exec()` while another thread ran a Python loop gave 1 frame in 3 s; with
  the view's `paintEvent` overridden in Python (one wait, then every item's `paint`
  finds the GIL held) it gave 64. Three changes:
  - `TkItem` is a `QGraphicsRectItem` with no Python `boundingRect` (Qt asks it of every
    changed item from C++); its rect is the bounding box;
  - `PaneView.paintEvent` enters the paint pass from Python, and the panes' letterbox
    margins are painted by Qt (`autoFillBackground`), not by a Python `paintEvent`;
  - a *paint gate* (`canvas.PAINT_GATE`): the GUI thread holds it while it syncs the
    scenes and while a view paints, and every canvas command waits for it. The engine,
    which draws constantly, then waits on a lock without the GIL, and the GUI thread's
    Python runs at full speed instead of in 5 ms turns.

  Then a second finding: **PySide6's constructors release the GIL.** `QColor(1, 2, 3)`
  300 times took 0.1 ms alone and 162 ms with a busy Python thread (QPen 471 ms, QPointF
  172 ms, QRectF 162 ms); method calls such as `setPen` or `pen.setWidth` don't release
  it. So the items record their painter calls (with their `QColor`s, `QPen`s and
  `QPolygonF`s) once, at their first paint after a change, under the gate, and replay
  them afterwards: a Workspace paint at run 7's answer went from 22 ms to 6 ms.
  Driven run 7 (`drive_qt_gui.py`): the GUI thread answers within 0.05–0.09 s (0.41 s
  before the gate), and the panes repaint about 150 times in the measured part of the
  run. Tests: `test_qt_engine.py::test_scene_items_have_no_python_bounding_rect` and
  `::test_the_panes_repaint_while_another_thread_runs_python`, which runs
  `python/tests/qt_paint_probe.py` (38–42 timer ticks and paints in 2 s; 2 ticks with
  `--without-fix`). Two more findings from the probe: a Python event filter on the
  viewport is enough by itself to cure the stall (Qt enters Python once per paint
  event), and the gate helps only a thread that draws: a thread spinning in pure Python
  still slows the GUI to 8 ticks in 2 s. The engine draws constantly.

### A pane that comes back to its size kept a clamped scroll position (port bug, fixed)
- **Seen:** 2026-10-04, loop0003 item 04, the run7 picture after 14 window resizes.
- **What:** the Commentary pane wasn't scrolled to its last line: scroll value 6992 of
  7106. Between two turns of the hosts' configure feeder the pane grew (Qt clamped the
  scroll bar to the smaller range) and shrank back to the size it had last told the panel.
  The host, copying `TkHost._configure`, skips a configure whose size equals the last
  one, so the panel never ran `reposition-vertical-scrollbar`. Tk would have sent both
  configures.
- **Evidence:** `python/tests/test_qt_panes.py::test_a_pane_that_comes_back_to_its_size_still_gets_a_configure`
  (it fails with the old size test) and
  `test_resizing_the_window_during_run7_changes_nothing`.
- **Status:** fixed: a host sends a configure when its pane changed size since the last
  one, even if it came back to the same size.

### Python's cyclic garbage collector freed a Qt widget on a worker thread: a deadlock (Qt GUI bug, fixed)
- **Seen:** loop0003, 2026-10-04/05. Iteration 7 and iteration 9 with its fix sessions
  chased it. A separate debugging agent found the cause in its own worktree.
- **What:** `tests/test_qt_panes.py::test_fonts_measure_from_several_threads_at_once` hung
  intermittently (about 1 run in 2 or 3), and only after the other Qt tests:
  - Python's *automatic* cyclic garbage collector ran at an eval breaker on one of the
    test's font-measuring worker threads, while that thread held the canvas's
    `PAINT_GATE` lock and the GIL.
  - It freed garbage left by earlier tests: `QtHost` ↔ `Pane` reference cycles (hosts
    that were never placed), whose parentless, shown `Pane` has a native `QWindow`.
  - Destroying it (`QWidget::~QWidget` → `QWindow::close` →
    `QWindowSystemInterface::flushWindowSystemEvents`) waits for the GUI thread. The GUI
    thread was in `t.join()` waiting for the workers, so neither could continue.
  - The menu tests created no garbage. They only moved a full collection into the
    measuring loop.
  - A narrower fix (handing the pane to the GUI thread from `__del__`) exposed the next
    off-thread deletion: a `QGraphicsScene` freed on a worker, then a SIGSEGV when its
    timer fired.
- **Evidence:** `ralph_loops/loop0003/hang-it07-faulthandler.log` (Python stacks); gdb's C
  stacks (`delete_garbage` ← `gc_collect_main` ← `_Py_HandlePending` under
  `QWidget::~QWidget`); `test_the_garbage_collector_runs_on_the_gui_thread_only` fails
  every time without the fix (128+ collections on a worker).
- **Status:** fixed. `metacat.qt.hosts.collect_on_gui_thread()`, called from
  `hosts.install()` and the tests' `qapp` fixture, turns off the automatic collector and
  runs `gc.collect(generation)` from a 100 ms `QTimer` on the GUI thread whenever the
  automatic thresholds would have triggered. The real app could have deadlocked the same
  way whenever a canvas command on the engine thread triggered a collection. Caveat: an
  explicit `gc.collect()` on a worker still runs there; no production code calls one.

### racket/gui: a custom `place-children` gets every child, and showing one doesn't re-place
- **Seen:** 2026-10-05, loop0003 item 10 (racket/gui/one-window.rkt's pane layout).
- **What:** three surprises with a `panel%` that overrides `place-children`:
  - `place-children`'s `info` has one entry per child, hidden children included.
    Returning places for the shown children only raises `container-redraw: result from
    place-children is not a list of length 11 (matching the input list length)`;
  - after `(send child show #f)` or `#t`, neither the show nor `reflow-container` calls
    `place-children` again: the other panes stayed where they were, and a pane shown from
    the Windows menu kept a 46×8 size at the panel's corner;
  - `reparent` shows a hidden widget: the self-watching warning, hidden by
    make-control-panel, was visible once moved into the control strip.
- **Evidence:** one-window-test.rkt's "the Coderack moved left", "EEG has room" and
  "self-watching is on" checks fail without the workarounds below.
- **Status:** worked around. The layout gives hidden children `(0 0 0 0)`; a pane's host
  calls `container-flow-modified` on the panel after showing or hiding its canvas; the
  strip restores the warning's shown state after `reparent`.

### racket/gui: a text field's `set-value` from another thread can find its editor locked
- **Seen:** 2026-10-05, loop0003 item 11's gate (fix 1): control-panel-test.rkt's "Stop in
  the middle of a run, then Go" check failed once in many runs.
- **What:** run.rktl's `go`, `break` and `quiet-break` run in the engine thread and send
  the control panel `switch-to-run-mode` / `switch-to-input-mode`, which set the command
  line's text. When the GUI thread held the text field's editor at that moment, the engine
  thread got `sequence-contract-violation: negative: method insert cannot be called, except
  in states (unlocked), args "running..." 0 0`; the engine-error handler's own
  `switch-to-input-mode` failed the same way, and the resumed run stopped at codelet 723
  instead of finishing (`'(723 1487051997)` for the expected `'(2170 4089168737)`).
- **Evidence:** the gate log of loop0003 iteration 15; the stack went through
  gui.rktl's control panel object from gui.rkt's engine thread.
- **Status:** fixed. The mode switches (and `update-current-problem`'s clearing of the
  command line, which `run-new-problem` reaches from the engine thread) now run on the
  control panel's eventspace thread, and the engine thread waits for them
  (`call-on-gui-thread`, gui.rkt). The GUI thread never waits for the engine, so this
  cannot deadlock; the order of the engine's work is unchanged, and no RNG draws move.

## 🔗 Hidden couplings

### The graphics and rules.ss tell strings from symbols
- **Seen:** loop0002 iteration 4 (item 03), the grep that docs/python-translation-plan.md
  asked item 03 to repeat.
- **What:** the plan makes symbols and Scheme strings both Python `str`, because the model
  never tells them apart. It does in a few places. `string?` picks strings out in
  sgl-interpreter.ss:386, 398 and 432, general-graphics.ss:418, fonts.ss:93 and gui.ss:363
  (text versus symbolic arguments, colour names versus colour objects, font faces).
  rules.ss:269 runs `(filter-out symbol? (flatten rule-clauses))`, so a string in a
  rule clause would survive where a symbol is dropped. `symbol?` at answers.ss:140,
  themes.ss:595, trace.ss:83, justify.ss:242/245 and gui.ss:743/749 only tells a symbol
  from a list or a number, which `str` handles. There is still no `eq?` on a string
  literal, and `~s` is only used in run.ss's `no-prompt` error and fonts.ss's debugging
  output.
- **Status:** open. The items that translate those files must keep the distinction there:
  `chez.String` for the strings those tests see, or an explicit tag.
- **Update (loop0002 iteration 14, item 13):** sgl-interpreter.ss and fonts.ss keep it.
  `metacat/gui/sgl.py` tests `isinstance(x, chez.String)` where the original has
  `string?`, so a text, a colour name and an `erase` colour must be `chez.String`, and a
  colour given as a symbol reaches Tk unconverted, as in the original (the stream
  fixture has one: `(foreground-color red)` → `-fill red`). Colour names in
  `gui/colors.py` are `chez.String`, because `swl-color` looks them up with `assoc`
  (`equal?`, which never equates a string with a symbol). A mutant that let `lookup`
  convert symbols too fails the stream test.

### Model state that only a window can provide
- **Seen:** iteration 2 (item 01).
- **What:**
  - Every codelet's `run` sends `set-last-codelet-type` to a coderack window that only
    coderack-graphics.ss installs.
  - Memory descriptions call `get-normal-icon-pexp`, which only the Memory window provides.
  - `group-graphics 'erase` is called even with graphics off (groups.ss:727).
  - The reverse also happens: graphics-gated code sets model state, e.g.
    `set-shrunk-singleton?` on groups.
- **Status:** worked around in the oracle with null windows that reject unknown messages.
  It's open for the GUI items, which must show that a GUI run equals a headless run.

### The Memory outlives a run
- **Seen:** iteration 11 (item 10), running several goldens in one Racket process.
- **What:** `init-mcat` clears the Memory's activations and highlights, not its answers
  and snags, so a second problem in the same session starts with the first problem's
  answers in the Memory (that is the point of the Memory in the GUI: answers from earlier
  runs). Running the goldens one after another in one engine gave different traces from
  the second run on (the first difference being a stale codelet count). The oracle runs
  every golden in a fresh Chez process.
- **Evidence:** racket/tests/golden-test.rkt gives each run a fresh engine (a new
  namespace); without that, only the first run matches.
- **Status:** explained; not a bug. Any batch runner (item 11's CLI, tests) must start
  each golden run with a fresh engine, or clear the Memory, to reproduce the oracle.
- **Update (loop0002 iteration 11, item 10):** the Python golden runner
  (python/tests/golden_harness.py) starts a fresh Python process, loads the engine there
  once, and runs each golden in a fresh fork of it (`maxtasksperchild=1`), so every run
  starts from a freshly loaded engine, as in the oracle. The test session's own engine,
  which other test files change and restore, is never used for a golden.

### Trace events and the EEG reach into graphics files
- **Seen:** iteration 11 (item 10).
- **What:**
  - Every group event's print name, which is in the trace (`"name":"[a-b-c]"`), is made
    by `group-event-pexp-text-string` from trace-graphics.ss.
  - The Temporal Trace's `display-workspace-state` sets `*fg-color*` and
    `%bridge-label-background-color%`, globals of general-graphics.ss and constants.ss.
  - workspace.ss's `initialize` sends `initialize` to `*EEG*`, an object defined only in
    eeg-graphics.ss; headless runs never read it.
- **Evidence:** `grep -n "group-event-pexp-text-string\|\*EEG\*" chez_scheme/original/*.ss`.
- **Status:** worked around: engine/pending.rktl has an early verbatim copy of
  `group-event-pexp-text-string` (pure); racket/tests/golden-harness.rkt gives a null
  `*EEG*`. The GUI items move them back.
- **Update (iteration 15, item 14):** the engine now has these parts of the graphics
  files, verbatim: `group-event-pexp-text-string` (engine/trace-graphics.rktl),
  `relation-name` (engine/theme-graphics.rktl, used by trace.ss's `print-pattern`) and
  the EEG object with `%EEG-table%` (engine/eeg-graphics.rktl). The early copies and the
  null `*EEG*` are gone: headless runs use the real EEG, as the oracle does. The EEG
  records values only when `%workspace-graphics%` is on, and the goldens with views
  attached show that recording changes nothing.
- **Update (loop0002 iteration 11, item 10):** the Python port has the same couplings.
  `metacat/trace_graphics.py` holds only `group-event-pexp-text-string`,
  `metacat/theme_graphics.py` only `relation-name`, and `metacat/general_graphics.py` only
  `find-next-space-position`, each translated verbatim (the panels item adds the rest of
  each file, as group_graphics.py already does). The golden harness gives `*EEG*` a null
  object accepting `initialize`.

### Urgencies are exact rationals
- **Seen:** iteration 3 (item 02).
- **What:** 3601 of the golden codelet lines carry urgencies like `102/5`, from
  `(* (% conceptual-depth) activation)`. One stray flonum in the port would change the
  codelet choices.
- **Status:** explained. The port keeps exact arithmetic; the trace writes `"n/d"`.

### `*temperature-clamped?*` is never defined
- **Seen:** iteration 7 (item 06), compiling formulas.ss.
- **What:** formulas.ss's `update-temperature` reads `*temperature-clamped?*`, and
  answers.ss and trace.ss `set!` it, but no file defines it. It exists only because
  run.ss's `init-mcat` does `(set! *temperature-clamped?* #f)`, which Chez's top level
  allows for an unbound variable. Calling `update-temperature` before the first
  `init-mcat` would raise "variable not bound".
- **Status:** worked around: engine/pending.rktl defines it as `#f` (porting-notes.md,
  item 06); the run.ss item should keep a definition.
- **Update (iteration 12, item 11):** run.ss does the same with
  `*initial-slipnode-unclamp-time*`: `run-mcat` compares the codelet count with it,
  `init-mcat` and `clamp-initial-slipnodes` `set!` it, and no file defines it. Racket
  rejected run.rktl at compile time ("unbound identifier"). engine/pending.rktl now
  defines both, as never-defined names (not pending on any item).
- **Update (iteration 18, item 17):** still so. engine/pending.rktl now holds only the
  original's never-defined names (these two, `same-direction?`,
  `complement-codelet-pattern`); every file is ported.

### Fonts the model reads but nothing defines
- **Seen:** iteration 8 (item 07), compiling groups.ss.
- **What:** groups.ss's `set-graphics-parameters` reads `%group-letter-category-font%`
  and `%relevant-group-length-font%`, which exist only once the Workspace window's
  initialisation `set!`s them (workspace-graphics.ss). Like `*temperature-clamped?*`,
  they rely on Chez's top level accepting `set!` of an unbound variable.
- **Status:** worked around: engine/pending.rktl defines them as `#f`; the Workspace
  panel item must define them.
- **Update (iteration 18, item 17):** since item 13 they are view globals,
  `#f` in engine/view-globals.rktl until views.rkt's workspace-graphics.rktl `set!`s
  them through `set-global!`.

### Concept mappings are only made through bridges
- **Seen:** iteration 8 (item 07).
- **What:** bonds.ss and groups.ss make concept mappings only in
  `get-incompatible-bridge`, which returns early without a bridge. With bridge codelets
  disabled, a run never makes one, so the codelet harness cannot exercise
  concept-mappings.ss; its battery tests the mappings directly.
- **Status:** explained. Since item 08, bridges make them in runs, and
  tests/diff/bridge-battery.scm checks them (`new-cms` events, each bridge's mappings).

### Bridges call themes.ss on every bridge
- **Seen:** iteration 9 (item 08).
- **What:** although bridges.ss loads before themes.ss, every bridge calls themes.ss's
  `bridge-type->theme-type` when it is made, `bridge-theme-compatibility-sigmoid` (via
  `get-thematic-compatibility`) whenever its strength is updated, and bridge-builder's
  `boost-themespace-activations` calls `descriptions-affect-themespace?` and messages
  `*themespace*` and `*themespace-window*` for every bridge built (the window message
  is not gated by `%workspace-graphics%`; the oracle's null window absorbs it). With no
  active theme the strength only depends on `(weighted-average '() '())` = 0 and
  sigmoid(0) = 0. Building a bridge also calls trace.ss's
  `monitor-new-concept-mappings`.
- **Evidence:** bridges.ss lines 42, 270–313, 1352, 1416; tests/diff/bridge-battery.scm
  records the boost calls (`add-theme`, `update-dominant-themes`, `themespace-window
  update-graphics`) and the monitor calls (`new-cms`).
- **Status:** worked around: engine/pending.rktl has early verbatim copies of the four
  pure themes.ss helpers (item 10 moves them back); `monitor-new-concept-mappings` is a
  settable stand-in until trace.ss is ported.
- **Update (iteration 18, item 17):** done in item 10: the helpers are in
  engine/themes.rktl and `monitor-new-concept-mappings` in engine/trace.rktl, the
  original's own definitions; pending.rktl has neither.

- **Update (loop0002 iteration 9, Python item 08):** the Python port meets the same
  coupling. Until themes.py and trace.py exist, python/tests/codelet_harness.py's
  `STAND_INS` gives `metacat.themes` translated copies of `bridge-type->theme-type`,
  `descriptions-affect-themespace?` (with `ignore-descriptions?`) and
  `bridge-theme-compatibility-sigmoid`, and `metacat.trace` a recording
  `monitor-new-concept-mappings`. The headless driver (items 10–11) needs the real ones.
- **Update (loop0002 iteration 11, Python item 10):** themes.py and trace.py define them
  now, and the stand-in copies are gone from codelet_harness.py. The bridge battery's fake
  Themespace and recording monitors stay: they are the battery's own fakes.

### Dead code in bridges.ss
- **Seen:** iteration 9 (item 08).
- **What:** `propose-singleton-group` and `try-to-propose-singleton-group` are never
  called (the comment in `build-bridge` describing singleton-group proposals has no code
  under it). `calculate-external-strength` computes `(round (* (min 100 total-support)))`,
  a one-argument `*`, harmless.
- **Status:** not a bug; ported verbatim.

### A rule's intrinsic quality is computed and never read
- **Seen:** loop0002 iteration 10 (item 09), a surviving mutant.
- **What:** `set-quality-values` sets `intrinsic-quality` from
  `compute-rule-intrinsic-quality` (both marked "temporary" in rules.ss), and the rule
  answers `get-intrinsic-quality`, but no file sends that message. So the value can't
  affect a run, and rule-battery.scm can't see it: a mutant of its cohesion factor
  (`exp(5(u − 1))` → `exp(4(u − 1))`) passed the whole battery.
- **Evidence:** `grep -n "get-intrinsic-quality" chez_scheme/original/*.ss` (only the
  definition); `python/oracle/batteries/rule-extra-battery.scm`'s `quality-*` tests read it
  directly, and they kill the mutant.
- **Status:** not a bug (dead value); ported verbatim.

### A horizontal bridge's internal-coherence factor never shows in the batteries
- **Seen:** loop0002 iteration 9 (Python item 08), mutation testing bridges.py.
- **What:** `calculate-internal-strength` multiplies by 2.5 when a bridge is internally
  coherent, then caps at 100. Coherence needs at least two supporting relevant
  distinguishing mappings (factor 1.2 or more), so a coherent bridge stays under the cap
  only if its mappings average under about 33, i.e. with low-association slippages and
  no identity. For horizontal bridges (initial to modified string) that never happened:
  changing the factor to 2.0 left every bridge-battery trace identical, and a search over
  every problem and seed of tests/problems.txt at seven points of the run (100 to 1500
  codelets) found no fresh coherent horizontal bridge under 100. Vertical ones exist:
  abc abd glz, seed 2, 1500 codelets, a–z with first=>last and lmost=>rmost (strengths 27
  and 23, internal strength 75). python/oracle/batteries/bridge-extra-battery.scm pins it.
- **Status:** explained; the horizontal factor stays unpinned by tests (faithful by
  reading).

### Rules and answers lean on later files and on a REPL abbreviation
- **Seen:** iteration 10 (item 09).
- **What:** rules.ss and answers.ss load before themes.ss, trace.ss and the graphics
  files, yet the model reaches into them on every rule and answer:
  - `make-rule` calls `transcribe-to-english`, which calls general-graphics.ss's
    `find-next-space-position`;
  - `set-translated-rule-information` calls the Workspace's `get-real-object`, which
    calls trace.ss's `equivalent-workspace-objects?`;
  - answers.ss's theme phrases compare a theme's relation with `diff`. That is one of
    themes.ss's REPL abbreviations (`top`, `len`, `iden`, … for typing commands such as
    `(set-themes top lcat same 100)`), and it is `#f`, the "different" relation;
  - the slippage log's `get-highlight-color` reads four constants.ss colours.
  Also, `process-snag` clamps the temperature (`*temperature-clamped?*`), and only the
  Trace's `undo-snag-condition` (or a clamp's undo) unclamps it. Without a working Trace,
  every run after its first snag stays at temperature 100 and never posts an
  answer-finder.
- **Evidence:** rules.ss line 1739, workspace.ss line 407 (`get-real-object`),
  answers.ss lines 373–394 and 902 (`theme-abstractness`), trace.ss lines 188–196; `tests/diff/rule-battery.scm` (its fake Trace ends
  snag periods for this reason).
- **Status:** worked around. engine/pending.rktl has early verbatim copies of the pure
  definitions (`find-next-space-position`, `equivalent-workspace-objects?`, `diff`),
  which items 10 and 12 move back; the colours are `#f` stand-ins until the GUI items.
- **Update (loop0002 iteration 10, item 09):** the Python port reads all of them through
  the package at call time (`_metacat.general_graphics.find_next_space_position`,
  `_metacat.trace.equivalent_workspace_objects_p`, `_metacat.themes.diff`), and
  `process-snag` sets `_metacat.run.g_temperature_clamped_p`. Until those modules exist,
  python/tests/test_rules.py supplies verbatim test-side copies as stand-ins. The oracle has
  the real definitions loaded, so these are the real ones there. Items 10 and 11 replace
  them.
- **Update (iteration 18, item 17):** done: `diff` and `equivalent-workspace-objects?`
  are in engine/themes.rktl and engine/trace.rktl (item 10), `find-next-space-position`
  in engine/general-graphics.rktl (item 13), and the colours are view globals
  (engine/view-globals.rktl), installed by racket/gui/views.rkt.
- **Update (loop0002 iteration 11, Python item 10):** done in Python: `diff` is
  themes.py's, `equivalent-workspace-objects?` trace.py's, and `find-next-space-position`
  general_graphics.py's (an early partial module). test_rules.py no longer has copies.

### Rules are never removed
- **Seen:** iteration 10 (item 09).
- **What:** the Workspace understands `delete-rule`, but nothing sends it. The breaker
  leaves rules out of its candidates (`filter-out rule?`, breakers.ss line 26), so every
  rule built during a run stays in the Workspace until the next problem.
- **Evidence:** `grep -n "'delete-rule" chez_scheme/original/*.ss` finds nothing;
  `tests/diff/rule-battery.scm`'s traces contain no broken rule in 109 runs.
- **Status:** not a bug, apparently by design; ported verbatim.

### Erasing draws: the canvas only grows until a `clear`
- **Seen:** iteration 13 (item 12), porting sgl-interpreter.ss.
- **What:** `(erase color pexp)` and `erase!` don't delete anything: they draw pexp
  again in the erase colour, as new Tk items tagged `eraser`. A window that erases and
  redraws (the Workspace's structures, the Coderack's bars) keeps adding items until the
  next `(clear)` or `delete`. The tag `eraser` also replaces the caller's tag.
- **Evidence:** tests/diff/sgl-battery.scm's `erase-*` tests (all items carry
  `'eraser`); racket/gui/sgl.rkt's viewport keeps them in its display list.
- **Status:** not a bug; ported verbatim. If long runs in the GUI get slow, this is the
  first place to look.

### Workspace graphics write graphics state into model objects, but draw no random numbers
- **Seen:** iteration 14 (item 13).
- **What:** with `%workspace-graphics%` on, the model calls the graphics files directly.
  Groups compute their graphics coordinates (`set-graphics-parameters`), and bridges
  number their labels (`new-bridge-label-number`) and set concept-mapping pexps.
  Images store group pexps (images.ss line 207). Answers lay out the translated string
  (`init-translated-string-graphics`). The Memory makes answer-description pexps
  through the Trace (`make-answer-description-pexp`, which draws on the Workspace window
  in cache mode and takes the cached pexp back). The EEG records values each update.
  Groups and bridges also keep a `drawn?` flag that only the graphics set. A run could
  diverge if any of this drew from the generator or changed what the model reads.
- **Evidence:** racket/tests/views-test.rkt (workspace-view-test.rkt in item 13): all 109
  golden runs give identical traces with the Workspace window attached, and a mutation that makes `bridge-graphics`
  draw one random number makes them differ.
- **Status:** explained: none of it draws or feeds back into the model's choices. The
  ported graphics are verbatim, so this also holds for the original.
- **Update (iteration 15, item 14):** the same holds with every window attached and every
  graphics switch on. The Slipnet, Coderack, Temperature and EEG windows redraw at every
  update. The Coderack window recomputes the selection probabilities, and the codelet
  types keep its slot pexps. Themes keep their panel. Trace events and Memory answers
  keep their icons and bounding boxes. All 109 traces stay identical, and a mutation
  that makes the Slipnet window's `update-graphics` draw one random number changes
  all 109.

### `update-rule-pexps!` mutates pexps shared with the rules
- **Seen:** iteration 14 (item 13), porting rule-graphics.ss.
- **What:** on a resize, the Workspace window recomputes the rule pexps inside each
  answer and snag description in the Memory, in place with `set-car!`. Those
  `(rule ...)` cells are shared with the rules' own pexps, which the window recomputes
  just before with `initialize-rule-graphics`, so in the original both end up new.
- **Evidence:** rule-graphics.ss line 77; workspace-graphics.ss `update-rule-pexps`.
- **Status:** worked around. Racket's pairs are immutable, so the port's
  `update-rule-pexps!` returns an updated copy and the window stores it in the
  description. tests/diff/graphics-battery.scm checks the result against the
  original's mutated pexp.
- **Update (loop0002, the graphics engine):** Python lists are mutable, so
  python/metacat/rule_graphics.py's `update_rule_pexps_bang` updates in place as the
  original does (and returns the pexp, so a caller written after the Racket port also
  works); test_graphics.py's `test_update_rule_pexps_is_in_place` checks it.

### One resize queue for every window
- **Seen:** iteration 16 (item 15), at setup.
- **What:** general-graphics.ss's resize handler drops a waiting resize from the single
  `*resize-message-queue*` before queueing its own, whichever window the waiting one
  was for. When several windows get a configure at once, only the last one redraws.
  Under Tk this happened only during user drags. In the port, every scrolling window
  got one at startup as its scrollbars appeared, and the Commentary kept its creation
  width and scroll position.
- **Evidence:** eprintf in the handler and listener showed 5 handler calls and 2 thunks.
- **Status:** worked around: scrollbars are shown before the windows become resizable,
  so the frames grow around them and the windows keep their sizes. The original's
  queue is unchanged.
- **Update (loop0003 item 04, the Qt GUI):** in one window every pane gets a new size at
  once: at start, and whenever the window or a splitter changes. `python/metacat/qt/hosts.py`
  keeps each pane's latest size and sends the configures one at a time, each only when
  the resize queue is empty. Every panel redraws (`test_every_window_resized_at_once_redraws`).
  At start this takes about 3 seconds, one panel per 250 ms pause of the listener. The
  original's queue is still unchanged.

### Verbose mode reached an unregistered `format-slipnode` (port bug, fixed)
- **Seen:** iteration 18 (item 17), auditing porting-notes.md against the code.
- **What:** utilities.ss's `reveal-obj` names slipnodes with rules.ss's
  `format-slipnode`. utilities.rkt is a module loaded before the engine, so it looks the
  name up with `(top-level-value 'format-slipnode)`. Item 03's notes said the rules port
  must register it, and item 09 didn't. The only caller is jootsing.ss, inside a
  `vprintf`: `(reveal entry)`, evaluated only when `%verbose%` is on. So the 109 goldens,
  which run with verbose mode off, never reached it, but a jootser in the GUI with
  Options > Verbose mode on (or in verbose step mode) would have raised "not a top-level
  value" in the port, where the original prints `(<stringpos> <identity>) entry: ...`.
  No test had ever turned verbose mode on.
- **Evidence:** `racket racket/cli.rkt a b z --seed 1 --max-codelets 1000 --keep-going
  --verbose` against the oracle's run.ss with the same arguments (racket/tests/cli-test.rkt);
  the `reveal-slipnodes` test in tests/diff/slipnet-battery.scm. Both failed before the
  fix.
- **Status:** explained and fixed: racket/engine.rkt registers `format-slipnode` after
  including rules.rktl (marked `port:`). Verbose output of all 109 golden runs (1.55
  million lines; 10 runs reach the `reveal` line) is now byte-identical to the oracle's.
  Both run.ss and cli.rkt got a `--verbose` option for this.

### racket/draw imports a racket/gui module
- **Seen:** iteration 18 (item 17), writing racket/tests/no-gui-test.rkt.
- **What:** walking the transitive imports of racket/gui/sgl.rkt finds
  `racket/gui/dynamic`, imported by racket/draw's PostScript dc. It is a small module in
  the base collection that only asks whether racket/gui is loaded; it does not load it.
- **Evidence:** racket/tests/no-gui-test.rkt (it exempts that one module).
- **Status:** not a bug. The engine and the headless driver reach neither racket/gui nor
  racket/draw; the views modules reach racket/draw and, through it, only that module.

### Two threads measuring text on the one hidden canvas
- **Seen:** 2026-10-04, loop0003 item 04: one run in six of
  `test_resizing_the_window_during_run7_changes_nothing` crashed with `IndexError: string
  index out of range` in `gui/fonts.py`'s `get_pixel_size` (the Themes' garbage-collect
  redrawing a panel).
- **What:** fonts.ss's `get-pixel-size` measures a string in three canvas commands on the
  single `*hidden-canvas*`: `create text`, `bbox` of the new item, `delete all`. When the
  engine and the resize listener measure at once, one thread's `delete all` can come
  between the other's `create` and `bbox`. The `bbox` of a deleted item is empty. The
  original has the same three commands, and SWL's threads are preemptive, so a resize
  during a run could in principle do the same there. The tkinter GUI shares it too.
- **Evidence:** `python/tests/test_qt_panes.py::test_fonts_measure_from_several_threads_at_once`
  (four threads, a 1 µs switch interval) fails every time without the workaround.
- **Status:** worked around in the Qt GUI: `metacat/qt/canvas.py`'s `HiddenCanvas` keeps
  one display list per thread, so each thread's items are its own. The measurements are
  the same. fonts.ss's code and the tkinter GUI are unchanged.

### Two errors within 700 ms leave the first one on the control panel (port bug)
- **Seen:** 2026-10-04, loop0003 item 06: `drive_qt_menus.py` chose Clamp theme pattern
  and then a codelet clamp with no current problem; the info label kept "No current
  problem!" for good.
- **What:** gui.ss's `display-error` saves the label's text, shows the error, `(pause
  700)` in the GUI thread, and puts the saved text back. The pause blocks the GUI, so a
  second error can't come before the first is gone. gui.py (tkinter) and the Qt panel
  replace the pause with a 700 ms timer, so that the GUI keeps answering. Then a second
  error within 700 ms saves the first error's text as "the message", and puts it back
  after the first timer has restored the right one.
- **Evidence:** the `theme-edit` scenario of `python/tests/drive_qt_menus.py` (it failed
  at "the message comes back" before the fix).
- **Status:** fixed in the Qt GUI: `metacat/qt/controls.py`'s `display-error` restores the
  panel's last `display`ed message (`info_text`), not the label's text. The tkinter GUI
  (`metacat/gui/gui.py`) still has the bug; it is not this loop's to change.

### A Qt dialog collected by Python in the engine thread crashes the process
- **Seen:** 2026-10-04, loop0003 item 06: `drive_qt_menus.py` died with a segmentation
  fault in the engine thread (`trace.py`'s `initialize`, during a new problem's
  `init-mcat`) after the Help window had been closed. With `WA_DeleteOnClose`, Qt
  instead printed "shared QObject was deleted directly" and glibc "corrupted
  double-linked list".
- **What:** the Help window and the Confirm dialogs hold closures that refer back to
  them (signal connections), so when the dialog closes and its holder drops it, it is
  cyclic garbage. Python's cycle collector runs in whichever thread allocates at the
  time, here the engine thread, and deleting a `QWidget` off the GUI thread is fatal.
  Dropping the last reference inside the dialog's own `closeEvent` deletes it in the
  middle of its event, which is the other message.
- **Evidence:** `python3 -X faulthandler python/tests/drive_qt_menus.py OUT` before the
  fix (the engine thread's stack at the crash).
- **Status:** worked around: `controls.SwlDialog.closeEvent` calls `deleteLater()` (the
  GUI thread's event loop deletes the C++ object, so the collector later finds an empty
  wrapper) and keeps the dialog in a list until the loop's next turn.

### Qt widgets freed by the cycle collector in a worker thread (test hang)
- **Seen:** 2026-10-05, loop0003 item 06, fix attempt 3: the gate timed out after 30
  min in `test_qt_panes.py::test_fonts_measure_from_several_threads_at_once` when the
  whole Python suite ran (the file alone, or menus + panes, passed).
- **What:** the in-process `MainWindow` tests of `test_qt_panes.py` close their windows
  but leave the fake hosts as cyclic garbage (`QtHost` <-> `Pane`, with 11 `PaneView`s
  and `QGraphicsScene`s). Whether the cycle collector runs before the threaded test
  depends on how much the earlier tests allocated. When it ran inside one of that test's
  four measuring threads, while that thread held the canvas paint gate, it destroyed the
  widgets off the GUI thread and blocked in a futex; the three other threads waited on
  the paint gate and the main thread in `join`. faulthandler showed "Garbage-collecting"
  and no thread running.
- **Evidence:** a probe plugin (`gc.set_debug(gc.DEBUG_SAVEALL); gc.collect()` before
  that test) listed the garbage; `bash python/run-tests.sh --qt -o faulthandler_timeout=150`
  hung there before the fix.
- **Status:** worked around in the tests: an autouse fixture in `python/tests/conftest.py`
  processes events and runs `gc.collect()` on the GUI thread after every `test_qt_*` test,
  so no Qt garbage is left for another thread's collector. The program itself keeps its
  hosts for its whole life, and its dialogs use `deleteLater` (previous entry).
- **Update (loop0003 item 11, the final audit):** the root cause was found later and fixed
  in the program, not only in the tests: see "Python's cyclic garbage collector freed a Qt
  widget on a worker thread: a deadlock" above (`hosts.collect_on_gui_thread()`). The
  conftest fixture stays as a second line of defence; with the automatic collector off it
  is the only place where the tests' Qt garbage is collected between tests.

### Collecting fake `QtHost`s one after another crashes the cycle collector
- **Seen:** 2026-10-05, loop0003 item 07, while writing `python/tests/test_qt_clicks.py`:
  a segfault ("Garbage-collecting") in the conftest's `gc.collect()` after the fourth
  test, each of which made, showed and dropped a fake `QtHost`.
- **What:** a standalone script that makes a `QtHost` with a fake viewport, shows its pane,
  drops it, processes events and calls `gc.collect()`, three times in a row, segfaults in
  the collector in its second or third round (one round is fine). It happens with the
  committed `hosts.py` too, so it is not the mouse code. Not explained; the resize feeder
  holds a host until its timer runs, so the order in which the collector frees a pane, its
  `PaneView` and the canvas's `QGraphicsScene` may differ between rounds.
- **Evidence:** the steps above, with `QT_QPA_PLATFORM=offscreen`; `test_qt_panes.py`
  keeps its hosts alive by accident (they live until the end of the session).
- **Status:** worked around in the tests: `test_qt_clicks.py` keeps one host per
  scrolling kind for the whole session. The program never drops a host.

### `QTest.mouseDClick` on a widget sends no press, only the double click
- **Seen:** 2026-10-05, loop0003 item 07.
- **What:** on a `QWidget`, `QTest.mouseDClick` delivers one `MouseButtonDblClick` and
  nothing else, and two `QTest.mouseClick`s in a row never make a double click. On the
  widget's `QWindow` it delivers what a real mouse does: press, release, double click,
  release. Tk delivers a second `<ButtonPress>` for the second click of a double click.
- **Evidence:** `test_a_double_click_is_two_presses_as_in_tk`; with widget-level events,
  the clicks scenario's double click on the Memory selected the answer once, not twice.
- **Status:** explained. The panes treat `mouseDoubleClickEvent` as a press, and the
  tests send their clicks through the window.

### A hidden pane still gets a configure (Qt GUI)
- **Seen:** 2026-10-05, loop0003 item 11 (the final audit): `drive_qt_audit.py` read the
  hidden EEG's size as 640×26, where the tkinter inventory has 900×120.
- **What:** the EEG pane is hidden from the start, but the splitter lays it out once
  before it is hidden, and `QtHost.pane_resized` queues a configure like any other pane's.
  When the window becomes resizable, the feeder delivers it, and the EEG panel redraws at
  that size while nobody sees it. In the tkinter GUI the hidden EEG window keeps its
  default size until it is shown. Showing the pane resizes it and sends a new configure,
  so the EEG draws at its real size (`test_qt_layout.py::test_the_eeg_doubles_the_bottom_row`
  and the run test with every pane shown).
- **Evidence:** a probe that wraps `QtHost.deliver_configure` in `drive_qt_menus.py`'s
  window: one delivery to the EEG, `((640, 26), visible False)`, with the pane then at
  1920×40.
- **Status:** not a bug: views never change a run (the goldens pass with the EEG hidden
  and shown), and the cost is one redraw of a hidden panel at start. A
  `deliver_configure` that skipped hidden panes would have to deliver the size when the
  pane is shown; it was not worth the change at the end of the loop.

## 🛸 UFO sightings

### The Python port's Coderack labels lose their `i`s and `l`s under Xvfb
- **Seen:** 2026-10-03/04, in every Python screenshot taken under Xvfb (loop0002 items
  14–16, and the README screenshots of 2026-10-04).
- **What:** the Coderack window's small codelet-type labels drop thin letters. "Bond
  builders" shows as "Bond bu ders", "Bond evaluators" as "Bond eva uators", "Whole-string"
  as "Who e-str ng", "Description" as "Descr pt on". The Racket port's screenshots of the same
  run show every letter. The font request is a faithful copy of the original's:
  `(make-mfont sans-serif (- desired-type-height) '(normal))` (coderack-graphics.ss:31),
  a sans-serif font only a few pixels tall, given as a negative (pixel) size.
- **Evidence:** `docs/screenshots/panels/coderack-run7-answer.png`,
  `docs/screenshots/python-run7-wyz.png` and `python-ijk-clamp.png`, against
  `docs/screenshots/run7-wyz.png` (Racket).
- **Status:** explained (loop0003 item 03). The default Coderack window is 598 pixels high,
  so the labels are `round(14/1000 × 598)` = 8 pixels. The Tk that tkinter loads here
  (Anaconda's `libtk8.6.so`) is built without Xft: `ldd` shows libX11 but no libXft or
  fontconfig, `font actual {helvetica -11}` names the core font family `nimbus sans l`, and
  the X server rasterises the Type 1 files in `/usr/share/fonts/X11/Type1` as 1-bit
  bitmaps. At 8, 9 and 10 pixels it gives the one-pixel stems of `i` and `l` no pixels
  ("illil" shows three strokes at 10 pixels); at 7 and 11 they survive. So it is the font
  path, not Xvfb as such: a real X screen with the same Tk would do the same, and a Tk
  built with Xft would not. The Racket port (Cairo/Pango) and the Qt GUI (FreeType,
  antialiased) draw every letter:
  `docs/screenshots/panels/small-text-tk.png` against `small-text-qt.png`
  (`python/tests/render_small_text.py`), and
  `python/tests/test_qt_fonts.py::test_tiny_coderack_labels_keep_their_thin_letters`
  (5 to 11 pixels). The tkinter GUI is left as it is.

### `drive_qt_layout.py save` exits 1 with no output, now and then
- **Seen:** 2026-10-05, in the loop0003 gate after item 09 (`test_qt_layout.py`, the
  `saved` fixture: `save failed:` with empty stdout and stderr). It did not happen in 18
  runs of the scenario on its own, 12 of them in parallel.
- **What:** the `save` and `restore` scenarios end with `on_main(WINDOW.close)`. Closing
  the last window ends `QAPP.exec()` (Qt's quit-on-last-window-closed), and the main thread
  then called `os._exit(STATUS[0])` right away. When it got there before the driver thread
  had finished (setting `STATUS[0] = 0` and printing the JSON line), the process exited
  with the initial status 1 and printed nothing.
- **Status:** fixed in the test driver. After `QAPP.exec()` returns, the main thread now
  joins the driver thread, which ends the process itself; the watchdog still bounds the
  wait. The GUI itself is not affected.
