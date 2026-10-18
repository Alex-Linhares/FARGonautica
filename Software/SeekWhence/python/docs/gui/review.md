# The GUI reviewed against its design (loop0004, item 19)

The final review of `seekwhence/gui/` against `ralph_loops/loop0004/PANES.md`,
`ENGINE-API.md` and `ANSWERS.md` (which overrides both), done on 2026-10-12 on the code as
items 00 to 18 left it. Every departure from PANES.md, and every place the GUI follows the
Numbo GUI (`Software/Numbo/python/`, the precedent for every convention, ANSWERS.md 7)
instead, is listed with the `G-NNN` entry of `GUI-DECISIONS.md` that records it. The
checks the item asked for follow: a sample of citations re-read on the page images, the
engine's diff since the loop's baseline, determinism on five more seeds per appendix run,
the whitelist of engine methods, the long functions and the test suite's time. The open
questions and the weakest parts are in `ralph_loops/loop0004/PROGRESS.md`, "For the author".

## 1. Pane by pane

Each table has what PANES.md (or ANSWERS.md) says, what the GUI does, and the decision.
Where a row says "as designed" nothing departs; the rows are the departures and the places
Numbo is followed. Everything PANES.md lists under **Shows** and **Fed by** that is not in a
table is built as written.

### P1. Sequence and input

| Design | The GUI | Decision |
|---|---|---|
| Ghost cells show the predicted continuation after the last term | The whole block of `guess_block` is shown as ghost cells aligned by position, the hits beyond the terms dashed, so the prediction and the terms can be compared cell by cell | G-014 |
| The `show` buttons in this pane write into the console | The four buttons live in the console pane (P8) beside its command line; with an interactive run live a click is a session input, otherwise it prints at the cursor | G-017, G-019 |
| Click a cell to highlight its glint, its gnoth, and seek to the term's entry | The click highlights and selects the glint in the cytoplasm; the gnoth is not selected and the timeline does not seek | G-014, G-025, **G-062** (left out) |
| The guess panel "appears when the engine calls the answerer" | The panel is always in the pane; its buttons are enabled while a guess waits, through the `answerer` hook on the worker | G-008 |
| Terms typed in the box | An interactive run reads its console lines from a `TermFeed` stream; several words queue one per prompt | G-015 |

### P2. Cytoplasm

| Design | The GUI | Decision |
|---|---|---|
| Animation "about 150 ms, scaled by speed; no animation at max" | A flash lasts `FLASH` = 6 events and a fade `FADE` = 24 events, counted in events, not time, so a replay and a seek show the same picture at any speed | G-021 |
| Gloms as nested hulls, "one level per nesting depth" (Figure 1 draws a glom as a circle joined by lines to its members) | Nested rounded hulls above the baseline by nesting depth, as PANES.md; Figure 1's circle-and-lines convention recorded and not followed | G-024 |
| Pseudo-gloms "drawn only when the top-down layer is on"; section 3: hidden by default | **ANSWERS.md 10.4 overrides**: shown by default in grey, dotted; the toggle stays; legend "pseudo-glom (grey)" | G-053 |
| Proposals removed by `glom`, `glom-failed` or `glomtester-failed` | Also keyed by task number and resolved when the task is chosen; the rack is the authority at every snapshot, so a silent rejection, a cleaning or a stale task clears the marker at the next task boundary | G-024, G-007 |
| Sparks shown | A layer, off by default (PANES.md section 3) | G-026 |
| Toggles: sparks, top-down, bond values, names | Checkable actions shared by the canvas's bar and View > Cytoplasm layers; not remembered in QSettings (Numbo's log filters are not) | G-026 |
| Zoom and pan, "fit-to-width as the default" | Numbo's `TreeView`: auto-fit, wheel zoom, +, -, 1:1, Fit, drag to pan; `render.py` is Numbo's | G-023 |
| Click to select, shared with every pane | One `Selection` held by the window; a glom selects everything under it; a click toggles; empty canvas clears | G-025 |
| Hover an element for its fields in `show`'s format | As designed, with the bond's value in the reigning class when known | G-022 |
| Inactive glints | Drawn lighter (the glint's `active` field), not in PANES.md | G-023 |
| A removal's picture | A spark used up, a bond lost with its glom, a glom burst, dissolved, blasted or gone silently stays as a ghost fading over `FADE` events | G-021, G-024 |

### P3. Socratoplasm

| Design | The GUI | Decision |
|---|---|---|
| Gnoths "aligned with the glints of P2" | A row of rectangles in the cytoplasm's units, scaled to the strip's width: the two panes are docks of different widths, so the columns match in order and proportion, not in pixels | G-031 |
| Fields the engine changes with no event (bond lists, a cast's form, the equivalence type, a slip's class) | While a socratoplasm task or the Slip-scout runs, a card is marked unconfirmed (amber) until the next task boundary; two silent paths (a Gnoth-setter's clearing of its span, a Gnoth-maker's first cap taking the template glom) are modelled from their neighbouring events | G-030 |
| Cards per gnoth; the template | One `LineList` (Numbo's `LogList`) for the template's block and the cards, a bond line per bond for the hover; the template from its events, refreshed by the snapshot while it lives | G-031, G-032 |
| Hover a bond to highlight the arc | As designed, through the cytoplasm's `highlight` | G-031 |
| Copy as `show` | The `gnoths:` block of `show` at the cursor | G-030 |

### P4. Hypothesis

| Design | The GUI | Decision |
|---|---|---|
| Ovals with "argument lobes", first hits "printed under it" | Ovals for forms, circles for integers, no argument lobes; a shared place drawn as a small empty circle joined by a dashed line to its structure (Figure 2 draws the shared node once below its users); the first `TREE_HITS` values beside the top oval | G-034 |
| The box tree from `Hypothesis.box.structure_text()` with the real names | From the snapshot taken at the event when the hypothesis is new; between a new hypothesis and the next snapshot a fresh `BoxTree` gives provisional names and the pane says so | G-033 |
| Predictions `list_terms(n + k)` of a fresh hypothesis | `LOOKAHEAD` = 3 terms beyond the entered ones, on a fresh `Hypothesis(form)` (ENGINE-API rule 3) | G-035 |
| Validity bar with the threshold's mark | The bar's scale keeps the mark inside it at every validity | G-035 |
| Hit a private fresh box | The private box is built from the edit or the form in force, kept across seeks, forgotten at `start`; `build-box` and `show-box` of the appendix in any of the three spellings | G-035 |
| The box tree hidden by default | A toggle in the model, off by default | G-035 |
| No selection interplay | None (PANES.md gives it none) | item 09 |

### P5. Platoplasm (GUI design, not Meredith's; ANSWERS.md 4)

| Design | The GUI | Decision |
|---|---|---|
| A fixed network, nodes by category "from the strictest down" | One column per category from the strictest on the left, a box per type with its format and counts, the atoms as a strip below, after Numbo's Pnet view; labelled "GUI design, not Meredith's" in the pane, the tooltip, the dock's tooltip and the guide | G-036 |
| Pulls and pushes "in a side table for the reigning class" | The pane's list prints the reigning class's bonds under the check; the tables are in the tooltips | G-038 |
| The slip-check "as it happens"; the crown animates along the s-link | The crown travels the s-link over `CROWN_MOVE` events (events, not time, as P2's flashes) | G-037 |
| Live links to the cytoplasm as lines (Figure 2) | Tags on the nodes, not lines across docks: a line between two docks breaks when one is floated or hidden; "Links" off by default | G-038 |
| The manifestations' counts | The snapshot's lists, plus the events since; one known correction on run 3 (a glom burst silently by the Gnoth-setter stays listed until the next task boundary) | G-037 |
| Painting | A cached pixmap of the layout, re-rendered only when the layout, the size or the pixel ratio change (item 18) | G-060 |

### P6. Taskrack (GUI design, not Meredith's; ANSWERS.md 4)

| Design | The GUI | Decision |
|---|---|---|
| Rows by type with a bar of summed weight (Numbo's coderack view: a running scale) | Rows in order of first appearance; the bar is the type's share of the total weight, not Numbo's running scale | G-027 |
| The chance "computed from the snapshot before the draw" | The rack is read after the draw (G-007), so the chance is the weight over the shadow rack's total plus the task's own; the Hfilter run at once (`run_now`) has no chance and the band says so | G-027 |
| The chosen task on a band (Numbo: at the choice event) | The band stays tinted from the `chosen` event until the next, grey when stale | G-028 |
| A cleaning banner "transient" | `BANNER` = 30 events | G-028 |
| A history of "the last N" chosen tasks | The whole history, in a virtualised list (Numbo's `LogList`), with tallies since the last `term` event | G-028, G-029 |
| Click a task to select what its arguments name; filter by arena | As designed, through the shared selection; the arenas' colours distinct from the bond colours | G-028, G-029 |

### P7. Reformulation

| Design | The GUI | Decision |
|---|---|---|
| The "after" side of an operation "is the next snapshot" | `before` is taken just before the operation's first event and `after` at its `operation` event, read off a socratoplasm model of the pane's own; the next snapshot would be wrong for a PROGRAM's middle steps | G-039 |
| The storyboard's cards | Built from the events alone, one handler per kind, each event on exactly one card, in event order; a late Capper gets a card of its own; the catchall is left out of the pictures | G-039 |
| The freeze-dried list, the threshold plot | From the snapshot with the events between; one plot point per change, armed at the freeze-dried card; the Bond-assessor's given threshold is shown, not plotted | G-040 |
| "Operations in series" as a column view | A toggle, off by default, giving a header of gnoth names and a line of brackets per operation (p. 120's side-by-side figure as lines) | G-041 |
| Click to seek; hover an operation to highlight; episodes collapse | As designed, through `MainWindow.seek` and the shared selection (leaving a card restores the selection); the pane raises itself at the run's first `refuted` event, live only | G-041 |

### P8. Console

| Design | The GUI | Decision |
|---|---|---|
| Two `Trace`s fed by the tap, lines tagged by event | Two `Trace`s fed on the GUI thread by the console model; the lines the `Session` writes outside any event (echoes, commands' output, `bye`, the hint) are played from the input lines at each ask; the summary line at `finish` for scripted runs, as the CLI prints it | G-018 |
| Lines after the cursor "greyed (when following) or hidden (option)" | Greyed; no option to hide them | G-020, **G-062** (left out) |
| Find (Ctrl+F) | A find box above the rows, Enter or the button; no shortcut | G-019, **G-062** (left out) |
| Click a line to seek (Numbo's `LogList`: double-click) | A single click seeks, as PANES.md says; the rows are Numbo's `LogList` otherwise | G-020 |
| Copy | Edit > Copy copies the whole transcript in the mode shown; no selection of lines | G-019 |
| The command line at the bottom | One path for every typed line (the sequence pane's box and the console's line): with an interactive run live a command is a session input, otherwise it prints at the cursor | G-019 |

### P9. Event log

| Design | The GUI | Decision |
|---|---|---|
| Numbo's `log_view.py`, driven by hand from the window | A view of the window's protocol (`set_position`, `redraw`), with a clock column; otherwise Numbo's | G-045 |
| Filter by kind and by group | The seven groups follow the sections of `events.py`; the `visible` list is guarded by a lock because the tap appends on the worker | G-044 |
| Save events as JSON lines | In the Session menu, byte-equal to `python3 -m seekwhence --json`; built in item 12, not item 13 | G-045 |

### P10. Timeline

| Design | The GUI | Decision |
|---|---|---|
| Numbo's `TimelineBar` with a spin box | One line edit for the three go-to forms (`term 5`, `clock 812`, `event 402`, a bare term number) and a Go button | G-043 |
| Markers and the parse strip | Two plain strips under the slider; the markers from a model that scans the history (so a replay's history fills it too); a slip marker at every `class-changed`; the parse from the prompt's own snapshot | G-042, G-043 |
| Scrubbing only when no run is live | The bar is disabled while a run is live; seeking rebuilds every view from the history and draws once | G-043 |

### P11. Controls

| Design | The GUI | Decision |
|---|---|---|
| Numbo's panel | Numbo's `ControlsPanel` with a Qt-free `ControlValues`; the inputs rebuilt at every change, so `window.inputs` is always the panel's; the buttons take no keyboard focus; the panel is not a view | G-049 |
| Presets: the appendix runs, the Blackburn dozen, the other lists | The engine's catalogue (49 presets in six groups), each with its citation and reported result; a sequence preset feeds 16 terms as the bench does; the answering mode is independent of the preset | G-048 |
| Answering modes | Interactive, scripted, and the continuation judge through the `answerer` hook; a judged run's answers are stored in the session file as an interactive run's are, so the file replays as a scripted run | G-048 |
| Keys (section 4) | Numbo's, plus Step task Ctrl+Right | G-013 |
| A run started any way | The preset shown, the title and the stats row agree with the inputs used (ANSWERS.md 10.3) | G-054 |

### P12. Stats

| Design | The GUI | Decision |
|---|---|---|
| Numbo's `stats_view.py` (QLabels) | Paints its own rows, one paint per event; reads the snapshot through `set_position`, a method of the view protocol the window calls before each event; the configuration note (`default`, or `edited:` and the fields) | G-016 |

### P13. Configuration

| Design | The GUI | Decision |
|---|---|---|
| PANES.md: read-only; **ANSWERS.md 5: editable** | Editable: a widget per kind, a Reset per row, Reset all, an editor under the table for long values; edits apply to the next run through `dataclasses.replace(Config(), **edits)`; saved under one key `config/edits`; the title and the stats mark a non-default run; a replay shows the file's configuration read-only | G-050, G-051 |
| The caps and the trace | Shown here disabled: they belong to the controls pane (P11) | G-051 |
| Copy a row; click a decision number to open its text | Neither: the Source tooltip shows the decisions' headings | **G-062** (left out) |
| Hidden by default, in the right area | Hidden by default; tabified beside the Hypothesis and Reformulation docks, not stacked fourth in the right column, so the window fits 1280 x 800 (item 18) | G-058 |

## 2. The architecture, the layout, the menus, the settings, the session file

| Design | The GUI | Decision |
|---|---|---|
| The snapshot and the models live in the GUI (ANSWERS.md 1, 2) | `seekwhence/gui/models/`; the engine tests' `modules()` skip `gui/` (the one permitted edit) | G-001, G-002 |
| Numbo's history, filled by the window on the GUI thread | The history is the tap's event log, filled on the worker as the events happen, so `headless.run` reads the same history the window shows | G-012 |
| Numbo's run controller and event log | Adopted whole, with sessions, tasks and terms for runs and iterations; the engine lock kept | G-005 |
| A snapshot at every task boundary (ENGINE-API) | Dense while the clock is at most 500, keyframes every 64 events after, always at `prompt`, `guess`, `answered` and `ended`; a per-run cache (a snapshot at every task cost 7x the engine on blackburn-11) | G-003, G-004 |
| The answerer blocks the worker | A `threading.Event` released by the guess panel, or cancelled by a stop; the Bridge has a fourth queued signal | G-008 |
| Docks, QSettings, Reset layout (ANSWERS.md 3); a default layout file (ANSWERS.md 8) | Precedence: the user's state, then `default_layout.json`, then the built-in arrangement (a function); Reset layout, Reset to built-in layout, Save layout as default for this code…; the layout bytes carry a state version | G-010, G-059 |
| The built-in arrangement | Made at the first `showEvent`, never before the window is laid out (Qt 6.11 keeps stale tab bars otherwise; ANSWERS.md 10.1 and 10.2) | G-052 |
| Settings keys `controls/preset`, `controls/terms`, `controls/seed`, `controls/cap`, `controls/speed` | `controls/preset`, `terms`, `seed`, `cap_term`, `cap_total`, `answering`, `verbose`, `speed`, and `config/edits`; `window/geometry`, `window/state` | G-049, G-050 |
| Menus (section 4) | Session also has Save transcript…, Save events (JSON lines)…; Edit > Copy copies the transcript; Run has Step task; View has Cytoplasm layers and the three layout actions; Help's page is the guide's table in a dialog | G-013, G-019, G-026, G-010, G-055 |
| The command line: Numbo's `--puzzle` / `--custom` | The CLI's names: `--preset`, `--terms`, `--seed`, `--answers`, `--max-steps`, `--play`, `--smoke`; `--smoke` prints `Session.summary()` | G-055 |
| The session file (section 5) | Numbo's JSON lines with a run line; records compared as JSON text; a `Form` argument as text; completeness from the last event | G-046 |
| Open: replay and compare; on a difference the structure panes "marked not available" | The re-run goes through the window's controller with a checker on the worker that stops the run before the differing event is drawn; the stored events are then loaded without snapshots and the panes show what the events alone give; the status bar, a message box and the stats row say so; a part-way file's re-run goes on | G-047, **G-062** |
| Screenshots and documentation (Numbo's `docs_screenshots.py`, `test_readme.py`) | Four pictures from the appendix presets; the guide around the help page's table; the README's section with the `--smoke` command in a `sh` block | G-056, G-057 |
| The smallest window | 1280 x 800 with every dock shown (item 18): explicit minimums, clipping labels, a scrolling controls form | G-058 |

## 3. Citations re-verified on the page images

24 tags were drawn with `random.Random(20261012)` from the 815 tags of `seekwhence/gui/`
(16) and the 123 of `docs/gui/panes.md` (8), and each cited page rendered with
`pdftoppm -r 90` (thesis PDF page = printed + 12; FCCA PDF page = printed + 10; MHN pages
are PDF pages, as the engine's SPEC.md reads them) and read against the claim.

| Where | Tag | The claim, in brief | On the page |
|---|---|---|---|
| `models/reformulation.py:11` | [M86 p. 174] | the strict class slipping at once, `Countup --> S-group` | yes: the two commented lines after the Reformulator |
| `console_view.py:308` | [M86 pp. 164-179] | what the CLI takes at its prompt: a term or a show-command | yes: `show-hypothesis`, `show-parse`, `show` at the prompts of p. 174 |
| `timeline.py:16` | [FCCA pp. 64-65] | the mountain chain solved by successive bracketing | yes: `1 (1 2) 1 1 (1 2 3) 2 2 1 (1 2 3 4) 3`, p. 64 |
| `cytoplasm_view.py:319` | [M86 p. 165] | a proposal's family and test, `Same -cover` | yes: `Same -cover proposed --> glint1` |
| `console_view.py:6` | [M86 pp. 164-179] | Meredith's only display is the console transcript | yes: pp. 165, 166 and 174 are transcript |
| `cytoplasm_view.py:204` | [FCCA pp. 58-60] | a glom's hull as an island of order | yes: Building up Islands of Order, p. 58 |
| `models/snapshot.py:160` | [M86 pp. 77-78] | a gnoth's record: an actualization of a Platonic class, its glom collection, one hit of the hypothesis | yes: the three views of a gnoth, p. 77 |
| `socratoplasm_view.py:10` | [M86 p. 102] | the row gnoth1 gnoth2 gnoth3 over their gloms and glints | yes: Figure 1's top row |
| `models/sequence.py:49` | [M86 p. 166] | the term a hypothesis predicted at a position | yes: term 7 hypothesis-filtered through `(Cycle 3 2 (3 7))`, the guess `3 7 3 7` |
| `models/tap.py:16` | [M86 pp. 164-179] | every appendix run has under 200 tasks | yes: the `doing task` lines of the three runs |
| `hypothesis_view.py:9` | [M86 p. 79] | the Tuple fall-back as a hypothesis's source | yes: the Hypothesizer's fall-back position, a Tuple |
| `models/event_log.py:181` | [MHN p. 3] | the terms are entered one by one | yes: terms entered one-by-one as requested |
| `models/presets.py:25` | [M86 pp. 164-179] | the three appendix runs | yes |
| `models/cytoplasm.py:247` | [M86 p. 102] | an arc below the baseline with its value, `+10`, `-5` | yes: Figure 1's arcs under the glints, labelled +10 and -5 |
| `models/hypothesis.py:49` | [M86 p. 56] | the hits printed under a form, `(1 2) (1 2 3) (1 2 3 4)` | yes: `(S-group 1 (Countup 2))` giving 1 2, 1 2 3, 1 2 3 4 |
| `models/snapshot.py:123` | [M86 p. 69] | a dubbed glom's structure as form and purity | yes: new structure and purity, pure or secondary |
| `models/snapshot.py:123` | [M86 p. 67] | a glint's `active` field | with a note: p. 67 says the Glommer makes the new glom an active focus; the engine's `Glint.active` cites it (and p. 47), and the GUI follows the engine |
| `docs/gui/panes.md:390` | [M86 p. 37] | a user presents numbers to the system | yes |
| `docs/gui/panes.md:386` | [FCCA pp. 53-54] | FCCA's sampler of sequences | yes: Typical Sequences in the Seek-Whence Domain |
| `docs/gui/panes.md:243` | [M86 pp. 103-105] | each neighbour's pressure as p. 104's product | yes: 0.4 times (10 + 10) = 8 |
| `docs/gui/panes.md:139` | [M86 p. 67] | a glint inside a glom drawn lighter, its `active` field | the same note as above |
| `docs/gui/panes.md:91` | [M86 pp. 16-17, ...] | P4's ovals after Figures 1 and 2 | yes: Figure 1, the major primitives, p. 16 |
| `docs/gui/panes.md:263` | [M86 pp. 41-42] | tasks run in simulated parallel and wait on the taskrack | yes: section 3, Processing and Tasks, p. 41 |
| `docs/gui/panes.md:240` | [M86 pp. 48-49] | the ideal atoms with their manifestations | yes: the ideal-atoms, p. 48 |
| `docs/gui/panes.md:401` | [M86 p. 164] | the terms entered | yes |

Twenty-three confirmed outright; the two tags on the glint's `active` field cite the page
the engine cites and are kept. The machine checks stand beside this: `tools/check_quotes.py`
finds every quotation of six or more words on its page (the gate runs it over all of
`Software/SeekWhence/`), and `tests/gui/test_gui_conventions.py` requires a tag, a decision
or a precedent in every public docstring.

## 4. The engine since BASELINE

`git diff --stat 79b73f5 -- Software/SeekWhence/python/seekwhence Software/SeekWhence/python/tests`
lists, outside `gui/`, only the two permitted files, 4 lines added and 1 deleted in each
(the two-line comment and the one changed line of `modules()`, ANSWERS.md 1):

```
Software/SeekWhence/python/tests/test_conventions.py | 4 +-
Software/SeekWhence/python/tests/test_package.py     | 4 +-
```

`tools/`, `SPEC.md`, `DECISIONS.md` and `RESULTS.md` have no diff. The gate checks the
same on every iteration, and runs the engine's suite with PySide6 unimportable.

## 5. Determinism on five more seeds per appendix run

`ralph_loops/loop0004/check_determinism.py` (not the gate) runs each appendix run, with
Meredith's inputs and answers, on seeds 1, 2, 3, 7 and 11 (the golden seeds are 54, 54 and
30), and compares the events, `rng.draws` and the outcome of a plain `Session` with
`headless.run(window=False)` (the Qt-free controller) and `headless.run(window=True)` (the
offscreen `MainWindow`). All 30 comparisons were identical (19 s): run 1 ends `no-terms`
on four of the seeds and `ok` on seed 7; runs 2 and 3 end `ok` on all five; run 3 has 597
to 1048 events by seed. The gate's own check covers the golden seeds and two Blackburn
sequences.

## 6. The engine methods called

`grep -rnE '\.hit\(|\.next_term\(|\.check\(|\.postdict\(|\.reset\(|\.rebuild\(|\.choose\(|rng\.|counters' seekwhence/gui`
finds, in the GUI's code:

- `self.reset()` in the pane models, `self.check(event)` in `ReplayChecker` and
  `self.reset(n)` in `ConfigView`: the models' own methods;
- `tree.hit()` and `tree.reset()` in `models/hypothesis.py` (`first_hits_text`,
  `node_ranges`, `PrivateBox`): on a `BoxTree` built fresh from the form's text for the
  pane's own use, never the run's (ENGINE-API rule 3; G-034, G-035);
- `world.rng.draws` in `run_controller.py` and the docstring of `headless.py`: an attribute
  read at the end of the run.

Nothing calls `Hypothesis.hit`, `frame`, `next_term`, `check`, `postdict`, `reset` or
`rebuild`, `BoxTree.show`, a `Rng` method, `World.name` or `Taskrack.choose`, and nothing
passes `world.counters`. `tests/gui/test_snapshot.py` and `test_hypothesis_model.py` check
at run time that a snapshot and the pane leave `covered`, the box values, the counters,
the draws, the rack and the arenas untouched.

## 7. Functions over 60 lines

Two pure functions were split at the review: `models/cytoplasm.py`'s `layout` (73 code
lines) into `layout`, `_arc_shapes`, `_hull_shapes`, `_marker_shapes` and `_bounds`, and
`sequence_view.py`'s `paintEvent` (75) into `_paint_cells`, `_paint_ghosts` and
`_paint_badges`; their tests pin the layout and the pixels, and pass unchanged. Six remain,
all constructors that declare widgets in a straight line, and the menu builder, Numbo's
shape (its `MainWindow.__init__` is 122 lines): `ControlsPanel.__init__` (149),
`ConfigView.__init__` (84), `MainWindow._build_menus` (78), `ConsoleView.__init__` (70),
`HypothesisView.__init__` (69), `TaskrackView.__init__` (67). G-063 records the reason;
`test_gui_conventions.py` lists the six and fails on any other.

## 8. The test suites' time

Measured under the gate on 2026-10-12 (a 32-core machine; the GUI suite runs offscreen
with every window test painting):

| Suite | Tests | Time |
|---|---|---|
| The engine's suite with PySide6 unimportable (`tests/`, with the Qt-free GUI model tests) | 1442 passed, 26 skipped | 74 s |
| The GUI suite (`tests/gui/`, offscreen) | 650 passed | 368 s |

TASK.md asks for the GUI suite under five minutes; it has been over since item 14 (the
preset runs through the window) and item 18 added about two minutes (`blackburn-11` to the
end). G-061 suggests the long run behind an environment variable; that is the author's
call (PROGRESS.md, "For the author").
