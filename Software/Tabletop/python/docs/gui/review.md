# The GUI reviewed against its design (loop0006, item 20)

The final review of `tabletop/gui/` against the loop's `PANES.md`, the engine's
`docs/engine-api.md` and the loop's `ANSWERS.md` (which overrides both), done on 2026-10-13
on the code as items 00 to 19 left it (commit `d845a27`). Every departure from PANES.md, and
every place the GUI follows the Numbo GUI (`Software/Numbo/python/`, the precedent for every
convention, ANSWERS.md 5) or the Seek-Whence GUI (`Software/SeekWhence/python/`, the closest
worked example) instead, is listed with the `G-NNN` entry of `GUI-DECISIONS.md` that
records it. The checks the item asked for follow: a sample of citations re-read on the page
images, the engine's diff since the loop's baseline, determinism over the whole library, the
whitelist of engine calls, the long functions and the test suites' times. What the review
found and changed is in section 9; the open questions and the weakest parts are in
`ralph_loops/loop0006/PROGRESS.md`, "For the author".

## 1. Pane by pane

Each table has what PANES.md (or ANSWERS.md) says, what the GUI does, and the decision.
Everything PANES.md lists under **Shows**, **Events**, **State** and **Interactions** that is
not in a table is built as written, and every test PANES.md names exists in `tests/gui/`
(`tests/gui/README.md` maps them). The dynamics are shown as they are (ANSWERS.md 11): on
the quick start's run (`surround-05` seed 3 since the roles' item 04; the facts on the v2
defaults since v5's fix, D-116 to D-120: 59 codelets, 720 events, Eliza touches spoon3
(none); 44 codelets, 507 events, cup2 (E2) from item 08's runs to v4)
the Worldview pane lists its one correspondence, the competitions pane the two competitions
(an entry without a rival at codelet 18, and the fight at codelet 40 that cp2 wins), the
network says "no context push in this run", and the thermometer holds at 80.0 then jumps to
97.9 at the first Worldview entry and ends at 43.6.

### P1. Table

| Design | The GUI | Decision |
|---|---|---|
| Fed by `RunStarted` and `Snapshot.table` | The window hands the run's `Problem` to the table before the run starts, since `RunStarted` carries ids only and the start's snapshot comes some twenty events later; the events give the marks, the snapshot the positions only when no problem was given | G-017 |
| The groups, proposals and correspondences from the events and the snapshot | From the events alone, exact at every index (checked against every snapshot of two runs); the snapshot only dates the hover text's numbers "as of codelet N" | G-020, G-023 |
| A proposal "with its stage" | A faint dotted outline with three stage dots above its corner, one filled per stage passed (a text label cluttered the gestalt box) | G-021 |
| The flash by the event index | The dash offset by the index's parity; a new correspondence's end boxes filled on even and outlined on odd indexes for two codelets; removed structures fade for three codelets then go | G-021 |
| Hover an object for its descriptors "and salience terms" | The descriptors always; the nine salience terms (French's eight plus "role", D-100) when the overlays' salience layer is on, since they are read beside the snapshot for the overlays | G-018, G-025 |
| Click to select, shared "with P5, P6 and P10" | One `Selection` held by the window, shared with P3, P3b, P5 and P6; the log pane (P10) has no element that names a structure and does not follow it | G-022, **G-059** (left out) |
| Zoom, pan and fit (Numbo's `tree_view.py`) | Numbo's `TreeView` on a canvas with a bar (Fit, +, −, 1:1) and the legend; the keys + − 0 F | G-019 |
| The legend "saying that the icons are new drawings" | The bar's legend, with the pages and the overlays on, dated; an ignored horizontal size policy so that it clips rather than widens the window | G-018, G-025 |
| The end state visible after `RunEnded` | The answer arrow and the Worldview line stay drawn; the window settles every view once after the end, since a step's snapshot is recorded after its events were delivered | G-024 |

### P1 overlays

| Design | The GUI | Decision |
|---|---|---|
| The saliences, terms and region order read "without a draw" | Read by the tap beside each snapshot (`salience.terms` for every item in view, `salience.region_order`), dated by the snapshot's clock and never rolled back; the draw count is checked against a plain run | G-024 |
| "Each noticed object's last drawn region" | The notice's region first, then the snapshot's later look when it differs; the hover text adds "last drawn region" only then | G-024 |
| Toggles | Three layers and Eliza's view as checkable actions on the canvas's bar and in View > Table overlays (one state in two places), remembered under `controls/overlay-*`; every layer off leaves the layout untouched, so item 04's pictures stand | G-025 |
| Eliza's view, "the whole table rotated by 180 degrees" | A pure transform of the layout, (x, y) to (width − x, height − y), icons turned round and the names swapped, not a `QTransform` of the view, so that a fresh view and the window's render byte for byte alike; the regions keep the engine's names (D-005) | G-026 |

### P2. Thermometer

| Design | The GUI | Decision |
|---|---|---|
| "A grey band marks the updates while T is still held" | The column shows one value, so the band is read as the fill's colour: grey while the last update is held, black after; the plot shows the history | G-027 |
| "A small tick shows the last update's reason" | A magenta tick at the fill's top with the reason's name under the column | G-027 |
| Not in PANES.md | The GUI's additions, said in the tooltip and the pages text: faint scale marks at 0, 50 and 100, the reading's codelet and its age ("at 45 (+1)", D-050); empty with "no reading yet" before the first reading | G-027 |

### P2b. Temperature plot (GUI design, not French's)

| Design | The GUI | Decision |
|---|---|---|
| T, Q, ΔQ, Δ_WV and T_cp "against the codelets run" | Three bands sharing the x axis (T on 0-100 with half the height; Q and ΔQ on 0-1; Δ_WV and T_cp on 0 to the largest count); the geometry a pure function of the model, the size and the two options | G-028 |
| The terms as stacked areas | Stacked on the T band where the formula applied (not while held or under `temperature_fixed`); the run's `Config` put back from `RunStarted` so that a replayed session needs nothing more | G-027, G-028 |
| French's chapter 4 thermometer from `tests/data/ch4.json` | A constant in the model, checked against the file by the tests (the package reads no test data at run time); offered for `surround-05` only, the checked state kept when disabled | G-029 |
| The options | The plot's own actions on its bar and in View > Temperature plot, remembered under `controls/plot-*` | G-028 |

### P3. Slipnet grid

| Design | The GUI | Decision |
|---|---|---|
| Fed by `SlipnetUpdated`, `NodeJolted` and `Snapshot.slipnet` | From the events alone, exact to a tenth at every index (`changed`, the jolts, a push's reset, a pop's restored values); the snapshot dates only the hover text's effective lengths | G-030, G-032 |
| A jolt "outlines its box for one event" | As designed, so a step by codelet shows none; the hover text keeps the last jolt | G-030 |
| The chapter 4 dump "nearest in codelets" | The nine grids as a constant checked against `ch4.json`; the nearest by codelets, the earlier figure on a tie; in red in the lower right corners for `surround-05` only, remembered under `controls/grid-*` | G-032 |
| Click to select the node "in P3b" | The kind `node` in the shared selection, so the grid, the network and the status bar follow each other | G-030 |
| The box | French's: the label along the bottom, the number in the upper right-hand corner as the dumps print it, the black square's side linear in the activation; the seventh row dashed grey under "GUI design, not French's" | G-031 |

### P3b. Slipnet network (GUI design, not French's)

| Design | The GUI | Decision |
|---|---|---|
| The lengths equal "the snapshot's `effective` values" | From the engine's own reports: every labeled link starts at its floor length (the intrinsic times 1 − `label_shrink` / 150, the state the first update's report compares with) and `SlipnetUpdated.lengths` sets the links that changed; exact to a tenth at every snapshot of three runs. Recomputing from the activations would be wrong between a jolt and the next update | G-033 |
| "The label nodes highlighted when they shorten links" | A label node is bold while a link it labels is shorter than at the floor and it is the link's most active label (the one D-011 takes) | G-033 |
| Fig. 3.6's style | Three bands by level, the most abstract on top, up to three staggered sub-rows, the object band ordered by its labeled pairs; circles with a dark disc, ISA links as thin arrows, labeled links thicker as they shrink with the length beside them, identity loops, dashed stems from the label nodes; a cached pixmap (Seek-Whence GUI, `platoplasm_view.py`) | G-034 |
| The push and the pop | The reset nodes ringed in magenta while inside the subworld, the restored nodes ringed in blue through the pop's codelet; the banner "no context push in this run" until a push; the push seed `blockage-07` seed 116 found in a bounded search of 18,020 runs (with the roles, `blockage-08` seed 111, one of two in 15,900 runs; with the role codelets, `surround-04` seed 20, the first of 17 in 15,900 runs; with the four role detectors of item 05, `surround-07` seed 13, the first of 17 in 15,900 runs; with the ROW and COLUMN detectors and the re-cast of item 07, `ch2-fig12` seed 16, the first of 10 in 15,900 runs; with the GAP, LONER, STRAY and CORNER detectors of item 08, `ch2-fig08` seed 2, the first of 35 in 15,900 runs, with two pushes and two pops; since item 09's runs, with the FIGURE, GROUND, ECHO, TWIN and TYPE_KEY detectors, `ch2-fig11` seed 3, the first of 82 in 15,900 runs, with four pushes and four pops) | G-035 |

### P4. Coderack bars

| Design | The GUI | Decision |
|---|---|---|
| The bars from `Coderack.shares()` | From the events alone (the counts by type are exact at every one of 8,797 snapshots of 159 runs); the chances recomputed from `CodeletChosen.a` with the engine's `softened_weights`, bit-identical to the engine's | G-036 |
| "The number of codelets run" | On the pane's top row, right-aligned over the strip's right end, as the dumps place it | G-037 |
| French's labels | On two staggered lines under the box: 29 labels of up to nine characters fit one line nowhere | G-037 |
| The chosen bar outlined | From its choice to the next (a step by codelet shows it); a type with none left gets a short outline on the floor | G-037 |
| The legend | A checkable action on the bar and in View > Coderack, remembered under `controls/coderack-legend`; the 29 lines of `codelets.CATALOGUE` (the legend's 23 and the six role codelets, D-081) in a read-only text beside the strip | G-037 |
| Section 3: the Coderack group in the bottom area | The group is taller in the built-in layout (`BOTTOM_HEIGHTS` 170, 180), since `resizeDocks` on a tab group counts the tab bar and the bars were three pixels tall | G-037 |

### P4b. Rack list (GUI design, after the Numbo GUI's `coderack_view.py`)

| Design | The GUI | Decision |
|---|---|---|
| "The list equals `Snapshot.coderack["codelets"]`" | Not from the events alone: `CodeletChosen` carries no `posted_at`, and the engine draws among twins alike in type, arguments, generation and urgency at random. The list is re-based on each new snapshot and the events since re-applied, so it equals the snapshot at every step boundary live and at every index when seeking; only a twin's posting can be wrong until its step ends | G-036 |
| The chosen codelet on a band with its chance, `a` and the rack size | As designed, tinted at the `CodeletChosen` event only (Numbo's rule), with what the codelet did; "one of N alike on the rack" where the events cannot say which twin ran | G-038 |
| Removed codelets in grey for one event | As designed, at the codelet's old position with "(removed: age)" | G-038 |
| Columns | Nine (#, codelet, raw, urgency, gen, posted, age, chance, by), the oldest first; greyscale text antialiasing so that the tests can read a row's colour | G-038 |

### P5. Worldview (GUI design, not French's)

| Design | The GUI | Decision |
|---|---|---|
| "The entries equal the snapshot's correspondences" | The entries, membership, wins, losses and slippages from the events through the structures model, exact at every index; the points, the rating's three terms and its pairs from the snapshot in force, dated "as of codelet N"; the rivals read by the tap (`worldview.rivals_of`) beside each snapshot, since recomputing them would miss D-044's inside-only rule | G-039 |
| The chapter 4 state of `tests/test_worldview.py` | A model that has seen no events lists the snapshot's correspondences, which is the one path the live GUI never takes; the test loads the engine's test module by path | G-039 |
| The picture | Lines of text by role: a solid bar before a Worldview entry and a dashed one before a shadow entry (Fig. 3.11's convention), an incoherent pair in a warning colour, the rating's block with its pair lists; the Proposals as a second tab of the same pane; the lines widget shared with P6 and taking its lines lazily | G-040, G-058 |

### P6. Competitions (GUI design, after Fig. 3.11)

| Design | The GUI | Decision |
|---|---|---|
| Each fight with the old and the rival Worldview side by side | One fight per `CompetitionHeld`, the Old Worldview rebuilt from the `WorldviewChanged` events (an uncontested entry logs its change first, D-040) and the Rival Worldview as the old less the rivals plus the newcomer, as two small tables with the old correspondences solid and the newcomer dashed; the ratings, the chance, the exponent and the winner under them | G-041 |
| "Entered without a fight" | Listed as such on one line with the rating before and after; the banner counts fights and entries apart ("3 competitions: 2 fights, 1 entered without a fight") | G-041 |
| Not in PANES.md | The fight the event shown reports is tinted for that one event; a click selects the newcomer | G-041 |

### P7. Palette and editor

| Design | The GUI | Decision |
|---|---|---|
| Each type's count "above its cell" | Beside the cell, as Fig. 4.1 draws it (PANES.md's "above" is the figure's rotation); the silverware cells show the four orientations as Fig. 2.1 | G-051 |
| "Drag an icon onto the table" | Onto the editor's own canvas in the pane, not the P1 table, which carries a run's structures, overlays and selection; the run that Do this! starts shows on the P1 table like any other | G-051 |
| Save config with "provenance `synthetic`" | `problems.validate` refuses a synthetic provenance (a tag, a page, an image string and answers are required), so the dict carries true words in the file's form: the palette's page as the source, "placed in the GUI, not extracted by pdfimages", a 1100 x 1000 frame with boxes computed from the positions; the editor asks for at least one expected answer, which the format needs | G-050 (pending the author) |
| The buttons of the dumps | Load config, Clear table, Save config, Do this! and Run it again in two rows of three (Exit left out); Do this! takes the next run's configuration and seed, Run it again the next seed; a path inside `problems/` is refused | G-050, G-051 |
| Editing refused while a run is live | The model, the strip, the canvas, the buttons and the fields are turned off, and the edit line says so | G-051 |
| Not in PANES.md | Rotate silverware (R), mark the expected answers (E), a context menu, a double click to touch; the realism faults in orange with the page and a green line when French would set the table | G-050, G-051 |

### P8. Results

| Design | The GUI | Decision |
|---|---|---|
| "`run_batch` (or `run_problem` on `rng.run_seeds`)", equal to `--batch N --seed S` | `run_problem` on `run_seeds(seed, count)`, the CLI's seeds, on its own worker with no observer; the bench's `survey` derives a batch seed from the bench seed and the problem's name and would not match | G-052 |
| "Never during a live run" | The batch worker holds the run controller's engine lock, so a batch and a live run exclude each other in both orders (`EngineBusy`), and the window refuses a batch while a run is live, during a replay and on an input error, with the reason | G-052 |
| French's bar graph and table | French's bars black, ours grey, the percentage above each, the label under, "% of total runs" on the axis, his title with his page under the graph; the table under his three headings with the z of `harness.compare`; `other` as RESULTS.md groups it; French's set chosen when he prints several | G-052 |
| "Tabbed or floated" | Tabbed with the log pane in the bottom area; the body in a scroll area under the batch line, since a `QVBoxLayout` squeezed under its minimum painted the table over the graph | G-052, G-058 |
| Section 4: Run > Batch… | Shows and raises the dock and starts a batch of the pane's count | G-052 |

### P9. Controls (Numbo's `controls.py`)

| Design | The GUI | Decision |
|---|---|---|
| "The chapter 4 configuration (`surround-05`) is first", every problem by family | One entry per problem, so `surround-05` heads the list and is left out of its family's section; disabled family headers; the title, family and page under the selector | G-044 |
| Play, Pause, Step, Step codelet, Run to end, Stop, Back | The seven buttons without keyboard focus (Back added by ANSWERS.md 5); Pause only while running, Stop while live, Back when not | G-044 |
| The selector, the title and the Stats "Run" line agree whichever way the run was started | Every entry point goes through `start_run` and `_show_started`: the panel, the Problem menu, `--problem`, `set_next_run`, Do this!, a replayed session | G-044 |
| Not in PANES.md | A run line along the top of the pane, outside the scroll area, is the one thing the pane repaints per event (the window's paint-once probe counts every pane) | G-044 |
| The inputs remembered under `controls/*` | `controls/problem`, `profile`, `seed`, `speed`, every value checked and a bad one replaced by its default | G-044 |

### P10. Trace and event log (Numbo's `log_view.py`)

| Design | The GUI | Decision |
|---|---|---|
| "The text trace exactly as the CLI prints it" | Two of the engine's own `observe.TextTrace`s printing nowhere, so the lines are the CLI's by construction (tested against the CLI as a subprocess, normal, quiet and JSON) | G-042 |
| "Every line is tagged with its event index" | Two indexes: the first event of the line's block (the `CodeletChosen`) and the event at whose arrival the CLI printed it; the greying and a click use the block's start | G-042 |
| Click a line to seek (Numbo's rows: double-click) | A single click on a trace line, as PANES.md says; double-click or Enter on an event row; the rows in view painted whole at every event rather than Numbo's two retinted rows, so the paint-once rule holds whatever is in view | G-042 |
| Save trace, Save as JSON lines | In the Session menu and on the tabs, the CLI's text byte for byte | G-042 |
| Not in PANES.md | The Quiet switch is not remembered in the settings | G-042 |

### P11. Timeline (Numbo's `timeline.py`)

| Design | The GUI | Decision |
|---|---|---|
| The markers | The eight kinds of PANES.md P11 with shapes and colours on two rows, scanned off the history once; an uncontested entry's marker says "entered the Worldview without a fight" (D-040) | G-043 |
| "A strip of T under the slider (P2b's series, thin)" | The Seek-Whence GUI's strip through a composed temperature model, grey while held; one polyline per run of readings alike in `held`, the pens cached | G-043, G-058 |
| Back and forward; go to codelet N | Numbo's bar with ◀ ▶, the slider seeking on release, a codelet spin box with Go | G-043 |
| Refused while live | The window disables the bar with a history and a live run; the count and the markers are the whole history's, the cursor marks the position (live, the count changes at every event) | G-043 |

### P12. Stats (Numbo's `stats_view.py`)

| Design | The GUI | Decision |
|---|---|---|
| Numbo's labels | Rows painted on a scroll area, one paint per event, the text elided; the title dim | G-045 |
| `invariants.check` "on the current state" | Run by the tap beside each snapshot (D-077; on the whitelist), so a seek shows the verdict in force, dated; red with one row per sentence when any is violated; "not checked yet" before the first snapshot | G-045 |
| The Worldview's rating | `WorldviewChanged.rating` is the rating at the change and the structure value keeps changing (D-062), so the row takes the rating as it stands from the snapshot in force, dated | G-045 |
| The report line | From `RunEnded`'s fields through `observe.report_sentence`, equal to `report_line(result)`; a stopped run or an error from the controller through `finish`, kept across a seek | G-045 |
| "A mismatch on replay" | The Source row says `stored events of NAME (differs at event K)` and the State row `stored` | G-049 |

### P13. Configuration (ANSWERS.md 3; Seek-Whence GUI, `config_view.py`)

| Design | The GUI | Decision |
|---|---|---|
| The comments "as `tools/knobs_table.py` reads them" | The model carries the tool's three patterns (`tools/` is not a package and is not installed); a test loads the tool by path and compares every section, name and comment | G-046 |
| `dataclasses.replace(PROFILES[profile], **edits)` | As designed, every value conformed to its field's type (integers kept inside float slots of the tables, as the engine's own tables mix them); the edits kept across a profile change and marked where they differ from the new profile; the edits saved as one JSON text under `config/edits` | G-046, G-047 |
| The title marks a run "as `RunStarted.changed` reports it" | Read apart: the title says `(human; edited: max_codelets)` with the edits against the profile, the stats row keeps `human: type_weight=50` in the edited colour, which is `changed`'s letter (against `standard`) | G-047 |
| The pane's label | Cites its precedent (Seek-Whence GUI) rather than "GUI design, not French's", which the window's test reads as marking a whole new pane and expects for five panes only; the guide says the same | G-047 |
| Not in PANES.md | A status line along the top is what the pane repaints per event; a replay shows the file's configuration read-only with the user's edits untouched | G-047, G-049 |

### Help

| Design | The GUI | Decision |
|---|---|---|
| Generated from the pane-to-pages table of `docs/gui/panes.md` | The table is data in `models/guide.py` (`PANE_ROWS`), which the window's dock tooltips, every pane's `pages` and Help read; the guide's table is the model's cell for cell, held equal by a test, and Help prefers the file's table when it parses | G-053, G-055 |
| "What to expect" | The one correspondence, the rare push, the thermometer's jump, nothing tuned, with their RESULTS.md sections; About names the reconstruction, the sources and the licence | G-053 |

## 2. The architecture, the layout, the menus, the settings, the session file, the command line and the documentation

| Design | The GUI | Decision |
|---|---|---|
| `observers=[tap, ..., gate]` (TASK.md) | The tap is a controller observer behind the gate, so the log holds exactly the events the GUI was handed; the records, draws and report are the same either way (the gate's determinism check) | G-006 |
| A snapshot per codelet, "fall back to keyframes if it costs more than twice the engine's time or 200 MB" | Measured on the capped run: 4.2x and 382 MB. Snapshots after every step while the clock is at most 300, every 16 steps after, always at the end: 1.27x and 59 MB; nearly every library run stays exact at every codelet | G-007 |
| "Each pane model is a pure function of (the events up to k, the nearest snapshot)" | Every pane model but the rack list is a function of the events alone, exact at every index; the snapshot dates the hover text's numbers (`as of codelet N`) and supplies what the events do not carry (the points, the rating's pairs, the saliences, the rivals, the invariants); the rack list is re-based on each snapshot | G-020, G-024, G-036, G-039, G-045 |
| Numbo's run controller | Adopted through the Seek-Whence GUI's, with a step codelet that ends in the worker at the step boundary (so the pause falls exactly between two `World.step()` calls), a generation on every delivery, the engine lock shared with the batch worker | G-008, G-052 |
| Step = one event, Step codelet, Back | As ANSWERS.md 5; every view repaints exactly once per event and a fresh render equals the window's (the paint-once probe and the pixel probes) | G-008, G-011, G-058 |
| `headless.run(..., window=True)` | Through the real `MainWindow` with `run_to_end()`, reading the window's own history | G-009 |
| Docks, QSettings, the layout file, the three layout actions (ANSWERS.md 2) | Precedence: the user's state, then `default_layout.json`, then the built-in arrangement, with status-bar fallbacks; the state stamped with `STATE_VERSION`; the arrangement made at the first `showEvent` | G-010, G-012, G-013 |
| Section 3: the default layout | As the table, with the three splits of French's screen checked by geometry; 1280 x 800 with every dock shown (minimum hint 437 x 567) | G-014, G-015 |
| Section 4: the menus | As listed, with submenus the design did not name: View > Table overlays, Temperature plot, Slipnet grid, Coderack (one state with the panes' bars); the Problem menu by family with pages; a problem chosen during a replay closes it | G-015, G-025, G-028, G-032, G-037, G-049 |
| Section 5: the session file | To the letter; a file with no end line is a mid-run save unless its last event is `RunEnded` or a line is cut; the digest is checked on reading, so an event edited in place is refused before the replay compares anything | G-048 |
| Open: replay through the controller, compare, warn and show the stored events | The re-run goes through the window's own controller with a checker subscribed after the tap that stops the run at the first difference; the stored events are then loaded without snapshots and the panes show what the events give; `start_events` is unused | G-049 |
| Section 6: the icons | Qt-free primitives in `models/icon_shapes.py`, painted by `icons.py`, shared with `scripts/docs_figures.py` (its `icon()` is gone; `table-surround-05.png` regenerated, the gallery byte-identical) | G-016 |
| The command line (ANSWERS.md 1) | `--problem`, `--seed`, `--profile`, `--play`, `--smoke`, and `--set FIELD=VALUE` through the CLI's own `configure`; `--smoke` prints French's report line and exits 0, 1 or 2; no `--session FILE` or `--table FILE` | G-005, G-054 |
| pytest-qt in the `test` extra (Numbo, Seek-Whence) | In the `gui` extra (ANSWERS.md 1's reading) | G-004 |
| `tests/gui/test_readme.py` (ANSWERS.md 12) | `tests/gui/test_gui_readme.py`: pytest refuses two test files with one basename when neither folder is a package, and the fix is outside the loop's places | G-056 (pending the author) |
| The permitted engine edits | Six files, each within the gate's limits (section 4) | G-003, G-057 |
| The GUI's default tests in about 45 s (ANSWERS.md 10) | Over the budget since item 13 for a structural reason (about 35 whole runs through windows, every pane painted per event); the whole default suite stays under the gate's 300 s backstop (section 8) | G-058 |

## 3. Citations re-verified on the page images

24 tags were drawn with `random.Random(20261013)` from the 254 tags of `tabletop/gui/` (16)
and the 46 of `docs/gui/` (8; the sample was drawn before the correction of section 9, which
changed no tag), and each cited page was rendered with `pdftoppm -r 100` (the PDF page from
the book's "Page N" markers, as `tools/check_quotes.py` maps them) or read from its text
layer, and checked against the claim.

| Where | Tag | The claim, in brief | On the page |
|---|---|---|---|
| `models/structures.py:7` | [F95 p. 102 (img)] | a new subgroup captioned "Build new subgroup" | yes: Fig. 4.5, under the spoon-spoon subgroup's box |
| `models/palette.py:279` | [F95 p. 114] | Henry touches one object, marked H | yes: "the object touched by Henry is designated by an arrow marked H" |
| `models/coderack.py:103` | [F95 p. 76] [F95 p. 82] | a codelet taken off the rack unrun, by age or capacity | yes: p. 76, a codelet "on the Coderack too long is eliminated"; p. 82, the rack holds 150 and the oldest are pushed off |
| `models/slipnet_network.py:5` | [F95 p. 56] | a labeled link's length varies with the activation of its label node | yes: "the length of any labeled link varies dynamically as a function of the activation of the link's label node" |
| `table_view.py:13` | [F95 p. 104 (img)] | group-level correspondences thicker | yes: Fig. 4.7, the solid line between the two spoon-spoon subgroups is heavier than the dashed one to the glass |
| `thermometer_view.py:33` | [F95 p. 98 (img)] | the thermometer filled black | yes: Fig. 4.2, a black column with 80.0 beside its top |
| `table_view.py:227` | [F95 pp. 98-107 (img)] | the dumps' drawing conventions | yes: Figs. 4.2 to 4.9 |
| `palette_view.py:6` | [F95 p. 55] | the silverware in four orientations | yes: "The forks, knives, and spoons have four possible orientations" |
| `models/guide.py:63` | [F95 p. 26 (img)] | the palette, Fig. 2.1 | yes: the column of eleven types, the silverware four ways |
| `thermometer_view.py:27` | [F95 p. 96 (img)] | the thermometer is narrow, right of the table | yes: Fig. 4.1 |
| `models/palette.py:399` | [F95 p. 96 (img)] | a Save config button | yes: Fig. 4.1's six buttons |
| `controls.py:116` | [F95 p. 114] | the problems by family | yes: the families named (Blockage and the rest) |
| `models/overlays.py:11` | [F95 pp. 66-67] | the salience factors in French's order | yes: "The Salience of Objects", the factors listed |
| `models/structures.py:52` | [F95 p. 102 (img)] | the screen's caption "Build new subgroup" | yes, as above |
| `models/structures.py:11` | [F95 p. 101] | a new correspondence "is highlighted by flashing boxes at both ends" | yes, word for word |
| `palette_view.py:15` | [F95 p. 38] | whether French would set the table (realism) | yes: tables "reasonably realistic, in the sense of looking like a real table that was set" |
| `docs/gui/panes.md:337` | [F95 p. 105] | rivals against incoherent pairs | yes: rivals "are like Necker-cube interpretations", incoherence is "discouraged" (p. 101) |
| `docs/gui/panes.md:125` | [F95 pp. 96-107 (img)] | Fig. 4.1 and the dumps behind P1 | yes; **the row's own words had the sides reversed** (section 9) |
| `docs/gui/panes.md:132` | [F95 pp. 67-74] | the Worldview's per-correspondence points and rating in the text | yes: p. 68 the static, dynamic and bonus parts; p. 74 strength, coherence, parallelism |
| `docs/gui/panes.md:333` | [F95 p. 72 (img)] | solid against dashed | yes: Fig. 3.11 |
| `docs/gui/panes.md:125` | [F95 p. 26 (img)] | the palette | yes |
| `docs/gui/panes.md:30` | [F95 p. 26 (img)] | the icons in the spirit of his palette | yes |
| `docs/gui/panes.md:130` | [F95 pp. 98-107 (img)] | the Coderack strip in the dumps | yes: the bar graph under every dump, with "Number of codelets run" |
| `docs/gui/panes.md:324` | [F95 pp. 67-74] | the Worldview and the fight in the text | yes: p. 72, the Old and Rival Worldview; p. 74, the rating |

Twenty-four confirmed. The machine checks stand beside this: `tools/check_quotes.py` finds
every quotation of six or more words on its page (the gate runs it over all of
`Software/Tabletop/`: 1411 checked, 0 failed), and `tests/gui/test_gui_conventions.py`
requires a tag, a decision or a precedent in every public docstring.

## 4. The engine since BASELINE

`git diff --stat 924cb799 -- Software/Tabletop/python` lists, outside the GUI's places
(`tabletop/gui/`, `tests/gui/`, `docs/gui/`, `scripts/`, the PNGs of `docs/`, the two
READMEs, `GUI-DECISIONS.md`, `pyproject.toml`, `run-tests.sh`), only the six permitted
files, each within the gate's limits (lines added / deleted since the baseline):

```
docs/engine-api.md        4 / 2   (limit 12 / 8; item 18, G-057)
docs/standalone.md        5 / 3   (limit 12 / 8; item 18, G-057)
tests/test_conventions.py 2 / 1   (limit  4 / 2; item 00, G-003)
tests/test_package.py     3 / 3   (limit  8 / 5; item 00, G-003)
tests/test_readme.py      2 / 1   (limit 12 / 4; items 00 and 18)
tests/test_standalone.py 10 / 4   (limit 12 / 5; item 00, G-003)
```

and `docs/table-surround-05.png`, regenerated by `scripts/docs_figures.py` from the shared
icons (ANSWERS.md 4; G-016). `tabletop/` outside `gui/`, `tools/`, `SPEC.md`,
`DECISIONS.md`, `RESULTS.md`, `config_defaults.lock`, `problems/`, `results/` and
`tests/data/` have no diff. The gate checks the same on every iteration, runs the engine's
suite with PySide6 unimportable, and checks the four golden fingerprints, the two golden
traces and the defaults lock.

## 5. Determinism over the whole library

`ralph_loops/loop0006/check_determinism.py` (not the gate) runs every one of the 53
problems on seeds 1 and 2 under `standard`, and five problems (`surround-05`,
`blockage-02`, `buridan-01`, `figure-ground-01`, `ch2-fig08`) on seeds 1, 2 and 3 under
`human`, `buridan` and `french-faults`: 151 runs, each compared with a plain `run_problem`
through the GUI's Qt-free controller and through the offscreen `MainWindow`
(`tabletop.gui.headless.run`), on the event records, the draw count and French's report
line. **All 302 comparisons were identical** (420 s; 0 mismatches): 151 runs of 169 to 941
events (`ch2-fig10` seed 2 the shortest, `surround-07` seed 1 the longest), each the
same through the controller and through the window. The gate's own check covers the
four golden cases, `surround-05` seed 3 under `human` and `buridan-01` seed 1 under
`french-faults` on every iteration.

## 6. The engine methods called

`grep -rnE '\.step\(|\brng\b|\.choose\(|run_codelet|\.post\(|\bfinish\(|update_temperature|schedule_find_answer|context\.enter|top_down_postings' tabletop/gui scripts/docs_screenshots.py`
finds, in the GUI's code:

- `world.step()` in `models/run_controller.py` (the worker's loop, the one permitted place)
  and `self.controller.step()` in `main_window.py` (the controller's own step mode);
- `finish(...)` on the GUI's own objects: `RunStats.finish`, `BatchModel.finish`,
  `ReplayChecker.finish` and `StatsView.finish`;
- `from tabletop.rng import run_seeds` in `models/results.py`: the engine's seed
  derivation for a batch, which PANES.md P8 names (`run_problem` on `rng.run_seeds`), a pure
  function of the seed and the count.

Nothing calls `Coderack.choose`, `run_codelet`, `World.post`, `World.finish`,
`update_temperature`, `schedule_find_answer`, `context.enter`, the Slipnet's `update` or
`top_down_postings`, or any codelet, and nothing touches `world.rng`; the draw count comes
from `Result.draws`. The reads beside the snapshot (`correspondences.static_terms` and
`dynamic_terms`, `salience.terms`, `salience.region_order`, `worldview.rivals_of`,
`invariants.check`) are on engine-api.md's whitelist, and four model tests check the draw
count against a plain run after each. The gate's determinism check (12 comparisons) and
section 5 confirm it at run time.

## 7. Functions over 60 lines

None. `tests/gui/test_gui_conventions.py`'s `LONG_FUNCTIONS_JUSTIFIED` is empty, and an
independent scan at the review (code lines apart from the docstring, blank and comment
lines left out) found no function over the limit in `tabletop/gui/`. The long constructors
of the Numbo and Seek-Whence GUIs were split here from the start (`_build`, `_lay_out`,
`_build_menus` and the like).

## 8. The test suites' time

Measured on 2026-10-13 on the loop's machine (32 cores) at a load average of 25 to 38 from
other users' jobs, which ran throughout; a quiet machine takes about half:

| Suite | Tests | Time |
|---|---|---|
| The engine's default suite in place, `python3 -m pytest -q --ignore=tests/gui` (with the GUI's Qt-free model tests) | 825 passed, 1 skipped, 3 deselected | 81 s at load 27 |
| The GUI's default suite, `python3 -m pytest -q tests/gui`, offscreen, before the time cuts of section 9 | 558 passed, 7 deselected | 196 s at load 26 and 292 s at load 25-38, the same tests half an hour apart |
| The whole default suite in place under the gate, after the cuts | 1384 passed, 10 deselected | 260 s at load 25-30 (326 s before the cuts at load 31-37, over the 300 s backstop) |
| The standalone copy, no source variables, PySide6 unimportable (the gate's step 13) | 1182 passed, 37 skipped, 8 deselected | 98 s |
| The whole-library determinism check (section 5) | 302 comparisons | 438 s |

ANSWERS.md 10 gives the GUI's default tests about 45 s serially; they have been over it since
item 13 (G-058 says why: about 35 whole runs through windows with every pane painted per
event, no single test over 10 s). The gate's 300 s backstop for the whole default suite holds
on a quiet machine with a wide margin and under this machine's load with a narrow one; it is
the author's call (PROGRESS.md, "For the author").

## 9. What the review found and changed

- **The guide's words on the sides.** `models/guide.py`'s P1 row and the guide's "What it
  shows" paragraph said "Henry's side below and Eliza's above". The engine's geometry
  (`geometry.py`: y downwards from Henry's edge), the table model (Henry's name at y = 0)
  and the dumps (Fig. 4.2 [F95 p. 98 (img)], "Henry" above the table) all put Henry above,
  and so does the GUI's picture. The two places now say "Henry's side above and Eliza's
  below", and a new test ties the row's words to the table model's name placement (G-059).
- **The log pane and the selection.** PANES.md P1 shares the selection with P10; the log
  pane never followed it (its `attach` takes the selection and ignores it). Recorded as left
  out in G-059: an event row names structures only in its text, and a filter by selection
  would be a new design.
- **The time budget.** The gate at the review failed its 300 s backstop (326 s at load
  average 31 to 37; every other check passed), with the same tests that took 264 s at load
  26 to 34 in item 19. The cuts G-058 left to item 20 were taken: six whole runs through
  the window that stand for any run (the session menu, save-then-open, save-part-way and
  tampered-file tests, the controls' same-seed test, the Ctrl+N run of the run-actions
  test) now take the shortest library run on seeds 1 to 5, `buridan-03` seed 2 (15
  codelets, 190 events), instead of 496 or 806 events. No check was deleted (G-059).
- Everything else checked at the review agrees with the design or has its decision.
