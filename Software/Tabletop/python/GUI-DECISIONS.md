# GUI decisions: where the design leaves a choice open

The choices the GUI of the Tabletop reconstruction (`tabletop/gui/`, loop0006) makes where
its design (the loop's `PANES.md`, `docs/engine-api.md`), the author's answers (the loop's
`ANSWERS.md`, which override everything), the Numbo GUI (the precedent for every convention)
through the Seek-Whence GUI (the closest worked example, a GUI over a frozen reconstruction)
and the sources (French 1995, FCCA chapters 8-9, French & Hofstadter 1991) do not dictate
one, and every place the GUI follows the Numbo or the Seek-Whence GUI rather than PANES.md.
Code that rests on a decision says `G-NNN` in its docstring; `tests/gui/test_gui_conventions.py`
and the loop's gate check that every `G-NNN` in the code has an entry here. Engine decisions
are `D-NNN` in `DECISIONS.md`; the GUI never adds to them.

Each entry has four parts, in this order: **Sources** (what F95, FCCA, the engine's docs,
Numbo, Seek-Whence or ANSWERS.md say, with tags, or that they say nothing), **Choice** (what
the GUI does), **Reason** (why) and **Alternatives** (what else would fit). Numbers are given
in order of creation and never reused. A decision made while a question for the author is
open is marked "pending the author".

## Index

| Decision | What it settles | Item |
|---|---|---|
| G-001 | The Qt-free models live in `tabletop/gui/models/`, not beside the engine | 00 |
| G-002 | The offscreen conftest is `tests/gui/conftest.py`, not `tests/conftest.py` | 00 |
| G-003 | The four permitted edits to the engine's scanning tests, and nothing else | 00 |
| G-004 | pytest-qt is in the `gui` extra, not in `test` | 00 |
| G-005 | The skeleton's command line: the CLI's argument names; `--smoke` opens an empty window until item 16; the tests of what later items build are staged | 00 |
| G-006 | The tap sits behind the controller's gate; the start's snapshot is taken when the `World` is built and recorded under the last event of the start | 01 |
| G-007 | Snapshots after every step while the clock is at most 300, keyframes every 16 steps after, always at the end: the measurement | 01 |
| G-008 | The run controller is Numbo's, with runs for sessions, a step codelet that ends at the step boundary, and a generation on every delivery | 01 |
| G-009 | The determinism contract: `window=True` opens the skeleton's empty window until item 02; the golden runs are the tests' reference | 01 |
| G-010 | The layout precedence: the user's settings, the repository's `default_layout.json`, the built-in arrangement (a function) | 02 |
| G-011 | Placeholders for the panes not built yet; the view protocol; visibility by visible region | 02 |
| G-012 | The built-in dock arrangement is made at the first `showEvent`, never before the window is laid out | 02 |
| G-013 | The layout bytes carry a state version; a state from another version is ignored and said so | 02 |
| G-014 | The window fits 1280 x 800 with every dock shown: explicit minimums from the start | 02 |
| G-015 | The menus of PANES.md section 4 from item 02: later items' actions disabled and named; the next run's inputs held by the window; the Problem menu by family with pages | 02 |
| G-016 | The icons are Qt-free primitives in `models/icon_shapes.py`, painted by `icons.py` and shared with `scripts/docs_figures.py` | 03 |
| G-017 | The table model takes its positions from the run's `Problem` when the run starts, its marks from the events, and the snapshot only when no problem was given; views get `set_position` | 03 |
| G-018 | The table's picture: the names, a faint centre line, the H and E arrows' geometry, a light ring on a noticed object, the hover text | 03 |
| G-019 | The table's zoom, pan and fit are Numbo's `TreeView`'s, on a canvas with a bar; the click selection waits for item 04 | 03 |
| G-020 | The structures come from the events, exact at every index; the snapshot supplies the hover text's numbers; the tap records the correspondences' terms beside each snapshot | 04 |
| G-021 | The structures' picture: group boxes with pads, a wavy border for gestalt groups, proposals as dotted outlines with stage dots, lines between the ends' centres, the flash by the event index's parity, new ends for two codelets, removed structures fading for three | 04 |
| G-022 | One `Selection` for every pane, held by the window (Seek-Whence's); a click toggles the element under the mouse; a group selects its members and a correspondence its ends | 04 |
| G-023 | The hover text of a group, a proposal and a correspondence: the events' facts, the snapshot's numbers "as of codelet N", the terms and the slippages | 04 |
| G-024 | The tap records the salience readings beside each snapshot; the overlays are dated by the snapshot's clock and never rolled back; a drawn region is the notice's, then the snapshot's later look | 05 |
| G-025 | The overlays' picture and toggles: cells with soft bands as wide as the blur, the engine's names, the drawn tint, the ranks, the heat by share; three layers as checkable actions on the bar and in the View menu, remembered under `controls/overlay-*`; every layer off leaves the layout untouched | 05 |
| G-026 | Eliza's view is a pure transform of the layout, (x, y) to (width - x, height - y), not a view transform: icons turned round, names swapped, the regions keeping the engine's names | 05 |
| G-027 | The thermometer is French's column from the events alone: the last reading, grey while held, a reason tick, the scale and the age as the GUI's additions, empty before the first reading, the last update shown at the answer | 06 |
| G-028 | The temperature plot is three bands over the codelets, its geometry a pure function of the model, the size and two options; the terms stacked on the T band; the options as the plot's own actions on its bar and in the View menu, remembered under `controls/plot-*` | 06 |
| G-029 | French's chapter 4 thermometer is a constant in the model, checked against `tests/data/ch4.json` by the tests, and offered for `surround-05` only | 06 |
| G-030 | The Slipnet grid's activations come from the events alone, exact to a tenth at every index; a seventh row for the seven nodes the screen lacks; a jolt outlines its box for one event; a click selects a `node` in the shared selection | 07 |
| G-031 | The box as French's screen drew it: the label along the bottom, the number in the upper right-hand corner as an integer, the black square's side linear in the activation; the geometry a pure function of the model, the size and the options | 07 |
| G-032 | French's nine chapter 4 grids are a constant in the model, checked against `tests/data/ch4.json`, the nearest in codelets shown in red on request for `surround-05` only, remembered under `controls/grid-*`; the hover text's links and effective lengths come from the table and the snapshot in force, dated | 07 |
| G-033 | The network's effective lengths come from the engine's reports: every labeled link starts at its floor length, the intrinsic times (1 - `label_shrink` / 150), and `SlipnetUpdated.lengths` sets the ones that changed; exact to a tenth at every index; the activations through a composed grid model; a label node is highlighted while a link it labels is shorter than at the floor | 08 |
| G-034 | The picture, GUI design after Fig. 3.6: three bands by level, the most abstract on top, each band's nodes in up to three staggered sub-rows, the object band ordered by its labeled pairs; circles with a dark disc linear in the activation, ISA links as thin arrows, labeled links thicker as they shrink with the length beside them, identity links as loops, dashed stems from the label nodes; the geometry a pure function; a cached pixmap | 08 |
| G-035 | The context as the events say it: the reset nodes ringed while inside the subworld, the restored nodes ringed through the pop's codelet, the banner `no context push in this run` until a push, then the push or the last pop with the counts; the push seed `blockage-07` seed 116 under `standard`, found in a bounded search | 08 |
| G-036 | The Coderack's model: the rack from the events, exact by type at every index; the chances recomputed from the event's exponent with the engine's weights; twins alike in all but their posting are told apart by nothing in the events, so the list is re-based on each new snapshot and the events since re-applied | 09 |
| G-037 | The strip as French's screen drew it: the box with a bar and its percent per type in the strip's order, the labels on two staggered lines, the counter on the pane's top row over the strip's right end, the chosen bar outlined from its choice to the next; the legend as a checkable action remembered under `controls/coderack-*`; the Coderack group taller in the built-in layout | 09 |
| G-038 | The rack list, GUI design after the Numbo GUI's coderack view: nine columns, the oldest first; the chosen band with the chance, the exponent and the rack size, tinted at the choice; what the codelet did; a removed codelet greyed at its old position for the one event of its removal; a twin's posting left unsaid; greyscale text | 09 |
| G-039 | The Worldview's entries come from the events through the composed structures model, exact at every index; the numbers (the points, the rating with its three terms and its pairs) from the snapshot in force, dated; the tap records every correspondence's rivals beside each snapshot; a model that has seen no events lists the snapshot's correspondences, so the chapter 4 state can be shown | 10 |
| G-040 | The Worldview pane's picture, GUI design from the text: lines of text by role, a solid bar before a Worldview entry and a dashed one before a shadow entry (Fig. 3.11's convention), an incoherent pair in a warning colour and a rival said in words, the banner in plain words, the Proposals as a second tab of the same pane; the lines widget shared with the competitions pane | 10 |
| G-041 | The competitions from the events alone: one fight per `CompetitionHeld` with the Old and Rival Worldviews rebuilt from `WorldviewChanged`, side by side as two small tables with the old correspondences solid and the newcomer dashed, the ratings, the chance, the exponent and the winner under them; entries without a fight listed as such; `no competition in this run` until one is held; the fight the event shown reports tinted; a click selects the newcomer | 10 |
| G-042 | The trace tab from the engine's own `TextTrace`, every line tagged with its block's first event and the event that printed it; lines after the position greyed, the block tinted, a click seeks to the block; the Events tab Numbo's rows painted whole at every event; Save trace and Save events write the CLI's text byte for byte | 11 |
| G-043 | The timeline as Numbo's with the Seek-Whence GUI's strip: the eight marker kinds of PANES.md P11 with their shapes and colours, a thin strip of T under them, the go-to-codelet box; the count and the markers are the whole history's, the cursor marks the position; refused while live by the window | 11 |
| G-044 | The controls are Numbo's panel with a Qt-free `ControlValues`: the 53 problems by family with the chapter 4 configuration first, the profile, the seed, the read-only cap, seven buttons and the speed slider; the window keeps the next run's inputs and the pane shows the run actually started, whichever way it was started; the inputs round-trip under `controls/*`; a run line along the top is what the pane repaints per event | 12 |
| G-045 | The stats are a function of the events alone (French's report line from `RunEnded`), with the invariants' verdict read by the tap beside each snapshot and shown in red when violated; the outcome of a stopped run from the controller; rows painted on a scroll area | 12 |
| G-046 | The configuration pane's model reads the fields' comments from `config.py` with `tools/knobs_table.py`'s patterns and the decisions' headings from DECISIONS.md; the edits are a dict over the chosen profile, `dataclasses.replace(PROFILES[profile], **edits)`, every value conformed to its field's type, the tables entry by entry; the `profile` field is the selector's; the edits saved as one JSON text under `config/edits` | 13 |
| G-047 | The configuration pane is a table with a widget per kind, the profile's value, the source, the marks (French's faults, the calibrated knobs, where the profile differs from `standard`) and an editor under it; the edits survive a profile change and are marked where they differ from the profile; the window reads the edits only when a run starts; the title and the stats pane mark a started run's edits; a status line is what the pane repaints per event | 13 |
| G-048 | The session file is one header line (the problem by name or as its dict, the profile, the edits, the seed, the version), the engine's JSON lines as the body and an end line with the count and the digest, absent when saved mid-run; the digest is checked on reading; the recorder and the writer share one path | 14 |
| G-049 | Replay re-executes the header through the window's own controller with a checker subscribed after the tap that compares each event's JSON line with the stored one and stops the run at the first difference; the window then warns and loads the stored events into the history without snapshots; replay mode disables the inputs, shows the file's configuration read-only and is left by Close replay, or by choosing a problem | 14 |
| G-050 | The editor's model: a table in the file form of `problems/` with provenance in the GUI's own words, so that `problems.validate` passes it; the touch and the expected answers; positions clamped inside the table; Save never into `problems/`; Load takes the library's files | 15 |
| G-051 | The palette as Figure 2.1's column with the count boxes beside the cells and the screen's buttons in two rows; the editor as its own canvas in the pane (drops, drags, keys, a context menu), the P1 table showing runs only; editing refused while a run is live; an edit line is what the pane repaints per event | 15 |
| G-052 | The results pane runs a batch on its own worker with `run_problem` on `run_seeds(seed, count)`, the CLI's `--batch` seeds, holding the run controller's engine lock so that a batch and a live run exclude each other; the picture is French's chapter 5 layout from `reported.py` and the bench's `compare`, with `other` as RESULTS.md groups it; Run > Batch… raises the pane and starts one; a batch line is what the pane repaints per event | 16 |
| G-053 | The pane-to-pages table is data in `models/guide.py`, read by the window's dock table and by Help > What am I looking at?, which prefers the user guide's own table when the file is there and shows what to expect from the dynamics in a non-modal dialog | 16 |
| G-054 | The command line takes `--set FIELD=VALUE` through the CLI's own `configure`, the inputs given winning over the remembered ones; `--smoke` runs to the end through the window and prints French's report line with the exit codes 0, 1 and 2 | 16 |
| G-055 | The user guide `docs/gui/panes.md` carries the model's pane table verbatim, one section per pane with what it shows, how to read it, its pages and what is the GUI's own, and the expectations with their RESULTS.md sections; Help shows the file's table; the screenshots wait for item 18 | 17 |
| G-056 | The four screenshots are rendered offscreen from the quick start's run through the real window by `scripts/docs_screenshots.py`, run as a module; the front page's "Using the GUI" section and the README's "The GUI" section show them with captions, and `tests/gui/test_gui_readme.py` checks the sections, the pictures and the `--smoke` line | 18 |
| G-057 | Item 18's three permitted engine edits: the front-page pictures test leaves `gui-*.png` to the GUI's test, and the two documents that said no GUI exists now point at it | 18 |
| G-058 | Robustness: the pixel probe over every painting widget, memory flat over five runs, closing joins the run and batch workers, the capped run within a bound with no slowdown, the smallest window with a run, states of another version; the results pane's body scrolls, the line widgets take their lines lazily, the timeline strip's pens are cached; the GUI's test time against its budget; the standalone check | 19 |
| G-059 | The final review: the citation sample's seed, what it found and corrected (the guide's P1 row had the sides reversed), the one designed interaction left out (the log pane does not follow the selection), the checks and the suites' times | 20 |
| G-060 | The roles pane (P14): the tags by family from the four role events alone, each with its evidence, readers and, once lost, when and why; the Worldview's solid-against-dashed bars (the dashed pen's cap made flat) and the event shown's tint; the dock tabbed with the Worldview | roles 14 |
| G-061 | The table's roles layer, a fourth overlay: badges with the registry's codes, the midline band and the axis, the gaps, the images of the selected or touched object, the re-cast's tiers, the barriers' claims, a ring on a role-proposed correspondence (also in the Worldview pane); the overlay items repaint only their own changes | roles 14 |
| G-062 | The roles in the grid (the roles' band under its own caption), the network (violet role nodes and links), the timeline (a violet dot per role change) and the log (the engine's `RL` lines and role events, nothing added) | roles 14 |
| G-063 | The engine in the window: a permanent note at the right of the status bar and a line in Help > About; without a build the GUI refuses to start in a dialog (on stderr alone under `--smoke`), exit status 1; the title unchanged | fast engine 06 |
| G-064 | The table shows the next run's problem before Play, bare, whenever the problem chosen differs from the one drawn and no run is live | author 2026-10-17 |

## Foundations (item 00)

### G-001: The Qt-free models live in tabletop/gui/models/, not beside the engine
- **Sources:** the Numbo GUI keeps its Qt-free models in a package beside the engine,
  `numbo/models/`, and its views in `numbo/gui/` (Numbo GUI, `numbo/models/__init__.py`).
  The Seek-Whence GUI, whose engine is frozen as this one is, put them one level down, in
  `seekwhence/gui/models/` (Seek-Whence GUI, its G-001). ANSWERS.md 1 names
  `tabletop/gui/models/` outright, and `docs/engine-api.md` ("For loop0006") asks for
  "Qt-free view models in `gui/models/`, each tested without Qt". French's program had one
  screen and no separate display code to copy [F95 pp. 95-111].
- **Choice:** the models are the subpackage `tabletop/gui/models/` (the tap, the snapshot
  history, the event log, the run controller, the run statistics, the session file, the
  presets, the configuration table, the layout file, the guide, the icon shapes and one
  model per pane). They import the engine and nothing from Qt; the views in `tabletop/gui/`
  import them. `pyproject.toml` lists both packages, and
  `tests/gui/test_gui_conventions.py` fails on any Qt import under `models/`.
- **Reason:** with the GUI inside the engine package, a sibling `tabletop/models/` would be
  an engine folder, and the engine is frozen (ANSWERS.md 6): everything the GUI adds must
  stay under `gui/`, where the engine's scanning tests skip it (G-003) and the gate's frozen
  check allows it. Numbo's separation of models from views is kept one level down; the
  models are still Qt-free and tested with plain pytest, which is what makes the pane logic
  testable in the engine's suite without PySide6.
- **Alternatives:** `tabletop/models/` beside the engine, as Numbo's (rejected by
  ANSWERS.md 1 and the frozen engine); a separate top-level package `tabletop_gui`
  (rejected by ANSWERS.md 1).

### G-002: The offscreen conftest is tests/gui/conftest.py, not tests/conftest.py
- **Sources:** the Numbo GUI sets `QT_QPA_PLATFORM=offscreen` by `setdefault` in
  `tests/conftest.py`, shared by its whole suite (Numbo GUI, `tests/conftest.py`), and keeps
  its `thread_errors` and `window` fixtures in `tests/gui/test_gui_shell.py`. The Seek-Whence
  GUI moved both into `tests/gui/conftest.py` (Seek-Whence GUI, its G-002). TASK.md says the
  engine's `tests/` folder gets no new file, and ANSWERS.md 10 says the setting lives in
  `tests/gui/conftest.py`.
- **Choice:** `tests/gui/conftest.py` sets offscreen by `setdefault` and holds the fixtures
  every GUI test shares: Numbo's `thread_errors` and `window(qtbot, thread_errors)`, a
  module-scoped `module_window(qapp)` that shares one window between a module's tests for
  speed (ANSWERS.md 10), and `ini`, a temporary INI path for the settings tests. The window
  fixtures import `tabletop.gui.main_window` only when used, so the engine's suite loads the
  conftest without PySide6; they work from item 02. `run-tests.sh` exports the same setting
  and unsets `WAYLAND_DISPLAY`, so a shell run is offscreen too.
- **Reason:** the engine's test folder is frozen but for the four edits of G-003, and the
  GUI's tests are the only ones that need Qt. A module-scoped window cannot depend on the
  function-scoped `qtbot`, so it takes pytest-qt's session `qapp`, shows the window itself
  and installs the thread hook for the module by hand.
- **Alternatives:** `tests/conftest.py` as Numbo's (rejected: a new engine file); the
  fixtures repeated in each test file (rejected: Seek-Whence's suite showed how many files
  need them).

### G-003: The four permitted edits to the engine's scanning tests, and nothing else
- **Sources:** ANSWERS.md 6 freezes the engine at the commit in `BASELINE` and lists the
  exceptions the author grants for item 00: `tests/test_package.py`,
  `tests/test_conventions.py`, `tests/test_standalone.py` and the module-map test of
  `tests/test_readme.py`, each within the line limits of the gate's `PERMITTED_EDITS`
  (8/5, 4/2, 12/5 and 12/4 lines added/deleted, cumulative since the baseline, every added
  code line about the GUI). These four tests scan the whole package or the whole folder,
  and without the edits the first module under `tabletop/gui/` breaks the engine's suite:
  its docstrings cite precedents and `G-NNN`s, which the engine's conventions test does not
  accept; its views import PySide6, which the package test forbids and the standalone test
  allows only in `scripts/docs_figures.py`; and its modules are not in the engine README's
  module map. Numbo and Seek-Whence had no such freeze.
- **Choice:** the edits made in item 00 (3/3, 2/1, 10/4 and 2/1 lines added/deleted):
  - `tests/test_package.py`: `modules()` leaves out `tabletop/gui/` (one comment, one line);
    `test_no_gui_code` loses its `assert not (PACKAGE / "gui").exists()` and its docstring
    becomes "No GUI toolkit is imported outside `tabletop/gui/`". The GUI toolkit check on
    the engine's modules stays.
  - `tests/test_conventions.py`: `modules()` leaves out `tabletop/gui/`; the GUI's own
    `tests/gui/test_gui_conventions.py` takes its place there, with the wider set of
    citations TASK.md allows (source tags, `D-NNN`, `G-NNN`, "Numbo GUI", "Seek-Whence GUI").
  - `tests/test_standalone.py`: the single exception for `scripts/docs_figures.py` becomes
    `QT_MODULES` (PySide6, shiboken6, pytestqt) allowed under the three
    `GUI_CODE_PLACES` (`tabletop/gui/`, `tests/gui/`, `scripts/`), through a three-line
    `in_gui_code()`; `docs_figures.py` falls under `scripts/`.
  - `tests/test_readme.py`: `test_the_module_map_names_every_module` leaves out
    `tabletop/gui/`, whose own `README.md` is its module map;
    `tests/gui/test_gui_conventions.py` checks that map names every GUI module. The
    front-page pictures test is left to item 18.

  Nothing else under the engine changes. Any further need goes under "For the author".
- **Reason:** the smallest edits that let the GUI exist without weakening a check: each
  check the engine loses over `tabletop/gui/` is taken up, as strictly or more, by the GUI's
  own tests and by the gate (which also scans the GUI's docstrings and imports).
- **Alternatives:** a GUI outside the package (rejected by ANSWERS.md 1); making the
  engine's tests accept `G-NNN` and precedents everywhere (rejected: it would weaken the
  engine's own rule); `tabletop/gui/` added to the engine README's module map (rejected: the
  map would then change with every GUI item, and ANSWERS.md 12 gives the README one row
  pointing at the GUI's map instead).

### G-004: pytest-qt is in the gui extra, not in test
- **Sources:** Numbo and Seek-Whence put `pytest-qt` in the `test` extra (Numbo GUI,
  `pyproject.toml`; Seek-Whence GUI, `pyproject.toml`). ANSWERS.md 1 puts it in `gui` here,
  with the reason: pytest-qt refuses to start when no Qt binding is installed.
  `tools/standalone_check.sh` step 5 installs `.[test]` in a fresh virtual environment
  without PySide6 and runs the suite; the gate's standalone check does the same with
  PySide6 made unimportable.
- **Choice:** `gui = ["PySide6>=6.6", "pytest-qt>=4.5"]` and
  `test = ["pytest>=7.4", "pytest-xdist>=3.0"]` (unchanged); `docs = ["PySide6>=6.6"]`
  stays for `scripts/docs_figures.py`. `pip install -e '.[test]'` runs the engine's tests
  and skips the GUI's; `pip install -e '.[gui,test]'` runs everything.
  `tests/gui/test_packaging.py` pins the three lists.
- **Reason:** a reader who installs `.[test]` without PySide6 would otherwise get a test
  suite that cannot start (pytest-qt's usage error comes before any test is collected); with
  pytest-qt in `gui`, the engine's promise that its tests need only pytest holds. The GUI's
  Qt tests already `importorskip("pytestqt")`, so they skip either way.
- **Alternatives:** Numbo's placement (rejected by ANSWERS.md 1); a fourth extra `gui-test`
  (rejected: one more name for a reader to learn, for no gain).

### G-005: The skeleton's command line, its smoke run, and the staged tests
- **Sources:** ANSWERS.md 1: `python3 -m tabletop.gui` and `tabletop-gui`;
  `--smoke --problem NAME --seed N` runs offscreen and prints French's report line, the
  same last line as `python3 -m tabletop NAME --seed N --quiet`, exit 0 with an answer, 1
  capped, 2 on a usage error; without PySide6 Numbo's message and exit 1. Iterations.md
  item 00: `--smoke` "opens an empty `QMainWindow` offscreen and exits 0 (item 16 completes
  it)". Numbo's `app.py` names its arguments after its CLI (`--puzzle`, `--seed`,
  `--max-iterations`), Seek-Whence's after its CLI (`--preset`, `--terms`, `--seed`,
  `--answers`). The engine's CLI takes a problem name, `--seed`, `--profile` and `--set`
  (`tabletop/__main__.py`). Item 00's test list names `tabletop.gui.headless` and a
  `window` fixture, which items 01 and 02 build; the gate's determinism check starts the
  moment `tabletop/gui/headless.py` exists.
- **Choice:** `app.py` follows Numbo's shape (`parser`, `use_pyside_plugins`,
  `quiet_offscreen_warnings`, `run`, `main`) with the CLI's names: `--problem NAME`,
  `--seed N`, `--profile NAME` (one of `PROFILES`), `--play`, `--smoke`. `run()` checks
  PySide6 before parsing, as Numbo's does, and prints
  "the GUI needs PySide6: pip install -e '.[gui]'" to stderr with status 1. Until item 16,
  `--smoke` opens an empty `QMainWindow` titled "Tabletop" offscreen with the offscreen
  warnings dropped, prints one line beginning `smoke:` that says the run arrives with item
  16, and exits 0; outside a smoke run, the empty window is shown and the event loop runs.
  The tests of what later items build are staged: `tests/gui/test_packaging.py` imports
  `tabletop.gui.headless` under the PySide6 stub only once the file exists (item 01 writes
  it; writing a stub now would start the gate's determinism check with nothing to drive
  it); the `window` and `module_window` fixtures import `main_window.py` lazily (item 02);
  the test that the GUI's tests skip without PySide6 is a collection-only run
  (`pytest --co`), because the gate already runs the whole suite in a standalone copy with
  PySide6 unimportable, and a full second run of `tests/gui/` in a subprocess would cost the
  budget of ANSWERS.md 10 twice.
- **Reason:** the argument names make `python3 -m tabletop.gui --problem surround-05
  --seed 3 --smoke` and `python3 -m tabletop surround-05 --seed 3 --quiet` read as the same
  run, which is what ANSWERS.md 1 promises their last lines are. A smoke run that says
  plainly what it does not yet do is more honest than one that prints nothing.
- **Alternatives:** a positional problem name as the CLI's (rejected: Numbo's and
  Seek-Whence's GUIs take every input as an option, so that none is required and the
  remembered inputs can serve); parsing before the PySide6 check so that `--help` works
  without it (rejected: Numbo's order, and the message tells the reader what to do first);
  a silent `--smoke` until item 16 (rejected, above).

## The tap, the history, the log and the run controller (item 01)

### G-006: The tap sits behind the controller's gate; the start's snapshot is taken when the World is built and recorded under the last event of the start
- **Sources:** PANES.md §1 asks for `tap.py`, "an observer that appends every event and
  takes `World.snapshot()` at chosen moments: at `RunStarted`, after each `World.step()`
  (the controller knows when a step ends), and at `RunEnded`", and TASK.md's engine
  contract 2 says the tap "may read the world when an event arrives, and between steps,
  but only through `World.snapshot()`"; its contract 1 writes the observers as
  `observers=[tap, ..., gate]`. The Numbo GUI feeds its history from the events the gate
  has delivered (Numbo GUI, `main_window.py`, `_on_delivered`), and the Seek-Whence GUI
  subscribes its tap to the controller, behind the gate, and gives it the world through a
  callback once the session is built (Seek-Whence GUI, its G-004). `RunStarted` is
  published inside `World.__init__`, before the Coderack exists (`tabletop/world.py`), so
  `World.snapshot()` cannot be taken at that event.
- **Choice:** the `World` is built with the gate as its only observer, and the tap is a
  controller observer, published to by the gate once the event has passed the pause
  (`RunController(observers=[tap], on_world=tap.attach, on_step=tap.after_step)`, which
  is how `headless.run` wires it). The worker calls `on_world(world)` right after the
  `World` is built: the tap takes the start's snapshot (clock 0, the starting rack loaded,
  the Slipnet's first update done, the touched object noticed, the first temperature) and
  records it under the last event published so far, the last event of the start. After
  each `World.step()` the worker calls `on_step(world)` and the tap records the step's
  snapshot under the step's last event when the rule of G-007 says so. `state_at(index)`
  is the last snapshot at or before the index, and None for the events of the start before
  its snapshot. The tap lets the world go when the run has finished and keeps no other
  engine object. `EventLog.record` keeps `record_of(event)` beside every event, so the
  session file and the determinism contract read what the log holds.
- **Reason:** behind the gate, the log holds exactly the events the GUI has been handed:
  while the gate is paused with an event in hand, the timeline, the log view and the
  determinism contract agree with what is drawn. A tap subscribed to the `World` directly
  would be one event ahead whenever the run is paused. For the records, the draws and the
  report the two wirings give the same result, which is what the gate checks. The start's
  state cannot be read at `RunStarted` itself; reading it when the constructor has returned
  gives the state French's screen shows at the start, with the rack "loaded up with its
  standard set of starting codelets" [F95 p. 97], and recording it under the last event of
  the start keeps `state_at` truthful (a snapshot under index 0 would already contain the
  postings of events 1 to n).
- **Alternatives:** the tap as a direct observer of the `World`, first in its list
  (rejected: the log would run ahead of the display while paused); the start's snapshot
  under index 0 (rejected: it would describe a later state than the event it sits under);
  a snapshot rebuilt from the start's events (rejected: the engine's snapshot is the only
  copy of the state the GUI is allowed, and it is exact).

### G-007: Snapshots after every step while the clock is at most 300, keyframes every 16 steps after, always at the end: the measurement
- **Sources:** PANES.md §1: "Runs are short (50-90 codelets, 400-800 events; a capped run
  is 3000 codelets), so a snapshot per codelet is affordable. Measure it in item 01 on a
  capped run, and fall back to keyframes as Numbo's `layout_history.py` does if it costs
  more than twice the engine's time or 200 MB." Iterations.md item 01 allows the cap to be
  reached by lowering `max_codelets` with `dataclasses.replace` for the measurement only.
  The Numbo GUI keeps a checkpoint every `EVERY` (64) events and rebuilds the rest from
  the events (Numbo GUI, `layout_history.py`); the Seek-Whence GUI takes a snapshot at
  every task while the clock is at most 500 and keyframes after (Seek-Whence GUI, its
  G-003). RESULTS.md: under `standard`, 44 to 67 codelets on average per configuration and
  0 capped runs in 5,000.
- **Choice:** `SnapshotHistory.wants_after_step(clock, finished)`: a snapshot after every
  step while the clock is at most `DENSE_CODELETS` (300), after that one every
  `KEYFRAME_STEPS` (16) steps, and always at the end of the run; plus the start's snapshot
  of G-006. The two constants are arguments of `SnapshotHistory`, so a test lowers them to
  force keyframes on a short run. A pane model shown between two keyframes starts from the
  earlier one and applies the events after it (the pane items).
- **Reason:** measured on 2026-10-13 (item 01) on `surround-05` seed 1. No run in the
  library's first twelve problems on seeds 1 to 5 reaches the cap, and RESULTS.md reports
  none in 5,000, so the cap was forced by switching the find-answer postings off
  (`answer_post_rate=0`, for the measurement only; lowering `max_codelets` would give a
  short run, not a long one): 3000 codelets, 22,764 events (22,773 since the roles substrate,
  D-079, 23,496 since the geometric family, D-082 to D-084, 23,646 since the content
  roles and the role factor, D-087 to D-090 and D-098, and 24,638 since the selector roles,
  the re-cast and consumer C2, D-085, D-093 and D-096, 2026-10-14, and 24,389 since the GAP,
  LONER, STRAY and CORNER detectors, the midline on loners, consumer C1's loner, corner and
  part pairs and C7's role term, D-086 and D-100 (item 08), 2026-10-14, counted through the
  window offscreen in 149.8 s, and 24,291 since the FIGURE, GROUND, ECHO, TWIN and TYPE_KEY
  detectors, consumer C1's figure, ground, echo and key pairs, C5's key identity, the
  re-cast's TWIN tier and find-roles after a built group, D-091, D-092 and D-094 (item 09),
  2026-10-14, counted through the window offscreen in 189.4 s, and 25,753 since the BARRIER
  and CLAIMED roles (read 27 times in this run), consumer C4's fights for the barrier's
  correspondence and the re-cast's CLAIMED tier, D-089 and D-101 (item 10), 2026-10-14,
  counted through the window offscreen in 209.2 s, and 24,744 since CONTAINER's setting
  and holder forms, consumer C3 and examine-cp's anchor pair, D-087 and D-097 (item 06,
  reopened), 2026-10-14, counted on the plain run, which the window's run equals, and
  30,692 since v5's structural Blockage fix (the Hobj points in every rating, the win bonus
  and the loss penalty once per rivalry, `proximity_zero_gap` bounded, a BARRIER's discard
  cascading to its CLAIMED tag, the time pressure `answer_patience`), D-116 to D-120,
  2026-10-17, counted on the plain run; the tests pin the current count). Wall clock: the engine alone
  3.9 s; through the controller and the tap with a snapshot after every step 16.5 s
  (4.2x); with the rule chosen 5.0 s (1.27x; 470 snapshots). Peak memory, measured with
  `tracemalloc` (which slows every run alike; under it the three times were 20.5 s,
  45.4 s and 26.2 s): the engine alone 0.6 MB; a snapshot per step 382 MB (3,001
  snapshots, about 120 KB each late in the run: the saliences, the correspondences and
  the Slipnet's links grow with the structures); the rule 59 MB. Both budgets rule out a
  snapshot per step on a capped run, and the rule meets both. On the normal run (81
  codelets, 806 events; the engine alone 0.05 s) a snapshot per step costs 0.09 s (1.8x)
  and 4.6 MB peak, and the rule changes nothing there: every run of 300 codelets or
  fewer, which is nearly every run, is exact at every codelet, so a seek shows what the
  live run showed.
- **Alternatives:** a snapshot per step (rejected by both budgets on a capped run); a
  keyframe every 16 steps from the start (rejected: the normal runs would lose the exact
  seek for no gain); sharing unchanged sections between snapshots as Seek-Whence's does
  (rejected for now: the engine's snapshot is built whole and read-only, and comparing
  sections would cost about what it saves; open if a pane needs denser keyframes).

### G-008: The run controller is Numbo's, with runs for sessions, a step codelet that ends at the step boundary, and a generation on every delivery
- **Sources:** ANSWERS.md 5 and TASK.md's engine contract 3 adopt the Numbo GUI's
  `RunController` (`numbo/models/run_controller.py`) through the Seek-Whence GUI's: one
  worker, the gate subscribed last with its pause, delivery, acknowledgment and delay, the
  modes paused, playing, step, step codelet and run to end, a stop by an exception,
  generation numbers, one worker at a time. Numbo's modes are paused, playing, step,
  step-iteration and run-to-end, and its `Delivery` has a ticket, an index and an event;
  the generation lives in its window. PANES.md §1: "Step codelet = the rest of the current
  `World.step()`". The engine's run loop is `run_problem`'s: `World(problem, config,
  seed, observers)` stepped until `finished` (`tabletop/run.py`, D-066).
- **Choice:** `RunController.start_run(problem, config, seed, paused=)` checks its inputs
  on the caller's thread (`check_inputs`: a `Problem` or a problem name, loaded at once so
  that a bad name raises before the worker starts; a `Config`; an integer seed) and the
  worker runs the engine's own loop with the gate as the world's only observer. The modes
  are `step` (one event), `step-codelet`, `playing` and `run-to-end`; `start_events`
  replays recorded events as Numbo's does. A step codelet is ended by the worker between
  two steps (`_end_of_step`: the mode becomes paused once `World.step()` has returned and
  the tap has taken its snapshot), so the gate pauses before the first event of the next
  codelet, exactly at a step boundary; the start counts as a boundary, so a step codelet
  from the beginning shows the start and stops before the first codelet. Every start has
  a new `generation`, carried by every `Delivery` (a fourth field) and by the `RunOutcome`,
  so that the window of item 02 can drop a stopped run's late deliveries. The outcome's
  `result` is the engine's `Result` (`result_of`, plain data). The controller has its own
  `Subject` and `ObserverError`, as Numbo's `observe.py` has, since the engine's publisher
  is the `World`'s and a callback's error has no event. The engine lock (`EngineBusy`)
  is kept although every `World` is independent, so that one run is live at a time, as
  the timeline's seeking rule assumes. The callbacks `on_world` and `on_step` are new
  (G-006).
- **Reason:** ANSWERS.md 5: every convention follows Numbo; the names change only where
  Tabletop's run differs in kind (a problem under a profile, paced by codelets). The step
  boundary cannot be told from the events alone (a step's last event may be a posting, a
  temperature update or a Slipnet update, and the next step begins with the removals of
  aged codelets), so the worker, which calls `World.step()`, is the one that knows it.
- **Alternatives:** step-iteration kept under its name (rejected: nothing in Tabletop is
  an iteration); the boundary detected inside the gate from the next `CodeletChosen`
  (rejected: the aged-out removals of the next step would already have passed); the
  generation outside `Delivery` as Numbo's (rejected: the controller's consumers, the
  window and the tests, need it with the event).

### G-009: The determinism contract: window=True opens the skeleton's empty window until item 02; the golden runs are the tests' reference
- **Sources:** TASK.md's determinism contract: `tabletop.gui.headless.run(problem, config,
  seed, window=False) -> dict` with `records`, `draws` and `report`; with `window=True` it
  builds the real `MainWindow` offscreen. The loop's gate compares both paths on six
  cases as soon as `headless.py` exists, and item 02 builds `main_window.py`. The
  Seek-Whence loop met the same gap: its item 00 ran `window=True` with an empty window
  open offscreen until its item 02 routed it through the window's controller (loop0004,
  PROGRESS.md items 00 and 01). The Numbo GUI's controller tests compare every run through
  the gate with a plain run (Numbo GUI, `test_run_controller.py`'s `reference`); the
  Seek-Whence GUI keeps its plain reference runs in `tests/gui/golden.py`.
- **Choice:** `headless.run` with `window=False` goes through `Tap`, `EventLog` and the
  Qt-free `RunController` (`run_through_controller`: `start_run`, `run_to_end`, `wait`),
  and returns the log's records, `Result.draws` and `report_line(result)`; it imports no
  Qt. With `window=True`, until item 02, it opens the skeleton's empty `QMainWindow`
  offscreen (the shape of item 00's `--smoke`) and records the run through the same
  Qt-free controller while the window is open. Item 02 replaced that: `window=True` now
  builds `MainWindow(settings=None)` offscreen with the package's default layout, starts
  the run with `start_run`, calls `run_to_end()`, turns the Qt event loop until the
  window's `run_finished` has come, and returns the window's history (G-010 to G-015). The GUI's tests take their reference from
  `tests/gui/golden.py`: the four golden cases of `tests/data/golden/fingerprints.json`
  run plainly once per session (`golden_runs`), each with its events, records, draws,
  `Result` and report line.
- **Reason:** the gate runs both paths the moment the file exists, so `window=True` must
  answer truthfully from item 01 on; running the Qt-free path under an empty window is
  what the skeleton can do, and the docstring says so. The golden cases are the gate's own
  cases, so the tests and the gate check the same runs.
- **Alternatives:** `window=True` raising until item 02 (rejected: the gate would fail
  for an item that cannot build a window); a `headless.py` written only in item 02
  (rejected: the gate's controller comparison would stay off for an item).

## The main window (item 02)

### G-010: The layout precedence: the user's settings, the repository's `default_layout.json`, the built-in arrangement (a function)
- **Sources:** ANSWERS.md 2: QDockWidgets, one per pane; `QSettings("tabletop",
  "tabletop-gui")` restores the geometry and state; a default layout that travels with the
  code, `tabletop/gui/default_layout.json`, holding `saveState()` as base64 beside a
  readable record, no geometry, a format version and the Qt version; the precedence
  settings, file, built-in, with fallbacks that never crash and a status-bar line; the
  three View actions (Save layout as default for this code…, Reset layout, Reset to
  built-in layout); a shipped file equal to the built-in. PANES.md section 3 gives the
  arrangement: French's screen as the starting point [F95 pp. 95-97], the rest in tabs.
  The Numbo GUI restores `window/state` from QSettings and has no reset; the Seek-Whence
  GUI did all of this (its decision 10), and its loop's answers are the author's again.
- **Choice:** the built-in arrangement is a function, `_arrange_builtin`, that puts every
  dock back in its area, makes the three splits of French's screen (the thermometer to
  the right of the table and the Slipnet grid to the right of the thermometer; the
  Worldview group under the Slipnet; the Coderack above the trace), rebuilds the four tab
  groups, sets the default visibility (P13 hidden) and the sizes; it is the last fallback.
  The repository file is read and written by the Qt-free `models/layout_file.py`
  (`LayoutRecord`, `encode`, `decode`, `read_layout`, `write_layout`, `missing_docks`;
  format `tabletop-gui-layout`, version 1, under 20 KB; the Seek-Whence GUI's codec with
  the format renamed). `MainWindow(layout_path=)` names the file (the package's by
  default; None for none, as the tests' `window` fixture). On open: the settings' state if
  it restores; else the file if it reads, names only known docks and restores; else the
  built-in, with a status-bar line saying why. Reset layout re-arranges the built-in,
  applies the file over it, and removes `window/state` from the settings; Reset to built-in
  layout stops at the first step. `layout_source` says which is in force (`settings`,
  `default file`, `built-in`). The shipped file was written offscreen at 1500 x 950 from
  the built-in arrangement by `save_layout_as_default`; it is rewritten whenever the docks
  change (G-013).
- **Reason:** a stored byte blob of the built-in would depend on the window's size when it
  was captured; a function is the same on every machine and is what the file is checked
  against. Applying the file over a fresh built-in arrangement means a partial
  `restoreState` cannot leave a dock stranded. The codec is copied rather than imported
  from Seek-Whence because nothing may reach outside the folder (ANSWERS.md 9).
- **Alternatives:** the built-in as a `saveState()` captured at construction (rejected:
  size-dependent, opaque); the default file read by Qt alone with no readable record
  (rejected by ANSWERS.md 2); the Coderack and the trace side by side at the bottom
  (rejected: PANES.md section 3 lists them top to bottom, as French's screen has the
  Coderack strip under the table).

### G-011: Placeholders for the panes not built yet; the view protocol; visibility by visible region
- **Sources:** item 02 lays the window out before any pane exists; the Numbo GUI's views
  are observers with `on_event` on the GUI thread and `redraw(now=False)`, counted by a
  `paints` attribute in the tests (Numbo GUI, `main_window.py`'s `add_view`,
  `tree_view.py`, `test_polish.py`'s probe). ANSWERS.md 4: the panes PANES.md marks as new
  design carry the label "GUI design, not French's" in the pane, its tooltip and the
  guide. The Seek-Whence GUI did the same (its decision 11). TASK.md's file list has no
  `placeholder.py`.
- **Choice:** `placeholder.py`'s `PanePlaceholder(title, pane_id, pages, narrow=False)` is
  a QWidget that paints its pane id, its name, its pages and the number of events it has
  seen, counts its paints, and resets at `RunStarted`; the central widget, the timeline
  toolbar's widget and the fourteen docks hold one each, with the pages PANES.md gives;
  the five new designs (P3b, P5, P6, P2b, P4b) and the editor half of P7 say "GUI design,
  not French's" in the pane and its tooltip from the start. The module is one file beyond
  TASK.md's list, named in the GUI's README. Each real view replaces its placeholder in
  its own item and keeps the protocol. The tests judge whether a view is on screen by its
  `visibleRegion()`: in Qt 6 a dock tabbed under another is `isVisible()` yet has no
  pixels and gets no paint.
- **Reason:** the window's handshake (every view drawn once per event, then the
  acknowledgment) can be tested from item 02 on, with the same probe the real views will
  face; a placeholder that paints its own text (no QLabel) keeps the count at exactly one
  paint per event.
- **Alternatives:** empty QLabels (rejected: a label's paint count is not the view's);
  no docks until their items (rejected: the layout, the menus and the layout file must
  exist now, and every dock name is in the shipped file).

### G-012: The built-in dock arrangement is made at the first `showEvent`, never before the window is laid out
- **Sources:** TASK.md "Lessons from Seek-Whence's GUI" and ANSWERS.md 2: a stray dock
  title over the menu bar and a tab group shown twice, seen in a Seek-Whence screenshot;
  item 02 tests both. The Seek-Whence GUI's decision 52 found the cause: with tab groups
  tabified before the window is shown, Qt 6.11 keeps stale `QTabBar`s in use, one at the
  origin (over the menu bar) and one with the old geometry. Numbo GUI's `showEvent` sizes
  its docks only once the window is laid out, for the same kind of reason.
- **Choice:** `_arrange_builtin` before the first show only sets each dock's visibility
  and marks the arrangement pending; `showEvent` makes it (`splitDockWidget`,
  `tabifyDockWidget`, `resizeDocks`) the first time the window is shown. A layout
  restored from the settings or the default file before the show cancels the pending
  arrangement, since it replaces the built-in. After the show, Reset layout and Reset to
  built-in layout arrange at once. `test_main_window.py` checks that nothing but the menu
  bar occupies the menu bar's rectangle (down to the pixels, and the Session menu opens on
  a click there), and that each of the four tab groups has exactly one visible tab bar in
  the built-in layout, the default file and a restored state, also after both resets.
  The tests let the dock layout settle (20 ms) after showing a window before counting
  tab bars.
- **Reason:** the lesson was learned late in Seek-Whence's loop (its iteration 17); here
  it is built in from the start and tested from the start, so that no later item can
  reintroduce it.
- **Alternatives:** arranging in `__init__` and hiding stale bars by hand (rejected:
  fragile, and the Seek-Whence GUI found the show-time arrangement sufficient).

### G-013: The layout bytes carry a state version; a state from another version is ignored and said so
- **Sources:** iterations.md item 02: a QSettings state version, as the Seek-Whence GUI's
  decision 59 has it. ANSWERS.md 2: a layout that fails to restore falls back to the next level and
  says so in the status bar; never crash. Numbo's `main_window.py` calls `saveState()`
  and `restoreState(bytes)` with Qt's default version 0. `QMainWindow.restoreState(state,
  version)` refuses bytes whose version is not the one given.
- **Choice:** `STATE_VERSION = 1`; `saveState(STATE_VERSION)` for the QSettings
  `window/state` and for the default layout file's `state` (`layout_record`), and
  `restoreState(..., STATE_VERSION)` in both places. A refused state leaves the built-in
  arrangement pending, puts "the saved layout is from another version of the window and
  was ignored" (or, for the file, "could not be restored (saved by another version of the
  window?)") in the status bar, and goes on to the next layout of the precedence; a
  `window/state` that is not bytes is ignored the same way. Closing saves the current
  version over the old one. The version is bumped by hand when the docks change in a way
  an old state would misplace (a dock renamed, a tab group reorganised); the shipped
  `default_layout.json` is regenerated then. Tested with a state saved under
  `STATE_VERSION + 1`.
- **Reason:** the loop adds panes item by item; a state saved by a window with other
  docks would otherwise restore half-right without a word.
- **Alternatives:** Qt's version 0 (rejected: no way to tell an old state); checking the
  dock names before restoring (done too, for the file's readable record, by
  `missing_docks`).

### G-014: The window fits 1280 x 800 with every dock shown: explicit minimums from the start
- **Sources:** iterations.md item 02: "small minimum sizes from the start, so that the
  window fits 1280 x 800 with every dock shown"; TASK.md's lesson from Seek-Whence, whose
  window opened at 2224 x 1436 until its robustness item (its decision 58). PANES.md section 3
  says the same. Numbo's `main_window.py` says nothing of a smallest size.
- **Choice:** `SMALLEST_SIZE = (1280, 800)` in `main_window.py`, and every placeholder has
  an explicit minimum of 120 x 60 (the thermometer's and the timeline's 56 x 60, the
  timeline at most 60 high), which overrides Qt's size hint. `test_main_window.py` shows
  every dock (the hidden Configuration dock comes up in its tab group), resizes the window
  to 1280 x 800 and checks that `minimumSizeHint()` fits and that every dock is on the
  window or parked under a tab. Each pane item keeps the test green by giving its lists
  and long labels a small minimum (a list 40 x 24, a long label a minimum width of 1 so
  that it clips).
- **Reason:** the window's minimum is the sum of its columns' minimums; one wide widget
  in any dock pushes it past a laptop screen. Testing it from item 02 catches the pane
  that breaks it in the item that adds it.
- **Alternatives:** fixing the size at the end (rejected: Seek-Whence's lesson).

### G-015: The menus of PANES.md section 4 from item 02: later items' actions disabled and named; the next run's inputs held by the window; the Problem menu by family with pages
- **Sources:** PANES.md section 4 lists six menus (Session, Edit, Run, Problem, View,
  Help) with Numbo's keys (ANSWERS.md 5); iterations.md item 02 asks for "the menus of
  PANES.md section 4, with every dock's toggleViewAction()". The session file (item 17),
  the batch (item 16), the overlays and Eliza's view (item 04), the controls pane (item
  15) and the guide from `docs/gui/panes.md` (item 16) do not exist yet. PANES.md P9: the
  problem selector lists every problem by family with its title and page, `surround-05`
  first; whichever way a run is started, the title agrees with the run. The Seek-Whence
  GUI disabled the actions of later items and said which item.
- **Choice:** every menu and action of section 4 exists from item 02. Actions whose item
  has not come are disabled, with the status tip "comes with item NN of the GUI loop"
  (`_later`): Open session…, Save session…, Close replay, Save trace…, Save events as JSON
  lines… (17), Batch… (16), Table overlays and Eliza's view (04). The next run's inputs
  are three plain attributes of the window, `problem`, `profile` and `seed`, set by
  `set_next_run` (validated: a problem of the library, a profile of `PROFILES`), by the
  Problem menu (five family submenus, each problem as "Surround 5 (surround-05, p. 118)"
  from `Problem.title` and `source["page"]`, checkable, the chosen one checked), by the
  command line's `--problem`, `--profile`, `--seed`, and remembered under
  `controls/problem`, `controls/profile`, `controls/seed` and `controls/speed`; the
  configuration pane's edits come with item 15 (`next_config` is the profile's `Config`
  until then). The title is `title_for(problem, profile, seed)`: "Tabletop — surround-05,
  seed 3", with the profile in parentheses when it is not `standard`; `started` records
  what the run shown was started with, and the title follows the run, not the next-run
  inputs, once a run has started. Run > Speed is a submenu of radio actions over Numbo's
  `DELAYS` (the controls pane's slider comes with item 15). Help > What am I looking at?
  lists every pane with its pages from `PANES` until item 16's guide; Help > About names
  the engine's version and the sources.
- **Reason:** the menus and keys are the window's contract with the user and the tests;
  having them from the start means every later item adds behaviour to an existing action
  rather than a menu, and a reader of the menus sees what is coming.
- **Alternatives:** building each menu in its item (rejected: iterations.md puts the
  menus in item 02); a `RunInputs` dataclass in a Qt-free model now (deferred to item 15,
  which adds the edits and the presets).

## The icons and the table (item 03)

### G-016: The icons are Qt-free primitives in `models/icon_shapes.py`, painted by `icons.py` and shared with `scripts/docs_figures.py`
- **Sources:** ANSWERS.md 4: the icons are new vector icons in the spirit of French's
  Figure 2.1 [F95 p. 26 (img)], never copied bitmaps, "drawn with QPainter paths from the
  footprints of D-002", in one module from which both the GUI and `scripts/docs_figures.py`
  draw; if the script is moved onto the shared module, the README's pictures must still pass
  `tests/test_readme.py` and stay under 200 KB. PANES.md section 6: one Qt-free module
  describing each type as primitives in its footprint box pointing up, one Qt module painting
  them in the four orientations of `objects.ORIENTATIONS` [F95 p. 55], and tests that every
  type has a shape inside its box in every orientation. The script's `icon()` was the first
  sketch (TASK.md). French's screen drew the icons at their size on the table
  [F95 pp. 96-107 (img)].
- **Choice:** `models/icon_shapes.py` describes each of the eleven types as a tuple of
  `Primitive`s (a rounded rectangle, an ellipse, a polygon, a path of lines and quadratic
  curves, a line, a dot), each with a fill *role* (`metal`, `white`, `glass`, `dark`,
  `pepper`) rather than a colour, in a box of the caller's width and height centred on the
  origin, pointing up, every point inside the box; `ROTATION` gives the angle of each
  orientation, `rotated` and `bounds` let a test measure a turned icon. `icons.py` maps the
  roles to the README's colours, draws the primitives with QPainter (`paint_icon`, turning
  the painter by `ROTATION`) and renders a pixmap for a palette or a legend (`icon_pixmap`).
  `scripts/docs_figures.py` lost its `icon()` and `ROTATION` and draws through `paint_icon`;
  its figure registry and `draw_table` keep their form. The two table pictures of the README
  were regenerated (69 and 44 KB). The knife's handle, which the sketch drew 15 percent wider
  than the box on each side, now stays inside the box, as the test demands. The caption
  "icons: new drawings, not French's" (`icons.NOT_FRENCHS`) goes wherever icons appear.
- **Reason:** the primitives are plain data, so the shape test runs without Qt and the
  shapes are measurable; one module for both consumers is what ANSWERS.md 4 asks for, and
  moving the script onto it keeps the README's pictures and the GUI's table identical.
  Roles rather than colours keep the Qt-free module free of styling, which the painter owns.
- **Alternatives:** keep the script's `icon()` beside the GUI's (two sets drifting apart);
  describe the icons as SVG text (harder to test for the box, and Qt's SVG module is another
  dependency); use the four orientations as four separate drawings (rotation is a transform).

### G-017: The table model takes its positions from the run's `Problem` when the run starts, its marks from the events, and the snapshot only when no problem was given; views get `set_position`
- **Sources:** PANES.md section 1: each pane model is "a pure function of (the events up to
  index k, the nearest snapshot)". PANES.md P1: the state is `Snapshot.table` (`id`, `type`,
  `x`, `y`, `orientation`, `box`, `region`, `noticed`) and the events `RunStarted`,
  `ObjectNoticed`, `AnswerGiven`, `RunEnded` among others. `RunStarted.objects` carries the
  ids only (`world.py`), and the start's snapshot is taken when the `World` has been built,
  so the first events of a run (the start's postings, the Slipnet's first update, the touched
  object noticed) have no snapshot in force (G-006). Late in a long run the snapshots are
  keyframes (G-007). The Seek-Whence GUI gives each view the index of the event about to
  come through `set_position`, and its views take the snapshot's lists as the authority at a
  boundary plus the events since (Seek-Whence GUI, `cytoplasm_view.py`, `main_window.py`).
- **Choice:** `TableModel.set_problem(problem)` is called by the window's `start_run` with
  the `Problem` the controller's `check_inputs` resolved (the window now resolves the problem
  itself, before `_connect`, and passes the `Problem` on); the model copies the size, the
  touched object, the answer labels and the objects with their footprints
  (`table_of_problem`) into plain data. The events give the rest exactly at every index:
  `ObjectNoticed` marks and the last region, `DescriptionAdded` the hover text's
  descriptors, `AnswerGiven` the E, `RunEnded` the end; a `RunStarted` of another problem
  forgets the table. `update(snapshot)` supplies the positions only when the model has none
  (a history loaded without a problem) and never rolls back what the events said. The
  window gained Seek-Whence's `set_position` protocol (`add_view` collects the positioners,
  `_on_delivered` and `seek` call them before each event), so every pane model from here on
  can read `history.state_at(position)`.
- **Reason:** the table is drawn from the first event, with no empty table during the start
  (the Seek-Whence GUI's lesson of an end state that looked empty applies to the start too);
  the events the table reads are exact, so taking marks from an older keyframe would move the
  picture backwards; and the problem's data is the same the engine copied into
  `Snapshot.table`, so the picture is the same either way (tested: a model fed from snapshots
  alone equals one fed the problem).
- **Alternatives:** draw nothing until the start's snapshot (an empty table for some twenty
  events); take the ids from `RunStarted` and the positions from `load_problem` inside the
  model (a file read in a pane model, and wrong for an edited table of item 07); make the
  snapshot the authority and re-apply the events since it (Seek-Whence's way, needed there
  because its events do not carry everything; not needed here).

### G-018: The table's picture: the names, a faint centre line, the H and E arrows' geometry, a light ring on a noticed object, the hover text
- **Sources:** Figure 4.1 [F95 p. 96 (img)] and the dumps [F95 pp. 98-107 (img)]: the table
  is a plain rectangle with "Henry" above it and "Eliza" below; the touched object has a
  short arrow pointing at it from above with an **H** at the arrow's tail, and the answer an
  arrow from above with an **E** at its tail, SPEC.md 6c.14 ("the answer is an arrow with an
  E at its tail and the touch an arrow with an H"); the dumps have no centre line. PANES.md
  P1 asks for "a faint centre line (as `docs_figures.draw_table`)", noticed objects "marked
  lightly", the id on hover, and a hover with the descriptors and salience terms. D-005: the
  halves are a description for the trace and the GUI only.
- **Choice:** `models/table.py`'s `layout()` gives the names centred above and below the
  table, the centre line as the README's figures draw it (faint and dashed; a GUI
  convention, not French's, and the module says so), and `arrow_for`: the head on the top
  edge of the object's box, the tail 9 units up and 5 units to the left for the H, to the
  right for the E, as the dumps place the two letters, from below when the box is within a
  unit of the top edge. A noticed object wears a light blue rounded ring around its box
  (the region overlay comes with item 04). The hover text (`object_tooltip`) gives the id
  and type, the orientation, the centre, whose half (the upper half is Henry's, as his name
  is printed above the table), the footprint, the last region, the descriptors so far, and
  whether it is the touch or the answer; the salience terms wait for the overlays of item 04,
  which read the world's accessors. The scale is 6 scene units per table unit, the README's
  hero figure's.
- **Reason:** the dumps are the only picture of the table; what they show is drawn as they
  show it, and the two additions (the centre line, the ring) are faint and named as the
  GUI's. The arrows' geometry is a reading of the dumps' pictures, not French's numbers.
- **Alternatives:** no centre line (as the dumps); arrows from the nearest free side; the
  noticed mark as a dot or a change of the icon's outline.

### G-019: The table's zoom, pan and fit are Numbo's `TreeView`'s, on a canvas with a bar; the click selection waits for item 04
- **Sources:** PANES.md P1 interactions: "zoom, pan and fit (Numbo's `tree_view.py`)";
  hover; "click to select: the selection is shared with P5, P6 and P10
  (`models/selection.py`)". iterations.md gives `models/selection.py` to item 04. The Numbo
  GUI's `TreeCanvas` puts Fit, +, −, 1:1 and the legend on a bar over the view, zooms with the
  wheel and + / −, 0 for 1:1, F to fit, pans by dragging, and auto-fits until a zoom by
  hand (Numbo GUI, `tree_view.py`). G-014: the window fits 1280 x 800.
- **Choice:** `table_view.py`'s `TableView` adopts Numbo's zoom, pan and fit whole
  (`MAX_FIT_SCALE` 1.5, `ZOOM_STEP` 1.25, auto-fit on at first and after Fit, off after a
  zoom by hand; the fit kept on a resize); `TableCanvas` is the central widget with Numbo's
  bar, the legend reading "icons: new drawings, not French's" first and the pane's pages
  after, with an ignored horizontal size policy so that a narrow window clips it instead of
  growing; the view's minimum is 160 x 120. The click selection is built in item 04 with
  `models/selection.py`, as iterations.md orders it; the hover tooltips come from Qt's item
  tooltips, so a test reads `toolTip()`.
- **Reason:** Numbo's interaction is the convention (ANSWERS.md 5), and its canvas already
  fits a dockable window; the selection needs the other panes to mean anything.
- **Alternatives:** a plain `QWidget` painting the table (no pan or zoom); a toolbar of
  actions in the View menu instead of buttons.

## The table's structures and the selection (item 04)

### G-020: The structures come from the events, exact at every index; the snapshot supplies the hover text's numbers; the tap records the correspondences' terms beside each snapshot
- **Sources:** PANES.md P1 lists the events (`GroupProposed`, `GroupStagePassed`,
  `GroupBuilt`, `GroupRemoved`, `CorrespondenceProposed`, `CorrespondenceBuilt`,
  `CorrespondenceTested`, `CompetitionHeld`, `WorldviewChanged`, `CorrespondenceRemoved`)
  and the state (`Snapshot.groups`, `proposals`, `correspondences` with `tentative`,
  `in_worldview`, `loser`), and asks the hover text for `correspondences.static_terms` and
  `dynamic_terms`, which `docs/engine-api.md` ("The read-only state") says give the nine
  and the three factors "for an overlay, and draw nothing"; the snapshot carries only
  their sums. iteration 4's notes expected the snapshot to be the authority at a keyframe
  plus the events since (the Seek-Whence GUI's way, `cytoplasm_view.py`). G-007: late in
  a long run the snapshots are keyframes. `worldview.compete` (D-038, D-040) awards the
  wins and losses; `build_object.run_stage` records the stage that ran and moves a
  passing proposal to the next one (D-024).
- **Choice:** `models/structures.py`'s `StructuresModel` builds the groups, the proposals
  and the correspondences from the events alone, so that the picture is exact at every
  index (a group exists from `GroupBuilt` to `GroupRemoved`; a proposal's stage is the
  event's stage plus one when it passed, and a failed stage ends it; a correspondence's
  level comes from `CorrespondenceProposed`, its slippages and test from
  `CorrespondenceTested`, its Worldview membership from `WorldviewChanged`, its wins and
  losses from `CompetitionHeld` as `compete` awards them, its removal from
  `CorrespondenceRemoved` with the ones built inside it). `update(snapshot, terms)` keeps
  the snapshot's numbers only for the hover text (the rating as it stands, the wins and
  losses, a group's frame) and never an older snapshot's. The tap records
  `correspondence_terms(world)` beside each snapshot (`SnapshotHistory.record(index,
  snapshot, terms)`, `terms_at`), through the two accessors, copied into tuples of
  (name, value) pairs; between keyframes the hover text says "as of codelet N".
- **Reason:** the events carry everything the drawing needs, and the snapshot in force
  can be sixteen codelets old late in a long run (G-007), so a picture built from it would
  lag; the numbers that change without an event (a rating recomputed, a frame described)
  are the hover text's, where a stale value, dated, does no harm. Tested: at every snapshot
  index of two runs the event-built groups, proposals and correspondences equal the
  snapshot's, and the tap's terms equal a fresh computation on a plain world.
- **Alternatives:** the snapshot as the authority plus the events since (the Seek-Whence
  way; needed there, not here); terms computed by the view from the live world (an engine
  object kept, against the contract); no terms (the snapshot's sums alone).

### G-021: The structures' picture: group boxes with pads, a wavy border for gestalt groups, proposals as dotted outlines with stage dots, lines between the ends' centres, the flash by the event index's parity, new ends for two codelets, removed structures fading for three
- **Sources:** SPEC.md 6c.14 and the dumps [F95 pp. 98-107 (img)]: "a gestalt group is a
  dashed wavy box; a subgroup a dashed rectangle, captioned "Build new subgroup" when
  built" (Figure 4.5 [F95 p. 102 (img)]); "a tentative or competing correspondence a
  dashed line, a Worldview correspondence a solid one, group-level ones thicker" (Figure
  4.7 [F95 p. 104 (img)], the thick line between the two spoon subgroups); a new
  correspondence "is highlighted by flashing boxes at both ends" [F95 p. 101]; the
  incumbent during a fight is "actually a flashing dashed line on the computer screen"
  [F95 p. 105]. PANES.md P1: proposals "as a faint dotted outline with its stage, removed
  groups fading out", losers fading, and "the flash is a two-phase dash offset, driven by
  the event index, not by a timer, so that a picture is the same at every redraw". The
  dumps draw no proposals and nothing of what was removed.
- **Choice:** a group's box is the union of its members' footprint boxes grown by a pad
  (2.6 table units for a gestalt group, 1.3 for a subgroup, 0.5 for a proposal, so that
  each nests inside the next); a gestalt border is `wavy_outline`, a closed polyline with
  a sine wave of 0.6 units across the rectangle's edges, drawn with a dashed pen; a
  subgroup is a dashed rectangle, captioned "Build new subgroup" for two codelets after
  its `GroupBuilt`; a proposal is a dotted rectangle at low alpha with three small dots
  above its corner, one filled per stage passed (the stage in words is the hover text's;
  a text label was tried and cluttered the gestalt box, since the proposals of a run are
  many and overlap). A correspondence is a line between the centres of its ends (an
  object's box or a group's box), solid when in the Worldview, else dashed with the dash
  offset by half a period on odd event indexes (`flash_phase(index) = index % 2`), 2.8
  scene units wide when an end is a group and 1.5 otherwise, at alpha 120 when it lost a
  competition and is outside the Worldview, at 60 when removed; for two codelets after
  its `CorrespondenceBuilt` its two ends are boxed, filled on even indexes and outlined
  on odd ones. A removed group or correspondence and a failed proposal stay drawn, faint,
  for three codelets (`FADE_CODELETS`), then go; the end state after `RunEnded` stays
  drawn, since the model only moves its clock. Every number is a module constant.
- **Reason:** the dumps' conventions as French describes them, with the GUI's three
  additions (proposals, fading, the parity flash) kept faint and named as the GUI's;
  parity rather than a timer keeps every picture a function of the event index, as
  PANES.md demands and the pixel probe checks. The pads nest the three kinds of box;
  the fade keeps a removal visible for a step or two at speed.
- **Alternatives:** no proposals on the table (a list in the Worldview pane); an animated
  flash (not reproducible); removed structures gone at once; group boxes from the
  engine's region geometry rather than the footprints.

### G-022: One `Selection` for every pane, held by the window (Seek-Whence's); a click toggles the element under the mouse; a group selects its members and a correspondence its ends
- **Sources:** PANES.md P1: "click to select: the selection is shared with P5, P6 and P10
  (`models/selection.py`)"; iterations.md item 04. The Seek-Whence GUI's
  `models/selection.py` (its decision 25): one Qt-free `Selection` owned by the window,
  with a kind, a name, the members the selection implies and listeners; a left click on
  the canvas toggles the element under the mouse and a click on empty canvas clears; a
  hull's click band is its outline. The Numbo GUI has no shared selection.
- **Choice:** `models/selection.py` adopts the Seek-Whence module whole, with the kinds
  `object`, `group`, `proposal` and `correspondence`. The window owns `self.selection` and
  gives it to the `TableCanvas`; the panes to come (P5, P6, P10) subscribe to the same
  object. `TableModel.selection_of(name)` says what a click means: an object alone, a
  group or a proposal with its members, a correspondence with its two ends and their
  members. `TableView.element_at` finds the topmost element under a viewport point
  (objects, then lines, proposals and groups by their z order); a group's, a proposal's
  and a line's click band is its border or stroke (`shape()`), so that empty table inside
  a group clears the selection. Selected items are drawn with an orange halo or ring; the
  view emits `selected(name)` after every change and the window's status bar names the
  selection with the first line of its hover text.
- **Reason:** the precedent fits exactly, and names only keep the engine contract; the
  border band follows the Seek-Whence hull and keeps nested boxes selectable one by one.
- **Alternatives:** a selection per pane synchronised by the window; whole-box click
  areas for groups; a Qt signal instead of listeners (the models could not be tested
  without Qt).

### G-023: The hover text of a group, a proposal and a correspondence: the events' facts, the snapshot's numbers "as of codelet N", the terms and the slippages
- **Sources:** PANES.md P1: "hover a correspondence for its static and dynamic terms
  (`correspondences.static_terms`, `dynamic_terms`) and its slippages"; engine-api.md's
  "Three facts" (the frame words a group gave its members come from `groups`, not from the
  objects' description lists). The screen shows no text on hover; the hover text is the
  GUI's own (G-018 for the objects).
- **Choice:** a correspondence's text gives its name, the two ends ("Henry's end",
  "Eliza's end") and level; its status in words (in the Worldview; tentative, not tested
  yet; failed its test; competing with its rivals at this codelet; lost a competition,
  outside the Worldview; tested, outside the Worldview; removed at codelet N); the rating
  as the sum of the static, dynamic and bonus points, from the snapshot in force when it
  has the correspondence ("as of codelet N") and from `CorrespondenceBuilt` before; the
  nine static and three dynamic terms the tap recorded, by name; the slippages as "a to b
  (label)"; the wins and losses. A group's text gives its kind, members, strength and the
  codelet it was built at, its parent, the frame (orientation, linear, ends, middles) from
  the snapshot, and its removal; a proposal's its kind, members, base, the codelet
  proposed at and its stages passed, failed or built.
- **Reason:** the hover text is where the numbers belong (the drawing follows the
  dumps), and dating a snapshot's numbers is the honest way to show a value that may be
  a few codelets old between keyframes.
- **Alternatives:** the numbers drawn beside the lines (the screen draws none); a terms
  table in the Worldview pane only.

## The table's overlays and Eliza's view (item 05)

### G-024: The tap records the salience readings beside each snapshot; the overlays are dated by the snapshot's clock and never rolled back; a drawn region is the notice's, then the snapshot's later look
- **Sources:** PANES.md "P1 overlays" asks for the salience heat map from
  `salience.saliences`, the eight terms of a hovered item from `salience.terms` in
  French's order (D-031) and the nine regions ranked from `salience.region_order` (D-032),
  "all read without a draw"; `docs/engine-api.md` ("The read-only state") lists the three
  accessors as drawing nothing, and says that later looks at an object "update
  `Workspace.regions` without an event" (`ObjectNoticed` carries the first look's region,
  D-004). `Snapshot.saliences` carries the sums and `Snapshot.table[*]["region"]` the last
  drawn region, but no terms and no region order. G-020 set the pattern: the tap reads the
  correspondences' terms beside each snapshot, and the hover text dates them "as of
  codelet N". G-007: late in a long run the snapshots are keyframes.
- **Choice:** `models/tap.py`'s `salience_readings(world, items)` reads `salience.terms`
  for every item of the snapshot's `saliences` and `salience.region_order`, copied into
  tuples of (name, value) pairs, and the tap records them beside each snapshot
  (`SnapshotHistory.record(index, snapshot, terms, salience)`, `salience_at`,
  `EventLog.salience_at`). `models/overlays.py`'s `OverlaysModel.update(snapshot,
  salience)` takes the saliences, the terms, the region order and the noticed objects'
  last drawn regions from them, dated by `snapshot.clock`, and ignores a snapshot older
  than the last applied, so that seeking never rolls them back. A drawn region is the
  `ObjectNoticed` event's until a snapshot in force says the object is noticed, when the
  snapshot's later look replaces it; the table's own hover line keeps the event's region
  (G-017's marks follow the events), and the overlays add "last drawn region: X (as of
  codelet N)" when the two differ. With every layer off the `Overlays` carry no date, so
  the layout stays a pure function of the events (item 04's tests hold unchanged).
- **Reason:** the saliences change with every Slipnet update and every structure, without
  an event, so they can only be read from the state, and the three accessors are the ones
  the contract allows; reading them beside the snapshot keeps every reading on the
  worker, between two steps, where the engine stands still (G-006). The terms of every
  item cost less than the snapshot itself (the loop's determinism check and `test_tap.py`
  confirm that the draw count is unchanged). Dating by the clock, not the event index,
  makes the roll-back guard independent of the history.
- **Alternatives:** terms read lazily on hover from a live world (an engine object kept,
  against the contract); the terms recomputed by the GUI from the snapshot's activations
  (a second implementation of D-031 to keep in step); the drawn region from the snapshot
  alone (empty between the notice and the next keyframe late in a long run).

### G-025: The overlays' picture and toggles: cells with soft bands as wide as the blur, the engine's names, the drawn tint, the ranks, the heat by share; three layers as checkable actions on the bar and in the View menu, remembered under `controls/overlay-*`; every layer off leaves the layout untouched
- **Sources:** PANES.md "P1 overlays" (GUI design, not French's; ANSWERS.md 4): the nine
  blurry regions with their names and each noticed object's last drawn region, the
  salience heat map with the eight terms on hover, the region preference as the nine
  regions ranked, with "toggles in P1's toolbar and the View menu, remembered in
  QSettings" (iterations.md item 05). French: "the boundaries between regions are
  intentionally blurry" [F95 p. 65], the salience factors [F95 pp. 66-67], the regions
  "diagonally opposite the touched object" and "directly opposite" it preferred [F95
  p. 66]; no figure of any of these. D-004 draws a region with a blur of
  `Config.region_blur_width` units around each boundary. The Seek-Whence GUI's
  `cytoplasm_view.py` keeps its layers as a dictionary on the model and checkable
  `QAction`s on the view, shown as `QToolButton`s on the bar; the Numbo GUI remembers
  its controls under `controls/*`. G-014: the window must fit 1280 x 800.
- **Choice:** `OverlaysModel.layers` (`regions`, `salience`, `ranks`) and
  `TableModel.eliza` are the state; `layout()` includes an `Overlays` record only for the
  layers on. `RegionsItem` paints the nine cells (the thirds of each axis, in
  `geometry.REGIONS`'s reading order) with soft translucent bands as wide as the run's
  blur along the two boundaries of each axis (the blur read from `RunStarted`'s profile and
  changed fields, `blur_width_of`), the engine's region names in the top left corners, a
  light blue tint and "drawn: id, ..." in a cell that is some noticed object's last drawn
  region, and, with the ranks layer on, each region's rank from the touched object (1 the
  most preferred, in `region_order`'s order, reading order among equals as the engine
  ranks them) in the centre with its preference weight in the top right corner.
  `SalienceItem` tints every item in view over its box with a warm colour whose alpha
  grows with the item's share of the largest salience, and writes the value above the
  box. The hover text of an object or a group gains "salience V as of codelet N" and the
  eight terms in French's order (signed, one decimal) while the salience layer is on. The
  three layers and Eliza's view are checkable `QAction`s owned by the `TableView`
  (`layer_actions`, `eliza_action`), shown as `QToolButton`s on the canvas's bar (with a
  small minimum width, G-014) and added to the View menu's "Table overlays" submenu and
  "Eliza's view" by the window, so that the bar and the menu are one state; the window
  saves them as `controls/overlay-regions`, `-salience`, `-ranks` and `-eliza` and
  restores them (a value that does not read as a boolean leaves the default, off). The
  bar's legend names the layers on, dated "as of codelet N", with "GUI design, not
  French's", and the toggles' tooltips cite the decisions and say the same.
- **Reason:** the regions and the salience have no figure in the sources, so the picture
  is the GUI's own and says so everywhere it appears; soft bands make the blur visible
  without pretending to a probability the picture cannot show (D-004's exponential is
  the hover text's business, not the overlay's); the share, not the value, drives the tint
  because the saliences' scale varies with the weights of D-031; the engine's names stay
  on the cells so that the overlay reads as the trace does. One set of actions in two
  places is the simplest way to keep the bar and the menu in step, and what Seek-Whence's
  layers do.
- **Alternatives:** a single "Overlays" menu button on the bar (saves width, hides the
  state); the blur drawn as a gradient of D-004's probability; the heat as a colour scale
  with a legend; the regions always drawn faintly (would alter item 04's picture); the
  overlay state in the layout file rather than the settings.

- **Note (2026-10-14, the roles loop's item 08):** the hover text lists nine terms, French's
  eight in his order and the role term after them (`salience.FACTORS`; consumer C7, D-100),
  since the engine gained the ninth term; G-024's readings carry it unchanged, as a (name,
  value) pair.

### G-026: Eliza's view is a pure transform of the layout, (x, y) to (width - x, height - y), not a view transform: icons turned round, names swapped, the regions keeping the engine's names
- **Sources:** PANES.md "P1 overlays": "the whole table rotated by 180 degrees, as she
  sees it. This is the GUI's own transform; the engine has no notion of a side beyond
  the regions (D-005)"; `docs/engine-api.md` says the same of x and y. The test asked
  for by iterations.md item 05: Eliza's view maps (x, y) to (width - x, height - y).
  The dumps write Henry's name above the table and Eliza's below [F95 p. 98 (img)].
- **Choice:** `models/overlays.py`'s `eliza_view(layout)` returns a new `Layout` with
  every object at (width - x, height - y) and its orientation turned round (`FLIP`: up
  and down, left and right swap), the names swapped (Eliza's above the table, Henry's
  below), the arrows' tails and heads, the groups' and proposals' boxes, the
  correspondences' ends and end boxes and the overlays' cells and heat boxes mapped by
  the same rule, and `eliza=True`; applying it twice gives the layout back. The regions
  keep the engine's names (an `upper-left` cell is drawn at the lower right of Eliza's
  view). `TableModel.layout()` applies it when `eliza` is on; the view paints the
  layout as it is, so the names and letters stay upright and `_place_table` writes the
  name whose y is 0 above the table.
- **Reason:** a `QTransform` rotation of the view would turn the text upside down and
  would not be part of the layout, so the pixel probe (a fresh view fed the history draws
  the window's picture) could not cover it; a layout transform is Qt-free, testable
  without Qt, and keeps the picture a pure function of the events and the flags. The
  engine's region names are what the trace prints, and D-005 says the halves are regions
  only, so renaming them for Eliza would invent a side.
- **Alternatives:** a rotated `QTransform` with text items that ignore transformations
  (they would ignore the zoom too); a mirror across the middle line only (not what Eliza
  sees); region names rewritten from Eliza's side.

## The thermometer and the temperature plot (item 06)

### G-027: The thermometer is French's column from the events alone: the last reading, grey while held, a reason tick, the scale and the age as the GUI's additions, empty before the first reading, the last update shown at the answer
- **Sources:** "Immediately to the right of the table is the thermometer" [F95 p. 95];
  Fig. 4.1 draws the column empty at 0 codelets and the dumps draw it filled from the
  bottom with the value printed beside the top of the fill, 80.0 at 2 codelets, 59.0 at
  22 [F95 pp. 96-107 (img)]; `tests/data/ch4.json` reads the column as empty at 0 and
  12 codelets. PANES.md P2 asks for a grey band on the updates while T is held (D-049)
  and a small tick for the last update's reason, and says to show the last update and
  not to expect one at the answer (D-050). `TemperatureUpdated` carries T, the inputs,
  `held` and `by` (D-065). The Numbo GUI's `coderack_view.py` is a `QWidget` that paints
  its model.
- **Choice:** `models/temperature.py`'s `TemperatureModel` is built from the events
  alone, as the structures are (G-020): a `Reading` per `TemperatureUpdated`, a `Mark`
  per competition and change, the run's `Config` put back from `RunStarted`'s profile
  and changed fields (`config_of_start`, parsing the reprs of D-065), `RunEnded`'s T;
  exact at every index, so seeking rebuilds it. `thermometer_of` gives the form: the
  last reading, `fill` = T / 100, the text to one decimal as the dumps print it, `held`,
  `by`, the clock and the `age` (the last event's clock less the reading's). The view
  paints French's column: black from the bottom to T, the value at the fill's top; the
  GUI adds, and says so in the tooltip and the pages text, a grey fill while held (the
  band of PANES.md P2 read as the fill's colour, since the column holds one value), a
  magenta tick at the fill's top with the reason's name under the column, faint scale
  marks at 0, 50 and 100, and the reading's codelet with the age in codelets. Before the
  first reading the column is empty with "no reading yet". After `RunEnded` the form is
  the last reading's, and the note says no update follows the answer.
- **Reason:** the events carry everything, so the snapshot is not needed and a fresh
  view fed the history draws the window's picture (the pixel test); the `Config` from
  the start event keeps the model a function of the events, so a replay of a session
  file (item 17) needs nothing else; a band on the updates would need a history the
  column cannot show, while the colour of the fill shows the same fact at a glance;
  the age makes D-050's staleness visible instead of hiding it (ANSWERS.md 11).
- **Alternatives:** the `Config` handed over by the window as the table's `Problem` is
  (G-017); the held updates as grey marks on the scale; a thermometer that moves at the
  answer (it would be an update the engine never made).

### G-028: The temperature plot is three bands over the codelets, its geometry a pure function of the model, the size and two options; the terms stacked on the T band; the options as the plot's own actions on its bar and in the View menu, remembered under `controls/plot-*`
- **Sources:** PANES.md P2b: T, Q, ΔQ, Δ_WV and T_cp against the codelets run, with the
  competitions and the Worldview changes marked, an option for the three terms as
  stacked areas [F95 p. 91 (img)] and one for French's chapter 4 thermometer; "The plot
  is GUI design". No figure of French's plots the temperature (PANES.md section 0).
  RESULTS.md section 6.4 plots T against the codelets run and reports the mean size of
  the three terms. The overlays' toggles are the table view's own actions, on its bar
  and in the View menu (G-025).
- **Choice:** `plot_layout(model, width, height, terms, french)` is a pure function
  giving a `PlotLayout`: three bands sharing the x axis (the codelets run, from 0 to
  the last event's clock, at least 10, and to French's last dump when his points are
  shown), T on 0-100 taking half the height, Q and ΔQ on 0-1 a quarter, Δ_WV and T_cp
  on 0 to the largest count seen a quarter; each series a `Polyline` with one point per
  reading; a `MarkLine` per competition (dashed, magenta) and per change (solid, green)
  across the bands; with `terms`, three `Area`s on the T band, the two positive terms
  stacked from 0 and the subtracted one from the top of the stack down, so their net is
  the T line while the formula applies and T lies inside 0-100 (held readings and a
  clamped T are where it departs, and the hover text says "not applied"); a caption
  that names the design's status, the options on and the state. The view
  (`temperature_view.py`) paints the layout for its size, with a dot per reading, and
  shows the nearest reading's numbers and terms on hover. The two options are
  checkable actions of the pane (`terms_action`, `french_action`) on its bar and in the
  View menu's "Temperature plot" submenu, one state in two places, remembered as
  `controls/plot-terms` and `controls/plot-french` through `plot_state` and
  `set_plot_state`.
- **Reason:** the five quantities have three scales, so one axis would flatten Q and
  the counts; the geometry as a pure function lets the Qt-free tests check the picture
  (the points equal the events, the areas sum to T) and keeps the view a painter of
  polylines; the options follow G-025's pattern so that the settings and the menu work
  the same way for every pane.
- **Alternatives:** one band with the fractions scaled by 100 and the counts on a right
  axis; a `QGraphicsScene` with items (more code for a picture nobody clicks on);
  the options in the View menu only.

### G-029: French's chapter 4 thermometer is a constant in the model, checked against `tests/data/ch4.json` by the tests, and offered for `surround-05` only
- **Sources:** PANES.md P2b: French's chapter 4 thermometer "from `tests/data/ch4.json`"
  as labelled points when the problem is `surround-05`; the readings 80.0 at 2
  codelets, 59.0, 43.2, 31.3, 35.8, 13.6 and 2.0 at 22, 33, 36, 47, 47 and 55 [F95
  pp. 98-107 (img)]; the engine keeps French's reported numbers in `tabletop/reported.py`
  as constants, and `tests/test_ch4.py` checks `ch4.json`. ANSWERS.md 11 allows French's
  numbers beside the run's, clearly labelled, never adjusting the run.
- **Choice:** `FRENCH_CH4` in `models/temperature.py` holds the seven readings as
  (figure, codelets, T); `test_temperature_model.py` checks that they equal the dumps of
  `tests/data/ch4.json` whose thermometer is not empty, in order. The model's
  `french_offered` is True only while the run's problem is `surround-05`; the plot's
  French option is enabled only then, its checked state kept, and `french_points()` is
  empty otherwise. The points are drawn as hollow red diamonds labelled "Fig. 4.4: 59.0"
  on the T band, never joined to the run's line.
- **Reason:** the package must not read the tests folder at run time (a standalone
  install has no `tests/`, ANSWERS.md 9), and a constant in the model follows the
  engine's own practice for French's numbers; the test is what PANES.md asks for
  ("French's points equal `ch4.json`'s"); the chapter 4 run is Surround 5 and no other
  problem, so the points would mislead elsewhere.
- **Alternatives:** reading `ch4.json` at run time when it exists; offering the points
  for every problem with a warning.

## The Slipnet grid (item 07)

### G-030: The Slipnet grid's activations come from the events alone, exact to a tenth at every index; a seventh row for the seven nodes the screen lacks; a jolt outlines its box for one event; a click selects a `node` in the shared selection
- **Sources:** PANES.md P3: the 41 boxes of `slipnet_def.SCREEN_GRID` [F95 p. 95]; a
  seventh row, labelled as the GUI's, for the six object nodes the screen lacks and
  "isolated" (D-007; SPEC.md 3.1); a jolt outlines its box for one event, with the
  amount and `by` in the tooltip; the events `SlipnetUpdated` (`changed`, `lengths`) and
  `NodeJolted`, the state `Snapshot.slipnet`; click to select the node in P3b.
  `docs/engine-api.md`: `SlipnetUpdated.changed` lists every node that moved, rounded
  to a tenth, after every update (D-019), and the first, at clock 0, every node above 1
  after the initial state's update (D-017); `NodeJolted.activation` is the value after
  the jolt (D-016); `ContextPushed.reset` names the nodes put back to 1 and
  `ContextPopped.restored` every node touched with its activation after (D-021).
  G-020 and G-027 build the structures and the temperature from the events alone,
  exact at every index. G-022 holds one `Selection` for every pane, with four kinds.
- **Choice:** `SlipnetGridModel` keeps one activation per node of `slipnet_def.NODES`:
  every node starts at 1, `SlipnetUpdated.changed` sets the nodes that moved,
  `NodeJolted` sets the node's activation after the jolt and records the `Jolt` (index,
  clock, amount, by, activation), `ContextPushed` puts the reset nodes back to 1 and
  `ContextPopped` sets the restored values. The model is a function of the events, so
  seeking rebuilds it, and `test_slipnet_grid_model.py` checks it against every
  snapshot of two golden runs to a tenth (the worst difference is the rounding, 0.05).
  The rows are `SCREEN_GRID`'s six and `EXTRA_ROW`, the nodes with `on_screen` False in
  the definition's order (saucer, plate, saltshaker, pepper shkr, big glass, soup bowl,
  isolated), drawn with a dashed grey border under a gap, captioned "GUI design, not
  French's" with the reason; the pane's pages text says the seventh row is the GUI's
  without that exact phrase, since the pane as a whole is French's and the window's
  test of the new designs reads the phrase as marking a whole pane. A box is outlined
  (magenta) while the event shown is the jolt itself (`jolted`: the last jolt's index
  equals the model's index), so stepping event by event shows every jolt and a step
  codelet shows none, as PANES.md's "for one event" reads; the hover text names the
  last jolt with its amount and `by` whenever it was. A left click toggles the node in
  the window's shared selection as the kind `node` (added to `selection.KINDS`), with
  no members; the status bar names it by the grid's hover text; the table's panes
  ignore a node selection, and P3b (item 08) will highlight it.
- **Reason:** the events carry every change of activation to the precision the trace
  prints, so no snapshot is needed to draw the grid, and the picture is exact while
  seeking, where the snapshots are keyframes (G-007); the seventh row keeps the engine's
  48 nodes visible without pretending French's screen had them; one event is what the
  design says, and the hover text keeps the jolt readable afterwards; a fifth kind is
  the smallest change to the shared selection that lets P3b follow the grid.
- **Alternatives:** the snapshot's activations at keyframes with the events between
  (G-020's first reading, rejected there too); outlining a jolt until the codelet's end
  (more visible while stepping by codelet, but not the design's one event); a selection
  of nodes apart from the table's (two selections to keep in step).

### G-031: The box as French's screen drew it: the label along the bottom, the number in the upper right-hand corner as an integer, the black square's side linear in the activation; the geometry a pure function of the model, the size and the options
- **Sources:** "The activation of a node is indicated by the number in the upper
  right-hand corner of the corresponding box. This activation is also represented
  graphically by the black square within the box. Activation values range from 1 to
  150." [F95 p. 95]. The dumps print each label in small type along the bottom edge of
  its box and integers in the corners ("139", "44", "9", "1"), with no square at 1, a
  dot at 9, a small square at 44 and a large one at 139, the sides in about the
  proportion of the numbers [F95 p. 98 (img)]. Whether the side or the area grows with
  the activation is not said. G-028 makes the plot's geometry a pure function.
- **Choice:** `square_side(activation, inner)` is `inner` times (activation - 1) / 149,
  clipped to the floor and the ceiling: nothing at 1, the whole inner box (0.72 of the
  box's side) at 150, linear between; `number_text` prints the activation rounded to an
  integer as the dumps do (the hover text keeps the tenth). `grid_layout(model, width,
  height, french, selected)` gives the `GridLayout`: seven columns and seven rows of
  square boxes at the side that fits the size, a margin of 0.15 and a gap of 0.45 of a
  side before the seventh row, each `Box` with its corner, side, number, square side,
  jolted and selected flags, `on_screen` and French's value; `box_at` finds the box
  under a point. The widget paints the layout: French's rows with a solid dark border,
  the seventh with a dashed grey one, the label bottom-left, the number top-right, the
  square centred, the jolt outline inset by 2 px, the selection outline (orange) by 1 px,
  French's number bottom-right in red, and the caption under the seventh row. The font
  shrinks with the box, between 5 and 8 points.
- **Reason:** the side read against the dumps' numbers is linear within the reading's
  precision (44 against 139 gives a third of the side, as the figure shows), and a side
  linear in the activation is the simplest rule that fits; integers are what French
  printed; a pure layout lets the model test check the picture's geometry without Qt and
  the view test compare the window's pixels with a fresh render.
- **Alternatives:** the area linear in the activation (the side as its square root;
  the dumps' small squares would then be larger than they look); printing a decimal.

### G-032: French's nine chapter 4 grids are a constant in the model, checked against `tests/data/ch4.json`, the nearest in codelets shown in red on request for `surround-05` only, remembered under `controls/grid-*`; the hover text's links and effective lengths come from the table and the snapshot in force, dated
- **Sources:** PANES.md P3: an option sets the activations of the chapter 4 dump
  nearest in codelets beside the run's (`tests/data/ch4.json`), labelled; hover a box
  for its depth, level and links. ANSWERS.md 11 allows French's numbers beside the
  run's, clearly labelled. G-029 keeps French's thermometer as a constant in the
  model, checked against `ch4.json` by the tests, offered for `surround-05` only, and
  G-028 remembers a pane's options under `controls/plot-*`. The nine dumps are at 0,
  2, 12, 22, 33, 36, 47, 47 and 55 codelets, Figs. 4.7, 4.8 and 4.9 printing the same
  41 values [F95 pp. 96-107 (img)]. The links are `slipnet_def.LINKS` (D-009, D-010);
  `Snapshot.links` carries the labeled links' effective lengths (D-011), and
  `SlipnetUpdated.lengths` only the ones that changed. G-024 dates what the snapshot
  supplies by its clock and never rolls it back.
- **Choice:** `FRENCH_CH4_GRIDS` holds the nine dumps as (figure, page, codelets, the
  41 values in reading order); `test_slipnet_grid_model.py` checks them against
  `ch4.json`. `nearest_dump(clock)` takes the smallest distance in codelets, the earlier
  figure on a tie. The pane's bar and the View menu's "Slipnet grid" submenu hold one
  checkable action, "French's ch. 4", enabled only while the run is `surround-05`
  (its checked state kept), remembered as `controls/grid-french`; when on, each of the
  41 boxes prints the dump's value in red in its lower right-hand corner and the
  caption names the figure, its codelets and the run's codelet. The hover text gives
  the activation to a tenth at the event shown, the level (D-008) and the depth from
  the run's `depth_by_level` (the standard's before a run), where the box is, the last
  jolt, and every link of the table with its length, the labeled and identity links
  with the effective length of the snapshot in force, dated "as of codelet N"; the view
  reads the snapshot through the window's history at `redraw`, by G-017's
  `set_position` protocol, so the pane takes the history and the selection through
  `attach(history, selection)` when the window builds it.
- **Reason:** as G-029: the package must not read `tests/` at run time, and the engine
  keeps French's numbers as constants; a dump of another problem would mislead; the
  link table is static, and the effective lengths are P3b's business, so the hover
  text dates them rather than tracking `SlipnetUpdated.lengths` (whose first event
  does not list the initial lengths). `attach` keeps `BUILT`'s one-line construction
  for the panes that need nothing of the window's.
- **Alternatives:** reading `ch4.json` when it exists; interpolating between dumps;
  tracking the effective lengths from the events with the intrinsic lengths as the
  start; passing the history and the selection to every built pane's constructor.

## The Slipnet network (item 08)

### G-033: The network's effective lengths come from the engine's reports: every labeled link starts at its floor length, the intrinsic times (1 - `label_shrink` / 150), and `SlipnetUpdated.lengths` sets the ones that changed; exact to a tenth at every index; the activations through a composed grid model; a label node is highlighted while a link it labels is shorter than at the floor
- **Sources:** PANES.md P3b: the labeled links drawn at their current effective lengths
  (D-011, D-012), with the label nodes highlighted when they shorten links; the events
  `SlipnetUpdated`, `ContextPushed`, `ContextPopped`; the state `Snapshot.links` and
  `Snapshot.context`; the test "the drawn lengths equal the snapshot's `effective`
  values". "the length of any labeled link varies dynamically as a function of the
  activation of the link's label node" [F95 p. 56]. D-011: a labeled link's effective
  length is its intrinsic length times (1 - `label_shrink` x a_label / 150), with the
  more active of two labels, recomputed after every update; ISA and identity links never
  change. `docs/engine-api.md`: `SlipnetUpdated.lengths` lists the labeled links whose
  effective length changed, rounded to a tenth (D-019), and the first update does not
  list the initial lengths (G-032). G-030 builds the activations from the events alone.
- **Choice:** `SlipnetNetworkModel` composes a `SlipnetGridModel` for the activations
  and keeps `effective` by link name: at a `RunStarted` every link takes `floor_length`,
  the intrinsic length times (1 - `label_shrink` / 150) for a labeled link with labels
  (the engine's own rule with every node at 1, which is the state the first update's
  report compares with), rounded to a tenth as the reports are (D-019; the roles loop's
  item 03b: unrounded, a floor such as end-middle's 39.84 sat 0.06 from the engine's
  39.78 while both rounded to 39.8, so the model is within 0.05 only when it holds the
  rounded floor), and the intrinsic length for the rest, `label_shrink` read from
  the run's configuration (`config_of_start`, G-027); every `SlipnetUpdated.lengths`
  entry sets its link. A jolt, a push or a pop changes no length until the next update,
  as in the engine. The model is a function of the events, exact to a tenth:
  `test_slipnet_network_model.py` checks it against every snapshot of three runs
  (`surround-05` seed 3, `blockage-06` seed 2, `blockage-07` seed 116; the worst
  difference is the rounding, 0.05). A link is `shortened` when it is shorter than its
  floor length by at least a tenth, and a label node is `shortening` while a link it
  labels is and it is the link's most active label, the one the engine's rule takes
  (D-011; "diagonal symmetry" at the floor does not shorten left-right while "opposite"
  does); the stems of those links are drawn active.
- **Reason:** the engine's reports are what the trace prints, so the pane shows the
  engine's numbers, not a recomputation from rounded activations (which would also shrink
  a link at a jolt, before the engine recomputes); the floor length is the one value the
  reports leave implicit, and it follows from the rule at the floor; composing the grid
  model keeps one reading of the activations (G-030) for both panes.
- **Alternatives:** the formula from the model's activations (within 0.001 of the engine,
  but shrinking at the wrong moments); the snapshot's `links` at keyframes with the
  events between (G-020's first reading, rejected again).

### G-034: The picture, GUI design after Fig. 3.6: three bands by level, the most abstract on top, each band's nodes in up to three staggered sub-rows, the object band ordered by its labeled pairs; circles with a dark disc linear in the activation, ISA links as thin arrows, labeled links thicker as they shrink with the length beside them, identity links as loops, dashed stems from the label nodes; the geometry a pure function; a cached pixmap
- **Sources:** PANES.md P3b: the nodes by depth level, in Fig. 3.6's style; the network
  is GUI design. Fig. 3.6 draws nodes as circles with their names inside, ISA links as
  arrows from the type to the category, the direct link A-B as a line with its length
  written beside it, and the label node n3 tied to that link by a dashed stem [F95 p. 61
  (img)]; no page draws the whole network. "three levels of abstractness" [F95 pp.
  61-62] (D-008). iterations.md item 08: cache the layout's picture, as the Seek-Whence
  GUI's platoplasm does (`platoplasm_view.py`: a pixmap rendered again only when the
  layout, the size or the pixel ratio differ). G-031 makes the grid's geometry a pure
  function; G-022 holds the shared selection.
- **Choice:** `network_layout(model, width, height, selected)` gives the `NetworkLayout`:
  three bands between the banner and the caption, level 3 on top and level 1 at the
  bottom; each band's nodes spread across the width in `nodes_of_level`'s order (by group
  in French's list's order, then the definition's; the object band in `BAND_ORDER`,
  which puts the eight labeled pairs of types side by side), in three staggered sub-rows
  when the band holds more than eight nodes (the object band's eleven and level 2's
  twenty-nine), so that the names fit and the labeled links between neighbours are not
  collinear. A `Dot` has the circle's centre and radius (between 3 and 9 px, from the
  band and the width) and `fill`, the dark disc's radius, linear in the activation as the
  grid's square is (G-031); a `Line` per link of the table (66: 47 ISA, 15 labeled, 4
  identity) with the intrinsic and effective lengths, `shrink`, `shortened` and the
  effective length as text for a labeled link; a `Stem` per label of each labeled link
  (21), from the label node to the link's midpoint. The widget paints ISA links thin
  grey with an arrowhead at the category, labeled links dark with a stroke of 1 + 3 x
  shrink pixels, blue once shortened, the number at the midpoint, identity links as a
  loop above the node, stems dashed (green while active), the circles (green for a label
  node, bold while it shortens), the disc, the rings of G-035 and the selection (orange),
  and the names under the dots (above, in the upper sub-rows). The whole is rendered to
  a pixmap keyed by the layout, the size and the pixel ratio, repainted as a blit while
  nothing changes (`renders` counts the renders, `paints` the paints); `dot_at` and
  `line_at` find what is under the mouse. A left click toggles the node in the shared
  selection, the grid's kind `node` (G-030), so the grid's selection and the network's
  are one, and a selected node's links are drawn orange.
- **Reason:** the figure's vocabulary (circles, ISA arrows, a length beside the link, a
  stem from the label) applied to the whole table is the closest the GUI can come to
  French's style without inventing a figure he did not draw, and the pane says so; the
  lengths cannot be drawn as distances with 48 nodes placed by level, so the number and
  the stroke carry them; the cache is the item's instruction, and most events (a codelet
  chosen, posted, ran) change no activation, so a blit is enough between updates.
- **Alternatives:** a force-directed layout (not deterministic across sizes, and the
  level is what the design asks for); drawing only the labeled links (the ISA fan is
  dense, but it is where the activation spreads); a toggle for the ISA links (left for
  the review if the pane is found too dense; the hover text carries every link).

### G-035: The context as the events say it: the reset nodes ringed while inside the subworld, the restored nodes ringed through the pop's codelet, the banner `no context push in this run` until a push, then the push or the last pop with the counts; the push seed `blockage-07` seed 116 under `standard`, found in a bounded search
- **Sources:** PANES.md P3b: the context, the correspondence pushed and the nodes reset
  to 1 (`ContextPushed`); the pop's reinitialized nodes, spread and fixed amounts
  (`ContextPopped`) [F95 pp. 87-89]; a banner "no context push in this run" while there
  has been none (ANSWERS.md 11: a push happens in well under one percent of runs, and
  tests may look for a seed on which a pane fires, searched once and recorded).
  iterations.md item 08: search once for a problem and seed with a push under
  `standard`, widening to `french-faults` if none is found in a bounded search. D-021:
  the push and the pop happen before a codelet runs, inside `World.step`, so their
  events carry the codelet's clock; D-070: what the push resets. `Snapshot.context` is
  `{correspondence, reset, pushed_at}` or None.
- **Choice:** the model keeps `context`, a `Push` (index, clock, correspondence, the
  (node, before) pairs) from a `ContextPushed` until the `ContextPopped`, and `last_pop`,
  a `Pop` with the event's five fields; `snapshot_context()` gives the snapshot's form,
  and the test checks it at every snapshot index of the push run. The layout rings the
  reset nodes (dashed magenta) while the context is in force and the restored nodes
  (blue) while the clock is the pop's, that is through the pop's codelet, so a step by
  codelet shows the pop and a step by event shows it for the rest of its step. The
  banner along the top reads `no context push in this run` while there has been none
  (muted), "inside cp5 since codelet 43: 5 nodes reset to 1 (...)" with the counts while
  inside (magenta), and "popped cp5 at codelet 44: 5 reinitialized, spread into N, fixed
  +180 over 6; pushes 1, pops 1" after the pop (blue), where "spread into N" counts the
  nodes that received anything (the spread of a pop whose reset nodes' neighbours are
  at the floor is all zeros). The hover text of a touched node says what the push or
  the pop did to it ("reset to 1 by the push of cp5 at codelet 43 (was 67.0)",
  "restored to 61.0 by the pop of cp5 at codelet 44: +60 fixed (2 slippage concepts)").
  The search (2026-10-13): the 53 problems on seeds 1 to 40 under `standard` (2,120
  runs, no push), then under `french-faults` (2,120 runs, no push), then `standard` on
  seeds 41 to 300 (13,780 runs): one push, **`blockage-07` seed 116**, a run of 64
  codelets and 618 events in which examine-cp(cp5) runs at codelet 43 (the push, event
  416, resetting mirror symmetry, diagonal symmetry, many, sameness and horizontal) and
  the next codelet pops (event 423, clock 44, fixed +30 to many twice, sameness twice
  and horizontal twice). Under an edited configuration that lengthens the runs
  (`answer_post_rate` 0, `max_codelets` 400) pushes are common: `blockage-06` seeds 1
  and 4 to 6, `buridan-01` seeds 1 to 6, `buridan-03` seed 3 give 6 to 45 each, past
  codelet 150. The tests use the unedited seed. With the roles in the engine (the roles
  loop, item 03b, 2026-10-14) `blockage-07` seed 116 no longer pushes; the search was run
  again over the 53 problems on seeds 1 to 300 under `standard` (15,900 runs): two pushes,
  **`blockage-08` seed 111** (78 codelets, 717 events) and `surround-06` seed 179 (89
  codelets); the tests use the first. With the geometric family (item 04, 2026-10-14)
  `blockage-08` seed 111 no longer pushes either; the same search (15,900 runs) found
  pushes in 17 runs, the first by seed **`surround-04` seed 20** (68 codelets, 693
  events, one push and one pop); the tests use that one. With the four role detectors,
  the tenth static factor and the C1 pairs (item 05, 2026-10-14) `surround-04` seed 20
  no longer pushes; the same search (the 53 problems on seeds 1 to 300 under `standard`,
  15,900 runs) found pushes in 17 runs, the first by seed **`surround-07` seed 13** (90
  codelets, 957 events): examine-cp(cp1) pushes at codelet 82 (event 870, resetting
  mirror symmetry, diagonal symmetry, left, sameness, many and horizontal) and the next
  codelet pops (event 880, clock 83: 6 reinitialized, spread into 3, fixed +30 to 17
  entries, 9 nodes restored); the tests use that one. The other 16: `surround-07` seed
  242, `ch2-fig12` 33, `ch2-fig13` 34 and 174, `blockage-07` 54, `surround-06` 105, 157
  and 188, `blockage-05` 117, `surround-03` 146, `ch2-fig19` 148, `blockage-04` 189,
  `ch2-fig10` 228, `surround-09` 273, 277 and 290. With the ROW and COLUMN detectors,
  the re-cast and its recast-role codelet (item 07, 2026-10-14) `surround-07` seed 13 no
  longer pushes; the same search (the 53 problems on seeds 1 to 300 under `standard`,
  15,900 runs) found pushes in 10 runs, the first by seed **`ch2-fig12` seed 16** (113
  codelets, 1200 events): examine-cp(cp9) pushes at codelet 98 (event 1040, index 1039,
  resetting mirror symmetry, diagonal symmetry, left, sameness, many and horizontal) and
  the next codelet pops (event 1048, index 1047, clock 99: 6 reinitialized, spread into 4,
  fixed +30 to 10 entries, 6 nodes restored); the tests use that one. The other 9:
  `ch2-fig13` seed 21, `ch2-fig11` 82, `ch2-fig13` 102, `blockage-06` 105, `ch2-fig10` 105
  (4 pushes), `surround-02` 222, `surround-05` 266 (2 pushes), `surround-07` 212 and 236.
  With the GAP, LONER, STRAY and CORNER detectors, the midline on tables of loners, the C1
  loner, corner and part pairs and the ninth salience term (item 08, 2026-10-14) `ch2-fig12`
  seed 16 no longer pushes; the same search (the 53 problems on seeds 1 to 300 under
  `standard`, 15,900 runs) found pushes in 35 runs, the first by seed **`ch2-fig08` seed 2**
  (115 codelets, 1307 events; Eliza touches cup2, T 24.6), the first run used by the tests
  with two pushes and two pops, both of cp1: examine-cp(cp1) pushes at codelet 75 (event
  829, index 828, resetting mirror symmetry, diagonal symmetry, left, sameness, many and
  horizontal) and the next codelet, propose-role-cp at 76, pops (event 837, index 836: 6
  reinitialized, spread into 4, fixed +30 to 10 entries, 6 nodes restored, many and
  horizontal by the fixed term alone); then create-cp(small-glass1 salt-shaker1 cp1)
  pushes cp1 again at codelet 77 (event 847, index 846) and find-group at 78 pops it (event
  857, index 856: spread into 5), so the banner at the end reads "pushes 2, pops 2"; the
  tests use that one since item 08's runs. The other 34: `blockage-06` seed 184,
  `blockage-07` 30, 57, 140, 162 and 251, `blockage-08` 293, `ch2-fig08` 88 and 195 (2 pushes
  each), `ch2-fig10` 138, `ch2-fig11` 177 and 297 (3 pushes), `ch2-fig12` 35 and 67,
  `ch2-fig13` 66 and 170, `ch2-fig17` 184, `ch2-fig19` 117 and 212, `surround-02` 146,
  `surround-03` 95 (3 pushes) and 299, `surround-04` 182 (2 pushes) and 238, `surround-06`
  44 and 135, `surround-07` 235 (2 pushes), `surround-08` 142 and 144, `surround-09` 48, 77,
  120 (2 pushes), 125 and 133.
  `surround-07` 235 (2 pushes), `surround-08` 142 and 144, `surround-09` 48, 77,
  120 (2 pushes), 125 and 133.
- **Note (2026-10-14, the roles loop's item 09):** with the FIGURE, GROUND, ECHO, TWIN and
  TYPE_KEY detectors, consumer C1's figure, ground, echo and key pairs, C5's key identity
  and the re-cast's TWIN tier (item 09) `ch2-fig08` seed 2 no longer pushes; the same search
  (the 53 problems on seeds 1 to 300 under `standard`, 15,900 runs) found pushes in 82
  runs, the first by seed **`ch2-fig11` seed 3** (126 codelets, 1467 events; Eliza touches
  spoon3, T 30.5), the first run used by the tests with four pushes and four pops, all of
  cp4: examine-cp(cp4) pushes at codelet 73 (event 852, index 851, resetting mirror
  symmetry, diagonal symmetry, left, sameness, many and horizontal) and the next codelet,
  create-cp(spoon4 cup1) at 74, pops (event 860, index 859: 6 reinitialized, spread into 3,
  fixed +30 to 10 entries, 6 nodes restored, left, many and horizontal by the fixed term
  alone); examine-cp(cp4) pushes again at codelets 87, 94 and 109 (indexes 994, 1047 and
  1228) and the next codelet pops each time (build-obj-0 at 88, pick-strong-cp at 95,
  propose-role-cp at 110; indexes 1001, 1054 and 1235; the last pop fixed +30 to 31
  entries), so the banner at the end reads "pushes 4, pops 4"; the tests use that one
  since item 09's runs. The other 81: `blockage-05` seed 155 (2 pushes), `blockage-06` 60,
  174 and 203 (2 each), `blockage-07` 152, 166 and 176, `buridan-01` 48 (2) and 175,
  `buridan-02` 61, 71, 112 (2), 122, 144 and 196, `buridan-03` 90 and 154, `buridan-04` 82,
  169, 211, 214 and 248, `buridan-05` 37, `buridan-06` 20 and 27, `buridan-07` 28, 33, 76
  and 216, `buridan-08` 16 and 128 (2), `buridan-09` 154, 221 and 235, `buridan-11` 112,
  171, 221 and 282, `ch2-fig08` 182, `ch2-fig09` 240 (2), `ch2-fig11` 235 (2), 251 and 295,
  `ch2-fig12` 87 (2), 91 and 247 (2), `ch2-fig13` 112 and 170, `ch2-fig14` 220, `ch2-fig17`
  60, 174 and 203 (2 each), `ch2-fig18` 264, `ch2-fig19` 117 and 212, `surround-02` 99 and
  239, `surround-04` 93 and 185, `surround-05` 235, `surround-06` 17, 132, 152, 213 and 237,
  `surround-07` 17 and 235 (2), `surround-08` 144, 277 and 294, `surround-09` 77, 125, 127
  (2), 252 and 263 (2), `surround-10` 11, 60, 153, 154, 236 and 237.
- **Reason:** the events say exactly what the push and the pop did, and the marks last
  as long as the thing they mark (the context; the pop's codelet) rather than for a
  fixed count; the banner's exact words are the author's; a seed found under the
  profile the GUI runs by default needs no edit to show the pane working, and the
  edited runs are recorded for a demonstration with many pushes.
- **Alternatives:** marks for one event, as the grid's jolt (a step by codelet would
  then never show a pop); a push made by hand through the engine's test helpers
  (ANSWERS.md 11 forbids feeding a run); the edited configuration as the test's run
  (3,000 events, slower, and a capped run).

## The Coderack bars and the rack list (item 09)

### G-036: The Coderack's model: the rack from the events, exact by type at every index; the chances recomputed from the event's exponent with the engine's weights; twins alike in all but their posting are told apart by nothing in the events, so the list is re-based on each new snapshot and the events since re-applied
- **Sources:** PANES.md P4: the shares in the strip's order with French's labels
  (`Coderack.shares()`, `config.codelet_label`) and the number of codelets run
  [F95 p. 97], fed by `CodeletPosted`, `CodeletChosen`, `CodeletRemoved` and
  `Snapshot.coderack`; P4b: every codelet with its urgency, generation, age and the chance
  at the last draw, the chosen codelet with `chance`, `a` and the rack size, and the test
  that the chances of `Coderack.chances(a)` sum to 1, computed on the snapshot's copy.
  `docs/engine-api.md`: `Coderack.chances(a)` draws nothing; `CodeletChosen.chance` is the
  last draw's chance; `CodeletPosted` carries no `posted_at`, but its `clock` is the
  codelet's (`World.post` records the posting at the clock `make_codelet` stamps, D-053);
  `CodeletChosen` carries the type, arguments, generation and urgency but not the posting
  or `by`; `CodeletRemoved` carries `posted_at`. The engine's draw (`Coderack.choose`,
  D-060) takes one of several codelets alike in type, arguments, generation and urgency
  by its generator. G-020 and G-030 build the table's structures and the Slipnet from the
  events alone; G-024 dates the overlays by the snapshot and never rolls them back.
- **Choice:** `models/coderack.py`'s `CoderackModel` rebuilds the rack from the events: a
  posting hangs a `Waiting` at the end with `posted_at` the event's clock; a choice gives
  every codelet the chance `chances_of` computes from the event's `a` with the engine's
  `softened_weights` (the same arithmetic, so bit-identical to the chance the engine
  records) and takes the first codelet off that matches the event's type, arguments,
  generation and urgency; a removal takes the one with the event's type, arguments and
  `posted_at`, else the oldest of that type and arguments. The counts by type, so the
  strip's shares and the hover text's "N waiting", are therefore exact at every event
  index from the events alone, whichever twin went (checked at every snapshot of three
  runs by `test_coderack_model.py`, and of the 53 problems on seeds 1 to 3, 159 runs and
  8,797 snapshots, by the item's measurement). The list is not: when the engine's draw
  takes the newer of two twins the model has taken the older, and in 7,281 of those 8,797
  snapshots a twin's `posted_at`, `by` and age were wrong; and the fallback of a later
  removal can then take a codelet of another generation, so the never-corrected rack
  drifts in urgency too. So `rebase(history, position)`, called by the rack list's
  `redraw` at every event, puts the rack as the newest snapshot at or before the position
  has it (`Snapshot.coderack["codelets"]`, the engine's own word) and re-applies the rack
  changes of the events since (`apply`, without recording): the list is exact at every
  step boundary while the run is live and at every index when seeking, and within a step
  it is exact as a multiset of what the events can tell apart (type, arguments, raw and
  adjusted urgency, generation, chance), measured at all 8,797 snapshots with no
  mismatch; only a twin's posting can be wrong until the step ends. Past 300 codelets the
  snapshots are keyframes every 16 steps (G-007) and the drift lasts up to 16 steps.
  `mismatches` counts an event the rack could not follow at all; it is 0 on every run
  measured.
- **Reason:** the bars (P4) need only the counts by type, which the events give exactly;
  the list (P4b) needs each codelet's posting, which the events do not give for the
  chosen one, and the snapshot has it after every step; re-basing keeps the model a
  function of the history (a replayed session needs nothing more) and keeps the view
  exact where the engine has spoken. Reading the `Step` the controller gets back from
  `World.step()` would say which twin ran, but it comes after the step as the snapshot
  does and would be a new read of the world beyond engine-api.md's list.
- **Alternatives:** the snapshot alone (empty inside a step, and one step behind at the
  live edge); the oldest twin without re-basing (wrong in most runs); a `posted_at` on
  `CodeletChosen` (an engine change, written under "For the author": with it the rack
  would be exact from the events alone and `rebase` would go).
  Dated note (2026-10-14, the roles loop's item 09): the twin drift reached a draw on a
  golden run. On surround-05 seed 2 the engine drew, at codelet 53, one of seven
  check-roles twins of generation 1 (posted at 31) where the events-only model took the
  oldest twin (posted at 14); the age removals at codelets 75, 78 and 80 then named
  postings the model no longer had, and `_remove`'s fallback (the oldest of that type and
  arguments) took a generation-0 twin the third time, so the generation-0 draw at codelet
  83 matched nothing and the rack stayed one codelet too large for the last nine draws:
  10 mismatches, pinned by `tests/gui/test_coderack_model.py`. The re-based list (P4b) is
  exact; the strip (P4) is fed the events alone and shows one check-roles too many from
  codelet 83 to the end of that run. The fix is GUI work for item 14 (re-base the strip's
  model as the list's is, or `CodeletRemoved` carrying the generation, an engine change
  for the author); nothing was changed here.

### G-037: The strip as French's screen drew it: the box with a bar and its percent per type in the strip's order, the labels on two staggered lines, the counter on the pane's top row over the strip's right end, the chosen bar outlined from its choice to the next; the legend as a checkable action remembered under `controls/coderack-*`; the Coderack group taller in the built-in layout
- **Sources:** "Below the table is a representation of the Coderack, indicating the
  percentage of particular codelet types waiting to run. The total number of codelets run
  so far is also indicated." [F95 p. 97]; the dumps: a long box under the table, a black
  bar at each label with its percent printed above it, the 23 labels along the bottom
  edge in one line, "Number of codelets run: 55" over the box's right end
  [F95 p. 107 (img)]; the legend of the full names printed beside Fig. 4.1 [F95 p. 96
  (img)] (D-051). PANES.md P4: the bar of the codelet just chosen outlined; the legend
  from `codelets.CATALOGUE`. G-031 makes the grid's geometry a pure function; G-032 keeps
  the grid's option as the pane's own action, on its bar and in the View menu, remembered
  under `controls/grid-*`; G-014 asks for small minimums. The built-in layout gives the
  bottom area's two groups 110 and 190 pixels (item 02's `BOTTOM_HEIGHTS`).
- **Choice:** `strip_layout(model, width, height)` is a pure function: the box, 23 slots
  in `config.CODELET_TYPES`' order, a black bar of `BAR_SHARE` (0.6) of its slot as high
  as the type's percent of the rack with the percent rounded to an integer printed above
  it (nothing for a type with none), the labels under the box, and the text "Number of
  codelets run: N" with N the clock. The pane shows the counter as a label at the right
  end of its top row, over the strip's right end (the row also holds the legend button and
  the pane's caption); the strip widget paints the rest. The labels sit on two lines, the
  odd ones a line lower (`STAGGER`): 23 labels of up to nine characters do not fit one
  line at the widths the dock gets. The bar of the type of the codelet just chosen is
  outlined in magenta (the jolt's colour, G-030) from its `CodeletChosen` to the next,
  that is while that codelet is the one running or just run; a type with no codelet left
  gets a short outline on the box's floor. Hovering a slot gives the type's legend entry
  (its label, name, role, job and whether the job is from the text or inferred, D-051) with
  how many wait and their share. The legend is a checkable action, `legend_action`, on the
  pane's bar and in the View menu's "Coderack" submenu (one state in two places), showing
  `legend_text()`, one line per type, in a read-only text beside the strip; remembered
  under `controls/coderack-legend`. The built-in layout now asks `resizeDocks` for 170
  and 180 pixels for the two bottom groups (each with its tab bar; the Coderack dock gets
  about 140), so that the bars have some 60 pixels of height; `default_layout.json` was
  written again from the built-in arrangement.
- **Reason:** the strip is the one Coderack display French shows, so it is drawn as the
  dumps show it; the counter on the top row keeps the dump's place for it without a line
  of the strip's height; staggered labels keep every label legible where one line would
  elide most; the outline lasts while the chosen codelet is the current one, as the
  thermometer's reading does; the legend is a text, not a drawing, because the catalogue
  is a table.
- **Alternatives:** labels rotated or elided on one line; the outline for the choice's one
  event, as the grid's jolt (a step by codelet would never show it); the legend as a
  dialog; the dock heights of item 02 (the bars were three pixels high).

### G-038: The rack list, GUI design after the Numbo GUI's coderack view: nine columns, the oldest first; the chosen band with the chance, the exponent and the rack size, tinted at the choice; what the codelet did; a removed codelet greyed at its old position for the one event of its removal; a twin's posting left unsaid; greyscale text
- **Sources:** PANES.md P4b (GUI design, after the Numbo GUI's `coderack_view.py`): every
  codelet waiting (type, label, arguments, raw and adjusted urgency, generation, age, the
  chance at the last draw), the oldest first; the chosen codelet on a band with `chance`,
  `a` and the rack size; removed codelets (`age`, `capacity`, D-053) in grey for one
  event; `CodeletRan` and `CodeletFizzled` for the band. The Numbo GUI's coderack view
  draws the codelet just chosen on a magenta-tinted band when the latest event was that
  choice, and a line "N waiting (at most M this run)". French's form of a codelet,
  "(codelet-name codelet-age codelet-urgency)" [F95 p. 77], with the generation tag
  [F95 p. 81] and the "Total Coderack urgency" [F95 p. 77]. ANSWERS.md 4: a pane with no
  figure in the sources says "GUI design, not French's".
- **Choice:** `RackListWidget`, a `QAbstractScrollArea` painting on its viewport: the
  band's two lines (`band_lines`: "codelet 7: FND-GP()  chance 0.032 of 35 at a 1.28
  (T 80.0)", then the urgency and generation, the raw urgency with who posted it and when
  and its age, or "one of 16 alike on the rack" when the events cannot say which twin
  ran, G-036, then "running", "fizzled: ..." or the `CodeletRan` detail with the types
  posted), tinted and in magenta while the event shown is the `CodeletChosen` itself
  (`chosen_fresh`), kept until the next choice; the summary line (how many wait, the most
  this run, the total urgency); the column headings; then one row per codelet of the rack
  in rack order (`COLUMNS`: #, codelet, raw, urgency, gen, posted, age, chance, by), the
  band, the summary and the headings fixed at the top and the rows scrolling under them.
  A codelet the event shown removed is put back in grey at the position it had, with
  "(removed: age)" in its last cell, for that one event only (`removed_now`); consecutive
  removals (the aged codelets go one event each) show one at a time, and a step by
  codelet shows none, as the grid's jolt (G-030). Hovering a row gives the codelet's
  catalogue entry and its numbers with their decisions. The text is drawn with greyscale
  antialiasing (`NoSubpixelAntialias`), so that a row's colour can be read off its pixels
  by the tests and renders are the same in a window and alone. The pane's label says
  "GUI design, not French's".
- **Reason:** the list is Numbo's design, kept where the engine's rack has the same
  shape; the band answers the question the strip cannot (why this codelet, with what
  chance); the greying follows PANES.md to the letter; the twin's posting is left unsaid
  rather than guessed (ANSWERS.md 11's honesty).
- **Alternatives:** a `QTableWidget` (its paint count is not the view's to count, and
  its minimum size is larger); the removed codelets listed under the rack instead of at
  their positions; naming the oldest twin as the one that ran.
- **Note (2026-10-14, the roles loop's item 05):** the position a greyed row is put back
  at was recorded when the removal's event was taken, on the model's rack as it then
  stood; a seek replays the events with no re-basing between, so its rack drifts by the
  twins and the same removal was recorded at another position than while playing (found
  by `test_log_view.py`'s seek test at event 750 of the quick start, inside the
  age-removal burst after the snapshot at 745). `CoderackModel.rebase` now records the
  removals since the snapshot again as it re-applies the events, so the position is read
  on the re-based rack either way. The racks themselves were equal; only the greyed row's
  position differed.

## The Worldview and the competitions (item 10)

### G-039: The Worldview's entries come from the events through the composed structures model, exact at every index; the numbers (the points, the rating with its three terms and its pairs) from the snapshot in force, dated; the tap records every correspondence's rivals beside each snapshot; a model that has seen no events lists the snapshot's correspondences, so the chapter 4 state can be shown
- **Sources:** PANES.md P5: the correspondences in the Worldview and the shadow cabinet
  with their static, dynamic and bonus parts, rating, wins, losses and slippages; the
  rating's strength, coherence, parallelism and total with its pairs (D-042, D-043),
  rivals and incoherent pairs marked differently [F95 p. 105]; the events
  `CorrespondenceProposed`, `CorrespondenceBuilt`, `CorrespondenceTested`,
  `WorldviewChanged`, `CorrespondenceRemoved`, `GroupProposed`, `GroupStagePassed` and the
  state `Snapshot.correspondences`, `worldview`, `proposals`; the tests: the entries equal
  the snapshot's, the rating equals `worldview.current` on the same state, a model test
  on the hand-built chapter 4 state. `docs/engine-api.md` lists `worldview.current`,
  `summary` and `rivals_of` as drawing nothing, and TASK.md's whitelist names them. The
  events carry each correspondence's membership, wins, losses and slippages exactly
  (G-020, tested against every snapshot of two runs), but not its points as they change
  (the static points move with the Slipnet's distances and the dynamic points with the
  Worldview, D-036, D-037), nor the rating's pairs, nor whom a correspondence outside the
  Worldview would have to fight. ANSWERS.md 11: the chapter 4 state built by hand in
  `tests/test_worldview.py` may feed a model test, never a run.
- **Choice:** `WorldviewModel` composes item 04's `StructuresModel` for the events: the
  entries, their membership, wins, losses, slippages and standing are the events' and
  exact at every index (`entries()`: the Worldview's, then the shadow cabinet's, each in
  the order built). `update(snapshot, terms, readings)` keeps the snapshot's numbers,
  dated by its clock and never rolled back (G-020's guard): each correspondence's
  static, dynamic and bonus points as they stand (an entry whose snapshot has not come
  yet shows the first evaluation's, marked "first evaluation"), `Snapshot.worldview`
  (the three terms, the total, `coherent` and the three pair lists, which is
  `worldview.current` on that state), and the rivals. The tap reads
  `worldview_readings(world)` beside each snapshot (`models/tap.py`, the way G-020 and
  G-024 read the terms and the saliences): `worldview.rivals_of` for every correspondence
  in the Workspace, copied into tuples; `SnapshotHistory.record` and `EventLog` gained
  `worldview_at(index)`. `rating()` gives a `RatingView` with `as_of` the snapshot's clock
  and `latest`, the last `WorldviewChanged` since that snapshot (its total, coherence and
  reason), so that a step inside a step says what changed before the next snapshot dates
  it. A model that has seen no events (`structures.events == 0`) lists the snapshot's
  correspondences (`state_of_snapshot`): that is how the chapter 4 state of
  `tests/test_worldview.py` (`fig_4_7`, loaded by path by the GUI's test) is shown, two
  correspondences in the Worldview with their incoherent and parallel pair and the
  cup-glass newcomer as cp1's rival. The proposals come from the structures model's
  proposal states (D-024: the stage reached, failed, or built as which group, by the
  members).
- **Reason:** the membership must be exact at every event (a step shows one change,
  ANSWERS.md 5), which the events give and the snapshot cannot between steps; the
  points and the pairs are the engine's own numbers, read through the whitelist, dated
  rather than recomputed; the rivals are the one thing neither the events nor the
  snapshot carry, and `rivals_of` is on the whitelist. The gate's determinism check
  confirms the reads draw nothing (12 comparisons identical), and the model test
  checks the draw count.
- **Alternatives:** re-basing on each snapshot as the rack list does (G-036; the same
  result at every step boundary, more code); computing the rivals in the GUI from the
  shared-end rule of [F95 p. 101] (would miss the inside-only rule of D-044 when a
  group correspondence stands); adding `rivals` to the snapshot (an engine change).

### G-040: The Worldview pane's picture, GUI design from the text: lines of text by role, a solid bar before a Worldview entry and a dashed one before a shadow entry (Fig. 3.11's convention), an incoherent pair in a warning colour and a rival said in words, the banner in plain words, the Proposals as a second tab of the same pane; the lines widget shared with the competitions pane
- **Sources:** PANES.md P5 (GUI design, not French's, from the text of pp. 67-74; a
  **Proposals** tab; "one correspondence; no pairs to rate" in most runs). No figure of
  the Worldview's per-correspondence points exists (PANES.md section 0). Fig. 3.11 draws
  the Worldview's correspondence solid and the rival dashed [F95 p. 72 (img)]; rivals
  "are forbidden to belong to the Worldview at the same time" [F95 p. 105] and
  incoherent correspondences "can nonetheless coexist" [F95 p. 105]. ANSWERS.md 11: say
  so plainly when the Worldview holds one correspondence. The Numbo GUI's coderack view
  and the rack list (G-038) paint a model's rows on a scroll area's viewport.
- **Choice:** the model gives the pane's text as `TextLine`s with a role (`heading`,
  `worldview`, `shadow`, `detail`, `warning`, `muted`), the name of the structure the
  line is about and, for the competitions, two cells. `LinesWidget` (a
  `QAbstractScrollArea`, as the rack list) paints them: a solid bar at the left of a
  Worldview entry and a dashed bar at the left of a shadow entry (the dashes as long as
  the bar is wide, so that a line of 16 px shows them), headings bold, details indented,
  warnings in a dark orange, the selected structure's lines tinted (G-022) and the lines
  of the event shown tinted magenta (as the rack's chosen band); greyscale text
  antialiasing, as G-038. Each entry takes two lines: the ends with "(groups)" when both
  are groups, the rating as the sum of its three parts and the wins and losses; then
  the standing in words, the slippages, "rival of ...", "incoherent with ...",
  "coherent with ..." and "parallel to ...", the second line in the warning role when the
  entry is in an incoherent pair. The rating's block: the total as its three terms "as of
  codelet N", the latest change since, and the three pair lists, the incoherent one in
  the warning role when it is not empty. The banner is the first line: "no run yet", "no
  correspondence in the Worldview yet" (with how many are outside it), "one
  correspondence; no pairs to rate", or "N correspondences; K pairs rated". The
  Proposals are the second tab of a `QTabWidget` inside the pane (PANES.md's tab),
  painted by a second `LinesWidget` over `ProposalLines`; the pane's `paints` sums both
  (the hidden tab paints nothing). Hovering a line gives the structures' hover text with
  the rivals added, dated; a left click toggles the structure in the shared selection,
  the empty space clears it. The competitions pane paints its own model through the same
  widget. The pane's label says "GUI design, not French's". A visible `LinesWidget`
  takes its lines as soon as `refresh` marks them stale (the roles loop, item 04b,
  2026-10-14): the scroll range, and so the bar, then change before the pass that paints
  the viewport, where they used to change inside its paint, so that at the first event of
  a second run in one window the window showed the last run's handle while a fresh render
  showed none (the one mismatch of the per-event pixel probe, `(0, 'worldview-view')`, and
  the competitions pane's twin of it under the second tabs). A widget under a hidden tab
  stays lazy, as G-058 asks.
- **Reason:** text is the honest form for numbers the sources never drew; the bars keep
  the one convention French gave (solid against dashed); the two marks (a rival in words,
  an incoherent pair in colour) follow p. 105's distinction; one widget for three lists
  keeps the views thin.
- **Alternatives:** a `QTableWidget` or a `QTreeWidget` (their paint counts are not the
  view's to count, G-038); the Proposals as a section under the shadow cabinet instead
  of a tab; drawing each entry as a small table picture.

### G-041: The competitions from the events alone: one fight per `CompetitionHeld` with the Old and Rival Worldviews rebuilt from `WorldviewChanged`, side by side as two small tables with the old correspondences solid and the newcomer dashed, the ratings, the chance, the exponent and the winner under them; entries without a fight listed as such; `no competition in this run` until one is held; the fight the event shown reports tinted; a click selects the newcomer
- **Sources:** PANES.md P6 (GUI design, after Fig. 3.11 [F95 p. 72 (img)]): each fight in
  order, the old Worldview against the rival one side by side as small tables, the old
  ones solid and the newcomer dashed; under them the two ratings, the chance, the
  exponent a and the winner; entries without a fight listed as "entered without a
  fight" [F95 p. 101]; "no competition in this run" when there is none (ANSWERS.md 11);
  the events `CompetitionHeld` and `WorldviewChanged`, the state `Snapshot.competitions`
  and `changes`; the tests: one entry per `CompetitionHeld`, the winner equals
  `rival_wins`, the empty banner. "All the conflicting correspondences are temporarily
  removed (subjunctively) from the Old Worldview, and the new correspondence is
  temporarily inserted (subjunctively) in their place to form the Rival Worldview." [F95
  p. 72] An uncontested entry logs its `WorldviewChanged` before its `CompetitionHeld`
  (`worldview.admit`, then `_hold`, D-040); a won fight logs the change after it.
- **Choice:** `CompetitionsModel` is built from the events alone and is exact at every
  index: a `Fight` per `CompetitionHeld` with the event's fields, the Old Worldview (the
  names in the Worldview before the newcomer entered, kept from the `WorldviewChanged`
  events in the order entered, the newcomer left out for an uncontested entry) and the
  Rival Worldview (the old less the rivals plus the newcomer), and the rating the
  `WorldviewChanged` with reason `won` that follows reports; the model's logs equal
  `Snapshot.competitions` and `Snapshot.changes` at every snapshot of two runs (tested).
  The pane lists the banner ("no competition in this run" until one is held, then "N
  competitions: K fights, M entered without a fight"), then each competition: an entry
  without a fight on one line with the Worldview's rating before and after; a fight as a
  heading ("3. codelet 48: cp4 against cp1"), two cells "Old Worldview" and "Rival
  Worldview", one row per correspondence with its ends (the old ones in the `worldview`
  role, solid; the newcomer in the `shadow` role, dashed), the two ratings, then the
  chance, the exponent and the winner in words ("the Rival Worldview won", "the Old
  Worldview held") with the rating after the change. The lines of the fight whose
  `CompetitionHeld` is the event shown are tinted magenta for that one event (as the
  rack's chosen band, G-038). Hovering a fight's lines gives its text; a click selects
  its newcomer as a correspondence in the shared selection (G-022), so the table and the
  Worldview pane highlight it.
- **Reason:** the events carry everything the pane shows, so no snapshot is needed and
  seeking is exact; the two tables are Fig. 3.11's picture in text; the newcomer is what
  a reader wants to find on the table after a fight.
- **Alternatives:** drawing the two Worldviews as pictures of the table (the table pane
  already shows the lines, solid and dashed, G-021); selecting every correspondence of
  the two Worldviews at once (the selection holds one element, G-022).

## The trace, the event log and the timeline (item 11)

### G-042: The trace tab from the engine's own `TextTrace`, every line tagged with its block's first event and the event that printed it; lines after the position greyed, the block tinted, a click seeks to the block; the Events tab Numbo's rows painted whole at every event; Save trace and Save events write the CLI's text byte for byte
- **Sources:** PANES.md P10: the Trace tab shows the text trace exactly as the CLI prints
  it (`observe.TextTrace`, normal and `--quiet`), every line tagged with its event index,
  the lines after the position greyed when not live, a click seeks, Save trace; the Events
  tab every event with a Kinds filter, double-click or Enter to seek, Save as JSON lines
  byte for byte the CLI's `--json`. Numbo GUI, `log_view.py`: the rows painted straight
  from the history, the run's next event retinting two rows and blitting the rest. The
  engine prints a codelet's line only when the next codelet is chosen or the run ends
  (`TextTrace.flush`, D-065), and the trace is an observer: one event may print several
  lines (the codelet line and its notes), and most events print none. `docs/trace-format.md`
  defines both forms. ANSWERS.md 5: every event drawn, exactly one change per step.
- **Choice:** `models/trace.py`'s `TraceModel` feeds the history's events to two of the
  engine's own `TextTrace`s (normal and quiet) printing nowhere, so the lines are the CLI's
  by construction (tested against the CLI as a subprocess, normal, `--quiet` and `--json`).
  Each line is a `TraceLine` with two indexes: `start`, the first event of its block (the
  `CodeletChosen` for a codelet line and the notes under it, 0 for the first line and the
  start's notes, the `RunEnded` for the report line) and `end`, the event at whose arrival
  the CLI printed it. The pane greys the lines whose `start` is after the position shown,
  tints the block the position falls in (as the rack's chosen band, G-038) and seeks to
  `start` on a single click (PANES.md P10's "click a line"). Live, the trace holds the lines
  the CLI has printed by then: the current codelet's line appears when the next is chosen,
  as on a terminal; after a seek back, every line of the run is known and the later ones
  are greyed. The Events tab is the Numbo GUI's event log over `EventLog.visible`, the rows
  `index  name  clock  field=value ...` (`models/event_log.py`'s `row_text`, the compound
  values as compact JSON), the Kinds menu over the 27 event names, double-click or Enter to
  seek. Both lists are one `RowList` (a `QAbstractScrollArea` painting the rows in view
  straight from the source); at every event the rows in view are painted whole (one paint)
  rather than the Numbo GUI's two retinted rows, so that the window's paint-once rule holds
  whatever is in view (a hidden kind, Follow off) and the trace, which adds no row at most
  events, is still painted once. Greyscale text antialiasing (G-038's lesson). Session >
  Save trace… writes the form shown (normal or quiet) and Save events as JSON lines… the
  whole history as `observe.json_line` per event; both are enabled once there is a history,
  and the pane's buttons open the same dialogs. The selection is not used: the log has no
  element to select.
- **Reason:** reusing the engine's `TextTrace` is the only way to be byte-identical with
  the CLI without copying its code; the two indexes let the pane be honest about when the
  CLI prints and still seek to where a line's events begin; painting the rows in view whole
  costs about the same as the two-row retint (the rows in view, not the events, bound it)
  and keeps the probe exact.
- **Alternatives:** showing the current codelet's line as it is being assembled (needs the
  trace's private state, or a second trace flushed early; left out: the terminal does not
  show it either); greying by `end` (the line would be grey for the whole codelet it
  describes); remembering the quiet switch in the settings (left out: a session's trace is
  the normal form).

### G-043: The timeline as Numbo's with the Seek-Whence GUI's strip: the eight marker kinds of PANES.md P11 with their shapes and colours, a thin strip of T under them, the go-to-codelet box; the count and the markers are the whole history's, the cursor marks the position; refused while live by the window
- **Sources:** PANES.md P11: a slider over the events with markers (the first
  `ObjectNoticed`, each `CorrespondenceBuilt`, `CompetitionHeld`, `WorldviewChanged`, the
  pushes and pops, the first `AnswerCodeletPosted`, `AnswerGiven`), back and forward, go to
  codelet N, a strip of T under the slider (P2b's series, thin), `seek_requested` refused
  while live (Numbo's rule). Numbo GUI, `timeline.py`: the slider seeks when released, the
  buttons, the iteration spin box, the fixed-width label, `set_scrubbable`. Seek-Whence GUI,
  `timeline.py` and `models/timeline.py`: a marker strip under the slider painted from a
  Qt-free model, `marker_x` and `index_at`. ANSWERS.md 11: pushes are rare. An uncontested
  entry also logs a `CompetitionHeld` with no rival (D-040).
- **Choice:** `models/timeline.py`'s `TimelineModel` scans the history once (`sync`) into
  `Marker`s of eight kinds and composes a `TemperatureModel` (G-028) for the readings of
  the strip; `marker_style` gives the shapes and colours: the first notice a grey tick and
  the first find-answer posting an orange tick on the top row, a push a purple triangle
  and a pop one pointing down, a correspondence a blue diamond, a Worldview change a green
  diamond, a competition a magenta bar and the answer a black bar on the lower row; an
  uncontested entry's marker says "entered the Worldview without a fight". `timeline.py`'s
  `TimelineBar` is Numbo's bar (back, forward, the slider, the label `event N of M: Name
  codelet k`, a codelet spin box with "Go to codelet", which the window answers with
  `jump_to_codelet` through `EventLog.codelet_index`) with a `TimelineStrip` under the
  slider: the markers on two rows, the strip of T below (0 at the bottom, 100 at the top,
  grey while held, D-049) and a magenta line at the position; a click seeks to the marker
  under the mouse or the nearest event; the tooltip names the marker. The count, the
  slider's range and the markers are the whole history's, so after a seek back the markers
  ahead stay visible and the cursor shows where the views are; while playing, the history
  ends at the position. The window's `_update_actions` makes the bar scrubbable only when
  there is a history and no run is live, and `seek` refuses otherwise.
- **Reason:** the markers are what a reader scrubs to; the strip of T is the run's shape
  at a glance, as French's thermometer is beside every dump; a whole-history timeline is
  what a timeline is for.
- **Alternatives:** a go-to box that reads `codelet 5 / event 402` as the Seek-Whence GUI's
  (left out: Numbo's spin box is the model, and the Events tab seeks to any event); every
  `AnswerCodeletPosted` marked (dozens per run: noise).

## The controls and the stats (item 12)

### G-044: The controls are Numbo's panel with a Qt-free `ControlValues`: the 53 problems by family with the chapter 4 configuration first, the profile, the seed, the read-only cap, seven buttons and the speed slider; the window keeps the next run's inputs and the pane shows the run actually started, whichever way it was started; the inputs round-trip under `controls/*`; a run line along the top is what the pane repaints per event
- **Sources:** PANES.md P9: the problem selector with every problem by family, its title
  and page, the 53 digitized problems as the presets, `surround-05` first [F95 p. 118];
  the seed, the profile and the cap shown read-only (`max_codelets`, editable in P13);
  Play, Pause, Step, Step codelet, Run to end, Stop, Back and the speed slider over
  Numbo's `DELAYS`; the shortcuts Space, Right, Shift+Right, Left, Esc, Ctrl+N; the
  inputs remembered under `controls/*`; the selector, the title and the Stats "Run" line
  agree with the run actually started, whichever way it was started (loop0004 ANSWERS
  10.3). Numbo GUI, `controls.py`: a panel that holds the widgets, parses its texts at
  the start (`inputs()`), shows input errors under the buttons, the slider the mouse's
  only. Seek-Whence GUI, `controls.py` and `models/controls.py` (its decision 49): the
  values as plain text in a Qt-free dataclass, rebuilt at every change, the settings round
  trip checked value by value, the buttons without keyboard focus, the form in a scroll
  area. ANSWERS.md 5: all as in Numbo. G-015 (item 02): the next run's inputs are the
  window's `problem`, `profile` and `seed`, set by `set_next_run`, which the Problem menu
  and the command line call. The window's probe (G-011) expects every view on screen to
  paint exactly once per event; a form of widgets paints nothing by itself.
- **Choice:** `models/controls.py` holds `DELAYS` and `DEFAULT_DELAY` (the window's own
  constants now read them), the defaults (`surround-05`, `standard`, seed 1, the CLI's),
  `catalogue()`, the 53 problems as `ProblemEntry`s (name, title, family, page) with the
  chapter 4 configuration first and then each family in `problems.FAMILIES`' order
  [F95 p. 114], `parse_seed` (any integer, as the CLI's `--seed`), `cap_text` (the next
  run's `max_codelets` and where to edit it), the `ControlValues` dataclass (problem,
  profile, the seed as text, the speed index), `run_inputs` (values to the (problem,
  profile, seed) triple, raising `InputError`), `values_of` (the reverse, for a run started
  any other way: a problem not in the library or a configuration that is no profile leave
  the previous choice standing, the stats pane saying what the run really was) and the
  `to_settings`/`from_settings` round trip under `controls/problem`, `profile`, `seed` and
  `speed`, every value checked and a bad one replaced by its default (the keys item 02
  already used; `_restore_inputs` and `_save_settings` now go through the model).
  `controls.py`'s `ControlsPanel` is Numbo's: the selector by family with disabled family
  headers (the Seek-Whence GUI's way), a line under it with the problem's title, family
  and page, the profile combo over `PROFILES`, the seed field, the cap label, the seven
  buttons in a grid (Back added, ANSWERS.md 5) with no keyboard focus, the speed slider
  and its label, the error label; the form in a scroll area (G-014). The panel emits
  `inputs_changed` when the problem, the profile or the seed changes; the window's
  `_inputs_from_controls` parses and calls `set_next_run`, or shows the error in the pane
  and refuses the next start (`_start`), as Numbo's error does. `set_next_run` shows its
  inputs back in the pane (`show_run`) with the cap of `next_config()`; `start_run`,
  whichever way it is called (the buttons, the keys, the Problem menu, `--problem`, a
  test's direct call, later a replayed session or Do this!), shows the run actually
  started in the pane and the title (`_show_started`) and makes it the next run's inputs
  when it is a problem of the library and a profile; the stats pane's Run line comes from
  the `RunStarted` event itself, so the three agree by construction. The speed is one
  state in three places: the slider, Run > Speed and the controller (`set_delay` sets all
  three). The pane keeps G-011's view protocol with a `RunLine`, a 16 px strip along the
  top of the pane, outside the scroll area so that it is always in view, which says the
  controller's state and the events shown and is the one thing the pane repaints per
  event (`paints` counts its paints); `set_state` enables Pause only while running, Stop
  while live and Back when not. `set_replay` is Numbo's, for item 17.
- **Reason:** Numbo's pattern (ANSWERS.md 5) with the Seek-Whence GUI's Qt-free values,
  so that the round trip and the parsers are tested without Qt; the window already kept
  the next run's inputs (G-015), so the pane is their view and not a second owner, which is
  what makes every entry point agree; a strip that changes at every event is the honest
  way to keep the paint-once probe exact for a pane of widgets (a hidden counter would
  pass the probe and show nothing).
- **Alternatives:** the pane as the owner of the inputs with the window reading `inputs()`
  at the start only (Numbo's; rejected: the Problem menu and the command line would then
  write into widgets); a spin box for the seed (a text field takes what was restored and
  shows a bad value's error); the pane skipped by `add_view` as the Seek-Whence GUI's is
  (rejected: `test_main_window.py`'s probe lists every pane of PANES.md, and a pane that
  takes no events cannot say the state); a Random seed button (left out: PANES.md P9 does
  not list it, and the seed is typed or remembered); listing `surround-05` a second time
  within its family (one entry per problem keeps the selector's data unambiguous).

### G-045: The stats are a function of the events alone (French's report line from `RunEnded`), with the invariants' verdict read by the tap beside each snapshot and shown in red when violated; the outcome of a stopped run from the controller; rows painted on a scroll area
- **Sources:** PANES.md P12: problem, seed, profile and the changed fields
  (`RunStarted.changed`); codelets run; draws; T; Q; the Worldview's size and rating;
  structures built and removed; competitions; changes; fizzles; at the end `Result`'s
  fields and the report line, a mismatch on replay, and `invariants.check` (D-077) on the
  current state, empty, or shown in red as an engine finding, never hidden; the tests:
  the report line equals `report_line(result)`, the invariants are empty on the golden
  runs. `docs/engine-api.md`: `invariants.check(world)` reads the state and draws
  nothing, so it may be called between any two events; TASK.md's whitelist lists it.
  Numbo GUI, `stats_view.py` and `run_stats.py`: a Qt-free `RunStats` observer with
  `texts()`, `finish(outcome)`, a form of labels. Seek-Whence GUI, `stats_view.py` (its
  decision 16): the rows painted in one `paintEvent` so that each event costs one paint,
  the snapshot read through `set_position`. G-020, G-024, G-039: readings the snapshot
  does not carry are taken by the tap beside each snapshot and dated by its clock. The
  window re-feeds every view from the first event on a seek back (G-017), so a model
  reset by `RunStarted` forgets what the controller said at the end.
- **Choice:** `models/run_stats.py`'s `RunStats` is Numbo's shape with the fields of
  PANES.md P12, a function of the events alone: the run and the configuration from
  `RunStarted` (`run_text`: `surround-05, seed 3`, the profile added when not `standard`;
  `config_note`: the profile, then the pairs of `changed` other than `profile` as
  `field=value`), the count, the clock and the draws from every event, the codelet last
  chosen (`observe.codelet_text`), T with `held` and the reason and Q from
  `TemperatureUpdated`, the Worldview's names and rating from `WorldviewChanged`, the
  built and removed structures, the competitions, the changes and the fizzles counted,
  the answer from `AnswerGiven`, and from `RunEnded` the outcome (`answered`, `capped`),
  `Result`'s fields the other rows lack (the answer with its label, `literal`, how it was
  read and via which correspondence, the structure value, the Worldview) and French's
  report line as `observe.report_sentence` of the end event's fields, which is the line
  `run.report_line` makes of the `Result` (tested equal on the golden runs). So a seek
  shows the numbers of the past, and the report row is empty until the end is reached.
  The tap records `invariant_readings`, the sentences of `invariants.check`, beside each
  snapshot (`SnapshotHistory.record(..., invariants)`, `invariants_at`, `EventLog.
  invariants_at`), and `update(snapshot, invariants)` dates them by the snapshot's clock:
  the Invariants row says `not checked yet` before the first snapshot, `none violated (as
  of codelet N)`, or how many and which, and then one red row per sentence under it. What
  no event says comes from the controller's `RunOutcome` through `finish`: `stopped after
  N events` or `error: ...`; the view keeps the outcome across a seek (the model's
  `finish` is overwritten by the re-fed `RunStarted`) and forgets it when the controller
  reports a live state again. `stats_view.py`'s `StatsView` is a `QAbstractScrollArea`
  painting one row per field on its viewport (the rack list's way, G-038, greyscale text):
  the title dim, the text elided, the configuration in the edited colour when fields
  changed, a stopped or failed outcome likewise, the invariants in red when violated;
  `attach(history)` for the snapshot in force; `set_state`, `set_source` (`live run`;
  item 17 will say what a replay verified) and `finish` from the window. The row
  "Source" and the replay mismatch wait for item 17.
- **Reason:** the events carry everything the numbers need, and a pure function of them
  is exact at every index and needs no re-basing; the report line from the end event is
  the same line the engine prints, checked against `report_line(result)`; the invariants
  are the one thing a GUI must read from the world, and the tap already reads beside the
  snapshot, so a seek shows the verdict as it stood; a scroll area paints once per event
  and lets a short dock scroll (G-014) where Numbo's labels would widen the window.
- **Alternatives:** `invariants.check` after every event (ten times the checks for the
  same verdicts between steps: the state changes only inside a step); T to two decimals
  (the dumps print one); a row per `Result` field (nineteen rows already; the extra fields
  are one line); Numbo's form of labels (rejected, the Seek-Whence GUI's reason: a label
  change can repaint the parent and break the one-paint probe).

## The configuration pane (item 13)

### G-046: The configuration pane's model reads the fields' comments from `config.py` with `tools/knobs_table.py`'s patterns and the decisions' headings from DECISIONS.md; the edits are a dict over the chosen profile, `dataclasses.replace(PROFILES[profile], **edits)`, every value conformed to its field's type, the tables entry by entry; the `profile` field is the selector's; the edits saved as one JSON text under `config/edits`
- **Sources:** ANSWERS.md 3: one row per `dataclasses.fields(Config)` with the field, the
  selected profile's value, the edited value and the comment of `config.py` with its tags
  and D-NNN, read as `tools/knobs_table.py` reads them; decision titles from
  DECISIONS.md's headings on hover; the next run's configuration is
  `dataclasses.replace(PROFILES[profile], **edits)`, so the engine is not changed; each
  edit validated against the field's type; the last profile and edits remembered with
  QSettings; the defaults frozen (`config_defaults.lock`, D-075), never written. PANES.md
  P13 repeats this and asks for the tests (one row per field with a tag or D-NNN; no edits
  on profile p gives `PROFILES[p]`; an edit of one field of each type changes that field
  only; invalid values rejected; the lock untouched). `docs/engine-api.md` ("For
  loop0006"): the comments of `config.py`, which `tools/knobs_table.py` parses, are the
  tooltips. The engine: `Config` is a frozen dataclass whose every field is commented with
  its tag or decision (D-066); `tools/knobs_table.py`'s `fields_of` reads the source text
  with three patterns (the group heading `# ---- ... ----`, the comment lines, the field
  line) and joins a field's comment lines, so that the README's table keeps the comments
  word for word; `FAULT_KNOBS` (ANSWERS 2 of the engine's loop) and `CALIBRATION_KNOBS`
  (D-076) name the fields that are French's faults and the calibrated knobs. The
  Seek-Whence GUI's `models/config_table.py` (its decision 50) reads the comments with
  `tokenize` and `ast`, keeps the edits as a dict over `Config()` and conforms every
  value, nested tables included, with `parse_text` for what the user types and
  `value_text` for what the pane shows, and stores the edits as one JSON text under
  `config/edits`. Nothing says what an edit means once the profile changes, or which
  profile a saved edit belongs to.
- **Choice:** `models/config_table.py`. `read_fields` reads `config.py` with
  `knobs_table.py`'s three patterns, exactly as `fields_of` does (a test loads the tool
  by path and checks that the section, name and comment of every field agree), so the
  pane's comments are the README's; the values come from the dataclass itself; the tags
  are each bracket whole (`[F95 p. 66; F95 p. 93]`, as the engine writes them) and the
  decisions every `D-NNN`; the kind is `bool`, `int`, `float`, `str` or `tuple` from the
  `standard` default. `decision_titles` reads the `### D-NNN: title` headings.
  `ConfigTable` holds the chosen `profile` and the `edits`, a dict of field name to value;
  `config()` is `dataclasses.replace(PROFILES[profile], **edits)`, `set_profile` changes
  the base and leaves the edits as they are, and `edited_names` are the edits whose value
  differs from the profile's, so an edit survives a profile change and is marked only
  where it makes a difference; `set_value` removes an edit equal to the profile's value;
  the `profile` field is refused (`PROFILE_FIELD`: the selector sets it, and
  `dataclasses.replace` keeps it, so the run's `RunStarted.profile` stays the profile's
  name). `conform(value, default)` is the Seek-Whence GUI's validator with two
  additions: a `str` field takes a string, and an integer in a float slot of a table stays
  an integer (the engine's own tables mix `("salience", 0.2)` with `("kind", 15)`, and a
  conformed default must equal the default), while a top-level float field takes an
  integer as a float. `parse_text` reads an integer, a decimal, `true`/`false`
  (`yes`/`no`, `1`/`0`, `on`/`off`), a name as typed, or a Python literal for a table
  through `ast.literal_eval`. The settings hold the edits as one JSON text
  `[[name, value], ...]` under `config/edits`; `from_settings` checks every pair and
  drops a bad or unknown one; the profile itself is the controls' `controls/profile`
  (G-044), so the two are never two states.
- **Reason:** reading the comments the way the README's table is made keeps the pane and
  the README the same text, and the tool's three patterns are a few lines; the edits over
  the profile is ANSWERS.md 3's formula word for word; keeping an edit across a profile
  change is what a user who switches profiles to compare them expects (an edit lost at
  every switch would have to be retyped), and marking it only where it differs keeps the
  rows honest; conforming the tables keeps a typo from reaching the engine as a wrong
  type; `literal_eval` runs no code; one JSON text keeps the settings to one key per
  concern, as `controls/*` is one per control.
- **Alternatives:** `tokenize` and `ast` as the Seek-Whence GUI (a second reading beside
  the tool's; the comments could then differ from the README's); dropping the edits at a
  profile change (rejected above); the edits as a `Config` rather than a dict (an edit
  equal to the profile's value could not be told from none); one settings key per field
  (74 keys, most absent); a combo for `push_reset_set`'s two names (a line of text,
  validated by type as ANSWERS.md 3 says; the comment names both values).

### G-047: The configuration pane is a table with a widget per kind, the profile's value, the source, the marks (French's faults, the calibrated knobs, where the profile differs from `standard`) and an editor under it; the edits survive a profile change and are marked where they differ from the profile; the window reads the edits only when a run starts; the title and the stats pane mark a started run's edits; a status line is what the pane repaints per event
- **Sources:** ANSWERS.md 3: a profile selector; `Config.differences` marks the fields
  where the profile differs from `standard`; the `FAULT_KNOBS` marked as French's faults
  (fixed in `standard`, restored in `french-faults`); edited fields highlighted, with
  Reset per field and Reset all; an edit never changes the run in progress; the Stats
  pane and the window title show a run whose configuration differs from its profile, as
  `RunStarted.changed` reports it; the configuration goes into the session file and is
  restored on replay. PANES.md P13: widgets by type with validation, a filter; the
  tests: an edit during a run does not change its records, the next run's
  `RunStarted.changed` shows the edit, a session saved with edits replays exactly, the
  edits survive a restart through a temporary QSettings. PANES.md P9 and G-044: the
  controls' profile combo; the window keeps the next run's inputs and every entry point
  goes through `_show_next_run` and `_show_started`. G-045: the stats pane's
  Configuration row is `config_note(profile, changed)`, in the edited colour when fields
  changed from `standard`. G-011: the window's probe expects every pane on screen to
  paint exactly once per event, and a hidden one not at all; G-044's run line is how the
  controls pane meets it. The Seek-Whence GUI's `config_view.py` (its decision 51): a
  `QTableWidget` with a widget per kind, an editor under the table for the long values,
  Reset all beside the filter, the title marked `[edited: ...]` with the stats note's
  words, a replay shown read-only. Nothing says how the title marks a profile and edits
  together, nor what "differs from its profile" means for a profile's own calibration.
- **Choice:** `config_view.py`. A `QTableWidget` with the columns Field, Value, Profile,
  Source, Marks, Comment and a Reset button: a `QSpinBox` for an int, a `QDoubleSpinBox`
  (four decimals) for a float, a `QCheckBox` for a bool, a `QLineEdit` for a name or a
  table's literal; the Profile column holds the chosen profile's value, tinted blue where
  the profile differs from `standard`; the Marks column says `French's fault 3
  (french-faults: 15)` (tinted rose), `calibrated for human (D-076)`, `differs from
  standard` and, for the `profile` row, `set by the profile selector` (its widget
  disabled); an edited row is tinted amber with its name in bold and its Reset enabled;
  under the table an editor with the selected field's value one entry per line, Apply
  and Reset; Reset all beside the filter, which matches the name, section, source,
  marks or comment. The profile selector on the pane's top row emits `profile_changed`,
  which the window answers with `set_next_run(profile=...)`, and `_show_next_run` shows
  the profile back in the pane, the controls and the title: one state (G-044). The
  window's `next_config()` is `ConfigTable.config()` under the window's profile, read by
  `start_run` only, so an edit during a run changes nothing of it (tested: the records
  equal the plain run's). `start_run` takes the profile from `config.profile` (the name
  `replace` keeps) and the edits as `edited_fields(config)`, the differences from that
  profile without `profile` itself; `title_for(problem, profile, seed, edited)` gives
  `Tabletop — surround-05, seed 3 (human; edited: max_codelets)`. So "differs from its
  profile" is read as differing from `PROFILES[profile]`: a run under `human` with no
  edit is `(human)` in the title, while the stats pane's row says `human: type_weight=50`
  in the edited colour, as `RunStarted.changed` reports against `standard` (G-045's
  letter, kept). The pane's status line along the top, outside the table, says the next
  run's configuration as the stats pane will say it, the edits, and the run in
  progress (from `RunStarted` through `config_note`); it is what `redraw` repaints, so
  the paint-once rule holds when the dock is shown. During a replay `set_replay(config)`
  shows the file's configuration read-only (item 17's hook); the session file and the
  replay test are item 17's. The edits are saved under `config/edits` and restored with
  the other inputs; `MainWindow(settings=None)` writes nothing.
- **Reason:** the columns are ANSWERS.md 3's list in order, with the marks it asks for
  in one column rather than three; one state for the profile keeps the two selectors
  from fighting (the Seek-Whence GUI's reason for one owner per field); reading the
  edits at the start only is the simplest way to make "never the run in progress" true
  by construction; the title's `(profile; edited: fields)` says both facts in one
  bracket, and the stats pane's row keeps G-045's words so that a replayed session's
  row reads the same; a status line is G-044's answer to the one-paint probe.
- **Alternatives:** marking the title with every field changed from `standard`
  (`(human; edited: type_weight)` for a plain `human` run would call a profile an edit);
  hiding the pane during a replay (the restored configuration would be invisible);
  rebuilding the rows per profile (73 widgets rebuilt at every switch; the rows are built
  once and refreshed); a dialog for long values (awkward offscreen and for the tests).

## Sessions and replay (item 14)

### G-048: The session file is one header line (the problem by name or as its dict, the profile, the edits, the seed, the version), the engine's JSON lines as the body and an end line with the count and the digest, absent when saved mid-run; the digest is checked on reading; the recorder and the writer share one path
- **Sources:** PANES.md section 5 gives the format: a header `{"tabletop-gui-session": 1,
  "problem": "surround-05" or {problem dict for an edited table}, "profile": ..., "edits":
  {...}, "seed": ..., "version": ...}`, then the JSON-lines trace byte for byte as `--json`
  writes it, then `{"end": <events>, "digest": "<sha256 of the trace lines>"}`, absent when
  saved mid-run; errors are a `SessionError` with the path and the line; the body lines of a
  named problem with no edits equal `python3 -m tabletop P --seed N --profile R --json -`'s
  and the digest equals `fingerprint`. ANSWERS.md 3: the configuration goes into the
  session file and is restored on replay; ANSWERS.md 5: session save/replay as Numbo's.
  `docs/engine-api.md`: session files as the JSON-lines trace of `docs/trace-format.md`,
  replayed by running the `RunStarted` record's problem, seed and profile again.
  `docs/trace-format.md` and `observe.Hasher` (D-065): the fingerprint hashes each line
  with its newline. The Numbo GUI's `session.py` and the Seek-Whence GUI's
  `models/session_file.py` (its decision 46): a recorder that is an observer, a writer for
  a recorded history, a reader that checks the whole file and names the line, lists back to
  tuples, a run saved part way is still a complete file. The engine has no function that
  writes a `Problem` back to its file form; `problems.problem_from_dict` and `validate`
  read one (D-003). Nothing says how a mid-run save (no end line, by design) is told from
  a file cut at a line boundary, nor what to do when the digest is not the body's.
- **Choice:** `models/session_file.py`. `SessionHeader` holds the five parts; `header_of
  (problem, config, seed)` stores the library's problem of its name by name (a `Problem`
  equal to `load_problem(name)`) and any other as `problem_dict(problem)`, the file form of
  `problems/` with the provenance flattened per object, which `problem_from_dict` reads back
  (tested on all 53 problems); the profile is `config.profile` and the edits are the fields
  on which the configuration differs from `PROFILES[profile]`, so `config_of(header)` is
  `dataclasses.replace(PROFILES[profile], **edits)`, ANSWERS.md 3's formula; a configuration
  whose profile is none of `PROFILES` cannot be stored (`ValueError`), since the GUI never
  builds one. `from_json` checks every part on line 1: the version (a newer one refused),
  the keys, the problem (a name of the catalogue, or a dict that passes `validate`), the
  profile, the seed (an int), the engine version (a string), and every edit conformed to
  its field's type with `config_table.conform` (the `profile` field refused). The body is
  `observe.json_line` per event, so `json_lines(history)`'s text; `decode_event` builds the
  typed event back from the record with lists made tuples, and its line equals the stored
  one. `SessionRecorder` writes the header at once, each event's line as it comes (hashing
  it), and the end line `{"end": N, "digest": ...}` when `RunEnded` comes (or on `close()`
  after it); closed before the run ended, it writes no end line. `write_session` is the
  recorder fed a history. `read_session` returns a `SessionFile` (the header, the typed
  events, their lines and their `record_of` records, `ended`); it raises `SessionError` with
  the path and the line for a line that is not JSON (`(truncated?)` when it has no newline),
  a bad header, a bad event, an event after `RunEnded`, a bad end line, a wrong count, a
  digest that is not the body's (the events were edited, or the file was damaged), a line
  after the end line, an empty file, and a `RunEnded` with no end line after it (`truncated
  after line N: the run ended but the end line is missing`). A file with no end line whose
  last event is not `RunEnded` is a mid-run save, `ended` False, and opens. The window's
  `session_header()` keeps what `start_run` was given (the name when started by name, else
  the `Problem`) and the configuration, so a session saved from any entry point has the
  right header; `save_session` writes the history so far, also while a run is live.
- **Reason:** the format is PANES.md's to the letter; the digest, which the design puts in
  the end line, is cheap to check and tells an edited or damaged file from one the engine
  no longer reproduces, so the two cases give different messages; a mid-run file has to be
  told from a cut one somehow, and the `RunEnded` event is the one certain sign that an end
  line should follow; the problem dict reuses the engine's own reader and validator rather
  than a second schema, and makes item 15's edited tables storable with no change here.
- **Alternatives:** an end line on every file with a `mid-run` flag (not the design);
  storing the whole `Config` as pairs (the Seek-Whence GUI's way; the design asks for the
  profile and the edits, which is also what the configuration pane restores); ignoring a
  wrong digest and letting the replay find the difference (the replay would then report the
  engine as differing when the file was edited); storing the problem by name always (an
  edited table could not be replayed).

### G-049: Replay re-executes the header through the window's own controller with a checker subscribed after the tap that compares each event's JSON line with the stored one and stops the run at the first difference; the window then warns and loads the stored events into the history without snapshots; replay mode disables the inputs, shows the file's configuration read-only and is left by Close replay, or by choosing a problem
- **Sources:** PANES.md section 5: Open re-executes the header's run through the controller
  and compares event by event, as Numbo's `replay_oracle_trace` does (raising
  `TraceMismatch`); on a mismatch it warns and shows the stored events in the log, trace and
  timeline; Save works mid-run; Close replay. PANES.md section 4: Open session… (Ctrl+O),
  Save session… (Ctrl+S), Close replay. PANES.md P9: the selector, the title and the Stats
  Run line agree with the run actually started, a replayed session included; P12: a
  mismatch on replay; P13: a session saved with edits replays exactly. TASK.md's contract 6:
  replay re-runs the session file's problem, profile, edits and seed through the same
  controller and compares each event with the stored one; a mismatch is reported, never
  hidden. G-008 named `start_events` for the playback of a loaded session. The Numbo GUI's
  `_TraceChecker` and the Seek-Whence GUI's `main_window.py` (its decision 47): replay mode
  held by the window, run-only observers subscribed after the tap and dropped at the next
  start, the stored events loaded into the history and the window seeking to the first
  difference, the controls' inputs off with a tooltip, the configuration pane read-only,
  Close replay restoring the inputs chosen before. Nothing says what the Problem menu does
  during a replay.
- **Choice:** `main_window.py` with `models/session_file.py`'s `ReplayChecker`, `Divergence`
  and `TraceMismatch`. `open_session(path)` reads the file (an error said in the status bar
  as `cannot open NAME, line N: reason`), builds a `Replay` (the file, its `Problem` and
  `Config` from the header), enters replay mode (`replay_source`; the inputs chosen before
  kept in `_live_inputs`; `controls_view.set_replay(name)`; `config_view.set_replay(config)`;
  `stats_view.set_source("replay of NAME")`; a note when the file's engine version is not
  this one) and starts the re-run paused, stepping once so that `RunStarted` is drawn. Every
  run action goes through `_begin(mode)`: in replay mode `_start_replay`, which subscribes a
  fresh checker for that run only (after the tap, so the log holds what the checker checked)
  with `on_divergence` stopping the controller, and calls `start_run(problem, config, seed,
  observers=[checker])`; so Play, Step, Step codelet, Run to end and Ctrl+N drive the re-run,
  every event drawn as live with its snapshots, and Play after the end re-executes it. The
  title says `Tabletop — replaying NAME: surround-05, seed 3 (human; edited: max_codelets)`.
  `_on_finished` calls `_finish_replay`: verified (`replay of NAME: verified, N events`),
  stopped by the user (`stopped after N events, M verified`), saved part way (`the file's N
  events verified; it was saved part way, and the run went on for M more`), each also in the
  stats pane's Source row; or a divergence, when `_show_stored` loads the file's events into
  the history (`EventLog.load`, no snapshots), reloads the log pane, seeks to the differing
  event, says `NAME differs from the engine at event K (line L): ...` in the status bar and
  a `QMessageBox.warning`, and sets the stats' Source to `stored events of NAME (differs at
  event K)` and its State to `stored`. The checker compares `json_line(event)` with the
  stored line, byte for byte; a re-run past an ended file is a difference, past a mid-run
  file it goes on and is counted. `close_replay` stops the run, leaves replay mode, restores
  the controls, the configuration pane and the stats' Source, sets the title to the run in
  the history (no `replaying`) and the next run's inputs to the ones chosen before; the
  history stays, to scrub. `set_next_run` closes a replay first, so a problem chosen from
  the Problem menu during a replay makes the next run a live run. `start_events` is not
  used: the stored events are shown through the real views by loading the history and
  seeking, as in the Seek-Whence GUI.
- **Reason:** re-executing through the window's own controller is the one way to show a
  replay with snapshots and every pane live, and the checker on the worker stops the run at
  the very event that differs, so the history holds exactly the verified prefix; loading the
  stored events on a mismatch gives the trace, the event log and the timeline whole, as the
  design asks, with the snapshot panes honestly empty; closing a replay from the Problem
  menu keeps the controls, the title and the stats in agreement with the run (PANES.md P9's
  lesson) instead of letting a disabled selector drift; comparing lines rather than parsed
  records is exact and is what the digest hashes.
- **Alternatives:** playing the stored events through `start_events` on a mismatch (the
  views would be fed through the gate, slower and no more faithful than a seek); a replay
  that goes on past the first difference and lists every one (Numbo stops at the first);
  the Problem menu disabled during a replay (one more state to keep); `--session FILE` on
  the command line (not asked for; item 16 may add it).

## The palette and editor (item 15)

### G-050: The editor's model is a table in the file form of `problems/` with provenance in the GUI's own words, so that `problems.validate` passes it; the touch and the expected answers are set in the editor; positions are clamped inside the table; Save never writes into `problems/`; Load takes the library's files
- **Sources:** PANES.md P7: the edited table is validated live (`problems.validate`),
  `tables.realism_faults` says whether French would set it [F95 p. 38]; Do this! starts a
  run on it (`problems.problem_from_dict`), Run it again repeats it with the next seed;
  Save config writes a problem file (`docs/problems.md` section 10's keys; provenance
  `synthetic`) wherever the user chooses, never into `problems/`; editing is refused
  while a run is live. TASK.md's engine contract 5: an edited table becomes a `Problem`
  only through `problem_from_dict` after `validate` returns no error. `problems.validate`
  (D-003) requires of a file: a family of `FAMILIES`; a `source` with the tag F95, FCCA or
  FH91, a printed page and a figure; for every object a figure, a printed page of F95 and
  an image string naming `pdfimages`, and a pixel box inside the frame whose centre is the
  position; a non-empty `answers` block (a `ch2` file needs French's quoted words
  instead); a method of forty characters or more. `tables.random_table` (D-068) builds a
  synthetic `Problem` directly, with the tag `synthetic`, page 0 and the family `random`,
  which `validate` would refuse. The session file stores an edited table as its dict and
  reads it back only when `validate` passes it (G-048).
- **Choice:** `models/palette.py`'s `TableEditor` keeps `DraftObject`s (id, type, centre
  to a tenth, orientation), the touched id and the answers as labels `E1`, `E2`, ... in
  the order marked (`toggle_answer`; the labels close up when one is unmarked or its
  object deleted), the file's name, title and family and `based_on`. `to_dict` gives the
  file form of `docs/problems.md` section 10 with provenance that says, in words, that
  the table was set in the GUI and passes `validate`'s letter: the source is the
  palette's page, `{"tag": "F95", "page": 26, "figure": "set in the GUI's palette and
  editor (P7), not measured on a figure"}`; each object carries that figure, page 26 and
  the image string "placed in the GUI, not extracted by pdfimages"; the frame is 1100 x
  1000 pixels at ten per unit and each box is computed from the position with its centre
  at a whole pixel (`box_px_of`), so that `position_from_box` gives the position back
  exactly; the method, uncertainty (0 units) and review say the table was set in the
  GUI; `notes` holds `tables.describe`. The family is one of the four chapter 5 families
  (`FAMILIES`; `ch2` is left out, since its file needs French's words), chosen by the
  user, the first by default. `problem()` validates and then calls `problem_from_dict`,
  raising `ValueError` with the messages otherwise; `messages()` are `validate`'s own
  words, each followed by the GUI's hint where it has one (no objects, no touch, no
  answers); `faults()` is `realism_faults`, and a table with faults can still run.
  `add`, `move` and `rotate` clamp the centre so that the footprint (D-002) lies inside
  the table with a tenth to spare (`EDGE`), which keeps every box inside the frame.
  `load` takes any `Problem` (the library's through Load config, which opens in
  `problems/`, or a saved file) and records the library problem's name in `based_on`.
  `save` refuses a path under `problems/` and writes the dict as indented JSON. Every
  change raises `ValueError` while `enabled` is False (the window sets it from the
  controller's state).
- **Reason:** the contract makes `validate` the gate, and `validate` was written for
  digitized figures; a synthetic table must satisfy its letter, so the GUI fills the
  provenance with words that are true (the page is the palette's, the figure string and
  the image string say the table was set in the GUI) rather than invent a measurement.
  "Provenance `synthetic`" in PANES.md is read as the meaning, not the tag, since the tag
  `synthetic` cannot pass `validate` (recorded here as the departure). The expected
  answers are required by the file format and are useful: the report line names Eliza's
  answer by its label, so the user says which object or objects they expect. Clamping at
  the edit keeps "not inside the table" from ever being a message, and keeps the file's
  boxes inside the frame.
- **Alternatives:** asking the author for a `validate` that accepts synthetic provenance
  (an engine change; written under "For the author"); building the `Problem` directly
  as `random_table` does (breaks the contract); allowing `ch2` with an empty expected
  block (refused by `validate`); a fixed family `surround` (a label the user could not
  change); answers optional with a dummy `E1` on the touched object (dishonest: the
  report line would call every answer `E1`).

### G-051: The palette is Figure 2.1's column with the count boxes beside the cells and the screen's buttons in two rows; the editor is its own canvas in the pane, with drops, drags, keys and a context menu, and the P1 table shows runs only; editing is refused while a run is live; an edit line is what the pane repaints per event
- **Sources:** PANES.md P7: the palette with the eleven types as icons, the silverware in
  four orientations, each type's count on the table above its cell [F95 p. 96 (img)],
  Fig. 2.1's layout [F95 p. 26 (img)], the buttons of the dumps; drag an icon onto the
  table, move or delete objects, choose the touched object. Figure 2.1 is a column of
  eleven cells, fork to soup bowl (`objects.TYPES`' order), each silverware cell showing
  the piece in its four orientations [F95 p. 26 (img)]; Figure 4.1 puts that column at
  the left of the table with a small box beside each cell holding the count of that type
  on the table (4, 2, 4, 2, ..., 1 for the chapter 4 configuration), and above the column
  a block of six buttons in two rows of three: Load config, Clear table, Save config, Do
  this!, Run it again, Exit [F95 p. 96 (img)]. ANSWERS.md 4: the icons are new drawings,
  said so where the palette shows them. PANES.md section 3: the palette dock is in the
  left area. G-011: a pane repaints one thing per event. G-014: small minimum sizes.
- **Choice:** `PaletteStrip` paints the column as the figures draw it: the cells in
  `TYPES`' order with the shared icons (`icons.paint_icon`), the silverware four times in
  `ORIENTATIONS`' order, and the count box beside each cell (to its right, as Figure 4.1
  has it, not above as PANES.md's words say); the five buttons in two rows of three, Exit
  left out (the window's Quit). The editor is `EditorCanvas`, a `QGraphicsView` of its
  own inside the pane, drawing the table, the names, one movable `EditorItem` per object
  (the icon and the id) and the H and E arrows with the table pane's `ArrowItem`: a drag
  from the strip (`MIME_TYPE`) dropped on it adds the type at the drop point; a click on
  a cell adds one at the centre of Henry's row; an item is dragged to move it (the model
  clamps and the item snaps to the model's position); a double click touches; the keys
  H, E, R and Delete and the context menu touch, mark an expected answer, turn
  silverware and remove the selected object. The P1 table is not edited: it shows runs,
  and the run Do this! starts is drawn there like any other. The messages are shown under
  the canvas: `validate`'s in red, the realism faults in orange with the page [F95 p. 38],
  a green line when French would set it; Do this! and Run it again are enabled only when
  `validate` passes. `set_state` turns editing off while the run is live (the model, the
  strip, the canvas's interaction and drops, the buttons and the fields) and the edit
  line along the top says so; it is the one widget repainted per event. The pane's body
  scrolls (minimum 120 x 60). Do this! runs the table with the next run's seed and
  configuration, Run it again with the next seed, playing, through `start_run` (so the
  title, the controls and the stats say the edited table, G-044, and the session header
  stores its dict, G-048); a replay is closed first.
- **Reason:** the figures show the counts beside the cells, and TASK.md says show what
  French shows; a canvas of its own keeps the run's display and the editing apart (the P1
  table carries a run's structures, overlays and selection, and editing on it would have
  to clear them and refuse the run's own interactions), and a dock can be floated and
  enlarged for editing; drops, drags and keys are what a user of any editor expects, and
  the context menu lists them for discovery; the click-to-add is for keyboard and touch
  users, and for the tests, since the offscreen platform does not run a drag's nested
  loop. Enabling Do this! only on a valid table makes the gate visible.
- **Alternatives:** editing on the P1 table itself (rejected above); the counts above the
  cells as PANES.md's words say (the figure decides); a grid of the eleven types in two
  columns (narrower, but not Figure 2.1); a Problem menu entry "Edit this table" (Load
  config opens in `problems/`, which serves); remembering the edited table in the
  settings (not asked for; Save config serves).

## The results pane, the help and the command line (item 16)

### G-052: The results pane runs a batch on its own worker with `run_problem` on `run_seeds(seed, count)`, the CLI's `--batch` seeds, holding the run controller's engine lock so that a batch and a live run exclude each other; the picture is French's chapter 5 layout from `reported.py` and the bench's `compare`, with `other` as RESULTS.md groups it; Run > Batch… raises the pane and starts one; a batch line is what the pane repaints per event
- **Sources:** PANES.md P8: for the current problem, a batch of N runs (default 50,
  French's count [F95 p. 114]) on a worker with a progress bar, then French's bar graph
  and table of frequency, average structure value and average codelets per answer
  [F95 pp. 115-139 (img)] from `reported.py`, beside ours, with the page; the answers
  French prints no bar for grouped as "other", as RESULTS.md does; `run_batch` (or
  `run_problem` on `rng.run_seeds`) with no observer, on its own worker, never during a
  live run. TASK.md's contract 4: the results must equal `python3 -m tabletop NAME
  --batch N --seed S` for the same arguments, which runs `run_problem` on
  `run_seeds(seed, count)` (`__main__.run_batch_cli`), as `run_batch` does; the bench's
  `survey` takes a batch seed derived from the bench seed and the problem's name
  (D-072) instead. Fig. 5.x for Surround 5 [F95 p. 118 (img)]: the bar graph at the right
  of the table, "% of total runs" on the axis, the percentage printed above each black bar
  and the label under it, the configuration's title under the graph, and the table headed
  "Object Eliza touches", "Average structure", "No. of codelets run". `bench/harness.py`:
  `compare` puts a survey beside a `ResultSet` label by label, with `E1+E2` taking both
  labels and `other` for the rest, and `markdown_table` prints the three headings each
  with his column and ours, then the z (D-072). PANES.md section 4: Run > Batch… (P8).
  The Numbo GUI's `run_controller.py` holds a module lock while its worker runs the
  engine. G-011: a pane repaints one thing per event. ANSWERS.md 5: the engine runs on
  one worker thread.
- **Choice:** `BatchWorker` runs `run_problem(problem, config, s)` for `s` in
  `run_seeds(seed, count)` on its own thread, the window's next-run problem,
  configuration (`next_config()`, the edits included) and seed as the batch's; so the
  batch equals `run_batch` and the CLI's `--batch` run for run (and the survey's bench
  seed and batch seed are both the seed given). The worker takes the run controller's
  engine lock (`engine_lock()`, now public) for the batch's duration, without blocking:
  a batch while a run is live, and a live run while a batch runs, are refused with
  `EngineBusy` and the reason in the status bar (the window refuses a batch while live,
  during a replay and on an input error before the lock is tried). Cancel stops the
  batch after the run in progress; the runs so far are kept and the status says
  `cancelled after 12 of 50 runs`. The worker's callbacks reach the pane through the
  view's own queued signals with a generation, so a cancelled batch's late result is
  dropped. `BatchModel` gives the `Survey` (`harness.tallies_of`), French's sets for the
  problem (`reported.result_sets`, the book's first; a combo chooses another when there
  are several, as Buridan 1 has the original and the modified program), the rows of
  `harness.compare` (one per label he prints and `other` for the rest, as RESULTS.md's
  tables), the cells formatted as `markdown_table` formats them, the chi-square and the
  footer, and `bars_layout`, the graph's geometry as a pure function: a pair of bars per
  row, French's black and ours grey, the percentage above each (his as printed, ours to a
  tenth), the label under the pair, a 0-100 axis in percent of runs and the title with
  his page under the graph. The pane: the count box (fifty by default), Run batch, Cancel,
  the progress bar, the next batch's inputs, the status, the graph, a `QTableWidget` under
  French's headings with his column and ours, the footer and the note. Run > Batch…
  shows and raises the results dock and starts a batch of the pane's count. The
  `BatchLine` along the top (the batch's status and the live run's events) is the one
  widget repainted per event.
- **Reason:** the contract names the CLI's batch as the reference, and the CLI's seeds
  are `run_seeds(seed, count)`; one lock for both workers is the simplest guarantee that a
  batch never overlaps a live run in either order, and the controller already had it;
  keeping a cancelled batch's runs makes Cancel safe to press; the figure's layout is
  what French shows, and the bench's `compare` is already the reconstruction's own way of
  grouping `other`, so the pane and RESULTS.md agree; a menu action that starts the batch
  is what its name says, and the progress and Cancel make it harmless.
- **Alternatives:** the bench's `survey` with its derived batch seed (then the pane would
  not equal the CLI's `--batch`); running the batch inside the run controller (a batch
  has no events to deliver, so the gate would be idle); allowing a live run during a
  batch (two engines at once, harmless to determinism but confusing in the stats);
  Batch… as a dialog asking for the count (the pane's box serves); a 50-run batch test by
  default (it takes seconds and is marked `slow`); a selector for the problem of the batch
  apart from the run's (PANES.md says the current problem).

### G-053: The pane-to-pages table is data in `models/guide.py`, read by the window's dock table and by Help > What am I looking at?, which prefers the user guide's own table when the file is there and shows what to expect from the dynamics in a non-modal dialog
- **Sources:** PANES.md "Help": Help > "What am I looking at?" is generated from the
  pane-to-pages table of `docs/gui/panes.md` (`models/guide.py`, as the Seek-Whence
  GUI's), marks the panes that are GUI design and gives the expectations of section 0;
  Help > About names the reconstruction, the sources and the licence. iterations.md 16:
  the help from the pane table (`models/guide.py`); the test: the help lists every pane
  with its pages. Item 17 writes `docs/gui/panes.md` and says Help's page is generated
  from it. ANSWERS.md 4: every new design is labelled in the pane, its tooltip and the
  guide; the icons say they are new drawings. ANSWERS.md 11: the dynamics are shown as
  they are. The Seek-Whence GUI's `models/guide.py` reads the table from its guide file
  at run time (`GUIDE_PATH`), with no fallback.
- **Choice:** `PANE_ROWS`, one `GuideRow` per pane (id, name, what it shows, the pages or
  precedent, whose drawing it follows or GUI design, not French's), is the table; the
  window's `PANES`, `CENTRAL` and `TIMELINE_PAGES` take their pages texts from
  `pages_of(pane)`, so the dock's tooltip, the pane's `pages` and the help are one set of
  words. `guide_rows()` returns the user guide's table (`read_guide`, the Seek-Whence
  parser over `| Pane | Name | What it shows | Pages | Drawing |`) when
  `docs/gui/panes.md` is there and parses, else `PANE_ROWS`; `guide_html` is the page:
  the tags explained, one table row per pane with the GUI-design rows in bold, the icons'
  caption, and "What to expect" (the one correspondence, the rare push, the thermometer's
  jump, nothing tuned) with the RESULTS.md sections. `show_guide` opens it in a non-modal
  `QDialog` with a `QTextBrowser` and Close, returned for the tests. `gui_design` is true
  only when the drawing text starts with the label: a pane with an overlay or an editor
  of the GUI's (P1, P7) names it in the text and is not marked whole.
- **Reason:** a table in the package works before the guide exists, when the package is
  installed without `docs/`, and keeps the window and the help from drifting apart; the
  file is preferred so that item 17's guide is what the help shows, as PANES.md asks, and
  a test will keep the two equal; a message box cannot show a table of sixteen rows.
- **Alternatives:** the Seek-Whence way alone (the file, no fallback: Help would fail in
  an installed package); the pages texts kept in `main_window.py` and copied into the
  guide (two sources); a modal dialog (blocks the run).

### G-054: The command line takes `--set FIELD=VALUE` through the CLI's own `configure`, the inputs given winning over the remembered ones; `--smoke` runs to the end through the window and prints French's report line with the exit codes 0, 1 and 2
- **Sources:** iterations.md 16: `tabletop-gui` and `python3 -m tabletop.gui` take
  `--problem NAME`, `--seed N`, `--profile P`, `--set FIELD=VALUE` (the CLI's names),
  `--play` and `--smoke`; the inputs given win over the remembered ones; `--smoke` uses
  no settings, sets offscreen, silences the offscreen plugin, runs to the end through
  the window and prints the report line of `python3 -m tabletop NAME --seed N --quiet`,
  exiting 0 for an answer, 1 when capped, 2 on a usage error. ANSWERS.md 1: the same.
  The CLI's `configure(profile, settings)` (`tabletop/__main__.py`) parses `--set` values
  as literals and checks the field and its type (D-066). G-005: the skeleton's command
  line. The Numbo GUI's `app.py`: the smoke run quits the application on `run_finished`
  and prints the stats' lines.
- **Choice:** `--set` is parsed by the CLI's `configure` under the profile in force
  (`--profile`, else the remembered one) and given to the window whole through
  `MainWindow.set_next_config`, which makes the configuration's profile the next run's
  and its other differences the configuration pane's edits (G-047), so the title says
  `(edited: max_codelets)` and the run's `RunStarted.changed` reports them. An unknown
  problem, a bad `--set` and argparse's own errors are usage errors through
  `parser().error`, status 2. `smoke` connects `run_finished` to `app.quit`, calls the
  window's `run_to_end`, returns 2 with the status bar's reason when the run could not
  start, runs the event loop, prints `report_line(outcome.result)` and returns 1 when
  the result is capped or the run did not end, else 0. `tabletop.__main__` is imported
  for `configure`; its `main` runs only under `__name__ == "__main__"`.
- **Reason:** the CLI's parser is the one place that knows the `--set` syntax, so the
  GUI's `--set` cannot drift from it; the window's own `run_to_end` draws every event,
  as a smoke run of the GUI should; the report line is `report_line` in both places.
- **Alternatives:** parsing `--set` in the GUI with the configuration pane's `set_text`
  (two syntaxes); a smoke run through `headless.run` (no window drawn); `--session FILE`
  and `--table FILE` options (not asked for; the Session menu and Load config serve).

## The user guide (item 17)

### G-055: The user guide carries the model's pane table verbatim, one section per pane, and the expectations with their RESULTS.md sections; Help shows the file's table; the screenshots wait for item 18
- **Sources:** iterations.md 17: `docs/gui/panes.md` gives, for each pane, what it shows,
  how to read it, French's pages behind it and which drawings are GUI design; what to
  expect from the dynamics (ANSWERS.md 11) with the RESULTS.md sections; starting, the
  layout and how to change and share the default, sessions and the configuration; the
  quotations pass the checker and Help's page is generated from it. ANSWERS.md 12: a GUI
  user guide with the French pages behind each pane; the screenshots are drawn by
  `scripts/docs_screenshots.py` and go on the front page (item 18). ANSWERS.md 4: every
  new design is labelled in the pane, its tooltip and the guide; the icons are new
  drawings. PANES.md "Help": the page is generated from the guide's pane-to-pages table
  (`models/guide.py`, as the Seek-Whence GUI's). G-053 made the table data in
  `PANE_ROWS`, read back from the file by `read_guide` with the built-in table as the
  fallback. The Seek-Whence GUI's `docs/gui/panes.md` is the worked example: an
  introduction with the tags, Starting, The window (the layout table, the menus and keys,
  the selection, every event drawn), the pane table, one section per pane with "What it
  shows" and "How to read it", Sessions and replay, Changing the default layout,
  Screenshots. The engine's `tools/check_quotes.py` checks every double-quoted quotation
  of six words or more followed by a tag against the page; `tools/check_links.py` checks
  every relative link and anchor.
- **Choice:** the guide follows the Seek-Whence GUI's shape with Tabletop's panes. Its
  pane table is the model's `PANE_ROWS` cell for cell (written from the model, and
  `tests/gui/test_guide.py` asserts `read_guide() == PANE_ROWS`), so the dock's tooltip,
  the pane's pages text, the help and the guide are one set of words; the window's
  `guide_text()` is `guide_html(read_guide())`, and `test_main_window.py` asserts it
  equals the file's HTML and that every dock's title is in the guide. Each pane's section
  opens with **GUI design, not French's** and the reason (what French describes, what he
  draws, what he does not) when the whole pane is the GUI's (P2b, P3b, P4b, P5, P6), and
  names the GUI's parts inside a pane of French's (P1's overlays and Eliza's view, P2's
  additions, P3's seventh row, P7's editor). "What to expect" gives the four expectations
  of `guide.EXPECTATIONS` with the RESULTS.md sections they rest on (§9.2 and §6.4 for the
  one correspondence, §6.2 and §8 for the push rate, §6.4 for the hold and the jump), the
  chapter 4 numbers the panes may set beside the run's, and the push seed of G-035.
  Quotations are the few already verified by the gate in the views' docstrings
  (the thermometer's and the grid's sentences of [F95 p. 95]); everything else is
  paraphrased without quotation marks, so the checker has little to check and nothing to
  fail. Every `D-NNN` and `G-NNN` the guide cites is a heading of its file (a test).
  The guide has no pictures yet: the Screenshots section says they arrive with item 18's
  front-page section, rather than linking files that do not exist (the link checker would
  fail on them, and a picture that is not there is worse than none). `docs/gui/review.md`
  is named as item 20's.
- **Reason:** one table in two places drifts unless a test holds them equal; writing the
  Markdown from the model and asserting equality is cheaper than a generator the reader
  must run. The guide names the dynamics as RESULTS.md measured them, with the sections, so
  that a reader who sees a one-line Worldview pane finds the measurement rather than a
  bug. A guide that quotes little cannot misquote.
- **Alternatives:** generating the whole guide from the model (then the prose would live
  in Python, where it is harder to read and to cite); a guide whose table is the authority
  and a model that reads it at run time with no fallback (the Seek-Whence GUI's way; here
  the window must open from an installed wheel without the docs); screenshots now from
  `render_png` (item 18 owns the script and the size limits).

## The screenshots, the READMEs and the permitted documentation edits (item 18)

### G-056: The four screenshots are rendered offscreen from the quick start's run through the real window by `scripts/docs_screenshots.py`, run as a module; the front page and the README show them with captions, and `tests/gui/test_gui_readme.py` checks the sections, the pictures and the smoke line
- **Sources:** ANSWERS.md 12: real GUI screenshots, regenerated offscreen by a script
  (`scripts/docs_screenshots.py`, run as `python3 -m scripts.docs_screenshots`), each under
  200 KB; the front page gains a "Using the GUI" section with screenshots as the
  Seek-Whence front page has, placed after "Using the command line"; `python/README.md`
  gains "The GUI", the extras and the status; `tests/gui/test_gui_readme.py` checks them and
  runs the GUI section's commands; nothing that needs PySide6 in a block the engine's
  `test_readme.py` runs. ANSWERS.md 9: no `sys.path` tricks (the Seek-Whence script inserts
  into `sys.path`; this one must not). ANSWERS.md 10: the suite's time. Iterations.md names
  the four pictures: the whole window during `surround-05` seed 3, the table at the answer,
  the grid beside French's nearest dump, the plot with the jump beside French's
  thermometer. The Numbo GUI's `scripts/docs_screenshots.py` and the Seek-Whence GUI's are
  the precedents (`spin`, `wait_until`, `floated`, `save`, `--out`).
- **Choice:** `scripts/docs_screenshots.py` is a module under `scripts/` beside
  `docs_figures.py`, run from `python/` as `python3 -m scripts.docs_screenshots [--out DIR]`
  with no `sys.path` line; it opens `MainWindow(settings=None)` at 1500 x 950 with the
  shipped layout, runs the quick start's problem (`surround-05` seed 3 under `standard`,
  the run the front page already shows: Eliza touches cup2 (E2), 46 codelets, T 34.8)
  through `set_next_run` and `run_to_end`, every event drawn, and then seeks (no run is
  live) to the moment of each picture:
  - `gui-window.png`: the whole window at the choice of the last codelet, find-answer
    (the structures complete, the Worldview holding cp1, the thermometer down to 34.8,
    the answer not yet given), `window.grab()` scaled to 1000 px wide (170 KB). Earlier
    moments were tried: at the first `AnswerCodeletPosted` (codelet 5) the table is bare;
  - `gui-table.png`: the P1 scene at the answer through `TableCanvas.render_png` at 1.6
    px per unit (1152 x 1056, 101 KB);
  - `gui-slipnet.png`: the P3 grid dock floated at 1000 x 660 at the jump (the temperature
    update that follows the first `WorldviewChanged`, codelet 18) with the pane's
    "French's ch. 4" action on, through `SlipnetGridView.render_png`; the nearest dump is
    Figure 4.4 at 22 codelets (G-032);
  - `gui-temperature.png`: the P2 thermometer dock floated at 150 x 420 at the jump (T
    96.8 by the change, the tick and the reason under the column; G-027), set beside the
    P2b plot dock floated at 900 x 420 at the end of the run with the terms and French's
    options on (G-028, G-029), on one white ground (`side_by_side`); two moments in one
    picture, said in the caption. The jump is at the `TemperatureUpdated` after the change,
    not at the `WorldviewChanged` itself, which still shows 80.0 held: the update of a
    codelet is published after its change.

  Each picture's docstring and caption cite the page or the decision, and the two whole-pane
  designs in them (the plot; the grid's seventh row) are said to be the GUI's. The script
  prints a line per picture with its size and fails over 200 KB. Every option it turns on
  is turned off again and every dock docked back, so the pictures are independent of
  their order.

  The front page's "Using the GUI" section (after "Using the command line", ANSWERS.md 12)
  shows the four pictures, each `![alt](python/docs/gui-NAME.png)` followed by an italic
  caption, the form the engine's `test_readme.py` requires of every picture; its launch
  lines are a `text` block (they open a window), and its one `sh` block is the `--smoke`
  run with its output, which only `tests/gui/test_gui_readme.py` runs. The README's "The GUI"
  section (after "Using the command line") shows the window picture, lists the panes, the
  keys, the sessions, the settings and the options, and has two `sh` blocks, the `--smoke`
  run and the CLI's quiet run, whose shown outputs the test requires to end in the same
  line. The guide's Screenshots section lists the four with their pages and shows the
  window.

  `tests/gui/test_gui_readme.py` (after the Seek-Whence GUI's): the two sections' words and
  links; every `sh` block of both GUI sections runs from a fresh shell (PATH, HOME, LANG,
  offscreen) and prints what is shown; the four pictures are committed PNGs under `docs/`
  under 200 KB, exactly the pictures the two GUI sections and the guide reference; the
  guide lists them; the GUI's README and the tests' README name every module and test
  file; the engine's `test_readme.py` leaves `gui-*.png` out. The script's own run
  (`--out` a temporary folder, the four files, the limit, the committed pictures untouched)
  is marked `slow`: it is a window, a run and four renders in a subprocess (about 6 s on a
  quiet machine, more under load) and the default suite stands at the gate's 300 s backstop
  on the loaded development machine (PROGRESS.md, iterations 17 and 18).

  The file is `tests/gui/test_gui_readme.py`, not the `tests/gui/test_readme.py` that
  ANSWERS.md 12, TASK.md and the gate's comment name: pytest refuses two test files with
  one basename when neither folder is a package ("import file mismatch" against the
  engine's `tests/test_readme.py`), and the two ways to keep the name are closed to this
  loop: a `tests/gui/__init__.py` would make `tests/` the import root and break the
  twenty-five GUI test files that import `golden.py` by its bare name, and an import mode
  or `pythonpath` is a `pyproject.toml` change TASK.md does not allow. The author may
  prefer one of them (PROGRESS.md, "For the author").
- **Reason:** the front page already follows one run (the quick start's), so its screenshots
  come from the same run and a reader can reproduce every picture with one command; a
  script that opens the real window and seeks is the only way to show the panes as the
  user sees them; a module run from `python/` keeps ANSWERS.md 9's rule where the
  Seek-Whence script broke it.
- **Alternatives:** one screenshot per pane (sixteen pictures; rejected: the item names four
  and the guide names the rest in words); pictures from bare views (`render_png` of a fresh
  view fed the history; rejected for the window picture, which must show the layout);
  the script's run in the default test set (rejected for time, above; the committed
  pictures are checked instead); a picture of the network, the competitions or a push
  (the push seed of G-035 is a candidate for the review); the test named
  `tests/gui/test_readme.py` through a package marker or pytest's importlib mode (above).
- **Note (2026-10-14, the roles loop, item 15):** a fifth picture, `gui-roles.png`, from a
  second run through the same window, `blockage-07` seed 16 (the run
  `tests/gui/test_roles_view.py` uses: every family read, a barrier, its claim, a re-cast and
  the axis), since the quick start's run reads only first-sight tags before its answer: the
  P1 scene with the roles layer on at 0.8 px per unit (G-061) beside the roles dock floated
  at 700 x 528 and scrolled to the structural family (G-060), on one white ground
  (`side_by_side`), 190 KB; the layer is turned off and the dock docked back after. The
  five are listed in `PICTURES`, the front page, the README, the guide and the tests. The
  quick start's facts above are v1's: since item 08 of the roles loop the run is 44 codelets,
  T 41.0, the window picture's thermometer at 41.0, the jump still at codelet 18 and T 96.8;
  the four pictures were regenerated with the roles' runs, the pictures of the grid showing
  the roles' band (G-062) and the window the roles tab.

### G-057: Item 18's three permitted engine edits: the front-page pictures test leaves `gui-*.png` to the GUI's test, and the two documents that said no GUI exists now point at it
- **Sources:** ANSWERS.md 6 and `gate.py`'s `PERMITTED_EDITS`: item 18 may edit the
  front-page pictures check of `tests/test_readme.py` (12 added, 4 deleted lines in all
  with item 00's, every added code line about the GUI), `docs/engine-api.md` and
  `docs/standalone.md` (12/8 each), the sentences that say no GUI exists. G-003 records
  item 00's four edits.
- **Choice:**
  - `tests/test_readme.py`: one line in
    `test_the_front_page_pictures_exist_are_small_captioned_and_regenerated_by_the_script`,
    after the count of captioned pictures, drops the pictures named `gui-*` from the set
    compared with `docs_figures.py`'s registry and from the size loop, with a comment naming
    `tests/gui/test_gui_readme.py`. The caption rule still covers every picture of the front
    page, the screenshots included. With item 00's line, 2 lines added and 1 deleted since
    the baseline.
  - `docs/engine-api.md`: the sentence "No GUI code exists in this repository" in the
    introduction becomes one naming the package, its module map, its decisions and its
    guide (4 added, 2 deleted). The "For loop0006" notes, written before the GUI, stay as
    the history they are.
  - `docs/standalone.md`: the rule of the imports gains the GUI's places and the three Qt
    modules they may import, and step 3's comment says the GUI's Qt tests skip without
    PySide6, each file by `pytest.importorskip` (5 added, 3 deleted). The results table of
    2026-10-13 stays as recorded.
- **Reason:** the smallest edits that make the documents true and keep the engine's test
  strict over its own pictures; the GUI's pictures get a stricter test of their own.
- **Alternatives:** teaching the engine's test the screenshot script's registry (rejected:
  the engine's test would then need to know the GUI); rewriting the "For loop0006" notes
  (rejected: they are the plan the GUI was built against, and the gate's line limits are
  for sentences, not sections).

## Robustness, speed and standalone readiness (item 19)

### G-058: Robustness: the pixel probe over every painting widget, memory, closing, the capped run, the smallest window and the states of another version; the results pane scrolls, the line widgets take their lines lazily, the timeline strip's pens are cached; the time budget; the standalone check
- **Sources:** iterations.md 19 lists the checks; ANSWERS.md 5 asks that the drawn state
  after event k equal a fresh render of the models at event k; ANSWERS.md 10 gives the
  GUI's default tests about 45 s serially and the gate a 300 s backstop for the whole
  suite; ANSWERS.md 9 asks for `tools/standalone_check.sh` and `tools/check_links.py` in
  both places. The Numbo GUI's `tests/gui/test_polish.py` and the Seek-Whence GUI's
  `tests/gui/test_robustness.py` are the precedents; the Seek-Whence loop's lessons are
  in TASK.md.
- **Choice:**
  - **The pixel probe** (`tests/gui/test_robustness.py`, after the Seek-Whence GUI's):
    every painting widget by name (the table's viewport, the thermometer, the plot, the
    grid, the network, the strip, the rack list's, the Worldview's and the Proposals'
    and the competitions' viewports, the trace and events lists, the results graph and
    line, the stats viewport, the timeline strip, the controls' run line, the palette's
    edit line, the configuration's status line, and every whole pane) is compared,
    what the window's backing store shows against a fresh `grab()`, at every 50th event
    of the quick start's run in the default set, and (`slow`) at every one of its 496
    events under the built-in tabs and under the second tabs (the network, the
    competitions, the rack list, the results), then the temperature plot, the
    configuration pane and the Proposals tab at ten seeks each. The whole timeline bar
    is left out: the toolbar paints its own background under it, so a bare grab of the
    container differs in shade though its strip, which is compared, does not.
  - **What the probe found and the fix:** at the bottom area's height (about 130 px)
    the results pane's body (the top row, two labels, the graph of at least 90 px, the
    table of at least 40, the footer and the note: 256 px at least) did not fit, and a
    `QVBoxLayout` squeezed under its minimum lays its children over each other: the
    table and the footer were painted over the graph. The body now sits in a
    `QScrollArea` under the batch line, the controls' pattern (G-014, G-044), so a short
    dock scrolls; the line stays outside it, the one widget repainted per event.
  - **Memory:** the shortest library run on seeds 1-5 (`buridan-03` seed 2: 15 codelets,
    190 events) five times in one window; once warm, the objects alive vary by under 200
    and the blocks Python's allocator holds grow by under 500 a run. Measured: 68,524
    objects after each of the last four runs, and the blocks 321,500 to 321,563, 18 more
    a run on average. `tracemalloc` is not used: it
    slows the engine about five-fold (G-007), and the allocator's own count
    (`sys.getallocatedblocks`) costs nothing.
  - **Closing:** the window closed while a run plays (delay 0.02 s, past event 5) stops
    and joins the worker, with the thread count down and the engine lock free; closed
    during a batch of 500 (after its first result), `shutdown` cancels and joins the
    batch worker and the lock is free, the results so far kept.
  - **The capped run** (`slow`): G-007's measurement run (`surround-05` seed 1,
    `answer_post_rate` 0, 3000 codelets, 22,764 events) through the window at max
    speed, every event drawn: equal to the plain run, within 0.02 s an event (455 s;
    measured 345 s at 15 ms an event on a machine at load average 25, about half on a
    quiet one), and the GUI thread's CPU time for the last 3000 events at most 2.5 times
    the first 3000's (measured 36.9 s against 21.2 s, 1.7). Before the two fixes below
    the ratio was 3.7 (61.9 s against 16.6 s): a cost that grew with the run.
  - **The line widgets take their lines lazily:** `LinesWidget.refresh` (the Worldview,
    the Proposals and the competitions) rebuilt the whole text from the model at every
    event, hidden under a tab or not; over the capped run the competitions' 166 fights
    cost 19 s of the last window's 61.9 s and the three widgets 44 s together.
    `refresh` now marks the lines stale and `lines` (a property) takes them from the
    model when next painted or read, setting the scroll range then. A hidden widget
    costs nothing per event; a visible one is unchanged (painted once per event, the
    paint-once probe holds), and the tests that read `widget.lines` get the same tuple.
  - **The timeline strip's pens are cached**, one (pen, brush) per marker colour and the
    two pens of the strip of T made once, and the strip of T is drawn as one polyline
    per run of readings alike in `held` (each segment keeps the colour of its later
    reading) instead of a line per segment. The picture at a long run has hundreds of
    markers and thousands of readings, all repainted at every event because the markers
    spread over the whole history's count (G-043); the cost per event still grows with
    the picture, which is why the slowdown bound is 2.5 rather than Seek-Whence's 2.
  - **The smallest window:** `test_main_window.py`'s check of 1280 x 800 with every dock
    shown stays; the robustness file adds a run through the window at that size,
    equal to the plain run, with every dock still on the window or in a tab.
  - **States of another version:** `window/state` stamped `STATE_VERSION - 1` or `+ 1`
    is refused by Qt, said in the status bar, the built-in layout used, and the close
    saves the current version, which the next window restores; a state of this version
    from a window with other docks (one of ours missing, one unknown) leaves every
    pane on the window or in a tab with its View action enabled, the menu bar clear and
    a run going through; a state that is not bytes is ignored (G-013).
  - **Two timing flakes fixed:** the "paused" state after `new_run` (Ctrl+N) and after a
    `step` arrives on the worker after the first event's delivery, so
    `test_main_window.py` and `test_log_view.py` now wait for the state as well as the
    position (iteration 4 saw the first fail under load; the standalone check's step 3
    saw it again this item).
  - **The time budget:** the GUI's default tests took 155 s at load average 25 before
    this item (548 tests) and 167 s after it (557 tests; 200 s at load 28-35); a quiet
    machine runs them in about half, over the 45 s of ANSWERS.md 10. The cost is structural: about 35 whole runs
    through windows at 3-7 ms an event (the per-event paint of every pane, ANSWERS.md 5),
    spread over the pane modules, with no single test over 10 s. Nothing was deleted and
    nothing more was marked `slow`: the item's own long tests (the whole-run probe, the
    capped run) are `slow`, its memory test uses the shortest library run, and its
    settings tests share the short run. The whole default suite stays under the gate's
    300 s backstop on this machine (243 s in iteration 19's gate). Item 20 measures
    again on a quiet machine; the remaining ways (the Seek-Whence GUI's) are recorded
    under Alternatives.
  - **Standalone:** `bash tools/standalone_check.sh --offline-install` on the committed
    tree (it clones HEAD, so item 18's commit): clone, split and checkout pass; the
    tests with no source variables pass but for the Ctrl+N flake above, fixed in this
    item; the tests with the variables pass (1373 passed, 1 skipped: FCCA); the offline
    venv install, the installed command and the suite in the venv pass (1365 passed, 9
    skipped: the PDF tests); the link checker passes in both places.
- **Reason:** the probe over every widget found a real defect the per-pane probes did
  not (each floated its dock to a large size); the capped run's slowdown bound found
  two costs that grew with the run; the rest confirms what the earlier items built.
- **Alternatives:** caching the timeline strip's picture in a pixmap keyed by the
  history's count (rejected: live, the count changes at every event, so the markers
  move and the cache never hits; keyed by a coarser count the picture would lag by a
  fraction of a pixel, and a fresh strip and the window's would no longer render byte
  for byte alike); `tracemalloc` for the memory test (rejected: five-fold slower, 11 s
  against 5); for the time budget, sharing one window across `test_session_view.py`'s
  eight tests, taking the shortest run in the tests that need any run, caching the
  table items' pictures (`DeviceCoordinateCache`) and caching the grid's frame and
  labels in a pixmap (each a few seconds; left for item 20 or the author, since each
  changes an earlier item's test or picture).

## The final review (item 20)

### G-059: The final review: what it checked, what it found and what it left out
- **Sources:** ANSWERS.md 13 and the loop's iterations.md ask item 20 for every pane
  checked against PANES.md and ANSWERS.md with every departure and its decision in
  `docs/gui/review.md`, a sample of at least 20 page citations re-verified on the page
  images drawn by a recorded seed, the engine's diff since BASELINE, the whole-library
  determinism check, the whitelist of engine calls, the 60-line rule and the suites' times.
  The Seek-Whence GUI's review (`docs/gui/review.md` there) is the precedent for the
  document's shape. PANES.md P1 shares the click selection "with P5, P6 and P10"; the
  dumps print "Henry" above the table and "Eliza" below [F95 p. 98 (img)], and the engine's
  geometry runs y downwards from Henry's edge (D-001, D-005).
- **Choice:** the review is `docs/gui/review.md`, with one table per pane (the design, the
  GUI, the decision), the architecture and the rest in a second table, and the six checks
  with their results. The citation sample is 24 tags drawn with `random.Random(20261013)`
  from the tags of `tabletop/gui/` (16 of 254) and `docs/gui/` (8 of 46), each read on
  the rendered page or its text layer. What the review found and changed: the P1 row of
  `models/guide.py`'s `PANE_ROWS` and the guide's "What it shows" paragraph for P1 said
  "Henry's side below and Eliza's above", the opposite of the dumps, the engine's geometry,
  the table model (Henry's name at y = 0) and the GUI's own picture; both now say "Henry's
  side above and Eliza's below", and `tests/gui/test_guide.py` ties the row's words to the
  table model's name placement. What the review records as left out: the log pane (P10)
  takes the shared selection in `attach` and does not follow it; the event rows name
  structures only in their text, and a filter by selection would be a design of its own.
  Everything else checked agrees with the design or has its decision (G-003 to G-058).
  The time cuts G-058 left to item 20, taken: the whole runs that stand for any run now
  take the shortest library run on seeds 1 to 5, `buridan-03` seed 2 (15 codelets, 190
  events), instead of the quick start's 496 or the golden seed 1's 806: the session menu,
  save-then-open, save-part-way and tampered-file tests of `tests/gui/test_session_view.py`,
  the same-seed test of `tests/gui/test_controls_view.py` and the Ctrl+N run of
  `tests/gui/test_main_window.py`'s run-actions test. No check was deleted; the quick
  start's facts are still tested where they are the point (the tap, the table, the stats,
  the trace). The gate at the review had failed its 300 s backstop at load average 31 to
  37 (326 s; every other check passed) with the same tests that took 264 s at load 26 to
  34 in item 19.
- **Reason:** the review's purpose is to make every departure findable from one document;
  the wording error was found by reading the sampled citation's page against the row's own
  words, which is what the sample is for; the left-out interaction is recorded rather than
  built, since item 20 builds nothing new (TASK.md: one item per session, the review).
- **Alternatives:** a filter of the event list by the selected structure (the Seek-Whence
  GUI's event log has none either); a seed drawn at random rather than recorded (rejected:
  the sample must be reproducible).

### G-060: The roles pane (P14): the tags by family from the four role events alone, each with its evidence, readers and, once lost, when and why; the Worldview's solid-against-dashed bars (the dashed pen's cap made flat) and the event shown's tint; the dock tabbed with the Worldview
- **Sources:** the roles loop's iterations.md 14 asks for "P14 Roles: a dock listing the
  perceived roles, grouped by family. Each tag shows its evidence (the descriptions it was
  read from), its weight, and the consumers that read it (from `RoleConsumed`). The dock
  also shows the re-cast tiers and the axis", and, from the author's answer D1 of that
  loop, roles over time: "current and past roles per object, when and why each changed".
  The engine gives the four role events (D-105: `RoleTagged`, `RoleDiscarded` with its
  reason, `RoleConsumed`, `RoleRecast`), the families and codes of the registry (D-079),
  the re-cast (D-093), the axis (D-083) and `Snapshot.roles`. French's program has no
  roles: the prose names the loner, the sidekick and the figure [F95 pp. 118-140] and
  shows no figure of them, so the pane is a re-interpretation (D-078). The precedents are
  the Worldview and competitions panes (G-039 to G-041): a label over a `LinesWidget` of a
  Qt-free model with `lines()`, `tooltip` and `selection_of`, a solid bar for the Worldview
  and a dashed one for the shadow cabinet, and the rack list's tint of the event shown
  (G-038).
- **Choice:** `roles_view.py` and `models/roles.py`. The model is built from the four role
  events alone: a `TagState` per `RoleTagged` (name, item, role, arguments, weight,
  strength, evidence, the codelet type that read it, the codelet and the event index),
  closed by `RoleDiscarded` with its reason and codelet, its readings appended from
  `RoleConsumed` (the consumer, the effect, who ran it, the codelet; when an item carries
  several tags of one role, an image per partner, the readings go to the tags whose
  arguments the effect names), the last `RoleRecast` kept with its signature and tiers, and
  the groups' members from `GroupBuilt` for the selection. It is exact at every event
  index, so seeking rebuilds it (the window replays from the start when scrubbing back)
  and the standing tags equal `Snapshot.roles`, the re-cast `Snapshot.recast` and the axis
  `Snapshot.axis` at every snapshot of `blockage-07` seed 16, the run that reads every
  family (`tests/gui/test_roles_model.py`). The lines: a banner in plain words ("12 roles
  standing on 7 items, 3 past; 2 re-casts", ANSWERS.md 11 of the GUI loop), the axis, the
  re-cast block (the signature and the candidates by tier), then one heading per family
  with its standing and past counts and the family's tags item by item in the order first
  tagged, each item's standing tags before its past ones, two lines per tag (`tag_text`:
  `r7 cup2 POINT_IMAGE (cup1) w1 s42 by find-roles at codelet 9 - seen in ...`; then its
  readers, or "read by no consumer yet", and, once past, "lost to r11 by find-group at
  codelet 21"). A standing tag's first line has the Worldview role (a solid bar) and a
  past one the shadow role (a dashed bar); both lines of the tag the event shown built or
  discarded, and the re-cast's lines at its event, are `fresh` (the tint). The hover text
  of an item's lines is its whole history, current and past; a click selects the object,
  or the group with its members, in the shared selection (G-022), nothing for the table
  or the Worldview item. The pane keeps G-011's view protocol with `set_position` and
  `on_event`, `attach(history, selection)`, the lines panes' minimum, and says "GUI
  design, not French's, a re-interpretation (D-078)" in its label; the dock is tabbed with
  the Worldview, the competitions and the temperature plot in `default_layout.json` (its
  state bytes regenerated by View > Save layout as default for this code...), and the
  guide's table has its P14 row. **A fix in the shared widget:** the roles pane's test
  reads the bar's pixels down a line and found the dashed bar drawn solid, because
  `LinesWidget` gave the dashed pen Qt's default square cap, which lengthens each dash by
  half the pen's width at both ends and so fills gaps as long as the width; the dashed pen
  now has a flat cap, and the Worldview's and the competitions' dashed bars are dashed
  for the first time (their tests sampled across the bar, where the background beyond it
  passed for a gap).
- **Reason:** the model from the events rather than from `Snapshot.roles` keeps the past
  tags, which the snapshot does not hold, and makes seeking exact without a snapshot;
  the lines widget is reused so that the solid-against-dashed convention and the tint mean
  one thing across the panes; the two lines per tag keep the evidence, the readers and the
  going readable at the pane's width.
- **Alternatives:** a tree by item with the families as children (rejected: the design
  groups by family; the item's history is one hover away); a table widget with a column
  per field (rejected: the house convention is the lines widget); reading `Snapshot.roles`
  for the standing tags and the events for the past (rejected: two sources for one list).

### G-061: The table's roles layer, a fourth overlay: badges with the registry's codes, the midline band and the axis, the gaps, the images of the selected or touched object, the re-cast's tiers, the barriers' claims, a ring on a role-proposed correspondence (also in the Worldview pane); the overlay items repaint only their own changes
- **Sources:** the roles loop's iterations.md 14 asks for "P1 overlays: role badges on
  objects and groups, the midline and the gaps, the mirror and point images of the
  selected object, the re-cast candidates by tier, and barrier and claimed marks" and
  "P5: role-proposed correspondences marked"; the author's answer D1 of that loop asks
  that the overlays follow the current tags. The engine's roles: the images (G1, G2;
  D-082), the midline (S1; D-084), the gaps (S2; D-086), the barrier and the counterpart
  it claims (S5, S6; D-089), the re-cast's tiers (K1; D-093), the axis (G3; D-083), and
  the consumers' `create-cp a b` effects (D-095). The overlays' conventions are G-024 to
  G-026 (a layer per toggle on the bar and in the View menu, remembered in the settings;
  Qt-free `Overlays` data from `models/overlays.py`; the legend's note; Eliza's view flips
  every box); ANSWERS.md 5 of the GUI loop asks that the drawn state after event k equal a
  fresh render (the pixel probe, G-058).
- **Choice:** a fourth layer, `roles`, in `LAYERS`, the bar and the View menu, saved with
  the others; `OverlaysModel` composes a `RolesModel` (G-060) fed every event, and its
  `layout(boxes)` adds to `Overlays`: `badges` (a `RoleBadge` per item in view with a
  standing tag: its codes in the order tagged), `midline` (the middle third's box while a
  MIDLINE tag stands on the table), `gaps` (a `GapMark` per standing GAP: the white space
  between its two clusters' boxes along the row), `images` (an `ImageMark` per standing
  MIRROR_IMAGE or POINT_IMAGE of the focused object: the selected object, the touched one
  while nothing is selected; the table view sets the focus from the shared selection),
  `tiers` (a `TierMark` per standing COUNTERPART tag, with the re-cast's weight), `blocks`
  (a `BlockMark` per standing BARRIER with its claimed counterpart and the correspondence
  through which the claim is made) and the `axis` word; the legend's note counts the
  standing roles, and the hover text of an object or group gets "roles: N standing, M
  past" with each standing tag's code, arguments, weight, strength, evidence and readers.
  `RolesItem` (`table_view.py`, z between the lines and the arrows) paints them: a badge
  as an opaque rounded box with the codes at the top-right corner of the item's box, the
  midline as a faint translucent band with "midline (S1); axis: point (G3)", a gap as a
  dotted bracket with "gap (S2) above", an image as a dashed arrow labelled "mirror image
  of cup1", a tier as "tier4 (1)" under the candidate, a block as a bar along the
  barrier's top and a dashed line to the claimed object labelled "claimed by small-glass2
  (cp4)". A correspondence a role consumer proposed (`CorrespondenceState.role_proposed`,
  set from the consumer's `create-cp a b` effect when the pair's correspondence is built)
  carries a small ring at its middle, its hover text a "role-proposed: C1 ROW by
  propose-role-cp (D-095)" line, and the Worldview pane's entry says "role-proposed (C1
  ROW by propose-role-cp)" in its second line. **Repainting:** the roles item keeps a constant
  bounding rectangle (the table with room for the labels, so that a new tag does not
  dirty the whole view through a geometry change) and repaints whole when its overlays
  change, as the regions and salience items do; the badges are opaque and the midline a
  plain translucent fill rather than a pattern brush, because a translucent box over an
  antialiased border, or a pattern's phase, rounded differently in a partial repaint than
  in a fresh render by a pixel. A repaint of the changed marks' rectangles alone, and a
  comparison of only what the regions and salience items draw, were tried and dropped:
  neither was what moved the pixel probe. **The probe's tolerance** (`same_picture` in
  `tests/gui/test_table_view.py`) is three steps instead of two since this item: at the
  window geometry the fifth overlay button and the roles dock give (the table's viewport
  238 x 478 instead of 214 x 383) one pixel at an object's box corner in Eliza's view
  renders 3 apart between the backing store and a grab (`surround-05` seed 3, every
  sampled event, with every overlay on and with Eliza's view alone, with the roles item
  repainting whole and with `FullViewportUpdate` forced), a device rounding and not a
  stale repaint, which differs by tens. The pixel probes of the table with the layer on
  find no mismatch (`tests/gui/test_roles_view.py`, `test_overlays_view.py`).
- **Reason:** the marks are the tags the pane lists, so a reader can check a badge against
  the pane's line; the images follow the selection because every object carries several
  (an image per partner, D-082) and all at once would cover the table; the partial
  repaints keep the per-event cost of the layer at the changed marks, as G-058 asks.
- **Alternatives:** a badge per role as a glyph rather than the registry's codes
  (rejected: the codes are the registry's and the pane's); the images of every object at
  once (rejected above); a separate pane for the marks (rejected: the design puts them on
  the table).

### G-062: The roles in the grid (the roles' band under its own caption), the network (violet role nodes and links), the timeline (a violet dot per role change) and the log (the engine's `RL` lines and role events, nothing added)
- **Sources:** the roles loop's iterations.md 14 asks for "P3: one extra row of role boxes
  below French's 41, labelled 'not on French's screen (re-interpretation)' (ANSWERS.md
  C7)", "P3b: the role nodes and their slip links", "P10: the `RL` lines", and, from the
  author's answer D1, "role changes marked on the timeline". The engine adds 15 role nodes
  and 29 links to the Slipnet (D-080) and writes one `RL` line per role event in the text
  trace (D-105; `docs/trace-format.md`). The grid's seventh row (G-030, G-031: the nodes
  the screen lacks, dashed grey under a caption) and the network's groups (G-034) are
  the precedents; the timeline's markers are G-043's.
- **Choice:** the grid (`models/slipnet_grid.py`): the role nodes, which item 09 of the
  roles loop had placed in plain rows after the seventh, form the **roles' band**, rows of
  seven under their own caption "not on French's screen (re-interpretation): the 15 role
  nodes (D-080)" after a second gap, the boxes dashed in violet (`ROLE_BOX`) with
  `Box.role` set, `row_top` giving each row's top, the layout's `role_caption_y`, the box
  side computed for two caption lines (three with French's note), and the hover text
  saying "in the roles' band". The network (`models/slipnet_network.py`): `Dot.role` and
  `Line.role` flag the 15 nodes and the 29 links, drawn with a dashed violet ring and in
  violet, the caption saying "violet: the role nodes and links", the hover text "a role
  node". The timeline (`models/timeline.py`): a `role` marker per `RoleDiscarded` ("role
  change at codelet 21: small-glass1 is no longer POINT_IMAGE (r12 lost to r11, by
  find-group)") and per `RoleRecast` (the signature and the first three candidates), a
  violet dot on the top row, drawn before the correspondences so the diamonds stay on top.
  The log pane adds nothing: the trace tab shows the engine's `RL tag`, `RL used`, `RL
  drop` and `RL recast` lines under their codelet as the CLI prints them, and the events
  tab the four role events among the kinds.
- **Reason:** the roles are a re-interpretation and every place they appear says so in the
  same words and colour; a marker per `RoleTagged` as well would bury the other markers
  (76 tags in `blockage-07` seed 16 against 8 drops) and the pane's tint already shows
  the taggings; the log is the engine's text and must stay byte-identical to the CLI's.
- **Alternatives:** the role nodes mixed into the seventh row (what item 09 left:
  rejected by ANSWERS.md C7's label); a marker per tagging (rejected above); the `RL`
  lines filtered into a tab of their own (rejected: the pane lists them).

### G-063: The engine in the window: a permanent note at the right of the status bar and a line in Help > About; without a build the GUI refuses to start in a dialog (on stderr alone under `--smoke`), exit status 1; the title unchanged
- **Sources:** the engine's D-107: the compiled engine is the reference and runs by
  default; without a valid build every entry point fails at start with an error naming
  the build command, with no fallback; the pure engine runs on an explicit request only,
  and every output names its engine (the CLI's and the benches' first line, the trace's
  `RunStarted`). The fast-engine loop's item 06 asks that the GUI "refuses to start without
  it (the clear error, in a dialog where Qt is up, on stderr otherwise)" and, on the pure
  engine, "says so in its title or status bar". The Numbo GUI reports what cannot start in
  a message box once Qt is up and on stderr before; `--smoke` prints only its own lines
  (G-054). The window's title is the run's (G-015, G-047, G-049) and the tests pin it.
- **Choice:** `app.py` starts the engine only after the `QApplication` exists
  (`engine_error`: `import tabletop.accel`, the error caught and relayed, nothing chosen in
  its place) and before anything of the window is imported; on an error `refuse` writes
  `python -m tabletop.gui: <the seam's message>` on stderr and, unless `--smoke` was given,
  shows it in a critical `QMessageBox` ("Tabletop cannot start", the message as the
  informative text), then returns exit status 1, the status of a GUI that could not start
  (the same as without PySide6). The window carries a permanent `QLabel` at the right of
  its status bar, `engine_note()`: `engine: compiled`, or `engine: python (explicit
  request)`; its tooltip and a line of Help > About give `engine_details()`: the build
  stamp, the compiler, the Cython version and the pinned flags of the compiled engine
  (D-110), or that the pure engine is the specification run by explicit request. The
  title is unchanged. `headless.run(problem, config, seed, window=False)` keeps its
  signature; without a build it fails at import, as every entry point does.
- **Reason:** a modal dialog under `--smoke` would wait for nobody (the smoke run is a
  subprocess of the tests and the packaging check), so there the message goes to stderr
  alone, where the tests read it; the status bar rather than the title because the title
  is the run's and pinned by the tests of three panes, and a permanent widget sits apart
  from the status messages (the outcome, the selection), which keep the left; the engine
  shown on both engines, not only the pure one, because every other output names its
  engine whichever runs (D-107), and the note's tooltip gives the build the record holds.
- **Alternatives:** the engine in the title (rejected: the title is the run's and the
  pinned facts would change with the engine); an exit status of its own for the refusal
  (rejected: the CLI exits 1 on the same error, and the message says which; `--smoke`'s 1
  for a capped run is told apart by the report line, printed only when a run ended); a
  dialog under `--smoke` with a timer (rejected: a smoke run prints and exits); the engine
  check before Qt starts (rejected: the item asks for a dialog where Qt is up).

### G-064: The table shows the next run's problem before Play, bare, whenever the problem chosen differs from the one drawn and no run is live
- **Sources:** the author, 2026-10-17: "when we open a particular problem, it doesn't show
  up immediately, only after it's running". G-017: `TableModel.set_problem` draws the table
  from the `Problem` alone, before any event, so that no empty table shows during the
  start; until now the window called it from `start_run` only, so a problem chosen in the
  selector, the Problem menu or the command line (G-015, G-044) reached the title, the
  controls, the configuration pane and the results pane (`_show_next_run`) but not the
  table, which stayed empty, or kept the last run, until Play. PANES.md P9: whichever way a
  run is started, the panes agree on it. The Numbo GUI draws nothing before its run.
- **Choice:** `_show_next_run` ends with `_preview_problem`: when no run is live
  (`_live()`, running or paused) and the table's model shows another problem than the next
  run's (or none, at the window's first `set_next_run`), the problem is loaded and given to
  `table_view.set_problem`, which draws the objects and Henry's touch with no mark, no
  structure and no E, exactly the picture the run's first event would clear to (G-017).
  A problem the library lacks leaves the table as it is; Play says so (G-044). The
  history, the position and every other pane are untouched: a finished run's panes keep
  the run until the next one starts, and only the table changes, as the title does.
- **Reason:** the table is the pane the problem is chosen for, and a choice that shows
  nowhere but the title reads as a choice not taken; the model already draws a bare
  problem exactly, so the preview adds no second picture of a table. Keyed on the problem
  drawn rather than on every input change so that a new profile or seed after a finished
  run keeps that run's table (its structures are the run's, and the other panes still
  show it); never while a run is live, so a choice made during a run waits for Play, as the
  title does. The command line's `--problem` goes through `set_next_run` and previews
  without a case of its own.
- **Alternatives:** preview on every input change, clearing a finished run's table
  (rejected: the other panes would still show the run); clear every pane when the
  problem changes (rejected: PANES.md keeps a run on show until the next starts, and the
  trace and the results are what a reader compares across problems); no preview, the
  Numbo GUI's way (rejected by the author's report).
