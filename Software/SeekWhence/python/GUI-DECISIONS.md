# GUI decisions: where the design leaves a choice open

The choices the GUI of the Seek-Whence reconstruction (`seekwhence/gui/`, loop0004) makes
where its design (`ralph_loops/loop0004/PANES.md`, `ENGINE-API.md`), the author's answers
(`ANSWERS.md`, which override everything), the Numbo GUI (the precedent for every
convention, ANSWERS.md 7) and the sources (Meredith's thesis, FCCA chapter 1) do not dictate
one, and every place the GUI follows the Numbo GUI rather than PANES.md. Code that rests on
a decision says `G-NNN` in its docstring; `tests/gui/test_gui_conventions.py` checks that
every `G-NNN` in the code has an entry here. Engine decisions are `D-NNN` in
`DECISIONS.md`; the GUI never adds to them.

Each entry has four parts, in this order: **Sources** (what the thesis, FCCA, Numbo or
ANSWERS.md say, with tags, or that they say nothing), **Choice** (what the GUI does),
**Reason** (why) and **Alternatives** (what else would fit). Numbers are given in order of
creation and never reused. A decision made while a question for the author is open is
marked "pending the author".

## Index

| Decision | What it settles | Item |
|---|---|---|
| G-001 | The Qt-free models live in `seekwhence/gui/models/`, not beside the engine | 00 |
| G-002 | The offscreen conftest is `tests/gui/conftest.py`, not `tests/conftest.py` | 00 |
| G-003 | Snapshots at every task boundary while a run is short, keyframes after; a per-run cache | 01 |
| G-004 | The tap learns the `World` after the `Session` is built; the `start` event has no snapshot | 01 |
| G-005 | The run controller and the event log are Numbo's, with sessions, tasks and terms for runs and iterations | 01 |
| G-006 | The hypothesis's source is tracked from the events | 01 |
| G-007 | The rack is read after the draw: the chosen task is off it | 01 |
| G-008 | The guess is answered through the `answerer` hook by a blocking `InteractiveAnswerer`; the Bridge has a fourth signal | 02 |
| G-009 | A run starts from `RunInputs`; the window has a default run and the presets of item 02 are the appendix runs | 02 |
| G-010 | The layout precedence: the user's settings, the repository's `default_layout.json`, the built-in arrangement (a function) | 02 |
| G-011 | Placeholders for the panes not built yet; the view protocol; visibility by visible region | 02 |
| G-012 | The history is filled by the tap on the worker, not by the window on the GUI thread | 02 |
| G-013 | Menu actions whose items have not come are present but disabled; Step task is Ctrl+Right | 02 |
| G-014 | The sequence pane's model is event-driven: fates under the hypothesis in force, the whole block as ghost cells aligned by position, badges | 03 |
| G-015 | Interactive terms come through a `TermFeed` stream the `Console` reads; the box is enabled at a prompt and queues several terms | 03 |
| G-016 | The stats view paints its own rows and reads the snapshot through `set_position`; its fields | 03 |
| G-017 | The show buttons read the snapshot at the cursor and print into a text area until the console pane | 03 |
| G-018 | The console model keeps two `Trace`s and plays the console protocol on the run's input lines at each ask; the lines are append-only and a seek moves the cursor | 04 |
| G-019 | A command typed or clicked with an interactive run live is a session input; otherwise it prints at the cursor; find, Save transcript and Copy | 04 |
| G-020 | The console's rows are Numbo's `LogList`, with a single click to seek | 04 |
| G-021 | The cytoplasm model is the snapshot plus the `glint`, `spark` and `bond` events since it; flashes and fading ghosts from the events | 05 |
| G-022 | The cytoplasm's layout: glints on a baseline, arcs below it with depth by adjacency and gap, sparks dashed and deeper, a glom anchored at its span; the tooltips | 05 |
| G-023 | The cytoplasm's styles: colour by type, width by strength, the flash, the highlight, inactive glints lighter; the canvas is Numbo's `TreeView` | 05 |
| G-024 | The gloms are nested hulls above the baseline, labelled by type or dubbing; pseudo-gloms dotted on the top-down layer; proposals as markers keyed by task number; burst, dissolve and the blast as fading ghosts | 06 |
| G-025 | One `Selection` for every pane, held by the window; a glom selects everything under it; a click toggles | 06 |
| G-026 | The four layers are checkable actions on the canvas and in View > Cytoplasm layers; sparks and the top-down layer off by default; not remembered | 06 |
| G-027 | The taskrack model is a shadow of the rack kept from the events, corrected by every snapshot; rows by type in order of first appearance with their share of the weight; the chosen task's chance from the rack before the draw; the Hfilter run at once has none | 07 |
| G-028 | The taskrack pane is two virtualised lists after Numbo's `LogList`: the band, the banner and the rows above, the whole history below; a click on a task selects what its arguments name; the band stays tinted while the task runs; the banner lasts `BANNER` events | 07 |
| G-029 | The tallies count the tasks chosen since the last `term` event; the arenas' colours | 07 |
| G-030 | The socratoplasm model is the snapshot's gnoths plus the gnoth events since it; silent changes wait for the next task boundary and the card says so; ghosts fade | 08 |
| G-031 | The gnoths are a row of rectangles over the cytoplasm's columns; the cards are one `LineList` with a bond line per bond; a click selects the gnoth with its glom and members; a hovered bond lights its arc | 08 |
| G-032 | The template is kept from its events: the four fields, the check line, the fate and the terms filtered | 08 |
| G-033 | The hypothesis model is the events plus the snapshot's box tree: the form, source, validity and candidates from the events, the box with the run's own names from the snapshot taken at the event, a fresh tree with provisional names in between | 09 |
| G-034 | The form is drawn as Figures 1-2's ovals: a leaf per column, a parent over its children, a shared place joined to its structure; nodes map to boxes by creation order; an oval's terms by provenance over a fresh box | 09 |
| G-035 | The predictions table looks `LOOKAHEAD` terms ahead on a fresh box; the validity bar's scale; the box tree is a toggle in the model, off by default; the private box; one `LineList` for candidates, records and hits | 09 |
| G-036 | The platoplasm network is the GUI's own drawing, after the Numbo GUI's Pnet view: one column per category from the strictest on the left, a box per type with its format and counts, the s-links as arrows offset in pairs with width and label by slipperiness, the ideal atoms as a strip below; a pure `layout()` | 10 |
| G-037 | The platoplasm model is the snapshot's tables and lists plus what the events since it announce; the reigning class, the threshold and the episode's check, change and scout from their events; the crown travels the s-link over `CROWN_MOVE` events | 10 |
| G-038 | The pane's list is one `LineList` of the reigning class, the last change, the check as the transcript prints it and the reigning class's bonds; a click on a type selects its manifestations and actualizations; the "Links" toggle draws Figure 2's links as tags, off by default; tooltips carry the tables | 10 |
| G-039 | The storyboard is built from the events alone: one episode per refutation, each event on the one card its kind names, cards in event order; the operation pictures are read off a socratoplasm model of the pane's own, with the names under the groups as pp. 110 and 120 write them | 11 |
| G-040 | The freeze-dried list, the reigning class and the threshold come from the snapshot, with the events between; the threshold plot keeps one point per change of value, read once the Gnoth-setter has cleared the old threshold; the Bond-assessor's given threshold is not plotted | 11 |
| G-041 | One `LineList` for the storyboard: episodes fold (the newest open), a click on a card seeks, a hovered card selects what it names and leaving restores the selection, "In series" is a toggle off by default; the pane raises itself at the run's first refutation, live only | 11 |
| G-042 | The timeline's model scans the history once per event for its markers (terms, guesses with their verdict, refutations, slips, blasts) and the parse at each term boundary from the prompt's snapshot; the position text | 12 |
| G-043 | The timeline is Numbo's bar with two strips under the slider, the markers and the parses, placed by a pure function; a click on a marker, a cell or the strip seeks; the go-to box reads `term`, `clock` or `event`; the controls are disabled while a run is live | 12 |
| G-044 | The log's kinds are grouped by the sections of `events.py`; the history's `visible` list is guarded by a lock, because the tap appends on the worker and the filter rebuilds on the GUI thread | 12 |
| G-045 | The event log is Numbo's `LogView` as a view of the protocol, with a clock column, group actions in the Kinds menu, a double-click or Enter to seek, and Save events (JSON lines) in the Session menu, built in item 12 | 12 |
| G-046 | The session file is Numbo's JSON lines with a run line; the events are `event_record`s compared as JSON; a `Form` in a task's arguments comes back as its text; a file is complete when its last event ends the run; Save writes the whole history with the lines typed and the answers given | 13 |
| G-047 | Open re-executes the run line through the window's own controller with a checker on the worker that stops the run at the first difference; a part-way file's re-run goes on; the stored events are then loaded without snapshots and the structure panes show what the events alone give; Play after the end re-executes; Close replay keeps the history | 13 |
| G-048 | The presets are the three appendix runs, Custom, and every sequence of `seekwhence.sequences` fed as the bench feeds it (16 terms); three answering modes, the continuation judge through the `answerer` hook; a judged run's answers are stored in the session file as an interactive run's are | 14 |
| G-049 | The controls pane is Numbo's `ControlsPanel`: its values are plain text in a Qt-free `ControlValues`, turned into `RunInputs` at every change (an invalid text refuses the start), shown back from any `RunInputs`, and saved under `controls/*`; the random seed comes from Python's `random`; the panel takes no events | 14 |
| G-050 | The configuration pane's model reads the fields' comments with `tokenize` and the decisions' headings from DECISIONS.md; the edits are a dict over `Config()`, every value conformed to its field's type, saved as one JSON text under `config/edits` | 15 |
| G-051 | The configuration pane is a table with a widget per kind and an editor under it; the controls' caps and trace are shown but edited in the controls pane; the edits are replaced into the next run's inputs at every change and marked in the title; a replay shows the file's configuration read-only | 15 |
| G-052 | The built-in dock arrangement is made at the first `showEvent`, never before the window is laid out: Qt 6 leaves stale tab bars (one over the menu bar, a second "Console / Event log" bar) when two tab groups are made on an unshown window | 17 (ANSWERS.md 10.1-2) |
| G-053 | The pseudo-gloms are shown by default, dotted, with a light grey outline and fill lighter than a real glom's; the legend says "pseudo-glom (grey)"; the toggle stays | 17 (ANSWERS.md 10.4) |
| G-054 | A run started any way shows its inputs in the controls (its preset, or `custom`), the title and the stats Run row; outside a replay they become the window's inputs exactly as used; Close replay restores the inputs chosen before | 17 (ANSWERS.md 10.3) |
| G-055 | The command line is Numbo's with the CLI's names (`--preset` or `--terms`, `--seed`, `--answers`, `--max-steps`, `--play`, `--smoke`); the arguments go through the controls' `ControlValues`; `--smoke` prints `Session.summary()` and exits 0 for `ok`; Help > What am I looking at? is the table of `docs/gui/panes.md` in a dialog, Help > About a text | 16 |
| G-056 | The four screenshots are rendered offscreen from the appendix presets on their golden seeds: the window at run 1's guess with the guess panel asking, the cytoplasm scene of run 2's end at 1.5x, run 3's storyboard at its first candidate and the platoplasm at its slip with the crown half-way, the two docks floated at fixed sizes; `--out DIR` for the tests | 17 |
| G-057 | The documentation: the guide has one section per pane naming its pages, its drawing's figure or precedent and its interactions, P5 and P6 opening with the GUI-design label; the README's GUI section follows Numbo's, its `--smoke` command in a `sh` block, which the engine's `test_docs.py` runs too; `tests/gui/test_readme.py` checks the screenshots, the sections, the module maps and the guide's quotations | 17 |
| G-058 | The window fits 1280 x 800 with every dock shown (`SMALLEST_SIZE`): the lists and the painted widgets get explicit minimums of a few rows, the long labels clip (a minimum width of 1), the two combo boxes size to a contents length, the controls' form sits in a scroll area, and the Configuration dock is tabbed beside the Hypothesis dock rather than stacked under it | 19 |
| G-059 | The layout bytes carry `STATE_VERSION` (`saveState(version)`): a `window/state` saved by another version of the window's docks, or one that is not bytes, is ignored with a status-bar note and the next layout of the precedence is used; the default layout file's state carries the version too | 19 |
| G-060 | The platoplasm network paints a pixmap of its layout rendered once per change of layout, size or pixel ratio, since the platoplasm is the same over thousands of events; `blackburn-11` through the window went from 232 s to 129 s | 19 |
| G-061 | The robustness tests (item 18): the pixel probe over every painting widget and every whole view after each event of run 1 and every fifth of run 3 with the tabs switched; memory flat over six runs (bounds Numbo's); closing while playing, waiting for a term or with a guess pending; `blackburn-11` to the end at max speed under 12 ms an event with no slowdown (the last 5000 events at most twice the first 5000's CPU time); the smallest window; the state version | 19 |
| G-062 | Four interactions of PANES.md are left out at the final review and listed in `docs/gui/review.md`: the term cell's click neither selects the gnoth nor seeks to the term's entry; Ctrl+F and the option to hide the console's lines after the cursor; Copy a row and click-a-decision in the configuration pane; the structure panes are not themselves marked when a replay differs | 19 |
| G-063 | Functions over 60 lines are the widget-declaring constructors and the menu builder, Numbo's shape; `test_gui_conventions.py` lists them and refuses any other | 19 |

## Foundations

### G-001: The Qt-free models live in seekwhence/gui/models/, not beside the engine
- **Sources:** the Numbo GUI keeps its Qt-free models in a package beside the engine,
  `numbo/models/`, and its views in `numbo/gui/` (Numbo GUI, `numbo/models/__init__.py`).
  ANSWERS.md 1 puts the GUI inside the engine package as `seekwhence/gui/`, and ANSWERS.md 2
  puts the read-only snapshot code in the GUI, not in an engine file. The thesis says nothing:
  Meredith's program had no separate display [M86 pp. 164-179].
- **Choice:** the models are the subpackage `seekwhence/gui/models/` (the tap, the snapshot,
  the event log, the run controller, the session file, the presets, the configuration table
  and one model per pane). They import the engine and nothing from Qt; the views in
  `seekwhence/gui/` import them. `pyproject.toml` lists both packages.
- **Reason:** with the GUI inside the engine package, a sibling `seekwhence/models/` would be
  an engine folder, and the engine is frozen (TASK.md): everything the GUI adds must stay
  under `gui/`, where the engine's scanning tests skip it (ENGINE-API.md section 5) and the
  gate's frozen check allows it. Numbo's separation of models from views is kept one level
  down; the models are still Qt-free and tested with plain pytest.
- **Alternatives:** `seekwhence/models/` beside the engine, as Numbo's (rejected by
  ANSWERS.md 1 and the frozen engine); a separate top-level package `seekwhence_gui`
  (rejected by ANSWERS.md 1).

### G-002: The offscreen conftest is tests/gui/conftest.py, not tests/conftest.py
- **Sources:** the Numbo GUI sets `QT_QPA_PLATFORM=offscreen` by `setdefault` in
  `tests/conftest.py`, shared by its whole suite (Numbo GUI, `tests/conftest.py`), and keeps
  its `thread_errors` and `window` fixtures in the test files. TASK.md says the engine's test
  folder gets no new file; ENGINE-API.md section 5 says the setting goes in `tests/gui/`.
- **Choice:** `tests/gui/conftest.py` sets `QT_QPA_PLATFORM=offscreen` by `setdefault` and
  defines Numbo's `thread_errors` fixture (and, from item 02, the `window` fixture). The
  engine's `tests/` has no conftest and gets none.
- **Reason:** the engine's tests are frozen (the gate allows only the two scanning tests to
  change, by a few lines each), and pytest reads a `conftest.py` for every test under its
  folder, so `tests/gui/conftest.py` covers every GUI test and nothing else. Keeping the
  fixtures in one conftest rather than in each file, as Numbo does, saves repeating them
  across the twenty items' test files; each Qt test file still starts with the two
  `importorskip` lines.
- **Alternatives:** `tests/conftest.py`, as Numbo's (rejected: a new engine-test file);
  the fixtures repeated in every test file, as Numbo's (more copies to keep alike).

## The history: the tap, the snapshot, the event log and the run controller (item 01)

### G-003: Snapshots at every task boundary while a run is short, keyframes after; a per-run cache
- **Sources:** ENGINE-API.md section 2 asks for a read-only snapshot of the `World` at the
  `chosen`, `prompt`, `guess`, `answered` and `ended` events, because several changes of
  state are silent in the events (a spark used up, the bonds and sparks of a destroyed
  glom, a gnoth the Gnoth-setter destroys, the validity between refutations, the box
  tree, the gnoths' bond lists, the reigning class and the threshold between the events
  that print them). iterations.md item 01 sets the budget on `blackburn-11` seed 1: at most
  2x the engine's own time and 200 MB, else "Numbo's `LayoutHistory` approach instead (a
  keyframe every 64 events)" (Numbo GUI, `layout_history.py`: a checkpoint every `EVERY`
  events, the rest rebuilt from the events). The thesis says nothing about displays; the
  world's three levels are Figure 1 [M86 p. 44].
- **Choice:** `snapshot(world, previous, source, cache)` reads every level into frozen
  dataclasses of plain data, shares with the previous snapshot every section that is
  equal to it, and keeps in a per-run `SnapshotCache` (plain keys: task numbers, spark and
  bond names, freeze-dried positions, type names) the records of the structures the
  engine never changes once made. The tap takes a snapshot at every `chosen` event while
  the clock is at most `DENSE_TASKS` (500; every appendix run has under 200 tasks
  [M86 pp. 164-179]), then at the first `chosen` event `KEYFRAME` (64) or more events
  after the last snapshot, and always at `prompt`, `guess`, `answered` and `ended`.
  `EventLog.state_at(index)` gives the snapshot in force, the last at or before the index;
  a pane model between two snapshots starts from the earlier one and applies the events
  after it, as Numbo's `LayoutHistory.layout_at` does.
- **Reason:** measured on `blackburn-11` seed 1 (10,110 tasks, 28,959 events, the engine
  alone 0.24 s; the engine is faster than when ENGINE-API.md was written, which measured
  26,110 tasks and 1.7 s at commit 9f48d40, so the 2x budget is stricter): a snapshot at
  every task boundary costs 0.15 ms each, 10,126 snapshots, 1.7 s, 7x the engine and
  33 MB; the dense-then-keyframes rule costs 928 snapshots, 0.43 s, 1.8x the engine and
  15 MB. The budget rules out the dense rule for long runs, and the keyframe rule meets
  it. Every appendix run, the course's showcase, stays exact at every task: a seek shows
  what the live run showed. The cache is what makes a snapshot cheap (before it, 2.2 ms
  each: the task arguments' text and the bonds' lines were the cost); it holds no engine
  object, only records under names and numbers the engine never reuses (DECISION D-014).
- **Alternatives:** a keyframe every 64 events from the start (rejected: the appendix runs
  would lose the exact seek for no gain); a snapshot at every task boundary (rejected by
  the budget, 7x); events for the silent changes (rejected by ENGINE-API.md A4: the engine
  is frozen); rebuilding the state from the events alone (rejected: it would drift).

### G-004: The tap learns the `World` after the `Session` is built; the `start` event has no snapshot
- **Sources:** `Session.__init__` makes the `World` (`run.new_world`) with the observers
  already subscribed and publishes `RunStarted` before it returns (DECISION D-069), so an
  observer cannot hold the world when it receives `start`. The Numbo GUI's observers
  never read the engine's state, so it has no such step. ENGINE-API.md section 2 lists the
  events that get a snapshot; `start` is not among them.
- **Choice:** `Tap(log)` is made without a world; `tap.attach(world)` gives it one, and
  `tap.attach_session(session)` is the run controller's `on_session` callback, called on
  the worker right after the `Session` is built and before `session.run()`. The `start`
  event is recorded without a snapshot; `state_at(0)` is None, and the views show an
  empty world there.
- **Reason:** the engine is frozen, so the session's constructor cannot be changed to hand
  the world to its observers; a callback after construction is the smallest hook, and
  nothing is lost: at `start` the world holds no structure.
- **Alternatives:** a snapshot of an empty world at `start` (rejected: the tap has no world
  yet); making the world first and passing it to the session (rejected: `Session` makes
  its own, and the GUI drives runs only through `Session`, TASK.md rule 1).

### G-005: The run controller and the event log are Numbo's, with sessions, tasks and terms for runs and iterations
- **Sources:** ENGINE-API.md section 3 and PANES.md section 1 adopt the Numbo GUI's
  `RunController` (`numbo/models/run_controller.py`): the worker thread, the gate
  subscribed last, the deliveries and acknowledgments, the modes, the stop by a
  `BaseException`, one worker at a time. Numbo's modes are paused, playing, step,
  step-iteration and run-to-end; its `start_run` takes a problem; its `EventLog` indexes
  iterations. PANES.md P11 asks for Step, Step task and Step term; Seek-Whence has no
  iterations: a run is paced by tasks [M86 p. 41] and by terms [MHN p. 3].
- **Choice:** `RunController.start_session(config, seed, lines, answers, answerer=,
  inp=, paused=, out=)` builds the `Session` on the worker; the modes are `step`
  (one event), `step-task` (on through the next `chosen` event), `step-term` (on through
  the next `prompt` event, so the display is waiting for a term), `playing` and
  `run-to-end`; `start_events` replays recorded events as Numbo's does; `start_oracle` is
  left to item 14 (session files). The outcome's `result` is a dict with the session's
  `outcome`, `summary`, `draws`, `tasks`, `terms` and `hypothesis`. The `EventLog` indexes
  terms (`term_index`, `term_at`) and clocks (`clock_index`, `clock_at`) where Numbo's
  indexes iterations, and keeps the snapshots beside the events. The engine lock
  (`EngineBusy`) is kept although every `World` is independent, so that one run is live
  at a time, as the timeline's seeking rule assumes. `event_text` prints the `doing task`
  line for a `chosen` event and the fields as `name value` pairs otherwise; item 10 may
  refine it. The default error reporter is the controller's own, because the engine's
  `report_to_stderr` expects an event and a callback's error has none.
- **Reason:** ANSWERS.md 7: every convention follows Numbo; the names change only where
  Seek-Whence's run differs in kind (a session with console inputs; tasks and terms).
- **Alternatives:** step-iteration kept under its name (rejected: nothing in Seek-Whence
  is an iteration); no engine lock (rejected: Numbo's rule that one run is live at a time
  is what makes seeking safe).

### G-006: The hypothesis's source is tracked from the events
- **Sources:** `HypothesisMade.source` says `template` or `fallback` [M86 p. 79] and
  `CandidateMade` names the candidate instantiated "as the new hypothesis for the
  system" [M86 p. 125]; the `Hypothesis` object has no such field (`seekwhence/
  hypothesis.py`). PANES.md P4 shows the form "with its source".
- **Choice:** the tap remembers the last `hypothesis` event's `source`, or `candidate`
  after a `candidate` event, and passes it to `snapshot(world, source=)`; the
  `HypothesisRecord.source` is that word, None when no such event came.
- **Reason:** the engine is frozen and the information is in the events; the tap is the
  one place that sees both the events and the world.
- **Alternatives:** leave the source out of the snapshot and let the hypothesis pane read
  the events (rejected: every pane model draws from the snapshot in force).

### G-007: The rack is read after the draw: the chosen task is off it
- **Sources:** `Taskrack.choose` pops the task before `World.run_task` publishes
  `TaskChosen` (DECISION D-007), and `World.run_now` posts and removes the Hfilter without
  a draw (DECISION D-012). PANES.md P6 shows for the chosen task "the chance it had (its
  weight over the total, computed from the snapshot before the draw)".
- **Choice:** the `RackSnapshot` at a `chosen` event is the rack as it is then, the chosen
  task already off it, with every waiting task's weight (urgency to the `urgency_power`)
  and the total weight. The rack before the draw is this rack plus the chosen task, whose
  urgency the event carries; the taskrack pane (item 08) computes the chance from the two,
  and shows none for a task run without a draw.
- **Reason:** the snapshot reads the world as it is at the event, never a reconstruction;
  the one task the draw removed is in the event.
- **Alternatives:** a snapshot before the draw (rejected: no event is published there, and
  the GUI never runs the loop itself, TASK.md rule 1).

## The main window, the bridge and the answerer (item 02)

### G-008: The guess is answered through the `answerer` hook by a blocking `InteractiveAnswerer`; the Bridge has a fourth signal
- **Sources:** the engine asks for the answer to a guess through `world.answerer.answer(
  world, form, block, terms)`, synchronously, inside the Hfilter task (`filtering.
  make_guess`, DECISION D-064), right after publishing `GuessMade` and before
  `GuessAnswered` [M86 pp. 166, 175]; `Session(answerer=)` takes a function of the console
  that returns the `Answerer` (DECISION D-069). TASK.md rule 5: in interactive mode the
  answerer blocks the worker until the GUI answers; answers are the session's inputs.
  PANES.md P1: the guess panel appears when the engine calls the answerer, already drawn
  up to the guess by the gate. The Numbo GUI has no user input during a run, so its
  `Bridge` has three signals (`delivered`, `state_changed`, `finished`).
- **Choice:** `models/answerer.py`'s `InteractiveAnswerer(console, on_guess)` is Qt-free:
  `answer` records the `GuessPending`, calls `on_guess` on the worker and waits on a
  `threading.Event`; `give(answer)` from the GUI thread releases it; `cancel()` releases
  it with the controller's `RunStopped`, so a stop or a close with a guess pending unwinds
  the run as a stop from any other wait does (the window cancels before `controller.stop()`,
  which would otherwise wait out its join timeout on a worker blocked on a click). The
  answer is written after the prompt with `Console.tell`, as a scripted answer is, so the
  transcript and the events of an interactive run equal those of the same run scripted
  (tested for run 1 answered `ok` and `no`, and run 3 answered `no` then `ok`). The window's
  `Bridge` gets a fourth queued signal, `guess_asked(generation, pending)`, and the guess
  panel's OK and No call `answer_guess`. Scripted and judged answers keep the engine's own
  `ConsoleAnswerer`.
- **Reason:** the hook is the engine's one entry for an answer, and it already runs on the
  worker; blocking there changes nothing the engine can see (ENGINE-API.md section 3).
  Writing the answer as `tell` does keeps the transcript the thesis's [M86 p. 166].
- **Alternatives:** the GUI polling a queue from a timer (rejected: the worker is the
  place that waits); a `Console.ask` on a GUI-fed stream (rejected: the answer prompt is
  not a console line, and a stream cannot be cancelled cleanly).

### G-009: A run starts from `RunInputs`; the window has a default run and the presets of item 02 are the appendix runs
- **Sources:** a run is its seed and its configuration (D-069), plus the
  console lines and the answers [M86 pp. 164-179]; the Numbo GUI's controls give a
  problem, a seed and a cap, and `start_run` takes them (Numbo GUI, `controls.py`,
  `main_window.py`). PANES.md P11 and item 14 define the controls; item 02 asks for the
  Presets menu and the `controls/*` settings keys before the controls exist.
- **Choice:** `models/presets.py` defines `RunInputs` (preset id, `Config`, seed, lines,
  answers, `interactive`, title) and the three appendix presets with Meredith's inputs
  and answers from `tests/data/expectations.json` on the golden seeds. The window keeps
  `inputs` (by default appendix run 1 on seed 54), the Presets menu sets them, the title
  shows them, and `controls/preset` and `controls/seed` are the settings keys of item 02;
  `MainWindow.start_session(inputs, paused)` starts the run and the Run menu starts one of
  `inputs` when no run is live, as Numbo's does with its controls' inputs. Item 14's
  controls will produce `RunInputs` and add the Blackburn dozen and the other lists.
- **Reason:** a plain-data record of the inputs is what the session file (item 13) saves
  and what the determinism contract passes; a default run makes Space do something from
  the first open, as in Numbo, where the first puzzle is selected.
- **Alternatives:** the window taking the five arguments separately (rejected: item 13
  needs the record anyway); no default run until item 14 (rejected: nothing to see).

### G-010: The layout precedence: the user's settings, the repository's `default_layout.json`, the built-in arrangement (a function)
- **Sources:** ANSWERS.md 3 (docks, QSettings, a Reset layout action) and ANSWERS.md 8
  (a default layout that travels with the code: `seekwhence/gui/default_layout.json`
  holding `saveState()` as base64 beside a readable record, no geometry, a format version
  and the Qt version; precedence settings, file, built-in; fallbacks that never crash;
  Save layout as default for this code…, Reset layout, Reset to built-in layout; a shipped
  file equal to the built-in). PANES.md section 3: Reset layout "restores the default dock
  arrangement and clears the saved state". The Numbo GUI restores `window/state` from
  QSettings and has no reset.
- **Choice:** the built-in arrangement is a function, `_arrange_builtin`, that puts every
  dock back in its area of PANES.md section 3, rebuilds the tab groups, sets the default
  visibility and sizes; it runs at construction and is the last fallback. The repository
  file is read and written by the Qt-free `models/layout_file.py` (`LayoutRecord`,
  `encode`, `decode`, `read_layout`, `write_layout`, `missing_docks`; format
  `seekwhence-gui-layout`, version 1, under 20 KB). `MainWindow(layout_path=)` names the
  file (the package's by default; None for none, as the tests' windows). On open: the
  settings' state if it restores; else the file if it reads, names only known docks and
  restores; else the built-in, with a status-bar line saying why. Reset layout re-arranges
  the built-in, applies the file over it, and removes `window/state` from the settings;
  Reset to built-in layout stops at the first step. `layout_source` says which is in force
  (`settings`, `default file`, `built-in`). The shipped file was written offscreen from
  the built-in arrangement. `pyproject.toml` gets `[tool.setuptools.package-data]` for the
  file, one entry beyond TASK.md's list, because ANSWERS.md 8 says the file ships with the
  package.
- **Reason:** a stored byte blob of the built-in would depend on the window's size when it
  was captured; a function is the same on every machine and is what the file is checked
  against. Applying the file over a fresh built-in arrangement means a partial
  `restoreState` cannot leave a dock stranded.
- **Alternatives:** the built-in as a `saveState()` captured at construction (rejected:
  size-dependent, opaque); the default file read by Qt alone with no readable record
  (rejected by ANSWERS.md 8); no package-data entry (rejected: an installed copy would
  silently lose the default).

### G-011: Placeholders for the panes not built yet; the view protocol; visibility by visible region
- **Sources:** item 02 lays the window out before any pane exists; the Numbo GUI's views
  are observers with `on_event` on the GUI thread and `redraw(now=False)`, counted by a
  `paints` attribute in the tests (Numbo GUI, `main_window.py`'s `add_view`,
  `tree_view.py`, `test_polish.py`'s probe). ANSWERS.md 4: the platoplasm network and the
  taskrack drawing are labelled GUI design, not Meredith's.
- **Choice:** `placeholder.py`'s `PanePlaceholder(title, pane_id, pages)` is a QWidget
  that paints its name, its pages and the number of events it has seen, counts its paints,
  and resets at `start`; the two central panes and the ten docks hold one each, with the
  pages PANES.md gives; P5 and P6 say "GUI design, not Meredith's" in the pane and its
  tooltip from the start. Each real view replaces its placeholder in its item and keeps
  the protocol. The tests judge whether a view is on screen by its `visibleRegion()`: in
  Qt 6 a dock tabbed under another is `isVisible()` yet has no pixels and gets no paint.
- **Reason:** the window's handshake (every view drawn once per event, then the
  acknowledgment) can be tested from item 02 on, with the same probe the real views will
  face; a placeholder that paints its own text (no QLabel) keeps the count at exactly one
  paint per event.
- **Alternatives:** empty QWidgets (rejected: nothing to count or read); QLabels
  (rejected: a text change may post a relayout and a second paint).

### G-012: The history is filled by the tap on the worker, not by the window on the GUI thread
- **Sources:** the Numbo GUI's history is fed on the GUI thread, in `_on_delivered`
  (`history.on_event(event)`), because its views need only the events. Here the tap must
  read the `World` at the moment of the event, inside the engine's thread
  (ENGINE-API.md section 2; G-003, G-004).
- **Choice:** the tap is subscribed to the controller (published to on the worker, before
  the delivery, and unsubscribed for a replay's playback, `_connect(recording=False)`); the
  window's `history` is the tap's `EventLog`, and `_on_delivered` only moves `position`,
  publishes to the views and draws. The GUI thread reads the log at indexes up to the event
  delivered, which the worker appended before delivering and does not touch again.
- **Reason:** the snapshot cannot be taken later than the event; the ordering (append,
  then deliver, then wait for the acknowledgment) is what makes the read safe.
- **Alternatives:** two logs, one on each thread (rejected: two copies to keep equal); the
  tap on the GUI thread reading a world that has moved on (rejected: wrong state).

### G-013: Menu actions whose items have not come are present but disabled; Step task is Ctrl+Right
- **Sources:** PANES.md section 4 lists the menus and Numbo's keys (Space, Right,
  Shift+Right, Left, Esc, Ctrl+N) and gives Step task no key; item 02 asks for the six
  menus before sessions (item 13), the configuration pane (item 15), the full presets
  (item 14) and the guide (item 17) exist.
- **Choice:** the menus are complete from item 02: Open session…, Save session…, Save
  transcript…, Save events…, Copy and the Blackburn presets are present, disabled, and say
  which item builds them in the status bar when triggered; Quit, Edit > Configuration,
  the Run menu, the View menu, the appendix presets, Reset layout, the layout file actions
  and Help work. Step task has Ctrl+Right, beside Right (step) and Shift+Right (step
  term). Help > What am I looking at? shows the pane-to-pages list until item 17's guide.
- **Reason:** a user sees the whole shape of the GUI at once, and the items that follow
  enable actions rather than add menus.
- **Alternatives:** menus added item by item (rejected: the item asks for all six).

## The panes

### G-014: The sequence pane's model is event-driven: fates under the hypothesis in force, the whole block as ghost cells aligned by position, badges
- **Sources:** PANES.md P1: cells green when "correctly predicted (`filtered` ok)", red
  when the term "refuted the hypothesis (`refuted`)", grey when "not yet filtered"; after
  the last term "the hypothesis's predicted continuation as ghost cells, grouped into its
  blocks"; the badges with "guess form, block, the answer given". The transcripts print
  the filter line `new term being hypothesis-filtered through (Cycle 3 2 (3 7))`
  [M86 p. 166], the refutation `set-out -- validity: 0` [M86 p. 168] and the guess's
  block, `3 7 3 7` [M86 p. 166], `(1 2)(1 2)(1 2)` answered `nope` [M86 p. 175],
  `(1 2)(1 2 3)(1 2 3 4)` [M86 p. 179]. ENGINE-API.md section 2, rule 3: predictions come
  from a fresh `Hypothesis(form)`. Every event the pane needs carries the form
  (`hypothesis`, `candidate`, `filtered`, `refuted`, `guess`).
- **Choice:** `models/sequence.py`'s `SequenceModel` is an observer built from the events
  alone, as the Numbo GUI's `RunStats` is; no snapshot. A term's fate is its verdict under
  the hypothesis in force: `filtered` sets `predicted` or `refuting`, `refuted` sets
  `refuting`; a new hypothesis (`hypothesis` or `candidate`, the one event the rebuild
  publishes) clears every fate to `unfiltered`, because the old verdicts belonged to the
  refuted form; a refused guess (`answered`, not accepted) keeps the form and the fates,
  as the engine keeps the hypothesis with validity 0. The ghost cells are the *whole*
  block `Hypothesis(parse(form)).guess_block(n, guess_min_hits)` gives on a fresh object
  (DECISION D-033; `guess_min_hits` read from the `start` event's config), drawn in a
  row under the term cells, each cell under its position, with a bracket under each hit;
  the cells beyond the last term are dashed and are the predicted continuation. So the
  row reads as the guess prints (`3 7 3 7`; `(1 2)(1 2 3)(1 2 3 4)`), and a cell under an
  entered term shows whether the fresh box agrees with it. The strip
  (`sequence_view.py`'s `SequenceStrip`) paints the cells, the block and the badges in
  one `paintEvent`, as the placeholders do (G-011); the input row, the guess panel and
  the buttons are ordinary widgets below it, and `paints` is the strip's count.
- **Reason:** the fates and the block are fully determined by the events, so the pane is
  exact at every index, live or seeking, with no snapshot lookup; drawing the whole block
  rather than only the continuation keeps the grouping visible (the hits are the synopsis
  the guess shows [M86 p. 7]) and shows, for the entered terms, what a fresh box
  postdicts. One paint per event keeps the probe of G-011 meaningful.
- **Alternatives:** fates from the snapshot's `covered` (rejected: the snapshot has no
  per-term verdicts and is absent between keyframes on long runs); keeping a refuting
  cell red after the rebuild (rejected: the new hypothesis explains it); ghosts of the
  continuation only (rejected: the grouping would be lost).

### G-015: Interactive terms come through a `TermFeed` stream the `Console` reads; the box is enabled at a prompt and queues several terms
- **Sources:** "a user presents to the system numbers which presumably follow some pattern
  the user has in mind" [M86 p. 37], one per prompt, `please enter a term:` [M86 p. 164].
  `Session(lines=None, inp=stream)` reads each line with `stream.readline()` and echoes
  it when the stream is not a terminal (`Console`, DECISION D-069). TASK.md rule 5:
  answers and console inputs are the session's inputs. PANES.md P1: the box is "enabled
  at a prompt"; "paste several terms ("3 7 3 7"), which queues them, one per prompt".
  G-008 settled the guess with a blocking answerer cancelled before `controller.stop()`.
- **Choice:** `models/term_feed.py`'s `TermFeed` is the stream: `readline` on the worker
  blocks on a `threading.Condition` until the GUI `put`s a line (`put_words` splits a
  typed `3 7 3 7` into four, queued in order); `close` gives the end of the stream (the
  outcome `eof`); `cancel` raises `RunStopped`, and the window cancels the feed with the
  answerer before every stop and at close. The window passes `inp=feed` when the inputs'
  `lines` is None; `presets.interactive_inputs(seed)` (Presets > Type the terms yourself)
  is such a run with interactive guesses too. The box is enabled while an interactive run
  is live and the model is at a prompt (from `start`, which precedes the first `please
  enter a term:`, and each `prompt` event until the `term`); its words go to the feed and
  the queue is shown beside the box. The feed has no `isatty`, so the transcript of an
  interactive run equals the scripted run's (tested: typing `3 7 3 7` on seed 54 and
  clicking OK gives run 1's events, text and draws [M86 p. 166]).
- **Reason:** the stream is the engine's one entry for console lines, and reading it on
  the worker changes nothing the engine can see; the same cancel rule as G-008 keeps stop
  and close from waiting out the join timeout.
- **Alternatives:** calling `Session.handle` from the GUI thread (rejected: the engine
  must stay on one thread, behind the gate); a box enabled at all times (rejected: PANES.md
  says at a prompt; typing ahead still works at the prompt, through the queue).

### G-016: The stats view paints its own rows and reads the snapshot through `set_position`; its fields
- **Sources:** PANES.md P12: clock, terms, tasks waiting, the reigning class, the
  threshold, the hypothesis and its validity, the outcome, and `Session.summary()` at the
  end; fed by the events and the snapshot. ANSWERS.md 5: the stats dock shows when a run
  uses a non-default configuration. The Numbo GUI's `StatsPanel` is a `QFormLayout` of
  `QLabel`s over a Qt-free `RunStats` with `texts()`, `set_state`, `finish(outcome)`.
  The validity, the rack and the threshold are not in the events (ENGINE-API.md
  section 2).
- **Choice:** `models/run_stats.py`'s `RunStats` is Numbo's shape (an observer, `texts()`,
  `finish`) with Seek-Whence's fields: state, run (title and seed), configuration
  (`default`, or `edited:` and the fields that differ from `Config()`), events, clock,
  task, terms, tasks waiting, reigning class, threshold, hypothesis, validity, outcome,
  summary; `update(snapshot)` reads the four snapshot-only fields. The view
  (`stats_view.py`) paints the rows itself in one `paintEvent` instead of Numbo's labels,
  so that each event costs exactly one paint (the probe of G-011). The window tells the
  views that read the snapshot which event comes next with `set_position(index)`, a
  method `add_view` discovers as it discovers `redraw` (Numbo GUI), called before each
  event is published, live and when seeking; the view then reads
  `history.state_at(index)`.
- **Reason:** the fields are what the CLI's summary and the transcripts' happenings
  report; `set_position` keeps the views free of the window and makes seeking exact
  (the snapshot in force at an index is a lookup in the log, G-003).
- **Alternatives:** Numbo's labels (rejected: a label change can repaint the parent and
  break the one-paint probe); views holding the window to read `position` (rejected:
  coupling); passing the index inside the event (rejected: events are the engine's).

### G-017: The show buttons read the snapshot at the cursor and print into a text area until the console pane
- **Sources:** the four commands `show`, `show-hypothesis`, `show-seq` and `show-parse`
  are typed at the prompt in the transcripts [M86 pp. 166-179]; `Session.command_lines`
  formats them (DECISION D-054). PANES.md P1: the buttons write the output "into the
  console (P8) exactly as `Session.command_lines` formats it, read from the snapshot at
  the cursor"; item 03 says "output to a temporary text area until item 04"; item 04
  records commands typed live as session inputs.
- **Choice:** `models/commands.py` rebuilds each command's lines from a `Snapshot` (the
  bonds' `show_line`s, the gloms as `session.glom_line` prints them, the gnoths' `show`
  fields, the parse text; `nil`, `()` and empty sections for the empty world before any
  snapshot), tested equal to `Session.command_lines` at every prompt of the appendix
  runs. The window's `show_command(name)` reads `history.state_at(position)` and prints
  into a read-only text area under the buttons, shown on the first click; it never calls
  the live session, and the click is not a session input (a command typed in the box of
  an interactive run does go to the session, as any console line, and is echoed in the
  transcript). Item 04 moves the buttons' output to the console pane.
- **Reason:** reading the snapshot works at any cursor position and with no run live;
  calling the session would read a world that has moved on, from the wrong thread.
- **Alternatives:** feeding the command to the live session (rejected: wrong thread,
  wrong moment, and it would alter the transcript as a side effect of looking).

Item 04 moved the buttons and their output to the console pane (G-019); `show_command`
keeps the rule that a click never asks the live session.

## The console (item 04)

### G-018: The console model keeps two `Trace`s and plays the console protocol on the run's input lines at each ask; the lines are append-only and a seek moves the cursor
- **Sources:** the transcripts are Meredith's only display [M86 pp. 164-179]; the engine's
  `Trace` writes their lines from the events (DECISION D-055), with `prompts=True` the two
  prompt lines too, while a `Session` prints its prompts itself and writes a command's
  output and the hint outside any event (DECISIONS D-054, D-069). PANES.md P8: two
  `Trace` instances, verbose and terse, fed by the tap, "every line tagged with the index
  of the event that produced it", the switch instant, lines after the cursor greyed. The
  item: the console text equals the CLI's output for the same seed and inputs, in both
  modes; ENGINE-API.md section 2: the GUI never asks the live world from the GUI thread.
- **Choice:** `models/console.py` keeps `Trace(verbose=True, prompts=True)` and
  `Trace(verbose=False, prompts=True)`, fed every event on the GUI thread, and tags their
  lines with the event's index. The lines the session writes outside events are
  reconstructed by playing the console protocol on the run's input lines: after `start`
  and after every `prompt` the session is at an ask; the model reads its queued lines
  until a term (whose echo comes with the `term` event): a command is echoed and its
  output printed from the snapshot in force at the ask (`models/commands.py`; the tap
  always snapshots at a prompt, G-003), `bye` ends, another word gets the hint, and no
  line left with the input closed is the empty prompt. A scripted run's lines are all
  queued at `begin`; an interactive run's are `put` as the user types them, read at once
  if the session waits at an ask, else at the next prompt, which is when the session
  reads them. `finish` adds the empty prompt of an `eof` and, for a scripted run, the
  summary line the CLI prints. The lines are append-only: a seek moves the cursor and the
  view greys the lines after it; an event given again (index at or before `seen`) adds
  nothing, and `begin` announces the run whose `start` clears. The text equals the CLI's
  stdout for the three appendix runs in both modes and the session's own transcript
  (`outcome.output`) for scripted and interactive runs (tested).
- **Reason:** the CLI's text has lines no event carries; reading the session's output
  stream would give them in one mode only and would need the trace's own lines told
  apart from the rest, while the input lines are the GUI's to begin with and the
  snapshot at a prompt is exactly the state the session prints then. Append-only lines
  make a seek free and keep the live transcript whole, as Numbo's log view does.
- **Alternatives:** tapping the session's `out` stream tagged by event (one mode, and
  the trace's lines would have to be separated from the prompts); rebuilding the lines
  on every seek (slow for a long run and no better); a third `Trace` for the session's
  native mode (redundant: the two cover it).

### G-019: A command typed or clicked with an interactive run live is a session input; otherwise it prints at the cursor; find, Save transcript and Copy
- **Sources:** the item: a command line "for terms and show/show-hypothesis/show-seq/
  show-parse", output "formatted exactly as `Session.command_lines` and read from the
  snapshot at the current position", "commands typed live are recorded as session
  inputs", the item-03 buttons move here; PANES.md P8: click to seek, Find (Ctrl+F),
  Copy, "Save transcript..."; PANES.md P1: a command used in a live session "is recorded
  as an input, so the appendix runs replay with her commands in place".
- **Choice:** the window's `enter_console_line` splits the text into words as the
  sequence pane's box does (`split_words`). With an interactive run live (its `TermFeed`
  reads the GUI's lines) every word goes to the console model and the feed, in that
  order, through one path (`_put_inputs`, shared with the sequence pane's box), so the
  session and the console read the same lines in the same order and the typed command
  is an input, echoed with its output when read (at once at a prompt). With no such run
  live, a command prints at the cursor from the snapshot (`ConsoleModel.print_command`,
  appended at the end of both modes' lines with the cursor's index, not an input), and
  a term, `bye` or anything else is refused with a status line. The four buttons call
  the same `show_command`. Find is a box above the rows (Enter or the button: the next
  match after the cursor row, case ignored, wrapping round); Session > Save transcript…
  writes the text in the mode shown (`save_transcript(path, mode)`); Edit > Copy puts it
  on the clipboard. The Find shortcut Ctrl+F is not bound (the box has focus by a
  click; item 19 may add it).
- **Reason:** one path for every typed line is what keeps the console equal to the
  session's transcript; a command with no session to read it still has a state to show,
  the cursor's. Appending at the end keeps the model append-only (G-018).
- **Alternatives:** showing a typed command at once even in a live run (its output
  could differ from what the session prints at the next prompt); inserting the printed
  command after the cursor's line (reorders the transcript); a dialog for find.

### G-020: The console's rows are Numbo's `LogList`, with a single click to seek
- **Sources:** PANES.md P8 ("Click a line to seek to its event"); the Numbo GUI's
  `log_view.py` (`LogList`, a `QAbstractScrollArea` painting only the rows in view, a
  double-click or Enter to seek, a tinted row for the event drawn now, greyed future
  rows, Follow); ANSWERS.md 7 (follow Numbo and say so).
- **Choice:** `ConsoleList` is `LogList` adopted: rows straight from the model (no
  per-row state, one paint per event however long the transcript), the cursor's last
  line tinted, the lines after it greyed, a keyboard cursor row (arrows, Page Up/Down,
  Home, End; Enter seeks), the whole line as a tooltip, Follow. A single click seeks, as
  PANES.md P8 says, not Numbo's double-click. The blit on scroll is left out: the
  whole viewport is repainted, one paint either way.
- **Reason:** the view is proven in Numbo; PANES.md is explicit on the click.
- **Alternatives:** a `QPlainTextEdit` (its appends and selections paint more than once
  per event and it has no event index per line); Numbo's double-click.

## The cytoplasm (item 05)

### G-021: The cytoplasm model is the snapshot plus the `glint`, `spark` and `bond` events since it; flashes and fading ghosts from the events
- **Sources:** PANES.md P2 ("The snapshot is authoritative for what exists, because
  several removals are silent"; "Events give the animation: flash on creation, fade on
  destruction"); ENGINE-API.md section 2 (a spark used up by a Tester and the bonds of a
  dissolved glom leave no event); G-003 (a snapshot at every `chosen` event while the
  run is short, keyframes after, and always at the prompt, the guess, the answer and
  the end); the thesis prints a spark or a bond when it is made [M86 p. 164] and says
  nothing of its end.
- **Choice:** `CytoplasmModel` is fed like the stats model (G-016): `on_event` for every
  event, then `update(snapshot)` with `history.state_at(index)`. A new snapshot replaces
  the glints, sparks and bonds by name; a `glint`, `spark` or `bond` event after it adds
  what it announces (a new glint takes the glint before it as its left neighbour, and
  gives itself as that one's right neighbour [M86 p. 61]), so the picture is exact at
  every event, live and when seeking. The event's index is kept as `made_at`: the mark
  flashes for its first `FLASH` (6) events. A spark or bond a snapshot no longer lists
  becomes a ghost with `gone_at` the snapshot's index, fading to nothing over `FADE`
  (24) events, then pruned. Bond values come from the gnoths' `internal_bonds` and
  `external_bonds` in the snapshot, an internal listing winning over an external one
  (the same bond is external to both its gnoths) [M86 p. 102].
- **Reason:** the snapshot decides what exists and the events decide what is new; between
  snapshots nothing is removed silently that the model could know about, and the three
  creation events are exactly the additions (`seekwhence/cytoplasm.py`). Ghosts answer
  PANES.md's "fade on use" without any engine help. The windows are event counts, not
  times, so a replay at any speed and a seek show the same thing; PANES.md's "about
  150 ms, scaled by speed" animation is left to the controller's delay.
- **Alternatives:** rebuilding the state from the events only (wrong: the silent
  removals); a snapshot at every event (G-003 rejected the cost); time-based fades.

### G-022: The cytoplasm's layout: glints on a baseline, arcs below it with depth by adjacency and gap, sparks dashed and deeper, a glom anchored at its span; the tooltips
- **Sources:** Ch. 3 Figure 1 [M86 p. 102]: the glints as teardrops, the value above
  the name, in a row; the bonds as arcs below them, the adjacent successor bond short
  and the remote sameness bonds tall, each labelled `+10` or `-5`; Figure 2
  [M86 p. 106] draws the glints the same way. PANES.md P2: "adjacent bonds as short arcs,
  remote bonds as tall ones", "Sparks as faint dashed arcs between the two elements".
  The thesis has no figure of a spark. The glint's fields [M86 p. 61], `show`'s bond line
  [M86 p. 167], the trace's spark line [M86 p. 164].
- **Choice:** a pure `layout(model)`: glint `n` stands on the baseline (y = 0) at
  `n * SPACING`, a teardrop `GLINT_WIDTH` by `GLINT_HEIGHT` with the point up; an arc
  runs between its members' centres and dips below the baseline, `ADJACENT_DEPTH` for
  an adjacent bond, `REMOTE_DEPTH` plus `REMOTE_DEPTH_PER_GAP` per position of gap for
  a remote one, so that wider bonds pass under narrower ones; a spark dips
  `SPARK_EXTRA_DEPTH` deeper than a bond of its span would, so it never hides a bond;
  arcs between the same pair are stacked `STACK_STEP` apart; a bond's value is drawn
  under its lowest point as `+10`, `-5` or `0`. A glom is anchored at the middle of its
  span on the baseline (part 2, item 06, draws the hulls above it); an arc with a member
  not drawn is left out. The tooltips are the thesis's own formats: a glint's fields
  (`name`, `print-value`, `position`, `left-nbr`, `right-nbr`, `bonds-in`), `show`'s
  line for a bond with its value in the reigning class when known, the trace's line for
  a spark.
- **Reason:** the figure is followed where it exists; sparks go below the baseline with
  the bonds because that is the "glue" region and the space above is the gloms'
  (item 06, the islands of order [FCCA pp. 58-60]); dashed, faint and deeper keeps them
  apart from the bonds. Depth by gap is the figure's own convention (its remote bonds are
  taller than its adjacent ones).
- **Alternatives:** sparks above the baseline (they would cross the hulls); depth by
  strength; a label on every arc.

### G-023: The cytoplasm's styles: colour by type, width by strength, the flash, the highlight, inactive glints lighter; the canvas is Numbo's `TreeView`
- **Sources:** PANES.md P2: "colour by type: Same (blue), Succ (green), Pred (orange), as
  Hofstadter's sameness, successorship and predecessorship glue [FCCA pp. 58-60]";
  "line width by strength"; "Zoom and pan (QGraphicsView), with fit-to-width as the
  default"; PANES.md P1 (a click on a term cell highlights its glint); the glint's
  `active` field [M86 p. 67]; the Numbo GUI's `tree_view.py` (a `QGraphicsView` with
  items kept by name, pure `node_style`, auto-fit, zoom with the wheel and keys, a paint
  counter, `render_png`) and `render.py`.
- **Choice:** pure functions `glint_style`, `spark_style`, `bond_style` and
  `value_label`: a bond is blue, green or orange by type (grey otherwise), its width
  1 to 3.5 for strength 0 to 10 (DECISION D-019's scale), solid; a spark is grey,
  dashed, at 0.55 opacity, drawn strong while it flashes; a flash is a pink halo for
  the first `FLASH` events; a ghost's opacity falls linearly to 0 over `FADE` events; a
  glint not `active` (inside a glom) is drawn at 0.7 opacity; the highlighted glint has
  a thick orange border. `CytoplasmView` is Numbo's `TreeView` adopted: one
  `GlintItem` per glint and one `ArcItem` per arc, kept by name and changed only where
  they differ; auto-fit on at first and after Fit, zoom with the wheel, + / -, 0 and F;
  `paints` counted; `render.py` is Numbo's. `CytoplasmCanvas` (Numbo's `TreeCanvas`)
  adds the bar with Fit, zoom and a legend, and delegates the view protocol. The
  window's `paint_all` settles the canvas's scene.
- **Reason:** the colours are PANES.md's; the strength scale makes a span bond (strength
  2) visibly thinner than a print-value bond (10 or 5); Numbo's canvas is proven and the
  paint probe holds for it (one paint per event).
- **Alternatives:** a plain `QWidget` painting everything (no zoom and pan); colour by
  family (Same, pred-succ) rather than type.

## The cytoplasm, part 2 (item 06)

### G-024: The gloms are nested hulls above the baseline, labelled by type or dubbing; pseudo-gloms dotted on the top-down layer; proposals as markers keyed by task number; burst, dissolve and the blast as fading ghosts
- **Sources:** PANES.md P2: "Gloms as nested rounded hulls above the baseline, one level
  per nesting depth, so a glom of gloms sits above its subgloms", labelled with the type
  and, once dubbed, the plato-class, the form and the purity [M86 p. 69]; "Pseudo-gloms
  ... as dotted hulls, drawn only when the 'top-down' layer is on"; "a small marker on
  the target glint for each `proposed` Glomtester ... removed by `glom`, `glom-failed`
  or `glomtester-failed`". The thesis draws a glom as a circle above its glints joined
  to each by a line (Ch. 3 Figure 1 [M86 p. 102]) and has no figure of a pseudo-glom
  or a proposal; it describes dubbing [M86 p. 69], dissolving and bursting
  [M86 pp. 69-70], the template glom [M86 p. 73], the gnoth-gloms [M86 p. 77] and the
  blast [M86 p. 92]. FCCA's islands of order are stretches of a sequence bracketed
  together [FCCA pp. 58-60]. The engine: a Glomtester's rejection is silent
  (`seekwhence/gloms.py`), the frame gloms go without an event
  (`seekwhence/filtering.py`), and the snapshot lists the gloms with `type`, `pseudo`,
  `subgloms`, `parent` and `structure` (G-003).
- **Choice:** a `HullMark` per glom in the model, from the snapshot (`_refresh_gloms`,
  keeping `made_at`) and from the `glom`, `top-down` and `operation-glom` events since
  it; `dubbed` appends to the structure; `burst`, `blast` and `blasted` take the glom
  and its non-glint subgloms, `dissolved` the glom alone and frees its members; a glom
  a snapshot no longer lists has the fate `gone`. The nesting `depth` is 1 for a glom of
  glints and one more than the deepest member otherwise. The layout puts a hull over
  its glom's span, `HULL_PAD_X` plus `HULL_STEP_X` per level beyond the teardrops on
  each side, its top `HULL_GAP` plus `HULL_BAND` per level above the glint tips, its
  bottom just under the baseline so the teardrops are inside; the outer hull is laid
  out first and drawn under the inner. The label is `glom1  Same print-value fence`,
  or `glom1  Cycle (Cycle 3 2 (3 7)) pure` once dubbed (several dubbings joined by
  `;`), or a pseudo-glom's kind from its type (`template glom`, `gnoth-glom`, `frame
  glom`, `top-down unit`; `pseudo-glom` from a `top-down` event until a snapshot says
  which, except the template glom, named by the `template-glom` event before it); the
  view elides a label to its hull's width and the tooltip (`show`'s glom line
  [M86 p. 167], the subgloms, the structure, the fate) has it whole. A pseudo-glom is
  dotted, grey and unfilled; a dubbed glom's border is darker and its label bold. A
  ghost fades over `FADE` events as the arcs do (G-021). The proposals are
  `ProposalMark`s keyed by task number: a `posted` Glomtester makes one (stage
  `proposed`, a hollow triangle at the target's tip, in the family's colour), a
  `posted` Glommer one in the stage `glomming` (filled, with a bracket over the members)
  and resolves the Glomtester chosen; `glomtester-failed`, `glom`, `glom-failed` and a
  stale Glomtester or Glommer resolve the task chosen; at a snapshot the proposals are
  the rack's Glomtesters and Glommers plus the one chosen and not yet resolved (the
  chosen task is off the rack, G-007). Several markers on one target stand side by side
  in task order and the first carries one label naming them all.
- **Reason:** PANES.md's hulls are the design, and they make the nesting visible in the
  sequence's own geometry, which Figure 1's circle-and-lines tree does not; the figure's
  convention is recorded here, not followed. Keying proposals by task number makes the
  rack the authority at every snapshot (a silent rejection, a cleaning, a stale task all
  leave the rack and so the picture) and lets the events carry the picture exactly
  between snapshots; the marker appears at the `posted` event, one event after the
  `proposed` line, because only the `posted` event has the number. Fading ghosts answer
  PANES.md's "fade on destruction" for the four ways a glom goes, with no engine help.
- **Alternatives:** Figure 1's circles above the glints with lines to the members; hulls
  by position only (no bands, overlapping outlines); proposals keyed by target (two
  identical proposals would collapse); a marker at the `proposed` event (it has no task
  number, so it could not be matched to the rack).

### G-025: One `Selection` for every pane, held by the window; a glom selects everything under it; a click toggles
- **Sources:** PANES.md P2: "Click to select: the selection is shared with every pane
  (its gnoth, its plato-class, the tasks whose arguments name it, its log lines)";
  PANES.md P3 and P5 (a click on a gnoth or a node selects in P2); iterations.md item
  06 ("`models/selection.py`, a selection shared by all panes"; "selecting a glom
  selects its glints"). The Numbo GUI has no shared selection.
- **Choice:** `seekwhence/gui/models/selection.py`, Qt-free: the window owns one
  `Selection` (`kind`, `name`, `members`, listeners called after every change) and
  gives it to the cytoplasm canvas; the panes to come subscribe to the same object.
  Selecting a glom selects it with its subgloms and glints at every depth
  (`CytoplasmModel.under`), a bond or spark with its two ends, a glint alone. A left
  click on the canvas toggles the element under the mouse (the hull's outline and label
  band, a teardrop, an arc's stroke; a marker stands for its target) and a click on
  empty canvas clears; the sequence pane's cell click (`highlight_term`) selects the
  glint and keeps the model's `highlighted` for it. The view repaints on every change
  and emits `selected(name)`. The selection holds names only.
- **Reason:** one object, names only, is the least the panes need to agree on, and it
  keeps the engine contract (no engine object is kept). Toggling on a second click is
  the usual way to deselect without a key.
- **Alternatives:** a Qt signal on the window (the models could not be tested without
  Qt); a selection per pane synchronised by the window.

### G-026: The four layers are checkable actions on the canvas and in View > Cytoplasm layers; sparks and the top-down layer off by default; not remembered
- **Sources:** PANES.md P2: "Toggles: sparks, top-down layer, bond values, names";
  PANES.md section 3, hidden by default: "sparks and pseudo-gloms inside P2"; section
  4: "View: every dock's toggle and the timeline's; the P2 layers; Reset layout". The
  Numbo GUI's log view keeps its kind filters as checkable `QAction`s in a menu and does
  not save them in QSettings.
- **Choice:** the model holds `layers` (`sparks`, `top-down`, `values`, `names`), and
  the pure `layout` leaves out what is off: the spark arcs, the pseudo-gloms' hulls, the
  value labels, the glint names and the glom names in the hull labels. The view makes
  one checkable `QAction` per layer (`layer_actions`); the canvas's bar shows them as
  tool buttons and the window lists them under View > Cytoplasm layers. Sparks and the
  top-down layer start off, as PANES.md section 3 says; the layers survive a new run
  and are not saved in QSettings, as Numbo's filters are not. Item 05 drew the sparks
  always; from this item they are a layer, and item 05's tests switch it on.
- **Reason:** keeping the layers in the model keeps `layout` a pure function of the
  model, so the pixel probe compares the view with a fresh build of the same model;
  actions shared between the bar and the menu keep the two in step for free.
- **Alternatives:** layers applied in the view only (the layout would not describe the
  picture); saving the layers in QSettings (Numbo does not).


### G-027: The taskrack model is a shadow of the rack kept from the events, corrected by every snapshot; rows by type in order of first appearance with their share of the weight; the chosen task's chance from the rack before the draw; the Hfilter run at once has none
- **Sources:** PANES.md P6: the waiting tasks "grouped by type, each group a row with its
  count and a bar of summed weight", the chosen task with "the chance it had (its weight
  over the total, computed from the snapshot before the draw)", fed by `posted`, `chosen`,
  `stale`, `cleaned` and the snapshot's `rack`. "All tasks have 'urgencies' (integer
  weights), and more urgent tasks will have a greater chance of being chosen than less
  urgent ones do" [M86 p. 42]; one draw proportional to the urgency to `urgency_power`
  (DECISION D-007); the cleaning rule (DECISION D-010); the Hfilter of a new term is set
  out and run at once, ahead of any draw (DECISION D-012; `World.run_now`). G-007: at a
  `chosen` event the snapshot's rack is the rack after the draw. The Numbo GUI's coderack
  model is a shadow kept from the events alone, checked against the event's rack counts.
- **Choice:** `models/taskrack.py`'s `TaskrackModel` keeps the rack as a shadow from the
  events: `posted` adds a `TaskRecord` (the snapshot's own record type, with `at_term`
  counted from the `term` events and the weight from the `start` event's `urgency_power`),
  `chosen` removes by number, `cleaned` applies DECISION D-010's rule to the (type, count)
  pairs the event carries (at `term`, the type's tasks posted before the previous term;
  else all of the type; the oldest `count` if the rule's count disagrees). Every new
  snapshot's rack, read from `Taskrack.tasks`, is the authority: where it differs the
  shadow is replaced and `corrections` counts it (the tests require zero on the appendix
  runs and on blackburn-1, with the dense rule and with keyframes only, so the shadow is
  exact at every event between snapshots). The rows are the types in order of first
  appearance on the rack (`Taskrack.count_by_type`'s order, so a test compares them with
  the snapshot's counts), each with its count, summed weight and share of the total: the
  chance the next draw falls on the type. The chosen task's chance is its weight over the
  rack's total before the draw (the shadow's total plus its own weight); `draw_chances`
  gives every task's at that draw and they sum to 1. An Hfilter chosen as the very next
  event after its own `posted` event was run at once (DECISION D-012): it has no chance,
  and the band says so.
- **Reason:** the events alone give the rack exactly, as the tests show, and the snapshot
  keeps it honest when they would not; keeping the snapshot's `TaskRecord` lets the two be
  compared field for field. The share of the total weight is what the bar means under
  DECISION D-007, and it is bounded, so the bars do not need Numbo's running scale. The
  Hfilter is the only type `run_now` sets out, and a chance for a task that was never
  drawn would mislead.
- **Alternatives:** the rack from the snapshot only (stale between keyframes on a long
  run); bars scaled by the largest count so far (Numbo); treating every task posted and
  chosen in succession as run at once (a drawn task can be, too).

### G-028: The taskrack pane is two virtualised lists after Numbo's `LogList`: the band, the banner and the rows above, the whole history below; a click on a task selects what its arguments name; the band stays tinted while the task runs; the banner lasts `BANNER` events
- **Sources:** PANES.md P6: the chosen task "on a highlighted band", `stale` in grey,
  cleanings "as a transient banner", "a history of the last N chosen tasks ... scrollable",
  "the history is a virtualised list, as in Numbo's log view"; "Expanding a row lists the
  tasks"; "Click a task to select the structures named in its arguments"; "Filter by
  arena". The Numbo GUI's coderack view paints the band and the bins in one `paintEvent`
  and tints the band only at the choice event; its log view is a virtualised
  `QAbstractScrollArea`. ANSWERS.md 4: the drawing is GUI design, not Meredith's.
- **Choice:** `taskrack_view.py`: a `LineList` (Numbo's `LogList`: rows painted straight
  from the model in the dirty rectangle, a scroll bar, a tooltip per row, a click per
  row), with two subclasses in a vertical splitter. `RackList` paints the `Line`s of the
  pure `rack_lines(model, expanded, arena)`: the chosen task on a band tinted from its
  `chosen` event until the next one (the task is the one running; grey with its reason
  once a `stale` event names it), a note with its clock, urgency and chance, the cleaning
  banner while `model.banner()` is up (`BANNER` = 30 events after the `cleaned` event, so
  a seek shows it too), the count of waiting tasks, then one row per type with a
  triangle, the type in its arena's colour, a bar of its share with the count in it, the
  share as a percentage; a click on a row opens or folds it (the view's `opened` set) and
  the "every task" box opens them all. `HistoryList` keeps one index per `ChosenTask` that
  passes the arena filter and builds a row's `Line` only when painted, so the whole
  history is kept (not PANES.md's last N) and 26,000 tasks cost 26,000 integers; Follow
  keeps the last row in view. A click on a task, waiting or in the history, toggles in the
  shared selection the structures its arguments name (`names_in`: `glint1`, `glom2`,
  `bond1`, `spark3`, `gnoth1`, also inside nested lists), and the view emits `selected`;
  tasks naming anything selected are outlined, so a click in the cytoplasm shows "the
  tasks whose arguments name it" (G-025). The arena filter is a combo box over the rows
  and the history. The pane's label says "GUI design, not Meredith's" with the pages in
  its tooltip. `paints` is the rack list's count, so the once-per-event probe covers it.
- **Reason:** one list class for both halves keeps the painting in one place and the
  rows virtual; tinting while the task runs shows which task the events between two
  `chosen` events belong to, which Numbo's choice-event flash does not; keeping the whole
  history costs nothing once the rows are virtual and lets the arena filter and a seek
  find any task. Event counts for the banner, as for the cytoplasm's flashes (G-021),
  make a replay and a seek show the same thing.
- **Alternatives:** Numbo's single painted widget (no scrolling for a long rack); the
  band tinted at the choice event only (Numbo); a timer for the banner (not reproducible
  on a seek); the history capped at N.

### G-029: The tallies count the tasks chosen since the last `term` event; the arenas' colours
- **Sources:** PANES.md P6: "a per-type tally since the last term"; DECISION D-057 gives
  each type its arena, "conceptual (platoplasm), short-term analytical (socratoplasm), or
  perceptual (cytoplasm)" [MHN p. 3], with Call-term and empty-task for the user. No colours
  are given anywhere.
- **Choice:** `tallies` counts each type chosen since the last `term` event and is cleared
  at it; `tally_text` prints `since term 3: Sparkler 4 · Tester 2` above the history (or
  `since the start:` before any term). The arenas' colours: cytoplasm teal, socratoplasm
  purple, platoplasm brown, user slate (`ARENA_COLORS`), distinct from the cytoplasm pane's
  bond colours (G-023) so that a row's colour is never read as a bond type.
- **Reason:** the `term` event is the moment the thesis's "since the last term" names, and
  the tallies read at a prompt then say what the program did with the term just given.
- **Alternatives:** tallies since the last prompt; the Numbo GUI's colours.

### G-030: The socratoplasm model is the snapshot's gnoths plus the gnoth events since it; silent changes wait for the next task boundary and the card says so; ghosts fade
- **Sources:** PANES.md P3: one card per gnoth with the fields `show` prints
  [M86 pp. 168, 175], the Gnoth-maker's record [M86 p. 78], the catchall [M86 p. 98],
  the parse as `show-parse` prints it [M86 pp. 174, 178]; fed by the gnoth events and
  the snapshot's `gnoths`, `catchall` and `parse`. ENGINE-API.md section 2: gnoths the
  Gnoth-setter destroys have no event; the gnoths' bond lists are silent. The engine:
  `slipping.set_gnoths` destroys the catchall and every gnoth overlapping a frame with
  no event and sets the equivalence type after the `gnoth-filled` event; `assess_bonds`
  (the Reformulator, the Bond-assessor, the slip-or-stay check of the Gnoth-setter and
  the Slip-scout) fills the bond lists silently [M86 pp. 99-100]; the Gnoth-caster sets a
  cast's form silently [M86 p. 125]; a slip moves every gnoth to the new class
  [M86 pp. 174-175]; a shift uncaps both gnoths before recapping them [M86 p. 110]; the
  Gnoth-maker takes the template glom before its first cap (DECISION D-062). G-021 does
  the same for the cytoplasm.
- **Choice:** `models/socratoplasm.py`'s `SocratoplasmModel`, fed like the stats model
  (G-016). The snapshot decides which gnoths exist and what their fields say (`fields`
  is the record's own, read off `show_lines`). The events since it add, remove, fill and
  recap what they announce: `gnoth` and `catchall` add a card (the form from the one
  dubbed member's structure, DECISION D-066), `gnoth-filled` fills one with the reigning
  class, `gnoths-set` keeps the set's names, frames and ranges and ghosts the rest,
  `gnoth-gone` and `blasted` ghost, `gnoth-uncapped` empties, `gnoth-capped` and
  `operation` recap by pairing the `top-down` caps made since the last gnoth event with
  the gnoths the event names, in order; a Gnoth-setter's `top-down` cap clears its span
  first (the catchall and every other overlapping gnoth go, but the one that begins where
  the frame begins); a Gnoth-maker's first cap takes the template glom. The names, frames,
  caps and ranges and the parse are then exact at every event (the tests check them
  against an independent reader of the `World` on the appendix runs and on blackburn-1,
  -5, -8 and -11) except while a Gnoth-operator or Capper runs, between its first change
  and its `operation` or `gnoth-capped` event (a shift's silent uncap), and at the
  `cleaned` event just before a `blasted` one. The fields the engine changes silently are
  not rebuilt: while a socratoplasm task or the Slip-scout runs, every live card is
  `confirmed=False` until the next task boundary, the card's heading turns amber and the
  tooltip says so; the fields of a confirmed card equal `Gnoth.show_lines()` at every
  event (tested). The catchall's range, which the Hfilter grows silently, is never
  claimed by the events. `corrections` counts the snapshots that disagreed with the
  cards the events built (zero on every run tested). A gnoth gone stays as a ghost fading
  over the cytoplasm's `FADE` events. "Copy as `show`" copies the `gnoths:` block of
  `show` rebuilt from the cards (`show_block`), which equals `Session.show_lines()`'s at
  every snapshot.
- **Reason:** the snapshot is the authority and the events keep the picture exact between
  task boundaries where they can; where the engine changes a card silently, saying "the
  next task boundary confirms this" is honest, and the dense rule (G-003) makes that
  boundary the next `chosen` event on every appendix run. Pairing caps by order follows
  the engine's own order of recaps (`operations.shift`, `recap`, `slipping.fill_gnoth`).
- **Alternatives:** the snapshot only (a gnoth made by the Gnoth-maker would appear one
  task late); rebuilding the bond values from the bonds' strengths and the reigning
  class's pulls and pushes (a re-implementation of `assess_bonds` the gate forbids in
  spirit: the GUI would compute what the engine computes).

### G-031: The gnoths are a row of rectangles over the cytoplasm's columns; the cards are one `LineList` with a bond line per bond; a click selects the gnoth with its glom and members; a hovered bond lights its arc
- **Sources:** PANES.md P3: "The gnoths are drawn as rounded rectangles over the
  cytoplasm's columns, aligned with the glints of P2. The top row of Ch. 3 Fig. 1
  [M86 p. 102] is 'gnoth1 gnoth2 gnoth3' over their gloms"; "Click a gnoth to select it
  and its glom and members in P2"; "Hover a bond in a card's bond list to highlight the
  arc"; the parse "large". Figure 1 [M86 p. 102] draws the gnoths as rounded rectangles
  in a row, each over its glom circle. The cytoplasm pane lays its glints at
  `position * SPACING` (G-022) and auto-fits the canvas (G-023). G-025: one selection;
  G-028: the taskrack's `LineList`.
- **Choice:** `socratoplasm_view.py`: the parse in a large monospace label; a
  `GnothStrip` painting the pure `gnoth_boxes(model)`: one rounded rectangle per gnoth
  from `BOX_PAD_X` left of its first glint to `BOX_PAD_X` right of its last, in the
  cytoplasm's units, scaled down to fit the strip's width (the two panes are docks of
  different widths, so the columns are the same proportions, not the same pixels), the
  name over the terms it covers, the gnoths of the parse on one row, a box that would
  overlap another on the next row, the catchall dotted and grey, an uncapped gnoth dashed
  [M86 p. 113], a ghost fading, an unconfirmed card's box amber; a `CardList`, the
  taskrack's `LineList` with its own `CardLine`s from the pure `pane_lines(model)`: the
  template's block first, then per gnoth a bold heading (`gnoth1  Cycle  frame 0`) on a
  tinted band, the fields exactly as `show` prints them, and under each bond field one
  `bond` line per bond (`bond2  0`), then the catchall's block and the ghosts' in grey.
  A click on a box or a card toggles the gnoth in the shared selection with its
  gnoth-glom and everything under it (`SocratoplasmModel.select_gnoth`; the catchall with
  its glints), so the cytoplasm's hull and glints light up, and the view emits
  `selected`; a gnoth whose glom is selected from the cytoplasm is outlined. Hovering a
  bond line emits `bond_hovered(name)`, which the window connects to the cytoplasm
  canvas's new `highlight(name)`: the model's `highlighted` now names a glint or an arc,
  and `bond_style`/`spark_style` take `lit` (highlighted or selected), drawn as a flash
  is. The label "GUI design" is not needed: the row of rectangles is Figure 1's.
- **Reason:** the strip keeps Figure 1's geometry with the cytoplasm's own constants, so
  a gnoth's box and its glints line up by proportion; one list for the template and the
  cards keeps the pane to one paint per event; bond lines under the fields let a hover
  name one bond, which the printed `((bond2 0) (bond3 -5))` line cannot.
- **Alternatives:** the strip inside the cytoplasm's `QGraphicsView` (one pane, but the
  socratoplasm is a dock of its own in PANES.md section 3); hit-testing the bond names
  within the printed line by x; a tree widget for the cards.

### G-032: The template is kept from its events: the four fields, the check line, the fate and the terms filtered
- **Sources:** PANES.md P3: the template's "four fields, `(form (Cycle 3 2 (3 7)) state
  working coverage (1 2) glom glom2)` [M86 p. 167]; its fate: accepted [M86 pp. 75-76],
  filtered (each term fits or not [M86 p. 74]), modified, abandoned [M86 pp. 74-75],
  resolved by a blast [M86 p. 75]; the evaluator's check line, `check-cycle template
  (Cycle 3 2 ((3 7)))` [M86 p. 167]"; fed by the seven template events and `blast`, and
  the snapshot's `template`. The snapshot's template is None once abandoned or released
  (`templates._release`), so its fate is not in the snapshot; the Gnoth-maker takes the
  template glom silently (DECISION D-062).
- **Choice:** a `TemplateCard` built from the `template` event (a `template-modified`
  just before it records `modified_from`), given its `check` and `passed` by
  `template-check` (the printed line, or `check: passed`/`failed` when nothing is
  printed, DECISION D-027), its state by `template-accepted` and `template-abandoned`
  (with the reason and the task), every `template-filtered` term as `term 3 (3) fits` or
  `does not fit`, and the `blast`'s gloms; a `blasted` event forgets it. The snapshot's
  template, when present, refreshes the four fields; the Gnoth-maker's first cap sets the
  glom to `nil`, as `show` prints then [M86 pp. 167-168]. `template_lines` and
  `fate_text` are pure functions; the fields text equals `Template.fields_text()` at
  every event of the runs tested.
- **Reason:** the template's history (the check, the filtering, a modification) is what
  the pane is for and only the events have it; the snapshot keeps the four fields honest
  while the template lives.
- **Alternatives:** the snapshot's template only (no fate once gone, no check line).

### G-033: The hypothesis model is the events plus the snapshot's box tree: the form, source, validity and candidates from the events, the box with the run's own names from the snapshot taken at the event, a fresh tree with provisional names in between
- **Sources:** PANES.md P4 is fed by `hypothesis` (form, source), `filtered`, `refuted`,
  `guess`, `candidate`, and by the snapshot's `hypothesis` (form, validity, covered, last
  predicted, refuted) and its box (the structure text and the per-box fields, "the real
  names, `box15`, `pstruc15`, ..."). ENGINE-API.md section 2 lists the validity and the box
  tree among what the events do not say, and rule 3 says predictions come from a fresh
  `Hypothesis(form)`. The validity's rule is DECISION D-032 ("a number that grows as new,
  correctly-predicted terms are encountered" [M86 p. 81]; 0 at a refutation, `set-out --
  validity: 0` [M86 p. 168]); a refused guess sets it to 0 too (DECISION D-053
  [M86 pp. 175-176]), and a candidate starts at 0 (DECISION D-034). The source is tracked
  from the events (G-006). The candidate's postdiction is the `candidate` event itself:
  the engine makes one only when it postdicts [M86 p. 125] (DECISION D-034).
- **Choice:** `HypothesisModel` keeps the form, its source (and the class a candidate was
  cast under), the validity, the refutation `(n, term, predicted)`, the terms, the
  threshold (`guess_validity` from the `start` config), the guess and its answer, the
  last freeze-dried form, and the casts of the episode since the last refutation
  (`Candidate`: the casts, the generalised form, `postdicts` True at the `candidate`
  event, False at a `cast-failed` or a cast with no form, None while pending; the terms
  known when cast), all from the events; the validity follows D-032, D-034 and D-053,
  the refused guess's zero applied at the next event because the engine zeroes it after
  publishing the answer. `update(snapshot)` takes a snapshot new to the model, which, fed
  in order, was taken at the event just given (the socratoplasm model's rule, G-030):
  when its hypothesis is the form in force it gives the box tree's records with the run's
  own names and confirms the validity (`corrections` counts a disagreement; the tests
  require zero and get it on the three appendix runs at every snapshot). Between a new
  form and the next snapshot the records come from a fresh `BoxTree(parse(form))`, with
  provisional names (`box1`, ...), and the pane says so. `tree_records` in
  `models/snapshot.py` reads any `BoxTree` into records (the run's own, a fresh one, the
  private box).
- **Reason:** the validity and the box state change at task boundaries and the events
  carry enough to follow the validity exactly; the box's real names and values exist only
  in the run's box, which the snapshot reads and nothing else may touch. A snapshot older
  than the latest validity event must not override the events, hence the new-object rule.
- **Alternatives:** the snapshot's validity only (stale for the events since it, as the
  stats pane shows it); no box tree between snapshots (a blank at the very event the
  hypothesis is announced).

### G-034: The form is drawn as Figures 1-2's ovals: a leaf per column, a parent over its children, a shared place joined to its structure; nodes map to boxes by creation order; an oval's terms by provenance over a fresh box
- **Sources:** PANES.md P4: "the primitive's name in an oval, argument lobes, nested forms
  as child ovals, and the first hits printed under it, `(S-group 2 3) --> (2 3 4)`
  [M86 pp. 50-53]"; "Click an oval to highlight the terms it generates (on the fresh box,
  by hit ranges)". Figure 1 draws each primitive as an oval with its arguments in circles
  below and the terms it gives beside it; Figure 2 draws a shared structure once, with
  lines from each place that shares it [M86 pp. 16-17]. The figures have no box names;
  Figure 3 numbers the boxes in creation order [M86 p. 83] (DECISION D-056).
- **Choice:** `form_tree` walks the form depth-first in the order `BoxTree` creates
  boxes (a structure's box before its arguments; a group's start before its length; a
  list type's elements in order; a `Shared` a box of its own; a Constant's integer value
  no box), so the i-th node with a box is the i-th record of the box tree, and the pane
  shows each oval's real box name in its tooltip. The layout is a pure function: leaves
  take successive columns `X_STEP` apart, a parent stands over the middle of its
  children, a level is `Y_STEP` down. A form is an oval with its type's name, an integer a
  circle, a shared place a small empty circle joined by a dashed line to the structure it
  shares (Figure 2 draws the structure once below its users; the dashed line keeps the
  tree a tree). Beside the top oval the first `TREE_HITS` values of a fresh box; each
  nested form's node carries its own first value for the tooltip, empty when it cannot
  stand alone (it shares a sibling). The "argument lobes" of the ovals are not drawn.
  `node_ranges` gives the positions each node's structure fills over `RANGE_HITS` hits of
  a fresh box of the whole form: the root every term of a hit; an element of a Tuple or
  Y-group the slice its value fills (the values read off the fresh tree's boxes after
  the hit); an element of a Cycle the whole hit on the turns it is fired (the
  printstructure's `turn`); a parameter (a start or a length) no term, since it is a
  value, not a term. A click toggles the oval and the predictions table outlines those
  positions.
- **Reason:** the figures' conventions, with the one addition (the box name) that ties the
  drawing to Figure 3; provenance over a fresh box is the only way to know which terms
  a sub-structure gave without instrumenting the engine.
- **Alternatives:** Figure 2's layout for shared structures (the shared node drawn once,
  below and between its users: a graph, not a tree); the lobes of Figure 1's ovals; no
  click ranges.

### G-035: The predictions table looks `LOOKAHEAD` terms ahead on a fresh box; the validity bar's scale; the box tree is a toggle in the model, off by default; the private box; one `LineList` for candidates, records and hits
- **Sources:** PANES.md P4: "a two-row table, the predicted terms above (`list_terms(n +
  k)` of a fresh hypothesis) and the terms entered below. Matches are green and the
  refuting term red. The predicted term at the refutation is shown in its own cell:
  `HypothesisRefuted.predicted`"; "Validity as a number and a small bar ... A mark shows
  the guess threshold (`Config.guess_validity`)"; "The box tree, hidden by default and
  shown with a toggle"; "'Hit' steps a *private* fresh box one hit, as `show-box` did
  [M86 p. 161], to explore the form. This never touches the run"; "Hover a box record
  for its fields". ENGINE-API.md rule 3: a fresh `Hypothesis(form)`, never the run's box.
  The cytoplasm keeps its layer toggles in the model so that its layout stays pure (G-026).
- **Choice:** `predictions` gives one `PredictionCell` per position up to `LOOKAHEAD` = 3
  beyond the terms, from `Hypothesis(parse(form)).list_terms` on a fresh object: `agrees`
  (green), `differs`, `refuting` (red, the prediction the `refuted` event carried, until a
  new hypothesis), `beyond` (dashed) or `unknown`. The bar's fill is the validity over
  the larger of the threshold and itself, and the threshold's mark the threshold over the
  same, so the mark always sits inside the bar and reaching it fills the bar to it.
  `show_box` is a flag of the model, off by default, set by the pane's checkable
  "Box tree" button; `pane_lines` is pure over it. The private box is a `PrivateBox`
  (`build_box` of a text in any of the three spellings, with bindings; `hit` as
  `show-box`; `reset`; its own records): "Hit" builds it from the box's text, or the form
  in force when the box is empty, and hits it; "Reset" starts it over; it survives seeks
  and is forgotten at `start`; a text that is not a form is refused in the box's
  tooltip. The pane's list is one `LineList` (G-028) of `HLine`s: the guess and its
  answer, the last freeze-dried form, the episode's casts (green when they postdict, red
  when not, grey while pending), the box tree's records (a line per Box and per Pstruc
  in Figure 3's field order, every field in the tooltip, grey while provisional), and the
  private box's hits with its records when the box tree is shown. `paints` is the oval
  tree's count.
- **Reason:** the table needs a few terms of continuation to show what the hypothesis
  expects next, and three is the guess's minimum block (DECISION D-033); the bar must
  show the threshold whatever the validity; the toggle in the model keeps the lines
  pure and testable; one list keeps the pane to one scrolling region.
- **Alternatives:** `k` equal to the guess block's length; a bar on a fixed scale; a
  separate widget per section; a dialog for the private box.

### G-036: The platoplasm network is the GUI's own drawing, after the Numbo GUI's Pnet view: one column per category from the strictest on the left, a box per type with its format and counts, the s-links as arrows offset in pairs with width and label by slipperiness, the ideal atoms as a strip below; a pure `layout()`
- **Sources:** the thesis describes the platoplasm in prose: it "houses the ideal types"
  [M86 p. 48], the ideal atoms with their glints [M86 pp. 48-49], the eight non-atomic
  types in "a hierarchy of categories" [M86 p. 54], "from the strictest down"
  [M86 pp. 54-55, 59], the s-links that "have direction and 'slipperiness'" [M86 p. 90],
  "a number between 0 (non-slippery) and 1 (perfectly slippery)" [M86 p. 90]. **There is
  no figure of this network in the thesis or in FCCA** (PANES.md P5 "Why"); PANES.md P5
  asks for nodes "arranged by category, from the strictest down", "directed s-links drawn
  as arrows, with thickness and label giving the slipperiness", and the atoms as "a strip
  of small value nodes under the network, each with its glint count"; the pane must be
  labelled "GUI design, not Meredith's" (ANSWERS.md 4). The Numbo GUI's Pnet view
  (`pnet_view.py`, `pnet_model.py`) is the precedent: a fixed arrangement, pure layout
  and style functions, the widget only paints. Item 10 says "record the layout as a
  G-decision".
- **Choice:** `layout(model)` in `models/platoplasm.py` is a pure function giving a
  `Layout` of shapes, and `NetworkWidget` paints it. Four columns, one per category in
  `CATEGORIES` order (repeater, generate, process, fence), a heading over each in the
  category's colour; the column's types stacked in `TYPE_TABLE`'s order, the thesis's
  [M86 pp. 50-55]. A type is a `NODE_W` x `NODE_H` box with its name, its format and
  "n m / k a", the counts of its manifestations and actualizations; a strict class is
  marked, the reigning class is filled gold, the selected one bordered orange, and during
  a check the best pretender bordered orange. The s-links are arrows from box edge to box
  edge; a two-way pair is offset `LINK_OFFSET` to the right of its direction of travel so
  the two never overlap; the stroke width is `link_width`, 0.6 to 3.0 by slipperiness; the
  label at `LABEL_AT` of the way is the slipperiness as the thesis writes it (`0.4`) and,
  while a check is in force, the pressure as p. 104's product (`0.4 * 10.0 = 4.0`), the
  links out of the checked class lit and the best one orange. The ideal atoms met so far
  are a strip of `ATOM_W` x `ATOM_H` cells under the network, sorted by value, each with
  its glint count. The paint order is headings, arrows, tags, boxes, atoms, crown. The
  tooltips (`node_tooltip`, `link_tooltip`, `atom_tooltip`) and the pane's label and
  empty-space tooltip say "GUI design, not Meredith's" (ANSWERS.md 4) with the pages.
- **Reason:** no source draws the network, so the layout is a GUI choice and is labelled
  as such; one column per category makes the hierarchy readable at a glance and puts the
  strictest classes, which never slip, on the left; a pure layout keeps the geometry
  testable without Qt and lets the hit tests (`node_at`, `link_at`, `atom_at`) read the
  same shapes the widget painted, as the Numbo GUI's Pnet view does.
- **Alternatives:** a force-directed or circular arrangement; a single row of eight;
  arrows drawn centre to centre; a colour scale for slipperiness instead of width; the
  atoms as nodes inside the network.

### G-037: The platoplasm model is the snapshot's tables and lists plus what the events since it announce; the reigning class, the threshold and the episode's check, change and scout from their events; the crown travels the s-link over `CROWN_MOVE` events
- **Sources:** PANES.md P5 "Fed by": static from `RunStarted.config` (`s_links`,
  `bond_values`, `strict_classes`) and `TYPE_TABLE`; dynamic from `dubbed`, `gnoth`,
  `freeze-dried`, `slip-check`, `class-changed`, `slip-scouted`, `reformulated` and
  `bonds-assessed`; the snapshot's `reigning`, `threshold` and the counts. ENGINE-API.md:
  a snapshot is taken at every task boundary and is the authority. The thesis: each type
  keeps the "lists each Platonic concept maintains of its manifestations and
  actualizations" [M86 p. 90]; the reigning class is "the Platonic class at the highest
  level of the (former) hypothesis" [M86 p. 99]; a change moves the gnoths to the new
  class [M86 pp. 174-175]; `Changing to a new reigning class. Countup --> S-group`
  [M86 p. 174]; the Slip-scout [M86 p. 126]; a Gnoth-operator that acted lowers the
  threshold by one [M86 p. 124]. The stats model is fed by `on_event` then `update`
  (G-016); the socratoplasm model is the snapshot's gnoths plus the events since it
  (G-030); the hypothesis model takes a snapshot new to it as taken at the event just
  given (G-033); animation runs by event counts, not time (G-021).
- **Choice:** `PlatoplasmModel` is fed as the stats model is: `on_event` on every event,
  then `update` with the snapshot in force. The tables (strictness, s-links, pulls,
  pushes) come from the `start` event's config (DECISIONS D-038, D-039, D-040) and are
  confirmed by the snapshot's `platoplasm`. The lists are the snapshot's at every task
  boundary plus what the events since it announce: `dubbed` adds a manifestation,
  `gnoth` and `gnoth-filled` an actualization, `class-changed` moves every actualization
  to the new class, the removal events (`burst`, `dissolved`, `blast`, `blasted`,
  `gnoth-gone`, `gnoths-set`, `operation`) forget what they name (DECISION D-059),
  `glint` adds an atom's manifestation. A snapshot new to the model was taken at the
  event just given (G-033's rule); one already taken is left alone; where it disagrees
  with the event-built lists, `corrections` counts it. The reigning class comes from
  `freeze-dried`, `class-changed` and `blasted`, the threshold from `reformulated` and
  `operated`, cleared where the engine clears it (DECISION D-046). The episode's check
  is the last `slip-check` event, the change the last `class-changed`, the scout the last
  `slip-scouted`; a `freeze-dried` begins a new episode and clears them; a check is
  shown on the links only while the class it checked is in the tables
  (`check_current`). The crown sits over the reigning class and, after a
  `class-changed`, travels the s-link from the old class to the new one over
  `CROWN_MOVE` = 6 events (`crown_progress`), by event counts, so a replay and a seek
  show the same picture.
- **Reason:** the snapshot is the authority but is only taken at task boundaries, and
  the thesis's transcript shows the dubbing, the gnoths and the slip as they happen;
  the events fill the gap and the snapshot corrects them, with the corrections counted
  so a test can show there are none on the three runs; the crown's travel by events is
  what keeps the view deterministic under `--smoke` and seeks (G-021).
- **Alternatives:** the snapshot alone, with the lists frozen between task boundaries;
  the events alone, trusting them never to disagree; a timer-driven crown; keeping every
  check of the episode rather than the last.

### G-038: The pane's list is one `LineList` of the reigning class, the last change, the check as the transcript prints it and the reigning class's bonds; a click on a type selects its manifestations and actualizations; the "Links" toggle draws Figure 2's links as tags, off by default; tooltips carry the tables
- **Sources:** PANES.md P5: "the reigning class with a crown and highlight [M86 p. 99].
  The threshold is shown beside it [M86 p. 105]"; "each node's pull and push bonds ... as
  a small table in the node's tooltip and in a side table for the reigning class"; "the
  slip-check, as it happens: the reigning class's staying pressure (`stayval`), each
  neighbour's pressure along its s-link, the best one, and the result, stay or slip
  [M86 pp. 103-105]. Example: `slip-check: stayval: 15.0` / `best: (Y-group 4.0)`
  [M86 p. 176]"; "live links to the cytoplasm (toggle): manifestation and actualization
  lines from the node to the dubbed glom and the gnoth, as Ch. 2 Fig. 2 draws them
  [M86 p. 45]"; interactions: "Click a node to list its manifestations and
  actualizations and select them in P2/P3", "Hover an s-link for its slipperiness and,
  during a check, its pressure term". PANES.md section 3: the optional layers are off by
  default. The taskrack's and socratoplasm's lists are one `LineList` (G-028); the
  shared selection (G-025).
- **Choice:** `pane_lines(model)` gives `PLine`s painted by `PlatoplasmList`, a
  `LineList`: the reigning class and the threshold (or "no reigning class yet", grey),
  the last class change (`Countup --> S-group`, in gold), the check as the transcript
  prints it (`check_lines`: `slip-check: stayval: 15.0`, one indented line per neighbour
  with its pressure and its product term, the best in orange, `best: (Y-group 4.0)` or
  `best: nil`, the outcome; the Slip-scout's factor when the check was its), the scout's
  line, and the reigning class's pull and push bonds (`bond_lines`); every line's
  tooltip cites its pages. The reigning class and the threshold are repeated in a bold
  label over the pane. A left click on a type (or on a list line that names one) puts
  its manifestations and actualizations into the shared selection as `select("class",
  name, members)`, so the cytoplasm and the socratoplasm show them; a second click, or a
  click on empty space, clears. The "Links" button is checkable and off by default; on,
  the layout adds `_tag_shapes`, each type's manifestations and actualizations as tags
  beside it joined by a line, the first few per node and "+n" for the rest, as the
  pane's reading of Figure 2's level-spanning links, rather than lines drawn across the
  window into the cytoplasm and socratoplasm docks. The tooltips carry the tables: a
  type's category, format, strictness, bonds and s-links; an s-link's slipperiness and,
  during a check, its pressure term; an atom's glints.
- **Reason:** the transcript's lines are what the thesis shows of the check, so the list
  repeats them verbatim; one list keeps the pane to one scrolling region, as the other
  docks do; the shared selection is how the panes point at the same structures; tags
  beside the type keep the links layer inside the pane, since Qt docks cannot share a
  canvas and a line across docks would break when one is floated or hidden; off by
  default keeps the network readable.
- **Alternatives:** a table widget for the bonds; the check in the console only; lines
  drawn over the whole window into the other docks; the links layer on by default; a
  selection that does not clear on a second click.

### G-039: The storyboard is built from the events alone: one episode per refutation, each event on the one card its kind names, cards in event order; the operation pictures are read off a socratoplasm model of the pane's own, with the names under the groups as pp. 110 and 120 write them
- **Sources:** PANES.md P7: "Each refutation opens an *episode*, drawn as a vertical list
  of cards in order", the ten cards with their pages, and "The 'after' side is the next
  snapshot"; TASK.md item 11: "every reformulation event of the three runs lands on
  exactly one card" and "gnoth operations as before/after bracket pictures with glom
  names, in the style of pp. 110-119, with 'operations in series' [M86 p. 120]". The
  thesis's storyboards write a gnoth as its name, a bracket of its top-level groups and
  the glom names under them, `gnoth2: [ (1 1) (2 2) ]` over `glom3 glom5` [M86 p. 110],
  and p. 120 stacks the parses in one column per gnoth, with `[] (disappears)` and
  `(emerges)` [M86 p. 120]. Every step of stage two publishes an event (DECISION D-055);
  the socratoplasm model's names, caps and ranges are exact at every gnoth event
  (G-030).
- **Choice:** `ReformulationModel` keeps a list of `Episode`s, each a list of `Card`s.
  Only a `refuted` event opens an episode (a `freeze-dried` with the last episode closed
  does too, for safety; any other event with no episode at all opens one "begun without
  a refutation"). Each event kind has one handler and lands on one card, whose
  `indices` record it: `refuted` the refutation; `freeze-dried` the freeze-dried;
  `gnoth-filled` and `gnoths-set` one gnoths-set card; `slip-check`, `class-changed` and
  `slip-scouted` one slip-check card (a check opens one; a change or a scout joins the
  open one); `move-found` and `reformulated` one Reformulator card per round;
  `operation-glom`, `gnoth-gone`, `gnoth-uncapped`, `gnoth-capped` and `operation` one
  operation card per operation, with `operated` joining the last one (a Capper's recap
  that comes after the operation closed, the slow recap of p. 113, gets a card of its
  own, `(CAPPER gnoth2)`); `bonds-assessed` the Bond-assessor; `casts` the casts;
  `candidate`, `cast-failed` and `blasted` the outcome cards. A candidate or a blast
  closes the episode; a failure leaves it open for the Slip-scout and the next round.
  Cards stay in event order, so the rounds repeat Reformulator, operation,
  Bond-assessor as the engine runs them, and a Bond-assessor drawn before the
  Gnoth-operator precedes its operation card. The pictures: the model owns a
  `SocratoplasmModel` fed the same events and snapshots; `picture()` reads its live
  gnoths in parse order, each top-level member as a `Group` of its print value
  (`member_text`, added to the socratoplasm model) and its name; an operation card's
  `before` is taken just before its first event is applied, its `after` at the
  `operation` event, with the gnoths that vanished marked `gone` in their old places and
  the new ones `emerged`. `picture_lines` writes two lines per gnoth with each name
  aligned under its group; `series_table` gives p. 120's columns and rows from the
  episode's operation cards. The catchall is left out of the pictures, as `show` leaves
  it out (DECISION D-030).
- **Reason:** the events carry everything the cards print, so the cards never wait for
  a task boundary; one handler per kind makes "exactly one card" a property of the code,
  which the tests check on five runs; reading the pictures off the socratoplasm model
  reuses the one place that already reconstructs the parse exactly, rather than a second
  reconstruction; p. 110's two-line form fits a list of lines, where a side-by-side
  figure would not.
- **Alternatives:** the after picture from the next snapshot only (PANES.md's words), a
  task boundary later and wrong for a PROGRAM's middle steps; one card per round
  grouping the Reformulator, operator and assessor; a graphics scene for the pictures;
  episodes opened by any stage-two event.

### G-040: The freeze-dried list, the reigning class and the threshold come from the snapshot, with the events between; the threshold plot keeps one point per change of value, read once the Gnoth-setter has cleared the old threshold; the Bond-assessor's given threshold is not plotted
- **Sources:** PANES.md P7: "Beside the storyboard, **the freeze-dried list** (all old
  hypotheses, newest first, with terms explained) and a **threshold plot** (threshold
  against clock within the episode)"; "Snapshot: `gnoths` ..., `freeze_dried`,
  `threshold`, `cast_failures`". The threshold is "either 1 more than the existing
  threshold value or, if none exists, 80% of the strongest bond" [M86 p. 105]
  (DECISION D-046); the Gnoth-setter and a slip clear it; a Gnoth-operator that acts
  lowers it by 1 (DECISION D-046); the Bond-assessor carries the threshold it was given,
  `(S-group 8.0)` [M86 p. 174], not the one in force after the operator acted.
- **Choice:** `update(snapshot)` takes the snapshot's `reformulation` section whole: the
  freeze-dried list (as `(form, terms explained)` pairs), the reigning class and the
  threshold; between snapshots the `freeze-dried`, `class-changed`, `reformulated`,
  `operated` and `blasted` events set the same fields. The threshold's course is a list
  of `(clock, value)` points per episode, from the `reformulated` and `operated` events
  and from every new snapshot; a point is kept only when the value changes, and the
  clock of the last reading is `clock_end`, so the plot draws a step line to it. Readings
  count from the episode's `freeze-dried` card on (`armed`): before it the snapshot still
  carries the previous episode's threshold, which the Gnoth-setter is about to clear.
  The `bonds-assessed` event's threshold is printed on its card and not plotted. The
  freeze-dried list is shown newest first, `(S-group 1 2) (4 terms)` (DECISION D-035).
- **Reason:** the list and the threshold are the engine's own state, so the snapshot is
  the authority and the events only bridge the gap to the next task boundary (the rule
  of G-016 and G-030); one point per change keeps the plot small on a long run; the
  stale reading would draw the old episode's last value at the start of the new one; the
  assessor's number is what it was given, and plotting it would show a rise the world
  did not make.
- **Alternatives:** a point at every snapshot; the plot over the whole run rather than
  the episode; the list oldest first, as the engine keeps it.

### G-041: One `LineList` for the storyboard: episodes fold (the newest open), a click on a card seeks, a hovered card selects what it names and leaving restores the selection, "In series" is a toggle off by default; the pane raises itself at the run's first refutation, live only
- **Sources:** PANES.md P7 interactions: "Click a card to seek the timeline to it",
  "Hover an operation to highlight the gnoths in P3 and the gloms in P2", "Episodes
  collapse; the newest is open"; its heading, "tabbed with P4, raised at the first
  refutation"; "A column view, 'operations in series', stacks the successive parses
  [M86 p. 120]". The taskrack's, socratoplasm's, hypothesis's and platoplasm's lists are
  one `LineList` (G-028); the shared selection (G-025); the socratoplasm's hover lights
  one arc through the cytoplasm's `highlight` (G-031); seeking is refused while a run is
  live (Numbo GUI).
- **Choice:** `pane_lines(model)` gives `RLine`s painted by `StoryList`: an episode's
  title on a blue band with a fold mark (folded episodes show the title and the outcome
  on one line), a card's heading on a grey band, the pictures and the forms in
  monospace, notes grey, good lines green, bad ones red. Every episode but the newest is
  folded when a new one opens; a click on a title folds or unfolds it. A click on a card
  emits `card_clicked(index)` with the index of the event that opened the card, and the
  window seeks when no run is live, as it does from the console (G-020). Hovering any
  card that names gnoths or gloms (gnoths set, Reformulator, operation, casts, blast)
  puts the names into the shared selection as `select("operation", first, names)`, so
  the socratoplasm outlines the gnoths and the cytoplasm the gloms; leaving the card
  restores the selection there was before the hover (the window keeps it in
  `_hover_saved`). The "In series" button is checkable and off by default; on, each
  unfolded episode ends with p. 120's table as monospace lines. The dock raises itself
  when the run's first `refuted` event is delivered live (`_on_delivered`), not when a
  seek passes it, and nothing lowers it. The label over the pane gives the episode count,
  the reigning class and the threshold.
- **Reason:** one list keeps the pane to one scrolling region, as the other docks do;
  the hover goes through the selection because the cytoplasm's `highlight` takes one
  glint or arc and the socratoplasm has no highlight, while both already draw the
  selection; restoring the selection keeps a hover from undoing a click made elsewhere;
  raising on live delivery only is what "raises itself at the first refutation" means for
  a watcher, and raising during a seek would flip the tabs under the user's pointer;
  the series table is long, so it is a toggle, as the box tree is (G-035).
- **Alternatives:** a tree widget with collapsible episodes; the series as a separate
  tab or a table widget; a hover that only tints the cards; raising at every refutation,
  or on a seek too; the toggle saved in QSettings.

## The timeline and the event log (item 12)

### G-042: The timeline's model scans the history once per event for its markers (terms, guesses with their verdict, refutations, slips, blasts) and the parse at each term boundary from the prompt's snapshot; the position text
- **Sources:** PANES.md P10: "A slider over the whole history, with term ticks (`term`
  events), guess markers (green `ok`, red `no`), refutation markers, slip markers and
  blast markers", and "A parse history strip under the slider: the parse at each term
  boundary", Hofstadter's Figure 1-1 [FCCA p. 41] and the mountain chain solved by
  successive bracketing [FCCA pp. 64-65]; "Fed by. The history: events plus the indices
  of the snapshots." The Numbo GUI's `TimelineBar` has no markers and no strip; its
  label says `event N of M, iteration k`. The tap always takes a snapshot at a `prompt`
  (G-003). The thesis prints the parse with `show-parse` [M86 p. 174] and has no
  time-lapse figure.
- **Choice:** `models/timeline.py`'s `TimelineModel(history)` keeps `markers` and
  `parses`, scanning each event of the history once (`sync`, called by the bar at every
  `set_position`; O(new events)). A `term` event is a tick labelled with its number; a
  `guess` event a marker whose `verdict` is None until the `answered` event, which
  turns the last unanswered guess marker `ok` or `no` [M86 pp. 166, 175]; a `refuted`
  event a refutation; a `class-changed` event a slip (either reason, `strict` or `slip`
  [M86 pp. 99, 174]); a `blast` (the Template-resolver's [M86 p. 75]) or a `blasted`
  (the reformulation's [M86 p. 92]) event a blast. The parse strip has one entry per
  `prompt` event and one at `ended`, with the parse of `history.state_at(index)`, which
  is the snapshot taken at that very event, so it is exact. A `start` clears. The
  position text is the window's of item 02, `event N of M: kind   term t   clock c`,
  with the event number counted from 1 as the console's lines are read.
- **Reason:** scanning the history, rather than observing the run, lets the bar be
  rebuilt from any history (a replay's, item 13) and keeps it out of the view protocol,
  as Numbo's bar is; one scan per event keeps a long run cheap. A slip marker at every
  class change, not only at `reason == "slip"`, because the storyboard shows both and
  the user wants to see where the reigning class moved [M86 p. 174].
- **Alternatives:** markers computed by a pure function over the whole history at each
  draw (O(n) per event: too slow on a Blackburn run); the guess marker placed at the
  `answered` event; only `slip`-reason changes as slips; the parse read from the
  socratoplasm model.

### G-043: The timeline is Numbo's bar with two strips under the slider, the markers and the parses, placed by a pure function; a click on a marker, a cell or the strip seeks; the go-to box reads `term`, `clock` or `event`; the controls are disabled while a run is live
- **Sources:** PANES.md P10: "Back and forward buttons; a go-to box ("clock 812",
  "term 5", "event 402")"; "During a live run the slider grows with the history but
  cannot be dragged"; the Numbo GUI's `TimelineBar` (a slider with tracking off, a
  spin box and "Go to iteration", `seek_requested` and `iteration_requested`, the
  window refusing while a run is live, `set_scrubbable`). The drawing of the markers
  has no figure in the sources.
- **Choice:** `timeline.py`'s `TimelineBar(history)` is Numbo's bar (the slider, the
  two buttons, the fixed-width label) with a `QLineEdit` go-to box and a "Go" button in
  place of the spin box, and under the slider a `MarkerStrip` and a `ParseStrip`, laid
  out in a grid so the strips span the slider's width. `marker_x(index, count, width)`
  places an event on the strip with the slider's handle inset, and `index_at` is its
  inverse; `parse_cells` tiles the parse strip from each boundary to the next.
  `marker_style` gives the shapes and colours: term ticks grey on the top row with their
  numbers, a guess a diamond (grey, then green `ok` or red `no`), a refutation an orange
  triangle, a slip a purple diamond, a blast a black bar; a magenta line marks the event
  shown. A click on the marker strip seeks to the marker under the mouse (within four
  pixels) or else to the nearest event; a click on a parse cell seeks to its boundary.
  The go-to box takes `term 5`, `clock 812`, `event 402` or a bare term number
  (`parse_goto`); `goto_index` finds the event (a term's `term` event, a clock's
  `chosen` event, or the event by number from 1), and the window's `jump_to` seeks or
  says what is missing. `set_scrubbable(False)` disables the slider, the buttons, the
  box and the strips' clicks while a run is live. Tooltips give the marker's text or the
  cell's parse with the event number.
- **Reason:** a line edit reads the three forms PANES.md gives in one box, where
  Numbo's spin box took one kind of target; the strips are plain widgets painted from
  the model so the toolbar stays one widget and repaints are cheap (a few hundred marks
  at most); the slider's own tick marks cannot be coloured or labelled. The colours
  follow the cytoplasm's conventions where they meet (green for accepted, red for
  refused or refuting, G-014, G-035).
- **Alternatives:** a `QSlider` subclass painting the marks on the groove; the parse
  strip as a tooltip on the slider; three go-to spin boxes; Numbo's spin box for terms
  alone; markers in the event log instead.

### G-044: The log's kinds are grouped by the sections of `events.py`; the history's `visible` list is guarded by a lock, because the tap appends on the worker and the filter rebuilds on the GUI thread
- **Sources:** PANES.md P9: "The rows can be filtered by kind and by group (run,
  taskrack, cytoplasm, templates, gnoths, filtering, reformulation)". The Numbo GUI's
  `EventLog` has `hidden`, `visible` and `set_hidden`, filled on the GUI thread by the
  window; here the tap fills the history on the worker (G-012). `seekwhence/events.py`
  lists the kinds in sections (DECISION D-055).
- **Choice:** `GROUPS` in `models/event_log.py` names the seven groups of PANES.md P9
  with their kinds, following the sections of `events.py` (`blast`, the
  Template-resolver's, under templates; `blasted`, the reformulation's, under
  reformulation); every kind is in exactly one group (a test checks). `EventLog.record`
  appends to `visible` and `set_hidden` rebuilds it under one `threading.Lock`, so a
  filter change while a run is live loses no event.
- **Reason:** the groups are the sections the engine's author drew; without the lock
  an event recorded between the rebuild's list comprehension and its assignment would
  never get a row (the worker runs free between an acknowledgment and its next
  publish, and a menu click lands there). The lock costs one acquire per event.
- **Alternatives:** refusing a filter change while a run is live; the view deriving
  the rows on the GUI thread from a count of events scanned; groups by the panes they
  feed.

### G-045: The event log is Numbo's `LogView` as a view of the protocol, with a clock column, group actions in the Kinds menu, a double-click or Enter to seek, and Save events (JSON lines) in the Session menu, built in item 12
- **Sources:** PANES.md P9: "one row per engine event: index, clock, kind, fields, as
  `event_record` gives them", "Double-click or Enter to seek; 'Save events as JSON
  lines' (the same format as `python3 -m seekwhence --json`)"; section 4 lists Save
  events under Session. The Numbo GUI's `log_view.py` (a virtualised `QAbstractScrollArea`
  painting only the rows in view, blitting on scroll, Follow, the Kinds menu with Show
  all and Hide all, `event_activated`), which the window drives from `_draw` with
  `appended`, `set_position` and `redraw`. Item 02's menu stub put Save events under
  item 13. The CLI's `JsonLines` writes `json.dumps(event_record(event),
  separators=(",", ":"))` per line (DECISION D-069).
- **Choice:** `log_view.py` is Numbo's module adopted, with `row_text(index, clock,
  event)` adding the clock of the task under way (`history.clock_at`, blank before the
  first task) between the index and the kind. `LogView` is a view of the window's
  protocol (`pane_id`, `events`, `on_event`, `set_position`, `redraw`, `paints`,
  G-011): `set_position` brings the scroll range up to the history (the tap has recorded
  the event already) and follows the row; `on_event` counts. The Kinds menu has one
  checkable action per group above the kinds; toggling a group checks or unchecks its
  kinds, and a group's action reads checked while any of its kinds shows. A double-click
  or Enter emits `event_activated`, which the window takes to `seek` when no run is
  live (`_seek_from_log`). Session > Save events (JSON lines)… (`save_events(path)`)
  writes every event of the history in the CLI's format; it is enabled once the history
  has events. The log's rows are painted at least once per event while on screen, and
  all but a few exactly once: a scroll whose range changes at the same time can flush
  the exposed strip apart (Numbo's own test asserts "at least once").
- **Reason:** the view protocol lets the window treat the log as every other dock
  (`add_view`), where Numbo calls it by hand; the clock column is what PANES.md asks
  and what the console's `doing task` lines count by [M86 p. 41]; Save events belongs
  with the log it serialises, so it comes in this item rather than item 13's session
  file. The JSON-lines test compares the saved file with the CLI's byte for byte.
- **Alternatives:** the window calling `appended`/`set_position` by hand as Numbo's
  does; groups as submenus; a Save button on the pane; waiting for item 13.


## Sessions and replay (item 13)

### G-046: The session file is Numbo's JSON lines with a run line; the events are `event_record`s compared as JSON; a `Form` in a task's arguments comes back as its text; a file is complete when its last event ends the run; Save writes the whole history with the lines typed and the answers given
- **Sources:** PANES.md section 5 gives the file's shape: Numbo's header and end line
  (`numbo/session.py`), with a run line between them holding the seed, the configuration
  as `[field, value]` pairs, the console inputs, the answers, the mode, the preset and the
  engine version, and one `event_record` per event. ANSWERS.md 5 says the configuration a
  run used is saved in its session file and restored on replay. TASK.md's contract (rule
  5) says the answers and console inputs are the session's inputs and are saved in the
  session file. ENGINE-API.md section 3: whatever an answer's source, it is input. Numbo's
  `decode_event` gives back typed events equal to the originals; Numbo's events hold only
  plain data. The engine's `TaskPosted` and `TaskChosen` carry the task's arguments, and a
  Template-applier's argument is a `Form` (`seekwhence.forms`), which `json_value` writes
  as the text the trace prints [M86 p. 167]. The engine publishes `ended` only when a
  guess is accepted (DECISION D-053) and `capped` at the whole-run cap (DECISION D-011); a
  run that ends on `bye` or on the end of its input publishes no ending event.
- **Choice:** `models/session_file.py` is Numbo's `session.py` with the run line added.
  `SessionRecorder` writes the header and the run line at once and the end line at an
  `ended` or `capped` event or on `close()`; `write_session` writes a history; `read_session`
  reads and checks the whole file into a `SessionFile` (Numbo's `load_session`) and raises
  `SessionError` with the path and line. The events are stored as `event_record` dicts and
  `decode_event` gives back a typed event whose record equals the stored one, lists made
  tuples; a `Form` inside a task's arguments comes back as its printed text, so the text
  every pane shows is the same, and the tests check `decode_event(record) == event` for
  every event whose arguments hold no `Form` and the records' equality for all. A replay is
  compared on records as JSON text (Numbo compares parsed JSON lines), never on typed
  events. `SessionFile.ended` is true when the last event is `ended` or `capped`;
  otherwise the run may have been saved part way, or have ended on `bye` or the end of its
  input, and the checker lets a re-run go on past the stored events (G-047). Session >
  Save writes the whole history, live or not, with the run line of the inputs the run was
  started from (`MainWindow.run_line`): a scripted run's lines and answers are the inputs'
  own; a run whose lines were typed stores every line typed (`ConsoleModel.inputs`, the
  session's inputs in order, G-019), and an interactive run stores the answers of its
  `answered` events, which the answerer wrote as given (G-008). The mode is `interactive`
  when the lines were typed or the guesses answered in the GUI, else `scripted`. The
  configuration goes as `World.config_pairs` records it in the `start` event and comes
  back through `dataclasses.replace(Config(), **edits)` (ANSWERS.md 5), with every pair
  checked against the field's default type.
- **Reason:** the record form is the engine's own JSON (DECISION D-069, `python3 -m
  seekwhence --json`), so a session file's event lines are the CLI's; comparing records
  as JSON is what Numbo does and is exact for everything the file can hold. Rebuilding a
  `Form` from its text would need the engine's parser on a string that might be a name;
  nothing in the GUI needs the object. Completeness from the last event keeps the end
  line as PANES.md gives it. Saving every line typed, read or not, makes the file the
  run's whole input: on replay the lines not yet read are read at the prompts that follow,
  as they would have been.
- **Alternatives:** an `outcome` field on the end line to say whether the run ended (PANES.md
  fixes the end line's two keys); storing only the lines the session had read (then a
  part-way file's re-run would end at `no-terms` where the live run went on); typed-event
  equality with `Form`s parsed back (`seekwhence.forms.parse` on arguments that start with
  a parenthesis).

### G-047: Open re-executes the run line through the window's own controller with a checker on the worker that stops the run at the first difference; a part-way file's re-run goes on; the stored events are then loaded without snapshots and the structure panes show what the events alone give; Play after the end re-executes; Close replay keeps the history
- **Sources:** PANES.md section 5: Open re-executes the run line through the controller
  and compares each event with the stored one, like Numbo's `replay_oracle_trace`, which
  raises `TraceMismatch`; same events, the replay shows everything, snapshots included; a
  difference, the stored events are shown instead in the log, the console and the
  timeline, and the structure panes are marked "not available for this file". The
  contract's rule 6: replay re-executes the engine from the file's seed, config and
  inputs through the same controller. Numbo's `open_file` starts an oracle replay paused
  and steps once to draw its start event; its `_start_replay` at the end of a playback
  starts it over; its `close_replay` keeps the history to scrub. Numbo's `_TraceChecker`
  treats a re-run that goes on past the trace as a difference ("truncated?"): Numbo's
  traces are whole runs. The run controller's gate raises `RunStopped` at its next wait
  after `stop()` (G-005).
- **Choice:** `open_session` reads the file (an error is a status line and no replay),
  turns its run line into scripted `RunInputs` (`run_inputs`: the file's lines and answers
  stand in for the user, so a replay needs no one at the console and an interactive
  session replays as a scripted one), and starts it through `start_session` paused, with a
  `ReplayChecker` subscribed to the controller for that run only, after the tap, then
  steps once. The checker runs on the worker, inside the engine's `Subject`: it compares
  the event's record with the stored one and, at the first difference, calls
  `controller.stop(wait=False)`, so the gate unwinds the run before that event is delivered
  or drawn (the tap has recorded it; it is replaced). A re-run past the end of a file whose
  last event ends the run is a difference; past the end of any other file it goes on,
  counted in `beyond`, and the status says the file was saved part way and how far the run
  went on. `finish` adds the last check, a re-run that ended before the file did, unless
  the user stopped it. On a difference the window warns (a `QMessageBox` and the status
  bar, naming the event, its line and the fields that differ), loads the stored events
  into the history with `EventLog.load` (no snapshots), tells the console the file's
  inputs, reloads the log and seeks to the first difference; the Stats pane's Run row says
  `stored events of <file> (differs at event N)` and its State `stored`. The log, the
  console and the timeline are built from the events alone, so they show the file whole;
  the pane models that read the snapshot in force get None at every event and show what
  the events since the start give (the sequence pane, the taskrack's shadow, the
  storyboard, the hypothesis's provisional tree are built from events; the cytoplasm, the
  socratoplasm and the platoplasm show their events' additions and no silent removal; a
  `show` command's output at an ask, rebuilt from the snapshot (G-017), prints the empty
  world). While a replay is open, Play, Step and Run to end drive the re-run; after it ends they
  re-execute it from the start, with a fresh checker. Close replay stops the run, forgets
  the file and the inputs chosen before are the next run's; the history stays, to scrub.
  Save during a replay writes the replayed history with the replay's run line, so the
  file written equals the one opened for a scripted run. The Qt-free `replay()` does the
  same through a `RunController` of its own, for the tests and the gate.
- **Reason:** re-executing is what PANES.md asks, and it gives a replay every snapshot a
  live run has, so every pane is exact; the comparison on the worker is Numbo's place for
  it (inside the trace writer) and stops the run at once, before anything wrong is drawn.
  A part-way file is allowed by PANES.md ("the file then has no `ended` event"), and the
  only way to tell a part-way file from a `bye` file is the events themselves, so the
  checker lets both go on: a `bye` file's re-run ends where the file does anyway. The
  stored events are all the window has when the engine no longer reproduces them, and the
  event-built panes still show something true of the run as recorded; marking the
  snapshot-reading panes one by one would add a state to every view for a case the tests
  have to construct by hand.
- **Alternatives:** playing the stored events back through `start_events` without
  re-executing (no snapshots, so the structure panes would be empty for every replay);
  comparing on the GUI thread at delivery (one event later, and already drawn); stopping a
  part-way file's re-run at the file's end (then the saved run could never be watched to
  its end); a `not available` banner painted by every snapshot-reading view.

### G-048: The presets are the three appendix runs, Custom, and every sequence of `seekwhence.sequences` fed as the bench feeds it (16 terms); three answering modes, the continuation judge through the `answerer` hook; a judged run's answers are stored in the session file as an interactive run's are
- **Sources:** PANES.md P11: the presets are the three appendix runs with her exact
  console inputs, commands included, and her answers [M86 pp. 164-179], the Blackburn
  dozen [M86 pp. 6-7, 145-147] and the other lists of `seekwhence.sequences`, each
  showing its citation and Meredith's reported result; the answering modes are
  interactive, scripted (`ConsoleAnswerer`'s rule, DECISIONS D-033, D-053) and the
  continuation judge (DECISION D-070). ENGINE-API.md rule 5: answers come through the
  `answerer` hook. The bench feeds a sequence's first terms one at a time, 16 by default
  (`bench.main`'s `--terms`), and judges every guess with `ContinuationAnswerer`
  (DECISION D-070). iterations.md item 14: "Custom" for free terms. Nothing says how many
  terms a preset feeds in the GUI, nor what a judged run's session file stores.
- **Choice:** `models/controls.py`'s `catalogue()` lists the three appendix runs, then
  `custom`, then every sequence of `sequences.CATALOGUE` in its groups, with `Sequence`'s
  `tag`, `reported`, `parsed` and `remarks`; the Presets menu has one submenu per group.
  A sequence preset feeds its first `SEQUENCE_TERMS` = 16 terms (`presets.sequence_inputs`),
  the bench's default. The answering mode is independent of the preset: `interactive`
  gives `RunInputs(lines=None, interactive=True)` (the terms typed in the sequence pane,
  G-015, the guesses answered in the panel, G-008), `scripted` the preset's lines and
  answers (none for a sequence or Custom, so the session judges against the terms fed),
  `judged` the preset's lines with `RunInputs.continuation`, a new field: the sequence's
  whole continuation, an appendix run's terms, or Custom's own terms. The window makes
  the bench's `ContinuationAnswerer` for it through the `answerer` hook, so the judge's
  answers are written after the prompt exactly as scripted ones are and the transcript
  and events of a judged run equal a scripted run given the same answers. A judged
  run's session file therefore stores the answers given (from the `answered` events, as
  an interactive run's are) with the mode `judged`, and replays as a scripted run.
- **Reason:** the catalogue is the engine's own list and the bench's feeding rule is the
  one `RESULTS.md` reports on, so a GUI run of a preset is the bench's run on that seed
  (`python3 -m seekwhence.bench dozen --names blackburn-2 --trace 1` shows the same
  transcript). Keeping the mode apart from the preset lets any sequence be typed by hand
  or judged. The `answerer` hook is the one entry the contract allows for answers.
- **Alternatives:** feeding the terms the source prints (`Sequence.shown`), which
  differs per sequence and from the bench; a "Terms to feed" field (item 15's
  configuration pane could carry it later); storing the continuation in the session
  file and re-judging on replay (the stored answers are simpler and replay without the
  catalogue).

### G-049: The controls pane is Numbo's `ControlsPanel`: its values are plain text in a Qt-free `ControlValues`, turned into `RunInputs` at every change (an invalid text refuses the start), shown back from any `RunInputs`, and saved under `controls/*`; the random seed comes from Python's `random`; the panel takes no events
- **Sources:** iterations.md item 14, "as Numbo's `controls.py`": the preset combo with
  Custom, the seed and the cap fields, the answering modes, trace verbose/terse, the
  seven buttons, the speed slider over Numbo's `DELAYS`, the inputs saved under
  `controls/*`, the Run menu's shortcuts. Numbo's panel holds widgets, parses its texts
  at `_start` (`inputs()`), shows input errors under the buttons, and the window saves
  each widget's text. PANES.md P11 adds "random seed". ANSWERS.md 7: follow Numbo.
  TASK.md: the GUI draws nothing from the engine's `Rng`.
- **Choice:** `controls.py`'s `ControlsPanel` is Numbo's with Seek-Whence's fields; the
  Qt-free `models/controls.py` holds `DELAYS`, the parsers (Numbo's `run_stats.py`), the
  `ControlValues` dataclass (every field as text, plus the slider's index), `run_inputs`
  (values to `RunInputs`, raising `InputError`), `values_of` (the reverse, for a preset
  action, a restored setting or the title) and the `to_settings`/`from_settings` round
  trip, which checks every value read and replaces a bad one by its default. The panel
  emits `inputs_changed` at every change (a preset chosen, an edit finished, a mode or
  trace switched, a random seed) and the window rebuilds `inputs` then, so `inputs` is
  always the panel's; an invalid text is shown in the panel at once and refuses the next
  start, as Numbo's error does. `set_inputs` shows any `RunInputs` back in the panel; an
  appendix run with interactive answers (item 02's tests) shows as the preset with the
  `interactive` mode, which stands for "terms typed, guesses answered" if re-derived. The
  "Random" button draws from Python's `random`, never the engine's. The panel is not a
  view of the window's protocol (it has no `on_event`), so the controls dock is the one
  dock `add_view` skips. The Run menu keeps item 02's shortcuts (G-013); the buttons take
  no keyboard focus so the arrows and Space stay the shortcuts' (Numbo's slider rule,
  extended to the buttons).
- **Reason:** Numbo's pattern (ANSWERS.md 7) with the one difference that the inputs are
  rebuilt at each change rather than at the start, because the window already keeps
  `inputs` as the next run's `RunInputs` (G-009) for the presets, the title and the
  settings; a Qt-free model is the package's rule and makes the round trip testable
  without Qt.
- **Alternatives:** parsing only at the start (Numbo's); a spin box for the seed (a text
  field takes any integer and shows what was restored); saving `RunInputs` as JSON in
  the settings (the texts are what the user typed, and bad ones are easy to replace).

### G-050: The configuration pane's model reads the fields' comments from `config.py` with `tokenize` and the decisions' headings from DECISIONS.md; the edits are a dict over `Config()`, every value conformed to its field's type, nested tables included, and saved as one JSON text under `config/edits`
- **Sources:** ANSWERS.md 5: the pane lists every `Config` field with its value, its
  source tag or decision number and its comment; the defaults are shown first; an edit
  applies to the next run, never the one in progress; the GUI builds the run's `Config`
  with `dataclasses.replace(Config(), **edits)`; each field is validated before it is
  accepted; the edits are remembered with QSettings as Numbo remembers its inputs.
  PANES.md P13: the tags and comments are read once with `tokenize` from `config.py`,
  the reading `test_conventions.py` does; the headings once from DECISIONS.md; hovering
  a decision number shows its heading. iterations.md item 15: validate each value against
  the field's type (ints, floats, bools, tuples and the nested tables) and reject invalid
  ones with a message. A run is its seed and its configuration (D-069).
  Nothing says how a table is typed, what a comment spanning several lines is, or how
  the edits are stored in the settings.
- **Choice:** `models/config_table.py`. `read_fields` walks `config.py` with `ast` and
  `tokenize`: a field's comment is every comment line between the previous field (or the
  class docstring) and the field, so comments of several lines (`bond_values` has eight)
  are whole; a `# ---- heading ----` line is the field's section instead; the tags and
  `D-NNN`s are pulled out of the joined text, the leading `DECISION` word dropped.
  `decision_titles` reads the `### D-NNN: title` headings. A value's kind is `bool`,
  `int`, `float` or `tuple`, from its default (`bool` tested first). `conform(value,
  default)` is the validator: a bool must be a bool, an int an int (not a bool), a float
  a number (an integer becomes a float), a table a tuple (or a list, as JSON gives it)
  whose every entry has the shape of the default's first entry, recursively; everything
  else raises `ConfigError` with a message. `parse_text` reads an integer, a decimal,
  `true`/`false` (`yes`/`no`, `1`/`0`), or, for a table, a Python literal through
  `ast.literal_eval`. `ConfigTable.edits` holds only values that differ from the default;
  `config()` is `dataclasses.replace(Config(), **edits)`. The settings hold the edits as
  one JSON text `[[name, value], ...]` under `config/edits`; `from_settings` checks every
  pair and drops a bad or unknown one.
- **Reason:** reading the comments is the reading the engine's own test does, so the
  source column is never empty (PANES.md); conforming the nested tables keeps a typo
  from reaching the engine as a wrong type (the engine's `platoplasm` and `taskrack` read
  the tables into dictionaries and would fail in the run, not at the edit); a literal is
  the shortest exact way to type a table, and `literal_eval` runs no code. One JSON text
  keeps the settings file to one key per concern, as `controls/*` is one per control.
- **Alternatives:** a comment of at most four lines (the engine test's window; too short
  for `bond_values`); JSON for the tables (the literal reads back what `repr` shows and
  what the thesis prints); one settings key per field (31 keys, most absent).

### G-051: The configuration pane is a table with a widget per kind and an editor under it; the controls' caps and trace are shown but edited in the controls pane; the edits are replaced into the next run's inputs at every change and marked in the title as the stats note; a replay shows the file's configuration read-only
- **Sources:** ANSWERS.md 5: a widget suited to each type; edited fields marked, with
  Reset per field and Reset all; the Stats dock and the window title show when a run
  uses a non-default configuration; the configuration a run used is saved in its
  session file and restored on replay. PANES.md P13: long values can be expanded; a
  filter by name; the edits apply to the next run only (the controls say so). PANES.md
  P11 and G-049: the caps (DECISION D-011) and the trace (DECISION D-055) are controls of
  the controls pane, replaced into the configuration at every change (`run_inputs(values,
  base_config)`). G-016: the stats pane's note is `default` or `edited: <fields>`,
  computed from the run's `start` event against `Config()`. G-047: a replay re-executes
  the file's run line, whose `Config` is the file's. Nothing says which pane owns the
  three fields both show, how the title marks the run, or what the pane shows during a
  replay.
- **Choice:** `config_view.py`. A `QTableWidget` with the columns Field, Value, Default,
  Source, Comment and a Reset button: a `QSpinBox` for an int, a `QDoubleSpinBox` (four
  decimals) for a float, a `QCheckBox` for a bool, a `QLineEdit` holding the literal for
  a table; under the table, a `QPlainTextEdit` with the selected field's value one entry
  per line and Apply (the expansion of a long value), beside Reset; Reset all beside the
  filter; an error label under the editor. Every edit goes through `ConfigTable.set_text`,
  so the widget and the editor agree and a refused text changes nothing. The three
  `CONTROLLED` fields (`max_tasks_per_term`, `max_tasks`, `verbose`) are rows like the
  others, with the next run's values and tinted when they differ from the default, but
  their widgets are disabled and say "set in the Controls pane": the controls pane owns
  them, so `ConfigTable.config(base)` keeps them from `base` and takes every other field
  from the defaults or the edits. The window keeps `controls.base_config` equal to
  `config_table.config()`, rebuilds `inputs` at every change of the edits (through the
  controls when they parse), and `set_inputs` (a preset, the interactive run, a restored
  setting) replaces the edits into the given inputs' configuration, so the next run is
  always the edits plus the controls, whichever way it was chosen. The title is
  `title_for(inputs)`: `Seek-Whence — <run> [edited: <fields>]` with the stats note's
  words, `default` showing nothing; so run 2's terse printout is marked `[edited:
  verbose]`, as its stats row already is. During a replay `set_replay(config)` shows the
  file's configuration (rows, tints and a read-only status line) with every widget
  disabled; Close replay shows the user's edits again, untouched. The pane is a view of
  the protocol: at `start` it notes the running configuration under the next run's, from
  `RunStarted.config`. Nothing is written to QSettings by `MainWindow(settings=None)`.
- **Reason:** one owner per field keeps the two panes from fighting over the caps and the
  trace, and the controls pane is where Meredith's printout modes and the bench's caps are
  chosen (G-048, G-049); the title and the stats pane agree because both use
  `config_note`; showing the file's configuration during a replay is "restored on
  replay" made visible without letting a click change a run the file fixes.
- **Alternatives:** editing the caps and the trace here and mirroring them into the
  controls (two editors of one value); marking the title only for edits made in this
  pane (then the title and the stats row would disagree on run 2); hiding the pane
  during a replay (the restored configuration would be invisible); a dialog for long
  values (dialogs are awkward offscreen and for the tests).

## The command line and the help (item 16), and the screenshot defects (ANSWERS.md 10)

### G-052: The built-in dock arrangement is made at the first `showEvent`, never before the window is laid out
- **Sources:** ANSWERS.md 10.1 and 10.2: a stray "Console" label over the Session menu
  and the "Console | Event log" tab bar twice, seen after run 3 at 1920x1080. PANES.md
  section 3 gives the arrangement; G-010 made it a function called in `__init__` and
  by Reset layout. Numbo GUI's `showEvent` sizes its docks only once the window is laid
  out, for the same kind of reason. Nothing says when to arrange.
- **Choice:** `_arrange_builtin` before the first show only sets each dock's visibility
  and marks the arrangement pending; `showEvent` makes it (`tabifyDockWidget`,
  `splitDockWidget`, `resizeDocks`) the first time the window is shown. A layout
  restored from the settings or the default file before the show cancels the pending
  arrangement, since it replaces the built-in. After the show, Reset layout and Reset to
  built-in layout arrange at once, as before.
- **Reason:** reproduced in a bare `QMainWindow` with labels: with two tab groups
  (console/log and hypothesis/reformulation) tabified before the window is shown, Qt
  6.11 keeps stale `QTabBar`s in use, one left at the origin with the text "Console"
  (over the menu bar) and one with the old geometry under the console (the second bar);
  with the arrangement made after the show every stale bar is parked off the window and
  only one bar per group is drawn. Removing and re-adding the docks, `restoreState(
  saveState())` and `layout().activate()` did not help; arranging once shown did. The
  shipped `default_layout.json` was saved from the old arrangement and still restores
  cleanly after the show, so it is kept.
- **Alternatives:** hide the stale bars by hand after every arrangement (fragile: Qt
  owns them); a single tab group (loses PANES.md's layout); show the window hidden
  off-screen before arranging (what the deferred arrangement does without the trick).

### G-053: The pseudo-gloms are shown by default, in grey
- **Sources:** ANSWERS.md 10.4, the author's choice: show pseudo-gloms by default,
  drawn in grey, a light grey outline and fill clearly lighter than real gloms, keeping
  the toggle; the legend says "pseudo-glom (grey)". It overrides PANES.md section 3's
  "hidden by default". G-024 drew them dotted with no fill; G-026 put the layer off.
- **Choice:** `LAYERS["top-down"]` is True; `hull_style` gives a pseudo-glom the fill
  `PSEUDO_FILL` (#fafafa) and the border `PSEUDO_BORDER` (#9a9a9a), dotted as before,
  where a real glom has the pale blue fill and slate border; the legend reads
  "pseudo-glom (grey)"; the action's tooltip says they are shown by default. The tests
  of items 05 and 06 that pinned the old default now switch the layer off where the
  bare picture is wanted.
- **Reason:** the author's; and with it `show-parse`'s groupings are visible at every
  moment, which is what the cytoplasm is for [M86 p. 102].
- **Alternatives:** PANES.md's hidden layer (withdrawn by the author).

### G-054: A run started any way shows its inputs in the controls, the title and the stats
- **Sources:** ANSWERS.md 10.3: whichever way a run is started (the Presets menu, the
  controls, `start_session`, a replayed session file), the preset shown, the window
  title and the Stats Run line must agree with the inputs actually used, or show
  `custom`. G-049 rebuilt the inputs from the panel at every change; `start_session`
  took any `RunInputs` without telling the panel.
- **Choice:** `start_session` calls `_show_started(inputs)`: the controls show the
  inputs (`values_of`: a preset of the catalogue, else `custom`; G-049), the stats pane
  takes the title, and outside a replay the inputs become the window's `inputs` exactly
  as used (the configuration pane's edits are not reapplied, so the next Play repeats
  the run) and the title is `title_for(inputs)`. During a replay the title is
  `Seek-Whence — replaying <file>: <preset>, seed N` and the window's own inputs are kept aside
  (`_live_inputs`) for Close replay, which shows them again through `set_inputs`. The
  command line goes through `apply_arguments` and `set_inputs`, so it agrees too.
- **Reason:** one method on the one path every run takes, so the three displays cannot
  disagree, whichever entry point was used.
- **Alternatives:** have every caller update the panel (the defect came from one that did
  not).

### G-055: The command line is Numbo's with the CLI's names; `--smoke` prints the summary line; the help page is the user guide's table
- **Sources:** iterations.md item 16: `--preset NAME | --terms ...`, `--seed N`,
  `--answers ...`, `--max-steps N` (the CLI's name), `--play`, `--smoke`; the inputs
  given win over the remembered ones, `--smoke` uses the defaults for the others and no
  settings; the smoke run prints `Session.summary()` and exits 0 for an accepted guess,
  1 otherwise, 2 for a usage error; Help > "What am I looking at?" generated from the
  pane-to-pages table in `docs/gui/panes.md` with P5 and P6 marked GUI design
  (ANSWERS.md 4); Help > About. Numbo GUI's `app.py` sets the controls' widgets from the
  arguments and prints two lines. The CLI (DECISION D-069) names `--max-steps` the cap on
  the run's tasks. Item 17 writes the rest of the guide.
- **Choice:** `apply_arguments(window, args)` builds a `ControlValues` (G-049) from the
  panel's values (or the defaults for `--smoke`), replaces the preset or the free terms
  (`custom`, answering `scripted`), the seed (the preset's own when a preset or terms are
  given without one, as the panel's `choose_preset` does), and the cap for the run as
  `cap_total`; `run_inputs` turns it into the `RunInputs`, `--answers` replaces the
  answers, and `set_inputs` shows it. An invalid value is argparse's usage error (status
  2) after the window is shut down. `--smoke` runs `run_to_end`, quits at
  `run_finished`, prints the summary line of the controller's result (`Session.
  summary()`, the CLI's last line) to stdout, and returns 0 when the outcome is `ok`; a
  run that did not end prints to stderr and returns 1. `docs/gui/panes.md` holds the
  table `| Pane | Name | What it shows | Pages | Drawing |`, with `GUI design, not
  Meredith's` in the Drawing column of P5 and P6; `models/guide.py` (Qt-free) reads it
  into `GuideRow`s and renders lines or HTML; `show_guide` shows the HTML in a
  `QTextBrowser` dialog (a message box cannot hold a table), and falls back to the
  docks' own `pages` of `PANES` when the docs are not installed. About is a short text
  with the engine's version.
- **Reason:** the panel's `ControlValues` is already the one way inputs are parsed and
  shown, so the command line cannot disagree with it (G-054); the summary line equals
  the CLI's because it is the session's own; the table in the guide keeps the help page
  and the documentation one text.
- **Alternatives:** Numbo's `--puzzle N`/`--custom` names (the CLI's names are the
  engine's); a help page typed in the code (would drift from the guide).

### G-056: The screenshots are rendered offscreen from the appendix presets, the storyboard and the platoplasm from their docks floated at fixed sizes
- **Sources:** iterations.md item 17: `scripts/docs_screenshots.py`, as Numbo's, renders
  offscreen and deterministically `gui-window.png` (the whole window at the guess of run 1,
  `window.grab()` scaled to 1000 px), `gui-cytoplasm.png` (the cytoplasm at the end of run
  2), `gui-reformulation.png` (run 3's storyboard) and `gui-platoplasm.png` (run 3's slip),
  each under 200 KB (Numbo's limit). Numbo GUI's `docs_screenshots.py`: a window of 1500 x
  950 shown offscreen at full speed, `window.grab()` scaled with
  `SmoothTransformation`, the canvas's scene through `render_png` at 1.5x, `save` failing
  over the limit. The guess of run 1 is `I have a guess!`, `3 7 3 7` [M86 p. 166]; run 3's
  slip is `Changing to a new reigning class. Countup --> S-group` [M86 p. 174], and the
  crown travels the s-link over `CROWN_MOVE` events (G-037); the first candidate of run 3
  is `new hypoth candidate (S-group 1 2)` [M86 p. 174].
- **Choice:** the script is Numbo's with four pictures. The window picture is taken with
  the run in *interactive* mode, stopped in the answerer at the guess (`run_to_guess`:
  the worker waits until the guess panel is answered, G-008), so the panel shows
  `I have a guess!` with its OK and No as a user sees it; the script then answers `ok`
  and the run ends as run 1 does. The window is asked for 1500 x 950 and grows to its
  docks' minimum (about 2224 x 1116 with Qt 6.11; item 18's minimum size will shrink it),
  then scaled to 1000 px wide as the item says. The cytoplasm is the canvas's whole scene
  at 1.5x at the end of run 2 (`CytoplasmCanvas.render_png`). The storyboard is taken at
  the index of run 3's first `candidate` event, where the first episode is complete and
  open (the newest episode is the open one, G-041), with the reformulation dock floated at
  760 x 980 so the whole episode is in view; the platoplasm at the `class-changed` event
  plus `CROWN_MOVE // 2`, the crown half-way along the s-link, with its dock floated at
  1300 x 300 so the list beside the network is readable; both docks are docked back after.
  `--out DIR` renders elsewhere (the default is `python/docs/`), so the tests render into
  a temporary folder and never overwrite the committed pictures. The floating dock's
  offscreen message, `This plugin does not support raise()`, joined `OFFSCREEN_NOISE` in
  `app.py`. Two consecutive renders are byte-identical on one machine; the fonts of
  another machine may differ, so the tests check sizes and limits, not pixels.
- **Reason:** the docked storyboard and platoplasm are small (570 x 332 and 1224 x 200 in
  the default layout) and would show a few lines each; a floated dock at a fixed size is
  deterministic and needs no change to the views. The interactive guess shows what the
  guess panel is for; at the scripted `guess` event the panel says no guess is waiting.
- **Alternatives:** grab the docked panes as they are (too small to read); resize the
  docks with `resizeDocks` (clamped by the neighbours' minimums; item 18); pixel-compare
  the committed pictures in the tests (fonts differ between machines).

### G-057: The documentation: the guide has one section per pane; the README's GUI section follows Numbo's, with the `--smoke` command in a `sh` block that the engine's `test_docs.py` runs too
- **Sources:** iterations.md item 17: `docs/gui/panes.md`, the user guide, each pane, what
  it shows, the pages behind it, how to read it, and which drawings are GUI design; the
  "The GUI" section of `python/README.md` (install with `pip install -e '.[gui]'`, run,
  screenshot, the guide); `seekwhence/gui/README.md` (module map); one line in
  `Software/SeekWhence/README.md`; `tests/gui/test_readme.py` (Numbo's: the screenshot
  files exist, are under 200 KB and are referenced; the README's commands run); the
  guide's quotations pass the checker. TASK.md: `README.md` gets ONLY a new "The GUI"
  section. Numbo's README puts its `pip` lines in a `text` block and its `--smoke` command
  in a `sh` block followed by the output it prints, and its `test_readme.py` runs every
  `sh` block from a fresh shell. The engine's `tests/test_docs.py` (frozen) runs every
  `sh` block of `python/README.md` as a subprocess with `PYTHONPATH` set to `python/`,
  treating a command without `--terms` as the interactive run (it feeds run 1's terms and
  expects `I have a guess!`). `models/guide.py` (G-055) reads the first table of the
  guide whose header is `| Pane | Name | What it shows | Pages | Drawing |`.
- **Choice:** the guide keeps that table unchanged (the help page is made from it) and
  adds, around it, how to start, the window (the default layout, the menus and keys, the
  shared selection, the drawing rule), one `### Pn.` section per pane with what it shows,
  its pages, the figure or precedent its drawing follows and how to read it (P5 and P6
  open with **GUI design, not Meredith's**), sessions and replay, the default layout, and
  the screenshots with their pages. Quotations of six words or more are the ones the
  code's docstrings already carry (checked), everything else is paraphrased without
  quotation marks. The README's section follows Numbo's: the `pip` lines and the plain
  run in a `text` block, the panes in brief, two of the four pictures, the keys, the
  session and layout notes, the options, then the `--smoke` command in a `sh` block with
  the line it prints and the same run in the CLI with the same line. The smoke command
  gives `--terms 3 7 3 7 --seed 54 --answers ok` rather than `--preset run1_3737` so that
  `test_docs.py` runs it as a scripted command; it therefore needs PySide6 when the
  engine's suite runs (the subprocess sees the real package even under the gate's stub,
  because `test_docs.py` replaces `PYTHONPATH`). `tests/gui/test_readme.py` runs the
  section's `sh` blocks from a fresh environment (PATH, HOME, LANG, and offscreen) and
  compares the shown output, as Numbo's does, and also checks the four pictures (PNG,
  under 200 KB, in `docs/`, tracked, referenced from the README or the guide), the
  section's content, the guide's sections, the module maps of `seekwhence/gui/README.md`
  and `tests/gui/README.md` against the files, runs the screenshot script into a
  temporary folder, and runs the quotation checker over the guide. The top-level README
  gets one line pointing at the section and the guide. The sentence "no GUI" in the
  README's first section and "They need only `pytest`" in its tests section are now
  stale, but TASK.md allows only the new section: left for the final audit (item 19) and
  noted for the author.
- **Reason:** Numbo's conventions (ANSWERS.md 7); the engine's own documentation test
  already runs the README's commands, so the smoke line is exercised by both suites;
  machine-checked quotations cannot drift.
- **Alternatives:** the commands in a `text` block (then nothing runs them, against
  Numbo's convention and the item's test); `--preset run1_3737` (the engine's test would
  feed it terms on stdin and expect the interactive transcript); editing the stale
  sentences (outside the permitted change).

### G-058: The window fits 1280 x 800 with every dock shown: explicit minimums, clipping labels, a scrolling controls form, the Configuration dock tabbed beside the Hypothesis dock
- **Sources:** iterations.md item 18: "minimum size 1280x800 with every dock shown".
  PANES.md section 3 puts P13 in the right area, hidden by default (ANSWERS.md 5: a dock
  opened by Edit > Configuration). Numbo's `main_window.py` sizes its docks in
  `showEvent` and says nothing of a smallest size. Qt's own minimum for a scroll area is
  about 70 x 70, a label's its text's width, a combo box's its longest item's; with ten
  docks, the cytoplasm's legend (895 px) and the controls' two combo boxes (346 and
  420 px), the window could not be narrower than 2224 px nor shorter than 1436 px with
  every dock shown (measured in iteration 19; item 17 had noted 2224).
- **Choice:** `SMALLEST_SIZE = (1280, 800)` in `main_window.py`, and the panes made to
  fit it: every `LineList`, the console and log lists, the configuration's table and
  editor get an explicit minimum of `LIST_MIN` = 40 x 24 (an explicit minimum overrides
  Qt's hint); the stats view's minimum is four rows, the oval tree's 40 px, the gnoth
  strip's 36 px, the platoplasm network's 80 x 48 (it scales its picture down); the long
  labels (the cytoplasm's legend, the taskrack's and the platoplasm's "GUI design, not
  Meredith's", the socratoplasm's parse, the hypothesis's form, the configuration's
  editor label) get a minimum width of 1, so they keep their preferred width and clip
  when the dock is narrower; the preset and answering combo boxes size to a contents
  length of 18 characters; the controls' form sits in a `QScrollArea` inside the panel
  (the panel is still the dock's widget), so a short dock scrolls it; and the
  Configuration dock is tabified beside the Hypothesis and Reformulation docks (PANES'
  `under` column) rather than stacked fourth in the right column, where its minimum
  (the wrapped note, the table, the editor) alone was 222 px. Measured after: 862 x 783
  with every dock shown. `default_layout.json` was regenerated for the tab.
- **Reason:** a laptop screen; a pane too small clips or scrolls rather than widening
  the window past the screen, and nothing is hidden for good (a wider dock shows all).
  The configuration pane is a tall table and sits well as a tab in the column that
  holds the other read-and-click panes; Edit > Configuration still shows and raises it.
- **Alternatives:** overriding `minimumSizeHint` on each view (children would overlap);
  wrapping the long labels (their height would then vary with the width); keeping P13
  stacked and cutting the other right-column panes further (the hypothesis's prediction
  strip and tree would lose their rows).

### G-059: The layout bytes carry a state version; a state from another version is ignored and said so
- **Sources:** iterations.md item 18: "settings from an older layout are ignored safely".
  ANSWERS.md 8: a layout that fails to restore falls back to the next level and says so
  in the status bar; never crash. Numbo's `main_window.py` calls `saveState()` and
  `restoreState(bytes)` with Qt's default version 0. `QMainWindow.restoreState(state,
  version)` refuses bytes whose version is not the one given.
- **Choice:** `STATE_VERSION = 1`; `saveState(STATE_VERSION)` for the QSettings
  `window/state` and for the default layout file's `state` (`layout_record`), and
  `restoreState(..., STATE_VERSION)` in both places. A refused state leaves the
  built-in arrangement pending, puts "the saved layout is from another version of the
  window and was ignored" (or, for the file, "could not be restored (saved by another
  version of the window?)") in the status bar after the inputs are restored (the note
  used to be wiped by the inputs' own status line), and goes on to the next layout of
  the precedence; a `window/state` that is not bytes is ignored as before. Closing
  saves the current version over the old one. The version is bumped by hand when the
  docks change in a way an old state would misplace (a dock renamed, a tab group
  reorganised); the shipped `default_layout.json` is regenerated then.
- **Reason:** Qt's own mechanism for exactly this; a state of version 0 is what every
  window before item 18 wrote, so the tab moved by G-058 never comes back from an old
  setting. A state of the right version from a window with other docks is accepted by
  Qt (the unknown dock ignored, ours left where they were built), which is safe too:
  a test checks every pane stays reachable and a run goes through.
- **Alternatives:** keeping version 0 and checking the dock names in the bytes (Qt's
  format is private); clearing `window/state` whenever the window's version changes (a
  version in QSettings beside the state: the same thing in two places).

### G-060: The platoplasm network paints a cached pixmap of its layout
- **Sources:** iterations.md item 18: "`blackburn-11` runs to the end at max speed within
  a time bound". Numbo's `pnet_view.py` paints only the pnodes that changed. The profile
  of a capped blackburn-11 run through the window (iteration 19): 7 ms an event, of
  which the platoplasm's `paintEvent` 1.3 ms (27 arrows and 8 boxes with their texts,
  about 166 `drawText` calls, painted at every one of the 28,959 events although the
  platoplasm changes only at a slip, a dubbing, a slip-check or a selection).
- **Choice:** `NetworkWidget.paintEvent` draws `_picture`, a `QPixmap` the widget's
  size rendered by `render_picture` (the old `paintEvent` body) when the layout, the
  size or the device pixel ratio differ from the ones it was rendered for; `Layout` is
  a frozen dataclass of frozen dataclasses, so a layout rebuilt equal to the last is
  equal by value. A fresh `grab()` paints the same pixmap, so the pixel probe holds.
  Measured: blackburn-11 through the window 232 s before, 129 s after (4.4 ms an
  event), with the smaller window of G-058 contributing to the difference.
- **Reason:** the every-event redraw rule (each view painted exactly once per event)
  stays; only what the paint costs changes. The remaining cost is spread over the
  cytoplasm's scene, the lists and the models and has no single large piece.
- **Alternatives:** painting only the dirty parts (the crown, the lit links) over a
  static layer (two pictures to keep in step); skipping the redraw when the model did
  not change (the view protocol has no such signal, and the paint count is the probe).

### G-061: The robustness tests and their bounds
- **Sources:** iterations.md item 18 (the six checks); Numbo's `test_polish.py` (the
  pixel probe over five widgets, `tracemalloc` over six runs with the bounds 50 objects
  and 24 KB a run, the closes); item 12's log test (CPU time of the last window against
  the first, G-045). Measured in iteration 19 on an idle machine: 4 objects and 2.7 KB
  a run; blackburn-11 129 s for 28,959 events.
- **Choice:** `tests/gui/test_robustness.py`: the probe compares, after every event of
  run 1, the backing store with a fresh `grab()` for every painting widget (the strips,
  the viewports of the lists and the cytoplasm, the tree, the bar, the network, the
  plot, the timeline's strips) and for every whole view, skipping widgets with an empty
  visible region (the tabs underneath), and again every fifth event of run 3 with the
  log and the reformulation raised, then at ten seeks; memory as Numbo's bounds;
  closing with `close()` while a run plays at 20 ms an event, while an interactive run
  waits for a term, and with a guess pending, each leaving no live worker; blackburn-11
  judged, as the bench runs it, through the window at max speed within 12 ms an event
  (347 s, the waitSignal timeout a minute more), equal to the plain run, the last 5000
  events' CPU time at most twice the first 5000's; the window resized to 1280 x 800
  with every dock shown, its minimum size hint within it, every dock on the window or
  in a tab, and run 1 through it; the state of version 0 and 2 ignored with the note, a
  state of this version naming other docks leaving every pane reachable, a state that
  is not bytes ignored.
- **Reason:** the item's list; the bounds leave room for a loaded machine (iteration 16
  saw 2x) while a leak or a per-event cost that grows with the run would show many
  times over. The blackburn-11 run adds about two minutes to the GUI suite (now about
  nine minutes under the gate, whose budget is twenty); the author may prefer it behind
  an environment variable.
- **Alternatives:** a shorter sequence for the time bound (the item names
  blackburn-11); sampling the pixel probe (Numbo checks every event of its puzzle 1).

### G-062: Four interactions of PANES.md are left out at the final review, and listed
- **Sources:** PANES.md P1 ("Click a cell to highlight its glint in the cytoplasm and its
  gnoth in the socratoplasm, and to seek the timeline to the moment it was entered"); P8
  ("Find (Ctrl+F)", lines after the cursor "greyed (when following) or hidden (option)");
  P13's interactions ("Copy a row; filter by name; click a decision number to open its text
  in a read-only viewer", written for the read-only pane that ANSWERS.md 5 withdrew);
  section 5 (on a differing replay "the structure panes are marked 'not available for this
  file'"). ANSWERS.md says nothing of them. Item 19 (the final review) adds no behaviour.
- **Choice:** the review (`docs/gui/review.md`) lists them as departures and the GUI stays
  as items 03 to 18 left it: a cell's click highlights and selects the glint (G-014, G-025),
  and the socratoplasm outlines a gnoth only when the gnoth or its glom is selected
  (G-031); the find box takes Enter and its button, with no shortcut (G-019), and the lines
  after the cursor are greyed, never hidden (G-020); the configuration pane filters by name
  and shows a decision's heading in the Source tooltip, with no Copy and no viewer (G-051);
  a differing replay is said in the status bar, a message box and the stats row's
  `stored events of <file> (differs at event N)`, and the structure panes show what the
  stored events alone give (G-047).
- **Reason:** each is a few lines with a test, but the review's one job is to check and
  record; adding behaviour there would leave it untested by the item's own criteria. They
  are the first things a later item should do (PROGRESS.md, "For the author").
- **Alternatives:** add them: `MainWindow` would connect `cell_clicked` to the shared
  selection (`select_gnoth` of the gnoth whose range covers the term) and to
  `jump_to("term", n)` when no run is live; `QKeySequence.StandardKey.Find` on the console's
  find box; a "Hide lines after the cursor" check box beside Follow; a Copy button per
  configuration row and a `QTextBrowser` for a decision's entry; a banner on each
  structure pane while `replay_source` holds a differing file.

### G-063: Functions over 60 lines are the widget-declaring constructors and the menu builder
- **Sources:** iterations.md item 19 ("functions over about 60 lines are split or
  justified"); the package's conventions (short functions with one job); the Numbo GUI, whose
  `main_window.py` constructor is 122 lines, `controls.py`'s 67 and `coderack_view.py`'s
  `paintEvent` 68.
- **Choice:** at the review `cytoplasm.layout` (73 code lines) became `layout` plus
  `_arc_shapes`, `_hull_shapes`, `_marker_shapes` and `_bounds`, and the sequence strip's
  `paintEvent` (75) became `_paint_cells`, `_paint_ghosts` and `_paint_badges`; the six
  that remain over 60 code lines (docstrings not counted) declare widgets and connect
  signals in a straight line with no branching to test apart: `ControlsPanel.__init__`
  (149), `ConfigView.__init__` (84), `MainWindow._build_menus` (78), `ConsoleView.__init__`
  (70), `HypothesisView.__init__` (69) and `TaskrackView.__init__` (67).
  `tests/gui/test_gui_conventions.py` lists these six by file and name and fails on any
  other function over 60 code lines, so a new one must be split or added to the list with
  its reason.
- **Reason:** a constructor that builds a form is a declaration; cutting it into
  `_build_x` methods that each set three attributes hides the order the widgets appear in,
  which is the one thing a reader of a constructor wants, and Numbo's constructors have
  the same shape. The two pure functions were worth splitting because each section has a
  name and a test of its own.
- **Alternatives:** split the constructors by row of the form; raise the threshold to
  Numbo's longest.
