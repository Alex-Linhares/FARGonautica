# The GUI's panes, and the pages behind them

The user guide of the Seek-Whence GUI (`python3 -m seekwhence.gui`, or `seekwhence-gui`
once installed with `pip install -e '.[gui]'`). The GUI watches the reconstruction of
Meredith's program run, one event at a time: the terms coming in, the cytoplasm glomming
them, the gnoths parsing them, the hypothesis predicting, the guess, and the reformulation
when a term refutes the hypothesis. It drives a run and observes it, and nothing else: the
engine is not changed, draws no random number from the GUI, and gives the same run for a
seed with or without the window (the loop's gate checks it on every iteration).

Every pane says which pages of the sources justify what it draws. The tags are the
loop's: `[M86 p. N]` is Meredith's thesis (*Seek-Whence: A Model of Pattern Perception*,
1986), printed page N; `[FCCA p. N]` is Hofstadter's chapter 1 of *Fluid Concepts and
Creative Analogies*, printed page N; `[MHN p. N]` Meredith's historical notes (2017);
"Numbo GUI" is the precedent followed, the Numbo GUI in `Software/Numbo/python/numbo/gui/`,
whose conventions this GUI adopts throughout (the loop's ANSWERS.md 7); `D-NNN` is a
decision of the engine's `DECISIONS.md`, `G-NNN` one of the GUI's `GUI-DECISIONS.md`.

Two drawings have no figure in the sources and are **GUI design, not Meredith's**: the
platoplasm network (P5) and the taskrack drawing (P6). The panes say so, in the pane and
in its tooltip, and so does their row below (ANSWERS.md 4). Everything else follows a
figure, a transcript or a precedent, named in the pane's section.

Help > "What am I looking at?" shows the table below, generated from this file
(`seekwhence/gui/models/guide.py`).

## Starting

```text
python3 -m pip install -e '.[gui]'          # PySide6 6.6 or later (tested with 6.11)
python3 -m seekwhence.gui                   # or: seekwhence-gui
python3 -m seekwhence.gui --preset run3_121231234 --play
python3 -m seekwhence.gui --terms 3 7 3 7 --seed 54 --answers ok --smoke
```

Without PySide6 the engine, its tests and the command line work as before, and
`python3 -m seekwhence.gui` says what to install. The arguments are the CLI's (`--preset
NAME` or `--terms ...`, `--seed N`, `--answers ...`, `--max-steps N`) plus `--play`, which
starts the run at once, and `--smoke`, which runs the inputs to the end with every event
drawn, offscreen, prints the CLI's summary line and exits 0 for an accepted guess, 1
otherwise, 2 for a usage error, reading and writing no settings (G-055). The window's size,
docks, inputs and speed are remembered between sessions (QSettings `seekwhence` /
`seekwhence-gui`, as the Numbo GUI's).

## The window

The window is the Numbo GUI's: one main window, a central area and one dock per pane,
each with a tick in the View menu, and the timeline as a toolbar along the bottom. The
central area holds the sequence (P1) over the cytoplasm (P2). By default the controls
(P11) and the stats (P12) are on the left; the taskrack (P6), the socratoplasm (P3) and the
hypothesis (P4, with the reformulation P7 tabbed under it) on the right; the platoplasm
(P5) and the console (P8, with the event log P9 tabbed under it) along the bottom; the
configuration (P13) is hidden until Edit > Configuration opens it, as a tab beside the
hypothesis (G-058). The window fits 1280 x 800 with every dock shown: a dock made smaller than
its pane clips or scrolls it (G-058).

![The window at the guess of run 1](../gui-window.png)

*The whole window at the guess of run 1, `3 7 3 7` [M86 p. 166], as
`scripts/docs_screenshots.py` renders it offscreen.*

**The menus and keys** (Numbo's, plus Edit):

| Menu | Actions |
|---|---|
| Session | Open session… (Ctrl+O), Save session… (Ctrl+S), Close replay; Save transcript…, Save events (JSON lines)…; Quit |
| Edit | Configuration (opens P13); Copy the transcript |
| Run | Play / Pause (Space), Step (Right; when no run is live, one event forward in the history), Step task (Ctrl+Right, to the next task chosen), Step term (Shift+Right, to the next prompt), Run to end, Stop (Esc), Back one event (Left), New run (Ctrl+N) |
| View | a tick per dock and for the timeline; Cytoplasm layers (sparks, top-down, values, names); Reset layout, Reset to built-in layout, Save layout as default for this code… |
| Presets | the three appendix runs [M86 pp. 164-179]; Type the terms yourself (interactive); the Blackburn dozen [M86 pp. 6-7, 145-147] and the other lists of `seekwhence.sequences` (pp. 88, 38, 150, and FCCA's sampler [FCCA pp. 53-54]) in submenus |
| Help | What am I looking at? (the table of this page); About |

**One selection for every pane** (G-025). A click on a glint, an arc or a glom in the
cytoplasm, on a gnoth in the socratoplasm, on a type in the platoplasm or on a task in the
taskrack puts the thing and what it implies (a glom its members, a gnoth its glom, a type
its gnoths) into one shared selection, and every pane shows it: the cytoplasm outlines
the selected glints and hulls, the socratoplasm its gnoths, the taskrack the tasks whose
arguments name any of them. A second click clears it, as does a click on empty canvas.

**Every event is drawn.** After each event the run waits until every pane on screen has
redrawn, so a step of one event shows exactly one change (the Numbo GUI's rule, G-011).
Speed "max" means as fast as the window can draw.

## The panes

| Pane | Name | What it shows | Pages | Drawing |
|---|---|---|---|---|
| P1 | Sequence and input | the terms with their fate under the hypothesis, the ghost cells of the guess, the input box and the guess panel | [M86 pp. 7, 37, 164-166, 175-176]; [FCCA p. 52] | Meredith's transcripts |
| P2 | Cytoplasm | glints on a baseline, sparks and bonds as arcs, gloms as nested hulls, pseudo-gloms in grey, proposals | [M86 pp. 44, 61-70, 73, 77, 92, 102, 106]; [FCCA pp. 58-60] | Meredith's Ch. 3 Figs. 1-2 |
| P3 | Socratoplasm | the gnoths of the parse as boxes over the terms, the catchall, the template, the parse text | [M86 pp. 46, 78, 98, 102, 167-168, 174-175] | Meredith's Ch. 3 Fig. 1 |
| P4 | Hypothesis | the form and its source, the ovals of the form, the box tree, the predictions against the terms, the validity | [M86 pp. 16-17, 50-53, 79-85, 125, 161, 166-168, 174] | Meredith's Ch. 1 Figs. 1-2 and Ch. 2 Fig. 3 |
| P5 | Platoplasm | the eight ideal types with their s-links and slipperiness, the reigning class, the slip-check, the atoms | [M86 pp. 45, 48-55, 59, 90-91, 99-105, 126, 168, 174-176] | GUI design, not Meredith's |
| P6 | Taskrack | the tasks waiting by type with their weights, the task chosen and its chance, the cleanings, the history | [M86 pp. 41-42] | GUI design, not Meredith's |
| P7 | Reformulation | the storyboard of a refutation: slip-check, moves, gnoth operations, casts, the candidate, the blast | [M86 pp. 87-126, 168, 174-176] | Meredith's before/after pictures, pp. 110-120 |
| P8 | Console | the transcript, verbose or terse, as the CLI prints it, and a command line | [M86 pp. 164-179] | Meredith's transcripts |
| P9 | Event log | every event of the run with its clock, filtered by kind | Numbo GUI, log_view.py | Numbo GUI |
| P10 | Timeline | the slider over the history with the terms, guesses, refutations, slips and blasts marked, and the parse strip | Numbo GUI, timeline.py; [FCCA pp. 41, 64-65] | Numbo GUI; FCCA Figure 1-1 |
| P11 | Controls | the preset, the terms, the seed, the caps, the answering mode, the trace, the run buttons and the speed | [M86 pp. 6-7, 37, 145-147, 164-179]; [FCCA pp. 53-54]; Numbo GUI, controls.py | Numbo GUI |
| P12 | Stats | the run's numbers: events, clock, terms, tasks waiting, the reigning class, the hypothesis, the outcome | Numbo GUI, stats_view.py | Numbo GUI |
| P13 | Configuration | every field of the configuration with its source, editable for the next run | ANSWERS.md 5; D-069 | Numbo GUI |

### P1. Sequence and input

**What it shows.** The terms entered so far, as large numbered cells (the position above,
the value inside), the way the transcripts show them coming in one at a time after
`please enter a term:` [M86 p. 164]; [FCCA p. 52]. A cell's colour is the term's fate under
the hypothesis in force: green when the Hfilter found it predicted (run 1's fourth term,
`new term being hypothesis-filtered through (Cycle 3 2 (3 7))` [M86 p. 166]), red when it
refuted the hypothesis (`set-out -- validity: 0` [M86 p. 168]), grey when it was not
filtered. Under the cells, the block a fresh hypothesis of the form in force predicts,
grouped into its hits with a bracket under each, `3 7 3 7` [M86 p. 166] or
`(1 2)(1 2 3)(1 2 3 4)` [M86 p. 179]; the cells beyond the terms entered are dashed
ghosts, the predicted continuation (G-014). The predictions come from a fresh
`Hypothesis(form)`, never from the run's own box, which the GUI never touches
(ENGINE-API rule 3).

**The input and the guess.** The row `please enter a term:` with a box that takes one term
or several (`3 7 3 7` is queued, one per prompt), enabled while an interactive run waits
at a prompt (G-015); a show command typed here is a console line too, and the four show
buttons are the console's (P8). The guess panel is the transcripts'
`I have a guess!`, the form, the block and `enter no if wrong, ok if right` as two
buttons, OK and No [M86 pp. 166, 175]; it appears when the engine asks the user, and the
run waits until you answer (G-008). Under the cells, a badge per answered guess:
`3 7 3 7 ok` [M86 p. 166], `(1 2)(1 2)(1 2) nope` [M86 p. 175].

**How to read it.** A green row with a ghost continuation is a hypothesis that fits; a red
cell is the moment a reformulation starts (the storyboard, P7, raises itself then). Hover
a cell for its fate, a ghost for the form and hit that predicted it; click a cell to
select its glint in the cytoplasm.

### P2. Cytoplasm

**What it shows.** The perceptual level as one drawing in the style of Meredith's Ch. 3
Figures 1 and 2 [M86 pp. 102, 106] and of Hofstadter's islands of order held together by
glue [FCCA pp. 58-60]:

- **Glints**, one per term, as teardrops on a baseline, the value inside and the name
  (`glint3`) under it [M86 p. 61]; a glint inside a glom is drawn lighter, its `active`
  field [M86 p. 67].
- **Bonds** as arcs below the baseline [M86 p. 64]: blue for Same, green for Succ, orange
  for Pred, Hofstadter's sameness, successorship and predecessorship glue
  [FCCA pp. 58-60]; a short arc for an adjacent bond, a tall one for a remote bond, as
  Figure 1 draws them; the width is the bond's strength (D-019). Under reformulation each
  bond is labelled with its value in the reigning class, `+10`, `-5` or `0`, exactly as
  Figure 1 labels them [M86 p. 102], read from the gnoths' bond lists.
- **Sparks** as faint dashed arcs, deeper than the bonds [M86 p. 63] (`spark6 --- between
  glint3glint1` [M86 p. 164]); they flash when made and fade when used up. They are a
  layer, off by default.
- **Gloms** as nested rounded hulls above the baseline, one level per nesting depth, so a
  glom of gloms sits above its subgloms, the islands of order [FCCA pp. 58-60]; each hull is
  labelled with its type (`Same print-value fence` [M86 p. 167]) and, once dubbed, with
  its class and form, pure or secondary (`Cycle (Cycle 3 2 (3 7)) pure` [M86 p. 69]), in
  bold. Burst, dissolved and blasted gloms fade out [M86 pp. 69-70, 92].
- **Pseudo-gloms** (the template glom, the gnoth-gloms, the frame gloms) as dotted grey
  hulls, lighter than a real glom's blue, so the two kinds are told apart at a glance
  [M86 pp. 73, 77] (the author's choice, ANSWERS.md 10.4, G-053); the "top-down" layer
  hides them. With them the current parse is visible at every moment.
- **Proposals in flight**: a small marker on a glint for each Glomtester proposed
  (`Same -cover proposed --> glint1` [M86 p. 165]) and a bracket over a Glommer's members,
  gone when the glom is made or the test fails: the terraced scan at work [M86 pp. 66-67].

![The cytoplasm at the end of run 2](../gui-cytoplasm.png)

*The cytoplasm at the end of run 2, `3 7 3 3 7 3` [M86 pp. 167-169]: the dubbed Cycle glom
over the first two glints, the remote Same bonds below the baseline, and the grey
pseudo-gloms of the parse.*

**How to read it.** Hover anything for its fields in `show`'s format, `bond1 Same
print-value (remote) -- (glint1 glint3)`, `glom1 (Same print-value fence) --> (3 7) terms
1 to 2` [M86 p. 167]; click to select it in every pane. The bar above the canvas has Fit,
zoom in and out, 1:1, the four layers (sparks, top-down, values, names; also under View >
Cytoplasm layers) and the legend with its pages. Wheel or + and − to zoom, drag to pan,
F to fit. A flash lasts six events and a fade twenty-four, by event count, not time, so a
replay and a seek show the same picture at any speed (G-021).

### P3. Socratoplasm

**What it shows.** The parse, large, in Meredith's bracket notation as `show-parse` prints
it, `((1 2) (1 2 3))` [M86 pp. 174, 178]; the gnoths as a row of rounded rectangles over
the terms they cover, the top row of Ch. 3 Figure 1 [M86 p. 102], with the catchall gnoth
dotted (a repository for the terms no gnoth troubles over [M86 p. 98]) and an uncapped
gnoth dashed [M86 p. 113]; and one card per gnoth with the fields exactly as `show`
prints them: class, name, frame, plato-class, glom, notes, form, state, range
[M86 pp. 168, 175], the Gnoth-maker's record [M86 p. 78], and after reformulation
external-bonds, internal-bonds, equivalence-type and groups, one line per bond
[M86 p. 175]. The template has a card of its own: its four fields,
`(form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)`, the evaluator's check
line [M86 p. 167], and its fate, accepted, filtered, modified, abandoned or resolved by a
blast [M86 pp. 74-76].

**How to read it.** Gnoths capture the program's current view, the parenthesization, of
the sequence [M86 p. 46]: the parse line and the row of boxes are the one thing to watch.
A card whose fields a running task is still changing is marked unconfirmed until the next
task boundary (G-030). Click a box or a card to select the gnoth with its glom and members
in the cytoplasm; hover a bond line to light its arc; "Copy as show" copies the `gnoths:`
block the CLI would print at this moment.

### P4. Hypothesis

**What it shows.** The form, large, as the transcripts print it after `hypothesis:`,
`(Cycle 3 2 (3 7))` [M86 p. 166], with its source: the template, the Tuple fall-back
[M86 p. 79] or a candidate of the reformulation [M86 p. 125]. Under it the form as Ch. 1
Figures 1 and 2 draw the primitives: the type's name in an oval, the arguments in circles
hanging from it, nested forms as child ovals, a shared structure joined by a dashed line
to each place that shares it, and beside the top oval the first values it gives,
`(2 3 4)` [M86 pp. 16-17, 50-53] (G-034). The validity is a number and a bar, "a number
that grows as new, correctly-predicted terms are encountered" [M86 p. 81], falling to 0
at a refutation (`set-out -- validity: 0` [M86 p. 168]), with a mark at the guess
threshold (`Config.guess_validity`). The predictions table has the terms a fresh box
predicts above the terms entered, matches green, the refuting term red with the term that
was predicted [M86 p. 168], and three terms of lookahead beyond. The list below gives the
episode's casts and whether each postdicts the known terms [M86 pp. 125, 174]; the box
tree, Ch. 2 Figure 3's Box and Pstruc records with the run's own names (`box15`,
`pstruc15`) [M86 p. 83], behind the "Box tree" button, off by default; and a private box
to Hit and Reset, the appendix's `show-box` [M86 p. 161], which explores a form (yours, or
the form in force) on a box of its own and never touches the run.

**How to read it.** Click an oval to outline, in the predictions table, the positions that
node gives (the top oval every term, a Cycle's element the hits of its turn). Hover a box
record for its fields. Between a new hypothesis and the next task boundary the box names
are provisional and the list says so (G-033).

### P5. Platoplasm

**GUI design, not Meredith's.** The thesis describes the platoplasm, the ideal types and the
slipping network between them in prose and has no figure of the network; FCCA has none.
The layout below is the GUI's own (ANSWERS.md 4), with the Numbo GUI's Pnet view as the
engineering precedent. The pane says so in its corner and in its tooltip.

**What it shows.** The eight ideal types as boxes by category, the strictest first:
repeater types (Constant, Countup), generate types (C-group, S-group, P-group), the
process type (Y-group) and the fence types (Cycle, Tuple) [M86 pp. 54-55, 59]; each box
gives its format, whether it is strict [M86 p. 99], and its counts of manifestations
(dubbed gloms, `m`) and actualizations (gnoths, `a`), the lists each Platonic concept
keeps [M86 p. 90]. The directed s-links between them are arrows labelled with their
slipperiness, a number from 0 to 1 (`S-group -> Countup 0.1`, `S-group -> Tuple 0.4`
[M86 p. 90]; the table is `Config.s_links`, D-038). The reigning class wears a crown and is
highlighted, with the threshold beside it [M86 pp. 99, 105]. The ideal atoms, the values
seen, are a strip of small boxes under the network, each with its glint count
[M86 pp. 48-49]. The list on the right prints the reigning class's pulls and pushes
(bonds with a strength from 1 to 10 [M86 p. 91]) and, as it happens, the slip-check:
the staying value, each neighbour's pressure along its s-link as the product p. 104
computes it, the best pretender, and stay or slip [M86 pp. 103-105], for example
`slip-check: stayval: 15.0`, `best: (Y-group 4.0)` [M86 p. 176]. A slip moves the crown
along the s-link over six events, `Changing to a new reigning class. Countup --> S-group`
[M86 p. 174] (G-037). The "Links" button adds the manifestation and actualization lines
from each type to its gloms and gnoths, the links Ch. 2 Figure 2 draws between the plasms
[M86 p. 45].

![The platoplasm at run 3's slip](../gui-platoplasm.png)

*The platoplasm at run 3's slip, `Countup --> S-group` [M86 p. 174], the crown half-way
along the s-link.*

**How to read it.** Hover a type for its pulls and pushes and an s-link for its
slipperiness; during a check the lit links are the pressures being weighed. Click a type
to select its gloms and gnoths in the other panes (G-038).

### P6. Taskrack

**GUI design, not Meredith's.** The thesis describes the taskrack in prose, tasks that
"run in simulated parallel" [M86 p. 41] and wait on the rack with urgencies that weight
the draw, and the cleaning of old tasks [M86 pp. 41-42], but has no figure of it, and FCCA
has none. The drawing follows the Numbo GUI's coderack view (ANSWERS.md 4); the pane says
so in its corner and in its tooltip.

**What it shows.** The task just chosen on a band at the top, as the trace prints it
(`doing task Sparkler-plus on (glint1 glint1 2)` [M86 p. 164]), with the chance it had at
the draw (its weight over the rack's total, D-007) and `stale` in grey when it found the
world changed since it was posted [M86 p. 42] (D-009); the Hfilter, run at once and never
drawn, says so (D-012). Under it the waiting tasks as rows, one per task type, coloured by
arena (cytoplasm, socratoplasm, platoplasm, user; D-057), each with its count and a bar of
its share of the total weight, the chance the next draw falls on it; a click on a row lists
its tasks (number, arguments, urgency, posted-at clock), and "every task" opens them all. A
cleaning shows as a banner for a while (`term: removed 3 Sparkler, 1 Tester`, D-010).
Under the rack, the history of every task chosen this run, with the per-type tallies since
the last term above it; the arena box filters the rows and the history.

**How to read it.** The band is what the program is doing now; the rows are what it might
do next, the longer the bar the likelier. Click a task to select the structures its
arguments name; tasks naming anything selected are outlined (G-027 to G-029).

### P7. Reformulation

**What it shows.** Stage two [M86 ch. 3, pp. 87-126] as a storyboard: each refutation opens
an episode, a list of cards in the order of the chapter, and each reformulation event of
the run lands on exactly one card (G-039):

1. the refutation, the term, the prediction and `set-out -- validity: 0` [M86 p. 168];
2. the form freeze-dried with the number of terms it explained, and the class made
   reigning [M86 pp. 91, 99];
3. the gnoths set, each with its frame, range and glom, new or kept, and
   `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 pp. 168, 173, 176];
4. the slip-check as P5 prints it, and a class change, `Countup --> S-group`
   [M86 pp. 103-105, 174];
5. the Reformulator's moves: for each move found its level in the order of p. 107, the
   pull, which gnoth moves, the hold, the strength, and the PROGRAM built,
   `PROGRAM: (SHIFT-RIGHT gnoth1 gnoth2 (glint1))` [M86 pp. 105-109, 174];
6. each gnoth operation as a before/after bracket picture in the style of pp. 110-120,
   `gnoth2: [ 1  2 ]` with the glint or glom names aligned under the groups,
   `[] (disappears)` and `(emerges)` as p. 120 writes them, with the gloms made, the gnoths
   gone or uncapped, the Capper's recap [M86 p. 113], and "carried out" or why not;
7. the Bond-assessor: the threshold, the strongest bond, and whether another round
   follows (`doing task Bond-assessor on (S-group 8.0)` [M86 pp. 124, 174]);
8. the casts and the generalised form, `casts: ((S-group 1 2) (S-group 1 1))`
   [M86 pp. 124-125, 174];
9. the candidate, `new hypoth candidate (S-group 1 2)` [M86 p. 174], with its
   postdiction [M86 p. 125], or the failure with its count and what follows, a Slip-scout
   or the blast [M86 p. 126];
10. the blast: the gnoths destroyed, the gloms burst, the bonds that survive [M86 p. 92].

Above the storyboard, the freeze-dried list (every old hypothesis with the terms it
explained, newest first [M86 p. 91]), the reigning class and threshold, and a plot of the
threshold against the clock within the episode [M86 p. 105]. "In series" stacks the
successive parses as p. 120's side-by-side figure does, one column per gnoth
[M86 p. 120].

![Run 3's storyboard at its first candidate](../gui-reformulation.png)

*Run 3's first episode at its candidate, `(S-group 1 2)` [M86 p. 174]: the refutation, the
freeze-dried `(Countup 1)`, the gnoths set, the slip to S-group, the Reformulator's
PROGRAM, the SHIFT-RIGHT before and after, and the Bond-assessor.*

**How to read it.** The pane raises itself at the run's first refutation. Click a card to
seek the whole window to its event; hover a card to select the gnoths and gloms it names;
click an episode's title to fold it (G-041).

### P8. Console

**What it shows.** The transcript exactly as the CLI prints it, produced by the engine's own
`Trace`: `-> (startup)`, `please enter a term: 3`, the `doing task` lines, the happenings
(`spark1 --- between glint2glint1`), the show commands' output, the guess and `bye`
[M86 pp. 164-179], Meredith's only display. Verbose prints the `doing task` lines as runs 1
and 3 do, Terse only the happenings as run 2 [M86 pp. 164, 167] (D-055); the switch is
instant, both texts are kept (G-018). The lines after the cursor are greyed when you look
into the past. Under the lines a command line takes what the CLI takes, a term, `show`,
`show-hypothesis`, `show-seq`, `show-parse` or `bye` (D-054), with the four commands as
buttons; in an interactive run a command typed here is an input of the session, recorded
in its session file, so the appendix runs replay with Meredith's commands in place
(G-019). Find (Enter or the button finds the next match), Follow, the line count, and
Session > Save transcript… and Edit > Copy the transcript in the mode shown.

**How to read it.** Read it beside the thesis's printouts [M86 pp. 164-179]: the lines are
the same. Click a line to seek to its event when no run is live.

### P9. Event log

**What it shows.** One row per engine event, with its index, the clock of the task under way
[M86 p. 41], its kind and its fields as the JSON-lines trace records them (the Numbo GUI's
`log_view.py`, adopted; G-044). The Kinds menu filters by kind and by group (run, taskrack,
cytoplasm, templates, gnoths, filtering, reformulation; the sections of `seekwhence/
events.py`), with Show all and Hide all; Follow keeps the last row in view. Session > Save
events (JSON lines)… writes the history exactly as `python3 -m seekwhence --json` does
(D-069).

**How to read it.** It is the debugging view of the structured stream behind the transcript.
Double-click a row, or press Enter on it, to seek there.

### P10. Timeline

**What it shows.** A slider over the whole history (the Numbo GUI's `TimelineBar`), one
event back and forward, a go-to box (`term 5`, `clock 812`, `event 402`, or a bare term
number), and the position (`event 172 of 174: guess   term 4   clock 52`). Under the
slider two strips (G-042, G-043): the markers, a tick per term [M86 p. 164], a diamond per
guess, green once answered `ok` and red once `no` [M86 pp. 166, 175], an orange triangle per
refutation [M86 p. 168], a purple diamond per slip of the reigning class [M86 pp. 99, 174]
and a black bar per blast [M86 pp. 75, 92]; and the parse strip, the parse at each term
boundary as `show-parse` prints it, `()`, `(((1 2)))`, `((1 2) (1))`, `((1 2) (1 2 3))`
[M86 pp. 174, 178], the time-lapse of Hofstadter's Figure 1-1 [FCCA p. 41] and of the
mountain chain solved by successive bracketing [FCCA pp. 64-65].

**How to read it.** While a run is live the slider grows and cannot be dragged. When no run
is live (paused after Stop, finished, or a replay that has ended) every pane follows the
slider: a seek rebuilds every model from the history at the target and draws once, and
shows what the live run showed there, since the history keeps a snapshot of the world at
every task boundary of a short run and at keyframes after (G-003). Click a marker or a
parse cell to seek to it.

### P11. Controls

**What it shows** (the Numbo GUI's `controls.py`, adopted; G-049): the preset list with the
three appendix runs on their golden seeds with Meredith's exact console lines, commands
included, and her answers (`3 7 3 7` [M86 pp. 164-166], `3 7 3 3 7 3` [M86 pp. 167-169],
`1 2 1 2 3 1 2 3 4` answered `nope` then `ok` [M86 pp. 170-179]), Custom, and every
sequence of the catalogue in its group (the Blackburn dozen [M86 pp. 6-7, 145-147], the
lists of pp. 88, 38 and 150, FCCA's sampler [FCCA pp. 53-54]), each with its citation and
Meredith's reported result under the list; the terms (for Custom); the seed and Random;
the two caps, tasks per term and tasks in all (D-011); the answering mode: interactive
(you type the terms and answer the guesses, as a user presents numbers to the system
[M86 p. 37]), scripted (the preset's terms are fed on request and the guesses answered
from the script, then judged against the terms, D-033, D-053) or the continuation judge
(each guess judged against the sequence's true continuation, the bench's rule, D-070);
the trace, verbose or terse (D-055); the seven run buttons, Play, Pause, Step (one event),
Step task, Step term, Run to end, Stop; and the speed, a delay per event from one second
down to max. An invalid value shows under the buttons and refuses the next start.

### P12. Stats

**What it shows** (the Numbo GUI's `stats_view.py`): the state, the run (preset and seed),
the configuration (default, or the fields edited, ANSWERS.md 5), the events, the clock
(tasks run [M86 p. 41]), the task last chosen, the terms entered [M86 p. 164], the tasks
waiting, the reigning class [M86 p. 99], the threshold [M86 p. 105], the hypothesis's form
and validity [M86 p. 81], the outcome and, at the end, `Session.summary()`, the line the CLI
prints (D-069) (G-016).

### P13. Configuration

**What it shows.** Every field of the engine's `Config` as an editable table (ANSWERS.md 5):
the field, its value, its default, its source (the page tags and decision numbers of the
field's comment in `config.py`, every one of which the engine's own tests require) and
the comment, with a widget suited to the type (a spin box, a decimal spin box, a check
box, or the literal with an editor under the table for the long tables such as the
s-links); edited fields are tinted and bold with Reset per field and Reset all; a filter.
The caps and the trace belong to the controls pane and are shown here disabled (G-051).

**How to read it.** The values shown at first are the defaults, Meredith's numbers where
the sources give them and the decisions' choices where they do not. An edit applies to the
*next* run, never to the one in progress: a run is its seed and its configuration
(D-069). The title and the stats pane say when the next run's configuration differs
from the default; the configuration a run used is saved in its session file and shown
read-only during a replay; the edits are remembered between sessions.

## Sessions and replay

Session > Save session… writes the run as JSON lines, the Numbo GUI's format with a run
line: the seed, the configuration, the inputs (every console line and every answer, as
given) and every event (G-046); a run can be saved before it ends. Open session…
re-executes the file's run through the engine, from its seed, configuration and inputs,
and compares each event with the stored one as it is drawn: the replay is verified when
they all agree. If the engine has changed since the file was saved, the first differing
event stops the re-run before it is drawn, a dialog says where, and the stored events are
loaded into the history so the event-built panes still show them (G-047). Close replay
goes back to live runs. An interactive session replays as a scripted one: the file's lines
and answers stand in for the user.

## Changing the default layout

View > Reset layout restores the layout shipped with the code, `seekwhence/gui/default_layout.json`;
View > Save layout as default for this code… overwrites that file with the arrangement on
screen, and committing it shares it with everyone who uses this copy of the code (ANSWERS.md 8).
Your own layout, saved when the window closes, takes precedence on your machine; View >
Reset to built-in layout ignores both. A layout saved by another version of the window's
docks (the layout bytes carry a version) is ignored with a note in the status bar, and the
next layout in that order is used (G-059).

## Screenshots

`python3 scripts/docs_screenshots.py` (from `python/`) renders the four pictures of this
guide offscreen, each under 200 KB, from the appendix presets on their golden seeds, so
they come out the same at every regeneration (G-056):

| File | What it shows |
|---|---|
| `docs/gui-window.png` | the whole window at the guess of run 1, `3 7 3 7` [M86 p. 166], scaled to 1000 px wide |
| `docs/gui-cytoplasm.png` | the cytoplasm at the end of run 2 [M86 pp. 167-169] |
| `docs/gui-reformulation.png` | run 3's storyboard at its first candidate, `(S-group 1 2)` [M86 p. 174] |
| `docs/gui-platoplasm.png` | run 3's slip, `Countup --> S-group` [M86 p. 174], the crown half-way along the s-link |

The final review of the GUI against its design, with every departure from PANES.md and every place the Numbo GUI is followed instead, each with its decision, is `docs/gui/review.md`.
