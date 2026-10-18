# The GUI's panes, and the pages behind them

The user guide of the Tabletop GUI (`python3 -m tabletop.gui`, or `tabletop-gui` once
installed with `pip install -e '.[gui]'`). The GUI watches the Python reconstruction of
Robert French's Tabletop (1992) answer a table, one event at a time: Henry touches an
object, the Slipnet lights "opposite" and the touched type, codelets look over the table,
groups and correspondences are proposed, tested and fought over, the Worldview settles, the
temperature moves, and a find-answer codelet touches an object on Eliza's side. The GUI
drives a run and observes it, and nothing else: the engine is not changed, draws no random
number from the GUI, and gives the same run for a seed with or without the window (the
loop's gate checks it on every iteration; `tabletop/gui/headless.py` is the contract).

Every pane says which pages of the sources justify what it draws. The tags are the
engine's: `[F95 p. N]` is French's *The Subtlety of Sameness* (1995), printed page N, and
`(img)` marks evidence that is a figure, a dump or a bar graph; `[FCCA p. N]` is *Fluid
Concepts and Creative Analogies*, chapters 8 and 9; `[FH91 p. N]` is French and Hofstadter
(1991). "Numbo GUI" is the precedent followed for every convention, the Numbo GUI in
`Software/Numbo/python/numbo/gui/`, and "Seek-Whence GUI" the closest worked example, a GUI
over a frozen reconstruction (`Software/SeekWhence/python/seekwhence/gui/`); `D-NNN` is a
decision of the engine's [`DECISIONS.md`](../../DECISIONS.md), `G-NNN` one of the GUI's
[`GUI-DECISIONS.md`](../../GUI-DECISIONS.md).

French showed one screen: the palette, the table, the thermometer, the Slipnet grid and the
Coderack strip, Fig. 4.1 and the dumps of the chapter 4 run [F95 pp. 95-107 (img)]. Five
panes have no figure in the sources and are **GUI design, not French's**: the temperature
plot (P2b), the Slipnet network (P3b), the rack list (P4b), the Worldview (P5) and the
competitions (P6). Two more carry drawings of the GUI's inside a pane of French's: the
table's overlays and Eliza's view (P1) and the editor's canvas (P7). Each says so, in the
pane, in its tooltip and in its row below (ANSWERS.md 4). The icons of the objects are
**new drawings, not French's**, drawn in the spirit of his palette [F95 p. 26 (img)]; the
palette, the table and the legend say so. Nothing of French's is reproduced.

Help > "What am I looking at?" shows the table below, generated from this file
(`tabletop/gui/models/guide.py` reads the table back; a test keeps the file's table and
the model's equal, so the pane, its tooltip and this guide are one set of words;
G-055).

## Starting

```text
python3 -m pip install -e '.[gui]'                   # PySide6 6.6 or later (tested with 6.11)
python3 -m tabletop.gui                              # or: tabletop-gui
python3 -m tabletop.gui --problem surround-05 --seed 3 --play
python3 -m tabletop.gui --problem buridan-01 --profile human --set max_codelets=400
python3 -m tabletop.gui --problem surround-05 --seed 3 --smoke
```

Without PySide6 the engine, its tests and the command line work as before, and
`python3 -m tabletop.gui` says what to install. The arguments are the CLI's: `--problem
NAME` (a name as `python3 -m tabletop --list` prints them), `--seed N`, `--profile NAME`
(`standard`, `human`, `buridan`, `french-faults`) and `--set FIELD=VALUE` (repeatable, a
`Config` field for the next run, parsed by the CLI's own code), plus `--play`, which starts
the run at once, and `--smoke`, which runs the inputs to the end offscreen with every event
drawn, prints French's report line (the last line of `python3 -m tabletop NAME --seed N
--quiet`) and exits 0 when the run ends with an answer, 1 when it is capped, 2 for a usage
error, reading and writing no settings (G-054). The inputs given on the command line win
over the remembered ones. The window's size, docks, inputs, speed, overlays and edits are
remembered between sessions (QSettings `tabletop` / `tabletop-gui`, as the Numbo GUI's).

The first run to try is the chapter 4 configuration, `surround-05`, on seed 3: since v5's
structural Blockage fix (D-116 to D-120) it ends with Eliza touching `cup2` (E2), structure
72, 384 codelets, T 28.3, the quick start of the [front page](../../README.md). Seed 1 runs
228 codelets and 2,690 events. (On v4's defaults before the fix, D-115: `cup2` (E2),
structure 88, 305 codelets, T 17.8; seed 1, 304 codelets and 3,174 events. On v3's, D-114:
`small-glass1` (E1), 144 codelets, T 31.0; seed 1, 118 codelets and 1,355 events. On v2's:
`cup2` (E2), 44 codelets, T 41.0; seed 1, 62 codelets and 901 events.)

## The window

The window is the Numbo GUI's: one main window, a central area and one dock per pane, each
with a tick in the View menu, and the timeline as a toolbar along the bottom. The built-in
layout keeps French's screen as its starting point [F95 pp. 95-97]: the palette to the left
of the table, the thermometer immediately to its right, the Slipnet grid to the right of the
thermometer, the Coderack below the table. The rest goes in tabs:

| Area | Docks (top to bottom; "/" = tabbed) |
|---|---|
| left | Controls (P9); Palette and editor (P7); Stats (P12) |
| centre | Table (P1), the central widget, with the overlays' toggles on its own bar |
| right, inner | Thermometer (P2), narrow, the table's height |
| right, outer | Slipnet grid (P3) / Slipnet network (P3b); Worldview (P5) / Competitions (P6) / Temperature plot (P2b) / Roles (P14) / Configuration (P13, hidden until Edit > Configuration) |
| bottom | Coderack bars (P4) / Rack list (P4b); Trace and event log (P10) / Results (P8) |
| bottom toolbar | Timeline (P11) |

The window fits 1280 x 800 with every dock shown (G-014): a dock made smaller than its pane
clips or scrolls it. Float a dock (double-click its title) to enlarge a pane, the Slipnet
network and the editor especially.

**The menus and keys** (Numbo's, plus Edit and Problem; PANES.md section 4):

| Menu | Actions |
|---|---|
| Session | New run (Ctrl+N); Open session… (Ctrl+O), Save session… (Ctrl+S), Close replay; Save trace…, Save events as JSON lines…; Quit |
| Edit | Configuration (opens P13); Edit table (raises P7) |
| Run | Play / Pause (Space), Step (Right; when no run is live, one event forward in the history), Step codelet (Shift+Right, to the end of the current codelet), Back (Left, one event back when no run is live), Run to end, Stop (Esc); Speed; Batch… (P8) |
| Problem | the 53 problems of the library by family, each with its page; the chapter 4 configuration first |
| View | a tick per dock and for the timeline; Table overlays (regions, salience, ranks, roles) and Eliza's view; Temperature plot (the terms, French's chapter 4 thermometer); Slipnet grid (French's chapter 4 grid); Coderack (the legend); Save layout as default for this code…, Reset layout, Reset to built-in layout |
| Help | What am I looking at? (the table of this page); About |

**What a step is** (ANSWERS.md 5). Step is one engine event, so exactly one change is
drawn: a codelet chosen, a codelet posted, a description added, a group proposed, a
temperature update. Step codelet runs to the end of the current `World.step()`, from the
`CodeletChosen` to the temperature update and the find-answer posting that close it, so a
step codelet shows one codelet's whole work. Back is one event back in the history, when
no run is live.

**Every event is drawn.** After each event the run waits until every pane on screen has
redrawn, so a step of one event shows exactly one change (the Numbo GUI's rule; G-011). The
speed is a delay per event, from one second down to max, which means as fast as the window
can draw. A pane's picture at an event is a function of the events up to it and the
engine's snapshot in force, so a seek to that event, a replay and a fresh window show the
same picture (G-017, G-020).

**One selection for every pane** (G-022). A click on an object, a group, a proposal or a
correspondence on the table, on an entry of the Worldview pane or a fight of the
competitions pane, or on a node of the Slipnet grid or network, puts the thing into one
shared selection, and every pane that shows it highlights it in orange: the table its
item, the Worldview pane its lines, the grid and the network the node. A second click
clears it, as does a click on empty canvas. The status bar names what is selected.

**The dynamics, as they are** (ANSWERS.md 11). The GUI shows runs honestly: an empty pane
says so, nothing is smoothed or tuned. See "What to expect" below before reading a short
Worldview pane as a defect.

## The panes

| Pane | Name | What it shows | Pages | Drawing |
|---|---|---|---|---|
| P1 | Table | the table from above, Henry's side above and Eliza's below, the objects as icons, the touch (H) and the answer (E), the groups, the proposals and the correspondences as the dumps draw them; overlays for the regions, the salience and the region ranks, and Eliza's view | Fig. 4.1 and the dumps [F95 pp. 96-107 (img)]; the palette [F95 p. 26 (img)]; icons: new drawings, not French's; the overlays (regions, salience, region ranks) and Eliza's view: GUI design, not French's, from the prose [F95 pp. 65-67] | Figs. 4.1-4.9 and SPEC.md 6c.14; the overlays and Eliza's view are GUI design, not French's |
| P2 | Thermometer | French's column, filled to the temperature, with the value; grey while held | Fig. 4.1 [F95 pp. 95-96] and the dumps [F95 pp. 98-107 (img)]; the grey band while held, the reason tick, the scale and the age are the GUI's | Fig. 4.1; the held colour, the tick, the scale and the age are the GUI's |
| P2b | Temperature plot | the temperature over the codelets with its three terms, the competitions and the changes marked, and French's chapter 4 readings on request | GUI design, not French's, from the formula [F95 p. 91 (img)]; French's chapter 4 thermometer beside the run's [F95 pp. 98-107 (img)] | GUI design, not French's |
| P3 | Slipnet grid | French's grid of 41 boxes, each with its number and a black square for the activation, and a seventh row for the nodes the screen lacks | [F95 p. 95] and the dumps [F95 pp. 96-107 (img)]; the seventh row (the six object nodes the screen lacks, and isolated, D-007) is the GUI's, not French's; French's chapter 4 grid beside the run's on request | Fig. 4.1 and the dumps; the seventh row is the GUI's |
| P3b | Slipnet network | the nodes and links at their current lengths, with the labels that shorten them, and the context push and pop | GUI design, not French's, in Fig. 3.6's style [F95 p. 61 (img)] | GUI design, not French's, after Fig. 3.6 |
| P4 | Coderack bars | French's strip: a bar per codelet type with the percentage waiting, and the count of codelets run | [F95 p. 97] and the dumps [F95 pp. 98-107 (img)]; the catalogue legend (D-051) on request | Fig. 4.1 and the dumps |
| P4b | Rack list | the codelets waiting, oldest first, with their urgencies and chances, and the one chosen | GUI design, not French's, after the Numbo GUI's coderack_view.py | GUI design, not French's |
| P5 | Worldview | the correspondences in the Worldview and the shadow cabinet with their points, the rating and its pairs, and the proposals | GUI design, not French's, from the text of [F95 pp. 67-74] | GUI design, not French's |
| P6 | Competitions | each competition as the Old Worldview beside the Rival Worldview, with the ratings and the chance | GUI design, not French's, after Fig. 3.11 [F95 p. 72 (img)] | GUI design, not French's, after Fig. 3.11 |
| P7 | Palette and editor | Fig. 2.1's column of types with the counts, and a canvas for setting a table to run | the palette [F95 p. 96 (img)]; the editor is GUI design, not French's | Figs. 2.1 and 4.1; icons: new drawings, not French's; the editor is GUI design, not French's |
| P8 | Results | a batch of runs of the problem, then French's bar graph and table of frequency, average structure and average codelets per answer beside ours | the chapter 5 layout [F95 pp. 115-139] | the bar graphs and tables of chapter 5 [F95 pp. 115-139 (img)] |
| P9 | Controls | the problem, the profile, the seed, the run buttons and the speed | Numbo GUI, controls.py | Numbo GUI |
| P10 | Trace and event log | the text trace as the command line prints it, and every event as a row | Numbo GUI, log_view.py | Numbo GUI |
| P11 | Timeline | the slider over the run's events with the markers and a strip of the temperature | Numbo GUI, timeline.py | Numbo GUI |
| P12 | Stats | the run's numbers as they stand, the invariants' verdict and French's report line at the end | Numbo GUI, stats_view.py | Numbo GUI |
| P13 | Configuration | every field of the configuration with its comment, the profile's value and the edits for the next run | ANSWERS.md 3 (Edit > Configuration): every field of Config with the comments of config.py (D-066) and French's faults marked; Seek-Whence GUI, config_view.py | Seek-Whence GUI |
| P14 | Roles | the role tags of the run by family, each with its evidence, weight, readers and, once lost, when and why; the axis and the re-cast's tiers | GUI design, not French's, a re-interpretation (the roles loop's ANSWERS.md): the roles have no figure in French, who names the loner, the sidekick and the figure in the prose [F95 pp. 118-140] (D-078, D-079); the families and codes of the registry | GUI design, not French's, a re-interpretation |

### P1. Table

**What it shows.** The table seen from above, as French's screen and the nine dumps of the
chapter 4 run show it [F95 pp. 96-107 (img)]: Henry's side above with his name, Eliza's
below with hers, a faint centre line between them, and each object as its icon, turned to
its orientation and sized by its footprint (D-001 to D-003). The touched object carries an
arrow with an **H**; once Eliza answers, her object carries an arrow with an **E**. The
structures are drawn by the dumps' conventions (SPEC.md 6c.14, transcribed from the figures):

- a **gestalt group** is a dashed wavy box around its members, a bottom-up **subgroup** a
  dashed rectangle; a new subgroup wears the dumps' caption "Build new subgroup" for two
  codelets [F95 p. 102 (img)];
- a **proposal** (a group being built in stages, D-024) is a faint dotted outline with a
  dot per stage passed; a failed proposal and a removed group fade for three codelets and go;
- a **correspondence** is a line between the ends' centres: dashed and flashing while
  tentative or competing (French calls the dashed line a flashing one on his screen
  [F95 p. 105]; here the flash is a two-phase dash offset driven by the event index, never
  by a timer, so a picture is the same at every redraw), solid once in the Worldview,
  thicker between groups, faint when it lost a competition or was removed; a new
  correspondence boxes its two ends for two codelets;
- a **noticed object** (`ObjectNoticed`) wears a light ring.

Before a run starts the table already shows the problem chosen for the next run, bare:
the objects and Henry's touch, no ring, no structure, no E (G-064). It changes when you
choose another problem (the selector, the Problem menu, the command line) while no run is
live; a new profile or seed after a finished run keeps that run's table.

The legend on the bar says the icons are new drawings, not French's, and names the pages.

**The overlays** (GUI design, not French's; the View menu's Table overlays, or the bar's
toggles; G-024 to G-026). The engine's own view of the table is in the prose
[F95 pp. 65-67] and has no figure, so three layers draw it as the GUI's own design, and
a fourth draws the roles, which French's program has not:

- **Regions**: the nine blurry regions (D-004) as cells with soft bands along their
  boundaries, as wide as the profile's blur, the engine's names in the corners, and, for
  every noticed object, the region it was last drawn in;
- **Salience**: a warm tint over every object, darker the more salient (`salience.saliences`,
  D-031), the value above it; hover an object for its nine terms, French's eight in his
  order and the roles' "role" term after them;
- **Ranks**: the nine regions ranked from the touched object (`salience.region_order`,
  D-032), the rank in the cell's centre and its weight in the corner;
- **Roles** (a re-interpretation, the roles loop's item 14; G-061): the standing role tags
  (P14) as a badge with the registry's codes at the top-right corner of each object and
  group ("L2 A1 C3"), the midline band (S1) with the axis word (G3), a bracket across each
  gap (S2), a dashed arrow from the selected object (the touched one while nothing is
  selected) to its mirror and point images (G1, G2), the re-cast's tier under each
  candidate (K1), a bar on a barrier with a dashed line to the counterpart it claims (S5,
  S6), and a ring at the middle of a correspondence a role consumer proposed (D-095); hover
  an object for its roles and their readers. Exact at every event, dated by none.

Every overlay is dated: the legend says "as of codelet N", the snapshot it was read from,
and "not read yet" before the first. **Eliza's view** turns the whole table through 180
degrees, as she sees it, names swapped; the engine has no notion of a side beyond the
regions (D-005), so this is the GUI's transform only.

**How to read it.** Watch the dashed lines: the one that turns solid is the Worldview, and
the E arrow lands at its far end in most runs. Hover an object for its id, type, centre,
half, footprint, last region, descriptors and whether Henry touched it or Eliza answered
with it; hover a correspondence for its status, its rating "as of codelet N", the nine
static and three dynamic terms, its slippages, wins and losses; hover a group for its kind,
members, strength and frame. Click anything to select it in every pane. The bar has Fit,
zoom in and out and 1:1; wheel or + and − zoom, drag pans, F fits, 0 is 1:1 (the Numbo
GUI's canvas; G-019). The end state stays drawn after the run ends: the answer arrow and the
Worldview line are there to read.

### P2. Thermometer

**What it shows.** French's thermometer, "Immediately to the right of the table is the
thermometer" [F95 p. 95], a tall narrow column filled black from the bottom to the
temperature, the value printed beside the fill's top to one decimal, as the dumps print it
(80.0, 59.0, 43.2 ... 2.0 [F95 pp. 98-107 (img)]). What the GUI adds, said in the tooltip
(G-027): the fill is grey while T is still held at its initial value, before the first
Worldview change (D-049); a small tick at the fill's top names the last update's reason
(`schedule`, `competition`, `change`, `start`); faint scale marks at 0, 50 and 100; under
the column the reading's codelet and its age, "at 45 (+1)", since the temperature is
updated on a schedule and the value at the answer is the last update's, up to four codelets
old (D-050). Before the first reading the column is empty, as at 0 codelets.

**How to read it.** Grey means nothing has happened yet to the Worldview; the jump to black
near 96 is the first Worldview change, and the slow fall afterwards is the structure
settling. The plot (P2b) shows the same readings over time with the formula's terms.

### P2b. Temperature plot

**GUI design, not French's.** French gives the formula, T = α ΔQ/Q + β Δ_WV − γ T_cp
[F95 p. 91 (img)], and shows the thermometer at nine moments of one run, but no plot of the
temperature over time; the plot is the GUI's own (G-028). The pane says so on its bar and in
its tooltip.

**What it shows.** Three bands over the codelets run: T on 0 to 100 with a dot per update;
Q and ΔQ on 0 to 1; Δ_WV and T_cp as counts. Competitions are dashed magenta lines across
the bands and Worldview changes solid green ones. The **terms** option (the View menu's
Temperature plot, or the bar) stacks the three terms of the formula as areas whose net is
the T line wherever the formula applied (it does not while T is held, D-049, or when the
configuration fixes T). The **French** option, offered for `surround-05` only, sets French's
chapter 4 thermometer beside the run's as hollow red diamonds labelled with their figure:
80.0 at 2 codelets, then 59.0, 43.2, 31.3, 35.8, 13.6 and 2.0 at 22, 33, 36, 47, 47 and 55
[F95 pp. 98-107 (img)] (`tests/data/ch4.json`, [`docs/ch4-run.md`](../ch4-run.md); G-029).

**How to read it.** Hover a point for the reading's numbers and terms. French's curve falls
from the start; ours holds at 80 for thirty to forty codelets and then jumps, which is the
reconstruction's behaviour, not a drawing choice (RESULTS.md §6.4; see "What to expect").

### P3. Slipnet grid

**What it shows.** French's grid of square boxes to the right of the thermometer, 41 boxes in
six rows [F95 p. 95] (`slipnet_def.SCREEN_GRID`), each with the node's label along the
bottom edge, its activation as "the number in the upper right-hand corner" and as "the
black square within the box" [F95 p. 95], the square's side growing with the activation from
nothing at 1 to the whole inner box at 150 (G-031), as the dumps draw them
[F95 pp. 96-107 (img)]. A **seventh row**, dashed grey under the caption "GUI design, not
French's", holds the six object nodes the screen lacks (saucer, plate, saltshaker, pepper
shaker, big glass, soup bowl) and `isolated` (D-007), so that every node of the
reconstruction's Slipnet is on screen. Under it, the **roles' band**, dashed violet under
the caption "not on French's screen (re-interpretation): the 15 role nodes (D-080)" (the
author's answer C7 of the roles loop; G-062), holds the role nodes in rows of seven. A box
jolted by the event shown (`NodeJolted`) is
outlined in magenta for that one event; the hover text keeps the last jolt with its amount
and what caused it. The **French** option (the View menu's Slipnet grid, or the bar), offered
for `surround-05` only, prints in red, in each box's lower corner, the activation of the
chapter 4 dump nearest in codelets, with a note naming the figure (G-032).

**How to read it.** At the touch, "opposite" and the touched type light up and the rest sit at
1; the squares then spread along the links and decay. Hover a box for the activation to a
tenth, the node's level and depth, its place in French's grid, the last jolt, and every
link of the node with its intrinsic and effective lengths "as of codelet N". Click a box to
select the node; the network (P3b) highlights it and its links.

### P3b. Slipnet network

**GUI design, not French's.** French draws the Slipnet as the grid of boxes and, once, two
nodes with the pathways between them, Fig. 3.6 [F95 p. 61 (img)]; there is no figure of the
whole network with its lengths. The pane draws the network in Fig. 3.6's style as the GUI's
own design (G-033, G-034), and says so in its label and its tooltip.

**What it shows.** The 63 nodes (French's 48 and the 15 role nodes, D-080) in three bands by
level, the most abstract on top, each as a
circle with a dark disc for its activation (the grid's scale), the role nodes with a dashed
violet ring and the 29 links the roles add in violet, as the caption says (G-062); the ISA links as thin grey
arrows pointing at the category; the labeled links darker and thicker as they shrink, the
effective length printed at the midpoint (D-011, D-012), blue once shortened below the
floor; the identity links as loops; a dashed stem from each label node to the link it
labels, green while that label is the one shortening it (Fig. 3.6 ties the label to the
pathway the same way). **The context** [F95 pp. 87-89]: while a correspondence's subworld is
pushed (`ContextPushed`), the nodes reset to 1 wear a dashed magenta ring and the banner
along the top says which correspondence and since which codelet; at the pop
(`ContextPopped`) the restored nodes wear a blue ring through the pop's codelet and the
banner says what was reinitialized, how much was spread and how much fixed (D-021). Until a
push happens the banner says **"no context push in this run"**, which is what it says in
almost every run (ANSWERS.md 11; G-035).

**How to read it.** The shortened links are the slippages the run can make cheaply: at the
touch, "opposite" shortens the three links it labels (open against closed object,
left against right, above against below).
Hover a node for its activation, level, the links it labels and which it shortens, and what
a push or a pop did to it; hover a link for its lengths. Click a node to select it; the grid
follows. One recorded seed shows a push and a pop in an ordinary run, `ch2-fig11` seed 3
(G-035; one pair since v5's fix, D-116 to D-120, four pushes and four pops from item 09's runs to v4; `ch2-fig08` seed 2 before the FIGURE, GROUND, ECHO, TWIN and TYPE_KEY detectors of item 09, `ch2-fig12` seed 16 before the GAP, LONER, STRAY and CORNER detectors of item 08, `surround-07` seed 13 before the ROW and COLUMN detectors and the re-cast of item 07, `surround-04` seed 20 before the four role detectors of item 05, `blockage-08` seed 111 before the geometric family, `blockage-07` seed 116 before the roles); edited configurations that lengthen the runs (`answer_post_rate` 0, `max_codelets`
400) make pushes common past codelet 150.

### P4. Coderack bars

**What it shows.** French's Coderack strip under the table, a bar graph of the percentage of
the rack each codelet type occupies, the early types on the left and the find-answer
codelets on the right, with French's abbreviated labels under the box and the number of
codelets run beside it [F95 p. 97] [F95 pp. 98-107 (img)]. Each bar is as high as the
type's share of the rack (`Coderack.shares()`), the percentage printed above it; the labels
sit on two staggered lines, since 29 labels fit one line nowhere (G-037). The bar of the
codelet just chosen is outlined in magenta from its choice to the next. The **Legend**
button (also under the View menu's Coderack) lists the 29 types (the legend's 23 and the six
role codelets, D-081) with their labels, bands and jobs from `codelets.CATALOGUE`, and whether
each job is from the text, inferred, or the re-interpretation's (D-051, D-081).

**How to read it.** The strip is what the program might do next: the taller the bar, the
likelier the next draw falls on that type. Hover a slot for the type's catalogue entry and
its share. Compare with the dumps: at the start the rack is all look-around codelets; late in
a run the answer bar grows.

### P4b. Rack list

**GUI design, not French's.** French describes the Coderack and its urgencies in the text
[F95 pp. 77-82] and shows it only as the strip; the list is the GUI's own, after the Numbo
GUI's coderack view (G-036, G-038). The pane says so in its label and its tooltip.

**What it shows.** On a band at the top, the codelet just chosen as the trace prints it, with
the chance it had at the draw, the exponent `a` the temperature set and the rack size
(D-060), its urgency and generation, who posted it and when, and what it did: running,
fizzled with its reason, or the structures it posted. The band is tinted magenta at the
event of the choice only. Under it a summary (how many wait, the most this run, the total
urgency) and one row per codelet waiting, the oldest first: its number, type and arguments,
raw and adjusted urgency, generation, when it was posted, its age and its chance at the last
draw (D-052, D-053). A codelet the event shown removed, aged out or over capacity, stays in
grey at its old place for that one event.

**How to read it.** When several codelets on the rack are alike in type, arguments,
generation and urgency, the events cannot say which twin the engine drew, and the band says
"one of N alike on the rack" rather than guessing; the list is put right at every snapshot
(G-036). Hover a row for the codelet's text.

### P5. Worldview

**GUI design, not French's.** French describes the Worldview, the shadow cabinet, the rating
of a Worldview and the fight between a newcomer and its rivals in the text [F95 pp. 67-74]
and shows the Worldview only as the solid lines on the table; the per-correspondence points
and the pairs have no figure. The pane is the GUI's own (G-039, G-040), and says so in its
label and its tooltip.

**What it shows.** First a banner in plain words: "no correspondence in the Worldview yet",
"one correspondence; no pairs to rate", "2 correspondences; 2 pairs rated" (ANSWERS.md 11).
Then the correspondences in the **Worldview**, each with a solid bar before it, and those of
the **shadow cabinet**, built but outside the Worldview, each with a dashed bar (Fig. 3.11's
solid against dashed [F95 p. 72 (img)]): two lines per entry, the ends (with "(groups)" for
a group-level one), the rating as its static, dynamic and bonus parts (D-031), wins and
losses; then its standing, its slippages, and its "rival of", "incoherent with", "coherent
with" and "parallel to" partners, an incoherent pair in a warning colour (rivals against
incoherent pairs [F95 p. 105]; D-044). The **rating** block gives the Worldview's strength,
coherence, parallelism and total (D-042, D-043) with its coherent, incoherent and parallel
pairs listed. The numbers are the engine's snapshot in force, dated "as of codelet N"; the
membership follows the events exactly. The **Proposals** tab lists every bottom-up group
proposal with the stage it reached, failed, or built as which group; it is not a queue: a
proposal whose codelet aged out stays where it stopped (D-024). A correspondence a role
consumer proposed (D-095) says so in its second line, "role-proposed (C1 ROW by
propose-role-cp)", and the table draws a ring at its middle (G-061).

**How to read it.** In most runs the pane holds one entry and the banner says there are no
pairs to rate; that is the run, not the pane (see "What to expect"). Hover an entry for the
correspondence's full text with its rivals; click to select it on the table. The lines of the
event shown are tinted.

### P6. Competitions

**GUI design, not French's.** French draws one competition, Fig. 3.11, a Worldview
correspondence solid against a dashed rival [F95 p. 72 (img)], and describes the fight's
chance in the text; the pane draws every fight of a run in that convention as the GUI's own
design (G-041), and says so in its label and its tooltip.

**What it shows.** Each competition (`CompetitionHeld`) in order, as a heading over two small
tables side by side: the **Old Worldview**, the correspondences in the Worldview before the
newcomer, solid, and the **Rival Worldview**, the old less the newcomer's rivals plus the
newcomer, dashed; under them the two ratings, then "chance 0.417 at a 3.12: the Rival
Worldview won; rating after 73.4", the chance and the exponent of the fight and the rating
the following change reports. An entry that met no rival is one line, "entered the
Worldview without a fight", with the rating before and after (D-040). When there has been
none the pane says **"no competition in this run"**, which is common (ANSWERS.md 11).

**How to read it.** The fight the event shown reports is tinted for that one event; click a
fight to select its newcomer on the table and in the Worldview pane. The blue diamonds and
magenta bars of the timeline (P11) are the same correspondences and fights.

### P7. Palette and editor

**What it shows.** The palette as French's screen shows it: a column of the eleven object
types in Fig. 2.1's order [F95 p. 26 (img)], the silverware drawn in its four orientations,
with a small box beside each cell holding the count of that type on the table (Fig. 4.1
puts the counts beside the cells [F95 p. 96 (img)]; G-051), and the screen's buttons in two
rows: **Load config**, **Clear table**, **Save config**, **Do this!** and **Run it again**
(the screen's Exit is the window's Quit). The icons are new drawings, not French's. Under the
buttons, the table's name, title and family; beside the strip, the **editor's canvas**, which
is GUI design, not French's: the table being set, with its objects movable and the H and E
arrows; under it the messages, `validate`'s in red, the realism faults in orange with their
page (`tables.realism_faults`, French's rule for a table he would set [F95 p. 38]), and a
green line when French would set it.

**How to use it.** Click a cell to add one of its type at the centre of Henry's side, or drag
it onto the canvas; drag an object to move it (its footprint is kept inside the table);
double-click or press H to make it the touched object; press E to mark it an expected answer
(`E1`, `E2`, ...; the file format needs at least one, and the report line names Eliza's
answer by its label), R to turn silverware, Delete to remove it; the context menu has the
same. The table is validated as you edit (`problems.validate`); **Do this!** runs it, on the
next run's seed and configuration, when it validates, and **Run it again** repeats it on the
next seed; the title, the controls and the stats say "edited". A table with realism faults
can still run. **Save config** writes a problem file in the format of `docs/problems.md`,
anywhere but into the library's `problems/`; **Load config** reads one back, the library's
own files included; `Clear table` empties the canvas. Editing is refused while a run is live,
and the line along the top says so (G-050, G-051). Edit > Edit table raises the dock.

### P8. Results

**What it shows.** French's chapter 5 layout for the problem on screen [F95 pp. 115-139
(img)]: a batch of N runs (fifty by default, French's count [F95 p. 114]) on a worker with a
progress bar, then the bar graph, French's bars black and ours grey beside them, the
percentage above each bar and the answer's label under it, the configuration's title and
his page under the graph, and the table under his three headings, the object Eliza touches,
the average structure, the number of codelets run, his column beside ours with the
chi-square in the footer (G-052). Answers French prints no bar for are grouped as "other",
as RESULTS.md's tables group them. When French printed several sets for a problem (the
book's and FCCA's reprint, or Buridan 1's modified program) a selector chooses the set.

**How to read it.** The batch takes the next run's problem, profile, edits and seed, and its
results equal `python3 -m tabletop NAME --batch N --seed S` run for run, so a batch here is
a batch there. A batch and a live run exclude each other: Run batch is off while a run is
live, and a run started during a batch is refused with "busy" in the status bar. Cancel
stops between two runs and keeps what was run. Hover a bar for its row. Run > Batch… raises
the pane and starts one. The reconstruction's numbers against French's for every problem,
and why they differ, are in [`RESULTS.md`](../../RESULTS.md) (§2, §8).

### P9. Controls

**What it shows** (the Numbo GUI's `controls.py`, adopted; G-044): the problem selector with
every problem of the library by family, each with its title and the page of its figure, the
chapter 4 configuration `surround-05` first; the profile (`standard`, `human`, `buridan`,
`french-faults`, one state with the configuration pane's selector); the seed; the cap,
`max_codelets`, read-only here and editable in the configuration pane; the seven run
buttons, Play, Pause, Step, Step codelet, Run to end, Stop and Back; and the speed slider
over Numbo's delays. An invalid seed shows under the buttons and refuses the next start.
Along the top a line says the controller's state and the events shown ("paused: 23 events
shown", "finished: 496 events").

**How to read it.** The selector, the window title and the stats pane's Run line always
agree with the run actually started, whichever way it was started: here, from the Problem
menu, from the command line, from a session file or from Do this!. The same seed on the
same problem and configuration repeats the run exactly, with or without the window.

### P10. Trace and event log

**What it shows** (the Numbo GUI's `log_view.py`, adopted; G-042). The **Trace** tab is the
text trace exactly as `python3 -m tabletop NAME --seed N` prints it, produced by the
engine's own `TextTrace`: the start's lines, one line per codelet with its follow-ups
indented under it, and French's report line at the end (`docs/trace-format.md`); **Quiet**
switches to the `--quiet` form. Every line is tagged with the first event of its block; the
lines after the position are greyed when you look into the past, the current block is
tinted, and a click on a line seeks to its block when no run is live. Follow keeps the last
line in view; Save trace… writes the text (also Session > Save trace…). The **Events** tab
is Numbo's event log: one row per engine event with its index, kind, clock and fields, a
Kinds menu over the 27 event names with Show all and Hide all, Follow, and Save as JSON
lines…, which writes the history byte for byte as `python3 -m tabletop NAME --seed N --json
-` does. The roles are in both as the engine writes them (G-062): the trace's `RL` lines
under their codelet (`RL tag`, `RL used`, `RL drop`, `RL recast`; `docs/trace-format.md`)
and the four role events (`RoleTagged`, `RoleConsumed`, `RoleDiscarded`, `RoleRecast`,
D-105) in the events tab.

**How to read it.** Read the trace beside the dumps' codelet counts: the codelet's line
appears when the next codelet is chosen, as the CLI prints it. Double-click a row of the
events tab, or press Enter on it, to seek there.

### P11. Timeline

**What it shows** (the Numbo GUI's `timeline.py`, adopted; G-043). A slider over the whole
history with one event back and forward, the position ("event 202 of 496: WorldviewChanged
codelet 18") and a Go to codelet box. Under the slider, the markers on two rows: the first
object noticed as a grey tick, the first find-answer posting as an orange tick, a context
push as a purple triangle and a pop as one pointing down, each role change (a tag lost to
a rival, broken or gone with its structure, and each re-cast) as a violet dot (G-062); each
correspondence built as a
blue diamond, each Worldview change as a green diamond, each competition as a magenta bar
(an entry without a fight says so in its tooltip) and the answer as a black bar. Under the
markers, a thin strip of the temperature, grey while held (D-049).

**How to read it.** While a run is live the slider grows and cannot be dragged. When no run
is live (paused after Stop, finished, or a replay that has ended) every pane follows the
slider: a seek rebuilds every pane from the history at the target event and draws once, and
shows what the live run showed there, since the history keeps the engine's snapshot after
every codelet of a run up to 300 codelets and at keyframes after (G-007). Click a marker or
the strip to seek to it; hover a marker for what it is.

### P12. Stats

**What it shows** (the Numbo GUI's `stats_view.py`, adopted; G-045): the state and the source
(a live run, or a replay and what it verified), the run (problem and seed), the
configuration (the profile and the fields changed from `standard`, in the edited colour),
the events, the codelets run, the draws of the engine's generator, the codelet last chosen,
T with "held" and the reason, Q, the Worldview's names and its rating as it stands, the
structures built and removed, the competitions, the changes, the fizzles, the answer, the
outcome, the result's fields and French's report line at the end, and the **invariants**:
`invariants.check` (D-077) on the state in force, "none violated (as of codelet N)" or, in
red with one line per sentence, what is violated, an engine finding never hidden.

**How to read it.** The report line equals the CLI's last line. A stopped run says "stopped
after N events" and keeps saying so when you scrub back; a new run forgets it.

### P13. Configuration

**What it shows** (ANSWERS.md 3; the Seek-Whence GUI's `config_view.py`; G-046, G-047). Edit
> Configuration opens the dock, hidden by default: every field of the engine's `Config` as a
table, one row per field, with the field, a widget suited to its type (a spin box, a decimal
spin box, a check box, or the literal with an editor under the table for the long tables),
the selected **profile's** value, the source (the page tags and decision numbers of the
field's comment in `config.py`, every one of which the engine's own tests require, D-066),
the marks (French's seven faults with the value `french-faults` restores, the knobs
calibrated for `human` and `buridan`, where the profile differs from `standard`) and the
comment, read as the engine's own knobs table reads them; the decisions' titles on hover. A
profile selector (one state with the controls'), a filter, Reset per field and Reset all.

**How to read it.** The values shown at first are the profile's defaults, French's numbers
where the sources give them and the decisions' choices where they do not. An edit applies to
the **next** run, never to the one in progress: the next run's configuration is the profile
with the edits over it, the engine's own `Config` replaced field by field, and the engine is
not changed. Edited rows are tinted and bold, the line along the top says what the next run
will use and what the run in progress used, and the title says "(human; edited:
max_codelets)". The configuration a run used is in its session file and is shown read-only
during a replay; the profile and the edits are remembered between sessions.

### P14. Roles

**GUI design, not French's.** A re-interpretation: French's program has no roles, and the
reconstruction's roles (D-078, D-079) are a re-interpretation of what the prose names, the
loner, the sidekick and the figure [F95 pp. 118-140], and nothing of this pane has a figure
in the sources. The pane is the GUI's own (G-060), and says so in its label and its tooltip.
It is tabbed with the Worldview.

**What it shows.** First a banner in plain words: "no role read yet", "12 roles standing on
7 items, 3 past; 2 re-casts". Then the **symmetry axis** the Worldview reads (D-083), the
last **re-cast** of the touched object with its signature and its candidates by tier
(D-093), and one heading per **family** (D-079: geometric, structural, selectors, keys,
content) with the family's standing and past counts. Under each heading come the family's
tags, item by item in the order first tagged, each item's standing tags before its past
ones, two lines per tag: the tag with its name, item, role, arguments, weight, strength, the
codelet that read it and its evidence (the descriptions it was read from), then its readers
("read by C1 at codelet 14: create-cp cup1 small-glass1 (propose-role-cp)", from the
`RoleConsumed` events, D-105) and, once lost, when and why it went: "lost to r11", "broken"
or "structure gone", by which codelet (the author's answer D1 of the roles loop). A
standing tag carries a **solid bar** and a past one a **dashed bar**, the Worldview pane's
convention for the Worldview against the shadow cabinet; the lines of the tag the event
shown built or discarded are tinted, as the rack list's chosen band is.

**How to read it.** The pane is exact at every event: seeking rebuilds it from the role
events, so scrub the timeline to watch a tag appear, be read and go. Hover an item's lines
for every tag it carried, current and past; click them to select the object (or the group
with its members) in every pane: the table shows its badge among the others and its roles
layer's images move to it. The same tags drive the table's **Roles** overlay (P1, G-061),
the roles' band of the grid and the role nodes of the network (P3, P3b), the violet dots of
the timeline (P11) and the `RL` lines of the trace (P10) (G-062).

![The table with the roles layer on, and the Roles pane, at the end of blockage-07 seed 1](../gui-roles.png)

## What to expect

The reconstruction's dynamics, measured in [`RESULTS.md`](../../RESULTS.md), are shown as they
are (ANSWERS.md 11). Nothing is tuned, smoothed or hidden: the GUI is a tool for seeing why
runs miss.

- **The Worldview usually holds one correspondence.** Over 240 runs on four problems the
  Worldview held at most one correspondence in 98 percent of the runs and two in the rest,
  and no rating ever had a coherent or an incoherent pair (RESULTS.md §9.2; also §6.4). So
  the Worldview pane is usually one entry with "one correspondence; no pairs to rate", and the
  competitions pane says "no competition in this run" or lists one fight. Groups and group
  correspondences are almost never built before a run ends (RESULTS.md §8).
- **With the roles (D-078 on), a little more.** On the roles engine the Worldview holds two
  or more correspondences in about 12 percent of runs on the catalogue's originals (v1 under
  4; RESULTS.md §11.5), a fight is held in about half the runs (v1 41 percent; §11.1), and a
  run reads about 27 role tags, most of them at first sight (the column, the corner, the
  members of a group), the structural roles (the midline, a gap, a barrier) and the
  Worldview's (the axis, the keys) later and in fewer runs (§11.3, §11.7). So the Roles pane
  (P14) usually shows the selector and content families full and the others thin; the quick
  start's run reads no role beyond first sight before its answer, which is why the roles
  picture comes from `blockage-07` seed 1 (G-056; seed 27 on v3's defaults, D-114, and 16
  before; seed 1 since v4's, D-115). Roles change during a run (lost to a rival
  reading, broken by the role checker, gone with their structure) in 60 to 87 percent of runs
  on the four carriers (§11.7): scrub the timeline to watch the dashed bars appear.
- **On v3's defaults (D-114) the runs are longer and fuller.** About 236 codelets a run on
  the target problems against v2's 72; the Worldview holds two or more correspondences in
  23 percent of runs and a group-to-group one in 17.5 percent (v2 14.5 and 3.6); a barrier
  stands at the end of 7 percent of runs and on Blockage 6 in a quarter (RESULTS.md §12).
  **On v4's (D-115)** the runs are as long on the targets (236 codelets) and fuller still:
  two or more correspondences in 32.5 percent of runs, a group-to-group one in 18.0, a
  barrier at the end of 13.5 percent of runs and of half of Blockage 6's, a symmetry axis in
  16.9, and the temperature ending near 16 (RESULTS.md §13). The bullets above read v2's
  numbers.
- **A context push is rare.** A push happens in 0.4 percent of runs (RESULTS.md §6.2, §8), so
  the Slipnet network's banner says "no context push in this run" almost always. The one
  recorded seed with a push in an ordinary run is `ch2-fig11` seed 3 (G-035, since item 09's runs).
- **The thermometer holds, then jumps.** T is 80 for the first codelets (the first
  seventeen of the quick start), held until the first Worldview change (D-049), then jumps
  (to 96.8 at codelet 18 in the quick start) and decays; French's fell from the start, to
  2.0 at the answer (RESULTS.md §6.4). The thermometer, the plot and
  the timeline's strip show the hold in grey and the jump as it is; the plot's French option
  sets his readings beside ours.
- **Where French's chapter 4 shows more**, a pane may set his numbers beside the run's,
  labelled: the thermometer's readings (P2b) and the grid's activations (P3) from
  `tests/data/ch4.json`, French's results (P8) from `reported.py`. They never adjust the run.
- **A miss is traced, not fixed.** RESULTS.md §8 traces every miss of French's results to a
  decision; the panes are there to see those decisions at work.

## Sessions and replay

Session > Save session… (Ctrl+S) writes the run as JSON lines (PANES.md section 5; G-048): a
header line with the problem (by name, or as its dict for a table set in the editor), the
profile, the edits over it, the seed and the engine's version; then every event, byte for
byte as `python3 -m tabletop NAME --seed N --json -` writes it; then an end line with the
count and a digest of the body, the same digest as `run.fingerprint`'s. A run can be saved
before it ends, with no end line. Open session… (Ctrl+O) re-executes the file's run through
the window's own run controller, from its problem, configuration and seed, and compares each
event with the stored one as it is drawn: the replay is verified when they all agree, and the
stats pane's Source row says so. If the engine has changed since the file was saved, the
first differing event stops the re-run before it is drawn, a dialog says where, and the
stored events are loaded into the history so that the trace, the log, the timeline and
every event-built pane still show them (G-049). While a replay is open the inputs are off,
the configuration pane shows the file's configuration read-only, and the title says
"replaying"; Play after the end re-executes. Close replay goes back to live runs with the
inputs chosen before. Save trace… and Save events as JSON lines… write the history's text
trace and JSON lines at any time.

## Changing the default layout

View > Reset layout restores the layout shipped with the code,
`tabletop/gui/default_layout.json`; View > Save layout as default for this code… overwrites
that file with the arrangement on screen (it asks first, and offers another place when the
package directory is not writable), and committing it shares it with everyone who uses this
copy of the code (ANSWERS.md 2; G-010). Your own layout, saved when the window closes, takes
precedence on your machine; View > Reset to built-in layout ignores both. A layout saved by
another version of the window's docks (the layout bytes carry a version) is ignored with a
note in the status bar, and the next layout in that order is used (G-013). The file is small
text: the dock state as base64 beside a readable record of each dock's visibility, area and
tab group, no window geometry.

## Screenshots

`python3 -m scripts.docs_screenshots` (from `python/`; the `gui` extra) renders the five
pictures of the front page's "Using the GUI" section offscreen, each under 200 KB, four
from the quick start's run (`surround-05` seed 3 under `standard`: on v5's defaults, D-116
to D-120, Eliza touches cup2 (E2), 384 codelets, T 28.3; cup2 (E2), 305 codelets, T 17.8 on
v4's, D-115; small-glass1 (E1), 144 codelets, T 31.0 on v3's, D-114; cup2 (E2), 44 codelets,
T 41.0 from item 08 of the roles loop to v2) and the roles picture from a run that reads
every family of roles (`blockage-07` seed 104 on v5, seed 1 on v4, seed 27 on v3, seed 16
before: a barrier, its claim, a re-cast and the axis), through the
real window, so that they come out the same at every regeneration (the fonts of the machine
apart; G-056). `tests/gui/test_gui_readme.py` checks them.

| File | What it shows |
|---|---|
| `docs/gui-window.png` | the whole window, the built-in layout, at the choice of the last codelet (find-answer): the structures complete, the Worldview holding cp33, the thermometer at 28.3, the answer not yet given; scaled to 1000 px wide |
| `docs/gui-table.png` | P1 at the answer: the icons (new drawings, not French's), H on cup1, E on cup2, Eliza's row as a gestalt group, the cup-to-cup correspondence solid in the Worldview and the cup-to-glass one that lost dashed, as the dumps draw them [F95 pp. 96-107 (img)] |
| `docs/gui-slipnet.png` | P3 at the jump (the temperature update after the first Worldview change, codelet 217; since v3 a drop, on v5 to 40.5), with "French's ch. 4" on: his nearest dump's values (Figure 4.4, 22 codelets) in red beside ours [F95 p. 95] (G-032); the seventh row and the roles' band under it (G-062) are the GUI's |
| `docs/gui-temperature.png` | P2 at the jump (T 40.5 by the change; G-027) beside P2b at the end of the run, the three terms stacked and French's chapter 4 thermometer as labelled points (G-028, G-029); the plot is GUI design, not French's |
| `docs/gui-roles.png` | P1 with the roles layer on (G-061) beside P14 (G-060) at the end of `blockage-07` seed 104: the badges, the midline band, the point-symmetry axis, the touched cup's images and the barrier's claim on the table; the pane scrolled to the structural family, the midline, the gap, the BARRIER and the CLAIMED tag with their readers (C4's fight, the answer recorded as blocked); GUI design, not French's: a re-interpretation (D-078) |

![The window at the last codelet of surround-05 seed 3](../gui-window.png)

The final review of the GUI against its design, with every departure from PANES.md and every
place the Numbo GUI is followed instead, each with its decision, is `docs/gui/review.md`
(item 20).
