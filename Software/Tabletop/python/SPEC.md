# Tabletop: the specification, distilled from the sources

This file is the specification the package `tabletop` implements: French's book, read page by
page, with FCCA's chapters 8-9 for the domain and the design intent, and the 1991 paper. Every
statement about Tabletop cites a page that has been looked at. Where the sources are silent,
the choice made is not here but in [`DECISIONS.md`](DECISIONS.md).

**Status:** sections 1-7, with 6b and 6c, are written (items 01-06) and cross-referenced to
the modules that implement them (the table after "The sources", item 29).

## Citation conventions

| Tag | Source | Page given as |
|---|---|---|
| `[F95 p. 91]`, `[F95 pp. 90-91]` | French 1995, *The Subtlety of Sameness* | the **printed** page |
| `[FCCA p. 387]` | Hofstadter & FARG 1995, *Fluid Concepts and Creative Analogies* | the **printed** page |
| `[FH91 p. 181]` | French & Hofstadter 1991, the Cognitive Science Society paper (reprint pp. 175-182) | the **printed** page |

**(img)** after a citation means its content was read from a page image (a figure, an
equation, a bar graph, a screen dump), not from the text layer. These three are the only
sources (ANSWERS.md 3).

Quotations are checked by machine: a quotation of six or more words in double quotes followed
by a tag is checked by `tools/check_quotes.py` against the text layer of the cited page (plus or
minus one page). Quotations are word for word; insertions go in [square brackets]; "..." marks
an omission; no quotation exceeds 80 words (the books are read in place, never copied into the
repository). Where a true quotation fails because the text layer is broken, it is verified on
the page image and given as a paraphrase without quotation marks.

## The sources

| Source | Where | Offsets (PDF page from printed page) |
|---|---|---|
| French (1995), *The Subtlety of Sameness*, MIT Press | FARGonautica's `Literature/Tabletop-French-1995-The Subtlety of Sameness.pdf` (198 PDF pages) | the text layer marks every printed page ("Page N") and the PDF skips blank pages, so the offset changes: +13 (pp. 1-21), +12 (pp. 23-45), +11 (pp. 47-111), +10 (pp. 113-149), +9 (pp. 151-185), +8 (pp. 187-190); the checker uses the markers |
| Hofstadter & FARG (1995), *Fluid Concepts and Creative Analogies*, Basic Books | the author's copy, outside any repository (copyrighted; read in place) | PDF = printed + 10 |
| French & Hofstadter (1991), "Tabletop: An Emergent, Stochastic Model of Analogy-Making" | FARGonautica's `Literature/Tabletop-French, Hofstadter-1992.pdf` (the reprint, pp. 175-182) | PDF = printed − 174 |

French's book, by printed page: chapter 2 (the domain and the problems, Figs. 2.1-2.21)
pp. 25-45; chapter 3 (the architecture) pp. 47-94; chapter 4 (a run, screen dumps Figs.
4.1-4.14) pp. 95-111; chapter 5 (results; Tables 5.1-5.3, Fig. 5.4) pp. 113-145; the summary
pp. 177-178. FCCA: Preface 8 and chapter 8 (the domain, its variants, Potelbat) from p. 319;
Preface 9 (the validation criteria, pp. 363-365) and chapter 9 (pp. 377-399, with the reprinted
results, Figures IX-1 to IX-3, pp. 394-399).

The program is Copycat's architecture applied to table settings: it "consists of three main
components: the Slipnet, the Workspace, and the Coderack" [F95 p. 55].

## The modules that implement each section

Where each section of this specification lives in the package (`tabletop/`), with the tests
that pin it (`tests/`) and the decisions that fill its open points (`DECISIONS.md`). The
module map of `README.md` gives the same table from the modules' side; every public
docstring under `tabletop/` cites the page it implements.

An engineering note (D-107 to D-112): the modules named here are the pure-Python engine,
the readable specification of the program, which runs only on the explicit request
`TABLETOP_ENGINE=python`. The engine that runs by default, and whose runs, goldens and
bench numbers are the reference, is the compiled one (`tabletop/_native/`, built from the
same specification function for function: the generator, the Slipnet and the Coderack's
chances in C); it requires a C compiler and Cython to build, and without a build every
entry point stops at start. As the fast-engine loop leaves them, the two engines' runs are
identical bit for bit; the guards that replace that identity, once anything departs, are
the compiled engine's self-determinism, each engine's own goldens and the statistical
equivalence of the pure engine.

| Section | What it specifies | Modules | Tests | Decisions |
|---|---|---|---|---|
| 1.1-1.3 | the game, the objects, the table | `geometry.py`, `objects.py`, `problems.py` | `test_geometry.py`, `test_problems.py` | D-001, D-002, D-005, D-006 |
| 1.4 | realistic tables only | `tables.py`, `bench/robustness.py` | `test_tables.py` | D-068 |
| 1.5-1.6 | the input; chapter 2's nineteen configurations | `problems.py`, `problems/ch2-*.json`, `bench/ch2.py` | `test_ch2.py`, `test_problems.py` | D-003 |
| 1.7 | the families and the Potelbat baseline | `problems/`, `potelbat.py`, `bench/potelbat.py` | `test_potelbat.py` | D-003, D-067 |
| 2.2-2.6 | the three components and the flow of a run | `world.py`, `run.py` | `test_run.py`, `test_events.py` | D-064, D-066 |
| 2.7 | the end of a run and its output | `answer.py`, `observe.py`, `run.py` | `test_run.py`, `test_trace.py` | D-056 to D-063 |
| 2.8 | how the program was judged | `bench/lesions.py`, `bench/results.py` | `test_benches.py`, `test_results.py` | D-073, D-075 |
| 3.1-3.3 | the nodes, depth, the links | `slipnet_def.py` | `test_slipnet.py` | D-007 to D-010 |
| 3.4-3.9 | lengths, distance, activation, labels, the initial state, posting | `slipnet.py` | `test_slipnet.py` | D-011 to D-020 |
| 3.10 | the context push and pop | `context.py`, `slipnet.py` | `test_context.py` | D-021, D-070 |
| 4.1-4.2 | the Workspace, the start and scanning | `workspace.py`, `perceive.py`, `world.py` | `test_workspace.py` | D-004, D-023, D-032, D-071 |
| 4.3 | descriptions | `describe.py`, `workspace.py` | `test_describe.py` | D-027 to D-029 |
| 4.4 | groups: gestalt, bottom-up, strength, subgroups, ends | `groups.py`, `build_object.py`, `describe.py` | `test_groups.py`, `test_bottom_up.py`, `test_describe.py` | D-022 to D-028, D-069 |
| 4.5 | salience and the region preference | `salience.py`, `geometry.py` | `test_salience.py` | D-031 to D-034, D-046 |
| 4.6 | correspondences and their factors | `correspondences.py` | `test_correspondences.py` | D-030, D-035 to D-039, D-045 |
| 4.7 | the Worldview, rivals, fights, coherence, fading | `worldview.py` | `test_worldview.py` | D-040 to D-044 |
| 5.1-5.3 | codelets, the catalogue, posting | `codelets/`, `coderack.py` | `test_codelets.py`, `test_coderack.py` | D-051, D-052, D-054, D-055 |
| 5.4-5.6 | urgency, choice, generations, capacity, age | `coderack.py`, `temperature.py` | `test_coderack.py`, `test_worked_examples.py` | D-052, D-053, D-060 |
| 6.1-6.4 | temperature: meaning, factors, formula, uses | `temperature.py`, `world.py` | `test_run.py`, `test_temperature.py` | D-047 to D-050, D-060 |
| 6.5-6.6 | find-answer, the end, the output | `codelets/finish.py`, `answer.py`, `observe.py` | `test_run.py`, `test_trace.py`, `test_cli.py` | D-056 to D-059, D-061 to D-063 |
| 6b | French's faults | `config.py` (`FAULT_KNOBS`), `bench/faults.py` | `test_config.py`, `test_benches.py` | D-021, D-026, D-028, D-045, D-046, D-069, D-070 |
| 6c | the chapter 4 run | `docs/ch4-run.md`, `tests/data/ch4.json`, `bench/ch4.py`, `bench/stated.py` | `test_ch4.py`, `test_stated.py` | D-017, D-049, D-055, D-074 |
| 7 | French's numbers and their inconsistencies | `reported.py`, `bench/families.py`, `bench/fcca.py`, `bench/humans.py`, `bench/results.py` | `test_reported.py`, `test_bench.py`, `test_results.py` | D-059, D-072, D-075, D-076 |
| Beyond French: roles | the roles, a re-interpretation (the pointer section at the end; `docs/roles.md`) | `roles.py`, `describe.py`, `correspondences.py`, `worldview.py`, `salience.py`, `answer.py`, `codelets/scouts.py`, `codelets/searches.py`, `codelets/judges.py`, `slipnet_def.py`, `bench/variants.py`, `bench/churn.py` | `test_roles.py`, `test_role_consumers.py`, `test_roles_geometry.py`, `test_role_slippages.py`, `test_recast.py`, `test_roles_parts.py`, `test_roles_content.py`, `test_roles_blockage.py`, `test_variants.py` | D-078 to D-106 |

## 1. The domain and the problems

### 1.1 The game

Two people, Henry and Eliza, sit at a small table facing each other. Henry touches an object
and says "Do this!"; Eliza "must find the object (or, possibly, the group of objects) that, from
her vantage point, seems to correspond most closely to the object that Henry touched" [F95
p. 25]. She has to touch something physical: "She is required to touch a physical object"
[F95 p. 25]; touching a bare location, though it "could be a reasonable thing to do under some
circumstances" [F95 p. 25], is not allowed. The program plays Eliza, "with selection playing the
role of touching" [FCCA p. 377].

Touching the very object Henry touched is always available: "Such an option is always open,
no matter what the configuration and no matter what object Henry touches" [FCCA p. 377]; in
French's words, literal sameness is "always a pressure" [F95 p. 28]. Small children and the
occasional literal-minded adult take it [FCCA p. 325]; most adults slip, and touch what they
perceive as the corresponding object from their side [FCCA p. 325].

**The pressures.** A human Eliza weighs "the positions and orientations of the objects on the
table, the categories to which they belong, their sizes, their functional associations with other
objects ... and their participation in groups on the table" [F95 p. 25]. FCCA's list has eight
entries: the nature and location of the touched object, the categories of the other objects,
their physical locations, their "ownership", their orientations, their sizes, common functional
associations (cups with saucers, knives with forks), and "perceptual groupings on one or more
hierarchical levels" [FCCA p. 378]. Some pressures are context-independent (an object's category
does not depend on the other objects); others, such as belonging to a group, "emerge gradually as
the person or program develops a representation of the configuration of objects on the table"
[F95 p. 26]. The 1991 paper says the same, listing position, category, size and functional
association [FH91 p. 175].

**Few answers.** "Tabletop problems generally have only one or two reasonable answers—sometimes
three, but no more." [F95 p. 41] For the central problem of the book (Figure 2.11, §1.6), the
two plausible answers are the glass in the lower left and the cup in the lower right, and the
implausible but possible one is Henry's own cup [F95 pp. 41-42]. FCCA gives the reason: since
Eliza only touches an object instead of transforming it, "the number of plausible answers to a
Tabletop problem is often much smaller than the number of plausible answers to a Copycat
problem" [FCCA p. 389]. This is why French's statistics are frequencies over two or three
answers rather than Copycat's spread over six or seven [F95 p. 42].

**No learning across problems.** Eliza "begins each problem as if it were the first Tabletop
problem she had ever seen" [F95 p. 26]. The program does no long-term learning, not even on
problems it has already seen [F95 p. 41]; it learns only within a run, where it "notices objects
and groups of objects, builds representations for them, discovers correspondences, keeps some
correspondences and rejects others, activates certain concepts, adjusts conceptual distances"
[F95 p. 41], and "when the run is completed, everything is reset to 'zero' for the next run"
[F95 p. 41]. (For the reconstruction: a fresh world per run.)

**Recognition, not retrieval.** The two situations to be mapped are both on the table; there is
no memory retrieval [F95 p. 39]. The situations "overlap and blur on the table", so that
"there is, in some sense, only one situation" [F95 p. 39] which people break into two halves,
"roughly (but not necessarily exactly) halfway across the table" [F95 p. 40].

### 1.2 The objects

**Eleven types.** The palette of Figure 2.1 lists, top to bottom: fork, knife, spoon, cup,
saucer, plate, big glass, small glass, pepper shaker, salt shaker, soup bowl [F95 p. 26 (img)].
The text says the same in prose: "pieces of silverware, cups, saucers, plates, soup bowls,
drinking glasses of two different sizes, saltshakers, and pepper shakers" [F95 p. 55], and the
program's own palette, to the left of the table on the screen, holds "forks, knives, and spoons,
(in four possible orientations); cups; saucers; plates; big glasses; small glasses; pepper
shakers; saltshakers; and soup bowls" [F95 p. 95]. Each of these eleven types is a node of the
Slipnet (§3); the Slipnet's 47 concepts also hold "receptacle", not a drawable object [FCCA
p. 381].

**Idealized.** An object has no properties beyond its type, its position and, for silverware,
its orientation: "no object has any idiosyncratic distinguishing properties above and beyond its
location on the table and its category membership", so that a cup "is shapeless, sizeless, and
decoration-free" [FCCA p. 326].

**Sizes.** Nevertheless "All objects have a size; they are not simply point objects" [F95
p. 55], and "the sizes of objects" are a pressure [FCCA p. 378]. The two statements are
reconciled by reading FCCA's "sizeless" as the absence of variation within a type: every cup is
the same cup, and the two glass sizes are two types (big glass, small glass), with the Slipnet
nodes "bigger than" and "smaller than" [FCCA p. 381] for comparisons between types. The books
give no footprint, in any unit, for any type; the drawn icons are the only evidence (→ D-002).

**Orientations.** "The forks, knives, and spoons have four possible orientations." [F95 p. 55]
The palette draws each of the three in its four orientations, pointing up, down, left and
right [F95 p. 26 (img)]; the screen's palette does the same, with the number of objects of each
type on the table above its cell [F95 p. 96 (img)]. The other eight types have no orientation. Orientation is part of the input
[FCCA p. 379] and listed among the pressures [FCCA p. 378] and among the relational labels
("relative sizes and orientations") [FCCA p. 380]; French's book never says how a difference
of orientation between two pieces of silverware affects a correspondence or a group, and
chapter 5's problems do not vary it (→ D-006). Groups, by contrast, do have an orientation:
horizontal or vertical (§4).

**Categories above the objects (the Platobet).** FCCA calls the domain's list of abstract
categories its "Platobet" [FCCA p. 326]. Plate and saucer are close because both are round and
used for putting things on; cup and saucer are close "in a different way" because they are used
together; all three are, without contextual pressure, "very far from 'salt shaker'" [FCCA
p. 326]. The web is small, "perhaps two pages' worth of text" [FCCA p. 326], but is a
"hodgepodge of concepts with vague, blurry interconnections" [FCCA p. 389] rather than
Copycat's crystalline alphabet: forks and knives are used together, forks and spoons both hold
food, spoons and knives share only a side of the plate [FCCA p. 389]. French's superordinates,
with the ISA links that make the categories "blurrier" than Copycat's, are in §3; the example
he gives is that a soup bowl is a liquid holder, a food holder and crockery, and a knife is
silverware and a sharp object [F95 p. 42].

### 1.3 The table

**Continuous.** "Tabletop is a continuous two-dimensional world." [F95 p. 42] "These objects
can be placed anywhere on the table." [F95 p. 55] The input gives "the coordinates and
orientation of each placed object" [FCCA p. 379]. Physical proximity, a minor matter in
Copycat, "figures very significantly in the tabletop world": groups can arise "on the mere
basis of objects' being physically close to one another on the table" [F95 p. 42].

**Drawn as a frame, slightly wider than tall.** Every figure draws the table as a frame with
"Henry" above its top edge and "Eliza" below its bottom edge [F95 p. 27 (img)]; FCCA's cartoon
has Henry at the top and Eliza at the bottom too [FCCA p. 322 (img)]. The frame is not quite
square: in the chapter 5 figures it measures 191-195 × 175-176 pixels inside its lines, 1.10
times wider than tall, the same in all ten Surround figures [F95 pp. 115-121 (img)], and the
screen's own frame in the chapter 4 dumps is 306 × 268 pixels at 150 dpi, 1.14 [F95 p. 98
(img)]; FCCA's redrawn figures have the same shape [FCCA p. 394 (img)]. The book gives no
dimensions and no coordinate system (searched: chapters 2-4, the screen dumps, and FCCA chs.
8-9), so the unit and the origin are a decision (→ D-001: a 110 × 100 table), and the layouts
are digitized from the figures (→ D-003; `docs/problems.md`).

**Nine blurry regions.** "The table is divided into nine regions like a tic-tac-toe board,
although the boundaries between regions are intentionally blurry. In other words, an object
that is actually in one region but very close to a region boundary may sometimes be considered
by the program to be in the adjacent region." [F95 p. 65] The Slipnet's position concepts
(left, right, middle, end, above, below, position, direction) are in §3 [FCCA p. 381]. How
blurry, and whether the region is drawn once or each time it is asked, is not said (→ D-004).

**Positions relative to the touched object.** What matters most is where an object stands with
respect to the touched object: "directly across the table" from it, "diagonally across the
table", and "in the immediate neighborhood of the touched object" [FCCA p. 386]. Group-finding
codelets look preferentially, "but not deterministically", in the corner diagonally opposite
the touched object, a bias "built into the program deliberately in order to imitate typical
human behavior" [FH91 p. 178]. The concepts "opposite", "diagonal symmetry" and "mirror
symmetry" are Slipnet nodes [F95 p. 55]; a correspondence is more likely between items "in
mirror-image or diagonally opposite locations on the table, or nearly so" [FCCA p. 382].

**No fixed sides.** In Copycat a mapping goes from the initial string to the target string; in
Tabletop "any particular object or group of objects can, at least theoretically, map to any
other object or group of objects", because "there is really only one situation in which all of
the groupings and mappings occur" [F95 p. 43]. FCCA: "There is no pre-set boundary line
dividing this arrangement into two disjoint 'situations'" [FCCA p. 388]; there are two observers,
each free to carve the table up, and the dividing line that often appears "approximately halfway
between the two people" is a membrane that "remains partially permeable", since either party
can reach across it [FCCA p. 389]. "Ownership" is nevertheless a pressure [FCCA p. 378]. The
reconstruction therefore puts no side restriction on correspondences; Henry's and Eliza's halves
exist only through regions and positions (→ D-005).

**Marking the answers.** In the figures the touched object carries an arrow marked H, and
Eliza's possible answers arrows marked E1, E2, and so on [F95 p. 114]; on the screen Eliza's
choice "is always shown by a short arrow with an E at its back end" [F95 p. 105].

### 1.4 Realistic tables only

The domain was meant to have "a real-world feel to it", so "each of the table configurations
had to be reasonably realistic, in the sense of looking like a real table that was set with
plates, cups, saucers, saltshakers, pepper shakers, and silverware"; it "was not a
patterns-and-shapes domain in which just any configuration was permissible", and objects would
not be "arranged in the shape of a Christmas tree or in strange, alternating patterns" [F95
p. 38]. Figure 2.21 shows two disallowed tables: on the right, a heart of saucers around a
fork; on the left, fragments of place settings scattered into the corners [F95 p. 39 (img)].
FCCA says the same from the other side: tables with artificial geometric symmetries "would be
unnatural and would even run against the spirit of the project" [FCCA p. 388], and Hofstadter's
own five-forks-and-a-spoon translation of a real analogy "looks highly unrealistic" [FCCA
p. 321]. The generator of random tables for robustness runs respects this (→ D-068).

### 1.5 The input

The program does not see pixels. "analogy problems on the tabletop are communicated to the
program by a graphical interface whereby a user drags highly stylized icons around on a computer
screen until satisfied with the configuration. At that point, the coordinates and orientation
of each placed object, along with its category membership, are conveyed to the program, and
that, along with an indication of which object was touched by Henry, is what constitutes the
raw input." [FCCA p. 379] "Thus low-level object perception is completely bypassed." [FCCA
p. 379] The screen's buttons are Load config., Do this!, Clear table, Run it again, Save
config. and Exit [F95 p. 96 (img)], so configurations were files that could be saved and
reloaded; their format is not given.

The input is not what the program knows. It "should not be thought of as what the program is
aware of, but rather as the physical situation itself, or perhaps as extremely low-level visual
information (something like retinal information)" [FCCA p. 386]. At the start of a run "the
program explicitly knows only the location and object type of the touched object" [F95 p. 64];
"the program knows only which object was touched by Henry and where on the table that object is
located" [FH91 p. 177]. Everything else is discovered by codelets (§2.5, §4).

In the reconstruction a problem is a JSON file in `problems/` with one record per object (id,
type, x, y, orientation), the touched object's id and the labelled answers (E1, E2, ...), each
object carrying the figure, printed page and image it was measured from (→ D-003).

### 1.6 Chapter 2's nineteen configurations (qualitative evidence)

Chapter 2 presents "a first series of thirteen table configurations followed by a second series
of six", each "a (roughly) escalating series, in which pressures present in the first problem
are systematically varied, and new pressures are introduced and also varied" [F95 p. 26]. They
come with French's expected human answers in words and no statistics, so they are qualitative
targets (M2 in the validation protocol). Every table is drawn with Henry at the top; "upper"
means Henry's edge and "lower" Eliza's. The descriptions below come from the text and from the
figures on their pages (img).

**The first series** [F95 pp. 26-32]:

| Fig. | Page | Configuration | French's expected answer |
|---|---|---|---|
| 2.2 | 27 | A single cup in front of Henry, touched | The same cup, "the only possible response" [F95 p. 26] |
| 2.3 | 27 | Henry's cup at top centre; a cup at bottom centre | Her own cup, "by far the most likely response"; two pressures, literal sameness and position, "the latter pressure swamps the former" [F95 p. 26]; touching Henry's cup "a very unlikely answer" [F95 p. 27] |
| 2.4 | 28 | Eliza's cup replaced by a glass | The glass, generally; the category difference "puts a little more pressure on Eliza to reach across the table" but "not enough for her to do so very often" [F95 p. 27] |
| 2.5 | 28 | The glass replaced by a piece of silverware, a fork (img) | Henry's cup, "most often"; the category difference more than matches the position pressure [F95 p. 27] |
| 2.6 | 29 | The silverware replaced by a saucer | More pressure to touch the saucer than in 2.5, "perhaps a bit less than in figure 2.4", because cups and saucers "are used together" [F95 pp. 27-28] |
| 2.7 | 29 | Henry's cup in the upper left; a glass in the lower left and a cup in the lower right | "the chances are good" that she touches the cup diagonally opposite [F95 p. 28] |
| 2.8 | 29 | As 2.7, with objects around both the touched cup and the glass (img: a saucer, a knife, a fork and a saltshaker around the cup; two spoons, a glass and a knife in the lower left) | The glass becomes "more likely" than in 2.7: the new pressure "belonging to a group" [F95 p. 28] |
| 2.9 | 30 | Objects to the right of the touched cup and to the right of the glass, differing in number and makeup | More pressure toward the glass: the "bunch of objects to the right" of each may map, "even though the groups are not the same either in number or in makeup" [F95 p. 28] |
| 2.10 | 30 | Silverware to the right of the cup (a fork between two spoons) and to the right of the glass (a fork inside the group) [F95 p. 36] | More pressure toward the glass: each collection may be grouped "as 'silverware'" and the two put into correspondence [F95 p. 30] |
| 2.11 | 31 | Henry's cup in the upper left, with two forks and a knife on its left and two spoons on its right; Eliza's glass in the lower left, with two spoons on its left and two forks and a knife on its right; an isolated cup in the lower right (img) | The glass, with more pressure than 2.10: the groups "have the same number of objects" and "the objects surrounding the cup and the glass are the same" [F95 p. 30]. "Take close note of this problem. We will be seeing it throughout the course of this work." [F95 p. 31] Two plausible answers, the glass and the lower-right cup; one implausible, Henry's cup [F95 pp. 41-42] |
| 2.12 | 31 | As 2.11 with the subgroups (two forks, two spoons) "in corresponding positions on either side" [F95 p. 31] | The glass, "even stronger than in the previous configuration" [F95 p. 31] |
| 2.13 | 32 | The glass replaced by a saucer | Shifts toward the isolated cup "Possibly, but not necessarily", because cups and saucers are "objects that are used together" [F95 p. 31] |
| 2.14 | 32 | The saucer replaced by a saltshaker | Shifts "even more significantly to the isolated cup" [F95 p. 31]: a saltshaker and a cup "are conceptually very dissimilar and have no obvious relationship between them" [F95 p. 32] |

The nineteen configurations are digitized as `problems/ch2-fig02.json` to `ch2-fig20.json`,
each with French's expectation in his words as an `expected` block (D-003; `docs/problems.md`
9). The 1991 paper's Figure 1 shows three of these (its 1a is a cup and a glass with the cup
diagonally opposite, 1b is Figure 2.11 and 1c is Figure 2.14) [FH91 p. 176 (img)], with the
same expected answers [FH91 p. 176].

**The second series, "Blockage"** [F95 pp. 32-35], "essentially the one referred to as the
'Blockage' series in chapter 5" [F95 p. 32]:

| Fig. | Page | Configuration | French's expected answer |
|---|---|---|---|
| 2.15 | 33 | Henry's cup in the upper left; a spoon in the upper right; a glass in the lower left | The glass, "most probably"; otherwise Henry's cup, "certainly not ... his spoon" [F95 p. 32] |
| 2.16 | 33 | The spoon replaced by a glass | Henry's cup becomes "considerably more likely" than in 2.15 [F95 p. 34]: Eliza may notice the glass-glass correspondence, and the cup/glass and glass/glass correspondences "are competing with one another" [F95 p. 33], like the two readings of the Necker cube [F95 p. 34] |
| 2.17 | 34 | Two glasses in the upper right and two in the lower left | Henry's cup, "much more likely than in figure 2.16", because of the "two identical pairs of objects on the table, diagonally opposite one another" [F95 p. 34] |
| 2.18 | 34 | The lower-left pair is a glass and a piece of silverware, a spoon (img); the upper-right pair stays two glasses | Back toward the glass: the groups "are not identical any longer", and "may well not be perceived by Eliza as corresponding" [F95 p. 35] |
| 2.19 | 35 | A saltshaker and a plate in the upper right; a glass and a spoon in the lower left | The glass, "strongly": "very little likelihood" of the group correspondence [F95 p. 35] |
| 2.20 | 35 | The saltshaker and plate in the upper right; the glass alone in the lower left | The glass, "extremely strong": "virtually no possibility" of the group correspondence [F95 p. 35] |

The 1991 paper's Figure 2 draws three of these as runs: 2a (a cup and a glass, as 2.15 without
the spoon), 2b with "a single glass ... added in the upper right-hand corner", shown with its
two solutions, and 2c with two glasses in each corner [FH91 p. 180 (img)], where the glass-glass
correspondence is "very salient and very strong, usually strong enough to cause the program to
touch the cup that Henry touched" [FH91 p. 181]. FCCA's Figure VIII-9 is the same "blockage"
scenario [FCCA p. 355 (img)]: the diagonal bridge between the two glasses "tends to block an
otherwise perfectly reasonable answer" and "to push strongly for touching Henry's cup itself",
with "no need for a special 'blockage-checking mechanism'" [FCCA p. 355].

**What the series are for.** The problems "were designed to bring out and isolate certain
general abilities or mechanisms necessary for analogy-making" [F95 p. 36]: grouping, physically
and conceptually; generalizing to the right level of abstraction (cup, liquid holder or
crockery); ignoring the irrelevant, "implicit pruning", which must be done by not considering,
"not consider, then reject" [F95 p. 36]; and focusing resources [F95 pp. 36-37]. French's
example of pruning is a further qualitative target: a cup in front of Henry, a cup in front of
Eliza, and three small groups of silverware in the upper right; "The program should be able to
ignore—not consider, then reject—the small groups of silverware" [F95 p. 36].

**Two more configurations in chapter 2.** Figure 2.22 is the "single node with multiple
activations" example [F95 p. 44 (img)]: a fork and two knives in the upper left, a cup in the
upper right, two forks and a knife in the lower left and a cup in the lower right; with the
silverware groups and the cups in correspondence, "mirror symmetry" is active, yet should put
little or no pressure on mapping the knives inside the groups mirror-symmetrically [F95
pp. 43-44]. Figure 2.21 holds the two disallowed tables (§1.4).

**FCCA's illustrations.** Chapter 8 opens with three coffeehouse puzzles: Henry touches a salt
shaker and Eliza, after hovering over the pepper shaker, touches the salt shaker [FCCA
pp. 323-324]; Henry touches his plate and Eliza, "after some hesitation", her saucer [FCCA
p. 324]; Henry reaches over and touches Eliza's cup, and she touches his spoon, reasoning from a
glass-glass-to-fork-fork mapping [FCCA p. 325]. The "East St. Louis of Illinois" series, Figures
VIII-6 to VIII-8 [FCCA pp. 353-354 (img)], contrasts an object's category with its "locational"
description ("between a fork and a spoon") [FCCA p. 354]. Figure VIII-10, "A Tabletop scenario
in which people naturally see triply-nested groups" [FCCA p. 357 (img)], has many people drawn to
a salt shaker because of its place inside a six-object group that breaks into a fork-fork group
and a knife-spoon-spoon group [FCCA pp. 356-357]; it is close to Figure IX-1e [FCCA p. 356].
These are not result sets; they illustrate pressures.

### 1.7 Chapter 5's families and the other variants (pointers)

Chapter 5's four families, Surround, Blockage, Buridan and Figure/Ground, are "some two
thousand runs of Tabletop" on "the forty or so problems" [F95 p. 113]; their layouts, numbers
and inconsistencies are §7, and the problems are digitized in `problems/` (items 06-09).
French's description of the families [F95 p. 114]: Surround places "various assortments of
objects ... around two of the key objects" to push against touching the conceptually closest
object on Eliza's side; in Blockage "A specific pressure, not explicitly anticipated, causes a
particularly natural answer to be blocked"; for Buridan "I adjusted the program to produce an
exactly balanced response to one particular configuration; then I varied the configuration in
several ways"; in Figure/Ground "The standard version of the program was not able to get a
certain, very 'structural' answer".

FCCA's chapter 8 is titled for the domain's thought-experiment variants, none of which is a
program to reconstruct: **BattleOp**, retaliation "in proportion to" an act of aggression,
"Tabletop scaled up with a vengeance" [FCCA p. 332]; **Ob-Platte** puzzles, "What is the A of
Y?" for cities and states, after the classic "What is the Ob of Nebraska?" [FCCA p. 333]
(attributed to a fictitious Belpatto, 1890); the **Platobet**, the domain's "alphabet" of
Platonic categories [FCCA p. 326]; and **Potelbat**, "tabletop" backwards [FCCA p. 328].

**Potelbat, the brute-force baseline.** The first program written for the domain was "a pure
brute-force formula-based program": a weighted sum of terms, one per attribute (category,
location, orientation, owner, neighbours), scoring each object's similarity to the touched one
[FCCA pp. 327-328]. It "performed reasonably well on simple Tabletop problems, although on some
trickier ones ... it did poorly" [FCCA p. 328]; it "sees no relationships between objects,
makes no groupings, builds no bridges" [FCCA p. 356], so it cannot see that Eliza's glass is
"already taken" in a blockage problem [FCCA p. 355] and would need a special mechanism for every
such case [FCCA p. 356]. The reconstruction keeps one as a contrast (→ D-067; measure M9).

## 2. The architecture: an overview

### 2.1 Lineage

Tabletop and Copycat implement one abstract architecture, the "Copycat Memo" of 1984, "in
different domains" [F95 p. 47]; they are "the same project in different domains" [F95 p. 41].
Tabletop's architecture "is therefore on a deep level the same as the Copycat architecture, but
it has been modified and augmented as appropriate to fit the exigencies of the new domain" [F95
p. 47]. Before Copycat, Jumbo, Seek-Whence and Numbo implemented parts of the Memo [F95 p. 47].
From Jumbo come the parallel terraced scan, "exploration proportional to promise" [F95 p. 48],
codelets with urgencies "hung" on a Coderack and chosen "in a biased, random manner" [F95
p. 49], the staged evaluation of a candidate structure by successive codelets [F95 p. 49], and
computational temperature as a feedback mechanism, not an annealing schedule [F95 pp. 50-51],
affecting "virtually all situations in which multiple choices are possible" [F95 p. 51]. From
the Memo come the Slipnet, the Workspace (Hofstadter's "Cytoplasm"), codelets, the Coderack,
the continual interaction of Slipnet and Workspace, competition among structures with a
salience for every structure, no global executive, simulated parallelism, stochastic decisions,
and the integration of representation-building with correspondence-finding [F95 pp. 52-53].
"All these architectural features were implemented in Mitchell's Copycat program as well as in
Tabletop" [F95 p. 54]; the differences are "at the level of how competitions are resolved and
how temperature is determined" [F95 p. 54].

Tabletop was begun in 1987 [F95 p. 54] and was "over twenty-six thousand lines of code, over
twice the length of Copycat" [F95 p. 54]; the growth is in codelet types, whose number "is
essentially proportional to the number of concepts in the Slipnet" [F95 p. 54]. The
architecture is built against combinatorial explosion: codelets engender codelets, are chosen
by urgency and pick structures by salience, which lets the program "(probabilistically) focus
its resources very sharply and sensitively" [F95 p. 54].

### 2.2 The three components

"Tabletop, like Copycat, consists of three main components: the Slipnet, the Workspace, and the
Coderack. As in Copycat, the Slipnet is a permanent concept network, and the Workspace is a
theater of operations." [F95 p. 55] Codelets are "dispatched stochastically from the Coderack
to the scene", where they observe the table, build and break structures, send activation to the
Slipnet and look for correspondences between objects and groups [F95 p. 55]. In the summary's
words the Slipnet "models long-term memory" and the Workspace "models short-term memory" [F95
p. 177]; the Coderack is the codelets' "staging ground" [F95 p. 53]. The 1991 paper puts it as
types and tokens: "the Slipnet contains Platonic concepts, while the Workspace contains instances
of those concepts, which codelets observe and manipulate" [FH91 p. 177].

- **The Slipnet** (§3): a node-and-link network of 47 concepts [FCCA p. 381], each node the
  "core" of a concept with a fixed conceptual depth [F95 p. 55] and a varying activation [F95
  p. 55]; link lengths that vary during a run; nothing built or destroyed [FCCA p. 381]. Its
  activity: a jolt when an instance is perceived, decay at a rate set by depth, spreading to
  neighbours "gated by conceptual proximity", and the adjustment of distances by the activation
  of key concepts [FCCA p. 381].
- **The Workspace** (§4): "a kind of short-term perceptual memory" [FCCA p. 380] holding every
  temporary structure: descriptions of objects, groups built hierarchically, correspondences,
  each with a time-varying salience, and strengths for groups and correspondences [FCCA p. 380].
  Inside it, the elite **Worldview**, "a non-self-contradictory subset of the Workspace" of the
  structures currently considered best, with the constraint that "groups do not overlap, objects
  have at most one counterpart" [FCCA p. 390]; losers of competitions are demoted from it, not
  destroyed [FCCA p. 390]. Its **structure-value** is "essentially a sum of the individual
  strengths of all the structures in the Worldview, with penalties for incoherence" [FCCA
  p. 387].
- **The Coderack** (§5): where codelets, "approximately thirty different codelet types" [F95
  p. 63], are posted with an urgency and chosen stochastically; codelets are posted when a
  concept's activation is high, and as follow-ups when a codelet succeeds [F95 p. 63].
- **Temperature** (§6) is not a component but a quantity: it "reflects the amount and the
  quality of overall structure" [F95 p. 177] and sets "the amount of randomness in decision
  making" [F95 p. 177]; the thermometer on the screen runs from 0 to 100 [F95 p. 95].

The 1991 paper names three central features behind these: "continual interaction between the
associative concept network (the Slipnet) and many low-level perceptual agents (codelets)";
"local processing and (simulated) parallelism"; and "stochastic mechanisms" [FH91 p. 177], with
temperature as a further one [FH91 p. 177].

### 2.3 French's six differences from Copycat [F95 pp. 42-45]

French's section 2.9.2 lists what Tabletop does that Copycat does not. Each lands in a later
section of this specification:

1. **Blurrier categories.** The concept network "contains far more ISA links ... than the
   Copycat concept network does"; one category often has several superordinates, and "The way
   in which activation spreads and link lengths shrink must take these differences into
   account" [F95 p. 42]. (§3: the parallel-path distance.)
2. **Real-valued object locations.** A continuous world, where physical proximity "figures very
   significantly" and can by itself make groups [F95 p. 42]. (§1.3, §4: proximity groups.)
3. **No rigid compartmentalization.** Any object or group can map to any other; one situation
   [F95 p. 43]. (§1.3, §4: correspondences.)
4. **The problem of a single node with multiple activations.** A concept active at the level of
   the whole table ("mirror symmetry" from two diagonal correspondences, like "west" on the
   interstate) should exert "little or no influence on the way in which the structures at the
   lower level are built" [F95 p. 44]; "a single node must to be able to maintain different
   levels of activation simultaneously" [F95 p. 44]. (§3: push and pop of activations.)
5. **Competition that is less destructive.** In Copycat the loser of a fight "is destroyed";
   in Tabletop "structures that lose competitions are penalized but not destroyed", through the
   distinction between the Workspace, "where essentially all of the structures that have been
   built are present, and the more elite Worldview inside it, where only the currently perceived
   structures are present" [F95 p. 44], on the model of Ebbinghaus's savings and the young
   woman/old woman drawing [F95 p. 44]. (§4: the Worldview.)
6. **Urgency explosion, and a "local" temperature.** "A problem called 'urgency explosion' was
   observed in Tabletop, and a solution to this problem was conceived and implemented" [F95
   p. 44], a problem that Copycat's constraints on follow-up codelets had hidden [F95 p. 45];
   and temperature "is derived in a more emergent manner than in Copycat" [F95 p. 45]. (§5:
   generations and ρ; §6: the temperature formula.)

### 2.4 The eleven features [F95 pp. 177-178]

French's summary lists "the key features of the Tabletop architecture" [F95 p. 177]:

1. "A concept network, called the Slipnet, that models long-term memory and a Workspace that
   models short-term memory" [F95 p. 177];
2. "Processing that is carried out entirely by many small agents, called codelets" [F95 p. 177];
3. "The absence of a global executive" [F95 p. 177];
4. "Biased probabilistic mechanisms at all levels of processing" [F95 p. 177];
5. gradual building of structures in the Workspace, "groups of objects and correspondence
   between groups and/or objects" [F95 p. 177];
6. "A context-dependent conceptual similarity metric, embodied by the link lengths in the
   Slipnet" [F95 p. 177];
7. "A set of context-dependent, top-down search pressures, determined by the activations of nodes
   in the Slipnet and the salience of objects in the Workspace" [F95 p. 177];
8. continual interaction between Slipnet and Workspace, bottom-up (the Workspace adds activation
   to nodes, "thus indirectly causing certain Slipnet links to shrink and others to grow") and
   top-down (high activations "cause certain types of relationships and structures to be
   preferentially sought and built") [F95 p. 177];
9. temperature, "reflecting the amount and the quality of overall structure that the system has
   built" [F95 p. 177];
10. temperature controlling randomness: "the higher the temperature, the more random the
    decisions, the lower the temperature, the more deterministic" [F95 p. 177];
11. the parallel terraced scan, exploring "potentially more promising routes ... before less
    promising ones, without, however, totally excluding the possibility of exploring seemingly
    unpromising routes" [F95 p. 178].

### 2.5 FCCA's account of high-level perception, and its three architectural differences

FCCA describes the same architecture as a list of concurrent activities of "high-level
perception" [FCCA p. 380]: labelling objects with surface attributes ("size, sharpness, open
versus closed, liquid-holder, food-holder, receptacle"); labelling them with relational
properties ("neighbor relations, functional associations, relative sizes and orientations,
position inside group, position relative to salient objects"); building groups hierarchically
from physical proximity, conceptual proximity of descriptions, structural similarity of
subgroups and "prior existence of similar groups"; building correspondences from corresponding
positions, conceptual proximity, structural similarity of groups and "prior existence of similar
correspondences"; assigning every item a time-varying salience and every group and
correspondence a time-varying strength; and competition that prunes weaker structures and
"structures that do not cohere with one another" [FCCA p. 380]. The Slipnet's four activities
complete the list [FCCA p. 381]. The architecture "is deeply parallel" and "probabilistic, with
biased decisions of all sorts being made quickly with the aid of a random-number generator"
[FCCA p. 381]; the parallelism is "effective parallelism, somewhat like classical time-sharing
on a mainframe computer, rather than hardware parallelism" [FCCA p. 384], as French also says
[F95 p. 53].

FCCA's "noteworthy differences" from Copycat [FCCA p. 390] add to French's six:

- **Proximity groups.** Empty space is evidence: "sheer physical proximity is often a
  sufficient reason to form a group - a proximity group, as we call it - whereas to the Copycat
  program, physical proximity alone can never justify the building of a group of letters" [FCCA
  p. 390].
- **Outside-in grouping.** "Tabletop often builds proximity groups out of objects it hasn't yet
  even looked at individually", then scans the group "to see what sorts of 'fish' it has caught
  in its 'net'", so it can build nested groups from the outside in, or by a mixture of outside-in
  and inside-out [FCCA p. 390]. French's "gestalt" group is the same idea [F95 p. 65] (§4).
- **Losers demoted, not destroyed.** The loser "is merely demoted, in the sense of being removed
  from the special 'elite' area inside the Workspace known as the Worldview" [FCCA p. 390]; the
  Workspace keeps "a 'shadow cabinet'" of structures ready to return [FCCA p. 391]; promotion
  through "a series of graded micro-tests" is reversed gently [FCCA p. 391].

### 2.6 The flow of a run

What the sources say happens between "Do this!" and Eliza's touch, in order. The mechanisms
themselves are specified in §§3-6.

1. **Start.** The program knows the touched object's type and location and nothing else [F95
   p. 64; FH91 p. 177]; "The concept in the Slipnet corresponding to the type of the touched
   object is highly activated" [F95 p. 64]. The other objects are "in its visual field" but
   unknown [FCCA p. 386]. The Coderack initially holds "a number of find-group codelets", giving
   "an initial built-in bias to look for groups of objects", and also pick-salient-object and
   pick-salient-correspondence codelets, which pick an object, group or correspondence "for
   further processing" with a bias toward high salience [F95 p. 65]. The first codelets "are
   thus inspired by the touching-action" [FCCA p. 386].
2. **Scanning.** Scouts look "directly across the table from the object Henry touched",
   "diagonally across the table", and "in the immediate neighborhood of the touched object"
   [FCCA p. 386]; group-finding codelets prefer the corner diagonally opposite the touched object
   [FH91 p. 178]. There is "nothing resembling a systematic or patterned scouring of the table"
   [FCCA p. 386]; some areas may be "left nearly totally unexplored" [FCCA p. 386]. Codelets "do
   not have a global view of the table at any time" [FH91 p. 177]. The factors that draw
   attention to an area include "the presence of objects known to be conceptually similar to the
   touched object and being diagonally or directly opposite the touched object" [FH91 p. 178].
3. **Groups, gestalt first.** A newly found group is first represented "in a 'gestalt' manner":
   the codelet that finds it sends activation to "group" and posts codelets that will "further
   examine the newly found group for more details about its components, internal structure, and
   the like": its end objects, the neighbours of its members, sameness groups inside it [F95
   p. 65]. The program "will almost always discover 'sameness groups'", usually "shared-ISA
   groups" (silverware, crockery), and "only very rarely" groups of unrelated objects [F95
   p. 65]. "Once any group is built, the program devotes resources to looking for identical or
   similar groups in other areas of the table." [FCCA p. 386]
4. **Concepts dispatch codelets; the apple tree.** "An active concept in the Slipnet dispatches
   codelets to look for instances of itself in the Workspace." [F95 p. 64] Finding a group
   activates "group", which sends out more group-finders; as the rate of finding falls, "group"
   decays and fewer are sent, until "the program will thus cease looking for groups, which can be
   taken as a tacit conclusion that there are probably no more groups to be found on the table"
   [F95 p. 64]. The decision is tacit: no codelet sees the whole table [F95 p. 64; FH91 p. 179].
   The same loop runs for every concept: a find-neighbor codelet activates "neighbor", a find-end
   codelet activates "end" and the category it finds, a find-same-obj codelet activates
   "sameness" [F95 p. 63]. Top-down activities "are engendered by the results of observations
   made in a less directed, more bottom-up manner", with "no sharp or absolute distinction between
   bottom-up and top-down processing" [FCCA pp. 386-387].
5. **Correspondences and fights.** A correspondence between two objects "is likely to be built
   if their categories are identical, or are conceptually close", the forgiven mismatch being a
   conceptual slippage [FCCA p. 382]; mirror-image or diagonally opposite positions, and for
   groups similar members or similar structure, count in its favour [FCCA p. 382]. Each has a
   strength "constantly being re-evaluated in light of the latest perceptual events" [FCCA
   p. 382]. "When a rival correspondence is proposed, the existing one and the upstart must
   'fight', and the greater the discrepancy between their strengths, the more likely the stronger
   one is to win the fight." [FCCA p. 382] Groups fight the same way when they claim the same
   items [FCCA p. 383]. Rivals (two correspondences from the same object) cannot share the
   Worldview, like Necker-cube readings; incoherent correspondences can coexist, like the local
   parts of an impossible figure [F95 p. 105].
6. **The Worldview and its rating.** "Gradually, a coherent set of perceptual structures emerges
   in the Workspace; this is called the Worldview" [FCCA p. 387], rated by its structure-value,
   with rewards for correspondences that link two groups element by element and for
   correspondences supported by the same type of slippage [FCCA p. 387]. A coherent Worldview with
   a cup-glass correspondence can beat an incoherent one with a cup-cup correspondence, "even
   though a cup-cup correspondence is a priori stronger" [F95 p. 105], because the program "is
   quite heavily biased against incoherent Worldviews" [F95 p. 105].
7. **Temperature.** It responds to changes of the Worldview and of its quality: adding a strong
   correspondence pushes it down, but a change is "taken by the program as a hint that there may
   be still more such correspondences", which pushes it up [F95 p. 105]. When the Worldview stops
   changing "in the face of this bombardment" of correspondences trying to get in, "the
   conclusion must be that the correspondences in the Worldview are probably pretty good and
   temperature falls" [F95 p. 94]; unlike Copycat, it falls even when "there is very little good
   structure to be found" [F95 p. 94]. Lower temperature means codelet choice respects urgencies
   more [F95 p. 92].
8. **The end.** "Gradually, fewer and fewer new, good structures and correspondences between
   objects are discovered. At this point, the program begins to conclude that, possibly, it has
   found the best way of looking at the situation that it is going to find, the temperature
   begins to fall, and, eventually, a codelet will run that says, 'OK, stop and designate as
   answer whatever is currently mapped onto the touched object.' And this completes the run."
   [F95 p. 55] (§2.7.)

A run that stops early answers by category; "The longer the program runs, the more likely it
will be to find relational structure" [F95 p. 95], the "relational shift" of Gentner and
colleagues [F95 p. 95]. At high temperature the program will try strange things: a cup-spoon
group mapped to a single spoon entered the Worldview "at least for a short time" in Figure 4.14,
which French defends as necessary for fluidity, since "all pathways should always be, at least
theoretically, open to exploration (although not with equal probabilities)" [F95 p. 111].

### 2.7 The end of a run and its output

**Find-answer codelets.** "There is, of course, no sharp temperature threshold below which the
program decides on an answer and above which it continues to build up its representation of the
problem. Rather, on each cycle of the program, a certain number of find-answer codelets are put
on the Coderack. The lower the temperature, the more such codelets are hung on the Coderack.
When a find-answer codelet runs, an answer to the 'Do this!' challenge is picked, and the program
stops." [F95 p. 94] The codelet's function is "to announce the current object corresponding to
the touched object and to stop the program" [F95 p. 105]. "The run ends only when a find-answer
codelet is run." [F95 p. 105]

**The default answer.** "It should be noted that the default object corresponding to the touched
object is the touched object itself. Thus, if a find-answer program runs very early on before
any significant structure is built (unlikely, but theoretically possible), Eliza will elect to
touch the touched object." [F95 p. 105]

**Time pressure.** The program is pressed to maximize structure-value, "counterbalancing this,
however, is a competing pressure - time pressure - that pushes for the program to finish within
a reasonable amount of time" [FCCA p. 387]. No cap on the number of codelets is mentioned
anywhere (→ D-061).

**What a run reports.** "At the end of each run, Tabletop indicates the object touched by Eliza,
the sequence of structures that led to that answer, the amount of resources expended, and the
type and quality of structures present at the end of the run." [F95 p. 145] FCCA's monitor
"recorded the answer given, the final structure-value, and the run-length (i.e., total number of
codelets)" [FCCA p. 392]. Chapter 5 reports, per problem, the frequency of each answer, "The
average quality of the structure built for each answer", and "The average number of codelets
required to produce an answer" [F95 pp. 113-114]; "in general, the better the structure built by
the program, the more codelets will be needed" [F95 p. 114]. The screen dumps show, next to the
Coderack, the number of codelets run so far [F95 p. 96 (img)]. The reconstruction's last trace
line gives the same four things (the object, the structure-value, the codelet count, the
temperature); `docs/trace-format.md` (drafted in item 10, final in item 21) defines it.

**How many runs.** "Almost all of the configurations were run fifty times. No configuration was
ever run fewer than forty times. An experiment consisting of fifty runs of the program usually
took about three hours on a Sun IV workstation." [F95 p. 114] FCCA: "Each problem was run a
total of 50 times." [FCCA p. 392]

**Unspecified here** (decided in §§5-6 and DECISIONS.md): what a "cycle" is, how many
find-answer codelets it posts as a function of temperature, and their urgency (→ D-056); what
is "currently mapped onto the touched object" when the touched object is matched only through a
group (→ D-057, D-058); a run cap (→ D-061).

### 2.8 How the program was judged (FCCA Preface 9)

FCCA's criteria for Copycat and Tabletop are "qualitative but certainly not quantitative" [FCCA
p. 363]: answers should almost always seem plausible, especially those that occur "more than one
time in 100"; the spectrum of human answers should be accessible; obvious answers should be
frequent, far-fetched ones rare, "elegant but subtle" ones rare too; silly answers should be
rated low and elegant ones high; the program should "occasionally come up with insightful or
creative answers" [FCCA pp. 363-364]. Three more go beyond a fixed problem: as a situation is
"tweaked" in stages, frequencies and quality judgments "should shift to reflect this tendency";
the most frequent pathways to an answer should seem plausible; and when features are "lesioned"
one by one, "the behavior should degrade in ways that reflect the theoretical role that each
feature was intended to play" [FCCA p. 364]. Four lesions are named: collapsing scout and
effector codelets into one, giving every concept the same depth, clamping the temperature high
or low, and freezing link lengths [FCCA p. 365]. French's chapter 5 adds the quantitative
"personality profile" (§7), and the validation protocol of this reconstruction uses both
(ANSWERS.md 1).

## 3. The Slipnet

The Slipnet is Tabletop's "permanent concept network" [F95 p. 55], the component that "models
long-term memory" [F95 p. 177]. French's statement of its purpose: "The Slipnet is a model of
human concepts and conceptual associations. One of its principal purposes is to provide a means
of achieving conceptual slippage, the key cognitive mechanism underlying this approach to
analogy-making. Another is to focus and guide processing in the Workspace." [F95 p. 55] It is
"a node-and-link network, each of whose nodes represents the 'core' of a particular concept"
[F95 p. 55]; FCCA adds that "the surrounding 'halo' of other nearby nodes constitutes the rest
of the concept" [FCCA p. 381], and that the Slipnet "is the repository of all information about
concepts, including their proximities, their levels of activation (activation being essentially
an indicator of perceived relevance), their preassigned depths (depth being essentially a
measure of abstraction and generality), and so on" [FCCA p. 381]. Its nodes and links are
permanent: "a network of permanent nodes joined by permanent associative links" [FCCA p. 380];
"During a run, nothing is built or destroyed in the Slipnet, but there is activity of another
sort: the waxing and waning of conceptual activations, and the concomitant stretching and
shrinking of conceptual distances (conceptual depths do not change)." [FCCA p. 381]

The 1991 paper states the coupling with the codelets in both directions. The Slipnet is
"reminiscent of a typical spreading-activation concept network" [FH91 p. 177] (Collins and
Loftus's) "with a number of differences, possibly the most significant being that the distances
between concepts in the Slipnet vary according to pressures evoked by the situation (and
conveyed to the Slipnet by codelets)" [FH91 p. 177], and "In a reciprocal manner, the numbers and the types of active
perceptual agents are governed by the activations of the concepts in the Slipnet." [FH91 p. 177]

What the sources specify about the Slipnet, section by section: the nodes (3.1), their depths
(3.2), the links (3.3), the lengths that change (3.4), the distance over several paths (3.5),
activation (3.6), label nodes (3.7), the initial state (3.8), how concepts post codelets (3.9)
and the context push and pop (3.10). Section 3.11 collects every point the sources leave open,
with the decision that fills it.

### 3.1 The nodes

**The list.** "The Tabletop Slipnet contains forty-six nodes representing the following
concepts. These concepts are possible descriptors of the objects, groups of objects, and other
structures on the table" [F95 p. 62]. The list that follows on the same page has **forty-seven**
entries; FCCA says "The 47 concepts in Tabletop's Slipnet are as follows" [FCCA p. 381] and
lists the same forty-seven under slightly different names. Both lists, grouped here as French's
order suggests, with FCCA's name where it differs and the label the screen dumps use (3.1,
below):

| Group | F95 p. 62 | FCCA p. 381, where it differs | Screen label [F95 p. 98 (img)] |
|---|---|---|---|
| Objects (11) | cup, saucer, plate, knife, fork, spoon, saltshaker, pepper shaker, small glass, big glass, soup bowl | salt shaker | cup, knife, fork, spoon, small glass (the other six are absent) |
| Categories (10) | sharp object, silverware, crockery, liquid holder, food holder, eat-from object, receptacle, buccal object (i.e., can be put in the mouth), open object, closed object | object-that-is-eaten-from, object-that-goes-in-mouth | sharp obj, silverware, crockery, liq holder, food holder, eat-from-ob, receptacle, buccal obj, open obj, closed obj |
| Relations (9) | sameness, opposite, bigger than, smaller than, similar shape to, used with, symmetry, mirror symmetry, diagonal symmetry | same, similar-shape, used-together | same, opposite, big. than, sm. than, sim shape, used with, symmetry, mirror, diagonal |
| Structure and position (12) | group, neighbor, direction, left, right, above, below, horizontal, vertical, position, end, middle | — | group, neighbor, direction, left, right, above, below, horiz, vert, position, end, middle |
| Number (5) | number, one, two, three, many | — | number, one, two, three, many |

The numbers in the first column are this reconstruction's count of French's list; they sum to
47. "Forty-six" against the 47 listed is the first discrepancy; since FCCA, written with
Hofstadter, counts 47 and lists 47, the list is taken as authoritative over the number.
FCCA notes that the set is open: "There is of course nothing sacrosanct about this exact set of
concepts. Many others could easily be added, but such additions would require no changes to the
architecture of the program." [FCCA p. 381] French says the same: "The number of nodes and link
types could, of course, be increased, but the algorithm governing activation spreading and
decay would remain unchanged." [F95 p. 63]

**Names used elsewhere.** The chapter's first description names "the most concrete concepts"
and goes on: "'liquid holder,' 'food holder,' 'sharp object,' 'identity,' 'group,' 'used
together,' 'opposite,' 'diagonal symmetry,' 'mirror symmetry,' 'left,' 'right,' 'end,'
'middle,' and so on" [F95 p. 55]; so "identity" (p. 55) is "sameness" (p. 62) and "same" (the
screen, FCCA), and "used together" (pp. 55, 62) is "used with" (p. 62's list and the screen).
Slippages of a concept to itself are called identities: "When a concept slips to itself (e.g.,
right-right), this is a degenerate case of the notion of slippage, called an identity." [F95
p. 68] The list on p. 62 also has "eat-from object", which p. 55 writes "eat from object" and
the screen "eat-from-ob" [F95 p. 98 (img)].

**The "isolated" node.** The static evaluation of a correspondence uses a node that is in
neither list: "If the 'group' node is active, the former are favored over the latter; if the
'isolated' node is active, the latter are favored; both of these slippages are preferred over
group-to-simple-object slippages." [F95 p. 68] It is the only mention of such a node in F95, FCCA
or FH91 (searched for "isolated"; every other occurrence in F95 is an isolated *object* on the
table, e.g. pp. 31, 33, 36, 97, 101, 118-125, 140); it appears in none of the 41 cells of the
screen's grid. Note the asymmetry: "group" is a node that is jolted when groups are found,
while "isolated" has no codelet, no jolt and no box on the screen. → D-007.

**The screen dumps' grid.** The screen draws the Slipnet as a grid of square boxes: "The
various nodes of the Slipnet are represented by the square boxes forming the grid. The
activation of a node is indicated by the number in the upper right-hand corner of the
corresponding box. This activation is also represented graphically by the black square within
the box." [F95 p. 95] The grid, read from the page images, has seven columns and six rows, the
last row six cells wide: **41** nodes, the same in the three dumps read for this section
(Figs. 4.1, 4.2 and 4.9) [F95 p. 96 (img); F95 p. 98 (img); F95 p. 107 (img)]. Row by row (the screen's labels):

| Row | Column 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|---|
| 1 | spoon | silverware | buccal obj | sim shape | neighbor | horiz | one |
| 2 | small glass | crockery | open obj | used with | direction | vert | two |
| 3 | fork | receptacle | closed obj | symmetry | right | position | three |
| 4 | knife | eat-from-ob | big. than | mirror | left | end | many |
| 5 | cup | liq holder | sm. than | diagonal | above | middle | same |
| 6 | sharp obj | food holder | opposite | group | below | number | — |

[F95 p. 98 (img)]. The six nodes of p. 62's list that the grid lacks are exactly the six object
types absent from the chapter 4 configuration and from most of chapter 5's: saucer, plate,
saltshaker, pepper shaker, big glass and soup bowl. (The configuration of Fig. 4.1 has forks,
knives, spoons, cups and small glasses only [F95 p. 96 (img)].) Whether the program's Slipnet
had 41 nodes in that build or the screen simply omitted six boxes, no page says; the
reconstruction keeps all 47 (→ D-007) and treats the grid as the display of a subset.

### 3.2 Conceptual depth and the three levels

Every node has a fixed depth: "Each concept has a fixed degree of conceptual depth, also
sometimes called semanticity, a real number that represents roughly where the concept lies on
the continuum of concepts, ranging from the most concrete, such as 'cup,' 'fork,' and 'plate,'
to the most abstract, such as, 'eat from object,' 'similar shape to,' and 'opposite from.'"
[F95 p. 55] The ordering French gives: "the concept 'cup' has less conceptual depth than the
concept 'liquid holder,' which, in turn, has less conceptual depth than the concepts 'similar
to' and 'opposite.'" [F95 p. 55] Depth follows Rosch's basic-level distinctions: "The concept
'armchair,' for example, would have less conceptual depth than 'chair,' which in turn would
have less than 'furniture.'" [F95 p. 55] FCCA: depths are "preassigned" [FCCA p. 381] and "do
not change" [FCCA p. 381].

**Three levels.** "In the Tabletop Slipnet, for purposes of simplicity, there are three levels
of abstractness." [F95 pp. 61-62] The heuristics, in French's words [F95 p. 62]:

1. **Level 1, the least abstract:** "Nodes that represent the categories of the actual
   objects on the table (e.g., 'cup,' 'fork,' and other basic-level categories [Rosch 1976])
   are the least abstract." [F95 p. 62] These are the eleven object types.
2. **Level 2:** "Categories at the second level of abstraction are those that contain the
   first-level categories (e.g., 'crockery,' 'silverware,' 'sharp objects')." [F95 p. 62] The
   same level holds the relations among level-1 categories; the examples are "used with" (a
   knife is used with a fork), "bigger than" (a plate is bigger than a saucer), "neighbor"
   (cup1 is the neighbor of fork3) and "left" (cup1 is to the left of fork3) [F95 p. 62].
3. **Level 3, the most abstract:** "at the level of highest abstraction come concepts that are
   typically superordinates of concepts at the second level, or that might be relations
   between relations at the second level (e.g., 'opposite': 'left' is the opposite of 'right';
   'direction': 'left' is a direction)." [F95 p. 62]

What depth does: it sets the decay rate (3.6), and through it the abstractness bias. "Let us
briefly consider the consequences of the third point—namely, that the more abstract a node is
(in other words, the greater its conceptual depth), the more slowly it loses activation. This
means that nodes representing very concrete concepts lose activation quickly and, as a result,
must receive new activation continually to remain active." [F95 p. 56] "The 'abstractness
principle,' in which more abstract relationships are favored over less abstract ones, so
crucial to Dedre Gentner's theory of analogy-making, is thus a natural consequence of decay
rates' varying according to conceptual depth." [F95 p. 56] And the bias is only one factor:
"the bias toward using more abstract concepts is just one factor in determining the quality of
an analogy and can be overridden by other aspects of the situation" [F95 p. 56]. FCCA's second
lesion removes it: "make all concepts have the same conceptual-depth value (thus effectively
abolishing conceptual depth as a motivator)" [FCCA p. 365].

**UNSPECIFIED:** the numeric depth of each level (French calls depth "a real number" [F95
p. 55] and gives none), and the level of each of the 47 nodes beyond the examples quoted
(every object type is level 1; crockery, silverware, sharp object, used with, bigger than,
neighbor and left are level 2; opposite and direction are level 3). → D-008.

### 3.3 Links

**Two kinds, then three.** "Links between nodes in the Slipnet are of only two types: ISA links,
in the tradition of Quillian's semantic networks (Quillian 1968), and labeled links. ISA links
are links that designate category membership—for example, 'robin ISA bird' and 'bird ISA
flying animal.' For labeled links, the label indicates the type of relationship that the link
encodes. Each label is itself a concept and hence is represented by a node." [F95 p. 56] The
list on pp. 62-63 counts three, adding the inverse of ISA [F95 pp. 62-63]:

- **Labeled links:** "These are links controlled by label nodes like 'opposite,' 'used with,'
  'bigger than,' 'smaller than,' and so on." [F95 p. 62]
- **ISA links:** "These are standard ISA links between categories and their superordinate
  categories—for instance, 'cup ISA liquid holder.'" [F95 p. 63]
- **Has-member links:** "These are the opposite of ISA links—for instance, 'liquid holder
  HAS-MEMBER cup.'" [F95 p. 63]

"Every link in the Slipnet also has a length." [F95 p. 56]

**The labeled-link structure (Figs. 3.1-3.3).** Figure 3.1 draws two core nodes, node1 and
node2, joined by a horizontal link, with a "Label Node" above joined to the link by a dashed
line; the link is called "a labeled link (or sliplink)" [F95 p. 57 (img)]. Figure 3.2 gives the
two Copycat examples, "A [is the] successor [of] B" and "successor [is the] opposite [of]
predecessor" [F95 p. 57], each as a labeled link with its label above [F95 p. 57 (img)];
Figure 3.3 hooks them together: "Opposite" labels the Successor-Predecessor link, and
"Successor" labels the A-B link [F95 p. 58 (img)]. "The Copycat and Tabletop Slipnets contain
many instances of the abstract structure shown in figure 3.1." [F95 p. 57]

**Why ISA links were added.** "The original design of the Tabletop Slipnet was identical to
Copycat's original Slipnet in that it used only relational links." [F95 p. 59] The label
between "fork" and "spoon" was then "utensils that go in mouth", which does not fit the
relational form (a fork is not the utensils-that-go-in-mouth of a spoon) [F95 p. 59]; "Many of
the labels in the original Tabletop Slipnet were ISA links, for which relational labeling was
inappropriate." [F95 p. 59] Relational links remain, French's three examples being that a
knife is used with a fork, a cup is used with a saucer, and a saucer is smaller than a plate
[F95 p. 59]. "The
overriding difference, then, between Tabletop's Slipnet and Copycat's Slipnet was that ISA
links are more common in the former." [F95 p. 59] Chapter 2 says the same under "Blurrier
categories": "the Tabletop concept network contains far more ISA links (indicating membership
in one category in another [Quillian 1968]) than the Copycat concept network does. In Tabletop,
a single category is often associated with a number of different superordinate categories"
[F95 p. 42], with the examples of the soup bowl and the knife below.

**Every link the sources name.** No page gives the link table. The links the text and figures
assert, each with its page, are the only ones known:

| Link | Kind | Source |
|---|---|---|
| soup bowl ISA liquid holder; soup bowl ISA food holder; soup bowl ISA crockery | ISA | "a 'soupbowl' is a 'liquid holder,' but it is also a 'food holder' as well as 'crockery'" [F95 p. 42] |
| knife ISA silverware; knife ISA sharp object | ISA | "a 'knife' is 'silverware' as well as a 'sharp object'" [F95 p. 42] |
| cup ISA liquid holder (and liquid holder HAS-MEMBER cup) | ISA | [F95 p. 63] |
| small glass ISA liquid holder; big glass ISA liquid holder | ISA | Fig. 3.5 [F95 p. 60 (img)] |
| small glass — big glass, labeled "smaller-than" | labeled | Fig. 3.5 [F95 p. 60 (img)] |
| knife — fork, labeled "used with" ("used together") | labeled | [F95 p. 59; F95 p. 62]; "they are both items of silverware and are used together" [F95 p. 117] |
| cup — saucer, labeled "used with" | labeled | [F95 p. 59; F95 p. 62]; "a cup and a saucer are both types of crockery and are used together" [F95 p. 119] |
| saucer — plate, labeled "smaller than" | labeled | [F95 p. 59] |
| fork ISA silverware; saucer ISA crockery; cup ISA crockery | ISA | from the two sentences above [F95 p. 117; F95 p. 119] |
| left — right, labeled "opposite" | labeled | "'opposite': 'left' is the opposite of 'right'" [F95 p. 62] |
| left — right, also labeled "diagonal symmetry" | labeled | the value of a left-right slippage "depends on how active the 'diagonal symmetry' and 'opposite' nodes are, these concepts being the ones that link the two concepts 'left' and 'right' in the Slipnet" [F95 p. 68] |
| left ISA direction | ISA | "'direction': 'left' is a direction" [F95 p. 62] |
| the number nodes are linked: "two" to "two" is closer than "one" to "two" | unstated | "the conceptual distance between 'two' and 'two' in the Slipnet is less than the distance between 'one' and 'two'" [F95 p. 68] |
| spoon and knife share only silverware; forks and spoons share "that they can hold food" | ISA (FCCA's reading) | "Spoons and knives, aside from being silverware, share only the curious and much less meaningful connection of sitting together on the same side of a plate" [FCCA p. 389] |

FCCA's characterization of the whole: "The rest of the Platobet of Tabletop - its Platonic
repertoire - is a similar hodgepodge of concepts with vague, blurry interconnections." [FCCA
p. 389]

**What the text says about distances between object types.** These are the qualitative
constraints chapter 5 states (they are what the link table must reproduce; none gives a
number):

- "the link between 'knife' and 'fork' is quite short, whereas the path (i.e., a series of
  simple links, since there is no direct link) from 'knife' to 'saltshaker' is very long" [F95
  p. 59]; knives and forks are "conceptually fairly close" [F95 p. 117].
- "the concepts 'cup' and 'small glass' are very close in the Slipnet" [F95 p. 122]; "a cup is
  conceptually very close to a glass" [F95 p. 126].
- "In the Slipnet, the conceptual distance between 'cup' and 'saucer' is small." [F95 p. 119]
  "For the Tabletop program, these two concepts are almost as close together, albeit for
  different reasons, as the concepts 'cup' and" small glass [F95 p. 119].
- "plates are conceptually quite far removed from cups" [F95 p. 120]; yet "cups and plates are
  at least weakly related conceptually" [F95 p. 139].
- a saltshaker "conceptually is completely dissimilar to" a cup [F95 p. 121].
- "a spoon is conceptually very far from a glass" [F95 p. 125].
- "forks and spoons are conceptually relatively close" [F95 p. 135].
- "a spoon is conceptually very distant from a saltshaker" [F95 p. 136]; "Saltshakers are
  conceptually very far from spoons" [F95 p. 139].
- "A cup is conceptually much closer to a glass than to a saltshaker" [F95 p. 136].

**What the first screen dump shows.** Before any codelet has run, with "cup" at 51, the nodes
that have received spread activation are liquid holder (4), crockery (2), open obj (2),
eat-from-ob (2) and small glass (2); receptacle, buccal obj, food holder and every other node
are at 1 [F95 p. 96 (img)]. This is the direct evidence for the cup's neighbourhood: cup is
nearest to liquid holder, then (equally, at this resolution) to crockery, open object,
eat-from object and small glass. The text says the same in words: "some activation has already
begun to spread from 'cup' to 'liquid holder,' 'crockery,' and so on" [F95 p. 97].

**UNSPECIFIED:** the full link table (which pairs are linked, by which label or ISA), and
every default length. → D-009 (the table, with the constraints above as tests), D-010 (the
lengths).

### 3.4 Lengths that change with the label's activation

"One crucial feature of the Tabletop Slipnet that sets it apart from other semantic network
architectures (with the exception, of course, of Copycat) is the length of any labeled link
varies dynamically as a function of the activation of the link's label node. This feature
allows the Slipnet to adapt to the specific situation (i.e., the analogy problem) with which it
is presented." [F95 p. 56] In Copycat's terms: "The conceptual distance between a given pair of
concept-core nodes in Copycat is completely controlled by the level of activity in the label
node." [F95 p. 58] Figure 3.4 draws it twice: with "Opposite" not very active, Leftmost and
Rightmost are far apart; with "Opposite" very active, "the concepts 'leftmost' and 'rightmost'
are much closer together, i.e., 'leftmost' can slip to 'rightmost' or vice versa more easily
than before" [F95 p. 58 (img)]. Distance is slippability: "The conceptual distance between two
concepts measures how easily the first concept can be replaced by ('slipped to') the second in
the context under consideration." [F95 p. 58]

A label shrinks every link it labels: "This will then shrink the length of all of the links
labeled by the label node. For example, the concepts 'fork' and 'knife' are joined by a link
labeled 'used together.' If the slippage from 'knife' to 'fork' is part of the description of
one of the analogical mappings, this will spread activation to the concept 'used together.'"
[F95 p. 62] "But the concept 'used together' also labels the link joining the two concepts
'cup' and 'saucer.' Because 'used together' is active, not only will the link between 'knife'
and 'fork' shrink but so will the link between 'cup' and 'saucer.' This is the way the Slipnet
modifies its structure to reflect context." [F95 p. 62] The shrinking is not permanent: in the beer and
driver's-license example, "When the 'American university' context decays, the original, long
default distance returns." [F95 p. 56] FCCA lists the same activity, "the adjustment of
interconceptual distances in accordance with the activation levels of certain key concepts"
[FCCA p. 381], and its fourth lesion removes it: "prevent link-lengths in the Slipnet from
changing at all during a run" [FCCA p. 365]. Only labeled links are said to change; ISA and
has-member links have no label, and no page describes a change in their length. Chapter 2 asks
only that the many ISA links be taken into account: "The way in which activation spreads and
link lengths shrink must take these differences into account." [F95 p. 42]

**UNSPECIFIED:** the function from the label's activation to the link's length (Copycat's rule,
a linear shrink, or another). → D-011.

### 3.5 Conceptual distance over several paths

**Default distance is path length.** "This notion is implemented in the Tabletop Slipnet by
means of links (or series of links) of various lengths. The length of the path between two
concepts represents the default distance between them." [F95 p. 59]

**All pathways count.** The question "what happens if there are multiple pathways between a
given pair of nodes?" [F95 p. 60] is illustrated by Figure 3.5: "small-glass" and "big-glass"
are joined by a direct link labeled "smaller-than" (the label above, on a dashed line), and
each has an ISA link down to "liquid-holder" [F95 p. 60 (img)]. The answer "led to the
generalization of the Copycat Slipnet, the heart of which is the following premise: All
pathways contribute to reducing the conceptual distance between nodes; the amount of activity
spread between nodes is a function only of the conceptual distance between the nodes." [F95
p. 60] "All pathways between a given pair of nodes, regardless of their component links' types,
help determine the conceptual distance between those nodes." [F95 p. 60] "in the Slipnet, each additional pathway between two" [F95 p. 60] "nodes decreases the
conceptual distance between the nodes. This stands to reason if one considers that if two nodes
have many pathways joining them through ISA links and labeled links, they are more similar than
two nodes that do not." [F95 p. 61]

**The formula (Figs. 3.6 and 3.7, read from the images).** Figure 3.6 draws nodes A and B with
three pathways: a direct link A-B of length d3, labeled by a node n3 above it (dashed line);
a two-link path through n2, with A ISA n2 of length d2 and B ISA n2 of length d2'; and a
two-link path through n1, with A ISA n1 of length d1 and B ISA n1 of length d1'. The legend
says "d_i are the lengths of the links" [F95 p. 61 (img)]. Figure 3.7 draws three resistors
r1, r2, r3 in parallel [F95 p. 61 (img)]. The resistor formula and the distance by analogy with
it, as printed [F95 p. 61 (img)]:

    R = 1 / (1/r1 + 1/r2 + 1/r3)

    Distance(A,B) = 1 / ( 1/(d1 + d1') + 1/(d2 + d2') + 1/d3 )

The sentence under the formula reads "where di is the distance between A and ri, dj is the
distance between ni and B" [F95 p. 61], on the page as printed as well as in the text layer (the
citation audit, `docs/citation-audit.md`, read it at 400 dpi: the subscripts are "r_i" and
"d_j"). Figure 3.6 itself labels the links d_1, d_1', d_2, d_2' and d_3, with "d_i are the
lengths of the links" [F95 p. 61 (img)], and the formula's denominators are d_1 + d_1' and
d_2 + d_2', so the sentence's "r" and "j" are the book's own typesetting slips for n_i and
d_i'; the figure and the formula are followed. In words: a path's length is the sum of its links' lengths;
the distance between two nodes is the parallel combination (the reciprocal of the sum of
reciprocals) of its pathways' lengths. Figure 3.6 shows pathways of one link and of two links
through ISA links; the "premise" of p. 60 says *all* pathways, "regardless of their component
links' types" [F95 p. 60].

**UNSPECIFIED:** which pathways enter the sum (only one- and two-link paths as in Fig. 3.6, or
all simple paths to some length), whether a path may use has-member links, labeled links or
both directions of an ISA link, and when the distances are recomputed as lengths change.
→ D-012.

### 3.6 Activation

**Three sources.** "At any given time, each node in the Slipnet has a particular level of
activation. The amount of activation of each node is determined by three factors:" [F95 p. 55]

1. "Activation added by low-level perceptual agents (codelets) whenever an instance of the
   particular concept is discovered or noticed in the work space." [F95 p. 55]
2. "Activation added by activation-spreading from nearby concept nodes in the net itself." [F95
   p. 55]
3. "Decay of activation, the rate of which is a function of the conceptual depth of the node.
   The idea is that highly 'semantic' or abstract concepts, once active, will continue to
   influence the perception of a situation longer than concrete concepts." [F95 p. 56]

FCCA's list is the same, with spreading limited to neighbours: "the sending of a jolt of
activation to a concept, at the moment an instance of it is perceived in the Workspace; the
gradual decay of activation of any concept, at a rate determined by its depth; the flow of
activation from any concept to its immediate neighbors, gated by conceptual proximity" [FCCA
p. 381].

**Range.** "Activation values range from 1 to 150." [F95 p. 95] The screen dumps confirm both
ends: most nodes sit at exactly 1 [F95 p. 96 (img); F95 p. 98 (img)], and "right" and "opposite" both show
150 in Fig. 4.9 [F95 p. 107 (img)], so 150 is a ceiling that is reached, not a scale.

**Jolts.** Codelets jolt the node of what they notice: find-group codelets send activation to
"group", find-neighbor to "neighbor", find-end to the end object's category and to "end",
find-same-obj to "sameness" [F95 p. 63] (the catalogue is in §5). The size is a constant: the
find-group codelet of Fig. 4.2 "has sent a standard jolt of activation to the 'group' node in
the Slipnet" [F95 p. 97]; the grid shows "group" at 44 two codelets into the run, from 1 at the
start [F95 p. 96 (img); F95 p. 98 (img)]. A node jolted once and never again is meant to fade: "If a node in
the Slipnet is sent activation by a codelet very early on (i.e., if an instance of that concept
was noticed) and if no instance of the same concept is noticed again, this would indicate that
the concept is probably not very important and should therefore not remain active." [F95 p. 62]

**Spreading.** "In Tabletop's Slipnet, activation spreads from one concept to another in a
manner inversely proportional to the distance between the two concepts." [F95 p. 59] Against
connectionist weights: "distance can be likened to the reciprocal of weight, rather than
weight itself" [F95 p. 59]; "the amount of activation spread is (essentially) inversely
proportional to the distance between concepts, although, of course, the node cannot spread more
activation than it possesses" [F95 p. 59]. Activation "radiates": "The closer the two nodes are
together, the more activation is radiated from the 'warmer' node to its neighbor." [F95 p. 60]
The statement after the formula: "Assuming that the distance from node A to node B is dA,B,
then the amount of activation that spreads from A to B is proportional to the activation of A
divided by dA,B." [F95 p. 61] No markers: "in the Copycat and Tabletop Slipnets, no markers are
passed; rather, activation is spread" [F95 p. 60].

**Decay.** "Since all nodes lose activation through decay, the node will thus tend to return to
dormancy unless it is resupplied with activation either from other active nodes (i.e.,
spreading activation) or from codelets examining the table. The greater the conceptual depth
of a node, the less its activation will decay with each cycle." [F95 p. 62] "In the Slipnet,
activation decays as a function of the conceptual depth (abstractness) of the concept in
question." [F95 p. 61] "Dormancy" and the grid's floor of 1 agree: a node decays towards 1,
not 0.

**Activations and the Workspace.** "The activation levels of concepts in the Slipnet reflect
the presence of instances of those concepts in the Workspace." [F95 p. 62] Activation also acts
back: the salience of an object counts the "Activation of the concept corresponding to the type
of the object" [F95 p. 66]; the value of a position slippage depends on the activation of the
nodes that link the slipped concepts [F95 p. 68]; and active concepts post codelets (3.9).

**UNSPECIFIED:** the constant of proportionality in spreading and whether it reaches only
immediate neighbours (FCCA) or every node at a distance (F95's "radiating"); the decay
function of depth and its rate "with each cycle"; what a cycle is (after every codelet, or
every n codelets); the size of "a standard jolt" and whether every codelet type sends the same;
the arithmetic of the clip to [1, 150]. → D-013 (spreading), D-014 (decay), D-015 (range and
floor), D-016 (jolts), D-019 (the update schedule).

### 3.7 Label nodes

A label node is activated by slippages that use its link: "If both of the concepts joined by a
labeled link play an active role in a correspondence—for example, if the leftmost object of
one group is mapped to the rightmost object of a second group, so that 'left' slips to
'right'" then "activation is sent to the label node associated with that link" [F95 p. 62]
(the sentence is split here because the text layer runs its two halves together). The second
route is passive: "(In addition, whenever both of the concepts joined by a labeled link are
active, a small amount of activation is also sent to the label node.)" [F95 p. 62] The
consequence is 3.4's shrinking of every link the label controls [F95 p. 62]. The symmetries
are the labels that chapter 5 and section 3.10 rely on: the slippage left-to-right feeds
"diagonal symmetry" and "opposite", and identity slippages such as right-to-right feed "mirror
symmetry" [F95 p. 68; F95 p. 85; F95 pp. 88-89]: "Both of these identity slippages will cause activation to
be sent to the 'mirror symmetry' concept node in the Slipnet. The concept 'mirror symmetry' will
therefore become very active." [F95 p. 85] The activations of "opposite" and "diagonal
symmetry" also depend "on how many other correspondences in the Worldview have 'opposite' and
'diagonal-symmetry' slippages" [F95 p. 68].

**UNSPECIFIED:** how much a slippage sends to its label (the same jolt as 3.6, or another
amount), the "small amount" of the passive route and the threshold of "active" that triggers
it. → D-016, D-018.

### 3.8 The initial state

"Initially, the program explicitly knows only the location and object type of the touched
object. The concept in the Slipnet corresponding to the type of the touched object is highly
activated." [F95 p. 64] "Certain concepts in the Slipnet—in particular, the concept 'opposite'
and the category of the touched object—are active initially. 'Opposite' is active because,
given that Eliza is sitting opposite Henry, there should be a natural tendency to look for
groups and examine objects on the opposite side of the table from Henry. Activating 'opposite'
encourages just that." [F95 p. 66] Chapter 4 narrates it: "the node 'opposite' in the Slipnet
has been given a large amount of activation. The reason for this is that Eliza is sitting
opposite Henry at the table. In addition, since it was a cup that Henry touched, the concept
'cup' is active. Even though no codelets have yet run, some activation has already begun to
spread from 'cup' to 'liquid holder,' 'crockery,' and so on." [F95 p. 97]

The two first screen dumps give the numbers (the touched object is a cup; the Slipnet grid of
3.1):

| Node | Fig. 4.1, 0 codelets run [F95 p. 96 (img)] | Fig. 4.2, 2 codelets run [F95 p. 98 (img)] |
|---|---|---|
| opposite | 139 | 139 |
| cup | 51 | 89 |
| group | 1 | 44 |
| horiz | 1 | 22 |
| liq holder | 4 | 9 |
| spoon, fork | 1, 1 | 9, 9 |
| knife | 1 | 5 |
| small glass, crockery, open obj, eat-from-ob | 2 each | 4 each |
| silverware, closed obj, right, above | 1 each | 2 each |
| every other node | 1 | 1 |

So at the start "opposite" is near, not at, the ceiling, "cup" is about a third of it, the
cup's neighbours have received a little spread, and every other node is at the floor. Two
codelets later (one a find-group that found the group of the touched cup [F95 p. 97], the other
unnamed) "cup" has risen from 51 to 89, "group" and "horiz" have been jolted, and the types of
the group's members (fork, spoon, knife) have risen; what each of the two codelets sent is
read in §6c (item 05). The thermometer stands at 80.0 in Fig. 4.2 [F95 p. 98 (img)].

**UNSPECIFIED:** the initial values (whether 139 and 51 are the values set, or larger values
after a decay step), whether they are clamped, and whether the Slipnet is updated (spreading
and decay) before the first codelet runs, as the 4s and 2s of Fig. 4.1 suggest. → D-017.

### 3.9 Concepts post codelets

The Slipnet drives the Coderack in two ways [F95 p. 63]: "Specifically, codelets are posted to
the Coderack when" (1) "The activation of a particular concept in the Slipnet is high; codelets
associated with that concept are posted to the Coderack (e.g., when the concept 'group' is
sufficiently active, find-group codelets are posted to the Coderack)." [F95 p. 63] and (2) a
codelet "runs and succeeds in doing the task for which it was designed; this codelet then posts
a new set of codelets to the Coderack" [F95 p. 63]. The first is the Slipnet's: "An active
concept in the Slipnet dispatches codelets to look for instances of itself in the Workspace."
[F95 p. 64] Its feedback loop, in the case of groups: "If one of these codelets succeeds in
finding a group, this causes more activation to be sent to the concept 'group' in the Slipnet.
This amounts to a signal to the system to hunt for even more groups; thus more codelets are
sent out to look for groups." [F95 p. 64] "Eventually, however, as the rate of group-finding
falls (i.e., the rate of change of activation of 'group' decreases) and less activation gets
sent to 'group'—and thus the activation of 'group' decays—fewer codelets will be sent out to
look for groups." [F95 p. 64] The 1991 paper says "proportionately fewer codelets are sent out to look for groups"
[FH91 p. 178]. Chapter 4 shows it: "The concept 'group' is fairly active, since a find-group
codelet was successful (a group was found); the Slipnet node 'group' has thus spawned a fair
number of similar find-group codelets." [F95 p. 97] and "the concept 'neighbor' is active,
causing more 'find-neighbor' codelets to be posted on the Coderack" [F95 p. 97]. The apple
picker is the image for the tacit end of the search [F95 p. 64; FH91 p. 178].

Which codelets a concept posts is given only by example: group → find-group [F95 pp. 63-64],
neighbor → find-neighbor [F95 p. 97]; the other codelet types "associated with" a concept are
not listed (the catalogue of about thirty types is §5's).

**UNSPECIFIED:** the table from node to codelet type, the threshold of "sufficiently active",
how many codelets a posting sends and at what urgency, and whether a concept posts on every
cycle while it stays above the threshold. → D-020.

### 3.10 One node, several activations: the context push and pop

French's section 3.4.6, "The Problem of Single Nodes with Multiple Activations" [F95 p. 82],
starts from the premise that "humans have only one 'copy' of each concept in long-term memory"
[F95 p. 82] while "there can be any number of tokens of a particular type in working memory"
[F95 p. 83]. The difficulty: concepts "seem capable of suddenly changing their level of
activation in the presence of a new context and then reverting to their former state when the
new context is no longer present" [F95 p. 83]; "Can the mind maintain something like a stack of
activation values for each concept, or must it reconstruct the earlier activation level 'from
scratch' when the second context disappears?" [F95 p. 83] The everyday examples are the west
exit on the way to Ann Arbor [F95 p. 83], the two Davids [F95 p. 84] and driving on the left in
England [F95 p. 84].

**Where it arises in Tabletop** [F95 pp. 84-87]. In the configuration of Fig. 3.17 the program
has built two mirror-symmetric correspondences, cp1 between the silverware groups at the left
and cp2 between the cups at the right; "Both of these identity slippages will cause activation
to be sent to the 'mirror symmetry' concept node in the Slipnet." [F95 p. 85] "When the
correspondence cp1 was built, a number of examine-cp(cp1) codelets were posted to the
Coderack. The role of these codelets is to take the group-level correspondence and whether
there are any correspondences to be built between the objects making up the connected groups."
[F95 p. 85] "in some sense, it is as if when examine-cp(cp1) runs, it enters a subset of the
full Tableworld, a smaller world consisting of just the two groups in question (figure 3.19)"
[F95 p. 85]. Inside, the obvious mapping is diagonal, against the prior activation of mirror
symmetry: "Humans seem to treat the two contexts as esssentially independent, meaning that they
would ignore the prior activation of 'mirror symmetry' and go for the diagonal mapping, and
that is therefore how they are treated by Tabletop." [F95 p. 86] French's survey on Fig. 3.20:
object-type pressure beats size pressure 62 percent to 38 percent without the cups; with the
mirror-symmetric cup-cup mapping present, 56 percent chose the mirror-symmetric mapping within
cp1, so the parent context's activation "apparently has only a slight influence on the
lower-level mappings within cp1" [F95 p. 86]. "In sum, it is as if a single concept—a single
node in the Slipnet—could have two different activation levels, one for the 'main world' and
one for the 'subworld.'" [F95 p. 87]

**The two questions** [F95 p. 87]: what happens to a concept's activation when the program
enters a new level ("a 'context push'"), and how the former levels are restored when it leaves
("a 'context pop'").

**Push.** "The answer to the first question is that the activation level is set to its initial
value (in this case, 1). So, in Tabletop, the activation slate really is wiped clean." [F95
p. 87] Which concepts: of the two proposals, reinitializing "Only concepts whose activation is
above a certain threshold level" or "Only concepts that participate in the supporting slippages
of the correspondence under investigation" [F95 p. 88], "The solution adopted by Tabletop was
essentially the latter. In reality, however, only the 'mirror-symmetry' and 'diagonal-symmetry'
nodes are reinitialized, these being the only concepts that would have had a significant
influence on the subsequent formation of 'sub-correspondences'." [F95 p. 88] (This also says
the initial value of a node is 1, confirming 3.6's floor.)

**Pop.** French rejects an explicit activation stack and a self-watching "notebook" [F95
p. 88]: "stacks are too 'clean.'" [F95 p. 88] "An alternative notion seems more reasonable: the
program inspects the structures present in the Workspace and then reconstructs the old
activations on the basis of what it has observed. This is the solution adopted in Tabletop."
[F95 p. 88] "When the program leaves one context, it reinitializes the activation values of the
concepts associated with the correspondences in that context. When it then reenters a former
context (i.e., 'pops' to a previous context), it reconstructs the activation values at that
level by looking around in the Workspace and finding the correspondences in that context."
[F95 p. 88] "It then reactivates concepts—in Tabletop, only 'mirror symmetry' and 'diagonal
symmetry' are affected—on the basis of the slippages that support the correspondences it
finds." [F95 p. 88]
The amounts: "for each 'right'-to-'left' slippage, it puts a fixed amount of activation into
the concepts 'right,' 'left,' and 'diagonal symmetry.'" [F95 p. 88] and "for each
'right'-to-'right' slippage it finds, it puts a fixed amount of activation into 'right' (twice)
and into 'mirror symmetry.' In this way, it roughly rebuilds the activation profile of that
level." [F95 p. 89] So although "only" the two symmetries are named as affected, the pop also
puts activation into "right" and "left".

**French's own verdict (a fault, fixed by ANSWERS.md 2).** "A more complex reconstruction
technique than the one implemented in Tabletop is needed, however. It is not enough merely to
put a fixed amount of activation into the Slipnet for each concept encountered in the
structures present at a particular level of the Worldview." [F95 p. 89] "Although this
certainly captures some of the information stored in the prior patterns of activation, too much
is lost. For example, all of the (possibly considerable) activation that might have spread to
these concepts from other concepts is lost." [F95 p. 89] He keeps to reconstruction over stacks: "I believe
that reconstruction techniques, rather than any resort to an 'activation stack,' will provide
the ultimate resolution to this difficulty." [F95 p. 89] This is one of the faults listed in
§6b: the default profile keeps the reconstruction (French rejects stacks) and adds to the fixed
amounts one spreading step from the neighbours of the reinitialized nodes, the refinement he
names on p. 89; the fixed amounts alone stay reachable in the `french-faults` profile (→ D-021;
§6b, fault 4, has the reasoning).

**UNSPECIFIED:** when a serial program "enters" a context (the examine-cp codelet's run, or
every codelet that works inside a group correspondence) and when it "leaves" it; whether a
context can nest (groups of groups); the "fixed amount"; whether the push resets the two
symmetries only, as p. 88 says, or every concept in the supporting slippages, as the adopted
proposal says. → D-021.

### 3.11 The unspecified points of the Slipnet

| Point | Where the sources stop | Decision |
|---|---|---|
| The node set: 46 or 47; "isolated"; the 41-cell grid | [F95 p. 62; F95 p. 68; F95 p. 98 (img); FCCA p. 381] | D-007 |
| The depth of each level and the level of each node | [F95 p. 55; F95 pp. 61-62] | D-008 |
| The link table | [F95 p. 42; F95 pp. 59-63; F95 p. 68; F95 pp. 117-139; FCCA p. 389] | D-009 |
| The default lengths | [F95 p. 56; F95 p. 59] | D-010 |
| The length as a function of the label's activation | [F95 p. 56; F95 p. 58; F95 p. 62] | D-011 |
| Which pathways enter the parallel-path distance | [F95 pp. 60-61 (img)] | D-012 |
| Spreading: the constant, the reach, the buffering | [F95 pp. 59-61; FCCA p. 381] | D-013 |
| Decay as a function of depth; its rate | [F95 p. 56; F95 pp. 61-62] | D-014 |
| The range 1-150, the floor, the clip | [F95 p. 87; F95 p. 95; F95 p. 96 (img); F95 p. 107 (img)] | D-015 |
| The size of a jolt; the amount a slippage sends to its label | [F95 pp. 62-63; F95 p. 97] | D-016 |
| The initial activations of "opposite" and the touched type | [F95 p. 64; F95 p. 66; F95 pp. 96-98 (img)] | D-017 |
| The "small amount" sent to a label whose two concepts are active | [F95 p. 62] | D-018 |
| The update schedule ("each cycle") | [F95 p. 62] | D-019 |
| The node-to-codelet table and the posting rule | [F95 pp. 63-64; F95 p. 97; FH91 p. 178] | D-020 |
| Context push and pop: when, what, how much; the lossy pop fixed | [F95 pp. 85-89] | D-021 |

## 4. The Workspace, groups, salience, correspondences and the Worldview

This section covers French's sections 3.4.2 to 3.4.4 and the opening of 3.4.5 [F95 pp. 63-75],
with chapter 4's statements of mechanism [F95 pp. 97-111] and chapter 5's where they say what
the program does; FCCA's "Structures, Strengths, and Fights for Survival" [FCCA pp. 382-383],
its lists of salience and urgency factors [FCCA p. 385], "The Gradual Emergence of Coherence"
[FCCA pp. 386-387], "Tabletop Gauges its Own Progress" [FCCA p. 387] and the architectural
differences from Copycat [FCCA pp. 389-391]; and FH91's account of scanning [FH91 pp. 177-178].
The Coderack and the codelets' urgencies are §5; temperature and the answer are §6; the screen
dumps are read dump by dump in §6c.

### 4.1 The Workspace and what lives in it

French keeps Copycat's metaphor: "The Copycat Workspace was originally called the 'Cytoplasm'
in Hofstadter's 1984 Copycat Memo" [F95 p. 63], and "it is instructive to liken the Tabletop
Workspace to a cell's cytoplasm" [F95 p. 63]. One should think of "a raw, unstructured table
configuration as being immersed in a 'seething broth' of codelets qua enzymes, all of which are
attempting to help build the representation of the table configuration" [F95 p. 63]; the image
is "one of parallel, local activity of codelets, resulting in a high-level product, much like
enzymes collectively producing a new strand of DNA" [F95 p. 63]. In the paper's words, "the
Slipnet contains Platonic concepts, while the Workspace contains instances of those concepts,
which codelets observe and manipulate" [FH91 p. 177].

What the Workspace holds, in the sources' vocabulary:

- **Descriptors** attached to objects and groups (§4.3): the Slipnet's concepts "are potential
  descriptors, both concrete and abstract, of individual objects, of relations between objects,
  of pairs of objects, of groups of objects (or of groups of groups), and of correspondences
  between objects (or between groups)" [FCCA p. 381].
- **Groups** of objects, and groups of groups (§4.4), drawn on the screen as boxes with wavy
  borders, with subgroups as dashed boxes inside them [F95 p. 71 (img)].
- **Correspondences** between objects, between groups, or between an object and a group (§4.6):
  "Correspondences between objects (or groups of objects) are high-level structures, much as
  groups of objects are high-level structures." [F95 p. 67] A correspondence in the Worldview is
  a solid line, a correspondence outside it a dashed line: "Correspondences in the Worldview are
  always indicated by solid lines connecting objects or groups of objects." [F95 p. 71]
- **The Worldview** (§4.7), "the special 'elite' area inside the Workspace known as the
  Worldview" [FCCA p. 390]. Outside it, "many perceptual fragments are free to float around in
  it without necessarily fitting into any larger perceptual whole" [FCCA p. 391].

Every structure carries a number. Objects and groups have a **salience** (§4.5); groups also
have a **strength** in FCCA's account (§4.4.3); correspondences have a **strength**, which
French also calls their rating or their salience (§4.6); the Worldview has a **rating**, FCCA's
**structure-value** (§4.7.5). Objects, groups and correspondences are "rated by the program in
much the same manner" [F95 p. 67]: a sum of factors, each a real number.

### 4.2 What is known at the start, and scanning

**The start.** "Initially, the program explicitly knows only the location and object type of
the touched object." [F95 p. 64] "It is not aware of the other objects, their positions on the
table, their groupings, and so on. Such things will be discovered during the run of the
program." [F95 p. 64] The paper says the same: "At the start of a run, the program knows only
which object was touched by Henry and where on the table that object is located." [FH91 p. 177]
FCCA explains how to read the input file: the data "representing the categories, positions, and
orientations of all the table objects should not be thought of as what the program is aware of,
but rather as the physical situation itself" [FCCA p. 386], something like retinal information;
the program "ignores everything except the most salient features" [FCCA p. 386]. In the Slipnet,
the touched type and "opposite" are active (§3.8).

**Regions.** "The table is divided into nine regions like a tic-tac-toe board, although the
boundaries between regions are intentionally blurry." [F95 p. 65] "In other words, an object
that is actually in one region but very close to a region boundary may sometimes be considered
by the program to be in the adjacent region." [F95 p. 65] (§1.3 has the rest; the blur is
D-001.)

**Scanning.** The first codelets "scan the table in a biased manner, giving probabilistic
preference to certain areas of the table" [FCCA p. 386]: some look directly across, some
diagonally across, some in the touched object's neighbourhood (§2.6, step 2). They are "scouts
probabilistically scanning the table for information such as what objects are in interesting
areas, what objects are neighbors of interesting objects, what objects might potentially be
grouped together into chunks, and what objects resemble each other" [FCCA p. 386]. As objects of
interest are found, "the diffuse searchlight of attention wanders around the table in a
semi-purposive manner and many items are at least given the once-over" [FCCA p. 386], but with
"nothing resembling a systematic or patterned scouring of the table" [FCCA p. 386], so that
some areas may be "explored on a superficial level and then essentially abandoned, out of lack
of interest" [FCCA p. 386]. "The neighborhoods of objects considered interesting fill up rapidly
with perceptual information, so that recognition of relationships between objects, followed by
the building of groups of objects, is likely to take place there earliest." [FCCA p. 386]

The paper describes the same scan as top-down: "The initial codelets that run may be thought of
as scouts examining the table for information" [FH91 p. 177]; "Depending on their relation to
the touched object, certain areas of the table are given preference and will generally be
examined first." [FH91 p. 177] It is "a scan with top-down control" [FH91 p. 177], one "in which
the salience of objects preferentially determines which objects will be attended to first, and,
significantly, one in which some objects, or even groups of objects, might sometimes be
altogether ignored" [FH91 p. 178]. For group-finders: "Suppose that the object touched is in the
left-hand corner of the far side of the table: then these codelets will preferentially (but not
deterministically) look in the corner of the table diagonally opposite the touched object to
determine if there are any groups of objects there." [FH91 p. 178] "Codelets are biased to look
at different parts of the table with different probabilities." [FH91 p. 178] The factors that
attract a look are conceptual similarity to the touched object and being diagonally or directly
opposite it [FH91 p. 178]. The price is accepted: "Any probabilistic search technique is a
risk-taking strategy and as such, sacrifices guaranteed success for greater efficiency." [FH91
p. 178]

**The initial Coderack.** "There are a number of find-group codelets initially on the Coderack,
giving the program an initial built-in bias to look for groups of objects." [F95 p. 65] "There
are also pick-salient-object and pick-salient-correspondence codelets initially on the
Coderack." [F95 p. 65] When Henry touches, "this will cause the Coderack to be loaded up with
its standard set of starting codelets" [F95 p. 97]. The screen's Coderack strip names the
codelet types in roughly their order of running, "In general, the types of codelets that run
early in the program are to the left of the bar graph; those that run later are to the right."
[F95 p. 97] Read at 600 dpi the strip's labels are, as far as they are legible: FND-GP, SAM,
FD-END, FD-MID, two more FD- labels of which the second is FD-NHBR, BD-OB0, BD-OB1, BD-OB2,
BD-OB-D, two SC- labels (the first SC-SMGP), a cluster of PK-SAL labels (PK-SALO among them),
CT-CP, PK-SALC, EXAM-CP, BLD-CP, CP-COMP, CHK-WV, CP-CLN and FND-ANS [F95 p. 98 (img)]. The
expansions are inferences from the codelet names in the text (find-group, sameness-detector,
find-end-object, find-middle-object, find-neighbor, build-object, pick-salient-object,
pick-salient-correspondence, examine-cp, find-answer); item 05 transcribes the strip in §6c.
**UNSPECIFIED:** the number and urgency of each starting codelet (→ D-055, D-052).

### 4.3 Descriptions

A codelet's first action is **description-building**: it notices an aspect of the table and
"attaches an appropriate descriptor to the object concerned" [F95 p. 75]; the examples are "a
group of objects, the neighbor of a particular object, the object on the end of a group, a
correspondence between two objects, the presence of a particular group of objects" [F95 p. 75].
Salience depends on it: "The salience of an object thus varies according to how well developed
its description is." [F95 p. 67] The descriptors the sources name:

| Descriptor | Who finds it | Source |
|---|---|---|
| the object's type | known for the touched object at the start [F95 p. 64]; the others are discovered as they are looked at (the scouts report "what objects are in interesting areas" [FCCA p. 386]; a find-end codelet "sends activation to that category in the Slipnet" [F95 p. 63]) | [F95 p. 63; F95 p. 64; FCCA p. 386] |
| neighbor | find-neighbor codelets; "If a neighbor is found, activation is sent to the 'neighbor' node in the Slipnet." [F95 p. 63] Only members of groups have neighbours: "in order for an object to have a neighbor, it must be in a group" [F95 p. 76] | [F95 p. 63; F95 p. 76; F95 p. 80] |
| end, middle | find-end codelets "look at the ends of groups" [F95 p. 63]; find-middle-object codelets find "objects in the middle of the group" [F95 p. 76] | [F95 p. 63; F95 p. 76] |
| left, right (ends of a group) | "It has also discovered that they are on the left- and right-hand ends of the group, respectively, which explains why 'left' and 'right' (and not 'above' and 'below') are active." [F95 p. 101] | [F95 p. 101] |
| above, below | the vertical counterparts of left and right, named with them | [F95 p. 101] |
| horizontal, vertical | the group's orientation: "the program has discovered that the objects in it seem to be arranged horizontally, thus activating the concept 'horizontal.'" [F95 p. 97] | [F95 p. 97; F95 p. 69] |
| number (one, two, three, many) | "one, if it is a simple object; otherwise, the size of the group if there is a group of objects" [F95 p. 68] | [F95 p. 68] |
| sameness | find-same-obj: "looks for an identical copy of the given object (or group of objects)" [F95 p. 63]; success sends activation to "sameness" | [F95 p. 63] |
| the group's kind | sameness group, shared-ISA group (silverware, crockery) or neither (§4.4.2) | [F95 p. 65; F95 p. 69] |
| region | the touched object's region and the others' (§1.3) | [F95 p. 65; F95 p. 66] |

**Two frames for left and right.** Inside a group, position descriptors are "the descriptions
of the positions within their respective groups of the objects making up the correspondences"
[F95 p. 68], so that an object "on the left end of one group" and one "on the right end of
another group" give "a left-right slippage" [F95 p. 68]. Across the table, the same words name
sides: a correspondence can be supported by a right-right identity slippage "where the terms
'left' and 'right' refer to the sides of the table as a whole" [F95 p. 85]. Both frames feed the
symmetry labels (§3.7, §3.10). **UNSPECIFIED:** how an object's table-side descriptor is
assigned, and whether every object gets one (→ D-029, D-030).

**Amorphous groups.** "Tabletop cannot handle completely amorphous groups." [F95 p. 63] "For
groups that have clear horizontal or vertical ends, it will find them." [F95 p. 63] For an
L-shaped group, "the program will choose only one as the upper end object" [F95 p. 63], where a
person would see two ends. **UNSPECIFIED:** how ends are chosen in a group that is not a line
(→ D-028).

### 4.4 Groups

#### 4.4.1 Gestalt groups, found first

"Because Tabletop builds its representations gradually, it initially represents a newly
discovered group of objects in a 'gestalt' manner." [F95 p. 65] "In other words, no
representation is built for the individual objects comprising the group; rather, when a
particular codelet discovers a group, it does two things: (1) sends activation to the concept
'group' in the Slipnet and (2) posts new codelets to the Coderack that will, if they run,
further examine the newly found group for more details about its components, internal
structure, and the like." [F95 p. 65] What such a group is, the chapter 4 run says: after the
first find-group codelet, "the program knows next to nothing about this group, other than that
it is a bunch of objects that are physically close together" [F95 p. 97]. "This is a gestalt
view of the group: It involves top-down visual chunking of several undifferentiated things
solely on the basis of physical proximity, before any individual details of the chunk are
analyzed." [F95 p. 97] It "amounts to recognizing the group qua group from afar and then
focusing on its details" [F95 p. 97]. On the screen the group is a wavy-bordered box around the
six objects at Henry's side, with nothing yet inside it [F95 p. 98 (img)].

FCCA calls these **proximity groups** and makes them Tabletop's first architectural difference
from Copycat: "The presence of objects in a region of the table, and then the lack of objects in
an adjacent region, is in itself evidence that the former objects 'belong together', in some
sense, whether or not they share any conceptual properties." [FCCA p. 390] So "sheer physical
proximity is often a sufficient reason to form a group" [FCCA p. 390], which Copycat never
allows. "Moreover, Tabletop often builds proximity groups out of objects it hasn't yet even
looked at individually." [FCCA p. 390] It builds the group and "only after doing so will it scan
it to see what sorts of 'fish' it has caught in its 'net'" [FCCA p. 390]. Hence nested groups
"from the outside in" [FCCA p. 390], where Copycat always builds "from the inside out" [FCCA
p. 390]; "In fact, Tabletop can even make groups through a mixture of outside-in and inside-out
processing." [FCCA p. 390]

A gestalt group is not yet a bottom-up group. Of a spoon placed next to a glass: "The program
will almost always spot this group qua collection of unidentified items but, because a spoon is
conceptually very far from a glass, it may not always group these objects in a bottom-up manner
(i.e., forming a much more solid structure in which each of the elements knows about its
neighbors)." [F95 p. 125] In Blockage 4, "the bottom-up glass-spoon group" forms only
"sometimes" [F95 p. 125].

**The follow-ups of find-group.** When a find-group codelet finds a group, besides activating
"group" and "updating a short-term record of the number of groups found (a kind of short-term
memory)" [F95 p. 76], it posts: "A find-group codelet, which will look for more groups" [F95
p. 76]; "A sameness-detector, which looks to see if that exact configuration of objects appears
elsewhere on the table" [F95 p. 76]; "A find-neighbor codelet, which takes as a parameter an
object in the group just found and determines its neighbors" [F95 p. 76]; "A find-end-object
codelet, which will find the end objects of the group" [F95 p. 76]; "A find-middle-object
codelet, which will find objects in the middle of the group" [F95 p. 76]; and "Pick-salient-
object codelets, which pick other salient objects on the table" [F95 p. 76]. Chapter 4 adds the
within-group search: "codelets that look for sameness groups (i.e., groups of neighboring
objects all having the same object type—for example, a group consisting of two adjacent forks)
within the group just found" [F95 p. 97], and codelets "that pick objects inside the group and
'look' to the right and left of those objects to determine their immediate neighbors" [F95
p. 97]. The point of posting only then: "if there are no groups on the table, it makes no sense
for there to be any perceptual agents hunting for the neighbors of objects, the end objects of
groups, and so on" [F95 p. 76]. The group-finding loop through "group" (the apple tree) is §2.6,
step 4. There is also a find-group-around(x) codelet, seen only in the urgency examples [F95
p. 77; F95 p. 93].

**UNSPECIFIED:** the proximity criterion that makes a "bunch of objects that are physically
close together" (→ D-022); how a find-group codelet chooses where to look and what it proposes
(→ D-023).

#### 4.4.2 Bottom-up groups: sameness, shared-ISA, and the rest

Within a gestalt group the program looks for smaller groups. French's Fig. 3.8 is a row of
fork, fork, knife, cup, spoon, spoon, the spoons drawn as the palette draws them, a circle on a
stem [F95 p. 65 (img)]. "Notice that there is theoretically a
huge range of groupings that could be made." [F95 p. 65] But "even though there is no reason in
principle that Tabletop could not make these groups, it very seldom does so" [F95 p. 65]:

- "It will almost always discover 'sameness groups' (i.e., groups made up of identical
  objects—the spoon-spoon group and the fork-fork group, in this case)." [F95 p. 65]
- "It will usually see groups whose objects share a common ISA category ('shared-ISA groups')."
  [F95 p. 65] "So, for example, the program will generally build silverware groups or crockery
  groups but will do so less often than it will build sameness groups." [F95 p. 65]
- "And it will only very rarely build groups whose objects have nothing in common." [F95 p. 65]
  "This is why one will not often see a cup-spoon grouping in the representation built by
  Tabletop of the above set of objects." [F95 p. 65]

"The process of 'understanding' the initial 'gestalt' group is, in some sense, the reverse of
distinguishing individual objects, a key goal of standard vision programs." [F95 p. 65] "The
goal in Tabletop is not to separate objects from each other but to group them into
conceptually organized, coordinated sets of items." [F95 p. 65]

**How they are built.** The codelet is build-object, with two objects as arguments: the
urgency examples list "build-object (fork1, fork2)" [F95 p. 77]. Groups grow by accretion: "the
process by which Tabletop builds large groups of identical objects (by progressively adding
adjacent objects to already-existing groups) is flawed" [F95 p. 140]; for six cups in a row "the
program would end up making many of the possible groupings of cups (e.g., cup1-cup2, cup2-cup3,
cup3-cup4, etc., and then cup1-cup2-cup3, cup3-cup4-cup5, etc., gradually working its way up to
the entire group of cup1-cup2-cup3- ... cup6)" [F95 p. 140]. FCCA describes the building as a
staircase of **micro-tests**: building a group "involves an escalating series of 'micro-tests'
that check the physical and conceptual distances between prospective group members, as well as
the compatibility of the prospective group with other already-established groups" [FCCA
p. 384]. "If any such test fails, the potential group is aborted" [FCCA p. 384]; "If all these
prerequisite hurdles are cleared, the group gets officially created by a specific codelet."
[FCCA p. 384] A structure "has to have risen up through the ranks by passing a series of graded
micro-tests" [FCCA p. 391]; it "is gradually 'promoted' upwards, and with luck eventually attains
the inner circle of the Worldview" [FCCA p. 391]. The Coderack strip's three labels BD-OB0,
BD-OB1 and BD-OB2 [F95 p. 98 (img)] are consistent with three stages of build-object, as in
Jumbo's graded tests [F95 p. 49]; that reading is an inference, not a statement of French's.

**UNSPECIFIED:** the stages, what each tests, and their pass probabilities as functions of the
members' conceptual distance and of temperature (→ D-024).

#### 4.4.3 Group strength and fights between groups

FCCA gives groups a strength of their own: "Perceptual groups, like correspondences, are
assigned strengths when they are made, and those strengths likewise undergo constant
re-evaluation in light of recent perceptions." [FCCA p. 382] The factors, "some of which are
fairly obvious, others of which are rather subtle" [FCCA p. 382]: "its physical dimensions"
[FCCA p. 382]; "number of constituent items" [FCCA p. 382]; "uniformity of composition" [FCCA
p. 383]; "distance between constituent items" [FCCA p. 383]; "the prior existence of similar
groups" [FCCA p. 383]. French's book has no list under that name; its nearest is the three
group-only salience factors, repetition, common superordinate category and size [F95 p. 67]
(§4.5), and the loss of salience by groups in losing correspondences [F95 p. 73].

Groups fight: "a proposed perceptual group will often be incompatible with an already-existing
group (i.e., it would lay claim to items already 'taken' by the pre-existing group), and thus
the two will be in implicit competition" [FCCA p. 383]. "As with correspondences, a fight will
sooner or later take place, in which the probability of victory of either competitor is
determined by its current strength relative to its rival's." [FCCA p. 383] In the Worldview,
"groups do not overlap" [FCCA p. 390]. French's book describes competition only for
correspondences (§4.7), adding that the loss of rating "applies not only to correspondences but,
in particular, to various conceptual groupings of objects" [F95 p. 73].

**UNSPECIFIED:** the weights of the five factors, the scale, when groups fight and what
"incompatible" admits (a subgroup inside a group is not a rival: Fig. 3.9 shows both) (→ D-025).

#### 4.4.4 Subgroups, and the fault of keeping them all

Groups nest: FCCA speaks of "groups of objects (or of groups of groups)" [FCCA p. 381], and
the figures draw sameness subgroups as dashed boxes inside wavy-bordered groups [F95 p. 71
(img)]. In the chapter 4 run, "back in the big group on Henry's side, the program has also
discovered the fork-fork subgroup and the spoon-spoon subgroup" [F95 p. 101]. On the screen,
"a subgroup is visible only when the program first discovers it, and thereafter only if it
forms the end point of a correspondence in the Worldview" [F95 p. 101]. Finding one triggers a
search for its twins: when a spoon-spoon subgroup was built, "a large number of
search-for-same-group codelets were posted to the Coderack (shown by the tall bar in the
middle, with '70' above it)" [F95 p. 101], and "The program is therefore very likely to go and
search for other occurrences of spoon-spoon groups." [F95 p. 101] (FCCA: "Once any group is
built, the program devotes resources to looking for identical or similar groups in other areas
of the table." [FCCA p. 386].) In that run the subgroups found lay at the ends of the group:
"The program has noticed that these subgroups are on the ends of the group, which is why the
concept 'end' is highly active in the Slipnet." [F95 p. 101]

**The fault.** French's program keeps every intermediate grouping of a run of identical
objects: "It keeps all of the intermediate subgroups around, considering each of them important
in its own right and trying to establish mappings with other 'similar' groups." [F95 p. 140]
People do not: "when we humans view a group of six adjacent cups, we tend to see them simply as
a group of cups, and only under the most exceptional of circumstances as, say, a group of two
cups next to a group of four cups" [F95 p. 140]. "Unlike humans, however, Tabletop retains all
of the subgroups, which means that it gets hopelessly confused whenever it is given a problem
containing groups made up of more than three identical objects. This is a problem with Tabletop
that needs to be fixed." [F95 p. 140] This is why the Figure/Ground problem has only two forks
and two glasses as background [F95 p. 140]. Fixed by default (§6b, D-026; ANSWERS.md 2), with
the faulty reading kept for the `french-faults` profile.

#### 4.4.5 Ends, middles, neighbours and orientation

Find-end codelets "look at the ends of groups. Whatever category of object such a codelet
finds on the end of a group, it sends activation to that category in the Slipnet and also to
the concept 'end.'" [F95 p. 63] Find-end-object and find-middle-object are posted together, "a
find-end-object codelet and a find-middle-object codelet, each of roughly the same urgency"
[F95 p. 79], because "Why should find-end-object necessarily run before find-middle-object? It
should not" [F95 p. 79].

Neighbours are found in a chain along the group: "If there is a group consisting of fork1,
fork2, knife1, cup1, spoon1, and spoon2, the first find-neighbor codelet might have as its
parameter fork1. It would find fork2 and spawn the codelet find-neighbor(fork2)." [F95 p. 80]
That codelet "would find knife1) and post the codelet find-neighbor(knife1), and so on" [F95
p. 80]; "it cannot be known how many 'generations' of find-neighbor codelets there will be"
[F95 p. 80]. Neighbours are looked for "to the right and left" of an object [F95 p. 97], and
in Fig. 4.4 "it has just discovered that the two forks in that group are neighbors" [F95
p. 101].

Orientation: a group is horizontal when "the objects in it seem to be arranged horizontally"
[F95 p. 97]; a group may be "linear" [F95 p. 69], with horizontal or vertical orientation, or
amorphous (§4.3). **UNSPECIFIED:** the geometry of "neighbor", "end", "middle" and "linear"
(→ D-027, D-028).

### 4.5 Salience

**What it is.** "On the table, certain objects (and correspondences between objects) are more
salient to people than others." [F95 p. 66] "In Tabletop, the salience of an object (or
correspondence) is a variable that determines how much attention the program will tend to pay
to that object (or correspondence)." [F95 p. 66] "Salience plays a key role in focusing the
program's attention during a run and thus in ensuring that the architecture will be able to
scale up to real-world domains." [F95 p. 66] "Pick-salient-object codelets are continually
picking objects in proportion to their salience (for human beings, this corresponds to
consciously noticing the objects) for further processing." [F95 p. 66] The same codelets exist
for correspondences, "pick-salient-correspondence codelets" [F95 p. 65], with "a probabilistic
bias toward picking objects and correspondences of high salience over ones having lower
salience" [F95 p. 65]. FCCA: "certain objects and areas of the table are perceptually 'hot'
while others are 'cool', and these biases are dynamic: they change as new perceptions are made"
[FCCA p. 385].

**French's factors** [F95 pp. 66-67], for objects and groups:

1. **Activation of the concept corresponding to the type of the object.** "For example, if a
   certain number of spoons have been noticed by the program, the concept 'spoon' will be
   active. This will contribute to the salience of all spoons and make them more likely to be
   noticed than objects whose types are less active." [F95 p. 66]
2. **Proximity to the touched object.** "Objects close to the touched object (usually the most
   salient object on the table) are noticed preferentially because of this proximity." [F95
   p. 66]
3. **Extremal position in a group of objects.** "Objects at the ends of a group are, in general,
   more likely to be noticed than objects in the middle of a group (unless, of course, the
   concept 'middle' is much more active in the Slipnet than the concept 'end,' in which case the
   default preference for objects on the ends of groups can be overridden)." [F95 p. 66]
4. **Preferred regions with respect to the touched object.** "Studies I ran on human subjects
   showed that certain regions are favored a priori as places to look for an object
   corresponding to the touched object." [F95 p. 66] "If the touched object is in a corner, for
   example, the most favored region is diagonally opposite the touched object, and the next
   most preferred region is directly opposite the touched object." [F95 p. 66] The order
   continues "and so on" [F95 p. 93] without being given.
5. **Groups versus simple objects.** "Groups of objects compete for attention with single
   objects. Groups are given salience points simply for being groups, which constitutes a
   pressure in favor of looking at them over simple objects." [F95 p. 66]
6. **Whether or not the object corresponds to another object.** "An object's salience increases
   if it has been mapped to another object on the table (specifically, if the object is part of
   a correspondence in the Worldview" [F95 p. 66], the Worldview being "the program's current
   perception of the central structures on the table" [F95 p. 66].
7. **How often the object has been part of a correspondence that has won (or lost) a
   competition.** "Each time a correspondence competes to become part of the Worldview and wins,
   bonus points are awarded to the two objects making up the correspondence. Objects in losing
   correspondences lose points." [F95 p. 66]

And for groups, under **composition of the groups of objects** [F95 p. 67]:

8. **Repetition of objects.** "If all the objects in a group are of the same type (i.e., it is a
   sameness group), this increases the group's salience." [F95 p. 67]
9. **Common superordinate category.** "If all the objects in a group have the same superordinate
   object category (e.g., they are all 'silverware' or 'crockery'), this increases the salience
   of the group." [F95 p. 67]
10. **Size.** "Larger groups are favored over smaller groups." [F95 p. 67]

"Each of these factors determines a real number, and these numbers are summed to determine the
salience of the object." [F95 p. 67] Salience is dynamic: "at the beginning of a run, there are
no objects, in correspondence with other objects, and no relations between objects have been
found yet. In this case, then, this factor contributes zero to all calculations of salience"
[F95 p. 67]; "as soon as it is noticed that a particular object corresponds to another in the
Worldview (i.e., the program's current perception of the table), this will increase the
salience of both objects" [F95 p. 67]. "In other words, salience is a dynamic quantity that
changes as the program develops its picture of the Tableworld." [F95 p. 67] Losses lower it:
"the salience ratings of objects that are part of losing correspondences are decreased, of
winning correspondences, increased" [F95 p. 73].

**FCCA's list** [FCCA p. 385], "some of the main ones being" [FCCA p. 385]:

| FCCA's factor | French's factor |
|---|---|
| "the item's location relative to the touched object" [FCCA p. 385] | 2 and 4 |
| "the item's conceptual proximity to the touched object" [FCCA p. 385] | not in French's list (it enters correspondence strength, §4.6, and the spread of activation to the touched type's neighbours makes it an effect of factor 1) |
| "the current level of activation of the object's category" [FCCA p. 385] | 1 |
| "the item's being a group (as opposed to a primitive object)" [FCCA p. 385] | 5 |
| "if the item is a group, its size and its type" [FCCA p. 385] | 8, 9, 10 |
| "if the item is inside a group, its physical position inside the group" [FCCA p. 385] | 3 |
| "prior perception of other items having similar attributes" [FCCA p. 385] | not in French's list (compare the "prior existence of similar groups" of group strength, §4.4.3) |
| "the item's already having a counterpart" [FCCA p. 385] | 6 (and 7) |

FCCA also speaks of a table **area's** attractiveness as one of the biases, "the attractiveness
level of a table area" [FCCA p. 387], each bias "computed by an algebraic formula that takes
into account pressures of all sorts, whether they are bottom-up or top-down" [FCCA p. 387],
and refers for "The exact formulas" [FCCA p. 387] to French 1995, which gives the factors but no
formula and no weight.

**Temperature.** Salience choices soften with temperature. For the region factor: "When the
temperature is very low, the program deterministically respects the priorities of the regions
that human subjects were shown to choose preferentially with respect to the touched object."
[F95 p. 93] "In other words, an area diagonally opposite the touched object is most preferred,
an area directly opposite the touched object is next, and so on." [F95 p. 93] "If the
temperature should ever happen to reach 100, the program is as likely to choose any region as
any other." [F95 p. 93] "The selection of objects or correspondences based on their salience
works in the same way." [F95 p. 93] "The selection process approaches determinism as the
temperature approaches 0, is completely unbiased (i.e., uniformly distributed random) at 100,
and has varying degrees of bias at intermediate temperatures." [F95 p. 93] (§6 has the
exponent a(T) used for codelets.)

**The early focus.** "The first focus of attention of the program tends to be objects whose
corresponding concept node in the Slipnet is very active." [F95 p. 67] If Henry touches a cup,
the program "will, at least initially, be more likely to focus attention on cups than on other
objects whose corresponding concepts are less active" [F95 p. 67]; "Activation will subsequently
spread to other objects that are conceptually close to cups (e.g., glasses), and the program
will focus on these objects." [F95 p. 67] So "the program will start to test for possible
correspondences involving cups and glasses, examine the table for possible neighbors of these
salient objects, and so on" [F95 p. 67]: French's "progressive alignment", the relational shift
of Kotovsky and Gentner [F95 p. 67]. In the chapter 4 run the isolated cup was salient for two
reasons: "it shares object type ('cup') with the touched object, and it is in a very good
position on the table with respect to the touched object (i.e., roughly diagonally opposite
from it)" [F95 p. 97].

**UNSPECIFIED:** every weight, the scale of each factor, the region order past the first two,
and the form of the proximity term (→ D-031 to D-034).

### 4.6 Correspondences

#### 4.6.1 What a correspondence is

"A correspondence is defined by its end objects." [F95 p. 67] "The two 'objects' in
correspondence may be either simple objects—e.g., a fork—or chunked groups of objects—e.g., a
group consisting of a fork, a knife, and a spoon." [F95 p. 68] An object may map to a group:
"A fork, for example, could very well be mapped onto a chunked group of two forks." [F95 p. 71]
FCCA: "a correspondence built between two items symbolizes the program's commitment, at least
tentatively, to a view of those items as each other's counterparts, and along with the
correspondence itself are stored the specific reasons underpinning that vision of
'counterparthood'" [FCCA p. 382]. Those reasons are **slippages**: "if 'leftmost' is one of the
descriptors of a particular object and 'rightmost' is one of the descriptors of the object to
which it corresponds, then we say that the right-left slippage" supports the correspondence
[F95 p. 68]. "When a concept slips to itself (e.g., right-right), this is a degenerate case of
the notion of slippage, called an identity." [F95 p. 68] FCCA: "In the case of conceptual
proximity but not identity, the forgiven mismatch is called a conceptual slippage." [FCCA
p. 382] "Conceptual slippages come in various types, since conceptual proximity comes about for
many different reasons" [FCCA p. 382]: looking similar, similar functions, being used together.
A slippage weakens an analogy and makes it interesting at once, so "it is not by counting
slippages, or by summing the conceptual distances in all the slippages, or by any technique
nearly that simple that one can measure the quality or depth or interest of an analogy" [FCCA
p. 383]; "two 'parallel' slippages - slippages of the same type - can be better than just one,
because they reinforce each other" [FCCA p. 383].

"Its rating is a function of three types of factors: static factors, dynamic factors, and a
'bonus' factor." [F95 p. 67] "The static evaluation of a correspondence consists of those
factors that do not depend on the relation of that correspondence to the other correspondences
in the" Worldview [F95 p. 67]; "The dynamic evaluation is a rating of how well the
correspondence fits into the current Worldview." [F95 p. 68] "The bonus is awarded each time
the correspondence wins a competition to get into (or to remain in) the Worldview.
Correspondences that lose competitions lose points." [F95 p. 68] The bonus models
"representational inertia (Kaplan and Kaplan 1982), whereby previously established
representations are favored a priori over new representations of data" [F95 p. 68]: "The
longer we humans maintain and use a particular representation, the harder it becomes to
dislodge it (James 1892)." [F95 p. 68]

French uses three words for the one number: the correspondence's **rating** [F95 p. 67], its
**strength** ("This determines the strength of a particular correspondence." [F95 p. 69]), and
its **salience** ("The salience of a correspondence in the Worldview is increased each time it
wins a competition" [F95 p. 69]; "its salience rating drops" [F95 p. 73]). FCCA says strength.

#### 4.6.2 The static factors [F95 pp. 68-69]

"All of the static factors taken into consideration in evaluating correspondences are as
follows:" [F95 p. 68]

1. **Salience of the two objects that make up the correspondence.** "The higher the salience of
   the objects (see previous section), the more points are awarded for the correspondence."
   [F95 p. 68]
2. **Simple-object/group-of-objects slippages.** "The program favors either group-group or
   simple-object-to-simple-object slippages. If the 'group' node is active, the former are
   favored over the latter; if the 'isolated' node is active, the latter are favored; both of
   these slippages are preferred over group-to-simple-object slippages." [F95 p. 68] (On
   "isolated", §3.1.)
3. **End/middle location slippages.** "The program favors mapping an object on the end of one
   group to an object on the end of another group or, alternately, an object in the middle of
   one group to an object in the middle of another group." [F95 p. 68]
4. **Position slippages.** "These are slippages of the descriptions of the positions within
   their respective groups of the objects making up the correspondences." [F95 p. 68] A left
   end mapped to a right end "will produce a left-right slippage" [F95 p. 68]. "The value of
   this slippage depends on how active the 'diagonal symmetry' and 'opposite' nodes are, these
   concepts being the ones that link the two concepts 'left' and 'right' in the Slipnet." [F95
   p. 68] And the loop closes: "(The activation of the concepts 'opposite' and 'diagonal
   symmetry' also depends on how many other correspondences in the Worldview have 'opposite'
   and 'diagonal-symmetry' slippages.)" [F95 p. 68]
5. **Same or similar object type.** "Points are awarded based on how conceptually close the
   types of the two corresponding objects are in the Slipnet." [F95 p. 68] (The distance is
   §3.5; this is the weight French raised for the human comparison, ANSWERS.md 7.)
6. **Number-of-objects slippage.** "Points are awarded based on the slippage of the number of
   objects making up the first object of the correspondence (one, if it is a simple object;
   otherwise, the size of the group if there is a group of objects) to the number of objects
   making up the second object in the correspondence." [F95 p. 68] "For example, the conceptual
   distance between 'two' and 'two' in the Slipnet is less than the distance between 'one' and
   'two.' For this reason, the first slippage ('two' to 'two') would be awarded more points
   than the second ('one' to 'two')." [F95 p. 68]
7. **Same or similar ISA description** (shared-ISA groups only). "If both objects connected by
   a correspondence are shared-ISA groups, the program compares them. Points are awarded on the
   basis of how conceptually close the ISA description of one group (e.g., 'silverware') is to
   that of the other." [F95 p. 69]
8. **Same-objects slippage** (groups only). "If the object types in both groups making up the
   correspondence are the same (e.g., both groups consist of a fork, a knife, and a cup), points
   are awarded to the correspondence." [F95 p. 69]
9. **Orientation slippage** (groups only). "If both groups are linear, there can be
   horizontal-horizontal, vertical-vertical, or horizontal-vertical slippages. This works in the
   same way as the position slippages." [F95 p. 69]

FCCA's shorter account agrees: a correspondence "is likely to be built if their categories are
identical, or are conceptually close (as measured in the Slipnet)" [FCCA p. 382]; "if the items
are situated in mirror-image or diagonally opposite locations on the table, or nearly so" [FCCA
p. 382] that helps; and for groups "either having similar items for members or having similar
abstract structures counts in favor of building the correspondence" [FCCA p. 382].

#### 4.6.3 The dynamic factors and the bonus [F95 p. 69]

"The dynamic part of the evaluation of a correspondence can be done only when there are other
correspondences in the Worldview." [F95 p. 69]

1. **Whether or not the correspondence contains the touched object.** "If it does, it is given a
   special status, a 'Hobj correspondence' (for Henry's object), and receives points." [F95
   p. 69]
2. **Whether or not the correspondence is a neighbor of the current Hobj correspondence.** "Two
   correspondences are considered neighbors if the end objects (i.e., the objects that the
   correspondence maps onto one another) of one correspondence are the neighbors of the end
   objects of the second correspondence." [F95 p. 69]
3. **Whether or not the correspondence is the neighbor of one of the correspondences in the
   Worldview.** "Being a neighbor of a non-Hobj correspondence in the Worldview is worth fewer
   points than being the neighbor of the Hobj correspondence, but if a correspondence is close
   to a correspondence already in the Worldview, this nonetheless increases its salience
   somewhat." [F95 p. 69]

**The bonus.** "Bonus points in the evaluation of a correspondence are awarded on the basis of
whether a correspondence wins or loses a competition." [F95 p. 69] "Any correspondence that
loses a competition has its rating decreased." [F95 p. 69] It is "the strong get stronger, the
weak get weaker" [F95 p. 69]. "In much the same way as the salience of objects is determined,
the points from the static and dynamic evaluations and from the winning or losing competitions
are added together. This determines the strength of a particular correspondence." [F95 p. 69]

**FCCA's re-evaluation.** "A correspondence's strength is not fixed but is constantly being
re-evaluated in light of the latest perceptual events." [FCCA p. 382] "For example, the
building of a new correspondence that resembles an old one, especially if the two are near each
other on the table, enhances the strength of the old one." [FCCA p. 382] "On the other hand,
the building of a very different kind of correspondence right next to a prior one tends to
weaken the older one." [FCCA p. 382] The first half is French's dynamic factors 2 and 3
restated; the weakening by a different neighbour appears only in FCCA. "Thus a once-shaky
correspondence can gradually become solidly entrenched, and conversely, a once-powerful
correspondence can grow feeble and endangered." [FCCA p. 382]

**UNSPECIFIED:** the points of every static and dynamic factor, the bonus and the penalty, the
scale of the sum, and whether FCCA's weakening exists in the book's program (→ D-036 to
D-038).

#### 4.6.4 Creation, testing, competition

"The incorporation of correspondences into the Worldview is a three-stage process:
correspondence creation, initial testing, and competition." [F95 p. 71]

1. **Creation.** "A correspondence will not even be created unless certain not-too-stringent
   conditions are met—for example," [F95 p. 71] "the objects in question must not be in the same
   group" [F95 p. 72]. Which codelet creates one is not said; the strip's CT-CP label [F95 p. 98
   (img)] fits. A new correspondence is drawn dashed and "is highlighted by flashing boxes at
   both ends" [F95 p. 101].
2. **Testing.** "Once created, the correspondence will be evaluated on the basis of a set of
   more rigorous conditions. Again, the outcome of this evaluation is a probabilistic one: there
   is no rigid evaluation threshold above which the correspondence passes and below which it
   fails." [F95 p. 72] Chapter 4 shows the program "testing its quality" [F95 p. 101] after a
   correspondence between two spoon-spoon groups is made.
3. **Competition.** "If the correspondence passes this test, it can then proceed to compete with
   any conflicting (in the sense shown above) correspondences in the Worldview so far
   developed." [F95 p. 72] With no conflict there is no fight: "Since this correspondence is not
   a rival to any correspondence already in the Worldview, it is put in the Worldview without a
   fight." [F95 p. 101] On entry the line turns solid: "the tentative (dashed) cup-cup
   correspondence has now been solidified and is thus now officially part of the Worldview"
   [F95 p. 101].

The codelets that act on correspondences, by name in the text: pick-salient-correspondence
[F95 p. 65] and examine-cp, posted when a group-level correspondence is built, whose role is to
take the group-level correspondence and see "whether there are any correspondences to be built
between the objects making up the connected groups" [F95 p. 85] (the text layer drops a word
before "whether"; read on the image). The strip adds CT-CP, EXAM-CP, BLD-CP, CP-COMP, CHK-WV and
CP-CLN [F95 p. 98 (img)].

**Correspondences inside correspondences.** When two groups are mapped, the objects inside
may be mapped too: in the run of Fig. 4.10 the program "built correspondences from both of the
forks of the upper group to corresponding forks in the lower fork-fork group, as well as a
correspondence between the two subgroups themselves" [F95 p. 108]; in Fig. 4.12 "the program
did not take the trouble to look for correspondences inside the subgroup-to-subgroup
correspondences" [F95 p. 108]. These are the levels of §3.10 and §4.7.5.

**Group-to-object correspondences, and two biases French calls too strong.** The program
"strongly resists mapping groups onto individual objects, although under some circumstances it
might be happy mapping a spoon-spoon group to an individual spoon" [F95 p. 111]; at high
temperature it once put "a correspondence from a cup-spoon group to a single spoon" [F95
p. 111] into the Worldview. "It should be noted that Tabletop favors group-to-group mappings
and abhors group-to-isolated-object mappings." [F95 p. 125] "This strong bias against
group-to-isolated-object mappings should probably be relaxed." [F95 p. 125] And at the group
level: "when there are groups of objects on the table, the program is strongly biased to hunt
for group-level correspondences in preference to object-level correspondences. Thus, this bias
is clearly a bit too strong." [F95 p. 109] Both are fixed by default (§6b, D-045, D-046;
ANSWERS.md 2). **UNSPECIFIED:** the creation conditions beyond "not in the same group", the
test's form, and the size of each bias (→ D-035, D-039, D-045, D-046).

### 4.7 The Worldview

#### 4.7.1 Definition

"All the resources of the program are devoted to building up the Worldview. The Worldview is a
set of noncontradictory mappings of objects and groups of objects onto one another." [F95
p. 70] "In this context, 'noncompeting' (sometimes called 'noncontradictory') means that no
object can simultaneously correspond to two or more different objects." [F95 p. 70] Mapping an
object onto a group is allowed [F95 pp. 70-71]. At the group level: "if the program maps a group
of objects, A, onto another group of objects, B, as wholes, then the individual objects inside A
can map only to objects inside B and not to some other object elsewhere on the table" [F95
p. 71]. In short, "no object or group of objects is the end object for two distinct
correspondences" [F95 p. 71]. FCCA's version: "The Worldview culls from the contents of the
Workspace all those perceptual structures that are currently considered the best, operating
under the constraint, of course, that the selected items all be mutually compatible (i.e.,
groups do not overlap, objects have at most one counterpart, meaning that no object is the
endpoint of two or more correspondences, and so on)." [FCCA p. 390] "Thus at worst, the
Worldview is a non-self-contradictory subset of the Workspace," [FCCA p. 390] "and at best it
is a highly coherent subset of the Workspace; on the other hand, the rest of the Workspace need
not be so organized." [FCCA p. 391] The Worldview is "the sole, current interpretation of the
table" [F95 p. 73], against "the larger Workspace, in which there can be many structures that
have been built but are below 'conscious threshold'" [F95 p. 73].

**The figures** [F95 pp. 71-75 (img)]. All five use one configuration, that of Fig. 2.11 and
of the chapter 4 run (§1.6): at Henry's side a group of fork, fork, knife, the touched cup,
spoon, spoon; at Eliza's side a group of spoon, spoon, small glass, fork, fork, knife; and an
isolated cup at the lower right [F95 p. 71 (img)].
- Fig. 3.9, captioned "Noncompeting correspondences" [F95 p. 71]: solid lines from Henry's fork
  subgroup to Eliza's fork subgroup, from Henry's spoon subgroup to Eliza's spoon subgroup, and
  from the touched cup to the isolated cup; no end is shared [F95 p. 71 (img)].
- Fig. 3.10, "Directly competing correspondences: the cup-glass and the cup-cup correspondences.
  They compete, and one of them will emerge a winner." [F95 p. 71] The drawing: a solid
  spoon-subgroup to spoon-subgroup line, and two dashed lines from the touched cup, one to
  Eliza's small glass and one to the isolated cup [F95 p. 71 (img)].
- Fig. 3.11, "Competition between a correspondence in the Worldview (solid line) and a proposed
  rival correspondence (dashed line)" [F95 p. 72]: solid lines fork subgroup to fork subgroup and
  touched cup to isolated cup, a dashed rival from the touched cup to the small glass [F95 p. 72
  (img)].
- Fig. 3.12, captioned "An incoherent Worldview" [F95 p. 74]: solid lines fork subgroup to fork
  subgroup, spoon subgroup to spoon subgroup, and the touched cup to the isolated cup, which
  carries Eliza's arrow [F95 p. 74 (img)].
- Fig. 3.13, captioned "A coherent Worldview" [F95 p. 75]: solid lines fork subgroup to fork
  subgroup and the touched cup to the small glass, which carries Eliza's arrow; the isolated cup
  is unmapped [F95 p. 75 (img)].

#### 4.7.2 Rivals and incoherence

Two correspondences that share an end are **rivals**; two whose ends lie in different groups
are **incoherent**. "Rival correspondences start or finish at the same end point, and for this
reason" [F95 p. 101] "they are forbidden to belong to the Worldview at the same time. In other
words, rival correspondences are like Necker-cube interpretations: they are mutually exclusive
and must therefore fight each other for primacy." [F95 p. 105] "On the other hand, incoherent
correspondences are like local aspects of an impossible figure (such as the 'impossible
triangle'): globally, they add up to something bad, but they can nonetheless coexist." [F95
p. 105] In the run, the spoon-group correspondence "makes the Worldview incoherent—that is, one
correspondence goes from an object inside one group to an object inside another group, and the
second correspondence (between the two cups) starts inside one group and ends on an isolated
object" [F95 p. 101]. "Although incoherence of this sort is permitted, it is 'discouraged'—that
is, the program greatly prefers all Worldview correspondences to start and finish 'together.'"
[F95 p. 101] "There is strong pressure for all Worldview correspondences to originate in a
single group and to end in a single group." [F95 p. 101]

The exclusivity of rivals is the Necker cube's: "Psychological plausibility does not allow a
single object to belong simultaneously to two rival subgroups at once." [F95 p. 69] "There may
be competing descriptions of reality, but at any given time, only one of these descriptions
belongs to our perception of the situation." [F95 p. 69] Of the ambiguous headlines, "The mutual
exclusivity is total." [F95 p. 70]

#### 4.7.3 Competition: the subjunctive Rival Worldview

"The proposed new correspondence must compete with any conflicting correspondences already in
the Worldview." [F95 p. 72] "All the conflicting correspondences are temporarily removed
(subjunctively) from the Old Worldview, and the new correspondence is temporarily inserted
(subjunctively) in their place to form the Rival Worldview." [F95 p. 72] "The Rival Worldview is
evaluated according to the criteria given in the section on determining the salience of
correspondences between objects." [F95 p. 72] "The probabilistic winner of this competition
between the Rival Worldview and the Old Worldview is then installed as the new Worldview." [F95
p. 72] So the fight is between two whole Worldviews, rated as in §4.7.5, not between the two
correspondences alone: in the run, "the cup/small-glass correspondence has won the fight,
because the structure rating of a coherent Worldview containing a cup-glass correspondence is
higher than that of an incoherent Worldview containing a cup-cup correspondence, even though a
cup-cup correspondence is a priori stronger than a cup-glass correspondence" [F95 p. 105], a
"probabilistically determined victory" [F95 p. 105]. FCCA gives the shape of the probability:
"the greater the discrepancy between their strengths, the more likely the stronger one is to
win the fight" [FCCA p. 382]. During the fight the incumbent is drawn dashed, "actually a
flashing dashed line on the computer screen" [F95 p. 105], "merely to indicate that it is
competing with the newcomer" [F95 p. 105].

**After the fight.** "Once the winning Worldview has been chosen, the rating of each of the
correspondences in that Worldview is improved and the rating of the losing correspondence is
lowered." [F95 p. 72] "In other words, winning a competition improves the chances of the winner
the next time it competes. Similarly, it becomes a little harder for the loser to compete the
next time around." [F95 p. 72] "The loser, although not in the Worldview, remains nonetheless
in the Workspace with the potential to compete again." [F95 p. 72] The objects at the ends gain
or lose salience too (§4.5, factor 7).

**UNSPECIFIED:** the probability of the rival's victory as a function of the two ratings and of
temperature (→ D-040); which correspondences conflict when groups are involved (→ D-044).

#### 4.7.4 Losers stay, fade and go

"Competition in Tabletop works in the same way as in Copycat with one exception." [F95 p. 70]
"In Copycat, competition is a 'gladiatorial' process: structures that lose competitions are
'killed'—that is, are eliminated entirely from the Workspace." [F95 p. 70] "Competition among
structures in Tabletop is a more 'sporting' process: although losers are penalized for losing,
they stay in the game." [F95 p. 70] The reason is the young woman/old woman figure: once both
readings have been seen, "we can flip back and forth between the two interpretations with no
trouble" [F95 p. 70], which "argues for a perceptual architecture that does keep rejected
representations around for some time, even if they are not highly active" [F95 p. 70]. "In
Tabletop, structures (like old soldiers) do not die, they just fade away." [F95 p. 72]

The fading: "each time a structure is involved in a competition and loses, its salience rating
drops until finally, if this happens enough times, it gets removed from the Workspace" [F95
p. 73]. "Eventually, if the value of a correspondence in Tabletop drops below a certain
threshold (for example, because it has lost a number of competitions), it is (probabilistically)
removed from the Workspace and then must be built again." [F95 p. 73] Codelets holding a removed
structure fizzle: "once a correspondence is removed from the Workspace, any codelets that might
have this correspondence as one of its parameters would, if selected to run, simply 'fizzle,'
i.e., have no action at all" [F95 p. 73]. Removal by age is a suggestion, not a mechanism: "It
would be reasonable to maintain a record of the age of a particular structure, even though this
is not presently implemented in Copycat." [F95 p. 73] French does not say whether Tabletop has
it. In drawing terms, "the losing solid-line structure would simply be demoted to dashed-line
status, rather than requiring the program to rebuild it from scratch again" [F95 p. 73]. "There
is thus a pleasing time symmetry in Tabletop: structures come into existence gradually and go
out of existence gradually." [F95 p. 73] Two reasons are given: plausibility, since a rejected
structure "is kept intact in the background for a while, even if they are not directly
conscious of it" [F95 p. 73]; and scaling up, since "we do not want each competition to
completely destroy all the work previously done in creating the losing structure" [F95 p. 74],
"We want one structure to gradually pull ahead while the other gradually falls behind." [F95
p. 74]

FCCA's telling, the second architectural difference: "In Tabletop, by contrast, a loser is
merely demoted, in the sense of being removed from the special 'elite' area inside the Workspace
known as the Worldview." [FCCA p. 390] "If an upheaval takes place in the Worldview, uprooting
some established viewpoint (i.e., set of compatible perceptual structures), then new perceptual
ideas are needed, and luckily, many such structures are already available elsewhere in the
Workspace." [FCCA p. 391] Tabletop keeps a "shadow cabinet" of fragments, "so that if the
dominant party ever loses power, its rivals can quickly step out of the shadows and try to grab
power" [FCCA p. 391]. A structure that loses should have "its gradual rise to the top" gently
reversed "rather than out-and-out court-martialing it and sending it mercilessly to Siberia"
[FCCA p. 391].

**UNSPECIFIED:** the threshold, the probability of removal, which codelet removes (the strip's
CP-CLN [F95 p. 98 (img)] is the candidate), and whether anything but correspondences is removed
(→ D-041).

#### 4.7.5 The rating of a Worldview, and FCCA's structure-value

"It is important to know how a Worldview is rated. The evaluation of the Worldview is the sum
of the following:" [F95 p. 74]

1. **Strength.** "The total of the strength ratings of all of the correspondences contained in
   it." [F95 p. 74]
2. **The degree of coherence of the Worldview.** "This is closely related to the notion of
   systematicity developed by Gentner (1983) and others." [F95 p. 74] "In the Tabletop domain,
   this translates into a preference for a 'coherent' Worldview in which, basically,
   correspondences tend to bunch together logically." [F95 p. 74] "For example, if there were
   three groups of objects on the table, the program would penalize a Worldview consisting of
   four strong correspondences from objects in group 1 to objects in group 2 and one
   correspondence from an object in group 1 to another object in group 3." [F95 p. 74]
   Coherence "would be much higher in the above example if all of the correspondences went from
   objects in group I to objects in group 2" [F95 p. 74]. "Consistency is a measure of how close
   a particular Worldview is to such an ideal arrangement of correspondences." [F95 p. 74] Figs.
   3.12 and 3.13 (§4.7.1) are the incoherent and the coherent case.
3. **Degree of parallelism.** "One correspondence is said to be parallel to another if the
   slippages that support the first have the same labels as the slippages that support the
   second." [F95 p. 74] If one correspondence "is supported by a left-to-right slippage, which
   has an 'opposite' label, then a parallel" [F95 p. 74] "correspondence in the same Worldview
   would have a slippage with the same label (e.g., a right-to-left or an above-to-below
   slippage)" [F95 p. 75]. "The notion of parallel slippages is more complicated in Tabletop
   than in Copycat for two reasons:" [F95 p. 75] (a) subworlds: "There can be two (or more)
   physically separate groups of correspondences on the table" [F95 p. 75], "with no interaction
   between the two. It is as if there are two separate subworlds on the table." [F95 p. 75]
   "If there is a left-to-right slippage in the first set of correspondences, should this
   affect the second set of correspondences?" [F95 p. 75] (b) levels: "There can be two or more
   levels of correspondences within correspondences." [F95 p. 75] "Do we require that the kinds
   of slippages at one level (for example, the level of the group) significantly influence
   slippages at another (e.g., the level of individual objects)?" [F95 p. 75] Both are the
   problem of a single node with multiple activations, §3.10; "it is sufficient to mention that
   these issues do enter into the calculation of degree of parallelism of slippages in the
   Worldview" [F95 p. 75].

How heavy the coherence term is, the sources say only in words. "In this case, the overall
Worldview rating has improved so much (the program is quite heavily biased against incoherent
Worldviews) that this fact by far dominates the pressure to increase the temperature." [F95
p. 105] Yet incoherence does not always lose: in Blockage 3, two glasses of one group mapped to
the touched cup and to a far glass "will provide two strong correspondences, offset only by the
incoherence of the Worldview they create" [F95 p. 124], and in Blockage 4 "The fact that these
mappings are incoherent is usually not enough to force Eliza to abandon the cup-glass mapping
and choose the cup as her answer." [F95 p. 125] For Buridan 1, "there is a penalty for incoherent
mappings, but it is not severe enough to prevent the program from making them" [F95 p. 130], so
French raised it: "All the subsequent configurations in this family were run with this modified
version of the program, which is more strongly resistant to incoherent mappings." [F95 p. 130]
(§7; the `buridan` profile, ANSWERS.md 7.) Of Fig. 4.13's run, which mapped much and
still chose the isolated cup, "The structure value for such a run is, perhaps surprisingly,
quite good" [F95 p. 109].

**FCCA's structure-value.** "To this end, there is a dynamically varying number called the
structure-value, which is intended to summarize the total strength of the current Worldview."
[FCCA p. 387] It is "essentially a sum" of the strengths of "all the structures in the
Worldview, with penalties for incoherence" [FCCA p. 387]. "By 'incoherence', we mean a
situation in which there are two or more correspondences emanating from within a single group
(that is, from different items inside the group) but terminating in unrelated spots on the
table." [FCCA p. 387] Stated positively, "when correspondences reinforce each other by linking
two groups together on an element-by-element basis, there is a 'reward' to the Worldview in the
sense that its structure-value is increased" [FCCA p. 387]. "Moreover, when two or more
correspondences are supported by the same type of conceptual slippage, there is also a reward."
[FCCA p. 387] "These two ways of rewarding Worldviews for coherence effectively encourage
tightly linked systems of correspondences to arise and survive." [FCCA p. 387] "At the end of a
run, the value of this number can serve a useful secondary function" [FCCA p. 387], that of "a
'quality measure' of the answer produced by the program" [FCCA p. 387]: "the program's way of
telling us how well it 'likes' the answer it has come up with" [FCCA p. 387]. The pressure to
maximize it is balanced by time pressure (§6).

The two accounts differ in one word: French sums "the strength ratings of all of the
correspondences" [F95 p. 74], FCCA "all the structures in the Worldview" [FCCA p. 387], which
would include the groups' strengths (§4.4.3). Both readings are recorded; the choice is D-042
and the number reported at the end of a run is D-062. **UNSPECIFIED:** the coherence measure
and its penalty, the element-by-element reward, the parallelism points and how subworlds and
levels enter (→ D-042, D-043).

### 4.8 The unspecified points of the Workspace

| Point | Where the sources stop | Decision |
|---|---|---|
| The proximity criterion of a gestalt group | [F95 p. 97; F95 p. 125; FCCA p. 390] | D-022, D-118 |
| How find-group looks and what it proposes | [F95 p. 65; F95 p. 76; FH91 p. 178] | D-023 |
| Bottom-up groups: the stages and their pass probabilities | [F95 p. 65; F95 p. 98 (img); F95 p. 140; FCCA p. 384] | D-024 |
| Group strength: weights, scale, fights | [F95 p. 73; FCCA pp. 382-383] | D-025 |
| Subgroups kept (the fault, fixed) | [F95 p. 101; F95 p. 140] | D-026 |
| Neighbours: which members, along what axis | [F95 p. 76; F95 p. 80; F95 p. 97] | D-027 |
| Ends, middles, orientation; amorphous groups | [F95 p. 63; F95 p. 69; F95 p. 97] | D-028 |
| Two frames of position: in-group and table-wide | [F95 p. 68; F95 p. 85] | D-029 |
| Which slippages feed which symmetry label | [F95 p. 68; F95 p. 85] | D-030 |
| Salience weights | [F95 pp. 66-67; FCCA p. 385] | D-031 |
| The region order past the first two | [F95 p. 66; F95 p. 93] | D-032 |
| The proximity term | [F95 p. 66] | D-033 |
| Temperature in salience choices | [F95 p. 93] | D-034 |
| Creation conditions | [F95 pp. 71-72] | D-035 |
| Static points | [F95 pp. 68-69; FCCA p. 382] | D-036 |
| Dynamic points | [F95 p. 69; FCCA p. 382] | D-037, D-116 |
| The bonus and the penalty | [F95 p. 68; F95 p. 72] | D-038, D-117 |
| The test stage | [F95 p. 72; F95 p. 101] | D-039 |
| The winner of a competition | [F95 p. 72; F95 p. 105; FCCA p. 382] | D-040 |
| Removal from the Workspace | [F95 p. 73] | D-041 |
| Coherence: measure, penalty, reward | [F95 p. 74; F95 p. 101; F95 p. 105; F95 pp. 124-125; F95 p. 130; FCCA p. 387] | D-042 |
| Parallelism, subworlds and levels | [F95 pp. 74-75; FCCA p. 387] | D-043 |
| Conflicts when a group is mapped | [F95 p. 71; F95 p. 124] | D-044 |
| The group-to-object penalty (too strong, fixed) | [F95 p. 111; F95 p. 125] | D-045 |
| The group-level preference (too strong, fixed) | [F95 p. 109] | D-046 |

## 5. The Coderack

The Coderack is "the staging area for the codelets" [F95 p. 76]: the codelets wait there, each
with an urgency and an age, until one is chosen to run. This section gathers what the sources say
about the codelets themselves (what one does, which types exist), how they are posted, how one
is chosen, French's "urgency explosion" and the generation mechanism that answers it, and the
rack's capacity and the age of its codelets. The text layer of F95 pp. 75-82 and 92-94 is clean;
the two formulas (pp. 81, 91) and the tables of pp. 77, 92-93 were read on the page images.

### 5.1 Codelets and what one does

"Codelets are small pieces of code that perform certain simple actions. In Tabletop, there are
approximately thirty different codelet types. Typically, a codelet searches for (or builds) some
particular kind of structure associated with some specific concept in the Slipnet." [F95 p. 63]
A run is a long sequence of them: "A key idea of the architecture is that each type of
perceptual activity is implemented as a sequence of small, independent codelets." [FCCA p. 383]
Building a group, for instance, "involves an escalating series of 'micro-tests' that check the
physical and conceptual distances between prospective group members, as well as the
compatibility of the prospective group with other already-established groups. If any such test
fails, the potential group is aborted; if it succeeds, the way is clear for further tests to be
carried out." [FCCA p. 384] The codelets of many such sequences "are interleaved stochastically
so as to bring about parallelism of higher-level processes" [FCCA p. 384]; the program runs on a
serial computer, and its parallelism "is thus effective parallelism, somewhat like classical
time-sharing on a mainframe computer, rather than hardware parallelism" [FCCA p. 384].

**The threefold action.** "A typical codelet generally has a threefold action" [F95 p. 75]:

1. **Description-building.** "It notices a particular aspect of the Tableworld - for example, a
   group of objects, the neighbor of a particular object, the object on the end of a group, a
   correspondence between two objects, the presence of a particular group of objects, and so
   on - and attaches an appropriate descriptor to the object concerned." [F95 p. 75]
2. **Sending activation to the Slipnet.** "It sends activation to the Slipnet node that
   corresponds to the discovery that it has made, allowing the Slipnet to 'flex' in reaction to
   what has been discovered on the table. For example, if a codelet discovers a group of
   objects, it sends activation to the 'group' node in the Slipnet." [F95 p. 76] (The jolts of
   §3.6; their size is D-016.)
3. **Engendering other codelets.** "This is a crucial part of a codelet's task. A codelet that
   has just succeeded will put on the Coderack codelets that 'follow up' the just-completed
   action." [F95 p. 76] The example is find-group: it posts codelets "that will hunt for the end
   objects of the group just found, others that will hunt for the middle objects of the group,
   others that will pick elements from the group and determine their immediate neighbors, and
   so on" [F95 p. 76]. §5.3 has the full list.

FCCA's Preface 9 names, among the lesions a model like this should be tested with, telescoping
"the series of scout and effector codelets for each given type of action into one single
codelet" [FCCA p. 365], the scout that proposes and the effector that builds (§2.8);
Tabletop's micro-tests are that division.

### 5.2 The catalogue: the names in the text and the 23 labels on the screen

**Named in the text.** The book names these codelet types (with a parameter where the text gives
one):

| Codelet | What it does | Page |
|---|---|---|
| find-group | "look for groups of objects" and jolt "group" | [F95 p. 63] |
| find-group-around (small-glass1) | the worked example's first codelet: a find-group codelet with an object as its parameter | [F95 p. 77] |
| find-neighbor (cup1) | "look for the neighbor of a particular object" and jolt "neighbor"; spawns find-neighbor of the neighbour it found | [F95 p. 63; F95 p. 80] |
| find-end | "look at the ends of groups"; jolts the end object's category and "end" | [F95 p. 63] |
| find-end-object, find-middle-object | the end objects and the middle objects of a group just found | [F95 p. 76] |
| find-same-obj | given an object or a group, "looks for an identical copy" elsewhere; jolts "sameness" | [F95 p. 63] |
| sameness-detector | "looks to see if that exact configuration of objects appears elsewhere on the table" (the same job, under its other name) | [F95 p. 76] |
| search-for-same-group | posted in numbers when a subgroup is built; "search for other occurrences of spoon-spoon groups" | [F95 p. 101] |
| build-object (fork1, fork2) | the worked example's third codelet: builds a group out of two objects | [F95 p. 77] |
| pick-salient-object, pick-salient-correspondence | "simply pick an object (or group of objects) or a correspondence between objects (or groups of objects) for further processing" | [F95 p. 65] |
| examine-cp (cp1) | takes a group-level correspondence and looks for correspondences between the objects inside the two groups | [F95 p. 85] |
| find-answer | announces the object corresponding to the touched object and stops the program | [F95 p. 94; F95 p. 105] |

**The screen's labels.** The screen dumps show the Coderack as a bar graph, "indicating the
percentage of particular codelet types waiting to run" [F95 p. 97]; "In general, the types of
codelets that run early in the program are to the left of the bar graph; those that run later
are to the right." [F95 p. 97] The strip under Fig. 4.9, read at 600 dpi, carries 23 labels, in
this order [F95 p. 107 (img)]; the page of Fig. 4.1 prints a legend beside the rotated dump
giving each label's full name, transcribed in the second column [F95 p. 96 (img)] (item 05):

| # | Label | Legend [F95 p. 96 (img)] | Reading | Where the reading comes from |
|---|---|---|---|---|
| 1 | FND-GP | find-group | find-group | the text [F95 p. 63]; "21 percent" of the rack at Fig. 4.3 [F95 p. 101] |
| 2 | SAM | simple-sameness-detect. | sameness-detector / find-same-obj | the text [F95 p. 63; F95 p. 76] |
| 3 | FD-END | find-end-objs. | find-end-object | the text [F95 p. 76] |
| 4 | FD-MID | find-middle-objs. | find-middle-object | the text [F95 p. 76] |
| 5 | FD-SIMN | find-sim.-nhbr | find-similar-neighbor | the legend |
| 6 | FD-NNBR | find-neighbor | find-neighbor | the text [F95 p. 63; F95 p. 80] |
| 7-9 | BD-OB0, BD-OB1, BD-OB2 | build-obj-0, build-obj-1, build-obj-2 | build-object, in three stages (FCCA's "escalating series of 'micro-tests'" [FCCA p. 384]) | the legend and FCCA |
| 10 | BD-OB-DSC | build-obj.-desc. | build-object-description | the legend |
| 11 | SC-SMGP | search-for-sam.-gp | search-for-same-group | the text [F95 p. 101] (the "70" bar of Fig. 4.5) |
| 12 | SC-SIMGP | search-for-sim.-gp | search-for-similar-group (FCCA: "identical or similar groups" [FCCA p. 386]) | the legend and FCCA |
| 13-15 | PK-SALO, PK-SALS, PK2SALO | pick-salient-obj, pick-salient-subgp., pick-2nd-salient-obj | pick-salient-object, pick-salient-subgroup, pick-second-salient-object | the legend; the text names only pick-salient-object [F95 p. 65] |
| 16 | CT-CP | create-cp | create-correspondence ("correspondence creation" [F95 p. 71]) | the legend |
| 17 | PK-SALC | pick-strong-cp | pick-salient-correspondence, under the screen's name pick-strong-cp | the text [F95 p. 65] |
| 18 | EXAM-CP | examine-cp | examine-cp | the text [F95 p. 85] |
| 19 | BLD-CP | build-cp-descrip. | build-correspondence-description (not build-correspondence, as the label suggests) | the legend |
| 20 | CP-COMP | cp-compete | correspondence competition (the fights of §4.7.3) | the legend |
| 21 | CHK-WV | check-worldview | check-Worldview | the legend |
| 22 | CP-CLN | clean-up-corresp. | clean-up-correspondences, the removal of losers (§4.7.4) | the legend |
| 23 | FND-ANS | find-answer | find-answer | the text [F95 p. 94] |

The same strip gives the rack's composition at the end of the chapter 4 run (55 codelets run):
SAM 5, FD-END 3, FD-MID 3, FD-NNBR 11, BD-OB0 35, PK-SALO 2, PK2SALO 8, CP-COMP 3 and FND-ANS
30, summing to 100 [F95 p. 107 (img)]. (A first reading at a glance placed four of these bars
between two labels; measured against the 23 evenly spaced labels, every bar in every dump sits
over one label, within a tenth of the spacing.) §6c and `docs/ch4-run.md` (item 05) transcribe
every dump. Fig. 4.2's strip, two codelets in, is read in §4.2 and §6c.4.

**Why there are no more at the start.** "A colleague once asked me why all possible codelet
types were not simply put on the Coderack at the beginning of a run of the program. The answer
is (again) scaling up." [F95 p. 76] "For example, if there are no groups on the table, it makes
no sense for there to be any perceptual agents hunting for the neighbors of objects, the end
objects of groups, and so on. Such codelets, if present, would run and inevitably fail because,
in order for an object to have a neighbor, it must be in a group." [F95 p. 76]

**UNSPECIFIED:** the exact set of types (the text says "approximately thirty", the screen and
its legend 23), the job of the labels the text does not describe, and each codelet's
parameters (→ D-051).

### 5.3 Posting: top-down from the Slipnet, and follow-ups

"Specifically, codelets are posted to the Coderack when" [F95 p. 63] one of two things happens:

1. **A concept is active.** "The activation of a particular concept in the Slipnet is high;
   codelets associated with that concept are posted to the Coderack (e.g., when the concept
   'group' is sufficiently active, find-group codelets are posted to the Coderack)." [F95
   p. 63] "An active concept in the Slipnet dispatches codelets to look for instances of itself
   in the Workspace." [F95 p. 64] The loop closes through the jolts: a found group sends
   activation to "group", "This amounts to a signal to the system to hunt for even more groups;
   thus more codelets are sent out to look for groups." [F95 p. 64] And it winds down by
   itself: "as the rate of group-finding falls (i.e., the rate of change of activation of
   'group' decreases) and less activation gets sent to 'group' - and thus the activation of
   'group' decays - fewer codelets will be sent out to look for groups" [F95 p. 64], which is
   "a tacit conclusion that there are probably no more groups to be found on the table" [F95
   p. 64] (the apple-picker of §4.4.1). In the chapter 4 run, the active "neighbor" node causes
   "more 'find-neighbor' codelets to be posted on the Coderack" and the "group" node "has thus
   spawned a fair number of similar find-group codelets" [F95 p. 97]. (The node-to-codelet
   table and the posting rule are D-020.)
2. **A codelet succeeds.** "A particular codelet runs and succeeds in doing the task for which
   it was designed; this codelet then posts a new set of codelets to the Coderack" [F95 p. 63].

**Find-group's follow-ups, the one list the book gives.** "When a find-group codelet finds a
group, in addition to sending activation to the 'group' node in the Slipnet and updating a
short-term record of the number of groups found (a kind of short-term memory), the program puts
the following codelets on the Coderack" [F95 p. 76]:

1. "A find-group codelet, which will look for more groups" [F95 p. 76];
2. "A sameness-detector, which looks to see if that exact configuration of objects appears
   elsewhere on the table" [F95 p. 76];
3. "A find-neighbor codelet, which takes as a parameter an object in the group just found and
   determines its neighbors" [F95 p. 76];
4. "A find-end-object codelet, which will find the end objects of the group" [F95 p. 76];
5. "A find-middle-object codelet, which will find objects in the middle of the group" [F95
   p. 76];
6. "Pick-salient-object codelets, which pick other salient objects on the table" [F95 p. 76].

Chapter 4 describes the same list in action: the find-group codelet "posted a number of other
codelets to the coderack, among which are: codelets that look for sameness groups ... within
the group just found, others whose job it is to determine the objects at the ends and in the
middle of the new group, and others that pick objects inside the group and 'look' to the right
and left of those objects to determine their immediate neighbors" [F95 p. 97]. The explosion example of §5.5 counts "six new codelets" from one find-group [F95
p. 77], and the generation example has one find-group spawn "a find-end-object codelet, a
find-middle-object codelet, and two find-neighbor codelets" [F95 p. 81]: the list is a kind, not
a fixed count.

**Other follow-ups the text names.** Find-neighbor spawns a find-neighbor on the neighbour it
found, down the group: "the first find-neighbor codelet might have as its parameter fork1. It
would find fork2 and spawn the codelet find-neighbor(fork2). If this codelet ran, it would find
knife1) and post the codelet find-neighbor(knife1), and so on." [F95 p. 80] When a group-level
correspondence is built, "a number of examine-cp(cp1) codelets were posted to the Coderack"
[F95 p. 85]. When a subgroup is built, "a large number of search-for-same-group codelets were
posted to the Coderack (shown by the tall bar in the middle, with '70' above it)" [F95 p. 101];
FCCA: "Once any group is built, the program devotes resources to looking for identical or
similar groups in other areas of the table." [FCCA p. 386] Find-answer codelets are posted by
the temperature on every cycle (§6.5). The initial rack (find-group, pick-salient-object,
pick-salient-correspondence) is in §4.2.

**The form of a codelet.** "A typical codelet hung on the Coderack looks like this: (codelet-name
codelet-age codelet-urgency)." [F95 p. 77] Its generation tag is added in §5.5.

**UNSPECIFIED:** the follow-up list of every type but find-group, how many of each are posted,
and the raw urgencies they are given (→ D-052); the number of each starting codelet (→ D-055).

### 5.4 Urgency and the choice of a codelet

"Each codelet put on the Coderack has an urgency - a number that represents approximately how
important the program estimates it is to run that particular codelet. The higher the number,
the more important it is to run the codelet." [F95 p. 76] FCCA calls the urgency "effectively
its probability of being chosen next" [FCCA p. 385], set from "its action's perceived promise"
[FCCA p. 385], and lists the factors that enter it: "the type of action that the codelet is
supposed to take; the location on the table where the proposed action will occur; the types of
items that the proposed action will involve; the strengths and/or saliences of the objects or
correspondences involved; and so on" [FCCA p. 385]; "a proposed codelet involving a salient
object would be given a higher urgency than one involving an object of low salience (all other
things being equal)" [FCCA p. 385]. The effect is the terraced scan: "sequences of logically
related high-urgency codelets will tend to be accelerated, while sequences of low-urgency
codelets will tend to be slowed down" [FCCA p. 385].

**The choice.** "Codelets are selected to run based on two factors: their urgency and the
current computational temperature." [F95 p. 77] Without temperature, the chance is the
urgency's share of the total. The worked example [F95 p. 77]:

| Codelet | Urgency | Probability |
|---|---|---|
| find-group-around (small-glass1) | 6 | .6 |
| find-neighbor (cup1) | 3 | .3 |
| build-object (fork1, fork2) | 1 | .1 |
| Total Coderack urgency | 10 | |

"It will choose the find-group-around codelet with a probability of .6, the find-neighbor
codelet with a probability of .3, and the build-object codelet with a probability of .1." [F95
p. 77]

**With temperature: the exponent a.** "Each codelet has an assigned urgency that determines
its probability of being executed, but urgency varies as a function of temperature. The higher
the temperature, the more random (i.e., the less biased) the selection of codelets; the lower
the temperature, the less random (i.e., the more biased). In the limit of very low temperatures,
this strong bias turns into determinism." [F95 p. 92] Each assigned urgency u becomes u^a, "a
is the 'temperature exponent'" [F95 p. 92]: the table of p. 92 lists 6^a, 3^a, 1^a, total
6^a + 3^a + 1^a [F95 p. 92 (img)]. "The value of a varies linearly between 6 (when the
temperature is 0) and 0.1 (when the temperature is 100). When the temperature is 100, the
temperature-dependent urgency for all codelets, for all intents and purposes, is 1." [F95
p. 92] (6^0.1 is 1.196, so "for all intents and purposes".) At that temperature "codelet
execution is an almost completely random process, with any codelet being as likely to run as
any other" [F95 p. 93]. At a = 6 [F95 p. 93 (img)]:

| Codelet | Assigned urgency | Temperature-dependent urgency |
|---|---|---|
| find-group-around (small-glass1) | 6 | 46,656 |
| find-neighbor (cup1) | 3 | 729 |
| build-object (fork1, fork2) | 1 | 1 |
| Total Coderack urgency | 10 | 47,386 |

"The probability that find-group-around will be chosen is 46,656/47,386, that is, 0.985
(compared to 0.6 when a = 1); that find-neighbor will be picked is 729/47,386, i.e., 0.015
(compared to 0.4 when a = 1); and that build-object will be picked is infinitesimally small
(less than 0.001, compared to 0.1 when a = 1)." [F95 p. 93] The "0.4" is a print slip: at a = 1
find-neighbor's chance is 3/10 = 0.3, as the table of p. 77 says; the data file records 0.3 and
notes the slip. Ties stay random: "(If two codelets have the same assigned urgency, the
selection between them remains, of course, random.)" [F95 p. 93]

The linear law is a(T) = 6 − 0.059 T for T in [0, 100]; what a is outside that range, and
whether the exponent applies to the adjusted urgency of §5.5 (the natural reading: it is the
urgency the codelet carries on the rack), are not said (→ D-060).

### 5.5 Urgency explosion and generations

**The three problems of codelet sequencing.** "There were three problems that had to be
overcome - namely: Determining which types of codelets should be spawned by a particular
codelet; Determining how a codelet sets the 'raw' urgency of the codelets it spawns; Preventing
urgency explosions - somewhat analogous to inflation in economics" [F95 p. 77]. The first two
"continue to pose problems which will have no easy solutions" [F95 p. 82]; the book solves only
the third.

**The explosion.** Suppose the rack holds "only two find-group codelets, each of urgency 10"
[F95 p. 77], each with half the total. "One of these codelets runs, spawning six new codelets,
each of which has an urgency of 10." [F95 p. 77] The surviving find-group now has 10 of 70:
"its importance has been reduced in one instant to approximately one-quarter of its previous
value" [F95 p. 77]. One of the children then runs and spawns its own, diminishing the importance of the
original find-group codelet to the point [F95 p. 77] "where it will probably never get called to
run, even though a mere two codelets before it was as important as any codelet on the Coderack"
[F95 p. 79] (Figs. 3.14-3.16 [F95 p. 78]). (In this copy of the book the sentence
of p. 77 that follows "This means that the urgency of the find-group" is garbled by a duplicated
passage, on the page image as well as in the text layer; the sense is as given.) "That this
should happen is antithetical to the whole philosophy of codelets. Codelets are small pieces of
code that should effect small, gradual changes in the building up (or breaking down) of
structure." [F95 p. 79] "If at generation t the program has certain resource priorities, it
seems wrong that they would have almost always changed completely by generation t + 2." [F95
p. 79] Copycat never met it, because there "each codelet engendered at most one follow-up
codelet" [F95 p. 79]; in Tabletop "one codelet may spawn four or five different codelets" [F95
p. 79]. "The crux of the problem is that one codelet of a given urgency can spawn many codelets
of the same (average) urgency." [F95 p. 79]

**The alternatives French weighed** [F95 pp. 79-81]:

1. "Adopt the Copycat strategy of having any codelet spawn at most one codelet." [F95 p. 79]
   Rejected: "This produces an unacceptable requirement of sequentiality in running codelets."
   [F95 p. 79] "Why should find-end-object necessarily run before find-middle-object? It should
   not" [F95 p. 79].
2. "Reduce the urgency of the spawned codelets by a constant amount." [F95 p. 79] Rejected: the
   explosion is only slower, and chains like find-neighbor's have no known length, so "codelets
   of zero urgency (or even negative urgency!) would be put on the Coderack" [F95 p. 80].
3. "Do not allow the total urgency of the spawned codelets to exceed the urgency of the spawning
   codelet." [F95 p. 80] Closer, but "In some cases, you want it to be possible to post a
   codelet to the Coderack with an extremely high urgency, far higher than its parent's urgency."
   [F95 p. 80]
4. "Spawn 'bundles' of codelets instead of individual codelets." [F95 p. 80] Rejected: once
   one codelet of the bundle has run "the others in the bundle would never have any chance
   whatsoever to run" [F95 p. 80].
5. A diminished bundle returned to the rack with its urgency reduced in proportion: "It seems to
   me that this technique might be able to overcome the problem and should be investigated by
   researchers implementing this type of codelet-based architecture. I did not explore this
   method." [F95 pp. 80-81]
6. **Adopted:** "Decrease the urgency of spawned codelets as an exponential function of their
   'generation.' This is the solution I have adopted." [F95 p. 81]

**Generations and the formula.** "Each codelet is tagged to indicate the generation to which it
belongs. The original codelets on the Coderack are of generation 0, their children are of
generation 1, their grandchildren are of generation 2, and so on." [F95 p. 81] "There is an
urgency-reduction factor defined as the urgency-reduction coefficient (a global variable, p,
currently set at 3) raised to the power of the codelet's generation (G in the formula below).
Each codelet has a raw urgency, typically an integer value between I and 100, which is the
urgency associated with the codelet originally." [F95 p. 81] (The text layer prints "I" for
"1"; the image reads 1 and 100.) The coefficient is ρ on the image, p in the text layer. "within
the definition of the find-group codelet, there will be an instruction to hang certain codelets
on the Coderack, among them find-end-object and find-middle-object, with certain raw urgencies,
say, 10. However, when a codelet is actually put on the Coderack, it is assigned an adjusted
urgency defined by" [F95 p. 81]

    adjusted urgency = raw urgency / ρ^G        [F95 p. 81 (img)]

"where p is the urgency reduction coefficient and G is the generation number of the codelet"
[F95 p. 81].

**The worked example** [F95 p. 81]: "Assume, for the purposes of example, that p = 2 and that
there are two second-generation find-group codelets on the Coderack, each with an (adjusted)
urgency of 4. One of these codelets runs and spawns a find-end-object codelet, a
find-middle-object codelet, and two find-neighbor codelets, each with a raw urgency of 8." Each
child is of generation 3, and its adjusted urgency is 8 / 2³ = 1 [F95 p. 81 (img)]. "Thus, the
total amount of urgency that the spawned codelets contribute to the Coderack is 1 + 1 + 1 + 1 =
4, as opposed to 32 in the standard implementation. In the standard implementation, the running
of a single find-group codelet would have reduced the importance of the competing find-group
codelet from 50 percent to 12.5 percent." [F95 p. 81] (4 of 36 is 11 percent; 12.5 is 4 of 32.
French's figure is recorded as printed.) "In the approach suggested in this section, the
remaining find-group codelet would actually gain in relative importance." [F95 pp. 81-82] Such
gains are not guaranteed: "the gain in this case is simply a result of the specific numbers used
in this example" [F95 p. 82], but French argues "that it is indeed reasonable to have older
codelets sometimes gain in relative importance with respect to more recently spawned ones"
[F95 p. 82], the "veneration of elders" [F95 p. 82]. A child may still outrank its parent: "it
is possible - even if it is never actually done in Tabletop - to post to the Coderack a codelet
whose adjusted urgency is greater than the adjusted urgency of its parent" [F95 p. 82].

**What ρ means.** "If p is 1, we have the original situation, in which all codelets, when put on
the Coderack, are given their raw urgency. If p is 1.5 or 2, then we have the situation
described above. As the size of p increases, the relative importance of parent and grandparent
codelets increases with respect to the younger generations of codelets." [F95 p. 82] A large ρ
"would produce the codelet equivalent of strict breadth-first search" [F95 p. 82]; "if p were
made very small (less than 1), this would ensure that the most promising of the newly spawned
codelets would always run, thus producing strict depth-first search" [F95 p. 82]. The program's
value is 3 [F95 p. 81].

**UNSPECIFIED:** the generation of a codelet posted top-down by the Slipnet, or by the
find-answer schedule, rather than by a parent (→ D-054); whether the adjusted urgency is
rounded, and its floor (raw 1 at generation 4 under ρ = 3 is 0.012) (→ D-052).

### 5.6 Capacity and age

"Each codelet also has an 'age' that represents how long it has been on the Coderack. A codelet
that has been on the Coderack too long is eliminated, because chances are that it is no longer
relevant to the continually evolving representation of the table." [F95 p. 76] Copycat removes
by urgency too: "urgency is divided by age, and if the resulting number is (probabilistically)
below a certain threshold, the codelet is removed. This has the effect of removing a number of
low-urgency codelets, even if they have not been on the Coderack too long. I chose to leave
low-urgency codelets on the Coderack since, at least in theory, this will lead to a greater
diversity in representational possibilities." [F95 p. 77] The rack is bounded: "because the
Tabletop Coderack can contain only a certain number of codelets (currently 150), the oldest
codelets will eventually be 'pushed off' the Coderack in order to make room for the newcomers"
[F95 p. 82]; "if the 'opinions' of the older codelets are not expressed (i.e., they have not yet
run) by the time of their demise, they will never be expressed" [F95 p. 82]. The breadth-first
limit of §5.5 assumes "codelets were never pushed off the Coderack when they became too old"
[F95 p. 82].

**UNSPECIFIED:** the maximum age, and whether age is counted in codelets run or in cycles
(→ D-053).

### 5.7 The unspecified points of the Coderack

| Point | Where the sources stop | Decision |
|---|---|---|
| The catalogue: the types, their jobs and parameters | [F95 p. 63; F95 pp. 76-77; F95 p. 85; F95 p. 94; F95 p. 107 (img)] | D-051 |
| Raw urgencies and the follow-up lists of every type but find-group; rounding and the floor of an adjusted urgency | [F95 p. 76; F95 p. 77; F95 p. 81; FCCA p. 385] | D-052 |
| The maximum age; the unit of age | [F95 p. 76; F95 p. 82] | D-053 |
| The generation of top-down and find-answer codelets | [F95 p. 81] | D-054 |
| The initial rack: how many of each | [F95 p. 65; F95 p. 98 (img)] | D-055 |
| The find-answer schedule: the cycle, the count as a function of T, the urgency | [F95 p. 94; F95 p. 105; F95 p. 107 (img)] | D-056, D-120 |

Given by the sources and not open: the selection rule (probability proportional to u^a), a(T)
from 6 to 0.1, the adjusted urgency raw/ρ^G with ρ = 3, raw urgencies in 1-100, the capacity
150 with the oldest pushed off, no removal by urgency, and find-group's follow-ups.

## 6. Temperature and the answer

Section 3.4.7 of the book, "Computational Temperature in Tabletop" [F95 p. 89], is "the final
area that must be considered" [F95 p. 89] and, with the find-answer codelets, how the program
decides it is done. French defers the general case: "The underlying mechanisms as well as the
justifications for computational temperature have been discussed at length by Hofstadter and
Mitchell (1991) and Mitchell (1993)." [F95 p. 89] The formula of p. 91 is an image; the text
layer drops its Greek letters and deltas, so the equation and its justifications were read on
the page.

### 6.1 What temperature means in Tabletop

"Computational temperature is a measure of the program's 'belief' that it has found all the
perceptual structures (groupings, correspondences, etc.) necessary for a good answer. In other
words, the lower the temperature, the more the program considers that it has spent enough time
and resources in its attempt to structure the Tableworld perceptually." [F95 p. 89] The idea
follows the local manner of finding groups: "Initially, a lot of new correspondences will be
found and put into the Worldview. Whenever, by chance, a new high-quality correspondence is
found, it will compete with, and usually replace, weaker competitors in the Worldview. In this
way, a process akin to natural selection among correspondences gradually improves the quality of
the Worldview." [F95 p. 89] "Ultimately, the correspondences in the Worldview will cease to be
replaced by new ones, simply because the ones already in the Worldview are stronger than any
competitors that come along." [F95 p. 89] This is no guarantee: "Sometimes it may never even notice a very good
correspondence. Or it may do so so late that it makes no difference." [F95 p. 90] And the
Worldview entrenches itself: "each time the Worldview emerges victorious from a competition
with a rival interpretation of the table, all of its correspondences are strengthened. Thus, the
longer a particular Worldview dominates the scene, the harder it is for new correspondences,
however good, to force themselves into it." [F95 p. 90]

So "just as a decline in the rate of group finding tells Tabletop there may not be many more
groups to be found ... a drop in the rate of good correspondences being added to the Worldview
tells Tabletop there may not be many more good correspondences to be found. In this case, the
temperature drops and the program expends fewer resources to discover new correspondences."
[F95 p. 90] "In Tabletop, the program uses a more indirect method than in Copycat of
ascertaining when it has achieved such a state." [F95 p. 90]

**Against Copycat.** "Copycat and Tabletop do not calculate temperature in exactly the same
way. In essence, the Tabletop calculation is much more local and therefore, at least
theoretically, would be more amenable to being scaled up to larger domains." [F95 p. 93] In
Copycat "Temperature is inversely proportional to the quality of inter- and intrastring
relationships" [F95 p. 93]; "In Tabletop, the emphasis is much less on good structure than on
changes in structure in the Worldview. The quality of the structures in the Worldview does play
a role, but it is much less significant and, especially, less direct than in Copycat." [F95
p. 93] The consequence when there is little to find: "In Copycat, under these circumstances the
temperature will remain high. In Tabletop, by contrast, the temperature will nonetheless fall,
because what is important is the change in the quality of the Worldview. When quality stops
changing over a significantly long time, the program effectively concludes that it has done its
best, and temperature falls." [F95 p. 94] "If, under these circumstances, the Worldview remains
unchanged for a period of time in the face of this bombardment, the conclusion must be that the
correspondences in the Worldview are probably pretty good and temperature falls." [F95 p. 94]

### 6.2 The four factors [F95 pp. 90-91]

"Four factors determine temperature in Tabletop" [F95 p. 90]:

1. **Quality of the Worldview.** "The quality of the Worldview is based on the average quality
   of the correspondences comprising it. In the formula for temperature, this is measured as a
   fraction of an optimally good correspondence." [F95 p. 90] So Q is a number in (0, 1], not
   the rating of §4.7.5 (which runs into the hundreds: the example below quotes ratings of 274
   and 285); how a correspondence's quality is normalized to the optimum is not said (→ D-049).
2. **Average rate of change of the quality.** "This is the heart of this particular means of
   computing temperature. The idea is that if very few changes are occurring in the quality of
   the Worldview, the program has probably found a good representation of the table, and the
   temperature drops." [F95 p. 90] "The rate of change of Worldview quality is determined by
   the difference between the quality rating of the current Worldview and the rating of the
   previous Worldview. This rate of change of the quality of the Worldview is averaged in with
   a certain number of past rates of change (generally averaged over the last six)." [F95
   p. 90] Whether
   the differences are signed or absolute is not said (→ D-048).
3. **Number of changes of the correspondences in the Worldview.** Quality alone misses
   oscillation: two rivals "almost equally good" may "more or less alternately win the
   competition. They take turns moving in and out of the Worldview. Thus the variable containing
   the quality ratings of the Worldview would look, for example, like this: 274 (with the first
   correspondence), 267 (with the second), 280 (with the first), 273 (with the second again),
   285 (with the first)." [F95 p. 90] "In this scenario, the change in quality between
   alternate Worldviews is very small, even though there is a considerable amount of change in
   the identity of the correspondences." [F95 p. 90] French argues both sides [F95 pp. 90-91] and chooses the
   second: "continual oscillation between two different choices should prevent the temperature
   from falling, since the system is not settling on a single representation" [F95 p. 91]; "The
   latter alternative was the one implemented in Tabletop, because it seems that temperature
   should represent one's degree of satisfaction with one particular representation or solution
   to a problem." [F95 p. 91] "For this reason, the number of actual physical changes in the
   correspondences in the Worldview is also taken into consideration. In short, the more often
   the Worldview changes, the higher the temperature." [F95 p. 91]
4. **Number of competitions.** "Since the program considers picking an answer only when the
   temperature is sufficiently low, a Buridan's-ass type of paralytic indecision could be
   created by the presence of two equally good correspondences" [F95 p. 91]. "The program, like
   people, must eventually tire of trying to decide, and that is why temperature slowly falls
   as a function of the number of competitions." [F95 p. 91]

### 6.3 The formula [F95 p. 91 (img)]

"The equation incorporating these four factors to calculate temperature is as follows" [F95
p. 91]:

    T = α · ΔQ / Q + β · Δ_WV − γ · T_cp        [F95 p. 91 (img)]

where, as the page defines them (the equation is an image and has every Greek letter and Δ;
the definitions and the three cases below are text in which the PDF prints a blank where each
Δ, α, β and γ should stand, in the text layer and on the rendered page alike, so those letters
are restored from the equation and the sense; `docs/citation-audit.md`): Q "is the average quality of the Worldview" with 0 < Q ≤ 1 [F95 p. 91 (img)]; ΔQ "is the
change in quality of the Worldview" [F95 p. 91] (the averaged rate of factor 2); α, β and γ "are
positive constants" [F95 p. 91]; Δ_WV "is the number of changes in the Worldview in the last ten
competitons" [F95 p. 91] (sic); T_cp "is the total number of competitions" [F95 p. 91].
"Note that the values of the coefficients β and γ are chosen to ensure that the latter two terms
determining temperature play a less significant role than ΔQ/Q, which is the dominant factor in
the calculation of temperature." [F95 p. 91 (img)] (The text layer prints "# and y" for β and
γ, and "AQ/Q".) French's justification, in three cases [F95 pp. 91-92]:

- "If ΔQ is small (few recent changes in the Worldview) and Q is large (good quality of the
  structures), then ΔQ/Q is small. Consequently, the temperature is low." [F95 p. 91 (img)]
- "If ΔQ is large (many of recent changes in the Worldview), the temperature will remain
  relatively high, because Q would have to be very high to compensate for this. Since Q can be
  at most 1, a large ΔQ will necessarily produce a high temperature." [F95 p. 92 (img)]
- "If Q is small (the quality of the Worldview is poor), the temperature will remain high
  unless no better structure is found. If no additional structure is found, even if Q is high,
  the value of ΔQ will decrease over time, thus causing ΔQ/Q to become small as well. The
  temperature will then decrease." [F95 p. 92 (img)]

The three bullets are checked against the page (the PDF drops each Δ, which makes "If Q is
small" and "If Q is large" look contradictory; the blanks are on the rendered page too, not
only in the text layer); the words are as printed with Δ restored from the equation's
definitions, which is why they carry the (img) tag.

What the formula leaves open: the values of α, β, γ (→ D-047); the sign of ΔQ (→ D-048); the
scale of Q, the temperature's range (the thermometer of the screen runs 0-100, and the exponent
a of §5.4 is defined for 0-100, but the equation is not clamped on the page) and the value
before the first competition (→ D-049); when T is recomputed (→ D-050). Chapter 4's narrative
constrains the dynamics: the temperature falls while the Worldview's correspondences "have
changed very little" [F95 p. 101]; it rises slightly when a strong correspondence enters without
a fight, because "adding the new correspondence is taken by the program as a hint that there
may be still more such correspondences" [F95 p. 105], against the push to decrease from a very
strong addition; and it drops when a fight replaces an incoherent Worldview by a coherent one,
since "the overall Worldview rating has improved so much (the program is quite heavily biased
against incoherent Worldviews) that this fact by far dominates the pressure to increase the
temperature" [F95 p. 105]. The thermometer readings of the fourteen dumps are transcribed in
§6c (item 05) and are the direct evidence for the initial value.

### 6.4 What temperature does

"Temperature has roughly the same effect on Tabletop, affecting all of its decision making."
[F95 p. 92] The sources name these uses:

1. **Codelet selection**, through the exponent a (§5.4) [F95 pp. 92-93].
2. **The choice of a region.** "As might be expected, temperature affects not only codelet
   urgencies but all of the probabilistic processes of the program. A simple example is the
   action taken by a pick-salient-object codelet." [F95 p. 93] "When the temperature is very
   low, the program deterministically respects the priorities of the regions that human subjects
   were shown to choose preferentially with respect to the touched object. In other words, an
   area diagonally opposite the touched object is most preferred, an area directly opposite the
   touched object is next, and so on." [F95 p. 93] "However, if the temperature goes up, the
   chances decrease that the program will rigorously respect this order. If the temperature
   should ever happen to reach 100, the program is as likely to choose any region as any other."
   [F95 p. 93] (The
   region order is D-032.)
3. **Choices by salience.** "The selection of objects or correspondences based on their salience
   works in the same way. The selection process approaches determinism as the temperature
   approaches 0, is completely unbiased (i.e., uniformly distributed random) at 100, and has
   varying degrees of bias at intermediate temperatures." [F95 p. 93] (D-034.)
4. **The find-answer schedule**: "The lower the temperature, the more such codelets are hung on
   the Coderack." [F95 p. 94] (§6.5.)
5. **Odd structures at high temperature.** Of the cup-spoon group of Fig. 4.14: "It is rare for
   a cup-spoon grouping, as shown here, to survive. The program is, in general, highly unlikely
   to group together objects of such disparate categories." [F95 p. 110] "However, if the
   temperature is high, the program is relatively willing to try such things out. Similarly, it
   strongly resists mapping groups onto individual objects, although under some circumstances it
   might be happy mapping a spoon-spoon group to an individual spoon." [F95 p. 111] "At lower
   temperatures, such a correspondence would almost certainly never have been given the time of
   day." [F95 p. 111]

How temperature enters each choice other than codelet selection (the same exponent, or another
law) is not said (→ D-060).

### 6.5 Find-answer and the end of a run

§2.7 quotes the mechanism in full; the points of substance are these. "when the temperature is
sufficiently low, the program concludes that the representation it has built is as good as it is
going to get, and it selects the answer that is consistent with the structure it has built up"
[F95 p. 94]. There is "no sharp temperature threshold" [F95 p. 94]: "on each cycle of the
program, a certain number of find-answer codelets are put on the Coderack. The lower the
temperature, the more such codelets are hung on the Coderack. When a find-answer codelet runs,
an answer to the 'Do this!' challenge is picked, and the program stops." [F95 p. 94] A
find-answer codelet's "function is to announce the current object corresponding to the touched
object and to stop the program" [F95 p. 105]; "The run ends only when a find-answer codelet is
run." [F95 p. 105] The answer is read off the Worldview: "In order for the program to give an
answer, it needs to have built up one or more mappings between objects or groups of objects
across the table (called correspondences)." [FH91 p. 180] "the default object corresponding to
the touched object is the touched object itself" [F95 p. 105], so a very early find-answer makes
Eliza touch Henry's object.

The dumps show the schedule at work: at the end of the chapter 4 run, 55 codelets in, the
FND-ANS bar is 30 percent of the rack [F95 p. 107 (img)], after the temperature has fallen
because "The correspondences in the Worldview in figure 4.9 are no longer changing" [F95
p. 105]; and a slight rise of temperature earlier was "slightly reducing the probability for the
program to run a find-answer codelet" [F95 p. 105]. FCCA frames the end as a trade: "An
important pressure on Tabletop is therefore to maximize structure-value; counterbalancing this,
however, is a competing pressure - time pressure - that pushes for the program to finish within
a reasonable amount of time." [FCCA p. 387]

**UNSPECIFIED:** what a cycle is, how many find-answer codelets it posts at a given temperature,
and their urgency (→ D-056); which object a find-answer codelet announces when the touched
object is mapped only through its group (→ D-057, D-058); how an answer that French reports as
"either glass" is matched (→ D-059); a cap on the run (none is mentioned; → D-061); the
structure value reported for a run (→ D-062); the default when no correspondence contains the
touched object (→ D-063).

### 6.6 The output of a run

§2.7 has the sources: the object touched, "the sequence of structures that led to that answer,
the amount of resources expended, and the type and quality of structures present at the end of
the run" [F95 p. 145]; the answer, the final structure-value and the run-length in codelets
[FCCA p. 392]. The reconstruction's text trace ends with a line in that form (the object, the
structure value, the codelet count, the temperature); the JSON-lines trace is defined in
`docs/trace-format.md` (drafted in item 10, final in item 21). French notes one regularity the
benches can test: "in general, the better the structure built by the program, the more codelets
will be needed" [F95 p. 114], and that the most frequent answer need not have the best
structure-value, which "rather than reflecting a defect of the architecture, reflect an
inevitable fact about high-level perception: deep perceptions are often hard to discover" [FCCA
p. 392].

### 6.7 The unspecified points of temperature and the answer

| Point | Where the sources stop | Decision |
|---|---|---|
| α, β, γ | [F95 p. 91 (img)] | D-047 |
| ΔQ signed or absolute | [F95 p. 90] | D-048 |
| Q's normalization, the range of T, the initial value | [F95 p. 90; F95 p. 91 (img); F95 p. 98 (img)] | D-049 |
| When T is recomputed | [F95 pp. 90-91; F95 p. 101; F95 p. 105] | D-050 |
| What find-answer announces | [F95 p. 94; F95 p. 105; FH91 p. 180] | D-057 |
| A touched object matched only through its group | [F95 p. 105] | D-058 |
| "Either glass" answers | [F95 p. 125] | D-059 |
| Temperature in choices other than codelet selection | [F95 p. 93] | D-060 |
| A run cap | none found; [FCCA p. 387] | D-061 |
| The structure value reported | [F95 p. 145; FCCA p. 392] | D-062 |
| The default answer before any structure | [F95 p. 105] | D-063 |

### 6b. French's faults

Every place where French says that his program does something wrong, too much or too little.
The search covered the whole text layer of F95 (every page, with a list of self-critical phrases,
then the context of every hit read), FCCA Preface 8, chapter 8, Preface 9 and chapter 9 (pp.
319-399) and FH91 the same way. ANSWERS.md 2 fixes every fault found in the default profile, as
a decision marked FIXED, with the faulty reading kept for the `french-faults` profile; the
decisions are written in item 10, and the table names them. The first four are the ones PLAN.md
§0 listed; the others were found in this search.

| # | Fault | Page | Decision |
|---|---|---|---|
| 1 | All intermediate subgroups kept, "hopelessly confused" beyond three identical objects | [F95 p. 140] | D-026 |
| 2 | The bias against group-to-isolated-object mappings, "should probably be relaxed" | [F95 p. 125] | D-045 |
| 3 | The group-level bias, "clearly a bit too strong" | [F95 p. 109] | D-046 |
| 4 | The lossy reconstruction after a context pop, "too much is lost" | [F95 p. 89] | D-021 |
| 5 | The context push reinitializes only the two symmetry nodes, against the proposal French endorses | [F95 p. 88] | D-070 |
| 6 | A group built out of the touched object and a conceptually distant neighbour, "the rather unhumanlike action" | [F95 pp. 141-142] | D-069 |
| 7 | Amorphous groups: only one end object found where humans see two | [F95 p. 63] | D-028 |

**Fault 1: every subgroup kept** [F95 p. 140]. "the process by which Tabletop builds large groups
of identical objects (by progressively adding adjacent objects to already-existing groups) is
flawed" [F95 p. 140]: for six cups "the program would end up making many of the possible
groupings of cups (e.g., cup1-cup2, cup2-cup3, cup3-cup4, etc., and then cup1-cup2-cup3,
cup3-cup4-cup5, etc., gradually working its way up to the entire group" [F95 p. 140]. "It keeps
all of the intermediate subgroups around, considering each of them important in its own right and
trying to establish mappings with other 'similar' groups." [F95 p. 140] "Unlike humans, however,
Tabletop retains all of the subgroups, which means that it gets hopelessly confused whenever it is
given a problem containing groups made up of more than three identical objects. This is a problem
with Tabletop that needs to be fixed." [F95 p. 140] *What the program did:* kept every
intermediate sameness subgroup as a structure in its own right, each mapped and fought over
(§4.4.4). *The fix French implies:* humans "tend to see them simply as a group of cups, and only
under the most exceptional of circumstances as, say, a group of two cups next to a group of four
cups" [F95 p. 140]; so a sameness subgroup that is absorbed into a larger sameness group of the
same type is dropped (or demoted) rather than kept. *Why Figure/Ground 1 has only two forks and
two glasses:* this fault is the reason, "the problem as presented to Tabletop contains only two
forks and two glasses as background" [F95 p. 140], so the fix cannot move that result set by
itself; it may move every problem with three or more identical adjacent objects.

**Fault 2: the bias against group-to-object mappings** [F95 p. 125]. "It should be noted that
Tabletop favors group-to-group mappings and abhors group-to-isolated-object mappings. Therefore,
even though the diagonal mapping in the Blockage 3 configuration might seem cleaner (because it
involves only glasses), the program does not make it (or only very rarely), whereas in Blockage 4
it does make the (somewhat forced) diagonal mapping between the glass-glass group and the
glass-spoon group. This strong bias against group-to-isolated-object mappings should probably be
relaxed." [F95 p. 125] *What the program did:* a static factor of a correspondence penalized a
group mapped to a single object so heavily that the glass-glass group of Blockage 3 was almost
never mapped onto the isolated glass diagonally opposite (§4.6.2). *The fix:* the penalty is
relaxed, by an amount chosen a priori (ANSWERS.md 2) and recorded in D-045. *Result sets it may
move:* Blockage 3 and 4 directly, and every problem with an isolated object across from a group.

**Fault 3: the group-level bias** [F95 p. 109]. Of Fig. 4.12: "Notice also that the program did
not find the knife-knife correspondence, even though this correspondence is extremely obvious to
humans, especially once the neighboring correspondence between the two fork groups has been
found. This seemingly glaring oversight is due to the fact that, when there are groups of objects
on the table, the program is strongly biased to hunt for group-level correspondences in
preference to object-level correspondences. Thus, this bias is clearly a bit too strong." [F95
p. 109] *What the program did:* once groups exist, the codelets that pick things to map (and the
salience factor "being a group" of §4.5) favoured groups so much that an obvious object-level
correspondence beside a found group-level one went unbuilt. *The fix:* the bias is reduced "a
bit", by an amount chosen a priori and recorded in D-046. *Result sets it may move:* any whose
structure value counts object-level correspondences inside mapped groups (the Surround family's
"average structure" columns in particular).

**Fault 4: the lossy reconstruction after a context pop** [F95 p. 89]. §3.10 has the mechanism.
"A more complex reconstruction technique than the one implemented in Tabletop is needed, however.
It is not enough merely to put a fixed amount of activation into the Slipnet for each concept
encountered in the structures present at a particular level of the Worldview." [F95 p. 89]
"Although this certainly captures some of the information stored in the prior patterns of
activation, too much is lost. For example, all of the (possibly considerable) activation that
might have spread to these concepts from other concepts is lost." [F95 p. 89] *What the program did:* on popping to a
former context, it reset the two symmetry nodes and put "a fixed amount of activation" [F95
p. 88] into "right", "left" and the symmetry of each slippage it found [F95 pp. 88-89]. *The fix
French implies:* "More refined reconstruction techniques, perhaps taking into account the
activation levels of the nodes connected to the reinitialized nodes, might permit a greater
amount of the information to be recovered" [F95 p. 89]; he rules out a stack ("too 'clean'"
[F95 p. 88]) and wants reconstruction, so the fix adds to the fixed amounts the activation that
would have spread into the reinitialized nodes from their neighbours at their current levels
(one spreading step), recorded in D-021. *Result sets it may move:* only problems where
examine-cp enters and leaves a subworld (groups mapped to groups with correspondences inside
them): the chapter 4 configuration and the Surround family.

**Fault 5: what the context push reinitializes** [F95 p. 88]. Of the two proposals for which
concepts to reset on entering a subworld, "The solution adopted by Tabletop was essentially the
latter. In reality, however, only the 'mirror-symmetry' and 'diagonal-symmetry' nodes are
reinitialized, these being the only concepts that would have had a significant influence on the
subsequent formation of 'sub-correspondences'." [F95 p. 88] "The problem of which activation
levels should be reinitialized, to what degree, and under what contextual circumstances
definitely deserves to be studied more carefully. The most fruitful path of research would seem
to be along the lines of the second proposal above." [F95 p. 88] *What the program did:* reset
two nodes. *The fix French points to:* the second proposal as stated, "Only concepts that
participate in the supporting slippages of the correspondence under investigation are
reinitialized." [F95 p. 88] So the push resets every node that a supporting slippage of the
correspondence names (its two concepts and its label), not only the two symmetries; D-070
records the set, with the two-node reading as the `french-faults` value. The pop of fault 4
restores the same set. This is the weakest entry of the table: French presents the two-node
version as a simplification of the policy he adopted, not as an error, but he says in the same
breath that the policy deserves study and names the direction. *Result sets it may move:* the
same as fault 4.

**Fault 6: the touched object pulls a conceptually distant neighbour into a group** [F95
pp. 141-142]. In Figure/Ground 1, "a surprising mapping, one that humans would be very unlikely
to make, was often made by the program to produce its answer" [F95 p. 141]. "Now for the rather
unhumanlike action: The program will often also create a forkglass group containing the touched
fork (see figure 5.3)." [F95 p. 141] "However, the most salient object on the table is always
the object touched by Henry. Because of the high degree of salience of the touched fork, the
program 'reasons' that if it creates a group out of the touched fork and the neighboring glass,
the new group will be, if not a great one, at least not too bad. Indeed, without the presence of
the touched object, such a group would probably never be made in the first place." [F95 p. 142]
Then "there is again a top-down pressure to find groups like it on the table. In this case, the
program does indeed find an exact copy of this group on the other side of the table" [F95
p. 142], maps the two, and "one of the mappings is from the touched fork to the fork on Henry's
side of the table. The program ranks this as a very strong mapping and puts it in the
Worldview. But this mapping constitutes an answer! And in fact it is the answer generally given
by the program." [F95 p. 142] *What the program did:* let the salience of a member raise the
strength of a group enough to build a group of two conceptually distant objects (§4.4.2 says
such groups are built "only very rarely" [F95 p. 65]). *The fix French's words imply:* the
touched object's salience (or any member's) does not count towards the strength of a group of
conceptually distant members; a fork-glass group is as unlikely with the touched fork as
without it. D-069 records it, with the salience term kept for `french-faults`. *Result sets it
may move:* Figure/Ground 1 above all (French reports E1/E2 at 92 structure and 57 codelets and
E3 never [F95 p. 139]); the fix removes the route by which the program reached E1/E2, so the
`standard` numbers may diverge from his there, as ANSWERS.md 2 accepts. Any problem where the
touched object has a conceptually distant neighbour may move a little.

**Fault 7: amorphous groups and their ends** [F95 p. 63]. "(Note: Tabletop cannot handle
completely amorphous groups. For groups that have clear horizontal or vertical ends, it will
find them. However, for more amorphous groups, it will conclude that certain objects are
horizontal or vertical end objects, even though we humans might think that they are very weak
or questionable end objects." [F95 p. 63] Of an L-shaped group: "Even though we would clearly
perceive both objects at the ends of the L to be the end objects of this group, the program will
choose only one as the upper end object.)" [F95 p. 63] (The text layer drops the letter L; the
page shows it.) *What the program did:* took the extreme object along an axis as the end, so an
L-shaped group had one upper end and no end at the other arm. *The fix:* the ends of a group
whose members know their neighbours are the two extremities of the chain of neighbours, whatever
its shape; the axis reading stays for gestalt groups without neighbours and for `french-faults`.
Recorded in D-028. *Result sets it may move:* none of chapter 5's layouts has an L-shaped group
(§1.6 and the problem library of items 07-09 will say), so none is expected to move.

**Named and defended, so not faults.** These passages read like criticism but French (or FCCA)
argues the behaviour is right, so the reconstruction keeps it:

- Strange structures at high temperature (the cup-spoon group and its mapping to a spoon, Fig.
  4.14): "it would be wrong to consider the presence, on rare occasions, of such structures to
  be a mistake" [F95 p. 111]; the condition is only "that you do not want such structures to be
  built too often or to be given overly strong ratings" [F95 p. 111].
- The heavy bias against incoherent Worldviews [F95 p. 105]: a design choice, which French
  raised further for Buridan 2-13 [F95 p. 130]; that raise is a calibration, the `buridan`
  profile (ANSWERS.md 7), not a fix.
- The program weighting "structural relations more heavily" than the subjects, who "were far
  more influenced by object-type matches" [F95 p. 142], answered by raising the object-type
  weight for the second runs [F95 p. 143]: a calibration, the `human` profile (ANSWERS.md 7).
- The most frequent answer not having the best structure-value: "rather than reflecting a
  defect of the architecture, reflect an inevitable fact about high-level perception" [FCCA
  p. 392].
- Missed structure at the end of a run (the silverware correspondences of Fig. 4.9 [F95 p. 105];
  "Sometimes it may never even notice a very good correspondence" [F95 p. 90]): the price of
  the local temperature, argued for in §6.1.
- The two open problems of codelet sequencing, which codelets to spawn and their raw urgencies
  [F95 p. 82]: open design questions, decided in D-052, not faults of behaviour.
- The absence of blockage-checking mechanisms: the paper says it would be unreasonable to
  have a mechanism that always looked for blockages, a case of its general principle of no
  special-purpose mechanisms [FH91 p. 179] (the text layer interleaves the two columns, so the
  sentence is paraphrased).
- No learning across runs [F95 p. 41]: a stated scope.
- The sidekicks of Buridan 1 being "much less important than the touched object itself and the
  candidate answers" [F95 p. 130]: a weakness of the problem's design, answered by doubling
  them, not of the program.
- FCCA's "To be sure, we could have added on more mechanisms" [FCCA p. 328] is about the
  brute-force Potelbat, and its "beyond the reach of the current implementation of Copycat"
  [FCCA p. 390] is about Copycat.

RESULTS.md marks, for every result set, which of the seven fixes may have moved it, and reports
the `french-faults` numbers beside the `standard` ones (ANSWERS.md 2).

### 6c. The chapter 4 run

Chapter 4 follows one run on the configuration of Fig. 4.1, which is chapter 5's Surround 5
[F95 p. 96 (img); F95 p. 118 (img)], through nine screen dumps, then shows five Worldviews from
other runs [F95 pp. 95-111]. Every dump is transcribed in `docs/ch4-run.md` and the numbers are
in `tests/data/ch4.json` (the 41 Slipnet boxes, the thermometer, the Coderack bars and the
structures of each dump, with the doubtful readings flagged). This section says what the run
tells the reconstruction about the mechanisms of §3-6, numbered so that DECISIONS.md can cite
each point (item 10). "The series of screen dumps in this chapter was taken from a nonideal
run—that is, a run in which not all of the good structure was found by the program." [F95
p. 95] It is qualitative evidence in the sense of the validation protocol: a direct reading of
a dump may set a default; nothing here is a curve to fit (ANSWERS.md 1).

**The run in numbers** [F95 pp. 96-107 (img)]:

| Fig. | Codelets run | Temperature | Worldview after the dump |
|---|---|---|---|
| 4.1 | 0 | (empty) | empty |
| 4.2 | 2 | 80.0 | empty |
| 4.3 | 12 | (empty) | empty; cup-cup tentative |
| 4.4 | 22 | 59.0 | cup-cup |
| 4.5 | 33 | 43.2 | cup-cup |
| 4.6 | 36 | 31.3 | cup-cup; spoons-spoons entering without a fight |
| 4.7 | 47 | 35.8 | cup-cup, spoons-spoons; cup-glass rival |
| 4.8 | 47 | 13.6 | cup-glass, spoons-spoons |
| 4.9 | 55 | 2.0 | cup-glass, spoons-spoons; answer: the small glass |

**6c.1 The codelet catalogue is printed.** Beside Fig. 4.1 the book prints a legend giving the
full name of each of the 23 labels of the Coderack strip [F95 p. 96 (img)]: find-group,
simple-sameness-detect., find-end-objs., find-middle-objs., find-sim.-nhbr, find-neighbor,
build-obj-0, build-obj-1, build-obj-2, build-obj.-desc., search-for-sam.-gp,
search-for-sim.-gp, pick-salient-obj, pick-salient-subgp., pick-2nd-salient-obj, create-cp,
pick-strong-cp, examine-cp, build-cp-descrip., cp-compete, check-worldview,
clean-up-corresp., find-answer. §5.2's table carries them. Three of §5.2's inferences change:
PK-SALS is pick-salient-subgroup; PK-SALC is pick-strong-cp (the text's
"pick-salient-correspondence" [F95 p. 65]); BLD-CP is build-cp-description. The screen's
"approximately thirty" types [F95 p. 76] are 23 on the strip (→ D-051).

**6c.2 The Slipnet is updated before the first codelet runs.** At 0 codelets 'opposite' is 139,
'cup' 51, 'liq holder' 4, and 'small glass', 'crockery', 'open obj' and 'eat-from-ob' 2; every
other box is at the floor 1 [F95 p. 96 (img)]. "Even though no codelets have yet run, some
activation has already begun to spread from 'cup' to ''liquid holder,' 'crockery,' and so on."
[F95 p. 97] So spreading (and, since 139 and 51 are not round, presumably decay) runs at least
once before the Coderack is loaded; the values set are not shown (→ D-017, D-018).

**6c.3 Where activation goes.** 'cup' climbs 51, 89, 127, 128, 129, 129, 130 and stays just
under the ceiling; 'opposite' holds 137-139 for 22 codelets and reaches 150 at 33; 'right'
reaches 150 at 47; 'horiz', 'left' and 'end' all stand at 132 in the last dumps [F95 pp. 96-107
(img)]. The ceiling of 150 is reached by three nodes, so it is a clamp, not an asymptote
(§3.6). 'group' goes 1, 44, 26, 23, 64, 57, 32: one find-group jolt at the start, decay, then
the jolts of the groups found around Fig. 4.5 [F95 pp. 98-107 (img)] (→ D-019, D-020).
'neighbor' 41 at 12 codelets, 'end' 105 at 22, 'horiz' 55 at 12: the descriptors found jolt
their nodes hard (§3.6, §4.3). The text's qualitative claims all check against the boxes:
'small glass' the most active object type after 'cup' at Fig. 4.3 (17, against fork 6, spoon 5,
knife 5) [F95 p. 101; F95 p. 99 (img)]; 'left' and 'right' active and not 'above' and 'below'
at Fig. 4.4 (30 and 41 against 8 and 7) [F95 p. 101; F95 p. 100 (img)]; 'end' highly active
(105) [F95 p. 101; F95 p. 100 (img)]. 'same' stays 1 until the last dumps (22), 'symmetry' and
'mirror' stay at 1 for 36 codelets (mirror 22 at the end), 'one', 'many', 'food holder' never
leave the floor [F95 pp. 98-107 (img)].

**6c.4 The initial Coderack.** Two codelets in, the strip reads FND-GP 56, SAM 6, FD-END 3,
FD-MID 3, FD-SIMN 26, PK2SALO 7 percent [F95 p. 98 (img)]. The SAM, FD-END, FD-MID and FD-SIMN
bars are the follow-ups find-group posts on success [F95 p. 97], so the starting set is
find-group codelets with a few pick-2nd-salient-obj ones; no PK-SALC bar is visible although
"There are also pick-salient-object and pick-salient-correspondence codelets initially on the
Coderack." [F95 p. 65] (→ D-055). One find-group's follow-ups are about 38 percent of a rack
that also holds 56 percent find-groups: in round numbers, about 20 find-groups and about 14
follow-ups, of which about 9 are find-sim.-nhbr (→ D-053).

**6c.5 "21 percent" is build-object.** At Fig. 4.3 French says the Coderack "shows that 21
percent of the codelets are ones whose job it is to find (and possibly build) new groups of
objects" [F95 p. 101]. The 21 bar stands over BD-OB0, and FND-GP has no bar at all (SAM 15,
FD-END 8, FD-MID 8, FD-NNBR 30, BD-OB0 21, PK2SALO 18) [F95 p. 99 (img)]. So "find (and
possibly build) new groups" is what build-obj-0 does, and the find-group codelets spawned by
'group' [F95 p. 97] had already run. The reconstruction reads build-object as the codelet that
turns neighbours into a bottom-up group (§4.4.2) and keeps find-group for the gestalt scan.

**6c.6 Search-for-same-group is posted in bulk.** When Eliza's spoon-spoon subgroup is built
(Fig. 4.5), "a large number of search-for-same-group codelets were posted to the Coderack
(shown by the tall bar in the middle, with '70' above it)" [F95 p. 101]: 70 percent of the
rack, three codelets after the previous dump [F95 p. 102 (img)]. With a rack of 150 [F95
p. 82] that is of the order of a hundred codelets from one event, the behaviour §5.5 calls
urgency explosion, kept in check by the generation discount (→ D-054). Three codelets later
(Fig. 4.6) the SC-SMGP bar is gone and FND-ANS is 30 percent [F95 p. 103 (img)]: the bulk
posting ran, or was displaced, within three codelets (→ D-054, D-056).

**6c.7 Find-answer appears early and its share swings.** FND-ANS is 24 percent at 22 codelets
(T 59.0), 2 at 33 (T 43.2), 30 at 36, 29 at 47 and 30 at 55 [F95 pp. 100-107 (img)]. The
find-answer codelets are posted well before the run can end, and their share is diluted by
bulk postings and restored as those run. The end "probabilistically" depends on temperature
[F95 p. 105]: at Fig. 4.7, T rising from 31.3 to 35.8 is said to be "slightly reducing the
probability for the program to run a find-answer codelet" [F95 p. 105], so the temperature acts
on find-answer's chance of being chosen, not on its being posted (§6.4, §6.5; → D-056, D-058).

**6c.8 The temperature's path.** 80.0 at two codelets; 59.0, 43.2, 31.3 as the Worldview holds
one unchanging correspondence; 35.8 after the spoon-spoon group correspondence enters; 13.6
when cup-glass replaces cup-cup and the Worldview becomes coherent; 2.0 at the end [F95
pp. 98-107 (img)]. The start is at 80, not 100 (→ D-047). French's accounts of each step are
the four factors of §6.2 in action: no change in the Worldview lowers T [F95 p. 101]; a strong
new correspondence lowers it but the hint of more raises it, and the sum is a slight rise [F95
p. 105]; a change raises it but a much better rating lowers it, and the rating wins because
"the program is quite heavily biased against incoherent Worldviews" [F95 p. 105]. The
thermometer is empty in Figs. 4.1 and 4.3: no value is displayed at 0 and at 12 codelets
(§6.3's windows are six codelets and ten competitions [F95 pp. 90-91]; whether the display
waits for a window to fill is a reading → D-048).

**6c.9 Gestalt groups come first, whole.** The first find-group finds Henry's group of six as
"a bunch of objects that are physically close together" [F95 p. 97], drawn as one box, before
any neighbour inside it is known; Eliza's group appears the same way at Fig. 4.4 [F95 p. 100
(img)]. The isolated cup is never boxed. Subgroups (fork-fork, spoon-spoon) are found inside
the gestalt groups, at their ends, and are drawn only when built or when they end a Worldview
correspondence [F95 p. 101] (§4.4.1, §4.4.4).

**6c.10 Salience picks the first correspondence.** The first correspondence, at 12 codelets, is
cup-cup, because the isolated cup "is one of the most salient objects on the table" by object
type and by being "roughly diagonally opposite" [F95 p. 97]; it enters the Worldview by 22
codelets without a rival (§4.5, §4.6.4).

**6c.11 A non-rival enters without a fight; coherence decides the fight.** The spoon-spoon
group correspondence "is put in the Worldview without a fight" [F95 p. 101] although it makes
the Worldview incoherent; the cup-glass rival then beats cup-cup "because the structure rating
of a coherent Worldview containing a cup-glass correspondence is higher than that of an
incoherent Worldview containing a cup-cup correspondence, even though a cup-cup correspondence
is a priori stronger" [F95 p. 105] (§4.7.2, §4.7.3, §4.7.5; → D-041, D-042).

**6c.12 The dumps are not one per codelet.** Figs. 4.7 and 4.8 both read 47 codelets, with the
same Slipnet and the same Coderack, while the table shows the fight on and then decided and the
thermometer falls from 35.8 to 13.6 [F95 p. 104 (img); F95 p. 106 (img)]; Fig. 4.9, eight
codelets later, still shows Fig. 4.7's Slipnet [F95 p. 107 (img)]. Either a fight and the
Worldview change are completed inside one codelet, with the temperature recomputed at once, or
the dumps were taken at pauses within a codelet and the Slipnet panel was not redrawn. The
reconstruction's trace records the fight, the Worldview change and the temperature as events
inside the codelet that caused them (→ D-059); the identical Slipnet of Fig. 4.9 is treated as a
display artefact, not as evidence that nothing decayed.

**6c.13 What the run leaves undone.** The answer comes with every silverware correspondence
missing: "even though it has missed all the correspondences between the silverware to the left
of Henry's cup and to the right of Eliza's glass" [F95 p. 105]. Figs. 4.10-4.13 show the
structures other runs find (both forks mapped inside the fork-fork correspondence; the flanking
spoons; the typical three-correspondence Worldview; the rare good structure that still picks
the isolated cup) and what none finds: the knife-knife correspondence, which French blames on
the group-level bias (§6b, fault 3) [F95 pp. 108-110]. Fig. 4.14, at about 97.5 degrees, shows
a cup-spoon group mapped to a single spoon, the strange structure high temperature permits
[F95 pp. 110-111] (§6.4).

**6c.14 Drawing conventions**, for the trace and for loop0006's GUI: a gestalt group is a
dashed wavy box; a subgroup a dashed rectangle, captioned "Build new subgroup" when built; a
tentative or competing correspondence a dashed line, a Worldview correspondence a solid one,
group-level ones thicker; a new correspondence flashes its ends; the answer is an arrow with an
E at its tail and the touch an arrow with an H [F95 pp. 98-107 (img); F95 p. 101; F95 p. 105].

**UNSPECIFIED** (→ item 10): the starting set and its urgencies (6c.4; D-055, D-052), the
size of the search-for-same-group posting (6c.6; D-054), when find-answer is posted and how
temperature gates it (6c.7; D-056, D-058), the initial temperature and the empty thermometer
(6c.8; D-047, D-048), and the granularity of dumps against codelets (6c.12; D-059).

## 7. French's numbers and their inconsistencies

Every number French published about Tabletop's behaviour, and about his human subjects, is a
measurement target (ANSWERS.md 1): none is ever used to choose a default. This section says
what he reports, how it was read, where his numbers disagree with each other, and which
version each comparison in RESULTS.md will use. The numbers themselves are data, in
[`tabletop/reported.py`](tabletop/reported.py), with their pages; `tests/test_reported.py`
pins the transcription.

### 7.1 What French reports, and how it was read

For each problem chapter 5 prints the table configuration, a bar graph of "the frequencies of
Eliza's responses" and a table of "the average structure value at the end of the runs and the
average number of codelets run in runs giving the answer in question" [F95 p. 115]. The runs
per problem: "Almost all of the configurations were run fifty times. No configuration was ever
run fewer than forty times." [F95 p. 114]; FCCA says "(Each problem was run a total of 50
times.)" [FCCA p. 392]. The book's wording is the right one: exactly fifty runs give even
percentages, and five of the bar graphs print odd ones (Surround 1's 5/95, Surround 3's 15/85,
Surround 4's 45/55, Surround 10's 73/27, Buridan 12's 75/25) [F95 pp. 115-138 (img)]. A
fifty-run percentage has a standard error of up to seven points, which every comparison must
keep in mind.

The frequencies are printed only in the bar graphs, which are images; each was read from the
number printed above its bar and checked against the bar's pixel height (every bar agrees
with its label within one point of the 100 the two bars sum to) [F95 pp. 115-139 (img)]. The
structure values and codelet counts are in the text layer and were checked on the page images.
Where the running text states a number ("95 percent of the time Eliza opts to touch the cup"
[F95 p. 115]), `reported.py` records it in the entry's `text` field.

**Labels.** The answers are labelled E1, E2, E3 on the drawing and under the bars. In the
Surround family E1 is Eliza's small glass (or the object that replaces it) lower left, directly
opposite the touched cup, and E2 her cup lower right, diagonally opposite; Surround 6 swaps the
cup and the glass, so E1 is the cup in the group and E2 the isolated glass [F95 pp. 115-121
(img)]. In Blockage 1 and 2, E1 is Eliza's glass and E2 the touched cup itself; from Blockage
3 the two glasses lower left share one bar, labelled "E1+E2" under the bar and "El or E2" in
the table, and the touched cup is E3 [F95 pp. 122-129 (img)]. In the Buridan family E1 is
Eliza's spoon lower left and E2 her fork lower right, except Buridan 5 (E2 is her second
spoon, beside the glass pair), Buridan 9 and 13 (E2 is her salt shaker) [F95 pp. 129-139
(img)]. Figure/Ground 1 has "E1 or E2" for Henry's two forks and E3 for his glass [F95
p. 139]. Table 5.3 names its columns "Either small glass" and "Originally touched cup"
[F95 p. 144].

### 7.2 The thirty-five result sets of chapter 5

Frequency in percent; structure and codelets are the averages over the runs ending in that
answer; a dash means the answer never came up. Every bar graph sums to 100. Buridan 1 appears
twice, under the original program and under the modified one with the raised incoherence
penalty; "All the subsequent configurations in this family were run with this modified
version of the program" [F95 p. 130], which FCCA confirms: "the parameter controlling this
degree of dislike was slightly jacked up in all the runs on the Buridan family" [FCCA p. 398].

| Problem | E1 % | E2 % | Structure E1 / E2 | Codelets E1 / E2 | Bar graph | Table | The text says |
|---|---|---|---|---|---|---|---|
| Surround 1 | 5 | 95 | 58 / 89 | 52 / 50 | p. 115 | p. 115 | 95 percent [p. 115] |
| Surround 2 | 10 | 90 | 98 / 83 | 68 / 50 | p. 116 | p. 116 | from 5 to 10 percent [p. 116] |
| Surround 3 | 15 | 85 | 112 / 84 | 89 / 64 | p. 116 | p. 117 | 112 against 84 [p. 117] |
| Surround 4 | 45 | 55 | 186 / 107 | 64 / 56 | p. 117 | p. 117 | 45 percent; 186 against 107 [pp. 117-118] |
| Surround 5 | 86 | 14 | 248 / 146 | 101 / 94 | p. 118 | p. 118 | 86 percent [p. 120]; "about 10 percent longer" [p. 118] |
| Surround 6 | 100 | 0 | 237 / — | 62 / — | p. 119 | p. 119 | |
| Surround 7 | 80 | 20 | 231 / 127 | 109 / 107 | p. 119 | p. 119 | 80 percent [p. 120] |
| Surround 8 | 10 | 90 | 214 / 151 | 204 / 102 | p. 120 | p. 120 | |
| Surround 9 | 2 | 98 | 266 / 150 | 445 / 99 | p. 120 | p. 121 | "only once" [p. 121] |
| Surround 10 | 73 | 27 | 259 / 141 | 88 / 78 | p. 121 | p. 121 | "13 percent less often" than Surround 5 [p. 121] |
| Blockage 1 | 100 | 0 | 66 / — | 67 / — | p. 122 | p. 122 | "always" [p. 122] |
| Blockage 2 | 88 | 12 | 71 / 94 | 106 / 158 | p. 122 | p. 123 | 12 percent; 30 percent higher; 50 percent longer [p. 123] |
| Blockage 3 | 100 | 0 | 78 / — | 101 / — | p. 123 | p. 123 | "never" [p. 124] |
| Blockage 4 | 80 | 20 | 64 / 88 | 125 / 179 | p. 124 | p. 125 | about 40 percent higher, 45 percent longer [p. 125] |
| Blockage 5 | 58 | 42 | 58 / 144 | 95 / 190 | p. 125 | p. 126 | "more than twice as often" [p. 126] |
| Blockage 6 | 22 | 78 | 57 / 177 | 66 / 94 | p. 126 | p. 126 | 78 percent; three times; 40 percent longer [p. 127] |
| Blockage 7 | 24 | 76 | 59 / 196 | 59 / 119 | p. 127 | p. 127 | "virtually no effect" [p. 127] |
| Blockage 8 | 38 | 62 | 73 / 153 | 142 / 124 | p. 127 | p. 128 | from 78 to 62 percent [p. 128] |
| Blockage 9 | 80 | 20 | 81 / 165 | 130 / 124 | p. 128 | p. 128 | 20 percent [p. 128] |
| Blockage 10 | 100 | 0 | 83 / — | 106 / — | p. 129 | p. 129 | "always" [p. 129] |
| Buridan 1 (original) | 72 | 28 | 170 / 201 | 88 / 78 | p. 129 | p. 129 | 72 percent; two and a half times [p. 130] |
| Buridan 1 (modified) | 50 | 50 | 246 / 280 | 86 / 75 | p. 130 | p. 130 | "exactly the same frequency" [p. 131] |
| Buridan 2 | 98 | 2 | 96 / 192 | 62 / 105 | p. 131 | p. 131 | |
| Buridan 3 | 48 | 52 | 95 / 272 | 74 / 77 | p. 132 | p. 132 | |
| Buridan 4 | 52 | 48 | 122 / 279 | 89 / 113 | p. 132 | p. 132 | "almost perfectly balanced" [p. 133] |
| Buridan 5 | 40 | 60 | 138 / 237 | 96 / 83 | p. 133 | p. 133 | 60 percent [p. 134] |
| Buridan 6 | 40 | 60 | 75 / 277 | 77 / 88 | p. 134 | p. 134 | 60/40 [p. 134] |
| Buridan 7 | 8 | 92 | 74 / 285 | 113 / 95 | p. 135 | p. 135 | "about 20 percent faster" [p. 135] |
| Buridan 8 | 44 | 56 | 74 / 283 | 80 / 91 | p. 135 | p. 136 | 44 against 40 percent [p. 136] |
| Buridan 9 | 96 | 4 | 187 / 248 | 98 / 139 | p. 136 | p. 136 | "almost always" [p. 136] |
| Buridan 10 | 100 | 0 | 65 / — | 85 / — | p. 137 | p. 137 | "always" [p. 137] |
| Buridan 11 | 94 | 6 | 57 / 199 | 84 / 93 | p. 137 | p. 137 | |
| Buridan 12 | 75 | 25 | 311 / 264 | 81 / 105 | p. 138 | p. 138 | |
| Buridan 13 | 98 | 2 | 173 / 161 | 98 / 155 | p. 139 | p. 139 | "overwhelmingly" [p. 139] |
| Figure/Ground 1 | 100 (E1 or E2) | 0 (E3) | 92 / — | 57 / — | p. 139 | p. 139 | "never selected Henry's glass" [p. 140] |

All pages are F95's, and the frequencies are (img). For Blockage 3-10 the two columns are
E1+E2 and E3. The bar graph sits at the foot of one page and the table at the head of the next
for seven problems (Surround 3 and 9, Blockage 2, 4, 5 and 8, Buridan 8); `reported.py`
carries the table's page.

### 7.3 Tables 5.1-5.3 and Figure 5.4: subjects against a second set of runs

French "asked subjects (thirty-four students in an introductory experimental psychology class
at Willamette University) to do these problems" [F95 p. 142] and, because pilot studies had
shown the program to be more structurally oriented than they were, re-ran it on twenty-three
problems after one change: "I slightly increased the relative importance that the program
attached to object-type matches compared to relational structure in the evaluation of good
correspondences" [F95 p. 143]. "The entire Buridan series was tested, and five representative
problems were chosen from each of the Surround and the Blockage series" [F95 p. 143]:
Surrounds 1-5 and Blockages 1, 2, 5, 6 and 7. "The figures indicate the percentage of subjects
who chose a particular answer compared to the percentage of times (out of fifty runs) that the
program chose the same answer." [F95 p. 143]. These are the `human` profile's calibration and
test data (ANSWERS.md 7); Tabletop's columns are second runs under a changed weight, not the
chapter 5 sets.

The three tables, read from the page images and the text layer [F95 pp. 143-144]:

| Problem | Subjects E1 | TT E1 | Subjects E2 | TT E2 | Sum (subjects / TT) |
|---|---|---|---|---|---|
| Buridan 1 | 75 | 75 | 25 | 25* | 100 / 100 (+3) |
| Buridan 2 | 66 | 100 | 31 | 0 | 97 / 100 |
| Buridan 3 | 66 | 84 | 34 | 16 | 100 / 100 |
| Buridan 4 | 66 | 72 | 34 | 25* | 100 / 97 (+3) |
| Buridan 5 | 22 | 47 | 78 | 53 | 100 / 100 |
| Buridan 6 | 53 | 59 | 47 | 41 | 100 / 100 |
| Buridan 7 | 62 | 72 | 38 | 28 | 100 / 100 |
| Buridan 8 | 62 | 75 | 38 | 25 | 100 / 100 |
| Buridan 9 | 100 | 100 | 0 | 0 | 100 / 100 |
| Buridan 10 | 97 | 100 | 3 | 0 | 100 / 100 |
| Buridan 11 | 75 | 100 | 25 | 0 | 100 / 100 |
| Buridan 12 | 56 | 84 | 44 | 16 | 100 / 100 |
| Buridan 13 | 97 | 100 | 3 | 0 | 100 / 100 |
| Surround 1 | 0 | 0 | 100 | 100 | 100 / 100 |
| Surround 2 | 0 | 3 | 100 | 97 | 100 / 100 |
| Surround 3 | 16 | 16 | 81 | 84 | 97 / 100 |
| Surround 4 | 22 | 75 | 73 | 25 | 95 / 100 |
| Surround 5 | 33 | 81 | 72 | 19 | 105 / 100 |
| Blockage 1 | 67 | 100 | 33 | 0 | 100 / 100 |
| Blockage 2 | 38 | 47 | 62 | 53 | 100 / 100 |
| Blockage 5 | 16 | 28 | 34 | 47 | 50 / 75 |
| Blockage 6 | 26 | 19 | 72 | 81 | 98 / 100 |
| Blockage 7 | 22 | 28 | 78 | 72 | 100 / 100 |

The footnotes, verified on the page image [F95 pp. 143-144 (img)]: Table 5.1's asterisks sit
on Buridan 1's and Buridan 4's TT E2 cells only, and read "TT also touched the cup close to
E1: 3%" [F95 p. 143]; Table 5.2's footnote "3% touched the touched cup" [F95 p. 143] marks no
cell (the only row three short of 100 is Surround 3's subjects); Table 5.3's footnote "The
other cup on Henry's side of the table" [F95 p. 144] marks no cell either, and can only be
Blockage 5, whose columns sum to 50 and 75: the remainder went to the other cup (Henry's cup
beside his glass, upper right). In Table 5.3 E1 is "Either small glass" and E2 the "Originally
touched cup" [F95 p. 144].

**Figure 5.4** [F95 p. 144 (img)] plots the twenty-three problems: "In this graph the
percentages for the answer most frequently chosen by subjects are compared to how often the
program got the same answer in fifty runs." [F95 p. 143]; "The problems are arranged in
decreasing order of goodness of fit." [F95 p. 143]. The labels, left to right: Bur18, Bur1,
Sur1, Sur3, Bur13, Bur10, Sur2, Bur4, Bur6, Blk7, Blk6, Blk2, Bur7, Bur8, Bur3, Bur11, Blk5,
Bur5, Bur12, Blk1, Bur2, Sur4, Sur5. There is no Buridan 18: "Bur18" is read as Buridan 9,
the one Buridan problem without a bar of its own and the only 100 / 100 row. With that
reading, and Blockage 5's "most frequent answer" taken as the other cup (50 against 25), the
gaps run 0, 0, 0, 3, 3, 3, 3, 6, 6, 6, 9, 9, 10, 13, 18, 25, 25, 25, 28, 33, 34, 48, 53:
never decreasing, as the caption says (`tests/test_reported.py`). The bars read from the
plot agree with the tables (Sur1's Tabletop bar looks a shade under 100; the table says 100).
French's summary: "on over half of these problems, Tabletop's answers differ from those of
subjects by no more than 10 percentage points" [F95 p. 144] (thirteen of twenty-three), and
the program does worst where the touched object is in a group and an isolated object of its
type lies elsewhere: "This problem is particularly evident in Tabletop's performance on
Surrounds 4 and 5." [F95 p. 144].

### 7.4 The other human data

- Blockage 1: "a significant percentage of them (33 percent) touched the touched cup instead
  of the glass on the opposite side of the table" [F95 p. 122]; Table 5.3 has the same 33.
- Surround 5: in an initial informal survey subjects often drew the group correspondences
  and yet "chose to touch the isolated cup in more than half of the cases" [F95 p. 118]; in
  the chapter 5 experiment over two-thirds touched the cup [F95 p. 118] (Table 5.2: 72).
  FCCA tells the survey the same way, without a number [FCCA p. 395].
- The mirror-symmetry survey of chapter 3: without the cups, "object-type pressure is favored
  over size pressure (62 percent vs. 38 percent)" [F95 p. 86]; "With the cups present, 56
  percent of subjects opted for the" object-type mapping [F95 p. 86]. A "small, preliminary
  survey", with no subject count; its configuration is Figure 3.20's [F95 p. 86], which is
  not one of chapter 5's problems.
- FCCA on Blockage 2: "Many human subjects (40 percent; see French, 1992) saw the glasses as
  counterparts." [FCCA p. 396]: seeing the glasses as counterparts is touching Henry's cup,
  for which Table 5.3 gives 62.
- FCCA on Blockage 6: "Human subjects chose Henry's cup 66 percent of the time" [FCCA
  p. 397], citing the 1991 paper; Table 5.3 gives 72.
- The 1991 paper: "Thirty-one human subjects were given these three problems" [FH91 p. 181]
  (its Figure 2: a lone glass; a glass added upper right; two glasses lower left and two
  upper right, that is Blockage 1, 2 and 6). The sentence that follows is garbled as printed
  but gives the numbers: twice as many touched the touched cup in the third problem as in the
  first, 66 percent against 33 [FH91 p. 181]. The 33 is Blockage 1's; the 66 is FCCA's
  figure for Blockage 6, not the book's 72.

### 7.5 FCCA's reprint

FCCA's chapter 9 reprints eighteen of the problems as Figures IX-1a to IX-3f [FCCA
pp. 394-399 (img)], each with the drawing, the bar graph and a table of "Average
structure-value" and "Average run-length (in codelets)", read here from the page images.
They are Surround 1, 2, 4, 5, 8, 10 (IX-1a-f), Blockage 1, 2, 4, 5, 6, 9 (IX-2a-f) and
Buridan 10, 11, 1 (modified), 4, 5, 6 (IX-3a-f). Sixteen carry the book's numbers exactly.
Two do not:

| FCCA | Problem | FCCA's numbers | The book's |
|---|---|---|---|
| IX-1d [FCCA p. 395 (img)] | Surround 5 | 86 / 14; structure 253 / 154; codelets 107 / 85 | 86 / 14; 248 / 146; 101 / 94 [F95 p. 118] |
| IX-1f [FCCA p. 395 (img)] | Surround 10 | 82 / 18; structure 242 / 148; codelets 91 / 127 | 73 / 27; 259 / 141; 88 / 78 [F95 p. 121] |

FCCA's caption for IX-1f also says six objects were added, "more than doubling the number of
theoretically possible correspondences between objects" [FCCA p. 395], where the book's
drawing shows five (salt and pepper shakers mid-table; a fork, a knife and a glass mid-right)
[F95 p. 121 (img)]. FCCA's redrawn figure, read at 300 dpi while digitizing the layouts
(item 07; `docs/problems.md` 6), shows six: the same five and a cup beside the glass at
mid-right [FCCA p. 395 (img)], as its caption says. So IX-1f is a different configuration
from the book's Surround 10, with a second cup among the junk, and its numbers are that
layout's; IX-1d looks like a different set of fifty runs of the same configuration (its
frequencies are the book's while all four averages move). Neither is a transcription slip.
FCCA's own text keeps the book's reading of the junk ("there is hardly any difference"
between IX-1f and IX-1d [FCCA p. 395]), which its 82 against 86 fits better than the book's
73. The digitized `surround-10` is the book's five-object layout (D-003).

### 7.6 The inconsistencies, and which number each comparison uses

Every version is kept and shown (ANSWERS.md 1). The rule for RESULTS.md: **the book is the
primary target** for every set; where FCCA or the paper differs, its number is printed
beside the book's as a second target, never averaged with it.

1. **Surround 5 and 10** (7.5): the book's sets are primary; FCCA's IX-1d and IX-1f are
   second targets, IX-1f with the note that its layout has a sixth junk object, a cup, which
   the digitized problem (the book's drawing) does not. For Surround 10's "13 percent less
   often" [F95 p. 121], the book's 73.
2. **Blockage 2's subjects**: Table 5.3's 62 for the touched cup is primary; FCCA's 40
   [FCCA p. 396] is shown beside it. They may be different samples (FCCA cites French 1992,
   the dissertation; the book's tables are the Willamette class).
3. **Blockage 6's subjects**: Table 5.3's 72 is primary; FCCA's 66 [FCCA p. 397] and the
   1991 paper's 66 [FH91 p. 181] (thirty-one subjects against the book's thirty-four) are
   shown beside it.
4. **Columns that do not sum to 100** (7.3): Surround 5's subjects (105), Surround 4's (95),
   Surround 3's and Buridan 2's (97), Blockage 6's (98), Buridan 4's TT (97 plus the
   footnote's 3), Buridan 1's TT (100 plus the footnote's 3), Blockage 5's (50 and 75, the
   rest to the other cup). One subject of thirty-four is 2.9 points, so 97-98 can be
   rounding or a non-response; 95 and 105 cannot. The percentages are compared as printed;
   the `human` fit uses the two printed columns and never renormalizes them.
5. **Footnotes that mark no cell** (7.3): read as above (Surround 3; Blockage 5), with the
   reading recorded in `reported.py`'s `footnote` fields.
6. **"Bur18"** in Figure 5.4: read as Buridan 9 (7.3).
7. **Runs per problem**: "approximately fifty runs of the program per problem" [F95 p. 114]
   against FCCA's "total of 50 times" [FCCA p. 392]; the odd percentages settle it (7.1).
   The benches run exactly fifty seeds per set, and more for the slow surveys.
8. **The labels** change meaning inside the Blockage family (7.1): RESULTS.md prints what
   each label names, never a bare "E2".
9. **Two different Tabletops in the human tables**: Tables 5.1-5.3's TT columns were run with
   a raised object-type weight [F95 p. 143], so they differ from chapter 5's sets for the
   same problems (Buridan 4: 52 / 48 in chapter 5, 72 / 25 in Table 5.1) without being
   inconsistent. The `standard` profile is compared with chapter 5; the `human` profile
   with the tables; the `buridan` profile with Buridan 2-13 of chapter 5 and Buridan 1
   (modified), which is its calibration point, not a match.
10. **French's ratios** are approximate and all follow from his tables within a few points
    (Surround 5's "about 10 percent longer" is 7; Blockage 2's "30 percent higher" is 32 and
    "50 percent longer" 49; Blockage 4's 40 and 45 are 38 and 43; Blockage 6's "three
    times" is 3.1 and "40 percent longer" 42; Buridan 1's "two and a half times" is 2.6;
    Buridan 7's "20 percent faster" is 16); `tests/test_reported.py` checks each with a
    tolerance.

### 7.7 What else the numbers show

"Over the course of these several thousand runs, certain trends were observed" [F95 p. 142].
Each is derived from `reported.py` by a function, tested, and is a validation target of
RESULTS.md beside the per-problem numbers.

- **Better structure takes longer.** "Answers involving better structure generally take more
  time before they are selected." [F95 p. 142]. In the twenty-nine sets where both answers
  came up, the better-structured answer took more codelets in twenty. The nine exceptions,
  where it came faster: Surround 1, Blockage 8 and 9, Buridan 1 under both settings, Buridan
  5, 7, 12 and 13. French remarks only on Buridan 7, where the fork answer is "about 20
  percent faster than the spoon" [F95 p. 135] because its mappings are so salient.
- **Frequency against structure.** "Of particular interest are cases where the highest-
  frequency answer does not coincide with the answer having the highest final
  structure-value." [FCCA p. 392]. French names Surrounds 3, 4, 8, 9 and Blockages 2, 5:
  "Sometimes it eschews the paths to an obvious answer in favor of a structurally better
  answer (e.g., Surrounds 3, 4, 8, and 9; Blockages 2 and 5)." [F95 p. 142], and Buridans 9
  and 11: "This type of discrepancy between frequency and structure value crops up in some
  problems that follow" [F95 p. 131]. The tables give fourteen such sets: those eight plus
  Surround 2, Blockage 4 and 9, Buridan 1 (original), 2 and 4. Buridan 1 (modified), a tie
  at 50 / 50 whose better structure is the fork's, is left out of the count.
- **Focus.** Adding junk to Surround 5 (Surround 10) moved the glass's frequency by 13 points
  and the run length hardly at all: "the program took approximately the same amount of time
  to decide on an answer here as it did in Surround 5" [F95 p. 121] (88 against 101 codelets
  for the glass, 78 against 94 for the cup).
- **Throughput.** Fifty runs "usually took about three hours on a Sun IV workstation" [F95
  p. 114]; about 100 codelets a run. Not a target.
- **Robustness.** Thousands of runs on forty to fifty problems and virtually no weird answer
  [F95 p. 111]; RESULTS.md's M8 counts answers outside the labelled ones.

**UNSPECIFIED**: nothing in this section needs a decision; it is data. What the comparisons
will need from later items: the layouts of the thirty-five problems (item 07, D-003) and the
`human` and `buridan` fits (item 26, ANSWERS.md 7).

## Beyond French: roles (re-interpretation)

Everything above is French's program as the sources specify it, and nothing in this file
changed for the roles. Since the roles loop (2026-10-14; D-078 on) the engine also reads the
**role** each item plays in the analogy, the loner, the sidekick, the anchor, the mirror
image, the counterpart, which French names in his prose [F95 p. 118; F95 p. 130; F95 p. 140]
and his program never represents. That is a **re-interpretation** of Tabletop, not part of
its specification: where French describes a mechanism the reconstruction had left out (the
table-frame placement slippage of p. 85, D-082) the decision is spec-inferred and names the
omission; everywhere else the roles go beyond the sources and say so. The design is
[`docs/roles.md`](docs/roles.md) (the three readings, the five families and the registry of
23, the tag as a competing structure, the 15 Slipnet nodes, the six role codelets, the
re-cast, the eight consumers, the honesty measures); the decisions are
[`DECISIONS.md`](DECISIONS.md) D-078 to D-106, each of Kind re-interpretation or
spec-inferred with a knob; the numbers are [`RESULTS.md`](RESULTS.md) §11, French | v1 | roles,
with every change of runs since v1 (commit `e38d6e0`) in its re-interpretation log; the
predictions registered before any role was built are
[`docs/predictions.md`](docs/predictions.md). The sections above keep their numbering and
their decision lists (D-001 to D-077); the roles' revisions of those decisions are the
"As revised" paragraphs of DECISIONS.md, and the module table names the modules the roles
reach.
