# Decisions: where the sources are silent

Every place where French's book (and FCCA chapters 8-9, the 1991 paper) does not dictate what
the program does, and the choice this reconstruction makes. Code that implements a decision
says `DECISION D-NNN`; `tests/test_conventions.py` checks that each has an entry here. All
knobs live in `tabletop.config.Config`, each field commented with its tag or decision. How an
entry is written, what the numbers from D-078 (roles), D-107 (the fast engine), D-114 (the
optimized defaults, v3) and D-116 (the structural Blockage fix, v5) on mean, the seven fixes,
the two calibrations, the law of temperature
("under D-060") and the index by number and by kind are in [`DECISIONS-INDEX.md`](DECISIONS-INDEX.md); the
entries stand in subject order.

## Index

See [`DECISIONS-INDEX.md`](DECISIONS-INDEX.md).

### D-001: The table and its units: 110 x 100
- **Kind:** digitization (spec-inferred for the shape).
- **Sources:** "Tabletop is a continuous two-dimensional world" [F95 p. 42]; the objects "can
  be placed anywhere on the table" [F95 p. 55]; the program receives "the coordinates and
  orientation of each placed object" [FCCA p. 379]. No page gives the table's dimensions, a
  unit or an origin (chapters 2-5 and FCCA chs. 8-9 searched; SPEC.md 1.3). The table is
  "divided into nine regions like a tic-tac-toe board" [F95 p. 65]. The only evidence of its
  shape is the drawings: the frame of every chapter 5 figure measures 191-195 x 175-176 pixels
  inside its lines, 1.10 times wider than tall, in all ten Surround figures [F95 pp. 115-121
  (img)]; the screen's own frame in the chapter 4 dumps is 306 x 268 pixels at 150 dpi, 1.14
  [F95 p. 98 (img)]. The palette and the figures draw the icons at one scale (a fork is 6 x 17
  pixels in both) [F95 p. 26 (img); F95 p. 118 (img)].
- **Choice:** the table is a rectangle of 110 x 100 units (`geometry.TABLE_WIDTH`,
  `TABLE_HEIGHT`), the origin at Henry's left corner (the upper left of every figure), x to the
  right, y downwards towards Eliza. A digitized position is the centre of the icon's pixel box
  as a fraction of the frame's inner width and height, times 110 and 100; in the chapter 5
  figures one pixel is 0.567 units along both axes, so distances are isotropic and the
  footprints (D-002) keep their drawn shape. Every problem file repeats the size and the
  loader checks it.
- **Reason:** the drawings are the only evidence and all ten agree on 1.10; a uniform scale
  keeps the drawn geometry (gaps between neighbours, the icons' shapes) exactly, which is what
  the distance-sensitive problems (Blockage 8-10) need; a height of 100 units makes positions
  read like percentages of the way from Henry to Eliza.
- **Knob:** none.
- **Alternatives:** a 100 x 100 square with each axis normalized separately (the plan's
  proposal; it stretches vertical distances by 10 percent relative to horizontal ones); the
  screen's 1.14; units of a small glass's width (4.5 units here).

### D-002: Object footprints: the icons' boxes
- **Kind:** digitization.
- **Sources:** "All objects have a size; they are not simply point objects" [F95 p. 55]; "the
  sizes of objects" are a pressure [FCCA p. 378]; a cup "is shapeless, sizeless, and
  decoration-free" [FCCA p. 326], read as no variation within a type (SPEC.md 1.2). No page
  gives a size for any type. The palette draws every type, and the silverware in four
  orientations [F95 p. 26 (img)]; the chapter 5 figures draw nine of the eleven types [F95
  pp. 115-121 (img)].
- **Choice:** one axis-aligned box per type, pointing up, in pixels of the figures
  (`objects.FOOTPRINTS_PX`), converted to table units by D-001's 0.567 units per pixel
  (`objects.footprint`): fork 6 x 17, knife 3 x 17, spoon 6 x 17, cup 14 x 7, saucer 14 x 13,
  plate 19 x 19, small glass 8 x 10, pepper shaker 12 x 15, salt shaker 12 x 15 (measured on
  the Surround figures); big glass 11 x 15 and soup bowl 21 x 12 (measured on the palette,
  whose icons are at the figures' scale). Silverware pointing left or right swaps the box's
  sides. Size enters the engine through the footprint box (`objects.box`, the footprint
  centred on the object's position) and the white space between two boxes
  (`objects.edge_gap`, `geometry.gap_between`), which is the gap the proximity criterion
  measures (D-022) and `objects.gap_in_glass_widths` expresses in French's unit; an
  object's region is decided by its centre alone (D-004).
- **As revised (loop0008, item 04):** `objects.footprint` is worked out once per (type,
  orientation) and remembered (`lru_cache`; a few dozen pairs): the engine asked it on
  every box, 390 thousand times in the timed workload, 7.6 percent of a run on the
  compiled engine; the same tuples, every run unchanged (D-112).
- **Reason:** the icons are the only evidence of size, and the figures show them at the scale
  the positions were measured at; the palette's plate (21 x 17) is clipped by its cell, so the
  figure's 19 x 19 is used; the palette's cup (15 x 8), saucer (14 x 14), glass (8 x 11) and
  shakers (14 x 17) are within two pixels of the figures' and the figures' values are kept
  for consistency.
- **Knob:** none.
- **Alternatives:** one radius per type (loses the silverware's elongation); the palette's
  sizes throughout; point objects (which French rules out).

### D-003: Digitizing the layouts: method, format, ids, uncertainty
- **Kind:** digitization (ANSWERS.md 4).
- **Sources:** the configurations survive only as drawings: in chapter 5 one figure per result
  set, an embedded JPEG of 487-496 x 200-207 pixels at 135 ppi, with the table's frame at the
  left, "Henry" above it, "Eliza" below it, the touched object "designated by an arrow marked
  H" and Eliza's answers "labeled E1, E2, and so on" [F95 p. 114; F95 p. 115 (img)]. The
  input French's program took is "the coordinates and orientation of each placed object, along
  with its category membership" and "an indication of which object was touched by Henry"
  [FCCA p. 379]; configurations were saved and loaded as files from the screen's buttons [F95
  p. 96 (img)], in a format the book does not give. FCCA redraws six of the Surround layouts
  [FCCA pp. 394-395 (img)]; five agree with the book's figures and one does not (its Surround
  10 has a sixth junk object, a cup at mid-right, as its text says [FCCA p. 395]); the book's
  figure is followed (ANSWERS.md 4; `docs/problems.md` 6). FCCA's six Blockage and six
  Buridan layouts [FCCA pp. 396-399 (img)] agree with the book's figures object for object,
  except that its Blockage 9 draws the left upper-right glass 8 units further left (the
  same gap in glass widths, FCCA's icons being larger relative to its square table); the
  book's figure is followed there too. Chapter 2's nineteen configurations, Figures 2.2-2.20
  [F95 pp. 27-35 (img)], are drawings of the same kind with no bar graph or E arrows, each
  with French's expected answer in words and no statistics [F95 pp. 26-35].
- **Choice:** one JSON file per configuration in `problems/`, named by its stem
  (`surround-05`): `name`, `title`, `family`, `source` (tag, printed page, figure, the PDF page
  and index of the extracted image), `table` (110 x 100 and the frame's pixel box), `touched`,
  `answers` (label to id), `objects`, `method`, `uncertainty` (`units`, `note`), `notes`,
  `review`, and `based_on` for a variant of another problem. Each object has `id`, `type`,
  `x`, `y`, `orientation` (silverware only), the pixel box it was read from (`box_px`) and its
  provenance (`figure`, `page`, `image`). `tabletop.problems.validate` checks all of it and
  recomputes each position from its box and the frame. The method (`docs/problems.md`):
  extract the figure with `pdfimages -png`; find the frame as the darkest column and row
  profiles of the image's left part (given by hand, `--frame`, for the two figures whose
  lines are too faint for the profile); take the clusters of pixels darker than grey level 200,
  merged within 2 pixels, as the icons; where an arrow touches an icon, read the icon's own
  outline on an ASCII dump of the region, leaving the arrow out; the position is the centre
  of the icon's box. Ids are the type's stem and a number (`small-glass1`), in reading
  order, Henry's row first, then Eliza's, then the loners; a variant French describes as
  another problem changed keeps that problem's ids and numbers its additions after them, so
  that the same object has the same id across a family. Every file is rendered beside its
  figure (`tools/render_problem.py`) and reviewed by eye; the review is recorded in the file
  and in `docs/problems.md`. A chapter 2 file (family `ch2`, `ch2-fig11`) has an empty
  `answers` and an `expected` block: `answer` (the id French names as the most likely
  answer, or null where he states only a direction), `plausible`, `unlikely`, `quotes` (his
  words with their pages, machine-checked) and `comparisons` (`toward` an id, `more_than` or
  `less_than` an earlier figure), so that the qualitative measure M2 can be run as a bench
  (`docs/problems.md` 9).
- **As revised (loop0008, item 04):** `Problem.object` reads an index by id built on the
  first ask (`cached_property`, as `ids` since D-108; the first object of an id, as the
  scan found it): the objects never change after loading, and the engine asked for one
  760 thousand times in the timed workload (D-112).
- **Reason:** the centre of the icon's own box is reproducible (two readers of the same image
  get the same box to a pixel) and is not pulled by thin handles or by the arrows, which the
  centroid of the ink is; keeping the box in the file makes every coordinate checkable
  without the image; shared ids let the tests say exactly how a variant differs from its base.
- **Uncertainty:** one pixel is 0.57 units; boxes are good to about one pixel, and the objects
  an arrow overlaps to about two. Surround 5 measured on its chapter 5 figure agrees with the
  chapter 4 screen dump of the same configuration, an independent image at another scale
  (`tests/data/ch4.json`), to within 1.6 units in x and 2.2 in y over all thirteen objects;
  the files therefore state 2 units. Buridan 1, printed twice [F95 p. 129 (img); F95 p. 130
  (img)], was measured on both figures and agrees with itself to within 1.5 units (the fork
  under the E2 arrow; the rest within 0.7). **The distance-sensitive problems, Blockage 8-10:**
  French pulls the two upper-right glasses of Blockage 6 apart by "one glass-width" and then
  "each time by the width of one glass more" [F95 p. 128]. The gap between the two glass boxes
  is 5, 10, 14 and 21 px in Blockage 6, 8, 9 and 10 (2.8, 5.7, 7.9 and 11.9 units): 0.6, 1.3,
  1.8 and 2.6 glass widths of 8 px (D-002), where French's words give one, two and three. Each
  glass edge is good to a pixel, so a gap is good to about 2 px, 1.1 units, a quarter of a
  glass width; the three measured gaps lie within the files' stated uncertainty of 2 units
  (0.44 widths) of French's whole numbers, in the right order, but the drawing's increments
  are 4 and 7 px, not 8 and 8, so Blockage 8's gap is nearer one and a quarter widths than
  one. The files record the measured gap (`gap`: pixels, units, glass widths, French's
  number); the engine sees the drawn distances, not French's round ones; whether a group
  forms across the gap is D-023's business (`docs/problems.md` 8).
- **Knob:** none.
- **Alternatives:** the centroid of the ink (pulled by handles and arrows); coordinates placed
  by hand on the drawing (not reproducible); a square table with per-axis fractions (D-001).

### D-004: The nine blurry regions: a draw each time
- **Kind:** simplest.
- **Sources:** "The table is divided into nine regions like a tic-tac-toe board, although the
  boundaries between regions are intentionally blurry. In other words, an object that is
  actually in one region but very close to a region boundary may sometimes be considered by
  the program to be in the adjacent region." [F95 p. 65] Nothing says how close is "very
  close", how often is "sometimes", or whether the program decides an object's region once or
  every time it looks (SPEC.md 1.3, 4.2).
- **Choice:** the table (D-001) is cut into three equal columns and three equal rows, by the
  object's centre (`geometry.exact_region`; a centre on a line belongs to the far third).
  Each time a codelet asks for an object's region, the region is drawn
  (`geometry.drawn_region`): along each axis in turn (x, then y), the nearer of the two
  lines bounding the object's third is found; when it lies within `region_blur_width`
  units (4.0, about one glass width) of the centre, one coin puts the object across it
  with probability `region_blur_max` x exp(-d / width), d being the distance to that
  line, so an object on a line crosses it half the time and one a glass width away about
  a fifth of the time; both coins may pass, which puts the object in the diagonal
  neighbour. At most two coins per draw, none for an object further than the width from
  every line (the thirds are wider than twice the width, so only one line per axis can be
  within it). The draw goes through the run's generator; it is not softened by
  temperature (the blur is perceptual, not a decision). The regions are named by row and
  column (`upper-left`, `upper-middle`, ..., `middle` for the centre, ..., `lower-right`),
  which is what the trace and the events print.
- **Reason:** a fresh draw each time is the simplest reading of "sometimes", and keeps the
  nine regions stateless; an exponential fall-off is the simplest curve that is largest on
  the boundary and vanishes smoothly, with one width parameter.
- **Knob:** `Config.region_blur_width` (4.0 units), `Config.region_blur_max` (0.5).
- **Alternatives:** a fixed band (an object within w of a boundary is in both regions); a
  region decided once per run; the footprint's overlap with each region as the probability.

### D-005: No sides: Henry's and Eliza's halves are regions only
- **Kind:** spec-inferred.
- **Sources:** "There is no pre-set boundary line dividing this arrangement into two disjoint
  'situations'" [FCCA p. 388]; in Tabletop "any particular object or group of objects can, at
  least theoretically, map to any other object or group of objects" [F95 p. 43]; the line
  between the two people is a membrane that "remains partially permeable" [FCCA p. 389]. Yet
  "ownership" is a pressure [FCCA p. 378], and chapter 5's answers include the touched object
  itself (Blockage 1-2's E2, Blockage 3-10's E3) (SPEC.md 1.3).
- **Choice:** correspondences are allowed between any two items, on the same side or across;
  a correspondence from the touched object to itself is not built (the literal answer is the
  default of D-063, not a structure). Henry's and Eliza's halves exist only as the regions of
  D-004 and through the region preference (D-032), which is what makes the far side
  attractive: `geometry.half` names the upper row of regions Henry's and the lower row
  Eliza's, with the middle row nobody's, for the trace and the GUI only; no codelet uses
  it to allow or forbid anything.
- **Reason:** French's and FCCA's words; the region preference already carries the pressure
  to look across, and a hard side rule would make the literal answer unreachable through
  structure in exactly the problems (Blockage) where French's program reaches it.
- **Knob:** none.
- **Alternatives:** Copycat's two situations, with mappings only across the middle line.

### D-006: Orientation is input, not perception
- **Kind:** simplest.
- **Sources:** "The forks, knives, and spoons have four possible orientations." [F95 p. 55]
  Orientation is part of the input [FCCA p. 379] and among the pressures [FCCA p. 378], but
  no page says how a difference of orientation between two pieces of silverware affects a
  correspondence or a group, and no chapter 5 problem varies it; groups have an orientation
  of their own, horizontal or vertical [F95 p. 69; F95 p. 97] (SPEC.md 1.2).
- **Choice:** an object's orientation is read from the problem file, kept on the object,
  used for its footprint (D-002, the box's sides swap for left and right) and drawn, but no
  codelet notices it and no slippage uses it. A group's orientation is horizontal or vertical
  from its members' positions (D-028).
- **Reason:** the simplest reading of a pressure the sources list but never describe in
  action; it can be added later as a descriptor without changing the architecture.
- **Knob:** none.
- **Alternatives:** an orientation descriptor on silverware feeding an orientation slippage
  between objects (the ninth static factor of D-036 is for groups only [F95 p. 69]).

## Slipnet (D-007 to D-021, D-070)

### D-007: The node set: the 47 of the list, plus "isolated"
- **Kind:** spec-inferred.
- **Sources:** p. 62 says forty-six nodes and lists forty-seven; FCCA says 47 and lists the
  same 47 [FCCA p. 381]; "isolated" is used as a node once, in the static factors [F95 p. 68],
  and is in neither list; the screen's grid shows 41 boxes, lacking exactly the six object
  types absent from the chapter 4 configuration (SPEC.md 3.1).
- **Choice:** the Slipnet has 48 nodes: the 47 of p. 62's list, under French's names
  ("sameness", "used with", "eat-from object"), and "isolated", an added node that is jolted
  when a find-group codelet finds that an object has no neighbour near enough to group with
  (D-022) and favours object-to-object correspondences in the static evaluation (D-036). The
  screen's labels are kept as each node's short name for the trace and the GUI. The grid's
  41 cells are a display subset; the engine never drops a node.
  As built (`groups.find_group_around`): the jolt to "isolated" is a descriptor's,
  `jolt_description` (D-016), sent once per object, and the object is described as
  `isolated` in the Workspace.
- **Reason:** two lists of 47 outweigh one count of 46; "isolated" is needed by a mechanism
  French describes, so it must exist, and it is marked as the one node he never lists.
- **Knob:** none.
- **Alternatives:** deriving "isolated" from the absence of a group around an object (no
  node); dropping it, and with it the second half of the static factor of p. 68.

- **As revised (loop0007, item 03):** the node set gains the 15 role nodes of D-080 (63 in
  all), appended after "isolated" and none on French's screen; the 47 of the list and
  "isolated" are unchanged.

### D-008: Depth: three levels, 20, 50, 80
- **Kind:** spec-inferred (the levels), Copycat choice (the values).
- **Sources:** depth is "a real number" [F95 p. 55] with "three levels of abstractness" [F95
  pp. 61-62]; level 1 is the object types, level 2 their categories and the relations between
  objects (used with, bigger than, neighbor, left), level 3 relations between relations and
  superordinates of level 2 (opposite, direction) [F95 p. 62]; no number is given for any level
  (SPEC.md 3.2). Depth sets the decay rate and nothing else [F95 p. 56; F95 p. 62].
- **Choice:** depths 20, 50 and 80 for levels 1, 2 and 3 (`depth_by_level`), on Copycat's
  0-100 scale. The level of each node, data in `slipnet_def.py`: level 1, the eleven object
  types; level 2, the ten categories, "used with", "bigger than", "smaller than", "similar
  shape to", "neighbor", "left", "right", "above", "below", "end", "middle", "horizontal",
  "vertical", "group", "isolated", "one", "two", "three", "many"; level 3, "sameness",
  "opposite", "symmetry", "mirror symmetry", "diagonal symmetry", "direction", "position",
  "number". The assignment follows p. 62's rules: a node is level 3 if it is a relation
  between level-2 relations (opposite, the symmetries, sameness as identity between
  descriptors) or a superordinate of level-2 nodes (direction over left and right, position
  over end and middle, number over the counts).
- **Reason:** three evenly spaced values are the simplest reading of three levels; Copycat's
  depths run over the same range, and French calls the two architectures the same project
  [F95 p. 55]. The lesion "flat depths" [FCCA p. 365] sets all three equal.
- **Knob:** `Config.depth_by_level` ((20, 50, 80)).
- **Alternatives:** (10, 50, 90); per-node depths copied from Copycat's for the nodes the
  two share (left, right, opposite, sameness); "group" at level 3.

### D-009: The link table
- **Kind:** spec-inferred.
- **Sources:** the links the sources name are tabulated in SPEC.md 3.3 (soup bowl, knife,
  cup and the glasses' ISA links; knife-fork and cup-saucer "used with"; saucer-plate and
  small-big glass "smaller than"; left-right under "opposite" and "diagonal symmetry"; left
  ISA direction; the number nodes; FCCA's spoon-knife sharing only silverware [FCCA p. 389]),
  with ten qualitative distance claims of chapter 5 (knife-fork short; cup-small glass very
  close; cup-saucer almost as close; cup-plate far but weakly related; cup-saltshaker
  completely dissimilar; spoon-glass very far; fork-spoon relatively close; spoon-saltshaker
  very distant; knife-saltshaker a very long path). No page gives the table.
- **Choice:** a data table in `slipnet_def.py`, one row per link with the page that supports
  it or the mark `domain judgement`. ISA links: every object type to its categories (cup:
  liquid holder, crockery, open object, eat-from object, receptacle; saucer: crockery,
  receptacle, open object; plate: crockery, food holder, eat-from object, open object,
  receptacle; soup bowl: liquid holder, food holder, crockery, eat-from object, open object,
  receptacle; small glass and big glass: liquid holder, open object, receptacle; knife:
  silverware, sharp object, buccal object; fork: silverware, sharp object, buccal object,
  food holder; spoon: silverware, buccal object, food holder, liquid holder; salt shaker and
  pepper shaker: closed object, receptacle). Has-member links are the inverses. Labeled
  links: knife-fork and cup-saucer (used with); saucer-plate, small glass-big glass, cup-big
  glass (smaller than, with bigger than the other way); salt-pepper shaker, cup-small glass,
  fork-spoon (similar shape to); left-right and above-below (opposite, diagonal symmetry);
  left-left, right-right, above-above, below-below as the identity slippages that feed
  mirror symmetry (D-030); one-two, two-three, three-many (number); end-middle (position).
  ISA links from the position and count descriptors to their superordinates: left, right,
  above, below ISA direction; end, middle ISA position; one, two, three, many ISA number;
  mirror and diagonal symmetry ISA symmetry. The table carries D-009's ten orderings as
  tests (item 12). **As built (item 12), five rows follow the chapter 4 screen dumps, the
  one direct evidence of which concepts French's net joined [F95 pp. 96-107 (img)]:** (1)
  "open object" and "closed object" are joined by a link labeled "opposite", because
  "closed obj" rises with "open obj" (2, 13, 18, 24, 32, 42 against 4, 20, 27, 35, 45, 58)
  while "opposite" is at 137-150 and no closed object is on the table; (2) there is no
  horizontal-vertical link, because "vert" stays at 2-7 while "horiz" is at 132 and
  "opposite" at 150; (3) fork and spoon are not food holders, because "food holder" never
  leaves the floor while "fork" is at 36 and "spoon" at 53 (FCCA's sharing "that they can
  hold food" [FCCA p. 389] is read as "buccal object", which both are); (4) the cup and the
  glasses are not receptacles, because "receptacle" stays at 1-6 through a run in which
  "cup" is at 51-130 and "small glass" at 53; (5) the cup-small glass link is a size link
  ("smaller than" and "bigger than"), not a "similar shape to" one, because "big. than" and
  "sm. than" rise together from the twelfth codelet (4-13 and 3-15) while "sim shape" stays
  at 1 until the cup-glass correspondence is built. `slipnet_def.LINKS_LEFT_OUT` records
  the rows left out with their reasons.
- **Reason:** the named links are kept exactly; the rest are the domain judgements the
  categories' names make obvious (a knife is a sharp object, a spoon holds liquid), each
  marked so that a reader can disagree with one row without touching the mechanism. The
  spread seen before any codelet ran (cup to liquid holder 4, crockery, open object, eat-from
  object and small glass 2 [F95 p. 96 (img)]) is the one piece of direct evidence of the
  cup's neighbourhood and the ISA rows for the cup follow it.
- **Knob:** none (the table is data; D-010 gives the lengths).
- **Alternatives:** fewer categories per object (Copycat-like sparseness); FCCA's reading
  that forks and spoons share "food holder" only, without "similar shape to"; the five rows
  above as the plan had them (a cup ISA receptacle, and so on), which the dumps argue
  against but do not exclude (the screen prints integers, and a box at 1 may hold 1.4).
  As built (item 32): the queries `links_of`, `categories_of` and `labels_between` take
  the link table as an argument, and `Slipnet` answers them from its own table
  (`Slipnet.links_of`, `categories_of`, `labels_between`), so that the slippages of D-030
  are read from the Slipnet the run holds, not from the module's constant; with the default
  table the two are the same. `groups.grouping_categories` still reads the definition
  table's intrinsic ISA lengths, which no label shrinks (D-011), so it has no Slipnet to ask.

- **As revised (loop0007, item 03):** the table gains the roles' 29 rows (D-080), 14 ISA
  and 15 labelled, each with source `re-interpretation` or the page that supports it; the
  label nodes `opposite` and `sameness`, linked to nothing in v1, are now linked (`key`,
  `echo`, `counterpart`).

### D-010: Link lengths
- **Kind:** spec-inferred.
- **Sources:** "Every link in the Slipnet also has a length." [F95 p. 56] Path length is the
  default distance [F95 p. 59]; the only numbers are relative (knife-fork short,
  knife-saltshaker very long) (SPEC.md 3.3, 3.5). No page gives a length.
- **Choice:** integer lengths from 10 to 100, data beside each link in `slipnet_def.py`: a
  type's nearest category 30 and its other categories 35; the cup's defining category,
  "liquid holder", 15, because the first dump shows it receiving most of what spreads from
  "cup" before any codelet runs (4 against 2 for the next four) [F95 p. 96 (img)];
  "receptacle" 60 for each of its members (the vaguest category; the dumps keep it near the
  floor, D-009); used-with and similar-shape links 25; smaller-than links 35, but the cup to
  the small glass 25; the opposite links between position descriptors 40 (end-middle, under
  "position", 40 too); the identity links 10; the number links 30; and the ISA links of
  descriptors to their superordinates 30. With D-012's two-link paths these give, before any
  label shrinks anything (item 12 computes them): cup-liquid holder 10.0, cup-small glass
  11.0, knife-fork 11.7, cup-big glass 12.2, cup-open object 13.1, cup-saucer 14.1, fork-spoon
  14.1, cup-plate 16.1, cup-crockery 19.4, cup-eat-from object 35, cup-spoon 50, spoon-small
  glass 65, cup-receptacle 85, and no path of two links or fewer, so no finite distance, for
  cup-saltshaker, spoon-saltshaker and knife-saltshaker: the ten orderings of D-009 hold
  (`tests/test_slipnet.py` asserts them). The plan's rounder estimates ("about 15", "about
  65") were replaced by the computed values.
- **Reason:** the orderings are all the text gives; round numbers in a narrow band keep the
  table readable and leave the orderings to the structure of the links rather than to tuned
  lengths. Nothing else was adjusted (the orderings are qualitative evidence, allowed by the
  validation protocol).
- **Knob:** none (data).
- **Alternatives:** Copycat's lengths for the shared nodes; one length for every link, with
  the distances coming from path counts alone.

- **As revised (loop0007, item 03):** the roles' links take their lengths from these bands
  (30 nearest, 35 other, 40 a descriptor's opposite; D-080), so no new length was added.

### D-011: Label-controlled length: a linear shrink
- **Kind:** Copycat choice.
- **Sources:** "the length of any labeled link varies dynamically as a function of the
  activation of the link's label node" [F95 p. 56]; an active label shrinks every link it
  labels [F95 p. 62]; the shrink is undone when the context decays [F95 p. 56]; only labeled
  links change (SPEC.md 3.4). The function is not given.
- **Choice:** the effective length of a labeled link is its intrinsic length times
  (1 - s x a_label / 150), with s = `label_shrink` = 0.6: a label at the ceiling shrinks its
  links to 40 percent of their length, a dormant label (activation 1) leaves them as they
  are. A link with two labels (saucer-plate under "smaller than" and "bigger than",
  left-right under "opposite" and "diagonal symmetry") takes the more active of the two.
  ISA, has-member and identity links never change. Distances (D-012) are recomputed from
  the effective lengths after every Slipnet update, after decay, so a label's shrink is that
  of its activation at the end of the update.
- **Reason:** Copycat's rule is also a shrink proportional to the label's activation; a
  linear law with one coefficient is the simplest that is continuous and undoes itself as
  the label decays. The value 0.6 keeps the shrunk link longer than an identity link (10),
  so a slippage never becomes free. The lesion "fixed link lengths" [FCCA p. 365] sets s to 0.
- **Knob:** `Config.label_shrink` (0.6).
- **Alternatives:** Copycat's exact rule (the link's length is the label's "shrunk length"
  when the label is fully active, else the intrinsic one); a threshold switch.

### D-012: Which paths count: one and two links
- **Kind:** spec-inferred.
- **Sources:** "All pathways contribute to reducing the conceptual distance between nodes"
  [F95 p. 60]; Figure 3.6 shows a direct link and two two-link paths through ISA nodes, and
  the distance is the parallel combination of the paths' lengths, each path's length being
  the sum of its links' [F95 p. 61 (img)] (SPEC.md 3.5). Whether longer paths count is not
  said.
- **Choice:** the distance between two nodes is 1 / sum(1 / L_p) over every path p of at
  most `max_path_links` = 2 links, where a path may use any link in either direction (ISA
  and has-member being each other's inverses, a two-link path through a category counts
  once; identity links join a node to itself and are not pathways); two nodes with no such
  path are at distance infinity (`math.inf`), out of reach of spreading (D-013) and of any
  slippage. The pathways are enumerated once, when the Slipnet is built (288 pathways over
  138 pairs for the table of D-009), and their lengths are re-summed from the effective
  lengths (D-011) after every update.
- **Reason:** Fig. 3.6 is the specification and shows paths of one and two links only;
  longer paths through a hodgepodge of categories would make every pair of nodes close, and
  French says knife and saltshaker are joined only by "a series of simple links" that is "very
  long" [F95 p. 59], which two-link paths through "receptacle" and "closed object" already give.
- **Knob:** `Config.max_path_links` (2).
- **Alternatives:** all simple paths up to three links; shortest path only (Copycat).
- **As revised (loop0008, item 01):** the distances are kept as a flat n x n matrix
  (`Slipnet.distances`, d(i, j) at `i * n + j`, 0 on the diagonal, `math.inf` where no
  pathway joins the pair), rebuilt after every update from the same pathways by the same
  sums in the same order, and `distance` reads it by index; the tuple-keyed dict is gone.
  Each node's neighbours, the nodes a pathway joins it to, are listed once
  (`_neighbours`) for spreading (D-013). Every run is unchanged (D-108).
- **As revised (loop0008, item 03):** on the compiled engine the matrix, the pathways and
  the neighbour lists are C arrays and the sums C loops, in the same order (D-111).

### D-013: Spreading: activation over distance, buffered
- **Kind:** spec-inferred.
- **Sources:** activation spreads "in a manner inversely proportional to the distance between
  the two concepts" [F95 p. 59]; "the node cannot spread more activation than it possesses"
  [F95 p. 59]; the amount from A to B "is proportional to the activation of A divided by
  dA,B" [F95 p. 61]; FCCA limits it to "immediate neighbors, gated by conceptual proximity"
  [FCCA p. 381] (SPEC.md 3.6). The constant and the reach are not given.
- **Choice:** on each Slipnet update, every node A with activation above the floor sends to
  every cooler node B (a_B < a_A) within distance `spread_radius` (100) the amount
  c x (a_A - a_B) / d(A, B), with c = `spread_constant` = 0.5; the total sent by A is capped
  at its excess over dormancy, a_A - 1 (every amount scaled down proportionally when the
  sum would exceed it); the sender keeps its activation. The amounts are summed in a buffer
  and added after every node has been read, so the order of nodes does not matter. The
  spread does not pass through: what B receives in one update is not forwarded until the
  next. **As built (item 12):** the plan's form, c x a_A / d with c = 10, was replaced by
  the difference form and c = 0.5 before any mechanism beyond the Slipnet existed, for the
  reasons below; `Config.spread_constant` = 0 is the lesion that stops spreading.
- **Reason:** French's picture is conduction: "The closer the two nodes are together, the
  more activation is radiated from the 'warmer' node to its neighbor." [F95 p. 60], and his
  distance formula is the resistor formula [F95 p. 61 (img)], in which a current flows in
  proportion to the difference of potential over the resistance; the sentence "the amount
  of activation that spreads from A to B is proportional to the activation of A divided by
  dA,B" [F95 p. 61] is kept in the amount's form, with the cooler node's activation
  subtracted, and the cap is his "the node cannot spread more activation than it possesses"
  [F95 p. 59]. The difference form is also the only one of the two that the screen dumps
  allow: with a sender that keeps its activation and sends c x a_A / d to every node in
  reach, the net's total activation grows by a constant factor per update (every unit of
  activation sends out c times the sum of 1/d over its reach, 0.3-0.6 for the table of
  D-009, against a decay of 0.02-0.08), so that every node joined to the touched type
  reaches the ceiling within a few tens of updates; the nine dumps show "receptacle" at 1-6
  and "crockery" at 2-25 through 55 codelets while "cup" stands at 51-130 [F95 pp. 96-107
  (img)]. Under the difference form no node can become warmer than the warmest node it is
  fed by, and a category fed by one steady source settles at the fraction (c/d) / (c/d +
  decay) of it. The constant 0.5 is a round number that puts the first update's spread from
  "cup" at 51 into "liquid holder" at about 2.4 (the dump shows 3 above the floor) and the
  settled level of a category at distance 10-20 from its type at a third to a half of the
  type's (the dumps show "liq holder" at 32-50 under "cup" at 127-130): a direct reading of
  the dumps, as the validation protocol allows, not a fit (ANSWERS.md 1). Synchronous
  buffering is the only way to make the result independent of node order (lists, never
  sets).
- **Knob:** `Config.spread_constant` (0.5), `Config.spread_radius` (100.0).
- **Alternatives:** the plan's form, c x a_A / d with the sender keeping its activation
  (unstable, above); spreading only along direct links (FCCA's "immediate neighbors"); a
  fraction of the sender's activation divided among its neighbours (Copycat); a
  conservative flow in which the sender loses what it sends (stable too, but then "cup"
  could not hold 130 for forty codelets [F95 pp. 99-107 (img)] without jolts of its own
  size every update).
- **As revised (loop0008, item 01):** the sender reads only its neighbours of D-012, in
  definition order, since every other node is at distance infinity and never in reach; the
  amounts, their sum and the cap are the same floating-point operations in the same order,
  so every run is unchanged (D-108).

- **As revised (loop0008, item 03):** on the compiled engine spreading is a C loop over
  the same neighbours, the same amounts summed in the same order (D-111); the runs are
  unchanged.
### D-014: Decay by depth
- **Kind:** Copycat choice.
- **Sources:** "The greater the conceptual depth of a node, the less its activation will
  decay with each cycle." [F95 p. 62] Decay returns a node "to dormancy" [F95 p. 62], the
  floor of 1 (D-015). The function and the rate are not given (SPEC.md 3.6).
- **Choice:** on each update, a <- a - (a - 1) x (100 - depth) / 100 x `decay_rate`, with
  `decay_rate` = 0.1: a level-1 node (depth 20) loses 8 percent of its excess over the floor
  per update, a level-3 node (depth 80) 2 percent. Applied after spreading and jolts, before
  the clip (D-015).
- **Reason:** Copycat's decay is a loss proportional to (100 - depth); the proportion is the
  simplest rate, and 0.1 per codelet (D-019) lets a jolt of 40 on a level-1 node fade to
  near the floor in about 30 codelets, which is what French asks of a concept noticed once
  and never again [F95 p. 62], while "opposite" (level 3) keeps most of its activation across
  a run of 50-200 codelets, as the dumps show it doing [F95 pp. 96-107 (img)].
- **Knob:** `Config.decay_rate` (0.1).
- **Alternatives:** exponential decay with a half-life per level; Copycat's rounding to
  integers.
- **Finding (item 12):** under this law "opposite" (level 3) would fall from 139 to about
  109 in the twelve codelets before Fig. 4.3, where the dump shows 137 [F95 p. 99 (img)];
  decay alone cannot hold it there (holding it would need a level-3 rate near zero, against
  "all nodes lose activation through decay" [F95 p. 62]), so French's "opposite" was being
  resupplied, by codelets looking at the opposite side of the table or by the slippages'
  jolts (D-030). Which, the sources do not say; the rate is not changed for it, and the
  course of "opposite" across a run is a result for RESULTS.md (M2).

### D-015: The range 1 to 150, clipped
- **Kind:** spec-inferred.
- **Sources:** "Activation values range from 1 to 150." [F95 p. 95] The initial value of a
  node is 1 [F95 p. 87]; three nodes stand at exactly 150 in the last dumps [F95 p. 107 (img)]
  (SPEC.md 3.6, 6c.3).
- **Choice:** activations are real numbers kept in [1, 150] by clipping after every change
  (jolt, spread, decay, reset); 150 is a ceiling that is reached, not an asymptote. The trace
  and the GUI print them rounded, as the screen does.
- **Reason:** the sentence and the dumps.
- **Knob:** none (`slipnet.ACTIVATION_MIN`, `ACTIVATION_MAX` are constants citing p. 95).
- **Alternatives:** integer activations throughout (Copycat).

### D-016: Jolt sizes
- **Kind:** simplest.
- **Sources:** a find-group codelet "has sent a standard jolt of activation to the 'group'
  node" [F95 p. 97]; the grid shows "group" at 44 two codelets in, from 1 [F95 p. 98 (img)];
  a slippage sends activation to its label node [F95 p. 62]; the size of a jolt and whether
  every codelet sends the same are not said (SPEC.md 3.6, 3.7).
- **Choice:** three sizes: `jolt_structure` = 40 when a structure is found or built (a group,
  a subgroup, a neighbour relation, a correspondence: to "group", "neighbor", the group's
  kind node, and so on), `jolt_description` = 20 when a descriptor is noticed (a type, "end",
  "middle", "left", "right", "horizontal", a count), `jolt_label` = 20 to a label node for
  each slippage that supports a correspondence entering the Worldview (D-030). Every codelet
  of a kind sends the same.
- **Reason:** "a standard jolt" says there is one size per event; 40 is the one direct
  reading the dump allows (1 to 44 after one find-group, before the decay of that update
  would take it back to about 41); descriptors and labels get half, so that a structure
  noticed outweighs a property noticed.
- **Knob:** `Config.jolt_structure` (40), `Config.jolt_description` (20), `Config.jolt_label`
  (20).
- **Alternatives:** one size for everything; jolts scaled by the strength of the structure.
  As built (item 32, a note): a jolt changes the node's activation at once, clipped
  (D-015), and the next update spreads it; the alternative, a jolt held in a buffer until
  the next update, would differ by at most one codelet under D-019's update per codelet and
  is not built.

### D-017: Initial activations: opposite 139, the touched type 51
- **Kind:** screen-dump-inferred.
- **Sources:** at the start "opposite" and the touched type are active [F95 p. 64; F95 p. 66];
  at 0 codelets the grid shows "opposite" 139, "cup" 51, "liq holder" 4 and four of the cup's
  neighbours 2, every other node 1 [F95 p. 96 (img)]; "some activation has already begun to
  spread" before any codelet ran [F95 p. 97] (SPEC.md 3.8, 6c.2). Whether 139 and 51 are the
  values set or the values after a decay step is not shown.
- **Choice:** every node starts at 1; "opposite" is set to `initial_opposite` = 139 and the
  touched object's type to `initial_touched_type` = 51; then one Slipnet update (spreading,
  decay, the label trickle; no top-down posting) runs before the Coderack is loaded, so that
  the touched type's neighbours carry a little activation when the first codelet runs
  (`Slipnet.start`). After that update "opposite" stands at 136.2 and "cup" at 47.0, with
  "liquid holder" the warmest of the cup's neighbours; the first dump's boxes at 1 all
  round to 1 and its boxes above 1 are all above the floor (`tests/test_slipnet.py`, a
  direction check only). The values are read off the dump directly, which the validation
  protocol allows (ANSWERS.md 1).
- **Reason:** the dump is the only evidence; taking its numbers as the values set is the
  reading that adds nothing. The plan's 150 and 150 are rejected by the dump: neither node is
  at the ceiling at 0 codelets, and "opposite" holds 137-139 for 22 codelets.
- **Knob:** `Config.initial_opposite` (139), `Config.initial_touched_type` (51).
- **Alternatives:** 150 and 150 before a first decay (the plan's proposal; a level-3 node
  would not fall from 150 to 139 in one step under D-014); the values after the update being
  the targets.
  As built (item 24, `World.focus`, `Config.focus_jolt` = 20): the dumps hold the touched
  object's category and "opposite" near the ceiling for the whole run, cup 89 at 2 codelets
  and 127-130 from 12 codelets on, opposite 137-150 [F95 pp. 98-107 (img)], while under item
  12's reading both decayed (cup from 57 at 2 codelets to about 20 by 22 codelets, opposite
  to about 90 by 33) and every type node the looks jolted rose above them, so that the
  salience of the isolated cup, "one of the most salient objects on the table" [F95 p. 97],
  fell under the groups' and the row's objects'. A direct reading of the dumps (ANSWERS.md 1)
  read with p. 66's sentence: the two nodes are kept active by one `jolt_description` each
  before every Slipnet update, published `by` `focus`; 0 restores the decaying reading. The
  size is the existing description jolt, not fitted: under it both saturate at 150 where the
  dumps read 130 and 137-150.

### D-018: The small amount a label gets when both its concepts are active
- **Kind:** simplest.
- **Sources:** "(In addition, whenever both of the concepts joined by a labeled link are
  active, a small amount of activation is also sent to the label node.)" [F95 p. 62] Neither
  "active" nor "small" is quantified (SPEC.md 3.7).
- **Choice:** on each update, for every labeled link whose two concepts are both above
  `label_trickle_threshold` = 50, each of its labels receives `label_trickle` = 0.05 times
  the mean of the two activations (at most 7.5 per update), buffered with the spread of
  D-013. Identity links (left-left and the like, D-009) join one concept, not two, and never
  trickle: the dumps show "mirror" at 1 for 36 codelets while "right" is at 26-133 [F95
  pp. 99-103 (img)].
- **Reason:** a third of the ceiling is the simplest reading of "active"; five percent of the
  mean is "small" beside a jolt of 20-40 and still lets a label rise over a few updates
  while its concepts stay active, which is the effect French describes.
- **Knob:** `Config.label_trickle` (0.05), `Config.label_trickle_threshold` (50).
- **Alternatives:** a fixed amount (5); the product of the two activations over the ceiling.

### D-019: The update schedule: every codelet
- **Kind:** simplest.
- **Sources:** decay happens "with each cycle" [F95 p. 62]; the dumps show "group" at 44
  after two codelets and 26 after twelve [F95 p. 98 (img); F95 p. 99 (img)]; runs are 50-450
  codelets long [F95 pp. 115-139] (SPEC.md 3.6, 6c.3). What a cycle is, is not said.
- **Choice:** a cycle is one codelet: the Slipnet is updated (spreading, label trickle,
  decay, clip, then top-down posting, D-020) after every codelet, `slipnet_update_every` = 1.
- **Reason:** the simplest reading of "each cycle"; with runs this short, Copycat's update
  every 15 codelets would give only three or four updates to a typical run, too few for the
  dumps' smooth changes.
- **Knob:** `Config.slipnet_update_every` (1).
- **Alternatives:** every 5 or 15 codelets, with the rates of D-013, D-014 and D-018 scaled.
  As built (item 32, a note): the update has no probabilistic jump of a warm node to full
  activation and the Slipnet has no clamp of its initial nodes (Copycat's two mechanisms):
  French's account of activation, jolts, spreading and decay (SPEC.md 3.6, [F95 pp. 59-60]),
  names neither, and the initial state of D-017 and the focus jolts of D-017 (item 24) do
  the work a clamp would. A Slipnet built from the same table in another order gives the
  same activations and distances (a test, item 32).
- **As revised (loop0008, item 01):** the update's report of what moved is worked out
  when it is first read, not at every update (D-108); the schedule is unchanged.
- **As revised (loop0008, item 03):** on the compiled engine the cycle is the C loops of
  `_native/slipnet.pyx`, operation for operation the Python's (D-111); the report as D-108.

### D-020: Top-down posting: the node-to-codelet table
- **Kind:** spec-inferred (the examples), simplest (the rule).
- **Sources:** an active concept "dispatches codelets to look for instances of itself in the
  Workspace" [F95 p. 64]; the examples are group -> find-group [F95 pp. 63-64], neighbor ->
  find-neighbor [F95 p. 97]; the paper says "proportionately fewer codelets are sent out to
  look for groups" as "group" decays [FH91 p. 178]. The table, the threshold of
  "sufficiently active" and the number posted are not given (SPEC.md 3.9).
- **Choice:** a table `top_down_table` from node to codelet type: group -> find-group;
  neighbor -> find-neighbor; end -> find-end-objs; middle -> find-middle-objs; sameness ->
  simple-sameness-detect; the eleven object types -> pick-salient-obj; mirror symmetry,
  diagonal symmetry and opposite -> pick-strong-cp; isolated -> pick-2nd-salient-obj. After
  each update, every node in the table posts as many codelets of its type as its activation
  has *risen* since the last posting, divided by `top_down_rise` = 40 (the integer part
  always, the remainder with one coin, in the table's order); a node that held or fell posts
  nothing. The codelets are posted at the type's raw urgency (D-052), generation 0 (D-054),
  with no argument: the codelet chooses what to look at when it runs (by salience, D-034).
  `Slipnet.top_down_postings(rng)` returns the (node, codelet type) pairs; the run loop
  posts them.
- **Reason:** the two examples give the pattern (a concept posts the codelet that finds its
  instances) and the rest of the table applies it to the concepts that have such a codelet
  (SPEC.md 5.2). The rate follows the rise because French ties the dispatch to the rate of
  change: "as the rate of group-finding falls (i.e., the rate of change of activation of
  'group' decreases) and less activation gets sent to 'group' - and thus the activation of
  'group' decays - fewer codelets will be sent out to look for groups" [F95 p. 64], and the
  dump at two codelets shows "group" at 44, far below any threshold, having "spawned a fair
  number of similar find-group codelets" [F95 p. 97]. A threshold rule (item 12's first
  reading: a node at or above 100 posts with probability a / 150) is contradicted by the nine
  dumps read directly: "cup" sits at 127-130 from the twelfth codelet on while the
  pick-salient-obj bar never exceeds 3 percent, and "end" at 99-132 while find-end-objs
  stays at 1-5 percent [F95 pp. 96-107 (img)]; at that rate they would pile up. Forty is one
  structure jolt (D-016): one find posts one more looker, French's apple picker.
- **Knob:** `Config.top_down_table`, `Config.top_down_rise` (40.0). (`Config.top_down_threshold`
  = 100 remains the level a node must reach to count as active for a correspondence's
  kind factor, D-036.)
- **Alternatives:** the threshold rule (item 12's); a lower rise per codelet; counting the
  jolts themselves rather than the net rise after decay.
  As built (`Slipnet.top_down_postings`, items 12 and 19): the activations at the last
  posting are kept on the Slipnet; a whole-number rise spends no coin. In the first whole
  runs the "opposite" node, static at the start, posts nothing until the scouts' looks across
  the table resupply it (D-071).
  As built (item 24, `Slipnet.start`): the posting levels start at the initial state, so
  that the initial activations of D-017 are not read as a rise (started at 1, the first
  update posted pick-strong-cps from "opposite" at 139 and pick-salient-objs from the
  touched type, all fizzling; D-055's starting rack already holds p. 65's pick codelets).

- **As revised (loop0007, item 03):** `top_down_table` gains the role nodes' rows (D-081):
  nine role nodes post `propose-role-cp` and `role` posts `check-roles`; the rows for the
  other role codelets come with their items.

### D-021: Context push and pop; the lossy pop fixed
- **Kind:** FIXED (ANSWERS 2); spec-inferred for the mechanism.
- **Sources:** SPEC.md 3.10 and 6b (fault 4). When examine-cp(cp1) runs "it enters a subset
  of the full Tableworld, a smaller world consisting of just the two groups in question" [F95
  p. 85]; on the push the activation "is set to its initial value (in this case, 1)" [F95
  p. 87]; on the pop the program "reconstructs the activation values at that level by looking
  around in the Workspace and finding the correspondences in that context" [F95 p. 88],
  putting "a fixed amount of activation" into "right", "left" and "diagonal symmetry" for a
  right-left slippage and into "right" (twice) and "mirror symmetry" for a right-right one
  [F95 pp. 88-89]. French's verdict: "too much is lost. For example, all of the (possibly
  considerable) activation that might have spread to these concepts from other concepts is
  lost." [F95 p. 89]; stacks are "too 'clean'" [F95 p. 88] and reconstruction is the way [F95
  p. 89].
- **Choice:** a context is a group-level correspondence in the Worldview. The push happens
  when a codelet that works inside one (examine-cp, and the create-cp, build-cp-descrip and
  cp-compete codelets it spawns, each carrying the context as an argument) starts running
  with a context different from the current one; the engine then sets every node of the
  reset set (D-070) to 1. The pop happens when the next codelet to run carries no context, or
  another one: the engine reinitializes the reset set again, then reconstructs the outer
  level from the Worldview's correspondences at that level: for each supporting slippage it
  adds `pop_fixed_amount` = 30 to the slippage's two concepts and to its label (the pattern of
  pp. 88-89: a right-left slippage feeds right, left and diagonal symmetry; a right-right
  identity feeds right twice and mirror symmetry), **and**, the fix, one spreading step
  (D-013) from every other node into the reinitialized nodes at their current levels,
  before the amounts are clipped (`pop_restores_spread` = True). Contexts do not nest
  (groups of groups are outside the problem library; a codelet inside a subworld that finds
  a group correspondence posts its examine-cp at the top level). `context_mechanism` =
  False (a lesion, FCCA criterion 11 [FCCA p. 365]) turns push and pop off.
- **Reason:** French asks for reconstruction that recovers "the activation that might have
  spread to these concepts from other concepts" and names "the activation levels of the
  nodes connected to the reinitialized nodes" as the refinement [F95 p. 89]; one spreading
  step from the neighbours is exactly that. The fixed amount 30 is three quarters of a
  structure jolt: a slippage found earlier counts a little less than one found now.
  Reading (a) of the plan (the context is the running codelet's) needs no nested loop and
  matches "when examine-cp(cp1) runs".
- **Fault:** French's pop put only the fixed amounts in [F95 pp. 88-89]; `pop_restores_spread
  = False` restores it (`french-faults`).
- **Knob:** `Config.pop_fixed_amount` (30), `Config.pop_restores_spread` (True),
  `Config.context_mechanism` (True).
- **Alternatives:** an activation stack (French rejects it); reconstruction from every
  structure at the level, not only correspondences; nested contexts.

  As built (`context.py`, `World.step`, `Slipnet.reset`, `restore`, `spread_step_into`): the
  paragraph moved for size to DECISIONS-INDEX.md, "D-021's as-built paragraph".

### D-070: What the context push resets
- **Kind:** FIXED (ANSWERS 2).
- **Sources:** SPEC.md 3.10 and 6b (fault 5). Of the two proposals, French adopted "Only
  concepts that participate in the supporting slippages of the correspondence under
  investigation are reinitialized." [F95 p. 88], but "In reality, however, only the
  'mirror-symmetry' and 'diagonal-symmetry' nodes are reinitialized" [F95 p. 88]; the
  question "definitely deserves to be studied more carefully" and the second proposal is
  "The most fruitful path" [F95 p. 88].
- **Choice:** `push_reset_set` = "slippage-concepts": the push (D-021) sets to 1 every node
  named by a supporting slippage of the group correspondence being entered, its two concepts
  and its label (for a left-right slippage: left, right, opposite, diagonal symmetry; for an
  identity: the concept and mirror symmetry), and always the two symmetry nodes. The pop
  reconstructs the same set.
- **Reason:** the policy French says he adopted, taken at its word, with his own two nodes
  kept as the minimum.
- **Fault:** the two-node version; `push_reset_set = "symmetries"` restores it
  (`french-faults`). This is the weakest of the seven fixes (SPEC.md 6b).
- **Knob:** `Config.push_reset_set` ("slippage-concepts").
- **Alternatives:** the first proposal, every node above a threshold.

  As built (`context.reset_set`, item 20): the two symmetry nodes first, then, under
  "slippage-concepts", every node a supporting slippage of the correspondence being entered
  names (its two concepts, then its labels; the kind slippage none), each once, in the
  slippages' order. For the chapter 4 row-to-row correspondence (many-many,
  horizontal-horizontal) the set is the two symmetries, "many", "sameness" and
  "horizontal"; for a correspondence not yet described, the two symmetries only. Under
  "symmetries" (`french-faults`) exactly French's two nodes, whatever the slippages; the
  pop then adds its fixed amounts to "right" and "left" on top of their current levels, as
  pp. 88-89 describe.

### D-071: "Opposite" as a descriptor of objects seen across the table
- **Kind:** screen-dump-inferred.
- **Sources:** "the concept 'opposite' and the category of the touched object ... are active
  initially" [F95 p. 66]; the pressures include "being diagonally or directly opposite the
  touched object" [FH91 p. 178]; the nine dumps hold "opposite" at 137-139 for twelve
  codelets and at 150 from the thirty-third on [F95 pp. 96-107 (img)], where the decay law
  alone (D-014) would take it to about 109 and then lower: something resupplied it (D-014's
  finding). No page says what.
- **Choice:** an object seen by a scout's look in a region diagonally or directly opposite
  the touched object's (D-032's two relations) is described as `opposite` the first time,
  and the descriptor jolts the "opposite" node by `jolt_description` (D-016), like any other
  descriptor noticed.
- **Reason:** the looks across the table are the one recurring action of the early run
  that the pressure lists tie to "opposite", and a descriptor with its jolt is how every
  other noticing enters the Slipnet (D-016); it is the smallest mechanism that resupplies
  the node as the dumps show. Under the rise rule of D-020 the jolts also let "opposite"
  post pick-strong-cp codelets while the scouts look across, which the correspondence
  chain needs (SPEC.md 4.6.4).
- **Knob:** none (`jolt_description` sizes it).
- **Alternatives:** a jolt to "opposite" from every correspondence whose ends lie across
  the table from each other; no resupply (item 12's state).

## Perception, groups, descriptions and salience (D-022 to D-034, D-069)

### D-022: The proximity criterion of a gestalt group
- **Kind:** spec-inferred.
- **Sources:** a gestalt group is "a bunch of objects that are physically close together"
  [F95 p. 97]; "sheer physical proximity is often a sufficient reason to form a group" [FCCA
  p. 390]; French pulls the two glasses of Blockage 6 apart by one, two and three glass widths
  [F95 p. 128] and says the group forms less and less often until, at three widths, the
  glasses are no longer grouped [F95 pp. 128-129] (SPEC.md 4.4.1; the measured gaps are 0.6,
  1.3, 1.8 and 2.6 widths, D-003). No criterion is given.
- **Choice:** two objects are linked when a coin passes with probability p(g), where g is the
  gap between their footprints' boxes (D-002) in small-glass widths (4.5 units): p = 1 for
  g <= `proximity_full_gap` (0.5), falling linearly to 0 at g = `proximity_zero_gap` (2.5),
  0 beyond. The coin is not softened by temperature. A gestalt group is a connected
  component of these links among the objects a find-group codelet looks at (D-023); an
  object with no link is noticed as isolated (a jolt to "isolated", D-007). The gap is
  measured between boxes, which is where an object's size enters the engine (D-002).
  As built (`groups.link_probability`, `linked`, `proximity_component`): the coin is drawn
  only when p is strictly between 0 and 1, so a pair that touches or a pair past the zero
  gap costs no draw and Blockage 10's glasses are never grouped, from any seed. A finding,
  recorded before any bench exists: the chapter 4 row on Henry's side (fork fork knife cup
  spoon spoon) has gaps of 0.3 to 0.64 glass widths on the digitized figure, so four of its
  five links are coins at about 0.93 and a flood fill from an end finds the whole row about
  73 percent of the time, where the chapter 4 run found it at once [F95 p. 97]; the end
  points are kept, and how often the row comes whole is for RESULTS.md.
- **Reason:** French's statement of behaviour (often at one width, never at three) is
  qualitative evidence the protocol allows; the two end points were set from his words in the
  plan, before the figures were measured, and are kept unchanged although the drawn gaps turn
  out to be 1.3, 1.8 and 2.6 widths: whether the engine's frequencies fall as his do is a
  result (RESULTS.md), not a setting.
- **Knob:** `Config.proximity_full_gap` (0.5 widths), `Config.proximity_zero_gap` (2.5).
- **Alternatives:** a hard threshold; a probability that also falls with the objects'
  conceptual distance (that is the bottom-up stage, D-024).
- **As revised (v5, D-118):** `proximity_zero_gap` is bounded at 2.6 widths by French's
  Blockage 10 and the default is the bound; v4's fitted 5.4786 is refused.

### D-023: Find-group's search: a seed and a flood fill
- **Kind:** spec-inferred.
- **Sources:** group-finding codelets "will preferentially (but not deterministically) look
  in the corner of the table diagonally opposite the touched object" [FH91 p. 178]; the first
  find-group of the chapter 4 run found the whole group of six on Henry's side at once [F95
  p. 97]; find-group-around(x) takes an object [F95 p. 77]; the follow-ups are listed on p. 76
  (SPEC.md 4.4.1, 5.3).
- **Choice:** a find-group codelet without an argument chooses a region by the region
  preference (D-032, under D-060), then a seed object in it by salience (D-031, under D-060;
  an empty region makes the codelet fizzle); find-group-around(x) uses x as the seed. From
  the seed it flood-fills the proximity links of D-022 (coins drawn in the order of the
  objects' ids) and proposes the component as a gestalt group if it has two or more members
  and is not already in the Workspace; a component of one jolts "isolated". On success it
  jolts "group" (D-016), records the group, and posts p. 76's follow-ups (D-052).
  As built (`perceive.look`, `groups.find_group_around`): a look at a region draws every
  object's region through the blur (D-004), in file order, and notices the objects seen
  there, jolting the type node of each on its first look (a descriptor, D-016); a region
  with nothing in it fizzles the codelet. The flood fill runs over every object of the
  problem, not only the noticed ones ("proximity groups out of objects it hasn't yet even
  looked at individually" [FCCA p. 390]), with each pair tested once in file order, and the
  members are not thereby noticed one by one. A component identical to an existing group's
  members is nothing new; a component that overlaps an existing group without equalling it
  is recorded as a second gestalt group, and the compatibility fights of D-025 decide
  between them. The choice of region (`perceive.choose_region`) raises the preferences of
  D-032 to the exponent a(T) the caller passes (D-060); the seed by salience is the
  codelet's (D-031, D-034).
- **Reason:** the sentences above, read together: a biased look, a whole bunch found at once,
  then the details.
- **Knob:** none.
- **Alternatives:** growing the group one neighbour at a time (inside-out, which FCCA says
  Tabletop does not do first [FCCA p. 390]).

### D-024: Bottom-up groups: three stages, pass by kind
- **Kind:** screen-dump-inferred (the stages), simplest (the values).
- **Sources:** sameness groups "almost always", shared-ISA groups "usually", groups with
  nothing in common "only very rarely" [F95 p. 65]; building a group "involves an escalating
  series of 'micro-tests' that check the physical and conceptual distances between
  prospective group members, as well as the compatibility of the prospective group with other
  already-established groups" [FCCA p. 384]; the strip's labels build-obj-0, -1, -2 [F95 p. 96
  (img)]; at high temperature "the program is relatively willing to try such things out"
  [F95 p. 111] (SPEC.md 4.4.2, 6.4).
- **Choice:** build-object proposes a run of adjacent members of a gestalt group (a pair of
  neighbours, or an existing bottom-up group plus one adjacent member, D-027) and goes
  through three stages, one codelet each, in FCCA's order: build-obj-0 tests physical
  closeness (the proximity coin of D-022 between the new neighbours); build-obj-1 tests
  conceptual closeness: a coin with chance q by kind, `group_stage_pass`, 0.95 for a
  sameness group, 0.6 for a shared-ISA group (all members share a category node), 0.1 for
  anything else, under D-060; build-obj-2 tests compatibility: the group is built (its
  strength from D-025, a jolt to "group" and to its kind), then fights any Workspace group it
  overlaps without containing or being contained by (D-025), and enters the Worldview on
  winning. Each stage posts the next at the same raw urgency, one generation on.
  **As built** (`build_object.py`, `groups.py`): a proposal names its members in file
  order, its kind and the links stage 0 tests (the pair; or the new member and the member
  of the base it is adjacent to in the parent's chain); the candidates are every neighbour
  pair of the parent's chain (D-027) and every bottom-up group inside the parent plus one
  adjacent member, so nothing is proposable before find-neighbor has run on the parent. A
  bottom-up group over the same objects as a gestalt group is a new structure, "a much more
  solid structure in which each of the elements knows about its neighbors" [F95 p. 125]; the
  same bottom-up group twice is nothing. The kind is read off the members' types:
  `sameness` when all are of one type; `shared-isa` when every member reaches a common
  category by an ISA link of intrinsic length at most `shared_isa_max_length` = 30 (a type's
  own superordinates: silverware, crockery, liquid holder for the drinking vessels, closed
  object), the nearest common one in table order being the group's `category`; `mixed`
  otherwise. The bound is needed because D-009's Slipnet joins spoon to liquid holder by a
  long link (35), and without it a cup and a spoon, French's example of objects that "have
  nothing in common" [F95 p. 65], would share a category. Stage 0 draws no coin for a pair
  that always links (D-022). Stage 1's chance is `group_stage_pass` by kind (plus D-069's
  term under `french-faults`), passed under D-060's yes-or-no law at the exponent the
  codelet passes. Stage 2 creates the group inside its parent with the parent's neighbour
  relations among its members (its chain known, so its ends can be read at once), jolts
  "group" by `jolt_structure` and the kind's node ("sameness", or the shared category; none
  for a mixed group) by `jolt_description`, keeps its strength on the group, fights its
  rivals (D-025) and, surviving, absorbs the subgroups of D-026. Entering the Worldview is
  the Worldview item's.
- **Reason:** three labels, three micro-tests; the kinds and their order of likelihood are
  French's; under D-060 the values give, at a middling temperature (a = 3.5), about 1.0, 0.8
  and 0.0005 per test and at T = 80 about 0.9, 0.6 and 0.06: "almost always", "usually",
  "very rarely", and more willing at high temperature, as the pages say.
- **Knob:** `Config.group_stage_pass` ((sameness 0.95), (shared-isa 0.6), (mixed 0.1));
  `Config.shared_isa_max_length` (30).
- **Alternatives:** one stage with the product of the three chances; chances derived from the
  members' Slipnet distance rather than their kind; any shared category, however distant,
  making a shared-ISA group.
  As built (item 32, a note): a proposal stays in the Workspace's registry for the whole
  run at the stage it reached (a build-obj codelet aged out or pushed off, D-053, leaves
  it as it was; at most 10 proposals in 530 runs over the catalogue); the snapshot shows
  them with their stage for the proposal pane, nothing else reads them.
  `build_object.build_object`, every remaining stage at once, is a test helper.

### D-025: Group strength: five factors on 0-100
- **Kind:** simplest.
- **Sources:** FCCA's five factors: "its physical dimensions", "number of constituent items",
  "uniformity of composition", "distance between constituent items", "the prior existence of
  similar groups" [FCCA pp. 382-383]; groups fight when one "would lay claim to items already
  'taken' by the pre-existing group" [FCCA p. 383]; "groups do not overlap" in the Worldview
  [FCCA p. 390]; a subgroup inside a group is not a rival (Fig. 3.9 [F95 p. 71 (img)])
  (SPEC.md 4.4.3). No weight is given.
- **Choice:** strength = compactness 20 x (1 - span / 55) + size 10 x min(n, 5) / 5 +
  uniformity 40 x u + closeness 20 x (1 - mean gap / 2.5 widths) + 10 if a group of the same
  kind and size exists elsewhere in the Workspace, clipped to [0, 100], where span is the
  longer side of the members' bounding box in units, u is 1 for a sameness group, 0.5 for a
  shared-ISA group and 0 otherwise, and the mean gap is the mean of the gaps (D-022) between
  neighbours. Two groups conflict when they share a member and neither contains the other;
  the fight is under D-040's rule applied to the two strengths. A group's strength is
  recomputed when it is read.
  **As built** (`groups.py`): the span is the longer side of the bounding box of the
  members' footprint boxes (D-002); the gaps are the edge gaps (D-022) over the adjacent
  pairs, read from the parent group's neighbour relations among the members, or, before the
  chain is known, between consecutive members along the run's axis; the similar group is
  another group of the same kind and size in the Workspace sharing no member with this one;
  the sum is clipped to [0, 100] and kept on `Group.strength`, recomputed by
  `update_strength` whenever a fight reads it. A group fights every rival in the order the
  rivals were built, when it is recorded: a bottom-up group at stage 2 (D-024), a gestalt
  group as soon as find-group records it (FCCA's "sooner or later" [FCCA p. 383] read as at
  once, the simplest timing), at the exponent a(T) the codelet passes; the challenger wins
  with probability c^a / (c^a + h^a), strengths floored at 1; a rival it beats is removed
  as `lost`, the first fight it loses removes the group itself and ends its fights. Two
  sameness groups of one object type are never rivals (D-026): French's program kept
  "cup1-cup2, cup2-cup3, cup3-cup4, etc." side by side [F95 p. 140], and under the fix the
  later one is proposed as their union.
- **Reason:** the five factors, each on a plain scale, with uniformity heaviest because it is
  the only one French's own list of group salience factors repeats (sameness, shared-ISA,
  size [F95 p. 67]).
- **Knob:** `Config.group_strength_weights` ((compactness 20), (size 10), (uniformity 40),
  (gap 20), (similar_group 10)).
- **Alternatives:** the members' mean salience as a sixth factor (D-069 says why not).
  As built (item 32, a note): a bottom-up group is recorded, announced and its jolts sent
  before its fights (`build_object.stage_compatibility`), and `resolve_conflicts` removes
  each beaten rival as that fight is won, so a newcomer that loses a later fight has already
  removed an earlier rival and sent its jolts. The alternative, every fight first and the
  losers removed only if all are won, as the Worldview's competition does with whole
  Worldviews (D-040), is recorded here and not built: French describes a group's fights
  one rival at a time (SPEC.md 4.4.3) and gives no order. A gestalt group that loses leaves
  the bottom-up groups built inside it in place with their parent pointer to it; in 530
  runs over the catalogue no such orphan arose (bottom-up groups are rarely built, RESULTS.md
  §8.2), and no two groups of one kind over the same members were ever both built.

### D-069: A member's salience does not make a group
- **Kind:** FIXED (ANSWERS 2).
- **Sources:** SPEC.md 6b (fault 6). In Figure/Ground 1 "The program will often also create a
  forkglass group containing the touched fork" [F95 p. 141]; "Because of the high degree of
  salience of the touched fork, the program 'reasons' that if it creates a group out of the
  touched fork and the neighboring glass, the new group will be, if not a great one, at least
  not too bad. Indeed, without the presence of the touched object, such a group would
  probably never be made in the first place." [F95 p. 142] French calls it "the rather
  unhumanlike action" [F95 p. 141].
- **Choice:** the salience of a group's members does not enter its strength (D-025) or the
  pass chance of its stages (D-024): `member_salience_in_group_strength` = 0.0. A fork-glass
  group is as unlikely with the touched fork in it as without.
- **Reason:** French's words say the touched object's salience is what made the group, and
  that without it the group would not be made; removing the term is the smallest change that
  does what he says a human would do.
- **Fault:** `member_salience_in_group_strength` = 0.2 adds a fifth of the members' mean
  salience (0-100) to the strength and to the stage-1 chance (`french-faults`). The size of
  the faulty term is a reading: large enough that a member at the salience ceiling makes a
  mixed group "not too bad". **As built:** `groups.member_salience_term` is the share times
  the members' mean salience, added to the strength as points and to the stage-1 chance as a
  fraction of 100 (a mixed pair with the touched object at 100: 0.1 + 0.1 = 0.2, capped at
  1); the salience function is the `salience_of` argument of the strength and stage
  functions, supplied by the salience module when it exists, so the term is 0 until then.
- **Knob:** `Config.member_salience_in_group_strength` (0.0).
- **Alternatives:** keeping the term but excluding the touched object only.

### D-026: Subgroups absorbed, not kept
- **Kind:** FIXED (ANSWERS 2).
- **Sources:** SPEC.md 4.4.4 and 6b (fault 1). "It keeps all of the intermediate subgroups
  around, considering each of them important in its own right and trying to establish
  mappings with other 'similar' groups." [F95 p. 140] "Unlike humans, however, Tabletop
  retains all of the subgroups, which means that it gets hopelessly confused whenever it is
  given a problem containing groups made up of more than three identical objects. This is a
  problem with Tabletop that needs to be fixed." [F95 p. 140] Humans "tend to see them simply
  as a group of cups" [F95 p. 140]. On the screen "a subgroup is visible only when the program
  first discovers it, and thereafter only if it forms the end point of a correspondence in
  the Worldview" [F95 p. 101].
- **Choice:** `keep_all_subgroups` = False: when a sameness group is built that contains an
  existing sameness group of the same type, the smaller one is removed from the Workspace
  (and from the Worldview, with any correspondence that ends on it demoted to the Workspace
  as a loser, D-041), so that a run of identical objects ends as one group. Subgroups of
  different kinds (a fork-fork inside a silverware group) and gestalt groups containing
  subgroups are kept, as Fig. 3.9 shows them.
  **As built** (`groups.py`, `build_object.py`): under the fix a proposed sameness run that
  already lies inside a sameness group of its type is nothing new (not proposed), and one
  that shares a member with such a group is proposed as their union, repeated until nothing
  overlaps, so that cup2-cup3 beside an existing cup1-cup2 is proposed as cup1-cup2-cup3;
  when the union is built every smaller sameness group of its type inside it is removed as
  `absorbed`, and its name is never reused. The gestalt group over the same objects is a
  different structure and stays. A run of n identical objects therefore ends as one
  sameness group beside its gestalt group under `standard`, and as all n(n − 1)/2 runs of
  two or more under `french-faults`, the list French gives for six cups [F95 p. 140]. The
  demotion of a correspondence ending on an absorbed group is the Worldview item's.
- **Reason:** French's words; absorbing the smaller group is the smallest change that makes
  six cups one group.
- **Fault:** `keep_all_subgroups` = True keeps every intermediate subgroup as a structure in
  its own right (`french-faults`).
- **Knob:** `Config.keep_all_subgroups` (False).
- **Alternatives:** demoting the subgroup to a description of the larger group instead of
  removing it.

### D-027: Neighbours along the group's axis
- **Kind:** spec-inferred.
- **Sources:** find-neighbor "takes as a parameter an object in the group just found and
  determines its neighbors" [F95 p. 76]; the chain fork1 -> fork2 -> knife1 along a row [F95
  p. 80]; neighbours are looked for "to the right and left" of an object [F95 p. 97]; an
  object must be in a group to have a neighbour [F95 p. 76] (SPEC.md 4.4.5).
- **Choice:** within a group, an object's neighbours are the nearest other member on each
  of its four sides, left and right for a member displaced mostly along x, above and below
  for one displaced mostly along y, provided the object is in turn that member's nearest on
  the opposite side (mutual nearest, so that the relation is symmetric and "immediate"
  [F95 p. 97]: a member far down the arm of an L is the nearest thing below a member of the
  row without being its neighbour). In a linear group only the two sides along its axis are
  occupied, which gives the chain of p. 80 exactly. find-neighbor(x)
  (`describe.find_neighbours`) records each relation both ways with its side, jolts
  "neighbor" (`jolt_structure`, D-016) when a relation is new, marks x visited (so that
  D-028 can know when the chain is complete), and returns the new neighbours, for which the
  codelet posts find-neighbor(y), one generation on, as p. 80's chain does.
- **Reason:** the chain of p. 80 is one-dimensional, and "right and left" says so.
- **Knob:** none.
- **Alternatives:** the k nearest members in the plane; neighbours by the proximity links of
  D-022.

### D-028: Ends, middles, orientation; the ends of an L fixed
- **Kind:** FIXED (ANSWERS 2); spec-inferred for the geometry.
- **Sources:** a group is horizontal when "the objects in it seem to be arranged
  horizontally" [F95 p. 97]; groups may be "linear" [F95 p. 69]; "Tabletop cannot handle
  completely amorphous groups." [F95 p. 63]; of an L-shaped group "the program will choose
  only one as the upper end object" [F95 p. 63] where humans see two (SPEC.md 4.3, 4.4.5; 6b,
  fault 7).
- **Choice:** a group's axis is horizontal if its members' spread in x exceeds their spread
  in y, else vertical (a jolt to "horizontal" or "vertical" when a find-group's follow-up
  notices it); it is linear if every member lies within half a glass width of the axis line
  through the members' centroid, amorphous otherwise. The ends are found by
  find-end-objs: with `ends_from_chain` = True, the ends are the members with exactly one
  neighbour in the chain of D-027 once the chain is known, that is, once every member has
  been the argument of a find-neighbor (two ends for an L, whatever its shape); before
  that, and always for `french-faults`, the ends are the members with the extreme
  coordinates along the axis, one at each extreme, the first in member order among members
  tied there. The middles are the members that are not ends (as found, or as read now when
  find-middle-objs runs first, which p. 79 allows). Descriptors: "end" or "middle", and for
  a linear group with two ends "left" or "right" ("above", "below") for the lower and the
  higher end along the axis (D-029); an amorphous group's ends get no side word. Each new
  descriptor jolts its node and the member's type node [F95 p. 63] (`jolt_description`,
  D-016); the orientation is a descriptor of the group and jolts "horizontal" or
  "vertical" (`describe.notice_orientation`).
- **Reason:** French's words give the axis reading and name its failure on an L; the chain's
  extremities are the simplest definition that gives both arms an end.
- **Fault:** `ends_from_chain` = False keeps the axis reading throughout (`french-faults`).
- **Knob:** `Config.ends_from_chain` (True).
- **Alternatives:** ends by convex hull; no ends for amorphous groups.
  As built (item 32, a note): the frame words a group gives its members (`end`, `middle`,
  `left`, `right`, `above`, `below`) stay in the members' description lists when the group
  is removed; salience and the slippages read the words of live groups only
  (`salience.position_words` over `Group.positions`), so nothing in a run depends on the
  stale words, but the snapshot's `descriptions` show them (docs/engine-api.md).

### D-029: Two frames of position
- **Kind:** spec-inferred.
- **Sources:** position descriptors are "the descriptions of the positions within their
  respective groups of the objects making up the correspondences" [F95 p. 68]; across the
  table the same words name the sides, "where the terms 'left' and 'right' refer to the sides
  of the table as a whole" [F95 p. 85] (SPEC.md 4.3).
- **Choice:** every item has a table description, its region (D-004: the column gives
  `left`, `middle` or `right`; the row `above` for Henry's row, `middle`, or `below` for
  Eliza's), drawn afresh each time it is read (`describe.table_frame`; a group by its
  centroid), so it is never stored; an object in a group also has a group description (end,
  middle, left, right, above, below) from D-028, kept on the group (`Group.positions`,
  `describe.group_frame`), with the same word attached to the object in the Workspace for
  salience (D-031). The count descriptor ("one" for an object, the group's size as one, two,
  three or many [F95 p. 68]) is attached by `describe.describe_count`. A correspondence between objects in two mapped groups uses the group frame; one
  between objects or groups at the top level uses the table frame. Both are in the figure's
  frame, Henry at the top, left being the left of the page.
- **Reason:** the two sentences are the two frames; French's examples of mirror and diagonal
  symmetry (pp. 84-85) use the table frame at the top level and the group frame inside cp1.
- **Knob:** none.
- **Alternatives:** Eliza's frame for her side (a left-right flip), which the sources never
  suggest.
  As revised (loop0007, item 04): the table frame is read at last: `describe.table_frame`
  reads `describe.seen_region` (one blur draw) and `correspondences.placement_slippages`
  (D-082) gives every top-level correspondence across a perceived midline (D-084) its
  table-frame slippage, factor 4; the group frame still rules inside a group correspondence.
  - **As revised (loop0007, item 07):** the table frame is read as roles: `describe.row_role`
    and `describe.column_role` (D-085) read `table_frame` for an item in view and tag it ROW
    (`above`, `below`; once a midline is perceived) and COLUMN (`left`, `middle`, `right`);
    the re-cast (D-093) and the placement (D-082) read the tags.

### D-030: Which slippages feed which symmetry
- **Kind:** spec-inferred.
- **Sources:** a left-right slippage's value depends on "diagonal symmetry" and "opposite"
  [F95 p. 68]; right-right identities send activation to "mirror symmetry" [F95 p. 85]; the
  pop puts activation into "right", "left" and "diagonal symmetry" for a right-left slippage
  and into "right" twice and "mirror symmetry" for a right-right one [F95 pp. 88-89] (SPEC.md
  3.7, 3.10).
- **Choice:** when a correspondence enters the Worldview, each of its supporting slippages
  jolts (D-016, `jolt_label`) its label node: a horizontal flip (left-right, right-left) and a
  vertical flip (above-below) jolt "opposite" and "diagonal symmetry"; an identity
  (left-left, right-right, above-above, below-below, end-end, middle-middle) jolts "mirror
  symmetry" and "sameness"; a type slippage jolts the label of the link it crosses (used
  with, similar shape to, smaller than) or the category shared (an ISA slippage jolts the
  category); a count slippage jolts "number". A left-right slippage's static points (D-036)
  are scaled by the mean activation of "opposite" and "diagonal symmetry" over 150, an
  identity's by "mirror symmetry" over 150.
- **Reason:** French's three passages; the vertical case is read by symmetry with the
  horizontal one.
- **Knob:** none.
- **Alternatives:** feeding "symmetry" too.
  As built (`correspondences.py`, item 17): every slippage is a `Slippage(kind, a, b,
  labels)`, the `labels` being the nodes it will feed: a position flip (left-right,
  above-below) "opposite" and "diagonal symmetry"; a position identity, and an end-end or
  middle-middle identity, "mirror symmetry" and "sameness"; an end-middle slippage its link's
  label, "position"; a type identity "sameness", a type slippage the labels of its link or
  the first category the two types share (none when no link or category joins them); a count
  identity "sameness", a count slippage "number"; a shared-ISA or orientation identity
  "sameness", a flip nothing (D-009 has no horizontal-vertical link). The kind slippage
  (object-object, group-group, group-object) feeds nothing: "group" and "isolated" are read
  by the static factor, not jolted. The jolts themselves are sent on entry into the Worldview
  (item 18). `symmetry_scale` is the mean activation of a position slippage's label nodes over
  150, as the choice says. The jolts are sent by `worldview.entry_jolts` (item 18): on every
  entry into the Worldview, `jolt_label` to each label node of each slippage, in slippage
  order; a correspondence that leaves and re-enters jolts again.
  As built (item 32, a note): `Slippage` is a frozen dataclass compared by value (unlike
  the structures, which compare by identity): two slippages of one kind between the same
  two concepts with the same labels are the same slippage, which is what the descriptors'
  uniqueness and the parallelism test of D-043 mean by it.

### D-031: Salience weights
- **Kind:** simplest.
- **Sources:** French's ten factors [F95 pp. 66-67] (type activation; proximity to the
  touched object; extremal position unless "middle" is more active than "end"; the preferred
  regions; being a group; being in a Worldview correspondence; wins and losses; for groups,
  sameness, a common superordinate, size), "summed to determine the salience of the object"
  [F95 p. 67]; FCCA's eight [FCCA p. 385] (SPEC.md 4.5). No weight is given.
- **Choice:** salience, a number on about 0-150, is the sum of: 0.3 x the activation of the
  item's type node (a group: its kind node, "group"); the proximity term (D-033, up to 30);
  10 if the object is an end of its group, or 10 if it is a middle and "middle" is more
  active than "end"; the region term (D-032, up to 40); `group_salience_points` for being a
  group (D-046); 15 if the item is an end of a Worldview correspondence; +5 per competition
  won and -5 per competition lost by a correspondence it was part of; for groups, 10 for a
  sameness group, 5 for a shared-ISA group, and 2 per member. The touched object's salience
  is computed the same way (its proximity term is the maximum). Salience is recomputed when
  read.
  As built (`salience.py`, item 15): `terms` gives the eight contributions in French's
  order (type, proximity, position, region, group, worldview, competitions, composition)
  and `salience` their sum, never below a floor of 1, so that at a temperature of 100 an
  item of salience 0 is not shut out of a choice that p. 93 says is uniform. Reading a
  salience draws nothing from the generator: an object's region term uses the region the
  Workspace recorded at its last look (D-004), not a fresh draw, and an object not yet
  noticed is read in its exact region; a group's region is its centroid's exact region. The
  position term reads the group frame (D-029) of every group the object belongs to. The
  override of ends by middles follows French's "much more active" [F95 p. 66] and not the
  plan's "more active": a group's middles outnumber its ends, so the describing codelets'
  own jolts make "middle" more active than "end" in any row of five or more, while the
  chapter 4 dumps keep "end" at 99-132 against "middle" at 5-8 [F95 pp. 100-107 (img)];
  "middle" must be at least `middle_over_end_factor` = 2 times as active as "end"
  (`salience.middle_preferred`). A group's type term reads the "group" node; a gestalt
  group, whose kind has not been examined (D-024), takes the size points of the
  composition term but neither the sameness nor the shared-ISA points. The Worldview term
  reads `Workspace.in_worldview` (a correspondence with `in_worldview` set and the item among
  its `ends`) and the competition term `Workspace.competitions_of`, the tallies that
  `Workspace.record_competition` keeps; both are empty until the correspondence and
  Worldview items fill them, as "this factor contributes zero to all calculations of
  salience" [F95 p. 67] at the start. `salience_of(world)` is the function D-069's hook
  takes.
- **As revised (loop0008, item 04):** the dict is the configuration's own
  (`salience_weights_by_name`, a `cached_property` of `Config` built once per instance,
  not a field) and `salience.weight` reads it by name; item 01's cache keyed on the weight tuple hashed
  the nine pairs on every call. The same floats, every run unchanged (D-112).
- **Reason:** the factors are French's; the region and proximity terms are heaviest because
  French says they decide what is noticed first [F95 p. 66; F95 p. 97], and the type term is
  scaled so that a type at the ceiling counts as much as the best region.
- **Knob:** `Config.salience_weights`; `Config.middle_over_end_factor` (2.0).
- **Alternatives:** FCCA's conceptual proximity to the touched object as an eleventh term
  (left out: French's list lacks it, and spreading from the touched type gives it through the
  first term).
  - **As revised (loop0007, item 08):** a ninth term, `role`, follows French's eight in
    `salience.FACTORS` and `salience.terms`: consumer C7 of the roles (D-100), the points
    of the roles an item plays, and a scale on the region term for a stray. With no role
    tag standing the term is 0 and the scale 1, and the salience is as before.
- **As revised (loop0008, item 01):** `salience.weight` reads a dict built once per
  weight table (`weights_by_name`, cached on the frozen tuple `Config.salience_weights`)
  instead of rebuilding it on every term, and `is_group` reads the problem's ids listed
  once; the values are the same floats, every run unchanged (D-108).

### D-032: Region preference beyond the first two
- **Kind:** spec-inferred (the order), simplest (the values).
- **Sources:** "the most favored region is diagonally opposite the touched object, and the
  next most preferred region is directly opposite the touched object" [F95 p. 66]; the order
  continues "and so on" [F95 p. 93]; FCCA's caption says the diagonally opposite corner is
  preferred to the mirror-image corner [FCCA p. 394] (SPEC.md 4.5). Buridan's touched object
  is at the top centre.
- **Choice:** `region_preference`: diagonal 1.0, directly opposite 0.7, the touched object's
  own region and the regions adjacent to it 0.4, every other region 0.15, each times 40 in
  the salience sum (D-031). "Diagonal" is the region reached by reflecting the touched
  object's region through the table's centre, "opposite" the one reached by reflecting it
  across the middle row (`geometry.diagonal_region`, `opposite_region`); for a touched
  object in the middle column (Buridan's spoon at the top centre) they coincide, that
  region takes 1.0 and the two far corners take 0.7. A reflection that lands on the
  touched object's own region (a touched object in the middle row, or at the centre) is
  "near", not opposite to itself (`geometry.region_relation`). The points reflected the
  same two ways (`geometry.diagonal_point`, `opposite_point`) are kept for the GUI's
  overlays and for later decisions; no codelet uses them yet. A find-group codelet's
  choice of region (D-023) uses the same weights under D-060.
  As built (item 15): `salience.region_order` lists the nine regions from the most to the
  least preferred, reading order among equals, from the same table as the scouts'
  `perceive.region_preferences`; for Surround's corner cup it is lower-right, lower-left,
  the four regions around the touched one, then the three others; for Buridan's top-centre
  spoon lower-middle, then lower-left and lower-right, then the six others at 0.4 (every
  other region is adjacent to the upper middle); for Figure/Ground's bottom-centre fork
  upper-middle, then the two upper corners. The region term of salience is the weight (40)
  times the preference of the region the object was last seen in.
- **Reason:** the two named regions in their order; the rest at low values so that "and so
  on" never beats them; the middle-column case is the plan's reading of what "diagonally
  opposite" can mean there.
- **Knob:** `Config.region_preference`.
- **Alternatives:** a gradient by distance from the diagonal point; values from the human
  data of F95 p. 86 (not used: that is calibration).

### D-033: Proximity to the touched object
- **Kind:** simplest.
- **Sources:** "Objects close to the touched object (usually the most salient object on the
  table) are noticed preferentially because of this proximity." [F95 p. 66] No form is given.
- **Choice:** 30 x max(0, 1 - d / `proximity_reach`), d the distance between centres and
  `proximity_reach` = 40 units (about a third of the table), for objects and for groups
  (their centroid).
- **Reason:** the simplest term that is largest at the touched object and vanishes at a
  distance where "close" no longer applies.
- **Knob:** `Config.proximity_reach` (40.0); the 30 is in `salience_weights`.
- **Alternatives:** an exponential fall-off; the gap between footprints instead of the
  centres.
  As built (`salience.proximity_term`, item 15): on Surround 5 the touched cup's row
  neighbours take 17-24 of the 30 (fork1 at 18 units, knife1 at 8) and everything on
  Eliza's side 0; the touched object takes 30 and is the most salient object on the table,
  as "usually the most salient object on the table" [F95 p. 66] says.

### D-034: Temperature in salience choices
- **Kind:** spec-inferred.
- **Sources:** "The selection of objects or correspondences based on their salience works in
  the same way." [F95 p. 93], after the region example; the selection "approaches determinism
  as the temperature approaches 0, is completely unbiased (i.e., uniformly distributed
  random) at 100" [F95 p. 93].
- **Choice:** every choice by salience (pick-salient-obj, pick-2nd-salient-obj, pick-strong-cp,
  the seed of D-023, the arguments of search-for-same-group and search-for-similar-group) is
  a weighted choice under D-060 with the saliences as weights.
- **Reason:** French's sentence applies the codelets' law to these choices; one law.
- **Knob:** none.
- **Alternatives:** none that fit p. 93.
  As built (`salience.choose`, `choose_object`, `choose_group`, item 15): one weighted
  choice with the saliences raised to the exponent a(T) the codelet passes, one draw,
  candidates and exclusions given by the caller, None without a draw when nothing is left;
  `most_salient` is the item a choice at T = 0 almost surely makes. A measurement, not a
  tuning: with one cup at the centre of each region and the touched cup in the upper left,
  the diagonal cup (salience 54) against the directly opposite one (42) is chosen 72 percent
  of the time at a = 6 and the six others under 6 percent each; French's "essentially
  deterministic" [F95 p. 93] 0.985 is for urgencies 6 : 3 : 1, a ratio of two, where the
  sixth power separates far more. At a = 0.1 every one of the eight has a chance between
  0.10 and 0.14.

## Correspondences and the Worldview (D-035 to D-046)

### D-035: Creation conditions
- **Kind:** spec-inferred.
- **Sources:** "A correspondence will not even be created unless certain not-too-stringent
  conditions are met" [F95 p. 71], for example "the objects in question must not be in the
  same group" [F95 p. 72]; an object may map to a group [F95 p. 71] (SPEC.md 4.6.4).
- **Choice:** create-cp builds a correspondence between two items when: they are distinct;
  neither contains the other; they are not members of one gestalt group; neither is the
  touched object mapped to itself; and no correspondence between the same two items exists
  (then the codelet fizzles, or re-tests the existing one if it is not in the Worldview).
  Object-object, group-group and group-object pairs are all allowed; the last carries D-045's
  penalty.
- **Reason:** French's one example and the Worldview's definitions.
- **Knob:** none.
- **Alternatives:** requiring opposite sides (D-005 says no).
  As built (`correspondences.creatable`, `create`, item 17): the conditions, in order, are
  distinct items; both in view (a noticed object or a group built); neither contains the
  other (a group containing the object, or a different group all of whose members are its
  own); no group holds both (so two objects of one row, an object and a subgroup of its row,
  and two subgroups of one row are all refused; "the objects in question must not be in the
  same group" [F95 p. 72] read for any group, gestalt or bottom-up); and no correspondence
  between the two exists in either order. A refused pair gives the reason as a word for the
  trace (`same item`, `not in view`, `one contains the other`, `in one group`, `exists`). A
  correspondence is recorded with its slippages and points at once; `parent` names the
  group-group correspondence it is built inside, when examine-cp creates it [F95 p. 108],
  and then both ends must lie inside the parent's groups. `inner_pairs` lists the pairs
  examine-cp may create, in member order.

### D-036: Static points
- **Kind:** simplest.
- **Sources:** the nine static factors [F95 pp. 68-69]: the ends' salience; group-group or
  object-object favoured over group-object, the former when "group" is active, the latter
  when "isolated" is; end-end and middle-middle; position slippages weighted by the
  symmetry nodes; type closeness in the Slipnet; the count slippage; ISA closeness for two
  shared-ISA groups; the same object types in both groups; orientation for two linear groups
  (SPEC.md 4.6.2). No points are given; the type weight is the one French raised for the
  human comparison [F95 pp. 142-143].
- **Choice:** a sum on about 0-100: 0.2 x the mean salience of the two ends; kind: 15 for
  object-object or group-group, plus 5 for group-group when "group" is above 100 and 5 for
  object-object when "isolated" is; for group-object, -`group_object_penalty` (D-045); 10 for
  end-end or middle-middle; 10 x (the supporting symmetry's activation / 150) for a position
  slippage or identity (D-030); `type_weight` x (1 - d / 100) for the ends' type distance d
  (D-012), 0 beyond 100, with `type_weight` = 30; 10 x (1 - d / 100) for the count slippage's
  distance; 10 x (1 - d / 100) between two shared-ISA groups' categories; 10 for the same
  multiset of types; 5 for the same orientation, 2 for horizontal-vertical. Points are
  recomputed when the correspondence is read.
- **Reason:** the type term is heaviest because French singles it out as the weight that
  moved his program towards his subjects; the others are the plan's plain values, 10 for a
  factor that is present or absent and a scaled 10 for a distance.
- **Knob:** `Config.cp_static_weights`; `Config.type_weight` (30), the knob of the `human`
  profile (ANSWERS.md 7), fitted to 50 in item 26 (D-076).
- **Alternatives:** Copycat's product of slippage strengths; weighting by depth.
  As built (`correspondences.static_terms`, item 17): the nine terms in French's order,
  one function each, summed by `static_points` and kept on the correspondence by
  `evaluate`. Three readings the choice left open: the "active" of factor 2 is at or above
  `top_down_threshold` (100), the Slipnet's own notion of active (D-020); factor 5 uses an
  object's type node and a sameness group's one type (so that a spoon-spoon group may map
  to a spoon on its type [F95 p. 111]), and gives 0 for any other group; factor 4 sums the
  axes on which both objects carry a word (at most left/right and above/below, so at most
  twice the points), and the closeness 1 - d / 100 uses the Slipnet's current distance
  (D-012), which the label nodes shrink. On the digitized central configuration a cup-cup
  pair scores about 68 at the start and the row-to-isolated-cup pair about 2.
  As revised (loop0007, item 04): factor 4 (`position_points`) sums over the group-frame
  position slippages and the placement slippage of D-082, both with the `position` weight
  and the symmetry scale; `static_terms` keeps its nine names.
  As revised (loop0007, item 05): a tenth static factor follows French's nine in
  `static_terms`, consumer C5's role factor (D-098); `static_points` is the sum of the ten.

### D-037: Dynamic points
- **Kind:** simplest.
- **Sources:** a Hobj correspondence "receives points" [F95 p. 69]; a neighbour of the Hobj
  correspondence more than a neighbour of another Worldview correspondence [F95 p. 69]; two
  correspondences are neighbours when their ends are neighbours [F95 p. 69]; FCCA's weakening
  by a very different neighbour [FCCA p. 382] has no counterpart in the book (SPEC.md 4.6.3).
- **Choice:** +15 for containing the touched object; +10 for being a neighbour (both ends
  neighbours, D-027, of the other's ends) of the Worldview's Hobj correspondence; +5 for
  being a neighbour of any other Worldview correspondence (once). No weakening term.
- **Reason:** the three factors in French's order of worth; FCCA's weakening is left out
  because the book, the specification, does not have it.
- **Knob:** `Config.cp_dynamic_points` ((hobj 15), (hobj_neighbor 10), (worldview_neighbor
  5)).
- **Alternatives:** FCCA's weakening (-5 for a neighbour with a different slippage).
  As built (`correspondences.dynamic_terms`, item 17): all three terms are 0 while the
  Worldview holds no correspondence other than the one rated, read from "The dynamic part
  of the evaluation of a correspondence can be done only when there are other
  correspondences in the Worldview." [F95 p. 69]; a correspondence contains the touched
  object when it is an end or a member of a group at an end; the Hobj correspondence is the
  first Worldview correspondence that contains it; two items are neighbours when a member
  of one is recorded as a neighbour of a member of the other in some group (D-027), and two
  correspondences when both their first ends and their second ends are neighbours.
- **As revised (v5, D-116):** the Hobj points count in every rating, a correspondence alone
  in a Rival Worldview included; only the two neighbour terms wait for another Worldview
  correspondence.

### D-038: The bonus and the penalty
- **Kind:** simplest.
- **Sources:** "The bonus is awarded each time the correspondence wins a competition to get
  into (or to remain in) the Worldview. Correspondences that lose competitions lose points."
  [F95 p. 68]; after a fight "the rating of each of the correspondences in that Worldview is
  improved and the rating of the losing correspondence is lowered" [F95 p. 72]; the objects
  at the ends gain or lose salience [F95 p. 66] (SPEC.md 4.6.3, 4.7.3).
- **Choice:** after every competition, every correspondence of the winning Worldview gains
  `win_bonus` = 5 and every correspondence of the losing side loses `loss_penalty` = 8; the
  objects at their ends record the win or loss for D-031. The bonus accumulates without a cap;
  the rating is clipped at 0 below.
- **Reason:** representational inertia, with the penalty a little larger than the bonus so
  that a repeated loser fades to the removal threshold (D-041) within a few fights.
- **Knob:** `Config.win_bonus` (5), `Config.loss_penalty` (8).
- **Alternatives:** equal amounts; a bonus proportional to the margin.
  As built (`correspondences.award`, item 17): the bonus is a field accumulated on the
  correspondence (never recomputed by `evaluate`); each award also counts a win or a loss
  on the correspondence and on its two ends in the Workspace (D-031, factor 7). The
  rating is `max(0, static + dynamic + bonus)`. `worldview.compete` (item 18) calls `award`
  with `True` for every correspondence of the winning Worldview and with `False` for each
  loser: the newcomer when the Old Worldview wins, the rivals when the Rival wins. An
  uncontested entry awards nothing, since no competition was held.
- **As revised (v5, D-117):** the bonus and the penalty are awarded once per correspondence,
  outcome and rivalry (`Workspace.awarded`), not at every rematch; the wins and losses are
  counted every time.

### D-039: The test stage
- **Kind:** spec-inferred.
- **Sources:** "the outcome of this evaluation is a probabilistic one: there is no rigid
  evaluation threshold above which the correspondence passes and below which it fails" [F95
  p. 72]; the program is seen "testing its quality" [F95 p. 101] (SPEC.md 4.6.4).
- **Choice:** build-cp-descrip computes the correspondence's static points (D-036), writes
  its slippages, and passes it with chance (static points / 100), clipped to [0.01, 0.99],
  under D-060; on passing it posts cp-compete, one generation on; on failing the
  correspondence stays in the Workspace as a tentative structure that pick-strong-cp may
  test again.
- **Reason:** a chance proportional to strength is the simplest probabilistic test with no
  threshold; D-060 makes it deterministic at low temperature, as every choice is.
- **Knob:** none.
- **Alternatives:** a chance from the full rating including dynamic points.
  As built (`correspondences.describe`, `test`, item 17): `describe` rewrites the
  slippages from the ends' descriptors as they now stand (a find-end-objs may have run since
  creation), attaches them to the correspondence as descriptors (`left-right`, `cup-cup`,
  `level:top`), and recomputes the points; `test` draws one coin at the static points over
  100, clipped to [0.01, 0.99], through `temperature.passes` at the exponent the codelet
  passes, records `chance`, `passed` and `tested` on the correspondence, and leaves a failed
  one in the Workspace. The posting of cp-compete is the codelet's (item 19).
  As built (item 24; `builders.create_cp`, `build_cp_descrip`, `judges.cp_compete`;
  `Config.failed_test_removes` = True): the three stages are now three codelets in the order
  of p. 71, "correspondence creation, initial testing, and competition": create-cp creates
  the correspondence and runs the test at once, "Once created, the correspondence will be
  evaluated" [F95 p. 72], posting cp-compete on a pass; cp-compete fights or enters; and
  build-cp-descrip writes the descriptors of a correspondence that has entered, posted by
  cp-compete as build-obj-desc is posted for a built group (D-051's reading of the legend's
  two "build ... desc" labels). A correspondence that fails its test is removed from the
  Workspace, as a proposed structure that fails is in the Copycat lineage (SPEC.md 2.1), so
  that a later pick may propose the pair again; `failed_test_removes` = False restores item
  17's reading, in which a failed one stayed tentative for a pick-strong-cp to test again.
  The reasons, from the development evidence (item 24): under item 17's reading a failed
  correspondence stayed in the Workspace, every create-cp for the pair fizzled on "exists",
  and nothing retested it (pick-strong-cp is posted only on a rise of "opposite", D-020),
  so the touched cup's correspondence was in limbo in a third of the runs on every chapter 2
  table, and the first entry into the Worldview came at 35-100 codelets where the dumps show
  it at 22 [F95 p. 100 (img)]; and build-cp-descrip, posted by create-cp, ran on
  correspondences already in the Worldview and marked them failed. Nothing was read from any
  result set.

### D-040: The winner of a competition
- **Kind:** spec-inferred.
- **Sources:** the fight is between the Old Worldview and the Rival Worldview, "The
  probabilistic winner of this competition between the Rival Worldview and the Old Worldview
  is then installed as the new Worldview." [F95 p. 72]; "the greater the discrepancy between
  their strengths, the more likely the stronger one is to win the fight" [FCCA p. 382]; a
  weaker correspondence can win on coherence [F95 p. 105] (SPEC.md 4.7.3).
- **Choice:** cp-compete builds the Rival Worldview (the Old one less every rival of the
  newcomer, D-044, plus the newcomer), rates both (D-042, D-043), and the rival wins with
  probability R^a / (R^a + O^a), a = a(T) (D-060's two-outcome law with the ratings as
  weights; ratings are floored at 1). A newcomer with no rival enters without a fight, as
  p. 101 says, and the entry still counts as a Worldview change for the temperature (D-048).
  The same rule, with strengths for ratings, decides fights between groups (D-025).
- **Reason:** the ratings of two whole Worldviews, French's; the law of D-060 gives FCCA's
  shape and the determinism at T = 0 that p. 93 asks of every choice.
- **Knob:** none.
- **Alternatives:** a fixed exponent (3) regardless of temperature.
  As built (`worldview.py`, item 18): the Worldview is the subset of the Workspace's
  correspondences whose `in_worldview` flag is set; there is no separate structure. `compete`
  fizzles (None) when the newcomer is gone from the Workspace or already in; with no rival
  (`rivals_of`, D-044) it calls `admit`, which sets the flag, sends the entry jolts (D-030)
  and logs a `Change` with reason `no rival` and, for the record, a `Competition` with
  `chance` 1 and no draw. Otherwise `rate_subjunctively` flags the rivals out and the
  newcomer in, rates that Rival Worldview (the strengths recomputed under the flags, since
  the dynamic and salience terms read them), and restores the flags and the strengths;
  `rival_chance` is `temperature.pass_probability(R / (R + O), a)`, which is R^a / (R^a + O^a)
  with both ratings floored at 1, and one coin decides. On a rival win the rivals are expelled,
  the newcomer admitted with its jolts, every Worldview correspondence awarded a win and each
  rival a loss, and a `Change` logged with reason `won`; on an old win every Worldview
  correspondence is awarded a win and the newcomer a loss, and nothing is logged as a change.
  Both logs live on the Workspace (`competitions`, `changes`) for the temperature's windows
  (D-048). `challengers` is what a check-worldview codelet reads: every strength recomputed,
  then the tested correspondences outside the Worldview that outrate every rival they have,
  or have none.

### D-041: Removal from the Workspace
- **Kind:** screen-dump-inferred (the owner), simplest (the values).
- **Sources:** a loser "remains nonetheless in the Workspace with the potential to compete
  again" [F95 p. 72]; "if the value of a correspondence in Tabletop drops below a certain
  threshold (for example, because it has lost a number of competitions), it is
  (probabilistically) removed from the Workspace and then must be built again" [F95 p. 73];
  codelets holding a removed structure fizzle [F95 p. 73]; the strip has a clean-up-corresp.
  label [F95 p. 96 (img)]; Copycat kills losers [F95 p. 70] (SPEC.md 4.7.4).
- **Choice:** a clean-up-corresp. codelet (posted after every competition, generation 1) looks
  at every correspondence in the Workspace but not in the Worldview whose rating is below
  `removal_threshold` = 20 and removes each with probability 1 - rating / 20 (one coin each,
  in Workspace order; not softened by temperature). Codelets whose argument was removed
  fizzle. Groups are never removed by rating (only absorbed, D-026, or beaten, D-025). The
  lesion `losers_destroyed` = True (FCCA criterion 11 [FCCA p. 365]) removes a loser at once.
- **Reason:** the sentence, with the one codelet label that fits; a linear chance below the
  threshold is the simplest "probabilistically".
- **Knob:** `Config.removal_threshold` (20), `Config.losers_destroyed` (False).
- **Alternatives:** removal by age (French says it is not implemented in Copycat and does not
  claim it for Tabletop).
  As built (`worldview.clean_up`, `remove`, item 18): the candidates are the correspondences
  outside the Worldview in the order built; `removal_chance` is 1 − rating / 20 below the
  threshold (the rating recomputed first) and 0 at or above it; `clean_up` draws one coin per
  candidate with a chance strictly between 0 and 1 and removes without a draw at 1 (a rating
  of 0). `remove` takes the correspondences built inside the removed one with it [F95 p. 108]
  (their level no longer exists), expels any that were in the Worldview first and logs that
  as a `Change` with reason `removed`; names are never reused. `alive` tells a codelet holding
  a removed correspondence to fizzle. Under `losers_destroyed` `compete` removes the losing
  side at once after the awards.
- **As built (item 25):** the run loop drops every correspondence whose group end has
  left the Workspace in one sweep (`worldview.remove_all`): all of them are expelled
  from the Worldview before any is removed, so that none is rated with an end that is
  gone (a `temperature-high` lesion run on the chapter 4 configuration crashed when two
  correspondences were orphaned by one lost group and the first's removal rated the
  second). One `CorrespondenceRemoved` per correspondence, one change for all; a single
  orphan gives exactly the events it gave before.

### D-042: Coherence: penalty and reward
- **Kind:** spec-inferred (the form), simplest (the values); the `buridan` profile calibrates
  the penalty (ANSWERS.md 7).
- **Sources:** the rating of a Worldview sums the correspondences' strengths, the degree of
  coherence and the degree of parallelism [F95 p. 74]; incoherence is "two or more
  correspondences emanating from within a single group (that is, from different items inside
  the group) but terminating in unrelated spots on the table" [FCCA p. 387]; the reward when
  correspondences link "two groups together on an element-by-element basis" [FCCA p. 387];
  the program is "quite heavily biased against incoherent Worldviews" [F95 p. 105] yet the
  penalty "is not severe enough to prevent the program from making them" in Buridan 1 [F95
  p. 130], so French raised it for Buridan 2-13 (SPEC.md 4.7.5). FCCA sums all structures,
  French only the correspondences.
- **Choice:** rating = sum of the Worldview's correspondence strengths (static + dynamic +
  bonus, D-036 to D-038) - `incoherence_penalty` x (the number of incoherent pairs) +
  `coherence_reward` x (the number of coherent pairs) + the parallelism points (D-043). Two
  object-level correspondences are a pair when their first ends lie in one gestalt group;
  the pair is coherent when their second ends lie in one group too, incoherent when they do
  not (one in a group and one on an isolated object, or in two groups); the same for the
  second ends. `incoherence_penalty` = 40, `coherence_reward` = 10. Group strengths are not
  summed (French's reading); D-062 reports this rating.
- **Reason:** FCCA's definition of incoherence is the one French's example of three groups
  illustrates; 40 is the plan's a-priori value, a penalty of about one strong correspondence
  per incoherent pair, which is "heavy" and still beatable by two strong correspondences, as
  Blockage 3 and 4 need [F95 pp. 124-125].
- **Knob:** `Config.incoherence_penalty` (40; the `buridan` profile's knob, fitted in item 26
  to Buridan 1 modified and left at 40, since the scan was flat: D-076), `Config.coherence_reward`
  (10).
- **Alternatives:** FCCA's sum over all structures; a penalty proportional to the
  correspondences' strengths.
  As built (`worldview.rate`, `pair_status`, item 18): a `Rating` carries the strengths' sum,
  the coherent, incoherent and parallel pairs as (name, name) tuples, the coherence points
  (reward times the coherent pairs less the penalty times the incoherent ones), the
  parallelism points and the total. Pairs are read among the **top-level** correspondences
  only: one built inside a group correspondence [F95 p. 108] is counted through its parent,
  so the cup-cup correspondence beside a spoon-group correspondence with two inner
  correspondences is one incoherent pair, not three. "Lie in one gestalt group" is read with
  `holds`: an object is inside a gestalt group when it is a member, a group when it is the
  gestalt group itself or lies inside it; so French's chapter 4 case, a group-group
  correspondence (the spoon subgroups) beside the cup to the isolated cup, is incoherent as
  p. 101 says, and beside the cup-glass correspondence coherent. Since the ends follow the
  picking order (D-005), both alignments of two correspondences' ends are tried and the better
  is taken (coherent over incoherent over no pair). Two correspondences with no gestalt group
  in common on either side are no pair, which leaves French's "separate subworlds" [F95
  p. 75] without interaction. The bare `coherent` of a `Rating` means no incoherent pair.

### D-043: Parallelism within a level
- **Kind:** spec-inferred.
- **Sources:** "One correspondence is said to be parallel to another if the slippages that
  support the first have the same labels as the slippages that support the second." [F95
  p. 74]; FCCA's reward "when two or more correspondences are supported by the same type of
  conceptual slippage" [FCCA p. 387]; subworlds and levels complicate it [F95 p. 75] (SPEC.md
  4.7.5).
- **Choice:** `parallel_points` = 5 for every pair of Worldview correspondences at the same
  level (both at the top level, or both inside the same group correspondence) that share a
  slippage label; levels do not mix; subworlds (physically separate sets) are not
  distinguished.
- **Reason:** French's definition, applied per level as his p. 75 question suggests.
- **Knob:** `Config.parallel_points` (5).
- **Alternatives:** parallelism across levels; counting per label rather than per pair.
  As built (`worldview.parallel`, item 18): two correspondences are parallel when they are
  at the same level (`Correspondence.level`) and some slippage of one shares a label with some
  slippage of the other, the count identity ("one"-"one", "two"-"two") left out because
  every correspondence between two single objects carries it, so it says nothing about "the
  same type of conceptual slippage" [FCCA p. 387]. Identities count otherwise: two type
  identities (cup-cup, fork-fork) are parallel through "sameness", two flips (left-right,
  right-left) through "opposite" and "diagonal symmetry", as p. 75's example says. The
  alternative of counting non-identity slippages only is recorded; it would make the chapter
  4 run's cup-cup and spoon-group correspondences not parallel.

### D-044: Usurping: inside-only when the group correspondence stands
- **Kind:** spec-inferred.
- **Sources:** when two groups are mapped as wholes "the individual objects inside A can map
  only to objects inside B and not to some other object elsewhere on the table" [F95 p. 71];
  in Blockage 3 the two grouped glasses are mapped separately when the group itself is not
  mapped [F95 p. 124] (SPEC.md 4.7.1, 4.7.2).
- **Choice:** the rivals of a newcomer are the Worldview correspondences that share an end
  with it; and, when a group-group correspondence is in the Worldview, every object-level
  correspondence with one end inside the pair and the other outside is a rival of it (in both
  directions: a newcomer that would reach outside fights the group correspondence). When no
  group correspondence stands, members of a group map freely.
- **Reason:** the rule and the example.
- **Knob:** none.
- **Alternatives:** forbidding such correspondences outright (no fight).
  As built (`worldview.conflicts`, `rivals_of`, item 18): two correspondences conflict when
  they share an end, or when one maps two groups as wholes and the other `reaches_outside`:
  one end inside one of the two groups (a member, or a group inside it, not the group itself)
  and the other end not inside the other group. The two groups are taken in either order
  (D-005). A correspondence inside the group correspondence (one end inside each group), with
  or without `parent` set, is no rival of it; a correspondence from a member to the whole of
  the other group shares that end and is. `violations` lists every conflicting pair among the
  Worldview's correspondences; the engine keeps it empty, and the tests check it after every
  competition of a random sequence.

### D-045: The group-to-object penalty, relaxed
- **Kind:** FIXED (ANSWERS 2).
- **Sources:** SPEC.md 6b (fault 2). "Tabletop favors group-to-group mappings and abhors
  group-to-isolated-object mappings." [F95 p. 125]; "This strong bias against
  group-to-isolated-object mappings should probably be relaxed." [F95 p. 125]; the program
  "strongly resists mapping groups onto individual objects, although under some circumstances
  it might be happy mapping a spoon-spoon group to an individual spoon" [F95 p. 111].
- **Choice:** `group_object_penalty` = 15 static points off a group-object correspondence
  (D-036), half the faulty value.
- **Reason:** "relaxed", not removed: halving is the plainest reading of a relaxation that
  keeps the preference French defends on p. 111.
- **Fault:** `group_object_penalty` = 30, the plan's reading of "abhors" (`french-faults`).
- **Knob:** `Config.group_object_penalty` (15).
- **Alternatives:** 20 (a third off); 0 (removed, which French does not ask for).
  As built (`correspondences.kind_points`, item 17): a group-object pair gets
  -`group_object_penalty` as its whole factor 2, in place of the 15 (+5) of a same-kind
  pair, so the gap between the kinds is 30 to 35 points under `standard` and 45 to 50 under
  `french-faults`.

### D-046: The group-level preference, weakened
- **Kind:** FIXED (ANSWERS 2).
- **Sources:** SPEC.md 6b (fault 3). "when there are groups of objects on the table, the
  program is strongly biased to hunt for group-level correspondences in preference to
  object-level correspondences. Thus, this bias is clearly a bit too strong." [F95 p. 109];
  groups "are given salience points simply for being groups, which constitutes a pressure in
  favor of looking at them over simple objects" [F95 p. 66].
- **Choice:** the hunt is the salience's doing (pick-salient codelets choose what to map by
  salience, D-031, D-034), so the bias is the points for being a group:
  `group_salience_points` = 10, a third less than the faulty value. The static kind points of
  D-036 (group-group favoured when "group" is active) are not the hunt and stay.
- **Reason:** "a bit too strong" asks for a small reduction, not a removal; a third is a bit.
- **Fault:** `group_salience_points` = 15, the plan's value (`french-faults`).
- **Knob:** `Config.group_salience_points` (10).
- **Alternatives:** lowering the raw urgency of search-for-same-group and
  search-for-similar-group (D-052) instead.
  As built (`salience.group_term`, item 15): the points are one term of the sum, 10 under
  `standard` and 15 under `french-faults`; a group also carries the composition points of
  D-031 and its type term reads the "group" node, so that on Henry's row the sameness group
  of the two forks outranks either fork.

## Temperature (D-047 to D-050)

### D-047: The temperature coefficients
- **Kind:** simplest.
- **Sources:** T = alpha x DeltaQ / Q + beta x Delta_WV - gamma x T_cp, with alpha, beta and
  gamma positive constants chosen so that the last two terms "play a less significant role
  than" DeltaQ / Q, "which is the dominant factor" [F95 p. 91 (img)]; the chapter 4 narrative
  of each step (SPEC.md 6.2, 6.3, 6c.8). No value is given.
- **Choice:** `temperature_alpha` = 600, `temperature_beta` = 4, `temperature_gamma` = 0.5.
  With Q in (0, 1] and DeltaQ the running average of D-048, into which each change enters
  with the weight 1/6, the first term runs from 0 to 100 (a change of the whole of Q, 1.0,
  gives DeltaQ = 1/6 and the term 100); ten changes in the last ten competitions add 40;
  each competition takes half a degree off. `temperature_fixed` >= 0 clamps T to that value for the whole run
  (the lesion of FCCA criterion 11 [FCCA p. 365]; -1 is off).
- **Reason:** the first term alone must span the thermometer, so alpha is 100 times the
  weight's inverse (the plan's 100 assumed DeltaQ itself ran to 1, which the averaging of
  D-048 never lets it do; item 19 made the two consistent); beta and gamma are small enough that the other terms never dominate it over a run of 50-200
  codelets, which is what the page asks, and large enough to be visible (a run's ten
  oscillations add 40; sixty competitions take 30 off). Nothing here was fitted to the
  dumps' readings; their path (80, 59, 43, 31, 36, 14, 2) is a target of the ch4 bench.
- **Knob:** `Config.temperature_alpha` (600.0), `Config.temperature_beta` (4.0),
  `Config.temperature_gamma` (0.5), `Config.temperature_fixed` (-1.0).
- **Alternatives:** a smaller alpha with no clamp; gamma = 0 (no tiring).
  As built (`temperature.Temperature`, item 19): `formula(q, dq, dwv, tcp)` is the equation
  clamped to [0, 100] with Q floored at `q_floor`; `update` applies it at every
  recomputation of D-050. A finding, not a tuning: on the digitized central configuration
  the first correspondence to enter (a cup-cup pair, Q about 0.67 from the floor of 0.05)
  sends T from 80 to about 96, where the choice of codelets is nearly uniform (a = 0.3),
  whereas the chapter 4 thermometer went from 80 to 59 at that moment [F95 p. 100 (img)];
  with the plan's alpha of 100 it would go to about 20 and the run would end at once with
  that correspondence. Neither value was fitted; the ch4 bench (item 23) reports the path,
  and the reading of the first term is the decision to revisit if the path is wrong.

### D-048: Delta-Q: absolute, over the last six updates
- **Kind:** simplest.
- **Sources:** the rate of change "is determined by the difference between the quality rating
  of the current Worldview and the rating of the previous Worldview" and "is averaged in with
  a certain number of past rates of change (generally averaged over the last six)" [F95
  p. 90]; "If no additional structure is found, even if Q is high, the value of ΔQ will
  decrease over time" [F95 p. 92 (img)]; Delta_WV "is the number of changes in the Worldview
  in the last ten competitons" [F95 p. 91]; the thermometer is empty at 0 and 12 codelets
  [F95 p. 96 (img); F95 p. 99 (img)] (SPEC.md 6.2, 6.3, 6c.8).
- **Choice:** at each temperature update (D-050) the rate is |Q_now - Q_previous update|, 0
  when nothing changed; DeltaQ is the running average into which each rate is "averaged
  in" with the weight 1 / `dq_window` = 1/6: DeltaQ <- DeltaQ + (rate - DeltaQ) / 6, so
  that a change raises DeltaQ by a sixth of itself and six quiet updates leave about a
  third of it. `dq_signed` = False: absolute values, so an improvement counts as change like a
  loss. Delta_WV is the number of Worldview changes (entries, removals, demotions) since the
  competition `wv_window` = 10 competitions back (all changes so far while fewer than ten
  competitions have been held). The thermometer's emptiness in two dumps is read as a
  display matter, not as T being undefined: T is defined from the start (D-049).
- **Reason:** averaging over updates rather than over changes is what makes DeltaQ "decrease
  over time" when nothing is found, as p. 92 says it must; "averaged in with" [F95 p. 90]
  is read as a running average rather than a window mean because a window mean holds a
  change at full weight for six updates and then drops it at once, a plateau and a cliff,
  whereas the chapter 4 thermometer falls gradually, 59.0, 43.2, 31.3 over fourteen
  codelets while the Worldview "changed very little" [F95 p. 101; F95 pp. 100-103 (img)];
  signed differences would let a steady improvement lower T below the stable value, which
  no page describes.
- **Knob:** `Config.dq_signed` (False), `Config.dq_window` (6) [F95 p. 90], `Config.wv_window`
  (10) [F95 p. 91].
- **Alternatives:** a window over Worldview changes (the plan's first reading); the window
  mean (item 10's reading); signed differences.
  As built (`temperature.Temperature`, item 19): `rate`, `average_rate` and
  `changes_in_window` are the three readings, each a method; the `rates` list keeps every
  rate for the trace. French's narrative says twice that an improvement of the Worldview
  pushes T down, "the push to decrease from a very strong addition" and a fight whose
  winner "has improved so much" that T drops [F95 p. 105]; the absolute reading pushes T
  up at every change, as "the more often the Worldview changes, the higher the
  temperature" [F95 p. 91] says, and the signed reading (`dq_signed`) is the knob that
  tries the other. A finding for the ch4 bench, not a tuning.

### D-049: Q, the clamp and the start at 80
- **Kind:** screen-dump-inferred.
- **Sources:** Q is "measured as a fraction of an optimally good correspondence" [F95 p. 90],
  0 < Q <= 1 [F95 p. 91 (img)]; the thermometer runs 0-100 and the exponent a is defined for
  0-100 [F95 p. 92]; the thermometer reads 80.0 at two codelets, before any Worldview [F95
  p. 98 (img)] (SPEC.md 6.2, 6.3, 6c.8).
- **Choice:** Q = the mean rating (D-036 to D-038) of the Worldview's correspondences / 100,
  clipped to [`q_floor` = 0.05, 1]; an empty Worldview has Q = `q_floor`. T is clipped to
  [0, 100]. T = `initial_temperature` = 80 from the start until the first Worldview change,
  after which the formula applies at every update.
- **Reason:** the mean rating over 100 is the plainest "fraction of an optimally good
  correspondence"; 80 is read directly off Fig. 4.2 (allowed by ANSWERS.md 1); holding it
  until the first change keeps the formula's DeltaQ = 0 from dropping T to 0 before anything
  has been seen, which would end runs at once with the literal answer, against p. 105's
  "unlikely".
- **Knob:** `Config.initial_temperature` (80.0), `Config.q_floor` (0.05).
- **Alternatives:** Q from the best correspondence; a start at 100 (the plan's).
  As built (`World.quality`, `Temperature.update`, item 19): Q is the Worldview's summed
  strength (`worldview.current`) over its size, over 100, clipped; `held` on every
  `TemperatureUpdate` says whether the initial value still stands; the first update is
  made when the world is built (`by` = `start`).

### D-050: When T is recomputed
- **Kind:** simplest.
- **Sources:** temperature is computed from the Worldview's history [F95 pp. 90-91]; the
  dumps show it recomputed within the codelet that held a fight [F95 pp. 104-106 (img)]
  (SPEC.md 6c.12); nothing says how often otherwise.
- **Choice:** T is recomputed after every competition and every Worldview change (inside the
  codelet that caused it), and otherwise every `temperature_update_every` = 5 codelets. Each
  recomputation appends one rate to the window of D-048.
- **Reason:** the dumps for the first rule; five codelets for the second so that the
  six-update window of D-048 spans about thirty codelets, the scale over which the chapter 4
  thermometer falls with an unchanging Worldview (59 at 22, 31 at 36) [F95 pp. 100-103 (img)].
  This is a reading of the dumps' time scale, not a fit of their values.
- **Knob:** `Config.temperature_update_every` (5).
- **Alternatives:** every codelet (the window then spans six codelets); only at competitions.
  As built (`World.step`, item 19): after the codelet has run and its follow-ups are
  posted, T is recomputed with `by` = `change` when the Worldview's change log grew,
  `competition` when only the competition log grew, and `schedule` every fifth codelet;
  one recomputation per step at most.
  As built (item 32, a note): no recomputation follows the find-answer codelet that ends
  the run, so `Result.temperature` and the report line's "T" are the last scheduled or
  event-driven value, up to four codelets old; find-answer builds nothing, so the
  difference is the decay of the averaged rate over those codelets. A final update before
  the report is the recorded alternative, not built because every reported number of
  RESULTS.md was made under this reading.

## Coderack (D-051 to D-056)

### D-051: The catalogue: the 23 types of the screen
- **Kind:** screen-dump-inferred.
- **Sources:** the text names twelve codelets, the legend beside Fig. 4.1 prints the full
  names of the strip's 23 labels [F95 p. 96 (img)], "approximately thirty" types [F95 p. 76]
  (SPEC.md 5.2, 6c.1).
- **Choice:** exactly the 23 types of the legend, under the legend's names, each a function
  in `tabletop/codelets/` with a docstring saying whether its job is in the text or inferred:
  find-group, simple-sameness-detect, find-end-objs, find-middle-objs, find-sim-nhbr,
  find-neighbor, build-obj-0, build-obj-1, build-obj-2, build-obj-desc, search-for-sam-gp,
  search-for-sim-gp, pick-salient-obj, pick-salient-subgp, pick-2nd-salient-obj, create-cp,
  pick-strong-cp, examine-cp, build-cp-descrip, cp-compete, check-worldview,
  clean-up-corresp, find-answer. The inferred jobs: find-sim-nhbr looks for a neighbour of
  the touched object (or of its argument) that is conceptually close to it; build-obj-desc
  writes a built group's descriptors (kind, count, orientation); search-for-sim-gp looks for
  a group of the same kind and size elsewhere; pick-salient-subgp picks a subgroup by
  salience and posts a search for its twin; pick-2nd-salient-obj picks an object by salience
  among those not yet described; check-worldview re-reads the Worldview's ratings and posts a
  cp-compete for any tentative correspondence that now outrates its rival. The trace prints
  the screen's labels (FND-GP, SAM, ...).
- **Reason:** the legend is the program's own list; "approximately thirty" against 23 is a
  looseness of the text (SPEC.md 6c.1).
- **Knob:** none.
- **Alternatives:** merging the three build-obj stages into one type.
  As built (`tabletop/codelets/catalogue.py`, item 16): `CATALOGUE` holds the 23 types in
  the strip's order as `CodeletType` rows (name, label, role, job, `job_source`, pages);
  `job_source` is `text` where the text describes the job and `inferred` where the legend's
  name is all there is. Seven are inferred, the six above and build-cp-descrip, whose
  legend name ("build-cp-descrip.") is read as writing a built correspondence's descriptors
  (its level and slippages), not as building the correspondence itself, which create-cp
  does [F95 p. 96 (img)]. The role column is D-052's band (scout, search, builder,
  worldview, answer), and a test checks that `Config.urgencies` gives one value per band.
  The functions themselves come with the items that build each mechanism (17-19); the
  catalogue is data.

- **As revised (loop0007, item 03):** the catalogue keeps the legend's 23 in the strip's
  order and appends the role codelets after `find-answer`, marked `re-interpretation`
  (D-081); its "roles" (the urgency bands) are renamed bands, `catalogue.BANDS`,
  `CodeletType.band`, `of_band`, so that "role" means one thing in the package (D-079).

### D-052: Raw urgencies, follow-ups, the floor
- **Kind:** simplest.
- **Sources:** raw urgencies are integers "between 1 and 100" [F95 p. 81]; find-group's
  follow-ups are listed [F95 p. 76], the other lists are not; "which types of codelets should
  be spawned by a particular codelet" and "how a codelet sets the 'raw' urgency of the
  codelets it spawns" are the two open problems [F95 p. 77]; a codelet's urgency reflects
  "its action's perceived promise" [FCCA p. 385]; the adjusted urgency raw / rho^G is not
  said to be rounded (SPEC.md 5.3, 5.5).
- **Choice:** `urgencies`, one integer per type: scouts 20 (find-group, find-sim-nhbr,
  find-neighbor, find-end-objs, find-middle-objs, pick-salient-obj, pick-2nd-salient-obj,
  pick-salient-subgp, simple-sameness-detect), searches 30 (search-for-sam-gp,
  search-for-sim-gp), builders 40 (build-obj-0/1/2, build-obj-desc, create-cp,
  build-cp-descrip), the Worldview's codelets 50 (pick-strong-cp, examine-cp, cp-compete,
  check-worldview, clean-up-corresp); find-answer's is D-056's. Follow-ups, in
  `codelets.py`, one generation on, at the type's raw urgency: find-group on success posts
  one find-group, one simple-sameness-detect, two find-neighbor (two members), one
  find-end-objs, one find-middle-objs and two pick-salient-obj (p. 76's list, with p. 81's
  two find-neighbors); find-neighbor posts find-neighbor on each new neighbour (p. 80) and
  build-obj-0 on the pair; build-obj-k posts build-obj-(k+1); build-obj-2 posts
  build-obj-desc and twelve search-for-sam-gp (the bulk of 6c.6, scaled to the rack) and two
  search-for-sim-gp; search-for-sam-gp and search-for-sim-gp post create-cp on a find;
  pick-salient-obj and pick-2nd-salient-obj post create-cp between their pick and the most
  salient item in the preferred regions; create-cp posts build-cp-descrip; build-cp-descrip
  posts cp-compete; cp-compete posts clean-up-corresp and check-worldview, and, when a
  group-group correspondence enters, four examine-cp; examine-cp posts create-cp for pairs
  inside the two groups; pick-strong-cp posts cp-compete for a tentative correspondence;
  pick-salient-subgp posts search-for-sam-gp. An adjusted urgency is a float, floored at
  `urgency_floor` = 0.01.
- **Reason:** the plan's bands by role, builders above scouts so that found structure is
  pursued (FCCA's promise), with the one list French gives kept exactly and the rest the
  simplest that connects the 23 types into the flow of SPEC.md 2.6; a floor keeps a
  fourth-generation codelet choosable at low temperature instead of exactly never.
- **Knob:** `Config.urgencies`, `Config.urgency_floor` (0.01).
- **Alternatives:** urgencies scaled by the argument's salience (FCCA p. 385's "all other
  things being equal"); rounding to integers.
  As built (`coderack.py`, item 16): `raw_urgency` reads the table, `adjusted_urgency`
  is max(`urgency_floor`, raw / rho^G) as a float, `make_codelet` sets both at posting, and
  `child_of` makes a follow-up one generation on, posted `by` its parent's type, at the
  table's raw urgency unless the parent passes one (find-answer's comes from D-056). The
  worked examples are tests: 8 / 2^3 = 1 and the children's 4 against 32 [F95 p. 81], the
  explosion's one seventh under rho = 1 and one third under rho = 3 [F95 p. 77], rho = 1
  giving every codelet its raw urgency and the parent's share rising with rho from 0.5 to
  100 [F95 p. 82]. The follow-up lists are applied by each codelet's function (items
  17-19), not by the rack.
  As built (`tabletop/codelets/`, item 19): what each of the 23 codelet functions does,
  as the entry's lists were read where it was silent, is the paragraph moved for size to
  DECISIONS-INDEX.md, "D-052's as-built paragraph (item 19)"; the run loop removes every
  correspondence whose group end has left the Workspace (`World.drop_orphans`, D-041).
  As built (item 24): the correspondence chain's raw urgencies, create-cp, build-cp-descrip
  and cp-compete, are 100, the top of French's range [F95 p. 81]: the builders' lead over
  the scouts was defeated by the generation discount (a create-cp at 40 / 3 = 13.3 ran
  behind twenty find-groups at 20), and the dumps show no CT-CP or BLD-CP bar in nine
  screens while the first correspondence is built by 12 codelets and in the Worldview by
  22 [F95 pp. 98-100 (img)]. pick-salient-obj picks objects only, "pick other salient
  objects on the table" [F95 p. 76], one other than the touched object, and proposes the
  correspondence between the two, the touched object's counterpart being what the run is
  after [F95 p. 105]; groups are picked by pick-salient-subgp and found by the searches,
  the pairs inside a group correspondence by examine-cp [F95 p. 108] (the legend's two pick
  codelets [F95 p. 96 (img)]). Before, any item and any partner, which at a(80) = 1.3 gave
  a dozen junk pairs (an object to a row, a spoon to a knife) before the touched cup's;
  they filled the Worldview while the touched cup stayed unmapped, and the literal answer
  ended more than half the runs on the chapter 4 configuration, where French's run built
  no silverware pair, "it has missed all the correspondences between the silverware" [F95
  p. 105], and first joined the touched cup to the isolated cup [F95 p. 97]. The chain of
  D-039 is now create-cp, cp-compete, build-cp-descrip.

- **As revised (loop0007, item 03):** two rows join the urgency table, `propose-role-cp` at
  60 and `check-roles` at 20 (D-081), a priori; the bands of the 23 are unchanged.
  - **As revised (loop0007, item 07):** pick-salient-obj's choice is weighted by the
    re-cast (consumer C2, D-096): each object's salience times one plus the tier weight
    of its COUNTERPART tag (D-093), so the touched object is no longer the only selector
    of its partner (RESULTS.md §8.1); with no tag standing the draw is as before.
  - **As revised (loop0007, item 10):** pick-strong-cp takes an optional argument, a
    correspondence name (consumer C4, D-101): its candidates are then that correspondence's
    challengers, or itself once outside the Worldview; without one the draw is as before.
  - **As revised (loop0007, item 06):** examine-cp offers the anchor to the anchor first
    (consumer C3, D-097, `judges.anchor_pair`, no draw) when the first group has one
    ANCHOR, `answer.role_member` matches it to an anchor and the pair is open; otherwise
    the two salience draws as before.

### D-053: Maximum age
- **Kind:** simplest.
- **Sources:** "A codelet that has been on the Coderack too long is eliminated" [F95 p. 76];
  the rack holds 150 and the oldest are pushed off [F95 p. 82]; no removal by low urgency
  [F95 p. 77] (SPEC.md 5.6). The age is not given.
- **Choice:** age counts the codelets run since the codelet was posted; a codelet older than
  `max_codelet_age` = 60 is removed (an event) at the next update; the capacity
  `coderack_capacity` = 150 pushes off the oldest first [F95 p. 82].
- **Reason:** sixty codelets is a third of a typical run and longer than any chain of
  follow-ups needs; the dumps show a rack that turns over faster than that (6c.6).
- **Knob:** `Config.max_codelet_age` (60); `Config.coderack_capacity` (150) [F95 p. 82].
- **Alternatives:** age in Slipnet cycles (the same under D-019); no age limit, only the
  capacity.
  As built (`Coderack`, item 16): the rack is a list in posting order, so the oldest
  codelet is first and `post` pops from the front while the rack exceeds the capacity,
  returning the codelets pushed off with the reason `capacity`; `remove_aged(clock)`
  removes those whose age, clock minus `posted_at`, exceeds `max_codelet_age` (60 stays, 61
  goes), with the reason `age`, in rack order. `choose` takes the chosen codelet off and
  records every codelet's chance. No path removes a codelet for its urgency: a floored
  codelet survives sixty choices at a(0) = 6 and goes only by age (a test).
  As built (item 32, a note): the clock advances before the chosen codelet runs, so a
  follow-up's `posted_at` is its parent's ordinal; ages are consistent (a codelet posted by
  codelet n is 60 codelets old after codelet n + 60) and no reading of them changes.

### D-054: Generation 0 for top-down and find-answer codelets
- **Kind:** simplest.
- **Sources:** "The original codelets on the Coderack are of generation 0, their children are
  of generation 1" [F95 p. 81]; nothing is said of codelets the Slipnet posts (D-020) or of
  find-answer codelets (D-056) (SPEC.md 5.5).
- **Choice:** codelets posted by the Slipnet and by the find-answer schedule are generation
  0, like the starting set; only a codelet's children carry a generation.
- **Reason:** they have no parent, and the Slipnet's postings are the program's renewed
  interest, not a chain reaction.
- **Knob:** none.
- **Alternatives:** generation 1 for top-down postings.
  As built (item 16): `make_codelet` defaults to generation 0 and takes `by` as `start`,
  `slipnet` or `answer-schedule`; only `child_of` raises the generation.

### D-055: The initial rack
- **Kind:** screen-dump-inferred.
- **Sources:** "There are a number of find-group codelets initially on the Coderack" and
  "also pick-salient-object and pick-salient-correspondence codelets" [F95 p. 65]; two
  codelets in, the strip reads FND-GP 56, SAM 6, FD-END 3, FD-MID 3, FD-SIMN 26, PK2SALO 7
  percent [F95 p. 98 (img)], a rack of about 33 with about 19 find-groups and 2
  pick-2nd-salient-obj, the rest one find-group's follow-ups (SPEC.md 4.2, 6c.4).
- **Choice:** `initial_codelets`: 20 find-group, 2 pick-2nd-salient-obj, 1 pick-salient-obj,
  1 pick-strong-cp, all generation 0 at their raw urgencies, with no argument. The
  find-sim-nhbr bar is read as one find-group's follow-ups and is not in the starting set.
- **Reason:** the dump's counts for the two types it shows; one each of the two types the
  text names, which the dump does not contradict (the second codelet that ran may have been
  one of them).
- **Knob:** `Config.initial_codelets`.
- **Alternatives:** the plan's 15 find-group and 3 pick-salient-object.
  As built (`coderack.initial_rack`, item 16): the 24 codelets are posted in the table's
  order (the twenty find-groups first), generation 0, `by` `start`, with no argument and
  the adjusted urgency equal to the raw; building the rack draws nothing. The screen's
  bars are `Coderack.shares`, the percentage of codelets of each type in the strip's
  order [F95 p. 97], which reproduce Fig. 4.9's nine bars from a rack with those counts.
  A rack that empties mid-run is reloaded with the same starting set (`World.step`, item
  19), the simplest way to keep a run alive; it has not happened in the first whole runs.
  As revised (loop0007, item 04): the initial rack holds one `find-roles` (D-084) beside
  the 24, so that the midline is read early; 25 codelets at generation 0.

### D-056: The find-answer schedule
- **Kind:** simplest.
- **Sources:** "on each cycle of the program, a certain number of find-answer codelets are
  put on the Coderack. The lower the temperature, the more such codelets are hung on the
  Coderack." [F95 p. 94]; a rise of temperature was "slightly reducing the probability for
  the program to run a find-answer codelet" [F95 p. 105]; FND-ANS is 24 percent of the rack
  at 22 codelets (T 59) and 30 percent at the end (T 2) [F95 pp. 100-107 (img)]; an early
  find-answer is "unlikely, but theoretically possible" [F95 p. 105] (SPEC.md 6.5, 6c.7).
- **Choice:** after every codelet, `answer_post_rate` x ((100 - T) / 100)^`answer_post_power`
  find-answer codelets are posted (10 x (0.2)^3 = 0.08 per codelet at T = 80, 0.7 at T = 59,
  9.4 at T = 2; the integer part always, the remainder with one coin), generation 0, each
  with raw urgency `answer_urgency_max` x ((100 - T) / 100)^`answer_urgency_power` rounded,
  at least 1 (1 at T = 80, 7 at T = 59, 26 at T = 36, 64 at T = 14, 94 at T = 2), fixed at
  posting.
- **Reason:** "the lower the temperature, the more" read against the dumps: no find-answer
  bar at 0, 2 or 12 codelets with T at 80, a quarter of the rack at 22 codelets with T at
  59, and a third at 55 codelets with T at 2 [F95 pp. 96-107 (img)]; a linear law (item
  10's: one per cycle with probability (100 - T) / 100 at raw urgency (100 - T) / 2) posts
  two or three before the twelfth codelet and gives them an urgency that lets them run at
  T = 80, which ended the first whole runs after ten to thirty codelets with the literal
  answer, against "unlikely, but theoretically possible" [F95 p. 105]. A cubic law in
  (100 - T) posts almost none at 80 and floods the rack below 20; the urgency law rising to
  the top of French's range [F95 p. 81] at T = 0 is how temperature acts on the codelet's
  chance of running [F95 p. 105]: at a(T) the find-answers on the rack are negligible above
  T = 50 and dominant below 20. The power 3 is the plan's own alternative; nothing here was
  fitted to a result set.
- **Knob:** `Config.answer_post_rate` (10.0), `Config.answer_post_power` (3.0),
  `Config.answer_urgency_max` (100.0), `Config.answer_urgency_power` (3.0).
- **Alternatives:** the linear laws (item 10's); a fixed urgency (30) with the cubic posting
  law (the plan's).
  As built (`World.schedule_find_answer`, item 19): the schedule runs at the end of every
  step after the temperature update, unless the run has ended; `AnswerCodeletPosted` is
  what the trace will print.
- **As revised (v5, D-120):** both laws read T x (1 - pressure), the time pressure of
  `World.time_pressure` (0 within `answer_patience` codelets of the last Worldview change);
  with no pressure nothing differs.

### D-057: What find-answer announces
- **Kind:** spec-inferred.
- **Sources:** a find-answer codelet announces "the current object corresponding to the
  touched object" and stops the program [F95 p. 105]; the answer needs "one or more mappings
  between objects or groups of objects across the table" [FH91 p. 180] (SPEC.md 6.5).
- **Choice:** the other end of the Worldview's object-level correspondence that contains the
  touched object; if there is none, D-058, then D-063. A group as the other end (a
  group-object correspondence from the touched object) is answered by the group's most
  salient member, and the trace records the case.
- **Reason:** the sentence.
- **Knob:** none.
- **Alternatives:** none that fit p. 105.
  As built (`answer.answer_for`, item 19): the Worldview correspondence with the touched
  object at an end, in the order built (a correspondence built inside a group
  correspondence counts, since it is flagged like any other); a group at the other end
  answers with `salience.most_salient` of its members and `Answer.through` says `group`.
  Reading the answer draws nothing.

### D-058: A touched object mapped only through its group
- **Kind:** simplest.
- **Sources:** "the default object corresponding to the touched object is the touched object
  itself" [F95 p. 105]; nothing says what happens when the touched object's group is mapped
  but the object is not (SPEC.md 6.5).
- **Choice:** `answer_from_group_position` = False: the literal object, as D-063, with the
  trace recording that a group correspondence covered the touched object; True answers with
  the member of the corresponding group that has the same group description (left end for
  left end, middle for middle), or the literal object if none has.
- **Reason:** the default of p. 105 is the only rule given; the knob lets the bench count how
  often the case decides a run.
- **Knob:** was `answer_from_group_position` (False); `Config.group_position_match` since D-097.
- **Alternatives:** the member with the same type.
  As built (`answer.group_correspondence`, `member_in_same_position`, item 19): the first
  Worldview group correspondence whose first end holds the touched object; under the knob
  the first member of the other group in member order whose group-frame words equal the
  touched object's (so a middle maps to the other row's first middle); the `Answer.note`
  records the case either way.
  As revised (loop0007, item 06): answered through roles (D-097). The boolean is gone;
  `group_position_match` is `roles` (the member with the touched object's roles, then the
  words above, then the literal object) or `words` (the words, then the literal object):
  both answer from group position; the literal answer is no value of the knob.

### D-059: Several objects under one answer label
- **Kind:** spec-inferred.
- **Sources:** Blockage 3-10 report one bar for "E1 or E2", the two glasses of a group [F95
  p. 125] (SPEC.md 7.1; `reported.BLOCKAGE`).
- **Choice:** a problem file's `answers` maps each label to an id; a bar that names two
  labels is matched when the run's answer is either of them (`reported.ResultSet` keeps the
  bar's label, the bench joins the ids). The engine never sees labels.
- **Reason:** the bar graphs' own labelling.
- **Knob:** none.
- **Alternatives:** counting the two glasses separately beside French's joint bar (the bench
  prints both).

### D-060: One law for temperature in every choice
- **Kind:** spec-inferred.
- **Sources:** "temperature affects not only codelet urgencies but all of the probabilistic
  processes of the program" [F95 p. 93]; every selection "approaches determinism as the
  temperature approaches 0, is completely unbiased (i.e., uniformly distributed random) at
  100" [F95 p. 93]; the exponent a runs linearly from 6 at T = 0 to 0.1 at T = 100 [F95
  p. 92] (SPEC.md 5.4, 6.4). The law for choices other than codelet selection is not given.
- **Choice:** one law, in `tabletop/temperature.py`: a(T) = 6 - 0.059 T for T in [0, 100]
  (`exponent_at_zero` = 6, `exponent_at_hundred` = 0.1 [F95 p. 92]); a weighted choice among
  items with weights w takes item i with probability w_i^a / sum w_j^a (codelets by their
  adjusted urgency; items by salience, D-034; regions by preference, D-032); a yes-or-no test
  with chance q passes with probability q^a / (q^a + (1 - q)^a) (the stage tests of D-024, the
  test of D-039, the fight of D-040). Coins that are perceptual, not decisions, are not
  softened: the region blur (D-004), the proximity link (D-022), the removal (D-041), the
  find-answer posting (D-056), and the Slipnet's top-down posting (D-020).
- **As revised (loop0008, item 04):** the chances of a rack, u^a over the total of u^a
  with the empty and the all-zero rules, are the function `temperature.chances_of`, which
  `Coderack.chances` reads through the engine seam: on the compiled engine it is the C
  mirror of `_native/coderack.pyx` (the same powers, the total summed as Python's `sum`
  sums floats, the same divisions; D-112), on the pure engine the function itself. The
  other choices of the law (salience, the regions, the yes-or-no tests) stay the Python's:
  their lists are a dozen items where the rack's is 150.
- **As revised (v3, D-114):** an infinite weight (an urgency times 2 per generation under
  `rho` 0.5, past generation 1,023 in one run of the `rho` bench) shares all the chance
  with the other infinite ones, the limit of the weights over the largest; finite runs as before.
- **Reason:** French's sentence; the two-outcome form is the same formula with two weights,
  so there is one law to test, and both forms are deterministic at T = 0 and uniform at
  T = 100, as the page requires. The exception list keeps "all of the probabilistic
  processes" to the program's decisions: a boundary is not blurrier because the program is
  undecided.
- **Knob:** none (`exponent_at_zero`, `exponent_at_hundred`: French's values to v2, D-114).
- **Alternatives:** softening every coin; Copycat's separate temperature-adjusted
  probability function.

### D-061: The run cap
- **Kind:** simplest.
- **Sources:** no cap is mentioned; time pressure "pushes for the program to finish within a
  reasonable amount of time" [FCCA p. 387]; French's longest reported average is 450 codelets
  [F95 pp. 115-139] (SPEC.md 2.7, 6.5).
- **Choice:** `max_codelets` = 3000: a run that reaches it ends with the answer D-057 would
  give at that moment (the literal object if there is none) and is marked capped; the
  benches report the share of capped runs beside every result.
- **Reason:** six times the longest average French reports, so that no run he describes
  would be cut; a cap is needed for the benches to terminate.
- **Knob:** `Config.max_codelets` (3000).
- **Alternatives:** 1000; none (unsafe for a stochastic program).
  As built (`World.step`, item 19): the step that reaches the cap finishes the run with
  `answer.answer_for` and sets `capped`; `Result.capped` and the report line's
  `(capped)` carry it. The first whole runs end in 20 to 140 codelets; none has been
  capped.

### D-062: The structure value reported
- **Kind:** spec-inferred.
- **Sources:** chapter 5 reports "The average quality of the structure built for each
  answer" [F95 pp. 113-114]; FCCA's monitor recorded "the final structure-value" [FCCA
  p. 392], which is the Worldview's rating with its penalties and rewards [FCCA p. 387]
  (SPEC.md 4.7.5, 6.6).
- **Choice:** the Worldview rating of D-042 at the moment the find-answer codelet runs (or
  the cap). The benches compare its ratios between answers of one problem with French's,
  never its absolute scale, which his numbers (about 50 to 311) do not fix.
- **Reason:** both sources name the Worldview's rating.
- **Knob:** none.
- **Alternatives:** the rating plus the groups' strengths (FCCA's "all the structures").
  As built (`World.structure_value`, `run.Result.structure_value`, item 19):
  `worldview.current(world).total` when the run ends.

### D-063: The default answer: the touched object
- **Kind:** spec-inferred.
- **Sources:** "the default object corresponding to the touched object is the touched object
  itself" [F95 p. 105] (SPEC.md 2.7).
- **Choice:** when no Worldview correspondence contains the touched object (D-057, D-058),
  Eliza touches the touched object. The trace says `literal`.
- **Reason:** the sentence.
- **Knob:** none.
- **Alternatives:** none.
  As revised (loop0007, item 10): the default is recorded as `blocked` instead of
  `default` while a CLAIMED tag stands on an obvious counterpart (`answer.default_answer`,
  consumer C4, D-101); the object and `literal` are unchanged.
  As built (`answer.answer_for`, item 19): `Answer.literal` is True whenever the object is
  the touched one, however it was reached; `through` is `default` when no Worldview
  correspondence holds the touched object or its group.

## Engineering (D-064 to D-068, D-072 to D-077)

### D-064: The generator: splitmix64
- **Kind:** extension (engineering).
- **Sources:** French does not name his program's random number generator. The text layer of
  F95 was searched for "random number", "generator", "pseudo-random" and "seed": nothing. The
  book says only that the codelets "are dispatched stochastically from the Coderack" [F95
  p. 55], that "each codelet has an assigned urgency that determines its probability of being
  executed" [F95 p. 92], and that temperature affects "all of the probabilistic processes of
  the program" [F95 p. 93]. FH91 does not mention one either.
- **Choice:** one `Rng` per run, splitmix64 (Steele, Lea and Flood 2014; Vigna's reference
  code), written out in full in `tabletop/rng.py`, with its published test vectors in
  `tests/test_rng.py` (checked against an independent SBCL implementation, the Numbo port's
  oracle). It
  provides `next_u64`, `below` (rejection sampling, no modulo bias), `random` (53 bits),
  `choice` (lists and tuples only; sets and dicts are refused), `weighted_index`
  (select-list-position of Mitchell's Copycat, util.l), `coin` (flip-coin) and a draw counter
  `draws`; `run_seeds(batch_seed, count)` gives a batch its per-run seeds, so any run of a
  batch can be replayed alone. The `random` module is never imported under `tabletop/`.
- **Reason:** A generator written out in a few
  lines of integer arithmetic gives the same numbers on every machine and every Python version,
  which is what makes a seed a complete record of a run and the fingerprint of a run
  (`tabletop.run.fingerprint`, item 21) stable; and the draw counter lets events and tests say
  how many numbers a step drew. The choosing
  functions are Copycat's because Tabletop's architecture is Copycat's [F95 p. 55].
- **As revised (loop0008, item 02):** the compiled engine's generator is the same
  splitmix64 in native 64-bit arithmetic (`tabletop/_native/rng.pyx`, D-109), drawing the
  identical stream; every construction goes through `tabletop.accel`, and `rng.py` is the
  specification.
- **Knob:** none.
- **Alternatives:** `random.Random` drawing only through `random()` (what Seek-Whence does;
  its sequence is guaranteed across Python versions for a given seed, but it is not readable
  code and gives no per-draw oracle); Chez Scheme's linear congruential generator (what the
  Metacat port copies, to match its oracle: there is no oracle to match here).

### D-065: Typed events and the two traces
- **Kind:** extension (engineering).
- **Sources:** French's screen shows the table, the thermometer, the Slipnet grid and the
  Coderack bars [F95 pp. 95-107 (img)]; a run reports the object touched, the structures, the
  resources and the final structure [F95 p. 145]; nothing in the sources describes an event
  stream (it is engineering).
- **Choice:** the engine says what it does by publishing frozen dataclasses of plain data
  (`tabletop/events.py`, documented in `docs/engine-api.md`), each carrying the clock (the
  codelets run) and the generator's draw count; observers receive every event in subscription
  order and cannot change the run (an observer that raises is recorded and skipped;
  `tabletop/observe.py`). The text trace (one line per codelet, the last in French's report
  style) and the JSON-lines trace (`docs/trace-format.md`) are two observers; the fingerprint
  of a run (`tabletop.run.fingerprint`, item 21) is the SHA-256 of the JSON lines.
- **Reason:** the GUI of loop0006 and the tests need the run's every step without touching
  it; the clock and the draw count locate a divergence between two runs to within one
  codelet.
- **As built (item 21):** 27 event classes (`tabletop/events.py`, `EVENT_TYPES`), the 26 of
  the plan plus `CodeletRan` (what a codelet did, for the text trace). One `Publisher` per
  `World` (`World.events`), shared with its Workspace and Slipnet, builds each event with the
  clock and the draw count at the moment it is published and hands it to the observers in
  order; it catches every exception an observer raises, records (observer, event, error) on
  `Result.observer_errors` and prints a line on stderr. With no observers nothing is built.
  Events are published where the mechanism acts (the Workspace's `notice`, `describe`,
  `add_proposal`, `remove_group`; the Slipnet's `jolt`; `groups.announce`;
  `build_object._record`; `describe.find_neighbours`; `correspondences.create` and `test`;
  `worldview._hold`, `record_change` and `remove`; the `World` for the run, the codelets, the
  context, the Slipnet updates, the temperature and the answer). The text trace
  (`observe.TextTrace`), the JSON-lines trace (`observe.JsonLines`), the hashing observer
  (`observe.Hasher`) and the collector (`events.Collector`) are observers; the read-only
  state is `snapshot.snapshot(world)` (`World.snapshot()`), which recomputes a
  correspondence's points for the copy instead of storing them, so that a GUI reading
  between events changes nothing (a first version called `evaluate` and did). The
  fingerprint is `run.fingerprint(problem, seed, profile)`. The codelets' `detail` strings
  no longer name their jolts in words (item 19's "; jolt end"), since the `NodeJolted`
  events report them with their amounts. Golden traces: `tests/data/golden/`.
- **Knob:** none.
- **Alternatives:** callbacks per mechanism; a mutable log on the World; events built by
  diffing the state after each step (loses the order within a codelet and the draw count).

### D-066: Profiles
- **Kind:** extension (engineering; ANSWERS.md 2 and 7).
- **Sources:** French ran Buridan 2-13 with a raised incoherence penalty [F95 p. 130] and the
  second runs of Tables 5.1-5.3 with a raised object-type weight [F95 pp. 142-143]; he names
  seven faults of his program (SPEC.md 6b).
- **Choice:** `config.PROFILES`, a dict from name to a `Config`: `standard` (every default of
  this file, never tuned); `buridan` (`standard` with `incoherence_penalty` fitted in item 26
  to Buridan 1 modified, D-076: the scan was flat and 40 was kept); `human` (`standard` with
  `type_weight` fitted in item 26 to half of Tables 5.1-5.3, D-076: 50);
  `french-faults` (`standard` with the seven fault knobs at their faulty values, for
  diagnosis only). Each `Config` carries its `profile` name; a run and its trace record it and
  every field that differs from `standard`. A profile differs from `standard` only in the
  fields its decisions name (`tests/test_config.py` asserts it). `config_defaults.lock`
  (item 25) freezes all four.
- **Reason:** ANSWERS.md 2 (the faults restorable, never default) and 7 (the two
  calibrations apart, `standard` never tuned).
- **Knob:** `Config.profile`.
- **Alternatives:** command-line overrides only (kept too: `--set field=value`).

### D-067: The Potelbat baseline
- **Kind:** extension.
- **Sources:** FCCA's formula-based program, "a weighted sum of terms, one for each
  attribute" [FCCA p. 327], written "to keep ourselves honest" [FCCA p. 328], in five steps
  [FCCA p. 337]; it fails where a relation between two other objects matters (Blockage) and
  where groups matter (Surround) (SPEC.md 1.7).
- **Choice:** `tabletop/potelbat.py`: for each object, a score = 0.6 x (100 - the Slipnet's
  intrinsic type distance to the touched object's type, at least 0) + 40 x the better of its
  nearness to the diagonal place and 0.7 x its nearness to the directly opposite place, each
  nearness falling linearly to 0 over 40 units; the highest scores. No randomness, no
  structures; for contrast only (measure M9).
- **Reason:** FCCA's description; the numbers are the baseline's own and are not Tabletop's
  (they are the engine's static type and region weights, so that the contrast is in what the
  two can see, not in their constants).
- **Knob:** none.
- **Alternatives:** any other fixed formula.
- **As built (item 23):** `tabletop/potelbat.py`. The intrinsic type distance is read off a
  Slipnet built with the label shrink of D-011 switched off (`intrinsic_slipnet`), so that
  it is the links' own lengths over the pathways of D-012; a pair no pathway joins (cup and
  saltshaker under D-009) counts as 100, so its type term is 0. The touched object is scored
  like every other (its type term the whole 60, its place term whatever its own place earns),
  so that the literal answer stays open to the baseline as it is to Tabletop [F95 p. 25] and
  a table holding the touched object alone (Figure 2.2) has an answer. Ties go to the earlier
  object in file order. On the digitized catalogue the baseline takes the isolated cup on
  Surround 5 and 10 and a glass on every Blockage, which is what FCCA says a formula must do
  [FCCA pp. 355-356] (`tests/test_potelbat.py`); the `potelbat` bench (D-073) prints its answer
  on every problem beside French's and Tabletop's.

### D-068: Realistic random tables
- **Kind:** extension.
- **Sources:** configurations must look "like a real table that was set with plates, cups,
  saucers, saltshakers, pepper shakers, and silverware" and never like "a Christmas tree"
  [F95 p. 38]; Fig. 2.21's two disallowed tables [F95 p. 39 (img)] (SPEC.md 1.4).
- **Choice:** `tabletop/tables.py` (item 28): a generator, seeded, that lays out one to three
  place settings per side (a plate or soup bowl with silverware beside it in the usual order,
  a cup with or without a saucer, a glass), optional shakers between the rows, and jitter of
  at most a glass width; the touched object is drawn by type weight. For robustness runs
  only, never for results compared with French's.
- **Reason:** the robustness bench needs tables the program was meant for.
- **Knob:** none.
- **Alternatives:** uniformly random scatter (ruled out by p. 38).
  As built (`tabletop/tables.py`, item 28): how `random_table(seed)` lays a table, the paragraph
  moved for size to DECISIONS-INDEX.md, "D-068's as-built paragraph (item 28)".

### D-072: The benches: runs, seeds and the statistics beside French's numbers
- **Kind:** extension (engineering; ANSWERS.md 1).
- **Sources:** French reports each configuration as a bar graph of the frequencies and a
  table of "the average structure value at the end of the runs and the average number of
  codelets run in runs giving the answer in question" [F95 p. 115], headed "Object Eliza
  touches", "Average structure", "No. of codelets run" [F95 p. 115]; "Almost all of the
  configurations were run fifty times. No configuration was ever run fewer than forty
  times." [F95 p. 114]; FCCA: "Each problem was run a total of 50 times." [FCCA p. 392]. He
  gives no test statistic and no seeds; his frequencies are percentages of about fifty runs,
  five of his bar graphs print odd percentages (SPEC.md §7.1), and two of FCCA's reprinted
  sets differ from the book's (SPEC.md §7.5).
- **Choice:** a bench (`python3 -m tabletop.bench NAME`) surveys each problem of a result
  set over `--runs` runs (default 500; `--runs 50` for French's own size) from one bench
  `--seed` (default 1) under one `--profile`, and prints, for every answer French labels,
  his three numbers beside ours in his table's format, with the page. The seeds: a problem's
  batch seed is the first output of the generator seeded with the bench seed combined
  (exclusive or) with the first eight bytes of the SHA-256 of the problem's name, so that a
  survey is the same whichever other problems the bench runs, and the runs' seeds follow
  from it as in any batch (D-064); `--json` records every run's seed, answer, structure
  value and codelets, so that any run can be replayed alone. The statistics
  (`bench/stats.py`): a run counts under French's label when its own label is one of the
  labels joined in his ("E1+E2" takes E1 and E2 [F95 p. 123]); runs with no label of his
  fall under "other", for which he prints no bar; each frequency gets the two-sample z of
  our proportion against his, taking his count as frequency x 50 / 100 (fractional for his
  odd percentages) and his n as 50, since his number is itself a sample; each problem gets
  the chi-square of homogeneity between our counts and his over his labels and "other"
  (cells empty in both dropped, degrees of freedom one less than the cells kept), with the
  p-value and limits by the Wilson-Hilferty approximation (at one degree of freedom its
  5-percent limit is 3.75 against the exact 3.84: a stated approximation, used for
  reporting only). The `families` bench covers the 35 chapter 5 sets, Buridan 1's problem
  surveyed once and shown beside both its sets; the `fcca` bench covers FCCA's 18 figures
  against FCCA's own numbers, and shows its two divergent sets against the book's numbers
  as well, from the same runs. Nothing a bench prints is ever used to set a default
  (ANSWERS.md 1); before the freeze (item 25) the benches are run only at the tests' sample
  size.
- **Reason:** French's numbers are samples of about fifty, so the right comparison of two
  proportions is a two-sample one, and the right comparison of a whole answer distribution
  a chi-square of homogeneity; the per-problem batch seed keeps every survey reproducible
  and independent of the bench's selection; recording every run's seed is what makes a
  miss traceable to a run and then to a decision (RESULTS.md).
- **Knob:** none (the command line's `--runs`, `--seed`, `--profile`, `--problems`).
- **Alternatives:** a one-sample z taking French's percentage as the true chance (it
  overstates the evidence, since his fifty runs carry a standard error of about seven
  points at 50 percent); exact tests (Fisher's) in place of the chi-square; one batch seed
  shared by every problem.

### D-073: The probe benches: humans, chapter 2, chapter 4, focus, rho, lesions, faults, the Potelbat, and `all`
- **Kind:** extension (engineering; ANSWERS.md 1, 2 and 7).
- **Sources:** beyond the 35 result sets (D-072), French reports Tables 5.1-5.3, the
  subjects' percentages beside a second set of runs made after he "slightly increased the
  relative importance that the program attached to object-type matches" [F95 p. 143], and
  Figure 5.4, the problems "arranged in decreasing order of goodness of fit" [F95 p. 143],
  with his verdict that on over half of them the two differ by at most ten points [F95
  p. 144]; the chapter 2 configurations with his expectations in words [F95 pp. 26-35]
  (D-003's `expected` block); the chapter 4 run's nine screen dumps [F95 pp. 96-107] (SPEC.md
  §6c); Surround 10, "the effect of adding superfluous 'junk' to a particular table
  configuration" [F95 p. 121], with the small glass touched "13 percent less often" and
  "approximately the same amount of time" [F95 p. 121]; the parameter rho, "If p is 1, we have
  the original situation" [F95 p. 82] and the two extremes of search he describes; FCCA's
  criterion 11, "a few of dozens of interesting ways of 'lesioning' Copycat or Tabletop"
  [FCCA p. 365], four of them listed; the Potelbat [FCCA pp. 327-328] (D-067); and the seven
  faults fixed in `standard` (ANSWERS.md 2). French gives no number for any lesion, any value
  of rho but his own, or any junk count but Surround 10's.
- **Choice:** eight more benches under `python3 -m tabletop.bench NAME`, all on the harness
  of D-072, and a `bench all --out DIR` that writes every bench's tables and records, the
  files RESULTS.md is built from: `humans`, `ch2`, `ch4`, `focus`, `rho`, `lesions`,
  `faults` and `potelbat`, each described in the catalogue moved for size to
  DECISIONS-INDEX.md, "D-073's catalogue" (v5, 2026-10-17). Nothing a bench prints sets a
  default (ANSWERS.md 1).
- **Reason:** every result set and statement of French's is a measurement target (ANSWERS.md
  1), frequency table or not; the lesions are FCCA's own criterion; the junk scaling and the
  rho sweep extend French's two probes along their axes; one protocol for every bench
  (`SETS`, `problems_of`, `planned`, `run`, `report`, `records`) keeps `all` and RESULTS.md's
  builder simple.
- **As built (item 27):** a ninth probe bench, `temperature` (`tabletop/bench/temperature.py`),
  and the results builder's reading of measure M7 from the `lesions` records
  (`lesions.READINGS`, `lesions.verdict`), described in the catalogue; the expectations
  (item 23) and readings (item 27) were stated before the records were read (RESULTS.md §6.2).
- **Knob:** none (the command line's `--runs`, `--seed`, `--profile`, `--problems`, `--out`).
- **Alternatives:** one bench per table of French's; a `directions` bench of pre-registered
  orderings (the stated-behaviour bench, D-074); junk drawn from D-068's realistic tables
  instead of a grid; Kendall's tau in place of Spearman's rho.

### D-074: The stated-behaviour bench
- **Kind:** extension (engineering; ANSWERS.md 1; measure M2).
- **Sources:** French's statements of what the program does, in words, each with its page:
  the first group is a bunch of objects close together and Henry's row is found whole
  [F95 p. 97]; the first correspondence joins the touched cup to the isolated cup, "one of
  the most salient objects on the table" [F95 p. 97]; it enters "without a fight" [F95
  p. 101]; while the Worldview does not change the temperature falls [F95 p. 101]; the
  cup-glass correspondence beats cup-cup on coherence [F95 p. 105]; "The run ends only when
  a find-answer codelet is run." [F95 p. 105]; the literal answer is "unlikely, but
  theoretically possible" [F95 p. 105]; the program's answers on the chapter 4
  configuration are the small glass and the isolated cup [F95 p. 118]; a cup-spoon group is
  a structure of high temperature [F95 p. 111]; group-to-group mappings are favoured [F95
  p. 125]; correspondences are built inside group correspondences [F95 p. 85; F95 p. 108];
  the spoon-spoon groups' correspondence enters [F95 pp. 103-105]; the dumps' order of
  milestones and their activations [F95 pp. 96-107 (img)]; the junk of Surround 10 [F95
  p. 121]; the Blockage gaps, grouped at one width and "no longer" at three [F95 pp.
  128-129]; the fork-glass group of Figure/Ground 1 [F95 pp. 141-142] and the subgroups of
  four identical objects [F95 p. 140] (the two faults the runs can show, D-069 and D-026).
  None is a number of a result set.
- **Choice:** `python3 -m tabletop.bench stated`: ten samples (Surround 5, Surround 10,
  Blockage 6, 8, 9 and 10 under the profile given; Figure/Ground 1 and Buridan 12 under it
  and under `french-faults`), every run's facts read off its events (the groups built with
  the temperature at the build, the correspondences and their ends, the Worldview entries
  and fights, the context pushes, the scheduled temperature updates, the last codelet, the
  last activation of the touched type and "opposite", the `ch4` milestones); 25 numbered
  statements, each with its page, our numbers and a verdict (yes, no, or nothing to read);
  then the chapter 2 series as the `ch2` bench reads it, with its tally. One statement
  (the junk costing codelets) is marked as the bench's own reading, not French's words.
  The thresholds are stated in each check (a majority, 90 or 100 percent of runs, under 5
  percent for "no longer" and "virtually never", as PLAN.md §7's examples put them).
- **Reason:** M2 is the development evidence: these are the only claims a mechanism may
  answer to before the freeze (ANSWERS.md 1), so they are read by machine, with the numbers,
  on every run of the bench; a miss is a finding traced to a decision.
- **Knob:** none (the command line's `--runs`, `--seed`, `--profile`, `--problems`).
- **Alternatives:** the statements as unit tests over fixed seeds (too few runs for a
  yes-or-no on a rate); thresholds from French's result sets (forbidden).

### D-075: The freeze, the results file and its builder
- **Kind:** extension (engineering; ANSWERS.md 1 and 2; TASK.md's validation protocol).
- **Sources:** French reports every result set as a bar graph of frequencies with a table of
  the average structure value and the number of codelets run for each answer, over about
  fifty runs [F95 pp. 114-115]; Tables 5.1-5.3 put his program's second runs beside the
  subjects' percentages [F95 pp. 143-144]; FCCA reprints eighteen of the sets [FCCA pp.
  394-399]. Nothing in the sources says how a reconstruction should report itself; the
  protocol is the loop's (TASK.md, Validation: a freeze before any result set is run in
  earnest, every default frozen in a lock, every later change logged with its before and
  after results).
- **Choice:** `config_defaults.lock` is written from `tabletop.config.PROFILES` before any
  bench runs at full size: JSON, `{profile: {field: repr(value)}}` for every profile, every
  field. The benches run at 500 runs per problem with bench seed 1 (`python3 -m
  tabletop.bench NAME --json`), `families` and `fcca` under `standard` and again under
  `french-faults`, and their records are stripped of the per-run lists by
  `python3 -m tabletop.bench.results strip` into `results/NAME.json` (each survey keeps its
  tallies, statistics, seeds, the median and quartiles of its codelets and the number of
  runs, so that any run is replayable from the recorded batch seed). RESULTS.md's tables
  are generated from those stripped records only, between `<!-- begin NAME -->` and
  `<!-- end NAME -->` markers (`python3 -m tabletop.bench.results update results
  RESULTS.md`); the prose around them is written by hand after the numbers are seen, and a
  test regenerates every block and compares it with the file. The measures M3-M6 and M9 are
  computed by the same module: M3 counts, within each family, the ordered pairs of result
  sets and kinds of answer (the kind of object a label names, since his labels change
  meaning across a family and his descriptions differ in detail) where French's two
  frequencies differ by more than two standard errors at n = 50, and whether ours (n = 500) go the same way or are
  significantly reversed; M4 whether French's main answer is within 15 points; M5 whether
  our better-structured answer is his and whether it takes longer as his does; M6 the
  median codelets per family against his 50-200 and Surround 10's mean against Surround
  5's; M9 the Potelbat's answers where FCCA says a formula must fail [FCCA pp. 355-356].
  The result sets a fault fix may move are marked by the rule SPEC.md §6b gives for each
  fault, read mechanically off the layouts (three or more identical objects within the
  proximity reach of one another for fault 1; an isolated object across from a group for
  fault 2; the Surround family for faults 3-5; Figure/Ground 1 for fault 6; none for 7).
- **Reason:** the lock makes the freeze checkable by the gate; the stripped records keep
  the repository small (the full records are megabytes and are not committed; the gate
  rejects files over 300 KB) while keeping every number RESULTS.md prints; generating the
  tables by machine means the file can never disagree with the records it cites.
- **Knob:** none (the builder's command line: `strip`, `tables`, `update`).
- **Alternatives:** committing the full records (too large); writing the tables by hand
  (no consistency test possible); one bench seed per family rather than per problem (D-072
  chose per problem, so a partial run reproduces the whole).

### D-076: The two calibrations: the protocol, the grids, the objectives and the split
- **Kind:** calibrated (ANSWERS 7).
- **Sources:** French calibrated twice. For the human comparison, "Because pilot studies had
  shown Tabletop to be more 'structurally oriented' than most subjects, I slightly increased
  the relative importance that the program attached to object-type matches compared to
  relational structure in the evaluation of good correspondences" [F95 p. 143], and ran "a
  sample of twenty-three problems from the original forty" [F95 p. 142] fifty times each
  [F95 p. 143]. For Buridan, "With the standard penalty value, the program goes for the
  spoon on Eliza's side of the table about 72 percent of the time" [F95 p. 130]; "When the
  penalty for incoherent mappings was increased ... I was able to adjust the program so that
  Eliza's choice was precisely poised, like Buridan's ass between two equidistant bales of
  hay, between the spoon and the fork on her side" [F95 p. 130], and "All the subsequent
  configurations in this family were run with this modified version of the program" [F95
  p. 130]. He gives neither the values nor the method. ANSWERS.md 7 allows exactly these two,
  after the freeze, as separate profiles: `human` on a random half of Tables 5.1-5.3,
  evaluated on the other half; `buridan` on Buridan 1 modified, evaluated on Buridan 2-13.
- **Choice:** `tabletop/bench/calibrate.py`. Each fit is a scan over a grid fixed before any
  run, the default first so that "no change" is a possible outcome: `type_weight` over 30,
  40, 50, 60, 80, 100, 150 and `incoherence_penalty` over 40, 60, 80, 100, 150, 200, 300. One
  survey per value and problem with the benches' per-problem batch seeds (D-072), the value
  with the least objective chosen, the smallest among ties. `human`: the split is a
  Fisher-Yates shuffle of the 23 problems through the generator (D-064) seeded with the
  recorded split seed 1, the first twelve fitted, the eleven others held out; the objective is
  the sum, over the fit rows, of the squared differences between our two percentages and the
  subjects' two columns (least squares, as PLAN §7.3 proposed); 200 runs per survey. `buridan`:
  Buridan 1 under the modified program's labels; the objective is |E1 − E2|, French's
  "precisely poised ... between the spoon and the fork"; the distance from his 50-50 is
  reported beside it, not fitted; 500 runs. The evaluation is the `humans` bench (both
  profiles) and the `families` bench under each profile at 500 runs, generated into
  RESULTS.md §9 by `bench/results.py` with the before and after on all families; the lock
  is regenerated and the calibration log gains a dated entry. Outcome (2026-10-13): `human`
  `type_weight` = 50, a shallow minimum (the scan varies by under five percent and a second
  bench seed puts the minimum at 30); `buridan` `incoherence_penalty` = 40, the default,
  because the scan was exactly flat: the Worldview never holds two correspondences, so the
  coherence terms never enter a rating (RESULTS.md §9.2; D-052, D-044, D-024).
- **Reason:** a grid with the default first and the objective fixed before the run is the
  smallest protocol that can say "no change" honestly, which both ANSWERS.md 1 (never
  tune) and 7 (only French's two) need; least squares against the subjects' columns is the
  fit PLAN §7.3 named and the one the tables support; the imbalance is the literal reading
  of "poised between", and a distance from 50-50 would reward lowering the `other` share,
  which the penalty is not about. A flat or shallow scan is reported as such, never refined
  by a finer grid or more seeds until a difference appears.
- **Knob:** `Config.type_weight` (50 in `human`), `Config.incoherence_penalty` (40 in
  `buridan`); `calibrate.HUMAN_GRID`, `BURIDAN_GRID`, `SPLIT_SEED`.
- **Alternatives:** a continuous search (a line search on a stochastic objective would chase
  noise); fitting to Figure 5.4's gap alone (one column per row; the two-column least squares
  uses everything the tables give); fitting `buridan` to the distance from 50-50; a different
  half (the split seed is a knob, and RESULTS.md §9.1 reports the fit's instability).

### D-077: The invariants checked after every codelet
- **Kind:** extension (engineering).
- **Sources:** the rules the sources state of the program's state: the Worldview is "a
  non-self-contradictory subset of the Workspace" [FCCA p. 390] in which "no object or
  group of objects is the end object for two distinct correspondences" [F95 p. 71];
  activations run "from 1 to 150" [F95 p. 59]; the Coderack holds at most 150 codelets
  [F95 p. 82] and codelets too old are eliminated [F95 p. 76]; the temperature's range
  (D-047); a sameness group holds "identical objects" [F95 p. 65]. No page describes a
  self-check in French's program.
- **Choice:** `tabletop/invariants.py`: `check(world)` returns every violation of these
  rules and of the decisions that keep structures consistent (D-041: no correspondence
  ends on a structure that has left the Workspace; D-026: under the fix no sameness group
  lies inside another of its type; D-065: names never reused; D-062: the structure value is
  the Worldview's rating; D-061: a capped run has run exactly to the cap; D-063: the literal
  flag; descriptions belong to structures that exist) as sentences, reading the state and
  drawing nothing, so that a checked run is the same run; `checked_run(problem, config,
  seed)` steps a run with the check after every codelet and at the end and returns the
  result with the violations. The `robustness` bench runs the check on 500 random tables
  (D-068) and the default test set on a fast sample of tables and catalogue problems, the
  slow set on 1,000 runs (`tests/test_invariants.py`).
- **Reason:** a stochastic program with 23 codelet types and removals from three
  registries can leak or contradict itself on a table its author never set; a check after
  every codelet finds the step that breaks a rule, not only the end state. The first run of
  the check (item 28) found one leak, descriptions of removed groups and correspondences
  left behind in the Workspace, fixed by removing them with their structure (no event, no
  draw: the golden traces and fingerprints are unchanged); nothing else in 1,000 runs.
- **Knob:** none.
- **Alternatives:** checking the end state only (cheaper, blind to transient faults);
  assertions inside the engine (they would stop a run, which observers may not do).
  As revised (loop0007, item 11): `check_roles` joins `check` (D-079, D-089): tag names
  unique; a registry role, a finite strength in 0 to 100, a weight of one or more; the
  item exists and every structure the arguments name stands; one tag per reading; no
  standing rivals on one item; a CLAIMED tag has its BARRIER; COUNTERPART tags and the
  re-cast are the touched object's. Draws nothing; no violation on the catalogue under
  any profile nor on 500 random tables.

## Roles (D-078 on)

### D-078: Roles as part of the engine; the two sides
- **Kind:** re-interpretation.
- **Sources:** French never uses "role" as a technical term, but his account is role-like
  throughout: "within the particular context under discussion, X is the counterpart of Y"
  [F95 p. xv]; in a supertranslation "the role of A in framework X is so obvious that it
  does not even need hinting at" [F95 p. 12], and B plays the same role in framework Y;
  the cup in front of Henry becomes the schema of the cup in front of person X, carried to
  Eliza's side, where under pressure it slips to the glass [F95 p. 6]. Chapter 5 names
  roles: the "sidekick" objects beside the candidates [F95 p. 130]; the "figure object"
  against a dull background, echoing a larger group across the table [F95 p. 140]; the
  "loner" cup of Surround 5 [F95 p. 118]; the objects "placed around two of the key
  objects" [F95 p. 114]; chapter 2: "correspondences that a priori have nothing to do with
  (i.e., are nowhere in the vicinity of) the touched object are discovered and ultimately
  come to play essential roles" [F95 p. 36]. One table, not two situations: "the two
  situations between which mappings will be formed overlap and blur on the table" [F95
  p. 39]. The program has no role vocabulary: what plays which part is implicit in the
  correspondences, the slippages and the selectors (D-052, D-032, D-058).
- **Choice:** roles are part of the engine and always on (the author's decision: a
  re-interpretation of Tabletop, not a bit-by-bit replication; nothing may turn the
  mechanism off; no `roles` profile, subpackage or hook). A role is the function an item
  plays in the analogy, not what it looks like, read on the two sides the domain offers:
  - an *intrinsic* role is read on one side of the table alone (a loner, a corner, a row);
  - a *comparison* role needs Henry's side against Eliza's (an echo, a twin, the images);
  - a *Worldview* role is read off the correspondences already standing (the reigning
    symmetry axis, a barrier, a claimed object, the type that keys the story).

  Two sides make one comparison, weaker than confirming a role across many examples and
  the only analogue the domain offers, so no role-producing operation is built; the
  re-cast is the one transport, from the touched object's role on Henry's side to its
  holder on Eliza's. A tag is a Workspace structure
  with a strength, proposed, rated, built and breakable like a correspondence; rival
  readings of one item fight by the Worldview's own competition and compatible roles
  coexist: a role is the run's current interpretation (the author's answer D1). The registry is closed at 23 roles in five families (geometric, structural,
  selector, key, content); every role has Slipnet node(s) and a consumer, built with the
  perception it reads (D-079 on). "Before roles" is commit `e38d6e0`, its numbers kept in
  `results/v1/` as the v1 column; RESULTS.md §11 sets French | v1 | roles side by side,
  and every intended change of runs is logged in its Re-interpretation log.
- **Reason:** French's account of Tabletop is in terms of roles, which his program could
  only approximate with selectors and constants, to which several of §8's misses are traced
  (D-032, D-052, D-058). Roles as structures that compete and change let the program say
  what plays which part and the write-up which reading moved an answer. No switch is the
  author's decision; the lesion is a test-only probe instead (D-104).
- **Knob:** none. A knob that turned roles off, by name or by value, would be the switch the
  author forbids; each role's and consumer's values are knobs in their own decisions (D-079
  on), every one validated above zero by `Config`.
- **Alternatives:** roles as an optional extension with a profile that leaves them out
  (the author: no separation, no off switch); a read-only role report consumed by nothing;
  French's four names only (the author asked for more).

### D-079: The registry, the pool and the tag as a competing structure
- **Kind:** re-interpretation.
- **Sources:** French's account is role-like (D-078: the "sidekick" objects [F95 p. 130],
  the "figure object" [F95 p. 140], the "loner" [F95 p. 118]), and his structures compete:
  "the rating of each of the correspondences in that Worldview is improved and the rating of
  the losing correspondence is lowered" [F95 p. 72]; a structure that has gone makes its
  codelets "have no action at all" [F95 p. 73]. Weight: "weak sidekicks (i.e., single
  objects as opposed to small sameness groups) are not sufficient" [F95 p. 131]. Nothing
  in the program tags, rates or breaks a role.
- **Choice:** `tabletop/roles.py` holds the one registry (`REGISTRY`, 23 `RoleSpec`s in
  five families, each with its reading, nodes, detector and consumers; `pending()` lists
  the roles still owed one; closed: a new role needs the author). The Workspace gets the
  pool: `roles`, every `RoleTag` standing in the order
  tagged, named `r1`, `r2`, ... (`tag_role`, `discard_role`, `roles_of`, `role_tag`,
  `has_role`); a tag goes with the group or correspondence it stands on or names
  (`discard_roles_on`). A tag carries item, role, arguments, weight (the objects in
  the item: 1 for an object, n for a group or a run of identical sidekicks), evidence, who
  tagged it, and a **strength** in 0 to 100 (`rate`): `role_weight_points` (10) per
  unit of weight, `role_node_points` (30) times the activation of the role's first node
  over 150, `role_story_points` (20) per Worldview correspondence with the tag's item, or
  an item it names, at an end. **Fights:** `RIVALS` names the readings that exclude one
  another on one item (BARRIER-SIDEKICK, MIRROR_IMAGE-POINT_IMAGE, LONER and STRAY against
  MEMBER, STRAY-LONER, FIGURE-GROUND); `roles.propose` builds the newcomer, jolts its node
  by `jolt_description` (D-016) and fights every standing rival by `worldview.rival_chance`
  (the strengths as ratings, one coin under a(T), D-040, D-060); the loser is discarded
  `lost to rN`. Compatible roles coexist. **Breaking:** check-roles (D-081) re-rates one
  tag and breaks it with chance `1 - strength / role_break_scale` (100), floored at 0,
  under the temperature law. **Perception:** `roles.detect(world, item)` is the one entry
  point to the detectors; `World.step` calls it for every object brought into view,
  find-roles for what needs structures first. Nothing is gated.
- **As revised (loop0008, item 04):** the pool keeps an index of its tags by item
  (`Workspace._roles_by_item`), which `tag_role` and `discard_role` maintain and
  `roles_of` and `has_role` read; `roles` stays the list in the order tagged, and a hand
  edit of it that changes its length (a test's) rebuilds the index on the next read. The
  same tags in the same order, every run unchanged (D-112).
- **Reason:** the author's answer D1: a role is a competing, revisable structure, not a
  fixed label; the same object can be a BARRIER while one reading holds and a SIDEKICK
  once another wins. The strength reads what the item is (weight), how alive the concept
  is (the node, as French's slippages take their node's activation [F95 p. 68]) and how
  the story uses it (the Worldview's support, lost when it changes). The fight reuses the
  Worldview's law; the entry point is one function so the oracle probe can replace it.
- **Knob:** `Config.role_weight_points` (10), `role_node_points` (30), `role_story_points`
  (20), `role_break_scale` (100), each refused at zero or below.
- **Alternatives:** a fixed label computed once (rejected by the author); a tag with no
  strength, broken only with its structure (the story could never change it); a
  competition rule of its own (a second law to tune).

### D-080: The Slipnet's role nodes and links
- **Kind:** re-interpretation.
- **Sources:** FCCA on the node set: "There is of course nothing sacrosanct about this exact set
  of concepts. Many others could easily be added, but such additions would require no
  changes to the architecture of the program." [FCCA p. 381] (checked on the page). The
  two symmetries give way to one another under pressure [F95 p. 86; F95 p. 115]; figure against ground [F95 p. 140];
  corners pair through the centre [F95 p. 43]; "in front of" is mirror placement [F95
  p. 6]; sameness means counterpart [F95 p. xv]; forks and glasses "are seen as 'opposites'"
  [F95 p. 140]. French's screen draws 41 boxes and none of these (D-007).
- **Choice:** 15 nodes appended after "isolated" (63 in all), none on the screen: `role`
  (level 3, the role kinds' superordinate), `counterpart` (level 3), and at level 2
  `sidekick`, `anchor`, `figure`, `ground`, `echo`, `container`, `connector`, `barrier`,
  `separator`, `key` (group `role`), `row`, `column`, `corner` (group `structure`). Depth
  follows D-008's levels. 29 links with source
  `re-interpretation`: 14 ISA (the eleven kinds to `role`, the three positions to
  `position`, length 30) and 15 labelled: `mirror symmetry`-`diagonal symmetry` (opposite,
  40: the central slip), `figure`-`ground` (opposite, 40), `separator`-`mirror symmetry`
  (symmetry, 40), `corner`-`diagonal symmetry` (symmetry, 40), `column`-`mirror symmetry`
  (symmetry, 30), `sidekick`-`neighbor` (role, 30), `anchor`-`middle` (role, 30),
  `connector`-`middle` and `connector`-`anchor` (role, 35), `container`-`group` (role, 35),
  `echo`-`sameness` and `counterpart`-`sameness` (role, 30), `barrier`-`echo` (role, 35),
  `isolated`-`figure` (role, 35), `key`-`opposite` (role, 40). Jolts: a tag jolts its node
  by `jolt_description`; a comparison role confirmed on both sides and the axis by
  `jolt_structure`; a discarded tag nothing; a role slippage entering the Worldview by
  `jolt_label`. **A consequence measured, not intended:** in v1 the label nodes
  `opposite` and `sameness` had no link; `key`-`opposite`, `echo`-`sameness` and
  `counterpart`-`sameness` give them one, so `opposite`, held near the ceiling all run
  (D-017), warms the role subnet (`key`, through it `role`, a little `group` and
  `sameness`) from the first update; the Slipnet's tests record it (D-081, item 11).
- **Reason:** every role needs a node to be posted by, to jolt and to slip through (the
  author's Q12); the lengths follow D-010's bands (30 nearest, 35 other, 40 a descriptor's
  opposite), so no length is new; each kind is linked to the concept it refines, and the
  one slip French describes, mirror to diagonal, gets its label.
- **Knob:** none in `Config` (the table is data, as D-009 and D-010); the depths are
  `depth_by_level` (D-008).
- **Alternatives:** four nodes only (the author asked for the geometric and positional
  roles); the role nodes on the screen's grid (French's 41 boxes stay); no link from
  `opposite` and `sameness` (the key and the echo unreachable from what means them).

### D-081: The role codelets: types, urgencies, follow-ups and top-down rows
- **Kind:** re-interpretation.
- **Sources:** "there are approximately thirty different codelet types" [F95 p. 63] and the
  legend names 23 (D-051); urgencies lie "between 1 and 100" [F95 p. 81]; "An active
  concept in the Slipnet dispatches codelets to look for instances of itself in the
  Workspace." [F95 p. 64]. No page names a role codelet.
- **Choice:** the role codelets are appended to `CODELET_TYPES` after `find-answer`, with
  strip labels of their own and catalogue rows marked `re-interpretation`. Item 03 adds
  two: `propose-role-cp` (`PR-RCP`, scout band, raw urgency 60), consumer C1 (D-095), and
  `check-roles` (`CHK-RL`, scout band, 20), D-079's checker, which samples one tag (the
  weak first, by 101 minus the strength under a(T)), re-rates it and may break it; with
  no tag it fizzles. The others come with items 04 and 07. **Top-down rows**
  (D-020): `sidekick`, `anchor`, `connector`, `container`, `echo`, `figure`, `ground`,
  `corner` and `key` post `propose-role-cp`; `role` posts `check-roles`; all at generation
  0 (D-054). `propose-role-cp` posts its create-cp and one `check-roles`. The urgencies
  are a priori: 60 sits between the scouts (20-30) and the correspondence chain (100); the
  checker at the scouts' 20, "at low urgency" (the author's answer D1). Item 11 measures
  the role codelets before the answer; a change of urgency is a logged decision.
- **Reason:** the legend's order and labels stay French's (D-051); the role codelets after
  them keep the strip readable and the catalogue honest. The checker as a codelet makes
  re-examination ordinary codelet work, as the author asked.
- **Knob:** `Config.urgencies` rows `propose-role-cp` (60) and `check-roles` (20), refused
  at zero or below; `top_down_table` rows (data).
- **Alternatives:** the checker folded into check-worldview (the pool should be
  re-examined when the Worldview has not changed); `propose-role-cp` in the builder band
  at 100 (a role pair is a proposal).
  As revised (loop0007, item 04): three more types, to 28: `find-roles` (`FND-RL`, scout
  band, 30; D-084), `compare-roles` (`CMP-RL`, scout, 30; D-099), `read-worldview-roles`
  (`RD-WVR`, Worldview band, 50; D-083), a priori. Top-down rows: `separator` posts
  find-roles, `symmetry` read-worldview-roles. Follow-ups: find-group posts one
  find-roles; find-roles a compare-roles for its item and a propose-role-cp when a tag
  stands; compare-roles a propose-role-cp on a find; cp-compete a read-worldview-roles
  after every Worldview change.
  As revised (loop0007, item 05): find-neighbor posts one `find-roles` on its object when
  it records a new relation (D-087, D-088, D-090 read the neighbours).
  As revised (loop0007, item 07): the sixth role codelet, `recast-role` (`RC-RL`, search
  band, 40 a priori), 29 types in all (the plan's 28 predates the checker). `counterpart`,
  `row` and `column` post it at generation 0; find-roles posts one when it read the
  touched object's ROW (D-093); recast-role posts one create-cp per tier-1 candidate and
  one check-roles.
  As revised (loop0007, item 08): find-roles reads the table (D-084, D-086) on every call,
  then its argument or one item in view by salience; find-group posts its find-roles on
  the group it found (D-085, D-086) and one on a seed it marks isolated (D-022). No type,
  urgency or row changed.
  As revised (loop0007, item 09): no new type, urgency or row. The last build-obj stage
  posts one `find-roles` on its group (D-091, D-092); compare-roles reads echoes and the
  twin beside the images; read-worldview-roles reads the axis and the keys (D-094), drops
  a stale Worldview tag (`broken`) and jolts its node by `jolt_structure`.
  As revised (loop0007, item 10): no new type, urgency or row. read-worldview-roles reads
  BARRIER and CLAIMED too (D-089) and posts C4's pick-strong-cp when it read a barrier;
  check-worldview posts the same for the barriers standing (D-101); pick-strong-cp takes
  the barrier's correspondence as an argument (D-052).
  As revised (loop0007, item 11): measured, not changed (D-106, 500 runs, RESULTS.md
  §11.7): the role codelets are 23 to 32 percent of the codelets chosen before the answer
  on the four carriers, the first at codelet 5 to 8, against 12 to 26 find-answer codelets
  hung. No urgency or row changed after seeing this (the author's C8).
  As revised (loop0007, item 06): find-roles also reads the gestalt group of an object it
  read as an ANCHOR and the holder under an object it read (`scouts.container_items`,
  D-087); no new type, urgency or row.

### D-095: Consumer C1: role-guided proposals
- **Kind:** re-interpretation.
- **Sources:** "correspondences that a priori have nothing to do with (i.e., are nowhere in
  the vicinity of) the touched object are discovered and ultimately come to play essential
  roles" [F95 p. 36]; in v1 every object correspondence is proposed between the touched
  object and another (D-052), so any two are rivals (D-044) and the Worldview holds one
  correspondence in 98 percent of runs (RESULTS.md §9.2).
- **Choice:** `codelets.scouts.propose_role_cp`. `role_pairs(world)` lists the pairs the
  standing tags name: for a pair role read on two items (SIDEKICK, ECHO, ANCHOR, CONNECTOR,
  CONTAINER, LONER, CORNER, FIGURE, GROUND, TYPE_KEY), every two items tagged with it in
  the two outer row bands, one in each (`across`: an object's last drawn region, D-004; a
  group's centroid's region; nothing in the middle band); for a tag carrying its
  partner (the images), the item and its partner; only pairs a correspondence may be
  created for (D-035). A pair's weight is the tags' role weights times their strengths,
  floored at `role_pair_floor` (1). The codelet chooses one pair under a(T) (D-060),
  records the consumption on both tags (`RoleConsumed`), and posts one `create-cp` into
  the existing chain, which tests, fights and fades it like any other (D-039 to D-041), no
  bonus for its origin, and one `check-roles`. An empty pool, or no creatable pair,
  fizzles, posting nothing and changing no rating.
- **Reason:** C1 gives the dynamic factors (D-037), coherence (D-042), parallelism (D-043)
  and the usurping rule (D-044) something to act on, by proposing the pairs French says the
  program must discover away from the touched object; the existing chain deciding keeps
  "the Worldview decides" true of them.
- **Knob:** `Config.role_pair_floor` (1), refused at zero or below; the urgency is D-081's.
- **Alternatives:** proposing every pair at once (floods the rack); a bonus for role pairs
  in the static rating (C5's slippage, D-098, for shared roles only).
  As revised (loop0007, item 04): `across` requires a perceived MIDLINE (D-084), so C1
  proposes nothing until the separator is seen; under a SYMMETRY_AXIS (D-083) the image
  pairs are those of the axis's kind alone (D-099).
  As revised (loop0007, item 05): a role read on one table (`roles.SINGLE`) is paired tag
  with tag across the midline whatever the tags' `args` name (a sidekick's is the object it
  stands beside, D-090); only a comparison or Worldview role carries its partner in `args`.
  As revised (loop0007, item 07): `row_band` reads an item's ROW tag when one stands
  (D-085), the drawn region otherwise, so `across` pairs the perceived rows (ROW's consumer
  C1), the chosen pair's ROW tags consumed; recast-role (D-093) posts a create-cp for the
  touched object and each tier-1 candidate, the re-cast's part of C1.
  As revised (loop0007, item 08): loner with loner and corner with corner (D-086, D-085),
  a corner pair's weight scaled by `corner_order` (D-032's order, the point image first);
  the GAP's part pairs (`scouts.part_pairs`): when a gap stands on each row the parts are
  paired under the axis (D-083), left with left under a mirror axis, with the right under
  a point axis, both ways while no axis stands, the two GAP tags consumed; no pair with a
  stray at an end (`describe.is_stray`, D-086), French's "not consider, then reject" [F95
  p. 36]. `pair_weight` names the weight rule.
  As revised (loop0007, item 09): figure with figure and ground with ground (D-091; object
  with object or group with group, `scouts.like_with_like`); echo with echo through the
  tag's partner (D-092); the key's pairs (`scouts.key_pairs`, D-094): every object in view
  of the one type with every one of the other across the midline, either way round,
  neither in a Worldview correspondence. TWIN names no pair (C2, D-093).
  As revised (loop0007, item 06): a CONTAINER pair is holder with holder or setting with
  setting (`like_with_like`, as a GROUND pair).

### D-104: The test-only probes: lesions and the oracle
- **Kind:** re-interpretation.
- **Sources:** FCCA's criterion 11, lesioning the architecture [FCCA p. 365], already used
  by the `lesions` bench through `Config` knobs (D-073); the author forbids any user-facing
  way to run without roles (D-078).
- **Choice:** `tests/role_probes.py` (a helper, not a test file) and `tools/role_probes.py`.
  `lesioned(consumer)` replaces one module-level function with a stand-in through
  `unittest.mock.patch` for the duration of a run (`LESIONS`, one row per consumer; item
  03's: C1, `codelets.scouts.propose_role_cp` fizzling); every consumer and detector is
  therefore one module-level function called through its module, and `codelets.dispatch`
  looks functions up at run time. `oracle_roles(problem)` replaces `roles.detect` with the
  roles hand-read from the layout in `tests/data/oracle_roles.json` (four carriers,
  objects only; group-level and Worldview roles stay the engine's), so every object gets
  its roles at first sight. `python3 -m tools.role_probes` runs both perceptions at 200
  runs and the incoherence-penalty scan at 40 and 300 on the Buridan carrier, writing
  `results/role-probes.json` for RESULTS.md §11.3 and §11.4. Nothing under `tabletop/`
  imports the probes or `unittest`. A consumer that cannot be isolated by one patch loses
  its lesion, and RESULTS.md says what attribution is lost.
- **Reason:** attribution needs a lesion and the author's rule leaves only the test side;
  one patched function is the smallest lesion that leaves the engine switchless.
- **Knob:** none (test-only code; the tool's `--runs`, `--seed`, `--problems`, `--out`).
- **Alternatives:** a `Config` knob per consumer (the forbidden switch); subclassing the
  World in the tests (copies engine code).
  As revised (loop0007, item 07): `LESIONS` gains `C2` (`codelets.scouts.recast_weight`
  returning 0: salience alone), `K1` (`roles.recast` returning None) and `L2`
  (`describe.column_role` reading nothing). The oracle file gains the hand-read ROW and
  COLUMN of every object on the four carriers, by its exact region.
  As revised (loop0007, item 08): `LESIONS` gains `C7` (`salience.role_reading` returning
  (0, 1), D-100), `S2` (`describe.gaps_of`), `L3` (`describe.corner_role`), `L4`
  (`describe.loner_of`), `L5` (`describe.stray_role`), each reading nothing; a GAP and a
  stray group stay the engine's reading.
  As revised (loop0007, item 09): `LESIONS` gains `A4` (`describe.figure_role`), `A5`
  (`describe.ground_role`), `A6` (`correspondences.echoes_of`), `A7`
  (`correspondences.twin_of`), `K2` (`worldview.type_keys`).
  As revised (loop0007, item 10): `LESIONS` gains `S5` (`worldview.barrier_and_claimed`
  reading nothing: no barrier, no claimed object, no `blocked` record) and `C4`
  (`codelets.judges.barrier_fights` posting nothing).
  As revised (loop0007, item 11): `LESIONS` gains `D1` (D-106).
  As revised (loop0007, item 06): `LESIONS` gains `C3` (`answer.role_member` returning
  None: the words and the literal object answer; examine-cp offers no anchor pair).

### D-105: The role events, the snapshot and the trace
- **Kind:** re-interpretation.
- **Sources:** D-065's events and traces; a run reports "the sequence of structures that
  led to that answer" [F95 p. 145], and a role tag is a structure (D-079).
- **Choice:** four events appended to `EVENT_TYPES` after the reconstruction's 27, nothing
  before them changed: `RoleTagged(tag, item, role, args, weight, strength, evidence, by)`,
  `RoleDiscarded(tag, item, role, reason, by)`, `RoleRecast(touched, signature, tiers,
  ambiguous, by)` and `RoleConsumed(role, consumer, item, effect, by)`. `Snapshot` gains
  `roles` (one dictionary per tag standing), `recast` (None until the re-cast exists) and
  `axis` (the tag on the item `worldview`, None until one is tagged). The text trace
  prints one indented `RL` line per role event (`RL tag`, `RL drop`, `RL used`, `RL
  recast`; `docs/trace-format.md`), and the JSON-lines trace one record each, so the
  fingerprint covers them.
- **Reason:** the trace, the event log and the GUI must show every change of a role (the
  author's answer D1); appending keeps the GUI's reading of every earlier event unchanged.
- **Knob:** none (data).
- **Alternatives:** one `RoleChanged` event with a kind field; roles only in the snapshot
  (the trace would not show a change).
  As revised (loop0007, item 07): `RoleRecast` is published by recast-role (D-093), the
  `tiers` as (tier, candidate, weight) tuples and `ambiguous` the first non-empty tier's
  tie; `Snapshot.recast` carries the last re-cast (touched, signature, tiers, ambiguous,
  clock) from `Workspace.recast`; the `RL recast` line prints it.
  As revised (loop0007, item 10): `AnswerGiven.through` gains `blocked` (D-101); no new
  event, snapshot field or trace line.
  As revised (loop0007, item 06): `Answer.roles` carries the roles consumer C3 matched
  (D-097); find-answer records them consumed and the FND-ANS line's note names them. No
  new event, snapshot field or trace line.

### D-082: Consumer C6: the placement slippage in the table frame
- **Kind:** spec-inferred. French describes the slippage and the build left it out: D-029
  defined the table frame (`describe.table_frame`) and nothing called it, so no top-level
  correspondence carried a position slippage and "mirror symmetry" was fed only from
  inside groups (`docs/roles-audit.md`).
- **Sources:** the two identity slippages of cp1 "where the terms 'left' and 'right' refer
  to the sides of the table as a whole. Both of these identity slippages will cause
  activation to be sent to the 'mirror symmetry' concept node in the Slipnet" [F95 p. 85];
  a left-right slippage feeds "opposite" and "diagonal symmetry" [F95 p. 68]; the region
  "directly opposite" and the one "diagonally opposite the touched object" [F95 p. 66];
  "The value of this slippage depends on how active the 'diagonal symmetry' and
  'opposite' nodes are" [F95 p. 68] (D-030).
- **Choice:** `correspondences.placement_slippages(world, cp)`, called from
  `slippages_of` for every correspondence at the top level (none inside a group
  correspondence, whose frame is the group's, D-029) while a MIDLINE is perceived
  (D-084). The two ends' regions are drawn afresh through the blur (`describe.seen_region`,
  D-004; a group by its centroid) and `geometry.placement` read on them: `mirror` (the
  same column, the other outer row) gives a column identity labelled "mirror symmetry"
  and "sameness"; `point` (the column flipped) a column flip labelled "opposite" and
  "diagonal symmetry"; `middle` (both in the middle column, where the two images
  coincide, D-032) a middle-middle identity labelled "symmetry" alone; `none` (one row,
  or an end in the middle band) nothing. The kind is `placement`, beside French's seven;
  scored by factor 4 (`position_points`) with the existing `position` weight times the
  symmetry scale, jolting its labels on entry (D-030) and parallel through them (D-043). `geometry.placement`, `row_of`, `column_of` and `in_midline_band`
  are pure and equivariant under the mirror, the side swap and the rotation of the table
  (an exact test over every pair of regions). The C6 lesion (D-104) patches
  `placement_slippages` to return nothing.
- **Reason:** French's own example of mirror symmetry is a table-frame slippage, and the
  build's omission is why the symmetry nodes were seldom active (RESULTS.md §9.2). The
  blur supplies the tolerance, so no distance constant is added; the midline gate keeps a
  table with no perceived separator from earning placement points.
- **Knob:** `cp_static_weights["position"]` (shared with the group-frame position
  slippage, D-036) and the symmetry scale's nodes; no new value (French has one factor 4).
- **Alternatives:** a tenth static factor with its own weight (French lists one position
  factor); placements inside group correspondences (the group frame is theirs); no midline
  gate (objects in the band leave no "sides" to be opposite across).
  As revised (loop0007, item 07): an end tagged with a COLUMN (D-085) is read in the
  column of its tag (`correspondences.tagged_column`), the consumption recorded
  (`RoleConsumed`, C6), and drawn afresh only when it has none: COLUMN's consumer C6. The
  row still comes from the draw.

### D-083: The symmetry axis, read off the Worldview without the touched object
- **Kind:** re-interpretation.
- **Sources:** "one wants Tabletop to go for the very obvious and attractive diagonal
  mapping" [F95 p. 86]; cp1 and cp2 mirror-symmetric [F95 pp. 84-85]; "Activation of a
  concept (in this case, 'mirror symmetry') in one context may have no significant
  influence on choices made in a" lower-level one [F95 p. 86] (D-021, D-070).
- **Choice:** role G3, `SYMMETRY_AXIS`, tagged on the item `worldview` by the
  `read-worldview-roles` codelet (D-081) through `worldview.symmetry_axis`: the placement,
  mirror or point, that more of the Worldview's top-level correspondences carry
  (`placement_votes` over their placement slippages, D-082), **leaving out every
  correspondence that contains the touched object**, the guard against reading the answer
  off the geometry; none when they tie or there are none; the weight is the votes. Read
  at the top context only (no axis inside a subworld, the top-level one not carried in).
  A standing axis the reading no longer supports is discarded (`broken`) by the codelet;
  the one read is proposed like any tag (D-079) and its node, "mirror symmetry" or
  "diagonal symmetry" (`AXIS_NODES`), jolted by `jolt_structure` (D-016), so the
  placements agreeing with it score more (C6, D-082) and C1 proposes the images under it
  (D-095; both kinds while no axis stands). cp-compete posts `read-worldview-roles` after
  every Worldview change, `symmetry` top down.
- **Reason:** the axis is the Worldview's own geometry made explicit, so a story that is
  mirror on the rows can favour the mirror reading of the touched object without that
  reading being written in; leaving its correspondences out keeps the axis a reading of the
  rest of the table.
- **Knob:** none of its own: `jolt_structure` (shared, D-016); `urgencies` row
  `read-worldview-roles` (50, D-081).
- **Alternatives:** counting the touched object's correspondence too (reads the answer
  off itself); a weighted vote by rating (the axis's strength is its node's activation).

### D-084: The single-table detectors: MIDLINE, and the find-roles codelet
- **Kind:** re-interpretation (written at item 04 for the MIDLINE, S1; items 05 and 08
  add the other single-table detectors of `describe.py` below).
- **Sources:** people "easily and intuitively break it into two situations (e.g., the
  objects on Eliza's half of the table and those on Henry's side of the table) unless the
  table configuration is a very strange one" [F95 p. 39]; "There is no pre-set boundary
  line dividing this arrangement into two disjoint 'situations'" [FCCA p. 388] (D-005);
  the regions [F95 p. 65] (D-004) and the gestalt groups (D-023).
- **Choice:** `describe.midline_perceived(world, item)`, read on the item `table` alone
  (`roles.TABLE`): the middle band of regions is perceived as a separator, role S1
  `MIDLINE` (node `separator`, D-080), when at least one gestalt group stands and no
  member of one has been seen in the band (each noticed member's last drawn region,
  D-004); an object in no group may lie there. Weight 1; the evidence names the
  groups. Read by `find-roles` (D-081) through `roles.perceive`, which proposes a tag for
  each reading (D-079); find-roles posts a compare-roles for its item (D-099) and a
  propose-role-cp when a tag stands (C1), and fizzles when nothing was read. find-group
  posts one find-roles after the follow-ups of p. 76, the initial rack holds one (D-055
  revised), `separator` posts it top down. Consumers: C6 (D-082) and C1 (D-095) only
  across a perceived midline. Groups and correspondences may still cross the band (D-005).
- **Reason:** the midline is what makes "opposite" mean anything; perceiving it from the
  groups keeps D-005 (no pre-set sides) and gives C6 and C1 the line people see.
- **Knob:** `urgencies` row `find-roles` (30, D-081); `initial_codelets` row
  `("find-roles", 1)` (D-055). No distance constant: the band is D-004's.
- **Alternatives:** a midline pre-set at y = 50 (D-005); a midline read from the exact
  regions (the detectors inherit D-004's blur).
  As revised (loop0007, item 05): `describe.py` gains the content detectors MEMBER and
  SIDEKICK (D-090), ANCHOR (D-087) and CONNECTOR (D-088); find-roles reads them on its
  argument when find-neighbor posts it.
  As revised (loop0007, item 08): the MIDLINE is perceived when at least one *cluster*
  stands on an outer row (`describe.clusters_on`, D-086: a gestalt group, or an isolated
  object in no group), so a table of loners (Blockage 2, Surround 1) has its two sides too
  (item 07's question); a gestalt group wholly in the middle band (`describe.stray_group`)
  no longer blocks it, only a group reaching into the band from a side does. `describe.py`
  gains GAP, LONER, STRAY (D-086) and CORNER (D-085).

### D-099: The image roles, read by compare-roles, and C1's images under the axis
- **Kind:** re-interpretation.
- **Sources:** the next most preferred region "is directly opposite the touched object"
  [F95 p. 66] and the most preferred "diagonally opposite" it; "the chances are good that
  Eliza would touch the cup on her side of the table, diagonally opposite the touched cup"
  [F95 p. 28]; "two identical pairs of objects on the table, diagonally opposite one
  another" [F95 p. 34].
- **Choice:** roles G1 `MIRROR_IMAGE` and G2 `POINT_IMAGE`, read by
  `correspondences.mirror_and_point_images(world, item)` for an object or a group in
  view, once a MIDLINE is perceived (D-084): every other item in view whose seen region
  (`describe.seen_region`, through the blur) is the item's reflection across the midline
  is tagged as its mirror image, every one at its reflection through the centre as its
  point image, each tag carrying its partner in `args`; in the middle column the two
  coincide and neither is read (D-032). Weight the item's objects. The two are rivals on
  one item (D-079), so an item carries one reading at a time. `compare-roles` (D-081)
  reads them for its argument, or one item in view by salience, and posts a
  propose-role-cp when a tag stands; find-roles posts it for the item it read. C1 proposes
  the item with its partner (D-095), under the axis alone when one stands (D-083).
  `roles.detect` asks a detector once even when it serves two roles.
- **Reason:** the images are French's two preferred regions re-read as a relation between
  two items, which C1 can propose without the touched object and the axis can count.
- **Knob:** none of its own: `urgencies` row `compare-roles` (30, D-081); the blur (D-004)
  is the tolerance.
- **Alternatives:** images at the level of points with a distance threshold (the blur is
  the tolerance); images without a midline (D-084).

### D-102: The variants bench: eight variants of every problem, paired with the original
- **Kind:** re-interpretation (the guard against fitting; French never ran it).
- **Sources:** French probes a changed table twice: Surround 10 adds junk to Surround 5 and
  the small glass is touched "13 percent less often" [F95 p. 121]; the Figure/Ground table
  sets a "figure object" against a dull background [F95 p. 140]. He reports no mirrored,
  side-swapped, rotated or translated table and no substituted types. Such variants touch
  D-001, D-004, D-009, D-022, D-068 and D-072. The roles loop asked for the bench as
  held-out data before any role code (its ANSWERS.md Q10) and registered what it should
  show (`docs/predictions.md` §6).
- **Choice:** `tabletop/bench/variants.py`, `python3 -m tabletop.bench variants`. Eight
  variants of each problem file: mirrored (x to width minus x; left and
  right silverware turn round), side-swapped (y to height minus y; the touched object moves
  with the rest), rotated (both), translated (half a glass width in x and y, where there is
  room), type-substituted (every object of a type becomes its partner within D-009's
  categories, both ways at once: small and big glass, fork and spoon, salt and pepper
  shaker, cup and soup bowl, saucer and plate; knives have none), junk-added (a salt shaker
  in the band between the rows at the x with the most clearance, two units at least, else
  the far corner), decoupled (the other row's largest cluster moved one column third
  sideways, inside the table, overlapping nothing that stays; Surround 5's isolated cup is
  not part of the row), on-saucer (a saucer under the first untouched cup with room, else
  the touched cup). A variant is a file in
  D-003's format (boxes transformed, positions recomputed, `validate` passing); the
  answers, the touched object and chapter 2's `expected` block go through the object map
  (D-059); an added object is marked added. Three of 424 do not apply (no cup on Buridan
  10 and Figure/Ground 1; no other row on Figure 2.2). Original and variants run with the
  same seeds (the problem's batch seed, D-072), so the runs are paired. Measures per problem
  and variant: the total-variation distance between the relabelled answer distributions; a
  percentile bootstrap interval (200 resamples, seeded from the bench seed and the
  variant's name); the chi-square of homogeneity (D-072); "role kept", the share of paired
  runs whose answer carries the same label [F95 p. 114] or is the same object; the
  mechanism measures (the Worldview's size, the share with two or more correspondences,
  D-042's coherent and incoherent pairs, the role tags per run).
  `results/v1/variants.json` holds the records made on the baseline commit by
  `tools/run_on_baseline.py` (a detached worktree outside the repository, removed
  afterwards); RESULTS.md §11.5 sets v1 beside the roles. **Reading:** on the
  three exact variants a chi-square p below 0.01 is an exception to trace; "lost nothing"
  means that on no family the roles' mean distance exceeds v1's by more than two bootstrap
  standard errors. A failure of exact invariance is a finding, never a fix.
- **Reason:** French never reported these tables, so nothing in them was fitted; paired
  seeds make the exact variants a sharp test (an engine with no handedness repeats the
  relabelled answer run for run); variants of the files keep the engine untouched.
- **Knob:** none in `Config` (the bench's own constants and the command line): the engine
  never reads a bench value.
- **Alternatives:** independent seeds per variant (loses the pairing); a permutation test;
  variants of random tables only; moving the whole other row when decoupling (Surround 5's
  lone cup leaves no room); one substitution per problem.

### D-087: ANCHOR, what a setting is laid out around (and CONTAINER, item 06)
- **Kind:** re-interpretation (item 05 for the ANCHOR, A3; item 06 adds CONTAINER below).
- **Sources:** "not only are there groups around the touched cup and the glass, but the
  groups are quite similar" [F95 p. 30]; "the saltshaker and the touched cup are surrounded
  by identical groups of objects" [F95 p. 32]; "in order for an object to have a neighbor,
  it must be in a group" [F95 p. 76] (D-027); the ISA categories of D-009.
- **Choice:** `describe.anchor_of(world, item)`, read by find-roles (D-084) through
  `roles.detect`: a member of a linear gestalt group (D-028) that has a neighbour on each
  side along the group's axis (`describe.axis_neighbours`, from the relations find-neighbor
  recorded, D-027) and whose nearest ISA category (`describe.nearest_category`, the first of
  D-009's table order: "liquid holder" for a cup or a glass, "closed object" for a shaker,
  "silverware" for a fork) no other member of the group shares, is tagged `ANCHOR` (node
  `anchor`, D-080), weight 1, the evidence naming the category and the two neighbours.
  Nothing is read before both neighbours are known, at an end, in a group that is not a
  line, or for a type that ISA nothing. Consumers: C1 pairs anchor with anchor across the
  midline (D-095); C5 gives the pair a role identity (D-098); C3 (D-097); C7 (D-100).
- **Reason:** French's "landmark" reading of the Surround family: the touched cup and the
  glass are what the rows are laid out around, and the engine had no description for it
  (the group frame says only `middle`). Category uniqueness makes the saltshaker an anchor
  of its row on Surround 8 and 9 too, which the registered predictions take as a test of
  the type term.
- **Knob:** none of its own: the strength is D-079's, the points D-098's. A category
  threshold of its own would be a second D-024.
- **Alternatives:** the most salient member as the anchor (a crowded end object would
  anchor nothing); type-level uniqueness (two glasses of different sizes are one liquid
  holder to the row).
  As revised (loop0007, item 06): CONTAINER (S3, node `container`), `describe.container_of`,
  in two forms. The *setting*: a linear gestalt group with exactly one standing ANCHOR tag
  among its members, tagged on the group, naming the anchor, weight its size, "various
  assortments of objects are placed around two of the key objects" [F95 p. 114]. The
  *holder*: an object whose footprint box (D-002) holds the centre of a smaller object in
  view, tagged on the holder, naming it, weight 1; cups and saucers "are strongly
  associated because, like knives and forks, they are used together" [F95 p. 27]; no
  figure of French's has one, so it is measured on the on-saucer variant (D-102) and
  random tables alone. find-roles reads the group of an anchor it read and the holder
  under an object it read (`scouts.container_items`, D-081). Consumers: C1, setting with
  setting and holder with holder (D-095); C3 (D-097). No rival. Alternatives: the setting
  without the anchor (MEMBER's reading already); the holder by overlap alone (a knife
  across a plate would be held).

### D-088: CONNECTOR, the member that bridges two runs
- **Kind:** re-interpretation.
- **Sources:** "the subgroups (two forks, two spoons) are in corresponding positions on
  either side of the touched object and the glass" [F95 p. 31]; the neighbours of D-027;
  the bottom-up subgroups of D-024, built in few runs (RESULTS.md §9.2).
- **Choice:** `describe.connector_of(world, item)`, read by find-roles (D-084): a member of
  a linear gestalt group whose two immediate neighbours along the axis are of different
  types, neither its own, and each of which has a neighbour of its own type on its far side
  (`describe.axis_neighbours` twice over), is tagged `CONNECTOR` (node `connector`, D-080),
  weight 1, the evidence naming the two neighbours and the two beyond them. It needs the
  four relations known, so it is read late in a run or not at all (D-027, D-052). On
  Surround 5 the glass is a connector (spoon, spoon on one side; fork, fork on the other)
  and the touched cup is not (its left neighbour, a knife, has a fork beyond it): the
  oracle file says so. Consumers: C1 pairs connector with connector across the midline
  (D-095); C5 gives the pair a role identity (D-098); C3 matches it (D-097).
- **Reason:** French reads Surround 5 as subgroups on either side of the two key objects,
  and the subgroups are almost never built; the connector reads the same structure off the
  neighbours.
- **Knob:** none of its own (D-079's strength, D-098's points; `urgencies` rows
  `find-roles` and `find-neighbor`); one object beyond each neighbour is the definition,
  not a count.
- **Alternatives:** the connector from the bottom-up subgroups (seldom built); neighbours
  whose far sides may be empty (an end's neighbour bridges nothing).

### D-090: MEMBER and SIDEKICK, the content roles of a group
- **Kind:** re-interpretation.
- **Sources:** "End/middle location slippages" and "Position slippages" [F95 p. 68]
  (D-029); "these 'sidekick' objects are much less important than the touched
  object itself and the candidate answers" [F95 p. 130]; "weak sidekicks (i.e., single
  objects as opposed to small sameness groups) are not sufficient" [F95 p. 131]; "in order
  for an object to have a neighbor, it must be in a group" [F95 p. 76].
- **Choice:** two detectors in `describe.py`, read by find-roles (D-084) and, for an object
  brought into view when its group is found, at first sight (`World.perceive_roles`, D-079).
  `member_roles(world, item)`: an object in a gestalt group (`gestalt_group_of`, the
  earliest built) is tagged `MEMBER` (node `group`, D-080), weight 1, the evidence the
  group's name and the group-frame words found so far. `sidekicks_of(world, item)`, read on
  the sidekick itself: an object is a `SIDEKICK` of each immediate neighbour x in its group
  (D-027) whose type differs from its own, **when x is the touched object or shares its
  nearest ISA category** (`describe.in_touched_family`, D-009: the objects French calls the
  candidate answers); the tag names x in `args` and its weight is the run of the object's own
  type away from x (`describe.run_length`): the first of two spoons beside the cup is one
  sidekick of weight 2, the second spoon is tagged nothing. The oracle file reads
  sidekicks the same way. find-neighbor posts a find-roles on its object when it records a
  new relation (D-081 revised), so the content roles are read once the neighbours are
  known. LONER and STRAY are MEMBER's rivals (D-079). Consumers: C5 reads member-to-member
  and sidekick-to-sidekick identities (D-098); C1 pairs sidekick with sidekick across the
  midline, tag with tag whatever the tags' `args` (D-095 revised); C3 reads MEMBER (D-097).
- **Reason:** French's Buridan account sets the sidekicks of the touched spoon and of the
  two candidates against one another (the doubled cups against the doubled glasses);
  sidekicks beside every object would tag a dozen on Surround 5 against the four his
  account names and flood C1 with pairs he never mentions. The run as one sidekick of
  weight n is his weighing of p. 131.
- **Knob:** none of its own (D-079's strength, D-098's points). The family is D-009's
  nearest category, not a new threshold.
- **Alternatives:** sidekicks beside every object (rejected above; to revisit if a
  problem's candidates are not of the touched object's family); a tag on every object of
  the run (p. 131 counts the run, not its members).

### D-098: Consumer C5: the role slippage, a tenth static factor
- **Kind:** re-interpretation.
- **Sources:** "The static factors involve, for the most part, slippages that 'support' the
  correspondence" [F95 p. 68]; the position slippage's worth, "The value of this slippage
  depends on how active the 'diagonal symmetry' and 'opposite' nodes are" [F95 p. 68]
  (D-030); parallelism through shared labels (D-043); French's nine static factors (D-036).
- **Choice:** `correspondences.role_slippages(world, cp)`, called from `slippages_of` after
  the group slippages: for each role read on one table (`roles.SINGLE`) that both ends carry,
  one identity of kind `role` (member-member, sidekick-sidekick, anchor-anchor, ...),
  labelled with the role's node (`group` for a member, `sidekick`, `anchor`, `connector`),
  which it jolts on entry by `jolt_label` like every slippage (D-030) and through which the
  correspondence is parallel to others with the same label (D-043); for each pair of rival
  roles (D-079's `RIVALS`) one on each end, a slip with no label, which feeds and earns
  nothing (member to loner). The
  comparison and Worldview roles are not C5's (the images are C6's placement, the axis and
  the keys C1's and C2's). `correspondences.role_points(world, cp)` is the tenth static
  factor, after French's nine in `static_terms`: `cp_static_weights["role"]` (10, a
  priori, beside the position factor's 10) times the role node's activation over 150, per
  role identity; no role in common, 0 and no slippage: nothing is gated. The `role` row
  is refused at zero or below (D-078). The C5 lesion (D-104) patches `role_slippages`,
  taking the points and the parallelism with it (`docs/predictions.md` §5 named
  `role_points`; item 13 scores it as registered).
- **Reason:** a correspondence between two anchors, or two sidekicks of the two key
  objects, is supported by their playing the same part, which French's nine factors cannot
  see (a cup and a glass share no group-frame word on Surround 5); the form is his own for
  a slippage whose worth follows its node.
- **Knob:** `cp_static_weights["role"]` (10), refused at zero or below.
- **Alternatives:** a bonus for role-proposed correspondences (it would reward the
  origin, not the ends); role points independent of the node (French's slippages take
  their node's activation); role slippages for the comparison roles too (the images are
  the placement slippage's already: a second factor 4).
  As revised (loop0007, item 07): C5 reads the single-table roles other than ROW and
  COLUMN (`correspondences.read_by_c5`, `C5_LEAVES_OUT`; D-085): two ends across the
  table differ in row by construction, and the same column is the placement slippage's
  identity (D-082), which a role identity would count twice.
  As revised (loop0007, item 08): `C5_LEAVES_OUT` gains CORNER (the placement slippage's
  identity or flip, D-085), STRAY (of neither side, never across the table, and junk
  earns no support) and GAP (tagged on the table, never on an end). LONER is read:
  loner-to-loner an identity labelled `isolated`, member-to-loner the slip with no label
  (D-086).
  As revised (loop0007, item 09): FIGURE and GROUND are read as every single-table role.
  The key identity (`correspondences.key_slippages`, D-094): a correspondence whose type
  slippage is between a standing TYPE_KEY's two types earns one identity `key`, labelled
  `key`, the consumption recorded. ECHO and TWIN, comparison roles, are not read by C5.

### D-097: Consumer C3: the answer from group position, by the members' roles
- **Kind:** re-interpretation.
- **Sources:** "the default object corresponding to the touched object is the touched
  object itself" [F95 p. 105] (D-063); D-058's case, which nothing in French decides; "B
  plays 'the same' role as A plays" [F95 p. 12]; the groups "are quite similar" around the
  touched cup and the glass [F95 p. 30].
- **Choice:** when the touched object is covered only by a Worldview group correspondence
  (`answer.group_correspondence`, D-058), `answer.answer_through_group` reads the answer
  from its group position in three steps under `group_position_match` = `roles`:
  `answer.role_member`, the member of the other group with the touched object's roles,
  matched in `ROLE_ORDER` (its standing ANCHOR and CONNECTOR tags, D-087, D-088, then its
  `end` or `middle` group-frame word, D-029, read while a MEMBER tag stands, D-090; each
  role keeps the candidates that carry it when any does, else goes unmatched; the first
  left in member order, as D-058 takes the first; None when no role matched: nothing is
  gated); then D-058's words (`member_in_same_position`); then the literal object
  (`default_answer`, D-063, D-101). `words` skips the first step. Both answer from group
  position: D-058's boolean is gone, and the configuration refuses any other value.
  find-answer records the tags matched as consumed (`answer.consume_roles`, D-105). The
  same reading serves examine-cp, which offers the anchor to the anchor first
  (`judges.anchor_pair`, no draw) when the first group has one anchor, `role_member`
  matches it to an anchor and the pair is open; and CONTAINER's setting (D-087).
- **Reason:** D-058 answered through roles (the author's Q11): a role says what a member
  is to its row (the anchor it is laid out around, the connector between its runs), the
  group-frame words only where it stands; so the member that plays the touched object's
  part answers before the one in its place.
- **Knob:** `Config.group_position_match` (`roles`); `words` is the other reading, not an
  off value (both answer from group position; C3's lesion is test-only, D-104).
- **Alternatives:** the member of the same type (D-058's alternative); an exact match of
  the whole signature (D-093); the most salient member (D-057's rule for a group end).

### D-085: ROW and COLUMN, the selector roles of the table frame (CORNER at item 08)
- **Kind:** re-interpretation.
- **Sources:** "the cup on her side of the table" [F95 p. 28]; the table frame, "where
  the terms 'left' and 'right' refer to the sides of the table as a whole" [F95 p. 85]
  (D-029, described and never read); the schema "the cup in front of Henry" [F95 p. 6],
  which the re-cast carries across (D-093); the blurry regions (D-004) and the perceived
  midline (D-084). French's program has no row or column description of an object: its
  regions are constants relative to the touched object (D-032).
- **Choice:** two single-table detectors in `describe.py`, read by find-roles through
  `roles.detect` (D-084) and at an object's first sight. `row_role(world, item)` tags an
  item in view ROW with the row word of `table_frame` (`above`, Henry's; `below`,
  Eliza's), only once a MIDLINE is perceived and never for an item seen in the middle
  band, of neither side. `column_role(world, item)` tags it COLUMN (`left`, `middle`,
  `right`), with or without a midline. Each reads `table_frame` afresh (one blur draw,
  D-029), so the roles inherit the blur's tolerance and no distance of their own; weight
  the item's objects; nodes D-080's `row` and `column`. Consumers: ROW by the re-cast
  (C2, D-093) and C1 (D-095: `across` pairs the perceived rows); COLUMN by the re-cast's
  second tier (C2) and the placement slippage (C6, D-082). C5 leaves both out (D-098).
- **Reason:** the table frame D-029 described was the one frame the engine never read; as
  roles its words become perceptions that compete and can be broken (D-079), and the
  re-cast has something to carry.
- **Knob:** none of its own: D-079's strength, D-004's blur (`region_blur_width`,
  `region_blur_max`) as the tolerance, `urgencies` row `find-roles` (D-081).
- **Alternatives:** rows and columns from the exact regions (the blur is the engine's
  perception, D-004); ROW without a midline (a side needs a perceived separator, D-084;
  Blockage 1 got no ROW at item 07, reported, not patched); one detector for both words
  (the L2 lesion would take ROW with it).
  As revised (loop0007, item 08): CORNER (L3), `describe.corner_role(world, item)`: an
  item seen in one of the four corner regions (`geometry.is_corner`: an outer row and an
  outer column), "two correspondences between diagonally opposite corners of the table"
  [F95 p. 43]. The item is an object on its own (`Workspace.isolated`), by its last drawn
  region (D-004, no draw), or a gestalt group, by its centroid through the blur
  (`seen_region`, one draw); a member is in a corner only through its group (D-029), so a
  row in a corner region is one corner item, not six. Weight the
  item's objects; node `corner` (D-080). Consumer C1 pairs corner with corner across the
  midline, the weight scaled by `scouts.corner_order`, D-032's preference (1 for the
  point image, "the most favored region is diagonally opposite the touched object" [F95
  p. 66]; the `opposite` weight, 0.7, for the mirror image), so Blockage 2's glasses are
  proposed first and without the touched object. C5 leaves CORNER out (D-098, D-082).
  D-084's revision supersedes the "ROW without a midline" note: a table of loners has a
  midline now. Knob: none of its own (D-032's `region_preference`). Alternatives: every
  object in a corner region a corner (C1 would propose every member pair of two corner
  rows, which the images propose already); a corner at the level of points.

### D-093: COUNTERPART: the one re-cast of the touched object's signature, in tiers
- **Kind:** re-interpretation.
- **Sources:** the schema and its transport: "the cup in front of Henry" into the schema
  "the cup in front of person X." [F95 p. 6], carried to Eliza's side, where under pressure
  "it slips to the close notion 'glass in front of Eliza.'" [F95 p. 6]; "X is the
  counterpart of Y" [F95 p. xv]; "B plays 'the same' role as A plays" [F95 p. 12]; the
  images, "directly opposite" and "diagonally opposite" [F95 p. 66] (D-099); the Buridan
  family, where two candidates are equally good and the program must still choose [F95
  p. 130]. French's program has no re-cast: the touched object's partner is whatever
  pick-salient-obj picks (D-052).
- **Choice:** `roles.recast(world)` is the one re-cast and `roles.signature_of(world,
  item)` the signature it carries, read from the tags standing on the object (a discarded
  role is never re-cast): its signature roles (`SIGNATURE_ROLES`: member, anchor,
  connector, corner, loner, container, figure, ground; ROW and COLUMN are the tiers'
  criteria), `end` or `middle` as a member with that group-frame word,
  `sidekick:<category>` for each SIDEKICK tag naming it (the nearest ISA category, D-009),
  `group:<categories>` for its gestalt group. The candidates are the objects in view tagged with the ROW opposite the
  touched object's (D-085; both rows read, else None). A candidate *plays* the signature
  when it carries every word of it (`plays`; more roles still play each). Each falls in
  its best tier (`tier_of`): 1, the image under the standing SYMMETRY_AXIS (D-083, either
  way round) that plays the signature; 2, plays it in the touched object's COLUMN; 3, the
  touched object's type; 4, anything else in the row. Weights `role_recast_tiers` (8, 4,
  2, 1, a priori and halving); within a tier every candidate has the tier's weight, so a
  tie is published and never resolved (Eliza must touch something and the Coderack
  chooses: Buridan). The re-cast is published as
  `roles.Recast` (touched, signature, tiers, ambiguous, clock) on `Workspace.recast` and as
  `RoleRecast` by `recast-role` (`codelets.searches.recast_role`, D-081), which tags each
  candidate COUNTERPART with `(touched, tierN)` in `args` (a candidate whose tier changed
  loses its old tag, `broken`), records the touched object's ROW and COLUMN as consumed
  (C2), and posts one create-cp per creatable tier-1 pair (C1's part, D-095) with one
  check-roles; with a row unread it fizzles, drawing nothing. Consumers: C2 (D-096) and
  C1. Tiers 3 and 5 are revised below (items 09, 10).
- **Reason:** the schema's transport is the one step French describes and his program
  lacks; standing tags keep the re-cast honest about what the run perceived; ties published
  unresolved keep Buridan's balance with the coin.
- **Knob:** `Config.role_recast_tiers` (8, 4, 2, 1), each refused at zero or below; the
  codelet's urgency is D-081's (40).
- **Alternatives:** an exact signature match (the glass of Surround 5, a connector too,
  would never match the cup); a tie resolved by salience (pick-salient-obj's draw, C2);
  candidates from the drawn regions rather than the ROW tags (the transport step is ROW's
  consumer); a re-cast at first sight of every object (it is the codelet's).
  As revised (loop0007, item 09): tier 3 is the TWIN tag's (D-092, `roles.is_twin`), not
  the layout's type; until a twin is read a same-type candidate is in tier 4.
  As revised (loop0007, item 10): a fifth tier, last, for a candidate CLAIMED by a standing
  correspondence (`roles.is_claimed`, D-089, D-101), whatever its natural tier
  (`roles.natural_tier`); `role_recast_tiers` gains its weight, 0.5 (the halving goes on);
  `roles.natural_first_tier` reads the first tier with the claim left out (D-089).

### D-096: Consumer C2: the role-weighted partner choice
- **Kind:** re-interpretation.
- **Sources:** pick-salient-obj picks "other salient objects on the table" [F95 p. 76],
  "in proportion to their salience" [F95 p. 66], and the correspondence it proposes is
  with the touched object (D-052, item 24): RESULTS.md §8.1 traces the `other` share, the
  40 to 50 percent of answers French prints no bar for, to that chain. The author: C2 is
  built and on, with a priori weights, guarded by the variants bench.
- **Choice:** `codelets.scouts.pick_salient_obj` weights each object other than the
  touched one by its salience times one plus `recast_weight(world, item)`, the tier weight
  of its standing COUNTERPART tag (`role_recast_tiers`, D-093) or 0, softened under a(T)
  (D-060); the chosen object's COUNTERPART tags are recorded as consumed (C2). With no tag
  standing the draw is the one before: nothing is gated. The lesion (D-104) patches
  `recast_weight` to return 0.
- **Reason:** the counterpart the re-cast names should be the partner tried first, as
  French's cup in front of Eliza is tried before anything slips [F95 p. 6]; weighting the
  existing pick leaves French's chain in place, and the Worldview decides.
- **Knob:** none of its own: the weights are D-093's `role_recast_tiers`; the urgency is
  French's (D-052).
- **Alternatives:** picking the tier-1 candidate outright (a tie must stay a draw, and a
  wrong re-cast recoverable by the ordinary pick); weighting find-answer (C2 touches the
  proposal only).

### D-086: GAP, LONER and STRAY: the parts of a side, and the items of neither
- **Kind:** re-interpretation.
- **Sources:** "the cup in the lower right-hand corner is seen as a 'loner,' and so there
  is a good deal of pressure to touch the small glass" [F95 p. 118]; "if the 'isolated'
  node is active" [F95 p. 68] (D-007, D-022: `Workspace.isolated`); the two glasses of
  Blockage 2 "are then viewed as
  part of a single, albeit relatively weak, structure, even though they are located at
  opposite corners of the table" [F95 p. 123]; the junk of Surround 10, "the junk should
  not make a significant difference" [F95 p. 121]; "The program should be able to
  ignore--not consider, then reject--the small groups of silverware." [F95 p. 36]; the
  proximity criterion's zero gap, beyond which a group forms no more [F95 pp. 128-129]
  (D-022, `proximity_zero_gap`).
- **Choice:** three single-table detectors in `describe.py`, read by find-roles through
  `roles.detect` (D-084), on a shared reading of the rows. A *cluster* of an outer row band
  is a gestalt group seen there, or an isolated object in no gestalt group (`clusters_on`;
  an item's band is its last drawn region's row, a group's its centroid's, `band_of`,
  D-004); the *gaps* of a row (`gaps_on`) are the white spaces between two clusters
  adjacent along it, between their footprint boxes (`cluster_box`, D-002), of at least
  `proximity_zero_gap` glass widths, with no object between them in that band (the
  physical layout, as the proximity criterion reads it, D-023). **GAP**
  (S2), `gaps_of`, on the table item: one tag per gap, `args` the row word and the two
  clusters parted, weight 1, node `separator` (the MIDLINE's). **LONER** (L4), `loner_of`:
  an object on its own seen in an outer row, set off by a gap it is a cluster of, or alone
  on its side; weight 1, node `isolated`. **STRAY** (L5), `stray_role`: an item of neither
  side, an object on its own seen in the middle band or a gestalt group wholly in it
  (`stray_group`; weight its members); node `separator`; a member of a stray group is a
  stray through it (`is_stray`). Standing on its own is known only after a find-group
  looked around an object, so LONER, STRAY and CORNER are never read at first sight
  (find-group's find-roles, D-081 revised). Consumers: C1 proposes loner with loner and the part pairs the gaps mark
  off, never a pair from a stray (D-095 revised); C5 reads loner-to-loner as an identity,
  member-to-loner as a slip (D-098); C7 scales a stray's region term (D-100). The MIDLINE
  (D-084 revised) is perceived from the clusters and tolerates a stray group. LONER and
  STRAY are rivals of MEMBER and of each other (D-079's `RIVALS`).
- **Reason:** French's loner is set off by empty space and his junk is what the program
  should not consider: facts about the parts of a side the engine had only as the
  "isolated" mark. D-022's distance makes "set off" mean "could not be grouped". The stray
  group is forced by the data: Surround 10's junk forms groups of two and three under the
  proximity criterion, so a stray read on ungrouped objects alone would tag nothing there
  and the junk would block the midline all run; French names the small groups of
  silverware as what to ignore.
- **Knob:** none of its own: the gap's distance is `proximity_zero_gap` (D-022), reused,
  not added; the tags' strength is D-079's; the stray's scale is D-100's.
- **Alternatives:** a gap distance of its own (the zero gap is where a group stops); a
  stray as an ungrouped object only (rejected above); a loner needing no gap ("set off",
  p. 118: an isolated object between two groups of its row is no loner); LONER, STRAY and
  CORNER at first sight (the tag would fight the MEMBER the object becomes a moment
  later); no "nothing between" check (an unnoticed object in the gap is there).

### D-100: Consumer C7: role salience, the ninth term
- **Kind:** re-interpretation.
- **Sources:** salience's factors are "factors that tend to make an object stand out"
  [F95 p. 66], French's list of ten [F95 pp. 66-67] (D-031), none about the part an
  object plays; Surround 5, "not only are there groups around the touched cup and the
  glass, but the groups are quite similar" [F95 p. 30]; "The program should be able to
  ignore--not consider, then reject--the small groups of silverware." [F95 p. 36]; the
  junk of Surround 10 [F95 p. 121].
- **Choice:** `salience.role_reading(world, item)` returns two numbers, the role points
  and the scale on the region term; `salience.role_term` is the ninth term of
  `salience.FACTORS` and `salience.terms`, after French's eight: `salience_weights["role"]`
  (5, a priori) per unit of weight of each ANCHOR, CONNECTOR or FIGURE tag standing on
  the item (`salience.SALIENT_ROLES`: what the rows are laid out around, what bridges
  them, the figure against its ground); the region term (D-031, D-032) is multiplied by
  `stray_salience_scale` (0.5, a priori a half) for a stray (`describe.is_stray`, D-086)
  and by 1 otherwise. With no tag standing the reading is (0, 1) and the salience is
  French's: nothing is gated. Both knobs are refused at zero or below (D-078). The C7
  lesion patches `role_reading` to return (0, 1) (D-104).
- **Reason:** the glass of Surround 5 is what Eliza's row is laid out around and what
  bridges its two runs, as the touched cup is of Henry's; French's account rests on that
  likeness, and a term for it lets the scouts pick it as they pick the touched object's
  neighbours. A stray's lower region term is the engine's way of not considering the junk
  rather than considering and rejecting it; the region term is what makes the band's
  objects "near" the touched object (D-032).
- **Knob:** `salience_weights` row `role` (5); `stray_salience_scale` (0.5). Set a
  priori; a change needs a logged decision.
- **Alternatives:** points proportional to the tag's strength (the weight is French's
  count of p. 131); scaling a stray's whole salience (the type and proximity terms are the
  object's own); leaving strays out of the picks ("nothing is forbidden", D-005, and a
  gate would be the forbidden switch); a role term for MEMBER (every object of a row
  would gain alike).

### D-091: FIGURE and GROUND: the one repeated type of a row and what stands out against it
- **Kind:** re-interpretation.
- **Sources:** Figure/Ground 1, where Henry touches the figure object on Eliza's side that
  echoes the background material on his own, the fork [F95 p. 139]; "It was hoped that,
  at least on occasion, the program would" [F95 p. 139] give the glass, "an exceptional
  object--a 'figure object' standing out against a dull background" [F95 p. 140], and it
  "never selected Henry's glass" [F95 p. 140] (v1: 8.4 percent). His program has no
  figure and no ground.
- **Choice:** two detectors in `describe.py`, read by find-roles through `roles.detect`
  (D-084) and at first sight (D-079). A row's *ground type* is the one type two or more
  members of a gestalt group seen in an outer row band share (`ground_type_of`; two forks
  and two spoons are a composition, not a background). A *figure* is the only object of
  its type on its row (`figures_on`, the row read physically as `loner_of` does, D-086). A
  ground stands where both exist and differ (`grounds_on`; a background with nothing
  against it is none). FIGURE (`figure_role`, A4) is read on the figure, a member or an
  object on its own; weight 1, node `figure`. GROUND (`ground_role`, A5) on each object of
  the ground type (weight 1, as the oracle file reads it) and on a sameness subgroup
  (D-024) of that type inside the gestalt group, the ground as a whole (weight its
  members). Consumers: C1 (D-095), C5 (D-098), C7 (D-100), the key (D-094). The two are
  rivals (D-079). Surround 5 reads neither.
- **Reason:** French's account of Figure/Ground 1 is the figure against its background;
  as roles they let the engine propose what his program could not (toward his theory, away
  from his numbers, both reported). One repeated type is the narrowest "dull background".
- **Knob:** none of its own: the weights are 1 and the group's size.
- **Alternatives:** the figure by category (French's example is by type); any repeated
  type as a ground (floods C1); the ground on the gestalt group (it holds the figure too).

### D-092: ECHO and TWIN: a group's echoes and the touched object's twin across the midline
- **Kind:** re-interpretation.
- **Sources:** "each one echoes, in object type, a larger group on the other side of the
  table" [F95 p. 140]; Blockage 6's glass pairs [F95 p. 126]; "aside from literal
  sameness (which is always a pressure)" [F95 p. 28]. French's program finds same groups
  among bottom-up groups only (D-052).
- **Choice:** two comparison detectors in `correspondences.py`, beside the images
  (D-099), read by compare-roles and find-roles once a MIDLINE is perceived (D-084).
  ECHO (`echoes_of`, A6), on a group in view: every group in the other outer row band
  (`across_rows`) sharing no member with the same multiset of types (`type_multiset`) or,
  failing that, of nearest ISA categories (`category_multiset`, D-009: Surround 5's rows;
  Figure/Ground 1's rows do not echo); and, for a sameness group, every object of its
  type across. The tag names the partner and weighs the group's objects; node `echo`;
  consumer C1 (D-095). TWIN (`twin_of`, A7), on an object of the touched object's type
  across the midline, naming it; weight 1, node `sameness`; consumer C2 through the
  re-cast's third tier (D-093, `roles.is_twin`). Nothing on the touched object, a group,
  or its own row.
- **Reason:** the echo is French's word for the likeness that makes the row-to-row and
  glass-pair mappings worth building; as a role it gives C1 the group pairs the searches
  reach only through bottom-up groups. The twin makes literal sameness a perceived
  relation.
- **Knob:** none of its own: the echo weighs the group's size, the twin 1; its tier is
  `role_recast_tiers[2]` (D-093).
- **Alternatives:** echoes by category only (the type reading is stricter, so first); the
  twin on the touched object (the tag belongs to the candidate the re-cast ranks).

### D-094: TYPE_KEY: the type substitution a correspondence between two grounds makes
- **Kind:** re-interpretation.
- **Sources:** "there is a kind of temporary conceptual symmetry, in which just during
  this problem, forks and glasses are seen as 'opposites'" [F95 p. 140]. In French's
  program a type slippage feeds its labels on entry (D-030) and is forgotten.
- **Choice:** a Worldview detector, `worldview.type_keys` (K2), read on the Worldview item
  at the top context (D-021, as the axis, D-083) by read-worldview-roles (D-081). Each
  Worldview correspondence between two grounds makes one substitution (`key_of`): a
  group pair through its type slippage (two sameness groups) or else through the two
  gestalt groups' ground types (`describe.ground_type_of`, D-091: Figure/Ground 1's rows
  mapped as wholes make fork for small glass); or two GROUND-tagged objects. The reading
  is (TYPE_KEY, the two type nodes sorted, the correspondences making it), unordered.
  Consumers: C1's key pairs (`scouts.key_pairs`, D-095: the touched fork with the lone
  glass, E3) and C5's key identity (`correspondences.key_slippages`, D-098). A stale key
  is dropped by the next read-worldview-roles. Nothing is gated.
- **Reason:** the mechanism French names for the answer his program never gave. It is
  read off the grounds because the sameness subgroups the plan expected it from are never
  built before the answer (the `ch4` bench's first-bottom-up-group milestone: 0 percent of
  200 runs before and after this item, and of v1's 500, RESULTS.md §5.2); recorded, not tuned.
- **Knob:** none of its own: the weight is the count of correspondences, the jolt
  `jolt_structure` (D-016), the points D-098's `role` row.
- **Alternatives:** a key from every correspondence with a type slippage (the touched
  object's own would set it to its answer, D-083's guard); sameness groups only (the first
  case; dead alone); an ordered key (`henry` is the first end).

### D-089: BARRIER and CLAIMED: the claim on the obvious counterpart
- **Kind:** re-interpretation.
- **Sources:** Blockage: "the cup/glass and the glass/glass correspondences are competing
  with one another" [F95 p. 33]; "We therefore choose one correspondence or the other."
  [F95 p. 34]; "the natural tendency to touch whatever object on her side of the table
  has the closest conceptual resemblance to the touched object" [F95 p. 114]; the
  glass-pair correspondence "strong enough to make Eliza very reluctant to break it"
  [F95 p. 127]. Nothing stands spatially between (the author: the relational reading).
  French's program has the rivalry (D-044) and no name for the blocker.
- **Choice:** two Worldview roles with one detector, `worldview.barrier_and_claimed`
  (S5, S6), read at the top context (D-021) by read-worldview-roles (D-081) and by
  `roles.detect` on an object. The *obvious counterparts* (`worldview.obvious_counterparts`)
  are, of the first non-empty natural tier of the re-cast as it reads now
  (`roles.natural_first_tier`, D-093: off the standing ROW tags, the claim left out), the
  candidates whose type is nearest the touched object's at the Slipnet's current distances
  (D-012), ties kept; none while the rows are unread. `worldview.blockers`
  lists each Worldview correspondence that does not contain the touched object and has an
  obvious counterpart at an end, itself or inside a group mapped as a whole (`holds`): the
  other end is the BARRIER, (BARRIER, (the claimed object, the correspondence), the end's
  objects), node `barrier`; the counterpart is CLAIMED, (CLAIMED, (the barrier, the
  correspondence), 1), read only while the barrier's tag stands (a barrier that lost its
  fight with a SIDEKICK, D-079, claims nothing). read-worldview-roles reads the items
  `worldview.blocked_items` names, barriers first, proposes each, drops every standing
  BARRIER or CLAIMED the reading did not give (`broken`), jolts `barrier` by
  `jolt_structure` and posts C4's fight (D-101). Consumers: C4 for both, C2 for CLAIMED
  (D-093's last tier). Nothing is gated: no re-cast or no such correspondence, nothing
  read, no draw.
- **Reason:** French's blocker is a standing correspondence, named here so the consumers
  can keep it fighting and the answer can say it was blocked; the obvious counterpart is
  his "closest conceptual resemblance", read through the re-cast so that it is the run's
  perception, not the layout.
- **Knob:** none of its own: the weight is the end's objects, the jolt D-016's, the
  distance the Slipnet's.
- **Alternatives:** a spatial barrier (the author: no consumer, no page); the re-cast's
  tier 1 alone (the axis seldom stands before the answer); the whole first tier (a fork
  across the table would be a cup's obvious counterpart); the last published re-cast
  (nothing until recast-role ran).
- **As revised (v5, D-119):** the pool cascades the discard of a BARRIER to the CLAIMED tags
  that name it (`Workspace.discard_role`), so no claim outlives its barrier; the expulsion
  of the correspondence from the Worldview breaks neither (D-101).

### D-101: Consumer C4: the blocked counterpart
- **Kind:** re-interpretation.
- **Sources:** "This new, unanticipated pressure makes it considerably more likely that
  Eliza will wind up simply touching Henry's cup" [F95 p. 34]; the default answer, the
  touched object [F95 p. 105] (D-063); a loser "remains nonetheless in the Workspace with
  the potential to compete again" [F95 p. 72].
- **Choice:** three readings of the tags of D-089. (1) `codelets.judges.barrier_fights`:
  check-worldview, and a read-worldview-roles that read a barrier, post one pick-strong-cp
  for the correspondence each standing BARRIER tag names, each once, the tag consumed;
  pick-strong-cp with an argument (D-052 revised) picks among the challengers of that
  correspondence (`challengers_of`: outside the Worldview and in conflict with it while
  it stands; itself once it has lost its place), so the claim keeps being contested. (2)
  `answer.default_answer`: the default answer is recorded with `through` = `blocked` while
  a CLAIMED tag stands (`answer.claimed_counterpart`, the first tagged), the note naming
  the claim; the object is still the touched object; find-answer records the tag consumed;
  `invariants.check_end` checks the record (D-077). (3) The re-cast's fifth tier (D-093
  revised). Nothing is gated: with no barrier check-worldview posts as before and the
  record is `default`.
- **Reason:** the contest over the claimed object is French's Blockage; a posted fight
  makes it happen rather than wait for chance; the record lets RESULTS.md count the E3s
  that came by blocking.
- **Knob:** `Config.role_recast_tiers[4]` (0.5, the claimed tier, D-093); the fight's
  urgency is pick-strong-cp's.
- **Alternatives:** a bonus for the barrier's correspondence (a chapter 5 number by
  another name); a cp-compete posted directly (pick-strong-cp tests a challenger first);
  `blocked` as a new answer object.

### D-106: The churn bench: roles over time, and the role codelets against the find-answer flood
- **Kind:** re-interpretation (a measurement).
- **Sources:** the author's answers D1 (roles change at run time; how often, which changes
  moved the answer and whether the churn settles as T falls are measured, not assumed) and
  C8 (the role codelets against the find-answer flood of D-056; an urgency change is a
  logged decision).
- **Choice:** `tabletop/bench/churn.py`, the four carriers of the oracle probe, an
  observer (`Timeline`) on each run's events. A *change* is a `RoleDiscarded` (lost,
  broken, structure gone; D-079). Per carrier: the runs with a
  change; changes by reason and role; changes on the answer object and on the touched
  object; whether the last change preceded the last Worldview change; changes per ten
  codelets in each half of the run and by the temperature at the change (thirds of T);
  the role codelets chosen per run by type (D-081) against the find-answer codelets hung
  (D-056) and chosen; tags read and standing per role; re-casts and ties (D-093). The
  `robustness` bench (D-068) gains the same role statistics on random tables (`RoleStats`,
  with C1's proposals). The test-only D1 probe (`role_probes.LESIONS["D1"]`, D-104) runs
  check-roles with the discard left out, the coin still drawn: the paired runs whose
  answer differs are those a broken role changed. RESULTS.md §11.6, §11.7.
- **Reason:** halves and thirds need no threshold chosen after the fact; a discard is the
  one event every change shares; the paired probe needs no counterfactual per change.
- **Knob:** none in `Config` (the bench's constants and the command line).
- **Alternatives:** a `RoleTagged` as a change; deciles of T.

## The fast engine (D-107 on)

### D-107: Two engines: the compiled one the reference, the pure one on request only
- **Kind:** engineering.
- **Sources:** French says nothing about implementation; the profile (RESULTS.md, "Engines
  and speed") puts `Slipnet.update` at three quarters of a run. The author's answers
  (2026-10-14), in substance: what is built departs from French's original anyway, so no
  bit-for-bit Python is needed (the pure Python is a draft) and the generator should be the
  fastest; the compiled build becomes the reference, in a short loop of its own; no fallback
  to the pure Python, which an optimisation study might pick up by some chain of events:
  better an error at start than two days of lost compute.
- **Choice:** the *compiled* engine, built from `tabletop/_native/` by `python3
  tools/build_fast.py`, is the reference: its runs are the package's, its goldens
  (`tests/data/golden/compiled/`) and bench numbers the ones reported. The *python* engine
  (`rng.py`, `slipnet.py`, ...) stays as the specification and the oracle of the equivalence
  test, with its own goldens (`tests/data/golden/python/`). `tabletop/accel.py` chooses once
  per process from `TABLETOP_ENGINE`: unset, empty or `compiled` is the compiled engine,
  required (a missing, failing or stale build is an error at import naming the build
  command); `python` is the pure engine; anything else is an error naming the two legal
  values. Nothing else in the package reads or sets the
  variable or catches the error to carry on. Every result records its engine (`Result`, `RunStarted`,
  the CLI's and the benches' first line, the bench JSON).
  Bit-identity is not required; each engine's determinism and goldens and their
  statistical equivalence (D-113) replace it.
  `tools/workload.py` and `tools/paired_timing.py` name the engine that ran (before the
  seam, `python`).
- **As revised (loop0008, item 02):** built: `accel.py` is the seam, `_native/rng.pyx`
  the first compiled module (D-109), `setup.py` and `tools/build_fast.py` the build
  (D-110); the goldens are per engine (`tests/data/golden/compiled/`, `python/`), read by
  the tests of the engine running.
- **As revised (loop0008, item 03):** `accel.Slipnet` is the native class of
  `_native/slipnet.pyx` on the compiled engine (D-111), and the seam checks the stamp of
  every module of `_native/`.
- **As revised (loop0008, item 04):** the seam also exports `chances_of`, the Coderack's
  chances (D-112: C on the compiled engine, `temperature.chances_of` on the pure one);
  `_native/MODULES` is `rng`, `slipnet`, `coderack`, each stamped and checked.
- **As revised (loop0008, item 05):** the equivalence test is D-113's: `tools/equivalence.py`
  runs the engines in two processes and holds every problem's TV within 0.25 at 200 runs.
- **As revised (loop0008, item 06):** the GUI starts the engine once Qt is up, refuses a
  missing build in a dialog (stderr under `--smoke`) and names the engine in its status bar
  (G-063); `pip install .` builds the extension, failing clearly with no compiler;
  `tools/standalone_check.sh` checks the refusal without a build, builds where it can and
  tests the pure engine by explicit request alone; `tools/run_on_baseline.py` builds the
  compiled engine in its worktree, the pure one only on `--engine python`.
- **As revised (loop0008, item 07):** every bench re-run at full size on the compiled
  engine replaced `results/NAME.json` (RESULTS.md); `tabletop.bench.calibrate` names and
  records its engine as the benches do; `strip` keeps of a record's `accel.info()` the
  engine's name (the whole is in the bench's JSON; 477 copies would overflow
  the 300 KB file limit) and every generated block begins with an
  engine line. The previous records differed by loop0007's last engine change, not by the
  engine (max TV 0.058, under D-113's 0.25).
- **Reason:** the author's answers; one place deciding per process is the simplest to audit,
  and an error at start costs minutes where a silent fallback costs a study.
- **Knob:** the environment variable `TABLETOP_ENGINE` (`compiled` or unset; `python`), read
  in `accel.py` alone: not a `Config` field, so that no optimisation, profile or lock can
  select the engine; not an argument, since one process runs one engine.
- **Alternatives:** the pure engine as a fallback, or dropped (both refused); a `Config`
  field or a command-line option.

### D-108: The update's change report, worked out when read
- **Kind:** engineering.
- **Sources:** the pre-loop profile (RESULTS.md, "Engines and speed"): `round`, in the
  report of every Slipnet update (D-019), was 6 percent of a run that no observer watched;
  D-065 says a publisher with no observer builds nothing.
- **Choice:** `Slipnet.update` returns an `UpdateReport` holding the activations and
  effective lengths before and after the update; its `changed` and `lengths` (the nodes and
  links that moved to a tenth, in definition order) are computed on first read and kept.
  `World._announce_update` reads them only when the publisher is listening
  (`Publisher.listening`), so an unobserved run never rounds. What is read is exactly what
  the eager report gave, the same rounding of the same values, so the `SlipnetUpdated`
  events, the trace and the fingerprints are unchanged (checked against the pre-loop
  goldens and the three workload digests of the plain-Python item). That item's other
  speedups change no result either: the distance matrix and the neighbour lists (D-012),
  spreading over the neighbours alone (D-013), the salience weights cached per weight
  table and the problem's ids listed once (D-031), and loops in place of generator
  expressions in `Workspace.has_role` and `describe.is_stray`.
- **Reason:** the report is for observers; computing it for nobody was the largest
  avoidable cost outside the Slipnet's arithmetic, and deferring it leaves the mechanism of
  D-019 as it was.
- **Knob:** none: the report is identical whenever it is read, so there is nothing to tune.
- **Alternatives:** rounding only the nodes whose value moved (the same saving, more code);
  a flag on `update` saying whether a report is wanted (two code paths).

### D-109: The generator stays splitmix64, in native arithmetic on the compiled engine
- **Kind:** engineering.
- **Sources:** D-064 (splitmix64, its vectors checked against an independent SBCL
  implementation); the author's answer that the generator should be the fastest possible
  (D-107); the pre-loop profile (the generator under 2 percent of a run). A micro-benchmark in C (gcc 15.2, `-O2`, no `-march`, one pinned core at `nice`
  10, 400 M draws, three repeats, load 15 on 32 cores, 2026-10-15): splitmix64 1.80 to
  1.94 G draws/s, xoshiro256++ 1.78, PCG64 (XSL-RR 128/64) 0.84.
- **Choice:** splitmix64 on both engines. `tabletop/_native/rng.pyx` mirrors `rng.py`
  method for method in C unsigned 64-bit arithmetic (the step and the two multiplications
  wrap modulo 2**64, which is what the Python's masks compute; `below` keeps the rejection
  rule, written as x < 2**64 - 2**64 mod n in 64 bits), so the two engines draw identical
  streams: the same outputs, rejections, floats and draw counts (`tests/test_rng.py` checks
  both), and `run_seeds` gives a batch the same seeds on both. A run draws about two thousand numbers; the native draw costs 31 ns
  against the pure 480 ns (`timeit`, one core).
- **Reason:** the fastest of the three in C is the one already there, with its vectors and
  its oracle; keeping it makes the compiled runs, while the Slipnet is still pure, identical
  to the pure engine's (the three identity digests of the plain-Python item reproduced), so
  the only difference the record shows is the engine's name.
- **Knob:** none: a fixed choice; a generator is not a parameter of the model, and the two
  engines must agree on it.
- **Alternatives:** xoshiro256++ (as fast; no reason to change); PCG64 (half the speed).

### D-110: The build: pinned flags, the stamp, a stale build refused
- **Kind:** engineering.
- **Sources:** D-107; the Cython prototype's record that motivated the loop: a fused
  multiply-add changes the last bit of `a * b + c`; the goldens must hold on every x86-64
  machine.
- **Choice:** `setup.py` builds `tabletop/_native/` (`pip install .`, or `python3
  tools/build_fast.py [--jobs N] [--cython PATH]`, which runs it with at most 4 jobs and
  then starts the engine once to check that the seam accepts the build). The flags added to the interpreter's own are pinned in
  `tabletop/_native/__init__.py`: `-O2 -ffp-contract=off`; never `-ffast-math` (the
  distances use `math.inf`), never `-march=native`. The build stamp is the SHA-256 of the
  `.pyx` and `.pxd` sources (names and bytes) and the flags, embedded in every module as a C
  string macro beside the flags, the compiler's version line and the Cython version;
  `accel` compares it with the sources beside the extension at import and refuses a
  mismatch, naming the build command, as it refuses a missing build. Without Cython (the
  module, or the executable of `--cython`, `TABLETOP_CYTHON` or PATH, which only generates
  C) or a C compiler, the build stops with a message naming what is missing and leaves no
  artefact. The generated C, the extensions and `build/` are git-ignored. `accel.info()`
  records the stamp, the flags, the compiler and the Cython version.
- **Reason:** a stale extension would run old code silently, the one failure the author's
  answers forbid; pinned flags keep the compiled floating point the same on every machine.
- **Knob:** `--jobs` (at most 4: the machine is shared) and `--cython`; the flags are not a
  knob (a change is a new stamp, and a decision).
- **Alternatives:** a stamp from the extension's mtime (fooled by copies); `-O3` or
  `-march=native` (a little faster, goldens per CPU); cythonizing the `.py` modules in
  place (a stale build would shadow the Python silently).

### D-111: The Slipnet in C arrays, the Python's operations in the Python's order
- **Kind:** engineering.
- **Sources:** the profile after the plain-Python speedups (RESULTS.md, "Engines and
  speed"): the Slipnet's update still a third of a run, its spreading loop the largest
  single cost; the author's answers (D-107): bit-identity with the pure Python is not
  required.
- **Choice:** `tabletop/_native/slipnet.pyx` is a `cdef class Slipnet` holding the
  activations, the depths, the effective lengths, the posting levels and the n x n distance
  matrix in C `double` arrays, with the link table, the pathways and the neighbour lists
  flattened into C index arrays at construction, and spreading, the trickle, decay, the
  clip, the lengths and the distances as C loops; `update`, `start`, `top_down_postings`,
  `reset`, `restore`, `spread_step_into` and the whole reading interface of `slipnet.py`
  are the Python's, method for method, each naming its mirror. The seam serves it as
  `accel.Slipnet`; the pure class stays the specification, on request. Though not
  required, it performs the Python's floating-point operations in the Python's order (the
  amounts summed in the neighbours' order, the reciprocals path by path, no fused
  multiply-add under the pinned flags, D-110), so its runs are the pure engine's bit for
  bit: D-108's identity digests and both engines' goldens are unchanged by it;
  `tests/test_slipnet.py` runs every unit test on both
  classes, `tests/test_native_slipnet.py` a run of jolts, updates, resets and postings
  for equality (1e-9 asked; identical). Where it differs from the Python:
  the update's scratch buffers are allocated once with the network; the report copies the
  activations and lengths into C arrays and works `changed` and `lengths` out with Python's
  `round` when first read (D-108); `activation`, `effective` and `distances` are writable
  views on the arrays where the Python has lists.
- **Reason:** the Slipnet's arithmetic is where the pure engine spent its time, the one
  part of a run with no Python object in it; keeping the Python's order of operations costs
  nothing in C and keeps the engines identical until something else departs, so every
  later divergence is attributable to its item. Measured
  (RESULTS.md): one update 364 µs pre-loop, 90 µs pure after D-108, 1.6 µs native; the
  paired workload x5.21 CPU over the pre-loop engine.
- **Knob:** none: the mechanism is D-011 to D-021's, with their knobs; which class runs is
  the engine's choice (D-107).
- **Alternatives:** typing the Python with `.pxd` files alone (x2 at most, the data
  structures the Python's); C loops in any order (last bits of difference, for nothing);
  recomputing only the distances whose links changed (another order of sums; the lengths
  move at nearly every update).

### D-112: The typed hot spots after the native Slipnet: one, and six plain-Python fixes
- **Kind:** engineering.
- **Sources:** the profile of the compiled engine after D-111 (RESULTS.md, "Engines and
  speed"; 34 problems x 10 runs, cProfile and a sampling profile): no Python function at
  more than 8 percent of a run, the Slipnet out of the top twenty, and what is left the
  run loop's object work.
- **Choice:** every function at 3 percent or more of the sampled run (self time), with its
  verdict. *Typed mirror:* the Coderack's chances (`chances`, `softened_weights` and the
  draw's `choose`, 5 to 7 percent together, 150 powers and a sum at every codelet):
  `_native/coderack.pyx`, `chances_of`, mirroring `temperature.chances_of` (where the
  function now lives, so that the seam can export the pure one without a cycle) with the
  Python's floating-point operations in the Python's order, the total by the compensated
  summation Python's `sum` applies to floats, the overflow rule of `softened_weights`
  included. *Plain-Python fixes, each leaving the mechanism readable and every run
  unchanged:* `objects.footprint` remembered per (type, orientation) (7.6 percent, D-002);
  the role pool indexed by item for `has_role` and `roles_of` (3.8 and 3.2 percent,
  D-079); `salience.weight` reading the configuration's dict by name (3.6 percent, D-031);
  `roles.detect` reading the registry's distinct detectors, with their modules, once (3.1
  percent; the function is still looked up by name at every call, so a probe or lesion
  that replaces it in its module is what runs); `Problem.object` through an
  index by id (3.1 percent, D-003); `Coderack.remove_aged` in one pass, each codelet's
  age read once (2.4 percent with `age`, D-053); `geometry.region_weight` reading the
  preference table's dict built once per table (D-032). *Nothing:* `World.step` (4.9
  percent, the run loop's bookkeeping; a C mirror would be the whole engine); the
  constructors of the frozen events and records (4.5 percent; the typed, frozen events are
  the record itself); `geometry._third` (2.4 percent, one expression, whose cost is the
  call); salience's terms (a fifth of a run inclusive, but object traversal, not
  arithmetic: a mirror would duplicate D-031's one function per factor). No `.py` module
  is compiled in place: a compiled module beside its `.py` would make a stale build
  silent, which D-110's stamp exists to prevent.
- **Reason:** after D-111 the profile is flat and bound by Python calls and attribute
  reads, where a Cython `def` saves little (it still pays the call) and a C mirror of
  object code would copy what the specification keeps readable; what pays is not asking
  the same question twice (the caches and indexes, plain data structures) and the one
  numeric loop left (the chances). The runs stay the pure engine's bit for bit (D-108's
  identity digests reproduced on both engines, the goldens unchanged), so the GUI's
  pinned facts stand. Measured
  (RESULTS.md): 34 x 10 on one core 2.6 s after D-111, 2.2 s after the plain fixes, 2.0 s
  with the typed chances; paired x6.28 CPU [6.21-6.67] over the pre-loop engine (34 x 20,
  3 repeats; D-111's engine x5.28 in the same session); the pure engine x2.69, from x2.32.
- **Knob:** none: nothing here is a parameter of the model; the engine is D-107's choice.
- **Alternatives:** typed mirrors of salience's terms, `has_role`, `is_stray` and the
  geometry (each under 4 percent, call-bound, object code); cythonizing whole modules in
  place (silent staleness; a throwaway snapshot may, D-110); an unguarded index of the
  pool (a hand edit of `roles` would go unseen).

### D-113: The equivalence of the two engines: total variation at 200 runs, tolerance 0.25
- **Kind:** engineering.
- **Sources:** D-107 (bit-identity between the engines is not required; their answer
  distributions must agree within a stated tolerance); French's reading of a program's behaviour
  as answer frequencies over a batch [F95 p. 115], as the benches tally them (D-072).
- **Choice:** `tools/equivalence.py` runs every families problem N = 200 times on each
  engine, each in a fresh process (D-107; the pure one on explicit request), on
  the families bench's own seeds (bench seed 1001, D-072, D-064), and reports per problem
  the total variation (TV, half the sum of the absolute differences of the answer
  frequencies) between the two engines' distributions, with the mean. The tolerance is one
  number for every problem, **TV 0.25 at N = 200**, derived from the null *before the
  comparison*: the compiled engine's next 200 seeds give a second, independent
  sample per problem (observed null TV mean 0.085, max 0.150), and
  2,000 pairs of multinomial samples of 200 drawn from each problem's pooled 400 runs
  (`random.Random`, seed 20261015) give the null's percentiles: the 99th at most 0.180,
  the 99.9th at most 0.230 over the 34 problems (buridan-01), and no pair of 5,000 x 34
  beyond 0.25: under the null the whole table passes with a family-wise false alarm below
  one in ten thousand (0.20 would fail it once in ninety). The slow test
  (`tests_slow/test_equivalence_slow.py`) runs the tool at these defaults, every problem
  within the tolerance; the fast test (`tests/test_equivalence.py`) runs a few
  problems x 30 runs on the compiled engine against the pure engine's recorded 200-run
  distributions (`tests/data/equivalence.json`, from the tool, naming engine, commit and
  date), with the tolerance 30 against 200 allows: the same simulation's 99.9th percentile
  per problem (about 0.4). The pure engine's own determinism is its goldens in two
  processes (`tests/test_determinism.py`). A problem beyond it is a finding (RESULTS.md).
- **Reason:** the comparison must be read as distributions (the author's answers: the
  compiled engine may depart from the pure one), so the measure is the one the benches
  already use to put a survey beside French's numbers, and the tolerance comes from the
  sampling noise at N, not from a hoped-for closeness. As this loop leaves the engines,
  their runs are identical seed for seed, so every TV is 0.000 and only the null has
  content; the tolerance is for the day they diverge.
- **Knob:** `--runs` (RUNS = 200) and `--tolerance` (TOLERANCE = 0.25) of the tool, module
  constants, with `NULL_DRAWS` (2,000) and `NULL_SEED`; the tolerance holds at RUNS only.
- **Alternatives:** a chi-square homogeneity test with a Bonferroni correction over the 34
  problems (a p-value rather than a distance a reader can picture; sparse rare-answer
  cells); a tolerance per problem (34 numbers to maintain); comparing the engines seed for
  seed (bit-identity by another name, which D-107 drops).

## v3 (D-114)

### D-114: The optimized defaults (v3): 67 knobs from the Bayesian optimization
- **Kind:** re-interpretation.
- **Sources:** the cross-validated Bayesian optimization, study of 2026-10-15/16 (engine
  `b70f110`): J = (2F + H) / 3 over French's 37 sets and 28 human data, five folds; **fold 1
  trial 244**, the best J past the mechanism floor. Held-out J **0.3156 (sd 0.0559), v2
  0.4679** (RESULTS.md, v3).
- **Choice:** `standard` takes the trial's 67 values (v2 → v3, four digits, in the table
  moved for size to DECISIONS-INDEX.md, "D-114's table"; exact in `config.py` and
  `V2_DEFAULTS`); the other profiles keep their overrides.

- **Reason:** the engine is not frozen (the author); the gain is held out. The entries
  keep their a-priori values as v2's; v3 is fitted, French's constants included, faults 5,
  2, 3, 6 in part. Trade-offs: RESULTS.md.
- **Knob:** the 67 fields; v2 is `dataclasses.replace(Config(), **dict(V2_DEFAULTS))`.
- **Alternatives:** fold 2 trial 250 (fails the floor); the trial as a profile.

## v4 (D-115)

### D-115: The second optimization's defaults (v4): 65 knobs from fold 1 trial 800
- **Kind:** re-interpretation.
- **Sources:** the second Bayesian optimization (2026-10-17, engine `e320e1c`, v3 as
  `standard`, a no-regression penalty), **fold 1 trial 800**: in sample J 0.2113 against
  v3's 0.2629; **held out 0.2567 (sd 0.0726) against v3's 0.2479**, penalty 0.0358 against
  0.0207 (RESULTS.md §13).
- **Choice:** `standard` takes its 65 values (v3 → v4, four digits, in the grouped table
  that stands for size in DECISIONS-INDEX.md, "D-115's table"; exact in `config.py` and
  `V3_DEFAULTS`); `temperature_update_every` (D-050, 5 → 3) joins `V2_DEFAULTS`; the
  profiles keep their overrides. The largest moves: `pop_fixed_amount` 56 → 98,
  `removal_threshold` 75 → 126, `coderack_capacity` 179 → 260, `temperature_alpha` 304 →
  634, `parallel_points` 16 → 6, `role_break_scale` 261 → 155, `spread_radius` 89 → 139.
- **Reason:** the author's, knowingly: the in-sample gain and a richer mechanism, though
  the held-out estimate does not beat v3's.
- **Knob:** the 65 fields; v3 is `dataclasses.replace(Config(), **dict(V3_DEFAULTS))`.
- **Alternatives:** keeping v3, the better held-out estimate.

## v5 (D-116 to D-120): the structural Blockage fix

### D-116: The Hobj status counts in every rating
- **Kind:** re-interpretation (revising D-037 as built).
- **Sources:** the correspondence containing the touched object is "given a special
  status" and "receives points" [F95 p. 69]; "The dynamic part of the evaluation of a
  correspondence can be done only when there are other correspondences in the Worldview."
  [F95 p. 69]; the answer is read off that correspondence [F95 p. 105]; "Eliza's attention
  is focused on Henry's cup" [F95 p. 33]; Blockage 2's glass-glass blocker is "relatively
  weak" and wins 12 percent of the time [F95 p. 123].
- **Choice:** `correspondences.dynamic_terms` gives the Hobj points (`cp_dynamic_points`,
  row `hobj`) to a correspondence that contains the touched object in every rating it
  enters, a Rival Worldview in which it stands alone included; the two neighbour terms keep
  p. 69's condition. The status is the correspondence's own, not a relation to another.
- **Reason:** the study of 2026-10-17 (RESULTS.md, v5): in Blockage 2 the Rival Worldview
  {cup=glass} against the Old {glass=glass} rated the cup's correspondence alone, so D-037
  as built took its Hobj points away in exactly the contest French describes, and the
  blocker, a type identity with a diagonal placement, outrated it by its statics alone (63
  against 55 under v4): the first contest was a coin at 0.4-0.6 in the blocker's favour and
  the cup was blocked in 66 percent of runs against his 12. In Surround both rivals usually
  contain the touched object, so the points cancel there. p. 69's "only when there are other
  correspondences" is read as covering the relational terms, which have nothing to relate
  to without another.
- **Knob:** none of its own; `Config.cp_dynamic_points` (D-037).
- **Alternatives:** a separate status term; FCCA's weakening by a different neighbour.

### D-117: Inertia per rivalry
- **Kind:** re-interpretation (revising D-038).
- **Sources:** after a competition "it becomes a little harder for the loser to compete the
  next time around" [F95 p. 72]; a repeated loser fades and "is (probabilistically) removed
  from the Workspace and then must be built again" [F95 p. 73]; "We want one structure to
  gradually pull ahead while the other gradually falls behind." [F95 p. 74].
- **Choice:** `correspondences.award` takes the fight's rivalry (the newcomer and its
  rivals, `worldview.compete`) and awards `win_bonus` or `loss_penalty` once per
  correspondence, outcome and rivalry (`Workspace.awarded`); a rematch of one rivalry is
  decided by the ratings as they stand plus that one step of inertia. Wins and losses are
  still counted every time, for the ends' salience (D-031). A correspondence still pulls
  ahead against a new challenger, a new rivalry.
- **Reason:** D-038 as built bounded nothing: in Blockage 2 seed 2 under v4 ten rematches of
  one rivalry took the cup's correspondence from -12 to -66 and the blocker to +33, the
  Rival Worldview's rating 49 to 0 and the chance of its return 0.23 to 0.000, so a 0.6 coin
  became certainty in two or three rematches; over 100 runs the cup's correspondence won
  9.9 percent of 414 rematches at a median chance 0.028 (RESULTS.md, v5). French's loser
  fades to removal and rebuilding (D-041); it does not carry a growing tally.
- **Knob:** none of its own; `Config.win_bonus`, `Config.loss_penalty` (D-038).
- **Alternatives:** a cap on the bonus; a bonus proportional to the margin; one proximity
  coin per pair per run (prototyped and rejected: it cut Blockage 6 and 8 by 26-27 points).

### D-118: The proximity law is bounded by Blockage 10
- **Kind:** re-interpretation (revising D-022 and the optimizations' search space).
- **Sources:** "The gap in Blockage 10 is so wide that the program no longer perceives the
  glasses as forming a group." [F95 p. 129]; the gaps of Blockage 8-10 [F95 p. 128]; the
  digitized gap, 2.64 small-glass widths (D-003, D-022).
- **Choice:** `config.PROXIMITY_ZERO_GAP_MAX` = 2.6 widths; the `__post_init__` of `Config`
  refuses a `proximity_zero_gap` above it, as it refuses a role knob at zero (D-078), so
  `groups.link_probability(2.64)` is 0 under every configuration. The default is the bound,
  the largest value allowed (v4's 5.4786, fitted, linked Blockage 10's pair with p 0.53 per
  look and grouped it in 93-96 percent of runs). The optimizations' search space is
  discovered from `Config` at launch, so the bound binds it too. v3's and v4's values can no
  longer be built as a `Config`; `results/v3/` and `results/v4/` stand as records.
- **Reason:** a bound from the text, not a fitted value; no mechanism can keep Blockage 10
  ungrouped while the knob links its gap half the time and the coin is re-drawn at every
  look, so that over a run any p above 0 tends to 1 (RESULTS.md, v5).
- **Knob:** `Config.proximity_zero_gap` (2.6, the bound).
- **Alternatives:** a hard threshold at the gap; one coin per pair per run (D-117's rejected
  alternative); leaving the knob free and the statement unmet.

### D-119: Discarding a BARRIER discards the CLAIMED tag that names it
- **Kind:** re-interpretation (revising D-089 as built).
- **Sources:** D-089's reading, a claim read "only while the barrier's tag stands"; a
  structure standing on nothing has "no action at all" [F95 p. 73].
- **Choice:** `Workspace.discard_role` cascades: when a BARRIER tag goes, for whatever
  reason (check-roles breaking it, a SIDEKICK beating it, read-worldview-roles dropping it),
  every CLAIMED tag whose arguments name that barrier and its correspondence goes with it,
  for the same reason and by the same codelet, as `discard_roles_on` already takes both
  when the correspondence leaves the Workspace. Nothing breaks at the correspondence's
  expulsion from the Worldview: the expelled blocker keeps its tag until the next
  read-worldview-roles, so that consumer C4 keeps posting its fight, "itself once it has
  lost its place" (D-101).
- **Reason:** every invariant violation of the robustness bench under v3 and v4 (176 on 6
  tables, 367 on 7 of 500) was a CLAIMED tag outliving its broken BARRIER; the answer never
  read the orphan (only the `blocked` record and C2's fifth tier do), and the prototype
  cascade gave the same answer on 97-100 of 100 paired seeds on every problem (RESULTS.md, v5).
- **Knob:** none.
- **Alternatives:** breaking both at the expulsion (rejected: it would end C4's fight for
  the blocker's return); re-reading the claim at every check-roles.

### D-120: A time pressure on the answer that does not depend on T
- **Kind:** re-interpretation (revising D-056).
- **Sources:** beside the pressure to maximize structure-value "is a competing pressure -
  time pressure - that pushes for the program to finish within a reasonable amount of time."
  [FCCA p. 387]; "the default object corresponding to the touched object is the touched
  object itself" [F95 p. 105]; the find-answer law [F95 p. 94] (D-056); the cap (D-061).
- **Choice:** `World.time_pressure`: with `idle` the codelets since the Worldview last
  changed (since the start while it never has), the pressure is 0 up to `answer_patience`
  codelets and rises linearly to 1 at twice it; `World.schedule_find_answer` reads T x (1 -
  pressure) in both laws of D-056, so a run under no pressure is the same run, draw for
  draw. `answer_patience` = 1000: the longest idle stretch over 3,400 runs of the 35 result
  sets and 100 random tables was 251 codelets (99th percentile 198), so no run where
  structure forms is touched; a one-object table answers around 1,400 codelets, the default
  answer, well before the cap of 3,000.
- **Reason:** D-056 posts about 1e-5 find-answers per codelet at the starting temperature,
  so a table where no Worldview change can happen (chapter 2's Figure 2.2, one object;
  junk-flooded tables) ran to the cap: 403 and 484 of 500 runs under v3 and v4, and 1 and 4
  random tables (RESULTS.md §13). FCCA's time pressure is the second pressure the program
  is under; reading it off the Worldview's own quiet, not off T, keeps p. 105's default
  reachable without changing the law French's dumps support. Checked on the families bench
  against the pressure switched beyond the cap (RESULTS.md, v5).
- **Knob:** `Config.answer_patience` (1000 codelets; refused at zero or below).
- **Alternatives:** a find-answer posted when nothing is creatable (a table whose
  correspondences are built and fail for ever would still cap); the rate rising with the
  codelets since any structural change (the same churn would reset it); a lower cap.
