# The digitized problems: method, scale, uncertainty and review

How the table configurations of French's figures became the JSON files of `problems/`
(D-001 to D-003 in `DECISIONS.md`; ANSWERS.md 4 allowed the digitization and asked for the
method and the uncertainty). This file is the record: the method step by step, the scale, the
footprints, the measured uncertainty, the review by eye of every file, the cross-check against
FCCA's redrawn figures (6), the one configuration printed twice (7), the gaps of Blockage
8-10 (8), chapter 2's nineteen configurations with French's expectations (9) and the file
format with what the engine reads from it (10). The figures
are copyrighted and are read in place; no image is kept in the repository. The 53 files are
the four families of chapter 5, Surround 1-10, Blockage 1-10, Buridan 1-13 and Figure/Ground 1
[F95 pp. 115-139], and the `ch2` family, Figures 2.2-2.20 [F95 pp. 27-35].

## 1. The sources

The configurations survive only as drawings. Chapter 5 prints one figure per result set: the
table as a frame at the left with "Henry" above it and "Eliza" below it, the objects as small
icons, "the object touched by Henry is designated by an arrow marked H", Eliza's answers
"labeled E1, E2, and so on" [F95 p. 114], and the bar graph of frequencies to the right [F95
p. 115 (img)]. In the PDF each figure is an embedded JPEG of 487-496 × 200-207 pixels at 135
ppi (`pdfimages -list`), the same scale as the palette of Figure 2.1 (188 × 222 pixels, 135
ppi): a fork is 6 × 17 pixels in both [F95 p. 26 (img); F95 p. 118 (img)]. The Surround family
is on pp. 115-121 (PDF pages 125-131; the chapter's offset is 10): Surround 1 p. 115, 2 and 3
p. 116, 4 p. 117, 5 p. 118, 6 and 7 p. 119, 8 and 9 p. 120, 10 p. 121 (the tables of numbers of
Surround 3 and 9 are on the next page, which is the page `reported.py` cites for them).
The other three families follow on pp. 122-139 (PDF 132-149), two figures to a page where
two fit: Blockage 1 and 2 p. 122, 3 p. 123, 4 p. 124, 5 p. 125, 6 p. 126, 7 and 8 p. 127, 9
p. 128, 10 p. 129; Buridan 1 p. 129 (and again p. 130, section 7), 2 p. 131, 3 and 4 p. 132, 5
p. 133, 6 p. 134, 7 and 8 p. 135, 9 p. 136, 10 and 11 p. 137, 12 p. 138, 13 p. 139;
Figure/Ground 1 p. 139. Each file's `source` gives the figure's printed page, its PDF page and
the index `pdfimages` gives the image (the even indices; the odd ones are the JPEGs' masks;
Figure 5.1 on p. 124 is image 6, Figures 5.2-5.3 are on p. 141). Where the figure's page and
the page of its table of numbers differ (Blockage 2, 4, 5, 8, 9 and Buridan 8), the file
cites the figure's page and `reported.py` the table's.

FCCA redraws six of the ten Surround layouts, Figures IX-1a to IX-1f (Surround 1, 2, 4, 5, 8,
10) [FCCA pp. 394-395 (img)]; they are the cross-check of section 6.

## 2. The method

Every step is a command or a function of `tools/measure_figure.py` (standard library only; it
reads and writes PNGs with `zlib`), so that a second reader gets the same numbers.

1. **Extract** the figure: `pdfimages -f N -l N -png "<F95>" /tmp/tt/s/img` (N the PDF page);
   the even-numbered files are the figures, the odd ones their one-bit masks. Never anywhere
   but `/tmp`.
2. **Find the frame**: `find_frame` takes the darkest column of the image's left eighth
   (left), the darkest column between three-tenths and twelve-twenty-fifths of the width
   (right; the bar graph's axis lies further right), and the darkest rows of the top and
   bottom quarters (top, bottom), each averaged over the span the other pair encloses. The
   JPEG smears the one-pixel frame to a faint grey (mean grey 60-215 against a page of 245),
   which is why profiles are used rather than runs of black. The frames come out at 191-195
   × 174-176 pixels inside their lines, 1.10 times wider than tall (D-001). Two figures print
   lines too faint even for the profile: Blockage 9's left line (mean grey 233; the profile
   picks the E1 label's column) and all four of Buridan 12's (none darker than 230, so
   `find_frame` reports no frame). For these the frame is given by hand, `--frame L,T,R,B`,
   from the profile's darkest candidates read off a dump (Blockage 9: `--frame 5,14,200,190`;
   Buridan 12: `--frame 11,14,205,189`); each file's `notes` say so.
3. **Cluster the ink**: `components` lists the 8-connected clusters of pixels darker than grey
   level 200 strictly inside the frame (a 2-pixel margin inside the lines is ignored); at the
   default threshold of 128 the thin handles of the silverware vanish, at 200 every icon is
   whole. `merge_close` merges clusters whose boxes lie within 2 pixels (a fork's tines and
   handle, a cup's body and handle); neighbouring icons stand 4 pixels or more apart and stay
   separate. The command is
   `python3 tools/measure_figure.py /tmp/tt/s/img-008.png --dark 200 --merge 2`.
4. **Resolve the arrows**: the H arrow enters the touched cup from the upper left and the E
   arrows enter their objects from the lower right, so those clusters hold the icon, the
   arrow's head and sometimes its label and a neighbour. For each such cluster the region was
   printed as an ASCII dump (`#` below 128, `+` below 200) and the icon's own outline read off
   it: the cup's rim row and bottom row (14 × 7 pixels), the glass's rim and body (8 × 10), the
   saucer's circle (14 × 13), the plate's circle (19 × 19), the shaker's rounded box (12 × 15).
   The arrow is left out. Each file's `notes` say where this was done.
5. **The position** is the centre of the icon's box, as a fraction of the frame's inner width
   and height, times 110 and 100 (D-001), rounded to a tenth of a unit; the box itself is
   kept in the file (`box_px`) with the frame (`table.frame_px`), and
   `tabletop.problems.validate` recomputes every position from them.
6. **Orientation** of the silverware was read on the enlarged image
   (`--scale 4 --out /tmp/tt/big.png`): tines, blade and bowl at the top in every Surround
   figure, `up`; in the Buridan and Figure/Ground figures every piece of silverware points up
   too (the spoons' bowls and the forks' tines at the top).
7. **Ids** are the type's stem and a number (`fork1`, `small-glass1`), numbered in reading
   order, Henry's row first, then Eliza's, then the loners. A variant French describes as
   another problem changed (Surround 6-10 from Surround 5, `based_on`) keeps that problem's
   ids and numbers its additions after them, so that `cup2` is the same cup in Surround 5, 7,
   8, 9 and 10 and the row cup of Surround 6, where it was "switched" with the glass [F95
   p. 119]. In Blockage the base is Blockage 1 (`cup1` touched, `small-glass1` Eliza's) and
   each problem is based on the one before it (Blockage 8-10 on Blockage 6): `small-glass2`
   is the upper-right glass of Blockage 2-10, `small-glass3` the second lower-left glass of
   Blockage 3-10, `spoon1`, `cup2` and `small-glass4` the upper-right glass's successive
   companions, `fork1`, `knife1`, `fork2`, `knife2` Blockage 7's silverware. In Buridan the
   base is Buridan 1: `cup1`, `cup2`, `spoon1` (touched) on Henry's side, `spoon2` (E1),
   `small-glass1`, `small-glass2` lower left, `cup3`, `cup4`, `fork1` (E2) lower right; a
   variant that keeps one of a pair keeps the one nearer the silverware (`cup2`, `cup4`,
   `small-glass1`), and replacements are numbered after the base (`cup5`, `salt-shaker1`,
   `plate1`, `spoon3`). Buridan 10's three pieces of silverware are the base of Buridan 11
   and 12, as French describes them [F95 pp. 137-138].
8. **Render and review**: `tools/render_problem.py problems/surround-05.json:/tmp/tt/s/img-008.png
   --out /tmp/tt/review.png --scale 2` draws the figure's frame enlarged with every object's
   box outlined in its type's colour beside the layout drawn from the file alone (the 110 × 100
   table, the nine regions faint, each object as a filled rectangle of its footprint, the
   touched object with a black border and the answers with a red one). The two panels must
   show the same picture. Section 5 records the review.

## 3. The scale and the units (D-001)

The ten Surround frames measure, inside their lines (left, top, right, bottom in pixels):

| Problem | Frame | Inner size |
|---|---|---|
| Surround 1 | 6, 15, 200, 191 | 193 × 175 |
| Surround 2 | 9, 15, 205, 192 | 195 × 176 |
| Surround 3 | 10, 15, 203, 191 | 192 × 175 |
| Surround 4 | 9, 12, 201, 188 | 191 × 175 |
| Surround 5 | 9, 14, 203, 190 | 193 × 175 |
| Surround 6 | 9, 14, 202, 190 | 192 × 175 |
| Surround 7 | 13, 16, 206, 192 | 192 × 175 |
| Surround 8 | 10, 14, 204, 190 | 193 × 175 |
| Surround 9 | 12, 13, 206, 189 | 193 × 175 |
| Surround 10 | 11, 11, 204, 187 | 192 × 175 |

| Blockage 1 | 10, 14, 205, 191 | 194 × 176 |
| Blockage 2 | 8, 13, 202, 190 | 193 × 176 |
| Blockage 3 | 7, 12, 201, 188 | 193 × 175 |
| Blockage 4 | 10, 16, 205, 192 | 194 × 175 |
| Blockage 5 | 9, 14, 203, 190 | 193 × 175 |
| Blockage 6 | 5, 11, 201, 187 | 195 × 175 |
| Blockage 7 | 9, 13, 203, 190 | 193 × 176 |
| Blockage 8 | 11, 16, 206, 192 | 194 × 175 |
| Blockage 9 | 5, 14, 200, 190 (by hand) | 194 × 175 |
| Blockage 10 | 11, 14, 205, 191 | 193 × 176 |
| Buridan 1 (p. 129) | 7, 14, 199, 189 | 191 × 174 |
| Buridan 1 (p. 130, not digitized) | 7, 13, 201, 188 | 193 × 174 |
| Buridan 2 | 7, 12, 201, 189 | 193 × 176 |
| Buridan 3 | 6, 16, 201, 193 | 194 × 176 |
| Buridan 4 | 11, 13, 204, 190 | 192 × 176 |
| Buridan 5 | 7, 16, 200, 191 | 192 × 174 |
| Buridan 6 | 5, 12, 200, 188 | 194 × 175 |
| Buridan 7 | 8, 12, 203, 189 | 194 × 176 |
| Buridan 8 | 9, 12, 203, 188 | 193 × 175 |
| Buridan 9 | 7, 14, 201, 191 | 193 × 176 |
| Buridan 10 | 6, 14, 200, 191 | 193 × 176 |
| Buridan 11 | 11, 16, 205, 191 | 193 × 174 |
| Buridan 12 | 11, 14, 205, 189 (by hand) | 193 × 174 |
| Buridan 13 | 8, 13, 202, 190 | 193 × 176 |
| Figure/Ground 1 | 10, 13, 203, 188 | 192 × 174 |

All are 1.10 times wider than tall; the screen's frame in the chapter 4 dumps is 306 × 268
pixels at 150 dpi, 1.14 [F95 p. 98 (img)]. The table is therefore taken as 110 × 100 units,
one pixel of these figures being 0.567 units on both axes, with the origin at the upper left
(Henry's left corner), x to the right and y down towards Eliza. A position of (26.1, 75.9) is
26.1 units from the left edge and 75.9 of the way from Henry's edge to Eliza's.

## 4. The footprints (D-002)

The icons' bounding boxes, pointing up, in pixels of the figures and in units:

| Type | Pixels (w × h) | Units (w × h) | Measured on |
|---|---|---|---|
| fork | 6 × 17 | 3.4 × 9.6 | the figures (and the palette: 6 × 17) |
| knife | 3 × 17 | 1.7 × 9.6 | the figures (palette 4 × 17) |
| spoon | 6 × 17 | 3.4 × 9.6 | the figures (palette 8 × 17 with its bowl); the Buridan figures draw the bowl as a circle 6-7 px wide and the box is centred on the handle |
| cup | 14 × 7 | 7.9 × 4.0 | the figures (palette 15 × 8) |
| saucer | 14 × 13 | 7.9 × 7.4 | Surround 7 (palette 14 × 14) |
| plate | 19 × 19 | 10.8 × 10.8 | Surround 8 (the palette's 21 × 17 is clipped by its cell); Buridan 13's two are 19 × 20 |
| big glass | 11 × 15 | 6.2 × 8.5 | the palette only |
| small glass | 8 × 10 | 4.5 × 5.7 | the figures (palette 8 × 11) |
| pepper shaker | 12 × 15 | 6.8 × 8.5 | Surround 10 (palette 14 × 17) |
| salt shaker | 12 × 15 | 6.8 × 8.5 | Surround 9 and 10 (palette 14 × 17); Buridan 8 and 9 draw it 12 × 15, Buridan 6 and 13 13 × 16 and 12 × 16 |
| soup bowl | 21 × 12 | 11.9 × 6.8 | the palette only |

Silverware pointing left or right swaps width and height (`tabletop.objects.footprint`).

## 5. The uncertainty and the review (D-003)

**Uncertainty.** One pixel is 0.57 units. An icon's box is good to about one pixel on each
side, the objects an arrow overlaps (the touched cup, the E1 object) to about two. The one
configuration drawn twice, Surround 5 and the chapter 4 screen dumps [F95 p. 98 (img)], was
measured on both images (the dump's fractions are in `tests/data/ch4.json`): over the thirteen
objects the two readings differ by at most 1.6 units in x and 2.2 in y (the isolated cup's y),
with a median of 0.5. The files state an uncertainty of 2 units. No Surround problem turns on
a small distance; Blockage 8-10 do, and item 08 states theirs separately.

**Review by eye.** Each file was rendered beside its figure (step 8) and the two panels
compared on the sheet, then against French's description of the configuration:

| Problem | Objects | What the figure shows, and what French says |
|---|---|---|
| Surround 1 | 3 | Henry's cup upper left (H), Eliza's glass lower left (E1), her cup lower right (E2) [F95 p. 115]. Boxes on their icons. |
| Surround 2 | 7 | A spoon right of Henry's cup; spoon, spoon, glass, fork on Eliza's side: "two groups ... The only thing they share is the presence of a spoon" [F95 p. 116]. |
| Surround 3 | 8 | Knife, cup, spoon; spoon, spoon, glass, fork: "silverware on either side of both the cup and the small glass" [F95 p. 117]; the knife's top lies under the H arrow (its x is firm, its height is the neighbours'). |
| Surround 4 | 7 | Cup with a pair of spoons; a pair of spoons with the glass: the identical subgroups of p. 117. The cup stands further left (x 13) than in the other figures. |
| Surround 5 | 13 | Fork, fork, knife, cup, spoon, spoon; spoon, spoon, glass, fork, fork, knife; the loner cup [F95 p. 118]; identical to the chapter 4 configuration. |
| Surround 6 | 13 | Surround 5 with "the isolated cup and the small glass ... switched" [F95 p. 119]; the row's silverware stands 3 units further right (the cup is wider than the glass). |
| Surround 7 | 13 | The glass "replaced by a saucer" [F95 p. 120], a circle with a dot in the glass's place. |
| Surround 8 | 13 | The saucer "replaced in Surround 8 by a plate" [F95 p. 120]; the plate's left edge nearly touches the second spoon's handle. |
| Surround 9 | 13 | The plate "replaced by a saltshaker" [F95 p. 121]; the S box in the glass's place. |
| Surround 10 | 18 | Surround 5 plus "junk" [F95 p. 121]: a pepper shaker (P) and a salt shaker (S) at mid-table, a fork, a knife and a small glass at mid-right; five objects. |

Every box sat on its icon and every rendered layout matched its figure; nothing was moved
after the review. The doubtful readings are the three recorded in the files' `notes`: the knife
under the H arrow of Surround 3, the faint handle of the first spoon of Surround 2, and the
first spoon of Surround 5 measuring 14 pixels tall. Each is a 1-2 pixel matter inside the
stated uncertainty.

The other three families were reviewed the same way, on five sheets of four or five (every
Blockage, Buridan and Figure/Ground file beside its figure, at twice the figure's size):

| Problem | Objects | What the figure shows, and what French says |
|---|---|---|
| Blockage 1 | 2 | Henry's cup upper left (H, and E2: the touched cup itself is an answer), Eliza's glass lower left (E1) [F95 p. 122]. |
| Blockage 2 | 3 | A glass "in the upper right-hand corner" [F95 p. 123]; E2 is still the touched cup. |
| Blockage 3 | 4 | "one more small glass ... in the lower left-hand corner" [F95 p. 123]: a glass pair lower left (E1 the left one, E2 the right one, by their labels), the touched cup E3. |
| Blockage 4 | 5 | "A spoon has been placed next to the small glass in the upper right-hand corner" [F95 p. 125]: to its right, bowl up. |
| Blockage 5 | 5 | "The spoon in the upper right-hand group has been replaced by a cup" [F95 p. 126]: a glass and a cup touching. |
| Blockage 6 | 5 | "two glasses in the upper right-hand corner" [F95 p. 126], 5 px apart; the turning point of the series. |
| Blockage 7 | 9 | "the addition of the silverware" [F95 p. 127]: a fork and a knife left of the upper-right glasses, a fork and a knife right of the lower-left glasses (the text does not name the pieces; the icons do). |
| Blockage 8 | 5 | Blockage 6 with the upper-right glasses "one glass-width" apart [F95 p. 128]; section 8. |
| Blockage 9 | 5 | "two glasses wide" [F95 p. 128]; the frame given by hand. |
| Blockage 10 | 5 | "three glasses would fit in the gap" [F95 p. 128]; the right glass almost at the corner. |
| Buridan 1 | 9 | Cup, cup, spoon (H) at the top centre; spoon (E1), glass, glass lower left; cup, cup, fork (E2) lower right [F95 p. 129]; the "sidekick" pairs of p. 130. Section 7. |
| Buridan 2 | 6 | The sidekicks "not doubled" [F95 p. 131]: one cup, one glass, one cup. |
| Buridan 3 | 8 | "two paired-cup groups" [F95 p. 132] again, one glass beside Eliza's spoon. |
| Buridan 4 | 9 | "one of the glasses in the lower left-hand corner has been replaced by a cup" [F95 p. 132]: spoon, glass, cup. |
| Buridan 5 | 9 | "spoons as the sole type of silverware" [F95 p. 134]: spoon, glass, cup lower left (E1); glass, glass, spoon lower right (E2). |
| Buridan 6 | 9 | A glass "replaced by a saltshaker" [F95 p. 134]: spoon, S, glass. |
| Buridan 7 | 7 | "no objects at all next to the rival spoon" [F95 p. 135]. |
| Buridan 8 | 9 | Two saltshakers beside Eliza's spoon, where Buridan 6 had "a saltshaker and a small glass, instead of two saltshakers" [F95 p. 136]: spoon, S, S. |
| Buridan 9 | 9 | Eliza's fork a saltshaker: "there is little pressure to touch the saltshaker" [F95 p. 136]; glass pair and cup pair as in Buridan 1. |
| Buridan 10 | 3 | "The complete lack of any positional or grouping factors" [F95 p. 137]: three pieces of silverware, Henry's spoon at the centre line. |
| Buridan 11 | 5 | "cups next to the touched spoon and the fork" [F95 p. 138]: one each. |
| Buridan 12 | 10 | "pairs of cups are added next to each piece of silverware" [F95 p. 138] and the lower-left group keeps a glass, at its far end: spoon, cup, cup, glass. The frame given by hand. |
| Buridan 13 | 9 | Plates beside Eliza's spoon, "the cup-cup and plate-plate groups" [F95 p. 139], and a saltshaker (E2) beside the cup pair. |
| Figure/Ground 1 | 6 | "Henry reaches across the table and touches" Eliza's fork [F95 p. 139]: fork (E1), fork (E2), glass (E3) at the top centre; glass, glass, fork (H) at the bottom centre. |

Doubtful readings, each inside the stated uncertainty and recorded in the file's `notes`: the
touched spoon's bowl lies under the H arrow's head in every Buridan figure and is placed from
the handle (6 px wide, centred on it), as is Figure/Ground's touched fork (its tines' top row
under the arrow); the glasses of Blockage 3 (upper right) and Buridan 9 and 12 measure 6-7 px
wide, their far side faint; Blockage 5's upper-right glass and cup touch and were separated on
the dump; the first lower-left cup of Buridan 12 has a faint rim against the second cup;
Buridan 11's lower-right cup measures 13 × 8 px; the saltshakers of Buridan 6 and 13 measure
a pixel more than the footprint. One reading of the text is recorded too: French's "imagine
this picture with no cups or glasses (Buridan 9)" [F95 p. 130] does not match the Buridan 9
figure, which has both; the configuration with none is Buridan 10, and the figure is digitized
as printed.

## 6. The cross-check against FCCA's figures

FCCA redraws eighteen layouts: six Surround (IX-1a-f: Surround 1, 2, 4, 5, 8, 10), six
Blockage (IX-2a-f: Blockage 1, 2, 4, 5, 6, 9) and six Buridan (IX-3a-f: Buridan 10, 11, 1, 4,
5, 6) [FCCA pp. 394-399 (img)]. Each was read as a 300 dpi crop of its page image
(`pdftoppm -r 300 -x -y -W -H`, under `/tmp`) and compared object by object with the file
rendered by `tools/render_problem.py`; the reading is recorded, object by object, in
`tests/data/fcca_crosscheck.json`, which `tests/test_ch2.py` checks against the files (every
object FCCA draws is the file's object of that id, of the same type, in the same ninth of the
table, with the same H or E label). Three things differ in kind between the two books'
drawings: FCCA draws the table **square** (the eighteen frames measure 359-372 × 365-374 px
inside their lines at 300 dpi, a ratio of 0.98-1.01, where the book's are 1.10 wide; D-001),
it prints the H, E1 and E2 labels and their arrows **inside** the frame (the book prints them
outside), and its icons are larger relative to the table (a small glass is 25 px of a 362 px
frame, 7.6 units, against the book's 8 px of 194, 4.5 units). The figures are halftone scans:
the icons are light grey and the labels and arrows black, so no ink threshold separates an
icon from the arrow that touches it (at grey level 200 the touched object, its neighbours and
the arrow are one cluster), which is why the comparison is by eye and positions are compared
as fractions of the frame. Two gaps were measured on the scans, by the column profile of the
darkest pixel over the glasses' rows (IX-2e and IX-2f, below).

| FCCA | Page | Book's figure | Objects | Verdict |
|---|---|---|---|---|
| IX-1a | 394 | Surround 1 | cup (H) upper left; small glass (E1) lower left; cup (E2) lower right | agree |
| IX-1b | 394 | Surround 2 | cup (H), spoon; spoon, spoon, small glass (E1), fork; cup (E2) | agree |
| IX-1c | 394 | Surround 4 | cup (H), spoon, spoon; spoon, spoon, small glass (E1); cup (E2) | agree |
| IX-1d | 395 | Surround 5 | fork, fork, knife, cup (H), spoon, spoon; spoon, spoon, small glass (E1), fork, fork, knife; cup (E2) | agree |
| IX-1e | 395 | Surround 8 | as Surround 5 with a plate (E1) for the glass | agree |
| IX-1f | 395 | Surround 10 | as Surround 5 plus, mid-table, a pepper and a salt shaker and, mid-right, a fork, a knife, a small glass **and a cup** | **differs**: a sixth junk object, the cup at the right end of the mid-right row, as FCCA's text says ("Six objects have been added" [FCCA p. 395]); the book's figure draws five [F95 p. 121 (img)]. A different configuration; the file follows the book (SPEC.md 7.6) |
| IX-2a | 396 | Blockage 1 | cup (H, E2) upper left; small glass (E1) lower left | agree |
| IX-2b | 396 | Blockage 2 | cup (H, E2); small glass upper right; small glass (E1) | agree |
| IX-2c | 396 | Blockage 4 | cup (H, E3); small glass and spoon upper right; small glasses (E1, E2) lower left | agree |
| IX-2d | 397 | Blockage 5 | cup (H, E3); small glass and cup upper right; small glasses (E1, E2) | agree |
| IX-2e | 397 | Blockage 6 | cup (H, E3); two small glasses upper right; small glasses (E1, E2) | agree; measured: the upper-right glasses at 0.755 and 0.833 of the width (the book's 0.765 and 0.826), 6 px of 362 apart, 1.8 units (the book's 2.8) |
| IX-2f | 397 | Blockage 9 | cup (H, E3); two small glasses upper right, far apart; small glasses (E1, E2) | **differs in one position**: the left upper-right glass stands at 0.649 of the width against the book's 0.721, that is 8 units further left (the right glass agrees, 0.833 against 0.834); FCCA's gap is 42 px of 362, 12.8 units, 1.7 of its own glass widths, the book's 7.9 units, 1.8 of its glass widths (section 8): the same in glass widths, not in table units. The file follows the book |
| IX-3a | 398 | Buridan 10 | spoon (H) top centre, at the centre line; spoon (E1) lower left; fork (E2) lower right | agree |
| IX-3b | 398 | Buridan 11 | cup, spoon (H) top centre; spoon (E1); cup, fork (E2) | agree |
| IX-3c | 398 | Buridan 1 | cup, cup, spoon (H); spoon (E1), small glass, small glass; cup, cup, fork (E2) | agree |
| IX-3d | 399 | Buridan 4 | cup, cup, spoon (H); spoon (E1), small glass, cup; cup, cup, fork (E2) | agree |
| IX-3e | 399 | Buridan 5 | cup, cup, spoon (H); spoon (E1), small glass, cup; small glass, small glass, spoon (E2) | agree |
| IX-3f | 399 | Buridan 6 | cup, cup, spoon (H); spoon (E1), salt shaker, small glass; cup, cup, fork (E2) | agree |

Sixteen of the eighteen agree object for object. The two that do not are Surround 10, where
FCCA's drawing is of another configuration (the extra cup also explains why FCCA's numbers
for this set, 82 and 18 percent, differ from the book's 73 and 27; SPEC.md 7.6), and
Blockage 9, where one glass stands 8 units from where the book draws it. In both the file
follows the book's figure (ANSWERS.md 4); FCCA's variant of Surround 10 is not digitized, and
`RESULTS.md` will show FCCA's numbers beside the book's as a second target for the book's
configuration, with this note. An earlier reading of IX-2f by eye (item 08) had put its
glasses "about two glass widths apart", which the measurement bears out in FCCA's own glass
widths; the disagreement is in units, and matters because the engine sees units (D-023).

## 7. Buridan 1, printed twice

Buridan 1 is the one configuration French prints twice, as "Buridan 1 (original program)"
[F95 p. 129 (img)] and "Buridan I (modified program)" [F95 p. 130 (img)], the second being
"This configuration, identical to the previous one, is the critical configuration of this
series, but the statistics given here are for the modified program." [F95 p. 131] One file,
`buridan-01.json`, digitized from the first figure (PDF page 139, image 22), serves both
result sets of `reported.BURIDAN`. The second figure (PDF page 140, image 24; frame 7, 13,
201, 188, inner 193 × 174 px) was measured the same way as a check of the method on two
independent drawings of one configuration, as the chapter 4 dump was for Surround 5:

| Object | p. 129 (the file) | p. 130 | Difference |
|---|---|---|---|
| cup1 | 47.3, 14.3 | 47.3, 14.3 | 0.0, 0.0 |
| cup2 | 56.4, 14.3 | 55.9, 14.3 | 0.5, 0.0 |
| spoon1 (touched) | 62.2, 13.7 | 61.5, 13.1 | 0.7, 0.6 |
| spoon2 (E1) | 8.9, 86.9 | 9.4, 86.9 | 0.5, 0.0 |
| small-glass1 | 13.5, 87.7 | 13.9, 87.7 | 0.4, 0.0 |
| small-glass2 | 18.6, 88.3 | 19.3, 88.3 | 0.7, 0.0 |
| cup3 | 78.2, 85.7 | 78.0, 85.7 | 0.2, 0.0 |
| cup4 | 86.8, 85.7 | 86.5, 85.7 | 0.3, 0.0 |
| fork1 (E2) | 94.2, 84.6 | 92.7, 84.0 | 1.5, 0.6 |

The two readings differ by at most 1.5 units (the fork, whose handle is under the E2 arrow's
head in both figures), and by 0.7 or less elsewhere: inside the 2 units the files state, and
of the same size as the Surround 5 comparison (1.6 and 2.2).

## 8. The gaps of Blockage 8-10

Blockage 8-10 are the distance-sensitive problems: "I wish to examine what happens when the
two objects in the upper right-hand corner of Blockage 6 are gradually pulled further apart"
[F95 p. 128], so that the glasses stop being seen as a group and the diagonal group-to-group
correspondence that blocks the cup-to-glass answer can no longer be built. French gives the
gaps in glass widths: "In other words, in Blockage 8, a third glass could be placed between
the two glasses; in Blockage 9, two glasses could be placed between the original two glasses;
and in Blockage 10, three glasses would fit in the gap." [F95 p. 128] The measured gap is the
white space between the two glasses' boxes along x (`gap` in each file; the test
`test_blockage_gaps_in_order_and_near_french` recomputes it from the boxes), against a glass
width of 8 px, 4.5 units (D-002; the figures draw these glasses 7-8 px wide):

| Problem | Boxes (left glass, right glass) | Gap (px) | Gap (units) | Glass widths | French |
|---|---|---|---|---|---|
| Blockage 6 | 152-158, 164-170 | 5 | 2.8 | 0.6 | "two glasses in the upper right-hand corner" [F95 p. 126] |
| Blockage 8 | 151-158, 169-176 | 10 | 5.7 | 1.3 | one |
| Blockage 9 | 142-149, 164-171 | 14 | 7.9 | 1.8 | two |
| Blockage 10 | 140-147, 169-176 | 21 | 11.9 | 2.6 | three |

The gaps are in French's order and each lies within the files' uncertainty of 2 units (0.44
glass widths) of his whole number: 0.27, 0.24 and 0.36 widths off. But the drawing's steps
are 4 and 7 px where "each time by the width of one glass more" [F95 p. 128] would make them 8
and 8, so Blockage 8's gap is nearer one and a quarter widths than one, and Blockage 10's
nearer two and a half than three (measured from the ink rather than the boxes, 11, 15 and 22
px: 1.4, 1.9 and 2.75 widths). The uncertainty's effect: each glass edge is good to about a
pixel, so a gap is good to about 2 px, 1.1 units, a quarter of a width; Blockage 6's glasses
could be 3-7 px apart, Blockage 8's 8-12, Blockage 9's 12-16, Blockage 10's 19-23. The order
is never in doubt (the ranges do not overlap), but where the proximity threshold of a group
falls between 6 and 12 units (D-023) is a decision the engine must make without these
numbers, and RESULTS.md will record whether Blockage 8-10 sit on its edge. The files keep the
drawn gaps; nothing is rounded to French's whole numbers (ANSWERS.md 1: no special-casing of
a problem).

**What the engine sees.** The engine does not read the boxes: it measures the white space
between the two glasses' footprint boxes (D-002, 8 px, 4.5 units, centred on the recorded
positions) with `objects.edge_gap`, which is the gap the proximity criterion uses (D-022).
On Blockage 8-10 that gap is 5.7, 7.9 and 12.0 units (1.27, 1.76 and 2.67 glass widths), the
files' numbers to a tenth. On Blockage 6 it is 2.2 units (0.49 widths) against the drawn 2.8,
because that figure draws both glasses 7 px wide, one pixel narrower than the footprint; the
engine's gap is therefore one pixel narrower than the ink's there. `tests/test_geometry.py`
checks these values.

## 9. Chapter 2's nineteen configurations

Chapter 2 presents "a first series of thirteen table configurations followed by a second
series of six" [F95 p. 26], Figures 2.2-2.20, each with French's expected answer in words and
no statistics (SPEC.md 1.6; the qualitative evidence M2 of the validation protocol). They are
digitized as `problems/ch2-fig02.json` to `ch2-fig20.json` (family `ch2`) by the method of
section 2, with these differences.

**The figures.** Chapter 2's figures draw the table alone, without a bar graph: embedded
JPEGs of 227-246 × 221-229 px at 135 ppi, two to a page (three on p. 29), the frame nearly
filling the image with "Henry" above it and "Eliza" below it [F95 p. 27 (img)]. `pdfimages`
numbers them 1, 3, 5, ... 37 for Figures 2.2 to 2.20 (here the odd images are the figures and
the even ones their masks; the palette of Figure 2.1 on p. 26 is image 0). PDF page =
printed + 12 for pp. 23-45: Figures 2.2-2.3 p. 27, 2.4-2.5 p. 28, 2.6-2.8 p. 29, 2.9-2.10
p. 30, 2.11-2.12 p. 31, 2.13-2.14 p. 32, 2.15-2.16 p. 33, 2.17-2.18 p. 34, 2.19-2.20 p. 35.
`find_frame` searches the right line in the right eighth of such a figure (one less than one
and a half times wider than tall) instead of the band left of a bar graph. **No E arrows**:
only the H arrow is drawn, entering the touched cup from the upper left, so every file's
`answers` is empty and the expectation is the `expected` block below.

**The scale.** The frames measure 212-215 × 193-196 px inside their lines, a tenth larger
than chapter 5's and of the same shape (1.10 wide; D-001), so one pixel is 0.51 units:

| Figure | Frame | Inner size | Figure | Frame | Inner size |
|---|---|---|---|---|---|
| 2.2 | 10, 13, 224, 210 | 213 × 196 | 2.12 | 6, 13, 221, 208 | 214 × 194 |
| 2.3 | 10, 16, 226, 212 | 215 × 195 | 2.13 | 8, 15, 222, 210 | 213 × 194 |
| 2.4 | 10, 16, 225, 212 | 214 × 195 | 2.14 | 6, 14, 222, 208 | 215 × 193 |
| 2.5 | 7, 12, 222, 207 | 214 × 194 | 2.15 | 12, 12, 225, 208 | 212 × 195 |
| 2.6 | 10, 15, 225, 210 | 214 × 194 | 2.16 | 8, 13, 221, 209 | 212 × 195 |
| 2.7 | 10, 15, 224, 209 | 213 × 193 | 2.17 | 12, 15, 227, 210 | 214 × 194 |
| 2.8 | 6, 15, 220, 210 | 213 × 194 | 2.18 | 8, 12, 224, 208 | 215 × 195 |
| 2.9 | 9, 14, 222, 209 | 212 × 194 | 2.19 | 7, 13, 221, 210 | 213 × 196 |
| 2.10 | 9, 13, 224, 207 | 214 × 193 | 2.20 | 12, 15, 225, 210 | 212 × 194 |
| 2.11 | 10, 14, 225, 210 | 214 × 195 | | | |

The icons are drawn at the palette's scale (D-002), a tenth larger than on the chapter 5
figures: a fork 6-7 × 18-19 px, a knife 3-4 × 17-18, a spoon 6-8 × 18-19, a cup 15-16 × 7-9, a
small glass 7-9 × 10-12, a saucer 14-15 × 14-15, a plate 21-22 × 22, a salt shaker 13-15 ×
17-18; every box is within 2 px in width and 3 in height of its type's footprint scaled by
the frame (`tests/test_problems.py`, `test_the_footprints`).

**The arrows and touching icons.** The H arrow's head overlaps the touched cup's left side in
every figure; the cup's box was read on an ASCII dump as its rim row, the right edge of its
handle and its bottom row (15 × 7 or 8 px). Neighbouring icons touch in seven figures and
were separated on the dump: the fork left of the cup (2.8, 2.12-2.14), the first spoon right
of the cup (2.10), the two spoons right of the cup (2.12-2.14), the knife under the H label
(2.11), the fork, knife and saucer of Henry's row (2.9), the glass and knife (2.10) and the
spoon beside the saucer or the saltshaker (2.13, 2.14) on Eliza's side. Each file's `notes`
say where.

**Ids** follow section 2's convention, with the chain of variants French describes: Figure
2.3 on 2.2 (`based_on`), 2.4-2.6 each on the one before (Eliza's object replaced); 2.8-2.11
on 2.7, whose `cup1`, `small-glass1` and `cup2` they keep, numbering their additions in reading
order (so Figure 2.11 has exactly Surround 5's ids); 2.12 on 2.11 (the knives removed), 2.13
and 2.14 on the one before (the glass replaced); 2.16 on 2.15 (`spoon1` replaced by
`small-glass2`), 2.17 on 2.16 (`small-glass3` upper right, `small-glass4` lower left, in
reading order), 2.18 on 2.17 (`small-glass4` replaced by `spoon2`, the chain having used
`spoon1`), 2.19 on 2.18 (the upper-right glasses replaced by `salt-shaker1` and `plate1`),
2.20 on 2.19 (`spoon2` removed). The chapter 5 Blockage family numbers its glasses in its own
order, so `small-glass3` is not the same glass in `ch2-fig17` and `blockage-03`.

**The expectations.** Each file's `expected` block records French's words: `answer`, the id
he names as the answer Eliza gives most often ("the only possible response", "by far the
most likely", "generally", "most often", "most probably", "favoring", "strongly inclined",
"extremely strong"), or null where he states only a direction of change; `plausible`, the
objects he names as candidates, the first being the one his words favour (a reading);
`unlikely`, the ones he rules out ("a very unlikely answer", "certainly not"); `quotes`, his
sentences with their printed pages, checked word for word against the text layer by
`tests/test_ch2.py`; and `comparisons`, each comparative he makes as `toward` an object
`more_than` or `less_than` an earlier figure (Figure 2.6 sits between 2.5 and 2.4). The
benches of a later item turn the comparisons into the qualitative measure M2: the frequency
of `toward` in the later problem beside the earlier one.

| Figure | Objects | What the figure shows, and what French says |
|---|---|---|
| 2.2 | 1 | A single cup at the top centre, touched. "the only possible response" [F95 p. 26]. |
| 2.3 | 2 | Henry's cup (H) at the top centre, Eliza's at the bottom centre. Her own cup "by far the most likely response" [F95 p. 26]. |
| 2.4 | 2 | Eliza's cup replaced by a small glass. A little more pressure to reach across, "generally not enough" [F95 p. 27]. |
| 2.5 | 2 | The glass replaced by a fork, tines up (the text says "the object"; the icon is a fork). Henry's cup "most often" [F95 p. 27]. |
| 2.6 | 2 | The fork replaced by a saucer. Between 2.4 and 2.5 [F95 p. 28]. |
| 2.7 | 3 | Henry's cup upper left (H), a small glass lower left, a cup lower right: Surround 1's objects. "the chances are good" for the diagonal cup [F95 p. 28]. |
| 2.8 | 10 | A saucer, a knife, a fork, the cup (H) and a saltshaker in Henry's row; a spoon, the glass, a spoon and a knife lower left; the lone cup. "belonging to a group" [F95 p. 28]. |
| 2.9 | 10 | Right of the cup: a saltshaker, a fork, a knife, a saucer; right of the glass: a spoon, a knife, a spoon. Groups "not the same either in number or in makeup" [F95 p. 28]. |
| 2.10 | 10 | Right of the cup: spoon, fork, spoon; right of the glass: knife, knife, fork, fork. "a fork between the two spoons" and "a fork inside the group" [F95 p. 36]. |
| 2.11 | 13 | Surround 5 and the chapter 4 configuration. "Take close note of this problem" [F95 p. 31]. |
| 2.12 | 11 | Figure 2.11 without the knives: fork, fork, cup, spoon, spoon; spoon, spoon, glass, fork, fork. Subgroups "in corresponding positions" [F95 p. 31]. |
| 2.13 | 11 | The glass replaced by a saucer. "Possibly, but not necessarily" toward the lone cup [F95 p. 31]. |
| 2.14 | 11 | The saucer replaced by a saltshaker. "even more significantly to the isolated cup" [F95 p. 31]. |
| 2.15 | 3 | Henry's cup upper left (H), a spoon upper right, a glass lower left. The glass "most probably" [F95 p. 32]. |
| 2.16 | 3 | The spoon replaced by a glass. Henry's cup "considerably more likely" than in 2.15 [F95 p. 34]. |
| 2.17 | 5 | Two glasses upper right, two lower left. Henry's cup "much more likely than in figure 2.16" [F95 p. 34]. |
| 2.18 | 5 | The second lower-left glass replaced by a spoon (the text says the groups "are not identical any longer"; the icon is a spoon). "back to favoring the glass" [F95 p. 35]. |
| 2.19 | 5 | A saltshaker and a plate upper right; the glass and the spoon lower left. The glass, "strongly inclined" [F95 p. 35]. |
| 2.20 | 4 | The spoon removed: the glass alone lower left. "extremely strong" for the glass [F95 p. 35]. |

**Review.** The nineteen files were rendered beside their figures (step 8) on four sheets of
four or five and compared by eye: every box sat on its icon and every layout matched its
figure and French's description; nothing was moved after the review. Two readings from the
image where the text is silent: the object in front of Eliza in Figure 2.5 is a fork, and the
piece of silverware in the lower-left pair of Figure 2.18 is a spoon.

**Uncertainty.** Figure 2.11 is the third independent drawing of the central configuration
(with Surround 5 and the chapter 4 screen dumps). Measured here, its thirteen objects agree
with `surround-05.json` to within 1.9 units in x and 2.3 in y (the lone cup; median 0.7),
the same size as the Surround 5 against chapter 4 comparison (1.6 and 2.2) and inside the 2
units the files state (D-003).

## 10. The file format, and what the engine reads

**Status:** final (item 29). The 53 files are the whole problem library; the random tables of
D-068 (`tabletop/tables.py`) are built in memory with synthetic provenance and never written
here, and the `focus` bench's junk variants likewise. A new configuration is a new file in
`problems/`, validated by `tabletop.problems.validate` on loading (`load_problem` refuses a
faulty or misnamed file). The keys of a file, in the order written:

| Key | Meaning |
|---|---|
| `name`, `title`, `family` | the file's stem (`surround-05`), French's printed title ("Surround 5"), one of `surround`, `blockage`, `buridan`, `figure-ground`, `ch2` |
| `source` | `tag` (`F95`), `page` (the printed page of the figure), `figure`, `pdf_page`, `image_index` (as `pdfimages` numbers it), `image_size_px` |
| `table` | `width` 110, `height` 100 (D-001) and `frame_px`, the frame's inner box on the figure (left, top, right, bottom) |
| `touched` | the id of the object Henry touches |
| `answers` | French's labels to ids (`E1`, `E2`, ...; `E1+E2` is written as two labels, D-059); empty for chapter 2 |
| `expected` | chapter 2 only (section 9): `answer`, `plausible`, `unlikely`, `quotes`, `comparisons` |
| `objects` | one entry per object: `id`, `type` (the palette's eleven, `objects.TYPES`), `x`, `y` (units, the centre of the icon's box), `orientation` (`up`, `down`, `left`, `right` for silverware, else null), `box_px` (the icon's box on the figure, from which the position is recomputed), `figure`, `page`, `image` |
| `based_on` | the problem this one is a variant of, whose ids it keeps (section 2, step 7) |
| `gap` | Blockage 6, 8, 9 and 10 only (section 8): the two glasses, the gap in pixels, units and glass widths, French's number and the uncertainty |
| `method`, `uncertainty`, `notes`, `review` | the method in a sentence, the stated uncertainty (2 units) with its note, the doubtful readings, and the review by eye |

**What the engine reads.** A `Problem` (`tabletop.problems`) gives the engine the objects
with their types, positions and orientations, the touched object and the answer labels;
nothing else in the file reaches a run. From the positions and the footprints of D-002
(`objects.box`, `objects.edge_gap`) come the proximity of two objects (D-022; the Blockage
gaps, section 8), the region each object is seen in (D-004, through the blur), the places
diagonally and directly opposite the touched object (D-032) and the spans and gaps that
enter a group's strength (D-025). Orientation is input, never perceived (D-006). The
uncertainty of 2 units matters only where a mechanism turns on a small distance: the
proximity criterion's end points are 0.5 and 2.5 glass widths (2.25 and 11.25 units), so a
2-unit error moves a link's probability by at most a fifth, and the four Blockage gaps keep
their order under it (section 8; `tests/test_geometry.py`).
