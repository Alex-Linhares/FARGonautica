# The citation audit

Date: 2026-10-13. Every number and claim in this folder cites a page of French 1995 (F95),
of *Fluid Concepts and Creative Analogies* (FCCA) or of French & Hofstadter 1991 (FH91).
Quotations of six words or more are checked by machine against the text layer of the cited
page (`tools/check_quotes.py`, run by `tests/test_quotes.py` and the loop's gate), but a
citation marked `(img)` was read from a page image (a figure, an equation, a bar graph, a
screen dump) and a paraphrase without quotation marks is never checked at all. This audit
re-verified a random sample of both kinds on the page images, every bar-graph value of
`tabletop/reported.py`, and ten digitized layouts against their figures, and fixed what was
wrong. Nothing here changed a default of any profile: a wrong citation changes a claim's
page or wording, a wrong value in `reported.py` would change only RESULTS.md's comparison
(none was wrong).

## 1. The sample and how it was drawn

`python3 tools/audit_sample.py` (seed 31) inventories every page citation in `SPEC.md`,
`DECISIONS.md` and `RESULTS.md`, line by line in that order, then in every docstring of the
package (`tabletop/**/*.py`, sorted by path, read with `ast`), and splits them into two
strata: the `(img)` citations and the plain ones. One splitmix64 generator (`tabletop/rng.py`,
loaded by file) seeded with 31 draws 24 of the `(img)` stratum, then 24 of the plain one,
then 10 problem stems from `problems/` (sorted), each by `rng.below(len(pool))` on the
shrinking pool. The inventory at the time of the audit held 2,293 citations: 212 `(img)` and
2,081 plain (the same counts after the fixes, which changed wording and pages but added and
removed no citation; the SPEC.md line numbers below are the ones after the fixes, as the tool
prints them now). The sample is 48 citations, above the 40 the item asked for, and weighted
towards the `(img)` stratum because the checker never reads it.

How each item was verified: the cited page rendered with `pdftoppm` at 110 dpi (text), at
200-400 dpi for equations and disputed lines, at 300 dpi for the Slipnet grids and the table
panels of the chapter 4 screen dumps and at 600 dpi for their Coderack strips; the chapter 5
bar graphs and the chapter 2 and 5 layouts as the embedded JPEG images (`pdfimages`,
135 ppi), the bar graphs enlarged twice on review sheets; FCCA's pp. 394-399 at 150 dpi.
Every image was read by eye; the bars were also measured by a pixel script (the two bars of a
graph sum to 100, so each bar's share of the two heights is its percentage, good to one
point). All scratch images were written under `/tmp` and none is committed.

Verdicts: **yes** (the page says what the citation claims), **yes, note** (right, with a
remark), **fixed** (wrong, corrected in this item).

## 2. The 24 `(img)` citations

| # | Where | Citation | What was checked | Verdict |
|---|---|---|---|---|
| 1 | `tabletop/bench/calibrate.py:154` | `[F95 p. 144 (img)]` | the Figure 5.4 gap | **fixed**: cited p. 143; Figure 5.4 is printed on p. 144 (Tables 5.1-5.2 are on p. 143) |
| 2 | `SPEC.md:1388` | `[F95 p. 98 (img)]` | the 23 labels of Fig. 4.2's Coderack strip, FND-GP to FND-ANS | yes (600 dpi) |
| 3 | `DECISIONS.md:1921` | `[F95 pp. 100-107 (img)]` | FND-ANS 24 percent at 22 codelets (T 59) and 30 at the end (T 2) | yes: the strips of Figs. 4.4 and 4.9 and their thermometers |
| 4 | `tabletop/slipnet_def.py:28` | `[F95 pp. 98-107 (img)]` | "food holder" and "vert" never leave the floor while fork, spoon, horiz are active | **fixed**: "food holder" stays at 1 in all nine dumps, but "vert" climbs 1, 2, 3, 4, 5, 7, 7, 7 (Figs. 4.2-4.9); now "never leaves the floor" for food holder and "stays under 8" for vert |
| 5 | `SPEC.md:3238` | `[F95 pp. 143-144 (img)]` | the footnotes and asterisks of Tables 5.1-5.3 | yes: asterisks on Buridan 1's and 4's TT E2 cells only; the three footnotes as transcribed |
| 6 | `RESULTS.md:907` | `[F95 pp. 96-107 (img)]` | the milestones' order in the dumps (gestalt 2, correspondence 12, Worldview entry 22, bottom-up group 33, group correspondence and fight 47, answer 55) | yes: the table panels of Figs. 4.2, 4.3, 4.4, 4.5, 4.7, 4.9 |
| 7 | `SPEC.md:2759` | `[F95 p. 90; F95 p. 91 (img); F95 p. 98 (img)]` | Q as a fraction (p. 90), 0 < Q ≤ 1 (p. 91), the thermometer at 80.0 at two codelets (p. 98) | yes |
| 8 | `SPEC.md:995` | `[F95 p. 61 (img)]` | the subscripts of the sentence under the distance formula | **fixed**: SPEC said the image reads "d_i ... n_i, d_i' ... n_i and B" and that the text layer's "r_i" and "d_j" were slips of the text layer; at 400 dpi the printed sentence reads "r_i" and "d_j" too. The slips are the book's; Figure 3.6's labels (d_1, d_1', d_2, d_2', d_3) and the formula's denominators settle the reading |
| 9 | `SPEC.md:2463` | `[F95 p. 81 (img)]` | 8 / 2³ = 1 | yes |
| 10 | `SPEC.md:2973` | `[F95 p. 96 (img)]` | Fig. 4.1's grid: opposite 139, cup 51, liq holder 4, small glass, crockery, open obj, eat-from-ob 2, every other box 1 | yes (the rotated figure read at 300 dpi) |
| 11 | `tabletop/coderack.py:175` | `[F95 p. 92 (img)]` | u^a over the total of u^a | yes: the table 6^a, 3^a, 1^a over 6^a + 3^a + 1^a |
| 12 | `SPEC.md:3073` | `[F95 p. 107 (img)]` | Fig. 4.9 shows Fig. 4.7's Slipnet | yes: the two grids are identical box for box |
| 13 | `tabletop/slipnet.py:41` | `[F95 p. 61 (img)]` | Distance = 1 / (1/(d1 + d1') + 1/(d2 + d2') + 1/d3) | yes |
| 14 | `tabletop/reported.py:35` | `[F95 p. 115 (img)]` | frequencies read from the bar graphs | yes (§4) |
| 15 | `tabletop/build_object.py:11` | `[F95 p. 96 (img)]` | the legend's build-obj-0, -1, -2 | yes |
| 16 | `tabletop/events.py:4` | `[F95 pp. 95-107 (img)]` | the screen shows the table, the thermometer, the Slipnet grid and the Coderack bars | yes |
| 17 | `DECISIONS.md:589` | `[F95 p. 98 (img); F95 p. 99 (img)]` | "group" 44 after two codelets and 26 after twelve | yes |
| 18 | `tabletop/correspondences.py:804` | `[F95 p. 96 (img)]` | the legend's "build-cp-descrip." | yes |
| 19 | `tabletop/slipnet.py:13` | `[F95 p. 61 (img)]` | the distance is the parallel combination of the pathways' lengths | yes |
| 20 | `SPEC.md:994` | `[F95 p. 61 (img)]` | Figure 3.7 draws r1, r2, r3 in parallel; "d_i are the lengths of the links" | yes |
| 21 | `SPEC.md:2647` | `[F95 p. 92 (img)]` | the third case of the temperature's justification, with ΔQ | yes, note: the page prints a blank where each Δ should stand, on the rendered page as in the text layer (the PDF's font lacks the glyph); the Δ is restored from the equation image of p. 91 and the sense. SPEC §6.3 said the images had the letters; reworded |
| 22 | `tabletop/codelets/searches.py:66` | `[F95 p. 96 (img)]` | the legend's "search-for-sim.-gp" | yes |
| 23 | `tabletop/temperature.py:193` | `[F95 p. 92 (img)]` | "the value of ΔQ will decrease over time" | yes, note: as 21 |
| 24 | `SPEC.md:2624` | `[F95 p. 91 (img)]` | T = α ΔQ/Q + β Δ_WV − γ T_cp | yes |

## 3. The 24 plain citations

| # | Where | Citation | What was checked | Verdict |
|---|---|---|---|---|
| 1 | `SPEC.md:601` | `[FCCA p. 387]` | "a coherent set of perceptual structures emerges ... this is called the Worldview" | yes |
| 2 | `tabletop/potelbat.py:6` | `[FCCA p. 327]` | the skeptic's weighted sum, "the top-scoring object would be the 'winner'" | yes |
| 3 | `tabletop/bench/stated.py:5` | `[F95 pp. 101-105]` | how the Worldview grows and fights | yes (pp. 101 and 105 read; p. 97 for "what it notices first") |
| 4 | `SPEC.md:2155` | `[F95 p. 111; F95 p. 125]` | the group-to-object penalty, too strong | yes: "strongly resists mapping groups onto individual objects" (p. 111), "should probably be relaxed" (p. 125) |
| 5 | `SPEC.md:614` | `[F95 p. 92]` | lower temperature means codelet choice respects urgencies more | yes |
| 6 | `SPEC.md:1661` | `[F95 p. 67]` | "salience is a dynamic quantity that changes as the program develops its picture of the Tableworld" | yes |
| 7 | `SPEC.md:528` | `[FCCA p. 381]` | the Slipnet's four activities complete the list | yes |
| 8 | `tabletop/correspondences.py:486` | `[F95 p. 69]` | the three group slippages (ISA, same objects, orientation) | yes |
| 9 | `tabletop/worldview.py:550` | `[F95 p. 72]` | the winners improved, the loser lowered | yes |
| 10 | `DECISIONS.md:667` | `[F95 pp. 88-89]` | the fixed amounts into right, left, diagonal symmetry; right twice and mirror symmetry | yes (the sentence crosses the page break) |
| 11 | `tabletop/objects.py:6` | `[F95 p. 55]` | four orientations of the silverware; every object has a size | yes |
| 12 | `SPEC.md:1044` | `[F95 p. 63]` | which node each scout jolts | yes |
| 13 | `SPEC.md:1813` | `[FCCA p. 382]` | built "if their categories are identical, or are conceptually close" | yes |
| 14 | `SPEC.md:978` | `[F95 p. 60]` | all pathways help determine the distance | yes |
| 15 | `SPEC.md:1496` | `[F95 p. 65]` | "it very seldom does so" | yes |
| 16 | `SPEC.md:1408` | `[F95 p. 101]` | left and right (not above and below) active for the ends | yes |
| 17 | `DECISIONS.md:1414` | `[F95 p. 105]` | a weaker correspondence can win on coherence | yes: "even though a cup-cup correspondence is a priori stronger" |
| 18 | `tabletop/describe.py:168` | `[F95 p. 63]` | "the program will choose only one as the upper end object" | yes |
| 19 | `SPEC.md:301` | `[F95 p. 31]` | Figure 2.12's subgroups "in corresponding positions", "even stronger" | yes |
| 20 | `RESULTS.md:1172` | `[F95 p. 123]` | Blockage 2's main answer E1 at 88 percent | yes, note: the bar graph is on p. 122; p. 123 holds the table and the text's "12 percent of the time ... touch the cup", the complement |
| 21 | `RESULTS.md:1761` | `[F95 p. 143]` | the raised object-type weight | yes |
| 22 | `tabletop/events.py:7` | `[F95 p. 145]` | what a run reports | yes |
| 23 | `DECISIONS.md:685` | `[FCCA p. 365]` | criterion 11's lesions | yes |
| 24 | `tabletop/bench/ch4.py:66` | `[F95 pp. 96-107]` | the milestones read from the structures drawn | yes |

Three of 48 fixed (two of them `(img)`), two notes on the PDF's missing Δ glyphs, one note on
a page. No fixed item touches a number that any bench or test compares with French's.

## 4. Every bar-graph value of `reported.py`

**The 35 result sets of chapter 5** [F95 pp. 115-139 (img)]. The 36 embedded images of PDF
pages 125-149 (35 bar graphs and Fig. 5.1) were extracted and each graph's two bars measured
and its printed labels read. Every frequency in `reported.SURROUND`, `BLOCKAGE`, `BURIDAN`
(Buridan 1 twice) and `FIGURE_GROUND` equals its printed label, and every measured bar is
within one point of its label (the 2-percent bars of Surround 9, Buridan 2 and Buridan 13 are
too thin for the script and were read from the labels alone). Every structure value and
codelet count is on its cited page's text layer (a script checked all 60 numbers; the tables
of pp. 123 and 125 were also read on the page).

| Set | Printed | Recorded | Set | Printed | Recorded |
|---|---|---|---|---|---|
| Surround 1 | 5 / 95 | 5 / 95 | Blockage 9 | 80 / 20 | 80 / 20 |
| Surround 2 | 10 / 90 | 10 / 90 | Blockage 10 | 100 / — | 100 / 0 |
| Surround 3 | 15 / 85 | 15 / 85 | Buridan 1 (original) | 72 / 28 | 72 / 28 |
| Surround 4 | 45 / 55 | 45 / 55 | Buridan 1 (modified) | 50 / 50 | 50 / 50 |
| Surround 5 | 86 / 14 | 86 / 14 | Buridan 2 | 98 / 2 | 98 / 2 |
| Surround 6 | 100 / — | 100 / 0 | Buridan 3 | 48 / 52 | 48 / 52 |
| Surround 7 | 80 / 20 | 80 / 20 | Buridan 4 | 52 / 48 | 52 / 48 |
| Surround 8 | 10 / 90 | 10 / 90 | Buridan 5 | 40 / 60 | 40 / 60 |
| Surround 9 | 2 / 98 | 2 / 98 | Buridan 6 | 40 / 60 | 40 / 60 |
| Surround 10 | 73 / 27 | 73 / 27 | Buridan 7 | 8 / 92 | 8 / 92 |
| Blockage 1 | 100 / — | 100 / 0 | Buridan 8 | 44 / 56 | 44 / 56 |
| Blockage 2 | 88 / 12 | 88 / 12 | Buridan 9 | 96 / 4 | 96 / 4 |
| Blockage 3 | 100 / — | 100 / 0 | Buridan 10 | 100 / — | 100 / 0 |
| Blockage 4 | 80 / 20 | 80 / 20 | Buridan 11 | 94 / 6 | 94 / 6 |
| Blockage 5 | 58 / 42 | 58 / 42 | Buridan 12 | 75 / 25 | 75 / 25 |
| Blockage 6 | 22 / 78 | 22 / 78 | Buridan 13 | 98 / 2 | 98 / 2 |
| Blockage 7 | 24 / 76 | 24 / 76 | Figure/ground 1 | 100 / — | 100 / 0 |
| Blockage 8 | 38 / 62 | 38 / 62 | | | |

**FCCA's 18 sets** [FCCA pp. 394-399 (img)]: every frequency, structure value and run length of
`reported.FCCA` was read again on the six pages. All 18 agree with the entries, including the
two sets that differ from the book's: IX-1d (86/14, 253/154, 107/85) and IX-1f (82/18,
242/148, 91/127). The captions were read too: IX-2e says the subjects chose Henry's cup 66
percent of the time, which `HUMAN_DATA` carries.

**Tables 5.1-5.3 and Figure 5.4** [F95 pp. 143-144 (img)]: every cell of `TABLE_5_1`,
`TABLE_5_2` and `TABLE_5_3` equals the page, the asterisks and footnotes as SPEC §7.3 says,
and `FIGURE_5_4`'s 23 labels are in the printed order (Bur18, Bur1, Sur1, Sur3, Bur13, Bur10,
Sur2, Bur4, Bur6, Blk7, Blk6, Blk2, Bur7, Bur8, Bur3, Bur11, Blk5, Bur5, Bur12, Blk1, Bur2,
Sur4, Sur5).

## 5. Ten layouts against their figures

The ten problems drawn: `surround-03`, `surround-10`, `blockage-01`, `surround-01`,
`surround-05`, `buridan-06`, `buridan-07`, `ch2-fig05`, `ch2-fig07`, `ch2-fig15`. Each was
rendered beside its figure with `tools/render_problem.py` (the figure's frame enlarged three
times with every object's recorded pixel box outlined, and the layout drawn from the file
alone), two sheets of five, and read by eye: every box sits on its icon, every type is the
icon's, the touched object and the labelled answers are the ones the arrows and labels mark,
and the layout drawn from the file shows the figure's picture (Surround 10's five junk
objects, Blockage 1's touched cup labelled E2, Buridan 6's shaker and glass beside the
lower-left spoon, Buridan 7's spoon alone, the chapter 2 figures' two or three objects).
Nothing was moved. Ten of ten agree.

## 6. What was fixed, and one tool strengthened

- `tabletop/bench/calibrate.py`: the Figure 5.4 citation now names p. 144.
- `tabletop/slipnet_def.py`: the dumps' evidence on "vert" stated as the grids show it.
- `SPEC.md` §3.5: the p. 61 sentence's subscripts are the book's slips, not the text
  layer's; the figure and the formula are followed. §6.3: the missing Δ, α, β, γ are blanks on
  the page itself, restored from the equation image; the bullets keep their `(img)` tag for
  that reason.
- `tools/check_quotes.py`: the gap iteration 22 noted is closed. An ellipsis-split quotation
  whose parts were all under six words was not checked at all; now a quotation of six words
  or more is checked part by part, every part of three words or more (`PART_MIN_WORDS`).
  Over the folder this checks eight more parts and fails none; `tests/test_quotes.py`
  covers the rule with a true and a made-up split quotation. The limits the gate reads
  (`MIN_WORDS` 6, `MAX_WORDS` 80, `THRESHOLD` 0.90) are unchanged.

## 7. Repeating the audit

```sh
python3 tools/audit_sample.py --seed 31          # the sample above (same inventory, same draws)
python3 tools/audit_sample.py --seed 32 --json   # a new sample
```

`tests/test_audit.py` checks that this document records the seed and the inventory, lists
every citation of the sample with a verdict, names every chapter 5 and FCCA result set and the
ten layouts, and that the tool runs and draws a sample of the recorded size. The page images
are not kept: render them again from the PDFs as the tool's docstring and `SPEC.md`'s source
table describe (F95's printed-to-PDF offsets change by range; FCCA is printed + 10).

**Line numbers after item 32.** The final review inserted "As built (item 32)" notes in
`DECISIONS.md` and a few lines in `tabletop/correspondences.py`, which moved seven of the
locations in sections 2 and 3 down; each was re-pointed to the line that now carries the same
citation (the test `test_every_listed_location_exists_and_still_carries_the_citation` checks
this). The citations themselves were not re-read.

**Line numbers after the roles loop's item 00.** The re-interpretation's frame (D-078, its
Kind in the header and the indexes of `DECISIONS.md`; the new opening, §11 and the
Re-interpretation log of `RESULTS.md`) moved eight of the locations in sections 2 and 3
down (five in `DECISIONS.md`, three in `RESULTS.md`); each was re-pointed to the line that
now carries the same citation, as above. The citations themselves were not re-read.

**Line numbers after the roles loop's item 02.** D-102 (the variants bench), its index rows
and a numbering note in the header of `DECISIONS.md` moved 5 of the locations in
sections 2 and 3 down; each was re-pointed to the line that now carries the same citation,
as above. The citations themselves were not re-read.

**Line numbers after the roles loop's item 03.** D-079, D-080, D-081, D-095, D-104 and D-105,
their index rows and six revision paragraphs in `DECISIONS.md`, and §11.3 and the first
Re-interpretation log entry in `RESULTS.md`, moved 6 of the locations in sections 2 and 3
down; each was re-pointed to the line that now carries the same citation, as above. The
citations themselves were not re-read.

**Line numbers after the roles loop's item 04.** D-082, D-083, D-084 and D-099, their index rows and five revision paragraphs in `DECISIONS.md`, the second Re-interpretation log entry in `RESULTS.md` and the placement functions in `tabletop/correspondences.py` moved locations in sections 2 and 3 down; each was re-pointed to the line that now carries the same citation, as above. The citations themselves were not re-read.

**Line numbers after the roles loop's item 05.** D-087, D-088, D-090 and D-098, their index rows and four revision paragraphs in `DECISIONS.md`, the third Re-interpretation log entry in `RESULTS.md` and the role slippage functions in `tabletop/correspondences.py` moved locations in sections 2 and 3 down; each was re-pointed to the line that now carries the same citation, as above. The citations themselves were not re-read.

Re-pointed after the roles loop's item 07 (D-085, D-093, D-096 and the revisions inserted into DECISIONS.md; the re-cast and the column reading inserted into `tabletop/correspondences.py`): the audited locations keep their tags, at their new line numbers.

Re-pointed after the roles loop's item 08 (D-086, D-100 and the revisions inserted into DECISIONS.md; the item 08 entry of the Re-interpretation log in RESULTS.md; the C5 note inserted into `tabletop/correspondences.py`): the audited locations keep their tags, at their new line numbers.

Re-pointed after the roles loop's item 09 (D-091, D-092, D-094 and the revisions of D-081, D-093, D-095, D-098 and D-104 shifted DECISIONS.md's lines; the new detectors shifted `tabletop/correspondences.py`'s): the rows above name the lines as they stand now.

Re-pointed 2026-10-14 after the roles loop's item 10 (BARRIER and CLAIMED with C4: D-089, D-101 and six revisions in DECISIONS.md; the Worldview roles in `worldview.py`; the item's Re-interpretation log entry in RESULTS.md): the audited tags are the same, on the lines they moved to.

Re-pointed 2026-10-14 after the roles loop's item 11 (D-106 and the revisions of D-077 and D-081 in DECISIONS.md, with this loop's own entries tightened in wording to stay under the file limit; §11.6 and §11.7 of RESULTS.md): the audited tags are the same, on the lines they moved to.

Re-pointed 2026-10-14 after the roles loop's item 12 (the overflow guard in `tabletop/temperature.py`; the v1 column in every block of RESULTS.md and the §11.1 block, regenerated at full size): the audited tags are the same, on the lines they moved to.

Re-pointed 2026-10-14 after the roles loop's item 13 (the opening and the probes' paragraph of "How these numbers were made" in RESULTS.md, and §11.2-§11.5 and §11.8 written): the audited tags are the same, on the lines they moved to.

Re-pointed 2026-10-14 after the roles loop's item 16 (the roles' row inserted into SPEC.md's module table, which shifted every later line of that file by one; the pointer section appended at its end): the audited tags are the same, on the lines they moved to.

Re-pointed 2026-10-15 after the fast-engine loop's item 00 (the "From D-107 on" paragraph,
D-107 and its index rows in DECISIONS.md, with the index's item markers, the by-kind
qualifiers and six "As built" paragraphs tightened to stay under the file limit; the
sentence at the top and the "Engines and speed" section of RESULTS.md): the audited tags
are the same, on the lines they moved to.

Re-pointed 2026-10-15 after the fast-engine loop's item 02 (the engine seam and the

Re-pointed 2026-10-15 after the fast-engine loop's item 03 (the index table and the index by kind moved from DECISIONS.md to `DECISIONS-INDEX.md`, D-111 and four revisions inserted; the roles' entries of the Re-interpretation log moved from RESULTS.md to `RESULTS-roles-log.md`, two speed rows and the item's entry inserted): the audited tags are the same, on the lines they moved to.
generator: D-109, D-110 and two "As revised" paragraphs in DECISIONS.md, whose index lost
its "Knob" column and whose by-kind lists keep the first clause of each title, to stay under
the file limit; the item's log entry and speed rows in RESULTS.md): the audited tags are
the same, on the lines they moved to.

Re-pointed 2026-10-15 after the fast-engine loop's item 01 (the plain-Python speedups: an
import line in `tabletop/slipnet.py`; D-108 and four "As revised" paragraphs in
DECISIONS.md, whose index lost its "Cited by" column to stay under the file limit; a
speed row and a paragraph in RESULTS.md's "Engines and speed"): the audited tags are the
same, on the lines they moved to.

Re-pointed after the roles loop's item 06, reopened and built after the final review
(D-097 and the revisions inserted into `DECISIONS.md`, with the loop's own entries shortened
in wording to keep the file under its size limit; the item's log entry appended to
`RESULTS.md`; the registry comment of `tabletop/roles.py` grown by two lines): the audited
locations of sections 2, 3 and 8 keep their tags, at their new line numbers (the nearest
line carrying the same citation). The citations themselves were not re-read.

## 8. The roles loop's citations, re-verified (2026-10-14, the loop's final review)

The roles loop (D-078 to D-106, `tabletop/roles.py` and the detectors in `describe.py`,
`correspondences.py`, `worldview.py`, `geometry.py` and `salience.py`, the role codelets,
RESULTS.md §11, `docs/roles.md` and `docs/predictions.md`) added 236 page citations to the
folder. The final review drew 20 of them with the tool's own generator and read each on its
page image, as section 1 describes (90 dpi; FCCA p. 381 from the book's PDF): the pool is
every citation of the tool's inventory on a line added or changed since the loop's baseline
commit (`e38d6e0`, by `git blame`), plus every citation of the two documents the loop added,
drawn without replacement by `rng.below(len(pool))` with the generator seeded 17. The
`(img)` stratum has no member in the pool (the loop cites no figure or equation by image),
so the twenty are plain citations, fourteen of them quotations the checker reads and six
paraphrases it never reads. Verdict for all twenty: **yes**; nothing was fixed. Pages
118-140 are the loop's most cited (five of the twenty are p. 140).

| # | Location | Citation | Claim | Verdict |
|---|---|---|---|---|
| 1 | `DECISIONS.md:2578` | `[F95 p. 140]` | forks and glasses seen as opposites, a temporary conceptual symmetry | yes |
| 2 | `tabletop/roles.py:8` | `[F95 p. 140]` | the figure object, an exceptional object standing out against a dull background | yes |
| 3 | `tabletop/correspondences.py:409` | `[F95 p. 28]` | literal sameness is always a pressure (the TWIN detector) | yes |
| 4 | `RESULTS.md:909` | `[F95 pp. 103-105]` | a correspondence between the two spoon groups enters the Worldview in French's run (statement 16) | yes: figure 4.7 shows it and p. 105 says the building of it sent mixed signals |
| 5 | `tabletop/roles.py:202` | `[F95 p. 131]` | weak sidekicks, single objects rather than small sameness groups, are not sufficient (the tag's weight) | yes |
| 6 | `tabletop/correspondences.py:378` | `[F95 p. 140]` | each one echoes, in object type, a larger group across the table (the ECHO detector) | yes |
| 7 | `docs/roles.md:183` | `[FCCA p. 381]` | the node set could take many more nodes without a change of architecture | yes, verbatim |
| 8 | `DECISIONS.md:3389` | `[F95 p. 140]` | the temporary conceptual symmetry of forks and glasses (D-094's source) | yes |
| 9 | `tabletop/codelets/scouts.py:166` | `[F95 p. 76]` | find-neighbor takes an object of the group just found and determines its neighbours; an object in no group has no neighbour | yes, both |
| 10 | `tabletop/geometry.py:156` | `[F95 p. 85]` | left and right in the table frame refer to the sides of the table as a whole | yes |
| 11 | `DECISIONS.md:3035` | `[F95 p. 131]` | weak sidekicks are not sufficient (D-090's source) | yes |
| 12 | `docs/predictions.md:98` | `[F95 p. 115]` | the subjects' order is the diagonally opposite corner first, then the directly opposite one | yes |
| 13 | `RESULTS.md:905` | `[F95 p. 125]` | the program favours group-to-group mappings over group-to-isolated-object ones (statement 12) | yes |
| 14 | `DECISIONS.md:2970` | `[F95 p. 32]` | the saltshaker and the touched cup are surrounded by identical groups (D-087's source) | yes |
| 15 | `tabletop/describe.py:488` | `[F95 p. 32]` | the same sentence, for the ANCHOR detector | yes |
| 16 | `DECISIONS.md:3364` | `[F95 p. 28]` | literal sameness is always a pressure (D-092's source) | yes |
| 17 | `DECISIONS.md:3307` | `[F95 p. 36]` | the program should ignore, not consider and then reject, the small groups of silverware (D-086's source) | yes; the page's dashes are double hyphens in the text layer, as quoted |
| 18 | `DECISIONS.md:3068` | `[F95 p. 68]` | the end/middle location and position slippages among the static factors (D-090's source) | yes |
| 19 | `DECISIONS.md:3263` | `[F95 pp. 128-129]` | the gap widened glass by glass until the program no longer perceives the pair as a group | yes: Blockage 8-10 |
| 20 | `docs/roles.md:34` | `[F95 p. 140]` | Henry's glass is a figure object against a dull background | yes |

The line numbers are those of the tree at the review (DECISIONS.md and RESULTS.md at
their roles-loop sizes); the citations in sections 2 and 3 are the reconstruction's and were
re-pointed, not re-read, after each item of the loop, as the notes above say.

Re-pointed 2026-10-15 after the fast-engine loop's item 04 (an engineering note inserted in SPEC.md's module section, which shifted every later line of that file; D-112 and six "As revised" paragraphs in DECISIONS.md; three speed rows and a paragraph in RESULTS.md; the plain-Python fixes in `objects.py`, `problems.py`, `coderack.py`, `geometry.py`, `roles.py`, `workspace.py`, `temperature.py`, `config.py` and `salience.py`): the audited tags are the same, on the lines they moved to.

Re-pointed 2026-10-15 after the fast-engine loop's item 07 (an engine line at the head of every generated block of RESULTS.md and the item's paragraphs in "How these numbers were made", "Engines and speed" and §9; the engine line in `tabletop/bench/calibrate.py`'s `main`; D-107's item 07 paragraph in DECISIONS.md): 3 locations in `RESULTS.md`, 1 location in `tabletop/bench/calibrate.py` re-pointed to the lines their tags moved to, nothing re-read.

Re-pointed 2026-10-17 after v3's optimized defaults (D-114: the head of `DECISIONS.md` moved to `DECISIONS-INDEX.md`, D-060's revision, the generated blocks and the v3 section of `RESULTS.md`, the comment lines of `tabletop/config.py` and a docstring of `tabletop/temperature.py`): 13 locations in `DECISIONS.md`, 5 in `RESULTS.md`, 1 in `tabletop/bench/calibrate.py` and 1 in `tabletop/temperature.py` re-pointed to the lines their tags moved to, nothing re-read.

Re-pointed 2026-10-17 after v4's defaults (D-115: the v4 head paragraph, the generated blocks, §13 and the log entry of `RESULTS.md`): 5 locations in `RESULTS.md` re-pointed to the lines their tags moved to, nothing re-read.

Re-pointed 2026-10-17 after v5's structural Blockage fix (D-116 to D-120: five entries and five "As revised" lines in `DECISIONS.md`, with D-073's catalogue and three as-built paragraphs moved verbatim to `DECISIONS-INDEX.md`; `award` rewritten in `tabletop/correspondences.py` and `adjusted_urgency` in `tabletop/coderack.py`; the generated blocks, §14 and the log entry of `RESULTS.md`): 13 locations in `DECISIONS.md`, 5 in `RESULTS.md`, 1 in `tabletop/correspondences.py` and 1 in `tabletop/coderack.py` re-pointed to the lines their tags moved to, nothing re-read.
