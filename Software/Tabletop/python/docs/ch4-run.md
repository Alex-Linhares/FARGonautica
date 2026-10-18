# The chapter 4 run, dump by dump

French follows one run of Tabletop through nine screen dumps (Figs. 4.1-4.9), then shows five
Worldviews from other runs (Figs. 4.10-4.14) [F95 pp. 95-111]. This file is the transcription
of every dump: what the table, the thermometer, the Slipnet and the Coderack show, read on the
page images at 300 dpi (600 dpi for the doubtful boxes; the strips' bar positions measured
against the 23 evenly spaced labels with a pixel script), with French's narration beside each.
The numbers are in `tests/data/ch4.json` (checked by `tests/test_ch4.py`); what the run says
about the mechanisms is in `SPEC.md` §6c. Every quotation is machine-checked; a citation marked
(img) was read from the image. Doubtful readings are flagged in place and collected at the end.

The run was chosen on purpose as an imperfect one: "The series of screen dumps in this chapter
was taken from a nonideal run—that is, a run in which not all of the good structure was found by
the program." [F95 p. 95] "If a run of Tabletop is cut short, its answer will, in general, be
based more on object category than on relational structure. The longer the program runs, the
more likely it will be to find relational structure." [F95 p. 95]

## The screen

"To the left of the table is the palette used to select objects for the table." [F95 p. 95]
"Immediately to the right of the table is the thermometer. This will indicate the computational
temperature, which falls in the range from 0 to 100." [F95 p. 95] "To the right of the
thermometer, the Slipnet is represented. The various nodes of the Slipnet are represented by
the square boxes forming the grid." [F95 p. 95] "Activation values range from 1 to 150."
[F95 p. 95] "Below the table is a representation of the Coderack, indicating the percentage of
particular codelet types waiting to run. The total number of codelets run so far is also
indicated." [F95 p. 97] "In general, the types of codelets that run early in the program are to
the left of the bar graph; those that run later are to the right." [F95 p. 97]

**The Slipnet grid** has seven columns and six rows, 41 boxes, in this order (SPEC.md §3.1):
spoon, silverware, buccal obj, sim shape, neighbor, horiz, one / small glass, crockery, open
obj, used with, direction, vert, two / fork, receptacle, closed obj, symmetry, right, position,
three / knife, eat-from-ob, big. than, mirror, left, end, many / cup, liq holder, sm. than,
diagonal, above, middle, same / sharp obj, food holder, opposite, group, below, number
[F95 p. 98 (img)].

**The Coderack strip** carries 23 labels, evenly spaced; beside Fig. 4.1 the book prints a
legend giving each label's full name [F95 p. 96 (img)]:

| Label | Legend | Label | Legend |
|---|---|---|---|
| FND-GP | find-group | PK-SALO | pick-salient-obj |
| SAM | simple-sameness-detect. | PK-SALS | pick-salient-subgp. |
| FD-END | find-end-objs. | PK2SALO | pick-2nd-salient-obj |
| FD-MID | find-middle-objs. | CT-CP | create-cp |
| FD-SIMN | find-sim.-nhbr | PK-SALC | pick-strong-cp |
| FD-NNBR | find-neighbor | EXAM-CP | examine-cp |
| BD-OB0 | build-obj-0 | BLD-CP | build-cp-descrip. |
| BD-OB1 | build-obj-1 | CP-COMP | cp-compete |
| BD-OB2 | build-obj-2 | CHK-WV | check-worldview |
| BD-OB-DSC | build-obj.-desc. | CP-CLN | clean-up-corresp. |
| SC-SMGP | search-for-sam.-gp | FND-ANS | find-answer |
| SC-SIMGP | search-for-sim.-gp | | |

The legend settles the readings SPEC.md §5.2 had marked as inferences: PK-SALS is
pick-salient-subgroup (not a pick-salient-object variant), PK-SALC is pick-strong-cp (the text's
"pick-salient-correspondence" [F95 p. 65] is presumably the same codelet under another name),
BLD-CP is build-cp-description (not build-correspondence), and CT-CP is create-cp.

**The bars.** A bar's number is printed above it. In every dump the bars sit exactly over a
label (the measured offsets are within a tenth of the label spacing), so the readings below
are not ambiguous. The printed numbers are rounded, and sum to 98-101.

## The configuration (Fig. 4.1, p. 96)

The table of Fig. 4.1 is the one chapter 5 calls Surround 5 [F95 p. 118 (img)]. Henry's side,
upper left, a row of six: fork, fork, knife, **cup** (touched, with the H arrow), spoon, spoon.
Eliza's side, lower left, a row of six: spoon, spoon, small glass, fork, fork, knife. An
isolated cup in the lower right corner [F95 p. 96 (img)]. The palette's counts above its cells
confirm the inventory: 4 forks, 2 knives, 4 spoons, 2 cups, 1 small glass [F95 p. 98 (img)].
All the silverware points up. Positions, as fractions of the table's side measured on Fig.
4.2's image (good to about 0.02), are in `ch4.json`; the two rows are at about 0.16 and 0.75 of
the height, the objects about 0.045 apart, the isolated cup at (0.81, 0.75).

## Fig. 4.1, 0 codelets run (p. 96)

- **Table:** the thirteen objects and the H arrow; no structure.
- **Thermometer:** empty (no reading).
- **Slipnet:** opposite 139; cup 51; liq holder 4; small glass, crockery, open obj,
  eat-from-ob 2; every other box 1 [F95 p. 96 (img)].
- **Coderack:** empty; "Number of codelets run: 0".
- **French:** "Henry has just touched an object on his side of the table. In a moment, this
  will cause the Coderack to be loaded up with its standard set of starting codelets, but
  things are already happening in the Slipnet." [F95 p. 97] The node 'opposite' has been given
  a large amount of activation because Eliza is sitting opposite Henry; 'cup' is active because
  a cup was touched [F95 p. 97]. "Even though no codelets have yet run, some activation has
  already begun to spread from 'cup' to ''liquid holder,' 'crockery,' and so on." [F95 p. 97]
- **Doubtful:** the page is printed rotated by 90 degrees; it was rotated upright before
  reading.

## Fig. 4.2, 2 codelets run (p. 98)

- **Table:** a dashed wavy box around Henry's six objects: the gestalt group.
- **Thermometer:** 80.0.
- **Slipnet:** cup 89, opposite 139, group 44, horiz 22, spoon 9, fork 9, liq holder 9, knife
  5, small glass 4, crockery 4, open obj 4, eat-from-ob 4, silverware 2, closed obj 2, right 2,
  above 2; the rest 1 [F95 p. 98 (img)].
- **Coderack:** FND-GP 56, SAM 6, FD-END 3, FD-MID 3, FD-SIMN 26, PK2SALO 7 (sum 101)
  [F95 p. 98 (img)].
- **French:** "At this point only two codelets—one of them a find-group codelet—have run. The
  find-group codelet discovered that the touched cup is part of a group of objects." [F95
  p. 97] "For the moment, though, the program knows next to nothing about this group, other
  than that it is a bunch of objects that are physically close together. This is a gestalt
  view of the group" [F95 p. 97]. The find-group codelet "posted a number of other codelets to
  the coderack, among which are: codelets that look for sameness groups" within the group,
  "others whose job it is to determine the objects at the ends and in the middle of the new
  group, and others that pick objects inside the group and 'look' to the right and left of
  those objects to determine their immediate neighbors" [F95 p. 97]. It also "has sent a
  standard jolt of activation to the 'group' node in the Slipnet" [F95 p. 97].
- **Doubtful:** the 'direction' box shows a dot where its number should be (read as 1). The
  second codelet is not named; 'horiz' is already 22 although the text places the discovery of
  horizontality at Fig. 4.3.

## Fig. 4.3, 12 codelets run (p. 99)

- **Table:** Henry's gestalt box; a dashed line from the touched cup to the isolated cup.
  Eliza's group is not yet drawn.
- **Thermometer:** empty (no reading).
- **Slipnet:** cup 127, opposite 137, horiz 55, neighbor 41, liq holder 32, group 26, right 26,
  left 20, open obj 20, small glass 17, crockery 15, eat-from-ob 14, closed obj 13, end 9, two
  8, middle 8, direction 7, fork 6, above 6, spoon 5, silverware 5, knife 5, below 5, used
  with 4, big. than 4, receptacle 3, sm. than 3, buccal obj 2, vert 2, position 2, sharp obj 2,
  number 2; the rest 1 [F95 p. 99 (img)].
- **Coderack:** SAM 15, FD-END 8, FD-MID 8, FD-NNBR 30, BD-OB0 21, PK2SALO 18 (sum 100)
  [F95 p. 99 (img)].
- **French:** "the program has just made a tentative (i.e., dashed) correspondence between the
  touched cup and the isolated cup in the lower right-hand corner of the table" [F95 p. 97],
  attractive because that cup "is one of the most salient objects on the table. Its salience is
  due to two facts: it shares object type ('cup') with the touched object, and it is in a very
  good position on the table with respect to the touched object (i.e., roughly diagonally
  opposite from it)." [F95 p. 97] "the program has discovered that the objects in it seem to be
  arranged horizontally, thus activating the concept 'horizontal.' The program has further
  discovered many of the neighbors of objects inside this group, and therefore the concept
  'neighbor' is active, causing more 'find-neighbor' codelets to be posted on the Coderack."
  [F95 p. 97] "The concept 'group' is fairly active, since a find-group codelet was successful
  (a group was found); the Slipnet node 'group' has thus spawned a fair number of similar
  find-group codelets." [F95 p. 97] This is visible in the Coderack, "which shows that 21
  percent of the codelets are ones whose job it is to find (and possibly build) new groups of
  objects" [F95 p. 101]. "After 'cup,' the concept 'small glass' is the most active
  table-object concept, having received its activation from 'cup.'" [F95 p. 101]
- **Doubtful:** the 21 percent bar stands over BD-OB0 (build-obj-0); FND-GP has no bar at all
  in this dump. French's "find (and possibly build)" therefore describes the build-object
  codelets, and his "spawned a fair number of similar find-group codelets" is not visible on
  the strip.

## Fig. 4.4, 22 codelets run (p. 100)

- **Table:** the cup-cup line is now solid; a dashed wavy box around Eliza's six objects; a
  small dashed rectangle around Eliza's two forks with a pointer under it.
- **Thermometer:** 59.0.
- **Slipnet:** opposite 137, cup 128, end 105, horiz 48, neighbor 45, right 41, liq holder 37,
  left 30, open obj 27, group 23, small glass 20, closed obj 18, crockery 17, eat-from-ob 15,
  spoon 13, knife 13, direction 11, position 9, above 8, fork 7, used with 7, two 7, middle 7,
  below 7, silverware 6, big. than 5, receptacle 4, sm. than 4, vert 3, buccal obj 2, three 2,
  sharp obj 2, number 2; the rest 1 [F95 p. 100 (img)].
- **Coderack:** SAM 10, FD-END 5, FD-MID 5, FD-NNBR 17, BD-OB0 21, PK-SALO 3, PK2SALO 16,
  FND-ANS 24 (sum 101) [F95 p. 100 (img)].
- **French:** "the tentative (dashed) cup-cup correspondence has now been solidified and is
  thus now officially part of the Worldview. A large group of objects on Eliza's side of the
  table has also recently been discovered" [F95 p. 101]; "it has just discovered that the two
  forks in that group are neighbors" [F95 p. 101]. "In the meantime, back in the big group on
  Henry's side, the program has also discovered the fork-fork subgroup and the spoon-spoon
  subgroup." [F95 p. 101] "These subgroups cannot be seen in the picture, because a subgroup is
  visible only when the program first discovers it, and thereafter only if it forms the end
  point of a correspondence in the Worldview." [F95 p. 101] "The program has noticed that these
  subgroups are on the ends of the group, which is why the concept 'end' is highly active in
  the Slipnet." [F95 p. 101] They are at the left and right ends, "which explains why 'left'
  and 'right' (and not 'above' and ''below') are active" [F95 p. 101].
- **Doubtful:** 'horiz' is 48 at 600 dpi (40 at a glance). The marker around Eliza's forks has
  no label; it is read as the neighbour relation French mentions.

## Fig. 4.5, 33 codelets run (p. 102)

- **Table:** both gestalt boxes; the cup-cup line; a solid rectangle around Eliza's two spoons
  with the caption "Build new subgroup".
- **Thermometer:** 43.2.
- **Slipnet:** opposite 150 (the ceiling), cup 129, end 102, neighbor 92, horiz 82, right 75,
  group 64, left 55, liq holder 41, open obj 35, fork 29, small glass 27, spoon 24, closed obj
  24, knife 21, crockery 20, direction 17, eat-from-ob 17, position 15, above 12, silverware
  10, used with 9, below 9, big. than 6, two 6, middle 6, sm. than 5, receptacle 4, vert 4,
  buccal obj 3, diagonal 3, sharp obj 3, number 3, three 2; the rest 1 [F95 p. 102 (img)].
- **Coderack:** SAM 2, FD-END 1, FD-MID 1, FD-NNBR 6, BD-OB0 13, SC-SMGP 70, PK-SALO 1,
  PK2SALO 3, CP-COMP 1, FND-ANS 2 (sum 100) [F95 p. 102 (img)].
- **French:** "the program has just built a spoon-spoon subgroup in the group around the small
  glass, and that subgroup is therefore briefly visible. As soon as this subgroup was built, a
  large number of search-for-same-group codelets were posted to the Coderack (shown by the tall
  bar in the middle, with '70' above it)." [F95 p. 101] "Notice that the temperature has fallen.
  This is because the correspondences in the Worldview (only one, in this case, running from
  the touched cup to the isolated cup) have changed very little. This lack of change starts to
  suggest to the program that perhaps the structure is getting pretty good, and thus the
  temperature decreases." [F95 p. 101]

## Fig. 4.6, 36 codelets run (p. 103)

- **Table:** both gestalt boxes; the cup-cup line, thin; Henry's spoon-spoon and Eliza's
  spoon-spoon subgroups in reverse video, joined by a thick line.
- **Thermometer:** 31.3.
- **Slipnet:** opposite 150, right 133, horiz 132, cup 129, end 99, left 92, neighbor 90,
  group 57, liq holder 45, open obj 45, spoon 38, closed obj 32, small glass 30, direction 28,
  fork 27, knife 25, crockery 22, position 21, eat-from-ob 19, above 16, silverware 14, below
  13, used with 12, two 10, big. than 8, sm. than 8, middle 6, receptacle 5, vert 5, diagonal
  5, sharp obj 5, buccal obj 4, number 3, sim shape 2, three 2; the rest 1 [F95 p. 103 (img)].
- **Coderack:** SAM 5, FD-END 2, FD-MID 2, FD-NNBR 15, BD-OB0 32, PK-SALO 2, PK2SALO 8, CP-COMP
  2, FND-ANS 30 (sum 98) [F95 p. 103 (img)].
- **French:** "A while back, the program spotted a spoon-spoon subgroup at the right end of the
  group surrounding Henry's cup. Figure 4.6 shows that it has now made a tentative
  correspondence between the two spoon-spoon groups and is testing its quality." [F95 p. 101]
  "when a correspondence is first discovered, it is highlighted by flashing boxes at both ends.
  This screen dump happens to have recorded the reverse-video part of the flashing." [F95
  p. 101] "The quality of this correspondence is very high, since the two subgroups not only
  are identical in terms of category membership but also are in very good positions with
  respect to one another (i.e., they more or less directly face each other across the table).
  Since this correspondence is not a rival to any correspondence already in the Worldview, it
  is put in the Worldview without a fight." [F95 p. 101] "Notice that this act makes the
  Worldview incoherent—that is, one correspondence goes from an object inside one group to an
  object inside another group, and the second correspondence (between the two cups) starts
  inside one group and ends on an isolated object." [F95 p. 101] "There is strong pressure for
  all Worldview correspondences to originate in a single group and to end in a single group."
  [F95 p. 101] Rivals "start or finish at the same end point" [F95 p. 101] and "are forbidden
  to belong to the Worldview at the same time" [F95 p. 105]; "incoherent correspondences are
  like local aspects of an impossible figure" [F95 p. 105].
- **Doubtful:** 'spoon' 38 and 'small glass' 30 at 600 dpi; 'end' prints as "99." with a
  trailing mark, read as 99.

## Fig. 4.7, 47 codelets run (p. 104)

- **Table:** both gestalt boxes; the two spoon-spoon subgroups as dashed rectangles with a
  thick solid line between them; the cup-cup line now dashed; a new dashed line from the
  touched cup to Eliza's small glass.
- **Thermometer:** 35.8.
- **Slipnet:** opposite 150, right 150, horiz 132, left 132, end 132, cup 130, neighbor 105,
  open obj 58, spoon 53, small glass 53, liq holder 50, direction 45, knife 45, closed obj 42,
  fork 36, group 32, position 29, crockery 25, above 23, mirror 22, same 22, eat-from-ob 21,
  silverware 20, below 19, used with 15, sm. than 15, big. than 13, two 12, sim shape 9,
  diagonal 8, sharp obj 7, vert 7, buccal obj 6, receptacle 6, middle 5, number 4, three 3;
  the rest 1 [F95 p. 104 (img)].
- **Coderack:** SAM 5, FD-END 2, FD-MID 2, FD-NNBR 11, BD-OB0 35, BD-OB1 1, PK-SALO 2,
  PK2SALO 8, CP-COMP 3, FND-ANS 29 (sum 98) [F95 p. 104 (img)].
- **French:** "a tentative correspondence from Henry's cup to Eliza's glass has been suggested.
  This correspondence is a rival to the already-existing one from Henry's cup to Eliza's cup,
  so there is a fight to enter (or remain in) the Worldview." [F95 p. 105] The cup-cup
  correspondence "is indicated during the competition with the new rival correspondence with a
  dashed line (actually a flashing dashed line on the computer screen)" [F95 p. 105]. "Notice
  that the temperature has gone up relative to figure 4.6. This is because the building of the
  new Worldview correspondence between the two spoon groups sent mixed signals to the program."
  [F95 p. 105] "On the one hand, adding a very strong correspondence to the Worldview pushes for
  the temperature to decrease somewhat. On the other hand, adding the new correspondence is
  taken by the program as a hint that there may be still more such correspondences." [F95
  p. 105] "The result of these opposing pressures is a slight increase in the temperature,
  slightly reducing the probability for the program to run a find-answer codelet" [F95
  p. 105]. "the default object corresponding to the touched object is the touched object
  itself" [F95 p. 105].
- **Doubtful:** the Slipnet panel is identical, box for box, to those of Figs. 4.8 and 4.9,
  and the Coderack panel and the count (47) are identical to Fig. 4.8's.

## Fig. 4.8, 47 codelets run (p. 106)

- **Table:** the cup-cup line is gone; the cup-to-small-glass line is solid; the spoon-spoon
  group correspondence stays.
- **Thermometer:** 13.6.
- **Slipnet and Coderack:** identical to Fig. 4.7 [F95 p. 106 (img)].
- **French:** "the cup/small-glass correspondence has won the fight, because the structure
  rating of a coherent Worldview containing a cup-glass correspondence is higher than that of an
  incoherent Worldview containing a cup-cup correspondence, even though a cup-cup correspondence
  is a priori stronger than a cup-glass correspondence" [F95 p. 105]. "The fact that the
  Worldview has just changed (the cup-cup correspondence has been removed, and the cup-glass
  correspondence has been added) would tend to increase the temperature. On the other hand,
  when structure improves, this tends to cause the temperature to decrease." [F95 p. 105] "the
  overall Worldview rating has improved so much (the program is quite heavily biased against
  incoherent Worldviews) that this fact by far dominates the pressure to increase the
  temperature. The temperature therefore drops." [F95 p. 105] "The run ends only when a
  find-answer codelet is run." [F95 p. 105]
- **Doubtful:** the count is 47, as in Fig. 4.7: the fight was decided, the Worldview changed
  and the temperature fell from 35.8 to 13.6 between two dumps at the same codelet count.

## Fig. 4.9, 55 codelets run (p. 107)

- **Table:** as Fig. 4.8, plus a short arrow with an E at its tail pointing at the small glass.
- **Thermometer:** 2.0.
- **Slipnet:** identical to Figs. 4.7 and 4.8 [F95 p. 107 (img)].
- **Coderack:** SAM 5, FD-END 3, FD-MID 3, FD-NNBR 11, BD-OB0 35, PK-SALO 2, PK2SALO 8, CP-COMP
  3, FND-ANS 30 (sum 100) [F95 p. 107 (img)].
- **French:** "The correspondences in the Worldview in figure 4.9 are no longer changing. This
  lack of change is by definition what makes the temperature fall, so the temperature does
  fall." [F95 p. 105] The program concludes "that it has probably found all the good structure
  that it is going to find—even though it has missed all the correspondences between the
  silverware to the left of Henry's cup and to the right of Eliza's glass" [F95 p. 105]. "A
  find-answer codelet finally runs, Eliza touches the small glass (her choice is always shown
  by a short arrow with an E at its back end), and the program comes to a halt." [F95 p. 105]
- **Doubtful:** the Slipnet is unchanged eight codelets after Fig. 4.8; either no box changed
  (unlikely, with decay running) or the panel was not redrawn for this dump.

## The run in one table

| Fig. | Page | Codelets | T | Worldview after the dump | New on the table |
|---|---|---|---|---|---|
| 4.1 | 96 | 0 | — | empty | the objects |
| 4.2 | 98 | 2 | 80.0 | empty | Henry's gestalt group |
| 4.3 | 99 | 12 | — | empty | cup-cup tentative |
| 4.4 | 100 | 22 | 59.0 | cup-cup | Eliza's gestalt group; her forks are neighbours |
| 4.5 | 102 | 33 | 43.2 | cup-cup | Eliza's spoon-spoon subgroup built |
| 4.6 | 103 | 36 | 31.3 | cup-cup (+ spoons-spoons entering) | the spoon-spoon group correspondence under test |
| 4.7 | 104 | 47 | 35.8 | cup-cup, spoons-spoons | cup-glass rival; the fight |
| 4.8 | 106 | 47 | 13.6 | cup-glass, spoons-spoons | cup-cup removed |
| 4.9 | 107 | 55 | 2.0 | cup-glass, spoons-spoons | the answer: the small glass |

[F95 pp. 96-107 (img)]. The thermometer is empty in Figs. 4.1 and 4.3.

## Other runs (Figs. 4.10-4.14, pp. 108-110)

"because of its probabilistic architecture, Tabletop does not follow a fixed sequence of steps
to arrive at its answer to a given challenge; rather, on different runs it follows different
pathways, sometimes leading to different answers, sometimes leading to the same answer" [F95
p. 108]. The five partial dumps omit the Slipnet and the Coderack; Fig. 4.14 keeps the
thermometer.

- **Fig. 4.10** [F95 p. 108 (img)]: the two fork-fork subgroups boxed; a group correspondence
  between them with two fork-to-fork correspondences inside it; the cup-to-small-glass
  correspondence; E at the small glass. "It built correspondences from both of the forks of the
  upper group to corresponding forks in the lower fork-fork group, as well as a correspondence
  between the two subgroups themselves. It did not, however, notice that there were two
  spoon-spoon groups that could similarly be put into correspondence." [F95 p. 108]
- **Fig. 4.11** [F95 p. 109 (img)]: the structure of Fig. 4.10 plus a line from the spoon
  right of Henry's cup to the spoon left of Eliza's glass. The program "has realized that
  Henry's cup and Eliza's small glass are both flanked by spoons. These spoons are put into
  correspondence." [F95 p. 108] Not as strong as the spoon-spoon group correspondence of the
  main run, "it nonetheless strongly bolsters the program's confidence in picking the small
  glass" [F95 p. 108].
- **Fig. 4.12** [F95 p. 109 (img)]: four subgroups boxed (both fork pairs, both spoon pairs);
  three correspondences, forks-forks, spoons-spoons and cup-glass, crossing; E at the small
  glass. "a very typical set of Worldview mappings, one of the most frequently found by the
  program. Notice that the program did not take the trouble to look for correspondences inside
  the subgroup-to-subgroup correspondences." [F95 p. 108] "Notice also that the program did not
  find the knife-knife correspondence, even though this correspondence is extremely obvious to
  humans" [F95 p. 109]: "when there are groups of objects on the table, the program is strongly
  biased to hunt for group-level correspondences in preference to object-level correspondences.
  Thus, this bias is clearly a bit too strong." [F95 p. 109] (fault 3 of SPEC.md §6b).
- **Fig. 4.13** [F95 p. 110 (img)]: the four subgroups boxed; forks-forks and spoons-spoons
  mapped, and the touched cup mapped to the isolated cup; E at the isolated cup. "on rare
  occasions, Tabletop is capable of finding a great deal of very good structure between the two
  main groups of objects on the table but still deciding to pick the isolated cup" [F95
  p. 109]. The Worldview is incoherent, "since the cup-cup correspondence is off by itself"
  [F95 p. 110]; "if the program were ever to put the cup-glass correspondence in competition
  with the cup-cup correspondence, the former would almost certainly win" [F95 p. 110]. "either
  the program never even noticed the potential of a cup-glass correspondence, or else it did do
  so, but when that correspondence was suggested, it was defeated by the rival cup-cup
  correspondence" [F95 p. 110]. Doubtful: two lines join the fork boxes, one thick and one
  thin; whether the thin one is an inner correspondence is not legible.
- **Fig. 4.14** [F95 p. 110 (img)]: the thermometer reads about 97.5 (the last digit is not
  legible at 800 dpi); Henry's forks boxed, and a box around the touched cup and the spoon next
  to it; Eliza's forks boxed; a thick line forks-forks and a thin line from the cup-spoon box
  to Eliza's leftmost spoon; no E arrow. "It is rare for a cup-spoon grouping, as shown here,
  to survive. The program is, in general, highly unlikely to group together objects of such
  disparate categories." [F95 p. 110] "Similarly, it strongly resists mapping groups onto
  individual objects, although under some circumstances it might be happy mapping a spoon-spoon
  group to an individual spoon. Here, however, it suggests, tests, and even winds up putting
  into the Worldview, at least for a short time, a correspondence from a cup-spoon group to a
  single spoon." [F95 p. 111] French defends it: "This would mean that some pathways of
  exploration were theoretically completely closed, which would violate one of the major tenets
  of the philosophy behind the architecture—namely, that all pathways should always be, at
  least theoretically, open to exploration (although not with equal probabilities)." [F95
  p. 111]

## Drawing conventions read from the dumps

- A gestalt (proximity) group is a dashed wavy box [F95 p. 98 (img)]; a subgroup is a dashed
  rectangle, shown when first built (then with the caption "Build new subgroup", solid) and
  when it ends a Worldview correspondence [F95 p. 101; F95 p. 102 (img)].
- A tentative or competing correspondence is a dashed line; a Worldview correspondence is
  solid; a group-level correspondence is drawn thicker than an object-level one; a newly
  discovered correspondence flashes its two ends (reverse video) [F95 p. 101; F95 p. 104 (img)].
- The answer is a short arrow with an E at its tail [F95 p. 105]; Henry's touch is the H arrow.

## Doubtful readings, collected

1. The thermometer is empty in Figs. 4.1 and 4.3 (no number, no bar).
2. Fig. 4.2: the 'direction' box shows a dot (read as 1); 'horiz' is already 22 though the text
   places the discovery of horizontality at Fig. 4.3.
3. Fig. 4.3: the "21 percent" bar is over BD-OB0, and FND-GP has no bar.
4. Fig. 4.4: 'horiz' 48; the unlabelled marker around Eliza's forks.
5. Fig. 4.6: 'spoon' 38, 'small glass' 30, 'end' "99." read as 99.
6. Figs. 4.7-4.9 share one Slipnet; Figs. 4.7 and 4.8 share one Coderack and the count 47.
7. The bars' printed numbers sum to 101 (Figs. 4.2, 4.4), 98 (Figs. 4.6-4.8) or 100.
8. Figs. 4.10, 4.11, 4.13: which object each inner line joins is read from the geometry.
9. Fig. 4.14: 97.5 or 97.6.
10. The positions in `ch4.json` are measurements of a printed dump, good to about 0.02 of the
    table's side; the book gives no coordinates.
