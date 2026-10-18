# Registered predictions for the roles

Written at item 01 of the roles loop, on 2026-10-14, on commit `4657583` (the engine of
`e38d6e0`, "v1", with no role code: `tabletop/roles.py` does not exist). Everything above the
`## Scored` heading at the end is registered: from the commit that adds this file, it is never
edited, and the loop's gate compares it with that commit on every run. Item 13 scores it below
the heading, hits and misses alike. This is the guard against fitting that French's chapter 5
numbers invite: the direction and the rough size of every expected change are written here
before the first role is perceived, and a change that goes the other way is a miss, reported as
one.

The vocabulary is the package's own (D-078): a **role** is the function an item plays in the
analogy, not what it looks like; a **consumer** is the mechanism that reads a role tag and does
something with it; the **re-cast** carries the touched object's roles onto Eliza's side. The 23
roles and the eight consumers are the roles loop's plan; every one of them becomes a decision
after D-078, written before its code. The French pages behind each role are given here so
that the prediction can be read against his account, not only against his program's numbers.

## 1. How to read these predictions

**The baseline is v1**, RESULTS.md §1-§10 and `results/v1/`: 500 runs per result set under
`standard` at bench seed 1. The answers are French's labels (E1, E2, E3 of RESULTS.md §2; `other`
is every answer he prints no bar for). The v1 values that the predictions below are measured
against:

| Result set | French's main answer | French % | v1: E1 | E2 | E3 | other |
|---|---|---:|---:|---:|---:|---:|
| Surround 1 | E2 | 95 | 32.6 | 50.2 | - | 17.2 |
| Surround 2 | E2 | 90 | 16.8 | 45.8 | - | 37.4 |
| Surround 3 | E2 | 85 | 20.4 | 40.4 | - | 39.2 |
| Surround 4 | E2 | 55 | 21.8 | 41.6 | - | 36.6 |
| Surround 5 | E1 | 86 | 17.6 | 36.6 | - | 45.8 |
| Surround 6 | E1 | 100 | 26.8 | 31.4 | - | 41.8 |
| Surround 7 | E1 | 80 | 15.2 | 40.6 | - | 44.2 |
| Surround 8 | E2 | 90 | 16.6 | 37.8 | - | 45.6 |
| Surround 9 | E2 | 98 | 4.2 | 46.4 | - | 49.4 |
| Surround 10 | E1 | 73 | 13.6 | 33.0 | - | 53.4 |
| Blockage 1 | E1 | 100 | 92.0 | 8.0 | - | - |
| Blockage 2 | E1 | 88 | 74.0 | 11.6 | - | 14.4 |
| Blockage 3 | E1+E2 | 100 | 85.2 (E1+E2) | | 7.4 | 7.4 |
| Blockage 4 | E1+E2 | 80 | 81.4 (E1+E2) | | 7.6 | 11.0 |
| Blockage 5 | E1+E2 | 58 | 78.8 (E1+E2) | | 10.0 | 11.2 |
| Blockage 6 | E3 | 78 | 81.8 (E1+E2) | | 7.6 | 10.6 |
| Blockage 7 | E3 | 76 | 59.6 (E1+E2) | | 8.2 | 32.2 |
| Blockage 8 | E3 | 62 | 79.2 (E1+E2) | | 8.2 | 12.6 |
| Buridan 1 (modified) | E1 = E2 | 50 | 31.8 | 16.4 | - | 51.8 |
| Figure/Ground 1 | E1+E2 | 100 | 81.8 (E1+E2) | | 8.4 | 9.8 |

(In Blockage 1 and 2 the touched cup is E2; from Blockage 3 on it is E3. Figure/Ground's E3
is Henry's lone glass, which French's program never gave [F95 p. 140] and v1 gives in 8.4
percent of runs, by the default answer and thin Worldviews, with a mean structure value of
52.7 against 83.8 for the forks.)

**The mechanism measures**, read beside every frequency, from the traces and the snapshot:
- **Worldview size**: the share of runs whose final Worldview holds two or more
  correspondences. In v1 it is about 2 percent on every problem measured (RESULTS.md §9.2:
  at most one correspondence in 98 percent of 240 runs over four problems), because every
  object correspondence shares the touched object as an end and so is a rival of every other
  (D-052 item 24, D-044).
- **Coherent and incoherent pairs** per run (D-042): 0 and 0 in every v1 run measured. The
  `buridan` penalty scan is exactly flat for this reason (RESULTS.md §9.2).
- **Group correspondences**: the share of runs with a group-to-group correspondence in the
  Worldview; 0.4 percent of the `ch4` bench's 500 runs, 0.0 percent on Surround 5 (the
  `stated` bench's statement 16).
- **Role tags per run**: the number of role tags standing at the end of a run, by role; 0 in
  v1 by definition. Also the tags proposed, broken and replaced during the run (the roles
  loop's answer D1: tags compete and change).
- **Time**: the median codelets per run (v1: 52 to 60 per family, RESULTS.md §1 M6) and how
  many role codelets run before the answer.

**How a change is read.** A frequency has moved when a two-sample z between the roles' survey
and v1's, at the same size, is beyond 2; gained and lost are reported separately, never as a
net number. "Up by at least k points" means the roles' share minus v1's is at least k and the
z is beyond 2. "Within noise" means the z is within 2. Sizes are rough: a prediction of "up by
at least 20" that comes in at 15 is a miss on size and a hit on direction, and is scored as
both.

**Carriers.** Each engine item measures its own carriers at 200 runs against the previous
commit, and the per-item changes are logged in RESULTS.md's Re-interpretation log. The
predictions here are scored at item 13 at 500 runs, against v1.

## 2. Per role

Family, reading (on one table, a comparison of the two sides, or off the Worldview), the result
sets a role could move, the direction and the rough size, and the mechanism measure that should
move with it. "Weight" is the number of objects an item carrying the role holds, after French's
remark that single-object sidekicks are too weak to decide a Buridan choice [F95 p. 131].

### The geometric family

| Role | Reading | Result sets | Direction and size | Mechanism |
|---|---|---|---|---|
| G1 MIRROR_IMAGE (directly opposite: the same column, the other row) | comparison | Surround 5-7, 10 (E1); Surround 1-4 (E1, against French); Blockage 1-5 (the glasses) | E1 up on Surround 5-7 and 10, by 5 to 15 points from this role alone (the rest of the E1 gain is the content family's); E1 up on Surround 1-4 by under 10; the glasses on Blockage 1-5 up by under 5 | placement slippages labelled "mirror symmetry" on top-level correspondences: present in most runs with a Worldview entry (v1: none, the table frame is never read, D-029) |
| G2 POINT_IMAGE (diagonally opposite: the reflection through the centre) | comparison | Surround 1-4 (E2); Blockage 2-8 (the glass pairs) | E2 up on Surround 1-4 by 5 to 15 points; the glass-to-glass correspondences on Blockage 2-8 gain placement points, which feeds S5 | placement slippages labelled "opposite" and "diagonal symmetry" on top-level pairs |
| G3 SYMMETRY_AXIS (the placement most of the Worldview's top-level correspondences share, the touched object's own left out) | Worldview | Surround 5-7, 10 (E1 strongly, once the row-to-row correspondence is mirror); Blockage 6-8 (E3); Figure/Ground (none: the middle column) | E1 up on Surround 5-7 and 10; E3 up on Blockage 6-8; no change on Surround 1-4 (one correspondence, no axis) | an axis is read in a majority of runs on Surround 5 and Blockage 6, in few on Surround 1-4 and Figure/Ground |

French's region order is the point image first, then the mirror image [F95 pp. 66, 93]; his
subjects' is the same [F95 p. 115]. The symmetry family is the most fit-prone of the five,
because French's E1 sits at the touched cup's mirror image and E2 at its point image on every
Surround set (measured on the digitised layouts of `problems/`: E1 is 8 to 14 table units from
the mirror image, E2 7 to 13 from the point image). The guards are the
axis rule (the touched object's correspondences never vote) and the decoupled variant (§6).

### The structural family

| Role | Reading | Result sets | Direction and size | Mechanism |
|---|---|---|---|---|
| S1 MIDLINE (the middle band of regions holds no member of a gestalt group) | one table | indirect: it opens C6 on every set; on Surround 10 the junk is in the band | no direct move; placement points exist only across a perceived midline | a midline is perceived in nearly every run on the 35 sets; in fewer on random tables with objects in the band |
| S2 GAP (empty space splitting a side beyond D-022's zero gap) | one table | Surround 5-10 (E1, through part-to-part pairing); Blockage 2-8 (E3, the glass proposals come earlier) | E1 up on Surround 5-10 by under 10 from this role; Blockage 2-8 E3 up by under 5 | a gap is tagged on Eliza's side of Surround 5-10 in most runs |
| S3 CONTAINER (holder: a footprint containing another object's centre; setting: a linear group with one anchor and members on both sides) | one table | setting: Surround 5-10 (E1, through C3); holder: none of the 35 sets (held out, §8) | E1 up on Surround 5-7 and 10 by 5 to 15 from the setting form; nothing from the holder on the 35 sets | both rows of Surround 5 tagged as settings in most runs |
| S4 CONNECTOR (a member whose two neighbours along the axis are of different types, each with its own type beyond it) | one table | Surround 5-7, 10 (E1); Surround 8-9 (E1 slightly up, against French's E2 at 90 and 98) | E1 up on Surround 5-7 and 10; on Surround 8-9 E1 up by under 10 points (the type term should reject plate-to-cup and saltshaker-to-cup) | connector tags on the touched cup and the glass in Surround 5 in most runs |
| S5 BARRIER (the Henry-side end of a standing Worldview correspondence whose other end is the touched object's obvious counterpart) | Worldview | Blockage 2, 6-8 (the touched cup) | E3 up on Blockage 6-8 by 10 to 25 points; Blockage 2's touched cup up by 5 to 15 (away from French's 88/12 on that set; a rise beyond 25 would say the barrier is too strong) | a barrier is tagged in a third or more of the runs on Blockage 6 (the glass pair is grouped in 39 percent of v1 runs, the `stated` bench's statement 19) |
| S6 CLAIMED (the obvious counterpart already at the end of someone else's correspondence) | Worldview | Blockage 6-8 (E3 up); Blockage 3-5 (E1+E2 slightly down, against French's 58-100) | Blockage 3-5 E1+E2 down by under 10 points; an `AnswerGiven.through` of `blocked` appears in 10 to 40 percent of Blockage 6-8 runs | a claimed tag coincides with a barrier tag (one implies the other) |

French: the Blockage family is built so that "A specific pressure, not explicitly anticipated,
causes a particularly natural answer to be blocked" [F95 p. 114], and in Blockage 6 the
glass-to-glass correspondence is "strong enough to make Eliza very reluctant to break it by
touching one of the glasses on her side of the table" [F95 p. 127]. v1 never blocks
(RESULTS.md §8.4): the touched cup is given only by the early default answer.

### The selector family

| Role | Reading | Result sets | Direction and size | Mechanism |
|---|---|---|---|---|
| L1 ROW (Henry's row band or Eliza's) | one table | the `other` share on every family (RESULTS.md §8.1); Surround and Buridan E1 and E2 | `other` down by 10 or more on Surround 5 and Buridan 1, partly from this role: the re-cast looks in Eliza's row first, so Henry-side answers (the cups of Buridan 1, the touched cup's neighbours) fall | row tags on every item in a perceived row in nearly every run |
| L2 COLUMN (left, middle, right) | one table and relational | Blockage 1-5 (E1, "in front of"); Surround 1-4 (E1 up, against French; G2 and L3 pull the other way) | Blockage 1-5 E1 within noise or up by under 5; Surround 1-4 E1 up by under 10 | column tags on every item; the re-cast's second tier is non-empty in most runs |
| L3 CORNER (an item in one of the four corner regions) | one table | Blockage 2 (the glass pair proposed without the touched object); Blockage 3-8 (the barrier forms earlier, E3); Surround 1-4 (cup-to-cup proposals, E2 up toward French) | Surround 1-4 E2 up by 5 to 10 from this role; Blockage 3-8 E3 up (counted with S5) | corner-to-corner correspondences proposed in most Blockage 2-8 runs |
| L4 LONER (an object in no gestalt group, set off by a gap or alone on its side) | one table | Surround 5-10 (E1: the touched cup is a member of a row, the isolated object a loner, so the loner is a poorer counterpart; on Surround 6 the isolated object is the glass and the cup in the row is E1, so every role points the same way there [F95 p. 119]); Surround 1 (E2 toward French's 95: both cups are loners); Blockage 2 (E2) | Surround 1 E2 up by 5 to 15; Surround 5-10 E2 (the isolated object) down by 5 to 15 | loner tags on Surround 5's isolated cup in nearly every run (v1 marks it isolated in the Workspace already, D-007, D-022) |
| L5 STRAY (an ungrouped object in the midline band) | one table | Surround 10 (toward Surround 5 in its answers and its run length); the `focus` bench's junk scaling | Surround 10's E1 within 5 points of Surround 5's E1; the junk scaling's cost in codelets per two junk objects down from v1's 2 to at most 1; the main answer's fall over twelve junk objects (v1: 36.6 to 26.4) halved | stray tags on Surround 10's junk in nearly every run; no role pair is ever proposed from a stray |

French on the junk: "the junk should not make a significant difference" [F95 p. 121], and
adding extraneous objects "does not diminish the program's ability to home in on an answer"
[F95 p. 142]. His program gave the glass 13 points less often with the junk; v1 gives it 4
less (RESULTS.md §5.4).

### The key family

| Role | Reading | Result sets | Direction and size | Mechanism |
|---|---|---|---|---|
| K1 COUNTERPART (the touched object's signature re-cast onto the other row in four tiers: the image under the axis with the same signature; the same signature in the same column; the same type; anything else in the row; claimed candidates last) | comparison | the `other` share (RESULTS.md §8.1); Surround 5-10 (E1); Buridan (the ties are kept) | `other` down by 10 or more on Surround 5 and Buridan 1; E1 up on Surround 5-10; on Buridan 1 the spoon and the fork tie in tier 3 (the same signature, different types) and the re-cast publishes both with equal weights, so neither gains from K1 alone | a re-cast is published in nearly every run; its tier-1 set is non-empty on Surround 5-10 in most runs and empty on Buridan 1 (no axis) |
| K2 TYPE_KEY (a type slippage a Worldview group correspondence has made, read as a substitution for other items of the two types) | Worldview | Figure/Ground 1 (E3, Henry's glass, up from v1's 8.4) | Figure/Ground E3 up by 10 or more; this is away from French's program (0 percent, "the program never selected Henry's glass" [F95 p. 140]) and toward his account of the figure object (§7) | a key is read in a quarter or more of Figure/Ground runs, in few elsewhere |

French's schema for "the cup in front of Henry" and its transport to Eliza's side, where under
pressure "it slips to the close notion 'glass in front of Eliza.'" [F95 p. 6], is the re-cast:
the one place in the engine where a role is carried across the table. "X is the counterpart
of Y" [F95 p. xv] names the result.

### The content family

| Role | Reading | Result sets | Direction and size | Mechanism |
|---|---|---|---|---|
| A1 MEMBER (an object in a gestalt group, with its group-frame words) | one table | Surround 5-10 (E1, through C3 when the touched cup is mapped only through its row) | E1 up on Surround 5-7 and 10 by 5 to 15 points from C3 (v1 answers literally in that case, D-058) | the "mapped only through its group" case occurs in a tenth or more of Surround 5 runs once echo and setting pairs exist (v1: it needs a group correspondence, 0.0 percent on Surround 5) |
| A2 SIDEKICK (an immediate neighbour of a different type; a run of identical neighbours is one sidekick of weight n) | one table | Buridan 1-13 (E2, the fork, through the coherent cup-to-cup beside spoon-to-fork); Buridan 2 less so (weight 1) | Buridan 1 (modified) E2 up by 10 or more (from 16.4), E1 within 10 points of E2 at the end; Buridan 2 E2 within noise; the `buridan` scan no longer flat | coherent pairs per run above 0 on Buridan 1 in a quarter or more of runs (v1: 0 in every run) |
| A3 ANCHOR (a member with members on both sides whose nearest category is unique in its group) | one table | Surround 5-7, 10 (E1); Surround 8-9 (the plate and the saltshaker are anchors too) | E1 up on Surround 5-7 and 10; on Surround 8-9, E1 up by under 10 points: the Worldview should reject anchor-to-anchor there on type, since "a saltshaker and a cup are conceptually very dissimilar" [F95 p. 32] | anchor tags on the touched cup and on the glass of Surround 5 in most runs |
| A4 FIGURE (the only object of its type on its side where a ground stands) | one table | Figure/Ground 1 (E3 up, with K2) | counted with K2 | a figure tag on the touched fork and on Henry's glass in most Figure/Ground runs |
| A5 GROUND (two or more objects of one type in one gestalt group on a side) | one table | Figure/Ground 1 (E3) | counted with K2; the ground-to-ground group correspondence (the one French hoped for) enters the Worldview in a fifth or more of runs | group correspondences in the Worldview up from 0.0 |
| A6 ECHO (a group on each side with the same multiset of types, or of nearest categories; or an object echoing a group's type) | comparison | Surround 5-7, 10 (E1); Blockage 6-8 (E3: the glass pairs echo, which feeds S5); Buridan (the cup pairs echo, with A2) | E1 up on Surround 5-7 and 10; the row-to-row correspondence in the Worldview in a quarter or more of Surround 5 runs (v1: 0.0, the `stated` bench's statement 16) | group correspondences in the Worldview; Worldview size up |
| A7 TWIN (an object of the touched object's type on the other side) | comparison | the human tables (M8, toward the subjects); Surround 1-4 (E2 slightly up) | Surround 1-4 E2 up by under 5 from this role; M8 moves by under 0.05 in Spearman from this role | the re-cast's third tier non-empty wherever a same-type object stands across |

French's names for these roles are his own: the "sidekick" objects [F95 p. 130], the "figure
object" [F95 p. 140], the "loner" [F95 p. 118], and "B plays 'the same' role as A plays"
[F95 p. 12] for the counterpart. v1 has the ingredients (neighbours, groups, the isolated
mark, the same-multiset factor) and no reader that names the part an object plays (the
audit, `docs/roles-audit.md`).

## 3. Per consumer

| Consumer | What it does | Result sets | Direction and size | Mechanism |
|---|---|---|---|---|
| C1, role-guided proposals (`propose-role-cp`) | proposes the pairs a role relation names, across the midline (sidekick with sidekick, echo with echo, anchor with anchor, connector with connector, setting with setting, loner with loner, corner with corner, figure with figure, ground with ground, the pairs a type key implies, each unmapped item with its image under the axis), into the existing create-cp chain | every family: the correspondences that do not contain the touched object, French's ones that "come to play essential roles" [F95 p. 36] | the largest single cause of every gain above; with C1 lesioned (§5) the Worldview size falls back toward v1's 2 percent | Worldview size from about 2 percent to a majority on Surround 5, Buridan 1 and Blockage 6; group correspondences from 0 to a quarter or more of Surround 5 runs |
| C2, the role-weighted partner choice (`pick-salient-obj` weighted by salience times one plus the re-cast weight) | the object paired with the touched object is chosen with the re-cast's weights | the `other` share on every family | `other` down by 10 or more on Surround 5 and Buridan 1, by 5 or more on every Surround set; the literal share (v1: 11 to 17 on Surround 5) unchanged (C2 does not touch find-answer) | the pair proposed by pick-salient-obj is a tier-1 or tier-2 candidate in most runs |
| C3, the answer from group position (`role_member`) | when the touched object is mapped only through its group, the member of the corresponding group with the same roles answers (anchor for anchor, connector for connector, end for end, middle for middle), then D-058's group-frame words, then the literal object | Surround 5-7, 10 (E1) | E1 up by 5 to 15 from C3 alone; the "words" value of `group_position_match` gives the same answers within noise on the 35 sets (the anchor and the end coincide there) and differs on random tables | the `through` of the answer reads `group` with a role note in a tenth or more of Surround 5 runs |
| C4, the blocked counterpart (`pick-strong-cp` for the barrier's correspondence; the `blocked` record) | keeps the barrier's correspondence fighting; records the default answer as blocked when the obvious counterpart is claimed | Blockage 2, 6-8 | E3 up on Blockage 6-8 by 10 to 25 (with S5, S6); the touched cup's mean structure value on Blockage 6 up from v1's 8.2 (a Worldview stands when it is given) | `blocked` in 10 to 40 percent of Blockage 6-8 runs |
| C5, role slippages (a tenth static factor, `role_points`, a priori 10 times the role node's activation over 150) | a correspondence whose ends share a role gains a role slippage; a mismatch is a slip with no points | Buridan (member-member, sidekick-sidekick beside the cups); Surround 5 (anchor-anchor, connector-connector) | small on its own (under 5 points on any set); its effect is through parallelism and the ratings in fights | parallel pairs per run above 0 (v1: 0); role slippages jolt their nodes on entry |
| C6, placement slippages and the axis | top-level correspondences across a perceived midline carry a placement slippage in the table frame (column identity: mirror; column flip: point; the middle column: symmetry), scored by factor 4 with the existing position weight and the symmetry scale; the axis's node is jolted | Surround 5-7, 10 (E1); Surround 1-4 (E1 and E2 both up: the sets where the touched cup's partner is the image); Blockage 2-8 | the symmetry nodes active (above 100) in most runs with a Worldview entry (v1: "mirror symmetry" is fed only from inside groups, so it is seldom active) | placement slippages on most top-level correspondences |
| C7, role salience (a "role" term, a priori 5 per unit of weight, for ANCHOR, CONNECTOR and FIGURE; the stray scale 0.5 on a stray's region term) | the glass of Surround 5 and the touched cup gain salience; the junk of Surround 10 loses it | Surround 5-10 (E1); Surround 10 and the `focus` bench | Surround 10's E1 within 5 of Surround 5's; the junk's cost halved (§2, L5) | the glass is the most salient object on Eliza's side in most Surround 5 runs |
| C8, urgencies and top-down posting (role nodes post `propose-role-cp` and `recast-role` at generation 0; an item whose roles are discarded posts nothing) | the consumers reach the rack ahead of the find-answer flood | time: every family | median codelets per run up by 10 to 40 percent (v1: 52 to 60), still within a factor of 2.5 of French's 50-200; no capped run under `standard`; 5 to 15 role codelets run before the answer on Surround 5 | role codelets in the trace before the answer in nearly every run |

Two rules hold for every consumer and are unit tests: **nothing is gated** (a consumer with an
empty pool posts nothing and changes no rating), and **the Worldview decides** (a role-proposed
correspondence is tested, fought and faded like any other, with no bonus for its origin; the
only points it gets are C5's, which any correspondence with those ends would get).

## 4. The twelve predictions, refined

The twelve registered in the plan, each with the roles that carry it, the measure, the
threshold and v1's value.

1. **Worldview size.** On Surround 5, Buridan 1 and Blockage 6 the share of runs whose final
   Worldview holds two or more correspondences rises from about 2 percent to a majority (50
   percent or more at 500 runs). Carried by C1 with A2, A6, L3, A3, S4. The refined claim per
   problem: Surround 5 through echo and setting pairs beside the cup's own; Buridan 1 through
   sidekick pairs (cup to cup beside spoon to spoon or spoon to fork); Blockage 6 through the
   corner glass pair beside the cup's.
2. **The `buridan` scan is no longer flat.** On Buridan 1 (modified), at least one value of
   the D-076 grid (40, 60, 80, 100, 150, 200, 300) gives E1 or E2 at a z beyond 2 from the
   value at 40, and the imbalance between E1 and E2 at 40 is under v1's 15.4 points. Carried by
   A2 and C1 (coherent and incoherent pairs exist), read by C5 and D-042. The penalty is not
   re-fitted in the roles loop; the `buridan` profile keeps 40.
3. **E1 up by at least 20 on each of Surround 5, 6, 7 and 10** (v1: 17.6, 26.8, 15.2, 13.6).
   Carried by G1 and G3 with C6 (5 to 15 of it), A3, S4 and A6 with C1 and C5, S3 and A1 with C3
   (5 to 15), L4 and K1 with C2. The refined order of arrival, by item: the geometric family
   first (item 04: a gain of 5 to 15), then anchors and connectors (item 05), the setting and
   C3 (item 06), the re-cast and C2 (item 07). If the E1 gain comes entirely at item 04 and
   nothing is added by the content family, the symmetry consumers are doing the work alone,
   and prediction 12 decides whether that is geometry fitting.
4. **E2 up by at least 10 on Surround 1-4** (v1: 50.2, 45.8, 40.4, 41.6). Carried by G2 with
   C6, L3 with C1, L4, K1 with C2 and A7; L2 pulls the other way (the mirror image in front of
   Eliza is E1 there). Surround 1 moves most (both cups are loners: L4).
5. **E3 up by at least 15 on Blockage 6, 7 and 8** (v1: 7.6, 8.2, 8.2). Carried by A6 and L3
   with C1 (the glass pair is proposed without the touched object), G2 and G3 with C6 (its
   placement points), S5 and S6 with C4. The mean structure value of the E3 answers rises from
   v1's 8.2, 9.1 and 3.5 to above 30 (a Worldview stands when the cup is given, as in French's
   177 at 94 codelets on Blockage 6). The glasses (E1+E2) fall by the same amount; their fall
   on Blockage 3-5 is under 10.
6. **Figure/Ground E3 rises** above v1's 8.4 by at least 10 points, and the E3 answers' mean
   structure value rises above v1's 52.7. Carried by A4, A5 and K2 with C1. This is away from
   French's program and toward his account (§7).
7. **The `other` share falls by at least 10 on Surround 5 and Buridan 1** (v1: 45.8, 51.8).
   Carried by L1 and K1 with C2, and by A2 (Buridan 1's cups, which are answers French prints
   no bar for, become sidekick pairs beside the spoon's or the fork's correspondence instead
   of the touched spoon's partner). On every Surround set `other` falls by 5 or more.
8. **M4 stays at or below about 12 of 35** result sets within 15 points of French's main
   answer (v1: 6). Roles move several sets toward French and some away (Surround 1-4's E1,
   Surround 8-9's E1, Blockage 2 and 3-5's glasses); the count is not expected to double.
9. **The temperature course does not improve.** The end temperature stays within v1's 55-68
   (RESULTS.md §6.4), the first term of the formula stays the whole of it, and M6's medians
   lengthen by 10 to 40 percent, still within a factor of 2.5 of French's 50-200 on every
   family. More structure means more Worldview changes, each of which raises T (D-048).
10. **M8 moves away from the subjects**: Spearman with the subjects' percentages under
    `standard` at or below v1's +0.39 and under `human` at or below +0.45; the problems within
    ten points stay at 2 or fewer of 23. A7 alone pulls their way, by under 0.05.
11. **Exact invariance holds for v1 and for roles alike.** On the mirrored, side-swapped and
    rotated variants of every family, the total-variation distance between the relabelled
    answer distributions lies within the bootstrap interval of no change, for both engines
    (§6). A failure on v1 is a finding about a v1 decision, written up at item 02, not fixed.
12. **The decoupled Surround variants keep at least half of the E1 gain.** Let g be the
    roles' E1 minus v1's E1 on the original Surround 5, 6, 7 and 10, and g' the same on the
    decoupled variant (Eliza's row moved one column sideways, so that E1 is no longer the
    touched cup's mirror image while the rows' composition is kept). The prediction is
    g' at or above g / 2 on each of the four. If it fails, the symmetry consumers were doing
    the work rather than the structure, and that is reported as a finding about fitting.

Further registered claims, numbered on:

13. **Roles change during a run** (the roles loop's answer D1). On each of the four carriers
    (Surround 5, Blockage 6, Buridan 1, Blockage 2), a role tag is broken or replaced in 20 to
    70 percent of runs; the last role change comes before the last Worldview change in most of
    those runs; and the churn settles as the temperature falls (fewer changes per ten codelets
    in the second half of a run than in the first). A carrier on which no role ever changes is
    a finding to report, not a pass.
14. **The oracle probe** (hand-read roles for the four carriers, item 03; perfect perception
    against perceived roles, item 13): the gap between hand-read and perceived role sets is
    under a fifth of the tags on each carrier (the detectors read descriptions the engine
    already makes, so the cost of perception is the blur's and the proximity coin's), and the
    answer distributions under the two differ by a total-variation distance under 0.15.
15. **Tags per run.** At the end of a run, 8 to 25 role tags stand on Surround 5 (13 objects,
    two rows, a loner, a glass), 3 to 10 on Blockage 1-2 (four or five objects), 10 to 30 on
    Buridan 1. On 500 random tables, every run ends, no invariant fails, and a re-cast tie (two
    or more candidates in the first non-empty tier) occurs in 10 to 40 percent of runs.
16. **The faults' fixes stay inert** on Surround 1-4 and Blockage 1-2, where no subgroup,
    group-to-object mapping or subworld is exercised, and begin to show on Surround 5-10 and
    Blockage 6-8 (the group correspondences now exist): under `french-faults`, Surround 5's
    E1 differs from `standard`'s at a z beyond 2 on at least one of those sets.

## 5. The lesion expectations

Lesions are test-only probes (`tests/role_probes.py`, `tools/role_probes.py`): one
module-level function patched per consumer with `unittest.mock.patch`, no user-facing switch.
Each expectation names the measure that should degrade, so that "no change" is a verdict and
not an excuse. A consumer that cannot be isolated by one patch loses its lesion, and RESULTS.md
§11 says what attribution is lost.

| Lesion (the function patched) | Expected degradation | Measure | Hit if |
|---|---|---|---|
| C1 (`propose-role-cp` fizzles) | the Worldview is thin again: the pairs that do not contain the touched object are never proposed | Worldview size on Surround 5, Buridan 1, Blockage 6 | back below 10 percent (from a majority); E1 on Surround 5-7, 10 loses at least half its gain |
| C6 (placement slippages return nothing) | the symmetry nodes are fed only from inside groups again; no axis | E1 on Surround 5-7, 10; E2 on Surround 1-4 | the Surround E1 gain loses its geometric part (5 to 15 points); Surround 1-4's E2 gain lost by at least half |
| S5 and C4 (the barrier reading returns nothing) | the glass pair stands but nothing keeps it fighting and nothing records a block | E3 on Blockage 6-8 | E3's gain lost by at least half; `blocked` never recorded |
| A2 (the sidekick detector returns nothing) | no coherent pair on Buridan | coherent pairs per run on Buridan 1; E2 | coherent pairs back to 0; Buridan 1's E2 gain lost; the `buridan` scan flat again |
| C2 (the partner choice unweighted) | pick-salient-obj back to salience alone | the `other` share on Surround 5 and Buridan 1 | `other` back up by at least half of its fall |
| C3 (`role_member` returns None) | the literal object when the touched cup is mapped only through its row | E1 on Surround 5-7, 10; the literal share | E1 loses 5 to 15; the literal share rises by the same |
| C5 (`role_points` returns 0) | no role slippages, no role parallelism | parallel pairs per run; the fights' outcomes | parallel pairs back toward 0; answers within noise (C5 is small on its own) |
| C7 (the role term returns 0 and the stray scale is 1) | the glass loses its salience bonus; the junk is salient again | Surround 10 against Surround 5; the `focus` bench | Surround 10's E1 falls below Surround 5's by 5 or more again; the junk's cost per two objects back toward 2 codelets |
| K1 (the re-cast returns an empty result) | C2 unweighted and C1's tier-1 pairs absent | `other`; E1 on Surround 5-10 | as C2's lesion, plus E1 loses 5 to 10 |
| L2 (the column detector returns nothing) | the re-cast's second tier empty; placements read from the images only | Blockage 1-5's E1; the four carriers | within noise on the four carriers (the merge question, §8); Blockage 1-5's E1 down by under 5 |

**The existing seven lesions** (RESULTS.md §6.2) are re-run. `losers-destroyed` and
`fixed-lengths` were inert in v1 because nothing coexisted in the Worldview and the answer was
settled before the Slipnet's distances mattered; with two or more correspondences per run and
slippages that feed the symmetry nodes, both should now show an effect: `losers-destroyed`
fewer Worldview changes after the first fight on Surround 5 and Blockage 6; `fixed-lengths`
fewer opposite-side correspondences (placement flips) on Surround 1-4. `flat-depths` may stay
inert. The four that were as predicted stay as predicted.

## 6. The variants expectations

The variants bench (item 02, built on v1 before any role code) runs eight variants of every
family. The headline is **"lost nothing"**: on no variant family are the roles less invariant
than v1 (the roles' total-variation distance is not above v1's by more than two standard
errors of the bootstrap). The per-variant expectations:

| Variant | What it is | v1 expected | Roles expected |
|---|---|---|---|
| mirrored | left-right reflection, x to 110 minus x | exact invariance within noise (every geometric reading is symmetric; the regions are) | the same; a difference would be a bug in a detector's handedness and is fixed as one |
| side-swapped | y to 100 minus y, the touched object moving with the rest | exact invariance within noise | the same (the rows swap names, nothing else) |
| rotated | both reflections | exact invariance within noise | the same |
| translated | a shift of up to half a glass width | near-invariant: the blur (D-004) moves a few region draws | the same; role tags that read regions (corner, row, column, stray) change in the few runs where the region draw changes |
| type-substituted | a consistent substitution within D-009's categories (small glass to big glass, fork to spoon), on every object of a type | the answer's role kept in most runs (the type term reads distances, which change little within a category) | the share of runs whose answer keeps its role at or above v1's; the roles read types only through the nearest category (anchor, echo by category) or exact sameness (twin, ground), so a consistent substitution leaves every tag in place |
| junk-added | a stray in the midline band or a far corner | little change, as French reports [F95 pp. 121, 142] and v1 shows (RESULTS.md §5.4: 4 points) | less change than v1: the stray's salience is scaled (C7) and no role pair is proposed from it |
| decoupled | Eliza's structured row moved one column sideways, so that E1 is no longer the touched cup's mirror image while the rows' composition is kept | E1 within noise of the original (v1 reads the region preference, which the move changes little for a row) | prediction 12: at least half the E1 gain kept on Surround 5, 6, 7 and 10; the axis is read from the row-to-row and anchor-to-anchor correspondences, which the move does not break |
| on-saucer | a cup stood on a saucer where the table has a cup and space | the saucer is one more object; the answer distribution moves by a total-variation distance under 0.15 | held out (§8): the holder is tagged, the pair held together by the inside-only rule (D-044), the saucer itself never the answer in more than 5 percent of runs |

**A failure of exact invariance on v1** (mirrored, side-swapped, rotated) at item 02 is a
finding about a v1 decision (a handed tie-break, an ordering by id), traced and written up in
RESULTS.md §11, not fixed in that item. The prediction is that there is none: v1's geometry
reads regions by thirds and reflections, and its ties fall to member order, which the variants
relabel.

## 7. The two readings: away from French's program, toward his account

Three roles are built to reach answers French wanted and his program did not give, or that his
program gave and his theory does not favour. Both readings are reported for each, "closer to
his account, further from his numbers", and neither is called a win on its own.

- **TYPE_KEY, FIGURE and GROUND on Figure/Ground 1.** French's account: Henry touches the
  "figure" object on his side that echoes the "background material" across the table, and
  "there is a kind of temporary conceptual symmetry, in which just during this problem, forks
  and glasses are seen as 'opposites'" [F95 p. 140]; his program never selected Henry's glass.
  Prediction 6 says E3 rises. The two readings: against his numbers, a loss of E1+E2 by the
  same amount (from v1's 81.8, itself below his 100); for his account, the lone glass given
  through a Worldview in which forks and glasses are opposites, with a structure value above
  v1's 52.7. The honest score counts both.
- **ANCHOR and CONNECTOR on Surround 8 and 9.** The plate and the saltshaker are anchors of
  their rows as the touched cup is of its own, so anchor-to-anchor is proposed. French's
  numbers: E2 at 90 and 98. The prediction is that the Worldview rejects the plate-to-cup and
  saltshaker-to-cup correspondences on type, and E1 rises by under 10 points on each set (v1:
  16.6 and 4.2). A rise of 10 or more is a miss against his numbers, and a finding about the
  type term's weight (D-036), not about the role. For his account, French himself names the
  type dissimilarity as what should reject the mapping [F95 p. 32].
- **Blockage 2's touched cup (S5, S6).** French's program: the glass at 88, the touched cup at
  12. The barrier reading makes the corner glass pair claim the natural counterpart earlier
  and more often than his weak long-distance structure did [F95 p. 123]. The prediction (§2,
  S5) is a rise of 5 to 15 in the touched cup, away from his numbers on this set and in the
  direction of his account of the family, where the same structure at a smaller distance
  blocks (Blockage 6). A rise beyond 25 says the barrier is too strong.

## 8. Held-out questions

Two of the roles loop's open questions have no carrier among the 35 result sets, or overlap
another role; they are registered here as held-out predictions, measured only where the result
sets cannot have shaped the default.

- **COLUMN against MIRROR_IMAGE (question 2).** "In front of" (the column, read at the level
  of regions) and "directly opposite" (the mirror image, read at the level of points) name
  nearly the same place. Both are built. The prediction: on the four carriers (Surround 5,
  Blockage 6, Buridan 1, Blockage 2) the lesion of COLUMN changes no measure beyond two
  standard errors, and on Blockage 1-5, where the glass "in front of Eliza" is E1, it changes
  E1 by under 5 points. That is, COLUMN is expected to add nothing beyond MIRROR_IMAGE on the
  carriers, and the merge rule (a decision and a log entry) is expected to apply. A difference
  beyond two standard errors on any carrier keeps both roles and is a finding about where the
  region reading and the point reading part.
- **STRAY and the junk (L5, C7).** No open question, but a held-out family of tables: the
  `focus` bench's scaled junk (0 to 12 objects) and the junk-added variant. Registered in §2
  (L5) and §6: the cost per two junk objects at most 1 codelet (v1: 2), the main answer's fall
  over twelve objects halved, and no role pair ever proposed from a stray.
- **The cup on its saucer (question 3).** None of the 35 sets has a cup standing on a saucer,
  so the holder form of CONTAINER can move nothing in chapter 5. It is built anyway and
  measured on the on-saucer variant and on random tables only. The predictions: the holder is
  tagged in 90 percent or more of the runs on the on-saucer variant of Surround 5 (the
  footprint test is deterministic, D-002); the cup and its saucer are kept together by the
  inside-only rule; the saucer is the answer in at most 5 percent of runs; the answer
  distribution differs from Surround 5's by a total-variation distance under 0.15; and on
  random tables with a cup on a saucer, holder-to-holder pairs are proposed when both sides
  have one. French's only support is the remark that cups and saucers "are used together"
  [F95 p. 27] and the tables set with plates, cups and saucers [F95 p. 38]; no figure has
  one, so the default's values cannot have been shaped by a result set.

## 9. What would count as fitting

The predictions are the first guard; these are the signs that would say roles were fitted to
chapter 5 even if every prediction above were a hit, and item 13 reports on each:
- a default moved after a measurement without a logged decision and a re-check on the held-out
  families and the variants bench (the lock and the log make this visible);
- prediction 12 failing while prediction 3 holds (the geometry doing the work);
- a loss of invariance on any variant family against v1 ("lost nothing" failing);
- the oracle gap (prediction 14) closing only on the carriers and not on random tables;
- a role whose lesion changes nothing on any carrier or random table, kept without the merge
  question being asked.

## Scored

Scored at item 13 of the roles loop, on 2026-10-14, against the benches of item 12 (500 runs
per result set under `standard`, bench seed 1, `results/`; v1 from `results/v1/`) and the
probes of item 13 (`results/role-probes.json`: 200 runs per carrier and lesion, the same
seeds for every batch of a problem). Everything above the `## Scored` heading is as it was
registered; what follows reads it number by number, hits and misses alike. A "z" is the
two-sample z of the roles against v1 (n = 500 each) unless the line says otherwise; the
probes' z is at n = 200. The roles engine at this item lacks CONTAINER and C3 (item 06 is
blocked), so every row that depends on them is scored "not built", not as a miss or a hit.
RESULTS.md §11.2 summarises this section and §11.8 traces the moves to decisions.

### S1. The sixteen numbered predictions (§4)

| # | Registered | Measured | Verdict |
|---|---|---|---|
| 1 | Worldview with two or more correspondences: from about 2 percent to a majority on Surround 5, Buridan 1, Blockage 6 | 8.4 → 19.0, 3.4 → 23.2, 1.0 → 4.4 percent of 500 (§11.5's mechanism, the originals) | **miss**: up on all three, nowhere near a majority |
| 2 | the `buridan` scan no longer flat (a grid value at z beyond 2 from 40); the imbalance at 40 under v1's 15.4 | E1 28.8 at 40 to 28.2 at 150-300, E2 18.6 to 18.8: no value beyond z 0.3; the imbalance at 40 is 10.2 | **miss** on the first part (the scan moves, but within noise); **hit** on the second, by both answers moving within noise (E1 31.8 → 28.8, E2 16.4 → 18.6) |
| 3 | E1 up by at least 20 on Surround 5, 6, 7, 10 | 17.6 → 14.6 (z -1.3), 26.8 → 28.8 (+0.7), 15.2 → 15.2 (0.0), 13.6 → 17.0 (+1.5) | **miss** on all four, direction and size |
| 4 | E2 up by at least 10 on Surround 1-4; Surround 1 moves most | +6.6 (z +2.1), +5.2 (+1.6), +11.2 (+3.6), +7.6 (+2.4) | **hit** on direction (three of four beyond z 2), **miss** on size on three of four; Surround 3 moved most, not Surround 1 |
| 5 | E3 up by at least 15 on Blockage 6, 7, 8; its mean structure value above 30; the glasses fall by the same on 6-8 and by under 10 on 3-5 | E3 7.6 → 9.2 (z +0.9), 8.2 → 11.2 (+1.6), 8.2 → 7.8 (-0.2); structure 8.2 → 42.7, 9.1 → 70.8, 3.5 → 26.8; the glasses on Blockage 3-5 rose (85.2 → 85.4, 81.4 → 85.0, 78.8 → 83.2) | **miss** on the frequency; **hit** on the structure value on two of three (a Worldview stands when the cup is given); the glasses did not fall at all |
| 6 | Figure/Ground E3 up by at least 10 from 8.4; its structure above 52.7 | 8.4 → 11.0 (z +1.4); structure 52.7 → 68.1 | **miss** on the frequency, **hit** on the structure |
| 7 | `other` down by at least 10 on Surround 5 and Buridan 1; by 5 or more on every Surround set | Surround 5 45.8 → 35.8 (z -3.2); Buridan 1 51.8 → 52.6 (+0.3); Surround 2-10 down by 7 to 15 (every z beyond 2), Surround 1 by 3.6 (z -1.6) | **hit** on Surround 5 (exactly 10), **miss** on Buridan 1; **hit** on nine of the ten Surround sets |
| 8 | M4 at or below about 12 of 35 | 6 of 35 (v1 6) | **hit** |
| 9 | the temperature course does not improve: end T within v1's 55-68, the first term the whole, M6's medians up 10 to 40 percent and within a factor of 2.5 of 50-200 | end T (mean) 54.9 → 48.4 (Surround 5), 68.0 → 57.0 (Blockage 2), 57.8 → 50.4 (Blockage 6), 55.7 → 51.4 (Buridan 1); the first term 58 to 65 of about 66; the medians 61 → 79, 52 → 66, 59 → 76, 53 → 70 (+27 to +32 percent), all within the factor | **miss** on the end T (it falls by 4 to 11 points: the course improved a little, the opposite of what was registered); **hit** on the term and on the run length |
| 10 | M8 away from the subjects: Spearman at or below +0.39 (`standard`) and +0.45 (`human`); within ten points at 2 or fewer of 23 | `standard` +0.45 (v1 +0.39), `human` +0.39 (v1 +0.45); 0 of 23 under both | **miss** under `standard` (it moved toward the subjects), **hit** under `human` and on the within-ten count |
| 11 | exact invariance for v1 and roles alike | p below 0.01 on 2 of 159 exact surveys for v1 (ch2-fig07 rotated, ch2-fig12 mirrored) and 2 of 159 for the roles (blockage-02 side-swapped, surround-08 mirrored), 1.6 expected by chance each; mean TV 0.042 to 0.049 (v1) and 0.041 to 0.044 (roles) | **hit** for both engines |
| 12 | the decoupled Surround variants keep at least half the E1 gain: g' at or above g / 2 on Surround 5, 6, 7, 10 | g (roles minus v1 on the originals) -3.0, +2.0, 0.0, +3.4; g' on the decoupled variant -4.2, -3.2, -3.2, +0.2; E1 on the decoupled variant 14.6 → 5.4 (roles) against 17.6 → 9.6 (v1) on Surround 5 | **miss** on all four; there was no E1 gain to keep, and on the decoupled table the roles read the mirror image as v1 does (§11.5) |
| 13 | roles change in 20 to 70 percent of runs on each carrier; the last change before the last Worldview change in most of those; the churn settles | 87, 60, 67, 66 percent (Surround 5, Blockage 2, Blockage 6, Buridan 1); 49 to 60 percent; changes per ten codelets higher in the second half on every carrier (§11.7) | **hit** on three carriers, **miss** on Surround 5; **miss** (half, not most); **miss** (it does not settle) |
| 14 | the oracle gap under a fifth of the tags on each carrier; the answers' total variation under 0.15 | of the hand-read tags, 36, 49, 31 and 39 percent are not standing at the end of a run (Surround 5, Blockage 6, Buridan 1, Blockage 2); 17, 8, 0 and 10 percent of the engine's tags of those roles are not hand-read; TV 0.115, 0.095, 0.065, 0.080 | **miss** on the tags (the gap is a third to a half, §11.3), **hit** on the answers |
| 15 | 8 to 25 tags standing on Surround 5, 3 to 10 on Blockage 1-2, 10 to 30 on Buridan 1; on random tables every run ends, no invariant fails, a re-cast tie in 10 to 40 percent | 52.8 (Surround 5), 9.5 and 10.3 (Blockage 1, 2), 33.0 (Buridan 1) at the end of a run; 500 random tables: every run ends, no invariant fails, a tie in 59 of 500 (12 percent) | **miss** on Surround 5 and Buridan 1 (twice the bound: the column and row tags on every object, the image tags, were not counted in), **hit** on Blockage 1, at the bound on Blockage 2; **hit** on the random tables |
| 16 | the faults' fixes inert on Surround 1-4 and Blockage 1-2; a z beyond 2 between `standard` and `french-faults` on at least one of Surround 5-10 and Blockage 6-8 | every E1 (and E3) within z 0.9 under both profiles on all fifteen sets (Surround 5: 14.6 against 14.2) | **hit** on the inert sets, **miss** on the rest: the fixes still move nothing (§6.3) |

Tally of the sixteen: 3 hits (8, 11, and 16's first half with 4's direction), 9 misses, 4 split
(2, 4, 5, 6, 9, 10, 13, 14, 15 each carry a hit and a miss in their parts; the table says which).

### S2. Per role (§2)

"Present" is the share of the 200 probe runs on a carrier in which at least one tag of the
role stands at the end (`roles_read.present`); "read" at an earlier item is from that item's
log entry.

| Role | Registered | Measured | Verdict |
|---|---|---|---|
| G1 MIRROR_IMAGE | E1 up 5 to 15 on Surround 5-7, 10; under 10 on Surround 1-4; the Blockage 1-5 glasses up by under 5; placement slippages on most top-level correspondences | E1 on Surround 5-7, 10: -3.0, +2.0, 0.0, +3.4; on Surround 1-4: -3.0, +3.8, -1.4, -0.2; Blockage 1-5's E1+E2: -0.8, +5.6, +0.2, +3.6, +4.4; an image tag stands in 86 to 93 percent of runs; the C6 lesion changes the draws of 60 to 77 of 200 runs on every carrier and no answer beyond z 2 | **miss** on every E1 rise; **hit** on the Blockage bound and on the mechanism's presence |
| G2 POINT_IMAGE | E2 up 5 to 15 on Surround 1-4 | +6.6, +5.2, +11.2, +7.6 (three beyond z 2) | **hit** |
| G3 SYMMETRY_AXIS | an axis read in a majority of runs on Surround 5 and Blockage 6 | standing at the end in 6.5 and 4.0 percent (1.0 on Blockage 2, none on Buridan 1) | **miss**: the axis needs a Worldview with two or more correspondences, which holds in 4 to 19 percent of runs |
| S1 MIDLINE | perceived in nearly every run on the 35 sets | standing in 96.5, 91.5, 92.5, 86.5 percent of the carriers' runs | **hit** (Blockage 2 at the margin: its three loners' clusters, D-084 as revised at item 08) |
| S2 GAP | E1 up by under 10 on Surround 5-10; a gap tagged on Eliza's side in most runs | a gap stands in 86 percent of Surround 5 runs, 76 of Buridan 1, 22 of the Blockage carriers; the S2 lesion moves E1 by +7, -5, +3, +7 of 200 on Surround 5, 6, 7, 10 (every z under 1) | **hit** on the presence; the E1 effect is within noise either way |
| S3 CONTAINER | the setting form: E1 up 5 to 15 on Surround 5-7, 10 through C3 | not built (item 06) | **not built** |
| S4 CONNECTOR | connector tags on the touched cup and the glass of Surround 5 in most runs | the glass's CONNECTOR is not standing at the end of any of the 200 runs (the chain of neighbours seldom reaches the glass before the answer, item 09's note); the touched cup is not one by the definition (the oracle file's note) | **miss** |
| S5 BARRIER | a barrier in a third or more of Blockage 6 runs; E3 up 10 to 25 on Blockage 6-8; Blockage 2's touched cup up 5 to 15 | a barrier stands in 0 to 1.5 percent of runs on the four carriers (read in 0 to 3 of 200 at item 10); E3 as in S1 row 5; Blockage 2's E2 11.6 → 12.8 (z +0.6) | **miss** on all three |
| S6 CLAIMED | `blocked` in 10 to 40 percent of Blockage 6-8 runs; Blockage 3-5's glasses down by under 10 | `blocked` in 0, 0 and 1 percent of the probe runs (Blockage 6, 7, 8); the glasses rose on Blockage 3-5 (S1 row 5) | **miss**, and the glasses moved the other way |
| L1 ROW | `other` down by 10 or more on Surround 5 and Buridan 1; row tags on nearly every run | Surround 5 -10.0 (z -3.2), Buridan 1 +0.8; a ROW stands in 90 to 97 percent of runs | **hit** on Surround 5 and on the presence, **miss** on Buridan 1 |
| L2 COLUMN | Blockage 1-5's E1 within noise or up by under 5; Surround 1-4's E1 up by under 10; column tags on every item; the re-cast's second tier non-empty in most runs | Blockage 1-5's E1: -0.8, +5.6 (z +2.1), +0.2, +3.6, +4.4; Surround 1-4's E1 within noise (G1); a COLUMN stands in 98.5 to 100 percent of runs; a re-cast (COUNTERPART) stands in 23 to 50 percent | **hit** on the bounds and the tags (Blockage 2 just over 5); **miss** on the re-cast's frequency |
| L3 CORNER | Surround 1-4's E2 up 5 to 10 from this role; Blockage 3-8's E3 up (with S5); corner pairs proposed in most Blockage 2-8 runs | E2 rose on Surround 1-4 (G2; the lesions cannot apportion it between G2, L3, L4 and A7); E3 did not rise; a CORNER stands in 66 to 99 percent of runs; under the L3 lesion Blockage 7's E3 goes 18 → 27 and Blockage 8's E1 68 → 79, E2 86 → 76 of 200 (within noise) | **hit** on the presence; the E2 share cannot be attributed; **miss** on E3 |
| L4 LONER | Surround 1's E2 up 5 to 15; Surround 5-10's E2 (the isolated object) down 5 to 15; the loner tag on Surround 5's isolated cup in nearly every run | Surround 1's E2 +6.6 (z +2.1); Surround 5-10's E2 **up** by 6.0 to 14.0 on every set (z +2.0 to +4.2); a LONER stands in 58.5 percent of Surround 5 runs and 94 of Blockage 2's; under the L4 lesion Surround 1's E2 goes 103 → 117 and Surround 5's E1 34 → 46, E2 98 → 91 of 200 | **hit** on Surround 1; **miss**, in the opposite direction, on Surround 5-10: the loner pairs (cup with glass, glass with glass) lower E1 and raise E2 where the registered reading had the loner as the poorer counterpart; **miss** on "nearly every run" |
| L5 STRAY | Surround 10's E1 within 5 points of Surround 5's; the junk's cost at most 1 codelet per two objects; the main answer's fall over twelve junk objects halved; stray tags on the junk in nearly every run; no role pair from a stray | 17.0 against 14.6; 0.75 codelets per two objects (80.1 at none, 84.6 at twelve; v1 2.4); the main answer falls 49.6 → 32.6 (17 points; v1 36.6 → 26.4, 10 points); a stray group was read in 8 of 30 Surround 10 runs at item 08; no pair from a stray by construction (tested) | **hit**, **hit**, **miss** (the fall is larger, not halved), **miss** on the presence, **hit** on the pairs |
| K1 COUNTERPART | `other` down 10 or more on Surround 5 and Buridan 1; E1 up on Surround 5-10; a re-cast in nearly every run, tier 1 non-empty on Surround 5-10 in most runs, empty on Buridan 1 | Surround 5 -10.0, Buridan 1 +0.8; E1 did not rise; a re-cast is published in 23 to 52 percent of runs, tier 1 in 1 run of 800 (item 07) | **hit** on Surround 5's `other`; **miss** on the rest |
| K2 TYPE_KEY | Figure/Ground E3 up by 10 or more; a key read in a quarter or more of Figure/Ground runs | E3 +2.6 (z +1.4); a key in 7 of 40 runs at item 09 (17.5 percent), standing in 1.5 percent of Buridan 1 runs; the K2 lesion changes 8 of 200 Figure/Ground runs and no count | **miss** on both |
| A1 MEMBER | E1 up 5 to 15 through C3; the touched object mapped only through its group in a tenth or more of Surround 5 runs | C3 not built; `through: group` in 2 of 200 Surround 5 runs (and 0.6 percent of the 500 of §11.5) | **not built** (C3); **miss** on the case's frequency |
| A2 SIDEKICK | Buridan 1 (modified) E2 up by 10 or more from 16.4, E1 within 10 of E2; Buridan 2's E2 within noise; the scan no longer flat; coherent pairs in a quarter or more of Buridan 1 runs | E2 18.6 (z +0.9), E1 28.8 (10.2 apart); Buridan 2's E2 24.4 → 20.8 (z -1.4); the scan within noise (S1 row 2); 0.18 coherent pairs per run at 500 (so in at most 18 percent of runs); a SIDEKICK stands in 18.5 percent of Buridan 1 runs, the hand-read sidekicks missing in 185 to 187 of 200; the A2 lesion leaves the coherent pairs (0.155 → 0.175) and E2 (38 → 35 of 200) where they were | **miss** on E2 and the gap (by 0.2 points), **hit** on Buridan 2, **miss** on the scan and the pairs; the coherent pairs that exist are the ground pairs', not the sidekicks' |
| A3 ANCHOR | E1 up on Surround 5-7, 10; Surround 8-9's E1 up by under 10; anchor tags on the touched cup and the glass in most Surround 5 runs | E1 did not rise (G1); Surround 8-9's E1 16.6 → 15.2, 4.2 → 2.8; an ANCHOR stands in 4.5 percent of Surround 5 runs (the glass's and the cup's anchor tags missing in 197 and 194 of 200) | **miss** on E1 and on the presence; **hit** on the Surround 8-9 bound, by the role never being read there rather than by the type term rejecting it (§7's reading) |
| A4 FIGURE | a figure on the touched fork and Henry's glass in most Figure/Ground runs | every run at item 09; standing in 98.5 percent of Buridan 1 runs; the A4 lesion moves Figure/Ground's E2 63 → 78 and E3 23 → 16 of 200 (within noise) | **hit** |
| A5 GROUND | the ground-to-ground group correspondence in the Worldview in a fifth or more of runs; group correspondences up from 0.0 | a group correspondence in the Worldview in 6.6 percent of Surround 5 runs (v1 0.4; `ch4`) and the `stated` bench's statement 16 at 5.6 (v1 0.0); the A5 lesion takes Figure/Ground's Worldview from 14 to 3 percent with two or more correspondences and its literal share from 39 to 19 of 200 (z -2.8), Buridan 1's from 22.5 to 10 percent | **miss** on the fifth, **hit** on "up from 0"; the lesion shows the ground pairs are what fills the Worldview on those two sets, and that they raise the literal answer |
| A6 ECHO | E1 up on Surround 5-7, 10; the row-to-row correspondence in the Worldview in a quarter or more of Surround 5 runs | E1 did not rise; 5.6 percent (statement 16); an ECHO stands in 48.5 percent of Surround 5 runs and 59 of Buridan 1's; the A6 lesion moves nothing beyond noise | **miss** on both |
| A7 TWIN | Surround 1-4's E2 up by under 5 from this role; M8 moved by under 0.05 from this role; the third tier non-empty where a same-type object stands across | under the A7 lesion E2 moves +6, -6, -3, -5 of 200 on Surround 1-4 (within noise); M8 moved +0.06 in all under `standard` (not attributable); a TWIN stands in 46.5 to 58 percent of runs | **hit** on the bound (the role moves nothing measurable), **not attributable** on M8, **miss** on "wherever" (half the runs) |

### S3. Per consumer (§3)

| Consumer | Registered | Measured | Verdict |
|---|---|---|---|
| C1 | the largest single cause of every gain; the Worldview from 2 percent to a majority; group correspondences from 0 to a quarter or more of Surround 5 runs | the C1 lesion takes the Worldview's two-or-more share from 16 to 7.5 (Surround 5), 22.5 to 1.5 (Buridan 1), 4 to 1 (Blockage 6), 24.5 to 12.5, 21 to 6.5, 17 to 6 percent (Surround 6, 7, 10) and the literal share from 42 to 21 of 200 on Buridan 1 (z -2.9); E1 unchanged everywhere; the Worldview share at 500 is 4 to 23 percent; group correspondences 6.6 percent | **hit** on C1 being what fills the Worldview; **miss** on the majority and the quarter; the gains C1 caused are the literal answer's, not E1's |
| C2 | `other` down by 10 or more on Surround 5 and Buridan 1, by 5 or more on every Surround set; the literal share unchanged; the pair proposed by pick-salient-obj a tier-1 or tier-2 candidate in most runs | `other` as in S1 row 7; the literal share 13.2 → 12.8 on Surround 5; the C2 lesion changes 16, 5, 7 and 16 of 200 runs and no count (`other` 68 → 63 on Surround 5, 100 → 100 on Buridan 1); a tagged counterpart is picked in about a sixth of runs (item 07) | **hit** on `other` (by other consumers: the lesion of C2 alone does nothing); **hit** on the literal share; **miss** on the pair being a tier candidate in most runs |
| C3 | E1 up 5 to 15; `through: group` with a role note in a tenth or more of Surround 5 runs | not built | **not built** |
| C4 | E3 up 10 to 25 on Blockage 6-8; the touched cup's structure value on Blockage 6 above 8.2; `blocked` in 10 to 40 percent | E3 within noise; structure 8.2 → 42.7; `blocked` in 0 to 1 percent; the C4 lesion changes 0 to 3 runs per carrier | **miss**, **hit**, **miss** |
| C5 | small on its own (under 5 points on any set); parallel pairs above 0 | the C5 lesion moves nothing beyond noise (Surround 5's E2 98 → 92, E1 34 → 39 of 200; Buridan 1's coherent pairs 0.155 → 0.125) | **hit** on "small"; the parallel pairs were not counted (no measure records them) |
| C6 | the symmetry nodes active in most runs with a Worldview entry; placement slippages on most top-level correspondences | an image tag stands in 86 to 93 percent of runs; the C6 lesion moves E1 by 0, -6, -7, -1 of 200 on Surround 5, 6, 7, 10 and the Worldview's two-or-more share from 15.5 to 12 and 16 to 11.5 on Surround 2 and 3 (within noise); the nodes' activations were not recorded | **hit** on the slippages' presence; the effect on the answers is within noise; the activation claim is unmeasured |
| C7 | Surround 10's E1 within 5 of Surround 5's; the junk's cost halved; the glass the most salient object on Eliza's side in most Surround 5 runs | 17.0 against 14.6; 0.75 against v1's 2.4 codelets per two objects; the C7 lesion changes 0 to 55 runs per carrier and no count (Surround 10's E1 31 → 33 of 200); the salience ranking was not recorded | **hit**, **hit** (by the whole engine: the lesion of the role term alone moves nothing); **unmeasured** |
| C8 | the medians up 10 to 40 percent, within a factor of 2.5 of 50-200; no capped run under `standard`; 5 to 15 role codelets before the answer on Surround 5; role codelets before the answer in nearly every run | +27 to +32 percent, within the factor; no capped run in any `standard` survey (the only capped runs of the 486,800 are the temperature-high lesion's); 25.3 role codelets per run on Surround 5 (§11.7); a role codelet in every run | **hit**, **hit**, **miss** (nearly twice the bound), **hit** |

### S4. The lesion expectations (§5)

The probes of item 13 (`tools/role_probes.py`, 200 runs per batch, every batch of a
problem on the same seeds; `results/role-probes.json`). C5's patch is of
`correspondences.role_slippages`, not `role_points` as the row says (the slippages gone take
the points and the parallelism with them, which is the degradation the row registers). An
A2 lesion (`describe.sidekicks_of`) was added at item 13 for the row that names it. C3 has
no lesion (not built).

| Lesion | Hit if | Measured | Verdict |
|---|---|---|---|
| C1 | the Worldview's two-or-more share back below 10 percent; Surround E1 loses half its gain | 16 → 7.5, 22.5 → 1.5, 4 → 1 percent on the carriers; there was no E1 gain | **hit** on the share, **no gain to lose** |
| C6 | the Surround E1 gain loses 5 to 15 points; Surround 1-4's E2 gain lost by at least half | E1 0, -6, -7, -1 of 200 on Surround 5, 6, 7, 10; E2 -1, -5, +6, +7 of 200 on Surround 1-4 | **miss** on both (within noise either way) |
| S5 and C4 | E3's gain lost by at least half; `blocked` never recorded | no E3 gain; under S5 no `blocked` on any carrier; under C4 `blocked` still recorded (the record is read off the CLAIMED tag, which S5 lesions) | **hit** on the record under S5; nothing to lose |
| A2 | coherent pairs back to 0 on Buridan 1; E2's gain lost; the scan flat again | coherent pairs 0.155 → 0.175; E2 38 → 35 of 200; the scan was not run under the lesion | **miss**: the coherent pairs are the ground pairs' (A5), not the sidekicks' |
| C2 | `other` back up by at least half of its fall | Surround 5 68 → 63 of 200, Buridan 1 100 → 100 | **miss**: C2 reads a tag in a sixth of runs and moves nothing measurable |
| C3 | E1 loses 5 to 15; the literal share rises | not built | **not built** |
| C5 | parallel pairs back toward 0; answers within noise | answers within noise (above) | **hit** on the answers; the pairs were not counted |
| C7 | Surround 10's E1 below Surround 5's by 5 or more again; the junk's cost back toward 2 | 33 against 34 of 200 under the lesion; the `focus` bench was not run under the lesion | **miss** on E1; the cost is unmeasured under the lesion |
| K1 | as C2, plus E1 loses 5 to 10 | E1 +1, -1, -5, -1 of 200 on Surround 5, 6, 7, 10 | **miss** |
| L2 | within noise on the four carriers; Blockage 1-5's E1 down by under 5 | Surround 5's spoon4 9 → 26 of 200 (z +3.0) and E1 34 → 25; the three other carriers within noise (Buridan 1's E1 62 → 75, z +1.5); Blockage 1-5's E1 186 → 182, 163 → 156, 86 → 86, 90 → 88, 91 → 84 of 200 (at most 3.5 points) | **miss** on Surround 5 (so the merge rule does not apply: §8), **hit** on Blockage 1-5 |

**The existing seven lesions**, re-run at item 12 (RESULTS.md §6.2, 500 runs): 14 of the 28
cells as predicted, 14 no change, none against (v1: 16, 12, 0). `losers-destroyed` and
`fixed-lengths`, registered to show an effect now: no change on any carrier (Surround 5's E2
47.0 and 45.2 against the intact 49.6, Buridan 1's E1 27.8 and 27.6 against 28.8): **miss**.
`flat-depths` stays inert, as allowed: **hit**. The four that were as predicted did not all
stay so: `rho-1` is "no change" on Surround 5 and Blockage 2 now (its literal share 9.0
against the intact 12.8 on Surround 5, where v1 read 2.0 against 13.2): **miss** on that
part.

### S5. The variants expectations (§6)

| Variant | Registered for the roles | Measured (500 runs, both engines) | Verdict |
|---|---|---|---|
| mirrored, side-swapped, rotated | exact invariance within noise | mean TV 0.041, 0.044, 0.041 (v1 0.042, 0.049, 0.048); two exceptions of 159 at p below 0.01 for each engine | **hit** |
| translated | the same as v1 | 0.019 (v1 0.024), no p below 0.01 but one each | **hit** |
| type-substituted | the share keeping its role at or above v1's | 41.1 percent (v1 38.8); mean TV 0.085 (v1 0.093) | **hit** |
| junk-added | less change than v1 | mean TV 0.077 (v1 0.095); 30 surveys at p below 0.01 (v1 42); the stray's share lower on every small table (Blockage 2: 51 of 500 against v1's 67; ch2-fig02 315 against 323) | **hit** |
| decoupled | prediction 12 | miss (S1 row 12); the Blockage family's mean TV 0.534 against v1's 0.480, 2.3 standard errors apart | **miss**, and the one cell where "lost nothing" fails (§11.5) |
| on-saucer | held out (§8): the holder tagged, the pair held together, the saucer never the answer in more than 5 percent of runs | the holder is not built; the saucer is the answer in 17 to 39 percent of runs on Surround 1-10 (v1 15 to 37) and 42 percent on ch2-fig03 (v1 38) | **not built** (the holder); **miss** on the saucer's share, which is v1's |

"Lost nothing" (D-102's rule: the roles' mean TV not above v1's by two bootstrap standard
errors, per variant and family): holds on 38 of the 39 variant-family cells and **fails on
decoupled Blockage** (above). Every other difference is within two standard errors, and on
junk-added the roles are more invariant than v1 on three families by more than that.

### S6. The two readings (§7)

- **TYPE_KEY, FIGURE and GROUND on Figure/Ground 1.** Against his numbers: E1+E2 81.8 → 69.4
  (z -4.6), lost. For his account: Henry's lone glass 8.4 → 11.0 (z +1.4), within noise,
  with a structure value of 68.1 (v1 52.7). What rose is the literal fork, 9.8 → 19.6 (z
  +4.4): the ground pairs fill the Worldview and the touched fork is then mapped through
  nothing (D-063; the A5 lesion returns the literal share to 19 of 200). **Further from his
  numbers and not nearer his account**: both readings are misses.
- **ANCHOR and CONNECTOR on Surround 8 and 9.** E1 16.6 → 15.2 and 4.2 → 2.8: no rise, so the
  bound of 10 holds. Not because the Worldview rejected anchor-to-anchor on type, but because
  the anchor is read in 4.5 percent of runs and the connector in none before the answer. The
  registered reading was not tested by the runs.
- **Blockage 2's touched cup.** 11.6 → 12.8 (z +0.6): neither the registered rise of 5 to 15
  nor a rise beyond 25. The barrier is read in 1 percent of runs there (S5).

### S7. The held-out questions (§8)

- **COLUMN against MIRROR_IMAGE.** The registered test: the L2 lesion changes no measure on
  the four carriers beyond two standard errors, and Blockage 1-5's E1 by under 5 points. On
  Surround 5 it changes an answer beyond two standard errors (spoon4, 9 → 26 of 200, z +3.0,
  with E1 34 → 25) and the tags per run (52.8 → 41.3); on Blockage 1-5 E1 moves by at most
  3.5 points. By the registered rule **both roles are kept** and the merge is not made; the
  difference is where the region reading and the point reading part: on Surround 5 the
  touched cup's column holds three objects (the cup, a spoon and a fork of the other row's
  cluster), so the column identity of C6 admits a spoon that the point image does not, and
  without the column tags those runs go elsewhere. RESULTS.md §11.4.
- **STRAY and the junk.** The cost per two junk objects 0.75 codelets (at most 1: hit); the
  main answer's fall over twelve objects 17 points against v1's 10 (halved: miss); no role
  pair from a stray (hit, by construction).
- **The cup on its saucer.** The holder form is not built (item 06), so nothing of this
  question is measured beyond S5's on-saucer row: the saucer's share is v1's.

### S8. The signs of fitting (§9)

- **A default moved after a measurement without a logged decision**: none. The lock changed
  at items 03, 04, 05, 07, 08 and 10, each with its decision written before the measurement
  and a Re-interpretation log entry. Two definitions were changed after a seed search at
  item 10 (D-089's Alternatives), one structural rule at item 08 (D-084 as revised), and a
  top-down row was tried and dropped at item 10 for doubling the quick start's length, all
  logged; no value was chosen from a chapter 5 number.
- **Prediction 12 failing while prediction 3 holds**: both fail, so the sign is not raised;
  what the pair says instead is that the roles read the mirror image no less than v1 does.
- **A loss of invariance against v1**: one cell, decoupled Blockage (S5).
- **The oracle gap closing only on the carriers**: the gap did not close on the carriers
  (S1 row 14), and random tables have no hand-read roles to measure it on.
- **A role whose lesion changes nothing on any carrier, kept without the merge question**:
  L5 STRAY (0 runs changed on three carriers, 25 of 200 within noise on Surround 10), K2
  TYPE_KEY (0 to 8 runs), S5 BARRIER and C4 (0 to 3 runs), C7's role term (no count moved)
  and C5 (no count moved). None was merged or removed at this item: each is read too seldom
  before the answer (STRAY needs a find-roles on the junk's group, BARRIER a Worldview with
  two correspondences and a re-cast, TYPE_KEY a ground pair in the Worldview) for a 200-run
  probe to show its consumer's effect, and the registered question (§8) was asked of COLUMN
  alone. They are listed for the author in RESULTS.md §11.4, with the alternative of
  reading them earlier (a structure change and a logged decision).

### S9. The tally

Of everything registered with a measurable threshold: the hits are the exact and translated
invariance (both engines), the type-substituted and junk-added readings, M4, the run length,
the Surround 1-4 E2 direction, Surround 5's `other` (and nine Surround sets' `other`), the
Blockage structure values under E3, the midline, row and column tags, C1 as what fills the
Worldview, the focus bench's two bounds, the random tables, the within-ten count and the
`human` Spearman bound. The misses are every E1 and E3 prediction (3, 5, 6, G1, G3, S4-S6,
A3, A6, K1, K2), the Worldview majority (1), Buridan's scan and sidekicks (2, A2), the
re-cast's frequency, the oracle gap, the tags per run, the churn's settling, the temperature's
end, the `standard` Spearman direction, the faults' fixes, the decoupled variant, the saucer,
and the inert lesions. Roles as built move the answers the Worldview already reached (the
isolated cup, the `other` share, the literal answer) and seldom reach the answers French's
account names (the glass inside the group, the blocked cup, the lone glass) before the run
ends: the mechanism is in place, read in most runs, and too late for the answer on the sets
that matter most. RESULTS.md §11.8 traces each miss to its decision.

### S10. Re-scored at item 06 (2026-10-14): CONTAINER and C3, built

Item 06 (cut off at iteration 8, reopened and built after item 17) adds what the rows
above score "not built": CONTAINER in its setting and holder forms (D-087 as revised) and
consumer C3, the answer from group position by the members' roles (D-097), with examine-cp
offering the anchor to the anchor first and C1's setting-with-setting and holder-with-holder
pairs. The verdicts above stand as they were; this section re-reads the "not built" rows on
the same measures, at the item's size (200 runs per carrier against HEAD, the probes re-run
at 200, a 500-seed scan for the parts' frequency), hits and misses alike.

| Row above | Registered | Measured at item 06 | Verdict |
|---|---|---|---|
| S3 CONTAINER (S2) | the setting form: E1 up 5 to 15 on Surround 5-7, 10 through C3 | the setting tag is read in 26, 21 and 29 of 500 runs on Surround 5, 7 and 10 (whenever an anchor is read, its row is read as a setting); E1 36 → 36, 31 → 33, 31 → 31 of 200 against HEAD (z 0.0 to +0.3) | **miss** (built, read in 5 percent of runs, E1 unmoved) |
| A1 MEMBER (S2) | E1 up 5 to 15 through C3; the touched object mapped only through its group in a tenth or more of Surround 5 runs | the group case in 4 of 200 Surround 5 runs (2 before; 6, 1, 4 of 500 on Surround 5, 7, 10): about 1 percent, not a tenth; E1 unmoved | **miss** |
| C3 (S3) | E1 up 5 to 15; `through: group` with a role note in a tenth or more of Surround 5 runs | `through: group` in 2 percent of runs; a role note in 0 of 200 (0, 1, 1 of 500 on Surround 5, 7, 10: the touched cup carries no ANCHOR when the case comes, so the roles' reading agrees with the words'); examine-cp's anchor pair fired in 0 of 1,500 runs | **miss** |
| C3 lesion (S4) | E1 loses 5 to 15; the literal share rises | `answer.role_member` patched to None: 0 of 200 paired runs differ on surround-05, blockage-06, buridan-01, blockage-02, surround-06, -07, -10 | **miss** (no degradation to see: the consumer fires too seldom) |
| on-saucer (S5) | the holder tagged, the pair held together, the saucer never the answer in more than 5 percent of runs | the holder tag is read in 496 of 500 runs of the on-saucer variant of Surround 5; the saucer takes the answer in 38 of 200 (36 before, z +0.3): nothing holds the pair together (C1's holder pair needs a second holder across the table, and no carrier has one) | **split**: the tag a hit, the share a miss |
| the cup on its saucer (S7) | held out | as the row above: the holder is read; the saucer's share is unchanged; 137 of 200 paired runs differ from HEAD's draws (the holder's reading is a draw) and the answers do not move | measured, not met |

What the re-score adds to the tally of S9: the two "not built" rows become misses, the
on-saucer row a split, and the one hit is a perception (the holder read in 99 percent of the
runs that have a holder), not an answer. The reading is item 07's, 10's and 13's again: the
structure the consumer reads (ANCHOR on the touched object, both anchors tagged while the
group pair's inner pairs are open) forms after the first Worldview entry, and the answer comes
first. The decisions: D-097, D-087, D-056 with D-081 (RESULTS.md §11.8, finding 7).
