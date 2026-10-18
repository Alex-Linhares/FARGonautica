# The roles: the design (D-078 on)

The reconstruction's engine (`tabletop/`) reads, since the roles loop of 2026-10-14, the
**role** each item plays in the analogy: not what an object looks like but what it does in
the story the run is building, the loner, the sidekick, the anchor, the mirror image, the
counterpart. French names these parts in his prose and his program has no vocabulary for
them, so the roles are a **re-interpretation** of Tabletop, not a replication: every choice
is a decision of Kind re-interpretation (or spec-inferred, where French describes a mechanism
the reconstruction had left out) in [`DECISIONS.md`](../DECISIONS.md) after D-078, each with
its knob. This page is the design in the package's own words, with French's pages where he
supports it: what a role is, the five families and the registry, the tag as a structure that
competes and changes, the Slipnet's role nodes, the six role codelets, the re-cast, the eight
consumers, what a run shows, and the measures that keep the roles honest. The numbers are
[`RESULTS.md`](../RESULTS.md) §11; the predictions registered before any role was built, and
their scores, are [`predictions.md`](predictions.md); v1 read in role vocabulary is
[`roles-audit.md`](roles-audit.md).

The roles are part of the engine and always on: no profile, `Config` field, command-line
option, bench option or GUI control turns a role or a consumer off, and every role knob is
refused at zero or below, so that no value can do it either (D-078; the author's answers). v1,
the engine before roles, is commit `e38d6e0`, kept as a baseline in `results/v1/` and re-run
from a throwaway worktree when a comparison needs it.

## 1. What a role is, and the three readings

A role is the function an item plays in the analogy. French's own account is role-like:
sameness is counterpart, "within the particular context under discussion, X is the
counterpart of Y" [F95 p. xv]; an analogy holds when B plays the same role as A plays
[F95 p. 12]; a cup set apart is a loner, the cup in the lower right-hand corner of Surround 1
being seen as one, with a good deal of pressure to touch the small glass as a result
[F95 p. 118]; the objects beside the candidates are sidekicks, and "weak sidekicks (i.e.,
single objects as opposed to small sameness groups) are not sufficient" [F95 p. 131]; the
glass of Figure/Ground is an exceptional object, a figure object standing out against a dull
background [F95 p. 140]. His program reaches none of these by name: it has regions relative to the
touched object (D-032), a salience of ten factors (D-031), the group frame (D-029) and the
correspondences' slippages (D-030), and v1 reproduced those (D-001 to D-077).

A role is read in one of three ways (D-078, `roles.READINGS`):

- **on one table** (`single`): from the items of one side alone, the members of a group, a
  gap in a row, a loner, the column an object stands in;
- **as a comparison** (`comparison`): Henry's side against Eliza's, the mirror image of an
  object across the midline, a group's echo, the touched object's twin;
- **off the Worldview** (`worldview`): read from the correspondences already standing, the
  symmetry axis the story has taken, the type substitution it makes, the correspondence that
  bars the obvious counterpart.

The two sides make one comparison (D-078): a role read on Henry's side is paired with the
same role on Eliza's, and that pairing is what the consumers read.

## 2. The five families and the registry

The registry (`tabletop/roles.py`, `REGISTRY`; D-079) is closed at 23 roles in five
families; a new role needs the author's answer, not an item. Each row names the role's code,
its reading, the Slipnet node its strength reads (and the others it jolts), the detector that
reads it (one module-level function, so that a test can lesion it, D-104), the consumers that
use it and the decision that defines it.

**The geometric family** (G): where an item lies relative to the table's symmetries.

| Code | Role | Reading | Node(s) | Read by | Consumers | Decision |
|---|---|---|---|---|---|---|
| G1 | MIRROR_IMAGE: the object across the midline at the mirror point, within the blur | comparison | mirror symmetry, opposite | `correspondences.mirror_and_point_images` | C1, C6 | D-099 |
| G2 | POINT_IMAGE: the object at the point image through the table's centre | comparison | diagonal symmetry, opposite | the same | C1, C6 | D-099 |
| G3 | SYMMETRY_AXIS: the symmetry the Worldview's correspondences agree on, the touched object's left out | worldview | symmetry | `worldview.symmetry_axis` | C6, C1 | D-083 |

"Mirror symmetry" is reflection across the midline and "diagonal symmetry" is point symmetry
through the table's centre: French's names stay (the author's answer C9). The images are
read at the level of points with D-004's blur as the only tolerance; the next most preferred
region is directly opposite the touched object and the most preferred diagonally opposite it
[F95 p. 66], and under pressure one symmetry gives way to the other [F95 p. 86].

**The structural family** (S): what parts the table into situations, holds, bridges or bars.

| Code | Role | Reading | Node(s) | Read by | Consumers | Decision |
|---|---|---|---|---|---|---|
| S1 | MIDLINE: the separator between the two sides, perceived when a cluster stands on an outer row and nothing grouped reaches the middle band | single, on the table | separator | `describe.midline_perceived` | C6, C1 | D-084 |
| S2 | GAP: the white space between two clusters of a row, at least the proximity criterion's zero gap wide | single, on the table | separator | `describe.gaps_of` | C1 | D-086 |
| S3 | CONTAINER: the holder whose footprint holds a smaller object's centre (a cup on its saucer), tagged on the holder; the setting, a linear gestalt group with one anchor, read as a unit around it | single | container | `describe.container_of` (`holder_of`, `setting_of`) | C1, C3 | D-087 (as revised at item 06) |
| S4 | CONNECTOR: the member whose two axis neighbours are of different types, each with its own type beyond, so that it bridges two runs it is in neither of | single | connector | `describe.connector_of` | C1, C5 | D-088 |
| S5 | BARRIER: the other end of a Worldview correspondence that claims the touched object's obvious counterpart | worldview | barrier | `worldview.barrier_and_claimed` | C4 | D-089 |
| S6 | CLAIMED: that counterpart, while the barrier's tag stands | worldview | barrier | the same | C2, C4 | D-089 |

People break a table into two situations, Henry's half and Eliza's, unless the arrangement is
strange [F95 p. 39], and French's program has no boundary line (D-005): the midline is a
perception, read from the clusters of the two outer rows, and a stray in the band does not
block it. The barrier is a relational reading of Blockage, where the glass-pair
correspondence and the cup-glass one compete [F95 pp. 33-34] and Eliza has "the natural
tendency to touch whatever object on her side of the table has the closest conceptual
resemblance to the touched object" [F95 p. 114]; nothing spatially between is read (the
author's answer C1).

**The selector family** (L): where an item stands in the table frame.

| Code | Role | Reading | Node(s) | Read by | Consumers | Decision |
|---|---|---|---|---|---|---|
| L1 | ROW: the row word of the table frame, Henry's or Eliza's, read once a midline is perceived and never for an item seen in the middle band | single | row | `describe.row_role` | C2, C1 | D-085 |
| L2 | COLUMN: left, middle or right, read at first sight | single | column | `describe.column_role` | C2, C6 | D-085 |
| L3 | CORNER: an object on its own, or a gestalt group, seen in a corner region | single | corner | `describe.corner_role` | C1 | D-085 |
| L4 | LONER: an object a find-group found no neighbour for, on an outer row, set off by a gap or alone on its row | single | isolated | `describe.loner_of` | C1, C5 | D-086 |
| L5 | STRAY: an item of neither side, an object or a whole gestalt group lying in the middle band | single | separator | `describe.stray_role` | C7, C1 | D-086 |

The table frame of D-029, described for the slippages and never read in v1, is read at last
(the frame's left and right are the sides of the table as a whole [F95 p. 85]); the stray is
what the program should be able to ignore, not consider and then reject, as French says of
the small groups of silverware [F95 p. 36]. A corner
and a stray are read only after a find-group has looked around the object, or on a gestalt
group, so that they never fight the MEMBER the object is about to become.

**The key family** (K): the one carrying of a role across the table.

| Code | Role | Reading | Node(s) | Read by | Consumers | Decision |
|---|---|---|---|---|---|---|
| K1 | COUNTERPART: a candidate for the touched object's part on the other side, in a tier | comparison | counterpart | `roles.recast` (the recast-role codelet) | C2, C1 | D-093 |
| K2 | TYPE_KEY: the type substitution a Worldview correspondence between two grounds makes (fork for glass) | worldview | key | `worldview.type_keys` | C1, C5 | D-094 |

**The content family** (A): what an item is to its group and to the other side.

| Code | Role | Reading | Node(s) | Read by | Consumers | Decision |
|---|---|---|---|---|---|---|
| A1 | MEMBER: an object in a gestalt group, with its group-frame words as evidence | single | group, end, middle | `describe.member_roles` | C5 | D-090 |
| A2 | SIDEKICK: an immediate neighbour of a different type of the touched object or of an object of its family (its nearest ISA category); a run of identical sidekicks is one tag of that weight | single | sidekick | `describe.sidekicks_of` | C1, C5 | D-090 |
| A3 | ANCHOR: a member of a linear group with a neighbour on each side whose nearest category no other member shares | single | anchor | `describe.anchor_of` | C1, C5 | D-087 |
| A4 | FIGURE: the only object of its type on a row that has a ground | single | figure | `describe.figure_role` | C1, C5, C7 | D-091 |
| A5 | GROUND: each object of the one type two or more members of the row's group share, and the sameness subgroup of that type as a whole | single | ground | `describe.ground_role` | C1, C5 | D-091 |
| A6 | ECHO: a group across the midline with the same multiset of types, or of nearest categories | comparison | echo | `correspondences.echoes_of` | C1 | D-092 |
| A7 | TWIN: an object of the touched object's type across the midline | comparison | sameness | `correspondences.twin_of` | C2 | D-092 |

The sidekicks are French's p. 130, the anchor and the connector his reading of Surround 5,
where the groups around the touched cup and the glass are alike and the subgroups sit in
corresponding positions on either side of them [F95 pp. 30-31]; the figure and the ground
are his Figure/Ground table [F95 pp. 139-140], where each small group echoes, in object type,
a larger group on the other side; a ground needs a figure and a figure a ground, so a row of
doubled forks and doubled spoons reads neither.

**Rivals** (D-079, `roles.RIVALS`): readings that exclude one another on one item fight, and
the loser is discarded: BARRIER against SIDEKICK, MIRROR_IMAGE against POINT_IMAGE, LONER and
STRAY against MEMBER, STRAY against LONER, FIGURE against GROUND. Compatible roles coexist: a
glass can be a MEMBER, an ANCHOR and a CONNECTOR at once.

## 3. The tag: a competing, revisable structure

A role tag (`roles.RoleTag`) is a Workspace structure like a bond or a correspondence, not a
label computed once (D-079; the author's answer D1): proposed by a codelet, rated, built,
read, and breakable. The pool is `Workspace.roles`, the tags standing in the order tagged,
named `r1`, `r2`, ...; a tag goes with the group or correspondence it stands on or names.
Each carries its item (an object, a group, the table for the midline, the Worldview item for
the axis and the keys), its role, its arguments (the partner of a pair role, the object a
sidekick stands beside, a barrier's claim), its weight (the objects in the item: 1 for an
object, n for a group or a run of identical sidekicks), the evidence it was read from, who
tagged it, and a **strength** in 0 to 100 (`roles.rate`): points per unit of weight, points
for the activation of the role's node over 150, and points per Worldview correspondence that
uses the tag's item at an end, the story's support. So the strength reads what the item is,
how alive the concept is and how the current story uses it, and it falls when the Worldview
changes.

A newcomer fights every standing rival on its item by the Worldview's own law
(`worldview.rival_chance`, the strengths as ratings, one coin under a(T); D-040, D-060); the
loser is discarded `lost to rN`. The role checker, check-roles, samples a tag (the weak
first), re-rates it and breaks it with chance one minus its strength over `role_break_scale`,
under the temperature law; a tag at or above the scale cannot be broken. A tag whose
structure is removed goes with it (`structure gone`). Every change is an event, so the trace,
the event log and the GUI show a role appear, be read and go, and the churn bench measures
how often (§11.7 of RESULTS.md: roles change in 60 to 87 percent of runs on the four
carriers, and the churn does not settle before the answer, because the runs end before the
temperature falls into its cold third).

**Perception** goes through one entry point, `roles.detect(world, item)`, which asks every
built detector for the roles it reads on an item: `World.step` calls it for every object a
codelet brings into view (the column, the corner, the members: the first-sight roles), and the
find-roles codelet for what needs structures first (a group, a gap, a loner, the midline).
The oracle probe replaces that one function with hand-read roles, and nothing in the engine
knows the difference (D-104). **Nothing is gated**: with an empty pool every consumer posts
nothing, draws nothing and changes no rating, and the run is French's.

## 4. The Slipnet's role nodes and links

Fifteen nodes were appended after `isolated` (63 in all; D-080), none on French's screen of 41
boxes (D-007): `role` and `counterpart` at level 3, and at level 2 `sidekick`, `anchor`,
`figure`, `ground`, `echo`, `container`, `connector`, `barrier`, `separator`, `key` in the
group `role`, and `row`, `column`, `corner` among the positions of the table frame. FCCA says
of the node set that "Many others could easily be added, but such additions would require no
changes to the architecture of the program." [FCCA p. 381]. The 29 links (14 ISA and 15
labelled, each marked `re-interpretation` in the link table) follow D-010's bands of length,
so no length is new: the one slip French describes, mirror symmetry giving way to diagonal
symmetry under pressure [F95 p. 86], gets the `opposite` label that names it; figure against
ground is an opposite too; the separator and the column lie near mirror symmetry, the corner
near diagonal symmetry, the sidekick near the neighbour, the anchor and the connector near
the middle, the echo and the counterpart near sameness, the key near opposite. A tag jolts
its node when built; a comparison role confirmed on both sides, and the axis, jolt by the
structure amount; a role slippage entering the Worldview jolts by the label amount (D-016).
A consequence measured, not intended: in v1 the label nodes `opposite` and `sameness` had no
link, and through the new ones `opposite`, held near the ceiling all run (D-017), warms the
role subnet from the first update. The GUI's grid shows the fifteen in a band under French's
boxes, labelled as not on his screen (P3; the author's answer C7).

## 5. The six role codelets

Codelet types are 29 (D-081): French's 23 (D-051) and six for the roles, all in the strip's
order with their labels, posted top down by the role nodes as an active concept dispatches
codelets to look for instances of itself [F95 p. 64], and as follow-ups of the codelets that
build what they read. Their raw urgencies were set a priori and never moved after a
measurement (the author's answer C8; the churn bench records that a quarter to a third of the
codelets chosen before the answer are role codelets, D-106).

| Type | Label | Band | Urgency | What it does | Posted by |
|---|---|---|---|---|---|
| find-roles | FND-RL | scout | 30 | reads the table (the midline, the gaps, the strays) and one item, its argument or a salient one, through `roles.detect`; posts compare-roles, propose-role-cp and, once the touched object's row is read, recast-role | one in the starting rack (D-055); `separator` top down; find-group on the group it found or a seed it marks isolated; find-neighbor after a new relation; build-obj-2 on the group it built |
| compare-roles | CMP-RL | scout | 30 | reads the comparison roles of an item once a midline stands: the images under the axis, the echoes, the twin | find-roles |
| propose-role-cp | PR-RCP | scout | 60 | consumer C1: chooses one role pair under a(T) by weight and posts one create-cp into the unchanged chain, with one check-roles | nine role nodes top down; find-roles |
| recast-role | RC-RL | search | 40 | the re-cast (§6): publishes the tiers, tags every candidate COUNTERPART, posts one create-cp per first-tier candidate; fizzles while the rows are unread | `counterpart`, `row`, `column` top down; find-roles |
| read-worldview-roles | RD-WVR | worldview | 50 | reads the Worldview roles after a change: the axis, the keys, the barriers and their claims, dropping stale ones; posts C4's fight when it read a barrier | `symmetry` top down; cp-compete after a Worldview change |
| check-roles | CHK-RL | scout | 20 | the role checker: samples a tag, re-rates it, may break it | `role` top down; propose-role-cp and recast-role |

A codelet that reads nothing fizzles without a draw, so a run in which no role beyond first
sight is read keeps its draws: the quick start's run (`surround-05` seed 3) is one.

## 6. The re-cast

The re-cast (D-093, `roles.recast`) is the one place a role is carried across the table, the
transport of French's schema: the cup in front of Henry becomes the cup in front of person X
and, under pressure, slips to the glass in front of Eliza [F95 p. 6]. The touched object's
**signature** (`roles.signature_of`) is read from its *standing* tags, so a discarded role is
never re-cast: its signature roles (member, anchor, connector, corner, loner, container,
figure, ground), `end` or `middle` as a member, `sidekick:<category>` for each sidekick
naming it, and `group:<categories>` for its gestalt group. The candidates are the objects
tagged with the ROW opposite the touched object's; both rows must be read, else nothing. A
candidate *plays* the signature when it carries every word of it, and may carry more (an
exact match would never let the glass of Surround 5, a connector too, match the cup). Five
tiers, weights `role_recast_tiers` (8, 4, 2, 1, 0.5, a priori and halving): the image under
the standing axis that plays the signature; one that plays it in the touched object's
column; the TWIN (the touched object's type across the midline); the rest of the row; and
last a candidate CLAIMED by a standing correspondence. A tie is published with equal weights
and never resolved (`Recast.ambiguous`): the Buridan family is where two candidates are
equally good and the program must still choose [F95 p. 130]. The last re-cast is
`Workspace.recast` and the snapshot's `recast`.

## 7. The eight consumers

Every role has a consumer, and every consumer was built together with the perception it
reads, never perception alone (the author's answer Q9). Each is one module-level function
called through its module, so that the test-only probes can lesion it by patching that one
name (D-104). The rule they share: nothing is gated, and the Worldview decides.

| Consumer | What it reads into | Function | Decision |
|---|---|---|---|
| C1 role-guided proposals | a create-cp between the two items of a role pair across the table (the images under the axis, loner with loner, corner with corner with the point image first, the parts a gap marks off, sidekick with sidekick, figure with figure, ground with ground like with like, echo with echo, the key's pairs; never a pair with a stray at an end), chosen by weight under a(T), into the unchanged chain; without a role pair, French's chain alone | `scouts.role_pairs`, `propose_role_cp` | D-095 |
| C2 the role-weighted partner choice | pick-salient-obj weights each object by its salience times one plus its COUNTERPART tier's weight; with no tag standing the draw is exactly French's | `scouts.recast_weight` | D-096 |
| C3 the answer from group position | when the touched object is mapped only through its group, the member of the corresponding group with its roles (anchor for anchor, connector for connector, end for end, middle for middle), then D-058's group-frame words, then the literal object; `group_position_match` is `roles` or `words`, both answering from group position; examine-cp offers the anchor to the anchor first; find-answer records the tags matched as consumed | `answer.role_member`, `answer_through_group`, `judges.anchor_pair` | D-097, D-058 revised |
| C4 the blocked counterpart | a pick-strong-cp fight on a barrier's correspondence, posted at the read and by check-worldview; the default answer recorded as `blocked` while a CLAIMED tag stands, the claim named in the find-answer line | `judges.barrier_fights` | D-101 |
| C5 the role slippage | a tenth static factor after French's nine: an identity of kind `role` for each single-table role both ends carry (the selectors of place left out), weighed by the node's activation; a slip with no label for rival roles; parallelism through the label comes with D-043 | `correspondences.role_slippages`, `role_points` | D-098 |
| C6 the placement slippage | French's table-frame slippage, which the reconstruction had never read (spec-inferred): for a correspondence across a perceived midline, the same column feeds mirror symmetry and sameness, a flipped column opposite and diagonal symmetry, the middle column symmetry, scored by factor 4; a tagged COLUMN is read instead of drawn afresh; the axis is read off these | `correspondences.placement_slippages`, `tagged_column`; `worldview.symmetry_axis` | D-082, D-083 |
| C7 role salience | a ninth salience term after French's eight: points per unit of weight of an ANCHOR, CONNECTOR or FIGURE tag, and a stray's region term scaled down; an empty pool reads (0, 1) | `salience.role_reading`, `role_term` | D-100 |
| C8 top-down posting | the role nodes post the consumers' codelets at generation 0, with a priori urgencies | `Config.top_down_table` | D-081 |

What the consumers were found to do is in RESULTS.md §11.4 (the lesion probes): C1 fills the
Worldview and raises the literal answer where nothing reads the pairs back into the answer
(the cost item 03 named, which C3 was to pay off); COLUMN moves the draws most and the
answers a little; the barrier is read in about one run in a hundred, because the glass pair
seldom enters the Worldview and the re-cast seldom stands before the answer. These are
findings, not tunings: no weight was moved after a measurement.

## 8. What a run shows: events, snapshot, trace, GUI

Four events were appended to the engine's table and nothing before them changed (D-105;
[`engine-api.md`](engine-api.md)): `RoleTagged`, `RoleDiscarded` (the reason: `lost to rN`,
`broken`, `structure gone`), `RoleRecast` and `RoleConsumed` (the consumer and its effect).
The snapshot gained `roles` (every tag standing with its readers), `recast` and `axis`. The
text trace prints one `RL` line per role event, `RL tag`, `RL drop`, `RL used` and
`RL recast`, and the find-answer line of a blocked default names the claim
([`trace-format.md`](trace-format.md)). The invariants checked after every codelet (D-077)
cover the pool: names unique, every tag's item and every structure its arguments name
standing, one tag per reading, no two standing rivals, a CLAIMED tag with its BARRIER, the
re-cast the touched object's.

The GUI shows the roles (the roles loop's items 14 and 15; [`gui/panes.md`](gui/panes.md)):
the Roles pane (P14, G-060) lists the tags by family with their evidence, readers and, once
lost, when and why; the table's roles overlay (P1, G-061) draws each item's standing codes as
a badge, the midline band with the axis word, a bracket across each gap, the images, the
re-cast's tiers, a barrier's claim and a ring on a correspondence a consumer proposed; the
grid's band and the network's role nodes, the timeline's marks and the log's `RL` lines
(G-062) read the same events. The pane and the overlay are GUI design, not French's, and say
so.

## 9. Honesty: how the roles were kept from fitting

The roles changed runs on purpose, and the measures below are what keeps "it matches French
better" from being the product of fitting (D-078; RESULTS.md's opening):

- **The lock, the goldens and the log.** `config_defaults.lock`, the golden fingerprints and
  traces (`tests/data/golden/`) and the GUI's determinism references were regenerated only
  for an intended change of runs, never to hide a bug, and every such change has a dated
  entry in RESULTS.md's re-interpretation log naming the item, the decisions, the files, the
  changed lock fields, the old and new fingerprints, and the item's carriers at 200 runs
  before and after. A change of an answer's count is read as gained or lost by a two-sample
  z beyond 2, never as a net number.
- **A priori defaults.** Every role knob was set and written in its decision before any
  measurement; no chapter 5 number chose a default. Where a default would have to move after
  a measurement it is a logged decision with a re-check on held-out data, and none was moved.
  The two calibration scans of D-076 were re-run on the roles engine as a measurement only;
  the profiles keep v1's values (RESULTS.md §9).
- **The baseline.** v1 is commit `e38d6e0`: its records are `results/v1/`, and comparisons
  that need a fresh run re-run that commit from a throwaway `git worktree` under `/tmp`
  through `tools/run_on_baseline.py`, removed afterwards.
- **Registered predictions.** `predictions.md` was written before any role code, per role,
  per consumer, with the lesion and variants expectations; nothing above its `## Scored`
  heading has changed since, and the scoring below it reports hits and misses alike.
- **Held-out data.** The variants bench (D-102; eight variants of every problem, run with the
  original's seeds and relabelled through the object map) was built on v1 before any role
  code and run on both engines at 500 runs; the headline is that the roles lost nothing on 38
  of 39 variant-and-family cells (RESULTS.md §11.5). Random realistic tables (D-068) run with
  every invariant checked (§11.6).
- **The oracle probe.** Hand-read roles for four carriers (`tests/data/oracle_roles.json`)
  handed to the engine in place of its detectors, at 200 runs: a measurement of what
  perception costs (§11.3), never a gate on the loop.
- **Lesions as test-only probes.** No user-facing switch exists; `tests/role_probes.py` and
  `tools/role_probes.py` patch one module-level function per consumer or detector with
  `unittest.mock.patch`, and nothing under `tabletop/` imports them (D-104). Each lesion's
  expected degradation was registered first (§11.4).
- **The guards in the code.** `tests/test_conventions.py` keeps every role knob refused at
  zero or below and no boolean about roles in `Config`; `tests/test_roles.py` keeps the
  registry at 23 and the no-switch rule live; the citation checker verifies every quotation
  on its page.

## 10. The knobs

Every role knob lives in `Config` with the others, commented with its decision, and is
refused at zero or below by `Config.__post_init__` (the `role` rows of the weight tables
included); the full table is in [`README.md`](../README.md), "The knobs".

| Knob | Default | Decision |
|---|---|---|
| `role_weight_points` | 10 | D-079: a tag's points per unit of weight |
| `role_node_points` | 30 | D-079: the points from its node's activation over 150 |
| `role_story_points` | 20 | D-079: the points per Worldview correspondence using its item |
| `role_break_scale` | 100 | D-079: the checker breaks a tag with chance one minus strength over this |
| `role_pair_floor` | 1 | D-095: the floor of a pair's weight in C1's choice |
| `role_recast_tiers` | (8, 4, 2, 1, 0.5) | D-093, D-101: the tier weights of the re-cast, C2's multipliers |
| `cp_static_weights["role"]` | 10 | D-098: the role slippage's weight, the tenth static factor |
| `salience_weights["role"]` | 5 | D-100: the role term of salience, per unit of weight |
| `stray_salience_scale` | 0.5 | D-100: the scale on a stray's region term |
| `urgencies` rows | 60, 20, 30, 30, 50, 40 | D-081: propose-role-cp, check-roles, find-roles, compare-roles, read-worldview-roles, recast-role |

Decisions with no tunable value say so in their Knob line and why: the detectors reuse
D-004's blur and D-022's zero gap rather than add distances of their own, the axis and the
keys jolt by D-016's amounts, and the Slipnet's role links are rows of the link table.

## 11. What came last, and where the numbers are

- **CONTAINER (S3) and consumer C3** were the last built (item 06, reopened after the
  final review, 2026-10-14): `roles.pending()` is empty, D-058's boolean is gone and
  `group_position_match` answers from group position under both of its values. The
  predictions' scores say "not built" where they were written before it and re-score the
  rows in their section S10; RESULTS.md §11's sentences carry a dated note and its
  re-interpretation log the item's entry. What the item measured: the group case comes in
  about 1 percent of runs and the roles' reading agrees with the words' there; the setting
  is read in about 5 percent of runs, the holder in 99 percent of the on-saucer variant's;
  no answer moved.
- **The numbers**: RESULTS.md §11.1 every result set as French | v1 | roles (14 of the 35
  sets moved beyond z = 2, 7 gained and 7 lost); §11.2 the predictions scored (invariance and
  robustness hit, the French-frequency claims missed); §11.3 the oracle probe; §11.4 the
  lesions; §11.5 the variants; §11.6 random tables; §11.7 roles over time; §11.8 the
  findings traced to decisions, chief among them that the roles stand in most runs and too
  late for the answer, which the find-answer schedule ends at about 80 codelets.
