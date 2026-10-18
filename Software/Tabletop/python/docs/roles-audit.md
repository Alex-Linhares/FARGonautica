# The roles audit: v1's decisions and knobs read in role vocabulary

Written at item 01 of the roles loop (2026-10-14, on commit `4657583`, the v1 engine of
`e38d6e0` with no role code), before any role is perceived. It reads every decision of
D-001 to D-077 and every knob of `tabletop/config.py` in the vocabulary of D-078: a **role**
is the function an item plays in the analogy; a **consumer** is what reads it. The question
for each decision is whether v1 already has the role, under another name, and whether
anything reads it. Three verdicts:

- **covered**: v1 perceives the function and something reads it; the role names it, gets a
  tag and a node, and its consumers read the tag where v1's reader read the raw fact;
- **parameter gap**: v1 has the function as a constant, a knob or a hard-coded selector,
  where French describes a perception; the role replaces the constant by a reading;
- **role gap**: French describes the function and v1 has nothing, or has the perception and
  no reader (a description built and read by no proposer).

For each, the module and the function, and the role or consumer that will read it. The
roles (G1-G3, S1-S6, L1-L5, K1-K2, A1-A7) and the consumers (C1-C8) are those of
`docs/predictions.md` §2-§3; every one becomes a decision after D-078, with its knob.
Nothing is changed by this audit: it says where the role code of items 03-10 goes and what
it replaces.

## 1. The six places the item names

### D-052, item 24: the touched object as the only selector (role gap: COUNTERPART, C1, C2)

- **Where:** `tabletop/codelets/scouts.py`, `pick_salient_obj`. Since item 24 it picks an
  object other than the touched one, by salience under a(T), and posts a create-cp between
  it and the touched object, "the current object corresponding to the touched object" being
  what the run is after [F95 p. 105]. Groups are picked by `pick_salient_subgp` and the
  searches, the pairs inside a group correspondence by `examine_cp`.
- **What it does to the engine:** every object correspondence has the touched object as an
  end, so any two of them are rivals (D-044's shared end) and the Worldview holds at most one
  correspondence in 98 percent of runs (RESULTS.md §9.2). The coherence terms (D-042), the
  parallelism (D-043), the `buridan` knob (D-076) and the usurping rule's group half (D-044)
  have nothing to read. French's correspondences that "are nowhere in the vicinity of" the
  touched object [F95 p. 36] are never proposed.
- **In role vocabulary:** v1 has one selector, given, not perceived: the touched object. The
  partner is chosen by salience, which has no notion of the part the partner would play. The
  re-cast (K1 COUNTERPART, `roles.recast`) is the missing step: the touched object's
  signature carried to the other row in tiers. C2 weights `pick_salient_obj`'s choice by the
  re-cast weights (salience times one plus the weight). C1 (`propose-role-cp`) proposes the
  pairs that do not contain the touched object at all: sidekick with sidekick, echo with echo,
  anchor with anchor, corner with corner, loner with loner, figure with figure, ground with
  ground, each unmapped item with its image under the axis. Both enter the existing chain
  (create-cp, cp-compete, build-cp-descrip) unchanged.
- **Knobs touched:** `urgencies` (five rows added for the role codelets, C8), `top_down_table`
  (the role nodes' rows), `initial_codelets` (one find-roles, D-055 revised).

### D-032: positions as constants (parameter gap: MIRROR_IMAGE, POINT_IMAGE, CORNER, SYMMETRY_AXIS)

- **Where:** `Config.region_preference` (diagonal 1.0, opposite 0.7, near 0.4, other 0.15,
  each times the salience weight 40); `geometry.diagonal_region`, `opposite_region`,
  `region_relation`, `region_weight`; `salience.region_term` (factor 4 of salience);
  `perceive.region_preferences` and `across` (D-071's "opposite" descriptor); the `focus`
  of every scout's look (D-023). `geometry.diagonal_point` and `opposite_point` reflect a
  point the same two ways; the entry says no codelet uses them (the Potelbat baseline,
  `potelbat.py`, does).
- **What it does:** "diagonally opposite" and "directly opposite" are a fixed order of
  regions relative to the touched object's region, "the most favored region is diagonally
  opposite the touched object, and the next most preferred region is directly opposite the
  touched object" [F95 p. 66]. The preference is a constant per relation: it does not know
  whether the object there is the touched object's counterpart, whether the Worldview's
  other correspondences are mirror or point, or whether the touched object is a corner
  object at all.
- **In role vocabulary:** the two regions are the point image (G2) and the mirror image
  (G1) of the touched object, read as a comparison of two items by `compare-roles` from
  their drawn regions (`geometry.placement`, item 04), for any pair and not only the touched
  object's. The order "diagonal first, then directly opposite" [F95 pp. 66, 93] stays as C1's
  order for corner pairs (L3). The axis (G3, `worldview.symmetry_axis`) replaces the fixed
  order by the placement most of the Worldview's own correspondences share, the touched
  object's left out. The constants stay: salience's region term is French's and is untouched;
  the roles add a reading beside it.
- **Knobs touched:** none of D-032's; the role knobs are new.

### D-036: static points (covered in part; role gap for the role factor and the table frame)

- **Where:** `tabletop/correspondences.py`: `static_terms` (nine terms, one function each),
  `kind_points` (factor 2, the "group" and "isolated" nodes), `end_middle_points` (3),
  `position_points` with `symmetry_scale` (4), `type_points` (5, `type_weight`), the count,
  ISA, same-members and orientation terms (6-9); `Config.cp_static_weights`, `type_weight`.
- **Covered:** factor 2's "isolated" node is the LONER's node (L4): v1 gives object-object
  pairs 5 points when "isolated" is active, which is a reading of the loner role by the
  Slipnet alone. Factor 8, the same multiset of types, is the ECHO relation (A6) read on two
  groups already mapped; the role reads it before the mapping exists and proposes it (C1).
  Factors 3 and 4 inside a group correspondence read MEMBER's group-frame words (A1).
- **Role gap, the table frame:** factor 4 is computed from `salience.position_words`, the
  group frame only (D-029). A top-level correspondence between two objects in no group, or
  between two groups, carries no position slippage, so "mirror symmetry" and "diagonal
  symmetry" are fed only from inside groups. C6 adds the placement slippage in the table
  frame (`correspondences.placement_slippages`), scored by the same factor 4 with the same
  `position` weight and `symmetry_scale`; the omission is named in its decision (Kind
  spec-inferred, French p. 85).
- **Role gap, the tenth factor:** nothing in v1 gives points for two ends playing the same
  part (anchor to anchor, connector to connector, loner to loner). C5 adds `role_points`,
  `cp_static_weights["role"]` (a priori 10) times the role node's activation over 150, the
  form of French's position slippage, "The value of this slippage depends on how active the
  'diagonal symmetry' and 'opposite' nodes are" [F95 p. 68]. A mismatch (member to loner) is
  a slip with no points.
- **Knobs touched:** `cp_static_weights` gains `("role", 10)`; `type_weight` stays (the
  `human` profile's knob, D-076).

### D-058: a touched object mapped only through its group (parameter gap: MEMBER, ANCHOR, CONNECTOR, C3)

- **Where:** `tabletop/answer.py`: `group_correspondence` (the first Worldview group
  correspondence whose first end holds the touched object), `member_in_same_position` (the
  member of the other group with the same group-frame words), `answer_for`;
  `Config.answer_from_group_position` (False: the literal object).
- **What it does:** a boolean chooses between the literal object and the member with the
  same words (left end for left end, middle for middle). Neither reading knows what the
  touched object is to its group: the cup among the silverware of Surround 5 is a middle,
  and so is every other middle of Eliza's row.
- **In role vocabulary:** the answer is the member of the corresponding group with the same
  roles: anchor for anchor (A3), connector for connector (S4), then end for end and middle
  for middle (A1), then D-058's words, then the literal object. That is C3, `answer.role_member`.
  The boolean becomes `group_position_match: "roles" | "words"`, and both values answer from
  group position (the roles loop's rule: no knob may be a switch). The case itself occurs in
  v1 only after a group correspondence, which is never built (0.0 percent on Surround 5); the
  echo and setting pairs of C1 make it reachable.
- **Knobs touched:** `answer_from_group_position` removed; `group_position_match` added.

### D-042 and D-043: consumers with nothing to read (covered once C1 exists: SIDEKICK, ECHO, MEMBER; C5, C6)

- **Where:** `tabletop/worldview.py`: `coherence_pairs`, `pair_status`, `together`,
  `holds` (D-042: two top-level object correspondences whose first ends lie in one gestalt
  group are a pair, coherent when their second ends do too); `parallel`, `parallel_labels`,
  `parallel_pairs` (D-043: two correspondences at one level sharing a slippage label);
  `rate`; `Config.incoherence_penalty` (40, the `buridan` profile's knob), `coherence_reward`
  (10), `parallel_points` (5).
- **What they read in v1:** nothing. No rating has ever held a coherent or an incoherent
  pair (RESULTS.md §9.2: 240 runs over four problems), and the `buridan` scan is exactly
  flat from 40 to 300. Parallelism has one correspondence to compare with nothing.
- **In role vocabulary:** coherence is sidekicks mapping beside their anchors: Buridan 1's cup
  pairs (A2 SIDEKICK, weight 2 for the doubled pair) proposed by C1 beside the touched spoon's
  correspondence give `pair_status` its first pairs. The group correspondences of A6 ECHO and
  S3's setting form give the usurping rule (D-044) its group half. Parallelism gets two new
  label families: the placement labels of C6 ("mirror symmetry", "opposite") and the role
  labels of C5 (the role nodes). The three knobs stay as they are; `buridan`'s 40 is not
  re-fitted in the roles loop (its scan is re-run once as a measurement).
- **Knobs touched:** none.

### D-029: the table frame described and not built (role gap: MIDLINE, ROW, COLUMN; C6)

- **Where:** `tabletop/describe.py`, `table_frame(world, item)`: the column (`left`,
  `middle`, `right`) and the row (`above` for Henry's row, `middle`, `below` for Eliza's)
  of the region an item's centre (a group: its centroid) is drawn in, through the blur,
  afresh each time; tested in `tests/test_describe.py`; **called by nothing in
  `tabletop/`**. `correspondences.position_slippages` reads `salience.position_words`, which
  returns the group frame of live groups only and an empty list for a group or an ungrouped
  object; `describe.group_frame` is the frame that is used.
- **What French describes:** across the table the same words name the sides, "where the
  terms 'left' and 'right' refer to the sides of the table as a whole. Both of these identity
  slippages will cause activation to be sent to the 'mirror symmetry' concept node in the
  Slipnet" [F95 p. 85]; his cp1 and cp2 of the chapter 4 run are mirror-symmetric through
  the table frame [F95 pp. 84-85]. D-029's choice says a top-level correspondence uses the
  table frame; the code never does.
- **In role vocabulary:** the table frame is three roles. S1 MIDLINE is the middle row band
  perceived as a separator when no gestalt group's member lies in it (`describe
  .midline_perceived`, item 04); L1 ROW is the row word (`geometry.row_of`); L2 COLUMN is
  the column word (`geometry.column_of`). C6 gives a top-level correspondence across a
  perceived midline a placement slippage from the columns (an identity is mirror, a flip is
  point, the middle column is symmetry), fed into factor 4 as D-029 intended. `table_frame`
  is finally called, by `find-roles`. The Kind of that decision is spec-inferred, naming the
  omission.
- **Knobs touched:** none of D-029's (it has none); the placement's points use D-036's
  `position` weight.

### Descriptions built and read by no proposer (role gap: a list)

`Workspace.descriptions` (D-028, D-029: `describe`, `descriptors`, `has`) is written by seven
functions and read by two: `snapshot.py` (for the GUI and the traces) and `invariants.py` (every
description belongs to a structure that exists). No codelet, no proposer and no evaluation
reads a description from the Workspace list; each reader goes to the structure's own field
instead, or recomputes the fact. The descriptions, with the role that will read each:

| Description | Written by | Read in v1 by | Role that reads it, consumer |
|---|---|---|---|
| `opposite` on an object seen across the table (D-071) | `perceive.look` (the jolt to "opposite" is the effect that matters) | nobody: the descriptor is never read; the node is read by `symmetry_scale` and the top-down table | G1 MIRROR_IMAGE and G2 POINT_IMAGE read the two items' regions directly (`geometry.placement`); the descriptor stays as the Slipnet's feed; C1's image pairs |
| `isolated` on an object a find-group found no neighbour for (D-007, D-022) | `Workspace.mark_isolated` | `Workspace.isolated` (the list, not the descriptor) by `pick_2nd_salient_obj` and `common.argument`; the "isolated" node by `kind_points` | L4 LONER (`describe.loner_of`): an object in no gestalt group, set off by a GAP (S2) or alone on its side; C1's loner pairs, C5's loner-loner identity |
| the count, `one`, `two`, `three`, `many` (D-029, `describe_count`) | `build_obj_desc` (bottom-up groups only) | nobody: `slippages_of` recomputes the count from `members_of` | A2 SIDEKICK's weight n (a run of identical neighbours is one sidekick), A6 ECHO's multisets; no new reader of the descriptor |
| the orientation, `horizontal` or `vertical` (D-028, `notice_orientation`) | `find_group` and `build_obj_desc` | `Group.orientation` by `group_slippages` (factor 9); the descriptor itself by nobody | S3 CONTAINER's setting form (a linear group) and A3 ANCHOR (members on both sides along the axis) read `Group.linear` and the axis of D-028 |
| the kind and category of a bottom-up group (D-024) | `build_obj_desc` | `Group.kind` and `Group.category` by `kind_points`, `group_slippages`, salience's composition term | A6 ECHO by nearest category, K2 TYPE_KEY (the type slippage of a group correspondence) |
| `end`, `middle`, `left`, `right`, `above`, `below` in the group frame (D-028, D-029) | `describe._describe_in_group` (find-end-objs, find-middle-objs) | `Group.positions` by `salience.position_words` and `describe.group_frame`; the Workspace copy by nobody (stale after the group is removed, D-028's note) | A1 MEMBER publishes the tag from the same words; C3 reads them after the roles; C5's member-member identity |
| `level:N` and the slippage pairs on a correspondence (D-043) | `correspondences.describe` (build-cp-descrip) | the snapshot only; `parallel_labels` reads the slippages themselves | C5 and C6 add role and placement slippages, read through `Correspondence.slippages` as now |

Two more perceptions have readers but no role reader:

| Perception | Written by | Read in v1 by | Role that reads it |
|---|---|---|---|
| the neighbours of a member, with their sides (D-027, `find_neighbours`) | find-neighbor, recorded on `Group.neighbours` | `ends_of` (the chain's extremities) and find-neighbor's own follow-ups | A2 SIDEKICK (an immediate neighbour of another type), S4 CONNECTOR (two neighbours of different types, each with its own type beyond), A3 ANCHOR (members on both sides, a unique category) |
| the last drawn region of an object (D-004, `Workspace.regions`) | `Workspace.notice` | `salience.region_of` (the region term) | L3 CORNER (a corner region), L5 STRAY (the midline band, ungrouped), S1 MIDLINE, L1 ROW and L2 COLUMN (`geometry.is_corner`, `in_midline_band`, `row_of`, `column_of`) |

## 2. Every decision, read in role vocabulary

The verdict per decision, in order; the roles in parentheses are those that read what the
decision builds. "Not a role" means the decision is about something roles do not touch.

| Decision | Verdict | Role reading; module and function; reader |
|---|---|---|
| D-001 the table's units | not a role | the geometry the images are computed in (`geometry.diagonal_point`, `opposite_point`: x to 110 minus x, y to 100 minus y) |
| D-002 footprints | role gap (S3 holder) | `objects` boxes; `geometry.gap_between` reads them for D-022; the holder form of CONTAINER reads whether one box contains another object's centre (`describe.holder_of`) |
| D-003 digitising | not a role | the layouts the variants bench transforms (item 02) |
| D-004 the blurry regions | covered (L3, L5, S1, L1, L2) | `geometry.drawn_region`, `Workspace.regions`; every region-reading role inherits the blur, so no distance constant is added |
| D-005 no sides | covered (S1, L1) | the halves are regions only; MIDLINE is perceived from the middle band, never pre-set; nothing is forbidden across it |
| D-006 orientation is input | not a role | Henry's touch is the one pointer and it is given; no direction role is built |
| D-007 the node set | covered (L4) | "isolated" is the LONER's node; the 15 role nodes are added beside it (`slipnet_def`), none on French's screen |
| D-008 depth | covered | the role nodes take the level's depth (50 at level 2, 80 at level 3) |
| D-009, D-010 links and lengths | covered | the ISA links of the role nodes to `role` and of `row`, `column`, `corner` to `position` at the nearest length; the labelled slip links (`mirror symmetry` to `diagonal symmetry` labelled "opposite" is the central one) |
| D-011 label-controlled length | covered (C5, C6) | role and placement labels shrink their links when active, as the other labels do |
| D-012 to D-015 spreading, decay, the range | not a role | unchanged; the role nodes obey them |
| D-016 jolts | covered | a tag jolts its node by `jolt_description`; a comparison role confirmed on both sides, and the axis, by `jolt_structure`; a role slippage entering the Worldview by `jolt_label` (D-030's form); a discarded role jolts nothing |
| D-017 initial activations | parameter gap (G1, G2) | "opposite" starts at 139 and is re-jolted before every update (`focus_jolt`) without any reading of what is opposite what; the images give it a reading; the constants stay |
| D-018, D-019 the trickle, the schedule | not a role | unchanged |
| D-020 top-down posting | covered (C8) | `Config.top_down_table` gains the role nodes' rows (`propose-role-cp`, `recast-role`, `find-roles`, `read-worldview-roles`, `check-worldview` from `barrier`); the symmetries' `pick-strong-cp` rows stay |
| D-021, D-070 context push and pop | covered (G3) | the axis is per context: not carried into a group's subworld (`read-worldview-roles` reads the current context) |
| D-071 "opposite" as a descriptor | role gap (G1, G2) | §1's table: the descriptor has no reader; the images are the reading |
| D-022 the proximity criterion | covered (S2, L4) | GAP reuses `proximity_zero_gap` as the distance beyond which two clusters are parted; LONER reads `Workspace.isolated`; the coin's stochasticity is inherited |
| D-023 find-group's search | covered | `find-roles` is a follow-up of find-group (C8) |
| D-024 bottom-up groups | not a role (out of reach) | the build chain's generations are untouched; the echo and setting pairs bypass it by pairing gestalt groups |
| D-025 group strength | not a role | unchanged |
| D-026 subgroups absorbed | not a role | unchanged; `french-faults` restores the fault on top of roles |
| D-027 neighbours | role gap (A2, S4, A3) | §1's second table: `Group.neighbours` with sides, read by `ends_of` only |
| D-028 ends, middles, orientation | covered (A1, S3, A3) | the group frame's words are MEMBER's; `Group.linear` and the axis are the setting's and the anchor's |
| D-029 two frames | role gap (S1, L1, L2, C6) | §1: `describe.table_frame` never called |
| D-030 which slippages feed which symmetry | covered (C5, C6) | `Slippage.labels` carry the nodes a slippage feeds; placement slippages label "mirror symmetry" and "sameness" or "opposite" and "diagonal symmetry"; role slippages label their role node |
| D-031 salience weights | parameter gap (C7) | eight terms, none about the part an object plays; C7 adds the `role` term (a priori 5 per unit of weight) for ANCHOR, CONNECTOR and FIGURE and scales a STRAY's region term by `stray_salience_scale` (0.5) |
| D-032 region preference | parameter gap (G1, G2, L3, G3) | §1 |
| D-033 proximity to the touched object | not a role | unchanged |
| D-034 temperature in salience choices | covered (C1, C2) | the role-weighted choices draw under the same a(T) (D-060) |
| D-035 creation conditions | covered (C1) | every role-proposed pair passes `creatable` unchanged |
| D-036 static points | covered in part, role gap (C5, C6) | §1 |
| D-037 dynamic points | covered | unchanged; the hobj and neighbour terms read the role-proposed correspondences like any other |
| D-038 bonus and penalty | not a role | unchanged |
| D-039 to D-041 test, winner, removal | covered (C1) | a role-proposed correspondence is tested, fought and faded like any other; the test-only probes check that the chain is unchanged |
| D-042 coherence | consumer with nothing to read (A2, A6, C1) | §1 |
| D-043 parallelism | consumer with nothing to read (C5, C6) | §1 |
| D-044 usurping | covered (S5, S6, C4) | `conflicts`, `rivals_of`, `reaches_outside`: the blocking is D-044's rivalry; BARRIER and CLAIMED are read off its results (`worldview.barrier_and_claimed`); the setting's holder and content are kept together by the inside-only rule |
| D-045, D-046 the group-object penalty, the group preference | not a role | unchanged; the `french-faults` values stay |
| D-047 to D-050 temperature | not a role | unchanged; prediction 9 says more structure makes T no better |
| D-051 the catalogue | covered (C8) | five rows appended after `find-answer`, marked as re-interpretation; `catalogue.ROLES` (the urgency bands) renamed `BANDS` so that "role" means one thing |
| D-052 urgencies, follow-ups | role gap (K1, C1, C2, C8) | §1; the five new raw urgencies are a priori |
| D-053 maximum age | not a role | unchanged |
| D-054 generation 0 for top-down codelets | covered (C8) | the role nodes post the consumers at generation 0, so they reach the rack ahead of the find-answer flood |
| D-055 the initial rack | covered (C8) | one `find-roles` in the initial rack |
| D-056, D-057 the find-answer schedule and announcement | covered (C4) | `AnswerGiven.through` gains `blocked`; the schedule is untouched (prediction 9's time cost is measured, not pre-empted) |
| D-058 group position | parameter gap (A1, A3, S4, C3) | §1 |
| D-059 several objects under one label | not a role | the variants bench relabels through the object map (item 02) |
| D-060 one law for temperature | covered | every role draw passes through it |
| D-061 the run cap | not a role | prediction 15: no capped run |
| D-062 the structure value | covered | predictions 5 and 6 read it for the answers given through a Worldview |
| D-063 the default answer | covered (C4) | the touched object stays the default; when its obvious counterpart is CLAIMED the record says `blocked` |
| D-064 the generator | covered | every role draw goes through the one generator; the fingerprints change only with a log entry |
| D-065 events and traces | covered | four events appended to `EVENT_TYPES` (tagged, discarded, re-cast, consumed; a broken or replaced tag as well, the roles loop's answer D1); `RL` lines in the text trace |
| D-066 profiles | covered | all four profiles run with roles; no `roles` profile |
| D-067 the Potelbat | not a role | `potelbat.py` reads `diagonal_point` and `opposite_point`, the only v1 reader of the point reflections |
| D-068 random tables | covered | the role statistics on random tables (prediction 15) |
| D-069 member salience in group strength | not a role | unchanged |
| D-072 to D-075 the benches | covered | the variants bench joins them (item 02); the three-column tables (French, v1, roles) through `bench/results.py` |
| D-076 the two calibrations | consumer with nothing to read (A2) | `buridan`'s flat scan is D-042's emptiness; re-run once as a measurement, not re-fitted |
| D-077 the invariants | covered | four role invariants added: every tag's item exists; every tag's evidence is in the Workspace; CLAIMED implies a BARRIER; no tag on a removed structure |

## 3. Every knob, read in role vocabulary

The fields of `Config`, grouped as the file groups them. "Stays" means the roles read it or
leave it alone; "gains" names the rows a table gains; "new" names the role knobs, every one
validated above zero by `Config.__post_init__` (a zero would be a switch).

| Knobs | Verdict | Role reading |
|---|---|---|
| `profile` | stays | no `roles` profile; all four run with roles |
| `region_blur_width`, `region_blur_max` | stays (inherited) | every region-reading role (S1, L1-L3, L5, G1-G2 through placement) draws through the blur; no new distance constant |
| `depth_by_level`, `label_shrink`, `max_path_links`, `spread_constant`, `spread_radius`, `decay_rate` | stays | the role nodes obey them |
| `jolt_structure`, `jolt_description`, `jolt_label` | stays | the three sizes of a role's jolts (D-016's reading above) |
| `initial_opposite`, `initial_touched_type`, `focus_jolt` | stays (parameter gap, D-017) | the images give "opposite" a reading; the constants are untouched |
| `label_trickle`, `label_trickle_threshold`, `slipnet_update_every` | stays | unchanged |
| `top_down_table` | gains rows | the role nodes' rows (C8) |
| `top_down_rise`, `top_down_threshold` | stays | the role nodes post by the same rise rule; "active" for the kind factor is unchanged |
| `pop_fixed_amount`, `pop_restores_spread`, `context_mechanism`, `push_reset_set` | stays | the axis is per context (G3) |
| `proximity_full_gap`, `proximity_zero_gap` | stays (reused) | GAP (S2) reuses the zero gap |
| `group_stage_pass`, `shared_isa_max_length`, `group_strength_weights`, `member_salience_in_group_strength`, `keep_all_subgroups`, `ends_from_chain` | stays | unchanged; the `french-faults` values stay |
| `salience_weights` | gains `("role", 5)` | C7's role term for ANCHOR, CONNECTOR, FIGURE |
| `middle_over_end_factor`, `group_salience_points`, `region_preference`, `proximity_reach` | stays | the region constants are D-032's parameter gap; left as French's |
| `cp_static_weights` | gains `("role", 10)` | C5's tenth factor; the placement slippage (C6) uses the existing `("position", 10)` |
| `type_weight` | stays | the `human` profile's knob; prediction 7's reading of Surround 8-9 depends on it |
| `group_object_penalty`, `cp_dynamic_points`, `win_bonus`, `loss_penalty`, `removal_threshold`, `losers_destroyed` | stays | unchanged; `losers-destroyed` should stop being inert (predictions §5) |
| `incoherence_penalty`, `coherence_reward`, `parallel_points` | stays (consumers with nothing to read) | A2 and A6 through C1 give them pairs |
| `temperature_alpha`, `temperature_beta`, `temperature_gamma`, `temperature_fixed`, `dq_signed`, `dq_window`, `wv_window`, `initial_temperature`, `q_floor`, `temperature_update_every` | stays | prediction 9 |
| `urgencies` | gains five rows | `find-roles` 30, `compare-roles` 30, `recast-role` 40, `propose-role-cp` 60, `read-worldview-roles` 50, a priori (C8) |
| `urgency_floor`, `rho`, `exponent_at_zero`, `exponent_at_hundred`, `max_codelet_age`, `coderack_capacity` | stays | unchanged |
| `initial_codelets` | gains one row | one `find-roles` |
| `failed_test_removes`, `answer_post_rate`, `answer_post_power`, `answer_urgency_max`, `answer_urgency_power` | stays | the find-answer flood is untouched; the consumers are posted at generation 0 against it |
| `answer_from_group_position` | removed | becomes `group_position_match: "roles" \| "words"` (C3) |
| `max_codelets` | stays | prediction 15: no capped run |
| new: the role weights, the re-cast tier weights and the claimed tier weight, the stray salience scale, the role codelets' urgencies, the role slippage weight, the role salience weight | new | each in its own decision after D-078, with its a priori value and its reason; none may be zero or negative |

## 4. Summary by role

Where each of the 23 roles stands in v1, in one line each.

| Role | v1 verdict | What v1 has | The reader to come |
|---|---|---|---|
| G1 MIRROR_IMAGE | parameter gap | `opposite_region`, a region preference of 0.7 | `compare-roles`; C6, C1, C2 |
| G2 POINT_IMAGE | parameter gap | `diagonal_region`, a preference of 1.0; the "opposite" descriptor | `compare-roles`; C6, C1, C2 |
| G3 SYMMETRY_AXIS | role gap | the symmetry nodes, fed from inside groups only | `read-worldview-roles`; C6, C1, C2 |
| S1 MIDLINE | role gap | the middle row of regions, never read as a separator | `find-roles`; C6, C1 |
| S2 GAP | role gap | `proximity_zero_gap` as a grouping limit only | `find-roles`; C1, L4 |
| S3 CONTAINER | role gap | footprints for the gap test; linear groups | `find-roles`; C1, C3 |
| S4 CONNECTOR | role gap | neighbours with sides, read by `ends_of` only | `find-roles`; C5, C1, C3 |
| S5 BARRIER | role gap | D-044's rivalry, with nothing that names the blocker | `read-worldview-roles`; C4 |
| S6 CLAIMED | role gap | as S5 | `read-worldview-roles`; C2, C4 |
| L1 ROW | role gap | `table_frame`'s row word, never called | `find-roles`; C2, C1 |
| L2 COLUMN | role gap | `table_frame`'s column word, never called | `find-roles`, `recast-role`; C2, C6 |
| L3 CORNER | parameter gap | the corner regions as salience constants | `find-roles`; C1 |
| L4 LONER | covered in part | `Workspace.isolated`, the "isolated" node (factor 2), pick-2nd-salient-obj | `find-roles`; C1, C5 |
| L5 STRAY | role gap | junk is looked at like any object (RESULTS.md §5.4) | `find-roles`; C7, C1 |
| K1 COUNTERPART | role gap | the touched object as the only selector (D-052) | `recast-role`; C2, C1 |
| K2 TYPE_KEY | role gap | type slippages of group correspondences, which are never built | `read-worldview-roles`; C1, C5 |
| A1 MEMBER | covered | the group frame's words (D-028, D-029) | `find-roles` publishes the tag; C3, C5 |
| A2 SIDEKICK | role gap | neighbours with types, unread | `find-roles`; C1, C5 |
| A3 ANCHOR | role gap | neighbours and D-009 categories, unread together | `find-roles`; C1, C3, C5, C7 |
| A4 FIGURE | role gap | nothing names the only object of its type on a side | `find-roles`; C1, C5, C7 |
| A5 GROUND | role gap | sameness runs inside gestalt groups, unread as a ground | `find-roles`; C1 |
| A6 ECHO | covered in part | factor 8 (the same multiset of types), read only on a pair already mapped | `compare-roles`; C1, C5 |
| A7 TWIN | covered in part | the type identity slippage (factor 5) | `compare-roles`; C2 |

The pattern is the one D-078 names: v1 has most of the ingredients (neighbours, groups, the
isolated mark, the regions, the same-multiset factor, the rivalry) and no vocabulary for the
part an object plays, so its readers are selectors and constants. The role code of items
03-10 adds the readers; it changes no v1 constant, and every change of runs it causes is
logged.
