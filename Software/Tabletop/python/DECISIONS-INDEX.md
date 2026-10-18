# Decisions: the index

The index of [`DECISIONS.md`](DECISIONS.md)'s entries: by number, with each entry's Kind, and
by kind. It is kept in its own file so that the file of entries stays within the size the
repository allows; the entries themselves, in subject order, stay in `DECISIONS.md`. Since
v3 (D-114) the paragraphs on how an entry is written, the seven fixes, the two calibrations
and the law of temperature are here too. An entry's number is stable; a revised entry keeps its row.

## How the entries are written

These paragraphs stood at the head of `DECISIONS.md` until v3 (D-114, 2026-10-16), when they
moved here to keep that file within the size the repository allows; "below" in them means
the entries of `DECISIONS.md`.

**The seven fixes of French's faults** (ANSWERS.md 2; SPEC.md 6b), and the knob value that
restores each for the `french-faults` profile: D-026 (`keep_all_subgroups = True`), D-045
(`group_object_penalty = 30`), D-046 (`group_salience_points = 15`), D-021
(`pop_restores_spread = False`), D-070 (`push_reset_set = "symmetries"`), D-069
(`member_salience_in_group_strength = 0.2`), D-028 (`ends_from_chain = False`). Each fix's
size was chosen from French's words before any bench ran (item 10, 2026-10-13); v3's fitted
defaults (D-114) take 5 whole and 2, 3, 6 in part.

Entries D-001 to D-068 are numbered as the plan listed them; D-069 and D-070 are two of
French's faults found by the search of SPEC.md 6b. Every entry was written before the code
that implements it; where the code forced a detail, an "As built" paragraph says so. Each
entry has these parts, in this order:

```
### D-NNN: Subgroups kept
- **Kind:** spec-inferred | screen-dump-inferred | digitization | Copycat choice | engineering |
  simplest | extension | FIXED (ANSWERS 2) | calibrated (ANSWERS 7) | re-interpretation
- **Sources:** what the sources say, with tags and quotations, and where you looked and found
  nothing.
- **Choice:** what the code does (the default).
- **Reason:** why (French's words; the screen dumps; the simplest reading).
- **Knob:** `Config.keep_all_subgroups` (or "none").
- **Alternatives:** what else fits the sources.
```

A fix of one of French's faults (ANSWERS.md 2) also says, under **Fault:**, what French's
program did, with the page, and which value of the knob restores it (for the `french-faults`
profile). A decision made while a question for the author is open is marked "pending the
author".

**From D-078 on, the roles.** D-001 to D-077 are the reconstruction's ("v1", the engine at
commit `e38d6e0`, before roles). D-078 to D-106 are the re-interpretation's: roles, the
functions objects play in the analogy, built into the engine and always on (D-078); Kind
**re-interpretation** where the choice goes beyond French's program, **spec-inferred** where
French describes the mechanism and the reconstruction left it out. Every one has a
**Knob:** line ("none" and why, where nothing is tunable); defaults were set a priori. An
earlier entry the roles revise gains an "As revised (loop0007, item NN)" paragraph; nothing
earlier is deleted. The roles' numbers follow the plan's map, by subject, so the entries
stand in the items' order (D-102, of item 02, precedes D-079).

**From D-107 on, the fast engine.** Two engines run one model: the *compiled* engine
(Cython, `tabletop/_native/`), the reference, whose runs, goldens and bench numbers are the
package's; and the *python* engine, the readable specification and the test oracle, run
only on the explicit request `TABLETOP_ENGINE=python`, never as a fallback (D-107). The
compiled code mirrors the Python function for function. Bit-identity between the two is
not required: each engine's own determinism and goldens, and their statistical equivalence,
replace it. Kind **engineering**, or **re-interpretation** where a choice changes what the
model does; every entry has a Knob line; an earlier entry revised gains an "As revised
(loop0008, item NN)" paragraph, nothing earlier deleted.

**From D-114 on, the optimized defaults (v3).** Through v2 every default was set a priori
in its entry and never tuned. On 2026-10-16, 67 of them became the configuration a
cross-validated Bayesian optimization chose against French's result sets and the human data
(fold 1 trial 244 of the study of 2026-10-15/16). The entries keep their a-priori values and
reasons as v2's; D-114 lists every changed knob with its v2 and v3 values, and
`tabletop.config.V2_DEFAULTS` holds the v2 values exactly. On 2026-10-17, 65 of them became fold 1
trial 800 of a second optimization (v4, D-115), the author's choice although its held-out
estimate does not beat v3's; D-115 lists them with their v3 and v4 values, and
`tabletop.config.V3_DEFAULTS` holds the v3 values exactly.

**From D-116 on, the structural Blockage fix (v5).** On 2026-10-17 a study of why Blockage 2, 9
and 10 were lost under v3 and v4 traced the loss to mechanisms, not knob values, and five
decisions changed them: the Hobj status in every rating (D-116, revising D-037), inertia per
rivalry (D-117, revising D-038), the proximity law bounded by Blockage 10 (D-118, revising
D-022: the one default moved, to the bound), the cascade from a BARRIER to its CLAIMED (D-119,
revising D-089) and a time pressure on the answer (D-120, revising D-056, the one new knob).
Each revised entry gains an "As revised (v5, D-NNN)" line; RESULTS.md's v5 section and its
Re-interpretation log give the numbers before and after.

**The two calibrations** (ANSWERS.md 7), as separate profiles after the freeze (item 26,
D-076): `human` changes only `type_weight` (D-036), fitted to 50 by least squares on a
random half of Tables 5.1-5.3; `buridan` changes only `incoherence_penalty` (D-042), fitted
to Buridan 1 under the modified program, where the scan was flat and the default, 40, was
kept. RESULTS.md §9 reports both with the data fitted, the held-out results and the
calibration log.

**The law of temperature** (D-060) is used wherever a decision below says "under D-060":
a weighted choice among items with weights w takes item i with probability
w_i^a / sum w_j^a, and a yes-or-no test with chance q passes with probability
q^a / (q^a + (1 - q)^a), where a = a(T) is the codelets' exponent, 6 at T = 0 and 0.1 at
T = 100 [F95 p. 92]. At T = 0 every choice is deterministic, at T = 100 every choice is
uniform, as French says of "all of the probabilistic processes of the program" [F95 p. 93].

## Index

| Decision | Title | Kind |
|---|---|---|
| D-001 | The table and its units: 110 x 100 | digitization |
| D-002 | Object footprints: the icons' boxes | digitization |
| D-003 | Digitizing the layouts: method, format, ids, uncertainty | digitization |
| D-004 | The nine blurry regions: a draw each time | simplest |
| D-005 | No sides: Henry's and Eliza's halves are regions only | spec-inferred |
| D-006 | Orientation is input, not perception | simplest |
| D-007 | The node set: the 47 of the list, plus "isolated" | spec-inferred |
| D-008 | Depth: three levels, 20, 50, 80 | spec-inferred (levels), Copycat choice (values) |
| D-009 | The link table | spec-inferred |
| D-010 | Link lengths | spec-inferred |
| D-011 | Label-controlled length: a linear shrink | Copycat choice |
| D-012 | Which paths count: one and two links | spec-inferred |
| D-013 | Spreading: activation over distance, buffered | spec-inferred |
| D-014 | Decay by depth | Copycat choice |
| D-015 | The range 1 to 150, clipped | spec-inferred |
| D-016 | Jolt sizes | simplest |
| D-017 | Initial activations: opposite 139, the touched type 51, kept active | screen-dump-inferred |
| D-018 | The small amount a label gets when both its concepts are active | simplest |
| D-019 | The update schedule: every codelet | simplest |
| D-020 | Top-down posting: the node-to-codelet table | spec-inferred (examples and the rise rule) |
| D-021 | Context push and pop; the lossy pop fixed | FIXED (ANSWERS 2) |
| D-022 | The proximity criterion of a gestalt group | spec-inferred |
| D-023 | Find-group's search: a seed and a flood fill | spec-inferred |
| D-024 | Bottom-up groups: three stages, pass by kind | screen-dump-inferred (stages), simplest (values) |
| D-025 | Group strength: five factors on 0-100 | simplest |
| D-026 | Subgroups absorbed, not kept | FIXED (ANSWERS 2) |
| D-027 | Neighbours along the group's axis | spec-inferred |
| D-028 | Ends, middles, orientation; the ends of an L fixed | FIXED (ANSWERS 2) |
| D-029 | Two frames of position | spec-inferred |
| D-030 | Which slippages feed which symmetry | spec-inferred |
| D-031 | Salience weights | simplest |
| D-032 | Region preference beyond the first two | spec-inferred (order), simplest (values) |
| D-033 | Proximity to the touched object | simplest |
| D-034 | Temperature in salience choices | spec-inferred |
| D-035 | Creation conditions | spec-inferred |
| D-036 | Static points | simplest |
| D-037 | Dynamic points | simplest |
| D-038 | The bonus and the penalty | simplest |
| D-039 | The test stage | spec-inferred |
| D-040 | The winner of a competition | spec-inferred |
| D-041 | Removal from the Workspace | screen-dump-inferred (owner), simplest (values) |
| D-042 | Coherence: penalty and reward | spec-inferred (form), simplest (values); `buridan` calibrates (D-076) |
| D-043 | Parallelism within a level | spec-inferred |
| D-044 | Usurping: inside-only when the group correspondence stands | spec-inferred |
| D-045 | The group-to-object penalty, relaxed | FIXED (ANSWERS 2) |
| D-046 | The group-level preference, weakened | FIXED (ANSWERS 2) |
| D-047 | The temperature coefficients | simplest |
| D-048 | Delta-Q: absolute, averaged in over the last six updates | simplest |
| D-049 | Q, the clamp and the start at 80 | screen-dump-inferred |
| D-050 | When T is recomputed | simplest |
| D-051 | The catalogue: the 23 types of the screen | screen-dump-inferred |
| D-052 | Raw urgencies, follow-ups, the floor | simplest |
| D-053 | Maximum age | simplest |
| D-054 | Generation 0 for top-down and find-answer codelets | simplest |
| D-055 | The initial rack | screen-dump-inferred |
| D-056 | The find-answer schedule | simplest, screen-dump-inferred |
| D-057 | What find-answer announces | spec-inferred |
| D-058 | A touched object mapped only through its group | simplest |
| D-059 | Several objects under one answer label | spec-inferred |
| D-060 | One law for temperature in every choice | spec-inferred |
| D-061 | The run cap | simplest |
| D-062 | The structure value reported | spec-inferred |
| D-063 | The default answer: the touched object | spec-inferred |
| D-064 | The generator: splitmix64 | extension |
| D-065 | Typed events and the two traces | extension |
| D-066 | Profiles | extension |
| D-067 | The Potelbat baseline | extension |
| D-068 | Realistic random tables | extension |
| D-069 | A member's salience does not make a group | FIXED (ANSWERS 2) |
| D-070 | What the context push resets | FIXED (ANSWERS 2) |
| D-071 | "Opposite" as a descriptor of objects seen across the table | screen-dump-inferred |
| D-072 | The benches: runs, seeds and the statistics beside French's numbers | extension |
| D-073 | The probe benches: humans, chapter 2, chapter 4, focus, rho, lesions, faults, the Potelbat, and `all` | extension |
| D-074 | The stated-behaviour bench: French's statements as yes-or-no checks over the runs | extension |
| D-075 | The freeze, the results file and its builder | extension |
| D-076 | The two calibrations: the protocol, the grids, the objectives and the split | calibrated (ANSWERS 7) |
| D-077 | The invariants checked after every codelet | extension |
| D-078 | Roles as part of the engine; the two sides | re-interpretation |
| D-079 | The registry, the pool and the tag as a competing structure | re-interpretation |
| D-080 | The Slipnet's role nodes and links | re-interpretation |
| D-081 | The role codelets: types, urgencies, follow-ups and top-down rows | re-interpretation |
| D-095 | Consumer C1: role-guided proposals | re-interpretation |
| D-104 | The test-only probes: lesions and the oracle | re-interpretation |
| D-105 | The role events, the snapshot and the trace | re-interpretation |
| D-082 | Consumer C6: the placement slippage in the table frame | spec-inferred |
| D-083 | The symmetry axis, read off the Worldview without the touched object | re-interpretation |
| D-084 | The single-table detectors: MIDLINE, and the find-roles codelet | re-interpretation |
| D-099 | The image roles, read by compare-roles, and C1's images under the axis | re-interpretation |
| D-087 | ANCHOR, what a setting is laid out around (and CONTAINER, item 06) | re-interpretation |
| D-088 | CONNECTOR, the member that bridges two runs | re-interpretation |
| D-090 | MEMBER and SIDEKICK, the content roles of a group | re-interpretation |
| D-098 | Consumer C5: the role slippage, a tenth static factor | re-interpretation |
| D-097 | Consumer C3: the answer from group position, by the members' roles | re-interpretation |
| D-085 | ROW and COLUMN, the selector roles of the table frame (CORNER at item 08) | re-interpretation |
| D-093 | COUNTERPART: the one re-cast of the touched object's signature, in tiers | re-interpretation |
| D-096 | Consumer C2: the role-weighted partner choice | re-interpretation |
| D-086 | GAP, LONER and STRAY: the parts of a side, and the items of neither | re-interpretation |
| D-100 | Consumer C7: role salience, the ninth term | re-interpretation |
| D-091 | FIGURE and GROUND: the one repeated type of a row and what stands out against it | re-interpretation |
| D-092 | ECHO and TWIN: a group's echoes and the touched object's twin across the midline | re-interpretation |
| D-094 | TYPE_KEY: the type substitution a correspondence between two grounds makes | re-interpretation |
| D-089 | BARRIER and CLAIMED: the claim on the obvious counterpart | re-interpretation |
| D-101 | Consumer C4: the blocked counterpart | re-interpretation |
| D-102 | The variants bench: eight variants of every problem, paired with the original | re-interpretation |
| D-106 | The churn bench: roles over time, and the role codelets against the find-answer flood | re-interpretation |
| D-107 | Two engines: the compiled one the reference, the pure one on request only | engineering |
| D-108 | The update's change report, worked out when read | engineering |
| D-109 | The generator stays splitmix64, in native arithmetic on the compiled engine | engineering |
| D-110 | The build: pinned flags, the stamp, a stale build refused | engineering |
| D-111 | The Slipnet in C arrays, the Python's operations in the Python's order | engineering |
| D-112 | The typed hot spots after the native Slipnet: one, and six plain-Python fixes | engineering |
| D-113 | The equivalence of the two engines: total variation at 200 runs, tolerance 0.25 | engineering |
| D-114 | The optimized defaults (v3): 67 knobs from the Bayesian optimization | re-interpretation |
| D-115 | The second optimization's defaults (v4): 65 knobs from fold 1 trial 800 | re-interpretation |
| D-116 | The Hobj status counts in every rating | re-interpretation |
| D-117 | Inertia per rivalry | re-interpretation |
| D-118 | The proximity law is bounded by Blockage 10 | re-interpretation |
| D-119 | Discarding a BARRIER discards the CLAIMED tag that names it | re-interpretation |
| D-120 | A time pressure on the answer that does not depend on T | re-interpretation |

## Index by kind

The same 116 decisions grouped by their **Kind** line. A decision with a mixed kind is
listed under each of its kinds; the index above gives the qualifier. Every **FIXED (ANSWERS 2)** and every **calibrated
(ANSWERS 7)** entry is here.

**FIXED (ANSWERS 2)** (7): French's own faults, fixed by default and restorable in `french-faults` (SPEC.md 6b).

- D-021: Context push and pop
- D-026: Subgroups absorbed, not kept
- D-028: Ends, middles, orientation
- D-045: The group-to-object penalty, relaxed
- D-046: The group-level preference, weakened
- D-069: A member's salience does not make a group
- D-070: What the context push resets

**calibrated (ANSWERS 7)** (1): the two calibrations French made himself, as the `human` and `buridan` profiles, fitted after the freeze.

- D-076: The two calibrations
- D-042: Coherence

**spec-inferred** (31): read from the text: French's words dictate the mechanism and leave a detail open.

- D-001: The table and its units
- D-005: No sides
- D-007: The node set
- D-008: Depth
- D-009: The link table
- D-010: Link lengths
- D-012: Which paths count
- D-013: Spreading
- D-015: The range 1 to 150, clipped
- D-020: Top-down posting
- D-021: Context push and pop
- D-022: The proximity criterion of a gestalt group
- D-023: Find-group's search
- D-027: Neighbours along the group's axis
- D-028: Ends, middles, orientation
- D-029: Two frames of position
- D-030: Which slippages feed which symmetry
- D-032: Region preference beyond the first two
- D-034: Temperature in salience choices
- D-035: Creation conditions
- D-039: The test stage
- D-040: The winner of a competition
- D-042: Coherence
- D-043: Parallelism within a level
- D-044: Usurping
- D-057: What find-answer announces
- D-059: Several objects under one answer label
- D-060: One law for temperature in every choice
- D-062: The structure value reported
- D-063: The default answer
- D-082: Consumer C6

**screen-dump-inferred** (7): read directly off the chapter 4 screen dumps [F95 pp. 96-107 (img)].

- D-017: Initial activations
- D-024: Bottom-up groups
- D-041: Removal from the Workspace
- D-049: Q, the clamp and the start at 80
- D-051: The catalogue
- D-055: The initial rack
- D-071: "Opposite" as a descriptor of objects seen across the table

**digitization** (3): how the figures became problem files (ANSWERS.md 4).

- D-001: The table and its units
- D-002: Object footprints
- D-003: Digitizing the layouts

**Copycat choice** (3): a value or rule taken from Copycat where French gives none.

- D-008: Depth
- D-011: Label-controlled length
- D-014: Decay by depth

**simplest** (25): the simplest reading or value that fits the sources; the knobs a reader will most want to turn.

- D-004: The nine blurry regions
- D-006: Orientation is input, not perception
- D-016: Jolt sizes
- D-018: The small amount a label gets when both its concepts are active
- D-019: The update schedule
- D-020: Top-down posting
- D-024: Bottom-up groups
- D-025: Group strength
- D-031: Salience weights
- D-032: Region preference beyond the first two
- D-033: Proximity to the touched object
- D-036: Static points
- D-037: Dynamic points
- D-038: The bonus and the penalty
- D-041: Removal from the Workspace
- D-042: Coherence
- D-047: The temperature coefficients
- D-048: Delta-Q
- D-050: When T is recomputed
- D-052: Raw urgencies, follow-ups, the floor
- D-053: Maximum age
- D-054: Generation 0 for top-down and find-answer codelets
- D-056: The find-answer schedule
- D-058: A touched object mapped only through its group
- D-061: The run cap

**extension** (10): engineering that French's program did not have: the generator, the events, the profiles, the benches, the baseline, the invariants.

- D-064: The generator
- D-065: Typed events and the two traces
- D-066: Profiles
- D-067: The Potelbat baseline
- D-068: Realistic random tables
- D-072: The benches
- D-073: The probe benches
- D-074: The stated-behaviour bench
- D-075: The freeze, the results file and its builder
- D-077: The invariants checked after every codelet

**engineering** (7): the fast engine (D-107 on).

- D-107: Two engines
- D-108: The update's change report, worked out when read
- D-109: The generator stays splitmix64
- D-110: The build
- D-111: The Slipnet in C arrays
- D-112: The typed hot spots after the native Slipnet
- D-113: The equivalence of the two engines

**re-interpretation** (34): a choice beyond French's program, made for the roles (D-078 on; its knob, where it has one, is validated above zero, since no value may turn roles off), the optimized defaults (D-114, D-115) and the structural Blockage fix (D-116 to D-120).

- D-078: Roles as part of the engine
- D-102: The variants bench
- D-079: The registry, the pool and the tag as a competing structure
- D-080: The Slipnet's role nodes and links
- D-081: The role codelets
- D-095: Consumer C1
- D-104: The test-only probes
- D-105: The role events, the snapshot and the trace
- D-083: The symmetry axis, read off the Worldview without the touched object
- D-084: The single-table detectors
- D-099: The image roles, read by compare-roles, and C1's images under the axis
- D-087: ANCHOR, what a setting is laid out around
- D-088: CONNECTOR, the member that bridges two runs
- D-090: MEMBER and SIDEKICK, the content roles of a group
- D-098: Consumer C5
- D-097: Consumer C3
- D-085: ROW and COLUMN, the selector roles of the table frame
- D-093: COUNTERPART
- D-096: Consumer C2
- D-086: GAP, LONER and STRAY
- D-100: Consumer C7
- D-091: FIGURE and GROUND
- D-092: ECHO and TWIN
- D-094: TYPE_KEY
- D-089: BARRIER and CLAIMED
- D-101: Consumer C4
- D-106: The churn bench
- D-114: The optimized defaults (v3)
- D-115: The second optimization's defaults (v4)
- D-116: The Hobj status counts in every rating
- D-117: Inertia per rivalry
- D-118: The proximity law is bounded by Blockage 10
- D-119: Discarding a BARRIER discards the CLAIMED tag that names it
- D-120: A time pressure on the answer that does not depend on T

## D-114's table

The table of D-114's **Choice** (v2 → v3, four digits), moved here from `DECISIONS.md` at
v4 (D-115, 2026-10-17) to keep that file within the size the repository allows, its rows
unchanged:

| Knobs | v2 → v3 |
|---|---|
| region blur width, max (D-004) | 4, 0.5 → 2.201, 0.05779 |
| depths; label_shrink; max_path_links (D-008) | (20, 50, 80) → (45, 75, 45); 0.6 → 0.1912; 2 → 3 |
| spread constant, radius; decay (D-013) | 0.5, 100, 0.1 → 0.1976, 89.09, 0.1054 |
| jolts structure, description, label (D-016) | 40, 20, 20 → 11, 14, 9 |
| initial opposite, touched type; focus_jolt (D-017) | 139, 51; 20 → 68, 147; 9 |
| label trickle, threshold (D-018) | 0.05, 50 → 0.02496, 32 |
| top_down rise, threshold (D-020) | 40, 100 → 10.57, 95 |
| pop_fixed_amount; push_reset_set (D-021) | 30; slippage-concepts → 56; symmetries (fault 5) |
| proximity full, zero gap; reach (D-022) | 0.5, 2.5; 40 → 0.1357, 4.936; 15.69 |
| group_stage_pass; shared_isa_max_length (D-024) | 0.95, 0.6, 0.1; 30 → 0.8106, 0.1581, 0.01803; 41 |
| group_strength_weights (D-025) | 20, 10, 40, 20, 10 → 26, 4, 34, 7, 3 |
| member salience in group (D-069) | 0 → 0.2061 (fault 6: 0.2) |
| salience_weights (D-031) | 0.3, 30, 10, 10, 40, 15, 5, 5, 10, 5, 2, 5 → 0.2175, 10, 2, 22, 14, 54, 6, 5, 9, 1, 4, 1 |
| middle_over_end; stray_salience_scale (D-031) | 2; 0.5 → 4.054; 1.2 |
| group_salience_points (D-046) | 10 → 29 (fault 3: 15) |
| region_preference (D-032) | 1, 0.7, 0.4, 0.15 → 0.6895, 0.5034, 0.1076, 0.4196 |
| cp_static_weights (D-036) | 0.2, 15, 5, 10, 10, 10, 10, 10, 5, 2, 10 → 0.06958, 24, 1, 3, 5, 3, 2, 24, 13, 5, 19 |
| type_weight; group_object_penalty | 30; 15 → 26; 45 (fault 2: 30) |
| cp_dynamic_points; win, loss (D-037) | 15, 10, 5; 5, 8 → 11, 6, 8; 3, 4 |
| removal; incoherence, coherence; parallel (D-041) | 20; 40, 10; 5 → 75; 27, 5; 16 |
| alpha, beta, gamma; dq, wv windows (D-047) | 600, 4, 0.5; 6, 10 → 304.3, 15.52, 0.8149; 14, 18 |
| initial T; q_floor (D-049) | 80; 0.05 → 70.65; 0.03837 |
| urgencies (D-052) | 20 ×6, 40 ×4, 30, 30, 20 ×3, 100, 50, 50, 100, 100, 50, 50, (0), 60, 20, 30, 30, 50, 40 → 21, 12, 5, 22, 17, 19, 70, 13, 25, 64, 50, 19, 12, 76, 7, 58, 14, 68, 95, 99, 34, 20, (0), 89, 16, 40, 14, 15, 35 |
| urgency_floor; rho; exponents | 0.01; 3; 6, 0.1 → 0.03709; 1.576; 6.699, 0.2126 |
| capacity; max age; initial_codelets | 150; 60; 20, 2, 1, 1, 1 → 179; 216; 48, 3, 3, 3, 3 |
| answer post rate, power; urgency max, power (D-056) | 10, 3; 100, 3 → 1.093, 7.796; 27.26, 1.667 |
| role points weight, node, story; break scale (D-079) | 10, 30, 20; 100 → 6.364, 14.09, 68.32; 261 |
| role_pair_floor; role_recast_tiers | 1; 8, 4, 2, 1, 0.5 → 2.392; 11.85, 1.902, 5.824, 0.3522, 1.827 |

## D-115's table

The grouped table of D-115's **Choice**, every knob v4 changed (v3 → v4, four digits;
exact in `tabletop/config.py` and `V3_DEFAULTS`), kept here beside D-114's for the same
reason:

| Knobs | v3 → v4 |
|---|---|
| blur (D-004) | 2.201; 0.05779 → 1.729; 0.07181 |
| depths; label shrink (D-008) | 45, 75, 45; 0.1912 → 45, 77, 79; 0.1864 |
| spread, radius; decay (D-013) | 0.1976; 89.09; 0.1054 → 0.2682; 139.3; 0.1202 |
| jolts (D-016) | 11; 14; 9 → 14; 19; 6 |
| opposite, type; focus (D-017) | 68; 147; 9 → 76; 117; 15 |
| trickle, threshold (D-018) | 0.02496; 32 → 0.01985; 30 |
| top-down; pop (D-020) | 10.57; 95; 56 → 14.79; 105; 98 |
| proximity; reach (D-022) | 0.1357; 4.936; 15.69 → 0.1075; 5.479; 11.12 |
| stage pass; ISA length (D-024) | 0.8106, 0.1581, 0.01803; 41 → 0.7619, 0.198, 0.01552; 22 |
| group strength; member (D-025) | 26, 4, 34, 7, 3; 0.2061 → 57, 3, 36, 6, 4; 0.2222 |
| salience (D-031) | 0.2175, 10, 2, 22, 14, 54, 6, 5, 9, 1, 4, 1 → 0.4381, 14, 4, 41, 15, 24, 5, 2, 10, 1, 2, 2 |
| middle/end; stray; group (D-031) | 4.054; 1.2; 29 → 2.785; 0.8656; 28 |
| regions (D-032) | 0.6895, 0.5034, 0.1076, 0.4196 → 0.6996, 0.7882, 0.09341, 0.5773 |
| static (D-036) | 0.06958, 24, 1, 3, 5, 3, 2, 24, 13, 5, 19 → 0.06996, 15, 1, 4, 9, 3, 3, 21, 14, 8, 21 |
| type; group-object; dynamic; loss | 26; 45; 11, 6, 8; 4 → 34; 38; 10, 6, 7; 6 |
| removal; coherence; parallel (D-041) | 75; 27; 5; 16 → 126; 24; 6; 6 |
| alpha, beta, gamma; windows (D-047) | 304.3; 15.52; 0.8149; 14; 18 → 634.3; 8.826; 0.9064; 18; 11 |
| initial T; Q floor; every (D-049) | 70.65; 0.03837; 5 → 81.86; 0.04082; 3 |
| urgencies (D-052) | 21, 12, 5, 22, 17, 19, 70, 13, 25, 64, 50, 19, 12, 76, 7, 58, 14, 68, 95, 99, 34, 20, 0, 89, 16, 40, 14, 15, 35 → 21, 13, 2, 31, 12, 33, 68, 8, 19, 69, 61, 66, 18, 90, 3, 57, 17, 67, 92, 99, 38, 14, 0, 97, 19, 43, 34, 31, 40 |
| floor; rho; exponents; age; capacity | 0.03709; 1.576; 6.699; 0.2126; 216; 179 → 0.04064; 1.533; 7.435; 0.1359; 196; 260 |
| initial codelets (D-055) | 48, 3, 3, 3, 3 → 56, 3, 2, 3, 4 |
| answer posting, urgency (D-056) | 1.093; 7.796; 27.26; 1.667 → 1.163; 6.631; 21.38; 2.86 |
| role points; break (D-079) | 6.364; 14.09; 68.32; 261 → 4.996; 11.88; 74.23; 154.6 |
| pair floor; tiers (D-093) | 2.392; 11.85, 1.902, 5.824, 0.3522, 1.827 → 4.356; 8.611, 3.751, 6.305, 0.3495, 2.052 |

## Paragraphs moved for size (v5)

Four paragraphs of `DECISIONS.md` moved here verbatim on 2026-10-17 (v5, D-116 to D-120) to keep
that file within the size the repository allows, as D-114's and D-115's tables were; each entry
points here.

### D-073's catalogue

The **Choice** of D-073 (the probe benches) and its **As built (item 27)** paragraph:

- **Choice:** eight more benches under `python3 -m tabletop.bench NAME`, all on the harness
  of D-072 (the per-problem batch seed, the survey, the JSON record of every run), and a
  `bench all --out DIR` that writes every bench's tables (`NAME.md`) and records (`NAME.json`),
  the files RESULTS.md is built from. `humans`: the 23 problems of the tables surveyed under
  two profiles, the one given and `human` (or `standard` when `human` is given), the four
  columns side by side (the subjects, Tabletop's second runs, ours twice); Table 5.3's
  "Either small glass" is every labelled small glass and "Originally touched cup" the touched
  object; Figure 5.4's order with French's gap and ours, the count within ten points, and
  three Spearman rank correlations (our percentage for the subjects' most frequent answer
  with the subjects', with Tabletop's second runs, and our gap with his); the other human
  data [F95 pp. 118, 122; FCCA pp. 396-397; FH91 p. 181] with our share for the object each
  names (the touched cup, or the isolated cup for the two Surround 5 statements). `ch2`: a
  survey of each configuration, every statement of the `expected` block a yes-or-no check:
  the named answer is our most frequent; every plausible answer appears; every unlikely one
  is rarer than the most frequent; a shift "toward" an object is a higher share for it than
  in the figure compared with. `ch4`: Surround 5 run many times, each run stepped, the mean
  temperature and rack shares of the runs still going at each dump's codelet count beside
  the dump's readings, and for each milestone of French's run (the first gestalt group,
  bottom-up group, correspondence, Worldview entry, group correspondence in the Worldview,
  fight, and the answer; each read off `tests/data/ch4.json`'s structures by the kind and
  status words) the share of our runs reaching it and the mean codelet. `focus`: Surround 5
  beside Surround 10 with French's numbers and the two ratios he states, then Surround 5
  with 0, 2, ... 12 synthetic junk objects (Surround 10's five types first, then the
  palette's small objects) on an even grid across the band between the rows (y 40-60) with
  a seeded jitter of 2 units, each kept 2 units clear of everything. `rho`: 0.5, 1, 1.5, 2,
  3, 4 and 6 on a few problems. `lesions`: FCCA's (2), (3) high and low, and (4), plus rho =
  1, losers destroyed (D-041) and the context mechanism off (D-021), each a variant of the
  profile changing only its knob; FCCA's (1), telescoping the scouts and effectors, has no
  knob. `faults`: each of the seven fault knobs restored alone, then all seven (equal to
  `french-faults`), on the problems SPEC.md §6b names for each fix. `potelbat`: the
  baseline's answer and margin on every problem of the catalogue beside French's answer and
  Tabletop's most frequent, with the counts of agreement. Every probe table lists, per
  variant and problem, the main answer and its share, French's main answer's share, the
  literal share, the mean codelets, the mean structure value and the capped runs. The probe
  benches' default problems are one of each family French reports (Surround 5, Blockage 2
  and 6, Buridan 1). Nothing a bench prints sets a default (ANSWERS.md 1).
- **As built (item 27):** a ninth probe bench, `temperature` (`tabletop/bench/temperature.py`):
  the four probe problems run 500 times each, every run stepped with a collector, the
  `TemperatureUpdated` events read as the run's path (clock, T, held, reason, the inputs
  Q, DeltaQ, Delta_WV, T_cp); the curve of T against the codelets run (0, 5, 10, 20, ...,
  100: mean and quartiles over the runs still going, T between updates the last update's),
  Surround 5's curve at the dumps' counts beside French's thermometer [F95 pp. 96-107
  (img)], and per variant the T at the first Worldview change (the first update not held,
  D-049) and at the answer, the lowest T, the updates per run by reason (D-050) and the mean
  of the three terms over the updates not held [F95 p. 91 (img)]. Beside the intact
  profile, two readings D-047 and D-048 record as alternatives, `dq_signed` = True and
  `temperature_alpha` = 100, probes, not candidates (ANSWERS.md 1). The results builder
  reads measure M7 from the `lesions` records: `lesions.READINGS` names, per lesion, the
  quantities its expectation says should move (the literal share, French's main answer's
  share, the mean codelets, the capped runs) with the direction, and `lesions.verdict` gives
  `as predicted`, `no change` or `against` (beyond two standard errors for a share, a tenth
  of the intact mean for the run length, five percent of the runs for the cap; a lesion
  expected to change nothing is as predicted when none moved). The expectations (item 23)
  and readings (item 27) were stated before the records were read (RESULTS.md §6.2).

### D-052's as-built paragraph (item 19)

The **As built (`tabletop/codelets/`, item 19)** paragraph of D-052:

  As built (`tabletop/codelets/`, item 19: `scouts.py`, `builders.py`, `searches.py`,
  `judges.py`, `finish.py`, `dispatch.FUNCTIONS` from type to function, `common.Outcome`
  for what a codelet did): the lists above, read thus where the entry was silent.
  find-group's two find-neighbors take the first and the last member of the group found;
  find-neighbor proposes each new neighbour pair as a bottom-up group and posts build-obj-0
  for it (D-024), as does find-sim-nhbr for the conceptually nearest known neighbour of its
  object; build-obj-desc writes the count, orientation and kind; pick-salient-obj picked
  any item in view and a partner a correspondence could join it to, both by salience (item
  24 below); pick-2nd-salient-obj picks an object in no group (looking at a region first
  when none is in view) and posts a find-group around it and a create-cp; pick-strong-cp
  picks a correspondence outside the Worldview by its rating, tests it if untested, and
  posts cp-compete on a pass; examine-cp proposes one inner pair per run (a member of the
  first group and a partner, by salience) and posts the create-cp inside the parent;
  check-worldview posts a cp-compete per challenger. A codelet posted without an argument
  (the Slipnet's, D-020) chooses its target by salience among the fitting items
  (`common.argument`): find-neighbor among the noticed members of gestalt groups whose
  neighbours are unknown, find-end-objs and find-middle-objs among the groups without ends
  or middles, the searches among the groups, build-obj-desc among the bottom-up groups,
  simple-sameness-detect and find-sim-nhbr among the items in view. A codelet whose
  structure has left the Workspace fizzles [F95 p. 73]; the run loop removes every
  correspondence whose group end has left it (`World.drop_orphans`, D-041), since nothing
  else could.

### D-068's as-built paragraph (item 28)

The paragraph of D-068:

  As built (`tabletop/tables.py`, item 28): `random_table(seed)` lays out Henry's row at
  y 15 and Eliza's at y 82 (every digitized row of the catalogue lies at 11-17 or 73-89,
  `docs/problems.md`), each row one to three place settings left to right, a setting being
  one or two forks, usually a knife, a centre (a plate, a soup bowl, a cup, or a cup with
  its saucer beside it), up to two spoons and sometimes a glass, in the order of French's
  own rows ("fork fork knife cup spoon spoon" [F95 p. 118 (img)]), Eliza's settings
  reversed for the person they are set for; in a third of the rows a stray cup, glass or
  spoon stands apart at an end, as Surround 5's isolated cup does; in half the tables one
  or two clusters (a shaker pair, one shaker, a lone cup or glass) stand in the middle band
  at y 50, as Surround 10's junk does. Gaps within a setting are drawn between 1 and 3
  units, between clusters between 8 and 24 (2.5 glass widths, D-022's zero point, is 11.25),
  the row shifted by a seeded part of its slack; a row that will not fit loses slots from
  its last setting, then settings; the row and each object jitter by at most 3 and 2.25
  units. The touched object is drawn by type weight (cup 6, small glass, spoon and fork 2,
  the rest 1: French touches a cup in every Surround and Blockage problem, a spoon in
  Buridan, a fork in Figure/Ground). `realism_faults` states the rule a set table obeys as
  a check (inside the table; no footprints overlapping by more than one figure pixel, since
  Buridan's digitized cups and spoons touch by up to 0.55 units; every object in one of
  three bands; a band's objects within a strip of 10.5 units), which every digitized layout
  of the catalogue passes and every generated table is asserted to pass. The tables run 7
  to 36 objects, 22 on average, larger than any of French's (5 to 18), by design. The
  stray, the shakers and the mirrored settings are the generator's own touches, read from
  French's figures, not from any statement of his.

### D-021's as-built paragraph

The paragraph of D-021:

  As built (`context.py`, `World.step`, `Slipnet.reset`, `restore`, `spread_step_into`;
  item 20). *The context a codelet carries* is the group correspondence named in its
  arguments (`context_of`): examine-cp's first, create-cp's third [F95 p. 108], and a
  second argument create-cp's chain adds to the build-cp-descrip and cp-compete it posts
  inside (`codelets.common.context_args`; pick-strong-cp and check-worldview add it when
  their correspondence was built inside one). *When:* the run loop calls `enter` after
  choosing a codelet and before running it; the same context changes nothing; otherwise
  the current context is popped and the codelet's, if its correspondence still stands, is
  pushed (no nesting). A context whose correspondence has gone is none, and the codelet
  fizzles (D-041). The context need not be in the Worldview: examine-cp codelets are
  posted when the group correspondence enters [F95 p. 85], and a later expulsion does not
  stop those on the rack. *The push* sets the reset set (D-070) to 1 with `Slipnet.reset`,
  the posting levels of D-020 too, so that a rise inside the subworld (the diagonal
  sub-correspondences' jolts to "diagonal symmetry" [F95 p. 87]) counts from 1. *The pop*
  reinitializes the reset set, then, under the fix, reads what one spreading step (D-013)
  from every other node at its current level would send into the reinitialized nodes at 1
  (`spread_step_into`: what "might have spread to these concepts" [F95 p. 89], independent
  of the fixed amounts), then adds `pop_fixed_amount` for every node of every supporting
  slippage of every top-level Worldview correspondence (one built inside a group
  correspondence belongs to its subworld and is not read): the slippage's first concept,
  its second (an identity feeds its one concept twice, "'right' (twice)" [F95 p. 89]) and
  its labels of D-030: a right-left slippage feeds right, left, "opposite" and "diagonal
  symmetry", a right-right identity right twice, "mirror symmetry" and "sameness"; the
  kind slippage names no node. Nodes outside the reset set get the amounts on top of their
  level (French's right and left under the two-node push [F95 p. 88]). The sum is clipped
  once (D-015) and written with `Slipnet.restore`, which sets the posting level to the
  restored activation, so the pop posts no codelet. Push and pop draw nothing. `Step`
  carries `pushed` and `popped` (`context.Push`, `context.Pop`), the `World` its
  `context`. *A finding, not acted on:* under D-009's link table the two symmetry nodes
  are linked only to "symmetry" and to each other, and the label nodes ("sameness",
  "opposite", "horizontal") to nothing, so the fix's spreading step recovers little:
  "symmetry" at 100 sends about 1.7 into each symmetry node and nothing into a label. The
  fix is kept as French's words ask, and its size is reported; the push fires in about two
  runs in a hundred (RESULTS.md §6).
