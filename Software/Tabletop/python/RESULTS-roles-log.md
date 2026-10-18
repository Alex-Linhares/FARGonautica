# The Re-interpretation log: the roles' entries

The entries of [`RESULTS.md`](RESULTS.md)'s Re-interpretation log written by the roles loop
(its items 03 to 10, 2026-10-14), from the reference at v1 (commit `e38d6e0`) to the engine
the fast-engine loop started from, moved here verbatim on 2026-10-15 so that `RESULTS.md`
stays within the size the repository allows; and, at its end, the fast-engine loop's items
02 and 03 (2026-10-15), moved here verbatim on 2026-10-17 (v4, D-115) for the same reason.
The log's rules, the reference at v1 and the later entries are in `RESULTS.md`; nothing here
was edited.

### 2026-10-14 Item 03: The role substrate, consumer C1 and the oracle probe

- **Decisions:** D-079 (the registry, the pool, the tag as a competing structure), D-080 (the
  15 Slipnet nodes and 29 links), D-081 (the role codelets `propose-role-cp` and
  `check-roles`, their urgencies and top-down rows), D-095 (consumer C1), D-104 (the
  test-only probes), D-105 (the events, the snapshot and the `RL` lines); D-007, D-009,
  D-010, D-020, D-051 and D-052 revised.
- **Why the runs changed:** the Slipnet has 15 more nodes and 29 more links, so spreading
  differs from the first update (`opposite` and `sameness`, linked to nothing in v1, now
  spread into the role subnet, D-080); the rack carries two more codelet types, posted top
  down by the role nodes, which fizzle while no detector is built; every draw after the
  first differs. No role is tagged in these runs (the detectors come with items 04 to 10).
- **Files changed:** `config_defaults.lock`, `fingerprints.json`, `surround-05-seed-1.txt`,
  `blockage-06-seed-2.txt`. The GUI's determinism references (`tests/gui/golden.py`) read
  the fingerprints file and did not change; the GUI tests' hard-coded run facts were
  updated: surround-05 seed 1 from 81 codelets and 806 events to 45 and 453
  (`test_tap.py`, `test_event_log.py`), surround-05 seed 3 from 46 codelets to 54
  (`test_config_view.py`), buridan-03 seed 2 from 15 codelets and 190 events to 79 and 722
  (`test_main_window.py`, `test_session_view.py`, `test_robustness.py`); the README's and
  `docs/trace-format.md`'s quoted trace lines were regenerated from the seed-3 run.
- **Lock fields changed** (every profile): `urgencies` (two rows added), `top_down_table`
  (ten rows added); fields added: `role_weight_points`, `role_node_points`,
  `role_story_points`, `role_break_scale`, `role_pair_floor`.
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `573aef4966804bd1...` (874) to `12acb05b2dd094fedfa31b4de4532bdd934207dd11a781cd5fb61e0c24e53a7e` (1237);
  - `surround-05 2`: `45c973d03eb307ea...` (784) to `2a7115b410b06968047e49fbe8e6d73b92fc717277093111b8a4fa724e5d012f` (1650);
  - `blockage-06 1`: `eebb079051d5bf3d...` (687) to `ddd1ccb0140f51be4f3bd076903b490c9a477cc605f9c08e42b43ff448e469fc` (1540);
  - `blockage-06 2`: `1dfc4e8a7e0b2aba...` (658) to `4787d934fd4f61257897f21b9a401a63173711c734b2580b3960bc83244048ce` (1994);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072; mechanism first, then the answers; a change is
  read only where the two-sample z passes 2):

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| surround-05 | 0.99 | 1.045 | 7.5 | 11.5 | 0.01 | 0.02 | 0.065 | 0.095 | 0.0 | 0.0 | 60.5 | 65.5 |
| blockage-06 | 0.92 | 0.935 | 1.0 | 2.0 | 0.0 | 0.0 | 0.01 | 0.02 | 0.0 | 0.0 | 51.2 | 54.7 |
| buridan-01 | 0.99 | 0.935 | 4.5 | 2.5 | 0.02 | 0.01 | 0.025 | 0.015 | 0.0 | 0.0 | 60.6 | 62.4 |
| blockage-02 | 0.945 | 0.95 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 43.7 | 46.9 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| surround-05 | E2 | 73 | 82 | +0.9 | within noise |
| surround-05 | E1 | 35 | 31 | -0.5 | within noise |
| surround-05 | cup1 | 28 | 28 | +0.0 | within noise |
| surround-05 | spoon3 | 19 | 22 | +0.5 | within noise |
| surround-05 | spoon4 | 21 | 17 | -0.7 | within noise |
| surround-05 | fork3 | 10 | 11 | +0.2 | within noise |
| surround-05 | fork4 | 12 | 7 | -1.2 | within noise |
| surround-05 | knife2 | 2 | 2 | +0.0 | within noise |
| blockage-06 | E1 | 89 | 79 | -1.0 | within noise |
| blockage-06 | E2 | 73 | 75 | +0.2 | within noise |
| blockage-06 | E3 | 18 | 18 | +0.0 | within noise |
| blockage-06 | small-glass2 | 9 | 17 | +1.6 | within noise |
| blockage-06 | small-glass4 | 11 | 11 | +0.0 | within noise |
| buridan-01 | E1 | 69 | 64 | -0.5 | within noise |
| buridan-01 | E2 | 28 | 36 | +1.1 | within noise |
| buridan-01 | cup3 | 26 | 24 | -0.3 | within noise |
| buridan-01 | small-glass2 | 25 | 23 | -0.3 | within noise |
| buridan-01 | cup4 | 25 | 19 | -1.0 | within noise |
| buridan-01 | small-glass1 | 15 | 16 | +0.2 | within noise |
| buridan-01 | spoon1 | 12 | 18 | +1.1 | within noise |
| blockage-02 | E1 | 152 | 153 | +0.1 | within noise |
| blockage-02 | small-glass2 | 30 | 31 | +0.1 | within noise |
| blockage-02 | E2 | 18 | 16 | -0.4 | within noise |

No answer moved beyond z = 2 on any carrier: the substrate alone changes the draws, not the
distributions (the nearest, Blockage 6's E1 at 89 to 79, z -1.1). The mechanism measures are
within noise too (no role is tagged; the tags per run stay 0). The oracle probe's numbers
are in §11.3.

### 2026-10-14 Item 04: The geometric family with C6

- **Decisions:** D-082 (consumer C6, the placement slippage in the table frame;
  spec-inferred), D-083 (the symmetry axis, read without the touched object), D-084 (the
  MIDLINE detector and the find-roles codelet), D-099 (the image roles, compare-roles and
  C1's images under the axis); D-029, D-036, D-055, D-081 and D-095 revised.
- **Why the runs changed:** three more codelet types (`find-roles`, `compare-roles`,
  `read-worldview-roles`; 28 in all), one find-roles in the initial rack and one posted by
  every find-group, two more top-down rows (`separator`, `symmetry`); the midline is read
  on every carrier and the images across it are tagged, so the role pool is no longer
  empty and consumer C1 proposes pairs; every top-level correspondence across the midline
  carries a placement slippage scored by factor 4, which draws the two ends' regions
  through the blur and feeds the symmetry nodes on entry; cp-compete posts a re-reading of
  the Worldview's roles after each change. Every draw after the first find-roles differs.
- **Files changed:** `config_defaults.lock`, `fingerprints.json`, `surround-05-seed-1.txt`,
  `blockage-06-seed-2.txt`. The GUI's determinism references (`tests/gui/golden.py`) read
  the fingerprints file and did not change; the GUI tests' hard-coded run facts (surround-05
  seed 3 now 81 codelets, the answer small-glass1 at T 44.8; surround-05 seed 1; blockage-06
  seeds 2, 4 and 12; blockage-08 seed 111's push; buridan-03 seed 2) are re-derived in item
  04b, as item 03's were in 03b: `test_structures_model.py`, `test_worldview_model.py`,
  `test_table_model.py`, `test_table_view.py`, `test_coderack_model.py`,
  `test_coderack_view.py`, `test_temperature_model.py`, `test_temperature_views.py`,
  `test_slipnet_network_model.py`, `test_slipnet_network_view.py`, `test_trace_model.py`,
  `test_event_log.py`, `test_tap.py`, `test_config_view.py`, `test_log_view.py`,
  `test_results_view.py`, `test_stats_view.py`, `test_worldview_views.py`,
  `test_main_window.py`, `test_robustness.py` (the push run is now `surround-04` seed 20;
  the held-fight branch `surround-05` seed 10; the proposals `surround-05` seed 6; the
  removals `blockage-06` seed 1), and in the slow suite `test_session_view.py`,
  `test_main_window_slow.py`, `test_robustness_slow.py` and `test_trace_model_slow.py`
  (buridan-03 seed 2 from 79 codelets and 722 events to 36 and 386; the capped run of
  G-007 from 22,773 events to 23,496); the README's, the front page's and
  `docs/trace-format.md`'s quoted trace lines and report lines were regenerated from the
  seed-3 run.
- **Lock fields changed** (every profile): `urgencies` (three rows added), `top_down_table`
  (two rows added), `initial_codelets` (one row added). No field added.
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `12acb05b2dd094fe...` (1237) to `a3f2d8cd6dba6e7934c15653e19172f05c669c4dc7de0459527de5c4fb335917` (1534);
  - `surround-05 2`: `2a7115b410b06968...` (1650) to `54d2a42bbd19a4e37e4fbb2d048bac8c3b37a5cdef0acb7bd4ebd9bef3a70c54` (2031);
  - `blockage-06 1`: `ddd1ccb0140f51be...` (1540) to `554764683e502f37fec6a5ea04f4f9bfb930c18e4c7325490d153c3bf81eee6a` (1948);
  - `blockage-06 2`: `4787d934fd4f6125...` (1994) to `abb4b6c29fdca29e64734b9326650e5e00db5025210aa33f1984e86217620ef9` (2231);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1; mechanism first, then the answers;
  a change is read only where the two-sample z passes 2):

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| surround-01 | 0.94 | 0.935 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 46.4 | 49.5 |
| surround-05 | 1.045 | 1.235 | 11.5 | 23.0 | 0.02 | 0.15 | 0.095 | 0.14 | 0.0 | 15.775 | 65.5 | 71.0 |
| blockage-06 | 0.935 | 0.985 | 2.0 | 4.0 | 0.0 | 0.0 | 0.02 | 0.04 | 0.0 | 3.96 | 54.7 | 60.7 |
| figure-ground-01 | 0.895 | 0.98 | 3.0 | 5.0 | 0.03 | 0.05 | 0.0 | 0.0 | 0.0 | 0.685 | 56.2 | 60.2 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| surround-01 | E2 | 95 | 102 | +0.7 | within noise |
| surround-01 | E1 | 74 | 59 | -1.6 | within noise |
| surround-01 | cup1 | 31 | 39 | +1.1 | within noise |
| surround-05 | E2 | 82 | 92 | +1.0 | within noise |
| surround-05 | E1 | 31 | 36 | +0.7 | within noise |
| surround-05 | cup1 | 28 | 19 | -1.4 | within noise |
| surround-05 | spoon3 | 22 | 20 | -0.3 | within noise |
| surround-05 | spoon4 | 17 | 19 | +0.3 | within noise |
| surround-05 | fork3 | 11 | 7 | -1.0 | within noise |
| surround-05 | fork4 | 7 | 4 | -0.9 | within noise |
| surround-05 | knife2 | 2 | 2 | +0.0 | within noise |
| surround-05 | spoon1 | 0 | 1 | +1.0 | within noise |
| blockage-06 | E2 | 75 | 98 | +2.3 | gained |
| blockage-06 | E1 | 79 | 72 | -0.7 | within noise |
| blockage-06 | E3 | 18 | 12 | -1.1 | within noise |
| blockage-06 | small-glass2 | 17 | 7 | -2.1 | lost |
| blockage-06 | small-glass4 | 11 | 11 | +0.0 | within noise |
| figure-ground-01 | E1 | 79 | 96 | +1.7 | within noise |
| figure-ground-01 | E2 | 75 | 75 | +0.0 | within noise |
| figure-ground-01 | fork3 | 29 | 14 | -2.4 | lost |
| figure-ground-01 | E3 | 17 | 15 | -0.4 | within noise |

The role pool is live for the first time: 15.8 tags per run stand at the end on Surround 5,
4.0 on Blockage 6, 0.7 on Figure/Ground 1 and none on Surround 1, where the two rows are
grouped with the touched cup's row reaching the middle band in the drawn regions, so the
midline is seldom perceived (prediction 16's "nearly every run on the 35 sets" is already
doubtful on Surround 1; item 13 scores it). On Surround 5 the Worldview grows (1.05 to
1.24 correspondences per run; two or more in 23 percent of runs against 11.5) and the
coherent pairs rise from 0.02 to 0.15 per run: the image pairs C1 proposes give D-042
something to read. Two answers moved beyond z = 2: Blockage 6's E2 (the cup pair's mirror
reading, 75 to 98, gained) and its literal glass small-glass2 (17 to 7, lost); Figure/Ground
1's fork (29 to 14, lost). Every other answer is within noise, Surround 5's E2 and E1 both
up by less than that. The codelet count rises by three to six per run (the role scouts).

### 2026-10-14 Item 05: Members, sidekicks, anchors and connectors, with C5

- **Decisions:** D-087 (ANCHOR; the setting form of CONTAINER follows at item 06), D-088
  (CONNECTOR), D-090 (MEMBER and SIDEKICK), D-098 (consumer C5, the role slippage and the
  tenth static factor); D-036, D-081, D-084 and D-095 revised.
- **Why the runs changed:** four more detectors answer `roles.detect`, so an object brought
  into view with its group is tagged a member at first sight and find-roles reads the
  sidekicks, anchors and connectors off the neighbours; find-neighbor posts one find-roles
  on its object after a new relation; every correspondence whose two ends share a
  single-table role carries a role slippage, scored by the tenth factor
  (`cp_static_weights["role"]`, 10, times the node's activation over 150), which jolts the
  role's node on entry and counts for parallelism; consumer C1 proposes sidekick, anchor
  and connector pairs tag with tag across the midline. Every draw after the first group is
  found differs (the members' tags jolt `group` and their proposals draw).
- **Files changed:** `config_defaults.lock`, `fingerprints.json`, `surround-05-seed-1.txt`,
  `blockage-06-seed-2.txt`. The GUI's determinism references (`tests/gui/golden.py`) read
  the fingerprints file and did not change; the GUI tests' hard-coded run facts (surround-05
  seed 3 is now 67 codelets, the answer cup2 (E2) at T 77.5, with cp2 (fork4 = cup1)
  entering the Worldview at codelet 43 and cp4 (cup2 = cup1) beating it at 65; surround-05
  seeds 1, 2, 6 and 10; blockage-06 seeds 1, 2 and 12; surround-04 seed 20's push; buridan-03
  seed 2; the capped run of G-007) were re-derived in this item, in the files items 03b and
  04b list (`tests/gui/` and `tests_slow/gui/`), with these seeds moved because their
  phenomenon moved: the push run to `surround-07` seed 13 (the first of 17 in seeds 1-300);
  the fights run (two held, one won) from `blockage-06` seed 2 to `surround-04` seed 69; the
  held-then-won branch from `surround-05` seed 10 to `ch2-fig13` seed 28; the proposals from
  `surround-05` seed 6 to `surround-05` seed 2; the run with none from `blockage-06` seed 12
  to seed 11; the greyed removals from `blockage-06` seed 1 to `surround-05` seed 2; the
  capped run of G-007 from 23,496 events to 23,646; buridan-03 seed 2 from 36 codelets and
  386 events to 66 and 687. One GUI model fix was needed for the seek test
  (`gui/models/coderack.py`, G-038's dated note). The README's, the front page's and
  `docs/trace-format.md`'s quoted trace lines, report lines, batch table and Surround 5 table
  were regenerated from the new runs.
- **Lock fields changed** (every profile): `cp_static_weights` (the row `("role", 10)` added).
  No field added.
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `a3f2d8cd6dba6e79...` (1534) to `94e8908499c7a819142dffc1f3c76dd070eb77ae7bcfe9ba22855cf42505d3e8` (1123);
  - `surround-05 2`: `54d2a42bbd19a4e3...` (2031) to `16bf7fe4e13a98069d9bbc288e2fae44b97b04e9e9e7a40cc5564f7f0cf9f244` (3024);
  - `blockage-06 1`: `554764683e502f37...` (1948) to `08e239a827e726e25bd94e742dc9c87cc22009921984505282907c3e527ddd50` (1352);
  - `blockage-06 2`: `abb4b6c29fdca29e...` (2231) to `e3ebe12063c9d5ce148b69552bd9c37f2cb92aaadd78fdff966ed4607d7b0c41` (1746);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1; mechanism first, then the answers;
  a change is read only where the two-sample z passes 2):

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| buridan-01 | 0.96 | 0.95 | 2.5 | 3.5 | 0.01 | 0.02 | 0.015 | 0.025 | 0.75 | 8.555 | 65.1 | 69.7 |
| buridan-02 | 0.93 | 0.955 | 0.5 | 4.0 | 0.005 | 0.025 | 0.0 | 0.015 | 0.735 | 6.02 | 63.3 | 68.5 |
| surround-05 | 1.235 | 1.195 | 23.0 | 21.5 | 0.15 | 0.11 | 0.14 | 0.135 | 15.775 | 27.07 | 71.0 | 73.6 |
| surround-09 | 1.215 | 1.135 | 20.5 | 17.0 | 0.15 | 0.07 | 0.15 | 0.13 | 15.31 | 25.73 | 68.1 | 70.8 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| buridan-01 | E1 | 61 | 69 | +0.9 | within noise |
| buridan-01 | E2 | 40 | 30 | -1.3 | within noise |
| buridan-01 | cup3 | 25 | 24 | -0.2 | within noise |
| buridan-01 | cup4 | 23 | 26 | +0.5 | within noise |
| buridan-01 | small-glass2 | 17 | 19 | +0.3 | within noise |
| buridan-01 | small-glass1 | 20 | 13 | -1.3 | within noise |
| buridan-01 | spoon1 | 14 | 19 | +0.9 | within noise |
| buridan-02 | E1 | 92 | 80 | -1.2 | within noise |
| buridan-02 | E2 | 47 | 50 | +0.3 | within noise |
| buridan-02 | cup4 | 28 | 31 | +0.4 | within noise |
| buridan-02 | small-glass1 | 17 | 22 | +0.8 | within noise |
| buridan-02 | spoon1 | 16 | 17 | +0.2 | within noise |
| surround-05 | E2 | 92 | 106 | +1.4 | within noise |
| surround-05 | E1 | 36 | 32 | -0.5 | within noise |
| surround-05 | spoon3 | 20 | 17 | -0.5 | within noise |
| surround-05 | spoon4 | 19 | 15 | -0.7 | within noise |
| surround-05 | cup1 | 19 | 15 | -0.7 | within noise |
| surround-05 | fork3 | 7 | 6 | -0.3 | within noise |
| surround-05 | fork4 | 4 | 5 | +0.3 | within noise |
| surround-05 | spoon2 | 0 | 2 | +1.4 | within noise |
| surround-05 | spoon1 | 1 | 1 | +0.0 | within noise |
| surround-05 | knife2 | 2 | 0 | -1.4 | within noise |
| surround-05 | fork2 | 0 | 1 | +1.0 | within noise |
| surround-09 | E2 | 123 | 114 | -0.9 | within noise |
| surround-09 | spoon4 | 25 | 17 | -1.3 | within noise |
| surround-09 | cup1 | 13 | 26 | +2.2 | gained |
| surround-09 | spoon3 | 16 | 23 | +1.2 | within noise |
| surround-09 | fork3 | 9 | 10 | +0.2 | within noise |
| surround-09 | E1 | 8 | 6 | -0.5 | within noise |
| surround-09 | fork4 | 4 | 2 | -0.8 | within noise |
| surround-09 | spoon1 | 2 | 0 | -1.4 | within noise |
| surround-09 | knife2 | 0 | 1 | +1.0 | within noise |
| surround-09 | spoon2 | 0 | 1 | +1.0 | within noise |

The content roles are read: the tags standing at the end of a run rise from 0.75 to 8.6 per
run on Buridan 1 (the nine members, and the doubled cups and glasses as sidekicks when the
chain of neighbours reaches them), from 0.7 to 6.0 on Buridan 2, and from 15.8 to 27.1 on
Surround 5 (the members; the glass as anchor and connector in the runs whose chain is
complete by the answer). One answer moved beyond z = 2: Surround 9's literal cup1 (13 to
26 of 200, gained); every other answer is within noise, Surround 5's E2 up by less (92 to
106, z +1.4) and Buridan 1's E2 down by less (40 to 30, z -1.3). The Worldview's size and
its coherent and incoherent pairs are within noise on every carrier (Surround 5's
coherent pairs 0.15 to 0.11, Surround 9's 0.15 to 0.07 per run: fewer image pairs are
proposed while find-roles reads the content roles too). The codelet count rises by three
to five per run (the find-roles that find-neighbor posts).

**The `buridan` penalty at 40 and 300 on Buridan 1 (prediction 2), `python3 -m
tools.role_probes --problems buridan-01` at 200 runs:** flat under the engine's own
perception, run for run: E1 69, E2 30, the literal spoon 19 at both values (coherent pairs
0.02 per run at 40, 0.025 at 300: the sidekick pairs that would make the penalty live are
seldom in the Worldview, since the doubled cups' sidekick tag needs the chain of neighbours
to reach the middle cup before the answer, which comes at about 70 codelets). Under the
hand-read roles (the oracle probe of §11.3) the scan moves a little: E2 38 at 40 and 40 at
300, E1 50 and 49, coherent pairs 0.215 and 0.27, incoherent 0.22 and 0.15. Prediction 2 is
not met at this item; item 12 re-measures the scan at full size and item 13 scores it.

### 2026-10-14 Item 07: COUNTERPART, ROW and COLUMN with C2

- **Decisions:** D-085 (ROW and COLUMN, the selector roles of the table frame), D-093
  (COUNTERPART: the one re-cast of the touched object's signature, in tiers), D-096
  (consumer C2, the role-weighted partner choice); D-029, D-052, D-081, D-082, D-095, D-098,
  D-104 and D-105 revised.
- **Why the runs changed:** two more detectors answer `roles.detect`: every object is tagged
  with its COLUMN at first sight (one blur draw, D-029's table frame read at last) and with
  its ROW once a midline is perceived, each tag jolting its node; the sixth role codelet,
  `recast-role` (search band, raw urgency 40), carries the touched object's signature to
  the other row (D-093), publishes the re-cast, tags the counterparts and posts a create-cp
  for each first-tier candidate; `counterpart`, `row` and `column` post it top down, and
  find-roles posts it once the touched object's row is read; pick-salient-obj weights its
  pick by the re-cast (C2, D-096), an identical draw with no COUNTERPART tag standing; the
  placement slippage reads an end's tagged column instead of drawing it (C6, D-082 revised);
  C1's `across` reads the tagged rows (D-095 revised); C5 leaves the two selector roles out
  (D-098 revised; every role it read before it still reads). The first draw differs on
  every run (the column of the first object noticed).
- **Files changed:** `config_defaults.lock`, `fingerprints.json`, `surround-05-seed-1.txt`,
  `blockage-06-seed-2.txt`; `oracle_roles.json` (the hand-read ROW and COLUMN of every
  object, by its exact region; not a watched file). The GUI's determinism references
  (`tests/gui/golden.py`) read the fingerprints file and did not change; the GUI tests'
  hard-coded run facts (surround-05 seed 3 is now 77 codelets and 856 events, the answer
  small-glass1 (E1) at T 34.6, with cp2 (small-glass1 = cup1) entering the Worldview without
  a fight at codelet 45 and never challenged; surround-05 seeds 1, 2, 6 and 10; blockage-06
  seeds 1, 2, 11 and 12; the push run; surround-04 seed 69; ch2-fig13 seed 28; buridan-03
  seed 2, now 46 codelets and 495 events; the capped run of G-007) were re-derived in this
  item in the files items 03b, 04b and 05 list (`tests/gui/` and `tests_slow/gui/`), with
  the seeds that moved named in PROGRESS.md's iteration 9 entry (the push run to `ch2-fig12`
  seed 16, the first of the runs with a push in seeds 1-300 of the 53 problems). The
  README's, the front page's and `docs/trace-format.md`'s quoted trace lines, report lines,
  batch table and Surround 5 table were regenerated from the new runs; the README's table
  of knobs gained `role_recast_tiers`.
- **Lock fields changed** (every profile): `urgencies` (the row `("recast-role", 40)`
  added), `top_down_table` (the rows `("counterpart", "recast-role")`, `("row",
  "recast-role")`, `("column", "recast-role")` added); one field added, `role_recast_tiers`
  (`(8.0, 4.0, 2.0, 1.0)`).
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `94e8908499c7a819...` (1123) to `2e79c784ef106ec1d6d2966b517d6844443c2e99e02858e9a9bec942d32a3661` (1595);
  - `surround-05 2`: `16bf7fe4e13a9806...` (3024) to `8b9c6f54435a148e661a70b3e5bde7979de1c6e362268de5a890b2ca1cfe8857` (1957);
  - `blockage-06 1`: `08e239a827e726e2...` (1352) to `383542f9a5e1c5f70da20449d7b49807c6bcf4f18d50b34550fd783c6374c16c` (1854);
  - `blockage-06 2`: `e3ebe12063c9d5ce...` (1746) to `7cc9af95a81c7678fe800c9d354a5686e522f5164df03e44e00da764acb640d0` (2850);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1; mechanism first, then the answers;
  a change is read only where the two-sample z passes 2). The third table is the item's
  report of the `other` share (§8.1's first finding: the answers French prints no bar for)
  and of how often the re-cast fired:

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| surround-05 | 1.195 | 1.17 | 21.5 | 16.5 | 0.11 | 0.12 | 0.135 | 0.11 | 27.07 | 44.97 | 73.6 | 77.6 |
| buridan-01 | 0.95 | 0.96 | 3.5 | 3.5 | 0.02 | 0.005 | 0.025 | 0.03 | 8.555 | 22.35 | 69.7 | 72.7 |
| blockage-01 | 0.85 | 0.89 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 1.395 | 46.8 | 50.9 |
| surround-02 | 1.11 | 1.09 | 15.5 | 12.5 | 0.075 | 0.075 | 0.08 | 0.05 | 12.655 | 23.19 | 64.9 | 70.4 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| surround-05 | E2 | 106 | 97 | -0.9 | within noise |
| surround-05 | E1 | 32 | 29 | -0.4 | within noise |
| surround-05 | cup1 | 15 | 22 | +1.2 | within noise |
| surround-05 | spoon3 | 17 | 18 | +0.2 | within noise |
| surround-05 | spoon4 | 15 | 15 | +0.0 | within noise |
| surround-05 | fork4 | 5 | 10 | +1.3 | within noise |
| surround-05 | fork3 | 6 | 6 | +0.0 | within noise |
| surround-05 | knife2 | 0 | 3 | +1.7 | within noise |
| surround-05 | spoon2 | 2 | 0 | -1.4 | within noise |
| surround-05 | spoon1 | 1 | 0 | -1.0 | within noise |
| surround-05 | fork2 | 1 | 0 | -1.0 | within noise |
| buridan-01 | E1 | 69 | 77 | +0.8 | within noise |
| buridan-01 | E2 | 30 | 24 | -0.9 | within noise |
| buridan-01 | cup4 | 26 | 24 | -0.3 | within noise |
| buridan-01 | cup3 | 24 | 23 | -0.2 | within noise |
| buridan-01 | small-glass2 | 19 | 21 | +0.3 | within noise |
| buridan-01 | spoon1 | 19 | 16 | -0.5 | within noise |
| buridan-01 | small-glass1 | 13 | 15 | +0.4 | within noise |
| blockage-01 | E1 | 170 | 178 | +1.2 | within noise |
| blockage-01 | E2 | 30 | 22 | -1.2 | within noise |
| surround-02 | E2 | 102 | 88 | -1.4 | within noise |
| surround-02 | E1 | 31 | 45 | +1.8 | within noise |
| surround-02 | spoon3 | 20 | 20 | +0.0 | within noise |
| surround-02 | spoon2 | 16 | 19 | +0.5 | within noise |
| surround-02 | cup1 | 18 | 15 | -0.5 | within noise |
| surround-02 | fork1 | 13 | 13 | +0.0 | within noise |

| Carrier | `other` before (of 200) | after | z | runs with a re-cast (of 200) | re-casts | ties | runs with a tier-1 candidate | runs where C2 picked a counterpart | pick-salient-obj picks |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| surround-05 | 62 (31.0 %) | 74 (37.0 %) | +1.3 | 55 | 327 | 168 | 0 | 35 (139 picks) | 1073 |
| buridan-01 | 101 (50.5 %) | 99 (49.5 %) | -0.2 | 75 | 425 | 116 | 0 | 61 (229 picks) | 1287 |
| blockage-01 | 0 (0.0 %) | 0 (0.0 %) | +0.0 | 0 | 0 | 0 | 0 | 0 (0 picks) | 512 |
| surround-02 | 67 (33.5 %) | 67 (33.5 %) | +0.0 | 56 | 192 | 45 | 1 | 34 (103 picks) | 1187 |

The selector roles are read: the tags standing at the end of a run rise from 27.1 to 45.0
per run on Surround 5 (a COLUMN on every object and a ROW on most, beside the members and
the images), from 8.6 to 22.4 on Buridan 1, from 12.7 to 23.2 on Surround 2, and from 0 to
1.4 on Blockage 1 (its two columns: with one object on each side no gestalt group forms, so
no midline is perceived (D-084) and no row, re-cast or placement follows; a two-object
table is outside what this item can move, which D-085 records). **No answer moved beyond
z = 2 on any carrier**; the nearest are Surround 2's E1 (31 to 45, z +1.8) and E2 (102 to
88, z -1.4), the literal cup1 on Surround 5 (15 to 22, z +1.2) and Blockage 1's E1 (170 to
178, z +1.2). The `other` share (§8.1) did not fall: 31.0 to 37.0 percent on Surround 5 (z
+1.3, within noise), 50.5 to 49.5 on Buridan 1, 33.5 to 33.5 on Surround 2. Prediction 7
("`other` down by at least 10 on Surround 5 and Buridan 1", `docs/predictions.md` §4,
carried by L1 and K1 with C2) is not met at this item; item 13 scores it. The mechanism
explains why: the re-cast is published in 55 of 200 Surround 5 runs, 75 of 200 Buridan 1
runs and 56 of 200 Surround 2 runs (never on Blockage 1), because the touched object's ROW
is read only by a find-roles that picks it after the midline is perceived (the row needs the
separator, D-085; the object's roles at first sight predate any group); a first-tier
candidate (the image under a standing axis that plays the signature) occurred in one run
of 800 (the axis seldom stands before the answer); and C2 picked a tagged counterpart in 35,
61 and 34 of 200 runs, out of about 1,100 pick-salient-obj picks per carrier. Ties are
common (168 of Surround 5's 327 re-casts, 116 of Buridan 1's 425): with an empty or
seldom-matched signature every candidate in the touched object's column, or the whole row,
shares a tier. How many role codelets run before the answer, and whether the re-cast should
be due earlier, is item 11's measurement (ANSWERS.md C8); no urgency or follow-up was
changed after seeing these numbers. The Worldview's size and its pairs are within noise on
every carrier. `check_determinism.py --quick`: 0 mismatches in 151 comparisons.

### 2026-10-14 Item 08: LONER, CORNER, GAP and STRAY with C1 and C7

- **Decisions:** D-086 (GAP, LONER and STRAY: the parts of a side, and the items of neither),
  D-100 (consumer C7: role salience, the ninth term); D-031, D-081, D-084, D-085, D-095, D-098
  and D-104 revised.
- **Why the runs changed:** four more detectors answer `roles.detect`: the gaps of a row are
  read on the table (D-086, with D-022's own zero gap as the distance), a loner and a stray
  are read on an object a find-group found no neighbour for, a corner on such an object or
  on a gestalt group (D-085); find-group posts its find-roles on the group it found and one on
  a seed it marks isolated (D-081), and find-roles reads the table on every call; the MIDLINE
  is perceived from the clusters of the two outer rows, so a table of loners (Blockage 2,
  Surround 1) has its two sides now, and a gestalt group lying wholly in the middle band no
  longer blocks it (D-084); consumer C1 proposes loner with loner, corner with corner (the
  point image before the mirror image, D-032's order) and the parts a gap marks off under the
  axis, and never a pair with a stray at an end (D-095); C5 leaves CORNER, STRAY and GAP out
  (D-098); consumer C7 adds the ninth salience term, `salience_weights["role"]` (5) per unit
  of weight of an ANCHOR, CONNECTOR or FIGURE tag, and scales a stray's region term by
  `stray_salience_scale` (0.5) (D-100). The first draw differs on every run (the find-roles
  posted on the first isolated object changes the rack).
- **Files changed:** `config_defaults.lock`, `fingerprints.json`, `surround-05-seed-1.txt`,
  `blockage-06-seed-2.txt`. The GUI's determinism references (`tests/gui/golden.py`) read the
  fingerprints file and did not change; the GUI tests' hard-coded run facts (surround-05 seed
  3 is now 44 codelets and 507 events, the answer cup2 (E2) at T 41.0, with cp1 (cup2 = cup1)
  entering the Worldview without a fight at codelet 18 and never challenged; surround-05 seeds
  1, 2, 6, 8 and 10; blockage-06 seeds 1, 2, 3, 11 and 12; the push run, now `ch2-fig08` seed 2,
  the first by seed of the runs with a push in seeds 1-300 of the 53 problems; surround-04
  seed 69 and ch2-fig13 seed 28 or their replacements; buridan-03 seed 2; the capped run of
  G-007) were re-derived in this item in the files items 03b, 04b, 05 and 07 list (`tests/gui/`
  and `tests_slow/gui/`), with the seeds that moved named in PROGRESS.md's iteration 10 entry.
  The README's, the front page's and `docs/trace-format.md`'s quoted trace lines, report
  lines, batch table and Surround 5 table were regenerated from the new runs; the README's
  table of knobs gained `stray_salience_scale` and the `role` row of `salience_weights`; the
  salience terms are nine (`salience.FACTORS`), which the GUI's docstrings and
  `docs/engine-api.md` say.
- **Lock fields changed** (every profile): `salience_weights` (the row `("role", 5)` added);
  one field added, `stray_salience_scale` (`0.5`).
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `2e79c784ef106ec1...` (1595) to `84ffa424bf6dab97fbf89336aded08eaf458a74cb709bc837a538f2a5f8f9831` (2389);
  - `surround-05 2`: `8b9c6f54435a148e...` (1957) to `88d4ed53aa7de0cea28cd73dbe3b4a802bdc91aa0ab2f1fa8e356c01a9002e27` (2479);
  - `blockage-06 1`: `383542f9a5e1c5f7...` (1854) to `c5b70b46d71321696e122d4566e0c344a4b9501be2441463b49c79559db95efc` (2078);
  - `blockage-06 2`: `7cc9af95a81c7678...` (2850) to `3bca0495cad7897a6192b2718eb81b5662ffb548b275e24c334973cb46d33e56` (2824);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1; mechanism first, then the answers;
  a change is read only where the two-sample z passes 2). The third table is the item's
  measurement of the `focus` bench at 200 runs (the junk scaling of §5.4, for prediction 16
  and L5's row of `docs/predictions.md` §2):

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| surround-01 | 0.945 | 0.965 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 2.04 | 14.385 | 49.8 | 58.6 |
| blockage-02 | 0.895 | 0.945 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 1.675 | 10.31 | 49.8 | 57.7 |
| surround-10 | 1.085 | 1.205 | 10.5 | 22.5 | 0.065 | 0.135 | 0.08 | 0.125 | 37.105 | 56.45 | 77.3 | 84.5 |
| surround-05 | 1.17 | 1.175 | 16.5 | 18.5 | 0.12 | 0.115 | 0.11 | 0.12 | 44.97 | 52.405 | 77.6 | 80.5 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| surround-01 | E2 | 115 | 109 | -0.6 | within noise |
| surround-01 | E1 | 59 | 66 | +0.8 | within noise |
| surround-01 | cup1 | 26 | 25 | -0.1 | within noise |
| blockage-02 | E1 | 153 | 163 | +1.2 | within noise |
| blockage-02 | E2 | 28 | 21 | -1.1 | within noise |
| blockage-02 | small-glass2 | 19 | 16 | -0.5 | within noise |
| surround-10 | E2 | 93 | 93 | +0.0 | within noise |
| surround-10 | E1 | 31 | 38 | +0.9 | within noise |
| surround-10 | cup1 | 21 | 18 | -0.5 | within noise |
| surround-10 | spoon4 | 17 | 12 | -1.0 | within noise |
| surround-10 | spoon3 | 12 | 15 | +0.6 | within noise |
| surround-10 | fork3 | 10 | 5 | -1.3 | within noise |
| surround-10 | fork4 | 1 | 8 | +2.4 | gained |
| surround-10 | small-glass2 | 6 | 3 | -1.0 | within noise |
| surround-10 | pepper-shaker1 | 5 | 2 | -1.1 | within noise |
| surround-10 | salt-shaker1 | 3 | 1 | -1.0 | within noise |
| surround-10 | knife2 | 0 | 2 | +1.4 | within noise |
| surround-10 | spoon2 | 1 | 1 | +0.0 | within noise |
| surround-10 | spoon1 | 0 | 1 | +1.0 | within noise |
| surround-10 | fork5 | 0 | 1 | +1.0 | within noise |
| surround-05 | E2 | 97 | 108 | +1.1 | within noise |
| surround-05 | E1 | 29 | 28 | -0.1 | within noise |
| surround-05 | cup1 | 22 | 24 | +0.3 | within noise |
| surround-05 | spoon3 | 18 | 18 | +0.0 | within noise |
| surround-05 | spoon4 | 15 | 11 | -0.8 | within noise |
| surround-05 | fork4 | 10 | 5 | -1.3 | within noise |
| surround-05 | fork3 | 6 | 5 | -0.3 | within noise |
| surround-05 | knife2 | 3 | 1 | -1.0 | within noise |

| `focus` bench, 200 runs | E1 before | after | E2 before | after | mean codelets before | after |
|---|---:|---:|---:|---:|---:|---:|
| Surround 5 | 29 | 28 | 97 | 108 | 77.6 | 80.5 |
| Surround 10 | 31 | 38 | 93 | 93 | 77.3 | 84.5 |
| Surround 5 + 2 junk | 31 | 28 | 106 | 90 | 77.1 | 77.8 |
| Surround 5 + 4 junk | 30 | 27 | 86 | 96 | 77.6 | 79.7 |
| Surround 5 + 6 junk | 24 | 30 | 90 | 82 | 82.3 | 82.0 |
| Surround 5 + 8 junk | 23 | 33 | 75 | 76 | 83.5 | 86.5 |
| Surround 5 + 10 junk | 26 | 25 | 69 | 70 | 87.0 | 86.8 |
| Surround 5 + 12 junk | 20 | 32 | 60 | 60 | 86.9 | 86.8 |

The four roles are read: the tags standing at the end of a run rise from 2.0 to 14.4 per run
on Surround 1 and from 1.7 to 10.3 on Blockage 2 (the loners, the corners, the gap on the
table, and, now that a table of loners has a midline, the rows, the images and the re-cast
that items 04 and 07 built: a midline is perceived in 29 of 30 sampled runs on each, where
before there was none), from 37.1 to 56.5 on Surround 10 and from 45.0 to 52.4 on Surround 5.
Over 30 sampled runs the loner is read in 30 of 30 on Surround 1 and Blockage 2 and in 18 and
22 of 30 on Surround 5 and 10 (the isolated cup needs a find-group to look around it before
the answer), the gap in 27, 7, 27 and 27 of 30, a corner in every run, and a stray group in 8
of 30 Surround 10 runs (the junk forms groups of two and three under the proximity
criterion, so a stray group is read only when a find-roles reads the junk group it found);
consumer C1 proposed a loner pair in 20 of 30 Surround 1 runs and 16 of 30 Blockage 2 runs,
and a corner pair in 6 and 18, pairs that no scout could propose before. **One answer moved
beyond z = 2**: Surround 10's fork4, 1 to 8 of 200 (z +2.4, gained; a literal silverware
answer, French's "other"); everything else is within noise (nearest: Blockage 2's E1 153 to
163, z +1.2, and Surround 5's E2 97 to 108, z +1.1). Surround 1's E2 (115 to 109) and Blockage
2's E2, the touched cup (28 to 21), did not move at this item: L4's registered direction
(Surround 1 E2 up by 5 to 15; Blockage 2's touched cup up) is not met here, and item 13
scores it. The Worldview on Surround 10 holds two or more correspondences in 22.5 percent of
runs against 10.5 before (the loner and part pairs give the Worldview something beside the
touched object's own); Surround 5's mechanism is within noise. The `focus` bench at 200
runs: Surround 10's E1 is 38 of 200 against Surround 5's 28 (before: 31 against 29), so the
junk costs the glass nothing, as French reports [F95 p. 121]; the run-length ratio is 1.05
(before 1.00; French 0.85); over the junk scaling the main answer E2 falls from 108 to 60 of
200 at twelve junk objects (before: 97 to 60) and the codelets rise from 80.5 to 86.8 (before
77.6 to 86.9): the cost per two junk objects is about 1 codelet (before 1.5 at this size; v1
2 at 500 runs), and the main answer's fall over twelve objects is 48 of 200 against 37
before, not halved. Prediction 16's three claims (L5's row) are therefore mixed at this
item: Surround 10's E1 within 5 points of Surround 5's holds (19.0 against 14.0 percent), the
cost per two junk objects at most 1 holds at the margin, the halved fall does not; item 13
scores them at 500 runs. Nothing was changed after seeing these numbers.
`check_determinism.py --quick`: 0 mismatches in 151 comparisons.

### 2026-10-14 Item 09: ECHO, TWIN, FIGURE, GROUND and TYPE_KEY

- **Decisions:** D-091 (FIGURE and GROUND: the one repeated type of a row and what stands
  out against it), D-092 (ECHO and TWIN: a group's echoes and the touched object's twin
  across the midline), D-094 (TYPE_KEY: the type substitution a correspondence between two
  grounds makes); D-081, D-093, D-095, D-098 and D-104 revised.
- **Why the runs changed:** five more detectors answer `roles.detect`: a figure and a ground
  are read on an object at first sight and by find-roles, a ground on a sameness subgroup
  too (D-091); a group's echoes by type, by category or by an object of its type, and the
  touched object's twin across the midline, are read by compare-roles and find-roles once a
  midline is perceived (D-092); the type key is read off the Worldview by
  read-worldview-roles, which now drops any stale Worldview tag and jolts the `key` node as
  it jolts the axis's (D-094, D-081); the last build-obj stage posts one find-roles on the
  group it built (D-081); consumer C1 proposes figure with figure, ground with ground (like
  with like), echo with echo and the unmapped pairs a key names (D-095); consumer C5 earns
  the key identity on a correspondence that repeats the key (D-098); the re-cast's third
  tier is the TWIN tag's, not the layout's type (D-093). A run in which none of these is read
  before the answer is unchanged: surround-05 seed 3 (the quick start) and blockage-06 seed 1
  (a golden case) are the same draws as before.
- **Files changed:** `fingerprints.json`, `surround-05-seed-1.txt`, `blockage-06-seed-2.txt`.
  `config_defaults.lock` did not change (no field, urgency or top-down row was added). The
  GUI's determinism references (`tests/gui/golden.py`) read the fingerprints file and did
  not change; the GUI tests' hard-coded run facts (surround-05 seeds 1 and 2; blockage-06
  seeds 2, 5, 11 and 12; buridan-03 seed 2, now 84 codelets and 913 events; the push run,
  now `ch2-fig11` seed 3, the first by seed of the runs with a push in seeds 1-300 of the 53
  problems; surround-04 seed 22 and ch2-fig13 seed 27 or their replacements; the capped run
  of G-007) were re-derived in this item in the files items 03b, 04b, 05, 07 and 08 list
  (`tests/gui/` and `tests_slow/gui/`), with the seeds that moved named in PROGRESS.md's
  iteration 11 entry. The front page's batch of 5 and Surround 5 table at 50 runs were
  regenerated; the quick start's run and the README's trace walkthrough (surround-05 seed 3)
  are unchanged.
- **Lock fields changed:** none.
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `84ffa424bf6dab97...` (2389) to `e4978746fab81b4e2ee0d31a205772d8bbf3ee1bb5d720db54705e17a424d045` (1794);
  - `surround-05 2`: `88d4ed53aa7de0ce...` (2479) to `f4f6f8b579b8bf61ef2ad7eb6a65f91ed2467dc3ffe384152ad6cc46fa151577` (2152);
  - `blockage-06 1`: `c5b70b46d7132169...` (2078), unchanged;
  - `blockage-06 2`: `3bca0495cad7897a...` (2824) to `726ee716e4da5bb22002aed4980de0d1dbf0237b412780cb6180afe8c76d9c52` (2288);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1; mechanism first, then the answers;
  a change is read only where the two-sample z passes 2). The third table is the item's
  measurement of the `ch4` bench's milestones at 200 runs (the share of runs reaching each
  and the mean codelet of its first occurrence), for prediction A5's and A6's "group
  correspondences in the Worldview" (v1: 0.4 percent of 500 runs):

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| figure-ground-01 | 0.945 | 1.045 | 3.0 | 13.5 | 0.03 | 0.135 | 0.0 | 0.0 | 15.04 | 20.37 | 65.8 | 69.5 |
| blockage-06 | 0.995 | 0.99 | 3.0 | 4.0 | 0.0 | 0.01 | 0.03 | 0.03 | 16.125 | 17.17 | 65.9 | 66.7 |
| surround-05 | 1.175 | 1.145 | 18.5 | 17.0 | 0.115 | 0.105 | 0.12 | 0.105 | 52.405 | 52.745 | 80.5 | 77.8 |
| buridan-01 | 0.975 | 1.2 | 5.0 | 22.5 | 0.02 | 0.155 | 0.03 | 0.12 | 24.835 | 33.05 | 73.2 | 77.6 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| figure-ground-01 | E1 | 94 | 74 | -2.0 | lost |
| figure-ground-01 | E2 | 73 | 63 | -1.1 | within noise |
| figure-ground-01 | fork3 | 18 | 41 | +3.2 | gained |
| figure-ground-01 | E3 | 15 | 22 | +1.2 | within noise |
| blockage-06 | E1 | 87 | 86 | -0.1 | within noise |
| blockage-06 | E2 | 76 | 80 | +0.4 | within noise |
| blockage-06 | E3 | 10 | 18 | +1.6 | within noise |
| blockage-06 | small-glass2 | 16 | 7 | -1.9 | within noise |
| blockage-06 | small-glass4 | 11 | 9 | -0.5 | within noise |
| surround-05 | E2 | 108 | 100 | -0.8 | within noise |
| surround-05 | E1 | 28 | 34 | +0.8 | within noise |
| surround-05 | cup1 | 24 | 27 | +0.4 | within noise |
| surround-05 | spoon3 | 18 | 15 | -0.5 | within noise |
| surround-05 | spoon4 | 11 | 9 | -0.5 | within noise |
| surround-05 | fork3 | 5 | 9 | +1.1 | within noise |
| surround-05 | fork4 | 5 | 5 | +0.0 | within noise |
| surround-05 | knife2 | 1 | 0 | -1.0 | within noise |
| surround-05 | spoon2 | 0 | 1 | +1.0 | within noise |
| buridan-01 | E1 | 74 | 62 | -1.3 | within noise |
| buridan-01 | E2 | 38 | 38 | +0.0 | within noise |
| buridan-01 | spoon1 | 17 | 42 | +3.5 | gained |
| buridan-01 | cup3 | 25 | 24 | -0.2 | within noise |
| buridan-01 | cup4 | 24 | 12 | -2.1 | lost |
| buridan-01 | small-glass1 | 10 | 12 | +0.4 | within noise |
| buridan-01 | small-glass2 | 12 | 10 | -0.4 | within noise |

| `ch4` bench, 200 runs | French's first dump (codelets) | reached before, % | after | mean codelet before | after |
|---|---:|---:|---:|---:|---:|
| first gestalt group | 2 | 100.0 | 100.0 | 3.9 | 3.9 |
| first bottom-up group | 33 | 0.0 | 0.0 | - | - |
| first correspondence | 12 | 99.5 | 99.5 | 22.6 | 22.0 |
| first Worldview entry | 22 | 96.5 | 95.5 | 52.3 | 51.3 |
| first group correspondence in the Worldview | 47 | 7.5 | 4.5 | 69.2 | 71.7 |
| first fight | 47 | 57.5 | 55.0 | 70.7 | 71.6 |

The five roles are read. Over 30 sampled runs per carrier, a figure and a ground are read in
every Figure/Ground 1 and Buridan 1 run (the touched fork and Henry's lone glass against the
pairs; the touched spoon against the two cups, and Eliza's two clusters each with their own),
in 5 and 9 of 30 Blockage 6 runs (the lone cup against the pair of glasses needs the pair's
gestalt group first), and in 5 of 30 Surround 5 runs (never by the rule, D-091: a run reads
one when the blur puts a row's cluster so that one type repeats); an echo in 17 of 30 Buridan
1 runs, 12 of 30 Surround 5 (the rows by category) and 4 of 30 Blockage 6 (the glass pairs by
type), the twin in 14 to 16 of 30 on Buridan 1, Surround 5 and Figure/Ground 1 and never on
Blockage 6 (no second cup); the key in 7 of 40 Figure/Ground 1 runs, read off the row-to-row
echo pair or a ground-to-ground pair, and consumed by C5 in each and by C1 in one. The tags
standing at the end rise from 15.0 to 20.4 per run on Figure/Ground 1 and from 24.8 to 33.1
on Buridan 1, and the Worldview holds two or more correspondences in 13.5 percent of
Figure/Ground 1 runs (3.0 before) and 22.5 of Buridan 1 (5.0 before): the figure, ground and
echo pairs give the Worldview something beside the touched object's own. **Three answers moved
beyond z = 2, and they are the item's finding**: on Figure/Ground 1 the touched fork itself
(fork3, the literal answer) rose from 18 to 41 of 200 (z +3.2, gained) and E1 fell from 94 to
74 (z -2.0, lost), while E3, Henry's glass, rose from 15 to 22 (z +1.2, within noise; the
registered direction, not yet beyond noise at 200 runs); on Buridan 1 the touched spoon
(spoon1, literal) rose from 17 to 42 (z +3.5, gained) and cup4 fell from 24 to 12 (z -2.1,
lost). The literal answers rise for the reason item 03's oracle probe found (§11.3): the role
pairs that do not contain the touched object take the Worldview, and when the answer comes
the touched object is mapped through nothing (D-063), so Eliza touches its own counterpart,
itself. The consumer that reads such a Worldview back into the answer through the touched
object's group is C3 (item 06, re-opened after iteration 8); until it is built this cost
stands and is reported here, not tuned away. On Blockage 6 E3 rose 10 to 18 (z +1.6, within
noise) with the glass pairs' echo. On Surround 5 nothing moved. The `ch4` milestones at 200
runs: a group correspondence enters the Worldview in 4.5 percent of runs against 7.5 before
(z -1.2, within noise; v1: 0.4 percent), at codelet 71.7 on average (French's run: 47); the
first bottom-up group is reached in **0 percent of runs, before and after**, as in v1's 500
(§5.2): the chapter 4 run's subgroups are never built before the answer, in v1 or now (the
three stages take more codelets than the answer leaves them), which is why the key is read
off the grounds as wholes and member to member rather than off sameness subgroups (D-094),
and is a finding for the author (PROGRESS.md, iteration 11).
Nothing was changed after seeing these numbers. `check_determinism.py --quick`: see
PROGRESS.md's iteration 11 entry.

### 2026-10-14 Item 10: BARRIER and CLAIMED with C4

- **Decisions:** D-089 (BARRIER and CLAIMED: the claim on the obvious counterpart), D-101
  (consumer C4: the blocked counterpart); D-052, D-063, D-081, D-093, D-104 and D-105
  revised.
- **Why the runs changed:** read-worldview-roles reads two more Worldview roles off the
  correspondences standing (D-089): the obvious counterparts of the touched object are
  the candidates of the re-cast's first natural tier whose type is nearest its own at the
  Slipnet's distances; a Worldview correspondence that does not contain the touched
  object and holds one of them at an end tags its other end BARRIER and the counterpart
  CLAIMED. A barrier read posts consumer C4's pick-strong-cp for its correspondence, as
  check-worldview does for the barriers standing, and pick-strong-cp with an argument
  picks among that correspondence's challengers, or itself once outside the Worldview
  (D-101, D-052 revised); the default answer is recorded `blocked` while a CLAIMED tag
  stands (D-063 revised); the re-cast puts a claimed candidate in a fifth, last tier
  (D-093 revised), whose weight joins `role_recast_tiers`. The read-worldview-roles fizzle
  text names the barrier, so every trace with that fizzle changed its digest at the same
  draws; a run in which no barrier is read is otherwise the same draws as before, and
  the quick start (surround-05 seed 3) and blockage-06 seed 1 are unchanged.
- **Files changed:** `config_defaults.lock`, `fingerprints.json`, `surround-05-seed-1.txt`,
  `blockage-06-seed-2.txt`. The GUI's determinism references (`tests/gui/golden.py`) read
  the fingerprints file and did not change; no GUI test's hard-coded run fact moved (the
  push run stays `ch2-fig11` seed 3, the first by seed of the runs with a push in seeds
  1-300 of the 53 problems; buridan-03 seed 2 stays 84 codelets and 913 events), and the
  only GUI test touched by this item reads the decision titles of DECISIONS.md. The
  front page's Surround 5 table at 50 runs was regenerated (one run of the fifty differs);
  the README's knobs table was regenerated; the quick start's run and the trace walkthrough
  are unchanged.
- **Lock fields changed:** `role_recast_tiers` in every profile, `(8.0, 4.0, 2.0, 1.0)` to
  `(8.0, 4.0, 2.0, 1.0, 0.5)` (the fifth, CLAIMED tier, D-101).
- **Fingerprints** (digest, draws), old to new:
  - `surround-05 1`: `e4978746fab81b4e...` (1794) to `eced6109c958e975d9f33182b546472805ff93e0a8d48958f44895b6861b90b7` (1794);
  - `surround-05 2`: `f4f6f8b579b8bf61...` (2152) to `af4658e44a30a817f721b6baa96782ab8d0919ce85d4e288500618d0dd0ea6ca` (2152);
  - `blockage-06 1`: `c5b70b46d7132169...` (2078), unchanged;
  - `blockage-06 2`: `726ee716e4da5bb2...` (2288) to `657a246a3fe70b2866ddd17c2f80f3b7937e00d4942c0352fc4b91893870b620` (2288);
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1; mechanism first, then the answers;
  a change is read only where the two-sample z passes 2). The third table counts, over
  the same 200 runs, the runs in which the re-cast was published, a BARRIER and a CLAIMED
  tag were read, C4 posted a fight, and the answer was recorded `blocked`, with the
  answers of the runs that read a barrier:

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| blockage-02 | 0.945 | 0.945 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 | 10.31 | 10.325 | 57.7 | 57.7 |
| blockage-06 | 0.99 | 0.99 | 4.0 | 4.0 | 0.01 | 0.01 | 0.03 | 0.03 | 17.17 | 17.17 | 66.7 | 66.7 |
| blockage-07 | 1.165 | 1.17 | 18.0 | 18.0 | 0.035 | 0.04 | 0.165 | 0.17 | 39.07 | 39.075 | 78.5 | 78.6 |
| blockage-04 | 0.935 | 0.94 | 2.0 | 2.5 | 0.0 | 0.0 | 0.02 | 0.025 | 16.055 | 16.065 | 64.8 | 64.7 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| blockage-02 | E1 | 163 | 163 | +0.0 | within noise |
| blockage-02 | E2 | 21 | 22 | +0.2 | within noise |
| blockage-02 | small-glass2 | 16 | 15 | -0.2 | within noise |
| blockage-06 | E1 | 86 | 86 | +0.0 | within noise |
| blockage-06 | E2 | 80 | 80 | +0.0 | within noise |
| blockage-06 | E3 | 18 | 18 | +0.0 | within noise |
| blockage-06 | small-glass4 | 9 | 9 | +0.0 | within noise |
| blockage-06 | small-glass2 | 7 | 7 | +0.0 | within noise |
| blockage-07 | E2 | 70 | 70 | +0.0 | within noise |
| blockage-07 | E1 | 55 | 55 | +0.0 | within noise |
| blockage-07 | E3 | 18 | 18 | +0.0 | within noise |
| blockage-07 | fork2 | 18 | 18 | +0.0 | within noise |
| blockage-07 | knife2 | 15 | 15 | +0.0 | within noise |
| blockage-07 | small-glass4 | 12 | 12 | +0.0 | within noise |
| blockage-07 | small-glass2 | 8 | 8 | +0.0 | within noise |
| blockage-07 | fork1 | 3 | 3 | +0.0 | within noise |
| blockage-07 | knife1 | 1 | 1 | +0.0 | within noise |
| blockage-04 | E1 | 89 | 90 | +0.1 | within noise |
| blockage-04 | E2 | 78 | 78 | +0.0 | within noise |
| blockage-04 | E3 | 18 | 17 | -0.2 | within noise |
| blockage-04 | small-glass2 | 13 | 13 | +0.0 | within noise |
| blockage-04 | spoon1 | 2 | 2 | +0.0 | within noise |

| Carrier, 200 runs | re-cast published | BARRIER read | CLAIMED read | C4 fight posted | `blocked` | answers of the barrier runs |
|---|---:|---:|---:|---:|---:|---|
| blockage-02 | 102 | 2 | 1 | 0 | 1 | E2 2 |
| blockage-06 | 46 | 0 | 0 | 0 | 0 | - |
| blockage-07 | 63 | 3 | 3 | 3 | 0 | E2 2, knife2 1 |
| blockage-04 | 62 | 1 | 1 | 1 | 0 | E1 1 |

The two roles are read, and they are rare: a barrier in 0 to 3 of 200 runs per carrier
(0 on Blockage 6), the `blocked` record in 1 of 800. **No answer moved** on any carrier
(every z is 0.2 or less; Blockage 6's 200 answers are identical run for run), and the
mechanism measures moved by at most 0.005. The reason is in the third table and in the
mechanism as built: a barrier needs three things before the answer, the re-cast (both
ROW tags, so the midline and a find-roles on the touched object: published in 23 to 51
percent of runs, item 07's timing finding), a correspondence between two objects of
Eliza's and Henry's rows that does not contain the touched object standing in the
Worldview (the glass-glass pair of French's account: the Worldview holds two or more
correspondences in 4 percent of Blockage 6 runs, and the pair alone in fewer), and the
nearest-type counterpart at its end. On Blockage 7 the three barrier runs read the
glass pair (two) and a fork-to-fork correspondence claiming the fork across from the
cup (one: that fork is in the cup's column, the re-cast's second tier, and alone there).
In the seed search of this item (seeds 1 to 150 of the four carriers, 600 runs) a
barrier was read in 10 runs and the answer recorded `blocked` in 4, every one the
touched cup (E3 on Blockage 6 and 7, E2 on Blockage 2). Registered predictions S5 ("a
barrier is tagged in a third or more of the runs on Blockage 6"), S6 and C4 (`blocked`
in 10 to 40 percent of Blockage 6-8 runs; E3 up by 10 to 25) are not met at this item;
item 13 scores them. Nothing was changed after seeing these numbers: the obvious
counterpart's definition (the nearest type within the re-cast's first natural tier) and
the live re-cast were fixed while the detector was being built, from the first seed
searches' misreadings (a fork as the cup's obvious counterpart; a claimed candidate
falling out of the first tier and so ceasing to be claimed), before any 200-run
measurement; the registry's `pending()` is down to CONTAINER (item 06, to be re-opened).
`check_determinism.py --quick`: 0 mismatches in 151 comparisons (PROGRESS.md's iteration
12 entry).

### 2026-10-14 Item 06: The answer from group position (C3) and CONTAINER

- **Decisions:** D-097 (consumer C3: the answer from group position, by the members'
  roles); D-058 revised (`answer_from_group_position` is gone, `group_position_match` is
  `roles` or `words`, both answering from group position); D-087 revised (CONTAINER's
  setting and holder forms); D-052, D-081, D-095, D-104 and D-105 revised.
- **Why the runs changed:** three things. A touched object mapped only through its group
  now answers from its group position under every profile (D-058's boolean was False in
  all four: the literal object), first by the roles the touched object carries (ANCHOR,
  CONNECTOR, then `end` or `middle` as a MEMBER), then by D-058's words, then the literal
  object. examine-cp offers the anchor to the anchor first, without a draw, when each
  group has one and the pair is still open (D-097). find-roles reads the gestalt group of
  an object it read as an ANCHOR (the setting form of CONTAINER, D-087) and the object
  under an object it read (the holder form), each a draw and a tag the run did not have.
  The four golden runs reach no group case and read no anchor before their answers, so
  **the four fingerprints and the two traces are unchanged**; the lock changed on one
  field. Item 06 was reopened after being cut off at iteration 8; the engine is items 03 to
  11's with these three additions.
- **Files changed:** `config_defaults.lock`. `fingerprints.json`, `surround-05-seed-1.txt`
  and `blockage-06-seed-2.txt` are unchanged; the GUI's determinism references
  (`tests/gui/golden.py`) read the fingerprints file and did not change; no GUI test's
  hard-coded run fact moved (the quick start is the same 44-codelet run; the push run
  stays `ch2-fig11` seed 3; buridan-03 seed 2 stays 84 codelets and 913 events); the
  GUI's configuration-table test names the new field in the answer section
  (`tests/gui/test_config_table.py`). The README's knobs table was regenerated.
- **Lock fields changed:** `answer_from_group_position` (`False` in every profile)
  removed; `group_position_match` (`'roles'` in every profile) added.
- **Fingerprints** (digest, draws), unchanged:
  - `surround-05 1`: `eced6109c958e975...` (1794);
  - `surround-05 2`: `af4658e44a30a817...` (2152);
  - `blockage-06 1`: `c5b70b46d7132169...` (2078);
  - `blockage-06 2`: `657a246a3fe70b28...` (2288).
- **Carriers at 200 runs, before (HEAD) and after** (`tabletop.bench.variants.run_facts`
  over the problem's batch seed of D-072, bench seed 1, the on-saucer variant of Surround
  5 through `variants.variant_of` with its own batch seed; mechanism first, then the
  answers; a change is read only where the two-sample z passes 2). The third table
  counts, over the same 200 runs, how the answer was reached (`through`), the answers
  whose note names the roles matched, and the paired runs whose answer differs from the
  HEAD run of the same seed:

| Carrier | wv before | after | wv2 % before | after | coherent before | after | incoherent before | after | tags before | after | codelets before | after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| surround-05 | 1.135 | 1.14 | 16.0 | 16.5 | 0.105 | 0.105 | 0.1 | 0.105 | 52.775 | 53.08 | 77.7 | 77.9 |
| surround-07 | 1.2 | 1.195 | 21.0 | 20.0 | 0.085 | 0.085 | 0.175 | 0.175 | 51.145 | 51.965 | 79.4 | 80.1 |
| surround-10 | 1.14 | 1.155 | 17.0 | 18.0 | 0.09 | 0.105 | 0.11 | 0.115 | 57.435 | 57.78 | 81.5 | 81.6 |
| surround-05+on-saucer | 1.17 | 1.175 | 18.5 | 20.0 | 0.16 | 0.16 | 0.045 | 0.08 | 53.975 | 55.695 | 81.7 | 81.3 |

| Carrier | Answer | Before (of 200) | After | z | Reading |
|---|---|---:|---:|---:|---|
| surround-05 | E2 | 98 | 98 | +0.0 | within noise |
| surround-05 | E1 | 34 | 36 | +0.3 | within noise |
| surround-05 | cup1 | 29 | 26 | -0.4 | within noise |
| surround-05 | spoon3 | 15 | 14 | -0.2 | within noise |
| surround-05 | spoon4 | 9 | 11 | +0.5 | within noise |
| surround-05 | fork3 | 9 | 9 | +0.0 | within noise |
| surround-05 | fork4 | 5 | 5 | +0.0 | within noise |
| surround-05 | spoon2 | 1 | 1 | +0.0 | within noise |
| surround-07 | E2 | 102 | 102 | +0.0 | within noise |
| surround-07 | E1 | 31 | 33 | +0.3 | within noise |
| surround-07 | cup1 | 23 | 24 | +0.2 | within noise |
| surround-07 | spoon4 | 19 | 19 | +0.0 | within noise |
| surround-07 | spoon3 | 14 | 11 | -0.6 | within noise |
| surround-07 | fork3 | 8 | 10 | +0.5 | within noise |
| surround-07 | knife2 | 2 | 0 | -1.4 | within noise |
| surround-07 | fork4 | 1 | 1 | +0.0 | within noise |
| surround-10 | E2 | 84 | 81 | -0.3 | within noise |
| surround-10 | E1 | 31 | 31 | +0.0 | within noise |
| surround-10 | cup1 | 24 | 24 | +0.0 | within noise |
| surround-10 | spoon4 | 23 | 25 | +0.3 | within noise |
| surround-10 | spoon3 | 14 | 15 | +0.2 | within noise |
| surround-10 | fork3 | 9 | 8 | -0.2 | within noise |
| surround-10 | small-glass2 | 7 | 7 | +0.0 | within noise |
| surround-10 | fork4 | 4 | 5 | +0.3 | within noise |
| surround-10 | pepper-shaker1 | 2 | 2 | +0.0 | within noise |
| surround-10 | spoon2 | 1 | 1 | +0.0 | within noise |
| surround-10 | knife2 | 1 | 1 | +0.0 | within noise |
| surround-05+on-saucer | E2 | 74 | 79 | +0.5 | within noise |
| surround-05+on-saucer | saucer1 | 36 | 38 | +0.3 | within noise |
| surround-05+on-saucer | E1 | 25 | 30 | +0.7 | within noise |
| surround-05+on-saucer | cup1 | 23 | 15 | -1.4 | within noise |
| surround-05+on-saucer | spoon4 | 12 | 12 | +0.0 | within noise |
| surround-05+on-saucer | spoon3 | 14 | 8 | -1.3 | within noise |
| surround-05+on-saucer | fork4 | 7 | 10 | +0.7 | within noise |
| surround-05+on-saucer | fork3 | 6 | 6 | +0.0 | within noise |
| surround-05+on-saucer | knife2 | 3 | 2 | -0.5 | within noise |

| Carrier, 200 runs | `through` before | after | answers noting the roles matched | paired runs that differ |
|---|---|---|---:|---:|
| surround-05 | correspondence 169, default 27, blocked 2, group 2 | correspondence 170, default 24, group 4, blocked 2 | 0 | 5 |
| surround-07 | correspondence 177, default 22, blocked 1 | correspondence 176, default 23, blocked 1 | 0 | 10 |
| surround-10 | correspondence 176, default 24 | correspondence 176, default 24 | 0 | 6 |
| surround-05+on-saucer | correspondence 177, default 23 | correspondence 185, default 15 | 0 | 137 |

- **Reading.** No answer moved beyond z = 2 on any carrier; the mechanism measures are
  within 0.04. The group case (a touched object covered only by a group correspondence
  at the answer) comes in about 1 percent of runs, and in those the touched cup carries
  no ANCHOR tag yet, so the roles' reading and the words' agree on the first middle of
  Eliza's row (a spoon) and the answer is the same object D-058's `True` would have given:
  the engine gives the literal answer in this case no more, but the case is rare. Over
  500 seeds per carrier (a scan beside the measurement): the answer came through a group in
  6, 1, 4 and 6 runs; it was matched on the roles in 0, 1, 1 and 0; examine-cp's anchor pair
  never fired (the two anchors are seldom both tagged while the group pair's inner pairs
  are still open: ANCHOR stands on the touched cup at the end of 4 percent of Surround 5
  runs); the setting tag was read in 26, 21, 29 and 24 of 500 runs (whenever an anchor is
  read, its row is read as a setting); on the on-saucer variant the holder tag was read in
  496 of 500 runs, at the saucer's first sight or when the cup is read, which is why 137
  of its 200 paired runs differ from HEAD's draws while its answers do not move (the
  saucer takes the answer in 38 of 200, 36 before: the holder is read but no consumer
  holds the pair together, prediction S3's held-out row). C3's lesion
  (`tools/role_probes.py`, `results/role-probes.json`, re-run at this item) changes no
  answer in 200 paired runs on the four carriers of the probe or on Surround 6, 7 and 10:
  0 paired runs differ. The mechanism is built, tested and read; the consumer that reads
  the roles back into the answer fires too seldom before the answer to show at this size,
  the same timing finding as items 07, 10 and 13's (§11.8, finding 7).

## The fast-engine loop's items 02 and 03

### 2026-10-15 Item 02 (loop0008): The engine seam, the build and the generator

- **Decisions:** D-109 (the generator stays splitmix64, native on the compiled engine),
  D-110 (the build: pinned flags, the stamp, a stale build refused); D-107 and D-064
  revised.
- **Why the runs changed:** only the record did. `RunStarted` and `Result` gained `engine`
  (`compiled` or `python`) and the trace's first line names it, so every fingerprint and
  first line changed; the runs did not: the compiled generator is the pure algorithm in
  native arithmetic (D-109) and the Slipnet is still pure, so both engines and the pre-loop
  engine give identical runs. Item 01's three identity digests (34 x 10: `6eaa1753a2d0...`,
  `2484a03c174c...`, `36d1412d78f1...`) are reproduced on the compiled engine (one core,
  `nice` 10, load 10.5, 02:28).
- **Files changed:** `fingerprints.json`, `surround-05-seed-1.txt` and
  `blockage-06-seed-2.txt` moved from `tests/data/golden/` to `tests/data/golden/python/`
  (the pure engine's set) and regenerated; the same three new under
  `tests/data/golden/compiled/` (the reference); `tests/gui/golden.py` reads the running
  engine's set. No lock field changed.
- **Fingerprints** (before = item 01's; draws unchanged):
  - `surround-05 1`: compiled `6774dbc7d0f48347...`, python `17bbac9873584c81...`, before `eced6109c958e975...` (1794);
  - `surround-05 2`: compiled `2f49f1096a92b7da...`, python `935db8d66a987f67...`, before `af4658e44a30a817...` (2152);
  - `blockage-06 1`: compiled `39889a7d71acfa45...`, python `d806054c298cce98...`, before `c5b70b46d7132169...` (2078);
  - `blockage-06 2`: compiled `2db14f95af1f42d0...`, python `f706153f9a63fc61...`, before `657a246a3fe70b28...` (2288);
  The traces differ in the first line alone.
- **Carriers at 200 runs, compiled against the previous engine:** the 34 families
  problems x 200 runs (bench seed 1001, `tools/workload.py --runs 200`, one pinned core
  each, `nice` 10, load 10.6 to 13.8, 2026-10-15 02:30-02:37): the rows digest
  `18c3d9e348b9...` (6,800 rows: problem, seed, answer, structure value, codelets, capped,
  draws; 486,620 codelets) is the same on the compiled engine, on this tree's pure engine
  and on the pre-loop engine. Read as distributions, the answers' total variation is 0.000
  on every problem (the same sample, since the runs are bit-identical here); the native
  Slipnet (item 03) gets its own entry.

### 2026-10-15 Item 03 (loop0008): The native Slipnet

- **Decisions:** D-111 (the Slipnet in C arrays, the Python's operations in the Python's
  order); D-012, D-013, D-019 and D-107 revised.
- **Why the runs did not change:** the compiled Slipnet (`tabletop/_native/slipnet.pyx`)
  performs the same floating-point operations in the same order as `slipnet.py`, under the
  pinned flags (no fused multiply-add, D-110), so every run is item 02's on both engines:
  the three identity digests of D-108 are reproduced on the compiled engine (34 x 10:
  `6eaa1753a2d0...`, `2484a03c174c...`, `36d1412d78f1...`; one core, `nice` 10, load 14,
  03:17), and the goldens hold unchanged: compiled `6774dbc7d0f4...`, `2f49f1096a92...`,
  `39889a7d71ac...`, `2db14f95af1f...`; python `17bbac987358...`, `935db8d66a98...`,
  `d806054c298c...`, `f706153f9a63...` (draws 1794, 2152, 2078, 2288).
- **Files changed:** no watched file; no lock field. (`DECISIONS-INDEX.md` and
  `RESULTS-roles-log.md` were split off for size, see the pointer above; no content changed.)
- **Carriers at 200 runs, compiled before (item 02) and after:** the 34 families problems
  x 200 runs (bench seed 1001, `tools/workload.py --runs 200`, one pinned core, `nice` 10,
  load 11.4, 2026-10-15 03:22): the rows digest `18c3d9e348b9...` (6,800 rows, 486,620
  codelets) is item 02's exactly, in 50.6 s of CPU against 107.7; read as distributions, the
  answers' total variation is 0.000 on every problem (the same sample). The GUI's compiled
  facts hold.
