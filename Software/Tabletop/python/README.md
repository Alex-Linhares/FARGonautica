# The `tabletop` package

A reconstruction of French's Tabletop from its specification (see [`../README.md`](../README.md)
for what Tabletop is, a quick start and how well it does). Python 3.12, standard library only;
tests need pytest.

**Status: complete (items 00-32 of the loop that built it); the final review (item 32)
found no departure from the conventions that changes a run, and its notes are in
`DECISIONS.md` under "As built (item 32)".** The
specification (`SPEC.md`), the digitized problems (`problems/`), every number French reports
as data (`tabletop/reported.py`), the decisions (`DECISIONS.md`: D-001 to D-077 the
reconstruction's, D-078 on the roles') with their knobs in one
frozen `Config` and four profiles (`tabletop/config.py`), the engine (geometry, the Slipnet,
the Workspace, perception, groups, salience, the Coderack, correspondences, the Worldview,
the temperature, the 23 codelet functions of French's catalogue, the context push and pop,
find-answer and the run loop), the typed events and observers, the text and JSON-lines traces with the fingerprint,
the read-only snapshot, the command line, the fifteen benches, the freeze
(`config_defaults.lock`), French's two calibrations as the `human` and `buridan` profiles,
the invariants checked after every codelet, random realistic tables, and `RESULTS.md` with
every bench at 500 runs, all exist and are tested. The dynamics are not yet French's:
`RESULTS.md` §8 traces every miss to a decision. **The GUI** (`tabletop/gui/`, built by a
second loop against the frozen engine, items 00 to 20 of its 21, complete) shows a run as it
happens in French's screen's layout, with sessions and replay, a palette and editor, a batch
pane and a user guide, has had its robustness pass, and its final review against its design
is [`docs/gui/review.md`](docs/gui/review.md). See
[The GUI](#the-gui). **The roles** (a third loop, D-078 on): the engine now reads the part
each item plays in the analogy, the loner, the sidekick, the anchor, the mirror image, the
counterpart, 23 roles in five families (`tabletop/roles.py`), with 15 Slipnet nodes (63 in
all), six role codelets (29 types) and eight consumers that read the roles into the
proposals, the slippages, the salience and the answer. They are part of the engine and
always on in every profile; nothing turns them off. The design is
[`docs/roles.md`](docs/roles.md), the numbers `RESULTS.md` §11, and every change of runs
since v1 (commit `e38d6e0`) is logged in `RESULTS.md`'s re-interpretation log. Every role
has its detector and a consumer (`roles.pending()` is empty); the last built were
CONTAINER (S3) and consumer C3, the answer from group position by the members' roles
(D-097; D-058's boolean became `group_position_match`). **The structural Blockage fix** (v5,
2026-10-17, D-116 to D-120) changed four mechanisms the study of Blockage 2, 9 and 10 found
at fault and bounded the proximity law by French's Blockage 10; `RESULTS.md`, "v5: structural
Blockage fix", and its Re-interpretation log give the numbers.

## Contents

- [Layout](#layout)
- [The module map](#the-module-map)
- [The profiles](#the-profiles)
- [The knobs](#the-knobs)
- [Installing](#installing)
- [Using the command line](#using-the-command-line)
- [The GUI](#the-gui)
- [How to read a trace](#how-to-read-a-trace)
- [Running the benches](#running-the-benches)
- [Running the tests](#running-the-tests)
- [The sources](#the-sources)
- [The quotation checker](#the-quotation-checker)
- [Conventions](#conventions)

## Layout

```
python/
  README.md  SPEC.md  DECISIONS.md  DECISIONS-INDEX.md  RESULTS.md  RESULTS-roles-log.md
  pyproject.toml  run-tests.sh
  config_defaults.lock # every profile's Config, frozen before the first full bench run (D-075)
  results/             # the benches' records at 500 runs on the compiled engine, stripped of the
                       # per-run lists, each naming its engine (D-075, D-107);
                       # results/v1/ the same records before roles, the v1 column (D-078),
                       # and results/v1/variants.json, the variants bench run on that commit (D-102);
                       # results/v2/ the roles engine's on its a-priori defaults, before v3 (D-114);
                       # results/v3/ v3's optimized defaults, before v4 (D-115);
                       # results/v4/ v4's, before the structural Blockage fix of v5 (D-116 to D-120)
  tabletop/            # the package (the module map below); tabletop/bench/ the benches;
                       # tabletop/codelets/ the 29 codelet functions (French's 23 and the six
                       # role codelets, D-081); tabletop/gui/ the
                       # PySide6 GUI (its own README.md is its module map; GUI-DECISIONS.md)
  problems/            # one JSON file per configuration, digitized from the figures (D-003)
  tests/               # pytest; tests/sources.py finds the source PDFs; tests/data/ the
                       # chapter 4 run, the worked examples, the golden traces per engine,
                       # the pure engine's recorded answer distributions (equivalence.json)
  tools/check_quotes.py     # the quotation checker
  tools/measure_figure.py   # measures a figure extracted under /tmp (frame, icons, boxes)
  tools/render_problem.py   # draws a problem file beside its figure, for review by eye
  tools/knobs_table.py      # prints the table of knobs below from config.py (--check)
  tools/role_probes.py      # the oracle and lesion probes of the roles, every consumer and detector lesioned on its carriers (test-only, D-104; python3 -m tools.role_probes)
  tools/check_links.py      # checks every Markdown link in both places (--online too)
  tools/audit_sample.py     # draws the citation audit's sample (docs/citation-audit.md)
  tools/standalone_check.sh # the standalone check of docs/standalone.md, end to end
  tools/run_on_baseline.py  # runs a bench against another commit's engine from a throwaway worktree (D-102), building the compiled engine there, or the pure one on --engine python (D-107)
  tools/workload.py         # times the families workload on the package of a given folder and digests its rows (D-107)
  tools/paired_timing.py    # paired CPU timing of engines, candidate beside baseline on two cores (D-107)
  tools/equivalence.py      # the answer distributions of the two engines, their total variation per problem, and the null (D-113)
  scripts/docs_figures.py   # redraws the front page's figures into docs/ (needs PySide6)
  scripts/docs_screenshots.py  # renders the GUI's screenshots into docs/ offscreen (gui extra)
  docs/                # problems.md, ch4-run.md, trace-format.md, engine-api.md, standalone.md,
                       # citation-audit.md, roles.md (the roles' design, D-078 on),
                       # predictions.md (the roles' registered predictions),
                       # roles-audit.md (v1 read in role vocabulary), gui/panes.md (the GUI's
                       # user guide), gui/review.md (the GUI reviewed against its design) and the
                       # front page's pictures (*.png: the figures and the GUI's screenshots gui-*.png)
```

- [`SPEC.md`](SPEC.md): the specification, every statement with its page, and the table of
  which module implements each section.
- [`DECISIONS.md`](DECISIONS.md): every choice the sources leave open, numbered, with its
  knob; [`DECISIONS-INDEX.md`](DECISIONS-INDEX.md) indexes them by number and by kind.
- [`RESULTS.md`](RESULTS.md): the benches' numbers beside French's and v1's, the findings,
  the calibration log and the re-interpretation log (every intended change of runs, with
  the before and after; the roles loop's entries are archived in
  [`RESULTS-roles-log.md`](RESULTS-roles-log.md)).
- [`docs/problems.md`](docs/problems.md): how the figures became problem files, and the
  file format. [`docs/ch4-run.md`](docs/ch4-run.md): French's chapter 4 run, dump by dump.
  [`docs/trace-format.md`](docs/trace-format.md): the two traces.
  [`docs/engine-api.md`](docs/engine-api.md): the events, the read-only state and the
  contract the GUI keeps. [`docs/gui/panes.md`](docs/gui/panes.md): the GUI's user guide,
  every pane with French's pages behind it. [`docs/gui/review.md`](docs/gui/review.md): the
  GUI reviewed against its design, every departure with its decision.
  [`docs/standalone.md`](docs/standalone.md): the
  folder as a repository of its own, the procedure that checks it and the results.
  [`docs/citation-audit.md`](docs/citation-audit.md): a random sample of the citations, every
  bar-graph value and ten layouts re-verified on the page images, with what was fixed.
- The roles' documents (D-078 on). [`docs/roles.md`](docs/roles.md): the design in the
  package's own words with French's pages: what a role is and its three readings, the five
  families and the registry of 23, the tag as a competing structure, the Slipnet's role
  nodes, the six role codelets, the re-cast, the eight consumers, the events and the trace,
  the GUI's panes, the honesty measures and the knobs.
  [`docs/predictions.md`](docs/predictions.md): the
  registered predictions, written before any role code (per role, per consumer, the twelve
  of the plan refined, the lesion and variants expectations, the held-out questions), never
  edited above its `## Scored` heading, where item 13 scores them, hits and misses alike.
  [`docs/roles-audit.md`](docs/roles-audit.md): every decision and knob of v1 read in role
  vocabulary, as covered, parameter gap or role gap, with the module, the function and the
  role or consumer that will read it.
- The pictures of the front page (`docs/*.png`): the figures drawn by
  [`scripts/docs_figures.py`](scripts/docs_figures.py) from the problem files, the records in
  `results/` and `results/v1/` (the results figures set French, v1 and v4, the roles on the
  second optimization's defaults, side by side, as RESULTS.md's tables do, D-078, D-115) and real runs, offscreen with PySide6
  (`pip install -e '.[docs]'`), as `python3 -m scripts.docs_figures`; the GUI's screenshots
  (`docs/gui-*.png`) rendered by
  [`scripts/docs_screenshots.py`](scripts/docs_screenshots.py) through the real window,
  offscreen, as `python3 -m scripts.docs_screenshots`: four from the quick start's run and
  the roles picture from a run of `blockage-07` that reads every family of roles. The object
  icons are new vector drawings, not French's.

## The module map

One module per job, as the decisions name them; the SPEC sections and the pages each module
implements. The tests are `tests/test_<module>.py` with few exceptions (`test_run.py` covers
`world.py`, `temperature.py` and `answer.py`; `test_codelets.py` the `codelets/` package;
`test_bench.py` and `test_benches.py` the benches).

**The data: the problems and French's numbers**

| Module | Responsibility | SPEC | Pages |
|---|---|---|---|
| `config.py` | every knob in one frozen `Config`; the four profiles (D-066), all four with roles (D-078); the 29 codelet types and their strip labels; `FAULT_KNOBS`, `CALIBRATION_KNOBS`; `__post_init__` refuses every role knob at zero or below, so no value turns a role or a consumer off (D-078), a `proximity_zero_gap` above `PROXIMITY_ZERO_GAP_MAX`, Blockage 10's bound (D-118), and an `answer_patience` at zero (D-120) | 5.2, 6b | [F95 p. 96 (img)]; DECISIONS.md |
| `rng.py` | the seeded generator: splitmix64, `below`, `choice`, `weighted_index`, `coin`, the draw counter, `run_seeds` (D-064); the specification of the compiled `_native/rng.pyx` (D-109) | 2.6 | [F95 p. 55; F95 p. 92] |
| `accel.py` | the engine seam (D-107): reads `TABLETOP_ENGINE` once, requires the compiled engine unless `python` is asked for, refuses a missing, stale or misnamed engine at import, exports `Rng`, `Slipnet`, `run_seeds`, `chances_of` (the Coderack's chances, D-112) and `info()` | — | DECISIONS.md |
| `_native/__init__.py`, `_native/rng.pyx`, `_native/slipnet.pyx`, `_native/coderack.pyx` | the compiled engine's sources (Cython): the pinned flags, the build stamp and `MODULES`; the generator in native 64-bit arithmetic, mirroring `rng.py` (D-109, D-110); the Slipnet in C arrays and C loops, mirroring `slipnet.py` operation for operation (D-111); the Coderack's chances in C, mirroring `temperature.chances_of` with Python's own float summation (D-112) | — | DECISIONS.md |
| `geometry.py` | the 110 × 100 table, distances (D-001); the nine regions by the centre and the blur draw (D-004); the diagonal and directly opposite regions and points, the relation and weight of a region to the touched object's (D-032); the halves as rows of regions (D-005); the gap between boxes (D-022); the four corners (D-085) | 1.3, 4.5 | [F95 p. 65; F95 p. 66; FCCA p. 388] |
| `objects.py` | the eleven types, the four silverware orientations, footprints (remembered per type and orientation, D-112), the footprint box of an object, the edge gap between two objects in units and in glass widths (D-002, D-006, D-022) | 1.2 | [F95 p. 26 (img); F95 p. 55; F95 p. 128] |
| `problems.py` | the problem files: loading, validation, the catalogue by family, French's expected answers (D-003, D-059) | 1.5, 1.6, 7.1 | [F95 pp. 26-35; F95 p. 114; FCCA p. 379] |
| `reported.py` | French's numbers as data: the 35 result sets, FCCA's 18, Tables 5.1-5.3, the other human data, the derived observations | 7 | [F95 pp. 113-145; FCCA pp. 394-399; FH91 p. 181] |
| `tables.py` | realistic random tables for robustness runs: `random_table(seed)` (place settings along the two rows, shakers between them, a stray object, the touched object by type weight), `realism_faults` (the rule a set table obeys, which every digitized layout passes), `band_of`, `describe` (D-068) | 1.4 | [F95 p. 38; F95 p. 39 (img)] |

**The Slipnet**

| Module | Responsibility | SPEC | Pages |
|---|---|---|---|
| `roles.py` | the roles (D-078, D-079): the closed registry of the 23 roles in five families (`RoleSpec`: reading, nodes, detector and consumers as the items build them, `pending()`), the `RoleTag` structure with its strength (`rate`: weight, node, the Worldview's support), `propose` with the fights between rival readings, `break_chance`, `detect`, the one perception entry point, and the re-cast (D-093): `signature_of`, the touched object's role signature read from its standing tags, and `recast`, the one place a role is carried across the table, in four tiers (`Recast`) | - | [F95 p. xv; F95 p. 6; F95 p. 12; F95 p. 118; F95 p. 130; F95 p. 131; F95 p. 140] |
| `slipnet_def.py` | the 63 nodes with levels and screen labels (the 48 of the reconstruction and the 15 role nodes, none on French's screen, D-080), the link table with a length and a source per row (the roles' 29 rows marked `re-interpretation`), the rows the dumps rule out (D-007 to D-010, D-080) | 3.1-3.3 | [F95 pp. 55-63; F95 pp. 96-107 (img)]; [FCCA p. 381] |
| `slipnet.py` | activation in 1-150, jolts, spreading from the warmer node, the label trickle, decay by depth, label-controlled lengths, the parallel-path distance, the initial state, the top-down posting hook (D-011 to D-020); `reset`, `restore` and `spread_step_into`, which the context push and pop use (D-021, D-070); the specification of the compiled `_native/slipnet.pyx` (D-111) | 3.4-3.10 | [F95 pp. 56-64; F95 pp. 82-89; F95 pp. 96-107 (img)] |
| `context.py` | the context push and pop (D-021, D-070): `context_of`, the group correspondence a codelet carries as an argument; `reset_set`, the nodes the push wipes clean (the supporting slippages' concepts and the two symmetries, or French's two nodes under `french-faults`); `push`; `pop`, the reconstruction of the top level from the Worldview's correspondences there, with the fixed amounts of pp. 88-89 and the one spreading step of the fix; `enter`, what the run loop does before each codelet; the `Context`, `Push` and `Pop` records; `context_mechanism` as the off switch | 3.10, 6b | [F95 pp. 85-89; F95 p. 108; FCCA p. 365] |

**The Workspace: perception, groups, salience, correspondences, the Worldview**

| Module | Responsibility | SPEC | Pages |
|---|---|---|---|
| `world.py` | the fresh `World` per run: the problem, the configuration, the generator, the Slipnet started on the touched type, the Workspace with the touched object in view, the Coderack with its starting set, the temperature at 80 (D-064, D-066); `step`, one codelet with everything that follows it: ageing out, the choice under a(T), the context entered (D-021), the follow-ups, the Slipnet update and its top-down postings, the temperature update, the find-answer schedule under the time pressure (`idle`, `time_pressure`, D-120), the cap (D-050, D-053, D-056, D-061); `Step`, what a step did; `snapshot()`; `drop_orphans`; `perceive_roles`, the roles of the objects a codelet just brought into view, read through `roles.perceive` (D-079) | 2.6, 2.7, 4.2 | [F95 p. 64; F95 p. 94; F95 p. 97; F95 p. 105; FCCA p. 386] |
| `workspace.py` | the objects in view with their drawn regions, the isolated ones, descriptions, groups by identity in the order built with their names, the innermost group of an object, the proposals, the correspondences and the Worldview flag, the competition tallies, the `competitions` and `changes` logs and the `awarded` rivalries (D-023, D-029, D-031, D-035, D-117); the role pool (D-079): `roles`, the tags standing in the order tagged (`r1`, `r2`, ...), `tag_role`, `discard_role` (a BARRIER takes the CLAIMED that names it, D-119), `discard_roles_on` (a tag goes with its group or correspondence), `roles_of`, `items_with`, `role_tag`, `has_role` (through an index of the pool by item, D-112), and `recast`, the last re-cast published (D-093) | 4.1-4.3 | [F95 pp. 63-65; F95 p. 75; F95 p. 76] |
| `perceive.py` | scanning: the region preferences relative to the touched object, the choice of a region under a(T), the look at a region that draws every object's region and notices what is seen; `across`, the "opposite" descriptor (D-004, D-023, D-032, D-071) | 4.2, 4.5 | [F95 p. 65; F95 p. 66; F95 p. 93; FCCA p. 386; FH91 p. 178] |
| `groups.py` | `Group`; the proximity criterion and the flood fill of a gestalt group from a seed, isolated objects (D-022, D-023); the three kinds of bottom-up group, strength, incompatible groups and their fights, subgroups absorbed with the fault restorable (D-024 to D-026, D-069) | 4.4 | [F95 p. 65; F95 p. 97; F95 p. 125; F95 pp. 128-129; F95 p. 140; FCCA pp. 382-384; FCCA p. 390] |
| `build_object.py` | `Proposal`; the candidate runs inside a gestalt group; build-obj-0, -1, -2: the physical, conceptual and compatibility micro-tests, the group created with its chain (D-024) | 4.4.2 | [F95 p. 65; F95 p. 77; F95 p. 125; F95 p. 140; FCCA p. 384] |
| `describe.py` | neighbours on four sides, mutual, and the chain of p. 80; ends and middles with left and right; orientation and linearity; counts; the group frame and the table frame of position (D-027 to D-029); the single-table role detectors: the midline (D-084), members and sidekicks (D-090), anchors (D-087) and connectors (D-088), the gaps of a row, loners and strays (D-086), corners (D-085) | 4.3, 4.4.5 | [F95 p. 63; F95 pp. 68-69; F95 p. 76; F95 p. 80; F95 p. 85; F95 p. 97; F95 p. 101]; CONTAINER's holder (`holder_under`, `holder_of`) and setting (`setting_of`) forms (D-087) |
| `salience.py` | French's ten factors as eight terms summed and recomputed when read, with the role term as a ninth (consumer C7, D-100: points for an anchor, a connector or a figure; a stray's region term scaled), the proximity term, the region term and the region order relative to the touched object, the group points, choices by salience under a(T) (D-031 to D-034, D-046) | 4.5 | [F95 pp. 66-67; F95 p. 93; F95 p. 97; FCCA p. 385] |
| `correspondences.py` | `Correspondence` and `Slippage`; the creation conditions (`creatable`, `create`, `inner_pairs` for examine-cp); the slippages with the nodes each feeds (D-030); the nine static factors, one function each, with the group-to-object penalty relaxed (D-045) and `type_weight` as the `human` knob, and the roles' tenth, the role slippage and `role_points` of consumer C5 (D-098); the three dynamic factors, the Hobj points in every rating (D-116); the bonus, once per rivalry (`award`, D-117); `describe` and the probabilistic `test` (D-035 to D-039) | 4.6 | [F95 pp. 67-72; F95 p. 108; FCCA p. 382] |
| `worldview.py` | the Worldview as the flagged subset of the Workspace's correspondences; rivals and usurping (`conflicts`, `rivals_of`, D-044); the `Rating` with its coherence and parallelism terms (`rate`, `current`, `pair_status`, `parallel`, D-042, D-043); the subjunctive Rival Worldview and the fight (`rate_subjunctively`, `rival_chance`, `compete`, D-040) with the awards (D-038; once per rivalry, D-117) and the entry jolts (D-030); losers fading and the clean-up (`removal_chance`, `clean_up`, `remove`, `remove_all`, `alive`, D-041); `challengers` for check-worldview; `violations`; the `Competition` and `Change` records; the Worldview roles (`symmetry_axis`, D-083; `type_keys`, D-094; `barrier_and_claimed` with `obvious_counterparts` and `blockers`, D-089) | 4.7 | [F95 pp. 70-75; F95 pp. 101-105; FCCA pp. 387-391] |

**The Coderack, the codelets, the temperature and the answer**

| Module | Responsibility | SPEC | Pages |
|---|---|---|---|
| `coderack.py` | `Codelet` (type, arguments, raw urgency, generation, age, who posted it, its last chance), raw / ρ^G with the floor, children one generation on, the rack in posting order with the capacity of 150 pushing the oldest off, ageing out, the choice by u^a with one draw (the chances through the seam, D-112), the screen's shares, the initial rack (D-052 to D-055, D-060) | 5.4-5.6 | [F95 pp. 76-82; F95 pp. 92-93; F95 p. 97] |
| `codelets/` | `catalogue.py`: the 23 types in the strip's order, each with its label, band (the urgency band, D-052), job and whether the job is in the text or inferred from the legend (D-051), then the six role codelets marked `re-interpretation` (D-081: propose-role-cp, consumer C1; check-roles, the role checker; find-roles, compare-roles and read-worldview-roles, the role scouts and the Worldview's reader; recast-role, the re-cast, D-093); the functions, one per type with their follow-ups (D-052): `scouts.py` (find-group, simple-sameness-detect, find-end-objs, find-middle-objs, find-sim-nhbr, find-neighbor, the three pick codelets, pick-salient-obj weighted by the re-cast as consumer C2, D-096, find-roles, compare-roles, propose-role-cp), `builders.py` (build-obj-0/1/2, build-obj-desc, create-cp, build-cp-descrip), `searches.py` (search-for-sam-gp, search-for-sim-gp, recast-role), `judges.py` (pick-strong-cp, with a barrier's correspondence as an argument for consumer C4, D-101; examine-cp, cp-compete, check-worldview with `barrier_fights`, clean-up-corresp, read-worldview-roles, check-roles), `finish.py` (find-answer); `common.py` (`Outcome`, follow-ups, fizzles, argument-less targets, the context arguments); `dispatch.py` (`FUNCTIONS`, `run_codelet`) | 5.2, 5.3, 6.5 | [F95 p. 63; F95 pp. 76-77; F95 p. 80; F95 p. 85; F95 p. 94; F95 p. 96 (img); FCCA p. 384] |
| `temperature.py` | the one law for every choice: a(T), softened weights, the chances of a set of weights (`chances_of`, the specification of the compiled `_native/coderack.pyx`, D-112), the yes-or-no form (D-060); `Temperature`: the four factors, the running average of ΔQ, the changes in the last ten competitions, T = α ΔQ/Q + β Δ_WV − γ T_cp clamped, held at 80 until the first change, the fixed-T lesion (D-047 to D-050) | 6.1-6.4 | [F95 pp. 90-93] |
| `answer.py` | the answer a find-answer codelet announces: the other end of the touched object's Worldview correspondence, a group's most salient member, the group-position reading by the members' roles, then the words, then the literal object (consumer C3, D-097; D-058), the default (D-057, D-063), recorded as `blocked` while a CLAIMED tag stands (consumer C4, D-101); the problem's labels (D-059) | 6.5 | [F95 p. 94; F95 p. 105; FH91 p. 180] |
| `run.py` | `run_problem(problem, config, seed, observers=()) -> Result`, `run_batch` over `run_seeds`, `report_line` (French's last line), `fingerprint(problem, seed, profile)` (the SHA-256 of the JSON-lines trace and the draw count), `trace_lines` | 2.6, 2.7 | [F95 p. 94; F95 p. 105; F95 p. 114; F95 p. 145; FCCA p. 392] |

**Observing a run**

| Module | Responsibility | SPEC | Pages |
|---|---|---|---|
| `events.py` | the 31 typed events (frozen dataclasses of plain data, each with the clock and the draw count; the last four are the roles', D-105), `EVENT_TYPES`, `record_of`; the `Publisher` every `World` owns (observers in order, errors caught and recorded) and the `Collector` (D-065; `docs/engine-api.md`) | — | [F95 p. 145] |
| `observe.py` | the observers: `TextTrace` (one line per codelet, French's report as the last line), `JsonLines` (one record per event), `Hasher` (the fingerprint's SHA-256); `report_sentence`, `codelet_text` (D-065; `docs/trace-format.md`) | 6.6 | [F95 p. 145; FCCA p. 392] |
| `snapshot.py` | `snapshot(world)`, a frozen copy of the whole state as plain data for an interface to read between events, drawing and storing nothing; `answer_now` (D-065; `docs/engine-api.md`, "The read-only state") | — | [F95 pp. 95-107] |
| `invariants.py` | the invariants a run must keep after every codelet, `check(world)` (the Slipnet's range, the Coderack's capacity and ages, the temperature's range, the structures standing on things that exist, the Worldview's rule, the role pool's rules, the end of the run) and `checked_run` (D-077) | 4.7, 5.6 | [F95 p. 71; F95 p. 82; FCCA p. 390] |
| `__main__.py` | the command line: `--list`, `--seed`, `--profile`, `--set`, `--quiet`, `--json`, `--batch`, `--table` (D-064 to D-066) | 6.6 | [F95 p. 114] |
| `gui/` | the PySide6 GUI, `python3 -m tabletop.gui` ([The GUI](#the-gui)): Qt-free pane models in `gui/models/` and thin views over them, the run controller on its own worker, the main window with one dock per pane, sessions and replay, the palette and editor, the results pane, the help; it only observes a run, through `World.step()` on its worker and `snapshot()`, and draws no random number. Its module map is [`tabletop/gui/README.md`](tabletop/gui/README.md); its decisions are [`GUI-DECISIONS.md`](GUI-DECISIONS.md); its user guide is [`docs/gui/panes.md`](docs/gui/panes.md) | — | [F95 pp. 95-111 (img)] |

**The baseline and the benches**

| Module | Responsibility | SPEC | Pages |
|---|---|---|---|
| `potelbat.py` | the brute-force baseline (D-067): `scores`, `ranked`, `answer`, `margin`; a type term from the intrinsic Slipnet distances and a place term from the two opposite places; no randomness | 1.7 | [FCCA pp. 327-328; FCCA pp. 355-356] |
| `bench/harness.py` | `survey(problem, config, seed, runs)`, `compare(survey, result_set)`, the tables in French's format, `record_of` for JSON, the per-problem batch seed (D-072) | 7.1, 7.2 | [F95 p. 114; F95 p. 115; F95 p. 123] |
| `bench/stats.py` | the statistics in one place: `z_score`, `two_sample_z`, `chi_square_homogeneity`, `chi_square_limit`, `chi_square_p`, `false_alarm` (D-072) | 7.1 | [F95 p. 114; FCCA p. 392] |
| `bench/families.py` | the `families` bench: the 35 chapter 5 result sets (D-072) | 7.2 | [F95 pp. 115-139] |
| `bench/fcca.py` | the `fcca` bench: FCCA's 18 reprinted sets, the two divergent ones against both versions (D-072) | 7.5, 7.6 | [FCCA pp. 394-399] |
| `bench/__main__.py` | `python3 -m tabletop.bench NAME --runs --seed --profile --problems --json --quiet`, `--list`; `all --out DIR` writing every bench's `NAME.md` and `NAME.json` (D-072, D-073) | 7 | [F95 p. 114] |
| `bench/probes.py` | what the probe benches share (D-073): `Variant`, `variant`, `survey_variants`, `variant_table`, `variant_records`, the readings of a survey (`frequency`, `label_frequency`, `literal_frequency`, `main_tally`, `mean_structure`, `french_main`), `ranks` and `spearman` | 7 | [FCCA p. 365] |
| `bench/humans.py` | the `humans` bench: Tables 5.1-5.3 under two profiles beside the subjects and Tabletop's second runs, Figure 5.4's order, the within-ten count and the rank correlations, the other human data (D-073) | 7.3, 7.4 | [F95 pp. 142-144; F95 pp. 118, 122; FCCA pp. 396-397; FH91 p. 181] |
| `bench/ch2.py` | the `ch2` bench: the nineteen configurations, French's `expected` block read as yes-or-no checks (D-073) | 1.6 | [F95 pp. 26-35] |
| `bench/ch4.py` | the `ch4` bench: Surround 5 stepped many times; the temperature and the rack shares at each dump's count and the milestones of French's run, read from `tests/data/ch4.json` (D-073) | 6c | [F95 pp. 96-107] |
| `bench/focus.py` | the `focus` bench: Surround 5 against 10 with French's two ratios; `junk_variant` and the scaling from 0 to 12 junk objects (D-073) | 7.2 | [F95 p. 121] |
| `bench/rho.py` | the `rho` bench: rho from 0.5 to 6 (D-073) | 5.5 | [F95 pp. 81-82] |
| `bench/lesions.py` | the `lesions` bench: FCCA's criterion 11, seven lesions as variants; `READINGS` and `verdict`, measure M7 (D-073) | 2.8 | [FCCA p. 365] |
| `bench/faults.py` | the `faults` bench: each fault restored alone, then all seven (D-073) | 6b | [F95 pp. 63, 88-89, 109, 125, 140-142] |
| `bench/potelbat.py` | the `potelbat` bench: the baseline's answer on every problem beside French's and Tabletop's (D-067, D-073) | 1.7 | [FCCA pp. 327-328] |
| `bench/stated.py` | the `stated` bench: 25 of French's statements of what the program does as yes-or-no checks over the runs, every run's facts read off its events, then chapter 2's series (D-074; M2) | 4.5, 6c | [F95 pp. 97-142] |
| `bench/temperature.py` | the `temperature` bench: the course of T over a run, many runs at a time: the curve against the codelets run (mean and quartiles of the runs still going), Surround 5's beside the dumps' thermometer, the T at the first Worldview change and at the answer, the updates by reason, the mean size of the three terms; two alternative readings as variants (D-073) | 6, 6c | [F95 pp. 90-93; F95 pp. 96-107] |
| `bench/robustness.py` | the `robustness` bench: random tables run once each with every invariant checked, the answers, run lengths and capped runs, the violations found, the role statistics (tags per role, changes, re-casts, C1's proposals; D-106), and a speed profile in runs per second (D-068, D-077) | 1.4 | [F95 p. 38] |
| `bench/variants.py` | the `variants` bench: eight variants of every problem file (mirrored, side-swapped, rotated, translated, type-substituted, junk-added, decoupled, on-saucer), each relabelled through its object map and run with the original's seeds; the total-variation distance with a bootstrap interval, the chi-square, the share of paired runs that kept their role, the mechanism measures (D-102) | 1.4, 7 | [F95 pp. 121, 140] |
| `bench/churn.py` | the `churn` bench: roles over time on the four carriers (role changes per run by reason, role and the temperature at the change; which fell on the answer; whether the churn settles) and the role codelets run before the answer against the find-answer flood (D-106) | 1.4 | [F95 p. 94] |
| `bench/results.py` | the results builder: `strip` the benches' JSON records of their runs into `results/`, `tables` and `update` the marked blocks of RESULTS.md from them; the measures M3-M7, M9; the fault marks of SPEC 6b; the calibration blocks (D-075, D-076) | 6b, 7 | [F95 pp. 114-115; FCCA pp. 355-356] |
| `bench/calibrate.py` | French's two calibrations after the freeze: `python3 -m tabletop.bench.calibrate human | buridan --runs --seed --split-seed --workers --json`; the recorded split of Tables 5.1-5.3, the two grids, least squares for `type_weight`, the E1-E2 balance on Buridan 1 for `incoherence_penalty`, a process pool for the scans (D-076; ANSWERS.md 7) | 7.3 | [F95 p. 130; F95 pp. 142-143] |

## The profiles

A run takes its configuration as data: `PROFILES[name]` is a frozen `Config`, and
`python3 -m tabletop PROBLEM --profile NAME` or `run_problem(problem, PROFILES[name], seed)`
uses it (D-066). Four profiles exist, and `config_defaults.lock` freezes every field of
every one of them (D-075): a change to the lock is made only for an intended change of
runs and is logged, a calibration in `RESULTS.md`'s calibration log, a change of the
engine in its re-interpretation log (D-078). **All four profiles run with roles**: the
role knobs (D-079 on) have the same values in every profile, no profile leaves a role or
a consumer out, and `french-faults` restores French's faults on top of the roles engine:

| Profile | What it is | Differs from `standard` in |
|---|---|---|
| `standard` | the defaults. **Since v5** (D-116 to D-120, 2026-10-17) the engine carries the structural Blockage fix: four mechanisms changed (the Hobj points in every rating, inertia per rivalry, the CLAIMED cascade, the time pressure), `proximity_zero_gap` at its bound 2.6 and `answer_patience` 1000 added (`RESULTS.md`, "v5: structural Blockage fix"). **Since v4** (D-115, 2026-10-17) 65 of them are fold 1 trial 800 of a second Bayesian optimization, chosen by the author although its cross-validated estimate does not beat v3's (`RESULTS.md`, "v4: second optimization (trial 800)"); `config.V3_DEFAULTS` holds v3's values. v3 (D-114, 2026-10-16) had set 67 of them to the configuration a cross-validated Bayesian optimization chose against French's result sets and the human data (fold 1 trial 244), the rest as v2 left them. Through v2 every value was set from the text, the screen dumps or the decision's stated reason, frozen before any bench ran at full size, **never tuned** to any result set (ANSWERS.md 1), with French's seven faults fixed; `config.V2_DEFAULTS` holds the v2 values, and v3 and v4 bring back fault 5 and parts of faults 2, 3 and 6 | nothing |
| `human` | French's first calibration, the object-type weight he raised for the second runs of Tables 5.1-5.3 [F95 p. 143], fitted after the freeze by least squares to the subjects' columns on a random half of the 23 problems (D-076; `RESULTS.md` §9.1); kept on top of v4, not re-fitted | `type_weight` 34 → 50 (fitted on v2 as `type_weight` 30 → 50) |
| `buridan` | French's second calibration, the incoherence penalty he raised until Buridan 1 was balanced [F95 p. 130], fitted after the freeze on Buridan 1 under the modified program (D-076; `RESULTS.md` §9.2) | `incoherence_penalty` 24 → 40 (v2's default, which the flat v2 scan kept; not re-fitted on v3 or v4) |
| `french-faults` | `standard` with the seven faults French names in his own program restored, for diagnosis only (ANSWERS.md 2; SPEC.md 6b; the `faults` bench); the roles stay on | `keep_all_subgroups` True, `group_object_penalty` 30, `group_salience_points` 15, `pop_restores_spread` False, `push_reset_set` "symmetries" (the default already since v3), `member_salience_in_group_strength` 0.2, `ends_from_chain` False |

`--set FIELD=VALUE` overrides any knob for one run (the trace's first line and the
`RunStarted` event record the change); `Config.differences(other)` lists what two
configurations disagree on.

## The knobs

Every knob of the reconstruction, from `tabletop/config.py`, in the order of the dataclass,
with its default and the page or decision that gives it. The table is generated by
`python3 tools/knobs_table.py` from the comments in the source (`--check` compares; the tests
run it), so it is never out of date. `D-NNN` is the decision in `DECISIONS.md`; a value with a
page is French's; "Other profiles" lists the profiles that change the knob.

<!-- begin knobs -->
**the profile (D-066)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `profile` | `"standard"` | `buridan` 'buridan', `human` 'human', `french-faults` 'french-faults' | D-066: the profile's name, recorded by every run and trace. |

**the table and its regions (D-004 to D-006)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `region_blur_width` | `1.7288679467971633` |  | D-004: the distance (units) within which a region boundary blurs; the probability of being put on the far side is region_blur_max * exp(-d / width). D-114 (v3, BO fold 1 trial 244): was 4.0. D-115 (v4, second BO fold 1 trial 800): was 2.2008565169053895. |
| `region_blur_max` | `0.07180613455879903` |  | D-004: the probability of crossing a boundary when standing on it. D-114 (v3, BO fold 1 trial 244): was 0.5. D-115 (v4, second BO fold 1 trial 800): was 0.05778812116655216. |

**the Slipnet (D-008 to D-021, D-070)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `depth_by_level` | `(45, 77, 79)` |  | D-008: the conceptual depth of levels 1, 2 and 3 [F95 pp. 61-62]. D-114 (v3, BO fold 1 trial 244): was (20, 50, 80). D-115 (v4, second BO fold 1 trial 800): was (45, 75, 45). |
| `label_shrink` | `0.1863732972649799` |  | D-011: a labeled link's length is scaled by (1 - label_shrink * a_label / 150). D-114 (v3, BO fold 1 trial 244): was 0.6. D-115 (v4, second BO fold 1 trial 800): was 0.19119336863601216. |
| `max_path_links` | `3` |  | D-012: paths of at most this many links enter the parallel-path distance. D-114 (v3, BO fold 1 trial 244): was 2. |
| `spread_constant` | `0.2681677074771584` |  | D-013: node A sends spread_constant * (a_A - a_B) / d(A, B) to each cooler B within spread_radius, the total capped at a_A - 1 [F95 pp. 59-61]. D-114 (v3, BO fold 1 trial 244): was 0.5. D-115 (v4, second BO fold 1 trial 800): was 0.19763673318675257. |
| `spread_radius` | `139.3105665668641` |  | D-013: nodes farther apart than this receive no spread. D-114 (v3, BO fold 1 trial 244): was 100.0. D-115 (v4, second BO fold 1 trial 800): was 89.08621192822841. |
| `decay_rate` | `0.12024525799397957` |  | D-014: a <- a - (a - 1) * (100 - depth) / 100 * decay_rate per update. D-114 (v3, BO fold 1 trial 244): was 0.1. D-115 (v4, second BO fold 1 trial 800): was 0.10537367710707346. |
| `jolt_structure` | `14` |  | D-016: the jolt when a structure is found or built [F95 p. 97]. D-114 (v3, BO fold 1 trial 244): was 40. D-115 (v4, second BO fold 1 trial 800): was 11. |
| `jolt_description` | `19` |  | D-016: the jolt when a descriptor is noticed. D-114 (v3, BO fold 1 trial 244): was 20. D-115 (v4, second BO fold 1 trial 800): was 14. |
| `jolt_label` | `6` |  | D-016: the jolt a slippage sends to its label node [F95 p. 62]. D-114 (v3, BO fold 1 trial 244): was 20. D-115 (v4, second BO fold 1 trial 800): was 9. |
| `initial_opposite` | `76` |  | D-017: the activation of "opposite" at the start [F95 p. 96 (img)]. D-114 (v3, BO fold 1 trial 244): was 139. D-115 (v4, second BO fold 1 trial 800): was 68. |
| `initial_touched_type` | `117` |  | D-017: the activation of the touched object's type at the start [F95 p. 96 (img)]. D-114 (v3, BO fold 1 trial 244): was 51. D-115 (v4, second BO fold 1 trial 800): was 147. |
| `focus_jolt` | `15` |  | The dumps hold the touched object's type and "opposite" near the ceiling for the whole run (cup 127-130, opposite 137-150 through 55 codelets [F95 pp. 98-107 (img)]): D-017: the jolt each receives before every Slipnet update (0 restores item 12's reading, in which they decayed; item 24). D-114 (v3, BO fold 1 trial 244): was 20. D-115 (v4, second BO fold 1 trial 800): was 9. |
| `label_trickle` | `0.01985159244084763` |  | D-018: the share of the mean activation of a label's two concepts that the label receives per update when both are above the threshold [F95 p. 62]. D-114 (v3, BO fold 1 trial 244): was 0.05. D-115 (v4, second BO fold 1 trial 800): was 0.024959712778966317. |
| `label_trickle_threshold` | `30` |  | D-018: "active", for the trickle. D-114 (v3, BO fold 1 trial 244): was 50. D-115 (v4, second BO fold 1 trial 800): was 32. |
| `slipnet_update_every` | `1` |  | D-019: codelets between Slipnet updates (a "cycle" [F95 p. 62]). |
| `top_down_table` | `(` |  | D-020: which codelet type an active node posts [F95 pp. 63-64; F95 p. 97]. |
| `top_down_rise` | `14.793932955471826` |  | D-020: a node posts rise / top_down_rise codelets per update, where rise is how much its activation grew since the last posting (the integer part, the rest with one coin), "as the rate of group-finding falls (i.e., the rate of change of activation of 'group' decreases) ... fewer codelets will be sent out" [F95 p. 64]; 40 is one structure jolt (D-016), so one find posts one more looker. D-114 (v3, BO fold 1 trial 244): was 40.0. D-115 (v4, second BO fold 1 trial 800): was 10.568496687557131. |
| `top_down_threshold` | `105` |  | D-036: "active", for the kind factor of a correspondence [F95 p. 68]: the level a node must reach (two thirds of the ceiling [F95 p. 95]). D-114 (v3, BO fold 1 trial 244): was 100. D-115 (v4, second BO fold 1 trial 800): was 95. |
| `pop_fixed_amount` | `98` |  | D-021: the amount a pop puts into each concept of a slippage it finds [F95 pp. 88-89]. D-114 (v3, BO fold 1 trial 244): was 30. D-115 (v4, second BO fold 1 trial 800): was 56. |
| `pop_restores_spread` | `True` | `french-faults` False | D-021, FIXED (ANSWERS 2): the pop also restores one spreading step from the neighbours [F95 p. 89]; False is French's lossy pop (`french-faults`). |
| `context_mechanism` | `True` |  | D-021: the context push and pop can be switched off (a lesion [FCCA p. 365]). |
| `push_reset_set` | `'symmetries'` |  | D-070, FIXED (ANSWERS 2): "slippage-concepts" resets every node named by the correspondence's supporting slippages [F95 p. 88]; "symmetries" only the two symmetry nodes (`french-faults`). D-114 (v3, BO fold 1 trial 244): was 'slippage-concepts'. |

**perception, groups and salience (D-022 to D-034, D-046, D-069)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `proximity_full_gap` | `0.1075377865053709` |  | D-022: the gap (in small-glass widths) up to which two objects always link. D-114 (v3, BO fold 1 trial 244): was 0.5. D-115 (v4, second BO fold 1 trial 800): was 0.1357240183058595. |
| `proximity_zero_gap` | `PROXIMITY_ZERO_GAP_MAX` |  | D-022: the gap at which the link probability reaches 0 [F95 pp. 128-129]. D-114 (v3, BO fold 1 trial 244): was 2.5. D-115 (v4, second BO fold 1 trial 800): was 4.935543682926514. D-118 (v5, the structural Blockage fix): was 5.478639330528657, which linked Blockage 10's glasses half the time; now the bound `PROXIMITY_ZERO_GAP_MAX`, the largest value allowed, pinned by French's words on Blockage 10 [F95 p. 129]. |
| `group_stage_pass` | `(("sameness", 0.7619120836473591),` |  | D-024: the chance of the conceptual micro-test by the group's kind [F95 p. 65; FCCA p. 384]. D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `shared_isa_max_length` | `22` |  | D-024: an ISA link of intrinsic length at most this joins a type to a category that can make a shared-ISA group [F95 p. 65]. D-114 (v3, BO fold 1 trial 244): was 30. D-115 (v4, second BO fold 1 trial 800): was 41. |
| `group_strength_weights` | `(("compactness", 57), ("size", 3), ("...` |  | D-025: the five factors of a group's strength [FCCA pp. 382-383]. D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `member_salience_in_group_strength` | `0.22218177813790996` | `french-faults` 0.2 | D-069, FIXED (ANSWERS 2): the share of the members' mean salience added to a group's strength and stage chance; 0.2 is French's fault [F95 pp. 141-142]. D-114 (v3, BO fold 1 trial 244): was 0.0. D-115 (v4, second BO fold 1 trial 800): was 0.20608913143316826. |
| `keep_all_subgroups` | `False` | `french-faults` True | D-026, FIXED (ANSWERS 2): True keeps every intermediate subgroup [F95 p. 140]. |
| `ends_from_chain` | `True` | `french-faults` False | D-028, FIXED (ANSWERS 2): ends from the chain of neighbours; False takes the extremes along the axis, one end per arm of an L [F95 p. 63]. |
| `salience_weights` | `(` |  | D-031: the salience factors [F95 pp. 66-67] (the group points are D-046). D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `middle_over_end_factor` | `2.7850438681534455` |  | D-031: "middle" must be at least this many times as active as "end" before the middles of a group take the position points instead of its ends, "unless, of course, the concept 'middle' is much more active" [F95 p. 66]. D-114 (v3, BO fold 1 trial 244): was 2.0. D-115 (v4, second BO fold 1 trial 800): was 4.054227501839888. |
| `group_salience_points` | `28` | `french-faults` 15 | D-046, FIXED (ANSWERS 2): the salience points for being a group [F95 p. 66]; 15 is the bias French calls "a bit too strong" [F95 p. 109]. D-114 (v3, BO fold 1 trial 244): was 10. D-115 (v4, second BO fold 1 trial 800): was 29. |
| `region_preference` | `(("diagonal", 0.6996372593354234), ("...` |  | D-032: the preference of each region relative to the touched object's [F95 p. 66; F95 p. 93]. D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `proximity_reach` | `11.115035683405221` |  | D-033: the distance (units) beyond which the proximity term is 0 [F95 p. 66]. D-114 (v3, BO fold 1 trial 244): was 40.0. D-115 (v4, second BO fold 1 trial 800): was 15.68728895366461. |

**correspondences and the Worldview (D-036 to D-046)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `cp_static_weights` | `(` |  | D-036: the static factors [F95 pp. 68-69], but the type weight below. D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `type_weight` | `34` | `human` 50 | D-036: the points for conceptually close object types, the weight French raised for his second runs [F95 pp. 142-143]; the `human` profile's knob (ANSWERS.md 7). D-114 (v3, BO fold 1 trial 244): was 30. D-115 (v4, second BO fold 1 trial 800): was 26. |
| `group_object_penalty` | `38` | `french-faults` 30 | D-045, FIXED (ANSWERS 2): the static points off a group-to-object correspondence; 30 is the bias French says "should probably be relaxed" [F95 p. 125]. D-114 (v3, BO fold 1 trial 244): was 15. D-115 (v4, second BO fold 1 trial 800): was 45. |
| `cp_dynamic_points` | `(("hobj", 10), ("hobj_neighbor", 6), ...` |  | D-037: the dynamic factors [F95 p. 69]. D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `win_bonus` | `3` |  | D-038: the bonus to every correspondence of a winning Worldview [F95 p. 72]. D-114 (v3, BO fold 1 trial 244): was 5. |
| `loss_penalty` | `6` |  | D-038: the penalty to the losing side [F95 p. 72]. D-114 (v3, BO fold 1 trial 244): was 8. D-115 (v4, second BO fold 1 trial 800): was 4. |
| `removal_threshold` | `126` |  | D-041: a non-Worldview correspondence below this rating may be removed [F95 p. 73]. D-114 (v3, BO fold 1 trial 244): was 20. D-115 (v4, second BO fold 1 trial 800): was 75. |
| `losers_destroyed` | `False` |  | D-041: True removes a loser at once, Copycat style (a lesion [FCCA p. 365]). |
| `incoherence_penalty` | `24` | `buridan` 40 | D-042: the penalty per incoherent pair in the Worldview rating [F95 p. 74; FCCA p. 387]; the `buridan` profile's knob [F95 p. 130] (ANSWERS.md 7). D-114 (v3, BO fold 1 trial 244): was 40. D-115 (v4, second BO fold 1 trial 800): was 27. |
| `coherence_reward` | `6` |  | D-042: the reward per coherent pair [FCCA p. 387]. D-114 (v3, BO fold 1 trial 244): was 10. D-115 (v4, second BO fold 1 trial 800): was 5. |
| `parallel_points` | `6` |  | D-043: the points per pair of parallel correspondences [F95 p. 74]. D-114 (v3, BO fold 1 trial 244): was 5. D-115 (v4, second BO fold 1 trial 800): was 16. |

**temperature (D-047 to D-050)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `temperature_alpha` | `634.263334931482` |  | D-047: alpha in T = alpha * dQ/Q + beta * dWV - gamma * Tcp [F95 p. 91 (img)]; 100 per unit of the running average's weight 1 / dq_window, so that one change of the whole of Q alone fills the thermometer. D-114 (v3, BO fold 1 trial 244): was 600.0. D-115 (v4, second BO fold 1 trial 800): was 304.28518892626295. |
| `temperature_beta` | `8.825853589620962` |  | D-047: beta. D-114 (v3, BO fold 1 trial 244): was 4.0. D-115 (v4, second BO fold 1 trial 800): was 15.522158275420585. |
| `temperature_gamma` | `0.9063537188894105` |  | D-047: gamma. D-114 (v3, BO fold 1 trial 244): was 0.5. D-115 (v4, second BO fold 1 trial 800): was 0.8149287013356975. |
| `temperature_fixed` | `-1.0` |  | D-047: a value >= 0 clamps T to it for the whole run (a lesion [FCCA p. 365]). |
| `dq_signed` | `False` |  | D-048: signed differences of Q (False: absolute) [F95 p. 90]. |
| `dq_window` | `18` |  | D-048: each update's rate is "averaged in with" the past rates [F95 p. 90]: dQ <- dQ + (rate - dQ) / dq_window, a running average with the weight of six. D-114 (v3, BO fold 1 trial 244): was 6. D-115 (v4, second BO fold 1 trial 800): was 14. |
| `wv_window` | `11` |  | D-048: changes of the Worldview in the last ten competitions [F95 p. 91]. D-114 (v3, BO fold 1 trial 244): was 10. D-115 (v4, second BO fold 1 trial 800): was 18. |
| `initial_temperature` | `81.86379423180182` |  | D-049: T before the first Worldview change [F95 p. 98 (img)]. D-114 (v3, BO fold 1 trial 244): was 80.0. D-115 (v4, second BO fold 1 trial 800): was 70.6525869148504. |
| `q_floor` | `0.04082205415935979` |  | D-049: the floor of Q, the Worldview's mean quality as a fraction of 1. D-114 (v3, BO fold 1 trial 244): was 0.05. D-115 (v4, second BO fold 1 trial 800): was 0.0383729024917592. |
| `temperature_update_every` | `3` |  | D-050: codelets between temperature updates when nothing changed. D-115 (v4, second BO fold 1 trial 800): was 5. |

**the Coderack (D-052 to D-056)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `urgencies` | `(` |  | The correspondence chain (create-cp, build-cp-descrip, cp-compete) is at the top of the range, so that after the generation discount a found pair is pursued ahead of the scouts, as the dumps show (no CT-CP or BLD-CP bar in nine dumps while correspondences are built and enter within ten codelets [F95 pp. 98-107 (img)]; item 24). D-052: the raw urgency of each codelet type, in 1-100 [F95 p. 81]; find-answer's is set at posting (D-056). D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `urgency_floor` | `0.040642816704546704` |  | D-052: the floor of an adjusted urgency raw / rho ** generation. D-114 (v3, BO fold 1 trial 244): was 0.01. D-115 (v4, second BO fold 1 trial 800): was 0.03709378761755724. |
| `rho` | `1.5329834865509773` |  | The urgency-reduction coefficient rho [F95 p. 81]. D-114 (v3, BO fold 1 trial 244): was 3.0. D-115 (v4, second BO fold 1 trial 800): was 1.5756786574636277. |
| `exponent_at_zero` | `7.434572541903903` |  | The temperature exponent a at T = 0 [F95 p. 92] (D-060: a(T) is linear between them). D-114 (v3, BO fold 1 trial 244): was 6.0. D-115 (v4, second BO fold 1 trial 800): was 6.699341892946163. |
| `exponent_at_hundred` | `0.13592262854844722` |  | The temperature exponent a at T = 100 [F95 p. 92]. D-114 (v3, BO fold 1 trial 244): was 0.1. D-115 (v4, second BO fold 1 trial 800): was 0.21261646315192564. |
| `max_codelet_age` | `196` |  | D-053: a codelet older than this (in codelets run) is removed [F95 p. 76]. D-114 (v3, BO fold 1 trial 244): was 60. D-115 (v4, second BO fold 1 trial 800): was 216. |
| `coderack_capacity` | `260` |  | The Coderack's capacity; the oldest are pushed off [F95 p. 82]. D-114 (v3, BO fold 1 trial 244): was 150. D-115 (v4, second BO fold 1 trial 800): was 179. |
| `initial_codelets` | `(("find-group", 56), ("pick-2nd-salie...` |  | D-055: the starting codelets [F95 p. 65; F95 p. 98 (img)]; one find-roles since item 04 of the roles loop (D-081, D-084: the midline is read early). D-114 (v3, BO fold 1 trial 244): rows changed; the v2 values (whose reasons the comments here give) are in D-114's table. D-115 (v4, second BO fold 1 trial 800): rows changed; the v3 values are `V3_DEFAULTS`' and D-115's table. |
| `failed_test_removes` | `True` |  | D-039: a correspondence that fails its initial test is removed from the Workspace, to be proposed again by a later pick (True), or stays tentative for a pick-strong-cp to test again (False, item 17's reading) (item 24). |
| `answer_post_rate` | `1.163287752380892` |  | D-056: answer_post_rate * ((100 - T) / 100) ** answer_post_power find-answer codelets are posted after each codelet (the integer part, the rest with one coin) [F95 p. 94]: none at a high temperature, a flood at a low one [F95 pp. 98-107 (img)]. D-114 (v3, BO fold 1 trial 244): was 10.0. D-115 (v4, second BO fold 1 trial 800): was 1.0933454350080711. |
| `answer_post_power` | `6.6307295348989665` |  | D-056: the power of the posting law. D-114 (v3, BO fold 1 trial 244): was 3.0. D-115 (v4, second BO fold 1 trial 800): was 7.795829030201836. |
| `answer_urgency_max` | `21.381195132739556` |  | D-056: its raw urgency is answer_urgency_max * ((100 - T) / 100) ** power, at least 1 [F95 p. 105]: negligible at high temperature, the top of French's range [F95 p. 81] at T = 0. D-114 (v3, BO fold 1 trial 244): was 100.0. D-115 (v4, second BO fold 1 trial 800): was 27.26120591316952. |
| `answer_urgency_power` | `2.860258435717891` |  | D-056: the power of the urgency law. D-114 (v3, BO fold 1 trial 244): was 3.0. D-115 (v4, second BO fold 1 trial 800): was 1.6674835999126352. |
| `answer_patience` | `1000` |  | D-120 (v5): the time pressure's patience, in codelets: while the Worldview has changed within the last answer_patience codelets (or, before any change, the run is younger than that) the two laws above read T as it is; past it they read T x (1 - pressure), the pressure rising from 0 to 1 over the next answer_patience codelets, so that a table where no structure can form still ends with the default answer [F95 p. 105] well before the cap (D-061), under "a competing pressure - time pressure" [FCCA p. 387]. Above every idle stretch measured on the 35 result sets (RESULTS.md, v5): runs where structure forms are unchanged. Refused at zero or below. |

**the answer and the run (D-058, D-061)**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `group_position_match` | `"roles"` |  | D-097 (D-058 revised): the answer when the touched object is mapped only through its group: "roles" (consumer C3: the member with the same roles, then the same group-frame words, then the literal object) or "words" (the words, then the literal). |
| `max_codelets` | `3000` |  | D-061: the run cap, in codelets; a capped run answers as it stands. |

**the roles (D-079 on); every value below zero or at zero is refused**

| Knob | Default | Other profiles | Source or decision |
|---|---|---|---|
| `role_weight_points` | `4.995662058779633` |  | D-079: a tag's strength takes these points per unit of its weight (the objects in the item carrying the role). D-114 (v3, BO fold 1 trial 244): was 10.0. D-115 (v4, second BO fold 1 trial 800): was 6.3640660568260445. |
| `role_node_points` | `11.875393095617262` |  | D-079: the points a tag takes from its node's activation over 150. D-114 (v3, BO fold 1 trial 244): was 30.0. D-115 (v4, second BO fold 1 trial 800): was 14.092581291430136. |
| `role_story_points` | `74.22679372210203` |  | D-079: the points per Worldview correspondence with the tag's item, or an item it names, at an end: the story's support. D-114 (v3, BO fold 1 trial 244): was 20.0. D-115 (v4, second BO fold 1 trial 800): was 68.31619729057013. |
| `role_break_scale` | `154.5748536747861` |  | D-079: a re-examined tag breaks with chance 1 - strength / this, under the temperature law; a tag at or above it cannot be broken. D-114 (v3, BO fold 1 trial 244): was 100.0. D-115 (v4, second BO fold 1 trial 800): was 261.0167640762452. |
| `role_pair_floor` | `4.356269749299028` |  | D-095: the floor of a pair's weight in consumer C1's choice, so that no pair a role names is impossible to propose. D-114 (v3, BO fold 1 trial 244): was 1.0. D-115 (v4, second BO fold 1 trial 800): was 2.392482515436875. |
| `role_recast_tiers` | `(8.611247787299543, 3.751299142362098...` |  | D-093: the re-cast's tier weights, first to fifth tier (the image under the axis with the signature, the signature in the same column, the same type, the rest of the row, and last the candidates CLAIMED by a standing correspondence, D-101), a priori and halving; consumer C2 multiplies a candidate's salience by one plus its tier's weight (D-096). Every value refused at zero or below. D-114 (v3, BO fold 1 trial 244): was (8.0, 4.0, 2.0, 1.0, 0.5). D-115 (v4, second BO fold 1 trial 800): was (11.85241303266348, 1.90191195551275, 5.824314877208685, 0.3522396443338284, 1.8269391338241927). |
| `stray_salience_scale` | `0.8655580465974726` |  | D-100: consumer C7 multiplies a stray's region term (D-031) by this, a priori a half, French's implicit pruning of the junk; refused at zero or below. D-114 (v3, BO fold 1 trial 244): was 0.5. D-115 (v4, second BO fold 1 trial 800): was 1.1999777021291365. |
<!-- end knobs -->

## Installing

**Requires a C compiler and Cython to build the engine; the pure-Python engine is included
as the readable specification and can be run explicitly** (D-107). The engine that runs by
default is the compiled one, the reference: build it once, from `python/`, with a C
compiler (gcc or clang) and Cython 3 (`pip install cython`, or any `cython` executable
named by `TABLETOP_CYTHON` or passed as `--cython PATH`):

```sh
python3 tools/build_fast.py          # at most 4 jobs; embeds the build stamp (D-110)
```

`pip install .` (or `-e .`) builds the same extension through `setup.py`: the build
requirements, `setuptools` and `Cython`, are declared in `pyproject.toml` and fetched by
pip into its build environment; the runtime dependencies stay empty (offline,
`pip install --no-index --no-build-isolation -e .` in a venv that sees the system's
packages needs Cython from `TABLETOP_CYTHON` or `PATH`). Without a C compiler, or without
Cython where pip cannot fetch it, the build stops with a message naming what is missing
and leaves no half-built extension. Without a build, every entry point (`python3 -m
tabletop`, the benches, the GUI, which says so in a dialog, `run_problem`) stops at start
with an error that names the build command; there is no silent fallback. The pure-Python
engine runs only on the explicit request `TABLETOP_ENGINE=python` (any other value is an
error), and every output then says `engine python`, the GUI in its status bar.
`tools/standalone_check.sh` checks all of this on a fresh checkout
([`docs/standalone.md`](docs/standalone.md)). Nothing else to install for the engine, the
command line or the benches. For the tests:

```sh
pip install -e '.[test]'        # pytest, and pytest-xdist for parallel runs
```

For the GUI (`python3 -m tabletop.gui`, [The GUI](#the-gui)), `pip install -e '.[gui]'`:
PySide6 and pytest-qt, which runs the GUI's tests (`tests/gui/`) offscreen; without it they
skip (`GUI-DECISIONS.md` G-004). To redraw the front page's figures, `pip install -e
'.[docs]'` (PySide6), then `python3 -m scripts.docs_figures`; the GUI's screenshots are
`python3 -m scripts.docs_screenshots` (the `gui` extra).

## Using the command line

From `python/` (nothing to install):

```sh
python3 -m tabletop surround-05 --seed 3              # one run: one line per codelet, French's report last
python3 -m tabletop surround-05 --seed 3 --quiet      # the first and last lines only
python3 -m tabletop surround-05 --seed 3 --json run.jsonl   # the JSON-lines trace too (docs/trace-format.md)
python3 -m tabletop surround-05 --seed 3 --json -     # the JSON lines alone, on stdout
python3 -m tabletop surround-05 --profile french-faults     # another profile (standard, buridan, human, french-faults)
python3 -m tabletop surround-05 --set rho=2 --set initial_temperature=100   # override Config fields
python3 -m tabletop surround-05 --batch 50 --seed 1   # fifty runs from one batch seed, a summary table
python3 -m tabletop surround-05 --table               # the layout as text, then the run
python3 -m tabletop --list                            # every problem with its answers, figure and page
```

A run is reproducible from its problem name, seed and profile (D-064); the same run in
Python:

```python
from tabletop.problems import load_problem
from tabletop.config import PROFILES
from tabletop.run import run_problem, report_line

result = run_problem(load_problem("surround-05"), PROFILES["standard"], seed=3)
print(report_line(result))      # Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3.
```

`Result` carries the answer and its label, whether it was literal and how it was reached,
whether the run was capped, the structure value, the codelet count, the final temperature,
the Worldview's summary, the counts of structures, competitions and changes, and the
generator's draw count; `fingerprint(problem, seed, profile)` gives the SHA-256 of the
JSON-lines trace and the draw count, the same in any process. Observers (`observers=[...]`)
receive every event and cannot change the run: `docs/engine-api.md`.

## The GUI

`python3 -m tabletop.gui` (or `tabletop-gui` once installed) opens a window where you choose
a problem, a profile and a seed, press Play, and watch the run one event at a time (a worked
example, three fights for the Worldview in a run of Figure 2.11, is
[`docs/gui/worldview-fights.md`](docs/gui/worldview-fights.md)): the
table with its groups and correspondences, the thermometer, the Slipnet as French's grid
and as a network, the Coderack as his bar strip and as a list, the Worldview and the
competitions, the trace exactly as the command line prints it, and the timeline, in the
layout of French's screen [F95 pp. 95-107 (img)]. It needs PySide6 (6.6 or later; tested
with 6.11), an optional dependency: without it the engine, the tests and the command line
work as before, and `python3 -m tabletop.gui` says what to install.

```text
python3 -m pip install -e '.[gui]'          # the GUI (PySide6 and pytest-qt)
python3 -m tabletop.gui                     # or: tabletop-gui
python3 -m tabletop.gui --problem blockage-06 --seed 2 --profile human --play
```

![The window at the last codelet of surround-05 seed 3](docs/gui-window.png)

The window at the last codelet of the quick start's run (`surround-05`, seed 3): the table in
the centre, the thermometer beside it, the Slipnet grid on the right over the Worldview (the
competitions, the temperature plot and the roles are its tabs), the controls, the palette
and the stats on the left, the Coderack bars and the trace along the bottom, the timeline
under them. The panes, briefly (the user guide,
[`docs/gui/panes.md`](docs/gui/panes.md), describes each with French's pages behind it, and
Help > "What am I looking at?" shows its table inside the program):

- **Table** (centre): the objects as new vector icons (not French's), the touch H and the
  answer E, the groups as wavy boxes, the correspondences as lines, solid inside the
  Worldview and dashed outside, as the screen dumps draw them; overlays for the nine
  regions, the salience and the region ranks, the roles (each item's standing role codes
  as a badge, the midline band and the axis, the images, the re-cast's tiers, a barrier's
  claim; G-061) and Eliza's view of the table (the GUI's own).
- **Thermometer** and **Temperature plot**: French's column, filled to the temperature; the
  plot of T and its three terms over the codelets, with his chapter 4 readings on request
  (the plot is GUI design, not French's).
- **Slipnet grid** and **Slipnet network**: his 41 boxes with the activation as a black
  square and a number, plus a seventh row for the nodes his screen lacks and a band of
  the 15 role nodes under it, labelled as not on French's screen (D-080), and his nearest
  chapter 4 dump in red on request; the nodes and links at their current lengths, the role
  nodes among them, with the context push and pop marked (the network is GUI design, not
  French's, after Figure 3.6).
- **Coderack bars** and **Rack list**: his strip of 29 bars (French's 23 and the six role
  codelets, D-081) with the count run; the
  codelets waiting with their urgencies, generations, ages and chances (the list is GUI
  design, not French's).
- **Worldview** and **Competitions**: the correspondences in the Worldview and the shadow
  cabinet with the rating and its pairs, the proposals; each fight with its Old and Rival
  Worldviews (both GUI design, not French's).
- **Roles** (a tab beside the Worldview; GUI design, a re-interpretation, G-060): the role
  tags of the run by family, each with its item, weight, strength, evidence and readers
  and, once lost, when and why it went; the symmetry axis the Worldview reads and the last
  re-cast with its tiers; exact at every event when seeking.
- **Trace and event log**, **Timeline**: the trace as the CLI prints it (quiet or full), every
  event filtered by kind, Save as text or JSON lines; the slider with the competitions,
  changes, pushes and the answer marked and a strip of T.
- **Controls**, **Stats**, **Configuration** (Edit > Configuration), **Palette and editor**
  (Edit > Edit table) and **Results** (Run > Batch…): the inputs and the run buttons; the
  run's numbers and French's report line; every knob with its page, editable for the next
  run; French's palette, where a table is set by dragging the icons and run with Do this!;
  a batch of runs with French's numbers beside ours.

Every event is drawn: after each one, every view is redrawn before the run goes on, and the
same seed gives the same run with or without the window (the GUI only observes the engine,
through `World.step()` on its worker and `World.snapshot()`; it draws no random number and
changes nothing). Keys (the Run menu): Space play/pause, Right step, Shift+Right step
codelet, Esc stop, Left one event back, Ctrl+N a new run. Session > Save session writes the
run's problem, profile, edits, seed and events as the JSON-lines trace; Open session re-runs
the file and checks every event against it. The window's size, docks, inputs, overlays and
speed are remembered between sessions (QSettings `tabletop`/`tabletop-gui`); View > Reset
layout restores the layout shipped with the code, `tabletop/gui/default_layout.json`, and
View > Save layout as default for this code… rewrites that file.

Options: `--problem NAME`, `--seed N`, `--profile NAME`, `--set FIELD=VALUE` (repeatable),
`--play` to start at once. `--smoke` runs the problem to the end with every event drawn,
offscreen (no display needed), prints French's report line and exits with its status: 0 for
an answer, 1 for a capped run, 2 for a usage error. It reads and writes no settings:

```sh
python3 -m tabletop.gui --smoke --problem surround-05 --seed 3
```

```
Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3.
```

The same line ends the command line's quiet run:

```sh
python3 -m tabletop surround-05 --seed 3 --quiet
```

```
tabletop surround-05  seed 3  profile standard  engine compiled  Henry touches cup1
Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3.
```

The GUI's module map is [`tabletop/gui/README.md`](tabletop/gui/README.md), its tests are
[`tests/gui/`](tests/gui/) (offscreen, skipped without PySide6), its decisions are
[`GUI-DECISIONS.md`](GUI-DECISIONS.md), and its screenshots (`docs/gui-*.png`) are rendered
by `python3 -m scripts.docs_screenshots`.

## How to read a trace

`python3 -m tabletop surround-05 --seed 3` prints 384 codelet lines for the run of the quick
start (on v4's defaults, D-115, with v5's structural Blockage fix, D-116 to D-120). Its first
lines:

```text
tabletop surround-05  seed 3  profile standard  engine compiled  Henry touches cup1
   1  T=81.9  a=1.5  FND-GP()  -> found g1 {spoon3 spoon4 small-glass1 fork3 fork4 knife2} gestalt horizontal; jolt knife +19; jolt group +14; jolt horizontal +19; posted 9 (FND-GP SAM FD-NNBR x2 FD-END FD-MID PK-SALO x2 FND-RL); jolt column +19; jolt group +19
      slipnet posts FND-GP x2 PK-SALO x2 PK-SALC RC-RL
      RL tag r1 knife2 COLUMN:middle w1 s5 by find-group
      RL tag r2 knife2 MEMBER w1 s6 by find-group
```

- The first line names the problem, the seed, the profile (and every knob changed with
  `--set`, as `field=value`) and the object Henry touched.
- Each codelet line starts with the codelet's ordinal, the temperature `T` at its choice and
  the exponent `a` = a(T) that temperature set, linear between its values at T = 0 and
  T = 100 (French's 6 and 0.1 [F95 p. 92], 7.43 and 0.136 since v4's defaults, D-115): at
  T = 81.9, the starting temperature, a = 1.5 and the choice among codelets is close to
  uniform, the urgent ones only a little favoured.
- Then the codelet, by the label of French's Coderack strip [F95 p. 107 (img)] with its
  arguments in parentheses and, after a slash, its generation when it is not 0: `FND-GP()`
  is a find-group scout with no argument; `CT-CP(cup2 cup1)/1` a create-cp of generation 1
  on the pair cup2, cup1. The legend of the 29 labels (French's 23 and the six role codelets, D-081) is `tabletop/codelets/catalogue.py`
  (`python3 -c "from tabletop.codelets.catalogue import CATALOGUE; ..."`).
- After `->`, what the codelet did, in a fixed vocabulary (`found`, `built`, `proposed`,
  `passed`, `failed`, `picked`, `described`, `entered`, `fight`, `removed`, `answer`) or
  `fizzled (reason)`; then its side effects after semicolons: every jolt it sent to the
  Slipnet (`jolt knife +19`) and the follow-ups it posted (`posted 9 (FND-GP SAM FD-NNBR x2
  ...)`). Codelet 1 looked at a region and found Eliza's row of six as a gestalt group,
  `g1`, horizontal, and posted the eight follow-ups of p. 76 and a find-roles on the group
  (D-081); the knife it brought into view was tagged with its column and as a member at
  first sight (`RL tag r1 knife2 COLUMN:middle w1 s5 by find-group`, `RL tag r2 knife2
  MEMBER ...`, D-085, D-090), and the jolts to `column` and `group` made the Slipnet post
  more scouts, the pick codelets and a recast-role (`RC-RL`).
- An indented line reports what happened between codelets: `slipnet posts ...` for the
  codelets a rising concept posted top-down (D-020), `answer schedule: FND-ANS x1 at
  urgency 12.0` for the find-answer codelets hung (D-056), `T 81.9 -> 40.7 (dQ 0.03 Q 0.59
  dWV 1 Tcp 1) by change` for a temperature update with the formula's four inputs (D-047 to
  D-050), `aged out:` and `pushed off:` for codelets leaving the rack unrun, and `RL tag`,
  `RL drop`, `RL used`, `RL recast` for a role tag built, discarded or read by a consumer,
  and for the re-cast of the touched object's signature (D-105).

The middle of the run:

```text
   3  T=81.9  a=1.5  FND-RL()  -> found r3 table MIDLINE r4 g1 MIRROR_IMAGE:cup1 r5 g1 ROW:below r6 g1 COLUMN:left r7 g1 CORNER; jolt separator +19; jolt mirror symmetry +19; jolt row +19; jolt column +19; jolt corner +19; posted 2 (CMP-RL PR-RCP)
      RL tag r3 table MIDLINE w1 s5 by find-roles
      RL tag r4 g1 MIRROR_IMAGE:cup1 w6 s30 by find-roles
   5  T=81.9  a=1.5  PR-RCP()  -> proposed g1 and cup1 as mirror_image pair; posted 2 (CT-CP CHK-RL)
      RL used g1 MIRROR_IMAGE by C1: create-cp g1 cup1
      RL used g1 ROW by C1: create-cp g1 cup1
 209  T=81.9  a=1.5  CT-CP(spoon3 cup1)/2  -> passed cp29 spoon3=cup1 static 41 p=0.41; posted 1 (CP-COMP)
 211  T=81.9  a=1.5  CT-CP(small-glass1 cup1)/1  -> passed cp30 small-glass1=cup1 static 56 p=0.56; posted 1 (CP-COMP)
 217  T=81.9  a=1.5  CP-COMP(cp29)/3  -> entered cp29 spoon3=cup1 entered the Worldview without a fight, rating 53; jolt mirror symmetry +6; jolt sameness +6; jolt liquid holder +6; jolt sameness +6; posted 4 (BLD-CP CP-CLN CHK-WV RD-WVR)
 226  T=35.6  a=4.8  CT-CP(cup2 cup1)/1  -> passed cp33 cup2=cup1 static 64 p=0.64; posted 1 (CP-COMP)
 239  T=30.4  a=5.2  CP-COMP(cp30)/2  -> fight cp30 small-glass1=cup1 vs cp29: old 53 vs rival 68, p=0.78, rival wins; jolt mirror symmetry +6; jolt sameness +6; jolt smaller than +6; jolt bigger than +6; jolt sameness +6; posted 4 (BLD-CP CP-CLN CHK-WV RD-WVR)
 250  T=35.8  a=4.8  CP-COMP(cp33)/2  -> fight cp33 cup2=cup1 vs cp30: old 71 vs rival 75, p=0.56, rival wins; jolt opposite +6; jolt diagonal symmetry +6; jolt sameness +6; jolt sameness +6; posted 4 (BLD-CP CP-CLN CHK-WV RD-WVR)
```

Codelet 3, the find-roles, read the midline and Eliza's row as the mirror image of Henry's
cup across it (`MIRROR_IMAGE:cup1`, an image role, D-099), in its row, its column and the
corner it fills (D-085). At codelet 5 consumer C1 proposed that pair, the row and Henry's
cup, by the image the find-roles had read; the create-cps it posted failed again and
again, a group-to-object correspondence rated below zero by v4's penalty (D-045), and for
two hundred codelets nothing entered the Worldview and the temperature held at its start.
At codelet 209 a create-cp built the correspondence `cp29` (spoon3 = cup1, the ends in
the picking order), rated its static points at 41 and passed its test at that chance
(D-039); at 211 another built `cp30`, Henry's cup to the glass in Eliza's row, and at 217
cp-compete found no rival for the spoon's and put it in the Worldview, which jolts the
label nodes of its slippages (D-030) and posts the clean-up, the check, the describer and a
re-reading of the Worldview's roles (`RD-WVR`, D-083). The Worldview's first change
recomputes the temperature, here down to the thirties. At codelet 239 the glass's `cp30`
fought the spoon's for the Worldview and won at the chance 0.78 that the two ratings gave
(D-040): the cup's correspondence, whichever it is, keeps its Hobj points in every rating
since v5 (D-116), so both sides here carry them and the statics decide. At 226 a create-cp
had built `cp33`, Henry's cup to the lone cup at the far right, and at 250 it beat the
glass's at 0.56. The end:

```text
 346  T=38.3  a=4.6  CP-COMP(cp30)/1  -> fight cp30 small-glass1=cup1 vs cp33: old 71 vs rival 60, p=0.32, old holds; posted 2 (CP-CLN CHK-WV)
      T 38.3 -> 36.6 (dQ 0.02 Q 0.72 dWV 4 Tcp 16) by competition
 382  T=28.8  a=5.3  CHK-WV()/3  -> fizzled (no challenger)
 383  T=28.8  a=5.3  CHK-RL()/2  -> removed r30 g2 ROW:below broken at strength 21
      RL drop r30 g2 ROW (broken)
 384  T=28.8  a=5.3  FND-ANS()  -> answer Eliza touches cup2 (E2)
Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3.
```

The three correspondences of Henry's cup traded the Worldview sixteen times in all: the
glass's `cp30` won it back at 293 and lost it again at 322, and from 338 came back three
times against the lone cup's `cp33` and lost each time (codelet 346 is the last), at a
chance of about a third that stays a third, since a rematch of one rivalry adds no second
penalty (D-117). A find-answer
codelet, hung by the schedule as the temperature fell (D-056: at high temperature
"unlikely, but theoretically possible" [F95 p. 105], at higher urgency as T falls),
announced the other end of the touched cup's Worldview correspondence as it stood (D-057):
the isolated cup at the far right, labelled with the problem file's `E2`, French's second
answer (his program took the glass in 86 percent of its runs; the check-roles at 383 broke a
weak ROW tag on the way, D-079). The last line is French's
report [F95 p. 145]: the object
touched, the structure value of the Worldview (D-062), the codelets run and the final
temperature; `literal` follows the label when Eliza touched Henry's own object (D-063), and
`(capped)` marks a run stopped at the cap (D-061). `--json` writes the same run as one JSON
record per event; `docs/trace-format.md` defines both traces.

## Running the benches

French's experiments, replayed with our numbers beside his (D-072; ANSWERS.md 1: every
result set is a measurement target and none is ever tuned to). A quick look, from `python/`:

```sh
python3 -m tabletop.bench --list                                  # the fifteen benches
python3 -m tabletop.bench families --problems surround-05 blockage-06 --runs 3 --quiet   # two result sets, three runs each
python3 -m tabletop.bench stated --problems surround-05 --runs 2 --quiet                 # French's statements, two runs
```

The full commands, as the committed results were made (500 runs per problem, bench seed 1;
`families` takes about 20 minutes, `all` several hours):

```sh
python3 -m tabletop.bench families                                # the 35 chapter 5 result sets, 500 runs each (about 20 minutes)
python3 -m tabletop.bench families --runs 50 --seed 7             # French's own size, another seed
python3 -m tabletop.bench fcca --profile french-faults            # FCCA's 18 reprinted sets under another profile
python3 -m tabletop.bench families --json families.json --quiet   # one JSON record per comparison, no progress lines
python3 -m tabletop.bench humans --runs 50                        # Tables 5.1-5.3 under standard and human
python3 -m tabletop.bench ch2 | ch4 | focus | rho | lesions | faults | potelbat | temperature   # the probe benches (D-073)
python3 -m tabletop.bench robustness --runs 500   # 500 random tables with every invariant checked, and a speed profile (D-068, D-077)
python3 -m tabletop.bench variants --runs 100     # eight variants of every problem, paired with the original; v1's run is results/v1/variants.json (D-102)
python3 -m tabletop.bench churn --runs 500                      # roles over time on the four carriers (D-106)
python3 -m tabletop.bench stated --runs 100     # French's statements as yes-or-no checks, and chapter 2's series (D-074)
python3 -m tabletop.bench all --runs 500 --out bench-results      # every bench: NAME.md and NAME.json (hours)
python3 -m tabletop.bench.results strip bench-results results     # the records without their runs, for the repository (D-075)
python3 -m tabletop.bench.calibrate buridan --runs 500 --workers 7 --json bench-results/calibrate-buridan.json   # the two calibrations (D-076)
python3 -m tabletop.bench.calibrate human --runs 200 --workers 24 --json bench-results/calibrate-human.json
python3 -m tabletop.bench.results update results RESULTS.md       # regenerate RESULTS.md's marked tables from them
```

Each bench prints one table per result set in French's format [F95 p. 115] (his heading,
then "Object Eliza touches", "Average structure", "No. of codelets run", each with our
column beside his, and the two-sample z of every frequency against his fifty runs), a line
with the runs, the batch seed, the capped runs and the chi-square of the problem, and a
summary table of every set; progress goes to stderr. A bench is deterministic for its
`--seed`: each problem's batch seed is derived from it and the problem's name, and the
JSON record of a comparison lists every run's seed, answer, structure value and codelets,
so that any run can be replayed with `python3 -m tabletop PROBLEM --seed SEED`. The
`fcca` bench shows Surround 5 and Surround 10, whose FCCA numbers differ from the book's,
against both versions from the same runs.

The ten probe benches (D-073) measure what French reports outside the result sets, or
what he only describes: `humans` (Tables 5.1-5.3 and Figure 5.4 [F95 pp. 143-144], our
numbers under the profile given and under `human` beside the subjects' and Tabletop's
second runs, with the rank correlations and the within-ten-points count), `ch2` (the
nineteen chapter 2 configurations, French's words read as yes-or-no checks), `ch4` (the
chapter 4 run many times: the temperature and the rack's bars at each dump's codelet count,
and his run's milestones, with the share of our runs reaching each), `focus` (Surround 5
against 10, and Surround 5 with 0 to 12 synthetic junk objects), `rho` (0.5 to 6), `lesions`
(FCCA's criterion 11 [FCCA p. 365]: the temperature clamped high and low, rho 1, losers
destroyed, flat depths, fixed link lengths, the context mechanism off; its expectations are
read as measure M7 by the results builder, `lesions.verdict`), `faults` (each of French's
seven faults restored alone and all together), `potelbat` (the brute-force baseline of
`tabletop/potelbat.py`, D-067, on every problem), `stated` (25 of French's statements of
what the program does, read as yes-or-no checks over the runs with the numbers behind each
verdict, then chapter 2's series; D-074, measure M2), `temperature` (the course of T over a
run, read off every run's events: the curve against the codelets run, Surround 5's beside
the chapter 4 thermometer, the T at the first Worldview change and at the answer, the three
terms of the formula, and two alternative readings as variants) and `robustness` (D-068,
D-077: `--runs` realistic random tables from `tabletop.tables` run once each with
`invariants.check` after every codelet, the answers, run lengths, capped runs and every
violation found, and the speed of plain and checked runs of the probe problems on one
core, a machine-dependent number recorded for the record) and `variants` (D-102: eight
variants of every problem file, mirrored, side-swapped, rotated, translated,
type-substituted, junk-added, decoupled and on-saucer, each run with the original's seeds
and compared with it after relabelling: the total-variation distance with a bootstrap
interval, the chi-square, the share of paired runs that kept their role; the run on the
pre-roles commit is `results/v1/variants.json`, made by `tools/run_on_baseline.py`, which
runs a bench against another commit's engine from a throwaway worktree). `bench all --out DIR` runs
every bench and writes `DIR/NAME.md` and `DIR/NAME.json`, the files RESULTS.md is built
from (`bench-results/` is ignored by git). Every probe table shows, per variant and
problem, the main answer and its share, the share of French's main answer, the literal
share, the mean codelets, the mean structure value and the capped runs; nothing in them
is ever used to set a default.

RESULTS.md is built from those records: `python3 -m tabletop.bench.results strip` copies
every bench's JSON into `results/` without the per-run lists (keeping each survey's
tallies, seeds, median and quartiles of codelets), and `update` regenerates every table
between the file's `<!-- begin NAME -->` and `<!-- end NAME -->` markers from them;
`tests/test_results.py` regenerates the blocks and compares, and checks that
`config_defaults.lock` equals the profiles (D-075). The `lesions` bench was run one problem
per process (`--problems NAME`), since its `temperature-high` variant runs every run to the
cap; `strip` merges `lesions-NAME.json` parts into one file (a part never belongs to
another bench's name, so `families-human-X.json` is a part of `families-human`). The full
records are large (0.5 to 1.5 MB per bench) and are not kept under this folder once
stripped: write them outside it (`--out /tmp/bench-results`), since the loop's gate rejects
any file over 300 KB here, ignored by git or not; and strip from a directory holding only
the new bench's files, since `strip` rewrites every bench's file it finds.

The two calibrated profiles (ANSWERS.md 7; D-076) are fitted by
`python3 -m tabletop.bench.calibrate human | buridan`: a scan over a grid of values fixed
before any run, one survey per value and problem with the benches' seeds, the least
objective chosen. `human` fits `type_weight` by least squares to the subjects' two columns
of Tables 5.1-5.3 on a random half of the 23 problems (the split from a recorded seed) and is
evaluated on the other half by the `humans` bench; `buridan` fits `incoherence_penalty` to
the balance between E1 and E2 on Buridan 1 under the modified program and is evaluated on
Buridan 2-13 by `families --profile buridan`. `--workers N` runs the surveys in a process
pool; the records (`results/calibrate-*.json`, `families-human.json`, `families-buridan.json`)
feed RESULTS.md §9, and every change to a profile goes into the lock and the calibration
log. These scans never touch `standard`: through v2 it was fitted to nothing, v3's defaults
(D-114) come from the cross-validated Bayesian optimization of 2026-10-15/16 and v4's (D-115)
from the second one of 2026-10-17, not from them.

## Running the tests

From this folder:

```sh
python3 -m pytest -q            # the default suite, tests/ (fast; no test over about 2 s)
bash run-tests.sh               # the same, in parallel when pytest-xdist is installed
python3 -m pytest -q tests_slow # the slow suite (bash run-tests.sh --slow); --all runs both
```

The default suite is `tests/`, the fast one: every test takes under about two seconds on
its own, the engine's in about 30 seconds and the GUI's in about 70 seconds on the
development machine, on the compiled engine, one core, load 11 on 32 (2026-10-15;
`python3 -m pytest -q --durations=20` lists the slowest). It runs on the compiled engine
and fails at collection without a build; with `TABLETOP_ENGINE=python` it runs on the pure
engine and the tests that need the compiled one skip, with a reason (D-107).
The slow suite is `tests_slow/` (its own `pytest.ini` and `README.md`), outside the default
suite's paths: the multi-seed surveys, among them 1,000 checked runs of the invariants
(`tests_slow/test_invariants_slow.py`), the `bench all` smoke run at full size (fifteen
benches at one run each; by default `all` is checked on its four quick benches), the
README's command blocks, the equivalence of the two engines on every families problem at
200 runs (D-113), and the GUI's whole runs through the window (sessions, the
configuration edits, the memory and the capped run). Each slow file imports its helpers
from the fast file it was moved from, which keeps a quick check of the same feature.
Tests that read a source PDF skip, with
a reason naming the variable to set, when the PDF is not found (below); the GUI's Qt tests
skip without PySide6 (`python3 -m pytest -q tests/gui` runs them alone). `tests/test_readme.py`
runs the commands of this file and of `../README.md` and checks the outputs they show, the
table of knobs, the module map and the links; `tests/gui/test_gui_readme.py` does the same for
the GUI sections and the screenshots.

**The links, in both places.** `python3 tools/check_links.py` checks every Markdown link
under `Software/Tabletop/`: relative links stay inside the folder and point at files and
headings that exist, anchors name headings, and links to FARGonautica's files on GitHub name
files of the enclosing checkout when one is found (`--fargonautica DIR` names one; `--online`
also fetches every URL and reports its status). `bash tools/standalone_check.sh` runs the
whole standalone procedure of [`docs/standalone.md`](docs/standalone.md) in a scratch clone
under `/tmp`: the subtree split, the tests with and without the source PDFs, a fresh virtual
environment and the install, the installed command, and the links. `tests/test_links.py`
tests the checker and keeps the folder's links good.

## The sources

The tests and the checker read the source PDFs in place, never copying them. They are found
through the environment, or through an enclosing FARGonautica checkout:

| Variable | What it names |
|---|---|
| `FARGONAUTICA` | a FARGonautica checkout; French's book (F95) and the 1991 paper (FH91) are in its `Literature/` |
| `F95_PDF`, `FH91_PDF` | overrides for those two PDFs |
| `FCCA_PDF` | *Fluid Concepts and Creative Analogies* (never in any repository) |

Without `FARGONAUTICA`, the nearest ancestor directory holding both `.git` and `Literature/` is
used, so inside FARGonautica nothing needs setting but `FCCA_PDF`. Without FCCA its quotations
are reported as skipped. This lookup lives in one place, `tools/check_quotes.py`, and
`tests/sources.py` shares it.

## The quotation checker

Every quotation of six or more words in double quotes followed by a citation tag (`[F95 p. 55]`,
`[FCCA p. 387]`, `[FH91 p. 181]`; the printed page) is checked against the text layer of the
cited page, plus or minus one page:

```sh
python3 tools/check_quotes.py ..                  # every .md and .py under Software/Tabletop/
python3 tools/check_quotes.py --page F95 55       # print the text layer of printed page 55
python3 tools/check_quotes.py --find F95 "three main components"   # where do these words occur?
python3 tools/audit_sample.py --seed 31          # the citation audit's sample (docs/citation-audit.md)
```

It needs `pdftotext` (poppler). Its limits (at least 6 words checked, at most 80 per
quotation, a match threshold of 0.90) are literals at the top of the file; the tests and the
loop's gate refuse a weaker checker.

## Conventions

The conventions: standard library only at run time (Cython and a C compiler build the
compiled engine, D-110); all randomness through one seeded generator passed in
(`accel.Rng`, splitmix64 on both engines, never the `random` module); a fresh world per run; lists, never
sets, wherever order matters; small modules with one job each; typed events. Every public
docstring under `tabletop/` cites a source tag or a decision number; every `Config` field is
commented with one; no problem name appears in engine code outside `problems.py`,
`reported.py`, `bench/` and `__main__.py`. `tests/test_conventions.py`,
`tests/test_package.py` and `tests/test_standalone.py` check all of this.
