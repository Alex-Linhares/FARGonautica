# The engine's API for observers and for the GUI (final, items 21 and 29; for loop0006; the roles' events and fields added by the roles loop, D-105)

The engine of the reconstruction (`tabletop/`) is built so that a graphical interface can
watch every step of a run without changing it, as French's own screen did [F95 pp. 95-107
(img)]: the table with its groups and correspondences, the thermometer, the Slipnet grid and
the Coderack bars. This page defines what the engine publishes (the typed events, D-065),
what it lets an observer read between events (the read-only state), and the rules an observer
must keep; then it maps each pane of the planned GUI (loop0006) onto the events and state that
feed it. The GUI is the package `tabletop/gui/` (`python3 -m tabletop.gui`), built by
loop0006 against this page and the frozen engine; its module map is
`tabletop/gui/README.md`, its decisions `GUI-DECISIONS.md` and its user guide
`docs/gui/panes.md`. The roles (D-078 on; `docs/roles.md`) added four events, three
snapshot fields and one pane (P14) to what is below; nothing that existed changed.

## Running a problem

```python
from tabletop.problems import load_problem
from tabletop.config import PROFILES
from tabletop.run import run_problem, fingerprint
from tabletop.events import Collector
from tabletop.observe import TextTrace, JsonLines

log = Collector()                                   # keeps every event
result = run_problem(load_problem("surround-05"), PROFILES["standard"], seed=3,
                     observers=[log, TextTrace(), JsonLines(open("run.jsonl", "w"))])
digest, draws = fingerprint("surround-05", 3)      # the same in any process
```

`run_problem` builds a fresh `World` (the Slipnet, the Workspace, the Worldview, the Coderack,
the temperature, the generator and the clock; nothing is shared between runs), runs codelets
until a find-answer codelet runs or the cap (D-061) is reached, and returns a `Result`: the
answer's id and the problem's label for it, whether it was literal (D-063) and how it was
reached (`through`, `via`), whether the run was capped, the structure value (D-062), the
codelet count, the final temperature, the profile and the fields changed from `standard`,
the `engine` that ran it, a summary of the Worldview, the counts of structures, competitions
and changes, the generator's draw count, and `observer_errors`. `run_batch(problem, config,
batch_seed, count)` runs a problem over the seeds of a batch (`accel.run_seeds`).

**The two engines** (D-107). `tabletop.accel` is the one place that chooses the engine, once
per process, from `TABLETOP_ENGINE`: unset (or `compiled`) is the compiled engine
(`tabletop/_native/`, built by `python3 tools/build_fast.py`), required, with an error at
import naming the build command when it is missing or stale (D-110); `python` is the pure
engine, the specification; anything else is an error. `accel` exports `Rng`, `Slipnet`,
`run_seeds` and `chances_of` (the Coderack's chances, the one typed hot spot after the
Slipnet, D-112), which every construction and every rack draw in the package goes through,
and `info()` (the engine, the generator, and the build's stamp, flags, compiler and Cython
version). There is no
fallback: `World`, `run_problem`, the CLI, the benches and the GUI all fail at start without
a build (the GUI once Qt is up, in a dialog, or on stderr under `--smoke`; it names the
engine that runs in its status bar and in Help > About, GUI-DECISIONS.md G-063). The
generator is splitmix64 on both engines (D-109), so a seed draws the same
stream on both, and the compiled Slipnet performs the Python's operations in the Python's
order (D-111), as does the compiled Coderack chance (D-112), so the two engines' runs are
identical as this loop leaves them. **The compiled engine is the reference:** its goldens
(`tests/data/golden/compiled/`), its bench numbers and its self-determinism (the same seed,
the same run, in two processes under two `PYTHONHASHSEED`s: `tests/test_determinism.py` and
`tests_slow/test_determinism_slow.py`) are the package's; the pure engine is the readable
specification, run on explicit request, and the oracle of the equivalence tests (D-113:
`tools/equivalence.py` runs every families problem 200 times on each engine, in two
processes, and holds every problem's answer distribution within a total variation of 0.25
of the other engine's, a tolerance derived from the sampling null at that size;
`tests_slow/test_equivalence_slow.py` at size, `tests/test_equivalence.py` quick). A GUI that wants to pause,
step and inspect builds `World(problem, config, seed, observers)` and runs the same loop one
codelet at a time through `World.step()`, which runs one codelet with its follow-ups, the
Slipnet update and its top-down postings, the temperature update and the find-answer
schedule, publishing the events below as it goes, and returns a `world.Step` saying what
happened (the codelet, the exponent and temperature at the choice, the codelet's `Outcome`
with its action, detail and the codelets it posted, the codelets removed, the Slipnet's
report, the top-down postings, the temperature update and the find-answer codelets posted,
and the context push and pop that happened before the codelet ran, `Step.pushed` and
`Step.popped`, D-021). Between steps, `World.snapshot()` copies the state (below). The
command line (`python3 -m tabletop`, `python/README.md`) is one such observer-driven run.

## The observer rules

- An observer is anything with `on_event(event)`. Observers are called in subscription order,
  as soon as the engine has done what the event describes, with the same event object
  (`tabletop.events.Publisher`; `World.events`). `tabletop.events.Collector` keeps every
  event in a list.
- Observers cannot change the run: events are frozen dataclasses of plain data (ids, names,
  numbers, strings, booleans, tuples), never engine objects; the read-only state (below)
  returns copies or recomputed values and stores nothing.
- An observer that raises is caught: the error is recorded on the run
  (`result.observer_errors`, (observer, event, error) triples; `World.events.errors`) and
  reported on stderr, the event goes on to the next observer, and the run continues
  exactly as it would have without the observer. The tests check that a raising observer and
  an observer that reads everything (the snapshot and every accessor, on every event) leave
  the fingerprint unchanged.
- Observers draw no random numbers from the run's generator: it is not passed to them. The
  run's fingerprint (`docs/trace-format.md`) is the same with any number of observers attached.
- Every event carries `clock` (codelets run; a codelet's own events carry its ordinal) and
  `draws` (the generator's draw count), so a GUI can align events with the trace and a test
  can locate a divergence to within one codelet. `event.name` is the class name.
- A `Workspace` or `Slipnet` built on its own (in a test) has a publisher with no observers
  and publishes nothing; the `World` gives them its own.

## The events (`tabletop/events.py`)

Frozen dataclasses, all with `clock: int` and `draws: int` first; `tabletop.events.EVENT_TYPES`
lists them in this order and `record_of(event)` gives the JSON record. Ids are object ids
(`cup1`), structure names are `g4`, `p2` and `cp2` (`docs/trace-format.md`), nodes are the
Slipnet's names (`group`, `mirror symmetry`), codelets are their type names (`find-group`)
with the strip's label available through `config.codelet_label`. A field that may have no
value (a group's category or parent, a correspondence's `via`) is the empty string.

| Event | Fields | When |
|---|---|---|
| `RunStarted` | `problem`, `seed`, `profile`, `engine` (`compiled` or `python`, D-107), `changed` ((field, value) pairs for the fields differing from `standard`, the value as `repr`), `touched`, `objects` (ids in file order) | once, first, before the starting rack and the Slipnet's first update |
| `RunEnded` | `answer`, `label`, `literal`, `capped`, `structure_value`, `codelets`, `temperature`, `worldview` (tuples of (name, end, end)) | once, last, right after `AnswerGiven` |
| `CodeletPosted` | `codelet`, `args`, `raw`, `generation`, `urgency` (adjusted), `by` (`start`, `slipnet`, `answer-schedule`, or the parent's type) | every posting (D-020, D-052, D-055, D-056), including the starting rack at clock 0 and a reload |
| `CodeletChosen` | `codelet`, `args`, `generation`, `urgency`, `chance`, `a`, `temperature`, `rack_size` (before it was taken off) | before the codelet runs [F95 p. 77; F95 p. 92]; its `clock` is the codelet's ordinal |
| `CodeletRan` | `codelet`, `args`, `generation`, `action` (the trace's vocabulary), `detail` (the codelet's words), `posted` (the types of its follow-ups) | after every codelet, before its follow-ups are posted (D-052) |
| `CodeletFizzled` | `codelet`, `args`, `reason` | a codelet that did nothing [F95 p. 73], right after its `CodeletRan` |
| `CodeletRemoved` | `codelet`, `args`, `posted_at`, `reason` (`age` or `capacity`) | D-053 |
| `SlipnetUpdated` | `changed` (tuples of (node, activation) for nodes that moved, rounded to a tenth), `lengths` (tuples of (link, effective length) for labeled links that changed) | after every update (D-019); the first, at clock 0, lists every node above 1 after the initial state's update (D-017) |
| `NodeJolted` | `node`, `amount`, `by` (the codelet type, D-016; `focus` for the jolts that keep the touched type and "opposite" active, D-017), `activation` (after the jolt) | every jolt |
| `ContextPushed` | `correspondence`, `reset` (tuples of (node, activation before), the nodes set to 1) | before a codelet that carries a context different from the current one runs (`context.enter`, `World.step`; `Step.pushed`, a `context.Push`) [F95 p. 87] (D-021, D-070) |
| `ContextPopped` | `correspondence` (the one left), `reinitialized` (the nodes set to 1 again), `spread` (tuples of (node, amount) from the one spreading step; empty under the fault), `fixed` (tuples of (node, amount) in the order found, one per concept per slippage), `restored` (tuples of (node, activation after) for every node touched) | before a codelet whose context differs from the current one runs, ahead of its push if any (`Step.popped`, a `context.Pop`) [F95 pp. 88-89] (D-021) |
| `ObjectNoticed` | `object`, `type`, `region`, `by` | the first time a codelet looks at an object [F95 p. 64] (`Workspace.notice`; `by` is `start` for the touched object, else the codelet type); `region` is the drawn region's name (`upper-left`, ..., `middle`, ..., `lower-right`; `geometry.Region.name`, D-004). Later looks update `Workspace.regions` without an event |
| `DescriptionAdded` | `item`, `descriptor` (`end`, `left`, `horizontal`, `two`, `isolated`, `opposite` (D-071), a type, a correspondence's `level:top` and slippages, ...), `by` (the codelet type running) | every new descriptor (`Workspace.describe`; D-028, D-029). The table-frame position of D-029 is drawn each time it is read (`describe.table_frame`) and is state, not an event; the group-frame words are on `Group.positions` |
| `NeighborFound` | `object`, `neighbor`, `side` (`left`, `right`, `above`, `below`: the side of `object` on which `neighbor` lies), `group` | every new relation (`describe.find_neighbours`; recorded both ways, one event per relation; D-027) |
| `GroupProposed` | `group` (the proposal's name, `p1`), `members`, `kind` (`sameness`, `shared-isa`, `mixed`), `category` (a shared-ISA group's category, else empty), `base` (the bottom-up group being extended, or empty), `by` | D-024 (`Workspace.add_proposal`, when the codelet names the proposal it made with `build_object.propose`) |
| `GroupStagePassed` | `group` (the proposal), `stage` (0, 1, 2), `chance`, `passed` | D-024 (`build_object.run_stage`; the `results` of a `Proposal`) |
| `GroupBuilt` | `group`, `members`, `kind` (`gestalt`, `sameness`, `shared-isa`, `mixed`), `category`, `strength`, `parent` (the gestalt group or empty), `orientation` (empty until noticed) | D-023 to D-025 (`groups.announce`, from `build_object.stage_compatibility` and `groups.find_group_around`, before the group's fights) |
| `GroupRemoved` | `group`, `reason` (`absorbed`, `lost`) | D-025, D-026 (`Workspace.remove_group`, from `groups.resolve_conflicts` and `groups.absorb_subgroups`) |
| `CorrespondenceProposed` | `correspondence`, `henry` (the first end), `eliza` (the second), `level` (`top` or the group correspondence's name), `by` | D-035 (`correspondences.create`, once the pair is creatable and named; a refused pair fizzles with a reason: `same item`, `not in view`, `one contains the other`, `in one group`, `exists`) |
| `CorrespondenceBuilt` | `correspondence`, `henry`, `eliza`, `static`, `dynamic`, `bonus` | right after `CorrespondenceProposed`: it enters the Workspace as a tentative structure (`correspondences.create`; the points are recomputed whenever read, `evaluate`) |
| `CorrespondenceTested` | `correspondence`, `static`, `slippages` (tuples of (from, to, label), `Slippage.as_tuple`), `chance`, `passed` | D-039 (`correspondences.describe`, then `test`) |
| `CorrespondenceRemoved` | `correspondence`, `rating`, `chance` (the clean-up's; 0 when removed for another reason), `inner` (the names of the correspondences inside it, removed with it) | D-041 (`worldview.remove`, from `clean_up` and the run loop's `drop_orphans`; a correspondence in the Worldview is never removed by the clean-up) |
| `CompetitionHeld` | `newcomer`, `rivals` (names; empty for an entry without a fight), `old_rating`, `rival_rating`, `chance` (1 without a fight), `rival_wins`, `a` | D-040, D-044 (`worldview.compete`, one `Competition` per call; `Workspace.competitions` keeps them) |
| `WorldviewChanged` | `added`, `removed` (names), `rating` (the `Rating.total` after the change), `coherent` (no incoherent pair), `reason` (`no rival`, `won`, `removed`) | every change (D-040 to D-042; `worldview.Change`, kept on `Workspace.changes`; an old Worldview that holds its ground logs no change) |
| `TemperatureUpdated` | `temperature`, `q`, `dq`, `dwv`, `tcp`, `a`, `held` (T still at its initial value, D-049), `by` (`schedule`, `competition`, `change`, `start`) | D-047 to D-050 (`temperature.TemperatureUpdate`, kept on `Temperature.last`) |
| `AnswerCodeletPosted` | `urgency`, `temperature` (the temperature the law read: T, or T x (1 - pressure) under the time pressure of D-120) | D-056, D-120 (`World.schedule_find_answer`; one event per codelet posted, before its `CodeletPosted`) |
| `AnswerGiven` | `answer`, `label`, `literal`, `through` (`correspondence`, `group`, `default`, or `blocked`: the default while a CLAIMED tag stands on an obvious counterpart of the touched object, consumer C4), `via` (the correspondence read, or empty), `note` (the group case; the claim) | D-057, D-058, D-063, D-101 (`answer.Answer`; `World.finish`) |
| `RoleTagged` | `tag` (`r1`, `r2`, ...), `item` (an object id, a group name, or `worldview` for the axis), `role`, `args` (the partner of a pair role, the item a sidekick stands beside), `weight`, `strength` (at tagging), `evidence`, `by` (the codelet type, or `perceived` under the oracle probe) | a role tag built in the pool (D-079, D-105; `Workspace.tag_role`) |
| `RoleDiscarded` | `tag`, `item`, `role`, `reason` (`lost to rN`, `broken`, `structure gone`), `by` | a tag removed: after a fight with a rival reading, by the role checker, or with its structure (D-079, D-105; `Workspace.discard_role`) |
| `RoleRecast` | `touched`, `signature`, `tiers` ((tier, candidate, weight) tuples), `ambiguous`, `by` | the re-cast of the touched object's signature onto the other row, published by the recast-role codelet (D-093, D-105; `roles.recast`): `tiers` holds (tier, candidate, weight) and `ambiguous` says whether the first non-empty tier is a tie |
| `RoleConsumed` | `role`, `consumer` (`C1` to `C8`), `item`, `effect` (`create-cp cup2 small-glass1`), `by` | a consumer read a tag (D-095 and the later consumers' decisions; D-105; `roles.consumed`) |

Changes from the draft of item 10: `CodeletRan` was added (the text trace needs what a
codelet did, not only that it fizzled); `CorrespondenceRemoved.with` became `inner` (`with`
is a Python keyword); `RunStarted.changed` is a tuple of pairs, not a dictionary (plain
data). No field is removed or renamed from here on: loop0006 builds against this table.
The four role events were appended by the roles loop (D-105); nothing before them changed.

## The read-only state

Between events a GUI may read, through `World.snapshot()` (`tabletop.snapshot.snapshot`,
a frozen `Snapshot` of tuples, dictionaries, numbers and strings; `snapshot.answer_now` gives
the answer as the Worldview stands), or through the live attributes named below, which draw
nothing when read, a copy of:

- **the table**: the problem's objects with id, type, x, y, orientation and footprint box
  (D-001 to D-003); the touched id; each object's last drawn region (D-004);
- **the Slipnet**: for every node its name, the screen's label, its level and depth, its
  activation (D-015) and, for labeled links, the intrinsic and effective lengths (D-011); the
  current distance matrix (D-012) on request;
- **the Workspace**: the noticed objects with their last drawn regions and descriptors, the
  competitions each item's correspondences have won and lost (D-031, factor 7), the
  isolated ones; every group (name, members, kind, category, strength, parent, orientation,
  linear, ends, middles, neighbours with sides, the members visited, the group-frame positions) and every correspondence (name, ends, level, static, dynamic, bonus,
  rating, slippages, wins, losses, whether tentative, in the Worldview, or a loser), in the
  order built; `correspondences.static_terms(world, cp)` and `dynamic_terms(world, cp)` give
  the ten static factors (French's nine and the role factor of consumer C5, D-098) and the
  three dynamic ones as (name, value) pairs for an overlay, and draw nothing;
- **the Worldview**: `Workspace.worldview()` (its correspondences in the order built) and
  `worldview.summary(world)` (as (name, end, end) tuples); `worldview.current(world)`, a
  `Rating` with `strength`, `coherence`, `parallelism`, `total`, the `coherent_pairs`,
  `incoherent_pairs` and `parallel_pairs` as (name, name) tuples (D-042, D-043), drawing
  nothing; `worldview.rivals_of(world, cp)` for any correspondence; `Workspace.competitions`
  (every `Competition`: newcomer, rivals, the two ratings, chance, winner, exponent, clock)
  and `Workspace.changes` (every `Change`); the Hobj correspondence
  (`correspondences.hobj_correspondence`); the current context, `World.context` (a
  `context.Context` with the correspondence's name, the nodes the push reset and the clock
  then; None at the top level, D-021), and `context.correspondences_at_top(world)`, the
  correspondences a pop would read;
- **the roles** (D-079): `Workspace.roles`, the pool, every `roles.RoleTag` standing in the
  order tagged (`item`, `role`, `args`, `weight`, `evidence`, `by`, `built_at`, `name`,
  `strength`, `consumed`); `Workspace.roles_of(item)`, `items_with(role)`,
  `role_tag(item, role, args)`, `has_role(item, role)`; `roles.REGISTRY` (the 23
  `RoleSpec`s: family, reading, nodes, detector, consumers), `roles.pending()`,
  `roles.rate(world, tag)` (recomputes a tag's strength and writes it on the tag: the one
  accessor here that writes, and it writes what a checker would) and
  `roles.break_chance(world, tag)`;
- **the run**: `World.clock`, `World.finished`, `World.answer` (an `answer.Answer` once the
  run has ended), `World.capped`; `answer.answer_for(world)` gives the answer as the Worldview
  stands at any moment, drawing nothing; `Workspace.proposals`, the bottom-up groups proposed
  (`build_object.Proposal`, named `p1`, `p2`, ..., with their stages and results, D-024);
- **the Coderack**: every codelet with type, label, arguments, raw and adjusted urgency,
  generation, age and the chance it had at the last draw (D-060), plus the strip's shares per
  type in the strip's order [F95 p. 97]: `Coderack.codelets` (in posting order, the oldest
  first; each a `Codelet` with `type`, `label`, `args`, `raw`, `generation`, `posted_at`,
  `age(clock)`, `by`, `urgency`, `chance`), `Coderack.chances(a)` (every codelet's chance
  at the exponent a, without a draw), `counts()` and `shares()` (the bars, as (label,
  percent) in the strip's order), `total_urgency()`; the catalogue
  (`tabletop.codelets.CATALOGUE`) gives each type's label, role, job and whether the job is
  from the text or inferred (D-051), for a legend;
- **the temperature**: `World.temperature`, a `Temperature` with `value` (T), `q`, `dq`, the
  `rates` list, `dwv`, `tcp`, `exponent` (a(T)) and `last` (the last update) (D-047 to
  D-050); `World.quality()` recomputes Q;
- **the clock and the draw count**; the salience of every item (D-031) on request:
  `salience.saliences(world)` gives every item in view with its salience and
  `salience.terms(world, item)` the nine contributions, French's eight in his order (type,
  proximity, position, region, group, worldview, competitions, composition) and the role
  term (consumer C7, D-100), for the P1 overlay; `salience.region_order(world)` the nine regions from the most to the least
  preferred (D-032). Reading any of these draws nothing, so a GUI may read them between
  any two events without changing the run.

Snapshots are plain dataclasses and dictionaries; mutating a snapshot changes nothing in the
run, and taking one draws nothing and stores nothing (a correspondence's points are
recomputed for the copy, not written back). `Snapshot` has: `clock`, `draws`, `finished`,
`capped`, `problem`, `seed`, `profile`, `touched`, `answer`; `table` (one dictionary per
object: `id`, `type`, `x`, `y`, `orientation`, `box`, `region`, `noticed`); `slipnet`
(`name`, `label`, `level`, `depth`, `activation` per node) and `links` (the labeled links:
`a`, `b`, `labels`, `length`, `effective`); `noticed`, `isolated`, `descriptions`,
`groups` (`name`, `members`, `kind`, `category`, `strength`, `parent`, `orientation`,
`linear`, `ends`, `middles`, `neighbours`, `visited`, `positions`), `proposals`,
`correspondences` (`name`, `henry`, `eliza`, `level`, `static`, `dynamic`, `bonus`,
`rating`, `slippages`, `wins`, `losses`, `tentative`, `tested`, `passed`, `in_worldview`,
`loser`, `descriptors`), `saliences`; `worldview` (`names`, `strength`, `coherence`,
`parallelism`, `total`, `coherent`, the three pair lists); `competitions`, `changes`,
`context`; `coderack` (`codelets`, `shares`, `total_urgency`); `temperature` (`value`,
`q`, `dq`, `dwv`, `tcp`, `a`, `rates`); and the roles (D-105): `roles` (one dictionary per
tag standing, in the order tagged: `name`, `item`, `role`, `args`, `weight`, `strength`,
`evidence`, `by`, `built_at`, `consumed` as (consumer, effect) pairs), `recast` (the
last re-cast published, D-093: `touched`, `signature`, `tiers`, `ambiguous`, `clock`; None
until the recast-role codelet publishes one) and `axis` (the symmetry axis's tag,
on the item `worldview`; None until one is tagged).

A GUI's own consistency check is `tabletop.invariants.check(world)` (D-077): every rule
the state must keep (the Slipnet's range, the Coderack's capacity and ages, the
temperature's range, structures standing on things that exist, the Worldview's rule of
[F95 p. 71], the end of the run) as sentences, empty when all hold; it reads the state and
draws nothing, so it may be called between any two events.

## The panes of loop0006, and what feeds them

The pane list is loop0006's plan (kept with the loop's own files, outside this repository);
the sources of each drawing are French's screen dumps and chapter text where the row says so, otherwise new
design.

| Pane | Events | State | Source of the drawing |
|---|---|---|---|
| P1 Table | `GroupBuilt`, `GroupRemoved`, `CorrespondenceBuilt`, `CorrespondenceTested`, `CompetitionHeld`, `WorldviewChanged`, `CorrespondenceRemoved`, `AnswerGiven` | the table, the Workspace, the Worldview | the dumps: gestalt groups as wavy boxes, subgroups dashed, tentative and competing correspondences dashed and flashing, Worldview ones solid, the H and E arrows (SPEC.md 6c.14) [F95 pp. 98-107 (img)] |
| P1 overlays (regions, salience, region preference, Eliza's view) | `ObjectNoticed`, `DescriptionAdded` | regions (D-004), salience (D-031), region preference (D-032) | new design |
| P1 roles overlay (badges of the standing role codes, the midline band and the axis, the images, the re-cast's tiers, a barrier's claim) | `RoleTagged`, `RoleDiscarded`, `RoleRecast`, `RoleConsumed` | `Snapshot.roles`, `recast`, `axis` (D-105) | new design, a re-interpretation (D-078; G-061) |
| P2 Thermometer | `TemperatureUpdated` | the temperature | Fig. 4.1 [F95 pp. 95-96] |
| P2b Temperature plot (T, Q, ΔQ, Δ_WV, T_cp over codelets, competitions marked) | `TemperatureUpdated`, `CompetitionHeld` | the temperature | new design |
| P3 Slipnet grid (7 × 6 boxes, number and black square) | `SlipnetUpdated`, `NodeJolted` | the Slipnet | [F95 p. 95] and the dumps; the six missing object nodes in a seventh row (new) |
| P3b Slipnet network (links at their current lengths, labels, push and pop) | `SlipnetUpdated`, `ContextPushed`, `ContextPopped` | the Slipnet's links and distances | new design, in Fig. 3.6's style [F95 p. 61 (img)] |
| P4 Coderack bars (shares per type in the strip's order, the count run) | `CodeletPosted`, `CodeletChosen`, `CodeletRemoved` | the Coderack's shares | [F95 p. 97] and the dumps |
| P4b Rack list (urgency, generation, age, last chance) | the same | the Coderack | new design, after the Numbo GUI's coderack view |
| P5 Worldview (each correspondence's static, dynamic, bonus, wins, losses; rivals and incoherent pairs marked) | `CorrespondenceBuilt`, `WorldviewChanged`, `CompetitionHeld` | the Workspace and the Worldview | new design, from the text of pp. 67-74 |
| P6 Competitions (old and rival Worldviews side by side, ratings, chance, winner) | `CompetitionHeld`, `WorldviewChanged` | the Worldview | new design; Fig. 3.11's solid against dashed [F95 p. 72] |
| P7 Palette and editor (drag objects; Load config, Clear table, Save config, Do this!, Run it again) | `RunStarted` | the table; `problems.validate` for an edited table | the palette and buttons of the dumps [F95 p. 96 (img)]; behaviour new |
| P8 Results (French's bar graph and table beside ours for a batch) | `RunEnded` per run | `reported.py`, the benches | the chapter 5 layout [F95 pp. 115-139] |
| P9 Controls, P10 Event log (one line per `CodeletRan`, the text trace's line), P11 Timeline, P12 Stats, P13 Configuration | every event | `Config`, `PROFILES`, `Config.differences` | the Numbo and Seek-Whence GUIs |
| P14 Roles (the tags by family with evidence, weight, strength, readers and, once lost, when and why; the axis; the re-cast's tiers) | `RoleTagged`, `RoleDiscarded`, `RoleRecast`, `RoleConsumed` | `Snapshot.roles`, `recast`, `axis`; `roles.REGISTRY` for the families and codes (D-079, D-105) | new design, a re-interpretation (D-078; G-060); the trace's `RL` lines (P10) and the timeline's role marks (P11) read the same events (G-062) |

## For loop0006

The notes for the loop that builds the graphical interface (the loop's ANSWERS.md 5 and 9),
written when the engine was finished (item 29). They go with the pane table above.

**The engine is frozen.** `config_defaults.lock` (D-075) holds every profile's `Config`; the
interface never changes a default, and the determinism check of the gate (`fingerprint` on
two problems and two seeds, in two processes) is re-run with every view attached. Nothing
in this file's event table changes from here on; a field the interface needs that is not
here is a new event or a new snapshot field, added with a test, never a change to an
existing one.

*Since the roles (D-078, 2026-10-14):* the engine is frozen no longer, but the rule above
holds in another form. The roles loop changed runs on purpose, and each change regenerated
the lock, the golden fingerprints and traces and the GUI's references together with a dated
entry in `RESULTS.md`'s re-interpretation log (the item, the decisions, the files, the lock
fields, the old and new fingerprints, the carriers before and after); the four role events
were appended to the table and the three role fields to the snapshot, and no existing event
or field was changed or removed. The GUI still only observes: the determinism check runs
through the Qt-free controller and the offscreen window on every change (`tabletop.gui.headless.run`).

**The conventions the plan names**, as the Numbo and Seek-Whence interfaces follow them:
the package `tabletop/gui/` inside this one, run as `python3 -m tabletop.gui` with the
script `tabletop-gui` and the extra `gui = ["PySide6>=6.6"]`; Qt-free view models in
`gui/models/`, each tested without Qt; a run controller on a worker thread with a per-event
handshake, built on `World.step()` and the observer rules above; QDockWidgets with
QSettings, Reset layout and a default layout file; Edit > Configuration editing a `Config`
for the next run (the comments of `config.py`, which `tools/knobs_table.py` parses, are the
tooltips); session files as the JSON-lines trace of `docs/trace-format.md`, replayed by
running the `RunStarted` record's problem, seed and profile again and comparing fingerprints;
`--smoke`, which runs offscreen and prints French's report line, the same line as
`python3 -m tabletop PROBLEM --seed N --quiet`; screenshots made offscreen under 200 KB
each; pytest-qt tests run offscreen (`QT_QPA_PLATFORM=offscreen`), never on the screen.

**Each pane against the API** (the plan's thirteen panes, P1-P13, in the table above, and
the roles' fourteenth, P14):

- P1 Table: the table from the snapshot (`id`, `type`, `x`, `y`, `orientation`, `box`,
  `region`, `noticed`), groups and correspondences with their kinds and status; the
  drawing conventions of French's screen are SPEC.md 6c.14 (gestalt groups as wavy boxes,
  subgroups dashed, tentative and competing correspondences dashed and flashing, Worldview
  ones solid, the H and E arrows). **Eliza's view** (the table rotated by 180 degrees) is the
  interface's own transform of x and y; the engine has no notion of a side beyond the
  regions (D-005). The overlays read `salience.terms`, `salience.region_order` and the
  regions of D-004, all without a draw.
- P2 and P2b: `TemperatureUpdated` carries T and the formula's four inputs; `held` marks the
  updates before the first Worldview change (D-049). Expect the course RESULTS.md §6.4
  describes: 80 held for thirty to forty codelets, one jump at the first change, then a decay.
- P3 and P3b: `SlipnetUpdated` lists the nodes that moved and the labeled links whose
  effective length changed; the 41-cell grid of the screen is `slipnet_def.SCREEN_GRID`,
  and the six nodes the screen lacks are the object types absent from the chapter 4
  configuration (SPEC.md 3.1), to be drawn in a seventh row. The push and pop events are
  rare in whole runs (a push in well under one percent of runs, RESULTS.md §6.2): P3b's
  push-and-pop display will mostly show nothing, which is a fact about the engine, not a bug.
- P4 and P4b: `Coderack.shares()` gives the bars in the strip's order [F95 p. 97] and
  `CodeletChosen.chance` the last draw's chance; `codelets.CATALOGUE` the legend.
- P5 and P6: the Worldview is one correspondence in 98 percent of runs under `standard`
  (RESULTS.md §9.2), so these panes will usually hold one entry and no fight; the chapter 4
  state with the spoon-group correspondence beside cup-cup can be built by hand for a
  demonstration (`tests/test_worldview.py`, `chapter_4_table` and `fig_4_7`).
- P7 Palette and editor: an edited table becomes a `Problem` through
  `problems.problem_from_dict` and `problems.validate`; `tables.realism_faults` says whether
  it is a table French would set [F95 p. 38]. Save config writes a problem file
  (`docs/problems.md` 10 gives the keys; provenance `synthetic`).
- P8 Results: `reported.py` holds every number French prints, and `bench/harness.py`'s
  `survey`, `compare` and `markdown_table` make the comparison for a batch of runs; the
  batch runs on a worker, one `RunEnded` per run.
- P9-P13: every event; `Config`, `PROFILES`, `Config.differences`; `invariants.check` as the
  interface's own consistency check between events.
- P14 Roles, and the roles overlay of P1 (the roles loop's item 14; G-060 to G-062):
  `Snapshot.roles` (every tag standing, in the order tagged, with its readers as
  (consumer, effect) pairs), `Snapshot.recast` and `Snapshot.axis`; the four role events
  rebuild the pane's history when seeking (a tag's `RoleTagged`, its `RoleConsumed`
  readings, its `RoleDiscarded` with the reason); `roles.REGISTRY` gives each role's family
  and code for the badges and headings. The tags are the run's current interpretation, not
  labels: a tag can be lost to a rival reading, broken by the role checker, or go with its
  structure (D-079), so a pane shows past tags apart from standing ones.

**The icons (ANSWERS.md 9).** The author wants new vector icons for the eleven object types,
in the spirit of the palette of Fig. 2.1 [F95 p. 26 (img)]: an outline drawing of each type
seen from above, the silverware in its four orientations (`objects.ORIENTATIONS`), the
footprints of D-002 giving the proportions (`objects.FOOTPRINTS_PX`: a fork 6 × 17 figure
pixels, a cup 14 × 7, a plate 19 × 19, ...). They are **new design, not French's**: the
interface should say so where it shows them, as the Seek-Whence interface marks its own
drawings, and no icon is copied from the book's pages (the figures are copyrighted and no
image of French's lives in this repository). The screen's palette also shows the count of
each type on the table above its cell [F95 p. 96 (img)]. A first sketch of such icons exists:
`icon()` in `scripts/docs_figures.py` draws the eleven types in their footprint boxes, with
QPainter, for the front page's pictures (`docs/table-surround-05.png`,
`docs/tables-gallery.png`); the interface may start from it or replace it.

**Three facts of the engine's state a pane should know** (the final review, item 32):

- `Snapshot.proposals` holds every proposal of the run, at whatever stage it reached; a
  proposal whose build-obj codelet was aged out or pushed off stays as it was (D-024).
  A proposal pane should show the stage, not treat the list as a queue.
- `Snapshot.descriptions` keeps the frame words a group gave its members (`end`, `left`,
  ...) after the group is removed; the engine reads such words from live groups only
  (D-028), so a table pane that draws them should take them from `groups`, not from the
  objects' lists.
- `Result.temperature` and the last `TemperatureUpdated` event are up to four codelets
  older than the answer (D-050): a thermometer should show the last update, not expect
  one at `AnswerGiven`.

**Open questions the interface will meet** (the engine's, recorded in RESULTS.md §8 and the
loop's "For the author"; none is for loop0006 to settle):

1. The temperature rises at the first Worldview change and never falls to French's 2.0
   (D-047, D-048; RESULTS.md §6.4): a thermometer drawn from the events will look unlike
   the chapter 4 dumps after the first change.
2. Bottom-up groups and group correspondences are almost never built before a run ends
   (D-052, D-024, D-056; RESULTS.md §8.2): the table pane will rarely show the dashed
   subgroups and the group-to-group line of Figs. 4.5-4.9.
3. The standalone repository's URL is `https://github.com/fargonauts/Tabletop`, confirmed by
   the author on 2026-10-13.
4. Whether `--smoke` should also check the fingerprint against `tests/data/golden/
   fingerprints.json`, so that an interface build proves it changes no run.
