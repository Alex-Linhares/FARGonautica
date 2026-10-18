# Engines and speed

The "Engines and speed" section of [`RESULTS.md`](RESULTS.md), written by the fast-engine loop
(D-107 to D-113, 2026-10-15) and extended through v4, moved here verbatim on 2026-10-17 (v5,
D-116 to D-120) so that `RESULTS.md` stays within the size the repository allows; nothing here
was edited. Its numbers are the fast-engine loop's and v4's; the two logs it refers to are
RESULTS.md's Calibration log and Re-interpretation log.

Every number in the two logs below, and in §1-§11 until the fast-engine loop's item 07
re-ran the benches (the last paragraph of this section), was made by the pure-Python engine
(the "python engine"), the only one until that loop (D-107). That loop adds a
compiled engine (Cython, `tabletop/_native/`), the reference from then on: its runs,
goldens and bench numbers are the package's, and every record names its engine. The pure engine stays as the readable specification and the oracle of the
equivalence test, run only on the explicit request `TABLETOP_ENGINE=python`; there is no
fallback. Bit-identity is not required: each engine is deterministic with its own goldens,
and the pure engine's answer distributions must stay within a stated total-variation
tolerance of the compiled one's.

**How speed is measured.** The workload is `tools/workload.py`: the families bench's 34
problems x N runs of `standard`, bench seed 1001, the seeds derived as the benches derive
them (D-072); it prints the CPU time (`process_time`), the wall time and a digest of the
rows, so every timing is also an identity check. A comparison is paired
(`tools/paired_timing.py`): candidate and baseline at the same time on two cores
(`taskset`), swapped between repeats, fresh processes, `nice -n 10`, 3 repeats or more,
the median and range of the CPU-time ratio. Every row names the engine, workload,
interpreter, load (the 1-minute average on 32 cores) and date; a number measured alone
says so.

| engine | interpreter | what ran | CPU s | against the pre-loop engine | load | date |
|---|---|---|---:|---|---:|---|
| python engine (pre-loop) | CPython 3.12.13 | 34 x 20, alone on one core | 27.6 | the baseline | 13.6 | 2026-10-15 |
| python engine (pre-loop) | PyPy 8.0.1 (3.12) | 34 x 20, alone on one core | 16.4 | — | 13.2 | 2026-10-15 |
| python engine (pre-loop) | PyPy 8.0.1 (3.12) | 34 x 20, paired, 3 repeats | 16.0-16.3 | x1.69 CPU [1.64-1.69], x1.69 wall | 13.0-13.1 | 2026-10-15 |
| python engine after item 01 (D-108, the plain-Python speedups) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 11.0-11.2 | x2.43 CPU [2.27-2.46], x2.43 wall; rows identical | 12.8-14.3 | 2026-10-15 |
| **compiled engine**, item 02 (D-109; the Slipnet still pure) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 10.4-10.6 | x2.58 CPU [2.57-2.60], x2.58 wall; rows identical | 12.4-12.7 | 2026-10-15 |
| python engine, item 02 (explicit request) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 11.1-11.4 | x2.32 CPU [2.26-2.43], x2.32 wall; rows identical | 12.2-12.3 | 2026-10-15 |
| **compiled engine**, item 03 (D-111; the native Slipnet) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 5.1-5.2 | **x5.21 CPU [4.99-5.28]**, x5.21 wall; rows identical | 11.7-12.3 | 2026-10-15 |
| python engine, item 03 (explicit request; the pure Slipnet) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 10.9-11.0 | x2.32 CPU [2.31-2.35], x2.32 wall; rows identical | 11.5 | 2026-10-15 |
| **compiled engine**, item 04 (D-112; the typed chances and the plain fixes) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 4.0-4.1 | **x6.28 CPU [6.21-6.67]**, x6.28 wall; rows identical | 10.4-10.8 | 2026-10-15 |
| compiled engine, item 03 again, in item 04's session (the same copies, for the comparison) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 5.0-5.2 | x5.28 CPU [4.90-5.36], x5.28 wall; rows identical | 10.9-11.1 | 2026-10-15 |
| python engine, item 04 (explicit request; the plain fixes) | CPython 3.12.13 | 34 x 20, paired, 3 repeats | 10.0 | x2.69 CPU [2.53-2.71], x2.69 wall; rows identical | 11.1-11.3 | 2026-10-15 |

PyPy's rows digest differs from CPython's (its float `sum` is not compensated as CPython
3.12's is): a speed row, not an identity row. The pre-loop
profile (34 x 5 under cProfile, the same day, load 14): `Slipnet.update` 75 percent of the
run, `_spread_into` 56, the codelets 15, `max`, `min` and the distance dict's `get` 8 to 9
each, `_recompute_distances` 8, salience 7, `round` (the update's change report) 6; the
same shares as the profile that motivated the loop. The identity reference of the
plain-Python speedups is the rows digest of three 34 x 10 workloads on the pre-loop
engine: `standard` `6eaa1753a2d0...`, `slipnet_update_every=5` `2484a03c174c...`, and
`rho=1.5`, `spread_radius=60`, `slipnet_update_every=2`, `decay_rate=0.2`
`36d1412d78f1...`.

**The plain-Python speedups** (D-108, with D-012, D-013, D-019 and D-031 as revised): the
distance matrix and the neighbour lists, the update report worked out only when read, the
salience weights cached, the problem's ids listed once, and loops for two generator
expressions. Every golden file and all three digests above are reproduced exactly. The
same 34 x 5 profile afterwards: 6.0 s under the profiler against 18.6, `_spread_into` 12
percent of it (self), `_recompute_distances` 9, the appends of spreading 5, `round` 1.

**The native Slipnet** (D-111, `tabletop/_native/slipnet.pyx`): the same operations in the
same order as `slipnet.py`, so the runs of both engines are unchanged (the digests above and
every golden reproduced). One `standard` update in a mid-run state (eight jolts, then 50
updates; the min of 40 samples, one pinned core, `nice` 10, load 12.9, 2026-10-15): 364 µs
on the pre-loop engine, 90 µs on the pure engine after the plain-Python speedups, 1.6 µs
native. The 34 x 5 profile on the compiled engine (3.2 s under cProfile against 6.0 after
item 01 and 18.6 pre-loop, load 13): the Slipnet is gone from the top twenty functions;
what remains is the codelets' object work (`World.step` 3.1 s cumulative; `remove_aged`,
salience's `is_group` and `terms`, geometry's `_third`, `has_role`, `roles.detect`,
`round` in the temperature and salience terms), the matter of the typed hot spots.

**The typed hot spots** (D-112, with D-002, D-003, D-031, D-060, D-079 and D-107 as
revised): a sampling profile of the compiled engine (34 x 10, one pinned core, a 0.5 ms
timer, 2,675 samples over 2.68 s of CPU, load 10.5, 2026-10-15; cProfile's shares agree but
inflate the small functions) put no function above 8 percent: `objects.footprint` 7.6
(self), `World.step` 4.9, the frozen records' constructors 4.5, `has_role` 3.8,
`salience.weight` 3.6, `roles_of` 3.2, `roles.detect` 3.1, `Problem.object` 3.1, the
Coderack's `remove_aged`, `choose`, `chances` and `softened_weights` 8 together, the
geometry's thirds and regions 5 together; inclusive, salience's `terms` 21 percent,
`roles.perceive` 19, `static_points` 14. The verdicts are D-112's: one typed mirror, the
Coderack's chances (`_native/coderack.pyx`, the total by Python's own compensated
summation, checked exact against `temperature.chances_of` on 3,000 random racks), six
plain-Python fixes (the footprint remembered, the role pool indexed by item, the weights
and the region preferences as dicts built once, the detectors read once, the objects
indexed by id, the ageing in one pass), and nothing for the run loop, the records, the
thirds and salience's terms. Every run unchanged on both engines: the three identity
digests reproduced on the compiled and on the pure engine, every golden file untouched,
the rows identical in all nine paired repeats. The same sampled workload afterwards: 2.11 s
of CPU (load 11.0), the constructors 6.0 percent, `step` 5.7, `choose` 2.8, `_third` 2.8,
`chances` 2.0 (the list of urgencies and the list returned; the arithmetic is in C),
`Problem.object` 1.9, `has_role` 1.4, `footprint` gone; the profile is flat and the
remaining cost is Python calls and attribute reads. Paired (34 x 20, 3 repeats, cores 20
and 21 swapped, `nice` 10, fresh processes, load 10.4-11.3, 04:00): the compiled engine
**x6.28 CPU [6.21-6.67]** against the pre-loop engine, item 03's build x5.28 [4.90-5.36] in
the same session, the pure engine x2.69 [2.53-2.71]. The default suite on the compiled
engine: see the loop's PROGRESS.md (about 90 s quiet).

**The final speedup** (the fast-engine loop's item 08; the engine as items 01 to 04 left
it, nothing changed since). The workload is `standard` over the 34 families problems x 30
runs (73,575 codelets), paired against the pre-loop engine (a `git archive` copy of the
loop's BASELINE commit, run by CPython 3.12.13) on cores 20 and 21, swapped between the 3
repeats; the same at `slipnet_update_every=5`, the other end of the optimisation space
(70,236 codelets), on cores 22 and 23 at the same time, so four cores in all; fresh
processes, `nice -n 10`, load 10.7 to 12.9 on 32 cores, 2026-10-15 06:49-07:01. The
compiled engine's build: stamp `2879d92df4ae`, gcc 15.2.0, Cython 3.3.0, the pinned flags
`-O2 -ffp-contract=off` (no `-march=native`, no `-ffast-math`, D-110). The ratio is the
pre-loop engine's CPU time over the candidate's, median and range of the repeats; the wall
ratio equalled the CPU ratio in every row (each process alone on its core). "Rows" says
whether the candidate's rows digest equalled the pre-loop engine's in every repeat. The
per-item rows run the engine of that item's commit (`ee393de`, `918345d`, `c08db54`), the
compiled ones built there with the same flags; items 05 to 07 changed no run and no
engine code. Records: the loop folder's `item08_timing_*.json`.

| engine | interpreter | `standard`: CPU s | against the pre-loop engine | `slipnet_update_every=5`: CPU s | against the pre-loop engine | rows |
|---|---|---:|---|---:|---|---|
| python engine, pre-loop (BASELINE) | CPython 3.12.13 | 38.4-41.9 | the baseline | 14.6-15.5 | the baseline | the reference |
| python engine after item 01 (D-108, the plain-Python speedups) | CPython 3.12.13 | 16.8-17.4 | x2.31 [2.22-2.40] | 9.7-10.1 | x1.57 [1.47-1.57] | identical |
| compiled engine, item 02 (D-109, D-110; the native generator, the Slipnet pure) | CPython 3.12.13 | 15.8-15.9 | x2.64 [2.46-2.64] | 9.4-9.5 | x1.63 [1.56-1.64] | identical |
| compiled engine, item 03 (D-111; the native Slipnet) | CPython 3.12.13 | 7.8-8.0 | x5.20 [4.85-5.26] | 7.7-7.8 | x1.95 [1.91-1.99] | identical |
| **compiled engine, item 04 and final** (D-112; the typed chances, the plain fixes) | CPython 3.12.13 | 6.1 | **x6.69 CPU [6.32-6.76]** | 5.8-5.9 | **x2.63 CPU [2.53-2.63]** | identical |
| python engine, final (explicit request) | CPython 3.12.13 | 15.0-15.2 | x2.68 [2.56-2.72] | 8.2-8.3 | x1.84 [1.76-1.84] | identical |
| python engine, final (explicit request) | PyPy 8.0.1 (3.12.15) | 9.5-9.8 | x4.13 [3.96-4.33] | 8.4-9.0 | x1.72 [1.64-1.74] | differ (PyPy's `sum`) |

**The target, x4 or more over the pre-loop engine on `standard`, is reached: x6.69.** The
gains by item: the plain-Python speedups x2.31, the native generator a further 14 percent,
the native Slipnet x2.0 over item 02, the typed chances and the plain fixes a further 29
percent. At `slipnet_update_every=5` the gain is x2.63, and that is not a shortfall of the
engine but of the workload: with the Slipnet updated once in five codelets the pre-loop
engine was already 2.6 times faster there than at `standard` (14.6-15.5 s against
38.4-41.9), so what the native Slipnet removes is small, and the x2.63 is the object work's
gain alone (item 01's x1.57 and item 04's). The compiled engine's own time hardly depends
on the update schedule any more (6.1 s at `standard`, 5.8-5.9 s at every fifth codelet):
the Slipnet is no longer where the time goes. PyPy runs this tree's pure engine x1.54
faster than CPython does at `standard` (x4.13 against x2.68) and slower than CPython at
every fifth codelet (x1.72 against x1.84: the JIT's gain was in the Slipnet's loops, which
that schedule runs a fifth as often); the compiled engine under CPython is x1.62 faster
than PyPy at `standard` and x1.53 at every fifth codelet, and its rows are the pure
engine's where PyPy's never were.

*The final profile* (the compiled engine, `standard`, 34 x 10, one pinned core, `nice` 10,
load 11.2 to 11.5, 2026-10-15 07:01; the loop folder's `item08_profile_sampled.txt`).
Sampled (a 0.5 ms timer, 2,066 samples over 2.08 s of CPU): no function above 7 percent of
the self time. The frozen records' constructors 6.4 percent, `World.step` 5.3, geometry's
`_third` 3.5, the Coderack's `choose` 3.1 and `chances` 2.9 (the lists around the C
arithmetic), `events.record` 2.5, `remove_aged` 2.1, `World.__init__` 1.9, `_drawn_third`
1.9, `groups_containing` 1.7, the records' `__eq__` 1.7, `has_role` 1.6, `Workspace.__init__`
1.5, `roles.detect` 1.5, `Problem.object` 1.5, `salience.weight` 1.4. Inclusive: the
codelets (`run_codelet`) 59 percent, salience 22 (`terms` 20), `find_group` 15,
`roles.perceive` 15, `static_points` 13, the correspondence's `evaluate` 13, `create_cp` 9,
`find_roles` 8. cProfile on the same workload (6.0 s under the profiler, 23.7 million
calls): `World.step` 0.30 s self of 5.83 cumulative, `remove_aged` 0.21, `is_group` 0.14,
`_third` 0.13, `sum` 0.13, `_tags_on` 0.12, `list.append` 0.12, `max` 0.11, `role_reading`
0.11; nothing of the Slipnet or the generator in the top forty. The profile is flat: what
is left is the price of Python calls, attribute reads and record construction spread over
the codelets, salience and the roles, with no single function worth a typed mirror (D-112's
verdicts stand). *What would go further*, measured for the optimisation study's hand-off
and not adopted here: compiling every remaining `.py` module of a throwaway copy with plain
Cython on top of `_native` (the BO's own `build_cython`, `annotation_typing` off, 57 modules
in 39 s on 4 cores) gives a further **x1.32 CPU [1.30-1.33]** over the compiled engine at
`standard` and x1.33 [1.31-1.34] at every fifth codelet (34 x 30, paired on the same cores,
load 11.8-12.4, 07:04), rows identical; the per-call price falls, the shape of the profile
does not. It stays out of the package: a `.so` beside every `.py` would take precedence
silently and is outside the build stamp's guard (D-110), so it belongs on a snapshot that
is built once and thrown away, never in the repository.

**The equivalence of the pure engine** (D-113, `tools/equivalence.py`): every families
problem 200 times on each engine, each engine in its own process (the pure one on the
explicit request), on the families bench's own seeds (bench seed 1001), the answer
distributions compared by total variation (TV); and the null that set the tolerance before
the comparison was run: the compiled engine's next 200 seeds as a second, independent
sample (the "observed" column), and the 99th and 99.9th percentiles of the TV between two
simulated samples of 200 from each problem's pooled 400 runs (2,000 pairs, seeded). The
null alone (the compiled engine, one pinned core, `nice` 10, load 10.1, 2026-10-15 04:16,
81 s of CPU): observed mean 0.085, max 0.150; 99th at most 0.180, 99.9th at most 0.230
(buridan-01); no pair of 5,000 x 34 beyond 0.25, 1.1 percent of the tables beyond 0.20.
**Tolerance: TV 0.25 at 200 runs.** The comparison (two cores, `nice` 10, load 10.3,
2026-10-15 04:19, 100 s wall; compiled 80 s of CPU for 13,600 runs, python 100 s for
6,800; commit `2739793cf43f`): **every TV is 0.000**, mean 0.000, max
0.000, and every seed gave the same answer on both engines (200 of 200 per
problem), since the compiled engine performs the pure one's operations in the pure one's
order as this loop leaves it (D-111, D-112). The null is the only measurement with
content; the tolerance is for the day the engines diverge. No problem is beyond the
tolerance, so there is no finding. The record the fast test reads is
`tests/data/equivalence.json` (the pure engine's counts, its `accel.info()`, the commit
and the date); the full record, with every count and the null, is in the loop's folder.

| problem | answers | TV, compiled against python | null, observed | null, 99th | null, 99.9th |
|---|---:|---:|---:|---:|---:|
| surround-01 | 3 | 0.000 | 0.115 | 0.130 | 0.150 |
| surround-02 | 6 | 0.000 | 0.090 | 0.155 | 0.195 |
| surround-03 | 6 | 0.000 | 0.065 | 0.155 | 0.175 |
| surround-04 | 5 | 0.000 | 0.070 | 0.155 | 0.180 |
| surround-05 | 8 | 0.000 | 0.110 | 0.160 | 0.190 |
| surround-06 | 9 | 0.000 | 0.060 | 0.165 | 0.195 |
| surround-07 | 8 | 0.000 | 0.150 | 0.160 | 0.195 |
| surround-08 | 7 | 0.000 | 0.080 | 0.160 | 0.195 |
| surround-09 | 9 | 0.000 | 0.085 | 0.150 | 0.180 |
| surround-10 | 15 | 0.000 | 0.080 | 0.180 | 0.200 |
| blockage-01 | 2 | 0.000 | 0.035 | 0.075 | 0.100 |
| blockage-02 | 3 | 0.000 | 0.045 | 0.105 | 0.150 |
| blockage-03 | 4 | 0.000 | 0.090 | 0.140 | 0.175 |
| blockage-04 | 5 | 0.000 | 0.070 | 0.150 | 0.175 |
| blockage-05 | 5 | 0.000 | 0.075 | 0.145 | 0.185 |
| blockage-06 | 5 | 0.000 | 0.080 | 0.155 | 0.180 |
| blockage-07 | 9 | 0.000 | 0.130 | 0.175 | 0.190 |
| blockage-08 | 5 | 0.000 | 0.100 | 0.155 | 0.185 |
| blockage-09 | 5 | 0.000 | 0.070 | 0.150 | 0.175 |
| blockage-10 | 5 | 0.000 | 0.050 | 0.145 | 0.180 |
| buridan-01 | 7 | 0.000 | 0.060 | 0.165 | 0.230 |
| buridan-02 | 5 | 0.000 | 0.060 | 0.150 | 0.180 |
| buridan-03 | 6 | 0.000 | 0.070 | 0.165 | 0.185 |
| buridan-04 | 7 | 0.000 | 0.145 | 0.165 | 0.195 |
| buridan-05 | 7 | 0.000 | 0.120 | 0.170 | 0.205 |
| buridan-06 | 7 | 0.000 | 0.070 | 0.165 | 0.200 |
| buridan-07 | 5 | 0.000 | 0.075 | 0.150 | 0.180 |
| buridan-08 | 7 | 0.000 | 0.075 | 0.165 | 0.200 |
| buridan-09 | 7 | 0.000 | 0.115 | 0.170 | 0.205 |
| buridan-10 | 3 | 0.000 | 0.095 | 0.140 | 0.165 |
| buridan-11 | 4 | 0.000 | 0.110 | 0.145 | 0.175 |
| buridan-12 | 8 | 0.000 | 0.095 | 0.175 | 0.210 |
| buridan-13 | 7 | 0.000 | 0.105 | 0.160 | 0.190 |
| figure-ground-01 | 4 | 0.000 | 0.055 | 0.145 | 0.160 |

34 problems: mean TV 0.000, max 0.000; null observed mean 0.085, max 0.150; null 99th max
0.180, 99.9th max 0.230; tolerance 0.25.

**Every bench on the compiled engine** (item 07, 2026-10-15; "How these numbers were made"
gives the commands, the build and the times). Every `results/NAME.json` is now the compiled
engine's. The previous records (the roles loop's item 12, commit `b810ea0`, the pure
engine) were compared with the new ones field for field and, for every answer distribution
they hold (a survey's tallies, a comparison's answer counts), by total variation (TV)
against D-113's tolerance of 0.25:

| Bench | Records | Identical to the item-12 record (`engine` aside) | Distributions compared | Max TV | Distributions with TV > 0 |
|---|---:|---:|---:|---:|---:|
| calibrate-buridan | 1 | 0 | 7 | 0.008 | 7 |
| calibrate-human | 1 | 0 | 84 | 0.020 | 26 |
| ch2 | 19 | 12 | 19 | 0.016 | 7 |
| ch4 | 1 | 0 | 1 | 0.008 | 1 |
| churn | 4 | 2 | 0 | - | - |
| families-buridan | 35 | 17 | 35 | 0.014 | 18 |
| families-french-faults | 35 | 18 | 35 | 0.014 | 17 |
| families-human | 35 | 17 | 35 | 0.014 | 18 |
| families | 35 | 17 | 35 | 0.014 | 18 |
| faults | 45 | 34 | 45 | 0.014 | 11 |
| fcca-french-faults | 20 | 10 | 20 | 0.014 | 10 |
| fcca | 20 | 9 | 20 | 0.014 | 11 |
| focus | 9 | 0 | 9 | 0.020 | 9 |
| humans | 23 | 10 | 46 | 0.014 | 24 |
| lesions | 32 | 18 | 32 | 0.058 | 14 |
| potelbat | 53 | 29 | 53 | 0.016 | 24 |
| rho | 28 | 15 | 28 | 0.026 | 13 |
| robustness | 2 | 0 | 0 | - | - |
| stated | 54 | 34 | 29 | 0.016 | 10 |
| temperature | 12 | 7 | 12 | 0.008 | 5 |
| variants | 477 | 223 | 0 | - | - |

No distribution is beyond the tolerance (max TV 0.058, the `temperature-high` lesion on
Surround 5: 500 capped runs spread over eight answers, the cup 105 against 98, the knife 40
against 52), so there is no finding against the engines. But the distributions are not
identical either, where items 02 to 05 found the two engines identical run for run, and
the explanation is not the compiled engine: **the item-12 records predate the roles loop's
last engine change.** They were made at loop0007's iteration 14 (`b810ea0`, 2026-10-14
16:40); its iteration 20 (`4c0e3ed`, 22:11, the re-run of item 06: the answer from group
position, consumer C3 and CONTAINER, D-087 revised, D-097) changed the engine afterwards,
regenerated the goldens and logged the change, but did not re-run the benches. The
differing surveys are concentrated on the Surround and Buridan families (in the chapter 5
sets, 18 of 35: Surround 2-10, Buridan 1-8 and 11; no Blockage set, not Figure/Ground 1).
The check: the pre-loop pure engine (a `git archive` copy of BASELINE, the commit after
loop0007's last) run on Surround 5, Surround 10 and Buridan 1 (both programs) at 500 runs
gave the compiled engine's records run for run (every seed, answer, structure value and
codelet count; 2,000 runs, 70 s on one core). So the compiled engine's numbers are the
pre-loop engine's, and the item-12 numbers that the prose of §2-§11 reads differ from them
by loop0007's own last change: by at most 0.058 of TV on any survey and by at most 0.014 on
any chapter 5 result set:

| Result set | French's main answer | python engine, item-12 records % | compiled engine % | TV |
|---|---|---:|---:|---:|
| Surround 1 | E2 | 56.8 | 56.8 | 0.000 |
| Surround 2 | E2 | 51.0 | 50.6 | 0.004 |
| Surround 3 | E2 | 51.6 | 51.8 | 0.004 |
| Surround 4 | E2 | 49.2 | 49.2 | 0.000 |
| Surround 5 | E1 | 14.6 | 15.2 | 0.008 |
| Surround 6 | E1 | 28.8 | 28.6 | 0.010 |
| Surround 7 | E1 | 15.2 | 15.4 | 0.012 |
| Surround 8 | E2 | 51.8 | 51.4 | 0.008 |
| Surround 9 | E2 | 54.8 | 54.8 | 0.002 |
| Surround 10 | E1 | 17.0 | 17.0 | 0.008 |
| Blockage 1 | E1 | 91.2 | 91.2 | 0.000 |
| Blockage 2 | E1 | 79.6 | 79.6 | 0.000 |
| Blockage 3 | E1+E2 | 85.4 | 85.4 | 0.000 |
| Blockage 4 | E1+E2 | 85.0 | 85.0 | 0.000 |
| Blockage 5 | E1+E2 | 83.2 | 83.2 | 0.000 |
| Blockage 6 | E3 | 9.2 | 9.2 | 0.000 |
| Blockage 7 | E3 | 11.2 | 11.2 | 0.000 |
| Blockage 8 | E3 | 7.8 | 7.8 | 0.000 |
| Blockage 9 | E1+E2 | 84.4 | 84.4 | 0.000 |
| Blockage 10 | E1+E2 | 77.8 | 77.8 | 0.000 |
| Buridan 1 (original program) | E1 | 28.8 | 28.8 | 0.008 |
| Buridan 1 (modified program) (modified) | E1 | 28.8 | 28.8 | 0.008 |
| Buridan 2 (modified) | E1 | 36.6 | 37.0 | 0.008 |
| Buridan 3 (modified) | E2 | 17.8 | 18.0 | 0.002 |
| Buridan 4 (modified) | E1 | 25.2 | 25.2 | 0.004 |
| Buridan 5 (modified) | E2 | 24.6 | 24.6 | 0.002 |
| Buridan 6 (modified) | E2 | 17.0 | 17.8 | 0.014 |
| Buridan 7 (modified) | E2 | 18.2 | 18.2 | 0.004 |
| Buridan 8 (modified) | E2 | 20.0 | 20.4 | 0.004 |
| Buridan 9 (modified) | E1 | 36.0 | 36.0 | 0.000 |
| Buridan 10 (modified) | E1 | 48.2 | 48.2 | 0.000 |
| Buridan 11 (modified) | E1 | 48.4 | 48.4 | 0.012 |
| Buridan 12 (modified) | E1 | 26.6 | 26.6 | 0.000 |
| Buridan 13 (modified) | E1 | 36.4 | 36.4 | 0.000 |
| Figure/ground 1 | E1+E2 | 69.4 | 69.4 | 0.000 |

The calibration scans, re-run as a measurement (ANSWERS.md C5): `human` chooses 40 and
`buridan` 150 again, as the item-12 scans did, and the profiles keep v1's 50 and 40
pending the author. The robustness bench's speed profile, the one bench measurement that
changes with the engine (one core, load 13): Surround 5 108 runs per second plain and 47
with every invariant checked after every codelet (item 12's record: 19 and 13), Blockage 2
369 and 194 (21 and 29), Blockage 6 226 and 117 (25 and 22), Buridan 1 140 and 70 (20 and
17); the checking's overhead is x1.9 to x2.3 now that the run itself is fast. Nothing else a
bench measures depends on the engine: no run capped under `standard`, no invariant violated
on the 500 random tables, as before.
