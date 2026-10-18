# `results/v4/`: the roles engine on v4's optimized defaults, before the structural Blockage fix

The stripped records of every bench (`../NAME.json`, D-075) as they stood on 2026-10-17,
before the structural Blockage fix (v5, D-116 to D-120) changed the engine: the roles engine
on v4's defaults, the 65 values of fold 1 trial 800 of the second cross-validated Bayesian
optimization (D-115), recorded at 500 runs per problem with bench seed 1 on the compiled
engine on 2026-10-17 (build stamp `3d83423d56cb`), the numbers that RESULTS.md §13 reads.
They are byte-for-byte copies of `../NAME.json` at that time, made before the benches were
re-run on v5, and they never change.

`tabletop.bench.results.Results` reads this folder as its `v4` source: the `v4` block of
RESULTS.md §13 takes its v4 column from here (so it reads as v4 left it), and the `v5` block
of §14 sets French | v1 | v2 | v3 | v4 | v5 side by side from `../v1/`, `../v2/`, `../v3/`,
this folder and `../`. Every other block of RESULTS.md reads `../NAME.json`, v5's.
`results/role-probes.json` (RESULTS.md §11.3-11.4) was made on v2 and is not copied here.
The v4 configuration can no longer be built as a `Config`: its `proximity_zero_gap`
(5.4786 glass widths) is above the bound D-118 sets (2.6); its values stand in the comments
of `tabletop/config.py` and in D-115's table.
