# `results/v3/`: the roles engine on v3's optimized defaults

The stripped records of every bench (`../NAME.json`, D-075) as they stood on 2026-10-17,
before `standard` took v4's defaults (D-115): the roles engine on v3's defaults, the 67
values of fold 1 trial 244 of the first cross-validated Bayesian optimization (D-114),
recorded at 500 runs per problem with bench seed 1 on the compiled engine on 2026-10-16/17
(build stamp `2879d92df4ae`, and `3d83423d56cb` for the `rho` bench), the numbers that
RESULTS.md §12 reads. They are byte-for-byte copies of `../NAME.json` at that time, made
before the benches were re-run on v4, and they never change.

`tabletop.bench.results.Results` reads this folder as its `v3` source: the `v3` block of
RESULTS.md §12 takes its v3 column from here (so it reads as v3 left it), and the `v4` block
of §13 sets French | v1 | v2 | v3 | v4 side by side from `../v1/`, `../v2/`, this folder and
`../`. Every other block of RESULTS.md reads `../NAME.json`, v4's. `results/role-probes.json`
(RESULTS.md §11.3-11.4) was made on v2 and is not copied here. The v3 configuration itself is
`dataclasses.replace(Config(), **dict(V3_DEFAULTS))` (`tabletop/config.py`).
