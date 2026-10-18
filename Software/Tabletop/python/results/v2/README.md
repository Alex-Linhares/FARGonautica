# `results/v2/`: the roles engine on its a-priori defaults

The stripped records of every bench (`../NAME.json`, D-075) as they stood on 2026-10-16,
before `standard` took v3's optimized defaults (D-114): the roles engine (D-078 on) on the
defaults each decision set a priori, "v2", recorded at 500 runs per problem with bench seed
1 on the compiled engine by the fast-engine loop's item 07 (2026-10-15; build stamp
`2879d92df4ae`), the numbers that RESULTS.md §11 reads. They are byte-for-byte copies of
`../NAME.json` at that time, made before the benches were re-run on v3, and they never change.

`tabletop.bench.results.Results` reads this folder as its `v2` source; the `v3` block of
RESULTS.md §12 sets French | v1 | v2 | v3 side by side from `../v1/`, this folder and
`../v3/` (`../` until 2026-10-17), and the `v4` block of §13 adds v4, with the
optimization's objective J worked out from each folder's families records. Every other
block of RESULTS.md reads `../NAME.json`: v3's until 2026-10-17, v4's since (D-115).
`results/role-probes.json`
(RESULTS.md §11.3-11.4) was made on v2 and is not copied here: it stands in `../` as v2's.
The v2 configuration itself is `dataclasses.replace(Config(), **dict(V2_DEFAULTS))`
(`tabletop/config.py`).
