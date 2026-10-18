# `results/v1/`: the reconstruction's numbers before roles

The stripped records of every bench (`../NAME.json`, D-075) as they stood at commit
`e38d6e0b0c70edd6ea8cc565bba2fba58c3cc655` (2026-10-14), the last commit of the engine
before roles were built into it: the untuned reconstruction of French's program, "v1"
(D-078). They are byte-for-byte copies of the records that RESULTS.md §1-§10 report, made
on 2026-10-14 by item 00 of the loop that adds the roles, and they never change: a later
comparison that needs v1 at a new size or on a new bench re-runs that commit, from a
worktree outside the repository, and records the result here with its own file name.

`tabletop.bench.results.Results` reads this folder as its `v1` column source, so that a
table can set French | v1 | roles side by side; the tables of RESULTS.md §1-§10 are built
from `../NAME.json` alone and render as before. Every record here is at 500 runs per
problem with bench seed 1 (D-072), as RESULTS.md's "How these numbers were made" says.

**`variants.json`** (added 2026-10-14 by item 02 of the roles loop at 100 runs, remade at 500
runs by item 12 the same day; D-102) is the one record made after that date: the variants
bench, which did not exist at the baseline, run against the baseline commit's engine through
`tools/run_on_baseline.py` (a detached worktree of `e38d6e0` outside the repository, the
current tree's `bench/variants.py` loaded into it; at item 12 the tool's inner stage with the
per-run call pooled, the same seeds and records), 500 runs per problem and variant, bench
seed 1, in three parts merged by `strip`. The roles' run of the same bench, at the same size,
is `../variants.json`.

The `churn` bench (D-106, the roles loop's item 11) reads the role pool, which v1 had not, so it has no record here (`bench/results.py`'s `ROLES_ONLY`).
