# python/scripts

Development scripts (not part of the package).

| Script | What it does |
|---|---|
| `regen_fixtures.sh [OUT_DIR]` | Regenerates `python/fixtures/` from the SBCL oracle by running every `lisp/tests/oracle/*.lisp`. |
| `gen_pnet_def.py [--check]` | Regenerates the Pnet tables in `numbo/pnet_def.py` from `lisp/src/pnet-def.lisp`. With `--check`, it only reports whether they are current; a test runs this. |
| `chapter_runs.py [--write \| --check]` | Runs the chapter's 11 puzzles × seeds 1–20 in Python and writes, or checks, `python/RESULTS.md`. |
| `docs_screenshots.py` | Regenerates the README's screenshots in `python/docs/`, offscreen. |
