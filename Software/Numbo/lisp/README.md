# Numbo on SBCL

The 1987 Numbo source (`../numbo-digitized/`), ported to SBCL.

- [`src/README.md`](src/README.md): how to run it, what was changed and why, known limitations.
- [`src/PORTING_NOTES.md`](src/PORTING_NOTES.md): every change, with scan page references.
- [`src/RESULTS.md`](src/RESULTS.md): the chapter's puzzles × 20 seeds.
- `tests/run-tests.sh`: the Lisp tests. `tests/oracle/` holds the scripts that capture the Python implementation's fixtures (`../python/fixtures/`).

All commands in these docs run from this folder (`lisp/`).
