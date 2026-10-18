# The folder as a repository of its own

`Software/Tabletop/` is read in two places: as a folder of FARGonautica and, later, as the
repository `git subtree split --prefix=Software/Tabletop` makes of it (ANSWERS.md 6, the
author's decision; the loop's TASK.md calls it "Standalone"). Everything the folder needs is
inside it; the source PDFs are found only through environment variables or an enclosing
FARGonautica checkout, and the tests that need them skip, with a reason naming the variable,
when they are not found (see [`../README.md`](../README.md), "The sources").

This document records the procedure that checks it and the results of 2026-10-13 (item 30 of
the loop that built the package), of 2026-10-14 (after the roles) and of 2026-10-15 (the
compiled engine, D-107: a checkout has no build, so it must refuse to run, build where it can,
and run its tests on the pure engine by explicit request alone). `tools/standalone_check.sh`
runs the whole procedure; the results below are its output, read step by step.

## The rules the folder keeps

Checked by `tests/test_standalone.py`, `tests/test_links.py` and `tests/test_readme.py`:

- Nothing under the folder imports anything but the standard library, `tabletop`, pytest and
  its own test helpers, except the GUI's places (`tabletop/gui/`, `tests/gui/`, `tests_slow/`, `scripts/`),
  which may import PySide6, shiboken6 and pytest-qt; no `sys.path` tricks; no absolute home
  path, no path into the author's PDF store, no path into the loop's folders.
- The PDFs are found by one lookup, `tools/check_quotes.py`'s `fargonautica()` and
  `source_path()`, which `tests/sources.py` and `tools/check_links.py` load by file:
  `$F95_PDF` and `$FH91_PDF` first, else `Literature/` of `$FARGONAUTICA`, else `Literature/`
  of the nearest ancestor directory holding both `.git` and `Literature/`; FCCA only through
  `$FCCA_PDF`.
- Every relative Markdown link resolves inside the folder (and its `#fragment` to a heading);
  every link that leaves it is an absolute URL, FARGonautica's files as
  `https://github.com/fargonauts/FARGonautica/blob/master/<path>` with spaces as `%20`.
- The standalone repository's own URL is `https://github.com/fargonauts/Tabletop`
  (`git@github.com:fargonauts/Tabletop.git`), confirmed by the author on 2026-10-13.

## The procedure

Run from inside a FARGonautica checkout, never in the working repository, never pushed:

```sh
cd python && bash tools/standalone_check.sh --online --keep     # the whole procedure
```

What it does, as commands (`$REPO` the FARGonautica checkout, `$SCRATCH` a fresh directory
under `/tmp`):

```sh
# 1-2. A scratch clone, the split, and the split branch checked out as its own repository.
git clone --no-local "$REPO" "$SCRATCH/clone"
git -C "$SCRATCH/clone" subtree split --prefix=Software/Tabletop -b tabletop-standalone
git clone -b tabletop-standalone "$SCRATCH/clone" "$SCRATCH/Tabletop"
cd "$SCRATCH/Tabletop/python"
# 2b. The checkout has no build of the compiled engine (D-107, D-110): with TABLETOP_ENGINE
#    unset the CLI must refuse to start, naming the build, and run nothing (no fallback).
env -u TABLETOP_ENGINE python3 -m tabletop surround-05 --seed 3 --quiet    # exit 1
# 3. The tests with no source variable set, on the pure engine by explicit request (the
#    only value the script ever gives the variable): the PDF tests skip, the tests that need
#    the compiled engine skip with a reason, everything else passes (without PySide6 the
#    GUI's Qt tests skip too, each file by pytest.importorskip).
env -u FARGONAUTICA -u F95_PDF -u FH91_PDF -u FCCA_PDF TABLETOP_ENGINE=python python3 -m pytest -q -rs
# 4. The tests with the sources: FARGONAUTICA names the checkout, FCCA_PDF the author's copy.
FARGONAUTICA="$REPO" FCCA_PDF=... TABLETOP_ENGINE=python python3 -m pytest -q -rs
# 4b. Where a C compiler and Cython are found (the module, TABLETOP_CYTHON or PATH): the
#    compiled engine built in the checkout, and the suite on it (the default engine).
python3 tools/build_fast.py --jobs 4
env -u FARGONAUTICA -u F95_PDF -u FH91_PDF -u FCCA_PDF -u TABLETOP_ENGINE python3 -m pytest -q -rs
# 5. A fresh virtual environment, the install (which builds the same extension; Cython is
#    fetched by pip), the installed command on the engine it built, the suite in parallel.
#    Without a compiler the install must fail with the build's message, and the rest skips.
python3 -m venv .venv && .venv/bin/python -m pip install -e '.[test]'
.venv/bin/tabletop --list | wc -l                       # 53
env -u TABLETOP_ENGINE .venv/bin/tabletop surround-05 --seed 3 --quiet   # "engine compiled"
env -u TABLETOP_ENGINE PATH=.venv/bin:$PATH bash run-tests.sh            # pytest-xdist, -n auto
# 6. The links, in both places.
python3 tools/check_links.py "$REPO/Software/Tabletop"                     # inside FARGonautica
env -u FARGONAUTICA python3 tools/check_links.py "$SCRATCH/Tabletop"       # on its own
python3 tools/check_links.py "$SCRATCH/Tabletop" --fargonautica "$REPO" --online
```

The split checks the repository's committed state (the clone has no working-tree changes), so
the procedure is run after a commit or reads the previous one; the loop's gate runs the test
suite in a plain copy of the working tree outside the repository, with the variables unset,
at every iteration.

Offline, step 5 cannot make a bare virtual environment install the package, because pip has
to download setuptools for the build backend; `bash tools/standalone_check.sh
--offline-install` uses a venv that sees the system's packages and
`pip install --no-index --no-build-isolation -e .` instead (the engine and the command, no
pytest), as [`../../README.md`](../../README.md), "Installing", explains.

## Results, 2026-10-13

| Step | Result |
|---|---|
| 1-2. The split and its checkout | 30 commits (every commit of the loop that touched the folder, hashes rewritten); the root holds `README.md` and `python/`, nothing else |
| 3. Tests, no variables | 795 passed, 7 skipped, 3 deselected (the `slow` set) in 43 s. The seven skips are the PDF tests, each naming its variable: six for F95 and FH91 (`tests/test_ch2.py` and `tests/test_quotes.py`: "set F95_PDF, or FARGONAUTICA to a FARGonautica checkout"), one for FCCA ("set FCCA_PDF to a copy of the book") |
| 4. Tests, `FARGONAUTICA` and `FCCA_PDF` set | 802 passed, 3 deselected in 55 s; no skips |
| 5. `python3 -m venv .venv` and `pip install -e '.[test]'` | installed online (setuptools fetched by pip; pytest 9.1.1, pytest-xdist 3.8.0 in the venv); `tabletop --list` prints the 53 problems; `tabletop surround-05 --seed 3 --quiet` prints the quick start's two lines; `bash run-tests.sh` in the venv (in parallel) 795 passed, 7 skipped in 20 s, and `--all` with the sources 805 passed in 39 s |
| 5, offline | a venv with `--system-site-packages` and `pip install --no-index --no-build-isolation -e .` installs and `tabletop --list` works; a bare venv with `--no-index` fails, as pip cannot fetch the build backend |
| 6. Links, inside FARGonautica | 62 links in 4 Markdown files: 32 relative, 23 anchors, 5 to FARGonautica's files on GitHub, 2 other URLs; every one good, the five GitHub paths existing in the checkout |
| 6. Links, on its own | the same 62 links, every one good; the GitHub paths only counted (no checkout) |
| 6. Links, in the working tree after this item | 67 links in 5 Markdown files (this document adds five), every one good |
| 6. Links, online | 3 of 6 distinct URLs answer 200 (the repository, `copycat.md`, the 1991 paper); 404 for `https://github.com/fargonauts/Tabletop` (not yet created), for `Literature/Tabletop-French-1995-The Subtlety of Sameness.pdf` and for `Software/SeekWhence/README.md` |

The three online 404s are not defects of the folder. GitHub's `fargonauts/FARGonautica` is
behind the Gitea origin the loop pushes to: on 2026-10-13 its `Literature/` listed the 1991
paper but not French's 1995 book, and its `Software/` held Capyblanca, Copycat, Letter-Spirit,
Metacat and Musicat, neither Seek-Whence nor Tabletop. Both files are tracked in the checkout
and the links are the ones that will work once the author pushes; the standalone URL was
confirmed on 2026-10-13. The online step fails until those pushes, and the script says so.

## What the check found and fixed

- `python/.gitignore` did not list `.venv/`, so a reader following the README's install saw
  the environment as untracked; added (the egg-info the editable install writes was already
  ignored).
- Under pytest-xdist the test process is multi-threaded and Python warned, twice, that the
  calibration bench's process pool forked it ("use of fork() may lead to deadlocks"); the pool
  now starts its workers from a fork server (`bench/calibrate.py`, `start_context`), and the
  scans give the same records with and without workers, as before (`tests/test_calibrate.py`).
- The README's "Installing" section said nothing about offline installs; it now gives the
  `--system-site-packages` and `--no-build-isolation` form.

## Results, 2026-10-14 (after the roles)

The whole procedure, offline (`bash tools/standalone_check.sh --offline-install --keep`),
re-run at item 16 of the roles loop on the committed state of that day (the GUI and the
roles in; the machine at load average 30 to 45 on 32 cores, which the times reflect):

| Step | Result |
|---|---|
| 1-2. The split and its checkout | 73 commits; the root holds `README.md` and `python/`, nothing else |
| 3. Tests, no variables | 1512 passed, 8 skipped in 402 s. The eight skips each name the variable to set: six for F95 and FH91 (`tests/test_ch2.py`, `tests/test_quotes.py`), one for FCCA, and `tests/test_links.py`'s GitHub-path check ("no FARGonautica checkout found: set FARGONAUTICA"). The GUI's tests ran, offscreen, since PySide6 is installed on the machine |
| 4. Tests, `FARGONAUTICA` set | 1519 passed, 1 skipped (FCCA, not given to the script) in 176 s |
| 5, offline. `python3 -m venv --system-site-packages .venv` and `pip install --no-index --no-build-isolation -e .` | installed; `tabletop --list` prints the 53 problems; `tabletop surround-05 --seed 3 --quiet` prints the quick start's two lines; `bash run-tests.sh` in the venv (pytest from the system's packages) 1512 passed, 8 skipped in 192 s |
| 6. Links, inside FARGonautica | 148 links in 8 Markdown files: 109 relative, 32 anchors, 5 to FARGonautica's files on GitHub, 2 other URLs; every one good, the five GitHub paths existing in the checkout |
| 6. Links, on its own | 133 links in 7 Markdown files (the split holds the previous commit, without `docs/roles.md`), every one good; the GitHub paths only counted |

Every step passed; nothing was found to fix. The online step was not run (the three 404s of
2026-10-13 stand until the author's pushes, as above).

## Checking again

```sh
cd python
python3 tools/check_links.py                    # the links, offline, in whichever place this is
python3 tools/check_links.py --online           # and every URL's status
bash tools/standalone_check.sh --online --keep  # the whole procedure (a few minutes)
python3 -m pytest -q tests/test_links.py tests/test_standalone.py tests/test_readme.py
```

## Results, 2026-10-15 (the compiled engine)

The whole procedure online (`bash tools/standalone_check.sh --keep`, `TABLETOP_CYTHON` naming
a Cython 3.3.0 executable, the system `python3` 3.12 with gcc 15.2; pinned to four cores at
`nice` 10, the machine at load 10 to 11 on 32 cores; 04:48 to 04:55), re-run at item 06 of
the fast-engine loop on the committed state of that morning (item 05's; the checkout holds the
last commit, so the GUI's engine note and this item's test fixes were not in it):

| Step | Result |
|---|---|
| 1-2. The split and its checkout | 82 commits; the root holds `README.md` and `python/`, nothing else; no build artefact (they are git-ignored) |
| 2b. No build: the CLI refuses | `python3 -m tabletop surround-05 --seed 3 --quiet` with `TABLETOP_ENGINE` unset exits 1 with `EngineError: the compiled Tabletop engine is not built (...): run python3 tools/build_fast.py ...`, and runs nothing |
| 3. Tests, no variables, `TABLETOP_ENGINE=python` | 1553 passed, 149 skipped, 3 failed in 136 s. The skips: the eight PDF and GitHub-path tests as before, and 141 tests that need the compiled engine, each saying so ("the compiled engine is not the one running (TABLETOP_ENGINE=python was requested explicitly)"). The three failures were the GUI tests that start a smoke run in a bare environment (`tests/gui/test_gui_readme.py`'s two block tests, `tests/gui/test_packaging.py`'s capped smoke run): they did not pass the explicit request on to the subprocess, which then refused for want of a build. Fixed in this item (the bare environments carry `TABLETOP_ENGINE` through when it is set, as `tests/test_readme.py` already did, and the GUI readme test reads the documented `engine compiled` as the engine running); the fixed files run on the pure engine in a copy of the working tree without the extension: 994 engine tests and 568 GUI tests passed, 150 skipped |
| 4. Tests, `FARGONAUTICA` set, `TABLETOP_ENGINE=python` | 1560 passed, 142 skipped (FCCA not given; the compiled-engine skips), the same 3 failures, in 136 s |
| 4b. The compiled engine built in the checkout | `python3 tools/build_fast.py --jobs 4`: built, stamp `2879d92df4ae`, flags `-O2 -ffp-contract=off`, gcc 15.2.0, Cython 3.3.0 (the executable of `TABLETOP_CYTHON`); the suite on it, no variables: 1697 passed, 8 skipped in 132 s |
| 5. `python3 -m venv .venv` and `pip install -e '.[test]'` | installed online: pip fetched setuptools and Cython into its build environment and built the extension in place; `tabletop --list` prints the 53 problems; `tabletop surround-05 --seed 3 --quiet` with `TABLETOP_ENGINE` unset prints the quick start's two lines, the first saying `engine compiled`; `bash run-tests.sh` in the venv (pytest-xdist, four workers, no PySide6 there) 1511 passed, 36 skipped in 17 s |
| 6. Links, inside FARGonautica | 155 links in 11 Markdown files: 115 relative, 33 anchors, 5 to FARGonautica's files on GitHub, 2 other URLs; every one good |
| 6. Links, on its own | the same 155 links, every one good; the GitHub paths only counted |

Two steps failed, both the python-engine test runs, on the three GUI tests above; the
refusal, the build, the compiled suite, the install and the links passed. The online step
was not run. What the check found and fixed, besides the three tests: the front page's bench
block (`sed -n 3,11p`) had not followed the benches' new first line (`engine: compiled`) and
now prints lines 4 to 12, as its shown output always was; `tools/run_on_baseline.py`, which
makes a worktree of a commit, now builds the compiled engine there (or notes the plain
package of a commit before it) and runs the pure one on `--engine python` alone, since a
fresh worktree has no build and must not fall back.
