#!/usr/bin/env bash
# The standalone-readiness check of docs/standalone.md (ANSWERS.md 6), automated:
# Software/Tabletop/ must work as the repository `git subtree split` will make of it.
#
#   bash tools/standalone_check.sh [--online] [--offline-install] [--keep] [SCRATCH]
#
# Run from inside a FARGonautica checkout (this folder is its Software/Tabletop/). Never
# touches the working repository and never pushes anything: in a scratch directory under
# /tmp (or SCRATCH) it
#   1. clones the repository and runs `git subtree split --prefix=Software/Tabletop`;
#   2. checks the split branch out as its own repository (README.md and python/ at the root);
#      the checkout has no build of the compiled engine, so the CLI must refuse to start
#      there with TABLETOP_ENGINE unset, naming the build (D-107: no fallback);
#   3. runs the tests there with no source variable set (the PDF tests must skip), on the
#      pure engine by explicit request (TABLETOP_ENGINE=python, the only value this script
#      ever sets; the tests that need the compiled engine skip, with a reason);
#   4. runs them again with FARGONAUTICA set to the repository (and FCCA_PDF as given);
#   4b. where a C compiler and Cython (the module, $TABLETOP_CYTHON or `cython` on PATH)
#      are found, builds the compiled engine in the checkout with tools/build_fast.py and
#      runs the suite on it (the default engine, no variable); otherwise says SKIP and why;
#   5. makes a fresh virtual environment and installs the package in it
#      (`pip install -e '.[test]'`, which builds the same extension, Cython fetched by pip;
#      with --offline-install, a venv that sees the system's packages and
#      `pip install --no-index --no-build-isolation -e .`, Cython from TABLETOP_CYTHON or
#      PATH), then runs the installed command (`tabletop --list`, a run that must name the
#      compiled engine) and the suite through run-tests.sh on the engine the install built.
#      Without a compiler (or, offline, without Cython) the install must fail with a message
#      naming what is missing, and the installed-command steps are skipped;
#   6. checks every Markdown link in both places with tools/check_links.py (and online,
#      with --online).
# Each step prints PASS or FAIL; the exit status is the number of failed steps. The scratch
# directory is removed when everything passed, unless --keep; it is always kept on failure.
# Everything runs on at most 4 cores (`--jobs 4`; pytest serially unless xdist is in the
# venv, where -n auto is capped by the caller's taskset).
set -uo pipefail

ONLINE=0; OFFLINE_INSTALL=0; KEEP=0; SCRATCH=""
for arg in "$@"; do
  case "$arg" in
    --online) ONLINE=1 ;;
    --offline-install) OFFLINE_INSTALL=1 ;;
    --keep) KEEP=1 ;;
    -h|--help) sed -n '2,36p' "$0"; exit 0 ;;
    *) SCRATCH="$arg" ;;
  esac
done

HERE="$(cd "$(dirname "$0")" && pwd)"                 # python/tools
FOLDER="$(cd "$HERE/../.." && pwd)"                   # Software/Tabletop
REPO="${FARGONAUTICA:-}"
if [[ -z "$REPO" ]]; then
  d="$FOLDER"
  while [[ "$d" != "/" ]]; do
    if [[ -e "$d/.git" && -d "$d/Literature" ]]; then REPO="$d"; break; fi
    d="$(dirname "$d")"
  done
fi
if [[ -z "$REPO" || ! -d "$REPO/Software/Tabletop" ]]; then
  echo "This script runs inside a FARGonautica checkout (or with FARGONAUTICA set): none found." >&2
  exit 2
fi
if [[ ! -x "$(git --exec-path)/git-subtree" ]]; then
  echo "git subtree is not available (it comes with git's contrib)." >&2
  exit 2
fi
[[ -z "$SCRATCH" ]] && SCRATCH="$(mktemp -d /tmp/tt-standalone.XXXXXX)"
mkdir -p "$SCRATCH"
export PYTHONDONTWRITEBYTECODE=1
FAILED=0
step() {                                               # step NAME COMMAND... (logged)
  local name="$1"; shift
  local log="$SCRATCH/$name.log"
  if "$@" >"$log" 2>&1; then
    echo "PASS $name"
  else
    echo "FAIL $name  (see $log)"; tail -5 "$log" | sed 's/^/     /'; FAILED=$((FAILED + 1))
  fi
}
echo "repository: $REPO"
echo "scratch:    $SCRATCH"

# 1-2. The split, and the checkout of the split branch as a repository of its own.
step clone git clone -q --no-local "$REPO" "$SCRATCH/clone"
step split git -C "$SCRATCH/clone" subtree split --prefix=Software/Tabletop -b tabletop-standalone
step checkout git clone -q -b tabletop-standalone "$SCRATCH/clone" "$SCRATCH/Tabletop"
TT="$SCRATCH/Tabletop"
if [[ -d "$TT/python" ]]; then
  echo "     $(git -C "$TT" rev-list --count HEAD) commits; root: $(ls "$TT" | tr '\n' ' ')"
else
  echo "no checkout; stopping."; exit $((FAILED + 1))
fi

# 2b. The checkout has no build of the compiled engine: with TABLETOP_ENGINE unset the CLI
# must refuse to start, naming the build, and run nothing (D-107: no fallback).
refuses() {
  local out status
  out="$( cd "$TT/python" && env -u TABLETOP_ENGINE python3 -m tabletop surround-05 --seed 3 --quiet 2>&1 )"
  status=$?
  printf '%s\n' "$out"
  [[ "$status" -ne 0 ]] && grep -q 'build_fast.py' <<<"$out" && ! grep -q 'touches' <<<"$out"
}
step no-build-refuses refuses
echo "     $(grep -m1 'not built' "$SCRATCH/no-build-refuses.log" | cut -c1-110)"

# 3-4. The tests, without the source variables and with them, on the pure engine by
# explicit request (D-107; the checkout has no build); the tests that need the compiled
# engine skip there, with a reason.
novars() { ( cd "$TT/python" && env -u FARGONAUTICA -u F95_PDF -u FH91_PDF -u FCCA_PDF \
             TABLETOP_ENGINE=python python3 -m pytest -q -rs ); }
withvars() { ( cd "$TT/python" && FARGONAUTICA="$REPO" TABLETOP_ENGINE=python python3 -m pytest -q -rs ); }
step tests-no-variables novars
echo "     $(grep -E '^[0-9]+ passed' "$SCRATCH/tests-no-variables.log" | tail -1)"
grep -c '^SKIPPED' "$SCRATCH/tests-no-variables.log" | sed 's/^/     skipped lines: /'
step tests-with-variables withvars
echo "     $(grep -E '^[0-9]+ passed' "$SCRATCH/tests-with-variables.log" | tail -1)"
grep '^SKIPPED' "$SCRATCH/tests-with-variables.log" | sed 's/^/     /'

# 4b. The compiled engine in the checkout, where it can be built (D-110): a C compiler and
# Cython (the module of python3, the executable TABLETOP_CYTHON names, or `cython` on PATH).
CC_NAME="${CC:-$(python3 -c 'import sysconfig; print((sysconfig.get_config_var("CC") or "cc").split()[0])')}"
HAVE_CC=0; command -v "$CC_NAME" >/dev/null 2>&1 && HAVE_CC=1
HAVE_CYTHON=0
if python3 -c "import Cython" 2>/dev/null; then HAVE_CYTHON=1
elif [[ -n "${TABLETOP_CYTHON:-}" && -x "${TABLETOP_CYTHON:-}" ]]; then HAVE_CYTHON=1
elif command -v cython >/dev/null 2>&1; then HAVE_CYTHON=1
fi
build_engine() { ( cd "$TT/python" && python3 tools/build_fast.py --jobs 4 ); }
compiled_suite() { ( cd "$TT/python" && env -u FARGONAUTICA -u F95_PDF -u FH91_PDF -u FCCA_PDF \
                     -u TABLETOP_ENGINE python3 -m pytest -q -rs ); }
if [[ "$HAVE_CC" == 1 && "$HAVE_CYTHON" == 1 ]]; then
  step build-compiled-engine build_engine
  step tests-compiled-engine compiled_suite
  echo "     $(grep -E '^[0-9]+ passed' "$SCRATCH/tests-compiled-engine.log" | tail -1)"
else
  echo "SKIP build-compiled-engine and tests-compiled-engine (C compiler found: $HAVE_CC, Cython found: $HAVE_CYTHON)"
fi

# 5. A fresh virtual environment, the install (which builds the extension: the same
# setup.py), the installed command on the engine it built, the suite in it. Without what
# the build needs, pip must fail with the build's message naming what is missing.
VENV="$TT/python/.venv"
if [[ "$OFFLINE_INSTALL" == 1 ]]; then
  CAN_INSTALL=$(( HAVE_CC && HAVE_CYTHON ))
  install() { python3 -m venv --system-site-packages "$VENV" &&
              "$VENV/bin/python" -m pip install -q --no-index --no-build-isolation -e "$TT/python"; }
else
  CAN_INSTALL=$HAVE_CC                 # pip fetches Cython into its isolated build
  install() { python3 -m venv "$VENV" &&
              "$VENV/bin/python" -m pip install -q -e "$TT/python[test]"; }
fi
if [[ "$CAN_INSTALL" == 1 ]]; then
  step venv-install install
  listed() { [[ "$("$VENV/bin/tabletop" --list | wc -l)" == 53 ]]; }
  step installed-command-lists-53-problems listed
  names_engine() {                       # the install built the engine, and the run says so
    local out
    out="$( env -u TABLETOP_ENGINE "$VENV/bin/tabletop" surround-05 --seed 3 --quiet )" || return 1
    printf '%s\n' "$out"
    grep -q '  engine compiled  ' <<<"$out"
  }
  step installed-command-runs-on-the-compiled-engine names_engine
  if "$VENV/bin/python" -c "import pytest" 2>/dev/null; then
    suite() { ( cd "$TT/python" && env -u FARGONAUTICA -u F95_PDF -u FH91_PDF -u FCCA_PDF \
                -u TABLETOP_ENGINE PATH="$VENV/bin:$PATH" bash run-tests.sh ); }
    step run-tests-in-the-venv suite
    echo "     $(grep -E '^[0-9]+ passed' "$SCRATCH/run-tests-in-the-venv.log" | tail -1)"
  else
    echo "SKIP run-tests-in-the-venv (no pytest in the venv: --offline-install installs the engine only)"
  fi
else
  fails_clearly() {
    local out
    if out="$(install 2>&1)"; then printf '%s\n' "$out"; return 1; fi
    printf '%s\n' "$out"
    grep -Eq 'Cython|compiler' <<<"$out"
  }
  step venv-install-fails-clearly fails_clearly
  echo "SKIP installed-command and run-tests-in-the-venv (nothing to build with: C compiler found: $HAVE_CC, Cython found: $HAVE_CYTHON)"
fi

# 6. The links, in both places.
LINKS="$FOLDER/python/tools/check_links.py"
online=(); [[ "$ONLINE" == 1 ]] && online=(--online)
step links-in-fargonautica python3 "$LINKS" "$FOLDER" --fargonautica "$REPO"
step links-standalone env -u FARGONAUTICA python3 "$LINKS" "$TT"
if [[ "$ONLINE" == 1 ]]; then
  step links-online python3 "$LINKS" "$TT" --fargonautica "$REPO" --online
  grep -E '^ +[0-9A-Za-z]+  https' "$SCRATCH/links-online.log" | sed 's/^/   /'
fi

echo
if [[ "$FAILED" == 0 ]]; then
  echo "standalone check: every step passed."
  [[ "$KEEP" == 1 ]] && echo "kept: $SCRATCH" || rm -rf "$SCRATCH"
else
  echo "standalone check: $FAILED step(s) failed; kept: $SCRATCH"
fi
exit "$FAILED"
