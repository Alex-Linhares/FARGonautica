#!/usr/bin/env bash
# Run Perl against the original Seqsee lib/, deterministically:
# fixed hash seed, no key perturbation, ASLR off (Class::Std keys on addresses).
# Usage: oracle/run_perl.sh script.pl [args...]   or   oracle/run_perl.sh -e 'code'
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
export PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:$PERL5LIB}"
export PERL_HASH_SEED=0 PERL_PERTURB_KEYS=0
# Script paths are relative to the caller's cwd; perl runs from the repo root.
if [[ $# -gt 0 && -f "$1" ]]; then set -- "$(realpath "$1")" "${@:2}"; fi
cd "$repo"
# Cap virtual memory (default 4 GB) so a runaway oracle dies with "Out of memory!"
# instead of triggering the kernel OOM killer (a 100 GB run killed loop0001 iter 37).
ulimit -v "${ORACLE_MEM_KB:-4000000}"
exec setarch "$(uname -m)" -R perl -I"$repo/lib" -I"$here" "$@" 2> >(grep -vE ' is deprecated at |__Positional_' >&2)
