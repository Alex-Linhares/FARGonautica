"""python3 -m metacat: run one problem headless, as chez_scheme/oracle/run.ss does.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
Translated to Python (2026) from chez_scheme/oracle/run.ss, with racket/cli.rkt
as a worked translation.

  python3 -m metacat INITIAL MODIFIED TARGET [ANSWER]
          [--seed N] [--max-codelets K] [--keep-going] [--trace FILE] [--verbose]

Prints what the oracle's run.ss prints for the same arguments: the problem, the
commentary as it is written, each answer found (answer, quality, codelet count,
temperature) and a summary.  With ANSWER the run is a justify run.  Without
--seed the seed comes from the clock (the original's randomize) and is printed on
the Problem line; giving it back with --seed replays the run.

The original stops (suspend) when it finds an answer or gives up, and waits for
Go.  The run ends there, or with --keep-going continues as if Go were pressed,
until K codelets have run.  --max-codelets K is the original's breakpoint
(runtil K): the run stops after codelet K.  Without it there is no cap.
"Stopped:" says why the run ended: suspend, cap, or halt (the original's
report-error-and-halt).

--trace FILE also writes the run's JSON-lines trace (docs/trace-format.md), which
for the runs of tests/problems.txt equals tests/golden/.  --verbose turns on the
original's verbose mode: the model's vprintf output is printed too.

Exit codes, as the oracle's: 0 after a run, 2 for bad arguments, 1 if the run
raises an error (as the original does on some runs: anomalies_and_quirks.md).
"""
from __future__ import annotations

import sys
import traceback


def usage():
    sys.stderr.write("usage: python3 -m metacat INITIAL MODIFIED TARGET [ANSWER] [--seed N] "
                     "[--max-codelets K] [--keep-going] [--trace FILE] [--verbose]\n")
    sys.exit(2)


def parse_positive(s):
    """run.ss: parse-positive (string->number: an exact positive integer)"""
    from metacat import chez
    try:
        n = chez.string_to_number(s)
    except Exception:   # noqa: BLE001 - a malformed number is #f
        n = False
    if (n is not False and isinstance(n, int) and not isinstance(n, bool) and n > 0):
        return n
    return usage()


def parse_args(args):
    """run.ss: parse-args.  Returns (strings seed max-codelets keep-going? trace
    verbose?); the words that start with a letter are the strings."""
    strings, seed, max_, keep, trace, verbose = [], False, False, False, False, False
    i = 0
    while i < len(args):
        a = args[i]
        if a in ("--seed", "--max-codelets", "--trace"):
            if i + 1 >= len(args):
                usage()
            if a == "--seed":
                seed = parse_positive(args[i + 1])
            elif a == "--max-codelets":
                max_ = parse_positive(args[i + 1])
            else:
                trace = args[i + 1]
            i += 2
            continue
        if a == "--keep-going":
            keep = True
        elif a == "--verbose":
            verbose = True
        elif a and a[0].isalpha():
            strings.append(a)
        else:
            usage()
        i += 1
    if len(strings) not in (3, 4):
        usage()
    return strings, seed, max_, keep, trace, verbose


def condition_text(e):
    """The oracle's error handler's first line (prelude.ss: display-condition)
    for a Chez error; a Python error's type and message otherwise."""
    from metacat import chez
    if isinstance(e, chez.SchemeError):
        if e.who is None or e.who is False:
            return "Exception: " + str(e)
        return "Exception in " + str(e)
    return "%s: %s" % (type(e).__name__, e)


def main(argv=None):
    """python3 -m metacat, and the installed `metacat` command (pyproject.toml)"""
    sys.setrecursionlimit(max(sys.getrecursionlimit(), 10000))
    args = sys.argv[1:] if argv is None else argv
    strings, seed, max_codelets, keep_going, trace_file, verbose = parse_args(args)
    from metacat import headless, sugar, utilities
    headless.prepare()
    if seed is False:
        utilities.randomize()
        from metacat import chez
        seed = chez.random_seed()
    if sugar.valid_number_p(seed) is False:
        sys.stderr.write("metacat: the seed must be between 1 and 4294967295\n")
        sys.exit(2)
    port = open(trace_file, "w", encoding="utf-8", newline="\n") if trace_file else None
    try:
        headless.run_problem(strings, seed, max_codelets, keep_going, port, verbose)
    except Exception as e:   # noqa: BLE001 - the original's own errors (exit 1, like Chez)
        sys.stdout.flush()
        sys.stderr.write("Error: %s\n" % condition_text(e))
        traceback.print_exc()
        sys.exit(1)
    finally:
        if port is not None:
            port.close()
        sys.stdout.flush()
    return 0


if __name__ == "__main__":
    sys.exit(main())
