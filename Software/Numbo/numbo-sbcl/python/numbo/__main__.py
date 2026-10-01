"""python -m numbo TARGET B1 B2 B3 B4 B5 [options]: run one Numbo puzzle.

Not a port of a Lisp file: the command-line face of harness.run_config
(src/harness.lisp's run-config, in oracle mode) and solution_checker.
It prints what config prints, then two summary lines:

    outcome: solved, 45 iterations (seed 1)
    check: valid: 114 = (6 x 20) - (7 - 1)

The exit status is 0 when the run printed a valid solution, 1 when it did
not (gave up, capped, an error, or an invalid "Done :"), and 2 for a usage
error.
"""

import argparse
import os
import sys

from numbo import harness, solution_checker


class _Tee:
    """Write to STREAM (unless None) and keep a copy."""

    def __init__(self, stream):
        self.stream = stream
        self.parts = []

    def write(self, s):
        self.parts.append(s)
        if self.stream is not None:
            self.stream.write(s)
        return len(s)

    def flush(self):
        if self.stream is not None:
            self.stream.flush()

    def getvalue(self):
        return "".join(self.parts)


def _max_iterations(text):
    if text.lower() == "none":
        return None
    try:
        n = int(text)
    except ValueError:
        raise argparse.ArgumentTypeError(f"not an integer or 'none': {text!r}")
    if n < 1:
        raise argparse.ArgumentTypeError(f"must be at least 1, got {n}")
    return n


def parser():
    p = argparse.ArgumentParser(
        prog="python -m numbo",
        description="Run Numbo (Defays, 1987) on one puzzle: reach TARGET from the five "
                    "bricks with + - x.  Oracle mode: the same seed gives the same run as "
                    "the SBCL port in oracle mode.",
        epilog="Exit status: 0 for a valid solution, 1 otherwise, 2 for a usage error.")
    p.add_argument("target", type=int, metavar="TARGET")
    p.add_argument("bricks", type=int, nargs=5, metavar="B", help="the five bricks")
    p.add_argument("--seed", type=int, default=1, help="seed of the shared RNG (default 1)")
    p.add_argument("--max-iterations", type=_max_iterations, default=20000, metavar="N",
                   help="stop after N main-loop iterations, or 'none' (default 20000)")
    p.add_argument("--verbose", action="store_true",
                   help='set %%verbose%%: print the "About to post codelet" lines')
    p.add_argument("--trace", metavar="FILE",
                   help="write the JSON-lines trace (src/oracle.lisp's events) to FILE")
    p.add_argument("--rng-events", action="store_true", help="add the RNG draws to the trace")
    p.add_argument("--quiet", action="store_true",
                   help="print only the two summary lines")
    return p


def main(argv=None):
    args = parser().parse_args(argv)
    problem = [args.target, *args.bricks]
    out = _Tee(None if args.quiet else sys.stdout)
    # An error's iteration count comes from the trace (as oracle-run-config
    # does), so there is always one, to os.devnull when no file is asked for.
    trace = args.trace if args.trace is not None else os.devnull
    result = harness.run_config(problem, seed=args.seed, max_iterations=args.max_iterations,
                                verbose=args.verbose, trace=trace,
                                rng_events=args.rng_events, out=out)
    text = out.getvalue()
    if not args.quiet and text and not text.endswith("\n"):
        sys.stdout.write("\n")
    line = f"outcome: {result['outcome']}, {result['iterations']} iterations (seed {args.seed})"
    if result["outcome"] == "error":
        line += f": {result['error']}"
    print(line)
    valid, reason, expression = solution_checker.check_solution(text, problem)
    print(f"check: valid: {expression}" if valid else f"check: invalid: {reason}")
    sys.stdout.flush()
    return 0 if valid else 1


if __name__ == "__main__":
    sys.exit(main())
