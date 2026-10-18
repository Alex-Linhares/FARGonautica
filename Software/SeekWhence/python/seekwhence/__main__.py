"""The command line of seekwhence: `python3 -m seekwhence` [M86 pp. 164-179] (DECISION D-069).

Without `--terms` the program is interactive, as in the transcripts: it prints `please
enter a term:` and reads a term, a console command (`show`, `show-hypothesis`, `show-seq`,
`show-parse`) or `bye` [M86 pp. 164-179] (DECISION D-054), and asks `enter no if wrong, ok
if right` when it has a guess [M86 p. 166] (DECISION D-053). With `--terms` it is scripted:
the terms (and any console commands among them) are fed on request, the `--answers` are
given in order and, when they run out, a guess is judged against the scripted terms
(DECISION D-033); at the end of the script, or when a guess is accepted, the run ends and
one `outcome:` line is printed. `--seed` fixes the run's only generator, so the same seed
gives the same output (DECISION D-069); `--trace` chooses the verbose trace of runs 1
and 3 or the terse one of run 2 (DECISION D-055); `--json FILE` writes every event as one
JSON object per line; `--max-steps` is the whole-run cap in tasks (DECISION D-011).

The exit status is 0 when a guess was accepted or the user quit (`bye`, or the end of the
input), 1 when the scripted terms ran out or the run was capped, and 2 for a usage error.
"""

import argparse
import dataclasses
import sys

from seekwhence.config import Config
from seekwhence.session import COMMANDS, Session, parse_line
from seekwhence.trace import JsonLines


def term_or_command(text: str) -> str:
    """An argument of `--terms`: a nonnegative integer or a console command
    (DECISION D-069)."""
    kind, _ = parse_line(text)
    if kind in ("term", "command"):
        return text.strip()
    raise argparse.ArgumentTypeError(
        f"not a term or a console command ({', '.join(COMMANDS)}): {text!r}")


def positive(text: str) -> int:
    """A cap: a positive integer (DECISION D-011)."""
    try:
        n = int(text)
    except ValueError:
        raise argparse.ArgumentTypeError(f"not an integer: {text!r}") from None
    if n < 1:
        raise argparse.ArgumentTypeError(f"must be at least 1, got {n}")
    return n


def nonnegative(text: str) -> int:
    """A seed: a nonnegative integer (`random.Random` draws the same numbers for a seed and
    its negative, so `seekwhence.rng.Rng` refuses negative seeds; DECISION D-069)."""
    try:
        n = int(text)
    except ValueError:
        raise argparse.ArgumentTypeError(f"not an integer: {text!r}") from None
    if n < 0:
        raise argparse.ArgumentTypeError(f"must be at least 0, got {n}")
    return n


def parser() -> argparse.ArgumentParser:
    """The command line's options (DECISION D-069)."""
    p = argparse.ArgumentParser(
        prog="python3 -m seekwhence",
        description="Seek-Whence (Meredith, 1986), reconstructed: shown the terms of an "
                    "integer sequence one at a time, it builds a structural hypothesis and "
                    "guesses the sequence. Without --terms it is interactive: type a term, "
                    "or show, show-hypothesis, show-seq, show-parse, or bye.",
        epilog="Exit status: 0 when a guess was accepted or the user quit, 1 when the "
               "scripted terms ran out or the run was capped, 2 for a usage error.")
    p.add_argument("--seed", type=nonnegative, default=0,
                   help="the seed of the run's only random generator (default 0)")
    p.add_argument("--terms", nargs="+", type=term_or_command, metavar="TERM",
                   help="run without a user: feed these terms (console commands allowed "
                        "among them) on request and stop when they run out")
    p.add_argument("--answers", nargs="+", default=(), metavar="ANSWER",
                   help="the answers to the guesses, in order (ok or no); when they run "
                        "out a guess is judged against the scripted terms, or asked")
    p.add_argument("--trace", choices=("verbose", "terse"), default="verbose",
                   help="verbose prints the 'doing task' lines as runs 1 and 3 do; terse "
                        "prints only the happenings, as run 2 (default verbose)")
    p.add_argument("--json", metavar="FILE",
                   help="write every event as one JSON object per line to FILE")
    p.add_argument("--max-steps", type=positive, metavar="N",
                   help=f"stop the run after N tasks (default {Config().max_tasks})")
    return p


def main(argv=None) -> int:
    """Run one session from the command line and return the exit status
    (DECISION D-069)."""
    args = parser().parse_args(argv)
    config = Config(verbose=args.trace == "verbose")
    if args.max_steps is not None:
        config = dataclasses.replace(config, max_tasks=args.max_steps)
    json_file = open(args.json, "w", encoding="utf-8") if args.json else None
    try:
        observers = [JsonLines(json_file)] if json_file else []
        session = Session(config, args.seed, out=sys.stdout, lines=args.terms,
                          answers=args.answers, observers=observers)
        outcome = session.run()
        if args.terms is not None:
            print(session.summary())
        sys.stdout.flush()
    finally:
        if json_file:
            json_file.close()
    return 0 if outcome in ("ok", "bye", "eof") else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
