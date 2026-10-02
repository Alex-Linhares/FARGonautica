"""The GUI shell's Qt-free half (not 1987 source; loop0003): the run's inputs
and the stats panel's model.

Inputs: CHAPTER_PUZZLES, the chapter's 11 puzzles (lisp/src/RESULTS.md) as
(target b1 ... b5), and the parsers for what the controls hold as text.
They accept what the CLI (python -m numbo) accepts, any integers, and raise
InputError (a ValueError) with a message meant for the user.

RunStats is an observer of a run's events.  It keeps the problem, seed and
cap (from start), the event count, the iteration, x, temperature and rack
size (from iteration), the codelet last chosen, and the outcome (from the
RunEnded event).  The solution check needs the run's printed text, which
is not an event: finish(output) runs solution_checker.check_solution on it
once the run has ended.  A replayed run has no printed text: finish(None)
checks solution_text(end event), the "Done :" paragraphs that decompose
printed, rebuilt from the Solved event's decomposition (tested equal to the
printed text's check).  outcome_text() and check_text() say what the
CLI's two summary lines say ("solved, 45 iterations",
"valid: 114 = (6 x 20) - (7 - 1)").  verdict() is how the run ended, for
the canvas's banner, and names the 1987 code's two flaws when a run hits
them (the reactivate-cyto race, the kill-block gap).
"""

from typing import NamedTuple

from numbo import observe, solution_checker

__all__ = ["CHAPTER_PUZZLES", "InputError", "puzzle_label", "problem_text", "parse_problem",
           "parse_seed", "parse_max_iterations", "solution_text", "RunStats", "Verdict"]

CHAPTER_PUZZLES = (
    (114, 11, 20, 7, 1, 6),
    (87, 8, 3, 9, 10, 7),
    (31, 3, 5, 24, 3, 14),
    (25, 8, 5, 5, 11, 2),
    (102, 6, 17, 2, 4, 1),
    (146, 12, 2, 5, 7, 18),
    (6, 3, 3, 17, 11, 22),
    (11, 2, 5, 1, 25, 23),
    (116, 20, 2, 16, 14, 6),
    (127, 6, 4, 22, 5, 7),
    (41, 5, 16, 22, 25, 1),
)


class InputError(ValueError):
    """A control holds something that is not a valid input."""


def problem_text(problem):
    """(114, 11, 20, 7, 1, 6) -> "114 from 11 20 7 1 6"."""
    return f"{problem[0]} from {' '.join(map(str, problem[1:]))}"


def puzzle_label(n):
    """The chapter's puzzle N (1-11), as the puzzle list shows it."""
    return f"{n}: {problem_text(CHAPTER_PUZZLES[n - 1])}"


def _integer(word, what=""):
    try:
        return int(word)
    except ValueError:
        raise InputError(f"{what}{word!r} is not an integer") from None


def parse_problem(text):
    """A target and 5 bricks, separated by spaces or commas."""
    words = text.replace(",", " ").split()
    if len(words) != 6:
        raise InputError(f"a problem is a target and 5 bricks (6 integers), "
                         f"got {len(words)} numbers")
    return tuple(_integer(word) for word in words)


def parse_seed(text):
    return _integer(text.strip(), "seed: ")


def parse_max_iterations(text):
    """An integer, at least 1, or 'none' (no cap)."""
    text = text.strip()
    if text.lower() == "none":
        return None
    try:
        n = int(text)
    except ValueError:
        raise InputError(f"max iterations: {text!r} is not an integer or 'none'") from None
    if n < 1:
        raise InputError(f"max iterations must be at least 1, got {n}")
    return n


def solution_text(ended):
    """What a run that ended with the RunEnded event ENDED printed after
    "Done :", as decompose printed it (codelets.py); "" if it wasn't
    solved."""
    if not isinstance(ended, observe.Solved):
        return ""
    lines = ["Done :"]
    for s in ended.decomposition:
        lines.append(f"Operation {s.op} has been applied ")
        lines.append(f"to {s.a} ( {s.va}) and to {s.b} ( {s.vb})")
        lines.append(f"to get {s.result}")
    return "\n".join(lines) + "\n"


class RunStats:
    """The stats of the run whose events it observes (see the module
    docstring).  A start event resets it."""

    def __init__(self):
        self.reset()

    def reset(self):
        self.problem = None
        self.seed = None
        self.max_iterations = None
        self.events = 0
        self.iteration = None
        self.x = None
        self.temperature = None
        self.rack_total = None
        self.codelet = None
        self.ended = None        # the RunEnded event
        self.check = None        # (valid, reason, expression)

    @property
    def outcome(self):
        return self.ended.outcome if self.ended is not None else None

    @property
    def iterations(self):
        return self.ended.iterations if self.ended is not None else None

    def on_event(self, event):
        if isinstance(event, observe.RunStarted):
            self.reset()
            self.problem = tuple(event.problem)
            self.seed = event.seed
            self.max_iterations = event.max_iterations
        self.events += 1
        if isinstance(event, observe.IterationBegan):
            self.iteration = event.n
            self.x = event.x
            self.temperature = event.temperature
            self.rack_total = sum(count for _, count in event.rack)
        elif isinstance(event, (observe.CodeletChosen, observe.SetupChoose)):
            self.codelet = event.codelet
        elif isinstance(event, observe.RunEnded):
            self.ended = event

    def finish(self, output):
        """Check the solution printed in OUTPUT (the run's printed text), as
        the CLI does, if the run has ended.  OUTPUT None (a replay): check
        solution_text of the end event."""
        if self.ended is None or self.problem is None:
            return
        if output is None:
            output = solution_text(self.ended)
        self.check = solution_checker.check_solution(output, list(self.problem))

    def outcome_text(self):
        if self.ended is None:
            return ""
        text = f"{self.outcome}, {self.iterations} iterations"
        if isinstance(self.ended, observe.RunError):
            text += f": {self.ended.message}"
        return text

    def check_text(self):
        if self.check is None:
            return ""
        valid, reason, expression = self.check
        return f"valid: {expression}" if valid else f"invalid: {reason}"

    def verdict(self):
        """The run's Verdict, once it has ended (None before)."""
        e = self.ended
        if e is None:
            return None
        n = e.iterations
        if isinstance(e, observe.RunError):
            detail = e.message
            if any(sign in e.message for sign in RACE_SIGNS):
                detail += "\n" + RACE_NOTE
            return Verdict("error", f"Error after {n} iterations", detail)
        if isinstance(e, observe.Solved):
            if self.check is None:
                return Verdict("good", f"Solved in {n} iterations", "")
            valid, reason, expression = self.check
            if valid:
                return Verdict("good", f"Solved in {n} iterations: {expression}", "")
            detail = reason or ""
            if GAP_SIGN in detail:
                detail += "\n" + GAP_NOTE
            return Verdict("warn", f"Solved in {n} iterations, but the solution is invalid",
                           detail)
        if isinstance(e, observe.Capped):
            return Verdict("warn", f"Stopped at the cap, {n} iterations: not solved", "")
        return Verdict("warn", f"Gave up after {n} iterations", "")


class Verdict(NamedTuple):
    """How a run ended, for the canvas's banner.  LEVEL: "good" (a valid
    solution), "warn" (an invalid one, gave up, capped) or "error"."""
    level: str
    headline: str
    detail: str


# The two errors real runs reach, both from the 1987 code's reactivate-cyto
# race (lisp/src/PORTING_NOTES.md, item 10's notes).
RACE_SIGNS = ("does not handle the message", "is unbound")
RACE_NOTE = ("This is the 1987 code's reactivate-cyto race: reactivate-cyto ran before "
             "every brick was linked to the Pnet (about 1 run in 100; see "
             "lisp/src/PORTING_NOTES.md).")
GAP_SIGN = "is used but never derived"
GAP_NOTE = ("This is the 1987 code's kill-block gap: a killed block was left under an "
            "operation and decompose printed it (see lisp/src/PORTING_NOTES.md).")
