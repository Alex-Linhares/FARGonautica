"""The hypothesis object: form, box and validity [M86 pp. 79-85, 91, 125].

"The Hypothesizer process will take such a form and from it construct a Seek-Whence
hypothesis -- a data structure with several fields, capabilities, and functions" [M86 p. 81]:
"the hypothesis can display its form ... It can also predict the next term to be expected
following that form. In addition, it has a validity associated with it -- a number that grows
as new, correctly-predicted terms are encountered. The most crucial field, however, and the one
that supports the others, is simply called the hypothesis' box" [M86 p. 81]. "The box can be
reset to start again, asked to list a number of terms, or asked to predict the next term, given
the sequence's currently known terms" [M86 pp. 81-82]. This module holds that object, built on
`seekwhence.boxes`, with the postdiction test of a candidate [M86 p. 125] (DECISION D-034), the
guess block (DECISION D-033) and the freeze-dried form of a failed hypothesis [M86 p. 91].
The tasks that make, filter and refute hypotheses are in later modules.
"""

from dataclasses import dataclass

from seekwhence.boxes import BoxTree
from seekwhence.forms import Form, flatten, form_text, is_form, is_int, value_text


class Hypothesis:
    """A hypothesis: its `form`, its `box` (a `BoxTree`) and its `validity` [M86 p. 81].

    The box is hit one value at a time; the terms of a hit not yet predicted wait in
    `pending`, so that `next_term` predicts term by term [M86 p. 81] while `hit` and `frame`
    give whole values. `covered` counts the terms predicted so far from the start of the
    sequence. The box is scrapped and rebuilt rather than edited, "the current technique,
    which is to scrap the old box tree and make a new one" [M86 p. 85].
    """

    def __init__(self, form: Form, counters: dict | None = None):
        if not is_form(form):
            raise TypeError(f"not a form: {form!r}")
        self.form = form
        self.counters = counters
        self.validity = 0
        self.box = BoxTree(form, counters)
        self.pending: list = []
        self.covered = 0
        self.last_predicted = None
        self.refuted = False

    def show_form(self) -> str:
        """"the hypothesis can display its form" [M86 p. 81]: `(Cycle 3 2 (3 7))` as the
        transcripts print it after `hypothesis:` [M86 p. 166]."""
        return form_text(self.form)

    def hit(self):
        """The next value of the box: "produces the next run of terms predicted by the
        hypothesis" [M86 p. 81]. Terms still pending from an earlier hit are dropped."""
        self.pending = []
        value = self.box.hit()
        self.covered += len(flatten(value))
        return value

    def frame(self):
        """One frame of the hypothesis: one hit, or, for a Cycle, one full turn as a list
        (DECISION D-004: p. 81 groups a Cycle's hits one turn per parenthesis)."""
        if self.form.kind == "Cycle":
            return tuple(self.hit() for _ in range(len(self.form.actual)))
        return self.hit()

    def reset(self):
        """"The box can be reset to start again" [M86 p. 81]: the prediction starts from the
        first term; the validity is untouched."""
        self.box.reset()
        self.pending = []
        self.covered = 0

    def rebuild(self):
        """Scrap the box tree and make a new one from the form [M86 p. 85]; the new tree
        starts from the beginning."""
        self.box = BoxTree(self.form, self.counters)
        self.pending = []
        self.covered = 0

    def next_term(self) -> int:
        """"predict the next term to be expected following that form" [M86 p. 81]: the next
        term of the pending hit, or of a new hit. A hit that gives no terms (an empty group)
        is followed by another, up to a limit."""
        tries = 0
        while not self.pending:
            self.pending = flatten(self.box.hit())
            tries += 1
            if tries > 100 and not self.pending:
                raise ValueError(f"{self.show_form()} gives no terms")
        self.covered += 1
        return self.pending.pop(0)

    def check(self, term: int) -> bool:
        """Compare a new term with the predicted one: a correct prediction makes the
        validity grow by one (DECISION D-032: "a number that grows as new,
        correctly-predicted terms are encountered" [M86 p. 81]); a wrong one sets it to 0,
        as "the system immediately sets the hypothesis' validity to 0" [M86 p. 98] and the
        transcript's `set-out -- validity: 0` shows [M86 p. 168]. The term predicted is
        kept in `last_predicted`; `refuted` is the Hfilter's mark that reformulation is
        awaited."""
        self.last_predicted = self.next_term()
        if self.last_predicted == term:
            self.validity += 1
            return True
        self.validity = 0
        return False

    def frame_bounds(self, position: int) -> tuple:
        """The first and last position (from 1) of the frame of the hypothesis that holds
        the term at `position`: one hit, or for a Cycle one full turn (DECISION D-004), read
        off a fresh box so the hypothesis's own box is untouched; the frame's end may lie
        beyond the terms entered (DECISION D-063)."""
        if position < 1:
            raise ValueError(f"positions count from 1, got {position}")
        fresh = BoxTree(self.form)
        per_frame = len(self.form.actual) if self.form.kind == "Cycle" else 1
        start, frames = 1, 0
        while frames <= 10 * position + 10:
            n = sum(len(flatten(fresh.hit())) for _ in range(per_frame))
            end = start + n - 1
            if n and end >= position:
                return (start, end)
            start = end + 1
            frames += 1
        raise ValueError(f"{self.show_form()} gives no terms")

    def postdict(self, terms) -> bool:
        """Whether the form can "accurately 'postdict' the known terms of the sequence"
        [M86 p. 125] (DECISION D-034): a fresh box, fired from the first term, produces
        exactly `terms`, the last hit possibly cut short. On success the box is left after
        the known terms, so `next_term` predicts the term that follows them, "to predict the
        next term, given the sequence's currently known terms" [M86 p. 82]; on failure (or a
        form that cannot be hit, DECISION D-006) the box is reset and False returned."""
        self.rebuild()
        terms = list(terms)
        try:
            for term in terms:
                if self.next_term() != term:
                    self.reset()
                    return False
        except ValueError:
            self.reset()
            return False
        return True

    def list_terms(self, n: int) -> list:
        """"asked to list a number of terms" [M86 p. 82]: the first n terms of the pattern,
        from a fresh box, the hypothesis's own box untouched."""
        fresh = BoxTree(self.form)
        out, hits = [], 0
        while len(out) < n and hits <= 10 * n + 10:
            out.extend(flatten(fresh.hit()))
            hits += 1
        return out[:n]

    def guess_block(self, n_entered: int, min_hits: int = 3) -> list:
        """The hits the guess shows (DECISION D-033): a fresh box fired from the first term
        for at least `min_hits` hits and until every one of the `n_entered` terms is covered,
        the one rule that gives the four printed guesses, `3 7 3 7` [M86 p. 166],
        `3 7 3 3 7` [M86 p. 169], `(1 2)(1 2)(1 2)` [M86 p. 175] and
        `(1 2)(1 2 3)(1 2 3 4)` [M86 p. 179]. The hypothesis's own box is untouched."""
        fresh = BoxTree(self.form)
        hits, covered = [], 0
        while len(hits) < min_hits or covered < n_entered:
            value = fresh.hit()
            hits.append(value)
            covered += len(flatten(value))
            if len(hits) > 10 * (n_entered + min_hits) + 10:
                break
        return hits


def postdicts(form: Form, terms) -> bool:
    """Whether a fresh box of a form gives exactly the known terms [M86 p. 125]
    (DECISION D-034), without making a hypothesis of it."""
    return Hypothesis(form).postdict(terms)


def block_text(hits) -> str:
    """A guess's hits as the transcripts print them: a Cycle's single terms separated by
    spaces, `3 7 3 7` [M86 p. 166]; a group's lists run together, `(1 2)(1 2)(1 2)`
    [M86 p. 175], `(1 2)(1 2 3)(1 2 3 4)` [M86 p. 179]."""
    text = ""
    previous = None
    for value in hits:
        if previous is not None and (is_int(value) or is_int(previous)):
            text += " "
        text += value_text(value)
        previous = value
    return text


@dataclass(frozen=True)
class FreezeDried:
    """A failed hypothesis "freeze-dried": its form "is kept on a list of old hypotheses,
    along with the number of terms of the sequence it explained" [M86 p. 91]
    (DECISION D-035)."""

    form: Form
    terms_explained: int

    def __str__(self):
        return f"{form_text(self.form)} ({self.terms_explained} terms)"
