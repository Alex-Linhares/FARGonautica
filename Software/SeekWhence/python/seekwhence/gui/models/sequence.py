"""The sequence pane's model: the terms with their fate, the hypothesis's predicted
block as ghost cells, the guess exchange and the answer badges (PANES.md P1; G-014).
Qt-free.

The program is "presented sequence terms one at a time by the user" and, when its
hypothesis holds up, ventures a guess "by showing the user a synopsis of its model"
[M86 p. 7]: `I have a guess!`, `hypothesis: (Cycle 3 2 (3 7))`, the block `3 7 3 7`,
`enter no if wrong, ok if right ok`, `bye` [M86 p. 166]; a wrong guess, `(1 2)(1 2)(1 2)`
answered `nope`, sends the system on [M86 pp. 175-176]. The model keeps, from the
events alone (no snapshot is needed, every event it uses carries the form):

  - `terms`: one `TermCell` per term entered, with its `fate` under the hypothesis in
    force: `predicted` (the Hfilter found it to be the term predicted, `filtered` with
    `ok`), `refuting` (it refuted the hypothesis, `refuted` [M86 p. 168]) or
    `unfiltered` (not filtered yet, or entered before a hypothesis existed); a new
    hypothesis (`hypothesis`, `candidate`) clears the fates, which belonged to the old one;
  - `form`: the hypothesis in force, and `refuted`, whether it stands refuted and not yet
    rebuilt;
  - `ghosts()`: the block the program would show for the form now, `Hypothesis(form).
    guess_block(n, min_hits)` on a fresh object with its own box (never the live
    hypothesis; ENGINE-API.md section 2, rule 3; DECISION D-033), as `GhostCell`s with
    their hit (block) number, their position and whether they lie `beyond` the last term
    entered: the predicted continuation; `ghost_text()` prints it as the guess does;
  - `waiting`: the run is at a prompt, `please enter a term:` [M86 p. 164], from the
    `start` event (the console asks before any task runs) and each `prompt` event until
    the `term`;
  - `pending` and `badges`: the guess waiting for its answer and the answered guesses,
    each an `AnswerBadge` with the form, the block, the answer as given and whether it
    was accepted (DECISION D-053).
"""

import dataclasses

from seekwhence.forms import flatten, parse
from seekwhence.hypothesis import Hypothesis, block_text

__all__ = ["FATES", "AnswerBadge", "GhostCell", "SequenceModel", "TermCell"]

# The three fates of a term under the hypothesis in force (PANES.md P1).
FATES = ("unfiltered", "predicted", "refuting")

DEFAULT_MIN_HITS = 3


@dataclasses.dataclass(frozen=True)
class TermCell:
    """One term entered [M86 p. 164]: its position `n` (from 1), its `value`, its `fate`
    (one of `FATES`) and, once filtered, the term the hypothesis `predicted` at that
    position [M86 p. 166] (G-014)."""
    n: int
    value: int
    fate: str = "unfiltered"
    predicted: int | None = None


@dataclasses.dataclass(frozen=True)
class GhostCell:
    """One term of the block a fresh hypothesis gives (DECISION D-033): its `value`, the
    `hit` it belongs to (from 1; the block's groups, `(1 2)(1 2 3)(1 2 3 4)`
    [M86 p. 179]), its `position` in the sequence (from 1), whether it lies `beyond` the
    terms entered (the predicted continuation) and, when it does not, whether it
    `agrees` with the term at that position (G-014)."""
    value: int
    hit: int
    position: int
    beyond: bool
    agrees: bool | None


@dataclasses.dataclass(frozen=True)
class AnswerBadge:
    """One guess and its answer [M86 pp. 166, 175]: the `form`, the `block` as printed,
    the `answer` as given (`ok`, `nope`, ...; None while the guess waits) and whether it
    was `accepted` (DECISION D-053; None while it waits) (G-014)."""
    form: str
    block: str
    terms: tuple
    answer: str | None = None
    accepted: bool | None = None

    @property
    def verdict(self) -> str:
        """`ok` for an accepted guess, `no` for a refused one, `waiting` before the answer
        (DECISION D-053)."""
        if self.accepted is None:
            return "waiting"
        return "ok" if self.accepted else "no"


class SequenceModel:
    """An observer of a run's events that keeps what the sequence pane shows (see the
    module docstring; PANES.md P1; G-014). A `start` event begins a new run."""

    def __init__(self):
        self.reset()

    def reset(self) -> None:
        """Forget the run (Numbo GUI, `run_stats.py`'s `reset`)."""
        self.seed = None
        self.min_hits = DEFAULT_MIN_HITS
        self.terms: list = []
        self.form: str | None = None
        self.refuted = False
        self.waiting = False
        self.done = False
        self.pending: AnswerBadge | None = None
        self.badges: list = []
        self.events = 0
        self._ghosts_key = None
        self._ghosts = ()
        self._hits = ()

    # ---- the events ------------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply one event (the observer protocol, DECISION D-055) (G-014)."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self.seed = event.seed
            self.min_hits = _min_hits(event.config)
            self.waiting = True           # the console asks first [M86 p. 164]
        self.events += 1
        if kind == "term":
            self.terms.append(TermCell(event.n, event.term))
            self.waiting = False
        elif kind == "prompt":
            self.waiting = True
        elif kind in ("hypothesis", "candidate"):
            self.form = event.form
            self.refuted = False
            self.terms = [dataclasses.replace(cell, fate="unfiltered", predicted=None)
                          for cell in self.terms]
        elif kind == "filtered":
            self._set_fate(event.n, "predicted" if event.ok else "refuting", event.predicted)
        elif kind == "refuted":
            self._set_fate(event.n, "refuting", event.predicted)
            self.refuted = True
        elif kind == "guess":
            self.pending = AnswerBadge(event.form, event.block, tuple(event.terms))
        elif kind == "answered":
            pending = self.pending
            if pending is None:
                pending = AnswerBadge(self.form or "", "", ())
            self.badges.append(dataclasses.replace(pending, answer=event.answer,
                                                   accepted=event.accepted))
            self.pending = None
        elif kind in ("ended", "capped"):
            self.waiting = False
            self.done = kind == "ended"

    def _set_fate(self, n, fate, predicted):
        for i, cell in enumerate(self.terms):
            if cell.n == n:
                self.terms[i] = dataclasses.replace(cell, fate=fate, predicted=predicted)
                return

    # ---- the ghost cells (DECISION D-033) --------------------------------------------------

    def hits(self) -> tuple:
        """The hits of the block a fresh `Hypothesis(form)` gives for the terms entered
        (`guess_block`, DECISION D-033): `(3, 7, 3, 7)` [M86 p. 166]; empty without a
        hypothesis. Computed once per form and number of terms (G-014)."""
        key = (self.form, len(self.terms), self.min_hits)
        if key != self._ghosts_key:
            self._ghosts_key = key
            self._hits = _fresh_hits(self.form, len(self.terms), self.min_hits)
            self._ghosts = _ghost_cells(self._hits, [cell.value for cell in self.terms])
        return self._hits

    def ghosts(self) -> tuple:
        """The block's terms as `GhostCell`s, in order (G-014)."""
        self.hits()
        return self._ghosts

    def ghost_blocks(self) -> tuple:
        """The ghost cells grouped by hit, `((1 2) (1 2 3) (1 2 3 4))` [M86 p. 179]
        (G-014)."""
        blocks: list = []
        for cell in self.ghosts():
            if not blocks or blocks[-1][0].hit != cell.hit:
                blocks.append([cell])
            else:
                blocks[-1].append(cell)
        return tuple(tuple(block) for block in blocks)

    def ghost_text(self) -> str:
        """The block as the guess prints it, `3 7 3 7` [M86 p. 166], `(1 2)(1 2 3)(1 2 3 4)`
        [M86 p. 179] (`block_text`, DECISION D-033); empty without a hypothesis."""
        hits = self.hits()
        return block_text(list(hits)) if hits else ""

    def continuation(self) -> tuple:
        """The ghost cells beyond the last term entered: the predicted continuation
        (PANES.md P1; G-014)."""
        return tuple(cell for cell in self.ghosts() if cell.beyond)

    # ---- texts -----------------------------------------------------------------------------

    def term_values(self) -> tuple:
        """The terms entered, as values [M86 p. 169]."""
        return tuple(cell.value for cell in self.terms)

    def cell_tooltip(self, cell: TermCell) -> str:
        """What a term cell says when hovered: its position, value and fate (G-014)."""
        text = f"term {cell.n}: {cell.value}, {cell.fate}"
        if cell.fate == "predicted":
            text += f" (the hypothesis predicted {cell.predicted}) [M86 p. 166]"
        elif cell.fate == "refuting":
            text += f" (the hypothesis predicted {cell.predicted}) [M86 p. 168]"
        elif self.form is None:
            text += " (no hypothesis yet)"
        return text

    def ghost_tooltip(self, cell: GhostCell) -> str:
        """What a ghost cell says when hovered: `predicted by (Cycle 3 2 (3 7)), hit 2`
        (PANES.md P1; G-014)."""
        text = f"predicted by {self.form}, hit {cell.hit}: {cell.value} at position {cell.position}"
        if cell.beyond:
            return text + " (not entered yet)"
        return text + (" (agrees)" if cell.agrees else " (differs)")


def _min_hits(config) -> int:
    """`guess_min_hits` from the `start` event's config pairs (DECISION D-033)."""
    for name, value in config:
        if name == "guess_min_hits":
            return int(value)
    return DEFAULT_MIN_HITS


def _fresh_hits(form, n_entered, min_hits) -> tuple:
    """`Hypothesis(form).guess_block(n_entered, min_hits)` on a fresh object: the
    hypothesis's own box is never touched (ENGINE-API.md section 2, rule 3)."""
    if form is None:
        return ()
    try:
        parsed = parse(form)
    except (ValueError, TypeError):
        return ()
    hits = Hypothesis(parsed).guess_block(n_entered, min_hits)
    return tuple(_plain(hit) for hit in hits)


def _plain(value):
    if isinstance(value, (list, tuple)):
        return tuple(_plain(v) for v in value)
    return value


def _ghost_cells(hits, terms) -> tuple:
    cells = []
    position = 0
    for number, hit in enumerate(hits, 1):
        for value in flatten(hit):
            position += 1
            beyond = position > len(terms)
            agrees = None if beyond else (terms[position - 1] == value)
            cells.append(GhostCell(value, number, position, beyond, agrees))
    return tuple(cells)
