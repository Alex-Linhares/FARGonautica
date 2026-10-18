"""Stage two begins: the Hfilter, validity and the guess [M86 pp. 39-40, 81-82, 98, 166,
168-169, 175-176].

Hypothesis filtering is "a process whereby new input terms are checked for conformity with
the reigning hypothesis. Should a new term not conform to the hypothesis, reformulation
begins" [M86 p. 181]. The task is printed `doing task Hfilter` and then `new term being
hypothesis-filtered through (Cycle 3 2 (3 7))` [M86 p. 166]. A term that agrees makes the
validity grow ("a number that grows as new, correctly-predicted terms are encountered"
[M86 p. 81]), joins the catchall gnoth ("The system stores hypothesis-confirming terms in a
catchall gnoth, a special gnoth that simply serves as a repository for non-troublesome
terms" [M86 p. 98]) and gets a top-down glom over the frame it belongs to (`top-down glom
glom4` [M86 p. 166]; DECISION D-063). "Consistent verification, in the form of terms which
support the hypothesis, will lead the system to a confirmation of the hypothesis and the
venturing of a public guess" [M86 p. 39]: `I have a guess!`, `hypothesis:` and the block of
terms (DECISIONS D-032, D-033), answered through an `Answerer` (DECISIONS D-053, D-064):
`ok` ends the run with `bye`, anything else sends the system on. A term that disagrees is
a refutation: "When an unexpected term is encountered, the system immediately sets the
hypothesis' validity to 0, releases sparks to encourage low-level activity, and places a
Gnoth-setter task on the taskrack" [M86 p. 98] (DECISION D-017); the Gnoth-setter and the
rest of reformulation belong to the later modules.

Nothing here draws from the `Rng`; the Sparklers a refutation releases draw when they run.
"""

from typing import Protocol

from seekwhence import cytoplasm as cy
from seekwhence import events, gnoths
from seekwhence.forms import flatten
from seekwhence.hypothesis import block_text

# The answer that accepts a guess: "enter no if wrong, ok if right ok" then "bye"
# [M86 p. 166]; anything else is a refusal (DECISION D-053).
OK = "ok"

# The type of the top-down glom the Hfilter puts over a confirmed frame (DECISION D-063).
FRAME_GLOM = ("frame",)


# ---- the user's answer [M86 pp. 166, 175-176] --------------------------------------------------

class Answerer(Protocol):
    """Whoever answers a guess: "enter no if wrong, ok if right" [M86 p. 166]
    (DECISION D-064). `answer` is given the world, the hypothesis's form as printed, the
    block as printed and the block's terms, and returns the user's words."""

    def answer(self, world, form: str, block: str, terms: tuple) -> str:
        """The answer to one guess (DECISION D-064)."""
        ...


class ScriptedAnswerer:
    """Answers from a script, for tests and the non-interactive mode (DECISIONS D-053,
    D-064): the `answers` are given in order; when they run out and the whole `terms` of
    the sequence are known, the guess is judged against them (`judge`, DECISION D-033);
    otherwise `default` is answered. `given` keeps every answer made."""

    def __init__(self, answers=(), terms=None, default: str = OK):
        self.answers = list(answers)
        self.terms = None if terms is None else tuple(terms)
        self.default = default
        self.given: list = []

    def answer(self, world, form: str, block: str, terms: tuple) -> str:
        """The next scripted answer, the judgement against the known sequence, or the
        default (DECISION D-064)."""
        del world, form, block
        if self.answers:
            answer = self.answers.pop(0)
        elif self.terms is not None:
            answer = OK if judge(terms, self.terms) else "no"
        else:
            answer = self.default
        self.given.append(answer)
        return answer


def judge(guess_terms, sequence) -> bool:
    """Whether a guess is right against a known sequence: its terms "agree with the
    scripted sequence as far as both go" (DECISION D-033). An empty guess is wrong."""
    guess_terms, sequence = tuple(guess_terms), tuple(sequence)
    n = min(len(guess_terms), len(sequence))
    return n > 0 and guess_terms[:n] == sequence[:n]


def accepted(answer: str) -> bool:
    """Whether an answer accepts the guess: `ok`, in any case and spacing, and nothing else
    (DECISION D-053)."""
    return answer.strip().lower() == OK


# ---- the catchall and the frame gloms (DECISION D-063) ---------------------------------------

def catchall(world) -> gnoths.Gnoth:
    """The catchall gnoth, made at the first confirming term if there is none ("a
    repository for non-troublesome terms" [M86 p. 98]; DECISIONS D-030, D-063): it takes
    in every glint the hypothesis postdicted beyond the listed gnoths ("in the catchall
    because they were predicted by the hypothesis" [M86 p. 98]), so that it is numbered
    after the gnoths of the parse, as `gnoth4` between `gnoth3` and `gnoth5` in run 3
    [M86 pp. 173, 176]."""
    found = gnoths.catchall(world)
    if found is None:
        covered = set()
        for gnoth in gnoths.gnoths(world):
            covered.update(range(gnoth.range[0], gnoth.range[1] + 1))
        trailing = [g for g in cy.glints(world) if g.position not in covered]
        found = gnoths.make_catchall(world, trailing)
    return found


def add_to_catchall(world, glint: cy.Glint) -> gnoths.Gnoth:
    """Put a filtered term's glint into the catchall, in sequence order [M86 p. 98]
    (DECISION D-063): confirming terms and the troublesome one alike ("the catchall gnoth
    might be holding '1 2 3 4' ... the '4' is the last term entered -- the troublesome one"
    [M86 p. 98])."""
    gnoth = catchall(world)
    if glint not in gnoth.glints:
        gnoth.glints.append(glint)
        gnoth.glints.sort(key=lambda g: g.position)
    tops = gnoth.collection
    gnoth.range = (tops[0].start_position, tops[-1].end_position)
    return gnoth


def frame_gloms(world) -> list:
    """The top-down gloms the Hfilter has put over confirmed frames, in creation order
    (DECISION D-063)."""
    return [x for x in world.cytoplasm
            if isinstance(x, cy.Glom) and x.pseudo and x.type == FRAME_GLOM]


def make_frame_glom(world, start: int, end: int) -> cy.Glom | None:
    """Put a top-down pseudo-glom over the terms of the current frame confirmed so far,
    `top-down glom glom4` [M86 p. 166] (DECISION D-063): the frame runs from `start`; the
    glom covers the free elements of the lineup from the frame's start, or from the first
    free element after whatever is not free, up to `end`, the new term. A frame glom
    already over a shorter prefix of the same frame is dissolved first, so one glom covers
    the prefix. Returns the glom, or None when nothing of the frame is free."""
    for old in frame_gloms(world):
        if start <= old.start_position and old.end_position < end:
            cy.dissolve(world, old)
    inside = [x for x in cy.lineup(world)
              if start <= x.start_position and x.end_position <= end]
    members: list = []
    for element in reversed(inside):
        if members and element.end_position + 1 != members[0].start_position:
            break
        if not members and element.end_position != end:
            continue
        members.insert(0, element)
    if not members:
        return None
    glom = cy.make_glom(world, FRAME_GLOM, members, pseudo=True)
    world.events.publish(events.TopDownGlom(glom.name, tuple(x.name for x in members),
                                            glom.print_value, True))
    return glom


def dissolve_previous_frame(world, hypothesis, position: int) -> list:
    """On a refutation, undo the hypothesis level's parse of the last frame before the one
    the troublesome term falls in: every top-level pseudo-glom holding a glint of that frame
    (a gnoth-glom or a frame glom) is dissolved, and what lies beneath floats free
    (DECISION D-063 as amended in item 21). This is what the transcripts show: before the
    Gnoth-setter runs, `glom3`, the gnoth-glom over `(1 2)` with the previous term under it,
    is gone and the bottom-up `glom1` is free again (`glom3 is not in cytoplasm`,
    `Sparkler-plus on (glom1 glint3 10)`, `bond4 --- between glom1glint3` [M86 pp. 172-173]);
    `glom8` (the cap over the `1` of terms 3-3) and `glom10` (the frame glom of term 4) are
    gone while `glom9`, the cap over terms 1-2, two frames back, is kept [M86 pp. 176, 178].
    Returns the names dissolved, outermost first."""
    start, _ = hypothesis.frame_bounds(position)
    if start <= 1:
        return []
    low, high = hypothesis.frame_bounds(start - 1)
    names = []
    for glom in list(world.cytoplasm):
        if (isinstance(glom, cy.Glom) and glom.pseudo and glom.parent is None
                and glom.start_position <= high and low <= glom.end_position
                and world.cytoplasm.find(glom.name) is glom):
            cy.dissolve(world, glom)
            names.append(glom.name)
    return names


def dissolve_frame_gloms(world) -> list:
    """Dissolve every frame glom: the refuted parse's top-down gloms go before the
    Gnoth-setter runs (`glom10 is not in cytoplasm` [M86 p. 176]; DECISION D-063). Returns
    the names dissolved."""
    names = []
    for glom in frame_gloms(world):
        if glom.parent is None:
            cy.dissolve(world, glom)
            names.append(glom.name)
    return names


# ---- the guess [M86 pp. 39, 166, 169, 175, 179] -----------------------------------------------

def make_guess(world) -> bool:
    """Venture the guess and take the answer: `I have a guess!`, `hypothesis: (Cycle 3 2
    (3 7))`, the block `3 7 3 7` [M86 p. 166] (DECISION D-033), then `enter no if wrong, ok
    if right` answered through the world's `Answerer` (DECISION D-064). `ok` ends the run
    (`bye` [M86 p. 166]; `world.done`); any other answer sets the validity to 0 and the
    system asks for the next term, as after `nope` [M86 pp. 175-176] (DECISIONS D-032,
    D-053). Returns whether the guess was accepted."""
    hypothesis = world.hypothesis
    hits = hypothesis.guess_block(len(world.terms), world.config.guess_min_hits)
    form = hypothesis.show_form()
    block = block_text(hits)
    terms = tuple(flatten(hits))
    world.events.publish(events.GuessMade(form, block, terms))
    answer = world.answerer.answer(world, form, block, terms)
    right = accepted(answer)
    world.events.publish(events.GuessAnswered(answer, right))
    if right:
        world.done = True
        world.events.publish(events.RunEnded("ok"))
    else:
        hypothesis.validity = 0
        world.post("Call-term")
    return right


# ---- the Hfilter [M86 pp. 98, 166, 168, 171-172, 175-176] ------------------------------------

def filter_one(world, n: int) -> bool:
    """Filter the term at position `n` (from 1) past the hypothesis and act on the outcome:
    the line `new term being hypothesis-filtered through (Cycle 3 2 (3 7))` [M86 p. 166];
    agreement grows the validity (DECISION D-032), puts the glint in the catchall with a
    frame glom (DECISION D-063) and ventures the guess when the validity reaches
    `Config.guess_validity`, else asks for the next term (DECISION D-012); disagreement is
    `refute`. Returns whether the term agreed."""
    hypothesis = world.hypothesis
    term = world.terms[n - 1]
    form = hypothesis.show_form()
    ok = hypothesis.check(term)
    world.events.publish(events.TermFiltered(n, term, form, ok, hypothesis.last_predicted))
    glint = cy.glints(world)[n - 1]
    if not ok:
        refute(world, glint, form)
        return False
    add_to_catchall(world, glint)
    start, end = hypothesis.frame_bounds(n)
    make_frame_glom(world, start, n)
    if hypothesis.validity >= world.config.guess_validity:
        make_guess(world)
    else:
        world.post("Call-term")
    return True


def refute(world, glint: cy.Glint, form: str) -> None:
    """A term the hypothesis did not predict: "the system immediately sets the hypothesis'
    validity to 0, releases sparks to encourage low-level activity, and places a
    Gnoth-setter task on the taskrack" [M86 p. 98]. The validity is already 0 (`check`);
    the terse trace prints `set-out -- validity: 0` [M86 p. 168] (DECISION D-055); the
    troublesome glint joins the catchall when one exists ("the '4' is the last term entered
    -- the troublesome one" [M86 p. 98]; none is made for it, DECISION D-063); the frame
    gloms and the caps over the previous frame are dissolved (`dissolve_previous_frame`,
    DECISION D-063); `Config.sparklers_on_refutation` plain Sparklers are set out (DECISION
    D-017); then the Gnoth-setter."""
    hypothesis = world.hypothesis
    hypothesis.validity = 0
    hypothesis.refuted = True
    world.events.publish(events.HypothesisRefuted(form, glint.position, glint.print_value,
                                                  hypothesis.last_predicted))
    if gnoths.catchall(world) is not None:
        add_to_catchall(world, glint)
    dissolve_frame_gloms(world)
    dissolve_previous_frame(world, hypothesis, glint.position)
    for _ in range(world.config.sparklers_on_refutation):
        world.post("Sparkler")
    world.post("Gnoth-setter")


def hfilter(world, *args) -> None:
    """The Hfilter task, `doing task Hfilter` [M86 p. 166]: filters every term entered since
    the hypothesis last predicted one, in order, stopping at a refutation or when a guess
    ends the run (DECISION D-063). It does nothing without a hypothesis, with one awaiting
    reformulation, after the run has ended, or when every term is filtered already
    (DECISION D-009)."""
    del args
    hypothesis = world.hypothesis
    if hypothesis is None:
        world.stale("Hfilter", (), "no hypothesis")
        return
    if world.done:
        world.stale("Hfilter", (), "the run has ended")
        return
    if hypothesis.refuted:
        world.stale("Hfilter", (), "the hypothesis awaits reformulation")
        return
    if hypothesis.covered >= len(world.terms):
        world.stale("Hfilter", (), "nothing to filter")
        return
    while hypothesis.covered < len(world.terms) and not world.done:
        if not filter_one(world, hypothesis.covered + 1):
            break


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Hfilter": hfilter,
}
