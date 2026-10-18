"""The Gnoth-caster, the Slip-scout and the blast: casting gnoths as forms of the reigning
class, generalising the casts, testing the candidate by postdiction, and what follows a
failure [M86 pp. 39, 91-92, 124-126, 168-169, 174, 178].

"When invoked, a Gnoth-caster attempts to describe each gnoth in terms of the reigning
class" [M86 p. 124]. "If all the gnoths can be cast, or if all but the last can and it shows
promise, the Gnoth-caster then attempts to create a more general form common to all the
castings" [M86 p. 125]: `(3 4) (3 4) (3)` gives `(S-group 3 2)`, `(1 2) (1 2 3)` gives
`(S-group 1 (Countup 2))` [M86 p. 125]; the transcripts print `casts: ((S-group 1 2)
(S-group 1 1))`, `(S-group 1 2)`, `new hypoth candidate (S-group 1 2)` [M86 p. 174] and, under
Cycle, `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3))) (Cycle 3 3 (3 7 3))`
[M86 p. 168]. "The casting form returned, if any, now becomes a hypothesis candidate. A
'box', or predictive model, is made for it and is tested to see whether it can accurately
'postdict' the known terms of the sequence" [M86 p. 125]; the old hypotheses "serve as a
check against cycling" [M86 p. 91]. "If the Gnoth-caster is unable to cast all the gnoths in
terms of the reigning class, or if it cannot generalize the casts to come up with a
candidate, or if the candidate fails to postdict the sequence properly, the reformulation
effort has failed. In each such case, a 'Slip-scout' process is placed on the taskrack"
[M86 p. 126]; "Slip-scouts are only skeletally implemented in the current system" [M86 p. 126].
After repeated failure the system can "blast away all gnoths and gloms, leaving only the
glints and their bonds to push up an inspiration" [M86 p. 92], "a return to the term level,
with all perceived groupings eradicated but with accumulated knowledge of term samenesses and
other primitive relations maintained" [M86 p. 39].

The decisions: how one gnoth is cast and the casts generalised (DECISION D-048), casting
under a fence class (DECISION D-037), the freeze-dried repeat check (DECISION D-035), the
Slip-scout's one effect (DECISION D-049) and what "no progress for quite some time" measures
and what the blast does (DECISION D-050). Nothing here draws from the `Rng`: the casting is
"quite mechanical" [M86 p. 125]. The answer step is a plain function that can be tested
without a run (`cast_gnoths`, `generalise_casts`), the candidate is checked against the known
terms before it is adopted, and a failure is a source of pressure (the Slip-scout, then the
blast), not the end of the run; the task checks what it needs, then builds.
"""

from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence import gnoths as gn
from seekwhence import platoplasm as pl
from seekwhence import slipping
from seekwhence.forms import GROUP_TYPES, REPEATER_TYPES, Form, flatten, form_text, is_form
from seekwhence.gnoths import generalise
from seekwhence.hypothesis import Hypothesis

# The classes whose casts are concatenated rather than generalised (DECISION D-037).
FENCE_TYPES = ("Cycle", "Tuple")


# ---- casting one gnoth [M86 p. 124] (DECISIONS D-048, D-037) ------------------------------------

def cast_terms(kind: str, terms) -> list:
    """The casts of one gnoth's terms "in terms of the reigning class" [M86 p. 124], as a
    list: one form for a group class (`(S-group 1 2)` for `1 2` [M86 p. 174], when the terms
    pass the class's process, length 1 included: `(S-group 1 1)` for `1` [M86 p. 174]), one `(Y-group
    first length (terms))` for a symmetric run, one bare value for a repeater class, or
    nothing when the gnoth cannot be cast (DECISION D-048); under a fence class, one
    one-element form per term, `(Cycle 3 1 (3))` [M86 p. 168] (DECISION D-037)."""
    terms = tuple(terms)
    if not terms:
        return []
    if kind in GROUP_TYPES:
        if pl.PROCESSES[kind](terms):
            return [Form(kind, terms[0], len(terms))]
        return []
    if kind == "Y-group":
        if pl.is_y_group(terms):
            return [Form("Y-group", terms[0], len(terms), terms)]
        return []
    if kind in REPEATER_TYPES:
        return [terms[0]] if len(terms) == 1 else []
    if kind in FENCE_TYPES:
        return [Form(kind, term, 1, (term,)) for term in terms]
    raise ValueError(f"not an ideal type: {kind!r}")


def promise_frame(head: Form, k: int) -> tuple:
    """The terms of the k-th frame (from 0) of a form, flattened: what the last gnoth's terms
    must begin for the gnoth to "show promise" [M86 p. 125] (DECISION D-048); an empty tuple
    for a form that cannot be hit."""
    try:
        hypothesis = Hypothesis(head)
        frame = ()
        for _ in range(k + 1):
            frame = tuple(flatten(hypothesis.frame()))
        return frame
    except ValueError:
        return ()


# ---- generalising the casts [M86 p. 125] (DECISIONS D-048, D-037) ------------------------------

def generalise_casts(kind: str, casts: list):
    """"a more general form common to all the castings" [M86 p. 125], or None: parameter by
    parameter over the casts (`generalise`: equal values stay, values counting up by one
    become `(Countup first)`): `(S-group 1 2) (S-group 1 3)` gives `(S-group 1 (Countup 2))`
    [M86 pp. 125, 178]; a Y-group's elements position by position when the casts have one
    length; repeater casts to `(Constant v)` or `(Countup first)`; fence-class casts are
    concatenated into one form over all the terms, `(Cycle 3 3 (3 7 3))` [M86 p. 168]
    (DECISION D-037). A cast of None, or no casts, gives None (DECISION D-048)."""
    casts = list(casts)
    if not casts or any(c is None for c in casts):
        return None
    if kind in FENCE_TYPES:
        terms = tuple(flatten([c.actual for c in casts]))
        return Form(kind, terms[0], len(terms), terms) if terms else None
    if kind in REPEATER_TYPES:
        if not all(isinstance(c, int) for c in casts):
            return None
        value = generalise(casts)
        if value is None:
            return None
        return value if is_form(value) else Form("Constant", value)
    if not all(is_form(c) and c.kind == kind for c in casts):
        return None
    start = generalise(c.start for c in casts)
    length = generalise(c.length for c in casts)
    if start is None or length is None:
        return None
    if kind in GROUP_TYPES:
        return Form(kind, start, length)
    if len({len(c.actual) for c in casts}) != 1:
        return None
    elements = tuple(generalise(c.actual[i] for c in casts) for i in range(len(casts[0].actual)))
    if any(e is None for e in elements) or not isinstance(length, int):
        return None
    return Form("Y-group", start, length, elements)


class Casting:
    """The outcome of casting the parse [M86 pp. 124-125]: the reigning `kind`, the `gnoths`
    cast in order, their `casts` (one list per gnoth; empty when it could not be cast),
    `form`, the generalised form or None, whether the `last` gnoth was left out of the
    generalisation as the one that "shows promise" [M86 p. 125] (under a fence class it is
    not cast at all, DECISION D-037), and the `reason` when no form came out
    (DECISION D-048)."""

    def __init__(self, kind: str, gnoths: list):
        self.kind = kind
        self.gnoths = list(gnoths)
        self.casts: list = []
        self.form = None
        self.last = False
        self.reason: str | None = None

    @property
    def cast_list(self) -> list:
        """Every cast in order, as the `casts:` line prints them [M86 pp. 168, 174]
        (DECISION D-048)."""
        return [c for casts in self.casts for c in casts]

    def casts_text(self) -> str:
        """The `casts:` line's list, `((S-group 1 2) (S-group 1 1))` [M86 p. 174]; a gnoth
        that could not be cast prints as `nil` (DECISION D-048)."""
        parts = []
        for casts in self.casts:
            if not casts:
                parts.append("nil")
            parts.extend(form_text(c) if is_form(c) else str(c) for c in casts)
        return "(" + " ".join(parts) + ")"


def cast_gnoths(kind: str, gnoths: list) -> Casting:
    """Cast every gnoth of the parse under `kind` and generalise [M86 pp. 124-125]: all the
    gnoths when every one can be cast and the casts generalise; else all but the last when
    those generalise and the last "shows promise" [M86 p. 125], its own cast staying in the
    printed list (`casts: ((S-group 1 2) (S-group 1 1))` then `(S-group 1 2)` [M86 p. 174];
    DECISION D-048). Under a fence class the promise check comes first, since a
    concatenation always "generalises": `(3 7) (3) (3)` under Cycle casts the first two
    gnoths' terms and leaves the last uncast, whose `3` begins `3 7 3`, so that three casts
    are printed [M86 p. 168] (DECISION D-037). Returns a `Casting`."""
    casting = Casting(kind, gnoths)
    casting.casts = [cast_terms(kind, g.terms) for g in casting.gnoths]
    if not casting.gnoths:
        casting.reason = "no gnoths"
        return casting
    if kind in FENCE_TYPES:
        return _cast_fence(casting)
    all_casts = casting.cast_list
    if all(casting.casts):
        casting.form = generalise_casts(kind, all_casts)
        if casting.form is not None:
            return casting
    if len(casting.gnoths) > 1 and all(casting.casts[:-1]):
        head = generalise_casts(kind, [c for casts in casting.casts[:-1] for c in casts])
        if head is not None:
            frame = promise_frame(head, len(casting.gnoths) - 1)
            last_terms = tuple(casting.gnoths[-1].terms)
            if last_terms and frame[:len(last_terms)] == last_terms:
                casting.form = head
                casting.last = True
                return casting
    if not all(casting.casts):
        casting.reason = "a gnoth cannot be cast"
    else:
        casting.reason = "the casts do not generalise"
    return casting


def _cast_fence(casting: Casting) -> Casting:
    gnoths = casting.gnoths
    head_terms = tuple(flatten([g.terms for g in gnoths[:-1]]))
    last_terms = tuple(gnoths[-1].terms)
    if len(gnoths) > 1 and last_terms and head_terms[:len(last_terms)] == last_terms:
        casting.casts = casting.casts[:-1]
        casting.last = True
    if not all(casting.casts):
        casting.reason = "a gnoth cannot be cast"
        return casting
    casting.form = generalise_casts(casting.kind, casting.cast_list)
    if casting.form is None:
        casting.reason = "the casts do not generalise"
    return casting


# ---- the candidate [M86 pp. 91, 125] (DECISIONS D-034, D-035) ---------------------------------

def repeats_freeze_dried(world, form: Form) -> bool:
    """Whether a candidate repeats a freeze-dried hypothesis that "explained" at least as many
    terms as are known now [M86 p. 91] (DECISION D-035): then it would fail again at the same
    term, which is the cycling the old hypotheses "serve as a check against" [M86 p. 91]; a
    repeat that could explain more is allowed. Off when
    `Config.reject_freeze_dried_repeats` is False."""
    if not world.config.reject_freeze_dried_repeats:
        return False
    return any(old.form == form and old.terms_explained >= len(world.terms)
               for old in world.freeze_dried)


def record_casts(casting: Casting) -> None:
    """Write each gnoth's cast into its `form` field: after the Gnoth-caster, `show` prints
    `form: (S-group 1 2)` and `form: (S-group 1 1)` for the gnoths cast `((S-group 1 2)
    (S-group 1 1))` [M86 pp. 174-175] and `form: (S-group 1 3)` for the gnoth cast
    `(S-group 1 3)` [M86 pp. 178-179] (DECISION D-048 as amended in item 21). A gnoth with
    one cast gets it; a gnoth with several (a fence class casts every term, D-037) or none
    keeps its field."""
    for gnoth, casts in zip(casting.gnoths, casting.casts):
        if len(casts) == 1:
            gnoth.form = casts[0]


def adopt_candidate(world, form: Form) -> Hypothesis:
    """Make the candidate "the new hypothesis for the system" [M86 p. 125]: a `Hypothesis`
    whose box is left after the known terms (`postdict`, DECISION D-034), its validity 0 (the
    postdicted terms are not confirmations); printed `new hypoth candidate (S-group 1 2)`
    [M86 p. 174]; then "the system typically calls for the next term in order to test its
    new hypothesis" [M86 p. 125]: a Call-term is set out (DECISION D-012). The failure count
    of the episode is reset (DECISION D-050). The gnoths keep their frames and marks (the
    `show` of p. 175 prints the Gnoth-setter's marks unchanged [M86 p. 175])."""
    hypothesis = Hypothesis(form, world.counters)
    hypothesis.postdict(world.terms)
    world.hypothesis = hypothesis
    world.cast_failures = 0
    world.events.publish(events.CandidateMade(hypothesis.show_form(), world.reigning))
    world.post("Call-term")
    return hypothesis


# ---- failure: the Slip-scout and the blast [M86 pp. 39, 92, 126] (DECISIONS D-049, D-050) ------

def fail(world, reason: str) -> None:
    """"the reformulation effort has failed. In each such case, a 'Slip-scout' process is
    placed on the taskrack" [M86 p. 126]; the failures of one episode are counted, and at
    the `Config.max_cast_failures`th the blast follows instead (DECISION D-050)."""
    world.cast_failures += 1
    blasting = world.cast_failures >= world.config.max_cast_failures
    world.events.publish(events.CastFailed(reason, world.cast_failures, blasting))
    if blasting:
        blast(world, reason)
    else:
        world.post("Slip-scout")


def blast(world, reason: str) -> None:
    """"blast away all gnoths and gloms, leaving only the glints and their bonds to push up an
    inspiration" [M86 p. 92]: every gnoth is destroyed with its gnoth-glom, every glom in the
    cytoplasm is burst (bonds attached to a destroyed glom go with it, DECISION D-059; bonds
    between glints stay, "accumulated knowledge of term samenesses and other primitive
    relations maintained" [M86 p. 39]), the reigning class, the threshold, the failure count
    and the template are forgotten, the waiting reformulation tasks removed (DECISION
    D-010), and stage one resumes: the surviving bonds release what a new bond releases,
    Sparklers and Glom-scouts [M86 p. 65] (DECISION D-050). Scrapping, "a return to the term
    level" [M86 p. 39], is this same action. The reconstruction prints its own line for it,
    `blasting away all gnoths and gloms` (DECISION D-050)."""
    gloms_gone = [x.name for x in world.cytoplasm if isinstance(x, cy.Glom)]
    gnoths_gone = []
    for gnoth in [g for g in world.socratoplasm if isinstance(g, gn.Gnoth)]:
        gn.destroy_gnoth(world, gnoth)
        gnoths_gone.append(gnoth.name)
    for element in cy.lineup(world):
        if not element.is_glint:
            cy.burst(world, element)
    world.hypothesis = None
    world.reigning = None
    world.threshold = None
    world.template = None
    world.cast_failures = 0
    world.clean("reform")
    survivors = slipping.bonds(world)
    world.events.publish(events.Blasted(reason, tuple(gnoths_gone), tuple(gloms_gone),
                                        tuple(b.name for b in survivors)))
    for _ in survivors:
        for _ in range(world.config.sparklers_per_bond):
            world.post("Sparkler")
        for _ in range(world.config.glom_scouts_per_bond):
            world.post("Glom-scout")


def slip_scout(world, *args) -> None:
    """The Slip-scout task [M86 p. 126], "only skeletally implemented": it does the one thing
    the thesis states as fact, "the probability of a class change will increase"
    [M86 p. 126]: the slip-or-stay check again with the staying pressure multiplied by
    `Config.slip_scout_factor`, printed as a `slip-check` (a strict class slips at once,
    DECISION D-040), and then a Reformulator, so that the episode goes on under the class
    that reigns after it (DECISION D-049). The detailed study of bond types and interleavings
    is described as an intention [M86 p. 126] and is not built. Nothing is done without a
    reigning class or once a hypothesis exists (DECISION D-009)."""
    del args
    if world.reigning is None:
        world.stale("Slip-scout", (), "no reigning class")
        return
    if world.hypothesis is not None:
        world.stale("Slip-scout", (), "a hypothesis exists")
        return
    old = world.reigning
    check = slipping.slip_or_stay(world, world.config.slip_scout_factor)
    world.events.publish(events.SlipScouted(old, world.reigning,
                                            world.config.slip_scout_factor,
                                            None if check is None else check.stayval))
    world.post("Reformulator")


# ---- the Gnoth-caster task [M86 pp. 124-126] -----------------------------------------------------

def gnoth_caster(world, *args) -> None:
    """The Gnoth-caster task, `doing task Gnoth-caster` [M86 p. 174]: casts every gnoth of the
    parse under the reigning class and generalises (`cast_gnoths`, printed `casts: (...)`
    and the form [M86 pp. 168, 174, 178]); the form "now becomes a hypothesis candidate"
    and must postdict the known terms [M86 p. 125] (DECISION D-034) and not repeat a
    freeze-dried hypothesis (DECISION D-035); then it is adopted (`adopt_candidate`) or the
    effort has failed (`fail`: a Slip-scout, or the blast) [M86 p. 126]. Nothing is done
    without a reigning class or once a hypothesis exists (DECISION D-009)."""
    del args
    kind = world.reigning
    if kind is None:
        world.stale("Gnoth-caster", (), "no reigning class")
        return
    if world.hypothesis is not None:
        world.stale("Gnoth-caster", (), "a hypothesis exists")
        return
    casting = cast_gnoths(kind, gn.gnoths(world))
    record_casts(casting)
    form = casting.form
    world.events.publish(events.CastsMade(
        kind, tuple(g.name for g in casting.gnoths), casting.casts_text(),
        None if form is None else form_text(form), casting.last, casting.reason))
    if form is None:
        fail(world, casting.reason)
        return
    if repeats_freeze_dried(world, form):
        fail(world, f"{form_text(form)} repeats a freeze-dried hypothesis")
        return
    if not Hypothesis(form).postdict(world.terms):
        fail(world, f"{form_text(form)} does not postdict the terms")
        return
    adopt_candidate(world, form)


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Gnoth-caster": gnoth_caster,
    "Slip-scout": slip_scout,
}
