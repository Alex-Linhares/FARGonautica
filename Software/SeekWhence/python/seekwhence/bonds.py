"""The tasks of sparking and bonding, and what a new term releases [M86 pp. 61-65].

"The identification and creation of useful gloms is the primary function of the tasks
operating at the cytoplasm level. To see how this is done, we must start at the bottom and
follow the process of 'pushing up' gloms" [M86 p. 63]. The bottom is here: the Sparkler (and
the transcripts' Sparkler-plus) make sparks between pairs of cyto-elements [M86 p. 63]; a
Tester takes a spark and tests the pair's bond-fields for sameness, successorship and
predecessorship [M86 p. 64]; a Bonder makes the bond, with a strength, and releases more
Sparklers and the Glom-scouts that begin the climb towards a glom [M86 pp. 64-65]. The
Glom-scout and everything above it are in `seekwhence.gloms`.

A new term is not a task: `new_term` records it in the `World`, makes its glint and lists it
in the platoplasm [M86 p. 61], and sets out the tasks the transcripts show after every term:
a Sparkler-plus for the glint against every element and itself (DECISION D-015), plain
Sparklers (DECISION D-016) and a Dissolver on the previous focus (DECISION D-024)
[M86 pp. 164, 170]. The `Dissolver` task lives here with them because the transcripts post it
on every term; the Burster and the rest of the terraced scan are in `seekwhence.gloms`.
Every task is a function `task(world, *args)` registered in `TASKS` by its printed name
(`seekwhence.run.new_world` installs them), and every random choice goes through the world's
`Rng` (DECISION D-025, D-058).
"""

from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence.cytoplasm import Bond, CytoElement, Spark


# ---- a new term ----------------------------------------------------------------------------

def new_term(world, term: int) -> cy.Glint:
    """Enter one term: record it (`World.enter_term`, which cleans the rack, DECISION D-010),
    make its glint and list it as a manifestation of its ideal atom [M86 p. 61], make the
    glint the active focus [M86 p. 47] and set out the tasks a term releases:

    - a Sparkler-plus for the new glint against every element of the cytoplasm at any
      level, itself included, with the urgency DECISION D-015 gives (`(glint2 glint1 10)`,
      `(glint2 glint2 2)` [M86 p. 164]; `(glint3 glint1 10)` with glint1 under glom1 under
      glom3, `(glint3 glom3 2)` against the pseudo-glom [M86 p. 172]);
    - `Config.sparklers_per_term` plain Sparklers, when there is a pair to draw from
      (DECISION D-016);
    - one Dissolver on the top-level element that holds the previous term's glint, or a
      bare Dissolver after the first term (`doing task Dissolver`, `doing task Dissolver on
      (glint1)` [M86 pp. 164, 170], `Dissolver on (glom3)` with glint2 under glom1 under the
      gnoth-glom glom3 [M86 p. 172], `Dissolver on (glom8)` and `on (glom10)`, the caps of
      glint3 and glint4, both run after the fifth term [M86 p. 176]; DECISION D-024 as
      amended in item 21). The element's focus ends with this term.

    Returns the glint.
    """
    world.enter_term(term)
    previous = cy.glints(world)[-1] if cy.glints(world) else None
    target = cy.top_of(previous) if previous is not None else None
    for focus in [x for x in cy.elements(world) if x.active]:
        focus.active = False
    glint = cy.make_glint(world, term)
    world.events.publish(events.GlintMade(glint.name, term, glint.position))
    others = cy.elements(world)
    for other in reversed(others):
        urgency = sparkler_plus_urgency(world, glint, other)
        world.post("Sparkler-plus", glint.name, other.name, urgency, urgency=urgency)
    if len(cy.elements(world, pseudo=False)) >= 2:
        for _ in range(world.config.sparklers_per_term):
            world.post("Sparkler")
    if target is not None:
        world.post("Dissolver", target.name)
    else:
        world.post("Dissolver")
    return glint


def sparkler_plus_urgency(world, a: CytoElement, b: CytoElement) -> int:
    """The number a Sparkler-plus is printed with, read as its urgency (DECISION D-015):
    the first of `Config.sparkler_plus_urgencies` (10) for two distinct real elements, the
    second (2) for a self-pair or when either is a pseudo-glom [M86 pp. 164, 172, 176]."""
    distinct, same = world.config.sparkler_plus_urgencies
    if a is b or a.pseudo or b.pseudo:
        return same
    return distinct


# ---- sparks ----------------------------------------------------------------------------------

def make_spark(world, a: CytoElement, b: CytoElement) -> Spark:
    """Make a spark between two elements, in the cytoplasm under the next `sparkN`, print it
    (`spark1 --- between glint1glint2` [M86 p. 164]) and set out the Testers it releases:
    "When a spark is created between two gloms, a horde of 'Testers' is placed on the
    taskrack" [M86 p. 64] (`Config.testers_per_spark`, DECISION D-016)."""
    spark = Spark(world.name("spark"), (a, b))
    world.cytoplasm.add(spark)
    world.events.publish(events.SparkMade(spark.name, a.name, b.name))
    for _ in range(world.config.testers_per_spark):
        world.post("Tester")
    return spark


def sparks(world) -> list:
    """The sparks now flying in the cytoplasm, in creation order [M86 p. 64]."""
    return [x for x in world.cytoplasm if isinstance(x, Spark)]


def sparkler(world) -> None:
    """The Sparkler task: "pulls those gloms at random from the cytoplasm and determines in a
    very cursory way that the two structures might be amenable to bonding. The Sparkler simply
    looks for gloms that are not subgloms of each other. It does not look for any common
    features" [M86 p. 63]. Two draws from the glints and real gloms at every level (only a
    glom and its own subglom are refused, p. 63; the transcripts spark elements nested under
    gloms, `spark12 --- between glom1glint3` with glom1 under the template glom [M86
    p. 165]), pseudo-gloms left out; the same element twice, or two nested in each other,
    makes no spark (DECISION D-025)."""
    candidates = cy.elements(world, pseudo=False)
    if len(candidates) < 2:
        world.stale("Sparkler", (), "fewer than two elements to spark")
        return
    a = world.rng.choice(candidates)
    b = world.rng.choice(candidates)
    if a is b:
        world.stale("Sparkler", (), f"drew {a.name} twice")
        return
    if cy.nested(a, b):
        world.stale("Sparkler", (), f"{a.name} and {b.name} are nested")
        return
    make_spark(world, a, b)


def sparkler_plus(world, new: str, other: str, number: int) -> None:
    """The Sparkler-plus task of the transcripts, `doing task Sparkler-plus on (glint2 glint1
    10)` [M86 p. 164]: a spark between a new element and a given other one, when both still
    exist, are distinct, neither is a subglom of the other and neither is a pseudo-glom
    (no spark follows `Sparkler-plus on (glint3 glom3 2)` or `(glint3 glom3 10)` against the
    pseudo-glom glom3 [M86 p. 172]; DECISION D-015). `number` is the urgency it was posted
    with, carried as its third argument because the transcripts print it."""
    del number
    a, b = world.find(new), world.find(other)
    if a is None or b is None:
        world.stale("Sparkler-plus", (new, other), "an element is gone")
        return
    if a is b:
        world.stale("Sparkler-plus", (new, other), "the same element twice")
        return
    if cy.nested(a, b):
        world.stale("Sparkler-plus", (new, other), "one is a subglom of the other")
        return
    if a.pseudo or b.pseudo:
        world.stale("Sparkler-plus", (new, other), "a pseudo-glom is not sparked")
        return
    make_spark(world, a, b)


# ---- testing for bonds --------------------------------------------------------------------

def bond_tests(a: CytoElement, b: CytoElement) -> list:
    """The bonding tests two elements pass, as (type, field) pairs, in a fixed order: "their
    bond-fields ... are intersected, and these fields' values are tested for similarities"
    [M86 p. 64]. DECISION D-018: on print-value, `Same` when the print-values are equal,
    `Succ`/`Pred` when the right element's terms are the successors/predecessors of the
    left's term by term (two glints; two gloms of equal span; a glint against the end of a
    glom that faces it: the glint `1` "might be bonded to the glom '(2 3)' by reason of
    adjacent successorship" [M86 p. 63]); on span, `Same` between two non-glint gloms of
    equal span (gloms `(1 2)` and `(1 2)` "might engender both 'same print-value' and 'same
    span' Bonders" [M86 p. 64]; a glint's span "is really unnecessary" [M86 p. 61])."""
    passed = []
    if a.print_value == b.print_value:
        passed.append(("Same", "print-value"))
    left, right = cy.left_to_right(a, b)
    left_values, right_values = _facing_values(left, right)
    if left_values is not None:
        if all(r == l + 1 for l, r in zip(left_values, right_values)):
            passed.append(("Succ", "print-value"))
        elif all(r == l - 1 for l, r in zip(left_values, right_values)):
            passed.append(("Pred", "print-value"))
    if not a.is_glint and not b.is_glint and a.span == b.span:
        passed.append(("Same", "span"))
    return passed


def _facing_values(left: CytoElement, right: CytoElement):
    """The terms compared for successorship, or (None, None) when the pair is not
    comparable (DECISION D-018)."""
    left_flat, right_flat = left.flat_values(), right.flat_values()
    if left.is_glint and right.is_glint:
        return left_flat, right_flat
    if left.is_glint:
        return left_flat, right_flat[:1]
    if right.is_glint:
        return left_flat[-1:], right_flat
    if left.span == right.span:
        return left_flat, right_flat
    return None, None


def tester(world) -> None:
    """The Tester task: "each Tester chooses some spark, not necessarily the one whose
    creation caused the tester's creation. The spark's members ... are tested to ascertain
    if they are currently in existence (recall that gloms are ephemeral). If both gloms
    exist, their bond-fields ... are intersected, and these fields' values are tested for
    similarities" [M86 p. 64]. The spark is drawn through the `Rng` and used up
    (DECISION D-058); a pair that is no longer bondable (one gone, or one now a subglom of the
    other [M86 p. 63]) makes nothing. "One Bonder task will be created for each bonding test
    passed by the two gloms" [M86 p. 64], with the arguments the transcripts print:
    `(Same print-value (remote) glint1 glint3)` [M86 p. 164]."""
    flying = sparks(world)
    if not flying:
        world.stale("Tester", (), "no spark to test")
        return
    spark = world.rng.choice(flying)
    world.cytoplasm.remove(spark.name)
    a, b = spark.members
    if not world.exists(a.name, b.name) or world.find(a.name) is not a \
            or world.find(b.name) is not b:
        world.stale("Tester", (spark.name,), "a member of the spark is gone")
        return
    if cy.nested(a, b):
        world.stale("Tester", (spark.name,), "one member is now a subglom of the other")
        return
    left, right = cy.left_to_right(a, b)
    modifier = cy.modifier(left, right)
    for type, field in bond_tests(left, right):
        world.post("Bonder", type, field, (modifier,), left.name, right.name)


# ---- bonding ---------------------------------------------------------------------------------

def bond_strength(world, type: str, modifier: str, field: str) -> int:
    """"Bond strength is derived from the bond type (e.g., 'sameness'), any bond modifiers
    (adjacent terms are more strongly bonded than non-adjacent ones, for example) and the
    glom characteristic (e.g., 'print-value') that is the subject of the bond"
    [M86 pp. 64-65]: the product of the three weights of DECISION D-019, rounded down."""
    config = world.config
    weight = (dict(config.bond_type_weights)[type] * dict(config.bond_modifier_weights)[modifier]
              * dict(config.bond_field_weights)[field])
    return int(weight)


def bonder(world, type: str, field: str, modifier, left: str, right: str) -> None:
    """The Bonder task: "When invoked, a Bonder 1) checks to see that both gloms still exist,
    and 2) checks to see that they are not already bonded in this way. If these conditions
    are satisfied, then the Bonder creates a Bond-class structure" [M86 p. 64], printed
    `bond1 --- between glint1glint3` [M86 p. 164]. "Creation of a bond causes the release of
    more Sparklers ... and causes the release of some Glom-scouts" [M86 p. 65]
    (`Config.sparklers_per_bond`, `Config.glom_scouts_per_bond`, DECISION D-016). The
    modifier arrives as the transcripts print it, in its own parentheses: `(adjacent)`."""
    modifier = modifier[0] if isinstance(modifier, (tuple, list)) else modifier
    args = (type, field, (modifier,), left, right)
    a, b = world.find(left), world.find(right)
    if a is None or b is None or not isinstance(a, CytoElement) or not isinstance(b, CytoElement):
        world.stale("Bonder", args, "an element is gone")
        return
    if a is b or cy.nested(a, b):
        world.stale("Bonder", args, "the elements cannot be bonded")
        return
    if cy.bonded(a, b, type, field) is not None:
        world.stale("Bonder", args, "already bonded in this way")
        return
    make_bond(world, type, field, modifier, a, b)


def make_bond(world, type: str, field: str, modifier: str, a: CytoElement,
              b: CytoElement) -> Bond:
    """Make the bond, in the cytoplasm under the next `bondN` and in both members'
    `bonds-in` [M86 pp. 61-62, 64], print it and release the Sparklers and Glom-scouts
    [M86 p. 65] (DECISION D-016)."""
    if type not in cy.BOND_TYPES or field not in cy.BOND_FIELDS or modifier not in cy.MODIFIERS:
        raise ValueError(f"not a bond: {type} {field} ({modifier})")
    left, right = cy.left_to_right(a, b)
    bond = Bond(world.name("bond"), type, field, modifier, (left, right),
                bond_strength(world, type, modifier, field))
    world.cytoplasm.add(bond)
    left.bonds_in.append(bond)
    right.bonds_in.append(bond)
    world.events.publish(events.BondMade(bond.name, type, field, modifier, left.name,
                                         right.name, bond.strength))
    for _ in range(world.config.sparklers_per_bond):
        world.post("Sparkler")
    for _ in range(world.config.glom_scouts_per_bond):
        world.post("Glom-scout")
    return bond


# ---- the Dissolver -------------------------------------------------------------------------

def dissolver(world, name: str | None = None) -> None:
    """The Dissolver task: "A Dissolver is a task that destroys a glom, leaving its
    highest-level subgloms to float independently in the cytoplasm" [M86 p. 70]. On a glint
    it does nothing ("Each glint is indestructible" [M86 p. 62]; `doing task Dissolver on
    (glint1)` [M86 p. 164]); without an argument it does nothing (`doing task Dissolver`
    after the first term [M86 p. 164]); on a glom that is gone it prints `glomN is not in
    cytoplasm` [M86 p. 172] (DECISION D-009). It dissolves its glom only when the glom is
    free, not dubbed, not a pseudo-glom and not an active focus (DECISION D-024)."""
    if name is None:
        return
    glom = world.find(name)
    if glom is None:
        world.say(f"{name} is not in cytoplasm")
        world.stale("Dissolver", (name,), "the glom is gone")
        return
    if not isinstance(glom, cy.Glom):
        return
    if glom.parent is not None or glom.dubbed or glom.pseudo or glom.active:
        world.stale("Dissolver", (name,), "the glom is protected")
        return
    cy.dissolve(world, glom)
    world.events.publish(events.GlomDissolved(name, tuple(sub.name for sub in glom.subgloms)))


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Sparkler": sparkler,
    "Sparkler-plus": sparkler_plus,
    "Tester": tester,
    "Bonder": bonder,
    "Dissolver": dissolver,
}
