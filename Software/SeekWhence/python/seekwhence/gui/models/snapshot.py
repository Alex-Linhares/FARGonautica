"""A read-only snapshot of the `World`: frozen dataclasses of plain data, one section per
level of Meredith's world, "three levels, with two potential intermediating structures --
the hypothesis and the template" [M86 p. 43] (Figure 1 [M86 p. 44]) (G-003).

The events are made for the trace, and several changes of state are silent: a spark used
up by a Tester, the bonds and sparks that go with a destroyed glom, a gnoth the
Gnoth-setter destroys, the validity and position of the hypothesis between refutations,
the box tree, the gnoths' bond lists, the reigning class and the threshold between the
events that print them (ENGINE-API.md section 2). So the GUI never rebuilds the state from
the events: its tap reads the `World` at task boundaries and keeps a `Snapshot`
(`seekwhence.gui.models.tap`), and every pane model draws from the snapshot in force.

The engine contract (TASK.md rule 2; ENGINE-API.md section 2) is followed to the letter:
`snapshot` reads attributes and calls only the pure methods `show_form`, `type_text`,
`show_line`, `structure_text`, `show_lines`, `gnoths.gnoths`, `gnoths.parse_text`,
`Taskrack.count_by_type` and `weights`; it never calls anything that hits a box or draws
from the `Rng`; it copies everything into tuples, strings, numbers and booleans and keeps
no engine object. Predictions are not in the snapshot: a pane that needs them builds a
fresh `Hypothesis(form)` of its own (rule 3). A section that did not change since the
previous snapshot is the previous snapshot's own object (`previous=`), and the records of
the structures the engine never changes once made (tasks, sparks, bonds, freeze-dried
hypotheses, the ideal types' link tables) are kept in a per-run `SnapshotCache` under
plain keys (names, numbers), so that a snapshot costs about a quarter of a millisecond.
That is still many times a task's cost, so the tap takes one at every task boundary only
while the run is short, and keyframes after (`seekwhence.gui.models.tap`; G-003).
"""

from dataclasses import dataclass, fields

from seekwhence import cytoplasm as cy
from seekwhence import gnoths as gn
from seekwhence.forms import form_text, is_form, value_text
from seekwhence.trace import item_text

__all__ = [
    "TaskRecord", "RackSnapshot", "GlintRecord", "SparkRecord", "BondRecord", "GlomRecord",
    "TemplateRecord", "GnothRecord", "BoxRecord", "PstrucRecord", "HypothesisRecord",
    "tree_records",
    "FreezeDriedRecord", "ReformulationRecord", "IdealTypeRecord", "AtomRecord",
    "PlatoplasmSnapshot", "Snapshot", "SnapshotCache", "snapshot",
]


# ---- the taskrack [M86 pp. 41-42] -------------------------------------------------------------

@dataclass(frozen=True)
class TaskRecord:
    """One task waiting on the rack [M86 p. 41] (`seekwhence.taskrack.Task`): its
    `number`, `type`, `args` as the trace prints each argument (`glint1`, `10`, `(Cycle 3
    2 (3 7))`), `urgency`, `arena` (DECISION D-057), `created_at` (the clock), `at_term`
    (terms entered then) and its `weight` in the draw, urgency to the `urgency_power`
    (DECISION D-007)."""
    number: int
    type: str
    args: tuple
    urgency: int
    arena: str
    created_at: int
    at_term: int
    weight: int


@dataclass(frozen=True)
class RackSnapshot:
    """The waiting tasks in posting order, the counts by type in order of first appearance
    (`Taskrack.count_by_type`) and the total weight (`Taskrack.weights`) [M86 pp. 41-42].
    At a `chosen` event the chosen task is already off the rack (`Taskrack.choose` pops
    it before `TaskChosen` is published), so the rack before the draw is this rack plus
    the chosen task (G-007)."""
    tasks: tuple
    counts: tuple
    total_weight: int


# ---- the cytoplasm [M86 pp. 60-65] ----------------------------------------------------------

@dataclass(frozen=True)
class GlintRecord:
    """A glint's fields [M86 p. 61]: `name`, `value` (its print-value), `position`,
    `left_nbr` and `right_nbr` (names or None), `parent` (the glom it is a subglom of, or
    None), `bonds_in` (the names of the bonds attached) and `active` [M86 p. 67]."""
    name: str
    value: int
    position: int
    left_nbr: str | None
    right_nbr: str | None
    parent: str | None
    bonds_in: tuple
    active: bool


@dataclass(frozen=True)
class SparkRecord:
    """A spark between two elements [M86 p. 63]: `name`, `a`, `b` (names, in the order the
    Sparkler gave them, `spark6 --- between glint3glint1` [M86 p. 167])."""
    name: str
    a: str
    b: str


@dataclass(frozen=True)
class BondRecord:
    """A bond [M86 p. 64]: `name`, `type` (`Same`, `Succ`, `Pred`), `field`, `modifier`
    (`adjacent`, `remote`), `a` and `b` (left to right), `strength` (DECISION D-019), and
    `show_line`, the bond as `show` prints it, `bond1 Succ print-value (adjacent) --
    (glint1 glint2)` [M86 p. 174]."""
    name: str
    type: str
    field: str
    modifier: str
    a: str
    b: str
    strength: int
    show_line: str


@dataclass(frozen=True)
class GlomRecord:
    """A glom's fields [M86 p. 62]: `name`, `type` (the three words, or an empty tuple for
    a pseudo-glom), `type_text` as printed (`(Same print-value fence)` or `pseudo`
    [M86 p. 167]), `print_value` (nested tuples), `start` and `end` positions, `span`,
    `subgloms` (names, left to right), `parent` (name or None), `structure` (the dubbed
    forms as `(form text, purity)` pairs [M86 p. 69], or None), `pseudo` [M86 pp. 73, 78],
    `bonds_in` (names) and `active` [M86 p. 67]."""
    name: str
    type: tuple
    type_text: str
    print_value: object
    start: int
    end: int
    span: int
    subgloms: tuple
    parent: str | None
    structure: tuple | None
    pseudo: bool
    bonds_in: tuple
    active: bool


# ---- the template [M86 pp. 73-76] ----------------------------------------------------------

@dataclass(frozen=True)
class TemplateRecord:
    """The template's fields: `form` text, `state`, `coverage`, `glom` (name or None),
    `from_glom`, `units` (names), and `fields_text`, the four fields as printed, `(form
    (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)` [M86 p. 167]."""
    form: str
    state: str
    coverage: tuple
    glom: str | None
    from_glom: str
    units: tuple
    fields_text: str


# ---- the socratoplasm [M86 pp. 76-79] -------------------------------------------------------

@dataclass(frozen=True)
class GnothRecord:
    """A gnoth [M86 pp. 77-78]: `name`, `frame`, `plato_class`, `glom` (its gnoth-glom's
    name), `state`, `range`, `members` (the names of its glom collection), `terms` (the
    terms it covers), `parse_print` (its value as parenthesized, `((1 2) 3)` [M86 p. 97]),
    the four reformulation fields `external_bonds` and `internal_bonds` (`(bond name,
    value)` pairs [M86 p. 175]), `equivalence_type` and `groups` (tuples of names), each
    None until set; `catchall` and its `glints` (names) for the catchall gnoth [M86 p. 98]
    (DECISION D-030); and `fields`, every field as `show` prints it, in its order, as
    `(field, text)` pairs [M86 pp. 168, 175]."""
    name: str
    frame: int | None
    plato_class: str | None
    glom: str | None
    state: str
    range: tuple
    members: tuple
    terms: tuple
    parse_print: str
    external_bonds: tuple | None
    internal_bonds: tuple | None
    equivalence_type: str | None
    groups: tuple | None
    catchall: bool
    glints: tuple
    fields: tuple


# ---- the hypothesis and its box tree [M86 pp. 79-85] -----------------------------------------

@dataclass(frozen=True)
class BoxRecord:
    """A Box record of Figure 3 [M86 p. 83]: `name`, its `printstruc`'s name, its
    `pstruc_val` as printed (None when empty) and `ready`."""
    name: str
    printstruc: str
    pstruc_val: str | None
    ready: bool


@dataclass(frozen=True)
class PstrucRecord:
    """A Pstruc record of Figure 3 [M86 p. 83]: `name`, `type`, the `boxes` it feeds
    (names), its `value` as printed (a Constant), its `n_val` and `k_val` boxes (names or
    None), its `elements` (names), and the state carried between hits, `last` (a Countup)
    and `turn` (a Cycle) [M86 pp. 50, 53]."""
    name: str
    type: str
    boxes: tuple
    value: str | None
    n_val: str | None
    k_val: str | None
    elements: tuple
    last: str | None
    turn: int


@dataclass(frozen=True)
class HypothesisRecord:
    """The hypothesis [M86 p. 81]: its `form` text, its `source` (`template`, `fallback`
    [M86 p. 79] or `candidate` [M86 p. 125], tracked from the events because the object
    has no such field, G-006), `validity`, `covered` (terms predicted so far from the
    start), `last_predicted`, `refuted`, `pending` (the terms of the last hit not yet
    predicted), and its box tree: `boxes`, `pstrucs` and `structure_text`
    [M86 p. 83]."""
    form: str
    source: str | None
    validity: int
    covered: int
    last_predicted: int | None
    refuted: bool
    pending: tuple
    boxes: tuple
    pstrucs: tuple
    structure_text: str


# ---- reformulation state [M86 pp. 91, 99, 105] ----------------------------------------------

@dataclass(frozen=True)
class FreezeDriedRecord:
    """A freeze-dried hypothesis: its `form` text and the number of terms it explained,
    "kept on a list of old hypotheses, along with the number of terms of the sequence it
    explained" [M86 p. 91]; `text` is `(Countup 1) (2 terms)` (DECISION D-035)."""
    form: str
    terms_explained: int
    text: str


@dataclass(frozen=True)
class ReformulationRecord:
    """The shared state of stage two: the `reigning` class [M86 p. 99], the `threshold`
    [M86 p. 105], the `freeze_dried` list [M86 p. 91] and the `cast_failures` of the
    episode (DECISION D-050)."""
    reigning: str | None
    threshold: float | None
    freeze_dried: tuple
    cast_failures: int


# ---- the platoplasm [M86 pp. 48-55, 90-91] --------------------------------------------------

@dataclass(frozen=True)
class IdealTypeRecord:
    """One ideal type [M86 pp. 49-55]: `name`, `category`, `format`, `strict` [M86 p. 99],
    its `manifestations` (dubbed gloms' names [M86 p. 69]) and `actualizations` (gnoths'
    names [M86 p. 77]), its `s_links` as `(neighbour, slipperiness)` pairs [M86 p. 90]
    and its `pulls` and `pushes` as `((type, modifier), strength)` pairs [M86 p. 91]."""
    name: str
    category: str
    format: str
    strict: bool
    manifestations: tuple
    actualizations: tuple
    s_links: tuple
    pulls: tuple
    pushes: tuple


@dataclass(frozen=True)
class AtomRecord:
    """An ideal atom [M86 p. 48]: its `value` and the names of its `manifestations`, the
    glints of that value [M86 p. 61]."""
    value: int
    manifestations: tuple


@dataclass(frozen=True)
class PlatoplasmSnapshot:
    """The eight ideal `types` in the thesis's order and the `atoms` made so far, by value
    [M86 pp. 48-55]."""
    types: tuple
    atoms: tuple


# ---- the whole world [M86 pp. 42-47] --------------------------------------------------------

@dataclass(frozen=True)
class Snapshot:
    """The `World` at one moment [M86 pp. 42-47] (G-003): `clock`, `terms`, `done`, the
    `rack`, the cytoplasm's `glints`, `sparks`, `bonds` and `gloms` (each in creation
    order), the `template` (or None), the `gnoths` of the parse in sequence order with the
    `catchall` apart (DECISION D-030), the `parse` as `show-parse` prints it
    [M86 p. 174], the `hypothesis` (or None), the `reformulation` state and the
    `platoplasm`. Each field but the first three is a section shared with the previous
    snapshot when nothing in it changed."""
    clock: int
    terms: tuple
    done: bool
    rack: RackSnapshot
    glints: tuple
    sparks: tuple
    bonds: tuple
    gloms: tuple
    template: TemplateRecord | None
    gnoths: tuple
    catchall: GnothRecord | None
    parse: str
    hypothesis: HypothesisRecord | None
    reformulation: ReformulationRecord
    platoplasm: PlatoplasmSnapshot

    def glom(self, name: str) -> GlomRecord | None:
        """The glom record of a name, or None (G-003)."""
        for glom in self.gloms:
            if glom.name == name:
                return glom
        return None

    def gnoth(self, name: str) -> GnothRecord | None:
        """The gnoth record of a name, the catchall included, or None (G-003)."""
        for gnoth in self.gnoths:
            if gnoth.name == name:
                return gnoth
        if self.catchall is not None and self.catchall.name == name:
            return self.catchall
        return None


# Sections of a Snapshot that are shared with the previous one when equal (G-003).
SHARED = tuple(f.name for f in fields(Snapshot) if f.name not in ("clock", "terms", "done"))


class SnapshotCache:
    """The records of one run's immutable structures, under plain keys (G-003): `tasks`
    by number (a `Task` is frozen and numbers are never reused), `sparks` and `bonds` by
    name (never changed once made, and names are never reused, DECISION D-014),
    `freeze_dried` by position in the world's list (frozen records on an append-only
    list [M86 p. 91]) and `ideal_links`, each ideal type's s-links, pulls and pushes,
    which "the current system does not" change during a run [M86 p. 91]. It holds no
    engine object; a new run gets a new cache."""

    def __init__(self):
        self.tasks: dict = {}
        self.sparks: dict = {}
        self.bonds: dict = {}
        self.freeze_dried: dict = {}
        self.ideal_links: dict = {}


def snapshot(world, previous: Snapshot | None = None, source: str | None = None,
             cache: SnapshotCache | None = None) -> Snapshot:
    """Read the `World` into a `Snapshot`, sharing with `previous` every section that is
    equal to it (G-003; TASK.md rule 2: attribute reads and the listed pure methods only).
    `source` is where the current hypothesis came from, tracked by the tap from the
    `hypothesis` and `candidate` events (G-006). `cache` is the run's `SnapshotCache`
    (a fresh one when None)."""
    if cache is None:
        cache = SnapshotCache()
    glints, sparks, bonds, gloms = [], [], [], []
    # One pass over the cytoplasm, dispatched on the exact type (the four classes are
    # final in the engine, `seekwhence.cytoplasm`), which a long run with hundreds of
    # live sparks makes worth it (G-003).
    for x in world.cytoplasm:
        kind = type(x)
        if kind is cy.Spark:
            sparks.append(_spark(x, cache.sparks))
        elif kind is cy.Bond:
            bonds.append(_bond(x, cache.bonds))
        elif kind is cy.Glint:
            glints.append(_glint(x))
        elif kind is cy.Glom:
            gloms.append(_glom(x))
        elif isinstance(x, cy.Glint):
            glints.append(_glint(x))
        elif isinstance(x, cy.Glom):
            gloms.append(_glom(x))
    sections = {
        "rack": _rack(world, cache.tasks),
        "glints": tuple(glints),
        "sparks": tuple(sparks),
        "bonds": tuple(bonds),
        "gloms": tuple(gloms),
        "template": _template(world.template),
        "gnoths": tuple(_gnoth(g) for g in gn.gnoths(world)),
        "catchall": _catchall(world),
        "parse": gn.parse_text(world),
        "hypothesis": _hypothesis(world.hypothesis, source),
        "reformulation": _reformulation(world, cache.freeze_dried),
        "platoplasm": _platoplasm(world.platoplasm, cache.ideal_links),
    }
    if previous is not None:
        for name in SHARED:
            old = getattr(previous, name)
            if sections[name] == old:
                sections[name] = old
    return Snapshot(clock=world.clock, terms=tuple(world.terms), done=bool(world.done),
                    **sections)


# ---- the sections -----------------------------------------------------------------------------

def _rack(world, cache: dict) -> RackSnapshot:
    rack = world.taskrack
    weights = rack.weights()
    tasks = []
    for task, weight in zip(rack.tasks, weights):
        record = cache.get(task.number)
        if record is None:
            record = TaskRecord(task.number, task.type,
                                tuple(item_text(a) for a in task.args), task.urgency,
                                task.arena, task.created_at, task.at_term, weight)
            cache[task.number] = record
        tasks.append(record)
    return RackSnapshot(tuple(tasks), tuple(rack.count_by_type().items()), sum(weights))


def _name(x) -> str | None:
    return None if x is None else x.name


def _names(xs) -> tuple:
    return tuple(x.name for x in xs)


def _glint(glint: cy.Glint) -> GlintRecord:
    return GlintRecord(glint.name, glint.print_value, glint.position, glint.left_nbr,
                       glint.right_nbr, _name(glint.parent), _names(glint.bonds_in),
                       bool(glint.active))


def _spark(spark: cy.Spark, cache: dict) -> SparkRecord:
    record = cache.get(spark.name)
    if record is None:
        a, b = spark.members
        record = cache[spark.name] = SparkRecord(spark.name, a.name, b.name)
    return record


def _bond(bond: cy.Bond, cache: dict) -> BondRecord:
    record = cache.get(bond.name)
    if record is None:
        a, b = bond.members
        record = cache[bond.name] = BondRecord(bond.name, bond.type, bond.field,
                                               bond.modifier, a.name, b.name,
                                               bond.strength, bond.show_line())
    return record


def _structure(structure) -> tuple | None:
    if structure is None:
        return None
    return tuple((form_text(form), purity) for form, purity in structure)


def _glom(glom: cy.Glom) -> GlomRecord:
    return GlomRecord(glom.name, tuple(glom.type), glom.type_text(), _plain(glom.print_value),
                      glom.start_position, glom.end_position, glom.span,
                      _names(glom.subgloms), _name(glom.parent), _structure(glom.structure),
                      bool(glom.pseudo), _names(glom.bonds_in), bool(glom.active))


def _plain(value):
    """A print-value as nested tuples of integers."""
    if isinstance(value, (tuple, list)):
        return tuple(_plain(v) for v in value)
    return value


def _template(template) -> TemplateRecord | None:
    if template is None:
        return None
    return TemplateRecord(form_text(template.form), template.state, tuple(template.coverage),
                          _name(template.glom), template.from_glom, tuple(template.units),
                          template.fields_text())


def _bond_values(pairs) -> tuple | None:
    if pairs is None:
        return None
    return tuple((getattr(bond, "name", bond), value) for bond, value in pairs)


def _groups(groups) -> tuple | None:
    if groups is None:
        return None
    return tuple(tuple(getattr(x, "name", str(x)) for x in group) for group in groups)


def _fields(gnoth: gn.Gnoth) -> tuple:
    """The `show` fields as (field, text) pairs, read off `show_lines` [M86 p. 168]."""
    return tuple(tuple(line.split(": ", 1)) for line in gnoth.show_lines())


def _gnoth(gnoth: gn.Gnoth) -> GnothRecord:
    return GnothRecord(gnoth.name, gnoth.frame, gnoth.plato_class, _name(gnoth.glom),
                       gnoth.state, tuple(gnoth.range), _names(gnoth.collection),
                       tuple(gnoth.terms), value_text(gnoth.parse_print),
                       _bond_values(gnoth.external_bonds), _bond_values(gnoth.internal_bonds),
                       gnoth.equivalence_type, _groups(gnoth.groups), bool(gnoth.catchall),
                       _names(gnoth.glints), _fields(gnoth))


def _catchall(world) -> GnothRecord | None:
    for x in world.socratoplasm:
        if isinstance(x, gn.Gnoth) and x.catchall:
            return _gnoth(x)
    return None


def _value(value) -> str | None:
    return None if value is None else value_text(value)


def _box(box) -> BoxRecord:
    return BoxRecord(box.name, box.printstruc.name, _value(box.pstruc_val), bool(box.ready))


def _pstruc(pstruc) -> PstrucRecord:
    return PstrucRecord(pstruc.name, pstruc.type, _names(pstruc.boxes), _value(pstruc.value),
                        _name(pstruc.n_val), _name(pstruc.k_val), _names(pstruc.elements),
                        _value(pstruc.last), pstruc.turn)


def tree_records(tree) -> tuple:
    """The `BoxRecord`s and `PstrucRecord`s of a `BoxTree`, in creation order
    [M86 p. 83] (G-003): the hypothesis's own box here, a fresh or private box in the
    hypothesis pane (G-033, G-035). Reads attributes only."""
    return (tuple(_box(b) for b in tree.boxes), tuple(_pstruc(p) for p in tree.pstrucs))


def _hypothesis(hypothesis, source) -> HypothesisRecord | None:
    if hypothesis is None:
        return None
    box = hypothesis.box
    boxes, pstrucs = tree_records(box)
    return HypothesisRecord(hypothesis.show_form(), source, hypothesis.validity,
                            hypothesis.covered, hypothesis.last_predicted,
                            bool(hypothesis.refuted), tuple(hypothesis.pending),
                            boxes, pstrucs, box.structure_text())


def _freeze_dried(position: int, old, cache: dict) -> FreezeDriedRecord:
    key = (id(old), id(old.form))      # the same frozen record at this position
    cached = cache.get(position)
    if cached is not None and cached[0] == key:
        return cached[1]
    form = form_text(old.form) if is_form(old.form) else str(old.form)
    record = FreezeDriedRecord(form, old.terms_explained, str(old))
    cache[position] = (key, record)
    return record


def _reformulation(world, cache: dict) -> ReformulationRecord:
    old = tuple(_freeze_dried(i, x, cache) for i, x in enumerate(world.freeze_dried))
    return ReformulationRecord(world.reigning, world.threshold, old, world.cast_failures)


def _ideal_type(ideal, cache: dict) -> IdealTypeRecord:
    links = cache.get(ideal.name)
    if links is None:
        links = cache[ideal.name] = (tuple(ideal.s_links.items()),
                                     tuple(ideal.pulls.items()),
                                     tuple(ideal.pushes.items()))
    return IdealTypeRecord(ideal.name, ideal.category, ideal.format, bool(ideal.strict),
                           _names(ideal.manifestations), _names(ideal.actualizations),
                           *links)


def _platoplasm(platoplasm, cache: dict) -> PlatoplasmSnapshot:
    types = tuple(_ideal_type(t, cache) for t in platoplasm.types.values())
    atoms = tuple(AtomRecord(value, _names(atom.manifestations))
                  for value, atom in sorted(platoplasm.atoms.items()))
    return PlatoplasmSnapshot(types, atoms)
