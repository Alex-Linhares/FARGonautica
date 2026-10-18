"""The table pane's model (PANES.md P1; G-017, G-018, G-020 to G-026): the table seen
from above, its objects, the touch and the answer, through `models/structures.py` its
groups, proposals and correspondences, and through `models/overlays.py` its overlays
(the regions, the salience, the region ranks) and Eliza's view, as a pure layout.
Qt-free.

What French's screen showed: the table as a rectangle with "Henry" written above it and
"Eliza" below, each object drawn at its place and size, the touched object with an
arrow and an **H**, and, once Eliza has answered, her object with an arrow and an **E**,
Figure 4.1 and the dumps of the chapter 4 run [F95 pp. 96-107 (img)]; SPEC.md 6c.14
transcribes the conventions. The objects' positions and footprints are the engine's
(D-001 to D-003); the ids are the problem file's.

The model is fed as every pane model is (PANES.md section 1): `set_problem` once with
the run's `Problem` (the window has it when it starts the run; G-017), `on_event` with
every event, and `update` with the snapshot in force. From the events it takes exactly
what the table shows: `RunStarted` (the ids and the touched object), `ObjectNoticed` (a
noticed mark and the region the object was last seen in), `DescriptionAdded` (the
descriptors of the hover text), `AnswerGiven` (the **E**) and `RunEnded`. The snapshot
only supplies the positions when no problem was given (a replay's history, G-017): the
events are exact at every index, whereas the snapshots are keyframes late in a long run
(G-007), so nothing drawn here depends on which snapshot is in force. `layout()` gives
plain data for the view to paint: the objects with their boxes, the names, the centre
line, the two arrows and the noticed marks, and the structures' boxes and lines (part 2,
`models/structures.py`: the events build them, the snapshot supplies the hover text's
numbers; G-020, G-021), and the overlays the `layers` ask for (`models/overlays.py`: the
snapshot and the tap's salience readings supply the saliences, the ranks and the later
looks at a region; G-024, G-025). With `eliza` on, the layout is the one Eliza sees, the
whole table turned by 180 degrees (`overlays.eliza_view`; G-026). `selection_of` says
what a click on an element selects in the window's shared selection
(`models/selection.py`; G-022).

Henry's half of the table is the upper one, as the dumps print his name above the table
and hers below [F95 p. 98 (img)]: this is a description for the hover text only; the
engine has no sides beyond the regions (D-005).
"""

from dataclasses import dataclass

from tabletop import events as ev
from tabletop.gui.models.overlays import Overlays, OverlaysModel, eliza_view
from tabletop.gui.models.structures import StructuresModel, flash_phase
from tabletop.objects import footprint

__all__ = ["ARROW_LENGTH", "ARROW_SIDE", "HENRY", "ELIZA", "Arrow", "Layout", "Name",
           "TableObject", "TableModel", "arrow_for", "half_of", "object_tooltip",
           "table_of_problem"]

HENRY, ELIZA = "Henry", "Eliza"

# The arrows' geometry in table units (G-018): the head touches the object's box, the
# tail (where the letter sits) lies `ARROW_LENGTH` above it and `ARROW_SIDE` to the left
# for the H, to the right for the E, as the dumps draw them [F95 p. 107 (img)]; an
# object too near the top edge is pointed at from below.
ARROW_LENGTH = 9.0
ARROW_SIDE = 5.0


@dataclass(frozen=True)
class TableObject:
    """One object on the table as drawn (D-001 to D-003): its `id`, `type`, centre
    (`x`, `y`), `orientation`, footprint (`width`, `height`, in table units, as it lies),
    whether it has been noticed, the region it was last seen in (D-004) and its
    descriptors so far (G-017)."""

    id: str
    type: str
    x: float
    y: float
    orientation: str | None
    width: float
    height: float
    noticed: bool = False
    region: str | None = None
    descriptors: tuple = ()

    @property
    def box(self) -> tuple:
        """The footprint box (left, top, right, bottom) (D-002)."""
        return (self.x - self.width / 2, self.y - self.height / 2,
                self.x + self.width / 2, self.y + self.height / 2)


@dataclass(frozen=True)
class Arrow:
    """An arrow with a letter at its tail: `H` on the touched object, `E` on the answer
    [F95 pp. 98-107 (img)] (SPEC.md 6c.14; G-018). Points in table units."""

    letter: str
    object: str
    tail: tuple
    head: tuple


@dataclass(frozen=True)
class Name:
    """A name written beside the table, centred at (`x`, `y`): Henry's above, Eliza's
    below [F95 p. 98 (img)] (G-018)."""

    text: str
    x: float
    y: float


@dataclass(frozen=True)
class Layout:
    """The table's picture as plain data (G-018, G-021): its `width` and `height`, the
    `objects` in file order, the two `names`, the `centre` line's ends (a faint line the
    README's figures draw; the dumps have none), the `touch` and `answer` arrows (None
    until given), the `answer_label` (`E1`, `E2`, ...; empty until given), the `groups`
    (`GroupBox`es, gestalt first), the `proposals` (`ProposalBox`es), the
    `correspondences` (`CorrespondenceLine`s), the flash `phase` of the event, the
    `overlays` the layers on ask for (`overlays.Overlays`; G-025) and whether this is
    `eliza`'s view (G-026)."""

    width: float
    height: float
    objects: tuple
    names: tuple
    centre: tuple
    touch: Arrow | None
    answer: Arrow | None
    answer_label: str = ""
    groups: tuple = ()
    proposals: tuple = ()
    correspondences: tuple = ()
    phase: int = 0
    overlays: Overlays = Overlays()
    eliza: bool = False

    def structure(self, name: str):
        """The group, proposal or correspondence drawn under this name, or None
        (G-021)."""
        for form in self.groups + self.proposals + self.correspondences:
            if form.name == name:
                return form
        return None

    def object(self, id_: str) -> TableObject | None:
        """The object with this id, or None (G-018)."""
        return next((o for o in self.objects if o.id == id_), None)


def table_of_problem(problem) -> tuple:
    """The problem's objects as `TableObject`s, in file order, nothing noticed yet
    (D-001 to D-003; G-017)."""
    out = []
    for o in problem.objects:
        w, h = footprint(o.type, o.orientation)
        out.append(TableObject(o.id, o.type, o.x, o.y, o.orientation, w, h))
    return tuple(out)


def _table_of_snapshot(snapshot) -> tuple:
    out = []
    for o in snapshot.table:
        w, h = footprint(o["type"], o["orientation"])     # the box's sides, exactly (D-002)
        out.append(TableObject(o["id"], o["type"], o["x"], o["y"], o["orientation"], w, h))
    return tuple(out)


def half_of(obj: TableObject, height: float) -> str:
    """Whose half of the table the object's centre lies in, by the middle line: Henry's
    is the upper one [F95 p. 98 (img)]. A description only (D-005; G-018)."""
    return HENRY if obj.y < height / 2 else ELIZA


def arrow_for(letter: str, obj: TableObject, height: float) -> Arrow:
    """The arrow pointing at the object with `letter` at its tail (G-018): the head on
    the top edge of the box, the tail up and to the left for `H`, up and to the right for
    `E`; from below when the box is too near the top edge."""
    left, top, right, bottom = obj.box
    dx = -ARROW_SIDE if letter == "H" else ARROW_SIDE
    if top - ARROW_LENGTH >= 1.0 or bottom + ARROW_LENGTH > height - 1.0:
        head = (obj.x + dx * 0.3, top - 0.4)
        tail = (obj.x + dx, top - ARROW_LENGTH)
    else:
        head = (obj.x + dx * 0.3, bottom + 0.4)
        tail = (obj.x + dx, bottom + ARROW_LENGTH)
    return Arrow(letter, obj.id, tail, head)


def object_tooltip(obj: TableObject, height: float, touched: str, answer) -> str:
    """The hover text of an object (G-018): its id, type, centre, half and footprint;
    the region it was last noticed in (D-004); its descriptors (`DescriptionAdded`);
    whether Henry touched it or Eliza answered with it."""
    lines = [f"{obj.id}: {obj.type}"
             + (f" pointing {obj.orientation}" if obj.orientation else ""),
             f"at ({obj.x:g}, {obj.y:g}), {half_of(obj, height)}'s half; "
             f"footprint {obj.width:g} x {obj.height:g}"]
    if obj.noticed:
        lines.append(f"noticed, last seen in the {obj.region} region")
    else:
        lines.append("not noticed yet")
    if obj.descriptors:
        lines.append("descriptors: " + ", ".join(obj.descriptors))
    if obj.id == touched:
        lines.append("touched by Henry (H)")
    if answer is not None and obj.id == answer[0]:
        lines.append(f"Eliza's answer (E, {answer[1]})")
    return "\n".join(lines)


class TableModel:
    """The table as the events leave it (see the module docstring; G-017, G-020):
    `width`, `height`, `problem` (the name), `touched`, `answers` (label -> id, from the
    problem), `objects` (id -> `TableObject` as placed, unmarked, file order), the marks
    the events gave (`noticed`: id -> last region; `descriptors`: id -> tuple), `answer`
    ((id, label) or None), `finished`, `structures` (the `StructuresModel`), `overlays`
    (the `OverlaysModel`, whose `layers` say which overlays are on; G-025), `eliza`
    (whether the layout is Eliza's view; G-026), and `layout()`, which puts the marks on
    the objects, lays the structures and the overlays out, and turns the picture round
    for Eliza."""

    def __init__(self):
        self.width = 110.0
        self.height = 100.0
        self.problem = None
        self.answers = {}
        self.objects = {}
        self.structures = StructuresModel()
        self.overlays = OverlaysModel()
        self.eliza = False
        self.clear_run()

    def clear_run(self) -> None:
        """Forget the run, keep the table (G-017)."""
        self.touched = None
        self.answer = None
        self.finished = False
        self.events = 0
        self.noticed = {}
        self.descriptors = {}
        self.structures.clear()
        self.overlays.clear_run()

    def set_problem(self, problem) -> None:
        """The run's `Problem`: its size, touched object, answer labels and objects
        (G-017). Called before the run starts."""
        self.width, self.height = float(problem.width), float(problem.height)
        self.problem = problem.name
        self.answers = dict(problem.answers)
        self.objects = {o.id: o for o in table_of_problem(problem)}
        self.overlays.set_problem(problem)
        self.clear_run()
        self.touched = problem.touched

    def on_event(self, event) -> None:
        """Apply one event (G-017, G-020): the start, a notice, a description, the answer,
        the end; the structures' events go to the `StructuresModel`. Every other event
        leaves the table as it is."""
        if isinstance(event, ev.RunStarted):
            if event.problem != self.problem:
                self.problem, self.objects, self.answers = event.problem, {}, {}
            self.clear_run()
            self.touched = event.touched
        elif isinstance(event, ev.ObjectNoticed):
            self.noticed[event.object] = event.region
        elif isinstance(event, ev.DescriptionAdded):
            had = self.descriptors.get(event.item, ())
            if event.descriptor not in had:
                self.descriptors[event.item] = had + (event.descriptor,)
        elif isinstance(event, ev.AnswerGiven):
            self.answer = (event.answer, event.label)
        elif isinstance(event, ev.RunEnded):
            self.finished = True
        self.events += 1
        self.structures.on_event(event)
        self.overlays.on_event(event)

    def _marked(self, obj: TableObject) -> TableObject:
        """The object with the marks the events gave it (G-017)."""
        return TableObject(obj.id, obj.type, obj.x, obj.y, obj.orientation, obj.width,
                           obj.height, obj.id in self.noticed, self.noticed.get(obj.id),
                           self.descriptors.get(obj.id, ()))

    def update(self, snapshot, terms=None, salience=None) -> None:
        """The snapshot in force, with the terms and the salience readings the tap
        recorded beside it: it supplies the positions only when no problem gave them
        (G-017), the structures' hover-text numbers (G-020), the overlays' saliences,
        ranks and later looks (G-024), and never rolls back what the events said (the
        marks are the events', kept apart from the positions)."""
        if snapshot is None:
            return
        self.structures.update(snapshot, terms)
        self.overlays.update(snapshot, salience)
        if self.objects:
            return
        self.problem = snapshot.problem
        self.objects = {o.id: o for o in _table_of_snapshot(snapshot)}
        self.touched = snapshot.touched

    def layout(self) -> Layout:
        """The picture as plain data (G-018, G-021, G-025), Henry's view or, with
        `eliza` on, Eliza's (G-026)."""
        objects = tuple(self._marked(o) for o in self.objects.values())
        touched = self.objects.get(self.touched)
        answered = self.objects.get(self.answer[0]) if self.answer else None
        boxes = {o.id: o.box for o in objects}
        groups = self.structures.group_boxes(boxes)
        lay = Layout(
            width=self.width, height=self.height, objects=objects,
            names=(Name(HENRY, self.width / 2, 0.0), Name(ELIZA, self.width / 2, self.height)),
            centre=((0.0, self.height / 2), (self.width, self.height / 2)),
            touch=arrow_for("H", touched, self.height) if touched else None,
            answer=arrow_for("E", answered, self.height) if answered else None,
            answer_label=self.answer[1] if self.answer else "",
            groups=groups, proposals=self.structures.proposal_boxes(boxes),
            correspondences=self.structures.correspondence_lines(boxes, groups),
            phase=flash_phase(max(0, self.events - 1)),
            overlays=self.overlays.layout(boxes | {g.name: g.box for g in groups}))
        return eliza_view(lay) if self.eliza else lay

    def tooltip(self, name: str) -> str:
        """The hover text of the object with this id, or of the group, proposal or
        correspondence with this name (G-018, G-023), with what the overlays add (the
        last drawn region, the salience terms; G-025); empty for none."""
        obj = self.objects.get(name)
        if obj is None:
            text = self.structures.tooltip(name)
            first = None
        else:
            text = object_tooltip(self._marked(obj), self.height, self.touched, self.answer)
            first = self.noticed.get(name)
        if not text:
            return ""
        return "\n".join([text] + self.overlays.hover_lines(name, first))

    def selection_of(self, name: str):
        """What a click on the element selects in the shared selection (G-022): (kind,
        name, members); an object alone, a group or a proposal with its members, a
        correspondence with its ends and their members; None for no such element."""
        if name in self.objects:
            return ("object", name, ())
        return self.structures.selection_of(name)
