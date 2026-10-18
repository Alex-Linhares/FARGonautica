"""The cytoplasm pane's model (P2, part 1): the glints, sparks and bonds of the run, their
layout on a baseline and their styles, Qt-free (PANES.md P2; G-021, G-022, G-023).

What it shows is the bottom level of Meredith's world, the cytoplasm of "Gloms, Glints,
Sparks, Bonds" (Figure 1 [M86 p. 44]): the glints, "the cyto-level representations of
the integers entered at the keyboard" [M86 p. 61]; the sparks, each "created between two
gloms when a Sparkler task pulls those gloms at random from the cytoplasm" [M86 p. 63];
and the bonds, which link two gloms and have a strength [M86 p. 64]. It draws them as
Ch. 3 Figure 1 does [M86 p. 102]: a glint is a teardrop with its value above its name,
the glints stand in sequence order on a baseline, and a bond is an arc below the
baseline between its two members, labelled with its value in the reigning class's
environment once the gnoths have been set up (`+10` for the adjacent successor bond,
`-5` for the remote sameness bond in that figure). The colours are Hofstadter's three
kinds of perceptual glue, sameness, successorship and predecessorship [FCCA pp. 58-60]
(PANES.md P2: Same blue, Succ green, Pred orange). Sparks have no figure in the thesis;
they are drawn as faint dashed arcs below the baseline too (G-022).

The model is a view's helper (`seekwhence.gui.cytoplasm_view`), fed like the stats
model (G-016): `on_event(event)` for every event, then `update(snapshot)` with the
snapshot in force at that event. What exists comes from the snapshot, because several
removals are silent (a spark used up by a Tester, the bonds of a dissolved glom;
ENGINE-API.md section 2); the `glint`, `spark` and `bond` events between two snapshots
add what they announce, so the live picture and a seek are exact at every event. The
events also give the animation: a spark or bond made within `FLASH` events flashes, and
one that has vanished stays as a fading ghost for `FADE` events (G-021).

Part 2 (item 06) adds the level above the baseline: the gloms, "cohesive units, made
of adjacent atoms bound by 'bonds' of one type or another" [M86 p. 62], drawn as nested
rounded hulls over their members, one band per nesting depth, so that a glom of gloms
sits above its subgloms: Hofstadter's islands of order, the stretches of a sequence that
hang together and the brackets a perceiver puts around them [FCCA pp. 58-60]. A hull is
labelled with the glom's type and, once dubbed, with the plato-class, the form put in
its structure field and its purity, pure or secondary [M86 p. 69]. The pseudo-gloms
(the template glom [M86 p. 73], the gnoth-gloms [M86 p. 77], the frame gloms and the
caps) are dotted grey hulls, lighter than a real glom's, on the top-down layer, shown by
default so that the current parse is visible at every moment (ANSWERS.md 10.4, which
overrides PANES.md section 3's "hidden by default"; G-053). A
Glom-scout's proposal, `Same -cover proposed --> glint1` [M86 p. 165], is a marker on
its target while its Glomtester waits, and a bracket over the members while the Glommer
it set out waits [M86 pp. 66-67]: the terraced scan at work. A glom burst or dissolved
[M86 pp. 69-70], or blasted away with every gnoth when nothing has worked for a long
time [M86 p. 92], stays as a fading ghost. The selection (`seekwhence.gui.models.
selection`) is shared with every pane: selecting a glom selects its glints (G-024,
G-025, G-026).

The layout (`layout`) and the styles (`glint_style`, `spark_style`, `bond_style`,
`hull_style`, `marker_style`, `value_label`) are pure functions of the marks, so the
tests check every item against the model (Numbo GUI, `tree_view.py`'s `node_style`).
"""

import dataclasses

from seekwhence.gui.models.commands import glom_line
from seekwhence.gui.models.selection import Selection

__all__ = ["FLASH", "FADE", "SPACING", "GLINT_WIDTH", "GLINT_HEIGHT", "ADJACENT_DEPTH",
           "REMOTE_DEPTH", "REMOTE_DEPTH_PER_GAP", "SPARK_EXTRA_DEPTH", "STACK_STEP",
           "HULL_GAP", "HULL_BAND", "HULL_PAD_X", "HULL_STEP_X", "HULL_BOTTOM", "MARKER_SIZE",
           "BOND_COLORS", "SPARK_COLOR", "LAYERS", "PSEUDO_KINDS", "PSEUDO_FILL", "GlintMark", "ArcMark",
           "HullMark", "ProposalMark", "GlintBox", "ArcShape", "HullShape", "MarkerShape",
           "Layout", "GlintStyle", "ArcStyle", "HullStyle", "MarkerStyle", "CytoplasmModel",
           "layout", "glint_style", "spark_style", "bond_style", "hull_style",
           "marker_style", "value_label", "hull_label", "glint_tooltip", "arc_tooltip",
           "hull_tooltip", "marker_tooltip"]

FLASH = 6              # events a new spark or bond flashes (G-021)
FADE = 24              # events a vanished spark or bond stays as a fading ghost (G-021)

# The layout, in scene units (G-022).
SPACING = 72           # from one glint's centre to the next
GLINT_WIDTH = 44
GLINT_HEIGHT = 50
ADJACENT_DEPTH = 24    # an adjacent bond's arc, below the baseline
REMOTE_DEPTH = 36      # a remote bond's arc, plus this per position of gap
REMOTE_DEPTH_PER_GAP = 12
SPARK_EXTRA_DEPTH = 10     # a spark's arc dips this much deeper than a bond's would
STACK_STEP = 7         # several arcs between the same pair are stacked by this
LABEL_HEIGHT = 14

# The hulls above the baseline (G-024).
HULL_GAP = 16          # between the glint tips and the first hull band (the markers' room)
HULL_BAND = 22         # each nesting level's band, with room for the hull's label
HULL_PAD_X = 8         # a hull's width beyond its members' teardrops
HULL_STEP_X = 7        # more per nesting level, so an outer hull sticks out past an inner one
HULL_BOTTOM = 4        # the hull's bottom edge, just under the baseline, so the teardrops are inside
HULL_RADIUS = 10
MARKER_SIZE = 8        # a proposal's marker, a small triangle above its target's tip
LABEL_WIDTH_PER_CHAR = 6   # a marker label's estimated width, for the layout's bounds

# The layers the view can switch (PANES.md P2 "Toggles: sparks, top-down layer, bond values,
# names"); the sparks are off by default (PANES.md section 3) and the top-down layer, the
# pseudo-gloms in grey, is on (ANSWERS.md 10.4; G-026, G-053).
LAYERS = {"sparks": False, "top-down": True, "values": True, "names": True}

# How a pseudo-glom's type reads on its hull [M86 pp. 73, 77-78] (G-024).
PSEUDO_KINDS = {"template": "template glom", "gnoth": "gnoth-glom", "frame": "frame glom",
                "top-down": "top-down unit", "pseudo": "pseudo-glom"}

# Hofstadter's three kinds of glue [FCCA pp. 58-60], as PANES.md P2 colours them.
BOND_COLORS = {"Same": "#1f5fbf", "Succ": "#2e8b3d", "Pred": "#e07b00"}
OTHER_BOND_COLOR = "#555555"
SPARK_COLOR = "#8a8a8a"
GLINT_FILL = "#fff6d5"
GLINT_BORDER = "#333333"
HIGHLIGHT_BORDER = "#c55a11"
FLASH_COLOR = "#e6007e"
HULL_FILL = "#eef3ff"
HULL_BORDER = "#5a6a8a"
DUBBED_BORDER = "#1f3f8f"
PSEUDO_BORDER = "#9a9a9a"    # a pseudo-glom: light grey outline and fill (ANSWERS.md 10.4)
PSEUDO_FILL = "#fafafa"
SELECTED_FILL = "#ffe9cc"
MARKER_LABEL = "#444444"


# ---- the marks: what exists, and since when (G-021) -------------------------------------------

@dataclasses.dataclass(frozen=True)
class GlintMark:
    """A glint as drawn [M86 p. 61]: `name`, `value` (its print-value), `position` (from
    1), `left_nbr` and `right_nbr` (names or None), `bonds_in` (names), `active`, and
    `made_at`, the index of its `glint` event (None when it was first seen in a
    snapshot) (G-021)."""
    name: str
    value: int
    position: int
    left_nbr: str | None
    right_nbr: str | None
    bonds_in: tuple
    active: bool
    made_at: int | None


@dataclasses.dataclass(frozen=True)
class ArcMark:
    """A spark or a bond as drawn (G-021): `kind` (`spark` or `bond`), `name`, its
    members `a` and `b` (names; a bond's left to right [M86 p. 64], a spark's in the
    order the Sparkler gave them [M86 p. 63]), and for a bond its `type` (`Same`,
    `Succ`, `Pred`), `field`, `modifier` (`adjacent`, `remote`) and `strength` (DECISION
    D-019); `value`, the bond's value in the reigning class's environment once a gnoth
    lists it [M86 p. 102], else None; `made_at`, the index of its event (None when first
    seen in a snapshot); `gone_at`, the index at which it was found missing from a
    snapshot (None while it exists)."""
    kind: str
    name: str
    a: str
    b: str
    type: str | None = None
    field: str | None = None
    modifier: str | None = None
    strength: int = 0
    value: int | None = None
    made_at: int | None = None
    gone_at: int | None = None

    @property
    def alive(self) -> bool:
        """Whether it still exists in the cytoplasm (G-021)."""
        return self.gone_at is None


@dataclasses.dataclass(frozen=True)
class HullMark:
    """A glom as drawn [M86 p. 62] (G-024): `name`; `type`, the three words of its type
    field (`Same print-value fence`) or a pseudo-glom's kind (`template`, `gnoth`,
    `frame`, `top-down`; `pseudo` until a snapshot says which); `type_text` as printed;
    `print_value` (nested tuples, printed `(3 7)`); `start` and `end` positions; `members` (its
    subgloms' names, left to right); `parent` (None at the top level); `depth` (1 for a
    glom of glints, one more than its deepest member otherwise); `structure`, the
    dubbings as `(form, purity)` pairs [M86 p. 69], None until dubbed; `pseudo`
    [M86 pp. 73, 77]; `made_at`, the index of its event (None when first seen in a
    snapshot); `gone_at`, the index at which it went, and `fate`: `burst`, `dissolved`
    [M86 pp. 69-70], `blasted` [M86 p. 92] or `gone` (silently, found missing from a
    snapshot)."""
    name: str
    type: tuple
    type_text: str
    print_value: object
    start: int
    end: int
    members: tuple
    parent: str | None
    depth: int
    structure: tuple | None
    pseudo: bool
    made_at: int | None
    gone_at: int | None = None
    fate: str | None = None

    @property
    def alive(self) -> bool:
        """Whether it still exists in the cytoplasm (G-024)."""
        return self.gone_at is None

    @property
    def dubbed(self) -> bool:
        """Whether its structure field is filled [M86 p. 69] (G-024)."""
        return bool(self.structure)


@dataclasses.dataclass(frozen=True)
class ProposalMark:
    """A proposal of the terraced scan in flight [M86 pp. 66-67] (G-024): the task's
    `number`; its `stage`, `proposed` (a Glomtester waits to test the target) or
    `glomming` (the Glommer it set out waits to make the glom); the bond `family` and
    the `test` (`group`, `cover`, `fence`), `Same -cover proposed --> glint1`
    [M86 p. 165]; the `target` (the Glomtester's, or the Glommer's first member);
    `members`, the Glommer's (empty for a Glomtester); `made_at`, the index of its
    `posted` event (None when first seen on the rack)."""
    number: int
    stage: str
    family: str
    test: str
    target: str
    members: tuple
    made_at: int | None


# ---- the layout (G-022) ------------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class GlintBox:
    """A glint's teardrop [M86 p. 102]: `name`, its centre `x` on the baseline, `width`
    and `height` (it stands on the baseline, the point up), `value` and `label` (the
    name) (G-022)."""
    name: str
    x: float
    width: float
    height: float
    value: int
    label: str

    @property
    def left(self) -> float:
        """The box's left edge (G-022)."""
        return self.x - self.width / 2

    @property
    def top(self) -> float:
        """The box's top, above the baseline at y = 0 (G-022)."""
        return -self.height


@dataclasses.dataclass(frozen=True)
class ArcShape:
    """An arc below the baseline [M86 p. 102]: `name`, `kind`, from `x1` to `x2` on the
    baseline, dipping `depth` below it; `label`, the bond's value as printed (`+10`,
    `-5`) or None (G-022)."""
    name: str
    kind: str
    x1: float
    x2: float
    depth: float
    label: str | None

    @property
    def mid_x(self) -> float:
        """The x of the arc's lowest point (G-022)."""
        return (self.x1 + self.x2) / 2


@dataclasses.dataclass(frozen=True)
class HullShape:
    """A glom's hull above the baseline [FCCA pp. 58-60] (G-024): `name`, its box
    `left`, `right`, `top` (negative) and `bottom`, its nesting `depth`, the `label`
    (`hull_label`) and whether it is `pseudo` (dotted, on the top-down layer)."""
    name: str
    left: float
    right: float
    top: float
    bottom: float
    depth: int
    label: str
    pseudo: bool

    @property
    def width(self) -> float:
        """The hull's width (G-024)."""
        return self.right - self.left

    @property
    def height(self) -> float:
        """The hull's height (G-024)."""
        return self.bottom - self.top


@dataclasses.dataclass(frozen=True)
class MarkerShape:
    """A proposal's marker [M86 p. 66] (G-024): the task `number`, the marker's tip at
    `x`, `y` (above its target's teardrop), the `x1` to `x2` bracket over the members (a
    Glommer's; `x1 == x2` for a Glomtester's), the `stage`, the `family` and the `label`
    (`Same -cover`; empty for the second and later markers on one target)."""
    number: int
    x: float
    y: float
    x1: float
    x2: float
    stage: str
    family: str
    label: str


@dataclasses.dataclass(frozen=True)
class Layout:
    """The pane's geometry at one event (G-022, G-024): the `glints` (`GlintBox`es, by
    position), the `arcs` (`ArcShape`s, bonds then sparks, live ones last so they draw on
    top), the `hulls` (`HullShape`s, outer ones first so inner ones draw on top), the
    `markers` (`MarkerShape`s), and the bounds `left`, `right`, `top` (negative, above
    the baseline) and `bottom`."""
    glints: tuple
    arcs: tuple
    left: float
    right: float
    top: float
    bottom: float
    hulls: tuple = ()
    markers: tuple = ()

    @property
    def width(self) -> float:
        """The layout's width (G-022)."""
        return self.right - self.left

    @property
    def height(self) -> float:
        """The layout's height (G-022)."""
        return self.bottom - self.top


# ---- the styles (G-023) ------------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class GlintStyle:
    """How a glint is drawn (G-023): `fill`, `border`, `border_width`, `opacity`, and
    `flash` for a glint made within `FLASH` events."""
    fill: str
    border: str
    border_width: float
    opacity: float
    flash: bool


@dataclasses.dataclass(frozen=True)
class ArcStyle:
    """How an arc is drawn (G-023): `color`, line `width`, `dashed`, `opacity` (a ghost
    fades to nothing over `FADE` events) and `flash` (made within `FLASH` events)."""
    color: str
    width: float
    dashed: bool
    opacity: float
    flash: bool


@dataclasses.dataclass(frozen=True)
class HullStyle:
    """How a hull is drawn (G-024): `fill`, `border`, `border_width`, `dotted` (a
    pseudo-glom), `opacity` (a ghost fades over `FADE` events), `flash` (made within
    `FLASH` events), `bold` (dubbed: its label in bold) and `selected`."""
    fill: str
    border: str
    border_width: float
    dotted: bool
    opacity: float
    flash: bool
    bold: bool
    selected: bool


@dataclasses.dataclass(frozen=True)
class MarkerStyle:
    """How a proposal's marker is drawn (G-024): `color` (the family's bond colour),
    `filled` (a Glommer waiting; a Glomtester's is hollow) and `flash`."""
    color: str
    filled: bool
    flash: bool


def _age(made_at, index) -> int | None:
    return None if made_at is None else index - made_at


def _ghost_opacity(gone_at, index, full) -> float:
    """A vanished mark fades from `full` to nothing over `FADE` events (G-021)."""
    age = index - gone_at
    return max(0.0, full * (1 - age / FADE))


def glint_style(mark: GlintMark, index: int, highlighted: bool = False) -> GlintStyle:
    """A glint's style at event `index` (G-023): a warm fill with a dark border, a thick
    orange border when `highlighted` (the sequence pane's click, PANES.md P1, or the
    shared selection, G-025), a flash for the first `FLASH` events after its `glint`
    event, and a lighter look when not `active` [M86 p. 67]."""
    age = _age(mark.made_at, index)
    flash = age is not None and 0 <= age < FLASH
    border, width = GLINT_BORDER, 1.2
    if highlighted:
        border, width = HIGHLIGHT_BORDER, 3.0
    return GlintStyle(GLINT_FILL, border, width, 1.0 if mark.active else 0.7, flash)


def hull_style(mark: HullMark, index: int, selected: bool = False) -> HullStyle:
    """A hull's style at event `index` (G-024): a pale fill with a slate border; a dubbed
    glom's border darker and its label bold [M86 p. 69]; a pseudo-glom dotted, with a
    light grey outline and fill, clearly lighter than a real glom's blue-slate [M86 p. 73]
    (ANSWERS.md 10.4; G-053); a flash for its first `FLASH` events; a ghost (burst, dissolved,
    blasted or gone) fading to nothing over `FADE` events [M86 pp. 69-70, 92]; the
    selected hull with a thick orange border and a warm fill (G-025)."""
    age = _age(mark.made_at, index)
    flash = mark.alive and age is not None and 0 <= age < FLASH
    opacity = 1.0 if mark.alive else _ghost_opacity(mark.gone_at, index, 1.0)
    if mark.pseudo:
        fill, border, width = PSEUDO_FILL, PSEUDO_BORDER, 1.0
    elif mark.dubbed:
        fill, border, width = HULL_FILL, DUBBED_BORDER, 1.8
    else:
        fill, border, width = HULL_FILL, HULL_BORDER, 1.2
    if selected:
        fill, border, width = SELECTED_FILL, HIGHLIGHT_BORDER, 3.0
    return HullStyle(fill, border, width, mark.pseudo, opacity, flash, mark.dubbed, selected)


def marker_style(mark: ProposalMark, index: int) -> MarkerStyle:
    """A proposal marker's style at event `index` (G-024): the colour of its bond family
    [FCCA pp. 58-60], hollow while the Glomtester waits and filled once the Glommer does
    [M86 pp. 66-67], a flash for its first `FLASH` events."""
    age = _age(mark.made_at, index)
    flash = age is not None and 0 <= age < FLASH
    return MarkerStyle(BOND_COLORS.get(mark.family, OTHER_BOND_COLOR),
                       mark.stage == "glomming", flash)


def spark_style(mark: ArcMark, index: int, lit: bool = False) -> ArcStyle:
    """A spark's style at event `index` (G-021, G-023): a faint dashed grey arc, drawn
    strong while it flashes (its first `FLASH` events), fading once used or gone; `lit`
    (highlighted from another pane, or selected) draws it as a flash does (G-031)."""
    age = _age(mark.made_at, index)
    flash = lit or (mark.alive and age is not None and 0 <= age < FLASH)
    opacity = 0.55
    if flash:
        opacity = 1.0
    elif not mark.alive:
        opacity = _ghost_opacity(mark.gone_at, index, 0.55)
    return ArcStyle(SPARK_COLOR, 2.0 if flash else 1.2, True, opacity, flash)


def bond_style(mark: ArcMark, index: int, lit: bool = False) -> ArcStyle:
    """A bond's style at event `index` (G-023): colour by type (Same, Succ, Pred
    [FCCA pp. 58-60]), line width by strength (DECISION D-019: 1 to 3.5 for 0 to 10),
    a flash for its first `FLASH` events, fading once gone (G-021); `lit` (highlighted
    from another pane, or selected) draws it as a flash does (G-031)."""
    age = _age(mark.made_at, index)
    flash = lit or (mark.alive and age is not None and 0 <= age < FLASH)
    opacity = 1.0 if mark.alive else _ghost_opacity(mark.gone_at, index, 1.0)
    width = 1.0 + 0.25 * max(0, min(10, mark.strength))
    return ArcStyle(BOND_COLORS.get(mark.type, OTHER_BOND_COLOR), width, False, opacity,
                    flash)


def value_label(value: int | None) -> str | None:
    """A bond's value as Figure 1 labels it, `+10` or `-5` [M86 p. 102]; `0` for zero,
    None when unknown (G-022)."""
    if value is None:
        return None
    return f"{value:+d}" if value else "0"


def hull_label(mark: HullMark, names: bool = True) -> str:
    """A hull's label (G-024): the glom's name (unless `names` is off), then its type,
    `Same print-value fence`, or, once dubbed, the plato-class, the form and the purity,
    `Cycle (Cycle 3 2 (3 7)) pure` [M86 p. 69] (several dubbings joined by `;`); a
    pseudo-glom's kind, `template glom` [M86 p. 73], `gnoth-glom` [M86 p. 77]."""
    if mark.pseudo:
        what = PSEUDO_KINDS.get(mark.type[0] if mark.type else "pseudo", "pseudo-glom")
    elif mark.dubbed:
        what = "; ".join(f"{_class_of(form)} {form} {purity}" for form, purity in mark.structure)
    else:
        what = " ".join(mark.type)
    return f"{mark.name}  {what}" if names else what


def _class_of(form: str) -> str:
    """The plato-class a form names, `Cycle` for `(Cycle 3 2 (3 7))` [M86 p. 69]."""
    return form.strip("()").split(" ", 1)[0] if form else ""


# ---- the tooltips (PANES.md P2) ------------------------------------------------------------------

def glint_tooltip(mark: GlintMark) -> str:
    """A glint's fields as the thesis lists them, `name`, `print-value`, `position`,
    `left-nbr`, `right-nbr`, `bonds-in` [M86 p. 61] (G-022)."""
    lines = [f"name: {mark.name}", f"print-value: {mark.value}", f"position: {mark.position}",
             f"left-nbr: {mark.left_nbr or 'nil'}", f"right-nbr: {mark.right_nbr or 'nil'}"]
    if mark.bonds_in:
        lines.append(f"bonds-in: ({' '.join(mark.bonds_in)})")
    return "\n".join(lines)


def arc_tooltip(mark: ArcMark, reigning: str | None = None) -> str:
    """A bond as `show` prints it, `bond1 Same print-value (remote) -- (glint1 glint3)`
    [M86 p. 167], with its value in the reigning class's environment when known
    [M86 p. 102]; a spark as the trace prints it, `spark6 --- between glint3glint1`
    [M86 p. 164] (G-022)."""
    if mark.kind == "spark":
        return f"{mark.name} --- between {mark.a}{mark.b}"
    text = f"{mark.name} {mark.type} {mark.field} ({mark.modifier}) -- ({mark.a} {mark.b})"
    if mark.value is not None:
        where = f" in {reigning}" if reigning else ""
        text += f"\nvalue{where}: {value_label(mark.value)}"
    if not mark.alive:
        text += "\n(gone)"
    return text


def hull_tooltip(mark: HullMark) -> str:
    """A glom as `show` prints it, `glom1 (Same print-value fence) --> (3 7) terms 1 to 2`
    [M86 p. 167], its `subgloms` [M86 p. 62], its `structure` once dubbed, `(Cycle 3 2
    (3 7)) pure` [M86 p. 69], and its fate when gone: burst or dissolved [M86 pp. 69-70],
    blasted [M86 p. 92] (G-024)."""
    lines = [glom_line(mark), f"subgloms: ({' '.join(mark.members)})"]
    if mark.dubbed:
        lines.append("structure: " + "; ".join(f"{form} {purity}" for form, purity in mark.structure))
    if not mark.alive:
        lines.append(f"({mark.fate})")
    return "\n".join(lines)


def marker_tooltip(mark: ProposalMark) -> str:
    """A proposal as the trace prints it, `Same -cover proposed --> glint1` [M86 p. 165],
    and the task waiting for it: `Glomtester on (Same cover glint1)` or `Glommer for Same
    print-value cover members: (glint1 glint2)` [M86 p. 165] (G-024)."""
    first = f"{mark.family} -{mark.test} proposed --> {mark.target}"
    if mark.stage == "glomming":
        second = (f"Glommer for {mark.family} print-value {mark.test} members: "
                  f"({' '.join(mark.members)})")
    else:
        second = f"Glomtester on ({mark.family} {mark.test} {mark.target})"
    return f"{first}\n{second} (task {mark.number})"


# ---- the model (G-021, G-024) ---------------------------------------------------------------------

PROPOSAL_TASKS = ("Glomtester", "Glommer")


class CytoplasmModel:
    """What the cytoplasm pane draws at the current event (see the module docstring;
    G-021, G-024): `glints`, `sparks`, `bonds` and `gloms`, each by name, the `ghosts`
    (sparks and bonds that vanished, by name, until they have faded) and the
    `hull_ghosts` (gloms burst, dissolved, blasted or gone), the `proposals` by task
    number, the `values` of bonds in the reigning class's environment, the `reigning`
    class, the glom `spans` (name to (start, end)), `index` (the current event's),
    `highlighted` (a glint's name or None), the shared `selection` (G-025) and the
    `layers` switched on (G-026)."""

    def __init__(self, selection: Selection | None = None):
        self.selection = selection if selection is not None else Selection()
        self.layers = dict(LAYERS)
        self.reset()

    def reset(self) -> None:
        """Forget the run (Numbo GUI, `run_stats.py`'s `reset`); the selection and the
        layers stay."""
        self.index = -1
        self.glints: dict = {}
        self.sparks: dict = {}
        self.bonds: dict = {}
        self.gloms: dict = {}
        self.ghosts: dict = {}
        self.hull_ghosts: dict = {}
        self.proposals: dict = {}
        self.values: dict = {}
        self.spans: dict = {}
        self.reigning = None
        self.highlighted = None
        self._snapshot = None
        self._running = None
        self._template_glom = None

    # -- the events ------------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Count the event and apply what it announces: a `glint`, `spark` or `bond`
        [M86 pp. 61-64]; a `glom`, `top-down` or `operation-glom` made, a `dubbed`
        glom [M86 pp. 67-69]; a glom `burst` or `dissolved` [M86 pp. 69-70], a `blast`
        or the `blasted` world [M86 pp. 75, 92]; a Glomtester or Glommer `posted`,
        `chosen` and resolved [M86 pp. 66-67]; a `start` begins a new run (G-021,
        G-024)."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self.index = 0
        else:
            self.index += 1
        if kind == "glint":
            self._add_glint(event)
        elif kind == "spark":
            self.sparks[event.name] = ArcMark("spark", event.name, event.a, event.b,
                                              made_at=self.index)
            self.ghosts.pop(event.name, None)
        elif kind == "bond":
            self.bonds[event.name] = ArcMark("bond", event.name, event.a, event.b, event.type,
                                             event.field, event.modifier, event.strength,
                                             self.values.get(event.name), self.index)
            self.ghosts.pop(event.name, None)
        elif kind == "glom":
            self._add_glom(event.name, tuple(event.type), event.members, event.print_value,
                           False)
        elif kind == "top-down":
            type = ("template",) if event.name == self._template_glom else ("pseudo",)
            self._add_glom(event.name, type if event.pseudo else ("top-down",),
                           event.members, event.print_value, event.pseudo)
        elif kind == "operation-glom":
            self._add_glom(event.name, tuple(event.type), event.members, event.print_value,
                           False)
        elif kind == "template-glom":
            self._template_glom = event.name
        elif kind == "dubbed":
            self._dub(event.name, event.form, event.purity)
        elif kind == "burst":
            self._burst(event.name, "burst")
        elif kind == "dissolved":
            self._dissolve(event.name)
        elif kind == "blast":
            for name in event.burst:
                self._burst(name, "blasted")
        elif kind == "blasted":
            for name in event.gloms:
                self._burst(name, "blasted")
        elif kind == "posted" and event.type in PROPOSAL_TASKS:
            if event.type == "Glommer":
                self._resolve_running()
            self.proposals[event.number] = _proposal(event.number, event.type, event.args,
                                                     self.index)
        elif kind == "chosen":
            self._running = event.number if event.number in self.proposals else None
        elif kind in ("glomtester-failed", "glom-failed") or (
                kind == "stale" and event.type in PROPOSAL_TASKS):
            self._resolve_running()
        if kind == "glom":
            self._resolve_running()
        self._prune_ghosts()

    def _add_glint(self, event) -> None:
        """A new glint stands at the right end: its left neighbour is the glint before it,
        which gets it as its right neighbour [M86 p. 61]."""
        left = None
        for glint in self.glints.values():
            if glint.position == event.position - 1:
                left = glint
        if left is not None:
            self.glints[left.name] = dataclasses.replace(left, right_nbr=event.name)
        self.glints[event.name] = GlintMark(event.name, event.value, event.position,
                                            left.name if left else None, None, (), True,
                                            self.index)

    def _add_glom(self, name, type, members, print_value, pseudo) -> None:
        """A glom made over its members [M86 pp. 62, 67, 74]: it spans them, they get it
        as their parent, and it is a nesting level deeper than its deepest member
        (G-024)."""
        members = tuple(members)
        start, end = self._span_of(members)
        depth = 1 + max([self.gloms[m].depth for m in members if m in self.gloms] + [0])
        type_text = "pseudo" if pseudo else "(" + " ".join(type) + ")"
        self.gloms[name] = HullMark(name, tuple(type), type_text, _plain(print_value),
                                    start, end, members, None, depth, None, pseudo, self.index)
        self.hull_ghosts.pop(name, None)
        for member in members:
            glom = self.gloms.get(member)
            if glom is not None:
                self.gloms[member] = dataclasses.replace(glom, parent=name)
        self.spans[name] = (start, end)

    def _span_of(self, members) -> tuple:
        """The positions a glom covers: from its first member's start to its last
        member's end [M86 p. 62]."""
        starts, ends = [], []
        for member in members:
            glint = self.glints.get(member)
            if glint is not None:
                starts.append(glint.position)
                ends.append(glint.position)
            elif member in self.gloms:
                starts.append(self.gloms[member].start)
                ends.append(self.gloms[member].end)
        if not starts:
            return (0, 0)
        return (min(starts), max(ends))

    def _dub(self, name, form, purity) -> None:
        """Dubbing fills the glom's structure field, `(platotype ...)` and `pure` or
        `secondary` [M86 p. 69]; a glom can be dubbed more than once."""
        glom = self.gloms.get(name)
        if glom is not None:
            structure = (glom.structure or ()) + ((form, purity),)
            self.gloms[name] = dataclasses.replace(glom, structure=structure)

    def _dissolve(self, name) -> None:
        """A dissolved glom goes, "leaving its highest-level subgloms to float
        independently" [M86 p. 70]: the members' parent is cleared (DECISION D-059)."""
        glom = self.gloms.pop(name, None)
        if glom is None:
            return
        self.hull_ghosts[name] = dataclasses.replace(glom, gone_at=self.index, fate="dissolved")
        self.spans.pop(name, None)
        for member in glom.members:
            sub = self.gloms.get(member)
            if sub is not None:
                self.gloms[member] = dataclasses.replace(sub, parent=None)

    def _burst(self, name, fate) -> None:
        """A burst glom goes with "all its non-glint subgloms" [M86 p. 69] (`blasted`
        when the blast took it [M86 p. 92])."""
        glom = self.gloms.pop(name, None)
        if glom is None:
            return
        self.hull_ghosts[name] = dataclasses.replace(glom, gone_at=self.index, fate=fate)
        self.spans.pop(name, None)
        for member in glom.members:
            if member in self.gloms:
                self._burst(member, fate)

    def _resolve_running(self) -> None:
        """The chosen Glomtester or Glommer has had its outcome: the glom, a failure, a
        stale task [M86 p. 67] (G-024)."""
        if self._running is not None:
            self.proposals.pop(self._running, None)
            self._running = None

    def _prune_ghosts(self) -> None:
        for ghosts in (self.ghosts, self.hull_ghosts):
            for name in [n for n, g in ghosts.items() if self.index - g.gone_at >= FADE]:
                del ghosts[name]

    # -- the snapshot ------------------------------------------------------------------------

    def update(self, snapshot) -> None:
        """Take the snapshot in force (None before the first): what it lists exists,
        what it no longer lists becomes a ghost at this index; the bond values come from
        the gnoths' `internal_bonds` and `external_bonds` [M86 pp. 102, 175]; the
        proposals are the rack's Glomtesters and Glommers, plus the one chosen and not
        yet resolved [M86 pp. 66-67] (G-021, G-024). A snapshot already taken costs
        nothing."""
        if snapshot is None or snapshot is self._snapshot:
            return
        self._snapshot = snapshot
        self.reigning = snapshot.reformulation.reigning
        self.values = _bond_values(snapshot)
        self.glints = {g.name: _glint_mark(g, self.glints.get(g.name))
                       for g in snapshot.glints}
        self.sparks = self._refresh(self.sparks, snapshot.sparks, "spark")
        self.bonds = self._refresh(self.bonds, snapshot.bonds, "bond")
        self.gloms = self._refresh_gloms(snapshot.gloms)
        self.spans = {g.name: (g.start, g.end) for g in self.gloms.values()}
        self.proposals = self._refresh_proposals(snapshot.rack.tasks)

    def _refresh(self, had: dict, records, kind: str) -> dict:
        """The marks of `kind` listed by the snapshot, keeping `made_at` and ghosting
        the ones no longer listed (G-021)."""
        now = {}
        for record in records:
            old = had.get(record.name)
            value = self.values.get(record.name)
            if old is not None and old.alive and old.value == value:
                now[record.name] = old
            elif kind == "spark":
                now[record.name] = ArcMark("spark", record.name, record.a, record.b,
                                           made_at=old.made_at if old else None)
            else:
                now[record.name] = ArcMark("bond", record.name, record.a, record.b,
                                           record.type, record.field, record.modifier,
                                           record.strength, value,
                                           old.made_at if old else None)
        for name, old in had.items():
            if name not in now and old.alive:
                self.ghosts[name] = dataclasses.replace(old, gone_at=self.index)
        return now

    def _refresh_gloms(self, records) -> dict:
        """The gloms the snapshot lists, with their nesting depths, keeping `made_at`;
        a glom no longer listed becomes a ghost with the fate `gone` (G-024)."""
        by_name = {r.name: r for r in records}
        depths: dict = {}

        def depth(name):
            if name not in by_name:
                return 0
            if name not in depths:
                depths[name] = 1 + max([depth(m) for m in by_name[name].subgloms] + [0])
            return depths[name]

        now = {}
        for record in records:
            old = self.gloms.get(record.name)
            now[record.name] = HullMark(
                record.name, tuple(record.type), record.type_text, record.print_value,
                record.start, record.end, tuple(record.subgloms), record.parent, depth(record.name),
                record.structure, bool(record.pseudo), old.made_at if old is not None else None)
        for name, old in self.gloms.items():
            if name not in now:
                self.hull_ghosts[name] = dataclasses.replace(old, gone_at=self.index, fate="gone")
        return now

    def _refresh_proposals(self, tasks) -> dict:
        """The Glomtesters and Glommers waiting on the rack, keeping `made_at`, plus the
        chosen one until it is resolved (the chosen task is off the rack, G-007)
        (G-024)."""
        now = {}
        for task in tasks:
            if task.type in PROPOSAL_TASKS:
                old = self.proposals.get(task.number)
                now[task.number] = (old if old is not None
                                    else _proposal(task.number, task.type, task.args, None))
        if self._running is not None and self._running in self.proposals:
            now[self._running] = self.proposals[self._running]
        return now

    # -- what is drawn ------------------------------------------------------------------------

    def highlight(self, name: str | None) -> None:
        """Highlight the glint, bond or spark of that name (None: none) (PANES.md P1,
        P2, P3; G-023, G-031)."""
        self.highlighted = name

    def set_layer(self, name: str, on: bool) -> None:
        """Switch a layer, `sparks`, `top-down`, `values` or `names` (PANES.md P2;
        G-026)."""
        if name not in self.layers:
            raise KeyError(name)
        self.layers[name] = bool(on)

    def select(self, name: str | None) -> None:
        """Select an element in the shared selection (G-025): a glom with its subgloms
        and glints, a bond or spark with its two ends, a glint alone; None clears."""
        if name is None:
            self.selection.clear()
        elif name in self.gloms or name in self.hull_ghosts:
            self.selection.select("glom", name, self.under(name))
        elif name in self.glints:
            self.selection.select("glint", name)
        else:
            arc = {m.name: m for m in self.arcs()}.get(name)
            if arc is not None:
                self.selection.select(arc.kind, name, (arc.a, arc.b))

    def under(self, name: str) -> tuple:
        """Every element under a glom, at any depth: its subgloms and its glints
        [M86 p. 62] (G-025)."""
        glom = self.gloms.get(name) or self.hull_ghosts.get(name)
        if glom is None:
            return ()
        out = []
        for member in glom.members:
            out.append(member)
            out.extend(self.under(member))
        return tuple(out)

    def glints_under(self, name: str) -> tuple:
        """The glints under a glom, left to right [M86 p. 62] (G-025)."""
        return tuple(sorted((n for n in self.under(name) if n in self.glints),
                            key=lambda n: self.glints[n].position))

    def is_selected(self, name: str) -> bool:
        """Whether the shared selection includes the element (G-025)."""
        return self.selection.includes(name)

    def glint_at(self, position: int) -> GlintMark | None:
        """The glint at a position (from 1), or None (G-021)."""
        for glint in self.glints.values():
            if glint.position == position:
                return glint
        return None

    def arcs(self) -> list:
        """Every arc to draw: the ghosts first, then the live bonds, then the live sparks
        (G-021)."""
        return list(self.ghosts.values()) + list(self.bonds.values()) + list(self.sparks.values())

    def hulls(self) -> list:
        """Every hull to draw: the ghosts first, then the live gloms, outer ones (deeper
        nesting) before inner ones so the inner draw on top (G-024)."""
        marks = list(self.hull_ghosts.values()) + list(self.gloms.values())
        return sorted(marks, key=lambda m: -m.depth)

    def anchor_x(self, name: str) -> float | None:
        """The x of an element's centre: a glint's position, or the middle of a glom's
        span [M86 p. 62] (G-022); None for a name not drawn."""
        glint = self.glints.get(name)
        if glint is not None:
            return glint.position * SPACING
        span = self.spans.get(name)
        if span is not None:
            return (span[0] + span[1]) / 2 * SPACING
        return None


def _glint_mark(record, old) -> GlintMark:
    return GlintMark(record.name, record.value, record.position, record.left_nbr,
                     record.right_nbr, tuple(record.bonds_in), bool(record.active),
                     old.made_at if old is not None else None)


def _plain(value):
    """A print-value as nested tuples (an event's may hold lists)."""
    if isinstance(value, int):
        return value
    return tuple(_plain(v) for v in value)


def _proposal(number, type, args, made_at) -> ProposalMark:
    """A Glomtester posted (`(Same cover glint1)`) or a Glommer (`Same print-value cover
    (glint1 glint2)`) [M86 p. 165] as a proposal in flight (G-024)."""
    if type == "Glomtester":
        return ProposalMark(number, "proposed", args[0], args[1], args[2], (), made_at)
    members = _member_names(args[3])
    return ProposalMark(number, "glomming", args[0], args[2], members[0] if members else "",
                        members, made_at)


def _member_names(members) -> tuple:
    """A Glommer's members, from the event's tuple or the rack's text `(glint1 glint2)`
    [M86 p. 165]."""
    if isinstance(members, str):
        return tuple(members.strip("()").split())
    return tuple(members)


def _bond_values(snapshot) -> dict:
    """Each bond's value from the gnoths that list it: an internal bond's value, else an
    external bond's (the same bond is external to both its gnoths) [M86 p. 102]."""
    values = {}
    gnoths = list(snapshot.gnoths)
    if snapshot.catchall is not None:
        gnoths.append(snapshot.catchall)
    for gnoth in gnoths:
        for name, value in gnoth.internal_bonds or ():
            values[name] = value
    for gnoth in gnoths:
        for name, value in gnoth.external_bonds or ():
            values.setdefault(name, value)
    return values


# ---- the layout, a pure function of the model (G-022, G-024) --------------------------------------

def layout(model: CytoplasmModel) -> Layout:
    """Lay the model out (G-022, G-024): the glints stand on the baseline (y = 0) at
    `position * SPACING`, as teardrops `GLINT_WIDTH` by `GLINT_HEIGHT`; each arc runs
    between its members' centres and dips below the baseline, an adjacent bond
    `ADJACENT_DEPTH`, a remote one `REMOTE_DEPTH` plus `REMOTE_DEPTH_PER_GAP` per
    position of gap, a spark `SPARK_EXTRA_DEPTH` deeper than a bond of its span would;
    arcs between the same pair are stacked `STACK_STEP` apart. A hull stands above the
    baseline over its glom's span, `HULL_PAD_X` plus `HULL_STEP_X` per nesting level
    beyond the teardrops on each side, its top `HULL_GAP` plus `HULL_BAND` per level
    above the glint tips, its bottom `HULL_BOTTOM` under the baseline [FCCA pp. 58-60];
    a proposal's marker points at its target's tip, a Glommer's with a bracket over its
    members [M86 p. 66]; several proposals on one target stand side by side, in task
    order, and the first carries the label of all of them (`Same -cover, Same -fence`).
    The layers leave out the sparks, the pseudo-gloms, the value labels or the names
    (G-026). An arc with a member not drawn is left out."""
    names_on = model.layers["names"]
    glints = tuple(GlintBox(g.name, g.position * SPACING, GLINT_WIDTH, GLINT_HEIGHT,
                            g.value, g.name if names_on else "")
                   for g in sorted(model.glints.values(), key=lambda g: g.position))
    arcs = _arc_shapes(model)
    hulls = _hull_shapes(model, names_on)
    markers = _marker_shapes(model)
    left, right, top, bottom = _bounds(glints, arcs, hulls, markers)
    return Layout(glints, arcs, left, right, top, bottom, hulls, markers)


def _arc_shapes(model: CytoplasmModel) -> tuple:
    """The arcs of `layout` (G-022): depth by adjacency and gap, sparks deeper, arcs
    between the same pair stacked; the sparks layer and the values layer honoured
    (G-026); an arc with a member not drawn is left out."""
    arcs = []
    stacked: dict = {}
    for mark in model.arcs():
        if mark.kind == "spark" and not model.layers["sparks"]:
            continue
        xa, xb = model.anchor_x(mark.a), model.anchor_x(mark.b)
        if xa is None or xb is None:
            continue
        x1, x2 = sorted((xa, xb))
        gap = max(0, round((x2 - x1) / SPACING) - 1)
        if mark.kind == "bond" and mark.modifier == "adjacent":
            depth = ADJACENT_DEPTH
        else:
            depth = REMOTE_DEPTH + REMOTE_DEPTH_PER_GAP * gap
        if mark.kind == "spark":
            depth += SPARK_EXTRA_DEPTH
        key = (x1, x2, mark.kind)
        depth += STACK_STEP * stacked.get(key, 0)
        stacked[key] = stacked.get(key, 0) + 1
        label = value_label(mark.value) if mark.kind == "bond" and model.layers["values"] else None
        arcs.append(ArcShape(mark.name, mark.kind, x1, x2, depth, label))
    return tuple(arcs)


def _hull_shapes(model: CytoplasmModel, names_on: bool) -> tuple:
    """The hulls of `layout` (G-024): one per glom over its span, wider and taller by
    nesting level; pseudo-gloms only when the top-down layer is on (G-026, G-053)."""
    hulls = []
    for mark in model.hulls():
        if mark.pseudo and not model.layers["top-down"]:
            continue
        if mark.start < 1 or mark.end < mark.start:
            continue
        pad = HULL_PAD_X + HULL_STEP_X * (mark.depth - 1)
        left = mark.start * SPACING - GLINT_WIDTH / 2 - pad
        right = mark.end * SPACING + GLINT_WIDTH / 2 + pad
        top = -(GLINT_HEIGHT + HULL_GAP + HULL_BAND * mark.depth)
        hulls.append(HullShape(mark.name, left, right, top, HULL_BOTTOM, mark.depth,
                               hull_label(mark, names_on), mark.pseudo))
    return tuple(hulls)


def _marker_shapes(model: CytoplasmModel) -> tuple:
    """The proposal markers of `layout` (G-024): at the target's tip, side by side in
    task order, the first carrying the label of all of them; a Glommer's bracket over
    its members [M86 p. 66]."""
    markers = []
    at_target: dict = {}
    labels: dict = {}
    for mark in sorted(model.proposals.values(), key=lambda m: m.number):
        if model.anchor_x(mark.target) is not None:
            text = f"{mark.family} -{mark.test}"
            if text not in labels.setdefault(mark.target, []):
                labels[mark.target].append(text)
    for mark in sorted(model.proposals.values(), key=lambda m: m.number):
        x = model.anchor_x(mark.target)
        if x is None:
            continue
        rank = at_target.get(mark.target, 0)
        at_target[mark.target] = rank + 1
        x1 = x2 = x
        if mark.members:
            xs = [model.anchor_x(m) for m in mark.members]
            xs = [v for v in xs if v is not None]
            if xs:
                x1, x2 = min(xs), max(xs)
        y = -(GLINT_HEIGHT + 3)
        label = ", ".join(labels[mark.target]) if rank == 0 else ""
        markers.append(MarkerShape(mark.number, x + rank * (MARKER_SIZE + 2), y, x1, x2,
                                   mark.stage, mark.family, label))
    return tuple(markers)


def _bounds(glints: tuple, arcs: tuple, hulls: tuple, markers: tuple) -> tuple:
    """The scene's bounds for `layout` (G-022): the glints' row, widened by the hulls
    and the markers' labels, from above the tallest hull to below the deepest arc."""
    if glints:
        left = glints[0].left
        right = glints[-1].left + glints[-1].width
    else:
        left, right = 0.0, SPACING
    left = min([left] + [h.left for h in hulls])
    right = max([right] + [h.right for h in hulls]
                + [m.x + LABEL_WIDTH_PER_CHAR * len(m.label) for m in markers if m.label])
    top = min([-(GLINT_HEIGHT + HULL_GAP)] + [h.top for h in hulls])
    bottom = max([ADJACENT_DEPTH + LABEL_HEIGHT] + [a.depth + LABEL_HEIGHT for a in arcs])
    return left, right, top, bottom
