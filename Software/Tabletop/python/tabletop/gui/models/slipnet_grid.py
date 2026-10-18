"""The Slipnet grid's model (P3; G-030 to G-032), Qt-free: French's grid of square boxes,
"The activation of a node is indicated by the number in the upper right-hand corner of
the corresponding box. This activation is also represented graphically by the black
square within the box." [F95 p. 95], with the activations rebuilt from the events, the
jolts outlined for one event, a seventh row for the nodes the screen lacks, French's
chapter 4 grids beside the run's, and the geometry as a pure function.

The six rows are `slipnet_def.SCREEN_GRID`, the 41 boxes of the chapter 4 dumps [F95
pp. 96-107 (img)]; the seventh row holds the six object nodes absent from that
configuration and "isolated" (D-007; SPEC.md 3.1), and is labelled as the GUI's design,
not French's (ANSWERS.md 4). The activations come from the events alone (G-030): every
node starts at 1 (D-017), `SlipnetUpdated.changed` gives the nodes that moved to a tenth
(D-019), `NodeJolted.activation` the value after a jolt (D-016), `ContextPushed.reset`
the nodes put back to 1 and `ContextPopped.restored` the values after a pop (D-021); so
the model is exact at every event index, to a tenth, and seeking rebuilds it. The
snapshot supplies only the hover text's dated numbers (the links' effective lengths;
G-032), never rolled back. The side of the black square is a pure function of the
activation (`square_side`: 0 at the floor of 1, the whole inner box at the ceiling of
150, linear between, as the dumps' squares grow [F95 pp. 98-107 (img)]; G-031). French's
nine chapter 4 grids are a constant here (`FRENCH_CH4_GRIDS`), as the engine keeps his
numbers in `tabletop/reported.py`, checked against `tests/data/ch4.json` by the tests,
and offered for `surround-05` only (G-029's rule, G-032).
"""

from dataclasses import dataclass

from tabletop import events as ev
from tabletop import slipnet_def as sd
from tabletop.config import PROFILES
from tabletop.gui.models.temperature import CH4_PROBLEM, NOT_FRENCHS, config_of_start
from tabletop.slipnet import ACTIVATION_MAX, ACTIVATION_MIN, clip

__all__ = ["Box", "CH4_PROBLEM", "EXTRA_ROW", "FIRST_ROLE_ROW", "FRENCH_CH4_GRIDS",
           "GridLayout", "Jolt", "NOT_FRENCHS", "ROLE_CAPTION", "ROLE_ROWS", "ROWS",
           "SCREEN_ROWS", "SlipnetGridModel", "box_at", "french_grid_of", "grid_layout",
           "hover_text", "nearest_dump", "number_text", "row_top", "square_side"]

# French's six rows [F95 p. 96 (img)] (D-007), then the GUI's seventh: the nodes the screen
# lacks, in the definition's order (G-030).
SCREEN_ROWS = sd.SCREEN_GRID
EXTRA_ROW = tuple(node.label for node in sd.NODES if not node.on_screen
                  and node.name not in sd.ROLE_NODES)
# The roles' band (D-080; G-062): the 15 role nodes under their own caption, "not on
# French's screen (re-interpretation)" (the author's answer C7 of the roles loop), in rows
# of French's seven columns.
ROLE_ROWS = tuple(tuple(sd.node_named(n).label for n in sd.ROLE_NODES[i:i + 7])
                  for i in range(0, len(sd.ROLE_NODES), 7))
ROWS = SCREEN_ROWS + (EXTRA_ROW,) + ROLE_ROWS
FIRST_ROLE_ROW = len(SCREEN_ROWS) + 1
EXTRA_CAPTION = (NOT_FRENCHS + ": the six object nodes the screen lacks, and isolated (D-007)")
ROLE_CAPTION = "not on French's screen (re-interpretation): the 15 role nodes (D-080)"
LABEL_TO_NAME = {node.label: node.name for node in sd.NODES}
# The 41 labels of the chapter 4 grid in reading order, the order of `FRENCH_CH4_GRIDS`'
# values (`tests/data/ch4.json`, "slipnet_grid").
CH4_LABELS = tuple(label for row in SCREEN_ROWS for label in row)
# French's nine chapter 4 grids, (figure, page, codelets run, the 41 activations in reading
# order) [F95 pp. 96-107 (img)]; Figs. 4.7, 4.8 and 4.9 print the same grid. Checked against
# `tests/data/ch4.json` (G-032).
FRENCH_CH4_GRIDS = (
    ("4.1", 96, 0, (1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1,
                    1, 1, 51, 4, 1, 1, 1, 1, 1, 1, 1, 139, 1, 1, 1)),
    ("4.2", 98, 2, (9, 2, 1, 1, 1, 22, 1, 4, 4, 4, 1, 1, 1, 1, 9, 1, 2, 1, 2, 1, 1, 5, 4, 1, 1, 1,
                    1, 1, 89, 9, 1, 1, 2, 1, 1, 1, 1, 139, 44, 1, 1)),
    ("4.3", 99, 12, (5, 5, 2, 1, 41, 55, 1, 17, 15, 20, 4, 7, 2, 8, 6, 3, 13, 1, 26, 2, 1, 5, 14,
                     4, 1, 20, 9, 1, 127, 32, 3, 1, 6, 8, 1, 2, 1, 137, 26, 5, 2)),
    ("4.4", 100, 22, (13, 6, 2, 1, 45, 48, 1, 20, 17, 27, 7, 11, 3, 7, 7, 4, 18, 1, 41, 9, 2, 13,
                      15, 5, 1, 30, 105, 1, 128, 37, 4, 1, 8, 7, 1, 2, 1, 137, 23, 7, 2)),
    ("4.5", 102, 33, (24, 10, 3, 1, 92, 82, 1, 27, 20, 35, 9, 17, 4, 6, 29, 4, 24, 1, 75, 15, 2,
                      21, 17, 6, 1, 55, 102, 1, 129, 41, 5, 3, 12, 6, 1, 3, 1, 150, 64, 9, 3)),
    ("4.6", 103, 36, (38, 14, 4, 2, 90, 132, 1, 30, 22, 45, 12, 28, 5, 10, 27, 5, 32, 1, 133, 21,
                      2, 25, 19, 8, 1, 92, 99, 1, 129, 45, 8, 5, 16, 6, 1, 5, 1, 150, 57, 13, 3)),
    ("4.7", 104, 47, (53, 20, 6, 9, 105, 132, 1, 53, 25, 58, 15, 45, 7, 12, 36, 6, 42, 1, 150, 29,
                      3, 45, 21, 13, 22, 132, 132, 1, 130, 50, 15, 8, 23, 5, 22, 7, 1, 150, 32,
                      19, 4)),
    ("4.8", 106, 47, (53, 20, 6, 9, 105, 132, 1, 53, 25, 58, 15, 45, 7, 12, 36, 6, 42, 1, 150, 29,
                      3, 45, 21, 13, 22, 132, 132, 1, 130, 50, 15, 8, 23, 5, 22, 7, 1, 150, 32,
                      19, 4)),
    ("4.9", 107, 55, (53, 20, 6, 9, 105, 132, 1, 53, 25, 58, 15, 45, 7, 12, 36, 6, 42, 1, 150, 29,
                      3, 45, 21, 13, 22, 132, 132, 1, 130, 50, 15, 8, 23, 5, 22, 7, 1, 150, 32,
                      19, 4)),
)
# The geometry's proportions (G-031): the margin and the gap before the seventh row as
# shares of a box's side, the square's largest side as a share of the box, the height in
# pixels of a caption line (one for the seventh row's caption, one more for French's note).
MARGIN, GAP, INNER, CAPTION_HEIGHT = 0.15, 0.45, 0.72, 14


@dataclass(frozen=True)
class Jolt:
    """One `NodeJolted` (D-016): the event's `index`, the `clock`, the `amount`, `by` and
    the `activation` after it (G-030)."""
    index: int
    clock: int
    amount: float
    by: str
    activation: float


@dataclass(frozen=True)
class Box:
    """One box of the grid as drawn (G-031): the node's `name` and `label`, its `row` and
    `col`, the box's top-left corner `x`, `y` and `side`, the `activation`, the `number`
    printed in the upper right-hand corner [F95 p. 95], the black `square`'s side,
    `jolted` (outlined for this one event), `selected`, `on_screen` (False in the seventh
    row and the roles' band), `french`, the activation of French's nearest dump, or None,
    and `role` (a box of the roles' band, D-080; G-062)."""
    name: str
    label: str
    row: int
    col: int
    x: float
    y: float
    side: float
    activation: float
    number: str
    square: float
    jolted: bool
    selected: bool
    on_screen: bool
    french: object
    role: bool = False


@dataclass(frozen=True)
class GridLayout:
    """The grid's geometry for one size (G-031): the `boxes`, the box `side`, the grid's
    `left` edge (the grid is centred across the width), the `caption` under the seventh
    row with its top `caption_y`, the `role_caption` under the roles' band with its top
    `role_caption_y` (G-062), the `note` about French's dump (empty when the option is
    off) on the line under it, and the `width` and `height` laid out for."""
    boxes: tuple
    side: float
    left: float
    caption: str
    caption_y: float
    note: str
    width: float
    height: float
    role_caption: str = ""
    role_caption_y: float = 0.0

    def box(self, name: str):
        """The box of the node with this name, or None (G-031)."""
        for box in self.boxes:
            if box.name == name:
                return box
        return None


def square_side(activation: float, inner: float) -> float:
    """The side of the black square for an activation: nothing at the floor of 1, the
    whole inner box at the ceiling of 150, linear between, "Activation values range from
    1 to 150." [F95 p. 95] (D-015; G-031). A pure function of the activation."""
    share = (clip(activation) - ACTIVATION_MIN) / (ACTIVATION_MAX - ACTIVATION_MIN)
    return inner * share


def number_text(activation: float) -> str:
    """The number in the box's corner, an integer as the dumps print it ("139", "44",
    "1") [F95 p. 98 (img)] (G-031)."""
    return str(int(round(activation)))


def nearest_dump(clock: int):
    """French's chapter 4 grid nearest in codelets to `clock` (PANES.md P3): the
    smallest distance, the earlier figure on a tie (Figs. 4.7 and 4.8 are both at 47
    codelets) (G-032)."""
    best = None
    for dump in FRENCH_CH4_GRIDS:
        distance = abs(dump[2] - clock)
        if best is None or distance < best[0]:
            best = (distance, dump)
    return best[1]


def french_grid_of(dump) -> dict:
    """One of `FRENCH_CH4_GRIDS` as a dict label -> activation (G-032)."""
    return dict(zip(CH4_LABELS, dump[3]))


class SlipnetGridModel:
    """The Slipnet as the events tell it (G-030): `activation` by node name (every node
    of `slipnet_def.NODES`), the last `Jolt` of each node (`jolts`) and the index of the
    event shown (`index`), the run's `problem`, `config` and `clock`, `pushes` and `pops`
    counted, `events` seen; `on_event` feeds it, `update(snapshot)` dates the hover
    text's effective lengths, `layout` gives the geometry."""

    def __init__(self):
        self.reset()

    def reset(self) -> None:
        """Forget the run: every node at 1 (D-017; a `RunStarted` begins a new run,
        D-065)."""
        self.problem = None
        self.config = None
        self.activation = {node.name: ACTIVATION_MIN for node in sd.NODES}
        self.jolts = {}
        self.clock = 0
        self.events = 0
        self.pushes = 0
        self.pops = 0
        self.ended = False
        self.effective = {}
        self.dated = None

    @property
    def index(self) -> int:
        """The index of the last event taken, -1 before any (G-030)."""
        return self.events - 1

    @property
    def french_offered(self) -> bool:
        """Whether French's chapter 4 grids apply: the run is `surround-05` (G-029's
        rule; G-032)."""
        return self.problem == CH4_PROBLEM

    def on_event(self, event) -> None:
        """Take one event: the start, an update, a jolt, a push, a pop or the end
        (D-065; G-030)."""
        if isinstance(event, ev.RunStarted):
            self.reset()
            self.problem = event.problem
            self.config = config_of_start(event)
        index = self.events
        self.events += 1
        self.clock = event.clock
        if isinstance(event, ev.SlipnetUpdated):
            for name, value in event.changed:
                self.activation[name] = float(value)
        elif isinstance(event, ev.NodeJolted):
            self.activation[event.node] = float(event.activation)
            self.jolts[event.node] = Jolt(index, event.clock, event.amount, event.by,
                                          event.activation)
        elif isinstance(event, ev.ContextPushed):
            self.pushes += 1
            for name, _before in event.reset:
                self.activation[name] = ACTIVATION_MIN
        elif isinstance(event, ev.ContextPopped):
            self.pops += 1
            for name, after in event.restored:
                self.activation[name] = float(after)
        elif isinstance(event, ev.RunEnded):
            self.ended = True

    def update(self, snapshot) -> None:
        """Take the snapshot in force for the hover text's effective lengths, dated by
        its clock; an older snapshot changes nothing (G-032; G-024's rule)."""
        if snapshot is None or (self.dated is not None and snapshot.clock < self.dated):
            return
        self.dated = snapshot.clock
        self.effective = {(link["a"], link["b"]): link["effective"] for link in snapshot.links}

    def jolted(self, name: str) -> bool:
        """Whether the node was jolted by the event shown: outlined for that one event
        (PANES.md P3; G-030)."""
        jolt = self.jolts.get(name)
        return jolt is not None and jolt.index == self.index

    def depth(self, name: str) -> int:
        """The node's conceptual depth from its level (D-008), by the run's
        `depth_by_level`, the standard's before a run (G-032)."""
        config = self.config if self.config is not None else PROFILES["standard"]
        return config.depth_by_level[sd.node_named(name).level - 1]

    def french_grid(self) -> dict:
        """French's nearest dump as (figure, codelets, {label: activation}) when the run
        is `surround-05`, else None (G-032)."""
        if not self.french_offered:
            return None
        dump = nearest_dump(self.clock)
        return {"figure": dump[0], "page": dump[1], "codelets": dump[2],
                "values": french_grid_of(dump)}

    def layout(self, width, height, french=False, selected=None) -> GridLayout:
        """The grid's geometry for a size (G-031)."""
        return grid_layout(self, width, height, french, selected)

    def tooltip(self, name: str) -> str:
        """The hover text of a node (G-032)."""
        return hover_text(self, name)

    def selection_of(self, name: str):
        """What a click on a box selects: `("node", name, ())` (G-022, G-030)."""
        return ("node", name, ())


def _box_side(width, height, caption_lines) -> float:
    """The side of a box for a size: seven columns and the rows of `ROWS` (French's six,
    the seventh and the roles' band) with the two gaps and the caption lines (G-031,
    G-062)."""
    columns = 7 + 2 * MARGIN
    rows = len(ROWS) + 2 * GAP + 2 * MARGIN
    return max(1.0, min(width / columns, (height - caption_lines * CAPTION_HEIGHT) / rows))


def row_top(row: int, side: float) -> float:
    """The top of a row: French's six, then a gap before the seventh, then its caption
    and a gap before the roles' band (G-031, G-062)."""
    y = MARGIN * side + row * side
    if row >= 6:
        y += GAP * side
    if row >= FIRST_ROLE_ROW:
        y += GAP * side + CAPTION_HEIGHT
    return y


def _note(model, french) -> str:
    """The note on French's dump: which figure, at how many codelets, beside the run's
    (G-032); empty when the option is off."""
    if not french:
        return ""
    grid = model.french_grid()
    if grid is None:
        return "French's chapter 4 grids apply to surround-05 only"
    return (f"red: French's Fig. {grid['figure']} at {grid['codelets']} codelets, beside the "
            f"run's at codelet {model.clock}")


def grid_layout(model, width, height, french=False, selected=None) -> GridLayout:
    """The grid's geometry: the boxes of `ROWS` at a side that fits the size, the number,
    the square by `square_side`, the jolt outlines, the selection, French's nearest dump
    when `french` is on, and the caption of the seventh row (G-031, G-032). A pure
    function of the model, the size and the options."""
    note = _note(model, french)
    side = _box_side(width, height, 2 if note == "" else 3)
    inner = INNER * side
    left = max(MARGIN * side, (width - 7 * side) / 2)
    grid = model.french_grid() if french else None
    values = grid["values"] if grid is not None else {}
    boxes = []
    for row, labels in enumerate(ROWS):
        y = row_top(row, side)
        for col, label in enumerate(labels):
            name = LABEL_TO_NAME[label]
            activation = model.activation[name]
            boxes.append(Box(name, label, row, col, left + col * side, y, side,
                             activation, number_text(activation), square_side(activation, inner),
                             model.jolted(name), name == selected, row < 6,
                             values.get(label) if row < 6 else None, row >= FIRST_ROLE_ROW))
    caption_y = row_top(6, side) + side
    role_caption_y = row_top(len(ROWS) - 1, side) + side
    return GridLayout(tuple(boxes), side, left, EXTRA_CAPTION, caption_y, note,
                      float(width), float(height), ROLE_CAPTION, role_caption_y)


def box_at(layout, x, y):
    """The box under a point, or None (G-031)."""
    for box in layout.boxes:
        if box.x <= x < box.x + box.side and box.y <= y < box.y + box.side:
            return box
    return None


def _link_lines(model, name) -> list:
    """The node's links from the table (D-009, D-010), with the labeled ones' effective
    lengths as of the snapshot's clock (G-032)."""
    lines = []
    for link in sd.links_of(name):
        other = link.b if link.a == name else link.a
        if link.kind == sd.ISA:
            text = f"{link.a} ISA {link.b}" if link.a == name else f"{other} ISA {name}"
        elif link.kind == sd.IDENTITY:
            text = f"{name} to itself [{', '.join(link.labels)}]"
        else:
            text = f"{name} - {other} [{', '.join(link.labels)}]"
        text += f", length {link.length}"
        effective = model.effective.get((link.a, link.b))
        if effective is not None and link.kind != sd.ISA:
            text += f", effective {effective:.1f}"
        lines.append("  " + text)
    return lines


def hover_text(model, name: str) -> str:
    """The hover text of a node (PANES.md P3: its depth, level and links; G-032): the
    activation at the event shown, the level (D-008) and depth, where its box is (French's
    grid or the seventh row), the last jolt (its amount and `by`, D-016) and the links,
    the effective lengths dated as of the snapshot's clock."""
    node = sd.node_named(name)
    lines = [f"{name}: activation {model.activation[name]:.1f} at codelet {model.clock}",
             f"level {node.level}, depth {model.depth(name)}"]
    if node.on_screen:
        for row, labels in enumerate(SCREEN_ROWS):
            if node.label in labels:
                lines.append(f"box '{node.label}', row {row + 1}, column "
                             f"{labels.index(node.label) + 1} of French's grid")
    elif name in sd.ROLE_NODES:
        lines.append(f"box '{node.label}' in the roles' band: not on French's screen "
                     "(re-interpretation; D-080)")
    else:
        lines.append(f"box '{node.label}' in the seventh row: not on French's screen "
                     f"({NOT_FRENCHS}; D-007)")
    jolt = model.jolts.get(name)
    if jolt is not None:
        when = "this event" if jolt.index == model.index else f"codelet {jolt.clock}"
        lines.append(f"last jolt +{jolt.amount:.0f} by {jolt.by} at {when}, to "
                     f"{jolt.activation:.1f}")
    links = _link_lines(model, name)
    dated = f" (effective lengths as of codelet {model.dated})" if model.dated is not None else ""
    lines.append(f"links ({len(links)}){dated}:" if links else "links: none")
    lines.extend(links)
    return "\n".join(lines)
