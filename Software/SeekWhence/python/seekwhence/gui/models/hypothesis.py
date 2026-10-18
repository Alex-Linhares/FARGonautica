"""The hypothesis pane's model (P4), Qt-free: the form with its source, the form as a tree
of ovals, the box tree with its real names, the predictions against the terms, the
validity with the guess threshold, the candidates of a reformulation, and a private box
to hit (PANES.md P4; G-033, G-034, G-035).

"the hypothesis can display its form ... It can also predict the next term to be expected
following that form. In addition, it has a validity associated with it -- a number that
grows as new, correctly-predicted terms are encountered. The most crucial field, however,
and the one that supports the others, is simply called the hypothesis' box" [M86 p. 81].
The form is drawn as Ch. 1 Figures 1 and 2 draw the primitives: the type's name in an
oval, its arguments hanging under it in circles, a nested form as a child oval and a
shared structure as one node with lines from each place that shares it, and beside each
the terms it gives, `(S-group 2 3)` giving `(2 3 4)` [M86 pp. 16-17, 50-53]. The box tree
is Ch. 2 Figure 3's: Box records (printstruc, pstruc-val, ready) pointing to Pstruc records
(type, boxes, n-val and k-val or value or elements) [M86 p. 83]. The private box is the
appendix's `build-box` and `show-box`: a box of a form "hit" one value at a time,
`(1)`, `(2 2)`, `(3 3 3)` [M86 p. 161].

The model is a view's helper (`seekwhence.gui.hypothesis_view`), fed like the stats model
(G-016): `on_event(event)` for every event, then `update(snapshot)` with the snapshot in
force at that event. The form, its source, the validity, the refutation and the
candidates come from the events (`hypothesis`, `candidate`, `filtered`, `refuted`,
`casts`, `cast-failed`, `guess`, `answered`); the box tree, with its real names, comes
from the snapshot while the snapshot's hypothesis is the one in force, and from a fresh
`BoxTree` of the form, with provisional names, in the events between a new hypothesis
and the next snapshot (G-033). The predictions, the oval tree's hits and the private box
read fresh boxes of their own: the run's box is never hit (ENGINE-API.md section 2,
rule 3). The model holds no engine object but its own fresh boxes.
"""

import dataclasses

from seekwhence.boxes import BoxTree, build_box
from seekwhence.forms import (
    GROUP_TYPES, LIST_TYPES, Form, Shared, flatten, form_text, is_form, is_int, is_wild,
    parse, value_text)
from seekwhence.gui.models.snapshot import BoxRecord, PstrucRecord, tree_records
from seekwhence.hypothesis import Hypothesis

__all__ = ["LOOKAHEAD", "TREE_HITS", "X_STEP", "Y_STEP", "Candidate", "PredictionCell",
           "TreeNode", "HLine", "PrivateBox", "HypothesisModel", "form_tree", "node_ranges",
           "first_hits_text", "fresh_records", "predictions", "validity_fraction",
           "threshold_fraction", "source_text", "postdicts_terms", "box_line", "pstruc_line",
           "box_tooltip", "pstruc_tooltip", "candidate_text", "pane_lines", "node_tooltip"]

# Predicted terms shown beyond the last term entered (G-035).
LOOKAHEAD = 3
# How many hits of the top form are printed under its oval, `(1 2) (1 2 3) (1 2 3 4)`
# (G-034) [M86 p. 56].
TREE_HITS = 3
# The oval tree's layout units: a leaf per column, a level per row (G-034).
X_STEP = 56
Y_STEP = 54
# The kinds of node: a form (an oval), an integer (a circle), a shared structure (a small
# empty circle joined to the structure it shares, Figure 2 [M86 p. 17]), a wildcard.
FORM, INT, SHARED, WILD = "form", "int", "shared", "wild"
# The edges: a parameter of a group or repeater, or an element of a list type.
START, LENGTH, ELEMENT = "start", "length", "element"
# How many hits the node ranges are read over (G-034).
RANGE_HITS = 12


@dataclasses.dataclass(frozen=True)
class Candidate:
    """One cast of a reformulation episode [M86 pp. 124-125] (G-033): the reigning
    `kind_` it was cast under, the `casts` as printed, `((S-group 1 2) (S-group 1 1))`
    [M86 p. 174], the generalised `form` (None when the casts gave none), `postdicts`
    (True once the `candidate` event made it the hypothesis, False once a `cast-failed`
    or a cast with no form, None while undecided), the `reason` of a failure and
    `terms` (the terms known when it was cast, the ones it had to postdict) and
    `made_at` (the model's event count)."""
    kind_: str
    casts: str
    form: str | None
    postdicts: bool | None = None
    reason: str | None = None
    terms: tuple = ()
    made_at: int = 0


@dataclasses.dataclass(frozen=True)
class PredictionCell:
    """One column of the predictions table (PANES.md P4; G-035): the `position` (from
    1), the `predicted` term from a fresh box (None when the form gives none), the
    `term` entered (None beyond the terms) and the `state`: `agrees` (the prediction is
    the term), `differs`, `refuting` (the term that refuted the hypothesis, with the
    prediction the event carried [M86 p. 168]), `beyond` (a term not entered yet) or
    `unknown` (no prediction)."""
    position: int
    predicted: int | None
    term: int | None
    state: str


@dataclasses.dataclass(frozen=True)
class TreeNode:
    """One node of the form drawn as Figures 1 and 2 draw it [M86 pp. 16-17] (G-034):
    `id` (the path of argument positions from the root), `kind` (`form`, `int`,
    `shared`, `wild`), `label` (the type's name or the number), `edge` (how it hangs from
    its parent: `start`, `length`, `element`, or None at the root), `parent`, `children`
    (ids, in argument order), `shares` (the id of the structure a `shared` node shares),
    `text` (the structure as printed), `hits` (the first values a fresh box of the
    structure gives, `(2 3 4)` [M86 p. 16]; empty where the structure cannot stand alone),
    `box` (the index of its box in the box tree of the whole form, in creation order
    [M86 p. 83], None for a parameter the tree keeps as a value), `depth`, `x` and `y`
    (layout units)."""
    id: tuple
    kind: str
    label: str
    edge: str | None
    parent: tuple | None
    children: tuple
    shares: tuple | None
    text: str
    hits: str
    box: int | None
    depth: int
    x: float
    y: float


@dataclasses.dataclass(frozen=True)
class HLine:
    """One line the pane's list paints (G-035): `kind` (`head`, `candidate`, `box`,
    `pstruc`, `private`, `note`), `text`, `tooltip`, `name` (a box or pstruc record's),
    `good` (a candidate that postdicts, a private hit), `bad` (one that does not),
    `grey` (provisional names, a pending candidate)."""
    kind: str
    text: str
    tooltip: str = ""
    name: str | None = None
    good: bool = False
    bad: bool = False
    grey: bool = False


# ---- the oval tree [M86 pp. 16-17, 50-53] -----------------------------------------------------

def form_tree(text: str | None) -> tuple:
    """The form as `TreeNode`s laid out as Figures 1 and 2 [M86 pp. 16-17] (G-034): a
    depth-first walk in the box tree's creation order [M86 p. 83], so that the i-th
    node with a box is the i-th box; leaves take successive columns, a parent stands
    over the middle of its children, a level is a row. An unreadable text gives ()."""
    if not text:
        return ()
    try:
        form = parse(text)
    except (ValueError, TypeError):
        return ()
    builder = _TreeBuilder()
    builder.visit(form, (), None, None, [])
    return builder.layout()


class _TreeBuilder:
    """The walk of `form_tree`: nodes in creation order and their boxes (G-034)."""

    def __init__(self):
        self.nodes = []
        self.boxes = 0
        self.leaf_x = 0

    def _add(self, node_id, kind, label, edge, parent, shares, text, hits, with_box):
        box = None
        if with_box:
            box = self.boxes
            self.boxes += 1
        node = TreeNode(node_id, kind, label, edge, parent, (), shares, text, hits, box,
                        len(node_id), 0.0, 0.0)
        self.nodes.append(node)
        return len(self.nodes) - 1

    def visit(self, structure, node_id, edge, parent, siblings) -> tuple:
        """One structure and, below it, its arguments, in the order `BoxTree` creates
        their boxes (G-034); returns the node's id. `siblings` are the ids of the earlier
        arguments of the same list, which a `Shared` indexes (DECISION D-002)."""
        if isinstance(structure, Shared):
            target = siblings[structure.path[0]]
            for index in structure.path[1:]:
                target = self._argument_ids(target)[index]
            self._add(node_id, SHARED, "shared", edge, parent, target, "shared", "", True)
            return node_id
        if is_int(structure):
            self._add(node_id, INT, str(structure), edge, parent, None, str(structure),
                      "", True)
            return node_id
        if is_wild(structure) or not is_form(structure):
            self._add(node_id, WILD, "n", edge, parent, None, "n", "", False)
            return node_id
        hits = first_hits_text(structure, 1 if parent is not None else TREE_HITS)
        index = self._add(node_id, FORM, structure.kind, edge, parent, None,
                          form_text(structure), hits, True)
        children = []
        if structure.kind == "Constant" and is_int(structure.start):
            # the box tree keeps the value in the printstructure: no box [M86 p. 83]
            children.append(self._value_leaf(structure.start, node_id, 0))
        elif structure.kind in ("Constant", "Countup"):
            children.append(self.visit(structure.start, node_id + (0,), START, node_id, []))
        elif structure.kind in GROUP_TYPES:
            start = self.visit(structure.start, node_id + (0,), START, node_id, [])
            children.append(start)
            children.append(self.visit(structure.length, node_id + (1,), LENGTH, node_id,
                                       [start]))
        elif structure.actual is not None:
            for position, element in enumerate(structure.actual):
                children.append(self.visit(element, node_id + (position,), ELEMENT,
                                           node_id, list(children)))
        node = self.nodes[index]
        self.nodes[index] = dataclasses.replace(node, children=tuple(children))
        return node_id

    def _value_leaf(self, value, parent, position):
        node_id = parent + (position,)
        self._add(node_id, INT, str(value), START, parent, None, str(value), "", False)
        return node_id

    def _argument_ids(self, node_id) -> tuple:
        for node in self.nodes:
            if node.id == node_id:
                return node.children
        return ()

    def layout(self) -> tuple:
        """Leaves in successive columns, a parent over the middle of its children, a
        level per row (G-034)."""
        by_id = {node.id: node for node in self.nodes}
        xs = {}
        next_x = [0]

        def place(node_id):
            node = by_id[node_id]
            if not node.children:
                xs[node_id] = float(next_x[0] * X_STEP)
                next_x[0] += 1
            else:
                for child in node.children:
                    place(child)
                xs[node_id] = sum(xs[c] for c in node.children) / len(node.children)

        if self.nodes:
            place(self.nodes[0].id)
        return tuple(dataclasses.replace(node, x=xs[node.id], y=float(node.depth * Y_STEP))
                     for node in self.nodes)


def first_hits_text(structure, count: int) -> str:
    """The first `count` values a fresh box of a structure gives, as Figure 1 prints
    them beside the ovals, `(2 3 4)` [M86 p. 16]; empty when the structure cannot stand
    alone (it shares a sibling) or gives nothing (G-034)."""
    try:
        tree = BoxTree(structure)
        return " ".join(value_text(tree.hit()) for _ in range(count))
    except (ValueError, TypeError, AttributeError, IndexError):
        return ""


def node_ranges(text: str | None, nodes: tuple, hits: int = RANGE_HITS) -> dict:
    """The positions (from 1) each node's structure gives over the first `hits` hits of
    a fresh box of the whole form (PANES.md P4: "Click an oval to highlight the terms it
    generates"; G-034): the root gives every term; an element of a Tuple or Y-group the
    slice of its parent's that its value fills [M86 pp. 52-53]; an element of a Cycle its
    parent's whole hit on the turns it is fired [M86 p. 53]; a parameter (a start, a
    length) no term, as it is a value, not a term [M86 pp. 50-52]. Keyed by node id."""
    if not text or not nodes:
        return {}
    try:
        tree = BoxTree(parse(text))
    except (ValueError, TypeError):
        return {}
    by_id = {node.id: node for node in nodes}
    ranges = {node.id: [] for node in nodes}
    position = 0
    for _ in range(hits):
        try:
            value = tree.hit()
        except (ValueError, TypeError):
            break
        terms = flatten(value)
        start, end = position + 1, position + len(terms)
        position = end
        if end >= start:
            _assign(nodes[0], (start, end), tree, by_id, ranges)
    return {node_id: tuple(spans) for node_id, spans in ranges.items()}


def _assign(node, span, tree, by_id, ranges):
    """Give a node its span of positions in one hit and share it out to its elements
    (G-034)."""
    ranges[node.id].append(span)
    if node.kind != FORM or node.label not in LIST_TYPES:
        return
    children = [by_id[c] for c in node.children]
    if node.label == "Cycle":
        pstruc = tree.boxes[node.box].printstruc
        fired = children[(pstruc.turn - 1) % len(children)] if children else None
        if fired is not None:
            _assign(fired, span, tree, by_id, ranges)
        return
    offset = span[0]
    for child in children:
        box = tree.boxes[child.box] if child.box is not None else None
        value = box.pstruc_val if box is not None else None
        n = len(flatten(value)) if value is not None else 0
        if n:
            _assign(child, (offset, offset + n - 1), tree, by_id, ranges)
        offset += n


def node_tooltip(node: TreeNode, box_name: str | None) -> str:
    """A node's tooltip: the structure, what it gives, its edge and its box's name
    [M86 pp. 16-17, 83] (G-034)."""
    lines = [node.text]
    if node.hits:
        lines.append(f"gives {node.hits}")
    if node.edge is not None:
        lines.append(f"the {node.edge} of its parent" if node.edge != ELEMENT
                     else "an element of its parent")
    if node.kind == SHARED:
        lines.append("shares the structure it is joined to [M86 p. 17]")
    if box_name:
        lines.append(f"box: {box_name} [M86 p. 83]")
    return "\n".join(lines)


# ---- the box tree and the predictions [M86 pp. 81-83] -----------------------------------------

def fresh_records(text: str | None) -> tuple:
    """The box tree of a fresh `BoxTree` of the form, as records with provisional names
    (`box1`, ...), for the events between a new hypothesis and the next snapshot
    [M86 p. 83] (G-033); `((), ())` when the form gives no tree."""
    if not text:
        return ((), ())
    try:
        return tree_records(BoxTree(parse(text)))
    except (ValueError, TypeError):
        return ((), ())


def predictions(form: str | None, terms, refutation=None, lookahead: int = LOOKAHEAD) -> tuple:
    """The predictions table: a `PredictionCell` per position up to `lookahead` beyond
    the terms, the predicted terms from a fresh `Hypothesis(form).list_terms` (the run's
    box untouched; ENGINE-API.md section 2, rule 3) against the terms entered;
    `refutation` is `(n, term, predicted)` from the `refuted` event [M86 p. 168]; with
    no form, the terms alone, `unknown` (G-035)."""
    terms = list(terms)
    count = len(terms) + (lookahead if form else 0)
    predicted = []
    if form:
        try:
            predicted = Hypothesis(parse(form)).list_terms(count)
        except (ValueError, TypeError):
            predicted = []
    cells = []
    for position in range(1, count + 1):
        guess = predicted[position - 1] if position <= len(predicted) else None
        term = terms[position - 1] if position <= len(terms) else None
        if refutation is not None and refutation[0] == position:
            cells.append(PredictionCell(position, refutation[2], term, "refuting"))
        elif term is None:
            cells.append(PredictionCell(position, guess, None, "beyond"))
        elif guess is None:
            cells.append(PredictionCell(position, None, term, "unknown"))
        else:
            cells.append(PredictionCell(position, guess, term,
                                        "agrees" if guess == term else "differs"))
    return tuple(cells)


def postdicts_terms(form: str | None, terms) -> bool | None:
    """Whether a fresh box of the form gives exactly the terms, "accurately 'postdict'
    the known terms of the sequence" [M86 p. 125] (DECISION D-034), read with
    `list_terms` on a fresh hypothesis (G-033); None when the form is unreadable."""
    if not form:
        return None
    terms = list(terms)
    try:
        return Hypothesis(parse(form)).list_terms(len(terms)) == terms
    except (ValueError, TypeError):
        return None


def validity_fraction(validity: int, threshold: int) -> float:
    """The validity bar's fill, 0 to 1: the validity over the larger of the threshold
    and itself, so the threshold mark sits inside the bar [M86 p. 81] (G-035)."""
    scale = max(threshold, validity, 1)
    return max(0.0, min(1.0, validity / scale))


def threshold_fraction(validity: int, threshold: int) -> float:
    """Where the guess threshold's mark sits on the validity bar, 0 to 1
    (`Config.guess_validity`; G-035)."""
    scale = max(threshold, validity, 1)
    return max(0.0, min(1.0, threshold / scale))


def source_text(source: str | None, kind_: str | None = None) -> str:
    """The hypothesis's source in words: made from the template, the Tuple fall-back
    [M86 p. 79], or a candidate cast under a class [M86 p. 125] (G-033)."""
    if source == "template":
        return "from the template [M86 p. 79]"
    if source == "fallback":
        return "the Tuple fall-back [M86 p. 79]"
    if source == "candidate":
        under = f" cast under {kind_}" if kind_ else ""
        return f"a candidate{under} [M86 p. 125]"
    return ""


def box_line(box: BoxRecord) -> str:
    """A Box record as one line, in Figure 3's field order: `box15 printstruc: pstruc15
    pstruc-val: (1 2) ready: true` [M86 p. 83] (G-035)."""
    value = box.pstruc_val if box.pstruc_val is not None else "-"
    return (f"{box.name} printstruc: {box.printstruc} pstruc-val: {value} "
            f"ready: {'true' if box.ready else 'false'}")


def pstruc_line(pstruc: PstrucRecord) -> str:
    """A Pstruc record as one line, in Figure 3's field order: `pstruc15 type: S-group
    boxes: (box15) n-val: box16 k-val: box17`, `value: 2` for a Constant, `elements:
    (box2 box3)` for a Cycle, Tuple or Y-group [M86 p. 83] (G-035)."""
    fields = [f"type: {pstruc.type}", "boxes: (" + " ".join(pstruc.boxes) + ")"]
    if pstruc.type == "Constant" and pstruc.n_val is None:
        fields.append(f"value: {pstruc.value}")
    elif pstruc.type in ("Constant", "Countup"):
        fields.append(f"n-val: {pstruc.n_val}")
    elif pstruc.type in GROUP_TYPES:
        fields += [f"n-val: {pstruc.n_val}", f"k-val: {pstruc.k_val}"]
    else:
        fields.append("elements: (" + " ".join(pstruc.elements) + ")")
    return f"{pstruc.name} " + " ".join(fields)


def box_tooltip(box: BoxRecord) -> str:
    """Every field of a Box record, one per line [M86 p. 83] (PANES.md P4: "Hover a box
    record for its fields"; G-035)."""
    return "\n".join([box.name, f"printstruc: {box.printstruc}",
                      f"pstruc-val: {box.pstruc_val if box.pstruc_val is not None else '(empty)'}",
                      f"ready: {'true' if box.ready else 'false'}", "[M86 p. 83]"])


def pstruc_tooltip(pstruc: PstrucRecord) -> str:
    """Every field of a Pstruc record, one per line, the state a Countup or a Cycle
    carries between hits included [M86 pp. 50, 53, 83] (G-035)."""
    lines = [pstruc.name, f"type: {pstruc.type}", "boxes: (" + " ".join(pstruc.boxes) + ")"]
    if pstruc.value is not None:
        lines.append(f"value: {pstruc.value}")
    if pstruc.n_val is not None:
        lines.append(f"n-val: {pstruc.n_val}")
    if pstruc.k_val is not None:
        lines.append(f"k-val: {pstruc.k_val}")
    if pstruc.elements:
        lines.append("elements: (" + " ".join(pstruc.elements) + ")")
    if pstruc.last is not None:
        lines.append(f"last: {pstruc.last}")
    if pstruc.type == "Cycle":
        lines.append(f"turn: {pstruc.turn}")
    lines.append("[M86 p. 83]")
    return "\n".join(lines)


def candidate_text(candidate: Candidate) -> str:
    """A cast as one line: the casts, the generalised form and whether it postdicts the
    terms known when it was cast, `casts ((S-group 1 2) (S-group 1 1)) under S-group -->
    (S-group 1 2) postdicts 1 2 1: the new hypothesis` [M86 pp. 125, 174] (G-033)."""
    head = f"casts {candidate.casts} under {candidate.kind_}"
    if candidate.form is None:
        return f"{head}: no form ({candidate.reason or 'the casts did not generalise'})"
    known = " ".join(str(t) for t in candidate.terms)
    if candidate.postdicts is True:
        return f"{head} --> {candidate.form} postdicts {known}: the new hypothesis"
    if candidate.postdicts is False:
        return f"{head} --> {candidate.form}: {candidate.reason or 'failed'}"
    return f"{head} --> {candidate.form}: postdiction pending"


# ---- the private box [M86 p. 161] ----------------------------------------------------------------

class PrivateBox:
    """A box of one's own to hit, as the appendix's `build-box` and `show-box`
    [M86 pp. 161-163] (PANES.md P4: a "Hit" that "never touches the run"; G-035): built
    from a form's text (the program, prose or `build-box` spelling with `bindings`,
    DECISION D-002), `hit()` gives the next value as `show-box` prints it, `hits` keeps
    them, `reset()` starts over [M86 p. 85], `records()` its box tree's records with
    their own names (`box1`, ...)."""

    def __init__(self, text: str, bindings: dict | None = None):
        self.tree = build_box(text, bindings)
        self.text = form_text(self.tree.form)
        self.hits: list = []

    def hit(self) -> str:
        """The next value, `(2 2)` [M86 p. 161]."""
        value = value_text(self.tree.hit())
        self.hits.append(value)
        return value

    def reset(self) -> None:
        """Start the pattern over [M86 p. 85]; the hits are forgotten."""
        self.tree.reset()
        self.hits = []

    def records(self) -> tuple:
        """The box tree's `(boxes, pstrucs)` records now [M86 p. 83]."""
        return tree_records(self.tree)


# ---- the model ------------------------------------------------------------------------------------

class HypothesisModel:
    """The hypothesis in force and what the pane shows of it (see the module docstring;
    G-033, G-034, G-035). `form`, `source`, `kind_` (the class a candidate was cast
    under), `validity`, `refuted`, `refutation` (`(n, term, predicted)` of the last
    refutation [M86 p. 168]), `terms`, `threshold` (`Config.guess_validity`),
    `candidates` (the casts of the episode since the last refutation [M86 p. 125]),
    `guess` (the block ventured, with the answer once given [M86 p. 166]),
    `freeze_dried` (the last form freeze-dried and the terms it explained [M86 p. 91]),
    `boxes` and `pstrucs` (the box tree's records), `box_confirmed` (whether they are
    the run's own, from the snapshot, or a fresh tree's with provisional names),
    `show_box` (the box tree toggle, off by default), `selected` (the oval clicked),
    `private` (the `PrivateBox`, or None), `events`, `corrections` (snapshots whose
    validity disagreed with the events')."""

    def __init__(self):
        self.show_box = False
        self.reset()

    def reset(self) -> None:
        """Forget the run (a `start` event) (Numbo GUI, `coderack_model.py`'s `reset`)."""
        self.form = None
        self.source = None
        self.kind_ = None
        self.validity = 0
        self.refuted = False
        self.refutation = None
        self.terms: list = []
        self.threshold = 1
        self.candidates: list = []
        self.guess = None
        self.freeze_dried = None
        self.boxes: tuple = ()
        self.pstrucs: tuple = ()
        self.box_confirmed = False
        self.selected = None
        self.private = None
        self.events = 0
        self.corrections = 0
        self._snapshot = None
        self._refusal_pending = False
        self._tree_text = None
        self._tree: tuple = ()
        self._ranges: dict = {}

    # -- the events ------------------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply what the event announces (see the module docstring; G-033)."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self.threshold = _threshold(event.config)
        self.events += 1
        if self._refusal_pending:
            # the engine zeroes the validity after publishing the refused answer
            self._refusal_pending = False
            self.validity = 0
        handler = self._HANDLERS.get(kind)
        if handler is not None:
            handler(self, event)

    def _on_term(self, event) -> None:
        self.terms.append(event.term)

    def _new_form(self, form, source, kind_=None) -> None:
        """A new hypothesis: its validity starts at 0, the fresh box tree stands in for
        the run's until the next snapshot (G-033)."""
        self.form = form
        self.source = source
        self.kind_ = kind_
        self.validity = 0
        self.refuted = False
        self.refutation = None
        self.guess = None
        self.selected = None
        self.boxes, self.pstrucs = fresh_records(form)
        self.box_confirmed = False

    def _on_hypothesis(self, event) -> None:
        self._new_form(event.form, event.source)
        self.candidates = []

    def _on_candidate(self, event) -> None:
        pending = [i for i, c in enumerate(self.candidates) if c.postdicts is None]
        if pending:
            i = pending[-1]
            self.candidates[i] = dataclasses.replace(self.candidates[i], form=event.form,
                                                     postdicts=True)
        else:
            self.candidates.append(Candidate(event.kind_, "", event.form, True, None,
                                             tuple(self.terms), self.events))
        self._new_form(event.form, "candidate", event.kind_)

    def _on_filtered(self, event) -> None:
        """"a number that grows as new, correctly-predicted terms are encountered"
        [M86 p. 81]; a wrong one sets it to 0 [M86 p. 98] (DECISION D-032)."""
        self.validity = self.validity + 1 if event.ok else 0

    def _on_refuted(self, event) -> None:
        """`set-out -- validity: 0` [M86 p. 168]; a new episode's casts begin."""
        self.validity = 0
        self.refuted = True
        self.refutation = (event.n, event.term, event.predicted)
        self.candidates = []

    def _on_casts(self, event) -> None:
        postdicts = None if event.generalised else False
        self.candidates.append(Candidate(event.kind_, event.casts, event.generalised,
                                         postdicts, event.reason, tuple(self.terms),
                                         self.events))

    def _on_cast_failed(self, event) -> None:
        pending = [i for i, c in enumerate(self.candidates) if c.postdicts is None]
        if pending:
            i = pending[-1]
            self.candidates[i] = dataclasses.replace(self.candidates[i], postdicts=False,
                                                     reason=event.reason)

    def _on_freeze_dried(self, event) -> None:
        self.freeze_dried = (event.form, event.explained)

    def _on_guess(self, event) -> None:
        self.guess = (event.form, event.block, None, None)

    def _on_answered(self, event) -> None:
        """A refused guess sets the validity to 0, as after `nope` [M86 pp. 175-176]
        (DECISION D-053), with no event of its own: the engine does it after publishing
        the answer, so the model does it at the next event (G-033)."""
        if self.guess is not None:
            form, block, _, _ = self.guess
            self.guess = (form, block, event.answer, event.accepted)
        if not event.accepted:
            self._refusal_pending = True

    _HANDLERS = {
        "term": _on_term, "hypothesis": _on_hypothesis, "candidate": _on_candidate,
        "filtered": _on_filtered, "refuted": _on_refuted, "casts": _on_casts,
        "cast-failed": _on_cast_failed, "freeze-dried": _on_freeze_dried,
        "guess": _on_guess, "answered": _on_answered,
    }

    # -- the snapshot ----------------------------------------------------------------------------

    def update(self, snapshot) -> None:
        """Take the snapshot in force: its box tree, with the run's own names, when its
        hypothesis is the form in force; its validity confirms the events'. Fed in
        order, a snapshot new to the model was taken at the event just given, so it is
        current; one already taken costs nothing and is left alone, since the events
        since it are newer (G-033)."""
        if snapshot is None or snapshot is self._snapshot:
            return
        self._snapshot = snapshot
        if snapshot.hypothesis is None:
            return
        record = snapshot.hypothesis
        if record.form != self.form:
            return
        self.boxes, self.pstrucs = record.boxes, record.pstrucs
        self.box_confirmed = True
        if record.validity != self.validity:
            self.corrections += 1
            self.validity = record.validity

    # -- reading ---------------------------------------------------------------------------------

    def tree(self) -> tuple:
        """The form as `TreeNode`s (G-034), kept while the form is the same."""
        if self._tree_text != self.form:
            self._tree_text = self.form
            self._tree = form_tree(self.form)
            self._ranges = node_ranges(self.form, self._tree)
        return self._tree

    def ranges(self, node_id) -> tuple:
        """The spans of positions a node gives (G-034)."""
        self.tree()
        return self._ranges.get(node_id, ())

    def box_name(self, node: TreeNode) -> str | None:
        """The name of a node's box in the box tree shown: the run's own name when
        confirmed, a provisional one otherwise [M86 p. 83] (G-034)."""
        if node.box is None or node.box >= len(self.boxes):
            return None
        return self.boxes[node.box].name

    def select_node(self, node_id) -> tuple:
        """Select an oval (a second click clears; None clears) and return the positions
        it gives, for the predictions table to highlight (PANES.md P4; G-034)."""
        if node_id is None or node_id == self.selected:
            self.selected = None
            return ()
        self.selected = node_id
        return self.highlighted()

    def highlighted(self) -> tuple:
        """The positions the selected oval gives, in order (G-034)."""
        out = []
        for start, end in self.ranges(self.selected) if self.selected is not None else ():
            out.extend(range(start, end + 1))
        return tuple(out)

    def predictions(self) -> tuple:
        """The predictions table for the form in force (G-035)."""
        return predictions(self.form, self.terms, self.refutation if self.refuted else None)

    def candidate_lines(self) -> tuple:
        """The episode's casts in words [M86 pp. 125, 174] (G-033)."""
        return tuple(candidate_text(c) for c in self.candidates)

    def structure_text(self) -> str:
        """The box tree shown as `BoxTree.structure_text` prints it [M86 p. 83]."""
        lines = [f"{b.name} printstruc: {b.printstruc}" for b in self.boxes]
        lines += [pstruc_line(p) for p in self.pstrucs]
        return "\n".join(lines)

    # -- the private box [M86 p. 161] --------------------------------------------------------------

    def open_private(self, text: str | None = None, bindings: dict | None = None) -> PrivateBox:
        """Build the private box from a form's text, the form in force by default
        (G-035); raises `ValueError` for a text that is not a form."""
        text = text or self.form
        if not text:
            raise ValueError("no form to build a box of")
        self.private = PrivateBox(text, bindings)
        return self.private

    def private_hit(self) -> str:
        """Hit the private box once, building it from the form in force if there is
        none [M86 p. 161] (G-035)."""
        if self.private is None:
            self.open_private()
        return self.private.hit()

    def private_reset(self) -> None:
        """Reset the private box [M86 p. 85]."""
        if self.private is not None:
            self.private.reset()


def _threshold(config) -> int:
    """`guess_validity` from the `start` event's config pairs [M86 p. 81]."""
    for name, value in config:
        if name == "guess_validity":
            try:
                return int(value)
            except (TypeError, ValueError):
                return 1
    return 1


# ---- the pane's lines (G-035) -------------------------------------------------------------------

def pane_lines(model: HypothesisModel) -> tuple:
    """The list under the drawings: the episode's candidates [M86 p. 125], the box tree's
    records when `show_box` is on [M86 p. 83], and the private box's hits [M86 p. 161]
    (G-035)."""
    lines = []
    if model.guess is not None:
        form, block, answer, accepted = model.guess
        verdict = ("waiting for the answer" if answer is None
                   else f"answered {answer}: {'accepted' if accepted else 'refused'}")
        lines.append(HLine("note", f"guess: {block}  ({verdict}) [M86 p. 166]",
                           good=bool(accepted), bad=answer is not None and not accepted))
    if model.freeze_dried is not None:
        form, explained = model.freeze_dried
        lines.append(HLine("note", f"freeze-dried: {form} ({explained} terms) [M86 p. 91]"))
    if model.candidates:
        lines.append(HLine("head", "candidates [M86 pp. 125, 174]"))
        for candidate in model.candidates:
            lines.append(HLine("candidate", candidate_text(candidate),
                               good=candidate.postdicts is True,
                               bad=candidate.postdicts is False,
                               grey=candidate.postdicts is None))
    if model.show_box:
        if model.boxes:
            note = "" if model.box_confirmed else " (provisional names until the next task)"
            lines.append(HLine("head", f"box tree [M86 p. 83]{note}", grey=not model.box_confirmed))
            for box in model.boxes:
                lines.append(HLine("box", box_line(box), box_tooltip(box), box.name,
                                   grey=not model.box_confirmed))
            for pstruc in model.pstrucs:
                lines.append(HLine("pstruc", pstruc_line(pstruc), pstruc_tooltip(pstruc),
                                   pstruc.name, grey=not model.box_confirmed))
        else:
            lines.append(HLine("head", "box tree [M86 p. 83]"))
            lines.append(HLine("note", "no hypothesis, no box"))
    if model.private is not None:
        lines.append(HLine("head", f"private box of {model.private.text} [M86 p. 161]"))
        for number, value in enumerate(model.private.hits, 1):
            lines.append(HLine("private", f"show-box {number}: {value}", good=True))
        if not model.private.hits:
            lines.append(HLine("note", "not hit yet: press Hit"))
        if model.show_box:
            boxes, pstrucs = model.private.records()
            for box in boxes:
                lines.append(HLine("box", box_line(box), box_tooltip(box), box.name))
            for pstruc in pstrucs:
                lines.append(HLine("pstruc", pstruc_line(pstruc), pstruc_tooltip(pstruc),
                                   pstruc.name))
    return tuple(lines)
