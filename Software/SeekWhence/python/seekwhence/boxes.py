"""Boxes, printstructures and firers: a hypothesis form made active [M86 pp. 81-85, 161-163].

"The box is the structure which, when 'hit', produces the next run of terms predicted by the
hypothesis" [M86 p. 81]. "Each box is a repository of information about an underlying
printstructure and through that printstructure branches out, tree-like, to represent in an
active way structures with such forms as: (C-group 1 3) or (C-group (S-group 2 3) 2)"
[M86 p. 82] (Figure 3, p. 83). This module builds that tree from a `Form`: one `Box` per
structure, one `Printstructure` per distinct structure (a shared structure has one
printstructure feeding several boxes [M86 p. 84]), and a firer per printstructure type
[M86 pp. 84-85]. `build_box` and `show_box` reproduce the appendix's sessions [M86 pp. 161-163].

Hits are indivisible, as in "the current version of the program" [M86 p. 82]: a hit on the top
box propagates down the tree and the values bubble up [M86 pp. 82-83] in one call. The values a
box tree gives are, by construction, those of `seekwhence.platoplasm.hits`, the reference
evaluator of a form; the tests check the two agree on every form the thesis prints.
"""

from dataclasses import dataclass, field

from seekwhence.forms import (
    GROUP_TYPES, LIST_TYPES, Form, Shared, form_text, is_form, is_int, parse, value_text)
from seekwhence.platoplasm import generate


@dataclass(eq=False)
class Box:
    """A box: "printstruc", "pstruc-val" and "ready" [M86 p. 83, Figure 3].

    "When a box is hit, it calls upon its underlying printstructure to feed it a value"
    [M86 p. 82]; when filled, "its 'ready' field will be set to 'true', and it will report
    the value in its 'pstruc-val' field" [M86 pp. 83-84]. Under DECISION D-003 a box is ready
    from the moment its printstructure fills it until the next hit on the top box, so a box
    hit twice within one top-level hit reports its stored value twice.
    """

    name: str
    printstruc: "Printstructure"
    pstruc_val: object = None
    ready: bool = False

    def hit(self):
        """"Boxes can be 'hit', prodded for their next value" [M86 p. 82]: a ready box
        reports its value; otherwise its printstructure fires and fills it."""
        if not self.ready:
            self.printstruc.fire()
        return self.pstruc_val

    def fill(self, value):
        """Record a value fed by the printstructure: "each box must be filled whenever the
        printstructure fires. Those boxes must then record the fact that they already have a
        value -- set their 'ready' fields to 'true'" [M86 p. 85]."""
        self.pstruc_val = value
        self.ready = True


@dataclass(eq=False)
class Printstructure:
    """A printstructure: "type", "boxes" (the boxes it feeds) and its fire-boxes
    [M86 pp. 82-84, Figure 3].

    "Each printstructure has a collection of fire-boxes, subboxes which must be hit to give it
    a value" [M86 p. 82]. For a C-group, S-group or P-group they are "its 'n-val' and 'k-val'
    boxes" [M86 p. 83], the start and the length; a Constant has a "value" instead
    [M86 p. 83, Figure 3]; a Countup has its start box (`n_val`) and counts on from it
    [M86 p. 50]; a Y-group, Cycle or Tuple has its element boxes (`elements`), the appendix's
    element lists [M86 pp. 162-163]. A Countup also remembers its last value and a Cycle its
    turn, the state a form carries between hits [M86 pp. 50, 53].
    """

    name: str
    type: str
    boxes: list = field(default_factory=list)
    value: object = None
    n_val: Box | None = None
    k_val: Box | None = None
    elements: list = field(default_factory=list)
    last: object = None
    turn: int = 0

    @property
    def fire_boxes(self) -> list:
        """The boxes this printstructure hits when it fires [M86 pp. 82-83]."""
        if self.type in GROUP_TYPES:
            return [self.n_val, self.k_val]
        if self.type == "Countup":
            return [self.n_val]
        if self.type == "Constant":
            return [self.n_val] if self.n_val is not None else []
        return list(self.elements)

    def fire(self):
        """"When the printstructure 'fires' -- that is, hits its fire-boxes, each box must
        return a value" [M86 p. 82]; the firer of its type turns them into this structure's
        value, which fills every box it feeds, "making both boxes 'ready'" [M86 p. 84]."""
        value = FIRERS[self.type](self)
        for box in self.boxes:
            box.fill(value)
        return value

    def reset(self):
        """Forget the state carried between hits, so the structure starts "from the
        beginning of the pattern described" [M86 p. 85]."""
        self.last = None
        self.turn = 0


# ---- firers [M86 pp. 84-85] ---------------------------------------------------------------
# "each printstructure type (C-group, S-group, etc.) has a firer associated with it, a
# process which knows how to fire the relevant fireboxes of the printstructure and what to
# do with the results" [M86 pp. 84-85].

def fire_constant(pstruc: Printstructure):
    """The Constant firer: "always returns one value" [M86 p. 50], the "value" field
    [M86 p. 83]; an argument that is a structure is hit once, the first time, and its value
    kept (a case the thesis does not show)."""
    if pstruc.value is None:
        pstruc.value = pstruc.n_val.hit()
    return pstruc.value


def fire_countup(pstruc: Printstructure):
    """The Countup firer: `(Countup 3)` "returns a 3 upon first request, then a 4, a 5, and
    so on" [M86 p. 56]; each firing gives the next value in sequence [M86 p. 84]."""
    if pstruc.last is None:
        first = pstruc.n_val.hit()
        if not is_int(first) or first < 0:
            raise ValueError(f"(Countup {first!r}) must start at a nonnegative integer "
                             "[M86 p. 50]")
        pstruc.last = first
    else:
        pstruc.last += 1
    return pstruc.last


def fire_group(pstruc: Printstructure):
    """The C-group, S-group and P-group firer: hit the "n-val" and "k-val" boxes
    [M86 p. 83] and generate the group from the start and length they report
    [M86 pp. 51-52, 54]; a P-group below 0 raises (DECISION D-006)."""
    return generate(pstruc.type, pstruc.n_val.hit(), pstruc.k_val.hit())


def fire_cycle(pstruc: Printstructure):
    """The Cycle firer: "one top-level element each time queried" [M86 p. 53], round and
    round; a shared element's printstructure fires again, this time with the next value in
    sequence (p. 84, paraphrased: the text layer garbles the sentence, checked on the page
    image [M86 p. 84]), because the top-level hit before it cleared `ready`
    (DECISION D-003)."""
    box = pstruc.elements[pstruc.turn % len(pstruc.elements)]
    pstruc.turn += 1
    return box.hit()


def fire_list(pstruc: Printstructure):
    """The Y-group and Tuple firer: hit every element box and return the list [M86 pp. 52-53];
    two boxes on one printstructure report the same value, `(1 8 1)` [M86 p. 162]."""
    return tuple(box.hit() for box in pstruc.elements)


FIRERS = {
    "Constant": fire_constant, "Countup": fire_countup,
    "C-group": fire_group, "S-group": fire_group, "P-group": fire_group,
    "Y-group": fire_list, "Cycle": fire_cycle, "Tuple": fire_list,
}


# ---- building the tree [M86 pp. 82-84, 161-163] -------------------------------------------

class BoxTree:
    """The box tree of one form: the top box and every box and printstructure under it,
    numbered in order of creation (DECISION D-056), with the top-level hit of DECISION D-003.

    `hit()` hits the top box: it first clears every box's `ready`, so that each structure is
    read afresh in this hit and a structure shared by two boxes is read once [M86 pp. 84-85],
    then propagates the hit down the tree [M86 pp. 82-83]. `reset()` makes the tree "start
    from the beginning of the pattern described" [M86 p. 85]; `show(n)` lists "a number of
    terms" [M86 p. 85].
    """

    def __init__(self, form: Form, counters: dict | None = None):
        if not is_form(form):
            raise TypeError(f"not a form: {form!r}")
        self.form = form
        self.counters = counters if counters is not None else {"box": 0, "pstruc": 0}
        self.boxes: list = []
        self.pstrucs: list = []
        self.top = self._box(form, [])

    def _name(self, kind: str) -> str:
        self.counters[kind] += 1
        return f"{kind}{self.counters[kind]}"

    def _box(self, structure, siblings: list) -> Box:
        """One box for a structure, named before its printstructure and its subtree as
        Figure 3 numbers them [M86 p. 83]; a `Shared` gets a box of its own on the
        printstructure of the structure it shares [M86 p. 84] (DECISION D-056)."""
        if isinstance(structure, Shared):
            pstruc = siblings[structure.path[0]].printstruc
            for index in structure.path[1:]:
                pstruc = pstruc.fire_boxes[index].printstruc
            box = Box(self._name("box"), pstruc)
            pstruc.boxes.append(box)
            self.boxes.append(box)
            return box
        box = Box(self._name("box"), None)
        self.boxes.append(box)
        box.printstruc = self._pstruc(structure, box)
        return box

    def _pstruc(self, structure, box: Box) -> Printstructure:
        """The printstructure of a structure, feeding `box`: a Constant for an integer
        [M86 p. 83, Figure 3], the form's type otherwise, with its fire-boxes built below it
        [M86 pp. 82-84]."""
        if is_int(structure):
            pstruc = Printstructure(self._name("pstruc"), "Constant", [box], value=structure)
            self.pstrucs.append(pstruc)
            return pstruc
        if not is_form(structure):
            raise TypeError(f"not a structure: {structure!r}")
        pstruc = Printstructure(self._name("pstruc"), structure.kind, [box])
        self.pstrucs.append(pstruc)
        kind = structure.kind
        if kind == "Constant" and is_int(structure.start):
            pstruc.value = structure.start
        elif kind in ("Constant", "Countup"):
            pstruc.n_val = self._box(structure.start, [])
        elif kind in GROUP_TYPES:
            pstruc.n_val = self._box(structure.start, [])
            pstruc.k_val = self._box(structure.length, [pstruc.n_val])
        else:
            if structure.actual is None:
                raise ValueError(f"{form_text(structure)} has no elements to hit [M86 p. 55]")
            for element in structure.actual:
                pstruc.elements.append(self._box(element, pstruc.elements))
        return pstruc

    def hit(self):
        """A hit on the top box, "prodded for their next value" [M86 p. 82]: the value that
        "appears in the top box's 'pstruc-val' field" [M86 p. 83]."""
        for box in self.boxes:
            box.ready = False
        return self.top.hit()

    def reset(self):
        """"Boxes can also be reset to start from the beginning of the pattern described"
        [M86 p. 85]."""
        for box in self.boxes:
            box.ready = False
            box.pstruc_val = None
        for pstruc in self.pstrucs:
            pstruc.reset()

    def show(self, n: int) -> list:
        """The next n values, as boxes can be "asked to show a number of terms" [M86 p. 85]
        and `show-box` shows one value per call [M86 pp. 161-163]."""
        return [self.hit() for _ in range(n)]

    def structure_text(self) -> str:
        """The tree as a list of nodes and fields, one line per node, in the order of
        Figure 3's drawing [M86 p. 83]: `Box1 printstruc: pstruc1`, `Pstruc1 type: C-group
        boxes: (box1) n-val: box2 k-val: box5`."""
        lines = []
        for box in self.boxes:
            lines.append(f"{box.name} printstruc: {box.printstruc.name}")
        for pstruc in self.pstrucs:
            fields = [f"type: {pstruc.type}",
                      "boxes: (" + " ".join(b.name for b in pstruc.boxes) + ")"]
            if pstruc.type == "Constant" and pstruc.n_val is None:
                fields.append(f"value: {pstruc.value}")
            elif pstruc.type in ("Constant", "Countup"):
                fields.append(f"n-val: {pstruc.n_val.name}")
            elif pstruc.type in GROUP_TYPES:
                fields += [f"n-val: {pstruc.n_val.name}", f"k-val: {pstruc.k_val.name}"]
            else:
                fields.append("elements: (" + " ".join(b.name for b in pstruc.elements) + ")")
            lines.append(f"{pstruc.name} " + " ".join(fields))
        return "\n".join(lines)


# ---- the appendix's build-box and show-box [M86 pp. 161-163] -------------------------------

def build_box(text: str, bindings: dict | None = None, counters: dict | None = None) -> BoxTree:
    """The appendix's `build-box`: a form in the program's spelling, with a shared
    substructure written `(same pstrucN)` at every place it occurs and a list binding each
    name to its form, `(build-box '(C-group (same pstruc1) (same pstruc1))
    '((pstruc1 (Countup 1))))` [M86 p. 161]; returns the tree whose top box the original
    returned by name (`box5`) [M86 p. 161]. The prose and program spellings are accepted too
    (DECISION D-002)."""
    return BoxTree(parse(text, bindings), counters)


def show_box(tree: BoxTree) -> str:
    """The appendix's `show-box`: the top box's next value, printed as the appendix prints
    it, `(2 2)` for a group [M86 p. 161], `(1 8 1)` for a Tuple [M86 p. 162], `8` for a
    Cycle's element [M86 p. 163]."""
    return value_text(tree.hit())
