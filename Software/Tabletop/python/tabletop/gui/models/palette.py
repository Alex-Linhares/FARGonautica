"""The palette and editor's model (PANES.md P7; G-050, G-051), Qt-free: `TableEditor`,
the table being set, and the file form it becomes.

The palette is French's: the eleven types in the order of Figure 2.1 [F95 p. 26 (img)]
(`objects.TYPES`), the silverware drawn in its four orientations [F95 p. 55], and, on
the screen, the count of each type on the table beside its cell, with the buttons
"Load config", "Clear table", "Save config", "Do this!" and "Run it again" [F95 p. 96
(img)]. The editing itself is GUI design, not French's (PANES.md P7).

The editor holds `DraftObject`s (an id, a type, a centre in table units to a tenth, an
orientation for silverware), the touched object (H) and the expected answers (E1, E2,
...), and gives the table in the file form of `problems/` (`to_dict`, the keys of
`docs/problems.md` section 10 with provenance in the GUI's own words, G-050), which
becomes a `Problem` only through `problems.problem_from_dict` after `problems.validate`
returns no error (`problem`, `messages`); `tables.realism_faults` says whether French
would set the table [F95 p. 38] (`faults`). A position is clamped so that the footprint
lies inside the table (D-002). Save writes the dict as JSON wherever the caller says,
never into `problems/`; Load reads a problem file, the library's included, and keeps
the library problem's name in `based_on`. Nothing here draws a random number.
"""

import dataclasses
import json
from dataclasses import dataclass
from pathlib import Path

from tabletop import objects as O
from tabletop import problems as P
from tabletop import tables as T
from tabletop.geometry import TABLE_HEIGHT, TABLE_WIDTH
from tabletop.gui.models.table import TableObject, arrow_for

__all__ = ["DraftObject", "EditorStatus", "FAMILIES", "PX_PER_UNIT", "TableEditor",
           "box_px_of", "problem_dir_of", "under_problems_dir"]

FAMILIES = tuple(f for f in P.FAMILIES if f != "ch2")   # a ch2 file needs French's words
DEFAULT_NAME = "edited"
PX_PER_UNIT = 10                   # the file's frame: 1100 x 1000 "pixels" for 110 x 100 units
FRAME_PX = (0, 0, int(TABLE_WIDTH * PX_PER_UNIT), int(TABLE_HEIGHT * PX_PER_UNIT))
EDGE = 0.1                         # a footprint stays this far inside the table (G-050)
GUI_FIGURE = "set in the GUI's palette and editor (P7), not measured on a figure"
GUI_IMAGE = "placed in the GUI, not extracted by pdfimages"
GUI_PAGE = 26                      # the palette's page, Figure 2.1 [F95 p. 26 (img)]
METHOD = ("set in the GUI's palette and editor (P7): each position is the centre of the "
          "icon's footprint in table units, to a tenth, exact (G-050)")
UNCERTAINTY = {"units": 0.0, "note": "set in the GUI: the positions are exact"}
REVIEW = "none: a table set in the GUI, not a digitized figure"
HINTS = {"no objects": "drag an object from the palette onto the table",
         "touched None is not an object": "choose the object Henry touches (H)",
         "no answers": "mark at least one expected answer (E1): the report line names it"}


@dataclass(frozen=True)
class DraftObject:
    """One object of the table being set (G-050): its `id` (the type's stem and a
    number, D-003), `type`, centre (`x`, `y`, table units to a tenth) and `orientation`
    (silverware only [F95 p. 55])."""

    id: str
    type: str
    x: float
    y: float
    orientation: str | None

    @property
    def footprint(self) -> tuple:
        """The footprint as it lies, (width, height) in units (D-002)."""
        return O.footprint(self.type, self.orientation)


@dataclass(frozen=True)
class EditorStatus:
    """What the table is worth (G-051): `messages` from `problems.validate` (empty when
    it can run), `faults` from `tables.realism_faults` [F95 p. 38], `text` in words."""

    messages: tuple
    faults: tuple
    text: str

    @property
    def runnable(self) -> bool:
        """Whether Do this! may start a run: no message from `validate` (G-051)."""
        return not self.messages


def box_px_of(obj: DraftObject) -> list:
    """The object's box on the file's frame (G-050): `PX_PER_UNIT` pixels per unit, the
    centre at a whole pixel, so that `problems.position_from_box` gives the position
    back exactly (D-003)."""
    w, h = obj.footprint
    cx, cy = round(obj.x * PX_PER_UNIT), round(obj.y * PX_PER_UNIT)
    hw, hh = max(1, int(w * PX_PER_UNIT / 2)), max(1, int(h * PX_PER_UNIT / 2))
    return [cx - hw, cy - hh, cx + hw, cy + hh]


def under_problems_dir(path) -> bool:
    """Whether the path lies in the library's folder, where Save config never writes
    (PANES.md P7; G-050)."""
    try:
        Path(path).resolve().relative_to(P.PROBLEMS_DIR.resolve())
    except ValueError:
        return False
    return True


def problem_dir_of(path) -> str:
    """The folder a file dialog should open in: the file's own, or the library's folder
    for Load config, so that the digitized tables can be edited (G-050)."""
    return str(Path(path).parent if path else P.PROBLEMS_DIR)


def clamp(value: float, extent: float, size: float) -> float:
    """`value` moved, if need be, so that a footprint `size` wide centred on it lies
    inside `extent` with `EDGE` to spare, to a tenth (G-050)."""
    low, high = size / 2 + EDGE, extent - size / 2 - EDGE
    return round(min(max(value, low), high), 1)


class TableEditor:
    """The table being set (G-050, G-051): `objects` in the order placed, `touched`,
    `answers` (label to id, `E1`, `E2`, ... in the order marked), the file's `name`,
    `title` and `family`, and `based_on`, the library problem the table was loaded
    from. `listeners` are called after every change. `enabled` is False while a run is
    live, when every change is refused (PANES.md P7)."""

    def __init__(self):
        self.objects: list = []
        self.touched = None
        self.answers: dict = {}
        self.name, self.title, self.family = DEFAULT_NAME, "Edited table", FAMILIES[0]
        self.based_on = None
        self.enabled = True
        self.listeners: list = []

    # -- reading -----------------------------------------------------------------------

    def counts(self) -> dict:
        """How many objects of each type are on the table, every type listed, in the
        palette's order [F95 p. 96 (img)]."""
        counts = {t: 0 for t in O.TYPES}
        for o in self.objects:
            counts[o.type] += 1
        return counts

    def object(self, id_: str) -> DraftObject | None:
        """The object with this id, or None (G-050)."""
        for o in self.objects:
            if o.id == id_:
                return o
        return None

    def placed(self) -> tuple:
        """The objects as the table pane draws them (`models/table.py`'s `TableObject`,
        nothing noticed), for the editor's canvas (G-051)."""
        return tuple(TableObject(o.id, o.type, o.x, o.y, o.orientation, *o.footprint)
                     for o in self.objects)

    def arrows(self) -> tuple:
        """The H arrow on the touched object and an E arrow per expected answer, drawn
        as the table pane draws them (G-018, G-051)."""
        out = []
        for drawn in self.placed():
            if drawn.id == self.touched:
                out.append(arrow_for("H", drawn, TABLE_HEIGHT))
            if drawn.id in self.answers.values():
                out.append(arrow_for("E", drawn, TABLE_HEIGHT))
        return tuple(out)

    def label_of(self, id_: str) -> str:
        """The answer label of an object (`E1`), or an empty string (G-050)."""
        for label, target in self.answers.items():
            if target == id_:
                return label
        return ""

    def hover_text(self, id_: str) -> str:
        """The hover text of an object on the editor's canvas (G-051)."""
        o = self.object(id_)
        if o is None:
            return ""
        w, h = o.footprint
        lines = [f"{o.id}: {o.type}" + (f" pointing {o.orientation}" if o.orientation else ""),
                 f"at ({o.x:g}, {o.y:g}); footprint {w:g} x {h:g}"]
        if o.id == self.touched:
            lines.append("touched by Henry (H)")
        if self.label_of(o.id):
            lines.append(f"expected answer {self.label_of(o.id)}")
        lines.append("drag to move; H touch, E answer, R rotate, Delete remove")
        return "\n".join(lines)

    # -- editing (refused while a run is live) -------------------------------------------

    def _changed(self) -> None:
        for listener in list(self.listeners):
            listener()

    def _refuse_if_disabled(self) -> None:
        if not self.enabled:
            raise ValueError("editing is refused while a run is live")

    def next_id(self, type_: str) -> str:
        """The next free id of the type: its stem and the smallest unused number (D-003;
        G-050)."""
        stem = O.id_stem(type_)
        taken = {o.id for o in self.objects}
        n = 1
        while f"{stem}{n}" in taken:
            n += 1
        return f"{stem}{n}"

    def add(self, type_: str, x: float, y: float, orientation=None) -> str:
        """Place an object of `type_` centred at (`x`, `y`), clamped inside the table;
        silverware points `orientation`, up by default [F95 p. 55]. Returns its id
        (G-050)."""
        self._refuse_if_disabled()
        if type_ not in O.TYPES:
            raise ValueError(f"unknown type {type_!r}")
        if type_ in O.SILVERWARE:
            orientation = orientation or "up"
            if orientation not in O.ORIENTATIONS:
                raise ValueError(f"unknown orientation {orientation!r}")
        else:
            orientation = None
        w, h = O.footprint(type_, orientation)
        obj = DraftObject(self.next_id(type_), type_, clamp(x, TABLE_WIDTH, w),
                          clamp(y, TABLE_HEIGHT, h), orientation)
        self.objects.append(obj)
        self._changed()
        return obj.id

    def _replace(self, id_: str, **changes) -> DraftObject:
        self._refuse_if_disabled()
        old = self.object(id_)
        if old is None:
            raise KeyError(id_)
        new = dataclasses.replace(old, **changes)
        self.objects[self.objects.index(old)] = new
        self._changed()
        return new

    def move(self, id_: str, x: float, y: float) -> DraftObject:
        """Move an object to (`x`, `y`), clamped inside the table, to a tenth (G-050)."""
        o = self.object(id_)
        if o is None:
            raise KeyError(id_)
        w, h = o.footprint
        return self._replace(id_, x=clamp(x, TABLE_WIDTH, w), y=clamp(y, TABLE_HEIGHT, h))

    def rotate(self, id_: str) -> DraftObject:
        """Turn a piece of silverware to the next orientation of `objects.ORIENTATIONS`
        [F95 p. 55]; any other type is left as it is (G-050)."""
        o = self.object(id_)
        if o is None:
            raise KeyError(id_)
        if o.type not in O.SILVERWARE:
            return o
        turned = O.ORIENTATIONS[(O.ORIENTATIONS.index(o.orientation) + 1) % 4]
        w, h = O.footprint(o.type, turned)
        return self._replace(id_, orientation=turned, x=clamp(o.x, TABLE_WIDTH, w),
                             y=clamp(o.y, TABLE_HEIGHT, h))

    def delete(self, id_: str) -> None:
        """Take an object off the table; a touch or an answer on it goes too (G-050)."""
        self._refuse_if_disabled()
        o = self.object(id_)
        if o is None:
            raise KeyError(id_)
        self.objects.remove(o)
        if self.touched == id_:
            self.touched = None
        self.answers = self._relabelled(id_)
        self._changed()

    def _relabelled(self, dropped: str) -> dict:
        kept = [target for target in self.answers.values() if target != dropped]
        return {f"E{n}": target for n, target in enumerate(kept, 1)}

    def touch(self, id_: str) -> None:
        """Henry touches this object (H) [F95 p. 114]; one object at a time (G-050)."""
        self._refuse_if_disabled()
        if self.object(id_) is None:
            raise KeyError(id_)
        self.touched = id_
        self._changed()

    def toggle_answer(self, id_: str) -> None:
        """Mark the object as an expected answer, the next label (`E1`, `E2`, ...), or
        unmark it, the labels closing up [F95 p. 114] (G-050)."""
        self._refuse_if_disabled()
        if self.object(id_) is None:
            raise KeyError(id_)
        if id_ in self.answers.values():
            self.answers = self._relabelled(id_)
        else:
            self.answers[f"E{len(self.answers) + 1}"] = id_
        self._changed()

    def set_names(self, name=None, title=None, family=None) -> None:
        """The file's name (its stem), title and family (one of the chapter 5 families,
        `FAMILIES`), each left as it is when None (G-050)."""
        self._refuse_if_disabled()
        if name is not None:
            self.name = name.strip() or DEFAULT_NAME
        if title is not None:
            self.title = title.strip() or f"Edited table ({self.name})"
        if family is not None:
            if family not in FAMILIES:
                raise ValueError(f"the family must be one of {FAMILIES}")
            self.family = family
        self._changed()

    def clear(self) -> None:
        """Clear table [F95 p. 96 (img)]: no objects, no touch, no answers, the default
        names (G-050)."""
        self._refuse_if_disabled()
        self.objects, self.touched, self.answers = [], None, {}
        self.name, self.title, self.family = DEFAULT_NAME, "Edited table", FAMILIES[0]
        self.based_on = None
        self._changed()

    def load(self, problem) -> None:
        """The table of a `Problem`, the library's or an edited one, into the editor
        (G-050): its objects, touch and answers; its name, title and family (a chapter 2
        family becomes the first of `FAMILIES`); `based_on` names the library problem
        loaded, or the one an edited table's file names."""
        self._refuse_if_disabled()
        self.objects = [DraftObject(o.id, o.type, round(o.x, 1), round(o.y, 1), o.orientation)
                        for o in problem.objects]
        self.touched = problem.touched
        self.answers = dict(problem.answers)
        self.name, self.title = problem.name, problem.title
        self.family = problem.family if problem.family in FAMILIES else FAMILIES[0]
        self.based_on = problem.name if problem.name in P.catalogue() else problem.based_on
        self._changed()

    # -- the file form (docs/problems.md section 10; G-050) --------------------------------

    def to_dict(self) -> dict:
        """The table in the file form of `problems/` (`docs/problems.md` section 10), with
        provenance in the GUI's own words: the source is the palette's page [F95 p. 26
        (img)], each object's box is computed from its position (`box_px_of`), the
        method, uncertainty and review say the table was set in the GUI (G-050). It is
        what `problems.validate` checks and `problems.problem_from_dict` reads."""
        data = {"name": self.name, "title": self.title, "family": self.family,
                "source": {"tag": "F95", "page": GUI_PAGE, "figure": GUI_FIGURE},
                "table": {"width": TABLE_WIDTH, "height": TABLE_HEIGHT, "frame_px": list(FRAME_PX)},
                "touched": self.touched, "answers": dict(self.answers),
                "objects": [{"id": o.id, "type": o.type, "x": o.x, "y": o.y,
                             "orientation": o.orientation, "box_px": box_px_of(o),
                             "figure": GUI_FIGURE, "page": GUI_PAGE, "image": GUI_IMAGE}
                            for o in self.objects],
                "method": METHOD, "uncertainty": dict(UNCERTAINTY),
                "notes": [T.describe(self.objects)] if self.objects else [],
                "review": REVIEW}
        if self.based_on:
            data["based_on"] = self.based_on
        return data

    def messages(self) -> tuple:
        """`problems.validate`'s messages on the table as it stands, each followed by
        the GUI's hint where it has one (`HINTS`); empty when the table can run
        (G-051)."""
        out = []
        for message in P.validate(self.to_dict()):
            out.append(message)
            if message in HINTS:
                out.append("  " + HINTS[message])
        return tuple(out)

    def faults(self) -> tuple:
        """`tables.realism_faults` on the objects: how the table breaks the rules a set
        table obeys [F95 p. 38] (D-068); empty when French would set it (G-051)."""
        return tuple(T.realism_faults(self.objects))

    def status(self) -> EditorStatus:
        """The messages, the faults and one line in words (G-051)."""
        messages, faults = self.messages(), self.faults()
        n = len(self.objects)
        head = f"{n} object{'s' if n != 1 else ''}"
        if messages:
            text = f"{head}; cannot run: {messages[0]}"
        elif faults:
            text = f"{head}; can run, but not a table French would set: {faults[0]}"
        else:
            text = f"{head}; can run; a table French would set"
        return EditorStatus(messages, faults, text)

    def problem(self) -> P.Problem:
        """The table as a `Problem`, through `problems.problem_from_dict` after
        `problems.validate` returns no error; otherwise `ValueError` with the messages
        (the engine contract; G-050)."""
        data = self.to_dict()
        errors = P.validate(data)
        if errors:
            raise ValueError("the table cannot run: " + "; ".join(errors))
        return P.problem_from_dict(data)

    def save(self, path) -> Path:
        """Save config [F95 p. 96 (img)]: the dict as JSON at `path`, which must lie
        outside `problems/` (G-050). Returns the path."""
        path = Path(path)
        if under_problems_dir(path):
            raise ValueError(f"not into the library's folder: {path}")
        path.write_text(json.dumps(self.to_dict(), indent=2) + "\n", encoding="utf-8")
        return path

    def load_file(self, path) -> Path:
        """Load config [F95 p. 96 (img)]: a problem file (one saved here, or the
        library's), validated, into the editor (G-050). Raises `ValueError` for a file
        that does not validate."""
        path = Path(path)
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except ValueError as error:
            raise ValueError(f"{path.name} is not a problem file: {error}") from None
        if not isinstance(data, dict):
            raise ValueError(f"{path.name} is not a problem file")
        errors = P.validate(data)
        if errors:
            raise ValueError(f"{path.name}: " + "; ".join(errors))
        self.load(P.problem_from_dict(data))
        return path
