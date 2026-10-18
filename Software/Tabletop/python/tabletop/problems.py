"""Loading, validating and listing the problems: the digitized table configurations (D-003).

A problem is a JSON file in `problems/` (its stem is the problem's name: `surround-05`,
`ch2-fig11`).
French's program took "the coordinates and orientation of each placed object, along with its
category membership" and "an indication of which object was touched by Henry" as its raw
input [FCCA p. 379]; a file records exactly that, plus the labelled answers of the figure
("an arrow marked H", "E1, E2, and so on" [F95 p. 114]) and, for every object, the figure,
printed page and extracted image it was measured on, with the method and the measured
uncertainty (ANSWERS.md 4; `docs/problems.md`). Chapter 2's nineteen configurations carry
no E arrows and no statistics, only French's expected answer in words [F95 pp. 26-35]; their
files have an empty `answers` and an `expected` block quoting him (`docs/problems.md` 9).
Coordinates are table units (D-001: 110 x
100, origin at Henry's left corner, y towards Eliza); each object also keeps the pixel box
it was read from, and `validate` recomputes its position from that box and the frame so
that the file cannot drift from its measurement.

This module and `reported.py` are the only engine modules allowed to name a problem; the
engine itself never does (`tests/test_conventions.py`).
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass
from functools import cached_property
from pathlib import Path

from tabletop.geometry import TABLE_HEIGHT, TABLE_WIDTH
from tabletop.objects import ORIENTATIONS, SILVERWARE, TYPES, id_stem

PROBLEMS_DIR = Path(__file__).resolve().parent.parent / "problems"

FAMILIES = ("surround", "blockage", "buridan", "figure-ground", "ch2")  # [F95 p. 114]; ch. 2

ANSWER_LABEL = re.compile(r"^E\d$")  # "E1, E2, and so on" [F95 p. 114]
ID = re.compile(r"^([a-z]+(?:-[a-z]+)?)(\d+)$")
F95_PAGES = (1, 190)


@dataclass(frozen=True)
class Provenance:
    """Where one object was measured: the figure's printed title, the printed page, and the
    extracted image (D-003; ANSWERS.md 4)."""

    figure: str
    page: int
    image: str


@dataclass(frozen=True)
class TableObject:
    """One object on the table: its id, type, position in table units, orientation (silverware
    only: up, down, left, right [F95 p. 55]), the pixel box it was read from and its
    provenance (D-003)."""

    id: str
    type: str
    x: float
    y: float
    orientation: str | None
    box_px: tuple[int, int, int, int]
    provenance: Provenance


@dataclass(frozen=True)
class Problem:
    """One digitized configuration: the objects, the touched object and the labelled answers
    of the figure [F95 p. 114], with the file's provenance, method and uncertainty (D-003)."""

    name: str
    title: str
    family: str
    source: dict
    width: float
    height: float
    frame_px: tuple[int, int, int, int]
    touched: str
    answers: dict[str, str]
    objects: tuple[TableObject, ...]
    method: str
    uncertainty: dict
    notes: tuple[str, ...]
    review: str
    based_on: str | None = None
    expected: dict | None = None

    def object(self, id_: str) -> TableObject:
        """The object with this id (D-003), through an index built on the first ask: the
        objects never change after loading (D-112)."""
        try:
            return self._by_id[id_]
        except KeyError:
            raise KeyError(id_) from None

    @cached_property
    def _by_id(self) -> dict:
        by_id = {}
        for o in self.objects:
            by_id.setdefault(o.id, o)       # the first of an id, as the scan found it
        return by_id

    def touched_object(self) -> TableObject:
        """The object Henry touched, marked H in the figure [F95 p. 114]."""
        return self.object(self.touched)

    def answer_object(self, label: str) -> TableObject:
        """The object the figure labels E1, E2, ... [F95 p. 114]."""
        return self.object(self.answers[label])

    @cached_property
    def _ids(self) -> tuple[str, ...]:
        return tuple(o.id for o in self.objects)

    def ids(self) -> tuple[str, ...]:
        """The object ids in file order (D-003), listed once: the objects never change
        after loading, and the engine asks on nearly every look at an item."""
        return self._ids

    def expected_object(self) -> TableObject | None:
        """The object French names as Eliza's most likely answer, for a chapter 2
        configuration whose text names one [F95 pp. 26-35]; None otherwise (D-003)."""
        if self.expected and self.expected.get("answer"):
            return self.object(self.expected["answer"])
        return None


# ---------------------------------------------------------------------------------------------
# The catalogue.


def catalogue(directory: Path = PROBLEMS_DIR) -> list[str]:
    """The names of every problem file, sorted (D-003)."""
    return sorted(p.stem for p in directory.glob("*.json"))


def by_family(family: str, directory: Path = PROBLEMS_DIR) -> list[str]:
    """The names of the problems of one family, in order [F95 p. 114]."""
    return [name for name in catalogue(directory) if name.rsplit("-", 1)[0] == family]


def problem_path(name: str, directory: Path = PROBLEMS_DIR) -> Path:
    """The file of a problem name; a path to a JSON file is returned as given (D-003)."""
    if name.endswith(".json"):
        return Path(name)
    return directory / f"{name}.json"


def load_problem(name: str, directory: Path = PROBLEMS_DIR) -> Problem:
    """Read, validate and build a problem; `ValueError` lists every fault found (D-003)."""
    path = problem_path(name, directory)
    data = json.loads(path.read_text())
    errors = validate(data)
    if errors:
        raise ValueError(f"{path.name}: " + "; ".join(errors))
    if data["name"] != path.stem:
        raise ValueError(f"{path.name}: the file's name is {data['name']!r}")
    return problem_from_dict(data)


def load_all(directory: Path = PROBLEMS_DIR) -> list[Problem]:
    """Every problem of the catalogue (D-003)."""
    return [load_problem(name, directory) for name in catalogue(directory)]


# ---------------------------------------------------------------------------------------------
# Building and validating.


def problem_from_dict(data: dict) -> Problem:
    """A `Problem` from a validated file's contents (D-003)."""
    objects = tuple(
        TableObject(o["id"], o["type"], float(o["x"]), float(o["y"]), o.get("orientation"),
                    tuple(o["box_px"]), Provenance(o["figure"], o["page"], o["image"]))
        for o in data["objects"])
    table = data["table"]
    return Problem(data["name"], data["title"], data["family"], data["source"],
                   float(table["width"]), float(table["height"]), tuple(table["frame_px"]),
                   data["touched"], dict(data["answers"]), objects, data["method"],
                   data["uncertainty"], tuple(data.get("notes", [])), data["review"],
                   data.get("based_on"), data.get("expected"))


REQUIRED = ("name", "title", "family", "source", "table", "touched", "answers", "objects",
            "method", "uncertainty", "review")
OBJECT_REQUIRED = ("id", "type", "x", "y", "orientation", "box_px", "figure", "page", "image")


def validate(data: dict) -> list[str]:
    """Every fault of a problem file's contents, as messages; empty when the file is good.

    Checks the file's keys and table (D-001); every object's keys, type [F95 p. 26 (img)],
    orientation (silverware only [F95 p. 55]), id (the type's stem and a number, unique),
    position inside the table and consistent with its pixel box and the frame (D-003), and
    provenance (a figure, a printed page of F95, an image; ANSWERS.md 4); the touched id and
    the answer labels [F95 p. 114]; the method and uncertainty.
    """
    errors = [f"missing key {k!r}" for k in REQUIRED if k not in data]
    if errors:
        return errors
    errors += _validate_table(data["table"])
    errors += _validate_source(data["source"])
    if data["family"] not in FAMILIES:
        errors.append(f"unknown family {data['family']!r}")
    ids = []
    for o in data["objects"]:
        errors += _validate_object(o, data["table"])
        if "id" in o:
            if o["id"] in ids:
                errors.append(f"duplicate id {o['id']!r}")
            ids.append(o["id"])
    if not ids:
        errors.append("no objects")
    if data["touched"] not in ids:
        errors.append(f"touched {data['touched']!r} is not an object")
    errors += _validate_answers(data, ids)
    if not isinstance(data["method"], str) or len(data["method"]) < 40:
        errors.append("the method must be described")
    unc = data["uncertainty"]
    if not isinstance(unc, dict) or not isinstance(unc.get("units"), (int, float)) \
            or not unc.get("note"):
        errors.append("the uncertainty needs 'units' (a number) and a 'note'")
    if not isinstance(data["review"], str) or not data["review"]:
        errors.append("the review by eye must be recorded")
    return errors


def _validate_answers(data: dict, ids: list[str]) -> list[str]:
    """The labelled answers of a chapter 5 figure [F95 p. 114], or, for a chapter 2
    configuration (family `ch2`), the `expected` block in French's words (D-003)."""
    errors = []
    answers = data["answers"]
    for label, id_ in answers.items():
        if not ANSWER_LABEL.match(label):
            errors.append(f"bad answer label {label!r}")
        if id_ not in ids:
            errors.append(f"answer {label} names {id_!r}, not an object")
    if len(set(answers.values())) != len(answers):
        errors.append("two answer labels name the same object")
    if data["family"] == "ch2":
        if "expected" not in data:
            errors.append("a chapter 2 configuration needs an 'expected' block")
        else:
            errors += _validate_expected(data["expected"], ids)
    elif not answers:
        errors.append("no answers")
    return errors


def _validate_expected(exp, ids: list[str]) -> list[str]:
    """French's expectation for a chapter 2 configuration: `answer` (the id he names as the
    most likely answer, or null where he only states a direction), `plausible` (the ids he
    names as candidates), `unlikely`, `quotes` (his words, each with its printed page, checked
    against the text layer by `tests/test_ch2.py`) and `comparisons` (`toward` an id,
    `more_than` or `less_than` another chapter 2 problem) [F95 pp. 26-35] (D-003)."""
    if not isinstance(exp, dict):
        return ["expected is not an object"]
    errors = [f"expected is missing {k!r}" for k in ("answer", "plausible", "unlikely", "quotes",
                                                      "comparisons") if k not in exp]
    if errors:
        return errors
    if exp["answer"] is not None and exp["answer"] not in ids:
        errors.append(f"expected.answer {exp['answer']!r} is not an object")
    for key in ("plausible", "unlikely"):
        if not isinstance(exp[key], list) or any(i not in ids for i in exp[key]):
            errors.append(f"expected.{key} must list object ids")
    if not exp["plausible"]:
        errors.append("expected.plausible is empty")
    if exp["answer"] is not None and exp["answer"] not in exp["plausible"]:
        errors.append("expected.answer is not among expected.plausible")
    if not exp["quotes"]:
        errors.append("expected.quotes is empty")
    for q in exp["quotes"]:
        if not (isinstance(q, dict) and isinstance(q.get("text"), str)
                and len(q["text"].split()) >= 6 and isinstance(q.get("page"), int)
                and F95_PAGES[0] <= q["page"] <= F95_PAGES[1]):
            errors.append("each quote needs 'text' (six words or more) and a printed 'page'")
    for c in exp["comparisons"]:
        if not (isinstance(c, dict) and c.get("toward") in ids
                and len({"more_than", "less_than"} & set(c)) == 1
                and re.match(r"^ch2-fig\d\d$", c.get("more_than", c.get("less_than", "")))):
            errors.append("each comparison needs 'toward' (an id) and one of 'more_than' "
                          "or 'less_than' (a chapter 2 problem)")
    return errors


def _validate_table(table) -> list[str]:
    """The table block: the D-001 size and the frame's pixel box."""
    errors = []
    if not isinstance(table, dict):
        return ["table is not an object"]
    if table.get("width") != TABLE_WIDTH or table.get("height") != TABLE_HEIGHT:
        errors.append(f"the table is {TABLE_WIDTH:g} x {TABLE_HEIGHT:g} units (D-001)")
    frame = table.get("frame_px")
    if not (isinstance(frame, list) and len(frame) == 4 and all(isinstance(v, int) for v in frame)
            and frame[0] < frame[2] and frame[1] < frame[3]):
        errors.append("frame_px must be [left, top, right, bottom] in pixels")
    return errors


def _validate_source(source) -> list[str]:
    """The file's source block: a tag, a printed page and a figure title."""
    if not isinstance(source, dict):
        return ["source is not an object"]
    errors = []
    if source.get("tag") not in ("F95", "FCCA", "FH91"):
        errors.append("source.tag must be F95, FCCA or FH91")
    if not isinstance(source.get("page"), int):
        errors.append("source.page must be a printed page number")
    if not source.get("figure"):
        errors.append("source.figure must name the figure")
    return errors


def _validate_object(o, table) -> list[str]:
    """One object record: keys, type, orientation, id, position, box and provenance."""
    errors = [f"object missing key {k!r}" for k in OBJECT_REQUIRED if k not in o]
    if errors:
        return errors
    tag = f"object {o['id']!r}"
    if o["type"] not in TYPES:
        errors.append(f"{tag}: unknown type {o['type']!r}")
        return errors
    if o["type"] in SILVERWARE:
        if o["orientation"] not in ORIENTATIONS:
            errors.append(f"{tag}: silverware needs an orientation in {ORIENTATIONS}")
    elif o["orientation"] is not None:
        errors.append(f"{tag}: only silverware has an orientation")
    m = ID.match(o["id"])
    if not m or m.group(1) != id_stem(o["type"]):
        errors.append(f"{tag}: the id must be the type's stem and a number")
    x, y = o["x"], o["y"]
    if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
        errors.append(f"{tag}: x and y must be numbers")
    elif not (0 <= x <= table.get("width", 0) and 0 <= y <= table.get("height", 0)):
        errors.append(f"{tag}: ({x}, {y}) is off the table")
    box = o["box_px"]
    frame = table.get("frame_px")
    if not (isinstance(box, list) and len(box) == 4 and all(isinstance(v, int) for v in box)
            and box[0] <= box[2] and box[1] <= box[3]):
        errors.append(f"{tag}: box_px must be [minx, miny, maxx, maxy]")
    elif isinstance(frame, list) and len(frame) == 4 and isinstance(x, (int, float)):
        ex, ey = position_from_box(box, frame, table["width"], table["height"])
        if abs(ex - x) > 0.06 or abs(ey - y) > 0.06:
            errors.append(f"{tag}: ({x}, {y}) is not the centre of its box, "
                          f"({ex:.1f}, {ey:.1f})")
        if not (frame[0] < box[0] and box[2] < frame[2] and frame[1] < box[1] and box[3] < frame[3]):
            errors.append(f"{tag}: the box lies outside the frame")
    if not isinstance(o["figure"], str) or not o["figure"]:
        errors.append(f"{tag}: the figure is missing")
    if not isinstance(o["page"], int) or not F95_PAGES[0] <= o["page"] <= F95_PAGES[1]:
        errors.append(f"{tag}: the page must be a printed page of F95")
    if not isinstance(o["image"], str) or "pdfimages" not in o["image"]:
        errors.append(f"{tag}: the image must say which extracted image it came from")
    return errors


def position_from_box(box, frame, width: float, height: float) -> tuple[float, float]:
    """The centre of a pixel box as table coordinates: fractions of the frame's inner width
    and height, scaled to the table (D-001, D-003)."""
    left, top, right, bottom = frame
    cx, cy = (box[0] + box[2]) / 2, (box[1] + box[3]) / 2
    return (round((cx - left) / (right - left) * width, 1),
            round((cy - top) / (bottom - top) * height, 1))
