"""The `variants` bench: eight variants of every problem of the catalogue, each run beside
the original with the same run seeds, and the distance between the two answer
distributions after the answers are relabelled through the variant's object map (D-102).

French reports two such probes himself: the "junk" of Surround 10, which lowers the small
glass's share by thirteen points [F95 p. 121], and the Figure/Ground table, where the
"figure object" stands out against a dull background [F95 p. 140]; he never reports a
mirrored, swapped or translated table. The variants are held-out data the program's
numbers were never read against, and the bench is the guard against fitting the geometry:

| Variant | What it does to the file |
|---|---|
| mirrored | reflects every object across the table's vertical midline (x to width minus x); left and right silverware swap |
| side-swapped | reflects across the midline between the two people (y to height minus y); up and down swap; the touched object moves with the rest |
| rotated | both reflections: point symmetry through the table's centre |
| translated | shifts every object by half a glass width in x and in y, each axis in the direction with room (D-004's blur moves a few region draws) |
| type-substituted | swaps every object of a type for its partner type within D-009's categories (small glass and big glass, fork and spoon, salt and pepper shaker, cup and soup bowl, saucer and plate), every object of the type at once, so that sameness is kept and only the footprints and the Slipnet distances change |
| junk-added | stands one stray salt shaker in the band between the rows where it is clearest of every object, or in the far corner when the band has no room |
| decoupled | moves the structured row on the other side of the table (its largest cluster by the proximity criterion, D-022) one column third sideways, so that the touched object's mirror image is no longer where it was while the row's composition is kept |
| on-saucer | stands a cup on a saucer, a cup that is not the touched one when one has room |

Every variant is a problem file in the format of D-003, relabelled through its object map
(answers, the touched object, chapter 2's `expected` block), and passes
`problems.validate`: the pixel boxes are transformed with the positions, so that the file
cannot drift from its own measurement. The original and the variant are run with the same
run seeds (the problem's batch seed, D-072), so that a run and its counterpart are paired:
an engine with no handedness gives the same relabelled answer in every pair of the three
exact variants. The measures, per problem and variant: the total-variation distance between
the two relabelled answer distributions with a bootstrap interval (seeded as D-072), the
chi-square of homogeneity (`stats.py`), the share of paired runs whose answer keeps its
role (French's label where the problem has one, the object otherwise), and the mechanism
measures beside the answers (the Worldview's size, its coherent and incoherent pairs, the
role tags per run). Nothing here sets a default (ANSWERS.md 1); a failure of exact
invariance is a finding, traced to a decision in RESULTS.md.
"""

from __future__ import annotations

import dataclasses
import json
from dataclasses import dataclass

from tabletop import objects as O
from tabletop import tables as TB
from tabletop import worldview as WV
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.bench import stats as ST
from tabletop.config import Config
from tabletop.geometry import gap_between, opposite_point
from tabletop.problems import (Problem, catalogue, load_problem, position_from_box,
                               problem_from_dict, problem_path, validate)
from tabletop.accel import Rng, run_seeds
from tabletop.run import result_of
from tabletop.world import World

SETS = tuple(catalogue())      # every problem of the catalogue, every family (D-102)
problems_of = P.problems_of

VARIANTS = ("mirrored", "side-swapped", "rotated", "translated", "type-substituted",
            "junk-added", "decoupled", "on-saucer")
EXACT = ("mirrored", "side-swapped", "rotated")   # the three the engine should not feel

# D-102: the bench's own values; none of them is an engine value.
TRANSLATION_UNITS = O.glass_width() / 2          # "half a glass width" [F95 p. 128]
PARTNERS = (("small glass", "big glass"), ("fork", "spoon"), ("salt shaker", "pepper shaker"),
            ("cup", "soup bowl"), ("saucer", "plate"))   # within D-009's categories
JUNK_TYPE = "salt shaker"                         # Surround 10's junk has shakers [F95 p. 121]
JUNK_CLEARANCE = 2.0                              # units kept clear, as the focus bench keeps
JUNK_CORNER_INSET = 10.0                          # units from the table's corner
CLUSTER_GAP = 2.5 * O.glass_width()               # D-022's zero point: beyond it, never linked
BOOTSTRAP_SAMPLES = 200
BOOTSTRAP_TAILS = (0.025, 0.975)
ADDED_NOTE = " (an object added by the variants bench, D-102)"


# ---------------------------------------------------------------------------------------------
# The variants, on the problem file's contents.


@dataclass(frozen=True)
class Variant:
    """One variant of a problem (D-102): the original's `problem` name, the variant's
    `name`, its file contents `data` (valid under `problems.validate`), the `mapping` from
    every original id to its id in the variant, the ids `added` by the variant (none in
    the mapping), and `params`, what was done, as plain data for the record."""

    problem: str
    name: str
    data: dict
    mapping: dict
    added: tuple
    params: dict

    def to_problem(self) -> Problem:
        """The variant as a `Problem` named `original+variant` (D-102)."""
        return problem_from_dict(self.data)

    def back(self, id_: str) -> str:
        """The original id behind an id of the variant; an added id maps to itself
        (D-102)."""
        for old, new in self.mapping.items():
            if new == id_:
                return old
        return id_


def problem_data(name: str) -> dict:
    """A problem file's contents, read as the loader reads them (D-003, D-102)."""
    return json.loads(problem_path(name).read_text())


def _frame(data: dict) -> list:
    return data["table"]["frame_px"]


def _reflect_x(box: list, frame: list) -> list:
    left, right = frame[0], frame[2]
    return [left + right - box[2], box[1], left + right - box[0], box[3]]


def _reflect_y(box: list, frame: list) -> list:
    top, bottom = frame[1], frame[3]
    return [box[0], top + bottom - box[3], box[2], top + bottom - box[1]]


def _shift(box: list, dx: int, dy: int) -> list:
    return [box[0] + dx, box[1] + dy, box[2] + dx, box[3] + dy]


def _fits(box: list, frame: list) -> bool:
    return frame[0] < box[0] and box[2] < frame[2] and frame[1] < box[1] and box[3] < frame[3]


def _px_per_unit(data: dict) -> float:
    frame = _frame(data)
    return (frame[2] - frame[0]) / data["table"]["width"]


def _placed(data: dict, objects: list) -> dict:
    """A copy of the file with these objects, each position recomputed from its box as
    the loader checks it (D-003, D-102)."""
    out = dict(data)
    table = data["table"]
    placed = []
    for o in objects:
        o = dict(o)
        o["x"], o["y"] = position_from_box(o["box_px"], table["frame_px"], table["width"],
                                           table["height"])
        placed.append(o)
    out["objects"] = placed
    return out


def _identity(data: dict) -> dict:
    return {o["id"]: o["id"] for o in data["objects"]}


def _relabelled(data: dict, mapping: dict, problem: str, variant: str) -> dict:
    """The file with every id written through the mapping: the objects, the touched
    object, the answers and chapter 2's `expected` block (D-003, D-059, D-102)."""
    out = dict(data)
    out["name"] = f"{problem}+{variant}"
    out["title"] = f"{data['title']} ({variant})"
    out["objects"] = [dict(o, id=mapping.get(o["id"], o["id"])) for o in data["objects"]]
    out["touched"] = mapping[data["touched"]]
    out["answers"] = {label: mapping[id_] for label, id_ in data["answers"].items()}
    exp = data.get("expected")
    if exp:
        exp = dict(exp)
        exp["answer"] = mapping.get(exp["answer"], exp["answer"]) if exp["answer"] else None
        exp["plausible"] = [mapping.get(i, i) for i in exp["plausible"]]
        exp["unlikely"] = [mapping.get(i, i) for i in exp["unlikely"]]
        exp["comparisons"] = [dict(c, toward=mapping.get(c["toward"], c["toward"]))
                              for c in exp["comparisons"]]
        out["expected"] = exp
    out["notes"] = list(data.get("notes", [])) + [f"{variant} variant of {problem} (D-102)"]
    return out


def _reflected(data: dict, in_x: bool, in_y: bool) -> list:
    """The objects reflected across the vertical midline, the horizontal one, or both;
    silverware pointing along a reflected axis turns round (D-102)."""
    frame = _frame(data)
    turn = {}
    if in_x:
        turn.update({"left": "right", "right": "left"})
    if in_y:
        turn.update({"up": "down", "down": "up"})
    out = []
    for o in data["objects"]:
        box = list(o["box_px"])
        if in_x:
            box = _reflect_x(box, frame)
        if in_y:
            box = _reflect_y(box, frame)
        orientation = turn.get(o["orientation"], o["orientation"])
        out.append(dict(o, box_px=box, orientation=orientation))
    return out


def mirrored(data: dict) -> tuple | None:
    """Left-right reflection: x to width minus x, in the pixel boxes (D-102). Applies to
    every problem; a mirror of a mirror is the original."""
    return _placed(data, _reflected(data, True, False)), _identity(data), (), {"axis": "x"}


def side_swapped(data: dict) -> tuple | None:
    """Henry's side for Eliza's: y to height minus y, the touched object moving with the
    rest (D-102). Applies to every problem; an involution."""
    return _placed(data, _reflected(data, False, True)), _identity(data), (), {"axis": "y"}


def rotated(data: dict) -> tuple | None:
    """Both reflections: point symmetry through the table's centre, the "diagonal
    symmetry" of the Slipnet [F95 p. 63] (D-102). Applies to every problem; an involution."""
    return _placed(data, _reflected(data, True, True)), _identity(data), (), {"axis": "xy"}


def _room(boxes: list, frame: list, axis: int, px: int) -> int:
    """The signed shift along an axis (0 for x, 1 for y) of `px` pixels that keeps every
    box inside the frame, the positive direction first; 0 when neither fits (D-102)."""
    for sign in (1, -1):
        d = sign * px
        if all(_fits(_shift(b, d if axis == 0 else 0, d if axis == 1 else 0), frame)
               for b in boxes):
            return d
    return 0


def translated(data: dict) -> tuple | None:
    """Every object shifted by half a glass width in x and in y, each axis in the
    direction with room (D-102; the blur of D-004 is the only thing that should feel it).
    Not applicable when neither axis has room."""
    frame = _frame(data)
    px = max(1, round(TRANSLATION_UNITS * _px_per_unit(data)))
    boxes = [o["box_px"] for o in data["objects"]]
    dx, dy = _room(boxes, frame, 0, px), _room(boxes, frame, 1, px)
    if dx == 0 and dy == 0:
        return None
    objects = [dict(o, box_px=_shift(o["box_px"], dx, dy)) for o in data["objects"]]
    units = 1.0 / _px_per_unit(data)
    return (_placed(data, objects), _identity(data), (),
            {"dx_px": dx, "dy_px": dy, "dx_units": round(dx * units, 1),
             "dy_units": round(dy * units, 1)})


def partner_types() -> dict:
    """Each type's partner under the substitution, both ways (D-009, D-102)."""
    out = {}
    for a, b in PARTNERS:
        out[a], out[b] = b, a
    return out


def type_substituted(data: dict) -> tuple | None:
    """Every object of a type with a partner becomes the partner type, every object of the
    type at once, the number of its id kept (`small-glass1` to `big-glass1`) (D-009,
    D-102). The swap is a bijection of types, so sameness among the objects is kept."""
    partners = partner_types()
    pairs = sorted({tuple(sorted((o["type"], partners[o["type"]])))
                    for o in data["objects"] if o["type"] in partners})
    if not pairs:
        return None
    mapping, objects = {}, []
    for o in data["objects"]:
        if o["type"] in partners:
            new_type = partners[o["type"]]
            number = o["id"][len(O.id_stem(o["type"])):]
            new_id = O.id_stem(new_type) + number
            mapping[o["id"]] = new_id
            objects.append(dict(o, type=new_type))   # the id is written by `_relabelled`
        else:
            mapping[o["id"]] = o["id"]
            objects.append(dict(o))
    out = dict(data)
    out["objects"] = objects
    return out, mapping, (), {"pairs": [list(p) for p in pairs]}


def _footprint_box(type_: str, x: float, y: float) -> tuple:
    w, h = O.footprint(type_)
    return (x - w / 2, y - h / 2, x + w / 2, y + h / 2)


def _clearance(type_: str, x: float, y: float, data: dict) -> float:
    """The least white space between a footprint centred here and every object's; negative
    when something overlaps it (D-002, D-102)."""
    mine = _footprint_box(type_, x, y)
    return min(gap_between(mine, O.box(_Obj(o))) for o in data["objects"])


class _Obj:
    """An object record read as `objects.box` reads a `TableObject` (D-102)."""

    def __init__(self, o: dict):
        self.type, self.orientation, self.x, self.y = o["type"], o["orientation"], o["x"], o["y"]


def _inside(type_: str, x: float, y: float, data: dict) -> bool:
    l, t, r, b = _footprint_box(type_, x, y)
    return l >= 0 and t >= 0 and r <= data["table"]["width"] and b <= data["table"]["height"]


def _box_at(data: dict, type_: str, x: float, y: float) -> list:
    """The pixel box of a type's icon centred at a table position (D-002, D-102)."""
    frame = _frame(data)
    scale = _px_per_unit(data)
    w_px, h_px = O.FOOTPRINTS_PX[type_]
    cx = frame[0] + x * scale
    cy = frame[1] + y * (frame[3] - frame[1]) / data["table"]["height"]
    left, top = round(cx - w_px / 2), round(cy - h_px / 2)
    return [left, top, left + w_px, top + h_px]


def _added(data: dict, type_: str, x: float, y: float) -> dict:
    """An object added by the bench: its id numbered after the type's last, its box drawn
    at the position, its provenance the figure the others were measured on, marked as
    added (D-003, D-102)."""
    stem = O.id_stem(type_)
    count = sum(1 for o in data["objects"] if o["type"] == type_)
    touched = next(o for o in data["objects"] if o["id"] == data["touched"])
    return {"id": f"{stem}{count + 1}", "type": type_, "x": x, "y": y,
            "orientation": "up" if type_ in O.SILVERWARE else None,
            "box_px": _box_at(data, type_, x, y), "figure": touched["figure"],
            "page": touched["page"], "image": touched["image"] + ADDED_NOTE}


def junk_place(data: dict) -> tuple | None:
    """Where the stray stands (D-102): in the band between the rows, at the x with the
    most clearance from every object, when that clearance is at least `JUNK_CLEARANCE`;
    otherwise in the corner farthest from the touched object with that clearance; None
    when nowhere has it."""
    width, height = data["table"]["width"], data["table"]["height"]
    y = TB.MIDDLE_Y
    best, best_x = -1.0, None
    x = JUNK_CORNER_INSET
    while x <= width - JUNK_CORNER_INSET:
        if _inside(JUNK_TYPE, x, y, data):
            c = _clearance(JUNK_TYPE, x, y, data)
            if c > best:
                best, best_x = c, x
        x += 1.0
    if best >= JUNK_CLEARANCE:
        return best_x, y, "band"
    touched = next(o for o in data["objects"] if o["id"] == data["touched"])
    corners = [(JUNK_CORNER_INSET, JUNK_CORNER_INSET), (width - JUNK_CORNER_INSET, JUNK_CORNER_INSET),
               (JUNK_CORNER_INSET, height - JUNK_CORNER_INSET),
               (width - JUNK_CORNER_INSET, height - JUNK_CORNER_INSET)]
    corners.sort(key=lambda c: -((c[0] - touched["x"]) ** 2 + (c[1] - touched["y"]) ** 2))
    for cx, cy in corners:
        if _inside(JUNK_TYPE, cx, cy, data) and _clearance(JUNK_TYPE, cx, cy, data) >= JUNK_CLEARANCE:
            return cx, cy, "corner"
    return None


def junk_added(data: dict) -> tuple | None:
    """One stray salt shaker added where `junk_place` puts it, as Surround 10 adds junk
    [F95 p. 121] (D-102)."""
    place = junk_place(data)
    if place is None:
        return None
    x, y, where = place
    stray = _added(data, JUNK_TYPE, x, y)
    if not _fits(stray["box_px"], _frame(data)):
        return None
    out = _placed(data, list(data["objects"]) + [stray])
    return out, _identity(data), (stray["id"],), {"added": stray["id"], "where": where,
                                                  "x": out["objects"][-1]["x"],
                                                  "y": out["objects"][-1]["y"]}


def clusters_along_x(objects: list) -> list:
    """The objects split into clusters by the proximity criterion along the row: a gap
    of white space wider than `CLUSTER_GAP` starts a new cluster (D-022, D-102)."""
    ordered = sorted(objects, key=lambda o: o["x"])
    out = []
    for o in ordered:
        if out and O.edge_gap(_Obj(out[-1][-1]), _Obj(o)) <= CLUSTER_GAP:
            out[-1].append(o)
        else:
            out.append([o])
    return out


def other_row(data: dict) -> list:
    """The objects of the row the touched object is not in, by D-068's bands (D-102)."""
    touched = next(o for o in data["objects"] if o["id"] == data["touched"])
    mine = TB.band_of(touched["y"])
    other = {"henry": "eliza", "eliza": "henry"}.get(mine)
    if other is None:
        return []
    return [o for o in data["objects"] if TB.band_of(o["y"]) == other]


def decoupled(data: dict) -> tuple | None:
    """The structured row on the other side (its largest cluster) moved one column third
    sideways, in the direction where it stays inside the frame and overlaps nothing that
    does not move, so that the touched object's mirror image moves while the row's
    composition is kept (D-102). Not applicable when the other row is empty or neither
    direction has room."""
    row = other_row(data)
    if not row:
        return None
    cluster = max(clusters_along_x(row), key=len)
    moved = {o["id"] for o in cluster}
    frame = _frame(data)
    px = round((frame[2] - frame[0]) / 3)
    still = [o for o in data["objects"] if o["id"] not in moved]
    for sign in (1, -1):
        dx = sign * px
        boxes = [_shift(o["box_px"], dx, 0) for o in cluster]
        if not all(_fits(b, frame) for b in boxes):
            continue
        units = dx / _px_per_unit(data)
        if any(TB.overlap_depth(_Obj(dict(m, x=m["x"] + units)), _Obj(s)) > TB.OVERLAP_SLOP
               for m in cluster for s in still):
            continue
        objects = [dict(o, box_px=_shift(o["box_px"], dx, 0)) if o["id"] in moved else dict(o)
                   for o in data["objects"]]
        return (_placed(data, objects), _identity(data), (),
                {"moved": [o["id"] for o in cluster], "shift_px": dx,
                 "shift_units": round(units, 1)})
    return None


def on_saucer(data: dict) -> tuple | None:
    """A cup stood on a saucer (D-102): the first cup in file order that is not the
    touched one and has room for a saucer's footprint (overlapping nothing but the cup),
    else the touched cup when it has room; the saucer follows the cup in the file. Not
    applicable without such a cup."""
    cups = [o for o in data["objects"] if o["type"] == "cup"]
    cups.sort(key=lambda o: o["id"] == data["touched"])
    for cup in cups:
        saucer = _added(data, "saucer", cup["x"], cup["y"])
        if not _fits(saucer["box_px"], _frame(data)):
            continue
        others = [o for o in data["objects"] if o["id"] != cup["id"]]
        if any(TB.overlap_depth(_Obj(saucer), _Obj(o)) > TB.OVERLAP_SLOP for o in others):
            continue
        objects = []
        for o in data["objects"]:
            objects.append(dict(o))
            if o["id"] == cup["id"]:
                objects.append(saucer)
        out = _placed(data, objects)
        return out, _identity(data), (saucer["id"],), {"cup": cup["id"], "saucer": saucer["id"]}
    return None


MAKERS = {"mirrored": mirrored, "side-swapped": side_swapped, "rotated": rotated,
          "translated": translated, "type-substituted": type_substituted,
          "junk-added": junk_added, "decoupled": decoupled, "on-saucer": on_saucer}


def variant_of(name: str, variant: str, data: dict | None = None) -> Variant | None:
    """A named variant of a problem, or None where it does not apply (D-102). The result
    passes `problems.validate`; a fault in it is a bug and raises."""
    data = problem_data(name) if data is None else data
    made = MAKERS[variant](data)
    if made is None:
        return None
    changed, mapping, added, params = made
    out = _relabelled(changed, mapping, name, variant)
    faults = validate(out)
    if faults:
        raise ValueError(f"{name} {variant}: " + "; ".join(faults))
    return Variant(name, variant, out, mapping, tuple(added), params)


def variants_of(name: str) -> list:
    """Every variant that applies to a problem, in `VARIANTS` order (D-102)."""
    data = problem_data(name)
    out = []
    for v in VARIANTS:
        made = variant_of(name, v, data)
        if made is not None:
            out.append(made)
    return out


# ---------------------------------------------------------------------------------------------
# The runs and the measures.


@dataclass(frozen=True)
class Run:
    """One run's facts (D-102): the `result` and the mechanism measures read off the
    finished world: the Worldview's size (`worldview`), its `coherent` and `incoherent`
    pairs (D-042) and the role `tags` standing at the end."""

    result: object
    worldview: int
    coherent: int
    incoherent: int
    tags: int


def role_tags(world) -> int:
    """How many role tags stand in the Workspace's pool at the end of a run (D-102,
    D-079)."""
    return len(getattr(world.workspace, "roles", ()))   # 0 on the v1 engine, which has no pool


def run_facts(problem: Problem, config: Config, seed: int) -> Run:
    """One whole run from a fresh world, as `run_problem` makes it, with the mechanism
    measures read before the world is dropped (D-064, D-102)."""
    world = World(problem, config, seed)
    while not world.finished:
        world.step()
    cps = world.workspace.worldview()
    coherent, incoherent = WV.coherence_pairs(world, cps)
    return Run(result_of(world), len(cps), len(coherent), len(incoherent), role_tags(world))


def mechanism(runs: list) -> dict:
    """The mechanism measures over a batch (D-102): the mean Worldview size (`wv_mean`),
    the share of runs with two or more correspondences in it, in percent (`wv2`), the mean
    `coherent` and `incoherent` pairs (D-042), the mean role `tags`."""
    n = len(runs)
    return {"wv_mean": round(sum(r.worldview for r in runs) / n, 3),
            "wv2": round(100.0 * sum(r.worldview >= 2 for r in runs) / n, 1),
            "coherent": round(sum(r.coherent for r in runs) / n, 3),
            "incoherent": round(sum(r.incoherent for r in runs) / n, 3),
            "tags": round(sum(r.tags for r in runs) / n, 3)}


def answer_counts(answers: list) -> dict:
    """How often each answer came up, in the order the answers first came (D-102)."""
    out: dict = {}
    for a in answers:
        out[a] = out.get(a, 0) + 1
    return out


def total_variation(a: dict, b: dict) -> float:
    """The total-variation distance between two answer distributions given as counts:
    half the sum over every answer of the absolute difference of the shares (D-102)."""
    na, nb = sum(a.values()), sum(b.values())
    keys = list(a) + [k for k in b if k not in a]
    return 0.5 * sum(abs(a.get(k, 0) / na - b.get(k, 0) / nb) for k in keys)


def bootstrap_interval(xs: list, ys: list, rng: Rng, samples: int = BOOTSTRAP_SAMPLES) -> tuple:
    """A percentile bootstrap interval for the total-variation distance between the
    distributions behind two lists of answers: each list is resampled with replacement
    `samples` times through the generator, and the 2.5 and 97.5 percentiles of the
    distances are returned (nearest rank) (D-064, D-102)."""
    distances = []
    for _ in range(samples):
        a = answer_counts([xs[rng.below(len(xs))] for _ in xs])
        b = answer_counts([ys[rng.below(len(ys))] for _ in ys])
        distances.append(total_variation(a, b))
    distances.sort()
    lo, hi = BOOTSTRAP_TAILS
    pick = lambda q: distances[min(len(distances) - 1, max(0, round(q * len(distances)) - 1))]
    return round(pick(lo), 4), round(pick(hi), 4)


def role_of(answer: str, labels: dict) -> str:
    """What an answer counts as when the question is whether it kept its role (D-102):
    French's label where the problem labels the object [F95 p. 114], the object itself
    otherwise."""
    for label, id_ in labels.items():
        if id_ == answer:
            return label
    return answer


def role_kept(original: list, variant_back: list, labels: dict) -> float:
    """The share, in percent, of paired runs whose relabelled answer carries the same role
    as the original run's (D-102)."""
    pairs = list(zip(original, variant_back))
    same = sum(role_of(a, labels) == role_of(b, labels) for a, b in pairs)
    return round(100.0 * same / len(pairs), 1)


@dataclass(frozen=True)
class VariantResult:
    """A variant run beside its original (D-102): the `variant`, its `survey` (the
    answers written back through the map), the `mechanism` measures, the distance `tv`
    with its bootstrap `interval`, the chi-square (`chi_square`, `df`, `p`) and the share
    `kept`."""

    variant: Variant
    survey: H.Survey
    mechanism: dict
    counts: dict
    tv: float
    interval: tuple
    chi_square: float
    df: int
    p: float
    kept: float


@dataclass(frozen=True)
class ProblemVariants:
    """One problem's original survey and every variant's result (D-102); `skipped` lists
    the variants that do not apply with the reason."""

    problem: Problem
    survey: H.Survey
    mechanism: dict
    counts: dict
    variants: tuple
    skipped: tuple


def _survey(name: str, config: Config, seed: int, batch_seed: int, runs: list,
            problem) -> H.Survey:
    results = [r.result for r in runs]
    return H.Survey(name, config.profile, seed, batch_seed, len(results),
                    H.tallies_of(results, problem), sum(r.capped for r in results),
                    sum(r.codelets for r in results) / len(results),
                    tuple((r.seed, r.answer, r.structure_value, r.codelets, r.capped)
                          for r in results))


class _Labels:
    """A problem's labels written back to the original's ids, for the tallies of a
    variant's survey (D-102)."""

    def __init__(self, answers: dict):
        self.answers = answers


def survey_variant(original: Problem, sv_runs: list, v: Variant, config: Config, seed: int,
                   batch_seed: int, runs: int) -> VariantResult:
    """Run a variant with the original's run seeds and measure it beside the original's
    runs (D-072, D-102)."""
    problem = v.to_problem()
    v_runs = [run_facts(problem, config, s) for s in run_seeds(batch_seed, runs)]
    original_answers = [r.result.answer for r in sv_runs]
    back = [v.back(r.result.answer) for r in v_runs]
    a, b = answer_counts(original_answers), answer_counts(back)
    keys = list(a) + [k for k in b if k not in a]
    statistic, df = ST.chi_square_homogeneity([a.get(k, 0) for k in keys],
                                              [b.get(k, 0) for k in keys])
    rng = Rng(H.batch_seed_for(seed, f"{v.problem}+{v.name} bootstrap"))
    interval = bootstrap_interval(original_answers, back, rng)
    written_back = [Run(_rewritten(r.result, v), r.worldview, r.coherent, r.incoherent, r.tags)
                    for r in v_runs]
    survey = _survey(f"{v.problem}+{v.name}", config, seed, batch_seed, written_back,
                     _Labels(original.answers))
    return VariantResult(v, survey, mechanism(v_runs), b, round(total_variation(a, b), 4),
                         interval, round(statistic, 3), df, round(ST.chi_square_p(statistic, df), 4),
                         role_kept(original_answers, back, original.answers))


def _rewritten(result, v: Variant):
    """A variant's result with its answer written back to the original's id (D-102)."""
    return dataclasses.replace(result, answer=v.back(result.answer))


def planned(sets=SETS, only: list | None = None) -> int:
    """How many surveys the bench will run: each problem and every variant that applies
    to it (D-102)."""
    return sum(1 + len(variants_of(name)) for name in problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only: list | None = None, progress=None,
        sets=SETS) -> list:
    """Survey every problem (or those of `only`) and each of its variants with the same
    run seeds (D-072, D-102); `progress` is called with each finished survey."""
    out = []
    for name in problems_of(sets, only):
        problem = load_problem(name)
        batch_seed = H.batch_seed_for(seed, name)
        sv_runs = [run_facts(problem, config, s) for s in run_seeds(batch_seed, runs)]
        survey = _survey(name, config, seed, batch_seed, sv_runs, problem)
        if progress is not None:
            progress(survey)
        results, skipped = [], []
        data = problem_data(name)
        for v_name in VARIANTS:
            v = variant_of(name, v_name, data)
            if v is None:
                skipped.append((v_name, "does not apply: no room or no such object"))
                continue
            vr = survey_variant(problem, sv_runs, v, config, seed, batch_seed, runs)
            results.append(vr)
            if progress is not None:
                progress(vr.survey)
        out.append(ProblemVariants(problem, survey, mechanism(sv_runs),
                                   answer_counts([r.result.answer for r in sv_runs]),
                                   tuple(results), tuple(skipped)))
    return out


# ---------------------------------------------------------------------------------------------
# The report and the records.


def _main(counts: dict, labels: dict) -> str:
    answer = max(counts, key=counts.get)
    label = role_of(answer, labels)
    share = 100.0 * counts[answer] / sum(counts.values())
    who = f"{answer} ({label})" if label != answer else answer
    return f"{who} {share:.0f}%"


def summary_lines(results: list) -> list:
    """One line per variant over every problem it applies to: the mean and largest
    distance, how many problems differ at chi-square p below 0.01, the mean share of
    runs that kept their role (D-102)."""
    lines = ["| Variant | Problems | Mean TV | Largest TV (problem) | p < 0.01 | "
             "Mean role kept, % |", "|---|---:|---:|---|---:|---:|"]
    for name in VARIANTS:
        rows = [(pv.problem.name, vr) for pv in results for vr in pv.variants
                if vr.variant.name == name]
        if not rows:
            lines.append(f"| {name} | 0 | - | - | - | - |")
            continue
        worst = max(rows, key=lambda r: r[1].tv)
        lines.append(f"| {name} | {len(rows)} | {sum(r.tv for _, r in rows) / len(rows):.3f} | "
                     f"{worst[1].tv:.3f} ({worst[0]}) | {sum(r.p < 0.01 for _, r in rows)} | "
                     f"{sum(r.kept for _, r in rows) / len(rows):.1f} |")
    return lines


def problem_lines(pv: ProblemVariants) -> list:
    """One problem's table: a row per variant with the distance, its interval, the
    chi-square p, the share kept, the main answers and the Worldview measure (D-102)."""
    labels = pv.problem.answers
    lines = [f"**{pv.problem.name}**: {pv.survey.runs} runs, main answer "
             f"{_main(pv.counts, labels)}, Worldview 2+ {pv.mechanism['wv2']:.1f}%, "
             f"coherent pairs {pv.mechanism['coherent']:.2f}, "
             f"incoherent {pv.mechanism['incoherent']:.2f}, "
             f"role tags {pv.mechanism['tags']:.2f}, "
             f"mean codelets {pv.survey.codelets:.1f}, {pv.survey.capped} capped.", "",
             "| Variant | TV | 95% interval | chi-square p | Role kept, % | Main answer, variant | "
             "Worldview 2+, % | Mean codelets | Capped |", "|---|---:|---|---:|---:|---|---:|---:|---:|"]
    for vr in pv.variants:
        lo, hi = vr.interval
        lines.append(f"| {vr.variant.name} | {vr.tv:.3f} | [{lo:.3f}, {hi:.3f}] | {vr.p:.3f} | "
                     f"{vr.kept:.1f} | {_main(vr.counts, labels)} | "
                     f"{vr.mechanism['wv2']:.1f} | {vr.survey.codelets:.1f} | "
                     f"{vr.survey.capped} |")
    for name, reason in pv.skipped:
        lines.append(f"| {name} | - | - | - | - | {reason} | - | - | - |")
    return lines + [""]


def report(results: list) -> list:
    """The bench's text: the summary over every problem, then one table per problem
    (D-102)."""
    lines = ["## The variants bench: eight variants of every problem [F95 pp. 121, 140] (D-102)", ""]
    lines += summary_lines(results) + [""]
    for pv in results:
        lines += problem_lines(pv)
    return lines[:-1]


def _short_mapping(v: Variant) -> dict:
    return {old: new for old, new in v.mapping.items() if old != new}


def records(results: list) -> list:
    """The JSON records (D-102): per problem one `original` record (the profile, the bench
    seed, the survey, the answer counts, the mechanism measures, every run), then one
    `variant` record per variant with its parameters, the ids it changed or added, the
    answers written back to the original's ids, the measures and every run (the profile
    and the bench seed are the original's); a variant that does not apply is a record
    with `applies` false."""
    out = []
    for pv in results:
        sv = pv.survey
        out.append({"kind": "original", "problem": sv.problem, "profile": sv.profile,
                    "seed": sv.seed, "batch_seed": sv.batch_seed, "runs": sv.runs,
                    "capped": sv.capped, "mean_codelets": round(sv.codelets, 2),
                    "answers": pv.counts, "mechanism": pv.mechanism,
                    "results": [list(r) for r in sv.results]})
        for vr in pv.variants:
            v, s = vr.variant, vr.survey
            out.append({"kind": "variant", "problem": v.problem, "variant": v.name,
                        "applies": True, "params": v.params, "mapping": _short_mapping(v),
                        "added": list(v.added), "batch_seed": s.batch_seed, "runs": s.runs,
                        "capped": s.capped,
                        "mean_codelets": round(s.codelets, 2), "answers": vr.counts,
                        "mechanism": vr.mechanism, "tv": vr.tv, "tv_interval": list(vr.interval),
                        "chi_square": vr.chi_square, "df": vr.df, "p": vr.p,
                        "role_kept": vr.kept, "results": [list(r) for r in s.results]})
        for name, reason in pv.skipped:
            out.append({"kind": "variant", "problem": sv.problem, "variant": name,
                        "applies": False, "reason": reason})
    return out
