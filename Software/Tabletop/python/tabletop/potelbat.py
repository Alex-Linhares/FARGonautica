"""The Potelbat: a brute-force, formula-based baseline for contrast with Tabletop (D-067).

FCCA describes the first program written for the domain, "a pure brute-force formula-based
program" [FCCA p. 328], the kind a skeptic proposes: "The formula would presumably be a
weighted sum of terms, one for each attribute. Then the top-scoring object would be the
'winner'." [FCCA p. 327]. It was built "to keep ourselves honest" [FCCA p. 328] and
"performed reasonably well on simple Tabletop problems, although on some trickier ones"
it "did poorly" [FCCA p. 328]; it sees no relationships between objects, makes no groups
and builds no bridges [FCCA p. 356] (SPEC.md §1.7).

This module is that formula, re-made for measure M9 only (D-067): each object other than
the touched one scores a weighted sum of a type term (how near its type is to the touched
object's type in the Slipnet at the links' intrinsic lengths) and a place term (how near it
is to the place diagonally opposite the touched object, or, at a lower weight, to the place
directly opposite). No randomness, no structures, no learning; the top score answers, the
first in file order among ties. Its numbers are the baseline's own, not Tabletop's.
"""

import dataclasses
from dataclasses import dataclass

from tabletop import geometry as G
from tabletop.config import Config
from tabletop.problems import Problem, TableObject
from tabletop.accel import Slipnet
from tabletop.slipnet_def import node_for_type

TYPE_WEIGHT = 0.6      # D-067: 0.6 x (100 - the intrinsic type distance), at least 0
PLACE_WEIGHT = 40.0    # D-067: 40 x the better of the two nearnesses
OPPOSITE_FACTOR = 0.7  # D-067: the directly opposite place counts 0.7 of the diagonal one
PLACE_REACH = 40.0     # D-067: a nearness falls linearly to 0 over 40 units
FAR = 100.0            # D-067: the distance given to types no pathway joins


@dataclass(frozen=True)
class Score:
    """One object's Potelbat score (D-067): its `id` and `type`, the intrinsic type
    `distance` to the touched object's type, the `type_term`, the `place_term` and the
    `total`."""

    id: str
    type: str
    distance: float
    type_term: float
    place_term: float
    total: float


def intrinsic_slipnet(config: Config) -> Slipnet:
    """A Slipnet at its intrinsic link lengths, nothing active: the label shrink of D-011
    switched off, so that the distances are the links' own [F95 p. 56] (D-067)."""
    return Slipnet(dataclasses.replace(config, label_shrink=0.0))


def type_distance(net: Slipnet, a: str, b: str) -> float:
    """The intrinsic conceptual distance between two object types [F95 p. 61 (img)]
    (D-012, D-067); `FAR` when no pathway joins them."""
    d = net.distance(node_for_type(a), node_for_type(b))
    return min(FAR, d)


def nearness(x: float, y: float, px: float, py: float) -> float:
    """How near (x, y) is to the place (px, py): 1 on it, falling linearly to 0 at
    `PLACE_REACH` units (D-067)."""
    return max(0.0, 1.0 - G.distance(x, y, px, py) / PLACE_REACH)


def place_term(obj: TableObject, touched: TableObject) -> float:
    """The place term (D-067): `PLACE_WEIGHT` times the better of the nearness to the
    diagonally opposite place and `OPPOSITE_FACTOR` times the nearness to the directly
    opposite place (the two places of D-032, "diagonally opposite locations on the table,
    or nearly so" and the mirror image [FCCA p. 382])."""
    dx, dy = G.diagonal_point(touched.x, touched.y)
    ox, oy = G.opposite_point(touched.x, touched.y)
    return PLACE_WEIGHT * max(nearness(obj.x, obj.y, dx, dy),
                              OPPOSITE_FACTOR * nearness(obj.x, obj.y, ox, oy))


def scores(problem: Problem, config: Config) -> list:
    """Every object's score, the touched one's included, in file order (D-067): one
    `Score` each. The touched object is scored like any other (its type term is the
    whole 60, its place term whatever its own place earns), so that the literal answer
    stays open to the baseline as it is to Tabletop [F95 p. 25]. Draws nothing: the
    formula has no randomness [FCCA p. 327]."""
    net = intrinsic_slipnet(config)
    touched = problem.touched_object()
    out = []
    for obj in problem.objects:
        d = type_distance(net, obj.type, touched.type)
        type_points = TYPE_WEIGHT * max(0.0, 100.0 - d)
        place_points = place_term(obj, touched)
        out.append(Score(obj.id, obj.type, d, type_points, place_points,
                         type_points + place_points))
    return out


def ranked(problem: Problem, config: Config) -> list:
    """The scores from the highest down, the earlier object first among ties (D-067)."""
    return sorted(scores(problem, config), key=lambda s: -s.total)


def answer(problem: Problem, config: Config) -> Score:
    """The Potelbat's answer: "the top-scoring object would be the 'winner'" [FCCA p. 327]
    (D-067)."""
    return ranked(problem, config)[0]


def margin(problem: Problem, config: Config) -> float:
    """The gap between the winner's score and the runner-up's; the winner's whole score
    on a table holding one object (D-067)."""
    order = ranked(problem, config)
    if len(order) < 2:
        return order[0].total
    return order[0].total - order[1].total
