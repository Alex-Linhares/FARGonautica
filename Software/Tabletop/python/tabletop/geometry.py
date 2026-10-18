"""The table, its units, its nine blurry regions and the places opposite an object
(D-001, D-004, D-005, D-032).

"Tabletop is a continuous two-dimensional world" [F95 p. 42] whose objects "can be placed
anywhere on the table" [F95 p. 55]; the book gives no dimensions and no coordinate system
(SPEC.md 1.3). Every figure draws the table as a frame with Henry above its top edge and
Eliza below its bottom edge [F95 p. 27 (img)]; the chapter 5 figures' frames are 194 x 176
pixels, 1.10 times wider than tall, and the screen's frame in the chapter 4 dumps has the
same shape [F95 p. 98 (img)]. The table is therefore a rectangle of 110 x 100 units, with
the origin at Henry's left corner, x to the right and y downwards, towards Eliza (D-001).

"The table is divided into nine regions like a tic-tac-toe board, although the boundaries
between regions are intentionally blurry." [F95 p. 65] The regions are the thirds of each
axis; an object's exact region is the one holding its centre, and the region a codelet sees
is drawn each time it asks, so that an object near a boundary "may sometimes be considered
by the program to be in the adjacent region" [F95 p. 65] (D-004). The most favoured region
is "diagonally opposite the touched object" and the next "directly opposite" it [F95 p. 66]:
the regions reached by reflecting the touched object's region through the table's centre
and across its middle row (D-032). Henry's and Eliza's halves are not sides: they exist only
as the upper and lower rows of regions (D-005).

Everything here is pure geometry: no module-level state, and the only randomness is the
blur draw, through the run's generator.
"""

import math
from dataclasses import dataclass
from functools import lru_cache

TABLE_WIDTH = 110.0   # D-001: the drawn frames' aspect, 1.10, at 100 units of height
TABLE_HEIGHT = 100.0  # D-001

COLUMNS = ("left", "middle", "right")  # the three columns of the tic-tac-toe board [F95 p. 65]
ROWS = ("upper", "middle", "lower")    # the three rows, from Henry's edge to Eliza's [F95 p. 65]

RELATIONS = ("diagonal", "opposite", "near", "other")  # D-032: a region's relation to the touched one
# D-082: the placement of one region against another across the midline, read in the table
# frame of D-029: `mirror` (the same column, the other outer row), `point` (the column
# flipped, the other outer row), `middle` (both in the middle column, the other outer row,
# where the two images coincide), `none` (the same row, or a region in the middle band).
MIRROR, POINT, MIDDLE, NONE = "mirror", "point", "middle", "none"
PLACEMENTS = (MIRROR, POINT, MIDDLE, NONE)
HENRY, ELIZA = "henry", "eliza"  # D-005: the two halves, as names of the upper and lower rows


def distance(ax: float, ay: float, bx: float, by: float) -> float:
    """The Euclidean distance between two points, in table units (D-001: the same unit on
    both axes, so distances are isotropic)."""
    return math.hypot(ax - bx, ay - by)


@dataclass(frozen=True, order=True)
class Region:
    """One of the nine regions of the tic-tac-toe board [F95 p. 65]: a column (0 left, 1
    middle, 2 right) and a row (0 upper, by Henry, 1 middle, 2 lower, by Eliza) (D-004)."""

    column: int
    row: int

    @property
    def name(self) -> str:
        """The region's name, as the trace prints it: `upper-left`, `middle-right`, and
        `middle` alone for the centre (D-004)."""
        if self.column == 1 and self.row == 1:
            return "middle"
        return f"{ROWS[self.row]}-{COLUMNS[self.column]}"

    def is_adjacent(self, other: "Region") -> bool:
        """Whether the two regions touch, along a side or at a corner (D-032: the regions
        "near" the touched object are its own and these)."""
        return (self != other and abs(self.column - other.column) <= 1
                and abs(self.row - other.row) <= 1)


REGIONS = tuple(Region(column, row) for row in range(3) for column in range(3))  # reading order

_BY_NAME = {region.name: region for region in REGIONS}


def region_named(name: str) -> Region:
    """The region with this name (`upper-left`, `middle`, ...; D-004)."""
    return _BY_NAME[name]


def column_boundaries() -> tuple[float, float]:
    """The x coordinates of the two lines between the columns (D-004: three equal columns)."""
    return TABLE_WIDTH / 3, 2 * TABLE_WIDTH / 3


def row_boundaries() -> tuple[float, float]:
    """The y coordinates of the two lines between the rows (D-004: three equal rows)."""
    return TABLE_HEIGHT / 3, 2 * TABLE_HEIGHT / 3


def _third(value: float, extent: float) -> int:
    """Which third of [0, extent] holds `value`: 0, 1 or 2, with a value on a line going to
    the far third, and values off the table clamped to the end thirds (D-004)."""
    return min(2, max(0, int(value * 3 // extent)))


def exact_region(x: float, y: float) -> Region:
    """The region holding the point (x, y): the one an object is "actually in" [F95 p. 65],
    decided by its centre (D-004)."""
    return Region(_third(x, TABLE_WIDTH), _third(y, TABLE_HEIGHT))


def blur_probability(distance_to_boundary: float, config) -> float:
    """The probability that an object this far from a region boundary is put on the far
    side of it (D-004): `region_blur_max` x exp(-d / `region_blur_width`) when d is within
    the width, 0 beyond, so an object on the line crosses half the time and one a glass
    width away about a fifth of the time."""
    width = config.region_blur_width
    if distance_to_boundary > width:
        return 0.0
    return config.region_blur_max * math.exp(-distance_to_boundary / width)


def _drawn_third(value: float, extent: float, rng, config) -> int:
    """The third an object at `value` is seen in along one axis (D-004): its exact third,
    moved across the nearer of the lines bounding that third when the blur coin passes.
    At most one coin per axis, and none when that line is beyond the blur width (the thirds
    are wider than twice the width, so only one line can ever be within it)."""
    third = _third(value, extent)
    lines = (extent / 3, 2 * extent / 3)
    candidates = []  # (distance to a bounding line, the third across it)
    if third > 0:
        candidates.append((abs(value - lines[third - 1]), third - 1))
    if third < 2:
        candidates.append((abs(value - lines[third]), third + 1))
    d, across = min(candidates)
    p = blur_probability(d, config)
    if p > 0 and rng.coin(p):
        return across
    return third


def drawn_region(x: float, y: float, rng, config) -> Region:
    """The region a codelet sees the point (x, y) in, drawn afresh each time it is asked
    (D-004): the exact region, blurred across any boundary within `region_blur_width` by
    one coin per axis through the run's generator (x first, then y; both may cross, which
    puts the object in the diagonal neighbour). The draw is not softened by temperature:
    the blur is perceptual, not a decision."""
    return Region(_drawn_third(x, TABLE_WIDTH, rng, config),
                  _drawn_third(y, TABLE_HEIGHT, rng, config))


def row_of(region: Region) -> int:
    """The row band of a region: 0 Henry's row, 1 the middle band, 2 Eliza's row (D-082;
    D-005: a band of regions, not a side)."""
    return region.row


def column_of(region: Region) -> int:
    """The column of a region: 0 left, 1 middle, 2 right, in the table frame (D-082;
    D-029: "where the terms 'left' and 'right' refer to the sides of the table as a
    whole" [F95 p. 85])."""
    return region.column


def in_midline_band(region: Region) -> bool:
    """Whether a region lies in the middle row of regions, the band between the two
    people's rows that the MIDLINE role is read on (D-082, D-084)."""
    return region.row == 1


def is_corner(region: Region) -> bool:
    """Whether a region is one of the four corners of the board, in an outer row and an
    outer column (D-085: the CORNER role, "diagonally opposite corners of the table" [F95
    p. 43])."""
    return region.row != 1 and region.column != 1


def placement(a: Region, b: Region) -> str:
    """How region `b` is placed against region `a` across the midline (D-082): `mirror`
    when b is a's reflection across the middle row ("directly opposite" [F95 p. 66]: the
    same column, the other outer row), `point` when b is a's reflection through the
    table's centre ("diagonally opposite" [F95 p. 66]: the column flipped, the other
    outer row), `middle` when both lie in the middle column in the two outer rows (the two
    reflections coincide, D-032's special case), and `none` otherwise (the same row, or
    either region in the middle band). A pure function: `placement(m(a), m(b)) ==
    placement(a, b)` for the mirror, the side swap and the rotation of the table, and
    `placement(b, a) == placement(a, b)`."""
    if {a.row, b.row} != {0, 2}:
        return NONE
    if a.column == 1 and b.column == 1:
        return MIDDLE
    if a.column == b.column:
        return MIRROR
    if a.column + b.column == 2:
        return POINT
    return NONE


def diagonal_region(touched: Region) -> Region:
    """The region "diagonally opposite the touched object" [F95 p. 66]: the touched object's
    region reflected through the table's centre (D-032)."""
    return Region(2 - touched.column, 2 - touched.row)


def opposite_region(touched: Region) -> Region:
    """The region "directly opposite the touched object" [F95 p. 66]: the touched object's
    region reflected across the middle row (D-032)."""
    return Region(touched.column, 2 - touched.row)


def region_relation(region: Region, touched: Region) -> str:
    """How a region stands to the touched object's region (D-032): `diagonal` for the
    reflection through the centre, `opposite` for the reflection across the middle row
    (and, when the touched object is in the middle column and the two coincide, for the two
    far corners), `near` for the touched object's own region and its neighbours, `other` for
    the rest. A reflection that lands on the touched region itself (a touched object in the
    middle row) counts as `near`, not as opposite to itself."""
    diagonal = diagonal_region(touched)
    opposite = opposite_region(touched)
    if region == diagonal and region != touched:
        return "diagonal"
    if region == opposite and region != touched:
        return "opposite"
    if (touched.column == 1 and touched.row != 1 and region.row == 2 - touched.row
            and region.column != 1):
        return "opposite"
    if region == touched or region.is_adjacent(touched):
        return "near"
    return "other"


@lru_cache(maxsize=64)
def preferences_by_relation(region_preference: tuple) -> dict:
    """`Config.region_preference` as a dict by relation (D-032), built once per table: the
    table is a frozen tuple of pairs, read on every region term (D-112)."""
    return dict(region_preference)


def region_weight(region: Region, touched: Region, config) -> float:
    """The preference weight of a region relative to the touched object's, from
    `Config.region_preference` (D-032): diagonal 1.0, opposite 0.7, near 0.4, other 0.15."""
    return preferences_by_relation(config.region_preference)[region_relation(region, touched)]


def diagonal_point(x: float, y: float) -> tuple[float, float]:
    """The point diagonally opposite (x, y): its reflection through the table's centre
    (D-032; "diagonally opposite locations on the table, or nearly so" [FCCA p. 382])."""
    return TABLE_WIDTH - x, TABLE_HEIGHT - y


def opposite_point(x: float, y: float) -> tuple[float, float]:
    """The point directly opposite (x, y): its mirror image across the table's middle line
    between the two people (D-032; "mirror-image ... locations" [FCCA p. 382])."""
    return x, TABLE_HEIGHT - y


def half(region: Region) -> str | None:
    """Whose half of the table a region lies in: `henry` for the upper row, `eliza` for the
    lower row, None for the middle row (D-005). This is a description for the trace and the
    GUI only: "There is no pre-set boundary line dividing this arrangement into two disjoint
    'situations'" [FCCA p. 388], and no codelet uses it to forbid a group or a
    correspondence."""
    if region.row == 0:
        return HENRY
    if region.row == 2:
        return ELIZA
    return None


def gap_between(a: tuple[float, float, float, float],
                b: tuple[float, float, float, float]) -> float:
    """The shortest distance between two axis-aligned boxes (left, top, right, bottom), in
    table units: 0 when they overlap or touch, the gap along one axis when they overlap
    along the other, the corner-to-corner distance otherwise (D-022: the proximity criterion
    measures the gap between footprints' boxes, which is where size enters the engine)."""
    dx = max(0.0, max(a[0], b[0]) - min(a[2], b[2]))
    dy = max(0.0, max(a[1], b[1]) - min(a[3], b[3]))
    return math.hypot(dx, dy)
