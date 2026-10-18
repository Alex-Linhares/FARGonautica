"""Realistic random tables for robustness runs (D-068).

French restricted the domain to tables that look set for a meal: "each of the table
configurations had to be reasonably realistic, in the sense of looking like a real table
that was set with plates, cups, saucers, saltshakers, pepper shakers, and silverware" [F95
p. 38]; it "was not a patterns-and-shapes domain in which just any configuration was
permissible" [F95 p. 38], and objects are never "arranged in the shape of a Christmas tree
or in strange, alternating patterns" [F95 p. 38]. Figure 2.21 shows the two kinds of table
he disallows [F95 p. 39 (img)]. FCCA says the same of tables with "unnatural" geometric
symmetries [FCCA p. 388].

`random_table(seed)` lays out one to three place settings along Henry's row and along
Eliza's (where every one of French's digitized rows lies: `docs/problems.md`), each a run
of silverware around a plate, a soup bowl or a cup, with an optional saucer beside the
cup and an optional glass; a stray cup or glass may stand apart at a row's end, as the
isolated cup of Surround 5 does [F95 p. 118 (img)]; shakers and a lone cup or glass may
stand in the middle band, as the junk of Surround 10 does [F95 p. 121 (img)]. Gaps within
a setting are drawn between one and three units (French's rows' silverware stands 0.3 to
0.64 glass widths apart, `docs/problems.md` 8), the rows and the objects jitter by at most
half a glass width each way, so that no object is further than a glass width from where
a set table would put it (D-068), and the touched object is drawn by type weight. Every
draw goes through the seeded generator (D-064), so a table is reproducible from its seed.
These tables are for robustness runs only, never for results compared with French's.
"""

from __future__ import annotations

from tabletop import objects as O
from tabletop.geometry import TABLE_HEIGHT, TABLE_WIDTH
from tabletop.problems import Problem, Provenance, TableObject
from tabletop.accel import Rng

FAMILY = "random"
SYNTHETIC = "synthetic"

# Where the rows lie: French's digitized Henry rows run at y 11-17, his Eliza rows at 73-89,
# and Surround 10's junk at 41-44 (`docs/problems.md`); the bands are read generously.
ROW_Y = (("henry", 15.0), ("eliza", 82.0))
MIDDLE_Y = 50.0
ROW_JITTER = 3.0                      # the whole row, up and down
OBJECT_JITTER = O.glass_width() / 2   # each object, each way: within a glass width in all (D-068)
BANDS = (("henry", 5.0, 28.0), ("middle", 36.0, 64.0), ("eliza", 70.0, 95.0))

EDGE_MARGIN = 1.0           # from the table's edge to a footprint
GAP = (1.0, 3.0)            # between neighbours in one cluster, in units
CLUSTER_GAP = (8.0, 24.0)   # between clusters along a row (2.5 glass widths is 11.25, D-022)
OVERLAP_SLOP = 0.6          # footprints may overlap by one figure pixel (0.567 units, D-002)

# The touched object is drawn by type weight: French touches a cup in every Surround and
# Blockage problem, a spoon in Buridan and a fork in Figure/Ground [F95 pp. 115-141].
TOUCH_WEIGHTS = (("cup", 6), ("small glass", 2), ("spoon", 2), ("fork", 2), ("knife", 1),
                 ("plate", 1), ("saucer", 1), ("big glass", 1), ("salt shaker", 1),
                 ("pepper shaker", 1), ("soup bowl", 1))

CENTRES = ("plate", "soup bowl", "cup", "cup", "cup+saucer", "plate")   # what a setting is set around
GLASSES = (None, "small glass", "small glass", "big glass")
STRAYS = ("cup", "cup", "small glass", "spoon")                        # what may stand apart
MIDDLE_CLUSTERS = (("salt shaker", "pepper shaker"), ("salt shaker",), ("pepper shaker",),
                   ("cup",), ("small glass",), ("salt shaker", "pepper shaker"))


def _slot(type_: str) -> tuple:
    return (type_, "up" if type_ in O.SILVERWARE else None)


def setting(rng: Rng) -> list:
    """One place setting as (type, orientation) slots from left to right (D-068): one or
    two forks, usually a knife, the centre (a plate, a soup bowl, a cup, or a cup with its
    saucer beside it), up to two spoons, and sometimes a glass at the end, the order of
    French's own rows ("fork fork knife cup spoon spoon" [F95 p. 118 (img)])."""
    slots = [_slot("fork")] * (1 + rng.below(2))
    if rng.coin(0.8):
        slots.append(_slot("knife"))
    centre = rng.choice(CENTRES)
    slots += [_slot(t) for t in centre.split("+")]
    slots += [_slot("spoon")] * rng.below(3)
    glass = rng.choice(GLASSES)
    if glass is not None:
        slots.append(_slot(glass))
    return slots


def row_clusters(rng: Rng, mirrored: bool) -> list:
    """The clusters of one row (D-068): one to three settings, each perhaps reversed for
    the person it is set for, and a stray object in a third of the rows."""
    clusters = []
    for _ in range(1 + rng.below(3)):
        s = setting(rng)
        if mirrored:
            s.reverse()
        clusters.append(s)
    if rng.coin(1 / 3):
        stray = [_slot(rng.choice(STRAYS))]
        if rng.coin():
            clusters.append(stray)
        else:
            clusters.insert(0, stray)
    return clusters


def middle_clusters(rng: Rng) -> list:
    """What stands between the rows (D-068): nothing in half the tables, otherwise one or
    two clusters of shakers or a lone cup or glass."""
    if rng.coin():
        return []
    return [[_slot(t) for t in rng.choice(MIDDLE_CLUSTERS)] for _ in range(1 + rng.below(2))]


def _widths(clusters: list) -> list:
    return [[O.footprint(t, o)[0] for t, o in c] for c in clusters]


def fit_clusters(rng: Rng, clusters: list, available: float) -> list:
    """Drop slots from the right of the last cluster, then whole clusters, until the row
    fits in `available` units with the smallest gaps; what remains is realistic because
    a shorter setting is still a setting (D-068)."""
    clusters = [list(c) for c in clusters]
    while clusters:
        widths = _widths(clusters)
        total = sum(sum(w) for w in widths)
        total += sum(GAP[0] * (len(w) - 1) for w in widths)
        total += CLUSTER_GAP[0] * (len(clusters) - 1)
        if total <= available:
            return clusters
        if len(clusters[-1]) > 1:
            clusters[-1].pop()
        else:
            clusters.pop()
    return clusters


def place_row(rng: Rng, clusters: list, y: float) -> list:
    """Place the clusters of one row left to right (D-068): gaps drawn within a cluster
    and between clusters, the row shifted by a seeded part of the slack, each object
    jittered up and down by at most half a glass width. Returns (type, orientation, x, y)
    tuples; nothing overlaps because the objects are laid end to end."""
    available = TABLE_WIDTH - 2 * EDGE_MARGIN
    clusters = fit_clusters(rng, clusters, available)
    placed = []
    gaps, least = [], []
    for i, cluster in enumerate(clusters):
        if i > 0:
            gaps.append(CLUSTER_GAP[0] + rng.random() * (CLUSTER_GAP[1] - CLUSTER_GAP[0]))
            least.append(CLUSTER_GAP[0])
        for j in range(len(cluster) - 1):
            gaps.append(GAP[0] + rng.random() * (GAP[1] - GAP[0]))
            least.append(GAP[0])
    widths = [w for ws in _widths(clusters) for w in ws]
    total = sum(widths) + sum(gaps)
    slack = available - total
    if slack < 0:    # the drawn gaps overshoot: shrink their excess over the least gaps
        excess = sum(g - m for g, m in zip(gaps, least))
        scale = (available - sum(widths) - sum(least)) / excess if excess else 0.0
        gaps = [m + (g - m) * scale for g, m in zip(gaps, least)]
        slack = 0.0
    cursor = EDGE_MARGIN + rng.random() * slack
    k = 0
    for cluster in clusters:
        for type_, orientation in cluster:
            w = widths[k]
            x = cursor + w / 2
            jitter = (rng.random() * 2 - 1) * OBJECT_JITTER
            placed.append((type_, orientation, round(x, 1), round(y + jitter, 1)))
            cursor += w
            if k < len(gaps):
                cursor += gaps[k]
            k += 1
    return placed


def _clamp_y(y: float, type_: str) -> float:
    h = O.footprint(type_)[1]
    return min(max(y, EDGE_MARGIN + h / 2), TABLE_HEIGHT - EDGE_MARGIN - h / 2)


def layout(rng: Rng) -> list:
    """The objects of one random table as (type, orientation, x, y) tuples, Henry's row,
    then Eliza's, then the middle band (the reading order of D-003) (D-068)."""
    placed = []
    for side, y0 in ROW_Y:
        y = y0 + (rng.random() * 2 - 1) * ROW_JITTER
        placed += place_row(rng, row_clusters(rng, mirrored=(side == "eliza")), y)
    middle = middle_clusters(rng)
    if middle:
        y = MIDDLE_Y + (rng.random() * 2 - 1) * ROW_JITTER
        placed += place_row(rng, middle, y)
    return [(t, o, x, round(_clamp_y(y, t), 1)) for t, o, x, y in placed]


def touched_index(rng: Rng, placed: list) -> int:
    """Which object Henry touches: drawn by type weight among the objects on the table
    (D-068)."""
    weights = dict(TOUCH_WEIGHTS)
    return rng.weighted_index([weights[t] for t, _, _, _ in placed])


def random_table(seed: int) -> Problem:
    """A realistic random table as a `Problem` named `random-<seed>` (D-068): its objects
    with synthetic provenance, no labelled answers (nothing of French's is measured on it)
    and the touched object drawn by type weight. Same seed, same table."""
    rng = Rng(seed)
    placed = layout(rng)
    counts: dict[str, int] = {}
    objects = []
    for type_, orientation, x, y in placed:
        counts[type_] = counts.get(type_, 0) + 1
        objects.append(TableObject(f"{O.id_stem(type_)}{counts[type_]}", type_, x, y,
                                   orientation, (0, 0, 0, 0),
                                   Provenance(SYNTHETIC, 0, "random table (D-068)")))
    touched = objects[touched_index(rng, placed)].id
    faults = realism_faults(objects)
    if faults:
        raise ValueError(f"random table {seed}: " + "; ".join(faults))
    return Problem(name=f"random-{seed}", title=f"Random table {seed}", family=FAMILY,
                   source={"tag": SYNTHETIC, "page": 0, "figure": "random table (D-068)"},
                   width=TABLE_WIDTH, height=TABLE_HEIGHT, frame_px=(0, 0, 0, 0),
                   touched=touched, answers={}, objects=tuple(objects),
                   method="generated by tabletop.tables.random_table from the seed: place "
                          "settings along the two rows, shakers between them (D-068)",
                   uncertainty={"units": 0.0, "note": "synthetic: the positions are exact"},
                   notes=(describe(objects),), review="none: a synthetic table (D-068)")


def band_of(y: float) -> str | None:
    """Which of the three bands a y coordinate lies in: Henry's row, the middle, Eliza's
    row, or None between them (D-068)."""
    for name, low, high in BANDS:
        if low <= y <= high:
            return name
    return None


def overlap_depth(a, b) -> float:
    """How deeply two footprint boxes overlap, in units: the smaller of the two axes'
    overlaps, negative when the boxes are apart (D-068)."""
    al, at, ar, ab = O.box(a)
    bl, bt, br, bb = O.box(b)
    return min(min(ar, br) - max(al, bl), min(ab, bb) - max(at, bt))


def realism_faults(objects) -> list:
    """How a layout breaks the rules a set table obeys (D-068; [F95 p. 38]), as sentences,
    empty when it obeys them: every object inside the table with its footprint; no two
    footprints overlapping by more than one figure pixel (`OVERLAP_SLOP`: the digitized
    icons of Buridan's rows touch by up to 0.55 units); every object in one of the three
    bands; every band's objects within a strip of twice the jitter. Every digitized layout
    of the catalogue passes (`tests/test_tables.py`)."""
    faults = []
    objects = list(objects)
    for o in objects:
        left, top, right, bottom = O.box(o)
        if left < 0 or top < 0 or right > TABLE_WIDTH or bottom > TABLE_HEIGHT:
            faults.append(f"{o.id} is not inside the table")
        if band_of(o.y) is None:
            faults.append(f"{o.id} lies between the rows and the middle band")
    for i, a in enumerate(objects):
        for b in objects[i + 1:]:
            if overlap_depth(a, b) > OVERLAP_SLOP:
                faults.append(f"{a.id} and {b.id} overlap")
    for name, low, high in BANDS:
        ys = sorted(o.y for o in objects if low <= o.y <= high)
        if ys and ys[-1] - ys[0] > 2 * (ROW_JITTER + OBJECT_JITTER):
            faults.append(f"the {name} band's objects spread over {ys[-1] - ys[0]:.1f} units")
    return faults


def describe(objects) -> str:
    """One sentence on a layout: how many objects, and what stands in each band (D-068)."""
    parts = []
    for name, low, high in BANDS:
        types = [o.type for o in objects if low <= o.y <= high]
        if types:
            parts.append(f"{name}: " + " ".join(types))
    return f"{len(objects)} objects; " + "; ".join(parts)
