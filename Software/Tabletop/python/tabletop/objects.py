"""The eleven object types, the four orientations of silverware, and their footprints (D-002).

The palette of Figure 2.1 lists the types top to bottom: fork, knife, spoon, cup, saucer,
plate, big glass, small glass, pepper shaker, salt shaker, soup bowl [F95 p. 26 (img)], and
draws each of the three pieces of silverware pointing up, down, left and right; "The forks,
knives, and spoons have four possible orientations" [F95 p. 55]. "All objects have a size;
they are not simply point objects" [F95 p. 55], but no page gives a size for any type, so
the footprints are the icons' bounding boxes measured in pixels on the chapter 5 figures
(the types that appear there) and on the palette (the big glass and the soup bowl), converted
to table units with the figures' scale (D-001, D-002; `docs/problems.md`).
"""

from functools import lru_cache

from tabletop.geometry import gap_between

TYPES = ("fork", "knife", "spoon", "cup", "saucer", "plate", "big glass", "small glass",
         "pepper shaker", "salt shaker", "soup bowl")  # the palette's order [F95 p. 26 (img)]

SILVERWARE = ("fork", "knife", "spoon")  # the types with an orientation [F95 p. 55]

ORIENTATIONS = ("up", "down", "left", "right")  # as the palette draws them [F95 p. 26 (img)]

# D-002: each icon's bounding box in pixels (width, height) as drawn pointing up, measured on
# the chapter 5 figures (135 ppi, frames of 194 x 176 px) [F95 pp. 115-121 (img)]; the big
# glass and the soup bowl, which chapter 5's Surround figures do not show, on the palette
# [F95 p. 26 (img)], whose icons are at the same scale (its fork is 6 x 17 px too).
FOOTPRINTS_PX = {
    "fork": (6, 17),
    "knife": (3, 17),
    "spoon": (6, 17),
    "cup": (14, 7),
    "saucer": (14, 13),
    "plate": (19, 19),
    "big glass": (11, 15),
    "small glass": (8, 10),
    "pepper shaker": (12, 15),
    "salt shaker": (12, 15),
    "soup bowl": (21, 12),
}

# D-001: the figures' frames are 194 x 176 px for a table of 110 x 100 units, so one pixel
# is 0.567 units along both axes.
UNITS_PER_PIXEL = 110 / 194


@lru_cache(maxsize=None)
def footprint(type_: str, orientation: str | None = None) -> tuple[float, float]:
    """The (width, height) in table units an object of this type covers (D-002).

    Silverware pointing left or right swaps the box's sides; the other types ignore the
    orientation. Values are rounded to a tenth of a unit. Worked out once per (type,
    orientation), of which there are a few dozen: the engine asks on every box (D-112).
    """
    w_px, h_px = FOOTPRINTS_PX[type_]
    if type_ in SILVERWARE and orientation in ("left", "right"):
        w_px, h_px = h_px, w_px
    return round(w_px * UNITS_PER_PIXEL, 1), round(h_px * UNITS_PER_PIXEL, 1)


def id_stem(type_: str) -> str:
    """The stem of an object id for this type: the type with its space replaced by a hyphen
    (`small glass` -> `small-glass`; ids are `small-glass1`, `fork3`; D-003)."""
    return type_.replace(" ", "-")


def glass_width() -> float:
    """The width of a small glass in table units, 4.5: the unit French measures the Blockage
    gaps in, "one glass-width" [F95 p. 128] (D-002, D-022)."""
    return footprint("small glass")[0]


def box(obj) -> tuple[float, float, float, float]:
    """The footprint box (left, top, right, bottom) in table units of an object with a
    `type`, an `orientation` and a centre (`x`, `y`): its type's footprint centred on its
    position (D-002)."""
    w, h = footprint(obj.type, obj.orientation)
    return obj.x - w / 2, obj.y - h / 2, obj.x + w / 2, obj.y + h / 2


def edge_gap(a, b) -> float:
    """The white space between two objects' footprint boxes, in table units: 0 when they
    overlap (D-002, D-022: the gap the proximity criterion measures)."""
    return gap_between(box(a), box(b))


def gap_in_glass_widths(gap_units: float) -> float:
    """A gap in table units expressed in small-glass widths, French's unit for the Blockage
    series [F95 p. 128] (D-022)."""
    return gap_units / glass_width()
