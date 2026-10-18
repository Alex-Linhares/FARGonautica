"""The eleven object icons as Qt-free primitives in their footprint boxes (PANES.md
section 6; ANSWERS.md 4; G-016).

The icons are **new drawings, not French's**: French's palette, Figure 2.1
[F95 p. 26 (img)], shows a fork, a knife, a spoon, a cup, a saucer, a plate, a big glass,
a small glass, a pepper shaker, a salt shaker and a soup bowl [F95 p. 95], the three
pieces of silverware in four orientations [F95 p. 55], and the dumps draw each object on
the table at its size [F95 pp. 98-107 (img)]. None of his bitmaps is copied: each type is
described here as a few primitives (rounded rectangles, ellipses, polygons, paths of
lines and quadratic curves, lines and dots) in a box of the proportions of
`objects.FOOTPRINTS_PX` (D-002), centred on the origin and pointing up, the y axis
down as on a screen. The GUI's `icons.py` paints them with QPainter, and
`scripts/docs_figures.py` draws the README's pictures from the same primitives, so that
the GUI and the README share one set (ANSWERS.md 4).

`primitives(type_, w, h)` gives the primitives for a box `w` by `h`; `ROTATION` gives the
angle of each orientation of `objects.ORIENTATIONS`, and `rotated` and `bounds` turn a
drawing to an orientation and measure it, so that a test can check that every type stays
inside `objects.footprint(type_, orientation)` in every orientation. Fills are roles
(`FILL_ROLES`), not colours: the painter chooses the colours.
"""

from dataclasses import dataclass

from tabletop.objects import ORIENTATIONS, SILVERWARE, TYPES

__all__ = ["FILL_ROLES", "KINDS", "ROTATION", "Primitive", "bounds", "orientations_of", "pen_width",
           "points_of", "primitives", "rotate_point", "rotated"]

# What a primitive's fill means; the painter maps each role to a colour (G-016).
FILL_ROLES = ("metal", "white", "glass", "dark", "pepper")

# The kinds of primitive: a rounded rectangle (`points` = two corners, `radius`), an
# ellipse (two corners), a polygon (its vertices, closed), a path (`segments`: ("move",
# p), ("line", p), ("quad", control, p), ("close",)), a line (two points) and a dot (its
# centre, `radius`) (G-016).
KINDS = ("rect", "ellipse", "polygon", "path", "line", "dot")

# The angle, clockwise in screen coordinates, that turns the icon pointing up to each
# orientation of `objects.ORIENTATIONS`; types without an orientation (None) are not
# turned [F95 p. 26 (img)] (G-016).
ROTATION = {None: 0, "up": 0, "right": 90, "down": 180, "left": 270}


@dataclass(frozen=True)
class Primitive:
    """One stroke of an icon (G-016): its `kind` (`KINDS`), its `points` (for a path, its
    `segments` instead), its `fill` role or None for no fill, whether its outline is
    `stroked`, and a `radius` (a rounded rectangle's corners, a dot's size)."""

    kind: str
    points: tuple = ()
    segments: tuple = ()
    fill: str | None = None
    stroked: bool = True
    radius: float = 0.0


def pen_width(w: float, h: float) -> float:
    """The outline's width for a box `w` by `h`: thin, never under one unit (the README's
    figures' rule; G-016)."""
    return max(1.0, min(w, h) * 0.07 + 0.6)


def _rect(left, top, width, height, fill, radius=0.0):
    return Primitive("rect", ((left, top), (left + width, top + height)), fill=fill,
                     radius=radius)


def _ellipse(left, top, width, height, fill):
    return Primitive("ellipse", ((left, top), (left + width, top + height)), fill=fill)


def _line(a, b):
    return Primitive("line", (a, b))


def _fork(w, h, top, bottom, left, right):
    head = (("move", (left, top + h * 0.2)), ("line", (left, top + h * 0.3)),
            ("quad", (left, top + h * 0.44), (0.0, top + h * 0.44)),
            ("quad", (right, top + h * 0.44), (right, top + h * 0.3)),
            ("line", (right, top + h * 0.2)), ("close",))
    tines = tuple(_line((x, top + h * 0.22), (x, top))
                  for x in (left, left + w / 3, right - w / 3, right))
    return (_rect(-w * 0.16, top + h * 0.42, w * 0.32, h * 0.58, "metal", w * 0.15),
            Primitive("path", segments=head, fill="metal")) + tines


def _knife(w, h, top, bottom, left, right):
    blade = (("move", (left, top + h * 0.55)), ("line", (left, top + h * 0.1)),
             ("quad", (left, top), (right, top)), ("line", (right, top + h * 0.55)),
             ("close",))
    return (Primitive("path", segments=blade, fill="metal"),
            _rect(left, top + h * 0.55, w, h * 0.45, "dark", w * 0.4))


def _spoon(w, h, top, bottom, left, right):
    return (_rect(-w * 0.14, top + h * 0.36, w * 0.28, h * 0.64, "metal", w * 0.14),
            _ellipse(left, top, w, h * 0.38, "metal"))


def _cup(w, h, top, bottom, left, right):
    bw = w * 0.74
    body = (("move", (left, top)), ("line", (left + bw, top)),
            ("line", (left + bw * 0.9, bottom - h * 0.25)),
            ("quad", (left + bw * 0.85, bottom), (left + bw * 0.5, bottom)),
            ("quad", (left + bw * 0.15, bottom), (left + bw * 0.1, bottom - h * 0.25)),
            ("close",))
    return (Primitive("path", segments=body, fill="white"),
            _ellipse(left + bw * 0.86, top + h * 0.18, w - bw * 0.86, h * 0.5, None))


def _dish(inset):
    def dish(w, h, top, bottom, left, right):
        return (_ellipse(left, top, w, h, "white"),
                _ellipse(left + w * inset, top + h * inset, w * (1 - 2 * inset),
                         h * (1 - 2 * inset), None))
    return dish


def _small_glass(w, h, top, bottom, left, right):
    return (Primitive("polygon", ((left, top), (right, top), (right - w * 0.14, bottom),
                                  (left + w * 0.14, bottom)), fill="glass"),
            _line((left + w * 0.06, top + h * 0.35), (right - w * 0.06, top + h * 0.35)))


def _big_glass(w, h, top, bottom, left, right):
    bowl = (("move", (left, top)), ("line", (right, top)),
            ("quad", (right, top + h * 0.55), (0.0, top + h * 0.58)),
            ("quad", (left, top + h * 0.55), (left, top)), ("close",))
    return (Primitive("path", segments=bowl, fill="glass"),
            _line((0.0, top + h * 0.58), (0.0, bottom)),
            _line((left + w * 0.2, bottom), (right - w * 0.2, bottom)))


def _shaker(body_fill):
    def shaker(w, h, top, bottom, left, right):
        dots = tuple(Primitive("dot", ((w * dx, top + h * 0.14),), fill="dark", stroked=False,
                               radius=w * 0.06) for dx in (-0.15, 0.0, 0.15))
        return (_rect(left, top + h * 0.3, w, h * 0.7, body_fill, w * 0.2),
                _rect(left + w * 0.2, top, w * 0.6, h * 0.32, "metal", w * 0.25)) + dots
    return shaker


def _soup_bowl(w, h, top, bottom, left, right):
    bowl = (("move", (left, top + h * 0.3)), ("line", (right, top + h * 0.3)),
            ("quad", (right, bottom), (0.0, bottom)),
            ("quad", (left, bottom), (left, top + h * 0.3)), ("close",))
    return (Primitive("path", segments=bowl, fill="white"),
            _ellipse(left, top, w, h * 0.6, "white"))


_SHAPES = {
    "fork": _fork, "knife": _knife, "spoon": _spoon, "cup": _cup,
    "saucer": _dish(0.22), "plate": _dish(0.18),
    "big glass": _big_glass, "small glass": _small_glass,
    "pepper shaker": _shaker("pepper"), "salt shaker": _shaker("white"),
    "soup bowl": _soup_bowl,
}
assert set(_SHAPES) == set(TYPES)


def primitives(type_: str, w: float, h: float) -> tuple:
    """The primitives of an icon of `type_` in a box `w` by `h` centred on the origin,
    pointing up, every point inside the box (G-016). `ValueError` for an unknown type."""
    if type_ not in _SHAPES:
        raise ValueError(f"no icon for {type_!r}; the types are {TYPES}")
    return _SHAPES[type_](w, h, -h / 2, h / 2, -w / 2, w / 2)


def points_of(primitive: Primitive) -> tuple:
    """Every point of a primitive, control points included, so that a primitive's bounds
    are at most the bounds of these (a quadratic curve stays inside its control
    polygon) (G-016)."""
    if primitive.kind == "path":
        return tuple(p for segment in primitive.segments for p in segment[1:])
    if primitive.kind == "dot":
        (x, y), r = primitive.points[0], primitive.radius
        return ((x - r, y - r), (x + r, y + r))
    return tuple(primitive.points)


def rotate_point(point: tuple, degrees: int) -> tuple:
    """A point turned clockwise on the screen by a multiple of 90 degrees about the
    origin (G-016)."""
    x, y = point
    turns = (degrees // 90) % 4
    for _ in range(turns):
        x, y = -y, x
    return (x, y)


def rotated(prims: tuple, orientation: str | None) -> tuple:
    """The primitives turned to `orientation` (`ROTATION`), as the painter turns them;
    None and `up` leave them as they are (G-016)."""
    degrees = ROTATION[orientation]
    if degrees == 0:
        return tuple(prims)
    out = []
    for p in prims:
        if p.kind == "path":
            segments = tuple((s[0],) + tuple(rotate_point(q, degrees) for q in s[1:])
                             for s in p.segments)
            out.append(Primitive(p.kind, (), segments, p.fill, p.stroked, p.radius))
        elif p.kind in ("rect", "ellipse"):
            (ax, ay), (bx, by) = (rotate_point(q, degrees) for q in p.points)
            corners = ((min(ax, bx), min(ay, by)), (max(ax, bx), max(ay, by)))
            out.append(Primitive(p.kind, corners, (), p.fill, p.stroked, p.radius))
        else:
            points = tuple(rotate_point(q, degrees) for q in p.points)
            out.append(Primitive(p.kind, points, (), p.fill, p.stroked, p.radius))
    return tuple(out)


def bounds(prims: tuple) -> tuple:
    """The box (left, top, right, bottom) that holds every point of the primitives
    (G-016)."""
    xs = [x for p in prims for x, _ in points_of(p)]
    ys = [y for p in prims for _, y in points_of(p)]
    return (min(xs), min(ys), max(xs), max(ys))


def orientations_of(type_: str) -> tuple:
    """The orientations an icon of `type_` is drawn in: the four of the silverware
    [F95 p. 55], else only None (D-002)."""
    return ORIENTATIONS if type_ in SILVERWARE else (None,)
