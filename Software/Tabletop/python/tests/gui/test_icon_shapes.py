"""The icons' shapes (item 03; PANES.md section 6; G-016), Qt-free: every type of
`objects.TYPES` has a shape, and it stays inside `objects.footprint(type, orientation)` in
every orientation; the rotation table; the kinds and the fill roles; the module imports
no Qt."""

import sys

import pytest

from tabletop.gui.models import icon_shapes as s
from tabletop.objects import FOOTPRINTS_PX, ORIENTATIONS, SILVERWARE, TYPES, UNITS_PER_PIXEL

EPS = 1e-9


def inside(box, bounds):
    left, top, right, bottom = box
    return (bounds[0] >= left - EPS and bounds[1] >= top - EPS
            and bounds[2] <= right + EPS and bounds[3] <= bottom + EPS)


@pytest.mark.parametrize("type_", TYPES)
@pytest.mark.parametrize("scale", (1.0, 6.0 * UNITS_PER_PIXEL))
def test_every_type_has_a_shape_inside_its_footprint_box_in_every_orientation(type_, scale):
    w_px, h_px = FOOTPRINTS_PX[type_]
    w, h = w_px * scale, h_px * scale
    prims = s.primitives(type_, w, h)
    assert prims, type_
    for orientation in s.orientations_of(type_):
        turned = s.rotated(prims, orientation)
        box_w, box_h = (h, w) if orientation in ("left", "right") else (w, h)
        box = (-box_w / 2, -box_h / 2, box_w / 2, box_h / 2)
        bounds = s.bounds(turned)
        assert inside(box, bounds), (type_, orientation, bounds, box)
        # The shape fills most of its box: it is not a dot in a corner.
        assert bounds[2] - bounds[0] >= 0.9 * box_w and bounds[3] - bounds[1] >= 0.9 * box_h


def test_the_orientations_are_the_silverwares():
    for type_ in TYPES:
        expected = ORIENTATIONS if type_ in SILVERWARE else (None,)
        assert s.orientations_of(type_) == expected


def test_rotation_is_clockwise_on_the_screen_by_quarter_turns():
    assert s.ROTATION == {None: 0, "up": 0, "right": 90, "down": 180, "left": 270}
    assert s.rotate_point((1, 0), 90) == (0, 1)          # right, down on the screen
    assert s.rotate_point((0, -1), 90) == (1, 0)         # up turns to the right
    assert s.rotate_point((0, -1), 180) == (0, 1)
    assert s.rotate_point((0, -1), 270) == (-1, 0)       # up turns to the left
    assert s.rotate_point((2.5, -3), 0) == (2.5, -3)
    prims = s.primitives("fork", 6, 17)
    assert s.rotated(prims, "up") == prims and s.rotated(prims, None) == prims
    assert s.bounds(s.rotated(prims, "right")) == (-8.5, -3.0, 8.5, 3.0)


def test_primitives_have_known_kinds_and_fill_roles():
    seen_kinds, seen_fills = set(), set()
    for type_ in TYPES:
        for p in s.primitives(type_, *FOOTPRINTS_PX[type_]):
            assert p.kind in s.KINDS, (type_, p.kind)
            assert p.fill is None or p.fill in s.FILL_ROLES, (type_, p.fill)
            if p.kind == "path":
                assert p.segments and p.segments[0][0] == "move"
                assert all(seg[0] in ("move", "line", "quad", "close") for seg in p.segments)
            else:
                assert p.points
            seen_kinds.add(p.kind)
            seen_fills.add(p.fill)
    assert seen_kinds == set(s.KINDS)
    assert seen_fills >= set(s.FILL_ROLES)


def test_the_points_of_a_primitive_cover_its_extent():
    dot = s.Primitive("dot", ((1.0, 2.0),), fill="dark", stroked=False, radius=0.5)
    assert s.points_of(dot) == ((0.5, 1.5), (1.5, 2.5))
    path = s.Primitive("path", segments=(("move", (0, 0)), ("quad", (1, 2), (3, 0)), ("close",)))
    assert s.points_of(path) == ((0, 0), (1, 2), (3, 0))


def test_an_unknown_type_raises():
    with pytest.raises(ValueError, match="no icon"):
        s.primitives("teapot", 10, 10)


def test_the_pen_is_thin_and_never_under_one_unit():
    assert s.pen_width(3, 17) == 1.0
    assert s.pen_width(60, 60) == pytest.approx(60 * 0.07 + 0.6)


def test_the_module_imports_no_qt():
    source = open(s.__file__, encoding="utf-8").read()
    assert "PySide6" not in source and "shiboken" not in source
    assert "tabletop.gui.models.icon_shapes" in sys.modules
