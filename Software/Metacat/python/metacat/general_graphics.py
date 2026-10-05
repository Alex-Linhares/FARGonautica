"""general-graphics.ss: the drawing helpers shared by the panels (the engine's part).

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from general-graphics.ss, with
racket/engine/general-graphics.rktl as a worked translation.

The engine's part of general-graphics.ss, as the Racket port split it
(docs/porting-notes.md, item 13): the procedures that build SGL expressions
(pexps: circles, boxes, ovaloids, octagons, dotted, dashed, zigzag and jagged
lines, arcs, arrowheads, double arrows, grids) and the text helpers.  The model
calls them when %workspace-graphics% is on (group, bridge and rule pexps, the
codelet types' bars), and rules.ss calls find-next-space-position on every rule.
They draw nothing, never draw a random number and never import tkinter.  The
window part of the file (make-graphics-window, the scrollable text window, the
resize listener, the default colours and font) is metacat/gui/general_graphics.py.
It also holds metacat.ss's *platform* and *tcl/tk-version-8_3?*, which the dotted
and dashed shapes read (metacat/gui/sgl.py has its own copies, as racket/gui/sgl.rkt).

Pexps are Python lists (SGL data, chez.py's representation); every `(,x ,y)` of the
quasiquotes is a fresh list.  The arithmetic is Chez's (chez.add, sub, mul, div,
sqrt, acos, make-polar, magnitude, angle), so exact coordinates stay exact and
flonum coordinates are bit-identical to the original's (graphics battery).
"""
from __future__ import annotations

from fractions import Fraction as F

from metacat import chez, setup
from metacat.chez import String, add, add1, angle, div, magnitude, make_polar, make_rectangular, \
    mul, sub, sub1, zero_p
from metacat.objects import tell
from metacat.utilities import filter_out, first, rest, round_, second, square, x_coord, y_coord

# port: metacat.ss defines these for the whole program; the dotted and dashed
# shapes below read them (the oracle prelude's *platform*, and Tk 8.5)
g_platform = "linux"
g_tcl_or_tk_version = 8.5
g_tcl_or_tk_version_8_3_p = g_tcl_or_tk_version >= 8.3


pi = 3.14159265359
radians_per_degree = div(pi, 180)       # pi/180
degrees_per_radian = div(180, pi)       # 180/pi


def _nice():
    """(or %nice-graphics% (not *tcl/tk-version-8_3?*)); %nice-graphics% is
    setup.ss's, read at call time (chez: #f only is false)"""
    return setup.p_nice_graphics is not False or g_tcl_or_tk_version_8_3_p is False


def remove_leading_blanks(line):
    """general-graphics.ss: remove-leading-blanks"""
    length = len(line)
    i = 0
    while True:
        if i == length:
            return line       # 1.2: an all-blank line comes back unchanged
        if line[i].isspace():
            i += 1
        else:
            return String(line[i:length])


def break_into_lines(g, font, max_line_length, text_string):
    """general-graphics.ss: break-into-lines"""
    previous_lines = []                  # newest first, as the original's cons
    current_line = String("")
    words_left = separate_into_words(text_string)
    space_left = max_line_length
    while True:
        if len(words_left) == 0:
            lines = list(reversed([current_line] + previous_lines))
            return filter_out(lambda s: s == "", lines)
        next_word = first(words_left)
        word_length = tell(g, "get-string-width", next_word, font)
        if word_length < space_left:
            current_line = String(current_line + next_word)
            words_left = rest(words_left)
            space_left = sub(space_left, word_length)
        elif word_length >= max_line_length:
            previous_lines = [next_word, current_line] + previous_lines
            current_line = String("")
            words_left = rest(words_left)
            space_left = max_line_length
        else:
            previous_lines = [current_line] + previous_lines
            current_line = String("")
            space_left = max_line_length


def separate_into_words(text_string):
    """general-graphics.ss: separate-into-words"""
    words = []
    # Scheme's recursion as a loop
    while True:
        next_pos = find_next_space_position(text_string, 0)
        next_word = String(" " + text_string[0:next_pos])
        length = len(text_string)
        words.append(next_word)
        if next_pos == length:
            return words
        text_string = String(text_string[next_pos + 1:length])


def find_next_space_position(s, i):
    """general-graphics.ss: find-next-space-position"""
    # Scheme's tail recursion as a loop
    while True:
        if i >= len(s):
            return len(s)
        if s[i] == " ":
            return i
        i += 1


# ------------------------------- Circles etc. -------------------------------

def circle(center, diameter):
    """general-graphics.ss: circle"""
    return ["arc", center, [diameter, diameter], 0, 360]


def disk(center, diameter):
    """general-graphics.ss: disk"""
    return ["filled-arc", center, [diameter, diameter], 0, 360]


def pie_slice(center, diameter, start_deg, sweep_deg):
    """general-graphics.ss: pie-slice"""
    return ["filled-arc", center, [diameter, diameter], start_deg, sweep_deg]


# --------------------------------- Boxes ------------------------------------

def outline_box(x0, y0, x1, y1):
    """general-graphics.ss: outline-box"""
    return ["rectangle", [x0, y0], [x1, y1]]


def solid_box(x0, y0, x1, y1):
    """general-graphics.ss: solid-box"""
    return ["filled-rectangle", [x0, y0], [x1, y1]]


def _ovaloid_parts(xc, yc, width, height, ovalness):
    x1 = sub(xc, mul(F(1, 2), width))
    x2 = add(xc, mul(F(1, 2), width))
    interior_height = mul(height, sub(1, ovalness))
    y1 = sub(yc, mul(F(1, 2), interior_height))
    y2 = add(yc, mul(F(1, 2), interior_height))
    exterior_height = sub(height, interior_height)
    return x1, x2, y1, y2, exterior_height


def centered_ovaloid(xc, yc, width, height, ovalness):
    """general-graphics.ss: centered-ovaloid"""
    x1, x2, y1, y2, exterior_height = _ovaloid_parts(xc, yc, width, height, ovalness)
    return ["let-sgl", [],
            ["arc", [xc, y2], [width, exterior_height], 0, 180],
            ["line", [x2, y1], [x2, y2]],
            ["arc", [xc, y1], [width, exterior_height], 180, 180],
            ["line", [x1, y2], [x1, y1]]]


def filled_centered_ovaloid(xc, yc, width, height, ovalness):
    """general-graphics.ss: filled-centered-ovaloid"""
    x1, x2, y1, y2, exterior_height = _ovaloid_parts(xc, yc, width, height, ovalness)
    return ["let-sgl", [],
            ["filled-arc", [xc, y2], [width, exterior_height], 0, 180],
            ["filled-rectangle", [x1, y1], [x2, y2]],
            ["filled-arc", [xc, y1], [width, exterior_height], 180, 180]]


def _rounded_box_parts(xc, yc, width, height, corner_radius):
    left = sub(xc, mul(F(1, 2), width))
    right = add(xc, mul(F(1, 2), width))
    top = add(yc, mul(F(1, 2), height))
    bottom = sub(yc, mul(F(1, 2), height))
    x1 = add(left, corner_radius)
    x2 = sub(right, corner_radius)
    y1 = add(bottom, corner_radius)
    y2 = sub(top, corner_radius)
    corner_diameter = mul(2, corner_radius)
    return left, right, top, bottom, x1, x2, y1, y2, corner_diameter


def centered_rounded_box(xc, yc, width, height, corner_radius):
    """general-graphics.ss: centered-rounded-box"""
    left, right, top, bottom, x1, x2, y1, y2, d = _rounded_box_parts(
        xc, yc, width, height, corner_radius)
    return ["let-sgl", [],
            ["line", [x1, bottom], [x2, bottom]],
            ["arc", [x2, y1], [d, d], 270, 90],
            ["line", [right, y1], [right, y2]],
            ["arc", [x2, y2], [d, d], 0, 90],
            ["line", [x2, top], [x1, top]],
            ["arc", [x1, y2], [d, d], 90, 90],
            ["line", [left, y2], [left, y1]],
            ["arc", [x1, y1], [d, d], 180, 90]]


def filled_centered_rounded_box(xc, yc, width, height, corner_radius, pixel):
    """general-graphics.ss: filled-centered-rounded-box"""
    left, right, top, bottom, x1, x2, y1, y2, d = _rounded_box_parts(
        xc, yc, width, height, corner_radius)
    return ["let-sgl", [],
            ["filled-rectangle", [left, sub(y1, pixel)], [right, add(y2, pixel)]],
            ["filled-rectangle", [sub(x1, pixel), bottom], [add(x2, pixel), top]],
            ["filled-arc", [x2, y1], [d, d], 270, 90],
            ["filled-arc", [x2, y2], [d, d], 0, 90],
            ["filled-arc", [x1, y2], [d, d], 90, 90],
            ["filled-arc", [x1, y1], [d, d], 180, 90]]


# -------------------------------- Octagons ----------------------------------

def _octagon_points(xc, yc, width):
    a = mul(F(1, 2), width)
    b = mul(a, div(1, add(1, chez.sqrt(2))))
    x1, x2, x3, x4 = sub(xc, a), sub(xc, b), add(xc, b), add(xc, a)
    y1, y2, y3, y4 = sub(yc, a), sub(yc, b), add(yc, b), add(yc, a)
    return [[x2, y1], [x3, y1], [x4, y2], [x4, y3], [x3, y4], [x2, y4], [x1, y3], [x1, y2]]


def centered_octagon(xc, yc, width):
    """general-graphics.ss: centered-octagon"""
    return ["polygon"] + _octagon_points(xc, yc, width)


def filled_centered_octagon(xc, yc, width):
    """general-graphics.ss: filled-centered-octagon"""
    return ["filled-polygon"] + _octagon_points(xc, yc, width)


# -------------------------- Dotted lines & boxes ----------------------------

p_dot_interval = F(1, 125)


def dotted_line(x1, y1, x2, y2, approx_interval_length):
    """general-graphics.ss: dotted-line"""
    if _nice():
        return ["polypoints"] + dotted_line_points(x1, y1, x2, y2, approx_interval_length)
    return ["let-sgl", [["line-style", "dotted"]],
            ["line", [x1, y1], [x2, y2]]]


def dotted_box(x1, y1, x2, y2, approx_interval_length):
    """general-graphics.ss: dotted-box"""
    if _nice():
        return (["polypoints"]
                + dotted_line_points(x1, y1, x1, y2, approx_interval_length)
                + dotted_line_points(x1, y2, x2, y2, approx_interval_length)
                + dotted_line_points(x2, y1, x2, y2, approx_interval_length)
                + dotted_line_points(x1, y1, x2, y1, approx_interval_length))
    return ["let-sgl", [["line-style", "dotted"]],
            ["rectangle", [x1, y1], [x2, y2]]]


def _line_transform(p1, l, origin):
    """The coord-transform of the line-point procedures: a point p of the line
    laid along the real axis, rotated onto l and moved to p1 (origin for p = 0).
    (angle l) is computed at each call, as in the original: for a line of
    length 0 it is never reached (it would raise)."""
    def coord_transform(p):
        if zero_p(p):
            return list(origin)
        p_prime = add(p1, make_polar(magnitude(p), add(angle(l), angle(p))))
        return [x_coord(p_prime), y_coord(p_prime)]
    return coord_transform


def dotted_line_points(x1, y1, x2, y2, approx_interval_length):
    """general-graphics.ss: dotted-line-points"""
    p1 = make_rectangular(x1, y1)
    p2 = make_rectangular(x2, y2)
    l = sub(p2, p1)
    n = chez.max_(1, round_(div(magnitude(l), approx_interval_length)))
    exact_interval_length = div(magnitude(l), n)
    coord_transform = _line_transform(p1, l, [x1, y1])
    # the letrec conses from i = n down to 0 (the transforms only compute)
    return [coord_transform(make_rectangular(mul(i, exact_interval_length), 0))
            for i in range(n + 1)]


# ----------------------------- Dashed lines & boxes ----------------------------------

# density parameter ranges from 0 (no dashes) to 1 (no spaces).
# dash-length parameter is the length of an individual dash in world-coordinates.

p_dash_length = F(1, 200)
p_dash_density = F(1, 2)


def dashed_line(x1, y1, x2, y2, density, approx_dash_length):
    """general-graphics.ss: dashed-line"""
    if _nice():
        return ["line"] + dashed_line_points(x1, y1, x2, y2, density, approx_dash_length)
    return ["let-sgl", [["line-style", "dashed"]],
            ["line", [x1, y1], [x2, y2]]]


def dashed_box(x1, y1, x2, y2, density, approx_dash_length):
    """general-graphics.ss: dashed-box"""
    if _nice():
        return (["line"]
                + dashed_line_points(x1, y1, x1, y2, density, approx_dash_length)
                + dashed_line_points(x1, y2, x2, y2, density, approx_dash_length)
                + dashed_line_points(x2, y1, x2, y2, density, approx_dash_length)
                + dashed_line_points(x1, y1, x2, y1, density, approx_dash_length))
    return ["let-sgl", [["line-style", "dashed"]],
            ["rectangle", [x1, y1], [x2, y2]]]


def dashed_line_points(x1, y1, x2, y2, density, approx_dash_length):
    """general-graphics.ss: dashed-line-points"""
    p1 = make_rectangular(x1, y1)
    p2 = make_rectangular(x2, y2)
    l = sub(p2, p1)
    total_dash_length = mul(magnitude(l), density)
    total_space_length = mul(magnitude(l), sub(1, density))
    n = chez.max_(3, round_(div(total_dash_length, approx_dash_length)))
    dash_length = div(total_dash_length, n)
    space_length = div(total_space_length, sub1(n))
    interval_length = add(dash_length, space_length)
    coord_transform = _line_transform(p1, l, [x1, y1])
    points = []
    for i in range(n):          # the letrec conses from i = n - 1 down to 0
        points.append(coord_transform(make_rectangular(mul(i, interval_length), 0)))
        points.append(coord_transform(
            make_rectangular(add(mul(i, interval_length), dash_length), 0)))
    return points


# ---------------------------- Zigzag lines -------------------------------

def zigzag_line(p1, p2, approx_zigzag_length, sign):
    """general-graphics.ss: zigzag-line (sign is chez.add or chez.sub)"""
    return ["polyline"] + zigzag_line_points(p1, p2, approx_zigzag_length, sign)


def centered_zigzag_line(p1, p2, approx_zigzag_length, sign):
    """general-graphics.ss: centered-zigzag-line"""
    return ["polyline"] + centered_zigzag_line_points(p1, p2, approx_zigzag_length, sign)


def zigzag_line_points(p1, p2, approx_zigzag_length, sign):
    """general-graphics.ss: zigzag-line-points"""
    l = sub(p2, p1)
    n = chez.max_(1, round_(div(magnitude(l), approx_zigzag_length)))
    period = make_rectangular(div(magnitude(l), n), 0)
    delta = div(magnitude(period), 2)
    zig = make_rectangular(delta, sign(delta))
    coord_transform = _line_transform(p1, l, [x_coord(p1), y_coord(p1)])
    points = []
    for i in range(n):          # the letrec conses from i = n - 1 down to 0
        points.append(coord_transform(mul(i, period)))
        points.append(coord_transform(add(mul(i, period), zig)))
    points.append([x_coord(p2), y_coord(p2)])
    return points


def centered_zigzag_line_points(p1, p2, approx_zigzag_length, sign):
    """general-graphics.ss: centered-zigzag-line-points"""
    l = sub(p2, p1)
    n = round_(div(magnitude(l), approx_zigzag_length))
    period = make_rectangular(div(magnitude(l), n), 0)
    delta = div(magnitude(period), 4)
    opp = sub if sign is add else add          # (if (eq? sign +) - +)
    zig = make_rectangular(delta, sign(delta))
    zag = make_rectangular(mul(3, delta), opp(delta))
    coord_transform = _line_transform(p1, l, [x_coord(p1), y_coord(p1)])
    points = [[x_coord(p1), y_coord(p1)]]
    for i in range(n):          # the letrec conses from i = n - 1 down to 0
        points.append(coord_transform(add(mul(i, period), zig)))
        points.append(coord_transform(add(mul(i, period), zag)))
    points.append([x_coord(p2), y_coord(p2)])
    return points


# -------------------------------- Jagged lines --------------------------------
# not used in Metacat

def jagged_line(x1, y1, x2, y2, approx_jag_width):
    """general-graphics.ss: jagged-line"""
    return ["polyline"] + jagged_line_points(x1, y1, x2, y2, approx_jag_width)


def jagged_line_points(x1, y1, x2, y2, approx_jag_width):
    """general-graphics.ss: jagged-line-points"""
    x_len = sub(x2, x1)
    y_len = sub(y2, y1)
    short_len = chez.min_(chez.abs_(x_len), chez.abs_(y_len))
    n = chez.max_(5, round_(div(short_len, approx_jag_width)))
    x_delta = div(x_len, n)
    y_delta = div(y_len, add1(n))
    points = []
    for i in range(n + 1):      # the letrec conses from i = n down to 0
        x = add(x1, mul(i, x_delta))
        y = add(y1, mul(i, y_delta))
        points.append([x, y])
        points.append([x, add(y, y_delta)])
    return points


# ------------------------------- Circular arcs ---------------------------------
# not used in Metacat

# These routines draw circular arcs clockwise from point x1,y1 to point x2,y2.
# height is the maximum height of the arc from the line connecting the endpoints.

def circular_arc(x1, y1, x2, y2, height):
    """general-graphics.ss: circular-arc"""
    if height <= 0:
        return ["line", [x1, y1], [x2, y2]]
    p1 = make_rectangular(x1, y1)
    p2 = make_rectangular(x2, y2)
    l = sub(p2, p1)
    radius = add(div(square(magnitude(l)), mul(8, height)), div(height, 2))
    d = mul(2, radius)
    alpha = mul(2, chez.acos(sub(1, div(height, radius))))
    beta = div(sub(pi, alpha), 2)
    origin = add(p1, make_polar(radius, sub(angle(l), beta)))
    start = add(beta, angle(l))
    start_deg = mul(degrees_per_radian, start)
    alpha_deg = mul(degrees_per_radian, alpha)
    return ["arc", [x_coord(origin), y_coord(origin)], [d, d], start_deg, alpha_deg]


def dotted_circular_arc(x1, y1, x2, y2, height, approx_interval_length):
    """general-graphics.ss: dotted-circular-arc"""
    if height <= 0:
        return dotted_line(x1, y1, x2, y2, approx_interval_length)
    if _nice():
        return ["polypoints"] + circular_arc_points(x1, y1, x2, y2, height, approx_interval_length)
    return ["let-sgl", [["line-style", "dotted"]], circular_arc(x1, y1, x2, y2, height)]


def _dashes_work():
    """(and *tcl/tk-version-8_3?* (not (eq? *platform* 'macintosh)))"""
    return g_tcl_or_tk_version_8_3_p is not False and g_platform != "macintosh"


def dashed_circular_arc(x1, y1, x2, y2, height):
    """general-graphics.ss: dashed-circular-arc"""
    if height <= 0:
        return dashed_line(x1, y1, x2, y2, p_dash_density, p_dash_length)
    # dashed lines don't seem to work on the Mac even with Tcl/Tk 8.4.4,
    # so just use dashed-polypoints for now
    if _dashes_work():
        return ["let-sgl", [["line-style", "dashed"]], circular_arc(x1, y1, x2, y2, height)]
    return ["dashed-polypoints"] + circular_arc_points(x1, y1, x2, y2, height, p_dot_interval)


def circular_arc_points(x1, y1, x2, y2, height, approx_interval_length):
    """general-graphics.ss: circular-arc-points"""
    p1 = make_rectangular(x1, y1)
    p2 = make_rectangular(x2, y2)
    l = sub(p2, p1)
    r = add(div(square(magnitude(l)), mul(8, height)), div(height, 2))
    alpha = mul(2, chez.acos(sub(1, div(height, r))))
    beta = div(sub(pi, alpha), 2)
    origin = add(p1, make_polar(r, sub(angle(l), beta)))
    start = add(beta, angle(l))
    arc_length = mul(r, alpha)
    n = chez.max_(3, round_(div(arc_length, approx_interval_length)))
    angle_delta = div(alpha, n)

    def arc_point(i):
        p = add(origin, make_polar(r, add(mul(i, angle_delta), start)))
        return [x_coord(p), y_coord(p)]
    return [arc_point(i) for i in range(n + 1)]   # consed from n down to 0


# -------------------------- Elliptical arcs --------------------------

def elliptical_arc(x1, y1, x2, y2, height):
    """general-graphics.ss: elliptical-arc"""
    y_orig = chez.min_(y1, y2)
    y = chez.abs_(sub(y1, y2))
    b = chez.max_(y, height)
    if zero_p(b):
        return ["line", [x1, y1], [x2, y2]]
    root = chez.sqrt(sub(1, div(square(y), square(b))))
    a = div(sub(x2, x1), add(1, root))
    x_orig = add(x1, a) if y1 == y_orig else sub(x2, a)
    theta_deg = mul(degrees_per_radian, chez.acos(root))
    start_deg = theta_deg if y1 == y_orig else 0
    return ["arc", [x_orig, y_orig], [mul(2, a), mul(2, b)], start_deg, sub(180, theta_deg)]


def dotted_elliptical_arc(x1, y1, x2, y2, height, approx_interval_length):
    """general-graphics.ss: dotted-elliptical-arc"""
    if y1 == y2 and height <= 0:
        return dotted_line(x1, y1, x2, y2, approx_interval_length)
    if _nice():
        return ["polypoints"] + elliptical_arc_points(x1, y1, x2, y2, height,
                                                      approx_interval_length)
    return ["let-sgl", [["line-style", "dotted"]], elliptical_arc(x1, y1, x2, y2, height)]


def dashed_elliptical_arc(x1, y1, x2, y2, height):
    """general-graphics.ss: dashed-elliptical-arc"""
    if y1 == y2 and height <= 0:
        return dashed_line(x1, y1, x2, y2, p_dash_density, p_dash_length)
    # dashed lines don't seem to work on the Mac even with Tcl/Tk 8.4.4,
    # so just use dashed-polypoints for now
    if _dashes_work():
        return ["let-sgl", [["line-style", "dashed"]], elliptical_arc(x1, y1, x2, y2, height)]
    return ["dashed-polypoints"] + elliptical_arc_points(x1, y1, x2, y2, height, p_dot_interval)


def elliptical_arc_points(x1, y1, x2, y2, height, approx_interval_length):
    """general-graphics.ss: elliptical-arc-points"""
    y_orig = chez.min_(y1, y2)
    y = chez.abs_(sub(y1, y2))
    b = chez.max_(y, height)
    root = chez.sqrt(sub(1, div(square(y), square(b))))
    a = div(sub(x2, x1), add(1, root))
    x_orig = add(x1, a) if y1 == y_orig else sub(x2, a)
    origin = make_rectangular(x_orig, y_orig)
    theta = chez.acos(root)
    alpha = sub(pi, theta)
    start = theta if y1 == y_orig else 0
    arc_length = mul(chez.sqrt(mul(a, b)), alpha)
    n = chez.max_(3, round_(div(arc_length, approx_interval_length)))
    angle_delta = div(alpha, n)

    def arc_point(i):
        phi = add(start, mul(i, angle_delta))
        p = add(origin, make_rectangular(mul(a, chez.cos(phi)), mul(b, chez.sin(phi))))
        return [x_coord(p), y_coord(p)]
    return [arc_point(i) for i in range(n + 1)]   # consed from n down to 0


# ------------------------------- Arrows ---------------------------------
#
# orientation-angle is measured counterclockwise from the horizontal
# (0 degrees = pointing to the right); angles are specified in degrees.
# (x0 y0) is the arrowhead tip coordinate.

def arrowhead(x0, y0, orientation_angle, arrowhead_length, arrowhead_angle_size):
    """general-graphics.ss: arrowhead"""
    theta = mul(radians_per_degree, orientation_angle)
    alpha = mul(radians_per_degree, arrowhead_angle_size)
    half_width = mul(arrowhead_length, chez.tan(div(alpha, 2)))

    def coord_transform(x, y):
        x_prime = sub(mul(x, chez.cos(theta)), mul(y, chez.sin(theta)))
        y_prime = add(mul(y, chez.cos(theta)), mul(x, chez.sin(theta)))
        return [add(x0, x_prime), add(y0, y_prime)]
    return ["line", [x0, y0], coord_transform(sub(arrowhead_length), sub(half_width)),
            [x0, y0], coord_transform(sub(arrowhead_length), 0),
            [x0, y0], coord_transform(sub(arrowhead_length), half_width)]


def centered_double_arrow(x, y, orientation_angle, arrow_length, arrow_width,
                          arrowhead_length, arrowhead_angle_size):
    """general-graphics.ss: centered-double-arrow"""
    theta = mul(radians_per_degree, orientation_angle)
    alpha = mul(radians_per_degree, arrowhead_angle_size)
    half_arrowhead_width = mul(arrowhead_length, chez.tan(div(alpha, 2)))
    half_arrow_width = div(arrow_width, 2)
    overhang = div(half_arrow_width, chez.tan(div(alpha, 2)))
    left = sub(div(arrow_length, 2))
    right = div(arrow_length, 2)
    right_line = sub(right, overhang)

    def coord_transform(p):
        x_prime = sub(mul(first(p), chez.cos(theta)), mul(second(p), chez.sin(theta)))
        y_prime = add(mul(second(p), chez.cos(theta)), mul(first(p), chez.sin(theta)))
        return [add(x, x_prime), add(y, y_prime)]
    points = [[left, half_arrow_width], [right_line, half_arrow_width],
              [left, sub(half_arrow_width)], [right_line, sub(half_arrow_width)],
              [sub(right, arrowhead_length), half_arrowhead_width], [right, 0],
              [sub(right, arrowhead_length), sub(half_arrowhead_width)], [right, 0]]
    return ["line"] + [coord_transform(p) for p in points]


def centered_double_headed_double_arrow(x, y, orientation_angle, arrow_length, arrow_width,
                                        arrowhead_length, arrowhead_angle_size):
    """general-graphics.ss: centered-double-headed-double-arrow"""
    x_delta = mul(F(1, 4), arrow_length, chez.cos(mul(radians_per_degree, orientation_angle)))
    y_delta = mul(F(1, 4), arrow_length, chez.sin(mul(radians_per_degree, orientation_angle)))
    return ["let-sgl", [],
            centered_double_arrow(add(x, x_delta), add(y, y_delta), orientation_angle,
                                  mul(F(1, 2), arrow_length), arrow_width, arrowhead_length,
                                  arrowhead_angle_size),
            centered_double_arrow(sub(x, x_delta), sub(y, y_delta), add(180, orientation_angle),
                                  mul(F(1, 2), arrow_length), arrow_width, arrowhead_length,
                                  arrowhead_angle_size)]


# --------------------------------- Grids ------------------------------------
# not used in Metacat

# style = solid | dashed | dotted

def grid(g, style, delta):
    """general-graphics.ss: grid"""
    xmax = tell(g, "get-x-max")
    ymax = tell(g, "get-y-max")
    dot_interval = mul(5, tell(g, "get-width-per-pixel"))

    def line(x0, y0, x1, y1):
        if style in ("solid", "dashed"):
            return ["line", [x0, y0], [x1, y1]]
        if style == "dotted":
            return dotted_line(x0, y0, x1, y1, dot_interval)
        return None             # 1.2: a case without else
    x, y, lines = 0, 0, []
    # the named let as a loop
    while True:
        if x > xmax and y > ymax:
            if style == "dashed":
                return ["let-sgl", [["line-style", "dashed"]]] + lines
            return ["let-sgl", []] + lines
        if x > xmax:
            x, y, lines = x, add(y, delta), [line(0, y, xmax, y)] + lines
        elif y > ymax:
            x, y, lines = add(x, delta), y, [line(x, 0, x, ymax)] + lines
        else:
            x, y, lines = add(x, delta), add(y, delta), [line(0, y, xmax, y), line(x, 0, x, ymax)] + lines
