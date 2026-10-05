"""A Tk canvas's display list, without Tk or Qt: the model of the Qt canvas.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026), from Tk 8.6's canvas (tkCanvas.c,
tkRectOval.c, tkArc.c, tkLine.c, tkPolygon.c, tkCanvText.c), for loop0003 item 02.

The panels draw by sending the original's Tk canvas commands (``tcl(*args)``,
python/tests/data/tk-canvas-commands.json lists them).  ``DisplayList.tcl`` takes
those commands as Tcl words (``swl.tcl_word``: str, int, float, tuples) and keeps
what Tk would keep: items with ids, kinds, coordinates, options and tags, in
stacking order.  It answers Tk's queries as Tk does, ``bbox`` included: the
bounding boxes follow Tk's own rules (the outline's bloat, the extra pixels,
the rounding of each kind), and text extents come from a measurer (the fonts
the Qt canvas draws with, metacat/qt/fonts.py).  Every command runs under one
lock, so any thread may draw; the Qt scene (metacat/qt/canvas.py) is updated
from the changes recorded here, on the GUI thread.

The commands are those the panels send (create line, rectangle, oval, arc,
polygon and text; delete, move, raise, lower, itemconfigure, scale, bbox,
canvasx, canvasy), plus Tk's queries find all, type, coords, itemcget and
gettags (the tests read the display list through them).  Tag searches take an
id, a tag or ``all``; Tk's tag expressions are not needed.
"""
from __future__ import annotations

import math
import threading


class TclError(Exception):
    """port: a Tk error (tkinter's TclError), with Tk's message"""


KINDS = ("line", "rectangle", "oval", "arc", "polygon", "text")

# Tk 8.6's defaults, as itemcget reports them
_COMMON = {"-state": "", "-tags": ()}
DEFAULTS = {
    "line": {"-fill": "black", "-width": 1.0, "-dash": "", "-arrow": "none",
             "-smooth": "0", **_COMMON},
    "rectangle": {"-fill": "", "-outline": "black", "-width": 1.0, "-dash": "", **_COMMON},
    "oval": {"-fill": "", "-outline": "black", "-width": 1.0, "-dash": "", **_COMMON},
    "arc": {"-fill": "", "-outline": "black", "-width": 1.0, "-dash": "", "-start": 0.0,
            "-extent": 90.0, "-style": "pieslice", **_COMMON},
    "polygon": {"-fill": "black", "-outline": "", "-width": 1.0, "-dash": "", "-smooth": "0",
                **_COMMON},
    "text": {"-anchor": "center", "-fill": "black", "-font": "TkDefaultFont", "-text": "",
             "-justify": "left", **_COMMON},
}
COORDS = {"line": (4, None), "rectangle": (4, 4), "oval": (4, 4), "arc": (4, 4),
          "polygon": (6, None), "text": (2, 2)}
_ENUMS = {
    "-state": ("", "normal", "hidden", "disabled"),
    "-style": ("pieslice", "chord", "arc"),
    "-anchor": ("n", "ne", "e", "se", "s", "sw", "w", "nw", "center"),
    "-justify": ("left", "right", "center"),
    "-arrow": ("none", "first", "last", "both"),
}
_COLOR_OPTIONS = ("-fill", "-outline")
_FLOAT_OPTIONS = ("-width", "-start", "-extent")
_TRUE = ("1", "true", "yes", "on", "bezier", "raw")
_FALSE = ("0", "false", "no", "off", "")
# Tk's default -arrowshape (8 10 3) and -splinesteps
ARROW_SHAPE = (8.0, 10.0, 3.0)
SPLINE_STEPS = 12


# ---------------------------------------------------------------------------
# Colours (Tk 8.6: its own X11 table, case-blind, and #rgb forms scaled)

_colors = None


def _color_table():
    global _colors
    if _colors is None:
        from metacat.gui import colors
        table = {name.lower(): (r, g, b) for name, r, g, b in colors._COLOR_TABLE}
        # Tk 8.6 (TIP 403) uses the web's values for these five, unlike X11's
        # rgb.txt and constants.ss (docs/anomalies_and_quirks.md)
        table.update({"gray": (128, 128, 128), "grey": (128, 128, 128),
                      "green": (0, 128, 0), "maroon": (128, 0, 0), "purple": (128, 0, 128)})
        _colors = table
    return _colors


_rgb_cache = {}


def color_rgb(name):
    """port: Tk_GetColor: a colour word as 8-bit (r, g, b); TclError if unknown"""
    name = str(name)
    rgb = _rgb_cache.get(name)
    if rgb is None:
        rgb = _rgb_cache[name] = _color_rgb(name)
    return rgb


def _color_rgb(name):
    if name.startswith("#"):
        digits = name[1:]
        n = len(digits) // 3
        if len(digits) in (3, 6, 9, 12) and all(c in "0123456789abcdefABCDEF" for c in digits):
            parts = [int(digits[i * n:(i + 1) * n], 16) for i in range(3)]
            if n == 1:
                return tuple(p * 17 for p in parts)
            return tuple(p >> (4 * (n - 2)) for p in parts)
    else:
        rgb = _color_table().get(name.lower())
        if rgb is not None:
            return rgb
    raise TclError('unknown color name "%s"' % name)


# ---------------------------------------------------------------------------
# Words

def _number(x):
    if isinstance(x, (int, float)):
        return float(x)
    try:
        return float(str(x))
    except ValueError:
        raise TclError('expected floating-point number but got "%s"' % x) from None


def _is_option(x):
    s = x if isinstance(x, str) else None
    return s is not None and len(s) > 1 and s[0] == "-" and s[1].isalpha()


def tcl_list(words):
    """port: a Tcl list's string form (Tcl_Merge, for simple words)"""
    if isinstance(words, str):
        return words
    out = []
    for w in words:
        w = str(w)
        out.append("{%s}" % w if (not w or any(c.isspace() or c in "{}\"" for c in w)) else w)
    return " ".join(out)


def split_list(x):
    """port: Tcl_SplitList, for the words the panels pass (tuples, or plain
    strings of words and {braced words})"""
    if isinstance(x, (list, tuple)):
        return [str(a) for a in x]
    s, out, i = str(x), [], 0
    while i < len(s):
        if s[i].isspace():
            i += 1
        elif s[i] == "{":
            depth, j = 1, i + 1
            while depth and j < len(s):
                depth += {"{": 1, "}": -1}.get(s[j], 0)
                j += 1
            out.append(s[i + 1:j - 1])
            i = j
        else:
            j = i
            while j < len(s) and not s[j].isspace():
                j += 1
            out.append(s[i:j])
            i = j
    return out


def _round(v):
    """port: Tk's ROUND macro (floor(v + 0.5))"""
    return int(math.floor(v + 0.5))


def _int(v):
    """port: C's (int) cast: truncation toward zero"""
    return int(v)


def _include(box, x, y):
    """port: TkIncludePoint"""
    tx, ty = _int(x + 0.5), _int(y + 0.5)
    box[0], box[2] = min(box[0], tx), max(box[2], tx)
    box[1], box[3] = min(box[1], ty), max(box[3], ty)


def dash_lengths(dash, width):
    """port: tkCanvUtil.c's DashConvert: a -dash word as on/off lengths in
    pixels (an empty list: a solid line)"""
    if isinstance(dash, (list, tuple)):
        return [int(v) for v in dash]
    int_width = max(1, _int(width + 0.5))
    out = []
    for ch in str(dash):
        if ch == " ":
            if out:
                out[-1] += int_width + 1
            continue
        size = {"_": 8, "-": 6, ",": 4, ".": 2}.get(ch)
        if size is None:
            raise TclError('bad dash value "%s": must be a list of integers or a format '
                           'like "-.."' % dash)
        out += [size * int_width, 4 * int_width]
    return out


# ---------------------------------------------------------------------------
# Items

class Item:
    """one canvas item: id, kind, coordinates (the line's arrow tips included),
    options (Tk's defaults for those not given) and z (its stacking key)"""
    __slots__ = ("id", "kind", "coords", "opts", "z")

    def __init__(self, item_id, kind, coords, opts, z):
        self.id, self.kind, self.coords, self.opts, self.z = item_id, kind, coords, opts, z

    def copy(self):
        return Item(self.id, self.kind, list(self.coords), dict(self.opts), self.z)

    @property
    def tags(self):
        return self.opts["-tags"]

    @property
    def state(self):
        return self.opts["-state"]

    def color(self, option):
        """the 8-bit (r, g, b) of a colour option, or None for no colour"""
        value = self.opts[option]
        return color_rgb(value) if str(value) else None

    def smooth(self):
        return str(self.opts["-smooth"]).lower() not in _FALSE

    # --- geometry, as Tk computes it

    def arc_points(self):
        """port: tkArc.c's ComputeArcOutline: the arc's two end points and the
        centre of its oval"""
        x1, y1, x2, y2 = self.coords
        start, extent = self.opts["-start"], self.opts["-extent"]
        a1 = -start * math.pi / 180.0
        a2 = a1 - extent * math.pi / 180.0
        cx, cy = (x1 + x2) / 2.0, (y1 + y2) / 2.0
        w, h = x2 - x1, y2 - y1
        return ((cx + math.cos(a1) * w / 2.0, cy + math.sin(a1) * h / 2.0),
                (cx + math.cos(a2) * w / 2.0, cy + math.sin(a2) * h / 2.0), (cx, cy))

    def line_geometry(self):
        """port: tkLine.c's ConfigureArrows: the line's points, shortened at its
        arrowed ends, and the arrowheads' polygons (6 points, or None)"""
        pts = [[self.coords[i], self.coords[i + 1]] for i in range(0, len(self.coords), 2)]
        arrow = self.opts["-arrow"]
        if arrow == "none" or len(pts) < 2:
            return pts, None, None
        width = self.opts["-width"]
        shape_a, shape_b = ARROW_SHAPE[0] + 0.001, ARROW_SHAPE[1] + 0.001
        shape_c = ARROW_SHAPE[2] + width / 2.0 + 0.001
        frac = (width / 2.0) / shape_c
        backup = frac * shape_b + shape_a * (1.0 - frac) / 2.0

        def head(tip, toward):
            dx, dy = tip[0] - toward[0], tip[1] - toward[1]
            length = math.hypot(dx, dy)
            sin_t, cos_t = (0.0, 0.0) if length == 0 else (dy / length, dx / length)
            vx, vy = tip[0] - shape_a * cos_t, tip[1] - shape_a * sin_t
            t = shape_c * sin_t
            p1x = tip[0] - shape_b * cos_t + t
            p4x = p1x - 2 * t
            t = shape_c * cos_t
            p1y = tip[1] - shape_b * sin_t - t
            p4y = p1y + 2 * t
            poly = [tuple(tip), (p1x, p1y),
                    (p1x * frac + vx * (1.0 - frac), p1y * frac + vy * (1.0 - frac)),
                    (p4x * frac + vx * (1.0 - frac), p4y * frac + vy * (1.0 - frac)),
                    (p4x, p4y), tuple(tip)]
            return poly, [tip[0] - backup * cos_t, tip[1] - backup * sin_t]

        first = last = None
        if arrow != "last":
            first, pts[0] = head(pts[0], pts[1])
        if arrow != "first":
            last, pts[-1] = head(pts[-1], pts[-2])
        return pts, first, last

    def text_layout(self, measure):
        """port: tkCanvText.c's ComputeTextBbox: (left x, top y, width, height,
        [(line, width)])"""
        font = self.opts["-font"]
        lines = str(self.opts["-text"]).split("\n")
        widths = [measure.text_width(font, line) for line in lines]
        width, height = max(widths), measure.linespace(font) * len(lines)
        left, top = _round(self.coords[0]), _round(self.coords[1])
        anchor = self.opts["-anchor"]
        if anchor in ("w", "center", "e"):
            top -= height // 2
        elif anchor in ("sw", "s", "se"):
            top -= height
        if anchor in ("n", "center", "s"):
            left -= width // 2
        elif anchor in ("ne", "e", "se"):
            left -= width
        return left, top, width, height, list(zip(lines, widths))

    def bbox(self, measure, ignore_state=False):
        """port: the item's Tk bounding box [x1, y1, x2, y2] (header.x1...), or
        None when Tk leaves it empty (hidden items, texts without colour)"""
        hidden = self.state == "hidden" and not ignore_state
        kind, c = self.kind, self.coords
        if kind in ("rectangle", "oval"):
            if hidden:
                return None
            bloat = 0 if self.color("-outline") is None else _int(self.opts["-width"] + 1) // 2
            x2 = max(c[2], c[0] + 1)
            y2 = max(c[3], c[1] + 1)
            return [_c_round(c[0]) - bloat, _c_round(c[1]) - bloat,
                    _c_round(x2) + bloat, _c_round(y2) + bloat]
        if kind == "arc":
            if hidden:
                return None
            width = max(self.opts["-width"], 1.0)
            p1, p2, centre = self.arc_points()
            box = [_int(p1[0]), _int(p1[1]), _int(p1[0]), _int(p1[1])]
            _include(box, *p2)
            if self.opts["-style"] == "pieslice":
                _include(box, *centre)
            start, extent = self.opts["-start"], self.opts["-extent"]
            for angle, point in ((0.0, (c[2], centre[1])), (90.0, (centre[0], c[1])),
                                 (180.0, (c[0], centre[1])), (270.0, (centre[0], c[3]))):
                tmp = angle - start
                if tmp < 0:
                    tmp += 360.0
                if tmp < extent or (tmp - 360) > extent:
                    _include(box, *point)
            grow = 1 if self.color("-outline") is None else _int((width + 1.0) / 2.0 + 1)
            return [box[0] - grow, box[1] - grow, box[2] + grow, box[3] + grow]
        if kind == "line":
            if hidden or not c:
                return None
            pts, first, last = self.line_geometry()
            box = [_int(pts[0][0]), _int(pts[0][1]), _int(pts[0][0]), _int(pts[0][1])]
            for p in pts[1:]:
                _include(box, *p)
            if first:
                _include(box, *first[0])
            if last:
                _include(box, *last[0])
            w = _int(max(self.opts["-width"], 1.0) + 0.5)
            box = [box[0] - w, box[1] - w, box[2] + w, box[3] + w]
            for poly in (first, last):
                for p in poly or ():
                    _include(box, *p)
            return [box[0] - 1, box[1] - 1, box[2] + 1, box[3] + 1]
        if kind == "polygon":
            if hidden or not c:
                return None
            box = [_int(c[0]), _int(c[1]), _int(c[0]), _int(c[1])]
            for i in range(2, len(c), 2):
                _include(box, c[i], c[i + 1])
            if self.color("-outline") is not None:
                w = _int(self.opts["-width"] + 1.0) // 2
                box = [box[0] - w, box[1] - w, box[2] + w, box[3] + w]
            return [box[0] - 1, box[1] - 1, box[2] + 1, box[3] + 1]
        # text: the insert cursor's fudge, (insertwidth 2 + 1) / 2 = 1 pixel
        left, top, width, height, _ = self.text_layout(measure)
        if hidden or self.color("-fill") is None:
            width = height = 0
        if height <= 0:
            return None
        return [left - 1, top, left + width + 1, top + height]


def _c_round(v):
    """the (int) (v >= 0 ? v + .5 : v - .5) of ComputeRectOvalBbox"""
    return _int(v + 0.5) if v >= 0 else _int(v - 0.5)


def bezier_points(points, closed, steps=SPLINE_STEPS):
    """port: tkTrig.c's TkMakeBezierCurve: the points of Tk's parabolic spline
    through a line's (or a closed polygon's) control points"""
    n = len(points)
    if n < 3:
        return list(points)

    def mix(a, b, f):
        return (a[0] * (1 - f) + b[0] * f, a[1] * (1 - f) + b[1] * f)

    def cubic(c0, c1, c2, c3):
        out = []
        for i in range(1, steps + 1):
            t = i / steps
            u = 1 - t
            out.append((c0[0] * u ** 3 + 3 * c1[0] * t * u * u + 3 * c2[0] * t * t * u
                        + c3[0] * t ** 3,
                        c0[1] * u ** 3 + 3 * c1[1] * t * u * u + 3 * c2[1] * t * t * u
                        + c3[1] * t ** 3))
        return out

    if closed:
        if points[0] == points[-1]:
            points = points[:-1]
            n -= 1
        out = [mix(points[-1], points[0], 0.5)]
        for i in range(n):
            a, b, c = points[i - 1], points[i], points[(i + 1) % n]
            out += cubic(mix(a, b, 0.5), mix(a, b, 0.833), mix(b, c, 0.167), mix(b, c, 0.5))
        return out
    out = [points[0]]
    for i in range(n - 2):
        a, b, c = points[i], points[i + 1], points[i + 2]
        c0, c1 = (a, mix(a, b, 0.667)) if i == 0 else (mix(a, b, 0.5), mix(a, b, 0.833))
        c3, c2 = (c, mix(c, b, 0.667)) if i == n - 3 else (mix(b, c, 0.5), mix(b, c, 0.167))
        out += cubic(c0, c1, c2, c3)
    return out


# ---------------------------------------------------------------------------
# The display list

class DisplayList:
    """port: a Tk canvas's items and its widget command, without a window.

    measure answers text_width(font, line), linespace(font) and ascent(font) for
    Tk font words.  Changes are recorded for the scene: take_changes() returns
    them (on the GUI thread)."""

    def __init__(self, measure):
        self.measure = measure
        self.lock = threading.RLock()
        self.items = {}            # id -> Item
        self.order = []            # the stacking order, bottom first
        self.next_id = 1
        self.next_z = 0
        self.x_origin = 0          # the view's scroll offset (canvasx, canvasy)
        self.y_origin = 0
        self._dirty = set()        # ids created, changed or deleted since take_changes
        self._restacked = False

    # --- the widget command

    def tcl(self, *words):
        if not words:
            raise TclError('wrong # args: should be "pathName option ?arg ...?"')
        handler = _COMMANDS.get(str(words[0]))
        if handler is None:
            raise TclError('bad option "%s": must be one of the commands of '
                           'tk-canvas-commands.json' % words[0])
        with self.lock:
            return handler(self, *words[1:])

    # --- searches

    def search(self, tag):
        """port: TagSearchFirst/Next: the items an id, a tag or "all" names,
        bottom first"""
        if isinstance(tag, int) or (isinstance(tag, str) and tag.isdigit()):
            item = self.items.get(int(tag))
            return [item] if item is not None else []
        tag = str(tag)
        if tag == "all":
            return list(self.order)
        return [i for i in self.order if tag in i.opts["-tags"]]

    def _first(self, tag):
        found = self.search(tag)
        return found[0] if found else None

    # --- commands

    def _create(self, kind=None, *args):
        if kind not in KINDS:
            raise TclError('unknown or ambiguous item type "%s"' % kind)
        n = 0
        while n < len(args) and not _is_option(args[n]):
            n += 1
        coords = args[:n]
        if len(coords) == 1:
            coords = split_list(coords[0]) if not isinstance(coords[0], (list, tuple)) \
                else list(coords[0])
        coords = [_number(v) for v in coords]
        least, exact = COORDS[kind]
        if len(coords) < least or len(coords) % 2 or (exact and len(coords) != exact):
            raise TclError('wrong # coordinates: expected %s, got %d'
                           % (exact or "at least %d" % least, len(coords)))
        item = Item(self.next_id, kind, coords, dict(DEFAULTS[kind]), self.next_z)
        self._configure(item, args[n:])
        _normalize(item)
        self.next_id += 1
        self.next_z += 1
        self.items[item.id] = item
        self.order.append(item)
        self._dirty.add(item.id)
        return item.id

    def _configure(self, item, args):
        if len(args) % 2:
            raise TclError('value for "%s" missing' % args[-1])
        new = {}
        for option, value in zip(args[::2], args[1::2]):
            option = str(option)
            if option not in DEFAULTS[item.kind]:
                raise TclError('unknown option "%s"' % option)
            new[option] = _convert(option, value)
        if "-dash" in new:
            dash_lengths(new["-dash"], 1.0)
        item.opts.update(new)

    def _delete(self, *tags):
        doomed = {i.id for t in tags for i in self.search(t)}
        if doomed:
            self.order = [i for i in self.order if i.id not in doomed]
            for i in doomed:
                del self.items[i]
            self._dirty |= doomed
        return ""

    def _move(self, tag, dx, dy):
        dx, dy = _number(dx), _number(dy)
        for item in self.search(tag):
            item.coords = [v + (dx if k % 2 == 0 else dy) for k, v in enumerate(item.coords)]
            self._dirty.add(item.id)
        return ""

    def _scale(self, tag, x0, y0, xs, ys):
        x0, y0, xs, ys = map(_number, (x0, y0, xs, ys))
        for item in self.search(tag):
            item.coords = [(x0 + xs * (v - x0)) if k % 2 == 0 else (y0 + ys * (v - y0))
                           for k, v in enumerate(item.coords)]
            _normalize(item)
            self._dirty.add(item.id)
        return ""

    def _relink(self, tag, prev):
        """port: tkCanvas.c's RelinkItems: the items tag names, in their order,
        put just above prev (None: at the bottom)"""
        moving = self.search(tag)
        if not moving:
            return
        ids = {i.id for i in moving}
        while prev is not None and prev.id in ids:
            k = self.order.index(prev)
            prev = self.order[k - 1] if k > 0 else None
        rest = [i for i in self.order if i.id not in ids]
        k = 0 if prev is None else rest.index(prev) + 1
        self.order = rest[:k] + moving + rest[k:]
        for z, item in enumerate(self.order):
            item.z = z
        self.next_z = len(self.order)
        self._restacked = True

    def _raise(self, tag, above=None):
        if above is None:
            prev = self.order[-1] if self.order else None
        else:
            found = self.search(above)
            if not found:
                raise TclError('tagOrId "%s" doesn\'t match any items' % above)
            prev = found[-1]
        self._relink(tag, prev)
        return ""

    def _lower(self, tag, below=None):
        if below is None:
            prev = None
        else:
            found = self.search(below)
            if not found:
                raise TclError('tagOrId "%s" doesn\'t match any items' % below)
            k = self.order.index(found[0])
            prev = self.order[k - 1] if k > 0 else None
        self._relink(tag, prev)
        return ""

    def _itemconfigure(self, tag, *args):
        if not args:
            raise TclError("itemconfigure without options is not supported")
        for item in self.search(tag):
            self._configure(item, args)
            _normalize(item)
            self._dirty.add(item.id)
        return ""

    def _bbox(self, *tags):
        box = None
        seen = set()
        for tag in tags:
            for item in self.search(tag):
                if item.id in seen:
                    continue
                seen.add(item.id)
                b = item.bbox(self.measure)
                if b is None or b[0] >= b[2] or b[1] >= b[3]:
                    continue
                box = b if box is None else [min(box[0], b[0]), min(box[1], b[1]),
                                             max(box[2], b[2]), max(box[3], b[3])]
        return tuple(box) if box else ""

    def _canvasx(self, x, spacing=None):
        return float(_screen_pixels(x) + self.x_origin)

    def _canvasy(self, y, spacing=None):
        return float(_screen_pixels(y) + self.y_origin)

    def _find(self, how, *args):
        if how != "all":
            raise TclError("only find all is supported")
        return tuple(i.id for i in self.order)

    def _type(self, tag):
        item = self._first(tag)
        return item.kind if item else ""

    def _coords(self, tag, *values):
        item = self._first(tag)
        if item is None:
            return ""
        if values:
            if len(values) == 1:
                values = split_list(values[0])
            item.coords = [_number(v) for v in values]
            _normalize(item)
            self._dirty.add(item.id)
            return ""
        return tuple(item.coords)

    def _itemcget(self, tag, option):
        item = self._first(tag)
        if item is None:
            return ""
        option = str(option)
        if option not in item.opts:
            raise TclError('unknown option "%s"' % option)
        value = item.opts[option]
        if option == "-font":
            return tcl_list(value)
        return value

    def _gettags(self, tag):
        item = self._first(tag)
        return tuple(item.opts["-tags"]) if item else ""

    # --- for the scene

    def take_changes(self):
        """(snapshots of the changed items that exist, ids of the deleted ones,
        {id: z} of every item if the stacking order changed), and forget them"""
        with self.lock:
            changed = [self.items[i].copy() for i in self._dirty if i in self.items]
            deleted = [i for i in self._dirty if i not in self.items]
            z = {i.id: i.z for i in self.order} if self._restacked else None
            self._dirty = set()
            self._restacked = False
            return changed, deleted, z


def _screen_pixels(x):
    """port: Tk_GetPixels of a screen coordinate (rounded to a whole pixel)"""
    v = _number(x)
    return _int(v + 0.5) if v >= 0 else _int(v - 0.5)


def _convert(option, value):
    if option in _FLOAT_OPTIONS:
        return _number(value)
    if option in _ENUMS:
        value = str(value)
        if value not in _ENUMS[option]:
            raise TclError('bad %s "%s": must be %s' % (option[1:], value,
                                                        ", ".join(_ENUMS[option])))
        return value
    if option in _COLOR_OPTIONS:
        value = str(value)
        if value:
            color_rgb(value)
        return value
    if option == "-tags":
        return tuple(split_list(value))
    if option == "-smooth":
        value = str(value)
        if value.lower() not in _TRUE + _FALSE:
            raise TclError('bad smooth value "%s"' % value)
        return value
    if option == "-font":
        from metacat.qt.fontspec import parse_font
        parse_font(value)       # Tk's errors
        return tuple(value) if isinstance(value, (list, tuple)) else str(value)
    if option == "-text":
        return str(value)
    return value


def _normalize(item):
    """what Tk does when an item is configured or its coordinates change: the
    corners of rectangles, ovals and arcs in order, an arc's angles reduced"""
    if item.kind in ("rectangle", "oval", "arc"):
        x1, y1, x2, y2 = item.coords
        item.coords = [min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2)]
    if item.kind == "arc":
        start = item.opts["-start"]
        start -= _int(start / 360.0) * 360.0
        if start < 0:
            start += 360.0
        extent = item.opts["-extent"]
        extent -= _int(extent / 360.0) * 360.0
        item.opts["-start"], item.opts["-extent"] = start, extent


_COMMANDS = {
    "create": DisplayList._create,
    "delete": DisplayList._delete,
    "move": DisplayList._move,
    "scale": DisplayList._scale,
    "raise": DisplayList._raise,
    "lower": DisplayList._lower,
    "itemconfigure": DisplayList._itemconfigure,
    "bbox": DisplayList._bbox,
    "canvasx": DisplayList._canvasx,
    "canvasy": DisplayList._canvasy,
    "find": DisplayList._find,
    "type": DisplayList._type,
    "coords": DisplayList._coords,
    "itemcget": DisplayList._itemcget,
    "gettags": DisplayList._gettags,
}
