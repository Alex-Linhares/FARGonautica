"""Draw ops: plain dataclasses mirroring Tk canvas items (line, oval, rectangle, polygon, arc,
text). Drawing functions return lists of them; ``to_dict`` gives the same shape as a Perl
canvas dump (python/oracle/CanvasDump.pm), so the two can be compared directly.

Field defaults are Tk 804's item defaults (checked by the ``defaults`` case of
tests/golden/gui_smoke.json), and ``to_dict`` lists only the options that differ from
them, as the dump does. ``None`` means unset (Tk's empty value), e.g. ``fill=None`` for a
hollow oval. Colours may be Tk colour names or hex; comparisons normalise them.
"""

from dataclasses import dataclass, fields
from typing import ClassVar

DEFAULT_FONT = "Helvetica -12"


@dataclass(frozen=True)
class Op:
    """A canvas item: ``coords`` is a flat tuple ``(x0, y0, x1, y1, ...)``."""

    TYPE: ClassVar[str] = ""
    coords: tuple = ()
    tags: tuple = ()

    def opts(self):
        """Tk option name (without '-') → value, for the options that differ from Tk's default."""
        out = {}
        for f in fields(self):
            if f.name in ("coords", "tags"):
                continue
            value = getattr(self, f.name)
            if value != f.default:
                out[f.name] = list(value) if isinstance(value, tuple) else value
        return out

    def to_dict(self):
        return {
            "type": self.TYPE,
            "coords": [float(c) for c in self.coords],
            "opts": self.opts(),
            "tags": list(self.tags),
        }


@dataclass(frozen=True)
class Line(Op):
    TYPE: ClassVar[str] = "line"
    fill: object = "black"
    width: float = 1
    arrow: str = "none"
    arrowshape: tuple = (8, 10, 3)
    smooth: int = 0
    dash: object = None
    capstyle: str = "butt"
    joinstyle: str = "round"
    splinesteps: int = 12
    stipple: object = None


@dataclass(frozen=True)
class Oval(Op):
    TYPE: ClassVar[str] = "oval"
    fill: object = None
    outline: object = "black"
    width: float = 1
    dash: object = None
    stipple: object = None
    outlinestipple: object = None


@dataclass(frozen=True)
class Rectangle(Op):
    TYPE: ClassVar[str] = "rectangle"
    fill: object = None
    outline: object = "black"
    width: float = 1
    dash: object = None
    stipple: object = None
    outlinestipple: object = None


@dataclass(frozen=True)
class Polygon(Op):
    TYPE: ClassVar[str] = "polygon"
    fill: object = "black"
    outline: object = None
    width: float = 1
    smooth: int = 0
    dash: object = None
    joinstyle: str = "round"
    splinesteps: int = 12
    stipple: object = None
    outlinestipple: object = None


@dataclass(frozen=True)
class Arc(Op):
    TYPE: ClassVar[str] = "arc"
    start: float = 0
    extent: float = 90
    style: str = "pieslice"
    fill: object = None
    outline: object = "black"
    width: float = 1
    dash: object = None
    stipple: object = None
    outlinestipple: object = None


@dataclass(frozen=True)
class Text(Op):
    TYPE: ClassVar[str] = "text"
    text: str = ""
    anchor: str = "center"
    fill: object = "black"
    font: str = DEFAULT_FONT
    justify: str = "left"
    width: float = 0
    stipple: object = None


OP_TYPES = {cls.TYPE: cls for cls in (Line, Oval, Rectangle, Polygon, Arc, Text)}


class DrawDied(Exception):
    """A Perl drawing routine that dies part way. ``ops`` are the items it had drawn by
    then: they stay on the Tk canvas, but the die leaves the caller (Tk::Seqsee::Update), so
    nothing after them is drawn. The message is Perl's, without " at FILE line N.".
    ``lowered``: how many of the first ``ops`` were lowered below the whole canvas
    (``$Canvas->lower``: the lists' bars), so a composite view can put them at the bottom.
    ``raised_at``: for the workspace views, the number of ops drawn when DrawGroups'
    ``raise('hilit')`` ran (None if it didn't run)."""

    def __init__(self, message, ops, lowered=0, raised_at=None):
        super().__init__(message)
        self.ops = list(ops)
        self.lowered = lowered
        self.raised_at = raised_at


def _flatten(coords):
    flat = []
    for c in coords:
        if isinstance(c, (list, tuple)):
            flat.extend(_flatten(c))
        else:
            flat.append(c)
    return tuple(flat)


def create(type_, *coords, **options):
    """The op for ``$canvas->create(type_, @coords, %options)``.

    Mirrors the Perl call: coords may be flat or nested lists; option names may carry Tk's
    leading '-' (pass a ``Style::*`` result as ``**style``); ``tags`` may be a string or a
    list; ``smooth`` is any truthy value; '' means unset. Unknown options raise TypeError.
    """
    cls = OP_TYPES[type_]
    kw = {}
    for key, value in options.items():
        key = key.lstrip("-")
        if key == "tags":
            value = (value,) if isinstance(value, str) else tuple(value or ())
        elif key == "smooth":
            value = 1 if value and value != "0" else 0
        elif isinstance(value, list):
            value = tuple(value)
        elif value == "":
            value = None
        kw[key] = value
    coords = _flatten(coords)
    if type_ in ("oval", "rectangle") and len(coords) == 4:
        # Tk (ComputeRectOvalBbox) swaps the corners so that x1 <= x2 and y1 <= y2.
        x1, y1, x2, y2 = coords
        coords = (min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2))
    return cls(coords=coords, **kw)


def from_dict(item):
    """The op for a canvas-dump item ``{type, coords, opts, tags}`` (the inverse of
    ``Op.to_dict``): ``null`` options are unset, lists become tuples."""
    kw = {}
    for key, value in item.get("opts", {}).items():
        kw[key] = tuple(value) if isinstance(value, list) else value
    return OP_TYPES[item["type"]](coords=tuple(item["coords"]), tags=tuple(item.get("tags", ())),
                                  **kw)


def line(*coords, **options):
    return create("line", *coords, **options)


def oval(*coords, **options):
    return create("oval", *coords, **options)


def rectangle(*coords, **options):
    return create("rectangle", *coords, **options)


def polygon(*coords, **options):
    return create("polygon", *coords, **options)


def arc(*coords, **options):
    return create("arc", *coords, **options)


def text(*coords, **options):
    return create("text", *coords, **options)
