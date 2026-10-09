"""The Relations pane (lib/SGUI/Relations.pm): one row per workspace relation.

DrawIt takes ``values %SWorkspace::relations`` once, and draws the Mapping::Numeric ones,
then every one that is not a Mapping::Structural, one row each from ``YOffset + Margin + 10``
down, ``(Height - 2 Margin) / RowCount`` apart. DrawRelation writes, anchored nw in the
RelationsLayout font: the strength and the type's complexity (``%5.2f``), the bounds strings
of both ends, and the type's ``as_text``. Every other row (0, 2, …) starts with a pale green
stripe across the rectangle (inside the margins), lowered below everything on the canvas.

Rows come in hash order in Perl; here they come in the snapshot's order (the golden test feeds
Perl's order through the snapshot).

PERL-QUIRK: the relations are SRelation objects, never Mappings, so the Mapping::Numeric list
is always empty and the "not Mapping::Structural" list holds every relation (structural
relations included). The two lists are kept anyway (a Mapping::Numeric would be drawn twice).

PERL-QUIRK: there is no row limit: rows past RowCount go on below the rectangle.

Item 013: ``$Canvas->lower($id)`` puts each stripe below every item of the whole canvas, not
just of this view. ``draw_layers`` returns the lowered stripes apart from the rest.
"""
import dataclasses
import math

from seqsee.util import perl_num

from . import ops


@dataclasses.dataclass(frozen=True)
class Layout:
    """config/GUI_ws3.conf ([Layout] Margin and [RelationsLayout]), read by SGUI::Relations'
    BEGIN block."""
    margin: float = 20
    row_count: int = 22
    strength_offset: float = 0
    simplicity_offset: float = 50
    end1_offset: float = 100
    end2_offset: float = 170
    type_offset: float = 260
    font: str = "-adobe-helvetica-bold-r-normal--9-140-100-100-p-105-iso8859-4"


LAYOUT = Layout()


def fmt(value):
    """Perl ``sprintf("%5.2f", $value)`` (undef and strings numify; Perl spells Inf/NaN)."""
    v = float(perl_num(value))
    if math.isnan(v):
        return "%5s" % "NaN"
    if math.isinf(v):
        return "%5s" % ("Inf" if v > 0 else "-Inf")
    return "%5.2f" % v


def rows(snap):
    """The relations DrawIt draws, in order: ``(@compound, @simple)``."""
    compound = [r for r in snap.relations if r.isa_numeric]
    simple = [r for r in snap.relations if not r.isa_structural]
    return compound + simple


def _text(x, y, text, font):
    """``createText(-anchor => 'nw', -font => $Font, -text => $text)``; '' or undef leaves
    Tk's default empty text."""
    if text:
        return ops.text(x, y, anchor="nw", font=font, text=text)
    return ops.text(x, y, anchor="nw", font=font)


def draw_layers(snap, x, y, w, h, layout=LAYOUT):
    """SGUI::Relations->Setup(canvas, x, y, w, h); ->DrawIt(), as (lowered, others): the
    stripes, in their final order at the bottom of the canvas, and the texts in order."""
    m = layout.margin
    row_height = (h - 2 * m) / layout.row_count    # no int(): Setup keeps the fraction
    left, right = x + m, x + w - m
    font = layout.font
    lowered, out = [], []
    ypos = y + m + 10
    for count, reln in enumerate(rows(snap)):
        if count % 2 == 0:
            lowered.insert(0, ops.rectangle(left, ypos, right, ypos + row_height,
                                            fill="#CCFFDD", outline=""))
        end1, end2 = reln.end_bounds
        out.append(_text(left + layout.strength_offset, ypos, fmt(reln.strength), font))
        out.append(_text(left + layout.simplicity_offset, ypos, fmt(reln.complexity), font))
        out.append(_text(left + layout.end1_offset, ypos, end1, font))
        out.append(_text(left + layout.end2_offset, ypos, end2, font))
        out.append(_text(left + layout.type_offset, ypos, reln.type_text, font))
        ypos += row_height
    return lowered, out


def draw(snap, x, y, w, h, layout=LAYOUT):
    """SGUI::Relations->Setup(canvas, x, y, w, h); ->DrawIt(): the list of draw ops."""
    lowered, out = draw_layers(snap, x, y, w, h, layout)
    return lowered + out
