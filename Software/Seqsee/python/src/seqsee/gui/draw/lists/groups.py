"""The Groups list (lib/SGUI/List/Groups.pm): one row per workspace group, in GetGroups'
order (longest span first; Perl breaks ties in hash order, the snapshot in get_groups' order).

DrawOneItem writes, anchored nw in the 9 px font: an "L" 8 px left of the row if the group is
locked against deletion, the strength (``%5.2f``), the bounds string and the categories
string. The categories string is Perl's ``join(', ', keys %categories)``, i.e. stringified
category refs (``SCategory::Ascending=HASH(0x…)``), and the snapshot keeps it as is.

Rows are tagged with the group's ``obj<oid>``, so a click can be mapped back to the snapshot
object (``lists.entry_for``). The popup's ActionButtons (Delete, Lock, …) are in
``seqsee.gui.listactions``.
"""
import dataclasses

from .. import ops
from ..relations import fmt
from . import MARGIN, draw_list

NAME = "SGUI::List::Groups"


@dataclasses.dataclass(frozen=True)
class Layout:
    """SGUI::List::Groups->new's fields (not in a config file)."""
    lock_x: float = -8
    strength_x: float = 0
    ends_x: float = 50
    categories_x: float = 140
    font: str = "-adobe-helvetica-bold-r-normal--9-140-100-100-p-105-iso8859-4"
    height_per_row: float = 15


LAYOUT = Layout()


def _text(x, y, text, font):
    """``createText(-anchor => 'nw', -font => $Font, -text => $text)``; '' leaves Tk's
    default empty text."""
    if text:
        return ops.text(x, y, anchor="nw", font=font, text=text)
    return ops.text(x, y, anchor="nw", font=font)


def draw_one(left, top, group, layout=LAYOUT):
    """DrawOneItem for one ObjectSnap (None, an undef slot, dies like Perl)."""
    if group is None:
        raise ops.DrawDied('Can\'t call method "get_is_locked_against_deletion" on an '
                           'undefined value', [])
    font = layout.font
    out = []
    if group.is_locked:
        out.append(_text(left + layout.lock_x, top, "L", font))
    out.append(_text(left + layout.strength_x, top, fmt(group.strength), font))
    out.append(_text(left + layout.ends_x, top, group.bounds_string, font))
    out.append(_text(left + layout.categories_x, top, group.categories_as_string, font))
    return out


def _tag(group):
    return "" if group is None else "obj%d" % group.oid


def draw_layers(snap, x, y, w, h, page=0, layout=LAYOUT, name=NAME):
    """Setup(canvas, x, y, w, h); PageNumber = page; DrawIt, as ``(lowered, others, state)``:
    the bars (latest first), the other ops in order, and the resulting ``ListState``."""
    return draw_list(snap.groups, lambda left, top, g: draw_one(left, top, g, layout),
                     x, y, w, h, name, layout.height_per_row, page=page, tag_of=_tag,
                     margin=MARGIN)


def draw(snap, x, y, w, h, page=0, layout=LAYOUT, name=NAME):
    """The list of draw ops for page ``page``."""
    lowered, out, _ = draw_layers(snap, x, y, w, h, page, layout, name)
    return lowered + out
