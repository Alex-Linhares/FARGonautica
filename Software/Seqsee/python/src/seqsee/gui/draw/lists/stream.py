"""The Stream list (lib/SGUI/List/Stream.pm): the current thought, then the older thoughts by
hit intensity.

GetItemList gives nothing without a current thought; otherwise the current thought and the
OlderThoughts sorted by ``thought_hit_intensity`` (``rnkeysort``: highest first, stable,
undef as 0). An entry is ``(index, ThoughtSnap)``: index 0 is the current thought, i + 1 the
i-th of OlderThoughts (a false entry is None); rows are tagged ``tht<index>`` ('' for a false
entry, Perl's stringified '').

DrawOneItem writes, anchored nw in the default font: the hit intensity ('-' for the current
thought), the thought's as_text, and below them its stored_fringe sorted by activation
(``rnkeysort``) as ``"[activation] label; "`` per component.

PERL-QUIRKs (confirmed by the oracle):
- ``my $fringe = $thought->stored_fringe() or return;`` returns from DrawOneItem without its
  item ids when the thought has no stored fringe: its two texts get no tags, so a page
  redraw's Clear leaves them on the canvas (see ``lists.survivors``).
- A false OlderThoughts entry sorts as 0 and its row dies on ``as_text`` after its bar.
"""
import dataclasses

from seqsee import util

from .. import ops
from . import MARGIN, Untagged, draw_list

NAME = "SGUI::List::Stream"


@dataclasses.dataclass(frozen=True)
class Layout:
    """SGUI::List::Stream->new's fields (not in a config file)."""
    height_per_row: float = 40
    intensity_x: float = 5
    name_x: float = 100
    fringe_y: float = 20
    fringe_x: float = 10


LAYOUT = Layout()


def _num(v):
    return 0 if v is None else util.perl_num(v)


def item_list(snap):
    """GetItemList: ``((0, current), (i + 1, older_i), …)`` sorted as Perl sorts them."""
    st = snap.stream
    if st.current is None:
        return ()
    older = sorted(((i + 1, t) for i, t in enumerate(st.older)),
                   key=lambda e: -_num(None if e[1] is None else e[1].hit_intensity))
    return ((0, st.current),) + tuple(older)


def tag_of(entry):
    return "" if entry[1] is None else "tht%d" % entry[0]


def _text(x, y, text):
    """``createText(-text => $text, -anchor => 'nw')``; undef or '' leaves Tk's empty text."""
    if text:
        return ops.text(x, y, text=text, anchor="nw")
    return ops.text(x, y, anchor="nw")


def fringe_string(fringe):
    """The fringe text: ``"[activation] label; "`` per component, highest activation first
    (None when there are no components: Perl's undef)."""
    for c in fringe:
        if c is None:
            raise ops.DrawDied("Can't use an undefined value as an ARRAY reference", [])
    parts = sorted(fringe, key=lambda c: -_num(c.activation))
    if not parts:
        return None
    return "".join("[%s] %s; " % (util.perl_str(c.activation), c.label) for c in parts)


def draw_one(left, top, entry, layout=LAYOUT):
    """DrawOneItem for one ``(index, ThoughtSnap)`` entry."""
    index, thought = entry
    if thought is None:
        raise ops.DrawDied('Can\'t call method "as_text" without a package or object '
                           'reference', [])
    if index == 0:
        intensity = "-"
    else:
        hit = thought.hit_intensity
        intensity = None if hit is None else util.perl_str(hit)
    out = [_text(left + layout.intensity_x, top + 5, intensity),
           _text(left + layout.name_x, top + 5, thought.text)]
    if thought.fringe is None:
        return Untagged(out)
    try:
        text = fringe_string(thought.fringe)
    except ops.DrawDied as e:
        raise ops.DrawDied(str(e), out) from None
    out.append(_text(left + layout.fringe_x, top + layout.fringe_y, text))
    return out


def draw_layers(snap, x, y, w, h, page=0, layout=LAYOUT, name=NAME):
    """Setup(canvas, x, y, w, h); PageNumber = page; DrawIt, as ``(lowered, others, state)``."""
    return draw_list(item_list(snap), lambda left, top, e: draw_one(left, top, e, layout),
                     x, y, w, h, name, layout.height_per_row, page=page, tag_of=tag_of,
                     margin=MARGIN)


def draw(snap, x, y, w, h, page=0, layout=LAYOUT, name=NAME):
    """The list of draw ops for page ``page``."""
    lowered, out, _ = draw_layers(snap, x, y, w, h, page, layout, name)
    return lowered + out
