"""The Stream view (lib/SGUI/Stream.pm): the main stream's thoughts, one box each.

DrawIt draws the current thought (if any) at ``(XOffset + Margin/2, YOffset)``, then each
true entry of OlderThoughts in a grid of ``EntriesPerColumn`` rows per column, starting at
row 1 of column 0 (the row counter is advanced before the first one is drawn). DrawThought
draws a ``ColumnWidth`` × ``RowHeight`` rectangle in ``Style::ThoughtBox(thought hit
intensity, is_current)``, the thought's ``as_text`` (Style::ThoughtHead) at its top left, and
the first three components of its stored fringe (Style::ThoughtComponent), 15 px apart.

PERL-QUIRKs (confirmed by the oracle):
- The current thought sits outside the margin (at half the margin from the left, and on the
  top edge of the rectangle), and its box overlaps column 0's older thoughts.
- The grid is not bounded by ColumnCount: past ``3 * ColumnCount - 1`` older thoughts the
  boxes go on to the right of the rectangle (with MaxOlderThoughts 10 this needs a stream
  with '' entries or a larger maximum).
- Components are drawn as Perl stringifies them, so all but plain scalars and SInts show as
  ``Class=HASH(0x…)`` (``snapshot.perl_string``; Python ids stand in for the addresses).
- Style::ThoughtComponent ignores its arguments (activation, component hit intensity): every
  component is the same blue.
"""
import dataclasses

from . import ops
from .theme import Style


@dataclasses.dataclass(frozen=True)
class Layout:
    """config/GUI_ws3.conf ([Layout] Margin and [StreamLayout]), read by SGUI::Stream's BEGIN
    block."""
    margin: float = 20
    entries_per_column: int = 3
    column_count: int = 4


LAYOUT = Layout()


def _text(x, y, text, **opts):
    """``createText`` with ``-text => $text``; '' leaves Tk's default empty text."""
    if text:
        opts["text"] = text
    return ops.text(x, y, **opts)


def draw(snap, x, y, w, h, layout=LAYOUT):
    """SGUI::Stream->Setup(canvas, x, y, w, h); ->DrawIt(): the list of draw ops."""
    m = layout.margin
    # Setup: Perl's int() truncates toward zero (the sizes are negative in a tiny rect).
    size = (int((w - 2 * m) / layout.column_count), int((h - 2 * m) / layout.entries_per_column))
    out = []
    if snap.stream.current is not None:
        _draw_thought(out, snap.stream.current, x + m / 2, y, True, size)
    row = col = 0
    for thought in snap.stream.older:
        if thought is None:          # next unless $tht
            continue
        row += 1
        if row >= layout.entries_per_column:
            row = 0
            col += 1
        _draw_thought(out, thought, x + m + col * size[0], y + m + row * size[1], False, size)
    return out


def _draw_thought(out, thought, left, top, is_current, size):
    """DrawThought($tht, $left, $top, $is_current)."""
    column_width, row_height = size
    out.append(ops.rectangle(left, top, left + column_width, top + row_height,
                             **Style.ThoughtBox(thought.hit_intensity, 1 if is_current else 0)))
    out.append(_text(left + 1, top + 1, thought.text, anchor="nw", **Style.ThoughtHead()))
    if thought.fringe is None:       # my $fringe = $tht->stored_fringe() or return
        return
    for count, component in enumerate(thought.fringe[:3], 1):
        if component is None:        # last unless $_
            break
        out.append(_text(left + 10, top + 15 * count, component.text, anchor="nw",
                         **Style.ThoughtComponent(component.activation,
                                                  component.hit_intensity)))
