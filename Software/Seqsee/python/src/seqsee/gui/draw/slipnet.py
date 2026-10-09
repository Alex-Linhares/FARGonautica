"""The Slipnet view (lib/SGUI/Slipnet.pm): the active LTM nodes, one oval and one label each,
in columns.

DrawIt walks ``SLTM::GetTopConcepts(10)`` (the snapshot's ``slipnet``: every node in memory
order; the 10 is ignored) and draws each node whose activation is above
``MinActivationForDisplay``, top to bottom, ``EntriesPerColumn`` to a column. DrawNode draws
a circle whose radius is ``activation * MaxOvalRadius``, centred in a square of side
``2 * MaxOvalRadius`` (offset by 2), filled with ``Style::NetActivation(int(raw_significance))``,
and the concept's ``as_text`` cut to ``MaxTextWidth`` characters, west-anchored to its right.
GetTopConcepts gives no raw significance (undef), so every node has the colour of
NetActivation(0).

PERL-QUIRK: DrawIt checks ``$col >= $ColumnCount`` at the top of the loop, before the row
overflow moves to the next column, so with more than EntriesPerColumn * ColumnCount shown
nodes one more node is drawn in an extra column, right of the rectangle.

PERL-QUIRK: Mapping::Dir nodes have no ``as_text``. When one is shown, DrawNode dies after
drawing its oval ("Can't locate object method ..."); ``draw`` raises ``ops.DrawDied`` with
the ops drawn so far, which is what the Tk canvas keeps. (Item 013: in Tk::Seqsee::Update
the die is not caught, as with Workspace_Attention.)
"""
import dataclasses

from . import ops
from .theme import Style


@dataclasses.dataclass(frozen=True)
class Layout:
    """config/GUI_ws3.conf ([Layout] Margin and [SlipnetLayout]), read by SGUI::Slipnet's
    BEGIN block."""
    margin: float = 20
    entries_per_column: int = 10
    column_count: int = 3
    max_oval_radius: float = 15
    max_text_width: int = 30
    min_activation_for_display: float = 0.01


LAYOUT = Layout()


def draw(snap, x, y, w, h, layout=LAYOUT):
    """SGUI::Slipnet->Setup(canvas, x, y, w, h); ->DrawIt(): the list of draw ops."""
    m = layout.margin
    # Setup: Perl's int() truncates toward zero (the sizes are negative in a tiny rect).
    column_width = int((w - 2 * m) / layout.column_count)
    row_height = int((h - 2 * m) / layout.entries_per_column)
    out = []
    row, col = -1, 0
    for concept in snap.slipnet:
        if col >= layout.column_count:
            break
        if not (concept.activation or 0) > layout.min_activation_for_display:
            continue
        row += 1
        if row >= layout.entries_per_column:
            row = 0
            col += 1
        _draw_node(out, concept, x + m + col * column_width, y + m + row * row_height, layout)
    return out


def _draw_node(out, concept, left, top, layout):
    """DrawNode."""
    big = layout.max_oval_radius
    radius = concept.activation * big
    out.append(ops.oval(
        left + 2 + big - radius, top + 2 + big - radius,
        left + 2 + big + radius, top + 2 + big + radius,
        **Style.NetActivation(int(concept.raw_significance or 0))))
    if concept.text is None:
        raise ops.DrawDied(
            f'Can\'t locate object method "as_text" via package "{concept.perl_class}"', out)
    out.append(ops.text(left + 6 + 2 * big, top + 2 + big, anchor="w",
                        text=concept.text[:layout.max_text_width]))
