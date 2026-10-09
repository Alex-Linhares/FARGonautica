"""The Coderack view (lib/SGUI/Coderack.pm): one row per codelet family, with its count on the
rack, its share of the urgencies and its share of all the runnables run so far.

DrawIt counts the codelets of ``@SCoderack::CODELETS`` per family and sums their urgencies as
a percentage of ``$SCoderack::URGENCIES_SUM`` (``'---'``, i.e. 0, when that sum is 0). It
writes the four column headers for each of ``MaxColumns`` columns, then walks
``%SCoderack::HistoryOfRunnable`` (keyed ``Seqsee::SCF::<family>``) with ``each``: one row per
key, holding the family's name (``family_to_name``), its count, a blue urgency bar (50 px is
100 %) with a tick at 50 px, a red bar for its share of all runs (only once something has run),
its run count and another tick. Every fourth row starts a pale green stripe two rows high,
lowered below everything on the canvas.

Before the walk DrawIt writes ``$HistoryOfRunnable{$_} ||= 0`` for every family on the rack,
so they get a row too. That write stays in the model, so a family once seen keeps its row
(with 0 runs) in every later draw until SCoderack->clear. The port never writes the model:
``draw`` adds the rack families to its own row list, and a widget that wants Perl's
persistent rows passes the families it has seen before as ``known_families`` (collect them with
``rack_families``).

Rows come in hash order in Perl; here they come in the snapshot's history order, then the
known families, then the rack's (the golden test feeds Perl's order through the snapshot).

PERL-QUIRK: no codelet family defines ``$NAME``, so ``family_to_name`` is undef and the name
column is always empty.

PERL-QUIRK: the headers are at ``YOffset - 10``, above the view's rectangle (off the canvas
when the view is at its top).

PERL-QUIRK: the column check ``$rows_displayed > $MaxRows`` gives MaxRows + 1 rows per
column, and ``last if $current_column > $MaxColumns`` lets a column MaxColumns (the third)
be drawn right of the rectangle.

PERL-QUIRK: the stripes ignore the Margin: a stripe for row r spans
``YOffset + (2 + r) * RowHeight - 3`` to ``YOffset + (4 + r) * RowHeight - 3``.

Item 013: ``$Canvas->lower($id)`` puts each stripe below every item of the whole canvas, not
just of this view. ``draw_layers`` returns the lowered stripes apart from the rest.
"""
import dataclasses

from seqsee.util import perl_num, perl_str, perl_true

from . import ops
from .family import family_to_name

HEADERS = ("NAME", "#", "Urgeny %", "% OF ALL RUN")     # sic


@dataclasses.dataclass(frozen=True)
class Layout:
    """config/GUI_ws3.conf ([Layout] Margin and [CoderackLayout]), read by SGUI::Coderack's
    BEGIN block."""
    margin: float = 20
    max_columns: int = 2
    max_rows: int = 15
    name_offset: float = 0
    count_offset: float = 200
    urgency_offset: float = 220
    historical_fraction_offset: float = 290


LAYOUT = Layout()


def rack_families(snap):
    """The history keys (``Seqsee::SCF::<family>``) of the families on the rack, in rack
    order: those DrawIt sets to ``||= 0`` in %HistoryOfRunnable."""
    return tuple(dict.fromkeys("Seqsee::SCF::" + perl_str(f) for f, _ in snap.coderack.codelets))


def rows(coderack, known_families=()):
    """The (key, count) pairs DrawIt walks: %HistoryOfRunnable after its ``||= 0`` writes."""
    out = dict(coderack.history)
    for key in tuple(known_families) + tuple(
            "Seqsee::SCF::" + perl_str(f) for f, _ in coderack.codelets):
        if not perl_true(out.get(key)):
            out[key] = 0
    return list(out.items())


def _text(x, y, text):
    """``createText(-text => $text, -anchor => 'nw')``; undef leaves Tk's empty text."""
    if text is None:
        return ops.text(x, y, anchor="nw")
    return ops.text(x, y, anchor="nw", text=perl_str(text))


def draw_layers(snap, x, y, w, h, layout=LAYOUT, known_families=()):
    """SGUI::Coderack->Setup(canvas, x, y, w, h); ->DrawIt(), as (lowered, others): the
    stripes, in their final order at the bottom of the canvas, and the other ops in order."""
    m = layout.margin
    # Setup: Perl's int() truncates toward zero (the sizes are negative in a tiny rect).
    row_height = int((h - 2 * m) / layout.max_rows)
    column_width = int((w - 2 * m) / layout.max_columns)

    cr = snap.coderack
    count, total = {}, {}
    for family, urgency in cr.codelets:
        key = "Seqsee::SCF::" + perl_str(family)
        count[key] = count.get(key, 0) + 1
        total[key] = total.get(key, 0) + perl_num(urgency)
    if perl_true(cr.urgencies_sum):
        usum = perl_num(cr.urgencies_sum)
        total = {k: v / (usum * 0.01) for k, v in total.items()}
    else:
        total = {k: 0 for k in total}            # '---', which is 0 as a number

    history = rows(cr, known_families)
    run_so_far = sum(perl_num(n) for _, n in history)

    lowered, out = [], []
    for col in range(layout.max_columns):
        base = x + m + col * column_width
        for text, offset in zip(HEADERS, (layout.name_offset, layout.count_offset,
                                          layout.urgency_offset,
                                          layout.historical_fraction_offset)):
            out.append(ops.text(base + offset, y - 10, anchor="nw", text=text))

    column, row = 0, 0
    base = x + m
    bar = 0.8 * row_height
    for family, run_count in history:
        if row > layout.max_rows:
            row = 0
            column += 1
            base += column_width
        if column > layout.max_columns:
            break
        y_pos = y + m + row * row_height
        urgency_x = base + layout.urgency_offset
        history_x = base + layout.historical_fraction_offset
        out.append(_text(base + layout.name_offset, y_pos, family_to_name(family)))
        out.append(_text(base + layout.count_offset, y_pos, count.get(family)))
        out.append(ops.rectangle(urgency_x, y_pos, urgency_x + total.get(family, 0) * 0.5,
                                 y_pos + bar, fill="#0000FF"))
        out.append(ops.rectangle(urgency_x + 49, y_pos, urgency_x + 50, y_pos + bar))
        if run_so_far:
            out.append(ops.rectangle(history_x, y_pos,
                                     history_x + 50 * perl_num(run_count) / run_so_far,
                                     y_pos + bar, fill="#FF0000"))
        out.append(_text(history_x + 60, y_pos, run_count))
        out.append(ops.rectangle(history_x + 49, y_pos, history_x + 50, y_pos + bar))
        if row % 4 == 0:
            lowered.insert(0, ops.rectangle(
                base, y + (2 + row) * row_height - 3,
                base + column_width, y + (4 + row) * row_height - 3,
                fill="#CCFFDD", outline=""))
        row += 1
    return lowered, out


def draw(snap, x, y, w, h, layout=LAYOUT, known_families=()):
    """SGUI::Coderack->Setup(canvas, x, y, w, h); ->DrawIt(): the list of draw ops."""
    lowered, out = draw_layers(snap, x, y, w, h, layout, known_families)
    return lowered + out
