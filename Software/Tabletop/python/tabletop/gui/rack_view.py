"""P4b Rack list, GUI design, not French's, after the Numbo GUI's `coderack_view.py`
(G-036, G-038): every codelet waiting on the rack, the oldest first, in the form
"(codelet-name codelet-age codelet-urgency)" [F95 p. 77] with its generation tag
[F95 p. 81]: the label with its arguments, the raw and the adjusted urgency (D-052), the
generation, when and by whom it was posted, its age (D-053) and the chance it had at the
last draw (D-060); over the list, the codelet just chosen on a band with its chance, the
exponent `a` and the rack size [F95 p. 77] [F95 p. 92], tinted while the event shown is
the choice, and what the codelet did [F95 p. 73]; the codelets the event shown removed
stay in the list, greyed, for that one event (D-053).

`RackListWidget` is a `QAbstractScrollArea` that paints its model's rows on its viewport
(Numbo GUI, `coderack_view.py`: a widget that paints its model), the band and the
column headings fixed at the top and the rows scrolling under them; hovering a row shows
the codelet's hover text. `RackView` is the pane: the label over the list, keeping the
view protocol the window expects (G-011); it takes the window's history through `attach`,
so that the list is re-based on each new snapshot (G-036).
"""

from PySide6.QtCore import QRectF, Qt
from PySide6.QtGui import QColor, QFont, QFontMetricsF, QImage, QPainter, QPen
from PySide6.QtWidgets import (QAbstractScrollArea, QHBoxLayout, QLabel, QSizePolicy,
                               QVBoxLayout, QWidget)

from tabletop.gui import paint
from tabletop.gui.models import coderack as cr

__all__ = ["BACKGROUND", "CHOSEN", "CHOSEN_TINT", "HEADING_ROWS", "MIN_SIZE", "REMOVED",
           "RackListWidget", "RackView"]

# The smallest the pane may be (G-014): a few rows.
MIN_SIZE = (200, 70)
HEADING_ROWS = 4                     # the band's two lines, the summary, the column headings
BACKGROUND = QColor("#fbfbf8")
TEXT, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
REMOVED = QColor("#9a9a9a")          # a codelet removed by the event shown (G-038)
CHOSEN, CHOSEN_TINT = QColor("#e6007e"), QColor("#fbe3f0")   # Numbo GUI, coderack_view.py
RULE = QColor("#d8d8d2")
PAD = 4.0


class RackListWidget(QAbstractScrollArea):
    """The list itself (G-038): paints the band, the summary, the headings and the rows
    of its model; `rows` are the ones drawn, `paints` the count for the window's probe
    (G-011); hovering a row gives its hover text."""

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.rows = ()
        self.band = cr.band_lines(model)
        self.paints = 0
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(8.5)
        # Greyscale antialiasing, so that a row's colour can be read off its pixels (G-038).
        self.font_.setStyleStrategy(QFont.StyleStrategy.NoSubpixelAntialias)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.metrics = QFontMetricsF(self.font_)
        self.line = self.metrics.height() + 3
        self.setMinimumSize(MIN_SIZE[0], MIN_SIZE[1] - 22)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.viewport().setMouseTracking(True)
        self.setToolTip("hover a row for its codelet")

    def refresh(self) -> None:
        """Take the model's rows and band, and set the scroll range (G-038)."""
        self.rows = self.model.rows()
        self.band = cr.band_lines(self.model)
        content = self.line * (HEADING_ROWS + len(self.rows)) + 2 * PAD
        self.verticalScrollBar().setRange(0, max(0, int(content - self.viewport().height())))
        self.verticalScrollBar().setPageStep(max(1, self.viewport().height()))
        self.verticalScrollBar().setSingleStep(int(self.line))

    def resizeEvent(self, event):
        """The scroll range follows the viewport's height (G-038)."""
        super().resizeEvent(event)
        self.refresh()

    def summary(self) -> str:
        """The line under the band: how many wait, the most this run, the total urgency
        [F95 p. 77] (G-038)."""
        m = self.model
        if m.problem is None:
            return "no rack yet"
        return (f"{len(m.rack)} waiting (at most {m.max_size} this run); total urgency "
                f"{m.total_urgency():.1f}")

    def columns(self) -> list:
        """The columns' x and width for the viewport's width (G-038)."""
        total = sum(weight for _, weight in cr.COLUMNS)
        room = self.viewport().width() - 2 * PAD
        x, out = PAD, []
        for _heading, weight in cr.COLUMNS:
            width = room * weight / total
            out.append((x, width))
            x += width
        return out

    def row_at(self, y):
        """The row under a viewport y, or None (G-038)."""
        top = PAD + self.line * HEADING_ROWS - self.verticalScrollBar().value()
        k = int((y - top) // self.line)
        if y < PAD + self.line * HEADING_ROWS or not 0 <= k < len(self.rows):
            return None
        return self.rows[k]

    def mouseMoveEvent(self, event):
        """The row under the mouse as the tooltip (G-038)."""
        row = self.row_at(event.position().y())
        text = "hover a row for its codelet" if row is None else cr.row_tooltip(row)
        self.setToolTip(text)
        self.viewport().setToolTip(text)

    def paintEvent(self, event):
        """Paint the band, the summary, the headings and the rows (G-038); counts the
        paints (G-011)."""
        self.paints += 1
        painter = QPainter(self.viewport())
        painter.fillRect(self.viewport().rect(), BACKGROUND)
        width = self.viewport().width()
        y = PAD
        if self.model.chosen_fresh:
            painter.fillRect(QRectF(PAD / 2, y - 1, width - PAD, 2 * self.line + 2), CHOSEN_TINT)
        painter.setFont(self.bold)
        painter.setPen(QPen(CHOSEN if self.model.chosen_fresh else TEXT, 1))
        self._text(painter, PAD, y, width - 2 * PAD, self.band[0])
        y += self.line
        painter.setFont(self.font_)
        painter.setPen(QPen(TEXT, 1))
        self._text(painter, PAD, y, width - 2 * PAD, self.band[1])
        y += self.line
        painter.setPen(QPen(MUTED, 1))
        self._text(painter, PAD, y, width - 2 * PAD, self.summary())
        y += self.line
        columns = self.columns()
        painter.setFont(self.bold)
        for (heading, _weight), (x, w) in zip(cr.COLUMNS, columns):
            self._text(painter, x, y, w, heading)
        y += self.line
        painter.setPen(QPen(RULE, 1))
        painter.drawLine(QRectF(PAD, y, width - 2 * PAD, 0).topLeft(),
                         QRectF(PAD, y, width - 2 * PAD, 0).topRight())
        painter.setFont(self.font_)
        painter.setClipRect(QRectF(0, y, width, self.viewport().height() - y))
        y -= self.verticalScrollBar().value()
        bottom = self.viewport().height()
        for row in self.rows:
            if y + self.line >= 0 and y <= bottom:
                painter.setPen(QPen(REMOVED if row.removed else TEXT, 1))
                for cell, (x, w) in zip(cr.row_text(row), columns):
                    self._text(painter, x, y, w, cell)
            y += self.line
        painter.end()

    def _text(self, painter, x, y, width, text) -> None:
        """One cell, elided to its width (G-038)."""
        painter.drawText(QRectF(x, y, width, self.line), Qt.AlignmentFlag.AlignVCenter,
                         self.metrics.elidedText(text, Qt.TextElideMode.ElideRight, width - 2))


class RackView(QWidget):
    """The rack list pane (G-036, G-038; G-011's view protocol): the label over the
    `RackListWidget`; `attach(history, selection)` takes the window's history for the
    re-basing; `set_position`, `on_event`, `redraw(now=False)`, `paints`, `events`,
    `pane_id`, `title`, `pages`."""

    def __init__(self, title="Rack list", pane_id="P4b", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = cr.CoderackModel()
        self.history = None
        self.position = -1
        self.list = RackListWidget(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self.label = QLabel(cr.NOT_FRENCHS + ': the rack as Coderack.codelets holds it, the '
                            'oldest first [F95 p. 77]')
        self.label.setToolTip(self.toolTip())
        self.label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        bar = QWidget()
        row = QHBoxLayout(bar)
        row.setContentsMargins(0, 0, 0, 0)
        row.addWidget(self.label, 1)
        column = QVBoxLayout(self)
        column.setContentsMargins(2, 2, 2, 2)
        column.setSpacing(2)
        column.addWidget(bar)
        column.addWidget(self.list, 1)

    def attach(self, history, selection=None) -> None:
        """Take the window's history, for the re-basing on each new snapshot (G-036);
        the selection is not used here."""
        self.history = history

    @property
    def paints(self) -> int:
        """The list's paint count (the window's probe; G-011)."""
        return self.list.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    def set_position(self, index: int) -> None:
        """Which event comes next: the snapshot in force is read at `redraw` (G-017's
        protocol; G-036)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model (G-036)."""
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Re-base the rack on the newest snapshot at or before the position (G-036),
        take the rows, mark the list dirty and, with `now`, paint it at once (Numbo
        GUI, `paint.py`)."""
        if self.history is not None and self.position >= 0:
            self.model.rebase(self.history, self.position)
        self.list.refresh()
        self.list.viewport().update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the list's viewport to a PNG file (RGB), for the tests
        (G-038)."""
        self.list.viewport().grab().toImage().convertToFormat(
            QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()
