"""P5 Worldview, GUI design, not French's, from the text of [F95 pp. 67-74] (G-039,
G-040): the correspondences in the Worldview and in the shadow cabinet with their points,
wins, losses, slippages and rivals, the rating with its pairs, and the Proposals tab
(D-024), as lines of text styled by role: an entry of the Worldview with a solid bar at
its left and one of the shadow cabinet with a dashed bar, Fig. 3.11's solid against
dashed [F95 p. 72 (img)]; an incoherent pair in a warning colour, a rival said in words
[F95 p. 105].

`LinesWidget` is a `QAbstractScrollArea` that paints a model's `lines()` on its viewport
(Numbo GUI, `coderack_view.py`: a widget that paints its model; the rack list's way,
G-038): hovering a line shows the hover text of what it is about, a left click toggles it
in the window's shared selection (G-022), and the selected lines are tinted. The
competitions pane paints its model through the same widget. `WorldviewView` is the pane:
the label over a tab widget with the Worldview and the Proposals, keeping the view
protocol the window expects (G-011) and taking the window's history and selection
through `attach`.
"""

from PySide6.QtCore import QRectF, Qt
from PySide6.QtGui import QColor, QFont, QFontMetricsF, QImage, QPainter, QPen
from PySide6.QtWidgets import (QAbstractScrollArea, QHBoxLayout, QLabel, QSizePolicy,
                               QTabWidget, QVBoxLayout, QWidget)

from tabletop.gui import paint
from tabletop.gui.models import worldview as wv

__all__ = ["BACKGROUND", "FRESH_TINT", "MIN_SIZE", "SELECTED_TINT", "WARNING",
           "LinesWidget", "ProposalLines", "WorldviewView"]

# The smallest the pane may be (G-014): a few lines.
MIN_SIZE = (200, 70)
BACKGROUND = QColor("#fbfbf8")
TEXT, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
WARNING = QColor("#a63d00")                  # an incoherent pair (G-040)
SELECTED_TINT = QColor("#fde8c8")            # the shared selection (G-022)
FRESH_TINT = QColor("#fbe3f0")               # the event shown (Numbo GUI, coderack_view.py)
BAR = QColor("#1a1a1a")
PAD = 4.0
BAR_WIDTH = 3.0
INDENT = 10.0


class LinesWidget(QAbstractScrollArea):
    """Paints the `TextLine`s of a model that has `lines()`, `tooltip(name)` and
    `selection_of(name)` (G-040): a solid bar before a `worldview` line, a dashed one
    before a `shadow` line, headings bold, details indented, warnings coloured; the lines
    of the selected structure tinted, the lines of the event shown tinted magenta.
    `lines` are the ones drawn, `paints` the count for the window's probe (G-011)."""

    def __init__(self, model, selection=None, parent=None):
        super().__init__(parent)
        self.model = model
        self.selection = selection
        self._lines = ()
        self._stale = True
        self.paints = 0
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(8.5)
        # Greyscale antialiasing, so that a line's colour can be read off its pixels (G-038).
        self.font_.setStyleStrategy(QFont.StyleStrategy.NoSubpixelAntialias)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.metrics = QFontMetricsF(self.font_)
        self.line = self.metrics.height() + 3
        self.setMinimumSize(MIN_SIZE[0], MIN_SIZE[1] - 22)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.viewport().setMouseTracking(True)
        self.setToolTip("hover a line for its structure")

    def refresh(self) -> None:
        """Mark the lines stale: they are taken from the model when next painted or read
        (`lines`), so that a widget hidden under a tab costs nothing per event (G-040,
        G-058). A widget on screen (a tabbed-away dock's is `isVisible` with an empty
        region) takes them at once, so that the scroll range, and the bar, change before
        the pass that paints it rather than inside the viewport's paint, where the bar
        would show the last run's handle for one event (G-040)."""
        self._stale = True
        if not self.visibleRegion().isEmpty():
            self.lines

    @property
    def lines(self) -> tuple:
        """The lines drawn (G-040), taken from the model when stale, with the scroll
        range set from their count (G-058)."""
        if self._stale:
            self._lines = tuple(self.model.lines())
            self._stale = False
            self._set_range()
        return self._lines

    def _set_range(self) -> None:
        content = self.line * len(self._lines) + 2 * PAD
        self.verticalScrollBar().setRange(0, max(0, int(content - self.viewport().height())))
        self.verticalScrollBar().setPageStep(max(1, self.viewport().height()))
        self.verticalScrollBar().setSingleStep(int(self.line))

    def resizeEvent(self, event):
        """The scroll range follows the viewport's height (G-040)."""
        super().resizeEvent(event)
        self._set_range()

    def line_at(self, y):
        """The line under a viewport y, or None (G-040)."""
        k = int((y - PAD + self.verticalScrollBar().value()) // self.line)
        return self.lines[k] if 0 <= k < len(self.lines) else None

    def line_top(self, k: int) -> float:
        """The viewport y of the k-th line's top (G-040)."""
        return PAD + k * self.line - self.verticalScrollBar().value()

    def mouseMoveEvent(self, event):
        """The hover text of the structure the line under the mouse is about (G-040)."""
        found = self.line_at(event.position().y())
        text = self.model.tooltip(found.name) if found is not None and found.name else ""
        text = text or "hover a line for its structure"
        self.setToolTip(text)
        self.viewport().setToolTip(text)

    def mousePressEvent(self, event):
        """A left click toggles the structure the line is about in the shared selection;
        on no structure it clears the selection (G-022, G-040)."""
        if event.button() != Qt.MouseButton.LeftButton or self.selection is None:
            return
        found = self.line_at(event.position().y())
        what = self.model.selection_of(found.name) if found is not None and found.name else None
        if what is None:
            self.selection.clear()
        else:
            self.selection.toggle(*what)

    def _selected(self, line) -> bool:
        if self.selection is None or not line.name:
            return False
        what = self.model.selection_of(line.name)
        return what is not None and self.selection.kind == what[0] \
            and self.selection.name == what[1]

    def paintEvent(self, event):
        """Paint the lines (G-040); counts the paints (G-011)."""
        self.paints += 1
        painter = QPainter(self.viewport())
        painter.fillRect(self.viewport().rect(), BACKGROUND)
        width = self.viewport().width()
        bottom = self.viewport().height()
        for k, line in enumerate(self.lines):
            y = self.line_top(k)
            if y + self.line < 0 or y > bottom:
                continue
            if line.fresh:
                painter.fillRect(QRectF(0, y, width, self.line), FRESH_TINT)
            elif self._selected(line):
                painter.fillRect(QRectF(0, y, width, self.line), SELECTED_TINT)
            if line.cells:
                half = (width - 2 * PAD) / 2
                for i, (text, role) in enumerate(line.cells):
                    self._cell(painter, PAD + i * half, y, half, text, role)
            else:
                self._cell(painter, PAD, y, width - 2 * PAD, line.text, line.role)
        painter.end()

    def _cell(self, painter, x, y, width, text, role) -> None:
        """One cell: its bar, its font and colour by role, the text elided (G-040)."""
        indent = 0.0
        if role in (wv.WORLDVIEW, wv.SHADOW):
            pen = QPen(BAR, BAR_WIDTH)
            if role == wv.SHADOW:
                pen.setDashPattern([1.0, 1.0])      # dashes as long as the bar is wide
                # A flat cap: the default square cap lengthens each dash by half the
                # width at both ends, which fills gaps as long as the width (G-060).
                pen.setCapStyle(Qt.PenCapStyle.FlatCap)
            painter.setPen(pen)
            painter.drawLine(QRectF(x + 1, y + 2, 0, self.line - 4).topLeft(),
                             QRectF(x + 1, y + 2, 0, self.line - 4).bottomLeft())
            indent = BAR_WIDTH + 4
        elif role == wv.DETAIL:
            indent = INDENT
        painter.setFont(self.bold if role == wv.HEADING else self.font_)
        painter.setPen(QPen(WARNING if role == wv.WARNING else MUTED if role == wv.MUTED
                            else TEXT, 1))
        room = width - indent - 2
        painter.drawText(QRectF(x + indent, y, room, self.line), Qt.AlignmentFlag.AlignVCenter,
                         self.metrics.elidedText(text, Qt.TextElideMode.ElideRight, room))


class ProposalLines:
    """The Proposals tab's model face (G-040): the Worldview model's `proposal_lines`
    as `lines`, with its hover text and selection."""

    def __init__(self, model):
        self.model = model

    def lines(self) -> list:
        """The proposals as lines (D-024; G-040)."""
        return self.model.proposal_lines()

    def tooltip(self, name: str) -> str:
        """A proposal's hover text (G-023)."""
        return self.model.tooltip(name)

    def selection_of(self, name: str):
        """What a click on a proposal selects (G-022)."""
        return self.model.selection_of(name)


class WorldviewView(QWidget):
    """The Worldview pane (G-039, G-040; G-011's view protocol): the label over a tab
    widget with the Worldview's lines and the Proposals' lines; `attach(history,
    selection)` takes the window's history (the snapshot in force and the readings
    beside it) and shared selection; `set_position`, `on_event`, `redraw(now=False)`,
    `paints`, `events`, `pane_id`, `title`, `pages`."""

    def __init__(self, title="Worldview", pane_id="P5", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = wv.WorldviewModel()
        self.history = None
        self.position = -1
        self.worldview = LinesWidget(self.model)
        self.proposals = LinesWidget(ProposalLines(self.model))
        self.tabs = QTabWidget()
        self.tabs.setDocumentMode(True)
        self.tabs.addTab(self.worldview, "Worldview")
        self.tabs.addTab(self.proposals, "Proposals")
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self.label = QLabel(wv.NOT_FRENCHS + ': the Worldview and the shadow cabinet from the '
                            'text [F95 pp. 67-74]; rivals against incoherent pairs [F95 p. 105]')
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
        column.addWidget(self.tabs, 1)

    def attach(self, history, selection=None) -> None:
        """Take the window's history and shared selection (G-022, G-039)."""
        self.history = history
        self.worldview.selection = selection
        self.proposals.selection = selection
        if selection is not None:
            selection.subscribe(self._on_selection)

    def _on_selection(self, _selection) -> None:
        """The shared selection changed, from this pane or another: redraw now (G-022)."""
        self.redraw(now=True)

    @property
    def paints(self) -> int:
        """The paints of both tabs (the window's probe counts the one on screen; G-011)."""
        return self.worldview.paints + self.proposals.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    def set_position(self, index: int) -> None:
        """Which event comes next: the snapshot in force is read at `redraw` (G-017's
        protocol; G-039)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model (G-039)."""
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Give the model the snapshot in force with its terms and readings (G-039),
        take the lines, mark both tabs dirty and, with `now`, paint at once (Numbo GUI,
        `paint.py`)."""
        if self.history is not None and self.position >= 0:
            self.model.update(self.history.state_at(self.position),
                              self.history.terms_at(self.position),
                              self.history.worldview_at(self.position))
        for widget in (self.worldview, self.proposals):
            widget.refresh()
            widget.viewport().update()
        if now:
            paint.paint_all()

    def render_png(self, path, tab=0) -> bytes:
        """A fresh render of a tab's viewport to a PNG file (RGB), for the tests
        (G-040)."""
        widget = (self.worldview, self.proposals)[tab]
        widget.viewport().grab().toImage().convertToFormat(
            QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()
