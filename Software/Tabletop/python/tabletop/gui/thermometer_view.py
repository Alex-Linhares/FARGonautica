"""P2 Thermometer: French's thermometer, "Immediately to the right of the table is the
thermometer" [F95 p. 95], a tall narrow column filled from the bottom to T with the
value printed beside the top of the fill, as the dumps draw it [F95 pp. 98-107 (img)]
(G-027; Numbo GUI, `coderack_view.py`: a `QWidget` that paints its model).

The view holds a `TemperatureModel` (`models/temperature.py`) and paints its
`Thermometer` form, a pure function of the events, so a fresh view fed the history draws
the window's picture (the tests check it pixel for pixel). What the GUI adds to French's
column, and says so in its tooltip: the fill is grey while T is still held at its
initial value (D-049), a small tick at the fill's top names the reason of the last
update (`start`, `schedule`, `competition`, `change`; D-050), faint marks at 0, 50 and
100 give the scale, and the reading's codelet and age are written under the column: the
value at the answer is the last update's, up to four codelets old (D-050). Before the
first reading the column is empty, as at 0 codelets [F95 p. 96 (img)].
"""

from PySide6.QtCore import QRectF, QSize, Qt
from PySide6.QtGui import QColor, QFont, QImage, QPainter, QPen
from PySide6.QtWidgets import QWidget

from tabletop.gui import paint
from tabletop.gui.models.temperature import TemperatureModel, reading_text

__all__ = ["COLUMN_WIDTH", "FILL", "HELD_FILL", "MIN_SIZE", "ThermometerView", "column_rect"]

# The smallest the pane may be: the window fits 1280 x 800 with every dock shown
# (G-014); the thermometer is narrow, as on French's screen [F95 p. 96 (img)].
MIN_SIZE = (56, 60)
COLUMN_WIDTH = 16
PAD_TOP, PAD_BOTTOM, PAD_LEFT = 14, 30, 22
BACKGROUND = QColor("#fbfbf8")
FRAME = QColor("#1a1a1a")
FILL = QColor("#1a1a1a")          # French's black fill [F95 p. 98 (img)]
HELD_FILL = QColor("#9a9a9a")     # the grey band while T is held (D-049)
SCALE = QColor("#b5b5b5")
TEXT, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
TICK = QColor("#e6007e")


def column_rect(width, height) -> QRectF:
    """The column's rectangle for a widget size: French's narrow column with room for
    the value at its right and the reading's codelet under it (G-027)."""
    h = max(10.0, height - PAD_TOP - PAD_BOTTOM)
    return QRectF(PAD_LEFT, PAD_TOP, COLUMN_WIDTH, h)


class ThermometerView(QWidget):
    """The thermometer pane (G-027; G-011's view protocol): `on_event` feeds the model,
    `redraw(now=False)` marks the widget dirty, `paints` counts the paints, `form` is
    the `Thermometer` drawn, `pane_id`, `title` and `pages` are the window's."""

    def __init__(self, title="Thermometer", pane_id="P2", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = TemperatureModel()
        self.form = self.model.thermometer()
        self.paints = 0
        self.setMinimumSize(*MIN_SIZE)
        self.setAutoFillBackground(False)
        self._set_tooltip()

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    def sizeHint(self):
        """Narrow and the table's height, as on French's screen [F95 p. 96 (img)] (G-027)."""
        return QSize(90, 320)

    def on_event(self, event) -> None:
        """Feed the model and take the new form (G-027)."""
        self.model.on_event(event)
        self.form = self.model.thermometer()
        self._set_tooltip()

    def redraw(self, now=False) -> None:
        """Mark the view dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.update()
        if now:
            paint.paint_all()

    def _set_tooltip(self) -> None:
        """The pages behind the pane, then the reading in words (G-027)."""
        lines = [f"{self.pane_id} {self.title}: {self.pages}",
                 "grey while held, the reason tick, the scale marks and the age are the "
                 "GUI's additions (D-049, D-050)", self.form.note]
        if self.model.last is not None:
            lines.append(reading_text(self.model.last, self.model.terms()))
        self.setToolTip("\n".join(lines))

    def render_png(self, path) -> bytes:
        """A fresh render of the widget to a PNG file (RGB, so that a grab inside a window
        and a bare widget's compare), for the tests (G-027)."""
        self.grab().toImage().convertToFormat(QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()

    def paintEvent(self, event):
        """Paint the column, the fill, the value, the reason tick and the reading's
        codelet (G-027); `paints` counts the calls for the window's probe (G-011)."""
        self.paints += 1
        painter = QPainter(self)
        painter.fillRect(self.rect(), BACKGROUND)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        column = column_rect(self.width(), self.height())
        self._paint_scale(painter, column)
        self._paint_fill(painter, column)
        painter.setPen(QPen(FRAME, 1))
        painter.drawRect(column)
        self._paint_text(painter, column)
        painter.end()

    def _paint_scale(self, painter, column) -> None:
        """Faint marks at 0, 50 and 100 to the left of the column (the GUI's; G-027)."""
        painter.setPen(QPen(SCALE, 1))
        font = QFont(painter.font())
        font.setPointSize(7)
        painter.setFont(font)
        for value in (0, 50, 100):
            y = column.bottom() - column.height() * value / 100.0
            painter.drawLine(int(column.left()) - 4, int(y), int(column.left()) - 1, int(y))
            painter.setPen(QPen(MUTED, 1))
            painter.drawText(QRectF(0, y - 6, column.left() - 5, 12),
                             Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
                             str(value))
            painter.setPen(QPen(SCALE, 1))

    def _paint_fill(self, painter, column) -> None:
        """The fill from the bottom to T, black as French's, grey while held (D-049);
        nothing before the first reading [F95 p. 96 (img)] (G-027)."""
        form = self.form
        if form.value is None:
            return
        top = column.bottom() - column.height() * form.fill
        painter.fillRect(QRectF(column.left(), top, column.width(), column.bottom() - top),
                         HELD_FILL if form.held else FILL)
        # The reason tick: a short magenta line at the fill's top, to the right.
        painter.setPen(QPen(TICK, 2))
        painter.drawLine(int(column.right()) + 2, int(top), int(column.right()) + 7, int(top))

    def _paint_text(self, painter, column) -> None:
        """The value beside the fill's top, as the dumps print it [F95 p. 98 (img)]; the
        reason and the reading's codelet under the column (G-027)."""
        form = self.form
        font = QFont(painter.font())
        font.setPointSize(8)
        painter.setFont(font)
        painter.setPen(QPen(TEXT, 1))
        if form.value is not None:
            top = column.bottom() - column.height() * form.fill
            y = min(max(top - 7, column.top()), column.bottom() - 14)
            painter.drawText(QRectF(column.right() + 9, y, self.width() - column.right() - 9, 14),
                             Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
                             form.text)
        font.setPointSize(7)
        painter.setFont(font)
        painter.setPen(QPen(MUTED, 1))
        foot = QRectF(2, column.bottom() + 3, self.width() - 4, PAD_BOTTOM - 4)
        text = (f"{form.by}\nat {form.clock}" + (f" (+{form.age})" if form.age else "")
                if form.value is not None else form.note)
        painter.drawText(foot, Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignHCenter
                         | Qt.TextFlag.TextWordWrap, text)
