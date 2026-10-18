"""P2b Temperature plot: T, Q, ΔQ, Δ_WV and T_cp against the codelets run, the
competitions and the Worldview changes marked, the three terms of T = alpha dQ/Q +
beta dWV - gamma Tcp [F95 p. 91 (img)] as stacked areas on request, and French's
chapter 4 thermometer [F95 pp. 98-107 (img)] as labelled points beside the run's when
the run is `surround-05` (G-028, G-029; ANSWERS.md 11). GUI design, not French's: no
figure of French's plots the temperature (ANSWERS.md 4); the pane, its tooltip and its
caption say so.

`TemperaturePlot` is a `QWidget` that paints `models/temperature.py`'s `plot_layout`,
a pure function of the model, the size and the two options (Numbo GUI,
`coderack_view.py`: a widget that paints its model). `TemperatureView` is the pane: a
bar with the two options as checkable actions (`terms_action`, `french_action`, the
latter enabled only while the run is `surround-05`) over the plot, keeping the view
protocol the window expects (G-011). Hovering a reading shows its numbers and terms.
"""

from PySide6.QtCore import QPointF, QRectF, QSize, Qt
from PySide6.QtGui import QAction, QColor, QFont, QImage, QPainter, QPen, QPolygonF
from PySide6.QtWidgets import QHBoxLayout, QLabel, QSizePolicy, QToolButton, QVBoxLayout, QWidget

from tabletop.gui import paint
from tabletop.gui.models import temperature as tm

__all__ = ["COLOURS", "MIN_SIZE", "OPTION_KEYS", "TemperaturePlot", "TemperatureView"]

# The smallest the pane may be (G-014).
MIN_SIZE = (120, 60)
OPTION_KEYS = ("terms", "french")      # `controls/plot-*` in the settings (G-028)
BACKGROUND = QColor("#fbfbf8")
TEXT, MUTED, AXIS = QColor("#1a1a1a"), QColor("#6b6b6b"), QColor("#c9c9c9")
# One colour per series and per term, by name (G-028).
COLOURS = {"temperature": QColor("#1a1a1a"), "q": QColor("#2a7fbf"), "dq": QColor("#e07b00"),
           "dwv": QColor("#2f9e44"), "tcp": QColor("#8a3ffc"),
           "dq_over_q": QColor(42, 127, 191, 70), "changes": QColor(47, 158, 68, 80),
           "competitions": QColor(230, 0, 126, 70)}
MARKS = {"competition": (QColor("#e6007e"), Qt.PenStyle.DashLine),
         "change": (QColor("#2f9e44"), Qt.PenStyle.SolidLine)}
FRENCH = QColor("#b00020")


class TemperaturePlot(QWidget):
    """The plot itself (G-028): paints the `PlotLayout` of its model for its size and
    the options `terms` and `french`; `drawn` is the last one painted, `paints` the
    count for the window's probe (G-011)."""

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.terms = False
        self.french = False
        self.drawn = tm.plot_layout(model, 10, 10)
        self.paints = 0
        self.setMinimumSize(MIN_SIZE[0], MIN_SIZE[1] - 20)
        self.setMouseTracking(True)
        self.setToolTip(tm.NOT_FRENCHS)

    def current_layout(self):
        """The layout for the current size and options (G-028)."""
        return tm.plot_layout(self.model, self.width(), self.height(), self.terms, self.french)

    def mouseMoveEvent(self, event):
        """The reading nearest the mouse as the tooltip (G-028)."""
        reading = tm.reading_at(self.model, self.drawn, event.position().x())
        if reading is None:
            self.setToolTip(f"{tm.NOT_FRENCHS}: {self.drawn.note}")
        else:
            self.setToolTip(tm.reading_text(reading, self.model.terms(reading)))

    def paintEvent(self, event):
        """Paint the bands, the lines, the areas, the marks, French's points and the
        caption (G-028); counts the paints (G-011)."""
        self.paints += 1
        self.drawn = lay = self.current_layout()
        painter = QPainter(self)
        painter.fillRect(self.rect(), BACKGROUND)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        self._paint_bands(painter, lay)
        self._paint_areas(painter, lay)
        self._paint_marks(painter, lay)
        self._paint_lines(painter, lay)
        self._paint_french(painter, lay)
        self._paint_caption(painter, lay)
        painter.end()

    def _paint_bands(self, painter, lay) -> None:
        """Each band's frame, its label and its y ticks; the x ticks under the last."""
        small = QFont(painter.font())
        small.setPointSize(7)
        painter.setFont(small)
        for band in lay.bands:
            painter.setPen(QPen(AXIS, 1))
            painter.drawRect(QRectF(lay.left, band.top, lay.right - lay.left, band.bottom - band.top))
            painter.setPen(QPen(MUTED, 1))
            for y, label in band.ticks:
                painter.drawText(QRectF(0, y - 6, lay.left - 3, 12),
                                 Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter, label)
            painter.drawText(QRectF(lay.left + 3, band.top + 1, 80, 12),
                             Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop, band.label)
        bottom = lay.bands[-1].bottom
        for x, label in lay.ticks:
            painter.drawText(QRectF(x - 20, bottom + 2, 40, 12), Qt.AlignmentFlag.AlignHCenter
                             | Qt.AlignmentFlag.AlignTop, label)

    def _paint_lines(self, painter, lay) -> None:
        """Every series as a polyline in its colour, with a dot per reading."""
        for band in lay.bands:
            for line in band.lines:
                colour = COLOURS[line.name]
                painter.setPen(QPen(colour, 1.5 if line.name == "temperature" else 1.2))
                pts = [QPointF(x, y) for x, y in line.points]
                if len(pts) > 1:
                    painter.drawPolyline(QPolygonF(pts))
                for p in pts:
                    painter.drawEllipse(p, 1.5, 1.5)

    def _paint_areas(self, painter, lay) -> None:
        """The stacked terms as translucent polygons on the T band (G-028)."""
        painter.setPen(Qt.PenStyle.NoPen)
        for area in lay.areas:
            pts = [QPointF(x, y) for x, y in area.upper] + \
                  [QPointF(x, y) for x, y in reversed(area.lower)]
            painter.setBrush(COLOURS[area.name])
            painter.drawPolygon(QPolygonF(pts))
        painter.setBrush(Qt.BrushStyle.NoBrush)

    def _paint_marks(self, painter, lay) -> None:
        """A vertical line per competition (dashed) and change (solid) across the bands."""
        top, bottom = lay.bands[0].top, lay.bands[-1].bottom
        for mark in lay.marks:
            colour, style = MARKS[mark.kind]
            painter.setPen(QPen(colour, 1, style))
            painter.drawLine(QPointF(mark.x, top), QPointF(mark.x, bottom))

    def _paint_french(self, painter, lay) -> None:
        """French's readings as labelled hollow diamonds on the T band (G-029)."""
        painter.setPen(QPen(FRENCH, 1.2))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        small = QFont(painter.font())
        small.setPointSize(7)
        painter.setFont(small)
        for i, point in enumerate(lay.french):
            d = 4
            painter.drawPolygon(QPolygonF([QPointF(point.x, point.y - d), QPointF(point.x + d, point.y),
                                           QPointF(point.x, point.y + d), QPointF(point.x - d, point.y)]))
            dy = -14 if i % 2 == 0 else 6
            x = min(max(point.x - 40, 0.0), self.width() - 80.0)     # the label stays inside
            painter.drawText(QRectF(x, point.y + dy, 80, 12),
                             Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignTop, point.label)

    def _paint_caption(self, painter, lay) -> None:
        """The caption above the bands: the design's status and the state (G-028)."""
        small = QFont(painter.font())
        small.setPointSize(7)
        painter.setFont(small)
        painter.setPen(QPen(MUTED, 1))
        painter.drawText(QRectF(2, 2, self.width() - 4, 14),
                         Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter, lay.note)


class TemperatureView(QWidget):
    """The temperature plot pane (G-028, G-029; G-011's view protocol): the bar with the
    two options over the `TemperaturePlot`; `on_event`, `redraw(now=False)`, `paints`,
    `events`, `pane_id`, `title`, `pages`; `plot_state` and `set_plot_state` for the
    settings; `options_changed` is a plain callback list for the window."""

    def __init__(self, title="Temperature plot", pane_id="P2b", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = tm.TemperatureModel()
        self.plot = TemperaturePlot(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self._build_bar()
        column = QVBoxLayout(self)
        column.setContentsMargins(2, 2, 2, 2)
        column.setSpacing(2)
        column.addWidget(self.bar)
        column.addWidget(self.plot, 1)

    def _build_bar(self) -> None:
        """The two checkable actions as tool buttons, and the label (G-028)."""
        self.terms_action = QAction("Terms", self)
        self.terms_action.setCheckable(True)
        self.terms_action.setToolTip("the three terms of T = α ΔQ/Q + β Δ_WV − γ T_cp as "
                                     "stacked areas [F95 p. 91 (img)]; " + tm.NOT_FRENCHS)
        self.terms_action.toggled.connect(self._on_option)
        self.french_action = QAction("French's ch. 4", self)
        self.french_action.setCheckable(True)
        self.french_action.setEnabled(False)
        self.french_action.setToolTip("French's chapter 4 thermometer, 80.0 at 2 codelets to "
                                      "2.0 at 55 [F95 pp. 98-107 (img)], beside the run's; "
                                      f"offered for {tm.CH4_PROBLEM} only; " + tm.NOT_FRENCHS)
        self.french_action.toggled.connect(self._on_option)
        self.bar = QWidget()
        row = QHBoxLayout(self.bar)
        row.setContentsMargins(0, 0, 0, 0)
        row.setSpacing(4)
        self.buttons = {}
        for name, action in (("terms", self.terms_action), ("french", self.french_action)):
            button = QToolButton()
            button.setDefaultAction(action)
            button.setMinimumWidth(24)
            self.buttons[name] = button
            row.addWidget(button)
        self.label = QLabel(tm.NOT_FRENCHS)
        self.label.setToolTip(self.toolTip())
        self.label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        row.addWidget(self.label, 1)

    @property
    def paints(self) -> int:
        """The plot's paint count (the window's probe; G-011)."""
        return self.plot.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    @property
    def layout_now(self):
        """The plot's geometry for its current size and options (G-028)."""
        return self.plot.current_layout()

    def plot_state(self) -> dict:
        """The two options, by `OPTION_KEYS` (G-028)."""
        return {"terms": self.terms_action.isChecked(), "french": self.french_action.isChecked()}

    def set_plot_state(self, state) -> None:
        """Set the options named in `state`; the others stay (G-028)."""
        if "terms" in state:
            self.terms_action.setChecked(bool(state["terms"]))
        if "french" in state:
            self.french_action.setChecked(bool(state["french"]))

    def _on_option(self, _checked=False) -> None:
        """An option changed: the plot takes it and redraws now (G-028)."""
        self.plot.terms = self.terms_action.isChecked()
        self.plot.french = self.french_action.isChecked()
        self.redraw(now=True)

    def on_event(self, event) -> None:
        """Feed the model; the French option is offered only while the run is
        `surround-05` (G-029)."""
        self.model.on_event(event)
        self.french_action.setEnabled(self.model.french_offered)

    def redraw(self, now=False) -> None:
        """Mark the plot dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.plot.update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the plot to a PNG file (RGB), for the tests (G-028)."""
        self.plot.grab().toImage().convertToFormat(QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()
