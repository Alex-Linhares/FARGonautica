"""P4 Coderack bars: French's strip under the table, "a representation of the Coderack,
indicating the percentage of particular codelet types waiting to run. The total number
of codelets run so far is also indicated." [F95 p. 97]: one bar per type at its label in
the strip's order, the percent printed above the bar and "Number of codelets run: N" over
the box's right end, as the dumps print them [F95 pp. 98-107 (img)]; the bar of the
codelet just chosen outlined; the catalogue legend (D-051) on request (G-036, G-037).

`CoderackStripWidget` is a `QWidget` that paints `models/coderack.py`'s `strip_layout`,
a pure function of the model and the size (Numbo GUI, `coderack_view.py`: a widget that
paints its model); hovering a slot shows the type's legend entry and its share.
`CoderackView` is the pane: a bar with the legend as a checkable action over the strip,
and the legend's text beside the strip while the action is on, keeping the view protocol
the window expects (G-011).
"""

from PySide6.QtCore import QRectF, Qt
from PySide6.QtGui import QAction, QColor, QFont, QImage, QPainter, QPen
from PySide6.QtWidgets import (QHBoxLayout, QLabel, QPlainTextEdit, QSizePolicy, QToolButton,
                               QVBoxLayout, QWidget)

from tabletop.gui import paint
from tabletop.gui.models import coderack as cr

__all__ = ["BACKGROUND", "BAR", "BOX", "CHOSEN", "MIN_SIZE", "OPTION_KEYS", "CoderackStripWidget",
           "CoderackView"]

# The smallest the pane may be (G-014): 23 slots of about 8 px, the counter, the labels.
MIN_SIZE = (200, 70)
OPTION_KEYS = ("legend",)            # `controls/coderack-*` in the settings (G-037)
BACKGROUND = QColor("#fbfbf8")
BOX = QColor("#1a1a1a")              # the strip's box [F95 p. 107 (img)]
BAR = QColor("#000000")              # the bars, black as the dumps print them
TEXT, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
CHOSEN = QColor("#e6007e")           # the outline of the codelet just chosen (G-037)
STAGGER = cr.LABEL_HEIGHT / 2        # the odd labels sit on a second line (G-037)


class CoderackStripWidget(QWidget):
    """The strip itself (G-037): paints the `StripLayout` of its model for its size;
    `drawn` is the last one painted, `paints` the count for the window's probe
    (G-011); hovering a slot gives the type's hover text."""

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.drawn = cr.strip_layout(model, 10, 10)
        self.paints = 0
        self.setMinimumSize(MIN_SIZE[0], MIN_SIZE[1] - 22)
        self.setMouseTracking(True)
        self.setToolTip("hover a bar for its codelet type")

    def current_layout(self):
        """The layout for the current size (G-037)."""
        return cr.strip_layout(self.model, self.width(), self.height())

    def mouseMoveEvent(self, event):
        """The slot under the mouse as the tooltip (G-037)."""
        bar = cr.bar_at(self.drawn, event.position().x(), event.position().y())
        self.setToolTip("hover a bar for its codelet type" if bar is None
                        else self.model.tooltip(bar.type))

    def paintEvent(self, event):
        """Paint the counter, the box, the bars with their percents, the labels and the
        chosen outline (G-037); counts the paints (G-011)."""
        self.paints += 1
        self.drawn = lay = self.current_layout()
        painter = QPainter(self)
        painter.fillRect(self.rect(), BACKGROUND)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        small = QFont(painter.font())
        small.setPointSizeF(8.0)
        tiny = QFont(painter.font())
        tiny.setPointSizeF(max(5.0, min(7.0, lay.bars[0].slot_width / 5.0)))
        x, y, w, h = lay.box
        painter.setPen(QPen(BOX, 1))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawRect(QRectF(x, y, w, h))
        for i, bar in enumerate(lay.bars):
            self._paint_bar(painter, lay, bar, i, small, tiny)
        painter.end()

    def _paint_bar(self, painter, lay, bar, i, small, tiny) -> None:
        """One slot: the bar, its percent above, the label under the box (the odd ones
        a line lower, so that they fit), the chosen outline (G-037)."""
        if bar.count:
            painter.fillRect(QRectF(bar.x, bar.y, bar.width, bar.height), BAR)
            painter.setFont(small)
            painter.setPen(QPen(TEXT, 1))
            painter.drawText(QRectF(bar.slot_x, bar.y - cr.NUMBER_HEIGHT, bar.slot_width,
                                    cr.NUMBER_HEIGHT), Qt.AlignmentFlag.AlignCenter, bar.number)
        if bar.chosen:
            base = lay.box[1] + lay.box[3] - 1
            top = min(bar.y, base - 4)
            painter.setPen(QPen(CHOSEN, 2))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRect(QRectF(bar.x - 2, top - 2, bar.width + 4, base - top + 4))
        painter.setFont(tiny)
        painter.setPen(QPen(TEXT, 1))
        painter.drawText(QRectF(bar.slot_x - bar.slot_width / 2,
                                lay.label_y + (STAGGER if i % 2 else 0), 2 * bar.slot_width,
                                STAGGER), Qt.AlignmentFlag.AlignCenter, bar.label)


class CoderackView(QWidget):
    """The Coderack bars pane (G-036, G-037; G-011's view protocol): the bar with the
    legend option and the counter "Number of codelets run: N" at its right end, over the
    `CoderackStripWidget`, the legend's text beside the strip while the option is on; `on_event`, `redraw(now=False)`, `paints`, `events`,
    `pane_id`, `title`, `pages`; `coderack_state` and `set_coderack_state` for the
    settings."""

    def __init__(self, title="Coderack bars", pane_id="P4", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = cr.CoderackModel()
        self.strip = CoderackStripWidget(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self._build_bar()
        self.legend = QPlainTextEdit()
        self.legend.setReadOnly(True)
        self.legend.setPlainText(cr.legend_text())
        self.legend.setToolTip("the catalogue of the codelet types: the legend's 23 (D-051) "
                               "[F95 p. 96 (img)] and the role codelets (D-081)")
        self.legend.setMinimumSize(40, 20)
        self.legend.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)
        self.legend.hide()
        column = QVBoxLayout(self)
        column.setContentsMargins(2, 2, 2, 2)
        column.setSpacing(2)
        column.addWidget(self.bar)
        row = QHBoxLayout()
        row.setContentsMargins(0, 0, 0, 0)
        row.setSpacing(4)
        row.addWidget(self.strip, 3)
        row.addWidget(self.legend, 2)
        column.addLayout(row, 1)

    def _build_bar(self) -> None:
        """The legend option as a tool button, and the label (G-037)."""
        self.legend_action = QAction("Legend", self)
        self.legend_action.setCheckable(True)
        self.legend_action.setToolTip("the catalogue legend beside the strip: each type's label, "
                                      "full name, role and job, and whether the job is from the "
                                      "text or inferred from the name (D-051) [F95 p. 96 (img)]")
        self.legend_action.toggled.connect(self._on_option)
        self.bar = QWidget()
        row = QHBoxLayout(self.bar)
        row.setContentsMargins(0, 0, 0, 0)
        row.setSpacing(4)
        self.buttons = {}
        button = QToolButton()
        button.setDefaultAction(self.legend_action)
        button.setMinimumWidth(24)
        self.buttons["legend"] = button
        row.addWidget(button)
        self.label = QLabel(f'the {len(cr.STRIP_ORDER)} types in the strip\'s order, the percents '
                            'and the count run [F95 p. 97]; the chosen bar outlined')
        self.label.setToolTip(self.toolTip())
        self.label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        row.addWidget(self.label, 1)
        self.counter = QLabel(cr.strip_layout(self.model, 10, 10).counter)
        self.counter.setToolTip('"The total number of codelets run so far is also indicated." '
                                '[F95 p. 97]: over the strip\'s right end, as on the screen '
                                '[F95 p. 107 (img)]')
        self.counter.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        self.counter.setMinimumWidth(1)
        row.addWidget(self.counter, 0)

    @property
    def paints(self) -> int:
        """The strip's paint count (the window's probe; G-011)."""
        return self.strip.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    @property
    def layout_now(self):
        """The strip's geometry for its current size (G-037)."""
        return self.strip.current_layout()

    def coderack_state(self) -> dict:
        """The options, by `OPTION_KEYS` (G-037)."""
        return {"legend": self.legend_action.isChecked()}

    def set_coderack_state(self, state) -> None:
        """Set the options named in `state`; the others stay (G-037)."""
        if "legend" in state:
            self.legend_action.setChecked(bool(state["legend"]))

    def _on_option(self, _checked=False) -> None:
        """The legend option changed: show or hide the legend's text (G-037)."""
        self.legend.setVisible(self.legend_action.isChecked())

    def on_event(self, event) -> None:
        """Feed the model (G-036)."""
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Set the counter, mark the strip dirty and, with `now`, paint it at once
        (Numbo GUI, `paint.py`)."""
        self.counter.setText(f"Number of codelets run: {self.model.clock}")
        self.strip.update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the strip to a PNG file (RGB), for the tests (G-037)."""
        self.strip.grab().toImage().convertToFormat(QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()
