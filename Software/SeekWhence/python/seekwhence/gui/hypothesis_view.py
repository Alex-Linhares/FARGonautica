"""The hypothesis pane (P4): the form large with its source, the form as a tree of ovals,
the validity bar with the guess threshold, the predictions against the terms, and a list
with the candidates, the box tree (hidden by default) and a private box to hit, a view of
`models/hypothesis.py`'s `HypothesisModel` (PANES.md P4; G-033, G-034, G-035).

"the hypothesis can display its form ... It can also predict the next term ... it has a
validity" [M86 p. 81]. The form is printed large as the transcripts print it after
`hypothesis:`, `(Cycle 3 2 (3 7))` [M86 p. 166], with its source: the template, the
Tuple fall-back [M86 p. 79] or a candidate [M86 p. 125]. Under it the form is drawn as
Ch. 1 Figures 1 and 2 draw the primitives: the type's name in an oval, the arguments in
circles hanging from it, nested forms as child ovals, a shared structure joined by lines
to each place that shares it, and beside the top oval the first values it gives,
`(2 3 4)` [M86 pp. 16-17, 50-53]; a click on an oval lights the terms it gives in the
predictions table. The validity is a number and a bar, "a number that grows as new,
correctly-predicted terms are encountered" [M86 p. 81], falling to 0 at a refutation,
`set-out -- validity: 0` [M86 p. 168], with a mark at the guess threshold. The
predictions table has the terms a fresh box gives above the terms entered, matches
green, the refuting term red with the term that was predicted [M86 p. 168]. The list
below gives the episode's casts and whether each postdicts the terms [M86 pp. 125,
174], the box tree as Ch. 2 Figure 3's records, with the run's own names [M86 p. 83],
when its button is on, and the hits of the private box, the appendix's `show-box`
[M86 p. 161]. The list is a `LineList` (Numbo GUI, `log_view.py`'s `LogList`, through
`taskrack_view.py`; G-028).
"""

from PySide6.QtCore import QPointF, QRect, QRectF, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import (
    QHBoxLayout, QLabel, QLineEdit, QPushButton, QToolButton, QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.models import hypothesis as hy
from seekwhence.gui.taskrack_view import LineList

__all__ = ["OvalTree", "ValidityBar", "PredictionStrip", "HypothesisList", "HypothesisView"]

PAGES = "[M86 pp. 16-17, 50-53, 79-85, 125, 161, 166-168, 174]"
BACKGROUND = "#fbfbf8"
TEXT, MUTED, GREY = "#1a1a1a", "#6b6b6b", "#9a9a9a"
OVAL_FILL, OVAL_BORDER = "#eef3ff", "#2f5597"       # the hypothesis's blue
LEAF_FILL, LEAF_BORDER = "#ffffff", "#4a4a4a"
SHARED_BORDER = "#8a8a8a"
SELECTED_FILL, SELECTED_BORDER = "#ffe9cc", "#e07b00"
AGREES_FILL, AGREES_BORDER = "#e3f3e3", "#2e7d32"
REFUTING_FILL, REFUTING_BORDER = "#fde2e2", "#c62828"
DIFFERS_FILL, DIFFERS_BORDER = "#f3f3f3", "#8a8a8a"
BEYOND_BORDER = "#b0b0b0"
BAR_TRACK, BAR_FILL, BAR_MARK = "#e6e6e6", "#2f5597", "#c62828"
HEAD_TINT = "#eef3ff"
GOOD, BAD = "#2e7d32", "#c62828"
MARGIN = 8
PAD = 4
INDENT = 16
OVAL_W, OVAL_H = 64, 28
LEAF_R = 11
SHARED_R = 7
CELL_W, CELL_H = 34, 22
ROW_GAP = 4


class OvalTree(QWidget):
    """The form as Figures 1 and 2 draw it [M86 pp. 16-17] (G-034): the `TreeNode`s of
    the model's `tree()`, scaled to fit the width; the top oval's first hits beside it;
    a click emits `clicked(node_id)` (None for empty space; the top oval's id is the
    empty tuple); the selected oval
    is tinted; `paints` counts the paint calls (Numbo GUI, `tree_view.py`'s `paints`)."""

    clicked = Signal(object)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.nodes = ()
        self.selected = None
        self.names = {}
        self.paints = 0
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(9)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.metrics = QFontMetrics(self.font_)
        self.setMinimumHeight(40)            # a short dock clips the tree (G-058)
        self.setAutoFillBackground(True)

    def sizeHint(self):
        """Room for three levels (G-034)."""
        return QSize(400, 3 * hy.Y_STEP + 2 * MARGIN)

    def set_nodes(self, nodes, selected, names) -> None:
        """New nodes, the selected id and each node's box name (for the tooltip), and
        mark the widget dirty (G-034)."""
        self.nodes = tuple(nodes)
        self.selected = selected
        self.names = dict(names)
        self.update()

    def _extent(self) -> tuple:
        if not self.nodes:
            return (1.0, 1.0)
        return (max(n.x for n in self.nodes) + OVAL_W, max(n.y for n in self.nodes) + OVAL_H)

    def _scale(self) -> float:
        width, height = self._extent()
        room_w = max(10.0, self.width() - 2 * MARGIN - 120)
        room_h = max(10.0, self.height() - 2 * MARGIN)
        return min(1.0, room_w / width, room_h / height)

    def centre_of(self, node: hy.TreeNode) -> QPointF:
        """A node's centre on the widget (G-034)."""
        scale = self._scale()
        return QPointF(MARGIN + (node.x + OVAL_W / 2) * scale,
                       MARGIN + (node.y + OVAL_H / 2) * scale)

    def rect_of(self, node: hy.TreeNode) -> QRectF:
        """A node's rectangle on the widget: an oval's or a circle's (G-034)."""
        scale = self._scale()
        c = self.centre_of(node)
        if node.kind == hy.FORM:
            return QRectF(c.x() - OVAL_W * scale / 2, c.y() - OVAL_H * scale / 2,
                          OVAL_W * scale, OVAL_H * scale)
        r = (SHARED_R if node.kind == hy.SHARED else LEAF_R) * scale
        return QRectF(c.x() - r, c.y() - r, 2 * r, 2 * r)

    def node_at(self, pos) -> hy.TreeNode | None:
        """The node under a point, or None (G-034)."""
        for node in reversed(self.nodes):
            if self.rect_of(node).contains(pos):
                return node
        return None

    def mousePressEvent(self, event):
        """A left click emits the node's id, or None on empty space (G-034)."""
        if event.button() == Qt.MouseButton.LeftButton:
            node = self.node_at(event.position())
            self.clicked.emit(node.id if node is not None else None)

    def event(self, e):
        """The tooltip is the node's structure, what it gives and its box (G-034)."""
        if e.type() == e.Type.ToolTip:
            node = self.node_at(e.pos())
            self.setToolTip(hy.node_tooltip(node, self.names.get(node.id))
                            if node is not None else "")
        return super().event(e)

    def paintEvent(self, event):
        """Paint the edges, then the nodes: ovals for forms, circles for numbers, a small
        empty circle for a shared place joined by a dashed line to the structure it
        shares [M86 p. 17], the selected node tinted, the first hits beside the top
        oval [M86 p. 16] (G-034)."""
        self.paints += 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), QColor(BACKGROUND))
        if not self.nodes:
            p.setPen(QColor(MUTED))
            p.setFont(self.font_)
            p.drawText(self.rect().adjusted(MARGIN, 0, -MARGIN, 0),
                       Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                       "no hypothesis yet")
            p.end()
            return
        by_id = {node.id: node for node in self.nodes}
        p.setPen(QPen(QColor(LEAF_BORDER), 1.2))
        for node in self.nodes:
            if node.parent is not None:
                p.drawLine(self.centre_of(by_id[node.parent]), self.centre_of(node))
        p.setPen(QPen(QColor(SHARED_BORDER), 1.2, Qt.PenStyle.DashLine))
        for node in self.nodes:
            if node.shares is not None and node.shares in by_id:
                p.drawLine(self.centre_of(node), self.centre_of(by_id[node.shares]))
        scale = self._scale()
        for node in self.nodes:
            rect = self.rect_of(node)
            selected = node.id == self.selected
            if node.kind == hy.FORM:
                p.setPen(QPen(QColor(SELECTED_BORDER if selected else OVAL_BORDER), 1.4))
                p.setBrush(QColor(SELECTED_FILL if selected else OVAL_FILL))
                p.drawEllipse(rect)
                p.setFont(self.bold)
                p.setPen(QColor(TEXT))
                p.drawText(rect.toRect(), Qt.AlignmentFlag.AlignCenter,
                           self.metrics.elidedText(node.label, Qt.TextElideMode.ElideRight,
                                                   int(rect.width()) - 4))
            else:
                border = SHARED_BORDER if node.kind == hy.SHARED else LEAF_BORDER
                p.setPen(QPen(QColor(SELECTED_BORDER if selected else border), 1.2))
                p.setBrush(QColor(SELECTED_FILL if selected else LEAF_FILL))
                p.drawEllipse(rect)
                if node.kind != hy.SHARED:
                    p.setFont(self.font_)
                    p.setPen(QColor(TEXT))
                    p.drawText(rect.toRect(), Qt.AlignmentFlag.AlignCenter, node.label)
            if node.hits and node.parent is None:
                p.setFont(self.font_)
                p.setPen(QColor(MUTED))
                x = int(rect.right() + 10 * scale + 6)
                p.drawText(QRect(x, int(rect.top()), max(10, self.width() - x - MARGIN),
                                 int(rect.height())),
                           Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                           self.metrics.elidedText("--> " + node.hits,
                                                   Qt.TextElideMode.ElideRight,
                                                   max(10, self.width() - x - MARGIN)))
        p.end()


class ValidityBar(QWidget):
    """The validity as a bar with a mark at the guess threshold [M86 p. 81] (G-035):
    `set_values(validity, threshold)`."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.validity = 0
        self.threshold = 1
        self.paints = 0
        self.setFixedHeight(14)
        self.setMinimumWidth(80)
        self.setToolTip("validity: grows by one at each correctly predicted term, 0 at a "
                        "refutation [M86 pp. 81, 168]; the mark is the guess threshold")

    def set_values(self, validity: int, threshold: int) -> None:
        """New values; marks the bar dirty (G-035)."""
        self.validity, self.threshold = validity, threshold
        self.update()

    def paintEvent(self, event):
        """The track, the fill by `validity_fraction`, the mark by `threshold_fraction`
        (G-035)."""
        self.paints += 1
        p = QPainter(self)
        track = self.rect().adjusted(0, 2, -1, -2)
        p.fillRect(track, QColor(BAR_TRACK))
        fill = round(track.width() * hy.validity_fraction(self.validity, self.threshold))
        if fill:
            p.fillRect(QRect(track.x(), track.y(), fill, track.height()), QColor(BAR_FILL))
        mark = track.x() + round(track.width() * hy.threshold_fraction(self.validity,
                                                                         self.threshold))
        p.setPen(QPen(QColor(BAR_MARK), 2))
        p.drawLine(mark, 0, mark, self.height())
        p.end()


class PredictionStrip(QWidget):
    """The predictions table: the predicted terms above the terms entered, one column
    per position, matches green, the refuting term red with the prediction the event
    carried, the terms beyond those entered dashed, the positions a selected oval gives
    outlined (PANES.md P4; G-035). `set_cells(cells, highlighted)`."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.cells = ()
        self.highlighted = ()
        self.paints = 0
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(9)
        self.metrics = QFontMetrics(self.font_)
        self.setFixedHeight(2 * CELL_H + ROW_GAP + 2 * MARGIN)
        self.setAutoFillBackground(True)

    def set_cells(self, cells, highlighted) -> None:
        """New cells and highlighted positions; marks the strip dirty (G-035)."""
        self.cells = tuple(cells)
        self.highlighted = tuple(highlighted)
        self.update()

    def _label_width(self) -> int:
        return self.metrics.horizontalAdvance("predicted") + 2 * PAD

    def _scale(self) -> float:
        room = max(10.0, self.width() - 2 * MARGIN - self._label_width())
        return min(1.0, room / (len(self.cells) * CELL_W)) if self.cells else 1.0

    def rect_of(self, position: int, row: int) -> QRectF:
        """The cell of a position (from 1) on a row (0 predicted, 1 entered) (G-035)."""
        scale = self._scale()
        x = MARGIN + self._label_width() + (position - 1) * CELL_W * scale
        return QRectF(x, MARGIN + row * (CELL_H + ROW_GAP), CELL_W * scale - 2, CELL_H)

    def cell_at(self, pos) -> hy.PredictionCell | None:
        """The cell under a point, or None (G-035)."""
        for cell in self.cells:
            if self.rect_of(cell.position, 0).contains(pos) or \
                    self.rect_of(cell.position, 1).contains(pos):
                return cell
        return None

    def event(self, e):
        """The tooltip is the cell's position, prediction, term and state (G-035)."""
        if e.type() == e.Type.ToolTip:
            cell = self.cell_at(e.pos())
            self.setToolTip(f"position {cell.position}: predicted "
                            f"{cell.predicted if cell.predicted is not None else '-'}, "
                            f"entered {cell.term if cell.term is not None else '-'}: "
                            f"{cell.state}" if cell is not None else "")
        return super().event(e)

    def paintEvent(self, event):
        """Paint the two rows (G-035)."""
        self.paints += 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), QColor(BACKGROUND))
        p.setFont(self.font_)
        p.setPen(QColor(MUTED))
        flags = Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft
        p.drawText(QRect(MARGIN, MARGIN, self._label_width(), CELL_H), flags, "predicted")
        p.drawText(QRect(MARGIN, MARGIN + CELL_H + ROW_GAP, self._label_width(), CELL_H),
                   flags, "entered")
        if not self.cells:
            p.drawText(QRect(MARGIN + self._label_width(), MARGIN, 300, CELL_H), flags,
                       "no predictions")
            p.end()
            return
        for cell in self.cells:
            fill, border = {
                "agrees": (AGREES_FILL, AGREES_BORDER), "refuting": (REFUTING_FILL, REFUTING_BORDER),
                "differs": (DIFFERS_FILL, DIFFERS_BORDER),
            }.get(cell.state, (BACKGROUND, BEYOND_BORDER))
            for row, value in ((0, cell.predicted), (1, cell.term)):
                rect = self.rect_of(cell.position, row)
                pen = QPen(QColor(border), 1.0)
                if cell.state == "beyond" and row == 0 or value is None:
                    pen.setStyle(Qt.PenStyle.DashLine)
                if cell.position in self.highlighted:
                    pen = QPen(QColor(SELECTED_BORDER), 2.0)
                p.setPen(pen)
                p.setBrush(QColor(fill if value is not None else BACKGROUND))
                p.drawRoundedRect(rect, 4, 4)
                if value is not None:
                    p.setPen(QColor(TEXT if cell.state != "beyond" else MUTED))
                    p.drawText(rect.toRect(), Qt.AlignmentFlag.AlignCenter, str(value))
        p.end()


class HypothesisList(LineList):
    """The lines of `pane_lines(model)`, painted straight from the model (Numbo GUI,
    `log_view.py`'s `LogList`; G-028, G-035): headings bold on a tinted band, candidates
    green or red by their postdiction, records indented with their fields in the
    tooltip, provisional names grey."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.lines = ()

    def set_lines(self, lines) -> None:
        """New lines; the scroll range follows and the viewport is marked dirty (G-028)."""
        self.lines = tuple(lines)
        self.update_rows()
        self.viewport().update()

    def row_count(self) -> int:
        """How many lines (G-028)."""
        return len(self.lines)

    def line(self, row: int) -> hy.HLine:
        """The row's line (G-035)."""
        return self.lines[row]

    def _paint_line(self, p, line, box):
        h = box.height()
        if line.kind == "head":
            p.fillRect(box, QColor(HEAD_TINT))
        color = QColor(GREY if line.grey else MUTED if line.kind == "note"
                       else OVAL_BORDER if line.kind == "head" else TEXT)
        if line.good and not line.grey:
            color = QColor(GOOD)
        elif line.bad:
            color = QColor(BAD)
        x = PAD + (INDENT if line.kind in ("box", "pstruc", "candidate", "private", "note")
                   else 0)
        p.setFont(self.bold if line.kind == "head" else self.font_)
        p.setPen(color)
        room = max(10, box.width() - x - PAD)
        p.drawText(QRect(x, box.y(), room, h),
                   Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                   self.metrics.elidedText(line.text, Qt.TextElideMode.ElideRight, room))


class HypothesisView(QWidget):
    """The hypothesis dock's view (see the module docstring; G-033, G-034, G-035).
    `history` is the window's `EventLog`, read at `position`. Signals:
    `node_clicked(node_id)` after a click on the oval tree."""

    pane_id = "P4"
    title = "Hypothesis"
    pages = PAGES
    node_clicked = Signal(object)

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history
        self.model = hy.HypothesisModel()
        self.position = -1
        self.events = 0
        self.form_label = QLabel("nil")
        self.form_label.setMinimumWidth(1)       # clipped when narrow (G-058)
        font = QFont("monospace")
        font.setStyleHint(QFont.StyleHint.Monospace)
        font.setPointSizeF(13)
        font.setBold(True)
        self.form_label.setFont(font)
        self.form_label.setToolTip("The hypothesis's form, as show-hypothesis prints it "
                                   "[M86 pp. 81, 166]")
        self.form_label.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self.source_label = QLabel("")
        self.source_label.setStyleSheet(f"color: {MUTED};")
        self.validity_label = QLabel("validity 0")
        self.validity_label.setToolTip("[M86 pp. 81, 168]")
        self.validity_bar = ValidityBar()
        self.tree = OvalTree()
        self.tree.clicked.connect(self._tree_clicked)
        self.strip = PredictionStrip()
        self.box_button = QToolButton()
        self.box_button.setText("Box tree")
        self.box_button.setCheckable(True)
        self.box_button.setChecked(False)
        self.box_button.setToolTip("Show the box tree's records, as Ch. 2 Figure 3 "
                                   "[M86 p. 83]; hidden by default")
        self.box_button.toggled.connect(self.set_show_box)
        self.private_edit = QLineEdit()
        self.private_edit.setPlaceholderText("a form for a private box (the hypothesis's by default)")
        self.private_edit.setToolTip("build-box: a form to hit on a box of your own; the run "
                                     "is never touched [M86 p. 161]")
        self.private_edit.returnPressed.connect(self.private_hit)
        self.hit_button = QPushButton("Hit")
        self.hit_button.setToolTip("show-box: the private box's next value [M86 p. 161]")
        self.hit_button.clicked.connect(self.private_hit)
        self.reset_button = QPushButton("Reset")
        self.reset_button.setToolTip("Reset the private box to the start of its pattern "
                                     "[M86 p. 85]")
        self.reset_button.clicked.connect(self.private_reset)
        self.list = HypothesisList()
        head = QHBoxLayout()
        head.setContentsMargins(MARGIN, 2, MARGIN, 0)
        head.addWidget(QLabel("hypothesis:"))
        head.addWidget(self.form_label, 1)
        head.addWidget(self.source_label)
        validity = QHBoxLayout()
        validity.setContentsMargins(MARGIN, 0, MARGIN, 0)
        validity.addWidget(self.validity_label)
        validity.addWidget(self.validity_bar, 1)
        tools = QHBoxLayout()
        tools.setContentsMargins(MARGIN, 0, MARGIN, 0)
        tools.addWidget(self.box_button)
        tools.addWidget(self.private_edit, 1)
        tools.addWidget(self.hit_button)
        tools.addWidget(self.reset_button)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addLayout(head)
        layout.addLayout(validity)
        layout.addWidget(self.tree)
        layout.addWidget(self.strip)
        layout.addLayout(tools)
        layout.addWidget(self.list, 1)
        self.redraw()

    def sizeHint(self):
        """Room for the form, the tree, the table and a few lines (G-011)."""
        return QSize(420, 420)

    @property
    def paints(self) -> int:
        """The oval tree's paint count (G-011)."""
        return self.tree.paints

    def form(self) -> str:
        """The form shown [M86 p. 166]."""
        return self.form_label.text()

    def lines(self) -> tuple:
        """The list's lines as last drawn (G-035)."""
        return self.list.lines

    def cells(self) -> tuple:
        """The predictions table's cells as last drawn (G-035)."""
        return self.strip.cells

    def nodes(self) -> tuple:
        """The oval tree's nodes as last drawn (G-034)."""
        return self.tree.nodes

    # -- the view protocol (G-011, G-016) ----------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and take the snapshot in force at `position` (G-033); a
        `start` begins a new run (G-011)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event)
        self.model.update(self.history.state_at(self.position))

    def redraw(self, now=False) -> None:
        """Rebuild the labels, the tree, the table and the lines and mark the widgets
        dirty; with `now`, paint at once (Numbo GUI, `paint.py`; G-035)."""
        model = self.model
        self._set_text(self.form_label, model.form or "nil")
        self._set_text(self.source_label, hy.source_text(model.source, model.kind_))
        self._set_text(self.validity_label,
                       f"validity {model.validity}  (guess at {model.threshold})"
                       + ("  refuted" if model.refuted else ""))
        self.validity_bar.set_values(model.validity, model.threshold)
        nodes = model.tree()
        self.tree.set_nodes(nodes, model.selected,
                            {node.id: model.box_name(node) for node in nodes})
        self.strip.set_cells(model.predictions(), model.highlighted())
        self.list.set_lines(hy.pane_lines(model))
        if model.form and not self.private_edit.text():
            self.private_edit.setPlaceholderText(model.form)
        if now:
            paint.paint_all()

    @staticmethod
    def _set_text(label, text):
        if label.text() != text:
            label.setText(text)

    # -- interactions (PANES.md P4) ---------------------------------------------------------

    def select_node(self, node_id) -> tuple:
        """Select an oval (a second click clears; None clears; the top oval's id is the
        empty tuple), paint now and emit `node_clicked`; returns the positions it gives
        (G-034)."""
        positions = self.model.select_node(node_id)
        self.redraw(now=True)
        self.node_clicked.emit(node_id)
        return positions

    def set_show_box(self, on: bool) -> None:
        """Show or hide the box tree's records (hidden by default; PANES.md P4; G-035)."""
        self.model.show_box = bool(on)
        if self.box_button.isChecked() != bool(on):
            self.box_button.setChecked(bool(on))
        self.redraw(now=True)

    def private_hit(self) -> str:
        """Hit the private box, built from the box's text or the form in force
        [M86 p. 161] (G-035); a bad text is refused with a status in the edit's
        tooltip and an empty string returned."""
        text = self.private_edit.text().strip()
        model = self.model
        try:
            if model.private is None or (text and text != model.private.text
                                         and text != self.private_edit.property("built")):
                model.open_private(text or None)
                self.private_edit.setProperty("built", text)
            value = model.private_hit()
        except (ValueError, TypeError) as error:
            self.private_edit.setToolTip(f"not a form: {error}")
            self.redraw(now=True)
            return ""
        self.redraw(now=True)
        return value

    def private_reset(self) -> None:
        """Reset the private box [M86 p. 85] (G-035)."""
        self.model.private_reset()
        self.redraw(now=True)

    def _tree_clicked(self, node_id):
        self.select_node(node_id)

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image
