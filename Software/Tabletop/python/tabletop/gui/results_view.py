"""P8 Results: a batch of runs of the problem, then French's bar graph and table beside
ours (PANES.md P8; G-052; `models/results.py`).

French prints, for each configuration, a bar graph of the percentage of runs giving each
answer, the percentage above each black bar and the answer's label under it, the
configuration's title under the graph, and a table headed "Object Eliza touches",
"Average structure", "No. of codelets run" [F95 p. 118 (img)]. The pane runs a batch (fifty
runs by default [F95 p. 114]) on the model's worker, with a progress bar and Cancel, and
draws the same picture as the runs come in: French's bars in black and ours in grey beside
them, his page in the title, and his three headings each with his column and ours; the
answers he prints no bar for are grouped as `other`, as RESULTS.md does. A batch is refused
while a run is live, and a live run while a batch runs (the engine lock, G-052). The
`BatchLine` along the top, saying the batch's state and the events of the live run, is the
one widget repainted per event (G-011).
"""

from PySide6.QtCore import QRect, QRectF, QSize, Qt, Signal
from PySide6.QtGui import QBrush, QColor, QFont, QPainter, QPen
from PySide6.QtWidgets import (QAbstractItemView, QComboBox, QHBoxLayout, QHeaderView, QLabel,
                               QProgressBar, QPushButton, QScrollArea, QSizePolicy, QSpinBox,
                               QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

from tabletop import events as ev
from tabletop.gui import paint
from tabletop.gui.models import results as rm
from tabletop.gui.models import run_controller as rc

__all__ = ["BatchLine", "BarGraphWidget", "ResultsView", "FRENCH", "OURS"]

INK, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
LIVE = QColor("#1b7f3b")
FRENCH = QColor("#000000")      # French's bars are black [F95 p. 118 (img)]
OURS = QColor("#9a9a9a")        # ours beside his, grey (the GUI's convention)
AXIS = QColor("#8a8a8a")
PAGES = "the chapter 5 layout [F95 pp. 115-139]"
NOTE = ("French's bars black with his page, ours grey beside them; \"other\" groups the "
        "answers he prints no bar for")


class BatchLine(QWidget):
    """The thin strip along the top of the pane: the batch's state and the live run's
    events, repainted once per event (G-011, G-052)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.batch = "no batch yet"
        self.state = "idle"
        self.events = 0
        self.paints = 0
        self.setMinimumHeight(16)
        self.setMaximumHeight(16)
        self.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)

    def text(self) -> str:
        """`no batch yet · idle` or `running: 12 of 50 runs · paused: 23 events shown`
        (G-052)."""
        if self.state == "idle" and self.events == 0:
            run = "no run yet"
        else:
            word = "events shown" if self.state in ("running", "paused") else "events"
            run = f"{self.state}: {self.events} {word}"
        return f"{self.batch} · {run}"

    def paintEvent(self, event):
        """Paint the line; `paints` counts the calls for the window's probe (G-011)."""
        self.paints += 1
        painter = QPainter(self)
        font = QFont(painter.font())
        font.setPointSizeF(8.0)
        painter.setFont(font)
        painter.setPen(LIVE if self.batch.startswith("running") else MUTED)
        painter.drawText(QRect(2, 0, self.width() - 4, self.height()),
                         Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                         painter.fontMetrics().elidedText(self.text(), Qt.TextElideMode.ElideRight,
                                                          self.width() - 4))
        painter.end()


class BarGraphWidget(QWidget):
    """French's bar graph [F95 p. 118 (img)] for the rows of a batch: a pair of bars per
    answer, his black and ours grey, the percentage above each, the label under, the
    axis in percent of runs and the title under the graph; hover a bar for its row's
    text (G-052)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.rows = ()
        self.title = ""
        self.drawn = None
        self.setMinimumSize(120, 90)
        self.setMouseTracking(True)
        self.setToolTip(NOTE + " [F95 p. 118 (img)]")

    def set_rows(self, rows, title) -> None:
        """The rows to draw and the title under the graph (G-052)."""
        self.rows, self.title = tuple(rows), title
        self.update()

    def layout_now(self) -> rm.BarsLayout:
        """The layout for the widget's size (G-052)."""
        return rm.bars_layout(self.rows, self.width(), self.height(), self.title)

    def bar_at(self, x, y):
        """The bar under a point, or None (G-052)."""
        layout = self.drawn or self.layout_now()
        for bar in layout.bars:
            if bar.x <= x <= bar.x + bar.width and layout.box[1] <= y <= layout.box[1] + layout.box[3]:
                return bar
        return None

    def row_of(self, label):
        """The row of a label, or None (G-052)."""
        for row in self.rows:
            if row.label == label:
                return row
        return None

    def mouseMoveEvent(self, event):
        """The hover text of the bar under the mouse (G-052)."""
        pos = event.position()
        bar = self.bar_at(pos.x(), pos.y())
        row = self.row_of(bar.label) if bar is not None else None
        self.setToolTip(rm.row_text(row) if row is not None else NOTE + " [F95 p. 118 (img)]")

    def paintEvent(self, event):
        """Paint the layout (G-052)."""
        layout = self.layout_now()
        self.drawn = layout
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        font = QFont(painter.font())
        font.setPointSizeF(8.0)
        painter.setFont(font)
        bx, by, bw, bh = layout.box
        painter.setPen(QPen(AXIS, 1))
        painter.drawLine(int(bx), int(by), int(bx), int(by + bh))
        painter.drawLine(int(bx), int(by + bh), int(bx + bw), int(by + bh))
        painter.setPen(MUTED)
        for pct, y in layout.ticks:
            painter.drawText(QRectF(0, y - 7, bx - 4, 14),
                             Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter, str(pct))
        painter.drawText(QRectF(bx + 2, 1, bw, 14), Qt.AlignmentFlag.AlignLeft, "% of total runs")
        for bar in layout.bars:
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(FRENCH if bar.who == "French" else OURS))
            if bar.value is not None and bar.height > 0:
                painter.drawRect(QRectF(bar.x, bar.y, bar.width, bar.height))
            painter.setPen(INK if bar.who == "French" else MUTED)
            painter.drawText(QRectF(bar.x - 10, bar.y - 14, bar.width + 20, 13),
                             Qt.AlignmentFlag.AlignHCenter | Qt.AlignmentFlag.AlignBottom, bar.text)
        painter.setPen(INK)
        for label, x, y in layout.labels:
            painter.drawText(QRectF(x - 40, y, 80, 13), Qt.AlignmentFlag.AlignHCenter, label)
        painter.setPen(MUTED)
        painter.drawText(QRectF(bx, by + bh + 17, bw, 14), Qt.AlignmentFlag.AlignHCenter,
                         painter.fontMetrics().elidedText(layout.title,
                                                          Qt.TextElideMode.ElideRight, int(bw)))
        painter.end()


class ResultsView(QWidget):
    """The results pane (PANES.md P8; G-052; G-011's view protocol): the batch line, the
    count, Run batch, Cancel and the progress bar, French's set when he prints several,
    the graph, the table and the footer. Signals: `batch_requested(count)` when Run batch
    is pressed (the window starts the batch with the next run's inputs); `batch_changed()`
    after every run of the batch and at its end."""

    batch_requested = Signal(int)
    batch_changed = Signal()
    _result = Signal(int, object)
    _done = Signal(int, bool, object)

    def __init__(self, title="Results", pane_id="P8", pages=PAGES, parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.events = 0
        self.error = ""
        self.model = rm.BatchModel()
        self.generation = 0
        self.worker = rm.BatchWorker(on_result=self._on_result, on_done=self._on_done)
        self._result.connect(self._add)
        self._done.connect(self._finish)
        self._build()
        self.setMinimumSize(200, 70)
        self.setToolTip(f"{pane_id} {title}: {pages}; {NOTE}")
        self.refresh()

    def _build(self):
        self.line = BatchLine(self)
        self.count_box = QSpinBox(self)
        self.count_box.setRange(1, rm.MAX_COUNT)
        self.count_box.setValue(rm.DEFAULT_COUNT)
        self.count_box.setToolTip("runs in the batch: fifty, French's count [F95 p. 114]")
        self.run_button = QPushButton("Run batch", self)
        self.run_button.clicked.connect(lambda: self.batch_requested.emit(self.count_box.value()))
        self.cancel_button = QPushButton("Cancel", self)
        self.cancel_button.clicked.connect(self.cancel_batch)
        self.progress = QProgressBar(self)
        self.progress.setTextVisible(True)
        self.progress.setMinimumWidth(60)
        self.set_combo = QComboBox(self)
        self.set_combo.setToolTip("French's result sets for the problem: the book's, then FCCA's")
        self.set_combo.currentIndexChanged.connect(self._set_chosen)
        top = QHBoxLayout()
        top.setContentsMargins(0, 0, 0, 0)
        top.addWidget(QLabel("Runs:", self))
        top.addWidget(self.count_box)
        top.addWidget(self.run_button)
        top.addWidget(self.cancel_button)
        top.addWidget(self.progress, 1)
        top.addWidget(self.set_combo)
        self.next_label = QLabel("", self)
        self.next_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        self.status_label = QLabel("", self)
        self.status_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        self.graph = BarGraphWidget(self)
        self.table = QTableWidget(0, len(rm.HEADINGS), self)
        self.table.setHorizontalHeaderLabels(list(rm.HEADINGS))
        self.table.verticalHeader().setVisible(False)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.table.setSelectionMode(QAbstractItemView.SelectionMode.NoSelection)
        self.table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        self.table.setMinimumHeight(40)
        self.footer_label = QLabel("", self)
        self.footer_label.setWordWrap(True)
        self.footer_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        self.note_label = QLabel(NOTE + " [F95 p. 118 (img)]", self)
        self.note_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        self.note_label.setStyleSheet("color: #6b6b6b")
        self._lay_out(top)

    def _lay_out(self, top) -> None:
        """The body under the line, in a scroll area (the controls' pattern, G-014,
        G-044): at the bottom area's height the body's minimum does not fit, and a layout
        squeezed under its minimum lays its children over each other (G-058)."""
        body = QWidget(self)
        layout = QVBoxLayout(body)
        layout.setContentsMargins(2, 0, 2, 2)
        layout.setSpacing(2)
        layout.addLayout(top)
        layout.addWidget(self.next_label)
        layout.addWidget(self.status_label)
        layout.addWidget(self.graph, 3)
        layout.addWidget(self.table, 2)
        layout.addWidget(self.footer_label)
        layout.addWidget(self.note_label)
        self.scroll = QScrollArea(self)
        self.scroll.setWidget(body)
        self.scroll.setWidgetResizable(True)
        self.scroll.setFrameShape(QScrollArea.Shape.NoFrame)
        self.scroll.setMinimumSize(40, 24)
        outer = QVBoxLayout(self)
        outer.setContentsMargins(0, 0, 0, 0)
        outer.setSpacing(0)
        outer.addWidget(self.line)
        outer.addWidget(self.scroll)

    # -- the batch ------------------------------------------------------------------------

    @property
    def busy(self) -> bool:
        """Whether a batch is running (G-052)."""
        return self.worker.is_alive()

    def start_batch(self, problem, config, seed, count=None) -> bool:
        """Run `count` runs (the count box's when None) of `problem` under `config` from
        the batch seed `seed` on the worker (PANES.md P8; G-052); False, with the reason
        in `error`, when the engine is busy or the inputs are bad."""
        count = self.count_box.value() if count is None else int(count)
        self.error = ""
        try:
            self.model.start(problem, config, seed, count)
            self.generation += 1
            self.worker.start(problem, config, seed, count)
        except (rc.EngineBusy, ValueError, TypeError) as error:
            self.error = str(error)
            if self.model.state == "running":
                self.model.finish(error=error)
            self.refresh()
            return False
        self.count_box.setValue(count)
        self.refresh()
        return True

    def cancel_batch(self) -> None:
        """Stop the batch after the run in progress (G-052)."""
        if self.busy:
            self.worker.cancel()

    def shutdown(self) -> None:
        """Cancel and join the worker (the window closes; G-052)."""
        self.worker.stop()

    def _on_result(self, result):
        self._result.emit(self.generation, result)

    def _on_done(self, cancelled, error):
        self._done.emit(self.generation, cancelled, error)

    def _add(self, generation, result):
        if generation != self.generation:
            return
        self.model.add(result)
        self.refresh()
        self.batch_changed.emit()

    def _finish(self, generation, cancelled, error):
        if generation != self.generation:
            return
        self.model.finish(cancelled, error)
        self.refresh()
        self.batch_changed.emit()

    def _set_chosen(self, index):
        if index >= 0 and index != self.model.set_index:
            self.model.choose_set(index)
            self.refresh()

    # -- what the window tells the pane --------------------------------------------------

    def show_next_run(self, problem, profile, seed) -> None:
        """The next run's inputs, which the next batch takes (G-052)."""
        self.next_label.setText(f"next batch: {problem} under {profile}, batch seed {seed}")

    def set_state(self, state: str) -> None:
        """The run controller's state: Run batch is off while a run is live (G-052)."""
        self.line.state = state
        self._enable()

    def _enable(self):
        live = self.line.state in ("running", "paused")
        self.run_button.setEnabled(not live and not self.busy)
        self.cancel_button.setEnabled(self.busy)
        self.count_box.setEnabled(not self.busy)

    # -- the picture ----------------------------------------------------------------------

    def refresh(self) -> None:
        """Take the model and show it: the line, the progress, the labels, the graph,
        the table and the footer (G-052)."""
        m = self.model
        self.line.batch = m.status()
        self.progress.setRange(0, max(1, m.count))
        self.progress.setValue(m.done if m.state != "idle" else 0)
        self.progress.setFormat("%v of %m runs" if m.state != "idle" else "")
        self.status_label.setText(self.error or m.status())
        self._show_sets()
        rows = m.rows()
        self.graph.set_rows(rows, m.french_title() or m.title())
        self._fill_table(m.cells(), rows)
        self.footer_label.setText(m.footer())
        self._enable()
        self.line.update()

    def _show_sets(self):
        sets = self.model.sets()
        self.set_combo.blockSignals(True)
        try:
            self.set_combo.clear()
            for rs in sets:
                self.set_combo.addItem(rm.set_title(rs))
            if sets:
                self.set_combo.setCurrentIndex(min(self.model.set_index, len(sets) - 1))
        finally:
            self.set_combo.blockSignals(False)
        self.set_combo.setVisible(len(sets) > 1)

    def _fill_table(self, cells, rows):
        t = self.table
        t.setRowCount(len(cells))
        for i, (row, texts) in enumerate(zip(rows, cells)):
            for j, text in enumerate(texts):
                item = QTableWidgetItem(text)
                if j > 0:
                    item.setTextAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
                item.setToolTip(rm.row_text(row))
                t.setItem(i, j, item)

    def cell_texts(self) -> tuple:
        """The table as the widget shows it, row by row (G-052)."""
        return tuple(tuple(self.table.item(i, j).text() for j in range(self.table.columnCount()))
                     for i in range(self.table.rowCount()))

    # -- the view protocol (G-011) --------------------------------------------------------

    @property
    def paints(self) -> int:
        """The batch line's paints, what the pane repaints per event (G-011)."""
        return self.line.paints

    def on_event(self, event) -> None:
        """Count the live run's events for the line (G-011)."""
        if isinstance(event, ev.RunStarted):
            self.events = 0
        self.events += 1
        self.line.events = self.events

    def redraw(self, now=False) -> None:
        """Mark the line dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.line.update()
        if now:
            paint.paint_all()

    def sizeHint(self):
        """A modest size: the docks are sized by the window (G-011)."""
        return QSize(520, 300)

    def render_png(self, path) -> None:
        """The pane as a PNG, for the screenshots (G-052)."""
        self.grab().toImage().save(str(path), "PNG")
