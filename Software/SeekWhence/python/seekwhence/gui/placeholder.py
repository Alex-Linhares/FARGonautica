"""Placeholders for the panes that later items build (G-011).

Item 02 lays the window out as PANES.md section 3 before any pane exists, so every dock
and the two halves of the central splitter hold a `PanePlaceholder`: a view that counts
the events it is given and paints its pane's name and the pages behind it. It has the
shape every real view has (Numbo GUI, `main_window.py`'s `add_view`): `on_event`
on the GUI thread, `redraw(now=False)` that only marks it dirty, a paint counter the
tests read (Numbo GUI, `tree_view.py`'s `paints`). The panes that have no figure in the
sources, the platoplasm network and the taskrack drawing, say "GUI design, not
Meredith's" from the start (ANSWERS.md 4). Item 03 replaced the sequence pane
(`sequence_view.py`, which also took over the guess panel) and the stats dock
(`stats_view.py`); item 04 the console (`console_view.py`); item 05 the cytoplasm
(`cytoplasm_view.py`); item 07 the taskrack (`taskrack_view.py`); item 08 the
socratoplasm (`socratoplasm_view.py`); item 09 the hypothesis pane (`hypothesis_view.py`);
item 10 the platoplasm (`platoplasm_view.py`); item 11 the reformulation pane
(`reformulation_view.py`); item 12 the event log (`log_view.py`); item 14 the controls
(`controls.py`); item 15 the configuration pane (`config_view.py`). No dock holds one
now; the class stays for a dock a later item may add.
"""

from PySide6.QtCore import QSize, Qt
from PySide6.QtGui import QColor, QPainter
from PySide6.QtWidgets import QWidget

from seekwhence.gui import paint

__all__ = ["PanePlaceholder"]


class PanePlaceholder(QWidget):
    """A pane not built yet: paints its title, its pane id, the pages behind it and how
    many events it has seen (G-011; Numbo GUI's view protocol)."""

    def __init__(self, title, pane_id, pages, parent=None):
        super().__init__(parent)
        self.title = title
        self.pane_id = pane_id
        self.pages = pages
        self.paints = 0
        self.events = 0
        self.last_kind = ""
        self.setMinimumSize(120, 60)
        self.setAutoFillBackground(True)

    def sizeHint(self):
        """A modest size: the docks are sized by the window (G-011)."""
        return QSize(300, 160)

    def on_event(self, event) -> None:
        """Count the event; a `start` event begins a new run (G-011)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.last_kind = event.kind

    def redraw(self, now=False) -> None:
        """Mark the view dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.update()
        if now:
            paint.paint_all()

    def paintEvent(self, event):
        """Paint the title, the pages and the count; `paints` counts the calls, for the
        tests (Numbo GUI, `tree_view.py`'s `paints`; G-011)."""
        self.paints += 1
        painter = QPainter(self)
        rect = self.rect().adjusted(8, 6, -8, -6)
        painter.setPen(QColor(40, 40, 40))
        font = painter.font()
        font.setBold(True)
        painter.setFont(font)
        painter.drawText(rect, Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft,
                         f"{self.pane_id}  {self.title}")
        font.setBold(False)
        painter.setFont(font)
        painter.setPen(QColor(90, 90, 90))
        body = rect.adjusted(0, 22, 0, 0)
        painter.drawText(body, Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft
                         | Qt.TextFlag.TextWordWrap,
                         f"{self.pages}\n\nnot built yet (a later item)\n"
                         f"events seen: {self.events}"
                         + (f", last: {self.last_kind}" if self.last_kind else ""))
        painter.end()
