"""Placeholders for the panes that later items build (G-011; Seek-Whence GUI,
`placeholder.py`).

Item 02 lays the window out as PANES.md section 3 before any pane exists, so the central
widget and every dock hold a `PanePlaceholder`: a view that counts the events it is given
and paints its pane's id, its name and the pages behind it. It has the shape every real
view has (Numbo GUI, `main_window.py`'s `add_view`): `on_event` on the GUI thread,
`redraw(now=False)` that only marks it dirty, and a paint counter the tests read (Numbo
GUI, `tree_view.py`'s `paints`). The panes that have no figure in the sources carry
"GUI design, not French's" in their pages text from the start (ANSWERS.md 4). Each real
view replaces its placeholder in its own item and keeps the protocol.
"""

from PySide6.QtCore import QSize, Qt
from PySide6.QtGui import QColor, QPainter
from PySide6.QtWidgets import QWidget

from tabletop import events as ev
from tabletop.gui import paint

__all__ = ["PanePlaceholder"]

# The smallest a placeholder may be: the window must fit 1280 x 800 with every dock
# shown (G-014). The thermometer's is narrower (`narrow=True`), as on French's screen
# [F95 p. 96 (img)].
MIN_SIZE = (120, 60)
NARROW_MIN_SIZE = (56, 60)


class PanePlaceholder(QWidget):
    """A pane not built yet: paints its pane id, its title, the pages behind it and how
    many events it has seen (G-011; Numbo GUI's view protocol)."""

    def __init__(self, title, pane_id, pages, narrow=False, parent=None):
        super().__init__(parent)
        self.title = title
        self.pane_id = pane_id
        self.pages = pages
        self.paints = 0
        self.events = 0
        self.last_name = ""
        self.setMinimumSize(*(NARROW_MIN_SIZE if narrow else MIN_SIZE))
        self.setAutoFillBackground(True)
        self.setToolTip(f"{pane_id} {title}: {pages}")

    def sizeHint(self):
        """A modest size: the docks are sized by the window (G-011)."""
        return QSize(300, 160)

    def on_event(self, event) -> None:
        """Count the event; a `RunStarted` begins a new run (G-011; D-065)."""
        if isinstance(event, ev.RunStarted):
            self.events = 0
        self.events += 1
        self.last_name = type(event).__name__

    def redraw(self, now=False) -> None:
        """Mark the view dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.update()
        if now:
            paint.paint_all()

    def paintEvent(self, event):
        """Paint the id, the title, the pages and the count; `paints` counts the calls, for
        the tests (Numbo GUI, `tree_view.py`'s `paints`; G-011)."""
        self.paints += 1
        painter = QPainter(self)
        rect = self.rect().adjusted(6, 4, -6, -4)
        painter.setPen(QColor(40, 40, 40))
        font = painter.font()
        font.setBold(True)
        painter.setFont(font)
        painter.drawText(rect, Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft
                         | Qt.TextFlag.TextWordWrap, f"{self.pane_id}  {self.title}")
        font.setBold(False)
        painter.setFont(font)
        painter.setPen(QColor(90, 90, 90))
        body = rect.adjusted(0, 20, 0, 0)
        painter.drawText(body, Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft
                         | Qt.TextFlag.TextWordWrap,
                         f"{self.pages}\n\nnot built yet (a later item)\n"
                         f"events seen: {self.events}"
                         + (f", last: {self.last_name}" if self.last_name else ""))
        painter.end()
