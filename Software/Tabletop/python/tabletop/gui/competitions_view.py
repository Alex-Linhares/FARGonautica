"""P6 Competitions, GUI design, not French's, after Fig. 3.11 [F95 p. 72 (img)] (G-041):
every fight for the Worldview in order, the Old Worldview against the Rival Worldview
side by side as two small tables, the old correspondences with a solid bar and the
newcomer with a dashed one (Fig. 3.11's convention), then the two ratings, the chance,
the exponent `a` and the winner; the entries without a fight listed as such [F95
p. 101]; and the banner "no competition in this run" when there is none (ANSWERS.md 11).

`CompetitionsView` is the pane: the label over a `LinesWidget` (`worldview_view.py`,
G-040) painting the model's lines; the lines of the fight the event shown reports are
tinted, as the rack list's chosen band is (G-038). It keeps the view protocol the window
expects (G-011) and takes the window's history and selection through `attach`: a click on
a fight selects its newcomer in the shared selection (G-022).
"""

from PySide6.QtGui import QImage
from PySide6.QtWidgets import QHBoxLayout, QLabel, QSizePolicy, QVBoxLayout, QWidget

from tabletop.gui import paint
from tabletop.gui.models import competitions as cm
from tabletop.gui.models import worldview as wv
from tabletop.gui.worldview_view import MIN_SIZE, LinesWidget

__all__ = ["CompetitionsView"]


class CompetitionsView(QWidget):
    """The competitions pane (G-041; G-011's view protocol): the label over the
    `LinesWidget` of a `CompetitionsModel`; `attach(history, selection)`;
    `set_position`, `on_event`, `redraw(now=False)`, `paints`, `events`, `pane_id`,
    `title`, `pages`."""

    def __init__(self, title="Competitions", pane_id="P6", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = cm.CompetitionsModel()
        self.history = None
        self.position = -1
        self.list = LinesWidget(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self.label = QLabel(wv.NOT_FRENCHS + ': the Old Worldview against the Rival Worldview, '
                            'solid against dashed as in Fig. 3.11 [F95 p. 72 (img)]')
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
        """Take the window's history and shared selection (G-022, G-041)."""
        self.history = history
        self.list.selection = selection
        if selection is not None:
            selection.subscribe(self._on_selection)

    def _on_selection(self, _selection) -> None:
        """The shared selection changed, from this pane or another: redraw now (G-022)."""
        self.redraw(now=True)

    @property
    def paints(self) -> int:
        """The list's paint count (the window's probe; G-011)."""
        return self.list.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    def set_position(self, index: int) -> None:
        """Which event comes next (G-017's protocol); the model needs no snapshot."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model (G-041)."""
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Take the lines, mark the list dirty and, with `now`, paint it at once (G-041;
        Numbo GUI, `paint.py`)."""
        self.list.refresh()
        self.list.viewport().update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the list's viewport to a PNG file (RGB), for the tests
        (G-041)."""
        self.list.viewport().grab().toImage().convertToFormat(
            QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()
