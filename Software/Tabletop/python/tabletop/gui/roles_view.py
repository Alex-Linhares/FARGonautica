"""P14 Roles, GUI design, not French's, a re-interpretation (G-060): the perceived roles of
a run, grouped by family (D-079), each tag with its evidence (the descriptions it was read
from), its weight and strength, the codelet that read it, and the consumers that read it
(from `RoleConsumed`, D-105); the tags an item carried and lost listed after its standing
ones, dashed, with when and why each went (lost to a rival, broken by the checker, gone
with its structure: the author's answer D1 of the roles loop); the symmetry axis and the
re-cast's tiers (D-083, D-093) at the top.

`RolesView` is the pane: the label over a `LinesWidget` (`worldview_view.py`, G-040)
painting the lines of a `models/roles.py` model; a standing tag carries a solid bar and a
past one a dashed bar, the convention the Worldview pane uses for the Worldview against
the shadow cabinet; the lines of the tag the event shown built or discarded are tinted,
as the rack list's chosen band is (G-038). It keeps the view protocol the window expects
(G-011) and takes the window's history and selection through `attach`: a click on an
item's lines selects the object, or the group with its members, in the shared selection
(G-022), so the table shows its badge among the others.
"""

from PySide6.QtGui import QImage
from PySide6.QtWidgets import QHBoxLayout, QLabel, QSizePolicy, QVBoxLayout, QWidget

from tabletop.gui import paint
from tabletop.gui.models import roles as rm
from tabletop.gui.worldview_view import MIN_SIZE, LinesWidget

__all__ = ["RolesView"]


class RolesView(QWidget):
    """The roles pane (G-060; G-011's view protocol): the label over the `LinesWidget`
    of a `RolesModel`; `attach(history, selection)`; `set_position`, `on_event`,
    `redraw(now=False)`, `paints`, `events`, `pane_id`, `title`, `pages`."""

    def __init__(self, title="Roles", pane_id="P14", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = rm.RolesModel()
        self.history = None
        self.position = -1
        self.list = LinesWidget(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self.label = QLabel(rm.NOT_FRENCHS + ", a re-interpretation (D-078): the role tags "
                            "by family, solid while standing and dashed once lost")
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
        """Take the window's history and shared selection (G-022, G-060)."""
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
        """Feed the model (G-060)."""
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Take the lines, mark the list dirty and, with `now`, paint it at once (G-060;
        Numbo GUI, `paint.py`)."""
        self.list.refresh()
        self.list.viewport().update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the list's viewport to a PNG file (RGB), for the tests
        (G-060)."""
        self.list.viewport().grab().toImage().convertToFormat(
            QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()
