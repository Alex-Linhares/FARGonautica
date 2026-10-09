"""The panes: each drawing module of lib/Tk/Seqsee.pm's @ViewOptions in a dock of its own.

Perl shows these modules only inside the 11 fixed views. Here each one is also a QDockWidget
(hidden at first; the Panes menu shows it), drawing its module over its whole canvas: the
composition of ``views.pane_view(part)``, as SetupParts would place a part covering 100% of
the canvas. A pane draws the window's snapshot when it is visible (on each snapshot, when
shown, and after a resize); a hidden pane doesn't draw. A list pane keeps its own page (Perl's
lists are shared by the views; a pane's page doesn't move the view's).

A module's die (PERL-QUIRK, see ``views``: the Rules list always dies) leaves what was drawn
before it, and the dock's title says "(died)" with the message as its tooltip.
"""
import dataclasses

from PySide6.QtCore import Qt, QTimer
from PySide6.QtWidgets import QDockWidget

from seqsee.gui import hover
from seqsee.gui.draw import views

from . import render


@dataclasses.dataclass(frozen=True)
class Pane:
    part: str           # the Perl module, as in views.VIEW_OPTIONS
    title: str
    object_name: str    # the dock's objectName (QMainWindow.saveState keys on it)


PANES = (
    Pane(views.WORKSPACE, "Workspace", "pane_workspace"),
    Pane(views.ATTENTION, "Attention", "pane_attention"),
    Pane(views.SLIPNET, "Slipnet", "pane_slipnet"),
    Pane(views.CODERACK, "Coderack", "pane_coderack"),
    Pane(views.STREAM, "Stream", "pane_stream"),
    Pane(views.RELATIONS, "Relations", "pane_relations"),
    Pane(views.GROUPS_LIST, "Groups", "pane_groups"),
    Pane(views.CATEGORIES_LIST, "Categories", "pane_categories"),
    Pane(views.RULES_LIST, "Rules", "pane_rules"),
    Pane(views.STREAM_LIST, "Stream list", "pane_stream_list"),
)

PANE_WIDTH = 390        # half the 780-pixel canvas
PANE_HEIGHT = 225       # half the 450-pixel canvas


class PaneDock(QDockWidget):
    """One pane. ``window`` gives the snapshot, ``measure``, ``known_families`` and
    ``remember_coderack_rows``; ``canvas_class`` is the window's Canvas."""

    def __init__(self, pane, window, canvas_class, background):
        super().__init__(pane.title, window)
        self.pane = pane
        self.view = views.pane_view(pane.part)
        self.setObjectName(pane.object_name)
        self._window = window
        self.pages = {}
        self.composition = None
        self.composed_size = None
        self._redraw_timer = QTimer(self)
        self._redraw_timer.setSingleShot(True)
        self._redraw_timer.setInterval(0)
        self._redraw_timer.timeout.connect(self.redraw)
        self.canvas = canvas_class(PANE_WIDTH, PANE_HEIGHT, on_resize=self._redraw_timer.start,
                                   on_click=self.canvas_clicked, tooltip_at=self.tooltip_at)
        self.canvas.scene().setBackgroundBrush(render.qcolor(background))
        self.setWidget(self.canvas)
        self.visibilityChanged.connect(self._visibility_changed)
        self.hide()

    def canvas_size(self):
        return self.canvas.size_px()

    def _visibility_changed(self, visible):
        if visible:
            self._redraw_timer.start()

    def set_page(self, page):
        """Show this list pane at ``page``."""
        self.pages[self.pane.part] = page
        self.redraw()

    def canvas_clicked(self, x, y):
        """Button 1: the list bindings (``MainWindow.handle_list_click``), with this pane's
        own page; a row opens the window's popup for the list."""
        self._window.handle_list_click(self.canvas.scene(), self.composition, self.pages,
                                       lambda part, page: self.set_page(page), x, y)

    def tooltip_at(self, x, y):
        """The hover tooltip at (x, y) of the pane ('' if no workspace object is there)."""
        snap = self._window.snapshot
        if snap is None or self.composed_size is None:
            return ""
        try:
            return hover.tooltip(snap, self.view, *self.composed_size, x, y,
                                 measure=self._window.measure)
        except Exception:  # noqa: BLE001 - a hover must never break the window
            return ""

    def redraw(self):
        """Compose the pane's module at the canvas's size, if the dock is visible."""
        self._redraw_timer.stop()
        snap = self._window.snapshot
        if self.isHidden() or snap is None:
            return
        w, h = self.canvas_size()
        try:
            comp = views.compose(self.view, snap, w, h, pages=self.pages,
                                 measure=self._window.measure,
                                 known_families=self._window.known_families)
            render.render(comp.ops, scene=self.canvas.scene(), width=w, height=h)
        except Exception as e:  # a drawing bug must not kill the window
            self._show_died(f"Drawing failed: {type(e).__name__}: {e}")
            return
        self.composition = comp
        self.composed_size = (w, h)
        self._window.remember_coderack_rows(comp, self.view)
        self._show_died(comp.died)

    def _show_died(self, died):
        if died:
            self.setWindowTitle(f"{self.pane.title} (died)")
            self.setToolTip(str(died).strip())
        else:
            self.setWindowTitle(self.pane.title)
            self.setToolTip("")


def make_docks(window, canvas_class, background, area=Qt.RightDockWidgetArea):
    """A dock per pane, all in ``area`` (shown ones stack there); part → dock."""
    docks = {}
    for pane in PANES:
        dock = PaneDock(pane, window, canvas_class, background)
        window.addDockWidget(area, dock)
        docks[pane.part] = dock
    return docks
