"""The list popups (lib/SGUI/List.pm's ProcessClickOnItem and CreatePopupWidget) and the
click on a list's canvas items.

``list_click(scene, x, y)`` is Tk's 'current' item and its bindings: the topmost item under
the point, and what ``lists.click_of_tags`` makes of its tags (page up, page down, or a row's
entry).

CreatePopupWidget makes one Toplevel per list, "Actions for <class>", withdrawn until a row
is clicked; closing it withdraws it again. It holds one button per ActionButtons entry, packed
top to bottom in Perl's hash order (``listactions.ACTIONS``). Here the window also shows the
clicked entry's details (``hover.describe_entry``; not in Perl). A button emits
``chosen(part, action)``; the owner runs the action (``Runner.list_action``) and the popup is
withdrawn, as the Tk button's command does after the action and Tk::Seqsee::Update.
"""
from PySide6.QtCore import QObject, QRectF, Qt, Signal
from PySide6.QtWidgets import QLabel, QPushButton, QVBoxLayout, QWidget

from seqsee.gui import listactions
from seqsee.gui.draw import lists

from . import render


CLOSE_ENOUGH = 1.0      # the Tk canvas's -closeenough default


def list_click(scene, x, y):
    """The ``lists.Click`` a button-1 press at (x, y) fires, or None. Tk's 'current' item is
    the topmost one within ``-closeenough`` (1 px) of the pointer, so a click on the line
    between two rows goes to the row stacked higher (the earlier one: each bar is lowered
    under everything when drawn)."""
    halo = QRectF(x - CLOSE_ENOUGH, y - CLOSE_ENOUGH, 2 * CLOSE_ENOUGH, 2 * CLOSE_ENOUGH)
    for item in scene.items(halo, Qt.IntersectsItemShape, Qt.DescendingOrder):
        tags = item.data(render.TAGS_ROLE)
        if tags is None:
            continue
        return lists.click_of_tags(tags)    # only the 'current' (topmost) item's bindings
    return None


class ListPopup(QWidget):
    """One list's popup window."""

    chosen = Signal(str, str)       # part, action

    def __init__(self, part, parent=None):
        super().__init__(parent, Qt.Window)
        self.part = part
        self.setWindowTitle(listactions.popup_title(part))
        layout = QVBoxLayout(self)
        self.details = QLabel()
        self.details.setTextInteractionFlags(Qt.TextSelectableByMouse)
        layout.addWidget(self.details)
        self.buttons = []
        for action in listactions.ACTIONS.get(part, {}):
            button = QPushButton(action)
            button.clicked.connect(lambda _=False, a=action: self._clicked(a))
            layout.addWidget(button, 0, Qt.AlignHCenter)    # pack -side top: natural width
            self.buttons.append(button)
        layout.addStretch(1)
        self.hide()

    def button(self, action):
        return next(b for b in self.buttons if b.text() == action)

    def show_item(self, details):
        """ProcessClickOnItem's deiconify, with the entry's details."""
        self.details.setText(details or "(this item is no longer shown)")
        self.adjustSize()
        self.show()
        self.raise_()
        self.activateWindow()

    def _clicked(self, action):
        self.chosen.emit(self.part, action)
        self.hide()                         # $Toplevel->withdraw

    def closeEvent(self, event):            # WM_DELETE_WINDOW → withdraw
        event.ignore()
        self.hide()


class ListPopups(QObject):
    """The popups of a window, created on first use (``$self->{POPUP_WIDGET} ||= ...``);
    the views and panes share them, as Perl's views share the list objects."""

    chosen = Signal(str, str)       # part, action

    def __init__(self, parent_widget=None):
        super().__init__(parent_widget)
        self._parent_widget = parent_widget
        self._popups = {}

    def popup(self, part):
        if part not in self._popups:
            p = ListPopup(part, self._parent_widget)
            p.chosen.connect(self.chosen)
            self._popups[part] = p
        return self._popups[part]

    def close_all(self):
        for p in self._popups.values():
            p.hide()
