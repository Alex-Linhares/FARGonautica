"""The selection shared by every pane, Qt-free (PANES.md P2, "Click to select: the
selection is shared with every pane"; G-025).

One `Selection` belongs to the window. A pane selects an element by `select(kind, name,
members)`: the element's `kind` (`glint`, `glom`, `bond`, `spark`, `gnoth`, `class`,
`task`), its `name` and the `members` the selection implies (a glom's glints and
subgloms, a bond's two ends), so that `includes(name)` is true for the element and
everything under it. The listeners (`subscribe`) are called with the selection after
every change; a view redraws itself from it. `clear()` empties it. The selection holds
names only, never an engine object (ENGINE-API.md section 2).
"""

__all__ = ["Selection"]


class Selection:
    """What is selected across the panes (G-025): `kind` and `name` (None when nothing
    is), `members` (a frozenset of the names the selection implies, the element's own
    included), and `listeners`."""

    def __init__(self):
        self.kind = None
        self.name = None
        self.members = frozenset()
        self.listeners = []

    def subscribe(self, listener) -> None:
        """Call `listener(selection)` after every change (G-025)."""
        self.listeners.append(listener)

    def select(self, kind: str, name: str, members=()) -> None:
        """Select one element with the names it implies (G-025)."""
        self.kind, self.name = kind, name
        self.members = frozenset(members) | {name}
        self._notify()

    def toggle(self, kind: str, name: str, members=()) -> None:
        """Select the element, or clear the selection if it is already the one selected
        (a second click on it) (G-025)."""
        if self.name == name and self.kind == kind:
            self.clear()
        else:
            self.select(kind, name, members)

    def clear(self) -> None:
        """Select nothing (G-025)."""
        if self.name is None and not self.members:
            return
        self.kind = self.name = None
        self.members = frozenset()
        self._notify()

    @property
    def empty(self) -> bool:
        """Whether nothing is selected (G-025)."""
        return self.name is None

    def includes(self, name: str) -> bool:
        """Whether the element of that name is selected, itself or as a member (G-025)."""
        return name in self.members

    def _notify(self) -> None:
        for listener in list(self.listeners):
            listener(self)
