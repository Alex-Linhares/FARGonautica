"""The selection shared by every pane, Qt-free (PANES.md P1, "click to select: the
selection is shared with P5, P6 and P10"; Seek-Whence GUI, `models/selection.py`,
adopted whole; G-022).

One `Selection` belongs to the window. A pane selects an element by `select(kind, name,
members)`: the element's `kind` (`object`, `group`, `proposal`, `correspondence`, or
`node`, a Slipnet node selected in the grid, G-030), its `name` (an object's id, a
group's or a proposal's or a correspondence's name, a node's name) and the
`members` the selection implies (a group's members, a correspondence's two ends with
their members), so that `includes(name)` is true for the element and everything under it.
The listeners (`subscribe`) are called with the selection after every change; a view
redraws itself from it. `clear()` empties it. The selection holds names only, never an
engine object (`docs/engine-api.md`, "The observer rules").
"""

__all__ = ["KINDS", "Selection"]

# The kinds of element the panes select (G-022; `node` from the Slipnet grid, G-030).
KINDS = ("object", "group", "proposal", "correspondence", "node")


class Selection:
    """What is selected across the panes (G-022): `kind` and `name` (None when nothing
    is), `members` (a frozenset of the names the selection implies, the element's own
    included), and `listeners`."""

    def __init__(self):
        self.kind = None
        self.name = None
        self.members = frozenset()
        self.listeners = []

    def subscribe(self, listener) -> None:
        """Call `listener(selection)` after every change (G-022)."""
        self.listeners.append(listener)

    def select(self, kind: str, name: str, members=()) -> None:
        """Select one element with the names it implies (G-022)."""
        self.kind, self.name = kind, name
        self.members = frozenset(members) | {name}
        self._notify()

    def toggle(self, kind: str, name: str, members=()) -> None:
        """Select the element, or clear the selection if it is already the one selected
        (a second click on it) (G-022)."""
        if self.name == name and self.kind == kind:
            self.clear()
        else:
            self.select(kind, name, members)

    def clear(self) -> None:
        """Select nothing (G-022)."""
        if self.name is None and not self.members:
            return
        self.kind = self.name = None
        self.members = frozenset()
        self._notify()

    @property
    def empty(self) -> bool:
        """Whether nothing is selected (G-022)."""
        return self.name is None

    def includes(self, name: str) -> bool:
        """Whether the element of that name is selected, itself or as a member (G-022)."""
        return name in self.members

    def _notify(self) -> None:
        for listener in list(self.listeners):
            listener(self)
