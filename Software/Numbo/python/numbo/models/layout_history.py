"""The tree layout at any event of a run, for scrubbing (not 1987 source;
loop0003).  Qt-free.

The tree canvas lays out the forest after every event, and each layout is
given the one before it, so that trees keep their place (tree_layout's
stability).  A layout is therefore a function of the whole run up to its
event, not of the forest alone: the run's *canonical* layouts are the
chain a view makes when it draws every event, from the run's start
(a start event resets the chain).

To show the canonical layout when a replay jumps to event N, LayoutHistory
keeps snapshots: CHECKPOINTS, the canonical layout at every EVERY-th event
(64 by default; a Layout is small and immutable).  The window records them
as it draws a run (record), and the history makes the rest when asked:
layout_at(events, N) takes the last checkpoint C at or before N, rebuilds a
TreeModel from events 0..C (models alone are fast: about 1 µs an event),
and lays out events C+1..N, keeping the checkpoints it passes.  So a jump
costs at most EVERY layouts once the run has been walked, and a first walk
costs one layout per event (about 0.1 ms); advance(events, count) walks
ahead in chunks, for idle time.  The walk is kept, so going on forward from
the last query (or the last chunk) doesn't rebuild anything.

MAKE_LAYOUTER() must make the TreeLayout the view uses (the same measure,
label and spacing) and GHOST_WINDOW be the view's, or the layouts would not
be the view's.
"""

from numbo.models.tree_model import TreeModel

__all__ = ["EVERY", "LayoutHistory"]

EVERY = 64


class _Walk:
    """A model and a layout chain at event INDEX (-1: before the first)."""

    def __init__(self, layouter):
        self.model = TreeModel()
        self.layouter = layouter
        self.layout = None
        self.index = -1


class LayoutHistory:
    """Canonical tree layouts of a run (see the module docstring)."""

    def __init__(self, make_layouter, ghost_window, every=EVERY):
        self.make_layouter = make_layouter
        self.ghost_window = ghost_window
        self.every = every
        self.clear()

    def clear(self):
        """Forget the run (a new one)."""
        self.checkpoints = {}       # event index -> Layout
        self._walk = None

    @property
    def frontier(self):
        """The last event whose layout is known without a walk (-1: none)."""
        known = max(self.checkpoints, default=-1)
        if self._walk is not None:
            known = max(known, self._walk.index)
        return known

    def record(self, index, layout):
        """LAYOUT is the canonical layout at event INDEX (kept if INDEX is a
        checkpoint)."""
        if index % self.every == 0:
            self.checkpoints[index] = layout

    def layout_at(self, events, index):
        """The canonical layout at EVENTS[INDEX] (None for INDEX -1)."""
        if index < 0:
            return None
        if index >= len(events):
            raise IndexError(f"event {index} of {len(events)}")
        layout = self.checkpoints.get(index)
        if layout is not None:
            return layout
        walk = self._walk_to(events, index)
        while walk.index < index:
            self._step(walk, events)
        self._walk = walk
        return walk.layout

    def advance(self, events, count):
        """Walk COUNT events past the frontier (not past the end); returns
        the new frontier."""
        target = min(len(events) - 1, self.frontier + count)
        if target > self.frontier:
            self.layout_at(events, target)
        return self.frontier

    def _walk_to(self, events, index):
        """A walk at or before INDEX: the kept one if no checkpoint is
        nearer, else one rebuilt at the last checkpoint."""
        c = max((k for k in self.checkpoints if k <= index), default=-1)
        walk = self._walk
        if walk is not None and c <= walk.index <= index:
            return walk
        walk = _Walk(self.make_layouter())
        for event in events[:c + 1]:
            walk.model.on_event(event)
        if c >= 0:
            walk.layout = walk.layouter.previous = self.checkpoints[c]
        walk.index = c
        return walk

    def _step(self, walk, events):
        """What the view does at the walk's next event."""
        i = walk.index + 1
        event = events[i]
        walk.model.on_event(event)
        if event.kind == "start":
            walk.layouter.reset()
        walk.layout = walk.layouter.layout(walk.model.forest(ghost_window=self.ghost_window))
        walk.index = i
        self.record(i, walk.layout)
