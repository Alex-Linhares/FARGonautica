"""A tidy layout of the cytoplasm's forest (not 1987 source; loop0003).

Qt-free.  layout_forest(forest, measure) places every TreeNode of a
tree_model.Forest as a box: roots on the top row, each node's children one
row down, in order, left to right.  Box sizes come from MEASURE, a function
of the label text giving (width, height), so that a view passes its font
metrics and the tests fake them; LABEL gives a node's text (node_label by
default: an operation's symbol, otherwise the value).

Inside a tree the layout is Reingold-Tilford's, with contours: a subtree is
laid out on its own, its left and right extent kept for each row under it;
siblings are placed left to right, each as far left as its contour allows
(sibling_gap from the subtrees before it on every row they share), so a
deep subtree's lower rows can tuck under a shallow sibling; a parent is
centered over its first and last child.  Each tree has its own rows (a row
as tall as the tree's tallest box at that depth, boxes centered in it), and
every root's top is at y = 0: rows shared by the whole forest would move
every tree down when one label anywhere grew a line.

The live trees are side by side from x = 0 in the forest's order (the
target's tree, the blocks, the free bricks, the loose operation nodes),
tree_gap apart, group_gap between groups.  The ghosts have a band of
their own above them (y < 0, from x = 0): they come and go first in, first
out at nearly every kill, and after the live trees they were pushed about
by every change there (test_tree_layout.py measured it).

Stability: given the previous layout, a live tree whose root was there
keeps its left edge when it can, that is, when that is not left of where
packing would put it (then it is pushed right) and not more than SLACK
right of it (then the gap closes).  So a change in a tree never moves the
trees before it, a tree that grows pushes those after it, and a tree that
shrinks (or leaves) moves the ones after it only when it frees more than
the slack.  A ghost keeps its x for as long as it is one, and a new ghost
takes the leftmost gap it fits in (_ghost_slots).  TreeLayout remembers
the previous layout, and measures each label once.
"""

import dataclasses

from numbo.models.tree_model import op_name

GROUPS = ("targets", "blocks", "bricks", "loose", "ghosts")


@dataclasses.dataclass(frozen=True)
class Spacing:
    """Gaps, in the measure's units."""
    sibling_gap: float = 12     # between neighboring subtrees in a tree
    tree_gap: float = 24        # between trees in a group (and ghosts)
    group_gap: float = 48       # between groups
    level_gap: float = 28       # between rows (and above the ghosts' band)
    min_row_height: float = 0
    slack: float = 120          # how far right of packed a live tree may stay


@dataclasses.dataclass(frozen=True)
class Box:
    """A node's box: X, Y its top left corner.  DEPTH: its row; ROOT: its
    tree's root; PARENT: its parent's name (None for a root)."""
    name: str
    x: float
    y: float
    width: float
    height: float
    depth: int
    root: str
    parent: object = None

    @property
    def cx(self):
        return self.x + self.width / 2

    @property
    def cy(self):
        return self.y + self.height / 2

    @property
    def right(self):
        return self.x + self.width

    @property
    def bottom(self):
        return self.y + self.height


@dataclasses.dataclass(frozen=True)
class TreeBox:
    """A tree's extent: ROOT's tree, in GROUP, from X, WIDTH wide, ROWS deep."""
    root: str
    group: str
    x: float
    width: float
    rows: int


@dataclasses.dataclass(frozen=True)
class Layout:
    """BOXES: name -> Box; TREES: TreeBoxes, the live trees in the forest's
    order, then the ghosts in x order; EDGES: (parent, child) names,
    parents first; the extent: x from 0 to WIDTH, y from TOP (the ghosts'
    band, < 0, when there are ghosts) to TOP + HEIGHT."""
    boxes: dict
    trees: tuple
    edges: tuple
    width: float
    height: float
    top: float = 0.0

    __hash__ = None


SPACING = Spacing()


def node_label(node):
    """An operation's symbol ("+", "-", "x", "/"), or its operation
    ("PLUS") when it has none (a ghost); otherwise the value."""
    if node.type == "5g":
        return node.symbol or node.op or op_name(node.name)
    return node.name if node.value is None else str(node.value)


def _place(node, parent, size, gap):
    """NODE's subtree, centered on NODE: ([node, parent, depth, cx], ...)
    and its contour, ((left, right), ...) for each row from NODE's."""
    w = size(node)[0]
    if not node.children:
        return [[node, parent, 0, 0.0]], [[-w / 2, w / 2]]
    items, contour, centers = [], [], []
    for child in node.children:
        sub, sub_contour = _place(child, node.name, size, gap)
        shift = 0.0
        if contour:
            shift = max(contour[d][1] + gap - sub_contour[d][0]
                        for d in range(min(len(contour), len(sub_contour))))
        for item in sub:
            item[3] += shift
        for d, (left, right) in enumerate(sub_contour):
            if d < len(contour):
                contour[d][0] = min(contour[d][0], left + shift)
                contour[d][1] = max(contour[d][1], right + shift)
            else:
                contour.append([left + shift, right + shift])
        items.extend(sub)
        centers.append(shift)
    mid = (centers[0] + centers[-1]) / 2
    for item in items:
        item[2] += 1
        item[3] -= mid
    return ([[node, parent, 0, 0.0]] + items,
            [[-w / 2, w / 2]] + [[left - mid, right - mid] for left, right in contour])


def layout_forest(forest, measure, label=node_label, spacing=SPACING, previous=None,
                  _size=None):
    """FOREST's Layout (see the module docstring).  PREVIOUS: the layout
    before, whose trees keep their place when they can."""
    if _size is None:
        def _size(node):
            return measure(label(node))
    live_before, ghosts_before = {}, {}
    for t in previous.trees if previous is not None else ():
        (ghosts_before if t.group == "ghosts" else live_before)[t.root] = t.x
    boxes, tree_boxes, edges = {}, [], []

    def put(group, tree, x):
        root, items, left, width, rows, tops = tree
        for node, parent, d, cx in items:
            w, h = _size(node)
            boxes[node.name] = Box(name=node.name, x=x - left + cx - w / 2,
                                   y=tops[d] + (rows[d] - h) / 2, width=w, height=h,
                                   depth=d, root=root.name, parent=parent)
            if parent is not None:
                edges.append((parent, node.name))
        tree_boxes.append(TreeBox(root=root.name, group=group, x=x, width=width,
                                  rows=len(rows)))

    cursor, last_group = 0.0, None
    for group in GROUPS[:-1]:
        for root in getattr(forest, group):
            tree = _shape(root, _size, spacing)
            if last_group is not None:
                cursor += spacing.tree_gap if group == last_group else spacing.group_gap
            last_group = group
            x = cursor
            kept = live_before.get(root.name)
            if kept is not None and cursor <= kept <= cursor + spacing.slack:
                x = kept
            put(group, tree, x)
            cursor = x + tree[3]
    ghosts = [_shape(root, _size, spacing, above=True) for root in forest.ghosts]
    for x, i in _ghost_slots(ghosts, ghosts_before, spacing):
        put("ghosts", ghosts[i], x)
    top = min((b.y for b in boxes.values()), default=0)
    height = max((b.bottom for b in boxes.values()), default=0) - top
    width = max((t.x + t.width for t in tree_boxes), default=0)
    return Layout(boxes=boxes, trees=tuple(tree_boxes), edges=tuple(edges),
                  width=width, height=height, top=top)


def _shape(root, size, spacing, above=False):
    """ROOT's tree laid out on its own: (root, items, left, width, rows,
    tops), with _place's items and contour's left, the rows' heights and
    their tops (from 0 down, or, ABOVE, ending level_gap above 0)."""
    items, contour = _place(root, None, size, spacing.sibling_gap)
    left = min(c[0] for c in contour)
    width = max(c[1] for c in contour) - left
    rows = [spacing.min_row_height] * len(contour)
    for node, _, d, _ in items:
        rows[d] = max(rows[d], size(node)[1])
    tops, y = [], 0.0
    for h in rows:
        tops.append(y)
        y += h + spacing.level_gap
    if above:
        tops = [t - y for t in tops]
    return root, items, left, width, rows, tops


def _ghost_slots(ghosts, before, spacing):
    """Where the GHOSTS (_shape's trees) go in their band, from x = 0 on:
    ((x, index), ...) in x order.  A ghost in the previous layout's band
    (BEFORE: name -> x) keeps its x (it is pushed right only if a ghost
    before it got wider); a new ghost takes the leftmost gap it fits in, in
    the forest's order, else goes last.  Gaps are not closed otherwise: a
    ghost is gone after the view's window, and first fit kept the band no
    wider than the ghosts packed side by side on the recorded runs."""
    gap = spacing.tree_gap
    kept = sorted((before[g[0].name], i) for i, g in enumerate(ghosts)
                  if g[0].name in before)
    placed, right = [], -gap
    for x, i in kept:
        x = max(x, right + gap)
        placed.append((x, i))
        right = x + ghosts[i][3]
    for i, g in enumerate(ghosts):
        if g[0].name in before:
            continue
        width, lo, at = g[3], 0.0, len(placed)
        for j, (x, k) in enumerate(placed):
            if lo + width + gap <= x:
                at = j
                break
            lo = x + ghosts[k][3] + gap
        placed.insert(at, (lo, i))
    return placed


class TreeLayout:
    """Lays out successive forests of a run, each given the one before (so
    trees keep their place), measuring each label once."""

    def __init__(self, measure, label=node_label, spacing=SPACING):
        self.measure = measure
        self.label = label
        self.spacing = spacing
        self._sizes = {}
        self.previous = None
        self._forest = self._laid_out = None

    def size(self, node):
        text = self.label(node)
        size = self._sizes.get(text)
        if size is None:
            size = self._sizes[text] = tuple(self.measure(text))
        return size

    def layout(self, forest):
        """FOREST's layout, from the previous one.  For the forest it has
        just laid out (the same object, and the previous layout still its
        own), the same layout: a layout is a fixed point of laying out its
        forest again (tested on recorded runs), and a TreeModel keeps its
        forest object while nothing in it changes."""
        if forest is self._forest and self.previous is not None \
                and self.previous is self._laid_out:
            return self.previous
        self.previous = layout_forest(forest, self.measure, self.label, self.spacing,
                                      self.previous, _size=self.size)
        self._forest, self._laid_out = forest, self.previous
        return self.previous

    def reset(self):
        """Forget the previous layout (a new run)."""
        self.previous = None
