"""Loop0003 item 3: the tree layout (numbo/models/tree_layout.py).

The layout is a tidy tree (Reingold-Tilford with contours) over the forest
of numbo.models.tree_model, Qt-free: box sizes come from a measure function
of the label text, here fake metrics.  These tests check:
  - hand-made trees: parents centered over their children, children in
    order and one row down, no overlaps, contours (a deep subtree's lower
    rows tucked under its sibling), wide parents, the live trees side by
    side in group order, the ghosts in a band above;
  - seeded random trees: no overlaps, centering, determinism;
  - stability on hand-made changes: trees keep their place, are pushed,
    close large gaps; ghosts keep their slots and new ones fill gaps;
  - recorded runs (puzzle 1 seed 1, puzzle 3 seed 8, and three more): the
    layout of the view's forest after every event has no overlaps and is
    deterministic, unchanged trees before a change never move, a ghost
    never moves, and live trees move within measured bounds (and less
    than without the previous layout).
"""

import io
import random

import pytest

from full_runs import PUZZLES
from numbo import harness, observe
from numbo.models.tree_layout import (Layout, Spacing, TreeLayout, layout_forest,
                                      node_label)
from numbo.models.tree_model import Forest, TreeModel, TreeNode


def measure(text):
    """Fake metrics: 7 px a character on the longest line, 16 px a line,
    plus padding."""
    lines = text.split("\n")
    return 7 * max(len(line) for line in lines) + 10, 16 * len(lines) + 6


def view_label(node):
    """A label as a view might draw it: the value, and the expression
    under it for a derived node (so widths change as trees grow)."""
    if node.type == "5g":
        return node_label(node)
    text = str(node.value)
    if node.children and node.expression:
        text += "\n" + node.expression
    return text


_counter = [0]


def leaf(value, type="2b", name=None):
    _counter[0] += 1
    return TreeNode(name=name or f"N{_counter[0]}-{value}", type=type, value=value,
                    status="free", level=1, activation=50, success=1, alive=True)


def node(value, *children, type="4bl", name=None):
    return dataclass_replace(leaf(value, type, name), children=tuple(children))


def op(symbol, a, b, name=None):
    _counter[0] += 1
    return TreeNode(name=name or f"OP{_counter[0]}", type="5g", value=None, status=None,
                    level=2, activation=None, success=None, alive=True,
                    op="PLUS" if symbol in "+-" else "TIMES", symbol=symbol,
                    children=(a, b))


def dataclass_replace(obj, **kw):
    import dataclasses
    return dataclasses.replace(obj, **kw)


# ---------------------------------------------------------------------------
# Invariants

def overlaps(layout):
    """Pairs of boxes that intersect (a sweep over x)."""
    boxes = sorted(layout.boxes.values(), key=lambda b: b.x)
    bad = []
    for i, a in enumerate(boxes):
        for b in boxes[i + 1:]:
            if b.x >= a.right:
                break
            if a.y < b.bottom and b.y < a.bottom:
                bad.append((a.name, b.name))
    return bad


def default_size(n):
    return measure(node_label(n))


def check_layout(layout, forest, spacing=Spacing(), size=default_size):
    """No overlaps; every node of the forest has a box of its measured size;
    each parent is centered over its first and last child; children are in
    order, left to right, at least sibling_gap apart, in the next row; live
    trees are side by side in the forest's order, tree_gap apart at least,
    roots at y = 0; ghosts in their band above, from x = 0, in x order."""
    assert overlaps(layout) == []
    names = [n.name for n in forest.walk()]
    assert sorted(layout.boxes) == sorted(names)
    for tree in forest.trees():
        for n in tree.walk():
            box = layout.boxes[n.name]
            assert (box.width, box.height) == size(n)
            if not n.children:
                continue
            kids = [layout.boxes[c.name] for c in n.children]
            assert box.cx == pytest.approx((kids[0].cx + kids[-1].cx) / 2)
            for k in kids:
                assert k.parent == n.name and k.depth == box.depth + 1
                assert k.y >= box.bottom + spacing.level_gap - 1e-9
            for a, b in zip(kids, kids[1:]):
                assert b.x >= a.right + spacing.sibling_gap - 1e-9
    # Live trees in the forest's order; then the ghosts, in x order (they
    # keep their slots), the same set as the forest's, in a band above.
    trees = layout.trees
    live = [t for t in trees if t.group != "ghosts"]
    ghosts = [t for t in trees if t.group == "ghosts"]
    assert trees == tuple(live + ghosts)
    assert [t.root for t in live] == [t.name for t in forest.trees()
                                       if t not in forest.ghosts]
    assert sorted(t.root for t in ghosts) == sorted(g.name for g in forest.ghosts)
    for row in (live, ghosts):
        for a, b in zip(row, row[1:]):
            assert b.x >= a.x + a.width + spacing.tree_gap - 1e-9
        assert all(t.x >= 0 for t in row)
    assert all(layout.boxes[t.root].y == 0 for t in live)
    for t in ghosts:
        assert all(b.bottom <= -spacing.level_gap + 1e-9
                   for b in layout.boxes.values() if b.root == t.root)
    assert layout.width == max((t.x + t.width for t in trees), default=0)
    assert layout.top == min((b.y for b in layout.boxes.values()), default=0)
    assert layout.height == max((b.bottom for b in layout.boxes.values()),
                                default=0) - layout.top
    # A tree's extent holds its boxes.
    for t in trees:
        for b in layout.boxes.values():
            if b.root == t.root:
                assert t.x - 1e-9 <= b.x and b.right <= t.x + t.width + 1e-9


def forest_of(*trees, group="targets"):
    return Forest(**{group: tuple(trees)})


# ---------------------------------------------------------------------------
# Hand-made trees

def test_a_single_node():
    f = forest_of(leaf(114, "1t", "CYTO-TARGET"))
    lay = layout_forest(f, measure)
    assert isinstance(lay, Layout)
    box = lay.boxes["CYTO-TARGET"]
    assert (box.x, box.y, box.width, box.height) == (0, 0, *measure("114"))
    assert box.depth == 0 and box.parent is None and box.root == "CYTO-TARGET"
    assert lay.edges == ()
    assert (lay.width, lay.height) == measure("114")


def test_default_labels():
    assert node_label(leaf(20)) == "20"
    assert node_label(op("x", leaf(6), leaf(20))) == "x"
    assert node_label(op("-", leaf(7), leaf(1))) == "-"
    # A ghost operation node has no symbol (no head): its operation.
    ghost_op = dataclass_replace(op("+", leaf(3), leaf(11), name="PLUS3-11-V5"),
                                 op=None, symbol=None, children=(), alive=False)
    assert node_label(ghost_op) == "PLUS"


def test_a_parent_is_centered_over_its_children():
    b6, b20 = leaf(6), leaf(20)
    times = op("x", b6, b20)
    block = node(120, times)
    f = forest_of(block, group="blocks")
    lay = layout_forest(f, measure)
    check_layout(lay, f)
    assert lay.boxes[block.name].cx == lay.boxes[times.name].cx
    assert lay.boxes[b6.name].cx < lay.boxes[times.name].cx < lay.boxes[b20.name].cx
    assert [b.depth for b in (lay.boxes[block.name], lay.boxes[times.name],
                              lay.boxes[b6.name])] == [0, 1, 2]
    assert set(lay.edges) == {(block.name, times.name), (times.name, b6.name),
                              (times.name, b20.name)}


def test_a_wide_parent_over_narrow_children():
    times = op("x", leaf(6), leaf(20))
    block = node("a very wide label indeed", times)
    f = forest_of(block)
    lay = layout_forest(f, measure)
    check_layout(lay, f)
    box = lay.boxes[block.name]
    assert box.x == 0 and lay.trees[0].width == box.width


def test_contours_tuck_a_deep_subtree_under_its_sibling():
    """Left: a deep, narrow chain whose lower rows lean right; right: a
    single leaf.  Rows deeper than the leaf don't push it away, so the leaf
    sits just right of the left subtree's top row (Reingold-Tilford), not
    right of its whole extent."""
    deep = op("+", leaf(1), node(9, op("+", leaf(2), node(8, op("+", leaf(3),
                                                                  leaf(4444444))))))
    right = leaf(5)
    top = op("-", deep, right)
    f = forest_of(node(100, top))
    lay = layout_forest(f, measure)
    check_layout(lay, f)
    deep_extent = max(b.right for b in lay.boxes.values() if b.name != right.name
                      and b.depth > lay.boxes[deep.name].depth)
    assert lay.boxes[right.name].x < deep_extent


def test_subtrees_never_overlap_at_any_depth():
    """Right subtree deep and leaning left under a left subtree that is
    shallow but wide at its last row."""
    left = op("+", leaf(11111111), leaf(22222222))
    right = op("+", node(9, op("+", node(8, op("+", leaf(1), leaf(2))), leaf(3))), leaf(4))
    f = forest_of(node(100, op("x", left, right)))
    check_layout(layout_forest(f, measure), f)


def test_the_forest_is_side_by_side_in_group_order():
    target = node(114, op("-", node(120, op("x", leaf(6), leaf(20))), leaf(6, "3dt")),
                  type="1t", name="CYTO-TARGET")
    block = node(6, op("-", leaf(7), leaf(1)), name="CYTO-BLOCK6-V2")
    bricks = (leaf(11, name="CYTO-BRICK1"), leaf(3, name="CYTO-BRICK2"))
    ghost = dataclass_replace(leaf(9, "4bl", "CYTO-BLOCK9-V7"), alive=False)
    f = Forest(targets=(target,), blocks=(block,), bricks=bricks, ghosts=(ghost,))
    spacing = Spacing()
    lay = layout_forest(f, measure, spacing=spacing)
    check_layout(lay, f, spacing)
    assert [(t.root, t.group) for t in lay.trees] == [
        ("CYTO-TARGET", "targets"), ("CYTO-BLOCK6-V2", "blocks"), ("CYTO-BRICK1", "bricks"),
        ("CYTO-BRICK2", "bricks"), ("CYTO-BLOCK9-V7", "ghosts")]
    t = lay.trees
    # Within a group trees are tree_gap apart, between groups group_gap.
    assert t[3].x - (t[2].x + t[2].width) == spacing.tree_gap
    assert t[1].x - (t[0].x + t[0].width) == spacing.group_gap
    assert t[2].x - (t[1].x + t[1].width) == spacing.group_gap
    # Every root on the top row; the ghost in its band above, at x = 0.
    assert {lay.boxes[x.root].depth for x in t} == {0}
    g = lay.boxes["CYTO-BLOCK9-V7"]
    assert g.x == 0 and g.bottom == -spacing.level_gap and lay.top == g.y
    assert lay.width == t[3].x + t[3].width
    assert lay.height == max(b.bottom for b in lay.boxes.values()) - g.y


def test_rows_belong_to_their_tree():
    """Each tree has its own rows (so a tree's boxes depend on that tree
    only): a row is as tall as the tree's tallest box at that depth, boxes
    centered in it; every root's top is at y = 0."""
    a, b = leaf("1\n2\n3"), leaf(2)
    target = node("114\n(6 x 20) - [6]", op("-", a, b), type="1t")
    bare = node(6, op("-", leaf(7), leaf(1)))
    f = Forest(targets=(target,), blocks=(bare,), bricks=(leaf(5),))
    spacing = Spacing()
    lay = layout_forest(f, measure, spacing=spacing)
    check_layout(lay, f)
    boxes = lay.boxes
    assert {boxes[t.name].y for t in f.trees()} == {0}
    assert boxes[a.name].cy == boxes[b.name].cy
    assert boxes[a.name].y == boxes[target.children[0].name].bottom + spacing.level_gap
    # The other tree's rows don't follow the target's taller ones.
    assert boxes[bare.children[0].name].y == boxes[bare.name].bottom + spacing.level_gap
    assert boxes[bare.children[0].name].y < boxes[target.children[0].name].y


def test_the_measure_and_label_functions_are_used():
    f = forest_of(leaf(7, name="A"))
    lay = layout_forest(f, lambda text: (len(text) * 100, 40), label=lambda n: n.name * 3)
    assert (lay.boxes["A"].width, lay.boxes["A"].height) == (300, 40)


def test_an_empty_forest():
    lay = layout_forest(Forest(), measure)
    assert lay.boxes == {} and lay.trees == () and (lay.width, lay.height) == (0, 0)


def random_tree(rng, depth=0):
    """A random expression tree: values over operations over values."""
    value = rng.choice([1, 7, 42, 114, 1000, 123456, "a long label here"])
    if depth >= 6 or rng.random() < 0.3:
        return leaf(value)
    kids = [random_tree(rng, depth + 2) for _ in range(rng.choice([1, 2, 2, 2, 3]))]
    sym = rng.choice("+-x/")
    ops = op(sym, *kids[:2]) if len(kids) >= 2 else op(sym, kids[0], leaf(1))
    if len(kids) == 3:
        ops = dataclass_replace(ops, children=tuple(kids))
    more = (op("+", leaf(2), leaf(3)),) if rng.random() < 0.15 else ()
    return node(value, ops, *more)


@pytest.mark.parametrize("seed", range(40))
def test_random_forests(seed):
    rng = random.Random(seed)
    f = Forest(targets=(random_tree(rng),),
               blocks=tuple(random_tree(rng) for _ in range(rng.randrange(4))),
               bricks=tuple(leaf(rng.randrange(1, 30)) for _ in range(rng.randrange(6))))
    lay = layout_forest(f, measure)
    check_layout(lay, f)
    # Deterministic: the same input, the same output.
    assert layout_forest(f, measure) == lay


# ---------------------------------------------------------------------------
# Stability

def test_trees_keep_their_place_when_a_tree_before_them_shrinks():
    big = node(120, op("x", leaf(6), leaf(20)), name="B1")
    small = leaf(120, "4bl", "B1")
    other = node(6, op("-", leaf(7), leaf(1)), name="B2")
    brick = leaf(11, name="CYTO-BRICK1")
    spacing = Spacing()
    first = layout_forest(Forest(blocks=(big, other), bricks=(brick,)), measure,
                          spacing=spacing)
    second = layout_forest(Forest(blocks=(small, other), bricks=(brick,)), measure,
                           spacing=spacing, previous=first)
    # B1 shrank by less than the slack: B2 and the brick stay put.
    assert first.trees[0].width - second.trees[0].width <= spacing.slack
    for name in [n.name for n in other.walk()] + ["CYTO-BRICK1"]:
        assert second.boxes[name] == first.boxes[name]
    # Without the previous layout they would have moved left.
    packed = layout_forest(Forest(blocks=(small, other), bricks=(brick,)), measure,
                           spacing=spacing)
    assert packed.boxes["B2"].x < first.boxes["B2"].x


def test_a_large_gap_is_closed():
    """A tree that leaves more than the slack free closes the gap."""
    spacing = Spacing(slack=10)
    wide = node("a very very very wide block", op("+", leaf(1), leaf(2)), name="B1")
    brick = leaf(11, name="CYTO-BRICK1")
    first = layout_forest(Forest(blocks=(wide,), bricks=(brick,)), measure, spacing=spacing)
    second = layout_forest(Forest(bricks=(brick,)), measure, spacing=spacing, previous=first)
    assert second.boxes["CYTO-BRICK1"].x == 0


def test_trees_are_pushed_right_when_a_tree_before_them_grows():
    small = leaf(120, "4bl", "B1")
    big = node("wider and wider", op("x", leaf(6), leaf(20)), name="B1")
    brick = leaf(11, name="CYTO-BRICK1")
    first = layout_forest(Forest(blocks=(small,), bricks=(brick,)), measure)
    second = layout_forest(Forest(blocks=(big,), bricks=(brick,)), measure, previous=first)
    check_layout(second, Forest(blocks=(big,), bricks=(brick,)))
    assert second.boxes["CYTO-BRICK1"].x > first.boxes["CYTO-BRICK1"].x


def ghost(name, value=9):
    return dataclass_replace(leaf(value, "4bl", name), alive=False)


def test_ghosts_keep_their_slots_and_new_ones_fill_gaps():
    """Ghosts come and go first in, first out: one that stays keeps its
    x; a new one takes the leftmost gap it fits in, else goes last."""
    brick = leaf(11, name="CYTO-BRICK1")
    spacing = Spacing()
    a, b, c = ghost("G-V1", 1), ghost("G-V5", 22222), ghost("G-V3", 3)
    first = layout_forest(Forest(bricks=(brick,), ghosts=(a, b, c)), measure, spacing=spacing)
    assert [t.root for t in first.trees] == ["CYTO-BRICK1", "G-V1", "G-V5", "G-V3"]
    # G-V5 leaves; a narrow new ghost fills its slot, a wide one goes
    # last, whatever the forest's order; G-V1 and G-V3 don't move.
    narrow, wide = ghost("G-V2", 4), ghost("G-V4", 444444444)
    f = Forest(bricks=(brick,), ghosts=(wide, narrow, c, a))
    second = layout_forest(f, measure, spacing=spacing, previous=first)
    check_layout(second, f, spacing)
    assert [t.root for t in second.trees] == ["CYTO-BRICK1", "G-V1", "G-V2", "G-V3", "G-V4"]
    for name in ("G-V1", "G-V3", "CYTO-BRICK1"):
        assert second.boxes[name] == first.boxes[name]
    assert second.boxes["G-V2"].x == first.boxes["G-V5"].x
    # The first ghost leaves: the rest stay (the gap is within the slack).
    f = Forest(bricks=(brick,), ghosts=(wide, c, narrow))
    third = layout_forest(f, measure, spacing=spacing, previous=second)
    check_layout(third, f, spacing)
    for name in ("G-V2", "G-V3", "G-V4"):
        assert third.boxes[name] == second.boxes[name]


def test_ghosts_dont_follow_the_live_trees():
    """The ghosts' band is apart: live trees growing or shrinking don't
    move it."""
    g1, g2 = ghost("G-V1"), ghost("G-V2")
    narrow = leaf(11, name="CYTO-BRICK1")
    wide = node(1234567890, op("+", leaf(1), leaf(2)), name="CYTO-BRICK1")
    first = layout_forest(Forest(bricks=(narrow,), ghosts=(g1, g2)), measure)
    second = layout_forest(Forest(bricks=(wide,), ghosts=(g1, g2)), measure, previous=first)
    third = layout_forest(Forest(bricks=(narrow,), ghosts=(g1, g2)), measure, previous=second)
    for name in ("G-V1", "G-V2"):
        assert first.boxes[name] == second.boxes[name] == third.boxes[name]


def test_a_ghost_never_moves():
    """Even when the ghosts before it leave a gap: ghosts are gone after
    the view's window, and new ones fill the gaps."""
    spacing = Spacing(slack=30)
    old, g1, g2 = ghost("G-V0", 123456789), ghost("G-V1"), ghost("G-V2")
    first = layout_forest(Forest(ghosts=(old, g1, g2)), measure, spacing=spacing)
    f = Forest(ghosts=(g1, g2))
    second = layout_forest(f, measure, spacing=spacing, previous=first)
    check_layout(second, f, spacing)
    for name in ("G-V1", "G-V2"):
        assert second.boxes[name] == first.boxes[name]
    # A new ghost fills the gap.
    f = Forest(ghosts=(g1, g2, ghost("G-V3")))
    third = layout_forest(f, measure, spacing=spacing, previous=second)
    check_layout(third, f, spacing)
    assert third.boxes["G-V3"].x == 0


def test_tree_layout_remembers_the_previous_layout():
    big = node(120, op("x", leaf(6), leaf(20)), name="B1")
    small = leaf(120, "4bl", "B1")
    brick = leaf(11, name="CYTO-BRICK1")
    layouter = TreeLayout(measure)
    first = layouter.layout(Forest(blocks=(big,), bricks=(brick,)))
    second = layouter.layout(Forest(blocks=(small,), bricks=(brick,)))
    assert second.boxes["CYTO-BRICK1"] == first.boxes["CYTO-BRICK1"]
    assert layouter.previous is second
    layouter.reset()
    assert layouter.previous is None


def test_tree_layout_measures_each_label_once():
    calls = []

    def counting(text):
        calls.append(text)
        return measure(text)

    layouter = TreeLayout(counting)
    f = Forest(bricks=(leaf(11), leaf(11), leaf(3)))
    layouter.layout(f)
    layouter.layout(f)
    assert sorted(calls) == ["11", "3"]


# ---------------------------------------------------------------------------
# Recorded runs

def record(problem, seed, cap=20000):
    evs = []

    class Rec:
        def on_event(self, event):
            evs.append(event)

    harness.run_config(list(problem), seed=seed, max_iterations=cap, out=io.StringIO(),
                       observers=[Rec()])
    return evs


GHOST_WINDOW = 50


def shape(tree):
    """What a tree's layout depends on: names, labels and structure."""
    return (tree.name, view_label(tree), tuple(shape(c) for c in tree.children))


class Movement:
    """Displacement of unchanged trees between consecutive layouts, for
    live trees and for ghosts apart (ghosts fade out first in, first out,
    so their strip changes at nearly every kill)."""

    def __init__(self):
        self.steps = 0          # consecutive pairs of layouts
        self.moved = {"live": 0, "ghosts": 0}       # pairs where an unchanged tree moved
        self.total = {"live": 0.0, "ghosts": 0.0}   # sum over pairs of the largest move
        self.largest = {"live": 0.0, "ghosts": 0.0}
        self.prefix_moved = 0   # live unchanged trees, after an unchanged prefix, that moved

    def add(self, f0, l0, f1, l1):
        # A tree is unchanged if it has the same shape and is still live,
        # or still a ghost (a killed node moving to the ghosts is a change).
        ghosts0 = {g.name for g in f0.ghosts}
        before = {t.name: (t.name in ghosts0, shape(t)) for t in f0.trees()}
        order0 = [shape(t) for t in f0.trees() if t.name not in ghosts0]
        ghosts = {g.name for g in f1.ghosts}
        prefix = True
        worst = {"live": 0.0, "ghosts": 0.0}
        for i, t in enumerate(f1.trees()):
            s = shape(t)
            kind = "ghosts" if t.name in ghosts else "live"
            prefix = prefix and kind == "live" and i < len(order0) and order0[i] == s
            if before.get(t.name) != (kind == "ghosts", s):
                continue
            move = max(abs(l1.boxes[n.name].cx - l0.boxes[n.name].cx)
                       + abs(l1.boxes[n.name].cy - l0.boxes[n.name].cy) for n in t.walk())
            if prefix and move:
                self.prefix_moved += 1
            worst[kind] = max(worst[kind], move)
        self.steps += 1
        for kind, move in worst.items():
            self.moved[kind] += move > 0
            self.total[kind] += move
            self.largest[kind] = max(self.largest[kind], move)

    def report(self, kind):
        return (f"{kind}: moved in {self.moved[kind]} of {self.steps}, mean "
                f"{self.total[kind] / self.steps:.2f} px, largest {self.largest[kind]:.0f} px")


def replay(evs, stable=True):
    """Replays EVS into a model; after every event lays out the view's
    forest and checks it; returns the movement of unchanged trees."""
    model = TreeModel()
    layouter = TreeLayout(measure, label=view_label)
    movement = Movement()
    prev = None
    for event in evs:
        model.on_event(event)
        forest = model.forest(ghost_window=GHOST_WINDOW)
        if not stable:
            layouter.reset()
        lay = layouter.layout(forest)
        assert overlaps(lay) == [], event
        if prev is not None:
            movement.add(prev[0], prev[1], forest, lay)
        prev = forest, lay
    return movement, prev


STABILITY_RUNS = [(1, 1), (3, 8), (2, 1), (6, 1), (10, 1)]


@pytest.fixture(scope="module")
def recorded():
    return {run: record(PUZZLES[run[0] - 1], run[1]) for run in STABILITY_RUNS}


@pytest.mark.parametrize("run", STABILITY_RUNS)
def test_recorded_runs_are_laid_out_without_overlaps_and_stably(recorded, run):
    evs = recorded[run]
    stable, (forest, lay) = replay(evs)
    naive, _ = replay(evs, stable=False)
    # The final layout passes every check, and is deterministic.
    check_layout(lay, forest, size=lambda n: measure(view_label(n)))
    assert TreeLayout(measure, label=view_label).layout(forest) == \
        layout_forest(forest, measure, label=view_label)
    # Unchanged trees before every changed tree never move.
    assert stable.prefix_moved == 0
    # Measured bounds (see PROGRESS.md, iteration 3).
    print(f"\n{run}, {len(evs)} events:")
    for kind in ("live", "ghosts"):
        print(f"  {stable.report(kind)}\n    naive {naive.report(kind)}")
        assert stable.moved[kind] <= naive.moved[kind]
        assert stable.total[kind] <= naive.total[kind]
        assert stable.moved[kind] / stable.steps <= MAX_MOVED_FRACTION[kind]
        assert stable.total[kind] / stable.steps <= MAX_MEAN_MOVE[kind]


# Measured on the 5 runs (iteration 3): live trees moved in at most 2.5% of
# the events (6 of 240, puzzle 1 seed 1), mean at most 0.78 px an event;
# without the previous layout, 3.8% and 2.22 px.  A ghost never moves.
MAX_MOVED_FRACTION = {"live": 0.03, "ghosts": 0.0}
MAX_MEAN_MOVE = {"live": 1.0, "ghosts": 0.0}   # px


# ---------------------------------------------------------------------------
# loop0003 item 11: an unchanged forest keeps its layout

@pytest.mark.parametrize("run", [(1, 1), (3, 8), (10, 1)])
def test_the_layout_of_an_unchanged_forest_is_a_fixed_point(recorded, run):
    """Laying out a forest again from its own layout gives the same layout,
    at every event of a recorded run: so TreeLayout may keep the layout of a
    forest it has just laid out, and the chain of layouts is unchanged."""
    model = TreeModel()
    prev = None
    for event in recorded[run]:
        model.on_event(event)
        forest = model.forest(ghost_window=GHOST_WINDOW)
        lay = layout_forest(forest, measure, view_label, previous=prev)
        assert layout_forest(forest, measure, view_label, previous=lay) == lay, event
        prev = lay


def test_tree_layout_keeps_the_layout_of_the_same_forest(monkeypatch):
    import numbo.models.tree_layout as tl
    calls = []
    real = tl.layout_forest
    monkeypatch.setattr(tl, "layout_forest", lambda *a, **k: calls.append(1) or real(*a, **k))
    layouter = TreeLayout(measure)
    f = Forest(bricks=(leaf(11, name="A"), leaf(3, name="B")))
    first = layouter.layout(f)
    assert layouter.layout(f) is first and len(calls) == 1
    # An equal but new forest, a reset, or a previous layout set from outside
    # (a seek) is laid out again.
    assert layouter.layout(Forest(bricks=(leaf(11, name="A"), leaf(3, name="B")))) == first
    assert len(calls) == 2
    other = layouter.layout(Forest(bricks=(leaf(7, name="C"),)))
    layouter.previous = first
    assert layouter.layout(f) == first and len(calls) == 4
    layouter.reset()
    layouter.layout(f)
    assert len(calls) == 5 and other is not first


@pytest.mark.parametrize("run", [(1, 1), (6, 1)])
def test_the_kept_layouts_are_the_chain_of_fresh_ones(recorded, run):
    """TreeLayout (which keeps the layout of the same forest object) gives,
    at every event, the layout a chain of fresh layout_forest calls gives."""
    model = TreeModel()
    layouter = TreeLayout(measure, label=view_label)
    prev = None
    kept = 0
    for event in recorded[run]:
        model.on_event(event)
        forest = model.forest(ghost_window=GHOST_WINDOW)
        before = layouter.previous
        lay = layouter.layout(forest)
        kept += lay is before
        prev = layout_forest(forest, measure, view_label, previous=prev)
        assert lay == prev, event
    # Most events change nothing in the trees.
    assert kept > len(recorded[run]) // 2
