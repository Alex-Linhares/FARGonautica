"""The Categories list (lib/SGUI/List/Categories.pm): one row per category that some
workspace object belongs to, with a picture of where its instances are.

GetItemList walks the groups sorted by span (``rikeysort``, longest first, stable), then the
elements, and pushes each object's edges onto ``%Cat2Objects`` for each of its categories;
the categories come out in hash order (``values %VivifyCats``). The port lists them in the
snapshot's ``categories`` order (first seen); the tests put the snapshot in Perl's order.

DrawOneItem draws the category's name (anchored w, right of the picture), a grey
WidthForImage × HeightForImage rectangle, a blue oval per instance (wider and taller with its
span) and a 2 px square per element, all relative to the row's top left. PrepareForDrawing
sizes them from ``$SWorkspace::ElementCount``: SpacePerElement = WidthForImage /
(ElementCount + 1), GroupHtPerUnitSpan = (MaxGpHeight − MinGpHeight) / (ElementCount || 1).

Rows are tagged ``cat<cid>``. The popup's ActionButtons (``seqsee.gui.listactions``, on
live objects) are DeleteAllOther
(delete every group not kept by ``groups_to_keep``, i.e. MarkDescendentsToKeep from each
group of the category) and AddBarlinesBefore.
"""
import dataclasses

from .. import ops
from . import MARGIN, draw_list

NAME = "SGUI::List::Categories"


@dataclasses.dataclass(frozen=True)
class Layout:
    """SGUI::List::Categories->new's fields (not in a config file)."""
    height_per_row: float = 40
    height_for_image: float = 35
    width_for_image: float = 300
    max_gp_height: float = 15
    min_gp_height: float = 5


LAYOUT = Layout()


@dataclasses.dataclass(frozen=True)
class CategoryEntry:
    """A row: the category and its ``%Cat2Objects`` entry, the (left, right) edges of its
    instances in drawing order."""
    category: object        # CategorySnap
    edges: tuple


def item_list(snap):
    """GetItemList: a CategoryEntry per category with at least one instance."""
    objects = sorted(snap.groups, key=lambda g: -int(g.span)) + list(snap.elements)
    cat2objects = {}
    for obj in objects:
        for cid in obj.category_ids:
            cat2objects.setdefault(cid, []).append((obj.left, obj.right))
    return tuple(CategoryEntry(c, tuple(cat2objects[c.cid]))
                 for c in snap.categories if c.cid in cat2objects)


def draw_one(left, top, entry, element_count, layout=LAYOUT):
    """DrawOneItem (after PrepareForDrawing) for one CategoryEntry."""
    if entry.category.name is None:
        raise ops.DrawDied('Can\'t call method "get_name" on an undefined value', [])
    width, height = layout.width_for_image, layout.height_for_image
    space = width / (element_count + 1)
    ht_per_span = (layout.max_gp_height - layout.min_gp_height) / (element_count or 1)
    name = entry.category.name
    out = [ops.text(left + width + 5, top + 0.5 * height, anchor="w", text=name)
           if name else ops.text(left + width + 5, top + 0.5 * height, anchor="w"),
           ops.rectangle(left, top, left + width, top + height, fill="#EEEEEE")]
    center_y = top + height / 2
    for l, r in entry.edges:
        half = layout.min_gp_height + (r - l + 1) * ht_per_span
        out.append(ops.oval(left + space * (l + 0.8), center_y - half,
                            left + space * (r + 1.2), center_y + half, fill="#0000FF"))
    sq_left, sq_right = left + space - 1, left + space + 1
    for _ in range(element_count):
        out.append(ops.rectangle(sq_left, center_y - 1, sq_right, center_y + 1))
        sq_left += space
        sq_right += space
    return out


def _tag(entry):
    return "cat%d" % entry.category.cid


def draw_layers(snap, x, y, w, h, page=0, layout=LAYOUT, name=NAME):
    """Setup(canvas, x, y, w, h); PageNumber = page; DrawIt, as ``(lowered, others, state)``."""
    count = snap.element_count
    return draw_list(item_list(snap),
                     lambda left, top, e: draw_one(left, top, e, count, layout),
                     x, y, w, h, name, layout.height_per_row, page=page, tag_of=_tag,
                     margin=MARGIN)


def draw(snap, x, y, w, h, page=0, layout=LAYOUT, name=NAME):
    """The list of draw ops for page ``page``."""
    lowered, out, _ = draw_layers(snap, x, y, w, h, page, layout, name)
    return lowered + out


def mark_descendents_to_keep(snap, oid, keep=None):
    """MarkDescendentsToKeep(\\%Keep, $group): the oids of the group and of every group
    under it, at any depth; elements are not marked. Objects outside the workspace (no
    snapshot) are skipped."""
    keep = set() if keep is None else keep
    obj = snap.obj(oid)
    if obj is None or obj.is_element:
        return keep
    keep.add(oid)
    for item in obj.items:
        mark_descendents_to_keep(snap, item, keep)
    return keep


def groups_to_keep(snap, cid):
    """DeleteAllOther's %Keep for the category ``cid``: every group of the category and its
    descendents (the action deletes all the other groups)."""
    keep = set()
    for g in snap.groups:
        if cid in g.category_ids:
            mark_descendents_to_keep(snap, g.oid, keep)
    return keep
