"""Hover tooltips on workspace objects, and the details a list popup shows (not in Perl: its
canvas shows nothing on hover, and its popups hold only the action buttons).

Pure Python on snapshots. ``targets`` replays the Workspace (or Workspace_Attention) drawing
of a part and records where each object went: an element's text bbox, a group's oval, a
relation's curve. ``target_at`` picks the object under a point: an element first, then a
relation (within ``RELATION_HALO`` px of its curve), then the smallest group whose oval holds
the point. ``tooltip(snap, view, w, h, x, y)`` does it for the part of a composite view under
the point and describes the object (``describe_object``).

The Workspace_Attention part dies at its first relation (PERL-QUIRK, see
``draw.workspace_attention``), so its relations are never drawn and have no target.
"""
import dataclasses

from seqsee.util import perl_str

from .draw import views, workspace, workspace_attention
from .draw.lists import entry_for
from .draw.ops import DrawDied
from .draw.relations import fmt
from .draw.workspace import approx_measure, text_bbox

RELATION_HALO = 4       # px from a relation's curve
_CURVE_SAMPLES = 40


@dataclasses.dataclass(frozen=True)
class Target:
    kind: str           # 'element', 'group' or 'relation'
    oid: int
    coords: tuple       # element: text bbox; group: oval box; relation: the line's coords


def _recording(base):
    class Recorder(base):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.targets = []

        def draw_anchored(self, obj, is_largest=0):
            super().draw_anchored(obj, is_largest)
            self.targets.append(Target("group", obj.oid, tuple(self.out[-1].coords)))

        def draw_element(self, elt, idx, x, y):
            n = len(self.out)
            super().draw_element(elt, idx, x, y)
            op = self.out[n]
            self.targets.append(Target("element", elt.oid, text_bbox(
                x, y, perl_str(elt.mag if elt.mag is not None else ""), op.font,
                self.measure)))

        def draw_relation(self, rel):
            n = len(self.out)
            super().draw_relation(rel)
            if len(self.out) > n:
                self.targets.append(Target("relation", rel.oid, tuple(self.out[n].coords)))
    return Recorder


_WorkspaceRecorder = _recording(workspace._Drawer)
_AttentionRecorder = _recording(workspace_attention._AttentionDrawer)


def targets(snap, part, rect, measure=approx_measure):
    """The hover targets of workspace part ``part`` (views.WORKSPACE or views.ATTENTION)
    drawn in ``rect`` = (x, y, w, h), in drawing order."""
    if part == views.WORKSPACE:
        d = _WorkspaceRecorder(snap, workspace.setup(*rect, workspace.LAYOUT), measure)
    elif part == views.ATTENTION:
        d = _AttentionRecorder(snap, workspace.setup(*rect, workspace_attention.LAYOUT),
                               measure)
    else:
        return ()
    try:
        d.draw_it()
    except DrawDied:
        pass
    return tuple(d.targets)


def _in_box(box, x, y):
    x1, y1, x2, y2 = box
    return x1 <= x <= x2 and y1 <= y <= y2


def _in_oval(box, x, y):
    x1, y1, x2, y2 = box
    rx, ry = (x2 - x1) / 2, (y2 - y1) / 2
    if rx <= 0 or ry <= 0:
        return False
    dx, dy = (x - (x1 + x2) / 2) / rx, (y - (y1 + y2) / 2) / ry
    return dx * dx + dy * dy <= 1


def _curve(coords):
    """Points along a relation's line: Tk's smooth curve through three points is the
    quadratic Bézier with the middle one as control point."""
    pts = list(zip(coords[0::2], coords[1::2]))
    if len(pts) != 3:
        return pts
    (ax, ay), (bx, by), (cx, cy) = pts
    out = []
    for i in range(_CURVE_SAMPLES + 1):
        t = i / _CURVE_SAMPLES
        u = 1 - t
        out.append((u * u * ax + 2 * u * t * bx + t * t * cx,
                    u * u * ay + 2 * u * t * by + t * t * cy))
    return out


def _near_curve(coords, x, y, halo=RELATION_HALO):
    pts = _curve(coords)
    for (ax, ay), (bx, by) in zip(pts, pts[1:]):
        dx, dy = bx - ax, by - ay
        length2 = dx * dx + dy * dy
        t = 0 if length2 == 0 else max(0, min(1, ((x - ax) * dx + (y - ay) * dy) / length2))
        px, py = ax + t * dx - x, ay + t * dy - y
        if px * px + py * py <= halo * halo:
            return True
    return False


def target_at(tgts, x, y):
    """The target under (x, y): an element, else a relation, else the smallest group."""
    for t in tgts:
        if t.kind == "element" and _in_box(t.coords, x, y):
            return t
    for t in reversed(tgts):
        if t.kind == "relation" and _near_curve(t.coords, x, y):
            return t
    groups = [t for t in tgts if t.kind == "group" and _in_oval(t.coords, x, y)]
    if groups:
        return min(groups, key=lambda t: (t.coords[2] - t.coords[0]) * (t.coords[3] - t.coords[1]))
    return None


# ---- descriptions -------------------------------------------------------------------------
def _num(v):
    return fmt(v).strip()


def _cats(names):
    names = [n if n is not None else "(unnamed)" for n in names]
    return ", ".join(names) if names else "none"


def describe_object(snap, oid, attention=False):
    """A few lines about an element, group or relation of the snapshot ('' if none)."""
    obj = snap.obj(oid)
    if obj is None:
        return ""
    lines = []
    if hasattr(obj, "ends"):
        a, b = (perl_str(e).strip() for e in obj.end_bounds) if obj.end_bounds else ("?", "?")
        lines += [f"Relation {obj.type_text}", f"{a} → {b}", f"Strength: {_num(obj.strength)}"]
    elif obj.is_element:
        lines += [f"Element {perl_str(obj.mag)} (index {obj.index})",
                  f"Strength: {_num(obj.strength)}",
                  f"Categories: {_cats(obj.categories)}"]
        if obj.group_p:
            lines.append("Squinted as a group")
    else:
        lines += [f"Group {obj.bounds_string.strip()} {obj.structure_string}",
                  f"Span {perl_str(obj.span)}, strength {_num(obj.strength)}",
                  f"Categories: {_cats(obj.categories)}"]
        if obj.is_locked:
            lines.append("locked against deletion")
    if getattr(obj, "metonym_active", False):
        lines.append(f"Metonym: {obj.starred_structure_string}")
    if obj.hilit:
        lines.append(f"Highlighted ({obj.hilit})")
    if attention:
        lines.append(f"Codelet attention: {float(obj.attention):.3f}")
    return "\n".join(lines)


def describe_entry(snap, part, key):
    """What a list popup shows about the entry clicked ('' if it is gone)."""
    entry = entry_for(snap, part, key)
    if entry is None:
        return ""
    if part == views.GROUPS_LIST:
        return describe_object(snap, entry.oid)
    if part == views.CATEGORIES_LIST:
        n = len(entry.edges)
        spans = ", ".join(f"<{perl_str(l)}, {perl_str(r)}>" for l, r in entry.edges)
        name = entry.category.name if entry.category.name is not None else "(unnamed)"
        return f"Category {name}\n{n} instance{'' if n == 1 else 's'}: {spans}"
    if part == views.STREAM_LIST:
        index, thought = entry
        lines = [f"Thought {thought.text}" + (" (current)" if index == 0 else ""),
                 f"Hit intensity: {perl_str(thought.hit_intensity) or 0}"]
        if thought.fringe:
            lines.append("Fringe: " + "; ".join(
                f"[{perl_str(c.activation)}] {c.label}" for c in thought.fringe if c))
        return "\n".join(lines)
    return ""


def tooltip(snap, view, w, h, x, y, measure=approx_measure):
    """The tooltip for (x, y) on a ``w`` × ``h`` canvas showing ``view`` ('' if nothing)."""
    for part, *rect in reversed(views.part_rects(view, w, h)):
        if part not in (views.WORKSPACE, views.ATTENTION):
            continue
        rx, ry, rw, rh = rect
        if not (rx <= x <= rx + rw and ry <= y <= ry + rh):
            continue
        t = target_at(targets(snap, part, rect, measure), x, y)
        if t is not None:
            return describe_object(snap, t.oid, attention=part == views.ATTENTION)
    return ""
