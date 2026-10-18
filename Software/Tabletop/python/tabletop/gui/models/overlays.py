"""The table's overlays (PANES.md "P1 overlays"; G-024 to G-026), Qt-free: the nine
blurry regions with their names and each noticed object's last drawn region (D-004), the
salience heat map with the nine terms of a hovered item (D-031, D-100), the nine regions ranked
from the touched object (D-032), the roles layer (G-061: the standing role tags as badges
on the objects and groups, the midline band and the gaps, the focused object's mirror and
point images, the re-cast's candidates by tier, the barriers with the counterparts they
claim, and the axis; read from a composed `models/roles.py` model, so exact at every
event, D-079, D-105), and Eliza's view, the whole table turned by 180 degrees (D-005). **GUI design, not French's**: the sources describe the regions, the salience and
the region preference in prose [F95 pp. 65-67] and show no picture of them (ANSWERS.md 4).

What the prose gives: "the boundaries between regions are intentionally blurry" [F95
p. 65], so the cells are drawn with soft bands along their boundaries, as wide as the
blur of D-004 (`Config.region_blur_width`, read from `RunStarted`'s profile and changed
fields); the salience factors in French's order [F95 pp. 66-67] (D-031, `salience.terms`),
which the hover text lists; and the regions "diagonally opposite the touched object" and
"directly opposite" it preferred [F95 p. 66] (D-032, `salience.region_order`), which the
ranks show. The engine has no notion of a side beyond the regions (D-005), so Eliza's
view is the interface's own transform: `eliza_view(layout)` maps every point (x, y) to
(width - x, height - y), turns every icon round, and writes Eliza's name above the table
and Henry's below; the regions keep the engine's names.

The model is fed as every pane model is (PANES.md section 1): `set_problem` with the
run's `Problem`, `on_event` with every event (`RunStarted` for the blur, `ObjectNoticed`
for the first drawn region), and `update(snapshot, salience)` with the snapshot in force
and the salience readings the tap recorded beside it (`models/tap.py`'s
`salience_readings`; G-024). The saliences, the terms, the ranks and the later looks at a
region live only in the snapshot and its readings, so between keyframes they are dated
"as of codelet N" (G-007); an older snapshot never rolls them back (its clock is
checked). `layout(boxes)` gives the `Overlays` as plain data for the view: the cells
(with their names, drawn objects and ranks as the layers ask), the heat cells and the
note; `hover_lines(item)` the lines the table's hover text adds (the last drawn region,
the nine terms); the `layers` dictionary says which overlays are on (Seek-Whence GUI,
`cytoplasm_view.py`'s layers).
"""

import dataclasses
from dataclasses import dataclass

from tabletop import events as ev
from tabletop import geometry as G
from tabletop import roles as R
from tabletop.config import PROFILES
from tabletop.gui.models.roles import RolesModel, code_of
from tabletop.salience import FACTORS

__all__ = ["BlockMark", "FLIP", "GapMark", "ImageMark", "LAYERS", "LAYER_TITLES",
           "NOT_FRENCHS", "HeatCell", "Overlays", "OverlaysModel", "RegionCell",
           "RoleBadge", "TierMark", "blur_width_of", "eliza_view", "flip_box",
           "flip_point", "region_cells"]

NOT_FRENCHS = "GUI design, not French's"

# The three overlay layers, in the order the bar shows them (G-025).
LAYERS = ("regions", "salience", "ranks", "roles")
LAYER_TITLES = {"regions": "Regions", "salience": "Salience", "ranks": "Region ranks",
                "roles": "Roles"}

# Turning an icon round for Eliza's view (G-026): up and down, left and right swap.
FLIP = {"up": "down", "down": "up", "left": "right", "right": "left", None: None}

# The blur of a region boundary in `standard` (D-004), until a run says otherwise.
DEFAULT_BLUR = float(PROFILES["standard"].region_blur_width)


@dataclass(frozen=True)
class RegionCell:
    """One of the nine regions as drawn (D-004; G-025): its engine `name`, `column` and
    `row` (0 to 2, as `geometry.Region`), its `box` in table units, the ids of the
    noticed objects whose last drawn region it is (`drawn`, when the regions layer is
    on), and its `rank` from the touched object with its preference `weight` (1 is the
    most preferred; None when the ranks layer is off or nothing has been read)."""

    name: str
    column: int
    row: int
    box: tuple
    drawn: tuple = ()
    rank: int | None = None
    weight: float | None = None


@dataclass(frozen=True)
class HeatCell:
    """One item's salience as a tint over its box (D-031; G-025): the `item` (an object
    id or a group name), its `box`, the `value`, and its `share` of the largest salience
    in view, 0 to 1."""

    item: str
    box: tuple
    value: float
    share: float


@dataclass(frozen=True)
class RoleBadge:
    """The standing roles of an item as a badge on its box (G-061): the `item` (an object
    id or a group name), its `box`, and the `codes`, one (code, role, args) per standing
    tag in the order tagged (D-079's codes: `G1`, `A2`, ...)."""

    item: str
    box: tuple
    codes: tuple


@dataclass(frozen=True)
class GapMark:
    """A gap (S2, D-086) as drawn: the `row` word, the `box` of the white space between
    the two clusters `a` and `b` it parts (G-061)."""

    row: str
    box: tuple
    a: str
    b: str


@dataclass(frozen=True)
class ImageMark:
    """The mirror or point image of the focused object (G1, G2; D-082): the `kind`
    (`mirror` or `point`), the focused object `source` and its image `image`, with their
    boxes (G-061)."""

    kind: str
    source: str
    image: str
    source_box: tuple
    image_box: tuple


@dataclass(frozen=True)
class TierMark:
    """A candidate of the re-cast (K1, D-093): the `candidate`, its `box`, the `tier`
    word (`tier1` to `tier5`) and its `weight` (G-061)."""

    candidate: str
    box: tuple
    tier: str
    weight: float


@dataclass(frozen=True)
class BlockMark:
    """A barrier and the counterpart it claims (S5, S6; D-089): the `barrier` object,
    the `claimed` one, their boxes and the `correspondence` through which the claim is
    made (G-061)."""

    barrier: str
    claimed: str
    barrier_box: tuple
    claimed_box: tuple
    correspondence: str


@dataclass(frozen=True)
class Overlays:
    """The overlays as plain data (G-025): the `regions` (nine `RegionCell`s in reading
    order when the regions or the ranks layer is on, else empty), whether their
    `names_shown` and `ranks_shown`, the `blur` width of the boundaries (table units),
    the `heat` cells (when the salience layer is on), `as_of`, the clock of the snapshot
    the saliences, the ranks and the later looks come from (None before the first), and
    a `note` for the legend; and, when the roles layer is on (G-061), `roles_shown`, the
    `badges`, the `midline` band's box (None while no MIDLINE stands), the `gaps`, the
    `images` of the focused object, the re-cast's `tiers`, the `blocks` and the `axis`
    word (empty while none stands)."""

    regions: tuple = ()
    names_shown: bool = False
    ranks_shown: bool = False
    blur: float = DEFAULT_BLUR
    heat: tuple = ()
    as_of: int | None = None
    note: str = ""
    roles_shown: bool = False
    badges: tuple = ()
    midline: tuple | None = None
    gaps: tuple = ()
    images: tuple = ()
    tiers: tuple = ()
    blocks: tuple = ()
    axis: str = ""

    @property
    def empty(self) -> bool:
        """Whether nothing is to be drawn (every layer off) (G-025, G-061)."""
        return not self.regions and not self.heat and not self.roles_shown


def region_cells(width: float, height: float) -> tuple:
    """The nine regions as cells tiling the table, in `geometry.REGIONS`'s reading order:
    the thirds of each axis (D-004; G-025)."""
    out = []
    for region in G.REGIONS:
        left, top = region.column * width / 3, region.row * height / 3
        out.append(RegionCell(region.name, region.column, region.row,
                              (left, top, left + width / 3, top + height / 3)))
    return tuple(out)


def blur_width_of(event) -> float:
    """The blur of D-004 for the run a `RunStarted` begins: `region_blur_width` among the
    fields `changed` from `standard`, else the profile's (G-025)."""
    changed = dict(event.changed)
    if "region_blur_width" in changed:
        return float(changed["region_blur_width"])
    profile = PROFILES.get(event.profile, PROFILES["standard"])
    return float(profile.region_blur_width)


class OverlaysModel:
    """The overlays' state (see the module docstring; G-024, G-025): the `layers` on,
    the table's size, the `blur`, the last drawn `regions` by id, the `saliences` and
    `terms` by item, the `region_order` and the `snapshot_clock` they are dated by."""

    def __init__(self):
        self.layers = {name: False for name in LAYERS}
        self.width, self.height = 110.0, 100.0
        self.blur = DEFAULT_BLUR
        self.roles = RolesModel()
        self.focus = None
        self.clear_run()

    def clear_run(self) -> None:
        """Forget the run, keep the layers and the table (G-025)."""
        self.regions = {}
        self.saliences = {}
        self.terms = {}
        self.region_order = ()
        self.snapshot_clock = None
        self.roles.clear()

    def set_problem(self, problem) -> None:
        """The run's `Problem`: its size (G-017)."""
        self.width, self.height = float(problem.width), float(problem.height)
        self.clear_run()

    def on_event(self, event) -> None:
        """Apply one event (G-025): the start sets the blur and clears the run; a notice
        gives the object's first drawn region (D-004)."""
        if isinstance(event, ev.RunStarted):
            self.clear_run()
            self.blur = blur_width_of(event)
        elif isinstance(event, ev.ObjectNoticed):
            self.regions[event.object] = event.region
        self.roles.on_event(event)

    def update(self, snapshot, salience=None) -> None:
        """The snapshot in force with the salience readings recorded beside it (G-024):
        the noticed objects' last drawn regions, the saliences, the terms and the region
        order, dated by the snapshot's clock; a snapshot older than the last one applied
        changes nothing."""
        if snapshot is None:
            return
        if self.snapshot_clock is not None and snapshot.clock < self.snapshot_clock:
            return
        self.snapshot_clock = snapshot.clock
        for o in snapshot.table:
            if o["noticed"] and o["region"] is not None:
                self.regions[o["id"]] = o["region"]
        self.saliences = {item: float(value) for item, value in snapshot.saliences}
        if salience:
            self.terms = dict(salience.get("terms", {}))
            self.region_order = tuple(salience.get("region_order", ()))

    def region_of(self, id_: str) -> str | None:
        """The last drawn region of an object, or None before it was noticed (D-004)."""
        return self.regions.get(id_)

    def _cells(self) -> tuple:
        """The nine cells with what the layers on ask for (G-025)."""
        ranks = {name: (k + 1, weight) for k, (name, weight) in enumerate(self.region_order)}
        drawn = {}
        for id_, name in self.regions.items():
            drawn.setdefault(name, []).append(id_)
        out = []
        for cell in region_cells(self.width, self.height):
            rank, weight = ranks.get(cell.name, (None, None)) if self.layers["ranks"] else (None, None)
            ids = tuple(drawn.get(cell.name, ())) if self.layers["regions"] else ()
            out.append(dataclasses.replace(cell, drawn=ids, rank=rank, weight=weight))
        return tuple(out)

    def _heat(self, boxes: dict) -> tuple:
        """The heat cells of the items in view that have a box (G-025)."""
        values = {item: v for item, v in self.saliences.items() if item in boxes}
        top = max(values.values(), default=0.0)
        return tuple(HeatCell(item, boxes[item], v, v / top if top > 0 else 0.0)
                     for item, v in values.items())

    def _note(self) -> str:
        parts = []
        if self.layers["regions"]:
            parts.append(f"regions (blur {self.blur:g})")
        if self.layers["salience"]:
            parts.append("salience" + (" (none yet)" if not self.saliences else ""))
        if self.layers["ranks"]:
            parts.append("ranks" + (" (not read yet)" if not self.region_order else ""))
        if self.layers["roles"]:
            standing = len(self.roles.standing())
            parts.append(f"roles ({standing} standing)" if standing else "roles (none read yet)")
        if not parts:
            return ""
        dated = f" as of codelet {self.snapshot_clock}" if self.snapshot_clock is not None else ""
        return "overlays: " + ", ".join(parts) + dated + f"; {NOT_FRENCHS}"

    # -- the roles layer (G-061) -------------------------------------------------------------

    def _badges(self, boxes: dict) -> tuple:
        """One badge per item in view with a standing tag, its codes in the order tagged
        (G-061)."""
        out = []
        for item in self.roles.items():
            tags = self.roles.tags_of(item)
            if tags and item in boxes:
                out.append(RoleBadge(item, boxes[item],
                                     tuple((code_of(t.role), t.role, t.args) for t in tags)))
        return tuple(out)

    def _gaps(self, boxes: dict) -> tuple:
        """The standing gaps (S2) as the white space between their clusters' boxes along
        the row (G-061)."""
        out = []
        for tag in self.roles.tags_of(R.TABLE):
            if tag.role != R.GAP or len(tag.args) < 3:
                continue
            row, a, b = tag.args[:3]
            if a not in boxes or b not in boxes:
                continue
            first, second = sorted((boxes[a], boxes[b]), key=lambda box: box[0])
            top = min(first[1], second[1])
            bottom = max(first[3], second[3])
            out.append(GapMark(row, (first[2], top, second[0], bottom), a, b))
        return tuple(out)

    def _images(self, boxes: dict) -> tuple:
        """The focused object's mirror and point images (G1, G2): the touched object's
        while nothing is selected (G-061)."""
        source = self.focus if self.focus in boxes else self.roles.touched
        if source not in boxes:
            return ()
        out = []
        for tag in self.roles.tags_of(source):
            if tag.role in (R.MIRROR_IMAGE, R.POINT_IMAGE) and tag.args and tag.args[0] in boxes:
                kind = "mirror" if tag.role == R.MIRROR_IMAGE else "point"
                out.append(ImageMark(kind, source, tag.args[0], boxes[source],
                                     boxes[tag.args[0]]))
        return tuple(out)

    def _tiers(self, boxes: dict) -> tuple:
        """The re-cast's candidates by tier, from their standing COUNTERPART tags (K1;
        D-093) (G-061)."""
        out = []
        for tag in self.roles.standing():
            if tag.role == R.COUNTERPART and len(tag.args) >= 2 and tag.item in boxes:
                weights = {R.TIER_WORDS[t]: w for t, _c, w in (self.roles.recast or {}).get("tiers", ())}
                out.append(TierMark(tag.item, boxes[tag.item], tag.args[1],
                                    float(weights.get(tag.args[1], 0.0))))
        return tuple(out)

    def _blocks(self, boxes: dict) -> tuple:
        """The standing barriers with the counterparts they claim (S5, S6; D-089)
        (G-061)."""
        out = []
        for tag in self.roles.standing():
            if tag.role == R.BARRIER and len(tag.args) >= 2 and tag.item in boxes \
                    and tag.args[0] in boxes:
                out.append(BlockMark(tag.item, tag.args[0], boxes[tag.item], boxes[tag.args[0]],
                                     str(tag.args[1])))
        return tuple(out)

    def _midline(self):
        """The middle band's box while a MIDLINE stands on the table (S1; D-084), else
        None (G-061)."""
        if any(t.role == R.MIDLINE for t in self.roles.tags_of(R.TABLE)):
            return (0.0, self.height / 3, self.width, 2 * self.height / 3)
        return None

    def _role_overlays(self, boxes: dict) -> dict:
        axis = self.roles.axis()
        return {"roles_shown": True, "badges": self._badges(boxes), "midline": self._midline(),
                "gaps": self._gaps(boxes), "images": self._images(boxes),
                "tiers": self._tiers(boxes), "blocks": self._blocks(boxes),
                "axis": str(axis.args[0]) if axis is not None and axis.args else ""}

    def layout(self, boxes: dict) -> Overlays:
        """The overlays as plain data for the layers on (G-025, G-061); `boxes` are the
        boxes of the objects and the groups by name, for the heat cells and the role
        marks."""
        grid = self.layers["regions"] or self.layers["ranks"]
        if not grid and not self.layers["salience"] and not self.layers["roles"]:
            return Overlays(blur=self.blur)       # every layer off: nothing dated, nothing drawn
        roles = self._role_overlays(boxes) if self.layers["roles"] else {}
        return Overlays(
            regions=self._cells() if grid else (),
            names_shown=self.layers["regions"], ranks_shown=self.layers["ranks"],
            blur=self.blur, heat=self._heat(boxes) if self.layers["salience"] else (),
            as_of=self.snapshot_clock, note=self._note(), **roles)

    def role_lines(self, item: str) -> list:
        """The hover text's lines for an item's roles (D-079; G-061): each standing tag
        with its readers, then the count of past ones; empty when it carries none."""
        tags = self.roles.tags_of(item)
        past = [t for t in self.roles.history_of(item) if not t.standing]
        if not tags and not past:
            return []
        lines = [f"roles: {len(tags)} standing, {len(past)} past"]
        for t in tags:
            args = f" ({', '.join(str(a) for a in t.args)})" if t.args else ""
            readers = ", ".join(t.consumers) or "no consumer yet"
            lines.append(f"  {code_of(t.role)} {t.role}{args} w{t.weight} s{t.strength:.0f}: "
                         f"{t.evidence}; read by {readers}")
        return lines

    def salience_lines(self, item: str) -> list:
        """The hover text's lines for an item's salience (D-031; G-025): its value and
        the nine terms, French's eight in his order and the role term (D-100), dated; empty
        when nothing was read."""
        if item not in self.saliences:
            return []
        dated = f" as of codelet {self.snapshot_clock}" if self.snapshot_clock is not None else ""
        lines = [f"salience {self.saliences[item]:.1f}{dated}"]
        terms = dict(self.terms.get(item, ()))
        if terms:
            lines.append("  " + " · ".join(f"{name} {terms.get(name, 0.0):+.1f}"
                                            for name in FACTORS))
        return lines

    def hover_lines(self, item: str, first_region=None) -> list:
        """What the overlays add to an element's hover text (G-025): the last drawn
        region when it differs from the first notice's, and, with the salience layer on,
        the salience and its terms."""
        lines = []
        last = self.regions.get(item)
        if last is not None and first_region is not None and last != first_region:
            dated = (f" (as of codelet {self.snapshot_clock})"
                     if self.snapshot_clock is not None else "")
            lines.append(f"last drawn region: {last}{dated}")
        if self.layers["salience"]:
            lines += self.salience_lines(item)
        if self.layers["roles"]:
            lines += self.role_lines(item)
        return lines


# ---------------------------------------------------------------------------------------------
# Eliza's view (G-026).


def flip_point(point: tuple, width: float, height: float) -> tuple:
    """(x, y) seen from Eliza's side: (width - x, height - y) (G-026; D-005)."""
    return (width - point[0], height - point[1])


def flip_box(box: tuple, width: float, height: float) -> tuple:
    """A box (left, top, right, bottom) seen from Eliza's side (G-026)."""
    left, top, right, bottom = box
    return (width - right, height - bottom, width - left, height - top)


def _flip_overlays(ov: Overlays, width: float, height: float) -> Overlays:
    def box(b):
        return flip_box(b, width, height)

    regions = tuple(dataclasses.replace(c, box=box(c.box)) for c in ov.regions)
    heat = tuple(dataclasses.replace(h, box=box(h.box)) for h in ov.heat)
    badges = tuple(dataclasses.replace(b, box=box(b.box)) for b in ov.badges)
    gaps = tuple(dataclasses.replace(g, box=box(g.box)) for g in ov.gaps)
    images = tuple(dataclasses.replace(i, source_box=box(i.source_box), image_box=box(i.image_box))
                   for i in ov.images)
    tiers = tuple(dataclasses.replace(t, box=box(t.box)) for t in ov.tiers)
    blocks = tuple(dataclasses.replace(b, barrier_box=box(b.barrier_box),
                                       claimed_box=box(b.claimed_box)) for b in ov.blocks)
    midline = box(ov.midline) if ov.midline is not None else None
    return dataclasses.replace(ov, regions=regions, heat=heat, badges=badges, gaps=gaps,
                               images=images, tiers=tiers, blocks=blocks, midline=midline)


def eliza_view(lay):
    """The layout as Eliza sees it (G-026; D-005): every point mapped to (width - x,
    height - y), every icon turned round (`FLIP`), Eliza's name above the table and
    Henry's below, the arrows, the boxes, the lines and the overlays mapped likewise;
    the regions keep the engine's names. Applying it twice gives the layout back, up to
    the rounding of the subtractions."""
    w, h = lay.width, lay.height

    def point(p):
        return flip_point(p, w, h)

    def box(b):
        return flip_box(b, w, h)

    objects = tuple(dataclasses.replace(o, x=w - o.x, y=h - o.y, orientation=FLIP[o.orientation])
                    for o in lay.objects)
    names = tuple(dataclasses.replace(n, y=h - n.y) for n in lay.names)
    arrows = {}
    for key in ("touch", "answer"):
        a = getattr(lay, key)
        arrows[key] = None if a is None else dataclasses.replace(a, tail=point(a.tail),
                                                                 head=point(a.head))
    groups = tuple(dataclasses.replace(g, box=box(g.box)) for g in lay.groups)
    proposals = tuple(dataclasses.replace(p, box=box(p.box)) for p in lay.proposals)
    lines = tuple(dataclasses.replace(c, a=point(c.a), b=point(c.b),
                                      end_boxes=tuple(box(e) for e in c.end_boxes))
                  for c in lay.correspondences)
    return dataclasses.replace(
        lay, objects=objects, names=names, touch=arrows["touch"], answer=arrows["answer"],
        groups=groups, proposals=proposals, correspondences=lines,
        overlays=_flip_overlays(lay.overlays, w, h), eliza=not lay.eliza)
