"""The roles pane's model (P14; G-060), Qt-free: every role tag of a run as the events tell
it, current and past, with its evidence, its weight, the consumers that read it and the
event that discarded it; the re-cast and the axis; and the overlays the table draws from
the same tags (G-061).

A role is the function an item plays in the analogy (D-078, D-079): a tag is a Workspace
structure with a strength, proposed, rated, built and breakable, so an item's roles change
during a run (the author's answer D1 of the roles loop). The model is built from the four
role events alone (D-105): `RoleTagged` opens a `TagState`, `RoleDiscarded` closes it with
its reason (`lost to rN`, `broken`, `structure gone`) and the codelet that did it,
`RoleConsumed` adds a reading to every standing tag of that role on that item, and
`RoleRecast` keeps the last re-cast (D-093). So the model is exact at every event index,
seeking rebuilds it, and the snapshot is never needed: the standing tags equal
`Snapshot.roles` at every snapshot of a run (tested). Nothing here draws from the engine's
generator.
"""

from dataclasses import dataclass, field

from tabletop import events as ev
from tabletop import roles as R
from tabletop.gui.models.worldview import DETAIL, HEADING, MUTED, SHADOW, WORLDVIEW, TextLine

__all__ = ["FAMILY_TITLES", "NOT_FRENCHS", "RolesModel", "TagState", "code_of", "tag_text"]

NOT_FRENCHS = "GUI design, not French's"
# The five families (D-079) as the pane heads them.
FAMILY_TITLES = {R.GEOMETRIC: "Geometric (images, the axis)",
                 R.STRUCTURAL: "Structural (midline, gaps, container, connector, barrier)",
                 R.SELECTOR: "Selectors (row, column, corner, loner, stray)",
                 R.KEY: "Keys (counterpart, type key)",
                 R.CONTENT: "Content (member, sidekick, anchor, figure, ground, echo, twin)"}
CODES = {spec.name: spec.code for spec in R.REGISTRY}


def code_of(role: str) -> str:
    """The registry's short code of a role (`G1`, `S5`, ...; D-079), the role itself for
    an unknown one (G-060)."""
    return CODES.get(role, role)


@dataclass
class TagState:
    """One role tag as the events left it (G-060): the tag's `name`, `item`, `role`,
    `args`, `weight`, `strength` at tagging, `evidence`, `by` (the codelet type, or
    `perceived`), the codelet `built_at` and the event `built_index`; `consumed`, the
    readings as (consumer, effect, by, clock); and, once discarded, `discarded_at`,
    `discarded_index`, the `reason` and `discarded_by`."""

    name: str
    item: str
    role: str
    args: tuple
    weight: int
    strength: float
    evidence: str
    by: str
    built_at: int
    built_index: int
    consumed: list = field(default_factory=list)
    discarded_at: int | None = None
    discarded_index: int | None = None
    reason: str = ""
    discarded_by: str = ""

    @property
    def standing(self) -> bool:
        """Whether the tag is in the pool now (G-060)."""
        return self.discarded_at is None

    @property
    def consumers(self) -> tuple:
        """The consumers that read it, each once, in order of first reading (G-060)."""
        out = []
        for consumer, *_ in self.consumed:
            if consumer not in out:
                out.append(consumer)
        return tuple(out)


def tag_text(tag: TagState) -> str:
    """One tag on one line (G-060): `r7 cup2 POINT_IMAGE (cup1) w1 s42 by find-roles at
    codelet 9`, with its evidence after a dash."""
    args = f" ({', '.join(str(a) for a in tag.args)})" if tag.args else ""
    return (f"{tag.name} {tag.item} {tag.role}{args} w{tag.weight} s{tag.strength:.0f} "
            f"by {tag.by} at codelet {tag.built_at} - {tag.evidence}")


class RolesModel:
    """The roles of a run from the events alone (see the module docstring; G-060):
    `tags` by name in the order tagged, `recast` (the last `RoleRecast`'s fields, or
    None), `recasts` counted, `clock`, `events` and `index` (the event shown);
    `standing()`, `history_of(item)`, `tags_of(item)`, `axis()`, `families()`; and the
    pane's `lines()`, `tooltip(name)` and `selection_of(name)` for the lines widget."""

    def __init__(self):
        self.clear()

    def clear(self) -> None:
        """Forget the run (G-060)."""
        self.tags = {}
        self.recast = None
        self.recasts = 0
        self.clock = 0
        self.events = 0
        self.touched = None
        self.groups = {}

    @property
    def index(self) -> int:
        """The index of the last event taken, -1 before any (G-060)."""
        return self.events - 1

    # -- the events (D-105) ------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply one event: a tag, a discard, a reading, a re-cast; a group's members are
        kept for the selection; every other event only moves the clock (G-060)."""
        if isinstance(event, ev.RunStarted):
            self.clear()
            self.touched = event.touched
        index = self.events
        self.events += 1
        self.clock = event.clock
        if isinstance(event, ev.RoleTagged):
            self.tags[event.tag] = TagState(event.tag, event.item, event.role, tuple(event.args),
                                            event.weight, float(event.strength), event.evidence,
                                            event.by, event.clock, index)
        elif isinstance(event, ev.RoleDiscarded):
            tag = self.tags.get(event.tag)
            if tag is not None and tag.standing:
                tag.discarded_at, tag.discarded_index = event.clock, index
                tag.reason, tag.discarded_by = event.reason, event.by
        elif isinstance(event, ev.RoleConsumed):
            for tag in self._read_by(event):
                tag.consumed.append((event.consumer, event.effect, event.by, event.clock))
        elif isinstance(event, ev.RoleRecast):
            self.recasts += 1
            self.recast = {"touched": event.touched, "signature": tuple(event.signature),
                           "tiers": tuple(tuple(t) for t in event.tiers),
                           "ambiguous": event.ambiguous, "clock": event.clock, "index": index}
        elif isinstance(event, ev.GroupBuilt):
            self.groups[event.group] = tuple(event.members)

    def _read_by(self, event) -> list:
        """The standing tags a `RoleConsumed` is about: those of its item and role; when
        the item carries several readings of one role (an image per partner, D-082), the
        ones whose arguments the effect names (G-060)."""
        mine = [t for t in self.tags.values()
                if t.standing and t.item == event.item and t.role == event.role]
        if len(mine) > 1:
            words = set(event.effect.split())
            named = [t for t in mine if any(str(a) in words for a in t.args)]
            if named:
                return named
        return mine

    # -- the readings (G-060) ----------------------------------------------------------------

    def standing(self) -> list:
        """The tags in the pool now, in the order tagged (D-079)."""
        return [t for t in self.tags.values() if t.standing]

    def tags_of(self, item: str) -> list:
        """The standing tags of an item (G-060, G-061)."""
        return [t for t in self.tags.values() if t.standing and t.item == item]

    def history_of(self, item: str) -> list:
        """Every tag the item carried, current and past, in the order tagged (D1 of the
        roles loop's answers; G-060)."""
        return [t for t in self.tags.values() if t.item == item]

    def axis(self):
        """The standing symmetry axis tag (D-083), or None (G-060)."""
        tags = [t for t in self.standing() if t.role == R.SYMMETRY_AXIS]
        return tags[-1] if tags else None

    def families(self) -> dict:
        """The tags by family (D-079), standing first within each item, in the order
        tagged (G-060)."""
        out = {family: [] for family in R.FAMILIES}
        for tag in self.tags.values():
            out[R.spec(tag.role).family].append(tag)
        return out

    def items(self) -> list:
        """The items that carry or carried a tag, in the order first tagged (G-060)."""
        out = []
        for tag in self.tags.values():
            if tag.item not in out:
                out.append(tag.item)
        return out

    def banner(self) -> str:
        """The first line, in plain words (ANSWERS.md 11 of the GUI loop; G-060)."""
        standing = self.standing()
        if not self.tags:
            return "no role read yet"
        past = len(self.tags) - len(standing)
        items = {t.item for t in standing}
        text = (f"{len(standing)} role{'s' if len(standing) != 1 else ''} standing on "
                f"{len(items)} item{'s' if len(items) != 1 else ''}, {past} past")
        if self.recasts:
            text += f"; {self.recasts} re-cast{'s' if self.recasts != 1 else ''}"
        return text

    # -- the lines (G-060) -------------------------------------------------------------------

    def _tag_lines(self, tag: TagState) -> list:
        """A tag's two lines: the tag, solid while standing and dashed once past, then
        its readers and, once past, how it went (G-060)."""
        fresh = self.index in (tag.built_index, tag.discarded_index)
        first = TextLine(tag_text(tag), WORLDVIEW if tag.standing else SHADOW, tag.item,
                         fresh=fresh)
        parts = []
        if tag.consumed:
            readers = "; ".join(f"{c} at codelet {clock}: {effect} ({by})"
                                for c, effect, by, clock in tag.consumed)
            parts.append(f"read by {readers}")
        else:
            parts.append("read by no consumer yet")
        if not tag.standing:
            parts.append(f"{tag.reason} by {tag.discarded_by} at codelet {tag.discarded_at}")
        return [first, TextLine("; ".join(parts), DETAIL, tag.item, fresh=fresh)]

    def _recast_lines(self) -> list:
        """The re-cast block: the touched object's signature and the tiers (D-093)."""
        rc = self.recast
        if rc is None:
            return [TextLine("re-cast: none published yet", MUTED)]
        fresh = self.index == rc["index"]
        signature = " ".join(rc["signature"]) or "(empty)"
        out = [TextLine(f"re-cast of {rc['touched']} at codelet {rc['clock']}: signature "
                        f"[{signature}]" + (" (tie)" if rc["ambiguous"] else ""), HEADING,
                        rc["touched"], fresh=fresh)]
        for tier in R.TIERS:
            names = [f"{c} ({w:g})" for t, c, w in rc["tiers"] if t == tier]
            if names:
                out.append(TextLine(f"tier {tier}: {', '.join(names)}", DETAIL, fresh=fresh))
        return out

    def lines(self) -> list:
        """The pane's text (G-060): the banner, the axis, the re-cast, then one heading
        per family with its tags, each item's standing tags before its past ones."""
        out = [TextLine(self.banner(), HEADING)]
        axis = self.axis()
        out.append(TextLine(f"axis: {axis.args[0]} symmetry ({axis.evidence})"
                            if axis is not None else "axis: none read yet",
                            DETAIL if axis is not None else MUTED, R.WORLDVIEW_ITEM))
        out += self._recast_lines()
        for family, tags in self.families().items():
            out.append(TextLine(f"{FAMILY_TITLES[family]}: "
                                f"{sum(1 for t in tags if t.standing)} standing, "
                                f"{sum(1 for t in tags if not t.standing)} past", HEADING))
            if not tags:
                out.append(TextLine("none read", MUTED))
                continue
            for item in self.items():
                mine = [t for t in tags if t.item == item]
                for tag in [t for t in mine if t.standing] + [t for t in mine if not t.standing]:
                    out += self._tag_lines(tag)
        return out

    def tooltip(self, name: str) -> str:
        """The hover text of an item's lines: every tag it carried, current and past,
        with when and why each went (D1 of the roles loop's answers; G-060)."""
        history = self.history_of(name)
        if not history and name != R.WORLDVIEW_ITEM:
            return ""
        lines = [f"{name}: {len([t for t in history if t.standing])} role(s) standing, "
                 f"{len([t for t in history if not t.standing])} past"]
        for tag in history:
            state = ("standing" if tag.standing
                     else f"{tag.reason} by {tag.discarded_by} at codelet {tag.discarded_at}")
            readers = ", ".join(tag.consumers) or "no consumer"
            lines.append(f"  {tag_text(tag)}; {state}; read by {readers}")
        return "\n".join(lines)

    def selection_of(self, name: str):
        """What a click on an item's lines selects (G-022): an object, or a group with
        its members; nothing for the table or the Worldview item."""
        if name in (R.TABLE, R.WORLDVIEW_ITEM) or not name:
            return None
        if name in self.groups:
            return ("group", name, self.groups[name])
        return ("object", name, ())
