"""The table pane's structures (PANES.md P1, part 2; G-020, G-021, G-023): the groups,
the proposals and the correspondences as the events leave them, laid out as boxes and
lines in table units, with the pure style functions. Qt-free.

What French's screen drew (SPEC.md 6c.14; the dumps [F95 pp. 98-107 (img)]): a gestalt
group as a box with a wavy border around its objects, a subgroup as a dashed rectangle
inside it, captioned "Build new subgroup" when it has just been built [F95 p. 102 (img)];
a correspondence as a line between the centres of its two ends, dashed while it is
tentative or competing ("actually a flashing dashed line on the computer screen" [F95
p. 105]) and solid once it is in the Worldview, thicker between groups [F95 p. 104 (img)];
a new correspondence "is highlighted by flashing boxes at both ends" [F95 p. 101]. The
proposals' faint dotted outlines with their stage, the fading of what was removed and the
hover texts are the GUI's own (G-021, G-023).

Everything drawn comes from the events, exact at every index (G-020): `GroupBuilt` and
`GroupRemoved`; `GroupProposed` and `GroupStagePassed` (the stage that ran, so a pass
moves the proposal to the next one, D-024); `CorrespondenceProposed` (the level),
`CorrespondenceBuilt`, `CorrespondenceTested`, `CompetitionHeld`, `WorldviewChanged` and
`CorrespondenceRemoved` (with the ones built inside it). The snapshot in force only
supplies numbers for the hover text, which the events do not carry as they change: a
correspondence's rating as it stands, its wins and losses, and a group's frame (its ends,
middles and orientation), and the tap records the nine static and three dynamic terms of
every correspondence beside each snapshot (`correspondences.static_terms`,
`dynamic_terms`; `models/tap.py`). The flash is the parity of the event index
(`flash_phase`), never a timer, so the picture at an index is the same at every redraw
(PANES.md P1).
"""

import math
from dataclasses import dataclass

from tabletop import events as ev

__all__ = ["FADE_CODELETS", "NEW_CODELETS", "GESTALT_PAD", "SUBGROUP_PAD", "PROPOSAL_PAD",
           "STAGES", "SUBGROUP_CAPTION", "BoxStyle", "CorrespondenceLine", "GroupBox",
           "LineStyle", "ProposalBox", "StructuresModel", "box_style", "centre_of",
           "flash_phase", "line_style", "union_box", "wavy_outline"]

# How long a removed structure stays drawn, faint, after its removal, in codelets; and
# how long a new correspondence flashes its ends and a new subgroup wears its caption
# (G-021).
FADE_CODELETS = 3
NEW_CODELETS = 2

# The room between a group's members and its border, in table units (G-021): a gestalt
# box outside its subgroups, a subgroup outside the proposals it may have come from.
GESTALT_PAD = 2.6
SUBGROUP_PAD = 1.3
PROPOSAL_PAD = 0.5

STAGES = 3                                  # the three micro-tests of a proposal (D-024)
SUBGROUP_CAPTION = "Build new subgroup"     # the screen's caption [F95 p. 102 (img)]


# -- the drawn forms (plain data for the view) --------------------------------------------------

@dataclass(frozen=True)
class GroupBox:
    """A group as drawn (G-021): its `name`, `kind` (`gestalt` or a bottom-up kind),
    `category`, `members`, `parent`, the `box` (left, top, right, bottom) around its
    members' footprints, `wavy` for a gestalt group's border, `fading` once removed, and
    its `caption` (the screen's "Build new subgroup" while new, else empty)."""

    name: str
    kind: str
    category: str
    members: tuple
    parent: str
    box: tuple
    wavy: bool
    fading: bool = False
    caption: str = ""

    @property
    def centre(self) -> tuple:
        """The box's centre, where a correspondence line ends (G-021)."""
        left, top, right, bottom = self.box
        return ((left + right) / 2, (top + bottom) / 2)


@dataclass(frozen=True)
class ProposalBox:
    """A bottom-up group proposed but not built (D-024; G-021): its `name`, `members`,
    `kind`, `category`, the `box` around its members, the `stage` it has reached (0 to
    3, the stages passed), whether it `failed`, `fading` after a failure, and the
    `label` the view writes ("p1: stage 1 of 3")."""

    name: str
    members: tuple
    kind: str
    category: str
    box: tuple
    stage: int
    failed: bool = False
    fading: bool = False
    label: str = ""


@dataclass(frozen=True)
class CorrespondenceLine:
    """A correspondence as drawn (G-021): its `name`, the `henry` and `eliza` ends and
    their centres `a` and `b`, the `level`, whether it is `in_worldview` (solid),
    `group_level` (an end is a group: thicker), `tentative` (not tested yet),
    `competing` (in a fight at this codelet), a `loser` (lost and outside the Worldview:
    faint), `fading` (removed), `new` (its ends flash), the `end_boxes` of the two ends,
    the flash `phase` (the event index's parity) and `role_proposed` (a role consumer
    proposed it, D-095; G-061)."""

    name: str
    henry: str
    eliza: str
    a: tuple
    b: tuple
    level: str
    in_worldview: bool
    group_level: bool
    tentative: bool = False
    competing: bool = False
    loser: bool = False
    fading: bool = False
    new: bool = False
    end_boxes: tuple = ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0))
    phase: int = 0
    role_proposed: bool = False


# -- the pure style functions (PANES.md P1 tests) -------------------------------------------------

@dataclass(frozen=True)
class LineStyle:
    """How a correspondence line is drawn (G-021): `dashed`, its `width` (scene units),
    its `alpha` (0 to 255) and the dash `phase` (0 or 1; 0 for a solid line)."""

    dashed: bool
    width: float
    alpha: int
    phase: int


@dataclass(frozen=True)
class BoxStyle:
    """How a group's or a proposal's border is drawn (G-021): the `border` (`wavy`,
    `dashed`, `dotted`), its `width` and `alpha`."""

    border: str
    width: float
    alpha: int


def flash_phase(index: int) -> int:
    """The flash phase at an event index: its parity, so that a picture at an index is
    the same at every redraw (PANES.md P1; G-021)."""
    return index % 2


def line_style(line: CorrespondenceLine) -> LineStyle:
    """Worldview lines solid, every other dashed with the line's phase; group-level
    thicker; losers faint, removed ones fainter (SPEC.md 6c.14; G-021)."""
    dashed = not line.in_worldview
    width = 2.8 if line.group_level else 1.5
    alpha = 60 if line.fading else 120 if line.loser else 255
    return LineStyle(dashed, width, alpha, line.phase if dashed else 0)


def box_style(form) -> BoxStyle:
    """A gestalt group's border wavy, a subgroup's dashed, a proposal's dotted and faint;
    a removed or failed one fainter (SPEC.md 6c.14; G-021)."""
    if isinstance(form, ProposalBox):
        return BoxStyle("dotted", 1.0, 50 if form.fading else 110)
    border = "wavy" if form.wavy else "dashed"
    return BoxStyle(border, 1.4, 70 if form.fading else 230)


# -- geometry --------------------------------------------------------------------------------------

def centre_of(box: tuple) -> tuple:
    """The centre of a box (left, top, right, bottom) (G-021)."""
    return ((box[0] + box[2]) / 2, (box[1] + box[3]) / 2)


def union_box(boxes, pad: float) -> tuple:
    """The smallest box around the boxes, grown by `pad` on every side (G-021)."""
    left = min(b[0] for b in boxes) - pad
    top = min(b[1] for b in boxes) - pad
    right = max(b[2] for b in boxes) + pad
    bottom = max(b[3] for b in boxes) + pad
    return (left, top, right, bottom)


def wavy_outline(box: tuple, amplitude: float = 0.6, wavelength: float = 2.4) -> tuple:
    """The points of a wavy border around the box (G-021): a closed polyline that
    follows the rectangle's edges with a sine wave of `amplitude` across them, one
    period per `wavelength`, every point within `amplitude` of the edge."""
    left, top, right, bottom = box
    corners = [(left, top), (right, top), (right, bottom), (left, bottom)]
    points = []
    for k in range(4):
        (x1, y1), (x2, y2) = corners[k], corners[(k + 1) % 4]
        length = math.hypot(x2 - x1, y2 - y1)
        steps = max(4, int(round(length / wavelength)) * 8)
        nx, ny = -(y2 - y1) / length, (x2 - x1) / length        # the outward normal
        for s in range(steps):
            t = s / steps
            wave = amplitude * math.sin(2 * math.pi * t * length / wavelength)
            points.append((x1 + (x2 - x1) * t + nx * wave, y1 + (y2 - y1) * t + ny * wave))
    return tuple(points)


# -- the event-built state ---------------------------------------------------------------------------

@dataclass
class GroupState:
    """A group as the events left it (G-020)."""

    name: str
    members: tuple
    kind: str
    category: str
    strength: float
    parent: str
    orientation: str
    built_at: int
    removed_at: int | None = None
    reason: str = ""


@dataclass
class ProposalState:
    """A proposal as the events left it (D-024; G-020): `stage` is the next stage to
    run, 3 once built; `failed` after a stage that did not pass."""

    name: str
    members: tuple
    kind: str
    category: str
    base: str
    proposed_at: int
    stage: int = 0
    failed: bool = False
    failed_at: int | None = None


@dataclass
class CorrespondenceState:
    """A correspondence as the events left it (G-020); `role_proposed` names the role
    consumer and the role whose reading proposed it (`C1 MIRROR_IMAGE by
    propose-role-cp`, D-095), empty for one French's scouts proposed (G-061)."""

    name: str
    henry: str
    eliza: str
    level: str
    built_at: int
    static: float = 0.0
    dynamic: float = 0.0
    bonus: float = 0.0
    tested: bool = False
    passed: bool = False
    slippages: tuple = ()
    in_worldview: bool = False
    wins: int = 0
    losses: int = 0
    competed_at: int | None = None
    rivals: tuple = ()
    removed_at: int | None = None
    inner: tuple = ()
    role_proposed: str = ""

    def competing_now(self, clock: int) -> bool:
        """Whether it is in a fight at this codelet (G-021)."""
        return self.competed_at == clock and self.removed_at is None


class StructuresModel:
    """The groups, proposals and correspondences of a run as the events leave them
    (G-020): `groups`, `proposals`, `correspondences` (name -> state, in the order
    built), `levels` (the level each `CorrespondenceProposed` gave), `clock` (the last
    event's), `events` (how many), and the snapshot's numbers for the hover text
    (`snapshot_groups`, `snapshot_correspondences`, `terms`, `snapshot_clock`)."""

    def __init__(self):
        self.clear()

    def clear(self) -> None:
        """Forget the run (G-020)."""
        self.groups = {}
        self.proposals = {}
        self.correspondences = {}
        self.levels = {}
        self.clock = 0
        self.events = 0
        self.snapshot_groups = {}
        self.snapshot_correspondences = {}
        self.terms = {}
        self.snapshot_clock = None
        self.role_pairs = {}

    # -- the events (G-020) -------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply one event (G-020); every event not about a structure only moves the
        clock."""
        if isinstance(event, ev.RunStarted):
            self.clear()
        self.clock = event.clock
        self.events += 1
        handler = self._HANDLERS.get(type(event))
        if handler is not None:
            handler(self, event)

    def _group_built(self, e) -> None:
        self.groups[e.group] = GroupState(e.group, tuple(e.members), e.kind, e.category,
                                          e.strength, e.parent, e.orientation, e.clock)

    def _group_removed(self, e) -> None:
        g = self.groups.get(e.group)
        if g is not None:
            g.removed_at, g.reason = e.clock, e.reason

    def _group_proposed(self, e) -> None:
        self.proposals[e.group] = ProposalState(e.group, tuple(e.members), e.kind, e.category,
                                                e.base, e.clock)

    def _stage_passed(self, e) -> None:
        p = self.proposals.get(e.group)
        if p is None:
            return
        if e.passed:
            p.stage = e.stage + 1
        else:
            p.stage, p.failed, p.failed_at = e.stage, True, e.clock

    def _cp_proposed(self, e) -> None:
        self.levels[e.correspondence] = e.level

    def _role_consumed(self, e) -> None:
        """A consumer's `create-cp a b` effect (D-095, D-093) names the pair whose
        correspondence, once built, is role-proposed (G-061)."""
        words = e.effect.split()
        if len(words) == 3 and words[0] == "create-cp":
            self.role_pairs[frozenset(words[1:])] = f"{e.consumer} {e.role} by {e.by}"

    def _cp_built(self, e) -> None:
        level = self.levels.get(e.correspondence, "top")
        self.correspondences[e.correspondence] = CorrespondenceState(
            e.correspondence, e.henry, e.eliza, level, e.clock, e.static, e.dynamic, e.bonus,
            role_proposed=self.role_pairs.pop(frozenset((e.henry, e.eliza)), ""))

    def _cp_tested(self, e) -> None:
        c = self.correspondences.get(e.correspondence)
        if c is not None:
            c.tested, c.passed, c.static = True, e.passed, e.static
            c.slippages = tuple(tuple(s) for s in e.slippages)

    def _competition(self, e) -> None:
        """Wins and losses as `worldview.compete` awards them (D-038, D-040)."""
        newcomer = self.correspondences.get(e.newcomer)
        if newcomer is None:
            return
        newcomer.competed_at, newcomer.rivals = e.clock, tuple(e.rivals)
        for name in e.rivals:
            rival = self.correspondences.get(name)
            if rival is not None:
                rival.competed_at, rival.rivals = e.clock, (e.newcomer,)
        if not e.rivals:
            return
        if e.rival_wins:
            for c in self.correspondences.values():
                if c.in_worldview and c.name not in e.rivals and c.removed_at is None:
                    c.wins += 1
            newcomer.wins += 1
            for name in e.rivals:
                if name in self.correspondences:
                    self.correspondences[name].losses += 1
        else:
            for c in self.correspondences.values():
                if c.in_worldview and c.removed_at is None:
                    c.wins += 1
            newcomer.losses += 1

    def _worldview_changed(self, e) -> None:
        for name in e.removed:
            if name in self.correspondences:
                self.correspondences[name].in_worldview = False
        for name in e.added:
            if name in self.correspondences:
                self.correspondences[name].in_worldview = True

    def _cp_removed(self, e) -> None:
        for name in (e.correspondence,) + tuple(e.inner):
            c = self.correspondences.get(name)
            if c is not None and c.removed_at is None:
                c.removed_at = e.clock
                c.in_worldview = False
        c = self.correspondences.get(e.correspondence)
        if c is not None:
            c.inner = tuple(e.inner)

    _HANDLERS = {
        ev.GroupBuilt: _group_built, ev.GroupRemoved: _group_removed,
        ev.GroupProposed: _group_proposed, ev.GroupStagePassed: _stage_passed,
        ev.CorrespondenceProposed: _cp_proposed, ev.CorrespondenceBuilt: _cp_built,
        ev.CorrespondenceTested: _cp_tested, ev.CompetitionHeld: _competition,
        ev.WorldviewChanged: _worldview_changed, ev.CorrespondenceRemoved: _cp_removed,
        ev.RoleConsumed: _role_consumed,
    }

    # -- the snapshot's numbers (G-020) ----------------------------------------------------------

    def update(self, snapshot, terms=None) -> None:
        """Keep the snapshot's numbers for the hover text, and the terms the tap recorded
        beside it; never older ones than those already kept (G-020)."""
        if snapshot is None:
            return
        if self.snapshot_clock is not None and snapshot.clock < self.snapshot_clock:
            return
        self.snapshot_clock = snapshot.clock
        self.snapshot_groups = {g["name"]: g for g in snapshot.groups}
        self.snapshot_correspondences = {c["name"]: c for c in snapshot.correspondences}
        self.terms = dict(terms or {})

    # -- what is drawn (G-021) --------------------------------------------------------------------

    def _shown(self, removed_at) -> bool:
        """Whether a structure removed at that clock is still drawn, fading (G-021)."""
        return removed_at is None or self.clock - removed_at < FADE_CODELETS

    def live_groups(self) -> list:
        """The groups in the Workspace now, in the order built (G-020)."""
        return [g for g in self.groups.values() if g.removed_at is None]

    def live_correspondences(self) -> list:
        """The correspondences in the Workspace now, in the order built (G-020)."""
        return [c for c in self.correspondences.values() if c.removed_at is None]

    def group_boxes(self, boxes: dict) -> tuple:
        """The groups as `GroupBox`es, given each object's box (G-021); gestalt first,
        then the subgroups, so that a subgroup is drawn over its parent."""
        out = []
        for g in self.groups.values():
            if not self._shown(g.removed_at) or not all(m in boxes for m in g.members):
                continue
            gestalt = g.kind == "gestalt"
            pad = GESTALT_PAD if gestalt else SUBGROUP_PAD
            new = not gestalt and g.removed_at is None and self.clock - g.built_at < NEW_CODELETS
            out.append(GroupBox(g.name, g.kind, g.category or "", g.members, g.parent,
                                union_box([boxes[m] for m in g.members], pad), gestalt,
                                g.removed_at is not None, SUBGROUP_CAPTION if new else ""))
        return tuple(sorted(out, key=lambda b: 0 if b.wavy else 1))

    def proposal_boxes(self, boxes: dict) -> tuple:
        """The proposals not built as `ProposalBox`es (D-024; G-021): a failed one fades,
        a built one is its group's."""
        out = []
        for p in self.proposals.values():
            if p.stage >= STAGES or not all(m in boxes for m in p.members):
                continue
            if p.failed and not self._shown(p.failed_at):
                continue
            label = (f"{p.name}: failed stage {p.stage + 1} of {STAGES}" if p.failed
                     else f"{p.name}: stage {p.stage} of {STAGES}")
            out.append(ProposalBox(p.name, p.members, p.kind, p.category or "",
                                   union_box([boxes[m] for m in p.members], PROPOSAL_PAD),
                                   p.stage, p.failed, p.failed, label))
        return tuple(out)

    def _end(self, name: str, boxes: dict, groups: dict):
        """The box of an end, an object's or a group's, or None (G-021)."""
        if name in boxes:
            return boxes[name]
        g = groups.get(name)
        return g.box if g is not None else None

    def correspondence_lines(self, boxes: dict, groups: tuple) -> tuple:
        """The correspondences as `CorrespondenceLine`s between the centres of their
        ends (G-021), with the phase of this event."""
        by_name = {g.name: g for g in groups}
        phase = flash_phase(self.events - 1)
        out = []
        for c in self.correspondences.values():
            if not self._shown(c.removed_at):
                continue
            box_a, box_b = self._end(c.henry, boxes, by_name), self._end(c.eliza, boxes, by_name)
            if box_a is None or box_b is None:
                continue
            out.append(CorrespondenceLine(
                c.name, c.henry, c.eliza, centre_of(box_a), centre_of(box_b), c.level,
                c.in_worldview and c.removed_at is None,
                c.henry in by_name or c.eliza in by_name,
                tentative=not c.tested, competing=c.competing_now(self.clock),
                loser=c.losses > 0 and not c.in_worldview, fading=c.removed_at is not None,
                new=c.removed_at is None and self.clock - c.built_at < NEW_CODELETS,
                end_boxes=(box_a, box_b), phase=phase, role_proposed=bool(c.role_proposed)))
        return tuple(out)

    # -- the selection and the hover text (G-022, G-023) ------------------------------------------

    def members_of(self, name: str) -> tuple:
        """The objects an end stands for: itself, or a group's members (G-022)."""
        g = self.groups.get(name)
        return g.members if g is not None else (name,)

    def selection_of(self, name: str):
        """What selecting the structure means: (kind, name, members), or None for no
        such structure (G-022)."""
        if name in self.groups:
            return ("group", name, self.groups[name].members)
        if name in self.proposals:
            return ("proposal", name, self.proposals[name].members)
        c = self.correspondences.get(name)
        if c is not None:
            return ("correspondence", name,
                    (c.henry, c.eliza) + self.members_of(c.henry) + self.members_of(c.eliza))
        return None

    def tooltip(self, name: str) -> str:
        """The hover text of a group, a proposal or a correspondence; empty for none
        (G-023)."""
        if name in self.groups:
            return self._group_tooltip(self.groups[name])
        if name in self.proposals:
            return self._proposal_tooltip(self.proposals[name])
        if name in self.correspondences:
            return self._correspondence_tooltip(self.correspondences[name])
        return ""

    def _as_of(self) -> str:
        return f" (as of codelet {self.snapshot_clock})" if self.snapshot_clock is not None else ""

    def _group_tooltip(self, g: GroupState) -> str:
        kind = "gestalt group" if g.kind == "gestalt" else f"{g.kind} subgroup"
        if g.category:
            kind += f" ({g.category})"
        lines = [f"{g.name}: {kind} of {len(g.members)}: {', '.join(g.members)}",
                 f"strength {g.strength:.1f}, built at codelet {g.built_at}"
                 + (f" inside {g.parent}" if g.parent else "")]
        frame = self.snapshot_groups.get(g.name)
        if frame is not None and frame.get("orientation"):
            lines.append(f"{frame['orientation']}"
                         + (", linear" if frame.get("linear") else "")
                         + (f"; ends {', '.join(frame['ends'])}" if frame.get("ends") else "")
                         + (f"; middles {', '.join(frame['middles'])}" if frame.get("middles")
                            else "") + self._as_of())
        if g.removed_at is not None:
            lines.append(f"removed at codelet {g.removed_at} ({g.reason})")
        return "\n".join(lines)

    def _proposal_tooltip(self, p: ProposalState) -> str:
        kind = f"{p.kind} group" + (f" ({p.category})" if p.category else "")
        lines = [f"{p.name}: proposed {kind} of {', '.join(p.members)}"
                 + (f", extending {p.base}" if p.base else ""),
                 f"proposed at codelet {p.proposed_at}"]
        if p.failed:
            lines.append(f"failed stage {p.stage + 1} of {STAGES} at codelet {p.failed_at}")
        elif p.stage >= STAGES:
            lines.append("built")
        else:
            lines.append(f"{p.stage} of {STAGES} stages passed")
        return "\n".join(lines)

    def _status(self, c: CorrespondenceState) -> str:
        if c.removed_at is not None:
            return f"removed at codelet {c.removed_at}"
        if c.competing_now(self.clock):
            return f"competing with {', '.join(c.rivals)} at codelet {c.competed_at}"
        if c.in_worldview:
            return "in the Worldview"
        if not c.tested:
            return "tentative, not tested yet"
        if not c.passed:
            return "failed its test"
        if c.losses:
            return "lost a competition, outside the Worldview"
        return "tested, outside the Worldview"

    def _correspondence_tooltip(self, c: CorrespondenceState) -> str:
        now = self.snapshot_correspondences.get(c.name)
        static, dynamic, bonus = ((now["static"], now["dynamic"], now["bonus"]) if now
                                  else (c.static, c.dynamic, c.bonus))
        lines = [f"{c.name}: {c.henry} (Henry's end) to {c.eliza} (Eliza's end), level {c.level}",
                 self._status(c),
                 f"rating {max(0.0, static + dynamic + bonus):.1f} = static {static:.1f} "
                 f"+ dynamic {dynamic:.1f} + bonus {bonus:.1f}" + (self._as_of() if now else "")]
        terms = self.terms.get(c.name)
        if terms:
            lines.append("static terms: " + ", ".join(f"{n} {v:g}" for n, v in terms["static"]))
            lines.append("dynamic terms: " + ", ".join(f"{n} {v:g}" for n, v in terms["dynamic"]))
        if c.slippages:
            lines.append("slippages: " + ", ".join(
                f"{a} to {b}" + (f" ({label})" if label else "") for a, b, label in c.slippages))
        if c.role_proposed:
            lines.append(f"role-proposed: {c.role_proposed} (D-095)")
        lines.append(f"wins {c.wins}, losses {c.losses}")
        return "\n".join(lines)
