"""The Worldview pane's model (PANES.md P5; GUI design, not French's, from the text of
[F95 pp. 67-74]; G-039, G-040): the correspondences in the Worldview and the shadow
cabinet (the ones built but outside it), each with its points, rating, wins, losses and
slippages; the Worldview's rating with its coherent, incoherent and parallel pairs; the
rivals of every correspondence; and the bottom-up proposals with the stage each reached
(D-024). Qt-free.

"The Worldview is a set of noncontradictory mappings of objects and groups of objects onto
one another." [F95 p. 70] A correspondence outside it "remains nonetheless in the Workspace
with the potential to compete again" [F95 p. 72]: that is the shadow cabinet. Two
correspondences may be rivals, which "are forbidden to belong to the Worldview at the same
time" [F95 p. 105], or an incoherent pair, which "can nonetheless coexist" [F95 p. 105];
the pane marks the two differently (G-040).

The entries come from the events, through a composed `StructuresModel` (G-020: exact at
every index), and the numbers from the snapshot in force, dated by its clock (G-039): a
correspondence's static, dynamic and bonus points as they stand, the Worldview's rating
with its three terms and its pairs (`Snapshot.worldview`, `worldview.current`, D-042,
D-043), and the rivals the tap read beside the snapshot (`worldview.rivals_of`, D-044;
`models/tap.py`). A model that has seen no events lists the snapshot's correspondences,
so that a state built by hand (`tests/test_worldview.py`'s chapter 4 state) can be shown.
The Worldview usually holds one correspondence and no pair (RESULTS.md section 9.2); the
banner says so in plain words (ANSWERS.md 11).
"""

from dataclasses import dataclass

from tabletop import events as ev
from tabletop.gui.models.structures import STAGES, CorrespondenceState, StructuresModel

__all__ = ["NOT_FRENCHS", "Entry", "Proposal", "RatingView", "TextLine", "WorldviewModel",
           "arrow_text", "pair_text", "slippage_text", "state_of_snapshot"]

NOT_FRENCHS = "GUI design, not French's"

# The roles a `TextLine` can have (G-040): the view styles each in one way.
HEADING, WORLDVIEW, SHADOW, DETAIL, WARNING, MUTED = (
    "heading", "worldview", "shadow", "detail", "warning", "muted")


@dataclass(frozen=True)
class TextLine:
    """One line of a pane's text (G-040): its `text`, its `role` (`heading`, `worldview`
    for an entry of the Worldview, `shadow` for one of the shadow cabinet, `detail`,
    `warning` for an incoherent pair, `muted`), the `name` of the structure it is about
    (empty for none; hover and click use it), `cells`, two (text, role) pairs when the
    line is drawn in two columns (the competitions' tables), and `fresh`, whether the
    event shown is the one the line reports."""

    text: str
    role: str = DETAIL
    name: str = ""
    cells: tuple = ()
    fresh: bool = False


@dataclass(frozen=True)
class Entry:
    """A correspondence as the pane lists it (G-039): its `name`, ends, `level`, whether
    it is `in_worldview`, its `status` in words, its points (`static`, `dynamic`, `bonus`,
    `rating`, D-036 to D-038) and whether they are `dated` by the snapshot or are the
    first evaluation's, `wins` and `losses` (D-031, factor 7), `slippages` as (from, to,
    label), the `rivals` it conflicts with in the Worldview (D-044), the names it
    makes a `coherent`, an `incoherent` or a `parallel` pair with (D-042, D-043), and
    `role_proposed`, the consumer and the role that proposed it, empty for one of
    French's scouts (D-095; G-061)."""

    name: str
    henry: str
    eliza: str
    level: str
    in_worldview: bool
    status: str
    static: float
    dynamic: float
    bonus: float
    rating: float
    dated: bool
    wins: int
    losses: int
    slippages: tuple
    rivals: tuple = ()
    coherent: tuple = ()
    incoherent: tuple = ()
    parallel: tuple = ()
    role_proposed: str = ""


@dataclass(frozen=True)
class RatingView:
    """The Worldview's rating as the snapshot in force reports it (D-042, D-043; G-039):
    the `names`, the three terms and the `total`, `coherent`, the three pair lists, the
    snapshot's clock as `as_of` (None before the first snapshot), and `latest`, the
    last `WorldviewChanged` after that snapshot as (clock, total, coherent, reason), or
    None when the snapshot is current."""

    names: tuple = ()
    strength: float = 0.0
    coherence: float = 0.0
    parallelism: float = 0.0
    total: float = 0.0
    coherent: bool = True
    coherent_pairs: tuple = ()
    incoherent_pairs: tuple = ()
    parallel_pairs: tuple = ()
    as_of: int | None = None
    latest: tuple | None = None


@dataclass(frozen=True)
class Proposal:
    """A bottom-up proposal as the Proposals tab lists it (D-024; G-039): its `name`,
    `members`, `kind`, `category`, the `stage` reached (the stages passed, 0 to 3),
    whether it `failed`, whether it was `built` (and as which `group`), when it was
    proposed and its `label`."""

    name: str
    members: tuple
    kind: str
    category: str
    stage: int
    failed: bool
    built: bool
    group: str
    proposed_at: int
    label: str


def arrow_text(henry: str, eliza: str) -> str:
    """The two ends as text, in the picking order (D-005)."""
    return f"{henry} → {eliza}"


def pair_text(pairs) -> str:
    """Pairs of names as text, "cp1 - cp2, ...", or "none" (G-040)."""
    return ", ".join(f"{a} - {b}" for a, b in pairs) if pairs else "none"


def slippage_text(slippages) -> str:
    """The slippages as text, "cup to cup (sameness), ..." (D-039; G-040)."""
    return ", ".join(f"{a} to {b}" + (f" ({label})" if label else "")
                     for a, b, label in slippages) or "none"


def state_of_snapshot(d: dict, clock: int) -> CorrespondenceState:
    """A correspondence state from a snapshot's dictionary (G-039: for a model that has
    seen no events)."""
    return CorrespondenceState(d["name"], d["henry"], d["eliza"], d["level"], clock,
                               d["static"], d["dynamic"], d["bonus"], d["tested"],
                               d["passed"], tuple(tuple(s) for s in d["slippages"]),
                               d["in_worldview"], d["wins"], d["losses"])


class WorldviewModel:
    """The Worldview pane's model (G-039, G-040): `structures`, the composed
    `StructuresModel`; `changes`, every `WorldviewChanged` as (index, clock, added,
    removed, rating, coherent, reason); the snapshot's `rating_now` and `rivals`, dated
    by `snapshot_clock`; `clock`, `events`, `started`, `ended`."""

    def __init__(self):
        self.structures = StructuresModel()
        self.clear()

    def clear(self) -> None:
        """Forget the run (G-039)."""
        self.structures.clear()
        self.changes = []
        self.clock = 0
        self.events = 0
        self.started = False
        self.ended = False
        self.rating_now = None
        self.rivals = {}
        self.snapshot_states = {}
        self.snapshot_clock = None

    # -- the events (G-039) -------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply one event (G-039): the structures take theirs; a `WorldviewChanged` is
        kept; `RunStarted` forgets the run and `RunEnded` closes it."""
        if isinstance(event, ev.RunStarted):
            self.clear()
            self.started = True
        self.structures.on_event(event)
        self.clock = event.clock
        self.events += 1
        if isinstance(event, ev.WorldviewChanged):
            self.changes.append((self.events - 1, event.clock, tuple(event.added),
                                 tuple(event.removed), event.rating, event.coherent,
                                 event.reason))
        elif isinstance(event, ev.RunEnded):
            self.ended = True

    # -- the snapshot's numbers (G-039) ----------------------------------------------------------

    def update(self, snapshot, terms=None, readings=None) -> None:
        """Keep the snapshot's numbers (the points, the rating and its pairs) and the
        rivals the tap read beside it; never older ones than those kept (G-039)."""
        if snapshot is None:
            return
        if self.snapshot_clock is not None and snapshot.clock < self.snapshot_clock:
            return
        self.structures.update(snapshot, terms)
        self.snapshot_clock = snapshot.clock
        self.rating_now = dict(snapshot.worldview)
        self.rivals = dict((readings or {}).get("rivals", {}))
        self.snapshot_states = {c["name"]: state_of_snapshot(c, snapshot.clock)
                                for c in snapshot.correspondences}

    # -- the entries (G-039) ----------------------------------------------------------------------

    def states(self) -> list:
        """The correspondences in the Workspace now, in the order built (G-039): the
        events' (exact at every index), or the snapshot's when no event has been seen."""
        if self.structures.events:
            return self.structures.live_correspondences()
        return list(self.snapshot_states.values())

    def status_of(self, c: CorrespondenceState) -> str:
        """A correspondence's standing in words (G-040)."""
        if c.competing_now(self.clock):
            return f"competing with {', '.join(c.rivals)} at codelet {c.competed_at}"
        if c.in_worldview:
            return "in the Worldview"
        if not c.tested:
            return "tentative, not tested yet"
        if not c.passed:
            return "failed its test"
        if c.losses:
            return f"lost {c.losses}, outside the Worldview"
        return "tested, outside the Worldview"

    def _pairs_with(self, name: str, pairs) -> tuple:
        return tuple(b if a == name else a for a, b in pairs if name in (a, b))

    def entry_of(self, c: CorrespondenceState) -> Entry:
        """One correspondence's `Entry`: the events' facts, the snapshot's points when
        it has them (dated), else the first evaluation's (G-039)."""
        now = self.structures.snapshot_correspondences.get(c.name)
        if now is None and c.name in self.snapshot_states and not self.structures.events:
            now = {"static": c.static, "dynamic": c.dynamic, "bonus": c.bonus}
        static, dynamic, bonus = ((now["static"], now["dynamic"], now["bonus"]) if now
                                  else (c.static, c.dynamic, c.bonus))
        r = self.rating_now or {}
        return Entry(c.name, c.henry, c.eliza, c.level, c.in_worldview, self.status_of(c),
                     static, dynamic, bonus, max(0.0, static + dynamic + bonus), now is not None,
                     c.wins, c.losses, c.slippages, tuple(self.rivals.get(c.name, ())),
                     self._pairs_with(c.name, r.get("coherent_pairs", ())),
                     self._pairs_with(c.name, r.get("incoherent_pairs", ())),
                     self._pairs_with(c.name, r.get("parallel_pairs", ())),
                     c.role_proposed)

    def entries(self) -> tuple:
        """(the Worldview's entries, the shadow cabinet's), each in the order built
        (G-039)."""
        all_ = [self.entry_of(c) for c in self.states()]
        return ([e for e in all_ if e.in_worldview], [e for e in all_ if not e.in_worldview])

    def rating(self) -> RatingView:
        """The Worldview's rating as the snapshot in force reports it, with the latest
        change since it (G-039)."""
        r = self.rating_now
        if r is None:
            return RatingView()
        latest = None
        if self.changes:
            _index, clock, _added, _removed, total, coherent, reason = self.changes[-1]
            if clock > self.snapshot_clock:
                latest = (clock, total, coherent, reason)
        return RatingView(tuple(r["names"]), r["strength"], r["coherence"], r["parallelism"],
                          r["total"], r["coherent"], tuple(r["coherent_pairs"]),
                          tuple(r["incoherent_pairs"]), tuple(r["parallel_pairs"]),
                          self.snapshot_clock, latest)

    def banner(self) -> str:
        """The pane's first line, in plain words (ANSWERS.md 11; G-040): how many
        correspondences the Worldview holds and whether there are pairs to rate."""
        if not self.started and not self.snapshot_states:
            return "no run yet"
        worldview, shadow = self.entries()
        if not worldview:
            return ("no correspondence in the Worldview yet"
                    + (f"; {len(shadow)} outside it" if shadow else ""))
        if len(worldview) == 1:
            return "one correspondence; no pairs to rate"
        r = self.rating()
        pairs = len(r.coherent_pairs) + len(r.incoherent_pairs) + len(r.parallel_pairs)
        return (f"{len(worldview)} correspondences; {pairs} pair"
                + ("" if pairs == 1 else "s") + " rated")

    # -- the proposals (D-024; G-039) --------------------------------------------------------------

    def proposals(self) -> list:
        """Every bottom-up proposal with the stage it reached, in the order proposed: not
        a queue, a proposal whose codelet was aged out stays where it stopped (D-024)."""
        out = []
        groups = {g.members: g.name for g in self.structures.groups.values()}
        for p in self.structures.proposals.values():
            built = p.stage >= STAGES
            group = groups.get(p.members, "") if built else ""
            if p.failed:
                label = f"failed stage {p.stage + 1} of {STAGES} at codelet {p.failed_at}"
            elif built:
                label = "built" + (f" as {group}" if group else "")
            else:
                label = f"stage {p.stage} of {STAGES} passed"
            out.append(Proposal(p.name, p.members, p.kind, p.category or "", p.stage,
                                p.failed, built, group, p.proposed_at, label))
        return out

    # -- the pane's text (G-040) -------------------------------------------------------------------

    def is_group(self, name: str) -> bool:
        """Whether an end is a group, by the events' groups or the snapshot's (G-039)."""
        return name in self.structures.groups or name in self.structures.snapshot_groups

    def _entry_lines(self, e: Entry, role: str) -> list:
        """The two lines of an entry: the ends and the points, then the slippages, the
        pairs and the rivals (G-040)."""
        groups = " (groups)" if self.is_group(e.henry) and self.is_group(e.eliza) else ""
        first = (f"{e.name}: {arrow_text(e.henry, e.eliza)}{groups}   rating {e.rating:.1f} = "
                 f"static {e.static:.1f} + dynamic {e.dynamic:.1f} + bonus {e.bonus:.1f}"
                 + ("" if e.dated else " (first evaluation)")
                 + f"   wins {e.wins}, losses {e.losses}")
        parts = [e.status, "slippages: " + slippage_text(e.slippages)]
        if e.role_proposed:
            parts.append(f"role-proposed ({e.role_proposed})")
        if e.rivals:
            parts.append("rival of " + ", ".join(e.rivals))
        if e.incoherent:
            parts.append("incoherent with " + ", ".join(e.incoherent))
        if e.coherent:
            parts.append("coherent with " + ", ".join(e.coherent))
        if e.parallel:
            parts.append("parallel to " + ", ".join(e.parallel))
        second_role = WARNING if e.incoherent else DETAIL
        return [TextLine(first, role, e.name), TextLine("; ".join(parts), second_role, e.name)]

    def lines(self) -> list:
        """The Worldview tab's text (G-040): the banner, the Worldview's entries, the
        rating with its pairs, then the shadow cabinet's entries."""
        worldview, shadow = self.entries()
        out = [TextLine(self.banner(), HEADING)]
        for e in worldview:
            out += self._entry_lines(e, WORLDVIEW)
        out += self._rating_lines()
        out.append(TextLine(f"Shadow cabinet: {len(shadow)} correspondence"
                            + ("" if len(shadow) == 1 else "s") + " outside the Worldview",
                            HEADING))
        if not shadow:
            out.append(TextLine("none built, or all removed", MUTED))
        for e in shadow:
            out += self._entry_lines(e, SHADOW)
        return out

    def _rating_lines(self) -> list:
        r = self.rating()
        if r.as_of is None:
            return [TextLine("rating: not read yet", MUTED)]
        out = [TextLine(f"Rating {r.total:.1f} = strength {r.strength:.1f} + coherence "
                        f"{r.coherence:.1f} + parallelism {r.parallelism:.1f} (as of codelet "
                        f"{r.as_of})", HEADING)]
        if r.latest is not None:
            clock, total, coherent, reason = r.latest
            out.append(TextLine(f"changed at codelet {clock} ({reason}): total {total:.1f}, "
                                + ("coherent" if coherent else "incoherent"), DETAIL))
        out.append(TextLine("incoherent pairs: " + pair_text(r.incoherent_pairs),
                            WARNING if r.incoherent_pairs else MUTED))
        out.append(TextLine("coherent pairs: " + pair_text(r.coherent_pairs),
                            DETAIL if r.coherent_pairs else MUTED))
        out.append(TextLine("parallel pairs: " + pair_text(r.parallel_pairs),
                            DETAIL if r.parallel_pairs else MUTED))
        return out

    def proposal_lines(self) -> list:
        """The Proposals tab's text (D-024; G-040): one line per proposal with its
        stage, or a word when there is none."""
        proposals = self.proposals()
        if not self.started:
            return [TextLine("no run yet", HEADING)]
        if not proposals:
            return [TextLine("no bottom-up group proposed in this run", HEADING)]
        out = [TextLine(f"{len(proposals)} proposal" + ("" if len(proposals) == 1 else "s")
                        + " (D-024: not a queue; each stays where it stopped)", HEADING)]
        for p in proposals:
            kind = f"{p.kind} group" + (f" ({p.category})" if p.category else "")
            role = MUTED if p.failed else WORLDVIEW if p.built else DETAIL
            out.append(TextLine(f"{p.name}: {kind} of {', '.join(p.members)}, proposed at "
                                f"codelet {p.proposed_at}: {p.label}", role, p.name))
        return out

    # -- hover and selection (G-022, G-023) --------------------------------------------------------

    def tooltip(self, name: str) -> str:
        """The hover text of an entry or a proposal: the structures' (G-023), with the
        rivals the snapshot in force reports (G-039)."""
        text = self.structures.tooltip(name)
        if not text and name in self.snapshot_states:
            e = self.entry_of(self.snapshot_states[name])
            text = f"{name}: {arrow_text(e.henry, e.eliza)}, level {e.level}\n{e.status}"
        rivals = self.rivals.get(name)
        if text and rivals:
            text += f"\nrival of {', '.join(rivals)} (as of codelet {self.snapshot_clock})"
        return text

    def selection_of(self, name: str):
        """What a click on an entry selects: the structures' answer (G-022)."""
        found = self.structures.selection_of(name)
        if found is None and name in self.snapshot_states:
            c = self.snapshot_states[name]
            return ("correspondence", name, (c.henry, c.eliza))
        return found
