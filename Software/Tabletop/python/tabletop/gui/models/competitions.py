"""The Competitions pane's model (PANES.md P6; GUI design, not French's, after Fig. 3.11
[F95 p. 72 (img)]; G-041): every fight for the Worldview in order, the Old Worldview
against the Rival Worldview side by side, with the two ratings, the chance, the exponent
and the winner; the entries without a fight; and the banner "no competition in this run"
when there is none (ANSWERS.md 11). Qt-free.

"All the conflicting correspondences are temporarily removed (subjunctively) from the Old
Worldview, and the new correspondence is temporarily inserted (subjunctively) in their
place to form the Rival Worldview." [F95 p. 72] Fig. 3.11 draws the Worldview's
correspondence solid and the proposed rival dashed [F95 p. 72 (img)]; the tables keep
that convention: the old Worldview's correspondences in the role `worldview` (solid) and
the newcomer in the role `shadow` (dashed). A newcomer that "is not a rival to any
correspondence already in the Worldview" [F95 p. 101] is listed as entered without a
fight.

Everything comes from the events, exact at every index (G-041): `CompetitionHeld` gives
the fight (D-040, D-044) and `WorldviewChanged` the Worldview before and after it; the
model's logs equal `Snapshot.competitions` and `Snapshot.changes` at every snapshot
(tested). `CorrespondenceBuilt` gives each correspondence's ends for the tables.
"""

from dataclasses import dataclass

from tabletop import events as ev
from tabletop.gui.models.worldview import (DETAIL, HEADING, MUTED, SHADOW, WORLDVIEW,
                                           TextLine, arrow_text)

__all__ = ["NO_COMPETITION", "CompetitionsModel", "Fight", "fight_text"]

NO_COMPETITION = "no competition in this run"


@dataclass(frozen=True)
class Fight:
    """One `CompetitionHeld` (D-040; G-041): the event's `index` and `clock`, the
    `newcomer`, its `rivals` (empty for an entry without a fight), `old_rating`,
    `rival_rating`, `chance`, the exponent `a`, `rival_wins`; the `old` Worldview (names
    before the fight) and the `rival` Worldview (the old less the rivals plus the
    newcomer); and the `change`, the total the `WorldviewChanged` that followed reports,
    or None when the Old Worldview held its ground."""

    index: int
    clock: int
    newcomer: str
    rivals: tuple
    old_rating: float
    rival_rating: float
    chance: float
    a: float
    rival_wins: bool
    old: tuple
    rival: tuple
    change: float | None = None

    @property
    def fought(self) -> bool:
        """Whether there was a fight at all (G-041)."""
        return bool(self.rivals)

    @property
    def winner(self) -> str:
        """Who won, in words (G-041)."""
        if not self.fought:
            return "entered without a fight"
        return "the Rival Worldview won" if self.rival_wins else "the Old Worldview held"


def fight_text(fight: Fight, ends: dict) -> str:
    """A fight's hover text (G-041): the two Worldviews with their correspondences'
    ends, the ratings, the chance, the exponent and the winner."""
    def side(names):
        return ", ".join(f"{n} ({arrow_text(*ends[n])})" if n in ends else n for n in names) \
            or "empty"
    lines = [f"competition at codelet {fight.clock}: {fight.newcomer}"
             + (f" against {', '.join(fight.rivals)}" if fight.fought else ", no rival"),
             f"Old Worldview: {side(fight.old)}; rating {fight.old_rating:.1f}",
             f"Rival Worldview: {side(fight.rival)}; rating {fight.rival_rating:.1f}",
             f"chance {fight.chance:.3f} at a {fight.a:.2f}: {fight.winner}"]
    if fight.change is not None:
        lines.append(f"Worldview rating after the change: {fight.change:.1f}")
    return "\n".join(lines)


class CompetitionsModel:
    """The competitions of a run as the events tell them (G-041): `fights`, in order;
    `changes`, every `WorldviewChanged` in the snapshot's form; `worldview`, the names in
    it now in the order entered; `ends` by correspondence name; `clock`, `events`,
    `started`, and `fresh`, the fight whose event is the one shown (None otherwise)."""

    def __init__(self):
        self.clear()

    def clear(self) -> None:
        """Forget the run (G-041)."""
        self.fights = []
        self.changes = []
        self.worldview = []
        self.ends = {}
        self.clock = 0
        self.events = 0
        self.started = False
        self.fresh = None

    def on_event(self, event) -> None:
        """Apply one event (G-041)."""
        if isinstance(event, ev.RunStarted):
            self.clear()
            self.started = True
        self.clock = event.clock
        self.events += 1
        self.fresh = None
        if isinstance(event, ev.CorrespondenceBuilt):
            self.ends[event.correspondence] = (event.henry, event.eliza)
        elif isinstance(event, ev.CompetitionHeld):
            self._held(event)
        elif isinstance(event, ev.WorldviewChanged):
            self._changed(event)

    def _held(self, e) -> None:
        """A fight: the Old Worldview is the one before the newcomer entered (an entry
        without a fight logs its change first, D-040)."""
        old = tuple(n for n in self.worldview if n != e.newcomer)
        rival = tuple(n for n in old if n not in e.rivals) + (e.newcomer,)
        self.fights.append(Fight(self.events - 1, e.clock, e.newcomer, tuple(e.rivals),
                                 e.old_rating, e.rival_rating, e.chance, e.a, e.rival_wins,
                                 old, rival))
        self.fresh = self.fights[-1]

    def _changed(self, e) -> None:
        for name in e.removed:
            if name in self.worldview:
                self.worldview.remove(name)
        for name in e.added:
            if name not in self.worldview:
                self.worldview.append(name)
        self.changes.append({"added": tuple(e.added), "removed": tuple(e.removed),
                             "rating": e.rating, "coherent": e.coherent, "reason": e.reason,
                             "clock": e.clock})
        if self.fights and e.reason == "won" and self.fights[-1].change is None \
                and self.fights[-1].newcomer in e.added:
            self.fights[-1] = Fight(**{**self.fights[-1].__dict__, "change": e.rating})

    def snapshot_competitions(self) -> list:
        """The fights in `Snapshot.competitions`' form (G-041)."""
        return [{"newcomer": f.newcomer, "rivals": f.rivals, "old_rating": f.old_rating,
                 "rival_rating": f.rival_rating, "chance": f.chance, "rival_wins": f.rival_wins,
                 "a": f.a, "clock": f.clock} for f in self.fights]

    def banner(self) -> str:
        """The pane's first line (ANSWERS.md 11; G-041): "no competition in this run"
        until one is held, then the counts."""
        if not self.started:
            return "no run yet"
        if not self.fights:
            return NO_COMPETITION
        fought = sum(1 for f in self.fights if f.fought)
        free = len(self.fights) - fought
        return (f"{len(self.fights)} competition" + ("" if len(self.fights) == 1 else "s")
                + f": {fought} fight" + ("" if fought == 1 else "s")
                + f", {free} entered without a fight")

    def _side_cell(self, name: str, role: str) -> tuple:
        return (f"{name}: {arrow_text(*self.ends[name])}" if name in self.ends else name, role)

    def _fight_lines(self, fight: Fight, number: int) -> list:
        """One fight as lines (G-041): the heading, the two tables side by side (the
        old ones solid, the newcomer dashed, Fig. 3.11), then the ratings, the chance,
        the exponent and the winner."""
        fresh = self.fresh is fight
        key = f"competition {fight.index}"
        if not fight.fought:
            return [TextLine(f"{number}. codelet {fight.clock}: {fight.newcomer} entered without "
                             f"a fight; Worldview rating {fight.old_rating:.1f} to "
                             f"{fight.rival_rating:.1f}", DETAIL, key, fresh=fresh)]
        out = [TextLine(f"{number}. codelet {fight.clock}: {fight.newcomer} against "
                        f"{', '.join(fight.rivals)}", HEADING, key, fresh=fresh)]
        out.append(TextLine("", DETAIL, key, (("Old Worldview", HEADING),
                                              ("Rival Worldview", HEADING)), fresh))
        rows = max(len(fight.old), len(fight.rival))
        for k in range(rows):
            left = (self._side_cell(fight.old[k], WORLDVIEW) if k < len(fight.old)
                    else ("", DETAIL))
            if k < len(fight.rival):
                name = fight.rival[k]
                right = self._side_cell(name, SHADOW if name == fight.newcomer else WORLDVIEW)
            else:
                right = ("", DETAIL)
            out.append(TextLine("", DETAIL, key, (left, right), fresh))
        out.append(TextLine("", DETAIL, key, ((f"rating {fight.old_rating:.1f}", DETAIL),
                                              (f"rating {fight.rival_rating:.1f}", DETAIL)),
                            fresh))
        out.append(TextLine(f"chance {fight.chance:.3f} at a {fight.a:.2f}: {fight.winner}"
                            + (f"; rating after {fight.change:.1f}" if fight.change is not None
                               else ""), DETAIL, key, fresh=fresh))
        return out

    def lines(self) -> list:
        """The pane's text (G-041): the banner, then every competition in order."""
        out = [TextLine(self.banner(), HEADING if self.fights else MUTED)]
        for k, fight in enumerate(self.fights, 1):
            out += self._fight_lines(fight, k)
        return out

    def fight_of(self, key: str):
        """The fight a line's name refers to ("competition N", N the event index), or
        None (G-041)."""
        for fight in self.fights:
            if key == f"competition {fight.index}":
                return fight
        return None

    def tooltip(self, key: str) -> str:
        """The hover text of a fight's lines (G-041); empty for none."""
        fight = self.fight_of(key)
        return "" if fight is None else fight_text(fight, self.ends)

    def selection_of(self, key: str):
        """A click on a fight selects its newcomer as a correspondence in the shared
        selection (G-022, G-041), or nothing."""
        fight = self.fight_of(key)
        if fight is None:
            return None
        return ("correspondence", fight.newcomer, self.ends.get(fight.newcomer, ()))
