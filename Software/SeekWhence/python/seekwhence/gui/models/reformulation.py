"""The reformulation pane's model (P7), Qt-free: stage two as a storyboard, one episode
per refutation, each a list of cards in the order of pp. 87-126, with the freeze-dried
list and the threshold's course (PANES.md P7; G-039, G-040, G-041).

A refutation opens an episode: the hypothesis's validity falls to 0, `set-out --
validity: 0` [M86 p. 168], and the Gnoth-setter freeze-dries the old hypothesis, "kept on
a list of old hypotheses, along with the number of terms of the sequence it explained"
[M86 p. 91], makes the class at its top reigning [M86 p. 99] and lays out the gnoths,
`gnoths: (gnoth1 gnoth2 gnoth3)` [M86 p. 168]. Then the slip-or-stay check, `slip-check:
stayval: 15.0` and `best: (Y-group 4.0)` [M86 p. 176], or a strict class slipping at once,
`Countup --> S-group` [M86 p. 174]; the Reformulator's threshold and moves and the PROGRAM
built for the strongest pull [M86 pp. 105-109]; the gnoth operations, each shown as the
thesis shows its examples, the gnoths' brackets before and after with the glom names under
the groups [M86 pp. 110-119], and "operations in series", the successive parses stacked
[M86 p. 120]; the Bond-assessor's verdict on the bonds against the threshold, `doing task
Bond-assessor on (S-group 8.0)` [M86 pp. 124, 174]; the Gnoth-caster's casts and the
generalised form, `casts: ((S-group 1 2) (S-group 1 1))` [M86 pp. 124-125, 174]; and the
outcome: a candidate, `new hypoth candidate (S-group 1 2)` [M86 p. 174], or the failure of
the effort and a Slip-scout [M86 p. 126], or the blast [M86 p. 92].

The model is a view's helper (`seekwhence.gui.reformulation_view`), fed like the stats
model (G-016): `on_event(event, index)` for every event, then `update(snapshot)` with the
snapshot in force at it. The cards are built from the events alone, since every step of
stage two announces itself (DECISION D-055); the snapshot gives what the events do not
carry, the freeze-dried list and the threshold between the events that print it (G-040).
The before and after pictures of an operation are read off a `SocratoplasmModel` of the
model's own, fed the same events and snapshots, whose names, caps and ranges are exact at
every gnoth event (G-030, G-039). The engine contract is kept: the model reads the
snapshot's plain records and the events; it holds no engine object and calls nothing on
the `World` (ENGINE-API.md section 2).
"""

import dataclasses

from seekwhence.gui.models import platoplasm as pm
from seekwhence.gui.models.socratoplasm import SocratoplasmModel

__all__ = ["KINDS", "CARD_KINDS", "CARD_PAGES", "Group", "GnothPicture", "Card", "Episode",
           "RLine", "ReformulationModel", "picture", "picture_lines", "bracket_text",
           "series_table", "series_lines", "episode_title", "outcome_text",
           "threshold_points", "freeze_dried_lines", "pane_lines", "card_lines"]

# The events the pane is fed by (PANES.md P7), each of which lands on exactly one card.
KINDS = ("refuted", "freeze-dried", "gnoth-filled", "gnoths-set", "slip-check",
         "class-changed", "move-found", "reformulated", "operation-glom", "operation",
         "gnoth-gone", "gnoth-uncapped", "gnoth-capped", "operated", "bonds-assessed",
         "casts", "candidate", "cast-failed", "slip-scouted", "blasted")

# The ten cards of the storyboard, in the order of PANES.md P7, with the pages behind each
# (TASK.md item 11: "Cite pp. 87-126 by card").
CARD_KINDS = ("refutation", "freeze-dried", "gnoths-set", "slip-check", "moves",
              "operation", "assessor", "casts", "candidate", "failure", "blast")
CARD_PAGES = {
    "refutation": "[M86 pp. 87, 98, 168]",
    "freeze-dried": "[M86 pp. 91, 99]",
    "gnoths-set": "[M86 pp. 98, 168, 173, 176]",
    "slip-check": "[M86 pp. 99, 103-105, 126, 168, 174, 176]",
    "moves": "[M86 pp. 105-109, 174]",
    "operation": "[M86 pp. 109-120, 124]",
    "assessor": "[M86 pp. 124, 174]",
    "casts": "[M86 pp. 124-125, 174]",
    "candidate": "[M86 pp. 125, 174]",
    "failure": "[M86 p. 126]",
    "blast": "[M86 p. 92]",
}
CARD_TITLES = {
    "refutation": "refutation", "freeze-dried": "freeze-dried", "gnoths-set": "gnoths set",
    "slip-check": "slip-check", "moves": "Reformulator", "operation": "gnoth operation",
    "assessor": "Bond-assessor", "casts": "casts", "candidate": "candidate",
    "failure": "failure", "blast": "the blast",
}


# ---- the pictures [M86 pp. 110-120] ------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class Group:
    """One top-level member of a gnoth in a storyboard picture: its print `text`, `(1 1)`,
    and the `name` written under it, `glom3` or `glint8` [M86 pp. 110, 120] (G-039)."""
    text: str
    name: str


@dataclasses.dataclass(frozen=True)
class GnothPicture:
    """One gnoth of a picture [M86 pp. 110, 120] (G-039): its `name`, its `groups` in order
    and its `state`: `live`, `uncapped` (its cap suspended [M86 p. 113]), `gone` (`[]
    (disappears)` [M86 p. 120]) or `emerged` (a SPLIT's new gnoth, `(emerges)`
    [M86 p. 120])."""
    name: str
    groups: tuple
    state: str = "live"


def picture(socratoplasm: SocratoplasmModel) -> tuple:
    """The parse as a picture: one `GnothPicture` per live gnoth of the parse, in order,
    its top-level members as groups with their print values and names [M86 pp. 110, 120]
    (G-039). The catchall is left out, as `show` leaves it out (DECISION D-030)."""
    out = []
    for card in socratoplasm.gnoths():
        groups = tuple(Group(socratoplasm.member_text(m), m) for m in card.members)
        out.append(GnothPicture(card.name, groups, "uncapped" if card.uncapped else "live"))
    return tuple(out)


def bracket_text(gnoth: GnothPicture) -> str:
    """A gnoth's bracket as the thesis writes it, `[ (1 1) (2 2) ]` [M86 p. 110];
    `[] (disappears)` for a gnoth gone and `[ ... ] (emerges)` for a new one
    [M86 p. 120] (G-039)."""
    if gnoth.state == "gone":
        return "[] (disappears)"
    inner = " ".join(g.text for g in gnoth.groups)
    text = f"[ {inner} ]" if inner else "[ ]"
    if gnoth.state == "emerged":
        text += " (emerges)"
    elif gnoth.state == "uncapped":
        text += " (uncapped)"
    return text


def picture_lines(pic: tuple) -> list:
    """A picture as the thesis's storyboards write it, two lines per gnoth: `gnoth2: [ (1
    1) (2 2) ]` and, under each group, its name, `glom3 glom5` [M86 p. 110] (G-039)."""
    lines = []
    for gnoth in pic:
        head = f"{gnoth.name}: "
        if gnoth.state == "gone" or not gnoth.groups:
            lines.append(head + bracket_text(gnoth))
            continue
        text, names = head + "[ ", " " * len(head + "[ ")
        for i, group in enumerate(gnoth.groups):
            if i:
                text, names = text + " ", names + " "
            names += " " * (len(text) - len(names))
            text += group.text
            names += group.name
            if len(names) > len(text):
                text += " " * (len(names) - len(text))
        tail = " ]" + (" (emerges)" if gnoth.state == "emerged"
                       else " (uncapped)" if gnoth.state == "uncapped" else "")
        lines.append(text.rstrip() + tail)
        lines.append(names.rstrip())
    return lines


def _with_fates(before: tuple, after: tuple) -> tuple:
    """The after picture with the gnoths of `before` that it lacks marked `gone`, in
    their old places, and the new ones `emerged` [M86 p. 120] (G-039)."""
    names_after = {g.name for g in after}
    names_before = {g.name for g in before}
    out = []
    for gnoth in before:
        if gnoth.name not in names_after:
            out.append(dataclasses.replace(gnoth, groups=(), state="gone"))
    for gnoth in after:
        if gnoth.name not in names_before and gnoth.state == "live":
            gnoth = dataclasses.replace(gnoth, state="emerged")
        out.append(gnoth)
    out.sort(key=lambda g: (_position(g, before, after)))
    return tuple(out)


def _position(gnoth, before, after):
    for pic in (after, before):
        for i, g in enumerate(pic):
            if g.name == gnoth.name:
                return i
    return len(after)


# ---- the cards and the episodes [M86 pp. 87-126] -------------------------------------------------

@dataclasses.dataclass
class Card:
    """One card of the storyboard (G-039): its `kind` (one of `CARD_KINDS`), the `index` of
    the event that opened it (for the seek) and the `indices` of every event it took,
    the `clock` then, its `lines` as `(kind, text, tooltip)` triples, the `names` of the
    gnoths and gloms it shows (for the hover), and for an operation card its `operation`
    text, the `before` and `after` pictures and `reason` (None when carried out)
    [M86 pp. 110-120]; `open` while its closing event has not come."""
    kind: str
    index: int
    clock: int | None
    indices: list = dataclasses.field(default_factory=list)
    lines: list = dataclasses.field(default_factory=list)
    names: tuple = ()
    operation: str | None = None
    before: tuple | None = None
    after: tuple | None = None
    reason: str | None = None
    open: bool = True

    @property
    def title(self) -> str:
        """The card's title (G-039)."""
        return CARD_TITLES[self.kind]

    @property
    def pages(self) -> str:
        """The pages behind the card (G-039)."""
        return CARD_PAGES[self.kind]


@dataclasses.dataclass
class Episode:
    """One reformulation episode, from a refutation to a candidate, a failure or the blast
    [M86 pp. 87-126] (G-039): its `number` (from 1), the `index` of the event that opened
    it, the refuted `form`, `n`, `term` and `predicted` (None for an episode that began
    without a refutation), its `cards`, the `thresholds` as `(clock, value)` points, one per
    change of value, and `clock_end`, the clock of the last reading [M86 p. 105], the
    `outcome` (`candidate`, `failure`, `blast`, None while open) with
    its text, `rounds`, how many Reformulators ran, and `armed`, whether the
    Gnoth-setter has cleared the old threshold, from when the snapshots' readings count
    (G-040)."""
    number: int
    index: int
    form: str | None = None
    n: int | None = None
    term: int | None = None
    predicted: int | None = None
    cards: list = dataclasses.field(default_factory=list)
    thresholds: list = dataclasses.field(default_factory=list)
    outcome: str | None = None
    outcome_form: str | None = None
    rounds: int = 0
    clock_end: int | None = None
    armed: bool = False

    @property
    def closed(self) -> bool:
        """Whether the episode has ended (G-039)."""
        return self.outcome in ("candidate", "blast")

    def last(self) -> Card | None:
        """The last card, or None (G-039)."""
        return self.cards[-1] if self.cards else None


@dataclasses.dataclass(frozen=True)
class RLine:
    """One line the pane's list paints (G-041): `kind` (`episode`, `head`, `text`, `pic`,
    `note`, `ok`, `bad`, `series`), `text`, `tooltip`, the `index` of the event to seek to
    on a click, the `episode` number, the `card` position in it (None for an episode
    line), the `names` the hover selects, `grey` and `collapsed` (an episode folded)."""
    kind: str
    text: str
    tooltip: str = ""
    index: int | None = None
    episode: int | None = None
    card: int | None = None
    names: tuple = ()
    grey: bool = False
    collapsed: bool = False


class ReformulationModel:
    """The storyboard's state (see the module docstring; G-039, G-040): `episodes` in
    order, `freeze_dried` (`(form, terms explained)` pairs, oldest first), `reigning`,
    `threshold`, `terms`, `clock`, `events` (this run), `index` (the history index of the
    last event), `folded` (the numbers of the episodes folded; the newest is open),
    `show_series` (the operations-in-series table, off by default), `s_links` (the
    slipperiness table from the `start` event's configuration, for the slip-check card),
    `first_refutation` (the index of the run's first `refuted` event, which raises the
    pane, G-041) and the `socratoplasm` model the pictures are read from."""

    def __init__(self):
        self.show_series = False
        self.reset()

    def reset(self) -> None:
        """Forget the run (a `start` event) (Numbo GUI, `coderack_model.py`'s `reset`)."""
        self.episodes: list = []
        self.freeze_dried: list = []
        self.reigning = None
        self.threshold = None
        self.terms = 0
        self.clock = None
        self.events = 0
        self.index = -1
        self.folded: set = set()
        self.s_links: dict = {}
        self.first_refutation = None
        self.socratoplasm = SocratoplasmModel()
        self._snapshot = None
        self._pending = None            # the picture before an operation's first change

    # -- reading ---------------------------------------------------------------------------------

    @property
    def episode(self) -> Episode | None:
        """The newest episode, or None (G-039)."""
        return self.episodes[-1] if self.episodes else None

    def cards(self) -> list:
        """Every card of every episode, in order (G-039)."""
        return [card for episode in self.episodes for card in episode.cards]

    def card_of(self, index: int) -> Card | None:
        """The card that took the event at `index`, or None (G-039)."""
        for card in self.cards():
            if index in card.indices:
                return card
        return None

    def freeze_dried_list(self) -> tuple:
        """The old hypotheses, newest first, as `(form, terms explained)` pairs
        [M86 p. 91] (G-040)."""
        return tuple(reversed(self.freeze_dried))

    def slipperiness(self, frm: str, to: str) -> float:
        """The slipperiness of an s-link from the run's configuration, 0 where there is
        none [M86 p. 90] (DECISION D-038)."""
        return self.s_links.get((frm, to), 0.0)

    def is_open(self, number: int) -> bool:
        """Whether the episode is unfolded: every one but the newest is folded unless
        clicked open (PANES.md P7, "Episodes collapse; the newest is open") (G-041)."""
        return number not in self.folded

    def toggle_fold(self, number: int) -> None:
        """Fold or unfold an episode (G-041)."""
        if number in self.folded:
            self.folded.discard(number)
        else:
            self.folded.add(number)

    # -- feeding ----------------------------------------------------------------------------------

    def on_event(self, event, index: int | None = None) -> None:
        """Apply the event: open or extend the card it lands on (see the module
        docstring; G-039). `index` is the event's place in the history; by default the
        model counts from 0 at the `start` event."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self.s_links = {(frm, to): s for frm, to, s in dict(event.config).get("s_links", ())}
        self.events += 1
        self.index = index if index is not None else self.index + 1
        if kind == "term":
            self.terms = event.n
        elif kind == "chosen":
            self.clock = event.clock
        before = picture(self.socratoplasm) if kind in _OPERATION_KINDS else None
        self.socratoplasm.on_event(event)
        handler = self._HANDLERS.get(kind)
        if handler is not None:
            handler(self, event, before)

    def update(self, snapshot) -> None:
        """Take the snapshot in force (None before the first): the freeze-dried list, the
        reigning class and the threshold are its (G-040); the socratoplasm model takes it
        too. A snapshot already taken costs nothing."""
        if snapshot is None or snapshot is self._snapshot:
            return
        self._snapshot = snapshot
        self.socratoplasm.update(snapshot)
        ref = snapshot.reformulation
        self.freeze_dried = [(r.form, r.terms_explained) for r in ref.freeze_dried]
        self.reigning = ref.reigning
        self.threshold = ref.threshold
        self.terms = max(self.terms, len(snapshot.terms))
        episode = self.episode
        if episode is not None and not episode.closed and episode.armed:
            self._point(episode, snapshot.clock, ref.threshold)

    # -- the episodes -------------------------------------------------------------------------------

    def _open_episode(self, event=None) -> Episode:
        """A new episode, from a refutation or, when another event comes with no
        episode open, without one (G-039)."""
        episode = Episode(len(self.episodes) + 1, self.index)
        if event is not None:
            episode.form, episode.n = event.form, event.n
            episode.term, episode.predicted = event.term, event.predicted
        if self.episodes:
            self.folded.add(self.episodes[-1].number)
        self.episodes.append(episode)
        return episode

    def _current(self) -> Episode:
        """The episode an event belongs to: the newest, even closed (a late Capper), or a
        new one when there is none (G-039)."""
        return self.episode if self.episode is not None else self._open_episode()

    def _card(self, episode: Episode, kind: str, names=()) -> Card:
        """A new open card on the episode (G-039)."""
        card = Card(kind, self.index, self.clock, names=tuple(names))
        episode.cards.append(card)
        return card

    def _take(self, card: Card) -> None:
        card.indices.append(self.index)

    def _point(self, episode: Episode, clock, value) -> None:
        """A point of the threshold's course: kept when the value changes; the clock
        of the last reading is `clock_end` (G-040)."""
        if clock is None:
            return
        episode.clock_end = max(episode.clock_end or clock, clock)
        if value is None:
            return
        point = (clock, float(value))
        if not episode.thresholds or episode.thresholds[-1][1] != point[1]:
            episode.thresholds.append(point)

    # -- the handlers, one per card ------------------------------------------------------------------

    def _on_refuted(self, event, _before) -> None:
        """Card 1, the refutation: the term, the prediction, `set-out -- validity: 0`
        [M86 pp. 98, 168]."""
        if self.first_refutation is None:
            self.first_refutation = self.index
        episode = self._open_episode(event)
        card = self._card(episode, "refutation")
        self._take(card)
        card.lines = [
            ("text", f"{event.form} predicted {event.predicted} at term {event.n}; "
                     f"the term was {event.term}",
             "the hypothesis did not predict the term [M86 pp. 87, 98]"),
            ("bad", "set-out -- validity: 0",
             "the validity is set to 0 and the Gnoth-setter set out [M86 pp. 98, 168]"),
        ]
        card.open = False

    def _on_freeze_dried(self, event, _before) -> None:
        """Card 2, freeze-dried: the old form with the terms it explained [M86 p. 91] and
        the class made reigning [M86 p. 99]."""
        episode = self._current()
        if episode.closed:
            episode = self._open_episode()
        card = self._card(episode, "freeze-dried")
        self._take(card)
        pair = (event.form, event.explained)
        if not self.freeze_dried or self.freeze_dried[-1] != pair:
            self.freeze_dried.append(pair)
        self.reigning = event.reigning
        self.threshold = None
        episode.armed = True
        n = event.explained
        card.lines = [
            ("text", f"{event.form} ({n} term{'s' if n != 1 else ''}) kept on the list of "
                     f"old hypotheses", "freeze-dried [M86 p. 91] (DECISION D-035)"),
            ("text", f"reigning class: {event.reigning}",
             "the Platonic class at the top of the old hypothesis reigns [M86 p. 99]"),
        ]
        card.open = False

    def _on_gnoth_filled(self, event, _before) -> None:
        """Card 3, the gnoths set: one line per gnoth filled, new or kept [M86 p. 98]
        (DECISION D-036)."""
        episode = self._current()
        card = episode.last()
        if card is None or card.kind != "gnoths-set" or not card.open:
            card = self._card(episode, "gnoths-set")
        self._take(card)
        card.names = card.names + (event.name, event.glom)
        lo, hi = event.range
        card.lines.append(
            ("text", f"{event.name}  frame {event.frame}  range ({lo} {hi})  glom "
                     f"{event.glom}  {'new' if event.new else 'kept'}",
             "a gnoth filled to a frame of the old hypothesis [M86 p. 98] (DECISION D-036)"))

    def _on_gnoths_set(self, event, _before) -> None:
        """Card 3 closes: `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 pp. 168, 173, 176]."""
        episode = self._current()
        card = episode.last()
        if card is None or card.kind != "gnoths-set" or not card.open:
            card = self._card(episode, "gnoths-set")
        self._take(card)
        card.names = card.names + tuple(event.names)
        card.lines.append(("text", f"gnoths: ({' '.join(event.names)})",
                           "the Gnoth-setter's line [M86 pp. 168, 173, 176]"))
        frames = " ".join(f"{n}: frame {f} ({r[0]} {r[1]})"
                          for n, f, r in zip(event.names, event.frames, event.ranges))
        card.lines.append(("note", frames, "frames and ranges (DECISION D-036)"))
        card.open = False

    def _on_slip_check(self, event, _before) -> None:
        """Card 4, the slip-check, as P5's list prints it [M86 pp. 103-105]."""
        episode = self._current()
        card = self._card(episode, "slip-check")
        self._take(card)
        check = pm.SlipCheckCard(event.reigning, event.stayval, event.best, event.pressure,
                                 tuple(event.pressures), event.slipped, self.events)
        for line in pm.check_lines(check, self.slipperiness):
            card.lines.append(("text" if line.kind != "change" else "ok", line.text,
                               line.tooltip))
        if not event.slipped:
            card.open = False

    def _on_class_changed(self, event, _before) -> None:
        """Card 4 takes a change of class, `Countup --> S-group` [M86 pp. 99, 174]; a
        strict class slips at once, with no check (DECISION D-042)."""
        episode = self._current()
        card = episode.last()
        if card is None or card.kind != "slip-check" or not card.open:
            card = self._card(episode, "slip-check")
        self._take(card)
        change = pm.ClassChange(event.old, event.new, event.reason, self.events)
        card.lines.append(("ok", pm.change_text(change),
                           "the reigning class changes [M86 pp. 99, 104, 174] "
                           "(DECISION D-042)"))
        self.reigning = event.new
        self.threshold = None
        card.open = False

    def _on_slip_scouted(self, event, _before) -> None:
        """A Slip-scout's check lands on card 4 with its factor [M86 p. 126]
        (DECISION D-049)."""
        episode = self._current()
        card = episode.last()
        if card is None or card.kind != "slip-check":
            card = self._card(episode, "slip-check")
        self._take(card)
        scout = pm.Scout(event.before, event.after, event.factor, event.stayval)
        card.lines.append(("text", pm.scout_text(scout),
                           "the Slip-scout: the staying pressure scaled, the check again "
                           "[M86 p. 126] (DECISION D-049)"))
        card.open = False

    def _on_move_found(self, event, _before) -> None:
        """Card 5, the Reformulator's moves: the level, the pull between the neighbours,
        which one moves, the hold, the strength, the program [M86 pp. 105-109]
        (DECISIONS D-043, D-044)."""
        episode = self._current()
        card = episode.last()
        if card is None or card.kind != "moves" or not card.open:
            card = self._card(episode, "moves")
        self._take(card)
        card.names = card.names + (event.left, event.right, event.left_gnoth, event.right_gnoth)
        mover = event.left if event.mover == "left" else event.right
        card.lines.append(
            ("text", f"level {event.level}: {event.left} ({event.left_gnoth}) <- "
                     f"{pm.pressure_text(event.pull)} -> {event.right} ({event.right_gnoth})",
             "a pull between neighbours at this level of p. 107's order "
             "[M86 pp. 106-108]"))
        card.lines.append(
            ("text", f"{mover} moves: held {pm.pressure_text(event.held)}, strength "
                     f"{pm.pressure_text(event.strength)}",
             "the pull minus the hold of its own gnoth [M86 pp. 108-109] (DECISION D-043)"))
        card.lines.append(("pic", f"PROGRAM: {event.program}",
                           "the operation or PROGRAM built for the move [M86 pp. 109, 122, "
                           "174] (DECISION D-044)"))

    def _on_reformulated(self, event, _before) -> None:
        """Card 5 closes: the threshold [M86 p. 105] and the program given to a
        Gnoth-operator, or no action [M86 p. 105] (DECISION D-046)."""
        episode = self._current()
        episode.rounds += 1
        card = episode.last()
        if card is None or card.kind != "moves" or not card.open:
            card = self._card(episode, "moves")
        self._take(card)
        self.threshold = event.threshold
        self._point(episode, self.clock, event.threshold)
        card.lines.insert(0, ("text", f"threshold {event.threshold} in {event.kind_}",
                              "the threshold bond strength: 1 more than before, or a "
                              "fraction of the strongest bond [M86 p. 105] (DECISION D-046)"))
        if event.program is None:
            card.lines.append(("note", "no actions to be taken: a Bond-assessor follows",
                               "the Reformulator finds no pull above the threshold "
                               "[M86 p. 105]"))
        else:
            card.lines.append(("text", "a Gnoth-operator is set out with the program",
                               "[M86 pp. 105, 174]"))
        card.open = False

    def _operation_card(self, episode: Episode, before) -> Card:
        """The open operation card, or a new one with `before` as its picture (G-039)."""
        card = episode.last()
        if card is None or card.kind != "operation" or not card.open:
            card = self._card(episode, "operation")
            card.before = before
        return card

    def _on_operation_glom(self, event, before) -> None:
        """An operation's glom (CAPTURE, ENCLOSE, MERGE) [M86 pp. 112-113] (DECISION
        D-066)."""
        card = self._operation_card(self._current(), before)
        self._take(card)
        card.names = card.names + (event.name,)
        words = " ".join(event.type)
        card.lines.append(("note", f"{event.name} made ({words}) over "
                                   f"({' '.join(event.members)})",
                           "a glom made by the operation [M86 pp. 112-113] (DECISION D-066)"))

    def _on_gnoth_gone(self, event, before) -> None:
        """A gnoth emptied, `[] (disappears)` [M86 pp. 108, 120]."""
        card = self._operation_card(self._current(), before)
        self._take(card)
        card.names = card.names + (event.name,)
        card.lines.append(("note", f"{event.name} gone: {event.reason}",
                           "an emptied gnoth is destroyed [M86 pp. 108, 120]"))

    def _on_gnoth_uncapped(self, event, before) -> None:
        """A NO-OP's uncapping [M86 p. 113] (DECISION D-045)."""
        card = self._operation_card(self._current(), before)
        self._take(card)
        card.names = card.names + (event.name,)
        lo, hi = event.range
        card.lines.append(("note", f"{event.name} uncapped over ({lo} {hi})",
                           "its pseudo-glom suspended until a Capper recaps it "
                           "[M86 p. 113] (DECISION D-045)"))

    def _on_gnoth_capped(self, event, before) -> None:
        """The Capper's recap [M86 p. 113]: on the open operation card, or a card of its
        own when it comes later (the slow recap)."""
        episode = self._current()
        card = episode.last()
        late = card is None or card.kind != "operation" or not card.open
        card = self._operation_card(episode, before)
        self._take(card)
        card.names = card.names + (event.name, event.glom)
        lo, hi = event.range
        card.lines.append(("text", f"Capper: {event.name} recapped with {event.glom} over "
                                   f"({lo} {hi}), claiming ({' '.join(event.claimed)})",
                           "the Capper claims the gloms in the gnoth's range [M86 p. 113]"))
        if late:
            card.operation = f"(CAPPER {event.name})"
            card.after = _with_fates(card.before, picture(self.socratoplasm))
            card.open = False

    def _on_operation(self, event, before) -> None:
        """Card 6 closes: the operation, carried out or why not, with its after picture
        [M86 pp. 110-120] (DECISION D-067)."""
        card = self._operation_card(self._current(), before)
        self._take(card)
        card.operation = event.operation
        card.reason = event.reason
        card.names = card.names + tuple(event.gnoths) + tuple(event.gone) \
            + tuple(event.created) + tuple(event.made)
        if event.reason is None:
            card.lines.append(("ok", "carried out", "[M86 pp. 110-120]"))
        else:
            card.lines.append(("bad", f"not carried out: {event.reason}",
                               "the operation found the gnoths changed [M86 pp. 42, 124]"))
        for name in event.created:
            card.lines.append(("note", f"{name} emerges", "a SPLIT's new gnoth [M86 p. 120]"))
        card.after = _with_fates(card.before, picture(self.socratoplasm))
        card.open = False

    def _on_operated(self, event, before) -> None:
        """The Gnoth-operator's summary lands on its last operation card: the steps,
        whether it acted (the threshold down by 1) and where it stopped [M86 p. 124]."""
        episode = self._current()
        card = episode.last()
        if card is None or card.kind != "operation":
            card = self._operation_card(episode, before)
            card.after = card.before
            card.operation = event.program
        self._take(card)
        n = event.steps
        text = f"Gnoth-operator: {n} step{'s' if n != 1 else ''} of {event.program}"
        if event.acted:
            text += f"; acted, threshold {event.threshold}"
        if event.stopped:
            text += f"; stopped: {event.stopped}"
        card.lines.append(("text", text, "the Gnoth-operator ran the program; one that "
                                         "acts lowers the threshold by 1 [M86 pp. 121-124] "
                                         "(DECISION D-046)"))
        if event.threshold is not None:
            self.threshold = event.threshold
            self._point(episode, self.clock, event.threshold)
        card.open = False

    def _on_bonds_assessed(self, event, _before) -> None:
        """Card 7, the Bond-assessor [M86 p. 124] (DECISION D-047)."""
        episode = self._current()
        card = self._card(episode, "assessor")
        self._take(card)
        card.lines = [
            ("text", f"doing task Bond-assessor on ({event.kind_} {event.threshold})",
             "the task line [M86 p. 174]"),
            ("text", f"strongest bond {pm.pressure_text(event.strongest)} against "
                     f"threshold {event.threshold}",
             "bonds strong enough warrant further reforms [M86 p. 124] (DECISION D-047)"),
            ("ok" if event.more else "text",
             "another Reformulator follows" if event.more else "a Gnoth-caster follows",
             "[M86 p. 124]"),
        ]
        card.open = False

    def _on_casts(self, event, _before) -> None:
        """Card 8, the casts [M86 pp. 124-125] (DECISION D-048)."""
        episode = self._current()
        card = self._card(episode, "casts", names=event.gnoths)
        self._take(card)
        card.lines = [("pic", f"casts: {event.casts}",
                       f"each gnoth cast as a form of {event.kind_} [M86 pp. 124-125, 174]")]
        if event.generalised is not None:
            card.lines.append(("pic", event.generalised,
                               "the generalised form [M86 p. 125]"))
        else:
            card.lines.append(("bad", f"no form: {event.reason}",
                               "the casts did not generalise [M86 p. 125] (DECISION D-048)"))
        if event.last:
            card.lines.append(("note", "the last gnoth left out as the one that shows promise",
                               "[M86 p. 125]"))
        card.open = False

    def _on_candidate(self, event, _before) -> None:
        """Card 9, the candidate [M86 pp. 125, 174]; the episode ends."""
        episode = self._current()
        card = self._card(episode, "candidate")
        self._take(card)
        card.lines = [("ok", f"new hypoth candidate {event.form}", "[M86 p. 174]"),
                      ("text", f"postdicts the {self.terms} terms; the new hypothesis "
                               f"under {event.kind_}",
                       "a candidate must postdict the known terms [M86 p. 125] "
                       "(DECISION D-034)")]
        card.open = False
        episode.outcome, episode.outcome_form = "candidate", event.form

    def _on_cast_failed(self, event, _before) -> None:
        """Card 9, the failure [M86 p. 126] (DECISION D-050)."""
        episode = self._current()
        card = self._card(episode, "failure")
        self._take(card)
        n = event.failures
        card.lines = [("bad", f"the reformulation effort has failed: {event.reason}",
                       "[M86 p. 126]"),
                      ("text", f"failure {n} of this episode; "
                               + ("the blast follows" if event.blast
                                  else "a Slip-scout is set out"),
                       "[M86 p. 126] (DECISION D-050)")]
        card.open = False
        episode.outcome = "failure"

    def _on_blasted(self, event, _before) -> None:
        """Card 10, the blast [M86 p. 92] (DECISION D-050); the episode ends."""
        episode = self._current()
        card = self._card(episode, "blast", names=tuple(event.gnoths) + tuple(event.gloms))
        self._take(card)
        card.lines = [("bad", "blasting away all gnoths and gloms", "[M86 p. 92]"),
                      ("text", f"gnoths destroyed: {' '.join(event.gnoths) or 'none'}",
                       "[M86 p. 92]"),
                      ("text", f"gloms burst: {' '.join(event.gloms) or 'none'}",
                       "[M86 p. 92]"),
                      ("text", f"bonds that survive: {' '.join(event.bonds) or 'none'}",
                       "the glints and their bonds remain [M86 pp. 39, 92]")]
        card.open = False
        episode.outcome = "blast"
        self.reigning, self.threshold = None, None

    _HANDLERS = {
        "refuted": _on_refuted, "freeze-dried": _on_freeze_dried,
        "gnoth-filled": _on_gnoth_filled, "gnoths-set": _on_gnoths_set,
        "slip-check": _on_slip_check, "class-changed": _on_class_changed,
        "slip-scouted": _on_slip_scouted, "move-found": _on_move_found,
        "reformulated": _on_reformulated, "operation-glom": _on_operation_glom,
        "gnoth-gone": _on_gnoth_gone, "gnoth-uncapped": _on_gnoth_uncapped,
        "gnoth-capped": _on_gnoth_capped, "operation": _on_operation,
        "operated": _on_operated, "bonds-assessed": _on_bonds_assessed,
        "casts": _on_casts, "candidate": _on_candidate, "cast-failed": _on_cast_failed,
        "blasted": _on_blasted,
    }


_OPERATION_KINDS = ("operation-glom", "gnoth-gone", "gnoth-uncapped", "gnoth-capped",
                    "operation", "operated")


# ---- the texts and the lines ------------------------------------------------------------------------

def episode_title(episode: Episode) -> str:
    """`episode 1: (Countup 1) refuted at term 3 (1 where 3 was predicted)`, or `episode
    2: begun without a refutation` [M86 pp. 87, 98] (G-039)."""
    if episode.form is None:
        return f"episode {episode.number}: begun without a refutation"
    return (f"episode {episode.number}: {episode.form} refuted at term {episode.n} "
            f"({episode.term} where {episode.predicted} was predicted)")


def outcome_text(episode: Episode) -> str:
    """The episode's outcome in words: `candidate (S-group 1 2)`, `failed, a Slip-scout`,
    `the blast`, `in progress` [M86 pp. 92, 125-126] (G-039)."""
    if episode.outcome == "candidate":
        return f"candidate {episode.outcome_form}"
    if episode.outcome == "blast":
        return "the blast"
    if episode.outcome == "failure":
        return "failed, a Slip-scout"
    return "in progress"


def threshold_points(episode: Episode | None) -> tuple:
    """The threshold against the clock within the episode, `(clock, value)` points
    [M86 pp. 105, 124] (G-040)."""
    return tuple(episode.thresholds) if episode is not None else ()


def freeze_dried_lines(model: ReformulationModel) -> list:
    """The freeze-dried list, newest first, each `(Countup 1) (2 terms)` [M86 p. 91]
    (DECISION D-035; G-040)."""
    return [f"{form} ({n} term{'s' if n != 1 else ''})"
            for form, n in model.freeze_dried_list()]


def series_table(episode: Episode) -> tuple:
    """The episode's operations in series, as p. 120 stacks them: the `columns` (the
    gnoths named, in the order they appear) and the `rows`, `(label, cells)` with the
    initial state first and one row per operation, each cell the gnoth's bracket text or
    an empty string [M86 p. 120] (G-039)."""
    cards = [c for c in episode.cards if c.kind == "operation" and c.after is not None]
    if not cards:
        return (), ()
    columns = []
    pictures = [("initial state", cards[0].before or ())]
    pictures += [(c.operation or "operation", c.after) for c in cards]
    for _label, pic in pictures:
        for gnoth in pic:
            if gnoth.name not in columns:
                columns.append(gnoth.name)
    rows = []
    for label, pic in pictures:
        by_name = {g.name: g for g in pic}
        cells = tuple(bracket_text(by_name[c]) if c in by_name else "" for c in columns)
        rows.append((label, cells))
    return tuple(columns), tuple(rows)


def series_lines(episode: Episode) -> list:
    """The series table as lines: the gnoth names as a header, then for each row its
    label and the brackets in columns [M86 p. 120] (G-039)."""
    columns, rows = series_table(episode)
    if not columns:
        return []
    widths = [max([len(c)] + [len(r[1][i]) for r in rows]) for i, c in enumerate(columns)]
    lines = ["  ".join(c.ljust(w) for c, w in zip(columns, widths)).rstrip()]
    for label, cells in rows:
        lines.append(label)
        lines.append("  ".join(cell.ljust(w) for cell, w in zip(cells, widths)).rstrip())
    return lines


def card_lines(episode: Episode, position: int, card: Card) -> list:
    """One card's `RLine`s: the heading with the card's pages, its lines, and for an
    operation card the before and after pictures in the thesis's style [M86 pp. 110-120]
    (G-039, G-041)."""
    tip = f"{card.title} {card.pages}; click to seek to its event"
    names = tuple(dict.fromkeys(card.names))
    common = dict(index=card.index, episode=episode.number, card=position, names=names)
    head = card.title
    if card.kind == "operation" and card.operation:
        head = f"{card.title}  {card.operation}"
    lines = [RLine("head", head, tip, **common)]
    if card.kind == "operation" and card.before is not None:
        lines.append(RLine("note", "before:", "the initial state [M86 p. 110]", **common))
        for text in picture_lines(card.before):
            lines.append(RLine("pic", "  " + text, "[M86 p. 110]", **common))
    for kind, text, tooltip in card.lines:
        lines.append(RLine(kind, text, tooltip, **common))
    if card.kind == "operation" and card.after is not None:
        lines.append(RLine("note", "after:", "the final state [M86 p. 110]", **common))
        for text in picture_lines(card.after):
            lines.append(RLine("pic", "  " + text, "[M86 p. 110]", **common))
    return lines


def pane_lines(model: ReformulationModel) -> tuple:
    """The lines of the storyboard: per episode its title line (folded episodes show
    the title and the outcome only; a click unfolds), then its cards, and the
    operations in series when `show_series` is on [M86 pp. 87-126] (G-039, G-041)."""
    lines = []
    for episode in model.episodes:
        folded = not model.is_open(episode.number)
        title = episode_title(episode)
        text = f"{title}  —  {outcome_text(episode)}" if folded or episode.closed else title
        lines.append(RLine("episode", text, "click to fold or unfold the episode; the "
                           "episode runs from the refutation to a candidate, a failure or "
                           "the blast [M86 pp. 87-126]", index=episode.index,
                           episode=episode.number, collapsed=folded))
        if folded:
            continue
        for i, card in enumerate(episode.cards):
            lines.extend(card_lines(episode, i, card))
        if model.show_series:
            series = series_lines(episode)
            if series:
                lines.append(RLine("head", "operations in series", "[M86 p. 120]",
                                   episode=episode.number))
                for text in series:
                    lines.append(RLine("series", "  " + text, "[M86 p. 120]",
                                       episode=episode.number))
        if not episode.closed:
            lines.append(RLine("note", f"outcome: {outcome_text(episode)}",
                               "[M86 pp. 92, 125-126]", episode=episode.number, grey=True))
    if not lines:
        lines.append(RLine("note", "no refutation yet", "stage two begins at a refutation "
                           "[M86 p. 87]", grey=True))
    return tuple(lines)
