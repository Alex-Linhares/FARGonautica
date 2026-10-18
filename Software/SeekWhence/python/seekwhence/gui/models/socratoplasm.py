"""The socratoplasm pane's model (P3), Qt-free: the parse in bracket notation, one card per
gnoth with the fields `show` prints, the catchall, and the template with its check line and
its fate (PANES.md P3; G-030, G-031, G-032).

The socratoplasm holds the gnoths, which "capture the current 'view' -- parse or
parenthesization -- of the sequence" [M86 p. 46]. Each gnoth has the fields of the
Gnoth-maker's example [M86 p. 78] and of `show` [M86 pp. 168, 175]; the catchall is "a
special gnoth that simply serves as a repository for non-troublesome terms" [M86 p. 98]
(DECISION D-030), which `show` does not list. The parse is what `show-parse` prints,
`((1 2) (1))` [M86 p. 174] (DECISION D-051). The template has the four fields the
transcript prints, `(form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)`, and
the evaluator's check, `check-cycle template (Cycle 3 2 ((3 7)))` [M86 p. 167]; it is
passed or "rejected and abandoned" [M86 p. 74], modified or left alone by a review
[M86 p. 75], and new terms are "'filtered' past the template" [M86 p. 74]. Figure 1 draws
the gnoths as rounded rectangles in a row over their gloms and glints [M86 p. 102].

The model is a view's helper (`seekwhence.gui.socratoplasm_view`), fed like the stats model
(G-016): `on_event(event)` for every event, then `update(snapshot)` with the snapshot in
force at that event. The snapshot decides which gnoths exist and what their fields say; the
gnoth events since it (`gnoth`, `catchall`, `gnoth-filled`, `gnoths-set`, `gnoth-gone`,
`gnoth-uncapped`, `gnoth-capped`, `operation`, with the `top-down` and `operation-glom`
events that make the caps) add, remove, fill and recap what they announce, so that the
names, frames, caps and ranges, and the parse, are exact at every event. The fields the
engine changes silently between task boundaries (the bond values, the equivalence type, a
cast's form [M86 pp. 98-100, 125]) are not in the events: a card touched by an event since
the snapshot is `confirmed=False` until the next snapshot, and the view says so (G-030).
The template is kept from its events alone, since its fate is not in the snapshot once it
is gone (G-032). A gnoth that disappears stays as a ghost fading over `FADE` events, as the
cytoplasm's gloms do (G-021, G-030).

The engine contract is kept: the model reads the snapshot's plain records and the events;
it holds no engine object and calls nothing on the `World` (ENGINE-API.md section 2).
"""

import dataclasses

from seekwhence.forms import flatten, value_text
from seekwhence.gui.models.cytoplasm import FADE, GLINT_WIDTH, SPACING
from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.snapshot import GnothRecord, TemplateRecord
from seekwhence.operations import OPERATION_GLOMS

__all__ = ["BOX_PAD_X", "BOX_HEIGHT", "ROW_STEP", "SILENT_ARENA", "SILENT_TYPES", "GnothCard", "TemplateCard", "GnothBox",
           "SocratoplasmModel", "card_fields", "card_lines", "bonds_text", "groups_text",
           "form_field_text", "parse_text", "show_block", "template_lines", "fate_text",
           "gnoth_boxes", "strip_extent", "card_tooltip", "CardLine", "head_text", "pane_lines"]

# The gnoth rectangles over the cytoplasm's columns (G-031): a box spans its range's
# glints, `position * SPACING` as the cytoplasm lays them out (G-022), plus this padding.
BOX_PAD_X = 10
BOX_HEIGHT = 34
ROW_STEP = 40          # a second row, when two boxes overlap
# The kinds the catchall and an uncapped gnoth are drawn in (G-031).
GNOTH_KIND, CATCHALL_KIND = "gnoth", "catchall"
# The arena whose tasks change the gnoths' fields with no event (DECISION D-057; G-030),
# and the one platoplasm task that does too: the Slip-scout's slip-or-stay check
# re-assesses the bonds and may move every gnoth to a new class [M86 pp. 104-105].
SILENT_ARENA = "socratoplasm"
SILENT_TYPES = ("Slip-scout",)


@dataclasses.dataclass(frozen=True)
class GnothCard:
    """One gnoth's card [M86 pp. 78, 168, 175] (G-030): its `name`, `frame`, `plato_class`,
    `glom` (its gnoth-glom's name, None while uncapped [M86 p. 113]), `state`, `range`,
    `members` (its glom collection's names), `terms`, `parse_print` (`((1 2) 3)`
    [M86 p. 97]), `form_text` (the `form` field as printed), the four reformulation fields
    `external_bonds`, `internal_bonds` (`(bond name, value)` pairs), `equivalence_type`
    and `groups`, each None until set [M86 p. 175]; `catchall` and its `glints`
    [M86 p. 98]; `fields`, every field as `show` prints it, in its order; `made_at` (the
    model's event count when it appeared), `gone_at` and `reason` once it has gone
    [M86 p. 108]; and `confirmed`, whether the fields come from a snapshot (True) or were
    rebuilt from the events since it (False)."""
    name: str
    frame: int | None
    plato_class: str | None
    glom: str | None
    state: str
    range: tuple
    members: tuple
    terms: tuple
    parse_print: str
    form_text: str
    external_bonds: tuple | None
    internal_bonds: tuple | None
    equivalence_type: str | None
    groups: tuple | None
    catchall: bool
    glints: tuple
    fields: tuple
    made_at: int | None = None
    gone_at: int | None = None
    reason: str | None = None
    confirmed: bool = True

    @property
    def alive(self) -> bool:
        """Whether the gnoth exists (not a ghost) (G-030)."""
        return self.gone_at is None

    @property
    def uncapped(self) -> bool:
        """Whether the gnoth has no cap, "'uncapped' -- have its pseudo-glom suspended"
        [M86 p. 113]."""
        return self.glom is None and not self.catchall


@dataclasses.dataclass(frozen=True)
class TemplateCard:
    """The template [M86 pp. 73-76] (G-032): its `form`, `state` (`working`, `accepted`,
    `abandoned`), `coverage`, `glom` (the template glom's name, or None), `from_glom`,
    `fields_text` (the four fields as printed [M86 p. 167]), `check` (the evaluator's
    printed line, empty when nothing is printed, None before the check) and `passed`,
    `filtered` (`(n, term, fits)` for every term filtered past it [M86 p. 74]),
    `modified_from` (the old form of a review's modification [M86 p. 75]), `reason` and
    `by` of an abandonment, `blast` (the gloms a Template-resolver burst [M86 p. 75]),
    `immediate` (made by the scout itself, DECISION D-060) and `made_at`."""
    form: str
    state: str
    coverage: tuple
    glom: str | None
    from_glom: str
    fields_text: str
    check: str | None = None
    passed: bool | None = None
    filtered: tuple = ()
    modified_from: str | None = None
    reason: str | None = None
    by: str | None = None
    blast: tuple = ()
    immediate: bool = False
    made_at: int | None = None


@dataclasses.dataclass(frozen=True)
class GnothBox:
    """One gnoth's rectangle over the cytoplasm's columns, as Figure 1's top row
    [M86 p. 102] (G-031): `name`, `kind` (`gnoth` or `catchall`), `left`, `right` and
    `row` in the cytoplasm's units (G-022), `label` (the name), `sub` (the terms it
    covers), `ghost` (fading, 1.0 to 0.0; 0.0 for a live gnoth), `uncapped`, `selected`
    and `confirmed`."""
    name: str
    kind: str
    left: float
    right: float
    row: int
    label: str
    sub: str
    ghost: float
    uncapped: bool
    selected: bool
    confirmed: bool

    @property
    def width(self) -> float:
        """The box's width (G-031)."""
        return self.right - self.left


# ---- the printed formats [M86 pp. 168, 175] -------------------------------------------------

def _num(value) -> str:
    return str(int(value)) if value == int(value) else str(value)


def bonds_text(bonds) -> str:
    """A bond list as printed, `((bond2 0) (bond3 -5))` [M86 p. 175], `nil` when empty
    (`gnoths._bonds_text`)."""
    if not bonds:
        return "nil"
    return "(" + " ".join(f"({name} {_num(value)})" for name, value in bonds) + ")"


def groups_text(groups) -> str:
    """The `groups` field as printed, `((glint1 glint2))` [M86 p. 168], `nil` when empty
    (`gnoths._groups_text`)."""
    if not groups:
        return "nil"
    return "(" + " ".join("(" + " ".join(group) + ")" for group in groups) + ")"


def form_field_text(structure) -> str:
    """The `form` field of a gnoth made over one dubbed glom: its structure field as a
    list of `(form purity)` pairs, `(((Cycle 3 2 (3 7)) pure))` [M86 p. 168]; `nil` for
    none (`gnoths._form_field_text`; DECISION D-066)."""
    if not structure:
        return "nil"
    return "(" + " ".join(f"({form} {purity})" for form, purity in structure) + ")"


def card_fields(card: GnothCard) -> tuple:
    """The fields `show` prints for a card, in its order, as (field, text) pairs: the four
    reformulation fields first when set [M86 p. 175], then class, name, frame,
    plato-class, glom, notes, form, state, range [M86 p. 168] (`Gnoth.show_fields`)."""
    out = []
    if card.external_bonds is not None:
        out.append(("external-bonds", bonds_text(card.external_bonds)))
    if card.internal_bonds is not None:
        out.append(("internal-bonds", bonds_text(card.internal_bonds)))
    if card.equivalence_type is not None:
        out.append(("equivalence-type", card.equivalence_type))
    if card.groups is not None:
        out.append(("groups", groups_text(card.groups)))
    out.extend([
        ("class", "Gnoths"),
        ("name", card.name),
        ("frame", "nil" if card.frame is None else str(card.frame)),
        ("plato-class", card.plato_class or "nil"),
        ("glom", card.glom or "nil"),
        ("notes", "nil"),
        ("form", card.form_text),
        ("state", card.state),
        ("range", f"({card.range[0]} {card.range[1]})"),
    ])
    return tuple(out)


def card_lines(card: GnothCard) -> list:
    """The card's block of `show`, one `field: value` line per field [M86 p. 168]
    (`Gnoth.show_lines`)."""
    return [f"{field}: {text}" for field, text in card.fields]


def parse_text(cards) -> str:
    """The parse as `show-parse` prints it, `((1 2) (1))` [M86 p. 174]: the gnoths'
    parse-prints in one pair of parentheses (`gnoths.parse_text`; DECISION D-051)."""
    return "(" + " ".join(card.parse_print for card in cards) + ")"


def show_block(cards) -> list:
    """The `gnoths:` block of `show`: the heading, a blank line, and each gnoth's fields,
    gnoths separated by a blank line [M86 pp. 168, 179]; the catchall is not listed
    (`gnoths.show_gnoths_lines`; DECISION D-030)."""
    lines = ["gnoths:", ""]
    for i, card in enumerate(cards):
        if i:
            lines.append("")
        lines.extend(card_lines(card))
    return lines


def fate_text(template: TemplateCard | None) -> str:
    """The template's fate in words [M86 pp. 74-75] (G-032): `working`, `accepted`,
    `abandoned by the Template-evaluator: the terms no longer fit`, `modified from
    (S-group 1 n)`, `resolved by a blast of 3 gloms`; empty for none."""
    if template is None:
        return ""
    if template.state == "abandoned":
        text = "abandoned"
        if template.by:
            text += f" by the {template.by}"
        if template.reason:
            text += f": {template.reason}"
        return text
    parts = [template.state]
    if template.modified_from:
        parts.append(f"modified from {template.modified_from}")
    if template.blast:
        n = len(template.blast)
        parts.append(f"resolved by a blast of {n} glom{'s' if n != 1 else ''}")
    return ", ".join(parts)


def template_lines(template: TemplateCard | None) -> list:
    """The template's lines for the pane (G-032): `template: ` and the four fields
    [M86 p. 167]; the check line, or `check: passed` / `check: failed` when nothing was
    printed (DECISION D-027); the fate; one line per term filtered, `term 3 (3) fits` or
    `does not fit` [M86 p. 74]. `no template` for none."""
    if template is None:
        return ["no template"]
    lines = [f"template: {template.fields_text}"]
    if template.check is not None:
        lines.append(template.check or f"check: {'passed' if template.passed else 'failed'}")
    lines.append(f"fate: {fate_text(template)}")
    for n, term, fits in template.filtered:
        lines.append(f"term {n} ({term}) {'fits' if fits else 'does not fit'}")
    return lines


def card_tooltip(card: GnothCard) -> str:
    """A gnoth's tooltip: its fields as `show` prints them [M86 p. 168], the terms it
    covers, and whether the fields wait for the next task boundary (G-030)."""
    lines = list(card_lines(card))
    if card.catchall:
        lines.append("the catchall: a repository for non-troublesome terms [M86 p. 98]")
        lines.append("glints: " + " ".join(card.glints))
    lines.append("terms: " + " ".join(str(t) for t in card.terms))
    if card.uncapped:
        lines.append("uncapped: its pseudo-glom is suspended until a Capper recaps it "
                     "[M86 p. 113]")
    if not card.alive:
        lines.append(f"gone: {card.reason}")
    if not card.confirmed:
        lines.append("from the events since the last task boundary; the next one confirms "
                     "the fields")
    return "\n".join(lines)


# ---- the model ----------------------------------------------------------------------------------

class SocratoplasmModel:
    """The gnoths, the catchall and the template in force (see the module docstring;
    G-030, G-031, G-032). `cards`: every gnoth by name, the catchall included, live and
    ghost; `template`; `reigning`; `events` (this run); `corrections`: the snapshots whose
    names, frames, caps or ranges disagreed with the cards built from the events;
    `selection`: the shared selection (G-025)."""

    def __init__(self, selection: Selection | None = None):
        self.selection = selection if selection is not None else Selection()
        self.reset()

    def reset(self) -> None:
        """Forget the run (a `start` event) (Numbo GUI, `coderack_model.py`'s `reset`)."""
        self.cards: dict = {}
        self.template = None
        self.reigning = None
        self.terms = 0
        self.events = 0
        self.corrections = 0
        self._snapshot = None
        self._gloms: dict = {}        # glom name -> (members, print_value, type words)
        self._structures: dict = {}   # glom name -> the dubbed structure, as (form, purity)
        self._glints: dict = {}       # glint name -> (value, position)
        self._caps: list = []         # pseudo gloms made since the last gnoth event, unassigned
        self._running = None          # the arena of the task running (G-030)
        self._running_type = None     # its type

    # -- reading ---------------------------------------------------------------------------------

    def gnoths(self) -> tuple:
        """The live gnoths of the parse, in sequence order (which is frame order), the
        catchall left out: what `show` lists under `gnoths:` (`gnoths.gnoths`; DECISION
        D-030)."""
        listed = [c for c in self.cards.values() if c.alive and not c.catchall]
        return tuple(sorted(listed, key=lambda c: (c.range[0], c.frame or 0)))

    @property
    def catchall(self) -> GnothCard | None:
        """The live catchall gnoth, or None [M86 p. 98] (DECISION D-030)."""
        for card in self.cards.values():
            if card.alive and card.catchall:
                return card
        return None

    def ghosts(self) -> tuple:
        """The gnoths gone within the last `FADE` events (G-030)."""
        return tuple(c for c in self.cards.values() if not c.alive)

    @property
    def parse(self) -> str:
        """The parse as `show-parse` prints it [M86 p. 174] (DECISION D-051)."""
        return parse_text(self.gnoths())

    def show_block(self) -> list:
        """The `gnoths:` block of `show` for the cards in force [M86 pp. 168, 179]: what
        "Copy as `show`" copies (PANES.md P3; G-030)."""
        return show_block(self.gnoths())

    def member_text(self, name: str) -> str:
        """The print value of a gnoth's member as the storyboards write it under a
        gnoth's bracket, `(1 1)` over `glom3`, `5` over `glint8` [M86 pp. 110, 120]: a
        glom's print-value or a glint's value, from the snapshot's records and the
        events since; an empty string for a name not known (G-039)."""
        if name in self._gloms:
            print_value = self._gloms[name][1]
            return value_text(tuple(print_value)) if print_value is not None else ""
        if name in self._glints:
            return str(self._glints[name][0])
        return ""

    def card(self, name: str) -> GnothCard | None:
        """The card of a gnoth, live or ghost, or None (G-030)."""
        return self.cards.get(name)

    def under(self, glom: str | None) -> tuple:
        """Every element under a glom, at any depth: its subgloms and its glints
        [M86 p. 62], from the gloms the snapshot and the events describe (G-025)."""
        if glom is None or glom not in self._gloms:
            return ()
        out = []
        for member in self._gloms[glom][0]:
            out.append(member)
            out.extend(self.under(member))
        return tuple(out)

    def select_gnoth(self, name: str | None) -> tuple:
        """Select, in the shared selection, a gnoth with its gnoth-glom and everything
        under it (a second click clears); None clears; returns the names (PANES.md P3:
        "Click a gnoth to select it and its glom and members in P2"; G-025, G-031)."""
        card = self.cards.get(name) if name is not None else None
        if card is None:
            self.selection.clear()
            return ()
        if card.catchall:
            members = tuple(card.glints)
        else:
            members = ((card.glom,) + self.under(card.glom)) if card.glom else ()
        self.selection.toggle("gnoth", name, members)
        return members

    def is_selected(self, name: str) -> bool:
        """Whether the gnoth, or its gnoth-glom, is in the shared selection (G-025)."""
        card = self.cards.get(name)
        return self.selection.includes(name) or (
            card is not None and card.glom is not None and self.selection.includes(card.glom))

    # -- the events ------------------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply what the event announces (see the module docstring; G-030, G-032)."""
        kind = event.kind
        if kind == "start":
            self.reset()
        self.events += 1
        if kind == "term":
            self.terms = event.n
        if kind == "chosen":
            self._running, self._running_type = event.arena, event.type
        elif self._running == SILENT_ARENA or self._running_type in SILENT_TYPES:
            self._unconfirm()
        handler = self._HANDLERS.get(kind)
        if handler is not None:
            handler(self, event)
        self._prune_ghosts()

    def _unconfirm(self) -> None:
        """A socratoplasm task is running: it may change the cards' fields with no event
        (the bond values, a cast's form, the plato-class of a slip [M86 pp. 99-100, 105,
        125]), so every live card waits for the next task boundary (G-030)."""
        for name, card in self.cards.items():
            if card.alive and card.confirmed:
                self.cards[name] = dataclasses.replace(card, confirmed=False)

    def _on_glint(self, event) -> None:
        self._glints[event.name] = (event.value, event.position)

    def _on_top_down(self, event) -> None:
        """A pseudo-glom: a cap to pair with the gnoth event that follows. A Gnoth-setter's
        cap clears its span first [M86 p. 98] (DECISION D-036): the catchall and every
        other gnoth overlapping the frame are destroyed, silently, but for the gnoth that
        begins where the frame begins, which is filled afresh (`slipping.set_gnoths`)."""
        self._gloms[event.name] = (tuple(event.members), event.print_value, ("pseudo",))
        if event.pseudo:
            self._caps.append(event.name)
        if event.pseudo and self._running_type == "Gnoth-maker":
            self._template_glom_gone()
        if event.pseudo and self._running_type == "Gnoth-setter":
            lo, hi = self._extent(event.members, (0, 0))
            for name, card in list(self.cards.items()):
                if not card.alive:
                    continue
                if card.catchall:
                    self._gone(name, "the catchall is destroyed when the gnoths are set "
                                     "[M86 p. 98]")
                elif card.range[0] != lo and card.range[0] <= hi and lo <= card.range[1]:
                    self._gone(name, "overlapping a frame of the old hypothesis "
                                     "[M86 p. 98]")

    def _on_operation_glom(self, event) -> None:
        self._gloms[event.name] = (tuple(event.members), event.print_value, tuple(event.type))

    def _on_glom(self, event) -> None:
        self._gloms[event.name] = (tuple(event.members), event.print_value, tuple(event.type))

    def _on_dubbed(self, event) -> None:
        old = self._structures.get(event.name, ())
        self._structures[event.name] = old + ((event.form, event.purity),)

    def _on_class(self, event) -> None:
        """The reigning class changed: a freeze-drying sets it [M86 p. 99], a slip moves
        every gnoth to the new class [M86 pp. 174-175] (`slipping.set_class`)."""
        self.reigning = getattr(event, "reigning", None) or getattr(event, "new", None)
        if event.kind == "class-changed":
            for name, card in self.cards.items():
                if card.alive and not card.catchall:
                    self.cards[name] = dataclasses.replace(card, plato_class=event.new,
                                                           confirmed=False)
                    self._refresh_fields(name)

    def _on_gnoth(self, event) -> None:
        """A Gnoth-maker's gnoth [M86 pp. 77-78]: the class, the frame, the cap and its
        members; the form is the one dubbed member's structure (DECISION D-066). The
        template glom gave way to the gnoth-gloms, so the template's `glom` is `nil`
        [M86 pp. 167-168] (DECISION D-062)."""
        self._caps = []
        self._template_glom_gone()
        self.cards[event.name] = self._build(
            event.name, event.frame, event.plato_class, event.glom, tuple(event.range),
            tuple(event.members), old=self.cards.get(event.name))

    def _template_glom_gone(self) -> None:
        """The Gnoth-maker took the template glom: the `glom` field is `nil` from its
        first cap on [M86 pp. 167-168] (DECISION D-062)."""
        t = self.template
        if t is not None and t.glom is not None and t.state == "accepted":
            self.template = dataclasses.replace(
                t, glom=None, fields_text=_fields_text(t.form, t.state, t.coverage, None))

    def _on_catchall(self, event) -> None:
        """The catchall over its glints [M86 p. 98] (DECISION D-030): no class, frame or
        cap; its range the glints' positions."""
        self._caps = []
        glints = tuple(event.glints)
        known = [self._glints[g] for g in glints if g in self._glints]
        positions = [p for _v, p in known]
        rng = (min(positions), max(positions)) if positions else (0, 0)
        terms = tuple(v for v, _p in known)
        card = GnothCard(event.name, None, None, None, "stable", rng, (), terms,
                         value_text(terms), "nil", None, None, None, None, True, glints,
                         (), made_at=self.events, confirmed=False)
        self.cards[event.name] = dataclasses.replace(card, fields=card_fields(card))

    def _on_gnoth_filled(self, event) -> None:
        """The Gnoth-setter filled a gnoth to a frame of the old hypothesis [M86 p. 98]
        (DECISION D-036): a new cap, the frame, the range, the reigning class; the
        equivalence type and the bond values wait for the snapshot."""
        self._caps = []
        old = self.cards.get(event.name)
        self.cards[event.name] = self._build(
            event.name, event.frame, self.reigning or (old.plato_class if old else None),
            event.glom, tuple(event.range), tuple(event.members), old=old)

    def _on_gnoths_set(self, event) -> None:
        """The Gnoth-setter's set: the gnoths not in it, and the catchall, were destroyed
        [M86 p. 98] (DECISION D-036); the names, frames and ranges are the set's."""
        names = set(event.names)
        for name, card in list(self.cards.items()):
            if card.alive and name not in names:
                reason = ("the catchall is destroyed when the gnoths are set [M86 p. 98]"
                          if card.catchall else "not in the Gnoth-setter's set [M86 p. 98]")
                self._gone(name, reason)
        for name, frame, rng in zip(event.names, event.frames, event.ranges):
            card = self.cards.get(name)
            if card is not None and (card.frame != frame or card.range != tuple(rng)):
                self.cards[name] = dataclasses.replace(card, frame=frame, range=tuple(rng),
                                                       confirmed=False)
                self._refresh_fields(name)

    def _on_gnoth_gone(self, event) -> None:
        self._gone(event.name, event.reason)

    def _on_blasted(self, event) -> None:
        """The blast destroyed every gnoth, "leaving only the glints and their bonds"
        [M86 p. 92], and the template with them (DECISION D-050)."""
        for name in event.gnoths:
            self._gone(name, f"blasted: {event.reason}")
        for name, card in list(self.cards.items()):
            if card.alive:
                self._gone(name, f"blasted: {event.reason}")
        self.template = None

    def _on_gnoth_uncapped(self, event) -> None:
        """A NO-OP took the cap off [M86 p. 113]: no glom, an empty collection, the range
        kept (DECISION D-045)."""
        card = self.cards.get(event.name)
        if card is None:
            return
        self.cards[event.name] = dataclasses.replace(
            card, glom=None, members=(), terms=(), parse_print="()", form_text="nil",
            range=tuple(event.range), confirmed=False)
        self._refresh_fields(event.name)

    def _on_gnoth_capped(self, event) -> None:
        """A Capper recapped the gnoth [M86 p. 113]: the new cap and its range."""
        self._caps = []
        old = self.cards.get(event.name)
        if old is None:
            return
        members = self._gloms.get(event.glom, ((), (), ""))[0]
        self.cards[event.name] = self._build(event.name, old.frame, old.plato_class,
                                             event.glom, tuple(event.range), members, old=old)

    def _on_operation(self, event) -> None:
        """A gnoth operation recapped the gnoths it touched [M86 pp. 110-118]: each got a
        new cap, printed `top-down glom glomN` before the event, one per gnoth in order;
        a gnoth gone is a ghost; the `groups` field lists the operation gloms' members
        (DECISION D-066, D-067)."""
        caps = [c for c in self._caps if c in self._gloms]
        touched = tuple(event.gnoths)
        if len(caps) >= len(touched):
            caps = caps[len(caps) - len(touched):]
        for i, name in enumerate(touched):
            old = self.cards.get(name)
            if old is None:
                continue
            cap = caps[i] if i < len(caps) else None
            if cap is None:
                self.cards[name] = dataclasses.replace(old, confirmed=False)
                continue
            members, print_value, _ = self._gloms[cap]
            lo, hi = self._extent(members, old.range)
            card = self._build(name, old.frame, old.plato_class, cap, (lo, hi), members,
                               old=old)
            groups = tuple(tuple(self._gloms[m][0]) for m in members
                           if m in self._gloms and self._gloms[m][2] in OPERATION_GLOMS)
            self.cards[name] = dataclasses.replace(card, groups=groups)
            self._refresh_fields(name)
        for name in event.gone:
            if name in self.cards and self.cards[name].alive:
                self._gone(name, "emptied by a gnoth operation")
        self._caps = []

    _HANDLERS = {
        "glint": _on_glint, "top-down": _on_top_down, "operation-glom": _on_operation_glom,
        "glom": _on_glom, "dubbed": _on_dubbed, "freeze-dried": _on_class,
        "class-changed": _on_class, "gnoth": _on_gnoth, "catchall": _on_catchall,
        "gnoth-filled": _on_gnoth_filled, "gnoths-set": _on_gnoths_set,
        "gnoth-gone": _on_gnoth_gone, "blasted": _on_blasted,
        "gnoth-uncapped": _on_gnoth_uncapped,
        "gnoth-capped": _on_gnoth_capped, "operation": _on_operation,
    }

    def _build(self, name, frame, plato_class, glom, rng, members, old=None) -> GnothCard:
        """A card from what an event gives: the terms and the parse-print from the cap's
        print-value, the form from the one dubbed member's structure (DECISION D-066),
        the reformulation fields kept from the old card, `confirmed=False`."""
        _members, print_value, _ = self._gloms.get(glom, (members, None, ""))
        if print_value is None:
            print_value = tuple(self._gloms[m][1] if m in self._gloms
                                else self._glints[m][0] if m in self._glints else None
                                for m in members)
        members = tuple(members) or tuple(_members)
        structure = ()
        if len(members) == 1 and members[0] in self._structures:
            structure = self._structures[members[0]]
        terms = tuple(flatten(list(print_value))) if print_value else ()
        card = GnothCard(
            name, frame, plato_class, glom, old.state if old else "stable", tuple(rng),
            members, terms, value_text(tuple(print_value)) if print_value else "()",
            form_field_text(structure),
            old.external_bonds if old else None, old.internal_bonds if old else None,
            old.equivalence_type if old else None, old.groups if old else None, False, (),
            (), made_at=old.made_at if old and old.alive else self.events, confirmed=False)
        return dataclasses.replace(card, fields=card_fields(card))

    def _refresh_fields(self, name) -> None:
        card = self.cards[name]
        self.cards[name] = dataclasses.replace(card, fields=card_fields(card))

    def _extent(self, members, default) -> tuple:
        starts, ends = [], []
        for m in members:
            if m in self._glints:
                starts.append(self._glints[m][1])
                ends.append(self._glints[m][1])
            elif m in self._gloms:
                lo, hi = self._extent(self._gloms[m][0], default)
                starts.append(lo)
                ends.append(hi)
        if not starts:
            return tuple(default)
        return (min(starts), max(ends))

    def _gone(self, name, reason) -> None:
        card = self.cards.get(name)
        if card is not None and card.alive:
            self.cards[name] = dataclasses.replace(card, gone_at=self.events, reason=reason)

    def _prune_ghosts(self) -> None:
        for name in [n for n, c in self.cards.items()
                     if not c.alive and self.events - c.gone_at > FADE]:
            del self.cards[name]

    # -- the template [M86 pp. 73-76] (G-032) -------------------------------------------------

    def _on_template(self, event) -> None:
        old = self.template
        modified_from = old.form if old is not None and old.state == "modified" else None
        self.template = TemplateCard(event.form, event.state, tuple(event.coverage),
                                     event.glom, event.from_glom,
                                     _fields_text(event.form, event.state, event.coverage,
                                                  event.glom),
                                     modified_from=modified_from, immediate=event.immediate,
                                     made_at=self.events)

    def _on_template_check(self, event) -> None:
        if self.template is not None:
            self.template = dataclasses.replace(self.template, check=event.text,
                                                passed=event.passed)

    def _on_template_accepted(self, event) -> None:
        self._set_template_state("accepted")

    def _on_template_abandoned(self, event) -> None:
        if self.template is not None:
            self.template = dataclasses.replace(self.template, state="abandoned",
                                                reason=event.reason, by=event.by)
            self._set_template_state("abandoned")

    def _on_template_filtered(self, event) -> None:
        if self.template is not None:
            self.template = dataclasses.replace(
                self.template, filtered=self.template.filtered + ((event.n, event.term,
                                                                   bool(event.fits)),))

    def _on_template_modified(self, event) -> None:
        if self.template is not None:
            self.template = dataclasses.replace(self.template, state="modified")

    def _on_blast(self, event) -> None:
        if self.template is not None:
            self.template = dataclasses.replace(self.template, blast=tuple(event.burst))

    def _on_dissolved(self, event) -> None:
        """The template glom dissolved: the Gnoth-maker took it (DECISION D-062), or the
        template was released [M86 pp. 73-75]; the `glom` field is `nil` then."""
        t = self.template
        if t is not None and t.glom == event.name:
            self.template = dataclasses.replace(
                t, glom=None, fields_text=_fields_text(t.form, t.state, t.coverage, None))

    def _set_template_state(self, state) -> None:
        t = self.template
        if t is not None:
            self.template = dataclasses.replace(
                t, state=state, fields_text=_fields_text(t.form, state, t.coverage, t.glom))

    _HANDLERS.update({
        "template": _on_template, "template-check": _on_template_check,
        "template-accepted": _on_template_accepted,
        "template-abandoned": _on_template_abandoned,
        "template-filtered": _on_template_filtered,
        "template-modified": _on_template_modified, "blast": _on_blast,
        "dissolved": _on_dissolved,
    })

    # -- the snapshot ----------------------------------------------------------------------------

    def update(self, snapshot) -> None:
        """Take the snapshot in force (None before the first): its gnoths and catchall are
        the cards, every field confirmed; a gnoth it no longer lists becomes a ghost; its
        gloms, glints, dubbings and reigning class are remembered for the events to come;
        its template refreshes the four fields of the card. `corrections` counts the
        snapshots whose names, frames, caps or ranges differed from the cards the events
        built. A snapshot already taken costs nothing (G-030, G-032)."""
        if snapshot is None or snapshot is self._snapshot:
            return
        self._snapshot = snapshot
        self.reigning = snapshot.reformulation.reigning
        self.terms = max(self.terms, len(snapshot.terms))
        for glom in snapshot.gloms:
            self._gloms[glom.name] = (glom.subgloms, glom.print_value, tuple(glom.type))
            if glom.structure:
                self._structures[glom.name] = tuple(glom.structure)
        for glint in snapshot.glints:
            self._glints[glint.name] = (glint.value, glint.position)
        self._caps = []
        records = list(snapshot.gnoths)
        if snapshot.catchall is not None:
            records.append(snapshot.catchall)
        before = {n: (c.frame, c.glom, None if c.catchall else c.range)
                  for n, c in self.cards.items() if c.alive}
        after = {r.name: (r.frame, r.glom, None if r.catchall else tuple(r.range))
                 for r in records}
        if before != after:
            self.corrections += 1
        for name, card in list(self.cards.items()):
            if card.alive and name not in after:
                self._gone(name, "gone at a task boundary")
        for record in records:
            old = self.cards.get(record.name)
            self.cards[record.name] = _card_of(record, old, self.events)
        if snapshot.template is not None:
            self._refresh_template(snapshot.template)

    def _refresh_template(self, record: TemplateRecord) -> None:
        t = self.template
        if t is None or t.form != record.form:
            t = TemplateCard(record.form, record.state, tuple(record.coverage), record.glom,
                             record.from_glom, record.fields_text, made_at=self.events)
        self.template = dataclasses.replace(
            t, state=record.state if t.state != "modified" else t.state,
            coverage=tuple(record.coverage), glom=record.glom, fields_text=record.fields_text)


def _fields_text(form, state, coverage, glom) -> str:
    """The template's four fields as printed, `(form (Cycle 3 2 (3 7)) state working
    coverage (1 2) glom glom2)` [M86 p. 167] (`Template.fields_text`)."""
    return f"(form {form} state {state} coverage ({coverage[0]} {coverage[1]}) glom {glom or 'nil'})"


def _card_of(record: GnothRecord, old: GnothCard | None, index: int) -> GnothCard:
    """A card from a snapshot's record: every field confirmed; `made_at` kept (G-030)."""
    form_text = dict(record.fields).get("form", "nil")
    made_at = old.made_at if old is not None and old.alive else index
    return GnothCard(record.name, record.frame, record.plato_class, record.glom, record.state,
                     tuple(record.range), tuple(record.members), tuple(record.terms),
                     record.parse_print, form_text, record.external_bonds,
                     record.internal_bonds, record.equivalence_type, record.groups,
                     record.catchall, tuple(record.glints), tuple(record.fields),
                     made_at=made_at, confirmed=True)


# ---- the layout [M86 p. 102] (G-031) ------------------------------------------------------------

def gnoth_boxes(model: SocratoplasmModel) -> tuple:
    """The gnoth rectangles, as Figure 1's top row [M86 p. 102], over the cytoplasm's
    columns: a box runs from `BOX_PAD_X` left of its first glint to `BOX_PAD_X` right of
    its last, the glints at `position * SPACING` and `GLINT_WIDTH` wide (G-022); the
    gnoths of the parse on row 0, the catchall after them, and a box that would overlap
    one already placed goes to the next row; ghosts fade (G-031). An uncapped gnoth keeps
    its last range, dashed."""
    cards = list(model.gnoths())
    if model.catchall is not None:
        cards.append(model.catchall)
    cards.extend(model.ghosts())
    boxes, placed = [], []
    for card in cards:
        lo, hi = card.range
        if lo <= 0 or hi < lo:
            continue
        left = lo * SPACING - GLINT_WIDTH / 2 - BOX_PAD_X
        right = hi * SPACING + GLINT_WIDTH / 2 + BOX_PAD_X
        row = 0
        while any(r == row and left < pr and pl < right for pl, pr, r in placed):
            row += 1
        placed.append((left, right, row))
        ghost = 0.0
        if not card.alive:
            ghost = max(0.0, 1.0 - (model.events - card.gone_at) / FADE)
        boxes.append(GnothBox(card.name, CATCHALL_KIND if card.catchall else GNOTH_KIND,
                              left, right, row, card.name,
                              " ".join(str(t) for t in card.terms), ghost, card.uncapped,
                              model.is_selected(card.name), card.confirmed))
    return tuple(boxes)


def strip_extent(model: SocratoplasmModel) -> float:
    """The width, in the cytoplasm's units, the strip lays its boxes over: from the left
    edge of a box at position 1 to the right edge of one at the last term, or of the
    rightmost box (G-031)."""
    right = max([b.right for b in gnoth_boxes(model)]
                + [model.terms * SPACING + GLINT_WIDTH / 2 + BOX_PAD_X])
    return right - (SPACING - GLINT_WIDTH / 2 - BOX_PAD_X)


# ---- the cards as lines (G-031) -------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class CardLine:
    """One line the card list paints (G-031): `kind` (`head`, `field`, `bond`, `note`,
    `template`), `text`, the gnoth's `name` (None for the template's lines), `bond` (the
    bond a `bond` line names, for the hover), `tooltip`, `selected`, `grey` (a ghost) and
    `unconfirmed`."""
    kind: str
    text: str
    name: str | None = None
    bond: str | None = None
    tooltip: str = ""
    selected: bool = False
    grey: bool = False
    unconfirmed: bool = False


def head_text(card: GnothCard) -> str:
    """A card's heading: `gnoth1  Cycle  frame 0`, `gnoth2  catchall` [M86 pp. 78, 98]
    (G-031)."""
    if card.catchall:
        return f"{card.name}  catchall"
    frame = "" if card.frame is None else f"  frame {card.frame}"
    return f"{card.name}  {card.plato_class or 'nil'}{frame}"


def pane_lines(model: SocratoplasmModel) -> tuple:
    """The lines of the socratoplasm pane: the template's block [M86 pp. 73-76, 167],
    then one block per gnoth of the parse, the catchall's, and the ghosts' in grey: a
    heading, the fields as `show` prints them [M86 pp. 168, 175], and under a bond field
    one `bond` line per bond, `bond2  0`, for the hover (PANES.md P3; G-031, G-032)."""
    lines = [CardLine("template", "template", tooltip="the template [M86 pp. 73-76]")]
    for text in template_lines(model.template):
        lines.append(CardLine("field", text, tooltip="[M86 pp. 74-75, 167]"))
    cards = list(model.gnoths())
    if model.catchall is not None:
        cards.append(model.catchall)
    cards.extend(model.ghosts())
    for card in cards:
        selected = model.is_selected(card.name)
        grey = not card.alive
        tip = card_tooltip(card)
        lines.append(CardLine("head", head_text(card), card.name, tooltip=tip,
                              selected=selected, grey=grey, unconfirmed=not card.confirmed))
        for field, text in card.fields:
            lines.append(CardLine("field", f"{field}: {text}", card.name, tooltip=tip,
                                  selected=selected, grey=grey,
                                  unconfirmed=not card.confirmed))
            bonds = (card.external_bonds if field == "external-bonds"
                     else card.internal_bonds if field == "internal-bonds" else None)
            for bond, value in bonds or ():
                lines.append(CardLine("bond", f"{bond}  {_num(value)}", card.name, bond=bond,
                                      tooltip=f"{bond}: value {_num(value)} in "
                                              f"{model.reigning or 'the reigning class'} "
                                              f"[M86 p. 102]; hover to show the arc",
                                      selected=selected, grey=grey))
        if card.catchall:
            lines.append(CardLine("note", "a repository for non-troublesome terms "
                                          "[M86 p. 98]", card.name, tooltip=tip, grey=grey))
        if not card.alive:
            lines.append(CardLine("note", f"gone: {card.reason}", card.name, tooltip=tip,
                                  grey=True))
    return tuple(lines)
