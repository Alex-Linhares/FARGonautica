"""The coderack model (not 1987 source; loop0003).

A Qt-free shadow of the coderack (coderack.py), kept from a run's events:
coderack-created gives the urgency bins, a post puts its codelet first in
its bin (cr-hang's order: newest first), rack-emptied empties every bin, and
a choice (setup-choose or codelet-chosen) takes its codelet out.  cr-choose
picks the codelet inside its bin with its last draw, (random (length bin)),
and the event carries that draw, (n, index): so the model removes exactly
the codelet the engine removed, and checks it is the one the event names.
The rack field of setup-choose and iteration events (the counts by
urgency) is checked too.  A mismatch is a CoderackModelError: the model was
fed a stream that is not a run's.

A codelet is (codelet name, arguments, urgency), the arguments in the
oracle trace's encoding (events.encode_data), as the events have them.
"""

import dataclasses

from numbo import coderack, events, observe
from numbo.franz import intern as _S

_CODERACK = _S("*CODERACK*")


class CoderackModelError(Exception):
    """An event that does not fit the model's coderack."""


@dataclasses.dataclass(frozen=True)
class Codelet:
    """A codelet waiting on the rack: CODELET (its name), ARGS, URGENCY."""
    codelet: str
    args: tuple
    urgency: object


@dataclasses.dataclass(frozen=True)
class Choice:
    """The codelet just chosen (None: the rack was empty), by iteration N's
    cr-choose or (SETUP) one of config's set-up chooses (N None)."""
    n: object
    codelet: object
    setup: bool


def _eql(a, b):
    return type(a) is type(b) and a == b


def _arg_text(x):
    if x is None:
        return "nil"
    if x is True:
        return "t"
    if isinstance(x, float):
        return f"{x:.1f}"
    if isinstance(x, dict):
        return f'"{x["str"]}"' if "str" in x else str(x.get("name"))
    if isinstance(x, tuple):
        if len(x) == 2 and x[0] == "QUOTE":
            return "'" + _arg_text(x[1])
        return "(" + " ".join(map(_arg_text, x)) + ")"
    return str(x)


def form_text(name, args):
    """A codelet NAME with its event ARGS as a short Lisp-like form."""
    return " ".join([name.lower(), *map(_arg_text, args or ())])


def codelet_text(codelet):
    """CODELET as a short Lisp-like form: "const-blx CYTO-BRICK4 CYTO-BRICK1
    84" (a cyto-node by its name, floats to one decimal)."""
    return form_text(codelet.codelet, codelet.args)


def bin_summary(codelets):
    """((codelet name, count), ...) of CODELETS, in order of each name's
    first (newest) codelet."""
    counts = {}
    for c in codelets:
        counts[c.codelet] = counts.get(c.codelet, 0) + 1
    return tuple(counts.items())


def codelet_of(form, urgency):
    """A coderack form (codelet-name arg ...) as a Codelet."""
    return Codelet(form[0].name, tuple(events.encode_data(a) for a in form[1:]), urgency)


class CoderackModel:
    """The coderack's bins, kept from events (an observer), or read from a
    World (from_world).  Two models are equal when they have the same name
    and bins; `chosen`, `last_posted`, `changed` and `max_total` are about
    the events, which from_world can't know."""

    def __init__(self):
        self.reset()

    def reset(self):
        self.name = None
        self._bins = []            # [urgency, [Codelet, ...]], newest first
        self.chosen = None         # Choice
        self.last_posted = None    # Codelet
        self.changed = ()          # the urgencies of the bins the last event changed
        self.max_total = 0

    # -- reading ---------------------------------------------------------

    def bins(self):
        """((urgency, (Codelet, ...)), ...) in bin order."""
        return tuple((u, tuple(cs)) for u, cs in self._bins)

    def bin(self, urgency):
        """The codelets waiting at URGENCY, newest first."""
        return tuple(self._bin(urgency))

    def levels(self):
        return tuple(u for u, _ in self._bins)

    def counts(self):
        """((urgency, count), ...) in bin order: the events' rack field."""
        return tuple((u, len(cs)) for u, cs in self._bins)

    @property
    def total(self):
        return sum(len(cs) for _, cs in self._bins)

    def state(self):
        return (self.name, self.bins())

    def __eq__(self, other):
        if not isinstance(other, CoderackModel):
            return NotImplemented
        return self.state() == other.state()

    def __repr__(self):
        return f"<CoderackModel {self.name} {self.counts()}>"

    def _bin(self, urgency):
        for u, cs in self._bins:
            if _eql(u, urgency):
                return cs
        raise CoderackModelError(f"no bin of urgency {urgency!r} in {self.levels()!r}")

    # -- from the World --------------------------------------------------

    @classmethod
    def from_world(cls, world):
        """The model of WORLD's coderack (*coderack*'s; empty if unbound)."""
        model = cls()
        name = world.values.get(_CODERACK)
        if name is None:
            return model
        rack = coderack.cr_get(world, name)
        model.name = events.plain(rack.name)
        model._bins = [[b[0], [codelet_of(form, b[0]) for form in b[1:]]] for b in rack.bins]
        return model

    # -- events ----------------------------------------------------------

    def on_event(self, event):
        self.changed = ()
        handler = _HANDLERS.get(type(event))
        if handler is not None:
            handler(self, event)

    def _start(self, e):
        self.reset()

    def _created(self, e):
        self.name = e.name
        self._bins = [[u, []] for u in e.levels]
        self.chosen = self.last_posted = None
        self.changed = self.levels()

    def _post(self, e):
        codelet = Codelet(e.codelet, e.args, e.urgency)
        self._bin(e.urgency).insert(0, codelet)
        self.last_posted = codelet
        self.changed = (e.urgency,)
        self.max_total = max(self.max_total, self.total)

    def _emptied(self, e):
        self.changed = tuple(u for u, cs in self._bins if cs)
        for _, cs in self._bins:
            cs.clear()

    def _check_rack(self, e):
        if self.name is not None and tuple(map(tuple, e.rack)) != self.counts():
            raise CoderackModelError(f"{e.kind}: rack {e.rack!r}, the model has "
                                     f"{self.counts()!r}")

    def _iteration(self, e):
        self._check_rack(e)

    def _setup_choose(self, e):
        self._check_rack(e)
        self._choose(e, None, True)

    def _codelet_chosen(self, e):
        self._choose(e, e.n, False)

    def _choose(self, e, n, setup):
        if e.codelet is None:
            self.chosen = Choice(n=n, codelet=None, setup=setup)
            return
        codelet = Codelet(e.codelet, e.args, e.urgency)
        cs = self._bin(e.urgency)
        if not e.draws:
            raise CoderackModelError(f"{e.kind} of {codelet} made no draw")
        bound, index = e.draws[-1]
        if bound != len(cs) or not 0 <= index < len(cs):
            raise CoderackModelError(f"{e.kind} of {codelet}: draw {e.draws[-1]!r}, but "
                                     f"bin {e.urgency!r} has {len(cs)} codelets")
        if cs[index] != codelet:
            raise CoderackModelError(f"{e.kind} of {codelet}: bin {e.urgency!r} has "
                                     f"{cs[index]} at {index}")
        del cs[index]
        self.chosen = Choice(n=n, codelet=codelet, setup=setup)
        self.changed = (e.urgency,)


_HANDLERS = {
    observe.RunStarted: CoderackModel._start,
    observe.CoderackCreated: CoderackModel._created,
    observe.CodeletPosted: CoderackModel._post,
    observe.RackEmptied: CoderackModel._emptied,
    observe.IterationBegan: CoderackModel._iteration,
    observe.SetupChoose: CoderackModel._setup_choose,
    observe.CodeletChosen: CoderackModel._codelet_chosen,
}
