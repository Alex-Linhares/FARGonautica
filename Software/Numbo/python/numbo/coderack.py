"""The Coderack.

Port of lisp/src/coderack.lisp, which is RECONSTRUCTED (no 1987 file defines the
cr- functions; see that file's header and PORTING_NOTES.md).  A coderack is
named by a symbol: (get name 'coderack) holds it, here
world.get_prop(name, CODERACK).  It is a list of urgency bins, one per level,
each a Python list [urgency, form, ...] standing for the Lisp cons
(urgency . forms), newest form first.  cr_choose picks a bin with probability
(level * codelets in bin) / total, then a codelet uniformly inside it, with
the shared RNG (world.rng, lisp/src/oracle.lisp), and removes it.
"""

from numbo.franz import Symbol, _eql, intern

CODERACK = intern("CODERACK")


class CoderackError(Exception):
    """A Lisp (error ...) signalled by a cr- function."""


class Coderack:
    """coderack.lisp: (defstruct coderack name bins)."""

    def __init__(self, name, bins):
        self.name = name
        self.bins = bins


def cr_get(world, name):
    """coderack.lisp: cr-get.  The coderack named NAME (or NAME itself if it
    is a coderack)."""
    if isinstance(name, Coderack):
        return name
    rack = world.get_prop(name, CODERACK) if isinstance(name, Symbol) else None
    if rack is None:
        raise CoderackError(f"No coderack named {name!r}")
    return rack


def _realp(u):
    return isinstance(u, (int, float)) and not isinstance(u, bool)


def cr_make_coderack(world, name, urgencies):
    """coderack.lisp: cr-make-coderack.  An empty coderack with one bin per
    urgency level (duplicates removed, first kept), named NAME.  Returns NAME."""
    for u in urgencies:
        if not (_realp(u) and u >= 0):
            raise CoderackError(f"cr-make-coderack: bad urgency level {u!r}")
    levels = []
    for u in urgencies:
        if not any(_eql(u, v) for v in levels):
            levels.append(u)
    world.put_prop(name, Coderack(name, [[u] for u in levels]), CODERACK)
    return name


def cr_hang(world, name, form, urgency):
    """coderack.lisp: cr-hang.  Posts FORM with URGENCY, which must be eql to
    one of the rack's levels (4.0 is not the level 4).  Returns FORM."""
    rack = cr_get(world, name)
    bin_ = next((b for b in rack.bins if _eql(b[0], urgency)), None)
    if bin_ is None:
        raise CoderackError(f"cr-hang: urgency {urgency!r} is not a level of coderack "
                            f"{name!r} {[b[0] for b in rack.bins]!r}")
    bin_.insert(1, form)
    return form


def cr_count(world, name):
    """coderack.lisp: cr-count.  The number of codelets on the rack."""
    return sum(len(b) - 1 for b in cr_get(world, name).bins)


def cr_empty_p(world, name):
    """coderack.lisp: cr-empty?."""
    return cr_count(world, name) == 0


def cr_empty_coderack(world, name):
    """coderack.lisp: cr-empty-coderack.  Removes every codelet.  Returns NAME."""
    for b in cr_get(world, name).bins:
        del b[1:]
    return name


def cr_choose(world, name, full=False):
    """coderack.lisp: cr-choose.  Removes a codelet chosen at random, weighted
    by urgency, and returns its form, or [form, urgency] if FULL; None if the
    rack is empty.  Urgency-0 codelets come out only when nothing else is
    left, and then the first such bin is taken without a draw."""
    bins = [b for b in cr_get(world, name).bins if len(b) > 1]
    if not bins:
        return None
    total = 0
    for b in bins:
        total = total + b[0] * (len(b) - 1)
    if total == 0:
        bin_ = bins[0]
    else:
        # Integer levels only: a float total makes the oracle's RANDOM (and
        # Rng.random) signal an error.
        r = world.rng.random(total)
        bin_ = None
        for b in bins:
            r = r - b[0] * (len(b) - 1)
            if r < 0:
                bin_ = b
                break
    i = world.rng.random(len(bin_) - 1)
    form = bin_.pop(1 + i)
    return [form, bin_[0]] if full else form
