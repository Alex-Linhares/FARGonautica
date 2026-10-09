"""Code repeated verbatim in SCategory/Ascending.pm, Descending.pm and Sameness.pm.

The Perl modules each carry their own copy; the port keeps one.
"""
from seqsee import util


def guesser(subobject):
    """Perl: _guesser (Ascending/Descending): SInt of the effective object's magnitude
    if that is exactly a Seqsee::Element (``ref eq``, not isa), else None."""
    if subobject is None:
        return None
    effective_object = subobject.get_effective_object()
    if util.perl_ref(effective_object) != "Seqsee::Element":
        return None
    from seqsee.sint import SInt
    return SInt(effective_object.get_mag())


def instancer_slippages(obj, result_of_can_be_seen_as):
    """The slippages of Instancer: the blemished parts (``|| {}``), replaced by
    ``{0: entire_blemish}`` when an element is seen with an entire blemish."""
    from seqsee.objects.element import Element
    slippages = result_of_can_be_seen_as.get_parts_blemished()
    if not util.perl_true(slippages):
        slippages = {}
    if isinstance(obj, Element):
        entire_blemish = result_of_can_be_seen_as.get_entire_blemish()
        if util.perl_true(entire_blemish):
            slippages = {0: entire_blemish}
    return slippages


def num(x):
    """An operand of Perl arithmetic: SInts keep their overloads, scalars numify."""
    from seqsee.sint import SInt
    return x if isinstance(x, SInt) else util.perl_num(x)


def mag_of(x):
    """``ref($x) ? $x->get_mag() : $x``."""
    return x.get_mag() if util.perl_ref(x) else x


def sufficient_start_end_length(atts):
    """Perl: AreAttributesSufficientToBuild of Ascending/Descending: 1 if at least two
    of start/end/length are present, else None."""
    count = sum(1 for name in ("start", "end", "length") if name in atts)
    return None if count < 2 else 1
