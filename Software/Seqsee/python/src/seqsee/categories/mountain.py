"""Port of SCategory/Mountain.pm: foot, foot+1, ..., peak, ..., foot+1, foot.

The ``~~`` overload (literal_comparison_hack_for_smart_match) is ``eq`` on refs,
i.e. identity, Python's default ``==``. Seqsee::Object->create goes through
``base._object_create`` (item 021).
"""
from seqsee import util
from seqsee.categories import base, sequence_common
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.categories.sequence_common import mag_of
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.ltmstorable import Independent
from seqsee.sbindings import SBindings


def _guesser(subobject):
    """Perl: _guesser. Like sequence_common.guesser, but with no undef guard: a missing
    subobject dies (AttributeError here; Perl: method call on undef)."""
    effective_object = subobject.get_effective_object()
    if util.perl_ref(effective_object) != "Seqsee::Element":
        return None
    from seqsee.sint import SInt
    return SInt(effective_object.get_mag())


class Mountain(Independent, NotMetonyable, SCategory):
    """Perl: SCategory::Mountain."""

    perl_name = "SCategory::Mountain"

    def string_to_recreate(self):
        return "SCategory::Mountain->new()"

    def get_name(self):
        return "mountain"

    def as_text(self):
        return "mountain"

    def instancer(self, obj):
        """Perl: Instancer($object): guess foot/peak from the first and middle items of
        an object with an odd number of parts."""
        object_size = obj.get_parts_count()
        if not int(util.perl_num(object_size)) % 2:
            return None
        items = obj.get_items()

        def item(i):
            return items[i] if i < len(items) else None

        foot = _guesser(item(0))
        if not util.perl_true(foot):
            return None
        peak = _guesser(item((object_size - 1) // 2))
        if not util.perl_true(peak):
            return None
        guess = {"foot": foot, "peak": peak}
        # PERL-QUIRK (oracle-confirmed): when peak < foot, build returns undef, but
        # CanBeSeenAs(undef) is still called and its result decides.
        guess_built = self.build(guess)
        result_of_can_be_seen_as = obj.can_be_seen_as(guess_built)
        if not util.perl_true(result_of_can_be_seen_as):
            return None
        slippages = sequence_common.instancer_slippages(obj, result_of_can_be_seen_as)
        return SBindings.create(slippages, guess, obj)

    def build(self, args):
        """Perl: build(\\%args). None if peak < foot; the bindings share ``args``."""
        if not self.are_attributes_sufficient_to_build(*args):
            raise Confess("Too few params")
        foot_mag = mag_of(args.get("foot"))
        peak_mag = mag_of(args.get("peak"))
        if util.perl_num(peak_mag) < util.perl_num(foot_mag):
            return None
        if util.perl_num(foot_mag) == util.perl_num(peak_mag):
            ret = base._object_create(foot_mag)
        else:
            ret = base._object_create(
                *util.perl_range(foot_mag, peak_mag),
                *reversed(util.perl_range(foot_mag, util.perl_num(peak_mag) - 1)))
        ret.add_category(self, SBindings.create({}, args, ret))
        ret.set_reln_scheme(RELN_SCHEME.CHAIN)
        return ret

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild: 1 if both foot and peak are present."""
        if "foot" not in atts or "peak" not in atts:
            return None
        return 1
