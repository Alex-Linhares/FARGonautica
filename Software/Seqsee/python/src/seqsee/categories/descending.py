"""Port of SCategory/Descending.pm: the run start, start-1, ..., end.

The ``~~`` overload (literal_comparison_hack_for_smart_match) is ``eq`` on refs,
i.e. identity, Python's default ``==``. Seqsee::Object->create goes through
``base._object_create`` (item 021).
"""
from seqsee import util
from seqsee.categories import base, sequence_common
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.categories.sequence_common import mag_of, num
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.ltmstorable import Independent
from seqsee.sbindings import SBindings


class Descending(Independent, NotMetonyable, SCategory):
    """Perl: SCategory::Descending."""

    perl_name = "SCategory::Descending"

    def string_to_recreate(self):
        return "SCategory::Descending->new()"

    def get_name(self):
        return "descending"

    def as_text(self):
        return "descending"

    def instancer(self, obj):
        """Perl: Instancer($object): guess start/end from the first and last items."""
        items = obj.get_items()
        start = sequence_common.guesser(items[0] if items else None)
        if not util.perl_true(start):
            return None
        end = sequence_common.guesser(items[-1] if items else None)
        if not util.perl_true(end):
            return None
        guess = {"start": start, "end": end}
        guess_built = self.build(guess)
        result_of_can_be_seen_as = obj.can_be_seen_as(guess_built)
        if not util.perl_true(result_of_can_be_seen_as):
            return None
        slippages = sequence_common.instancer_slippages(obj, result_of_can_be_seen_as)
        return SBindings.create(slippages, guess, obj)

    def build(self, args):
        """Perl: build(\\%args). Fills in the missing one of start/end/length in ``args``
        (which the bindings share). When start < end the object is empty and the
        length negative (oracle-confirmed)."""
        if not self.are_attributes_sufficient_to_build(*args):
            raise Confess("Too few params")
        if "start" in args:
            start = args["start"]
        else:
            start = num(args.get("end")) + num(args.get("length")) - 1
        if "end" in args:
            end = args["end"]
        else:
            end = num(args.get("start")) - num(args.get("length")) + 1

        # PERL-QUIRK (oracle-confirmed): `||=`, so a false length (0) is recomputed.
        if not util.perl_true(args.get("start")):
            args["start"] = start
        if not util.perl_true(args.get("end")):
            args["end"] = end
        if not util.perl_true(args.get("length")):
            args["length"] = num(start) - num(end) + 1

        items = reversed(util.perl_range(mag_of(end), mag_of(start)))
        ret = base._object_create(*items)
        ret.add_category(self, SBindings.create({}, args, ret))
        ret.set_reln_scheme(RELN_SCHEME.CHAIN)
        return ret

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild: two of start/end/length."""
        return sequence_common.sufficient_start_end_length(atts)
