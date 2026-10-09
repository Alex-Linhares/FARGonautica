"""Port of SCategory/Number.pm: every magnitude is a number; succ/pred step by 1.

The ``~~`` overload (literal_comparison_hack_for_smart_match) is ``eq`` on refs,
i.e. identity, Python's default ``==``.
"""
from seqsee.categories import numeric
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.ltmstorable import Independent
from seqsee.sbindings import SBindings
from seqsee.multimethods import perl_isa
from seqsee.util import perl_num


class Number(Independent, NotMetonyable, numeric.Numeric, SCategory):
    """Perl: SCategory::Number."""

    perl_name = "SCategory::Number"

    def numeric_instancer(self, mag):
        """Perl: NumericInstancer."""
        return SBindings.create({}, {})

    def find_mapping_for_cat(self, a, b):
        """Perl: FindMappingForCat($a, $b) on magnitudes: same/succ/pred, else None.

        PERL-QUIRK (oracle-confirmed in item 024): objects numify to their addresses, so
        two objects (e.g. Elements, from recalculate_relations) give "same" only when they
        are the same object and None otherwise."""
        if perl_isa(a, "Seqsee::Object") or perl_isa(b, "Seqsee::Object"):
            if a is not b:
                return None
            return numeric._mapping_numeric_create("same", self)
        a, b = perl_num(a), perl_num(b)
        if a == b:
            name = "same"
        elif a + 1 == b:
            name = "succ"
        elif a - 1 == b:
            name = "pred"
        else:
            return None
        return numeric._mapping_numeric_create(name, self)

    def apply_mapping_for_cat(self, transform, obj):
        """Perl: ApplyMappingForCat($transform, $mag)."""
        name = transform.get_name()
        if name == "same":
            return obj
        if name == "succ":
            return perl_num(obj) + 1
        if name == "pred":
            return perl_num(obj) - 1
        return None

    def string_to_recreate(self):
        return "SCategory::Number->new()"

    def get_name(self):
        return "number"

    def as_text(self):
        return "number"
