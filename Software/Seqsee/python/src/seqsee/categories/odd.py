"""Port of SCategory/Odd.pm: odd magnitudes; succ/pred step by 2."""
from seqsee.categories import numeric
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.ltmstorable import Independent
from seqsee.sbindings import SBindings
from seqsee.util import perl_num


class Odd(Independent, NotMetonyable, numeric.Numeric, SCategory):
    """Perl: SCategory::Odd."""

    perl_name = "SCategory::Odd"

    def numeric_instancer(self, mag):
        """Perl: NumericInstancer (``$mag % 2``: Perl truncates to integers first)."""
        if not int(perl_num(mag)) % 2:
            return None
        return SBindings.create({}, {})

    def find_mapping_for_cat(self, a, b):
        """Perl: FindMappingForCat($a, $b) on magnitudes: same/succ/pred, else None."""
        a, b = perl_num(a), perl_num(b)
        if a == b:
            name = "same"
        elif a + 2 == b:
            name = "succ"
        elif a - 2 == b:
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
            return perl_num(obj) + 2
        if name == "pred":
            return perl_num(obj) - 2
        return None

    def string_to_recreate(self):
        # PERL-QUIRK (oracle-confirmed): names Even, so Odd round-trips through
        # serialize/deserialize as an SCategory::Even.
        return "SCategory::Even->new()"

    def get_name(self):
        return "odd"

    def as_text(self):
        return "odd"
