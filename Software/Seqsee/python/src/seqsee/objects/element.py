"""Port of Seqsee/Element.pm (``Seqsee::Element``, extends Seqsee::Anchored): one term of
the sequence, with a magnitude.

Moose attribute ``mag`` (Int, required; reader ``get_mag``, and Moose's ``rw`` accessor
``mag``). BUILD describes the element as $S::NUMBER, and also as $S::PRIME and
$S::ODD/$S::EVEN when the Primes/Parity features are on. ``create($mag, $pos)`` makes
the element its own only item, with strength 20.

Also here: the ``Seqsee::Element::…`` subs that Seqsee/Object.pm defines
(HasAsPartDeep, GetEffectiveStructure, ContainsAMetonym).

Perl names: ``UpdateStrength`` → ``update_strength`` (does nothing),
``CheckSquintability`` → ``check_squintability``.
"""
from seqsee import util
from seqsee.errors import SErr
from seqsee.objects.anchored import Anchored
from seqsee.objects.object import SeqseeObject, _check
from seqsee.spos import SPos

_POS_FIRST = SPos(1)
_POS_LAST = SPos(-1)


class Element(Anchored):
    """Perl: Seqsee::Element."""

    perl_name = "Seqsee::Element"

    _ATTRS = Anchored._ATTRS + (("mag", "mag", True),)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._build()

    def _init_attributes(self, kwargs):
        super()._init_attributes(kwargs)
        self._mag = kwargs["mag"]

    def _build(self):
        """Perl: BUILD."""
        from seqsee import global_ as Global
        from seqsee import s as S
        from seqsee.categories.prime import is_prime
        self.describe_as(S.NUMBER)
        if Global.Feature.get("Primes") and is_prime(self.get_mag()):
            self.describe_as(S.PRIME)
        if Global.Feature.get("Parity"):
            if util.perl_num(self.get_mag()) % 2:
                self.describe_as(S.ODD)
            else:
                self.describe_as(S.EVEN)

    def get_mag(self):
        return self._mag

    def mag(self, *value):
        """Perl: the ``mag`` accessor (Moose ``is => 'rw'``): get, or set with the Int check."""
        if value:
            self._mag = _check("mag", value[0])
        return self._mag

    @classmethod
    def create(cls, mag, pos):
        """Perl: create($mag, $pos)."""
        selement = cls(left_edge=pos, right_edge=pos, mag=mag, items=[], group_p=0)
        selement.get_parts_ref()[:1] = [selement]  # [sic]
        selement.set_strength(20)
        return selement

    def get_structure(self):
        return self.get_mag()

    def as_text(self):
        """Perl: as_text: "<ref>:[l,r] mag"."""
        l, r = self.get_edges()
        return (f"{util.perl_ref(self)}:[{util.perl_str(l)},{util.perl_str(r)}] "
                f"{util.perl_str(self.get_mag())}")

    def get_at_position(self, position):
        """Perl: get_at_position: self for the first or last position; else SErr."""
        if position == _POS_FIRST or position == _POS_LAST:
            return self
        SErr.throw("out of range for Seqsee::Element")

    def get_flattened(self):
        return [self.get_mag()]

    def update_strength(self):
        """Perl: UpdateStrength: does nothing."""
        return None

    def check_squintability(self, intended):
        """Perl: CheckSquintability: describe_as $S::NUMBER, then Seqsee::Object's."""
        from seqsee import s as S
        self.describe_as(S.NUMBER)
        return SeqseeObject.check_squintability(self, intended)

    # --- subs defined in Seqsee/Object.pm ------------------------------------------------

    def has_as_part_deep(self, item):
        """Perl: Seqsee::Element::HasAsPartDeep (defined in Object.pm): ``$self eq $item``."""
        return self is item

    def get_effective_structure(self):
        """Perl: Seqsee::Element::GetEffectiveStructure (defined in Object.pm): the magnitude."""
        return self.get_mag()

    def contains_a_metonym(self):
        """Perl: Seqsee::Element::ContainsAMetonym (defined in Object.pm): always 0."""
        return 0
