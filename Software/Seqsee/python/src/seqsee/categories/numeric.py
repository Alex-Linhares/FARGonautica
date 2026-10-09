"""Port of SCategory/Numeric.pm (Moose role ``SCategory::Numeric``).

Numeric categories describe plain magnitudes: Instancer reads ``get_mag`` and
calls NumericInstancer, and build makes a Seqsee::Element.

Two outside calls go through hooks here, so tests can monkeypatch them and the
later items replace only their bodies:
``_element_create`` (Seqsee::Element->create → objects/element.py) and
``_mapping_numeric_create`` (Mapping::Numeric->create → mapping/numeric.py; used by
the FindMappingForCat of every numeric category).
"""
from abc import abstractmethod

from seqsee import util
from seqsee.errors import Confess
from seqsee.sbindings import SBindings


def _element_create(mag, pos):
    """Perl: Seqsee::Element->create($mag, $pos)."""
    from seqsee.objects.element import Element
    return Element.create(mag, pos)


def _mapping_numeric_create(name, category):
    """Perl: Mapping::Numeric->create($name, $category)."""
    from seqsee.mapping.numeric import MappingNumeric
    return MappingNumeric.create(name, category)


class Numeric:
    """Perl role SCategory::Numeric. Mix in before SCategory."""

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild.

        PERL-QUIRK (oracle-confirmed): ``my ($self, @atts) = 1;`` assigns the list (1),
        so @atts is always empty and this always returns 0."""
        atts = ()
        return 1 if "mag" in atts else 0

    def build(self, args):
        """Perl: build(\\%args): a Seqsee::Element with magnitude args["mag"], described
        as this category with bindings ``args`` itself (shared, not copied)."""
        if "mag" not in args:
            raise Confess("Need mag")
        ret = _element_create(args["mag"], -1)
        ret.add_category(self, SBindings.create({}, args, ret))
        return ret

    def instancer(self, obj):
        """Perl: Instancer($object) → NumericInstancer($object->get_mag). A group has no
        get_mag, so Perl dies (oracle-confirmed in sworkspace_more)."""
        if not hasattr(obj, "get_mag"):
            raise Confess(f'Can\'t locate object method "get_mag" via package '
                          f'"{util.perl_ref(obj)}"')
        return self.numeric_instancer(obj.get_mag())

    @abstractmethod
    def numeric_instancer(self, mag):
        """Perl: NumericInstancer($mag): SBindings if mag is an instance, else None."""
