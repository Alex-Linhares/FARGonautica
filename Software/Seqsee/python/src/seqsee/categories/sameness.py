"""Port of SCategory/Sameness.pm: a group of ``length`` copies of ``each``.

The ``~~`` overload (literal_comparison_hack_for_smart_match) is ``eq`` on refs,
i.e. identity, Python's default ``==``.

Seqsee::Object->create goes through ``base._object_create`` (item 021), and the
'each' metonym finder reaches Seqsee::Anchored->create through ``_anchored_create``
(→ objects/anchored.py) and ``smetonym.SMetonym`` (item 015).
"""
from seqsee import smetonym, util
from seqsee.categories import base, sequence_common
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import Metonyable
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.ltmstorable import Independent
from seqsee.sbindings import SBindings


def _anchored_create(obj):
    """Perl: Seqsee::Anchored->create($object)."""
    from seqsee.objects.anchored import Anchored
    return Anchored.create(obj)


def _each_finder(obj, cat, name, bindings):
    """The 'each' metonym finder: the group seen as its repeated item."""
    starred = base._object_create(bindings.get_binding_for_attribute("each"))
    info_lost = {"length": bindings.get_binding_for_attribute("length")}
    return smetonym.SMetonym(
        category=cat,
        name=name,
        starred=_anchored_create(starred),
        unstarred=obj,
        info_loss=info_lost,
    )


def _each_unfinder(cat, name, info_loss, obj):
    """The 'each' metonymy unfinder: rebuild the group from its item and info_loss
    (whose keys win over ``each``, as in Perl's ``{ each => $object, %$info_loss }``)."""
    if "length" not in info_loss:
        # Perl concatenates %$info_loss in scalar context: the key count.
        raise Confess(f"Length missing in info_loss: {len(info_loss)}")
    return cat.build({"each": obj, **info_loss})


def _guesser(subobject):
    """Perl: _guesser (unused by Sameness itself). Dies on undef, unlike Ascending's."""
    effective_object = subobject.get_effective_object()
    if util.perl_ref(effective_object) != "Seqsee::Element":
        return None
    from seqsee.sint import SInt
    return SInt(effective_object.get_mag())


class Sameness(Independent, Metonyable, SCategory):
    """Perl: SCategory::Sameness."""

    perl_name = "SCategory::Sameness"

    def _default_metonym_finders(self):
        return {"each": _each_finder}

    def _default_metonymy_unfinder(self):
        return {"each": _each_unfinder}

    def string_to_recreate(self):
        return "SCategory::Sameness->new()"

    def get_name(self):
        return "sameness"

    def as_text(self):
        return "sameness"

    def instancer(self, obj):
        """Perl: Instancer($object)."""
        from seqsee.sint import SInt
        items = obj.get_items()
        guess = {
            "length": SInt(obj.get_parts_count()),
            "each": items[0] if items else None,
        }
        guess_built = self.build(guess)
        result_of_can_be_seen_as = obj.can_be_seen_as(guess_built)
        if not util.perl_true(result_of_can_be_seen_as):
            return None
        slippages = sequence_common.instancer_slippages(obj, result_of_can_be_seen_as)
        return SBindings.create(slippages, guess, obj)

    def build(self, args):
        """Perl: build(\\%args). None when length < 1. The bindings share ``args``."""
        if not self.are_attributes_sufficient_to_build(*args):
            raise Confess("Too few params")
        length_ref = args["length"]
        each = args["each"]
        length = sequence_common.mag_of(length_ref)
        if util.perl_num(length) < 1:
            return None
        ret = base._object_create(*(each for _ in util.perl_range(1, length)))
        ret.add_category(self, SBindings.create({}, args, ret))
        ret.set_reln_scheme(RELN_SCHEME.CHAIN)
        return ret

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild: 1 if both each and length, else None."""
        if "each" not in atts or "length" not in atts:
            return None
        return 1
