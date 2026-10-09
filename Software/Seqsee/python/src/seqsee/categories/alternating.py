"""Port of SCategory/Alternating.pm: an object that is one of two pure objects.

Instances are made with ``Alternating.create(o1, o2)`` (Perl ``Create``, memoized in a
``state %MEMO``, which ``_MEMO`` ports). object1/object2 are pure objects
(SLTM::Platonic). Perl compares them with ``eq``, which on refs without an ``eq``
overload is identity.

Hooks, looked up at call time: ``_platonic_create`` (→ ``SLTMPlatonic.create``),
``_sltm_encode``/``_sltm_decode`` (SLTM::encode/decode) and ``_message``
(main::message, a GUI call: logged here). Seqsee::Object->create, Mapping::Numeric->create,
Mapping::Structural->create, FindMapping and $Mapping::Dir::Same go through the hooks in
``base`` and ``numeric``.
"""
import logging

from seqsee import util
from seqsee.categories import base, numeric
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.constants import METO_MODE
from seqsee.errors import Confess
from seqsee.sbindings import SBindings

_log = logging.getLogger(__name__)

# Perl: `state %MEMO` in Create, keyed by "$pure1#$pure2".
_MEMO = {}

_MISSING = object()


def _platonic_create(structure_string):
    """Perl: SLTM::Platonic->create($structure_string)."""
    from seqsee.sltm_platonic import SLTMPlatonic
    return SLTMPlatonic.create(structure_string)


def _sltm_encode(*objects):
    """Perl: SLTM::encode(@objects)."""
    from seqsee import sltm
    return sltm.encode(*objects)


def _sltm_decode(string):
    """Perl: SLTM::decode($string): the list of objects."""
    from seqsee import sltm
    return sltm.decode(string)


def _message(text, level):
    """Perl: main::message($text, $level) (UI::Graphical). Headless: a debug log."""
    _log.debug("%s", text)


def _perl_string(x):
    """How Perl stringifies x: SInt's ``""`` overload, scalars, else the ref string."""
    from seqsee.sint import SInt
    if isinstance(x, SInt):
        return x.as_text()
    if util._is_scalar(x) or x is None or isinstance(x, bool):
        return util.perl_str(x)
    return util.perl_ref_string(x)


def _perl_eq(a, b):
    """Perl ``$a eq $b``: SInt's overload, string comparison of scalars, else identity
    (two unoverloaded refs are eq only if they are the same ref)."""
    from seqsee.sint import SInt
    if isinstance(a, SInt):
        return a == b
    if isinstance(b, SInt):
        return b == a
    if util._is_scalar(a) and util._is_scalar(b):
        return util.perl_str(a) == util.perl_str(b)
    return a is b


def _get_common_categories(first, *others):
    """Perl: ``$first->get_common_categories(@others)`` (SInt or Categorizable)."""
    return first.get_common_categories(*others)


class Alternating(NotMetonyable, SCategory):
    """Perl: SCategory::Alternating."""

    perl_name = "SCategory::Alternating"

    def __init__(self, object1=_MISSING, object2=_MISSING):
        for attr, value in (("object1", object1), ("object2", object2)):
            if value is _MISSING:
                raise Confess(f"Attribute ({attr}) is required")
        self._object1 = object1
        self._object2 = object2
        # Perl: memoize('get_name'), memoize('as_text'), keyed by the object.
        self._memo = {}
        super().__init__()

    @classmethod
    def create(cls, o1, o2):
        """Perl: Create($o1, $o2): one instance per (sorted) pair of pure objects.

        Perl sorts the pures by their stringified refs (addresses), so which one
        becomes object1 is arbitrary; here it is ``util.perl_ref_string`` order."""
        pure1, pure2 = sorted((o1.get_pure(), o2.get_pure()), key=_perl_string)
        key = f"{_perl_string(pure1)}#{_perl_string(pure2)}"
        if key not in _MEMO:
            _MEMO[key] = cls(object1=pure1, object2=pure2)
        return _MEMO[key]

    def object1(self):
        return self._object1

    def object2(self):
        return self._object2

    def set_object1(self, value):
        self._object1 = value

    def set_object2(self, value):
        self._object2 = value

    def is_pure(self):
        return 1

    def instancer(self, obj):
        """Perl: Instancer($object): which = SInt(0) or SInt(1) if obj's pure is
        object1 or object2, else None."""
        from seqsee.sint import SInt
        pure = obj.get_pure()
        if _perl_eq(pure, self._object1):
            return SBindings(raw_slippages={}, bindings={"which": SInt(0)})
        if _perl_eq(pure, self._object2):
            return SBindings(raw_slippages={}, bindings={"which": SInt(1)})
        return None

    def find_mapping_for_cat(self, a, b):
        """Perl: FindMappingForCat($a, $b): 'no_flip' if both are the same of the two
        objects, 'flip' if they are the two different ones, else None."""
        a_pure, b_pure = a.get_pure(), b.get_pure()
        object1, object2 = self._object1, self._object2
        if _perl_eq(a_pure, b_pure):
            if _perl_eq(a_pure, object1) or _perl_eq(a_pure, object2):
                return numeric._mapping_numeric_create("no_flip", self)
            return None
        if _perl_eq(a_pure, object1) and _perl_eq(b_pure, object2):
            return numeric._mapping_numeric_create("flip", self)
        if _perl_eq(a_pure, object2) and _perl_eq(b_pure, object1):
            return numeric._mapping_numeric_create("flip", self)
        return None

    def apply_mapping_for_cat(self, transform, original_object):
        """Perl: ApplyMappingForCat($transform, $original_object). An object gives a new
        Seqsee::Object of the target's structure; a plain value (a structure string) gives
        the bare structure."""
        is_object_a_ref = not (util._is_scalar(original_object) or original_object is None)
        if is_object_a_ref:
            original_object_pure = original_object.get_pure()
        else:
            original_object_pure = _platonic_create(original_object)
        object1, object2 = self._object1, self._object2

        name = transform.get_name()
        if not util.perl_true(name):
            raise Confess("transform without name! " + util.perl_str(transform.as_text()))

        if name == "flip":
            targets = ((object1, object2), (object2, object1))
        elif name == "no_flip":
            targets = ((object1, object1), (object2, object2))
        else:
            raise Confess("Should not be here!")
        for source, target in targets:
            if _perl_eq(original_object_pure, source):
                structure = target.get_structure()
                return base._object_create(structure) if is_object_a_ref else structure
        return None

    def flipping_mapping(self):
        """Perl: FlippingMapping."""
        return numeric._mapping_numeric_create("flip", self)

    def build(self, opts):
        """Perl: build(\\%opts): a Seqsee::Object of object1's structure (which == 0) or
        object2's (which == 1), described as this category."""
        which = opts.get("which")
        if not util.perl_true(which):
            raise Confess("need which")
        mag = which.get_mag()
        # Perl: given/when (0)/(1) smartmatch numerically; an undef mag matches neither.
        if mag is not None and util.perl_num(mag) == 0:
            structure_of_object = self._object1.get_structure()
        elif mag is not None and util.perl_num(mag) == 1:
            structure_of_object = self._object2.get_structure()
        else:
            raise Confess("Should not be here")
        obj = base._object_create(structure_of_object)
        obj.describe_as(self)
        return obj

    def get_name(self):
        # PERL-QUIRK (oracle-confirmed): memoized per object, so the name is fixed by the
        # first call even after set_object1/set_object2. (Perl's Memoize also keeps a
        # separate cache for list context; the port has only the scalar one.)
        if "get_name" not in self._memo:
            self._memo["get_name"] = (_perl_string(self._object1) + " or "
                                      + _perl_string(self._object2))
        return self._memo["get_name"]

    def as_text(self):
        if "as_text" not in self._memo:
            self._memo["as_text"] = self.get_name()
        return self._memo["as_text"]

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild: 1 if 'which' is among the attributes."""
        if any(a is not None and util.perl_str(a) == "which" for a in atts):
            return 1
        return None

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        return [self._object1, self._object2]

    def serialize(self):
        return _sltm_encode(self._object1, self._object2)

    @classmethod
    def deserialize(cls, string):
        o1, o2 = _sltm_decode(string)
        return cls.create(o1, o2)

    @classmethod
    def check_for_alternation(cls, first, second, third):
        """Perl: CheckForAlternation($first, $second, $third).

        If first and third have the same pure, they alternate: every one of the three is
        described as Create(first, second) and the result is that category's flipping
        mapping. Otherwise, for a common non-numeric category, each binding must either
        change the same way twice (FindMapping) or alternate itself (recursively); the
        result is then a Mapping::Structural. Returns None when neither works."""
        from seqsee.sint import SInt
        _message("CheckForAlternation: "
                 + "; ".join(util.perl_str(x.as_text()) for x in (first, second, third)), 1)
        if _perl_eq(first.get_pure(), third.get_pure()):
            alternating_category = cls.create(first.get_pure(), second.get_pure())
            for obj in (first, second, third):
                if isinstance(obj, SInt):
                    # PERL-QUIRK: SInt's eq compares magnitudes, so with first == second
                    # every item gets which = 1. SInt's add_category ignores the bindings.
                    val = 1 if _perl_eq(obj, second) else 0
                    obj.add_category(alternating_category,
                                     SBindings.create({}, {"which": SInt(val)}, obj))
                else:
                    obj.describe_as(alternating_category)
            return alternating_category.flipping_mapping()

        # Perl: `my ($cat) = ... or return;` takes the first common category (hash order).
        common = _get_common_categories(first, second, third)
        if not common:
            return None
        cat = common[0]
        if cat.is_numeric():   # No structure to descend into!
            return None

        bindings = []
        for obj in (first, second, third):
            b = obj.is_of_category_p(cat)
            if not util.perl_true(b):
                return None
            bindings.append(b)
        b1, b2, b3 = (b.get_bindings_ref() for b in bindings)
        keys = list(b1)
        if not util.perl_true(cat.are_attributes_sufficient_to_build(*keys)):
            return None

        changed_bindings = {}
        for key in keys:
            v1, v2, v3 = b1.get(key), b2.get(key), b3.get(key)
            t1 = base._find_mapping(v1, v2)
            t2 = base._find_mapping(v2, v3)
            if util.perl_true(t1) and _perl_eq(t1, t2):
                changed_bindings[key] = t1
                continue
            _message(f"CheckForAlternation recursing (for {key})!", 1)
            new_transform = cls.check_for_alternation(v1, v2, v3)
            if not util.perl_true(new_transform):
                return None
            changed_bindings[key] = new_transform
        return base._structural_create({
            "category": cat,
            "meto_mode": METO_MODE.NONE,
            "direction_reln": base._mapping_dir_same(),
            "slippages": {},
            "changed_bindings": changed_bindings,
        })
