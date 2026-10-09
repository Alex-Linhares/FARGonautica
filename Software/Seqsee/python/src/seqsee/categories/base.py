"""Port of SCategory.pm (Moose role ``SCategory``): the category base class.

Perl's ``requires`` list (Instancer, build, get_name, as_text,
AreAttributesSufficientToBuild, plus get_meto_types from SCategory::MetonymySpec and
get_pure/get_memory_dependencies/serialize/deserialize from LTMStorable) becomes
abstract methods (get_meto_types is inherited from ``metonymy_spec.MetonymySpec``,
the role SCategory consumes). LTMStorable's SpikeBy/InsertISALink (``spike_by``/
``insert_isa_link``) come from the ``ltmstorable.LTMStorable`` mixin.

The ``~~`` overload (literal_comparison_hack_for_smart_match) is ``$_[0] eq $_[1]`` on
refs, i.e. identity, which is Python's default ``==``, so nothing is overridden.

FindMapping/ApplyMapping (Mapping.pm, item 016), Mapping::Structural->create (item 019)
and $Mapping::Dir::Same (item 016) are reached through the module-level hooks
``_find_mapping``, ``_apply_mapping``, ``_structural_create`` and ``_mapping_dir_same``.
``_object_create`` (Seqsee::Object->create, objects/object.py) serves the categories' build.
"""
from abc import abstractmethod

from seqsee import categorizable, util
from seqsee.categories.metonymy_spec import MetonymySpec
from seqsee.constants import METO_MODE
from seqsee.errors import Confess
from seqsee.ltmstorable import LTMStorable


def _find_mapping(a, b):
    """Perl: FindMapping($a, $b)."""
    from seqsee import mapping
    return mapping.find_mapping(a, b)


def _apply_mapping(transform, obj):
    """Perl: ApplyMapping($transform, $obj)."""
    from seqsee import mapping
    return mapping.apply_mapping(transform, obj)


def _structural_create(opts):
    """Perl: Mapping::Structural->create($opts_ref)."""
    from seqsee.mapping.structural import MappingStructural
    return MappingStructural.create(opts)


def _mapping_dir_same():
    """Perl: $Mapping::Dir::Same."""
    from seqsee.mapping import dir as mapping_dir
    return mapping_dir.SAME


def _object_create(*items):
    """Perl: Seqsee::Object->create(@items). Used by the build of the sequence
    categories (Sameness, Ascending, Descending, ...)."""
    from seqsee.objects.object import SeqseeObject
    return SeqseeObject.create(*items)


def _carp_str(x):
    return util.perl_str(x) if util._is_scalar(x) else util.perl_ref_string(x)


class SCategory(MetonymySpec, LTMStorable):
    """Perl role SCategory. Subclasses must call ``super().__init__()`` (Perl BUILD) once
    they are set up: that registers the category with Categorizable (the ``after 'BUILD'``)."""

    perl_name = "SCategory"

    def __init__(self):
        categorizable.register_category(self)

    # --- requires -------------------------------------------------------------------------

    @abstractmethod
    def instancer(self, obj):
        """Perl: Instancer($object): bindings if obj is an instance, else false."""

    @abstractmethod
    def build(self, bindings):
        """Perl: build(\\%bindings): a new object, or false."""

    @abstractmethod
    def get_name(self):
        """Perl: get_name."""

    @abstractmethod
    def as_text(self):
        """Perl: as_text."""

    @abstractmethod
    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild(@atts)."""

    @abstractmethod
    def get_pure(self):
        """Perl: get_pure (required by LTMStorable)."""

    @abstractmethod
    def get_memory_dependencies(self):
        """Perl: get_memory_dependencies (required by LTMStorable)."""

    @abstractmethod
    def serialize(self):
        """Perl: serialize (required by LTMStorable)."""

    @classmethod
    @abstractmethod
    def deserialize(cls, string):
        """Perl: deserialize (required by LTMStorable)."""

    # --- provided ------------------------------------------------------------------------

    def is_instance(self, obj):
        """Perl: is_instance: Instancer, and on success add the category to obj.

        Returns the bindings, or None (Perl: empty return) if Instancer gave a false value."""
        bindings = self.instancer(obj)
        if not util.perl_true(bindings):
            return None
        obj.add_category(self, bindings)
        return bindings

    def is_numeric(self):
        """Perl: IsNumeric (``does("SCategory::Numeric")``)."""
        from seqsee.categories.numeric import Numeric
        return isinstance(self, Numeric)

    def find_mapping_for_cat(self, *args):
        """Perl: FindMappingForCat($o1, $o2): a Mapping::Structural from o1 to o2 as
        instances of this category, or None."""
        if len(args) != 2:
            raise Confess("Need 3 arguments for Default_FindMapping")
        o1, o2 = args
        cat = self
        from seqsee.objects.object import SeqseeObject
        if not (isinstance(o1, SeqseeObject) and isinstance(o2, SeqseeObject)):
            raise Confess(f"Need Seqsee::Objects, got >>{_carp_str(o1)}<< >>{_carp_str(o2)}<<")
        opts = {"first": o1, "second": o2}

        b1 = o1.is_of_category_p(cat)
        if not util.perl_true(b1):
            return None
        b2 = o2.is_of_category_p(cat)
        if not util.perl_true(b2):
            return None
        opts["category"] = cat

        meto_mode = b1.get_metonymy_mode()
        if meto_mode is not b2.get_metonymy_mode():
            return None
        opts["meto_mode"] = meto_mode

        if not calculate_bindings_change(opts, b1.get_bindings_ref(), b2.get_bindings_ref(), cat):
            return None

        if meto_mode.is_metonymy_present():
            if meto_mode.is_position_relevant():
                rel = _find_mapping(b1.get_position(), b2.get_position())
                if not util.perl_true(rel):
                    return None
                opts["position_reln"] = rel
                rel = _find_mapping(b1.get_metonymy_type(), b2.get_metonymy_type())
                if not util.perl_true(rel):
                    return None
                opts["metonymy_reln"] = rel
            else:
                # PERL-QUIRK: metonymy_reln is left unset here (oracle: key missing).
                opts["position_reln"] = ""
        else:
            opts["metonymy_reln"] = ""
            opts["position_reln"] = ""

        opts["direction_reln"] = _mapping_dir_same()
        if opts.get("slippages") is None:
            opts["slippages"] = {}
        return _structural_create(opts)

    def apply_mapping_for_cat(self, transform, original_object):
        """Perl: ApplyMappingForCat($transform, $original_object): the object that
        ``transform`` (a Mapping::Structural on this category) maps the original to, or None."""
        reln = transform
        cat = self
        if original_object is None:
            raise Confess("Missing original_object")
        obj = original_object.get_effective_object()
        if reln.get_category() is not self:
            raise Confess("relation_type and base category do not match")

        bindings = obj.describe_as(cat)
        if not util.perl_true(bindings):
            return None

        bindings_ref = bindings.get_bindings_ref()
        changed_bindings_ref = reln.get_changed_bindings()
        slippages_ref = reln.get_slippages()
        new_bindings_ref = {}

        if slippages_ref:
            util.perl_hash_reset(slippages_ref)   # Perl: keys %$slippages_ref
            for att, old_attr in slippages_ref.items():
                if not util.perl_true(old_attr):
                    continue
                val = bindings_ref.get(old_attr)
                if att in changed_bindings_ref:
                    new_bindings_ref[att] = _apply_mapping(changed_bindings_ref[att], val)
                    if new_bindings_ref[att] is None:
                        return None
                    continue
                new_bindings_ref[att] = val
        else:
            # PERL-QUIRK (not ported): Perl walks this hash with `each` and may return
            # midway, leaving the hash's iterator part-way for the next `each` on it.
            for k, v in bindings_ref.items():
                if k in changed_bindings_ref:
                    new_bindings_ref[k] = _apply_mapping(changed_bindings_ref[k], v)
                    if new_bindings_ref[k] is None:
                        return None
                    continue
                new_bindings_ref[k] = v

        ret_obj = cat.build(new_bindings_ref)
        if not util.perl_true(ret_obj):
            return None

        # Perl `==` on the METO_MODE refs: identity.
        reln_meto_mode = reln.get_meto_mode()
        if reln_meto_mode is not bindings.get_metonymy_mode():
            return None

        if reln_meto_mode is not METO_MODE.NONE:
            new_metonymy_type = _apply_mapping(reln.get_metonymy_reln(), bindings.get_metonymy_type())
            if not util.perl_true(new_metonymy_type):
                return None
            if reln_meto_mode is METO_MODE.ALL:
                ret_obj = ret_obj.apply_blemish_everywhere(new_metonymy_type)
            else:
                new_position = _apply_mapping(reln.get_position_reln(), bindings.get_position())
                if not util.perl_true(new_position):
                    return None
                try:
                    blemished = ret_obj.apply_blemish_at(new_metonymy_type, new_position)
                except Exception:  # Perl: eval { ... }
                    blemished = None
                if not util.perl_true(blemished):
                    return None
                ret_obj = blemished

        ret_obj.describe_as(cat)
        # Perl reads `$reln->get_direction_reln() // $Mapping::Dir::Same` and never uses it.
        reln.get_direction_reln()
        ret_obj.set_group_p(1)
        return ret_obj


def calculate_bindings_change(output, bindings_1, bindings_2, cat):
    """Perl: CalculateBindingsChange: without slippages if possible, else with them."""
    if calculate_bindings_change_no_slips(output, bindings_1, bindings_2, cat):
        return 1
    return calculate_bindings_change_with_slips(output, bindings_1, bindings_2, cat)


def calculate_bindings_change_no_slips(output, bindings_1, bindings_2, cat):
    """Perl: CalculateBindingsChange_no_slips: FindMapping per attribute, same names.

    Returns 1 and fills output's changed_bindings/unchanged_bindings, or None (output
    untouched). Confesses if bindings_2 lacks one of bindings_1's attributes."""
    changed = {}
    unchanged = {}
    for k, v1 in bindings_1.items():
        if k not in bindings_2:
            raise Confess(f"In CalculateBindingsChange_no_slips:: binding for {util.perl_str(k)} "
                          "missing for second object!")
        rel = _find_mapping(v1, bindings_2[k])
        if not util.perl_true(rel):
            return None
        changed[k] = rel
    output["changed_bindings"] = changed
    output["unchanged_bindings"] = unchanged
    return 1


def calculate_bindings_change_with_slips(output, bindings_1, bindings_2, cat, is_reverse=None):
    """Perl: CalculateBindingsChange_with_slips: each attribute of bindings_2 takes the
    first attribute of bindings_1 (in shuffled order) that FindMapping relates to it.

    Attributes with no partner are skipped. The slipped attribute names must satisfy
    AreAttributesSufficientToBuild, and unless is_reverse the reverse direction is checked
    too. Returns 1 (setting output's slippages) or None.

    The shuffle draws from the central RNG, over uniq(keys b2, keys b1) in insertion
    order (Perl: hash order), so seeded runs pick partners differently from Perl."""
    changed = {}
    unchanged = {}
    slips = {}
    attributes = util.uniq(*bindings_2.keys(), *bindings_1.keys())
    for k2, v2 in bindings_2.items():
        for k in util.shuffle(attributes):
            rel = _find_mapping(bindings_1.get(k), v2)
            if rel is None:  # Perl: `// next` (definedness, not truth)
                continue
            changed[k2] = rel
            slips[k2] = k
            break
    output["changed_bindings"] = changed
    output["unchanged_bindings"] = unchanged
    if not util.perl_true(cat.are_attributes_sufficient_to_build(*sorted(slips, key=util.perl_str))):
        return None
    if not is_reverse:
        if not calculate_bindings_change_with_slips({}, bindings_2, bindings_1, cat, 1):
            return None
    output["slippages"] = slips
    return 1
