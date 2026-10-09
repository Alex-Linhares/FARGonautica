"""Port of Seqsee/Object.pm (``Seqsee::Object``).

Part I (item 021): construction, attributes, items, ``create``, categories (describe_as &
co.), the structure basics and the relation-hash handles. Part II (item 022): metonyms,
relations, apply_blemish_at, the CanBeSeenAs multimethod (``CAN_BE_SEEN_AS``) and its
helpers, effective slippages, squintability, UpdateStrength, set_underlying_ruleapp,
get_pure.

A Moose class with ``with 'Categorizable'``. ``SeqseeObject({...})`` or kwargs is Moose
``new``; ``SeqseeObject.create(...)`` is Perl's ``create``. Perl's ``@{}`` overload is
``__iter__``/``__getitem__``/``__len__``; objects stay true even when empty (``__bool__``).
The ``~~`` overload is ``eq`` on refs, i.e. identity, which is Python's default ``==``, so
there is no ``__eq__`` and hashing is by identity.

Naming: CamelCase methods are snake_case, except ``SetMetonym`` and
``SetMetonymActiveness``, which keep their Perl names because their snake_case forms are
the Moose writers ``set_metonym``/``set_metonym_activeness`` (Perl has both).

Hooks for modules not ported yet, looked up at call time (tests monkeypatch them):
``_find_mapping`` (→ ``mapping.find_mapping``), ``_srelation_new`` (SRelation->new,
item 024), ``_srule_create`` (SRule->create, item 025),
``_get_real_activations_for_concepts`` (SLTM) and ``_platonic_create``
(SLTM::Platonic->create, via ``alternating._platonic_create``, wired in item 028).

PERL-QUIRKs (oracle-confirmed):
- ``direction`` defaults to undef, not $DIR::RIGHT: the default is evaluated when
  Object.pm loads, before S.pm has created the DIR singletons.
- describe_as adds a new category twice (SCategory's is_instance adds it, then
  describe_as does), so the history says "Added category X" twice. redescribe_as does the
  same on success.
- create of a one-item group returns a copy of the item itself, not a group.
- MaybeAnnotateWithMetonym dies ("Died") whenever AnnotateWithMetonym succeeds: with no
  exception, ``Exception::Class->caught()`` is "" and it does ``die ""``.
- CanBeSeenAs: strings and undef dispatch as "$", and only (Seqsee::Element, "$") exists,
  so a group with a string or undef dies with "No viable candidate". An inactive metonym
  is still tried after the literal and by-part checks.
"""
import re
import sys
import weakref

from seqsee import util
from seqsee.categorizable import Categorizable
from seqsee.errors import SErr, Confess, MetonymNotAppicable
from seqsee.multimethods import Multimethod, perl_isa
from seqsee.shistory import SHistory

_MISSING = object()


def _element_create(mag, pos):
    """Perl: Seqsee::Element->create($mag, $pos), through numeric's hook (tests patch it)."""
    from seqsee.categories import numeric
    return numeric._element_create(mag, pos)


def _find_mapping(a, b):
    """Perl: FindMapping($a, $b) (the multimethod in Mapping.pm)."""
    from seqsee import mapping
    return mapping.find_mapping(a, b)


def _srelation_new(opts):
    """Perl: SRelation->new({first =>, second =>, type =>}) (item 024)."""
    from seqsee.srelation import SRelation
    return SRelation(opts)


def _srule_create(reln):
    """Perl: SRule->create($reln)."""
    from seqsee.srule import SRule
    return SRule.create(reln)


def _get_real_activations_for_concepts(concepts):
    """Perl: SLTM::GetRealActivationsForConcepts(\\@concepts): a list of activations."""
    from seqsee import sltm
    return sltm.get_real_activations_for_concepts(concepts)


def _platonic_create(structure_string):
    """Perl: SLTM::Platonic->create($string). Delegates to alternating's hook (item 028)."""
    from seqsee.categories import alternating
    return alternating._platonic_create(structure_string)


def _perl_string(x):
    """Perl's string form of a value interpolated into a message."""
    if x is None or util._is_scalar(x) or isinstance(x, bool):
        return util.perl_str(x)
    if isinstance(x, dict):
        return f"HASH(0x{id(x):x})"
    if isinstance(x, (list, tuple)):
        return f"ARRAY(0x{id(x):x})"
    return util.perl_ref_string(x)


def _result():
    from seqsee.objects import result_of_can_be_seen_as
    return result_of_can_be_seen_as.ResultOfCanBeSeenAs


def _moose_value(value):
    """How Moose shows a value in a type-constraint failure."""
    if value is None:
        return "undef"
    if isinstance(value, bool):
        return "1" if value else ""
    if isinstance(value, str):
        # Moose quotes strings that don't look like numbers ("x", but +3 and " 3" bare).
        return value if util.looks_like_number(value) else f'"{value}"'
    if util._is_scalar(value):
        return util.perl_str(value)
    if isinstance(value, dict) and not value:
        return "{  }"
    if isinstance(value, (list, tuple)):
        # Oracle-confirmed for scalars and DIR items ("[ 1 ]", "[ DIR={ … } ]"). Moose
        # shows refs nested inside an object item as HASH(0x…); not reproduced here.
        return "[ " + ", ".join(_moose_value(v) for v in value) + " ]" if value else "[  ]"
    from seqsee.constants import DIR
    if isinstance(value, DIR):
        # Moose dumps small blessed hashes (oracle-confirmed for DIR, in item 024).
        return f'DIR={{ text: "{value.text}" }}'
    return util.perl_ref_string(value)


def _moose_new_args(perl_name, args, kwargs):
    """The arguments of a Moose ``new``: a hash ref (a dict) and/or key-value pairs."""
    if args:
        if len(args) != 1 or not isinstance(args[0], dict):
            raise Confess(f"{perl_name}: odd arguments to constructor")
        kwargs = {**args[0], **kwargs}
    return kwargs


def _moose_weaken(value):
    """A Moose ``weak_ref`` attribute value, as a zero-argument getter.

    Objects are held through a ``weakref`` and read as None once collected (Perl frees
    them at once; CPython does too, unless a reference cycle keeps them for the cycle
    collector). Scalars aren't refs, so Moose doesn't weaken them. Port difference: Perl
    also weakens array/hash refs, but Python lists and dicts can't be weakly referenced,
    so they are held strongly."""
    try:
        ref = weakref.ref(value)
    except TypeError:
        return lambda: value
    return ref


def _type_error(attr, type_name, value):
    return Confess(f"Attribute ({attr}) does not pass the type constraint because: "
                   f"Validation failed for '{type_name}' with value {_moose_value(value)}")


def _is_bool(value):
    """Moose ``Bool``: undef, "", 0, 1 (by string form)."""
    return value is None or isinstance(value, bool) or (
        util._is_scalar(value) and util.perl_str(value) in ("", "0", "1"))


_INT_RE = re.compile(r"-?[0-9]+")


def _is_int(value):
    """Moose ``Int``: a defined non-ref whose string form matches ``/\\A-?[0-9]+\\z/``."""
    return util._is_scalar(value) and not isinstance(value, bool) and \
        _INT_RE.fullmatch(util.perl_str(value)) is not None


def _check(attr, value):
    """The Moose ``isa`` checks of the typed attributes (Object, Anchored, Element)."""
    if attr in ("group_p", "metonym_activeness", "is_locked_against_deletion"):
        if not _is_bool(value):
            raise _type_error(attr, "Bool", value)
    elif attr == "history_obj":
        if not isinstance(value, SHistory):
            raise _type_error(attr, "SHistory", value)
    elif attr == "item":
        if not isinstance(value, list):
            raise _type_error(attr, "ArrayRef", value)
    elif attr in ("reln_other_end", "categories"):
        if not isinstance(value, dict):
            raise _type_error(attr, "HashRef", value)
    elif attr == "mag":
        if not _is_int(value):
            raise _type_error(attr, "Int", value)
    return value


def _perl_eq(a, b):
    """Perl ``eq`` between items: strings for scalars, identity for objects (no ``""``
    overload on Seqsee::Object)."""
    if util._is_scalar(a) and util._is_scalar(b):
        return util.perl_str(a) == util.perl_str(b)
    return a is b


def _ref_string(x):
    return "" if x is None else util.perl_ref_string(x)


class SeqseeObject(Categorizable):
    """Perl: Seqsee::Object (``with 'Categorizable'``)."""

    perl_name = "Seqsee::Object"

    # The Moose constructor visits the attributes in name order (subclass attributes
    # included) and, for each, does the required check and then the type check
    # (oracle-confirmed). Triples are (attribute, init_arg, required); the types are in
    # _check. Subclasses extend this.
    _ATTRS = (("categories", "categories", False), ("group_p", "group_p", True),
              ("history_obj", "history_obj", False), ("item", "items", False),
              ("metonym_activeness", "metonym_activeness", False),
              ("reln_other_end", "reln_other_end", False))

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess(f"{self.perl_name}: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        for attr, init_arg, required in sorted(self._ATTRS):
            if init_arg in kwargs:
                _check(attr, kwargs[init_arg])
            elif required:
                raise Confess(f"Attribute ({attr}) is required")
        self._init_attributes(kwargs)

    def _init_attributes(self, kwargs):
        """Store the (already checked) constructor arguments. Subclasses extend this."""
        self._strength = kwargs.get("strength", 0)
        self._history_obj = kwargs["history_obj"] if "history_obj" in kwargs else SHistory()
        self._group_p = kwargs["group_p"]
        self._metonym = kwargs.get("metonym")
        self._metonym_activeness = kwargs.get("metonym_activeness", 0)
        self._is_a_metonym = kwargs.get("is_a_metonym")
        # PERL-QUIRK: the default $DIR::RIGHT is still undef when Object.pm loads.
        self._direction = kwargs.get("direction")
        self._reln_scheme = kwargs.get("reln_scheme")
        self._reln_other_end = kwargs["reln_other_end"] if "reln_other_end" in kwargs else {}
        self._underlying_reln = kwargs.get("underlying_reln")
        self._item = kwargs["items"] if "items" in kwargs else []
        if "categories" in kwargs:
            self._categories = kwargs["categories"]

    # --- attributes ----------------------------------------------------------------------

    def get_strength(self):
        return self._strength

    def set_strength(self, value):
        self._strength = value

    def get_history_obj(self):
        return self._history_obj

    def set_history_obj(self, value):
        self._history_obj = _check("history_obj", value)

    def get_group_p(self):
        return self._group_p

    def set_group_p(self, value):
        self._group_p = _check("group_p", value)

    def get_metonym(self):
        return self._metonym

    def set_metonym(self, value):
        self._metonym = value

    def get_metonym_activeness(self):
        return self._metonym_activeness

    def set_metonym_activeness(self, value):
        self._metonym_activeness = _check("metonym_activeness", value)

    def get_is_a_metonym(self):
        """Perl: get_is_a_metonym (the object this one is a metonym of)."""
        return self._is_a_metonym

    def set_is_a_metonym(self, value):
        self._is_a_metonym = value

    def get_direction(self):
        return self._direction

    def set_direction(self, value):
        self._direction = value

    def get_reln_scheme(self):
        return self._reln_scheme

    def set_reln_scheme(self, value):
        self._reln_scheme = value

    def get_underlying_reln(self):
        return self._underlying_reln

    def set_underlying_reln(self, value):
        self._underlying_reln = value

    def item(self, *value):
        """Perl: the ``item`` accessor (``is => 'rw'`` with only a reader named)."""
        if value:
            self._item = _check("item", value[0])
        return self._item

    def reln_other_end(self):
        """Perl: the ``reln_other_end`` accessor: {other object: relation}, live."""
        return self._reln_other_end

    # --- history (delegated to SHistory) -------------------------------------------------

    def get_history(self):
        return self._history_obj.get_history()

    def add_history(self, msg):
        """Perl: AddHistory."""
        self._history_obj.add_history(msg)

    def unchanged_since(self, since):
        """Perl: UnchangedSince."""
        return self._history_obj.unchanged_since(since)

    def search_history(self, pattern):
        return self._history_obj.search_history(pattern)

    def history_as_text(self):
        return self._history_obj.history_as_text()

    def get_age(self):
        """Perl: GetAge."""
        return self._history_obj.get_age()

    # --- relation hash (Moose Hash trait handles) ----------------------------------------
    # Perl keys the hash by the other end's ref string; here by the object itself.

    def get_relation(self, other):
        return self._reln_other_end.get(other)

    def set_relation_to(self, other, reln):
        self._reln_other_end[other] = reln
        return reln

    def remove_reln_to(self, other):
        return self._reln_other_end.pop(other, None)

    def relation_exists_to(self, other):
        return other in self._reln_other_end

    def all_relations(self):
        """Perl: all_relations (Hash 'values'; hash order there, insertion order here)."""
        return list(self._reln_other_end.values())

    # --- items (Moose Array trait handles, and the @{} overload) -------------------------

    def get_parts_ref(self):
        """The live item list (Perl: the array ref)."""
        return self._item

    def get_items(self):
        return self._item

    def get_items_array(self):
        """Perl: get_items_array (Array 'elements'): a copy of the items."""
        return list(self._item)

    def get_parts_count(self):
        return len(self._item)

    def __iter__(self):
        return iter(self._item)

    def __getitem__(self, index):
        return self._item[index]

    def __len__(self):
        return len(self._item)

    def __bool__(self):
        return True

    # --- create ---------------------------------------------------------------------------

    @classmethod
    def create(cls, *arguments):
        """Perl: create(@arguments).

        - no arguments: an empty group;
        - one non-ref (a number): Seqsee::Element->create($n, 0);
        - one array: create(@array);
        - one object: a copy (a fresh Element for an exact Seqsee::Element, else
          create(@items), so a one-item group gives a copy of its item), then describe_as
          each of the original's categories;
        - several: a group of Seqsee::Object->create($_) for each.
        """
        if not arguments:
            return cls({"group_p": 1, "items": []})
        if len(arguments) == 1:
            sole = arguments[0]
            if util._is_scalar(sole) or isinstance(sole, bool):
                return _element_create(sole, 0)
            if isinstance(sole, (list, tuple)):
                return cls.create(*sole)
            if isinstance(sole, dict):
                raise Confess('Can\'t call method "get_categories" on unblessed reference')
            categories = list(sole.get_categories())
            if util.perl_ref(sole) == "Seqsee::Element":
                new_object = _element_create(sole.get_mag(), 0)
            else:
                new_object = cls.create(*sole.get_items_array())
            for cat in categories:
                new_object.describe_as(cat)
            return new_object
        new_arguments = [SeqseeObject.create(a) for a in arguments]
        return cls({"group_p": 1, "items": new_arguments})

    # --- categories -----------------------------------------------------------------------

    def annotate_with_cat(self, cat):
        """Perl: annotate_with_cat: describe_as, or throw SErr "Not of category"."""
        bindings = self.describe_as(cat)
        if not util.perl_true(bindings):
            SErr.throw("Not of category")
        return bindings

    def describe_as(self, cat):
        """Perl: describe_as: the existing bindings if already of the category; else
        is_instance, and on success add_category (a second time; see the module doc)."""
        is_of_cat = self.is_of_category_p(cat)
        if util.perl_true(is_of_cat):
            return is_of_cat
        bindings = cat.is_instance(self)
        if util.perl_true(bindings):
            self.add_category(cat, bindings)
        return bindings

    def redescribe_as(self, cat):
        """Perl: redescribe_as: re-run is_instance; keep (re-add) or remove the category."""
        bindings = cat.is_instance(self)
        name = util.perl_str(cat.get_name())
        if util.perl_true(bindings):
            self.add_history(f"redescribe as instance of category {name} succeded")
            self.add_category(cat, bindings)
        else:
            self.add_history(f"redescribe as instance of category {name} failed")
            self.remove_category(cat)
        return bindings

    def recalculate_categories(self):
        """Perl: recalculate_categories: redescribe_as every category; confess if none is left."""
        cats = self.get_categories()
        for cat in cats:
            self.redescribe_as(cat)
        if not self.category_list_as_strings():
            had = " ".join(_ref_string(c) for c in cats)
            raise Confess(f"LOST ALL CATEGORIES!!! {util.perl_ref_string(self)}. Had {had}\n")

    # --- structure basics -----------------------------------------------------------------

    def get_structure(self):
        """Perl: get_structure: the only item's structure for one item, else a list."""
        if self.get_parts_count() == 1:
            return self._item[0].get_structure()
        return [x.get_structure() for x in self._item]

    def get_flattened(self):
        return [y for x in self._item for y in x.get_flattened()]

    def get_structure_string(self):
        return util.structure_to_string(self.get_structure())

    def get_span(self):
        """Perl: List::Util::sum of the items' spans: None for no items, undef spans count 0."""
        if not self._item:
            return None
        return sum(util.perl_num(x.get_span()) for x in self._item)

    def as_text(self):
        return "Seqsee::Object " + util.perl_str(self.get_structure_string())

    def has_as_item(self, item):
        """Perl: HasAsItem: 1 if some item ``eq`` item, else 0."""
        for x in self._item:
            if _perl_eq(x, item):
                return 1
        return 0

    def has_as_part_deep(self, item):
        """Perl: HasAsPartDeep: 1 if item is an item here or (recursively) inside one, else 0."""
        for x in self._item:
            if _perl_eq(x, item):
                return 1
            if util.perl_true(x.has_as_part_deep(item)):
                return 1
        return 0

    def get_annotated_structure_string(self):
        """Perl: GetAnnotatedStructureString: the magnitude for an element, else
        "[a, b, ...]" of the items' annotated strings; an active metonym appends
        " --*-> <effective object's structure string>"."""
        from seqsee.objects.element import Element
        if isinstance(self, Element):
            body = self.get_mag()
        else:
            body = "[" + ", ".join(util.perl_str(x.get_annotated_structure_string())
                                   for x in self._item) + "]"
        if util.perl_true(self.get_metonym_activeness()):
            meto_structure_string = self.get_effective_object().get_structure_string()
            body = util.perl_str(body) + " --*-> " + util.perl_str(meto_structure_string)
        return body

    def get_pure(self):
        """Perl: get_pure: SLTM::Platonic->create(structure string)."""
        return _platonic_create(self.get_structure_string())

    # --- blemishes ------------------------------------------------------------------------

    def apply_blemish_at(self, meto_type, position):
        """Perl: apply_blemish_at($meto_type, $position).

        Blemishes the item(s) at position with meto_type, builds a copy of the whole object
        with Seqsee::Object->create, then gives each blemished copy the metonym (starred =
        the original item, unstarred = the blemished object before copying, so the weak
        unstarred ref dies with it) and turns it on. The original item's is_a_metonym is
        pointed at the copy."""
        from seqsee.smetonym import SMetonym
        indices = list(position.find_range(self))
        metonyms = []
        subobjects = self.get_items_array()
        meto_cat = meto_type.get_category()
        meto_name = meto_type.get_name()
        for index in indices:
            obj_at_pos = subobjects[index]
            blemished_object_at_pos = meto_type.blemish(obj_at_pos)
            metonyms.append(SMetonym({
                "category": meto_cat,
                "name": meto_name,
                "info_loss": meto_type.get_info_loss(),
                "starred": obj_at_pos,
                "unstarred": blemished_object_at_pos,
            }))
            subobjects[index] = blemished_object_at_pos
        ret = SeqseeObject.create(*subobjects)
        for index in indices:
            metonym = metonyms.pop(0)
            ret[index].describe_as(meto_cat)
            ret[index].SetMetonym(metonym)
            metonym.get_starred().set_is_a_metonym(ret[index])
            ret[index].SetMetonymActiveness(1)
        return ret

    # --- metonyms -------------------------------------------------------------------------

    def SetMetonym(self, meto):  # noqa: N802 (set_metonym is the Moose writer)
        """Perl: SetMetonym($meto): the starred object must be a Seqsee::Object; it is
        marked as a metonym of self, and meto becomes self's metonym. Returns meto."""
        starred = meto.get_starred()
        if not isinstance(starred, SeqseeObject):
            SErr.throw(f"Metonym must be an Seqsee::Object! Got: {_perl_string(starred)}")
        starred.set_is_a_metonym(self)
        self.set_metonym(meto)
        return meto

    def SetMetonymActiveness(self, value):  # noqa: N802 (set_metonym_activeness is the writer)
        """Perl: SetMetonymActiveness($value). Turning it on needs a metonym and does
        nothing (returns None) if already on; returns the new activeness (1/0)."""
        if util.perl_true(value):
            if util.perl_true(self.get_metonym_activeness()):
                return None
            if not util.perl_true(self.get_metonym()):
                SErr.throw("Cannot SetMetonymActiveness without a metonym")
            self.add_history("Metonym activeness turned on")
            self.set_metonym_activeness(1)
            return 1
        self.add_history("Metonym activeness turned off")
        self.set_metonym_activeness(0)
        return 0

    def get_effective_object(self):
        """Perl: GetEffectiveObject: the metonym's starred object when active, else self."""
        if util.perl_true(self.get_metonym_activeness()):
            return self.get_metonym().get_starred()
        return self

    def get_effective_structure(self):
        """Perl: GetEffectiveStructure: the items' effective objects' structures (always
        a list, even for one item)."""
        return [x.get_effective_object().get_structure() for x in self._item]

    def get_effective_structure_string(self):
        """Perl: GetEffectiveStructureString."""
        return util.structure_to_string(self.get_effective_structure())

    def get_concrete_object(self):
        """Perl: GetConcreteObject: the object this is a metonym of, or self."""
        is_a_metonym = self.get_is_a_metonym()
        return is_a_metonym if util.perl_true(is_a_metonym) else self

    def annotate_with_metonym(self, cat, name):
        """Perl: AnnotateWithMetonym($cat, $name): annotate_with_cat if needed (may throw
        SErr "Not of category"), then the category's metonym, or SErr::MetonymNotAppicable."""
        if not util.perl_true(self.is_of_category_p(cat)):
            self.annotate_with_cat(cat)
        if cat is None or util._is_scalar(cat) or not hasattr(cat, "find_metonym"):
            from seqsee.mapping.numeric import _method_target
            _method_target(cat, "find_metonym")
            raise Confess(f'Can\'t locate object method "find_metonym" via package '
                          f'"{util.perl_ref(cat)}"')
        meto = cat.find_metonym(self, name)
        if not util.perl_true(meto):
            MetonymNotAppicable.throw()
        self.add_history(f'Added metonym "{util.perl_str(name)}" for cat '
                         f'{util.perl_str(cat.get_name())}')
        return self.SetMetonym(meto)

    def maybe_annotate_with_metonym(self, cat, name):
        """Perl: MaybeAnnotateWithMetonym: AnnotateWithMetonym, ignoring
        SErr::MetonymNotAppicable; other errors propagate.

        PERL-QUIRK (oracle-confirmed): on success Perl does ``die ""`` ("Died")."""
        try:
            self.annotate_with_metonym(cat, name)
        except MetonymNotAppicable:
            return None
        raise Confess("Died")

    def is_this_a_metonymed_object(self):
        """Perl: IsThisAMetonymedObject: 1 if it is a metonym of some other object."""
        is_a_metonym_of = self.get_is_a_metonym()
        if not util.perl_true(is_a_metonym_of) or is_a_metonym_of is self:
            return 0
        return 1

    def contains_a_metonym(self):
        """Perl: ContainsAMetonym: 1 if this or (recursively) an item is a metonym."""
        if self.is_this_a_metonymed_object():
            return 1
        for x in self._item:
            if util.perl_true(x.contains_a_metonym()):
                return 1
        return 0

    # --- relations ------------------------------------------------------------------------

    def add_relation(self, reln):
        """Perl: AddRelation (SErr "duplicate reln being added" if one exists)."""
        other = self._get_other_end_of_reln(reln)
        if self.relation_exists_to(other):
            SErr.throw("duplicate reln being added")
        self.add_history("added reln to " + util.perl_str(other.get_bounds_string()))
        return self.set_relation_to(other, reln)

    def remove_relation(self, reln):
        """Perl: RemoveRelation."""
        other = self._get_other_end_of_reln(reln)
        self.add_history("removed reln to " + util.perl_str(other.get_bounds_string()))
        return self.remove_reln_to(other)

    def remove_all_relations(self):
        """Perl: RemoveAllRelations: uninsert every relation."""
        for reln in self.all_relations():
            reln.uninsert()

    def _get_other_end_of_reln(self, reln):
        """Perl: _get_other_end_of_reln (SErr "relation error: not an end")."""
        f, s = reln.get_ends()
        if f is self:
            return s
        if s is self:
            return f
        SErr.throw("relation error: not an end")

    def recalculate_relations(self):
        """Perl: recalculate_relations: re-find each relation's mapping with its type's
        category; replace the relation (new one inserted) or drop it."""
        for reln in self.all_relations():
            typ = reln.get_type()
            new_type = typ.get_category().find_mapping_for_cat(*reln.get_ends())
            if util.perl_true(new_type):
                f, s = reln.get_ends()
                new_rel = _srelation_new({"first": f, "second": s, "type": new_type})
                reln.uninsert()
                new_rel.insert()
            else:
                reln.uninsert()

    def apply_reln_scheme(self, scheme):
        """Perl: apply_reln_scheme($scheme): nothing for a false scheme; CHAIN relates each
        neighbouring pair that has no (true) relation yet; anything else confesses."""
        from seqsee.constants import RELN_SCHEME
        if not util.perl_true(scheme):
            return None
        if scheme is RELN_SCHEME.CHAIN:
            parts = self.get_parts_ref()
            for i in range(self.get_parts_count() - 1):
                a, b = parts[i], parts[i + 1]
                if util.perl_true(a.get_relation(b)):
                    continue
                transform = _find_mapping(a, b)
                rel = _srelation_new({"first": a, "second": b, "type": transform})
                if util.perl_true(rel):
                    rel.insert()
            self.add_history('Relation scheme "chain" applied')
            return None
        raise Confess(f"Relation scheme {_perl_string(scheme)} not implemented")

    # --- CanBeSeenAs ----------------------------------------------------------------------

    def can_be_seen_as(self, structure):
        """Perl: ``$object->CanBeSeenAs($structure)`` (the multimethod)."""
        return CAN_BE_SEEN_AS.call(self, structure)

    def can_be_seen_as_by_part(self, structure):
        """Perl: CanBeSeenAs_ByPart: item by item. None if the counts differ or a part
        fails or is itself blemished by parts; else unblemished, or by-part with each
        entirely-blemished part's metonym."""
        if structure is None:
            raise Confess("Can't use an undefined value as an ARRAY reference")
        if util._is_scalar(structure) or isinstance(structure, bool):
            raise Confess(f'Can\'t use string ("{util.perl_str(structure)}") as an ARRAY ref '
                          'while "strict refs" in use')
        if isinstance(structure, dict):
            raise Confess("Not an ARRAY reference")
        seen_as_part_count = len(structure)
        if len(self) != seen_as_part_count:
            return None
        blemishes = {}
        obj_part_ref = self.get_parts_ref()
        for i in range(seen_as_part_count):
            part_can_be_seen_as = CAN_BE_SEEN_AS.call(obj_part_ref[i], structure[i])
            if not util.perl_true(part_can_be_seen_as):
                return None
            if part_can_be_seen_as.are_parts_blemished():
                return None
            if not part_can_be_seen_as.is_blemished():
                continue
            blemishes[i] = part_can_be_seen_as.get_entire_blemish()
        if not blemishes:
            return _result().new_unblemished()
        return _result().new_by_part(blemishes)

    def can_be_seen_as_meto(self, *args):
        """Perl: CanBeSeenAs_Meto($structure, $starred, $metonym): entire blemish when the
        starred structure matches, else None. Needs exactly three arguments."""
        if len(args) != 3:
            raise Confess("")
        structure, starred, metonym = args
        if util.compare_deep(starred.get_structure(), structure):
            return _result().new_entire_blemish(metonym)
        return None

    def can_be_seen_as_literal(self, structure):
        """Perl: CanBeSeenAs_Literal: unblemished if the structures match, else None."""
        if util.compare_deep(self.get_structure(), structure):
            return _result().new_unblemished()
        return None

    def can_be_seen_as_literal_or_meto(self, structure):
        """Perl: CanBeSeenAs_Literal0rMeto (sic, a zero): an active metonym's starred
        structure, then the literal structure, then an inactive metonym's; else None."""
        if isinstance(structure, SeqseeObject):
            structure = structure.get_structure()
        meto_activeness = self.get_metonym_activeness()
        metonym = self.get_metonym()
        starred = metonym.get_starred() if util.perl_true(metonym) else None
        if util.perl_true(meto_activeness):
            if util.compare_deep(starred.get_structure(), structure):
                return _result().new_entire_blemish(metonym)
        if util.compare_deep(self.get_structure(), structure):
            return _result().new_unblemished()
        if util.perl_true(metonym):
            if util.compare_deep(starred.get_structure(), structure):
                return _result().new_entire_blemish(metonym)
        return None

    def get_effective_slippages(self):
        """Perl: GetEffectiveSlippages: {index: metonym} for items whose metonym is active.
        Keys are ints (Perl: strings)."""
        return {idx: part.get_metonym() for idx, part in enumerate(self.get_items_array())
                if util.perl_true(part.get_metonym_activeness())}

    # --- squintability --------------------------------------------------------------------

    def check_squintability(self, intended):
        """Perl: CheckSquintability($intended): the metonym types, over all categories, whose
        squinted starred object has the intended structure string. Categories come in
        get_categories order (Perl: hash order)."""
        intended_structure_string = intended.get_structure_string()
        ret = []
        for cat in self.get_categories():
            ret.extend(self.check_squintability_for_category(intended_structure_string, cat))
        return ret

    def check_squintability_for_category(self, intended_structure_string, category):
        """Perl: CheckSquintabilityForCategory: confesses unless self is an instance."""
        bindings = self.get_binding_for_category(category)
        if not util.perl_true(bindings):
            raise Confess("CheckSquintabilityForCategory called on object not an instance "
                          "of the category")
        ret = []
        for name in category.get_meto_types():
            finder = category.get_meto_finder(name)
            squinted = finder(self, category, name, bindings)
            if not util.perl_true(squinted):
                continue
            starred_string = squinted.get_starred().get_structure_string()
            if util.perl_str(starred_string) != util.perl_str(intended_structure_string):
                continue
            ret.append(squinted.get_type())
        return ret

    # --- strength, rule apps --------------------------------------------------------------

    def update_strength(self):
        """Perl: UpdateStrength: 20 + 0.2 × (sum of item strengths) + 30 × (sum of the
        categories' real activations) + $Global::GroupStrengthByConsistency{$self},
        capped at 100 (no floor). Returns the new strength."""
        from seqsee import global_ as Global
        part_strengths = [x.get_strength() for x in self.get_parts_ref()]
        parts_sum = sum(util.perl_num(s) for s in part_strengths) if part_strengths else 0
        strength_from_parts = 20 + 0.2 * (parts_sum or 0)
        activations = _get_real_activations_for_concepts(self.get_categories())
        acts_sum = sum(util.perl_num(a) for a in activations) if activations else 0
        strength_from_categories = 30 * (acts_sum or 0)
        strength = strength_from_parts + strength_from_categories
        consistency = Global.GroupStrengthByConsistency.get(self)
        strength += util.perl_num(consistency) if util.perl_true(consistency) else 0
        if strength > 100:
            strength = 100
        self.set_strength(strength)
        return strength

    def set_underlying_ruleapp(self, reln):
        """Perl: set_underlying_ruleapp($reln). An SRelation or Mapping is first turned into
        a rule (SRule->create; nothing happens if that fails). The rule's
        CheckApplicability over the items (direction RIGHT) becomes the underlying reln,
        even when undef. Anything else prints and confesses "Funny argument ..."."""
        from seqsee.constants import DIR
        if not util.perl_true(reln):
            raise Confess("Cannot set underlying relation to be an undefined value!")
        if perl_isa(reln, "SRelation") or perl_isa(reln, "Mapping"):
            reln = _srule_create(reln)
            if not util.perl_true(reln):
                return None
        if perl_isa(reln, "SRule"):
            ruleapp = reln.check_applicability({
                "objects": self.get_items_array(),
                "direction": DIR.RIGHT,
            })
        else:
            message = f"Funny argument {_perl_string(reln)} to set_underlying_ruleapp!"
            sys.stdout.write(message)
            raise Confess(message)
        self.add_history(f"Underlying relation set: {_perl_string(ruleapp)} ")
        self.set_underlying_reln(ruleapp)
        return ruleapp


# --- the CanBeSeenAs multimethod -------------------------------------------------------------

CAN_BE_SEEN_AS = Multimethod("CanBeSeenAs")


def can_be_seen_as(a, b):
    """Perl: CanBeSeenAs($a, $b), a Seqsee::ResultOfCanBeSeenAs."""
    return CAN_BE_SEEN_AS.call(a, b)


@CAN_BE_SEEN_AS.variant("#", "#")
def _cbsa_numbers(a, b):
    if util.perl_num(a) == util.perl_num(b):
        return _result().new_unblemished()
    return _result().NO()


@CAN_BE_SEEN_AS.variant("Seqsee::Object", "Seqsee::Object")
def _cbsa_objects(obj, structure):
    return CAN_BE_SEEN_AS.call(obj, structure.get_structure())


@CAN_BE_SEEN_AS.variant("Seqsee::Object", "#")
def _cbsa_object_number(obj, n):
    lit_or_meto = obj.can_be_seen_as_literal_or_meto(n)
    if lit_or_meto is not None:
        return lit_or_meto
    return _result().NO()


@CAN_BE_SEEN_AS.variant("Seqsee::Element", "#")
def _cbsa_element_number(elt, n):
    if util.perl_num(elt.get_mag()) == util.perl_num(n):
        return _result().new_unblemished()
    return _result().NO()


_INTEGER = re.compile(r"-?\d+\n?")


@CAN_BE_SEEN_AS.variant("Seqsee::Element", "$")
def _cbsa_element_string(elt, s):
    if s is not None and _INTEGER.fullmatch(util.perl_str(s)):
        if util.perl_num(elt.get_mag()) == util.perl_num(s):
            return _result().new_unblemished()
        return _result().NO()
    raise Confess(f"SAW CanBeSeenAs(Seqsee::Element, $): {util.perl_str(elt.as_text())} "
                  f"'{util.perl_str(s)}")


@CAN_BE_SEEN_AS.variant("Seqsee::Object", "ARRAY")
def _cbsa_object_array(obj, structure):
    meto_activeness = obj.get_metonym_activeness()
    metonym = obj.get_metonym()
    starred = metonym.get_starred() if util.perl_true(metonym) else None
    if util.perl_true(meto_activeness):
        meto_seen_as = obj.can_be_seen_as_meto(structure, starred, metonym)
        if meto_seen_as is not None:
            return meto_seen_as
    part_seen_as = obj.can_be_seen_as_by_part(structure)
    if part_seen_as is not None:
        return part_seen_as
    if util.perl_true(metonym):
        meto_seen_as = obj.can_be_seen_as_meto(structure, starred, metonym)
        if meto_seen_as is not None:
            return meto_seen_as
    return _result().NO()
