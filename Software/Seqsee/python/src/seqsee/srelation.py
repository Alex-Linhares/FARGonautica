"""Port of SRelation.pm (``SRelation``) and SRelation/Structural.pm (``SRelation::Structural``):
a relation (a Mapping, its ``type``) between two workspace objects.

Moose classes. ``SRelation({...})`` or kwargs is Moose ``new``: one pass over the
attributes in name order (direction_reln, first, history_object, holeyness, second,
strength, type, unchanged_bindings), each with its required check and then its type check
(``first``/``second``: Seqsee::Object, ``type``: Mapping, ``history_object``: SHistory,
``holeyness``: Bool, ``unchanged_bindings``: HashRef). BUILD then sets holeyness from
SWorkspace->are_there_holes_here and a fresh SHistory, overriding any values passed in.

Naming: ``SuggestCategory`` → ``suggest_category``, ``SuggestCategoryForEnds`` →
``suggest_category_for_ends``, ``UpdateStrength`` → ``update_strength``,
``FlippedVersion`` → ``flipped_version``; the SHistory delegations are snake_case as on
Seqsee::Object (``AddHistory`` → ``add_history``, ``UnchangedSince`` → ``unchanged_since``,
``GetAge`` → ``get_age``). The rw ``history_object`` accessor is ``history_object(*value)``.

Hooks, looked up at call time (tests monkeypatch them):
``_are_there_holes_here`` (SWorkspace->are_there_holes_here), ``_workspace_add_relation`` / ``_workspace_remove_relation``
(SWorkspace->AddRelation/RemoveRelation) and
``_get_real_activations_for_one_concept`` (SLTM).

PERL-QUIRKs (oracle-confirmed):
- SuggestCategory returns false ("" — Python False) for a NUMBER mapping not named
  same/succ/pred: the if/elsif chain falls off the end.
- UpdateStrength caps at 100 but has no floor; an undef or "" activation counts as 0.
- insert still runs UpdateStrength when the workspace refuses the relation.
- get_span of a leftward relation is right edge of second minus left edge of first, + 1,
  so it can be 0 or negative.
- ``set_history`` is a delegation to a method SHistory doesn't have, so it always dies.
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.multimethods import perl_isa
from seqsee.shistory import SHistory


def _are_there_holes_here(*items):
    """Perl: SWorkspace->are_there_holes_here(@items): 1 if the items' edges leave a gap."""
    from seqsee import sworkspace
    return sworkspace.are_there_holes_here(*items)


def _workspace_add_relation(reln):
    """Perl: SWorkspace->AddRelation($reln)."""
    from seqsee import sworkspace
    return sworkspace.add_relation(reln)


def _workspace_remove_relation(reln):
    """Perl: SWorkspace->RemoveRelation($reln)."""
    from seqsee import sworkspace
    return sworkspace.remove_relation(reln)


def _get_real_activations_for_one_concept(concept):
    """Perl: SLTM::GetRealActivationsForOneConcept($concept)."""
    from seqsee import sltm
    return sltm.get_real_activations_for_one_concept(concept)


def _check(attr, value):
    """The Moose ``isa`` checks of SRelation's typed attributes."""
    from seqsee.objects.object import _is_bool, _type_error
    if attr in ("first", "second"):
        if not perl_isa(value, "Seqsee::Object"):
            raise _type_error(attr, "Seqsee::Object", value)
    elif attr == "type":
        if not perl_isa(value, "Mapping"):
            raise _type_error(attr, "Mapping", value)
    elif attr == "history_object":
        if not isinstance(value, SHistory):
            raise _type_error(attr, "SHistory", value)
    elif attr == "holeyness":
        if not _is_bool(value):
            raise _type_error(attr, "Bool", value)
    elif attr == "unchanged_bindings":
        if not isinstance(value, dict):
            raise _type_error(attr, "HashRef", value)
    return value


class SRelation:
    """Perl: SRelation."""

    perl_name = "SRelation"

    # (attribute, required); visited in name order by the constructor.
    _ATTRS = (("direction_reln", False), ("first", True), ("history_object", False),
              ("holeyness", False), ("second", True), ("strength", False),
              ("type", True))

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess(f"{self.perl_name}: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        for attr, required in sorted(self._ATTRS):
            if attr in kwargs:
                _check(attr, kwargs[attr])
            elif required:
                raise Confess(f"Attribute ({attr}) is required")
        self._init_attributes(kwargs)
        self._build()

    def _init_attributes(self, kwargs):
        self._strength = kwargs.get("strength", 0)
        self._first = kwargs["first"]
        self._second = kwargs["second"]
        self._type = kwargs["type"]
        self._history_object = kwargs.get("history_object")
        self._direction_reln = kwargs.get("direction_reln")
        self._holeyness = kwargs.get("holeyness")

    def _build(self):
        """Perl: BUILD."""
        f, s = self.get_ends()
        self.set_holeyness(_are_there_holes_here(f, s))
        self.history_object(SHistory())

    # --- attributes ----------------------------------------------------------------------

    def get_strength(self):
        return self._strength

    def set_strength(self, value):
        self._strength = value
        return value

    def get_first(self):
        return self._first

    def set_first(self, value):
        self._first = _check("first", value)

    def get_second(self):
        return self._second

    def set_second(self, value):
        self._second = _check("second", value)

    def get_type(self):
        return self._type

    def set_type(self, value):
        self._type = _check("type", value)

    def history_object(self, *value):
        """Perl: the rw ``history_object`` accessor."""
        if value:
            self._history_object = _check("history_object", value[0])
        return self._history_object

    def get_direction_reln(self):
        return self._direction_reln

    def set_direction_reln(self, value):
        self._direction_reln = value

    def get_holeyness(self):
        return self._holeyness

    def set_holeyness(self, value):
        self._holeyness = _check("holeyness", value)

    # --- SHistory delegations ------------------------------------------------------------

    def set_history(self, *args):
        raise Confess('Can\'t locate object method "set_history" via package "SHistory" '
                      "at inline delegation in SRelation for history_object->set_history")

    def get_history(self):
        return self._history_object.get_history()

    def add_history(self, msg):
        return self._history_object.add_history(msg)

    def search_history(self, pattern):
        return self._history_object.search_history(pattern)

    def unchanged_since(self, since):
        return self._history_object.unchanged_since(since)

    def get_age(self):
        return self._history_object.get_age()

    def history_as_text(self):
        return self._history_object.history_as_text()

    # --- methods -------------------------------------------------------------------------

    def get_ends(self):
        return self.get_first(), self.get_second()

    def get_extent(self):
        return self.get_first().get_left_edge(), self.get_second().get_right_edge()

    def are_ends_contiguous(self):
        return 1 if util.perl_num(self.get_first().get_right_edge()) + 1 == \
            util.perl_num(self.get_second().get_left_edge()) else 0

    def insert(self):
        """Perl: insert: replace any relation between the ends, add to the workspace and,
        if it accepts, to both ends; then UpdateStrength (whose value is returned)."""
        f, s = self.get_ends()
        reln = f.get_relation(s)
        if util.perl_true(reln):
            reln.uninsert()
        try:
            add_success = _workspace_add_relation(self)
        except NotImplementedError:
            raise
        except Exception as e:   # Perl: Try::Tiny catch
            raise Confess(f"Relation insertion error: {e} ")
        if util.perl_true(add_success):
            for end in (f, s):
                end.add_relation(self)
        return self.update_strength()

    def uninsert(self):
        """Perl: uninsert. Returns Perl's false (the value of the final for loop)."""
        _workspace_remove_relation(self)
        for end in self.get_ends():
            end.remove_relation(self)
        return False

    def get_direction(self):
        from seqsee.constants import DIR
        la, lb = (util.perl_num(x.get_left_edge()) for x in self.get_ends())
        if la < lb:
            return DIR.RIGHT
        if lb < la:
            return DIR.LEFT
        return DIR.UNKNOWN

    def get_span(self):
        left, right = self.get_extent()
        return util.perl_num(right) - util.perl_num(left) + 1

    def get_pure(self):
        return self.get_type()

    def suggest_category(self):
        """Perl: SuggestCategory."""
        from seqsee import s as S
        category = self.get_type().get_category()
        if category is S.NUMBER:
            name = util.perl_str(self.get_type().get_name())
            if name == "same":
                return S.SAMENESS
            if name == "succ":
                return S.ASCENDING
            if name == "pred":
                return S.DESCENDING
            return False   # PERL-QUIRK: falls off the if/elsif chain
        from seqsee.categories.mapping_based import MappingBased
        return MappingBased.create(self.get_type())

    def suggest_category_for_ends(self):
        """Perl: SuggestCategoryForEnds: always the empty list."""
        return None

    def update_strength(self):
        """Perl: UpdateStrength: 20 × the type's activation, ×0.8 if holey, capped at 100."""
        strength = 20 * util.perl_num(_get_real_activations_for_one_concept(self.get_type()))
        if util.perl_true(self.get_holeyness()):
            strength *= 0.8
        if strength > 100:
            strength = 100
        return self.set_strength(strength)

    def as_text(self):
        first_location = util.perl_str(self.get_first().get_bounds_string())
        second_location = util.perl_str(self.get_second().get_bounds_string())
        return f"{first_location} --> {second_location}: " + \
            util.perl_str(self.get_type().as_text())

    def flipped_version(self):
        """Perl: FlippedVersion: a new SRelation (always the base class) with the ends
        swapped and the flipped type, or None if the type doesn't flip."""
        flipped_type = self.get_type().flipped_version()
        if flipped_type is None:
            return None
        return SRelation({"first": self.get_second(), "second": self.get_first(),
                          "type": flipped_type})


class SRelationStructural(SRelation):
    """Perl: SRelation::Structural."""

    perl_name = "SRelation::Structural"

    _ATTRS = SRelation._ATTRS + (("unchanged_bindings", False),)

    def _init_attributes(self, kwargs):
        super()._init_attributes(kwargs)
        self._unchanged_bindings = kwargs["unchanged_bindings"] \
            if "unchanged_bindings" in kwargs else {}

    def get_unchanged_bindings(self):
        return self._unchanged_bindings

    def set_unchanged_bindings(self, value):
        self._unchanged_bindings = _check("unchanged_bindings", value)

    def no_unchanged_bindings(self):
        """Perl: the Hash trait's ``is_empty`` (1/0)."""
        return 0 if self._unchanged_bindings else 1
