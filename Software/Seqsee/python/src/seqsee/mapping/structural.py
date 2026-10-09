"""Port of Mapping/Structural.pm (``Mapping::Structural``): how one instance of a category
maps to another. It records which attributes changed and how (``changed_bindings``,
new attribute → mapping), which attributes slipped (``slippages``, new attribute → old
attribute), and the metonymy, position and direction relations.

A Moose class that extends Mapping. ``category``, ``meto_mode``, ``position_reln``,
``metonymy_reln`` and ``direction_reln`` are required but unchecked (undef is fine).
``slippages`` and ``changed_bindings`` are HashRefs that default to {}. ``create``
memoizes on a '#'-join of the fields and the two sorted hashes, and the objects in it are
keyed by identity.

PERL-QUIRKs (oracle-confirmed):
- IsEffectivelyASamenessRelation returns from inside ``each`` loops over the object's own
  slippages and changed_bindings, so the next ``each`` on those hashes resumes part-way
  (``util.perl_each``). This includes as_text's loop over changed_bindings, which can come
  out empty. ``values``, ``keys`` and copies reset the iterator.
- FlippedVersion is memoized per object, including None results, so it goes stale after
  the setters change the object.
"""
from seqsee import sltm, util
from seqsee.errors import Confess
from seqsee.mapping import Mapping
from seqsee.mapping.meto_type import _call, _perl_string
from seqsee.mapping.numeric import _method_target
from seqsee.multimethods import perl_isa

# Perl: `state %MEMO` in create. reset() is the test hook.
_MEMO = {}
_REQUIRED = ("category", "meto_mode", "position_reln", "metonymy_reln", "direction_reln")
_UNSET = object()


def reset():
    """Clear the create memo (Perl keeps it for the whole process)."""
    _MEMO.clear()


def _moose_value(value):
    if value is None:
        return "undef"
    if isinstance(value, str):
        return f'"{value}"'
    if util._is_scalar(value):
        return util.perl_str(value)
    return util.perl_ref_string(value)


def _check_hashref(attr, value, where):
    """Moose ``isa => 'HashRef'``."""
    if not isinstance(value, dict):
        raise Confess(f"Attribute ({attr}) does not pass the type constraint because: "
                      f"Validation failed for 'HashRef' with value {_moose_value(value)} at {where}")
    return value


def _autoviv_hash(opts, key):
    """Perl ``%{ $opts->{key} }`` as a sub argument: undef (or a missing key) autovivifies to
    {} in the caller's hash; a string or another ref dies."""
    value = opts.get(key)
    if value is None:
        value = opts[key] = {}
    if isinstance(value, dict):
        util.perl_hash_reset(value)
        return value
    if util._is_scalar(value):
        raise Confess(f'Can\'t use string ("{util.perl_str(value)}") as a HASH ref '
                      'while "strict refs" in use')
    raise Confess("Not a HASH reference")


def _copy(d):
    """Perl ``%copy = %$ref``: a copy, and flattening resets the original's iterator."""
    util.perl_hash_reset(d)
    return dict(d)


def _is_ref(x):
    return x is not None and util.perl_ref(x) != ""


class MappingStructural(Mapping):
    """Perl: Mapping::Structural. ``MappingStructural({...})`` or kwargs is Perl ``new`` (not
    memoized); ``MappingStructural.create({...})`` is the memoized form."""

    perl_name = "Mapping::Structural"

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess("Mapping::Structural: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        where = "constructor Mapping::Structural::new"
        for attr in _REQUIRED:
            if attr not in kwargs:
                raise Confess(f"Attribute ({attr}) is required at {where}")
        self._slippages = (_check_hashref("slippages", kwargs["slippages"], where)
                           if "slippages" in kwargs else {})
        self._changed_bindings = (
            _check_hashref("changed_bindings", kwargs["changed_bindings"], where)
            if "changed_bindings" in kwargs else {})
        self._category = kwargs["category"]
        self._meto_mode = kwargs["meto_mode"]
        self._position_reln = kwargs["position_reln"]
        self._metonymy_reln = kwargs["metonymy_reln"]
        self._direction_reln = kwargs["direction_reln"]
        self._flip_memo = _UNSET

    def get_category(self):
        return self._category

    def set_category(self, category):
        self._category = category

    def get_meto_mode(self):
        return self._meto_mode

    def set_meto_mode(self, meto_mode):
        self._meto_mode = meto_mode

    def get_position_reln(self):
        return self._position_reln

    def set_position_reln(self, position_reln):
        self._position_reln = position_reln

    def get_metonymy_reln(self):
        return self._metonymy_reln

    def set_metonymy_reln(self, metonymy_reln):
        self._metonymy_reln = metonymy_reln

    def get_direction_reln(self):
        return self._direction_reln

    def set_direction_reln(self, direction_reln):
        self._direction_reln = direction_reln

    def get_slippages(self):
        return self._slippages

    def get_changed_bindings(self):
        return self._changed_bindings

    @classmethod
    def create(cls, opts_ref):
        """Perl: create(\\%opts), memoized on a '#'-join of category, meto_mode,
        metonymy_reln, position_reln, direction_reln and the two hashes (sorted by key).

        Like Perl, it writes into ``opts_ref``: metonymy_reln/position_reln become 'x' when
        the meto mode makes them irrelevant, and missing/undef hashes become {}.
        PERL-QUIRK: a plain join, so undef and "" fields collide (oracle-confirmed).
        """
        meto_mode = opts_ref.get("meto_mode")
        if not util.perl_true(meto_mode):
            raise Confess("need meto_mode")
        if not util.perl_true(_call(meto_mode, "is_metonymy_present", "is_metonymy_present")):
            opts_ref["metonymy_reln"] = "x"
        if not util.perl_true(_call(meto_mode, "is_position_relevant", "is_position_relevant")):
            opts_ref["position_reln"] = "x"
        parts = [_perl_string(opts_ref.get(k)) for k in
                 ("category", "meto_mode", "metonymy_reln", "position_reln", "direction_reln")]
        for key in ("changed_bindings", "slippages"):
            flat = util.hash_sorted_as_array(_autoviv_hash(opts_ref, key))
            parts.append(";".join(_perl_string(x) for x in flat))
        string = "#".join(parts)
        if string not in _MEMO:   # Perl `||=`; objects are always true
            _MEMO[string] = cls(opts_ref)
        return _MEMO[string]

    def flipped_version(self):
        """Perl: FlippedVersion: the reverse mapping, or None when a slippage target repeats
        or a binding's mapping can't be flipped. Memoized per object (None included)."""
        if self._flip_memo is _UNSET:
            self._flip_memo = self._flipped_version()
        return self._flip_memo

    def _flipped_version(self):
        new_slippages = _flip_slippages(self.get_slippages())
        if new_slippages is None:
            return None
        new_bindings_change = _flip_changed_bindings(self.get_changed_bindings(),
                                                     self.get_slippages())
        if new_bindings_change is None:
            return None
        flipped_relns = {}
        for attr in ("position_reln", "metonymy_reln", "direction_reln"):
            reln = getattr(self, "get_" + attr)()
            flipped_relns[attr] = reln.flipped_version() if _is_ref(reln) else None
        flipped = MappingStructural.create({
            "category": self.get_category(),
            "meto_mode": self.get_meto_mode(),
            **flipped_relns,
            "changed_bindings": new_bindings_change,
            "slippages": new_slippages,
        })
        if not util.perl_true(flipped.check_sanity()):
            from seqsee import mapping
            util.perl_hash_reset(new_bindings_change)
            flat = [_perl_string(x) for pair in new_bindings_change.items() for x in pair]
            mapping._message("Flip problematic!" + ";".join(flat))
        return flipped

    def get_pure(self):
        return self

    def is_effectively_a_sameness_relation(self):
        """Perl: IsEffectivelyASamenessRelation: 1, or None (Perl's bare return).
        PERL-QUIRK: the early returns leave the each-iterators part-way (see module doc)."""
        for k, v in util.perl_each(self.get_slippages()):
            if util.perl_str(k) != util.perl_str(v):
                return None
        for _k, v in util.perl_each(self.get_changed_bindings()):
            if not util.perl_true(_call(v, "IsEffectivelyASamenessRelation",
                                        "is_effectively_a_sameness_relation")):
                return None
        meto_mode = self.get_meto_mode()
        if util.perl_true(_call(meto_mode, "is_metonymy_present", "is_metonymy_present")):
            for reln in (self.get_metonymy_reln(), self.get_direction_reln()):
                if not util.perl_true(_call(reln, "IsEffectivelyASamenessRelation",
                                            "is_effectively_a_sameness_relation")):
                    return None
            if util.perl_true(_call(meto_mode, "is_position_relevant", "is_position_relevant")):
                if not util.perl_true(_call(self.get_position_reln(),
                                            "IsEffectivelyASamenessRelation",
                                            "is_effectively_a_sameness_relation")):
                    return None
        return 1

    def get_memory_dependencies(self):
        """The fields that are refs, then the changed bindings' values (Perl: hash order)."""
        bindings = self.get_changed_bindings()
        util.perl_hash_reset(bindings)
        items = [self.get_category(), self.get_meto_mode(), self.get_position_reln(),
                 self.get_metonymy_reln(), self.get_direction_reln(), *bindings.values()]
        return [x for x in items if _is_ref(x)]

    def as_text(self):
        """Perl: as_text, e.g. "[ascending] start => succ" or
        "[ascending] (start => succ (of end))". The parts come in Perl hash order (here,
        insertion order); a '*' after the name marks metonymy."""
        cat_name = util.perl_str(_call(self.get_category(), "get_name", "get_name"))
        changed_bindings = self.get_changed_bindings()
        string = None
        presence = ("*" if util.perl_true(_call(self.get_meto_mode(), "is_metonymy_present",
                                                 "is_metonymy_present")) else "")
        slippages = _copy(self.get_slippages())
        if slippages:
            for new, old in slippages.items():
                reln = changed_bindings.get(new)
                if util.perl_true(reln):
                    string = (string or "") + (
                        f"({new} => " + util.perl_str(_call(reln, "as_text", "as_text"))
                        + f" (of {util.perl_str(old)}));")
                elif util.perl_str(old) != util.perl_str(new):
                    string = (string or "") + f"new {new} is the earlier {util.perl_str(old)};"
        else:
            for k, v in util.perl_each(changed_bindings):
                string = (string or "") + f"{k} => " + util.perl_str(
                    _call(v, "as_text", "as_text")) + ";"
        string = (string or "")[:-1]   # Perl chop (undef stays empty)
        return f"[{cat_name}{presence}] {string}"

    def serialize(self):
        return sltm.encode(self.get_category(), self.get_meto_mode(), self.get_metonymy_reln(),
                           self.get_direction_reln(), self.get_position_reln(),
                           self.get_changed_bindings(), self.get_slippages())

    @classmethod
    def deserialize(cls, string):
        """Perl: create with the fields from SLTM::decode; missing values are undef."""
        decoded = sltm.decode(string) + [None] * 7
        keys = ("category", "meto_mode", "metonymy_reln", "direction_reln", "position_reln",
                "changed_bindings", "slippages")
        return cls.create(dict(zip(keys, decoded)))

    def get_complexity(self):
        """Perl: get_complexity: 0.1 for Ascending/Descending/Sameness, 0.1 per part for
        Interlaced, else 0.3; +0.2 with metonymy; plus each binding's complexity; +0.2 per
        real slippage; capped at 0.9."""
        from seqsee import s as S
        from seqsee.constants import METO_MODE
        cat = self.get_category()
        if any(cat is c for c in (S.ASCENDING, S.DESCENDING, S.SAMENESS)):
            complexity = 0.1
        elif _isa(cat, "SCategory::Interlaced"):
            complexity = 0.1 * util.perl_num(cat.get_parts_count())
        else:
            complexity = 0.3
        if self.get_meto_mode() is not METO_MODE.NONE:
            complexity += 0.2
        bindings = self.get_changed_bindings()
        util.perl_hash_reset(bindings)
        for v in list(bindings.values()):
            complexity += util.perl_num(_call(v, "get_complexity", "get_complexity"))
        for k, v in _copy(self.get_slippages()).items():
            if util.perl_str(k) != util.perl_str(v):
                complexity += 0.2
        if complexity > 0.9:
            complexity = 0.9
        return complexity


def _isa(x, name):
    """Perl ``$x->isa($name)``: a plain string is a class-method call (false here); undef
    and "" die."""
    if x is None:
        _method_target(x, "isa")
    if isinstance(x, str) and x == "":
        raise Confess('Can\'t call method "isa" without a package or object reference')
    if util._is_scalar(x):
        return False
    return perl_isa(x, name)


def _flip_slippages(old_slippages):
    """Perl: _FlipSlippages: new → old becomes old → new; None if an old attribute repeats."""
    new_slippages = {}
    seen = set()
    for k, v in _copy(old_slippages).items():
        key = util.perl_str(v)
        if key in seen:
            return None
        seen.add(key)
        new_slippages[key] = k
    return new_slippages


def _flip_changed_bindings(old_bindings, slippages):
    """Perl: _FlipChangedBindings: each mapping flipped, keyed by the slipped attribute."""
    new_bindings = {}
    for k, v in _copy(old_bindings).items():
        new_v = _call(v, "FlippedVersion", "flipped_version")
        if new_v is None:
            return None
        new_k = slippages[k] if k in slippages else k
        new_bindings[util.perl_str(new_k)] = new_v
    return new_bindings
