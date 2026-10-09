"""Port of Mapping/MetoType.pm (``Mapping::MetoType``): how the info lost by a metonym
changes, key by key (``change_ref``: key → mapping), within one category and name.

A Moose class with ``category`` (required, unchecked), ``name`` (optional ``Str``) and
``change_ref`` (required, unchecked). In Perl it does NOT extend Mapping. ``create``
memoizes on ``join(';', category, name, %change_ref)``. Also registers
FindMapping(SMetonymType, SMetonymType) and ApplyMapping(Mapping::MetoType, SMetonymType).

PERL-QUIRK (oracle-confirmed): FlippedVersion, IsEffectivelyASamenessRelation,
FindMapping and ApplyMapping walk their hashes with ``each``. An early return leaves the
hash's iterator part-way, and the next ``each`` on that hash resumes there and sees only
the remaining keys. This is emulated with ``util.perl_each``; ``keys``/``values``/list
flattening reset it (``util.perl_hash_reset``).
"""
from seqsee import sltm, util
from seqsee.errors import Confess
from seqsee.mapping import APPLY_MAPPING, FIND_MAPPING, apply_mapping, find_mapping
from seqsee.mapping.numeric import _method_target

# Perl: `state %MEMO` in create. reset() is the test hook.
_MEMO = {}


def reset():
    """Clear the create memo (Perl keeps it for the whole process)."""
    _MEMO.clear()


def _check_str(value, where):
    """Moose ``isa => 'Str'``: a defined non-ref scalar."""
    if isinstance(value, bool) or not isinstance(value, (str, int, float)):
        if value is None:
            shown = "undef"
        elif isinstance(value, list) and not value:
            shown = "[  ]"
        else:
            shown = repr(value)
        raise Confess("Attribute (name) does not pass the type constraint because: "
                      f"Validation failed for 'Str' with value {shown} at {where}")
    return value


def _perl_string(x):
    """Perl's string form of x: undef is "", SInt uses its "" overload (as_text), other
    refs are their address string (so comparing these is identity)."""
    if x is None or util._is_scalar(x):
        return util.perl_str(x)
    if util.perl_ref(x) == "SInt":
        return x.as_text()
    return util.perl_ref_string(x)


def _hash_items(change_ref):
    """Perl ``%{ $change_ref }`` in create's join (an rvalue: undef dies)."""
    if isinstance(change_ref, dict):
        util.perl_hash_reset(change_ref)
        return list(change_ref.items())
    if change_ref is None:
        raise Confess("Can't use an undefined value as a HASH reference")
    if util.perl_ref(change_ref) == "":
        raise Confess(f'Can\'t use string ("{util.perl_str(change_ref)}") as a HASH ref '
                      'while "strict refs" in use')
    raise Confess("Not a HASH reference")


def _call(obj, perl_method, method):
    """``$obj->perl_method``, with Perl's errors for undef, strings and missing methods."""
    _method_target(obj, perl_method)
    if not hasattr(obj, method):
        pkg = util.perl_ref(obj)
        raise Confess(f'Can\'t locate object method "{perl_method}" via package "{pkg}"')
    return getattr(obj, method)()


class MappingMetoType:
    """Perl: Mapping::MetoType. ``MappingMetoType({...})`` or kwargs is Perl ``new`` (not
    memoized); ``MappingMetoType.create({...})`` is the memoized form."""

    perl_name = "Mapping::MetoType"

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess("Mapping::MetoType: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        where = "constructor Mapping::MetoType::new"
        for attr in ("category", "change_ref"):
            if attr not in kwargs:
                raise Confess(f"Attribute ({attr}) is required at {where}")
        self._category = kwargs["category"]
        self._name = _check_str(kwargs["name"], where) if "name" in kwargs else None
        self._change_ref = kwargs["change_ref"]

    def get_category(self):
        return self._category

    def set_category(self, category):
        self._category = category

    def get_name(self):
        return self._name

    def set_name(self, name):
        self._name = _check_str(name, "writer Mapping::MetoType::set_name")

    def get_change_ref(self):
        return self._change_ref

    def set_change_ref(self, change_ref):
        self._change_ref = change_ref

    @classmethod
    def create(cls, opts_ref=None, **kwargs):
        """Perl: create(\\%opts), memoized on ``join(';', category, name, %change_ref)``.

        PERL-QUIRK: a plain join, so undef and "" categories collide, and so do
        (name "x;a;q", {}) and (name "x", {a => "q"}). Objects key by address (identity),
        SInts by their "SInt(n)" text. Perl joins the pairs in hash order; the port sorts
        them by key (oracle: {a, b} and {b, a} give the same object).
        """
        opts = {**(opts_ref or {}), **kwargs}
        pairs = sorted(((_perl_string(k), _perl_string(v))
                        for k, v in _hash_items(opts.get("change_ref"))), key=lambda kv: kv[0])
        parts = [_perl_string(opts.get("category")), _perl_string(opts.get("name"))]
        for k, v in pairs:
            parts += [k, v]
        key = ";".join(parts)
        if key not in _MEMO:   # Perl `||=`; objects are always true
            _MEMO[key] = cls(opts)
        return _MEMO[key]

    def flipped_version(self):
        """Perl: FlippedVersion: each change flipped, and the name toggles a "flipped_"
        prefix."""
        new_change = {}
        for k, v in util.perl_each(self.get_change_ref()):
            new_change[k] = _call(v, "FlippedVersion", "flipped_version")
        name = util.perl_str(self.get_name())
        new_name = name[8:] if name.startswith("flipped_") else "flipped_" + name
        return MappingMetoType.create({"category": self.get_category(), "name": new_name,
                                       "change_ref": new_change})

    def get_memory_dependencies(self):
        """The category and the change values that are refs (SInts included; Perl: hash
        order of the values, here dict order)."""
        change_ref = self.get_change_ref()
        util.perl_hash_reset(change_ref)
        items = [self.get_category(), *change_ref.values()]
        return [x for x in items if util.perl_ref(x) != "" and x is not None]

    def serialize(self):
        util.perl_hash_reset(self.get_change_ref())
        return sltm.encode(self.get_category(), self.get_name(), self.get_change_ref())

    @classmethod
    def deserialize(cls, string):
        """Perl: create with (category, name, change_ref) from SLTM::decode; missing
        values are undef (so a missing change_ref dies in create)."""
        decoded = sltm.decode(string) + [None, None, None]
        return cls.create({"category": decoded[0], "name": decoded[1],
                           "change_ref": decoded[2]})

    def as_text(self):
        util.perl_hash_reset(self.get_change_ref())
        change = util.perl_str(util.stringify_for_carp(self.get_change_ref()))
        category = util.perl_str(util.stringify_for_carp(self.get_category()))
        return f"Mapping::MetoType(change=>{change}, category=>{category})"

    def get_pure(self):
        return self

    def is_effectively_a_sameness_relation(self):
        """Perl: IsEffectivelyASamenessRelation: 1, or None (Perl's bare return).
        PERL-QUIRK: the early return leaves the each-iterator part-way (see module doc)."""
        for _k, v in util.perl_each(self.get_change_ref()):
            if not util.perl_true(_call(v, "IsEffectivelyASamenessRelation",
                                        "is_effectively_a_sameness_relation")):
                return None
        return 1


@FIND_MAPPING.variant("SMetonymType", "SMetonymType")
def _find_meto_types(m1, m2):
    cat1 = m1.get_category()
    if _perl_string(m2.get_category()) != _perl_string(cat1):
        return None
    name1 = m1.get_name()
    if _perl_string(m2.get_name()) != _perl_string(name1):
        return None
    info_loss1 = m1.get_info_loss()
    info_loss2 = m2.get_info_loss()
    if len(util.perl_keys(info_loss1)) != len(util.perl_keys(info_loss2)):
        return None
    change_ref = {}
    for k, v in util.perl_each(info_loss1):
        if k not in info_loss2:
            return None
        rel = find_mapping(v, info_loss2[k])
        if not util.perl_true(rel):
            return None
        change_ref[k] = rel
    return MappingMetoType.create({"category": cat1, "name": name1, "change_ref": change_ref})


@APPLY_MAPPING.variant("Mapping::MetoType", "SMetonymType")
def _apply_meto_type(rel, meto):
    """Returns a ``new`` SMetonymType (not memoized), with the metonym's category and name."""
    from seqsee.smetonym_type import SMetonymType
    rel_change_ref = rel.get_change_ref() or {}
    new_loss = {}
    for k, v in util.perl_each(meto.get_info_loss()):
        if k not in rel_change_ref:
            new_loss[k] = v
            continue
        new_loss[k] = apply_mapping(rel_change_ref[k], v)
    return SMetonymType({"info_loss": new_loss, "name": meto.get_name(),
                         "category": meto.get_category()})
