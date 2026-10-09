"""Port of Mapping/Numeric.pm (``Mapping::Numeric``): a named step (same/succ/pred, or
flip/no_flip for Alternating) within one category.

A Moose class that extends Mapping, with two required attributes: ``name`` (a ``Str``)
and ``category`` (anything). ``create`` memoizes on ``SLTM::encode($name, $category)``,
through the partial SLTM port in ``seqsee.sltm``.

PERL-QUIRK (oracle-confirmed): a category that isn't in the LTM encodes as an empty
index, so until categories are inserted, "succ" for Even, Odd, Prime and Number all
share one memo entry, and the first category created wins.
"""
from seqsee import sltm, util
from seqsee.errors import Confess
from seqsee.mapping import Mapping
from seqsee.multimethods import perl_isa

# Perl: `state %MEMO` in create. reset() is the test hook.
_MEMO = {}
_FLIP_NAME = {"same": "same", "pred": "succ", "succ": "pred", "flip": "flip",
              "no_flip": "no_flip"}


def reset():
    """Clear the create memo (Perl keeps it for the whole process)."""
    _MEMO.clear()


def _check_str(value, where):
    """Moose ``isa => 'Str'``: a defined non-ref scalar."""
    if isinstance(value, bool) or not isinstance(value, (str, int, float)):
        shown = "undef" if value is None else repr(value)
        raise Confess("Attribute (name) does not pass the type constraint because: "
                      f"Validation failed for 'Str' with value {shown} at {where}")
    return value


def _method_target(obj, method):
    """Perl's errors for calling ``method`` on undef or on a plain string."""
    if obj is None:
        raise Confess(f'Can\'t call method "{method}" on an undefined value')
    if util._is_scalar(obj):
        pkg = util.perl_str(obj)
        raise Confess(f'Can\'t locate object method "{method}" via package "{pkg}" '
                      f'(perhaps you forgot to load "{pkg}"?)')
    return obj


class MappingNumeric(Mapping):
    """Perl: Mapping::Numeric. ``MappingNumeric({"name": n, "category": c})`` or
    ``MappingNumeric(name=n, category=c)`` is Perl ``new`` (not memoized);
    ``MappingNumeric.create(n, c)`` is the memoized form."""

    perl_name = "Mapping::Numeric"

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess("Mapping::Numeric: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        where = "constructor Mapping::Numeric::new"
        for attr in ("name", "category"):
            if attr not in kwargs:
                raise Confess(f"Attribute ({attr}) is required at {where}")
        self._name = _check_str(kwargs["name"], where)
        self._category = kwargs["category"]
        self._as_text_memo = self._complexity_memo = None

    def get_name(self):
        return self._name

    def set_name(self, name):
        self._name = _check_str(name, "writer Mapping::Numeric::set_name")

    def get_category(self):
        return self._category

    def set_category(self, category):
        self._category = category

    @classmethod
    def create(cls, name, category):
        """Perl: create($name, $category), memoized on SLTM::encode($name, $category)."""
        if not util.perl_true(name):
            raise Confess("Mapping::Numeric creation attempted without name!")
        key = sltm.encode(name, category)
        if _MEMO.get(key) is None:
            _MEMO[key] = cls(name=name, category=category)
        return _MEMO[key]

    def serialize(self):
        return sltm.encode(self.get_name(), self.get_category())

    @classmethod
    def deserialize(cls, string):
        """Perl: ``create(SLTM::decode($str))``; missing values are undef."""
        decoded = sltm.decode(string) + [None, None]
        return cls.create(decoded[0], decoded[1])

    def get_memory_dependencies(self):
        return [self.get_category()]

    def get_pure(self):
        return self

    def flipped_version(self):
        """Perl: FlippedVersion. An unknown name flips to create(undef), which dies."""
        return MappingNumeric.create(_FLIP_NAME.get(util.perl_str(self.get_name())),
                                     self.get_category())

    def is_effectively_a_sameness_relation(self):
        """Perl: IsEffectivelyASamenessRelation (1/0)."""
        return 1 if util.perl_str(self.get_name()) == "same" else 0

    def as_text(self):
        """Perl: as_text ("same", "even succ", ...). PERL-QUIRK: memoized per object, so it
        stays stale after set_name/set_category (oracle-confirmed)."""
        if self._as_text_memo is None:
            from seqsee import s as S
            cat = self.get_category()
            cat_string = "" if cat is S.NUMBER else _method_target(cat, "as_text").as_text() + " "
            self._as_text_memo = cat_string + util.perl_str(self.get_name())
        return self._as_text_memo

    def get_relation_based_category(self):
        """Perl: GetRelationBasedCategory: Ascending/Sameness/Descending for Number,
        else SCategory::MappingBased->Create($self)."""
        from seqsee import s as S
        if self.get_category() is not S.NUMBER:
            from seqsee.categories.mapping_based import MappingBased
            return MappingBased.create(self)
        name = util.perl_str(self.get_name())
        if name == "succ":
            return S.ASCENDING
        if name == "same":
            return S.SAMENESS
        if name == "pred":
            return S.DESCENDING
        raise Confess("Should not reach herre")

    def get_complexity(self):
        """Perl: get_complexity: 0 (Number same), 0.1 (other Number), 0.7 (Alternating),
        else 0.4. PERL-QUIRK: memoized per object, like as_text."""
        if self._complexity_memo is None:
            from seqsee import s as S
            cat = self.get_category()
            if cat is S.NUMBER:
                value = 0 if util.perl_str(self.get_name()) == "same" else 0.1
            elif cat is not None and util._is_scalar(cat):
                value = 0.4   # Perl: "x"->isa(...) is a class-method call, false here
            elif perl_isa(_method_target(cat, "isa"), "SCategory::Alternating"):
                value = 0.7
            else:
                value = 0.4
            self._complexity_memo = value
        return self._complexity_memo
