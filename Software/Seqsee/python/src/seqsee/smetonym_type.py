"""Port of lib/SMetonymType.pm: enough of a metonym to tell whether two metonyms are
essentially the same (category, name, and the bindings lost going from unstarred
to starred).

SLTM::encode/decode go through ``_sltm_encode``/``_sltm_decode``.
"""
from seqsee import util
from seqsee.errors import Confess

# Perl: the `my %METO` closure of create. Keyed by the joined key string; never cleared.
_METO = {}


def _sltm_encode(*objects):
    """Perl: SLTM::encode(@objects)."""
    from seqsee import sltm
    return sltm.encode(*objects)


def _sltm_decode(string):
    """Perl: SLTM::decode($string): the list of objects."""
    from seqsee import sltm
    return sltm.decode(string)


def _opts(opts_ref, kwargs):
    opts = dict(opts_ref or {})
    opts.update(kwargs)
    return opts


def _key_part(x):
    """One item of create's key: an SInt's magnitude (Perl ``$_->[0]``), a scalar's
    string form, or the ref string of anything else (Perl: the address)."""
    ref = util.perl_ref(x)
    if ref == "SInt":
        return util.perl_str(x.get_mag())
    if ref == "":
        return util.perl_str(x)
    return util.perl_ref_string(x)


def _hash_items(info_loss):
    """Perl ``%{ $opts{info_loss} }`` in create: undef autovivifies to {}."""
    if info_loss is None:
        return []
    if isinstance(info_loss, dict):
        return list(info_loss.items())
    if util.perl_ref(info_loss) == "":
        raise Confess(f'Can\'t use string ("{util.perl_str(info_loss)}") as a HASH ref '
                      'while "strict refs" in use')
    raise Confess("Not a HASH reference")


class SMetonymType:
    """Perl: SMetonymType (Class::Std).

    ``SMetonymType({...})`` or ``SMetonymType(category=, name=, info_loss=)``; other
    keys are ignored. BUILD's ``||`` checks use Perl truth, so a name of "0" dies and an
    empty info_loss hash ({} is a true ref) is fine."""

    perl_name = "SMetonymType"

    def __init__(self, opts_ref=None, **kwargs):
        opts = _opts(opts_ref, kwargs)
        self._category = opts.get("category")
        if not util.perl_true(self._category):
            raise Confess("Need category")
        self._name = opts.get("name")
        if not util.perl_true(self._name):
            raise Confess("Need name")
        info_loss = opts.get("info_loss")
        # A Perl ref is always true; Python's empty dict/list is not.
        if not (isinstance(info_loss, (dict, list)) or util.perl_true(info_loss)):
            raise Confess("Need info_loss")
        self._info_loss = info_loss

    @classmethod
    def create(cls, opts_ref=None, **kwargs):
        """Perl: SMetonymType->create(\\%opts), memoized by
        ``join(';', category, name, %info_loss)``.

        PERL-QUIRK: the key is a plain join, so e.g. (name "a", {b => "c"}) and
        (name "a;b;c", {}) collide and give the same type. SInts key by magnitude, so
        SInt(2), 2, 2.0 and "2" collide too (but not "2.0").
        Perl joins the pairs in hash order; the port sorts them by key, so equal hashes
        give equal keys whatever their insertion order (oracle-confirmed for 2 keys).
        """
        opts = _opts(opts_ref, kwargs)
        pairs = sorted(((_key_part(k), _key_part(v)) for k, v in _hash_items(opts.get("info_loss"))),
                       key=lambda kv: kv[0])
        parts = [_key_part(opts.get("category")), _key_part(opts.get("name"))]
        for k, v in pairs:
            parts += [k, v]
        key = ";".join(parts)
        if key not in _METO:
            _METO[key] = cls(opts)
        return _METO[key]

    def get_category(self):
        return self._category

    def get_name(self):
        return self._name

    def get_info_loss(self):
        return self._info_loss

    def blemish(self, obj):
        """Perl: blemish($object). Runs the category's unfinder, then describe_as."""
        cat, name, info_loss = self._category, self._name, self._info_loss
        unfinder = cat.get_meto_unfinder(name)
        if unfinder is None:
            raise Confess("Not a CODE reference")
        ret = unfinder(cat, name, info_loss, obj)
        ret.describe_as(cat)
        return ret

    def as_text(self):
        """PERL-QUIRK: as_text is defined twice; the second ("SMetonymType") wins over
        "Metotype: $self"."""
        return "SMetonymType"

    def get_cat_and_name(self):
        """Perl: GetCatAndName."""
        return (self._category, self._name)

    def get_memory_dependencies(self):
        """The pures of the category and the info_loss values that are refs but not SInts
        (values in dict order; Perl: hash order)."""
        items = [self._category, *self._info_loss.values()]
        return [x.get_pure() for x in items if util.perl_ref(x) not in ("", "SInt")]

    def serialize(self):
        return _sltm_encode(self._category, self._name, self._info_loss)

    @classmethod
    def deserialize(cls, string):
        decoded = list(_sltm_decode(string))[:3]
        decoded += [None] * (3 - len(decoded))
        category, name, info_loss = decoded
        return SMetonymType.create({"category": category, "name": name, "info_loss": info_loss})

    def get_pure(self):
        return self
