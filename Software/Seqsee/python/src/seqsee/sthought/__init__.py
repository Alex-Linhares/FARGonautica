"""Port of lib/SThought.pm: the base class of thoughts (what the stream focuses on).

``SThought.create(core)`` picks the thought class for a core:

- a category (Perl: ``isa('SCat::OfObj')`` or ``does('SCategory')``) → SThought::SCat
  (``seqsee.sthought.scat``);
- an object whose exact Perl class is Seqsee::Element, Seqsee::Anchored or SRelation →
  SThought::Seqsee::Element, SThought::Seqsee::Anchored (``seqsee.sthought.sobject``)
  or SThought::SRelation (``seqsee.sthought.relations``);
- anything else dies "Don't know how to think about …".

``create`` is memoized (Perl ``memoize('create')``) on the core's identity. Memoize keeps
separate caches for scalar and list context, so ``SThought->create($x)`` and
``ContinueWith(SThought->create($x))`` give different thoughts for the same core. The port
keeps both caches: pass ``list_context=True`` where the Perl call is in list context (the
codelets' ``ContinueWith(...)``). ``reset()`` clears them.

PERL-QUIRKs:
- ``schedule`` and ``force_to_be_next_runnable`` call ``SCoderack->schedule_thought`` and
  ``force_thought``, which don't exist, so they always die. Nothing calls them.
- A class-name string such as "SCategory::Ascending" passes Perl's class-method
  ``isa``/``does`` checks and makes an SThought::SCat around the string. The port doesn't
  reproduce that: any non-empty string dies "Don't know how to think about …".
- ``core`` is a weak ref in Perl (Memoize keeps the thought alive, the core can go away).
  Python keeps a strong ref.
"""
from seqsee import util
from seqsee.errors import Confess

# (class, list_context, id(core)) → (core, thought); the core is kept so its id stays unique.
_CREATE_MEMO = {}

_TYPE2CLASS = {
    "Seqsee::Element": ("seqsee.sthought.sobject", "SThoughtSeqseeElement"),
    "Seqsee::Anchored": ("seqsee.sthought.sobject", "SThoughtSeqseeAnchored"),
    "SRelation": ("seqsee.sthought.relations", "SThoughtSRelation"),
}


def _thought_class(module_name, class_name):
    import importlib
    return getattr(importlib.import_module(module_name), class_name)


class SThought:
    """Perl: package SThought (Moose). ``SThought({"core": c, "stored_fringe": f})`` or
    keyword arguments; ``core`` is required."""

    perl_name = "SThought"
    NAME = None   # Perl: our $NAME (set by each subclass)

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess(f"{self.perl_name}: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        if "core" not in kwargs:
            raise Confess("Attribute (core) is required")
        self._core = kwargs["core"]
        self._stored_fringe = kwargs.get("stored_fringe")

    # --- attributes (Moose rw accessors: with an argument they set and return it) ---------

    def core(self, *value):
        if value:
            self._core = value[0]
        return self._core

    def stored_fringe(self, *value):
        if value:
            self._stored_fringe = value[0]
        return self._stored_fringe

    # --- methods -------------------------------------------------------------------------

    @classmethod
    def create(cls, core, list_context=False):
        """Perl: SThought->create($core), memoized per context (see the module docstring)."""
        if core is None:
            raise Confess('Can\'t call method "isa" on an undefined value')
        if util.perl_ref(core) == "":
            if util.perl_str(core) == "":
                raise Confess('Can\'t call method "isa" without a package or object reference')
            raise Confess(f"Don't know how to think about {util.perl_str(core)}")
        key = (cls, bool(list_context), id(core))
        if key in _CREATE_MEMO:
            return _CREATE_MEMO[key][1]
        from seqsee.categories.base import SCategory
        if isinstance(core, SCategory):
            from seqsee.sthought.scat import SThoughtSCat
            klass = SThoughtSCat
        else:
            where = _TYPE2CLASS.get(util.perl_ref(core))
            if where is None:
                raise Confess(f"Don't know how to think about {util.perl_ref_string(core)}")
            klass = _thought_class(*where)
        thought = klass({"core": core})
        _CREATE_MEMO[key] = (core, thought)
        return thought

    @staticmethod
    def reset():
        """Forget the memoized thoughts (for tests; Perl never flushes the cache)."""
        _CREATE_MEMO.clear()

    def schedule(self):
        """Perl: schedule (``SCoderack->schedule_thought($self)``, a missing method)."""
        raise Confess('Can\'t locate object method "schedule_thought" via package "SCoderack"')

    def force_to_be_next_runnable(self):
        """Perl: force_to_be_next_runnable (``SCoderack->force_thought($self)``, a missing
        method)."""
        raise Confess('Can\'t locate object method "force_thought" via package "SCoderack"')

    def display_self(self, widget):
        """Perl: display_self($widget) (GUI)."""
        widget.Display("Thought", ["heading"], "\n", self.as_text())


def reset():
    """Module-level alias of ``SThought.reset`` (conftest)."""
    SThought.reset()
