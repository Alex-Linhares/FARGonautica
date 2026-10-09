"""Port of lib/SMetonym.pm: a specific metonym of one object.

A metonym records its SMetonymType (category + name + info_loss, memoized), the
starred ("unreal, hallucinated") object and the unstarred one (what is physically
present). Perl weakens the unstarred ref; the port holds it through a weakref.
"""
import weakref

from seqsee import util
from seqsee.errors import Confess
from seqsee.smetonym_type import SMetonymType


class SMetonym:
    """Perl: SMetonym (Class::Std).

    ``SMetonym({...})`` or ``SMetonym(category=, name=, info_loss=, starred=, unstarred=)``.
    """

    perl_name = "SMetonym"

    def __init__(self, opts_ref=None, **kwargs):
        opts = dict(opts_ref or {})
        opts.update(kwargs)
        # The type is created (and memoized) before starred/unstarred are checked.
        self._type = SMetonymType.create(opts)
        self._starred = opts.get("starred")
        if not util.perl_true(self._starred):
            raise Confess("Need starred")
        unstarred = opts.get("unstarred")
        if not util.perl_true(unstarred):
            raise Confess("Need unstarred")
        # Perl: weaken $unstarred_of{$id}.
        if util.perl_ref(unstarred) == "":
            raise Confess("Can't weaken a nonreference")
        try:
            self._unstarred = weakref.ref(unstarred)
        except TypeError:
            # Not weak-referenceable in Python (e.g. a plain list): keep a strong ref.
            self._unstarred = lambda: unstarred

    def get_type(self):
        return self._type

    def get_starred(self):
        return self._starred

    def get_unstarred(self):
        """None once the unstarred object is gone (Perl: the weakened ref becomes undef)."""
        return self._unstarred()

    @staticmethod
    def intersection(*meto):
        """Perl: SMetonym->intersection(@meto). The common type, or None if they differ.

        Types are compared with Perl ``eq`` on refs (SMetonymType is memoized), i.e. identity.
        """
        if not meto:
            raise Confess("Cannot take intersection of empty set")
        typ = meto[0].get_type()
        for m in meto[1:]:
            if m.get_type() is not typ:
                return None
        return typ

    def get_category(self):
        return self.get_type().get_category()

    def get_name(self):
        return self.get_type().get_name()

    def get_info_loss(self):
        return self.get_type().get_info_loss()
