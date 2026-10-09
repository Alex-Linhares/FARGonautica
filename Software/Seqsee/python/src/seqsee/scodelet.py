"""Port of lib/SCodelet.pm: a codelet waiting on the coderack.

``SCodelet(family, urgency, args)`` is Perl's ``SCodelet->new($family, $urgency, $args)``
(BUILDARGS): ``creation_time`` is ``$Global::Steps_Finished`` and a false ``args`` becomes
``{}``. Indexing/iterating gives ``[family, urgency, creation_time, arguments]`` (the
``@{}`` overload; Seqsee.pm reads ``$runnable->[0]``).

``run`` (the ``around 'run'``) first checks that no argument has changed since the codelet
was created (``CheckFreshness``/``IsFresh``) and silently returns None if one has.

Hook: ``_coderack_add_codelet(codelet)`` (SCoderack->add_codelet, in seqsee.scoderack).
"""
from seqsee import global_ as Global
from seqsee import scodelet_base, util
from seqsee.codelets.family import hash_deref
from seqsee.multimethods import Multimethod
from seqsee.scodelet_base import SCodeletBase


def _coderack_add_codelet(codelet):
    """Perl: SCoderack->add_codelet($codelet)."""
    from seqsee import scoderack
    scoderack.add_codelet(codelet)


class SCodelet(SCodeletBase):
    """Perl: package SCodelet (Moose, extends SCodeletBase)."""

    perl_name = "SCodelet"

    def __init__(self, family, urgency=None, args=None):
        if not util.perl_true(args):
            args = {}
        super().__init__(family, urgency, args)
        self.creation_time = Global.Steps_Finished

    def _as_list(self):
        return [self.family, self.urgency, self.creation_time, self.arguments]

    def __getitem__(self, i):
        return self._as_list()[i]

    def __iter__(self):
        return iter(self._as_list())

    def run(self):
        """Perl: ``around 'run'``: debug message, freshness check, then SCodeletBase::run."""
        if util.perl_true(Global.debugMAX):
            scodelet_base._message([self.family, "green",
                                    "About to run: " + util.stringify_for_carp(self)])
        if not check_freshness(self.creation_time, *hash_deref(self.arguments).values()):
            return None
        return self._base_run()

    def as_text(self):
        """Perl: as_text. PERL-QUIRK: there is no closing parenthesis."""
        return ("Codelet(family=" + util.perl_str(self.family)
                + ",urgency=" + util.perl_str(self.urgency)
                + ",args=" + util.perl_str(util.stringify_for_carp(self.arguments)))

    def schedule(self):
        """Perl: schedule. Adds the codelet to the coderack."""
        _coderack_add_codelet(self)


def check_freshness(since, *values):
    """Perl: CheckFreshness($since, @values). 1 if every value IsFresh, else None.

    Values are checked in order (Perl: hash order) and the check stops at the first stale one.
    """
    for v in values:
        if not util.perl_true(IS_FRESH.call(v, since)):
            return None
    return 1


IS_FRESH = Multimethod("IsFresh")


@IS_FRESH.variant("*", "#")
def _is_fresh_default(obj, since):
    return 1


@IS_FRESH.variant("HASH", "#")
def _is_fresh_hash(obj, since):
    return 1


@IS_FRESH.variant("Seqsee::Anchored", "#")
def _is_fresh_anchored(obj, since):
    return obj.unchanged_since(since)


@IS_FRESH.variant("SRelation", "#")
def _is_fresh_relation(rel, since):
    """Both ends must be unchanged; the relation's own history is ignored."""
    first, second = rel.get_ends()
    return util.perl_true(first.unchanged_since(since)) and util.perl_true(
        second.unchanged_since(since))
