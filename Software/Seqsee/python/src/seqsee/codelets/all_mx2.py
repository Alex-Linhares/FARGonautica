"""Port of lib/Seqsee/SCF_MX/AllMX2.pm: the families AttemptExtensionOfGroup and TryToSquint.

Each family is registered with ``@codelet_family`` under its Perl name (package
``Seqsee::SCF::<Name>``); the body gets the validated arguments positionally.
"""
from seqsee import sltm, sworkspace, util
from seqsee.codelets.family import codelet_family
from seqsee.codelets.general import _perl_eq, _schedule
from seqsee.errors import Confess


# ---- AttemptExtensionOfGroup ------------------------------------------------------------
@codelet_family("AttemptExtensionOfGroup", attributes=[("object", {}), ("direction", {})])
def attempt_extension_of_group(object, direction):  # noqa: A002 (Perl's name)
    """Perl: Seqsee::SCF::AttemptExtensionOfGroup. Extends the live group by its rule app's
    next item in ``direction`` (SafeExtend); then, with probability strength/100, schedules
    AreWeDone/100. The rule app is sanity-checked before and after looking, and losing it
    confesses "underlying_reln lost!"."""
    from seqsee.constants import DIR
    from seqsee.sanity import sanity_check
    if not sworkspace.check_liveness(object):
        return None
    underlying_reln = object.get_underlying_reln()
    if util.perl_true(underlying_reln):
        sanity_check(object, underlying_reln, "In AttemptExtensionOfGroup pre")
    extension = object.find_extension(direction, 0)
    if not util.perl_true(extension):
        return None
    if util.perl_true(underlying_reln):
        sanity_check(object, underlying_reln, "In AttemptExtensionOfGroup post")

    add_to_end_p = 1 if _perl_eq(direction, DIR.RIGHT) else 0
    if not util.perl_true(object.safe_extend(extension, add_to_end_p)):
        return None

    if util.toss(util.perl_num(object.get_strength()) / 100):
        _schedule("AreWeDone", 100, {"group": object})
    if util.perl_true(underlying_reln) and not util.perl_true(object.get_underlying_reln()):
        raise Confess("underlying_reln lost!")
    return None


# ---- TryToSquint ------------------------------------------------------------------------
@codelet_family("TryToSquint", attributes=[("actual", {}), ("intended", {})])
def try_to_squint(actual, intended):
    """Perl: Seqsee::SCF::TryToSquint. Of the metonym types under which ``actual`` squints to
    look like ``intended`` (CheckSquintability), spikes them by 100 and chooses one by
    activation; ``actual`` gets that metonym, made active."""
    potential_squints = actual.check_squintability(intended)
    if not potential_squints:
        return None
    chosen_squint = sltm.spike_and_choose(100, *potential_squints)
    if not util.perl_true(chosen_squint):
        return None
    cat, name = chosen_squint.get_cat_and_name()
    actual.annotate_with_metonym(cat, name)
    actual.set_metonym_activeness(1)
    return None
