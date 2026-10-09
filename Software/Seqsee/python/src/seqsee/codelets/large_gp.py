"""Port of lib/Seqsee/SCF_MX/LargeGp.pm: the families LargeGroup, MaybeStartBlemish,
InterlacedInitialBlemish and ArbitraryInitialBlemish (what to do with a group that covers
much of the sequence, and with an initial blemish: a group that won't reach the left edge).

Each family is registered with ``@codelet_family`` under its Perl name (package
``Seqsee::SCF::<Name>``); the body gets the validated arguments positionally.
UI hooks (headless): ``_message`` (main::message, logged).
"""
import logging
import re

from seqsee import sworkspace, util
from seqsee.codelets.family import action, codelet_family
from seqsee.codelets.general import _schedule
from seqsee.multimethods import perl_isa

_log = logging.getLogger(__name__)


def _message(msg, *rest):
    """Perl: main::message (UI). Headless: logged."""
    _log.info("%s", msg)


def _action(urgency, family, options):
    """Perl: ACTION($urgency, $family, $options)."""
    return action(urgency, family, options)


# ---- LargeGroup -------------------------------------------------------------------------
@codelet_family("LargeGroup", attributes=[("group", {})])
def large_group(group):
    """Perl: Seqsee::SCF::LargeGroup. A group spanning the whole sequence schedules
    AreWeDone/100; once the user has verified a term, one flush right only schedules
    MaybeStartBlemish/100."""
    from seqsee import global_ as Global
    flush_right = util.perl_true(group.is_flush_right())
    flush_left = util.perl_true(group.is_flush_left())
    if flush_right and flush_left:
        _schedule("AreWeDone", 100, {"group": group})
    elif util.perl_true(Global.AtLeastOneUserVerification) and flush_right and not flush_left:
        _schedule("MaybeStartBlemish", 100, {"group": group})
    return None


# ---- MaybeStartBlemish ------------------------------------------------------------------
@codelet_family("MaybeStartBlemish", attributes=[("group", {})])
def maybe_start_blemish(group):
    """Perl: Seqsee::SCF::MaybeStartBlemish. A group not flush left is extended leftward if
    its rule allows. Otherwise there is a blemish: with a structural Interlaced_N rule,
    schedule InterlacedInitialBlemish/100 (count N, as a string); else, if flush right,
    ArbitraryInitialBlemish/100."""
    from seqsee.constants import DIR
    flush_right = util.perl_true(group.is_flush_right())
    flush_left = util.perl_true(group.is_flush_left())
    if flush_left:
        return None
    extension = group.find_extension(DIR.LEFT, 0)
    if util.perl_true(extension):
        group.extend(extension, 0)
        return None

    # So there *is* a blemish!
    underlying_ruleapp = group.get_underlying_reln()
    if not util.perl_true(underlying_ruleapp):
        return None
    underlying_rule = underlying_ruleapp.get_rule()
    transform = underlying_rule.get_transform()

    if perl_isa(transform, "Mapping::Structural"):
        cat = transform.get_category()
        m = re.match(r"Interlaced_(.*)", util.perl_str(cat.get_name()))
        if m:
            _schedule("InterlacedInitialBlemish", 100,
                      {"count": m.group(1), "group": group, "cat": cat})
            return None

    # So: either statecount > 1, or not interlaced.
    if flush_right:
        _schedule("ArbitraryInitialBlemish", 100, {"group": group})
    return None


# ---- InterlacedInitialBlemish -----------------------------------------------------------
@codelet_family("InterlacedInitialBlemish",
                attributes=[("count", {}), ("group", {}), ("cat", {})])
def interlaced_initial_blemish(count, group, cat):
    """Perl: Seqsee::SCF::InterlacedInitialBlemish. The interlaced group started on the
    wrong foot: delete it, its parts and every live group of ``cat``; drop the first
    subpart, regroup the rest into ``count``-sized parts described as ``cat``; with at least
    two parts, group them (MappingBased on the first two's mapping) and think about it."""
    from seqsee import global_ as Global
    from seqsee.categories.mapping_based import MappingBased
    from seqsee.codelets.scf import continue_with
    from seqsee.mapping import find_mapping
    from seqsee.objects.anchored import Anchored
    from seqsee.sthought import SThought
    if not sworkspace.check_liveness(group):
        return None
    parts = list(group)
    Global.hilit(1, *parts)
    _message(f"I realize that there are {util.perl_str(count)} interlaced groups in the "
             "sequence, and I have started on the wrong foot. I will shift the big group one "
             "unit, and see if that helps!!")
    Global.clear_hilit()
    subparts = [x for p in parts for x in p]
    sworkspace.delete_group(group)
    for p in parts:
        sworkspace.delete_group(p)

    # Also delete other interlaced groups of this category.
    for obj in sworkspace.get_objects_belonging_to_category(cat):
        if not sworkspace.check_liveness(obj):
            continue
        sworkspace.delete_group(obj)

    subparts.pop(0)
    n = util.perl_num(count)
    newparts = []
    while len(subparts) >= n:
        new_part = [subparts.pop(0) for _ in range(1, int(n) + 1)]
        newpart = Anchored.create(*new_part)
        newpart.describe_as(cat)
        if not util.perl_true(sworkspace.add_group(newpart)):
            return None
        newparts.append(newpart)
    if len(newparts) > 1:
        transform = find_mapping(newparts[0], newparts[1])
        if not util.perl_true(transform):
            return None
        new_gp = Anchored.create(*newparts)
        new_gp.describe_as(MappingBased.create(transform))
        sworkspace.add_group(new_gp)
        continue_with(SThought.create(new_gp, list_context=True))
    return None


# ---- ArbitraryInitialBlemish ------------------------------------------------------------
@codelet_family("ArbitraryInitialBlemish", attributes=[("group", {})])
def arbitrary_initial_blemish(group):
    """Perl: Seqsee::SCF::ArbitraryInitialBlemish. In testing mode throws
    SErr::FinishedTestBlemished; else ACTION DescribeSolution/100."""
    from seqsee import global_ as Global
    from seqsee.errors import FinishedTestBlemished
    if util.perl_true(Global.TestingMode):
        FinishedTestBlemished.throw()
    _action(100, "DescribeSolution", {"group": group})
    return None
