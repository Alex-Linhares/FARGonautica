"""Port of lib/Sanity.pm: ``SanityFail`` and the ``SanityCheck`` multimethod.

Variants (Class::Multimethods dispatch, see multimethods.py): ``()`` checks every group and
relation of the workspace; ``(Seqsee::Element)``; ``(Seqsee::Anchored)``;
``(Seqsee::Anchored, SRuleApp)`` and ``(Seqsee::Anchored, SRuleApp, $)`` (item 042, called by
ConvulseEnd and AttemptExtensionOfGroup); ``(SRelation)``. Item 046 added all but the two
rule-app ones.

The binding checks are ``while (each %$bindings)`` loops (``util.perl_each``): a failure
leaves the hash's iterator part-way, so the next check of the same bindings resumes after
the bad key (oracle-confirmed). PERL-QUIRK: the "Unanchored part!" check is unreachable,
because are_there_holes_here throws SErr on a non-anchored part first.
"""
import logging

from seqsee import util
from seqsee.errors import Confess
from seqsee.multimethods import Multimethod

_log = logging.getLogger(__name__)

SANITY_CHECK = Multimethod("SanityCheck")


def _message(msg):
    """Perl: main::message (UI). Headless: logged."""
    _log.info("%s", msg)


def sanity_fail(m):
    """Perl: SanityFail($m): message, then confess "Sanity failed... exiting! ...".

    The message names ``$Global::CurrentCodelet->as_text``: with no current codelet (or an
    SAction, which has no as_text) building it dies first (oracle-confirmed)."""
    from seqsee import global_ as Global
    msg = (f"Entered inconsistent state after a {util.perl_str(Global.CurrentRunnableString)}."
           f"({util.perl_str(Global.Steps_Finished)})\n{util.perl_str(m)}")
    codelet = Global.CurrentCodelet
    if codelet is None:
        raise Confess('Can\'t call method "as_text" on an undefined value')
    as_text = getattr(codelet, "as_text", None)
    if as_text is None:
        raise Confess(f'Can\'t locate object method "as_text" via package '
                      f'"{util.perl_ref(codelet)}"')
    msg += "The codelet was: " + as_text()
    _message(msg)
    raise Confess(f"Sanity failed... exiting! {msg}")


def _check_bindings(gp, cats):
    for cat in cats:
        bindings = gp.get_binding_for_category(cat)
        if not util.perl_true(bindings):
            sanity_fail("No bindings?")
        for k, v in util.perl_each(bindings.get_bindings_ref()):
            if not util.perl_ref(v):
                sanity_fail(f"Non-ref in bindings: {util.perl_str(k)} => {util.perl_str(v)} for "
                            + cat.get_name())


@SANITY_CHECK.variant()
def _sanity_check_all():
    """Every group (longest first), then every relation."""
    from seqsee import sworkspace
    for gp in sworkspace.get_groups():
        SANITY_CHECK.call(gp)
    for rel in list(sworkspace.relations.values()):
        SANITY_CHECK.call(rel)


@SANITY_CHECK.variant("Seqsee::Element")
def _sanity_check_element(gp):
    _check_bindings(gp, gp.get_categories())


@SANITY_CHECK.variant("Seqsee::Anchored")
def _sanity_check_anchored(gp):
    from seqsee import sworkspace
    from seqsee.multimethods import perl_isa
    underlying_ruleapp = gp.get_underlying_reln()
    if util.perl_true(underlying_ruleapp):
        SANITY_CHECK.call(gp, underlying_ruleapp)
    left, right = gp.get_edges()
    if not 0 <= util.perl_num(left):
        sanity_fail(f"Edge problem: left {util.perl_str(left)}")
    if not util.perl_num(left) <= util.perl_num(right):
        sanity_fail(f"Edge problem: {util.perl_str(left)} {util.perl_str(right)}")
    if not util.perl_num(right) < util.perl_num(sworkspace.ElementCount):
        sanity_fail(f"Edge problem: right {util.perl_str(right)}; Workspace has "
                    f"{util.perl_str(sworkspace.ElementCount)}")

    parts = list(gp)
    if util.perl_true(sworkspace.are_there_holes_here(*parts)):
        sanity_fail("Holes in group!")
    for part in parts:
        if not perl_isa(part, "Seqsee::Anchored"):
            sanity_fail("Unanchored part!")
        if util.perl_true(part.get_is_a_metonym()):
            sanity_fail("Group has metonym as part")

    cats = gp.get_categories()
    if not cats:
        hist = "\n".join(gp.get_history())
        for subgp in gp:
            hist += "\n-------- " + subgp.as_text() + "\n"
            hist += "\n".join(subgp.get_history())
        sanity_fail("Group without any category:" + gp.as_text() + "\nhist:\n" + hist)
    _check_bindings(gp, cats)


@SANITY_CHECK.variant("SRelation")
def _sanity_check_relation(rel):
    ends = rel.get_ends()
    if util.perl_num(ends[0].get_left_edge()) > util.perl_num(ends[1].get_left_edge()):
        sanity_fail("Leftward relation " + rel.as_text() + "!")
    for end in ends:
        if util.perl_true(end.is_this_a_metonymed_object()):
            sanity_fail("End of a relation is a metonymed object")


@SANITY_CHECK.variant("Seqsee::Anchored", "SRuleApp")
def _sanity_check_group_ruleapp(gp, ra):
    return SANITY_CHECK.call(gp, ra, "")


@SANITY_CHECK.variant("Seqsee::Anchored", "SRuleApp", "$")
def _sanity_check_group_ruleapp_msg(gp, ra, m):
    """The group's parts and the rule app's items must be the same objects (parts with an
    active metonym are not compared)."""
    m = f"({util.perl_str(m)}) " if util.perl_true(m) else ""
    gp_parts = list(gp)
    ra_items = list(ra.get_items())
    count = len(gp_parts)
    if len(ra_items) != count:
        msg = (f"Group: {gp.as_text()} has {count} elements: "
               f"{' '.join(util.perl_ref_string(x) for x in gp_parts)}, whereas ruleapp only has "
               f"{' '.join(util.perl_ref_string(x) for x in ra_items)}")
        sanity_fail(f"{m} Gp/Ruleapp out of sync! {msg}")
    for gp_part, ra_part in zip(gp_parts, ra_items):
        if util.perl_true(gp_part.get_metonym_activeness()):
            pass  # The Perl check here is commented out.
        elif gp_part is not ra_part:
            sanity_fail(f"{m} Gp/Ruleapp item out of sync!")


def sanity_check(*args):
    """Perl: SanityCheck(...)."""
    return SANITY_CHECK.call(*args)
