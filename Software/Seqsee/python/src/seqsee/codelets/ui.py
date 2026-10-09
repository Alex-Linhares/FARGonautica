"""Port of lib/Seqsee/SCF_MX/UI.pm: the codelet families that ask the user about terms
beyond the known ones: AskIfThisIsTheContinuation, MaybeAskTheseTerms (+
get_core_type_and_rule), MaybeAskUsingThisGoodRule and DoTheAsking.

Each family is registered with ``@codelet_family`` under its Perl name (package
``Seqsee::SCF::<Name>``); the body gets the validated arguments positionally. The asking
itself is SErr::ElementsBeyondKnownSought's (user_interaction.py), whose answers come from
the pluggable ``user_interaction.boolean_response``.
UI hooks (headless): ``_message`` (main::message, logged).
"""
import logging

from seqsee import sworkspace, util
from seqsee.codelets.family import codelet_family
from seqsee.codelets.general import _schedule
from seqsee.constants import DIR
from seqsee.errors import Confess
from seqsee.multimethods import perl_isa
from seqsee.user_interaction import RulesAskedSoFar

_log = logging.getLogger(__name__)


def _message(msg, *rest):
    """Perl: main::message (UI). Headless: logged."""
    _log.info("%s", msg)


# ---- AskIfThisIsTheContinuation ---------------------------------------------------------
@codelet_family("AskIfThisIsTheContinuation", attributes=[
    ("relation", {"default": 0}),
    ("group", {"default": 0}),
    ("exception", {"required": 1}),
    ("expected_object", {"required": 1}),
    ("start_position", {"required": 1}),
    ("known_term_count", {"required": 1}),
])
def ask_if_this_is_the_continuation(relation, group, exception, expected_object,
                                    start_position, known_term_count):
    """Perl: Seqsee::SCF::AskIfThisIsTheContinuation. If no terms were added since it was
    scheduled, ask (based on the relation, else the group). On a yes, plonk the expected
    object at start_position; then relate it to the relation's second end with the same
    type, or (for a group with a rule app) relate it to the group's last item with the
    rule's transform and extend the group.

    PERL-QUIRK (oracle-confirmed): in testing mode a yes inserts no terms, so the plonk
    fails and nothing more happens."""
    if util.perl_num(sworkspace.ElementCount) != util.perl_num(known_term_count):
        return None
    if not (util.perl_true(relation) or util.perl_true(group)):
        raise Confess("Need relation or ruleapp")

    if util.perl_true(relation):
        success = exception.ask_based_on_relation(relation, "")
    else:
        success = exception.ask_based_on_group(group, "")
    if not util.perl_true(success):
        return None

    plonk_result = sworkspace.plonk_into_place.call(start_position, DIR.RIGHT, expected_object)
    if not util.perl_true(plonk_result.plonk_was_successful()):
        return None

    from seqsee.srelation import SRelation
    if util.perl_true(relation):
        SRelation({"first": relation.get_second(), "second": plonk_result.resultant_object(),
                   "type": relation.get_type()}).insert()
    else:
        ruleapp = group.get_underlying_reln()
        if not util.perl_true(ruleapp):
            return None
        transform = ruleapp.get_rule().get_transform()
        new_object = plonk_result.resultant_object()
        new_relation = SRelation({"first": group[-1], "second": new_object,
                                  "type": transform})
        if not util.perl_true(new_relation.insert()):
            return None
        group.extend(new_object, 1)
    return None


# ---- MaybeAskTheseTerms -----------------------------------------------------------------
def get_core_type_and_rule(core):
    """Perl: Seqsee::SCF::MaybeAskTheseTerms::get_core_type_and_rule($core):
    ("relation", createRule($core)) or ("ruleapp", $core->get_rule)."""
    if perl_isa(core, "SRelation"):
        type_of_core = "relation"
    elif perl_isa(core, "SRuleApp"):
        type_of_core = "ruleapp"
    else:
        raise Confess(f"Strange core {util.perl_str(core)}")
    if type_of_core == "relation":
        from seqsee.srule import SRule
        rule = SRule.create(core)
    else:
        rule = core.get_rule()
    return type_of_core, rule


@codelet_family("MaybeAskTheseTerms", attributes=("core", "exception"))
def maybe_ask_these_terms(core, exception):
    """Perl: Seqsee::SCF::MaybeAskTheseTerms. A rule that extended successfully before
    (at an earlier step) gets MaybeAskUsingThisGoodRule/100. Otherwise a relation core
    spikes its type by 10 and survives a toss of strength/100; a rule app core schedules
    DoTheAsking/100. Either way the rule goes on the failure list.

    PERL-QUIRK: ``$success`` is never set, so the rule always counts as a failure, and a
    relation core never asks. The DoTheAsking it schedules lacks msg_prefix, which is
    mandatory (the ``defualt`` typo), so running it dies (oracle-confirmed). A success at
    the current step gives a time of 0, which counts as never."""
    from seqsee import sltm
    _message("MaybeAskTheseTerms called")
    type_of_core, rule = get_core_type_and_rule(core)

    time_since_successful_extension = \
        RulesAskedSoFar.time_since_rule_used_to_extend_successfully(rule)
    RulesAskedSoFar.time_since_rule_used_to_extend_unsuccessfully(rule)

    if util.perl_true(time_since_successful_extension):
        _schedule("MaybeAskUsingThisGoodRule", 100,
                  {"core": core, "rule": rule, "exception": exception})
        return None

    success = None
    if type_of_core == "relation":
        sltm.spike_by(10, core.get_type())
        strength = core.get_strength()
        if not util.toss(util.perl_num(strength) / 100):
            return None
    else:
        _schedule("DoTheAsking", 100, {"core": core, "exception": exception})
    if util.perl_true(success):
        RulesAskedSoFar.add_rule_to_success_list(rule)
    else:
        RulesAskedSoFar.add_rule_to_failure_list(rule)
    return None


# ---- MaybeAskUsingThisGoodRule ----------------------------------------------------------
@codelet_family("MaybeAskUsingThisGoodRule", attributes=("core", "rule", "exception"))
def maybe_ask_using_this_good_rule(core, rule, exception):
    """Perl: Seqsee::SCF::MaybeAskUsingThisGoodRule: DoTheAsking/100 with the prefix
    "I know I have asked this before..."."""
    _schedule("DoTheAsking", 100, {"core": core, "exception": exception,
                                   "msg_prefix": "I know I have asked this before..."})
    return None


# ---- DoTheAsking ------------------------------------------------------------------------
@codelet_family("DoTheAsking", attributes=[
    ("core", {}), ("exception", {}), ("msg_prefix", {"defualt": ""})])
def do_the_asking(core, exception, msg_prefix):
    """Perl: Seqsee::SCF::DoTheAsking: ask based on the relation or rule app; the rule
    goes on the success or the failure list.

    PERL-QUIRK: the attribute spec's ``defualt`` typo makes msg_prefix mandatory."""
    _message("DoTheAsking called")
    type_of_core, rule = get_core_type_and_rule(core)
    if type_of_core == "relation":
        success = exception.ask_based_on_relation(core, msg_prefix)
    else:
        success = exception.ask_based_on_rule_app(core, msg_prefix)
    if util.perl_true(success):
        RulesAskedSoFar.add_rule_to_success_list(rule)
    else:
        RulesAskedSoFar.add_rule_to_failure_list(rule)
    return None
