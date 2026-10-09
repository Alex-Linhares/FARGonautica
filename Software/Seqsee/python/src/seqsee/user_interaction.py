"""Port of lib/UserInteraction.pm (headless), plus the UI hooks it relies on.

UserInteraction.pm has three packages:

- ``SErr::ElementsBeyondKnownSought``'s asking methods (Ask, AskBasedOnRelation,
  AskBasedOnRuleApp, AskBasedOnGroup, DoInsertBookKeeping, RuleAppPenetration,
  RelationPenetration). The exception class lives in errors.py and its methods call the
  functions here: ``ask``, ``ask_based_on_relation``, ... .
- ``RulesAskedSoFar`` and ``SolutionConfirmation``: classes whose class-level state are the
  file lexicals. ``reset()`` restores them.

Headless answers come from two pluggable callbacks (module attributes, looked up at call
time, so tests can monkeypatch them or use ``install``):

- ``boolean_response(question, *rest)``: Perl ``$SGUI::Commentary->
  MessageRequiringBooleanResponse(...)``. The default ``no_answer`` returns None (no
  answer, so the extension counts as rejected). ``perl_headless_boolean_response``
  reproduces headless Perl, where ``$SGUI::Commentary`` is undef and the call dies.
- ``ask_user_extension(items, msg=None)``: Perl ``main::ask_user_extension``, which the
  UI installs. The default ``gui_ask_user_extension`` is UI/Graphical.pm's (it asks
  ``boolean_response``). ``testing_ask_user_extension`` is Test::Seqsee's: it answers
  from ``Global.RealSequence`` (the "auto" answerer).
- ``response(choices, question)``: Perl ``$SGUI::Commentary->MessageRequiringAResponse(
  [choices], $question)`` (Scripts/DescribeSolution.pm asks Yes/No). It returns the chosen
  string. The default ``no_response`` returns None (not "Yes", so the solution counts as
  rejected). ``perl_headless_response`` raises headless Perl's death.

Also here: Seqsee.pm's ``already_rejected_by_user`` and Test::Seqsee's failed-request
counter (``reset_failed_requests``/``increment_failed_requests``/``get_failed_requests``).
"""
import logging

from seqsee import global_ as Global
from seqsee import util
from seqsee.errors import Confess, NotClairvoyant

_log = logging.getLogger(__name__)


# ---- the pluggable answers ------------------------------------------------------------------
def no_answer(question, *rest):
    """Default ``boolean_response``: no answer (None), logged."""
    _log.info("question (no answer): %s", question)
    return None


def perl_headless_boolean_response(question, *rest):
    """What headless Perl does: ``$SGUI::Commentary`` is undef, so the call dies."""
    raise Confess('Can\'t call method "MessageRequiringBooleanResponse" on an undefined value')


boolean_response = no_answer


def no_response(choices, question):
    """Default ``response``: no answer (None), logged."""
    _log.info("question (no response): %s", question)
    return None


def perl_headless_response(choices, question):
    """What headless Perl does: ``$SGUI::Commentary`` is undef, so the call dies."""
    raise Confess('Can\'t call method "MessageRequiringAResponse" on an undefined value')


response = no_response


def _update_display():
    """Perl: main::update_display (GUI). Headless: nothing."""
    return None


def gui_ask_user_extension(items, msg_suffix=None):
    """Perl: main::ask_user_extension(\\@items, $msg_suffix) from UI/Graphical.pm: unless
    already rejected, ask "Is the next term ...?" via ``boolean_response``. A yes sets
    $Global::AtLeastOneUserVerification. Returns the answer."""
    if already_rejected_by_user(items):
        return None
    joined = " ".join(util.perl_str(x) for x in items)
    msg = f"Is the next term {joined}?" if len(items) == 1 else f"Are the next terms: {joined}?"
    if util.perl_true(Global.Feature.get("debug")):
        ok = boolean_response(msg, "", msg_suffix, ["debug"])
    else:
        ok = boolean_response(msg)
    if util.perl_true(ok):
        Global.AtLeastOneUserVerification = 1
    return ok


_failed_requests = None


def reset_failed_requests():
    """Perl: Test::Seqsee's ResetFailedRequests."""
    global _failed_requests
    _failed_requests = 0


def increment_failed_requests():
    """Perl: Test::Seqsee's IncrementFailedRequests (undef++ is 1)."""
    global _failed_requests
    _failed_requests = (_failed_requests or 0) + 1


def get_failed_requests():
    """Perl: Test::Seqsee's GetFailedRequests (undef until reset or incremented)."""
    return _failed_requests


def testing_ask_user_extension(items, msg=None):
    """Perl: the main::ask_user_extension Test::Seqsee's INITIALIZE_for_testing installs.

    Answers from $Global::RealSequence: 1 (and sets AtLeastOneUserVerification) if the
    items continue the workspace's elements; None (and counts a failed request) if not.
    Dies on 0 items, and throws SErr::NotClairvoyant past the end of the real sequence.
    """
    from seqsee import sworkspace
    if already_rejected_by_user(items):
        return None
    ws_count = sworkspace.ElementCount
    ask_terms_count = len(items)
    if not ask_terms_count:
        raise Confess("ask_user_extension called with 0 terms!")
    if not ws_count + ask_terms_count <= len(Global.RealSequence):
        NotClairvoyant.throw()
    for i in range(ask_terms_count):
        if util.perl_num(Global.RealSequence[ws_count + i]) != util.perl_num(items[i]):
            increment_failed_requests()
            return None
    Global.AtLeastOneUserVerification = 1
    return 1


ask_user_extension = gui_ask_user_extension


def install(boolean_response=None, ask_user_extension=None, response=None):
    """Install answer callbacks (those given)."""
    g = globals()
    if boolean_response is not None:
        g["boolean_response"] = boolean_response
    if ask_user_extension is not None:
        g["ask_user_extension"] = ask_user_extension
    if response is not None:
        g["response"] = response


# ---- Seqsee.pm ------------------------------------------------------------------------------
def already_rejected_by_user(items):
    """Perl: Seqsee::already_rejected_by_user(\\@items): 1 if some prefix of the items
    (joined ", ") is in %Global::ExtensionRejectedByUser, else 0."""
    strs = [util.perl_str(x) for x in items]
    for i in range(len(strs)):
        if Global.ExtensionRejectedByUser.get(", ".join(strs[:i + 1])):
            return 1
    return 0


# ---- SErr::ElementsBeyondKnownSought --------------------------------------------------------
def ask(err, question_prefix=None, question_suffix=None, debug_msg=None):
    """Perl: SErr::ElementsBeyondKnownSought::Ask($prefix, $suffix, $debug_msg).

    Nothing (None) if the user already rejected the items. In testing mode, returns
    main::ask_user_extension's answer, and a yes inserts nothing. Otherwise asks the
    question; a yes does DoInsertBookKeeping, anything else records the rejection.
    Returns the answer."""
    items = list(err.next_elements)
    if already_rejected_by_user(items):
        return None
    if util.perl_true(Global.TestingMode):
        # PERL-QUIRK (oracle-confirmed): no DoInsertBookKeeping here, so a yes in testing
        # mode leaves the workspace as it is.
        return ask_user_extension(items)
    actual_question = err.actual_question()
    question = util.perl_str(question_prefix) + actual_question + util.perl_str(question_suffix)
    if util.perl_true(Global.Feature.get("debug")):
        validated = boolean_response(question, "", debug_msg, ["debug"])
    else:
        validated = boolean_response(question)
    if util.perl_true(validated):
        do_insert_book_keeping(err)
    else:
        Global.ExtensionRejectedByUser[", ".join(util.perl_str(x) for x in items)] = 1
    return validated


def _ask_hilit(err, objects, msg):
    Global.hilit(1, *objects)
    reply = err.ask(msg)
    Global.clear_hilit()
    return reply


def ask_based_on_relation(err, relation, msg_prefix):
    """Perl: AskBasedOnRelation($relation, $msg_prefix): ask with the relation and its
    ends hilit."""
    return _ask_hilit(err, [*relation.get_ends(), relation],
                      util.perl_str(msg_prefix) + " Extending analogy: ")


def ask_based_on_rule_app(err, ruleapp, msg_prefix):
    """Perl: AskBasedOnRuleApp($ruleapp, $msg_prefix): ask with the rule app's items hilit."""
    return _ask_hilit(err, list(ruleapp.get_items()),
                      util.perl_str(msg_prefix) + " Extending analogy: ")


def ask_based_on_group(err, group, msg_prefix):
    """Perl: AskBasedOnGroup($group, $msg_prefix): ask with the group's items hilit."""
    return _ask_hilit(err, list(group), util.perl_str(msg_prefix) + " Extending group: ")


def do_insert_book_keeping(err):
    """Perl: DoInsertBookKeeping: insert the items, reset AcceptableTrustLevel to 0.5, set
    AtLeastOneUserVerification and Break_Loop, update the display."""
    from seqsee import sworkspace
    sworkspace.insert_elements(*err.next_elements)
    Global.AcceptableTrustLevel = 0.5
    Global.AtLeastOneUserVerification = 1
    _update_display()
    Global.Break_Loop = 1


def rule_app_penetration(err, ruleapp_left_edge):
    """Perl: RuleAppPenetration($left_edge): 1 - left_edge / ElementCount."""
    from seqsee import sworkspace
    return 1 - util.perl_num(ruleapp_left_edge) / sworkspace.ElementCount


def relation_penetration(err, *args):
    """Perl: RelationPenetration($relation), an unfinished stub.

    PERL-QUIRK (oracle-confirmed): the body is only ``my ($self, $relation) = @_;``, so in
    scalar context it returns the number of arguments including $self."""
    return 1 + len(args)


# ---- RulesAskedSoFar ------------------------------------------------------------------------
class RulesAskedSoFar:
    """Perl: package RulesAskedSoFar. Rules are compared by identity (``eq`` on refs).

    ``successful_rules``/``unsuccessful_rules``: ``[rule, time]``, most recent first.
    ``accepted_rules``/``rejected_rules``: rule → rule."""

    successful_rules = []
    unsuccessful_rules = []
    accepted_rules = {}
    rejected_rules = {}

    @classmethod
    def reset(cls):
        cls.successful_rules.clear()
        cls.unsuccessful_rules.clear()
        cls.accepted_rules.clear()
        cls.rejected_rules.clear()

    @classmethod
    def is_most_recent_successful_rule(cls, rule):
        """Perl: IsMostRecentSuccessfulRule: -1 unless the rule is the most recent success,
        else the time since then."""
        if not cls.successful_rules or cls.successful_rules[0][0] is not rule:
            return -1
        return Global.Steps_Finished - cls.successful_rules[0][1]

    @classmethod
    def _time_since(cls, blocks, rule):
        for r, t in blocks:
            if r is rule:
                return Global.Steps_Finished - t
        return 0

    @classmethod
    def time_since_rule_used_to_extend_successfully(cls, rule):
        """Perl: TimeSinceRuleUsedToExtendSuccessfully (0 if never, or just now)."""
        return cls._time_since(cls.successful_rules, rule)

    @classmethod
    def time_since_rule_used_to_extend_unsuccessfully(cls, rule):
        """Perl: TimeSinceRuleUsedToExtendUnsuccessfully (0 if never, or just now)."""
        return cls._time_since(cls.unsuccessful_rules, rule)

    @classmethod
    def add_rule_to_success_list(cls, rule):
        """Perl: AddRuleToSuccessList."""
        cls.successful_rules.insert(0, [rule, Global.Steps_Finished])

    @classmethod
    def add_rule_to_failure_list(cls, rule):
        """Perl: AddRuleToFailureList."""
        cls.unsuccessful_rules.insert(0, [rule, Global.Steps_Finished])

    @classmethod
    def mark_rule_as_rejected(cls, rule):
        """Perl: MarkRuleAsRejected."""
        cls.rejected_rules[rule] = rule

    @classmethod
    def mark_rule_as_confirmed(cls, rule):
        """Perl: MarkRuleAsConfirmed."""
        cls.accepted_rules[rule] = rule

    @classmethod
    def has_rule_been_confirmed(cls, rule):
        """Perl: HasRuleBeenConfirmed (1/0)."""
        return 1 if rule in cls.accepted_rules else 0

    @classmethod
    def has_rule_been_rejected(cls, rule):
        """Perl: HasRuleBeenRejected (1/0)."""
        return 1 if rule in cls.rejected_rules else 0


# ---- SolutionConfirmation -------------------------------------------------------------------
class SolutionConfirmation:
    """Perl: package SolutionConfirmation (its class methods; the package argument is
    dropped). ``rejected``: rule → list of PositionStructures."""

    rejected = {}
    accepted_rule = None
    accepted_position_structure = None

    @classmethod
    def reset(cls):
        cls.rejected.clear()
        cls.accepted_rule = None
        cls.accepted_position_structure = None

    @classmethod
    def add_rejected_solution(cls, rule, position_structure):
        """Perl: AddRejectedSolution."""
        cls.rejected.setdefault(rule, []).append(position_structure)

    @classmethod
    def set_accepted_solution(cls, rule, position_structure):
        """Perl: SetAcceptedSolution."""
        cls.accepted_rule = rule
        cls.accepted_position_structure = position_structure

    @classmethod
    def has_this_been_rejected(cls, rule, position_structure):
        """Perl: HasThisBeenRejected: 1 if a rejected structure for the rule is a subset of
        this one, else None. (``||= []`` autovivifies the rule's entry.)"""
        for reject in cls.rejected.setdefault(rule, []):
            if util.perl_true(reject.is_a_subset_of(position_structure)):
                return 1
        return None


def reset():
    """Test-isolation hook: default callbacks, empty file lexicals, failed requests undef."""
    global boolean_response, ask_user_extension, response, _failed_requests
    boolean_response = no_answer
    response = no_response
    ask_user_extension = gui_ask_user_extension
    _failed_requests = None
    RulesAskedSoFar.reset()
    SolutionConfirmation.reset()
